{ gexJpeg Unit to interface with LibJpeg(Turbo) to read jpeg images.
  Dual License: MPL 1.1 or LGPL 2.1 with linking exception (the "FPC modified LGPL License")
  Portions Created by Jacob Boerema are Copyright (C) 2013-2015 Jacob Boerema.
  All Rights Reserved.
  This fork of GraphicEx can be found at https://bitbucket.org/jacobb/graphicex
}

unit gexJpeg;

interface

{$IFDEF FPC}
  {$mode delphi}
{$ENDIF}

{$I GraphicConfiguration.inc}

uses SysUtils, Classes,
     LibJpeg,
     {$IFNDEF FPC}C_Types,{$ENDIF} // Needed when UInt64 is not defined
     GraphicEx;

type
  // Define some TJpegImage compatibility types
  TJPEGQualityRange = 1..100;
  TJPEGPerformance = (jpBestQuality, jpBestSpeed);
  TJPEGScale = (jsFullSize, jsHalf, jsQuarter, jsEighth);
  TJPEGPixelFormat = (jf24Bit, jf8Bit);

  TColorConverter = procedure(const ASrcBuf: PByte; const ADestBuf: PByte;
    const ALineWidth, ANumComponents: Cardinal) of object;

  { TgexJpegImage }

  TgexJpegImage = class(TGraphicExGraphic)
  private
    FJpegInfo: j_decompress_ptr;
    FJpegErr: jpeg_error_mgr_ptr;
    {$IFDEF JPEG_MEASURE_SPEED}
    FLibJpegTicks: Int64;
    FConversionTicks: Int64;
    {$ENDIF}
    FScale: TJPEGScale;
    FAutoScaleLargeImage: Boolean;
    FAutoScaleMemoryLimit: UInt64;
  protected
    function InitJpegDecompress(const Memory: Pointer; const Size: Int64): Boolean;
    function InitJpegErrorManager: jpeg_error_mgr_ptr; virtual;
    function InitJpegSourceManager(var SrcManager: jpeg_source_mgr_ptr;
      const Memory: Pointer; const Size: Int64): Boolean; virtual;
    procedure Jpeg_RGB2BGR(const ASrcBuf: PByte; const ADestBuf: PByte;
      const ALineWidth, ANumComponents: Cardinal);
    procedure Jpeg_Copy(const ASrcBuf: PByte; const ADestBuf: PByte;
      const ALineWidth, ANumComponents: Cardinal);
    procedure Jpeg_ConvertRow(const ASrcBuf: PByte; const ADestBuf: PByte;
      const ALineWidth, {%H-}ANumComponents: Cardinal);
    function DoAutoScale: Boolean;
    function InternalReadImageProperties(const Memory: Pointer; Size: Int64;
      ImageIndex: Cardinal; ImageRequired: Boolean = False): Boolean;
  public
    constructor Create; override;
    destructor Destroy; override;

    class function CanLoad(const Memory: Pointer; Size: Int64): Boolean; override;
    function ReadImageProperties(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal): Boolean; override;
    procedure LoadFromMemory(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal = 0); override;
    {$IFDEF JPEG_MEASURE_SPEED}
    property LibJpegTicks: Int64 read FLibJpegTicks;
    property ConversionTicks: Int64 read FConversionTicks;
    {$ENDIF}
    property AutoScaleLargeImage: Boolean read FAutoScaleLargeImage write FAutoScaleLargeImage default False;
    property AutoScaleMemoryLimit: UInt64 read FAutoScaleMemoryLimit write FAutoScaleMemoryLimit;

    // TJpegImage compatible properties
    property Scale: TJPEGScale read FScale write FScale default jsFullSize;
  end;

// Made public for use in unit testing.
procedure InitgexJpeg;

implementation

uses Graphics,
     {$IFNDEF FPC}
     {$IFDEF NEED_TJPEGIMAGE_SAVING}
     // For being able to unregister TJpegImage. However this is bad because it
     // unnecessarily links in their libjpeg objects. Only use if you need it
     // anyway for saving since we don't support that yet ourselves.
     jpeg,
     {$ENDIF NEED_TJPEGIMAGE_SAVING}
     {$ENDIF}
     GraphicStrings, GraphicColor;

const
  BlockSize_FileMapping: Cardinal = High(Integer);
  BlockSize_MemoryStream: Cardinal = 16 * 1024;    // 16 KB

const
  cJpegSOIMarker = $d8ff;

type
   TShortFileHeader = record
     case byte of
       0: (Word1, Word2: Word;);
       1: (LongWord1: LongWord;);
       2: (string5: array [0..4] of char);
   end;
   PShortFileHeader = ^TShortFileHeader;

type
  TJpegSourceData = record
    jpeg_source: jpeg_source_mgr;
    // Extra fields we need go here
    jpeg_memory: Pointer;
    jpeg_filesize: UInt64;
    jpeg_pos: UInt64;
    jpeg_blocksize: Cardinal;
  end;
  PJpegSourceData = ^TJpegSourceData;

////////////////////////////////////////////////////////////////////////////////
//                           Jpeg DataSource handlers

//TJpegInitSource
procedure gexJpegInitSource(cinfo: j_decompress_ptr); cdecl;
begin
  PJpegSourceData(cinfo.Src).jpeg_pos := 0;
end;

const
  JpegEndOfInput: array [0..1] of Byte = ($ff, JPEG_EOI);

//TJpegFillInputBuffer
function gexJpegFillInputBuffer(cinfo: j_decompress_ptr): Boolean; cdecl;
var
  JpegData: PJpegSourceData;
  BufBytes: UInt64;
begin
  JpegData := PJpegSourceData(cinfo.Src);
  // Compute number of available bytes
  if JpegData.jpeg_pos + JpegData.jpeg_blocksize >= JpegData.jpeg_filesize then
    BufBytes := JpegData.jpeg_filesize - JpegData.jpeg_pos
  else
    BufBytes := JpegData.jpeg_blocksize;

  if BufBytes > 0 then begin
    // Point to start of input buffer
    cinfo.Src.next_input_byte := Pointer(PAnsiChar(JpegData.jpeg_memory) + JpegData.jpeg_pos);
    // Update position of input buffer
    Inc(JpegData.jpeg_pos, BufBytes);
    // Set available bytes in buffer
    cinfo.Src.bytes_in_buffer := BufBytes;
  end
  else begin
    // JpegLib requires the buffer never to be empty.
    // Insert a fake EOI marker.
    cinfo.Src.next_input_byte := @JpegEndOfInput;
    cinfo.Src.bytes_in_buffer := 2;
  end;

  Result := True;
end;

//TJpegSkipInputData
procedure gexJpegSkipInputData(cinfo: j_decompress_ptr; NumBytes: Integer); cdecl;
begin
  if NumBytes > 0 then begin
    while size_t(NumBytes) > cinfo.Src^.bytes_in_buffer do begin
      NumBytes := NumBytes - Integer(cinfo.Src^.bytes_in_buffer);
      cinfo.Src^.fill_input_buffer(cinfo);
		  { note we assume that fill_input_buffer will never
		    return FALSE, so suspension need not be handled. }
	  end;

    Inc(PJpegSourceData(cinfo.Src).jpeg_pos, NumBytes);
	  Inc(cinfo.Src^.next_input_byte, NumBytes);
	  Dec(cinfo.Src^.bytes_in_buffer, NumBytes);
  end;
end;

//TJpegResyncToRestart
function gexJpegResyncToRestart({%H-}cinfo: j_decompress_ptr; {%H-}Desired: Integer): Boolean; cdecl;
begin
  Result := False;
end;

//TJpegTermSource
procedure gexJpegTermSource({%H-}cinfo: j_decompress_ptr); cdecl;
begin
  // Nothing to be done here...
end;

////////////////////////////////////////////////////////////////////////////////
//                           TgexJpegImage

constructor TgexJpegImage.Create;
begin
  inherited Create;
  // Allocate memory for jpeg data and zero fill it
  FJpegInfo := AllocMem(SizeOf(jpeg_decompress_struct));
  FJpegErr := AllocMem(SizeOf(jpeg_error_mgr));
  FScale := jsFullSize;
  FAutoScaleLargeImage := False;
  FAutoScaleMemoryLimit := 1024 * 1024 * 1024; // Default 1 GB
end;

destructor TgexJpegImage.Destroy;
begin
  {if FJpegInfo.Src <> nil then
    FreeMem(FJpegInfo.Src);}
  FreeMem(FJpegErr);
  FreeMem(FJpegInfo);

  inherited Destroy;
end;

class function TgexJpegImage.CanLoad(const Memory: Pointer; Size: Int64): Boolean;
begin
  // TODO: Find a realistic minimum size for a jpeg image
  if Size > SizeOf(TShortFileHeader) then
    Result := PShortFileHeader(Memory)^.Word1 = cJpegSOIMarker
  else
    Result := False;
end;

function TgexJpegImage.InitJpegDecompress(const Memory: Pointer; const Size: Int64): Boolean;
begin
  // Initialize jpeg error manager
  FJpegInfo.err := InitJpegErrorManager;

  // Initialize jpeg decompress structure
  jpeg_create_decompress(FJpegInfo);

  // Initialize our source manager
  InitJpegSourceManager(FJpegInfo.Src, Memory, Size);

  Result := True;
end;

function TgexJpegImage.InitJpegErrorManager: jpeg_error_mgr_ptr;
begin
  // Initialize Jpeg error manager to the default first
  Result := jpeg_std_error(FJpegErr);
  // Initialize our error handlers. Since we don't want the other fields to be
  // changed we can't copy the whole record

  // The 5 functions are first in the record so we need to copy
  // 5 * SizeOf(Pointer)
  // Although by default we only really need error_exit and emit_message we
  // copy all 5 functions to make it possible to change these functions
  Move(DefaultErrorManager, Result^, 5*SizeOf(Pointer));
end;

function TgexJpegImage.InitJpegSourceManager(var SrcManager: jpeg_source_mgr_ptr;
  const Memory: Pointer; const Size: Int64): Boolean;
begin
  // Create decompression info and fill with our handlers
  if SrcManager = nil then begin
    // Get memory for the DataSource buffer. Use JpegLib's memory allocation
    // that way we don't need to worry about freeing it.
    SrcManager := FJpegInfo.Mem^.alloc_small(j_common_ptr(FJpegInfo),
      JPOOL_PERMANENT, SizeOf(TJpegSourceData));
    // Initialize it
    SrcManager.init_source := @gexJpegInitSource;
    SrcManager.fill_input_buffer := @gexJpegFillInputBuffer;
    SrcManager.skip_input_data := @gexJpegSkipInputData;
    SrcManager.resync_to_restart := @gexJpegResyncToRestart;
    SrcManager.term_source := @gexJpegTermSource;
  end;
  // Initialize extra data for our handlers
  PJpegSourceData(SrcManager).jpeg_memory := Memory;
  PJpegSourceData(SrcManager).jpeg_filesize := Size;
  PJpegSourceData(SrcManager).jpeg_blocksize := BlockSize_FileMapping;
  SrcManager.bytes_in_buffer := 0;
  SrcManager.next_input_byte := nil;
  Result := True;
end;

// InternalReadImageProperties handles the actual reading of properties.
// However it does not destroy the jpeg object.
function TgexJpegImage.InternalReadImageProperties(const Memory: Pointer;
  Size: Int64; ImageIndex: Cardinal; ImageRequired: Boolean = False): Boolean;
begin
  Result := False;
  // Initialize ImageProperties record
  if not inherited ReadImageProperties(Memory, Size, ImageIndex) then
    Exit;
  // Initialize Jpeg decompress data
  if not InitJpegDecompress(Memory, Size) then
    Exit;

  // read jpeg file header
  if jpeg_read_header(FJpegInfo, ImageRequired) = JPEG_HEADER_OK then begin
    FImageProperties.Compression := ctJPEG;
    FImageProperties.BitsPerSample := FJpegInfo.data_precision;
    FImageProperties.SamplesPerPixel := FJpegInfo.num_components;
    case FJpegInfo.jpeg_color_space of
      JCS_GRAYSCALE: FImageProperties.ColorScheme := csG;
      JCS_RGB: FImageProperties.ColorScheme := csRGB;
      JCS_YCbCr: FImageProperties.ColorScheme := csYCbCr;
      JCS_CMYK: FImageProperties.ColorScheme := csCMYK;
      JCS_YCCK: FImageProperties.ColorScheme := csYCCK;
    else
      FImageProperties.ColorScheme := csUnknown;
    end;
    FImageProperties.Width := FJpegInfo.image_width;
    FImageProperties.Height := FJpegInfo.image_height;
    FImageProperties.Version := 62;
    FImageProperties.HasAlpha := False;
    FImageProperties.BitsPerPixel := FImageProperties.BitsPerSample * FImageProperties.SamplesPerPixel;
    if FJpegInfo.saw_JFIF_marker then begin
      // fill pixel density info
      case FJpegInfo.density_unit of
        JFIF_DENSITY_UNIT_DPI:
          begin
            FImageProperties.XResolution := FJpegInfo.X_density;
            FImageProperties.YResolution := FJpegInfo.Y_density;
          end;
        JFIF_DENSITY_UNIT_DPCM:
          begin
            FImageProperties.XResolution := 2.54 * FJpegInfo.X_density;
            FImageProperties.YResolution := 2.54 * FJpegInfo.Y_density;
          end;
      else
        // Assume 96 dpi (normal screen resolution)
        FImageProperties.XResolution := FJpegInfo.X_density * 96;
        FImageProperties.YResolution := FJpegInfo.Y_density * 96;
      end;
    end;
    Result := True;
  end;
end;

function TgexJpegImage.ReadImageProperties(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal): Boolean;
begin
  Result := InternalReadImageProperties(Memory, Size, ImageIndex);
  // No need for a try block since in case of an exception the error handler
  // will call jpeg_destroy.
  if Result then
    jpeg_destroy(j_common_ptr(FJpegInfo));
end;

procedure TgexJpegImage.LoadFromMemory(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal = 0);
var
  PropertiesOK: Boolean;
  row_stride: Cardinal;
  buffer: array [0..0] of PByte;
  ConvertScanline: TColorConverter;
  {$IFDEF JPEG_MEASURE_SPEED}
  TempTick: Int64;
  {$ENDIF}
begin
  inherited;

  // True: We need an image too, not only the image header
  PropertiesOK := InternalReadImageProperties(Memory, Size, ImageIndex, True);

  if PropertiesOK then begin
    // Initialize outermost progress display.
    InitProgress(Width, 1);
    StartProgressSection(0, '');
    try

      {$IFDEF FPC}
      // We don't need a mask
      Self.Masked := False;
      {$ENDIF}
      // Jpeg's don't have transparency
      Self.Transparent := False;

      // From ReadImageProperties we already know what the OutColorSpace is gonna be
      case FJpegInfo.out_color_space of
      JCS_RGB:
        begin
          ConvertScanline := Jpeg_RGB2BGR;
          PixelFormat := pf24Bit;
        end;
      JCS_GRAYSCALE:
        begin
          {$IFNDEF FPC}
          ConvertScanline := Jpeg_Copy;
          PixelFormat := pf8Bit;
          Palette := ColorManager.CreateGrayScalePalette(False);
          {$ELSE}
          ConvertScanline := Jpeg_ConvertRow;
          ColorManager.SourceColorScheme := csG;
          Colormanager.SourceBitsPerSample := FImageProperties.BitsPerSample;
          ColorManager.SourceSamplesPerPixel := 1;
          ColorManager.TargetColorScheme := csBGR;
          ColorManager.TargetBitsPerSample := 8;
          ColorManager.TargetSamplesPerPixel := 3;
          PixelFormat := pf24Bit;
          {$ENDIF}
        end;
      JCS_CMYK, JCS_YCCK:
        begin
          ConvertScanline := Jpeg_ConvertRow;
          ColorManager.SourceColorScheme := csCMYK;
          Colormanager.SourceBitsPerSample := FImageProperties.BitsPerSample;
          ColorManager.SourceSamplesPerPixel := 4;
          ColorManager.TargetColorScheme := csBGR;
          ColorManager.TargetBitsPerSample := 8;
          ColorManager.TargetSamplesPerPixel := 3;
          if FJpegInfo.saw_Adobe_marker then
            // Adobe PhotoShop uses inverted CMYK
            ColorManager.SourceOptions := ColorManager.SourceOptions + [coInvertedCMYK];
          PixelFormat := pf24Bit;
        end;
      JCS_YCBCR:
        begin
          ConvertScanline := Jpeg_ConvertRow;
          ColorManager.SourceColorScheme := csYCBCR;
          Colormanager.SourceBitsPerSample := FImageProperties.BitsPerSample;
          ColorManager.SourceSamplesPerPixel := 3;
          ColorManager.TargetColorScheme := csBGR;
          ColorManager.TargetBitsPerSample := 8;
          ColorManager.TargetSamplesPerPixel := 3;
          PixelFormat := pf24Bit;
        end;
      else
        // If we don't know what to do then use an empty converter:
        // just copy without any conversion.
        ConvertScanline := Jpeg_Copy;
        PixelFormat := pf24Bit; // Assume 24 bit
      end;

      // Set buffer to nil so in case of an error we will know whether it's assigned or not
      buffer[0] := nil;
      try
        // Set scaling factor as defined by the user
        case FScale of
          jsHalf: FJpegInfo.scale_denom := 2;
          jsQuarter: FJpegInfo.scale_denom := 4;
          jsEighth: FJpegInfo.scale_denom := 8;
        end;
        // If requested scale down large images to reduce memory usage
        if FAutoScaleLargeImage then
          DoAutoScale;

        // Start decompressor
        jpeg_start_decompress(FJpegInfo);

        // Set dimensions after setting PixelFormat and after we know the output size
        Width := FJpegInfo.output_width;
        Height := FJpegInfo.output_height;

        row_stride := FJpegInfo.output_width * Cardinal(FJpegInfo.output_components);
        // Make a one-row-high sample array that will go away when done with image
        //buffer[0] := PByte(FJpegInfo.Mem.alloc_sarray(j_common_ptr(FJpegInfo), JPOOL_IMAGE, row_stride, 1));
        GetMem(buffer[0], row_stride * 1);

        {$IFDEF FPC}
        BeginUpdate(False);
        {$ENDIF}
        {$IFDEF JPEG_MEASURE_SPEED}
        FLibJpegTicks := 0;
        FConversionTicks := 0;
        {$ENDIF}
        {* Here we use the library's state variable cinfo.output_scanline as the
         * loop counter, so that we don't have to keep track ourselves.
         *}
        while (FJpegInfo.output_scanline < FJpegInfo.output_height) do begin
          {* jpeg_read_scanlines expects an array of pointers to scanlines.
           * Here the array is only one element long, but you could ask for
           * more than one scanline at a time if that's more convenient.
           *}
          {$IFDEF JPEG_MEASURE_SPEED}TempTick := GetTickCount64;{$ENDIF}
          jpeg_read_scanlines(FJpegInfo, @buffer, 1);
          {$IFDEF JPEG_MEASURE_SPEED}Inc(FLibJpegTicks, GetTickCount64 - TempTick);{$ENDIF}

          // Note: at this moment OutputScanline has already been incremented
          // by jpeg_read_scanlines. Thus we need to subtract 1* to get the
          // correct scanline number. * Or the number of scanlines we read at one time.
          {$IFDEF JPEG_MEASURE_SPEED}TempTick := GetTickCount64;{$ENDIF}
          ConvertScanline(buffer[0], ScanLine[FJpegInfo.output_scanline-1],
            FJpegInfo.output_width, Cardinal(FJpegInfo.output_components));
          {$IFDEF JPEG_MEASURE_SPEED}Inc(FConversionTicks, GetTickCount64 - TempTick);{$ENDIF}
        end;
        {$IFDEF FPC}
        EndUpdate(False);
        {$ENDIF}
        // Stop decompressor
        jpeg_finish_decompress(FJpegInfo);
      finally
        if Assigned(buffer[0]) then
          FreeMem(buffer[0]);
      end;
    finally
      // Release decompressor memory
      jpeg_destroy_decompress(FJpegInfo);
      FinishProgressSection(False);
    end;
  end
  else
    GraphicExError(gesInvalidImage, ['JPG']);
end;

// Scale down image widht/height if image memory size will be over a certain limit (default 1 GB)
// Assumes FJpegInfo already contains the user requested scale_denom.
// Result: True if it could be scaled within the memory limit, False if not.
function TgexJpegImage.DoAutoScale: Boolean;
var
  AWidth, AHeight: Cardinal;
  Denom: Cardinal;
begin
  AWidth := FJpegInfo.image_width;
  AHeight := FJpegInfo.image_height;
  Denom := FJpegInfo.scale_denom;
  while Denom <= 8 do begin
    FJpegInfo.scale_denom := Denom;
    if (AWidth div Denom) * (AHeight div Denom) * ColorManager.TargetSamplesPerPixel
       <= FAutoScaleMemoryLimit then
      break;
    Denom := Denom * 2;
  end;
  Result := Denom <= 8;
end;

procedure TgexJpegImage.Jpeg_RGB2BGR(const ASrcBuf: PByte; const ADestBuf: PByte;
  const ALineWidth, ANumComponents: Cardinal);
begin
  // Convert RGB to BGR
  RGBToBGR(ASrcBuf, ALineWidth, 1);
  // Move pixels from in buffer to out buffer

  Move(ASrcBuf^, ADestBuf^, ALineWidth * ANumComponents);
end;

procedure TgexJpegImage.Jpeg_Copy(const ASrcBuf: PByte; const ADestBuf: PByte;
  const ALineWidth, ANumComponents: Cardinal);
begin
  Move(ASrcBuf^, ADestBuf^, ALineWidth * ANumComponents);
end;

procedure TgexJpegImage.Jpeg_ConvertRow(const ASrcBuf: PByte; const ADestBuf: PByte;
  const ALineWidth, ANumComponents: Cardinal);
begin
  ColorManager.ConvertRow([ASrcBuf], ADestBuf, ALineWidth, $ff);
end;


////////////////////////////////////////////////////////////////////////////////


// Made public for use in unit testing.
procedure InitgexJpeg;
begin
  // Unregister TJpegImage first (both will just ignore it if TJpegImage isn't registered)
  {$IFDEF NEED_TJPEGIMAGE_SAVING}
  TPicture.UnregisterGraphicClass(TJpegImage);
  if FileFormatList = nil then
    Exit;
  FileFormatList.UnregisterFileFormat('', TJpegImage);
  {$ENDIF NEED_TJPEGIMAGE_SAVING}
  // Register Jpeg with our class
  if FileFormatList.GraphicFromExtension('jpg') <> nil then
    Exit; // Something else has already registered jpg

  // Register jpeg extensions to our jpeg class
  FileFormatList.RegisterFileFormat('jpeg', gesJPGImages, gesJPEGImages, [ftRaster], False, TgexJpegImage);
  FileFormatList.RegisterFileFormat('jpg',  '',           gesJPGImages,  [ftRaster], False, TgexJpegImage);
  FileFormatList.RegisterFileFormat('jpe',  '',           gesJPEImages,  [ftRaster], False, TgexJpegImage);
  FileFormatList.RegisterFileFormat('jfif', '',           gesJFIFImages, [ftRaster], False, TgexJpegImage);
end;

initialization
  InitgexJpeg;
end.
