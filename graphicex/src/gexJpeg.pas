{ gexJpeg Unit to interface with LibJpeg(Turbo) to read jpeg images.
  Dual License: MPL 1.1 or LGPL 2.1 with linking exception (the "FPC modified LGPL License")
  Portions Created by Jacob Boerema are Copyright (C) 2013-2015 Jacob Boerema.
  All Rights Reserved.
  This fork of GraphicEx can be found at https://bitbucket.org/jacobb/graphicex
}

unit gexJpeg;

interface

{$I gexdefines.inc}

{$IFNDEF FPC}
  // Delphi
  {$I Compilers.inc}
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
    FJpegSaveInfo: j_compress_ptr;
    FJpegErr: jpeg_error_mgr_ptr;
    {$IFDEF JPEG_MEASURE_SPEED}
    FLibJpegTicks: Int64;
    FConversionTicks: Int64;
    {$ENDIF}
    FPerformance: TJPEGPerformance;
    FQuality: TJPEGQualityRange;
    FScale: TJPEGScale;
    FAutoScaleLargeImage: Boolean;
    FAutoScaleMemoryLimit: UInt64;
  protected
    function InitJpegDecompress(const Memory: Pointer; const Size: Int64): Boolean;
    function InitJpegCompress(SaveStream: TStream): Boolean;
    function InitJpegErrorManager: jpeg_error_mgr_ptr; virtual;
    function InitJpegSourceManager(var SrcManager: jpeg_source_mgr_ptr;
      const Memory: Pointer; const Size: Int64): Boolean; virtual;
    function InitJpegDestManager(var DestManager: jpeg_destination_mgr_ptr;
      OutputStream: TStream): Boolean; virtual;
    procedure Jpeg_RGB2BGR(const ASrcBuf: PByte; const ADestBuf: PByte;
      const ALineWidth, ANumComponents: Cardinal);
    procedure Jpeg_Copy(const ASrcBuf: PByte; const ADestBuf: PByte;
      const ALineWidth, ANumComponents: Cardinal);
    procedure Jpeg_ConvertRow(const ASrcBuf: PByte; const ADestBuf: PByte;
      const ALineWidth, {%H-}ANumComponents: Cardinal);

    function DoAutoScale: Boolean;
    procedure CheckJpegMarkers;
    procedure HandleExif(ExifData: PByte; ExifLen: Cardinal);
    function InternalReadImageProperties(const Memory: Pointer; Size: Int64;
      ImageIndex: Cardinal; ImageRequired: Boolean = False): Boolean;
  public
    constructor Create; override;
    destructor Destroy; override;

    class function CanLoad(const Memory: Pointer; Size: Int64): Boolean; override;
    function ReadImageProperties(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal): Boolean; override;
    procedure LoadFromMemory(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal = 0); override;
    procedure SaveToStream(Stream: TStream); override;
    {$IFDEF JPEG_MEASURE_SPEED}
    property LibJpegTicks: Int64 read FLibJpegTicks;
    property ConversionTicks: Int64 read FConversionTicks;
    {$ENDIF}
    property AutoScaleLargeImage: Boolean read FAutoScaleLargeImage write FAutoScaleLargeImage default False;
    property AutoScaleMemoryLimit: UInt64 read FAutoScaleMemoryLimit write FAutoScaleMemoryLimit;

    // TJpegImage compatible properties
    property CompressionQuality: TJPEGQualityRange read FQuality write FQuality;
    property Performance: TJPEGPerformance read FPerformance write FPerformance;
    property Scale: TJPEGScale read FScale write FScale default jsFullSize;
  end;

// Made public for use in unit testing.
procedure InitgexJpeg;

implementation

uses Graphics,
     Windows, // TMaxLogPalette
     {$IFNDEF FPC}
     {$IFDEF NEED_TJPEGIMAGE_SAVING}
     // For being able to unregister TJpegImage. However this is bad because it
     // unnecessarily links in their libjpeg objects. Only use if you need it
     // anyway for saving since we don't support that yet ourselves.
     jpeg,
     {$ENDIF NEED_TJPEGIMAGE_SAVING}
     {$ENDIF}
     {$IFDEF LCMS}
     lcms2dll, gexICC, // ICC profile manager
     {$ENDIF}
     gexTypes, gexExif, GraphicStrings, GraphicColor;

const
  BlockSize_FileMapping: Cardinal = High(Integer);
  BlockSize_MemoryStream: Cardinal = 16 * 1024;    // 16 KB

const
  cJpegSOIMarker = $d8ff;
  // APP1
  // Note: We explicitly declare this as AnsiString since that is how they are
  // stored inside Jpeg's and we use byte comparison using CompareMem to
  // check if we have found the correct marker.
  cExifMarker: AnsiString = 'Exif'#0#0;
  cExifMarkerLen          = 6;
  cXMPMarker: AnsiString  = 'http://ns.adobe.com/xap/1.0/'#0;
  cXMPMarkerLen           = 29;
  // APP2
  cICCMarker: AnsiString  = 'ICC_PROFILE'#0;
  cICCMarkerLen           = 12;
  cICCMarkerHeaderLen     = 14; // Minimum length of header for ICC

  OUTPUT_BUF_SIZE = 65536;   // 64K buffer

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

  TJpegDestData = record
    jpeg_dest: jpeg_destination_mgr;
    // Extra fields we need go here
    output_stream: TStream;
    buffer: JOCTET_ptr;
  end;
  PJpegDestData = ^TJpegDestData;

  TIccProfileRecord = packed record
    id: array [0..11] of AnsiChar;
    CurrentChunk: Byte; // Starts at 1
    TotalChunks: Byte;
  end;
  PIccProfileRecord = ^TIccProfileRecord;

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
function gexJpegFillInputBuffer(cinfo: j_decompress_ptr): JPEG_BOOLEAN; cdecl;
var
  JpegData: PJpegSourceData;
  BufBytes: Int64;
begin
  JpegData := PJpegSourceData(cinfo.Src);
  // Compute number of available bytes
  if JpegData.jpeg_pos + JpegData.jpeg_blocksize >= JpegData.jpeg_filesize then
    // Note that in a corrupt image jpeg_pos can be > than jpeg_filesize.
    // For that reason BufBytes needs to be Int64 and can't be unsigned.
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
function gexJpegResyncToRestart({%H-}cinfo: j_decompress_ptr; {%H-}Desired: Integer): JPEG_BOOLEAN; cdecl;
begin
  Result := False;
end;

//TJpegTermSource
procedure gexJpegTermSource({%H-}cinfo: j_decompress_ptr); cdecl;
begin
  // Nothing to be done here...
end;

////////////////////////////////////////////////////////////////////////////////
//                           Jpeg DataDest (save) handlers

// See jdatadst.c
procedure gexJpegInitDest(cinfo: j_compress_ptr); cdecl;
begin
  // Don't set output_stream to nil here. If we arrive here output_stream has
  // already been initialized to a valid stream.
  //PJpegDestData(cinfo.dest)^.output_stream := nil;
  PJpegDestData(cinfo.dest)^.buffer := cinfo^.mem^.alloc_small(j_common_ptr(cinfo),
    JPOOL_IMAGE, OUTPUT_BUF_SIZE * SizeOf(JOCTET));
  PJpegDestData(cinfo.dest)^.jpeg_dest.next_output_byte :=
    PJpegDestData(cinfo.dest)^.buffer;
  PJpegDestData(cinfo.dest)^.jpeg_dest.free_in_buffer := OUTPUT_BUF_SIZE;
end;

{
 * Empty the output buffer --- called whenever buffer fills up.
 *
 * In typical applications, this should write the entire output buffer
 * (ignoring the current state of next_output_byte & free_in_buffer),
 * reset the pointer & count to the start of the buffer, and return TRUE
 * indicating that the buffer has been dumped.
}
function gexJpegEmptyOutputBuffer(cinfo: j_compress_ptr): JPEG_BOOLEAN; cdecl;
begin
  with PJpegDestData(cinfo.dest)^ do
    if output_stream.Write(buffer^, OUTPUT_BUF_SIZE) <> OUTPUT_BUF_SIZE then
      raise EgexSaveError.Create(gesStreamWriteError);

  PJpegDestData(cinfo.dest)^.jpeg_dest.next_output_byte :=
    PJpegDestData(cinfo.dest)^.buffer;
  PJpegDestData(cinfo.dest)^.jpeg_dest.free_in_buffer := OUTPUT_BUF_SIZE;

  Result := True;
end;

{
 * Terminate destination --- called by jpeg_finish_compress
 * after all data has been written.  Usually needs to flush buffer.
 *
 * NB: *not* called by jpeg_abort or jpeg_destroy; surrounding
 * application must deal with any cleanup that should happen even
 * for error exit.
}
procedure gexJpegTermDestination(cinfo: j_compress_ptr); cdecl;
var
  DataCount: Integer;
begin
  with PJpegDestData(cinfo.dest)^ do begin
    DataCount := OUTPUT_BUF_SIZE - jpeg_dest.free_in_buffer;
    // Write data if there is any in the buffer
    if DataCount > 0 then begin
      if output_stream.Write(buffer^, DataCount) <> DataCount then
        raise EgexSaveError.Create(gesStreamWriteError);
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
//                           TgexJpegImage

constructor TgexJpegImage.Create;
begin
  inherited Create;
  // Allocate memory for jpeg data and zero fill it
  FJpegInfo := AllocMem(SizeOf(jpeg_decompress_struct));
  FJpegSaveInfo := nil;
  FJpegErr := AllocMem(SizeOf(jpeg_error_mgr));
  FScale := jsFullSize;
  FPerformance := jpBestQuality;
  FQuality := 90;
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

  // We want info about APP1 (Exif etc.), APP2 (ICC) and COM (commment) markers
  jpeg_save_markers(FJpegInfo, JPEG_APP0+1, $ffff);
  jpeg_save_markers(FJpegInfo, JPEG_APP0+2, $ffff);
  jpeg_save_markers(FJpegInfo, JPEG_COM, $ffff);

  Result := True;
end;

function TgexJpegImage.InitJpegCompress(SaveStream: TStream): Boolean;
begin
  if FJpegSaveInfo = nil then begin
    Result := False;
    Exit;
  end;
  // Initialize jpeg error manager
  FJpegSaveInfo.err := InitJpegErrorManager;

  // Initialize jpeg compress structure
  jpeg_create_compress(FJpegSaveInfo);

  // Initialize our destination manager
  InitJpegDestManager(FJpegSaveInfo.dest, SaveStream);

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

function TgexJpegImage.InitJpegDestManager(var DestManager: jpeg_destination_mgr_ptr;
  OutputStream: TStream): Boolean;
begin
  // Create compression info and fill with our handlers
  if DestManager = nil then begin
    // Get memory for the DestData buffer. Use JpegLib's memory allocation
    // that way we don't need to worry about freeing it.
    DestManager := FJpegSaveInfo.Mem^.alloc_small(j_common_ptr(FJpegSaveInfo),
      JPOOL_PERMANENT, SizeOf(TJpegDestData));
    // Initialize it
    DestManager.init_destination := gexJpegInitDest;
    DestManager.empty_output_buffer := gexJpegEmptyOutputBuffer;
    DestManager.term_destination := gexJpegTermDestination;
  end;
  // Initialize extra data for our handlers
  PJpegDestData(DestManager).output_stream := OutputStream;

  DestManager.next_output_byte := PJpegDestData(DestManager)^.buffer;
  DestManager.free_in_buffer := OUTPUT_BUF_SIZE;

  Result := True;
end;

procedure TgexJpegImage.HandleExif(ExifData: PByte; ExifLen: Cardinal);
var
  ExifReader: TSimpleExifReader;
  ExifOrientation: Byte;
begin
  // The first bytes will be the Exif marker (6 bytes), after that the real Exif data.
  Inc(ExifData, 6);
  Dec(ExifLen, 6);
  ExifReader := TSimpleExifReader.Create(ExifData, ExifLen);
  try
    ExifOrientation := ExifReader.Orientation;
    FImageProperties.Orientation := TgexOrientation(ExifOrientation);
  finally
    ExifReader.Free;
  end;
end;

procedure TgexJpegImage.CheckJpegMarkers;
type
  TIccData = record
    Icc: PByte;
    Len: Integer;
  end;
var
  CurrentMarker: jpeg_saved_marker_ptr;
  {$IFDEF LCMS}
  i: Integer;
  NumIcc: Integer;
  LastIcc: Integer;
  IccSize: Cardinal;
  IccBlocks: array of TIccData;
  TempICC, TempPos, SourcePos: PByte;
  {$ENDIF}
begin
  // Get first marker.
  CurrentMarker := FJpegInfo.marker_list;
  {$IFDEF LCMS}
  NumIcc := 0; LastIcc := 0;
  IccSize := 0;
  SetLength(IccBlocks, 0);
  {$ENDIF}
  while CurrentMarker <> nil do begin
    case CurrentMarker.marker of
      JPEG_COM:
        begin
          if (CurrentMarker.data_length > 0) and (PByte(CurrentMarker.data)^ <> 0) then
            if FImageProperties.Comment = '' then
              FImageProperties.Comment := string(PAnsiChar(CurrentMarker.data))
            else // concatenate comments separated by a LF
              FImageProperties.Comment := FImageProperties.Comment + #10 + string(PAnsiChar(CurrentMarker.data));
        end;
      JPEG_APP0+1:
        if CurrentMarker.data_length > cExifMarkerLen then begin
          if CompareMem(CurrentMarker.data, @cExifMarker[1], cExifMarkerLen) then
            HandleExif(PByte(CurrentMarker.data), CurrentMarker.data_length);
        end;
      {$IFDEF LCMS}
      JPEG_APP0+2:
        if CurrentMarker.data_length > cICCMarkerHeaderLen then begin
          if CompareMem(CurrentMarker.data, @cICCMarker[1], cICCMarkerLen) then begin
            // Found an ICC chunk (there can be multiple).
            if NumIcc = 0 then begin
              // First Icc chunk found.
              NumIcc := PIccProfileRecord(CurrentMarker.data)^.TotalChunks;
              SetLength(IccBlocks, NumIcc);
            end
            else if NumIcc <> PIccProfileRecord(CurrentMarker.data)^.TotalChunks then begin
              // Different number of ICC chunks according to this chunk. Something is wrong!
              // TODO: Add warning.
              NumIcc := 0; LastIcc := 0;
              CurrentMarker := CurrentMarker.next;
              SetLength(IccBlocks, 0);
              continue;
            end;

            // Check if current chunk number is valid
            if PIccProfileRecord(CurrentMarker.data)^.CurrentChunk = LastIcc+1 then begin
              IccBlocks[LastIcc].Icc := PByte(CurrentMarker.data);
              IccBlocks[LastIcc].Len := CurrentMarker.data_length-cICCMarkerHeaderLen;
              Inc(IccSize, IccBlocks[LastIcc].Len);
              Inc(LastIcc);

              if LastIcc = NumIcc then begin
                // We found all ICC blocks and now know the total size needed
                // so we can now assemble the ICC profile.
                if not Assigned(FICCManager) then
                  FICCManager := TICCProfileManager.Create;
                // Need memory for the profile
                GetMem(TempICC, IccSize);
                try
                  TempPos := TempICC;
                  for i := 0 to NumIcc-1 do begin
                    SourcePos := IccBlocks[i].Icc;
                    Inc(SourcePos, cICCMarkerHeaderLen);
                    Move(SourcePos^, TempPos^, IccBlocks[i].Len);
                    Inc(TempPos, IccBlocks[i].Len);
                  end;
                  FICCManager.LoadSourceProfileFromMemory(TempICC, IccSize);
                finally
                  FreeMem(TempICC);
                end;
              end;
            end;
          end;
        end;
      {$ENDIF}
    end;
    CurrentMarker := CurrentMarker.next;
  end;

  // Finally we signal that we are not interested in this data anymore and
  // that the memory used by it can be freed. We do this by setting max length to 0.
  jpeg_save_markers(FJpegInfo, JPEG_APP0+1, 0);
  jpeg_save_markers(FJpegInfo, JPEG_APP0+2, 0);
  jpeg_save_markers(FJpegInfo, JPEG_COM, 0);
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
    CheckJpegMarkers; // First check if there are jpeg markers we are interested in.
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
  buffer: array [0..0] of PByte;
  {$IFDEF JPEG_MEASURE_SPEED}
  TempTick: Int64;
  {$ENDIF}
  {$IFDEF LCMS}
  SourceColorMode: Cardinal;
  DoIccConversion: Boolean;
  {$ENDIF}
  AdjustmentBuffer, BufferPtr: PByte;
  TargetLineWidth: Cardinal;
  DoRotate: Boolean;
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

      {$IFDEF LCMS}
      SourceColorMode := TYPE_RGB_8;
      DoIccConversion := False;
      {$ENDIF}

      // From ReadImageProperties we already know what the OutColorSpace is going to be.
      // Note that this is not necessarily the same as FImageProperties.ColorScheme,
      // that's way the Source properties are explicitly set based on out_color_space.
      Colormanager.SourceBitsPerSample := FImageProperties.BitsPerSample;
      case FJpegInfo.out_color_space of
      JCS_RGB:
        begin
          ColorManager.SourceColorScheme := csRGB;
          ColorManager.SourceSamplesPerPixel := 3;
          {$IFDEF LCMS}
          SourceColorMode := TYPE_RGB_8;
          {$ENDIF}
        end;
      JCS_GRAYSCALE:
        begin
          ColorManager.SourceColorScheme := csG;
          ColorManager.SourceSamplesPerPixel := 1;
          {$IFDEF LCMS}
          SourceColorMode := TYPE_GRAY_8;
          {$ENDIF}
        end;
      JCS_CMYK, JCS_YCCK:
        begin
          ColorManager.SourceColorScheme := csCMYK;
          ColorManager.SourceSamplesPerPixel := 4;
          if FJpegInfo.saw_Adobe_marker then begin
            // Adobe PhotoShop uses inverted CMYK
            ColorManager.SourceOptions := ColorManager.SourceOptions + [coInvertedCMYK];
            {$IFDEF LCMS}
            SourceColorMode := TYPE_CMYK_8_REV;
            {$ENDIF}
          end
          {$IFDEF LCMS}
          else
            SourceColorMode := TYPE_CMYK_8;
          {$ENDIF}
        end;
      JCS_YCBCR:
        begin
          ColorManager.SourceColorScheme := csYCBCR;
          ColorManager.SourceSamplesPerPixel := 3;
        end;
      else
        // If we don't know what to do then use an empty converter:
        // just copy without any conversion.
        ColorManager.SourceColorScheme := csRGB;
        ColorManager.SourceSamplesPerPixel := 3;
      end;
      // Select target color scheme
      ColorManager.SelectTarget;
      PixelFormat := ColorManager.TargetPixelFormat;
      // Palette has to be set after setting PixelFormat
      if ColorManager.TargetColorScheme in [csG, csIndexed] then
        Palette := ColorManager.CreateGrayScalePalette(False);

      {$IFDEF LCMS}
      if (FICCManager <> nil) and (ColorManager.TargetColorScheme = csBGR) and FICCTransformEnabled
        and (FPerformance <> jpBestSpeed) then begin
        // Assume we have a source profile loaded
        FICCManager.CreateTransformAnyTosRGB(SourceColorMode, ColorManager.TargetSamplesPerPixel >= 4);
        DoIccConversion := True;
      end;
      {$ENDIF}

      // Set buffer to nil so in case of an error we will know whether it's assigned or not
      buffer[0] := nil;
      AdjustmentBuffer := nil;
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
        if FPerformance = jpBestSpeed then begin
          // Go for best speed, see also djpeg.c
          FJpegInfo.two_pass_quantize := False;
          FJpegInfo.dither_mode := JDITHER_NONE;
          FJpegInfo.dct_method := JDCT_IFAST;
          FJpegInfo.do_fancy_upsampling := False;
        end
        else begin
          // Go for best quality
          FJpegInfo.two_pass_quantize := True;
          FJpegInfo.dither_mode := JDITHER_FS;
          FJpegInfo.dct_method := JDCT_FLOAT;
          FJpegInfo.do_fancy_upsampling := True;
        end;

        // Start decompressor
        jpeg_start_decompress(FJpegInfo);

        // compute line width of line not adjusted to orientation
        // Don't use FImageProperties.Width below because output might be scaled down!
        TargetLineWidth := FJpegInfo.output_width * Cardinal(FJpegInfo.output_components);

        // Set dimensions after setting PixelFormat and after we know the output size
        DoRotate := False;
        if AutoCorrectOrientation and (Byte(FImageProperties.Orientation) > 1) then begin
          // First we need to make sure that we can allocate enough extra memory
          // to be able to do the rotation. We need an extra buffer the size of
          // all the image data. In case this fails with EOutOfMemory we will continue
          // showing the image without rotation.
          try
            // Get buffer for the whole image if possible.
            // Don't use FImageProperties.Height below since output can be scaled down!
            // Zero the memory. That way in case of problems we won't have random pixel colors.
            AdjustmentBuffer := AllocMem(FJpegInfo.output_height * TargetLineWidth);
          except
            on EOutOfMemory do begin
              AdjustmentBuffer := nil;
            end;
          end;
          if Assigned(AdjustmentBuffer) then
            case FImageProperties.Orientation of
              TgexOrientation.gexoRightTop:
                begin
                  Height := FJpegInfo.output_width;
                  Width := FJpegInfo.output_height;
                  DoRotate := True;
                end;
              TgexOrientation.gexoBottomRight:
                begin
                  Width := FJpegInfo.output_width;
                  Height := FJpegInfo.output_height;
                  DoRotate := True;
                end;
              TgexOrientation.gexoLeftBottom:
                begin
                  Height := FJpegInfo.output_width;
                  Width := FJpegInfo.output_height;
                  DoRotate := True;
                end;
            else
              DoRotate := True;
              Width := FJpegInfo.output_width;
              Height := FJpegInfo.output_height;
            end;
        end
        else begin
          Width := FJpegInfo.output_width;
          Height := FJpegInfo.output_height;
        end;

        // Buffer for one row of output from jpeg_read_scanlines
        GetMem(buffer[0], TargetLineWidth);
        if not DoRotate then begin
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
            if jpeg_read_scanlines(FJpegInfo, @buffer, 1) <> 1 then
              // This can happen in corrupt images!
              break;
            {$IFDEF JPEG_MEASURE_SPEED}Inc(FLibJpegTicks, GetTickCount64 - TempTick);{$ENDIF}

            {$IFDEF JPEG_MEASURE_SPEED}TempTick := GetTickCount64;{$ENDIF}
            {$IFDEF LCMS}
            if DoIccConversion then begin
              FICCManager.ExecuteTransform(buffer[0], ScanLine[FJpegInfo.output_scanline-1],
                FJpegInfo.output_width);
            end
            else
            {$ENDIF}
            begin
              // Convert this row to our target color scheme.
              ColorManager.ConvertRow([buffer[0]], ScanLine[FJpegInfo.output_scanline-1], FJpegInfo.output_width, $ff);
            end;
            {$IFDEF JPEG_MEASURE_SPEED}Inc(FConversionTicks, GetTickCount64 - TempTick);{$ENDIF}
          end;
          {$IFDEF FPC}
          EndUpdate(False);
          {$ENDIF}
        end
        else begin
          // We want to rotate. First get the image, then rotate it.

          {* Here we use the library's state variable cinfo.output_scanline as the
           * loop counter, so that we don't have to keep track ourselves.
           *}
          BufferPtr := AdjustmentBuffer;
          while (FJpegInfo.output_scanline < FJpegInfo.output_height) do begin
            {* jpeg_read_scanlines expects an array of pointers to scanlines.
             * Here the array is only one element long, but you could ask for
             * more than one scanline at a time if that's more convenient.
             *}
            if jpeg_read_scanlines(FJpegInfo, @buffer, 1) <> 1 then
              // This can happen in corrupt images!
              break;

            {$IFDEF LCMS}
            if DoIccConversion then begin
              FICCManager.ExecuteTransform(buffer[0], BufferPtr, FJpegInfo.output_width);
            end
            else
            {$ENDIF}
            begin
              // Convert this row to our target color scheme.
              ColorManager.ConvertRow([buffer[0]], BufferPtr, FJpegInfo.output_width, $ff);
            end;
            Inc(BufferPtr, TargetLineWidth);
          end;
          {$IFDEF FPC}
          BeginUpdate(False);
          {$ENDIF}
          // And now do the rotation
          case FImageProperties.Orientation of
            TgexOrientation.gexoRightTop:
              RotateRightTop(AdjustmentBuffer, FJpegInfo.output_width, FJpegInfo.output_components);
            TgexOrientation.gexoLeftBottom:
              RotateLeftBottom(AdjustmentBuffer, FJpegInfo.output_width, FJpegInfo.output_components);
            TgexOrientation.gexoBottomRight:
              RotateBottomRight(AdjustmentBuffer, FJpegInfo.output_width, FJpegInfo.output_components);
            TgexOrientation.gexoTopLeft:
              RotateTopLeft(AdjustmentBuffer, FJpegInfo.output_width, FJpegInfo.output_components);
          end;
          {$IFDEF FPC}
          EndUpdate(False);
          {$ENDIF}
        end;
        // Stop decompressor
        jpeg_finish_decompress(FJpegInfo);
      finally
        {$IFDEF LCMS}
        if DoIccConversion then
          FICCManager.DestroyTransform;
        {$ENDIF}
        if Assigned(buffer[0]) then
          FreeMem(buffer[0]);
        if Assigned(AdjustmentBuffer) then
          FreeMem(AdjustmentBuffer);
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

procedure TgexJpegImage.SaveToStream(Stream: TStream);
var
  row_pointer: array [0..0] of PByte;
  RowBuffer: PByte;
  SavingError: Boolean;
  LogPalette: TMaxLogPalette;
begin
  if (Width > 0) and (Height > 0) then begin
    // If we have a custom pixelformat then force it to pf24Bit.
    {$IFDEF FPC}
    // Changing PixelFormat on an existing image doesn't work for Lazarus/LCL.
    // Getting the palette doesn't work for pf1Bit.
    // For now we just exit but we should set or show an error, or even better
    // TODO: Think of a way to convert pf1Bit ourselves for Lazarus.
    // TODO: Lazarus pf15/16Bit turns out as grayscale.
    if PixelFormat in [pf1Bit, {pf4Bit, pf8Bit,} pfCustom, pfDevice] then
      Exit;
    {$ENDIF}
    if PixelFormat = pfCustom then
      PixelFormat := pf24Bit;
    Stream.Position := 0;
    RowBuffer := nil;
    SavingError := False;
    // Allocate memory for the jpeg compression structure
    FJpegSaveInfo := AllocMem(SizeOf(jpeg_compress_struct));
    try
      if not InitJpegCompress(Stream) then
        Exit;  // TODO: Add warning message

      // Initialize save parameters
      FJpegSaveInfo^.image_width := Width;
      FJpegSaveInfo^.image_height := Height;
      FJpegSaveInfo^.input_components := 3;     // Always 3 components for RGB
      FJpegSaveInfo^.in_color_space := JCS_RGB; // colorspace of input image

      // Since on Windows color format is BGR we will have to convert all image data.
      // First set up source
      ColorManager.SetSourceFromPixelFormat(PixelFormat);
      // if source is Indexed then we need to add a source palette
      if ColorManager.SourceColorScheme = csIndexed then begin
        if GetLogPaletteFromHPalette(Palette, @LogPalette) then begin
          ColorManager.SetSourcePalette([@LogPalette.palPalEntry], pfInterlaced8Quad);
        end
        else begin
          // Couldn't get palette, something is wrong.
          SavingError := True;
          Exit;
        end;
      end;
      // And then select RGB target
      ColorManager.SelectTargetRGB8;
      // Allocate memory for the conversion row buffer
      GetMem(RowBuffer, Width * 3);

      // Set default jpeg saving parameters (needs to be done after setting in_color_space).
      jpeg_set_defaults(FJpegSaveInfo);
      // Adjust parameters to our personal preferences.
      jpeg_set_quality(FJpegSaveInfo, FQuality, TRUE { limit to baseline-JPEG values });
      if FPerformance = jpBestSpeed then begin
        FJpegSaveInfo.dct_method := JDCT_IFAST; // We want best speed for saving
      end
      else begin
        FJpegSaveInfo.dct_method := JDCT_FLOAT; // We want best quality for saving
      end;

      // True ensures that we will write a complete interchange-JPEG file.
      jpeg_start_compress(FJpegSaveInfo, True);

      SavingError := True; // In case we get an exception we will know there was an error

      // Loop over all rows
      row_pointer[0] := RowBuffer; // Since RowBuffer doesn't change, assign outside loop
      while (FJpegSaveInfo.next_scanline < FJpegSaveInfo.image_height) do begin
        // First Convert a row of image data to the desired jpeg format
        ColorManager.ConvertRow([Scanline[FJpegSaveInfo.next_scanline]], RowBuffer, Width, $ff);
        {
         * jpeg_write_scanlines expects an array of pointers to scanlines.
         * Here the array is only one element long, but you could pass
         * more than one scanline at a time if that's more convenient.
         *
        }
        if jpeg_write_scanlines(FJpegSaveInfo, @row_pointer, 1) <> 1 then begin
          SavingError := False; // False here because below it's negated to True
          break;
        end;

        // TODO: Progress info
      end;

      // Finish compression
      jpeg_finish_compress(FJpegSaveInfo);
      SavingError := not SavingError;
    finally
      // If there was a problem saving then we should reset the stream size
      if SavingError then
        Stream.Size := 0;
      // TODO: Check the best way to make sure a corrupted save gets deleted.

      // Release JPEG compression object
      // This is an important step since it will release a good deal of memory.
      jpeg_destroy_compress(FJpegSaveInfo);
      // Free our own memory objects
      FreeMem(FJpegSaveInfo);
      FJpegSaveInfo := nil;
      if Assigned(RowBuffer) then
        FreeMem(RowBuffer);
    end;
  end;
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
