{
  gexAmigaIFF Class for loading Amiga IFF ilbm/pbm images.
  Dual License: MPL 1.1 or LGPL 2.1 with linking exception (the "FPC modified LGPL License")
  Portions Created by Jacob Boerema are Copyright (C) 2015 Jacob Boerema.
  All Rights Reserved.
}
unit gexAmigaIFF;

interface

{$I GraphicConfiguration.inc}

{$IFNDEF FPC}
{$I Compilers.inc}

{$ifdef COMPILER_7_UP}
  // For some things to work we need code, which is classified as being unsafe for .NET.
  // We switch off warnings about that fact. We know it and we accept it.
  {$warn UNSAFE_TYPE off}
  {$warn UNSAFE_CAST off}
  {$warn UNSAFE_CODE off}
{$endif COMPILER_7_UP}
{$ELSE}
  // fpc
  {$mode delphi}
{$ENDIF}

uses
  Classes, SysUtils,
  gexIFF, GraphicCompression;

type
  TIffType = (itIlbm, itPbm, itAcbm);
  TIffCompression = (icNone, icPackedBits, icVDATRLE);
  // For VDAT rle see: http://www.atari-wiki.com/?title=ST_Picture_Formats (IFF format)
  TCamgFlag = (cf0001, cf0002, cfLace, cf0008, cf0010, cf0020, cf0040,
    cfExtraHalfBrite, cf0100, cf0200, cf0400, cfHam, cfExtendedMode, cf02000, cfHiRes);
  TCamgFlags = set of TCamgFlag;

  TIlbmChunk = (idCtbl, idPchg, idSham);
  TIlbmChunks = set of TIlbmChunk;

  TShamPalette = array [0..15] of Word;
  TShamColorTable = array [0..19] of TShamPalette;
  TShamData = record
    sdVersion: Word; // Always 0
    sdColorTable: TShamColorTable;
  end;

  TAmigaIffProperties = record
    IffType: TIffType;
    nPlanes: Byte;
    CompressionType: Byte;
    DummyByte: Byte; // Seems it not always is a dummy
    CamgFlags: TCamgFlags;
    CamgHiFlags: Word;
    PalSize: Word;  // Number of palette indexes
    Mask: Byte;     // See constants below
    TransparentColor: Word;
    xAspect, yAspect: Byte;
    XOfs, YOfs: SmallInt;
    PageWidth, PageHeight: Word;
    // Optional extra stuff
    ExtraChunks: TIlbmChunks;
    ShamOfs: PByte;
    ShamSize: Cardinal;
    CtblOfs: PByte;
    CtblSize: Cardinal;
    PchgOfs: PByte;
    PchgSize: Cardinal;
  end;

  TAmigaIffGraphic = class(TIffGraphicBase)
  private
   FData: TMemoryData;
   FIffProperties: TAmigaIffProperties;
   FUseMaskForAlpha: Boolean;
   FUseTransparentColorForAlpha: Boolean;
  protected
    class function CanHandle(const MainIffChunk: TIffChunk): Boolean; override;
    function GetRowSize(): Cardinal;
    function HandleBody(Decoder: TDecoder): Boolean;
  public
    constructor Create; override;
    procedure LoadFromMemory(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal = 0); override;
    function ReadImageProperties(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal): Boolean; override;
    property IffProperties: TAmigaIffProperties read FIffProperties;
    // The one image we have that uses a Mask shows wrong if we use it to show Alpha
    // Therefor we use a property that is False by default to set if we want the
    // mask to be used to set the alpha channel.
    property UseMaskForAlpha: Boolean read FUseMaskForAlpha write FUseMaskForAlpha default False;
    // Same as above; However: this is only used for the RGBA case not the Indexed case
    // since that last one seems to be ok for most cases where it's turned on.
    property UseTransparentColorForAlpha: Boolean read FUseTransparentColorForAlpha
      write FUseTransparentColorForAlpha default False;
  end;

const
  // ILBM maks types
  mskNone                = 0;
  mskHasMask             = 1;
  mskHasTransparentColor = 2;
  mskLasso               = 3;

  // ILBM compression types
  cmpNone          = 0;
  cmpByteRun1      = 1;
  cmpVDAT          = 2; // Atari-ST special compression type

  // IFF ILBM image ID's
  IFF_ID_BMHD      = $424d4844; // BMHD
  IFF_ID_CMAP      = $434d4150; // CMAP
  IFF_ID_BODY      = $424f4459; // BODY
  IFF_ID_CAMG      = $43414d47; // CAMG

  // Additional ID's
  IFF_ID_SHAM      = $5348414d; // SHAM
  IFF_ID_CTBL      = $4354424c; // CTBL
  IFF_ID_PCHG      = $50434847; // PCHG

  // IFF ILBM image types
  IFF_TYPE_ILBM    = $494c424d; // ILBM
  IFF_TYPE_PBM     = $50424d20; // PBM

resourcestring
  gesAmigaIff = 'Amiga ilbm/pbm IFF images';

procedure RegisterAmigaIff;


implementation

uses Graphics, gexTypes, GraphicEx, GraphicColor, GraphicStrings;

//------------------------------------------------------------------------------
//                           TAmigaIffGraphic
//------------------------------------------------------------------------------

constructor TAmigaIffGraphic.Create;
begin
  inherited Create;
  IffType := 'Amiga IFF';
  FUseMaskForAlpha := False;
  FUseTransparentColorForAlpha := False;
end;

//------------------------------------------------------------------------------

function TAmigaIffGraphic.GetRowSize(): Cardinal;
var
  RowSizeInWords: Cardinal;
begin
  RowSizeInWords := (FImageProperties.Width + 15) div 16;

  Result := RowSizeInWords * 2;
end;

//------------------------------------------------------------------------------

// Override this base handler for a specific IFF file.
class function TAmigaIffGraphic.CanHandle(const MainIffChunk: TIffChunk): Boolean;
begin
  // Note Data in MainIffChunk hasn't been byte swapped!

  // We can handle ILBM and PBM types
  Result :=
    (MainIffChunk.ckID.tag = SwapEndian(IFF_ID_FORM)) and
    ((MainIffChunk.ckType.tag = SwapEndian(IFF_TYPE_ILBM)) or
    (MainIffChunk.ckType.tag = SwapEndian(IFF_TYPE_PBM)));
end;

//------------------------------------------------------------------------------

function TAmigaIffGraphic.ReadImageProperties(const Memory: Pointer; Size: Int64;
  ImageIndex: Cardinal): Boolean;
var
  ChunkInfo: PDataChunk;
  nPlanes: Word;
  Flags: Cardinal;
  GotCmap: Boolean;
  TempOfs: PByte;
begin
  Result := False;
  if not inherited ReadImageProperties(Memory, Size, ImageIndex) then
    Exit;

  // Init Memory data
  FData.mdStart := Memory;
  FData.mdSize  := Size;
  FData.mdPos   := Memory;
  FData.mdEnd   := Memory + Size;

  GotCMap := False;
  TempOfs := nil;

  FIffProperties.CamgFlags := [];

  try
    // file should begin with a FORM chunk of type ILBM or PBM
    ChunkInfo := ReadIffChunk(@FData);

    case ChunkInfo.ChunkType.tag of
      IFF_TYPE_ILBM:
        begin
          FIffProperties.IffType := itIlbm;
          IffType := IffType + '-ilbm';
        end;
      IFF_TYPE_PBM:
        begin
          FIffProperties.IffType := itPbm;
          IffType := Ifftype + '-pbm';
        end
    else
      // Unexpected IFF type, Exit and return false
      FreeAllChunkData;
      Exit;
    end;

    while FData.mdPos < FData.mdEnd do with FImageProperties do begin
      // Go to next Iff chunk
      ChunkInfo := ReadIffChunk(@FData);

      case ChunkInfo.ChunkID.tag of
        IFF_ID_BMHD:
          begin
            // Image header chunk found, read image info
            Width  := ReadIffUInt16(@FData);
            Height := ReadIffUInt16(@FData);
            FIffProperties.XOfs := ReadIffUInt16(@FData); // X Offset into a larger image
            FIffProperties.YOfs := ReadIffUInt16(@FData); // Y Offset into a larger image
            nPlanes := ReadIffUInt8(@FData); // # of source bitplanes
            FIffProperties.Mask := ReadIffUInt8(@FData); // choice of masking technique
            FIffProperties.CompressionType := ReadIffUInt8(@FData); // choice of compression algorithm
            FIffProperties.DummyByte  := ReadIffUInt8(@FData); // Dummy byte for alignment
            FIffProperties.TransparentColor := ReadIffUInt16(@FData);
            FIffProperties.xAspect    := ReadIffUInt8(@FData); // pixel aspect ratio
            FIffProperties.yAspect    := ReadIffUInt8(@FData); // pixel aspect ratio
            FIffProperties.PageWidth  := ReadIffUInt16(@FData); // Source page size
            FIffProperties.PageHeight := ReadIffUInt16(@FData); // Source page size

            case FIffProperties.CompressionType of
              cmpNone: Compression := ctNone;
              cmpByteRun1: Compression := ctPackedBits;
            else
            end;
            FIffProperties.nPlanes := nPlanes;

            case nPlanes of
              6, 24, 32:
                begin
                  if nPlanes = 6 then // Assume Ham
                    FIffProperties.CamgFlags := [cfHam];
                  BitsPerPixel  := nPlanes;
                  if nPlanes <> 6 then begin
                    BitsPerSample := 8;
                    SamplesPerPixel := BitsPerPixel div BitsPerSample;
                  end
                  else begin
                    SamplesPerPixel := 3;
                    BitsPerSample   := 2;
                  end;
                  if nPlanes = 32 then
                    ColorScheme := csRGBA
                  else
                    ColorScheme := csRGB;
                end;
              1..5, 7..8:
                // TODO: 8 planes without a CMAP is probably a Grayscale image
                // needs to be set to grayscale and add a grayscale palette!
                begin
                  if FIffProperties.Mask in [mskHasMask, mskHasTransparentColor] then begin
                    ColorScheme := csIndexedA;
                    BitsPerSample   := nPlanes+1;
                  end
                  else begin
                    ColorScheme := csIndexed;
                    BitsPerSample   := nPlanes;
                  end;
                  SamplesPerPixel := 1;
                  BitsPerPixel    := BitsPerSample;
                end
            else
            end;

            Result := True;
          end;
        IFF_ID_CAMG:
          begin
            Flags := ReadIffUInt32(@FData);
            FIffProperties.CamgFlags := TCamgFlags(Word(Flags));
            FIffProperties.CamgHiFlags := Flags shr 16;
            if cfHam in FIffProperties.CamgFlags then begin
              if FIffProperties.Mask = mskHasTransparentColor then begin
                SamplesPerPixel := 4;
                ColorScheme := csRGBA;
              end
              else begin
                SamplesPerPixel := 3;
                // ColorScheme not correct: It's really a mixture of RGB and Indexed
                ColorScheme := csRGB;
              end;
              // Change mode to cs RGB(A)
              BitsPerPixel  := nPlanes;
              BitsPerSample := BitsPerPixel div 3; // Wrong for 8 planes Ham but 6 is usual
            end
            else if nPlanes = 6 then begin
              // Our assumption was wrong (planes = 6 but not Ham):
              // Reset ColorScheme to Indexed
              if FIffProperties.Mask in [mskHasMask, mskHasTransparentColor] then begin
                ColorScheme := csIndexedA;
                BitsPerSample   := nPlanes+1;
              end
              else begin
                ColorScheme := csIndexed;
                BitsPerSample   := nPlanes;
              end;
              SamplesPerPixel := 1;
              BitsPerPixel    := BitsPerSample;
            end;
          end;
        IFF_ID_CMAP: // Palette
          begin
            // We need to handle this when loading image so store the position
            // of the palette
            GotCMap := True;
            FIffProperties.PalSize := ChunkInfo.ChunkSize div 3;
            TempOfs := ChunkInfo.ChunkOfs;
          end;
        IFF_ID_SHAM: // Sliced HAM palette found
          begin
            Include(FIffProperties.ExtraChunks, idSham);
            FIffProperties.ShamOfs := FData.mdPos;
            FIffProperties.ShamSize := ChunkInfo.Chunksize;
          end;
        IFF_ID_CTBL: // CTBL palette found
          begin
            Include(FIffProperties.ExtraChunks, idCtbl);
            FIffProperties.CtblOfs := FData.mdPos;
            FIffProperties.CtblSize := ChunkInfo.Chunksize;
          end;
        IFF_ID_PCHG: // PCHG palette found
          begin
            Include(FIffProperties.ExtraChunks, idPchg);
            FIffProperties.PchgOfs := FData.mdPos;
            FIffProperties.PchgSize := ChunkInfo.Chunksize;
          end;
        IFF_ID_BODY:
          // As soon as we get to the body we can stop
          begin
            if GotCMap then begin
              // We need to seek back to the CMAP chunk;
              // First finish this chunk
              SeekNextIffChunk(@FData);
              FData.mdPos := TempOfs;
            end
            else begin
              // This image doesn't have a CMAP. Go back to before reading BODY.
              TempOfs := ChunkInfo.ChunkOfs;
              SeekNextIffChunk(@FData);
              FData.mdPos := TempOfs;
            end;
            Exit; // Exit without doing another Seek
          end;
      else
      end; // case

      // Go to next chunk
      SeekNextIffChunk(@FData);
    end; // whilte True
  except
    FreeAllChunkData;
    raise;
  end;
end;

//------------------------------------------------------------------------------

// Handle BODY tag
function TAmigaIffGraphic.HandleBody(Decoder: TDecoder): Boolean;
type
  TRGBArray16 = array [0..15] of TRGB;
  PRGBArray16 = ^TRGBArray16;
var
  w, x, y, iBit,
  bitplane: Cardinal;
  TotalPlanes: Cardinal;
  BytesPerPixel: Cardinal;
  bitmask, mask: Byte;
  LineBuf, LineBufPos: PAnsiChar;
  PixelBuf: PAnsiChar;
  LineSize,
  PixelLineSize,
  ScanlineSize: Cardinal;
  Line: PAnsiChar;
  nSamples: Cardinal;
  FixIncorrectCompression: Boolean;
  BodyStart: PByte;
  AdjustedLineSize: Cardinal;
  AlphaIndex: Integer;
  GrayPal: PByte;
  ExtraPal: PRGBArray16;
  ExtraPalOfs: PByte;
  ExtraPalIsSham: Boolean;

  procedure UnpackPal(const ASource: PByte; const ARow: Cardinal; const IsSham: Boolean);
  var
    i: Cardinal;
    aRowMul: Cardinal;
    PalData: PByte;
  begin
    aRowMul := ARow;
    PalData := ASource;
    if IsSham then begin
      Inc(PalData, 2);
      if Height > 200 then
        aRowMul := aRowMul div (Height div 200);
    end;
    Inc(PalData, ARowMul * 16 *2);
    for i := 0 to 15 do begin
      // order (a)rgb each 4 bits (because not big endian)
      ExtraPal[i].R := PalData^ shl 4 or $0f;
      Inc(PalData);
      ExtraPal[i].G := PalData^ or $0f;
      ExtraPal[i].B := PalData^ shl 4 or $0f;
      Inc(PalData);
    end;
  end;
  procedure MakeGrayPal;
  var i, nItems, Factor: Cardinal;
  begin
    nItems := 1 shl FIffProperties.nPlanes;
    Factor := 255 div (nItems-1);
    GetMem(GrayPal, nItems * SizeOf(TRGB));
    for i := 0 to nItems-1 do begin
      PRGB(GrayPal)^.R := i * Factor;
      PRGB(GrayPal)^.G := PRGB(GrayPal)^.R;
      PRGB(GrayPal)^.B := PRGB(GrayPal)^.R;
    end;
  end;

begin
  LineBuf := nil;
  PixelBuf := nil;
  ExtraPal := nil;
  GrayPal := nil;
  {$IFDEF FPC}
  BeginUpdate(False);
  {$ENDIF}
  try
    ExtraPalOfs := nil;
    if (FIffProperties.ShamSize > 0) and (FIffProperties.nPlanes <= 6) then begin
      GetMem(ExtraPal, SizeOf(TRGBArray16));
      ExtraPalIsSham := True;
      ExtraPalOfs := FIffProperties.ShamOfs;
    end
    else if (FIffProperties.CtblSize > 0) and (FIffProperties.nPlanes <= 4) then begin
      GetMem(ExtraPal, SizeOf(TRGBArray16));
      ExtraPalIsSham := False;
      ExtraPalOfs := FIffProperties.CtblOfs;
    end;

    case FImageProperties.ColorScheme of
      csRGB, csRGBA:
        begin
          LineSize := GetRowSize;
          PixelLineSize := LineSize * FIffProperties.nPlanes;
          if cfHam in FIffProperties.CamgFlags then begin
            nSamples := 1;
            if (FIffProperties.Mask = mskHasTransparentColor) and FUseTransparentColorForAlpha then
              AlphaIndex := FIffProperties.TransparentColor
            else
              AlphaIndex := -1;
            // Needed for the ColorManager Ham converter:
            ColorManager.TargetSamplesPerPixel := FImageProperties.SamplesPerPixel;
            // Target ColorScheme either RGB(A) or BGR(A) defined when calling
            // the conversion routine (for now).
          end
          else
            nSamples := FImageProperties.SamplesPerPixel;
          GetMem(LineBuf, PixelLineSize * Height);
          GetMem(PixelBuf, Width*nSamples);
          AdjustedLineSize := PixelLineSize;

          repeat
            FixIncorrectCompression := False;
            BodyStart := FData.mdPos;
            for y := 0 to Height-1 do begin
              // Initialize all pixels in this line to black
              FillChar(PixelBuf^, Width*nSamples, 0);
              if FImageProperties.Compression = ctPackedBits then begin
                // We don't know the packed size. In rare cases packed size might be
                // more than unpacked size. To be safe we set it to twice PixelLineSize.
                Decoder.Decode(FData.mdPos, LineBuf, FData.mdEnd - FData.mdPos, AdjustedLineSize);
                if TPackbitsRLEDecoder(Decoder).Overflow then begin
                  // Incorrect LineSize due to broken image compression.
                  // Try again with fixed LineSize unless we already tried that.
                  if AdjustedLineSize = PixelLineSize then begin
                    // TODO: Add a warning message about broken compression
                    AdjustedLineSize := (Width+7) div 8 * FIffProperties.nPlanes;
                    FixIncorrectCompression := True;
                    FData.mdPos := BodyStart;
                  end;
                  // Test again because new AdjustedLineSize might be the same
                  // as the original PixelLineSize and we don't want an endless loop!
                  if AdjustedLineSize = PixelLineSize then begin
                    // We already tried fixing but it didn't help. Stop processing.
                    raise EgexInvalidGraphic.CreateFmt(gesDecompression, [IffType]);
                  end;
                  break; // Get out of the for loop
                end;
              end
              else
                if not ReadIffData(@FData, PixelLineSize, LineBuf) = PixelLineSize then
                  raise EgexInvalidGraphic.CreateFmt(gesStreamReadError, [IffType]);
              LineBufPos := LineBuf;
              for bitplane := 0 to FIffProperties.nPlanes-1 do begin
                bitmask := 1 shl (bitplane mod 8);
                Line := PixelBuf;
                // With 24 planes r starts at 0, g at 7 and b at 15
                Inc(Line, bitplane div 8);
                w := 0;
                for x := 0 to LineSize-1 do begin
                  mask := $80; // Mask to loop over all 8 bits
                  for iBit := 0 to 7 do begin
                    // TODO: Possibly move out of loop, do last bits separate
                    if w >= FImageProperties.Width then
                      Break
                    else
                      Inc(w);
                    // Compute bit value for this plane and add to pixel
                    if Byte(LineBufPos^) and mask <> 0 then
                      Line^ := AnsiChar(Byte(Line^) or bitmask);
                    // Go to the next pixel in Scanline
                    Inc(Line, nSamples);
                    mask := mask shr 1;
                  end;
                  Inc(LineBufPos);
                end; // for x
              end; // for bitplane

              if cfHam in FIffProperties.CamgFlags then begin
                if (FIffProperties.ShamSize <> 0) then begin
                  // Unpack sham palette for current line
                  UnpackPal(FIffProperties.ShamOfs, y, True);
                end;
                ColorManager.ConvertHam(PixelBuf, Scanline[y], Width, FIffProperties.nPlanes,
                  AlphaIndex, PByte(ExtraPal));
              end
              else begin
                // Convert RGB(A) to BGR(A)
                case FImageProperties.ColorScheme of
                  csRGB: RGBToBGR(PixelBuf, Width, 1);
                  csRGBA:RGBAToBGRA(PixelBuf, Width, 1);
                end;
                // Finally Copy this line of Pixels to the Scanline
                Move(PixelBuf^, Scanline[y]^, Width*nSamples);
              end;
            end; // for y
          until FixIncorrectCompression = False;
        end; // csRGB, csRGBA
      csIndexed,
      csIndexedA:
        begin
          // Check if a color map has been defined. If not define a grayscale one.
          if FIffProperties.PalSize = 0 then begin
            // Since ColorManager currently can't make a grayscale palette
            // that we can use here. It always gets assigned to the palette.
            // Thus we do it here.
            // N.B.: untested since we don't have an example without cmap
            MakeGrayPal;
            ColorManager.SetSourcePalette([GrayPal], pfInterlaced8Triple);
          end;

          // Set up ColorManager
          ColorManager.SourceBitsPerSample := 8;
          ColorManager.SourceSamplesPerPixel := 1;
          ColorManager.SourceColorScheme := FImageProperties.ColorScheme;
          ColorManager.TargetBitsPerSample := 8;
          if FImageProperties.ColorScheme = csIndexed then begin
            ColorManager.TargetSamplesPerPixel := 3;
            ColorManager.TargetColorScheme := csBGR;
          end
          else begin
            ColorManager.TargetSamplesPerPixel := 4;
            ColorManager.TargetColorScheme := csBGRA;
          end;

          if FIffProperties.IffType = itPbm then begin
            // Number of bytes for a line of pixels
            {.$IFDEF FPC}
            PixelLineSize := Width * ColorManager.TargetSamplesPerPixel;
            {.$ELSE}
            //PixelLineSize := Width;
            {.$ENDIF}

            // Get size of 1 line of data aligned on 16 bytes:
            LineSize := (Width + 15) div 16 * 16;
            AdjustedLineSize := LineSize;

            // Get memory for buffers
            GetMem(LineBuf, LineSize);       // Source
            GetMem(PixelBuf, PixelLineSize); // Dest

            repeat
              FixIncorrectCompression := False;
              BodyStart := FData.mdPos;
              // Iterate over all lines
              for y := 0 to Height-1 do begin
                // Initialize all pixels in this line to black
                FillChar(PixelBuf^, PixelLineSize, 0);
                if FImageProperties.Compression = ctPackedBits then begin

                  // We don't know the packed size. In rare cases packed size might be
                  // more than unpacked size. To be safe we set it to twice PixelLineSize.
                  // Although according to the specs we need to use LineSize, it causes
                  // problems decoding on some images, but using (Width+1) div 2 * 2
                  // gives problems on other images.
                  // Detect the images that give problems and redo it wiht a correction
                  Decoder.Decode(FData.mdPos, LineBuf, FData.mdEnd - FData.mdPos, AdjustedLineSize);
                  if TPackbitsRLEDecoder(Decoder).Overflow then begin
                    // Incorrect LineSize due to broken image compression.
                    // Try again with fixed LineSize unless we already tried that.
                    if AdjustedLineSize = LineSize then begin
                      // TODO: Add a warning message about broken compression
                      AdjustedLineSize := (Width+1) div 2 * 2;
                      FixIncorrectCompression := True;
                      FData.mdPos := BodyStart;
                    end;
                    // Test again because new AdjustedLineSize might be the same
                    // as the original PixelLineSize and we don't want an endless loop!
                    if AdjustedLineSize = LineSize then begin
                      // We already tried fixing but it didn't help. Stop processing.
                      raise EgexInvalidGraphic.CreateFmt(gesDecompression, [IffType]);
                    end;
                    break; // Get out of the for loop
                  end;
                end
                else
                  if not ReadIffData(@FData, LineSize, LineBuf) = LineSize then
                    raise EgexInvalidGraphic.CreateFmt(gesStreamReadError, [IffType]);
                {.$IFDEF FPC}
                ColorManager.ConvertRow([LineBuf], PixelBuf, Width, $ff);
                // Finally Copy this line of Pixels to the Scanline
                Move(PixelBuf^, Scanline[y]^, PixelLineSize);
                {.$ELSE}
                // Copy this line of palette data to the Scanline
                //Move(LineBuf^, Scanline[y]^, PixelLineSize);
                {.$ENDIF}
              end; // for y
            until FixIncorrectCompression = False;
          end
          else begin // Ilbm
            if FIffProperties.nPlanes > 8 then
              Exit; // Currently we can only handle <= 8 bits per sample Indexed
            if FIffProperties.Mask in [mskHasMask, mskHasTransparentColor] then begin
              if FIffProperties.Mask = mskHasMask then
                TotalPlanes := FIffProperties.nPlanes + 1
              else
                TotalPlanes := FIffProperties.nPlanes;
              ColorManager.SourceSamplesPerPixel := 2;
            end
            else begin
              TotalPlanes := FIffProperties.nPlanes;
            end;
            BytesPerPixel := ColorManager.SourceSamplesPerPixel;

            LineSize := GetRowSize;

            PixelLineSize := LineSize * TotalPlanes;
            GetMem(LineBuf, PixelLineSize);
            {.$IFDEF FPC}
            ScanlineSize := Width * BytesPerPixel;
            {.$ELSE}
            //ScanlineSize := Width;
            {.$ENDIF}
            GetMem(PixelBuf, ScanlineSize);

            for y := 0 to Height-1 do begin
              // Initialize all pixels in this line to black
              FillChar(PixelBuf^, ScanlineSize, 0);
              if FImageProperties.Compression = ctPackedBits then begin
                // We don't know the packed size. In rare cases packed size might be
                // more than unpacked size. To be safe we set it to twice PixelLineSize.
                Decoder.Decode(FData.mdPos, LineBuf, FData.mdEnd - FData.mdPos, PixelLineSize);
                if TPackbitsRLEDecoder(Decoder).Overflow then
                  raise EgexInvalidGraphic.CreateFmt(gesDecompression, [IffType]);
              end
              else
                if not ReadIffData(@FData, PixelLineSize, LineBuf) = PixelLineSize then
                  raise EgexInvalidGraphic.CreateFmt(gesStreamReadError, [IffType]);
              LineBufPos := LineBuf;
              for bitplane := 0 to FIffProperties.nPlanes-1 do begin
                bitmask := 1 shl bitplane;
                Line := PixelBuf;
                w := 0;
                for x := 0 to LineSize-1 do begin
                  mask := $80; // Mask to loop over all 8 bits
                  for iBit := 0 to 7 do begin
                    // TODO: Possibly move out of loop, do last bits separate
                    if w < FImageProperties.Width then begin
                      // Compute bit value for this plane and add to pixel
                      if Byte(LineBufPos^) and mask <> 0 then
                        Line^ := AnsiChar(Byte(Line^) or bitmask);
                      // Go to the next palette index in Scanline
                      Inc(Line, BytesPerPixel);
                    end;
                    Inc(w);
                    mask := mask shr 1;
                  end;
                  Inc(LineBufPos);
                end; // for x
              end; // for bitplane
              case FIffProperties.Mask of
                mskHasMask:
                  if FUseMaskForAlpha then begin
                    // The one file I have with a mask (Sexy_Nadja.IFF) shows
                    // wrong when we try to interpret the mask plane as alpha
                    // For now it's turned off by default until we have more
                    // examples to experiment with.
                    // Now handle the alpha mask (1 bit)
                    // Convert 0 to alpha 0 (invisible) and 1 to 255 (opaque)
                    Line := PixelBuf+1; // +1 to go to the alpha byte
                    w := 0;
                    for x := 0 to LineSize-1 do begin
                      mask := $80; // Mask to loop over all 8 bits
                      for iBit := 0 to 7 do begin
                        if w < FImageProperties.Width then begin
                          // Compute bit value for this plane and add to pixel
                          if Byte(LineBufPos^) and mask <> 0 then
                            Line^ := AnsiChar(255);
                          // Go to the next palette index in Scanline
                          Inc(Line, BytesPerPixel);
                        end;
                        Inc(w);
                        mask := mask shr 1;
                      end;
                      Inc(LineBufPos);
                    end; // for x
                  end;
                mskHasTransparentcolor:
                  begin
                    // If a pixel uses the TransparentColor index then set alpha
                    // to 0 else to 255 (opaqua)
                    Line := PixelBuf;
                    w := 0;
                    for x := 0 to FImageProperties.Width-1 do begin
                      // Check the pixel palette index to see if it's the TransparentColor index
                      if Byte(Line^) = FIffProperties.TransparentColor then begin
                        Inc(Line);
                        Byte(Line^) := 0;
                      end
                      else begin
                        Inc(Line);
                        Byte(Line^) := 255;
                      end;
                      Inc(Line);
                    end; // for x
                  end;
              end;
              // Convert Indexed
              {.$IFDEF FPC}
              if ExtraPalOfs <> nil then begin
                // Unpack palette for current line
                UnpackPal(ExtraPalOfs, y, ExtraPalIsSham);
                // Set ColorManager palette data
                ColorManager.SetSourcePalette([ExtraPal], pfInterlaced8Triple);
              end;
              ColorManager.ConvertRow([PixelBuf], Scanline[y], Width, $ff);
              // Finally Copy this line of Pixels to the Scanline
              //Move(PixelBuf^, Scanline[y]^, ScanlineSize);
              {.$ELSE}
              // Copy this line of palette data to the Scanline
              //Move(LineBuf^, Scanline[y]^, ScanlineSize);
              {.$ENDIF}
              // Finally Copy this line of Pixels to the Scanline
              //Move(PixelBuf^, Scanline[y]^, Width*FImageProperties.SamplesPerPixel);
          end; // for y
        end; // else ilbm
      end; // csIndexed case
    end; // case
  finally
    {$IFDEF FPC}
    EndUpdate(False);
    {$ENDIF}
    if Assigned(ExtraPal) then
      FreeMem(ExtraPal);
    if Assigned(GrayPal) then
      FreeMem(GrayPal);
    if Assigned(LineBuf) then
      FreeMem(LineBuf);
    if Assigned(PixelBuf) then
      FreeMem(PixelBuf);
  end;
  Result := True;
end;

//------------------------------------------------------------------------------

procedure TAmigaIffGraphic.LoadFromMemory(const Memory: Pointer; Size: Int64;
  ImageIndex: Cardinal = 0);
var
  ChunkInfo: PDataChunk;
  PalSize: Cardinal;
  EHBPal, TempPos, SrcPos: PAnsiChar;
  i: Cardinal;
  Decoder: TDecoder;
begin
  if not ReadImageProperties(Memory, Size, ImageIndex) then
    raise EgexInvalidGraphic.CreateFmt(gesInvalidImage, [IffType]);

  Decoder := nil;
  EHBPal := nil;
  try
    case FImageProperties.ColorScheme of
      csRGB: PixelFormat := pf24Bit;
      csRGBA: PixelFormat := pf32Bit;
      csIndexed: PixelFormat := pf24Bit;
      csIndexedA: PixelFormat := pf32Bit;
    end;
    Width := FImageProperties.Width;
    Height := FImageProperties.Height;
    if (Width <= 0) or (Height <= 0) then
      Exit;  // TODO: Add warning
    if FImageProperties.Compression = ctPackedBits then begin
      Decoder := TPackbitsRLEDecoder.Create;
      Decoder.DecodeInit;
      // We need the source ptr to be updated since we don't know the size of the
      // compressed data.
      TPackbitsRLEDecoder(Decoder).UpdateSource := True;
    end
    else if FImageProperties.Compression = ctUnknown then
      Exit; // TODO: Add warning: unknown compression

    while FData.mdPos < FData.mdEnd do with FImageProperties do begin

      ChunkInfo := ReadIffChunk(@FData);

      case ChunkInfo.ChunkID.tag of
        IFF_ID_BODY:
          begin
            HandleBody(Decoder);
            Break;
          end;
        IFF_ID_CMAP: // Palette
          begin
            PalSize := ChunkInfo.ChunkSize div 3; // 3 bytes rgb per palette entry
            if cfExtraHalfBrite in FIffProperties.CamgFlags then begin
              GetMem(EHBPal, 64 * 3); // 64 rgb palette entries
              // Copy first 32 entries from CMAP
              Move(FData.mdPos^, EHBPal^, ChunkInfo.ChunkSize);
              // Add 32 more half bright values
              SrcPos := EHBPal;
              TempPos := SrcPos;
              Inc(TempPos, 3*32);
              for i := 0 to 3*32-1 do begin
                TempPos^ := AnsiChar(Byte(SrcPos^) shr 1);
                Inc(TempPos); Inc(SrcPos);
              end;
              Palette := ColorManager.CreateColorPalette([FData.mdPos], pfInterlaced8Triple, 64);
              ColorManager.SetSourcePalette([EHBPal], pfInterlaced8Triple);
            end
            else begin
              Palette := ColorManager.CreateColorPalette([FData.mdPos], pfInterlaced8Triple, PalSize);
              ColorManager.SetSourcePalette([FData.mdPos], pfInterlaced8Triple);
            end;
          end
      else
      end; // case

      // Go to next chunk
      SeekNextIffChunk(@FData);
    end; // while
  finally
    if Assigned(EHBPal) then
      FreeMem(EHBPal);
    Decoder.Free;
    // Free memory taken by chunk data
    FreeAllChunkData;
  end;
end;

//------------------------------------------------------------------------------

procedure RegisterAmigaIff;
begin
  FileFormatList.RegisterFileFormat('ilbm', gesAmigaIff, '', [ftRaster], False, TAmigaIffGraphic);
  FileFormatList.RegisterFileFormat('lbm', gesAmigaIff, '', [ftRaster], False, TAmigaIffGraphic);
end;

initialization
  RegisterAmigaIff;
end.

