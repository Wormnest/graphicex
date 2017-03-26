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
  TIffType = (itIlbm, itPbm, itAcbm, itAnim, itRgbn, itRgb8, itDeep);
  TIffCompression = (icNone, icPackedBits, icVDATRLE, ic0003, icRGBN);
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

  // PCHG Palette header
  TPCHGHeader = record
     Compression: Word;
     Flags: Word;
     StartLine: SmallInt;
     LineCount: Word;
     ChangedLines: Word;
     MinReg: Word;
     MaxReg: Word;
     MaxChanges: Word;
     TotalChanges: LongWord;
  end;

  TPCHGCompHeader = record
     CompInfoSize: LongWord;
     OriginalDataSize: LongWord;
  end;

  TSmallLineChanges = record
     ChangeCount16: Byte;
     ChangeCount32: Byte;
     {PaletteChange[]: Word;}
  end;

  TBigLineChanges = record
     ChangeCount: Word;
    { struct BigPaletteChange PaletteChange[];}
  end;

  TBigPaletteChange = record
     PalRegister: Word;
     Alpha, Red, Blue, Green: Byte;
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
    PchgHeader: TPCHGHeader;
  end;

  TAmigaIffGraphic = class(TIffGraphicBase)
  private
   FData: TMemoryData;
   CMapOfs: PByte;
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
  cmpRGB           = 4; // RGBN/RGB8 compression type

  // IFF ILBM image ID's
  IFF_ID_BMHD      = $424d4844; // BMHD
  IFF_ID_CMAP      = $434d4150; // CMAP
  IFF_ID_BODY      = $424f4459; // BODY
  IFF_ID_CAMG      = $43414d47; // CAMG

  // IFF ACBM ID's
  IFF_ID_ABIT      = $41424954; // ABIT (same as BODY for ilbm)

  // IFF VDAT ID
  IFF_ID_VDAT      = $56444154; // VDAT (Always nPlanes count VDAT's inside a BODY)

  // Additional ID's
  IFF_ID_SHAM      = $5348414d; // SHAM
  IFF_ID_CTBL      = $4354424c; // CTBL
  IFF_ID_PCHG      = $50434847; // PCHG

  // IFF ILBM image types
  IFF_TYPE_ILBM    = $494c424d; // ILBM
  IFF_TYPE_PBM     = $50424d20; // PBM
  IFF_TYPE_ACBM    = $4143424d; // ACBM
  IFF_TYPE_ANIM    = $414e494d; // ANIM
  IFF_TYPE_RGBN    = $5247424e; // RGBN
  IFF_TYPE_RGB8    = $52474238; // RGB8
  IFF_TYPE_DEEP    = $44454550; // DEEP

  // PCHG Compression types
  PCHG_COMP_NONE     = 0;
  PCHG_COMP_HUFFMANN = 1;

  // PCHG Flags
  PCHGF_12BIT        = 1; // Use SmallLineChanges
  PCHGF_32BIT        = 2; // Use BigLineChanges
  PCHGF_USE_ALPHA    = 4; // Meaningful only of PCHGB_32BIT is on: use the Alpha

resourcestring
  gesAmigaIff = 'Amiga ilbm/pbm IFF images';

procedure RegisterAmigaIff;


implementation

uses Graphics, gexTypes, {$IFNDEF FPC}gexUtils,{$ENDIF} GraphicEx, GraphicColor, GraphicStrings;

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

  // We can handle ILBM, PBM, ACBM and ANIM types (ANIM only first frame)
  // For Delphi 6 we need to cast the IFF_ID consts to Cardinal othewise we
  // get an error ambiguous overloaded call to SwapEndian.
  Result := False;
  if MainIffChunk.ckID.tag = SwapEndian(Cardinal(IFF_ID_FORM)) then
    case SwapEndian(MainIffChunk.ckType.tag) of
      IFF_TYPE_ILBM,
      IFF_TYPE_PBM,
      IFF_TYPE_ACBM,
      IFF_TYPE_ANIM,
      IFF_TYPE_RGBN,
      IFF_TYPE_RGB8: Result := True;
    end;
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
  FData.mdEnd   := PBYte(NativeUInt(Memory) + Size);

  GotCMap := False;
  TempOfs := nil;
  nPlanes := 0;

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
        end;
      IFF_TYPE_ACBM:
        begin
          FIffProperties.IffType := itAcbm;
          IffType := Ifftype + '-acbm';
        end;
      IFF_TYPE_ANIM:
        begin
          FIffProperties.IffType := itAnim;
          IffType := Ifftype + '-anim';
        end;
      IFF_TYPE_RGBN:
        begin
          FIffProperties.IffType := itRgbn;
          IffType := Ifftype + '-rgbn';
        end;
      IFF_TYPE_RGB8:
        begin
          FIffProperties.IffType := itRgb8;
          IffType := Ifftype + '-rgb8';
        end;
    else
      // Unexpected IFF type, Exit and return false
      FreeAllChunkData;
      Exit;
    end;

    while NativeUInt(FData.mdPos) < NativeUInt(FData.mdEnd) do with FImageProperties do begin
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
                end;
              13, 25:
                if FIffProperties.IffType in [itRgbn, itRgb8] then begin
                  ColorScheme := csRGB;
                  BitsPerSample := 4;
                  SamplesPerPixel := 3;
                end;
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
            // Read the PCHG Header info
            FIffProperties.PchgHeader.Compression := ReadIffUInt16(@FData);
            FIffProperties.PchgHeader.Flags := ReadIffUInt16(@FData);
            FIffProperties.PchgHeader.StartLine := ReadIffUInt16(@FData);
            FIffProperties.PchgHeader.LineCount := ReadIffUInt16(@FData);
            FIffProperties.PchgHeader.ChangedLines := ReadIffUInt16(@FData);
            FIffProperties.PchgHeader.MinReg := ReadIffUInt16(@FData);
            FIffProperties.PchgHeader.MaxReg := ReadIffUInt16(@FData);
            FIffProperties.PchgHeader.MaxChanges := ReadIffUInt16(@FData);
            FIffProperties.PchgHeader.TotalChanges := ReadIffUInt32(@FData);
            // Set Pchg offset to first byte after header
            FIffProperties.PchgOfs := FData.mdPos;
            FIffProperties.PchgSize := ChunkInfo.Chunksize;
          end;
        IFF_ID_BODY, IFF_ID_ABIT:
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
        IFF_ID_FORM: // Form inside a Form happens for ANIM type
          begin
            Continue;
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
  TRGBArray32 = array [0..31] of TRGB;
  PRGBArray32 = ^TRGBArray32;
var
  w: Integer;
  x, y, iBit,
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
  PchgMask: array of LongWord;
  PchgMaskLength: Cardinal;
  PchgBit, PchgLong: Cardinal;
  PchgData: TMemoryData;
  PchgPal: PRGBArray32;
  VDatDecoded: PByte;

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
        aRowMul := aRowMul div (Cardinal(Height) div 200);
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
  function ReadACBMPlane: Boolean;
  var TempData: TMemoryData;
    PlaneSize: Cardinal;
    HeightPlaneSize: Cardinal;
    iPlane: Cardinal;
    BufPtr: PAnsiChar;
  begin
    Result := False;
    TempData := FData;
    // ACBM is planar: You get all plane 0 data first for all lines; then for plane 1..n
    // (ilbm is interleaved: plane 0..n for line 1 then plane 0..n for line 2 etc...)
    PlaneSize := GetRowSize;
    HeightPlaneSize := Cardinal(Height) * PlaneSize;
    BufPtr := LineBuf;
    for iPlane := 0 to FIffProperties.nPlanes-1 do begin
      // Compute source pos in file
      TempData.mdPos := FData.mdPos;
      Inc(TempData.mdPos, iPlane * HeightPlaneSize);
      if not ReadIffData(@TempData, PlaneSize, BufPtr) = PixelLineSize then
        Exit; // Returns False
      // Compute dest pos in LineBuf
      Inc(BufPtr, PlaneSize);
    end;

    // Finally update FData to position of PlaneSize bytes after first plane
    Inc(FData.mdPos, LineSize);

    Result := True;
  end;

  procedure DecodeVDAT;
  type PIffChunk = ^TIffChunk;
  var
    TempData: TMemoryData;
    Chunk: PIffChunk;
    vSize: Cardinal;
    i: Cardinal;
    vSource: Pointer;
    vPlaneTarget: Pointer;
    LinePlaneSize, HeightPlaneSize: Cardinal;
  begin
    TempData := FData;
    LinePlaneSize := GetRowSize;
    HeightPlaneSize := Cardinal(Height) * LinePlaneSize;
    GetMem(VDatDecoded, FIffProperties.nPlanes * HeightPlaneSize);

    vPlaneTarget := VDatDecoded;
    for i := 0 to FIffProperties.nPlanes-1 do begin
      // 1. Check to see if we really got a VDAT chunk
      Chunk := PIffChunk(TempData.mdPos);
      if SwapEndian(Chunk^.ckID.tag) = IFF_ID_VDAT then begin
        vSize := SwapEndian(Chunk^.ckSize);
        vSource := @Chunk^.ckType;
        Decoder.Decode(vSource, vPlaneTarget, vSize, HeightPlaneSize);
        if Decoder.DecoderStatus <> dsOk then
          raise EgexInvalidGraphic.CreateFmt(gesDecompression, [IffType]);
      end
      else
        break;
      // Update Target Pointer. Use of PByte required for Delphi Berlin.
      Inc(PByte(vPlaneTarget), HeightPlaneSize);
      // Update Source position
      if vSize and 1 <> 0 then
        Inc(vSize);
      Inc(TempData.mdPos, vSize+8);
    end;
  end;

  procedure LoadVDATLine;
  var
    iPlane, iColumn: Cardinal;
    SourcePtr: PByte;
    SourceWord: PWord;
    DestPtr: PWord;
    LinePlaneSize, HeightPlaneSize: Cardinal;
    LineWordCount: Cardinal;
    ColWordCount: Cardinal;
  begin
    SourcePtr := VDatDecoded;
    LinePlaneSize := GetRowSize;
    HeightPlaneSize := Cardinal(Height) * LinePlaneSize;
    LineWordCount := LinePlaneSize div 2;
    ColWordCount := Height;
    Inc(SourcePtr, y*2); // Start at correct line, word sized offset
    // VDAT has vertical oriented data
    // plane one contains line 1: word 0, line 2 word 0 ... line n word 0, line 1 word 3, ...
    // plane two line 1: word 1, line 2 word 1..line n word 1,  line 1 word 4 ...
    for iPlane := 0 to FIffProperties.nPlanes-1 do begin
      DestPtr := PWord(LineBuf);
      Inc(DestPtr, iPlane);
      SourceWord := PWord(SourcePtr);
      for iColumn := 0 to LineWordCount-1 do begin
        DestPtr^ := SourceWord^;
        Inc(DestPtr, 4);
        Inc(SourceWord, ColWordCount);
      end;
      Inc(SourcePtr, HeightPlaneSize);
    end;
  end;

  procedure GetPchgPal;
  var aLong: LongWord;
    PalOn: Boolean;
    ChangeCount16,
    ChangeCount32: Byte;
    i: Cardinal;
    aByte1,
    aByte2: Byte;
    RegIdx: Cardinal;
  begin
    // Get the next palette on/off bit
    aLong := PchgMask[PchgLong];
    PalOn := aLong and 1 <> 0;
    // Update bit/byte counters and long
    PchgMask[PchgLong] := aLong shr 1;
    Inc(PchgBit);
    if PchgBit = 32 then begin
      PchgBit := 0;
      Inc(PchgLong);
    end;
    if PalOn then begin
      // Palette used. Get the SmallLineChanges data
      ChangeCount16 := ReadIffUInt8(@PchgData); // Changes for palette registers 0-15
      // For now we will be ignoring the ChangeCount32 which is used to access
      // registers 16-31 because we don't have any examples that use that.
      ChangeCount32 := ReadIffUInt8(@PchgData);
      i := ChangeCount16;
      while i > 0 do begin
        // lower 12 bits (big endian) are rgb; high 4 bits register number
        aByte1:= ReadIffUInt8(@PchgData);
        aByte2:= ReadIffUInt8(@PchgData);
        RegIdx := aByte1 and $f0 shr 4;
        // Make 4 bits RGB values into 8 bits (lower 4 bits $0f)
        PchgPal^[RegIdx].R := aByte1 shl 4 or $0f;
        PchgPal^[RegIdx].G := aByte2 or $0f;
        PchgPal^[RegIdx].B := aByte2 shl 4 or $0f;
        Dec(i);
      end;
      // Currently we are not handling ChangeCount32 since we have never seen it
      // but we do skip it in case we encounter it
      // TODO: Palette registers 16-31 need updating
      i := ChangeCount32;
      while i > 0 do begin
        // Commented out results to remove hint that they are never used.
        {aByte1:=} ReadIffUInt8(@PchgData);
        {aByte2:=} ReadIffUInt8(@PchgData);
        Dec(i);
      end;
      // Update ExtraPal
      Move(PchgPal^, ExtraPal^, SizeOf(TRGBArray16));
    end;
  end;

begin
  Result := False;
  LineBuf := nil;
  PixelBuf := nil;
  ExtraPal := nil;
  GrayPal := nil;
  PchgMask := nil;
  PchgPal := nil;
  VDatDecoded := nil;
  {$IFDEF FPC}
  BeginUpdate(False);
  {$ENDIF}
  try
    ExtraPalOfs := nil;
    ExtraPalIsSham := False;
    if (FIffProperties.ShamSize > 0) and (FIffProperties.nPlanes <= 6) then begin
      GetMem(ExtraPal, SizeOf(TRGBArray16));
      ExtraPalIsSham := True;
      ExtraPalOfs := FIffProperties.ShamOfs;
    end
    else if (FIffProperties.CtblSize > 0) and (FIffProperties.nPlanes <= 4) then begin
      GetMem(ExtraPal, SizeOf(TRGBArray16));
      ExtraPalOfs := FIffProperties.CtblOfs;
    end
    else if (FIffProperties.PchgSize > 0) and (FIffProperties.nPlanes <= 4) and
      // Only type of PCHG I have samples of:
      (FIffProperties.PchgHeader.Compression = PCHG_COMP_NONE) and
      (FIffProperties.PchgHeader.Flags = PCHGF_12BIT) then begin
      GetMem(ExtraPal, SizeOf(TRGBArray16));
      GetMem(PchgPal, SizeOf(TRGBArray32));
      // Compute length of mask for PCHG
      PchgMaskLength := (FIffProperties.PchgHeader.LineCount+31) shr 5; // shr 5 = div 32
      SetLength(PchgMask, PchgMaskLength*SizeOf(LongWord));
      // Get mask data (mask bit set determines if a line uses PCHG palette or not)
      Move(FIffProperties.PchgOfs^, PchgMask[0], PchgMaskLength*SizeOf(LongWord));
      for w := 0 to PchgMaskLength-1 do
        SwapEndian(PchgMask[w]);
      ExtraPalOfs := PByte(NativeUInt(FIffProperties.PchgOfs) + PchgMaskLength*SizeOf(LongWord));
      PchgBit := 0;
      PchgLong := 0;
      PchgData := FData;
      PchgData.mdPos := ExtraPalOfs;
      // Copy CMAP palette as default palette to PchgPal
      if CMapOfs <> nil then begin
        Move(CMapOfs^, PchgPal^, SizeOf(TRGBArray16));
        // Copy to ExtraPal
        Move(PchgPal^, ExtraPal^, SizeOf(TRGBArray16));
      end;
    end;

    case FImageProperties.ColorScheme of
      csRGB, csRGBA:
        begin
          if FIffProperties.CompressionType <> cmpRGB then begin
            LineSize := GetRowSize;
            PixelLineSize := LineSize * FIffProperties.nPlanes;
          end
          else begin
            LineSize := FIffProperties.nPlanes div 6 * Width;
            PixelLineSize := LineSize;
          end;
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
          else begin
            nSamples := FImageProperties.SamplesPerPixel;
            AlphaIndex := -1;
          end;
          GetMem(LineBuf, PixelLineSize * Cardinal(Height));
          GetMem(PixelBuf, Cardinal(Width)*nSamples);
          AdjustedLineSize := PixelLineSize;

          repeat
            FixIncorrectCompression := False;
            BodyStart := FData.mdPos;
            for y := 0 to Height-1 do begin
              // Initialize all pixels in this line to black
              FillChar(PixelBuf^, Cardinal(Width)*nSamples, 0);
              if FIffProperties.CompressionType <> cmpRGB then begin
                case FIffProperties.CompressionType of
                  cmpByteRun1:
                    begin
                      // We don't know the packed size. In rare cases packed size might be
                      // more than unpacked size. To be safe we set it to twice PixelLineSize.
                      Decoder.Decode(Pointer(FData.mdPos), Pointer(LineBuf),
                        NativeUInt(FData.mdEnd) - NativeUInt(FData.mdPos), AdjustedLineSize);
                      // Note that we can't test Decoder.DecoderStatus because it can return
                      // a status other than dsOk because we don't have the exact input size!
                      if Decoder.DecompressedBytes <> Integer(AdjustedLineSize) then begin
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
                    end;
                  cmpNone:
                    if not ReadIffData(@FData, PixelLineSize, LineBuf) = PixelLineSize then
                      raise EgexInvalidGraphic.CreateFmt(gesStreamReadError, [IffType]);
                  cmpVDAT:
                    begin
                      // VDAT RLE compression handling is done elsewhere
                    end;
                else
                  // Unknown compression!
                end;
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
                  Move(PixelBuf^, Scanline[y]^, Cardinal(Width)*nSamples);
                end;
              end
              else begin // RGB Compression, RGBN/RGB8 Iff type
                Decoder.Decode(Pointer(FData.mdPos), Pointer(LineBuf),
                  NativeUInt(FData.mdEnd) - NativeUInt(FData.mdPos), AdjustedLineSize);
                if Decoder.DecompressedBytes <> Integer(AdjustedLineSize) then
                  raise EgexInvalidGraphic.CreateFmt(gesDecompression, [IffType]);

                // Now decode/unpack LineBuf to pixels
                case FIffProperties.IffType of
                  itRGBN:
                    begin
                      X4R4G4B4ToBGR(LineBuf, PixelBuf, Width, 1);
                    end;
                  itRGB8:
                    begin
                      XRGBToBGR(LineBuf, PixelBuf, Width, 1);
                    end;
                end;
                // Finally Copy this line of Pixels to the Scanline
                Move(PixelBuf^, Scanline[y]^, Cardinal(Width)*nSamples);
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
                  Decoder.Decode(Pointer(FData.mdPos), Pointer(LineBuf),
                    NativeUInt(FData.mdEnd) - NativeUInt(FData.mdPos), AdjustedLineSize);
                  // Note that we can't test Decoder.DecoderStatus because it can return
                  // a status other than dsOk because we don't have the exact input size!
                  if Decoder.DecompressedBytes <> Integer(AdjustedLineSize )then begin
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
            ScanlineSize := Cardinal(Width) * BytesPerPixel;
            {.$ELSE}
            //ScanlineSize := Width;
            {.$ENDIF}
            GetMem(PixelBuf, ScanlineSize);

            if FIffProperties.CompressionType = cmpVDAT then begin
              DecodeVDAT;
            end;

            for y := 0 to Height-1 do begin
              // Initialize all pixels in this line to black
              FillChar(PixelBuf^, ScanlineSize, 0);
              if FImageProperties.Compression = ctPackedBits then begin
                // We don't know the packed size. In rare cases packed size might be
                // more than unpacked size. To be safe we set it to twice PixelLineSize.
                Decoder.Decode(Pointer(FData.mdPos), Pointer(LineBuf),
                  NativeUInt(FData.mdEnd) - NativeUInt(FData.mdPos), PixelLineSize);
                // Note that we can't test Decoder.DecoderStatus because it can return
                // a status other than dsOk because we don't have the exact input size!
                if Decoder.DecompressedBytes <> Integer(PixelLineSize) then
                  raise EgexInvalidGraphic.CreateFmt(gesDecompression, [IffType]);
              end
              else if FIffProperties.IffType <> itAcbm then begin
                if FIffProperties.CompressionType = cmpVDAT then begin
                  LoadVDATLine;
                end
                else
                  if not ReadIffData(@FData, PixelLineSize, LineBuf) = PixelLineSize then
                    raise EgexInvalidGraphic.CreateFmt(gesStreamReadError, [IffType]);
              end
              else begin // ACBM
                if not ReadACBMPlane then
                  raise EgexInvalidGraphic.CreateFmt(gesStreamReadError, [IffType]);
              end;
              if FIffProperties.CompressionType <> cmpVDAT then begin
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
              end
              else begin
                // VDAT handling
                // 4 planes means we have 2 pixel indexes per byte
                // Move each pixel to a separate byte
                LineBufPos := LineBuf;
                Line := PixelBuf;
                w := 0;
                while w < FImageProperties.Width do begin
                  Line^:= AnsiChar(Byte(LineBufPos^) and $0f); Inc(Line);
                  Line^:= AnsiChar(Byte(LineBufPos^) shr 4 and $0f); Inc(Line);
                  Inc(LineBufPos);
                  Inc(w, 2);
                end;
              end;
              case FIffProperties.Mask of
                mskHasMask:
                  begin
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
                          if not FUseMaskForAlpha or (Byte(LineBufPos^) and mask <> 0) then
                            Byte(Line^) := $ff;
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
                if PchgMask <> nil then begin
                  // Read PCHG Palette for current line
                  // StartLine is 1-based, while y is 0-base
                  if Integer(y) >= FIffProperties.PchgHeader.StartLine-1 then
                    GetPchgPal;
                end
                else begin
                  // Unpack palette for current line
                  UnpackPal(ExtraPalOfs, y, ExtraPalIsSham);
                end;
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
    if Assigned(PchgPal) then
      FreeMem(PchgPal);
    if Assigned(GrayPal) then
      FreeMem(GrayPal);
    if Assigned(LineBuf) then
      FreeMem(LineBuf);
    if Assigned(PixelBuf) then
      FreeMem(PixelBuf);
    if Assigned(VDatDecoded) then
      FreeMem(VDatDecoded);
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
  CMapOfs := nil;
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
    case FIffProperties.CompressionType of
      cmpByteRun1:
        begin
          Decoder := TPackbitsRLEDecoder.Create;
          Decoder.DecodeInit;
          // We need the source ptr to be updated since we don't know the size of the
          // compressed data.
          TPackbitsRLEDecoder(Decoder).UpdateSource := True;
        end;
      cmpRGB:
        begin
          Decoder := TAmigaRGBDecoder.Create(FIffProperties.nPlanes div 6 * 8);
          Decoder.DecodeInit;
          TAmigaRGBDecoder(Decoder).UpdateSource := True;
        end;
      cmpVDAT:
        begin
          Decoder := TVDATRLEDecoder.Create;
          Decoder.DecodeInit;
          //TVDATRLEDecoder(Decoder).UpdateSource := True;
        end;
    else
      if FImageProperties.Compression = ctUnknown then
        Exit; // TODO: Add warning: unknown compression
    end;

    while NativeUInt(FData.mdPos) < NativeUInt(FData.mdEnd) do with FImageProperties do begin

      ChunkInfo := ReadIffChunk(@FData);

      case ChunkInfo.ChunkID.tag of
        IFF_ID_BODY, IFF_ID_ABIT:
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
              CMapOfs := FData.mdPos;
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

