{
  gexMayaIFF Class for loading Maya IFF images.
  Dual License: MPL 1.1 or LGPL 2.1 with linking exception (the "FPC modified LGPL License")
  Portions Created by Jacob Boerema are Copyright (C) 2015 Jacob Boerema.
  All Rights Reserved.
}
unit gexMayaIFF;

interface

{$I GraphicConfiguration.inc}
{$I gexdefines.inc}

{$IFNDEF FPC}
{$I Compilers.inc}

{$ifdef COMPILER_7_UP}
  // For some things to work we need code, which is classified as being unsafe for .NET.
  // We switch off warnings about that fact. We know it and we accept it.
  {$warn UNSAFE_TYPE off}
  {$warn UNSAFE_CAST off}
  {$warn UNSAFE_CODE off}
{$endif COMPILER_7_UP}
{$ENDIF}

uses Classes, SysUtils,
  gexTypes, GraphicEx, GraphicCompression, gexIFF;

type
  TMayaIffGraphic = class(TIffGraphicBase)
  private
    FImagePropertiesLoaded: Boolean;
    FTiles: Word;
    FData: TMemoryData;
    FHasZBuffer: Boolean;
    FFlags: Cardinal;

    function  ReadUncompressedTile(const Data: PMemoryData; AWidth, AHeight, ADepth: Word): TByteArray;
    function  ReadCompressedTile(const Data: PMemoryData; CompressedSize: Cardinal): TByteArray;
    function  DecompressRLEtile(Decoder: TDecoder; AWidth, AHeight, ADepth: Word;
      CompressedData: TByteArray; CompressedDataSize: Cardinal): TByteArray;
  public
    constructor Create; override;
    class function CanLoad(const Memory: Pointer; Size: Int64): Boolean; override;
    procedure LoadFromMemory(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal = 0); override;
    function ReadImageProperties(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal): Boolean; override;

    property HasZBuffer: boolean read FHasZBuffer default False;
  end;


resourcestring
  gesMayaIffImage = 'Maya IFF images';

procedure RegisterMayaIff;


implementation

uses Graphics, {$IFNDEF FPC}gexUtils,{$ENDIF} GraphicColor, GraphicStrings;

const
  // Maya flags values
  MAYA_RGB_FLAG     = 1;
  MAYA_ALPHA_FLAG   = 2;
  MAYA_ZBUFFER_FLAG = 4;

const
  // MAYA Chunk ID's
  IFF_ID_TBHD      = $54424844; // TBHD
  IFF_ID_RGBA      = $52474241; // RGBA
  IFF_ID_ZBUF      = $5a425546; // ZBUF
  // CLPZ
  // ESXY
  // HIST
  // VERS
  // BLUR
  // BLRT
  // HIST

  // MAYA Group types
  IFF_TYPE_CIMG    = $43494d47; // CIMG
  IFF_TYPE_TBMP    = $54424D50; // TBMP


//------------------------------------------------------------------------------
//                           TMayaIffGraphic
//------------------------------------------------------------------------------

const
  cMAYA = 'Maya IFF';

constructor TMayaIffGraphic.Create;
begin
  inherited;
  FImagePropertiesLoaded := False;
  FHasZBuffer := False;
  IffType := cMAYA;
end;

//------------------------------------------------------------------------------

function TMayaIffGraphic.ReadUncompressedTile(const Data: PMemoryData;
  AWidth, AHeight, ADepth: Word): TByteArray;
var
  TileData: TByteArray;
  i, j: Integer;
  BytesRead: Cardinal;
  TileSize: Cardinal;
  PixelPtr: PByte;
  Bgra: PBGRA;
  Alpha: Byte;
begin
  TileSize := AWidth * AHeight * ADepth; // TODO: * BitsPerSample div 8
  SetLength(TileData, TileSize);

  BytesRead := ReadIffData(Data, TileSize, @TileData[0]);
  if BytesRead < TileSize then
    raise EgexInvalidGraphic.CreateFmt(gesStreamReadError, [IffType]);

  if ADepth = 4 then begin
    // Convert from ABGR to BGRA
    // TODO: Move to ColorManager
    PixelPtr := @TileData[0];
    for i := 0 to AHeight-1 do begin
      for j := 0 to AWidth-1 do begin
        // Note: Untested! (We don't have any samples like this.)
        Bgra   := PBGRA(PixelPtr);
        Alpha  := PixelPtr^; Inc(PixelPtr);
        Bgra.B := PixelPtr^; Inc(PixelPtr);
        Bgra.G := PixelPtr^; Inc(PixelPtr);
        Bgra.R := PixelPtr^; Inc(PixelPtr);
        Bgra.A := Alpha;
      end;
    end;
  end;
  Result := TileData;
end;

//------------------------------------------------------------------------------

function TMayaIffGraphic.ReadCompressedTile(const Data: PMemoryData; CompressedSize: Cardinal): TByteArray;
var Buffer: TByteArray;
    BytesRead: Cardinal;
begin
  SetLength(Buffer, CompressedSize);
  BytesRead := ReadIffData(Data, CompressedSize, @Buffer[0]);

  if BytesRead < CompressedSize then
    raise EgexInvalidGraphic.CreateFmt(gesStreamReadError, [IffType]);

  Result := Buffer;
end;

//------------------------------------------------------------------------------

// Decompress and convert the RGB(A) channels
function TMayaIffGraphic.DecompressRLEtile(Decoder: TDecoder; AWidth, AHeight, ADepth: Word;
  CompressedData: TByteArray; CompressedDataSize: Cardinal): TByteArray;
var
  Channels: array[0..3] of TByteArray;
  ByteData: TByteArray;
  i, row: Integer;
  ChannelSize: Cardinal;
  CompressedDataPtr, OldPtr, DataPtr: Pointer;
  ChannelPtrs: array [0..3] of Pointer;
begin
  ChannelSize := AWidth * AHeight {*bitspersample div 8};
  CompressedDataPtr := @CompressedData[0];
  OldPtr := CompressedDataPtr;

  // Loop over all abgr channels and decompress image data
  for i := 0 to ADepth-1 do begin
    SetLength(Channels[i], ChannelSize);
    Decoder.Decode(CompressedDataPtr, Pointer(Channels[i]), CompressedDataSize, ChannelSize);
    if Decoder.DecoderStatus <> dsOK then
      raise EgexInvalidGraphic.CreateFmt(gesDecompression, [IffType]);
    Dec(CompressedDataSize, NativeUInt(CompressedDataPtr)-NativeUInt(OldPtr));
    OldPtr := CompressedDataPtr;
  end;

  // Now convert the image channels to interleaved bitmap data
  SetLength(ByteData, AWidth * AHeight * ADepth);
  if ADepth = 4 then begin
    // Channels are ABGR change order to RGBA that we can handle
    ChannelPtrs[0] := @Channels[3][0];
    ChannelPtrs[1] := @Channels[2][0];
    ChannelPtrs[2] := @Channels[1][0];
    ChannelPtrs[3] := @Channels[0][0];
  end
  else begin // Untested: We don't have any samples like this.
    ChannelPtrs[0] := @Channels[2][0];
    ChannelPtrs[1] := @Channels[1][0];
    ChannelPtrs[2] := @Channels[0][0];
  end;
  DataPtr := @ByteData[0];

  // Loop over all rows in this tile
  for row := 0 to AHeight-1 do begin
    // Convert planar RGB(A) to interleaved BGR(A)
    ColorManager.ConvertRow(ChannelPtrs, DataPtr, AWidth, $ff);
    Inc(PAnsiChar(DataPtr), AWidth * ADepth);
    for i := 0 to ADepth-1 do
      Inc(PAnsiChar(ChannelPtrs[i]), AWidth);
  end;

  // Free allocated memory
  for i := 0 to ADepth-1 do
    channels[i] := nil;

  // Set the Result to the collected and converted image data
  Result := ByteData;
end;

//------------------------------------------------------------------------------

const
  NOSWAP_IFF_ID_FOR4:   Cardinal = $34524f46; // FOR4
  NOSWAP_IFF_ID_FOR8:   Cardinal = $38524f46; // FOR8
  NOSWAP_IFF_TYPE_CIMG: Cardinal = $474d4943; // CIMG
  NOSWAP_IFF_ID_TBHD:   Cardinal = $44484254; // TBHD
  NOSWAP_IFF_ID_FVER:   Cardinal = $52455646; // FVER

// Determine if this is a Maya IFF file that we can load.
class function TMayaIffGraphic.CanLoad(const Memory: Pointer; Size: Int64): Boolean;
var ChunkInfo: TIffChunk;
    Run: PAnsiChar;
begin
  Result := False;
  if Size < SizeOf(ChunkInfo) then
    Exit;

  // Get the info of the first chunk
  Move(Memory^, ChunkInfo, SizeOf(ChunkInfo));

  // First chunk should be FOR4 or FOR8 with a type id of CIMG
  // Using "in [NOSWAP_IFF_ID_FOR4, NOSWAP_IFF_ID_FOR8]" here doesn't seem to work correctly in Fpc.
  if ((ChunkInfo.ckID.tag = NOSWAP_IFF_ID_FOR4) or (ChunkInfo.ckID.tag = NOSWAP_IFF_ID_FOR8)) and
     (ChunkInfo.ckType.tag = NOSWAP_IFF_TYPE_CIMG) and
     (SwapEndian(Chunkinfo.ckSize) + 8 = Size) then begin
    Run := Memory;
    Inc(Run, 12);
    // Soon after that we should have a TBHD chunk, but sometimes in between
    // that is a FVER chunk and maybe others are possible as well
    while True do  begin
      Move(Run^, ChunkInfo, SizeOf(ChunkInfo));
      if ChunkInfo.ckID.tag = NOSWAP_IFF_ID_TBHD then begin
        Result := True;
        break;
      end
      else // if chunkInfo.ckID.tag = NOSWAP_IFF_ID_FVER then begin
        Inc(Run, SwapEndian(ChunkInfo.ckSize) + 8);
      if NativeUInt(Run) > NativeUInt(Memory) + Size then
        break;
    end;
  end;
end;

//------------------------------------------------------------------------------

// Read Maya IFF image properties
function TMayaIffGraphic.ReadImageProperties(const Memory: Pointer; Size: Int64;
  ImageIndex: Cardinal): Boolean;
var
  ChunkInfo: PDataChunk;
  //mWord1, mWord2: Word;
  mDatatype: Cardinal;
  CompressionType: Cardinal;
begin
  Result := False;
  if not inherited ReadImageProperties(Memory, Size, ImageIndex) then
    Exit;

  // Init Memory data
  FData.mdStart := Memory;
  FData.mdSize  := Size;
  FData.mdPos   := Memory;
  FData.mdEnd   := PByte(NativeUInt(Memory) + Size);

  // First chunk should be FOR4 or FOR8 with a tag type CIMG
  ChunkInfo := ReadIffChunk(@FData);
  if chunkInfo.ChunkType.tag <> IFF_TYPE_CIMG then begin
    // Should never happen since CanLoad checks for presence of CIMG
    // We Exit and return False
    Exit;
  end;

  // Now find the TBHD bitmap header and read the relevant information
  while NativeUInt(FData.mdPos) < NativeUInt(FData.mdEnd) do with FImageProperties do begin
    // Read the next chunk
    ChunkInfo := ReadIffChunk(@FData);

    // Check the chunk ID
    case ChunkInfo.ChunkID.tag of
      IFF_ID_TBHD:
        begin
          // This is the Header chunk: read image info
          Width  := ReadIffUInt32(@FData);    // Image width
          Height := ReadIffUInt32(@FData);    // Image height
          // Commented out results to remove hint that result is never used.
          {mWord1 :=} ReadIffUInt16(@FData);    // Unknown Word 1
          {mWord2 :=} ReadIffUInt16(@FData);    // Unknown Word 2
          FFlags := ReadIffUInt32(@FData);    // Flags: RGB, Alpha, ZBUFFER bits
          mDatatype := ReadIffUInt16(@FData);
          FTiles  := ReadIffUInt16(@FData);
          CompressionType := ReadIffUInt32(@FData);

          SeekNextIffChunk(@FData);

          // Now interpret some of the info

          // Image type
          case FFlags and (MAYA_RGB_FLAG+MAYA_ALPHA_FLAG) of
            MAYA_RGB_FLAG:
              begin
                SamplesPerPixel := 3;
                ColorScheme := csRGB;
              end;
            MAYA_RGB_FLAG+MAYA_ALPHA_FLAG:
              begin
                SamplesPerPixel := 4;
                ColorScheme := csRGBA;
              end
          else
            SamplesPerPixel := 0;
          end;

          // Data type of samples
          // Note: We do not have sample images other than datatype = 0
          // Therefor the other data type have not been implemented yet
          case mDataType of
            0: BitsPerSample := 8;
            1: BitsPerSample := 16; // Todo: not implemented yet
            3:
              begin
                SampleFormat := Ord(sdfFloat);
                BitsPerSample := 32; // Todo: not implemented yet
              end;
          else
            // Assume BitsPerSample = 8
            BitsPerSample := 8;
          end;

          BitsPerPixel := BitsPerSample * SamplesPerPixel;

          // Which type of compression do we have
          case CompressionType of
            0 : Compression := ctNone;
            1 : Compression := ctRLE;
          else
            Compression := ctUnknown;
          end;

          // Does it have a zbuffer
          if FFlags and MAYA_ZBUFFER_FLAG > 0 then
            FHasZBuffer := True;

          // IFF is always big endian
          Options := [ioBigEndian];

          break;
        end;
    end;

    SeekNextIffChunk(@FData);
  end; // while not found TBHD chunk

  Result := CheckBasicImageProperties();
  FImagePropertiesLoaded := True;
end;

//------------------------------------------------------------------------------

// Load Maya IFF image
procedure TMayaIffGraphic.LoadFromMemory(const Memory: Pointer; Size: Int64;
  ImageIndex: Cardinal = 0);
var
  ChunkInfo: PDataChunk;
  GroupType: TIffGroupType;
  depth: Cardinal;
  Done: Boolean;
  TileX1, TileX2, TileY1, TileY2, TileWidth, TileHeight: Word;
  TileNumber: Cardinal;
  TileData: TByteArray;
  CompressedTile: Boolean;
  CompressedTileSize: Cardinal;
  CompressedTileData: TByteArray;
  i: Cardinal;
  Decoder: TTargaRLEDecoder;
begin
  try
    if not FImagePropertiesLoaded then
      if not ReadImageProperties(Memory, Size, ImageIndex) then
        if FLastErrorReason <> '' then
          GraphicExError(gesInvalidImageEx, [IffType, FLastErrorReason])
        else
          GraphicExError(gesInvalidImage, [IffType]);

    if (FImageProperties.Width <= 0) or (FImageProperties.Height <= 0) then
      Exit;

    depth := FImageProperties.SamplesPerPixel;

    if not (depth in [3, 4]) then
      // Unsupported Samples per pixel image type
      raise EgexInvalidGraphic.CreateFmt(gesInvalidColorFormat, [IffType]);

    // Now look for a chunk of type TBMP
    Done := False;
    while not Done and (NativeUInt(FData.mdPos) < NativeUInt(FData.mdEnd)) do begin
      // Get info for next chunk
      ChunkInfo := ReadIffChunk(@FData);

      // Is this a TBMP chunk?
      if IsGroupChunk(ChunkInfo.ChunkID, GroupType) and (GroupType = igtForm) and
         (chunkInfo.chunkType.tag = IFF_TYPE_TBMP) then begin
        // We found a TBMP chunk

        // Set up ColorManager...
        ColorManager.SourceColorScheme := FImageProperties.ColorScheme;
        ColorManager.SourceBitsPerSample := FImageProperties.BitsPerSample;
        ColorManager.SourceSamplesPerPixel := FImageProperties.SamplesPerPixel;
        ColorManager.SourceOptions := ColorManager.SourceOptions + [coSeparatePlanes];
        if FImageProperties.BitsPerSample = 16 then
          ColorManager.SourceOptions := ColorManager.SourceOptions + [coNeedByteSwap]
        else if FImageProperties.BitsPerSample = 32 then
          // Not sure if float needs coByteSwap too, we don't have an example of this format
          ColorManager.SourceDataFormat := sdfFloat;

        // Select target color scheme
        ColorManager.SelectTarget;
        PixelFormat := ColorManager.TargetPixelFormat;
        // Set target dimensions
        Self.Width := FImageProperties.Width;
        Self.Height := FImageProperties.Height;

        // Setup Decoder for compressed tiles.
        Decoder := TTargaRLEDecoder.Create(8);
        try
          TileNumber := 0;
          // loop over all tiles and grab the image data
          while tileNumber < FTiles do begin

            // Get info for next chunk
            ChunkInfo := ReadIffChunk(@FData);
            if (chunkInfo.ChunkID.tag <> IFF_ID_RGBA) and
               (chunkInfo.ChunkID.tag <> IFF_ID_ZBUF) then begin
              // This is a chunk that we don't know how to handle: skip it.
              // TODO: Issue a WARNING: Unknown chunk ID inside TBMP
              // Some other chunk: skip
              SeekNextIffChunk(@FData);
            end
            else begin
              CompressedTileSize := chunkInfo.ChunkSize - 8;

              // Get tile size and location info
              TileX1     := ReadIffUInt16(@FData);
              TileY1     := ReadIffUInt16(@FData);
              TileX2     := ReadIffUInt16(@FData);
              TileY2     := ReadIffUInt16(@FData);
              TileWidth  := TileX2 - TileX1 + 1;
              TileHeight := TileY2 - TileY1 + 1;

              // Note don't check the Compression type here!
              // Individual tiles can be with or without compression.
              // This is a check whether the current tile uses compression.
              if ChunkInfo.ChunkSize >= TileWidth * TileHeight * depth + 8 then
                CompressedTile := False
              else
                CompressedTile := True;

              // Read the RGBA tile
              if chunkInfo.ChunkID.tag = IFF_ID_RGBA then begin
                if CompressedTile then begin
                  CompressedTileData := ReadCompressedTile(@FData, CompressedTileSize);
                  TileData := DecompressRLEtile(Decoder, TileWidth, TileHeight, depth,
                    CompressedTileData, CompressedTileSize);
                  CompressedTileData := nil; // unallocate
                end
                else begin // Uncompressed tile
                  TileData := ReadUncompressedTile(@FData, TileWidth, TileHeight, depth);
                end;

                if TileData <> nil then begin
                  // Move this Tile of imagedata into our bitmap
                  // Take care to do a vertical flip of the data
                  for i := 0 to TileHeight-1 do
                    Move( Tiledata[TileWidth * depth * i],
                      Pointer(NativeUInt(Scanline[Cardinal(Height)-1 - (TileY1+i)]) + (TileX1*depth))^,
                      TileWidth * depth);
                    TileData := nil; // free
                end;

                SeekNextIffChunk(@FData);
                inc(tileNumber);
              end // end if RGBA
              else begin
                // ZBUF: Ignore for now
                SeekNextIffChunk(@FData);
              end;
            end; // end if RGBA | ZBUF
          end; // END while TBMP tiles
        finally
          Decoder.Free;
        end;
        // We can now exit the outer loop
        Done := True;
      end // END if TBMP
      else begin
        // Some other chunk: skip
        SeekNextIffChunk(@FData);
      end;
    end; // while
  finally
    // We are done: free all remaining chunk data
    FreeAllChunkData;
  end;
end;

//------------------------------------------------------------------------------

procedure RegisterMayaIff;
begin
  FileFormatList.RegisterFileFormat('iff', gesMayaIffImage, '', [ftRaster], False, TMayaIffGraphic);
end;

initialization
  RegisterMayaIff;
end.
