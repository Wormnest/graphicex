{ gexXCF A Gimp XCF image format loader.
  Dual License: MPL 1.1 or LGPL 2.1 with linking exception (the "FPC modified LGPL License")
  Portions Created by Jacob Boerema are Copyright (C) 2013-2017 Jacob Boerema.
  All Rights Reserved.
  Based in part on xcftools.
  This fork of GraphicEx can be found at https://bitbucket.org/jacobb/graphicex

  TODO:
  - Hue: small differences: maybe caused by using integer conversion instead of floats?
  - A lot of the layer modes seems to have subtle differences which need to be investigated.
  - Layer modes other than normal for grayscale and indexed.
  - Maybe improve Progress sections.
  - Move all error strings to start of implementation.
  - Improve speed of blending layers together by converting whole lines at once
    (currently done pixel by pixel).
  - Saving.
  - More security checks for XCF images with invalid data.
  - Grayscale with alpha: Convert to pf32Bits and use the gray value for r, g and b.
  - Floating selection isn't taken into account although that is probably neccessary
    in certain cases. However since saving a floating selection is rare without
    attaching it we don't give this a high priority.
  - Extract separate layers, possibly even only certain tiles.
  - Create FileMapping instead of a temporary memory buffer for large images when extracting.
}
unit gexXCF;

interface

{$I gexdefines.inc}

{$I GraphicConfiguration.inc}

{$MINENUMSIZE 4}  // C compatible enums

uses SysUtils, Classes, GraphicEx, GraphicColor,
  gexLayers, gexXCFTypes, gexXCFBlendModes, GraphicCompression;

resourcestring
  gesXCF = 'Gimp XCF images';

type
  // XCF Image info to store XCF imaged data that doesn't have an exact representation
  // in FImageProperties from GraphicExGraphic
  TXcfImageInfo = record
    BaseType: Cardinal;         // Image basetype
    Compression: Byte;
    ColorMapColorCount: Integer; // Number of color entries in ColorMap if present (mode is Indexed)
    ColorMap: array of TRGB;
  end;
  PXcfImageInfo = ^TXcfImageInfo;

  // Channel / Layer Mask properties
  TXcfChannelProps = record
    Opacity: Cardinal;
    IsVisible: Boolean;
  end;

  // Layer Mask data
  TXcfLayerMask = record
    Width,
    Height: Integer;
    Name: WideString;
    Props: TXcfChannelProps;
    bpp: Integer;
    MaskDataOfs: Integer;
  end;

  // Parasite data
  TXcfParasite = record
    Name: WideString;
    Flags: Cardinal;
    Size: Cardinal;
    Data: array of byte;
  end;

  TXcfGraphic = class; // Forward declaration

  // A single layer inside an XCF image
  TXcfLayer = class(TgexLayer)
  private
    FOffset: Cardinal;
    FHierarchyOfs,             // Offset from start of file to hierarchy definition
    FLayerMaskOfs,             // Offset from start of file to layermask definition
    FLevelOfs: Cardinal;       // Offset from start of file to level data
  public
    LayerMode: TGimpLayerModeEffects;
    HasMask: Boolean;          // Does this layer have a mask
    BPP: Integer;              // BYTES per pixel (Integer to stop complaints about combining signed and unsigned)
    LayerType: Integer;        // Type of layer (RGB(A), GRAY(A), INDEXED(A))
    IsGroup: Boolean;          // Is this layer a group layer (2.8.0 and up)
    GroupItemFlags: Cardinal;  // Flags for layers belonging to a group
    ItemPathLength: Integer;
    ItemPathArray: array of Integer;
    IsParentGroupVisible: Boolean; // True if all groups this layer belongs to is True
    GroupParent: TXcfLayer;    // Group parent layer, nil if at top level
    IsActiveLayer: Boolean;    // Is this the active layer
    Mask: TXcfLayerMask;       // Layer Mask info
    destructor Destroy; override;
  end;

  // *.xcf images
  TXcfGraphic = class(TGraphicExGraphic)
  private
    FMemory: Pointer; // Start of memory when reading
    FLayersMem: Pointer; // Address inside stream to start of layer data (only valid when reading image)
    FLayers: TgexLayers; // List of layers containing layer info
  protected
    FXcfInfo: TXcfImageInfo;
    FLayerCount: Integer; // Number of layers in image
    FLastError,
    FLastWarning: string;

    function GetNextPropType(var Memory: PByte; var PropType, PropSize: Cardinal): Boolean;
    function GetParasite(var Memory: PByte; var Parasite: TXcfParasite): Boolean;
    function ReadChannelProps(var Memory: PByte; var Props: TXcfChannelProps): Boolean;
    function ReadLayer(var Memory: PByte; Layer: TXcfLayer;
      var ParentLayer: TXcfLayer; var GroupLevel: Integer): Boolean;
    function CombineLayers: Boolean;

    // WarningMessage: we encountered a non fatal error
    procedure WarningMessage( AMessage: string ); virtual;
    // FatalMessage: we encountered a fatal error
    procedure FatalMessage( AMessage: string ); virtual;

  public
    constructor Create; override;
    destructor Destroy; override;

    class function CanLoad(const Memory: Pointer; Size: Int64): Boolean; override;
    procedure LoadFromMemory(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal = 0); override;
    function ReadImageProperties(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal): Boolean; override;

    property LastError: string read FLastError;
    property LastWarning: string read FLastWarning;
  end;

  // Xcf compressed data decoders
  TXcfDecoder = class(TDecoder)
  private
    bpp: Integer;
  public
    constructor Create(BytesPerPixel: Cardinal);
    // Encode is an empty procedure since it's not implemented (yet).
    procedure Encode(Source, Dest: Pointer; Count: Cardinal; var BytesStored: Cardinal); override;
    // To stop warning about combining signed and unsigned we set BytesPerPixel to Integer
    property BytesPerPixel: Integer read bpp write bpp;
  end;

  TXcfRLEDecoder = class(TXcfDecoder)
  public
    // Currently only UnpackedSize is used/checked: it's the size in pixels of a tile (wxh)
    procedure Decode(var Source, Dest: Pointer; PackedSize, UnpackedSize: Integer); override;
  end;

  TXcfNoCompressionDecoder = class(TXcfDecoder)
  public
    // Currently only UnpackedSize is used/checked: it's the size in pixels of a tile (wxh)
    procedure Decode(var Source, Dest: Pointer; PackedSize, UnpackedSize: Integer); override;
  end;


implementation

uses Windows, gexTypes, GraphicStrings, gexUtils;

const gimp_base_id: AnsiString = 'gimp xcf ';
      gimp_id2: AnsiString     = 'file';
      gimp_known_max_version = 3; // Highest XCF file version known to us.

resourcestring
  ResXcfWarning_UnknownVersion = 'Unknown XCF file version %d!';

  ResXcfError_UnknownCompression = 'Unknown compression type %d!';
  ResXcfError_UnimplementedCompression = 'Unimplemented compression type!';
  ResXcfError_UnknownColorScheme = 'Unknown color scheme!';

type TXcfHeader = record
        case Integer of
          0: (gimp_header_long: array [0..13] of AnsiChar;);
          1: (gimp_header_short: array [0..8] of AnsiChar;
              gimp_v: AnsiChar;
              gimp_version: array [0..3] of AnsiChar;);
          2: (gimp_header_part1: array [0..8] of AnsiChar;
              gimp_header_part2: array [0..3] of AnsiChar;);
     end;
     PXcfHeader = ^TXcfHeader;
{  // follows header
   TXcfImage = packed record
     Width,
     Height: Integer;
     BaseType: Cardinal;         // Image basetype
   end;
   PXcfImage = ^TXcfImage;
}


//----------------- TXcfDecoder ------------------------------------------------

constructor TXcfDecoder.Create(BytesPerPixel: Cardinal);
begin
  bpp := BytesPerPixel;
  if bpp = 0 then begin
    // Make sure we don't cause buffer overflows because of incorrect input.
    bpp := 1;
    FDecoderStatus := dsInitializationError;
  end;
end;

procedure TXcfDecoder.Encode(Source, Dest: Pointer; Count: Cardinal; var BytesStored: Cardinal);
begin
  // Not implemented: todo add a warning!
end;


//----------------- TXcfRLEDecoder ---------------------------------------------

// Currently only UnpackedSize is used/checked: it's the size in pixels of a tile (wxh)
procedure TXcfRLEDecoder.Decode(var Source, Dest: Pointer; PackedSize, UnpackedSize: Integer);
var
  SourcePtr,
  DestCopy,
  TargetPtr: PByte;
  Val: Byte;
  RunLength: Integer;
  Size: Integer;
  i, j: Integer;
  OutputLeft: Integer;
begin
  FCompressedBytesAvailable := PackedSize;
  FDecompressedBytes := 0;
  if (PackedSize <= 0) or (UnpackedSize <= 0) then begin
    FCompressedBytesAvailable := 0;
    FDecoderStatus := dsInvalidBufferSize;
    Exit;
  end;
  FDecoderStatus := dsOK;
  SourcePtr := Source;
  DestCopy := Dest; // Don't change the original Dest
  for i := 0 to bpp-1 do begin
    // Data is compressed per plane
    TargetPtr := DestCopy; Inc(DestCopy);
    Size := UnpackedSize;
    while Size > 0 do begin
      if PackedSize < 1 then begin
        FDecoderStatus := dsNotEnoughInput;
        break;
      end;
      RunLength := ShortInt(SourcePtr^);
      Inc(SourcePtr);
      Dec(PackedSize);
      if RunLength < 0 then begin
        RunLength := abs(RunLength);
        if RunLength = 128 then begin
          // Next big endian word is RunLength (unsigned).
          if PackedSize < 2 then begin
            FDecoderStatus := dsNotEnoughInput;
            break;
          end;
          RunLength := SourcePtr^ shl 8; Inc(SourcePtr);
          RunLength := RunLength + SourcePtr^; Inc(SourcePtr);
          Dec(PackedSize, 2);
        end;
        if RunLength > PackedSize then begin
          FDecoderStatus := dsNotEnoughInput;
          RunLength := PackedSize;
        end;
        if RunLength > Size then begin
          FDecoderStatus := dsOutputBufferTooSmall;
          RunLength := Size;
        end;
        Dec(Size, RunLength);
        Dec(PackedSize, RunLength);
        // Move RunLength bytes from Source to Dest pixel plane
        while RunLength > 0 do begin
          TargetPtr^ := SourcePtr^;
          Inc(SourcePtr);
          Inc(TargetPtr, bpp); // Skip to next pixel
          Dec(RunLength);
        end;
        if FDecoderStatus <> dsOk then
          break;
      end
      else begin // >= 0
        Inc(RunLength);
        if RunLength = 128 then begin
          // Next big endian word is RunLength (unsigned).
          if PackedSize < 2 then begin
            FDecoderStatus := dsNotEnoughInput;
            break;
          end;
          RunLength := SourcePtr^ shl 8; Inc(SourcePtr);
          RunLength := RunLength + SourcePtr^; Inc(SourcePtr);
          Dec(PackedSize, 2);
        end;
        // Get Source byte
        if PackedSize < 1 then begin
          FDecoderStatus := dsNotEnoughInput;
          break;
        end;
        Val := SourcePtr^;
        Inc(SourcePtr);
        Dec(PackedSize);
        if RunLength > Size then begin
          FDecoderStatus := dsOutputBufferTooSmall;
          RunLength := Size;
        end;
        Dec(Size, RunLength);
        // Fill Source byte RunLength times in Dest pixel plane
        for j := 0 to RunLength-1 do begin
          TargetPtr^ := Val;
          Inc(TargetPtr, bpp); // Skip to next pixel
        end;
        if FDecoderStatus <> dsOk then
          break;
      end;
    end; // while
    Inc(FDecompressedBytes, UnpackedSize-Size);
    if FDecoderStatus <> dsOk then
      break;
  end; // for
  FCompressedBytesAvailable := PackedSize;
  if FDecoderStatus = dsOK then begin
    // Only check if status is ok. If it is not OK we already know something is wrong.
    if PackedSize <> 0 then begin
      if PackedSize < 0 then begin
        FDecoderStatus := dsInternalError;
        // This is a serious flaw: we got buffer overflow that we should have caught. We need to stop right now.
        CompressionError(Format(gesInputBufferOverflow, ['XCF RLE decoder']));
      end
      else begin // > 0
        // No error here since PackedSize may not be exact in Gimp
        // (for the last tile next offset is calculated/guessed)
      end;
    end;
    OutputLeft:= UnpackedSize * bpp - FDecompressedBytes;
    if OutputLeft <> 0 then begin
      if OutputLeft > 0 then begin
        // Broken/corrupt image
        FDecoderStatus := dsNotEnoughInput;
      end
      else begin // < 0
        FDecoderStatus := dsInternalError;
        // This is a serious flaw: we got buffer overflow that we should have caught. We need to stop right now.
        CompressionError(Format(gesOutputBufferOverflow, ['XCF RLE decoder']));
      end;
    end;
  end;
end;


//----------------- TXcfNoCompressionDecoder -----------------------------------

// Currently only UnpackedSize is used/checked: it's the size in pixels of a tile (wxh)
procedure TXcfNoCompressionDecoder.Decode(var Source, Dest: Pointer; PackedSize, UnpackedSize: Integer);
begin
  // WARNING: NOT TESTED! (I have no samples where there is no compression.)
  FCompressedBytesAvailable := PackedSize;
  FDecompressedBytes := 0;
  if (PackedSize <= 0) or (UnpackedSize <= 0) then begin
    FCompressedBytesAvailable := 0;
    FDecoderStatus := dsInvalidBufferSize;
    Exit;
  end;
  FDecoderStatus := dsOK;
  FDecompressedBytes := UnpackedSize * bpp;
  if FDecompressedBytes > PackedSize then begin
    FDecompressedBytes := PackedSize;
    FDecoderStatus := dsNotEnoughInput;
  end;
  Dec(FCompressedBytesAvailable, FDecompressedBytes);
  Move(Source^, Dest^, FDecompressedBytes);
end;


//------------------TXcfGraphic ------------------------------------------------

constructor TXcfGraphic.Create;
begin
  inherited Create;
  FLayers := nil;
end;

destructor TXcfGraphic.Destroy;
begin
  FLayers.Free;
  inherited Destroy;
end;

procedure TXcfGraphic.WarningMessage( AMessage: string );
begin
  FLastWarning := AMessage;
end;

procedure TXcfGraphic.FatalMessage( AMessage: string );
begin
  FLastError := AMessage;
  GraphicExError(AMessage);
end;

function TXcfGraphic.GetNextPropType(var Memory: PByte; var PropType, PropSize: Cardinal): Boolean;
begin
  // TODO Check that we dont read beyond filesize
  PropType := ReadBigEndianCardinal(Memory);
  PropSize := ReadBigEndianCardinal(Memory);
  Result := True;
end;

class function TXcfGraphic.CanLoad(const Memory: Pointer; Size: Int64): Boolean;
var
  Run: PByte;
begin
  Result := Size > 14 + (7*4); // Minimum size according to xcftools
  if Result then begin
    Run := Memory;
    if not CompareMem(@PXcfHeader(Run)^.gimp_header_short, @gimp_base_id[1], Length(gimp_base_id)) then
      Result := False
    else if CompareMem(@PXcfHeader(Run)^.gimp_header_part2, @gimp_id2[1], Length(gimp_id2)) then
      Result := True // version 0
    else if PXcfHeader(Run)^.gimp_v = 'v' then
      Result := True
    else
      Result := False;
  end;
end;

function TXcfGraphic.ReadChannelProps(var Memory: PByte; var Props: TXcfChannelProps): Boolean;
var
  PropType, PropSize: Cardinal;
begin
  Result := False;
  // Loop over all layer properties
  while True do begin
    if not GetNextPropType(Memory, PropType, PropSize) then begin
      // Message: error reading data
      Exit;
    end;
    case PropType of
      PROP_END:
        begin
          Result := True;
          Break;
        end;
      PROP_OPACITY:
        Props.Opacity := ReadBigEndianInteger(Memory);
      PROP_VISIBLE:
        Props.IsVisible := ReadBigEndianInteger(Memory) <> 0;
      {...more that we are ignoring for now and don't need for loading...}
    else
      Inc(Memory, PropSize);
    end; // case PropType
  end; // while True
end;

function TXcfGraphic.ReadLayer(var Memory: PByte; Layer: TXcfLayer;
      var ParentLayer: TXcfLayer; var GroupLevel: Integer): Boolean;
var
  PropType, PropSize: Cardinal;
  Temp, Junk: Cardinal;
  TempMem: PByte;
  hWidth, hHeight: Integer;
  i: Integer;
  CurLevel: Integer;
  MaskHierarchyOfs: Cardinal;
begin
  Result := False;
  // Initialize Layer Info
  Layer.LayerMode := GIMP_NORMAL_MODE;
  Layer.Opacity := 255;
  Layer.IsVisible := True;
  Layer.HasMask := False;
  Layer.IsGroup := False;
  Layer.ItemPathLength := 0;
  Layer.IsParentGroupVisible := True;
  Layer.GroupParent := nil;
  Layer.IsActiveLayer := False;
  Layer.Width := ReadBigEndianCardinal(Memory);
  Layer.Height := ReadBigEndianCardinal(Memory);
  Layer.LayerType := ReadBigEndianCardinal(Memory);
  Layer.Name := ReadUtf8StringBigEndianLength(Memory);

  // Loop over all layer properties
  while True do begin
    if not GetNextPropType(Memory, PropType, PropSize) then begin
      // Message: error reading data
      Exit;
    end;
    case PropType of
      PROP_END:
        begin
          Result := True;
          Break;
        end;
      PROP_ACTIVE_LAYER:
        Layer.IsActiveLayer := True;
      PROP_OPACITY:
        begin
          Temp := ReadBigEndianCardinal(Memory);
          if Temp > 255 then
            Layer.Opacity := 255
          else
            Layer.Opacity := Temp;
        end;
      PROP_VISIBLE:
        begin
          Temp := ReadBigEndianCardinal(Memory);
          Layer.IsVisible := Temp <> 0;
        end;
      PROP_APPLY_MASK:
        begin
          Temp := ReadBigEndianCardinal(Memory);
          Layer.HasMask := Temp <> 0;
        end;
      PROP_OFFSETS:
        begin
          Layer.OffsetX := ReadBigEndianInteger(Memory);
          Layer.OffsetY := ReadBigEndianInteger(Memory);
        end;
      PROP_MODE:
        begin
          Temp := ReadBigEndianCardinal(Memory);
          Layer.LayerMode := TGimpLayerModeEffects(Temp);
        end;
      PROP_GROUP_ITEM:
        begin
          Layer.IsGroup := True;
          Layer.GroupParent := ParentLayer;
          ParentLayer := Layer;
          Inc(GroupLevel);
        end;
      PROP_GROUP_ITEM_FLAGS:
        Layer.GroupItemFlags := ReadBigEndianCardinal(Memory);
      PROP_ITEM_PATH:
        begin
          Layer.ItemPathLength := PropSize div 4;
          SetLength(Layer.ItemPathArray, Layer.ItemPathLength);
          for i := 0 to Layer.ItemPathLength-1 do begin
            // i = 0: index within top layer stack of first level group
            // i = 1: index within first level group of second level group
            // i = n: index within nth level group of this layer
            Layer.ItemPathArray[i] := ReadBigEndianCardinal(Memory);
          end;
        end;
    else
      Inc(Memory, PropSize);
    end; // case PropType
  end; // while True

  // Check if we're still on the same level, if not we have to change
  // Parent group and level
  // Can't do that in PROP_ITEM_PATH because that prop is not there for level 0 (root level)
  // Note that we could be changing more than 1 level!!!
  if not Layer.IsGroup then begin
    if Layer.ItemPathLength = 0 then
      CurLevel := 0
    else
      CurLevel := Layer.ItemPathLength-1;
    while CurLevel < GroupLevel do begin
      Dec(GroupLevel);
      ParentLayer := ParentLayer.GroupParent;
    end;
    Layer.GroupParent := ParentLayer;
  end;

  // Set parent group visibility
  if Layer.GroupParent <> nil then
    Layer.IsParentGroupVisible := Layer.IsVisible and
      Layer.GroupParent.IsParentGroupVisible
  else if Layer.IsGroup then // Level 1 group with no GroupParent
    Layer.IsParentGroupVisible := Layer.IsVisible;

  // TODO: gimp loader has some text layer hacking code here, ignored for now

  // Read hierarchy and layer mask offsets
  Layer.FHierarchyOfs := ReadBigEndianCardinal(Memory);
  Layer.FLayerMaskOfs := ReadBigEndianCardinal(Memory);

  if Layer.FHierarchyOfs = 0 then begin
    Result := False;
    WarningMessage('Layer Hierarchy offset should not be 0!');
    Exit;
  end;
  if Layer.FLayerMaskOfs = 0 then
    Layer.HasMask := False;

  // Gimp reads hierarchy here
  TempMem := TXcfGraphic(Layer.Graphic).FMemory;
  Inc(TempMem, Layer.FHierarchyOfs);
  hWidth := ReadBigEndianCardinal(TempMem);
  hHeight := ReadBigEndianCardinal(TempMem);

  // Security check: width and height of hierarchy should be same as defined for layer
  if (Layer.Width <> hWidth) or (Layer.Height <> hHeight) then begin
    Result := False;
    WarningMessage('Layer dimensions not the same as hierarchy dimensions!');
    Exit;
  end;

  Layer.BPP := ReadBigEndianCardinal(TempMem);       // Bytes per pixel
  Layer.FLevelOfs := ReadBigEndianCardinal(TempMem); // Offset to level data

  // Discard offsets for layers below first, if any.
  repeat
    Junk := ReadBigEndianCardinal(TempMem);
  until Junk = 0;

  // Read layer mask if present
  if Layer.HasMask then begin
    TempMem := TXcfGraphic(Layer.Graphic).FMemory;
    Inc(TempMem, Layer.FLayerMaskOfs);
    Layer.Mask.Width := ReadBigEndianCardinal(TempMem);
    Layer.Mask.Height := ReadBigEndianCardinal(TempMem);
    Layer.Mask.Name := ReadUtf8StringBigEndianLength(TempMem);
    if (Layer.Mask.Width <> Layer.Width) or (Layer.Mask.Height <> Layer.Height) then
      WarningMessage('Mask Width or Height not the same as Layer!');
    if ReadChannelProps(TempMem, Layer.Mask.Props) then begin
      // Read hierarchy offset
      MaskHierarchyOfs := ReadBigEndianCardinal(TempMem);
      // Point to Mask hierarchy data
      TempMem := TXcfGraphic(Layer.Graphic).FMemory;
      Inc(TempMem, MaskHierarchyOfs);
      // Read mask hierarchy
      hWidth := ReadBigEndianCardinal(TempMem);
      hHeight := ReadBigEndianCardinal(TempMem);
      if (Layer.Mask.Width <> hWidth) or (Layer.Mask.Height <> hHeight) then
        WarningMessage('Mask Width or Height not the same in Hierarchy!');
      Layer.Mask.bpp := ReadBigEndianCardinal(TempMem);
      Layer.Mask.MaskDataOfs := ReadBigEndianCardinal(TempMem);
      if Layer.Mask.MaskDataOfs = 0 then begin
        WarningMessage('Mask data not present, changing HasMask to false!');
        Layer.HasMask := False;
      end;
    end;
  end;
end;

function TXcfGraphic.CombineLayers: Boolean;
var iTile, iLayer: Integer;
  Layer: TXcfLayer;
  lWidth, lHeight: Integer;
  TileOffset,
  TileOffset2,
  TileOffsetNext: Cardinal;
  MaskTileOffset,
  MaskTileOffset2,
  MaskTileOffsetNext: Cardinal;
  Memory: PByte;
  TileMem: PByte;
  PixelMem, PixelMemCopy, PixelMemDest : PByte;
  MaskMem, MaskTileMem: PByte;
  MaskTileBuffer, MaskTileBufCopy: PByte;
  CombinedLayersMem, CombinedLayersRowMem: PByte;
  BufferSize: Cardinal;
  RowTiles,
  ColTiles,
  TileCount: Integer; // Integer to stop complaints about combining signed and unsigned
  RightTileWidth,
  BottomTileHeight,
  LastRowStartTile: Integer; // Integer to stop complaints about combining signed and unsigned
  TileTop,            // Top and Left need to be integer to make it easier
  TileLeft: Integer;  // to add the Integer offsets.
  TileWidth,
  TileHeight: Integer; // Width and Height need to be Integer too: they can become negative
                       // after an intermediate calculation
  TileBottom,
  TileSize: Cardinal;  // Size in pixels of a tile (w x h)
  SourceRowSize: Integer;
  RowLeftOffset: Integer; // Integer to stop complaints about combining signed and unsigned
  Y: Cardinal;
  SourceXOffset,
  SourceYOffset: Integer;
  DestYOffset: Integer;
  DestRowSize: Integer;
  iSrcPixel: Integer;
  TempRGBA: packed record
    case Integer of
      0: (RGB: TRGB;
          Alpha: Byte);
      1: (RGBA: TRGBA);
      2: (Value: Cardinal);
  end;
  TempGray: packed record
    Gray: Byte;
    Alpha: Byte;
  end;
  LayerBlendProc: TgexBlendFunction;
  FirstVisibleLayer: Boolean;
begin
  Result := False;
  iLayer := FLayers.Count;
  MaskTileBuffer := nil;
  FirstVisibleLayer := True;
  // Get buffer to store the pixels for 1 complete tile
  // Assume maximum size: layer type same as image type
  GetMem(PixelMem, TILE_WIDTH*TILE_HEIGHT*FImageProperties.SamplesPerPixel);
  // Get buffer for pixels of combined layers = image
  BufferSize := Width * Height *FImageProperties.SamplesPerPixel;
  GetMem(CombinedLayersMem, BufferSize);
  // Init buffer with black/alpha 0
  FillChar(CombinedLayersMem^, BufferSize, 0);

  // Start progress section, guessing this part to be 80% (loading and combining layers)
  StartProgressSection(70, gesLoadingData);
  try
    // Compute DestRowSize: Size in bytes of 1 row of the image
    DestRowSize := Width * FImageProperties.SamplesPerPixel;
    repeat
      AdvanceProgress( 100 / FLayers.Count-iLayer, 0, 1, True);

      Dec(iLayer);
      Layer := TXcfLayer(FLayers.Items[iLayer]);
      if Layer.IsVisible and Layer.IsParentGroupVisible and not Layer.IsGroup then begin

        if FirstVisibleLayer then begin
          // Gimp requires that the bottommost visible layer has either mode normal or dissolve
          if Layer.LayerMode <> GIMP_DISSOLVE_MODE then
            Layer.LayerMode := GIMP_NORMAL_MODE;
          FirstVisibleLayer := False;
        end;

        // Get the layer blending function
        LayerBlendProc := SelectBlendFunction( TGimpImageType(Layer.LayerType),
          TGimpLayerModeEffects(Layer.LayerMode));
        if @LayerBlendProc = nil then
          FatalMessage('Fatal: No layer blending function assigned!');

        // Load starting offset of this level
        Memory := TXcfGraphic(Layer.Graphic).FMemory;
        Inc(Memory, Layer.FLevelOfs);

        // If this layer has a mask get the offset for that
        if Layer.HasMask then begin
          // If the Mask pixel buffer hasn't been created yet then create it.
          // Since a lot of images don't have layer masks we only create it when needed.
          if not Assigned(MaskTileBuffer) then
            GetMem(MaskTileBuffer, TILE_WIDTH*TILE_HEIGHT*1); // Channels always 1 bpp (?)
          MaskMem := TXcfGraphic(Layer.Graphic).FMemory;
          Inc(MaskMem, Layer.Mask.MaskDataOfs);
          // Read offset of first mask tile, but first skip width and height which we already know
          Inc(MaskMem, 8);
          MaskTileOffset := ReadBigEndianCardinal(MaskMem);
          MaskTileOffsetNext := 0;
        end
        else begin
          MaskMem := nil;
          MaskTileOffset := 0;
          MaskTileOffsetNext := 0;
        end;

        // Read level data: width, height and offset of first tile
        lWidth := ReadBigEndianCardinal(Memory);
        lHeight := ReadBigEndianCardinal(Memory);
        TileOffset := ReadBigEndianCardinal(Memory);
        if TileOffset = 0 then begin
          Continue; // Empty level
        end;

        // Compute number of tiles for this layer
        RowTiles := ((lWidth+TILE_WIDTH-1) div TILE_WIDTH);
        ColTiles := ((lHeight+TILE_HEIGHT-1) div TILE_HEIGHT);
        TileCount :=  RowTiles * ColTiles;

        // Compute right most tile width and bottom most tile height
        RightTileWidth := lWidth mod TILE_WIDTH;
        if RightTileWidth = 0 then
          RightTileWidth := TILE_WIDTH;
        BottomTileHeight := LHeight mod TILE_HEIGHT;
        if BottomTileHeight = 0 then
          BottomTileHeight := TILE_HEIGHT;

        // Compute start of bottom most row of tiles
        LastRowStartTile := TileCount - RowTiles;

        // Loop over all tiles, decode data and convert to target pixel format
        for iTile := 0 to TileCount-1 do begin
          if TileOffset = 0 then begin
            // Error: not enough tiles!
            TileOffset := 1;
            Break;
          end;

          // Read in the offset of the next tile so we can calculate the amount
          // of data needed for this tile.
          TileOffsetNext := ReadBigEndianCardinal(Memory);
          // If the offset is 0 then we need to read in the maximum possible
          // allowing for negative compression.
          if TileOffsetNext = 0 then
            // Gimp has 4 * 1.5 in the formula below. However Delphi thinks we need float conversion
            // which would slow things down, therefore we use 6, which is what 4 * 1.5 equates to.
            TileOffset2 := TileOffset + TILE_WIDTH * TILE_WIDTH * 6
          else
            TileOffset2 := TileOffsetNext;

          // Set Tile Memory address;
          TileMem := TXcfGraphic(Layer.Graphic).FMemory;
          Inc(TileMem, TileOffset);

          // Set Mask tile memory if mask present
          if Layer.HasMask and (MaskTileBuffer <> nil) then begin
            MaskTileMem := TXcfGraphic(Layer.Graphic).FMemory;
            Inc(MaskTileMem, MaskTileOffset);
            // Compute offset to next tile, and expected size of compressed tile
            MaskTileOffsetNext := ReadBigEndianCardinal(MaskMem);
            // If the offset is 0 then we need to read in the maximum possible
            // allowing for negative compression.
            if MaskTileOffsetNext = 0 then
              MaskTileOffset2 := MaskTileOffset + TILE_WIDTH * TILE_WIDTH * 6
            else
              MaskTileOffset2 := MaskTileOffsetNext;
          end;

          // Compute width of this tile
          if (iTile + 1) mod RowTiles = 0 then
            // Rightmost tile
            TileWidth := RightTileWidth
          else
            TileWidth := TILE_WIDTH;

          // Compute height of this tile
          if iTile >= LastRowStartTile then
            // Bottom tile
            TileHeight := BottomTileHeight
          else
            TileHeight := TILE_HEIGHT;

          // Compute position of tile inside image
          TileTop  := (iTile div RowTiles) * TILE_HEIGHT;
          TileLeft := (iTile mod RowTiles) * TILE_WIDTH;

          // Adjust tile coordinates to image coordinates
          Inc(TileLeft, Layer.OffsetX);
          Inc(TileTop,  Layer.OffsetY);

          // Don't need to do any decoding or conversion when location of
          // current tile is completely outside the image.
          if (TileLeft >= FImageProperties.Width) or   // right of image
             (TileLeft + TileWidth <= 0) or     // left of image
             (TileTop + TileHeight <= 0) or     // above image
             (TileTop >= FImageProperties.Height) then // below image
          begin
            // Set correct offset first which we otherwise do at the bottom of the loop
            TileOffset := TileOffsetNext;
            MaskTileOffset := MaskTileOffsetNext;
            // There may be several rows and/or columns of tiles that are completely
            // outside the image. However we can't skipt multiple tiles since we
            // are reading the offset to the tiles tiledata inside te loop.
            Continue;
          end;

          // Compute TileSize: size in pixels of decoded tile
          TileSize := TileWidth * TileHeight;

          // Make sure the Decoder uses the correct BPP since a layer might have
          // a different BPP than the image
          // Since decoding the mask can change it, we have to set it inside the loop.
          TXcfDecoder(Decoder).BytesPerPixel := Layer.BPP;

          // Decode the pixel data (since Decode Source is a var it can change, thus we use a copy)
          PixelMemCopy := PixelMem;
          Decoder.Decode(Pointer(TileMem), Pointer(PixelMemCopy),
            TileOffset2-TileOffset, TileSize);

          // Compute Tile PixelMem address Delta per row (Source)
          SourceRowSize := TileWidth * Layer.BPP;

          if Layer.BPP+1 = FImageProperties.SamplesPerPixel then begin
            // Layer without alpha but Image with alpha: We need to move the
            // source pixel data to make room for alpha = 255
            // Compute offset to last pixel in Source first
            PixelMemCopy := PixelMem;
            Inc(PixelMemCopy, TileHeight*SourceRowSize-Layer.BPP);
            // Set new SourceRowSize including alpha:
            SourceRowSize := TileWidth * FImageProperties.SamplesPerPixel;
            // Compute Destination offset of last pixel
            PixelMemDest := PixelMem;
            Inc(PixelMemDest, TileHeight*SourceRowSize-FImageProperties.SamplesPerPixel);
            if FImageProperties.SamplesPerPixel = 4 then begin
              // RGBA
              TempRGBA.Alpha := $FF; // Set Alpha to 255
              // Loop over all pixels starting from the end
              for iSrcPixel := TileHeight * TileWidth-1 downto 0 do begin
                TempRGBA.RGB := PRGB(PixelMemCopy)^;   // Get RGB
                PRGBA(PixelMemDest)^ := TempRGBA.RGBA; // Set RGBA
                Dec(PixelMemCopy, 3);
                Dec(PixelMemDest, 4);
              end;
            end
            else if FImageProperties.SamplesPerPixel = 2 then begin
              // GRAYA, INDEXEDA
              TempGray.Alpha := $FF;  // Set Alpha to 255
              for iSrcPixel := TileHeight * TileWidth-1 downto 0 do begin
                TempGray.Gray := PixelMemCopy^;         // Get Gray/Indexed
                PWord(PixelMemDest)^ := Word(TempGray); // Set GrayA/IndexedA
                Dec(PixelMemCopy, 1);
                Dec(PixelMemDest, 2);
              end;
            end
            else
              FatalMessage('XCF: Unexpected value for SamplesPerPixel!');
          end;

          // If there's a mask then Decode it and combine mask with tile pixel data.
          // We need to do this after we have possibly added an alpha channel to this layer
          // since ApplyAlphaMask expects a source with alpha
          if Layer.HasMask and (MaskTileBuffer <> nil) then begin
            MaskTileBufCopy := MaskTileBuffer;
            TXcfDecoder(Decoder).BytesPerPixel := Layer.Mask.bpp;
            // Decode Mask tile
            Decoder.Decode(Pointer(MaskTileMem), Pointer(MaskTileBufCopy),
              MaskTileOffset2-MaskTileOffset, TileSize);
            // Combine with tile data of layer
            ApplyAlphaMask(PixelMem, MaskTileBuffer, TileSize, FImageProperties.SamplesPerPixel);
          end;

          // Decrease usable TileWidth with the part to the left of the image
          if TileLeft < 0 then begin
            SourceXOffset := Abs(TileLeft); // X offset into decoded data
            Dec(TileWidth, SourceXOffset);
            TileLeft := 0;
          end
          else
            SourceXOffset := 0;
          // Same for TileHeight with the part above the image
          if TileTop < 0 then begin
            SourceYOffset := Abs(TileTop); // Y offset into decode data
            Dec(TileHeight, SourceYOffset);
            TileTop := 0;
          end
          else
            SourceYOffset := 0;

          // Compute offset from start of Scanline in bytes (Dest)
          // Don't use Layer.BPP here since it could be different than SampelsPerPixel
          // and if needed we have already adjusted source pixel data to include alpha
          RowLeftOffset := TileLeft * FImageProperties.SamplesPerPixel;

          // Adjust width if right side falls outside image
          if TileLeft+TileWidth >= FImageProperties.Width then
            TileWidth := FImageProperties.Width - TileLeft;

          // Check if tile ends below image
          if TileTop + TileHeight >= FImageProperties.Height then
            // Adjust Height
            TileHeight := FImageProperties.Height - TileTop;

          // Compute bottom of layer
          TileBottom := TileTop + TileHeight;

          PixelMemCopy := PixelMem;
          // Compute offset into Source pixel data ignoring the parts that
          // fall outside the image  (don't use Layer.BPP, see above)
          Inc(PixelMemCopy, (SourceYOffset*SourceRowSize) + (SourceXOffset*FImageProperties.SamplesPerPixel));

          CombinedLayersRowMem := CombinedLayersMem;
          // Compute Destination Start Offset for Row TileTop
          DestYOffset := TileTop * DestRowSize + RowLeftOffset;
          Inc(CombinedLayersRowMem, DestYOffset);

          // Loop over all pixel rows in this tile as far as they fall inside the image
          // A possible optimization could be to do the whole tile in one go instead
          // of line by line. However that can only be done for tiles that are
          // completely visible inside the image (which should be most tiles).
          // Partially visible tiles will always need to be handled line by line.
          for Y := TileTop to TileBottom - 1 do begin
            LayerBlendProc(PixelMemCopy, CombinedLayersRowMem, Layer.Opacity, TileWidth);
            // Update the source and dest buffer pointers
            Inc(PixelMemCopy, SourceRowSize);
            Inc(CombinedLayersRowMem, DestRowSize);
          end; // for all pixel rows in layer

          // TODO: Progress, but not inside the loop
          // TODO: Progress(Self, psRunning, MulDiv(Y, 100, LayerHeight), True, FProgressRect, '');
          // TODO: OffsetRect(FProgressRect, 0, 1);

          TileOffset := TileOffsetNext;
          MaskTileOffset :=  MaskTileOffsetNext;
        end; // for all tiles in layer
        if TileOffset <> 0 then
          Break;
      end;
    until iLayer = 0;

    FinishProgressSection(False);

    // We need to create palettes for grayscale and indexed, however since
    // RGB(A) is by far the most common we skip that first
    // But take into account that source color schemes csG(A) and csINDEXED(A)
    // wil be converted to csBGRA
    if not (ColorManager.SourceColorScheme in [csRGB, csRGBA]) then
      if ColorManager.SourceColorScheme in [csG, csGA] then
        // Gray scale image data.
        Palette := ColorManager.CreateGrayscalePalette(False)
      else if ColorManager.SourceColorScheme in [csIndexed, csIndexedA] then begin
        // Indexed image
        if FXcfInfo.ColorMapColorCount > 0 then begin
          // Color palette data present
          Palette := ColorManager.CreateColorPalette([FXcfInfo.ColorMap],
            pfInterlaced8Triple, FXcfInfo.ColorMapColorCount, False);
          ColorManager.SetSourcePalette([FXcfInfo.ColorMap], pfInterlaced8Triple);
        end
        else begin
          // No colormap present? Must be an error.
          WarningMessage('XCF: No colormap found in indexed image, substituted grayscale palette!');
          Palette := ColorManager.CreateGrayscalePalette(False)
        end;
      end;

    // And finally convert all the color data in one go
    // But first compute offsets outside the loop
    CombinedLayersRowMem := CombinedLayersMem; // We need to save our original ptr
    SourceRowSize := Width * FImageProperties.SamplesPerPixel;

    // Progress: Transfer data to bitmap, guessing 20%.
    StartProgressSection(20, gesTransfering);

    // TODO: We can further improve speed by computing ScanLine RowDelta when height is > 1
    // and then only ask for ScanLine ptr once.
    for Y := 0 to Height-1 do begin
      ColorManager.ConvertRow([CombinedLayersRowMem], PAnsiChar(ScanLine[Y]),
        Width, $FF);
      Inc(CombinedLayersRowMem, SourceRowSize);

      // Update progress
      AdvanceProgress(100 / Height, 0, 1, True);
    end;
    Result := True;
  finally
    FreeMem(CombinedLayersMem);
    FreeMem(PixelMem);
    if Assigned(MaskTileBuffer) then
      FreeMem(MaskTileBuffer);

    FinishProgressSection(False);
  end;
end;

procedure TXcfGraphic.LoadFromMemory(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal = 0);
var i: Integer;
  Run: PByte;
  LayerOfs: Cardinal;
  LayerMem: PByte;
  XcfLayer,
  GroupLayer: TXcfLayer;
  GroupLevel: Integer;
begin
  if ReadImageProperties(Memory, Size, ImageIndex) and (FLayerCount > 0) then begin
    // Initialize outermost progress display.
    InitProgress(Width, 1);
    StartProgressSection(0, '');

    // Initialize sub section for image preparation. We give it a (guessed) value of 1%.
    StartProgressSection(1, gesPreparing);

    FMemory := Memory;
    Run := FLayersMem;
    GroupLayer := nil;
    GroupLevel := 0;

    // FLayers might be assigned by a previous load
    if Assigned(FLayers) then
      FLayers.Free;
    FLayers := TgexLayers.Create(Self, TXcfLayer);
    try
      // Read basic info of all layers
      for i := 0 to FLayerCount-1 do begin
        // Get Offset to next layer
        LayerOfs := ReadBigEndianCardinal(Run);
        XcfLayer := TXcfLayer(FLayers.AddNewLayer);
        XcfLayer.FOffset := LayerOfs;
        LayerMem := FMemory; Inc(LayerMem, LayerOfs);
        if ReadLayer(LayerMem, XcfLayer, GroupLayer, GroupLevel) then begin
        end;
      end; // for all layers

      // After the layers follow the channels which we don't need thus ignore.

      // Set image properties only once
      XcfLayer := TXcfLayer(FLayers.First);

      // Note that different layers can have or not have alpha!
      // Thus we can't use just 1 layer to determine bpp.
      // For RGB image we will always assume 32 bits = 4 bpp, thus including alpha
      case FXcfInfo.BaseType of
        Ord(GIMP_RGB):     FImageProperties.SamplesPerPixel := 4;
        Ord(GIMP_GRAY):    FImageProperties.SamplesPerPixel := 2;
        Ord(GIMP_INDEXED): FImageProperties.SamplesPerPixel := 2;
      else
        FImageProperties.SamplesPerPixel := 4;
      end;
      FImageProperties.BitsPerPixel := FImageProperties.SamplesPerPixel * FImageProperties.BitsPerSample;

      // Set up Color Manager
      ColorManager.SourceOptions := [];
      ColorManager.SourceBitsPerSample := 8;
      // Note: layers without alpha will already be converted to with alpha before
      // the ColorManager gets at it
      ColorManager.SourceSamplesPerPixel := FImageProperties.SamplesPerPixel;
      ColorManager.SourceColorScheme := FImageProperties.ColorScheme;

      // Set pixel format before size to avoid possibly large conversion operation.
      ColorManager.SelectTarget;
      Self.PixelFormat := ColorManager.TargetPixelFormat;
      // Image dimensions
      Self.Width := FImageProperties.Width;
      Self.Height := FImageProperties.Height;

      // Get decoder for the pixel data
      case FImageProperties.Compression of
        ctNone: // WARNING: UNTESTED!!!
          Decoder := TXcfNoCompressionDecoder.Create(FImageProperties.SamplesPerPixel);
        ctRLE:
          Decoder := TXcfRLEDecoder.Create(FImageProperties.SamplesPerPixel);
      else
        // Error: unimplemented compression
        WarningMessage(ResXcfError_UnimplementedCompression);
        Exit;
      end;

      // The preparation part is finished. Finish also progress section (which will step the main progress).
      FinishProgressSection(False);

      // And now read in and combine all visible layers and convert color data
      try
        CombineLayers;
      except
        on EOutOfMemory do begin
          WarningMessage('XCF: Out of memory during loading image.');
        end;
      end;
    finally
      // To be able to access layer info after loading is done we keep FLayers
      // and don't free it here but in Destroy.
      // FreeAndNil(FLayers);
      // Decoder may bee needed again thus nil it.
      FreeAndNil(Decoder);

      FinishProgressSection(False);
    end;
  end
  else if FLastErrorReason <> '' then
    FatalMessage(Format(gesInvalidImageEx, ['XCF', FLastErrorReason]))
  else
    FatalMessage(Format(gesInvalidImage, ['XCF']));
end;

function TXcfGraphic.GetParasite(var Memory: PByte; var Parasite: TXcfParasite): Boolean;
begin
  Parasite.Name := ReadUtf8StringBigEndianLength(Memory);
  Parasite.Flags := ReadBigEndianCardinal(Memory);
  Parasite.Size := ReadBigEndianCardinal(Memory);
  SetLength(Parasite.Data, Parasite.Size);
  Move(Memory^, Parasite.Data[0], Parasite.Size);
  Inc(Memory, Parasite.Size);
  Result := True;
end;

function TXcfGraphic.ReadImageProperties(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal): Boolean;
var
  Run: PByte;
  i, n_colors: Integer;
  ByteVal: Byte;
  IntVal: Integer;
  PropType,
  Propsize: Cardinal;
  EndOfParasites: NativeUInt;
  Parasite: TXcfParasite;
begin
  FLastError := '';
  FLastWarning := '';
  // Initialize XCF image info
  FillChar(FXcfInfo,SizeOf(TXcfImageInfo),0);
  FLayersMem := nil;
  FLayerCount := 0;
  Result := inherited ReadImageProperties(Memory, Size, ImageIndex);
  if Result and (Size > 14 + (7*4)) then begin
    Run := Memory;
    if not CompareMem(@PXcfHeader(Run)^.gimp_header_short, @gimp_base_id[1], Length(gimp_base_id)) then begin
      Result := False;
      Exit;
    end
    else if CompareMem(@PXcfHeader(Run)^.gimp_header_part2, @gimp_id2[1], Length(gimp_id2)) then
      FImageProperties.Version := 0
    else if (PXcfHeader(Run)^.gimp_v = 'v') and (PXcfHeader(Run)^.gimp_version[3] = #0) then
      FImageProperties.Version := StrToIntDef( string(PXcfHeader(Run)^.gimp_version), -1)
    else begin
      Result := False;
      Exit;
    end;
    Inc(Run, 14);

    if FImageProperties.Version > gimp_known_max_version then
      // Warning unknown XCF file version (but try to continue)
      WarningMessage(Format(ResXcfWarning_UnknownVersion,[FImageProperties.Version]));

    FImageProperties.Width := ReadBigEndianCardinal(Run);
    FImageProperties.Height := ReadBigEndianCardinal(Run);
    FXcfInfo.BaseType := ReadBigEndianCardinal(Run);

    // Color scheme.
    case FXcfInfo.BaseType of       // Assume by default always alpha
      Ord(GIMP_RGB):
        begin
          FImageProperties.ColorScheme := csRGBA;
          FImageProperties.SamplesPerPixel := 4;
        end;
      Ord(GIMP_GRAY):
        begin
          FImageProperties.ColorScheme := csGA;
          FImageProperties.SamplesPerPixel := 2;
        end;
      Ord(GIMP_INDEXED):
        begin
          FImageProperties.ColorScheme := csIndexedA;
          FImageProperties.SamplesPerPixel := 2;
        end;
    else
      WarningMessage('XCF: Unknown color format!');
      FImageProperties.ColorScheme := csRGBA;
      FImageProperties.SamplesPerPixel := 4;
    end;
    FImageProperties.BitsPerSample := 8;

    // Loop over all properties
    Result := False;
    while True do begin
      if not GetNextPropType(Run, PropType, PropSize) then begin
        WarningMessage('Error reading XCF file.');
        Exit;
      end;
      case PropType of
        PROP_END:
          begin
            Result := True;
            break;
          end;
        PROP_COLORMAP:
          begin
            n_colors := ReadBigEndianCardinal(Run);
            FXcfInfo.ColorMapColorCount := n_colors;
            if n_colors > GIMP_IMAGE_COLORMAP_SIZE div 3 then begin
              WarningMessage('XCF: Illegal colormap size!');
              Exit;
            end;
            // Set the length of the ColorMap
            SetLength(FXcfInfo.ColorMap, n_colors);
            if FImageProperties.Version = 0 then begin
              // Version 0 had buggy ColorMap. Don't use it but give it a grayscale colormap
              for i := 0 to n_colors-1 do begin
                FXcfInfo.ColorMap[i].R := i;
                FXcfInfo.ColorMap[i].G := i;
                FXcfInfo.ColorMap[i].B := i;
              end;
              Inc(Run, n_colors);
            end
            else begin
              Move(Run^, FXcfInfo.ColorMap[0], n_colors*3);
              Inc(Run, 3*n_colors);
            end;
          end;
        PROP_COMPRESSION:
          begin
            ByteVal := Run^; Inc(Run);
            FXcfInfo.Compression := ByteVal;
            case ByteVal of
              COMPRESS_NONE: FImageProperties.Compression := ctNone;
              COMPRESS_RLE:  FImageProperties.Compression := ctRLE;
              // The next 2 compression types are not used according to gimp.
              COMPRESS_ZLIB: FImageProperties.Compression := ctPlainZip; // Not sure if this is correct!
              COMPRESS_FRACTAL: FImageProperties.Compression := ctUnknown; // TODO: Add Fractal compression to GraphicEx
            else
              // TODO fatal error: unknown compression
              FatalMessage(Format(ResXcfError_UnknownCompression,[Integer(ByteVal)]));
            end;
          end;
        PROP_RESOLUTION:
          begin
            FImageProperties.XResolution := ReadBigEndianSingle(Run);
            FImageProperties.YResolution := ReadBigEndianSingle(Run);
          end;
        PROP_PARASITES:
          begin
            // We are currently only interested in comments.
            // Gimp can also store exif data from files converted to xcf.
            EndOfParasites := NativeUInt(Run)+PropSize;
            while NativeUInt(Run) < EndOfParasites do begin
              if GetParasite(Run, Parasite) and (CompareText('gimp-comment'+#0, Parasite.Name) = 0) then begin
                // This is a Gimp comment block
                if (Parasite.Size > 0) and (AnsiChar(Parasite.Data[Parasite.Size-1]) = #0) then
                  Dec(Parasite.Size);
                SetString(FImageProperties.Comment, PAnsiChar(Parasite.Data), Parasite.Size);
              end;
            end;
          end;
      else // case PropType
        Inc(Run, PropSize);
      end;
    end; // while True

    FLayersMem := Run;
    // Loop over layers
    while True do begin
      IntVal := ReadBigEndianCardinal(Run);
      if IntVal = 0 then
        Break;
      Inc(FLayerCount);
    end;

    if Result then
      Result := CheckBasicImageProperties();
  end
  else // Since if can also fail because of size check we need to set result here.
    Result := False;

end;

//----------------- End of TXcfGraphic -----------------------------------------

//----------------- TXcfLayer --------------------------------------------------

// Can be removed since we currently don't need to Destroy anything
destructor TXcfLayer.Destroy;
begin
  inherited Destroy;
end;

//----------------- End of TXcfLayer -------------------------------------------


initialization
  // Register xcf fileformat
  FileFormatList.RegisterFileFormat('xcf', gesXCF, '', [ftRaster, ftLayered], False, TXCFGraphic);
//finalization
  // No need to unregister at finalization since that will be done in FileFormatList.Free.
end.
