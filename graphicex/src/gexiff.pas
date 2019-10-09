{
  gexIFF Base class for loading generic IFF files.
  Dual License: MPL 1.1 or LGPL 2.1 with linking exception (the "FPC modified LGPL License")
  Portions Created by Jacob Boerema are Copyright (C) 2015 Jacob Boerema.
  All Rights Reserved.
}
unit gexIFF;

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

uses
  Classes, SysUtils,
  C_Types, GraphicEx;

type
  // Make IFF ID accessible as either a string of 4 chars or a Cardinal
  TIffID = record
    case boolean of
      False: (ctag: array [0..3] of AnsiChar);
      True:  (tag: Cardinal);
  end;

  // IFF form chunk including type
  TIffChunk = record
   ckID:     TIffID;
   ckSize:   Cardinal;
   ckType:   TIffID;
  end;

  // IFF chunk info that we are saving
  PDataChunk = ^TDataChunk;
  TDataChunk = record
    ChunkID:      TIffID;      // Chunk ID
    ChunkType:    TIffID;      // Group type. Only valid for group chunks
    ChunkOfs:     PByte;       // Memory offset to start of chunk
    ChunkSize:    UInt64;      // Size of chunk
    ChunkParent:  PDataChunk;  // Parent group
  end;

  // IFF group types
  TIffGroupType = (igtForm, igtCat, igtList);

  // Access to file contents
  TMemoryData = record
    mdStart: PByte;
    mdEnd:   PByte;  // To save time on computing mdStart + mdSize
    mdSize:  UInt64;
    mdPos:   PByte;
  end;
  PMemoryData = ^TMemoryData;

  TIffGraphicBase = class(TGraphicExGraphic)
  private
    FAlignment: Byte;
    FGroups:    PDataChunk;
    FCurGroup:  PDataChunk;
    FCurChunk:  PDataChunk;
    FIffType:   string;

  protected
    class function CanHandle(const MainIffChunk: TIffChunk): Boolean; virtual;

    function  ReadIffData(const Data: PMemoryData; ACount: Cardinal; Buf: Pointer): Cardinal;
    function  ReadIffUInt8(const Data: PMemoryData): Byte;
    function  ReadIffUInt16(const Data: PMemoryData): Word;
    function  ReadIffUInt32(const Data: PMemoryData): Cardinal;
    function  ReadIffFloat(const Data: PMemoryData): Single;

    function  ReadIffChunk(const Data: PMemoryData): PDataChunk;
    procedure SeekNextIffChunk(const Data: PMemoryData);
    procedure FreeAllChunkData;

    function  IsGroupChunk(const AChunkID: TIffID; out AGroupType: TIffGroupType): boolean; virtual;
  public
    constructor Create; override;
    destructor  Destroy; override;

    class function CanLoad(const Memory: Pointer; Size: Int64): Boolean; override;

    property Alignment: Byte read FAlignment; // Should be a multiple of 2 (default = 2)
    property IffType: string read FIffType write FIffType;
  end;


const
  // IFF Group ID's
  IFF_ID_FORM      = $464f524d; // FORM (word aligned data)
  // Only in Maya?
  IFF_ID_FOR4      = $464f5234; // FOR4 (data aligned on 4 bytes)
  IFF_ID_FOR8      = $464f5238; // FOR8 (data aligned on 8 bytes)

  // IFF Chunk ID's
  IFF_ID_FVER      = $46564552; // FVER
  // ANNO
  // AUTH
  // DATE

implementation

uses gexTypes, {$IFNDEF FPC}gexUtils,{$ENDIF} GraphicStrings;

//------------------------------------------------------------------------------
//                           TIffGraphicBase
//------------------------------------------------------------------------------

// Read ACount bytes of data into Buf.
// Returns number of bytes placed into Buf.
function TIffGraphicBase.ReadIffData(const Data: PMemoryData; ACount: Cardinal; Buf: Pointer): Cardinal;
begin
  if NativeUInt(Data.mdPos) + ACount > NativeUInt(Data.mdEnd) then
    Result := UInt64(NativeUInt(Data.mdEnd) - NativeUInt(Data.mdPos))
  else
    Result := ACount;
  Move(Data.mdPos^, Buf^, Result);
  Inc(Data.mdPos, Result);
end;

//------------------------------------------------------------------------------

// Read one 16 bits Word.
function TIffGraphicBase.ReadIffUInt8(const Data: PMemoryData): Byte;
var Buf: array [0..0] of Byte;
    BytesInBuf: Integer;
begin
  BytesInBuf := ReadIffData(Data, SizeOf(Buf), @Buf);

  if BytesInBuf < SizeOf(Buf) then
    raise EgexInvalidGraphic.CreateFmt(gesStreamReadError, [IffType]);

  // Just 1 byte: no need for endian swap
  Result := Buf[0];
end;

//------------------------------------------------------------------------------

// Read one 16 bits Word.
function TIffGraphicBase.ReadIffUInt16(const Data: PMemoryData): Word;
var Buf: array [0..1] of Byte;
    BytesInBuf: Integer;
begin
  BytesInBuf := ReadIffData(Data, SizeOf(Buf), @Buf);

  if BytesInBuf < SizeOf(Buf) then
    raise EgexInvalidGraphic.CreateFmt(gesStreamReadError, [IffType]);

  // IFF file format is big endian
  Result := SwapEndian(Word(Buf));
end;

//------------------------------------------------------------------------------

// Read one 32 bits Cardinal.
function TIffGraphicBase.ReadIffUInt32(const Data: PMemoryData): Cardinal;
var Buf: array [0..3] of Byte;
    BytesInBuf: Integer;
begin
  BytesInBuf := ReadIffData(Data, SizeOf(Buf), @Buf);

  if BytesInBuf < SizeOf(Buf) then
    raise EgexInvalidGraphic.CreateFmt(gesStreamReadError, [IffType]);

  // IFF file format is big endian
  Result := SwapEndian(Cardinal(Buf));
end;

//------------------------------------------------------------------------------

// Read one 32 bits Single.
function TIffGraphicBase.ReadIffFloat(const Data: PMemoryData): Single;
type
  Dummy = record
    case Boolean of
      False: (DummyUInt32: Cardinal);
      True:  (DummySingle: Single);
  end;
begin
  // IFF file format is big endian
  Dummy(Result).DummyUInt32 := ReadIffUInt32(Data);
end;

//------------------------------------------------------------------------------

function TIffGraphicBase.IsGroupChunk(const AChunkID: TIffID; out AGroupType: TIffGroupType): boolean;
begin
  case AChunkID.tag of
    IFF_ID_FOR4,
    IFF_ID_FOR8,
    IFF_ID_FORM:
      begin
        Result := True;
        AGroupType := igtForm;
      end
  else
    Result := False;
  end;
end;

function TIffGraphicBase.ReadIffChunk(const Data: PMemoryData): PDataChunk;
var
  ChunkInfo: TIffChunk;
  BytesInBuf: Cardinal;
  AChunk: PDataChunk;
  AGroupType: TIffGroupType;
  ChunkOfs: PByte;
begin
  ChunkOfs := Data.mdPos;
  // Get the info of chunk
  BytesInBuf := ReadIffData(Data, SizeOf(ChunkInfo), @ChunkInfo);
  if BytesInBuf < SizeOf(ChunkInfo) then
    raise EgexInvalidGraphic.CreateFmt(gesStreamReadError, [IffType]);

  // Endian swap
  ChunkInfo.ckID.tag   := SwapEndian(ChunkInfo.ckID.tag);
  ChunkInfo.ckSize     := SwapEndian(ChunkInfo.ckSize);
  ChunkInfo.ckType.tag := SwapEndian(ChunkInfo.ckType.tag);

  // Allocate memory for chunk
  GetMem(AChunk, SizeOf(TDataChunk));

  // Is it a group chunk?
  if IsGroupChunk(ChunkInfo.ckID, AGroupType) then begin
    if FGroups = nil then begin
      FGroups := AChunk;
      // For now we assume the alignment can only be changed in the outermost group
      // Because of endian swap we don't look at pos 3 but at pos 0!
      case ChunkInfo.ckID.ctag[0] of
        '4': FAlignment := 4;
        '8': FAlignment := 8;
      else
        FAlignment := 2;
      end;
    end;
    FCurGroup := AChunk;

    // Init Group chunk type
    AChunk.ChunkType.tag := ChunkInfo.ckType.tag;
  end
  else begin
    // data chunk
    AChunk.ChunkType.tag := 0;
    // We read 4 bytes too much since we are not a group: set back position by 4
    Dec(Data.mdPos, 4);
  end;

  AChunk.ChunkParent := FCurChunk;
  FCurChunk := AChunk;

  FCurChunk.ChunkID.tag  := ChunkInfo.ckID.tag;
  FCurChunk.ChunkSize := ChunkInfo.ckSize;
  FCurChunk.ChunkOfs  := ChunkOfs;

  Result := FCurChunk;
end;

//------------------------------------------------------------------------------

procedure TIffGraphicBase.SeekNextIffChunk(const Data: PMemoryData);
var
  NextOfs: UInt64;
  AlignMod: Cardinal;
  AChunk: PDataChunk;
begin
  // Compute Offset of next chunk
  NextOfs := UInt64(FCurChunk.ChunkOfs) + FCurChunk.ChunkSize + 8;

  // Iff chunks need to be aligned on FAlignment bytes
  AlignMod := NextOfs mod FAlignment;
  if AlignMod <> 0 then
    Inc(NextOfs, FAlignment - AlignMod);

  // Update position
  Data.mdPos := PByte(NextOfs);

  AChunk := FCurChunk.ChunkParent;
  FreeMem(FCurChunk);
  FCurChunk := AChunk;
end;

procedure TIffGraphicBase.FreeAllChunkData;
var
  AChunk: PDataChunk;
begin
  while FCurChunk <> nil do begin
    AChunk := FCurChunk.ChunkParent;
    FreeMem(FCurChunk);
    FCurChunk := AChunk;
  end;
end;

//------------------------------------------------------------------------------

// Override this base handler for a specific IFF file.
class function TIffGraphicBase.CanHandle(const MainIffChunk: TIffChunk): Boolean;
begin
  Result := False;
end;

//------------------------------------------------------------------------------

class function TIffGraphicBase.CanLoad(const Memory: Pointer; Size: Int64): Boolean;
var chunkInfo: TIffChunk;
begin
  Result := False;
  if Size < SizeOf(chunkInfo) then
    Exit;

  // Get the info of the first chunk
  Move(Memory^, chunkInfo, SizeOf(chunkInfo));

  // Generic IFF CanLoad needs a specific function to determine if the class
  // can handle this IFF file.
  Result := CanHandle(chunkInfo);
end;

//------------------------------------------------------------------------------

constructor TIffGraphicBase.Create;
begin
  inherited Create;
  FIffType := 'IFF';
  FGroups := nil;
  FCurGroup := nil;
  FCurChunk := nil;
  FAlignment := 2;
end;

//------------------------------------------------------------------------------

destructor TIffGraphicBase.Destroy;
begin
  FreeAllChunkData; // Make sure everything is freed.
  inherited Destroy;
end;

//------------------------------------------------------------------------------

end.

