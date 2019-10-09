{ gexMemory A Memory access class for GraphicEx.
  Dual License: MPL 1.1 or LGPL 2.1 with linking exception (the "FPC modified LGPL License")
  Portions Created by Jacob Boerema are Copyright (C) 2017-2017 Jacob Boerema.
  All Rights Reserved.
  This fork of GraphicEx can be found at https://bitbucket.org/jacobb/graphicex
  Mirror: https://github.com/Wormnest/graphicex
}
unit gexMemory;

interface

{$I gexdefines.inc}

{$IFNDEF FPC}
  // Delphi
  {$I Compilers.inc}
{$ENDIF}

{$I GraphicConfiguration.inc}

type
  TByteOrder = (boLittleEndian, boBigEndian);
  TMemoryAccess = class(TObject)
  private
    FMemory: Pointer;     // Start of memory block we will provide access to
    FSize: Int64;         // Size of memory block
    FCurPos: PByte;       // Current position in memory
    FMaxPos: PByte;       // Precomputed max position (actually first position that's not allowed)
    FByteOrder: TByteOrder;
    FImageType: string;

    function GetCurrentPosition(): UInt64;
  public
    constructor Create(AMemory: Pointer; ASize: UInt64; AImageType: string);
    destructor Destroy; override;

    // Sets FCurPos without checks!!!!
    procedure TEMPORARY_SET_POSITION(APos: PByte);

    procedure GetBytes(var ABuffer; ASize: Cardinal);
    function GetByte(): Byte;
    function GetWord(): Word;
    function GetLongWord(): LongWord;
    procedure SeekForward(ACount: UInt64);
    procedure SeekBackward(ACount: UInt64);
    procedure SeekFromBeginning(ACount: UInt64);

    // In case we really need direct access (not advisable)
    // ask for a pointer and it will check the amount of bytes requested
    function GetAccessToMemory(ASize: Cardinal): PByte;

    property ByteOrder: TByteOrder read FByteOrder write FByteOrder default boLittleEndian;
    property CurrentPosition: UInt64 read GetCurrentPosition;
  end;

implementation

uses gexTypes, gexUtils, GraphicStrings;

// Sets FCurPos without checks!!!!
procedure TMemoryAccess.TEMPORARY_SET_POSITION(APos: PByte);
begin
  FCurPos := APos;
end;

constructor TMemoryAccess.Create(AMemory: Pointer; ASize: UInt64; AImageType: string);
begin
  FMemory := AMemory;
  FSize := ASize;
  FCurPos := FMemory;
  FMaxPos := PByte(NativeUInt(FMemory)+ASize);
  FImageType := AImageType;
  FByteOrder := boLittleEndian;
  // Computer memory sizes that would overflow this are not possible at this time
end;

destructor TMemoryAccess.Destroy;
begin
  inherited Destroy;
end;

function TMemoryAccess.GetCurrentPosition(): UInt64;
begin
  Result := NativeUInt(FCurPos)-NativeUInt(FMemory);
end;

function TMemoryAccess.GetAccessToMemory(ASize: Cardinal): PByte;
var NewPos: PByte;
begin
  NewPos := PByte(NativeUInt(FCurPos) + ASize);
  if NewPos <= FMaxPos then begin
    // In this case NewPos is only used to check if the memory size is valid.
    // We return the pointer to the current position.
    Result := FCurPos;
  end
  else
    Result := nil;
end;

procedure TMemoryAccess.GetBytes(var ABuffer; ASize: Cardinal);
var NewPos: PByte;
begin
  NewPos := PByte(NativeUInt(FCurPos) + ASize);
  // To be able to read the last byte of the file we need to allow
  // curpos to get moved to maxpos
  if NewPos <= FMaxPos then begin
    Move(FCurPos^, ABuffer, ASize);
    FCurPos := NewPos;
  end
  else begin
    // Buffer overflow!
    raise EgexMemoryAccessException.CreateFmt(gesMemoryAccess,
      [FImageType]) {$IFNDEF FPC}at ReturnAddress{$ENDIF};
  end;
end;

function TMemoryAccess.GetByte(): Byte;
var NewPos: PByte;
begin
  NewPos := PByte(NativeUInt(FCurPos) + 1);
  // To be able to read the last byte of the file we need to allow
  // curpos to get moved to maxpos
  if NewPos <= FMaxPos then begin
    Result := FCurPos^;
    FCurPos := NewPos;
  end
  else begin
    // Buffer overflow!
    raise EgexMemoryAccessException.CreateFmt(gesMemoryAccess,
      [FImageType]) {$IFNDEF FPC}at ReturnAddress{$ENDIF};
  end;
end;

function TMemoryAccess.GetWord(): Word;
var NewPos: PByte;
begin
  NewPos := PByte(NativeUInt(FCurPos) + 2);
  // To be able to read the last byte of the file we need to allow
  // curpos to get moved to maxpos
  if NewPos <= FMaxPos then begin
    if FByteOrder = boLittleEndian then
      Result := PWord(FCurPos)^
    else
      Result := SwapEndian(PWord(FCurPos)^);
    FCurPos := NewPos;
  end
  else begin
    // Buffer overflow!
    raise EgexMemoryAccessException.CreateFmt(gesMemoryAccess,
      [FImageType]) {$IFNDEF FPC}at ReturnAddress{$ENDIF};
  end;
end;

function TMemoryAccess.GetLongWord(): LongWord;
var NewPos: PByte;
begin
  NewPos := PByte(NativeUInt(FCurPos) + 4);
  // To be able to read the last byte of the file we need to allow
  // curpos to get moved to maxpos
  if NewPos <= FMaxPos then begin
    if FByteOrder = boLittleEndian then
      Result := PLongWord(FCurPos)^
    else
      Result := SwapEndian(PLongWord(FCurPos)^);
    FCurPos := NewPos;
  end
  else begin
    // Buffer overflow!
    raise EgexMemoryAccessException.CreateFmt(gesMemoryAccess,
      [FImageType]) {$IFNDEF FPC}at ReturnAddress{$ENDIF};
  end;
end;

procedure TMemoryAccess.SeekForward(ACount: UInt64);
var NewPos: PByte;
begin
  NewPos := PByte(NativeUInt(FCurPos) + ACount);
  if NewPos < FMaxPos then begin
    FCurPos := NewPos;
  end
  else begin
    // Buffer overflow!
    raise EgexMemoryAccessException.CreateFmt(gesMemoryAccess,
      [FImageType]) {$IFNDEF FPC}at ReturnAddress{$ENDIF};
  end;
end;

procedure TMemoryAccess.SeekBackward(ACount: UInt64);
var NewPos: PByte;
begin
  NewPos := PByte(NativeUInt(FCurPos) - ACount);
  if NewPos >= PByte(FMemory) then begin
    FCurPos := NewPos;
  end
  else begin
    // Buffer overflow!
    raise EgexMemoryAccessException.CreateFmt(gesMemoryAccess,
      [FImageType]) {$IFNDEF FPC}at ReturnAddress{$ENDIF};
  end;
end;

procedure TMemoryAccess.SeekFromBeginning(ACount: UInt64);
var NewPos: PByte;
begin
  NewPos := PByte(NativeUInt(FMemory) + ACount);
  if NewPos < FMaxPos then begin
    FCurPos := NewPos;
  end
  else begin
    // Buffer overflow!
    raise EgexMemoryAccessException.CreateFmt(gesMemoryAccess,
      [FImageType]) {$IFNDEF FPC}at ReturnAddress{$ENDIF};
  end;
end;


end.
