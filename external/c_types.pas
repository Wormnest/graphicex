// Unit defining some types often used in C to make conversion easier
// Jacob Boerema, 2015.

unit C_Types;

{$IFDEF FPC}
  {$mode delphi}
{$ELSE}
  {$IFDEF CPUX64}
    {$DEFINE CPU64}
  {$ENDIF}
{$ENDIF}

interface

uses
  Classes, SysUtils;

type
  // For compatibility with older Delphi versions
  {$IF NOT Declared(NativeInt)}
  NativeInt = Integer;
  {$IFEND}
  {$IF NOT Declared(NativeUInt)}
  NativeUInt = Cardinal;
  {$IFEND}
  {$IF NOT Declared(UInt64)}
  UInt64 = Int64;
  {$IFEND}
  {$IF NOT Declared(PUInt64)}
  // Fpc has UInt64 but not PUint64 so handle this separately from UInt64
  PUint64 = ^Uint64;
  {$IFEND}


  // C specific
  size_t  = NativeUInt;
  Psize_t = ^size_t;

  float  = Single;
  pfloat = ^float;

  short  = SmallInt;         ushort = Word;
  int8   = ShortInt;         uint8  = Byte;
  int16  = SmallInt;         uint16 = Word;
  int32  = Integer;          uint32 = Cardinal;
  int    = Integer;          uint   = Cardinal;
  pint   = ^int;             puint  = ^uint;

  {$IFDEF Windows}
  long   = LongInt;          ulong  = LongWord;
  {$ELSE}
  {$IFDEF CPU64}
  long   = Int64;            ulong  = UInt64;
  {$ELSE}
  {$ENDIF}
  long   = LongInt;          ulong  = LongWord;
  {$ENDIF}


implementation

end.

