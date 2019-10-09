{$TYPEDADDRESS OFF}

// The contents of this file are subject to the Mozilla Public License
// Version 1.1 (the "License"); you may not use this file except in compliance
// with the License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
//
// Software distributed under the License is distributed on an "AS IS" basis,
// WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for the
// specific language governing rights and limitations under the License.
//
// The original code is GraphicEx.pas, released November 1, 1999.
//
// The initial developer of the original code is Mike Lischke (www.soft-gems.net),
//
// Portions created by Mike Lischke are
// Copyright (C) 1999, 2008 Mike Lischke. All Rights Reserved.
// Portions created by Jacob Boerema are
// Copyright (C) 2013-2015 Jacob Boerema. All Rights Reserved.
// This fork of GraphicEx can be found at https://bitbucket.org/jacobb/graphicex
//
// gexUtils Utility functions for GraphicEx.

unit gexUtils;

interface

{$I gexdefines.inc}

// Return the length of a PAnsiChar buffer terminated with 0.
// Note that since there has to be a terminating 0 character the maximum length
// that can be returned is BufferLength-1. To make it overflow safe we will
// temporarily replace the last char in buffer with 0.
// In case that last char wasn't a 0 and no other 0 was found we will return -1
// since not finding a 0 character is an error.
function SafePAnsiCharLength(Source: PAnsiChar; BufferLength: Integer): Integer;


// Reads the next four bytes from the memory pointed to by Run, converts this into a
// cardinal number (inclusive byte order swapping) and advances Run.
function ReadBigEndianCardinal(var Run: PByte): Cardinal;

// Reads the next four bytes from the memory pointed to by Run, converts this into a
// cardinal number (inclusive byte order swapping) and advances Run.
function ReadBigEndianInteger(var Run: PByte): Integer;

// Reads the next two bytes from the memory pointed to by Run, converts this into a
// word number (inclusive byte order swapping) and advances Run.
function ReadBigEndianWord(var Run: PByte): Word;

// Reads the next four bytes from the memory pointed to by Run, converts this into a
// single number (inclusive byte order swapping) and advances Run.
function ReadBigEndianSingle(var Run: PByte): Single;

// Reads the next eight bytes from the memory pointed to by Run, converts this into a
// double number (inclusive byte order swapping) and advances Run.
function ReadBigEndianDouble(var Run: PByte): Double;

// Reads the next Len bytes from the memory pointed to by Run, converts this into a
// Unicode string (inclusive byte order swapping) and advances Run.
function ReadBigEndianString(var Run: PByte; Len: Cardinal): WideString; overload;

// Same as ReadBigEndianString with length parameter.
// However the length will first be retrieved.
function ReadBigEndianString(var Run: PByte): WideString; overload;

// Reads the next Len bytes from the memory pointed to by Run, converts this into a
// wide string and advances Run with Len bytes.
function ReadUtf8String(var Run: PByte; Len: Cardinal): WideString;

// Same as previous ReadUtf8String, however it first reads the length and converts it
// from BigEndian.
function ReadUtf8StringBigEndianLength(var Run: PByte): WideString;


// Swap/Reverse high and low byte of an array of 16 bit values
procedure SwapWordArrayEndian(P: PWord; Count: Cardinal);

// Reverse bytes (endianness) of an array of 32 bit values
procedure SwapCardinalArrayEndian(P: PCardinal; Count: Cardinal);

{$IFNDEF FPC}
// Reverse bytes of the given 16 bit value. Same as normal Swap for 16 bit values.
function SwapEndian(Value: Word): Word; overload;
function SwapEndian(Value: SmallInt): SmallInt; overload;

// Reverse bytes of the given 32 bit value.
function SwapEndian(Value: Cardinal): Cardinal; overload;
function SwapLong(Value: Cardinal): Cardinal; overload; // deprecated version

// Reverse bytes of the given 32 bit value.
function SwapEndian(Value: Integer): Integer; overload;
function SwapLong(Value: Integer): Integer; overload; // deprecated version

// Reverses the order of the 8 bytes.
function SwapEndian(Value: Int64): Int64; overload;
function SwapLong64(Value: Int64): Int64; overload; // deprecated version
{$ENDIF}

// Reverses the byte order in Source which must be 8 bytes in size (as well as the target).
procedure SwapDouble(const Source; var Target);


implementation


// Return the length of a PAnsiChar buffer terminated with 0.
// Note that since there has to be a terminating 0 character the maximum length
// that can be returned is BufferLength-1. To make it overflow safe we will
// temporarily replace the last char in buffer with 0.
// In case that last char wasn't a 0 and no other 0 was found we will return -1
// since not finding a 0 character is an error.
function SafePAnsiCharLength(Source: PAnsiChar; BufferLength: Integer): Integer;
var
  SaveLastChar: AnsiChar;
  SafeLength: Integer;
  LastIdx: Integer;
begin
  if BufferLength < 1 then begin
    Result := -1;
    Exit;
  end;
  LastIdx := BufferLength-1;
  SaveLastChar := Source[LastIdx];
  Source[LastIdx] := #0;
  SafeLength := Length(Source);
  if (SafeLength < LastIdx) or
    ((SafeLength = LastIdx) and (SaveLastChar = #0)) then
    Result := SafeLength
  else
    Result := -1;
  Source[LastIdx] := SaveLastChar;
end;


//----------------- support functions for image loading ----------------------------------------------------------------

{$IFNDEF FPC}
function SwapEndian(Value: Word): Word;
asm
   {$IFDEF CPU64}
   mov rax, rcx
   {$ENDIF}
   xchg   al, ah
end;

function SwapEndian(Value: SmallInt): SmallInt; overload;
asm
   {$IFDEF CPU64}
   mov rax, rcx
   {$ENDIF}
   xchg   al, ah
end;

{$ENDIF}

// Swap/Reverse high and low byte of an array of 16 bit values
procedure SwapWordArrayEndian(P: PWord; Count: Cardinal);
{$IFNDEF CPU64}
// EAX contains P, EDX contains Count
asm
        TEST    EDX, EDX
        JZ      @@Finish
@@Loop:
        MOV     CX, [EAX]
        XCHG    CH, CL
        MOV     [EAX], CX
        ADD     EAX, 2
        DEC     EDX
        JNZ     @@Loop
@@Finish:
end;
{$ELSE}
var i: Cardinal;
begin
  if Count > 0 then
    for i := 0 to Count-1 do begin
      P^:= SwapEndian(P^); // Same as Swap for Word values
      Inc(P);
    end;
end;
{$ENDIF}

//----------------------------------------------------------------------------------------------------------------------

// Reverse bytes (endianness) of an array of 32 bit values
procedure SwapCardinalArrayEndian(P: PCardinal; Count: Cardinal);
{$IFNDEF CPU64}
// EAX contains P, EDX contains Count
asm
        TEST    EDX, EDX
        JZ      @@Finish
@@Loop:
        MOV     ECX, [EAX]
        BSWAP   ECX
        MOV     [EAX], ECX
        ADD     EAX, 4
        DEC     EDX
        JNZ     @@Loop
@@Finish:
end;
{$ELSE}
var i: Cardinal;
begin
  if Count > 0 then
    for i := 0 to Count-1 do begin
      {$IFDEF FPC}
      P^:= SwapEndian(P^);
      {$ELSE}
      P^:= SwapLong(P^);
      {$ENDIF}
      Inc(P);
    end;
end;
{$ENDIF}

//----------------------------------------------------------------------------------------------------------------------

{$IFNDEF FPC}
// Reverses bytes of the given 32 bit value.
function SwapEndian(Value: Cardinal): Cardinal; //overload;
asm
{$IFDEF CPU64}
        mov     rax, rcx
{$ENDIF}
        BSWAP   EAX
end;

function SwapLong(Value: Cardinal): Cardinal; overload; // deprecated version
begin
  Result := SwapEndian(Value);
end;

//----------------------------------------------------------------------------------------------------------------------

// Reverses bytes of the given 32 bit value.
function SwapEndian(Value: Integer): Integer; overload;
asm
{$IFDEF CPU64}
        mov     rax, rcx
{$ENDIF}
        BSWAP   EAX
end;

function SwapLong(Value: Integer): Integer; overload; // deprecated version
begin
  Result := SwapEndian(Value);
end;
//----------------------------------------------------------------------------------------------------------------------

// Reverses the order of the 8 bytes.
function SwapEndian(Value: Int64): Int64; overload;
begin
  Result := SwapEndian(Cardinal(Value shr 32)) + Int64(SwapEndian(Cardinal(Value))) shl 32;
end;

function SwapLong64(Value: Int64): Int64; overload; // deprecated version
begin
  Result := SwapEndian(Value);
end;
{$ENDIF}

//----------------------------------------------------------------------------------------------------------------------

// Reverses the byte order in Source which must be 8 bytes in size (as well as the target).
procedure SwapDouble(const Source; var Target);
{$IFNDEF FPC}
var
  I: Int64;
{$ENDIF}
begin
  {$IFNDEF FPC}
  I := Int64(Source);
  Int64(Target) := SwapEndian(Cardinal(I shr 32)) + Int64(SwapEndian(Cardinal(I))) shl 32;
  {$ELSE}
  Int64(Target) := SwapEndian(Int64(Source));
  {$ENDIF}
end;

//----------------------------------------------------------------------------------------------------------------------

// Reads the next four bytes from the memory pointed to by Run, converts this into a
// cardinal number (inclusive byte order swapping) and advances Run.
function ReadBigEndianCardinal(var Run: PByte): Cardinal;
begin
  Result := SwapEndian(PCardinal(Run)^);
  Inc(PCardinal(Run));
end;

//----------------------------------------------------------------------------------------------------------------------

// Reads the next four bytes from the memory pointed to by Run, converts this into a
// cardinal number (inclusive byte order swapping) and advances Run.
function ReadBigEndianInteger(var Run: PByte): Integer;
begin
  Result := SwapEndian(PInteger(Run)^);
  Inc(PInteger(Run));
end;

//----------------------------------------------------------------------------------------------------------------------

// Reads the next two bytes from the memory pointed to by Run, converts this into a
// word number (inclusive byte order swapping) and advances Run.
function ReadBigEndianWord(var Run: PByte): Word;
begin
  Result := SwapEndian(PWord(Run)^);
  Inc(Run, SizeOf(Word));
end;

//----------------------------------------------------------------------------------------------------------------------

// Reads the next four bytes from the memory pointed to by Run, converts this into a
// single number (inclusive byte order swapping) and advances Run.
function ReadBigEndianSingle(var Run: PByte): Single;
type TSingleCardinal = record
       case Integer of
         0: (SingleValue: Single);
         1: (CardinalValue: Cardinal);
     end;
begin
  TSingleCardinal(Result).SingleValue :=
    TSingleCardinal(SwapEndian(PCardinal(Run)^)).SingleValue;
  Inc(PCardinal(Run));
end;

//----------------------------------------------------------------------------------------------------------------------

function ReadBigEndianDouble(var Run: PByte): Double;

// Reads the next eight bytes from the memory pointed to by Run, converts this into a
// double number (inclusive byte order swapping) and advances Run.

begin
  SwapDouble(Run^, Result);
  Inc(Run, SizeOf(Double));
end;

//----------------------------------------------------------------------------------------------------------------------

function ReadBigEndianString(var Run: PByte; Len: Cardinal): WideString; overload;

// Reads the next Len bytes from the memory pointed to by Run, converts this into a
// Unicode string (inclusive byte order swapping) and advances Run.

begin
  SetString(Result, PWideChar(Run), Len);
  Inc(PWideChar(Run), Len);
  SwapWordArrayEndian(PWord(Result), Len);
end;

//----------------------------------------------------------------------------------------------------------------------

function ReadBigEndianString(var Run: PByte): WideString; overload;

// Same as ReadBigEndianString with length parameter.
// However the length will first be retrieved.

var
  Len: Cardinal;

begin
  Len := ReadBigEndianCardinal(Run);
  Result := ReadBigEndianString(Run, Len);
end;

//----------------------------------------------------------------------------------------------------------------------

function ReadUtf8String(var Run: PByte; Len: Cardinal): WideString;

// Reads the next Len bytes from the memory pointed to by Run, converts this into a
// wide string and advances Run with Len bytes.

begin
  SetString(Result, PAnsiChar(Run), Len); // Not PWideChar!
  Inc(Run, Len);
end;

//----------------------------------------------------------------------------------------------------------------------

function ReadUtf8StringBigEndianLength(var Run: PByte): WideString;
var Len: Cardinal;
// Same as previous ReadUtf8String, however it first reads the length and converts it
// from BigEndian.

begin
  Len := ReadBigEndianCardinal(Run);
  Result := ReadUtf8String(Run, Len);
end;

//----------------------------------------------------------------------------------------------------------------------

end.
