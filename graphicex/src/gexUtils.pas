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
// Copyright (C) 2013 Jacob Boerema. All Rights Reserved.
// This fork of GraphicEx can be found at https://bitbucket.org/jacobb/jgb-thirdparty
//
// gexUtils Utility functions for GraphicEx.

unit gexUtils;

interface

// Reads the next four bytes from the memory pointed to by Run, converts this into a
// cardinal number (inclusive byte order swapping) and advances Run.
function ReadBigEndianCardinal(var Run: PByte): Cardinal;

// Reads the next four bytes from the memory pointed to by Run, converts this into a
// cardinal number (inclusive byte order swapping) and advances Run.
function ReadBigEndianInteger(var Run: PByte): Integer;

// Reads the next two bytes from the memory pointed to by Run, converts this into a
// word number (inclusive byte order swapping) and advances Run.
function ReadBigEndianWord(var Run: PByte): Word;

// Reads the next eight bytes from the memory pointed to by Run, converts this into a
// double number (inclusive byte order swapping) and advances Run.
function ReadBigEndianDouble(var Run: PByte): Double;

// Reads the next Len bytes from the memory pointed to by Run, converts this into a
// Unicode string (inclusive byte order swapping) and advances Run.
function ReadBigEndianString(var Run: PByte; Len: Cardinal): WideString; overload;

// Same as ReadBigEndianString with length parameter.
// However the length will first be retrieved.
function ReadBigEndianString(var Run: PByte): WideString; overload;


// swaps high and low byte of 16 bit values
procedure SwapShort(P: PWord; Count: Cardinal);

// swaps high and low bytes of 32 bit values
procedure SwapLong(P: PInteger; Count: Cardinal); overload;

// Swaps high and low bytes of the given 32 bit value.
function SwapLong(Value: Cardinal): Cardinal; overload;

// Swaps high and low bytes of the given 32 bit value.
function SwapLong(Value: Integer): Integer; overload;

// Reverses the byte order in Source which must be 8 bytes in size (as well as the target).
procedure SwapDouble(const Source; var Target);


implementation

//----------------- support functions for image loading ----------------------------------------------------------------

procedure SwapShort(P: PWord; Count: Cardinal);

// swaps high and low byte of 16 bit values
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

//----------------------------------------------------------------------------------------------------------------------

procedure SwapLong(P: PInteger; Count: Cardinal); overload;

// swaps high and low bytes of 32 bit values
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

//----------------------------------------------------------------------------------------------------------------------

function SwapLong(Value: Cardinal): Cardinal; overload;

// Swaps high and low bytes of the given 32 bit value.

asm
        BSWAP   EAX
end;

//----------------------------------------------------------------------------------------------------------------------

function SwapLong(Value: Integer): Integer; overload;

// Swaps high and low bytes of the given 32 bit value.

asm
        BSWAP   EAX
end;

//----------------------------------------------------------------------------------------------------------------------

procedure SwapDouble(const Source; var Target);

// Reverses the byte order in Source which must be 8 bytes in size (as well as the target).

var
  I: Int64;

begin
  I := Int64(Source);
  Int64(Target) := SwapLong(Cardinal(I shr 32)) + Int64(SwapLong(Cardinal(I))) shl 32;
end;

//----------------------------------------------------------------------------------------------------------------------

function ReadBigEndianCardinal(var Run: PByte): Cardinal;

// Reads the next four bytes from the memory pointed to by Run, converts this into a
// cardinal number (inclusive byte order swapping) and advances Run.

begin
  Result := SwapLong(PCardinal(Run)^);
  Inc(PCardinal(Run));
end;

//----------------------------------------------------------------------------------------------------------------------

function ReadBigEndianInteger(var Run: PByte): Integer;

// Reads the next four bytes from the memory pointed to by Run, converts this into a
// cardinal number (inclusive byte order swapping) and advances Run.

begin
  Result := SwapLong(PInteger(Run)^);
  Inc(PInteger(Run));
end;

//----------------------------------------------------------------------------------------------------------------------

function ReadBigEndianWord(var Run: PByte): Word;

// Reads the next two bytes from the memory pointed to by Run, converts this into a
// word number (inclusive byte order swapping) and advances Run.

begin
  Result := Swap(PWord(Run)^);
  Inc(Run, SizeOf(Word));
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
  SwapShort(Pointer(Result), Len);
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


end.
