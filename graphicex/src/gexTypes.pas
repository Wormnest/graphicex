{$TYPEDADDRESS OFF}

// The contents of this file are subject to the Mozilla Public License
// Version 1.1 (the "License"); you may not use this file except in compliance
// with the License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
//
// Software distributed under the License is distributed on an "AS IS" basis,
// WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for the
// specific language governing rights and limitations under the License.
//
// Portions created by Jacob Boerema are
// Copyright (C) 2013-2017 Jacob Boerema. All Rights Reserved.
// This fork of GraphicEx can be found at https://bitbucket.org/jacobb/graphicex
//
// Basic type definitions for GraphicEx.

unit gexTypes;

interface

{$I gexdefines.inc}

{$IFNDEF FPC}
  {$I Compilers.inc}
{$ENDIF}

uses SysUtils;

type
  // Define a new base exception from which we can derive our exceptions.
  EgexBaseException = class(Exception);
  EgexInvalidGraphic = class(EgexBaseException);
  EgexColorConversionError = class(EgexBaseException);
  EgexGraphicCompressionError = class(EgexBaseException);
  EgexStretchException = class(EgexBaseException);
  EgexMemoryAccessException = class(EgexBaseException);
  EgexSaveError = class(EgexBaseException);

  // Arrays for easy access to some base types.
  TCardinalArray = array of Cardinal;
  TByteArray = array of Byte;
  TFloatArray = array of Single;

  // Compatibility layer
  {$IF NOT Declared(UInt64)}
  UInt64 = Int64;
  {$IFEND}
  {$IF NOT Declared(PUInt64)}
  PUInt64 = ^UInt64; // Separate from declaring UInt64 since Fpc doesn't define puint64.
  {$IFEND}
  {$IF NOT Declared(NativeInt)}
  NativeInt = Integer;
  {$IFEND}
  {$IF NOT Declared(NativeUInt)}
  NativeUInt = Cardinal;
  {$IFEND}

  {$ifndef COMPILER_6_UP}
  {$IFNDEF FPC}
  PCardinal = ^Cardinal;
  {$ENDIF}
  {$endif COMPILER_6_UP}


// ReturnAddress is used in our Error functions to show a better error location:
// In case of an exception it shows the address of the function that called
// our error function instead of the address of the error function itself.
{$IFNDEF FPC} {$IFNDEF CPUX64} {$IF NOT Declared(ReturnAddress)}
{$DEFINE DEFINE_RETURNADDRESS}
// Delphi 6 doesn't have ReturnAddress
function ReturnAddress: Pointer; assembler;
{$IFEND} {$ENDIF} {$ENDIF}

implementation

{$IFDEF DEFINE_RETURNADDRESS}
// Delphi 6 doesn't have ReturnAddress
function ReturnAddress: Pointer; assembler;
asm
        MOV     EAX,[EBP+4]
end;
{$ENDIF}

end.
 
