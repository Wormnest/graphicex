unit LibDelphi;

interface

{$I gexdefines.inc}

uses
  Windows, SysUtils, C_Types;

procedure _exit(code: Integer); cdecl;
{$IFDEF FPC} public name 'exit';{$ENDIF}
function  fprintf(stream: Pointer; format: Pointer; arguments: Pointer): Integer; cdecl;
function  sprintf(buffer: Pointer; format: Pointer; arguments: Pointer): Integer; cdecl;
function  snprintf(buffer: Pointer; bufsize: Integer; format: Pointer; arguments: Pointer): Integer; cdecl;
{$IFDEF FPC} public name '_snprintf';{$ENDIF}
function  fputs(s: Pointer; stream: Pointer): Integer; cdecl;
function  fputc(c: Integer; stream: Pointer): Integer; cdecl;
function  isprint(c: Integer): Integer; cdecl;
procedure memset(a: Pointer; b: Integer; c: NativeUInt); cdecl;
//{$IFDEF FPC} public name '_memset';{$ENDIF}
function  memcpy(dest: Pointer; const src: Pointer; count: NativeUInt): Pointer; cdecl;
//{$IFDEF FPC} public name '_memcpy';{$ENDIF}
{$IFNDEF FPC}
{$IFNDEF CPU64}
function  _ftol: Integer; cdecl;
{$ENDIF}
{$ENDIF}
function  malloc(s: NativeUInt): Pointer; cdecl;
//{$IFDEF FPC} public name '_malloc';{$ENDIF}
procedure free(p: Pointer); cdecl;
//{$IFDEF FPC} public name '_free';{$ENDIF}
function  _ltolower(ch: Integer): Integer; cdecl;
function  _ltoupper(ch: Integer): Integer; cdecl;
function  _ltowlower(ch: Integer): Integer; cdecl;
function  _ltowupper(ch: Integer): Integer; cdecl;
function  strcpy(dest: Pointer; src: Pointer): Pointer; cdecl;

// bufsize = -1 means ignore max bufsize
function  sprintfsec(buffer: Pointer; format: Pointer; arguments: Pointer;
  bufsize: Integer = -1): Integer;

var
  __turboFloat: LongBool = False;
  _streams: Integer;

type
  ELibDelphiError = class(Exception);

implementation


// Needed for 64 bits version of LibTiff etc since we can't figure out the
// order yet that we need to link the libs for exit to be found in msvcrt.a
procedure _exit(code: Integer); cdecl;
begin
  Halt(code);
end;

function fputc(c: Integer; stream: Pointer): Integer; cdecl;
var
  m: array[0..1] of AnsiChar;
  n: Cardinal;
  o: Cardinal;
begin
  if c=13 then
  begin
    m[0]:=#13;
    m[1]:=#10;
    n:=2;
  end
  else
  begin
    m[0]:=AnsiChar(c);
    n:=1;
  end;
  WriteFile(NativeUInt(stream),m[0],n,o,nil);
  Result:=c;
end;

function isprint(c: Integer): Integer; cdecl;
begin
  if (c<32) or (127<=c) then
    Result:=0
  else
    Result:=1;
end;

function fputs(s: Pointer; stream: Pointer): Integer; cdecl;
var
  m: Integer;
  n: Pointer;
  o: Cardinal;
begin
  m:=0;
  n:=s;
  while PByte(n)^<>0 do
  begin
    Inc(m);
    Inc(PByte(n));
  end;
  WriteFile(NativeUInt(stream),s^,Cardinal(m),o,nil);
  Result:=1;
end;

function sprintf(buffer: Pointer; format: Pointer; arguments: Pointer): Integer; cdecl;
begin
  Result := sprintfsec(buffer,format,arguments);
end;

function  snprintf(buffer: Pointer; bufsize: Integer; format: Pointer; arguments: Pointer): Integer; cdecl;
begin
  Result := sprintfsec(buffer, format, @arguments, bufsize);
end;

function fprintf(stream: Pointer; format: Pointer; arguments: Pointer): Integer; cdecl;
var
  m: Integer;
  n: Pointer;
  o: Cardinal;
begin
  m:=sprintfsec(nil,format,arguments);
  GetMem(n,m);
  sprintfsec(n,format,arguments);
  WriteFile(NativeUInt(stream),n^,Cardinal(m),o,nil);
  FreeMem(n);
  Result := m;
end;

function strcpy(dest: Pointer; src: Pointer): Pointer; cdecl;
var
  ma,mb: PByte;
  n: Integer;
begin
  ma:=src;
  mb:=dest;
  while True do
  begin
    n:=ma^;
    mb^:=n;
    if n=0 then break;
    Inc(ma);
    Inc(mb);
  end;
  Result:=dest;
end;

function _ltolower(ch: Integer): Integer; cdecl;
begin
  raise ELibDelphiError.Create('LibDelphi - call to _ltolower - should presumably not occur');
end;

function _ltoupper(ch: Integer): Integer; cdecl;
begin
  raise ELibDelphiError.Create('LibDelphi - call to _ltoupper - should presumably not occur');
end;

function _ltowlower(ch: Integer): Integer; cdecl;
begin
  raise ELibDelphiError.Create('LibDelphi - call to _ltowlower - should presumably not occur');
end;

function _ltowupper(ch: Integer): Integer; cdecl;
begin
  raise ELibDelphiError.Create('LibDelphi - call to _ltowupper - should presumably not occur');
end;

// bufsize = -1 means ignore max bufsize
// buffer = nil: means only return the size needed for the buffer
function sprintfsec(buffer: Pointer; format: Pointer; arguments: Pointer;
  bufsize: Integer = -1): Integer;
var
  Modifier: Integer;
  Width: Integer;
  m,ma: PByte;
  mb: Boolean;
  n: PByte;
  o: PByte;
  r: PByte;
  // BufSize checking
  CheckSize: Boolean;
  StopPos: NativeUInt;

procedure Append(const p: AnsiString);
var
  q: Integer;
  IncLen: Integer;
begin
  if Width > Length(p) then
  begin
    if buffer <> nil then
    begin
      IncLen := Width-Length(p);
      if CheckSize then begin
        if NativeUInt(o) + NativeUInt(IncLen) >= StopPos then
          IncLen := StopPos - 1 - NativeUInt(o); // -1: make room for #0
      end;
      for q := 0 to IncLen-1 do
      begin
        o^ := Ord('0');
        Inc(o);
      end
    end
    else
      Inc(o, Width-Length(p));
  end;
  if not CheckSize then begin
    if buffer <> nil then
      CopyMemory(o,PAnsiChar(p),Length(p));
    Inc(o,Length(p));
  end
  else begin
    IncLen := Length(p);
    if buffer <> nil then begin
      // Note: compare with >= StopPos because we also need to take into
      // account the final #0 after the string is copied.
      if NativeUInt(o) + NativeUInt(IncLen) >= StopPos then
        IncLen := StopPos - 1 - NativeUInt(o); // -1: make room for #0
      CopyMemory(o, PAnsiChar(p), IncLen);
    end;
    Inc(o, IncLen);
  end;
end;
begin
  CheckSize := bufsize <> -1;
  if CheckSize then
    if buffer <> nil then
      StopPos := NativeUInt(buffer)+NativeUInt(bufsize)
    else
      StopPos := 0;
  m:=format;
  n:=arguments;
  o:=buffer;
  while True do
  begin
    if m^=0 then break;
    if m^=Ord('%') then
    begin
      ma:=m;
      mb:=True;
      Inc(m);
      Width:=-1;
      Modifier:=0;
      {flags}
      case m^ of
        Ord('-'): mb:=False;
        Ord('+'): mb:=False;
        Ord(' '): mb:=False;
        Ord('#'): mb:=False;
      end;
      if mb then
      begin
        {width}
        case m^ of
          Ord('1')..Ord('9'):
          begin
            Width:=0;
            while True do
            begin
              if (m^<Ord('0')) or (Ord('9')<m^) then break;
              Width:=Width*10+m^-Ord('0');
              Inc(m);
            end;
          end;
          Ord('0'): mb:=False;
          Ord('*'): mb:=False;
        end;
      end;
      if mb then
      begin
        {prec}
        case m^ of
          Ord('.'): mb:=False;
        end;
      end;
      if mb then
      begin
        {modifier}
        case m^ of
          Ord('F'): mb:=False;
          Ord('N'): mb:=False;
          Ord('h'): mb:=False;
          Ord('l'), Ord('I'):
          begin
            {$IFNDEF CPU64}
            Modifier:=4;
            {$ELSE}
            Modifier:=8; // On 64 bits OS this is a 64-bits type
            {$ENDIF}
            Inc(m);
          end;
          Ord('L'): mb:=False;
        end;
      end;
      if mb then
      begin
        {type}
        // Note that on Windows 64 bits signed/unsigned can be specified
        // as %I64d and %I64u. We need to handle that too. (N.B.: Uppercase i, not lowercase l!)
        // Besides that %lld and %llu for the same is also possible.
        if m^ = Ord('6') then begin
          Inc(m);
          if m^ = Ord('4') then begin
            Modifier := 8;
            Inc(m);
          end
          else
            Dec(m);
        end
        else if m^ = Ord('l') then begin
          Inc(m);
          Modifier := 8;
        end;
        case m^ of
          Ord('d'):
          begin
            case Modifier of
              0, 4:
              begin
                Append(IntToStr(PInteger(n)^));
                Inc(m);
                Inc(n,SizeOf(NativeInt));
              end;
              8:
              begin
                Append(IntToStr(PInt64(n)^));
                Inc(m);
                Inc(n,SizeOf(Int64));
              end;
            else
              mb:=False;
            end;
          end;
          Ord('i'): mb:=False;
          Ord('o'): mb:=False;
          Ord('u'):
          begin
            case Modifier of
              0, 4:
              begin
                Append(IntToStr(PCardinal(n)^));
                Inc(m);
                // Note that although we use 4 bytes to show the value on
                // 64 bits Windows the value is apparently put on the stack in 64 bits, 8 bytes!
                Inc(n, SizeOf(NativeUInt));
              end;
              8:
              begin
                {$IF Declared(UIntToStr)}
                Append(UIntToStr(PUInt64(n)^));
                {$ELSE}
                // For now we will have to add it as an Int64 and hope that
                // the value is not higher than High(Int64).
                Append(IntToStr(PUInt64(n)^));
                {$IFEND}
                Inc(m);
                Inc(n, SizeOf(UInt64));
              end;
            else
              mb:=False;
            end;
          end;
          Ord('x'):
          begin
            case Modifier of
              0,4:
              begin
                Append(IntToHex(PCardinal(n)^, 8));
                Inc(m);
                Inc(n, SizeOf(NativeUInt));
              end;
              8:
              begin
                Append(IntToHex(PUInt64(n)^, 16));
                Inc(m);
                Inc(n, SizeOf(UInt64));
              end
            else
              mb:=False;
            end;
          end;
          Ord('X'): mb:=False;
          Ord('f'): mb:=False;
          Ord('e'): mb:=False;
          Ord('g'):
          begin
            case Modifier of
              0:
              begin
                Append(FloatToStr(PSingle(n)^));
                Inc(m);
                Inc(n,SizeOf(Single));
              end;
            else
              mb:=False;
            end;
          end;
          Ord('E'): mb:=False;
          Ord('G'): mb:=False;
          Ord('c'): mb:=False;
          Ord('s'):
          begin
            r:=PPointer(n)^;
            // jb: It's possible to have a PChar that's nil
            if r <> nil then
              while r^<>0 do
              begin
                if buffer <> nil then
                  if not CheckSize or (NativeUInt(o) + 1 < StopPos) then
                    o^ := r^
                  else // No more room in buffer
                    break;
                Inc(o);
                Inc(r);
              end;
            Inc(n,SizeOf(Pointer));
            Inc(m);
          end;
          Ord('%'): mb:=False;
          Ord('n'): mb:=False;
          Ord('p'): mb:=False;
        else
          raise ELibDelphiError.Create('LibDelphi: unexpected specifier in sprintfsec');
        end;
      end;
      if mb=False then
      begin
        m:=ma;
        if buffer<>nil then o^:=m^;
        Inc(o);
        Inc(m);
      end;
    end
    else if m^=10 then
    begin
      if buffer<>nil then o^:=13;
      Inc(o);
      if buffer<>nil then o^:=10;
      Inc(o);
      Inc(m);
    end
    else
    begin
      if buffer<>nil then o^:=m^;
      Inc(o);
      Inc(m);
    end;
  end;
  if buffer<>nil then o^:=0;
  Inc(o);
  Result:=(NativeUInt(o)-NativeUInt(buffer));
end;

procedure free(p: Pointer); cdecl;
begin
  FreeMem(p);
end;

function malloc(s: NativeUInt): Pointer; cdecl;
begin
  Result := AllocMem(s);
end;

{$IFNDEF FPC}
{$IFNDEF CPU64}
function _ftol: Integer; cdecl;
var
  f: double;
begin
  asm
    lea    eax, f             //  BC++ passes floats on the FPU stack
    fstp  qword ptr [eax]     //  Delphi passes floats on the CPU stack
  end;
  Result := Trunc(f);
end;
{$ENDIF}
{$ENDIF}

function memcpy(dest: Pointer; const src: Pointer; count: NativeUInt): Pointer; cdecl;
begin
  CopyMemory(dest,src,count);
  Result:=dest;
end;

procedure memset(a: Pointer; b: Integer; c: NativeUInt); cdecl;
begin
  FillMemory(a,c,b);
end;

end.
