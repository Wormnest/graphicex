unit ZLibDelphi;

interface

uses
  Windows, SysUtils;

const

  // jb Note version number and obj files updated from 1.2.1 to 1.2.8.
  // However other defines etc have not been updated.
  ZLIB_VERSION = '1.2.8';

  Z_NO_FLUSH = 0;
  // jb next 3 copied from GXzlib
  Z_PARTIAL_FLUSH = 1; // will be removed, use Z_SYNC_FLUSH instead
  Z_SYNC_FLUSH    = 2;
  Z_FULL_FLUSH    = 3;
  Z_FINISH = 4;

  Z_OK = 0;
  Z_STREAM_END = 1;

type

  PRZStream = ^RZStream;

  RZStream = record
    NextIn: PByte;
    AvailIn: Cardinal;
    TotalIn: Cardinal;
    NextOut: PByte;
    AvailOut: Cardinal;
    TotalOut: Cardinal;
    Msg: PAnsiChar;
    State: Pointer;
    AllocFunc: Pointer;
    FreeFunc: Pointer;
    Opaque: Cardinal;
    DataType: Integer;
    Adler: Cardinal;
    Reserved: Cardinal;
  end;

// jb copied from GXzlib
function adler32(adler: Cardinal; buf: Pointer; len: Integer): Cardinal; cdecl; external;
function crc32(crc: Cardinal; buf: Pointer; len: Cardinal): Cardinal; cdecl; external;

function  InflateInit(strm: Pointer): Integer;
function  inflateInit_(strm: Pointer; version: Pointer; stream_size: Integer): Integer; cdecl; external;
function  inflateReset(strm: Pointer): Integer; cdecl; external;
function  inflate(strm: Pointer; flush: Integer): Integer; cdecl; external;
function  inflateSync(strm: Pointer): Integer; cdecl; external;
function  deflateInit(strm: Pointer; level: Integer): Integer;
function  deflateInit_(strm: Pointer; level: Integer; version: Pointer; stream_size: Integer): Integer; cdecl; external;
function  deflateReset(strm: Pointer): Integer; cdecl; external;
function  deflate(strm: Pointer; flush: Integer): Integer; cdecl; external;
function  deflateEnd(strm: Pointer): Integer; cdecl; external;
function  inflateEnd(strm: Pointer): Integer; cdecl; external;
function  deflateParams(strm: Pointer; level: Integer; strategy: Integer): Integer; cdecl; external;

implementation

uses
  LibStub;


// Initializes the internal stream state for decompression.
//
// InflateInit returns Z_OK if success, Z_MEM_ERROR if there was not enough memory, Z_VERSION_ERROR if the zlib library
// version is incompatible with the version assumed by the caller. Msg is reset if there is no
// error message. InflateInit does not perform any decompression: this will be done by Inflate.
function InflateInit(strm: Pointer): Integer;
begin
  Result := InflateInit_(strm, PAnsiChar(ZLIB_VERSION), SizeOf(RZStream));
end;

function deflateInit(strm: Pointer; level: Integer): Integer;
begin
  Result:=deflateInit_(strm,level,PAnsiChar(ZLIB_VERSION),SizeOf(RZStream));
end;


{$L deflate.obj}
{$L inflate.obj}
{$L inftrees.obj}
{$L infback.obj}
{$L inffast.obj}
{$L trees.obj}
{$L compress.obj}
{$L adler32.obj}
{$L crc32.obj}
// The next 2 are not used in DelphiZLib
{$L zutil.obj}
{$L uncompr.obj}

end.

