unit ZLibDelphi;

interface

{$IFDEF FPC}
  {$mode delphi}
{$ENDIF}

uses
  Windows, SysUtils;

const
  ZLIB_VERSION = '1.2.8'; // for compatibility with versions < 1.0.2

  // Constants from zlib.h
  // Flush values.
  Z_NO_FLUSH      = 0;
  Z_PARTIAL_FLUSH = 1;
  Z_SYNC_FLUSH    = 2;
  Z_FULL_FLUSH    = 3;
  Z_FINISH        = 4;
  Z_BLOCK         = 5;
  Z_TREES         = 6;

  // Return codes for the compression/decompression functions.
  // Negative values are errors, positive values are used for special but normal events.
  Z_OK            =  0;
  Z_STREAM_END    =  1;
  Z_NEED_DICT     =  2;
  Z_ERRNO         = -1;
  Z_STREAM_ERROR  = -2;
  Z_DATA_ERROR    = -3;
  Z_MEM_ERROR     = -4;
  Z_BUF_ERROR     = -5;
  Z_VERSION_ERROR = -6;

  // compression levels
  Z_NO_COMPRESSION      =  0;
  Z_BEST_SPEED          =  1;
  Z_BEST_COMPRESSION    =  9;
  Z_DEFAULT_COMPRESSION = -1;

  // compression strategy
  Z_FILTERED            = 1;
  Z_HUFFMAN_ONLY        = 2;
  Z_RLE                 = 3;
  Z_FIXED               = 4;
  Z_DEFAULT_STRATEGY    = 0;

  // Possible values of the data_type field
  Z_BINARY   = 0;
  Z_TEXT     = 1;
  Z_ASCII    = Z_TEXT;   // for compatibility with 1.2.2 and earlier
  Z_UNKNOWN  = 2;

  // The deflate compression method (the only one supported in this version)
  Z_DEFLATED = 8;

  Z_NULL     = 0;  // for initializing zalloc, zfree, opaque

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
    Opaque: Pointer;
    DataType: Integer;
    Adler: Cardinal;
    Reserved: Cardinal;
  end;

// jb copied from GXzlib
function adler32(adler: Cardinal; buf: Pointer; len: Integer): Cardinal; cdecl; external;
function crc32(crc: Cardinal; buf: Pointer; len: Cardinal): Cardinal; cdecl; external;

function  InflateInit(var strm: RZStream): Integer;
function  inflateInit_(var strm: RZStream; version: PAnsiChar; stream_size: Integer): Integer; cdecl; external;
function  inflateReset(var strm: RZStream): Integer; cdecl; external;
function  inflate(var strm: RZStream; flush: Integer): Integer; cdecl; external;
function  inflateSync(var strm: RZStream): Integer; cdecl; external;
function  deflateInit(var strm: RZStream; level: Integer): Integer;
function  deflateInit_(var strm: RZStream; level: Integer; version: PAnsiChar; stream_size: Integer): Integer; cdecl; external;
function  deflateReset(var strm: RZStream): Integer; cdecl; external;
function  deflate(var strm: RZStream; flush: Integer): Integer; cdecl; external;
function  deflateEnd(var strm: RZStream): Integer; cdecl; external;
function  inflateEnd(var strm: RZStream): Integer; cdecl; external;
function  deflateParams(var strm: RZStream; level: Integer; strategy: Integer): Integer; cdecl; external;

implementation

{$IFNDEF FPC} // Let's try fpc without libstub
uses
  LibStub;
{$ENDIF}


// Initializes the internal stream state for decompression.
//
// InflateInit returns Z_OK if success, Z_MEM_ERROR if there was not enough memory, Z_VERSION_ERROR if the zlib library
// version is incompatible with the version assumed by the caller. Msg is reset if there is no
// error message. InflateInit does not perform any decompression: this will be done by Inflate.
function InflateInit(var strm: RZStream): Integer;
begin
  Result := InflateInit_(strm, PAnsiChar(ZLIB_VERSION), SizeOf(RZStream));
end;

function deflateInit(var strm: RZStream; level: Integer): Integer;
begin
  Result:=deflateInit_(strm,level,PAnsiChar(ZLIB_VERSION),SizeOf(RZStream));
end;


{$IFNDEF FPC}
{$L deflate.obj}
{$L inflate.obj}
{$L inftrees.obj}
{.$L infback.obj} // Is most likely not needed, the functions are not declared here anyway.
{$L inffast.obj}
{$L trees.obj}
{$L compress.obj}
{$L adler32.obj}
{$L crc32.obj}
// The next 2 are not used in DelphiZLib
{$L zutil.obj}
{.$L uncompr.obj} // Seems not to be needed by, uncompress also isn't declared here.
{$ELSE}
  // fpc
  {$IFDEF MSWINDOWS}
    {$IFNDEF CPU64}
      {$LINKLIB libcrtdll} // _malloc and _free
    {$ELSE}
      {$LINKLIB libmsvcrt.a}
      {$LINKLIB libkernel32.a}
    {$ENDIF}
  {$ENDIF}
  {$IFDEF UNIX}
    Todo...
  {$ENDIF}
  {$LINKLIB libz.a}
{$ENDIF}

end.

