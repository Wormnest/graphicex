// Copyright (c) 2001-2002 Alessandro Briosi
//
// Permission is hereby granted, free of charge, to any person
// obtaining a copy of this software and associated documentation
// files (the "Software"), to deal in the Software without
// restriction, including without limitation the rights to use, copy,
// modify, merge, publish, distribute, sublicense, and/or sell copies
// of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be
// included in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
// BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
// ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
// CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.
//
//
// This software was written by Alessandro Briosi with the
// assistance of Russell Lang, as an example of how the
// Ghostscript DLL may be used Delphi.
//

// JGB, May 2006
// Changed to dynamic loading of the GhostScript library and detection of
// its path in the registry
// 2007-04-03: fixed initialization bug
// 2008-03-13: fixed changed registry for GPL Ghostscript
// 2012-04-06: Added error codes from gs 8.64 gserrors.h
// 2013-12-03: Add gsapi_set_arg_encoding


{$DEFINE USE_IN_TRANSCRIPT} // 2013-01-09 Loads exception handling unit for Transcript first

unit gsapi;

interface

uses Windows;

// jgb 2012-04-06 Added error codes from gs 8.64 gserrors.h

//* A procedure that may return an error always returns */
//* a non-negative value (zero, unless otherwise noted) for success, */
// or negative for failure. */
// We use ints rather than an enum to avoid a lot of casting. */
const
  gs_error_unknownerror      = -1;	//* unknown error */
  gs_error_interrupt         = -6;
  gs_error_invalidaccess     = -7;
  gs_error_invalidfileaccess = -9;
  gs_error_invalidfont       = -10;
  gs_error_ioerror           = -12;
  gs_error_limitcheck        = -13;
  gs_error_nocurrentpoint    = -14;
  gs_error_rangecheck        = -15;
  gs_error_typecheck         = -20;
  gs_error_undefined         = -21;
  gs_error_undefinedfilename = -22;
  gs_error_undefinedresult   = -23;
  gs_error_VMerror           = -25;
  gs_error_unregistered      = -28;

  gs_error_hit_detected      = -99;

  gs_error_Fatal             = -100;
// end gserrors.h
/////////////////////////////////////////////////////////

// {$HPPEMIT '#include <iminst.h>'}

// 2006-05-21: Changed to var.
// At initialisation time we fill it with the correct path when found.
var
  gsdll32: string = ''; // <> '' means GhostScript found and DLL is loaded
  gsName: string  = ''; // 2008-06-11 Name of installed GhostScript (AFPL or GPL GhostScript)
  gsVer: string   = ''; // 2008-06-11 GhostScript Version

const
  STDIN_BUF_SIZE = 128;
  STDOUT_BUF_SIZE = 128;
  STDERR_BUF_SIZE = 128;

  DISPLAY_VERSION_MAJOR = 1;
  DISPLAY_VERSION_MINOR = 0;

//* Define the color space alternatives */
    DISPLAY_COLORS_NATIVE = $01;
    DISPLAY_COLORS_GRAY   = $02;
    DISPLAY_COLORS_RGB    = $04;
    DISPLAY_COLORS_CMYK   = $08;

    DISPLAY_COLORS_MASK  = $000f;

//* Define whether alpha information, or an extra unused bytes is included */
//* DISPLAY_ALPHA_FIRST and DISPLAY_ALPHA_LAST are not implemented */
    DISPLAY_ALPHA_NONE   = $00;
    DISPLAY_ALPHA_FIRST  = $10;
    DISPLAY_ALPHA_LAST   = $20;
    DISPLAY_UNUSED_FIRST = $40;	 //* e.g. Mac xRGB */
    DISPLAY_UNUSED_LAST  = $80;	//* e.g. Windows BGRx */

    DISPLAY_ALPHA_MASK  = $0070;

// * Define the depth per component for DISPLAY_COLORS_GRAY,
// * DISPLAY_COLORS_RGB and DISPLAY_COLORS_CMYK,
// * or the depth per pixel for DISPLAY_COLORS_NATIVE
// * DISPLAY_DEPTH_2 and DISPLAY_DEPTH_12 have not been tested.
// *
    DISPLAY_DEPTH_1   = $0100;
    DISPLAY_DEPTH_2   = $0200;
    DISPLAY_DEPTH_4   = $0400;
    DISPLAY_DEPTH_8   = $0800;
    DISPLAY_DEPTH_12  = $1000;
    DISPLAY_DEPTH_16  = $2000;
    //* unused (1<<14) */
    //* unused (1<<15) */

    DISPLAY_DEPTH_MASK  = $ff00;


// * Define whether Red/Cyan should come first,
// * or whether Blue/Black should come first
// */
    DISPLAY_BIGENDIAN    = $00000;	//* Red/Cyan first */
    DISPLAY_LITTLEENDIAN = $10000;	//* Blue/Black first */

    DISPLAY_ENDIAN_MASK  = $00010000;

//* Define whether the raster starts at the top or bottom of the bitmap */
    DISPLAY_TOPFIRST    = $00000;	//* Unix, Mac */
    DISPLAY_BOTTOMFIRST = $20000;	//* Windows */

    DISPLAY_FIRSTROW_MASK = $00020000;


//* Define whether packing RGB in 16-bits should use 555
// * or 565 (extra bit for green)
// */
    DISPLAY_NATIVE_555 = $00000;
    DISPLAY_NATIVE_565 = $40000;
    DISPLAY_555_MASK  = $00040000;


    // Values for gsapi_set_arg_encoding
    GS_ARG_ENCODING_LOCAL   = 0;
    GS_ARG_ENCODING_UTF8    = 1;
    GS_ARG_ENCODING_UTF16LE = 2;

type
  TGSAPIrevision = packed record
    product: PChar;
    copyright: PChar;
    revision: longint;
    revisiondate: longint;
  end;
  PGSAPIrevision = ^TGSAPIrevision;

  // I couldn't understand what exactly was in this structure so resolved
  // doing a pointer with space wide enough
  Pgs_main_instance = Pointer;

  TStdioFunction = function(caller_handle:Pointer;buf:PChar;len:integer):integer stdcall;
  TPollFunction = function(caller_handle:Pointer):integer stdcall;

  TDisplayEvent = function(handle:Pointer;device:Pointer):integer; cdecl;
  TDisplayPreResizeEvent = function(handle:Pointer;device:Pointer;
         width:integer;height:integer;raster:integer;format:UINT):integer;cdecl;
  TDisplayResizeEvent = function(handle:Pointer;device:Pointer;
         width:integer;height:integer;raster:integer;format:UINT;pimage:PChar):integer;cdecl;
  TDisplayPageEvent = function(handle:Pointer;device:Pointer;copies:integer;flush:integer):integer;cdecl;
  TDisplayUpdateEvent = function(handle:Pointer;device:Pointer;x:integer;y:integer;w:integer;h:integer):integer;cdecl;
  TDisplayMemAlloc = procedure(handle:Pointer;device:Pointer;size:ulong);cdecl;
  TDisplayMemFree = function(handle:Pointer;device:Pointer;mem:Pointer):integer;cdecl;

  PDisplayEvent = ^TDisplayEvent;
  PDisplayPreResizeEvent = ^TDisplayPreResizeEvent;
  PDisplayResizeEvent = ^TDisplayResizeEvent;
  PDisplayPageEvent = ^TDisplayPageEvent;
  PDisplayUpdateEvent = ^TDisplayUpdateEvent;
  PDisplayMemAlloc = ^TDisplayMemAlloc;
  PDisplayMemFree = ^TDisplayMemFree;

  TDisplayCallback = packed record
    size:integer;
    version_major:integer;
    version_minor:integer;
    // New device has been opened */
    // This is the first event from this device. */
    display_open:TDisplayEvent;
    // Device is about to be closed. */
    // Device will not be closed until this function returns. */
    display_preclose:TDisplayEvent;
    // Device has been closed. */
    // This is the last event from this device. */
    display_close:TDisplayEvent;
    // Device is about to be resized. */
    // Resize will only occur if this function returns 0. */
    // raster is byte count of a row. */
    display_presize:TDisplayPreResizeEvent;
    // Device has been resized. */
    // New pointer to raster returned in pimage */
    display_size:TDisplayResizeEvent;

    // flushpage */
    display_sync:TDisplayEvent;

    // showpage */
    // If you want to pause on showpage, then don't return immediately */
    display_page:TDisplayPageEvent;

    // Notify the caller whenever a portion of the raster is updated. */
    // This can be used for cooperative multitasking or for
    // progressive update of the display.
    // This function pointer may be set to NULL if not required.
    //
    display_update:TDisplayUpdateEvent;

    // Allocate memory for bitmap */
    // This is provided in case you need to create memory in a special
    // way, e.g. shared.  If this is NULL, the Ghostscript memory device
    // allocates the bitmap. This will only called to allocate the
    // image buffer. The first row will be placed at the address
    // returned by display_memalloc.
    //

    display_memalloc:TDisplayMemAlloc;

    // Free memory for bitmap */
    // If this is NULL, the Ghostscript memory device will free the bitmap */
    display_memfree:TDisplayMemFree;

  end;
  PDisplayCallback = ^TDisplayCallback;
  PPChar = array of PChar;

(** STATIC LINKING:
{$EXTERNALSYM gsapi_revision}
function gsapi_revision(pr:PGSAPIrevision; len:integer):integer; stdcall;
{$EXTERNALSYM gsapi_new_instance}
function gsapi_new_instance(pinstance:Pgs_main_instance;caller_handle:Pointer):Integer; stdcall;
{$EXTERNALSYM gsapi_delete_instance}
procedure gsapi_delete_instance(pinstance:Pgs_main_instance); stdcall;
{$EXTERNALSYM gsapi_set_stdio}
function gsapi_set_stdio(pinstance:Pgs_main_instance;
                         stdin_fn:TStdioFunction; stdout_fn:TStdioFunction;
                         stderr_fn:TStdioFunction):Integer; stdcall;
{$EXTERNALSYM gsapi_set_poll}
function gsapi_set_poll(pinstance:Pgs_main_instance;poll_fn:TPollFunction):Integer; stdcall;
{$EXTERNALSYM gsapi_set_display_callback}
function gsapi_set_display_callback(pinstance:Pgs_main_instance;callback:PDisplayCallback):Integer; stdcall;
{$EXTERNALSYM gsapi_init_with_args}
function gsapi_init_with_args(pinstance:Pgs_main_instance;argc:integer;argv:PPChar):integer; stdcall;
{$EXTERNALSYM gsapi_run_string_begin}
function gsapi_run_string_begin(pinstance:Pgs_main_instance;user_errors:integer;pexit_code:Pinteger):integer; stdcall;
{$EXTERNALSYM gsapi_run_string_continue}
function gsapi_run_string_continue(pinstance:Pgs_main_instance;str:PChar;len:integer;user_errors:integer;pexit_code:pinteger):integer; stdcall;
{$EXTERNALSYM gsapi_run_string_end}
function gsapi_run_string_end(pinstance:Pgs_main_instance;user_errors:integer;pexit_code:pinteger):integer; stdcall;
{$EXTERNALSYM gsapi_run_string_with_length}
function gsapi_run_string_with_length(pinstance:Pgs_main_instance;str:PChar;len:integer;user_errors:integer;pexit_code:pinteger):integer; stdcall;
{$EXTERNALSYM gsapi_run_string}
function gsapi_run_string(pinstance:Pgs_main_instance;str:PChar;user_errors:integer;pexit_code:pinteger):integer; stdcall;
{$EXTERNALSYM gsapi_run_file}
function gsapi_run_file(pinstance:Pgs_main_instance;file_name:PChar;user_errors:integer;pexit_code:pinteger):integer; stdcall;
{$EXTERNALSYM gsapi_exit}
function gsapi_exit(pinstance:Pgs_main_instance):integer; stdcall;
**)

// DYNAMIC...

// 1. definieer proc function types
type
  gsapi_revision_func = function(pr:PGSAPIrevision; len:integer):integer; stdcall;
  gsapi_new_instance_func = function(pinstance:Pgs_main_instance;
    caller_handle:Pointer):Integer; stdcall;
  gsapi_delete_instance_func = procedure(pinstance:Pgs_main_instance); stdcall;
  gsapi_set_stdio_func = function(pinstance:Pgs_main_instance;
    stdin_fn:TStdioFunction; stdout_fn:TStdioFunction;
    stderr_fn:TStdioFunction):Integer; stdcall;
  gsapi_set_poll_func = function(pinstance:Pgs_main_instance;poll_fn:TPollFunction):Integer; stdcall;
  gsapi_set_display_callback_func = function(pinstance:Pgs_main_instance;
    callback:PDisplayCallback):Integer; stdcall;
  gsapi_set_arg_encoding_func = function(pinstance:Pgs_main_instance; encoding: Integer): Integer; stdcall;
  gsapi_init_with_args_func = function(pinstance:Pgs_main_instance;
    argc:integer;argv:PPChar):integer; stdcall;
  gsapi_run_string_begin_func = function(pinstance:Pgs_main_instance;
    user_errors:integer;pexit_code:Pinteger):integer; stdcall;
  gsapi_run_string_continue_func = function(pinstance:Pgs_main_instance;
    str:PChar;len:integer;user_errors:integer;pexit_code:pinteger):integer; stdcall;
  gsapi_run_string_end_func = function(pinstance:Pgs_main_instance;
    user_errors:integer;pexit_code:pinteger):integer; stdcall;
  gsapi_run_string_with_length_func = function(pinstance:Pgs_main_instance;
    str:PChar;len:integer;user_errors:integer;pexit_code:pinteger):integer; stdcall;
  gsapi_run_string_func = function(pinstance:Pgs_main_instance;
    str:PChar;user_errors:integer;pexit_code:pinteger):integer; stdcall;
  gsapi_run_file_func = function(pinstance:Pgs_main_instance;file_name:PChar;
    user_errors:integer;pexit_code:pinteger):integer; stdcall;
  gsapi_exit_func = function(pinstance:Pgs_main_instance):integer; stdcall;


// 2. definieer de vars de de proc adressen komen te bevatten
var
  {$EXTERNALSYM gsapi_revision}
  gsapi_revision: gsapi_revision_func = nil;
  {$EXTERNALSYM gsapi_new_instance}
  gsapi_new_instance: gsapi_new_instance_func = nil;
  {$EXTERNALSYM gsapi_delete_instance}
  gsapi_delete_instance: gsapi_delete_instance_func = nil;
  {$EXTERNALSYM gsapi_set_stdio}
  gsapi_set_stdio: gsapi_set_stdio_func = nil;
  {$EXTERNALSYM gsapi_set_poll}
  gsapi_set_poll: gsapi_set_poll_func = nil;
  {$EXTERNALSYM gsapi_set_display_callback}
  gsapi_set_display_callback: gsapi_set_display_callback_func = nil;
  {$EXTERNALSYM gsapi_init_with_args}
  gsapi_init_with_args: gsapi_init_with_args_func = nil;
  {$EXTERNALSYM gsapi_set_arg_encoding}
  gsapi_set_arg_encoding: gsapi_set_arg_encoding_func = nil;
  {$EXTERNALSYM gsapi_run_string_begin}
  gsapi_run_string_begin: gsapi_run_string_begin_func = nil;
  {$EXTERNALSYM gsapi_run_string_continue}
  gsapi_run_string_continue: gsapi_run_string_continue_func = nil;
  {$EXTERNALSYM gsapi_run_string_end}
  gsapi_run_string_end: gsapi_run_string_end_func = nil;
  {$EXTERNALSYM gsapi_run_string_with_length}
  gsapi_run_string_with_length: gsapi_run_string_with_length_func = nil;
  {$EXTERNALSYM gsapi_run_string}
  gsapi_run_string: gsapi_run_string_func = nil;
  {$EXTERNALSYM gsapi_run_file}
  gsapi_run_file: gsapi_run_file_func = nil;
  {$EXTERNALSYM gsapi_exit}
  gsapi_exit: gsapi_exit_func = nil;

function GhostScriptLoaded: Boolean;
procedure LoadGhostScript;
procedure UnloadGhostScript;


implementation

uses SysUtils, Classes {TStringList},
{$IFDEF USE_IN_TRANSCRIPT}
{$IFDEF DEBUG_LOG}
     JgbDebugHelper,  // debug log helper 2007-04-02
{$ELSE}
     {$IFNDEF FPC}
     TransExceptionDlg, // 2007-04-27 To make sure that the exception interception
                        // code will be loaded BEFORE this unit so that we can
                        // show any problems encountered here.
     {$ENDIF}
{$ENDIF DEBUG_LOG}
{$ENDIF USE_IN_TRANSCRIPT}
     JclRegistry;


// JGB: Because we don't know in advance where the GhosScript dll resides
// we cannot load it statically. It has to be loaded dynamically.
// How to do that was copied from the jedi apilib units/functions.
(**
{$EXTERNALSYM gsapi_revision}
function gsapi_revision; stdcall; external gsdll32 name 'gsapi_revision';
{$EXTERNALSYM gsapi_new_instance}
function gsapi_new_instance; stdcall; external gsdll32 name 'gsapi_new_instance';
{$EXTERNALSYM gsapi_new_instance}
procedure gsapi_delete_instance; stdcall; external gsdll32 name 'gsapi_delete_instance';
{$EXTERNALSYM gsapi_set_stdio}
function gsapi_set_stdio; stdcall; external gsdll32 name 'gsapi_set_stdio';
{$EXTERNALSYM gsapi_set_poll}
function gsapi_set_poll; stdcall; external gsdll32 name 'gsapi_set_poll';
{$EXTERNALSYM gsapi_set_display_callback}
function gsapi_set_display_callback; stdcall; external gsdll32 name 'gsapi_set_display_callback';
{$EXTERNALSYM gsapi_init_with_args}
function gsapi_init_with_args; stdcall; external gsdll32 name 'gsapi_init_with_args';
{$EXTERNALSYM gsapi_run_string_begin}
function gsapi_run_string_begin; stdcall; external gsdll32 name 'gsapi_run_string_begin';
{$EXTERNALSYM gsapi_run_string_continue}
function gsapi_run_string_continue; stdcall; external gsdll32 name 'gsapi_run_string_continue';
{$EXTERNALSYM gsapi_run_string_end}
function gsapi_run_string_end; stdcall; external gsdll32 name 'gsapi_run_string_end';
{$EXTERNALSYM gsapi_run_string_with_length}
function gsapi_run_string_with_length; stdcall; external gsdll32 name 'gsapi_run_string_with_length';
{$EXTERNALSYM gsapi_run_string}
function gsapi_run_string; stdcall; external gsdll32 name 'gsapi_run_string';
{$EXTERNALSYM gsapi_run_file}
function gsapi_run_file; stdcall; external gsdll32 name 'gsapi_run_file';
{$EXTERNALSYM gsapi_exit}
function gsapi_exit; stdcall; external gsdll32 name 'gsapi_exit';
**)

// En nu dynamisch

const
  INVALID_MODULEHANDLE_VALUE = 0; //TModuleHandle(0);

var
  ModuleHandle: HMODULE = 0;

procedure LoadGsapi;
  function GetSymbol(SymbolName: PChar): Pointer;
  begin
    Result := GetProcAddress(ModuleHandle, PChar(SymbolName));
  end;
begin
  if gsdll32 <> '' then
  begin
    ModuleHandle := LoadLibrary(PChar(gsdll32));
    if ModuleHandle <> INVALID_MODULEHANDLE_VALUE then // OK: Load gs api
    begin
      @gsapi_revision := GetSymbol('gsapi_revision');
      @gsapi_new_instance := GetSymbol('gsapi_new_instance');
      @gsapi_delete_instance := GetSymbol('gsapi_delete_instance');
      @gsapi_set_stdio := GetSymbol('gsapi_set_stdio');
      @gsapi_set_poll := GetSymbol('gsapi_set_poll');
      @gsapi_set_display_callback := GetSymbol('gsapi_set_display_callback');
      @gsapi_init_with_args := GetSymbol('gsapi_init_with_args');
      @gsapi_set_arg_encoding := GetSymbol('gsapi_set_arg_encoding');
      @gsapi_run_string_begin := GetSymbol('gsapi_run_string_begin');
      @gsapi_run_string_continue := GetSymbol('gsapi_run_string_continue');
      @gsapi_run_string_end := GetSymbol('gsapi_run_string_end');
      @gsapi_run_string_with_length := GetSymbol('gsapi_run_string_with_length');
      @gsapi_run_string := GetSymbol('gsapi_run_string');
      @gsapi_run_file := GetSymbol('gsapi_run_file');
      @gsapi_exit := GetSymbol('gsapi_exit');
      //FreeLibrary(gsdll32);
    end
  end;
end;

const
  //HKLM = DelphiHKEY(HKEY_LOCAL_MACHINE);  zie JclRegistry
  GhostScriptRegKey_AFPL = 'SOFTWARE\AFPL Ghostscript';
  GhostScriptRegKey_GPL  = 'SOFTWARE\GPL Ghostscript';
  GhostScriptRegKey_GPL_64  = 'SOFTWARE\Wow6432Node\GPL Ghostscript';

// Only tries to find it in registry
// Does NOT load the GhostScript library!
procedure TryToFindGhostScript;
var //SubKeys: TStringList;
    //VersionKey: string;
    GhostRegKey: string;
    GhostRoot: Cardinal;
    //i: Integer;
    //GhostExists: Boolean;
    function GhostScriptDllPresent: Boolean; // 9-6-2008
    var SubKeys: TStringList;
        VersionKey: string;
        i: Integer;
    begin
      // GhostScript exists, try to find the version
      SubKeys := TStringList.Create;
      try
  //      if RegGetKeyNames(HKLM,GhostScriptRegKey,SubKeys) and (SubKeys.Count >= 1) then
        if RegGetKeyNames(GhostRoot,GhostRegKey,SubKeys) and (SubKeys.Count >= 1) then
        begin
          // N.B.: UNINSTALL of older GhostScript versions (no idea if more recent
          // versions do the same) an empty registry key 8.xx [xx=number] stayed behind.
          // Which caused problems with our old implementation
          // Therefore we changed it to searching for the newest key first and
          // also checking if the value exists before we try to read it.

          // 2007-04-03
          // We begin with the last key (which is in most cases the latest version of GhostScript)
          i := SubKeys.Count;
          while i > 0 do
          begin
            VersionKey := SubKeys[i-1];
            // read string: if not exists returns ''
  //          gsdll32 := RegReadStringDef(HKLM,GhostScriptRegKey+'\'+VersionKey,'GS_DLL','');
            gsdll32 := RegReadStringDef(GhostRoot,GhostRegKey+'\'+VersionKey,'GS_DLL','');
            gsVer := VersionKey;
            // Remove SOFTWARE/ from the key to get the name:
            gsName := Copy(GhostRegKey,10,Length(GhostRegKey)-9);
            // 2007-04-07: Extra security check: looks to see if dll really exists
            // at that location!
            if gsdll32 <> '' then
            begin
              if not FileExists(gsdll32) then
                gsdll32 := ''; // Cannot find file: set it back to empty string
              break; // GhostScript dll found!
            end;
            Dec(i);
          end;
        end;
      finally
        SubKeys.Free;
      end;
      Result := gsdll32 <> '';
    end;
    function GhostScriptRegKeyExists(const RootKey: Cardinal; const RegKey: string): Boolean;
    begin
        if RegKeyExists(RootKey,RegKey) then
        begin
          GhostRegKey := RegKey;
          GhostRoot := RootKey;
          //Result := True;
          Result := GhostScriptDllPresent;
        end
        else
          Result := False;
    end;
begin
  gsdll32 := '';
  // 2008-06-09: We always need to enumerate over all keys because
  // it's possible that the GhostScript key exists in HCKU but without GS_DLL
  // while that value may be found under HKLM.
  if
     GhostScriptRegKeyExists(HKCU,GhostScriptRegKey_GPL_64) or
     GhostScriptRegKeyExists(HKLM,GhostScriptRegKey_GPL_64) or
     GhostScriptRegKeyExists(HKCU,GhostScriptRegKey_GPL) or
     GhostScriptRegKeyExists(HKLM,GhostScriptRegKey_GPL) or
     GhostScriptRegKeyExists(HKLM,GhostScriptRegKey_AFPL) then
  begin
    // GhostScript found, nothing to do here at the moment (9-6-2008)
  end;
end;

function GhostScriptLoaded: Boolean;
begin
  Result := ModuleHandle <> INVALID_MODULEHANDLE_VALUE;
end;

procedure LoadGhostScript;
//var SubKeys: TStringList;
//    VersionKey: string;
begin
  if ModuleHandle = INVALID_MODULEHANDLE_VALUE then
    LoadGsapi;
end;

procedure UnloadGhostScript;
begin
  if ModuleHandle <> INVALID_MODULEHANDLE_VALUE then
  begin
    FreeLibrary(ModuleHandle);
    ModuleHandle := INVALID_MODULEHANDLE_VALUE;
    gsdll32 := '';
    @gsapi_revision := nil;
    @gsapi_new_instance := nil;
    @gsapi_delete_instance := nil;
    @gsapi_set_stdio := nil;
    @gsapi_set_poll := nil;
    @gsapi_set_display_callback := nil;
    @gsapi_init_with_args := nil;
    @gsapi_set_arg_encoding := nil;
    @gsapi_run_string_begin := nil;
    @gsapi_run_string_continue := nil;
    @gsapi_run_string_end := nil;
    @gsapi_run_string_with_length := nil;
    @gsapi_run_string := nil;
    @gsapi_run_file := nil;
    @gsapi_exit := nil;
  end;
end;

initialization
{$IFDEF DEBUG_LOG}
  DebugHelper.LogAdd('Init GhostScript');
{$ENDIF DEBUG_LOG}
  TryToFindGhostScript; // Only tries to find it in registry
                        // Does NOT load the GhostScript library!
{$IFDEF DEBUG_LOG}
  DebugHelper.LogAdd('Init GhostScript finished.');
{$ENDIF DEBUG_LOG}
finalization
  UnloadGhostScript;  // for safety, if we forget it in our program
end.

