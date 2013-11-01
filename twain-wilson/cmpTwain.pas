unit cmpTwain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  unitCallbackComponent, Twain_Wilson, SyncObjs;

{.$DEFINE DEBUG}

type
  TOnImage = procedure (sender : TObject; bitmap : TBitmap; var release : boolean) of object;

// JGB: I would like to have another Event here:
//      OnScanningDone - possibly with a var that signifies whether something
//      was scanned or not
  TOnScanningDone = procedure (sender : TObject; ImageScanned: Boolean) of object;

  TTransferFormat = (tfBmp, tfJpg, tfTiff);
  TcwTwain = class(TCallbackComponent)
  private
    fActive: boolean;
    fOpen : boolean;
    fEnabled : boolean;
    fAppID : TW_IDENTITY;
    fSourceID : TW_IDENTITY;
    fOnImage: TOnImage;
    fTransferFormat: TTransferFormat;
    fWindowList : pointer;
    fActiveWindow : HWND;

    //jgb:
    FOnScanningDone: TOnScanningDone;
    FImageAvailable: Boolean;

    procedure SetActive(const Value: boolean);
    procedure SetOpen (const Value : boolean);
    procedure SetCapability (capability, value : Integer);
    // Silence a warning about HasCapability never being used.
    //function HasCapability (capability, value : Integer) : boolean;
    procedure SetTransferCount (value : Integer);

    procedure TransferImage;
    procedure SetTransferFormat(const Value: TTransferFormat);
    function TwainCheck (code : Integer) : Integer;
  protected
    procedure WndProc (var Msg : TMessage); override;
  public
    fPict : TPicture;  // jgb here instead of private as a test
    destructor Destroy; override;
    function SelectSource : boolean;
    procedure GetSingleImage (parent : TWinControl; pict : TPicture);

    { Public declarations }
  published
    property TransferFormat : TTransferFormat read fTransferFormat write SetTransferFormat;
    property OnImage : TOnImage read fOnImage write fOnImage;
    property OnScanningDone: TOnScanningDone read FOnScanningDone write FOnScanningDone;
  end;

  ETwain = class (Exception)
    fCode : Integer;
    fStatus : Integer;
    constructor Create (ACode, AStatus : Integer);
  end;

implementation

{ TcwTwain }

destructor TcwTwain.Destroy;
begin
  SetOpen (False);
  SetActive (False);
  FinalizeTwain; // jgb
  inherited;
end;

procedure TcwTwain.GetSingleImage (parent : TWinControl; pict : TPicture);
var
  twUserInterface : TW_USERINTERFACE;
begin
  InitializeTwain; // jgb
  FImageAvailable := False;
  fPict := pict;
  SetOpen (True);
  SetTransferCount (1);

(*
  if TransferFormat = tfJPg then
  begin
    SetCapability (ICAP_IMAGEFILEFORMAT, TWFF_JFIF)
  end;
*)
  twUserInterface.ShowUI := True;
  twUserInterface.hParent := parent.Handle;
  twUserInterface.ModalUI := True;

  fActiveWindow := GetActiveWindow;
  fWindowList := DisableTaskWindows (0);
  try
    TwainCheck (DSM_Entry (@fAppID, @fSourceID, DG_CONTROL, DAT_USERINTERFACE, MSG_ENABLEDS, @twUserInterface));
    fEnabled := True
  except
    EnableTaskWindows (fWindowList);
    SetActiveWindow (fActiveWindow)
  end
end;

// Silence a warning about HasCapability never being used.
(*
function TcwTwain.HasCapability(capability, value: Integer): boolean;
var
  twCapability : TW_CAPABILITY;
  pValEnum : pTW_ENUMERATION;
  pValOneValue : pTW_ONEVALUE;
  i : Integer;
begin
  result := False;
  twCapability.Cap := capability;
  twCapability.ConType := TWON_DONTCARE16;
  twCapability.hContainer := 0;

  TwainCheck (DSM_Entry (@fAppId, @fSourceID, DG_CONTROL, DAT_CAPABILITY, MSG_GET, @twCapability));

  case twCapability.ConType of
    TWON_ENUMERATION :
      begin
        pValEnum := GlobalLock (twCapability.hContainer);
        try

          for i := 0 to pValEnum^.NumItems - 1 do
          begin
            if pvalEnum^.ItemType = TWTY_UINT16 then
              if pValEnum^.ItemList [i * 2] = value then
              begin
                result := True;
                break
              end
          end
        finally
          GlobalUnlock (twCapability.hContainer)
        end
      end;

    TWON_ONEVALUE:
      begin
        pValOneValue := GlobalLock (twCapability.hContainer);
        try
          if pValOneValue^.Item = Cardinal (value) then
            result := True
        finally
          GlobalUnlock (twCapability.hContainer)
        end
      end
  end
end;
*)

function TcwTwain.SelectSource: boolean;
begin
  InitializeTwain; // jgb
  FImageAvailable := False;
  SetActive (True);
  fActiveWindow := GetActiveWindow;
  fWindowList := DisableTaskWindows (0);
  try
    result := TwainCheck (DSM_Entry (@fAppId, Nil, DG_CONTROL, DAT_IDENTITY, MSG_USERSELECT, @fSourceId)) = TWRC_SUCCESS;
  finally
    EnableTaskWindows (fWindowList);
    SetActiveWindow (fActiveWindow)
  end
end;

procedure TcwTwain.SetActive(const Value: boolean);
var
  h : HWND;
begin
  if fActive <> Value then
  case fActive of
    False :
      begin
        FillChar (fSourceId, SizeOf (fSourceID), 0);
        h := WindowHandle;
        fAppId.Version.MajorNum := 1;
        fAppId.Version.MinorNum := 0;
        fAppId.Version.Language := TWLG_ENGLISH_UK;
        fAppId.Version.Country := TWCY_UNITEDKINGDOM;
        lstrcpy (fAppId.Version.Info, 'Image View');
        fAppID.ProtocolMajor := TWON_PROTOCOLMAJOR;
        fAppID.ProtocolMinor := TWON_PROTOCOLMINOR;
        fAppID.SupportedGroups := DG_IMAGE or DG_CONTROL;
        lstrcpy (fAppID.Manufacturer, 'Colin Wilson');
        lstrcpy (fAppID.ProductFamily, '');
        lstrcpy (fAppID.ProductName, 'Image Viewer');

        TwainCheck (DSM_Entry (@fAppId, Nil, DG_CONTROL, DAT_PARENT, MSG_OPENDSM, @h));
        fActive := True
      end;

    True :
      begin
        h := WindowHandle;
        TwainCheck (DSM_Entry (@fAppId, Nil, DG_CONTROL, DAT_PARENT, MSG_CLOSEDSM, @h));
        fActive := False
      end
  end
end;

procedure TcwTwain.SetCapability(capability, value: Integer);
var
  twCapability : TW_CAPABILITY;
  pVal : pTW_ONEVALUE;
begin
  twCapability.Cap := capability;
  twCapability.ConType := TWON_ONEVALUE;
  twCapability.hContainer := GlobalAlloc (GHND, SizeOf (TW_ONEVALUE));
  try
    pVal := pTW_ONEVALUE (GlobalLock (twCapability.hContainer));
    try
      pVal^.ItemType := TWTY_INT16;
      pVal^.Item := value;
    finally
      GlobalUnlock (twCapability.hContainer)
    end;

    TwainCheck (DSM_Entry (@fAppId, @fSourceID, DG_CONTROL, DAT_CAPABILITY, MSG_SET, @twCapability));

  finally
    GlobalFree (twCapability.hContainer)
  end
end;

procedure TcwTwain.SetOpen(const Value: boolean);
begin
  if fOpen <> value then
  case fOpen of
    False :
      begin
        SetActive (True);
        TwainCheck (DSM_Entry (@fAppId, Nil, DG_CONTROL, DAT_IDENTITY, MSG_OPENDS, @fSourceId));
        fOpen := True;
      end;
    True :
      begin
        TwainCheck (DSM_Entry (@fAppId, Nil, DG_CONTROL, DAT_IDENTITY, MSG_CLOSEDS, @fSourceId));
        fEnabled := False;
        fOpen := False
      end
  end
end;

procedure TcwTwain.SetTransferCount(value: Integer);
begin
  SetCapability (CAP_XFERCOUNT, value);
end;

procedure TcwTwain.SetTransferFormat(const Value: TTransferFormat);
begin
  fTransferFormat := Value;
end;

// 2013-01-09 Delphi incorrectly thinks that twMemXFer, twSetupMemXFer and twImageInfo
// are not used and issues Hints to that end. Therefore we turn off hints for
// this function only.
{$HINTS OFF}
procedure TcwTwain.TransferImage;
var
  h : THandle;
  twPendingXfers : TW_PENDINGXFERS;
  twMemXFer : TW_IMAGEMEMXFER;        // NB: This is used below in TwainCheck even if Delphi thinks it isn't!
  twSetupMemXFer : TW_SETUPMEMXFER;   // NB: This is used below in TwainCheck even if Delphi thinks it isn't!
  twImageInfo : TW_IMAGEINFO;         // NB: This is used below in TwainCheck even if Delphi thinks it isn't!
  TwainCode: Integer;

  function CreateTBitmapFromTwainHandle (h : THandle) : TBitmap;
  var
    mem : PBitmapInfo;
    sz : Integer;
    bmp : TBitmap;
    mems : TMemoryStream;
    Bmf: TBitmapFileHeader;
  begin
    sz := GlobalSize (h);
    mem := GlobalLock (h);
    bmp := Nil;
    mems := Nil;
    try
      mems := TMemoryStream.Create;
      bmp := TBitmap.Create;
      try
        FillChar (bmf, sizeof (bmf), 0);
        bmf.bfType := $4d42;

        mems.Write(bmf, sizeof (bmf));
        mems.Write(mem^, sz);

        mems.Seek(0, soFromBeginning);

        bmp.LoadFromStream(mems);
{$IFDEF DEBUG}
        {if bmp.Width = 0 then
          MessageBeep(MB_IconInformation);}
{$ENDIF}
      except
        FreeAndNil (bmp)
      end;
      Result := bmp;
      FImageAvailable := True;

    finally
      GlobalUnlock (h);
      mems.Free;
    end
  end;

begin
  FImageAvailable := False;
  if False then // HasCapability (ICAP_XFERMECH, TWSX_MEMORY) then
  begin
    SetCapability (ICAP_XFERMECH, TWSX_MEMORY);

    TwainCheck (DSM_Entry (@fAppId, @fSourceID, DG_CONTROL, DAT_SETUPMEMXFER, MSG_GET, @twSetupMemXFer));
    TwainCheck (DSM_Entry (@fAppID, @fSourceID, DG_IMAGE, DAT_IMAGEINFO, MSG_GET, @twImageInfo));

    FillChar (twMemXFer, SizeOf (twMemXFer), 0);
    twMemXFer.Compression := TWON_DONTCARE16;
    twMemXFer.BytesPerRow := TWON_DONTCARE32;
    twMemXFer.Columnms := TWON_DONTCARE32;
    twMemXFer.Rows := TWON_DONTCARE32;
    twMemXFer.XOffset := TWON_DONTCARE32;
    twMemXFer.YOffset := TWON_DONTCARE32;
    twMemXFer.BytesWritten := TWON_DONTCARE32;
    twMemXFer.Memory.Flags := TWMF_APPOWNS or TWMF_POINTER;
    twMemXFer.Memory.Length := twSetupMemXFer.MinBufSize;
    GetMem (twMemXFer.Memory.TheMem, twSetupMemXFer.Preferred);

    TwainCheck (DSM_Entry (@fAppId, @fSourceId, DG_IMAGE, DAT_IMAGEMEMXFER, MSG_GET, @twMemXFer));

  end
  else
  begin
    TwainCode := TwainCheck (DSM_Entry (@fAppId, @fSourceId, DG_IMAGE, DAT_IMAGENATIVEXFER, MSG_GET, @h));
    // We need to check here if everything went fine.
    // If an image was scanned we will get a TWRC_XFERDONE here.
    // If the user canceled scanning we get TWRC_CANCEL.
    if TwainCode <> TWRC_XFERDONE then
      Exit;
    try
      fPict.Graphic := CreateTBitmapFromTwainHandle (h); //ORIGINAL
{$IFDEF DEBUG}
      {if fPict.Width = 0 then
        MessageBeep(MB_IconInformation);}
{$ENDIF}
    finally
      GlobalFree (h)
    end
  end;

  TwainCheck (DSM_Entry (@fAppId, @fSourceId, DG_CONTROL, DAT_PENDINGXFERS, MSG_ENDXFER, @twPendingXfers));

//    OnImage (self, bmp, release);

end;
{$HINTS ON}

function TcwTwain.TwainCheck(code: Integer): Integer;
var
  status : TW_STATUS;
begin
  if (code <> TWRC_SUCCESS) and (code <> TWRC_CANCEL) and (code <> TWRC_XFERDONE) then
  begin
    if (code = TWRC_FAILURE) or (code = TWRC_CHECKSTATUS) then
    begin
      TwainCheck (DSM_ENTRY (@fAppId, @fSourceID, DG_CONTROL, DAT_STATUS, MSG_GET, @status));
      raise ETwain.Create (code, status.ConditionCode)
    end
    else
      raise ETwain.Create (code, 0);
  end;

  result := code
end;

procedure TcwTwain.WndProc(var Msg: TMessage);
var
  rc : Integer;
  twEvent : TW_EVENT;
  twUserInterface : TW_USERINTERFACE;
  m : TMsg;

begin
  try
    rc := TWRC_NOTDSEVENT;
    if fEnabled then
    begin
      m.hwnd := WindowHandle;
      m.message := Msg.Msg;
      m.wParam := msg.WParam;
      m.lParam := msg.LParam;
      twEvent.pEvent := @m;
      twEvent.TWMessage := MSG_NULL;
      rc := DSM_Entry (@fAppId, @fSourceId, DG_CONTROL, DAT_EVENT, MSG_PROCESSEVENT, @twEvent);
      case twEvent.TWMessage of
        MSG_XFERREADY :
          begin
            TransferImage;
{JGB added: (see also ...\Overige\Twain2\Twain.pas) }
            FillChar (twUserInterface, SizeOf (twUserInterface), 0);
            TwainCheck (DSM_Entry (@fAppId, @fSourceId, DG_CONTROL, DAT_USERINTERFACE, MSG_DISABLEDS, @twUserInterface));
            EnableTaskWindows (fWindowList);
            SetActiveWindow (fActiveWindow);
            SetOpen (False);
            if Assigned(FOnScanningDone) then
              FOnScanningDone(Self,FImageAvailable);
{end jgb added}
          end;

        MSG_CLOSEDSREQ:
{JGB:
        MSG_CLOSEDSOK :
//end jgb}
          begin
            FillChar (twUserInterface, SizeOf (twUserInterface), 0);
            TwainCheck (DSM_Entry (@fAppId, @fSourceId, DG_CONTROL, DAT_USERINTERFACE, MSG_DISABLEDS, @twUserInterface));
            EnableTaskWindows (fWindowList);
            SetActiveWindow (fActiveWindow);
            SetOpen (False)
          end;

{JGB}
        MSG_CLOSEDSOK :
          begin
            SetOpen (False);
            // 18-3-2012 why is testing only code still present here????????????????
            // 26-3-2012 turned off beep
            //MessageBeep (MB_ICONEXCLAMATION);  // for testing only!
          end;
{}
{$IFDEF DEBUG}
        MSG_NULL :
          MessageBeep (MB_ICONEXCLAMATION)
{$ENDIF}
      end
    end;

    if rc = TWRC_NOTDSEVENT then
      Dispatch (Msg)
  except
    Application.HandleException (self)
  end
end;

{ ETwain }

constructor ETwain.Create(ACode, AStatus: Integer);
begin
  fCode := ACode;
  fStatus := AStatus;

  if AStatus <> 0 then
    inherited CreateFmt ('Twain error failure %d status %d', [ACode, ASTatus])
  else
    inherited CreateFmt ('Twain error %d', [ACode]);
end;

end.
