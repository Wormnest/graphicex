(*======================================================================*
 | unitCallbackComponent                                                |
 |                                                                      |
 | The contents of this file are subject to the Mozilla Public License  |
 | Version 1.1 (the "License"); you may not use this file except in     |
 | compliance with the License. You may obtain a copy of the License    |
 | at http://www.mozilla.org/MPL/                                       |
 |                                                                      |
 | Software distributed under the License is distributed on an "AS IS"  |
 | basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See  |
 | the License for the specific language governing rights and           |
 | limitations under the License.                                       |
 |                                                                      |
 | Copyright © Colin Wilson 2002  All Rights Reserved                   |
 |                                                                      |
 | Version  Date        By    Description                               |
 | -------  ----------  ----  ------------------------------------------|
 | 1.0      29/08/2002  CPWW  Original                                  |
 *======================================================================*)

unit unitCallbackComponent;

interface

uses
  Windows, Messages, SysUtils, Classes{$IFDEF FPC} , LclIntf {$ENDIF};

type
  TCallbackComponent = class(TComponent)
  private
    fWindowHandle : HWnd;

  protected
    procedure WndProc (var Msg : TMessage); virtual;

  public
    constructor Create (AOwner : TComponent); override;
    destructor Destroy; override;
    procedure DefaultHandler (var Msg); override;
    property WindowHandle : HWnd read fWindowHandle;
  published
  end;

implementation

constructor TCallbackComponent.Create (AOwner : TComponent);
begin
  inherited create (AOwner);
  fWindowHandle := {$IFDEF FPC} LclIntf. {$ENDIF}AllocateHWnd (WndProc);
end;

destructor TCallbackComponent.Destroy;
begin
  {$IFDEF FPC} LclIntf. {$ENDIF}DeallocateHWnd (fWindowHandle);
  inherited
end;

procedure TCallbackComponent.WndProc (var Msg : TMessage);
begin
  try
    Dispatch (Msg);
  except
    SysUtils.ShowException(ExceptObject, ExceptAddr);
  end
end;

procedure TCallbackComponent.DefaultHandler (var Msg);
begin
  if FWindowHandle <> 0 then
    with TMessage (Msg) do result := DefWindowProc (FWindowHandle, Msg, wParam, lParam)
  else
    inherited DefaultHandler (Msg);
end;

end.
