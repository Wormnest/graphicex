{******************************************************************************}
{                                  TRERuler                                    }
{                                  --------                                    }
{                                                                              }
{                       Copyright © 2004 Pieter Zijlstra                       }
{                                                                              }
{ TRERuler is a descendant of TRuler specialized for the standard TRichEdit.   }
{                                                                              }
{ E-mail: p.zylstra@hccnet.nl                                                  }
{ Website: http://home.hccnet.nl/p.zylstra/                                    }
{==============================================================================}
{                           This software is FREEWARE                          }
{                           -------------------------                          }
{                                                                              }
{ You may freely use it in any software, including commercial software, but    }
{ be-aware that the code is provided as-is, with no implied warranty.          }
{==============================================================================}
{                                                                              }
{ Version history:                                                             }
{ 1.0.0.0 22 feb 2004 - First release.                                         }
{ 1.6.1.1 21 may 2004 - Changed the Parents of the fake margins to the same    }
{                       parent of the RichEdit (it used to be the TForm).      }
{******************************************************************************}
unit RERuler;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls, Forms,
  ComCtrls, ExtCtrls, Ruler;

type
  TRERuler = class(TRuler)
  private
    FControlCanvas: TControlCanvas;
    FLineStyle: TPenStyle;
    FOrgSelectionChange: TNotifyEvent;
    FPanelLM: TPanel;
    FPanelRM: TPanel;
    FRichEdit: TRichEdit;
    FSaveX: Integer;
    FUpdating: Boolean;
    procedure AssignRichEditEvents;
    procedure DrawMarkLine;
    procedure RestoreRichEditEvents;
    procedure RichEditSelectionChange(Sender: TObject);
    procedure SetRichEdit(const Value: TRichEdit);
  protected
    procedure DoRulerItemMove(X: Integer; Removing: Boolean); override;
    procedure DoRulerItemRelease; override;
    procedure DoRulerItemSelect(X: Integer); override;
    procedure DoIndentChanged; override;
    procedure DoMarginChanged; override;
    procedure DoTabChanged; override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetEditRect; virtual;
    procedure UpdateRulerIndents;
    procedure UpdateRulerMargins;
  published
    property LineStyle: TPenStyle read FLineStyle write FLineStyle default psDot;
    property RichEdit: TRichEdit read FRichEdit write SetRichEdit;
  end;

implementation

{ TRERuler }

procedure TRERuler.AssignRichEditEvents;
begin
  if not Assigned(FRichEdit) or (csDesigning in ComponentState) then
    Exit;
  FOrgSelectionChange := TRichEdit(FRichEdit).OnSelectionChange;
  TRichEdit(FRichEdit).OnSelectionChange := RichEditSelectionChange;
end;

constructor TRERuler.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLineStyle := psDot;
end;

procedure TRERuler.DoIndentChanged;
var
  M: Extended;
begin
  inherited;
  if not Assigned(RichEdit) or FUpdating then
    Exit;
  M := MultiPoints;
  RichEdit.Paragraph.FirstIndent := Round(M * FirstIndent);
  RichEdit.Paragraph.LeftIndent  := Round(M * LeftIndent);
  RichEdit.Paragraph.RightIndent := Round(M * RightIndent);
  UpdateRulerIndents;
end;

procedure TRERuler.DoMarginChanged;
begin
  inherited;
  SetEditRect;
  UpdateRulerMargins;
end;

procedure TRERuler.DoRulerItemMove(X: Integer; Removing: Boolean);
begin
  inherited;
  if not Assigned(RichEdit) then
    Exit;
  DrawMarkLine;
  if Removing then
    FSaveX := -1
  else
    FSaveX := X - Inset;
  DrawMarkLine;
end;

procedure TRERuler.DoRulerItemRelease;
begin
  inherited;
  if not Assigned(RichEdit) then
    Exit;
  DrawMarkLine;
  FControlCanvas.Free;
  FControlCanvas := nil;
  FRichEdit.Refresh;
end;

procedure TRERuler.DoRulerItemSelect(X: Integer);
begin
  inherited;
  if not Assigned(RichEdit) then
    Exit;
  RichEdit.Update;
  if not Assigned(FControlCanvas) then
    FControlCanvas := TControlCanvas.Create;
  with FControlCanvas do
  begin
    Control := RichEdit;
    Pen.Color := clBlack;
    Pen.Mode := pmXor;
    Pen.Style := LineStyle;
  end;
  FSaveX := X - Inset;
  DrawMarkLine;
end;

procedure TRERuler.DoTabChanged;
var
  I: Integer;
begin
  inherited;
  if not Assigned(RichEdit) or FUpdating then
    Exit;
  RichEdit.Paragraph.TabCount := Tabs.Count + 1; // Workaroud for TRichEdit Tabs bug.
  for I := 0 to Tabs.Count - 1 do
    RichEdit.Paragraph.Tab[I] := Round(Tabs[I].Points);
  RichEdit.Paragraph.TabCount := Tabs.Count;
end;

procedure TRERuler.DrawMarkLine;
begin
  if Assigned(FControlCanvas) then
    with FControlCanvas do
      if FSaveX >= 0 then begin
        MoveTo(FSaveX{-FRichEdit.HScrollPos}, 0);
        LineTo(FSaveX{-FRichEdit.HScrollPos}, RichEdit.Height);
      end;
end;

procedure TRERuler.Loaded;
begin
  inherited;
  AssignRichEditEvents;
end;

procedure TRERuler.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FRichEdit) then
  begin
    RestoreRichEditEvents;
    FRichEdit := nil;
  end;
end;

procedure TRERuler.RestoreRichEditEvents;
begin
  if not Assigned(FRichEdit) or (csDesigning in ComponentState) then
    Exit;
  TRichEdit(FRichEdit).OnSelectionChange := FOrgSelectionChange;
  FOrgSelectionChange := nil;
end;

procedure TRERuler.RichEditSelectionChange(Sender: TObject);
begin
  if Assigned(FOrgSelectionChange) then
    FOrgSelectionChange(Sender);
  UpdateRulerIndents;
end;

procedure TRERuler.SetEditRect;
var
  R: TRect;
begin
  if not Assigned(FRichEdit) or (csDesigning in ComponentState) then
    Exit;
  R := Rect(Round(UnitsToPixs(LeftMargin)), 0,
            Round(UnitsToPixs(PageWidth - RightMargin)), RichEdit.ClientHeight);
  // Correction for W2K and XP, dont' know if this needed for NT.
  if Win32Platform = VER_PLATFORM_WIN32_NT then
    InflateRect(R, -1, 0);
  SendMessage(RichEdit.Handle, EM_SETRECT, 0, LongInt(@R));
  if Win32Platform = VER_PLATFORM_WIN32_NT then
    InflateRect(R, +1, 0);

  // Draw some fake margin lines by using small panels.
  FPanelLM.Left := RichEdit.Left + 1 + R.Left;
  FPanelRM.Left := RichEdit.Left + 3 + R.Right;
end;

procedure TRERuler.SetRichEdit(const Value: TRichEdit);

  procedure SetPanelDefaults(Panel: TPanel);
  begin
    Panel.Parent := RichEdit.Parent;
    Panel.BevelInner := bvNone;
    Panel.BevelOuter := bvNone;
    Panel.Top := RichEdit.Top + 2;
    Panel.Height := RichEdit.Height - 4;
    Panel.Width := 1;
  end;

begin
  RestoreRichEditEvents;
  FRichEdit := Value;
  if not (csLoading in ComponentState) then
    AssignRichEditEvents;

  if not Assigned(FRichEdit) or (csDesigning in ComponentState) then
    Exit;

  FPanelLM := TPanel.Create(FRichEdit);
  SetPanelDefaults(FPanelLM);

  FPanelRM := TPanel.Create(FRichEdit);
  SetPanelDefaults(FPanelRM);
end;

procedure TRERuler.UpdateRulerIndents;
var
  I: Integer;
  M: Extended;
begin
  if not Assigned(FRichEdit) or (csDesigning in ComponentState) then
    Exit;
  FUpdating := True;
  try
    M := 1 / MultiPoints;
    FirstIndent := M * RichEdit.Paragraph.FirstIndent;
    LeftIndent  := M * RichEdit.Paragraph.LeftIndent;
    RightIndent := M * RichEdit.Paragraph.RightIndent;

    // Update tabs
    Tabs.Clear;
    for I := 0 to RichEdit.Paragraph.TabCount - 1 do
      Tabs.Add.Points := RichEdit.Paragraph.Tab[I];
  finally
    FUpdating := False;
  end;
end;

procedure TRERuler.UpdateRulerMargins;
begin
//
end;

end.
