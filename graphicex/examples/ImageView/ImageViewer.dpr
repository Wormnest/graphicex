{ ViewerForm GraphicEx Image Viewer sample program demonstrating some of the
             capabilities of GraphicEx.
  Dual License: MPL 1.1 or LGPL 2.1 with linking exception (the "FPC modified LGPL License")
  Portions Created by Jacob Boerema are Copyright (C) 2013-2014 Jacob Boerema.
  All Rights Reserved.
  This fork of GraphicEx can be found at https://bitbucket.org/jacobb/jgb-thirdparty
}
program ImageViewer;

uses
  {$IFDEF HEAPTRACE}
  heaptrc, SysUtils,
  {$ENDIF}
  {$IFNDEF FPC}
  FastMM4,
  {$ENDIF}
  Forms,
  {$IFDEF FPC}
  Interfaces,
  {$ENDIF}
  ViewerForm in 'ViewerForm.pas' {frmViewer},
  gexThread in 'gexThread.pas',
  gexBlend in 'gexBlend.pas';

{$IFNDEF FPC}
  // Delphi seems to have problems reading the Lazarus created resource file
  // so we will use a separate version for each.
  {$R ImageViewer_Delphi.res}
{$ELSE}
  {$R ImageViewer_Lazarus.res}
{$ENDIF}

begin
  {$IFDEF HEAPTRACE}
  if FileExists('heaptrc.log') then
      DeleteFile('heaptrc.log');
    SetHeapTraceOutput('heaptrc.log');
  {$ENDIF}

{$IFDEF VER140}
  // It is Delphi 6 and FASTMM so register its known memory leaks
  RegisterExpectedMemoryLeak(36, 1); // TWinHelpViewer x 1
  RegisterExpectedMemoryLeak(20, 3); // TObjectList x 3
  RegisterExpectedMemoryLeak(20, 3); // Unknown x 3
  RegisterExpectedMemoryLeak(52, 1); // THelpManager x 1

{$IFNDEF FIXED_SHELLCTRLS}
  // ShellCtrls specific apparently: (The fixed version doesn't need these.)
  RegisterExpectedMemoryLeak(36, 1); // String x 1
  RegisterExpectedMemoryLeak(84, 1); // TShellChangeThread x 1
{$ENDIF}
  // Newer ShellCtrls doesn't need the next 2
  // RegisterExpectedMemoryLeak(52, 1); // TStringList x 1
  // RegisterExpectedMemoryLeak(52, 1); // TShellFolder x 1
{$ENDIF}

  Application.Initialize;
  Application.CreateForm(TfrmViewer, frmViewer);
  Application.Run;
end.
