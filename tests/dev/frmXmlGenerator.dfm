object Form1: TForm1
  Left = 309
  Top = 137
  Width = 549
  Height = 458
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 24
    Top = 96
    Width = 95
    Height = 13
    Caption = 'Global ignore masks'
  end
  object Label2: TLabel
    Left = 224
    Top = 96
    Width = 115
    Height = 13
    Caption = 'Global ignore extensions'
  end
  object Label3: TLabel
    Left = 24
    Top = 8
    Width = 106
    Height = 13
    Caption = 'Test Images root path:'
  end
  object edFolder: TEdit
    Left = 24
    Top = 24
    Width = 377
    Height = 21
    TabOrder = 0
    OnChange = edFolderChange
  end
  object btnBrowse: TButton
    Left = 432
    Top = 22
    Width = 75
    Height = 25
    Caption = 'Browse...'
    TabOrder = 1
    OnClick = btnBrowseClick
  end
  object btnRun: TButton
    Left = 24
    Top = 56
    Width = 75
    Height = 25
    Caption = 'Run'
    TabOrder = 2
    OnClick = btnRunClick
  end
  object Panel1: TPanel
    Left = 0
    Top = 379
    Width = 533
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
    object lblStatus: TLabel
      Left = 8
      Top = 12
      Width = 5
      Height = 18
    end
  end
  object lb1: TListBox
    Left = 24
    Top = 112
    Width = 177
    Height = 249
    ItemHeight = 13
    TabOrder = 4
  end
  object lb2: TListBox
    Left = 216
    Top = 112
    Width = 137
    Height = 249
    ItemHeight = 13
    TabOrder = 5
  end
  object JvBrowseForFolderDialog1: TJvBrowseForFolderDialog
    Options = [odOnlyDirectory, odStatusAvailable, odNewDialogStyle]
    Left = 424
    Top = 48
  end
end
