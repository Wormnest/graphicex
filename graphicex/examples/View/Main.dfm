object MainForm: TMainForm
  Left = 600
  Top = 265
  Width = 787
  Height = 566
  HorzScrollBar.Tracking = True
  VertScrollBar.Tracking = True
  BorderIcons = [biSystemMenu, biMinimize, biMaximize, biHelp]
  Caption = 'GraphicEx Demo Program'
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poDefault
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyPress = FormKeyPress
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 14
  object PaintBox1: TPaintBox
    Left = 0
    Top = 28
    Width = 369
    Height = 297
    PopupMenu = ContextPopup
    OnPaint = PaintBox1Paint
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 509
    Width = 771
    Height = 19
    Panels = <
      item
        Alignment = taCenter
        Width = 100
      end
      item
        Alignment = taCenter
        Width = 150
      end
      item
        Width = 50000
      end>
    SimplePanel = False
    OnResize = StatusBarResize
  end
  object CoolBar2: TCoolBar
    Left = 0
    Top = 0
    Width = 771
    Height = 30
    AutoSize = True
    Bands = <
      item
        Control = ToolBar3
        ImageIndex = -1
        MinHeight = 26
        Width = 767
      end>
    object ToolBar3: TToolBar
      Left = 9
      Top = 0
      Width = 754
      Height = 26
      ButtonWidth = 40
      Caption = 'ToolBar3'
      ShowCaptions = True
      TabOrder = 0
      object OpenButton: TToolButton
        Left = 0
        Top = 2
        Caption = '&Open'
        ImageIndex = 0
        OnClick = OpenButtonClick
      end
      object FilterBox: TComboBox
        Left = 40
        Top = 2
        Width = 145
        Height = 22
        Style = csDropDownList
        ItemHeight = 14
        TabOrder = 0
        Items.Strings = (
          'Box'
          'Triangle'
          'Hermite'
          'Bell'
          'Spline'
          'Lanczos3'
          'Mitchell')
      end
      object Label1: TLabel
        Left = 185
        Top = 2
        Width = 36
        Height = 22
        Caption = '  Width:'
        Transparent = True
        Layout = tlCenter
      end
      object WidthEdit: TEdit
        Left = 221
        Top = 2
        Width = 52
        Height = 22
        TabOrder = 1
        Text = '0'
        OnKeyPress = WidthEditKeyPress
      end
      object WidthUpDown: TUpDown
        Left = 273
        Top = 2
        Width = 14
        Height = 22
        Associate = WidthEdit
        Min = 0
        Max = 10000
        Increment = 5
        Position = 0
        TabOrder = 2
        Wrap = False
        OnChangingEx = UpDownChangingEx
      end
      object Label2: TLabel
        Left = 287
        Top = 2
        Width = 36
        Height = 22
        Caption = ' Height:'
        Layout = tlCenter
      end
      object HeightEdit: TEdit
        Left = 323
        Top = 2
        Width = 52
        Height = 22
        TabOrder = 3
        Text = '0'
        OnKeyPress = WidthEditKeyPress
      end
      object HeightUpDown: TUpDown
        Left = 375
        Top = 2
        Width = 14
        Height = 22
        Associate = HeightEdit
        Min = 0
        Max = 10000
        Increment = 5
        Position = 0
        TabOrder = 4
        Wrap = False
        OnChangingEx = UpDownChangingEx
      end
      object ScaleButton: TToolButton
        Left = 389
        Top = 2
        Caption = 'Scale'
        ImageIndex = 1
        OnClick = ScaleClick
      end
      object ToolButton1: TToolButton
        Left = 429
        Top = 2
        Width = 8
        Caption = 'ToolButton1'
        ImageIndex = 2
        Style = tbsSeparator
      end
      object ImageIndexEdit: TEdit
        Left = 437
        Top = 2
        Width = 21
        Height = 22
        TabOrder = 5
        Text = '1'
      end
      object ImageIndexUpDown: TUpDown
        Left = 458
        Top = 2
        Width = 14
        Height = 22
        Associate = ImageIndexEdit
        Min = 1
        Max = 1
        Position = 1
        TabOrder = 6
        Wrap = False
        OnChangingEx = ImageIndexUpDownChangingEx
      end
      object ImageCountLabel: TLabel
        Left = 472
        Top = 2
        Width = 59
        Height = 22
        Caption = ' of 0 images'
        Layout = tlCenter
      end
    end
  end
  object OPD: TOpenPictureDialog
    Title = 'Open Image'
    Left = 552
    Top = 52
  end
  object PopupMenu1: TPopupMenu
    Left = 520
    Top = 52
    object JPEGImage1: TMenuItem
      Caption = 'JPEG Image'
    end
    object TruevisionTarga1: TMenuItem
      Caption = 'Truevision Targa'
      OnClick = TruevisionTarga1Click
    end
    object WindowsBitmap1: TMenuItem
      Caption = 'Windows Bitmap'
    end
  end
  object SPD: TSavePictureDialog
    DefaultExt = '.tga'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofShowHelp, ofCreatePrompt, ofEnableIncludeNotify, ofEnableSizing]
    Title = 'Save image as'
    Left = 552
    Top = 84
  end
  object ContextPopup: TPopupMenu
    Left = 520
    Top = 84
    object PropertyItem: TMenuItem
      Caption = 'Image properties'
      Default = True
      Enabled = False
      OnClick = PropertyItemClick
    end
  end
end
