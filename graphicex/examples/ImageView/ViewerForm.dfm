object frmViewer: TfrmViewer
  Left = 263
  Top = 124
  Caption = 'frmViewer'
  ClientHeight = 655
  ClientWidth = 1600
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  ShowHint = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnMouseWheelDown = FormMouseWheelDown
  OnMouseWheelUp = FormMouseWheelUp
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 185
    Top = 0
    Width = 4
    Height = 655
    ResizeStyle = rsUpdate
  end
  object pnl4: TPanel
    Left = 189
    Top = 0
    Width = 1411
    Height = 655
    Align = alClient
    BevelOuter = bvNone
    Caption = 'pnl4'
    TabOrder = 0
    object Splitter2: TSplitter
      Left = 632
      Top = 0
      Width = 4
      Height = 655
      AutoSnap = False
      Color = clBtnFace
      ParentColor = False
      ResizeStyle = rsUpdate
      OnMoved = Splitter2Moved
    end
    object pnlRight: TPanel
      Left = 636
      Top = 0
      Width = 775
      Height = 655
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
      object pnlTop: TPanel
        Left = 0
        Top = 0
        Width = 775
        Height = 65
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        DesignSize = (
          775
          65)
        object lblThumb: TLabel
          Left = 4
          Top = 1
          Width = 4
          Height = 20
          Font.Charset = ANSI_CHARSET
          Font.Color = 9461808
          Font.Height = -15
          Font.Name = 'Segoe UI'
          Font.Style = []
          ParentFont = False
        end
        object lblComment: TLabel
          Left = 4
          Top = 21
          Width = 4
          Height = 20
          Font.Charset = ANSI_CHARSET
          Font.Color = 9461808
          Font.Height = -15
          Font.Name = 'Segoe UI'
          Font.Style = []
          ParentFont = False
        end
        object lblPages: TLabel
          Left = 4
          Top = 41
          Width = 4
          Height = 20
          Font.Charset = ANSI_CHARSET
          Font.Color = 9461808
          Font.Height = -15
          Font.Name = 'Segoe UI'
          Font.Style = []
          ParentFont = False
        end
        object spbtnFirst: TSpeedButton
          Left = 120
          Top = 40
          Width = 23
          Height = 22
          AllowAllUp = True
          Glyph.Data = {
            36040000424D3604000000000000360000002800000010000000100000000100
            20000000000000040000130B0000130B00000000000000000000FFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF0000
            00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFECEC
            ECFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF0000
            00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFD7D7D7FF5858
            58FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF0000
            00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFD4D4D4FF050505FF5858
            58FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF0000
            00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFD2D2D2FF000000FF050505FF5252
            52FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF0000
            00FFFFFFFFFFFFFFFFFFFFFFFFFFD4D4D4FF000000FF000000FF020202FF1C1C
            1CFFABABABFFABABABFFABABABFFBABABAFFFFFFFFFFFFFFFFFF000000FF0000
            00FFFFFFFFFFFFFFFFFFD6D6D6FF000000FF000000FF000000FF000000FF0000
            00FF000000FF000000FF000000FF2F2F2FFFFEFEFEFFFFFFFFFF000000FF0000
            00FFFFFFFFFFD4D4D4FF000000FF000000FF000000FF000000FF000000FF0000
            00FF000000FF000000FF000000FF2F2F2FFFFEFEFEFFFFFFFFFF000000FF0000
            00FFFFFFFFFF2F2F2FFF010101FF000000FF000000FF000000FF000000FF0000
            00FF000000FF000000FF000000FF2F2F2FFFFEFEFEFFFFFFFFFF000000FF0000
            00FFFFFFFFFFFEFEFEFF323232FF010101FF000000FF000000FF000000FF0000
            00FF000000FF000000FF000000FF2F2F2FFFFEFEFEFFFFFFFFFF000000FF0000
            00FFFFFFFFFFFFFFFFFFFEFEFEFF323232FF010101FF000000FF010101FF0A0A
            0AFF323232FF323232FF323232FF5B5B5BFFFFFFFFFFFFFFFFFF000000FF0000
            00FFFFFFFFFFFFFFFFFFFFFFFFFFFEFEFEFF303030FF010101FF050505FF5858
            58FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF0000
            00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFEFEFF313131FF060606FF5858
            58FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF0000
            00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFEFEFF373737FF5959
            59FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF0000
            00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFEFEFF8080
            80FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
          OnClick = spbtnClick
        end
        object spbtnPrev: TSpeedButton
          Left = 144
          Top = 40
          Width = 23
          Height = 22
          AllowAllUp = True
          Glyph.Data = {
            36040000424D3604000000000000360000002800000010000000100000000100
            20000000000000040000130B0000130B00000000000000000000FFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFECEC
            ECFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFD7D7D7FF5858
            58FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFD4D4D4FF050505FF5858
            58FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFD2D2D2FF000000FF050505FF5252
            52FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFD4D4D4FF000000FF000000FF020202FF1C1C
            1CFFABABABFFABABABFFABABABFFBABABAFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFD6D6D6FF000000FF000000FF000000FF000000FF0000
            00FF000000FF000000FF000000FF2F2F2FFFFEFEFEFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFD4D4D4FF000000FF000000FF000000FF000000FF000000FF0000
            00FF000000FF000000FF000000FF2F2F2FFFFEFEFEFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFF2F2F2FFF010101FF000000FF000000FF000000FF000000FF0000
            00FF000000FF000000FF000000FF2F2F2FFFFEFEFEFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFEFEFEFF323232FF010101FF000000FF000000FF000000FF0000
            00FF000000FF000000FF000000FF2F2F2FFFFEFEFEFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFEFEFEFF323232FF010101FF000000FF010101FF0A0A
            0AFF323232FF323232FF323232FF5B5B5BFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFEFEFF303030FF010101FF050505FF5858
            58FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFEFEFF313131FF060606FF5858
            58FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFEFEFF373737FF5959
            59FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFEFEFF8080
            80FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
          OnClick = spbtnClick
        end
        object spbtnNext: TSpeedButton
          Left = 168
          Top = 40
          Width = 23
          Height = 22
          AllowAllUp = True
          Glyph.Data = {
            36040000424D3604000000000000360000002800000010000000100000000100
            20000000000000040000130B0000130B00000000000000000000FFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFECECECFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF585858FFD7D7D7FFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF585858FF050505FFD4D4D4FFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF525252FF050505FF000000FFD2D2
            D2FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFBABABAFFABABABFFABABABFFABABABFF1C1C1CFF020202FF000000FF0000
            00FFD4D4D4FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFE
            FEFF2F2F2FFF000000FF000000FF000000FF000000FF000000FF000000FF0000
            00FF000000FFD6D6D6FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFE
            FEFF2F2F2FFF000000FF000000FF000000FF000000FF000000FF000000FF0000
            00FF000000FF000000FFD4D4D4FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFE
            FEFF2F2F2FFF000000FF000000FF000000FF000000FF000000FF000000FF0000
            00FF000000FF010101FF2F2F2FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFE
            FEFF2F2F2FFF000000FF000000FF000000FF000000FF000000FF000000FF0000
            00FF010101FF323232FFFEFEFEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFF5B5B5BFF323232FF323232FF323232FF0A0A0AFF010101FF000000FF0101
            01FF323232FFFEFEFEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF585858FF050505FF010101FF3030
            30FFFEFEFEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF585858FF060606FF313131FFFEFE
            FEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF595959FF373737FFFEFEFEFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF808080FFFEFEFEFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
          OnClick = spbtnClick
        end
        object spbtnLast: TSpeedButton
          Left = 192
          Top = 40
          Width = 23
          Height = 22
          AllowAllUp = True
          Glyph.Data = {
            36040000424D3604000000000000360000002800000010000000100000000100
            20000000000000040000130B0000130B00000000000000000000FFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFECECECFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF000000FFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF585858FFD7D7D7FFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF000000FFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF585858FF050505FFD4D4D4FFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF000000FFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF525252FF050505FF000000FFD2D2
            D2FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF000000FFFFFFFFFFFFFF
            FFFFBABABAFFABABABFFABABABFFABABABFF1C1C1CFF020202FF000000FF0000
            00FFD4D4D4FFFFFFFFFFFFFFFFFFFFFFFFFF000000FF000000FFFFFFFFFFFEFE
            FEFF2F2F2FFF000000FF000000FF000000FF000000FF000000FF000000FF0000
            00FF000000FFD6D6D6FFFFFFFFFFFFFFFFFF000000FF000000FFFFFFFFFFFEFE
            FEFF2F2F2FFF000000FF000000FF000000FF000000FF000000FF000000FF0000
            00FF000000FF000000FFD4D4D4FFFFFFFFFF000000FF000000FFFFFFFFFFFEFE
            FEFF2F2F2FFF000000FF000000FF000000FF000000FF000000FF000000FF0000
            00FF000000FF010101FF2F2F2FFFFFFFFFFF000000FF000000FFFFFFFFFFFEFE
            FEFF2F2F2FFF000000FF000000FF000000FF000000FF000000FF000000FF0000
            00FF010101FF323232FFFEFEFEFFFFFFFFFF000000FF000000FFFFFFFFFFFFFF
            FFFF5B5B5BFF323232FF323232FF323232FF0A0A0AFF010101FF000000FF0101
            01FF323232FFFEFEFEFFFFFFFFFFFFFFFFFF000000FF000000FFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF585858FF050505FF010101FF3030
            30FFFEFEFEFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF000000FFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF585858FF060606FF313131FFFEFE
            FEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF000000FFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF595959FF373737FFFEFEFEFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF000000FFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF808080FFFEFEFEFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF000000FFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
          OnClick = spbtnClick
        end
        object lblLoadTime: TLabel
          Left = 508
          Top = 40
          Width = 91
          Height = 20
          Caption = 'Loading time:'
          Font.Charset = ANSI_CHARSET
          Font.Color = 9461808
          Font.Height = -15
          Font.Name = 'Segoe UI'
          Font.Style = []
          ParentFont = False
        end
        object cbStretch: TCheckBox
          Left = 224
          Top = 42
          Width = 97
          Height = 17
          Caption = 'Stretch'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = 9461808
          Font.Height = -15
          Font.Name = 'Segoe UI'
          Font.Style = []
          ParentFont = False
          TabOrder = 0
          OnClick = cbStretchChange
        end
        object pbProgress: TProgressBar
          Left = 8
          Top = 21
          Width = 761
          Height = 17
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 1
        end
        object cbStretchFilter: TComboBox
          Left = 296
          Top = 40
          Width = 97
          Height = 23
          Hint = 'Stretch filters'
          Style = csDropDownList
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Segoe UI'
          Font.Style = []
          ItemIndex = 0
          ParentFont = False
          TabOrder = 2
          Text = 'Box'
          OnChange = cbStretchFilterChange
          Items.Strings = (
            'Box'
            'Triangle'
            'Hermite'
            'Bell'
            'Spline'
            'Lanczos3'
            'Mitchell')
        end
        object cbBackground: TComboBox
          Left = 400
          Top = 40
          Width = 97
          Height = 23
          Hint = 'Stretch filters'
          Style = csDropDownList
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Segoe UI'
          Font.Style = []
          ItemIndex = 0
          ParentFont = False
          TabOrder = 3
          Text = 'Dark'
          OnChange = cbBackgroundChange
          Items.Strings = (
            'Dark'
            'Light')
        end
      end
      object pnlImageContainer: TPanel
        Left = 0
        Top = 65
        Width = 775
        Height = 590
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 1
        object sbx1: TScrollBox
          Left = 0
          Top = 0
          Width = 775
          Height = 590
          HorzScrollBar.Smooth = True
          HorzScrollBar.Tracking = True
          VertScrollBar.Smooth = True
          VertScrollBar.Tracking = True
          Align = alClient
          BevelInner = bvNone
          BevelOuter = bvNone
          BorderStyle = bsNone
          TabOrder = 0
          object pnlScroll: TPanel
            Left = 0
            Top = 0
            Width = 775
            Height = 591
            BevelOuter = bvNone
            TabOrder = 0
            object pb2: TPaintBox
              Left = 0
              Top = 0
              Width = 775
              Height = 591
              Align = alClient
              Color = clAppWorkSpace
              ParentColor = False
              OnMouseDown = pb2MouseDown
              OnMouseMove = pb2MouseMove
              OnMouseUp = pb2MouseUp
              OnPaint = pbPaint
            end
          end
        end
      end
    end
    object pnlMiddle: TPanel
      Left = 0
      Top = 0
      Width = 632
      Height = 655
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 1
      object Splitter3: TSplitter
        Left = 0
        Top = 403
        Width = 632
        Height = 4
        Cursor = crVSplit
        Align = alTop
        MinSize = 60
        ResizeStyle = rsUpdate
      end
      object pnlImageFolder: TPanel
        Left = 0
        Top = 0
        Width = 632
        Height = 403
        Align = alTop
        Caption = 'pnlImageFolder'
        TabOrder = 0
        DesignSize = (
          632
          403)
        object lblStatus: TLabel
          Left = 8
          Top = 379
          Width = 40
          Height = 20
          Anchors = [akLeft, akBottom]
          Caption = 'Status'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Segoe UI'
          Font.Style = []
          ParentFont = False
        end
        object rkView1: TrkView
          Left = 1
          Top = 34
          Width = 630
          Height = 343
          Align = alTop
          Anchors = [akLeft, akTop, akRight, akBottom]
          ShowHint = True
          TabOrder = 0
          HotTracking = True
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          OnMouseDown = rkView1MouseDown
          OnMouseMove = rkView1MouseMove
          OnClick = ThumbViewClick
          OnSelecting = rkView1Selecting
          CellWidth = 140
          CellHeight = 140
          CellOffset = 5
          CellSpace = 5
          CellSelect = True
          Columns = ''
          ColorSel = 16750899
        end
        object pnlHeader: TPanel
          Left = 1
          Top = 1
          Width = 630
          Height = 33
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 1
          object lblInfo: TLabel
            Left = 140
            Top = 10
            Width = 4
            Height = 20
            Font.Charset = ANSI_CHARSET
            Font.Color = 9461808
            Font.Height = -15
            Font.Name = 'Segoe UI'
            Font.Style = []
            ParentFont = False
          end
          object tbSize: TTrackBar
            Left = 4
            Top = 2
            Width = 129
            Height = 28
            Max = 255
            Min = 31
            Frequency = 16
            Position = 127
            TabOrder = 0
            TickMarks = tmTopLeft
            OnChange = tbSizeChange
          end
        end
      end
      object pnlImageProperties: TPanel
        Left = 0
        Top = 407
        Width = 632
        Height = 248
        Align = alClient
        Caption = 'pnlImageProperties'
        TabOrder = 1
        object sgImgProperties: TStringGrid
          Left = 1
          Top = 1
          Width = 630
          Height = 246
          Align = alClient
          ColCount = 2
          DefaultColWidth = 170
          DefaultRowHeight = 20
          FixedCols = 0
          RowCount = 1
          FixedRows = 0
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Segoe UI'
          Font.Style = []
          Options = [goHorzLine, goRowSelect, goThumbTracking]
          ParentFont = False
          TabOrder = 0
          OnClick = sgImgPropertiesClick
          OnMouseDown = sgImgPropertiesMouseUpDown
          OnMouseMove = sgImgPropertiesMouseMove
          OnMouseUp = sgImgPropertiesMouseUpDown
          OnMouseWheelDown = sgImgPropertiesMouseWheelDown
          OnMouseWheelUp = sgImgPropertiesMouseWheelUp
          OnSelectCell = sgImgPropertiesSelectCell
          ColWidths = (
            170
            170)
          RowHeights = (
            20)
        end
      end
    end
  end
  object pnlFolderView: TPanel
    Left = 0
    Top = 0
    Width = 185
    Height = 655
    Align = alLeft
    Caption = 'pnlFolderView'
    TabOrder = 1
    object ShellTV1: TShellTreeView
      Left = 1
      Top = 1
      Width = 183
      Height = 653
      ObjectTypes = [otFolders]
      Root = 'rfDesktop'
      UseShellImages = True
      Align = alClient
      AutoRefresh = True
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      HideSelection = False
      Indent = 19
      ParentColor = False
      ParentFont = False
      RightClickSelect = True
      ShowRoot = False
      TabOrder = 0
      OnChange = ShellTV1Change
    end
  end
end
