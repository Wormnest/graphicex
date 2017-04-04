{ ViewerForm GraphicEx Image Viewer sample program demonstrating some of the
             capabilities of GraphicEx.
  Dual License: MPL 1.1 or LGPL 2.1 with linking exception (the "FPC modified LGPL License")
  Portions Created by Jacob Boerema are Copyright (C) 2013-2014 Jacob Boerema.
  All Rights Reserved.
  This fork of GraphicEx can be found at https://bitbucket.org/jacobb/jgb-thirdparty
}
unit ViewerForm;

interface

{$WARN UNIT_PLATFORM OFF} // Stop warning ShellCtrls is specific to a platform

{$DEFINE USE_XCF} // Detect Gimp XCF files
{$DEFINE USE_MAYAIFF}  // Detect Maya IFF images
{$DEFINE USE_AMIGAIFF} // Detect Amiga IFF images

{$IFDEF FPC}
  {$mode delphi}
{$ENDIF}

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
{$ifdef conditionalexpressions}
	{$if CompilerVersion >= 23} // XE2
  System.Types,
  {$endif}
{$endif}
  {$IFDEF FPC}
  FpImage,
  {$ENDIF}
  Dialogs, ComCtrls, ExtCtrls, StdCtrls, IniFiles,

  // ShellCtrls can be found in the Demos/Samples folder of your Delphi installation
  // See also: http://stackoverflow.com/questions/15077702/memory-leak-in-tshellchangethread
  // Note that older versions have several memory leaks.
  // An up to date version is available in the Embarcadero demos repository:
  // http://sourceforge.net/p/radstudiodemos/code/HEAD/tree/trunk/Object%20Pascal/VCL/ShellControls/ShellCtrls.pas
  // Warning: even that version (currently) still has memory leaks, especially it
  // seems a TShellChangeThread is leaked every time you change folders.
  // For a fixed version see:
  // http://www.kutinsoft.com/Hints/DelphiHints.php
  // http://web.archive.org/web/20110818065031/http://www.kutinsoft.com/Hints/DelphiHints.php

{$IFDEF FIXED_SHELLCTRLS}
  FixedShellCtrls, // ShellCtrls with the fixes and changes from kutinsoft, see link above...
                   // Since this is copyrighted material I can't include this version here,
                   // You will have to make the changes yourself to your own copy of ShellCtrls.
{$ELSE}
  ShellCtrls,      // Use the original which has bugs and memory leaks...
{$ENDIF}

  {$IFDEF USE_XCF}
  gexXCF, // Support for Gimp XCF files
  {$ENDIF}
  {$IFDEF USE_MAYAIFF}
  gexMayaIFF,
  {$ENDIF}
  {$IFDEF USE_AMIGAIFF}
  gexAmigaIFF,
  {$ENDIF}
  gexBmpWrapper,
  gexJpegWrapper,
  {$DEFINE USE_LIBJPEG}
  {$IFDEF USE_LIBJPEG}
  gexJpeg,
  {$ENDIF}
  gexTypes, GraphicEx, rkView, gexThread, Buttons, Grids;

const
  // Additional image format consts...
  CgexTiff         =  5;
  CgexTga          =  6;
  CgexPcd          =  7;
  CgexPsd          =  8;
  CgexPsp          =  9;
  CgexPnm          = 10;
  CgexPcx          = 11;
  CgexRla          = 12;
  CgexSgi          = 13;
  CgexAutodesk     = 14;
  CgexCut          = 15;
  CgexGed          = 16;
  CgexEps          = 17;
  CgexXcf          = 18;
  CgexMayaIff      = 19;
  CgexAmigaIff     = 20;
  CLASTIMAGEFORMAT = 20;
  //....

  cFileTypeNames : array [0..CLASTIMAGEFORMAT] of string =
    ( 'Unknown file type',
      'bmp image',  'jpeg image', 'png image', 'gif image',
      'tiff image', 'tga image', 'pcd image', 'psd image','psp image',
      'pnm image',  'pcx image',  'rla image', 'sgi image', 'Autodesk Animator cel/pic image',
      'DrHalo cut image', 'Arts & Letters ged thumbnail image', 'EPS image',
      'Gimp xcf image', 'Maya iff image', 'Amiga ilbm/iff image'
    );

type

  { TfrmViewer }

  TfrmViewer = class(TgexBaseForm)
    ShellTV1: TShellTreeView;
    pnlMiddle: TPanel;
    pnlRight: TPanel;
    rkView1: TrkView;
    lblStatus: TLabel;
    lblThumb: TLabel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    pnl4: TPanel;
    pnlTop: TPanel;
    lblComment: TLabel;
    lblPages: TLabel;
    spbtnFirst: TSpeedButton;
    spbtnPrev: TSpeedButton;
    spbtnNext: TSpeedButton;
    spbtnLast: TSpeedButton;
    pnlHeader: TPanel;
    tbSize: TTrackBar;
    lblInfo: TLabel;
    pb2: TPaintBox;
    cbStretch: TCheckBox;
    pbProgress: TProgressBar;
    lblLoadTime: TLabel;
    cbStretchFilter: TComboBox;
    sgImgProperties: TStringGrid;
    sbx1: TScrollBox;
    pnlImageContainer: TPanel;
    pnlScroll: TPanel;
    pnlImageFolder: TPanel;
    Splitter3: TSplitter;
    pnlFolderView: TPanel;
    pnlImageProperties: TPanel;
    cbBackground: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ShellTV1Change(Sender: TObject; Node: TTreeNode);
    procedure pbPaint(Sender: TObject);
    procedure ThumbViewClick(Sender: TObject);
    procedure rkView1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure spbtnClick(Sender: TObject);
    procedure tbSizeChange(Sender: TObject);
    procedure rkView1Selecting(Sender: TObject; Count: Integer);
    procedure rkView1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure sgImgPropertiesSelectCell(Sender: TObject; ACol,
      ARow: Integer; var CanSelect: Boolean);
    procedure sgImgPropertiesClick(Sender: TObject);
    procedure sgImgPropertiesMouseUpDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure FormMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure pb2MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pb2MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure pb2MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormResize(Sender: TObject);
    procedure sgImgPropertiesMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure Splitter2Moved(Sender: TObject);
    procedure sgImgPropertiesMouseWheelDown(Sender: TObject;
      Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure sgImgPropertiesMouseWheelUp(Sender: TObject;
      Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure cbBackgroundChange(Sender: TObject);
    procedure cbStretchChange(Sender: TObject);
    procedure cbStretchFilterChange(Sender: TObject);
  private
    { Private declarations }
    FThumbFrame,
    FThumbOffset,
    FTextHeight: Integer;
    FSelectedImage,
    FThumbWidth,
    FThumbHeight: Integer;
    FThumbnailBackground: TBitmap;
    FCheckerboardColor1,
    FCheckerboardColor2: TColor;
    FPicture: TPicture;

    FLoadTick,
    FStretchTick,
    FBlendTick: UInt64;

    CanView: Boolean;

    procedure RescaleImage(Source, Target: TBitmap; FastStretch: Boolean);
  protected
    FCapturing: Boolean;
    FLastX,
    FLastY: Integer;
    FInfoRow: Integer;
    FThumbNailPos: Integer;

    function GetView(): TrkView; override;
    procedure UpdateProgress(AMessage: string); override;
    function GetAccurateTick: Int64;

    // Info Grid Row Handling
    procedure ClearGrid;
    procedure IncInfoRow;

    procedure ReadIniSettings;
    procedure WriteIniSettings;

    property InfoRow: Integer read FInfoRow write FInfoRow;

  public
    IniFile: TIniFile;

    ImgGraphicClass: TGraphicExGraphicClass;
    ImgPage: Integer; // Curent page of multipage images or 0
    ImgPageCount: Integer;
    ImgComment: string;
    ImgFile: string;

    ImgThumbData: PgexThumbData;
    ImgProperties: TImageProperties;
    // Info about specific image types
    ImgTiffData: TActualTiffData;
    ImgIffData: TAmigaIffProperties;
    ImgPsdMode: Word;
    ImgLayerCount: Cardinal;
    PsdMergedTransparencyPresent: Boolean;
    ImgIccProfile: string;
    PsdIccEnabled: Boolean;
    ImgStrings: TStringList; // StringList with data depending on image type (currently only used by GIF)
    GifInfo: TGifInfo;       // Extra info for GIF images
    PcxIsCapture: Boolean;

    // Info for bmp type only:
    ImgRealPixelFormat: TPixelFormat;
    BmpCompression: Cardinal; // Actual bmp compression scheme

    { Public declarations }
    function DetermineImageFormat( const FileName: string;
        var ImageData: Pointer): TImageFileFormat; override;
    function ConvertImageToThumb(const FileName: string; AThumb: PgexThumbData): TBitmap; override;
    procedure DoUpdateThumbnailSize(ASize: Integer); override;
    procedure UpdateView; override;
    procedure GetBitmapInfo(ABitmap: TBitmap);
    procedure GetImageInfo(AGraphic: TGraphicExGraphic);
    procedure ReadImageInfo(AImgThumbData: PgexThumbData; APage: Integer);
    procedure CopyImageInfo(AGraphic: TGraphicExGraphic);
    procedure CopyBasicImageInfo(ABitmap: TBitmap); overload;
    procedure CopyBasicImageInfo(APicture: TPicture); overload;
    procedure ShowImageInfo;
    procedure UpdateStatus;
    procedure UpdateImageStatus;
    procedure UpdateLoadingStatus;
    procedure UpdatePageButtons;
    procedure LoadImage(Thumb: PgexThumbData);
    procedure ImageGotoPage(PageNo: Integer; ForceReload: Boolean = False);
    procedure HandleStretch;
    procedure UpdatePaintBoxSize;
    procedure ShowErrors;

    // Creates the checkered default background for an entry.
    procedure CreateDefaultBackground;
    procedure UpdateDefaultBackground;
    procedure FillBackground(R: TRect; Target: TCanvas);
    procedure WMEraseBkgnd(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;

    {$IFNDEF FPC}
    procedure ImageLoadProgress(Sender: TObject; Stage: TProgressStage; PercentDone: Byte; RedrawNow: Boolean;
      const R: TRect; const Msg: string);
    {$ELSE}
    procedure ImageLoadProgress(Sender: TObject; Stage: TProgressStage; PercentDone: Byte; RedrawNow: Boolean;
      const R: TRect; const Msg: string; var Continue : Boolean);
    {$ENDIF}
  end;

var
  frmViewer: TfrmViewer;

implementation

{$IFNDEF FPC}
{$R *.dfm}
{$ELSE}
{$R *.lfm}
{$ENDIF}

uses Math, GraphicColor,
  {$IFNDEF FPC} jpeg, {$ENDIF}
  LibTiffDelphi, LibJpeg,
  gexBlend, gexStretch;


const
  // Ini file settings
  C_IniSectionMain  = 'Settings';
  C_IniLastFolder   = 'LastFolder';
  C_WinLeft         = 'WinLeft';
  C_WinTop          = 'WinTop';
  C_WinWidth        = 'WinWidth';
  C_WinHeight       = 'WinHeight';
  C_ThumbNailPos    = 'ThumbnailPos';
  C_FolderViewWidth = 'FolderViewWidth';
  C_ImgFolderHeight = 'ImageFolderViewHeight';
  C_MiddleViewWidth = 'MiddleViewWidth';
  C_Maximized       = 'Maximized';  // 1 = maximized, 0 = not maximized

const
  Light_bgColor1 = clBtnFace;
  Light_bgColor2 = clBtnHighlight;
  Dark_bgColor1 = TColor($606060); // a medium dark gray
  Dark_bgColor2 = TColor($909090); // a little lighter gray

var TiffError: array[0..1000] of Char;
  CollectErrors: Boolean;

  // List of Errors/Warnings/Info when loading images
  // A code is stored in the object to specify the type of message
  // -2 = error, -1 = warning, >= 0 = info
  // -3 = exception during loading of image
  ErrorList: TStringList;

procedure gexIgnoreTIFFError(const a, b: AnsiString);
begin
  //ErrorList.Add(a + ':  ' + b);
  ErrorList.AddObject(string(a + ':  ' + b), TObject(-2));
  StrPCopy(TiffError,string(a + ':  ' + b));
end;

procedure gexTIFFWarning(const a, b: AnsiString);
begin
  ErrorList.AddObject(string(a + ':  ' + b), TObject(-1));
end;

// JpegLib error/warning/info message interception
procedure JpegMessageInterceptor(const AMessage: string; const AMessageLevel: Integer;
  var AContinue: Boolean);
begin
  // Don't add messages at level 3. There can sometimes be a large amount of them
  // and they are usually not interesting.
  if AMessageLevel < 3 then
    ErrorList.AddObject(Format('LibJpeg message level %d. %s',[AMessageLevel, AMessage]), TObject(AMessageLevel));
  // Continue for warnings and informational messages
  AContinue := AMessageLevel >= -1;
end;

// Add a disabled state bitmap to SpeedButtons (and BitBtns)
// Source: http://www.swissdelphicenter.ch/en/showcode.php?id=1865
procedure AddDisabledBmp(Buttons : array of TObject);
{$IFNDEF FPC}
var
  BM, SBM : TBitmap;
  w, x, y, NewColor, i : integer;
  PixelColor : TColor;
{$ENDIF}
begin
  // Disabled for fpc since it's not getting painted correctly.
  // Figure this out some other time since it looks fine on fpc
  // without doing this (at least when using Windows themes).
  {$IFNDEF FPC}
  for i := 0 to High(Buttons) do
  begin
    // jb For some reason assigning the glyps doesn't seem to work if we
    // reuse the bitmaps. Therefore we create and free them inside the loop
    // until we figure out a better way.
    BM := TBitmap.Create;
    SBM := TBitmap.Create;
    try
      if (Buttons[i] is TSpeedButton) then
        BM.Assign((Buttons[i] as TSpeedButton).Glyph)
      else if (Buttons[i] is TBitBtn) then
        BM.Assign((Buttons[i] as TBitBtn).Glyph)
      else
        Exit; // Will jump into the finally part.

      if not Assigned(BM) or (BM.Width <> BM.Height) then
        Exit; // Will jump into the finally part.

      w := BM.Width;
      SBM.Width := w * 2;
      SBM.Height := w;
      SBM.Canvas.Lock;
      SBM.Canvas.Draw(0, 0, BM);

      for x := 0 to w - 1 do
        for y := 0 to w - 1 do
        begin
          PixelColor := ColorToRGB(BM.Canvas.Pixels[x, y]);
          // jb Original below was 96, changed to 128 to make it look more gray
          NewColor := Round((((PixelColor shr 16) + ((PixelColor shr 8) and $00FF) +
            (PixelColor and $0000FF)) div 3)) div 2 + 128;
          BM.Canvas.Pixels[x, y] := RGB(NewColor, NewColor, NewColor);
        end;

      SBM.Canvas.Draw(w, 0, BM);

      if (Buttons[i] is TSpeedButton) then with (Buttons[i] as TSpeedButton) do
        begin
          Glyph.Assign(SBM);
          NumGlyphs := 2;
        end
      else
        with (Buttons[i] as TBitBtn) do
        begin
          Glyph.Assign(SBM);
          NumGlyphs := 2;
        end;

      // Make BM empty since the loop tests if BM is assigned
      // (and don't create BM and SBM here without freeing them
      // like the original function did)
    finally
      BM.Free;
      SBM.Free;
    end;
  end;
  {$ENDIF}
end;

procedure TfrmViewer.FormCreate(Sender: TObject);
var SelRect: TGridRect;
begin
  ShowHiddenFiles := True;
  // WARNING: Ini file is stored in the same location as exe: Obviously you
  //  shouldn't use this in production code installed to Program Files!
  IniFile := TIniFile.Create(ChangeFileExt(Application.ExeName,'.ini')) ;
  ReadIniSettings;
  CanView := False;
  // the space to be left between the border and the content in an image (horizontally and vertically)
  FThumbFrame := 2;
  // the space to be left between two adjacent images (horizontally and vertically)
  FThumbOffset := 10;
  // height of the entire text area below each image
  FTextHeight := 10;
  // thumb size
  FThumbWidth := 128;
  FThumbHeight := 128;

  FSelectedImage := -1;

  // Since we are an image viewer we don't want GraphicEx to throw an exception
  // on every image with problems.
  // For now we can only set TIFF reading to not show exceptions, for other formats this is TODO!
  LibTiffDelphiSetErrorHandler({$IFDEF FPC}@{$ENDIF}gexIgnoreTIFFError);
  // Also add a handler for TIFF warnings
  LibTiffDelphiSetWarningHandler({$IFDEF FPC}@{$ENDIF}gexTIFFWarning);
  // And set a handler for LibJpeg
  SetMessageInterceptor({$IFDEF FPC}@{$ENDIF}JpegMessageInterceptor);
  TiffError := '';

  // Add disabled state bitmaps to our page SpeedButtons
  AddDisabledBmp([spbtnFirst, spbtnPrev, spbtnNext, spbtnLast]);

  // Default checkerboard background colors are darkish
  FCheckerboardColor1 := Dark_bgColor1;
  FCheckerboardColor2 := Dark_bgColor2;
  // Make the checkered background
  CreateDefaultBackground;
  // Initialize picture
  FPicture := TPicture.Create;

  // ***************************************************************************
  // TODO: Create rkView here, that way developers wanting to test GraphicEx
  // don't need to have rkView installed.
  // ***************************************************************************

  // We don't want selection in stringgrid
  // Setting them all to -1 however leads to stringgrid out of range exceptions!
  SelRect.Left := 0;
  SelRect.Top  := 0;
  SelRect.Right := 1;
  SelRect.Bottom := 0;
  sgImgProperties.Selection := SelRect;

  // We don't want exception messages to popup during loading of images etc....
  SilentThumbLoadingException := True;
  SilentThreadException := True;

  ImgStrings := TStringList.Create;
end;

procedure TfrmViewer.FormDestroy(Sender: TObject);
begin
  WriteIniSettings;
  CanView := False;
  FPicture.Free;
  FThumbnailBackground.Free;
  IniFile.Free;
  ImgStrings.Free;
end;

procedure TfrmViewer.FormShow(Sender: TObject);
begin
  FBlendTick := 0;
  pbProgress.Hide;
  UpdatePageButtons;
  CanView := True;
  if ImageFolder <> '' then
    ShellTV1.Path := ImageFolder;
  sgImgProperties.ColWidths[1] := sgImgProperties.ClientWidth -
    sgImgProperties.ColWidths[0];
  tbSize.Position := FThumbNailPos;
  pb2.Invalidate;
end;

procedure TfrmViewer.ReadIniSettings;
begin
  if IniFile = nil then
    Exit;
  ImageFolder := IniFile.ReadString(C_IniSectionMain, C_IniLastFolder, '');
  if IniFile.ReadInteger(C_IniSectionMain, C_Maximized, 0) = 1 then
    WindowState := wsMaximized
  else begin
    SetBounds(
      IniFile.ReadInteger(C_IniSectionMain, C_WinLeft, Left),
      IniFile.ReadInteger(C_IniSectionMain, C_WinTop, Top),
      IniFile.ReadInteger(C_IniSectionMain, C_WinWidth, Width),
      IniFile.ReadInteger(C_IniSectionMain, C_WinHeight, Height)
    );
  end;
  FThumbNailPos := IniFile.ReadInteger(C_IniSectionMain, C_ThumbNailPos, 128);
  pnlFolderView.Width := IniFile.ReadInteger(C_IniSectionMain, C_FolderViewWidth, 185);
  pnlMiddle.Width := IniFile.ReadInteger(C_IniSectionMain, C_MiddleViewWidth, 600);
  pnlImageFolder.Height := IniFile.ReadInteger(C_IniSectionMain, C_ImgFolderHeight, 400);
end;

procedure TfrmViewer.WriteIniSettings;
begin
  if IniFile = nil then
    Exit;
  IniFile.WriteString(C_IniSectionMain, C_IniLastFolder, ImageFolder);
  if WindowState = wsMaximized then begin
    IniFile.WriteInteger(C_IniSectionMain, C_Maximized, 1);
  end
  else begin
    IniFile.WriteInteger(C_IniSectionMain, C_Maximized, 0);
    IniFile.WriteInteger(C_IniSectionMain, C_WinTop, Top) ;
    IniFile.WriteInteger(C_IniSectionMain, C_WinLeft, Left);
    IniFile.WriteInteger(C_IniSectionMain, C_WinWidth, Width);
    IniFile.WriteInteger(C_IniSectionMain, C_WinHeight, Height);
  end;
  IniFile.WriteInteger(C_IniSectionMain, C_ThumbNailPos, tbSize.Position);
  IniFile.WriteInteger(C_IniSectionMain, C_FolderViewWidth, pnlFolderView.Width);
  IniFile.WriteInteger(C_IniSectionMain, C_ImgFolderHeight, pnlImageFolder.Height);
  IniFile.WriteInteger(C_IniSectionMain, C_MiddleViewWidth, pnlMiddle.Width);
  IniFile.UpdateFile;
end;

procedure TfrmViewer.ShellTV1Change(Sender: TObject; Node: TTreeNode);
begin
  ImageFolder := ShellTV1.Path;
  if CanView then begin
    LoadThumbnails;
    UpdateStatus;
  end;
end;

function TfrmViewer.GetView(): TrkView;
begin
  Result := rkView1;
end;

function BytesToStr(const i64Size: Int64): string;
const
  i64GB = 1024 * 1024 * 1024;
  i64MB = 1024 * 1024;
  i64KB = 1024;
begin
  if i64Size div i64GB > 0 then
    Result := Format('%.1f GB', [i64Size / i64GB])
  else if i64Size div i64MB > 0 then
    Result := Format('%.2f MB', [i64Size / i64MB])
  else if i64Size div i64KB > 0 then
    Result := Format('%.0f kB', [i64Size / i64KB])
  else
    Result := IntToStr(i64Size) + ' byte';
end;

procedure TfrmViewer.UpdateView;
begin
  UpdateStatus;
end;

procedure TfrmViewer.UpdateStatus;
var
  i: integer;
  item: PgexThumbData;
  RecognizedCount: Integer;
begin
  RecognizedCount := 0;
  for i := 0 to rkView1.Items.Count - 1 do
  begin
    item := Items[i];
    if item.ImageFormat > CgexUnknown then
      Inc(RecognizedCount);
  end;
  lblInfo.Caption := IntToStr(rkView1.Items.Count) + ' files, ' +
    IntToStr(RecognizedCount) + ' recognized images';
end;

procedure TfrmViewer.UpdateProgress(AMessage: string);
begin
  lblStatus.Caption := AMessage;
end;

// Not used currently
procedure TfrmViewer.RescaleImage(Source, Target: TBitmap; FastStretch: Boolean);
// if source is in at least one dimension larger than the thumb size then rescale source
// but keep aspect ratio
var
  NewWidth,
  NewHeight: Integer;
begin
  if (Source.Width > FThumbWidth) or (Source.Height > FThumbHeight) then
  begin
    // Note: rescaling does only work for 24 bit images hence even monochrome images
    //       are converted to RGB.
    if Source.Width > Source.Height then
    begin
      NewWidth := FThumbWidth;
      NewHeight := Round(FThumbHeight * Source.Height / Source.Width);
    end
    else
    begin
      NewHeight := FThumbHeight;
      NewWidth := Round(FThumbWidth * Source.Width / Source.Height);
    end;
    if FastStretch then
    begin
      Target.PixelFormat := pf24Bit;
      Target.Width := NewWidth;
      Target.Height := NewHeight;
      Target.Palette := Source.Palette;
      SetStretchBltMode(Target.Canvas.Handle, COLORONCOLOR);
      StretchBlt(Target.Canvas.Handle, 0, 0, NewWidth, NewHeight, Source.Canvas.Handle, 0, 0,
                 Source.Width, Source.Height, SRCCOPY);
      //Target.Canvas.CopyRect(Rect(0, 0, NewWidth, NewHeight), Source.Canvas, Rect(0, 0, Source.Width, Source.Height));
    end
    else Stretch(NewWidth, NewHeight, sfTriangle, 0, Source, Target);
  end
  else Target.Assign(Source);
end;

procedure TfrmViewer.pbPaint(Sender: TObject);
var
  X: Integer;
  Y: Integer;
  R: TRect;
  Buffer: TBitmap;

begin
  with Sender as TPaintBox do
  begin
    if ClientWidth > FPicture.Width then
      X := (ClientWidth - FPicture.Width) div 2
    else
      X := 0;
    if ClientHeight > FPicture.Height then
      Y := (ClientHeight - FPicture.Height) div 2
    else
      Y := 0;

    {$IFNDEF FPC}
    if FPicture.Bitmap.PixelFormat = pf32Bit then
    {$ELSE}
    // In Fpc we need an extra check because 32 bit bitmaps get loaded as pf32Bit
    // but internally in the DC they are 24 bit!
    if (FPicture.Bitmap.PixelFormat = pf32Bit) and
       (ImgRealPixelFormat in [pf32Bit, pfCustom]) then
    {$ENDIF}
    begin
      Buffer := TBitmap.Create;
      try
        FBlendTick := GetAccurateTick;
        Buffer.Width := Max(ClientWidth, FPicture.Width);
        Buffer.Height := Max(ClientHeight, FPicture.Height);
        Buffer.PixelFormat := pf32Bit;

        R := Rect(0, 0, FPicture.Width, FPicture.Height);
        Buffer.Canvas.Lock;
        try
          FillBackground(ClientRect, Buffer.Canvas);
          // Note: I can't get bmPerPixelAlpha to work in 64 bits mode for Fpc thus
          // we use bmConstantAlpha. Since (currently) the background canvas is opaque
          // it doesn't matter anyway. We would only need it if we wanted to blend
          // two semi transparent images.
          // Note that even if 32 bits mode I have my doubts whether the Fpc
          // version works correct.
          // New note: It seems that Delphi only works (blends) when using bmPerPixelAlpha
          // so we will use that for Delphi and bmConstantAlpha for Lazarus.
          {$IFNDEF FPC}
          gexBlend.AlphaBlend(FPicture.Bitmap.Canvas.Handle, Buffer.Canvas.Handle, R, Point(X, Y), bmPerPixelAlpha, 0, 0);
          {$ELSE}
          gexBlend.AlphaBlend(FPicture.Bitmap.Canvas.Handle, Buffer.Canvas.Handle, R, Point(X, Y), bmConstantAlpha, 255, 0);
          {$ENDIF}
        finally
          Buffer.Canvas.Unlock;
        end;
        FBlendTick := GetAccurateTick - FBlendTick;
        Fillbackground(ClientRect, Canvas); // Needed for fpc
        Canvas.Draw(0, 0, Buffer);
        UpdateLoadingStatus;
      finally
        Buffer.Free;
      end;
    end
    else
    begin
      Canvas.Draw(X, Y, FPicture.Graphic);
      ExcludeClipRect(Canvas.Handle, X, Y, X + FPicture.Width, Y + FPicture.Height);
      Fillbackground(ClientRect, Canvas);
    end;
  end;
end;

procedure TfrmViewer.UpdateImageStatus;
begin
  lblComment.Caption := ImgComment;
  // To support being able to see multiline or long comments also store it in a hint
  lblComment.Hint := ImgComment;
  if ImgPageCount > 0 then
    lblPages.Caption := Format('Page %d of %d',[ImgPage+1, ImgPageCount])
  else
    lblPages.Caption := '';
end;

procedure TfrmViewer.UpdatePageButtons;
begin
  if ImgPageCount > 1 then begin
    spbtnFirst.Enabled := ImgPage > 0;
    spbtnPrev.Enabled := ImgPage > 0;
    spbtnNext.Enabled := ImgPage < ImgPageCount-1;
    spbtnLast.Enabled := ImgPage < ImgPageCount-1;
  end
  else begin
    // 0 or 1 page, disable all
    spbtnFirst.Enabled := False;
    spbtnPrev.Enabled := False;
    spbtnNext.Enabled := False;
    spbtnLast.Enabled := False;
  end;
end;

procedure TfrmViewer.ReadImageInfo(AImgThumbData: PgexThumbData; APage: Integer);
var AGraphic: TGraphicExGraphic;
begin
  if (AImgThumbData = nil) or (AImgThumbData.ImageFormat <= CgexJpeg) or
     (AImgThumbData.ImageData = nil) then
    Exit;
  AGraphic := AImgThumbData.ImageData;
  if AGraphic.InheritsFrom(TGraphicExGraphic) then begin
    if AGraphic.ReadImageProperties(AImgThumbData.Name, APage) then begin
      lblThumb.Caption := Format('%s (%d x %d), type: %s, modified: %s',
          [AImgThumbData.Name, AGraphic.ImageProperties.Width , AGraphic.ImageProperties.Height,
          cFileTypeNames[AImgThumbData.ImageFormat], DateToStr(AImgThumbData.Modified)]);
    end;
  end;
end;

const
  CColorScheme: array [TColorScheme] of string = (
    'Unknown',
    'Indexed',                      // Palette format.
    'Indexed with alpha channel',   // Palette format with alpha channel.
    'Grayscale',                    // Gray scale.
    'Grayscale with alpha channel', // Gray scale with alpha channel.
    'RGB',           // Red, green, blue.
    'RGBA',          // RGB with alpha channel
    'BGR',           // RGB in reversed order.
    'BGRA',          // BGR with alpha channel.
    'CMY',           // Cyan, magenta, yellow.
    'CMYK',          // CMY with black.
    'CMYKA',         // CMYK with alpha channel.
    'CIE L*a*b*',    // 1976 CIE L*a*b* color format using luminance and chromaticities.
    'ICC L*a*b*',    // ICC L*a*b*
    'ITU L*a*b*',    // ITU L*a*b*
    'CIE Log2(L)',   // CIE Log2(L)
    'CIE Log2(L) (u''v'')', // CIE Log2(L) (u', v')
    'YCbCr',         // Another format using luminance and chromaticities.
    'PhotoYCC',      // A modified YCbCr version used for photo CDs.
    'YCCK (CMYK)',   // CMYK stored in JPEG by PhotoShop
    'CIE XYZ'        // CIE XYZ
  );
  CCompression: array [TCompressionType] of string = (
    'Unknown',          // Compression type is unknown.
    'None (not compressed)',             // No compression.
    'RLE',              // Run length encoding.
    'PackedBits',       // Macintosh packed bits.
    'LZW',              // Lempel-Zif-Welch.
    'Fax3',             // CCITT T.4 (1D), also known as fax group 3.
    '2DFax3',           // CCITT T.4 (2D).
    'FaxRLE',           // Modified Huffman (CCITT T.4 derivative).
    'Fax4',             // CCITT T.6, also known as fax group 4.
    'FaxRLEW',          // CCITT T.4 with word alignment.
    'LZ77',             // Hufman inflate/deflate.
    'JPEG',             // TIF JPEG compression (new version)
    'OJPEG',            // TIF JPEG compression (old version)
    'Thunderscan',      // TIF thunderscan compression
    'Next',
    'IT8CTPAD',
    'IT8LW',
    'IT8MP',
    'IT8BL',
    'PixarFilm',
    'PixarLog',
    'DCS',
    'JBIG',
    'PCDHuffmann',      // PhotoCD Hufman compression
    'PlainZip',         // ZIP compression without prediction
    'PredictedZip',     // ZIP comression with prediction
    'SGILog',           // SGI Log Luminance RLE
    'SGILog24',         // SGI Log 24-bit packed
    'Jpeg2000',
    'LZMA'
  );

type
  TKnownTiffCompressionScheme = record
    tid: Cardinal;       // Original TIFF compression ID
    id:  Cardinal;       // Our id without gaps
  end;
  TKnownTiffColorScheme = TKnownTiffcompressionScheme;

const
  TIFF_COMPRESSION_MAX = 28;
  NOT_A_TIFF_COMPRESSION_SCHEME = 'Not available in TIFF';
  CGraphicEx2TiffCompression: array [TCompressionType] of string = (
    'Unknown or unsupported',
    'None (dump mode)',
    NOT_A_TIFF_COMPRESSION_SCHEME,
    'PackBits (Macintosh RLE)',
    'LZW',
    'Fax group 3 = CCITT T.4 (1D)',
    'Fax group 3 = CCITT T.4 (2D)',
    'CCITT modified Huffman RLE',
    'Fax group 4 (CCITT T.6)',
    'CCITTRLEW (T.4 with word alignment)',
    'LZ77 (Deflate or Adobe Deflate)',
    'JPEG',
    'Old JPEG in TIFF',
    'Thunderscan',
    'NeXt 2-bit RLE',
    'IT8 CT w/padding',
    'IT8 Linework RLE',
    'IT8 Monochrome picture',
    'IT8 Binary line art',
    'Pixar companded 10bit LZW',
    'Pixar companded 11bit ZIP',
    'Kodak DCS encoding',
    'ISO JBIG',
    NOT_A_TIFF_COMPRESSION_SCHEME,
    NOT_A_TIFF_COMPRESSION_SCHEME,
    NOT_A_TIFF_COMPRESSION_SCHEME,
    'SGI Log Luminance RLE',
    'SGI Log 24-bit packed',
    'Jpeg2000',
    'LZMA'
  );

  CKnownTiffCompressionSchemes: array [0..TIFF_COMPRESSION_MAX] of TKnownTiffCompressionScheme = (
    (tid: 0;                           id:  0;),
    (tid: COMPRESSION_NONE;            id:  1;),
    (tid: COMPRESSION_CCITTRLE;        id:  2;),
    (tid: COMPRESSION_CCITTFAX3;       id:  3;),
    (tid: COMPRESSION_CCITTFAX4;       id:  4;),
    (tid: COMPRESSION_LZW;             id:  5;),
    (tid: COMPRESSION_OJPEG;           id:  6;),
    (tid: COMPRESSION_JPEG;            id:  7;),
    (tid: COMPRESSION_ADOBE_DEFLATE;   id:  8;),
    (tid: COMPRESSION_T85;             id:  9;),
    (tid: COMPRESSION_T43;             id: 10;),
    (tid: COMPRESSION_NEXT;            id: 11;),
    (tid: COMPRESSION_CCITTRLEW;       id: 12;),
    (tid: COMPRESSION_PACKBITS;        id: 13;),
    (tid: COMPRESSION_THUNDERSCAN;     id: 14;),
    (tid: COMPRESSION_IT8CTPAD;        id: 15;),
    (tid: COMPRESSION_IT8LW;           id: 16;),
    (tid: COMPRESSION_IT8MP;           id: 17;),
    (tid: COMPRESSION_IT8BL;           id: 18;),
    (tid: COMPRESSION_PIXARFILM;       id: 19;),
    (tid: COMPRESSION_PIXARLOG;        id: 20;),
    (tid: COMPRESSION_DEFLATE;         id: 21;),
    (tid: COMPRESSION_DCS;             id: 22;),
    (tid: COMPRESSION_JBIG;            id: 23;),
    (tid: COMPRESSION_SGILOG;          id: 24;),
    (tid: COMPRESSION_SGILOG24;        id: 25;),
    (tid: COMPRESSION_JP2000;          id: 26;),
    (tid: COMPRESSION_LZMA;            id: 27;),
    (tid: COMPRESSION_LEADTOOLS_CMP;   id: 28;)
  );
  CTiffCompression: array [0..TIFF_COMPRESSION_MAX] of string = (
    'Unknown',
    'None (dump mode)',
    'CCITT modified Huffman RLE',
    'CCITT Group 3 fax encoding (CCITT T.4)',
    'CCITT Group 4 fax encoding (CCITT T.6)',
    'LZW',
    'Old JPEG in TIFF',
    'JPEG',
    'Deflate, as recognized by Adobe',
    'TIFF/FX T.85 JBIG compression',
    'TIFF/FX T.43 color by layered JBIG compression',
    'NeXT 2-bit RLE',
    'CCITTRLEW (#1 w/ word alignment)',
    'PackBits (Macintosh RLE)',
    'ThunderScan RLE',
    'IT8 CT w/padding',
    'IT8 Linework RLE',
    'IT8 Monochrome picture',
    'IT8 Binary line art',
    'Pixar companded 10bit LZW',
    'Pixar companded 11bit ZIP',
    'Deflate',
    'Kodak DCS encoding',
    'ISO JBIG',
    'SGI Log Luminance RLE',
    'SGI Log 24-bit packed',
    'Leadtools JPEG2000',
    'LZMA2',
    'LeadTools Proprietary "FILE_TIF_CMP"'
  );

  TIFF_COLOR_MAX = 14;
  CKnownTiffColorSchemes: array [0..TIFF_COLOR_MAX] of TKnownTiffColorScheme = (
    (tid: Cardinal(-1);                id:  0;),
    (tid: PHOTOMETRIC_MINISWHITE;      id:  1;),
    (tid: PHOTOMETRIC_MINISBLACK;      id:  2;),
    (tid: PHOTOMETRIC_RGB;             id:  3;),
    (tid: PHOTOMETRIC_PALETTE;         id:  4;),
    (tid: PHOTOMETRIC_MASK;            id:  5;),
    (tid: PHOTOMETRIC_SEPARATED;       id:  6;),
    (tid: PHOTOMETRIC_YCBCR;           id:  7;),
    (tid: PHOTOMETRIC_CIELAB;          id:  8;),
    (tid: PHOTOMETRIC_ICCLAB;          id:  9;),
    (tid: PHOTOMETRIC_ITULAB;          id:  10;),
    (tid: PHOTOMETRIC_CFA;             id:  11;),
    (tid: PHOTOMETRIC_LOGL;            id:  12;),
    (tid: PHOTOMETRIC_LOGLUV;          id:  13;),
    (tid: PHOTOMETRIC_LINEAR_RAW;      id:  14;)
  );
  CTiffColor: array [0..TIFF_COLOR_MAX] of string = (
    'Unknown',
    'Min is White',
    'Min is Black',
    'RGB',
    'Palette',
    'Mask',
    'Separated',
    'YCbCr',
    'CIE L*a*b*',
    'ICC L*a*b*',
    'ITU L*a*b*',
    'CFA (DNG color filter array)',
    'CIE Log2(L)',
    'CIE Log2(L) (u''v'')',
    'Linear Raw (DNG)'
  );

  CBmpCompression: array [0..6] of string = (
    'None (not compressed)',
    'RLE 8 bit',
    'RLE 4 bit',
    'Bit field',
    'JPEG',
    'PNG',
    'Alpha bit fields'
  );

  COrientation: array [TgexOrientation] of string = (
    'Unknown',
    'TopLeft',
    'TopRight',
    'BottomRight',
    'BottomLeft',
    'LeftTop',
    'RightTop',
    'RightBottom',
    'LeftBottom'
  );
  // TIFF Data format of samples
  CSampleFormat: array [0..6] of string = (
    'Unknown',
    'Unsigned integer',
    'Signed integer',
    'IEEE floating point',
    'Undefined data format',
    'Complex signed integer',
    'Complex IEEE floating point'
  );

  CPixelFormat: array [TPixelFormat] of string = (
    'pfDevice',
    'pf1Bit',
    'pf4Bit',
    'pf8Bit',
    'pf15Bit',
    'pf16Bit',
    'pf24Bit',
    'pf32Bit',
    'pfCustom'
  );

procedure TfrmViewer.ClearGrid;
var i: Integer;
begin
  for i := 0 to sgImgProperties.ColCount-1 do
    sgImgProperties.Cols[i].Clear;
  FInfoRow := 0
end;

procedure TfrmViewer.IncInfoRow;
begin
  Inc(FInfoRow);
  if FInfoRow >= sgImgProperties.RowCount then
    sgImgProperties.RowCount := sgImgProperties.RowCount + 5;
end;

function FindStringIndex(DataArray: Pointer; AMaxIndex: Cardinal; AValue: Cardinal): Integer;
type
  TDataArray = array [0..999] of TKnownTiffCompressionScheme;
var
  i: Integer;
  TiffDataArray: ^TDataArray;
begin
  Result := -1;
  TiffDataArray := DataArray;
  for i := 0 to AMaxIndex do begin
    if TiffDataArray^[i].tid = AValue then begin
      Result := TiffDataArray^[i].id;
      break;
    end;
  end;
end;

procedure TfrmViewer.ShowImageInfo;
var //r: Integer; // row
  Temp: string;
  TempVal: Integer;
  TempFloat: Single;
begin
  // ImgThumbData is expected to be validated here already!
  // ImgProperties is expected to contain valid data.

  sgImgProperties.RowCount := 15;
  // Basic things supported by all or expected to be set to 0
  sgImgProperties.Cells[0,InfoRow] := 'Image:';
  sgImgProperties.Cells[1,InfoRow] := ImgThumbData.Name; IncInfoRow;
  sgImgProperties.Cells[0,InfoRow] := 'Image format:';
  sgImgProperties.Cells[1,InfoRow] := cFileTypeNames[ImgThumbData.ImageFormat]; IncInfoRow;
  if (ImgThumbData.ImageFormat = CgexPNM) then begin
    sgImgProperties.Cells[0,InfoRow] := 'NetPBM type:';
     if ImgProperties.Version in [1, 3, 5] then
       Temp := 'Ascii'
     else
       Temp := 'Binary';
     case ImgProperties.Version of
       1, 4: sgImgProperties.Cells[1,InfoRow] := 'Portable BitMap (PBM) - ' + Temp + ' version.';
       2, 5: sgImgProperties.Cells[1,InfoRow] := 'Portable GrayMap (PGM) - ' + Temp + ' version.';
       3, 6: sgImgProperties.Cells[1,InfoRow] := 'Portable PixMap (PPM) - ' + Temp + ' version.';
     end;
     IncInfoRow;
  end
  else if (ImgThumbData.ImageFormat = CgexPCX) and PcxIsCapture then begin
    sgImgProperties.Cells[0,InfoRow] := 'Format note:';
    sgImgProperties.Cells[1,InfoRow] := 'Not a normal PCX but a Word for Dos screen capture.'; IncInfoRow;
  end
  else if (ImgThumbData.ImageFormat = CgexRLA) and (ImgProperties.Version = 0) then begin
    sgImgProperties.Cells[0,InfoRow] := 'Format note:';
    sgImgProperties.Cells[1,InfoRow] := 'Older version original RLA or RLB.'; IncInfoRow;
  end;
  if ImgProperties.Version > 0 then begin
    sgImgProperties.Cells[0,InfoRow] := 'Image format version:';
    sgImgProperties.Cells[1,InfoRow] := IntToStr(ImgProperties.Version); IncInfoRow;
  end;
  sgImgProperties.Cells[0,InfoRow] := 'Dimensions (w x h):';
  sgImgProperties.Cells[1,InfoRow] := Format('%u x %u',[ImgProperties.Width , ImgProperties.Height]); IncInfoRow;

  if (ImgThumbData.ImageFormat = CgexBitmap) and (ImgProperties.ColorScheme = csUnknown) then begin
    // .bmp with PixelFormat pfDevice, we can't exactly determine it without doing more work
    sgImgProperties.Cells[0,InfoRow] := 'Color scheme:';
    sgImgProperties.Cells[1,InfoRow] := 'probably Indexed'; IncInfoRow;
    sgImgProperties.Cells[0,InfoRow] := 'Bits per pixel:';
    sgImgProperties.Cells[1,InfoRow] := IntToStr(ImgProperties.BitsPerPixel) +
      ' (current device representation)'; IncInfoRow;
    sgImgProperties.Cells[0,InfoRow] := 'Compression:';
    sgImgProperties.Cells[1,InfoRow] := 'probably RLE 8 or RLE 4'; IncInfoRow;
  end
  {$IFDEF USE_AMIGAIFF}
  else if (ImgThumbData.ImageFormat = CgexAmigaIff) then begin
    sgImgProperties.Cells[0,InfoRow] := 'Iff type:';
    case ImgIffData.IffType of
      itIlbm: sgImgProperties.Cells[1,InfoRow] := 'ilbm';
      itPbm:  sgImgProperties.Cells[1,InfoRow] := 'pbm';
      itAcbm: sgImgProperties.Cells[1,InfoRow] := 'acbm';
      itAnim: sgImgProperties.Cells[1,InfoRow] := 'anim';
      itRgbn: sgImgProperties.Cells[1,InfoRow] := 'rgbn';
      itRgb8: sgImgProperties.Cells[1,InfoRow] := 'rgb8';
      itDeep: sgImgProperties.Cells[1,InfoRow] := 'deep';
    end;
    IncInfoRow;
    sgImgProperties.Cells[0,InfoRow] := 'Color scheme:';
    if cfHam in ImgIffData.CamgFlags then
      sgImgProperties.Cells[1,InfoRow] := 'HAM (Combined Indexed/RGB mode)'
    else
      case ImgIffData.nPlanes of
        13, 24, 25: sgImgProperties.Cells[1,InfoRow] := 'RGB';
        32: sgImgProperties.Cells[1,InfoRow] := 'RGBA';
      else
        sgImgProperties.Cells[1,InfoRow] := 'Indexed';
      end;
    IncInfoRow;
    sgImgProperties.Cells[0,InfoRow] := 'Planes:';
    sgImgProperties.Cells[1,InfoRow] := IntToStr(ImgIffData.nPlanes); IncInfoRow;
    sgImgProperties.Cells[0,InfoRow] := 'Compression:';
    if ImgProperties.Compression = ctUnknown then
      case ImgIffData.CompressionType of
        2: sgImgProperties.Cells[1,InfoRow] := 'VDAT (Vertical RLE)';
        4: sgImgProperties.Cells[1,InfoRow] := 'RGB RLE';
      else
        sgImgProperties.Cells[1,InfoRow] := 'Unknown (' + IntToHex(ImgIffData.CompressionType, 2) + ')';
      end
    else
      sgImgProperties.Cells[1,InfoRow] := CCompression[ImgProperties.Compression];
    IncInfoRow;
    if ImgIffData.DummyByte <> 0 then begin
      sgImgProperties.Cells[0,InfoRow] := 'Dummy not zero:';
      sgImgProperties.Cells[1,InfoRow] := IntToHex(ImgIffData.DummyByte, 2);
      IncInfoRow;
    end;
    if ImgIffData.CamgFlags <> [] then begin
      sgImgProperties.Cells[0,InfoRow] := 'Flags:';
      Temp := IntToHex(Word(ImgIffData.CamgFlags), 4);
      if cfLace in ImgIffData.CamgFlags then begin
        if Temp <> '' then Temp := Temp + ', ';
        Temp := Temp + 'Lace';
      end;
      if cfExtraHalfBrite in ImgIffData.CamgFlags then begin
        if Temp <> '' then Temp := Temp + ', ';
        Temp := Temp + 'ExtraHalfBrite';
      end;
      if cfHam in ImgIffData.CamgFlags then begin
        if Temp <> '' then Temp := Temp + ', ';
        Temp := Temp + 'Ham';
      end;
      if cfExtendedMode in ImgIffData.CamgFlags then begin
        if Temp <> '' then Temp := Temp + ', ';
        Temp := Temp + 'ExtendedMode';
      end;
      if cfHiRes in ImgIffData.CamgFlags then begin
        if Temp <> '' then Temp := Temp + ', ';
        Temp := Temp + 'HiRes';
      end;
      sgImgProperties.Cells[1,InfoRow] := Temp; IncInfoRow;
    end;
    if ImgIffData.CamgHiFlags <> 0 then begin
      sgImgProperties.Cells[0,InfoRow] := 'Flags hi word:';
      sgImgProperties.Cells[1,InfoRow] := IntToHex(ImgIffData.CamgHiFlags, 4); IncInfoRow;
    end;
    sgImgProperties.Cells[0,InfoRow] := 'Palette entries:';
    sgImgProperties.Cells[1,InfoRow] := IntToStr(ImgIffData.PalSize); IncInfoRow;
    sgImgProperties.Cells[0,InfoRow] := 'Mask:';
    case ImgIffData.Mask of
      0: sgImgProperties.Cells[1,InfoRow] := 'None';
      1: sgImgProperties.Cells[1,InfoRow] := 'Has Mask';
      2: sgImgProperties.Cells[1,InfoRow] := 'Has Transparent Color';
      3: sgImgProperties.Cells[1,InfoRow] := 'Has Lasso';
    else
      sgImgProperties.Cells[1,InfoRow] := IntToHex(ImgIffData.Mask, 2);
    end;
    IncInfoRow;
    sgImgProperties.Cells[0,InfoRow] := 'Transparent Color:';
    sgImgProperties.Cells[1,InfoRow] := IntToHex(ImgIffData.TransparentColor, 4); IncInfoRow;
    sgImgProperties.Cells[0,InfoRow] := 'Aspect ratio:';
    sgImgProperties.Cells[1,InfoRow] := IntToStr(ImgIffData.xAspect) + ':' +
      IntToStr(ImgIffData.yAspect); IncInfoRow;
    if ImgIffData.ExtraChunks <> [] then begin
      sgImgProperties.Cells[0,InfoRow] := 'Extra chunks present';
      if idSham in ImgIffData.ExtraChunks then begin
        sgImgProperties.Cells[1,InfoRow] := 'Sliced Ham palette (SHAM), size: ' + IntToStr(ImgIffData.ShamSize);
        IncInfoRow;
      end;
      if idCtbl in ImgIffData.ExtraChunks then begin
        sgImgProperties.Cells[1,InfoRow] := 'Color table palette (CTBL), size: ' + IntToStr(ImgIffData.CtblSize);
        IncInfoRow;
      end;
      if idPchg in ImgIffData.ExtraChunks then begin
        sgImgProperties.Cells[1,InfoRow] := 'Extra palette (PCHG), size: ' + IntToStr(ImgIffData.PchgSize);
        IncInfoRow;
        // Show extra info about the PCHG header
        sgImgProperties.Cells[0,InfoRow] := 'PCHG Compression:';
        case ImgIffData.PchgHeader.Compression of
          PCHG_COMP_NONE: sgImgProperties.Cells[1,InfoRow] := 'None (uncompressed)';
          PCHG_COMP_HUFFMANN: sgImgProperties.Cells[1,InfoRow] := 'Huffman';
        else
          sgImgProperties.Cells[1,InfoRow] := 'Unknown compression type ' +
            IntToStr(ImgIffData.PchgHeader.Compression);
        end;
        IncInfoRow;
        sgImgProperties.Cells[0,InfoRow] := 'PCHG Flags:';
        case ImgIffData.PchgHeader.Flags of
          PCHGF_12BIT: sgImgProperties.Cells[1,InfoRow] := '12 bit';
          PCHGF_32BIT: sgImgProperties.Cells[1,InfoRow] := '32 bit';
          PCHGF_32BIT+PCHGF_USE_ALPHA: sgImgProperties.Cells[1,InfoRow] := '32 bit including alpha';
        else
          sgImgProperties.Cells[1,InfoRow] := 'unknown flags combination!';
        end;
        IncInfoRow;
        sgImgProperties.Cells[0,InfoRow] := 'PCHG LineCount:';
        sgImgProperties.Cells[1,InfoRow] := IntToStr(ImgIffData.PchgHeader.LineCount);
        IncInfoRow;
        sgImgProperties.Cells[0,InfoRow] := 'PCHG StartLine:';
        sgImgProperties.Cells[1,InfoRow] := IntToStr(ImgIffData.PchgHeader.StartLine);
        IncInfoRow;
        sgImgProperties.Cells[0,InfoRow] := 'PCHG ChangedLines:';
        sgImgProperties.Cells[1,InfoRow] := IntToStr(ImgIffData.PchgHeader.ChangedLines);
        IncInfoRow;
      end;
    end;
  end
  {$ENDIF}
  else begin
    sgImgProperties.Cells[0,InfoRow] := 'Color scheme:';
    if (ImgThumbData.ImageFormat = CgexTIFF) then begin
      // For now, since we can't get at the real tiff compression atm
      //sgImgProperties.Cells[1,InfoRow] := CGraphicEx2TiffCompression[ImgProperties.Compression];
      TempVal := FindStringIndex(@CKnownTiffColorSchemes, TIFF_COLOR_MAX,
        ImgTiffData.TiffPhotometric);
      if TempVal > -1 then
        sgImgProperties.Cells[1,InfoRow] := CColorScheme[ImgProperties.ColorScheme] +
          ' (Tiff: ' + CTiffColor[TempVal] + ')'
      else
        sgImgProperties.Cells[1,InfoRow] := 'Unknown Tiff image format ' + IntToStr(ImgTiffData.TiffPhotometric);
    end
    else if (ImgThumbData.ImageFormat = CgexPSD) then begin
      case ImgPsdMode of
        PSD_BITMAP: Temp := 'Bitmap (Black & White)';
        PSD_GRAYSCALE: Temp := 'Grayscale';
        PSD_INDEXED: Temp := 'Indexed';
        PSD_RGB: Temp := 'RGB';
        PSD_CMYK: Temp := 'CMYK';
        PSD_MULTICHANNEL: Temp := 'Multi Channel';
        PSD_DUOTONE: Temp := 'Duotone';
        PSD_LAB: Temp := 'LAB';
      else
        Temp := 'Unknown PSD color mode ' + IntToStr(ImgPsdMode);
      end;
      sgImgProperties.Cells[1,InfoRow] := CColorScheme[ImgProperties.ColorScheme] +
          ' (PSD Mode: ' + Temp + ')'
    end
    else
      sgImgProperties.Cells[1,InfoRow] := CColorScheme[ImgProperties.ColorScheme];
    IncInfoRow;
    if ImgProperties.HasAlpha then begin
      sgImgProperties.Cells[0,InfoRow] := 'Alpha channel present:';
      sgImgProperties.Cells[1,InfoRow] := 'True'; IncInfoRow;
    end;
    sgImgProperties.Cells[0,InfoRow] := 'Bits per pixel:';
    sgImgProperties.Cells[1,InfoRow] := IntToStr(ImgProperties.BitsPerPixel); IncInfoRow;
    sgImgProperties.Cells[0,InfoRow] := 'Bits per sample:';
    sgImgProperties.Cells[1,InfoRow] := IntToStr(ImgProperties.BitsPerSample); IncInfoRow;
    sgImgProperties.Cells[0,InfoRow] := 'Samples per pixel:';
    sgImgProperties.Cells[1,InfoRow] := IntToStr(ImgProperties.SamplesPerPixel); IncInfoRow;
    if (ImgThumbData.ImageFormat = CgexPNM) and (ImgProperties.MaxValue > 0) and
      (ImgProperties.MaxValue <> 255) and (ImgProperties.MaxValue <> 65535) then begin
      sgImgProperties.Cells[0,InfoRow] := 'PNM maximum sample value:';
      sgImgProperties.Cells[1,InfoRow] := IntToStr(ImgProperties.MaxValue) +
        ' (' + IntToStr(GraphicColor.GetBitsRequiredToStoreNumber(ImgProperties.MaxValue)) +
        ' bits per sample used)';
      IncInfoRow;
    end;
    if Round(ImgProperties.XResolution) <> 0.0 then begin
      sgImgProperties.Cells[0,InfoRow] := 'X Resolution:';
      sgImgProperties.Cells[1,InfoRow] := FloatToStr(ImgProperties.XResolution) + ' dpi'; IncInfoRow;
    end;
    if Round(ImgProperties.YResolution) <> 0.0 then begin
      sgImgProperties.Cells[0,InfoRow] := 'Y Resolution:';
      sgImgProperties.Cells[1,InfoRow] := FloatToStr(ImgProperties.YResolution) + ' dpi'; IncInfoRow;
    end;
    if (Round(ImgProperties.FileGamma) <> 0.0) or (ioUseGamma in ImgProperties.Options) then begin
      sgImgProperties.Cells[0,InfoRow] := 'Gamma:';
      sgImgProperties.Cells[1,InfoRow] := FloatToStr(ImgProperties.FileGamma); IncInfoRow;
    end;
    sgImgProperties.Cells[0,InfoRow] := 'Compression:';
    if (ImgThumbData.ImageFormat = CgexTIFF) then begin
      // For now, since we can't get at the real tiff compression atm
      //sgImgProperties.Cells[1,InfoRow] := CGraphicEx2TiffCompression[ImgProperties.Compression];
      TempVal := FindStringIndex(@CKnownTiffCompressionSchemes, TIFF_COMPRESSION_MAX,
        ImgTiffData.TiffCompression);
      if TempVal > -1 then
        sgImgProperties.Cells[1,InfoRow] := CCompression[ImgProperties.Compression] +
          ' (Tiff: ' + CTiffCompression[TempVal] + ')'
      else
        sgImgProperties.Cells[1,InfoRow] := 'Unknown Tiff compression type ' + IntToStr(ImgTiffData.TiffCompression);
    end
    else if (ImgThumbData.ImageFormat <> CgexBitmap) then
      sgImgProperties.Cells[1,InfoRow] := CCompression[ImgProperties.Compression]
    else
      sgImgProperties.Cells[1,InfoRow] := CBmpCompression[BmpCompression];
    IncInfoRow;
    if ImgProperties.ImageCount > 0 then begin
      sgImgProperties.Cells[0,InfoRow] := 'Number of images/pages:';
      sgImgProperties.Cells[1,InfoRow] := IntToStr(ImgProperties.ImageCount); IncInfoRow;
    end;
    if ImgProperties.Orientation <> gexoUnknown then begin
      sgImgProperties.Cells[0,InfoRow] := 'Orientation:';
      sgImgProperties.Cells[1,InfoRow] := COrientation[ImgProperties.Orientation];
      IncInfoRow;
    end;
    if ImgProperties.Interlaced then begin
      sgImgProperties.Cells[0,InfoRow] := 'Interlaced:';
      sgImgProperties.Cells[1,InfoRow] := 'Yes'; IncInfoRow;
    end;
    if ImgProperties.Options <> [] then begin
      sgImgProperties.Cells[0,InfoRow] := 'Image settings:';
      Temp := '';
      if ioTiled in ImgProperties.Options then
        Temp := Temp + 'Tiled';
      if ioBigEndian in ImgProperties.Options then begin
        if Temp <> '' then
          Temp := Temp + ', ';
        Temp := Temp + 'Big Endian';
      end;
      if ioMinIsWhite in ImgProperties.Options then begin
        if Temp <> '' then
          Temp := Temp + ', ';
        Temp := Temp + 'Min is White';
      end;
      if ioReversed in ImgProperties.Options then begin
        if Temp <> '' then
          Temp := Temp + ', ';
        Temp := Temp + 'Reversed bit order';
      end;
      if ioSeparatePlanes in ImgProperties.Options then begin
        if Temp <> '' then
          Temp := Temp + ', ';
        Temp := Temp + 'Separate Planes';
      end;
      if ioUseGamma in ImgProperties.Options then begin
        if Temp <> '' then
          Temp := Temp + ', ';
        Temp := Temp + 'Gamma correction used';
      end;
      sgImgProperties.Cells[1,InfoRow] := Temp; IncInfoRow;
    end;
    if ImgProperties.SampleFormat > 0 then begin
      sgImgProperties.Cells[0,InfoRow] := 'Data type of samples:';
      sgImgProperties.Cells[1,InfoRow] := CSampleFormat[ImgProperties.SampleFormat];
      IncInfoRow;
    end;
    // For formats that have layers:
    if (ImgLayerCount <> 0) or (ImgThumbData.ImageFormat = CgexPSD)then begin
      sgImgProperties.Cells[0,InfoRow] := 'Number of layers:';
      sgImgProperties.Cells[1,InfoRow] := IntToStr(ImgLayerCount);
      IncInfoRow;
    end;
    if (ImgThumbData.ImageFormat = CgexPSD) then begin
      sgImgProperties.Cells[0,InfoRow] := 'PSD Merged transparency:';
      if PsdMergedTransparencyPresent then
        sgImgProperties.Cells[1,InfoRow] := 'Present'
      else
        sgImgProperties.Cells[1,InfoRow] := 'Not Present';
      IncInfoRow;
    end;
    if (ImgIccProfile <> '') then begin
      sgImgProperties.Cells[0,InfoRow] := 'ICC profile:';
      sgImgProperties.Cells[1,InfoRow] := ImgIccProfile;
      IncInfoRow;
      if (ImgThumbData.ImageFormat = CgexPSD) then begin
        sgImgProperties.Cells[0,InfoRow] := 'ICC Untagged:';
        if PsdIccEnabled then
          sgImgProperties.Cells[1,InfoRow] := 'No'
        else
          sgImgProperties.Cells[1,InfoRow] := 'Yes';
        IncInfoRow;
      end;
    end;
    if (ImgThumbData.ImageFormat = CgexGIF) then begin
      if (ImgStrings.Count > 0) then begin
        sgImgProperties.Cells[0,InfoRow] := 'Application Extensions:';
        sgImgProperties.Cells[1,InfoRow] := ImgStrings.Text;//ImgStrings.GetText;
        IncInfoRow;
      end;
      sgImgProperties.Cells[0,InfoRow] := 'GIF Canvas size (w x h):';
      sgImgProperties.Cells[1,InfoRow] := Format('%u x %u',[GifInfo.CanvasWidth , GifInfo.CanvasHeight]); IncInfoRow;
      sgImgProperties.Cells[0,InfoRow] := 'GIF Frame position (left, top):';
      sgImgProperties.Cells[1,InfoRow] := Format('(%u, %u)',[GifInfo.FrameLeft , GifInfo.FrameTop]); IncInfoRow;
      sgImgProperties.Cells[0,InfoRow] := 'GIF Frame size (w x h):';
      sgImgProperties.Cells[1,InfoRow] := Format('%u x %u',[GifInfo.FrameWidth , GifInfo.FrameHeight]); IncInfoRow;
      if gfInterlaced in GifInfo.Flags then begin
        sgImgProperties.Cells[0,InfoRow] := 'GIF is';
        sgImgProperties.Cells[1,InfoRow] := 'Interlaced';
        IncInfoRow;
      end;
      if gfHasGlobalColorTable in GifInfo.Flags then begin
        sgImgProperties.Cells[0,InfoRow] := 'GIF has';
        sgImgProperties.Cells[1,InfoRow] := 'global color table';
        IncInfoRow;
      end;
      if gfHasLocalColorTable in GifInfo.Flags then begin
        sgImgProperties.Cells[0,InfoRow] := 'GIF has';
        sgImgProperties.Cells[1,InfoRow] := 'local color table';
        IncInfoRow;
      end;
      if gfHasTransparentColor in GifInfo.Flags then begin
        sgImgProperties.Cells[0,InfoRow] := 'GIF has';
        sgImgProperties.Cells[1,InfoRow] := Format('transparent color defined (index %d)', [GifInfo.TransparentColorIndex]);
        IncInfoRow;
      end;
      if gfHasGlobalColorTable in GifInfo.Flags then begin
        sgImgProperties.Cells[0,InfoRow] := 'GIF has';
        sgImgProperties.Cells[1,InfoRow] := Format('background color defined (index %d)', [GifInfo.BackgroundColorIndex]);
        IncInfoRow;
      end;
      if GifInfo.AspectRatio > 0 then begin
        TempFloat := (GifInfo.AspectRatio+15) / 64;
        sgImgProperties.Cells[0,InfoRow] := 'GIF Aspect Ratio:';
        sgImgProperties.Cells[1,InfoRow] := Format('%f', [TempFloat]);
        IncInfoRow;
      end;
      sgImgProperties.Cells[0,InfoRow] := 'GIF frame delay:';
      sgImgProperties.Cells[1,InfoRow] := Format('%d /100 s', [GifInfo.DelayTime]);
      IncInfoRow;
      sgImgProperties.Cells[0,InfoRow] := 'GIF frame disposal type:';
      case GifInfo.Disposal of
        gdfNoDisposal: sgImgProperties.Cells[1,InfoRow] := 'no disposal specified (no action required)';
        gdfDoNotDispose: sgImgProperties.Cells[1,InfoRow] := 'Do not dispose (leave image)';
        gdfRestoreBackgroundColor: sgImgProperties.Cells[1,InfoRow] := 'Restore to background color';
        gdfRestorePrevious: sgImgProperties.Cells[1,InfoRow] := 'Restore to previous frame image';
      end;
      IncInfoRow;
    end;
  end;
  // Show the actual PixelFormat
  sgImgProperties.Cells[0,InfoRow] := 'Converted PixelFormat:';
  sgImgProperties.Cells[1,InfoRow] := CPixelFormat[FPicture.Bitmap.PixelFormat];
  IncInfoRow;
  if (FPicture.Bitmap.Width <> ImgProperties.Width) or (FPicture.Bitmap.Height <> ImgProperties.Height) then begin
    sgImgProperties.Cells[0,InfoRow] := 'Converted Dimensions:';
    sgImgProperties.Cells[1,InfoRow] := Format('%u x %u',[FPicture.Bitmap.Width , FPicture.Bitmap.Height]); IncInfoRow;
    IncInfoRow;
  end;
end;

// Source: http://www.efg2.com/Lab/Library/UseNet/2000/0120a.txt
// There are 39.370079 In/Meter, so, dpi * 39.370079 = dots/meter (PelsPerMeter).
function PixelsPerMeterToDpi( ppm: Integer): Single;
begin
  Result := ppm * 39.370079;
end;

// http://en.wikipedia.org/wiki/BMP_file_format extra compression values:
// Note: BI_JPEG and BI_PNG are for printer drivers and are not supported when rendering to the screen.
const gex_BI_JPEG = 4; // The bitmap contains a JPEG image or RLE-24 compressed bitmap for BITMAPCOREHEADER2
      gex_BI_PNG  = 5; // The bitmap contains a PNG image
// Based in part on:
// http://www.efg2.com/Lab/Library/UseNet/2000/0527.txt
// Thread: http://www.delphigroups.info/2/a3/203992.html
// In case of pfDevice only the Compression parameter of dsbmih seems valid
// We seem to get pfDevice when reading a bmp thas is RLE encoded
// The BitsPerPixel will then be 32 although the real value could be different
// but we would have to read the actual file to get that info.
procedure TfrmViewer.GetBitmapInfo(ABitmap: TBitmap);
var
  DIB: TDIBSection;
  Err: Integer;
begin
  ImgRealPixelFormat := ABitmap.PixelFormat;
  Err := GetObject(ABitmap.Handle, SizeOf(DIB), @DIB);
  if Err = 0 then
    RaiseLastOSError;

  ImgProperties.Width := DIB.dsBm.bmWidth;
  ImgProperties.Height := DIB.dsBm.bmHeight;
  if ABitmap.PixelFormat <> pfDevice then begin
    BmpCompression := DIB.dsBmih.biCompression;
    case DIB.dsBmih.biCompression of
      BI_RGB: ImgProperties.Compression := ctNone;
      BI_RLE8:
        begin
          // This gets turned into pfDevice format by Delphi so we need to handle info ourselves
          ImgProperties.Compression := ctRLE;
          ImgProperties.BitsPerPixel := 8;
          ImgProperties.SamplesPerPixel := 1;
          ImgProperties.ColorScheme := csIndexed;
          ImgProperties.BitsPerSample := ImgProperties.BitsPerPixel div ImgProperties.SamplesPerPixel;
          ImgRealPixelFormat := pf8Bit;
        end;
      BI_RLE4:
        begin
          // This gets turned into pfDevice format by Delphi so we need to handle info ourselves
          ImgProperties.Compression := ctRLE;
          ImgProperties.BitsPerPixel := 4;
          ImgProperties.SamplesPerPixel := 1;
          ImgProperties.ColorScheme := csIndexed;
          ImgProperties.BitsPerSample := ImgProperties.BitsPerPixel div ImgProperties.SamplesPerPixel;
          ImgRealPixelFormat := pf4Bit;
        end;
      BI_BITFIELDS:
        // We currently have no way to identify this compression in GraphicEx
        ImgProperties.Compression := ctNone;
      gex_BI_JPEG: ImgProperties.Compression := ctJPEG;
      gex_BI_PNG: ImgProperties.Compression := ctLZ77;
    else
      //gex_BI_PNG: ImgProperties.Compression := ...;
      ImgProperties.Compression := ctUnknown;
      BmpCompression := 0; // Change illegal/unknown value or we will get a crash
    end;
    ImgProperties.BitsPerPixel := DIB.dsBm.bmBitsPixel;
    if DIB.dsBm.bmBitsPixel > 8 then begin
      if DIB.dsBm.bmBitsPixel > 16 then begin
        ImgProperties.SamplesPerPixel := DIB.dsBm.bmBitsPixel div 8;
        ImgProperties.BitsPerSample := 8;
        if ImgProperties.SamplesPerPixel = 3 then
          ImgRealPixelFormat := pf24Bit
        else // 4
          ImgRealPixelFormat := pf32Bit
      end
      else begin
        ImgProperties.SamplesPerPixel := 3;
        ImgProperties.BitsPerSample := 5;
        if DIB.dsBmih.biCompression = BI_BITFIELDS then begin
          // Need to determine if it's pf15bit or pf16bit
          if DIB.dsBitFields[1] = $3E0 then
            ImgRealPixelFormat := pf15Bit
          else
            ImgRealPixelFormat := pf16Bit
        end
        else
          ImgRealPixelFormat := pf16Bit;
      end;
      if DIB.dsBm.bmBitsPixel = 32 then
        ImgProperties.ColorScheme := csBGRA
      else
        ImgProperties.ColorScheme := csBGR
    end
    else begin
      ImgProperties.BitsPerSample := DIB.dsBm.bmBitsPixel;
      ImgProperties.SamplesPerPixel := 1;
      ImgProperties.ColorScheme := csIndexed;
      case ImgProperties.BitsPerSample of
        1: ImgRealPixelFormat := pf1Bit;
        4: ImgRealPixelFormat := pf4Bit;
        5: ImgRealPixelFormat := pf8Bit;
      else
        ImgRealPixelFormat := pfCustom;
      end;
    end;
    ImgProperties.XResolution := PixelsPerMeterToDpi(DIB.dsBmih.biXPelsPerMeter);
    ImgProperties.YResolution := PixelsPerMeterToDpi(DIB.dsBmih.biYPelsPerMeter);
  end
  else begin
    // pfDevice: This seems to happen with RLE encoded bitmaps.
    // Alas we can't get to know much about it unless we read the actual
    // headers from the file which we are not gonna do right now.
    ImgProperties.BitsPerPixel := DIB.dsBm.bmBitsPixel;
    ImgProperties.Compression := ctUnknown;
  end;
end;

// Source: http://www.efg2.com/Lab/Library/UseNet/2000/0527.txt
// Thread: http://www.delphigroups.info/2/a3/203992.html
function GetTruePixelFormat(ABitmap: TBitmap): TPixelFormat;
var
  DIB: TDIBSection;
  Err: Integer;
begin
  Result := ABitmap.PixelFormat;
  Err := GetObject(ABitmap.Handle, SizeOf(DIB), @DIB);
  if Err = 0 then
    RaiseLastOSError;

  with DIB, dsbmih do
    if biBitCount = 16 then
      if biCompression = BI_BITFIELDS then
        if dsBitFields[1] = $3E0 then
          Result := pf15Bit;
end;

procedure TfrmViewer.CopyBasicImageInfo(ABitmap: TBitmap);
begin
  // Show some basic info for images not descended from TGraphicExGraphic
  if ImgThumbData = nil then
    Exit;

  // Clear old info
  FillChar(ImgProperties, SizeOf(TImageProperties), 0);
  ImgProperties.Width := ABitmap.Width;
  ImgProperties.Height := ABitmap.Height;
  GetBitmapInfo(ABitmap);
  ImgProperties.XResolution := 0;
  ImgProperties.YResolution := 0;

end;

procedure TfrmViewer.CopyBasicImageInfo(APicture: TPicture);
begin
  if APicture.Graphic is TBitmap then
    CopyBasicImageInfo(APicture.Bitmap);
end;

procedure TfrmViewer.CopyImageInfo(AGraphic: TGraphicExGraphic);
begin
  // We expect AGraphic to be valid and just having read a page from the image or the whole image.
  // Therefore it's image properties should be valid and show the state of the current page.
  ImgProperties := AGraphic.ImageProperties;
  {$IFDEF LCMS}
  if Assigned(AGraphic.ICCManager) then
    ImgIccProfile := AGraphic.ICCManager.SourceProfileDescription
  else
  {$ENDIF}
    ImgIccProfile := ''; // This part outside the ifdef.
  if ImgThumbData <> nil then begin
    case ImgThumbData.ImageFormat of
      CgexTIFF: ImgTiffData := TTiffGraphic(AGraphic).ActualTiffData;
      CgexPSD:
        begin
          ImgPsdMode := TPSDGraphic(AGraphic).Mode;
          ImgLayerCount := TPSDGraphic(AGraphic).LayerCount;
          PsdMergedTransparencyPresent := TPSDGraphic(AGraphic).MergedTransparencyPresent;
          if TPSDGraphic(AGraphic).ImageProperties.BitsPerSample = 32 then
            ImgProperties.SampleFormat := SAMPLEFORMAT_IEEEFP;
          PsdIccEnabled := not TPSDGraphic(AGraphic).ICCUntagged;
        end;
      CgexGIF:
        begin
          ImgStrings.Assign(TGIFGraphic(AGraphic).ApplicationExtensions);
          GifInfo := TGIFGraphic(AGraphic).GifInformation;
        end;
      CgexPCX:
        PcxIsCapture := TPCXGraphic(AGraphic).ScreenCapture;
      {$IFDEF USE_AMIGAIFF}
      CgexAmigaIff: ImgIffData := TAmigaIffGraphic(AGraphic).IffProperties;
      {$ENDIF}
    end;
    lblThumb.Caption := Format('%s (%d x %d), type: %s, modified: %s',
      [ImgThumbData.Name, ImgProperties.Width , ImgProperties.Height,
      cFileTypeNames[ImgThumbData.ImageFormat], DateToStr(ImgThumbData.Modified)]);
    ShowImageInfo;
  end;
end;

procedure TfrmViewer.GetImageInfo(AGraphic: TGraphicExGraphic);
begin
  ImgPageCount := AGraphic.ImageProperties.ImageCount;
  if ImgPageCount = 0 then
    Inc(ImgPageCount);
  ImgComment := AGraphic.ImageProperties.Comment;
end;

procedure TfrmViewer.ImageGotoPage(PageNo: Integer; ForceReload: Boolean = False);
var
  AGraphic: TGraphic;
begin
  if ((PageNo <> ImgPage) or ForceReload) and (PageNo >= 0) and (PageNo < ImgPageCount) then begin
    FBlendTick := 0;
    FLoadTick := GetAccurateTick; // Starting time for loading
    ImgPage := PageNo;
    ClearGrid; // Clear grid with image info
    // We assume that all multipage images are imageformats that GraphicEx handles.
    // However in corrupt or broken images an image page may be invalid so we need
    // to check if ImgGraphicClass exists
    if Assigned(ImgGraphicClass) then begin
      AGraphic := ImgGraphicClass.Create;
      try
        // Load the desired page of current Image File
        TGraphicExGraphic(AGraphic).LoadFromFileByIndex(ImgFile, ImgPage);
        // Assign loaded graphic to image
        FPicture.Assign(AGraphic);
      finally
        // Get page specific image info
        // Do this in the finally, that way we can even show some image info
        // when the image is corrupt.
        CopyImageInfo(TGraphicExGraphic(AGraphic));
        AGraphic.Free;
      end;
    end
    else begin
      // TODO: Show error in case it was NOT assigned and clear old image
      FPicture.Assign(nil);
    end;
    FLoadTick := GetAccurateTick - FLoadTick;
    // Update status text and buttons
    UpdateImageStatus;
    UpdatePageButtons;
    ShowErrors;
    HandleStretch;
    pb2.Invalidate;
    UpdateLoadingStatus;
  end;
end;

procedure TfrmViewer.UpdatePaintBoxSize;
begin
  if cbStretch.Checked then begin
    {$IFNDEF FPC}
    pnlScroll.Width   := pnlImageContainer.ClientWidth;
    pnlScroll.Height  := pnlImageContainer.ClientHeight;
    {$ELSE}
    pnlScroll.Width   := sbx1.ClientWidth;
    pnlScroll.Height  := sbx1.ClientHeight;
    {$ENDIF}
  end
  else begin
    if FPicture = nil then
      Exit; // Can happen if we arrive here very early at startup
    if FPicture.Width <= sbx1.ClientWidth then begin
      pnlScroll.Width := sbx1.ClientWidth;
    end
    else begin
      pnlScroll.Width := FPicture.Width;
    end;
    if FPicture.Height <= sbx1.ClientHeight then begin
      pnlScroll.Height := sbx1.ClientHeight;
    end
    else begin
      pnlScroll.Height := FPicture.Height;
    end;
  end;
end;

procedure TfrmViewer.LoadImage(Thumb: PgexThumbData);
var
  AGraphic: TGraphic;
  GraphicClass: TGraphicExGraphicClass;
//  jpgImg: TJpegImage;
//  bmpimg: TBitmap;
begin
  ImgFile := ImageFolder + Thumb.Name;
  // Reset image characteristics
  ImgGraphicClass := nil;
  ImgPage := 0; ImgPageCount := 1;
  ImgComment := '';
  ImgThumbData := Thumb;
  ImgStrings.Clear;
  {$IFDEF FPC}
  ImgRealPixelFormat := pfCustom;
  {$ENDIF}

  FLoadTick := GetAccurateTick; // Starting time for loading
  FBlendTick := 0;
  try

    ClearGrid; // Clear grid with image info

    CollectErrors := True;
    // To be able to handle situations where the file extension differs from
    // the actual file format (e.g. jpeg with tiff extension, or bmp without extension)
    // We cannot use Picture.LoadFromFile because that uses extensions to
    // determine which image type to load.
    case Thumb.ImageFormat of
      CgexBitmap:
        begin
          // To be able to load Bitmap files without extension or with another extension
          // than .bmp we will explicitly load a bitmap and then assign it to FPicture.
          {bmpImg := TBitmap.Create;
          try
            bmpImg.LoadFromFile(ImgFile);
            CopyBasicImageInfo(bmpImg);
            FPicture.Assign(bmpImg);
            if (FPicture.Bitmap.PixelFormat = pf32Bit) then
              // TODO: We should also test if there are any (partially) transparent
              // pixels in the bitmap. Only set Alpha to 255 if there are no transparent pixels!
              // The alpha component can be set to 0 making the image invisible, change this to all opaque
              BitmapSetAlpha255(FPicture.Bitmap);
          finally
            bmpImg.Free;
          end;}

          // Now using our GraphicEx Bmp wrapper.
          AGraphic := TgexBmpGraphic.Create;
          try
            TGraphicExGraphic(AGraphic).ReadImageProperties(ImgFile, ImgPage);
            // Now load the first page of our image
            TGraphicExGraphic(AGraphic).LoadFromFileByIndex(ImgFile, ImgPage);
            // Get some basic image info
            // TODO: Enhance our bmp wrapper to get all bmp image properties from
            // ReadImageProperties, but for now:
            CopyBasicImageInfo(TBitmap(AGraphic));
            ImgGraphicClass := TgexBmpGraphic;
            FPicture.Assign(AGraphic);
            // ShowImageInfo needs to be called AFTER assigning bitmap to FPicture.
            ShowImageInfo;
            if (FPicture.Bitmap.PixelFormat = pf32Bit) then
              // TODO: We should also test if there are any (partially) transparent
              // pixels in the bitmap. Only set Alpha to 255 if there are no transparent pixels!
              // The alpha component can be set to 0 making the image invisible, change this to all opaque
              BitmapSetAlpha255(FPicture.Bitmap);
          finally
            AGraphic.Free;
          end;

        end;
      CgexJpeg:
        begin
  {
          // Since certain jpegs (e.g. CMYK colorspace) need extra handling we
          // don't use FPicture.LoadFromFile.
          // Although this version is not used anymore here, we leave it in as an example.
          jpgImg := TJpegImage.Create();
          try
            jpgImg.Scale := jsFullSize;
            jpgImg.Performance := jpBestQuality;
            jpgImg.LoadFromFile(ImgFile);
            FPicture.Bitmap.Assign(jpgImg);
            // For CMYK jpeg's (as implemented by Gabriel Corneanu, http://cc.embarcadero.com/Item/19723)
            // we apparently need to explicitly set PixelFormat to pf24Bit.
            FPicture.Bitmap.PixelFormat := pf24Bit;
            CopyBasicImageInfo(FPicture);
          finally
            jpgImg.Free;
          end;
  }
          {$IFDEF USE_LIBJPEG}
          // Use our GraphicEx LibJpeg imple
          AGraphic := TgexJpegImage.Create;
          try
            // Get a scaled down version of a very large image
            TgexJpegImage(AGraphic).AutoScaleLargeImage := True;
            // Now load the first page of our image
            TGraphicExGraphic(AGraphic).LoadFromFileByIndex(ImgFile, ImgPage);
            {$IFDEF JPEG_MEASURE_SPEED}
            // testing speed of jpeg reading
            lblStatus.Caption := Format('LibJpeg: %d, ColorConversion %d',
              [TgexJpegImage(AGraphic).LibJpegTicks, TgexJpegImage(AGraphic).ConversionTicks]);
            {$ENDIF}
            ImgGraphicClass := TgexJpegImage;
//          FPicture.Assign(AGraphic);
          finally
            // Assign jpeg even in case of an error: we may be able to show
            // a partial image.
            // Note: we only do this because we are an image viewer. Normal usage
            // would be to assign it only if there was no exception/error.
            if (AGraphic <> nil) and (AGraphic.Width > 0) and (AGraphic.Height > 0) then
              FPicture.Assign(AGraphic);
            // Get some basic image info
            GetImageInfo(TGraphicExGraphic(AGraphic));
            CopyImageInfo(TGraphicExGraphic(AGraphic));
            AGraphic.Free;
          end;
          {$ELSE}
          // Now using our GraphicEx Jpeg wrapper.
          AGraphic := TgexJpegGraphic.Create;
          try
            // Now load the first page of our image
            TGraphicExGraphic(AGraphic).LoadFromFileByIndex(ImgFile, ImgPage);
            ImgGraphicClass := TgexJpegGraphic;
            // Get some basic image info
            GetImageInfo(TGraphicExGraphic(AGraphic));
            CopyImageInfo(TGraphicExGraphic(AGraphic));
            FPicture.Assign(AGraphic);
          finally
            AGraphic.Free;
          end;
          {$ENDIF}

        end;
    else
      if Thumb.ImageData <> nil then begin
        // ImageFormat could even be CgexUnknown if it's an image format we know
        // but we don't have explicitly determined
        GraphicClass := Thumb.ImageData;
        AGraphic := GraphicClass.Create;
        AGraphic.OnProgress := ImageLoadProgress;
        try
          if Thumb.ImageFormat = CgexPcd then
            // Set starting page for PCD to 3
            ImgPage := 2; // 0 based third page

          // Now load the first page of our image
          TGraphicExGraphic(AGraphic).LoadFromFileByIndex(ImgFile, ImgPage);
          ImgGraphicClass := GraphicClass;
//        FPicture.Assign(AGraphic);
        finally
          // Assign image even in case of an error: we may be able to show
          // a partial image.
          // Note: we only do this because we are an image viewer. Normal usage
          // would be to assign it only if there was no exception/error.
          if (AGraphic <> nil) and (AGraphic.Width > 0) and (AGraphic.Height > 0) then
            FPicture.Assign(AGraphic)
          else
            FPicture.Assign(nil);
          // Get some basic image info
          // Do this in the finally, that way we can even show some image info
          // when the image is corrupt.
          GetImageInfo(TGraphicExGraphic(AGraphic));
          CopyImageInfo(TGraphicExGraphic(AGraphic));
          AGraphic.Free;
        end;
      end
      else if Thumb.ImageFormat <> CgexUnknown then begin
        // Try to load image using Picture.LoadFromFile as a last resort.
        FPicture.LoadFromFile(ImgFile);
      end
      else begin
        FPicture.Graphic := nil;
        ImgPageCount := 0;
        pb2.Invalidate;
      end;
    end;
    CollectErrors := False;
  finally
    // Update info and errors even if we got an exception reading the image
    FLoadTick := GetAccurateTick - FLoadTick;
    UpdateImageStatus;
    UpdatePageButtons;
    ShowErrors;
    // Even images with errors may be able to show some partial result
    if CollectErrors then
      pb2.Invalidate;
  end;

  if ImgPageCount = 0 then  // Testing for img.Picture = nil doesn't work!
    Exit; // Can't stretch if there is no image
  HandleStretch;
  pb2.Invalidate;
  UpdateLoadingStatus;
end;

procedure TfrmViewer.HandleStretch;
var
  StretchW, StretchH: Integer;
  MulW, MulH: Single;
  {$IFDEF FPC}
  TempBmp: TBitmap;
  {$ENDIF}
begin
  UpdatePaintBoxSize;
  // OPTIONAL Stretch picture to fit in window (and beware of invalid image dimensions)
  if cbStretch.Checked and (FPicture.Bitmap.Width > 0) and (FPicture.Bitmap.Height > 0) then begin
    FStretchTick := GetAccurateTick;
    // Our stretch function only works on 24 and 32 bits
    // Therefore we need to convert other formats, we choose to convert to 24 bits
    if not (FPicture.Bitmap.PixelFormat in [pf24Bit, pf32Bit]) then begin
      // Unless we know its a format with alpha channel its best to convert to 24 bits
      {$IFNDEF FPC}
      FPicture.Bitmap.PixelFormat := pf24Bit;
      {$ELSE}
      // Just changing PixelFormat in fpc doesn't work since it currently
      // doesn't do any automatic conversion. Since most formats are already
      // converted  by us to 24/32 bit we only need to handle 15/16 bit bitmaps here.
      TempBmp := TBitmap.Create;
      try
        TempBmp.SetSize(FPicture.Bitmap.Width, FPicture.Bitmap.Height);
        TempBmp.PixelFormat := pf24Bit;
        TempBmp.Canvas.Draw(0,0, FPicture.Bitmap);
        FPicture.Assign(TempBmp);
      finally
        TempBmp.Free;
      end;
      {$ENDIF}
{     // In case we want PixelFormat pf32Bit:
      FPicture.Bitmap.PixelFormat := pf32Bit;
      // Changing to 32 bits usually sets the alpha channel to 0 (invisible).
      // We need to change that to opaque (255).
      BitmapSetAlpha255(FPicture.Bitmap);
}
    end;
    // Compute stretch width and height
    StretchW := pb2.Width;
    StretchH := pb2.Height;
    MulW := StretchW / FPicture.Bitmap.Width;
    MulH := StretchH / FPicture.Bitmap.Height;
    if MulW > MulH then begin
      StretchW := Trunc(FPicture.Bitmap.Width * MulH);
      if StretchW = 0 then StretchW := 1;
    end
    else begin
      StretchH := Trunc(FPicture.Bitmap.Height * MulW);
      if StretchH = 0 then StretchH := 1;
    end;
    Stretch(StretchW, StretchH, TResamplingFilter(cbStretchFilter.ItemIndex), 0, FPicture.Bitmap);
    FStretchTick := GetAccurateTick - FStretchTick;
  end
  else
    FStretchTick := 0;
end;

procedure TfrmViewer.ThumbViewClick(Sender: TObject);
var
  Thumb: PgexThumbData;
begin
  if rkView1.Selected <> -1 then
  begin
    Thumb := Items[rkView1.Items[rkView1.Selected]];
    LoadImage(Thumb);
  end;
end;

function TfrmViewer.DetermineImageFormat( const FileName: string;
        var ImageData: Pointer): TImageFileFormat;
var
  GraphicClass: TGraphicExGraphicClass;
  SecondCaseClass: TGraphicClass;
  Ext: string;
begin
  // inherited can determine bmp and jpeg
  // TODO: Maybe replace with our own determination for those too!
  Result := inherited DetermineImageFormat(FileName, ImageData);
  if Result = CgexUnknown then begin
    // Determine true file type from content rather than extension.
    GraphicClass := FileFormatList.GraphicFromContent(FileName);
    if GraphicClass = nil then
    begin
      // Some formats (e.g. Dr. Halo CUT images) cannot be determined from content.
      // Try to guess based on extension.
      // Problem with this is, that above we may have seen an image with our
      // image extension but determined in GraphicFromContent that it's not an
      // image (sub)format we recognize. This image will then be recognized here again
      // even though we won't be able to use it.
      // Since currently the only GraphicEx format we have that can't be determined
      // from content is CUT, we explicitly check for that extension
      Ext := LowerCase(ExtractFileExt(FileName));
      if Ext = '.cut' then begin
        SecondCaseClass := FileFormatList.GraphicFromExtension(FileName);
        if (SecondCaseClass <> nil) and SecondCaseClass.InheritsFrom(TGraphicExGraphic) then
          GraphicClass := TGraphicExGraphicClass(SecondCaseClass);
      end;
    end;
    if GraphicClass <> nil then
    begin
//      if GraphicClass = TJpegGraphic then
//        Result := CgexJpeg
      if GraphicClass = TPngGraphic then
        Result := CgexPng
      else if GraphicClass = TGifGraphic then
        Result := CgexGif
//      if GraphicClass = TBmpGraphic then
//        Result := CgexBitmap
      else if GraphicClass = TTiffGraphic then
        Result := CgexTiff
      else if GraphicClass = TTargaGraphic then
        Result := CgexTga
      else if GraphicClass = TPcdGraphic then
        Result := CgexPcd
      else if GraphicClass = TPsdGraphic then
        Result := CgexPsd
      else if GraphicClass = TPspGraphic then
        Result := CgexPsp
      else if GraphicClass = TPPMGraphic then
        Result := CgexPnm
      else if GraphicClass = TPcxGraphic then
        Result := CgexPcx
      else if GraphicClass = TRlaGraphic then
        Result := CgexRla
      else if GraphicClass = TSgiGraphic then
        Result := CgexSgi
      else if GraphicClass = TAutodeskGraphic then
        Result := CgexAutodesk
      else if GraphicClass = TCUTGraphic then
        Result := CgexCUT
      else if GraphicClass = TGEDGraphic then
        Result := CgexGED
      else if GraphicClass = TEpsGraphic then
        Result := CgexEPS
{$IFDEF USE_XCF}
      else if GraphicClass = TXcfGraphic then
        Result := CgexXcf
{$ENDIF}
{$IFDEF USE_MAYAIFF}
      else if GraphicClass = TMayaIffGraphic then
        Result := CgexMayaIff
{$ENDIF}
{$IFDEF USE_AMIGAIFF}
      else if GraphicClass = TAmigaIffGraphic then
        Result := CgexAmigaIff
{$ENDIF}
    end;
    ImageData := GraphicClass;
  end;
end;

function TfrmViewer.ConvertImageToThumb(const FileName: string; AThumb: PgexThumbData): TBitmap;
var
  AGraphic: TGraphic;
  GraphicClass: TGraphicExGraphicClass;
  LoadingFailed: Boolean;
begin
  Result := nil;
  // Get the type of image stored with the thumbnail data
  GraphicClass := AThumb.ImageData;
  if GraphicClass = nil then
    Exit;
  AGraphic := GraphicClass.Create;
  LoadingFailed := False;
  try
    try
      TGraphicExGraphic(AGraphic).LoadFromFileByIndex(FileName, 0);
      if AGraphic.Empty then
        LoadingFailed := True;
      if LoadingFailed then begin
{ Commented out this part since we can have a valid image that is empty: e.g. GIF without an image wxh 0x0
//        AThumb.ImageFormat := CgexUnknown;  (commented out
//        if AThumb.ImageData <> nil then begin
//          AThumb.ImageData := nil;
//        end;
}
        if TiffError <> '' then begin
          // Silent no exception tiff error
          lblStatus.Caption := TiffError;
          TiffError := '';
        end
{$IFDEF USE_XCF}
        else if GraphicClass = TXcfGraphic then begin
          if TXcfGraphic(AGraphic).LastError <> '' then
            lblStatus.Caption := TXcfGraphic(AGraphic).LastError
          else if TXcfGraphic(AGraphic).LastWarning <> '' then
            lblStatus.Caption := TXcfGraphic(AGraphic).LastWarning
          else
            lblStatus.Caption := 'XCF: Unknown error!';
        end
{$ENDIF}
      end;
    except
      // We don't want Exceptions caused by invalid or unsupported image (sub)formats
      // to pop up a Message every time. We will therefore only show a status message
      // for exceptions that we recognize.
      // When using GraphicEx for other purposes than an Image Viewer you should
      // usually be more conservative with eating all exceptions.
      on e:EgexInvalidGraphic do begin
        LoadingFailed := True;
        lblStatus.Caption := 'Error loading image: ' + FileName +#13#10 +
          e.Message;
          ErrorList.AddObject(lblStatus.Caption, TObject(-3));
      end;
      on e:EgexColorConversionError do begin
        LoadingFailed := True;
        lblStatus.Caption := 'Color conversion error loading image: ' + FileName +
          #13#10 + e.Message;
        ErrorList.AddObject(lblStatus.Caption, TObject(-3));
      end;
      on e:EOutOfMemory do begin
        LoadingFailed := True;
        lblStatus.Caption := 'Not enough free memory to load image: ' + FileName;
        ErrorList.AddObject(lblStatus.Caption, TObject(-3));
      end;
      on e:EOutOfResources do begin
        LoadingFailed := True;
        lblStatus.Caption := 'Not enough free resources to load image: ' + FileName;
        ErrorList.AddObject(lblStatus.Caption, TObject(-3));
      end;
      on e:EDivByZero do begin
        LoadingFailed := True;
        lblStatus.Caption := 'Division by Zero during loading of image: ' + FileName;
        ErrorList.AddObject(lblStatus.Caption, TObject(-3));
      end;
      else begin
        ExceptionMessage := 'Unknown error loading image: ' + FileName;
        ErrorList.AddObject(lblStatus.Caption, TObject(-3));
        raise;
      end;
    end;
    // This draws the full image to a Bitmap and then makes a
    // thumbnail image in the required size for it
    if not LoadingFailed then
      Result := CreateThumbnail(AGraphic.Width, AGraphic.Height, AGraphic, AThumb);
  finally
    AGraphic.Free;
  end;
end;

procedure TfrmViewer.rkView1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  i: Integer;
  Thumb: PgexThumbData;
begin
  i := rkView1.ItemAtXY(Point(X, Y), False);
  if i <> -1 then
  begin
    Thumb := Items[rkView1.Items[i]];
    if (Thumb.ImageFormat > CgexUnknown) then
      lblThumb.Caption := Format('%s (%d x %d), type: %s, modified: %s',
        [Thumb.Name,Thumb.IWidth,Thumb.IHeight,cFileTypeNames[Thumb.ImageFormat],DateToStr(Thumb.Modified)])
    else
      lblThumb.Caption := Format('%s, type: %s, modified: %s',
        [Thumb.Name,cFileTypeNames[Thumb.ImageFormat],DateToStr(Thumb.Modified)]);
  end
  else
    lblThumb.Caption := '';
end;

procedure TfrmViewer.spbtnClick(Sender: TObject);
begin
  if TSpeedButton(Sender).Name = 'spbtnFirst' then begin
    ImageGotoPage(0);  // 0 based page numbers
  end
  else if TSpeedButton(Sender).Name = 'spbtnPrev' then begin
    ImageGotoPage(ImgPage-1);
  end
  else if TSpeedButton(Sender).Name = 'spbtnNext' then begin
    ImageGotoPage(ImgPage+1);
  end
  else if TSpeedButton(Sender).Name = 'spbtnLast' then begin
    ImageGotoPage(ImgPageCount-1);
  end;
end;

procedure TfrmViewer.DoUpdateThumbnailSize(ASize: Integer);
begin
  // We don't want to fire an endless loop of updates thus temporarily disable OnChange
  tbSize.OnChange := nil;
  tbSize.Position := ASize;
  tbSize.OnChange := tbSizeChange;
end;

procedure TfrmViewer.tbSizeChange(Sender: TObject);
begin
  SetThumbSize(tbSize.Position, False);
  tbSize.Hint := Format('Thumbnail size: %d', [tbSize.Position]);
end;

procedure TfrmViewer.rkView1Selecting(Sender: TObject; Count: Integer);
begin
  UpdateStatus;
end;

procedure TfrmViewer.rkView1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  i: Integer;
  Thumb: PgexThumbData;
  aPoint: TPoint;
begin
  i := rkView1.ItemAtXY(Point(X, Y), False);
  if i <> -1 then
  begin
    Thumb := Items[rkView1.Items[i]];
    rkView1.Hint := Format('%s (size: %s)', [Thumb.Name, BytesToStr(Thumb.Size)]);
    aPoint.X := X;
    aPoint.Y := Y;
    aPoint := rkView1.ClientToScreen(aPoint);
    Application.ActivateHint(aPoint);
  end
  else
    rkView1.Hint := '';
end;

// Creates the checkered default background for an entry.
procedure TfrmViewer.CreateDefaultBackground;
begin
  FThumbnailBackground := TBitmap.Create;
  with FThumbnailBackground do
  begin
    Width := 16;
    Height := 16;
    UpdateDefaultBackground;
  end;
end;

procedure TfrmViewer.UpdateDefaultBackground;
begin
  with FThumbnailBackground do
  begin
    Canvas.Brush.Color := FCheckerboardColor1;
    Canvas.FillRect(Rect(0, 0, Width, Height));
    Canvas.Brush.Color := FCheckerboardColor2;
    Canvas.FillRect(Rect(0, 0, 8, 8));
    Canvas.FillRect(Rect(8, 8, 16, 16));
  end;
end;

// Tiles the background image over the given target bitmap.
procedure TfrmViewer.FillBackground(R: TRect; Target: TCanvas);
var
  X, Y: Integer;
  dX, dY: Integer;
begin
  with Target do
  begin
    dX := FThumbnailBackground.Width;
    dY := FThumbnailBackground.Height;

    Y := 0;
    while Y < R.Bottom - R.Top do
    begin
      X := 0;
      while X < R.Right - R.Left do
      begin
        Draw(X, Y, FThumbnailBackground);
        Inc(X, dX);
      end;
      Inc(Y, dY);
    end;
  end;
end;

procedure TfrmViewer.WMEraseBkgnd(var Msg: TWMEraseBkgnd);
begin
  Msg.Result := 1;
end;

{$IFNDEF FPC}
procedure TfrmViewer.ImageLoadProgress(Sender: TObject; Stage: TProgressStage; PercentDone: Byte; RedrawNow: Boolean;
  const R: TRect; const Msg: string);
{$ELSE}
procedure TfrmViewer.ImageLoadProgress(Sender: TObject; Stage: TProgressStage; PercentDone: Byte; RedrawNow: Boolean;
  const R: TRect; const Msg: string; var Continue : Boolean);
{$ENDIF}
begin
  case Stage of
    psStarting:
      begin
        pbProgress.Position := 0;
        pbProgress.Show;
      end;
    psEnding:
      begin
        pbProgress.Position := PercentDone;
        pbProgress.Update;
        pbProgress.Hide;
      end;
    psRunning:
      begin
        pbProgress.Position := PercentDone;
        pbProgress.Update;
        Application.ProcessMessages;
      end;
  end;
end;

// We want a little more accuracy than GetTickCount
function TfrmViewer.GetAccurateTick: Int64;
var st: _SYSTEMTIME;
    ft: TFileTime;
begin
  GetLocalTime(st);
  SystemTimeToFileTime(st, ft);
  Result := Int64(ft) div 10000;
end;

procedure TfrmViewer.UpdateLoadingStatus;
begin
  if cbStretch.Checked then
    lblLoadTime.Caption := 'Load time: ' + IntToStr(FLoadTick) + ' ms. Stretch time: ' +
      IntToStr(FStretchTick) + ' ms.'
  else
    lblLoadTime.Caption := 'Load time: ' + IntToStr(FLoadTick) + 'ms.';
  if FBlendTick > 0 then begin
    lblLoadTime.Caption := lblLoadTime.Caption + ' Blend time: ' + IntToStr(FBlendTick) + ' ms.';
  end;
end;

procedure TfrmViewer.sgImgPropertiesSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
begin
  if ACol < 2 then
    CanSelect := False;
end;

procedure TfrmViewer.sgImgPropertiesClick(Sender: TObject);
begin
  // ignore clicks
end;

procedure TfrmViewer.sgImgPropertiesMouseUpDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  // ignore
end;

procedure TfrmViewer.sgImgPropertiesMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var NewMousePos: TPoint;
    Col, Row: Integer;
begin
  sgImgProperties.MouseToCell(X, Y, Col, Row);
  if (Col >= 0) and (Row >= 0) then
    sgImgProperties.Hint := sgImgProperties.Cells[Col, Row]
  else
    sgImgProperties.Hint := '';
  NewMousePos.X := X;
  NewMousePos.Y := Y;
  NewMousePos := sgImgProperties.ClientToScreen(NewMousePos);
  Application.ActivateHint(NewMousePos);
end;

procedure TfrmViewer.FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  if PtInRect(pb2.BoundsRect,pb2.ScreenToClient(MousePos)) then
  begin
    if (ssctrl in Shift) then
      // TODO: Zoom in
    else if Shift = [] then
    begin
      sbx1.VertScrollBar.Position := sbx1.VertScrollBar.Position - sbx1.VertScrollBar.Increment;
    end;
    Handled := True;
  end;
end;

procedure TfrmViewer.FormMouseWheelDown(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  if PtInRect(pb2.BoundsRect,pb2.ScreenToClient(MousePos)) then
  begin
    if (ssctrl in Shift) then
      // TODO: Zoom in
    else if Shift = [] then
    begin
      sbx1.VertScrollBar.Position := sbx1.VertScrollBar.Position + sbx1.VertScrollBar.Increment;
    end;
    Handled := True;
  end;
end;

procedure TfrmViewer.pb2MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FLastX := X;
  FLastY := Y;
  FCapturing := True;
  // Should we be using Dragging instead?
end;

procedure TfrmViewer.pb2MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  HintPoint: TPoint;
  C: TRGBA;
begin
  if FCapturing then begin
    if (FLastX <> X) and (FLastY <> Y) then begin
      // TODO: This doesn't seem to be going right: jumps to top/bottom, left/right border
      sbx1.VertScrollBar.Position := sbx1.VertScrollBar.Position + (FLastY-Y);
      sbx1.HorzScrollBar.Position := sbx1.HorzScrollBar.Position + (FLastX-X);
      FLastX := X;
      FLastY := Y;
    end;
  end
  else begin
    // Show pixel info hint
    C := TRGBA(pb2.Canvas.Pixels[X,Y]);
    // Since we are using the pixels from the canvas the alpha will always be 0
    pb2.Hint := Format('RGB: %d, %d, %d (hex: %x, %x, %x)', [C.R, C.G, C.B, C.R, C.G, C.B]);
    HintPoint := pb2.ClientToScreen(Point(X,Y));
    Application.ActivateHint(HintPoint);
  end;
end;

procedure TfrmViewer.pb2MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FCapturing := False;
end;

procedure TfrmViewer.ShowErrors;
var i: Integer;
  MaxErr: Integer;
begin
  if ErrorList.Count > 0 then begin
    // 1 empty row between image info and image loading errors
    IncInfoRow;
    MaxErr := ErrorList.Count;
    // Very large amounts of errors can make it seem like we are in a endless
    // loop since adding huge amounts of rows to the grid will slow things down
    // and use a lot of memory. More than a 1000 doesn't seem useful to look at.
    if MaxErr > 1000 then
      MaxErr := 1000;
    for i := 0 to MaxErr-1 do begin
      case NativeInt(ErrorList.Objects[i]) of
        -2: sgImgProperties.Cells[0,InfoRow] := 'Image loading error:';
        -1: sgImgProperties.Cells[0,InfoRow] := 'Image loading warning:';
        -3: sgImgProperties.Cells[0,InfoRow] := 'Exception during image loading:';
      else
        {0..7:} sgImgProperties.Cells[0,InfoRow] := 'Image info:';
      end;
      sgImgProperties.Cells[1,InfoRow] := ErrorList.Strings[i];
      IncInfoRow;
    end;
    ErrorList.Clear;
  end;
end;

procedure TfrmViewer.FormResize(Sender: TObject);
begin
  // Form is being resized
  sgImgProperties.ColWidths[1] := sgImgProperties.ClientWidth -
    sgImgProperties.ColWidths[0];
  UpdatePaintBoxSize;
end;

procedure TfrmViewer.Splitter2Moved(Sender: TObject);
begin
  // Panel is being resized
  sgImgProperties.ColWidths[1] := sgImgProperties.ClientWidth -
    sgImgProperties.ColWidths[0];
end;

procedure TfrmViewer.sgImgPropertiesMouseWheelDown(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  Handled := True;
end;

procedure TfrmViewer.sgImgPropertiesMouseWheelUp(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  Handled := True;
end;

procedure TfrmViewer.cbBackgroundChange(Sender: TObject);
begin
  if cbBackground.ItemIndex = 1 then begin
    FCheckerboardColor1 := Light_bgColor1;
    FCheckerboardColor2 := Light_bgColor2;
  end
  else begin
    FCheckerboardColor1 := Dark_bgColor1;
    FCheckerboardColor2 := Dark_bgColor2;
  end;
  UpdateDefaultBackground;
  HandleStretch;
  pb2.Invalidate;
  UpdateLoadingStatus;
end;

procedure TfrmViewer.cbStretchChange(Sender: TObject);
begin
  if cbStretch.Checked then begin
    HandleStretch;
    pb2.Invalidate;
    UpdateLoadingStatus;
  end
  else begin
    // Since stretching is done on the actual image we will have to reload
    // the image if we uncheck the stretch checkbox
    ImageGotoPage(ImgPage, True);
  end;
end;

procedure TfrmViewer.cbStretchFilterChange(Sender: TObject);
begin
  if not cbStretch.Checked then
    Exit;
  // Since stretching is done on the actual image we will have to reload
  // the image if we uncheck the stretch checkbox
  ImageGotoPage(ImgPage, True);
end;

initialization
  CollectErrors := False;
  ErrorList := TStringList.Create;
  ErrorList.Capacity := 64;
finalization
  ErrorList.Free;
end.
////////////////////////////////////////////////////////////////////////////////
// TODO:
// - Don't stretch the loaded image itself but a copy when have stretch checked.
// - Exception reading image in folder thumbnail creator should not stop
//   reading the rest of the images in the same folder.
// ==> This needs a change in GraphicEx, we need to have a setting where we can
//     enable silent fails instead of exception!
// - Add Windows AlphaBlend function as alternate blend.
// - Limit size of img for reading thumbnails? (ignore very large images because
//   they might take a long time to get a thumnbnail)
// - Themed scrollbar painting, see removed thumbnail component for how it paints scrollbar
// + not painting transparent background of transparent images --> because we
//   convert thumbnails to jpeg and use 24bits to convert to thumbnail!
////////////////////////////////////////////////////////////////////////////////

