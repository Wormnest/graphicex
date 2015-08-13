{ gexThread A Threaded thumbnail creator based on R.M. Klever's Threaded ThumbNail Demo.
  License: MPL 1.1.
  Portions Created by Jacob Boerema are Copyright (C) 2013-2015 Jacob Boerema.
  All Rights Reserved.
  This fork of GraphicEx can be found at https://bitbucket.org/jacobb/jgb-thirdparty
}
unit gexThread;

interface

{$WARN SYMBOL_PLATFORM OFF} // FindFirst: faHidden, faSysFile, faArchive

{$IFDEF FPC}
  {$mode delphi}
{$ENDIF}

uses SysUtils, Forms, Classes, Windows, Messages, Graphics, ComCtrls,
     {$IFNDEF FPC}
     jpeg,
     {$ELSE}
     LclType, FPReadJPEG,
     gexJpegWrapper, // Extended TJpegImage with support for scale (add after Graphics!)
     {$ENDIF}
     rkView;

const
  // Define custom messages sent by Thumbnail thread
  CM_UpdateView     = WM_USER + 2102; // Form needs to update the view
  CM_UpdateProgress = WM_USER + 2103; // Progress: wparam = current pos, lparam = count

  // Define some standard image formats
  CgexUnknown      =  0;
  CgexBitmap       =  1;
  CgexJpeg         =  2;
  CgexPng          =  3;
  CgexGif          =  4;

type
  // ImageFileType defines the image format. We use Word instead of an
  // enumeration to make it easier to extend.
  TImageFileFormat = Word;

  PgexCacheItem = ^TgexCacheItem;
  TgexCacheItem = record
    Idx: Integer;
    Size: Integer;
    Age: TDateTime;
    Scale: Integer;
    Bmp: TBitmap;
  end;

  // The Information we store for each thumbnail
  PgexThumbData = ^TgexThumbData;
  TgexThumbData = record
    Name: string;
    ThumbWidth: Word;
    ThumbHeight: Word;
    Size: Int64;
    Modified: TDateTime;
    IWidth, IHeight: Word;
    GotThumb: Boolean;
    Image: TObject;
    ImageFormat: TImageFileFormat;  // The image file format
    ImageData: Pointer;             // Descendant classes can use this to store extra info
  end;

  // Define an interface for Forms that support our TgexThumbnailThread
  IThumbnail = interface['{BE89519B-90D8-46A4-ACB7-5C7DEC0C5998}']
    function GetExceptionMessage: string;
    procedure SetExceptionMessage(AMessage: string);
    function GetIgnoreException: Boolean;
    procedure SetIgnoreException(AValue: Boolean);

    procedure ThumbsGetThumbnail(Sender: TObject; Thumb: PgexThumbData);

    property ExceptionMessage: string read GetExceptionMessage write SetExceptionMessage;
    property IgnoreException: Boolean read GetIgnoreException write SetIgnoreException;
  end;

  // Thumbnail creation thread class
  TgexThumbnailThread = class(TThread)
  private
    { Private declarations }
    XPView: TrkView;
    XPList: TList;
    FThreadExceptionMsg: string;
    FParentForm: TForm;              // Parent form that supports IThumbnail interface
    FIThumbnail: IThumbnail;         // Parent form's IThumbnail interface
  protected
    procedure Execute; override;
    procedure ShowExceptionMessage;  // Message from exception caught by ParentForm's function
    procedure ShowThreadException;   // Message caught in Thread execute
  public
    constructor Create(AParentForm: TForm; xpThumbs: TrkView; Items: TList);
  end;

  // Define a form class that supports the IThumbnail interface
  TgexBaseForm = class(TForm, IThumbnail)
  private
    FExceptionMessage: string;    // Thread exception message
    FMaxThumbSizeW,
    FMaxThumbSizeH: Integer;      // Maximum WxH of Thumb
    FImageFolder: string;         // Current folder with images
    FThumbJpeg: TJpegImage;
    FThumbView: TrkView;
    FProgressCount,
    FProgressCurrent: Integer;
    FIgnoreException: Boolean;
    function GetExceptionMessage: string;
    procedure SetExceptionMessage(AMessage: string);
    function GetIgnoreException: Boolean;
    procedure SetIgnoreException(AValue: Boolean);
  protected
    CellJpeg: TJpegImage;
    CellScale: Integer;
    CellStyle: Integer;
    ThumbThr: TgexThumbnailThread;
    ThreadDone: Boolean;
    Items: TList;
    ThumbsPool: TList;
    PoolSize,
    MaxPool: Integer;
    procedure SetThumbSize(Value: Integer; UpdateTrackbar: Boolean);
    function CreateThumbnail( const ImgW, ImgH: integer; Img: TGraphic;
      Thumb: PgexThumbData ): TBitmap;
    procedure CreateJpegThumbnail( ThumbBmp: TBitmap; Thumb: PgexThumbData);
    procedure ThumbsGetThumbnail(Sender: TObject; Thumb: PgexThumbData);
    function GetThumbBitmap(idx: Integer): TBitmap;
    procedure BiResample(Src, Dest: TBitmap; Sharpen: Boolean);

    function IsThreadRunning: Boolean;
    procedure StartThumbThread;
    procedure StopThumbThread;
    procedure ClearThumbs;
    procedure ClearThumbsPool;
    function GetView(): TrkView; virtual; abstract;
    procedure InitView;
    procedure InitCellColors; virtual;
    procedure UpdateProgress(AMessage: string); virtual; abstract;
    procedure CMUpdateView(var message: TMessage); message CM_UpdateView;
    procedure CMUpdateProgress(var message: TMessage); message CM_UpdateProgress;
    property ExceptionMessage: string read GetExceptionMessage write SetExceptionMessage;
    property IgnoreException: Boolean read GetIgnoreException write SetIgnoreException;
  public
    // Colors
    cGSelectedStart,
    cGSelectedEnd,
    cGHotStart,
    cGHotEnd,
    cGDisabledStart,
    cGDisabledEnd,
    cGHeaderStart,
    cGHeaderEnd,
    cGHeaderHotStart,
    cGHeaderHotEnd,
    cGHeaderSelStart,
    cGHeaderSelEnd,
    cHot,
    cSelected,
    cDisabled,
    cBackground,
    cLineHighLight: TColor;
    cShadeSelect: TColor;
    cShadeDisabled: TColor;
    CellShade0: TColor;
    CellShade1: TColor;
    CellShade2: TColor;
    CellShade3: TColor;
    CellShade4: TColor;
    CellBkgColor: TColor;
    CellBrdColor: array[Boolean, Boolean] of TColor;

    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    // DetermineImageFormat can be overridden to determine extra image formats.
    // If you need to store extra data about the image type you can do that
    // in ImageData. This default function always sets it to nil.
    function DetermineImageFormat( const FileName: string;
        var ImageData: Pointer): TImageFileFormat; virtual;
    // Descendants can use ConvertImageToThumb to convert additional image formats to thumbnails
    function ConvertImageToThumb(const FileName: string; AThumb: PgexThumbData): TBitmap; virtual;

    // Read current image folder and make thumbnails
    procedure LoadThumbnails;
    procedure DoUpdateThumbnailSize(ASize: Integer); virtual;
    procedure UpdateView; virtual;

    procedure ItemPaintBasic(ACanvas: TCanvas; R: TRect; State: TsvItemState);
    // Paint function to be attached to rkView's OnCellPaint
    procedure ViewMainCellPaint(Sender: TObject; ACanvas: TCanvas;
      Cell: TRect; IdxA, Idx: Integer; State: TsvItemState);

    property MaxThumbSizeW: Integer read FMaxThumbSizeW write FMaxThumbSizeW;
    property MaxThumbSizeH: Integer read FMaxThumbSizeH write FMaxThumbSizeH;
    property ImageFolder: string read FImageFolder write FImageFolder;
    property ThumbView: TrkView read FThumbView;
  end;

  // Define our own Exception class to be able to distinguish it from other exceptions
  TThumbnailThreadException = class(Exception);


implementation

uses Dialogs, Math;


// -----------------------------------------------------------------------------
//                           Local functions
// -----------------------------------------------------------------------------

// List sort compare proc: P1, P2: PThumbData
function CompareModifiedDate(P1, P2: Pointer): Integer;
begin
  if PgexThumbData(P1).Modified < PgexThumbData(P2).Modified then
    Result := 1  // Date, sorted from newest first to oldest last
  else if PgexThumbData(P1).Modified > PgexThumbData(P2).Modified then
    Result := -1
  else
    Result := 0;
end;

// List sort compare proc: P1, P2: PThumbData
function CompareFileType(P1, P2: Pointer): Integer;
begin
  if PgexThumbData(P1).ImageFormat < PgexThumbData(P2).ImageFormat then
    Result := 1
  else if PgexThumbData(P1).ImageFormat > PgexThumbData(P2).ImageFormat then
    Result := -1
  else
    Result := 0;
end;

function CompareFileName(P1, P2: Pointer): Integer;
begin
  // Compare case insensitive
  Result := CompareText(PgexThumbData(P1).Name,PgexThumbData(P2).Name);
end;

// http://www.swissdelphicenter.ch/en/showcode.php?id=1698

function ReadMWord(f: TFileStream): Word;
type
  TMotorolaWord = record
    case Byte of
      0: (Value: Word);
      1: (Byte1, Byte2: Byte);
  end;
var
  MW: TMotorolaWord;
begin
  { It would probably be better to just read these two bytes in normally }
  { and then do a small ASM routine to swap them.  But we aren't talking }
  { about reading entire files, so I doubt the performance gain would be }
  { worth the trouble. }
  f.read(MW.Byte2, SizeOf(Byte));
  f.read(MW.Byte1, SizeOf(Byte));
  Result := MW.Value;
end;

procedure GetJPGSize(const sFile: string; var wWidth, wHeight: Integer);
const
  ValidSig: array[0..1] of Byte = ($FF, $D8);
  Parameterless = [$01, $D0, $D1, $D2, $D3, $D4, $D5, $D6, $D7];
var
  Sig: array[0..1] of byte;
  f: TFileStream;
  x: integer;
  Seg: byte;
  Dummy: array[0..15] of byte;
  Len: word;
  ReadLen: LongInt;
begin
  FillChar(Sig, SizeOf(Sig), #0);
  wWidth := 0;
  wHeight := 0;
  f := TFileStream.Create(sFile, fmOpenRead);
  try
    ReadLen := f.read(Sig[0], SizeOf(Sig));
    for x := Low(Sig) to High(Sig) do
      if Sig[x] <> ValidSig[x] then
        ReadLen := 0;
    if ReadLen > 0 then
    begin
      ReadLen := f.read(Seg, 1);
      while (Seg = $FF) and (ReadLen > 0) do
      begin
        ReadLen := f.read(Seg, 1);
        if Seg <> $FF then
        begin
          if (Seg = $C0) or (Seg = $C1) then
          begin
            ReadLen := f.read(Dummy[0], 3); { don't need these bytes }
            wHeight := ReadMWord(f);
            wWidth := ReadMWord(f);
          end
          else
          begin
            if not (Seg in Parameterless) then
            begin
              Len := ReadMWord(f);
              f.Seek(Len - 2, 1);
              f.read(Seg, 1);
            end
            else
              Seg := $FF; { Fake it to keep looping. }
          end;
        end;
      end;
    end;
  finally
    f.Free;
  end;
  // Make W x H safe
  if wWidth < 1 then
    wWidth := 1;
  if wHeight < 1 then
    wHeight := 1;
end;

function CalcThumbSize(w, h, tw, th: integer): TPoint;
begin
  Result.X := 0;
  Result.Y := 0;
  if (w < tw) and (h < th) then
  begin
    Result.X := w;
    Result.Y := h;
  end
  else if (w = 0) or (h = 0) then
    Exit
  else
  begin
    if w > h then
    begin
      if w < tw then
        tw := w;
      Result.X := tw;
      Result.Y := Trunc(tw * h / w);
      if Result.Y > th then
      begin
        Result.Y := th;
        Result.X := Trunc(th * w / h);
      end;
    end
    else
    begin
      if h < th then
        th := h;
      Result.Y := th;
      Result.X := Trunc(th * w / h);
      if Result.X > tw then
      begin
        Result.X := tw;
        Result.Y := Trunc(tw * h / w);
      end;
    end;
  end;
end;

procedure MakeThumbNail(Src, Dst: TBitmap);
var
  x, y, ix, iy, w, h, dx, dy: Integer;
  x1, x2, x3: integer;
  RowDest, RowSource, RowSourceStart: NativeInt;
  iRatio: Integer;
  Ratio: Single;
  iRed, iGrn, iBlu: Integer;
  pt: PRGB24;
  iSrc, iDst: NativeInt;
  lutW, lutH: array of Integer;
begin
  if (Src.Width <= Dst.Width) and (Src.Height <= Dst.Height) then
  begin
    Dst.Assign(Src);
    Exit;
  end;
  w := Dst.Width;
  h := Dst.Height;
  Ratio := 1 / (w / Src.Width);
  SetLength(lutW, w);
  x1 := 0;
  x2 := Trunc(Ratio);
  for x := 0 to w - 1 do
  begin
    lutW[x] := x2 - x1;
    x1 := x2;
    x2 := Trunc((x + 2) * Ratio);
  end;
  Ratio := 1 / (h / Src.Height);
  SetLength(lutH, h);
  x1 := 0;
  x2 := Trunc(Ratio);
  for x := 0 to h - 1 do
  begin
    lutH[x] := x2 - x1;
    x1 := x2;
    x2 := Trunc((x + 2) * Ratio);
  end;
  RowDest := NativeInt(Dst.Scanline[0]);
  RowSourceStart := NativeInt(Src.Scanline[0]);
  RowSource := RowSourceStart;
  iDst := ((w * 24 + 31) and not 31) shr 3;
  iSrc := ((Src.Width * 24 + 31) and not 31) shr 3;
  for y := 0 to h - 1 do
  begin
    dy := lutH[y];
    x1 := 0;
    x3 := 0;
    for x := 0 to w - 1 do
    begin
      dx := lutW[x];
      iRed := 0;
      iGrn := 0;
      iBlu := 0;
      RowSource := RowSourceStart;
      for iy := 1 to dy do
      begin
        pt := PRGB24(RowSource + x1);
        for ix := 1 to dx do
        begin
          iRed := iRed + pt.R;
          iGrn := iGrn + pt.G;
          iBlu := iBlu + pt.B;
          inc(pt);
        end;
        {$IFNDEF FPC}
        RowSource := RowSource - iSrc;
        {$ELSE}
        // fpc TBitmap rawdata always top to bottom?
        RowSource := RowSource + iSrc;
        {$ENDIF}
      end;
      iRatio := $00FFFFFF div (dx * dy);
      pt := PRGB24(RowDest + x3);
      pt.R := (iRed * iRatio) shr 24;
      pt.G := (iGrn * iRatio) shr 24;
      pt.B := (iBlu * iRatio) shr 24;
      x1 := x1 + 3 * dx;
      inc(x3, 3);
    end;
    {$IFNDEF FPC}
    RowDest := RowDest - iDst;
    {$ELSE}
    // fpc TBitmap rawdata always top to bottom?
    RowDest := RowDest + iDst;
    {$ENDIF}
    RowSourceStart := RowSource;
  end;
end;


type
  // Corrects the wrong declarartions of TRIVERTEX and GradientFill in Delphi unit WINDOWS.PAS
  TRIVERTEX = packed record
    X, Y: DWORD;
    Red, Green, Blue, Alpha: Word;
  end;

function GradientFill(DC: hDC; pVertex: Pointer; dwNumVertex: DWORD; pMesh: Pointer;
    dwNumMesh, dwMode: DWORD): DWord; stdcall; external 'msimg32.dll';

procedure WinGradient( DC: HDC; ARect: TRect; AColor2, AColor1: TColor );
var
  Vertexs: array [ 0 .. 1 ] of TRIVERTEX; // was TTriVertex;
  GRect: TGradientRect;
begin
  Vertexs[ 0 ].x := ARect.Left;
  Vertexs[ 0 ].y := ARect.Top;
  Vertexs[ 0 ].Red := ( AColor1 and $000000FF ) shl 8;
  Vertexs[ 0 ].Green := ( AColor1 and $0000FF00 );
  Vertexs[ 0 ].Blue := ( AColor1 and $00FF0000 ) shr 8;
  Vertexs[ 0 ].alpha := 0;
  Vertexs[ 1 ].x := ARect.Right;
  Vertexs[ 1 ].y := ARect.Bottom;
  Vertexs[ 1 ].Red := ( AColor2 and $000000FF ) shl 8;
  Vertexs[ 1 ].Green := ( AColor2 and $0000FF00 );
  Vertexs[ 1 ].Blue := ( AColor2 and $00FF0000 ) shr 8;
  Vertexs[ 1 ].alpha := 0;
  GRect.UpperLeft := 0;
  GRect.LowerRight := 1;
  GradientFill( DC, @Vertexs, 2, @GRect, 1, GRADIENT_FILL_RECT_V );
end;


// -----------------------------------------------------------------------------
//                           TgexBaseForm
// -----------------------------------------------------------------------------

constructor TgexBaseForm.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  FIgnoreException := False;
  FThumbView := nil;
  Items := TList.Create;
  PoolSize := 0;
  MaxPool := Round(((Screen.Width * Screen.Height) * 3) * 1.5);
  ThumbsPool := TList.Create;
  FThumbJpeg := TJpegImage.Create;
  FThumbJpeg.CompressionQuality := 80;
  FThumbJpeg.Performance := jpBestSpeed;
  FMaxThumbSizeW := 256;
  FMaxThumbSizeH := 256;
  CellJpeg := TJpegImage.Create;
  CellJpeg.Performance := jpBestSpeed;
  CellStyle := -1;
  CellScale := 0;

  // Make sure multi threaded is set
  IsMultiThread := True;
end;

destructor TgexBaseForm.Destroy;
var
  i: Integer;
  Item: PgexThumbData;
begin
  StopThumbThread;
  for i := Items.Count - 1 downto 0 do
  begin
    Item := Items[i];
    if Item.Size <> 0 then
      Item.Image.Free;
    Dispose(Item);
  end;
  ClearThumbsPool;
  ThumbsPool.Free;
  Items.Free;
  CellJpeg.Free;
  FThumbJpeg.Free;
  inherited Destroy;
end;

function TgexBaseForm.GetExceptionMessage: string;
begin
  Result := FExceptionMessage;
end;

procedure TgexBaseForm.SetExceptionMessage(AMessage: string);
begin
  FExceptionMessage := AMessage;
end;

function TgexBaseForm.GetIgnoreException: Boolean;
begin
  Result := FIgnoreException;
end;

procedure TgexBaseForm.SetIgnoreException(AValue: Boolean);
begin
  FIgnoreException := AValue;
end;

function TgexBaseForm.IsThreadRunning: Boolean;
begin
  Result := ThumbThr <> nil;
end;

procedure TgexBaseForm.StartThumbThread;
begin
  if IsThreadRunning then
    Exit;
  ThreadDone := False;
  ThumbThr := TgexThumbnailThread.Create(Self, ThumbView, Items);
end;

procedure TgexBaseForm.StopThumbThread;
begin
  if ThumbThr <> nil then
  begin
    ThumbThr.Terminate;
    ThumbThr.WaitFor;
    ThumbThr.Free;
    ThumbThr := nil;
  end;
end;

procedure TgexBaseForm.ClearThumbs;
var
  i: Integer;
  Item: PgexThumbData;
begin
  ThumbView.Items.Clear;
  for i := Items.Count - 1 downto 0 do
  begin
    Item := Items[i];
    if Assigned(Item) then
      if (Item.Size <> 0) and (Item.Image <> nil) then
        Item.Image.Free;
    Dispose(Item);
  end;
  Items.Clear;
end;

procedure TgexBaseForm.ClearThumbsPool;
var
  i: Integer;
  Thumb: PgexCacheItem;
begin
  for i := ThumbsPool.Count - 1 downto 0 do
  begin
    Thumb := ThumbsPool[i];
    if Thumb.Bmp <> nil then
      Thumb.Bmp.Free;
    Dispose(Thumb);
  end;
  ThumbsPool.Clear;
  PoolSize := 0;
end;

procedure TgexBaseForm.DoUpdateThumbnailSize(ASize: Integer);
begin
  // Override when needed
end;

procedure TgexBaseForm.SetThumbSize(Value: Integer; UpdateTrackbar: Boolean);
var
  w, h: Integer;
begin
  if FThumbView = nil then
    InitView;
  case Value of
    32..63: CellJpeg.Scale := jsQuarter;
    64..127: CellJpeg.Scale := jsHalf;
    128..255: CellJpeg.Scale := jsFullSize;
  else
    CellJpeg.Scale := jsEighth;
  end;
  w := Value;
  h := Value;
  w := w + 20;
  if CellStyle = 0 then
    h := h + 20
  else
    h := h + 40;
  ThumbView.CellWidth := w;
  ThumbView.CellHeight := h;
  CellScale := Value;
  if UpdateTrackbar then
  begin
    DoUpdateThumbnailSize(CellScale);
  end;
  ThumbView.CalcView(False);
  if not UpdateTrackBar then
    ThumbView.SetAtTop(-1, ThumbView.ViewIdx);
end;

function TgexBaseForm.CreateThumbnail( const ImgW, ImgH: integer; Img: TGraphic;
  Thumb: PgexThumbData ): TBitmap;
var bmp: TBitmap;
  ThumbBmp: TBitmap;
  ThumbSize: TPoint;
  NewW, NewH: Integer;
begin
  Result := nil;
  bmp := TBitmap.Create;
  try
    bmp.PixelFormat := pf24Bit;
    bmp.Width := ImgW;
    bmp.Height := ImgH;
    Thumb.IWidth := ImgW;
    Thumb.IHeight := ImgH;
    bmp.Canvas.Lock;
    bmp.Canvas.Draw(0, 0, Img);
    try
      ThumbBmp := TBitmap.Create;
      ThumbBmp.Canvas.Lock;
      ThumbSize := CalcThumbSize(bmp.Width, bmp.Height, FMaxThumbSizeW,
        FMaxThumbSizeH);
      newW := ThumbSize.X;
      newH := ThumbSize.Y;
      if newW <= 0 then
        newW := 1;
      if newH <= 0 then
        newH := 1;
      ThumbBmp.PixelFormat := pf24Bit;
      ThumbBmp.Width := newW;
      ThumbBmp.Height := newH;
      MakeThumbNail(bmp, ThumbBmp);
      Result := ThumbBmp;
    except
      ThumbBmp.Canvas.UnLock;
      FreeAndNil(ThumbBmp);
      raise;
    end;
  finally
    bmp.Canvas.UnLock;
    bmp.Free;
  end;
end;

procedure TgexBaseForm.CreateJpegThumbnail( ThumbBmp: TBitmap; Thumb: PgexThumbData);
var MS: TMemoryStream;
begin
  // This converts the thumbnail from bitmap to compressed jpg and stores
  // it in Thumb.Image
  if ThumbBmp <> nil then
  begin
    FThumbJpeg.Assign(ThumbBmp);
    {$IFNDEF FPC}
    FThumbJpeg.Compress;
    {$ENDIF}
    MS := TMemoryStream.Create;
    try
      MS.Position := 0;
      try
        FThumbJpeg.SaveToStream(MS);
      except
        MS.Free;
        raise;
      end;
      Thumb.Image := MS;
      Thumb.ThumbWidth := ThumbBmp.Width;
      Thumb.ThumbHeight := ThumbBmp.Height;
    finally
      // Do NOT FREE the MS stream here because that's the thumbnail image
      ThumbBmp.Free;
    end;
  end
  else
    Thumb.Image := nil;
end;

type
     TShortFileHeader = record
       case byte of
         0: (Word1, Word2: Word;);
         1: (LongWord1: LongWord;);
         2: (string5: array [0..4] of char);
     end;
// DetermineImageFormat can be overridden to determine extra image formats.
// This one only detects BMP and JPG files
// If you need to store extra data about the image type you can do that
// in ImageData. This default function always sets it to nil.
function TgexBaseForm.DetermineImageFormat( const FileName: string;
    var ImageData: Pointer): TImageFileFormat;
const cJpegSOIMarker = $d8ff;
      cBmpMarker = $4d42; // 'BM'
var
  Stream: TFileStream;
  TheHeader: TShortFileHeader;
begin
  Result := CgexUnknown;
  ImageData := nil;
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    if Stream.Size > sizeof(TShortFileHeader) then begin
      Stream.Seek(0,soBeginning);
      Stream.ReadBuffer(TheHeader,sizeof(TShortFileHeader));
      if TheHeader.Word1 = cJpegSOIMarker then
        Result := CgexJpeg
      else if TheHeader.Word1 = cBmpMarker then
        Result := CgexBitmap
    end;
  finally
    Stream.Free;
  end;
end;

// Descendants can use ConvertImageToThumb to convert additional image formats to thumbnails
function TgexBaseForm.ConvertImageToThumb(const FileName: string; AThumb: PgexThumbData): TBitmap;
begin
  // Empty here: should be overridden
  Result := nil;
end;

procedure TgexBaseForm.ThumbsGetThumbnail(Sender: TObject; Thumb: PgexThumbData);
var
  FName: string;
  WI, HI: Integer;
  sf: Integer;
  fail: Boolean;
  ThumbBmp: TBitmap;
  ImageExtraData: Pointer;
  ABitmap: TBitmap;
begin
  FName := ImageFolder + Thumb.name;
  ThumbBmp := nil;
  fail := False;
  if FName <> '' then
  begin
    Thumb.ImageFormat := DetermineImageFormat(FName, ImageExtraData);
    Thumb.ImageData := ImageExtraData;

    if Thumb.ImageFormat = CgexJpeg then
    begin
      // This part determines the dimensions of a jpeg, the optimal jpeg scale
      // and then loads the jpeg image
      FThumbJpeg.Scale := jsFullSize;
      GetJPGSize(FName, WI, HI);
      sf := Trunc(Min(HI / 255 {TH}, WI / 255 {TW}));
      if sf < 0 then
        sf := 0;
      case sf of
        0..1: FThumbJpeg.Scale := jsFullSize;
        2..3: FThumbJpeg.Scale := jsHalf;
        4..7: FThumbJpeg.Scale := jsQuarter;
      else
        FThumbJpeg.Scale := jsEighth;
      end;
      try
        FThumbJpeg.LoadFromFile(FName);
      except
        ExceptionMessage := 'Error loading jpeg: ' + FName;
        fail := True;
      end;
      // This draws the full image to a Bitmap and then makes a
      // thumbnail image in the required size for it
      if not fail then
        ThumbBmp := CreateThumbnail(FThumbJpeg.Width, FThumbJpeg.Height,
          FThumbJpeg, Thumb);
    end
    else if Thumb.ImageFormat = CgexBitmap then begin
      ABitmap := TBitmap.Create;
      try
        try
          ABitmap.LoadFromFile(FName);
        except
          // Since we are in a thread using ShowMessage isn't threadsafe
          // thus we use a var which in this case we know will only be
          // used either here, or in thread excute after this method has
          // finished, so should be safe
          FExceptionMessage := 'Error loading bitmap: ' + FName;
          fail := True;
        end;
        // This draws the full image to a Bitmap and then makes a
        // thumbnail image in the required size for it
        if not fail then
          ThumbBmp := CreateThumbnail(ABitmap.Width, ABitmap.Height, ABitmap, Thumb);
      finally
        ABitmap.Free;
      end;
    end
    else if Thumb.ImageFormat <> CgexUnknown then begin
      // If image format isn't unknown then assume a descendant class will
      // know how to load it
      try
        ThumbBmp := ConvertImageToThumb(FName, Thumb);
      except
        FExceptionMessage := 'Error loading image: ' + FName;
        fail := True;
      end;
    end;
  end
  else begin
    Thumb.ImageFormat := CgexUnknown;
    Thumb.ImageData := nil;
  end;

  // Convert thumbnail from Bitmap to compressed Jpeg
  CreateJpegThumbnail(ThumbBmp, Thumb);
  Thumb.GotThumb := True;
end;

procedure TgexBaseForm.UpdateView;
begin
  // Override when needed
end;

procedure TgexBaseForm.CMUpdateView(var message: TMessage);
begin
  if FThumbView <> nil then
    FThumbView.Invalidate;
  UpdateView;
end;

procedure TgexBaseForm.CMUpdateProgress(var message: TMessage);
begin
  FProgressCount := message.LParam;
  FProgressCurrent := message.WParam;
  if FProgressCount = FProgressCurrent then
  begin
    UpdateProgress('');
  end
  else if FProgressCurrent mod 50 = 0 then
    UpdateProgress('Determining ' + IntToStr(FProgressCurrent) +
      ' of ' + IntToStr(FProgressCount) + ' files.');
end;

procedure TgexBaseForm.InitCellColors;
begin
  cHot := $00FDDE99;
  cgHotStart := $00FDF5E6;
  cGHotEnd := $00FDFBF6;
  cSelected := $00FDCE99;
  cGSelectedStart := $00FCEFC4;
  cGSelectedEnd := $00FDF8EF;
  cShadeSelect := $00F8F3EA;
  cDisabled := $00D9D9D9;
  cGDisabledStart := $00EAE9E9;
  cGDisabledEnd := $00FCFBFB;
  cShadeDisabled := $00F6F5F5;
  cGHeaderStart := $00F9F9F9;
  cGHeaderEnd := $00FEFEFE;
  cGHeaderHotStart := $00FFEDBD;
  cGHeaderHotEnd := $00FFF7E3;
  cGHeaderSelStart := $00FCEABA;
  cGHeaderSelEnd := $00FCF4E0;
  cBackground := clWindow;

  cLineHighLight := $00FEFBF6;
  CellBkgColor := clWindow;
  CellBrdColor[False, False] := cDisabled;
  CellBrdColor[False, True] := cDisabled;
  CellBrdColor[True, False] := $00B5B5B5;
  CellBrdColor[True, True] := cSelected;
end;

procedure TgexBaseForm.InitView;
begin
  if FThumbView = nil then begin
    FThumbView := GetView();  // Initial loading of rkView
    InitCellColors;
    // Set OnCellPaint
    FThumbView.OnCellPaint := ViewMainCellPaint;
  end;
end;

// Read current image folder and make thumbnails
procedure TgexBaseForm.LoadThumbnails;
var
  Entry: PgexThumbData;
  SR: TSearchRec;
  n: Integer;
  Ext: string;
begin
  InitView; // Initialize our rkView where the thumbnails should be stored
  if FImageFolder <> '' then
  begin
    StopThumbThread;
    ClearThumbs;
    ClearThumbsPool;
    ThumbView.ViewIdx := -1;
    ThumbView.Clear;
    Forms.Application.ProcessMessages;
    if FImageFolder[length(FImageFolder)] <> '\' then
      FImageFolder := FImageFolder + '\';
    if FindFirst(FImageFolder + '*.*', faAnyFile - faDirectory - faHidden - faSysFile - faArchive, SR) = 0 then
    begin
      Items.Capacity := 1000;
      repeat
        // check if this is an image file we can show
        Ext := LowerCase(ExtractFileExt(SR.Name));
        begin
          if Items.Count mod 50 = 0 then
          begin
            if Items.Count = 0 then
              UpdateProgress('Loading filenames...')
            else
              UpdateProgress('Loading filenames... (' + IntToStr(Items.Count) + ')');
          end;
          New(Entry);
          Entry.Name := SR.Name;
          Entry.Size := SR.Size;
          Entry.Modified := FileDateToDateTime(SR.Time);
          Entry.IWidth := 0;
          Entry.IHeight := 0;
          Entry.ThumbWidth := 0;
          Entry.ThumbHeight := 0;
          Entry.GotThumb := False;
          Entry.Image := nil;
          Entry.ImageFormat := CgexUnknown;
          Entry.ImageData := nil;
          n := Items.Add(Entry);
          if n <> -1 then
            ThumbView.Items.Add(n);
        end;
      until FindNext(SR) <> 0;
      SysUtils.FindClose(SR);
      Items.Capacity := Items.Count;
      Items.Sort(CompareFileName);
    end;
  end;
  ThumbView.CalcView(True);

  // TODO tbSize.Position ...................................................................................
  SetThumbSize({tbSize.Position}100, False);

  if ThumbThr = nil then
  begin
    ThreadDone := False;
    ThumbThr := TgexThumbnailThread.Create(Self, ThumbView, Items);
  end;
end;

procedure TgexBaseForm.BiResample(Src, Dest: TBitmap; Sharpen: Boolean);
// Fast bilinear resampling procedure found at Swiss Delphi Center + my mods...
type
  PRGB24 = ^TRGB24;
  TRGB24 = record B, G, R: Byte;
  end;
  PRGBArray = ^TRGBArray;
  TRGBArray = array[0..0] of TRGB24;
var
  x, y, px, py: Integer;
  i, x1, x2, z, z2, iz2: Integer;
  w1, w2, w3, w4: Integer;
  Ratio: Integer;
  sDst, sDstOff: NativeInt;
  sScanLine: array[0..255] of PRGBArray;
  Src1, Src2: PRGBArray;
  C, C1, C2: TRGB24;
  y1, y2, y3: NativeInt;
  x3, iRed, iGrn, iBlu: Integer;
  p1, p2, p3, p4, p5: PRGB24;
begin
  // ScanLine buffer for Source
  sDst := NativeInt(src.Scanline[0]);
  sDstOff := NativeInt(src.Scanline[1]) - sDst;
  for i := 0 to src.Height - 1 do
  begin
    sScanLine[i] := PRGBArray(sDst);
    sDst := sDst + sDstOff;
  end;
  // ScanLine for Destiantion
  sDst := NativeInt(Dest.Scanline[0]);
  y1 := sDst; // only for sharpening...
  sDstOff := NativeInt(Dest.Scanline[1]) - sDst;
  // Ratio is same for width and height
  Ratio := ((src.Width - 1) shl 15) div Dest.Width;
  py := 0;
  for y := 0 to Dest.Height - 1 do
  begin
    i := py shr 15;
    if i > src.Height - 1 then
      i := src.Height - 1;
    Src1 := sScanLine[i];
    if i < src.Height - 1 then
      Src2 := sScanLine[i + 1]
    else
      Src2 := Src1;
    z2 := py and $7FFF;
    iz2 := $8000 - z2;
    px := 0;
    for x := 0 to Dest.Width - 1 do
    begin
      x1 := px shr 15;
      x2 := x1 + 1;
      C1 := Src1[x1];
      C2 := Src2[x1];
      z := px and $7FFF;
      w2 := (z * iz2) shr 15;
      w1 := iz2 - w2;
      w4 := (z * z2) shr 15;
      w3 := z2 - w4;
      C.R := (C1.R * w1 + Src1[x2].R * w2 + C2.R * w3 + Src2[x2].R * w4) shr 15;
      C.G := (C1.G * w1 + Src1[x2].G * w2 + C2.G * w3 + Src2[x2].G * w4) shr 15;
      C.B := (C1.B * w1 + Src2[x2].B * w2 + C2.B * w3 + Src2[x2].B * w4) shr 15;
      // Set destination pixel
      PRGBArray(sDst)[x] := C;
      inc(px, Ratio);
    end;
    sDst := sDst + sDstOff;
    inc(py, Ratio);
  end;

  if not Sharpen then
    Exit;

  // Sharpening...
  y2 := y1 + sDstOff;
  y3 := y2 + sDstOff;
  for y := 1 to Dest.Height - 2 do
  begin
    for x := 0 to Dest.Width - 3 do
    begin
      x1 := x * 3;
      x2 := x1 + 3;
      x3 := x1 + 6;
      p1 := PRGB24(y1 + x1);
      p2 := PRGB24(y1 + x3);
      p3 := PRGB24(y2 + x2);
      p4 := PRGB24(y3 + x1);
      p5 := PRGB24(y3 + x3);
      // -15 -11                       // -17 - 13
      iRed := (p1.R + p2.R + (p3.R * -15) + p4.R + p5.R) div -11;
      iGrn := (p1.G + p2.G + (p3.G * -15) + p4.G + p5.G) div -11;
      iBlu := (p1.B + p2.B + (p3.B * -15) + p4.B + p5.B) div -11;
      if iRed < 0 then
        iRed := 0
      else if iRed > 255 then
        iRed := 255;
      if iGrn < 0 then
        iGrn := 0
      else if iGrn > 255 then
        iGrn := 255;
      if iBlu < 0 then
        iBlu := 0
      else if iBlu > 255 then
        iBlu := 255;
      PRGB24(y2 + x2).R := iRed;
      PRGB24(y2 + x2).G := iGrn;
      PRGB24(y2 + x2).B := iBlu;
    end;
    inc(y1, sDstOff);
    inc(y2, sDstOff);
    inc(y3, sDstOff);
  end;
end;

{$IFDEF FPC}
function Rect( ATop, ALeft, ABottom, ARight: Integer): TRect; inline;
begin
  with Result do begin
    Top := ATop;
    Left := ALeft;
    Bottom := ABottom;
    Right := ARight;
  end;
end;
{$ENDIF}

function TgexBaseForm.GetThumbBitmap(Idx: Integer): TBitmap;
var
  i, n, sf: Integer;
  p: PgexCacheItem;
  T: PgexThumbData;
  Bmp, tmp: TBitmap;
  pt: TPoint;
  Oldest: TDateTime;
begin
  Result := nil;
  // if we have thumbs, see if we can find it...
  if ThumbsPool.Count > 0 then
  begin
    i := ThumbsPool.Count - 1;
    while (i >= 0) and (PgexCacheItem(ThumbsPool[i]).Idx <> Idx) do
      i := i - 1;
    if i <> -1 then
    begin
      p := ThumbsPool[i];
      if (p.Idx = Idx) then
      begin
        if (p.Scale = CellScale) then
        begin
          p.Age := Now;
          Result := p.Bmp
        end
        else
        begin
          PoolSize := PoolSize - p.Size;
          p.Bmp.Free;
          Dispose(p);
          ThumbsPool.Delete(i);
        end;
      end;
    end;
  end;
  // if we dont have a thumb, make one...
  if Result = nil then
  begin
    T := Items[Idx];
    if T.Image <> nil then
    begin
      TMemoryStream(T.Image).Position := 0;

      sf := Trunc(Min(T.ThumbWidth / CellScale, T.ThumbHeight / CellScale));
      if sf < 0 then
        sf := 0;
      case sf of
        0..1: CellJPEG.Scale := jsFullSize;
        2..3: CellJPEG.Scale := jsHalf;
        4..7: CellJPEG.Scale := jsQuarter;
      else
        CellJPEG.Scale := jsEighth;
      end;
      CellJPEG.LoadFromStream(TMemoryStream(T.Image));

      Bmp := TBitmap.Create;
      Bmp.PixelFormat := pf24bit;
      pt := CalcThumbSize(CellJPEG.Width, CellJPEG.Height, CellScale,
        CellScale);
      Bmp.Canvas.Lock;
      try
        if pt.x <> CellJPEG.Width then
        begin
          tmp := TBitmap.Create;
          try
            tmp.Canvas.Lock;
            tmp.Width := CellJPEG.Width;
            tmp.Height := CellJPEG.Height;
            tmp.PixelFormat := pf24bit;
            tmp.Canvas.Draw(0, 0, CellJPEG);
            Bmp.Width := pt.x;
            Bmp.Height := pt.y;
            if (Bmp.Width > 4) and (Bmp.Height > 4) then
              BiReSample(tmp, Bmp, False)
            else
              bmp.Canvas.StretchDraw(Rect(0, 0, pt.X, pt.Y), tmp);
          finally
            tmp.Canvas.Unlock;
            tmp.Free;
          end;
        end
        else
        begin
          Bmp.Width := CellJPEG.Width;
          Bmp.Height := CellJPEG.Height;
          Bmp.Canvas.Draw(0, 0, CellJPEG);
        end;
      finally
        Bmp.Canvas.Unlock;
      end;
      New(p);
      p.Idx := Idx;
      p.Size := (Bmp.Width * Bmp.Height) * 3;
      p.Age := Now;
      p.Scale := CellScale;
      p.Bmp := Bmp;
      ThumbsPool.Add(p);
      PoolSize := PoolSize + p.Size;
      Result := p.Bmp;
      // Purge thumbs if needed
      while (PoolSize > MaxPool) and (ThumbsPool.Count > 0) do
      begin
        Oldest := Now;
        n := 0;
        for i := 0 to ThumbsPool.Count - 1 do
        begin
          p := ThumbsPool[i];
          if p.Age <= Oldest then
          begin
            Oldest := p.Age;
            n := i;
          end;
        end;
        Assert(n >= 0);
        p := ThumbsPool[n];
        PoolSize := PoolSize - p.Size;
        p.Bmp.Free;
        Dispose(p);
        ThumbsPool.Delete(n);
      end;
    end;
  end;
end;

procedure TgexBaseForm.ItemPaintBasic(ACanvas: TCanvas; R: TRect; State: TsvItemState);
var
  C: TColor;
begin
  ACanvas.Brush.Style := bsClear;
  if ( State = svSelected ) or ( State = svHot ) then
  begin
    if ( ThumbView.Focused) and ( State = svSelected ) then
    begin
      ACanvas.Pen.Color := cSelected;
      WinGradient( ACanvas.Handle, R, cGSelectedStart, cGSelectedEnd );
    end
    else if ( State = svHot ) then
    begin
      ACanvas.Pen.Color := cHot;
      WinGradient( ACanvas.Handle, R, cGHotStart, cGHotEnd );
    end
    else
    begin
      ACanvas.Pen.Color := cDisabled;
      WinGradient( Canvas.Handle, R, cGDisabledStart, cGDisabledEnd );
    end;
    ACanvas.Rectangle( R );
    if ( ThumbView.Focused ) then
      C := cShadeSelect
    else
      C := cShadeDisabled;
    ACanvas.Pen.Color := C;
    ACanvas.MoveTo( R.Left + 1, R.Top + 2 );
    ACanvas.LineTo( R.Left + 1, R.Bottom - 2 );
    ACanvas.LineTo( R.Right - 2, R.Bottom - 2 );
    ACanvas.LineTo( R.Right - 2, R.Top + 1 );
    ACanvas.Pen.Style := psSolid;
    ACanvas.Pixels[ R.Left, R.Top ] := C;
    ACanvas.Pixels[ R.Left, R.Bottom - 1 ] := C;
    ACanvas.Pixels[ R.Right - 1, R.Top ] := C;
    ACanvas.Pixels[ R.Right - 1, R.Bottom - 1 ] := C;
  end;
end;

// Paint function to be attached to rkView's OnCellPaint
procedure TgexBaseForm.viewMainCellPaint(Sender: TObject; ACanvas: TCanvas;
  Cell: TRect; IdxA, Idx: Integer; State: TsvItemState);
var
  x, y: Integer;
  F, S: Boolean;
  R: TRect;
  TW, TH: Integer;
  Txt: string;
  Thumb: PgexThumbData;
  pt: TPoint;
begin
  Thumb := PgexThumbData(Items[idx]);
  pt := CalcThumbSize(Thumb.ThumbWidth, Thumb.ThumbHeight, CellScale,
    CellScale);
  TW := pt.X;
  TH := pt.Y;
  ACanvas.Lock; // Lock Canvas since we're in a thread
  try
    ItemPaintBasic(ACanvas, Cell, State);
    F := ThumbView.Focused;
    S := State = svSelected;
    x := Cell.Left + ((Cell.Right - (Cell.Left + TW)) shr 1);
    y := Cell.Top + ((Cell.Bottom - (Cell.Top + TH)) shr 1);
    y := y - 10;
    if (Thumb.Image <> nil) and (Thumb.GotThumb) then
      ACanvas.Draw(x, y, GetThumbBitmap(idx));
    R.Left := X;
    R.Top := Y;
    R.Right := X + TW;
    R.Bottom := Y + TH;
    ACanvas.Pen.Color := CellBrdColor[F, S];
    InflateRect(R, 2, 2);
    ACanvas.Rectangle(R);
    ACanvas.Pen.Color := clWhite;
    InflateRect(R, -1, -1);
    ACanvas.Rectangle(R);
    R := Cell;
    R.Top := R.Bottom - 20;
    txt := Thumb.Name;
    DrawText(ACanvas.Handle, PChar(txt), Length(txt), R, DT_END_ELLIPSIS or
      DT_SINGLELINE or DT_NOPREFIX or DT_CENTER or DT_VCENTER);
  finally
    ACanvas.Unlock;
  end;
end;

// -----------------------------------------------------------------------------
//                           TgexThumbnailThread
// -----------------------------------------------------------------------------

constructor TgexThumbnailThread.Create(AParentForm: TForm; xpThumbs: TrkView; Items: TList);
begin
  // Make sure the form supports our IThumbnail interface
  if not Supports(AParentForm, IThumbnail, FIThumbnail) then
    raise TThumbnailThreadException.Create('ParentForm doesn''t support IThumbnail interface!');

  FParentForm := AParentForm;
  XPView := xpThumbs;
  XPList := Items;
  FreeOnTerminate := False;
  inherited Create(False);
  Priority := tpLower;
end;

procedure TgexThumbnailThread.ShowExceptionMessage();
begin
  Showmessage(FIThumbnail.ExceptionMessage);
end;

procedure TgexThumbnailThread.ShowThreadException;  // Message caught in Thread execute
begin
  Showmessage(FThreadExceptionMsg);
end;

procedure TgexThumbnailThread.Execute;
var
  Cnt, i: Integer;
  PThumb: PgexThumbData;
  Old: Integer;
  Update: Boolean;
  InView: Integer;
begin
  inherited;
  if (XPView.Items.Count = 0) then
    Exit;
  Cnt := 0;
  Old := XPView.ViewIdx;
  try
    repeat
      FIThumbnail.ExceptionMessage := '';
      while (Cnt < XPView.Items.Count) and (Terminated = False) do
      begin
        if XPView.ViewIdx <> Old then
        begin
          Cnt := XPView.ViewIdx - 1;
          if Cnt = -1 then
            Cnt := 0;
          Old := XPView.ViewIdx;
        end;
        PThumb := PgexThumbData(XPList.Items[Cnt]);
        Update := PThumb.GotThumb;
        if (not Update) and (not Terminated) then
        begin
          PostMessage(FParentForm.Handle, CM_UpdateProgress, Cnt, XPView.Items.Count);
          FIThumbnail.ThumbsGetThumbnail(XPView, PThumb);
          if FIThumbnail.ExceptionMessage <> '' then
          begin
            Synchronize (ShowExceptionMessage);
            FIThumbnail.ExceptionMessage := '';
          end;
          InView := XPView.ViewIdx + (XPView.ViewColumns * (XPView.ViewRows));
          if (Cnt >= XPView.ViewIdx) and (Cnt <= InView) then
            PostMessage(FParentForm.Handle, CM_UpdateView, 0, 0);
        end;
        inc(Cnt);
      end;
      Cnt := 0;
      for i := 0 to XPView.Items.Count - 1 do
        if PgexThumbData(XPList.Items[i]).GotThumb = False then
          inc(Cnt);
    until (Cnt = 0) or (Terminated);
  except
    on E:exception do
    begin
      if not FIThumbnail.IgnoreException then begin
        FThreadExceptionMsg := E.Message;
        Synchronize (ShowThreadException);
      end
      else
        // Reset Exception status
        FIThumbnail.IgnoreException := False;
    end;
  end;
  if not Terminated then begin
    PostMessage(FParentForm.Handle, CM_UpdateView, 0, 0);
    PostMessage(FParentForm.Handle, CM_UpdateProgress, XPView.Items.Count, XPView.Items.Count);
  end;
end;

// -----------------------------------------------------------------------------
//                           ...
// -----------------------------------------------------------------------------

end.
