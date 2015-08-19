unit DU_ImageReader_Tests;

interface

uses
  GraphicEx,
  gexBmpWrapper,
  gexJpegWrapper,
  gexXCF,
  Graphics,
  {$IFNDEF FPC}
  TestFramework,
  {$ELSE}
  fpcunit,
  {$ENDIF}
  TestFrameworkXmlConfig;

type
  TImageReadingTestsClass = class of TImageReadingTests;
  TImageReadingTests = class(TImageTestCase)
  private
    Graphic: TGraphic;
  public
    {$IFNDEF FPC}
    class function Suite: ITestSuite; override;
    {$ELSE}
    class function Suite: TTestSuite; virtual;
    {$ENDIF}

    procedure SetUp; override;
    procedure TearDown; override;

    function DetermineImageFormat(AImage: string): TGraphicExGraphicClass;
    procedure ExpectExceptionReadingImage;
    procedure DoTestReadImage;
  published
    procedure TestReadImage;
  end;

implementation

uses SysUtils, Classes,
  {$IFDEF HEAPTRC_LOG}
  heaptrc,
  {$ENDIF}
  GraphicStrings;

const
  ImagesBasePath = 'E:\Delphi\Projects\Transcript\test-images\';
  XmlConfig_FileName = 'unit-tests.xml';

var
  BrushesLoaded: Boolean = False;

procedure TImageReadingTests.SetUp;
var gc: TGraphicClass;
  i: TBitmap;
begin
  // The FileFormat tests may have left the FileFormat list without our bmp wrapper class.
  // Thus make sure it gets initialized to our bmp wrapper class.
  gc := FileFormatList.GraphicFromExtension('.bmp');
  if (FileFormatList.GraphicFromExtension('bmp') = nil) or
     (gc <> TgexBmpGraphic) then begin
    FileFormatList.UnregisterFileFormat('bmp', Graphics.TBitmap);
    FileFormatList.RegisterFileFormat('bmp', gesBitmaps, '', [ftRaster], False, TgexBmpGraphic);
  end;

  if not BrushesLoaded and (LowerCase(ExtractFileExt(TestFileName)) = '.png') then
  begin
    // Try to remove false positives by selecting brushes that the PNG loading
    // later on may use too thereby causing them to be already loaded in cache.
    i := TBitmap.Create;
    i.Width := 1; i.Height := 1;
    try
      // TODO: Is there a better (faster) way than FillRect to make sure that
      // the brush gets loaded into cache?
      i.Canvas.Brush.Color := TColor($000000); i.Canvas.FillRect(Rect(0, 0, 1, 1));
      i.Canvas.Brush.Color := TColor($ffffff); i.Canvas.FillRect(Rect(0, 0, 1, 1));
      i.Canvas.Brush.Color := TColor($fefefe); i.Canvas.FillRect(Rect(0, 0, 1, 1));
      i.Canvas.Brush.Color := TColor($00ffff); i.Canvas.FillRect(Rect(0, 0, 1, 1));
      i.Canvas.Brush.Color := TColor($0000ff); i.Canvas.FillRect(Rect(0, 0, 1, 1));
      i.Canvas.Brush.Color := TColor($00a0ff); i.Canvas.FillRect(Rect(0, 0, 1, 1));
      i.Canvas.Brush.Color := TColor($00ff00); i.Canvas.FillRect(Rect(0, 0, 1, 1));
      i.Canvas.Brush.Color := TColor($ff0000); i.Canvas.FillRect(Rect(0, 0, 1, 1));
      i.Canvas.Brush.Color := TColor($80e0e0); i.Canvas.FillRect(Rect(0, 0, 1, 1));
      i.Canvas.Brush.Color := TColor($999999); i.Canvas.FillRect(Rect(0, 0, 1, 1));
      i.Canvas.Brush.Color := TColor($0000aa); i.Canvas.FillRect(Rect(0, 0, 1, 1));
      i.Canvas.Brush.Color := TColor($00000a); i.Canvas.FillRect(Rect(0, 0, 1, 1));
      i.Canvas.Brush.Color := TColor($0000f5); i.Canvas.FillRect(Rect(0, 0, 1, 1));
      i.Canvas.Brush.Color := TColor($0000ad); i.Canvas.FillRect(Rect(0, 0, 1, 1));
      i.Canvas.Brush.Color := TColor($0000c2); i.Canvas.FillRect(Rect(0, 0, 1, 1));
      i.Canvas.Brush.Color := TColor($00001f); i.Canvas.FillRect(Rect(0, 0, 1, 1));
      i.Canvas.Brush.Color := TColor($000013); i.Canvas.FillRect(Rect(0, 0, 1, 1));
      BrushesLoaded := True;
    finally
      i.Free;
    end;
  end;
end;

procedure TImageReadingTests.TearDown;
begin
  // Make sure Graphic gets freed even if test fails.
  Graphic.Free;
end;

function TImageReadingTests.DetermineImageFormat(AImage: string): TGraphicExGraphicClass;
var GraphicClass: TGraphicExGraphicClass;
  SecondCaseClass: TGraphicClass;
  Ext: string;
begin
  GraphicClass := FileFormatList.GraphicFromContent(AImage);
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
    Ext := LowerCase(ExtractFileExt(AImage));
    if Ext = '.cut' then begin
      SecondCaseClass := FileFormatList.GraphicFromExtension(AImage);
      if (SecondCaseClass <> nil) and SecondCaseClass.InheritsFrom(TGraphicExGraphic) then
        GraphicClass := TGraphicExGraphicClass(SecondCaseClass);
    end;
  end;
  Result := GraphicClass;
end;

procedure TImageReadingTests.ExpectExceptionReadingImage;
begin
  // Load image for which we are expecting an exception while trying to read it...
  TGraphicExGraphic(Graphic).LoadFromFileByIndex(TestFileName, TestPage);
end;

procedure TImageReadingTests.DoTestReadImage;
var GraphicClass: TGraphicExGraphicClass;
begin
  // Determine type of image
  GraphicClass := DetermineImageFormat(TestFileName);
  if not Unrecognized then
    Check( GraphicClass <> nil, Format('Failed to determine image format of %s!', [TestFileName]))
  else begin
    // should not be recognized as an image
    Check( GraphicClass = nil, Format('%s should not be recognized as an image!', [TestFileName]));
    Exit;
  end;

  // Create correct graphic image format
  Graphic := GraphicClass.Create;
  // On purpose no try finally since we are testing
  Check( Graphic <> nil, Format('Failed to create Graphic for %s!', [TestFileName]));

  if Readable then begin
    // Load image
    TGraphicExGraphic(Graphic).LoadFromFileByIndex(TestFileName, TestPage);
    if not EmptyImage then
      Check(Graphic.Empty = False, Format('Failed to load image %s!', [TestFileName]))
    else
      Check(Graphic.Empty = True, Format('%s is unexpectedly not empty!', [TestFileName]));
  end
  else
    AssertException('We didn''t get the exception we expected!', ExceptionType,
      {$IFDEF FPC}@{$ENDIF}ExpectExceptionReadingImage, ExceptionMessage);

  Graphic.Free;
  Graphic := nil;
end;

procedure TImageReadingTests.TestReadImage;
{$IFDEF FPC}
var
  fpcHeapStatus : TFPCHeapStatus;
  FpcUsedHeap: PtrUInt;
{$ENDIF}
begin
  {$IFDEF FPC}
  // Get initial heap state
  fpcHeapStatus := GetFPCHeapStatus();
  FpcUsedHeap := fpcHeapStatus.CurrHeapUsed;
  {$ELSE}
  //  FailsOnMemoryLeak := True; // Disabled: too many false positives
  {$ENDIF}

  DoTestReadImage;

  {$IFDEF FPC}
  // Get heap state after tests are done
  fpcHeapStatus := GetFPCHeapStatus();
  {$IFDEF HEAPTRC_LOG}
  if FpcUsedHeap <> fpcHeapStatus.CurrHeapUsed then
    DumpHeap;
  {$ENDIF}
  // Check if there was a memory leak
  // Note that we currently have some known false positives
  // In Fpc:
  // When reading PNG images a canvas may be used. When a new type of brush is
  // used that brush is cached causing allocation of memory that will not be
  // freed at this point. It will be freed when we close our program.
  Check(FpcUsedHeap = fpcHeapStatus.CurrHeapUsed, Format('Memory leak detected: %u bytes',[fpcHeapStatus.CurrHeapUsed - FpcUsedHeap]));
  {$ENDIF}
end;

{$IFNDEF FPC}
class function TImageReadingTests.Suite: ITestSuite;
{$ELSE}
class function TImageReadingTests.Suite: TTestSuite;
{$ENDIF}
begin
  Result := TImageReaderTestSuite.Create(self, ImagesBasePath, XmlConfig_FileName, False);
end;


var
  ImageReadingTestsClass: TImageReadingTestsClass;

initialization
  ImageReadingTestsClass := TImageReadingTests;
  AddImageReaderTests('Test Loading Image Formats', ImagesBasePath,
    XmlConfig_FileName, ImageReadingTestsClass);
end.
