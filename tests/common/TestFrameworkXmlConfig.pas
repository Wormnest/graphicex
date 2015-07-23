// Inspired by
// http://www.uweraabe.de/Blog/2012/03/17/a-dunit-folder-iterator-extension/

// Purpose: Read xml config files in a folder tree that specify which files
// should be tested and may specify extra conditions.
// This implementations is specifically used for testing an image loader/reader.

unit TestFrameworkXmlConfig;

interface

uses
  {$IFNDEF FPC}
  TestFramework;
  {$ELSE}
  fpcunit, testregistry;
  {$ENDIF}

const
  CRootName     = 'Test';
  CTestFiles    = 'TestFiles';
  CFile         = 'File';
  CTestSubject  = 'TestSubject';
  CCompareWith  = 'CompareWith';
  CName         = 'Name';
  CPage         = 'Page';
  CReadable     = 'Readable';
  CComparison   = 'Comparison';

type
  TImageTestCaseClass = class of TImageTestCase;
  TImageTestCase = class(TTestCase)
  private
    FTestFileName: string;
    FTestPage: Cardinal;
    FCompareFileName: string;
  public
    constructor Create(const AMethodName, ATestFileName: string; ATestPage: Cardinal;
      ACompareFileName: string); reintroduce; overload; virtual;
    {$IFNDEF FPC}
    function GetName: string; override;
    {$ELSE}
    function GetTestName: string; override;
    {$ENDIF}
    property TestFileName: string read FTestFileName;
    property TestPage: Cardinal read FTestPage;
    property CompareFileName: string read FCompareFileName;
  end;

  TExtendedTestSuite = class(TTestSuite)
  private
  protected
    // Called from AddTests
    procedure AddMethodTests(TestClass: TTestCaseClass; const NameOfMethod: string); overload; virtual;
  public
    {$IFDEF FPC}
    // Change Create to make it DUnit compatible by calling AddTests.
    constructor Create(AClass: TClass); override;
    {$ENDIF}

    procedure AddTests(TestClass: TTestCaseClass); {$IFNDEF FPC} override; {$ELSE} virtual; {$ENDIF}
    {$IFDEF FPC}
    // DUnit compatible procedure
    procedure AddSuite(Suite: TTestSuite); virtual;
    {$ENDIF}
  end;

  TImageReaderTestSuite = class(TExtendedTestSuite)
  private
    FBaseFolder: string;
    FXmlFile: string;
    FRecursive: Boolean;

    FRootName,
    FTestFiles,
    FFileToTest,
    FTestSubject,
    FCompareWith,
    FName,
    FPage,
    FReadable,
    FComparison: string;
  protected
    procedure AddMethodTests(TestClass: TTestCaseClass; const NameOfMethod: string); override;
    {$IFNDEF FPC}
    procedure ProcessXml(Suite: ITestSuite; TestClass: TImageTestCaseClass; const NameOfMethod, Path, XmlFile: string;
        Recursive: Boolean); overload; virtual;
    {$ELSE}
    procedure ProcessXml(Suite: TTestSuite; TestClass: TImageTestCaseClass; const NameOfMethod, Path, XmlFile: string;
        Recursive: Boolean); overload; virtual;
    {$ENDIF}

  public
    constructor Create(TestClass: TImageTestCaseClass; const ABaseFolder, AXmlFile: string; ARecursive: Boolean); overload;
    property BaseFolder: string read FBaseFolder;
    property XmlFile: string read FXmlFile;
    property Recursive: Boolean read FRecursive;

    // Default Names of Xml Names and Properties
    property RootName: string read FRootName; //default CRootName;
    property TestFiles: string read FTestFiles; //default CTestFiles;
    property FileToTest: string read FFileToTest; //default CFile;
    property TestSubject: string read FTestSubject; //default CTestSubject;
    property CompareWith: string read FCompareWith; //default CCompareWith;
    property Name: string read FName; //default CName;
    property Page: string read FPage; //default CPage;
    property Readable: string read FReadable; //default CReadable;
    property Comparison: string read FComparison; //default CComparison;
  end;

procedure AddImageReaderTests(ASuitePath, ARootPath, AXmlFileName: string;
  ATestClass: TImageTestCaseClass);

implementation

uses
  SysUtils,
  Classes,
  Types,
  {$IFDEF FPC}
  testutils, // GetMethodList
  {$ENDIF}
  JclFileUtils,
  JclSimpleXml;


{$IFDEF FPC}
constructor TExtendedTestSuite.Create(AClass: TClass);
begin
  TAssert.AssertNotNull(AClass);
  Create(AClass.ClassName);
  if AClass.InheritsFrom(TTestCase) then
  begin
    // Replace the inline part of the original Create with a call to AddTests
    // for DUnit compatibility.
    AddTests(TTestCaseClass(AClass));
  end
  else
    AddTest(Warning(AClass.ClassName + SNoValidInheritance));
  if Tests.Count = 0 then
    AddTest(Warning(SNoValidTests + AClass.ClassName));
end;
{$ENDIF}

{$IFNDEF FPC}
procedure TExtendedTestSuite.AddTests(testClass: TTestCaseClass);
var
  MethodIter     :  Integer;
  NameOfMethod   :  string;
  MethodEnumerator:  TMethodEnumerator;
begin
  { call on the method enumerator to get the names of the test
    cases in the testClass }
  MethodEnumerator := nil;
  try
    MethodEnumerator := TMethodEnumerator.Create(testClass);
    { make sure we add each test case  to the list of tests }
    for MethodIter := 0 to MethodEnumerator.Methodcount-1 do begin
      NameOfMethod := MethodEnumerator.nameOfMethod[MethodIter];
      AddMethodTests(testClass, NameOfMethod);
    end;
  finally
    MethodEnumerator.free;
  end;
end;

{$ELSE}

// DUnit compatible AddTests that calls AddMethodTests making it easier to
// only override the relevant part (AddMethodTests).
procedure TExtendedTestSuite.AddTests(TestClass: TTestCaseClass);
var
  ml: TStringList;
  i: integer;
  tc: TTestCaseClass;
begin
  tc := TestClass;
  ml := TStringList.Create;
  try
    GetMethodList(TestClass, ml);
    for i := 0 to ml.Count -1 do
    begin
      AddMethodTests(TestClass, ml.Strings[i]);
    end;
  finally
    ml.Free;
  end;
end;
{$ENDIF}

procedure TExtendedTestSuite.AddMethodTests(TestClass: TTestCaseClass; const NameOfMethod: string);
begin
  {$IFNDEF FPC}
  Self.AddTest(TestClass.Create(NameOfMethod) as ITest);
  {$ELSE}
  Self.AddTest(TestClass.CreateWith(NameOfMethod, TestClass.ClassName));
  {$ENDIF}
end;

{$IFDEF FPC}
procedure TExtendedTestSuite.AddSuite(Suite: TTestSuite);
begin
  AddTest(Suite);
end;
{$ENDIF}

////////////////////////////////////////////////////////////////////////////////

constructor TImageReaderTestSuite.Create(TestClass: TImageTestCaseClass; const ABaseFolder, AXmlFile: string; ARecursive:
    Boolean);
begin
  FBaseFolder := ABaseFolder;
  FXmlFile := AXmlFile;
  FRecursive := ARecursive;
  // Default names
  FRootName := CRootName;
  FTestFiles := CTestFiles;
  FFileToTest := CFile;
  FTestSubject := CTestSubject;
  FCompareWith := CCompareWith;
  FName := CName;
  FPage := CPage;
  FReadable := CReadable;
  FComparison := CComparison;
  ////////////////
  inherited Create(TestClass);
end;

procedure TImageReaderTestSuite.AddMethodTests(TestClass: TTestCaseClass; const NameOfMethod: string);
var
  {$IFNDEF FPC}
  Suite: ITestSuite;
  {$ELSE}
  Suite: TTestSuite;
  {$ENDIF}
begin
  if TestClass.InheritsFrom(TImageTestCase) then begin
    Suite := TTestSuite.Create(NameOfMethod);
    AddSuite(Suite);
    ProcessXml(Suite, TImageTestCaseClass(TestClass), NameOfMethod, BaseFolder, XmlFile, Recursive);
  end
  else begin
    {$IFNDEF FPC}
    AddTest(TestClass.Create(NameOfMethod));
    {$ELSE}
    AddTest(TestClass.CreateWithName(NameOfMethod));
    {$ENDIF}
  end;
end;

{$IFNDEF FPC}
procedure TImageReaderTestSuite.ProcessXml(Suite: ITestSuite; TestClass: TImageTestCaseClass; const NameOfMethod, Path,
    XmlFile: string; Recursive: Boolean);
{$ELSE}
procedure TImageReaderTestSuite.ProcessXml(Suite: TTestSuite; TestClass: TImageTestCaseClass; const NameOfMethod, Path,
    XmlFile: string; Recursive: Boolean);
{$ENDIF}
var
  i, j: integer;
  // Xml handling
  SimpleXML: TJclSimpleXML;
  Node, FileNode, FileData: TJclSimpleXMLElem;
  Prop: TJclSimpleXMLProp;
  NameOfTest: string;
  ImgFile, ImgPage, CompareFile: string;
  function GetPropValue(ANode: TJclSimpleXMLElem; APropName: string): string;
  var
    Prop: TJclSimpleXMLProp;
  begin
    Prop := ANode.Properties.ItemNamed[APropName];
    if Assigned(Prop) then
      Result := Prop.Value
    else
      Result := '';
  end;
begin

  SimpleXML := TJclSimpleXML.Create;
  try
    SimpleXML.LoadFromFile(Path + XmlFile);
    Node := SimpleXML.Root;
    if (SameText(Node.Name, FRootName)) and (Node.Items.Count = 1) and
      (Node.Items.Item[0].Name = FTestFiles) then begin
      Prop := Node.Properties.ItemNamed[FName];
      if Assigned(Prop) then
        NameOfTest := Prop.Value
      else
        NameOfTest := '<unnamed test>';
      Node := Node.Items.Item[0];
      for i := 0 to Node.Items.Count - 1 do begin
        FileNode := Node.Items.Item[i];
        ImgFile := ''; ImgPage := ''; CompareFile := '';
        for j := 0 to FileNode.Items.Count -1 do begin
          FileData := FileNode.Items[j];
          if SameText(FileData.Name, FTestSubject) then begin
            ImgFile := GetPropValue(FileData, FName);
            ImgPage := GetPropValue(FileData, FPage);
          end
          else if SameText(FileData.Name, FCompareWith) then begin
            CompareFile := GetPropValue(FileData, FName);
          end;
        end;
        Suite.AddTest(TestClass.Create(NameOfMethod, Path+ImgFile, StrToIntDef(ImgPage, 0), Path+CompareFile));
      end;
    end;
  finally
    SimpleXML.Free;
  end;
end;

constructor TImageTestCase.Create(const AMethodName, ATestFileName: string;
  ATestPage: Cardinal; ACompareFileName: string);
begin
  FTestFileName := ATestFileName;
  FTestPage := ATestPage;
  FCompareFileName := ACompareFileName;
  {$IFNDEF FPC}
  inherited Create(AMethodName);
  {$ELSE}
  inherited CreateWithName(AMethodName);
  {$ENDIF}
end;

{$IFNDEF FPC}
function TImageTestCase.GetName: string;
{$ELSE}
function TImageTestCase.GetTestName: string;
{$ENDIF}
begin
  result := ExtractFileName(TestFileName);
end;

procedure AddImageReaderTests(ASuitePath, ARootPath, AXmlFileName: string;
  ATestClass: TImageTestCaseClass);
var
  //CurPath: string;
  AddPath: string;
  FullSuitePath: string;
  procedure AddTestIfXmlExists(APath: string);
  begin
    if FileExists(APath + AXmlFileName) then begin
      AddPath := Copy(APath, Length(ARootPath)+1, Length(APath));
      // Remove dots from path for DUnit since that counts as a path separator
      AddPath := StringReplace(AddPath, '.', '_', [rfReplaceAll]);
      FullSuitePath := ASuitePath + '.' + AddPath;
      RegisterTest(FullSuitePath, TImageReaderTestSuite.Create(ATestClass, APath, AXmlFileName, False));
    end;
  end;
  procedure ParseFolders(ABasePath: string);
  var
    FindInfo: TSearchRec;
    Rslt: Integer;
    NewBase: string;
  begin
    // Search all folders under root folder for a test xml
    Rslt := FindFirst(ABasePath + '*.*', faAnyFile, FindInfo);
    try
      while Rslt = 0 do
      begin
        if (FindInfo.Name <> '.') and (FindInfo.Name <> '..') and
          (FindInfo.Attr and faDirectory = faDirectory) then begin
          NewBase := ABasePath + FindInfo.Name + DirDelimiter;
          // Found a folder: add a test if it has a test xml
          AddTestIfXmlExists(NewBase);
          // Check for folders inside this folder
          ParseFolders(NewBase);
        end;
        Rslt := FindNext(FindInfo);
      end;
    finally
      FindClose(FindInfo);
    end;
  end;
begin
  if ARootPath[Length(ARootPath)] <> DirDelimiter then
    ARootPath := ARootPath + DirDelimiter;

  AddTestIfXmlExists(ARootPath);
  ParseFolders(ARootPath);
end;

end.
