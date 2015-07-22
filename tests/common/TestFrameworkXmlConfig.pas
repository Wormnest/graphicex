// Inspired by
// http://www.uweraabe.de/Blog/2012/03/17/a-dunit-folder-iterator-extension/

// Purpose: Read xml config files in a folder tree that specify which files
// should be tested and may specify extra conditions.
// This implementations is specifically used for testing an image loader/reader.

unit TestFrameworkXmlConfig;

interface

uses
  TestFramework;

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
    function GetName: string; override;
    property TestFileName: string read FTestFileName;
    property TestPage: Cardinal read FTestPage;
    property CompareFileName: string read FCompareFileName;
  end;

  TImageReaderTestSuite = class(TTestSuite)
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
    procedure AddMethodTests(TestClass: TTestCaseClass; const NameOfMethod: string); overload; virtual;
    procedure ProcessXml(Suite: ITestSuite; TestClass: TImageTestCaseClass; const NameOfMethod, Path, XmlFile: string;
        Recursive: Boolean); overload; virtual;

  public
    constructor Create(TestClass: TImageTestCaseClass; const ABaseFolder, AXmlFile: string; ARecursive: Boolean); overload;
    procedure AddTests(testClass: TTestCaseClass); override;
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
  JclFileUtils,
  JclSimpleXml;

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
  Suite: ITestSuite;
begin
  if TestClass.InheritsFrom(TImageTestCase) then begin
    Suite := TTestSuite.Create(NameOfMethod);
    AddSuite(Suite);
    ProcessXml(Suite, TImageTestCaseClass(TestClass), NameOfMethod, BaseFolder, XmlFile, Recursive);
  end
  else begin
    AddTest(TestClass.Create(NameOfMethod));
  end;
end;

procedure TImageReaderTestSuite.AddTests(testClass: TTestCaseClass);
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

procedure TImageReaderTestSuite.ProcessXml(Suite: ITestSuite; TestClass: TImageTestCaseClass; const NameOfMethod, Path,
    XmlFile: string; Recursive: Boolean);
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
  inherited Create(AMethodName);
end;

function TImageTestCase.GetName: string;
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
