// http://www.uweraabe.de/Blog/2012/03/17/a-dunit-folder-iterator-extension/
// jb adapted because IOUtils doesn't exist until D2010

unit TestFrameworkExt;

interface

uses
  TestFramework;

type
  TFileTestCaseClass = class of TFileTestCase;
  TFileTestCase = class(TTestCase)
  private
    FTestFileName: string;
  public
    constructor Create(const AMethodName, ATestFileName: string); reintroduce; overload; virtual;
    function GetName: string; override;
    property TestFileName: string read FTestFileName;
  end;

  TFolderTestSuite = class(TTestSuite)
  private
    FBaseFolder: string;
    FFileMask: string;
    FRecursive: Boolean;
  protected
    procedure AddMethodTests(TestClass: TTestCaseClass; const NameOfMethod: string); overload; virtual;
    procedure ProcessFile(Suite: ITestSuite; TestClass: TFileTestCaseClass; const NameOfMethod, FileName: string); virtual;
    procedure ProcessFolder(Suite: ITestSuite; TestClass: TFileTestCaseClass; const NameOfMethod, Path, FileMask: string;
        Recursive: Boolean); overload; virtual;
    procedure ProcessBaseFolder(Suite: ITestSuite; TestClass: TFileTestCaseClass; const NameOfMethod: string); virtual;
  public
    constructor Create(TestClass: TFileTestCaseClass; const ABaseFolder, AFileMask: string; ARecursive: Boolean); overload;
    procedure AddTests(testClass: TTestCaseClass); override;
    property BaseFolder: string read FBaseFolder;
    property FileMask: string read FFileMask;
    property Recursive: Boolean read FRecursive;
  end;

implementation

uses
  SysUtils,
  Classes,
  Types, {IOUtils, }
  JclFileUtils;

constructor TFolderTestSuite.Create(TestClass: TFileTestCaseClass; const ABaseFolder, AFileMask: string; ARecursive:
    Boolean);
begin
  FBaseFolder := ABaseFolder;
  FFileMask := AFileMask;
  FRecursive := ARecursive;
  inherited Create(TestClass);
end;

procedure TFolderTestSuite.AddMethodTests(TestClass: TTestCaseClass; const NameOfMethod: string);
var
  suite: ITestSuite;
begin
  if TestClass.InheritsFrom(TFileTestCase) then begin
    suite := TTestSuite.Create(NameOfMethod);
    AddSuite(suite);
    ProcessBaseFolder(suite, TFileTestCaseClass(TestClass), NameOfMethod);
  end
  else begin
    AddTest(TestClass.Create(NameOfMethod));
  end;
end;

procedure TFolderTestSuite.AddTests(testClass: TTestCaseClass);
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

procedure TFolderTestSuite.ProcessBaseFolder(Suite: ITestSuite; TestClass: TFileTestCaseClass; const NameOfMethod:
    string);
begin
  ProcessFolder(Suite, TestClass, NameOfMethod, BaseFolder, FileMask, Recursive);
end;

procedure TFolderTestSuite.ProcessFile(Suite: ITestSuite; TestClass: TFileTestCaseClass; const NameOfMethod, FileName:
    string);
begin
  suite.AddTest(TestClass.Create(NameOfMethod, FileName));
end;

procedure TFolderTestSuite.ProcessFolder(Suite: ITestSuite; TestClass: TFileTestCaseClass; const NameOfMethod, Path,
    FileMask: string; Recursive: Boolean);
var
  //list: TStringDynArray;
  //S: string;
  //tmp: ITestSuite;
  // jb below
  //FileEnumerator: TJclFileEnumerator;
  FileList: TStringList;
  i: integer;
  PathAndMask: string;
begin
//  FileEnumerator := TJclFileEnumerator.Create;
  FileList := TStringList.Create;
//  FileEnumerator.RootDirectory := Path;
//  FileEnumerator.FileMask := FileMask;
//  FileEnumerator.FillList(FileList);
  if Path[Length(Path)] <> '\' then
    PathAndMask := Path + '\' + FileMask
  else
    PathAndMask := Path + FileMask;

  BuildFileList(PathAndMask, faAnyFile, FileList, False);
  for i := 0 to FileList.Count-1 do
    ProcessFile(suite, TestClass, NameOfMethod, FileList.Strings[i]);
  FileList.Free;
//  FileEnumerator.Free;
  // TODO: Support for Recursive!!!!!!!!!!!!
  
{
  list := TDirectory.GetFiles(Path, FileMask);
  for S in list do
    ProcessFile(suite, TestClass, NameOfMethod, S);
  if Recursive then begin
    list := TDirectory.GetDirectories(path);
    for S in list do begin
      tmp := TTestSuite.Create(TPath.GetFileName(S));
      suite.AddSuite(tmp);
      ProcessFolder(tmp, TestClass, NameOfMethod, S, FileMask, true);
    end;
  end;
}
end;

constructor TFileTestCase.Create(const AMethodName, ATestFileName: string);
begin
  FTestFileName := ATestFileName;
  inherited Create(AMethodName);
end;

function TFileTestCase.GetName: string;
begin
/////////////////  result := TPath.GetFileNameWithoutExtension(TestFileName);
  result := PathExtractFileNameNoExt(TestFileName);
end;

end.
