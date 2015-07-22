unit frmXmlGenerator;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, JvBaseDlg, JvBrowseFolder, StdCtrls,
  JclSimpleXml, ExtCtrls;

type
  TForm1 = class(TForm)
    edFolder: TEdit;
    btnBrowse: TButton;
    JvBrowseForFolderDialog1: TJvBrowseForFolderDialog;
    btnRun: TButton;
    Panel1: TPanel;
    lblStatus: TLabel;
    lb1: TListBox;
    Label1: TLabel;
    lb2: TListBox;
    Label2: TLabel;
    Label3: TLabel;
    procedure btnBrowseClick(Sender: TObject);
    procedure btnRunClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure edFolderChange(Sender: TObject);
  private
    { Private declarations }
    IgnoreList: TStringList;
    GlobalIgnoreMasks: TStringList;
  public
    { Public declarations }
    procedure WriteXml(AFolder: string);
    procedure ParseFolders(ABasePath: string);
    procedure ParseFiles(ABasePath: string; FilesNode: TJclSimpleXMLElem; SimpleXML: TJclSimpleXML);
    function IgnoreFile(AFile: string): Boolean;
  end;

var
  Form1: TForm1;

implementation

uses JclFileUtils;

{$R *.dfm}

const
  CDefaultXml = 'unit-tests.xml';
  CDefaultCompareFolder = 'unit-tests-expected';
  // Name of file that tells us this folder should be ignored
  CDefaultIgnoreFolder = 'ignore-folder.txt';
  // Name of file that tells us this folder and all sub folders should be ignored
  CDefaultIgnoreFolderTree = 'ignore-folder-tree.txt';
  // Name of file that contains file masks of files in this folder that should be ignored
  CDefaultIgnoreFiles  = 'ignore-files.txt';    // TODO: NOT YET IMPLEMENTED (use global ignore files)
  // File with global file masks of files to ignore
  CGlobalIgnoreFiles  = 'global-ignore-files.txt';
  // File with global list of extensions to ignore
  CGlobalIgnoreExtensions  = 'global-ignore-extensions.txt';

procedure TForm1.btnBrowseClick(Sender: TObject);
begin
  if JvBrowseForFolderDialog1.Execute then begin
    edFolder.Text := JvBrowseForFolderDialog1.Directory;
  end;
end;

procedure TForm1.WriteXml(AFolder: string);
var
  SimpleXML: TJclSimpleXML;
  RootNode: TJclSimpleXMLElemClassic;
  Node: TJclSimpleXMLElemClassic;
  AProlog: TJclSimpleXMLElemsProlog;
//  Prop: TJclSimpleXMLProp;
begin
  if FileExists(AFolder + CDefaultIgnoreFolder) then
    // This folder should be ignored...
    Exit;
  if AFolder <> '' then begin
    lblStatus.Caption := 'Parsing folder ' + AFolder;

    // I know we shouldn't do this but we're not going to use threads for such a small program
    Application.ProcessMessages;

    SimpleXML := TJclSimpleXML.Create;
    try
      // Set Options: We want it indented to make it easier to edit it by hand
      SimpleXML.Options := SimpleXML.Options + [sxoAutoIndent, sxoKeepWhitespace];
      SimpleXML.IndentString := #9; // tab as indent

      // Add a prolog
      AProlog := TJclSimpleXMLElemsProlog.Create(SimpleXML);
      AProlog.Encoding := 'iso-8859-1';
      SimpleXML.Prolog := AProlog;

      // Set our Root node
      RootNode := TJclSimpleXMLElemClassic.Create(SimpleXML);
      RootNode.Name := 'Test';
      RootNode.Properties.Add('Name', 'all images');
      SimpleXML.Root := RootNode;

      // Add TestFiles node
      Node := TJclSimpleXMLElemClassic.Create(SimpleXML);
      Node.Name := 'TestFiles';
      RootNode.Items.Add(Node);
      Node := TJclSimpleXMLElemClassic(RootNode.Items[0]);

      // Now parse all files in the current folder and add them to our xml
      ParseFiles(AFolder, Node, SimpleXML);

      if Node.Items.Count > 0 then
        // Save our xml file
        SimpleXML.SaveToFile(AFolder + DirDelimiter + CDefaultXml );
    finally
      SimpleXML.Free;
    end;
  end;
end;

function TForm1.IgnoreFile(AFile: string): Boolean;
var i: Integer;
  Ext: string;
begin
  Result := False;
  // Get extension of file and see if it needs to be ignored
  Ext := LowerCase(ExtractFileExt(AFile));
  if IgnoreList.IndexOf(Ext) <> -1 then
    Result := True
  else begin
    for i := 0 to GlobalIgnoreMasks.Count -1 do
      if IsFileNameMatch(AFile, GlobalIgnoreMasks.Strings[i], false) then begin
        Result := True;
        Exit;
      end;
  end;
end;

procedure TForm1.ParseFiles(ABasePath: string; FilesNode: TJclSimpleXMLElem; SimpleXML: TJclSimpleXML);
var
  FindInfo: TSearchRec;
  Rslt: Integer;
  Node, SubNode: TJclSimpleXMLElemClassic;
  CompareFile: string;
  CompareFileCode: Integer;
begin
  // Loop over all files in current folder
  Rslt := FindFirst(ABasePath + '*.*', faAnyFile, FindInfo);
  try
    while Rslt = 0 do
    begin
      if (FindInfo.Attr and faDirectory = 0) then begin
        // Not a folder: add to list of files in xml

        // Check if the file is one that needs to be ignored
        if not IgnoreFile(FindInfo.Name) then begin

          // Create File node
          Node := TJclSimpleXMLElemClassic.Create(SimpleXML);
          Node.Name := 'File';

          // Create TestSubject node
          SubNode := TJclSimpleXMLElemClassic.Create(SimpleXML);
          SubNode.Name := 'TestSubject';
          SubNode.Properties.Add('Name', FindInfo.Name);
          SubNode.Properties.Add('Page', 0);
          SubNode.Properties.Add('Readable', 1);

          CompareFile := CDefaultCompareFolder + DirDelimiter + FindInfo.Name;
          if FileExists(CompareFile) then
            CompareFileCode := 1
          else
            CompareFileCode := 0;
          SubNode.Properties.Add('Comparison', CompareFileCode);
          //SubNode.Value := FindInfo.Name;

          // Add SubNode to Node
          Node.Items.Add(SubNode);

          // Add CompareWith node
          SubNode := TJclSimpleXMLElemClassic.Create(SimpleXML);
          SubNode.Name := 'CompareWith';
          SubNode.Properties.Add('Name', CompareFile);

          // Add SubNode to Node
          Node.Items.Add(SubNode);

          // Add Node to FilesNode
          FilesNode.Items.Add(Node);
        end;
      end;
      Rslt := FindNext(FindInfo);
    end;
  finally
    FindClose(FindInfo);
  end;
end;

procedure TForm1.ParseFolders(ABasePath: string);
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
        (FindInfo.Attr and faDirectory = faDirectory) then
        // It's a folder; make sure it's not a folder we want to ignore
        // like the folder we use for comparison images
        // We igore everything starting with a dot (.git, .hg, .svn, etc)
        // Also ignore CVS folders
        if (FindInfo.Name <> CDefaultCompareFolder) and
           (FindInfo.Name[1] <> '.') and (FindInfo.Name <> 'CVS') then begin
          NewBase := ABasePath + FindInfo.Name + DirDelimiter;
          // Write xml and parse sub folders unless we are told to ignore this folder tree
          if not FileExists(NewBase + CDefaultIgnoreFolderTree) then begin
            WriteXml(NewBase);
            // Check for folders inside this folder
            ParseFolders(NewBase);
          end;
        end;
      Rslt := FindNext(FindInfo);
    end;
  finally
    FindClose(FindInfo);
  end;
end;


procedure TForm1.btnRunClick(Sender: TObject);
begin
  WriteXml(edFolder.Text + DirDelimiter);
  ParseFolders(edFolder.Text + DirDelimiter);
  lblStatus.Caption := 'Done.';
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  GlobalIgnoreMasks := TStringList.Create;
  GlobalIgnoreMasks.Sorted := True;
  IgnoreList := TStringList.Create;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  GlobalIgnoreMasks.Free;
  IgnoreList.Free;
end;

procedure TForm1.edFolderChange(Sender: TObject);
begin
  if FileExists(edFolder.Text + DirDelimiter + CGlobalIgnoreFiles) then begin
    // We have found a file with global ignore masks
    GlobalIgnoreMasks.Clear;
    GlobalIgnoreMasks.LoadFromFile(edFolder.Text + DirDelimiter + CGlobalIgnoreFiles);
    lb1.Items.Assign(GlobalIgnoreMasks);
  end
  else
    lb1.Items.Clear;
  if FileExists(edFolder.Text + DirDelimiter + CGlobalIgnoreExtensions) then begin
    // We have found a file with global ignore extensions
    IgnoreList.Clear;
    IgnoreList.LoadFromFile(edFolder.Text + DirDelimiter + CGlobalIgnoreExtensions);
    lb2.Items.Assign(IgnoreList);
  end
  else
    lb2.Items.Clear;
end;

end.
