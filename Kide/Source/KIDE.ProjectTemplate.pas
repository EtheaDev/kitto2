{-------------------------------------------------------------------------------
   Copyright 2012-2021 Ethea S.r.l.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
-------------------------------------------------------------------------------}
unit KIDE.ProjectTemplate;

interface

uses
  SysUtils,
  EF.Tree;

type
  TProjectTemplate = class
  private
    FTemplateDirectory: string;
    FOptions: TEFTree;
    FProjectDirectory: string;
    FProjectName: string;
    FProjectGuid: string;
    procedure SetOptions(const AValue: TEFTree);
    procedure CheckRemoveDelphiVersionFiles(const ADelphiVersion: string);
    function ReplaceUseKittoBooleanMacro(const AString, AOptionName,
      AUnitName: string): string;
    function ReplaceUseKittoStringMacro(const AString, AOptionName,
      AUnitFormat: string): string;
    procedure ExpandMacros(const AFileName: string; const AEncoding: TEncoding);
    procedure SetProjectName(const AValue: string);
    procedure ProcessConfigTemplate(const AFileName: string);
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
    function GetSupportFileName(const AFileName: string): string;
    property TemplateDirectory: string read FTemplateDirectory write FTemplateDirectory;
    property Options: TEFTree read FOptions write SetOptions;
    property ProjectDirectory: string read FProjectDirectory write FProjectDirectory;
    property ProjectName: string read FProjectName write SetProjectName;
    property ProjectGuid: string read FProjectGuid;
    procedure CreateProject;
  end;

implementation

uses
  StrUtils,
  EF.Sys.Windows, EF.StrUtils, EF.YAML;

{ TProjectTemplate }

procedure TProjectTemplate.AfterConstruction;
begin
  inherited;
  FOptions := TEFNode.Create;
end;

procedure TProjectTemplate.CheckRemoveDelphiVersionFiles(const ADelphiVersion: string);
begin
  if not Options.GetBoolean(ADelphiVersion) then
  begin
    DeleteTree(IncludeTrailingPathDelimiter(ProjectDirectory) + 'Projects' + PathDelim + ADelphiVersion);
    DeleteTree(IncludeTrailingPathDelimiter(ProjectDirectory) + 'Lib' + PathDelim + ADelphiVersion);
  end;
end;

function TProjectTemplate.ReplaceUseKittoBooleanMacro(const AString, AOptionName, AUnitName: string): string;
begin
  if Options.GetBoolean(AOptionName) then
    Result := ReplaceText(AString, '{' + AOptionName + '}', AUnitName)
  else
    Result := ReplaceText(AString, '{' + AOptionName + '}', '// ' + AUnitName);
end;

function TProjectTemplate.ReplaceUseKittoStringMacro(const AString, AOptionName, AUnitFormat: string): string;
var
  LOption: string;
  LReplace: string;
begin
  LOption := Options.GetString(AOptionName);
  if LOption = '' then
    LReplace := ''
  else
    LReplace := Format(AUnitFormat, [LOption]);
  Result := ReplaceText(AString, '{' + AOptionName + '}', LReplace);
end;

procedure TProjectTemplate.ExpandMacros(const AFileName: string; const AEncoding: TEncoding);
var
  LContents: string;
begin
  LContents := TextFileToString(AFileName, AEncoding);
  LContents := ReplaceText(LContents, '{ProjectName}', ProjectName);
  LContents := ReplaceText(LContents, '{ProjectGuid}', ProjectGuid);
  LContents := ReplaceText(LContents, '{AppTitle}', Options.GetString('AppTitle'));

  if SameText(ExtractFileFormat(AFileName), 'pas') then
  begin
    LContents := ReplaceUseKittoBooleanMacro(LContents, 'DB/ADO', 'EF.DB.ADO');
    LContents := ReplaceUseKittoBooleanMacro(LContents, 'DB/DBX', 'EF.DB.DBX');
    LContents := ReplaceUseKittoBooleanMacro(LContents, 'DB/FD', 'EF.DB.FD');
    LContents := ReplaceUseKittoStringMacro(LContents, 'Auth', sLineBreak + 'Kitto.Auth.%s,');
    LContents := ReplaceUseKittoStringMacro(LContents, 'AC', sLineBreak + 'Kitto.AccessControl.%s,');
  end
  else if SameText(ExtractFileFormat(AFileName), 'dproj') then
    LContents := ReplaceText(LContents, '{KittoPath}', Options.GetString('SearchPath'));

  StringToTextFile(LContents, AFileName, AEncoding);
end;

function TProjectTemplate.GetSupportFileName(const AFileName: string): string;
begin
  Result := IncludeTrailingPathDelimiter(TemplateDirectory) + '_Support' + PathDelim + AFileName;
end;

procedure TProjectTemplate.ProcessConfigTemplate(const AFileName: string);
const
  DB_NAMES: array[0..3] of string = ('Main', 'Other1', 'Other2', 'Other3');
var
  LTree: TEFTree;
  LDBNameIndex: Integer;
  LAuth: string;
  LAC: string;
  I: Integer;

  procedure AddDatabaseNode(const AProviderName: string);
  var
    LChildNode: TEFNode;
  begin
    LChildNode := LTree.GetNode('Databases').AddChild(DB_NAMES[LDBNameIndex], AProviderName);
    LChildNode.AddChild('Connection').LoadFromYamlFile(GetSupportFileName('DB.' + AProviderName + '.yaml'));
    Inc(LDBNameIndex);
  end;

begin
  LTree := TEFYAMLReader.LoadTree(AFileName);
  try
    if Options.GetBoolean('DB/ADO') or Options.GetBoolean('DB/DBX') or Options.GetBoolean('DB/FD') then
    begin
      LDBNameIndex := 0;
      if Options.GetBoolean('DB/ADO') then
        AddDatabaseNode('ADO');
      if Options.GetBoolean('DB/DBX') then
        AddDatabaseNode('DBX');
      if Options.GetBoolean('DB/FD') then
        AddDatabaseNode('FD');
    end
    else
    begin
      LTree.DeleteNode('DefaultDatabaseName');
      LTree.DeleteNode('Databases');
    end;

    LAuth := Options.GetString('Auth');
    if LAuth <> '' then
      LTree.SetString('Auth', LAuth).LoadFromYamlFile(
        GetSupportFileName('Auth.' + LAuth + '.yaml'));

    LAC := Options.GetString('AC');
    if LAC <> '' then
      LTree.SetString('AccessControl', LAC).LoadFromYamlFile(
        GetSupportFileName('AC.' + LAC + '.yaml'));

    LTree.SetString('ExtJS/Theme', Options.GetString('ExtJS/Theme'));
    LTree.SetString('LanguageId', Options.GetString('LanguageId'));
    LTree.SetString('Charset', Options.GetString('Charset'));
    LTree.SetString('Server/Port', Options.GetString('Server/Port'));
    LTree.SetString('Server/ThreadPoolSize', Options.GetString('Server/ThreadPoolSize'));
    LTree.SetString('Server/SessionTimeOut', Options.GetString('Server/SessionTimeOut'));

    TEFYAMLWriter.SaveTree(LTree, AFileName);
    ExpandMacros(AFileName, TEncoding.UTF8);
  finally
    FreeAndNil(LTree);
  end;
end;

procedure TProjectTemplate.CreateProject;
begin
  Assert(ProjectName <> '');
  Assert(ProjectDirectory <> '');
  Assert(TemplateDirectory <> '');

  if not DirectoryExists(TemplateDirectory) then
    raise Exception.CreateFmt('Project template directory %s not found.', [TemplateDirectory]);

  CopyAllFilesAndFolders(TemplateDirectory, ProjectDirectory,
    // before each file
    procedure (const ASourceFileName: string; var ADestinationFileName: string; var AAllow: Boolean)
    begin
      AAllow := not ContainsText(ExtractFilePath(ADestinationFileName), '_Support');
      if AAllow then
        // Expand template-level macros in file names.
        ADestinationFileName := ReplaceText(ADestinationFileName, '{ProjectName}', ProjectName);
    end,
    // after each file
    procedure (const ASourceFileName, ADestinationFileName: string)
    begin
      if SameText(ExtractFileExt(ADestinationFileName), '.dproj') then
        ExpandMacros(ADestinationFileName, TEncoding.UTF8)
      else if MatchText(ExtractFileExt(ADestinationFileName), ['.dpr', '.pas']) then
        ExpandMacros(ADestinationFileName, TEncoding.ANSI)
      else if MatchText(ExtractFileName(ADestinationFileName), ['Config.yaml']) then
        ProcessConfigTemplate(ADestinationFileName)
      else if MatchText(ExtractFileExt(ADestinationFileName), ['.yaml']) then
        ExpandMacros(ADestinationFileName, TEncoding.UTF8);
    end
  );
  CheckRemoveDelphiVersionFiles('DXE7');
  CheckRemoveDelphiVersionFiles('DXE8');
  CheckRemoveDelphiVersionFiles('D10');
  CheckRemoveDelphiVersionFiles('D10_1');
  CheckRemoveDelphiVersionFiles('D10_2');
  DeleteTree(IncludeTrailingPathDelimiter(ProjectDirectory) + '_Support');
end;

destructor TProjectTemplate.Destroy;
begin
  FreeAndNil(FOptions);
  inherited;
end;

procedure TProjectTemplate.SetOptions(const AValue: TEFTree);
begin
  FOptions.Assign(AValue);
end;

procedure TProjectTemplate.SetProjectName(const AValue: string);
begin
  FProjectName := AValue;
  FProjectGuid := CreateGuidStr;
end;

end.
