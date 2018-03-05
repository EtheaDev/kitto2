{-------------------------------------------------------------------------------
   Copyright 2012-2018 Ethea S.r.l.

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

{$I ..\..\Source\EF\EF.Defines.inc}


{$R 'Template_Empty.res'}
{$R 'Template_Basic.res'}

interface

uses
  SysUtils
  , EF.Tree
  ;

type
  TProjectTemplate = class
  private
    FTemplateName: string;
    FOptions: TEFTree;
    FProjectDirectory: string;
    FProjectName: string;
    FProjectGuid: string;
    procedure SetOptions(const AValue: TEFTree);
    function ReplaceUseKittoBooleanMacro(const AString, AOptionName, AUnitName: string): string;
    function ReplaceUseKittoStringMacro(const AString, AOptionName, AUnitFormat: string): string;
    procedure ExpandMacros(const AFileName: string; const AEncoding: TEncoding);
    procedure SetProjectName(const AValue: string);
    procedure ProcessConfigTemplate(const AFileName: string);
    procedure ExtractFileFromResource(const ABaseDirectory, APathName: string;
      const AOverrideFileName: string = '');
    function GetResourceName(const APathName: string): string;
    function GetProjectVersion: string;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
  public
    class var InstalledTemplates: TArray<string>;
    function GetSupportFileAsString(const AFileName: string; const AEncoding: TEncoding): string;
    property TemplateName: string read FTemplateName write FTemplateName;
    property Options: TEFTree read FOptions write SetOptions;
    property ProjectDirectory: string read FProjectDirectory write FProjectDirectory;
    property ProjectName: string read FProjectName write SetProjectName;
    property ProjectGuid: string read FProjectGuid;
    procedure CreateProject;
  end;

implementation

uses
  Types
  , StrUtils
  , IOUtils
  , Classes
  , EF.Sys
  , EF.Sys.Windows
  , EF.StrUtils
  , EF.YAML
  ;

{ TProjectTemplate }

procedure TProjectTemplate.AfterConstruction;
begin
  inherited;
  FOptions := TEFNode.Create;
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
  LContents := ReplaceText(LContents, '{ProjectVersion}', GetProjectVersion);
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

function TProjectTemplate.GetSupportFileAsString(const AFileName: string; const AEncoding: TEncoding): string;
var
  LResourceName: string;
  LBytes: TBytes;
begin
  LResourceName := GetResourceName(TPath.Combine('Support', AFileName));
  LBytes := GetRCDATAResourceBytes(HInstance, LResourceName);
  if LBytes = nil then
    raise Exception.CreateFmt('No data for resource %s.', [LResourceName]);
  Result := AEncoding.GetString(LBytes);
end;

procedure TProjectTemplate.ProcessConfigTemplate(const AFileName: string);
const
  DB_NAMES: array[0..3] of string = ('Main', 'Other1', 'Other2', 'Other3');
var
  LTree: TEFTree;
  LDBNameIndex: Integer;
  LAuth: string;
  LAC: string;

  procedure AddDatabaseNode(const AProviderName: string);
  var
    LChildNode: TEFNode;
  begin
    LChildNode := LTree.GetNode('Databases').AddChild(DB_NAMES[LDBNameIndex], AProviderName);
    LChildNode.AddChild('Connection').AsYamlString := GetSupportFileAsString('DB.' + AProviderName + '.yaml', TEncoding.UTF8);
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
      LTree.SetString('Auth', LAuth).AsYamlString := GetSupportFileAsString('Auth.' + LAuth + '.yaml', TEncoding.UTF8);

    LAC := Options.GetString('AC');
    if LAC <> '' then
      LTree.SetString('AccessControl', LAC).AsYamlString := GetSupportFileAsString('AC.' + LAC + '.yaml', TEncoding.UTF8);

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

function TProjectTemplate.GetResourceName(const APathName: string): string;
begin
  Assert(FTemplateName <> '');

  Result := (FTemplateName + '_' + APathName).Replace(PathDelim, '_').Replace('.', '_').ToUpper;
end;

function TProjectTemplate.GetProjectVersion: string;
begin
{ TODO : Include all supported versions }
  {$IF CompilerVersion >= 19}
  Result := '19.0';
  {$ELSE}
    {$IF CompilerVersion = 18}
    Result := '18.0';
    {$ELSE}
      {$IF CompilerVersion = 17}
      Result := '17.0';
      {$IFEND}
    {$IFEND}
  {$IFEND}
end;

procedure TProjectTemplate.ExtractFileFromResource(const ABaseDirectory, APathName: string;
  const AOverrideFileName: string = '');
var
  LBytes: TBytes;
  LFileName: string;
  LFileStream: TFileStream;
  LPath: string;
  LName: string;
  LExt: string;
begin
  LBytes := GetRCDATAResourceBytes(HInstance, GetResourceName(APathName));
  if LBytes <> nil then
  begin
    LFileName := TPath.Combine(ABaseDirectory, APathName);
    if AOverrideFileName <> '' then
    begin
      LPath := ExtractFileName(LFileName);
      LName := ExtractFileName(LFileName);
      LExt := ExtractFileExt(LFileName);
      LFileName := TPath.Combine(LPath, AOverrideFileName + LExt);
    end;
    LFileStream := TFileStream.Create(LFileName, fmCreate or fmShareDenyNone);
    try
      LFileStream.Write(LBytes, Length(LBytes));
    finally
      FreeAndNil(LFileStream);
    end;
  end;
end;

procedure TProjectTemplate.CreateProject;
var
  LTempDir: string;
begin
  Assert(ProjectName <> '');
  Assert(ProjectDirectory <> '');
  Assert(TemplateName <> '');

  LTempDir := GetUniqueDirectoryName(GetTempDirectory);
  try
    ExtractFileFromResource(LTempDir, 'Home\Metadata\Config.yaml');
    ExtractFileFromResource(LTempDir, 'Home\Metadata\Models\Sample.yaml');
    ExtractFileFromResource(LTempDir, 'Home\Resources\js\application.css');
    ExtractFileFromResource(LTempDir, 'Home\Resources\js\application.js');
    ExtractFileFromResource(LTempDir, 'Home\Project.kproj', ProjectName);
    ExtractFileFromResource(LTempDir, 'Source\Project.dproj', ProjectName);
    ExtractFileFromResource(LTempDir, 'Source\Project.dpr', ProjectName);
    ExtractFileFromResource(LTempDir, 'Source\Project.res', ProjectName);
    ExtractFileFromResource(LTempDir, 'Source\Controllers.pas');
    ExtractFileFromResource(LTempDir, 'Source\Rules.pas');
    ExtractFileFromResource(LTempDir, 'Source\UseKitto.pas');

    CopyAllFilesAndFolders(LTempDir, ProjectDirectory,
      // before each file
      procedure (const ASourceFileName: string; var ADestinationFileName: string; var AAllow: Boolean)
      begin
        AAllow := True;
      end,
      // after each file
      procedure (const ASourceFileName, ADestinationFileName: string)
      begin
        if SameText(ExtractFileExt(ADestinationFileName), '.dproj') then
          ExpandMacros(ADestinationFileName, TEncoding.UTF8)
        else if MatchText(ExtractFileExt(ADestinationFileName), ['.dpr', '.pas']) then
          ExpandMacros(ADestinationFileName, TEncoding.UTF8)
        else if MatchText(ExtractFileName(ADestinationFileName), ['Config.yaml']) then
          ProcessConfigTemplate(ADestinationFileName)
        else if MatchText(ExtractFileExt(ADestinationFileName), ['.yaml']) then
          ExpandMacros(ADestinationFileName, TEncoding.UTF8);
      end
    );
  finally
    DeleteTree(LTempDir);
  end;
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

initialization
  TProjectTemplate.InstalledTemplates := ['Empty', 'Basic'];

end.
