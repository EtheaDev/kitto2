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

{$I ..\..\Source\EF\EF.Defines.inc}


{$R 'Template_Empty.res'}
{$R 'Template_Basic.res'}

interface

uses
  SysUtils
  , Generics.Collections
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
    FModuleSources: TDictionary<string, string>;
    procedure SetOptions(const AValue: TEFTree);
    function ReplaceUseKittoBooleanMacro(const AString, AOptionName, AUnitName: string): string;
    function ReplaceUseKittoStringMacro(const AString, AOptionName, AUnitFormat: string): string;
    function ExpandMacros(const AString: string): string;
    procedure SetProjectName(const AValue: string);
    function ProcessConfigTemplate(const AString: string): string;
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

//  GetModuleSource('Home\Metadata\Config.yaml');
//  GetModuleSource('Home\Metadata\Models\Sample.yaml');
//  GetModuleSource('Home\Resources\js\application.css');
//  GetModuleSource('Home\Resources\js\application.js');
//  GetModuleSource('Home\Project.kproj');
//  GetModuleSource('Source\Controllers.pas');
//  GetModuleSource('Source\Rules.pas');
//  GetModuleSource('Source\UseKitto.pas');
    function GetProjectSource: string;
    function GetModuleSource(const APathName: string): string;
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

  , dialogs
  ;

{ TProjectTemplate }

procedure TProjectTemplate.AfterConstruction;
begin
  inherited;
  FOptions := TEFNode.Create;
  FModuleSources := TDictionary<string, string>.Create;
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

function TProjectTemplate.ExpandMacros(const AString: string): string;
begin
  Result := AString;
  Result := ReplaceText(Result, '{ProjectName}', ProjectName);
  Result := ReplaceText(Result, '{ProjectGuid}', ProjectGuid);
  Result := ReplaceText(Result, '{ProjectVersion}', GetProjectVersion);
  Result := ReplaceText(Result, '{AppTitle}', Options.GetString('AppTitle'));
  Result := ReplaceUseKittoBooleanMacro(Result, 'DB/ADO', 'EF.DB.ADO');
  Result := ReplaceUseKittoBooleanMacro(Result, 'DB/DBX', 'EF.DB.DBX');
  Result := ReplaceUseKittoBooleanMacro(Result, 'DB/FD', 'EF.DB.FD');
  Result := ReplaceUseKittoStringMacro(Result, 'Auth', sLineBreak + 'Kitto.Auth.%s,');
  Result := ReplaceUseKittoStringMacro(Result, 'AC', sLineBreak + 'Kitto.AccessControl.%s,');
  Result := ReplaceText(Result, '{KittoPath}', Options.GetString('SearchPath'));
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

function TProjectTemplate.ProcessConfigTemplate(const AString: string): string;
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
  LTree := TEFYAMLReader.LoadTreeFromString(AString);
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

    Result := ExpandMacros(TEFYAMLWriter.TreeAsString(LTree));
  finally
    FreeAndNil(LTree);
  end;
end;

function TProjectTemplate.GetResourceName(const APathName: string): string;
begin
  Assert(FTemplateName <> '');

  Result := (FTemplateName + '_' + APathName).Replace(PathDelim, '_').Replace('.', '_').ToUpper;
end;

function TProjectTemplate.GetProjectSource: string;
begin
  Result := GetModuleSource('Source\Project.dpr');
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

function TProjectTemplate.GetModuleSource(const APathName: string): string;
var
  LResourceName: string;
  LBytes: TBytes;
begin
  Result := '';
  LResourceName := GetResourceName(APathName);
  if not FModuleSources.ContainsKey(LResourceName) then
  begin
    LBytes := GetRCDATAResourceBytes(HInstance, LResourceName);
    if Assigned(LBytes) then
    begin
      Result := TEncoding.UTF8.GetString(LBytes);
      if SameText(ExtractFileName(APathName), 'Config.yaml') then
        Result := ProcessConfigTemplate(Result)
      else
        Result := ExpandMacros(Result);
      FModuleSources.Add(LResourceName, Result);
    end;
  end
  else
    Result :=  FModuleSources[LResourceName];
end;

destructor TProjectTemplate.Destroy;
begin
  FreeAndNil(FModuleSources);
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
//  TProjectTemplate.InstalledTemplates := ['Empty', 'Basic'];

end.
