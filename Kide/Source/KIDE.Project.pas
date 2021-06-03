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
unit KIDE.Project;

interface

uses
  Classes, Generics.Collections,
  EF.Tree, Kitto.Metadata.Views,
  Kitto.Config, Kitto.Web.Application, Kitto.Web.Server;

type
  TProjectConfig = class(TKConfig)
  end;

  TProject = class(TEFTree)
  private
    FFileName: string;
    FConfig: TProjectConfig;
    FApplication: TKWebApplication;
    FServer: TKWebServer;
    FAppConfigs: TObjectList<TKConfig>;
    FKittoEngineHandle: Cardinal;
    class var FCurrentProject: TProject;
    function GetDirectory: string;
    procedure RefreshConfig;
    class procedure SetCurrentProject(const AValue: TProject); static;
    function GetSourceDirectory: string;
    function GetAppConfigCount: Integer;
    function GetAppConfigs(const AIndex: Integer): TKConfig;
    procedure EnsureAppConfigs;
    procedure ReloadAppConfigs;
    function FindAppConfig(const AFileName: string): TKConfig;
    function GetBaseName: string;
    function GetKittoEngineFileName: string;
    function GetIsKittoEngineRunning: Boolean;
  public
    class property CurrentProject: TProject read FCurrentProject write SetCurrentProject;
    class destructor Destroy;

    class procedure OpenProject(const AFileName: string);
    class procedure CloseProject;

    constructor Create; override;
    destructor Destroy; override;

    property FileName: string read FFileName;
    property BaseName: string read GetBaseName;
    function GetMRUKeyName: string;
    function GetViewImageIndex(const AView: TKView): Integer;
    function GetLayoutImageIndex(const ALayout: TKLayout): Integer;
    property KittoEngineFileName: string read GetKittoEngineFileName;
    property KittoEngineHandle: Cardinal read FKittoEngineHandle write FKittoEngineHandle;
    property IsKittoEngineRunning: Boolean read GetIsKittoEngineRunning;
    procedure KittoEngineClose;
    procedure ShowPreviewAction(const ACommand: string);

    property Directory: string read GetDirectory;
    property SourceDirectory: string read GetSourceDirectory;

    property Config: TProjectConfig read FConfig;
    property Server: TKWebServer read FServer;
    property Application: TKWebApplication read FApplication;

    property AppConfigCount: Integer read GetAppConfigCount;
    property AppConfigs[const AIndex: Integer]: TKConfig read GetAppConfigs;

    procedure GetConfigFileNames(const AFileNames: TStrings;
      const AFullPaths: Boolean = True);
    function GetResourcesPath: string;

    // Project-scoped MRUs.
    procedure StoreMRUString(const AKey, AValue: string);
    procedure StoreMRUInteger(const AKey: string; const AValue: Integer);
    procedure StoreMRUBoolean(const AKey: string; const AValue: Boolean);
    procedure StoreMRUItem(const AKey, AValue: string);
    procedure StoreMRUStrings(const AKey: string; const AStrings: TStrings);
    procedure StoreMRUNode(const APath: string; const ANode: TEFNode);

    procedure RetrieveMRUStrings(const AKey: string; const AStrings: TStrings);
    function RetrieveMRUInteger(const AKey: string; ADefault: Integer = 0): Integer;
    function RetrieveMRUString(const AKey: string; const ADefault: string = ''): string;
    function RetrieveMRUBoolean(const AKey: string; const ADefault: Boolean = False): Boolean;
    function RetrieveMRUNode(const APath: string): TEFNode;
    ///	<summary>
    ///  Refreshes or invalidates metadata and config objects so that
    ///	 they are read anew from disk at next access.
    /// </summary>
    procedure RefreshAll;
    ///	<summary>
    ///  Refreshes or invalidates config objects only so that they are
    ///	 read anew from disk at next access.
    /// </summary>
    procedure RefreshConfigs;
  end;

implementation

uses
  Windows, SysUtils, Messages, StrUtils,
  EF.Shell, EF.Sys.Windows, EF.StrUtils, EF.YAML, EF.Macros,
  KIDE.MRUOptions, KIDE.MainDatamoduleUnit;

{ TProject }

class procedure TProject.CloseProject;
begin
  FreeAndNil(FCurrentProject);
end;

constructor TProject.Create;
begin
  inherited;
  FServer := TKWebServer.Create(nil);
  FApplication := FServer.Engine.AddRoute(TKWebApplication.Create) as TKWebApplication;
end;

destructor TProject.Destroy;
begin
  KittoEngineClose;
  FreeAndNil(FConfig);
  FreeAndNil(FServer);
  FreeAndNil(FAppConfigs);
  inherited;
end;

function TProject.FindAppConfig(const AFileName: string): TKConfig;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FAppConfigs.Count - 1 do
  begin
    if SameFileName(ExtractFileName(FAppConfigs[I].Config.PersistentFileName), AFileName) then
    begin
      Result := FAppConfigs[I];
      Break;
    end;
  end;
end;

procedure TProject.ReloadAppConfigs;
var
  LFileNames: TStrings;
  I: Integer;
  LConfig: TKConfig;
  LBaseConfigFileName: string;
  LAppConfig: TKConfig;

  function FindFileName(const AFileName: string): Boolean;
  var
    I: Integer;
  begin
    Result := False;
    for I := 0 to LFileNames.Count - 1 do
      if SameFileName(LFileNames[I], AFileName) then
        Exit(True);
  end;

begin
  LFileNames := TStringList.Create;
  try
    GetConfigFileNames(LFileNames, False);
    LBaseConfigFileName := TKConfig.BaseConfigFileName;
    try
      // Add new files and update existing files.
      for I := 0 to LFileNames.Count - 1 do
      begin
        LAppConfig := FindAppConfig(LFileNames[I]);
        if Assigned(LAppConfig) then
          LAppConfig.Config.LoadFromYamlFile(LAppConfig.Config.PersistentFileName)
        else
        begin
          TKConfig.BaseConfigFileName := LFileNames[I]; // effective for objects created from now on.
          LConfig := TKConfig.Create;
          try
            LConfig.Config; // Make sure config data is loaded.
            FAppConfigs.Add(LConfig);
          except
            FreeAndNil(LConfig);
            raise;
          end;
        end;
      end;
      // Remove no longer existing files.
      for I := FAppConfigs.Count - 1 downto 0 do
      begin
        if not FindFileName(ExtractFileName(FAppConfigs[I].Config.PersistentFileName)) then
          FAppConfigs.Delete(I);
      end;
    finally
      TKConfig.BaseConfigFileName := LBaseConfigFileName;
    end;
  finally
    FreeAndNil(LFileNames);
  end;
end;

procedure TProject.EnsureAppConfigs;
begin
  if not Assigned(FAppConfigs) then
  begin
    FAppConfigs := TObjectList<TKConfig>.Create(True);
    ReloadAppConfigs;
  end;
end;

class destructor TProject.Destroy;
begin
  CloseProject;
end;

function TProject.GetResourcesPath: string;
begin
  Result := IncludeTrailingPathDelimiter(Config.AppHomePath + 'Resources');
end;

function TProject.GetAppConfigCount: Integer;
begin
  EnsureAppConfigs;
  Result := FAppConfigs.Count;
end;

function TProject.GetAppConfigs(const AIndex: Integer): TKConfig;
begin
  Result := FAppConfigs[AIndex];
end;

function TProject.GetBaseName: string;
begin
  Result := ChangeFileExt(ExtractFileName(FileName), '');
end;

procedure TProject.GetConfigFileNames(const AFileNames: TStrings;
  const AFullPaths: Boolean = True);
begin
  FindAllFiles('yaml', Config.GetMetadataPath, AFileNames, False, AFullPaths);
end;

function TProject.GetDirectory: string;
begin
  Result := ExtractFilePath(FileName);
end;

function TProject.GetIsKittoEngineRunning: Boolean;
begin
  Result := WaitForSingleObject(FKittoEngineHandle, 0) = WAIT_TIMEOUT;
end;

function TProject.GetKittoEngineFileName: string;
begin
  Result := ChangeFileExt(FileName, '.exe');
end;

function TProject.GetLayoutImageIndex(const ALayout: TKLayout): Integer;
begin
  if SameText(RightStr(ALayout.PersistentName,5),'_Grid') then
    Result := LAYOUT_GRID
  else if SameText(RightStr(ALayout.PersistentName,5),'_Form') then
    Result := LAYOUT_FORM
  else
    Result := LAYOUT_PICTURE;
end;

function TProject.GetMRUKeyName: string;
begin
  Result := FileName;
end;

function TProject.GetSourceDirectory: string;
begin
  Result := Directory + '..\Source';
end;

function TProject.GetViewImageIndex(const AView: TKView): Integer;
begin
  if MatchText(AView.GetString('Controller'), ['Viewport', 'Window']) then
    Result := HOME_VIEW
  else if SameText(AView.GetString('Type'), 'Data') then
  begin
    if SameText(AView.GetString('Controller'), 'Form') then
      Result := FORM_VIEW
    else if SameText(AView.ControllerType, 'List') then
      Result := LIST_VIEW
    else
      Result := VIEW_PICTURE;
  end
  else if SameText(AView.GetString('Type'), 'Tree') then
    Result := TREE_VIEW
  else
    Result := VIEW_PICTURE;
end;

procedure TProject.KittoEngineClose;
begin
  if IsKittoEngineRunning then
    TerminateProcess(FKittoEngineHandle, 0);
end;

class procedure TProject.OpenProject(const AFileName: string);
begin
  FCurrentProject := TEFTreeFactory.LoadFromFile<TProject>(AFileName);
  FCurrentProject.FFileName := AFileName;
  FCurrentProject.RefreshConfig;
end;

procedure TProject.RefreshAll;
begin
  RefreshConfigs;
  // Useless, as we're about to destroy Config.
  //Config.Models.Refresh;
  //Config.Views.Refresh;
end;

procedure TProject.RefreshConfigs;
begin
  Config.InvalidateConfig;
  if Assigned(FAppConfigs) then
    ReloadAppConfigs;
end;

procedure TProject.RefreshConfig;
begin
  FreeAndNil(FConfig);
  TProjectConfig.AppHomePath := Directory;
  FConfig := TProjectConfig.Create;
  TEFMacroExpansionEngine.OnGetInstance :=
    function: TEFMacroExpansionEngine
    begin
      Result := FConfig.MacroExpansionEngine;
    end;
end;

function TProject.RetrieveMRUBoolean(const AKey: string;
  const ADefault: Boolean): Boolean;
begin
  Result := TMRUOptions.Instance.GetBoolean(GetMRUKeyName + '/' + AKey, ADefault);
end;

function TProject.RetrieveMRUNode(const APath: string): TEFNode;
begin
  Result := TMRUOptions.Instance.GetNode(GetMRUKeyName + '/' + APath);
end;

function TProject.RetrieveMRUInteger(const AKey: string; ADefault: Integer): Integer;
begin
  Result := TMRUOptions.Instance.GetInteger(GetMRUKeyName + '/' + AKey, ADefault);
end;

function TProject.RetrieveMRUString(const AKey, ADefault: string): string;
begin
  Result := TMRUOptions.Instance.GetString(GetMRUKeyName + '/' + AKey, ADefault);
end;

procedure TProject.RetrieveMRUStrings(const AKey: string;
  const AStrings: TStrings);
begin
  TMRUOptions.Instance.GetChildrenAsStrings(GetMRUKeyName + '/' + AKey, AStrings);
end;

class procedure TProject.SetCurrentProject(const AValue: TProject);
begin
  FreeAndNil(FCurrentProject);
  FCurrentProject := AValue;
end;

procedure TProject.ShowPreviewAction(const ACommand: string);
var
  LCommand: string;
begin
  LCommand := FApplication.GetHomeURL(FServer.DefaultPort)+'?'+ACommand;
  OpenDocument(LCommand);
end;

procedure TProject.StoreMRUInteger(const AKey: string; const AValue: Integer);
begin
  TMRUOptions.Instance.StoreInteger(GetMRUKeyName + '/' + AKey, AValue);
end;

procedure TProject.StoreMRUItem(const AKey, AValue: string);
begin
  TMRUOptions.Instance.StoreMRUItem(GetMRUKeyName + '/' + AKey, AValue);
end;

procedure TProject.StoreMRUNode(const APath: string; const ANode: TEFNode);
begin
  TMRUOptions.Instance.StoreNode(GetMRUKeyName + '/' + APath, ANode);
end;

procedure TProject.StoreMRUString(const AKey, AValue: string);
begin
  TMRUOptions.Instance.StoreString(GetMRUKeyName + '/' + AKey, AValue);
end;

procedure TProject.StoreMRUBoolean(const AKey: string; const AValue: Boolean);
begin
  TMRUOptions.Instance.StoreBoolean(GetMRUKeyName + '/' + AKey, AValue);
end;

procedure TProject.StoreMRUStrings(const AKey: string; const AStrings: TStrings);
begin
  TMRUOptions.Instance.SetChildrenAsStrings(GetMRUKeyName + '/' + AKey, AStrings);
  TMRUOptions.Instance.Save;
end;

end.
