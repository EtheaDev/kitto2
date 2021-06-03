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

unit Kitto.Config;

{$I Kitto.Defines.inc}

interface

uses
  SysUtils
  , Types
  , Generics.Collections
  , EF.Tree
  , EF.Macros
  , EF.Classes
  , EF.DB
  , EF.ObserverIntf
  , Kitto.Metadata.Models
  , Kitto.Metadata.Views
  ;

type
  TKConfigMacroExpander = class;

  TKConfig = class;

  TKConfigClass = class of TKConfig;

  TKGetConfig = reference to function: TKConfig;

  TKConfigGetAppNameEvent = procedure (out AAppName: string) of object;

  TKConfig = class(TEFComponent)
  strict private
  class var
    FAppHomePath: string;
    FJSFormatSettings: TFormatSettings;
    FBaseConfigFileName: string;
    FOnGetInstance: TKGetConfig;
    FInstance: TKConfig;
    FSystemHomePath: string;
    FConfigClass: TKConfigClass;
    FOnGetAppName: TKConfigGetAppNameEvent;
  var
    FMacroExpansionEngine: TEFMacroExpansionEngine;
    FModels: TKModels;
    FViews: TKViews;
    FUserFormatSettings: TFormatSettings;

    class function GetInstance: TKConfig; static;
    class function GetAppName: string; static;
    class function GetAppHomePath: string; static;
    class procedure SetAppHomePath(const AValue: string); static;
    class function GetSystemHomePath: string; static;
    class procedure SetSystemHomePath(const AValue: string); static;
    class function GetDatabase: TEFDBConnection; static;

    function GetDBConnectionNames: TStringDynArray;
    function GetMultiFieldSeparator: string;
    function GetDBAdapter(const ADatabaseName: string): TEFDBAdapter;
    function GetMacroExpansionEngine: TEFMacroExpansionEngine;
    function GetAppTitle: string;
    function GetAppIcon: string;
    function GetModels: TKModels;
    function GetViews: TKViews;
    function GetDefaultDatabaseName: string;
    function GetDatabaseName: string;
    function GetLanguagePerSession: Boolean;
    function GetFOPEnginePath: string;
  strict protected
    function GetUploadPath: string;
    function GetConfigFileName: string; override;
    class function FindSystemHomePath: string;
  public
    class procedure DestroyInstance;
    procedure AfterConstruction; override;
    destructor Destroy; override;
    class constructor Create;
    class destructor Destroy;
    procedure UpdateObserver(const ASubject: IEFSubject;
      const AContext: string = ''); override;
  public
    class procedure SetConfigClass(const AValue: TKConfigClass);

    class property AppName: string read GetAppName;
    class property OnGetAppName: TKConfigGetAppNameEvent read FOnGetAppName write FOnGetAppName;

    /// <summary>
    ///   <para>Returns or changes the Application Home path.</para>
    ///   <para>The Application Home path defaults to the exe file directory
    ///   unless specified through the '-home' command line argument.</para>
    ///   <para>Setting this property, if necessary, should be done at
    ///   application startup, preferably in a unit's initialization
    ///   section.</para>
    /// </summary>
    /// <remarks>Changing this property affects all TKConfg instances created
    /// from that point on, not existing instances.</remarks>
    class property AppHomePath: string read GetAppHomePath write SetAppHomePath;

    /// <summary>
    ///   <para>Returns or changes the System Home path, which is used to find
    ///   any resources that are not found in the Application Home path.
    ///   Generally, the System Home path contains all predefined metadata and
    ///   resources of the framework.</para>
    ///   <para>The System Home path defaults to a "Home" directory inside a
    ///   nearby directory named "Kitto". The following paths, relative to the
    ///   executable directory, are searched in order:</para>
    ///   <list type="number">
    ///     <item>..\Externals\Kitto\Home</item>
    ///     <item>..\..\Externals\Kitto\Home</item>
    ///     <item>..\..\..\Home</item>
    ///     <item>%KITTO%\Home</item>
    ///   </list>
    ///   <para>The first existing path is used. If none of these exist, the
    ///   value of AppHomePath is assumed.</para>
    ///   <para>If no default is suitable for your application, you can set
    ///   this property at application startup, preferably in a unit's
    ///   initialization section. If you also need to set AppHomePath, do it
    ///   <b>before</b> setting this property.</para>
    /// </summary>
    /// <remarks>Changing this property affects all TKConfg instances created
    /// from that point on, not existing instances.</remarks>
    class property SystemHomePath: string read GetSystemHomePath write SetSystemHomePath;

    /// <summary>
    ///   Returns the full path of the Metadata directory inside the home path.
    /// </summary>
    class function GetMetadataPath: string;

    /// <summary>
    ///  Format settings for Javascript/JSON data encoded in text format. use
    ///  it, don't change it.
    /// </summary>
    class property JSFormatSettings: TFormatSettings read FJSFormatSettings;

    /// <summary>
    ///  Name of the config file. Defaults to Config.yaml. Changing this
    ///  property only affects instances created afterwards.
    /// </summary>
    class property BaseConfigFileName: string read FBaseConfigFileName write FBaseConfigFileName;

    /// <summary>
    ///  Sets a global function that returns the global config object. In web
    ///  applications there will be a config object per session.
    /// </summary>
    class property OnGetInstance: TKGetConfig read FOnGetInstance write FOnGetInstance;

    /// <summary>
    ///  Returns a singleton instance.
    /// </summary>
    class property Instance: TKConfig read GetInstance;

    /// <summary>
    ///   Returns the database instance.
    /// </summary>
    class property Database: TEFDBConnection read GetDatabase;

    /// <summary>
    ///  A reference to the model catalog, opened on first access.
    /// </summary>
    property Models: TKModels read GetModels;

    /// <summary>
    ///  A reference to the view catalog, opened on first access.
    /// </summary>
    property Views: TKViews read GetViews;

    /// <summary>Makes sure catalogs are recreated at next access.</summary>
    procedure InvalidateCatalogs;

    /// <summary>
    ///  Returns the names of all defined database
    ///  connections.
    /// </summary>
    property DBConnectionNames: TStringDynArray read GetDBConnectionNames;

    /// <summary>
    ///  Creates a DB connection for the specified configured database.
    ///  The caller is responsible for the life cycle of the object.
    /// </summary>
    function CreateDBConnection(const ADatabaseName: string): TEFDBConnection;
    /// <summary>
    ///  Helper function that creates a DB connection, passes it to an anonymous method
    ///  then destroys it. Use it for DB read access and single update instructions.
    ///  For multiple update instructions please use <seealso>InDBTransaction</seealso>.
    /// </summary>
    procedure InDBConnection(const ADatabaseName: string; const AProc: TProc<TEFDBConnection>);
    /// <summary>
    ///  Helper function that creates a DB connection, starts a transaction and calls
    ///  the specified anonymous method. If the method raises an exception, then the
    ///  transaction is rolled back (and the exception is propagated), otherwise
    ///  it's committed after the method returns.
    ///  Finally, the connection object is destroyed. Use this method for multiple
    ///  update statements that need to be enclosed in a single database transaction.
    ///  For read operations or single update operations, please use <seealso>InDBConnection</seealso>.
    ///  then destroys it. Use it for DB read access and single update instructions.
    ///  For multiple update instructions please use <seealso>InDBTransaction</seealso>.
    /// </summary>
    procedure InDBTransaction(const ADatabaseName: string; const AProc: TProc<TEFDBConnection>);
    /// <summary>
    ///  Helper function that creates a DB connection, returns the value of the
    ///  connection's GetSingletonValue method, then destroys it.
    /// </summary>
    function GetDBSingletonValue(const ADatabaseName, ASQLStatement: string): Variant;

    /// <summary>Default DatabaseName to use when not specified elsewhere. Can
    /// be set through the DatabaseRouter/DatabaseName node or through the
    /// DefaultDatabaseName node.</summary>
    property DatabaseName: string read GetDatabaseName;

    /// <summary>
    ///  Returns the application title, to be used for captions, about
    ///  boxes, etc.
    /// </summary>
    property AppTitle: string read GetAppTitle;

    /// <summary>
    ///  Returns the application Icon, to be used mobile apps
    ///  and Browser
    /// </summary>
    property AppIcon: string read GetAppIcon;

    /// <summary>
    ///  Global expansion engine. Kitto-specific macro expanders should be
    ///  added here at run time. This engine is chained to the default engine,
    ///  so all default EF macros are supported.
    /// </summary>
    property MacroExpansionEngine: TEFMacroExpansionEngine read GetMacroExpansionEngine;

    property UserFormatSettings: TFormatSettings read FUserFormatSettings;

    property MultiFieldSeparator: string read GetMultiFieldSeparator;

    property LanguagePerSession: Boolean read GetLanguagePerSession;

    /// <summary>
    ///   <para>Returns or changes the home path for FOP engine.</para>
    /// </summary>
    property FOPEnginePath: string read GetFOPEnginePath;

    /// <summary>
    ///   <para>Returns or changes the Upload path accessible via %UPLOAD_PATH% macro.</para>
    /// </summary>
    property UploadPath: string read GetUploadPath;
  end;

  /// <summary>
  ///   <para>
  ///     A macro expander that can expand globally available macros.
  ///   </para>
  ///   <para>
  ///     %HOME_PATH% = TKConfig.Instance.GetAppHomePath.
  ///   </para>
  ///   <para>
  ///     It also expands any macros in the Config namespace to the
  ///     corresponding environment config string. Example:
  ///   </para>
  ///   <para>
  ///     %Config:AppTitle% = The string value of the AppTitle node in
  ///     Config.yaml.
  ///   </para>
  /// </summary>
  TKConfigMacroExpander = class(TEFTreeMacroExpander)
  strict private
    FConfig: TKConfig;
  strict protected
    property Config: TKConfig read FConfig;
    procedure InternalExpand(var AString: string); override;
  public
    constructor Create(const AConfig: TKConfig); reintroduce;
  end;

implementation

uses
  StrUtils
  , Variants
  , IOUtils
  , EF.Sys
  , EF.StrUtils
  , EF.YAML
  , EF.Localization
  , Kitto.Web.Application
  , Kitto.Types
  , Kitto.DatabaseRouter
  ;

procedure TKConfig.AfterConstruction;
var
  LDecimalSeparator: string;
  LThousandSeparator: string;
begin
  inherited;
  { TODO : allow to change format settings on a per-user basis. }
  FUserFormatSettings := FormatSettings.Create;

  FUserFormatSettings.ShortTimeFormat := Config.GetString('UserFormats/Time', FUserFormatSettings.ShortTimeFormat);
  if Pos('.', FUserFormatSettings.ShortTimeFormat) > 0 then
    FUserFormatSettings.TimeSeparator := '.'
  else
    FUserFormatSettings.TimeSeparator := ':';

  FUserFormatSettings.ShortDateFormat := Config.GetString('UserFormats/Date', FUserFormatSettings.ShortDateFormat);
  if Pos('.', FUserFormatSettings.ShortDateFormat) > 0 then
    FUserFormatSettings.DateSeparator := '.'
  else if Pos('-', FUserFormatSettings.ShortDateFormat) > 0 then
    FUserFormatSettings.DateSeparator := '-'
  else
    FUserFormatSettings.DateSeparator := '/';

  LDecimalSeparator := Config.GetString('UserFormats/DecimalSeparator', '');
  if LDecimalSeparator = '.' then
    FUserFormatSettings.DecimalSeparator := '.'
  else if LDecimalSeparator = ',' then
    FUserFormatSettings.DecimalSeparator := ',';

  LThousandSeparator := Config.GetString('UserFormats/ThousandSeparator', '');
  if LThousandSeparator = '.' then
    FUserFormatSettings.ThousandSeparator := '.'
  else if LThousandSeparator = ',' then
    FUserFormatSettings.ThousandSeparator := ',';
end;

destructor TKConfig.Destroy;
begin
  inherited;
  FreeAndNil(FViews);
  FreeAndNil(FModels);
  FreeAndNil(FMacroExpansionEngine);
end;

procedure TKConfig.UpdateObserver(const ASubject: IEFSubject;
  const AContext: string);
begin
  inherited;
  NotifyObservers(AContext);
end;

procedure TKConfig.InDBConnection(const ADatabaseName: string; const AProc: TProc<TEFDBConnection>);
var
  LDBConnection: TEFDBConnection;
begin
  Assert(Assigned(AProc));

  LDBConnection := CreateDBConnection(ADatabaseName);
  try
    AProc(LDBConnection);
  finally
    FreeAndNil(LDBConnection);
  end;
end;

procedure TKConfig.InDBTransaction(const ADatabaseName: string; const AProc: TProc<TEFDBConnection>);
begin
  InDBConnection(ADatabaseName,
    procedure (ADBConnection: TEFDBConnection)
    begin
      ADBConnection.StartTransaction;
      try
        AProc(ADBConnection);
        ADBConnection.CommitTransaction;
      except
        ADBConnection.RollbackTransaction;
        raise;
      end;
    end);
end;

procedure TKConfig.InvalidateCatalogs;
begin
  FreeAndNil(FViews);
  FreeAndNil(FModels);
end;

function TKConfig.CreateDBConnection(const ADatabaseName: string): TEFDBConnection;
var
  LConfig: TEFNode;
begin
  Result := GetDBAdapter(ADatabaseName).CreateDBConnection;
  try
    Result.Config.AddChild(TEFNode.Clone(Config.GetNode('Databases/' + ADatabaseName + '/Connection')));
    LConfig := Config.FindNode('Databases/' + ADatabaseName + '/Config');
    if Assigned(LConfig) then
      Result.Config.AddChild(TEFNode.Clone(LConfig));
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TKConfig.GetDBConnectionNames: TStringDynArray;
var
  LNode: TEFNode;
begin
  LNode := Config.FindNode('Databases');
  if Assigned(LNode) then
    Result := LNode.GetChildNames
  else
    Result := nil;
end;

function TKConfig.GetDBSingletonValue(const ADatabaseName, ASQLStatement: string): Variant;
var
  LResult: Variant;
begin
  InDBConnection(ADatabaseName,
    procedure (ADBConnection: TEFDBConnection)
    begin
      LResult := ADBConnection.GetSingletonValue(ASQLStatement);
    end);
  Result := LResult;
end;

function TKConfig.GetDatabaseName: string;
var
  LDatabaseRouterNode: TEFNode;
begin
  LDatabaseRouterNode := Config.FindNode('DatabaseRouter');
  if Assigned(LDatabaseRouterNode) then
    Result := TKDatabaseRouterFactory.Instance.GetDatabaseName(
      LDatabaseRouterNode.AsString, Self, LDatabaseRouterNode)
  else
    Result := GetDefaultDatabaseName;
end;

function TKConfig.GetDefaultDatabaseName: string;
begin
  Result := Config.GetExpandedString('DefaultDatabaseName', 'Main');
end;

function TKConfig.GetFOPEnginePath: string;
begin
  Result := Config.GetExpandedString('FOPEnginePath');
end;

function TKConfig.GetDBAdapter(const ADatabaseName: string): TEFDBAdapter;
var
  LDbAdapterKey: string;
begin
  Try
    LDbAdapterKey := Config.GetExpandedString('Databases/' + ADatabaseName);
    Result := TEFDBAdapterRegistry.Instance[LDbAdapterKey];
  except
    raise EKError.CreateFmt(_('DB connection type "%s" for database "%s" not available'),
      [LDbAdapterKey, ADatabaseName]);
  end;
end;

function TKConfig.GetMacroExpansionEngine: TEFMacroExpansionEngine;
begin
  if not Assigned(FMacroExpansionEngine) then
  begin
    FMacroExpansionEngine := TEFMacroExpansionEngine.Create;
    FMacroExpansionEngine.OnGetFormatSettings :=
      function: TFormatSettings
      begin
        Result := UserFormatSettings;
      end;
    AddStandardMacroExpanders(FMacroExpansionEngine);
    FMacroExpansionEngine.AddExpander(TKConfigMacroExpander.Create(Self));
  end;
  Result := FMacroExpansionEngine;
end;

class function TKConfig.GetMetadataPath: string;
begin
  Result := GetAppHomePath + IncludeTrailingPathDelimiter('Metadata');
end;

function TKConfig.GetModels: TKModels;
begin
  if not Assigned(FModels) then
  begin
    FModels := TKModels.Create;
    FModels.AttachObserver(Self);
    FModels.Path := GetMetadataPath + 'Models';
    FModels.Open;
  end;
  Result := FModels;
end;

function TKConfig.GetMultiFieldSeparator: string;
begin
  Result := Config.GetString('MultiFieldSeparator', '~');
end;

class function TKConfig.GetSystemHomePath: string;
begin
  if FSystemHomePath <> '' then
    Result := FSystemHomePath
  else
    Result := FindSystemHomePath;
  Result := IncludeTrailingPathDelimiter(Result);
end;

function TKConfig.GetUploadPath: string;
begin
  Result := Config.GetExpandedString('UploadPath');
end;

function TKConfig.GetViews: TKViews;
begin
  if not Assigned(FViews) then
  begin
    FViews := TKViews.Create(Models);
    FViews.AttachObserver(Self);
    FViews.Layouts.AttachObserver(Self);
    FViews.Path := GetMetadataPath + 'Views';
    FViews.Open;
  end;
  Result := FViews;
end;

class constructor TKConfig.Create;
var
  LAppName: string;
  LDefaultConfig: string;
begin
  LAppName := ChangeFileExt(ExtractFileName(ParamStr(0)),'');
  FConfigClass := TKConfig;
  LDefaultConfig := Format('Config_%s.yaml',[LAppName]);
  if FileExists(GetMetadataPath + LDefaultConfig) then
    FBaseConfigFileName := LDefaultConfig
  else
    FBaseConfigFileName := 'Config.yaml';

  FJSFormatSettings := TFormatSettings.Create;
  FJSFormatSettings.DecimalSeparator := '.';
  FJSFormatSettings.ThousandSeparator := ',';
  FJSFormatSettings.ShortDateFormat := 'yyyy/mm/dd';
  FJSFormatSettings.ShortTimeFormat := 'hh:nn:ss';
  FJSFormatSettings.DateSeparator := '/';
  FJSFormatSettings.TimeSeparator := ':';
  TEFYAMLReader.FormatSettings := FJSFormatSettings;
end;

class destructor TKConfig.Destroy;
begin
  FreeAndNil(FInstance);
end;

function TKConfig.GetAppTitle: string;
begin
  Result := Config.GetString('AppTitle', 'Kitto');
end;

function TKConfig.GetConfigFileName: string;
begin
  Result := TPath.Combine(GetMetadataPath, FBaseConfigFileName);
end;

function TKConfig.GetAppIcon: string;
begin
  Result := Config.GetString('AppIcon', 'kitto_128');
end;

class function TKConfig.GetInstance: TKConfig;
begin
  Result := nil;
  if Assigned(FOnGetInstance) then
    Result := FOnGetInstance();
  if not Assigned(Result) then
  begin
    if not Assigned(FInstance) then
      FInstance := FConfigClass.Create;
    Result := FInstance;
  end;
end;

class function TKConfig.GetDatabase: TEFDBConnection;
begin
  Result := TKConfig.Instance.CreateDBConnection(TKConfig.Instance.DatabaseName);
end;

function TKConfig.GetLanguagePerSession: Boolean;
begin
  Result := Config.GetBoolean('LanguagePerSession', False);
end;

class procedure TKConfig.SetAppHomePath(const AValue: string);
begin
  FAppHomePath := AValue;
end;

class procedure TKConfig.SetConfigClass(const AValue: TKConfigClass);
begin
  FConfigClass := AValue;
end;

class procedure TKConfig.SetSystemHomePath(const AValue: string);
begin
  FSystemHomePath := AValue;
end;

class function TKConfig.GetAppName: string;
var
  LConfig: TKConfig;
begin
  Result := '';
  if Assigned(FOnGetAppName) then
    FOnGetAppName(Result);

  if Result = '' then
    Result := GetCmdLineParamValue('appname');

  if Result = '' then
  begin
    LConfig := TKConfig.Create;
    try
      Result := LConfig.Config.GetString('AppName', '');
    finally
      FreeAndNil(LConfig);
    end;
  end;

  if Result = '' then
    Result := ChangeFileExt(ExtractFileName(ParamStr(0)), '');
end;

class function TKConfig.GetAppHomePath: string;
begin
  if FAppHomePath = '' then
  begin
    FAppHomePath := GetCmdLineParamValue('home', ExtractFilePath(ParamStr(0)));
    if not IsAbsolutePath(FAppHomePath) then
      FAppHomePath := ExtractFilePath(ParamStr(0)) + FAppHomePath;
  end;
  Result := IncludeTrailingPathDelimiter(FAppHomePath);
end;

class function TKConfig.FindSystemHomePath: string;
var
  LExePath: string;
begin
  LExePath := ExtractFilePath(ParamStr(0));
  Result := LExePath + '..\Externals\Kitto\Home\';
  if not DirectoryExists(Result) then
  begin
    Result := LExePath + '..\..\Externals\Kitto\Home\';
    if not DirectoryExists(Result) then
    begin
      Result := LExePath + '..\..\..\Home\';
      if not DirectoryExists(Result) then
      begin
        Result := LExePath + '..\..\..\..\Kitto\Home\';
        if not DirectoryExists(Result) then
        begin
          Result := '%KITTO%\Home\';
          ExpandEnvironmentVariables(Result);
          if not DirectoryExists(Result) then
            Result := GetAppHomePath;
        end;
      end;
    end;
  end;
end;

class procedure TKConfig.DestroyInstance;
begin
  FreeAndNil(FInstance);
end;

{ TKConfigMacroExpander }

constructor TKConfigMacroExpander.Create(const AConfig: TKConfig);
begin
  Assert(Assigned(AConfig));

  FConfig := AConfig;
  inherited Create(AConfig.Config, 'Config');
end;

procedure TKConfigMacroExpander.InternalExpand(var AString: string);
const
  IMAGE_MACRO_HEAD = '%IMAGE(';
  MACRO_TAIL = ')%';
  UPLOAD_PATH = '%UPLOAD_PATH%';
var
  LPosHead: Integer;
  LPosTail: Integer;
  LName: string;
  LURL: string;
  LRest: string;
begin
  inherited InternalExpand(AString);
  ExpandMacros(AString, '%HOME_PATH%', TKConfig.AppHomePath);
  ExpandMacros(AString, '%Config.AppName%', FConfig.AppName);
  ExpandMacros(AString, '%Config.AppHomePath%', FConfig.AppHomePath);
  ExpandMacros(AString, '%Config.AppTitle%', FConfig.Instance.AppTitle);
  ExpandMacros(AString, '%Config.AppIcon%', FConfig.AppIcon);
  if Pos(UPLOAD_PATH, AString) > 0 then
    ExpandMacros(AString, UPLOAD_PATH, IncludeTrailingPathDelimiter(Config.UploadPath));

  LPosHead := Pos(IMAGE_MACRO_HEAD, AString);
  if LPosHead > 0 then
  begin
    LPosTail := PosEx(MACRO_TAIL, AString, LPosHead + 1);
    if LPosTail > 0 then
    begin
      LName := Copy(AString, LPosHead + Length(IMAGE_MACRO_HEAD),
        LPosTail - (LPosHead + Length(IMAGE_MACRO_HEAD)));
      LURL := TKWebApplication.Current.GetImageURL(LName);
      LRest := Copy(AString, LPosTail + Length(MACRO_TAIL), MaxInt);
      InternalExpand(LRest);
      Delete(AString, LPosHead, MaxInt);
      Insert(LURL, AString, Length(AString) + 1);
      Insert(LRest, AString, Length(AString) + 1);
    end;
  end;
end;

end.
