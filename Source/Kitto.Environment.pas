unit Kitto.Environment;

{$I Kitto.Defines.inc}

interface

uses
  Generics.Collections,
  EF.Macros, EF.Classes,  EF.DB, EF.Environment, EF.Tree,
  Kitto.Auth, Kitto.AccessControl, Kitto.Metadata, Kitto.Metadata.Models,
  Kitto.Metadata.Views;

type
  {
    Provides access to the MetaSchema, GUICatalog, database connection and global
    configuration information.
  }
  TKEnvironment = class(TEFComponent, IEFEnvironment)
  private
    FDBConnections: TDictionary<string, TEFDBConnection>;
    FMacroExpansionEngine: TEFMacroExpansionEngine;
    FAccessControlHost: TKAccessControlHost;
    FAppHomePath: string;
    FModels: TKModels;
    FViews: TKViews;
    FResourcePathsURLs: TDictionary<string, string>;
    FMacroExpander: TEFMacroExpander;
    FAuthenticator: TKAuthenticator;
    const MAIN_DB_NAME = 'Main';
    function GetDBConnection(const ADatabaseName: string): TEFDBConnection;
    function GetAuthenticator: TKAuthenticator;
    function GetMainDBConnection: TEFDBConnection;
    function GetDBAdapter(const ADatabaseName: string): TEFDBAdapter;
    function GetMacroExpansionEngine: TEFMacroExpansionEngine;
    function GetAccessControlHost: TKAccessControlHost;
    function GetAppTitle: string;
    function GetAppName: string;
    function GetModels: TKModels;
    function GetViews: TKViews;
    function GetMetadataPath: string;
    procedure SetupResourcePathsURLs;
  protected
    procedure SetAppHomePath(const AValue: string);
    function GetConfigFileName: string; override;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
    {
      A reference to the model catalog, opened on first access.
    }
    property Models: TKModels read GetModels;
    {
      A reference to the model catalog, opened on first access.
    }
    property Views: TKViews read GetViews;
    {
      Gives access to the database connection, created on demand.
    }
    property MainDBConnection: TEFDBConnection read GetMainDBConnection;
    {
      Returns True if the connection has been created.
    }
    function HasMainDBConnection: Boolean;
    {
      Returns True if the specified path is an EW Home path that can be opened.
    }
    function CanOpen(const APath: string): Boolean;
    {
      Returns the application title, to be used for captions, about boxes, etc.
    }
    property AppTitle: string read GetAppTitle;
    {
      Closes and destroys the database connection. Do it before unloading a
      package that implements the database access layer in use.
    }
    procedure FinalizeDBConnections;
    {
      Global expansion engine for EW applications. This should be used in place
      of EF's default expansion engine in EW applications, because it is
      thread-safe when necessary. EW-specific macro expanders should be added
      here at run time. This engine is chained to the default engine, so all
      default EF macros are supported.
    }
    property MacroExpansionEngine: TEFMacroExpansionEngine read GetMacroExpansionEngine;

    ///	<summary>Access to the current authenticator.</summary>
    property Authenticator: TKAuthenticator read GetAuthenticator;

    {
      A reference to the object that keeps track of the currently active access
      controller, manages it and provides access control-related services.
    }
    property AccessControlHost: TKAccessControlHost read GetAccessControlHost;
    {
      Shortcut for AccessControlHost.CurrentAccessController.GetAccessGrantValue
      that uses the currently logged in user id.
    }
    function GetCurrentUserAccessGrantValue(const AResourceURI, AMode: string;
      const ADefaultValue: Variant): Variant;
    {
      Shortcut for GetCurrentUserAccessGrantValue for
      Boolean values. Returns True if a value is granted and it equals ACV_TRUE.
    }
    function IsAccessGranted(const AResourceURI, AMode: string): Boolean;
    {
      Calls IsAccessGranted and raises an "access denied" exception if the
      return value is not True.
    }
    procedure CheckAccessGranted(const AResourceURI, AMode: string);

    ///	<summary>
    ///	  Returns the URL for the specified resource, based on the first
    ///	  existing file in the ordered list of resource folders. If no existing
    ///	  file is found, an exception is raised.
    ///	</summary>
    ///	<param name="AResourceFileName">
    ///	  Resource file name relative to the resource folder. Examples:
    ///	  some_image.png, js\some_library.js.
    ///	</param>
    function GetResourceURL(const AResourceFileName: string): string;

    ///	<summary>
    ///	  Returns the URL for the specified resource, based on the first
    ///	  existing file in the ordered list of resource folders. If no existing
    ///	  file is found, returns ''.
    ///	</summary>
    ///	<param name="AResourceFileName">
    ///	  Resource file name relative to the resource folder. Examples:
    ///	  some_image.png, js\some_library.js.
    ///	</param>
    function FindResourceURL(const AResourceFileName: string): string;

    ///	<summary>
    ///	  Returns the full pathname for the specified resource, based on the first
    ///	  existing file in the ordered list of resource folders. If no existing
    ///	  file is found, returns ''.
    ///	</summary>
    ///	<param name="AResourceFileName">
    ///	  Resource file name relative to the resource folder. Examples:
    ///	  some_image.png, js\some_library.js.
    ///	</param>
    function FindResourcePathName(const AResourceFileName: string): string;

    ///	<summary>
    ///	  Returns the full pathname for the specified resource, based on the first
    ///	  existing file in the ordered list of resource folders. If no existing
    ///	  file is found, an exception is raised.
    ///	</summary>
    ///	<param name="AResourceFileName">
    ///	  Resource file name relative to the resource folder. Examples:
    ///	  some_image.png, js\some_library.js.
    ///	</param>
    function GetResourcePathName(const AResourceFileName: string): string;

    function GetImageURL(const AResourceName: string; const ASuffix: string = ''): string;

    {
      Returns the home path. In deployment, it is usually equal to
      the application home path. In development, it might differ.
    }
    function GetKittoHomePath: string;
    {
      Returns the application home path as specified by the 'home' command
      line argument or the executable directory.
    }
    function GetAppHomePath: string;
  end;
  TKEnvironmentClass = class of TKEnvironment;

///	<summary>
///	  Singleton access to the environment object.
///	</summary>
function Environment: TKEnvironment;

procedure SetEnvironmentClass(const AValue: TKEnvironmentClass);

type
  TKEnvironmentSingleton = function: TKEnvironment;

{
  Call this procedure at application startup to provide a custom singleton
  function. Useful in web applications that create an environment instance
  for each user. Pass nil to restore the default singleton function.
}
procedure SetEnvironmentSingleton(const AValue: TKEnvironmentSingleton);

type
  ///	<summary>
  ///	  <para>
  ///	    A macro expander that can expand globally available macros.
  ///	  </para>
  ///	  <para>
  ///	    %HOME_PATH% = Environment.GetAppHomePath.
  ///	  </para>
  ///	  <para>
  ///	    It also expands any macros in the Config namespace to the
  ///	    corresponding environment config string. Example:
  ///	  </para>
  ///	  <para>
  ///	    %Config:AppTitle% = The string value of the AppTitle node in
  ///	    Config.yaml.
  ///	  </para>
  ///	</summary>
  TKMacroExpander = class(TEFTreeMacroExpander)
  protected
    function InternalExpand(const AString: string): string; override;
  end;

implementation

uses
  SysUtils, Variants, StrUtils,
  EF.Intf, EF.SysUtils, EF.StrUtils, EF.Localization, EF.Types,
  Kitto.Types;

var
  _Environment: TKEnvironment = nil;
  _EnvironmentClass: TKEnvironmentClass = TKEnvironment;
  _EnvironmentSingleton: TKEnvironmentSingleton = nil;

function Environment: TKEnvironment;
begin
  if Assigned(_EnvironmentSingleton) then
    Result := _EnvironmentSingleton
  else
  begin
    if not Assigned(_Environment) then
      _Environment := _EnvironmentClass.Create;
    Result := _Environment;
  end;
end;

function EnvironmentAsIntf: IEFEnvironment;
begin
  Result := Environment;
end;

///	<summary>
///	  Substitutes the default environment class with a custom one. Call this
///	  function early (for example, in the initialization section of the unit
///	  that defines the new class). A custom environment object is useful in
///	  that it may expose features used by custom forms and classes, like
///	  additional configurable settings, additional database connection objects
///	  and so on. Note: don't forget to set the class back to nil (which will
///	  restore the default behaviour) in a balanced way (for example in the
///	  finalization section of the same unit).
///	</summary>
procedure SetEnvironmentClass(const AValue: TKEnvironmentClass);
begin
  FreeAndNil(_Environment);
  _EnvironmentClass := AValue;
  if _EnvironmentClass = nil then
    _EnvironmentClass := TKEnvironment;
end;

procedure SetEnvironmentSingleton(const AValue: TKEnvironmentSingleton);
begin
  if Addr(_EnvironmentSingleton) <> Addr(AValue) then
  begin
    FreeAndNil(_Environment);
    _EnvironmentSingleton := AValue;
  end;
end;

{ TKEnvironment }

procedure TKEnvironment.AfterConstruction;
var
  LLanguageId: string;
begin
  inherited;
  // Don't store interface reference to prevent the compiler from calling
  // _Release upon (or after) destruction.
  FDBConnections := TDictionary<string, TEFDBConnection>.Create;
  FResourcePathsURLs := TDictionary<string, string>.Create;
  SetupResourcePathsURLs;
  LLanguageId := Config.GetString('LanguageId');
  if LLanguageId <> '' then
    EFLocalizationTool.ForceLanguage(LLanguageId);
  FMacroExpander := TKMacroExpander.Create(Config, 'Config');
  MacroExpansionEngine.AddExpander(FMacroExpander);
end;

destructor TKEnvironment.Destroy;
begin
  inherited;
  FMacroExpansionEngine.RemoveExpander(FMacroExpander);
  FreeAndNil(FMacroExpander);
  FreeAndNil(FViews);
  FreeAndNil(FModels);
  FreeAndNil(FAccessControlHost);
  FreeAndNil(FAuthenticator);
  FreeAndNil(FMacroExpansionEngine);
  FreeAndNil(FResourcePathsURLs);
  FinalizeDBConnections;
  FreeAndNil(FDBConnections);
end;

procedure TKEnvironment.SetupResourcePathsURLs;
var
  LPath: string;
begin
  LPath := GetAppHomePath + 'Resources';
  if DirectoryExists(LPath) then
    FResourcePathsURLs.Add(IncludeTrailingPathDelimiter(LPath), '/' + GetAppName + '/');
  LPath := GetKittoHomePath + 'Resources';
  if DirectoryExists(LPath) and not FResourcePathsURLs.ContainsKey(IncludeTrailingPathDelimiter(LPath)) then
    FResourcePathsURLs.Add(IncludeTrailingPathDelimiter(LPath), '/' + GetAppName + '-Kitto/');
end;

procedure TKEnvironment.FinalizeDBConnections;
var
  I: Integer;
  LDBConnection: TEFDBConnection;
begin
  for I := 0 to FDBConnections.Count - 1 do
  begin
    LDBConnection := FDBConnections.Values.ToArray[I];
    FreeAndNil(LDBConnection);
  end;
end;

function TKEnvironment.CanOpen(const APath: string): Boolean;
begin
  Result := DirectoryExists(IncludeTrailingPathDelimiter(APath) + 'Models');
end;

function TKEnvironment.IsAccessGranted(const AResourceURI,
  AMode: string): Boolean;
begin
  Result := GetCurrentUserAccessGrantValue(AResourceURI, AMode, Null) = ACV_TRUE;
end;

function TKEnvironment.GetMainDBConnection: TEFDBConnection;
begin
  Result := GetDBConnection(MAIN_DB_NAME);
end;

function TKEnvironment.GetDBConnection(const ADatabaseName: string): TEFDBConnection;
var
  LConfig: TEFNode;
begin
  if not FDBConnections.ContainsKey(ADatabaseName) then
  begin
    Result := GetDBAdapter(ADatabaseName).CreateDBConnection;
    Result.Config.AddChild(TEFNode.Clone(Config.GetNode('Databases/' + ADatabaseName + '/Connection')));
    LConfig := Config.FindNode('Databases/' + ADatabaseName + '/Config');
    if Assigned(LConfig) then
      Result.Config.AddChild(TEFNode.Clone(LConfig));
    FDBConnections.Add(ADatabaseName, Result);
  end
  else
    Result := FDBConnections[ADatabaseName];
end;

function TKEnvironment.GetDBAdapter(const ADatabaseName: string): TEFDBAdapter;
begin
  Result := TEFDBAdapterRegistry.Instance[Config.GetExpandedString('Databases/' + ADatabaseName)];
end;

function TKEnvironment.GetMacroExpansionEngine: TEFMacroExpansionEngine;
begin
  if not Assigned(FMacroExpansionEngine) then
    FMacroExpansionEngine := TEFMacroExpansionEngine.Create(DefaultMacroExpansionEngine);
  Result := FMacroExpansionEngine;
end;

function TKEnvironment.GetMetadataPath: string;
begin
  Result := GetAppHomePath + IncludeTrailingPathDelimiter('Metadata');
end;

function TKEnvironment.GetModels: TKModels;
begin
  if not Assigned(FModels) then
  begin
    FModels := TKModels.Create;
    FModels.Path := GetMetadataPath + 'Models';
    FModels.Open;
  end;
  Result := FModels;
end;

function TKEnvironment.FindResourcePathName(const AResourceFileName: string): string;
var
  I: Integer;
begin
  I := 0;
  repeat
    Result := FResourcePathsURLs.Keys.ToArray[I] + AResourceFileName;
    Inc(I);
  until (I >= FResourcePathsURLs.Count) or FileExists(Result);

  if not FileExists(Result) then
    Result := '';
end;

function TKEnvironment.GetResourcePathName(const AResourceFileName: string): string;
begin
  Result := FindResourcePathName(AResourceFileName);
  if Result = '' then
    raise EKError.CreateFmt('Resource %s not found.', [AResourceFileName]);
end;

function TKEnvironment.FindResourceURL(const AResourceFileName: string): string;
var
  I: Integer;
  LResultURL: string;
begin
  I := 0;
  LresultURL := '';
  repeat
    Result := FResourcePathsURLs.Keys.ToArray[I] + AResourceFileName;
    LResultURL := FResourcePathsURLs.Values.ToArray[I] + ReplaceStr(AResourceFileName, '\', '/');
    Inc(I);
  until (I >= FResourcePathsURLs.Count) or FileExists(Result);

  if FileExists(Result) then
    Result := LResultURL
  else
    Result := ''
end;

function TKEnvironment.GetResourceURL(const AResourceFileName: string): string;
begin
  Result := FindResourceURL(AResourceFileName);
  if Result = '' then
    raise EKError.CreateFmt('Resource %s not found.', [AResourceFileName]);
end;

function TKEnvironment.GetViews: TKViews;
begin
  if not Assigned(FViews) then
  begin
    FViews := TKViews.Create;
    FViews.Path := GetMetadataPath + 'Views';
    FViews.Open;
  end;
  Result := FViews;
end;

function TKEnvironment.HasMainDBConnection: Boolean;
begin
  Result := FDBConnections.ContainsKey(MAIN_DB_NAME);
end;

procedure TKEnvironment.CheckAccessGranted(const AResourceURI, AMode: string);
begin
  if not IsAccessGranted(AResourceURI, AMode) then
    raise EKAccessDeniedError.CreateWithAdditionalInfo(_('Access denied. The user is not allowed to perform this operation.'),
      Format(_('Resource URI: %s; access mode: %s.'), [AResourceURI, AMode]));
end;

function TKEnvironment.GetAccessControlHost: TKAccessControlHost;
begin
  if not Assigned(FAccessControlHost) then
    FAccessControlHost := TKAccessControlHost.Create(EWAccessControllerFactory);
  Result := FAccessControlHost;
end;

function TKEnvironment.GetAppTitle: string;
begin
  Result := Config.GetString('AppTitle', 'Kitto');
end;

function TKEnvironment.GetAuthenticator: TKAuthenticator;
var
  LType: string;
  LConfig: TEFNode;
  I: Integer;
begin
  if not Assigned(FAuthenticator) then
  begin
    LType := Config.GetExpandedString('Auth', 'Null');
    FAuthenticator := TKAuthenticatorFactory.Instance.CreateObject(LType);
    LConfig := Config.FindNode('Auth');
    if Assigned(LConfig) then
      for I := 0 to LConfig.ChildCount - 1 do
        FAuthenticator.Config.AddChild(TEFNode.Clone(LConfig.Children[I]));
  end;
  Result := FAuthenticator;
end;

function TKEnvironment.GetConfigFileName: string;
begin
  Result := GetMetadataPath + 'Config.yaml';
end;

function TKEnvironment.GetCurrentUserAccessGrantValue(const AResourceURI,
  AMode: string; const ADefaultValue: Variant): Variant;
begin
  Result := AccessControlHost.CurrentAccessController.GetAccessGrantValue(
    Authenticator.UserName, AResourceURI, AMode, ADefaultValue);
end;

function TKEnvironment.GetImageURL(const AResourceName,
  ASuffix: string): string;

  // Adds a .png extension to the resource name.
  // ASuffix, if specified, is added before the file extension.
  // If the image name ends with _ and a two-digit number among 16, 24, 32, and 48,
  // then the suffix is added before the _.
  function AdaptWebResourceName(const AResourceName: string; const ASuffix: string = ''): string;

    function HasSize(const AName: string): Boolean;
    begin
      Result := EndsStr('_16', AName) or EndsStr('_24', AName)
        or EndsStr('_32', AName) or EndsStr('_48', AName);
    end;

  begin
    Result := AResourceName;
    if HasSize(Result) then
      Insert(ASuffix, Result, Length(Result) - 2)
    else
      Result := Result + ASuffix;
    Result := Result + '.png';
  end;

begin
  Result := GetResourceURL(AdaptWebResourceName(AResourceName, ASuffix));
end;

procedure TKEnvironment.SetAppHomePath(const AValue: string);
begin
  if AValue <> FAppHomePath then
  begin
    FreeAndNil(FModels);
    FAppHomePath := AValue;
  end;
end;

function TKEnvironment.GetAppName: string;
begin
  Result := Config.GetString('AppName');
  if Result = '' then
    Result := ChangeFileExt(ExtractFileName(ParamStr(0)), '');
end;

function TKEnvironment.GetAppHomePath: string;
begin
  if FAppHomePath = '' then
  begin
    FAppHomePath := GetCmdLineParamValue('home', ExtractFilePath(ParamStr(0)));
    if not IsAbsolutePath(FAppHomePath) then
      FAppHomePath := ExtractFilePath(ParamStr(0)) + FAppHomePath;
  end;
  Result := IncludeTrailingPathDelimiter(FAppHomePath);
end;

function TKEnvironment.GetKittoHomePath: string;
begin
  Result := ExtractFilePath(ParamStr(0)) + '..\Externals\Kitto\Home\';
  if not DirectoryExists(Result) then
  begin
    Result := ExtractFilePath(ParamStr(0)) + '..\..\Externals\Kitto\Home\';
    if not DirectoryExists(Result) then
      Result := GetAppHomePath;
  end;
end;

{ TKMacroExpander }

function TKMacroExpander.InternalExpand(const AString: string): string;
begin
  Result := inherited InternalExpand(AString);
  Result := ExpandMacros(Result, '%HOME_PATH%', Environment.GetAppHomePath);
end;

initialization
  SetEnvironmentGetFunction(EnvironmentAsIntf);

finalization
  SetEnvironmentGetFunction(nil);
  FreeAndNil(_Environment);

end.


