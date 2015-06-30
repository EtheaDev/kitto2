{-------------------------------------------------------------------------------
   Copyright 2012 Ethea S.r.l.

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

unit Kitto.Ext.Session;

{$I Kitto.Defines.inc}

interface

uses
  SysUtils, Classes, Generics.Collections, Types,
  gnugettext, superobject,
  ExtPascal, Ext, ExtPascalClasses,
  EF.Tree, EF.Macros, EF.Intf, EF.Localization, EF.ObserverIntf,
  Kitto.Ext.Base, Kitto.Config, Kitto.Metadata.Views,
  Kitto.Ext.Controller;

type
  IKExtViewHost = interface(IEFInterface)
    ['{F073B258-1D46-4553-9FF4-3697DFE5197D}']
    procedure SetActiveView(const AIndex: Integer);
    function AsExtContainer: TExtContainer;
  end;

  TKExtUploadedFile = class
  strict private
    FContext: TObject;
    FFullFileName: string;
    FFileName: string;
    FStream: TBytesStream;
    FOriginalFileName: string;
    function GetBytes: TBytes;
  public
    constructor Create(const AFileName, AFullFileName: string;
      const AContext: TObject; const AOriginalFileName: string = '');
    destructor Destroy; override;
    property FileName: string read FFileName;
    property FullFileName: string read FFullFileName;
    property OriginalFileName: string read FOriginalFileName;
    property Context: TObject read FContext;

    property Bytes: TBytes read GetBytes;
  end;

  TKExtSession = class;

  TKExtSessionMacroExpander = class(TEFMacroExpander)
  strict private
    FSession: TKExtSession;
  strict protected
    function InternalExpand(const AString: string): string; override;
  public
    constructor Create(const ASession: TKExtSession); reintroduce;
  end;

  /// <summary>
  ///   This class serves two purposes: redirects localization calls to a
  ///   per-session instance of dxgettext so we can have per-session language
  ///   selection, and configures Kitto's localization scheme based on two text
  ///   domains (the application's default.mo and Kitto's own Kitto.mo). The
  ///   former is located under the application home directory, the latter
  ///   under the system home directory.
  /// </summary>
  TKExtSessionLocalizationTool = class(TEFNoRefCountObject, IInterface,
    IEFInterface, IEFLocalizationTool)
  private const
    KITTO_TEXT_DOMAIN = 'Kitto';
  private
    function GetGnuGettextInstance: TGnuGettextInstance;
  public
    function AsObject: TObject;
    function TranslateString(const AString: string;
      const AIdString: string = ''): string;
    procedure TranslateComponent(const AComponent: TComponent);
    procedure ForceLanguage(const ALanguageId: string);
    function GetCurrentLanguageId: string;
    procedure AfterConstruction; override;
  end;

  /// <summary>
  ///  A modal window that hosts a controller and removes the controller
  ///  (instead of itself) from the session when it's closed.
  /// </summary>
  TKExtControllerHostWindow = class(TKExtModalWindow)
  private
    FHostedController: TObject;
  strict protected
    function GetControllerToRemove: TObject; override;
  end;

  TKExtSession = class(TExtSession, IEFInterface, IEFObserver)
  strict private
    FHomeController: TObject;
    FConfig: TKConfig;
    FViewHost: IKExtViewHost;
    FStatusHost: TKExtStatusBar;
    FSessionId: string;
    FUploadedFiles: TObjectList<TKExtUploadedFile>;
    FOpenControllers: TObjectList<TObject>;
    FMacroExpander: TKExtSessionMacroExpander;
    FRefreshingLanguage: Boolean;
    FControllerHostWindow: TKExtControllerHostWindow;
    FDynamicScripts: TStringList;
    FDynamicStyles: TStringList;
    FAutoOpenViewName: string;
    FHomeViewNodeName: string;
    FMobileBrowserDetectionDone: Boolean;
    FIsMobileBrowser: Boolean;
    FLoginNode: TEFNode;
    FOwnsLoginNode: Boolean;
    FLoginController: TObject;
    FViewportWidth: Integer;
    FViewportContent: string;
    FViewportWidthInInches: Integer;
    FCreationDateTime: TDateTime;
    FLastRequestDateTime: TDateTime;
    FDisplayName: string;
    procedure LoadLibraries;
    procedure DisplayHomeView;
    procedure DisplayLoginView;
    procedure ReloadOrDisplayHomeView;
    function GetConfig: TKConfig;
    procedure ClearStatus;
    function FindOpenController(const AView: TKView): IKExtController;
    procedure SetActiveViewInViewHost(const AObject: TObject);
    procedure SetLanguageFromQueriesOrConfig;
    procedure Reload;
    procedure SetViewHost(const AValue: IKExtViewHost);
    /// <summary>
    ///  If the specifield css file name exists, generates code that
    ///  adds it to the page and adds that code to the current response.
    ///  If called multiple times, only the first time the file is added.
    /// </summary>
    procedure EnsureDynamicStyle(const AStyleBaseName: string);
    /// <summary>
    ///  If the specifield script file name exists, generates code that
    ///  adds it to the page and adds that code to the current response.
    ///  If called multiple times, only the first time the file is added.
    /// </summary>
    procedure EnsureDynamicScript(const AScriptBaseName: string);
    function GetHomeView: TKView;
    function GetLoginView: TKView;
    procedure FreeLoginNode;
    procedure SetViewportContent;
    function GetDefaultHomeViewNodeNames(const ASuffix: string): TStringDynArray;
    function GetDefaultViewportWidth: Integer;
    const DEFAULT_VIEWPORT_WIDTH = 480;
    function GetDisplayName: string;
    procedure SetDisplayName(const AValue: string);
  private
    FGettextInstance: TGnuGettextInstance;
  strict protected
    function BeforeHandleRequest: Boolean; override;
    procedure AfterHandleRequest; override;
    procedure AfterNewSession; override;
    function GetMainPageTemplate: string; override;
    procedure SetLanguage(const AValue: string); override;
    function GetSessionCookieName: string; override;
    function GetViewportContent: string; override;
    function GetManifestFileName: string; override;
    function GetCustomJS: string; override;
  public
    constructor Create(AOwner: TObject); override;
    destructor Destroy; override;
    class constructor Create;
    class destructor Destroy;
    { IInterface }
    function QueryInterface(const IID: TGUID; out Obj): HRESULT; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    { IEFInterface }
    function AsObject: TObject;
    { IEFObserver }
    procedure UpdateObserver(const ASubject: IEFSubject; const AContext: string = '');

    function DisplayNewController(const AView: TKView; const AForceModal: Boolean = False;
      const AAfterCreateWindow: TProc<TKExtControllerHostWindow> = nil;
      const AAfterCreate: TProc<IKExtController> = nil): IKExtController;
  public
    function FindPageTemplate(const APageName: string): string;
    function GetPageTemplate(const APageName: string): string;

    /// <summary>
    ///  Checks user credentials (fetched from Query parameters UserName and Passwords)
    ///  and returns True if the current authenticator allows them, or if the
    ///  user was already authenticated in this session.
    /// </summary>
    function Authenticate: Boolean;

    procedure Refresh; override;
    /// <summary>
    ///  A reference to the main view container.
    /// </summary>
    property ViewHost: IKExtViewHost read FViewHost write SetViewHost;

    /// <summary>
    ///  A reference to the status bar to be used for wait messages.
    /// </summary>
    property StatusHost: TKExtStatusBar read FStatusHost write FStatusHost;

    procedure DisplayView(const AName: string); overload;
    procedure DisplayView(const AView: TKView); overload;

    procedure InitDefaultValues; override;
    procedure Home; override;

    /// <summary>
    ///  Opens the specified URL in a new browser window/tab.
    /// </summary>
    procedure Navigate(const AURL:string);
    /// <summary>
    ///  <para>
    ///   Adds to the current session a style class named after AView's
    ///   ImageName (or the specified custom AImageName) plus a '_img'
    ///   suffix, that sets background:url to the URL of the view's image.
    ///  </para>
    ///  <para>
    ///   The style class can have an optional custom prefix before the name
    ///   and custom rules attached to it.
    ///  </para>
    /// </summary>
    /// <returns>
    ///  Returns the class name so that it can be assigned to a component's
    ///  IconCls property.
    /// </returns>
    function SetViewIconStyle(const AView: TKView; const AImageName: string = '';
      const ACustomPrefix: string = ''; const ACustomRules: string = ''): string;
    function SetIconStyle(const ADefaultImageName: string; const AImageName: string = '';
      const ACustomPrefix: string = ''; const ACustomRules: string = ''): string;

    // Test
    function GetGCObjectCount: Integer;

    procedure Flash(const AMessage: string);

    property Config: TKConfig read GetConfig;

    /// <summary>
    ///  Called to signal that a new file has been uploaded. The
    ///  descriptor holds information about the file and its context
    ///  (for example which view is going to use it).
    /// </summary>
    /// <remarks>
    ///  The session acquires ownership of the descriptor object.
    /// </remarks>
    procedure AddUploadedFile(const AFileDescriptor: TKExtUploadedFile);

    /// <summary>
    ///  Removes a previously added file descriptor. To be called once
    ///  the uploaded file has been processed.
    /// </summary>
    procedure RemoveUploadedFile(const AFileDescriptor: TKExtUploadedFile);

    /// <summary>
    ///  Returns the first uploaded file descriptor matching the
    ///  specified context, or nil if no descriptor is found.
    /// </summary>
    function FindUploadedFile(const AContext: TObject): TKExtUploadedFile;

    /// <summary>
    ///  Calls AProc for each uploaded file in list.
    /// </summary>
    procedure EnumUploadedFiles(const AProc: TProc<TKExtUploadedFile>);

    /// <summary>
    ///  If the specified object is found in the list of open controllers,
    ///  it is removed from the list. Otherwise nothing happens.
    ///  Used by view hosts to notify the session that a controller was closed.
    /// </summary>
    procedure RemoveController(const AObject: TObject);

    /// <summary>
    ///  Returns all request query param name and values as an ISuperObject.
    ///  Note: All values are treated as strings.
    /// </summary>
    function GetQueries: ISuperObject;

    /// <summary>
    ///  The current session's UUID.
    /// </summary>
    property SessionId: string read FSessionId;

    /// <summary>
    ///  Ensures that existing js and css files with the specified base name
    ///  are dynamically added to the page. If the specified files don't exist
    ///  or were already added, nothing is done.
    /// </summary>
    procedure EnsureSupportFiles(const ABaseName: string);

    /// <summary>
    ///  Ensures that existing js and css files with a base name that depends
    ///  on the specified view are dynamically added to the page.
    ///  If the view has a 'SupportBaseName' attribute, then it is used as the
    //   base name for the support files, otherwise the view's name (if any)
    ///  is used.
    ///  If the specified files don't exist or were already added, nothing is done.
    /// </summary>
    procedure EnsureViewSupportFiles(const AView: TKView);

    /// <summary>
    ///  True if the last request came from a mobile browser.
    ///  The user agent detection is performed once per session and then cached.
    /// </summary>
    function IsMobileBrowser: Boolean;

    /// <summary>
    ///  Viewport width in mobile applications.
    /// </summary>
    property ViewportWidth: Integer read FViewportWidth;

    /// <summary>
    ///  True if tooltips are enabled for the session. By default, tooltips
    ///  are enabled for desktop browsers and disabled for mobile browsers.
    /// </summary>
    function TooltipsEnabled: Boolean;

    property CreationDateTime: TDateTime read FCreationDateTime;
    property LastRequestDateTime: TDateTime read FLastRequestDateTime;
    property DisplayName: string read GetDisplayName write SetDisplayName;
    function GetLoggedInUserName: string;
    function GetOrigin: string;
  published
    procedure DelayedHome;
    procedure Logout;
  end;
  TKExtSessionGetEvent = reference to procedure(out ASession: TKExtSession);

  /// <summary>
  ///  This helper guarantees that each Ext object has access to the current
  ///  thread's session, cast to the correct type.
  /// </summary>
  TKExtObjectHelper = class helper for TExtObject
  private
    function GetSession: TKExtSession;
  public
    property Session: TKExtSession read GetSession;
  end;

  TKExtDelayedHome = class(TExtFunction)
  public
    function GetViewportWidthInInches: TExtFunction;
  published
    procedure Execute;
  end;

implementation

uses
  StrUtils, ActiveX, ComObj, FmtBcd,
  ExtPascalUtils, ExtForm,
  EF.SysUtils, EF.StrUtils, EF.Logger, EF.Types,
  Kitto.Auth, Kitto.Types, Kitto.AccessControl,
  Kitto.Ext.Utils;

function Session: TKExtSession;
begin
  Result := TKExtSession(ExtPascal.Session);
end;

{ TKExtSession }

function TKExtSession.GetConfig: TKConfig;
begin
  if not Assigned(FConfig) then
    FConfig := TKConfig.Create;
  Result := FConfig;
end;

function TKExtSession.GetCustomJS: string;
begin
  Result :=
    'function setViewportWidth(w) {' + sLineBreak +
    '  var defWidth = ' + IntToStr(DEFAULT_VIEWPORT_WIDTH) + ';' + sLineBreak +
    '  var mvp = document.getElementById("viewport");' + sLineBreak +
    '  if (w != defWidth)' + sLineBreak +
    '    mvp.setAttribute("content", "' + ReplaceStr(FViewportContent, '{width}', '" + w + "') + '");' + sLineBreak +
    '}';
end;

type
  PGarbage = ^TGarbage;

  TGarbage = record
    Garbage: TObject;
    Persistent: Boolean;
  end;

function TKExtSession.GetGCObjectCount: Integer;
var
  I: Integer;
  LObject: TObject;
begin
  Result := 0;
  for I := 0 to FGarbageCollector.Count - 1 do
  begin
    LObject := FGarbageCollector.Objects[I];
    if (LObject <> nil) and (PGarbage(LObject)^.Garbage <> nil) then
      Inc(Result);
  end;
end;

function TKExtSession.GetMainPageTemplate: string;
begin
  Result := GetPageTemplate('index');
end;

function TKExtSession.GetManifestFileName: string;
var
  LManifestFile, LURL: string;
begin
  LManifestFile := GetHomeView.GetString('MobileSettings/Android/Manifest', 'Manifest.json');
  LURL := Config.FindResourceURL(LManifestFile);
  if LURL <> '' then
    Result := LURL
  else
    Result := '';
end;

function TKExtSession.GetOrigin: string;
begin
  Result := RequestHeader['REMOTE_ADDR'];
end;

function TKExtSession.FindPageTemplate(const APageName: string): string;
var
  LFileName: string;

  function GetEncoding: TEncoding;
  begin
    if Charset = 'utf-8' then
      Result := TEncoding.UTF8
    else
      Result := TEncoding.ANSI;
  end;

begin
  LFileName := Config.FindResourcePathName(APageName + '.html');
  if LFileName <> '' then
  begin
    Result := TextFileToString(LFileName, GetEncoding);
    Result := TEFMacroExpansionEngine.Instance.Expand(Result);
  end;
end;

function TKExtSession.GetPageTemplate(const APageName: string): string;
begin
  Result := FindPageTemplate(APageName);
  if Result = '' then
    raise Exception.CreateFmt('Template not found for page %s', [APageName]);
end;

function TKExtSession.GetQueries: ISuperObject;
var
  I: Integer;
begin
  Result := SO();
  for I := 0 to Queries.Count - 1 do
    Result.S[Queries.Names[I]] := Queries.ValueFromIndex[I];
end;

function TKExtSession.GetSessionCookieName: string;
begin
  Result := 'kitto';
end;

function TKExtSession.GetLoggedInUserName: string;
begin
  if Config.Authenticator.IsAuthenticated then
    Result := Config.Authenticator.UserName
  else
    Result := '<Not Authenticated>';
end;

function TKExtSession.GetViewportContent: string;
begin
  Result := ReplaceStr(FViewportContent, '{width}', IntToStr(DEFAULT_VIEWPORT_WIDTH));
end;

procedure TKExtSession.FreeLoginNode;
begin
  // Free login node if one was manufactured.
  if FOwnsLoginNode and Assigned(FLoginNode) then
  begin
    Config.Views.DeleteNonpersistentObject(FLoginNode);
    FreeAndNil(FLoginNode);
  end;
end;

destructor TKExtSession.Destroy;
var
  LUploadDirectory: string;
begin
  // Delete upload folder only for valid sessions.
  if FSessionId <> '' then
  begin
    LUploadDirectory := ReplaceStr(DocumentRoot + UploadPath, '/', '\');
    if DirectoryExists(LUploadDirectory) then
      DeleteTree(LUploadDirectory);
  end;
  FreeLoginNode;
  FConfig.MacroExpansionEngine.RemoveExpander(FMacroExpander);
  FreeAndNil(FMacroExpander);
  FreeAndNil(FConfig);
  FreeAndNil(FUploadedFiles);
  FreeAndNil(FHomeController);
  FreeAndNil(FLoginController);
  FreeAndNil(FGettextInstance);
  FreeAndNil(FDynamicScripts);
  FreeAndNil(FDynamicStyles);
  inherited;
  // Keep it alive as the inherited call might trigger calls to
  // RemoveController from objects being destroyed.
  FreeAndNil(FOpenControllers);
end;

class destructor TKExtSession.Destroy;
begin
  TEFMacroExpansionEngine.OnGetInstance := nil;
  TKConfig.OnGetInstance := nil;
end;

procedure TKExtSession.ReloadOrDisplayHomeView;
var
  LNewLanguageId: string;
begin
  LNewLanguageId := Queries.Values['Language'];
  if (LNewLanguageId <> '') and (LNewLanguageId <> Language) then
  begin
    FRefreshingLanguage := True;
    Language := LNewLanguageId;
  end;
  DisplayHomeView;
end;

function TKExtSession.GetDefaultHomeViewNodeNames(const ASuffix: string): TStringDynArray;
begin
  case FViewportWidthInInches of
    0..5:
    begin
      SetLength(Result, 3);
      Result[0] := 'HomeTiny' + ASuffix;
      Result[1] := 'HomeSmall' + ASuffix;
      Result[2] := 'Home' + ASuffix;
    end;
    6..10:
    begin
      SetLength(Result, 2);
      Result[0] := 'HomeSmall' + ASuffix;
      Result[1] := 'Home' + ASuffix;
    end
  else
    begin
      SetLength(Result, 1);
      Result[0] := 'Home' + ASuffix;
    end;
  end;
end;

function TKExtSession.GetDefaultViewportWidth: Integer;
begin
  Result := FViewportWidthInInches * 96;
//  case FViewportWidthInInches of
//    0..4: Result := 320;
//    5..7: Result := 480;
//    8..10: Result := 640;
//  else
//    Result := 0;
//  end;
end;

function TKExtSession.GetDisplayName: string;
begin
  Result := FDisplayName;
  if Result = '' then
    Result := SessionId;
end;

function TKExtSession.GetHomeView: TKView;
var
  LNodeNames: TStringDynArray;
begin
  if FHomeViewNodeName <> '' then
  begin
    SetLength(LNodeNames,1);
    LNodeNames[0] := FHomeViewNodeName;
  end
  else
    LNodeNames := GetDefaultHomeViewNodeNames('View');
  Result := Config.Views.FindViewByNode(Config.Config.FindNode(LNodeNames));
  if not Assigned(Result) then
    Result := Config.Views.ViewByName(GetDefaultHomeViewNodeNames(''));
end;

procedure TKExtSession.DisplayHomeView;
var
  LHomeView: TKView;
  LIntf: IKExtController;
begin
  FreeAndNil(FHomeController);
  LHomeView := GetHomeView;
  FHomeController := TKExtControllerFactory.Instance.CreateController(ObjectCatalog, LHomeView, nil).AsObject;
  if Supports(FHomeController, IKExtController, LIntf) then
    LIntf.Display;
  if FAutoOpenViewName <> '' then
  begin
    DisplayView(FAutoOpenViewName);
    FAutoOpenViewName := '';
  end;
  if FHomeController is TExtContainer then
    TExtContainer(FHomeController).DoLayout;
end;

procedure TKExtSession.Home;

  procedure SetAjaxTimeout;
  var
    LTimeout: TEFNode;
  begin
    LTimeout := Config.Config.FindNode('Ext/AjaxTimeout');
    if Assigned(LTimeout) then
      ExtAjax.Timeout := LTimeout.AsInteger;
  end;

begin
  if IsAjax then
    raise Exception.Create('Cannot navigate to home in async mode.');

  Refresh;
  if not FRefreshingLanguage then
    Config.Authenticator.Logout;
  FHomeController := nil;
  FLoginController := nil;
  FreeLoginNode;
  FOpenControllers.Clear;
  FViewHost := nil;
  FStatusHost := nil;
  FDynamicScripts.Clear;
  FDynamicStyles.Clear;
  FreeAndNil(FControllerHostWindow);

  LoadLibraries;

  FHomeViewNodeName := Queries.Values['home'];
  SetViewportContent;
  ResponseItems.ExecuteJSCode('kittoInit();');
  SetAjaxTimeout;
  if TooltipsEnabled then
    ExtQuickTips.Init(True)
  else
    ExtQuickTips.Disable;

  if not FRefreshingLanguage then
    SetLanguageFromQueriesOrConfig;

  FAutoOpenViewName := Queries.Values['view'];
  if FAutoOpenViewName <> '' then
    Queries.Values['view'] := '';

  with TKExtDelayedHome.Create(ObjectCatalog) do
  begin
    try
      Execute;
    finally
      Free;
    end;
  end;
end;

procedure TKExtSession.DelayedHome;
var
  LUserAgent: string;


begin
  if IsMobileApple then
  begin
    LUserAgent := RequestHeader['HTTP_USER_AGENT'];
    if LUserAgent.Contains('iPhone') then
      FViewportWidthInInches := 4
    else
      FViewportWidthInInches := 8;
  end
  else
    FViewportWidthInInches := QueryAsInteger['vpWidthInches'];

  FViewportWidth := GetDefaultViewportWidth();
  ResponseItems.ExecuteJSCode('setViewportWidth(' + IntToStr(FViewportWidth) + ');');
  // Try authentication with default credentials, if any, and skip login
  // window if it succeeds.
  if Authenticate then
    DisplayHomeView
  else
    DisplayLoginView;
  FRefreshingLanguage := False;
end;

function TKExtSession.Authenticate: Boolean;
var
  LAuthData: TEFNode;
  LUserName: string;
  LPassword: string;
begin
  if Config.Authenticator.IsAuthenticated then
    Result := True
  else
  begin
    LAuthData := TEFNode.Create;
    try
      Config.Authenticator.DefineAuthData(LAuthData);
      LUserName := Query['UserName'];
      if LUserName <> '' then
        LAuthData.SetString('UserName', LUserName);
      LPassword := Query['Password'];
      if Query['Password'] <> '' then
        LAuthData.SetString('Password', LPassword);
      Result := Config.Authenticator.Authenticate(LAuthData);
    finally
      LAuthData.Free;
    end;
  end;
end;

function TKExtSession.GetLoginView: TKView;
begin
  if not Assigned(FLoginNode) then
  begin
    FOwnsLoginNode := False;
    FLoginNode := Config.Config.FindNode('Login');
    if not Assigned(FLoginNode) then
    begin
      Result := Config.Views.FindView('Login');
      if Assigned(Result) then
        Exit;

      FLoginNode := TEFNode.Create('Login');
      try
        FOwnsLoginNode := True;
        FLoginNode.SetString('Controller', 'Login');
      except
        FreeAndNil(FLoginNode);
        FOwnsLoginNode := False;
        raise;
      end;
    end;
  end;
  Result := Config.Views.FindViewByNode(FLoginNode);
  if not Assigned(Result) then
    raise Exception.Create('Login View not found');
end;

procedure TKExtSession.DisplayLoginView;
var
  LLoginView: TKView;
  LIntf: IKExtController;
  LType: string;
begin
  FreeAndNil(FLoginController);
  LLoginView := GetLoginView;
  if LLoginView.ControllerType = '' then
    LType := 'Login'
  else
    LType := '';
  FLoginController := TKExtControllerFactory.Instance.CreateController(ObjectCatalog, LLoginView, nil, nil, Self, LType).AsObject;
  if Supports(FLoginController, IKExtController, LIntf) then
    LIntf.Display;
  if FLoginController is TExtContainer then
    TExtContainer(FLoginController).DoLayout;
end;

function TKExtSession.FindUploadedFile(const AContext: TObject): TKExtUploadedFile;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FUploadedFiles.Count - 1 do
  begin
    if FUploadedFiles[I].Context = AContext then
    begin
      Result := FUploadedFiles[I];
      Break;
    end;
  end;
end;

procedure TKExtSession.Flash(const AMessage: string);
begin
  ResponseItems.ExecuteJSCode('Ext.example.msg("' + _(Config.AppTitle) + '", "' + AMessage + '");');
end;

procedure TKExtSession.LoadLibraries;

  procedure SetRequiredLibrary(const ALibName: string; const AIncludeCSS: Boolean = False);
  var
    LLibURL: string;
  begin
    LLibURL := Config.GetResourceURL(IncludeTrailingPathDelimiter('js') + ALibName + '.js');
    SetLibrary(StripSuffix(LLibURL, '.js'), AIncludeCSS, False, True);
  end;

  procedure SetOptionalLibrary(const ALibName: string; const AIncludeCSS: Boolean = False);
  var
    LLibURL: string;
  begin
    LLibURL := Config.FindResourceURL(IncludeTrailingPathDelimiter('js') + ALibName + '.js');
    if LLibURL <> '' then
      SetLibrary(StripSuffix(LLibURL, '.js'), False, False, True);
    if AIncludeCSS then
    begin
      LLibURL := Config.FindResourceURL(IncludeTrailingPathDelimiter('js') + ALibName + '.css');
      if LLibURL <> '' then
        SetCSS(StripSuffix(LLibURL, '.css'), False);
    end;
  end;

var
  LLibraries: TStringDynArray;
  LLibName: string;
begin
{ TODO :
Find a way to reference optional libraries only if the controllers that need
them are linked in; maybe a global repository fed by initialization sections.
Duplicates must be handled/ignored. }
  SetLibrary(ExtPath + '/examples/ux/statusbar/StatusBar');
  SetCSS(ExtPath + '/examples/ux/statusbar/css/statusbar');

  SetLibrary(ExtPath + '/examples/ux/fileuploadfield/FileUploadField');
  SetCSS(ExtPath + '/examples/ux/fileuploadfield/css/fileuploadfield');

  SetLibrary(ExtPath + '/examples/ux/RowEditor');
  SetCSS(ExtPath + '/examples/shared/examples');
  SetCSS(ExtPath + '/examples/ux/css/RowEditor');

  SetLibrary(ExtPath + '/examples/shared/examples'); // For Ext.msg.
  SetRequiredLibrary('DateTimeField');
  SetRequiredLibrary('NumericField');
  SetRequiredLibrary('DefaultButton');
  SetRequiredLibrary('kitto-core', True);
  if Session.IsMobileBrowser then
    SetRequiredLibrary('kitto-core-mobile', True)
  else
    SetRequiredLibrary('kitto-core-desktop', True);
  SetRequiredLibrary('kitto-init');
  SetOptionalLibrary('application', True);

  LLibraries := Config.Config.GetStringArray('JavaScriptLibraries');
  for LLibName in LLibraries do
    SetRequiredLibrary(LLibName);
end;

procedure TKExtSession.Logout;
begin
  Config.Authenticator.Logout;
  Reload;
end;

procedure TKExtSession.Reload;
begin
  // Ajax calls are useless since we're reloading, so let's make sure
  // the response doesn't contain any.
  ResponseItems.Clear;
  ResponseItems.ExecuteJSCode('window.location.reload();');
end;

procedure TKExtSession.Navigate(const AURL: string);
begin
  ResponseItems.ExecuteJSCode(Format('window.open("%s", "_blank");', [AURL]));
end;

function TKExtSession.QueryInterface(const IID: TGUID; out Obj): HRESULT;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

procedure TKExtSession.Refresh;
begin
  inherited;
  Config.InvalidateCatalogs;
end;

procedure TKExtSession.RemoveController(const AObject: TObject);
begin
  FOpenControllers.Remove(AObject);
end;

procedure TKExtSession.RemoveUploadedFile(const AFileDescriptor: TKExtUploadedFile);
begin
  FUploadedFiles.Remove(AFileDescriptor);
end;

procedure TKExtSession.DisplayView(const AName: string);
begin
  Assert(AName <> '');

  DisplayView(Config.Views.ViewByName(AName));
end;

function TKExtSession.DisplayNewController(const AView: TKView; const AForceModal: Boolean;
  const AAfterCreateWindow: TProc<TKExtControllerHostWindow>;
  const AAfterCreate: TProc<IKExtController>): IKExtController;
var
  LIsSynchronous: Boolean;
  LWidth: Integer;
  LHeight: Integer;
  LIsModal: Boolean;
begin
  Assert(Assigned(AView));

  // If there's no view host, we treat all views as windows.
  LIsModal := AForceModal or not Assigned(FViewHost) or AView.GetBoolean('Controller/IsModal');
  if Assigned(FControllerHostWindow) then
  begin
    FControllerHostWindow.Free(True);
    FControllerHostWindow := nil;
  end;
  if LIsModal then
  begin
    FControllerHostWindow := TKExtControllerHostWindow.Create(ObjectCatalog);
    if Assigned(AAfterCreateWindow) then
      AAfterCreateWindow(FControllerHostWindow);
    Result := TKExtControllerFactory.Instance.CreateController(ObjectCatalog, AView, FControllerHostWindow);
    if Assigned(AAfterCreate) then
      AAfterCreate(Result);
    if not Result.Config.GetBoolean('Sys/SupportsContainer') then
      FreeAndNil(FControllerHostWindow)
    else
    begin
      FControllerHostWindow.Layout := lyFit;
      if FControllerHostWindow.Title = '' then
        FControllerHostWindow.Title := _(AView.DisplayLabel);
      FControllerHostWindow.Closable := AView.GetBoolean('Controller/AllowClose', True);
      FControllerHostWindow.FHostedController := Result.AsObject;
      FControllerHostWindow.Maximized := IsMobileBrowser;
      FControllerHostWindow.Border := not FControllerHostWindow.Maximized;
      LWidth := AView.GetInteger('Controller/PopupWindow/Width', -1);
      LHeight := AView.GetInteger('Controller/PopupWindow/Height', -1);
      if (LWidth <> -1) and (LHeight <> -1) then
      begin
        FControllerHostWindow.Width := LWidth;
        FControllerHostWindow.Height := LHeight;
        Result.Config.SetBoolean('Sys/HostWindow/AutoSize', False);
      end
      else
        Result.Config.SetBoolean('Sys/HostWindow/AutoSize', True);
      Result.Config.SetObject('Sys/HostWindow', FControllerHostWindow);
    end;
  end
  else
  begin
    Assert(Assigned(FViewHost));
    Result := TKExtControllerFactory.Instance.CreateController(ObjectCatalog, AView, FViewHost.AsExtContainer);
    Assert(Result.Config.GetBoolean('Sys/SupportsContainer'));
  end;
  LIsSynchronous := Result.IsSynchronous;
  if not LIsSynchronous then
    FOpenControllers.Add(Result.AsObject);
  try
    Result.Display;
    if Assigned(FControllerHostWindow) and not LIsSynchronous then
      FControllerHostWindow.Show;
  except
    if Assigned(FControllerHostWindow) and not LIsSynchronous then
      FControllerHostWindow.Hide;
    FOpenControllers.Remove(Result.AsObject);
    FreeAndNilEFIntf(Result);
    raise;
  end;
  // Synchronous controllers end their life inside Display.
  if LIsSynchronous then
    NilEFIntf(Result);
end;

function TKExtSession.FindOpenController(const AView: TKView): IKExtController;
var
  I: Integer;
begin
  Assert(Assigned(AView));

  Result := nil;
  for I := 0 to FOpenControllers.Count - 1 do
  begin
    if Supports(FOpenControllers[I], IKExtController, Result) then
    begin
      if Result.View = AView then
        Break
      else
        Result := nil;
    end;
  end;
end;

procedure TKExtSession.DisplayView(const AView: TKView);
var
  LController: IKExtController;
begin
  Assert(Assigned(AView));

  if AView.IsAccessGranted(ACM_VIEW) then
  begin
    try
      if AView.GetBoolean('Controller/AllowMultipleInstances') then
        LController := DisplayNewController(AView)
      else
      begin
        LController := FindOpenController(AView);
        if not Assigned(LController) then
          LController := DisplayNewController(AView);
      end;
      if Assigned(LController) and Assigned(FViewHost) and LController.Config.GetBoolean('Sys/SupportsContainer') then
        SetActiveViewInViewHost(LController.AsObject);
    finally
      ClearStatus;
    end;
  end;
end;

procedure TKExtSession.SetActiveViewInViewHost(const AObject: TObject);
var
  I: Integer;
  LViewHost: IKExtViewHost;
begin
  Assert(Assigned(FViewHost));
  Assert(Assigned(AObject));

  if Supports(FViewHost, IKExtViewHost, LViewHost) then
  begin
    for I := 0 to FViewHost.AsExtContainer.Items.Count - 1 do
    begin
      if FViewHost.AsExtContainer.Items[I] = AObject then
      begin
        LViewHost.SetActiveView(I);
        Break;
      end;
    end;
  end;
end;

procedure TKExtSession.SetDisplayName(const AValue: string);
begin
  FDisplayName := AValue;
end;

procedure TKExtSession.ClearStatus;
begin
  if Assigned(FStatusHost) then
    FStatusHost.ClearStatus;
end;

procedure TKExtSession.EnsureDynamicScript(const AScriptBaseName: string);
var
  LIndex: Integer;
  LURL: string;
begin
  if not FDynamicScripts.Find(AScriptBaseName, LIndex) then
  begin
    LURL := Config.FindResourceURL(IncludeTrailingPathDelimiter('js') + AScriptBaseName + '.js');
    if LURL <> '' then
    begin
      ResponseItems.ExecuteJSCode(Format('addScriptRef("%s");', [LURL]));
      FDynamicScripts.Add(AScriptBaseName);
    end;
  end;
end;

procedure TKExtSession.EnsureDynamicStyle(const AStyleBaseName: string);
var
  LIndex: Integer;
  LURL: string;
begin
  if not FDynamicStyles.Find(AStyleBaseName, LIndex) then
  begin
    LURL := Config.FindResourceURL(IncludeTrailingPathDelimiter('js') + AStyleBaseName + '.css');
    if LURL <> '' then
    begin
      ResponseItems.ExecuteJSCode(Format('addLinkRef("%s");', [LURL]));
      FDynamicStyles.Add(AStyleBaseName);
    end;
  end;
end;

procedure TKExtSession.EnsureSupportFiles(const ABaseName: string);
begin
  if ABaseName <> '' then
  begin
    EnsureDynamicStyle(ABaseName);
    EnsureDynamicScript(ABaseName);
  end;
end;

procedure TKExtSession.EnsureViewSupportFiles(const AView: TKView);
var
  LBaseName: string;
  LBaseNameNode: TEFNode;
begin
  LBaseName := '';
  if Assigned(AView) then
  begin
    LBaseNameNode := AView.FindNode('SupportBaseName');
    if Assigned(LBaseNameNode) then
      LBaseName := LBaseNameNode.AsString
    else
      LBaseName := AView.PersistentName;
  end;
  if LBaseName <> '' then
    EnsureSupportFiles(LBaseName);
end;

procedure TKExtSession.EnumUploadedFiles(const AProc: TProc<TKExtUploadedFile>);
var
  I: Integer;
begin
  if Assigned(AProc) then
  begin
    for I := FUploadedFiles.Count - 1 downto 0 do
      AProc(FUploadedFiles[I]);
  end;
end;

procedure TKExtSession.InitDefaultValues;
begin
  inherited;
end;

function TKExtSession.IsMobileBrowser: Boolean;
var
  LUserAgent: string;
begin
  if not FMobileBrowserDetectionDone then
  begin
    LUserAgent := RequestHeader['HTTP_USER_AGENT'];
    TEFLogger.Instance.Log('UserAgent: ' + LUserAgent);
    FIsMobileBrowser := LUserAgent.Contains('Windows Phone') or
      LUserAgent.Contains('iPhone') or
      LUserAgent.Contains('iPad') or
      LUserAgent.Contains('Android');
    FMobileBrowserDetectionDone := True;
    TEFLogger.Instance.Log('IsMobileBrowser: ' + BoolToStr(FIsMobileBrowser, True));
  end;
  Result := FIsMobileBrowser;
end;

procedure TKExtSession.AddUploadedFile(
  const AFileDescriptor: TKExtUploadedFile);
begin
  FUploadedFiles.Add(AFileDescriptor);
end;

procedure TKExtSession.AfterHandleRequest;
begin
  inherited;
  { TODO : only do this when ADO is used }
  CoUninitialize;
end;

procedure TKExtSession.AfterNewSession;
begin
  inherited;
  FCreationDateTime := Now;
  FSessionId := SessionCookie;
  TEFLogger.Instance.LogFmt('New session %s.', [FSessionId],
    TEFLogger.LOG_MEDIUM);
  UploadPath := '/uploads/' + Config.AppName + '/' + SessionGUID;
  ExtPath := Config.Config.GetString('Ext/URL', '/ext');
  Charset := Config.Config.GetString('Charset', 'utf-8');
  Theme := Config.Config.GetString('Ext/Theme');
end;

function TKExtSession.AsObject: TObject;
begin
  Result := Self;
end;

function TKExtSession.BeforeHandleRequest: Boolean;
begin
  FLastRequestDateTime := Now;
  TEFLogger.Instance.LogStrings('BeforeHandleRequest', Queries,
    TEFLogger.LOG_DETAILED);
  { TODO : only do this when ADO is used }
  OleCheck(CoInitialize(nil));
  if NewThread and not IsAjax then
    SetLanguageFromQueriesOrConfig;
  Result := inherited BeforeHandleRequest;
end;

procedure TKExtSession.SetLanguageFromQueriesOrConfig;
var
  LLanguageId: string;
begin
  LLanguageId := Queries.Values['lang'];
  if LLanguageId = '' then
    LLanguageId := Config.Config.GetString('LanguageId');
  if LLanguageId <> '' then
    Language := LLanguageId;
end;

procedure TKExtSession.SetLanguage(const AValue: string);
begin
  inherited;
  TEFLocalizationToolRegistry.CurrentTool.ForceLanguage(AValue);
  TEFLogger.Instance.LogFmt('Language %s set.', [AValue], TEFLogger.LOG_MEDIUM);
  Config.Config.SetString('LanguageId', AValue);
end;

constructor TKExtSession.Create(AOwner: TObject);
begin
  inherited;
  FUploadedFiles := TObjectList<TKExtUploadedFile>.Create;
  FOpenControllers := TObjectList<TObject>.Create(False);
  FMacroExpander := TKExtSessionMacroExpander.Create(Self);
  Config.MacroExpansionEngine.AddExpander(FMacroExpander);
  FGettextInstance := TGnuGettextInstance.Create;
  FDynamicScripts := TStringList.Create;
  FDynamicScripts.Sorted := True;
  FDynamicScripts.Duplicates := dupError;
  FDynamicStyles := TStringList.Create;
  FDynamicStyles.Sorted := True;
  FDynamicStyles.Duplicates := dupError;
  FMobileBrowserDetectionDone := False;
  FOwnsLoginNode := False;
end;

class constructor TKExtSession.Create;
begin
  TEFMacroExpansionEngine.OnGetInstance :=
    function: TEFMacroExpansionEngine
    begin
      if Session <> nil then
        Result := Session.Config.MacroExpansionEngine
      else
        Result := nil;
    end;
  TKConfig.OnGetInstance :=
    function: TKConfig
    begin
      if Session <> nil then
        Result := Session.Config
      else
        Result := nil;
    end;
end;

procedure TKExtSession.SetViewHost(const AValue: IKExtViewHost);
begin
  FViewHost := AValue;
end;

function TKExtSession.SetViewIconStyle(const AView: TKView; const AImageName: string;
  const ACustomPrefix: string; const ACustomRules: string): string;
begin
  Assert(Assigned(AView));
  Result := SetIconStyle(AView.ImageName, AImageName, ACustomPrefix, ACustomRules);
end;

procedure TKExtSession.SetViewportContent;
var
  LPair: TEFPair;
  LPairs: TEFPairs;

  function GetSeparator: string;
  begin
    // IE on Windows Phone wants comma, others want space.
    if RequestHeader['HTTP_USER_AGENT'].Contains('Windows Phone') then
      Result := ', '
    else
      Result := ' ';
  end;

begin
  FViewportContent := '';
  SetLength(LPairs, 2);
  LPairs[0] := TEFPair.Create('width', '{width}');
  LPairs[1] := TEFPair.Create('user-scalable', '0');
  LPairs := GetHomeView.GetChildrenAsPairs('MobileSettings/ViewportContent', True, LPairs);
  for LPair in LPairs do
  begin
    if FViewportContent = '' then
      FViewportContent := LPair.Key + '=' + LPair.Value
    else
      FViewportContent := FViewportContent + GetSeparator + LPair.Key + '=' + LPair.Value;
  end;
end;

function TKExtSession.SetIconStyle(const ADefaultImageName: string;
  const AImageName: string; const ACustomPrefix: string;
  const ACustomRules: string): string;
var
  LIconURL: string;
  LRule: string;
  LSelector: string;
begin
  Result := IfThen(AImageName <> '', AImageName, ADefaultImageName);
  LIconURL := Config.GetImageURL(Result);
  Result := ACustomPrefix + Result + '_img';
  // The !important rule allows to use a non-specific selector, so that the icon
  // can be shared by different components.
  // no-repeat is added because some components (such as buttons) repeat by default
  // (others, such as menu items and tree nodes, don't).
  LSelector := '.' + Result;
  LRule := '{background: url(' + LIconURL + ') no-repeat left !important;' + ACustomRules + '}';
  if IsAjax then
    ResponseItems.ExecuteJSCode(Format('addStyleRule("%s", "%s");', [LSelector, LRule]))
  else
    SetStyle(LSelector + ' ' + LRule);
end;

function TKExtSession.TooltipsEnabled: Boolean;
begin
  Result := not IsMobileBrowser;
end;

procedure TKExtSession.UpdateObserver(const ASubject: IEFSubject; const AContext: string);
begin
  if (ASubject.AsObject = FLoginController) and SameText(AContext, 'LoggedIn') then
    ReloadOrDisplayHomeView;
end;

function TKExtSession._AddRef: Integer;
begin
  Result := -1;
end;

function TKExtSession._Release: Integer;
begin
  Result := -1;
end;

{ TKExtUploadedFile }

constructor TKExtUploadedFile.Create(const AFileName, AFullFileName: string;
  const AContext: TObject; const AOriginalFileName: string = '');
begin
  inherited Create;
  FFileName := AFileName;
  FFullFileName := AFullFileName;
  FContext := AContext;
  FOriginalFileName := AOriginalFileName;
end;

destructor TKExtUploadedFile.Destroy;
begin
  FreeAndNil(FStream);
  inherited;
end;

function TKExtUploadedFile.GetBytes: TBytes;
begin
  if not Assigned(FStream) then
  begin
    FStream := TBytesStream.Create;
    FStream.LoadFromFile(FFullFileName);
  end;
  Result := FStream.Bytes;
  // Reset length, as FStream.Bytes for some reason is rounded up.
  SetLength(Result, FStream.Size);
  Assert(FStream.Size = Length(Result));
end;

{ TKExtObjectHelper }

function TKExtObjectHelper.GetSession: TKExtSession;
begin
  Result := ExtSession as TKExtSession;
end;

{ TKExtSessionMacroExpander }

constructor TKExtSessionMacroExpander.Create(const ASession: TKExtSession);
begin
  inherited Create;
  FSession := ASession;
end;

function TKExtSessionMacroExpander.InternalExpand(
  const AString: string): string;
begin
  Result := inherited InternalExpand(AString);
  if Assigned(FSession) then
  begin
    Result := ExpandMacros(Result, '%SESSION_ID%', FSession.SessionId);
    Result := ExpandMacros(Result, '%LANGUAGE_ID%', FSession.Language);
  end;
end;

{ TKExtSessionLocalizationTool }

procedure TKExtSessionLocalizationTool.AfterConstruction;
begin
  inherited;
  // Configure the global dxgettext instance.
  GetGnuGettextInstance.bindtextdomain(KITTO_TEXT_DOMAIN,
    TKConfig.SystemHomePath + 'locale');
end;

function TKExtSessionLocalizationTool.AsObject: TObject;
begin
  Result := Self;
end;

procedure TKExtSessionLocalizationTool.ForceLanguage(const ALanguageId: string);
var
  LInstance: TGnuGettextInstance;
begin
  LInstance := GetGnuGettextInstance;
  // Configure the per-session dxgettext instance.
  LInstance.bindtextdomain(KITTO_TEXT_DOMAIN,
    TKConfig.SystemHomePath + 'locale');
  LInstance.UseLanguage(ALanguageId);
end;

function TKExtSessionLocalizationTool.GetCurrentLanguageId: string;
begin
  Result := GetGnuGettextInstance.GetCurrentLanguage;
end;

function TKExtSessionLocalizationTool.GetGnuGettextInstance: TGnuGettextInstance;
begin
  if Session <> nil then
    Result := Session.FGettextInstance
  else
    Result := gnugettext.DefaultInstance;
end;

procedure TKExtSessionLocalizationTool.TranslateComponent(const AComponent: TComponent);
var
  LInstance: TGnuGettextInstance;
begin
  LInstance := GetGnuGettextInstance;
  LInstance.TranslateComponent(AComponent, KITTO_TEXT_DOMAIN);
  LInstance.TranslateComponent(AComponent, 'default');
end;

function TKExtSessionLocalizationTool.TranslateString(const AString,
  AIdString: string): string;
var
  LInstance: TGnuGettextInstance;
begin
  // Look in the Kitto text domain first, then in the application domain.
  LInstance := GetGnuGettextInstance;
  Result := LInstance.dgettext(KITTO_TEXT_DOMAIN, AString);
  if Result = AString then
    Result := LInstance.dgettext('default', AString);
end;

{ TKExtControllerHostWindow }

function TKExtControllerHostWindow.GetControllerToRemove: TObject;
begin
  Assert(Assigned(FHostedController));

  Result := FHostedController;
end;

{ TKExtDelayedHome }

procedure TKExtDelayedHome.Execute;
begin
  Session.ResponseItems.ExecuteJSCode(GetAjaxCode(Session.DelayedHome, ['vpWidthInches', GetViewportWidthInInches]));
end;

function TKExtDelayedHome.GetViewportWidthInInches: TExtFunction;
begin
  Session.ResponseItems.ExecuteJSCode(Self, 'getViewportWidthInInches()');
  Result := Self;
end;

initialization
  TEFLocalizationToolRegistry.RegisterTool(TKExtSessionLocalizationTool.Create);

finalization
  TEFLocalizationToolRegistry.UnregisterTool;

end.
