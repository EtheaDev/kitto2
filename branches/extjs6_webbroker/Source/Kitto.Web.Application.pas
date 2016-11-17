unit Kitto.Web.Application;

interface

uses
  Types
  , SysUtils
  , Classes
  , EF.Macros
  , Generics.Collections
  , Rtti
  , EF.Tree
  , EF.Intf
  , EF.ObserverIntf
  , Kitto.Config
  , Kitto.Metadata.Views
  , Kitto.Web
  , Kitto.Web.URL
  , Kitto.Web.Request
  , Kitto.Web.Response
  , Kitto.Ext.Base
  , Kitto.Ext.Controller
  ;

type
  { TODO : move away? Need TKExtApplication? }
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

  TLibraryRef = record
    IsCSS: Boolean;
    Path: string;
  end;

  TKWebApplicationSessionMacroExpander = class(TEFMacroExpander)
  strict protected
    function InternalExpand(const AString: string): string; override;
  end;

  TKWebApplication = class(TKWebRoute)
  private
    FConfig: TKConfig;
    FRttiContext: TRttiContext;
    FBasePath: string;
    FPath: string;
    FLoginNode: TEFNode;
    FOwnsLoginNode: Boolean;
    FTheme: string;
    FExtPath: string;
    FAdditionalRefs: TList<TLibraryRef>;
    FSessionMacroExpander: TKWebApplicationSessionMacroExpander;
    class threadvar FCurrent: TKWebApplication;
    function GetDefaultHomeViewNodeNames(const AViewportWidthInInches: Integer; const ASuffix: string): TStringDynArray;
    procedure Home;
    procedure FreeLoginNode;
    procedure ServeHomePage;
    function FindOpenController(const AView: TKView): IKExtController;
    procedure SetActiveViewInViewHost(const AObject: TObject);
    function GetMainPageTemplate: string;
    function GetManifestFileName: string;
    procedure ClearStatus;
    procedure LoadLibraries;
    procedure SetLibrary(const AURL: string; const AIsCSS: Boolean = False; const AHasDebug: Boolean = False);
    procedure SetCSS(const ACSS: string; const ACheck: Boolean = True);
    procedure SetViewportContent;
    function GetCustomJS: string;
    function GetViewportContent: string;
    procedure ActivateInstance;

    procedure DeactivateInstance;
    procedure Reload;
    function GetObjectFromURL(const AURL: TKURL): TObject;
    class function GetCurrent: TKWebApplication; static;
    class procedure SetCurrent(const AValue: TKWebApplication); static;
    function CallObjectMethod(const AObject: TObject; const AMethodName: string): Boolean;
  public
    const DEFAULT_VIEWPORT_WIDTH = 480;
    class constructor Create;
    class destructor Destroy;
    constructor Create(const APath: string);
    destructor Destroy; override;

    function HandleRequest(const ARequest: TKWebRequest; const AResponse: TKWebResponse; const AURL: TKURL): Boolean; override;
    procedure UpdateObserver(const ASubject: IEFSubject; const AContext: string = ''); override;

    property Name: string read FPath;
    property BasePath: string read FBasePath write FBasePath;

    property Config: TKConfig read FConfig;
    function GetHomeView(const AViewportWidthInInches: Integer): TKView;
    procedure DisplayView(const AName: string); overload;
    procedure DisplayView(const AView: TKView); overload;
    function FindPageTemplate(const APageName: string): string;
    function GetPageTemplate(const APageName: string): string;
    function DisplayNewController(const AView: TKView; const AForceModal: Boolean = False;
      const AAfterCreateWindow: TProc<TKExtControllerHostWindow> = nil;
      const AAfterCreate: TProc<IKExtController> = nil): IKExtController;
    property Theme: string read FTheme write FTheme;
    property ExtPath: string read FExtPath write FExtPath;

    class property Current: TKWebApplication read GetCurrent write SetCurrent;
    procedure ReloadOrDisplayHomeView;
    function GetLoginView: TKView;
    procedure DisplayHomeView;
    procedure DisplayLoginView;
    procedure Flash(const AMessage: string);
    procedure Navigate(const AURL: string);
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

    procedure DownloadFile(const AFileName: string; const AContentType: string = '');
    procedure DownloadStream(const AStream: TStream; const AFileName: string; const AContentType: string = '');
    procedure DownloadBytes(const ABytes: TBytes; const AFileName: string; const AContentType: string = '');
    /// <summary>
    ///  Checks user credentials (fetched from Query parameters UserName and Passwords)
    ///  and returns True if the current authenticator allows them, or if the
    ///  user was already authenticated in this session.
    /// </summary>
    function Authenticate: Boolean;
    /// <summary>
    ///  Call this in the initialization section of a unit defining a controller
    ///  to ensure that additional javascript or css files are included.
    /// </summary>
    procedure AddAdditionalRef(const APath: string; const AIsCSS: Boolean);
    procedure Alert(const AMessage: string);
    procedure Error(const AMessage, AMethodName, AParams: string);
    procedure NotFoundError(const AMethodName: string);
    procedure ErrorMessage(const AMessage: string; const AAction: string = '');
    function GetMethodURL(const AObjectName, AMethodName: string): string;
    /// <summary>
    ///  Returns the Home URL of the Kitto application assuming the URL is
    ///  visited from localhost.
    /// </summary>
    function GetHomeURL(const ATCPPort: Integer): string;

    { TODO : move to application once it gains the ability to execute ajax methods }
    procedure DelayedHome;
    procedure Logout;
  end;

implementation

uses
  StrUtils
  , EF.StrUtils
  , EF.Localization
  , EF.Types
  , Ext.Base
  , Kitto.AccessControl
  , Kitto.JS
  , Kitto.JS.Formatting
  , Kitto.Web.Types
  ;

{ TKExtControllerHostWindow }

function TKExtControllerHostWindow.GetControllerToRemove: TObject;
begin
  Assert(Assigned(FHostedController));

  Result := FHostedController;
end;

{ TKWebApplicationSessionMacroExpander }

function TKWebApplicationSessionMacroExpander.InternalExpand(
  const AString: string): string;
begin
  Result := inherited InternalExpand(AString);
  if Session <> nil then
  begin
    Result := ExpandMacros(Result, '%SESSION_ID%', Session.SessionId);
    Result := ExpandMacros(Result, '%LANGUAGE_ID%', Session.Language);
  end;
end;

{ TWebKApplication }

function TKWebApplication.GetObjectFromURL(const AURL: TKURL): TObject;
var
  LJSName: string;
begin
  LJSName := AURL.ExtractObjectName;
  if LJSName = '' then
    Result := Self
  else
    Result := Session.FindChildByJSName(LJSName);
  if not Assigned(Result) then
    raise Exception.CreateFmt('Handler object %s for method %s not found in session.', [LJSName, AURL.Document]);
end;

constructor TKWebApplication.Create(const APath: string);
begin
  Assert(APath <> '');

  inherited Create;
  FOwnsLoginNode := False;
  FPath := APath;
  FRttiContext := TRttiContext.Create;
  FConfig := TKConfig.Create;
  FAdditionalRefs := TList<TLibraryRef>.Create;
  FExtPath := '/ext';
  FTheme := Config.Config.GetString('Ext/Theme', 'triton');
  FSessionMacroExpander := TKWebApplicationSessionMacroExpander.Create;
  Config.MacroExpansionEngine.AddExpander(FSessionMacroExpander);
end;

destructor TKWebApplication.Destroy;
begin
  FreeLoginNode;
  FreeAndNil(FConfig);
  FreeAndNil(FAdditionalRefs);
  inherited;
end;

procedure TKWebApplication.AddAdditionalRef(const APath: string; const AIsCSS: Boolean);
var
  LValue: TLibraryRef;
begin
  LValue.Path := APath;
  LValue.IsCSS := AIsCSS;
  FAdditionalRefs.Add(LValue);
end;

procedure TKWebApplication.FreeLoginNode;
begin
  // Free login node only if one was manufactured.
  if FOwnsLoginNode and Assigned(FLoginNode) then
  begin
    Config.Views.DeleteNonpersistentObject(FLoginNode);
    FreeAndNil(FLoginNode);
  end;
end;

function TKWebApplication.GetHomeURL(const ATCPPort: Integer): string;
begin
  Result := 'http://localhost';
  if ATCPPort <> 80 then
    Result := Result + ':' + ATCPPort.ToString;
  Result := Result + FPath + '/home';
end;

function TKWebApplication.GetHomeView(const AViewportWidthInInches: Integer): TKView;
var
  LNodeNames: TStringDynArray;
begin
  if Session.HomeViewNodeName <> '' then
  begin
    SetLength(LNodeNames,1);
    LNodeNames[0] := Session.HomeViewNodeName;
  end
  else
    LNodeNames := GetDefaultHomeViewNodeNames(AViewportWidthInInches, 'View');
  Result := Config.Views.FindViewByNode(Config.Config.FindNode(LNodeNames));
  if not Assigned(Result) then
    Result := Config.Views.ViewByName(GetDefaultHomeViewNodeNames(AViewportWidthInInches, ''));
end;

function TKWebApplication.GetLoginView: TKView;
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

function TKWebApplication.GetDefaultHomeViewNodeNames(const AViewportWidthInInches: Integer;
  const ASuffix: string): TStringDynArray;
begin
  case AViewportWidthInInches of
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

function TKWebApplication.DisplayNewController(const AView: TKView; const AForceModal: Boolean;
  const AAfterCreateWindow: TProc<TKExtControllerHostWindow>;
  const AAfterCreate: TProc<IKExtController>): IKExtController;
var
  LIsSynchronous: Boolean;
  LIsModal: Boolean;
  LWindow: TKExtControllerHostWindow;
  LViewHost: IKExtViewHost;
begin
  Assert(Assigned(AView));

  // If there's no view host, we treat all views as windows.
  LIsModal := AForceModal or not Assigned(Session.ViewHost) or AView.GetBoolean('Controller/IsModal');
  if Assigned(Session.ControllerHostWindow) then
  begin
    Session.ControllerHostWindow.Delete;
    Session.ControllerHostWindow.Free;
    Session.ControllerHostWindow := nil;
  end;
  if LIsModal then
  begin
    LWindow := TKExtControllerHostWindow.Create(Session);
    Session.ControllerHostWindow := LWindow;
    if Assigned(AAfterCreateWindow) then
      AAfterCreateWindow(LWindow);
    Result := TKExtControllerFactory.Instance.CreateController(Session, AView, LWindow);
    if Assigned(AAfterCreate) then
      AAfterCreate(Result);
    if not Result.Config.GetBoolean('Sys/SupportsContainer') then
    begin
      Session.ControllerHostWindow.Free;
      Session.ControllerHostWindow := nil;
    end
    else
    begin
      LWindow.Layout := lyFit;
      if LWindow.Title = '' then
        LWindow.Title := _(AView.DisplayLabel);
      LWindow.Closable := AView.GetBoolean('Controller/AllowClose', True);
      LWindow.FHostedController := Result.AsObject;
      Result.Config.SetObject('Sys/HostWindow', LWindow);
      Result.Config.SetBoolean('Sys/HostWindow/AutoSize',
        LWindow.SetSizeFromTree(AView, 'Controller/PopupWindow/'));
    end;
  end
  else
  begin
    Assert(Assigned(Session.ViewHost));
    if not Supports(Session.ViewHost, IKExtViewHost, LViewHost) then
      raise Exception.Create('ViewHost does not support interface IKExtViewHost');
    Result := TKExtControllerFactory.Instance.CreateController(Session, AView, LViewHost.AsExtContainer);
    Assert(Result.Config.GetBoolean('Sys/SupportsContainer'));
  end;
  LIsSynchronous := Result.IsSynchronous;
  if not LIsSynchronous then
    Session.OpenControllers.Add(Result.AsObject);
  try
    Result.Display;
    if Assigned(Session.ControllerHostWindow) and not LIsSynchronous then
      TKExtControllerHostWindow(Session.ControllerHostWindow).Show;
  except
    if Assigned(Session.ControllerHostWindow) and not LIsSynchronous then
      TKExtControllerHostWindow(Session.ControllerHostWindow).Hide;
    Session.OpenControllers.Remove(Result.AsObject);
    FreeAndNilEFIntf(Result);
    raise;
  end;
  // Synchronous controllers end their life inside Display.
  if LIsSynchronous then
    NilEFIntf(Result);
end;

procedure TKWebApplication.DisplayView(const AName: string);
begin

end;

function TKWebApplication.FindOpenController(const AView: TKView): IKExtController;
var
  I: Integer;
begin
  Assert(Assigned(AView));

  Result := nil;
  for I := 0 to Session.OpenControllers.Count - 1 do
  begin
    if Supports(Session.OpenControllers[I], IKExtController, Result) then
    begin
      if Result.View = AView then
        Break
      else
        Result := nil;
    end;
  end;
end;

procedure TKWebApplication.DisplayView(const AView: TKView);
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
      if Assigned(LController) and Assigned(Session.ViewHost) and LController.Config.GetBoolean('Sys/SupportsContainer') then
        SetActiveViewInViewHost(LController.AsObject);
    finally
      ClearStatus;
    end;
  end;
end;

function TKWebApplication.CallObjectMethod(const AObject: TObject; const AMethodName: string): Boolean;
var
  LInfo : TRttiType;
  LMethod : TRttiMethod;
begin
  LInfo := FRttiContext.GetType(AObject.ClassType);
  LMethod := LInfo.GetMethod(AMethodName);
  Result := Assigned(LMethod);
  if Result then
    LMethod.Invoke(AObject, []);
end;

procedure TKWebApplication.ClearStatus;
begin
  if Assigned(Session.StatusHost) then
    TKExtStatusBar(Session.StatusHost).ClearStatus;
end;

class constructor TKWebApplication.Create;
begin
  TKConfig.OnGetInstance :=
    function: TKConfig
    begin
      if FCurrent <> nil then
        Result := FCurrent.Config
      else
        Result := nil;
    end;
end;

procedure TKWebApplication.SetActiveViewInViewHost(const AObject: TObject);
var
  I: Integer;
  LViewHost: IKExtViewHost;
begin
  Assert(Assigned(Session.ViewHost));
  Assert(Assigned(AObject));

  if Supports(Session.ViewHost, IKExtViewHost, LViewHost) then
  begin
    for I := 0 to LViewHost.AsExtContainer.Items.Count - 1 do
    begin
      if LViewHost.AsExtContainer.Items[I] = AObject then
      begin
        LViewHost.SetActiveView(I);
        Break;
      end;
    end;
  end;
end;

procedure TKWebApplication.SetCSS(const ACSS: string; const ACheck: Boolean);
var
  LCSS: string;
begin
  LCSS := ACSS.Replace('{ext}', ExtPath);
  if Pos(LCSS + '.css', Session.Libraries) = 0 then
    Session.Libraries := Session.Libraries + '<link rel=stylesheet href="' + LCSS + '.css" />';
end;

class procedure TKWebApplication.SetCurrent(const AValue: TKWebApplication);
begin
  FCurrent := AValue;
end;

procedure TKWebApplication.SetLibrary(const AURL: string; const AIsCSS, AHasDebug: Boolean);
var
  LURL: string;
begin
  Assert(AURL <> '');
  { TODO : refactor }
  LURL := AURL.Replace('{ext}', ExtPath);
  if Pos(LURL + '.js', Session.Libraries) = 0 then
  begin
    Session.Libraries := Session.Libraries + '<script src="' + LURL{$IFDEF DEBUGJS} + IfThen(HasDebug, '-debug', ''){$ENDIF} +
      '.js"></script>' + sLineBreak;
    { TODO : remove. use SetCSS }
    if AIsCSS then
      Session.Libraries := Session.Libraries + '<link rel=stylesheet href="' + LURL + '.css" />' + sLineBreak;
  end;
end;

function TKWebApplication.SetIconStyle(const ADefaultImageName: string;
  const AImageName: string; const ACustomPrefix: string;
  const ACustomRules: string): string;
var
  LIconURL: string;
  LRule: string;
  LSelector: string;
begin
  Assert(TKWebRequest.Current.IsAjax);

  Result := IfThen(AImageName <> '', AImageName, ADefaultImageName);
  LIconURL := Config.GetImageURL(Result);
  Result := ACustomPrefix + Result + '_img';
  // The !important rule allows to use a non-specific selector, so that the icon
  // can be shared by different components.
  // no-repeat is added because some components (such as buttons) repeat by default
  // (others, such as menu items and tree nodes, don't).
  LSelector := '.' + Result;
  LRule := '{background: url(' + LIconURL + ') no-repeat left !important;' + ACustomRules + '}';
  TKWebResponse.Current.Items.ExecuteJSCode(Format('addStyleRule("%s", "%s");', [LSelector, LRule]));
end;

function TKWebApplication.SetViewIconStyle(const AView: TKView; const AImageName, ACustomPrefix, ACustomRules: string): string;
begin
  Assert(Assigned(AView));

  Result := SetIconStyle(AView.ImageName, AImageName, ACustomPrefix, ACustomRules);
end;

procedure TKWebApplication.DownloadBytes(const ABytes: TBytes; const AFileName, AContentType: string);
begin
  if Length(ABytes) > 0 then
    DownloadStream(TBytesStream.Create(ABytes), AFileName, AContentType);
end;

procedure TKWebApplication.DownloadFile(const AFileName, AContentType: string);
begin
  if FileExists(AFileName) then
    DownloadStream(TFileStream.Create(AFileName, fmOpenRead, fmShareDenyNone), AFileName, AContentType);
end;

procedure TKWebApplication.DownloadStream(const AStream: TStream; const AFileName, AContentType: string);
begin
  { TODO : how to use AFileName as client-side file name? }
  if Assigned(AStream) then
  begin
    TKWebResponse.Current.ContentStream := AStream;
    if AContentType <> '' then
      TKWebResponse.Current.ContentType := AContentType
    else
      TKWebResponse.Current.ContentType := GetFileMimeType(AFileName);
  end;
end;

{ TKWebApplication }

function TKWebApplication.HandleRequest(const ARequest: TKWebRequest; const AResponse: TKWebResponse; const AURL: TKURL): Boolean;
var
  LSession: TJSSession;
  LHandlerObject: TObject;
begin
  Assert(Assigned(ARequest));
  Assert(Assigned(AResponse));

  Result := False;
  if StrMatchesEx(AURL.Path, FPath + '/*') then
  begin
    ActivateInstance;
    try
      try
        // Set session.
        LSession := TKWebServer.CurrentSession.Content.Objects[TKWebServer.CurrentSession.Content.IndexOf(TKWebServer.SESSION_OBJECT)] as TJSSession;
        Assert(Assigned(LSession));
        LSession.BeforeHandleRequest;
        try
          // Try to execute method.
          LHandlerObject := GetObjectFromURL(AURL);
          if (AURL.Document = '') or (AURL.Document = 'home') then
          begin
            Home;
            Result := True;
          end
          else
            Result := CallObjectMethod(LHandlerObject, AURL.Document);

          if Result then
            AResponse.Render
          else
            NotFoundError(AURL.Path + '/' + AURL.Document);
        except
          on E: Exception do
            Error(E.Message, AURL.Document, AURL.Params);
        end;
      except
        on E: Exception do
        begin
          { TODO : define and use status codes. }
          AResponse.StatusCode := 500;
          AResponse.Content := E.Message;
          AResponse.ContentType := 'text/html';
        end;
      end;
    finally
      DeactivateInstance;
    end;
  end;
end;

function TKWebApplication.GetMethodURL(const AObjectName, AMethodName: string): string;
begin
  Result := FPath + '/' + IfThen(AObjectName <> '',  AObjectName + '/', '') + AMethodName;
end;

procedure TKWebApplication.ActivateInstance;
begin
  FCurrent := Self;
  TEFMacroExpansionEngine.OnGetInstance :=
    function: TEFMacroExpansionEngine
    begin
      Result := Config.MacroExpansionEngine
    end;
end;

procedure TKWebApplication.DeactivateInstance;
begin
  FCurrent := nil;
  TEFMacroExpansionEngine.OnGetInstance := nil;
end;

procedure TKWebApplication.DelayedHome;
var
  LUserAgent: string;
begin
  if Session.IsMobileApple then
  begin
    LUserAgent := TKWebRequest.Current.UserAgent;
    if LUserAgent.Contains('iPhone') then
      Session.ViewportWidthInInches := 4
    else
      Session.ViewportWidthInInches := 8;
  end
  else
    Session.ViewportWidthInInches := Session.Global.ParamAsInteger('vpWidthInches');

  Session.ViewportWidth := Session.GetDefaultViewportWidth();
  TKWebResponse.Current.Items.ExecuteJSCode(Session.Global, 'setViewportWidth(' + IntToStr(Session.ViewportWidth) + ');');
  // Try authentication with default credentials, if any, and skip login
  // window if it succeeds.
  if Authenticate then
    DisplayHomeView
  else
    DisplayLoginView;
  Session.RefreshingLanguage := False;
end;

function TKWebApplication.Authenticate: Boolean;
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
      LUserName := Session.Global.ParamAsString('UserName');
      if LUserName <> '' then
        LAuthData.SetString('UserName', LUserName);
      LPassword := Session.Global.ParamAsString('Password');
      if LPassword <> '' then
        LAuthData.SetString('Password', LPassword);
      Result := Config.Authenticator.Authenticate(LAuthData);
    finally
      LAuthData.Free;
    end;
  end;
end;

procedure TKWebApplication.ReloadOrDisplayHomeView;
var
  LNewLanguageId: string;
begin
  LNewLanguageId := TKWebRequest.Current.QueryFields.Values['Language'];
  if (LNewLanguageId <> '') and (LNewLanguageId <> Session.Language) then
  begin
    Session.RefreshingLanguage := True;
    Session.Language := LNewLanguageId;
  end;
  DisplayHomeView;
end;

class destructor TKWebApplication.Destroy;
begin
  TKConfig.OnGetInstance := nil;
end;

procedure TKWebApplication.DisplayHomeView;
var
  LHomeView: TKView;
  LHomeController: IKExtController;
  LHomeContainer: TJSObject;
begin
  Session.HomeController.Free;
  Session.HomeController := nil;
  LHomeView := GetHomeView(Session.ViewportWidthInInches);
  Session.HomeController := TKExtControllerFactory.Instance.CreateController(Session, LHomeView, nil).AsObject;
  if Supports(Session.HomeController, IKExtController, LHomeController) then
  begin
    LHomeContainer := LHomeController.AsObject as TJSObject;
    TKWebResponse.Current.Items.ExecuteJSCode(LHomeContainer, 'var kittoHomeContainer = ' + LHomeContainer.JSName + ';');
    LHomeController.Display;
  end;
  if Session.AutoOpenViewName <> '' then
  begin
    DisplayView(Session.AutoOpenViewName);
    Session.AutoOpenViewName := '';
  end;
  if Session.HomeController is TExtContainer then
    TExtContainer(Session.HomeController).UpdateLayout;
end;

procedure TKWebApplication.DisplayLoginView;
var
  LLoginView: TKView;
  LIntf: IKExtController;
  LType: string;
begin
  Session.LoginController.Free;
  Session.LoginController := nil;
  LLoginView := TKWebApplication.Current.GetLoginView;
  if LLoginView.ControllerType = '' then
    LType := 'Login'
  else
    LType := '';
  Session.LoginController := TKExtControllerFactory.Instance.CreateController(Session, LLoginView, nil, nil, Self, LType).AsObject;
  if Supports(Session.LoginController, IKExtController, LIntf) then
    LIntf.Display;
  if Session.LoginController is TExtContainer then
    TExtContainer(Session.LoginController).UpdateLayout;
end;

procedure TKWebApplication.Home;

  procedure SetAjaxTimeout;
  var
    LTimeout: TEFNode;
  begin
    LTimeout := Config.Config.FindNode('Ext/AjaxTimeout');
    if Assigned(LTimeout) then
      TKWebResponse.Current.Items.ExecuteJSCode(Session.Global, Format('Ext.Ajax.setTimeout(%d);', [LTimeout.AsInteger]));
  end;

begin
  if TKWebRequest.Current.IsAjax then
    raise Exception.Create('Cannot call Home page in an Ajax request.');

  { TODO : doesn't work with Chrome - probably leaks }
//  if TKWebRequest.Current.IsRefresh then
  Session.Refresh;
  if not Session.RefreshingLanguage then
    Config.Authenticator.Logout;

  LoadLibraries;

  Session.HomeViewNodeName := TKWebRequest.Current.QueryFields.Values['home'];
  SetViewportContent;
  TKWebResponse.Current.Items.ExecuteJSCode(Session.Global, 'kittoInit();');
  SetAjaxTimeout;
  if Session.TooltipsEnabled then
    ExtQuickTips.Init(True)
  else
    ExtQuickTips.Disable;

  if not Session.RefreshingLanguage then
    Session.SetLanguageFromQueriesOrConfig(Config);

  Session.AutoOpenViewName := TKWebRequest.Current.QueryFields.Values['view'];
//  if FAutoOpenViewName <> '' then
//    Query['view'] := '';

  TKWebResponse.Current.Items.AjaxCallMethod(Session.Global).SetMethod(DelayedHome)
    .AddParam('vpWidthInches', Session.GetViewportWidthInInches);

  ServeHomePage;
end;

procedure TKWebApplication.ErrorMessage(const AMessage: string; const AAction: string);
begin
  TKWebResponse.Current.Items.ExecuteJSCode('Ext.Msg.show({title:"Error",msg:' + TJS.StrToJS(AMessage, True) +
    ',icon:Ext.Msg.ERROR,buttons:Ext.Msg.OK' + IfThen(AAction = '', '', ',fn:function(){' + AAction + '}') + '});');
end;

procedure TKWebApplication.Error(const AMessage, AMethodName, AParams: string);
begin
  TKWebResponse.Current.Items.Clear;
{$IFDEF DEBUG}
  ErrorMessage(AMessage + '<br/>Method: ' + IfThen(AMethodName = '', 'Home', AMethodName) + IfThen(AParams = '', '',
    '<br/>Params:<br/>' + AnsiReplaceStr(AParams, '&', '<br/>')));
{$ELSE}
  ErrorMessage(AMessage);
{$ENDIF}
  Abort;
end;

procedure TKWebApplication.NotFoundError(const AMethodName: string);
begin
  Alert(Format('Method: ''%s'' not found', [AMethodName]));
  Abort;
end;

procedure TKWebApplication.Alert(const AMessage: string);
begin
  ErrorMessage(AMessage);
end;

procedure TKWebApplication.LoadLibraries;

  procedure SetRequiredLibrary(const ALibName: string; const AIncludeCSS: Boolean = False);
  var
    LLibURL: string;
  begin
    LLibURL := Config.GetResourceURL(IncludeTrailingPathDelimiter('js') + ALibName + '.js');
    SetLibrary(StripSuffix(LLibURL, '.js'), AIncludeCSS);
  end;

  procedure SetOptionalLibrary(const ALibName: string; const AIncludeCSS: Boolean = False);
  var
    LLibURL: string;
  begin
    LLibURL := Config.FindResourceURL(IncludeTrailingPathDelimiter('js') + ALibName + '.js');
    if LLibURL <> '' then
      SetLibrary(StripSuffix(LLibURL, '.js'));
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
  LRef: TLibraryRef;
begin
{ TODO :
Find a way to reference optional libraries only if the controllers that need
them are linked in; maybe a global repository fed by initialization sections.
Duplicates must be handled/ignored. }
//  SetLibrary('{ext}/packages/ux/classic/src/statusbar/StatusBar');
//  SetCSS('{ext}/examples/ux/statusbar/css/statusbar');

//  SetLibrary('{ext}/examples/ux/fileuploadfield/FileUploadField');
//  SetCSS('{ext}/examples/ux/fileuploadfield/css/fileuploadfield');

//  SetLibrary('{ext}/examples/shared/examples'); // For Ext.msg.
//  SetCSS('{ext}/examples/shared/examples');
//  SetRequiredLibrary('DateTimeField');
//  SetRequiredLibrary('NumericField');
//  SetRequiredLibrary('DefaultButton');
  SetRequiredLibrary('kitto-core', True);
  if Session.IsMobileBrowser then
    SetRequiredLibrary('kitto-core-mobile', True)
  else
    SetRequiredLibrary('kitto-core-desktop', True);
  SetRequiredLibrary('kitto-init');
  SetOptionalLibrary('application', True);

  for LRef in FAdditionalRefs do
  begin
    if LRef.IsCSS then
      SetCSS(LRef.Path)
    else
      SetLibrary(LRef.Path);
  end;

  LLibraries := Config.Config.GetStringArray('JavaScriptLibraries');
  for LLibName in LLibraries do
    SetRequiredLibrary(LLibName);
end;

class function TKWebApplication.GetCurrent: TKWebApplication;
begin
  Result := FCurrent;
end;

function TKWebApplication.GetCustomJS: string;
begin
  Result :=
    'function setViewportWidth(w) {' + sLineBreak +
    '  var defWidth = ' + IntToStr(DEFAULT_VIEWPORT_WIDTH) + ';' + sLineBreak +
    '  var mvp = document.getElementById("viewport");' + sLineBreak +
    '  if (w != defWidth)' + sLineBreak +
    '    mvp.setAttribute("content", "' + ReplaceStr(Session.ViewportContent, '{width}', '" + w + "') + '");' + sLineBreak +
    '}';
end;

function TKWebApplication.GetViewportContent: string;
begin
  Result := ReplaceStr(Session.ViewportContent, '{width}', IntToStr(DEFAULT_VIEWPORT_WIDTH));
end;

procedure TKWebApplication.SetViewportContent;
var
  LPair: TEFPair;
  LPairs: TEFPairs;

  function GetSeparator: string;
  begin
    // IE on Windows Phone wants comma, others want space.
    if TKWebRequest.Current.UserAgent.Contains('Windows Phone') then
      Result := ', '
    else
      Result := ' ';
  end;

begin
  Session.ViewportContent := '';
  SetLength(LPairs, 2);
  LPairs[0] := TEFPair.Create('width', '{width}');
  LPairs[1] := TEFPair.Create('user-scalable', '0');
  LPairs := GetHomeView(Session.ViewportWidthInInches).GetChildrenAsPairs('MobileSettings/ViewportContent', True, LPairs);
  for LPair in LPairs do
  begin
    if Session.ViewportContent = '' then
      Session.ViewportContent := LPair.Key + '=' + LPair.Value
    else
      Session.ViewportContent := Session.ViewportContent + GetSeparator + LPair.Key + '=' + LPair.Value;
  end;
end;

procedure TKWebApplication.UpdateObserver(const ASubject: IEFSubject; const AContext: string);
begin
  inherited;
  if (ASubject.AsObject = Session.LoginController) and SameText(AContext, 'LoggedIn') then
    ReloadOrDisplayHomeView;
end;

procedure TKWebApplication.ServeHomePage;
var
  LHtml: string;
begin
  if Session.IsDownload or Session.IsUpload then
    Exit;

  LHtml := GetMainPageTemplate;
  // Replace template macros in main page code.
  LHtml := ReplaceText(LHtml, '<%HTMLDeclaration%>', '<?xml version=1.0?>' + sLineBreak +
    '<!doctype html public "-//W3C//DTD XHTML 1.0 Strict//EN">' + sLineBreak +
    '<html xmlns=http://www.w3org/1999/xthml>' + sLineBreak);
  LHtml := ReplaceText(LHtml, '<%ViewportContent%>', GetViewportContent);
  LHtml := ReplaceText(LHtml, '<%ApplicationTitle%>', Config.AppTitle);
  LHtml := ReplaceText(LHtml, '<%ApplicationIconLink%>',
    IfThen(Config.AppIcon = '', '', '<link rel="shortcut icon" href="' + Config.GetImageURL(Config.AppIcon) + '"/>'));
  LHtml := ReplaceText(LHtml, '<%AppleIconLink%>',
    IfThen(Config.AppIcon = '', '', '<link rel="apple-touch-icon" sizes="120x120" href="' + Config.GetImageURL(Config.AppIcon) + '"/>'));
  LHtml := ReplaceText(LHtml, '<%CharSet%>', TKWebResponse.Current.Items.Charset);
  LHtml := ReplaceText(LHtml, '<%ExtPath%>', ExtPath);
  LHtml := ReplaceText(LHtml, '<%DebugSuffix%>', {$IFDEF DebugExtJS}'-debug'{$ELSE}''{$ENDIF});
  LHtml := ReplaceText(LHtml, '<%ManifestLink%>', IfThen(GetManifestFileName = '', '',
    Format('<link rel="manifest" href="%s"/>', [GetManifestFileName])));
  LHtml := ReplaceText(LHtml, '<%ThemeLink%>',
    IfThen(Theme = '', '', '<link rel=stylesheet href="' + ExtPath + '/build/classic/theme-' + Theme +
      '/resources/theme-' + Theme + '-all.css" />'));
  LHtml := ReplaceText(LHtml, '<%LanguageLink%>',
    IfThen((Session.Language = 'en') or (Session.Language = ''), '',
      '<script src="' + ExtPath + '/build/classic/locale/locale-' + Session.Language + '.js"></script>'));
  LHtml := ReplaceText(LHtml, '<%LibraryTags%>', Session.Libraries);
  LHtml := ReplaceText(LHtml, '<%CustomJS%>', GetCustomJS);
  LHtml := ReplaceText(LHtml, '<%Response%>', TKWebResponse.Current.Items.Consume);

  TKWebResponse.Current.Items.Clear;
  TKWebResponse.Current.Items.AddHTML(LHtml);
end;

function TKWebApplication.GetMainPageTemplate: string;
begin
  Result := GetPageTemplate('index');
end;

function TKWebApplication.GetManifestFileName: string;
var
  LManifestFile, LURL: string;
begin
  LManifestFile := GetHomeView(Session.ViewportWidthInInches).GetString('MobileSettings/Android/Manifest', 'Manifest.json');
  LURL := Config.FindResourceURL(LManifestFile);
  if LURL <> '' then
    Result := LURL
  else
    Result := '';
end;

function TKWebApplication.FindPageTemplate(const APageName: string): string;
var
  LFileName: string;

  function GetEncoding: TEncoding;
  begin
    if TKWebResponse.Current.Items.Charset = 'utf-8' then
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

procedure TKWebApplication.Flash(const AMessage: string);
begin
  TKWebResponse.Current.Items.ExecuteJSCode('Ext.example.msg("' + _(Config.AppTitle) + '", "' + AMessage + '");');
end;

procedure TKWebApplication.Navigate(const AURL: string);
begin
  TKWebResponse.Current.Items.ExecuteJSCode(Format('window.open("%s", "_blank");', [AURL]));
end;

function TKWebApplication.GetPageTemplate(const APageName: string): string;
begin
  Result := FindPageTemplate(APageName);
  if Result = '' then
    raise Exception.CreateFmt('Template not found for page %s', [APageName]);
end;

procedure TKWebApplication.Logout;
begin
  Config.Authenticator.Logout;
  Reload;
end;

procedure TKWebApplication.Reload;
begin
  // Ajax calls are useless since we're reloading, so let's make sure
  // the response doesn't contain any.
  TKWebResponse.Current.Items.Clear;
  TKWebResponse.Current.Items.ExecuteJSCode('window.location.reload();');
end;

end.
