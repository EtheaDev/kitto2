unit Kitto.Web;

interface

uses
  SysUtils
  , Types
  , Classes
  , HTTPApp
  , Rtti
  , Generics.Collections
  , SyncObjs
  , System.Diagnostics
  , IdURI
  , IdCustomHTTPServer
  , IdContext
  , IdHTTPWebBrokerBridge
  , IdException
  , IdTCPServer
  , IdIOHandlerSocket
  , IdSchedulerOfThreadPool
  , EF.Intf
  , EF.Tree
  , EF.Macros
  , EF.ObserverIntf
  , Kitto.Config
  , Kitto.Metadata.Views
  , Kitto.Web.Request
  , Kitto.Web.Response
  , Kitto.Ext.Base
  , Ext.Base
  , Kitto.Ext.Controller
  ;

const
  DEFAULT_ENGINE_NAME = 'DefaultEngine';

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

  IKExtViewHost = interface(IEFInterface)
    ['{F073B258-1D46-4553-9FF4-3697DFE5197D}']
    procedure SetActiveView(const AIndex: Integer);
    function AsExtContainer: TExtContainer;
  end;

  TKURL = class(TIdURI)
  private
    FParsedParams: TStrings;
    procedure ParseParams;
  public
    function ParamByName(const AName: string): string;
    destructor Destroy; override;
  end;

  TKEngine = class;
  TKWebApplication = class;

  EKWebEngineException = class(Exception);

  IKWebHandleRequestEventListener = interface
    procedure BeforeHandleRequest(const ASender: TKEngine; const AApplication: TKWebApplication; var AIsAllowed: Boolean);
    procedure AfterHandleRequest(const ASender: TKEngine; const AApplication: TKWebApplication; const AStopWatch: TStopWatch);
  end;

  TKEngineBeforeHandleRequestEvent = TFunc<TKEngine, TKWebRequest, TKURL, Boolean>;

  TKWebApplicationDictionary = class(TObjectDictionary<string, TKWebApplication>);

  TKEngine = class
  private
    FApplications: TKWebApplicationDictionary;
    FSubscribers: TList<IKWebHandleRequestEventListener>;
    FCriticalSection: TCriticalSection;
    FName: string;
    FPort: Integer;
    FThreadPoolSize: Integer;
    FBasePath: string;
    FOnBeforeHandleRequest: TKEngineBeforeHandleRequestEvent;
  protected
    function DoBeforeHandleRequest(const AApplication: TKWebApplication): Boolean; virtual;
    procedure DoAfterHandleRequest(const AApplication: TKWebApplication; const AStopWatch: TStopWatch); virtual;
  public
    constructor Create(const AName: string = DEFAULT_ENGINE_NAME); virtual;
    destructor Destroy; override;

    function HandleRequest(const ARequest: TKWebRequest; const AResponse: TKWebResponse; const AURL: TKURL): Boolean;

    function AddApplication(const AName, ABasePath: string): TKWebApplication;
    procedure AddSubscriber(const ASubscriber: IKWebHandleRequestEventListener);
    procedure RemoveSubscriber(const ASubscriber: IKWebHandleRequestEventListener);

    procedure EnumerateApplications(const AProc: TProc<string, TKWebApplication>);

    property Applications: TKWebApplicationDictionary read FApplications;

    property BasePath: string read FBasePath write FBasePath;
    property Name: string read FName;
    property Port: Integer read FPort write FPort;
    property ThreadPoolSize: Integer read FThreadPoolSize write FThreadPoolSize;

    property OnBeforeHandleRequest: TKEngineBeforeHandleRequestEvent read FOnBeforeHandleRequest write FOnBeforeHandleRequest;
  end;

  EKWebApplicationException = class(Exception);
  EKWebAuthorizationException = class(EKWebApplicationException);

  TLibraryRef = record
    IsCSS: Boolean;
    Path: string;
  end;

  TKWebApplicationSessionMacroExpander = class(TEFMacroExpander)
  strict protected
    function InternalExpand(const AString: string): string; override;
  end;

  TKWebApplication = class(TEFSubjectAndObserver)
  private
    FConfig: TKConfig;
    FRttiContext: TRttiContext;
    FBasePath: string;
    FName: string;
    FEngine: TKEngine;
    FTitle: string;
    FIcon: string;
    FLoginNode: TEFNode;
    FOwnsLoginNode: Boolean;
    FTheme: string;
    FExtPath: string;
    FAdditionalRefs: TList<TLibraryRef>;
    FCharset: string;
    FSessionMacroExpander: TKWebApplicationSessionMacroExpander;
    class var FCurrent: TKWebApplication;
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
  public
    const DEFAULT_VIEWPORT_WIDTH = 480;
    class constructor Create;
    class destructor Destroy;
    constructor Create(const AEngine: TKEngine; const AName: string);
    destructor Destroy; override;

    function HandleRequest(const ARequest: TWebRequest; const AResponse: TWebResponse; const AURL: TKURL): Boolean;
    procedure UpdateObserver(const ASubject: IEFSubject; const AContext: string = ''); override;

    property Engine: TKEngine read FEngine;
    property Name: string read FName;
    property BasePath: string read FBasePath write FBasePath;
    { TODO : implement }
    property Title: string read FTitle write FTitle;
    property Icon: string read FIcon write FIcon;
    property Charset: string read FCharset;

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

    class property Current: TKWebApplication read FCurrent write FCurrent;
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

    procedure DownloadFile(const FileName: string; AContentType: string = '');
    procedure DownloadStream(const AStream: TStream; const AFileName: string; AContentType: string = '');
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
  published
    procedure HandleEvent;
    { TODO : move to application once it gains the ability to execute ajax methods }
    procedure DelayedHome;
    procedure Logout;
  end;

type
  TKWebRoute = class
  private
    FPattern: string;
    FPath: string;  public
    property Pattern: string read FPattern write FPattern;
  public
    function HandleRequest(const ARequest: TWebRequest; const AResponse: TWebResponse;
      const AURL: TKURL): Boolean; virtual; abstract;
    constructor Create(const APattern: string);
  end;

  TKStaticWebRoute = class(TKWebRoute)
  public
    function HandleRequest(const ARequest: TWebRequest; const AResponse: TWebResponse;
      const AURL: TKURL): Boolean; override;
    constructor Create(const APattern, APath: string);
  end;

  TKWebServer = class(TIdCustomHTTPServer)
  private
    FEngine: TKEngine;
    FRoutes: TObjectList<TKWebRoute>;
//    procedure ParseAuthenticationHandler(AContext: TIdContext;
//      const AAuthType, AAuthData: String; var VUsername, VPassword: String;
//      var VHandled: Boolean); virtual;
    class threadvar FCurrentSession: TIdHTTPSession;
    procedure DeleteSession(const ASession: TIdHTTPSession);
    class function GetCurrentSession: TIdHTTPSession; static;
    class procedure SetCurrentSession(const AValue: TIdHTTPSession); static;
  protected
    procedure Startup; override;
    procedure Shutdown; override;
    procedure DoCommandGet(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo); override;
    procedure DoCommandOther(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo); override;

    procedure DoSessionStart(Sender: TIdHTTPSession); override;
    procedure DoSessionEnd(Sender: TIdHTTPSession); override;

    procedure SetupThreadPooling(const APoolSize: Integer = 25);
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
  public
    const SESSION_OBJECT = 'KittoSession';
    constructor Create(AEngine: TKEngine); virtual;
    property Engine: TKEngine read FEngine;
    procedure AddRoute(const ARoute: TKWebRoute);
    class property CurrentSession: TIdHTTPSession read GetCurrentSession write SetCurrentSession;
  end;

implementation

uses
  StrUtils
  , TypInfo
  , ComObj
  , ActiveX
  , IOUtils
  , REST.Utils
  , IdGlobal
  , EF.Types
  , EF.Logger
  , EF.StrUtils
  , EF.Localization
  , Kitto.JS
  , Kitto.AccessControl
  , Kitto.Web.Types
  ;

{ TODO : temporary }
function GetObjectFromURL(const ASession: TJSSession; const AURL: TKURL): TObject;
var
  LJSName: string;
begin
  LJSName := AURL.ParamByName('Object');
  if (LJSName = '') or (AURL.ParamByName('Event') <> '') then
    Result := ASession
  else
    Result := ASession.FindChildByJSName(LJSName);
end;

{ TWebKApplication }

constructor TKWebApplication.Create(const AEngine: TKEngine; const AName: string);
begin
  Assert(Assigned(AEngine));
  Assert(AName <> '');

  inherited Create;
  FOwnsLoginNode := False;
  FName := AName;
  FEngine := AEngine;
  FRttiContext := TRttiContext.Create;
  FConfig := TKConfig.Create;
  FAdditionalRefs := TList<TLibraryRef>.Create;
  FExtPath := '/ext';
  FCharset := Config.Config.GetString('Charset', 'utf-8');
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
  Session.ResponseItems.ExecuteJSCode(Format('addStyleRule("%s", "%s");', [LSelector, LRule]));
end;

function TKWebApplication.SetViewIconStyle(const AView: TKView; const AImageName, ACustomPrefix, ACustomRules: string): string;
begin
  Assert(Assigned(AView));

  Result := SetIconStyle(AView.ImageName, AImageName, ACustomPrefix, ACustomRules);
end;

procedure TKWebApplication.DownloadFile(const FileName: string; AContentType: string);
var
  F: file;
  Buffer: AnsiString;
  Size: Longint;
begin
  if FileExists(FileName) then
  begin
    System.Assign(F, FileName);
    Reset(F, 1);
    Size := FileSize(F);
    SetLength(Buffer, Size);
    BlockRead(F, Buffer[1], Length(Buffer));
    Close(F);
{ TODO : find out the best way to download files with Indy HTTP }
//    DownloadBuffer(FileName, Size, Buffer, AContentType);
  end;
end;

procedure TKWebApplication.DownloadStream(const AStream: TStream;
  const AFileName: string; AContentType: string);
var
  LBuffer: TBytes;
  LSize: Longint;
begin
  if Assigned(AStream) then
  begin
    LSize := AStream.Size;
    SetLength(LBuffer, LSize);
    AStream.Position := 0;
    AStream.Read(LBuffer[0], Length(LBuffer));
{ TODO : find out the best way to download files with Indy HTTP }
//    DownloadBuffer(AFileName, LSize, LBuffer, AContentType);
  end;
end;

{ TKURI }

destructor TKURL.Destroy;
begin
  FreeAndNil(FParsedParams);
  inherited;
end;

function TKURL.ParamByName(const AName: string): string;
begin
  if not Assigned(FParsedParams) then
    ParseParams;
  Result := FParsedParams.Values[AName];
end;

procedure TKURL.ParseParams;
var
  I: Integer;
begin
  Assert(not Assigned(FParsedParams));

  FParsedParams := TStringList.Create;
  FParsedParams.Delimiter := '&';
  FParsedParams.StrictDelimiter := True;

  FParsedParams.DelimitedText := Params;

  for I := 0 to FParsedParams.Count - 1 do
  begin
    FParsedParams[I] := ReplaceStr(FParsedParams[I], '+', ' ');
    FParsedParams[I] := URLDecode(FParsedParams[I], IndyTextEncoding_UTF8);
  end;
end;

{ TKWebApplication }

function TKWebApplication.HandleRequest(const ARequest: TWebRequest; const AResponse: TWebResponse; const AURL: TKURL): Boolean;
type
  TMethodCall = procedure of object;
var
  LSession: TJSSession;
  LPath: string;
  LMethod: TMethod;
  LHandlerObject: TObject;
  LDocument: string;

  function CanHandleRequest: Boolean;
  begin
    Assert(Assigned(LSession));

    Result := True;//not LSession.IsUpload{ or (Pos('success:true', TKWebServer.CurrentResponse.Content) <> 0)};
  end;

begin
  Assert(Assigned(ARequest));
  Assert(Assigned(AResponse));

  ActivateInstance;
  try
    Result := False;
    try
      // Set session.
      LSession := TKWebServer.CurrentSession.Content.Objects[TKWebServer.CurrentSession.Content.IndexOf(TKWebServer.SESSION_OBJECT)] as TJSSession;
      Assert(Assigned(LSession));
  { TODO : still needed? }
  //    LSession.Application := Self;
  { TODO : refactor - move application, engine and server code out of the session }
  //    if Browser = brUnknown then
  //      DetectBrowser(RequestHeader['HTTP_USER_AGENT']);
  { TODO : handle favicon.ico }
      if CanHandleRequest and LSession.BeforeHandleRequest then
      begin
        try
          LPath := StripPrefix('/', AURL.Path);
          LDocument := AURL.Document;

          // Try to execute method.
          LHandlerObject := GetObjectFromURL(LSession, AURL);
          if not Assigned(LHandlerObject) then
            raise Exception.CreateFmt('Handler object for method %s not found in session.', [AURL.Path]);
          if (LPath = LSession.NameSpace) and (LDocument = '') then
            Home
          else
          begin
            LMethod.Code := LHandlerObject.MethodAddress(LDocument);
            if Assigned(LMethod.Code) then
            begin
              LMethod.Data := LHandlerObject;
              TMethodCall(LMethod); // Call published method
            end
            else
              LSession.OnNotFoundError(AURL.Path);
          end;
        except
          on E: Exception do
            LSession.OnError(E.Message, AURL.Path, AURL.Params);
        end;
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
  Session.ResponseItems.ExecuteJSCode(Session.Global, 'setViewportWidth(' + IntToStr(Session.ViewportWidth) + ');');
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
    Session.ResponseItems.ExecuteJSCode(LHomeContainer, 'var kittoHomeContainer = ' + LHomeContainer.JSName + ';');
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
      Session.ResponseItems.ExecuteJSCode(Session.Global, Format('Ext.Ajax.setTimeout(%d);', [LTimeout.AsInteger]));
  end;

begin
  { TODO : doesn't work with Chrome - probably leaks }
//  if TKWebRequest.Current.IsRefresh then
  Session.Refresh;
  if not Session.RefreshingLanguage then
    Config.Authenticator.Logout;

  LoadLibraries;

  Session.HomeViewNodeName := TKWebRequest.Current.QueryFields.Values['home'];
  SetViewportContent;
  Session.ResponseItems.ExecuteJSCode(Session.Global, 'kittoInit();');
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

  Session.Global.AjaxCallMethod.SetMethod(DelayedHome)
    .AddParam('vpWidthInches', Session.GetViewportWidthInInches);

  ServeHomePage;
end;

procedure TKWebApplication.HandleEvent;
var
  LObject: TJSObject;
  LEvent: string;
begin
  LEvent := TKWebRequest.Current.QueryFields.Values['Event'];
  if LEvent <> '' then
  begin
    LObject := Session.FindChildByJSName(TKWebRequest.Current.QueryFields.Values['Object']) as TJSObject;
    if not Assigned(LObject) then
      Session.OnError('Object not found in session list. It could be timed out, refresh page and try again', 'HandleEvent', '')
    else
      LObject.HandleEvent(LEvent);
  end;
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
  LMainPageCode: string;
  LResponse: string;
begin
  if Session.IsDownLoad or Session.IsUpload then
    Exit;

  LResponse := Session.ResponseItems.Consume;

  if not TKWebRequest.Current.IsAjax then
  begin
    { TODO : move to response }
    Session.ContentType := 'text/html; charset=' + Charset;
    LMainPageCode := GetMainPageTemplate;

    // Replace template macros in main page code.
    LMainPageCode := ReplaceText(LMainPageCode, '<%HTMLDeclaration%>', '<?xml version=1.0?>' + sLineBreak +
      '<!doctype html public "-//W3C//DTD XHTML 1.0 Strict//EN">' + sLineBreak +
      '<html xmlns=http://www.w3org/1999/xthml>' + sLineBreak);
    LMainPageCode := ReplaceText(LMainPageCode, '<%ViewportContent%>', GetViewportContent);
    LMainPageCode := ReplaceText(LMainPageCode, '<%ApplicationTitle%>', Title);
    LMainPageCode := ReplaceText(LMainPageCode, '<%ApplicationIconLink%>',
      IfThen(Icon = '', '', '<link rel="shortcut icon" href="' + Icon + '"/>'));
    LMainPageCode := ReplaceText(LMainPageCode, '<%AppleIconLink%>',
      IfThen(Icon = '', '', '<link rel="apple-touch-icon" sizes="120x120" href="' + Icon + '"/>'));
    LMainPageCode := ReplaceText(LMainPageCode, '<%CharSet%>', Charset);
    LMainPageCode := ReplaceText(LMainPageCode, '<%ExtPath%>', ExtPath);
    LMainPageCode := ReplaceText(LMainPageCode, '<%DebugSuffix%>',
{$IFDEF DebugExtJS}'-debug'{$ELSE}''{$ENDIF});
    LMainPageCode := ReplaceText(LMainPageCode, '<%ManifestLink%>', IfThen(GetManifestFileName = '', '',
      Format('<link rel="manifest" href="%s"/>', [GetManifestFileName])));
    LMainPageCode := ReplaceText(LMainPageCode, '<%ThemeLink%>',
      IfThen(Theme = '', '', '<link rel=stylesheet href="' + ExtPath + '/build/classic/theme-' + Theme +
      '/resources/theme-' + Theme + '-all.css" />'));
    LMainPageCode := ReplaceText(LMainPageCode, '<%LanguageLink%>',
      IfThen((Session.Language = 'en') or (Session.Language = ''), '',
        '<script src="' + ExtPath + '/build/classic/locale/locale-' + Session.Language + '.js"></script>'));
    LMainPageCode := ReplaceText(LMainPageCode, '<%LibraryTags%>', Session.Libraries);
    LMainPageCode := ReplaceText(LMainPageCode, '<%CustomJS%>', GetCustomJS);
    LMainPageCode := ReplaceText(LMainPageCode, '<%Response%>', LResponse);
    LResponse := LMainPageCode;
{$IFDEF DEBUGJS}
    LResponse := AnsiReplaceStr(LResponse, '%%', IntToStr(CountStr(sLineBreak, LResponse, 'eval('))); // eval() line number
{$ENDIF}
  end
  else
  begin
    if (LResponse <> '') and (LResponse[1] = '<') then
      Session.ContentType := 'text/html; charset=' + Charset
    else if (LResponse <> '') and CharInSet(LResponse[1], ['{', '[']) then
      Session.ContentType := 'application/json; charset=' + Charset
    else
      Session.ContentType := 'text/javascript; charset=' + Charset;
  end;
  TKWebResponse.Current.ContentType := Session.ContentType;
  TKWebResponse.Current.Content := LResponse;
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

procedure TKWebApplication.Flash(const AMessage: string);
begin
  Session.ResponseItems.ExecuteJSCode('Ext.example.msg("' + _(Config.AppTitle) + '", "' + AMessage + '");');
end;

procedure TKWebApplication.Navigate(const AURL: string);
begin
  Session.ResponseItems.ExecuteJSCode(Format('window.open("%s", "_blank");', [AURL]));
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
  Session.ResponseItems.Clear;
  Session.ResponseItems.ExecuteJSCode('window.location.reload();');
end;

{ TKWebEngine }

function TKEngine.AddApplication(const AName, ABasePath: string): TKWebApplication;
begin
  Result := TKWebApplication.Create(Self, AName);
  try
    Result.BasePath := ABasePath;

    Applications.Add(BasePath + '/' + ABasePath, Result);
  except
    Result.Free;
    raise
  end;
end;

procedure TKEngine.AddSubscriber(const ASubscriber: IKWebHandleRequestEventListener);
begin
  FSubscribers.Add(ASubscriber);
end;

constructor TKEngine.Create(const AName: string);
begin
  inherited Create;

  FName := AName;

  FApplications := TKWebApplicationDictionary.Create([doOwnsValues]);
  FCriticalSection := TCriticalSection.Create;
  FSubscribers := TList<IKWebHandleRequestEventListener>.Create;

  // default parameters
  FPort := 8080;
  FThreadPoolSize := 75;
  FBasePath := '/';
end;

destructor TKEngine.Destroy;
begin
  FCriticalSection.Free;
  FApplications.Free;
  FSubscribers.Free;
  inherited;
end;

procedure TKEngine.DoAfterHandleRequest(const AApplication: TKWebApplication; const AStopWatch: TStopWatch);
var
  LSubscriber: IKWebHandleRequestEventListener;
begin
  for LSubscriber in FSubscribers do
    LSubscriber.AfterHandleRequest(Self, AApplication, AStopWatch);
  { TODO : only do this when ADO is used }
  CoUninitialize;
end;

function TKEngine.DoBeforeHandleRequest(const AApplication: TKWebApplication): Boolean;
var
  LSubscriber: IKWebHandleRequestEventListener;
begin
  { TODO : only do this when ADO is used }
  OleCheck(CoInitialize(nil));
  Result := True;
  for LSubscriber in FSubscribers do
    LSubscriber.BeforeHandleRequest(Self, AApplication, Result);
end;

procedure TKEngine.EnumerateApplications(const AProc: TProc<string, TKWebApplication>);
var
  LPair: TPair<string, TKWebApplication>;
begin
  if Assigned(AProc) then
  begin
    FCriticalSection.Enter;
    try
      for LPair in FApplications do
        AProc(LPair.Key, LPair.Value);
    finally
      FCriticalSection.Leave;
    end;
  end;
end;

function TKEngine.HandleRequest(const ARequest: TKWebRequest; const AResponse: TKWebResponse; const AURL: TKURL): Boolean;
var
  LApplication: TKWebApplication;
  LStopWatch: TStopWatch;
begin
  Result := False;

  if Assigned(FOnBeforeHandleRequest) and not FOnBeforeHandleRequest(Self, ARequest, AURL) then
    Exit;

  { TODO : Select application based on path }
  if FApplications.Count > 0 then
  begin
    LApplication := FApplications.Values.ToArray[0];

    if DoBeforeHandleRequest(LApplication) then
    begin
      LStopWatch := TStopwatch.StartNew;
      LApplication.HandleRequest(ARequest, AResponse, AURL);
      LStopWatch.Stop;
      DoAfterHandleRequest(LApplication, LStopWatch);
    end;
    Result := True;
  end
  else
    { TODO : Status 404 }
    raise EKWebEngineException.CreateFmt('Bad request [%s]: unknown application', [AURL.URI]);
end;

procedure TKEngine.RemoveSubscriber(const ASubscriber: IKWebHandleRequestEventListener);
begin
  FSubscribers.Remove(ASubscriber);
end;

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

{ TKWebServer }

procedure TKWebServer.AddRoute(const ARoute: TKWebRoute);
begin
  FRoutes.Add(ARoute);
end;

procedure TKWebServer.AfterConstruction;
begin
  inherited;
  FRoutes := TObjectList<TKWebRoute>.Create;
end;

constructor TKWebServer.Create(AEngine: TKEngine);
begin
  inherited Create(nil);
//  OnParseAuthentication := ParseAuthenticationHandler;
  FEngine := AEngine;
end;

destructor TKWebServer.Destroy;
begin
  FreeAndNil(FRoutes);
  inherited;
end;

procedure TKWebServer.DoCommandGet(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  LHandled: Boolean;
  LRoute: TKWebRoute;
  LURL: TKURL;
begin
  inherited;

  Assert(Assigned(ARequestInfo.Session));
  CurrentSession := ARequestInfo.Session;

  LURL := TKURL.Create(ARequestInfo.URI);
  try
    TKWebRequest.Current := TKWebRequest.Create(AContext, ARequestInfo, AResponseInfo);
    try
      Session.LastRequestInfo.UserAgent := TKWebRequest.Current.UserAgent;
      Session.LastRequestInfo.ClientAddress := TKWebRequest.Current.RemoteAddr;

      TKWebResponse.Current := TKWebResponse.Create(TKWebRequest.Current, AContext, ARequestInfo, AResponseInfo);
      try
        // Switch stream ownership so that we have it still alive in AResponseInfo
        // which will destroy it later.
        TKWebResponse.Current.FreeContentStream := False;
        AResponseInfo.FreeContentStream := True;

        TEFLogger.Instance.LogStrings('DoCommand', TKWebRequest.Current.QueryFields, TEFLogger.LOG_DETAILED);

        LHandled := False;
        for LRoute in FRoutes do
        begin
          LHandled := LRoute.HandleRequest(TKWebRequest.Current, TKWebResponse.Current, LURL);
          if LHandled then
            Break;
        end;
        { TODO : make the engine a route? collapse engine and application into a route? }

        if not LHandled then
          LHandled := FEngine.HandleRequest(TKWebRequest.Current, TKWebResponse.Current, LURL);

        if not LHandled then
        begin
          { TODO : use a template }
          TKWebResponse.Current.ContentType := 'text/html';
          TKWebResponse.Current.Content := '<html><body>Unknown request</body></html>';
        end;
        AResponseInfo.CustomHeaders.AddStrings(TKWebResponse.Current.CustomHeaders);
      finally
        TKWebResponse.ClearCurrent;
      end;
    finally
      TKWebRequest.ClearCurrent;
    end;
  finally
    FreeAndNil(LURL);
  end;
end;

procedure TKWebServer.DoCommandOther(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
begin
  inherited;
  DoCommandGet(AContext, ARequestInfo, AResponseInfo);
end;

procedure TKWebServer.DoSessionEnd(Sender: TIdHTTPSession);
begin
  inherited;
  DeleteSession(Sender);
end;

procedure TKWebServer.DeleteSession(const ASession: TIdHTTPSession);
var
  LIndex: Integer;
begin
  LIndex := ASession.Content.IndexOf(SESSION_OBJECT);
  if LIndex >= 0 then
  begin
    ASession.Content.Objects[LIndex].Free;
    ASession.Content.Delete(LIndex);
  end;
end;

procedure TKWebServer.DoSessionStart(Sender: TIdHTTPSession);
var
  LSession: TJSSession;
begin
  inherited;
  TEFLogger.Instance.LogFmt('New session %s.', [Sender.SessionID], TEFLogger.LOG_MEDIUM);
  LSession := TJSSession.Create(nil);
  LSession.SessionId := Sender.SessionID;
  Sender.Content.AddObject(SESSION_OBJECT, LSession);
end;

class function TKWebServer.GetCurrentSession: TIdHTTPSession;
begin
  Result := FCurrentSession;
end;

//procedure TKWebServer.ParseAuthenticationHandler(AContext: TIdContext;
//  const AAuthType, AAuthData: String; var VUsername, VPassword: String;
//  var VHandled: Boolean);
//begin
//  if SameText(AAuthType, 'Bearer') then
//    VHandled := True;
//end;

class procedure TKWebServer.SetCurrentSession(const AValue: TIdHTTPSession);
begin
  FCurrentSession := AValue;
end;

procedure TKWebServer.SetupThreadPooling(const APoolSize: Integer);
var
  LScheduler: TIdSchedulerOfThreadPool;
begin
  if Assigned(Scheduler) then
  begin
    Scheduler.Free;
    Scheduler := nil;
  end;

  LScheduler := TIdSchedulerOfThreadPool.Create(Self);
  LScheduler.PoolSize := APoolSize;
  Scheduler := LScheduler;
  MaxConnections := LScheduler.PoolSize;
end;

procedure TKWebServer.Shutdown;
begin
  inherited;
  Bindings.Clear;
end;

procedure TKWebServer.Startup;
begin
  Bindings.Clear;
  DefaultPort := FEngine.Port;
  AutoStartSession := True;
  SessionTimeOut := 10 * MSecsPerSec * SecsPerMin; // 10 minutes.
  SessionState := True;
  { TODO : Get from application somehow - difficult since the session exists before the applications }
  SessionIDCookieName := 'kitto6';
  { TODO : increase in production }
  SetupThreadPooling(5);
  inherited;
end;

{ TKWebRoute }

constructor TKWebRoute.Create(const APattern: string);
begin
  inherited Create;
  FPattern := APattern;
end;

{ TKStaticWebRoute }

constructor TKStaticWebRoute.Create(const APattern, APath: string);
begin
  inherited Create(APattern);
  FPath := APath;
end;

function TKStaticWebRoute.HandleRequest(const ARequest: TWebRequest;
  const AResponse: TWebResponse; const AURL: TKURL): Boolean;
var
  LFileName: string;
  LPath: string;
  I: Integer;
begin
  Result := False;
  if StrMatchesEx(AURL.Path, FPattern) then
  begin
    LPath := StripPrefix(AURL.Path, '/');
    I := FirstDelimiter('/', LPath);
    if I <> 0 then
      Delete(LPath, 1, I);
    LFileName := ReplaceStr(LFileName, '/', PathDelim);
    LPath := ReplaceStr(LPath, '/', PathDelim);
    LFileName := TPath.Combine(TPath.Combine(FPath, LPath), AURL.Document);
    if FileExists(LFileName) then
    begin
      AResponse.ContentStream := TFileStream.Create(LFileName, fmOpenRead or fmShareDenyWrite);
      AResponse.ContentType := GetFileMimeType(LFileName, 'application/octet-stream');
      Result := True;
    end;
  end;
end;

end.
