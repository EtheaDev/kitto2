unit Kitto.Web.Server;

interface

uses
  SysUtils
  , HTTPApp
  , Generics.Collections
  , System.Diagnostics
  , IdCustomHTTPServer
  , IdContext
  , IdHTTPWebBrokerBridge
  , IdSchedulerOfThreadPool
  , EF.ObserverIntf
  , Kitto.Web.Request
  , Kitto.Web.Response
  , Kitto.Web.URL
  ;

type
  TKWebServer = class;

  /// <summary>
  ///  Abstract route. Handles requests for a class of paths.
  /// </summary>
  TKWebRoute = class(TEFSubjectAndObserver)
  public
    /// <summary>
    ///  Handles a request; if handled, returns True (and sets the reponse up) if handled.
    /// </summary>
    function HandleRequest(const ARequest: TKWebRequest; const AResponse: TKWebResponse;
      const AURL: TKURL): Boolean; virtual; abstract;
    /// <summary>
    ///  Called when the route is added to the server. May be used to add dependant routes.
    ///  The predefined implementation does nothing.
    /// </summary>
    procedure AddedToServer(const AServer: TKWebServer); virtual;
  end;

  /// <summary>
  ///  A route that serves requests for paths matching a specified pattern,
  ///  providing local files from the file system.
  /// </summary>
  TKBaseStaticWebRoute = class(TKWebRoute)
  protected
    function ComputeLocalFileName(const ALocalPath, AURLPath, AURLDocument: string): string;
    function ServeLocalFile(const AFileName: string; const AResponse: TKWebResponse): Boolean;
  end;

  TKStaticWebRoute = class(TKBaseStaticWebRoute)
  private
    FPattern: string;
    FPath: string;
  public
    constructor Create(const APattern, APath: string);
    function HandleRequest(const ARequest: TKWebRequest; const AResponse: TKWebResponse;
      const AURL: TKURL): Boolean; override;
  end;

  /// <summary>
  ///  A route that serves requests for paths matching a specified pattern,
  ///  looking through an ordered list of local paths and serving the first file
  ///  it finds.
  /// </summary>
  TKMultipleStaticWebRoute = class(TKBaseStaticWebRoute)
  private
    FBasePath: string;
    FLocalPaths: TArray<string>;
  public
    function HandleRequest(const ARequest: TKWebRequest;
      const AResponse: TKWebResponse; const AURL: TKURL): Boolean; override;
    constructor Create(const ABasePath: string; const ALocalPaths: TArray<string>);
  end;

  IKWebHandleRequestEventListener = interface
    procedure BeforeHandleRequest(const ASender: TKWebServer; const ARequest: TKWebRequest; const AResponse: TKWebResponse; var AIsAllowed: Boolean);
    procedure AfterHandleRequest(const ASender: TKWebServer; const ARequest: TKWebRequest; const AResponse: TKWebResponse; const AStopWatch: TStopWatch);
  end;

  TKWebServer = class(TIdCustomHTTPServer)
  private
    FThreadPoolSize: Integer;
    FRoutes: TObjectList<TKWebRoute>;
    FSubscribers: TList<IKWebHandleRequestEventListener>;
    FCharset: string;
//    procedure ParseAuthenticationHandler(AContext: TIdContext;
//      const AAuthType, AAuthData: String; var VUsername, VPassword: String;
//      var VHandled: Boolean); virtual;
    class threadvar FCurrentSession: TIdHTTPSession;
    procedure DeleteSession(const ASession: TIdHTTPSession);
    class function GetCurrentSession: TIdHTTPSession; static;
    class procedure SetCurrentSession(const AValue: TIdHTTPSession); static;
    function DoBeforeHandleRequest(const ARequest: TKWebRequest; const AResponse: TKWebResponse): Boolean;
    procedure DoAfterHandleRequest(const ARequest: TKWebRequest; const AResponse: TKWebResponse; const AStopWatch: TStopWatch);
    class function GetCurrentKittoSession: TObject; static;
  protected
    procedure Startup; override;
    procedure Shutdown; override;
    procedure DoCommandGet(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo); override;
    procedure DoCommandOther(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo); override;

    procedure DoSessionStart(Sender: TIdHTTPSession); override;
    procedure DoSessionEnd(Sender: TIdHTTPSession); override;

    procedure SetupThreadPooling;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
  public
    const SESSION_OBJECT = 'KittoSession';
    function AddRoute(const ARoute: TKWebRoute): TKWebRoute;
    class property CurrentSession: TIdHTTPSession read GetCurrentSession write SetCurrentSession;
    class property CurrentKittoSession: TObject read GetCurrentKittoSession;

    procedure AddSubscriber(const ASubscriber: IKWebHandleRequestEventListener);
    procedure RemoveSubscriber(const ASubscriber: IKWebHandleRequestEventListener);
  end;

implementation

uses
  StrUtils
  , Classes
  , ComObj
  , ActiveX
  , IOUtils
  , EF.Logger
  , EF.StrUtils
  , Kitto.Config
  , Kitto.JS
  , Kitto.Web.Types
  ;

{ TKWebServer }

function TKWebServer.AddRoute(const ARoute: TKWebRoute): TKWebRoute;
begin
  FRoutes.Add(ARoute);
  ARoute.AddedToServer(Self);
  Result := ARoute;
end;

procedure TKWebServer.AfterConstruction;
var
  LConfig: TKConfig;
begin
  inherited;
//  OnParseAuthentication := ParseAuthenticationHandler;
  FSubscribers := TList<IKWebHandleRequestEventListener>.Create;
  FRoutes := TObjectList<TKWebRoute>.Create;

  AutoStartSession := True;
  SessionState := True;

  // Standard config objects are per application; we need to create our own
  // instance in order to read server-wide params.
  LConfig := TKConfig.Create;
  try
    DefaultPort := LConfig.Config.GetInteger('Server/Port', 8080);
    FThreadPoolSize := LConfig.Config.GetInteger('Server/ThreadPoolSize', 20);
    SessionTimeOut := LConfig.Config.GetInteger('Server/SessionTimeOut', 10) * MSecsPerSec * SecsPerMin;
    { TODO :  No multiple applications until we can have multiple cookie names. }
    SessionIDCookieName := LConfig.AppName;
    FCharset := LConfig.Config.GetString('Charset', 'utf-8');
  finally
    FreeAndNil(LConfig);
  end;
end;

destructor TKWebServer.Destroy;
begin
  FreeAndNil(FRoutes);
  FreeAndNil(FSubscribers);
  inherited;
end;

procedure TKWebServer.DoCommandGet(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  LHandled: Boolean;
  LRoute: TKWebRoute;
  LURL: TKURL;
  LStopWatch: TStopWatch;
begin
  inherited;

  Assert(Assigned(ARequestInfo.Session));
  CurrentSession := ARequestInfo.Session;

  LURL := TKURL.Create(ARequestInfo.URI, ARequestInfo.Params);
  try
    TKWebRequest.Current := TKWebRequest.Create(AContext, ARequestInfo, AResponseInfo);
    try
      Session.LastRequestInfo.UserAgent := TKWebRequest.Current.UserAgent;
      Session.LastRequestInfo.ClientAddress := TKWebRequest.Current.RemoteAddr;
      Session.LastRequestInfo.DateTime := Now;

      TKWebResponse.Current := TKWebResponse.Create(TKWebRequest.Current, AContext, ARequestInfo, AResponseInfo);
      try
        TKWebResponse.Current.Items.Charset := FCharset;
        // Switch stream ownership so that we have it still alive in AResponseInfo
        // which will destroy it later.
        TKWebResponse.Current.FreeContentStream := False;
        AResponseInfo.FreeContentStream := True;

        TEFLogger.Instance.LogStrings('DoCommand', TKWebRequest.Current.QueryFields, TEFLogger.LOG_DETAILED);

        LStopWatch := TStopWatch.StartNew;
        if DoBeforeHandleRequest(TKWebRequest.Current, TKWebResponse.Current) then begin
          LHandled := False;

          for LRoute in FRoutes do
          begin
            LHandled := LRoute.HandleRequest(TKWebRequest.Current, TKWebResponse.Current, LURL);
            if LHandled then
              Break;
          end;

          if not LHandled then
          begin
            { TODO : use a template }
            TKWebResponse.Current.Items.Clear;
            TKWebResponse.Current.Items.AddHTML('<html><body>Unknown request</body></html>');
            TKWebResponse.Current.Render;
          end;
          DoAfterHandleRequest(TKWebRequest.Current, TKWebResponse.Current, LStopWatch);
          AResponseInfo.CustomHeaders.AddStrings(TKWebResponse.Current.CustomHeaders);
        end;
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
  LSession: TObject;
begin
  LIndex := ASession.Content.IndexOf(SESSION_OBJECT);
  if LIndex >= 0 then
  begin
    LSession := ASession.Content.Objects[LIndex];
    ASession.Content.Delete(LIndex);
    LSession.Free;
  end;
end;

procedure TKWebServer.DoSessionStart(Sender: TIdHTTPSession);
var
  LSession: TJSSession;
begin
  TEFLogger.Instance.LogFmt('New session %s.', [Sender.SessionID], TEFLogger.LOG_MEDIUM);
  LSession := TJSSession.Create(Sender.SessionID);
  Sender.Content.AddObject(SESSION_OBJECT, LSession);
  inherited;
end;

class function TKWebServer.GetCurrentKittoSession: TObject;
begin
  if Assigned(CurrentSession) then
    Result := CurrentSession.Content.Objects[
      CurrentSession.Content.IndexOf(SESSION_OBJECT)]
  else
    Result := nil;
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

procedure TKWebServer.SetupThreadPooling;
var
  LScheduler: TIdSchedulerOfThreadPool;
begin
  if Assigned(Scheduler) then
  begin
    Scheduler.Free;
    Scheduler := nil;
  end;

  LScheduler := TIdSchedulerOfThreadPool.Create(Self);
  LScheduler.PoolSize := FThreadPoolSize;
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
  SetupThreadPooling;
  inherited;
end;

procedure TKWebServer.AddSubscriber(const ASubscriber: IKWebHandleRequestEventListener);

  FSubscribers.Add(ASubscriber);
end;

procedure TKWebServer.RemoveSubscriber(const ASubscriber: IKWebHandleRequestEventListener);

  FSubscribers.Remove(ASubscriber);
end;

procedure TKWebServer.DoAfterHandleRequest(const ARequest: TKWebRequest; const AResponse: TKWebResponse; const AStopWatch: TStopWatch);

  LSubscriber: IKWebHandleRequestEventListener;
begin
  for LSubscriber in FSubscribers do
    LSubscriber.AfterHandleRequest(Self, ARequest, AResponse, AStopWatch);

  { TODO : only do this when ADO is used }
  CoUninitialize;
end;

function TKWebServer.DoBeforeHandleRequest(const ARequest: TKWebRequest; const AResponse: TKWebResponse): Boolean;
var
  LSubscriber: IKWebHandleRequestEventListener;
begin
  { TODO : only do this when ADO is used }
  OleCheck(CoInitialize(nil));

  Result := True;
  for LSubscriber in FSubscribers do
  begin
    LSubscriber.BeforeHandleRequest(Self, ARequest, AResponse, Result);
    if not Result then
      Break;
  end;
end;



function TKBaseStaticWebRoute.ComputeLocalFileName(const ALocalPath, AURLPath, AURLDocument: string): string;
var
  LURLPath: string;
  I: Integer;
begin
  LURLPath := StripPrefix(AURLPath, '/');
  I := FirstDelimiter('/', LURLPath);
  if I <> 0 then
    Delete(LURLPath, 1, I);
  LURLPath := ReplaceStr(LURLPath, '/', PathDelim);
  Result := TPath.Combine(TPath.Combine(ALocalPath, LURLPath), AURLDocument);
end;

function TKBaseStaticWebRoute.ServeLocalFile(const AFileName: string; const AResponse: TKWebResponse): Boolean;
begin
  if FileExists(AFileName) then
  begin
    AResponse.ContentStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
    AResponse.ContentType := GetFileMimeType(AFileName, 'application/octet-stream');
    Result := True;
  end
  else
    Result := False;
end;

{ TKStaticWebRoute }

constructor TKStaticWebRoute.Create(const APattern, APath: string);

  inherited Create;
  FPattern := APattern;
  FPath := APath;
end;

function TKStaticWebRoute.HandleRequest(const ARequest: TKWebRequest;

var
  LFileName: string;
begin
  Result := False;
  if StrMatchesEx(AURL.Path, FPattern) then
  begin
    LFileName := ComputeLocalFileName(FPath, AURL.Path, AURL.Document);
    Result := ServeLocalFile(LFileName, AResponse);
  end;
end;

{ TKWebRoute }

procedure TKWebRoute.AddedToServer(const AServer: TKWebServer);

end;

{ TKMultipleStaticWebRoute }

constructor TKMultipleStaticWebRoute.Create(const ABasePath: string;

begin
  inherited Create;
  FBasePath := ABasePath;
  FLocalPaths := RemoveDuplicates(ALocalPaths);
end;

function TKMultipleStaticWebRoute.HandleRequest(const ARequest: TKWebRequest;
  const AResponse: TKWebResponse; const AURL: TKURL): Boolean;
var
  LLocalPath: string;
  LFileName: string;
begin
  Result := False;
  for LLocalPath in FLocalPaths do
  begin
    LFileName := ComputeLocalFileName(LLocalPath, AURL.Path, AURL.Document);
    Result := ServeLocalFile(LFileName, AResponse);
    if Result then
      Break;
  end;
end;

end.
