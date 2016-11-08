unit Kitto.Web;

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
  EKWebException = class(Exception);

  TKWebRoute = class(TEFSubjectAndObserver)
  public
    function HandleRequest(const ARequest: TKWebRequest; const AResponse: TKWebResponse;
      const AURL: TKURL): Boolean; virtual; abstract;
  end;

  TKStaticWebRoute = class(TKWebRoute)
  private
    FPattern: string;
    FPath: string;
  public
    property Pattern: string read FPattern write FPattern;
    function HandleRequest(const ARequest: TKWebRequest; const AResponse: TKWebResponse;
      const AURL: TKURL): Boolean; override;
    constructor Create(const APattern, APath: string);
  end;

  TKWebServer = class;

  IKWebHandleRequestEventListener = interface
    procedure BeforeHandleRequest(const ASender: TKWebServer; const ARequest: TKWebRequest; const AResponse: TKWebResponse; var AIsAllowed: Boolean);
    procedure AfterHandleRequest(const ASender: TKWebServer; const ARequest: TKWebRequest; const AResponse: TKWebResponse; const AStopWatch: TStopWatch);
  end;

  TKWebServer = class(TIdCustomHTTPServer)
  private
    FThreadPoolSize: Integer;
    FBasePath: string;
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

    property BasePath: string read FBasePath write FBasePath;
    property ThreadPoolSize: Integer read FThreadPoolSize write FThreadPoolSize;

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
  , Kitto.JS
  , Kitto.Web.Types
  ;

{ TKWebServer }

function TKWebServer.AddRoute(const ARoute: TKWebRoute): TKWebRoute;
begin
  FRoutes.Add(ARoute);
  Result := ARoute;
end;

procedure TKWebServer.AfterConstruction;
begin
  inherited;
//  OnParseAuthentication := ParseAuthenticationHandler;
  FSubscribers := TList<IKWebHandleRequestEventListener>.Create;
  FRoutes := TObjectList<TKWebRoute>.Create;
  // default parameters
  DefaultPort := 8080;
  { TODO : increase in production }
  FThreadPoolSize := 5;
  FBasePath := '/';
  AutoStartSession := True;
  SessionTimeOut := 10 * MSecsPerSec * SecsPerMin; // 10 minutes.
  SessionState := True;
  { TODO : Get from application somehow - difficult since the session exists before the applications }
  SessionIDCookieName := 'kitto6';
  { TODO : parameterize }
  FCharset := 'utf-8';
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

  LURL := TKURL.Create(ARequestInfo.URI);
  try
    TKWebRequest.Current := TKWebRequest.Create(AContext, ARequestInfo, AResponseInfo);
    try
      Session.LastRequestInfo.UserAgent := TKWebRequest.Current.UserAgent;
      Session.LastRequestInfo.ClientAddress := TKWebRequest.Current.RemoteAddr;

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

          { TODO : handle favicon.ico }
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
begin
  FSubscribers.Add(ASubscriber);
end;

procedure TKWebServer.RemoveSubscriber(const ASubscriber: IKWebHandleRequestEventListener);
begin
  FSubscribers.Remove(ASubscriber);
end;

procedure TKWebServer.DoAfterHandleRequest(const ARequest: TKWebRequest; const AResponse: TKWebResponse; const AStopWatch: TStopWatch);
var
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

{ TKStaticWebRoute }

constructor TKStaticWebRoute.Create(const APattern, APath: string);
begin
  inherited Create;
  FPattern := APattern;
  FPath := APath;
end;

function TKStaticWebRoute.HandleRequest(const ARequest: TKWebRequest;
  const AResponse: TKWebResponse; const AURL: TKURL): Boolean;
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
