{-------------------------------------------------------------------------------
   Copyright 2012-2017 Ethea S.r.l.

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

unit Kitto.Web.Engine;

interface

uses
  SysUtils
  , DateUtils
  , Classes
  , HTTPApp
  , Generics.Collections
  , EF.ObserverIntf
  , Kitto.Web.Request
  , Kitto.Web.Response
  , Kitto.Web.Routes
  , Kitto.Web.Session
  , Kitto.Web.URL
  ;

type
  /// <summary>
  ///  Periodically cleans up the list of active sessions by disposing of
  ///  the stale ones. A stale session is a session that has been inactive
  ///  for a longer time than the specified timeout.
  /// </summary>
  TKWebSessionCleanupThread = class(TThread)
  private
    FSessions: TKWebSessions;
    FInterval: Double;
    procedure WaitInterval;
  protected
    procedure Execute; override;
  public
    const DEFAULT_INTERVAL = 30 * OneSecond;
    /// <summary>
    ///  Pass the session list to clean up and an interval between
    ///  each cleanup pass.
    /// </summary>
    constructor Create(const ASessions: TKWebSessions; const AInterval: Double);
  end;

  /// <summary>
  ///  Kitto engine route. Handles sub-routes (such as the application route)
  ///  and manages a list of active sessions. Also keeps the current session
  ///  in TKWebSession updated. It is normally embedded in a TKWebServer but can
  ///  be used as-is (for example inside an ISAPI dll or Apache module).
  /// </summary>
  TKWebEngine = class(TKWebRouteList)
  private type
    TKWebEngineSessionProc = TProc<TKWebEngine, TKWebSession>;
  private
    FCharset: string;
    FSessions: TKWebSessions;
    FSessionIDCookieName: string;
    FSessionCleanupThread: TKWebSessionCleanupThread;
    FActive: Boolean;
    FSessionCleanupInterval: Double;
    FOnSessionStart: TKWebEngineSessionProc;
    FOnSessionEnd: TKWebEngineSessionProc;
    procedure EnsureSession(const AURL: TKWebURL);
    function GetSessionIdFromRequest: string;
    procedure SetSessionIdIntoResponse(const ASession: TKWebSession; const ARemove: Boolean);
    procedure SetActive(const Value: Boolean);
    procedure DoSessionStart(ASession: TKWebSession);
    procedure DoSessionEnd(ASession: TKWebSession);
  protected
    procedure BeforeHandleRequest(const ARequest: TKWebRequest; const AResponse: TKWebResponse;
      const AURL: TKWebURL; var AIsAllowed: Boolean); override;
    procedure AfterHandleRequest(const ARequest: TKWebRequest;
      const AResponse: TKWebResponse; const AURL: TKWebURL; const AIsFatalError: Boolean); override;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
  public
    property Active: Boolean read FActive write SetActive;

    property Charset: string read FCharset;
    function GetSessions: TArray<TKWebSession>;

    /// <summary>
    ///  Fired when a new session has started.
    /// </summary>
    /// <remarks>
    ///  This event is queued in the main thread's context (fired through TThread.Queue).
    /// </remarks>
    property OnSessionStart: TKWebEngineSessionProc read FOnSessionStart write FOnSessionStart;
    /// <summary>
    ///  Fired just before a session ends, either prematurely or when cleaned up due to
    ///  timeout.
    /// </summary>
    /// <remarks>
    ///  This event is called in the main thread's context (fired through TThread.Queue).
    /// </remarks>
    property OnSessionEnd: TKWebEngineSessionProc read FOnSessionEnd write FOnSessionEnd;

    /// <summary>
    ///  Manufactures all kitto objects (url, request and response) base on
    ///  the provided Webbroken request and response objects, and then calls
    ///  HandleRequest and optionally disposes of the passed objects. Useful
    ///  as a HandleRequest wrapper to be called from the Indy Kitto server
    ///  or the ISAPI/Apache implementation or elsewhere.
    /// <param AOwnsObjects>
    ///  True if ARequest and AResponse should be destroyed before returning.
    /// </param>
    /// </summary>
    function SimpleHandleRequest(const ARequest: TWebRequest; const AResponse: TWebResponse;
      const AURLDocument: string; const AOwnsObjects: Boolean = False; const AHandleAllRequests: Boolean = False): Boolean;
  end;

implementation

uses
  StrUtils
  {$IFDEF MSWINDOWS}
  , ComObj
  , ActiveX
  {$ENDIF}
  , IOUtils
  , EF.DB
  , EF.Logger
  , Kitto.Config
  , Kitto.Web.Types
  ;

{ TKWebEngine }

procedure TKWebEngine.AfterConstruction;
var
  LConfig: TKConfig;
  LSessionTimeOut: Double;
begin
  inherited;
  // Standard config objects are per application; we need to create our own
  // instance in order to read engine-wide params.
  LConfig := TKConfig.Create;
  try
    { TODO :  No multiple applications until we can have multiple cookie names. }
    FSessionIDCookieName := LConfig.AppName;
    FCharset := LConfig.Config.GetString('Charset', 'utf-8');
    LSessionTimeOut := LConfig.Config.GetInteger('Engine/Session/TimeOut', 10) * OneMinute;
    FSessionCleanupInterval := LConfig.Config.GetInteger('Engine/Session/CleanupInterval') * OneSecond;
    FSessions := TKWebSessions.Create(LSessionTimeOut);
    FSessions.OnSessionStart := DoSessionStart;
    FSessions.OnSessionEnd := DoSessionEnd;
  finally
    FreeAndNil(LConfig);
  end;
end;

destructor TKWebEngine.Destroy;
begin
  Active := False;
  FreeAndNil(FSessions);
  inherited;
end;

function TKWebEngine.GetSessionIdFromRequest: string;
begin
  Result := TKWebRequest.Current.GetCookie(FSessionIDCookieName);
end;

procedure TKWebEngine.SetActive(const Value: Boolean);
begin
  if FActive <> Value then
  begin
    FActive := Value;
    if FActive then
      FSessionCleanupThread := TKWebSessionCleanupThread.Create(FSessions, FSessionCleanupInterval)
    else
    begin
      if Assigned(FSessionCleanupThread) then
      begin
        FSessionCleanupThread.Terminate;
        FSessionCleanupThread.WaitFor;
        FreeAndNil(FSessionCleanupThread);
      end;
      FSessions.ClearSessions;
    end;
  end;
end;

procedure TKWebEngine.SetSessionIdIntoResponse(const ASession: TKWebSession; const ARemove: Boolean);
begin
  Assert(Assigned(ASession));

  if ARemove then
    TKWebResponse.Current.SetCookie(FSessionIDCookieName, ASession.SessionId, Now - 7)
  else
    TKWebResponse.Current.SetCookie(FSessionIDCookieName, ASession.SessionId, Now + ASession.Timeout);
end;

function TKWebEngine.SimpleHandleRequest(const ARequest: TWebRequest; const AResponse: TWebResponse;
  const AURLDocument: string; const AOwnsObjects: Boolean = False; const AHandleAllRequests: Boolean = False): Boolean;
var
  LURL: TKWebURL;
begin
  LURL := TKWebURL.Create(AURLDocument);
  try
    TKWebRequest.Current := TKWebRequest.Create(ARequest, AOwnsObjects);
    try
      TKWebResponse.Current := TKWebResponse.Create(AResponse, AOwnsObjects);
      try
        Result := HandleRequest(TKWebRequest.Current, TKWebResponse.Current, LURL);
        if not Result and AHandleAllRequests then
        begin
          { TODO : Fetch the appname and other data from config to display meaningful error }
          AResponse.ContentType := 'text/html';
          AResponse.Content :=
            '<html>' +
            '<head><title>Web Server Application</title></head>' +
            '<body>Unknown request: ' + ARequest.PathInfo + '</body>' +
            '</html>';
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

procedure TKWebEngine.EnsureSession(const AURL: TKWebURL);
var
  LSessionId: string;
  LSession: TKWebSession;
  LClientAddress: string;
begin
  LSessionId := GetSessionIdFromRequest;
  LClientAddress := TKWebRequest.Current.RemoteAddr;

  Assert(LClientAddress <> '');

  LSession := FSessions.FindSession(LSessionId, LClientAddress);
  if not Assigned(LSession) then
    LSession := FSessions.NewSession(LClientAddress, LSessionId)
  else if TKWebRequest.Current.IsPageRefresh(AURL.Document) then
  begin
    // Page refresh case - need to create a new session with the same
    // id (if available), so that other requests coming from the same client
    // before this one is served are linked to the correct session.
    FSessions.RemoveSession(LSession);
    LSession := FSessions.NewSession(LClientAddress, LSessionId);
  end;

  TKWebSession.Current := LSession;
end;

procedure TKWebEngine.DoSessionEnd(ASession: TKWebSession);
begin
  // Clear the current session *in this thread*, as it's a threadvar...
  if ASession = TKWebSession.Current then
    TKWebSession.Current := nil;
  // ...then queue the rest in the main thread.
  TThread.Queue(nil,
    procedure
    begin
      TEFLogger.Instance.LogFmt('Session %s terminating.', [ASession.SessionId], TEFLogger.LOG_MEDIUM);
      if Assigned(FOnSessionEnd) then
        FOnSessionEnd(Self, ASession);
    end);
end;

procedure TKWebEngine.DoSessionStart(ASession: TKWebSession);
begin
  TThread.Queue(nil,
    procedure
    begin
      TEFLogger.Instance.LogFmt('New session %s.', [ASession.SessionId], TEFLogger.LOG_MEDIUM);
      if Assigned(FOnSessionStart) then
        FOnSessionStart(Self, ASession);
    end);
end;

procedure TKWebEngine.BeforeHandleRequest(const ARequest: TKWebRequest;
  const AResponse: TKWebResponse; const AURL: TKWebURL; var AIsAllowed: Boolean);
begin
  if not FActive then
  begin
    AIsAllowed := False;
    Exit;
  end;
  EnsureSession(AURL);
  TKWebSession.Current.SetDefaultLanguage(TKWebRequest.Current.AcceptLanguage);
  TKWebSession.Current.LastRequestInfo.SetData(TKWebRequest.Current);

  TKWebResponse.Current.Items.Charset := FCharset;
  {$IFDEF MSWINDOWS}
  if EF.DB.IsCOMNeeded then
    OleCheck(CoInitialize(nil));
  {$ENDIF}
  inherited;
end;

procedure TKWebEngine.AfterHandleRequest(const ARequest: TKWebRequest;
  const AResponse: TKWebResponse; const AURL: TKWebURL; const AIsFatalError: Boolean);
begin
  inherited;
  if not FActive then
    Exit;
  {$IFDEF MSWINDOWS}
  if EF.DB.IsCOMNeeded then
    CoUninitialize;
  {$ENDIF}
  // Send back the session id to the client and update the expiration time.
  // remove the cookie in case of a fatal error.
  SetSessionIdIntoResponse(TKWebSession.Current, AIsFatalError);
  // Make sure cookies and custom headers are passed through.
  TKWebResponse.Current.Send;
  // It's only after rendering the response that we can kill the session in case
  // of fatal error.
  if AIsFatalError then
    FSessions.RemoveSession(TKWebSession.Current);
end;

function TKWebEngine.GetSessions: TArray<TKWebSession>;
begin
  Result := FSessions.GetSessions;
end;

{ TKWebSessionCleanupThread }

constructor TKWebSessionCleanupThread.Create(const ASessions: TKWebSessions; const AInterval: Double);
begin
  inherited Create;
  FSessions := ASessions;
  if AInterval <> 0 then
    FInterval := AInterval
  else
    FInterval := DEFAULT_INTERVAL;
end;

procedure TKWebSessionCleanupThread.Execute;
begin
  while not Terminated do
  begin
    if Assigned(FSessions) then
    begin
      MonitorEnter(FSessions);
      try
        FSessions.CleanupExpiredSessions;
      finally
        MonitorExit(FSessions);
      end;
    end;
    WaitInterval;
  end;
end;

procedure TKWebSessionCleanupThread.WaitInterval;
const
  STEP = 100;
var
  LMilliseconds: Int64;
begin
  LMilliseconds := MilliSecondsBetween(Now, Now + FInterval);
  while (LMilliseconds > 0) and not Terminated do
  begin
    Sleep(STEP);
    Dec(LMilliseconds, STEP);
  end;
end;

end.

