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
    FInterval: Integer;
    procedure WaitInterval;
  protected
    procedure Execute; override;
  public
    const DEFAULT_INTERVAL = 30;
    /// <summary>
    ///  Pass the session list to clean up and an interval in seconds between
    ///  each cleanup pass.
    /// </summary>
    constructor Create(const ASessions: TKWebSessions; const AInterval: Integer);
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
    FSessionCleanupInterval: Integer;
    FOnSessionStart: TKWebEngineSessionProc;
    FOnSessionEnd: TKWebEngineSessionProc;
    procedure EnsureSession(const AURL: TKWebURL);
    function GetSessionIdFromRequest: string;
    procedure SetSessionIdIntoResponse(const ASessionId: string);
    procedure SetActive(const Value: Boolean);
    procedure DoSessionStart(ASession: TKWebSession);
    procedure DoSessionEnd(ASession: TKWebSession);
  protected
    procedure BeforeHandleRequest(const ARequest: TKWebRequest; const AResponse: TKWebResponse;
      const AURL: TKWebURL; var AIsAllowed: Boolean); override;
    procedure AfterHandleRequest(const ARequest: TKWebRequest;
      const AResponse: TKWebResponse; const AURL: TKWebURL); override;
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
    procedure SimpleHandleRequest(const ARequest: TWebRequest; const AResponse: TWebResponse;
      const ADocument: string; const AOwnsObjects: Boolean);
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
  LSessionTimeOut: Integer;
begin
  inherited;
  // Standard config objects are per application; we need to create our own
  // instance in order to read engine-wide params.
  LConfig := TKConfig.Create;
  try
    { TODO :  No multiple applications until we can have multiple cookie names. }
    FSessionIDCookieName := LConfig.AppName;
    FCharset := LConfig.Config.GetString('Charset', 'utf-8');
    LSessionTimeOut := LConfig.Config.GetInteger('Session/TimeOut', 10) * MSecsPerSec * SecsPerMin; // in minutes
    FSessionCleanupInterval := LConfig.Config.GetInteger('Session/CleanupInterval', TKWebSessionCleanupThread.DEFAULT_INTERVAL) * MSecsPerSec; // in seconds
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
      FSessions.Clear;
    end;
  end;
end;

procedure TKWebEngine.SetSessionIdIntoResponse(const ASessionId: string);
begin
  TKWebResponse.Current.SetCookie(FSessionIDCookieName, ASessionId);
end;

procedure TKWebEngine.SimpleHandleRequest(const ARequest: TWebRequest;
  const AResponse: TWebResponse; const ADocument: string; const AOwnsObjects: Boolean);
var
  LURL: TKWebURL;
begin
  LURL := TKWebURL.Create(ADocument);
  try
    TKWebRequest.Current := TKWebRequest.Create(ARequest, AOwnsObjects);
    try
      TKWebResponse.Current := TKWebResponse.Create(AResponse, AOwnsObjects);
      try
        if not HandleRequest(TKWebRequest.Current, TKWebResponse.Current, LURL) then
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
  LRequestSessionId: string;
  LSession: TKWebSession;
begin
  LRequestSessionId := GetSessionIdFromRequest;

  MonitorEnter(FSessions);
  try
    // We must create a new session on page refresh, as everything on the client side is gone.
    if TKWebRequest.Current.IsPageRefresh(AURL.Document) then
    begin
      if (LRequestSessionId <> '') and FSessions.SessionExists(LRequestSessionId) then
        FSessions.DeleteSession(LRequestSessionId);
      LSession := FSessions.CreateSession;
    end
    else if (LRequestSessionId <> '') and FSessions.SessionExists(LRequestSessionId) then
      // Session is active, use it.
      LSession := FSessions[LRequestSessionId]
    else
      // First request, start new session.
      LSession := FSessions.CreateSession;
  finally
    MonitorExit(FSessions);
  end;
  TKWebSession.Current := LSession;
  LSession.SetDefaultLanguage(TKWebRequest.Current.AcceptLanguage);
  LSession.LastRequestInfo.SetData(TKWebRequest.Current);
  SetSessionIdIntoResponse(LSession.SessionId);
end;

procedure TKWebEngine.DoSessionEnd(ASession: TKWebSession);
begin
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
  TKWebResponse.Current.Items.Charset := FCharset;
  {$IFDEF MSWINDOWS}
  if EF.DB.IsCOMNeeded then
    OleCheck(CoInitialize(nil));
  {$ENDIF}
  inherited;
end;

procedure TKWebEngine.AfterHandleRequest(const ARequest: TKWebRequest;
  const AResponse: TKWebResponse; const AURL: TKWebURL);
begin
  inherited;
  if not FActive then
    Exit;
  {$IFDEF MSWINDOWS}
  if EF.DB.IsCOMNeeded then
    CoUninitialize;
  {$ENDIF}
end;

function TKWebEngine.GetSessions: TArray<TKWebSession>;
begin
  MonitorEnter(FSessions);
  try
    Result := FSessions.Values.ToArray;
  finally
    MonitorExit(FSessions);
  end;
end;

{ TKWebSessionCleanupThread }

constructor TKWebSessionCleanupThread.Create(const ASessions: TKWebSessions; const AInterval: Integer);
begin
  inherited Create;
  FSessions := ASessions;
  FInterval := AInterval;
end;

procedure TKWebSessionCleanupThread.Execute;
begin
  while not Terminated do
  begin
    if Assigned(FSessions) then
    begin
      MonitorEnter(FSessions);
      try
        //FSessions.Cleanup;
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
  I: Integer;
begin
  I := FInterval;
  while (I > 0) and not Terminated do
  begin
    Sleep(STEP);
    Dec(I, STEP);
  end;
end;

end.

