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

unit Kitto.Web.Server;

interface

uses
  SysUtils
  , HTTPApp
  , Generics.Collections
  , IdCustomHTTPServer
  , IdContext
  , Kitto.Web.Engine
  , Kitto.Web.Request
  , Kitto.Web.Response
  , Kitto.Web.Session
  , Kitto.Web.URL
  ;

type
  TKWebServer = class(TIdCustomHTTPServer)
  private
    FCharset: string;
    FEngine: TKWebEngine;
    procedure InitThreadScheduler(const AThreadPoolSize: Integer);
  protected
    procedure Startup; override;
    procedure Shutdown; override;
    procedure DoCommandGet(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo); override;
    procedure DoCommandOther(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo); override;

    procedure DoSessionStart(Sender: TIdHTTPSession); override;
    procedure DoSessionEnd(Sender: TIdHTTPSession); override;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
  public
    // Internal object; we might add ability to provide it externally at some point if needed.
    property Engine: TKWebEngine read FEngine;
  end;

implementation

uses
  StrUtils
  , Classes
  , IOUtils
  , IdSchedulerOfThreadPool
  , EF.Logger
  , EF.StrUtils
  , Kitto.Config
  , Kitto.Web.Types
  ;

{ TKWebServer }

procedure TKWebServer.AfterConstruction;
var
  LConfig: TKConfig;
  LThreadPoolSize: Integer;
begin
  inherited;

  AutoStartSession := True;
  SessionState := True;

  // Standard config objects are per application; we need to create our own
  // instance in order to read server-wide params.
  LConfig := TKConfig.Create;
  try
    DefaultPort := LConfig.Config.GetInteger('Server/Port', 8080);
    LThreadPoolSize := LConfig.Config.GetInteger('Server/ThreadPoolSize', 20);
    SessionTimeOut := LConfig.Config.GetInteger('Server/SessionTimeOut', 10) * MSecsPerSec * SecsPerMin;
    { TODO :  No multiple applications until we can have multiple cookie names. }
    SessionIDCookieName := LConfig.AppName;
    FCharset := LConfig.Config.GetString('Charset', 'utf-8');
  finally
    FreeAndNil(LConfig);
  end;
  InitThreadScheduler(LThreadPoolSize);

  FEngine := TKWebEngine.Create;
end;

destructor TKWebServer.Destroy;
begin
  FreeAndNil(FEngine);
  inherited;
end;

procedure TKWebServer.DoCommandGet(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  LHandled: Boolean;
  LURL: TKWebURL;

  function IsPageRefresh: Boolean;
  begin
    Result := not TKWebRequest.Current.IsAjax and ((LURL.Document = '') or (LURL.Document = 'home'));
  end;

  procedure RefreshSession;
  var
    LSession: TKWebSession;
    LSessionId: string;
  begin
    Assert(Assigned(FEngine));
    
    LSession := TKWebSession.Current;
    Assert(Assigned(LSession));

    if IsPageRefresh then
    begin
      LSessionId := LSession.SessionId;
      LSession := FEngine.RecreateSession(LSessionId); // Also sets it as current session.
      LSession.SetDefaultLanguage(TKWebRequest.Current.AcceptLanguage);
    end;
  end;

begin
  inherited;
  Assert(Assigned(FEngine));
  
  Assert(Assigned(ARequestInfo.Session));
  TKWebSession.Current := FEngine.Sessions[ARequestInfo.Session.SessionID];
  
  LURL := TKWebURL.Create(ARequestInfo.URI);
  try
    TKWebRequest.Current := TKWebRequest.Create(AContext, ARequestInfo, AResponseInfo);
    try
      TKWebSession.Current.SetDefaultLanguage(TKWebRequest.Current.AcceptLanguage);
      TKWebSession.Current.LastRequestInfo.UserAgent := TKWebRequest.Current.UserAgent;
      TKWebSession.Current.LastRequestInfo.ClientAddress := TKWebRequest.Current.RemoteAddr;
      TKWebSession.Current.LastRequestInfo.DateTime := Now;

      TKWebResponse.Current := TKWebResponse.Create(TKWebRequest.Current, AContext, ARequestInfo, AResponseInfo);
      try
        TKWebResponse.Current.Items.Charset := FCharset;
        // Switch stream ownership so that we have it still alive in AResponseInfo
        // which will destroy it later.
        TKWebResponse.Current.FreeContentStream := False;
        AResponseInfo.FreeContentStream := True;

        // When the page is refreshed, the browser keeps sending the same
        // session id, but we need to treat the event as a new session.
        RefreshSession;

        Assert(Assigned(FEngine));
        LHandled := FEngine.HandleRequest(TKWebRequest.Current, TKWebResponse.Current, LURL);

        if not LHandled then
        begin
          { TODO : use a template }
          TKWebResponse.Current.Items.Clear;
          TKWebResponse.Current.Items.AddHTML('<html><body>Unknown request</body></html>');
          TKWebResponse.Current.Render;
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

procedure TKWebServer.DoCommandOther(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
begin
  inherited;
  DoCommandGet(AContext, ARequestInfo, AResponseInfo);
end;

procedure TKWebServer.DoSessionEnd(Sender: TIdHTTPSession);
begin
  inherited;
  FEngine.RemoveSession(Sender.SessionID); // also resets the current session
end;

procedure TKWebServer.DoSessionStart(Sender: TIdHTTPSession);
begin
  Assert(Assigned(FEngine));

  TEFLogger.Instance.LogFmt('New session %s.', [Sender.SessionID], TEFLogger.LOG_MEDIUM);
  FEngine.AddNewSession(Sender.SessionID); // also sets it as current session
  inherited;
end;

procedure TKWebServer.InitThreadScheduler(const AThreadPoolSize: Integer);
var
  LScheduler: TIdSchedulerOfThreadPool;
begin
  if Assigned(Scheduler) then
  begin
    Scheduler.Free;
    Scheduler := nil;
  end;

  LScheduler := TIdSchedulerOfThreadPool.Create(Self);
  LScheduler.PoolSize := AThreadPoolSize;
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
  inherited;
end;

end.

