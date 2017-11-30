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
  , System.Diagnostics
  , IdCustomHTTPServer
  , IdContext
  , IdHTTPWebBrokerBridge
  , IdSchedulerOfThreadPool
  , EF.ObserverIntf
  , Kitto.Web.Request
  , Kitto.Web.Response
  , Kitto.Web.Session
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
    procedure AddedToServer(const AServer: TKWebServer; const AIndex: Integer); virtual;
  end;

  /// <summary>
  ///  Base class for routes that serve local files from the file system.
  /// </summary>
  TKBaseStaticWebRoute = class(TKWebRoute)
  protected
    function ComputeLocalFileName(const ALocalPath, AURLPath, AURLDocument: string): string;
    function ServeLocalFile(const AFileName: string; const AResponse: TKWebResponse): Boolean;
  end;

  /// <summary>
  ///  A route that serves requests for paths matching a specified pattern,
  ///  providing local files from the file system.
  /// </summary>
  TKStaticWebRoute = class(TKBaseStaticWebRoute)
  private
    FPattern: string;
    FPath: string;
    function StripBasePath(const AURLPath: string): string;
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
    function StripBasePath(const AURLPath: string): string;
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
    FSessions: TThreadList<TKWebSession>;
    FThreadPoolSize: Integer;
    FRoutes: TObjectList<TKWebRoute>;
    FSubscribers: TList<IKWebHandleRequestEventListener>;
    FCharset: string;
    class threadvar FCurrentSession: TIdHTTPSession;
    procedure DeleteSession(const ASession: TIdHTTPSession);
    class function GetCurrentSession: TIdHTTPSession; static;
    class procedure SetCurrentSession(const AValue: TIdHTTPSession); static;
    function DoBeforeHandleRequest(const ARequest: TKWebRequest; const AResponse: TKWebResponse): Boolean;
    procedure DoAfterHandleRequest(const ARequest: TKWebRequest; const AResponse: TKWebResponse; const AStopWatch: TStopWatch);
    class function GetCurrentKSession: TObject; static;
    class procedure SetCurrentKSession(const AValue: TObject); static;
    const SESSION_OBJECT = 'KittoSession';
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
    function AddRoute(const ARoute: TKWebRoute; const AIndex: Integer = -1): TKWebRoute;
    class property CurrentSession: TIdHTTPSession read GetCurrentSession write SetCurrentSession;
    class property CurrentKSession: TObject read GetCurrentKSession write SetCurrentKSession;

    function LockSessionList: TList<TKWebSession>;
    procedure UnlockSessionList;

    procedure AddSubscriber(const ASubscriber: IKWebHandleRequestEventListener);
    procedure RemoveSubscriber(const ASubscriber: IKWebHandleRequestEventListener);
  end;

implementation

uses
  StrUtils
  , Classes
  {$IFDEF MSWINDOWS}
  , ComObj
  , ActiveX
  {$ENDIF}
  , IOUtils
  , EF.Logger
  , EF.StrUtils
  , Kitto.Config
  , Kitto.Web.Types
  ;

function CreateKSession(const ASessionId: string): TKWebSession;
begin
  Result := TKWebSession.Create(ASessionId);
end;

{ TKWebServer }

function TKWebServer.AddRoute(const ARoute: TKWebRoute; const AIndex: Integer): TKWebRoute;
var
  LIndex: Integer;
begin
  if AIndex = -1 then
    LIndex := FRoutes.Add(ARoute)
  else
  begin
    FRoutes.Insert(AIndex, ARoute);
    LIndex := AIndex;
  end;
  ARoute.AddedToServer(Self, LIndex);
  Result := ARoute;
end;

procedure TKWebServer.AfterConstruction;
var
  LConfig: TKConfig;
begin
  inherited;
  FSessions := TThreadList<TKWebSession>.Create;
  FRoutes := TObjectList<TKWebRoute>.Create;
  FSubscribers := TList<IKWebHandleRequestEventListener>.Create;

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
  FreeAndNil(FSubscribers);
  FreeAndNil(FRoutes);
  FreeAndNil(FSessions);
  inherited;
end;

procedure TKWebServer.DoCommandGet(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  LHandled: Boolean;
  LRoute: TKWebRoute;
  LURL: TKURL;
  LStopWatch: TStopWatch;

  function IsPageRefresh: Boolean;
  begin
    Result := not TKWebRequest.Current.IsAjax and ((LURL.Document = '') or (LURL.Document = 'home'));
  end;

  procedure RefreshSession;
  var
    LSession: TKWebSession;
    LSessionId: string;
  begin
    LSession := TKWebSession(CurrentKSession);
    Assert(Assigned(LSession));

    if IsPageRefresh then
    begin
      LSessionId := LSession.SessionId;
      FreeAndNil(LSession);
      LSession := CreateKSession(LSessionId);
      LSession.SetDefaultLanguage(TKWebRequest.Current.AcceptLanguage);
      CurrentKSession := LSession;
    end;
  end;

begin
  inherited;

  Assert(Assigned(ARequestInfo.Session));
  CurrentSession := ARequestInfo.Session;

  LURL := TKURL.Create(ARequestInfo.URI, ARequestInfo.Params);
  try
    TKWebRequest.Current := TKWebRequest.Create(AContext, ARequestInfo, AResponseInfo);
    try
      Session.SetDefaultLanguage(TKWebRequest.Current.AcceptLanguage);
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

        LStopWatch := TStopWatch.StartNew;
        if DoBeforeHandleRequest(TKWebRequest.Current, TKWebResponse.Current) then begin
          LHandled := False;

          // Recreate Kitto session each time the page is refreshed.
          RefreshSession;

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
    if LSession is TKWebSession then
      FSessions.Remove(TKWebSession(LSession));
    LSession.Free;
  end;
end;

procedure TKWebServer.DoSessionStart(Sender: TIdHTTPSession);
var
  LSession: TKWebSession;
begin
  TEFLogger.Instance.LogFmt('New session %s.', [Sender.SessionID], TEFLogger.LOG_MEDIUM);
  LSession := CreateKSession(Sender.SessionID);
  Sender.Content.AddObject(SESSION_OBJECT, LSession);
  FSessions.Add(LSession);
  inherited;
end;

class function TKWebServer.GetCurrentKSession: TObject;
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

function TKWebServer.LockSessionList: TList<TKWebSession>;
begin
  Result := FSessions.LockList;
end;

class procedure TKWebServer.SetCurrentKSession(const AValue: TObject);
begin
  Assert(Assigned(CurrentSession));

  CurrentSession.Content.Objects[CurrentSession.Content.IndexOf(SESSION_OBJECT)] := AValue;
end;

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

procedure TKWebServer.UnlockSessionList;
begin
  FSessions.UnlockList;
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

  {$IFDEF MSWINDOWS}
  { TODO : only do this when ADO is used }
  CoUninitialize;
  {$ENDIF}
end;

function TKWebServer.DoBeforeHandleRequest(const ARequest: TKWebRequest; const AResponse: TKWebResponse): Boolean;
var
  LSubscriber: IKWebHandleRequestEventListener;
begin
  {$IFDEF MSWINDOWS}
  { TODO : only do this when ADO is used }
  OleCheck(CoInitialize(nil));
  {$ENDIF}

  Result := True;
  for LSubscriber in FSubscribers do
  begin
    LSubscriber.BeforeHandleRequest(Self, ARequest, AResponse, Result);
    if not Result then
      Break;
  end;
end;

{ TKBaseStaticWebRoute }

function TKBaseStaticWebRoute.ComputeLocalFileName(const ALocalPath, AURLPath, AURLDocument: string): string;
var
  LURLPath: string;
begin
  LURLPath := AURLPath.Replace('/', PathDelim);
  Result := TPath.Combine(TPath.Combine(ALocalPath, LURLPath), AURLDocument);
end;

function TKBaseStaticWebRoute.ServeLocalFile(const AFileName: string; const AResponse: TKWebResponse): Boolean;
begin
  if FileExists(AFileName) then
  begin
    AResponse.ContentType := GetFileMimeType(AFileName, 'application/octet-stream');
    AResponse.ReplaceContentStream(TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone));
    Result := True;
  end
  else
    Result := False;
end;

{ TKStaticWebRoute }

constructor TKStaticWebRoute.Create(const APattern, APath: string);
begin
  inherited Create;
  FPattern := APattern;
  FPath := APath;
end;

function TKStaticWebRoute.StripBasePath(const AURLPath: string): string;
var
  LBasePath: string;
begin
  // This only works reliably in the basic case of path/*, which is all we need ATM.
  LBasePath := StripJollyCharacters(FPattern);
  Result := StripPrefix(AURLPath, LBasePath);
end;

function TKStaticWebRoute.HandleRequest(const ARequest: TKWebRequest;
  const AResponse: TKWebResponse; const AURL: TKURL): Boolean;
var
  LFileName: string;
begin
  Result := False;
  if StrMatches(AURL.Path, FPattern) then
  begin
    LFileName := ComputeLocalFileName(FPath, StripBasePath(AURL.Path), AURL.Document);
    Result := ServeLocalFile(LFileName, AResponse);
  end;
end;

{ TKWebRoute }

procedure TKWebRoute.AddedToServer(const AServer: TKWebServer; const AIndex: Integer);
begin
end;

{ TKMultipleStaticWebRoute }

constructor TKMultipleStaticWebRoute.Create(const ABasePath: string; const ALocalPaths: TArray<string>);
begin
  inherited Create;
  FBasePath := ABasePath;
  FLocalPaths := RemoveDuplicates(ALocalPaths);
end;

function TKMultipleStaticWebRoute.StripBasePath(const AURLPath: string): string;
begin
  Result := StripPrefix(AURLPath, FBasePath);
end;

function TKMultipleStaticWebRoute.HandleRequest(const ARequest: TKWebRequest;
  const AResponse: TKWebResponse; const AURL: TKURL): Boolean;
var
  LLocalPath: string;
  LFileName: string;
begin
  Result := False;
  if AURL.Path.StartsWith(FBasePath) then
  begin
    for LLocalPath in FLocalPaths do
    begin
      LFileName := ComputeLocalFileName(LLocalPath, StripBasePath(AURL.Path), AURL.Document);
      Result := ServeLocalFile(LFileName, AResponse);
      if Result then
        Break;
    end;
  end;
end;

end.

