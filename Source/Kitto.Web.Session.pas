{ -------------------------------------------------------------------------------
  Copyright 2012-2018 Ethea S.r.l.

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
  ------------------------------------------------------------------------------- }

{ -------------------------------------------------------------------------------
  Loosely based on code from ExtPascal
  Author: Wanderlan Santos dos Anjos. wanderlan.anjos@gmail.com
  Home: http://extpascal.googlecode.com
  License: BSD, http://www.opensource.org/licenses/bsd-license.php
  ------------------------------------------------------------------------------- }
unit Kitto.Web.Session;

interface

uses
  SysUtils
  , DateUtils
  , Classes
  , Generics.Collections
  , gnugettext
  , EF.Intf
  , EF.Tree
  , EF.Localization
  , EF.ObserverIntf
  , Kitto.JS.Base
  , Kitto.JS
  , Kitto.Config
  , Kitto.Metadata.Views
  , Kitto.Web.Request
  ;

type
  /// <summary>
  ///  Represents the server side of a user client session.
  ///  Holds all objects and data pertaining to the user session.
  /// </summary>
  TKWebSession = class(TEFSubjectAndObserver)
  private
    FSessionId: string;
    FLanguage: string;
    FRefreshingLanguage: Boolean;
    FViewportWidthInInches: Integer;
    FAutoOpenViewName: string;
    FAuthData: TEFNode;
    FIsAuthenticated: Boolean;
    FOpenControllers: TList<IJSController>;
    FHomeController: IJSController;
    FLoginController: IJSController;
    FControllerHostWindow: IJSContainer;
    FControllerContainer: IJSControllerContainer;
    FStatusHost: IJSStatusHost;
    FHomeViewNodeName: string;
    FViewportContent: string;
    FViewportWidth: Integer;
    FGettextInstance: TGnuGettextInstance;
    FDynamicScripts: TStringList;
    FDynamicStyles: TStringList;
    FDisplayName: string;
    FLastRequestInfo: TKWebRequestInfo;
    FCreationDateTime: TDateTime;
    FObjectSpace: TJSObjectSpace;
    FTimeout: Double;
    class threadvar FCurrent: TKWebSession;
    function GetDisplayName: string;
    procedure SetLanguage(const AValue: string);
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
    function GetObjectSpace: TJSObjectSpace;
    class function GetCurrent: TKWebSession; static;
    class procedure SetCurrent(const Value: TKWebSession); static;
  strict protected
    function GetViewportContent: string; virtual;
    function GetManifestFileName: string; virtual;
  public
    procedure SetLanguageFromQueriesOrConfig(const AConfig: TKConfig);
    property RefreshingLanguage: Boolean read FRefreshingLanguage write FRefreshingLanguage;

    function GetDefaultViewportWidth: Integer;
  public
    constructor Create(const AClientAddress, ASessionId: string; const ATimeout: Double);
    procedure AfterConstruction; override;
    destructor Destroy; override;

    property CreationDateTime: TDateTime read FCreationDateTime;
    /// <summary>
    ///  The current session's UUID.
    /// </summary>
    property SessionId: string read FSessionId;
    property Timeout: Double read FTimeout;
    property Language: string read FLanguage write SetLanguage;

    property ObjectSpace: TJSObjectSpace read GetObjectSpace;

    /// <summary>
    ///  Gives access to a copy of the auth data that was last passed
    ///  to Authenticate (and possibly modified by the object during
    ///  authentication).
    /// </summary>
    property AuthData: TEFNode read FAuthData;
    /// <summary>
    ///  Returns True if authentication has successfully taken
    ///  place.
    /// </summary>
    property IsAuthenticated: Boolean read FIsAuthenticated write FIsAuthenticated;

    /// <summary>
    ///  A reference to the main container of controllers.
    /// </summary>
    property ControllerContainer: IJSControllerContainer read FControllerContainer write FControllerContainer;
    /// <summary>
    ///  A reference to the status bar to be used for wait messages.
    ///  It is of type TKExtStatusBar.
    /// </summary>
    property StatusHost: IJSStatusHost read FStatusHost write FStatusHost;
    property ControllerHostWindow: IJSContainer read FControllerHostWindow write FControllerHostWindow;
    property OpenControllers: TList<IJSController> read FOpenControllers;
    property HomeController: IJSController read FHomeController write FHomeController;
    property LoginController: IJSController read FLoginController write FLoginController;
    property ViewportWidthInInches: Integer read FViewportWidthInInches write FViewportWidthInInches;
    property AutoOpenViewName: string read FAutoOpenViewName write FAutoOpenViewName;
    property HomeViewNodeName: string read FHomeViewNodeName write FHomeViewNodeName;
    property ViewportContent: string read FViewportContent write FViewportContent;
    /// <summary>
    ///  Viewport width in mobile applications.
    /// </summary>
    property ViewportWidth: Integer read FViewportWidth write FViewportWidth;
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
    ///  If the specified object is found in the list of open controllers,
    ///  it is removed from the list. Otherwise nothing happens.
    ///  Used by view hosts to notify the session that a controller was closed.
    /// </summary>
    procedure RemoveController(const AController: IJSController);
    property DisplayName: string read GetDisplayName write FDisplayName;

    property LastRequestInfo: TKWebRequestInfo read FLastRequestInfo;
    procedure SetDefaultLanguage(const AValue: string);

    /// <summary>
    ///  True if the session has expired, based on the value of Timeout and
    ///  the current time.
    /// </summary>
    function HasExpired: Boolean;

    /// <summary>
    ///  Globally accessible reference to the current thread's active session.
    /// </summary>
    class property Current: TKWebSession read GetCurrent write SetCurrent;
  end;

  /// <summary>
  ///  This class serves two purposes: redirects localization calls to a
  ///  per-session instance of dxgettext so we can have per-session language
  ///  selection, and configures Kitto's localization scheme based on two text
  ///  domains (the application's default.mo and Kitto's own Kitto.mo). The
  ///  former is located under the application home directory, the latter
  ///  under the system home directory.
  /// </summary>
  TKWebSessionLocalizationTool = class(TEFNoRefCountObject, IInterface, IEFInterface, IEFLocalizationTool)
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

  TKWebSessions = class
  private type
    TKWebSessionProc = TProc<TKWebSession>;
  private
    FSessions: TObjectList<TKWebSession>;
    FTimeout: Double;
    FOnSessionEnd: TKWebSessionProc;
    FOnSessionStart: TKWebSessionProc;
    function CreateNewSessionId: string;
    function FindSessionById(const ASessionId: string): TKWebSession;
    function FindSessionByClientAddress(const AClientAddress: string): TKWebSession;
  protected
    procedure SessionAdded(const ASession: TKWebSession);
    procedure SessionRemoved(const ASession: TKWebSession);
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
  public
    constructor Create(const ATimeout: Double);

    /// <summary>
    ///  Creates a new sessions and adds it to the list.
    /// </summary>
    /// <param AClientAddress>
    ///  The IP address of the client to bind to the session. Required.
    /// </param>
    /// <param ASessionId>
    ///  The new session will have the specified id, if specified. Otherwise
    ///  a new id is generated.
    /// </param>
    function NewSession(const AClientAddress: string; const ASessionId: string = ''): TKWebSession;

    /// <summary>
    ///  If ASessionId is provided, looks for a session with that id returns it, otherwise returns nil.
    ///  If ASessionId is not provided, looks for a session with the specified client address and
    ///  returns it, otherwise returns nil. In this case, if no client address is specified, returns nil.
    /// </summary>
    /// <param ASessionId>
    ///  The session id to look for (can be empty).
    /// </param>
    /// <param AClientAddress>
    ///  The client address to look for if no session id is specified.
    /// </param>
    function FindSession(const ASessionId, AClientAddress: string): TKWebSession;

    /// <summary>
    ///  Returns all sessions as an array for reporting and diagnostic purposes.
    /// </summary>
    /// <remarks>
    ///  The list can change at any time after this method returns.
    /// </remarks>
    function GetSessions: TArray<TKWebSession>;

    /// <summary>
    ///  Deletes and frees the specified session.
    /// </summary>
    procedure RemoveSession(const ASession: TKWebSession);

    procedure ClearSessions;

    procedure CleanupExpiredSessions;

    /// <summary>
    ///  Fired when a new session has started.
    /// </summary>
    /// <remarks>
    ///  This event is handled by the Kitto Engine. Applications should use
    ///  the equivalent engine event.
    /// </remarks>
    property OnSessionStart: TKWebSessionProc read FOnSessionStart write FOnSessionStart;
    /// <summary>
    ///  Fired just before a session ends, either prematurely or when cleaned up due to
    ///  timeout.
    /// </summary>
    /// <remarks>
    ///  This event is handled by the Kitto Engine. Applications should use
    ///  the equivalent engine event.
    /// </remarks>
    property OnSessionEnd: TKWebSessionProc read FOnSessionEnd write FOnSessionEnd;
  end;

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

implementation

uses
  EF.StrUtils
  , EF.Logger
  , Kitto.Web.Response
  , Kitto.Web.Application
  ;

{ TKWebSession }

function TKWebSession.GetViewportContent: string;
begin
  Result := '';
end;

function TKWebSession.HasExpired: Boolean;
begin
  Result := Now > FLastRequestInfo.DateTime + FTimeout;
end;

constructor TKWebSession.Create(const AClientAddress, ASessionId: string; const ATimeout: Double);
begin
  Assert(ASessionId <> '');
  Assert(AClientAddress <> '');

  inherited Create;
  FSessionId := ASessionId;
  FLastRequestInfo.ClearData;
  FLastRequestInfo.ClientAddress := AClientAddress;
  FTimeout := ATimeout;
end;

destructor TKWebSession.Destroy;
begin
  NilEFIntf(FHomeController);
  NilEFIntf(FLoginController);
  NilEFIntf(FControllerHostWindow);
  NilEFIntf(FControllerContainer);
  NilEFIntf(FStatusHost);

  FreeAndNil(FOpenControllers);
  FreeAndNil(FAuthData);
  FreeAndNil(FGettextInstance);
  FreeAndNil(FDynamicScripts);
  FreeAndNil(FDynamicStyles);
  FreeAndNil(FObjectSpace);
  inherited;
end;

class procedure TKWebSession.SetCurrent(const Value: TKWebSession);
begin
  FCurrent := Value;
end;

procedure TKWebSession.SetDefaultLanguage(const AValue: string);
var
  I: Integer;
  LNewLanguage: string;
begin
  if Language = '' then
  begin
    LNewLanguage := AValue;
    I := Pos('-', LNewLanguage);
    if I <> 0 then
      // Convert language code
      LNewLanguage := Copy(LNewLanguage, I - 2, 2) + '_' + Uppercase(Copy(LNewLanguage, I + 1, 2));
    Language := LNewLanguage;
  end;
end;

class function TKWebSession.GetCurrent: TKWebSession;
begin
  Result := FCurrent;
end;

function TKWebSession.GetDefaultViewportWidth: Integer;
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

procedure TKWebSession.RemoveController(const AController: IJSController);
begin
  if Assigned(FOpenControllers) then
    FOpenControllers.Remove(AController);
end;

procedure TKWebSession.SetLanguage(const AValue: string);
begin
  FLanguage := AValue;
  TEFLocalizationToolRegistry.CurrentTool.ForceLanguage(FLanguage);
  TEFLogger.Instance.LogFmt('Language %s set.', [FLanguage], TEFLogger.LOG_DETAILED);
end;

procedure TKWebSession.SetLanguageFromQueriesOrConfig(const AConfig: TKConfig);
var
  LLanguageId: string;
begin
  LLanguageId := TKWebRequest.Current.GetQueryField('lang');
  if LLanguageId = '' then
    LLanguageId := AConfig.Config.GetString('LanguageId');
  if LLanguageId <> '' then
    Language := LLanguageId;
end;

function TKWebSession.GetDisplayName: string;
begin
  Result := FDisplayName;
  if Result = '' then
    Result := SessionId;
end;

procedure TKWebSession.AfterConstruction;
begin
  inherited;
  FCreationDateTime := Now;

  FDynamicScripts := TStringList.Create;
  FDynamicScripts.Sorted := True;
  FDynamicScripts.Duplicates := dupError;
  FDynamicStyles := TStringList.Create;
  FDynamicStyles.Sorted := True;
  FDynamicStyles.Duplicates := dupError;

  FAuthData := TEFNode.Create;

  FGettextInstance := TGnuGettextInstance.Create;

  FOpenControllers := TList<IJSController>.Create;
end;

procedure TKWebSession.EnsureDynamicScript(const AScriptBaseName: string);
var
  LIndex: Integer;
  LURL: string;
  LResourceFileName: string;
  LResourcePathName: string;
begin
  if not FDynamicScripts.Find(AScriptBaseName, LIndex) then
  begin
    LResourceFileName := IncludeTrailingPathDelimiter('js') + AScriptBaseName + '.js';
    LResourcePathName := TKWebApplication.Current.FindResourcePathName(LResourceFileName);
    if LResourcePathName <> '' then
    begin
      LURL := TKWebApplication.Current.FindResourceURL(LResourceFileName);
      TKWebResponse.Current.Items.ExecuteJSCode(Format('addScriptRef("%s");', [LURL]));
      FDynamicScripts.Add(AScriptBaseName);
    end;
  end;
end;

procedure TKWebSession.EnsureDynamicStyle(const AStyleBaseName: string);
var
  LIndex: Integer;
  LURL: string;
  LResourceFileName: string;
  LResourcePathName: string;
begin
  if not FDynamicStyles.Find(AStyleBaseName, LIndex) then
  begin
    LResourceFileName := IncludeTrailingPathDelimiter('js') + AStyleBaseName + '.css';
    LResourcePathName := TKWebApplication.Current.FindResourcePathName(LResourceFileName);
    if LResourcePathName <> '' then
    begin
      LURL := TKWebApplication.Current.FindResourceURL(LResourceFileName);
      TKWebResponse.Current.Items.ExecuteJSCode(Format('addLinkRef("%s");', [LURL]));
      FDynamicStyles.Add(AStyleBaseName);
    end;
  end;
end;

procedure TKWebSession.EnsureSupportFiles(const ABaseName: string);
begin
  if ABaseName <> '' then
  begin
    EnsureDynamicStyle(ABaseName);
    EnsureDynamicScript(ABaseName);
  end;
end;

procedure TKWebSession.EnsureViewSupportFiles(const AView: TKView);
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

function TKWebSession.GetManifestFileName: string;
begin
  Result := '';
end;

function TKWebSession.GetObjectSpace: TJSObjectSpace;
begin
  if not Assigned(FObjectSpace) then
    FObjectSpace := TJSObjectSpace.Create(nil);
  Result := FObjectSpace;
end;

{ TKWebSessionLocalizationTool }

procedure TKWebSessionLocalizationTool.AfterConstruction;
begin
  inherited;
  // Configure the global dxgettext instance.
  GetGnuGettextInstance.bindtextdomain(KITTO_TEXT_DOMAIN,
    TKConfig.SystemHomePath + 'locale');
end;

function TKWebSessionLocalizationTool.AsObject: TObject;
begin
  Result := Self;
end;

procedure TKWebSessionLocalizationTool.ForceLanguage(const ALanguageId: string);
var
  LInstance: TGnuGettextInstance;
begin
  LInstance := GetGnuGettextInstance;
  // Configure the per-session dxgettext instance.
  LInstance.bindtextdomain(KITTO_TEXT_DOMAIN,
    TKConfig.SystemHomePath + 'locale');
  LInstance.UseLanguage(ALanguageId);
end;

function TKWebSessionLocalizationTool.GetCurrentLanguageId: string;
begin
  Result := GetGnuGettextInstance.GetCurrentLanguage;
end;

function TKWebSessionLocalizationTool.GetGnuGettextInstance: TGnuGettextInstance;
begin
  if TKWebSession.Current <> nil then
    Result := TKWebSession.Current.FGettextInstance
  else
    Result := gnugettext.DefaultInstance;
end;

procedure TKWebSessionLocalizationTool.TranslateComponent(const AComponent: TComponent);
var
  LInstance: TGnuGettextInstance;
begin
  LInstance := GetGnuGettextInstance;
  LInstance.TranslateComponent(AComponent, KITTO_TEXT_DOMAIN);
  LInstance.TranslateComponent(AComponent, 'default');
end;

function TKWebSessionLocalizationTool.TranslateString(const AString,
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

{ TKWebSessions }

constructor TKWebSessions.Create(const ATimeout: Double);
begin
  inherited Create;
  FTimeout := ATimeout;
end;

procedure TKWebSessions.AfterConstruction;
begin
  inherited;
  FSessions := TObjectList<TKWebSession>.Create;
end;

procedure TKWebSessions.CleanupExpiredSessions;
var
  I: Integer;
  LSession: TKWebSession;
begin
  MonitorEnter(FSessions);
  try
    for I := FSessions.Count - 1 downto 0 do
    begin
      LSession := FSessions[I];
      if LSession.HasExpired then
      begin
        FSessions.Extract(LSession);
        try
          SessionRemoved(LSession);
        finally
          FreeAndNil(LSession);
        end;
      end;
    end;
  finally
    MonitorExit(FSessions);
  end;
end;

procedure TKWebSessions.ClearSessions;
begin
  MonitorEnter(FSessions);
  try
    while FSessions.Count > 0 do
      RemoveSession(FSessions[0]);
  finally
    MonitorExit(FSessions);
  end;
end;

procedure TKWebSessions.RemoveSession(const ASession: TKWebSession);
begin
  MonitorEnter(FSessions);
  try
    FSessions.Extract(ASession);
    try
      SessionRemoved(ASession);
    finally
      ASession.Free;
    end;
  finally
    MonitorExit(FSessions);
  end;
end;

procedure TKWebSessions.SessionAdded(const ASession: TKWebSession);
begin
  if Assigned(FOnSessionStart) then
    FOnSessionStart(ASession);
end;

procedure TKWebSessions.SessionRemoved(const ASession: TKWebSession);
begin
  if Assigned(FOnSessionEnd) then
    FOnSessionEnd(ASession);
end;

destructor TKWebSessions.Destroy;
begin
  FreeAndNil(FSessions);
  inherited;
end;

function TKWebSessions.GetSessions: TArray<TKWebSession>;
begin
  MonitorEnter(FSessions);
  try
    Result := FSessions.ToArray;
  finally
    MonitorExit(FSessions);
  end;
end;

function TKWebSessions.NewSession(const AClientAddress: string; const ASessionId: string = ''): TKWebSession;
var
  LSessionId: string;
begin
  Assert(AClientAddress <> '');

  if ASessionId <> '' then
    LSessionId := ASessionId
  else
    LSessionId := CreateNewSessionId;

  Result := TKWebSession.Create(AClientAddress, LSessionId, FTimeout);
  FSessions.Add(Result);
  SessionAdded(Result);
end;

function TKWebSessions.FindSession(const ASessionId, AClientAddress: string): TKWebSession;
begin
  MonitorEnter(FSessions);
  try
    if ASessionId <> '' then
      Result := FindSessionById(ASessionId)
    else if AClientAddress <> '' then
      Result := FindSessionByClientAddress(AClientAddress)
    else
      Result := nil;
  finally
    MonitorExit(FSessions);
  end;
end;

function TKWebSessions.FindSessionById(const ASessionId: string): TKWebSession;
var
  I: Integer;
begin
  for I := 0 to FSessions.Count - 1 do
    if FSessions[I].SessionId = ASessionId then
      Exit(FSessions[I]);
  Result := nil;
end;

function TKWebSessions.FindSessionByClientAddress(const AClientAddress: string): TKWebSession;
var
  I: Integer;
begin
  for I := 0 to FSessions.Count - 1 do
    if FSessions[I].LastRequestInfo.ClientAddress = AClientAddress then
      Exit(FSessions[I]);
  Result := nil;
end;

function TKWebSessions.CreateNewSessionId: string;
begin
  Result := CreateCompactGuidStr;
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

initialization
  TEFLocalizationToolRegistry.RegisterTool(TKWebSessionLocalizationTool.Create);

finalization
  TEFLocalizationToolRegistry.UnregisterTool;

end.
