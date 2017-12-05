{ -------------------------------------------------------------------------------
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
  Classes
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
  ;

type
  /// <summary>
  ///  Represents the server side of a user client session.
  ///  Holds all objects pertaining to the user session.
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
    FLastRequestInfo: TJSRequestInfo;
    FCreationDateTime: TDateTime;
    FObjectSpace: TJSObjectSpace;
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
    procedure BeforeHandleRequest;

    function GetDefaultViewportWidth: Integer;
  public
    constructor Create(const ASessionId: string); reintroduce;
    procedure AfterConstruction; override;
    destructor Destroy; override;

    property CreationDateTime: TDateTime read FCreationDateTime;
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
    /// <summary>
    ///  The current session's UUID.
    /// </summary>
    property SessionId: string read FSessionId;
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

    property LastRequestInfo: TJSRequestInfo read FLastRequestInfo;
    procedure SetDefaultLanguage(const AValue: string);

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
  TKSessionLocalizationTool = class(TEFNoRefCountObject, IInterface, IEFInterface, IEFLocalizationTool)
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

implementation

uses
  SysUtils
  , EF.Logger
  , Kitto.Web.Request
  , Kitto.Web.Response
  , Kitto.Web.Application
  ;

{ TKWebSession }

function TKWebSession.GetViewportContent: string;
begin
  Result := '';
end;

procedure TKWebSession.BeforeHandleRequest;
var
  I: Integer;
begin
  if FLanguage = '' then
  begin
    FLanguage := TKWebRequest.Current.AcceptLanguage;
    I := Pos('-', FLanguage);
    if I <> 0 then
      // Convert language code
      FLanguage := Copy(FLanguage, I - 2, 2) + '_' + Uppercase(Copy(FLanguage, I + 1, 2));
  end;
end;

constructor TKWebSession.Create(const ASessionId: string);
begin
  Assert(ASessionId <> '');

  inherited Create;
  FSessionId := ASessionId;
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
  FreeAndNil(FLastRequestInfo);
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
  FLastRequestInfo := TJSRequestInfo.Create;
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

{ TKSessionLocalizationTool }

procedure TKSessionLocalizationTool.AfterConstruction;
begin
  inherited;
  // Configure the global dxgettext instance.
  GetGnuGettextInstance.bindtextdomain(KITTO_TEXT_DOMAIN,
    TKConfig.SystemHomePath + 'locale');
end;

function TKSessionLocalizationTool.AsObject: TObject;
begin
  Result := Self;
end;

procedure TKSessionLocalizationTool.ForceLanguage(const ALanguageId: string);
var
  LInstance: TGnuGettextInstance;
begin
  LInstance := GetGnuGettextInstance;
  // Configure the per-session dxgettext instance.
  LInstance.bindtextdomain(KITTO_TEXT_DOMAIN,
    TKConfig.SystemHomePath + 'locale');
  LInstance.UseLanguage(ALanguageId);
end;

function TKSessionLocalizationTool.GetCurrentLanguageId: string;
begin
  Result := GetGnuGettextInstance.GetCurrentLanguage;
end;

function TKSessionLocalizationTool.GetGnuGettextInstance: TGnuGettextInstance;
begin
  if TKWebSession.Current <> nil then
    Result := TKWebSession.Current.FGettextInstance
  else
    Result := gnugettext.DefaultInstance;
end;

procedure TKSessionLocalizationTool.TranslateComponent(const AComponent: TComponent);
var
  LInstance: TGnuGettextInstance;
begin
  LInstance := GetGnuGettextInstance;
  LInstance.TranslateComponent(AComponent, KITTO_TEXT_DOMAIN);
  LInstance.TranslateComponent(AComponent, 'default');
end;

function TKSessionLocalizationTool.TranslateString(const AString,
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

initialization
  TEFLocalizationToolRegistry.RegisterTool(TKSessionLocalizationTool.Create);

finalization
  TEFLocalizationToolRegistry.UnregisterTool;

end.
