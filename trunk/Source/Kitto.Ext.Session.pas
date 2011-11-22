{-------------------------------------------------------------------------------
   Copyright 2011 Ethea S.r.l.

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

unit Kitto.Ext.Session;

{$I Kitto.Defines.inc}

interface

uses
  SysUtils,
  ExtPascal, Ext,
  EF.Tree,
  Kitto.Ext.Base, Kitto.Ext.Controller, Kitto.Config, Kitto.Metadata.Views,
  Kitto.Ext.Login;

type
  TKExtSession = class(TExtThread)
  private
    FHomeController: IKExtController;
    FConfig: TKConfig;
    FLoginWindow: TKExtLoginWindow;
    FViewHost: TExtTabPanel;
    FStatusHost: TKExtStatusBar;
    procedure LoadLibraries;
    procedure DisplayHomeView;
    procedure DisplayLoginWindow;
    function GetConfig: TKConfig;
  protected
    function BeforeHandleRequest: Boolean; override;
    procedure AfterHandleRequest; override;
    procedure AfterNewSession; override;
  public
    destructor Destroy; override;
  public
    ///	<summary>
    ///	  A reference to the panel to be used as the main view container.
    ///	</summary>
    property ViewHost: TExtTabPanel read FViewHost write FViewHost;

    ///	<summary>
    ///	  A reference to the status bar to be used for wait messages.
    ///	</summary>
    property StatusHost: TKExtStatusBar read FStatusHost write FStatusHost;

    procedure DisplayView(const AName: string); overload;
    procedure DisplayView(const AView: TKView); overload;

    procedure InitDefaultValues; override;
    procedure Home; override;

    ///	<summary>Opens the specified URL in a new browser window/tab.</summary>
    procedure Navigate(const AURL:string);
    ///	<summary>
    ///	  <para>
    ///	    Adds to the current session a style class named after AView's
    ///	    ImageName (or the specified custom AImageName) plus a '_img'
    ///	    suffix, that sets background:url to the URL of the view's image.
    ///	  </para>
    ///	  <para>
    ///	    The style class can have an optional custom prefix before the name
    ///	    and custom rules attached to it.
    ///	  </para>
    ///	</summary>
    ///	<returns>
    ///	  Returns the class name so that it can be assigned to a component's
    ///	  IconCls property.
    ///	</returns>
    function SetViewIconStyle(const AView: TKView; const AImageName: string = '';
      const ACustomPrefix: string = ''; const ACustomRules: string = ''): string;

    // Test
    function GetGCObjectCount: Integer;

    procedure Flash(const AMessage: string);

    property Config: TKConfig read GetConfig;
  published
    procedure Logout;
  end;

function Session: TKExtSession;

implementation

uses
  Classes, StrUtils, ActiveX, ComObj, Types, FmtBcd,
  ExtPascalUtils, ExtForm, FCGIApp,
  EF.Intf, EF.StrUtils, EF.Localization, EF.Macros, EF.Logger,
  Kitto.Auth, Kitto.Types, Kitto.AccessControl,
  Kitto.Ext.Utils;

function Session: TKExtSession;
begin
  Result := TKExtSession(CurrentWebSession);
end;

function TKExtSession.GetConfig: TKConfig;
begin
  if not Assigned(FConfig) then
    FConfig := TKConfig.Create;
  Result := FConfig;
end;

type
  PGarbage = ^TGarbage;
  TGarbage = record
    Garbage    : TObject;
    Persistent : Boolean;
  end;

function TKExtSession.GetGCObjectCount: Integer;
var
  I: Integer;
  LObject: TObject;
begin
  Result := 0;
  for I := 0 to FGarbageCollector.Count - 1 do
  begin
    LObject := FGarbageCollector.Objects[I];
    if (LObject <> nil) and (PGarbage(LObject)^.Garbage <> nil) then
      Inc(Result);
  end;
end;

destructor TKExtSession.Destroy;
begin
  inherited;
  NilEFIntf(FHomeController);
  FreeAndNil(FConfig);
end;

procedure TKExtSession.DisplayHomeView;
var
  LHomeView: TKView;
begin
  NilEFIntf(FHomeController);

  LHomeView := Config.Views.FindViewByNode(Config.Config.FindNode('HomeView'));
  if not Assigned(LHomeView) then
    LHomeView := Config.Views.ViewByName('Home');
  FHomeController := TKExtControllerFactory.Instance.CreateController(LHomeView, nil);
  FHomeController.Display;
end;

procedure TKExtSession.Home;
begin
  if not IsAjax then
    LoadLibraries;

  // Try authentication with default credentials, if any, and skip login
  // window if it succeeds.
  if TKExtLoginWindow.Authenticate(Query['UserName'], Query['Password']) then
    DisplayHomeView
  else
    DisplayLoginWindow;
end;

procedure TKExtSession.DisplayLoginWindow;
begin
  FreeAndNil(FLoginWindow);
  FLoginWindow := TKExtLoginWindow.Create;
  FLoginWindow.OnLogin := DisplayHomeView;
  FLoginWindow.Show;
end;

procedure TKExtSession.Flash(const AMessage: string);
begin
  { TODO : move functionality into kitto-core.js. }
  JSCode('Ext.example.msg("' + Config.AppTitle + '", "' + AMessage + '");');
end;

procedure TKExtSession.LoadLibraries;

  procedure SetRequiredLibrary(const ALibName: string; const AIncludeCSS: Boolean = False);
  var
    LLibURL: string;
  begin
    LLibURL := Config.GetResourceURL(IncludeTrailingPathDelimiter('js') + ALibName + '.js');
    SetLibrary(StripSuffix(LLibURL, '.js'), AIncludeCSS, False, True);
  end;

  procedure SetOptionalLibrary(const ALibName: string);
  var
    LLibURL: string;
  begin
    LLibURL := Config.FindResourceURL(IncludeTrailingPathDelimiter('js') + ALibName + '.js');
    if LLibURL <> '' then
      SetLibrary(StripSuffix(LLibURL, '.js'), False, False, True);
  end;

var
  LLibraries: TStringDynArray;
  LLibName: string;
begin
  SetLibrary(ExtPath + '/examples/ux/statusbar/StatusBar');
  SetCSS(ExtPath + '/examples/ux/statusbar/css/statusbar');
  SetLibrary(ExtPath + '/examples/shared/examples'); // For Ext.msg.
  SetLibrary(ExtPath + '/src/locale/ext-lang-' + Language);
  SetRequiredLibrary('DateTimeField');
  SetRequiredLibrary('DefaultButton');
  SetRequiredLibrary('kitto-core', True);
  SetOptionalLibrary('application');

  LLibraries := Config.Config.GetStringArray('JavaScriptLibraries');
  for LLibName in LLibraries do
    SetRequiredLibrary(LLibName);
end;

procedure TKExtSession.Logout;
begin
  Config.Authenticator.Logout;
  Home;
end;

procedure TKExtSession.Navigate(const AURL: string);
begin
  Response := Format('window.open("%s", "_blank");', [AURL]);
end;

procedure TKExtSession.DisplayView(const AName: string);
begin
  Assert(AName <> '');

  DisplayView(Config.Views.ViewByName(AName));
end;

procedure TKExtSession.DisplayView(const AView: TKView);
var
  LController: IKExtController;
begin
  Assert(Assigned(AView));
  Assert(Assigned(FViewHost));

  if AView.IsAccessGranted(ACM_VIEW) then
  begin
    LController := TKExtControllerFactory.Instance.CreateController(AView, FViewHost);
    LController.Display;
    FViewHost.SetActiveTab(FViewHost.Items.Count - 1);
    if Assigned(FStatusHost) then
      FStatusHost.ClearStatus;
  end;
end;

procedure TKExtSession.InitDefaultValues;
var
  LLanguageId: string;
begin
  inherited;
  ExtPath := Config.Config.GetString('Ext/URL', '/ext');
  Charset := Config.Config.GetString('Charset', 'utf-8');
  LLanguageId := Config.Config.GetString('LanguageId');
  if LLanguageId <> '' then
    Language := LLanguageId;
  Theme := Config.Config.GetString('Ext/Theme');
end;

procedure TKExtSession.AfterHandleRequest;
begin
  inherited;
  { TODO : only do this when ADO is used }
  CoUninitialize;
end;

procedure TKExtSession.AfterNewSession;
begin
  inherited;
  TEFLogger.Instance.LogFmt('New session %s.', [Cookie['FCGIThread']], TEFLogger.LOG_MEDIUM);
end;

function TKExtSession.BeforeHandleRequest: Boolean;
begin
  TEFLogger.Instance.LogStrings('BeforeHandleRequest', Queries, TEFLogger.LOG_DETAILED);
  { TODO : only do this when ADO is used }
  OleCheck(CoInitialize(nil));
  Result := inherited BeforeHandleRequest;
end;

function TKExtSession.SetViewIconStyle(const AView: TKView; const AImageName: string;
  const ACustomPrefix: string; const ACustomRules: string): string;
var
  LIconURL: string;
  LRule: string;
begin
  Assert(Assigned(AView));

  Result := IfThen(AImageName <> '', AImageName, AView.ImageName);
  LIconURL := Config.GetImageURL(Result);
  Result := ACustomPrefix + Result + '_img';
  // The !important rule allows to use a non-specific selector, so that the icon
  // can be shared by different components.
  // no-repeat is added because some components (such as buttons) repeat by default
  // (others, such as menu items and tree nodes, don't).
  LRule := '.' + Result + ' {background: url(' + LIconURL + ') no-repeat left !important;' + ACustomRules + '}';
  if IsAjax then
    JSCode('addStyleRule("' + LRule + '");')
  else
    SetStyle(LRule);
end;

initialization
  TEFMacroExpansionEngine.OnGetInstance :=
    function: TEFMacroExpansionEngine
    begin
      if Session <> nil then
        Result := Session.Config.MacroExpansionEngine
      else
        Result := nil;
    end;

  TKConfig.OnGetInstance :=
    function: TKConfig
    begin
      if Session <> nil then
        Result := Session.Config
      else
        Result := nil;
    end;

finalization
  TEFMacroExpansionEngine.OnGetInstance := nil;
  TKConfig.OnGetInstance := nil;

end.

