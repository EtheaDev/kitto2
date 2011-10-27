unit Kitto.Ext.Session;

interface

uses
  SysUtils,
  ExtPascal, Ext,
  EF.Tree,
  Kitto.Ext.Base, Kitto.Ext.Controller, Kitto.Environment, Kitto.Metadata.Views,
  Kitto.Ext.Login;

type
  TKExtSession = class(TExtThread)
  private
    FHomeController: IKExtController;
    FEnvironment: TKEnvironment;
    FLoginWindow: TKExtLoginWindow;
    FViewHost: TExtTabPanel;
    FStatusHost: TKExtStatusBar;
    function GetEnvironment: TKEnvironment;
    procedure LoadLibraries;
    procedure DisplayHomeView;
    procedure DisplayLoginWindow;
  protected
    function BeforeHandleRequest: Boolean; override;
    procedure AfterHandleRequest; override;
  public
    procedure AfterConstruction; override;
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

    property Environment: TKEnvironment read GetEnvironment;
    procedure InitDefaultValues; override;
    procedure Home; override;

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
  published
    procedure Logout;
  end;

function Session: TKExtSession;

implementation

uses
  Classes, StrUtils, ActiveX, ComObj, Types, FmtBcd,
  ExtPascalUtils, ExtForm, FCGIApp,
  EF.Intf, EF.StrUtils, EF.Localization,
  Kitto.Ext.Utils, Kitto.Auth, Kitto.Types;

function GetSessionEnvironment: TKEnvironment;
begin
  Assert(Session <> nil);

  Result := Session.Environment;
end;

function Session: TKExtSession;
begin
  Result := TKExtSession(CurrentWebSession);
end;

procedure TKExtSession.AfterConstruction;
begin
  inherited;
  SetEnvironmentSingleton(@GetSessionEnvironment);
end;

function TKExtSession.GetEnvironment: TKEnvironment;
begin
  if not Assigned(FEnvironment) then
    FEnvironment := TKEnvironment.Create;
  Result := FEnvironment;
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
  FreeAndNil(FEnvironment);
end;

procedure TKExtSession.DisplayHomeView;
var
  LHomeView: TKView;
begin
  NilEFIntf(FHomeController);

  LHomeView := Environment.Views.FindViewByNode(Environment.Config.FindNode('HomeView'));
  if not Assigned(LHomeView) then
    LHomeView := Environment.Views.ViewByName('Home');
  FHomeController := TKExtControllerFactory.Instance.CreateController(LHomeView, nil);
  FHomeController.Display;
end;

procedure TKExtSession.Home;
begin
  if not IsAjax then
    LoadLibraries;

  // Try authentication with default credentials, if any, and skip login
  // window if it succeeds.
  if TKExtLoginWindow.Authenticate then
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
  JSCode('Ext.example.msg("' + Environment.AppTitle + '", "' + AMessage + '");');
end;

procedure TKExtSession.LoadLibraries;

  procedure SetRequiredLibrary(const ALibName: string; const AIncludeCSS: Boolean = False);
  var
    LLibURL: string;
  begin
    LLibURL := Environment.GetResourceURL(IncludeTrailingPathDelimiter('js') + ALibName + '.js');
    SetLibrary(StripSuffix(LLibURL, '.js'), AIncludeCSS, False, True);
  end;

  procedure SetOptionalLibrary(const ALibName: string);
  var
    LLibURL: string;
  begin
    LLibURL := Environment.FindResourceURL(IncludeTrailingPathDelimiter('js') + ALibName + '.js');
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

  LLibraries := Environment.Config.GetStringArray('JavaScriptLibraries');
  for LLibName in LLibraries do
    SetRequiredLibrary(LLibName);
end;

procedure TKExtSession.Logout;
begin
  Environment.Authenticator.Logout;
  Home;
end;

procedure TKExtSession.DisplayView(const AName: string);
begin
  Assert(AName <> '');

  DisplayView(Environment.Views.ViewByName(AName));
end;

procedure TKExtSession.DisplayView(const AView: TKView);
var
  LController: IKExtController;
begin
  Assert(Assigned(AView));
  Assert(Assigned(FViewHost));

  LController := TKExtControllerFactory.Instance.CreateController(AView, FViewHost);
  LController.Display;
  FViewHost.SetActiveTab(FViewHost.Items.Count - 1);
  if Assigned(FStatusHost) then
    FStatusHost.ClearStatus;
end;

procedure TKExtSession.InitDefaultValues;
var
  LLanguageId: string;
begin
  inherited;
  ExtPath := Environment.Config.GetString('Ext/URL', '/ext');
  Charset := Environment.Config.GetString('Charset', 'utf-8');
  LLanguageId := Environment.Config.GetString('LanguageId');
  if LLanguageId <> '' then
    Language := LLanguageId;
  Theme := Environment.Config.GetString('Ext/Theme');
end;

procedure TKExtSession.AfterHandleRequest;
begin
  inherited;
  { TODO : only do this when ADO is used }
  CoUninitialize;
end;

function TKExtSession.BeforeHandleRequest: Boolean;
begin
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
  LIconURL := Environment.GetImageURL(Result);
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

finalization
  SetEnvironmentSingleton(nil);

end.

