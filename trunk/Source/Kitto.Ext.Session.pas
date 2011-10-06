unit Kitto.Ext.Session;

interface

uses
  SysUtils,
  ExtPascal,
  Kitto.Controller, Kitto.Environment, Kitto.Metadata.Views, Kitto.Ext.Login;

type
  TKExtSession = class(TExtThread)
  private
    FHomeController: IKController;
    FEnvironment: TKEnvironment;
    FFormatSettings: TFormatSettings;
    FLoginWindow: TKExtLoginWindow;
    FIsAuthenticated: Boolean;
    function GetEnvironment: TKEnvironment;
    procedure LoadLibraries;
    procedure DisplayHomeView;
    procedure DoLogin;
  protected
    function BeforeHandleRequest: Boolean; override;
    procedure AfterHandleRequest; override;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
  public
    property Environment: TKEnvironment read GetEnvironment;
    procedure InitDefaultValues; override;
    procedure Home; override;
    {
      Adds to the current session a style class named after AView's Name.
      plus a '_icon' suffix, that sets background:url to the GUI element's
      bitmap. Returns the class name so that it can be assigned to a component's
      IconCls property.
    }
    function SetViewIconStyle(const AView: TKView; const AImageName: string = ''): string;

    // Test
    function GetGCObjectCount: Integer;

    property FormatSettings: TFormatSettings read FFormatSettings;
  published
    procedure Logout;
  end;

function Session: TKExtSession;

implementation

uses
  Classes, StrUtils, ActiveX, ComObj, Types,
  ExtPascalUtils, Ext, ExtForm, FCGIApp,
  EF.Intf, EF.StrUtils,
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
  FreeAndNil(FEnvironment);
  inherited;
end;

procedure TKExtSession.DisplayHomeView;
begin
  FIsAuthenticated := True;
  FreeAndNilEFIntf(FHomeController);
  FHomeController := TKControllerFactory.Instance.CreateController(
    Environment.Views.ViewByName(Environment.Config.GetString('Home/View', 'Main')));
  TExtPanel(FHomeController.AsObject).SetVisible(False);
  FHomeController.Display;
end;

procedure TKExtSession.Home;
var
  LAutoUserName: string;
  LAutoUserPassword: string;
begin
  //if not IsAjax then
  LoadLibraries;

  if TKAuthenticationHost.IsNullAuthenticator then
    FIsAuthenticated := True;
  if FIsAuthenticated then
    DisplayHomeView
  else
  begin
    LAutoUserName := Environment.Config.GetString('Authentication/AutoUserName');
    LAutoUserPassword := Environment.Config.GetString('Authentication/AutoUserPassword');
    if (LAutoUserName <> '') and (LAutoUserPassword <> '') then
    begin
      { TODO : refactor authentication so that it doesn't raise exceptions }
      try
        TKExtLoginWindow.Authenticate(LAutoUserName, LAutoUserPassword);
        DisplayHomeView;
      except
        on E: EKError do
          DoLogin;
      end;
    end
    else
      DoLogin;
  end;
end;

procedure TKExtSession.DoLogin;
begin
  FreeAndNil(FLoginWindow);
  FLoginWindow := TKExtLoginWindow.Create;
  FLoginWindow.OnLogin := DisplayHomeView;
  FLoginWindow.Show;
end;

procedure TKExtSession.LoadLibraries;

  procedure SetExistingLibrary(const ALibName: string);
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
  SetLibrary(ExtPath + '/examples/form/DateTimeField');

  SetLibrary(StripSuffix(Environment.GetResourceURL('js/kitto-core.js'), '.js'), False, False, True);
  SetExistingLibrary('application');

  LLibraries := Environment.Config.GetStringArray('JavaScriptLibraries');
  for LLibName in LLibraries do
    SetExistingLibrary(LLibName);
end;

procedure TKExtSession.Logout;
begin
  { TODO : shorthen names and provide a logout method when refactoring this }
  Environment.AuthenticationHost.CurrentAuthenticator.AuthenticationData.Clear;
  FIsAuthenticated := False;
  Home;
end;

procedure TKExtSession.InitDefaultValues;
var
  LLanguageId: string;
begin
  inherited;
  FIsAuthenticated := False;
  ExtPath := Environment.Config.GetString('Ext/URL', '/ext');
  Charset := Environment.Config.GetString('Charset', 'utf-8');
  LLanguageId := Environment.Config.GetString('LanguageId');
  if LLanguageId <> '' then
    Language := LLanguageId;
  FFormatSettings := TFormatSettings.Create;
  FFormatSettings.ShortTimeFormat := 'hh:mm:ss';
  { TODO : read default format settings from environment and allow to change them on a per-user basis. }
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

function TKExtSession.SetViewIconStyle(const AView: TKView; const AImageName: string): string;

  function GetViewIconStyleClassName(const AView: TKView): string;
  begin
    Result := AView.PersistentName + '_icon';
  end;

var
  LIconURL: string;
  LRule: string;
begin
  Assert(Assigned(AView));

  LIconURL := Environment.GetImageURL(IfThen(AImageName <> '', AImageName, AView.ImageName));
  Result := GetViewIconStyleClassName(AView);
  // The !important rule allows to use a non-specific selector, so that the icon
  // can be shared by different components.
  // no-repeat is added because some components (such as buttons) repeat by default
  // (others, such as menu items and tree nodes, don't).
  LRule := '.' + Result + '{background: url(' + LIconURL + ') no-repeat left !important;}';
  if IsAjax then
    JSCode('addStyleRule("' + LRule + '", 0);')
  else
    SetStyle(LRule);
end;

initialization

finalization
  SetEnvironmentSingleton(nil);

end.
