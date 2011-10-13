unit Kitto.Ext.Login;

interface

uses
  Ext, ExtForm,
  Kitto.Ext.Base;

type
  TKExtOnLogin = procedure of object;

  TKExtLoginWindow = class(TKExtWindowControllerBase)
  private
    FUserName: TExtFormTextField;
    FPassword: TExtFormTextField;
    FButton: TExtButton;
    FOnLogin: TKExtOnLogin;
  protected
    procedure InitDefaults; override;
  public
    property OnLogin: TKExtOnLogin read FOnLogin write FOnLogin;

    class function Authenticate(const AUserName, APassword: string): Boolean;
  published
    procedure DoLogin;
  end;

implementation

uses
  ExtPascalUtils,
  EF.Classes, EF.Localization, EF.Tree,
  Kitto.Types, Kitto.Environment, Kitto.Ext.Session;

{ TKExtLoginWindow }

procedure TKExtLoginWindow.DoLogin;
begin
  if Authenticate(Session.Query['UserName'], Session.Query['Password']) then
  begin
    Close;
    if Assigned(FOnLogin) then
      FOnLogin;
  end
  else
  begin
    Session.Flash(_('Invalid login.'));
    FUserName.Focus(False, 500);
  end;
end;

class function TKExtLoginWindow.Authenticate(const AUserName, APassword: string): Boolean;
var
  LAuthData: TEFNode;
begin
  LAuthData := TEFNode.Create;
  try
    Environment.Authenticator.DefineAuthData(LAuthData);
    LAuthData.SetString('UserName', AUserName);
    LAuthData.SetString('Password', APassword);
    Result := Environment.Authenticator.Authenticate(LAuthData);
  finally
    LAuthData.Free;
  end;
end;

procedure TKExtLoginWindow.InitDefaults;
begin
  inherited;
  Title := Environment.AppTitle;
  Modal := True;
  Width := 246;
  Height := 140;
  Plain := True;
  Layout := lyFit;
  Closable := False;
  Resizable := False;
  with TExtFormFormPanel.AddTo(Items) do begin
    LabelWidth := 70;
    Border := False;
    ButtonAlign := baRight;
    BodyStyle := SetPaddings(5, 5);
    Defaults := JSObject('width: 130');
    Frame := True;
    MonitorValid := True;

    FUserName := TExtFormTextField.AddTo(Items);
    FUserName.Name := 'UserName';
    FUserName.FieldLabel := _('User Name');
    FUserName.AllowBlank := False;

    FPassword := TExtFormTextField.AddTo(Items);
    FPassword.Name := 'Password';
    FPassword.FieldLabel := _('Password');
    FPassword.InputType := itPassword;
    FPassword.AllowBlank := False;

    FButton := TExtButton.AddTo(Buttons);
    FButton.Icon := Environment.GetImageURL('login');
    FButton.Text := _('Login');
    FButton.Handler := Ajax(DoLogin, ['UserName', FUserName.GetValue, 'Password', FPassword.GetValue]);
    FButton.Scale := 'medium';
    FButton.Plugins := JSObject('"defaultButton"', '', False);
    FButton.FormBind := True;
  end;
  FUserName.Focus(False, 1000);
end;

end.
