unit Kitto.Ext.Login;

interface

uses
  Ext, ExtForm,
  Kitto.Ext.Base;

type
  TKExtOnLogin = procedure of object;

  TKExtLoginWindow = class(TKExtWindow)
  private
    FUserName: TExtFormTextField;
    FPassword: TExtFormTextField;
    FButton: TExtButton;
    FOnLogin: TKExtOnLogin;
    procedure DoAuthenticate;
  protected
    procedure InitDefaults; override;
  public
    property OnLogin: TKExtOnLogin read FOnLogin write FOnLogin;

    class procedure Authenticate(const AUserName, AUserPassword: string);
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
  { TODO : refactor authenticators so that they don't necessarily raise exceptions }
  try
    DoAuthenticate;
    Close;
    if Assigned(FOnLogin) then
      FOnLogin;
  except
    on E: EKError do
    begin
      ExtMessageBox.Alert(Title, E.Message, JSFunction(FUserName.JSName + '.focus(false, 500);'));
      FUserName.Focus(False, 1000);
    end;
  end;
end;

class procedure TKExtLoginWindow.Authenticate(const AUserName,
  AUserPassword: string);
var
  LAuthenticationData: TEFNode;
begin
  LAuthenticationData := TEFNode.Create;
  try
    Environment.AuthenticationHost.CurrentAuthenticator.DefineAuthenticationData(LAuthenticationData);
    try
      LAuthenticationData.SetString('UserName', AUserName);
      LAuthenticationData.SetString('UserPassword', AUserPassword);
      Environment.AuthenticationHost.CurrentAuthenticator.Authenticate(LAuthenticationData);
    except
      raise;
    end;
  finally
    LAuthenticationData.Free;
  end;
end;

procedure TKExtLoginWindow.DoAuthenticate;
begin
  Authenticate(Session.Query['UserName'], Session.Query['UserPassword']);
end;

procedure TKExtLoginWindow.InitDefaults;
begin
  inherited;
  Title := Environment.AppTitle;
  Modal := True;
  Width := 366;
  Height := 136;
  Plain := True;
  Layout := lyFit;
  Closable := False;
  Resizable := False;
  with TExtFormFormPanel.AddTo(Items) do begin
    LabelWidth := 70;
    Border := False;
    ButtonAlign := baRight;
    BodyStyle := SetPaddings(5, 5);
    Defaults := JSObject('width: 250');
    Frame := True;
    MonitorValid := True;

    FUserName := TExtFormTextField.AddTo(Items);
    FUserName.Name := 'UserName';
    FUserName.FieldLabel := _('User Name');
    FUserName.AllowBlank := False;

    FPassword := TExtFormTextField.AddTo(Items);
    FPassword.Name := 'UserPassword';
    FPassword.FieldLabel := _('Password');
    FPassword.InputType := itPassword;
    FPassword.AllowBlank := False;

    FButton := TExtButton.AddTo(Buttons);
    FButton.Text := _('Login');
    FButton.Handler := Ajax(DoLogin, ['UserName', FUserName.GetValue, 'UserPassword', FPassword.GetValue]);
    //LButton.Scale := 'medium';
    FButton.Plugins := JSObject('"defaultButton"', '', False);
    FButton.FormBind := True;
  end;
  FUserName.Focus(False, 1000);
end;

end.
