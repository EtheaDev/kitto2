{-------------------------------------------------------------------------------
   Copyright 2012 Ethea S.r.l.

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

unit Kitto.Ext.Login;

{$I Kitto.Defines.inc}

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
    FStatusBar: TKExtStatusBar;
    FFormPanel: TExtFormFormPanel;
  protected
    procedure InitDefaults; override;
  public
    property OnLogin: TKExtOnLogin read FOnLogin write FOnLogin;

    class function Authenticate(const AUserName: string = ''; const APassword: string = ''): Boolean;
  published
    procedure DoLogin;
  end;

implementation

uses
  SysUtils,
  ExtPascalUtils,
  EF.Classes, EF.Localization, EF.Tree,
  Kitto.Types, Kitto.Ext.Session;

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
    FStatusBar.SetErrorStatus(_('Invalid login.'));
    FPassword.Focus(False, 500);
  end;
end;

class function TKExtLoginWindow.Authenticate(const AUserName, APassword: string): Boolean;
var
  LAuthData: TEFNode;
begin
  if Session.Config.Authenticator.IsAuthenticated then
    Result := True
  else
  begin
    LAuthData := TEFNode.Create;
    try
      Session.Config.Authenticator.DefineAuthData(LAuthData);
      if AUserName <> '' then
        LAuthData.SetString('UserName', AUserName);
      if APassword <> '' then
        LAuthData.SetString('Password', APassword);
      Result := Session.Config.Authenticator.Authenticate(LAuthData);
    finally
      LAuthData.Free;
    end;
  end;
end;

procedure TKExtLoginWindow.InitDefaults;

  function GetEnableButtonJS: string;
  begin
    Result := Format(
      '%s.setDisabled(%s.getValue() == "" || %s.getValue() == "");',
      [FButton.JSName, FUserName.JSName, FPassword.JSName]);
  end;

  function GetSubmitJS: string;
  begin
    Result := Format(
      // For some reason != does not survive rendering.
      'if (e.getKey() == 13 && !(%s.getValue() == "") && !(%s.getValue() == "")) %s.handler.call(%s.scope, %s);',
      [FUserName.JSName, FPassword.JSName, FButton.JSName, FButton.JSName, FButton.JSName]);
  end;

begin
  inherited;
  Title := _(Session.Config.AppTitle);
  Width := 246;
  Height := 120;
  Closable := False;
  Resizable := False;

  FStatusBar := TKExtStatusBar.Create;
  FStatusBar.DefaultText := '';
  FStatusBar.BusyText := _('Logging in...');

  FFormPanel := TExtFormFormPanel.AddTo(Items);
  FFormPanel.Region := rgCenter;
  FFormPanel.LabelWidth := 80;
  FFormPanel.Border := False;
  FFormPanel.BodyStyle := SetPaddings(5, 5);
  FFormPanel.Frame := False;
  FFormPanel.MonitorValid := True;
  FFormPanel.Bbar := FStatusBar;

  FButton := TExtButton.AddTo(FStatusBar.Items);
  FButton.Icon := Session.Config.GetImageURL('login');
  FButton.Text := _('Login');

  FUserName := TExtFormTextField.AddTo(FFormPanel.Items);
  FUserName.Name := 'UserName';
  FUserName.Value := Session.Config.Authenticator.AuthData.GetString('UserName');
  FUserName.FieldLabel := _('User Name');
  FUserName.AllowBlank := False;
  FUserName.Width := 136;
  FUserName.EnableKeyEvents := True;

  FPassword := TExtFormTextField.AddTo(FFormPanel.Items);
  FPassword.Name := 'Password';
  FPassword.Value := Session.Config.Authenticator.AuthData.GetString('Password');
  FPassword.FieldLabel := _('Password');
  FPassword.InputType := itPassword;
  FPassword.AllowBlank := False;
  FPassword.Width := 136;
  FPassword.EnableKeyEvents := True;

  FUserName.On('keyup', JSFunction(GetEnableButtonJS));
  FPassword.On('keyup', JSFunction(GetEnableButtonJS));
  FUserName.On('specialkey', JSFunction('field, e', GetSubmitJS));
  FPassword.On('specialkey', JSFunction('field, e', GetSubmitJS));

  FButton.Handler := Ajax(DoLogin, ['Dummy', FStatusBar.ShowBusy,
    'UserName', FUserName.GetValue, 'Password', FPassword.GetValue]);

  FButton.Disabled := (FUserName.Value = '') or (FPassword.Value = '');

  if (FUserName.Value <> '') and (FPassword.Value = '') then
    FPassword.Focus(False, 500)
  else
    FUserName.Focus(False, 500);
end;

end.


