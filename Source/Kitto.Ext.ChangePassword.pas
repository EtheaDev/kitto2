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

unit Kitto.Ext.ChangePassword;

{$I Kitto.Defines.inc}

interface

uses
  Ext, ExtForm,
  Kitto.Ext.Base;

type
  TKExtChangePasswordWindow = class(TKExtWindowControllerBase)
  private
    FOldPassword: TExtFormTextField;
    FNewPassword: TExtFormTextField;
    FConfirmNewPassword: TExtFormTextField;
    FButton: TExtButton;
    FStatusBar: TKExtStatusBar;
    FFormPanel: TExtFormFormPanel;
    FOldPasswordHash: string;
    function GetPasswordHash(const AClearPassword: string): string;
  protected
    procedure InitDefaults; override;
  public
    ///	<summary>Returns the display label to use by default when not specified
    ///	at the view or other level. Called through RTTI.</summary>
    class function GetDefaultDisplayLabel: string;

    ///	<summary>Returns the image name to use by default when not specified at
    ///	the view or other level. Called through RTTI.</summary>
    class function GetDefaultImageName: string;
  published
    procedure DoChangePassword;
  end;

implementation

uses
  SysUtils, StrUtils,
  ExtPascalUtils,
  EF.Classes, EF.Localization, EF.Tree, EF.StrUtils,
  Kitto.Types, Kitto.Config,
  Kitto.Ext.Controller, Kitto.Ext.Session;

{ TKExtChangePasswordWindow }

function TKExtChangePasswordWindow.GetPasswordHash(const AClearPassword: string): string;
begin
  if TKConfig.Instance.Authenticator.IsClearPassword then
    Result := AClearPassword
  else
    Result := GetStringHash(AClearPassword);
end;

procedure TKExtChangePasswordWindow.DoChangePassword;
begin
  if GetPasswordHash(Session.Query['OldPassword']) <> FOldPasswordHash then
  begin
    FStatusBar.SetErrorStatus(_('Old Password is wrong.'));
    FOldPassword.Focus(False, 500);
  end
  else if GetPasswordHash(Session.Query['NewPassword']) = FOldPasswordHash then
  begin
    FStatusBar.SetErrorStatus(_('New Password must be different than old password.'));
    FNewPassword.Focus(False, 500);
  end
  else if Session.Query['NewPassword'] <> Session.Query['ConfirmNewPassword'] then
  begin
    FStatusBar.SetErrorStatus(_('Confirm New Password is wrong.'));
    FConfirmNewPassword.Focus(False, 500);
  end
  else
  begin
    try
      TKConfig.Instance.Authenticator.Password := Session.Query['ConfirmNewPassword'];
      Close;
    except
      on E: Exception do
      begin
        FStatusBar.SetErrorStatus(E.Message);
        FNewPassword.Focus(False, 500);
      end;
    end;
  end;
end;

class function TKExtChangePasswordWindow.GetDefaultDisplayLabel: string;
begin
  Result := _('Change Password');
end;

class function TKExtChangePasswordWindow.GetDefaultImageName: string;
begin
  Result := 'password';
end;

procedure TKExtChangePasswordWindow.InitDefaults;

  function ReplaceMacros(const ACode: string): string;
  begin
    Result := ReplaceStr(ACode, '%BUTTON%', FButton.JSName);
    Result := ReplaceStr(Result, '%OLDPW%', FOldPassword.JSName);
    Result := ReplaceStr(Result, '%NEWPW%', FNewPassword.JSName);
    Result := ReplaceStr(Result, '%NEWPW2%', FConfirmNewPassword.JSName);
  end;

  function GetEnableButtonJS: string;
  begin
    Result := ReplaceMacros(
      '%BUTTON%.setDisabled(%OLDPW%.getValue() == "" || %NEWPW%.getValue() == "" ' +
      '|| !(%NEWPW%.getValue() == %NEWPW2%.getValue()));');
  end;

  function GetSubmitJS: string;
  begin
    Result := ReplaceMacros(
      'if (e.getKey() == 13 && !(%OLDPW%.getValue() == "") && !(%NEWPW%.getValue() == "") ' +
      '&& %NEWPW%.getValue() == %NEWPW2%.getValue()) %BUTTON%.handler.call(%BUTTON%.scope, %BUTTON%);');
  end;

begin
  inherited;
  FOldPasswordHash := TKConfig.Instance.Authenticator.Password;

  Modal := True;
  Title := _(Session.Config.AppTitle);
  Width := 316;
  Height := 142;
  Closable := True;
  Resizable := False;

  FStatusBar := TKExtStatusBar.Create;
  FStatusBar.DefaultText := '';
  FStatusBar.BusyText := _('Changing password...');

  FFormPanel := TExtFormFormPanel.AddTo(Items);
  FFormPanel.Region := rgCenter;
  FFormPanel.LabelWidth := 140;
  FFormPanel.Border := False;
  FFormPanel.BodyStyle := SetPaddings(5, 5);
  FFormPanel.Frame := False;
  FFormPanel.MonitorValid := True;
  FFormPanel.Bbar := FStatusBar;

  FButton := TExtButton.AddTo(FStatusBar.Items);
  FButton.Icon := Session.Config.GetImageURL('password');
  FButton.Text := _('Change password');

  FOldPassword := TExtFormTextField.AddTo(FFormPanel.Items);
  FOldPassword.Name := 'OldPassword';
  //FOldPassword.Value := ...
  FOldPassword.FieldLabel := _('Old Password');
  FOldPassword.InputType := itPassword;
  FOldPassword.AllowBlank := False;
  FOldPassword.Width := 136;
  FOldPassword.EnableKeyEvents := True;

  FNewPassword := TExtFormTextField.AddTo(FFormPanel.Items);
  FNewPassword.Name := 'NewPassword';
  //FNewPassword.Value := ...
  FNewPassword.FieldLabel := _('New Password');
  FNewPassword.InputType := itPassword;
  FNewPassword.AllowBlank := False;
  FNewPassword.Width := 136;
  FNewPassword.EnableKeyEvents := True;

  FConfirmNewPassword := TExtFormTextField.AddTo(FFormPanel.Items);
  FConfirmNewPassword.Name := 'ConfirmNewPassword';
  //FConfirmNewPassword.Value := ...
  FConfirmNewPassword.FieldLabel := _('Confirm New Password');
  FConfirmNewPassword.InputType := itPassword;
  FConfirmNewPassword.AllowBlank := False;
  FConfirmNewPassword.Width := 136;
  FConfirmNewPassword.EnableKeyEvents := True;

  FOldPassword.On('keyup', JSFunction(GetEnableButtonJS));
  FNewPassword.On('keyup', JSFunction(GetEnableButtonJS));
  FConfirmNewPassword.On('keyup', JSFunction(GetEnableButtonJS));
  FOldPassword.On('specialkey', JSFunction('field, e', GetSubmitJS));
  FNewPassword.On('specialkey', JSFunction('field, e', GetSubmitJS));
  FConfirmNewPassword.On('specialkey', JSFunction('field, e', GetSubmitJS));

  FButton.Handler := Ajax(DoChangePassword, ['Dummy', FStatusBar.ShowBusy,
    'OldPassword', FOldPassword.GetValue, 'NewPassword', FNewPassword.GetValue,
    'ConfirmNewPassword', FConfirmNewPassword.GetValue]);

  FButton.Disabled := True;

  FOldPassword.Focus(False, 500);
end;

initialization
  TKExtControllerRegistry.Instance.RegisterClass('ChangePassword', TKExtChangePasswordWindow);

finalization
  TKExtControllerRegistry.Instance.UnregisterClass('ChangePassword');

end.

