{-------------------------------------------------------------------------------
   Copyright 2012-2021 Ethea S.r.l.

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
  Ext.Base
  , Ext.Form
  , Kitto.Ext.Base
  , Kitto.Ext.Panel
  ;

type
  TKExtChangePassword = class(TKExtPanelControllerBase)
  private
    FOldPassword: TExtFormTextField;
    FNewPassword: TExtFormTextField;
    FConfirmNewPassword: TExtFormTextField;
    FConfirmButton: TKExtButton;
    FStatusBar: TKExtStatusBar;
    FOldPasswordHash: string;
    function GetPasswordHash(const AClearPassword: string): string;
  strict protected
    procedure DoDisplay; override;
  public
    ///	<summary>
    ///  Returns the display label to use by default when not specified
    ///	 at the view or other level. Called through RTTI.
    /// </summary>
    class function GetDefaultDisplayLabel: string;

    ///	<summary>
    ///  Returns the image name to use by default when not specified at
    ///	 the view or other level. Called through RTTI.
    /// </summary>
    class function GetDefaultImageName: string;
  //published
    procedure DoChangePassword;
  end;

implementation

uses
  SysUtils
  , StrUtils
  , Math
  , EF.Classes
  , EF.Localization
  , EF.Tree
  , EF.StrUtils
  , Kitto.Types
  , Kitto.Config
  , Kitto.Config.Defaults
  , Kitto.Auth
  , Kitto.JS
  , Kitto.JS.Formatting
  , Kitto.Web.Application
  , Kitto.Web.Request
  , Kitto.Web.Response
  , Kitto.JS.Controller
  ;

{ TKExtChangePassword }

function TKExtChangePassword.GetPasswordHash(const AClearPassword: string): string;
begin
  if TKAuthenticator.Current.IsClearPassword then
    Result := AClearPassword
  else
    Result := GetStringHash(AClearPassword);
end;

procedure TKExtChangePassword.DoChangePassword;
begin
  if GetPasswordHash(ParamAsString('OldPassword')) <> FOldPasswordHash then
  begin
    FStatusBar.SetErrorStatus(_('Old Password is wrong.'));
    FOldPassword.Focus(False, 500);
  end
  else if GetPasswordHash(ParamAsString('NewPassword')) = FOldPasswordHash then
  begin
    FStatusBar.SetErrorStatus(_('New Password must be different than old password.'));
    FNewPassword.Focus(False, 500);
  end
  else if ParamAsString('NewPassword') <> ParamAsString('ConfirmNewPassword') then
  begin
    FStatusBar.SetErrorStatus(_('Confirm New Password is wrong.'));
    FConfirmNewPassword.Focus(False, 500);
  end
  else
  begin
    try
      TKAuthenticator.Current.Password := ParamAsString('ConfirmNewPassword');
      Close;
      TKWebApplication.Current.Logout;
    except
      on E: Exception do
      begin
        FStatusBar.SetErrorStatus(E.Message);
        FNewPassword.Focus(False, 500);
      end;
    end;
  end;
end;

procedure TKExtChangePassword.DoDisplay;

  function ReplaceMacros(const ACode: string): string;
  begin
    Result := ReplaceStr(ACode, '%BUTTON%', FConfirmButton.JSName);
    Result := ReplaceStr(Result, '%OLDPW%', FOldPassword.JSName);
    Result := ReplaceStr(Result, '%NEWPW%', FNewPassword.JSName);
    Result := ReplaceStr(Result, '%NEWPW2%', FConfirmNewPassword.JSName);
    Result := ReplaceStr(Result, '%STATUSBAR%', FStatusBar.JSName);
    Result := ReplaceStr(Result, '%CAPS_ON%', _('Caps On'));
  end;

  function GetEnableButtonJS: string;
  begin
    Result := ReplaceMacros(
      '%BUTTON%.setDisabled(%OLDPW%.getValue() == "" || %NEWPW%.getValue() == "" ' +
      '|| !(%NEWPW%.getValue() == %NEWPW2%.getValue()));');
  end;

  function GetCheckCapsLockJS: string;
  begin
(*
    Result := ReplaceMacros(
      'if (event.keyCode !== 13 && event.getModifierState("CapsLock")) ' +
      '{%STATUSBAR%.setText(''%CAPS_ON%''); %STATUSBAR%.setIcon('''');} ' +
      'else {%STATUSBAR%.setText('''');}');
*)
  end;

  function GetSubmitJS: string;
  begin
    Result := ReplaceMacros(
      'if (e.getKey() == 13 && !(%OLDPW%.getValue() == "") && !(%NEWPW%.getValue() == "") ' +
      '&& %NEWPW%.getValue() == %NEWPW2%.getValue()) %BUTTON%.handler.call(%BUTTON%.scope, %BUTTON%);');
  end;

begin
  inherited;
  Title := _('Change password');

  // Needed to force the controller to display as a modal window.
  SetContainer(nil);
  DisplayMode := 'Modal';
  Closable := True;
  Width := 400;
  Height := 250;
  Closable := True;

  Layout := 'form';
  BodyPadding := Format('%dpx 0 0 0', [TKDefaults.GetSingleSpacing]);

  FOldPasswordHash := TKAuthenticator.Current.Password;

  FStatusBar := TKExtStatusBar.Create(Self);
  FStatusBar.DefaultText := '';
  FStatusBar.BusyText := _('Changing password...');
  BBar := FStatusBar;

  FConfirmButton := TKExtButton.CreateAndAddToArray(FStatusBar.Items);
  FConfirmButton.SetIconAndScale('password', 'medium');
  FConfirmButton.Text := _('Change password');

  FOldPassword := TExtFormTextField.CreateAndAddToArray(Items);
  FOldPassword.Name := 'OldPassword';
  FOldPassword.FieldLabel := _('Old Password');
  FOldPassword.InputType := itPassword;
  FOldPassword.AllowBlank := False;
  FOldPassword.WidthString := '100%';
  FOldPassword.EnableKeyEvents := True;

  FNewPassword := TExtFormTextField.CreateAndAddToArray(Items);
  FNewPassword.Name := 'NewPassword';
  FNewPassword.FieldLabel := _('New Password');
  FNewPassword.InputType := itPassword;
  FNewPassword.AllowBlank := False;
  FNewPassword.WidthString := '100%';
  FNewPassword.EnableKeyEvents := True;

  FConfirmNewPassword := TExtFormTextField.CreateAndAddToArray(Items);
  FConfirmNewPassword.Name := 'ConfirmNewPassword';
  FConfirmNewPassword.FieldLabel := _('Confirm New Password');
  FConfirmNewPassword.InputType := itPassword;
  FConfirmNewPassword.AllowBlank := False;
  FConfirmNewPassword.WidthString := '100%';
  FConfirmNewPassword.EnableKeyEvents := True;

  FOldPassword.On('keyup', GenerateAnonymousFunction(GetEnableButtonJS));
  FNewPassword.On('keyup', GenerateAnonymousFunction(GetEnableButtonJS));
  FConfirmNewPassword.On('keyup', GenerateAnonymousFunction(GetEnableButtonJS));

  FOldPassword.On('keydown', GenerateAnonymousFunction(GetCheckCapsLockJS));
  FNewPassword.On('keydown', GenerateAnonymousFunction(GetCheckCapsLockJS));
  FConfirmNewPassword.On('keydown', GenerateAnonymousFunction(GetCheckCapsLockJS));

  FOldPassword.On('specialkey', GenerateAnonymousFunction('field, e', GetSubmitJS));
  FNewPassword.On('specialkey', GenerateAnonymousFunction('field, e', GetSubmitJS));
  FConfirmNewPassword.On('specialkey', GenerateAnonymousFunction('field, e', GetSubmitJS));

  FConfirmButton.Handler := TKWebResponse.Current.Items.AjaxCallMethod(Self).SetMethod(DoChangePassword)
    .AddParam('Dummy', FStatusBar.ShowBusy)
    .AddParam('OldPassword', FOldPassword.GetValue)
    .AddParam('NewPassword', FNewPassword.GetValue)
    .AddParam('ConfirmNewPassword', FConfirmNewPassword.GetValue)
    .AsFunction;

  FConfirmButton.Disabled := True;

  FOldPassword.Focus(False, 500);
end;

class function TKExtChangePassword.GetDefaultDisplayLabel: string;
begin
  Result := _('Change Password');
end;

class function TKExtChangePassword.GetDefaultImageName: string;
begin
  Result := 'password';
end;

initialization
  TJSControllerRegistry.Instance.RegisterClass('ChangePassword', TKExtChangePassword);

finalization
  TJSControllerRegistry.Instance.UnregisterClass('ChangePassword');

end.

