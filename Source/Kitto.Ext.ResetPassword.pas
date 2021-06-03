{-------------------------------------------------------------------------------
   Copyright 2018 Ethea S.r.l.

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

unit Kitto.Ext.ResetPassword;

{$I Kitto.Defines.inc}

interface

uses
  SysUtils
  , Ext.Form
  , Kitto.Ext.Base
  , Kitto.Ext.Panel
  ;

type
  // Asks for an e-mail address and calls the current authenticator's ResetPassword method.
  TKExtResetPassword = class(TKExtPanelControllerBase)
  private
    FUserName: TExtFormTextField;
    FEmailAddress: TExtFormTextField;
    FSendButton: TKExtButton;
    FStatusBar: TKExtStatusBar;
    function GetEnableButtonJS: string;
    function GetSubmitJS: string;
  strict protected
    procedure DoDisplay; override;
  public
  //published
    procedure DoSend;
  end;

implementation

uses
  EF.Localization
  , EF.Tree
  , Ext.Base
  , Kitto.Auth
  , Kitto.Config.Defaults
  , Kitto.Web.Application
  , Kitto.Web.Request
  , Kitto.Web.Response
  , Kitto.JS.Controller
  ;

{ TKExtResetPassword }

procedure TKExtResetPassword.DoDisplay;
begin
  inherited;
  // Needed to force the controller to display as a modal window.
  SetContainer(nil);
  DisplayMode := 'Modal';
  Closable := True;
  Width := 400;
  Height := 250;
  Closable := True;

  Layout := 'form';
  BodyPadding := Format('%0:dpx 0 0 0', [TKDefaults.GetSingleSpacing]);

  FUserName := TExtFormTextField.CreateAndAddToArray(Items);
  FUserName.Name := 'UserName';
  FUserName.FieldLabel := _('User Name');
  FUserName.AllowBlank := False;
  FUserName.EnableKeyEvents := True;
  FUserName.SelectOnFocus := True;
  FUserName.WidthString := '100%';

  FEmailAddress := TExtFormTextField.CreateAndAddToArray(Items);
  FEmailAddress.Name := 'EmailAddress';
  FEmailAddress.FieldLabel := _('Email address');
  FEmailAddress.AllowBlank := False;
  FEmailAddress.EnableKeyEvents := True;
  FEmailAddress.SelectOnFocus := True;
  FEmailAddress.WidthString := '100%';

  FStatusBar := TKExtStatusBar.Create(Self);
  FStatusBar.DefaultText := '';
  FStatusBar.BusyText := _('Generating new password...');
  Bbar := FStatusBar;

  FSendButton := TKExtButton.CreateAndAddToArray(FStatusBar.Items);
  FSendButton.SetIconAndScale('email_go', 'medium');
  FSendButton.Text := _('Send');

  FEmailAddress.On('specialkey', GenerateAnonymousFunction('field, e', GetSubmitJS));

  TKWebResponse.Current.Items.ExecuteJSCode(Self, Format(
    '%s.enableTask = Ext.TaskManager.start({ ' + sLineBreak +
    '  run: function() {' + GetEnableButtonJS + '},' + sLineBreak +
    '  interval: 500});', [JSName]));
  On('beforedestroy', GenerateAnonymousFunction(Format('Ext.TaskManager.stop(%s.enableTask);', [JSName])));

  FSendButton.Handler := TKWebResponse.Current.Items.AjaxCallMethod(Self).SetMethod(DoSend)
    .AddParam('Dummy', FStatusBar.ShowBusy)
    .AddParam('UserName', FUserName.GetValue)
    .AddParam('EmailAddress', FEmailAddress.GetValue)
    .AsFunction;
  FSendButton.Disabled := (FEmailAddress.Value = '');
  FUserName.Focus(False, 750);
end;

procedure TKExtResetPassword.DoSend;
var
  LParams: TEFNode;
begin
  LParams := TEFNode.Create;
  try
    LParams.SetString('UserName', TKWebRequest.Current.GetQueryField('UserName'));
    LParams.SetString('EmailAddress', TKWebRequest.Current.GetQueryField('EmailAddress'));
    try
      TKAuthenticator.Current.ResetPassword(LParams);
      ExtMessageBox.ShowMessage(emtInfo, _('Reset Password'), _('A new temporary password was generated and sent to the specified e-mail address.'));
      Close;
    except
      on E: Exception do
      begin
        FStatusBar.SetErrorStatus(E.Message);
        FEmailAddress.Focus(False, 750);
      end;
    end;
  finally
    FreeAndNil(LParams);
  end;
end;

function TKExtResetPassword.GetEnableButtonJS: string;
begin
  Result := Format(
    '%0:s.setDisabled((%1:s.getValue() == "") || !Ext.form.VTypes.email(%1:s.getValue()));',
    [FSendButton.JSName, FEmailAddress.JSName]);
end;

function TKExtResetPassword.GetSubmitJS: string;
begin
  Result := Format(
    'if (e.getKey() == 13 && (%0:s.getValue() != "")) %1:s.handler.call(%1:s.scope, %1:s);',
    [FEmailAddress.JSName, FSendButton.JSName]);
end;

initialization
  TJSControllerRegistry.Instance.RegisterClass('ResetPassword', TKExtResetPassword);

finalization
  TJSControllerRegistry.Instance.UnregisterClass('ResetPassword');

end.
