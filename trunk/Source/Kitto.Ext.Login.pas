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
  ExtPascal, Ext, ExtForm,
  Kitto.Ext.Base, Kitto.Ext.BorderPanel;

type
  TKExtLoginWindow = class(TKExtWindowControllerBase)
  private
    FBorderPanel: TKExtBorderPanelController;
    FUserName: TExtFormTextField;
    FPassword: TExtFormTextField;
    FLanguage: TExtFormComboBox;
    FLoginButton: TKExtButton;
    FStatusBar: TKExtStatusBar;
    FFormPanel: TExtFormFormPanel;
    FLocalStorageEnabled: TExtFormCheckbox;
    function GetExtraWidth: Integer;
    function GetExtraHeight: Integer;
    function GetLabelWidth: Integer;
    function GetLocalStorageMode: string;
    function GetLocalStorageSaveJSCode(const ALocalStorageMode: string): string;
    function GetLocalStorageRetrieveJSCode(const ALocalStorageMode: string): string;
    function GetLocalStorageAskUser: Boolean;
    function GetLocalStorageAskUserDefault: Boolean;
    function GetLocalStorageAutoLogin: Boolean;
  strict protected
    procedure DoDisplay; override;
  public
    property ExtraWidth: Integer read GetExtraWidth;
    property ExtraHeight: Integer read GetExtraHeight;
    property LabelWidth: Integer read GetLabelWidth;
    property LocalStorageMode: string read GetLocalStorageMode;
  published
    procedure DoLogin;
  end;

implementation

uses
  SysUtils, Math,
  ExtPascalUtils,
  EF.Classes, EF.Localization, EF.Tree, EF.Macros,
  Kitto.Types,
  Kitto.Ext.Session, Kitto.Ext.Controller;

{ TKExtLoginWindow }

procedure TKExtLoginWindow.DoDisplay;
const
  STANDARD_HEIGHT = 82;
  CONTROL_HEIGHT = 30;
var
  LWidth, LHeight, LLabelWidth, LEditWidth: Integer;
  LUseLanguageSelector: Boolean;
  LFormPanelBodyStyle: string;
  LLocalStorageMode: string;

  function GetEnableButtonJS: string;
  begin
    Result := Format(
      '%s.setDisabled(%s.getValue() == "" || %s.getValue() == "");',
      [FLoginButton.JSName, FUserName.JSName, FPassword.JSName]);
  end;

  function GetSubmitJS: string;
  begin
    Result := Format(
      // For some reason != does not survive rendering.
      'if (e.getKey() == 13 && !(%s.getValue() == "") && !(%s.getValue() == "")) %s.handler.call(%s.scope, %s);',
      [FUserName.JSName, FPassword.JSName, FLoginButton.JSName, FLoginButton.JSName, FLoginButton.JSName]);
  end;

  function GetHorizontalMargin: Integer;
  begin
    if Maximized then
      Result := Session.ViewportWidth div 4
    else
      Result := 20;
  end;

begin
  Maximized := Session.IsMobileBrowser;
  Border := not Maximized;
  if Maximized then
    LWidth := Session.ViewportWidth
  else
    LWidth := Max(ExtraWidth, 236);
  LHeight := Max(ExtraHeight, 0) + STANDARD_HEIGHT;

  if Maximized then
  begin
    LLabelWidth := Trunc(Session.ViewportWidth * 0.4);
    LEditWidth := Trunc(Session.ViewportWidth * 0.6) - GetHorizontalMargin;
  end
  else
  begin
    LLabelWidth := Max(LabelWidth, 100);
    LEditWidth := Max(LWidth - LLabelWidth - GetHorizontalMargin * 2, 96);
  end;
  if not Maximized then
    Width := LWidth;
  LUseLanguageSelector := Session.Config.LanguagePerSession;

  Title := _(Session.Config.AppTitle);
  Closable := False;
  Resizable := False;

  FBorderPanel := TKExtBorderPanelController.CreateAndAddTo(Items);
  FBorderPanel.Config.Assign(Config.FindNode('BorderPanel'));
  //FBorderPanel.Border := False;
  FBorderPanel.Frame := False;
  FBorderPanel.View := View;
  FBorderPanel.Display;

  FStatusBar := TKExtStatusBar.Create(Self);
  FStatusBar.DefaultText := '';
  FStatusBar.BusyText := _('Logging in...');
  Bbar := FStatusBar;

  FFormPanel := TExtFormFormPanel.CreateAndAddTo(FBorderPanel.Items);
  FFormPanel.Region := rgCenter;
  FFormPanel.LabelAlign := laRight;
  FFormPanel.LabelWidth := LLabelWidth;
  FFormPanel.Border := False;
  FFormPanel.Frame := False;
  FFormPanel.AutoScroll := True;
  LFormPanelBodyStyle := Config.GetString('FormPanel/BodyStyle');
  if LFormPanelBodyStyle <> '' then
    FFormPanel.BodyStyle := LFormPanelBodyStyle;
  FFormPanel.MonitorValid := True;

  FLoginButton := TKExtButton.CreateAndAddTo(FStatusBar.Items);
  FLoginButton.SetIconAndScale('login', 'medium');
  FLoginButton.Text := _('Login');

  with TExtBoxComponent.CreateAndAddTo(FFormPanel.Items) do
    Height := 10;

  FUserName := TExtFormTextField.CreateAndAddTo(FFormPanel.Items);
  FUserName.Name := 'UserName';
  FUserName.Value := Session.Config.Authenticator.AuthData.GetExpandedString('UserName');
  FUserName.FieldLabel := _('User Name');
  FUserName.AllowBlank := False;
  FUserName.EnableKeyEvents := True;
  FUserName.SelectOnFocus := True;
  FUserName.Width := LEditWidth;
  Inc(LHeight, CONTROL_HEIGHT);

  FPassword := TExtFormTextField.CreateAndAddTo(FFormPanel.Items);
  FPassword.Name := 'Password';
  FPassword.Value := Session.Config.Authenticator.AuthData.GetExpandedString('Password');
  FPassword.FieldLabel := _('Password');
  FPassword.InputType := itPassword;
  FPassword.AllowBlank := False;
  FPassword.EnableKeyEvents := True;
  FPassword.SelectOnFocus := True;
  FPassword.Width := LEditWidth;
  Inc(LHeight, CONTROL_HEIGHT);

  FUserName.On('specialkey', JSFunction('field, e', GetSubmitJS));
  FPassword.On('specialkey', JSFunction('field, e', GetSubmitJS));

  Session.ResponseItems.ExecuteJSCode(Format(
    '%s.enableTask = Ext.TaskMgr.start({ ' + sLineBreak +
    '  run: function() {' + GetEnableButtonJS + '},' + sLineBreak +
    '  interval: 500});', [JSName]));

  if LUseLanguageSelector then
  begin
    FLanguage := TExtFormComboBox.CreateAndAddTo(FFormPanel.Items);
    FLanguage.StoreArray := JSArray('["it", "Italiano"], ["en", "English"]');
    FLanguage.HiddenName := 'Language';
    FLanguage.Value := Session.Config.Authenticator.AuthData.GetExpandedString('Language');
    if FLanguage.Value = '' then
      FLanguage.Value := Session.Config.Config.GetString('LanguageId');
    FLanguage.FieldLabel := _('Language');
    //FLanguage.EnableKeyEvents := True;
    //FLanguage.SelectOnFocus := True;
    FLanguage.ForceSelection := True;
    FLanguage.TriggerAction := 'all'; // Disable filtering list items based on current value.
    FLanguage.Width := LEditWidth;
    Inc(LHeight, CONTROL_HEIGHT);
  end
  else
    FLanguage := nil;

  LLocalStorageMode := GetLocalStorageMode;
  if (LLocalStorageMode <> '') and GetLocalStorageAskUser then
  begin
    FLocalStorageEnabled := TExtFormCheckbox.CreateAndAddTo(FFormPanel.Items);
    FLocalStorageEnabled.Name := 'LocalStorageEnabled';
    FLocalStorageEnabled.Checked := GetLocalStorageAskUserDefault;
    if SameText(LLocalStorageMode, 'Password') then
      FLocalStorageEnabled.FieldLabel := _('Remember Credentials')
    else
      FLocalStorageEnabled.FieldLabel := _('Remember User Name');
    Inc(LHeight, CONTROL_HEIGHT);
  end
  else
    FLocalStorageEnabled := nil;

  if Assigned(FLanguage) then
  begin
    if Assigned(FLocalStorageEnabled) then
      FLoginButton.Handler := Ajax(DoLogin, ['Dummy', FStatusBar.ShowBusy,
        'UserName', FUserName.GetValue, 'Password', FPassword.GetValue, 'Language', FLanguage.GetValue,
        'LocalStorageEnabled', FLocalStorageEnabled.GetValue])
    else
      FLoginButton.Handler := Ajax(DoLogin, ['Dummy', FStatusBar.ShowBusy,
        'UserName', FUserName.GetValue, 'Password', FPassword.GetValue, 'Language', FLanguage.GetValue]);
  end
  else
  begin
    if Assigned(FLocalStorageEnabled) then
      FLoginButton.Handler := Ajax(DoLogin, ['Dummy', FStatusBar.ShowBusy,
        'UserName', FUserName.GetValue, 'Password', FPassword.GetValue,
        'LocalStorageEnabled', FLocalStorageEnabled.GetValue])
    else
      FLoginButton.Handler := Ajax(DoLogin, ['Dummy', FStatusBar.ShowBusy,
        'UserName', FUserName.GetValue, 'Password', FPassword.GetValue]);
  end;

  if Assigned(FLanguage) then
    FLoginButton.Disabled := (FUserName.Value = '') or (FPassword.Value = '') or (FLanguage.Value = '')
  else
    FLoginButton.Disabled := (FUserName.Value = '') or (FPassword.Value = '');

  if (FUserName.Value <> '') and (FPassword.Value = '') then
    FPassword.Focus(False, 750)
  else
    FUserName.Focus(False, 750);

  Height := LHeight;

  On('render', JSFunction(GetLocalStorageRetrieveJSCode(LocalStorageMode)));
  inherited;
end;

function TKExtLoginWindow.GetLocalStorageSaveJSCode(const ALocalStorageMode: string): string;

  function IfChecked: string;
  begin
    if Assigned(FLocalStorageEnabled) then
      Result := 'if (' + FLocalStorageEnabled.JSName + '.getValue())'
    else
      Result := 'if (true)';
  end;

  function GetDeleteCode: string;
  begin
    Result := 'delete localStorage.' + Session.Config.AppName + '_UserName;' + sLineBreak;
    Result := Result + 'delete localStorage.' + Session.Config.AppName + '_Password;' + sLineBreak;
    Result := Result + 'delete localStorage.' + Session.Config.AppName + '_LocalStorageEnabled;' + sLineBreak;
  end;

begin
  Result := '';
  if (ALocalStorageMode <> '') then
  begin
    Result := Result + IfChecked + '{';
    if SameText(ALocalStorageMode, 'UserName') or SameText(ALocalStorageMode, 'Password') then
      Result := Result + 'localStorage.' + Session.Config.AppName + '_UserName = "' + Session.Query['UserName'] + '";';
    if SameText(ALocalStorageMode, 'Password') then
      Result := Result + 'localStorage.' + Session.Config.AppName + '_Password = "' + Session.Query['Password'] + '";';
    if GetLocalStorageAskUser then
      Result := Result + 'localStorage.' + Session.Config.AppName + '_LocalStorageEnabled = "' + Session.Query['LocalStorageEnabled'] + '";';
    Result := Result + '} else {' + GetDeleteCode + '};';
  end
  else
    Result := GetDeleteCode;
end;

function TKExtLoginWindow.GetLocalStorageMode: string;
begin
  Result := Config.GetString('LocalStorage/Mode');
end;

function TKExtLoginWindow.GetLocalStorageAskUser: Boolean;
begin
  Result := Config.GetBoolean('LocalStorage/AskUser');
end;

function TKExtLoginWindow.GetLocalStorageAskUserDefault: Boolean;
begin
  Result := Config.GetBoolean('LocalStorage/AskUser/Default', True);
end;

function TKExtLoginWindow.GetLocalStorageAutoLogin: Boolean;
begin
  Result := Config.GetBoolean('LocalStorage/AutoLogin', False);
end;

function TKExtLoginWindow.GetLocalStorageRetrieveJSCode(const ALocalStorageMode: string): string;
begin
  if SameText(ALocalStorageMode, 'UserName') or SameText(ALocalStorageMode, 'Password') then
    Result := Result + 'var u = localStorage.' + Session.Config.AppName + '_UserName; if (u) ' + FUserName.JSName + '.setValue(u);';
  if SameText(ALocalStorageMode, 'Password') then
    Result := Result + 'var p = localStorage.' + Session.Config.AppName + '_Password; if (p) ' + FPassword.JSName + '.setValue(p);';
  if Assigned(FLocalStorageEnabled) then
    Result := Result + 'var l = localStorage.' + Session.Config.AppName + '_LocalStorageEnabled; if (l) ' + FLocalStorageEnabled.JSName + '.setValue(l);';
  if GetLocalStorageAutoLogin then
    Result := Result + Format('setTimeout(function(){ %s.getEl().dom.click(); }, 100);', [FLoginButton.JSName]);
end;

procedure TKExtLoginWindow.DoLogin;
begin
  if Session.Authenticate then
  begin
    Session.ResponseItems.ExecuteJSCode(Format('Ext.TaskMgr.stop(%s.enableTask);', [JSName]));
    Session.ResponseItems.ExecuteJSCode(GetLocalStorageSaveJSCode(LocalStorageMode));
    Close;
    NotifyObservers('LoggedIn');
  end
  else
  begin
    FStatusBar.SetErrorStatus(_('Invalid login.'));
    FPassword.Focus(False, 750);
  end;
end;

function TKExtLoginWindow.GetLabelWidth: Integer;
begin
  Result := Config.GetInteger('LabelWidth');
end;

function TKExtLoginWindow.GetExtraHeight: Integer;
begin
  Result := Config.GetInteger('ExtraHeight');
end;

function TKExtLoginWindow.GetExtraWidth: Integer;
begin
  Result := Config.GetInteger('ExtraWidth');
end;

initialization
  TKExtControllerRegistry.Instance.RegisterClass('Login', TKExtLoginWindow);

finalization
  TKExtControllerRegistry.Instance.UnregisterClass('Login');

end.
