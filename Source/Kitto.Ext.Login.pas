{-------------------------------------------------------------------------------
   Copyright 2012-2018 Ethea S.r.l.

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
  SysUtils
  , EF.Tree
  , Ext.Base
  , Ext.Form
  , Kitto.Ext.Base
  , Kitto.Ext.BorderPanel
  ;

type
  // Utility class uses in login controllers. A form panel with standard login
  // controls and logic. It can be embedded in a login window, viewport or other
  // container for flexible login page appearance.
  TKExtLoginFormPanel = class(TExtFormFormPanel)
  private
    FUserName: TExtFormTextField;
    FPassword: TExtFormTextField;
    FLanguage: TExtFormComboBox;
    FLocalStorageEnabled: TExtFormCheckbox;
    FResetPasswordLink: TExtBoxComponent;
    FLoginButton: TKExtButton;
    FStatusBar: TKExtStatusBar;
    FLocalStorageMode: string;
    FLocalStorageAskUser: Boolean;
    FAfterLogin: TProc;
    FResetPassword: TEFNode;
    function GetEnableButtonJS: string;
    function GetSubmitJS: string;
    function GetLocalStorageSaveJSCode(const AMode: string; const AAskUser: Boolean): string;
    function GetLocalStorageRetrieveJSCode(const AMode: string; const AAutoLogin: Boolean): string;
  protected
    procedure InitDefaults; override;
  public
    property AfterLogin: TProc read FAfterLogin write FAfterLogin;
    procedure Display(const AEditWidth: Integer; const AConfig: TEFNode; var ACurrentHeight: Integer);
  //published
    procedure DoLogin;
    procedure DoResetPassword;
  end;

  // A login window, suitable as a stand-alone login interface.
  TKExtLoginWindow = class(TKExtWindowControllerBase)
  strict protected
    procedure DoDisplay; override;
  end;

  // A login panel, suitable for embedding into HTML code.
  // requires the ContainerElementId config property to be set,
  // otherwise it is not displayed.
  TKExtLoginPanel = class(TKExtPanelControllerBase)
  strict protected
    procedure DoDisplay; override;
  end;

implementation

uses
  Math
  , EF.Classes
  , EF.Localization
  , EF.Macros
  , Kitto.JS
  , Kitto.Types
  , Kitto.Utils
  , Kitto.Web.Application
  , Kitto.Web.Request
  , Kitto.Web.Response
  , Kitto.Web.Session
  , Kitto.Ext.Controller
  ;

{ TKExtLoginWindow }

procedure TKExtLoginWindow.DoDisplay;
const
  STANDARD_WIDTH = 380;
  STANDARD_HEIGHT = 160;
var
  LBorderPanel: TKExtBorderPanelController;
  LFormPanel: TKExtLoginFormPanel;
  LWidth, LHeight, LLabelWidth, LEditWidth: Integer;
  LFormPanelBodyStyle: string;
  LTitle: TEFNode;

  function GetHorizontalMargin: Integer;
  begin
    if Maximized then
      Result := TKWebSession.Current.ViewportWidth div 4
    else
      Result := 20;
  end;

begin
  Draggable := View.GetBoolean('Controller/Movable', False);
  if Maximized then
    LWidth := TKWebSession.Current.ViewportWidth
  else
    LWidth := Max(Config.GetInteger('ExtraWidth'), STANDARD_WIDTH);
  LHeight := Max(Config.GetInteger('ExtraHeight'), 0) + STANDARD_HEIGHT;

  if Maximized then
  begin
    LLabelWidth := Trunc(TKWebSession.Current.ViewportWidth * 0.4);
    LEditWidth := Trunc(TKWebSession.Current.ViewportWidth * 0.6) - GetHorizontalMargin;
  end
  else
  begin
    LLabelWidth := Max(Config.GetInteger('LabelWidth'), 100);
    LEditWidth := Max(LWidth - LLabelWidth - GetHorizontalMargin * 2, 96);
  end;
  if not Maximized then
    Width := LWidth;

  LTitle := Config.FindNode('Title');
  if Assigned(LTitle) then
    Title := _(LTitle.AsExpandedString)
  else
    Title := _(TKWebApplication.Current.Config.AppTitle);
  Closable := False;
  Resizable := False;

  LBorderPanel := TKExtBorderPanelController.CreateAndAddToArray(Items);
  LBorderPanel.Config.Assign(Config.FindNode('BorderPanel'));
  //FBorderPanel.Border := False;
  LBorderPanel.Frame := False;
  LBorderPanel.View := View;
  LBorderPanel.Display;

  LFormPanel := TKExtLoginFormPanel.CreateAndAddToArray(LBorderPanel.Items);
  LFormPanel.Region := rgCenter;
  LFormPanel.LabelWidth := LLabelWidth;
  LFormPanelBodyStyle := Config.GetString('FormPanel/BodyStyle');
  if LFormPanelBodyStyle <> '' then
    LFormPanel.BodyStyle := LFormPanelBodyStyle;
  LFormPanel.AfterLogin :=
    procedure
    begin
      Close;
      NotifyObservers('LoggedIn');
    end;
  LFormPanel.Display(LEditWidth, Config, LHeight);
  Height := LHeight;
  inherited;
end;

{ TKExtLoginPanel }

procedure TKExtLoginPanel.DoDisplay;
var
  LDummyHeight: Integer;
  LFormPanel: TKExtLoginFormPanel;
  LFormPanelBodyStyle: string;
begin
  inherited;
  Frame := False;
  Border := False;
  Layout := lyFit;
  Title := Config.GetString('Title');
  Width := Config.GetInteger('Width', 300);
  Height := Config.GetInteger('Height', 160);
  Padding := Config.GetString('Padding', '10px');

  LFormPanel := TKExtLoginFormPanel.CreateAndAddToArray(Items);
  LFormPanel.LabelWidth := Config.GetInteger('FormPanel/LabelWidth', 150);
  LFormPanelBodyStyle := Config.GetString('FormPanel/BodyStyle');
  if LFormPanelBodyStyle <> '' then
    LFormPanel.BodyStyle := LFormPanelBodyStyle;
  LFormPanel.AfterLogin :=
    procedure
    begin
      Delete;
      NotifyObservers('LoggedIn');
    end;
  LDummyHeight := 0;
  LFormPanel.Display(Config.GetInteger('FormPanel/EditWidth', 150), Config.FindNode('LocalStorage'), LDummyHeight);
  inherited;
end;

{ TKExtLoginFormPanel }

function TKExtLoginFormPanel.GetEnableButtonJS: string;
begin
  Result := Format(
    '%s.setDisabled(%s.getValue() == "" || %s.getValue() == "");',
    [FLoginButton.JSName, FUserName.JSName, FPassword.JSName]);
end;

function TKExtLoginFormPanel.GetSubmitJS: string;
begin
  Result := Format(
    // For some reason != does not survive rendering.
    'if (e.getKey() == 13 && !(%s.getValue() == "") && !(%s.getValue() == "")) %s.handler.call(%s.scope, %s);',
    [FUserName.JSName, FPassword.JSName, FLoginButton.JSName, FLoginButton.JSName, FLoginButton.JSName]);
end;

procedure TKExtLoginFormPanel.Display(const AEditWidth: Integer; const AConfig: TEFNode; var ACurrentHeight: Integer);
const
  CONTROL_HEIGHT = 32;
var
  LLocalStorageAskUserDefault: Boolean;
  LLocalStorageAutoLogin: Boolean;
  LLocalStorageOptions: TEFNode;
  LLoginHandler: TJSAjaxCall;
  LResetPasswordClickCode: string;
begin
  FStatusBar := TKExtStatusBar.Create(Self);
  FStatusBar.DefaultText := '';
  FStatusBar.BusyText := _('Logging in...');
  Bbar := FStatusBar;

  FLoginButton := TKExtButton.CreateAndAddToArray(FStatusBar.Items);
  FLoginButton.SetIconAndScale('login', 'medium');
  FLoginButton.Text := _('Login');

  with TExtBoxComponent.CreateAndAddToArray(Items) do
    Height := 20;

  FUserName := TExtFormTextField.CreateAndAddToArray(Items);
  FUserName.Name := 'UserName';
  FUserName.Value := TKWebSession.Current.AuthData.GetExpandedString('UserName');
  FUserName.FieldLabel := _('User Name');
  FUserName.AllowBlank := False;
  FUserName.EnableKeyEvents := True;
  FUserName.SelectOnFocus := True;
  FUserName.Width := AEditWidth + LabelWidth;
  Inc(ACurrentHeight, CONTROL_HEIGHT);

  FPassword := TExtFormTextField.CreateAndAddToArray(Items);
  FPassword.Name := 'Password';
  FPassword.Value := TKWebSession.Current.AuthData.GetExpandedString('Password');
  FPassword.FieldLabel := _('Password');
  FPassword.InputType := itPassword;
  FPassword.AllowBlank := False;
  FPassword.EnableKeyEvents := True;
  FPassword.SelectOnFocus := True;
  FPassword.Width := AEditWidth + LabelWidth;
  Inc(ACurrentHeight, CONTROL_HEIGHT);

  FUserName.On('specialkey', GenerateAnonymousFunction('field, e', GetSubmitJS));
  FPassword.On('specialkey', GenerateAnonymousFunction('field, e', GetSubmitJS));

  TKWebResponse.Current.Items.ExecuteJSCode(Self, Format(
    '%s.enableTask = Ext.TaskManager.start({ ' + sLineBreak +
    '  run: function() {' + GetEnableButtonJS + '},' + sLineBreak +
    '  interval: 500});', [JSName]));

  FResetPassword := AConfig.FindNode('ResetPassword');
  if Assigned(FResetPassword) and FResetPassword.AsBoolean then
  begin
    FResetPasswordLink := TExtBoxComponent.CreateAndAddToArray(Items);
    LResetPasswordClickCode := GetJSCode(
      procedure
      begin
        TKWebResponse.Current.Items.AjaxCallMethod(Self).SetMethod(DoResetPassword);
      end);
    FResetPasswordLink.Html := Format(
      '<div style="text-align:right;"><a href="#" onclick="%s">%s</a></div>',
      [HTMLEncode(LResetPasswordClickCode), HTMLEncode(_('Password forgotten?'))]);
    FResetPasswordLink.Width := AEditWidth + LabelWidth;
    Inc(ACurrentHeight, CONTROL_HEIGHT);
  end
  else
    FResetPasswordLink := nil;

  if TKWebApplication.Current.Config.LanguagePerSession then
  begin
    FLanguage := TExtFormComboBox.CreateAndAddToArray(Items);
    // Don't call JSArray on the window, as it will generate a dependency cycle.
	{ TODO: Verify if this still holds true }
    FLanguage.StoreArray := FLanguage.JSArray('["it", "Italiano"], ["en", "English"]');
    FLanguage.HiddenName := 'Language';
    FLanguage.Value := TKWebSession.Current.AuthData.GetExpandedString('Language');
    if FLanguage.Value = '' then
      FLanguage.Value := TKWebApplication.Current.Config.Config.GetString('LanguageId');
    FLanguage.FieldLabel := _('Language');
    //FLanguage.EnableKeyEvents := True;
    //FLanguage.SelectOnFocus := True;
    FLanguage.ForceSelection := True;
    FLanguage.TriggerAction := 'all'; // Disable filtering list items based on current value.
    FLanguage.Width := AEditWidth + LabelWidth;
    Inc(ACurrentHeight, CONTROL_HEIGHT);
  end
  else
    FLanguage := nil;

  LLocalStorageOptions := AConfig.FindNode('LocalStorage');
  if Assigned(LLocalStorageOptions) then
  begin
    FLocalStorageMode := LLocalStorageOptions.GetString('Mode');
    FLocalStorageAskUser := LLocalStorageOptions.GetBoolean('AskUser');
    LLocalStorageAskUserDefault := LLocalStorageOptions.GetBoolean('AskUser/Default', True);
    LLocalStorageAutoLogin := LLocalStorageOptions.GetBoolean('AutoLogin', False);
  end
  else
  begin
    FLocalStorageMode := '';
    FLocalStorageAskUser := False;
    LLocalStorageAskUserDefault := False;
    LLocalStorageAutoLogin := False;
  end;

  if (FLocalStorageMode <> '') and FLocalStorageAskUser then
  begin
    FLocalStorageEnabled := TExtFormCheckbox.CreateAndAddToArray(Items);
    FLocalStorageEnabled.Name := 'LocalStorageEnabled';
    FLocalStorageEnabled.Checked := LLocalStorageAskUserDefault;
    if SameText(FLocalStorageMode, 'Password') then
      FLocalStorageEnabled.FieldLabel := _('Remember Credentials')
    else
      FLocalStorageEnabled.FieldLabel := _('Remember User Name');
    Inc(ACurrentHeight, CONTROL_HEIGHT);
  end
  else
    FLocalStorageEnabled := nil;

  LLoginHandler := TKWebResponse.Current.Items.AjaxCallMethod(Self).SetMethod(DoLogin)
    .AddParam('Dummy', FStatusBar.ShowBusy)
    .AddParam('UserName', FUserName.GetValue)
    .AddParam('Password', FPassword.GetValue)
    .AddParam('UserName', FUserName.GetValue);
  if Assigned(FLanguage) then
    LLoginHandler.AddParam('Language', FLanguage.GetValue);
  if Assigned(FLocalStorageEnabled) then
    LLoginHandler.AddParam('LocalStorageEnabled', FLocalStorageEnabled.GetValue);
  FLoginButton.Handler := LLoginHandler.AsFunction;

  if Assigned(FLanguage) then
    FLoginButton.Disabled := (FUserName.Value = '') or (FPassword.Value = '') or (FLanguage.Value = '')
  else
    FLoginButton.Disabled := (FUserName.Value = '') or (FPassword.Value = '');

  if (FUserName.Value <> '') and (FPassword.Value = '') then
    FPassword.Focus(False, 750)
  else
    FUserName.Focus(False, 750);

  &On('render', GenerateAnonymousFunction(GetLocalStorageRetrieveJSCode(FLocalStorageMode, LLocalStorageAutoLogin)));
end;

procedure TKExtLoginFormPanel.InitDefaults;
begin
  inherited;
  LabelAlign := laRight;
  Border := False;
  Frame := False;
  AutoScroll := True;
  MonitorValid := True;
end;

procedure TKExtLoginFormPanel.DoLogin;
begin
  if TKWebApplication.Current.Authenticate then
  begin
    TKWebResponse.Current.Items.ExecuteJSCode(Format('Ext.TaskManager.stop(%s.enableTask);', [JSName]));
    TKWebResponse.Current.Items.ExecuteJSCode(GetLocalStorageSaveJSCode(FLocalStorageMode, FLocalStorageAskUser));
    Assert(Assigned(FAfterLogin));
    FAfterLogin();
  end
  else
  begin
    FStatusBar.SetErrorStatus(_('Invalid login.'));
    FPassword.Focus(False, 750);
  end;
end;

procedure TKExtLoginFormPanel.DoResetPassword;
begin
  Assert(Assigned(FResetPassword));

  { TODO : Add a way to open standard/system views without needing to store them in a yaml file.
    Maybe a json definition passed as a string? }
  TKWebApplication.Current.DisplayView('ResetPassword');
end;

function TKExtLoginFormPanel.GetLocalStorageSaveJSCode(const AMode: string; const AAskUser: Boolean): string;

  function IfChecked: string;
  begin
    if Assigned(FLocalStorageEnabled) then
      Result := 'if (' + FLocalStorageEnabled.JSName + '.getValue())'
    else
      Result := 'if (true)';
  end;

  function GetDeleteCode: string;
  begin
    Result := 'delete localStorage.' + TKWebApplication.Current.Config.AppName + '_UserName;' + sLineBreak;
    Result := Result + 'delete localStorage.' + TKWebApplication.Current.Config.AppName + '_Password;' + sLineBreak;
    Result := Result + 'delete localStorage.' + TKWebApplication.Current.Config.AppName + '_LocalStorageEnabled;' + sLineBreak;
  end;

begin
  Result := '';
  if (AMode <> '') then
  begin
    Result := Result + IfChecked + '{';
    if SameText(AMode, 'UserName') or SameText(AMode, 'Password') then
      Result := Result + 'localStorage.' + TKWebApplication.Current.Config.AppName + '_UserName = "' + ParamAsString('UserName') + '";';
    if SameText(AMode, 'Password') then
      Result := Result + 'localStorage.' + TKWebApplication.Current.Config.AppName + '_Password = "' + ParamAsString('Password') + '";';
    if AAskUser then
      Result := Result + 'localStorage.' + TKWebApplication.Current.Config.AppName + '_LocalStorageEnabled = "' + ParamAsString('LocalStorageEnabled') + '";';
    Result := Result + '} else {' + GetDeleteCode + '};';
  end
  else
    Result := GetDeleteCode;
end;

function TKExtLoginFormPanel.GetLocalStorageRetrieveJSCode(const AMode: string; const AAutoLogin: Boolean): string;
begin
  if SameText(AMode, 'UserName') or SameText(AMode, 'Password') then
    Result := Result + 'var u = localStorage.' + TKWebApplication.Current.Config.AppName + '_UserName; if (u) ' + FUserName.JSName + '.setValue(u);';
  if SameText(AMode, 'Password') then
    Result := Result + 'var p = localStorage.' + TKWebApplication.Current.Config.AppName + '_Password; if (p) ' + FPassword.JSName + '.setValue(p);';
  if Assigned(FLocalStorageEnabled) then
    Result := Result + 'var l = localStorage.' + TKWebApplication.Current.Config.AppName + '_LocalStorageEnabled; if (l) ' + FLocalStorageEnabled.JSName + '.setValue(l);';
  if AAutoLogin then
    Result := Result + Format('setTimeout(function(){ %s.getEl().dom.click(); }, 100);', [FLoginButton.JSName]);
end;

initialization
  TKExtControllerRegistry.Instance.RegisterClass('Login', TKExtLoginWindow);
  TKExtControllerRegistry.Instance.RegisterClass('LoginPanel', TKExtLoginPanel);

finalization
  TKExtControllerRegistry.Instance.UnregisterClass('Login');
  TKExtControllerRegistry.Instance.UnregisterClass('LoginPanel');

end.
