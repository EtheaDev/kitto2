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
  Kitto.Ext.Base, Kitto.Ext.HtmlPanel;


type
  TKExtOnLogin = procedure of object;

  TKExtLoginWindow = class(TKExtWindowControllerBase)
  private
    FUseLanguageSelector: Boolean;
    FHtmlPanel: TKExtHtmlPanelController;
    FUserName: TExtFormTextField;
    FPassword: TExtFormTextField;
    FLanguage: TExtFormComboBox;
    FLoginButton: TKExtButton;
    FOnLogin: TKExtOnLogin;
    FStatusBar: TKExtStatusBar;
    FFormPanel: TExtFormFormPanel;
    function GetHtmlPanel: string;
    function GetHtmlPanelWidth: integer;
    function GetHtmlPanelHeight: integer;
    function GetLabelWidth: integer;
  protected
    procedure InitDefaults; override;
  public
    property OnLogin: TKExtOnLogin read FOnLogin write FOnLogin;
    property HtmlPanel: string read GetHtmlPanel;
    property HtmlPanelWidth: integer read GetHtmlPanelWidth;
    property HtmlPanelHeight: integer read GetHtmlPanelHeight;
    property LabelWidth: integer read GetLabelWidth;

    class function Authenticate(const ASession: TExtSession): Boolean;
  published
    procedure DoLogin;
  end;

implementation

uses
  SysUtils, Math,
  ExtPascalUtils,
  EF.Classes, EF.Localization, EF.Tree, EF.Macros,
  Kitto.Types, Kitto.Ext.Session;

{ TKExtLoginWindow }

procedure TKExtLoginWindow.DoLogin;
begin
  if Authenticate(Session) then
  begin
    Close;
    if Assigned(FOnLogin) then
      FOnLogin;
  end
  else
  begin
    FStatusBar.SetErrorStatus(_('Invalid login.'));
    FPassword.Focus(False, 750);
  end;
end;

function TKExtLoginWindow.GetLabelWidth: integer;
begin
  Result := Session.Config.Config.GetInteger('LoginWindowAppearance/LabelWidth');
end;

function TKExtLoginWindow.GetHtmlPanelHeight: integer;
begin
  Result := Session.Config.Config.GetInteger('LoginWindowAppearance/HtmlPanelHeight');
end;

function TKExtLoginWindow.GetHtmlPanel: string;
begin
  Result := Session.Config.Config.GetString('LoginWindowAppearance/HtmlPanel');
end;

function TKExtLoginWindow.GetHtmlPanelWidth: integer;
begin
  Result := Session.Config.Config.GetInteger('LoginWindowAppearance/HtmlPanelWidth');
end;

class function TKExtLoginWindow.Authenticate(const ASession: TExtSession): Boolean;
var
  LAuthData: TEFNode;
  LSession: TKExtSession;
  LUserName: string;
  LPassword: string;
begin
  Assert(ASession is TKExtSession);

  LSession := TKExtSession(ASession);
  if LSession.Config.Authenticator.IsAuthenticated then
    Result := True
  else
  begin
    LAuthData := TEFNode.Create;
    try
      LSession.Config.Authenticator.DefineAuthData(LAuthData);
      LUserName := LSession.Query['UserName'];
      if LUserName <> '' then
        LAuthData.SetString('UserName', LUserName);
      LPassword := LSession.Query['Password'];
      if LSession.Query['Password'] <> '' then
        LAuthData.SetString('Password', LPassword);
      Result := LSession.Config.Authenticator.Authenticate(LAuthData);
    finally
      LAuthData.Free;
    end;
  end;
end;

procedure TKExtLoginWindow.InitDefaults;
var
  LWidth, LHeight, LLabelWidth, LEditWidth: integer;

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

begin
  inherited;
  FUseLanguageSelector := Session.Config.LanguagePerSession;
  LWidth := Max(HtmlPanelWidth, 228);
  LHeight := Max(HtmlPanelHeight, 0);
  LLabelWidth := Max(LabelWidth, 80);
  LEditWidth := Max(LWidth - LLabelWidth - 10, 136);

  Title := _(Session.Config.AppTitle);
  Width := LWidth + 20;
  if FUseLanguageSelector then
    Height := LHeight + (30 * 3) + 66
  else
    Height := LHeight + (30 * 2) + 66;
  Closable := False;
  Resizable := False;

  FStatusBar := TKExtStatusBar.Create(Self);
  FStatusBar.DefaultText := '';
  FStatusBar.BusyText := _('Logging in...');

  FFormPanel := TExtFormFormPanel.CreateAndAddTo(Items);
  FFormPanel.Region := rgCenter;
  FFormPanel.LabelWidth := LLabelWidth;
  FFormPanel.Border := False;
  FFormPanel.BodyStyle := SetPaddings(5, 5);
  FFormPanel.Frame := False;
  FFormPanel.MonitorValid := True;
  FFormPanel.Bbar := FStatusBar;

  FLoginButton := TKExtButton.CreateAndAddTo(FStatusBar.Items);
  FLoginButton.SetIconAndScale('login', 'medium');
  FLoginButton.Text := _('Login');

  if HtmlPanel <> '' then
  begin
    FHtmlPanel := TKExtHtmlPanelController.CreateAndAddTo(FFormPanel.Items);
    FHtmlPanel.Html := TEFMacroExpansionEngine.Instance.Expand(HtmlPanel);
    FHtmlPanel.Border := False;
    FHtmlPanel.Width := HtmlPanelWidth;
    FHtmlPanel.Height := HtmlPanelHeight;
    //Create a separator of 4 pixels
    with TExtBoxComponent.CreateAndAddTo(FFormPanel.Items) do
    begin
      Height := 4;
    end;
  end;

  FUserName := TExtFormTextField.CreateAndAddTo(FFormPanel.Items);
  FUserName.Name := 'UserName';
  FUserName.Value := Session.Config.Authenticator.AuthData.GetExpandedString('UserName');
  FUserName.FieldLabel := _('User Name');
  FUserName.AllowBlank := False;
  FUserName.Width := LEditWidth;
  FUserName.EnableKeyEvents := True;
  FUserName.SelectOnFocus := True;

  FPassword := TExtFormTextField.CreateAndAddTo(FFormPanel.Items);
  FPassword.Name := 'Password';
  FPassword.Value := Session.Config.Authenticator.AuthData.GetExpandedString('Password');
  FPassword.FieldLabel := _('Password');
  FPassword.InputType := itPassword;
  FPassword.AllowBlank := False;
  FPassword.Width := LEditWidth;
  FPassword.EnableKeyEvents := True;
  FPassword.SelectOnFocus := True;

  FUserName.On('keyup', JSFunction(GetEnableButtonJS));
  FPassword.On('keyup', JSFunction(GetEnableButtonJS));
  FUserName.On('specialkey', JSFunction('field, e', GetSubmitJS));
  FPassword.On('specialkey', JSFunction('field, e', GetSubmitJS));

  if FUseLanguageSelector then
  begin
    FLanguage := TExtFormComboBox.CreateAndAddTo(FFormPanel.Items);
    FLanguage.StoreArray := JSArray('["it", "Italiano"], ["en", "English"]');
    FLanguage.HiddenName := 'Language';
    FLanguage.Value := Session.Config.Authenticator.AuthData.GetExpandedString('Language');
    FLanguage.FieldLabel := _('Language');
    FLanguage.Width := LEditWidth;
    //FLanguage.EnableKeyEvents := True;
    //FLanguage.SelectOnFocus := True;
    FLanguage.ForceSelection := True;
    FLanguage.TriggerAction := 'all'; // Disable filtering list items based on current value.
  end
  else
    FLanguage := nil;

  if Assigned(FLanguage) then
    FLoginButton.Handler := Ajax(DoLogin, ['Dummy', FStatusBar.ShowBusy,
      'UserName', FUserName.GetValue, 'Password', FPassword.GetValue, 'Language', FLanguage.GetValue])
  else
    FLoginButton.Handler := Ajax(DoLogin, ['Dummy', FStatusBar.ShowBusy,
      'UserName', FUserName.GetValue, 'Password', FPassword.GetValue]);

  if Assigned(FLanguage) then
    FLoginButton.Disabled := (FUserName.Value = '') or (FPassword.Value = '') or (FLanguage.Value = '')
  else
    FLoginButton.Disabled := (FUserName.Value = '') or (FPassword.Value = '');

  if (FUserName.Value <> '') and (FPassword.Value = '') then
    FPassword.Focus(False, 750)
  else
    FUserName.Focus(False, 750);
end;

end.
