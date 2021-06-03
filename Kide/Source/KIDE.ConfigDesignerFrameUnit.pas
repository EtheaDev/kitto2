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
unit KIDE.ConfigDesignerFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, KIDE.TreeDesignerFrameUnit, Vcl.Samples.Spin,
  KIDE.BaseFrameUnit, KIDE.CodeEditorFrameUnit, Vcl.ExtCtrls, Vcl.Tabs,
  System.Actions, Vcl.ActnList, Vcl.ComCtrls, Vcl.ToolWin, Vcl.StdCtrls,
  EF.Tree, KIDE.NodeDesignerFrameUnit,
  KIDE.EditNodeBaseFrameUnit;

type
  TConfigDesignerFrame = class(TTreeDesignerFrame)
    EditorPageControl: TPageControl;
    MainTabSheet: TTabSheet;
    DatabasesTabSheet: TTabSheet;
    AuthTabSheet: TTabSheet;
    LogTabSheet: TTabSheet;
    TextFileGroupBox: TGroupBox;
    _Log_TextFile_FileName: TLabeledEdit;
    LogPanel: TPanel;
    LevelLabel: TLabel;
    _Log_Level: TSpinEdit;
    AccessControlTabSheet: TTabSheet;
    ControllerScrollBox: TScrollBox;
    ControllerAutoscrollPanel: TPanel;
    ApplicationGroupBox: TGroupBox;
    LanguageIdLabel: TLabel;
    CharSetLabel: TLabel;
    _AppTitle: TLabeledEdit;
    LanguageIdComboBox: TComboBox;
    CharSetComboBox: TComboBox;
    _HomeView: TLabeledEdit;
    UserFormatsGroupBox: TGroupBox;
    _UserFormats_Time: TLabeledEdit;
    _UserFormats_Date: TLabeledEdit;
    ServerGroupBox: TGroupBox;
    SessionTimeoutLabel: TLabel;
    PortLabel: TLabel;
    _Server_Port: TSpinEdit;
    _Server_SessionTimeout: TSpinEdit;
    ExtGroupBox: TGroupBox;
    ThemeLabel: TLabel;
    AjaxTimeoutLabel: TLabel;
    _ExtJS_Path: TLabeledEdit;
    _ExtJS_Theme: TComboBox;
    _ExtJS_AjaxTimeout: TSpinEdit;
    _FOPEnginePath: TLabeledEdit;
    _JavascriptLibraries: TLabeledEdit;
    _LanguagePerSession: TCheckBox;
    TopPanel: TPanel;
    _DatabaseRouter: TLabeledEdit;
    _DefaultDatabaseName: TLabeledEdit;
    DatabasesPanel: TPanel;
    DatabasesLabel: TLabel;
    EmailTabSheet: TTabSheet;
    LoginTabSheet: TTabSheet;
    _Log_TextFile_IsEnabled: TCheckBox;
    DefaultsTabSheet: TTabSheet;
    ThreadPoolSizeLabel: TLabel;
    _Server_ThreadPoolSize: TSpinEdit;
    procedure EditorPageControlChange(Sender: TObject);
  private
  protected
    class function SuitsTree(const ATree: TEFTree): Boolean; override;
    procedure CleanupDefaultsToEditNode; override;
    procedure UpdateDesignPanel(const AForce: Boolean); override;
    procedure DesignPanelToEditNode; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Init(const ANode: TEFTree); override;
  end;

implementation

{$R *.dfm}

uses
  KIDE.Config,
  EF.Classes, KIDE.Utils,
  Kitto.Config,
  //Embedded forms
  KIDE.ConfigAuthNodeFrameUnit, KIDE.ConfigDatabasesNodeFrameUnit,
  KIDE.ConfigAccessControlNodeFrameUnit, KIDE.ConfigEmailNodeFrameUnit,
  KIDE.LoginViewDesignerFrameUnit, KIDE.ConfigDefaultsNodeFrameUnit;

{ TConfigDesignerFrame }

procedure TConfigDesignerFrame.CleanupDefaultsToEditNode;
begin
  inherited;
  CleanupTextNode('JavascriptLibraries');
  CleanupTextNode('UserFormats/Time');
  CleanupTextNode('UserFormats/Date');
  CleanupOrphanNode('UserFormats');
  CleanupIntegerNode('Server/Port', 8080);
  CleanupIntegerNode('Server/SessionTimeout', 10);
  CleanupIntegerNode('Server/ThreadPoolSize', 20);
  CleanupOrphanNode('Server');
  CleanupTextNode('ExtJS/Theme', 'triton');
  CleanupIntegerNode('ExtJS/AjaxTimeout', 30000);
  CleanupTextNode('ExtJS/Path');
  CleanupOrphanNode('ExtJS');
  CleanupBooleanNode('Log/TextFile/IsEnabled');
  CleanupIntegerNode('Log/Level');
  CleanupTextNode('Log/TextFile/FileName');
  CleanupOrphanNode('Log/TextFile');
  CleanupOrphanNode('Log');
  CleanupTextNode('FOPEnginePath');
  CleanupTextNode('HomeView', 'Home');
  CleanupTextNode('DefaultDatabaseName', 'Main');
  CleanupTextNode('DatabaseRouter');
  CleanupBooleanNode('LanguagePerSession', False);
  CleanupTextNode('LanguageId');
  CleanupOrphanNode('Auth');
  CleanupOrphanNode('Databases');
  CleanupOrphanNode('AccessControl');
  CleanupOrphanNode('Email/SMTP/Default');
  CleanupOrphanNode('Email/SMTP');
  CleanupOrphanNode('Email');
  CleanupOrphanNode('Login');
  CleanupOrphanNode('Defaults/Grid');
  CleanupOrphanNode('Defaults/Window');
  CleanupOrphanNode('Defaults');
end;

constructor TConfigDesignerFrame.Create(AOwner: TComponent);
begin
  inherited;
  TKideConfig.Instance.Config.GetNode('ExtThemes').GetChildValues(_ExtJS_Theme.Items);
  TKideConfig.Instance.Config.GetNode('LanguageIds').GetChildValues(LanguageIdComboBox.Items);
  TKideConfig.Instance.Config.GetNode('Charsets').GetChildValues(CharsetComboBox.Items);
  //TConfig.Instance.Config.GetNode('KittoSearchPaths').GetChildValues(SearchPathComboBox.Items);
end;

procedure TConfigDesignerFrame.DesignPanelToEditNode;
begin
  inherited;
  EditorPageControl.ActivePageIndex := 0;
  EditNode.SetString('LanguageId', Copy(LanguageIdComboBox.Text,1,2));
  EditNode.SetString('Charset' ,CharSetComboBox.Text);
end;

procedure TConfigDesignerFrame.EditorPageControlChange(Sender: TObject);
begin
  inherited;
  if EditorPageControl.ActivePage = AuthTabSheet then
    EmbedEditNodeFrame(AuthTabSheet, TConfigAuthNodeFrame,
      EditNode.GetNode('Auth', True), True)
  else if EditorPageControl.ActivePage = DatabasesTabSheet then
    EmbedEditNodeFrame(DatabasesPanel, TConfigDatabasesNodeFrame,
      EditNode.GetNode('Databases', True), True)
  else if EditorPageControl.ActivePage = AccessControlTabSheet then
    EmbedEditNodeFrame(AccessControlTabSheet, TConfigAccessControlNodeFrame,
      EditNode.GetNode('AccessControl', True), True)
  else if EditorPageControl.ActivePage = LoginTabSheet then
    EmbedEditNodeFrame(LoginTabSheet, TLoginViewDesignerFrame,
      EditNode.GetNode('Login', True), True)
  else if EditorPageControl.ActivePage = EmailTabSheet then
    EmbedEditNodeFrame(EmailTabSheet, TConfigEmailNodeFrame,
      EditNode.FindNode('Email', True), True)
  else if EditorPageControl.ActivePage = DefaultsTabSheet then
    EmbedEditNodeFrame(DefaultsTabSheet, TConfigDefaultsNodeFrame,
      EditNode.FindNode('Defaults', True), True);
end;

procedure TConfigDesignerFrame.Init(const ANode: TEFTree);
begin
  inherited;
end;

class function TConfigDesignerFrame.SuitsTree(const ATree: TEFTree): Boolean;
begin
  Result := ATree is TEFComponentConfig;
end;

procedure TConfigDesignerFrame.UpdateDesignPanel(const AForce: Boolean);
var
  LLocaleId: string;
  I: integer;
  LCharSet: string;
  LTheme: string;
begin
  inherited;
  EditorPageControl.ActivePageIndex := 0;

  _Server_Port.Value := EditNode.GetInteger('Server/Port', 8080);
  _Server_SessionTimeout.Value := EditNode.GetInteger('Server/SessionTimeout', 10);

  LTheme := EditNode.GetString('ExtJS/Theme', 'default');
  _ExtJS_Theme.ItemIndex := _ExtJS_Theme.Items.IndexOf(LTheme);
  _ExtJS_Path.Text := EditNode.GetString('ExtJS/Path', '');
  _ExtJS_AjaxTimeout.Value := EditNode.GetInteger('ExtJS/AjaxTimeout', 30000);
  _HomeView.Text := EditNode.GetString('HomeView', 'Home');

  _DefaultDatabaseName.Text := EditNode.GetString('DefaultDatabaseName', 'Main');
  //Align ComboBox item
  LLocaleId := EditNode.GetString('LanguageId', 'en');
  for I := 0 to LanguageIdComboBox.Items.Count -1 do
  begin
    if SameText(Copy(LanguageIdComboBox.Items[I],1,2), LLocaleId) then
    begin
      LanguageIdComboBox.ItemIndex := I;
      break;
    end;
  end;
  LCharSet := EditNode.GetString('Charset');
  CharSetComboBox.ItemIndex := CharSetComboBox.Items.IndexOf(LCharSet);
end;

initialization
  TTreeDesignerFrameRegistry.Instance.RegisterClass(TConfigDesignerFrame.GetClassId, TConfigDesignerFrame);

finalization
  TTreeDesignerFrameRegistry.Instance.UnregisterClass(TConfigDesignerFrame.GetClassId);

end.
