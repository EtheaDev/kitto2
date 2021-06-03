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
unit KIDE.NewProjectWizardForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, KIDE.BaseWizardForm, Vcl.ActnList,
  Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls, KIDE.BaseFrame, KIDE.ProjectTemplate,
  KIDE.ProjectTemplateFrame, Vcl.Buttons, System.Actions, Vcl.Samples.Spin;

type
  TNewProjectWizardForm = class(TBaseWizardForm)
    SelectTabSheet: TTabSheet;
    OptionsTabSheet: TTabSheet;
    GoTabSheet: TTabSheet;
    TemplateFrame: TProjectTemplateFrame;
    TemplateSplitter: TSplitter;
    TemplateInfoPanel: TPanel;
    AuthenticationtypeLabel: TLabel;
    AuthComboBox: TComboBox;
    AccessControltypeLabel: TLabel;
    ACComboBox: TComboBox;
    ProjectPathEdit: TLabeledEdit;
    ProjectPathButton: TSpeedButton;
    TemplateInfoRichEdit: TRichEdit;
    ProjectNameEdit: TLabeledEdit;
    DoneTabSheet: TTabSheet;
    ProjectCreatedRichEdit: TRichEdit;
    AppTitleEdit: TLabeledEdit;
    ExtJSLabel: TLabel;
    ExtThemeComboBox: TComboBox;
    ThemeLabel: TLabel;
    LanguagEEncodingLabel: TLabel;
    LanguageLabel: TLabel;
    LanguageIdComboBox: TComboBox;
    CharsetLabel: TLabel;
    CharsetComboBox: TComboBox;
    ServerPortEdit: TSpinEdit;
    PortLabel: TLabel;
    ServerLabel: TLabel;
    DatabasesGroupBox: TGroupBox;
    DBADOCheckBox: TCheckBox;
    DBDBXCheckBox: TCheckBox;
    DBFDCheckBox: TCheckBox;
    SearchPathLabel: TLabel;
    SearchPathComboBox: TComboBox;
    ThreadPoolSizeLabel: TLabel;
    ServerThreadPoolSizeEdit: TSpinEdit;
    SessionTimeOutLabel: TLabel;
    ServerSessionTimeOutEdit: TSpinEdit;
    ProjectOptionsLabel: TLabel;
    FeaturesLabel: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure ProjectPathButtonClick(Sender: TObject);
    procedure ProjectNameEditChange(Sender: TObject);
  private
    FTemplate: TProjectTemplate;
    procedure UpdateTemplateList(const ASelectTemplateName: string);
    procedure UpdateTemplateInfo;
    procedure TemplateListDblClick(Sender: TObject);
    procedure TemplateChange(Sender: TObject);
    procedure SaveOptionsMRU;
    function GetKeyBase: string;
    function OptionsValid: Boolean;
    procedure LoadOptionsMRU;
    procedure SaveProjectMRU;
    procedure LoadProjectMRU;
    procedure LoadRichText(const ARichEdit: TRichEdit; const AText: string);
    procedure FreeTemplate;
  protected
    procedure AfterEnterPage(const ACurrentPageIndex: Integer;
      const AOldPageIndex: Integer; const AGoingForward: Boolean); override;
    procedure AfterLeavePage(const AOldPageIndex: Integer;
      const ACurrentPageIndex: Integer; const AGoingForward: Boolean); override;
    function CanGoForward: Boolean; override;
    procedure BeforeLeavePage(const ACurrentPageIndex: Integer;
      const ANewPageIndex: Integer; const AGoingForward: Boolean); override;
    procedure InitWizard; override;
  public
    constructor Create(AOwner: TComponent); override;
  public
    class function ShowDialog(out AProjectTemplate: TProjectTemplate): Boolean;
  end;

implementation

{$R *.dfm}

uses
  FileCtrl
  , IOUtils
  , EF.Localization
  , EF.Sys.Windows
  , EF.StrUtils
  , KIDE.MRUOptions
  , KIDE.Config
  ;

const
  PAGE_TEMPLATE = 0;
  PAGE_OPTIONS = 1;
  PAGE_PROJECT = 2;
  PAGE_DONE = 3;

{ TNewProjectWizardForm }

procedure TNewProjectWizardForm.AfterEnterPage(const ACurrentPageIndex,
  AOldPageIndex: Integer; const AGoingForward: Boolean);
begin
  inherited;
  if ACurrentPageIndex = PAGE_TEMPLATE then
  begin
    PageTitle := _('Choose a Project Template');
    if AGoingForward then
      UpdateTemplateList(TMRUOptions.Instance.GetString(GetKeyBase + 'DefaultTemplateName'))
    else
      UpdateTemplateList(TemplateFrame.CurrentTemplateName);
  end
  else if ACurrentPageIndex = PAGE_OPTIONS then
  begin
    PageTitle := _('Set Template Options');
    LoadOptionsMRU;
  end
  else if ACurrentPageIndex = PAGE_PROJECT then
  begin
    PageTitle := _('Select Project Name and Directory');
    LoadProjectMRU;
  end
  else if ACurrentPageIndex = PAGE_DONE then
  begin
    PageTitle := _('Project created successfully');
    LoadRichText(ProjectCreatedRichEdit, FTemplate.GetSupportFileAsString('AfterCreate.rtf', TEncoding.ANSI));
  end;
end;

procedure TNewProjectWizardForm.AfterLeavePage(const AOldPageIndex,
  ACurrentPageIndex: Integer; const AGoingForward: Boolean);
begin
  inherited;
  if (AOldPageIndex = PAGE_TEMPLATE) and AGoingForward then
    TMRUOptions.Instance.StoreString(GetKeyBase + 'DefaultTemplateName', TemplateFrame.CurrentTemplateName)
  else if (AOldPageIndex = PAGE_OPTIONS) and AGoingForward then
    SaveOptionsMRU
  else if (AOldPageIndex = PAGE_PROJECT) and AGoingForward then
    SaveProjectMRU;
end;

procedure TNewProjectWizardForm.BeforeLeavePage(const ACurrentPageIndex,
  ANewPageIndex: Integer; const AGoingForward: Boolean);
begin
  inherited;
  if (ACurrentPageIndex = PAGE_TEMPLATE) and AGoingForward then
    FTemplate.TemplateName := TemplateFrame.CurrentTemplateName
  else if (ACurrentPageIndex = PAGE_OPTIONS) and AGoingForward then
  begin
    FTemplate.Options.SetString('SearchPath', SearchPathComboBox.Text);
    FTemplate.Options.SetBoolean('DB/ADO', DBADOCheckBox.Checked);
    FTemplate.Options.SetBoolean('DB/DBX', DBDBXCheckBox.Checked);
    FTemplate.Options.SetBoolean('DB/FD', DBFDCheckBox.Checked);
    FTemplate.Options.SetString('Auth', AuthComboBox.Text);
    FTemplate.Options.SetString('AC', ACComboBox.Text);
    FTemplate.Options.SetString('ExtJS/Theme', ExtThemeComboBox.Text);
    FTemplate.Options.SetString('LanguageId', LanguageIdComboBox.Text);
    FTemplate.Options.SetString('Charset', CharsetComboBox.Text);
    FTemplate.Options.SetInteger('Server/Port', ServerPortEdit.Value);
    FTemplate.Options.SetInteger('Server/ThreadPoolSize', ServerThreadPoolSizeEdit.Value);
    FTemplate.Options.SetInteger('Server/SessionTimeOut', ServerSessionTimeOutEdit.Value);
  end
  else if (ACurrentPageIndex = PAGE_PROJECT) and AGoingForward then
  begin
    FTemplate.ProjectDirectory := ProjectPathEdit.Text;
    if System.SysUtils.DirectoryExists(FTemplate.ProjectDirectory) and not IsDirectoryEmpty(FTemplate.ProjectDirectory) then
    begin
      if MessageDlg(_('The chosen directory is not empty. Files may be overwritten. Are you sure you want to continue?'), mtWarning, [mbYes, mbNo], 0) <> mrYes then
        Abort;
    end;
    FTemplate.ProjectName := ProjectNameEdit.Text;
    FTemplate.Options.SetString('AppTitle', AppTitleEdit.Text);
  end;
end;

function TNewProjectWizardForm.GetKeyBase: string;
begin
  Result := 'NewProjectWizard/';
end;

procedure TNewProjectWizardForm.InitWizard;
begin
  inherited;
  TKideConfig.Instance.Config.GetNode('Authentication/Authenticators').GetChildValues(AuthComboBox.Items);
  TKideConfig.Instance.Config.GetNode('AccessControl/AccessControllers').GetChildValues(ACComboBox.Items);
  TKideConfig.Instance.Config.GetNode('ExtThemes').GetChildValues(ExtThemeComboBox.Items);
  TKideConfig.Instance.Config.GetNode('LanguageIds').GetChildValues(LanguageIdComboBox.Items);
  TKideConfig.Instance.Config.GetNode('Charsets').GetChildValues(CharsetComboBox.Items);
  TKideConfig.Instance.Config.GetNode('KittoSearchPaths').GetChildValues(SearchPathComboBox.Items);
end;

procedure TNewProjectWizardForm.LoadOptionsMRU;
var
  LKeyBase: string;
begin
  LKeyBase := GetKeyBase + TemplateFrame.CurrentTemplateName + '/';
  SearchPathComboBox.Text := TMRUOptions.Instance.GetString(LKeyBase + 'SearchPath', '..\..\..\..');
  DBADOCheckBox.Checked := TMRUOptions.Instance.GetBoolean(LKeyBase + 'DB/ADO');
  DBDBXCheckBox.Checked := TMRUOptions.Instance.GetBoolean(LKeyBase + 'DB/DBX', True);
  DBFDCheckBox.Checked := TMRUOptions.Instance.GetBoolean(LKeyBase + 'DB/FD', True);
  AuthComboBox.Text := TMRUOptions.Instance.GetString(LKeyBase + 'Auth');
  ACComboBox.Text := TMRUOptions.Instance.GetString(LKeyBase + 'AC');
  ExtThemeComboBox.Text := TMRUOptions.Instance.GetString(LKeyBase + 'ExtJS/Theme', 'triton');
  LanguageIdComboBox.Text := TMRUOptions.Instance.GetString(LKeyBase + 'LanguageId', 'en');
  CharsetComboBox.Text := TMRUOptions.Instance.GetString(LKeyBase + 'Charset', 'utf-8');
  ServerPortEdit.Value := TMRUOptions.Instance.GetInteger(LKeyBase + 'Server/Port', 8080);
  ServerThreadPoolSizeEdit.Value := TMRUOptions.Instance.GetInteger(LKeyBase + 'Server/ThreadPoolSize', 20);
  ServerSessionTimeOutEdit.Value := TMRUOptions.Instance.GetInteger(LKeyBase + 'Server/SessionTimeOut', 10);
end;

procedure TNewProjectWizardForm.SaveOptionsMRU;
var
  LKeyBase: string;
begin
  LKeyBase := GetKeyBase + TemplateFrame.CurrentTemplateName + '/';
  TMRUOptions.Instance.SetString(LKeyBase + 'SearchPath', SearchPathComboBox.Text);
  TMRUOptions.Instance.SetBoolean(LKeyBase + 'DB/ADO', DBADOCheckBox.Checked);
  TMRUOptions.Instance.SetBoolean(LKeyBase + 'DB/DBX', DBDBXCheckBox.Checked);
  TMRUOptions.Instance.SetBoolean(LKeyBase + 'DB/FD', DBFDCheckBox.Checked);
  TMRUOptions.Instance.SetString(LKeyBase + 'Auth', AuthComboBox.Text);
  TMRUOptions.Instance.SetString(LKeyBase + 'AC', ACComboBox.Text);
  TMRUOptions.Instance.SetString(LKeyBase + 'ExtJS/Theme', ExtThemeComboBox.Text);
  TMRUOptions.Instance.SetString(LKeyBase + 'LanguageId', LanguageIdComboBox.Text);
  TMRUOptions.Instance.SetString(LKeyBase + 'Charset', CharsetComboBox.Text);
  TMRUOptions.Instance.SetInteger(LKeyBase + 'Server/Port', ServerPortEdit.Value);
  TMRUOptions.Instance.SetInteger(LKeyBase + 'Server/ThreadPoolSize', ServerThreadPoolSizeEdit.Value);
  TMRUOptions.Instance.SetInteger(LKeyBase + 'Server/SessionTimeOut', ServerSessionTimeOutEdit.Value);
  TMRUOptions.Instance.Save;
end;

procedure TNewProjectWizardForm.LoadProjectMRU;
var
  LKeyBase: string;
begin
  LKeyBase := GetKeyBase + TemplateFrame.CurrentTemplateName + '/';
  ProjectPathEdit.Text := TMRUOptions.Instance.GetString(LKeyBase + 'ProjectPath');
  ProjectNameEdit.Text := TMRUOptions.Instance.GetString(LKeyBase + 'ProjectName');
end;

procedure TNewProjectWizardForm.SaveProjectMRU;
var
  LKeyBase: string;
begin
  LKeyBase := GetKeyBase + TemplateFrame.CurrentTemplateName + '/';
  TMRUOptions.Instance.SetString(LKeyBase + 'ProjectPath', ProjectPathEdit.Text);
  TMRUOptions.Instance.SetString(LKeyBase + 'ProjectName', ProjectNameEdit.Text);
  TMRUOptions.Instance.Save;
end;

function TNewProjectWizardForm.CanGoForward: Boolean;
begin
  if PageIndex = PAGE_TEMPLATE then
    Result := TemplateFrame.CurrentTemplateName <> ''
  else if PageIndex = PAGE_OPTIONS then
    Result := OptionsValid
  else if PageIndex = PAGE_PROJECT then
    Result := (ProjectPathEdit.Text <> '') and (ProjectNameEdit.Text <> '')
  else
    Result := inherited CanGoForward;
end;

function TNewProjectWizardForm.OptionsValid: Boolean;
begin
  Result := True;
  if SearchPathComboBox.Text = '' then
    Result := False;
end;

constructor TNewProjectWizardForm.Create(AOwner: TComponent);
begin
  inherited;
  FTemplate := TProjectTemplate.Create;
end;

procedure TNewProjectWizardForm.FormCreate(Sender: TObject);
begin
  inherited;
  TemplateFrame.OnChange := TemplateChange;
  TemplateFrame.OnDblClick := TemplateListDblClick;
end;

procedure TNewProjectWizardForm.FreeTemplate;
begin
  FreeAndNil(FTemplate);
end;

procedure TNewProjectWizardForm.TemplateListDblClick(Sender: TObject);
begin
  ForwardAction.Execute;
end;

procedure TNewProjectWizardForm.TemplateChange(Sender: TObject);
begin
  UpdateTemplateInfo;
end;

class function TNewProjectWizardForm.ShowDialog(out AProjectTemplate: TProjectTemplate): Boolean;
var
  LForm: TNewProjectWizardForm;
begin
  AProjectTemplate := nil;
  LForm := TNewProjectWizardForm.Create(Application);
  try
    Result := LForm.ShowModal = mrOk;
    if not Result then
      LForm.FreeTemplate;
    AProjectTemplate := LForm.FTemplate;
  finally
    FreeAndNil(LForm);
  end;
end;

procedure TNewProjectWizardForm.ProjectNameEditChange(Sender: TObject);
begin
  inherited;
  AppTitleEdit.Text := CamelToSpaced(ProjectNameEdit.Text);
end;

procedure TNewProjectWizardForm.ProjectPathButtonClick(Sender: TObject);
var
  LDirectory: string;
begin
  inherited;
  LDirectory := ProjectPathEdit.Text;
  if SelectDirectory(_('Select a directory'), '', LDirectory, [sdNewFolder, sdShowEdit, sdNewUI, sdValidateDir]) then
  begin
    ProjectPathEdit.Text := LDirectory;
    ProjectNameEdit.Text := ExtractFileName(LDirectory);
  end;
end;

procedure TNewProjectWizardForm.UpdateTemplateList(
  const ASelectTemplateName: string);
begin
  TemplateFrame.UpdateList(ASelectTemplateName);
  UpdateTemplateInfo;
end;

procedure TNewProjectWizardForm.UpdateTemplateInfo;
begin
  TemplateInfoRichEdit.Clear;
  if TemplateFrame.CurrentTemplateName <> '' then
  begin
    FTemplate.TemplateName := TemplateFrame.CurrentTemplateName;
    LoadRichText(TemplateInfoRichEdit, FTemplate.GetSupportFileAsString('Info.rtf', TEncoding.ANSI));
  end;
end;

procedure TNewProjectWizardForm.LoadRichText(const ARichEdit: TRichEdit; const AText: string);
var
  LStringStream: TStringStream;
begin
  LStringStream := TStringStream.Create(AText);
  try
    ARichEdit.PlainText := False;
    ARichEdit.Lines.LoadFromStream(LStringStream);
  finally
    FreeAndNil(LStringStream);
  end;
end;

end.
