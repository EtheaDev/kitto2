unit KIDE.MainFormUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.ActnCtrls,
  Vcl.ToolWin, Vcl.ActnMan, Vcl.ActnMenus, Vcl.PlatformDefaultStyleActnCtrls,
  Vcl.ActnList, Vcl.ComCtrls, KIDE.Project, Vcl.ImgList, Vcl.Menus,
  KIDE.FileTree, Vcl.AppEvnts;

type
  TFileTreeMenuItem = class(TMenuItem)
  private
    FTreeNode: TFileTreeNode;
    FActionIndex: Integer;
    procedure SetTreeNode(const AValue: TFileTreeNode);
    procedure SetActionIndex(const AValue: Integer);
    procedure RefreshActionProperties;
  public
    constructor Create(AOwner: TComponent); override;
    property TreeNode: TFileTreeNode read FTreeNode write SetTreeNode;
    property ActionIndex: Integer read FActionIndex write SetActionIndex;
    procedure Click; override;
  end;

  TMainForm = class(TForm)
    ActionManager: TActionManager;
    MainMenuBar: TActionMainMenuBar;
    MainActionToolBar: TActionToolBar;
    ClientPanel: TPanel;
    BrowsePanel: TPanel;
    Splitter1: TSplitter;
    EditPanel: TPanel;
    BrowsePageControl: TPageControl;
    FilesTabSheet: TTabSheet;
    FileTreeView: TTreeView;
    OpenProjectAction: TAction;
    ExitAction: TAction;
    OpenProjectDialog: TOpenDialog;
    StatusBar: TStatusBar;
    FileTreeActionToolBar: TActionToolBar;
    RefreshFileTreeAction: TAction;
    ActionImages: TImageList;
    ModelWizardAction: TAction;
    CloseProjectAction: TAction;
    UpdateLocaleFilesAction: TAction;
    TreePopupMenu: TPopupMenu;
    ApplicationEvents: TApplicationEvents;
    procedure FileTreeViewCreateNodeClass(Sender: TCustomTreeView;
      var NodeClass: TTreeNodeClass);
    procedure ExitActionExecute(Sender: TObject);
    procedure OpenProjectActionExecute(Sender: TObject);
    procedure OpenRecentProjectActionExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RefreshFileTreeActionExecute(Sender: TObject);
    procedure RefreshFileTreeActionUpdate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FileTreeViewDblClick(Sender: TObject);
    procedure ModelWizardActionUpdate(Sender: TObject);
    procedure ModelWizardActionExecute(Sender: TObject);
    procedure CloseProjectActionExecute(Sender: TObject);
    procedure CloseProjectActionUpdate(Sender: TObject);
    procedure UpdateLocaleFilesActionUpdate(Sender: TObject);
    procedure UpdateLocaleFilesActionExecute(Sender: TObject);
    procedure TreePopupMenuPopup(Sender: TObject);
    procedure ApplicationEventsHint(Sender: TObject);
  private
    procedure RebuildRecentProjectsMenu;
    procedure DoOpenProject(const AFileName: string);
    procedure UpdateCaption;
    procedure SetStatus(const AMessage: string; const AArgs: array of const); overload;
    procedure SetStatus(const AMessage: string); overload;
    function GetSelectedFileTreeNode: TFileTreeNode;
  public
    destructor Destroy; override;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  KIDE.MRUOptions, KIDE.ModelWizardFormUnit;

procedure TMainForm.ApplicationEventsHint(Sender: TObject);
begin
  SetStatus(Application.Hint);
end;

procedure TMainForm.CloseProjectActionExecute(Sender: TObject);
begin
  TProject.CloseProject;
  RefreshFilesTreeView(FileTreeView, nil);
  SetStatus('Project closed.');
  UpdateCaption;
end;

procedure TMainForm.CloseProjectActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := Assigned(TProject.CurrentProject);
end;

destructor TMainForm.Destroy;
begin
  inherited;
end;

procedure TMainForm.ExitActionExecute(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.FileTreeViewCreateNodeClass(Sender: TCustomTreeView;
  var NodeClass: TTreeNodeClass);
begin
  NodeClass := TFileTreeNode;
end;

procedure TMainForm.FileTreeViewDblClick(Sender: TObject);
var
  LNode: TFileTreeNode;
  LMousePos: TPoint;
begin
  LMousePos := FileTreeView.ScreenToClient(Mouse.CursorPos);

  LNode := FileTreeView.GetNodeAt(LMousePos.X, LMousePos.Y) as TFileTreeNode;
  if Assigned(LNode) then
  begin
    FileTreeView.Selected := LNode;
    LNode.DefaultAction;
  end;
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if CloseProjectAction.Enabled then
    CloseProjectAction.Execute;

  TMRUOptions.Instance.SetInteger('MainForm/WindowState', Ord(WindowState));
  TMRUOptions.Instance.SetInteger('MainForm/BrowsePanel/Width', BrowsePanel.Width);
  if WindowState = wsNormal then
  begin
    TMRUOptions.Instance.SetInteger('MainForm/Left', Left);
    TMRUOptions.Instance.SetInteger('MainForm/Top', Top);
    TMRUOptions.Instance.SetInteger('MainForm/Width', Width);
    TMRUOptions.Instance.SetInteger('MainForm/Height', Height);
  end;
  TMRUOptions.Instance.Save;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  WindowState := TWindowState(TMRUOptions.Instance.GetInteger('MainForm/WindowState', Ord(WindowState)));
  BrowsePanel.Width := TMRUOptions.Instance.GetInteger('MainForm/BrowsePanel/Width', BrowsePanel.Width);
  if WindowState = wsNormal then
  begin
    Left := TMRUOptions.Instance.GetInteger('MainForm/Left', Left);
    Top := TMRUOptions.Instance.GetInteger('MainForm/Top', Top);
    Width := TMRUOptions.Instance.GetInteger('MainForm/Width', Width);
    Height := TMRUOptions.Instance.GetInteger('MainForm/Height', Height);
  end;
  RebuildRecentProjectsMenu;
end;

procedure TMainForm.ModelWizardActionExecute(Sender: TObject);
var
  LWizard: TModelWizardForm;
begin
  LWizard := TModelWizardForm.Create(Self);
  try
    if LWizard.ShowModal = mrOk then
      RefreshFileTreeAction.Execute;
  finally
    FreeAndNil(LWizard);
  end;
end;

procedure TMainForm.ModelWizardActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := Assigned(TProject.CurrentProject);
end;

procedure TMainForm.OpenProjectActionExecute(Sender: TObject);
var
  LDefaultDir: string;
begin
  if Assigned(TProject.CurrentProject) then
    LDefaultDir := TProject.CurrentProject.Directory
  else
    LDefaultDir := TMRUOptions.Instance.GetString('LastDir');
  OpenProjectDialog.InitialDir := LDefaultDir;

  if OpenProjectDialog.Execute(Handle) then
  begin
    DoOpenProject(OpenProjectDialog.FileName);
    TMRUOptions.Instance.StoreString('LastDir', TProject.CurrentProject.Directory);
  end;
end;

procedure TMainForm.OpenRecentProjectActionExecute(Sender: TObject);
begin
  DoOpenProject((Sender as TAction).Caption);
end;

procedure TMainForm.DoOpenProject(const AFileName: string);
begin
  { TODO : ask to save pending changes }
  TProject.OpenProject(AFileName);
  TMRUOptions.Instance.StoreMRUItem('RecentProjects', TProject.CurrentProject.FileName);
  RefreshFilesTreeView(FileTreeView, TProject.CurrentProject);
  UpdateCaption;
  SetStatus('Project %s opened.', [TProject.CurrentProject.FileName]);
end;

procedure TMainForm.RebuildRecentProjectsMenu;
const
  RECENT_PROJECTS_CAT = 'RecentProjects';
var
  LParentItem: TActionClientItem;
  LItems: TStrings;
  I: Integer;
  LItem: TActionClientItem;
  LAction: TAction;
begin
  for I := ActionManager.ActionCount - 1 downto 0 do
    if ActionManager.Actions[I].Category = RECENT_PROJECTS_CAT then
      ActionManager.Actions[I].Free;

  LParentItem := ActionManager.ActionBars[1].Items[0].Items[1];
  LParentItem.Items.Clear;

  LItems := TStringList.Create;
  try
    TMRUOptions.Instance.GetChildrenAsStrings('RecentProjects', LItems);
    for I := 0 to LItems.Count - 1 do
    begin
      LAction := TAction.Create(Self);
      LAction.Category := RECENT_PROJECTS_CAT;
      LAction.Caption := LItems.ValueFromIndex[I];
      LAction.OnExecute := OpenRecentProjectActionExecute;

      LItem := LParentItem.Items.Add;
      LItem.Action := LAction;
    end;
  finally
    FreeAndNil(LItems);
  end;
end;

procedure TMainForm.RefreshFileTreeActionExecute(Sender: TObject);
begin
  Assert(Assigned(TProject.CurrentProject));

  RefreshFilesTreeView(FileTreeView, TProject.CurrentProject);
end;

procedure TMainForm.RefreshFileTreeActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := Assigned(TProject.CurrentProject);
end;

procedure TMainForm.SetStatus(const AMessage: string; const AArgs: array of const);
begin
  StatusBar.Panels[1].Text := Format(AMessage, AArgs);
  StatusBar.Update;
end;

procedure TMainForm.SetStatus(const AMessage: string);
begin
  SetStatus(AMessage, []);
end;

function TMainForm.GetSelectedFileTreeNode: TFileTreeNode;
begin
  Result := FileTreeView.Selected as TFileTreeNode;
end;

procedure TMainForm.TreePopupMenuPopup(Sender: TObject);
var
  LNode: TFileTreeNode;
  I: Integer;
  LItem: TFileTreeMenuItem;
  LMetadata: TFileActionMetadata;
begin
  TreePopupMenu.Items.Clear;
  LNode := GetSelectedFileTreeNode;
  if Assigned(LNode) then
  begin
    for I := 0 to LNode.ActionCount - 1 do
    begin
      LItem := TFileTreeMenuItem.Create(Self);
      LItem.TreeNode := LNode;
      LItem.ActionIndex := I;
      TreePopupMenu.Items.Add(LItem);
    end;
  end;
end;

procedure TMainForm.UpdateCaption;
var
  LCaption: string;
begin
  LCaption := 'KIDE';
  if Assigned(TProject.CurrentProject) then
    LCaption := ExtractFileName(TProject.CurrentProject.FileName) + ' - ' + LCaption;
  Caption := LCaption;
end;

procedure TMainForm.UpdateLocaleFilesActionExecute(Sender: TObject);
begin
  //
end;

procedure TMainForm.UpdateLocaleFilesActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := Assigned(TProject.CurrentProject);
end;

{ TFileTreeMenuItem }

procedure TFileTreeMenuItem.Click;
begin
  inherited;
  if Assigned(FTreeNode) and (FActionIndex >= 0) and (FActionIndex < FTreeNode.ActionCount) then
    FTreeNode.ExecuteAction(FActionIndex);
end;

procedure TFileTreeMenuItem.SetActionIndex(const AValue: Integer);
begin
  FActionIndex := AValue;
  RefreshActionProperties;
end;

procedure TFileTreeMenuItem.SetTreeNode(const AValue: TFileTreeNode);
begin
  FTreeNode := AValue;
  RefreshActionProperties;
end;

constructor TFileTreeMenuItem.Create(AOwner: TComponent);
begin
  inherited;
  FTreeNode := nil;
  FActionIndex := -1;
end;

procedure TFileTreeMenuItem.RefreshActionProperties;
var
  LMetadata: TFileActionMetadata;
begin
  if Assigned(FTreeNode) and (FActionIndex >= 0) and (FActionIndex < FTreeNode.ActionCount) then
  begin
    LMetadata := FTreeNode.ActionMetadata[FActionIndex];
    Caption := LMetadata.DisplayLabel;
    Hint := LMetadata.Hint;
    ImageIndex := LMetadata.ImageIndex;
    Default := FActionIndex = 0;
  end;
end;

end.
