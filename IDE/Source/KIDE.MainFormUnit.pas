unit KIDE.MainFormUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.ActnCtrls,
  Vcl.ToolWin, Vcl.ActnMan, Vcl.ActnMenus, Vcl.PlatformDefaultStyleActnCtrls,
  Vcl.ActnList, Vcl.ComCtrls, KIDE.Project, Vcl.ImgList, Vcl.Menus,
  KIDE.FileTree, Vcl.AppEvnts, Vcl.StdCtrls, EF.Logger, Vcl.ActnPopup;

type
  TMainForm = class;

  TMainFormLogEndpoint = class(TEFLogEndpoint)
  strict private
    FMainForm: TMainForm;
  strict protected
    procedure DoLog(const AString: string); override;
  public
    property MainForm: TMainForm read FMainForm write FMainForm;
  end;

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
    BottomSplitter: TSplitter;
    EditPanel: TPanel;
    BrowsePageControl: TPageControl;
    MetadataTabSheet: TTabSheet;
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
    NewProjectAction: TAction;
    NewProjectDialog: TSaveDialog;
    AboutAction: TAction;
    BottomPanel: TPanel;
    BrowseSplitter: TSplitter;
    BottomPageControl: TPageControl;
    LogTabSheet: TTabSheet;
    TabImages: TImageList;
    ViewLogAction: TAction;
    LogListBox: TListBox;
    LogImages: TImageList;
    ValidateMetadataAction: TAction;
    ClearMessagesAction: TAction;
    CopyMessagesAction: TAction;
    MessagesPopupActionBar: TPopupActionBar;
    Clear1: TMenuItem;
    Copy1: TMenuItem;
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
    procedure NewProjectActionExecute(Sender: TObject);
    procedure AboutActionExecute(Sender: TObject);
    procedure ViewLogActionExecute(Sender: TObject);
    procedure LogListBoxDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure ValidateMetadataActionExecute(Sender: TObject);
    procedure ClearMessagesActionExecute(Sender: TObject);
    procedure CopyMessagesActionUpdate(Sender: TObject);
    procedure CopyMessagesActionExecute(Sender: TObject);
  private
    FLogEndpoint: TMainFormLogEndpoint;
    procedure RebuildRecentProjectsMenu;
    procedure DoOpenProject(const AFileName: string);
    procedure UpdateCaption;
    procedure SetStatus(const AMessage: string; const AArgs: array of const); overload;
    procedure SetStatus(const AMessage: string); overload;
    function GetSelectedFileTreeNode: TFileTreeNode;
    procedure DoNewProject(const AFileName: string);
    procedure ShowLog(const AShow: Boolean);
    procedure UpdateBottomPanelVisibility;
    function GetLogBitmap(const ATag: string): TBitmap;
    procedure ClearMessages;
  public
    destructor Destroy; override;
    procedure Log(const AString: string);
    constructor Create(AOwner: TComponent); override;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  Clipbrd,
  EF.Localization, EF.SysUtils,
  KIDE.MRUOptions, KIDE.ModelWizardFormUnit, KIDE.SplashFormUnit,
  KIDE.EFHelpers, KIDE.ModelValidator, KIDE.ViewValidator;

procedure TMainForm.AboutActionExecute(Sender: TObject);
begin
  TSplashForm.ShowAbout;
end;

procedure TMainForm.ApplicationEventsHint(Sender: TObject);
begin
  SetStatus(Application.Hint);
end;

procedure TMainForm.ClearMessagesActionExecute(Sender: TObject);
begin
  ClearMessages;
end;

procedure TMainForm.ClearMessages;
begin
  LogListBox.Clear;
end;

procedure TMainForm.CloseProjectActionExecute(Sender: TObject);
begin
  TProject.CloseProject;
  RefreshFilesTreeView(FileTreeView, nil);
  ClearMessages;
  SetStatus('Project closed.');
  UpdateCaption;
end;

procedure TMainForm.CloseProjectActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := Assigned(TProject.CurrentProject);
end;

procedure TMainForm.CopyMessagesActionExecute(Sender: TObject);
var
  LText: string;
  I: Integer;
begin
  if LogListBox.SelCount > 0 then
  begin
    LText := '';
    for I := 0 to LogListBox.Count - 1 do
    begin
      if LogListBox.Selected[I] then
      begin
        if LText = '' then
          LText := LogListBox.Items[I]
        else
          LText := LText + sLineBreak + LogListBox.Items[I];
      end;
    end;
    Clipboard.AsText := LText;
  end
  else
    Clipboard.AsText := LogListBox.Items.Text;
end;

procedure TMainForm.CopyMessagesActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := LogListBox.Count > 0;
end;

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited;
  FLogEndPoint := TMainFormLogEndpoint.Create;
  FLogEndpoint.MainForm := Self;
end;

destructor TMainForm.Destroy;
begin
  inherited;
  FreeAndNil(FLogEndpoint);
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

  if (ParamCount = 1) and FileExists(ParamStr(1)) then
    DoOpenProject(ParamStr(1));
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

procedure TMainForm.NewProjectActionExecute(Sender: TObject);
begin
  NewProjectDialog.InitialDir := TMRUOptions.Instance.GetString('LastDir');

  if NewProjectDialog.Execute(Handle) then
  begin
    DoNewProject(NewProjectDialog.FileName);
    TMRUOptions.Instance.StoreString('LastDir', TProject.CurrentProject.Directory);
  end;
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
  ClearMessages;
  TProject.OpenProject(AFileName);
  TMRUOptions.Instance.StoreMRUItem('RecentProjects', TProject.CurrentProject.FileName);
  RefreshFilesTreeView(FileTreeView, TProject.CurrentProject);
  UpdateCaption;
  SetStatus('Project %s opened.', [TProject.CurrentProject.FileName]);
end;

procedure TMainForm.DoNewProject(const AFileName: string);
begin
  if not IsDirectoryEmpty(ExtractFilePath(AFileName)) then
    if MessageDlg(_('The chosen directory is not empty. Files may be overwritten. Are you sure you want to continue?'), mtWarning, [mbYes, mbNo], 0) <> mrYes then
      Abort;

  { TODO : ask to save pending changes }
  TProject.NewProject(AFileName);
  TMRUOptions.Instance.StoreMRUItem('RecentProjects', TProject.CurrentProject.FileName);
  RefreshFilesTreeView(FileTreeView, TProject.CurrentProject);
  UpdateCaption;
  SetStatus('Project %s created.', [TProject.CurrentProject.FileName]);
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

  { TODO : find a more reliable way to identify the parent item. }
  LParentItem := ActionManager.ActionBars[1].Items[0].Items[1];
  for I := LParentItem.Items.Count - 1 downto 2 do
    LParentItem.Items.Delete(I);
  //LParentItem.Items.Clear;

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

{ TODO : check if any changes are pending and ask }
  TProject.CurrentProject.Config.InvalidateConfig;
  TProject.CurrentProject.Config.Models.Open;
  TProject.CurrentProject.Config.Views.Open;
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

procedure TMainForm.Log(const AString: string);
var
  LString: string;
  LTag: string;
begin
  LString := TEFLogger.ExtractTag(AString, LTag);

  LogListBox.Items.BeginUpdate;
  try
    LogListBox.Items.AddObject(LString, GetLogBitmap(LTag));
    LogListBox.ItemIndex := -1;
    LogListBox.ItemIndex := LogListBox.Items.Count - 1;
  finally
    LogListBox.Items.EndUpdate;
  end;
  ViewLogAction.Checked := True;
  ShowLog(True);
  Update;
end;

function TMainForm.GetLogBitmap(const ATag: string): TBitmap;
begin
  if ATag <> '' then
  begin
    Result := TBitmap.Create;
    try
      if ATag = LOG_TAG_WARNING then
        LogImages.GetBitmap(1, Result)
      else if ATag = LOG_TAG_ERROR then
        LogImages.GetBitmap(2, Result)
      else
        LogImages.GetBitmap(0, Result);
    except
      FreeAndNil(Result);
      raise;
    end;
  end
  else
    Result := nil;
end;

procedure TMainForm.LogListBoxDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
var
  LBitmap: TBitmap;
  LOffset: Integer;
begin
  LogListBox.Canvas.FillRect(Rect);
  LOffset := 8;
  LBitmap := TBitmap(LogListBox.Items.Objects[Index]);
  if LBitmap <> nil then
  begin
    LogListBox.Canvas.BrushCopy(Bounds(Rect.Left + 2, Rect.Top + 2, LBitmap.Width,
      LBitmap.Height), LBitmap, Bounds(0, 0, LBitmap.Width, LBitmap.Height), clWhite);
    LOffset := LOffset + LBitmap.Width;
  end;
  LogListBox.Canvas.TextOut(Rect.Left + LOffset, Rect.Top, LogListBox.Items[Index]);
end;

procedure TMainForm.TreePopupMenuPopup(Sender: TObject);
var
  LNode: TFileTreeNode;
  I: Integer;
  LItem: TFileTreeMenuItem;
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
  LCaption := 'Kide';
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

procedure TMainForm.ValidateMetadataActionExecute(Sender: TObject);
var
  LModelValidator: TModelValidator;
  LViewValidator: TViewValidator;
begin
  LModelValidator := TModelValidator.Create;
  try
    LModelValidator.Execute;
  finally
    FreeAndNil(LModelValidator);
  end;

  LViewValidator := TViewValidator.Create;
  try
    LViewValidator.Execute;
  finally
    FreeAndNil(LViewValidator);
  end;
end;

procedure TMainForm.ViewLogActionExecute(Sender: TObject);
begin
  ShowLog((Sender as TAction).Checked);
end;

procedure TMainForm.ShowLog(const AShow: Boolean);
begin
  LogTabSheet.Visible := AShow;
  UpdateBottomPanelVisibility;
end;

procedure TMainForm.UpdateBottomPanelVisibility;
var
  I: Integer;
  LVisible: Boolean;
begin
  LVisible := False;
  for I := 0 to BottomPageControl.PageCount - 1 do
  begin
    if BottomPageControl.Pages[I].Visible then
    begin
      LVisible := True;
      Break;
    end;
  end;
  BottomPanel.Visible := LVisible;
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

{ TMainFormLogEndpoint }

procedure TMainFormLogEndpoint.DoLog(const AString: string);
begin
  inherited;
  Assert(Assigned(FMainForm));

  FMainForm.Log(AString);
end;

end.
