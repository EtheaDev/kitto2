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
unit KIDE.MainFormUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, System.UITypes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.ActnCtrls,
  Vcl.ToolWin, Vcl.ActnMan, Vcl.ActnMenus, Vcl.PlatformDefaultStyleActnCtrls,
  Vcl.ActnList, Vcl.ComCtrls, KIDE.Project, Vcl.ImgList, Vcl.Menus, Vcl.ActnPopup,
  KIDE.FileTree, Vcl.AppEvnts, Vcl.StdCtrls, EF.Logger, EF.ObserverIntf, EF.Intf,
  KIDE.Editor, EF.Tree, UxTheme, Themes, KIDE.MainDataModuleUnit, KIDE.BaseFormUnit,
  System.Actions, KIDE.DbGrid, Vcl.Imaging.pngimage, Vcl.Imaging.jpeg,
  System.ImageList;

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
    FContext: IEditContext;
    FTreeNode: TFileTreeNode;
    FActionIndex: Integer;
    FHandlerList: TFileNodeHandlerList;
    FRefreshProc: TProc;
    procedure RefreshProperties;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Setup(const AContext: IEditContext; const ANode: TFileTreeNode; const AActionIndex: Integer;
      const ASelection: TFileNodeHandlerList; const ARefreshProc: TProc);
    property TreeNode: TFileTreeNode read FTreeNode;
    property ActionIndex: Integer read FActionIndex;
    procedure Click; override;
  end;

  TMainForm = class(TBaseForm, IInterface, IEFInterface, IEditContext, IEFSubject, IEFObserver)
    ActionManager: TActionManager;
    MainMenuBar: TActionMainMenuBar;
    MainActionToolBar: TActionToolBar;
    ClientPanel: TPanel;
    BrowsePanel: TPanel;
    BottomSplitter: TSplitter;
    EditPanel: TPanel;
    BrowsePageControl: TPageControl;
    MetadataTabSheet: TTabSheet;
    MetadataTreeView: TTreeView;
    OpenProjectAction: TAction;
    ExitAction: TAction;
    OpenProjectDialog: TOpenDialog;
    StatusBar: TStatusBar;
    MetadataActionToolBar: TActionToolBar;
    RefreshMetadataAction: TAction;
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
    ViewLogAction: TAction;
    LogListBox: TListBox;
    LogImages: TImageList;
    ValidateMetadataAction: TAction;
    ClearMessagesAction: TAction;
    CopyMessagesAction: TAction;
    MessagesPopupActionBar: TPopupActionBar;
    Clear1: TMenuItem;
    Copy1: TMenuItem;
    ResourcesActionToolBar: TActionToolBar;
    ResourcesTreeView: TTreeView;
    ResourcesRefreshAction: TAction;
    SaveAction: TAction;
    SaveAllAction: TAction;
    TabPopupMenu: TPopupActionBar;
    CloseAction: TAction;
    Save1: TMenuItem;
    Close1: TMenuItem;
    EditPageControl: TPageControl;
    EditorPopupMenu: TPopupActionBar;
    StartAction: TAction;
    ChangeStyleAction: TAction;
    ReloadAction: TAction;
    UndoReload1: TMenuItem;
    HelpKideAction: TAction;
    HelpKittoAction: TAction;
    Messages1: TMenuItem;
    KittoRefAction: TAction;
    ViewProjectFilesAction: TAction;
    ClientImage: TImage;
    ChangeIconsAction: TAction;
    procedure BrowseTreeViewCreateNodeClass(Sender: TCustomTreeView; var NodeClass: TTreeNodeClass);
    procedure ExitActionExecute(Sender: TObject);
    procedure OpenProjectActionExecute(Sender: TObject);
    procedure OpenRecentProjectActionExecute(Sender: TObject);
    procedure RefreshMetadataActionExecute(Sender: TObject);
    procedure RefreshMetadataActionUpdate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BrowseTreeViewDblClick(Sender: TObject);
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
    procedure ResourcesRefreshActionUpdate(Sender: TObject);
    procedure ResourcesRefreshActionExecute(Sender: TObject);
    procedure ValidateMetadataActionUpdate(Sender: TObject);
    procedure BrowseTreeViewMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SaveActionUpdate(Sender: TObject);
    procedure SaveActionExecute(Sender: TObject);
    procedure SaveAllActionUpdate(Sender: TObject);
    procedure CloseActionUpdate(Sender: TObject);
    procedure CloseActionExecute(Sender: TObject);
    procedure EditPageControlMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure EditPageControlContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure EditPageControlMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure EditPageControlMouseLeave(Sender: TObject);
    procedure BrowseSplitterMoved(Sender: TObject);
    procedure BottomSplitterMoved(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure StartActionExecute(Sender: TObject);
    procedure StartActionUpdate(Sender: TObject);
    procedure ChangeStyleActionExecute(Sender: TObject);
    procedure ChangeStyleActionUpdate(Sender: TObject);
    procedure SaveAllActionExecute(Sender: TObject);
    procedure ReloadActionExecute(Sender: TObject);
    procedure ReloadActionUpdate(Sender: TObject);
    procedure HelpKittoActionExecute(Sender: TObject);
    procedure KittoRefActionExecute(Sender: TObject);
    procedure HelpKideActionExecute(Sender: TObject);
    procedure ViewProjectFilesActionExecute(Sender: TObject);
  private
    FLogEndpoint: TMainFormLogEndpoint;
    FSubjObserverImpl: TEFSubjectAndObserver;
    procedure RebuildRecentProjectsMenu;
    procedure DoOpenProject(const AFileName: string);
    procedure UpdateCaption;
    procedure SetStatus(const AMessage: string; const AArgs: array of const); overload;
    procedure SetStatus(const AMessage: string); overload;
    function GetSelectedFileTreeNode(const ATreeView: TTreeView): TFileTreeNode;
    procedure DoNewProject;
    procedure ShowLog(const AShow: Boolean);
    procedure ShowProjectFiles(const AShow: Boolean);
    procedure UpdateBottomPanelVisibility;
    procedure UpdateBrowsePanelVisibility;
    function GetLogImageIndex(const ATag: string): Integer;
    procedure ClearMessages;
    procedure RefreshBrowseTreeViews;
    procedure RefreshMetadataTreeView;
    procedure RefreshResourcesTreeView;
    function GetSelectedFileTreeNodeList(
      const ATreeView: TTreeView): TFileNodeHandlerList;
    procedure UpdateEditPageControl;
    function GetCurrentEditor: IEditor;
    function FindEditorInControl(const AControl: TControl): IEditor;
    function ChangesPending: Boolean;
    procedure ActivateEditor(const AEditor: IEditor);
    procedure CloseCurrentEditor;
    function GetEditorTabSheet(const AEditor: IEditor): TTabSheet;
    function FindCurrentEditor: IEditor;
    procedure CloseEditorByPageIndex(const APageIndex: Integer);
    function GetEditorInControl(const AControl: TControl): IEditor;
    procedure ForEachEditor(const AProc: TProc<IEditor>);
    procedure CloseAllEditors;
    procedure DoCloseProject;
    procedure CloseEditorBySpec(const ASpec: string; const AForce: Boolean);
    procedure GetCurrentAppName(out AAppName: string);
    procedure DoSaveAllEditors;
    procedure CheckAndSavePendings;
  protected
    function QueryInterface(const IID: TGUID; out Obj): HRESULT; override;
  public
    destructor Destroy; override;
    procedure Log(const AString: string);
    constructor Create(AOwner: TComponent); override;

    // IEFInterface.
    function AsObject: TObject;

    // IEditContext.
    function FindEditor(const ASpec: string): IEditor;
    function OpenEditor(const ASpec: string; const AParams: TEFNode): IEditor;

    // IEFSubject.
    procedure NotifyObservers(const AContext: string);
    procedure AttachObserver(const AObserver: IEFObserver);
    procedure DetachObserver(const AObserver: IEFObserver);

    // IEFObserver.
    procedure UpdateObserver(const ASubject: IEFSubject; const AContext: string = '');
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  Clipbrd, Math, Types, ShellAPI,
  EF.Localization, EF.Sys, EF.Sys.Windows, EF.StrUtils,
  Kitto.Config, Kitto.Vcl.MainForm,
  KIDE.Utils, KIDE.MRUOptions, KIDE.ModelWizardFormUnit, KIDE.SplashFormUnit,
  KIDE.EFHelpers, KIDE.ModelValidator, KIDE.ViewValidator, KIDE.ConfigValidator,
  KIDE.HelpViewer, KIDE.NewProjectWizardFormUnit, KIDE.SelectOptionsFormUnit;

procedure TMainForm.AboutActionExecute(Sender: TObject);
begin
  TSplashForm.ShowAbout;
end;

procedure TMainForm.ResourcesRefreshActionExecute(Sender: TObject);
begin
  Assert(Assigned(TProject.CurrentProject));

  RefreshResourcesTreeView;
end;

procedure TMainForm.ResourcesRefreshActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := Assigned(TProject.CurrentProject);
end;

procedure TMainForm.DoSaveAllEditors;
var
  LPageIndex: Integer;
  LEditor: IEditor;
begin
  for LPageIndex := 0 to EditPageControl.PageCount -1 do
  begin
    LEditor := GetEditorInControl(EditPageControl.Pages[LPageIndex]);
    if Assigned(LEditor) and LEditor.IsEditorActionEnabled(eaSave) then
      LEditor.ExecuteEditorAction(eaSave);
  end;
  if ChangesPending then
  begin
    MessageDlg('Cannot save all pending changes.', mtError, [mbOK], 0);
    Abort;
  end;
end;

procedure TMainForm.SaveAllActionExecute(Sender: TObject);
begin
  inherited;
  DoSaveAllEditors;
end;

procedure TMainForm.ApplicationEventsHint(Sender: TObject);
begin
  SetStatus(Application.Hint);
end;

function TMainForm.AsObject: TObject;
begin
  Result := Self;
end;

procedure TMainForm.ClearMessagesActionExecute(Sender: TObject);
begin
  ClearMessages;
end;

procedure TMainForm.ClearMessages;
begin
  LogListBox.Clear;
end;

procedure TMainForm.CloseActionExecute(Sender: TObject);
begin
  CloseCurrentEditor;
end;

procedure TMainForm.CloseCurrentEditor;
var
  LTabSheet: TTabSheet;
  LEditor: IEditor;
begin
  LEditor := GetCurrentEditor;
  LTabSheet := GetEditorTabSheet(LEditor);
  LEditor.CloseEditor(False);
  LTabSheet.Free;
  UpdateEditPageControl;
end;

procedure TMainForm.CloseEditorByPageIndex(const APageIndex: Integer);
var
  LEditor: IEditor;
begin
  LEditor := GetEditorInControl(EditPageControl.Pages[APageIndex]);
  LEditor.CloseEditor(False);
  EditPageControl.Pages[APageIndex].Free;
  UpdateEditPageControl;
end;

procedure TMainForm.CloseEditorBySpec(const ASpec: string; const AForce: Boolean);
var
  LEditor: IEditor;
  I: Integer;
begin
  for I := 0 to EditPageControl.PageCount - 1 do
  begin
    LEditor := FindEditorInControl(EditPageControl.Pages[I]);
    if Assigned(LEditor) and (LEditor.Spec = ASpec) then
    begin
      LEditor.CloseEditor(AForce);
      EditPageControl.Pages[I].Free;
      UpdateEditPageControl;
      Break;
    end;
  end;
end;

procedure TMainForm.CloseActionUpdate(Sender: TObject);
var
  LEditor: IEditor;
begin
  LEditor := FindCurrentEditor;
  (Sender as TAction).Enabled := Assigned(LEditor) and LEditor.IsEditorActionEnabled(eaClose);
end;

procedure TMainForm.CloseProjectActionExecute(Sender: TObject);
begin
  DoCloseProject;
end;

procedure TMainForm.DoCloseProject;
begin
  CloseAllEditors;
//  if TProject.CurrentProject <> nil then
//    TProject.CurrentProject.Config.DetachObserver(Self);
  TProject.CloseProject;
  RefreshBrowseTreeViews;
  ClearMessages;
  SetStatus('Project closed.');
  UpdateCaption;
end;

procedure TMainForm.CloseAllEditors;
begin
  while EditPageControl.PageCount > 0 do
    CloseCurrentEditor;
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
  FSubjObserverImpl := TEFSubjectAndObserver.Create;

  FLogEndPoint := TMainFormLogEndpoint.Create;
  FLogEndpoint.MainForm := Self;

  UpdateEditPageControl;
end;

destructor TMainForm.Destroy;
begin
  inherited;
  FreeAndNil(FLogEndpoint);
  FreeAndNil(FSubjObserverImpl);
end;

procedure TMainForm.EditPageControlContextPopup(Sender: TObject;
  MousePos: TPoint; var Handled: Boolean);
var
  LPageControl: TPageControl;
begin
  LPageControl := Sender as TPageControl;

  if [htOnItem] * LPageControl.GetHitTestInfoAt(MousePos.X, MousePos.Y) <> [] then
    LPageControl.PopupMenu := TabPopupMenu
  else
    LPageControl.PopupMenu := nil;
end;

procedure TMainForm.EditPageControlMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  LTabRect: TRect;
  I: Integer;
begin
  if (Button = mbMiddle) then
  begin
    for I := 0 to EditPageControl.PageCount - 1 do
    begin
      LTabRect := EditPageControl.TabRect(I);
      if PtInRect(LTabRect, Point(X, Y)) then
      begin
        CloseEditorByPageIndex(I);
        Break;
      end;
    end;
  end;
end;

procedure TMainForm.EditPageControlMouseLeave(Sender: TObject);
begin
  EditPageControl.Hint := '';
end;

procedure TMainForm.EditPageControlMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  LTabIndex: Integer;
  LPageControl: TPageControl;
  LPoint: TPoint;
begin
  LPageControl := Sender as TPageControl;

  LTabIndex := LPageControl.IndexOfTabAt(X, Y);
  if LTabIndex >= 0 then
  begin
    if LPageControl.Hint <> LPageControl.Pages[LTabIndex].Hint then
    begin
      LPageControl.Hint := LPageControl.Pages[LTabIndex].Hint;
      // Refresh displayed hint if changed.
      GetCursorPos(LPoint);
      Application.ActivateHint(LPoint);
    end;
  end;
end;

procedure TMainForm.ExitActionExecute(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.BottomSplitterMoved(Sender: TObject);
begin
  TMRUOptions.Instance.StoreInteger(GetMRURootKeyName + '/BottomPanel/Height', BottomPanel.Height);
end;

procedure TMainForm.BrowseSplitterMoved(Sender: TObject);
begin
  TMRUOptions.Instance.StoreInteger(GetMRURootKeyName + '/BrowsePanel/Width', BrowsePanel.Width);
end;

procedure TMainForm.BrowseTreeViewCreateNodeClass(Sender: TCustomTreeView;
  var NodeClass: TTreeNodeClass);
begin
  NodeClass := TFileTreeNode;
end;

procedure TMainForm.BrowseTreeViewDblClick(Sender: TObject);
var
  LNode: TFileTreeNode;
  LMousePos: TPoint;
  LList: TFileNodeHandlerList;
begin
  LMousePos := (Sender as TTreeView).ScreenToClient(Mouse.CursorPos);

  LNode := (Sender as TTreeView).GetNodeAt(LMousePos.X, LMousePos.Y) as TFileTreeNode;
  if Assigned(LNode) and (LNode.Count = 0) then
  begin
    (Sender as TTreeView).Selected := LNode;
    LList := GetSelectedFileTreeNodeList(Sender as TTreeView);
    try
      LNode.ExecuteDefaultAction(Self, LList);
    finally
      FreeAndNil(LList);
    end;
  end;
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  inherited;
  if CloseProjectAction.Enabled then
    CloseProjectAction.Execute;

  TMRUOptions.Instance.StoreInteger(GetMRURootKeyName + '/BrowsePageControl/ActivePageIndex', BrowsePageControl.ActivePageIndex);
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  LBackgroundFileName: string;
begin
  inherited;
  BrowsePageControl.ActivePageIndex := 0;
  TKConfig.OnGetAppName := GetCurrentAppName;
  TStyleManager.SetStyle(TMRUOptions.Instance.GetString('ApplicationStyle',  'Aqua Light Slate'));
  LBackgroundFileName := ExtractFilePath(Application.ExeName)+'background.jpg';
  if FileExists(LBackgroundFileName) then
    ClientImage.Picture.LoadFromFile(LBackgroundFileName);
  MainActionToolBar.ParentColor := True;
end;

procedure TMainForm.FormShow(Sender: TObject);
var
  LProjectName: string;
begin
  inherited;
  UpdateCaption;
  BrowsePanel.Width := TMRUOptions.Instance.GetInteger(GetMRURootKeyName + '/BrowsePanel/Width', BrowsePanel.Width);

  BottomPanel.Height := TMRUOptions.Instance.GetInteger(GetMRURootKeyName + '/BottomPanel/Height', BottomPanel.Height);
  LogTabSheet.Visible := TMRUOptions.Instance.GetBoolean(GetMRURootKeyName + '/LogTabSheet/Visible', LogTabSheet.Visible);
  UpdateBottomPanelVisibility;

  BrowsePanel.Visible := TMRUOptions.Instance.GetBoolean(GetMRURootKeyName + '/BrowsePanel/Visible', BrowsePanel.Visible);
  UpdateBrowsePanelVisibility;

  RebuildRecentProjectsMenu;
  BrowsePageControl.ActivePageIndex := TMRUOptions.Instance.GetInteger(GetMRURootKeyName + '/BrowsePageControl/ActivePageIndex', BrowsePageControl.ActivePageIndex);

  LProjectName := GetCmdLineParamValue('project');
  ExpandEnvironmentVariables(LProjectName);
  if LProjectName = '' then
    LProjectName := ParamStr(1);
  if SameText(ExtractFileExt(LProjectName),'.kproj') and (LProjectName <> '') and FileExists(LProjectName) then
    DoOpenProject(LProjectName);

  UpdateEditPageControl;
end;

procedure TMainForm.BrowseTreeViewMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbRight then
    (Sender as TTreeView).Selected := (Sender as TTreeView).GetNodeAt(X, Y);
end;

procedure TMainForm.ModelWizardActionExecute(Sender: TObject);
begin
  TModelWizardForm.ShowDialog(nil);
  if RefreshMetadataAction.Enabled then
    RefreshMetadataAction.Execute;
end;

procedure TMainForm.ModelWizardActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := Assigned(TProject.CurrentProject);
end;

procedure TMainForm.NewProjectActionExecute(Sender: TObject);
begin
  DoCloseProject;
  DoNewProject;
end;

function TMainForm.OpenEditor(const ASpec: string;
  const AParams: TEFNode): IEditor;
var
  LTabSheet: TTabSheet;
begin
  Result := FindEditor(ASpec);
  if not Assigned(Result) then
  begin
    Result := TEditorFactory.Instance.CreateEditor(ASpec, AParams, Self);
    // Allow multiple frames of the same type to be open at the same time.
    if Result.AsObject is TComponent then
      TComponent(Result.AsObject).Name := '';
    Result.InitEditor(AParams);
    LTabSheet := TTabSheet.Create(Self);
    try
      LTabSheet.PageControl := EditPageControl;
      UpdateEditPageControl;
      LTabSheet.Caption := Result.GetEditorProperty('ShortTitle');
      LTabSheet.Hint := Result.GetEditorProperty('LongTitle');
      LTabSheet.ShowHint := False; // Hints are only shown when hovering on tabs.
      LTabSheet.ParentShowHint := False;
      LTabSheet.ImageIndex := Result.GetEditorProperty('ImageIndex');
      LTabSheet.PopupMenu := EditorPopupMenu;
      // Do this now instead of waiting for ActivateEditor so that the
      // editor can perform a SetFocus in DisplayEmbedded.
      EditPageControl.ActivePageIndex := EditPageControl.PageCount - 1;
      Result.DisplayEmbedded(LTabSheet);
    except
      FreeAndNil(LTabSheet);
      raise;
    end;
  end
  else
    Result.RefreshEditor;
  ActivateEditor(Result);
end;

procedure TMainForm.ActivateEditor(const AEditor: IEditor);
var
  I: Integer;
begin
  for I := 0 to EditPageControl.PageCount - 1 do
  begin
    if FindEditorInControl(EditPageControl.Pages[I]) = AEditor then
    begin
      EditPageControl.ActivePageIndex := I;
      Break;
    end;
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

procedure TMainForm.CheckAndSavePendings;
begin
  if ChangesPending then
    case MessageDlg('Do you want to save all pending changes before continue?', mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
      mrYes: DoSaveAllEditors;
      mrCancel: Abort;
    end;
end;


procedure TMainForm.DoOpenProject(const AFileName: string);
begin
  CheckAndSavePendings;
  DoCloseProject;
  TProject.OpenProject(AFileName);
  TProject.CurrentProject.Config.AttachObserver(Self);
  TMRUOptions.Instance.StoreMRUItem('RecentProjects', TProject.CurrentProject.FileName);
  RefreshBrowseTreeViews;
  UpdateCaption;
  SetStatus('Project %s opened.', [TProject.CurrentProject.FileName]);
  RebuildRecentProjectsMenu;
  ShowProjectFiles(True);
end;

procedure TMainForm.RefreshBrowseTreeViews;
begin
  RefreshMetadataTreeView;
  RefreshResourcesTreeView;
end;

procedure TMainForm.RefreshMetadataTreeView;
begin
  BuildMetadataTreeView(MetadataTreeView, TProject.CurrentProject);
end;

procedure TMainForm.RefreshResourcesTreeView;
begin
  BuildResourcesTreeView(ResourcesTreeView, TProject.CurrentProject);
end;

procedure TMainForm.ReloadActionExecute(Sender: TObject);
begin
  inherited;
  GetCurrentEditor.ExecuteEditorAction(eaReload);
end;

procedure TMainForm.ReloadActionUpdate(Sender: TObject);
var
  LEditor: IEditor;
begin
  LEditor := FindCurrentEditor;
  (Sender as TAction).Enabled := Assigned(LEditor) and LEditor.IsEditorActionEnabled(eaReload);
end;

procedure TMainForm.DoNewProject;
var
  LProjectFileName: string;
begin
  CheckAndSavePendings;
  if TNewProjectWizardForm.ShowDialog(LProjectFileName) then
  begin
    TProject.OpenProject(LProjectFileName);
    TMRUOptions.Instance.StoreMRUItem('RecentProjects', TProject.CurrentProject.FileName);
    RefreshBrowseTreeViews;
    UpdateCaption;
    SetStatus('Project %s created.', [TProject.CurrentProject.FileName]);
  end;
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
  LFileName: string;
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
    //Add existing projects
    for I := 0 to LItems.Count - 1 do
    begin
      LFileName := LItems.ValueFromIndex[I];
      if FileExists(LFileName) then
      begin
        LAction := TAction.Create(Self);
        LAction.Category := RECENT_PROJECTS_CAT;
        LAction.Caption := LItems.ValueFromIndex[I];
        LAction.OnExecute := OpenRecentProjectActionExecute;

        LItem := LParentItem.Items.Add;
        LItem.Action := LAction;
      end;
    end;
    //Remove missing projects
    I := LItems.Count;
    while I > 0 do
    begin
      LFileName := LItems.ValueFromIndex[I-1];
      if not FileExists(LFileName) then
      begin
        LItems.Delete(I-1);
        Dec(I);
      end;
      Dec(I);
    end;
  finally
    FreeAndNil(LItems);
  end;
end;

procedure TMainForm.RefreshMetadataActionExecute(Sender: TObject);
begin
  Assert(Assigned(TProject.CurrentProject));

{ TODO : check if any changes are pending and ask }
  TProject.CurrentProject.RefreshAll;
  RefreshMetadataTreeView;
  ForEachEditor(
    procedure (AEditor: IEditor)
    begin
      AEditor.RefreshEditor();
    end
  );
end;

procedure TMainForm.RefreshMetadataActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := Assigned(TProject.CurrentProject);
end;

procedure TMainForm.SetStatus(const AMessage: string; const AArgs: array of const);
begin
  StatusBar.Panels[1].Text := Format(AMessage, AArgs);
  StatusBar.Update;
end;

procedure TMainForm.SaveActionExecute(Sender: TObject);
begin
  GetCurrentEditor.ExecuteEditorAction(eaSave);
end;

procedure TMainForm.SaveActionUpdate(Sender: TObject);
var
  LEditor: IEditor;
begin
  LEditor := FindCurrentEditor;
  (Sender as TAction).Enabled := Assigned(LEditor) and LEditor.IsEditorActionEnabled(eaSave);
end;

function TMainForm.ChangesPending: Boolean;
var
  I: Integer;
  LEditor: IEditor;
begin
  Result := False;
  for I := 0 to EditPageControl.PageCount - 1 do
  begin
    LEditor := FindEditorInControl(EditPageControl.Pages[I]);
    if Assigned(LEditor) and LEditor.IsEditorActionEnabled(eaSave) then
    begin
      Result := True;
      Break;
    end;
  end;
end;

procedure TMainForm.ChangeStyleActionExecute(Sender: TObject);
var
  I, LSelectedIndex: integer;
  LIconsStyle: integer;
  LOptions, LNewStyleName: string;
  LStringList: TStringList;
begin
  inherited;
  if CloseProjectAction.Enabled then
    CloseProjectAction.Execute;
  LOptions := '';
  LStringList := TStringList.Create;
  try
    for I := 0 to High(TStyleManager.StyleNames) do
      LStringList.Add(TStyleManager.StyleNames[I]);
    LStringList.Sorted := True;
    LOptions := LStringList.Text;
    LSelectedIndex := LStringList.IndexOf(TStyleManager.ActiveStyle.Name);
    LIconsStyle := Ord(MainDataModule.IconsStyle);
    if KideSelectOption(LOptions, Self.Font, 'Select style', 140,
      LSelectedIndex, LIconsStyle, 3, 400) then
    begin
      //Change Icons
      MainDataModule.IconsStyle := TIconsStyle(LIconsStyle);
      //Change Application Style
      LNewStyleName := LStringList.Strings[LSelectedIndex];
      TStyleManager.SetStyle(LNewStyleName);
      TMRUOptions.Instance.StoreString('ApplicationStyle', LNewStyleName);
    end;
  finally
    LStringList.Free;
  end;
end;

procedure TMainForm.ChangeStyleActionUpdate(Sender: TObject);
begin
  inherited;
  ChangeStyleAction.Enabled := StyleServices.Enabled;
end;

procedure TMainForm.SaveAllActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := ChangesPending;
end;

procedure TMainForm.SetStatus(const AMessage: string);
begin
  SetStatus(AMessage, []);
end;

function TMainForm.GetSelectedFileTreeNode(const ATreeView: TTreeView): TFileTreeNode;
begin
  Assert(Assigned(ATreeView));

  Result := ATreeView.Selected as TFileTreeNode;
end;

function TMainForm.GetSelectedFileTreeNodeList(const ATreeView: TTreeView): TFileNodeHandlerList;
var
  I: Integer;
begin
  Assert(Assigned(ATreeView));

  Result := TFileNodeHandlerList.Create;
  try
    for I := 0 to ATreeView.SelectionCount - 1 do
      Result.Add((ATreeView.Selections[I] as TFileTreeNode).Handler);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

procedure TMainForm.HelpKideActionExecute(Sender: TObject);
begin
  inherited;
  ShowKideHelp(100);
end;

procedure TMainForm.HelpKittoActionExecute(Sender: TObject);
begin
  inherited;
  ShowKittoWiki('home');
end;

procedure TMainForm.KittoRefActionExecute(Sender: TObject);
begin
  inherited;
  ShellExecute(0, 'open' , PChar('http://www.ethea.it/docs/kitto/en/ref/index.html'), nil, nil, SW_SHOW );
end;

procedure TMainForm.Log(const AString: string);
begin
  LogListBox.Items.BeginUpdate;
  try
    LogListBox.Items.Add(AString);
    LogListBox.ItemIndex := -1;
    LogListBox.ItemIndex := LogListBox.Items.Count - 1;
  finally
    LogListBox.Items.EndUpdate;
  end;
  ViewLogAction.Checked := True;
  ShowLog(True);
  Update;
end;

function TMainForm.FindCurrentEditor: IEditor;
begin
  Result := FindEditorInControl(EditPageControl.ActivePage);
end;

procedure TMainForm.GetCurrentAppName(out AAppName: string);
begin
  if TProject.CurrentProject <> nil then
    AAppName := TProject.CurrentProject.BaseName
  else
    AAppName := '';
end;

function TMainForm.GetCurrentEditor: IEditor;
begin
  Result := FindCurrentEditor;
  if not Assigned(Result) then
    raise Exception.Create('No current editor.');
end;

procedure TMainForm.ForEachEditor(const AProc: TProc<IEditor>);
var
  I: Integer;
  LEditor: IEditor;
begin
  for I := 0 to EditPageControl.PageCount - 1 do
  begin
    LEditor := FindEditorInControl(EditPageControl.Pages[I]);
    if Assigned(LEditor) then
      AProc(LEditor);
  end;
end;

function TMainForm.FindEditor(const ASpec: string): IEditor;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to EditPageControl.PageCount - 1 do
  begin
    Result := FindEditorInControl(EditPageControl.Pages[I]);
    if Assigned(Result) and Result.EditorMatchesSpec(ASpec) then
      Break;
    Result := nil;
  end;
end;

function TMainForm.GetEditorTabSheet(const AEditor: IEditor): TTabSheet;
var
  I: Integer;
  LEditor: IEditor;
begin
  Result := nil;
  for I := 0 to EditPageControl.PageCount - 1 do
  begin
    LEditor := FindEditorInControl(EditPageControl.Pages[I]);
    if LEditor = AEditor then
    begin
      Result := EditPageControl.Pages[I];
      Break;
    end;
  end;
  if not Assigned(Result) then
    raise Exception.Create('Cannot find TabSheet for specified editor');
end;


function TMainForm.FindEditorInControl(const AControl: TControl): IEditor;
var
  I: Integer;
begin
  if AControl = nil then
    Result := nil
  else
  begin
    if Supports(AControl, IEditor, Result) then
      Exit;
    if AControl is TWinControl then
    begin
      for I := 0 to TWinControl(AControl).ControlCount - 1 do
      begin
        Result := FindEditorInControl(TWinControl(AControl).Controls[I]);
        if Assigned(Result) then
          Break;
      end;
    end;
  end;
end;

function TMainForm.GetEditorInControl(const AControl: TControl): IEditor;
begin
  Result := FindEditorInControl(AControl);
  if not Assigned(Result) then
    raise Exception.Create('Editor not found.');
end;

function TMainForm.GetLogImageIndex(const ATag: string): Integer;
begin
  if ATag <> '' then
  begin
    if ATag = LOG_TAG_WARNING then
      Result := 1
    else if ATag = LOG_TAG_ERROR then
      Result := 2
    else
      Result := 0;
  end
  else
    Result := -1;
end;

procedure TMainForm.LogListBoxDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
var
  LOffset: Integer;
  LImageIndex: Integer;
  LString: string;
  LTag: string;
begin
  LString := TEFLogger.ExtractTag(LogListBox.Items[Index], LTag);
  LogListBox.Canvas.FillRect(Rect);
  LOffset := 6 + LogImages.Width;
  LImageIndex := GetLogImageIndex(LTag);
  if LImageIndex <> -1 then
    LogImages.Draw(LogListBox.Canvas, Rect.Left + 2, Rect.Top, LImageIndex);
  LogListBox.Canvas.TextOut(Rect.Left + LOffset, Rect.Top, LString);
end;

procedure TMainForm.TreePopupMenuPopup(Sender: TObject);
var
  LNode: TFileTreeNode;
  I: Integer;
  LItem: TFileTreeMenuItem;
  LMenuItem: TMenuItem;
  LList: TFileNodeHandlerList;
  LTreeView: TTreeView;
begin
  TreePopupMenu.Items.Clear;
  LTreeView := (Sender as TPopupMenu).PopupComponent as TTreeView;
  LNode := GetSelectedFileTreeNode(LTreeView);
  if Assigned(LNode) then
  begin
    LList := GetSelectedFileTreeNodeList((Sender as TPopupMenu).PopupComponent as TTreeView);
    try
      for I := 0 to LNode.ActionCount - 1 do
      begin
        LItem := TFileTreeMenuItem.Create(Self);
        try
          LItem.Setup(Self, LNode, I, LList,
            procedure
            begin
              if LTreeView = MetadataTreeView then
                RefreshMetadataTreeView
              else if LTreeView = ResourcesTreeView then
                RefreshResourcesTreeView;
            end);
          TreePopupMenu.Items.Add(LItem);
        except
          FreeAndNil(LItem);
          raise;
        end;
      end;
    finally
      FreeAndNil(LList);
    end;
  end;
  if TreePopupMenu.Items.Count = 0 then
  begin
    LMenuItem := TMenuItem.Create(Self);
    LMenuItem.Action := ViewProjectFilesAction;
    LMenuItem.Caption := 'Hide Project Files Panel';
    TreePopupMenu.Items.Add(LMenuItem);
  end;
end;

procedure TMainForm.UpdateCaption;
var
  LCaption: string;
begin
  LCaption := Format('%s - %s',[Application.Title, GetKIDEVersion]);
  if Assigned(TProject.CurrentProject) then
    LCaption := LCaption + ' - ' + TProject.CurrentProject.FileName;
  Caption := LCaption;
end;

procedure TMainForm.UpdateEditPageControl;
var
  LVisible: Boolean;
begin
  LVisible := EditPageControl.PageCount > 0;
  ClientImage.Visible := not LVisible;
  EditPageControl.Visible := LVisible;
end;

procedure TMainForm.UpdateLocaleFilesActionExecute(Sender: TObject);
begin
  //
end;

procedure TMainForm.UpdateLocaleFilesActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := Assigned(TProject.CurrentProject);
end;

procedure TMainForm.UpdateObserver(const ASubject: IEFSubject;
  const AContext: string);
var
  LParts: TStringDynArray;
begin
  if (Pos('ObjectRemoved' + #9, AContext) = 1) or (Pos('ObjectDisposed' + #9, AContext) = 1) then
  begin
    LParts := Split(AContext, #9);
    Assert(Length(LParts) = 2);
    CloseEditorBySpec(LParts[1], True);
  end;
end;

procedure TMainForm.ValidateMetadataActionExecute(Sender: TObject);
var
  LModelValidator: TModelValidator;
  LViewValidator: TViewValidator;
  LConfigValidator: TConfigValidator;
begin
  CheckAndSavePendings;

  TProject.CurrentProject.RefreshAll;

  LConfigValidator := TConfigValidator.Create;
  try
    LConfigValidator.Execute;
  finally
    FreeAndNil(LConfigValidator);
  end;

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

procedure TMainForm.ValidateMetadataActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := Assigned(TProject.CurrentProject);
end;

procedure TMainForm.ViewLogActionExecute(Sender: TObject);
begin
  ShowLog((Sender as TAction).Checked);
end;

procedure TMainForm.ViewProjectFilesActionExecute(Sender: TObject);
begin
  inherited;
  ShowProjectFiles((Sender as TAction).Checked);
end;

procedure TMainForm.ShowLog(const AShow: Boolean);
begin
  LogTabSheet.Visible := AShow;
  TMRUOptions.Instance.SetBoolean(GetMRURootKeyName + '/LogTabSheet/Visible', LogTabSheet.Visible);
  UpdateBottomPanelVisibility;
end;

procedure TMainForm.ShowProjectFiles(const AShow: Boolean);
begin
  BrowsePanel.Visible := AShow;
  TMRUOptions.Instance.SetBoolean(GetMRURootKeyName + '/BrowsePanel/Visible', BrowsePanel.Visible);
  UpdateBrowsePanelVisibility;
end;

procedure TMainForm.StartActionExecute(Sender: TObject);
var
  LKittoEngineHandle: Cardinal;
begin
  inherited;
  ExecuteApplication(TProject.CurrentProject.KittoEngineFileName+' -a',
    LKittoEngineHandle);
  TProject.CurrentProject.KittoEngineHandle := LKittoEngineHandle;
end;

procedure TMainForm.StartActionUpdate(Sender: TObject);
begin
  inherited;
  (Sender as TAction).Enabled := Assigned(TProject.CurrentProject) and
    FileExists(TProject.CurrentProject.KittoEngineFileName) and
    not TProject.CurrentProject.IsKittoEngineRunning;
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
  ViewLogAction.Checked := LVisible;
  BottomSplitter.Top := BottomPanel.Top -1;
  BottomSplitter.Visible := LVisible;
end;

procedure TMainForm.UpdateBrowsePanelVisibility;
begin
  BrowseSplitter.Visible := BrowsePanel.Visible;
  BrowseSplitter.Left := BrowsePanel.left+1;
  ViewProjectFilesAction.Checked := BrowsePanel.Visible;
end;

function TMainForm.QueryInterface(const IID: TGUID; out Obj): HRESULT;
begin
  // Don't delegate to FSubjObserverImpl. We want to expose our own interfaces
  // and get the callbacks.
  if GetInterface(IID, Obj) then Result := 0 else Result := E_NOINTERFACE;
end;

procedure TMainForm.AttachObserver(const AObserver: IEFObserver);
begin
  FSubjObserverImpl.AttachObserver(AObserver);
end;

procedure TMainForm.DetachObserver(const AObserver: IEFObserver);
begin
  FSubjObserverImpl.DetachObserver(AObserver);
end;

procedure TMainForm.NotifyObservers(const AContext: string);
begin
  FSubjObserverImpl.NotifyObserversOnBehalfOf(Self, AContext);
end;

{ TFileTreeMenuItem }

procedure TFileTreeMenuItem.Click;
begin
  inherited;
  if Assigned(FTreeNode) and (FActionIndex >= 0) and (FActionIndex < FTreeNode.ActionCount) then
  begin
    FTreeNode.ExecuteAction(FContext, FActionIndex, FHandlerList);
    if FTreeNode.Handler.GetActionMetadata(FActionIndex).RefreshAfterExecute then
      if Assigned(FRefreshProc) then
        FRefreshProc;
  end;
end;

procedure TFileTreeMenuItem.Setup(const AContext: IEditContext;
  const ANode: TFileTreeNode; const AActionIndex: Integer;
  const ASelection: TFileNodeHandlerList; const ARefreshProc: TProc);
begin
  Assert(Assigned(AContext));
  Assert(Assigned(ANode));

  FContext := AContext;
  FTreeNode := ANode;
  FActionIndex := AActionIndex;
  FHandlerList.Clear;
  FHandlerList.AddRange(ASelection);
  FRefreshProc := ARefreshProc;
  RefreshProperties;
end;

constructor TFileTreeMenuItem.Create(AOwner: TComponent);
begin
  inherited;
  FTreeNode := nil;
  FActionIndex := -1;
  FHandlerList := TFileNodeHandlerList.Create;
end;

destructor TFileTreeMenuItem.Destroy;
begin
  FreeAndNil(FHandlerList);
  inherited;
end;

procedure TFileTreeMenuItem.RefreshProperties;
var
  LMetadata: TFileAction;
begin
  if Assigned(FTreeNode) and (FActionIndex >= 0) and (FActionIndex < FTreeNode.ActionCount) then
  begin
    LMetadata := FTreeNode.ActionMetadata[FActionIndex];
    Caption := LMetadata.DisplayLabel;
    Hint := LMetadata.Hint;
    ImageIndex := LMetadata.ImageIndex;
    Default := (FTreeNode.Count = 0) and (FActionIndex = 0);
    Enabled := LMetadata.IsEnabled(FHandlerList);
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
