unit KIDE.ModelWizardFormUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, KIDE.BaseWizardFormUnit, Vcl.ActnList,
  Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls, KIDE.Project, Vcl.ImgList, EF.DB,
  KIDE.DatabaseFrameUnit, Vcl.CheckLst, KIDE.ModelCreator, Vcl.Menus;

type
  TModelWizardData = class
  private
    FConnection: TEFDBConnection;
    FDBInfo: TEFDBInfo;
    procedure SetConnection(const AValue: TEFDBConnection);
    function GetDBInfo: TEFDBInfo;
  public
    property Connection: TEFDBConnection read FConnection write SetConnection;
    property DBInfo: TEFDBInfo read GetDBInfo;
    var Options: TModelUpdateOptions;
    destructor Destroy; override;
  end;

  TModelUpdateActionTreeNode = class(TTreeNode)
  private
    FUpdateAction: TModelUpdateAction;
    procedure SetUpdateAction(const AValue: TModelUpdateAction);
    function GetIsActive: Boolean;
    procedure UpdateState;
    procedure SetIsActive(const AValue: Boolean);
    procedure UpdateChildrenState;
  public
    property UpdateAction: TModelUpdateAction read FUpdateAction write SetUpdateAction;
    procedure ToggleActive;
    property IsActive: Boolean read GetIsActive write SetIsActive;
  end;

  TModelWizardForm = class(TBaseWizardForm)
    DatabaseTabSheet: TTabSheet;
    SelectTabSheet: TTabSheet;
    OptionsTabSheet: TTabSheet;
    GoTabSheet: TTabSheet;
    DatabaseSplitter: TSplitter;
    DBConnectionPropsPanel: TPanel;
    DBConnectionPropsMemo: TRichEdit;
    DatabaseFrame: TDatabaseFrame;
    ModelsListView: TListView;
    OptionsLeftPanel: TPanel;
    ModelsLabel: TLabel;
    OptionsSplitter: TSplitter;
    OptionsRightPanel: TPanel;
    OptionsLabel: TLabel;
    AddModelsCheckBox: TCheckBox;
    ModelNameFilterEdit: TEdit;
    DeleteModelsCheckBox: TCheckBox;
    DeleteFieldsCheckBox: TCheckBox;
    ModelUpdateActionImages: TImageList;
    UpdateModelsCheckBox: TCheckBox;
    ModelsSelectCheckBox: TCheckBox;
    SelectLeftPanel: TPanel;
    SelectSplitter: TSplitter;
    SelectRightPanel: TPanel;
    ActionTreeView: TTreeView;
    TreeActionList: TActionList;
    TreePopupMenu: TPopupMenu;
    EnableAction: TAction;
    DisableAction: TAction;
    Disable1: TMenuItem;
    Enable1: TMenuItem;
    Label1: TLabel;
    Label3: TLabel;
    LogRichEdit: TMemo;
    Label2: TLabel;
    DeleteReferencesCheckBox: TCheckBox;
    AddDetailsCheckBox: TCheckBox;
    DetailNameFilterEdit: TEdit;
    DeleteDetailsCheckBox: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure OptionsChange(Sender: TObject);
    procedure ModelsSelectCheckBoxClick(Sender: TObject);
    procedure ModelsListViewChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure ActionTreeViewCreateNodeClass(Sender: TCustomTreeView;
      var NodeClass: TTreeNodeClass);
    procedure TreePopupMenuPopup(Sender: TObject);
    procedure EnableActionExecute(Sender: TObject);
    procedure DisableActionExecute(Sender: TObject);
    procedure ActionTreeViewMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    FProject: TProject;
    FData: TModelWizardData;
    FSelectingAll: Boolean;
    FCreator: TModelCreator;
    FUpdateActions: TModelUpdateList;
    procedure UpdateDBList(const ASelectDBName: string);
    procedure UpdateDBConnectionProps;
    procedure DatabaseChange(Sender: TObject);
    procedure InitOptions;
    procedure LoadOptionsMRU;
    procedure SaveOptionsMRU;
    procedure DatabaseListDblClick(Sender: TObject);
    procedure CheckAllModels(const AChecked: Boolean);
    procedure UpdateActionTree;
    procedure AddUpdateActionTreeNode(const AParentNode: TTreeNode;
      const AUpdateAction: TModelUpdateAction);
    function SelectedTreeNode: TModelUpdateActionTreeNode;
    procedure ClearLog;
    procedure UpdateModels;
    procedure LogString(const AString: string);
  protected
    procedure AfterEnterPage(const ACurrentPageIndex: Integer;
      const AOldPageIndex: Integer; const AGoingForward: Boolean); override;
    procedure InitWizard; override;
    function CanGoForward: Boolean; override;
    procedure AfterLeavePage(const AOldPageIndex: Integer;
      const ACurrentPageIndex: Integer; const AGoingForward: Boolean); override;
    procedure FinishWizard; override;
  public
    class procedure ShowDialog;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.dfm}

uses
  Types,
  EF.Tree, EF.Localization,
  Kitto.Config,
  KIDE.MRUOptions, KIDE.WaitFormUnit;

const
  PAGE_DB = 0;
  PAGE_OPTIONS = 1;
  PAGE_SELECT = 2;
  PAGE_GO = 3;

{ TModelWizardForm }

procedure TModelWizardForm.UpdateDBList(const ASelectDBName: string);
begin
  DatabaseFrame.UpdateDBList(FProject.Config, ASelectDBName);
  UpdateDBConnectionProps;
end;

procedure TModelWizardForm.UpdateDBConnectionProps;
begin
  DBConnectionPropsMemo.Clear;
  if DatabaseFrame.CurrentDBConnectionName <> '' then
    FProject.Config.DBConnections[DatabaseFrame.CurrentDBConnectionName].Config.GetChildrenAsStrings('Connection', DBConnectionPropsMemo.Lines);
end;

procedure TModelWizardForm.InitOptions;
var
  I: Integer;
begin
  Assert(Assigned(FData.Connection));

  // Init list of models.
  ModelsListView.Clear;
  for I := 0 to FProject.Config.Models.ModelCount - 1 do
    ModelsListView.AddItem(FProject.Config.Models[I].ModelName, FProject.Config.Models[I]);
  LoadOptionsMRU;
end;

procedure TModelWizardForm.CheckAllModels(const AChecked: Boolean);
var
  LItem: TListItem;
begin
  for LItem in ModelsListView.Items do
    LItem.Checked := AChecked;
end;

procedure TModelWizardForm.LoadOptionsMRU;
var
  I: Integer;
  LChecked: TStrings;
  LItem: TListItem;
begin
  LChecked := TStringList.Create;
  try
    FProject.RetrieveStrings('ModelWizard/ModelsToUpdate', LChecked);
    if LChecked.Count = 0 then
      CheckAllModels(True)
    else
    begin
      CheckAllModels(False);
      for I := 0 to LChecked.Count - 1 do
      begin
        LItem := ModelsListView.FindCaption(0, LChecked.ValueFromIndex[I], False, True, False);
        if Assigned(LItem) then
          LItem.Checked := True;
      end;
    end;
  finally
    FreeAndNil(LChecked);
  end;
  ModelsListView.Update;
  ModelsSelectCheckBox.State := cbGrayed;
  // Init other options.
  AddModelsCheckBox.Checked := FProject.RetrieveBoolean('ModelWizard/AddModels', True);
  ModelNameFilterEdit.Text := FProject.RetrieveString('ModelWizard/ModelNameFilter', '*');
  UpdateModelsCheckBox.Checked := FProject.RetrieveBoolean('ModelWizard/UpdateModels', True);
  DeleteModelsCheckBox.Checked := FProject.RetrieveBoolean('ModelWizard/DeleteModels', False);
  DeleteFieldsCheckBox.Checked := FProject.RetrieveBoolean('ModelWizard/DeleteFields', False);
  DeleteReferencesCheckBox.Checked := FProject.RetrieveBoolean('ModelWizard/DeleteReferences', False);
  AddDetailsCheckBox.Checked := FProject.RetrieveBoolean('ModelWizard/AddDetails', True);
  DetailNameFilterEdit.Text := FProject.RetrieveString('ModelWizard/DetailNameFilter', 'DT_*');
  DeleteDetailsCheckBox.Checked := FProject.RetrieveBoolean('ModelWizard/DeleteDetails', False);
end;

procedure TModelWizardForm.ModelsListViewChange(Sender: TObject;
  Item: TListItem; Change: TItemChange);
begin
  inherited;
  if not FSelectingAll then
    ModelsSelectCheckBox.State := cbGrayed;
end;

procedure TModelWizardForm.ModelsSelectCheckBoxClick(Sender: TObject);
begin
  inherited;
  if ModelsSelectCheckBox.State <> cbGrayed then
  begin
    FSelectingAll := True;
    try
      CheckAllModels(ModelsSelectCheckBox.Checked);
    finally
      FSelectingAll := False;
    end;
  end;
end;

procedure TModelWizardForm.OptionsChange(Sender: TObject);
begin
  inherited;
  ModelNameFilterEdit.Enabled := AddModelsCheckBox.Checked;
  DetailNameFilterEdit.Enabled := AddDetailsCheckBox.Checked;
end;

procedure TModelWizardForm.SaveOptionsMRU;
var
  I: Integer;
  LChecked: TStrings;
begin
  LChecked := TStringList.Create;
  try
    for I := 0 to ModelsListView.Items.Count - 1 do
      if ModelsListView.Items[I].Checked then
        LChecked.Add('Item=' + ModelsListView.Items[I].Caption);
    FProject.StoreStrings('ModelWizard/ModelsToUpdate', LChecked);
  finally
    FreeAndNil(LChecked);
  end;
  FProject.StoreBoolean('ModelWizard/AddModels', AddModelsCheckBox.Checked);
  FProject.StoreString('ModelWizard/ModelNameFilter', ModelNameFilterEdit.Text);
  FProject.StoreBoolean('ModelWizard/UpdateModels', UpdateModelsCheckBox.Checked);
  FProject.StoreBoolean('ModelWizard/DeleteModels', DeleteModelsCheckBox.Checked);
  FProject.StoreBoolean('ModelWizard/DeleteFields', DeleteFieldsCheckBox.Checked);
  FProject.StoreBoolean('ModelWizard/DeleteReferences', DeleteReferencesCheckBox.Checked);
  FProject.StoreBoolean('ModelWizard/AddDetails', AddDetailsCheckBox.Checked);
  FProject.StoreString('ModelWizard/DetailNameFilter', DetailNameFilterEdit.Text);
  FProject.StoreBoolean('ModelWizard/DeleteDetails', DeleteDetailsCheckBox.Checked);
end;

procedure TModelWizardForm.UpdateActionTree;
var
  I: Integer;
begin
  ShowDefaultWaitForm(_('Building Model Update List. Please wait...'));
  try
    FData.Connection.Open;
    LockWindowUpdate(ActionTreeView.Handle);
    try
      ActionTreeView.Items.Clear;
      FreeAndNil(FUpdateActions);
      { TODO : maybe display a waiting message }
      FUpdateActions := FCreator.CreateModelUpdateList(FProject.Config.Models,
        FData.DBInfo, FData.Options);

      for I := 0 to FUpdateActions.Count - 1 do
        AddUpdateActionTreeNode(nil, FUpdateActions[I]);
      ActionTreeView.FullExpand;
    finally
      LockWindowUpdate(0);
    end;
  finally
    HideDefaultWaitForm;
  end;
end;

procedure TModelWizardForm.ClearLog;
begin
  LogRichEdit.Clear;
end;

procedure TModelWizardForm.LogString(const AString: string);
begin
  LogRichEdit.Lines.Add(AString);
end;

procedure TModelWizardForm.UpdateModels;
begin
  Assert(Assigned(FUpdateActions));

  ClearLog;
  FUpdateActions.OnLog :=
    procedure (AString: string)
    begin
      LogString(AString);
    end;
  FUpdateActions.Execute;
end;

procedure TModelWizardForm.ActionTreeViewCreateNodeClass(
  Sender: TCustomTreeView; var NodeClass: TTreeNodeClass);
begin
  inherited;
  NodeClass := TModelUpdateActionTreeNode;
end;

procedure TModelWizardForm.ActionTreeViewMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if Button = mbRight then
    ActionTreeView.Selected := ActionTreeView.GetNodeAt(X, Y);
end;

procedure TModelWizardForm.AddUpdateActionTreeNode(
  const AParentNode: TTreeNode; const AUpdateAction: TModelUpdateAction);
var
  LNode: TModelUpdateActionTreeNode;
  I: Integer;
begin
  Assert(Assigned(AUpdateAction));

  LNode := ActionTreeView.Items.AddChild(AParentNode, '') as TModelUpdateActionTreeNode;
  LNode.UpdateAction := AUpdateAction;
  for I := 0 to AUpdateAction.SubActions.Count - 1 do
    AddUpdateActionTreeNode(LNode, AUpdateAction.SubActions[I]);
end;

procedure TModelWizardForm.AfterEnterPage(const ACurrentPageIndex,
  AOldPageIndex: Integer; const AGoingForward: Boolean);
begin
  inherited;
  if ACurrentPageIndex = PAGE_DB then
  begin
    PageTitle := _('Select a Database Connection');
    if AGoingForward then
      UpdateDBList(FProject.RetrieveString('ModelWizard/DefaultDatabaseName'))
    else
      UpdateDBList(DatabaseFrame.CurrentDBConnectionName);
  end
  else if ACurrentPageIndex = PAGE_OPTIONS then
  begin
    PageTitle := _('Select Models to update and other options');
    InitOptions;
  end
  else if ACurrentPageIndex = PAGE_SELECT then
  begin
    PageTitle := _('Customize Model Update Actions');
    UpdateActionTree;
  end
  else if ACurrentPageIndex = PAGE_GO then
  begin
    PageTitle := _('Updating Models...');
    Update;
    UpdateModels;
  end;
end;

procedure TModelWizardForm.AfterLeavePage(const AOldPageIndex,
  ACurrentPageIndex: Integer; const AGoingForward: Boolean);
begin
  inherited;
  if (AOldPageIndex = PAGE_DB) and AGoingForward then
  begin
    FData.FConnection := FProject.Config.DBConnections[DatabaseFrame.CurrentDBConnectionName];
    FProject.StoreString('ModelWizard/DefaultDatabaseName', DatabaseFrame.CurrentDBConnectionName);
  end
  else if (AOldPageIndex = PAGE_OPTIONS) and AGoingForward then
  begin
    FData.Options.AddModels := AddModelsCheckBox.Checked;
    FData.Options.ModelNameFilter := ModelNameFilterEdit.Text;
    FData.Options.UpdateModels := UpdateModelsCheckBox.Checked;
    FData.Options.DeleteModels := DeleteModelsCheckBox.Checked;
    FData.Options.DeleteFields := DeleteFieldsCheckBox.Checked;
    FData.Options.DeleteReferences := DeleteReferencesCheckBox.Checked;
    FData.Options.AddDetails := AddDetailsCheckBox.Checked;
    FData.Options.DetailNameFilter := DetailNameFilterEdit.Text;
    FData.Options.DeleteDetails := DeleteDetailsCheckBox.Checked;
    SaveOptionsMRU;
  end;
end;

function TModelWizardForm.CanGoForward: Boolean;
begin
  if PageIndex = PAGE_DB then
    Result := DatabaseFrame.CurrentDBConnectionName <> ''
  else
    Result := inherited CanGoForward;
end;

constructor TModelWizardForm.Create(AOwner: TComponent);
begin
  inherited;
  FCreator := TModelCreator.Create;
  FData := TModelWizardData.Create;
end;

procedure TModelWizardForm.FinishWizard;
begin
  inherited;
  FProject.Config.Models.SaveAll;
end;

procedure TModelWizardForm.FormCreate(Sender: TObject);
begin
  inherited;
  DatabaseFrame.OnChange := DatabaseChange;
  DatabaseFrame.OnDblClick := DatabaseListDblClick;
  ModelUpdateActionImages.Overlay(0, 0);
  ModelUpdateActionImages.Overlay(1, 1);
  ModelUpdateActionImages.Overlay(2, 2);
  ModelUpdateActionImages.Overlay(3, 3);
end;

procedure TModelWizardForm.DatabaseListDblClick(Sender: TObject);
begin
  ForwardAction.Execute;
end;

procedure TModelWizardForm.DatabaseChange(Sender: TObject);
begin
  UpdateDBConnectionProps;
end;

destructor TModelWizardForm.Destroy;
begin
  FreeAndNil(FCreator);
  FreeAndNil(FUpdateActions);
  FreeAndNil(FData);
  inherited;
end;

procedure TModelWizardForm.DisableActionExecute(Sender: TObject);
begin
  inherited;
  SelectedTreeNode.ToggleActive;
end;

procedure TModelWizardForm.EnableActionExecute(Sender: TObject);
begin
  inherited;
  SelectedTreeNode.ToggleActive;
end;

procedure TModelWizardForm.InitWizard;
begin
  Assert(Assigned(TProject.CurrentProject));

  FProject := TProject.CurrentProject;
  inherited;
end;

class procedure TModelWizardForm.ShowDialog;
var
  LForm: TModelWizardForm;
begin
  LForm := TModelWizardForm.Create(Application);
  try
    LForm.ShowModal;
  finally
    FreeAndNil(LForm);
  end;
end;

procedure TModelWizardForm.TreePopupMenuPopup(Sender: TObject);
begin
  inherited;
  DisableAction.Visible := (SelectedTreeNode <> nil) and SelectedTreeNode.Enabled and SelectedTreeNode.IsActive;
  EnableAction.Visible := (SelectedTreeNode <> nil) and SelectedTreeNode.Enabled and not SelectedTreeNode.IsActive;
end;

function TModelWizardForm.SelectedTreeNode: TModelUpdateActionTreeNode;
begin
  Result := ActionTreeView.Selected as TModelUpdateActionTreeNode;
end;

{ TModelWizardData }

destructor TModelWizardData.Destroy;
begin
  FreeAndNil(FDBInfo);
  inherited;
end;

function TModelWizardData.GetDBInfo: TEFDBInfo;
begin
  Assert(Assigned(FConnection));

  if not Assigned(FDBInfo) then
    FDBInfo := FConnection.CreateDBInfo;
  Result := FDBInfo;
end;

procedure TModelWizardData.SetConnection(const AValue: TEFDBConnection);
begin
  FreeAndNil(FDBInfo);
  FConnection := AValue;
end;

{ TModelUpdateActionTreeNode }

function TModelUpdateActionTreeNode.GetIsActive: Boolean;
begin
  Result := OverlayIndex < 0;
end;

procedure TModelUpdateActionTreeNode.SetIsActive(const AValue: Boolean);
begin
  if AValue then
    OverlayIndex := -1
  else
    OverlayIndex := 0;
  UpdateChildrenState;
  FUpdateAction.IsActive := AValue;
end;

procedure TModelUpdateActionTreeNode.SetUpdateAction(
  const AValue: TModelUpdateAction);
begin
  Assert(Assigned(AValue));

  FUpdateAction := AValue;
  Text := FUpdateAction.AsString;
  ImageIndex := FUpdateAction.ImageIndex;
  SelectedIndex := ImageIndex;
  IsActive := FUpdateAction.IsActive;
end;

procedure TModelUpdateActionTreeNode.ToggleActive;
begin
  IsActive := not IsActive;
end;

procedure TModelUpdateActionTreeNode.UpdateChildrenState;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    (Item[I] as TModelUpdateActionTreeNode).UpdateState;
end;

procedure TModelUpdateActionTreeNode.UpdateState;
var
  LParent: TModelUpdateActionTreeNode;
begin
  LParent := Parent as TModelUpdateActionTreeNode;
  if Assigned(LParent) then
  begin
    IsActive := LParent.IsActive;
    Enabled := LParent.IsActive;
  end;
end;

end.
