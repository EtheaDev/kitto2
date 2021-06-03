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
unit KIDE.ModelWizardFormUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ActnList, Vcl.Menus,
  Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls, KIDE.Project, Vcl.ImgList, Vcl.CheckLst,
  Generics.Collections,
  EF.DB,
  Kitto.Metadata, Kitto.Metadata.Models,
  KIDE.BaseWizardFormUnit, KIDE.DatabaseFrameUnit, KIDE.ModelCreator,
  KIDE.ModelUpdateActionFrameUnit, KIDE.BaseFrameUnit, KIDE.NodeDesignerFrameUnit,
  System.Actions, KIDE.EditNodeBaseFrameUnit, System.ImageList;

type
  TModelWizardData = class
  private
    FConnectionName: string;
    FConnection: TEFDBConnection;
    FDBInfo: TEFDBInfo;
    procedure SetConnection(const AValue: TEFDBConnection);
    function GetDBInfo: TEFDBInfo;
  public
    property ConnectionName: string read FConnectionName write FConnectionName;
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
    procedure UpdateCaption;
  end;

  TModelWizardForm = class(TBaseWizardForm)
    DatabaseTabSheet: TTabSheet;
    SelectTabSheet: TTabSheet;
    OptionsTabSheet: TTabSheet;
    GoTabSheet: TTabSheet;
    DatabaseSplitter: TSplitter;
    DBConnectionProps: TPanel;
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
    DeleteReferencesCheckBox: TCheckBox;
    AddDetailsCheckBox: TCheckBox;
    DetailNameFilterEdit: TEdit;
    DeleteDetailsCheckBox: TCheckBox;
    ActionFramePanel: TPanel;
    UseViewsCheckBox: TCheckBox;
    N1: TMenuItem;
    EnableAllAction: TAction;
    DisableAllAction: TAction;
    DisableAll1: TMenuItem;
    EnableAll1: TMenuItem;
    PreservePhisicalNamesCheckBox: TCheckBox;
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
    procedure ActionTreeViewChange(Sender: TObject; Node: TTreeNode);
    procedure ModelsListViewResize(Sender: TObject);
    procedure EnableAllActionExecute(Sender: TObject);
    procedure DisableAllActionExecute(Sender: TObject);
  private
    FProject: TProject;
    FData: TModelWizardData;
    FSelectingAll: Boolean;
    FCreator: TModelCreator;
    FUpdateActions: TModelUpdateList;
    FModelList: TKModelList;
    FNodeDesigner: TNodeDesignerFrame;
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
    function GetActionFrame: TModelUpdateActionFrame;
    procedure UpdateActionFrame;
    function SelectedModelUpdateAction: TModelUpdateAction;
    procedure UpdateTreeCaptions;
    procedure UpdateModelListViewColumnWidth;
    function GetKeyBase: string;
    procedure SaveCurrentNodeDesigner;
    procedure CreateNodeDesigner;
    procedure NodeDesignerApply(Sender: TObject);
    procedure AcceptUdateModel(const ATableName: string;
      var Allow: Boolean);
  protected
    procedure AfterEnterPage(const ACurrentPageIndex: Integer;
      const AOldPageIndex: Integer; const AGoingForward: Boolean); override;
    procedure InitWizard; override;
    function CanGoForward: Boolean; override;
    procedure AfterLeavePage(const AOldPageIndex: Integer;
      const ACurrentPageIndex: Integer; const AGoingForward: Boolean); override;
    procedure FinishWizard; override;
    procedure BeforeLeavePage(const ACurrentPageIndex: Integer;
      const ANewPageIndex: Integer; const AGoingForward: Boolean); override;
  public
    class procedure ShowDialog(const AModelList: TKModelList);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.dfm}

uses
  Types, Winapi.CommCtrl,
  EF.Tree, EF.Localization, EF.YAML,
  Kitto.Config,
  KIDE.MRUOptions, KIDE.WaitFormUnit;

const
  PAGE_OPTIONS = 0;
  PAGE_DB = 1;
  PAGE_SELECT = 2;
  PAGE_GO = 3;

{ TModelWizardForm }

procedure TModelWizardForm.UpdateDBList(const ASelectDBName: string);
begin
  DatabaseFrame.UpdateDBList(ASelectDBName);
  UpdateDBConnectionProps;
end;

procedure TModelWizardForm.UpdateDBConnectionProps;
var
  LNode: TEFNode;
begin
  LNode := DatabaseFrame.CurrentDBConnection;

  if Assigned(FNodeDesigner) and (FNodeDesigner.EditNode = LNode) then
    SaveCurrentNodeDesigner;
  CreateNodeDesigner;
end;

procedure TModelWizardForm.SaveCurrentNodeDesigner;
begin
  if Assigned(FNodeDesigner) then
    FNodeDesigner.Apply;
end;

procedure TModelWizardForm.CreateNodeDesigner;
var
  LNode: TEFNode;
begin
  FreeAndNil(FNodeDesigner);

  LNode := DatabaseFrame.CurrentDBConnection;
  if Assigned(LNode) then
  begin
    FNodeDesigner := TNodeDesignerFrameFactory.Instance.CreateDesignerFrame(LNode, Self);
    FNodeDesigner.Parent := DBConnectionProps;
    FNodeDesigner.Align := alClient;
    FNodeDesigner.OnApply := NodeDesignerApply;
  end;
end;

procedure TModelWizardForm.NodeDesignerApply(Sender: TObject);
begin
  with FProject.Config.Config as TEFPersistentTree do
    SaveToYamlFile(PersistentFileName);
  DatabaseFrame.UpdateCurrentDBConnection;
end;

procedure TModelWizardForm.InitOptions;
var
  I: Integer;
  LModel: TKModel;
begin
  // Init list of models.
  ModelsListView.Clear;
  for I := 0 to FProject.Config.Models.ModelCount - 1 do
  begin
    LModel := FProject.Config.Models[I];
    ModelsListView.AddItem(LModel.ModelName, LModel);
  end;
  LoadOptionsMRU;
end;

procedure TModelWizardForm.CheckAllModels(const AChecked: Boolean);
var
  LItem: TListItem;
begin
  for LItem in ModelsListView.Items do
    LItem.Checked := AChecked;
end;

function TModelWizardForm.GetKeyBase: string;
begin
  if Assigned(FModelList) then
    Result := 'ModelWizard/Partial/'
  else
    Result := 'ModelWizard/';
end;

procedure TModelWizardForm.LoadOptionsMRU;
var
  I: Integer;
  LChecked: TStrings;
  LItem: TListItem;
  LKeyBase: string;
begin
  LChecked := TStringList.Create;
  try
    if Assigned(FModelList) then
    begin
      FModelList.AddModelNamesToStrings(LChecked);
      for I := 0 to LChecked.Count - 1 do
        LChecked[I] := 'Item=' + LChecked[I];
    end
    else
      FProject.RetrieveMRUStrings('ModelWizard/ModelsToUpdate', LChecked);
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
  ModelsSelectCheckBox.State := TCheckBoxState(FProject.RetrieveMRUInteger(LKeyBase + 'ModelsSelectCheckBox', Ord(cbGrayed)));

  // Init other options.
  LKeyBase := GetKeyBase;
  if Assigned(FModelList) then
  begin
    AddModelsCheckBox.Checked := FProject.RetrieveMRUBoolean(LKeyBase + 'AddModels', False);
    ModelNameFilterEdit.Text := FProject.RetrieveMRUString(LKeyBase + 'ModelNameFilter', '*');
  end
  else
  begin
    AddModelsCheckBox.Checked := FProject.RetrieveMRUBoolean(LKeyBase + 'AddModels', True);
    ModelNameFilterEdit.Text := FProject.RetrieveMRUString(LKeyBase + 'ModelNameFilter', '*');
  end;
  UpdateModelsCheckBox.Checked := FProject.RetrieveMRUBoolean(LKeyBase + 'UpdateModels', True);
  DeleteModelsCheckBox.Checked := FProject.RetrieveMRUBoolean(LKeyBase + 'DeleteModels', False);
  DeleteFieldsCheckBox.Checked := FProject.RetrieveMRUBoolean(LKeyBase + 'DeleteFields', False);
  DeleteReferencesCheckBox.Checked := FProject.RetrieveMRUBoolean(LKeyBase + 'DeleteReferences', False);
  AddDetailsCheckBox.Checked := FProject.RetrieveMRUBoolean(LKeyBase + 'AddDetails', True);
  DetailNameFilterEdit.Text := FProject.RetrieveMRUString(LKeyBase + 'DetailNameFilter', 'DT_*');
  DeleteDetailsCheckBox.Checked := FProject.RetrieveMRUBoolean(LKeyBase + 'DeleteDetails', False);
  UseViewsCheckBox.Checked := FProject.RetrieveMRUBoolean(LKeyBase + 'UseViews', False);
  PreservePhisicalNamesCheckBox.Checked := FProject.RetrieveMRUBoolean(LKeyBase + 'PreservePhisicalNames', False);
end;

procedure TModelWizardForm.ModelsListViewChange(Sender: TObject;
  Item: TListItem; Change: TItemChange);
begin
  inherited;
  if not FSelectingAll then
    ModelsSelectCheckBox.State := cbGrayed;
end;

procedure TModelWizardForm.ModelsListViewResize(Sender: TObject);
begin
  inherited;
  UpdateModelListViewColumnWidth;
end;

procedure TModelWizardForm.UpdateModelListViewColumnWidth;
begin
  ModelsListView.Perform(LVM_SETCOLUMNWIDTH, 0, ModelsListView.ClientWidth);
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
  LKeyBase: string;
begin
  if not Assigned(FModelList) then
  begin
    LChecked := TStringList.Create;
    try
      for I := 0 to ModelsListView.Items.Count - 1 do
        if ModelsListView.Items[I].Checked then
          LChecked.Add('Item=' + ModelsListView.Items[I].Caption);
      FProject.StoreMRUStrings('ModelWizard/ModelsToUpdate', LChecked);
    finally
      FreeAndNil(LChecked);
    end;
  end;

  LKeyBase := GetKeyBase;
  FProject.StoreMRUInteger(LKeyBase + 'ModelsSelectCheckBox', Ord(ModelsSelectCheckBox.State));
  FProject.StoreMRUBoolean(LKeyBase + 'AddModels', AddModelsCheckBox.Checked);
  FProject.StoreMRUString(LKeyBase + 'ModelNameFilter', ModelNameFilterEdit.Text);
  FProject.StoreMRUBoolean(LKeyBase + 'UpdateModels', UpdateModelsCheckBox.Checked);
  FProject.StoreMRUBoolean(LKeyBase + 'DeleteModels', DeleteModelsCheckBox.Checked);
  FProject.StoreMRUBoolean(LKeyBase + 'DeleteFields', DeleteFieldsCheckBox.Checked);
  FProject.StoreMRUBoolean(LKeyBase + 'DeleteReferences', DeleteReferencesCheckBox.Checked);
  FProject.StoreMRUBoolean(LKeyBase + 'AddDetails', AddDetailsCheckBox.Checked);
  FProject.StoreMRUString(LKeyBase + 'DetailNameFilter', DetailNameFilterEdit.Text);
  FProject.StoreMRUBoolean(LKeyBase + 'DeleteDetails', DeleteDetailsCheckBox.Checked);
  FProject.StoreMRUBoolean(LKeyBase + 'UseViews', UseViewsCheckBox.Checked);
  FProject.StoreMRUBoolean(LKeyBase + 'PreservePhisicalNames', PreservePhisicalNamesCheckBox.Checked);
end;

procedure TModelWizardForm.UpdateActionTree;
var
  I: Integer;
begin
  ShowDefaultWaitForm(_('Building Model Update List. Please wait...'));
  try
    ActionTreeView.OnChange := nil;
    try
      GetActionFrame.Free;
      FData.Connection.Open;
      LockWindowUpdate(ActionTreeView.Handle);
      try
        ActionTreeView.Items.Clear;
        FreeAndNil(FUpdateActions);
        { TODO : maybe display a waiting message }
        FUpdateActions := FCreator.CreateModelUpdateList(FData.ConnectionName,
          FProject.Config.Models, FData.DBInfo, FData.Options);

        for I := 0 to FUpdateActions.Count - 1 do
          AddUpdateActionTreeNode(nil, FUpdateActions[I]);
        ActionTreeView.FullExpand;
        if ActionTreeView.Items.Count > 0 then
          ActionTreeView.Selected := ActionTreeView.Items[0];
        UpdateActionFrame;
      finally
        LockWindowUpdate(0);
      end;
    finally
      ActionTreeView.OnChange := ActionTreeViewChange;
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

procedure TModelWizardForm.AcceptUdateModel(const ATableName: string; var Allow: Boolean);
var
  I: Integer;
begin
  if ModelsSelectCheckBox.Checked then
    Allow := True
  else
  begin
    for I := 0 to ModelsListView.Items.Count - 1 do
      if SameText(ModelsListView.items[I].Caption, ATableName) then
      begin
        Allow := ModelsListView.Items[I].Checked;
        Exit;
      end;
  end;
end;

procedure TModelWizardForm.ActionTreeViewChange(Sender: TObject;
  Node: TTreeNode);
begin
  inherited;
  UpdateActionFrame;
end;

function TModelWizardForm.GetActionFrame: TModelUpdateActionFrame;
begin
  if ActionFramePanel.ControlCount > 0 then
    Result := ActionFramePanel.Controls[0] as TModelUpdateActionFrame
  else
    Result := nil;
end;

procedure TModelWizardForm.UpdateActionFrame;
var
  LFrame: TModelUpdateActionFrame;
  LAction: TModelUpdateAction;
begin
  LFrame := GetActionFrame;
  if Assigned(LFrame) then
  begin
    LFrame.SaveToAction;
    UpdateTreeCaptions;
    FreeAndNil(LFrame);
  end;
  LAction := SelectedModelUpdateAction;
  if Assigned(LAction) then
    CreateModelUpdateActionFrame(LAction, ActionFramePanel);
end;

procedure TModelWizardForm.UpdateTreeCaptions;
var
  I: Integer;
begin
  for I := 0 to ActionTreeView.Items.Count - 1 do
    (ActionTreeView.Items[I] as TModelUpdateActionTreeNode).UpdateCaption;
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
  if ACurrentPageIndex = PAGE_OPTIONS then
  begin
    PageTitle := _('Select Models to update and other options');
    if AGoingForward then
    begin
      InitOptions;
      UpdateModelListViewColumnWidth;
    end;
  end
  else if ACurrentPageIndex = PAGE_DB then
  begin
    PageTitle := _('Select a Database Connection');
    if AGoingForward then
      UpdateDBList(FProject.RetrieveMRUString('ModelWizard/DefaultDatabaseName'))
    else
      UpdateDBList(DatabaseFrame.CurrentDBConnectionName);
  end
  else if ACurrentPageIndex = PAGE_SELECT then
  begin
    PageTitle := _('Customize Model Update Actions');
    UpdateActionTree;
  end
  else if ACurrentPageIndex = PAGE_GO then
  begin
    PageTitle := _('Updating Models...');
    if AGoingForward then
      UpdateModels;
  end;
end;

procedure TModelWizardForm.AfterLeavePage(const AOldPageIndex,
  ACurrentPageIndex: Integer; const AGoingForward: Boolean);
begin
  inherited;
  if (AOldPageIndex = PAGE_OPTIONS) and AGoingForward then
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
    FData.Options.UseViews := UseViewsCheckBox.Checked;
    FData.Options.PreservePhisicalNames := PreservePhisicalNamesCheckBox.Checked;
    FData.Options.AcceptUdateModel := AcceptUdateModel;
    SaveOptionsMRU;
  end
  else if (AOldPageIndex = PAGE_DB) and AGoingForward then
  begin
    FData.ConnectionName := DatabaseFrame.CurrentDBConnectionName;
    FData.FConnection := FProject.Config.CreateDBConnection(DatabaseFrame.CurrentDBConnectionName);
    FProject.StoreMRUString('ModelWizard/DefaultDatabaseName', DatabaseFrame.CurrentDBConnectionName);
  end;
end;

procedure TModelWizardForm.BeforeLeavePage(const ACurrentPageIndex,
  ANewPageIndex: Integer; const AGoingForward: Boolean);
var
  LFrame: TModelUpdateActionFrame;
begin
  inherited;
  if (ACurrentPageIndex = PAGE_SELECT) and AGoingForward then
  begin
    LFrame := GetActionFrame;
    if Assigned(LFrame) then
    begin
      LFrame.SaveToAction;
      FreeAndNil(LFrame);
    end;
  end
  else if (ACurrentPageIndex = PAGE_GO) and not AGoingForward then
  begin
    GetActionFrame.Free;
    FreeAndNil(FUpdateActions);
    // Revert all in-memory changes.
    FProject.Config.Models.Open;
  end;
end;

function TModelWizardForm.CanGoForward: Boolean;
begin
  if PageIndex = PAGE_DB then
    Result := DatabaseFrame.CurrentDBConnectionName <> ''
  else if PageIndex = PAGE_SELECT then
    Result := ActionTreeView.Items.Count > 0
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

procedure TModelWizardForm.DisableAllActionExecute(Sender: TObject);
var
  I: Integer;
begin
  inherited;
  for I := 0 to ActionTreeView.Items.Count - 1 do
    with (ActionTreeView.Items[I] as TModelUpdateActionTreeNode) do
      if Enabled then
        IsActive := False;
end;

procedure TModelWizardForm.EnableActionExecute(Sender: TObject);
begin
  inherited;
  SelectedTreeNode.ToggleActive;
end;

procedure TModelWizardForm.EnableAllActionExecute(Sender: TObject);
var
  I: Integer;
begin
  inherited;
  for I := 0 to ActionTreeView.Items.Count - 1 do
    with (ActionTreeView.Items[I] as TModelUpdateActionTreeNode) do
      if Enabled then
        IsActive := True;
end;

procedure TModelWizardForm.InitWizard;
begin
  Assert(Assigned(TProject.CurrentProject));

  FProject := TProject.CurrentProject;
  FProject.RefreshConfigs;
  DatabaseFrame.Config := FProject.Config;
  inherited;
end;

class procedure TModelWizardForm.ShowDialog(const AModelList: TKModelList);
var
  LForm: TModelWizardForm;
begin
  LForm := TModelWizardForm.Create(Application);
  try
    LForm.FModelList := AModelList;
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

  DisableAllAction.Visible := ActionTreeView.Items.Count > 0;
  EnableAllAction.Visible := ActionTreeView.Items.Count > 0;
end;

function TModelWizardForm.SelectedTreeNode: TModelUpdateActionTreeNode;
begin
  Result := ActionTreeView.Selected as TModelUpdateActionTreeNode;
end;

function TModelWizardForm.SelectedModelUpdateAction: TModelUpdateAction;
var
  LNode: TModelUpdateActionTreeNode;
begin
  LNode := SelectedTreeNode;
  if Assigned(LNode) then
    Result := LNode.UpdateAction
  else
    Result := nil;
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

procedure TModelUpdateActionTreeNode.UpdateCaption;
var
  I: Integer;
begin
  Text := FUpdateAction.AsString;
  for I := 0 to Count - 1 do
    (Item[I] as TModelUpdateActionTreeNode).UpdateCaption;
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
