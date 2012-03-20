unit KIDE.DataViewWizardFormUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, KIDE.BaseWizardFormUnit, Vcl.ActnList,
  Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls, Kitto.Metadata.Models,
  Kitto.Metadata.Views, Kitto.Metadata.DataView, Vcl.ImgList;

type
  TDataViewTreeNode = class(TTreeNode)
  strict private
    FView: TKDataView;
    FViewTable: TKViewTable;
  public
    property View: TKDataView read FView;
    property ViewTable: TKViewTable read FViewTable;
    procedure Initialize(const AView: TKDataView; const AViewTable: TKViewTable);
    procedure Rename(const ANewName: string);
    function CanRename: Boolean;
    function IsValid: Boolean;
  end;

  TDataViewWizardForm = class(TBaseWizardForm)
    SelectTabSheet: TTabSheet;
    SelectLeftPanel: TPanel;
    DataViewTreeView: TTreeView;
    SelectRightPanel: TPanel;
    Label1: TLabel;
    ActionFramePanel: TPanel;
    SelectSplitter: TSplitter;
    TreeImages: TImageList;
    Label2: TLabel;
    procedure DataViewTreeViewMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DataViewTreeViewCreateNodeClass(Sender: TCustomTreeView;
      var NodeClass: TTreeNodeClass);
    procedure DataViewTreeViewEdited(Sender: TObject; Node: TTreeNode;
      var S: string);
    procedure DataViewTreeViewEditing(Sender: TObject; Node: TTreeNode;
      var AllowEdit: Boolean);
    procedure DataViewTreeViewCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure DataViewTreeViewKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    FModelList: TKModelList;
    FViews: TKViews;
    procedure CreateDataViews;
    procedure CreateDataViewsFromModelList;
    procedure DisplayDataViews;
    procedure AddViewTableTreeNode(const AParentNode: TDataViewTreeNode;
      const AView: TKDataView; const AViewTable: TKViewTable);
    procedure SaveDataViews;
    procedure ValidateDataViews;
  protected
    procedure InitWizard; override;
    procedure AfterEnterPage(const ACurrentPageIndex: Integer;
      const AOldPageIndex: Integer; const AGoingForward: Boolean); override;
    procedure BeforeLeavePage(const ACurrentPageIndex: Integer;
      const ANewPageIndex: Integer; const AGoingForward: Boolean); override;
    function CanGoForward: Boolean; override;
  public
    class procedure ShowDialog(const AModelList: TKModelList);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.dfm}

uses
  EF.Localization, EF.Macros, EF.SysUtils,
  Kitto.Metadata.ViewBuilders,
  KIDE.Project;

const
  WIZPAGE_SELECT = 0;

{ TDataViewWizardForm }

procedure TDataViewWizardForm.ValidateDataViews;
var
  I: Integer;
  LView: TKDataView;
begin
  Assert(Assigned(TProject.CurrentProject));
  Assert(FViews.ViewCount > 0);

  for I := 0 to FViews.ViewCount - 1 do
  begin
    LView := TKDataView.Clone(FViews.Views[I]);
    if Assigned(TProject.CurrentProject.Config.Views.FindView(LView.PersistentName)) then
      raise Exception.CreateFmt(_('View %s has a duplicate name.'), [LView.PersistentName]);
  end;
end;

procedure TDataViewWizardForm.SaveDataViews;
var
  I: Integer;
  LView: TKDataView;
begin
  Assert(Assigned(TProject.CurrentProject));
  Assert(FViews.ViewCount > 0);

  for I := 0 to FViews.ViewCount - 1 do
  begin
    LView := TKDataView.Clone(FViews.Views[I]);
    TProject.CurrentProject.Config.Views.AddObject(LView);
    TProject.CurrentProject.Config.Views.SaveObject(LView);
  end;
end;

procedure TDataViewWizardForm.CreateDataViewsFromModelList;
var
  I: Integer;
  LViewBuilder: TKAutoListViewBuilder;
begin
  Assert(Assigned(FModelList));
  Assert(FModelList.Count > 0);

  for I := 0 to FModelList.Count - 1 do
  begin
    LViewBuilder := TKAutoListViewBuilder.Create;
    try
      LViewBuilder.SetString('Model', FModelList[I].ModelName);
      LViewBuilder.BuildView(FViews, Pluralize(FModelList[I].ModelName));
    except
      FreeAndNil(LViewBuilder);
      raise;
    end;
  end;
end;

procedure TDataViewWizardForm.CreateDataViews;
begin
  FViews.Open;
  if FViews.ViewCount > 0 then
  begin
    if MessageDlg(_('Unsaved views from an earlier session were found. If you want to restore them, click Yes. If you want to discard them and start anew, click No.'), mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
    begin
      FViews.Close;
      DeleteTree(FViews.Path);
      FViews.Open;
      CreateDataViewsFromModelList;
    end;
  end
  else
    CreateDataViewsFromModelList;
end;

procedure TDataViewWizardForm.DisplayDataViews;
var
  I: Integer;
  LNode: TDataViewTreeNode;
  LView: TKDataView;
begin
  Assert(Assigned(FViews));

  DataViewTreeView.Items.Clear;

  DataViewTreeView.Items.BeginUpdate;
  try
    for I := 0 to FViews.ViewCount - 1 do
    begin
      LView := FViews[I] as TKDataView;
      LNode := DataViewTreeView.Items.AddChild(nil, LView.DisplayLabel) as TDataViewTreeNode;
      LNode.Initialize(LView, nil);
      AddViewTableTreeNode(LNode, LView, LView.MainTable);
    end;
    DataViewTreeView.FullExpand;
    if DataViewTreeView.Items.Count > 0 then
      DataViewTreeView.Select(DataViewTreeView.Items[0]);
  finally
    DataViewTreeView.Items.EndUpdate;
  end;
end;

procedure TDataViewWizardForm.AddViewTableTreeNode(const AParentNode: TDataViewTreeNode;
  const AView: TKDataView; const AViewTable: TKViewTable);
var
  LNode: TDataViewTreeNode;
  I: Integer;
begin
  Assert(Assigned(AParentNode));
  Assert(Assigned(AView));
  Assert(Assigned(AViewTable));
  Assert(AParentNode.View = AView);
  Assert(AViewTable.View = AView);
  Assert(AParentNode.ViewTable = AViewTable.MasterTable);

  LNode := (AParentNode.TreeView as TTreeView).Items.AddChild(AParentNode, AViewTable.ModelName) as TDataViewTreeNode;
  LNode.Initialize(AView, AViewTable);
  if AViewTable.MasterTable <> nil then
    LNode.ImageIndex := 2
  else
    LNode.ImageIndex := 1;
  LNode.SelectedIndex := LNode.ImageIndex;
  for I := 0 to AViewTable.DetailTableCount - 1 do
    AddViewTableTreeNode(LNode, AView, AViewTable.DetailTables[I]);
end;

procedure TDataViewWizardForm.AfterEnterPage(const ACurrentPageIndex,
  AOldPageIndex: Integer; const AGoingForward: Boolean);
begin
  inherited;
  if ACurrentPageIndex = WIZPAGE_SELECT then
  begin
    PageTitle := _('Data Views to be created');
    if AGoingForward then
    begin
      CreateDataViews;
      DisplayDataViews;
    end;
  end;
end;

procedure TDataViewWizardForm.BeforeLeavePage(const ACurrentPageIndex,
  ANewPageIndex: Integer; const AGoingForward: Boolean);
begin
  inherited;
  if (ACurrentPageIndex = WIZPAGE_SELECT) and AGoingForward then
  begin
    ValidateDataViews;
    if MessageDlg(Format(_('Data Views to be created: %d. Are you sure you want to continue?'), [FViews.ViewCount]), mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
      Abort;
    SaveDataViews;
  end;
end;

function TDataViewWizardForm.CanGoForward: Boolean;
begin
  if PageControl.ActivePageIndex = WIZPAGE_SELECT then
    Result := FViews.ViewCount > 0
  else
    Result := inherited CanGoForward;
end;

constructor TDataViewWizardForm.Create(AOwner: TComponent);
begin
  inherited;
  Assert(Assigned(TProject.CurrentProject));

  FViews := TKViews.Create(TProject.CurrentProject.Config.Models);
  FViews.Path := TEFMacroExpansionEngine.Instance.Expand('%APPDATA%\KIDE\DataViewWizard\' + TProject.CurrentProject.GetMRUKeyName);
  ForceDirectories(FViews.Path);
end;

procedure TDataViewWizardForm.DataViewTreeViewCreateNodeClass(
  Sender: TCustomTreeView; var NodeClass: TTreeNodeClass);
begin
  inherited;
  NodeClass := TDataViewTreeNode;
end;

procedure TDataViewWizardForm.DataViewTreeViewCustomDrawItem(
  Sender: TCustomTreeView; Node: TTreeNode; State: TCustomDrawState;
  var DefaultDraw: Boolean);
begin
  inherited;
  DefaultDraw := True;
  if State = [] then
  begin
    if not (Node as TDataViewTreeNode).IsValid then
      (Sender as TTreeView).Canvas.Font.Color := clRed;
  end;
end;

procedure TDataViewWizardForm.DataViewTreeViewEdited(Sender: TObject;
  Node: TTreeNode; var S: string);
begin
  inherited;
  (Node as TDataViewTreeNode).Rename(S);
  (Sender as TTreeView).Repaint;
end;

procedure TDataViewWizardForm.DataViewTreeViewEditing(Sender: TObject;
  Node: TTreeNode; var AllowEdit: Boolean);
begin
  inherited;
  AllowEdit := (Node as TDataViewTreeNode).CanRename;
end;

procedure TDataViewWizardForm.DataViewTreeViewKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  inherited;
  if (Key = VK_F2) and (Shift = []) and Assigned((Sender as TTreeView).Selected) then
    (Sender as TTreeView).Selected.EditText;
end;

procedure TDataViewWizardForm.DataViewTreeViewMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if Button = mbRight then
    DataViewTreeView.Selected := DataViewTreeView.GetNodeAt(X, Y);
end;

destructor TDataViewWizardForm.Destroy;
begin
  FViews.Close;
  DeleteTree(FViews.Path);
  FreeAndNil(FViews);
  inherited;
end;

procedure TDataViewWizardForm.InitWizard;
begin
  inherited;
  Assert(Assigned(FModelList));
end;

class procedure TDataViewWizardForm.ShowDialog(const AModelList: TKModelList);
var
  LForm: TDataViewWizardForm;
begin
  LForm := TDataViewWizardForm.Create(Application);
  try
    LForm.FModelList := AModelList;
    LForm.ShowModal;
  finally
    FreeAndNil(LForm);
  end;
end;

{ TDataViewTreeNode }

function TDataViewTreeNode.CanRename: Boolean;
begin
  // Only allow to rename views, not view tables.
  Result := FViewTable = nil;
end;

procedure TDataViewTreeNode.Initialize(const AView: TKDataView;
  const AViewTable: TKViewTable);
begin
  Assert(Assigned(AView));

  FView := AView;
  FViewTable := AViewTable;
end;

function TDataViewTreeNode.IsValid: Boolean;
begin
  Assert(Assigned(FView));

  Result := True;

  if Assigned(TProject.CurrentProject.Config.Views.FindView(FView.PersistentName)) then
    Result := False;
end;

procedure TDataViewTreeNode.Rename(const ANewName: string);
begin
  Assert(Assigned(FView));

  FView.PersistentName := ANewName;
end;

end.
