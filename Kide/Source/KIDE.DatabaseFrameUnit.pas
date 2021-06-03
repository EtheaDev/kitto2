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
unit KIDE.DatabaseFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.UITypes, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ImgList, Vcl.ComCtrls,
  Kitto.Config, Vcl.ActnList, Vcl.Menus, KIDE.EditNodeBaseFrameUnit, EF.DB, EF.Tree,
  System.Actions, KIDE.BaseFrameUnit, Vcl.ExtCtrls, System.ImageList;

type
  TDBAdapterMenuItem = class(TMenuItem)
  private
    FDBAdapter: TEFDBAdapter;
  public
    property DBAdapter: TEFDBAdapter read FDBAdapter write FDBAdapter;
  end;

  TDatabaseFrame = class(TEditNodeBaseFrame)
    DBListView: TListView;
    ImageList: TImageList;
    PopupMenu: TPopupMenu;
    AddDBAction: TAction;
    DeleteDBAction: TAction;
    NewDatabaseConnectionMenuItem: TMenuItem;
    TestDBConnectionAction: TAction;
    Delete1: TMenuItem;
    N1: TMenuItem;
    est1: TMenuItem;
    procedure AddDBActionUpdate(Sender: TObject);
    procedure DeleteDBActionUpdate(Sender: TObject);
    procedure AddDBActionExecute(Sender: TObject);
    procedure DeleteDBActionExecute(Sender: TObject);
    procedure DBListViewDblClick(Sender: TObject);
    procedure DBListViewSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure PopupMenuPopup(Sender: TObject);
    procedure TestDBConnectionActionUpdate(Sender: TObject);
    procedure TestDBConnectionActionExecute(Sender: TObject);
    procedure ActionListUpdate(Action: TBasicAction; var Handled: Boolean);
  private
    FOnChange: TNotifyEvent;
    FOnDblClick: TNotifyEvent;
    FConfig: TKConfig;
    function GetCurrentDBConnectionName: string;
    procedure DoChange;
    procedure DoDblClick;
    procedure BuildNewDatabaseConnectionMenu;
    procedure SaveConfig;
    function GetCurrentDBConnection: TEFNode;
    function GetDatabasesNode: TEFNode;
  protected
    procedure UpdateDesignPanel(const AForce: Boolean); override;
  public
    procedure Init(const ANode: TEFTree); override;
    property Config: TKConfig read FConfig write FConfig;
    property CurrentDBConnectionName: string read GetCurrentDBConnectionName;
    property CurrentDBConnection: TEFNode read GetCurrentDBConnection;
    procedure UpdateDBList(const ADefaultDBName: string);
    procedure UpdateCurrentDBConnection;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnDblClick: TNotifyEvent read FOnDblClick write FOnDblClick;
  end;

implementation

{$R *.dfm}

uses
  Types, StrUtils, KIDE.Project,
  EF.Classes, EF.YAML, EF.Localization,
  KIDE.Utils, KIDE.Config;

{ TDatabaseFrame }

procedure TDatabaseFrame.ActionListUpdate(Action: TBasicAction;
  var Handled: Boolean);
begin
  inherited;
  DeleteDBAction.Visible := not Assigned(EditNode);
end;

procedure TDatabaseFrame.AddDBActionExecute(Sender: TObject);
var
  LDBAdapter: TEFDBAdapter;
  LParentNode: TEFNode;
  LNewConnectionName: string;
  LNewConnection: TEFNode;

  function GetNewConnectionName: string;
  var
    I: Integer;
  begin
    Result := '';
    I := 1;
    while True do
    begin
      if not Assigned(LParentNode.FindNode(LDBAdapter.GetClassId + 'Connection' + IntToStr(I)))  then
        Exit(LDBAdapter.GetClassId + 'Connection' + IntToStr(I));
      Inc(I);
    end;
  end;

begin
  Assert(Assigned(FConfig));

  LDBAdapter := ((Sender as TAction).ActionComponent as TDBAdapterMenuItem).DBAdapter;
  LParentNode := GetDatabasesNode;
  LNewConnectionName := GetNewConnectionName;
  LNewConnection := LParentNode.AddChild(TEFNode.Clone(
    TKideConfig.Instance.Config.GetNode('DefaultDBConnections/' + LDBAdapter.GetClassId)));
  LNewConnection.Rename(LNewConnectionName);
  LNewConnection.AsString := LDBAdapter.GetClassId;
  SaveConfig;
  UpdateDBList(LNewConnectionName);
  IsChanged := True;
end;

procedure TDatabaseFrame.SaveConfig;
begin
  if Assigned(EditNode) then
  begin
    DesignPanelToEditNode;
  end
  else if Assigned(FConfig) then
    with FConfig.Config as TEFPersistentTree do
      SaveToYamlFile(PersistentFileName);
end;

procedure TDatabaseFrame.TestDBConnectionActionExecute(Sender: TObject);
var
  LDatabaseName: string;
  LDBConnection: TEFDBConnection;
  LDbAdapterKey: string;
  LDBAdapter: TEFDBAdapter;
  LConnection: TEFNode;

  procedure TestOpenDB;
  begin
    try
      Screen.Cursor := crHourGlass;
      LDBConnection.Open;
      Screen.Cursor := crDefault;
      try
        MessageDlg('Database connection established correctly.', mtInformation, [mbOK], 0);
      finally
        LDBConnection.Close;
      end;
    finally
      Screen.Cursor := crDefault;
    end;
  end;

begin
  inherited;
  LDatabaseName := GetCurrentDBConnectionName;
  if Assigned(EditNode) then
  begin
    Try
      LDbAdapterKey := EditNode.GetExpandedString(LDatabaseName);
      LDBAdapter := TEFDBAdapterRegistry.Instance[LDbAdapterKey];
    except
      raise Exception.CreateFmt(_('DB connection type "%s" for database "%s" not available'),
        [LDbAdapterKey, LDatabaseName]);
    end;
    LDBConnection := LDBAdapter.CreateDBConnection;
    try
      LConnection := EditNode.GetNode(LDatabaseName + '/Connection');
      LDBConnection.Config.AddChild(TEFNode.Clone(LConnection));
      TestOpenDB;
    finally
      LDBConnection.Free;
    end;
  end
  else if Assigned(FConfig) then
  begin
    LDBConnection := FConfig.CreateDBConnection(LDatabaseName);
    Try
      TestOpenDB;
    Finally
      LDBConnection.Free;
    End;
  end
  else
    raise Exception.CreateFmt('DB connection for database "%s" not found',[LDatabaseName]);
end;

procedure TDatabaseFrame.TestDBConnectionActionUpdate(Sender: TObject);
begin
  inherited;
  (Sender as TAction).Enabled := Assigned(DBListView.Selected);
end;

procedure TDatabaseFrame.AddDBActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := not Assigned(DBListView.Selected);
end;

procedure TDatabaseFrame.DBListViewDblClick(Sender: TObject);
begin
  DoDblClick;
end;

procedure TDatabaseFrame.DBListViewSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  DoChange;
end;

procedure TDatabaseFrame.DeleteDBActionExecute(Sender: TObject);
var
  LParentNode: TEFNode;
  LNode: TEFNode;
  LNewSelectedNode: string;
  LDBConnectionName: string;
begin
  Assert(Assigned(FConfig));
  LDBConnectionName := GetCurrentDBConnectionName;
  if (MessageDlg(Format('Are you sure you want to delete database connection %s?', [LDBConnectionName]),
    mtWarning, [mbYes, mbNo], 0) = mrYes) then
  begin
    LParentNode := GetDatabasesNode;
    LNode := LParentNode.GetNode(LDBConnectionName);
    if LNode.Index > 0 then
      LNewSelectedNode := LParentNode.Children[Pred(LNode.Index)].Name
    else if LNode.Index < LParentNode.ChildCount - 1 then
      LNewSelectedNode := LParentNode.Children[Succ(LNode.Index)].Name
    else
      LNewSelectedNode := '';
    LParentNode.RemoveChild(LNode);
    SaveConfig;
    UpdateDBList(LNewSelectedNode);
  end;
end;

procedure TDatabaseFrame.DeleteDBActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := Assigned(DBListView.Selected);
end;

procedure TDatabaseFrame.UpdateDesignPanel(const AForce: Boolean);
begin
  inherited;
  UpdateDBList('');
end;

procedure TDatabaseFrame.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TDatabaseFrame.DoDblClick;
begin
  if Assigned(FOnDblClick) then
    FOnDblClick(Self);
end;

function TDatabaseFrame.GetCurrentDBConnection: TEFNode;
begin
  if Assigned(DBListView.Selected) then
    Result := TEFNode(DBListView.Selected.Data)
  else
    Result := nil;
end;

function TDatabaseFrame.GetCurrentDBConnectionName: string;
begin
  if Assigned(DBListView.Selected) then
    Result := DBListView.Selected.Caption
  else
    Result := '';
end;

function TDatabaseFrame.GetDatabasesNode: TEFNode;
begin
  Assert(Assigned(FConfig));
  if Assigned(EditNode) then
    Result := EditNode as TEFNode
  else
    Result := FConfig.Config.GetNode('Databases');
end;

procedure TDatabaseFrame.Init(const ANode: TEFTree);
begin
  inherited;
  FConfig := TProject.CurrentProject.Config;
end;

procedure TDatabaseFrame.PopupMenuPopup(Sender: TObject);
begin
  inherited;
  AddDBAction.Update;
  NewDatabaseConnectionMenuItem.Enabled := AddDBAction.Enabled;
  BuildNewDatabaseConnectionMenu;
end;

procedure TDatabaseFrame.BuildNewDatabaseConnectionMenu;
var
  I: Integer;
  LMenuItem: TDBAdapterMenuItem;
begin
  NewDatabaseConnectionMenuItem.Clear;
  if NewDatabaseConnectionMenuItem.Enabled then
  begin
    for I := 0 to TEFDBAdapterRegistry.Instance.DBAdapterCount - 1 do
    begin
      LMenuItem := TDBAdapterMenuItem.Create(Self);
      try
        LMenuItem.DBAdapter := TEFDBAdapterRegistry.Instance.DBAdaptersByIndex[I];
        LMenuItem.Action := AddDBAction;
        LMenuItem.Caption := Format(AddDBAction.Caption, [LMenuItem.DBAdapter.GetClassId]);
        NewDatabaseConnectionMenuItem.Add(LMenuItem);
      except
        FreeAndNil(LMenuItem);
        raise;
      end;
    end;
  end;
end;

procedure TDatabaseFrame.UpdateCurrentDBConnection;
begin
  if Assigned(DBListView.Selected) then
    DBListView.Selected.Caption := TEFNode(DBListView.Selected.Data).Name;
end;

procedure TDatabaseFrame.UpdateDBList(const ADefaultDBName: string);
var
  I: Integer;
  LItem: TListItem;
  LParentNode: TEFNode;
  LDBAdapterType: string;
  LSelectItemHandler: TLVSelectItemEvent;
begin
  Assert(Assigned(FConfig));

  LSelectItemHandler := DBListView.OnSelectItem;
  DBListView.OnSelectItem := nil;
  try
    LParentNode := GetDatabasesNode;
    DBListView.Clear;
    for I := 0 to LParentNode.ChildCount - 1 do
    begin
      LItem := DBListView.Items.Add;
      LItem.Caption := LParentNode.Children[I].Name;
      LItem.Data := LParentNode.Children[I];
      LDBAdapterType := LParentNode.Children[I].AsExpandedString;
      if SameText(LDBAdapterType, 'ADO') then
        LItem.ImageIndex := 1
      else if SameText(LDBAdapterType, 'DBX') then
        LItem.ImageIndex := 2
      else if SameText(LDBAdapterType, 'FD') then
        LItem.ImageIndex := 3
      else
        LItem.ImageIndex := 0;
    end;
    if DBListView.Items.Count > 0 then
    begin
      DBListView.Selected := DBListView.FindCaption(0,
        IfThen(ADefaultDBName <> '', ADefaultDBName, FConfig.DatabaseName),
        False, True, False);
      if DBListView.Selected = nil then
        DBListView.Selected := DBListView.Items[0];
      DBListView.ItemFocused := DBListView.Selected;
      if DBListView.CanFocus then
        DBListView.SetFocus;
    end;
  finally
    DBlistView.OnSelectItem := LSelectItemHandler;
  end;
end;

end.
