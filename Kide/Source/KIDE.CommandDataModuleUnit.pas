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
unit KIDE.CommandDataModuleUnit;

interface

uses
  System.UITypes, System.SysUtils, System.Classes, Generics.Collections,
  Vcl.ActnList, Vcl.Menus, Vcl.PlatformDefaultStyleActnCtrls,
  Vcl.ActnPopup, Vcl.Dialogs,
  EF.Tree, System.Actions;

type
  TCommandDataModule = class;

  TAfterExecuteFlag = (aeNodeChanged, aeNodeAndChildrenChanged, aeNodeDeleted);
  TAfterExecute = procedure(const ASender: TCommandDataModule;
    const ANode: TEFTree; const AFlag: TAfterExecuteFlag) of object;

  TBeforeExecuteFlag = (beNodeAdding, beNodeAddingChildren, beNodeChanging, beNodeDeleting);
  TBeforeExecute = procedure(const ASender: TCommandDataModule;
    const ANode: TEFTree; const AFlag: TBeforeExecuteFlag) of object;

  TCommandDataModule = class(TDataModule)
    PopupActionBar: TPopupActionBar;
    ActionList: TActionList;
    AddChildAction: TAction;
    AddChildActionMenuItem: TMenuItem;
    DeleteNodeAction: TAction;
    DeleteNodeActionMenuItem: TMenuItem;
    procedure AddChildActionUpdate(Sender: TObject);
    procedure AddChildActionExecute(Sender: TObject);
    procedure DeleteNodeActionUpdate(Sender: TObject);
    procedure DeleteNodeActionExecute(Sender: TObject);
  private
    FMetadataConfig: TEFPersistentTree;
    FNode: TEFTree;
    FSelection: TList<TEFNode>;
    FAfterExecute: TAfterExecute;
    FBeforeExecute: TBeforeExecute;
    FCommandsOnAllNodes: Boolean;
    procedure UpdateActions;
    procedure RebuildPopupMenu;
    procedure DoAfterExecute(const ANode: TEFTree; const AFlag: TAfterExecuteFlag);
    procedure DoBeforeExecute(const ANode: TEFTree; const AFlag: TBeforeExecuteFlag);
    procedure AddNodeExecute(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    ///	<summary>If True, commands that would normally be disabled on leaf
    ///	nodes are enabled. This property and the ShowAllNodes property of the
    ///	tree frame go together.</summary>
    property CommandsOnAllNodes: Boolean read FCommandsOnAllNodes write FCommandsOnAllNodes;

    property Node: TEFTree read FNode;

    function GetPopupMenu(const ANode: TEFTree; const ASelection: TList<TEFNode>): TPopupMenu;
    property AfterExecute: TAfterExecute read FAfterExecute write FAfterExecute;
    property BeforeExecute: TBeforeExecute read FBeforeExecute write FBeforeExecute;
  end;

implementation

{$R *.dfm}

uses
  EF.Localization, EF.YAML, KIDE.Utils,
  KIDE.Config, KIDE.MainDataModuleUnit, KIDE.DesignMetadata;

{ TCommandDataModule }

procedure TCommandDataModule.AddChildActionExecute(Sender: TObject);
var
  LNode: TEFNode;
begin
  Assert(Assigned(FNode));

  DoBeforeExecute(FNode, beNodeAddingChildren);
  LNode := FNode.AddChild('NewNode');
  LNode.Value := '';
  DoAfterExecute(FNode, aeNodeAndChildrenChanged);
end;

procedure TCommandDataModule.AddChildActionUpdate(Sender: TObject);
var
  LIsDesignLeaf: Boolean;
begin
  if FNode is TEFNode then
    LIsDesignLeaf := TEFNode(FNode).IsDesignLeaf
  else
    LIsDesignLeaf := False;
  (Sender as TAction).Visible := Assigned(FNode) and FNode.CanAddChild
    and (not LIsDesignLeaf or FCommandsOnAllNodes);
end;

procedure TCommandDataModule.AddNodeExecute(Sender: TObject);
var
  LMenuItem: TMenuItem;
  LNode, LNewNode, LTemplateNode: TEFNode;
begin
  LMenuItem := Sender as TMenuItem;
  LNode := FMetadataConfig.Children[LMenuItem.Tag];

  DoBeforeExecute(FNode, beNodeAdding);

  LNewNode := FNode.AddChild(LNode.Name);
  LTemplateNode := LNode.FindNode('Template');
  if Assigned(LTemplateNode) then
  begin
    LNewNode.Clone(LTemplateNode);
    LNewNode.Name := LNode.Name;
  end
  else
    LNewNode.Value := LNode.Value;

  DoAfterExecute(FNode, aeNodeAndChildrenChanged);
end;

procedure TCommandDataModule.DoAfterExecute(const ANode: TEFTree; const AFlag: TAfterExecuteFlag);
begin
  if Assigned(FAfterExecute) then
    FAfterExecute(Self, ANode, AFlag);
end;

procedure TCommandDataModule.DoBeforeExecute(const ANode: TEFTree; const AFlag: TBeforeExecuteFlag);
begin
  if Assigned(FBeforeExecute) then
    FBeforeExecute(Self, ANode, AFlag);
end;

constructor TCommandDataModule.Create(AOwner: TComponent);
begin
  inherited;
  FSelection := TList<TEFNode>.Create;
end;

procedure TCommandDataModule.DeleteNodeActionExecute(Sender: TObject);
var
  LCanDelete: Boolean;
  I: Integer;

  procedure DeleteNode(const ANode: TEFTree);
  var
    LParent: TEFTree;
  begin
    DoBeforeExecute(ANode, beNodeDeleting);
    if ANode is TEFNode then
    begin
      LParent := TEFNode(ANode).Parent;
      TEFNode(ANode).Delete;
      if LParent is TEFNode then
        DoAfterExecute(TEFNode(LParent), aeNodeAndChildrenChanged)
      else
        DoAfterExecute(nil,  aeNodeAndChildrenChanged);
    end;
  end;

begin
  Assert(Assigned(FNode));

  LCanDelete := True;
  if (FSelection.Count > 1) then
    LCanDelete := MessageDlg(_('Multiple nodes are going to be deleted. Are you sure you want to continue?'), mtWarning, [mbYes, mbNo], 0) = mrYes
  else if Assigned(FNode) and (FNode.ChildCount > 0) then
    LCanDelete := MessageDlg(_('The selected node and all its children are going to be deleted. Are you sure you want to continue?'), mtWarning, [mbYes, mbNo], 0) = mrYes;

  if LCanDelete then
  begin
    if not FSelection.Contains(FNode as TEFNode) then
      DeleteNode(FNode);
    for I := 0 to FSelection.Count - 1 do
      DeleteNode(FSelection[I]);
  end;
end;

procedure TCommandDataModule.DeleteNodeActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Visible := Assigned(FNode) and (FNode is TEFNode) and TEFNode(FNode).CanDelete;
end;

destructor TCommandDataModule.Destroy;
begin
  FreeAndNil(FSelection);
  inherited;
end;

procedure TCommandDataModule.UpdateActions;
var
  I: Integer;
begin
  for I := 0 to ActionList.ActionCount - 1 do
  begin
    (ActionList.Actions[I] as TAction).Visible := True;
    (ActionList.Actions[I] as TAction).Enabled := True;
    ActionList.UpdateAction(ActionList.Actions[I]);
    (ActionList.Actions[I] as TAction).Enabled := (ActionList.Actions[I] as TAction).Visible;
  end;
end;

procedure TCommandDataModule.RebuildPopupMenu;
var
  I: Integer;
  LMenuItem: TMenuItem;
  LNode, LFoundNode: TEFNode;
  LConfigFile, LNodeName, LNodeValue: string;
  LSeparator: Boolean;
begin
  PopupActionBar.Items.Clear;
  //Add custom items to PopUp Menu
  LConfigFile := GetMetadataNodeFileName(FNode);
  LSeparator := False;
  if FileExists(LConfigFile) then
  begin
    FMetadataConfig := TEFPersistentTree.Create;
    TEFYAMLReader.LoadTree(FMetadataConfig, LConfigFile);
    LSeparator := False;
    for I := 0 to FMetadataConfig.ChildCount -1 do
    begin
      LNode := FMetadataConfig.Children[I];
      LNodeName := LNode.Name;
      LNodeValue := LNode.Value;
      LFoundNode := FNode.FindNode(LNodeName);
      if not Assigned(LFoundNode) or LNode.GetBoolean('Multiple') then
      begin
        LMenuItem := TMenuItem.Create(Self);
        LMenuItem.OnClick := AddNodeExecute;
        if LNodeValue = '' then
          LMenuItem.Caption := _(Format('Add %s', [LNodeName]))
        else
          LMenuItem.Caption := _(Format('Add %s: %s', [LNodeName, LNodeValue]));
        LMenuItem.ImageIndex := ADD_CHILD;
        LMenuItem.Tag := I;
        PopupActionBar.Items.Add(LMenuItem);
        LSeparator := True;
      end;
    end;
  end
  else
    LConfigFile := '';
  if LSeparator then
  begin
    LMenuItem := TMenuItem.Create(Self);
    LMenuItem.Caption := '-';
    PopupActionBar.Items.Add(LMenuItem);
  end;

  //Add standard actions
  LMenuItem := TMenuItem.Create(Self);
  LMenuItem.Action := AddChildAction;
  PopupActionBar.Items.Add(LMenuItem);

  LMenuItem := TMenuItem.Create(Self);
  LMenuItem.Action := DeleteNodeAction;
  PopupActionBar.Items.Add(LMenuItem);
end;

function TCommandDataModule.GetPopupMenu(const ANode: TEFTree;
  const ASelection: TList<TEFNode>): TPopupMenu;
begin
  Assert(Assigned(ASelection));

  FNode := ANode;
  FSelection.Clear;
  FSelection.AddRange(ASelection);
  UpdateActions;
  RebuildPopupMenu;
  Result := PopupActionBar;
end;

end.
