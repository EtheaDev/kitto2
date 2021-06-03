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
unit KIDE.EFTreeFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VirtualTrees,
  EF.Tree,
  KIDE.BaseFrameUnit, KIDE.CommandDataModuleUnit, Vcl.Menus;

type
  PEFVirtualTreeNode = ^TEFVirtualTreeNode;
  TEFVirtualTreeNode = record
    // Node *must* be the first field in this record.
    Node: TEFTree;
  end;

  TEFNodeStringEditLink = class(TStringEditLink)
  private
    FNode: TEFTree;
  public
    property Node: TEFTree read FNode write FNode;
    function EndEdit: Boolean; override; stdcall;
  end;

  TEFTreeFrame = class;

  TEFTreeFrameNodeEvent = procedure (const ASender: TEFTreeFrame; const ANode: TEFTree) of object;

  TEFTreeFrame = class(TBaseFrame)
  private
    FTree: TVirtualStringTree;
    FOnChange: TEFTreeFrameNodeEvent;
    FOnEdited: TEFTreeFrameNodeEvent;
    FEFTree: TEFTree;
    FShowAllNodes: Boolean;
    FCommandDataModule: TCommandDataModule;
    procedure StoreOptions;
    procedure RestoreOptions;
    procedure TreeChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure TreeClick(Sender: TObject);
    procedure TreeCreateEditor(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; out EditLink: IVTEditLink);
    procedure TreeEdited(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
    procedure TreeEditing(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var Allowed: Boolean);
    procedure TreeFocusChanging(Sender: TBaseVirtualTree; OldNode,
      NewNode: PVirtualNode; OldColumn, NewColumn: TColumnIndex;
      var Allowed: Boolean);
    procedure TreeGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean;
      var ImageIndex: TImageIndex);
    procedure TreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure TreeInitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode;
      var ChildCount: Cardinal);
    procedure TreeInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure DoChange(const ANode: TEFTree);
    procedure DoEdited(const ANode: TEFTree);
    procedure SetEFTree(const AValue: TEFTree);
    function FindNode(const AParentVTNode: PVirtualNode; const ANode: TEFTree): PVirtualNode;
    procedure TreeGetPopupMenu(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; const P: TPoint; var AskParent: Boolean;
      var PopupMenu: TPopupMenu);
    procedure BeforeExecuteCommand(const ASender: TCommandDataModule;
      const ANode: TEFTree; const AFlag: TBeforeExecuteFlag);
    procedure AfterExecuteCommand(const ASender: TCommandDataModule;
      const ANode: TEFTree; const AFlag: TAfterExecuteFlag);
    procedure ReinitTree;
    procedure SetShowAllNodes(const AValue: Boolean);
    procedure TreeGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle;
      var HintText: UnicodeString);
    procedure TreeColumnResize(Sender: TVTHeader; Column: TColumnIndex);
    function GetNodeText(const AColumn: TColumnIndex;
      const ANode: TEFNode): string;
    procedure ToggleNode(const ANode: PVirtualNode; const ACollapse, AOnlyChildrens: Boolean);
    function FindNodeByText(const ANode: PVirtualNode; const AText: string;
      const ASearchNext: Boolean; const AKeyOnly: Boolean;
      const ACaseSensitive: Boolean; const AWholeWords: Boolean): PVirtualNode;
  public
    procedure AfterConstruction; override;

    procedure Activate;

    property EFTree: TEFTree read FEFTree write SetEFTree;
    property OnChange: TEFTreeFrameNodeEvent read FOnChange write FOnChange;
    property OnEdited: TEFTreeFrameNodeEvent read FOnEdited write FOnEdited;

    ///	<summary>True to display all nodes, regardless of
    ///	IsDesignLeaf.</summary>
    property ShowAllNodes: Boolean read FShowAllNodes write SetShowAllNodes;

    procedure UpdateCurrentNode;

    ///	<summary>Returns current selected and focused node.</summary>
    function GetFocusedNode: TEFNode;

    ///	<summary>Set current selected and focused node.</summary>
    function FocusEFNode(const ANode: TEFTree): Boolean;

    ///	<summary>Collaps selected node and optionally the childrens.</summary>
    procedure CollapseSelectedNode(const ACollapseAll: Boolean);

    ///	<summary>Expand selected node and optionally the childrens.</summary>
    procedure ExpandSelectedNode(const AExpandAll: Boolean);

    ///	<summary>Search a node by Key and value Text (or key only).</summary>
    function SearchNodeByText(const AText: string;
      const AKeyOnly: Boolean; const AFromFocusedNode: Boolean;
      const ACaseSensitive: Boolean; const AWholeWords: Boolean): PVirtualNode;

  end;

implementation

{$R *.dfm}

uses
  Generics.Collections,
  EF.YAML, EF.Macros,
  KIDE.MRUOptions, KIDE.MainDataModuleUnit, KIDE.DesignMetadata;

procedure TEFTreeFrame.AfterConstruction;
begin
  inherited;
  FCommandDataModule := TCommandDataModule.Create(Self);
  FCommandDataModule.Name := '';
  FCommandDataModule.AfterExecute := AfterExecuteCommand;
  FCommandDataModule.BeforeExecute := BeforeExecuteCommand;
  FCommandDataModule.CommandsOnAllNodes := FShowAllNodes;

  FTree := TVirtualStringTree.Create(Self);
  FTree.Parent := Self;
  FTree.Align := alClient;
  FTree.EditDelay := 300;
  FTree.ShowHint := True;
  FTree.HintMode := hmHint;
  FTree.Images := MainDataModule.Images;
  FTree.TreeOptions.AnimationOptions := [toAnimatedToggle, toAdvancedAnimatedToggle];
  FTree.TreeOptions.MiscOptions := [toEditable, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning, toEditOnClick, toEditOnDblClick];
  FTree.TreeOptions.SelectionOptions := [toExtendedFocus, toLevelSelectConstraint, toMultiSelect]; //, toRightClickSelect];
  FTree.TreeOptions.PaintOptions := FTree.TreeOptions.PaintOptions - [toThemeAware]; // Otherwise hint window is not cleared between invocations.
  FTree.OnChange := TreeChange;
  FTree.OnClick := TreeClick;
  FTree.OnCreateEditor := TreeCreateEditor;
  FTree.OnEdited := TreeEdited;
  FTree.OnEditing := TreeEditing;
  FTree.OnFocusChanging := TreeFocusChanging;
  FTree.OnGetText := TreeGetText;
  FTree.OnGetImageIndex := TreeGetImageIndex;
  FTree.OnInitChildren := TreeInitChildren;
  FTree.OnInitNode := TreeInitNode;
  FTree.OnGetPopupMenu := TreeGetPopupMenu;
  FTree.OnGetHint := TreeGetHint;
  FTree.OnColumnResize := TreeColumnResize;

  FTree.Header.AutoSizeIndex := 1;
  FTree.Header.Options := [hoAutoResize, hoColumnResize, hoDrag, hoVisible];
  FTree.Header.Style := hsFlatButtons;

  FTree.Header.Columns.Clear;
  with FTree.Header.Columns.Add do
  begin
    Options := [coAllowClick, coEnabled, coParentBidiMode, coParentColor, coResizable, coVisible, coSmartResize, coAllowFocus];
    Width := 180;
    Text := 'Property';
  end;

  with FTree.Header.Columns.Add do
  begin
    Options := [coAllowClick, coEnabled, coParentBidiMode, coParentColor, coVisible, coAllowFocus];
    Text := 'Value';
  end;
end;

procedure TEFTreeFrame.AfterExecuteCommand(const ASender: TCommandDataModule;
  const ANode: TEFTree; const AFlag: TAfterExecuteFlag);
var
  LVTNode: PVirtualNode;
begin
  if ANode is TEFNode then
  begin
    LVTNode := FindNode(FTree.RootNode, ANode);
    if AFlag <> aeNodeDeleted then
    begin
      FTree.ClearSelection;
      FTree.Selected[LVTNode] := True;
    end;
    case AFlag of
      aeNodeChanged: FTree.ReinitNode(LVTNode, False);
      aeNodeAndChildrenChanged: FTree.ReinitNode(LVTNode, True);
    end;
  end
  else
    ReinitTree;
  DoEdited(ANode);
end;

procedure TEFTreeFrame.BeforeExecuteCommand(const ASender: TCommandDataModule;
  const ANode: TEFTree; const AFlag: TBeforeExecuteFlag);
var
  LVTNode: PVirtualNode;
begin
  if ANode is TEFNode then
  begin
    if AFlag = beNodeDeleting then
    begin
      //Change position to Parent Node before delete child node
      LVTNode := FindNode(FTree.RootNode, TEFNode(ANode).Parent);
      FTree.ClearSelection;
      FTree.Selected[LVTNode] := True;
    end;
  end;
end;

procedure TEFTreeFrame.ToggleNode(const ANode: PVirtualNode; const ACollapse, AOnlyChildrens: Boolean);
var
  LChildNode: PVirtualNode;
begin
  if not AOnlyChildrens then
  begin
    if ACollapse then
    begin
      if vsExpanded in ANode.States then
        FTree.ToggleNode(ANode);
    end
    else
    begin
      if not (vsExpanded in ANode.States) then
        FTree.ToggleNode(ANode);
    end;
  end;
  if Assigned(ANode) then
    LChildNode := ANode.FirstChild
  else if FTree.RootNode.ChildCount > 0 then
    LChildNode := FTree.RootNode.FirstChild
  else
    LChildNode := nil;
  while Assigned(LChildNode) do
  begin
    ToggleNode(LChildNode, ACollapse, False);
    LChildNode := LChildNode.NextSibling;
  end;
end;

procedure TEFTreeFrame.CollapseSelectedNode(const ACollapseAll: Boolean);
var
  LNode: PVirtualNode;
begin
  FTree.BeginUpdate;
  try
    if (FTree.SelectedCount > 0) then
      LNode := FTree.FocusedNode
    else
      LNode := nil;
    ToggleNode(LNode, True, ACollapseAll);
  finally
    FTree.EndUpdate;
  end;
end;

procedure TEFTreeFrame.ExpandSelectedNode(const AExpandAll: Boolean);
var
  LNode: PVirtualNode;
begin
  FTree.BeginUpdate;
  try
    if (FTree.SelectedCount > 0) then
      LNode := FTree.FocusedNode
    else
      LNode := nil;
    ToggleNode(LNode, False, AExpandAll);
  finally
    FTree.EndUpdate;
  end;
end;

procedure TEFTreeFrame.DoChange(const ANode: TEFTree);
begin
  if Assigned(FOnChange) then
    FOnChange(Self, ANode);
end;

procedure TEFTreeFrame.DoEdited(const ANode: TEFTree);
begin
  if Assigned(FOnEdited) then
    FOnEdited(Self, ANode);
end;

procedure TEFTreeFrame.SetEFTree(const AValue: TEFTree);
begin
  FEFTree := AValue;

  ReinitTree;
end;

procedure TEFTreeFrame.SetShowAllNodes(const AValue: Boolean);
begin
  FShowAllNodes := AValue;
  if Assigned(FCommandDataModule) then
    FCommandDataModule.CommandsOnAllNodes := FShowAllNodes;
end;

procedure TEFTreeFrame.StoreOptions;
var
  LNode: TEFNode;
begin
  if Assigned(FTree) and Assigned(FTree.Header) and Assigned(FTree.Header.Columns[0]) then
  begin
    LNode := TMRUOptions.Instance.GetNode('TreeFrame/FirstColSize', True);
    LNode.AsInteger := FTree.Header.Columns[0].Width;
    TMRUOptions.Instance.Save;
  end;
end;

procedure TEFTreeFrame.ReinitTree;
begin
  FTree.Clear;
  FTree.NodeDataSize := SizeOf(TEFVirtualTreeNode);
  if Assigned(FEFTree) then
  begin
    if EFTree is TEFNode then
      FTree.RootNodeCount := 1
    else
      FTree.RootNodeCount := FEFTree.ChildCount;

    //TreeChange(FTree, nil);
    DoChange(EFTree);
    RestoreOptions;
  end;
end;

procedure TEFTreeFrame.RestoreOptions;
var
  LNode: TEFNode;
begin
  if Assigned(FTree) and Assigned(FTree.Header) and Assigned(FTree.Header.Columns[0]) then
  begin
    LNode := TMRUOptions.Instance.GetNode('TreeFrame/FirstColSize', True);
    if VarToStr(LNode.Value) <> '' then
      FTree.Header.Columns[0].Width := LNode.AsInteger;
  end;
end;

function TEFTreeFrame.GetFocusedNode: TEFNode;
var
  LNode: PVirtualNode;
  LData: PEFVirtualTreeNode;
begin
  if (FTree.SelectedCount > 0) then
    LNode := FTree.FocusedNode
  else
    LNode := nil;
  if Assigned(LNode) then
  begin
    LData := FTree.GetNodeData(LNode);
    if LData.Node is TEFNode then
      Result := TEFNode(LData.Node)
    else
      Result := nil;
  end
  else
    Result := nil;
end;

function TEFTreeFrame.FocusEFNode(const ANode: TEFTree): Boolean;
var
  LNode, LFoundNode: PVirtualNode;
  LData: PEFVirtualTreeNode;
begin
  LFoundNode := nil;

  if Assigned(ANode) then
  begin
    LNode := FTree.GetFirst;
    while Assigned(LNode) do
    begin
      LData := FTree.GetNodeData(LNode);
      if LData.Node = ANode then
        LFoundNode := LNode
      else
        LFoundNode := FindNode(LNode, ANode);
      if Assigned(LFoundNode) then
        Break;
      LNode := FTree.GetNext(LNode);
    end;
  end;

  Result := Assigned(LFoundNode);

  if not Result then
    LFoundNode := FTree.RootNode;
  FTree.FocusedNode := LFoundNode;
  FTree.Selected[LFoundNode] := True;
end;

function TEFTreeFrame.FindNode(const AParentVTNode: PVirtualNode; const ANode: TEFTree): PVirtualNode;
var
  LNode: PVirtualNode;
  LData: PEFVirtualTreeNode;
begin
  Result := nil;
  LNode := FTree.GetFirstChild(AParentVTNode);
  while Assigned(LNode) do
  begin
    LData := FTree.GetNodeData(LNode);
    if LData.Node = ANode then
      Result := LNode
    else
      Result := FindNode(LNode, ANode);
    if Assigned(Result) then
      Break;
    LNode := FTree.GetNext(LNode);
  end;
end;

procedure TEFTreeFrame.Activate;
begin
  FTree.SetFocus;
end;

procedure TEFTreeFrame.TreeChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  LData: PEFVirtualTreeNode;
begin
  inherited;
  if Assigned(Node) and Assigned(Sender.GetNodeData(Node)) then
  begin
    if (vsSelected in Node.States) then
    begin
      LData := Sender.GetNodeData(Node);
      Assert(LData.Node <> nil);
      DoChange(LData.Node);
    end;
  end;
end;

procedure TEFTreeFrame.TreeClick(Sender: TObject);
var
  LTree: TBaseVirtualTree;
begin
  LTree := Sender as TBaseVirtualTree;
  if LTree.SelectedCount = 0 then
  begin
    DoChange(EFTree);
  end;
end;

procedure TEFTreeFrame.TreeColumnResize(Sender: TVTHeader; Column: TColumnIndex);
begin
  if Column = 0 then
    StoreOptions;
end;

procedure TEFTreeFrame.TreeCreateEditor(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
var
  LData: PEFVirtualTreeNode;
  LLink: TEFNodeStringEditLink;
begin
  inherited;
  LData := Sender.GetNodeData(Node);

  LLink := TEFNodeStringEditLink.Create;
  LLink.Node := LData.Node;
  EditLink := LLink;
end;

procedure TEFTreeFrame.TreeEdited(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex);
var
  LData: PEFVirtualTreeNode;
begin
  inherited;
  LData := Sender.GetNodeData(Node);
  DoEdited(LData.Node);
  FTree.ReinitNode(Node, True);
end;

procedure TEFTreeFrame.TreeEditing(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; var Allowed: Boolean);
var
  LData: PEFVirtualTreeNode;
begin
  inherited;
  LData := Sender.GetNodeData(Node);

  if LData.Node is TEFNode then
  begin
    if Column = 0 then
      Allowed := TEFNode(LData.Node).CanEditName
    else
      Allowed := TEFNode(LData.Node).CanEditValue;
  end
  else
    Allowed := False;
end;

procedure TEFTreeFrame.TreeFocusChanging(Sender: TBaseVirtualTree; OldNode,
  NewNode: PVirtualNode; OldColumn, NewColumn: TColumnIndex;
  var Allowed: Boolean);
var
  LData: PEFVirtualTreeNode;
begin
  inherited;
  Allowed := True;
  if NewColumn = 1 then
  begin
    LData := Sender.GetNodeData(NewNode);
    if LData.Node is TEFNode then
      Allowed := TEFNode(LData.Node).CanEditValue
    else
      Allowed := False;
  end;
end;

procedure TEFTreeFrame.TreeGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: TImageIndex);
var
  LData: PEFVirtualTreeNode;
begin
  inherited;
  ImageIndex := -1;
  if Kind <> ikState then
  begin
    if Assigned(Node) and Assigned(Sender.GetNodeData(Node)) then
    begin
      LData := Sender.GetNodeData(Node);
      Assert(LData.Node <> nil);
      if (Column = 0) and (Kind <> ikOverlay) and (LData.Node is TEFNode) then
        ImageIndex := TEFNode(LData.Node).GetDesignImageIndex;
    end;
  end;
end;

function TEFTreeFrame.GetNodeText(const AColumn: TColumnIndex; const ANode: TEFNode): string;
begin
  if AColumn = 0 then
    Result := ANode.Name
  else
    Result := ANode.AsString;
end;

procedure TEFTreeFrame.TreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  LData: PEFVirtualTreeNode;
begin
  inherited;
  if Assigned(Node) and Assigned(Sender.GetNodeData(Node)) then
  begin
    LData := Sender.GetNodeData(Node);
    if (LData.Node <> nil) and (LData.Node is TEFNode) then
      CellText := GetNodeText(Column, TEFNode(LData.Node))
    else
      CellText := '<Error>';
  end;
end;

procedure TEFTreeFrame.TreeInitChildren(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var ChildCount: Cardinal);
var
  LData: PEFVirtualTreeNode;
begin
  inherited;
  LData := Sender.GetNodeData(Node);

  ChildCount := LData.Node.ChildCount;
end;

procedure TEFTreeFrame.TreeInitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  LData: PEFVirtualTreeNode;
  LParentData: PEFVirtualTreeNode;
  LIsDesignLeaf: Boolean;
begin
  inherited;
  LData := Sender.GetNodeData(Node);
  if ParentNode = nil then
  begin
    if FEFTree is TEFNode then
      LData.Node := TEFNode(FEFTree)
    else
      LData.Node := FEFTree.Children[Node.Index];
  end
  else
  begin
    LParentData := Sender.GetNodeData(ParentNode);
    LData.Node := LParentData.Node.Children[Node.Index];
  end;

  if LData.Node is TEFNode then
    LIsDesignLeaf := TEFNode(LData.Node).IsDesignLeaf
  else
    LIsDesignLeaf := LData.Node.IsTreeDesignLeaf;

  if FShowAllNodes or not LIsDesignLeaf then
    InitialStates := InitialStates + [ivsHasChildren, ivsExpanded];
end;

procedure TEFTreeFrame.TreeGetPopupMenu(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; const P: TPoint; var AskParent: Boolean; var PopupMenu: TPopupMenu);
var
  LSelNode: PVirtualNode;
  LData, LSelNodeData: PEFVirtualTreeNode;
  LSelection: TList<TEFNode>;
begin
  inherited;
  if Assigned(FCommandDataModule) then
  begin
    LData := Sender.GetNodeData(Node);

    LSelection := TList<TEFNode>.Create;
    try
      //PopUp only on first column, becaus selected node referes to first column
      if Column = 0 then
      begin
        for LSelNode in Sender.SelectedNodes do
        begin
          LSelNodeData := Sender.GetNodeData(LSelNode);
          if LSelNodeData.Node is TEFNode then
            LSelection.Add(TEFNode(LSelNodeData.Node));
        end;
        if Assigned(LData) and (LData.Node is TEFNode) then
          PopupMenu := FCommandDataModule.GetPopupMenu(TEFNode(LData.Node), LSelection)
        else
          PopupMenu := FCommandDataModule.GetPopupMenu(nil, LSelection);
      end
      else
        PopupMenu := FCommandDataModule.GetPopupMenu(EFTree, LSelection);

    finally
      FreeAndNil(LSelection);
    end;
  end;
end;

procedure TEFTreeFrame.TreeGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle;
  var HintText: UnicodeString);
var
  LData: PEFVirtualTreeNode;
  LText: string;
  LExpandedText: string;
begin
  inherited;
  HintText := '';
  if Assigned(Node) and Assigned(Sender.GetNodeData(Node)) then
  begin
    LData := Sender.GetNodeData(Node);
    Assert(LData.Node <> nil);
    if LData.Node is TEFNode then
    begin
      LText := GetNodeText(Column, TEFNode(LData.Node));
      LExpandedText := LText;
      TEFMacroExpansionEngine.Instance.Expand(LExpandedText);
      if LText <> LExpandedText then
        HintText := LExpandedText;
    end;
  end;
end;

procedure TEFTreeFrame.UpdateCurrentNode;
var
  LNode: PVirtualNode;
  LData: PEFVirtualTreeNode;
begin
  LNode := FTree.FocusedNode;
  if Assigned(LNode) then
  begin
    LData := FTree.GetNodeData(LNode);
    if LData.Node is TEFNode then
    begin
      FTree.ReinitNode(FTree.FocusedNode, True);
      FTree.RepaintNode(FTree.FocusedNode);
    end
    else
      ReinitTree;
  end
  else
    ReinitTree;
end;

function TEFTreeFrame.FindNodeByText(const ANode: PVirtualNode; const AText: string;
  const ASearchNext: Boolean; const AKeyOnly: Boolean;
  const ACaseSensitive: Boolean; const AWholeWords: Boolean): PVirtualNode;
var
  LChildNode: PVirtualNode;
  LData: PEFVirtualTreeNode;
  LNode: TEFNode;

  function CheckNode(const ACurrentNode: PVirtualNode): Boolean;
  var
    LLen: Integer;

    function SameNodeText(const ANodeName, ANodeValue: string): boolean;
    var
      LNodeValue: string;
    begin
      if AKeyOnly then
        LNodeValue := ''
      else
        LNodeValue := ANodeValue;
      if ACaseSensitive then
        Result := (ANodeName = AText) or (LNodeValue = AText)
      else
        Result := SameText(ANodeName, AText) or SameText(LNodeValue, AText);
    end;

  begin
    Result := False;
    LData := FTree.GetNodeData(ACurrentNode);
    if Assigned(LData) and Assigned(LData.Node) and (LData.Node is TEFNode) then
    begin
      LNode := TEFNode(LData.Node);
      if AWholeWords then
        Result := SameNodeText(LNode.Name, LNode.Value)
      else
      begin
        LLen := Length(AText);
        Result := SameNodeText(Copy(LNode.Name, 1, LLen), Copy(LNode.Value, 1, LLen));
      end;
    end;
  end;

begin
  Result := nil;
  if not ASearchNext and CheckNode(ANode) then
  begin
    Result := ANode;
    Exit;
  end;
  LChildNode := ANode.FirstChild;
  while Assigned(LChildNode) do
  begin
    Result := FindNodeByText(LChildNode, AText, False, AKeyOnly,
      ACaseSensitive, AWholeWords);
    if Assigned(Result) then
      break;
    LChildNode := LChildNode.NextSibling;
  end;
end;

function TEFTreeFrame.SearchNodeByText(const AText: string;
  const AKeyOnly: Boolean; const AFromFocusedNode: Boolean;
  const ACaseSensitive: Boolean; const AWholeWords: Boolean): PVirtualNode;
var
  LFocusedNode: PVirtualNode;
begin
  if AFromFocusedNode and (FTree.SelectedCount > 0) then
    LFocusedNode := FTree.FocusedNode
  else
    LFocusedNode := nil;
  if Assigned(LFocusedNode) then
    Result := FindNodeByText(LFocusedNode, AText, True, AKeyOnly, ACaseSensitive, AWholeWords)
  else if Assigned(FTree.RootNode) then
    Result := FindNodeByText(FTree.RootNode, AText, False, AKeyOnly, ACaseSensitive, AWholeWords)
  else
    Result := nil;
  if Assigned(Result) then
  begin
    if Assigned(FTree.FocusedNode) then
      FTree.RemoveFromSelection(FTree.FocusedNode);
    FTree.FocusedNode := Result;
    FTree.AddToSelection(Result);
  end
  else
    MessageBeep(MB_ICONASTERISK);
end;

{ TEFNodeStringEditLink }

function TEFNodeStringEditLink.EndEdit: Boolean;
begin
  Result := inherited EndEdit;

  Assert(Assigned(FNode));

  if Result and (FNode is TEFNode) then
  begin
    if FColumn = 0 then
      TEFNode(FNode).Name := Edit.Text
    else
    begin
      TEFNode(FNode).LockDataType;
      try
        TEFNode(FNode).AsString := Edit.Text;
      finally
        TEFNode(FNode).UnlockDataType;
      end;
    end;
  end;
end;

end.
