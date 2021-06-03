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
unit KIDE.TreeEditorBaseFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ImgList, Vcl.ExtCtrls,
  Vcl.ToolWin, Vcl.ComCtrls, Vcl.ActnList,
  EF.Tree, EF.YAML,
  KIDE.EditorFrameUnit, KIDE.BaseFrameUnit, KIDE.CodeEditorFrameUnit, KIDE.Editor,
  KIDE.TreeDesignerFrameUnit, KIDE.NodeDesignerFrameUnit, KIDE.EFTreeFrameUnit, System.Actions;

type
  TTreeEditorBaseFrame = class {abstract}(TEditorFrame)
    ClientPanel: TPanel;
    DesignPanel: TPanel;
    EditPanel: TPanel;
    BottomSplitter: TSplitter;
    DesignerSplitter: TSplitter;
    DesignerPanel: TPanel;
    CodeEditorFrame: TCodeEditorFrame;
    ActionList: TActionList;
    ToolBar: TToolBar;
    ShowCodeAction: TAction;
    ShowCodeButton: TToolButton;
    ShowPreviewAction: TAction;
    ShowPreviewButton: TToolButton;
    CollapseNodeAction: TAction;
    ToolButtonTreeSep1: TToolButton;
    CollapseNodeToolButton: TToolButton;
    ExpandNodeActionToolButton: TToolButton;
    ExpandNodeAction: TAction;
    SearchAction: TAction;
    SearchAgainAction: TAction;
    SearchToolButton: TToolButton;
    ToolButtonTreeSep2: TToolButton;
    SearchAgainToolButton: TToolButton;
    ValidateAction: TAction;
    ToolButtonTreeSep3: TToolButton;
    ValidateToolButton: TToolButton;
    SaveAction: TAction;
    ToolButtonTreeSep4: TToolButton;
    SaveToolButton: TToolButton;
    UndoToolButton: TToolButton;
    ReloadAction: TAction;
    procedure BottomSplitterMoved(Sender: TObject);
    procedure ShowCodeActionExecute(Sender: TObject);
    procedure DesignerSplitterMoved(Sender: TObject);
    procedure ShowPreviewActionUpdate(Sender: TObject);
    procedure ShowPreviewActionExecute(Sender: TObject);
    procedure CollapseNodeActionExecute(Sender: TObject);
    procedure ExpandNodeActionExecute(Sender: TObject);
    procedure SelectedNodeActionUpdate(Sender: TObject);
    procedure SearchActionUpdate(Sender: TObject);
    procedure SearchActionExecute(Sender: TObject);
    procedure SearchAgainActionUpdate(Sender: TObject);
    procedure SearchAgainActionExecute(Sender: TObject);
    procedure ValidateActionExecute(Sender: TObject);
    procedure ValidateActionUpdate(Sender: TObject);
    procedure ActionListUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure SaveActionExecute(Sender: TObject);
    procedure ReloadActionExecute(Sender: TObject);
    procedure SaveUndoActionUpdate(Sender: TObject);
  strict private
    FOriginalObject: TEFTree;
    FEditObject: TEFTree;
    FgsSearchText: string;
    FgsSearchTextHistory: string;
    FgbSearchCaseSensitive: Boolean;
    FgbSearchWholeWords: Boolean;
    FgbSearchFromFocusedNode: Boolean;
    FgbSearchOnlyKeys: Boolean;
  private
    FTreeFrame: TEFTreeFrame;
    FOriginalFileName: string;
    FNodeDesigner: TTreeDesignerFrame;
    FMRURootKeyName: string;
    procedure EditObjectToTree(const AFocusedNodePath: string);
    procedure UpdateCodeEditor(const AForce: Boolean);
    procedure SaveCurrentNodeDesigner;
    procedure CreateNodeDesigner(const ANode: TEFTree);
    procedure DestroyNodeDesigner;
    procedure ShowEditPanel(const AShow: Boolean);
    procedure CodeEditorApply(Sender: TObject);
    procedure CodeEditorCancel(Sender: TObject);
    procedure NodeDesignerApply(Sender: TObject);
    procedure TreeFrameChange(const ASender: TEFTreeFrame;
      const ANode: TEFTree);
    procedure TreeFrameEdited(const ASender: TEFTreeFrame;
      const ANode: TEFTree);
    procedure InitEditObject;
    procedure ShowSearchDialog;
    procedure DoSearchText;
  strict protected
    function EditorMatchesSpec(const ASpec: string): Boolean; override;
    function EditorSuits(const ASpec: string; const AParams: TEFNode): Boolean; override;
    function GetEditorProperty(const AName: string): Variant; override;
    procedure Save; override;
    procedure RefreshEditor; override;
    property OriginalObject: TEFTree read FOriginalObject;
    property OriginalFileName: string read FOriginalFileName;
    property EditObject: TEFTree read FEditObject;
    function IsChanged: Boolean; override;
    function GetTreeClass: TEFTreeClass; virtual; abstract;
    function GetPreviewCommand: string; virtual;
  public
    procedure AfterConstruction; override;
    procedure CloseEditor(const AForce: Boolean); override;
    destructor Destroy; override;
    procedure InitEditor(const AParams: TEFNode); override;
    procedure DisplayEmbedded(const AParent: TWinControl); override;
    function IsEditorActionEnabled(const AAction: TEditorAction): Boolean; override;
    function GetSpec: string; override;
  end;

implementation

{$R *.dfm}

uses
  KIDE.TreeSearchDialogUnit,
  KIDE.MRUOptions, KIDE.DesignMetadata, KIDE.MainDataModuleUnit, KIDE.Project;

{ TTreeEditorBaseFrame }

procedure TTreeEditorBaseFrame.ActionListUpdate(Action: TBasicAction;
  var Handled: Boolean);
begin
  inherited;
  ValidateAction.Visible := False;
end;

procedure TTreeEditorBaseFrame.AfterConstruction;
begin
  inherited;
  FTreeFrame := TEFTreeFrame.Create(Self);
  FTreeFrame.ShowAllNodes := True;
  FTreeFrame.OnChange := TreeFrameChange;
  FTreeFrame.OnEdited := TreeFrameEdited;
  FTreeFrame.Parent := DesignPanel;
  FTreeFrame.Left := 0;
  FTreeFrame.Align := alClient;

  CodeEditorFrame.OnApply := CodeEditorApply;
  CodeEditorFrame.OnCancel := CodeEditorCancel;
end;

procedure TTreeEditorBaseFrame.DestroyNodeDesigner;
begin
  if Assigned(FNodeDesigner) then
  begin
//    FNodeDesigner.Cancel;
    FreeAndNil(FNodeDesigner);
//    if Assigned(FTreeFrame) then
//      FTreeFrame.UpdateCurrentNode;
  end;
end;

procedure TTreeEditorBaseFrame.TreeFrameChange(const ASender: TEFTreeFrame; const ANode: TEFTree);
begin
  inherited;
  if Assigned(FNodeDesigner) and (ANode is TEFNode) then
    FNodeDesigner.Cancel;

  if not (Assigned(FNodeDesigner) and (FNodeDesigner.EditNode = ANode)) then
  begin
    CreateNodeDesigner(ANode);
    if Assigned(FNodeDesigner) and (FNodeDesigner.HelpKeyword <> '') then
      self.HelpKeyword := FNodeDesigner.HelpKeyword;
  end
  else if Assigned(FNodeDesigner) then
  begin
    if not (ANode is TEFNode) then
      FNodeDesigner.Cancel;
    FNodeDesigner.Init(ANode);
    FNodeDesigner.UpdateDesigner;
  end;
end;

procedure TTreeEditorBaseFrame.TreeFrameEdited(const ASender: TEFTreeFrame; const ANode: TEFTree);
begin
  inherited;
  if Assigned(FNodeDesigner) then
  begin
    if FNodeDesigner.EditNode = ANode then
      FNodeDesigner.UpdateDesigner;
  end;
  UpdateCodeEditor(False);
end;

procedure TTreeEditorBaseFrame.CloseEditor(const AForce: Boolean);
begin
  if Assigned(FNodeDesigner) then
    FNodeDesigner.Cancel;
  inherited;
end;

procedure TTreeEditorBaseFrame.CodeEditorApply(Sender: TObject);
var
  LPath: string;
  LFocusedNode: TEFNode;
begin
  LFocusedNode := FTreeFrame.GetFocusedNode;
  if Assigned(LFocusedNode) then
    LPath := LFocusedNode.GetPath
  else
    LPath := '';
  
  FEditObject.AsYamlString := CodeEditorFrame.Code;
  FreeAndNil(FNodeDesigner);
  EditObjectToTree(LPath);
  UpdateCodeEditor(True);
end;

procedure TTreeEditorBaseFrame.CodeEditorCancel(Sender: TObject);
begin
  UpdateCodeEditor(True);
end;

procedure TTreeEditorBaseFrame.CollapseNodeActionExecute(Sender: TObject);
begin
  inherited;
  FTreeFrame.CollapseSelectedNode(True);
end;

procedure TTreeEditorBaseFrame.DesignerSplitterMoved(Sender: TObject);
begin
  inherited;
  TMRUOptions.Instance.StoreInteger(FMRURootKeyName + '/DesignerPanelWidth', DesignerPanel.Width);
end;

destructor TTreeEditorBaseFrame.Destroy;
begin
  DestroyNodeDesigner;
  FreeAndNil(FTreeFrame);
  FreeAndNil(FEditObject);
  inherited;
end;

procedure TTreeEditorBaseFrame.DisplayEmbedded(const AParent: TWinControl);
begin
  inherited;
  FTreeFrame.Activate;
end;

procedure TTreeEditorBaseFrame.InitEditor(const AParams: TEFNode);
begin
  inherited;
  FOriginalObject := AParams.GetObject('Object') as TEFTree;
  Assert(Assigned(FOriginalObject));

  FOriginalFileName := AParams.GetString('ObjectFileName');
  Assert(DirectoryExists(ExtractFilePath(FOriginalFileName)));

  FMRURootKeyName := AParams.GetString('MRURootKeyName', 'TreeEditor');
  DesignerPanel.Width := TMRUOptions.Instance.GetInteger(FMRURootKeyName + '/DesignerPanelWidth', DesignerPanel.Width);
  EditPanel.Height := TMRUOptions.Instance.GetInteger(FMRURootKeyName + '/EditPanelHeight', EditPanel.Height);
  DesignerSplitter.Left := -1; // Keeps being moved to the far right otherwise.
  ShowEditPanel(TMRUOptions.Instance.GetBoolean(FMRURootKeyName + '/EditPanel/Visible', False));

  InitEditObject;
  UpdateCodeEditor(True);
end;

procedure TTreeEditorBaseFrame.InitEditObject;
begin
  FreeAndNil(FEditObject);
  FEditObject := TEFTreeClass(FOriginalObject.ClassType).Clone(FOriginalObject);
  FTreeFrame.EFTree := FEditObject;
end;

function TTreeEditorBaseFrame.IsChanged: Boolean;
begin
  Result := Assigned(FOriginalObject) and Assigned(FEditObject)
    and (FOriginalObject.AsYamlString <> FEditObject.AsYamlString);
end;

function TTreeEditorBaseFrame.IsEditorActionEnabled(
  const AAction: TEditorAction): Boolean;
begin
  if AAction in [eaSave, eaReload] then
    Result := IsChanged
  else
    Result := inherited IsEditorActionEnabled(AAction);
end;

procedure TTreeEditorBaseFrame.BottomSplitterMoved(Sender: TObject);
begin
  inherited;
  TMRUOptions.Instance.StoreInteger(FMRURootKeyName + '/EditPanelHeight', EditPanel.Height);
end;

procedure TTreeEditorBaseFrame.Save;
begin
  inherited;
  OriginalObject.Assign(EditObject);
end;

procedure TTreeEditorBaseFrame.SaveActionExecute(Sender: TObject);
begin
  inherited;
  ExecuteEditorAction(eaSave);
end;

procedure TTreeEditorBaseFrame.SaveCurrentNodeDesigner;
begin
  if Assigned(FNodeDesigner) then
    FNodeDesigner.Apply;
end;

procedure TTreeEditorBaseFrame.SaveUndoActionUpdate(Sender: TObject);
begin
  inherited;
  (Sender as TAction).Enabled := IsChanged;
end;

procedure TTreeEditorBaseFrame.ShowSearchDialog;
var
  LSearchDialog: TTreeSearchDialog;
  LNode, LFocusedNode: TEFNode;
begin
  LSearchDialog := TTreeSearchDialog.Create(Self);
  LNode := TMRUOptions.Instance.GetNode(FMRURootKeyName + '/Search', True);
  LFocusedNode := FTreeFrame.GetFocusedNode;

  with LSearchDialog do
  try
    //Restore position
    SetBounds(
      LNode.GetInteger('Left',100),
      LNode.GetInteger('Top',100),
      LSearchDialog.Width,
      LSearchDialog.Height);

    // assign search options
    SearchCaseSensitive := FgbSearchCaseSensitive;
    SearchFromFocusedNode := FgbSearchFromFocusedNode and Assigned(LFocusedNode);
    // start with last search text
    SearchText := FgsSearchText;
    SearchTextHistory := FgsSearchTextHistory;
    SearchWholeWords := FgbSearchWholeWords;
    SearchOnlyKeys := FgbSearchOnlyKeys;
    if ShowModal = mrOK then
    begin
      FgbSearchCaseSensitive := SearchCaseSensitive;
      FgbSearchFromFocusedNode := SearchFromFocusedNode;
      FgbSearchWholeWords := SearchWholeWords;
      FgbSearchOnlyKeys := SearchOnlyKeys;
      FgsSearchText := SearchText;
      FgsSearchTextHistory := SearchTextHistory;
      FgbSearchFromFocusedNode := SearchFromFocusedNode;
      if FgsSearchText <> '' then
        DoSearchText;
    end;
    LNode.SetInteger('Left', LSearchDialog.Left);
    LNode.SetInteger('Top', LSearchDialog.Top);
    TMRUOptions.Instance.Save;
  finally
    LSearchDialog.Free;
  end;
end;

procedure TTreeEditorBaseFrame.SearchActionExecute(Sender: TObject);
begin
  inherited;
  ShowSearchDialog;
end;

procedure TTreeEditorBaseFrame.SearchActionUpdate(Sender: TObject);
begin
  inherited;
  SearchAction.Enabled := (FTreeFrame <> nil) and (FTreeFrame.EFTree.ChildCount > 0);
end;

procedure TTreeEditorBaseFrame.SearchAgainActionExecute(Sender: TObject);
begin
  inherited;
  FgbSearchFromFocusedNode := True;
  DoSearchText;
end;

procedure TTreeEditorBaseFrame.SearchAgainActionUpdate(Sender: TObject);
begin
  inherited;
  SearchAgainAction.Enabled := FgsSearchText <> '';
end;

procedure TTreeEditorBaseFrame.SelectedNodeActionUpdate(Sender: TObject);
begin
  inherited;
  (Sender as TAction).Enabled := True;
end;

procedure TTreeEditorBaseFrame.ShowCodeActionExecute(Sender: TObject);
begin
  inherited;
  ShowEditPanel(not EditPanel.Visible);
end;

procedure TTreeEditorBaseFrame.ShowEditPanel(const AShow: Boolean);
begin
  ShowCodeButton.Down := AShow;
  EditPanel.Visible := AShow;
  BottomSplitter.Visible := AShow;
  TMRUOptions.Instance.StoreBoolean(FMRURootKeyName + '/EditPanel/Visible', AShow);
  UpdateCodeEditor(True);
end;

procedure TTreeEditorBaseFrame.ShowPreviewActionExecute(Sender: TObject);
begin
  inherited;
  TProject.CurrentProject.ShowPreviewAction(GetPreviewCommand);
end;

procedure TTreeEditorBaseFrame.ShowPreviewActionUpdate(Sender: TObject);
begin
  inherited;
  ShowPreviewAction.Enabled := TProject.CurrentProject.IsKittoEngineRunning and
    (GetPreviewCommand <> '');
end;

procedure TTreeEditorBaseFrame.CreateNodeDesigner(const ANode: TEFTree);
begin
  Assert(Assigned(ANode));
  DestroyNodeDesigner;
  if ANode is TEFNode then
    FNodeDesigner := TNodeDesignerFrameFactory.Instance.CreateDesignerFrame(TEFNode(ANode), Self)
  else
    FNodeDesigner := TTreeDesignerFrameFactory.Instance.CreateDesignerFrame(TEFTree(ANode), Self);

  FNodeDesigner.Parent := DesignerPanel;
  FNodeDesigner.Align := alClient;
  FNodeDesigner.OnApply := NodeDesignerApply;
end;

procedure TTreeEditorBaseFrame.NodeDesignerApply(Sender: TObject);
begin
  FTreeFrame.UpdateCurrentNode;
  UpdateCodeEditor(False);
end;

procedure TTreeEditorBaseFrame.RefreshEditor;
begin
  inherited;
  InitEditObject;
  UpdateCodeEditor(True);
end;

procedure TTreeEditorBaseFrame.ReloadActionExecute(Sender: TObject);
begin
  inherited;
  ExecuteEditorAction(eaReload);
end;

procedure TTreeEditorBaseFrame.EditObjectToTree(const AFocusedNodePath: string);
begin
  Assert(Assigned(FEditObject));

  FTreeFrame.EFTree := FEditObject;
//  FTreeFrame.FocusEFNode(FEditObject.FindNode(AFocusedNodePath));
end;

procedure TTreeEditorBaseFrame.UpdateCodeEditor(const AForce: Boolean);
begin
  if Assigned(FEditObject) and EditPanel.Visible then
  begin
    if AForce then
      CodeEditorFrame.RefreshCode(FEditObject.AsYamlString)
    else
      CodeEditorFrame.Code := FEditObject.AsYamlString;
  end;
end;

procedure TTreeEditorBaseFrame.ValidateActionExecute(Sender: TObject);
begin
  inherited;
  ;
end;

procedure TTreeEditorBaseFrame.ValidateActionUpdate(Sender: TObject);
begin
  inherited;
  ;
end;

function TTreeEditorBaseFrame.EditorMatchesSpec(const ASpec: string): Boolean;
begin
  Result := (FOriginalFileName <> '') and SameFileName(ASpec, FOriginalFileName);
end;

function TTreeEditorBaseFrame.EditorSuits(const ASpec: string;
  const AParams: TEFNode): Boolean;
begin
  Result := AParams.GetObject('Object').InheritsFrom(GetTreeClass);
end;

procedure TTreeEditorBaseFrame.ExpandNodeActionExecute(Sender: TObject);
begin
  inherited;
  FTreeFrame.ExpandSelectedNode(True);
end;

function TTreeEditorBaseFrame.GetEditorProperty(const AName: string): Variant;
begin
  if SameText(AName, 'LongTitle') then
    Result := FOriginalFileName
  else
    Result := inherited GetEditorProperty(AName);
end;

function TTreeEditorBaseFrame.GetPreviewCommand: string;
begin
  Result := '';
end;

function TTreeEditorBaseFrame.GetSpec: string;
begin
  Result := FOriginalFileName;
end;

procedure TTreeEditorBaseFrame.DoSearchText;
begin
  FTreeFrame.SearchNodeByText(FgsSearchText, FgbSearchOnlyKeys,
    FgbSearchFromFocusedNode, FgbSearchCaseSensitive, FgbSearchWholeWords);
end;

end.
