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
unit KIDE.PairsValuesFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, KIDE.BaseFrameUnit, System.Actions,
  Vcl.ActnList, Vcl.ComCtrls, Vcl.ToolWin,
  EF.Tree, Vcl.Grids, Vcl.ValEdit, KIDE.EditNodeBaseFrameUnit, Vcl.ExtCtrls;

type
  TPairsValuesFrame = class(TEditNodeBaseFrame)
    ToolBar: TToolBar;
    AddRowActionToolButton: TToolButton;
    DeleteRowActionToolButton: TToolButton;
    AddRowAction: TAction;
    DeleteRowAction: TAction;
    MoveDownAction: TAction;
    MoveUpAction: TAction;
    MoveDownActionToolButton: TToolButton;
    MoveUpActionToolButton: TToolButton;
    PairsValueListEditor: TValueListEditor;
    procedure AddRowActionExecute(Sender: TObject);
    procedure DeleteRowActionExecute(Sender: TObject);
    procedure MoveDownActionExecute(Sender: TObject);
    procedure MoveUpActionExecute(Sender: TObject);
    procedure MoveDownActionUpdate(Sender: TObject);
    procedure MoveUpActionUpdate(Sender: TObject);
    procedure FrameResize(Sender: TObject);
    procedure PairsValueListEditorSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure PairsValueListEditorStringsChange(Sender: TObject);
  strict private
    FPairsNodeName: string;
    FParentNode: TEFTree;
    FFixedKey: string;
    procedure MoveItem(AStep: Integer);
    function GetPairsNode: TEFNode;
  private
    procedure SetFixedKey(const Value: string);
  protected
    class function SuitsNode(const ANode: TEFNode): Boolean; override;
    procedure UpdateDesignPanel(const AForce: Boolean = False); override;
    procedure DesignPanelToEditNode; override;
  public
    procedure Init(const ANode: TEFTree); override;
    property PairsNode: TEFNode read GetPairsNode;
    property FixedKey: string read FFixedKey write SetFixedKey;
  end;


implementation

{$R *.dfm}

uses
  EF.Types,
  Kitto.Metadata.Models, Kitto.Metadata.DataView,
  KIDE.NodeDesignerFrameUnit;

{ TPairsValuesFrame }

procedure TPairsValuesFrame.AddRowActionExecute(Sender: TObject);
var
  LStep: Integer;
begin
  inherited;
  PairsValueListEditor.InsertRow(FFixedKey,'',True);
  //shifts rows down to create space in current position
  LStep := PairsValueListEditor.RowCount - PairsValueListEditor.Row -2;
  PairsValueListEditor.Row := PairsValueListEditor.RowCount-1;
  MoveItem(-LStep);
end;

procedure TPairsValuesFrame.DeleteRowActionExecute(Sender: TObject);
begin
  PairsValueListEditor.DeleteRow(PairsValueListEditor.Row);
end;

procedure TPairsValuesFrame.FrameResize(Sender: TObject);
var
  LVScrollbarSize: Integer;
begin
  inherited;
  LVScrollbarSize := 30;
  PairsValueListEditor.ColWidths[0] := ClientWidth div 2;
  PairsValueListEditor.ColWidths[1] := ClientWidth div 2 - LVScrollbarSize;
end;

procedure TPairsValuesFrame.Init(const ANode: TEFTree);
begin
  //Save Parent Node and Node Name to alway access PairsNode into GetPairsNode
  //using FindNode, because sometimes an empty PairsNode was deleted
  FPairsNodeName := (ANode as TEFNode).Name;
  FParentNode := (ANode as TEFNode).Parent;
  inherited;
end;

function TPairsValuesFrame.GetPairsNode: TEFNode;
begin
  Result := FParentNode.FindNode(FPairsNodeName, True);
end;

procedure TPairsValuesFrame.DesignPanelToEditNode;
var
  I, J: Integer;
  LNodeName, LNodeValue: string;
  lNode: TEFNode;
  LPairsNode: TEFNode;
begin
  LPairsNode := PairsNode;
  for I := 1 to PairsValueListEditor.RowCount -1 do
  begin
    LNodeName := PairsValueListEditor.Cells[0,I];
    LNodeValue := PairsValueListEditor.Cells[1,I];
    if LNodeName <> '' then
    begin
      //In FixedKey mode use only node position
      if FFixedKey <> '' then
      begin
        if LPairsNode.ChildCount >= I then
          lNode := LPairsNode.Children[I-1]
        else
          LNode := LPairsNode.AddChild(TEFNode.Create(LNodeName, LNodeValue));
      end
      else
        LNode := LPairsNode.FindNode(LNodeName, True);
      LNode.AsString := PairsValueListEditor.Cells[1,I];
    end;
  end;
  //Remove deleted nodes
  for I := LPairsNode.ChildCount -1 downto 0 do
  begin
    //In FixedKey mode use only node position
    if FFixedKey <> '' then
    begin
      if LPairsNode.ChildCount > PairsValueListEditor.RowCount -1 then
        LPairsNode.RemoveChild(LPairsNode.Children[I]);
    end
    else
    begin
      if not PairsValueListEditor.FindRow(LPairsNode.Children[I].Name, J) then
        LPairsNode.RemoveChild(LPairsNode.Children[I]);
    end;
  end;
end;

procedure TPairsValuesFrame.MoveDownActionExecute(Sender: TObject);
begin
  inherited;
  MoveItem(1);
end;

procedure TPairsValuesFrame.MoveDownActionUpdate(Sender: TObject);
begin
  inherited;
  MoveDownAction.Enabled := PairsValueListEditor.Row <> PairsValueListEditor.RowCount-1;
end;

procedure TPairsValuesFrame.MoveItem(AStep: Integer);
var
  LId, LValue: string;
  I, LOffSet: Integer;
begin
  LId := PairsValueListEditor.Cells[0, PairsValueListEditor.Row];
  LValue := PairsValueListEditor.Cells[1, PairsValueListEditor.Row];
  if AStep > 0 then
    LOffSet := 1
  else
    LOffset := -1;
  for I := 1 to Abs(AStep) do
  begin
    PairsValueListEditor.Cells[0, PairsValueListEditor.Row] := PairsValueListEditor.Cells[0, PairsValueListEditor.Row+LOffSet];
    PairsValueListEditor.Cells[1, PairsValueListEditor.Row] := PairsValueListEditor.Cells[1, PairsValueListEditor.Row+LOffSet];
    PairsValueListEditor.Cells[0, PairsValueListEditor.Row+LOffSet] := LId;
    PairsValueListEditor.Cells[1, PairsValueListEditor.Row+LOffSet] := LValue;
    PairsValueListEditor.Row := PairsValueListEditor.Row+LOffSet;
  end;
end;

procedure TPairsValuesFrame.MoveUpActionExecute(Sender: TObject);
begin
  inherited;
  MoveItem(-1);
end;

procedure TPairsValuesFrame.MoveUpActionUpdate(Sender: TObject);
begin
  inherited;
  MoveUpAction.Enabled := PairsValueListEditor.Row <> 0;
end;

procedure TPairsValuesFrame.PairsValueListEditorSelectCell(Sender: TObject; ACol, ARow: Integer;
  var CanSelect: Boolean);
begin
  inherited;
  if (FFixedKey <> '') and (PairsValueListEditor.Cells[0, ARow]='') then
    PairsValueListEditor.Cells[0, ARow] := FFixedKey;
end;

procedure TPairsValuesFrame.PairsValueListEditorStringsChange(Sender: TObject);
begin
  inherited;
  IsChanged := True;
end;

procedure TPairsValuesFrame.SetFixedKey(const Value: string);
begin
  FFixedKey := Value;
  if FFixedKey <> '' then
    PairsValueListEditor.FixedCols := 1
  else
    PairsValueListEditor.FixedCols := 0;
end;

class function TPairsValuesFrame.SuitsNode(const ANode: TEFNode): Boolean;
var
  LParentNode: TEFTree;
  LIsParentNodeField: Boolean;
begin
  Result := False;
  if Assigned(ANode.Parent) and (ANode.Parent is TEFTree) then
  begin
    LParentNode := ANode.Parent;
    LIsParentNodeField := (LParentNode is TKModelField) or (LParentNode is TKViewField);
    Result :=
      (LIsParentNodeField and (SameText(ANode.Name, 'AllowedValues') or SameText(ANode.Name, 'AutoAddFields'))) or
      ((LParentNode is TKModel) and SameText(ANode.Name, 'DetailReferences')) or
      SameText(ANode.Name, 'Colors');
  end;
end;

procedure TPairsValuesFrame.UpdateDesignPanel(const AForce: Boolean);
var
  LPairs: TEFPairs;
  LPair: TEFPair;
  I: Integer;
begin
  inherited;
  if EditNode is TEFNode then
  begin
    LPairs := FParentNode.GetChildrenAsPairs(TEFNode(EditNode).Name);
    PairsValueListEditor.Strings.Clear;
    for I := Low(LPairs) to High(LPairs) do
    begin
      LPair := LPairs[I];
      PairsValueListEditor.InsertRow(LPair.Key, LPair.Value, True);
    end;
  end;
end;

initialization
  TEditNodeFrameRegistry.Instance.RegisterClass(TPairsValuesFrame.GetClassId, TPairsValuesFrame);

finalization
  if Assigned(TEditNodeFrameRegistry.Instance) then
    TEditNodeFrameRegistry.Instance.UnregisterClass(TPairsValuesFrame.GetClassId);

end.
