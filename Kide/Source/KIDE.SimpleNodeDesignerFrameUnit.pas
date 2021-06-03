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
unit KIDE.SimpleNodeDesignerFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.Tabs,
  EF.Tree,
  KIDE.EFTreeFrameUnit, System.Actions, Vcl.ActnList, Vcl.ComCtrls, Vcl.ToolWin,
  KIDE.EditNodeBaseFrameUnit, Vcl.StdCtrls, KIDE.NodeDesignerFrameUnit, KIDE.BaseFrameUnit,
  KIDE.CodeEditorFrameUnit, Vcl.Samples.Spin;

type
  TSimpleNodeDesignerFrame = class(TEditNodeBaseFrame)
    KeyPanel: TPanel;
    KeyEdit: TLabeledEdit;
    CheckBoxPanel: TPanel;
    CheckBoxValue: TCheckBox;
    IntegerPanel: TPanel;
    IntegerValue: TSpinEdit;
    IntegerValueLabel: TLabel;
    TextPanel: TPanel;
    ValueLabel: TLabel;
    ValueEdit: TMemo;
  private
  strict protected
  protected
    procedure UpdateDesignPanel(const AForce: Boolean); override;
    procedure DesignPanelToEditNode; override;
  public
    class function SuitsNode(const ANode: TEFNode): Boolean; override;
    procedure Init(const ANode: TEFTree); override;
  end;

implementation

{$R *.dfm}

{ TDefaultNodeDesignerFrame }

procedure TSimpleNodeDesignerFrame.DesignPanelToEditNode;
begin
  inherited;
  TEFNode(EditNode).Name := KeyEdit.Text;
  if TextPanel.Visible then
    TEFNode(EditNode).Value := ValueEdit.Text
  else if CheckBoxPanel.Visible then
    TEFNode(EditNode).AsBoolean := CheckBoxValue.Checked
  else if IntegerPanel.Visible then
    TEFNode(EditNode).AsInteger := IntegerValue.Value;
end;

procedure TSimpleNodeDesignerFrame.Init(const ANode: TEFTree);
var
  LNodeDataType: TEFDataType;
begin
  inherited;
  LNodeDataType := TEFNode(ANode).DataType;
  KeyEdit.Enabled := TEFNode(ANode).ChildCount = 0;
  CheckBoxPanel.Visible := LNodeDataType.InheritsFrom(TEFBooleanDataType);
  IntegerPanel.Visible := LNodeDataType.InheritsFrom(TEFIntegerDataType);
  TextPanel.Visible := not (CheckBoxPanel.Visible or IntegerPanel.Visible);
end;

class function TSimpleNodeDesignerFrame.SuitsNode(const ANode: TEFNode): Boolean;
begin
  Result := (ANode is TEFNode) and (TEFNode(ANode).ChildCount = 0);
end;

procedure TSimpleNodeDesignerFrame.UpdateDesignPanel(const AForce: Boolean);
begin
  inherited;
  KeyEdit.Text := TEFNode(EditNode).Name;
  if TextPanel.Visible then
    ValueEdit.Text := TEFNode(EditNode).AsString
  else if CheckBoxPanel.Visible then
    CheckBoxValue.Checked := TEFNode(EditNode).AsBoolean
  else if IntegerPanel.Visible then
    IntegerValue.Value := TEFNode(EditNode).AsInteger;
end;
(*
initialization
  TEditNodeFrameRegistry.Instance.RegisterClass(TSimpleNodeDesignerFrame.GetClassId, TSimpleNodeDesignerFrame);

finalization
  TEditNodeFrameRegistry.Instance.UnregisterClass(TSimpleNodeDesignerFrame.GetClassId);
*)
end.
