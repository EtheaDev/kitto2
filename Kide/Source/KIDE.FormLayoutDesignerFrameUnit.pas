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
unit KIDE.FormLayoutDesignerFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, KIDE.TreeDesignerFrameUnit, Vcl.Samples.Spin,
  KIDE.BaseFrameUnit, KIDE.CodeEditorFrameUnit, Vcl.ExtCtrls, Vcl.Tabs,
  System.Actions, Vcl.ActnList, Vcl.ComCtrls, Vcl.ToolWin, Vcl.StdCtrls,
  EF.Tree, KIDE.NodeDesignerFrameUnit, KIDE.PairsValuesFrameUnit,
  SynEdit, SynHighlighterHtml, Vcl.Buttons;

type
  TFormLayoutDesignerFrame = class(TTreeDesignerFrame)
    EditorPageControl: TPageControl;
    OptionsTabSheet: TTabSheet;
    MemoWidthLabel: TLabel;
    _MemoWidth: TSpinEdit;
    MaxFieldWidthLabel: TLabel;
    _MaxFieldWidth: TSpinEdit;
    _MinFieldWidth: TSpinEdit;
    MinFieldWidthLabel: TLabel;
    MsgTargetComboBox: TComboBox;
    MsgTargetLabel: TLabel;
    _RequiredLabelTemplate: TLabeledEdit;
    _LabelWidth: TSpinEdit;
    LabelWidthLabel: TLabel;
    LabelAlignComboBox: TComboBox;
    LabelAlignLabel: TLabel;
    _LabelSeparator: TLabeledEdit;
    UpdateLayoutAction: TAction;
    ToolButton1: TToolButton;
    UpdateLayoutToolButton: TToolButton;
    procedure UpdateLayoutActionExecute(Sender: TObject);
  private
    //procedure Preview;
  protected
    class function SuitsTree(const ATree: TEFTree): Boolean; override;
    procedure CleanupDefaultsToEditNode; override;
    procedure UpdateDesignPanel(const AForce: Boolean = False); override;
    procedure DesignPanelToEditNode; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

{$R *.dfm}

uses
  EF.Classes, KIDE.Utils, KIDE.Project,
  Kitto.Ext.Editors, Kitto.Ext.Form, Kitto.Metadata.Views, Kitto.Metadata.DataView,
  KIDE.DesignMetadata, KIDE.PreviewLayoutForm;

{ TLayoutDesignerFrame }

procedure TFormLayoutDesignerFrame.CleanupDefaultsToEditNode;
begin
  inherited;
  CleanupIntegerNode('MemoWidth', LAYOUT_MEMOWIDTH);
  CleanupIntegerNode('MaxFieldWidth', LAYOUT_MAXFIELDWIDTH);
  CleanupIntegerNode('LabelWidth', FORM_LABELWIDTH);
  CleanupIntegerNode('MinFieldWidth', LAYOUT_MINFIELDWIDTH);
  CleanupTextNode('MsgTarget', LAYOUT_MSGTARGET);
  CleanupTextNode('RequiredLabelTemplate', LAYOUT_REQUIREDLABELTEMPLATE);
  CleanupTextNode('LabelAlign', 'Top');
  CleanupTextNode('LabelSeparator', ':');
end;

constructor TFormLayoutDesignerFrame.Create(AOwner: TComponent);
begin
  inherited;
end;

procedure TFormLayoutDesignerFrame.DesignPanelToEditNode;
begin
  inherited;
  EditNode.SetString('MsgTarget', MsgTargetComboBox.Text);
  EditNode.SetString('LabelAlign', LabelAlignComboBox.Text);
end;

(*
procedure TFormLayoutDesignerFrame.Preview;
var
  LView: TKDataView;
  LViewTable: TKViewTable;
  LLayout: TKLayout;
  LControllerNode: TEFNode;
begin
  inherited;
  LLayout := EditNode as TKLayout;
  LView := GetViewOfLayout(LLayout);
  if LView <> nil then
  begin
    LViewTable := LView.MainTable;
    LControllerNode := LViewTable.FindNode('Controller');
    Assert(Assigned(LViewTable));
    Assert(Assigned(LView));
    Assert(Assigned(LControllerNode));
    if Assigned(LViewTable) then
      ShowLayout(TKDataView(LView), LViewTable, LLayout, LControllerNode);
  end;
end;
*)

procedure TFormLayoutDesignerFrame.UpdateLayoutActionExecute(Sender: TObject);
var
  LViewTable: TKViewTable;
  LLayout: TKLayout;
  I: Integer;
  LField: TKViewField;
  LNode: TEFNode;
begin
  inherited;
  LLayout := EditNode as TKLayout;
  LViewTable := GetViewTableOfLayout(LLayout, 'Form');
  if LViewTable <> nil then
  begin
    for I := 0 to LViewTable.FieldCount -1 do
    begin
      LField := LViewTable.Fields[I];
      if not LField.IsVisible then
        Continue;
      LNode := LLayout.FindChildByNameAndValue('Field', LField.AliasedName, True);
      if not Assigned(LNode) then
        LLayout.AddChild('Field', LField.AliasedName);
    end;
    Apply;
    UpdateDesigner;
  end;
end;

class function TFormLayoutDesignerFrame.SuitsTree(const ATree: TEFTree): Boolean;
begin
  Result := (ATree is TKLayout) and TKLayout(ATree).IsFormLayout;
end;

procedure TFormLayoutDesignerFrame.UpdateDesignPanel(const AForce: Boolean);
var
  LMsgTarget, LLabelAlign: string;
begin
  inherited;
  _MemoWidth.Value := EditNode.GetInteger('MemoWidth', LAYOUT_MEMOWIDTH);
  _MaxFieldWidth.Value := EditNode.GetInteger('MaxFieldWidth', LAYOUT_MAXFIELDWIDTH);
  _MinFieldWidth.Value := EditNode.GetInteger('MinFieldWidth', LAYOUT_MINFIELDWIDTH);
  _RequiredLabelTemplate.Text :=  EditNode.GetString('RequiredLabelTemplate', LAYOUT_REQUIREDLABELTEMPLATE);
  _LabelSeparator.Text := EditNode.GetString('LabelSeparator', ':');
  _LabelWidth.Value := EditNode.GetInteger('LabelWidth', 120);

  LMsgTarget := EditNode.GetString('MsgTarget', LAYOUT_MSGTARGET);
  MsgTargetComboBox.ItemIndex := MsgTargetComboBox.Items.IndexOf(LMsgTarget);

  LLabelAlign := EditNode.GetString('LabelAlign', 'Top');
  LabelAlignComboBox.ItemIndex := LabelAlignComboBox.Items.IndexOf(LLabelAlign);
end;

initialization
  TTreeDesignerFrameRegistry.Instance.RegisterClass(TFormLayoutDesignerFrame.GetClassId, TFormLayoutDesignerFrame);

finalization
  TTreeDesignerFrameRegistry.Instance.UnregisterClass(TFormLayoutDesignerFrame.GetClassId);

end.
