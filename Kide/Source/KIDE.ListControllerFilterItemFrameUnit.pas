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
unit KIDE.ListControllerFilterItemFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.Actions, Vcl.ActnList,
  Vcl.ExtCtrls, EF.Tree, Vcl.StdCtrls, SynEdit, SynHighlighterSQL,
  KIDE.EditNodeBaseFrameUnit;

type
  TListControllerFilterItemFrame = class(TEditNodeBaseFrame)
    LabelEdit: TLabeledEdit;
    ExpressionTemplateGroupBox: TGroupBox;
    _DefaultValue: TLabeledEdit;
  private
    FSynSQLSyn: TSynSQLSyn;
    FExpressionEdit: TSynEdit;
  protected
    class function SuitsNode(const ANode: TEFNode): Boolean; override;
    procedure CleanupDefaultsToEditNode; override;
    procedure DesignPanelToEditNode; override;
  public
    procedure UpdateDesignPanel(const AForce: Boolean = False); override;
    class function IsListControllerFilterItemNode(const ANode: TEFNode): Boolean;
    constructor Create(AOwner: TComponent); override;
    procedure Init(const ANode: TEFTree); override;
  end;

implementation

{$R *.dfm}
uses
  KIDE.Utils,
  Kitto.Ext.Filters,
  KIDE.ListControllerFiltersItemsNodeFrameUnit,
  KIDE.ListControllerFiltersNodeFrameUnit;

{ TListControllerListFilterItemFrame }

procedure TListControllerFilterItemFrame.CleanupDefaultsToEditNode;
begin
  inherited;
  CleanupTextNode('ExpressionTemplate');
  CleanupTextNode('DefaultValue');
end;

constructor TListControllerFilterItemFrame.Create(AOwner: TComponent);
begin
  inherited;
  FSynSQLSyn := TSynSQLSyn.Create(Self);
  FExpressionEdit := CreateSynEditor(Self, ExpressionTemplateGroupBox,
    '_ExpressionTemplate', FSynSQLSyn, Font.Size, EditorChange);
  FExpressionEdit.Gutter.Visible := False;
end;

procedure TListControllerFilterItemFrame.DesignPanelToEditNode;
begin
  inherited;
  TEFNode(EditNode).AsString := LabelEdit.Text;
end;

procedure TListControllerFilterItemFrame.Init(const ANode: TEFTree);
begin
  inherited;
end;

class function TListControllerFilterItemFrame.IsListControllerFilterItemNode(
  const ANode: TEFNode): Boolean;
var
  LClass: TClass;
begin
  Result := False;
  if (ANode.Parent is TEFNode) and
    SameText(TEFNode(ANode.Parent).Name, 'Items') and
    TListControllerFiltersItemsNodeFrame.IsListControllerFiltersItemsNode(TEFNode(ANode.Parent)) then
  begin
    LClass := TKExtFilterRegistry.Instance.FindClass(ANode.Name);
    Result := Assigned(LClass) and
      (LClass.InheritsFrom(TKFreeSearchFilter) or
       LClass.InheritsFrom(TKDateSearchFilter) or
       LClass.InheritsFrom(TKBooleanSearchFilter));
  end;
end;

class function TListControllerFilterItemFrame.SuitsNode(const ANode: TEFNode): Boolean;
begin
  Result := IsListControllerFilterItemNode(ANode);
end;

procedure TListControllerFilterItemFrame.UpdateDesignPanel(const AForce: Boolean);
begin
  LabelEdit.Text := '';
  inherited;
  LabelEdit.Text := TEFNode(EditNode).AsString;
end;

initialization
  TEditNodeFrameRegistry.Instance.RegisterClass(TListControllerFilterItemFrame.GetClassId, TListControllerFilterItemFrame);

finalization
  if Assigned(TEditNodeFrameRegistry.Instance) then
    TEditNodeFrameRegistry.Instance.UnregisterClass(TListControllerFilterItemFrame.GetClassId);

end.
