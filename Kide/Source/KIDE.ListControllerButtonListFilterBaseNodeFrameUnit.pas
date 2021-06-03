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
unit KIDE.ListControllerButtonListFilterBaseNodeFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, KIDE.EditNodeBaseFrameUnit, System.Actions, Vcl.ActnList,
  Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.Samples.Spin, Vcl.ComCtrls,
  EF.Tree,
  Kitto.Ext.Filters, Kitto.Ext.List, KIDE.PanelControllerDesignerFrameUnit;

type
  TListControllerButtonListFilterBaseNodeFrame = class(TEditNodeBaseFrame)
    ListFilterGroupBox: TGroupBox;
    LabelEdit: TLabeledEdit;
    _IsSingleSelect: TCheckBox;
    ConnectorRadioGroup: TRadioGroup;
    ButtonScaleComboBox: TComboBox;
    ButtonScaleLabel: TLabel;
  private
  protected
    class function SuitsNode(const ANode: TEFNode): Boolean; override;
    procedure CleanupDefaultsToEditNode; override;
    procedure UpdateDesignPanel(const AForce: Boolean = False); override;
    procedure DesignPanelToEditNode; override;
  public
    class function IsListControllerButtonListFilterBaseNode(const ANode: TEFNode): Boolean;
    constructor Create(AOwner: TComponent); override;
  end;

implementation

{$R *.dfm}

uses
  KIDE.Config,
  KIDE.ListControllerFiltersNodeFrameUnit,
  KIDE.ListControllerFiltersItemsNodeFrameUnit;

{ TListControllerFiltersNodeFrame }

procedure TListControllerButtonListFilterBaseNodeFrame.CleanupDefaultsToEditNode;
begin
  inherited;
  CleanupBooleanNode('IsSingleSelect');
  CleanupTextNode('Connector', 'or');
  CleanupTextNode('ButtonScale', 'small');
end;

constructor TListControllerButtonListFilterBaseNodeFrame.Create(AOwner: TComponent);
begin
  inherited;
  TKideConfig.Instance.Config.GetNode('ButtonScales').GetChildValues(ButtonScaleComboBox.Items);
end;

procedure TListControllerButtonListFilterBaseNodeFrame.DesignPanelToEditNode;
begin
  inherited;
  TEFNode(EditNode).AsString := LabelEdit.Text;
  EditNode.SetString('Connector', ConnectorRadioGroup.Items[ConnectorRadioGroup.ItemIndex]);
  EditNode.SetString('ButtonScale' ,ButtonScaleComboBox.Text);
end;

class function TListControllerButtonListFilterBaseNodeFrame.IsListControllerButtonListFilterBaseNode(const ANode: TEFNode): Boolean;
var
  LClass: TClass;
begin
  Result := False;
  if (ANode.Parent is TEFNode) and
    TListControllerFiltersItemsNodeFrame.IsListControllerFiltersItemsNode(TEFNode(ANode.Parent)) then
  begin
    LClass := TKExtFilterRegistry.Instance.FindClass(ANode.Name);
    Result := Assigned(LClass) and LClass.InheritsFrom(TKButtonListFilterBase);
  end;
end;

class function TListControllerButtonListFilterBaseNodeFrame.SuitsNode(const ANode: TEFNode): Boolean;
begin
  Result := IsListControllerButtonListFilterBaseNode(ANode);
end;

procedure TListControllerButtonListFilterBaseNodeFrame.UpdateDesignPanel(const AForce: Boolean);
var
  LConnector, LButtonScale: string;
begin
  inherited;
  LabelEdit.Text := TEFNode(EditNode).AsString;
  LConnector := EditNode.GetString('Connector', 'or');
  ConnectorRadioGroup.ItemIndex := ConnectorRadioGroup.Items.IndexOf(LConnector);
  LButtonScale := EditNode.GetString('ButtonScale', 'small');
  ButtonScaleComboBox.ItemIndex := ButtonScaleComboBox.Items.IndexOf(LButtonScale);
end;

initialization
  TEditNodeFrameRegistry.Instance.RegisterClass(TListControllerButtonListFilterBaseNodeFrame.GetClassId, TListControllerButtonListFilterBaseNodeFrame);

finalization
  if Assigned(TEditNodeFrameRegistry.Instance) then
    TEditNodeFrameRegistry.Instance.UnregisterClass(TListControllerButtonListFilterBaseNodeFrame.GetClassId);

end.
