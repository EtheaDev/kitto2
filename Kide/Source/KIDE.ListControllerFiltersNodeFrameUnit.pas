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
unit KIDE.ListControllerFiltersNodeFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, KIDE.EditNodeBaseFrameUnit, System.Actions, Vcl.ActnList,
  Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.Samples.Spin, Vcl.ComCtrls,
  EF.Tree,
  Kitto.Ext.Filters, Kitto.Ext.List;

type
  TListControllerFiltersNodeFrame = class(TEditNodeBaseFrame)
    FilterPageControl: TPageControl;
    FilterPanelTabSheet: TTabSheet;
    _ColumnWidth: TSpinEdit;
    ColumnWidthLabel: TLabel;
    LabelWidthLabel: TLabel;
    _LabelWidth: TSpinEdit;
    LabelAlignLabel: TLabel;
    LabelAlignComboBox: TComboBox;
    _DisplayLabel: TLabeledEdit;
    ItemsTabSheet: TTabSheet;
    ConnectorRadioGroup: TRadioGroup;
  private
  protected
    class function SuitsNode(const ANode: TEFNode): Boolean; override;
    procedure CleanupDefaultsToEditNode; override;
    procedure UpdateDesignPanel(const AForce: Boolean = False); override;
    procedure DesignPanelToEditNode; override;
    procedure UpdateEditComponents; override;
  public
    class function IsListControllerFilterNode(const ANode: TEFNode): Boolean;
    constructor Create(AOwner: TComponent); override;
    procedure Init(const ANode: TEFTree); override;
  end;

implementation

{$R *.dfm}

uses
  KIDE.ListControllerFiltersItemsNodeFrameUnit,
  KIDE.Config;

{ TListControllerFiltersNodeFrame }

procedure TListControllerFiltersNodeFrame.CleanupDefaultsToEditNode;
begin
  inherited;
  CleanupTextNode('DisplayLabel');
  CleanupIntegerNode('LabelWidth');
  CleanupIntegerNode('ColumnWidth');
  CleanupTextNode('LabelAlign', 'Right');
  CleanupTextNode('Connector', 'or');
end;

constructor TListControllerFiltersNodeFrame.Create(AOwner: TComponent);
begin
  inherited;
  TKideConfig.Instance.Config.GetNode('LabelAligns').GetChildValues(LabelAlignComboBox.Items);
end;

procedure TListControllerFiltersNodeFrame.DesignPanelToEditNode;
begin
  inherited;
  FilterPageControl.ActivePageIndex := 0;
  EditNode.SetString('LabelAlign' ,LabelAlignComboBox.Text);
  EditNode.SetString('Connector', ConnectorRadioGroup.Items[ConnectorRadioGroup.ItemIndex]);
end;

procedure TListControllerFiltersNodeFrame.Init(const ANode: TEFTree);
begin
  inherited;
  EmbedEditNodeFrame(ItemsTabSheet, TListControllerFiltersItemsNodeFrame,
    ANode.FindNode('Items'));
end;

class function TListControllerFiltersNodeFrame.IsListControllerFilterNode(
  const ANode: TEFNode): Boolean;
var
  LClass: TClass;
begin
  Result := False;
  if SameText(ANode.Name, 'Filters') and
    Assigned(ANode.Parent) and (ANode.Parent is TEFNode) then
  begin
    LClass := GetControllerClass(TEFNode(ANode.Parent));
    Result := Assigned(LClass) and LClass.InheritsFrom(TKExtListPanelController);
  end;
end;

class function TListControllerFiltersNodeFrame.SuitsNode(const ANode: TEFNode): Boolean;
begin
  Result := IsListControllerFilterNode(ANode);
end;

procedure TListControllerFiltersNodeFrame.UpdateDesignPanel(const AForce: Boolean);
var
  LLabelAlign, LConnector: string;
begin
  inherited;
  FilterPageControl.ActivePageIndex := 0;
  LLabelAlign := EditNode.GetString('LabelAlign', 'Right');
  LabelAlignComboBox.ItemIndex := LabelAlignComboBox.Items.IndexOf(LLabelAlign);
  LConnector := EditNode.GetString('Connector', 'or');
  ConnectorRadioGroup.ItemIndex := ConnectorRadioGroup.Items.IndexOf(LConnector);
end;

procedure TListControllerFiltersNodeFrame.UpdateEditComponents;
begin
  inherited;
end;

initialization
  TEditNodeFrameRegistry.Instance.RegisterClass(TListControllerFiltersNodeFrame.GetClassId, TListControllerFiltersNodeFrame);

finalization
  if Assigned(TEditNodeFrameRegistry.Instance) then
    TEditNodeFrameRegistry.Instance.UnregisterClass(TListControllerFiltersNodeFrame.GetClassId);

end.
