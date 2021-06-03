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
unit KIDE.DataPanelControllerDesignerFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, KIDE.EditNodeBaseFrameUnit,
  Vcl.ExtCtrls, Vcl.Tabs,
  System.Actions, Vcl.ActnList, Vcl.ComCtrls, Vcl.ToolWin, Vcl.StdCtrls, Vcl.Buttons,
  KIDE.BaseFrameUnit, KIDE.CodeEditorFrameUnit, KIDE.PanelControllerDesignerFrameUnit,
  EF.Tree, KIDE.ControllerDesignerFrameUnit, KIDE.BorderPanelControllerDesignerFrameUnit,
  Vcl.Samples.Spin, KIDE.MainDataModuleUnit;

type
  TDataPanelControllerDesignerFrame = class(TBorderPanelControllerDesignerFrame)
    DataPanelGroupBox: TGroupBox;
    TopToolBar: TToolBar;
    RefreshToolButton: TToolButton;
    AddToolButton: TToolButton;
    DupToolButton: TToolButton;
    EditToolButton: TToolButton;
    DeleteToolButton: TToolButton;
    ViewToolButton: TToolButton;
  strict private
  protected
    class function SuitsNode(const ANode: TEFNode): Boolean; override;
    procedure UpdateDesignPanel(const AForce: Boolean = False); override;
    procedure CleanupDefaultsToEditNode; override;
    procedure DesignPanelToEditNode; override;
  protected
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

{$R *.dfm}

uses
  EF.Macros,
  Kitto.Ext.Base,
  Kitto.Ext.DataPanelLeaf, Kitto.Ext.Form, Kitto.Ext.List;

{ TDataPanelLeafControllerDesignerFrame }

procedure TDataPanelControllerDesignerFrame.CleanupDefaultsToEditNode;
begin
  inherited;
  CleanupBooleanNode('PreventAdding');
  CleanupBooleanNode('AllowDuplicating');
  CleanupBooleanNode('PreventEditing');
  CleanupBooleanNode('PreventDeleting');
  CleanupBooleanNode('AllowViewing');
  CleanupBooleanNode('PreventRefreshing');
end;

constructor TDataPanelControllerDesignerFrame.Create(AOwner: TComponent);
begin
  inherited;
end;

procedure TDataPanelControllerDesignerFrame.DesignPanelToEditNode;
begin
  inherited;
  EditNode.SetBoolean('PreventAdding', not AddToolButton.Down);
  EditNode.SetBoolean('AllowDuplicating', DupToolButton.Down);
  EditNode.SetBoolean('PreventEditing', not EditToolButton.Down);
  EditNode.SetBoolean('PreventDeleting', not DeleteToolButton.Down);
  EditNode.SetBoolean('AllowViewing', ViewToolButton.Down);
  EditNode.SetBoolean('PreventRefreshing', not RefreshToolButton.Down);
end;

class function TDataPanelControllerDesignerFrame.SuitsNode(
  const ANode: TEFNode): Boolean;
var
  LControllerClass: TClass;
begin
  Assert(Assigned(ANode));
  LControllerClass := GetControllerClass(ANode);
  Result := Assigned(LControllerClass) and
    LControllerClass.InheritsFrom(TKExtDataPanelLeafController);
end;

procedure TDataPanelControllerDesignerFrame.UpdateDesignPanel(
  const AForce: Boolean);
var
  LControllerClass: TClass;
begin
  inherited;
  Assert(Assigned(EditNode));
  LControllerClass := GetControllerClass(TEFNode(EditNode));
  AddToolButton.Down := not EditNode.GetBoolean('PreventAdding');
  DupToolButton.Down := EditNode.GetBoolean('AllowDuplicating');
  EditToolButton.Down := not EditNode.GetBoolean('PreventEditing');
  DeleteToolButton.Down := not EditNode.GetBoolean('PreventDeleting');
  ViewToolButton.Down := EditNode.GetBoolean('AllowViewing');
  RefreshToolButton.Down := not EditNode.GetBoolean('PreventRefreshing');
end;

initialization
  TEditNodeFrameRegistry.Instance.RegisterClass(TDataPanelControllerDesignerFrame.GetClassId, TDataPanelControllerDesignerFrame);

finalization
  if Assigned(TEditNodeFrameRegistry.Instance) then
    TEditNodeFrameRegistry.Instance.UnregisterClass(TDataPanelControllerDesignerFrame.GetClassId);

end.
