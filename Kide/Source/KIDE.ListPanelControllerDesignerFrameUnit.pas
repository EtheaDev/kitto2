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
unit KIDE.ListPanelControllerDesignerFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, KIDE.EditNodeBaseFrameUnit,
  Vcl.ExtCtrls, Vcl.Tabs,
  System.Actions, Vcl.ActnList, Vcl.ComCtrls, Vcl.ToolWin, Vcl.StdCtrls, Vcl.Buttons,
  KIDE.BaseFrameUnit, KIDE.CodeEditorFrameUnit, KIDE.PanelControllerDesignerFrameUnit,
  EF.Tree, KIDE.ControllerDesignerFrameUnit, KIDE.BorderPanelControllerDesignerFrameUnit,
  Vcl.Samples.Spin;

type
  TListPanelControllerDesignerFrame = class(TBorderPanelControllerDesignerFrame)
    ListGroupBox: TGroupBox;
    _AllowMultipleInstances: TCheckBox;
    _AllowClose: TCheckBox;
    _InplaceEditing: TCheckBox;
  strict private
  protected
    class function SuitsNode(const ANode: TEFNode): Boolean; override;
    procedure UpdateDesignPanel(const AForce: Boolean = False); override;
    procedure CleanupDefaultsToEditNode; override;
  protected
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

{$R *.dfm}

uses
  EF.Macros,
  Kitto.Ext.Base,
  Kitto.Ext.DataPanel, Kitto.Ext.Form, Kitto.Ext.List;

{ TListPanelControllerDesignerFrame }

procedure TListPanelControllerDesignerFrame.CleanupDefaultsToEditNode;
begin
  inherited;
  CleanupBooleanNode('AllowClose', True);
  CleanupBooleanNode('AllowMultipleInstances');
  CleanupBooleanNode('InplaceEditing');
end;

constructor TListPanelControllerDesignerFrame.Create(AOwner: TComponent);
begin
  inherited;
end;

class function TListPanelControllerDesignerFrame.SuitsNode(
  const ANode: TEFNode): Boolean;
var
  LControllerClass: TClass;
begin
  Assert(Assigned(ANode));
  LControllerClass := GetControllerClass(ANode);
  Result := Assigned(LControllerClass) and
    LControllerClass.InheritsFrom(TKExtListPanelController);
end;

procedure TListPanelControllerDesignerFrame.UpdateDesignPanel(
  const AForce: Boolean);
var
  LControllerClass: TClass;
begin
  inherited;
  Assert(Assigned(EditNode));
  _AllowClose.Checked := EditNode.GetBoolean('AllowClose', True);
  LControllerClass := GetControllerClass(TEFNode(EditNode));
end;

initialization
  TEditNodeFrameRegistry.Instance.RegisterClass(TListPanelControllerDesignerFrame.GetClassId, TListPanelControllerDesignerFrame);

finalization
  if Assigned(TEditNodeFrameRegistry.Instance) then
    TEditNodeFrameRegistry.Instance.UnregisterClass(TListPanelControllerDesignerFrame.GetClassId);

end.
