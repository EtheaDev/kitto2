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
unit KIDE.TabPanelControllerDesignerFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, KIDE.EditNodeBaseFrameUnit,
  Vcl.ExtCtrls, Vcl.Tabs,
  System.Actions, Vcl.ActnList, Vcl.ComCtrls, Vcl.ToolWin, Vcl.StdCtrls, Vcl.Buttons,
  KIDE.BaseFrameUnit, KIDE.CodeEditorFrameUnit, KIDE.PanelControllerDesignerFrameUnit,
  EF.Tree, KIDE.ControllerDesignerFrameUnit, Vcl.Samples.Spin;

type
  TTabPanelControllerDesignerFrame = class(TPanelControllerDesignerFrame)
    TabPanelControllerGroupBox: TGroupBox;
    _TabIconsVisible: TCheckBox;
    _TabsVisible: TCheckBox;
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
  Kitto.Ext.TabPanel;

{ TDownloadFileToolDesignerFrame }

procedure TTabPanelControllerDesignerFrame.CleanupDefaultsToEditNode;
begin
  inherited;
  CleanupBooleanNode('TabIconsVisible', True);
  CleanupBooleanNode('TabsVisible', True);
end;

constructor TTabPanelControllerDesignerFrame.Create(AOwner: TComponent);
begin
  inherited;
end;

class function TTabPanelControllerDesignerFrame.SuitsNode(
  const ANode: TEFNode): Boolean;
var
  LControllerClass: TClass;
begin
  Assert(Assigned(ANode));
  LControllerClass := GetControllerClass(ANode);
  Result := Assigned(LControllerClass) and
    LControllerClass.InheritsFrom(TKExtTabPanelController);
end;

procedure TTabPanelControllerDesignerFrame.UpdateDesignPanel(
  const AForce: Boolean);
begin
  Assert(Assigned(EditNode));
  inherited;
  _TabIconsVisible.Checked := EditNode.GetBoolean('TabIconsVisible', True);
  _TabsVisible.Checked := EditNode.GetBoolean('TabsVisible', True);
end;

initialization
  TEditNodeFrameRegistry.Instance.RegisterClass(TTabPanelControllerDesignerFrame.GetClassId, TTabPanelControllerDesignerFrame);

finalization
  if Assigned(TEditNodeFrameRegistry.Instance) then
    TEditNodeFrameRegistry.Instance.UnregisterClass(TTabPanelControllerDesignerFrame.GetClassId);

end.
