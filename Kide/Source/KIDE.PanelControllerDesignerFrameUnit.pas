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
unit KIDE.PanelControllerDesignerFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, KIDE.EditNodeBaseFrameUnit,
  Vcl.ExtCtrls, Vcl.Tabs,
  System.Actions, Vcl.ActnList, Vcl.ComCtrls, Vcl.ToolWin, Vcl.StdCtrls, Vcl.Buttons,
  KIDE.BaseFrameUnit, KIDE.CodeEditorFrameUnit, KIDE.ControllerDesignerFrameUnit,
  EF.Tree, Vcl.Samples.Spin;

type
  TPanelControllerDesignerFrame = class(TControllerDesignerFrame)
    PanelControllerGroupBox: TGroupBox;
    WidthLabel: TLabel;
    HeightLabel: TLabel;
    _Width: TSpinEdit;
    _Height: TSpinEdit;
    _Split: TCheckBox;
    _Border: TCheckBox;
    _Collapsible: TCheckBox;
    _Header: TCheckBox;
    _Title: TLabeledEdit;
  private
  protected
    class function SuitsNode(const ANode: TEFNode): Boolean; override;
    procedure UpdateDesignPanel(const AForce: Boolean = False); override;
    procedure CleanupDefaultsToEditNode; override;
  protected
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

uses
  EF.Macros,
  Ext.Base,
  Kitto.Ext.Base,
  Kitto.Ext.AccordionPanel,
  Kitto.Ext.ToolBar,
  Kitto.Ext.TreePanel,
  Kitto.Ext.GridPanel,
  Kitto.Ext.DataPanelLeaf,
  Kitto.Metadata.Views,
  Kitto.Ext.List,
  Kitto.Ext.Form;

{ TDownloadFileToolDesignerFrame }

procedure TPanelControllerDesignerFrame.CleanupDefaultsToEditNode;
begin
  inherited;
  CleanupTextNode('Title');
  CleanupIntegerNode('Width');
  CleanupIntegerNode('Height');
  CleanupBooleanNode('Split');
  CleanupBooleanNode('Border');
  CleanupBooleanNode('Collapsible');
  CleanupBooleanNode('Header');
end;

class function TPanelControllerDesignerFrame.SuitsNode(
  const ANode: TEFNode): Boolean;
var
  LControllerClass: TClass;
begin
  Assert(Assigned(ANode));
  LControllerClass := GetControllerClass(ANode);
  Result := Assigned(LControllerClass) and
    LControllerClass.InheritsFrom(TExtTabPanel);
end;

procedure TPanelControllerDesignerFrame.UpdateDesignPanel(
  const AForce: Boolean);
begin
  inherited;
  Assert(Assigned(EditNode));
end;

initialization
  TEditNodeFrameRegistry.Instance.RegisterClass(TPanelControllerDesignerFrame.GetClassId, TPanelControllerDesignerFrame);

finalization
  if Assigned(TEditNodeFrameRegistry.Instance) then
    TEditNodeFrameRegistry.Instance.UnregisterClass(TPanelControllerDesignerFrame.GetClassId);

end.
