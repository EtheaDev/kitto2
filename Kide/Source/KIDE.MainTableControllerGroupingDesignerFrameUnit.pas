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
unit KIDE.MainTableControllerGroupingDesignerFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, KIDE.EditNodeBaseFrameUnit,
  Vcl.ExtCtrls, Vcl.Tabs, Vcl.Samples.Spin,
  System.Actions, Vcl.ActnList, Vcl.ComCtrls, Vcl.ToolWin, Vcl.StdCtrls, Vcl.Buttons,
  KIDE.BaseFrameUnit, KIDE.CodeEditorFrameUnit, KIDE.PanelControllerDesignerFrameUnit,
  EF.Tree, KIDE.ControllerDesignerFrameUnit;

type
  TMainTableControllerGroupingDesignerFrame = class(TEditNodeBaseFrame)
    _FieldName: TLabeledEdit;
    _SortFieldNames: TLabeledEdit;
    _EnableMenu: TCheckBox;
    _StartCollapsed: TCheckBox;
    _ShowName: TCheckBox;
    ShowCountGroupBox: TGroupBox;
    _ShowCount_Template: TLabeledEdit;
    _ShowCount_PluralItemName: TLabeledEdit;
    _ShowCount_ItemName: TLabeledEdit;
    _ShowCount: TCheckBox;
    procedure _ShowCountClick(Sender: TObject);
  strict private
  protected
    class function SuitsNode(const ANode: TEFNode): Boolean; override;
    procedure UpdateDesignPanel(const AForce: Boolean = False); override;
    procedure CleanupDefaultsToEditNode; override;
  protected
  public
  end;

implementation

{$R *.dfm}

uses
  EF.Macros,
  Kitto.Ext.Base,
  Kitto.Ext.GridPanel, Kitto.Metadata.DataView;

{ TDownloadFileToolDesignerFrame }

procedure TMainTableControllerGroupingDesignerFrame.CleanupDefaultsToEditNode;
begin
  inherited;
  //Cleaning Grouping Node
  CleanupTextNode('SortFieldNames');
  CleanupTextNode('FieldName');
  CleanupBooleanNode('EnableMenu');
  CleanupBooleanNode('StartCollapsed');
  CleanupBooleanNode('ShowName');
  CleanupBooleanNode('ShowCount');
  if not EditNode.GetBoolean('ShowCount') then
  begin
    CleanupBooleanNode('Template');
    CleanupBooleanNode('PluralItemName');
    CleanupBooleanNode('ItemName');
  end;
end;

class function TMainTableControllerGroupingDesignerFrame.SuitsNode(
  const ANode: TEFNode): Boolean;
begin
  Assert(Assigned(ANode));
  Result := SameText(ANode.Name, 'Grouping') and
    (ANode.Parent is TEFNode) and SameText(TEFNode(ANode.Parent).Name, 'Controller') and
    (TEFNode(ANode.Parent).Parent is TKViewTable);
end;

procedure TMainTableControllerGroupingDesignerFrame.UpdateDesignPanel(
  const AForce: Boolean);
begin
  inherited;
end;

procedure TMainTableControllerGroupingDesignerFrame._ShowCountClick(
  Sender: TObject);
begin
  inherited;
  ShowCountGroupBox.Enabled := _ShowCount.Checked;
end;

initialization
  TEditNodeFrameRegistry.Instance.RegisterClass(TMainTableControllerGroupingDesignerFrame.GetClassId, TMainTableControllerGroupingDesignerFrame);

finalization
  if Assigned(TEditNodeFrameRegistry.Instance) then
    TEditNodeFrameRegistry.Instance.UnregisterClass(TMainTableControllerGroupingDesignerFrame.GetClassId);

end.
