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
unit KIDE.ViewTableFormControllerDesignerFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, KIDE.EditNodeBaseFrameUnit,
  Vcl.ExtCtrls, Vcl.Tabs,
  System.Actions, Vcl.ActnList, Vcl.ComCtrls, Vcl.ToolWin, Vcl.StdCtrls, Vcl.Buttons,
  KIDE.BaseFrameUnit, KIDE.CodeEditorFrameUnit,
  EF.Tree, Vcl.Samples.Spin, Vcl.StdActns,
  KIDE.MainDataModuleUnit, KIDE.FormControllerButtonDesignerFrameUnit;

type
  TViewTableFormControllerDesignerFrame = class(TEditNodeBaseFrame)
    FormGroupBox: TGroupBox;
    ButtonScaleComboBox: TComboBox;
    ButtonScaleLabel: TLabel;
    ItemsTabControl: TTabControl;
    _KeepOpenAfterOperation: TCheckBox;
    procedure ItemsTabControlChange(Sender: TObject);
  private
    FormControllerButtonDesignerFrame: TFormControllerButtonDesignerFrame;
    function BuildButtonPages: Integer;
  protected
    class function SuitsNode(const ANode: TEFNode): Boolean; override;
    procedure UpdateDesignPanel(const AForce: Boolean = False); override;
    procedure CleanupDefaultsToEditNode; override;
    procedure DesignPanelToEditNode; override;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    class function IsViewTableFormControllerNode(const ANode: TEFNode): Boolean;
    procedure Init(const ANode: TEFTree); override;
  end;

implementation

{$R *.dfm}

uses
  StrUtils,
  EF.Macros,
  Kitto.Ext.Base,
  Kitto.Ext.Form,
  KIDE.Project, KIDE.Config, KIDE.MainTableControllerDesignerFrameUnit;

{ TViewTableFormControllerDesignerFrame }

function TViewTableFormControllerDesignerFrame.BuildButtonPages: Integer;
var
  I: Integer;
  LNodeName: string;
begin
  Result := 0;
  ItemsTabControl.Tabs.Clear;
  for I := 0 to EditNode.ChildCount -1 do
  begin
    LNodeName := EditNode.Children[I].Name;
    if MatchText(LNodeName, ['CloneButton', 'ConfirmButton', 'CancelButton', 'CloseButton']) then
    begin
      ItemsTabControl.Tabs.Add(LNodeName);
      FormControllerButtonDesignerFrame := EmbedEditNodeFrame(ItemsTabControl, TFormControllerButtonDesignerFrame,
        EditNode.Children[I]) as TFormControllerButtonDesignerFrame;
    end;
    Inc(Result);
  end;
  if Result = 0 then
  begin
    EmbedEditNodeFrame(ItemsTabControl, TFormControllerButtonDesignerFrame, nil);
    FormControllerButtonDesignerFrame := nil;
  end;
end;

procedure TViewTableFormControllerDesignerFrame.CleanupDefaultsToEditNode;
begin
  inherited;
  CleanupTextNode('ButtonScale', 'medium');
  CleanupOrphanNode('ConfirmButton');
  CleanupOrphanNode('CancelButton');
  CleanupOrphanNode('CloseButton');
  CleanupBooleanNode('KeepOpenAfterOperation');
  BuildButtonPages;
end;

constructor TViewTableFormControllerDesignerFrame.Create(AOwner: TComponent);
begin
  inherited;
  TKideConfig.Instance.Config.GetNode('ButtonScales').GetChildValues(ButtonScaleComboBox.Items);
end;

procedure TViewTableFormControllerDesignerFrame.DesignPanelToEditNode;
begin
  inherited;
  EditNode.SetString('ButtonScale' ,ButtonScaleComboBox.Text);
end;

procedure TViewTableFormControllerDesignerFrame.Init(const ANode: TEFTree);
begin
  inherited;
  BuildButtonPages;
end;

class function TViewTableFormControllerDesignerFrame.IsViewTableFormControllerNode(
  const ANode: TEFNode): Boolean;
begin
  Result := SameText(ANode.Name, 'FormController') and (ANode.Parent is TEFNode) and
    TMainTableControllerDesignerFrame.IsViewTableControllerNode(TEFNode(ANode.Parent));
end;

procedure TViewTableFormControllerDesignerFrame.ItemsTabControlChange(
  Sender: TObject);
var
  LNodeName: string;
  LNode: TEFNode;
begin
  inherited;
  LNodeName := ItemsTabControl.Tabs[ItemsTabControl.TabIndex];
  LNode := EditNode.FindNode(LNodeName);
  if Assigned(LNode) then
  begin
    FormControllerButtonDesignerFrame.Init(LNode);
    FormControllerButtonDesignerFrame.UpdateDesignPanel;
  end
  else
    FormControllerButtonDesignerFrame := nil;
end;

class function TViewTableFormControllerDesignerFrame.SuitsNode(
  const ANode: TEFNode): Boolean;
begin
  Assert(Assigned(ANode));
  Result := IsViewTableFormControllerNode(ANode);
end;

procedure TViewTableFormControllerDesignerFrame.UpdateDesignPanel(
  const AForce: Boolean);
var
  LButtonScale: string;
begin
  inherited;
  Assert(Assigned(EditNode));
  LButtonScale := EditNode.GetString('ButtonScale', 'medium');
  ButtonScaleComboBox.ItemIndex := ButtonScaleComboBox.Items.IndexOf(LButtonScale);
  if BuildButtonPages > 0 then
    ItemsTabControlChange(ItemsTabControl);
end;

initialization
  TEditNodeFrameRegistry.Instance.RegisterClass(TViewTableFormControllerDesignerFrame.GetClassId, TViewTableFormControllerDesignerFrame);

finalization
  if Assigned(TEditNodeFrameRegistry.Instance) then
    TEditNodeFrameRegistry.Instance.UnregisterClass(TViewTableFormControllerDesignerFrame.GetClassId);

end.
