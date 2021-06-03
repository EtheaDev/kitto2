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
unit KIDE.RuleNodeFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, KIDE.EditNodeBaseFrameUnit, System.Actions, Vcl.ActnList,
  Vcl.ExtCtrls, Vcl.StdCtrls, KIDE.BaseFrameUnit, Kitto.Metadata.Models,
  EF.Tree;

type
  TRuleNodeFrame = class(TEditNodeBaseFrame)
    RuleNameEdit: TLabeledEdit;
    ParamsGroupBox: TGroupBox;
    RuleValueEdit: TLabeledEdit;
  private
  protected
    class function SuitsNode(const ANode: TEFNode): Boolean; override;
    procedure UpdateDesignPanel(const AForce: Boolean); override;
    procedure DesignPanelToEditNode; override;
  public
    procedure Init(const ANode: TEFTree); override;
  end;

implementation

{$R *.dfm}

uses
  EF.Classes, KIDE.PairsValuesFrameUnit;

{ TConfigDatabasesNodeFrame }

procedure TRuleNodeFrame.DesignPanelToEditNode;
begin
  inherited;
  (EditNode as TEFNode).Name := RuleNameEdit.Text;
  (EditNode as TEFNode).AsString := RuleValueEdit.Text;
end;

procedure TRuleNodeFrame.Init(const ANode: TEFTree);
begin
  inherited;
  EmbedEditNodeFrame(ParamsGroupBox, TPairsValuesFrame, EditNode as TEFNode);
end;

class function TRuleNodeFrame.SuitsNode(const ANode: TEFNode): Boolean;
begin
  Result := (ANode is TKRule);
end;

procedure TRuleNodeFrame.UpdateDesignPanel(const AForce: Boolean);
begin
  inherited;
  RuleNameEdit.Text := (EditNode as TEFNode).Name;
  RuleValueEdit.Text := (EditNode as TEFNode).AsString;
end;

initialization
  TEditNodeFrameRegistry.Instance.RegisterClass(TRuleNodeFrame.GetClassId, TRuleNodeFrame);

finalization
  if Assigned(TEditNodeFrameRegistry.Instance) then
    TEditNodeFrameRegistry.Instance.UnregisterClass(TRuleNodeFrame.GetClassId);

end.
