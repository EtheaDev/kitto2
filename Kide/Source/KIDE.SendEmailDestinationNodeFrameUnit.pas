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
unit KIDE.SendEmailDestinationNodeFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, KIDE.EditNodeBaseFrameUnit, System.Actions, Vcl.ActnList,
  Vcl.ExtCtrls, EF.Tree, Vcl.StdCtrls;

type
  TSendEmailDestinationNodeFrame = class(TEditNodeBaseFrame)
    DestinationEdit: TLabeledEdit;
    RecipientsGroupBox: TGroupBox;
  private
  protected
    class function SuitsNode(const ANode: TEFNode): Boolean; override;
    procedure CleanupDefaultsToEditNode; override;
    procedure UpdateDesignPanel(const AForce: Boolean = False); override;
    procedure DesignPanelToEditNode; override;
  public
    procedure Init(const ANode: TEFTree); override;
  end;

implementation

{$R *.dfm}

uses
  KIDE.PairsValuesFrameUnit, Kitto.Ext.IndyTools;

{ TSendEmailDestinationNodeFrame }

procedure TSendEmailDestinationNodeFrame.CleanupDefaultsToEditNode;
begin
  inherited;
end;

procedure TSendEmailDestinationNodeFrame.DesignPanelToEditNode;
begin
  inherited;
  TEFNode(EditNode).AsString := DestinationEdit.Text;
end;

procedure TSendEmailDestinationNodeFrame.Init(const ANode: TEFTree);
var
  PairsValuesFrame: TPairsValuesFrame;
begin
  inherited;
  PairsValuesFrame := EmbedEditNodeFrame(RecipientsGroupBox, TPairsValuesFrame,
    EditNode as TEFNode) as TPairsValuesFrame;
  PairsValuesFrame.FixedKey := 'Recipient';
end;

class function TSendEmailDestinationNodeFrame.SuitsNode(const ANode: TEFNode): Boolean;
var
  LNode: TEFNode;
begin
  Result := False;
  if (ANode is TEFNode) then
  begin
    LNode := TEFNode(ANode);
    if SameText(LNode.Name, 'To') or
      SameText(LNode.Name, 'CC') or
      SameText(LNode.Name, 'BCC') then
    begin
      if Assigned(LNode.Parent) and (LNode.Parent is TEFNode) then
        Result := SameText(TEFNode(LNode.Parent).Name, 'Message');
    end;
  end;
end;

procedure TSendEmailDestinationNodeFrame.UpdateDesignPanel(const AForce: Boolean);
begin
  inherited;
  DestinationEdit.EditLabel.Caption := TEFNode(EditNode).Name;
  DestinationEdit.Text := TEFNode(EditNode).Value;
end;

initialization
  TEditNodeFrameRegistry.Instance.RegisterClass(TSendEmailDestinationNodeFrame.GetClassId, TSendEmailDestinationNodeFrame);

finalization
  if Assigned(TEditNodeFrameRegistry.Instance) then
    TEditNodeFrameRegistry.Instance.UnregisterClass(TSendEmailDestinationNodeFrame.GetClassId);

end.
