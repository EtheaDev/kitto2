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
unit KIDE.DatabaseNodeFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, KIDE.EditNodeBaseFrameUnit, System.Actions, Vcl.ActnList,
  Vcl.ExtCtrls, Vcl.StdCtrls, KIDE.BaseFrameUnit, KIDE.DatabaseFrameUnit,
  EF.Tree;

type
  TDatabaseNodeFrame = class(TEditNodeBaseFrame)
    ConnectionTypeEdit: TLabeledEdit;
    ConnectionGroupBox: TGroupBox;
    ConnectionNameEdit: TLabeledEdit;
  private
  protected
    class function SuitsNode(const ANode: TEFNode): Boolean; override;
  public
    procedure DesignPanelToEditNode; override;
    procedure UpdateDesignPanel(const AForce: Boolean); override;
    procedure Init(const ANode: TEFTree); override;
  end;

implementation

{$R *.dfm}

uses
  StrUtils,
  EF.Classes,
  KIDE.Utils, KIDE.PairsValuesFrameUnit;

{ TDatabaseNodeFrame }

procedure TDatabaseNodeFrame.DesignPanelToEditNode;
begin
  inherited;
  TEFNode(EditNode).Name := ConnectionNameEdit.Text;
end;

procedure TDatabaseNodeFrame.Init(const ANode: TEFTree);
begin
  inherited;
  EmbedEditNodeFrame(ConnectionGroupBox, TPairsValuesFrame,
    EditNode.GetNode('Connection', True));
end;

class function TDatabaseNodeFrame.SuitsNode(const ANode: TEFNode): Boolean;
begin
  Result := IsDatabaseNode(ANode);
end;

procedure TDatabaseNodeFrame.UpdateDesignPanel;
begin
  inherited;
  ConnectionNameEdit.Text := TEFNode(EditNode).Name;
  ConnectionTypeEdit.Text := TEFNode(EditNode).Value;
end;

initialization
  TEditNodeFrameRegistry.Instance.RegisterClass(TDatabaseNodeFrame.GetClassId, TDatabaseNodeFrame);

finalization
  if Assigned(TEditNodeFrameRegistry.Instance) then
    TEditNodeFrameRegistry.Instance.UnregisterClass(TDatabaseNodeFrame.GetClassId);

end.
