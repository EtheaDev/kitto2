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
unit KIDE.ConnectionFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, KIDE.EditNodeBaseFrameUnit, System.Actions, Vcl.ActnList,
  Vcl.ExtCtrls, Vcl.StdCtrls, KIDE.BaseFrameUnit, KIDE.DatabaseFrameUnit,
  EF.Tree;

type
  TConnectionFrame = class(TEditNodeBaseFrame)
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
  EF.Classes, KIDE.PairsValuesFrameUnit;

{ TConfigDatabasesNodeFrame }

procedure TConnectionFrame.DesignPanelToEditNode;
begin
  inherited;
  ;
end;

procedure TConnectionFrame.Init(const ANode: TEFTree);
begin
  inherited;
  EmbedEditNodeFrame(DesignPanel, TPairsValuesFrame,
    EditNode as TEFNode);
end;

class function TConnectionFrame.SuitsNode(const ANode: TEFNode): Boolean;
begin
  Result := SameText(ANode.Name, 'Connection') and (ANode.Parent is TEFNode) and
    MatchText(TEFNode(ANode.Parent).Value, ['ADO', 'FD', 'DBX']);
end;

procedure TConnectionFrame.UpdateDesignPanel;
begin
  inherited;
end;

initialization
  TEditNodeFrameRegistry.Instance.RegisterClass(TConnectionFrame.GetClassId, TConnectionFrame);

finalization
  if Assigned(TEditNodeFrameRegistry.Instance) then
    TEditNodeFrameRegistry.Instance.UnregisterClass(TConnectionFrame.GetClassId);

end.
