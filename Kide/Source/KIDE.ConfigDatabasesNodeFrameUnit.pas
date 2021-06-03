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
unit KIDE.ConfigDatabasesNodeFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, KIDE.EditNodeBaseFrameUnit, System.Actions, Vcl.ActnList,
  Vcl.ExtCtrls, Vcl.StdCtrls, KIDE.BaseFrameUnit, KIDE.DatabaseFrameUnit,
  EF.Tree;

type
  TConfigDatabasesNodeFrame = class(TEditNodeBaseFrame)
  private
  protected
    class function SuitsNode(const ANode: TEFNode): Boolean; override;
  public
    procedure Init(const ANode: TEFTree); override;
  end;

implementation

{$R *.dfm}

uses
  EF.Classes, KIDE.PairsValuesFrameUnit;

{ TConfigDatabasesNodeFrame }

procedure TConfigDatabasesNodeFrame.Init(const ANode: TEFTree);
begin
  inherited;
//  EmbedEditNodeFrame(DesignPanel, TPairsValuesFrame, EditNode as TEFNode);
  EmbedEditNodeFrame(DesignPanel, TDatabaseFrame, EditNode as TEFNode);
end;

class function TConfigDatabasesNodeFrame.SuitsNode(const ANode: TEFNode): Boolean;
begin
  Result := (ANode.Parent is TEFComponentConfig) and SameText(ANode.Name, 'Databases');
end;

initialization
  TEditNodeFrameRegistry.Instance.RegisterClass(TConfigDatabasesNodeFrame.GetClassId, TConfigDatabasesNodeFrame);

finalization
  if Assigned(TEditNodeFrameRegistry.Instance) then
    TEditNodeFrameRegistry.Instance.UnregisterClass(TConfigDatabasesNodeFrame.GetClassId);

end.
