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
unit KIDE.LoginViewDesignerFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, KIDE.ViewDesignerFrameUnit,
  Vcl.StdActns, System.Actions, Vcl.ActnList, Vcl.Tabs, KIDE.BaseFrameUnit,
  KIDE.CodeEditorFrameUnit, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Buttons,
  Vcl.ComCtrls, Vcl.ToolWin, EF.Tree,
  KIDE.SubViewDesignerFrameUnit, Vcl.Samples.Spin;

type
  TLoginViewDesignerFrame = class(TSubViewDesignerFrame)
  private
  protected
    class function SuitsTree(const ATree: TEFTree): Boolean; override;
    class function SuitsNode(const ANode: TEFNode): Boolean; override;
    procedure CleanupDefaultsToEditNode; override;
  public
    procedure Init(const ANode: TEFTree); override;
  end;

implementation

{$R *.dfm}

uses
  EF.Classes,
  Kitto.Metadata.Views,
  KIDE.Utils, KIDE.Project, KIDE.EditNodeBaseFrameUnit;

{ TLoginViewDesignerFrame }

procedure TLoginViewDesignerFrame.CleanupDefaultsToEditNode;
begin
  inherited;
end;

procedure TLoginViewDesignerFrame.Init(const ANode: TEFTree);
begin
  inherited;
  ;
end;

class function TLoginViewDesignerFrame.SuitsTree(const ATree: TEFTree): Boolean;
begin
  Result := IsLoginViewNode(ATree);
end;

class function TLoginViewDesignerFrame.SuitsNode(const ANode: TEFNode): Boolean;
begin
  Result := IsLoginViewNode(ANode);
end;

initialization
  TEditNodeFrameRegistry.Instance.RegisterClass(TLoginViewDesignerFrame.GetClassId, TLoginViewDesignerFrame);

finalization
  if Assigned(TEditNodeFrameRegistry.Instance) then
    TEditNodeFrameRegistry.Instance.UnregisterClass(TLoginViewDesignerFrame.GetClassId);

end.
