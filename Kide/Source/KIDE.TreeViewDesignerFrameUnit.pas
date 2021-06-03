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
unit KIDE.TreeViewDesignerFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, KIDE.SubViewDesignerFrameUnit, Vcl.StdActns, System.Actions,
  Vcl.ActnList, Vcl.Tabs, KIDE.BaseFrameUnit, KIDE.CodeEditorFrameUnit, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.Buttons, Vcl.ComCtrls, Vcl.ToolWin,
  EF.Tree, KIDE.TreeDesignerFrameUnit, Kitto.Metadata.Views,
  KIDE.ViewDesignerFrameUnit;

type
  TTreeViewDesignerFrame = class(TSubViewDesignerFrame)
  private
    function GetTreeView: TKTreeView;
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
  KIDE.EditNodeBaseFrameUnit;

{ TTreeViewDesignerFrame }

procedure TTreeViewDesignerFrame.CleanupDefaultsToEditNode;
begin
  inherited;
end;

function TTreeViewDesignerFrame.GetTreeView: TKTreeView;
begin
  Result := EditNode as TKTreeView;
end;

procedure TTreeViewDesignerFrame.Init(const ANode: TEFTree);
begin
  inherited;
  ControllerTabSheet.TabVisible := False;
  ControllerComboBox.Visible := False;
  ControllerLabel.Visible := False;
end;

class function TTreeViewDesignerFrame.SuitsTree(const ATree: TEFTree): Boolean;
begin
  Result := ATree is TKTreeView;
end;

class function TTreeViewDesignerFrame.SuitsNode(const ANode: TEFNode): Boolean;
begin
  Result := False;
end;

initialization
  TEditNodeFrameRegistry.Instance.RegisterClass(TTreeViewDesignerFrame.GetClassId, TTreeViewDesignerFrame);

finalization
  if Assigned(TEditNodeFrameRegistry.Instance) then
    TEditNodeFrameRegistry.Instance.UnregisterClass(TTreeViewDesignerFrame.GetClassId);

end.
