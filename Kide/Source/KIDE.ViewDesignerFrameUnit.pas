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
unit KIDE.ViewDesignerFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, KIDE.TreeDesignerFrameUnit,
  KIDE.BaseFrameUnit, KIDE.CodeEditorFrameUnit, Vcl.ExtCtrls, Vcl.Tabs,
  System.Actions, Vcl.ActnList, Vcl.ComCtrls, Vcl.ToolWin, Vcl.StdCtrls, Vcl.Buttons,
  EF.Tree, Kitto.Metadata.Views, Vcl.StdActns, KIDE.NodeDesignerFrameUnit;

type
  TViewDesignerFrame = class(TTreeDesignerFrame)
    FileOpenAction: TFileOpen;
  strict private
  private
  protected
    class function SuitsTree(const ATree: TEFTree): Boolean; override;
    procedure UpdateDesignPanel(const AForce: Boolean = False); override;
    procedure DesignPanelToEditNode; override;
    procedure CleanupDefaultsToEditNode; override;
  public
    procedure Init(const ANode: TEFTree); override;
  end;

implementation

{$R *.dfm}

uses
  EF.Macros,
  KIDE.Project, KIDE.Utils, KIDE.EditNodeBaseFrameUnit,
  Kitto.Ext.Base,
  //View classes for this designer
  Kitto.Ext.DataTool;

{ TDownloadFileToolDesignerFrame }

procedure TViewDesignerFrame.CleanupDefaultsToEditNode;
begin
  inherited;
end;

procedure TViewDesignerFrame.DesignPanelToEditNode;
begin
  inherited;
end;

class function TViewDesignerFrame.SuitsTree(const ATree: TEFTree): Boolean;
begin
  Result := ATree is TKView;
end;

procedure TViewDesignerFrame.UpdateDesignPanel(const AForce: Boolean);
begin
  inherited;
end;

procedure TViewDesignerFrame.Init(const ANode: TEFTree);
var
  LViewNode: TEFTree;
  LFrameClass: TEditNodeBaseFrameClass;
begin
  inherited;
  LViewNode := EditNode;
  if EditNode is TEFNode then
    LFrameClass := TEditNodeFrameFactory.Instance.GetEditNodeFrameClass(EditNode)
  else
    LFrameClass := TEditNodeFrameFactory.Instance.GetEditNodeFrameClass(EditNode);
  if Assigned(LFrameClass) then
    EmbedEditNodeFrame(DesignPanel, LFrameClass, EditNode);
end;

initialization
  TTreeDesignerFrameRegistry.Instance.RegisterClass(TViewDesignerFrame.GetClassId, TViewDesignerFrame);

finalization
  TTreeDesignerFrameRegistry.Instance.UnregisterClass(TViewDesignerFrame.GetClassId);

end.
