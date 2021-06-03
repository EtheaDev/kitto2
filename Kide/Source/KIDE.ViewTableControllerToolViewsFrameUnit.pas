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
unit KIDE.ViewTableControllerToolViewsFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, System.Actions,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Graphics, Vcl.ComCtrls,
  Vcl.ActnList, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.Samples.Spin,
  EF.Tree,
  KIDE.EditNodeBaseFrameUnit, KIDE.ActionToolDesignerFrameUnit;

type
  TViewTableControllerToolViewsFrame = class(TEditNodeBaseFrame)
    ItemsTabControl: TTabControl;
    procedure ItemsTabControlChange(Sender: TObject);
  private
    ActionToolDesignerFrame: TActionToolDesignerFrame;
  protected
    class function SuitsNode(const ANode: TEFNode): Boolean; override;
    procedure UpdateDesignPanel(const AForce: Boolean = False); override;
  public
    class function IsListControllerButtonListFilterItemsNode(const ANode: TEFNode): Boolean;
    procedure Init(const ANode: TEFTree); override;
  end;

implementation

{$R *.dfm}

uses
  Kitto.Ext.Filters,
  KIDE.MainTableControllerDesignerFrameUnit;

{ TViewTableControllerToolViewsFrame }

procedure TViewTableControllerToolViewsFrame.Init(const ANode: TEFTree);
begin
  inherited;
  if ANode.ChildCount > 0 then
    ActionToolDesignerFrame := EmbedEditNodeFrame(ItemsTabControl, TActionToolDesignerFrame,
      ANode.Children[0]) as TActionToolDesignerFrame;
end;

class function TViewTableControllerToolViewsFrame.IsListControllerButtonListFilterItemsNode(
  const ANode: TEFNode): Boolean;
begin
  Result := SameText(ANode.Name, 'ToolViews') and (ANode.Parent is TEFNode) and
    TMainTableControllerDesignerFrame.IsViewTableControllerNode(TEFNode(ANode.Parent));
end;

procedure TViewTableControllerToolViewsFrame.ItemsTabControlChange(Sender: TObject);
var
  LNodeName: string;
  LNode: TEFNode;
begin
  inherited;
  LNodeName := ItemsTabControl.Tabs[ItemsTabControl.TabIndex];
  LNode := EditNode.FindNode(LNodeName);
  ActionToolDesignerFrame.Init(LNode);
  ActionToolDesignerFrame.UpdateDesignPanel;
end;

class function TViewTableControllerToolViewsFrame.SuitsNode(const ANode: TEFNode): Boolean;
begin
  Result := IsListControllerButtonListFilterItemsNode(ANode);
end;

procedure TViewTableControllerToolViewsFrame.UpdateDesignPanel(const AForce: Boolean);
var
  I: Integer;
begin
  inherited;
  ItemsTabControl.Tabs.Clear;
  for I := 0 to EditNode.ChildCount -1 do
    ItemsTabControl.Tabs.Add(EditNode.Children[I].Name);
  if ItemsTabControl.Tabs.Count > 0 then
    ItemsTabControlChange(ItemsTabControl);
end;

initialization
  TEditNodeFrameRegistry.Instance.RegisterClass(TViewTableControllerToolViewsFrame.GetClassId, TViewTableControllerToolViewsFrame);

finalization
  if Assigned(TEditNodeFrameRegistry.Instance) then
    TEditNodeFrameRegistry.Instance.UnregisterClass(TViewTableControllerToolViewsFrame.GetClassId);

end.
