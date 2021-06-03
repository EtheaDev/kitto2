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
unit KIDE.ListControllerListFilterItemsFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, System.Actions,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Graphics, Vcl.ComCtrls,
  Vcl.ActnList, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.Samples.Spin,
  EF.Tree,
  KIDE.EditNodeBaseFrameUnit, KIDE.ListControllerListFilterNodeFrameUnit,
  KIDE.ListControllerListFilterItemFrameUnit;

type
  TListControllerListFilterItemsFrame = class(TEditNodeBaseFrame)
    ItemsTabControl: TTabControl;
    procedure ItemsTabControlChange(Sender: TObject);
  private
    ListFilterItemFrame: TListControllerListFilterItemFrame;
  protected
    class function SuitsNode(const ANode: TEFNode): Boolean; override;
    procedure UpdateDesignPanel(const AForce: Boolean = False); override;
  public
    class function IsListControllerListFilterItemsNode(const ANode: TEFNode): Boolean;
    procedure Init(const ANode: TEFTree); override;
  end;

implementation

{$R *.dfm}

uses
  Kitto.Ext.Filters,
  KIDE.ListControllerFiltersNodeFrameUnit;

{ TListControllerListFilterNodeFrame }

procedure TListControllerListFilterItemsFrame.Init(const ANode: TEFTree);
begin
  inherited;
  if ANode.ChildCount > 0 then
    ListFilterItemFrame := EmbedEditNodeFrame(ItemsTabControl, TListControllerListFilterItemFrame,
      ANode.Children[0]) as TListControllerListFilterItemFrame;
end;

class function TListControllerListFilterItemsFrame.IsListControllerListFilterItemsNode(
  const ANode: TEFNode): Boolean;
begin
  Result := SameText(ANode.Name, 'Items') and (ANode.Parent is TEFNode) and
    TListControllerListFilterNodeFrame.IsListControllerListFilterNode(TEFNode(ANode.Parent));
end;

procedure TListControllerListFilterItemsFrame.ItemsTabControlChange(Sender: TObject);
var
  LNodeName: string;
  LNode: TEFNode;
begin
  inherited;
  LNodeName := ItemsTabControl.Tabs[ItemsTabControl.TabIndex];
  LNode := EditNode.FindNode(LNodeName);
  ListFilterItemFrame.Init(LNode);
  ListFilterItemFrame.UpdateDesignPanel;
end;

class function TListControllerListFilterItemsFrame.SuitsNode(const ANode: TEFNode): Boolean;
begin
  Result := IsListControllerListFilterItemsNode(ANode);
end;

procedure TListControllerListFilterItemsFrame.UpdateDesignPanel(const AForce: Boolean);
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
  TEditNodeFrameRegistry.Instance.RegisterClass(TListControllerListFilterItemsFrame.GetClassId, TListControllerListFilterItemsFrame);

finalization
  if Assigned(TEditNodeFrameRegistry.Instance) then
    TEditNodeFrameRegistry.Instance.UnregisterClass(TListControllerListFilterItemsFrame.GetClassId);

end.
