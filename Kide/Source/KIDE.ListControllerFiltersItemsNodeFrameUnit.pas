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
unit KIDE.ListControllerFiltersItemsNodeFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, KIDE.EditNodeBaseFrameUnit, System.Actions, Vcl.ActnList,
  Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.Samples.Spin, Vcl.ComCtrls,
  EF.Tree,
  Kitto.Ext.Filters, Kitto.Ext.List;

type
  TListControllerFiltersItemsNodeFrame = class(TEditNodeBaseFrame)
    ItemsPageControl: TPageControl;
    ItemTabSheet: TTabSheet;
  private
  protected
    class function SuitsNode(const ANode: TEFNode): Boolean; override;
    procedure UpdateDesignPanel(const AForce: Boolean = False); override;
    procedure DesignPanelToEditNode; override;
  public
    class function IsListControllerFiltersItemsNode(const ANode: TEFNode): Boolean;
    constructor Create(AOwner: TComponent); override;
    procedure Init(const ANode: TEFTree); override;
  end;

implementation

{$R *.dfm}

uses
  KIDE.Config,
  KIDE.ListControllerFiltersNodeFrameUnit;

{ TListControllerFiltersNodeFrame }

constructor TListControllerFiltersItemsNodeFrame.Create(AOwner: TComponent);
begin
  inherited;
end;

procedure TListControllerFiltersItemsNodeFrame.DesignPanelToEditNode;
begin
  inherited;
end;

procedure TListControllerFiltersItemsNodeFrame.Init(const ANode: TEFTree);
var
  I: integer;
  LItemsNode, LItemNode: TEFNode;
  LTabSheet: TTabSheet;
  LFrameClass: TEditNodeBaseFrameClass;
begin
  inherited;
  //Clear all pages
  while ItemsPageControl.PageCount > 0 do
    ItemsPageControl.Pages[0].Free;
  //bUILD pages based on Filter Items
  LItemsNode := ANode as TEFNode;
  if Assigned(LItemsNode) then
  begin
    for I := 0 to LItemsNode.ChildCount -1 do
    begin
      LItemNode := LItemsNode.Children[I];
      LTabSheet := TTabSheet.Create(Self);
      LTabSheet.Name := Format('%s_%d', [LItemNode.Name, I]);
      LTabSheet.PageControl := ItemsPageControl;
      LTabSheet.Caption := LItemNode.Name;
      LFrameClass := TEditNodeFrameFactory.Instance.GetEditNodeFrameClass(LItemNode);
      if Assigned(LFrameClass) then
        EmbedEditNodeFrame(LTabSheet, LFrameClass, LItemNode);
    end;
  end;
end;

class function TListControllerFiltersItemsNodeFrame.IsListControllerFiltersItemsNode(
  const ANode: TEFNode): Boolean;
begin
  Result := (ANode is TEFNode) and SameText(TEFNode(ANode).Name, 'Items') and
    (TEFNode(ANode).Parent is TEFNode) and
    TListControllerFiltersNodeFrame.IsListControllerFilterNode(TEFNode(TEFNode(ANode).Parent));
end;

class function TListControllerFiltersItemsNodeFrame.SuitsNode(const ANode: TEFNode): Boolean;
begin
  Result := IsListControllerFiltersItemsNode(ANode);
end;

procedure TListControllerFiltersItemsNodeFrame.UpdateDesignPanel(const AForce: Boolean);
begin
  inherited;
end;

initialization
  TEditNodeFrameRegistry.Instance.RegisterClass(TListControllerFiltersItemsNodeFrame.GetClassId, TListControllerFiltersItemsNodeFrame);

finalization
  if Assigned(TEditNodeFrameRegistry.Instance) then
    TEditNodeFrameRegistry.Instance.UnregisterClass(TListControllerFiltersItemsNodeFrame.GetClassId);

end.
