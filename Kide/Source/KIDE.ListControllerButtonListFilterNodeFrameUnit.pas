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
unit KIDE.ListControllerButtonListFilterNodeFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, System.Actions,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Graphics, Vcl.ComCtrls,
  Vcl.ActnList, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.Samples.Spin,
  EF.Tree,
  KIDE.EditNodeBaseFrameUnit, KIDE.ListControllerButtonListFilterBaseNodeFrameUnit;

type
  TListControllerButtonListFilterNodeFrame = class(TListControllerButtonListFilterBaseNodeFrame)
  private
  protected
    class function SuitsNode(const ANode: TEFNode): Boolean; override;
    procedure CleanupDefaultsToEditNode; override;
    procedure UpdateDesignPanel(const AForce: Boolean = False); override;
    procedure DesignPanelToEditNode; override;
  public
    class function IsListControllerButtonListFilterNode(const ANode: TEFNode): Boolean;
    procedure Init(const ANode: TEFTree); override;
  end;

implementation

{$R *.dfm}

uses
  Kitto.Ext.Filters,
  KIDE.ListControllerFiltersNodeFrameUnit,
  KIDE.ListControllerFiltersItemsNodeFrameUnit,
  KIDE.ListControllerButtonListFilterItemsFrameUnit;

{ TListControllerButtonListFilterNodeFrame }

procedure TListControllerButtonListFilterNodeFrame.CleanupDefaultsToEditNode;
begin
  inherited;
  CleanupOrphanNode('Items');
end;

procedure TListControllerButtonListFilterNodeFrame.DesignPanelToEditNode;
begin
  inherited;
  ;
end;

procedure TListControllerButtonListFilterNodeFrame.Init(const ANode: TEFTree);
begin
  inherited;
  EmbedEditNodeFrame(DesignPanel, TListControllerButtonListFilterItemsFrame,
    ANode.FindNode('Items'));
end;

class function TListControllerButtonListFilterNodeFrame.IsListControllerButtonListFilterNode(
  const ANode: TEFNode): Boolean;
var
  LClass: TClass;
begin
  Result := False;
  if (ANode.Parent is TEFNode) and
    TListControllerFiltersItemsNodeFrame.IsListControllerFiltersItemsNode(TEFNode(ANode.Parent)) then
  begin
    LClass := TKExtFilterRegistry.Instance.FindClass(ANode.Name);
    Result := Assigned(LClass) and LClass.InheritsFrom(TKButtonListFilter);
  end;
end;

class function TListControllerButtonListFilterNodeFrame.SuitsNode(const ANode: TEFNode): Boolean;
begin
  Result := IsListControllerButtonListFilterNode(ANode);
end;

procedure TListControllerButtonListFilterNodeFrame.UpdateDesignPanel(const AForce: Boolean);
begin
  inherited;
end;

initialization
  TEditNodeFrameRegistry.Instance.RegisterClass(TListControllerButtonListFilterNodeFrame.GetClassId, TListControllerButtonListFilterNodeFrame);

finalization
  if Assigned(TEditNodeFrameRegistry.Instance) then
    TEditNodeFrameRegistry.Instance.UnregisterClass(TListControllerButtonListFilterNodeFrame.GetClassId);

end.
