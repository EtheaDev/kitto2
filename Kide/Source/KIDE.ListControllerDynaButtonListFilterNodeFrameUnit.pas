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
unit KIDE.ListControllerDynaButtonListFilterNodeFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, System.Actions,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Graphics, Vcl.ComCtrls,
  Vcl.ActnList, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.Samples.Spin,
  EF.Tree,
  KIDE.EditNodeBaseFrameUnit, KIDE.ListControllerButtonListFilterBaseNodeFrameUnit,
  SynEdit, SynHighlighterSQL, KIDE.ListControllerListFilterBaseNodeFrameUnit;


type
  TListControllerDynaButtonListFilterNodeFrame = class(TListControllerButtonListFilterBaseNodeFrame)
    _ExpressionTemplate: TLabeledEdit;
    CommandTextPanel: TPanel;
    CommandTextLabel: TLabel;
  private
    FSynSQLSyn: TSynSQLSyn;
    FCommandTextEdit: TSynEdit;
  protected
    class function SuitsNode(const ANode: TEFNode): Boolean; override;
    procedure CleanupDefaultsToEditNode; override;
    procedure UpdateDesignPanel(const AForce: Boolean = False); override;
    procedure DesignPanelToEditNode; override;
  public
    class function IsListControllerDynaButtonListFilterNode(const ANode: TEFNode): Boolean;
    constructor Create(AOwner: TComponent); override;
  end;

implementation

{$R *.dfm}

uses
  KIDE.Utils,
  Kitto.Ext.Filters,
  KIDE.ListControllerFiltersNodeFrameUnit,
  KIDE.ListControllerFiltersItemsNodeFrameUnit,
  KIDE.ListControllerListFilterItemsFrameUnit;

{ TListControllerListFilterNodeFrame }

procedure TListControllerDynaButtonListFilterNodeFrame.CleanupDefaultsToEditNode;
begin
  inherited;
  CleanupTextNode('ExpressionTemplate');
  CleanupTextNode('CommandText');
end;

constructor TListControllerDynaButtonListFilterNodeFrame.Create(AOwner: TComponent);
begin
  inherited;
  FSynSQLSyn := TSynSQLSyn.Create(Self);
  FCommandTextEdit := CreateSynEditor(Self, CommandTextPanel,
    '_CommandText', FSynSQLSyn, Font.Size, EditorChange);
end;

procedure TListControllerDynaButtonListFilterNodeFrame.DesignPanelToEditNode;
begin
  inherited;
  ;
end;

class function TListControllerDynaButtonListFilterNodeFrame.IsListControllerDynaButtonListFilterNode(
  const ANode: TEFNode): Boolean;
var
  LClass: TClass;
begin
  Result := False;
  if (ANode.Parent is TEFNode) and
    TListControllerFiltersItemsNodeFrame.IsListControllerFiltersItemsNode(TEFNode(ANode.Parent)) then
  begin
    LClass := TKExtFilterRegistry.Instance.FindClass(ANode.Name);
    Result := Assigned(LClass) and LClass.InheritsFrom(TKDynaButtonListFilter);
  end;
end;

class function TListControllerDynaButtonListFilterNodeFrame.SuitsNode(const ANode: TEFNode): Boolean;
begin
  Result := IsListControllerDynaButtonListFilterNode(ANode);
end;

procedure TListControllerDynaButtonListFilterNodeFrame.UpdateDesignPanel(const AForce: Boolean);
begin
  inherited;
end;

initialization
  TEditNodeFrameRegistry.Instance.RegisterClass(TListControllerDynaButtonListFilterNodeFrame.GetClassId, TListControllerDynaButtonListFilterNodeFrame);

finalization
  if Assigned(TEditNodeFrameRegistry.Instance) then
    TEditNodeFrameRegistry.Instance.UnregisterClass(TListControllerDynaButtonListFilterNodeFrame.GetClassId);

end.
