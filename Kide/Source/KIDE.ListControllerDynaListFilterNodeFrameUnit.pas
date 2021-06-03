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
unit KIDE.ListControllerDynaListFilterNodeFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, System.Actions,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Graphics, Vcl.ComCtrls,
  Vcl.ActnList, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.Samples.Spin,
  EF.Tree,
  KIDE.EditNodeBaseFrameUnit, KIDE.ListControllerListFilterBaseNodeFrameUnit,
  SynEdit, SynHighlighterSQL;


type
  TListControllerDynaListFilterNodeFrame = class(TListControllerListFilterBaseNodeFrame)
    ClientBottomPanel: TPanel;
    ExpressionTemplateGroupBox: TGroupBox;
    Splitter1: TSplitter;
    CommandTextGroupBox: TGroupBox;
    procedure ClientBottomPanelResize(Sender: TObject);
  private
    FSynSQLSyn: TSynSQLSyn;
    FCommandTextEdit: TSynEdit;
    FExpressionTemplateEdit: TSynEdit;
  protected
    class function SuitsNode(const ANode: TEFNode): Boolean; override;
    procedure CleanupDefaultsToEditNode; override;
    procedure UpdateDesignPanel(const AForce: Boolean = False); override;
    procedure DesignPanelToEditNode; override;
  public
    class function IsListControllerDynaListFilterNode(const ANode: TEFNode): Boolean;
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

procedure TListControllerDynaListFilterNodeFrame.CleanupDefaultsToEditNode;
begin
  inherited;
  CleanupTextNode('ExpressionTemplate');
  CleanupTextNode('CommandText');
end;

procedure TListControllerDynaListFilterNodeFrame.ClientBottomPanelResize(
  Sender: TObject);
begin
  inherited;
  ExpressionTemplateGroupBox.Height := ClientBottomPanel.Height div 2;
end;

constructor TListControllerDynaListFilterNodeFrame.Create(AOwner: TComponent);
begin
  inherited;
  FSynSQLSyn := TSynSQLSyn.Create(Self);
  FCommandTextEdit := CreateSynEditor(Self, CommandTextGroupBox,
    '_CommandText', FSynSQLSyn, Font.Size, EditorChange);
  FExpressionTemplateEdit := CreateSynEditor(Self, ExpressionTemplateGroupBox,
    '_ExpressionTemplate', FSynSQLSyn, Font.Size, EditorChange);
end;

procedure TListControllerDynaListFilterNodeFrame.DesignPanelToEditNode;
begin
  inherited;
  ;
end;

class function TListControllerDynaListFilterNodeFrame.IsListControllerDynaListFilterNode(
  const ANode: TEFNode): Boolean;
var
  LClass: TClass;
begin
  Result := False;
  if (ANode.Parent is TEFNode) and
    TListControllerFiltersItemsNodeFrame.IsListControllerFiltersItemsNode(TEFNode(ANode.Parent)) then
  begin
    LClass := TKExtFilterRegistry.Instance.FindClass(ANode.Name);
    Result := Assigned(LClass) and LClass.InheritsFrom(TKDynaListFilter);
  end;
end;

class function TListControllerDynaListFilterNodeFrame.SuitsNode(const ANode: TEFNode): Boolean;
begin
  Result := IsListControllerDynaListFilterNode(ANode);
end;

procedure TListControllerDynaListFilterNodeFrame.UpdateDesignPanel(const AForce: Boolean);
begin
  inherited;
end;

initialization
  TEditNodeFrameRegistry.Instance.RegisterClass(TListControllerDynaListFilterNodeFrame.GetClassId, TListControllerDynaListFilterNodeFrame);

finalization
  if Assigned(TEditNodeFrameRegistry.Instance) then
    TEditNodeFrameRegistry.Instance.UnregisterClass(TListControllerDynaListFilterNodeFrame.GetClassId);

end.
