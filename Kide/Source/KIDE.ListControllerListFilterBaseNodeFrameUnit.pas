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
unit KIDE.ListControllerListFilterBaseNodeFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, KIDE.EditNodeBaseFrameUnit, System.Actions, Vcl.ActnList,
  Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.Samples.Spin, Vcl.ComCtrls,
  EF.Tree,
  Kitto.Ext.Filters, Kitto.Ext.List, KIDE.PanelControllerDesignerFrameUnit;

type
  TListControllerListFilterBaseNodeFrame = class(TEditNodeBaseFrame)
    ListFilterGroupBox: TGroupBox;
    WidthLabel: TLabel;
    _Width: TSpinEdit;
    ListWidthLabel: TLabel;
    _ListWidth: TSpinEdit;
    AutoCompleteMinCharsLabel: TLabel;
    _AutoCompleteMinChars: TSpinEdit;
    LabelEdit: TLabeledEdit;
  private
  protected
    class function SuitsNode(const ANode: TEFNode): Boolean; override;
    procedure CleanupDefaultsToEditNode; override;
    procedure UpdateDesignPanel(const AForce: Boolean = False); override;
    procedure DesignPanelToEditNode; override;
  public
    class function IsListControllerListFilterBaseNode(const ANode: TEFNode): Boolean;
  end;

implementation

{$R *.dfm}

uses
  KIDE.Config,
  KIDE.ListControllerFiltersNodeFrameUnit,
  KIDE.ListControllerFiltersItemsNodeFrameUnit;

{ TListControllerFiltersNodeFrame }

procedure TListControllerListFilterBaseNodeFrame.CleanupDefaultsToEditNode;
begin
  inherited;
  CleanupIntegerNode('Width', DEFAULT_FILTER_WIDTH);
  CleanupIntegerNode('ListWidth', DEFAULT_FILTER_WIDTH);
  CleanupIntegerNode('AutoCompleteMinChars', 0);
end;

procedure TListControllerListFilterBaseNodeFrame.DesignPanelToEditNode;
begin
  inherited;
  TEFNode(EditNode).AsString := LabelEdit.Text;
end;

class function TListControllerListFilterBaseNodeFrame.IsListControllerListFilterBaseNode(const ANode: TEFNode): Boolean;
var
  LClass: TClass;
begin
  Result := False;
  if (ANode.Parent is TEFNode) and
    TListControllerFiltersItemsNodeFrame.IsListControllerFiltersItemsNode(TEFNode(ANode.Parent)) then
  begin
    LClass := TKExtFilterRegistry.Instance.FindClass(ANode.Name);
    Result := Assigned(LClass) and LClass.InheritsFrom(TKListFilterBase);
  end;
end;

class function TListControllerListFilterBaseNodeFrame.SuitsNode(const ANode: TEFNode): Boolean;
begin
  Result := IsListControllerListFilterBaseNode(ANode);
end;

procedure TListControllerListFilterBaseNodeFrame.UpdateDesignPanel(const AForce: Boolean);
begin
  inherited;
  _Width.Value := EditNode.GetInteger('Width', DEFAULT_FILTER_WIDTH);
  _ListWidth.Value := EditNode.GetInteger('ListWidth', DEFAULT_FILTER_WIDTH);
  _AutoCompleteMinChars.Value := EditNode.GetInteger('AutoCompleteMinChars', 0);
  LabelEdit.Text := TEFNode(EditNode).AsString;
end;

initialization
  TEditNodeFrameRegistry.Instance.RegisterClass(TListControllerListFilterBaseNodeFrame.GetClassId, TListControllerListFilterBaseNodeFrame);

finalization
  if Assigned(TEditNodeFrameRegistry.Instance) then
    TEditNodeFrameRegistry.Instance.UnregisterClass(TListControllerListFilterBaseNodeFrame.GetClassId);

end.
