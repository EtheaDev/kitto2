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
unit KIDE.GridPanelControllerDesignerFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, KIDE.EditNodeBaseFrameUnit,
  Vcl.ExtCtrls, Vcl.Tabs, Vcl.Samples.Spin,
  System.Actions, Vcl.ActnList, Vcl.ComCtrls, Vcl.ToolWin, Vcl.StdCtrls, Vcl.Buttons,
  KIDE.BaseFrameUnit, KIDE.CodeEditorFrameUnit, KIDE.PanelControllerDesignerFrameUnit,
  EF.Tree, KIDE.ControllerDesignerFrameUnit, KIDE.DataPanelControllerDesignerFrameUnit;

type
  TGridPanelControllerDesignerFrame = class(TDataPanelControllerDesignerFrame)
    tsGrouping: TTabSheet;
    PopUpWindowTabSheet: TTabSheet;
    _Grouping_SortFieldNames: TLabeledEdit;
    _Grouping_FieldName: TLabeledEdit;
    _Grouping_EnableMenu: TCheckBox;
    _Grouping_StartCollapsed: TCheckBox;
    _Grouping_ShowName: TCheckBox;
    ShowCountGroupBox: TGroupBox;
    _Grouping_ShowCount_Template: TLabeledEdit;
    _Grouping_ShowCount_PluralItemName: TLabeledEdit;
    _Grouping_ShowCount_ItemName: TLabeledEdit;
    _Grouping_ShowCount: TCheckBox;
    _PopUpWindow_Width: TSpinEdit;
    _PopUpWindow_Height: TSpinEdit;
    Label1: TLabel;
    Label2: TLabel;
    procedure _Grouping_ShowCountClick(Sender: TObject);
  strict private
  protected
    class function SuitsNode(const ANode: TEFNode): Boolean; override;
    procedure UpdateDesignPanel(const AForce: Boolean = False); override;
    procedure CleanupDefaultsToEditNode; override;
  protected
  public
  end;

implementation

{$R *.dfm}

uses
  EF.Macros,
  Kitto.Ext.Panel, Kitto.Ext.Base,
  Kitto.Ext.GridPanel;

{ TDownloadFileToolDesignerFrame }

procedure TGridPanelControllerDesignerFrame.CleanupDefaultsToEditNode;
begin
  inherited;
  //Cleaning Grouping Node
  CleanupTextNode('Grouping/SortFieldNames');
  CleanupTextNode('Grouping/FieldName');
  CleanupBooleanNode('Grouping/EnableMenu');
  CleanupBooleanNode('Grouping/StartCollapsed');
  CleanupBooleanNode('Grouping/ShowName');
  CleanupBooleanNode('Grouping/ShowCount');
  if not EditNode.GetBoolean('Grouping/ShowCount') then
  begin
    CleanupBooleanNode('Grouping/Template');
    CleanupBooleanNode('Grouping/PluralItemName');
    CleanupBooleanNode('Grouping/ItemName');
  end;
  CleanupOrphanNode('Grouping');

  //Cleaning PopupWindow Node
  CleanupIntegerNode('PopupWindow/Width', DEFAULT_WINDOW_WIDTH);
  CleanupIntegerNode('PopupWindow/Height', DEFAULT_WINDOW_HEIGHT);
  CleanupOrphanNode('PopupWindow');
end;

class function TGridPanelControllerDesignerFrame.SuitsNode(
  const ANode: TEFNode): Boolean;
var
  LControllerValue: string;
  LControllerClass: TClass;
begin
  Assert(Assigned(ANode));
  LControllerValue := ANode.AsString;
  LControllerClass := GetControllerClass(ANode);
  Result := (Assigned(LControllerClass) and LControllerClass.InheritsFrom(TKExtGridPanel));
(*
  //A Controller without specific class is a GridPanel Controller by default
  Result := (SameText(ANode.Name, 'Controller') and (LControllerValue='')) or
    (Assigned(LControllerClass) and LControllerClass.InheritsFrom(TKExtGridPanel));
*)
end;

procedure TGridPanelControllerDesignerFrame.UpdateDesignPanel(
  const AForce: Boolean);
begin
  inherited;
  _PopUpWindow_Width.Value := EditNode.GetInteger('PopUpWindow/Width', DEFAULT_WINDOW_WIDTH);
  _PopUpWindow_Height.Value := EditNode.GetInteger('PopUpWindow/Height', DEFAULT_WINDOW_HEIGHT);
end;

procedure TGridPanelControllerDesignerFrame._Grouping_ShowCountClick(
  Sender: TObject);
begin
  inherited;
  ShowCountGroupBox.Enabled := _Grouping_ShowCount.Checked;
end;

initialization
  TEditNodeFrameRegistry.Instance.RegisterClass(TGridPanelControllerDesignerFrame.GetClassId, TGridPanelControllerDesignerFrame);

finalization
  if Assigned(TEditNodeFrameRegistry.Instance) then
    TEditNodeFrameRegistry.Instance.UnregisterClass(TGridPanelControllerDesignerFrame.GetClassId);

end.
