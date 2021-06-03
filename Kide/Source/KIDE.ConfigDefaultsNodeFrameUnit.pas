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
unit KIDE.ConfigDefaultsNodeFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.Actions,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Samples.Spin,
  Vcl.ExtCtrls, Vcl.Tabs, Vcl.ActnList, Vcl.ComCtrls, Vcl.ToolWin, Vcl.StdCtrls,
  EF.Tree,
  KIDE.BaseFrameUnit, KIDE.CodeEditorFrameUnit, KIDE.EditNodeBaseFrameUnit;

type
  TConfigDefaultsNodeFrame = class(TEditNodeBaseFrame)
    AuthScrollBox: TScrollBox;
    AuthAutoScrollPanel: TPanel;
    GridPanel: TGroupBox;
    WindowGroupBox: TGroupBox;
    _Grid_PageRecordCount: TSpinEdit;
    _Window_Height: TSpinEdit;
    _Window_Width: TSpinEdit;
    PageRecordCountLabel: TLabel;
    DefaultActionComboBox: TComboBox;
    ThemeLabel: TLabel;
    WidthLabel: TLabel;
    HeightLabel: TLabel;
    procedure DefaultActionComboBoxChange(Sender: TObject);
  private
  protected
    class function SuitsNode(const ANode: TEFNode): Boolean; override;
    procedure CleanupDefaultsToEditNode; override;
    procedure UpdateDesignPanel(const AForce: Boolean = False); override;
    procedure DesignPanelToEditNode; override;
    procedure UpdateEditComponents; override;
  public
    procedure Init(const ANode: TEFTree); override;
    constructor Create(AOwner: TComponent); override;
  end;

implementation

{$R *.dfm}

uses
  KIDE.Config,
  EF.Classes, KIDE.Utils,
  Kitto.Config,
  Kitto.Auth, Kitto.Auth.DB, Kitto.Auth.DBServer, Kitto.Auth.OSDB, Kitto.Auth.TextFile,
  Kitto.AccessControl.DB;

{ TConfigDesignerFrame }

procedure TConfigDefaultsNodeFrame.UpdateEditComponents;
begin
  inherited;
end;

procedure TConfigDefaultsNodeFrame.CleanupDefaultsToEditNode;
begin
  inherited;
  CleanupIntegerNode('Grid/PageRecordCount');
  CleanupTextNode('Grid/DefaultAction');
  CleanupIntegerNode('Window/Width');
  CleanupIntegerNode('Window/Height');
  CleanupOrphanNode('Grid');
  CleanupOrphanNode('Window');
end;

constructor TConfigDefaultsNodeFrame.Create(AOwner: TComponent);
begin
  inherited;
  TKideConfig.Instance.Config.GetNode('Defaults/Grid/DefaultActions').GetChildValues(DefaultActionComboBox.Items);
end;

procedure TConfigDefaultsNodeFrame.DefaultActionComboBoxChange(Sender: TObject);
begin
  inherited;
  IsChanged := True;
end;

procedure TConfigDefaultsNodeFrame.DesignPanelToEditNode;
begin
  inherited;
  EditNode.SetString('Grid/DefaultAction' ,DefaultActionComboBox.Text);
end;

procedure TConfigDefaultsNodeFrame.Init(const ANode: TEFTree);
begin
  inherited;
end;

class function TConfigDefaultsNodeFrame.SuitsNode(const ANode: TEFNode): Boolean;
begin
  Result := Assigned(ANode.Parent) and
    (ANode.Parent is TEFComponentConfig) and SameText(ANode.Name, 'Defaults');
end;

procedure TConfigDefaultsNodeFrame.UpdateDesignPanel(const AForce: Boolean);
var
  LDefaultAction: string;
begin
  inherited;
  //Align ComboBox item
  LDefaultAction := TEFNode(EditNode).GetString('Grid/DefaultAction', NODE_NULL_VALUE);
  DefaultActionComboBox.ItemIndex := DefaultActionComboBox.Items.IndexOf(LDefaultAction);
  DefaultActionComboBoxChange(DefaultActionComboBox);
end;

initialization
  TEditNodeFrameRegistry.Instance.RegisterClass(TConfigDefaultsNodeFrame.GetClassId, TConfigDefaultsNodeFrame);

finalization
  if Assigned(TEditNodeFrameRegistry.Instance) then
    TEditNodeFrameRegistry.Instance.UnregisterClass(TConfigDefaultsNodeFrame.GetClassId);

end.
