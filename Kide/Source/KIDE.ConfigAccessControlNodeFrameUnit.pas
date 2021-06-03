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
unit KIDE.ConfigAccessControlNodeFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, KIDE.EditNodeBaseFrameUnit, System.Actions, Vcl.ActnList,
  Vcl.ExtCtrls, Vcl.StdCtrls, KIDE.BaseFrameUnit, KIDE.PairsValuesFrameUnit,
  EF.Tree, Vcl.ComCtrls, SynEdit, SynHighlighterHtml, SynHighlighterSQL;

type
  TConfigAccessControlNodeFrame = class(TEditNodeBaseFrame)
    ACPageControl: TPageControl;
    ReadPermissionsCommandTextTabSheet: TTabSheet;
    ReadPermissionsCommandTextPanel: TPanel;
    ReadRolesCommandTextTabSheet: TTabSheet;
    ReadRolesCommandTextPanel: TPanel;
    ACGroupBox: TGroupBox;
    ACComboBox: TComboBox;
    procedure ACComboBoxChange(Sender: TObject);
  private
    FReadPermissionsCommandTextEdit: TSynEdit;
    FReadRolesCommandTextEdit: TSynEdit;
    FSynSQLSyn: TSynSQLSyn;
  protected
    procedure CleanupDefaultsToEditNode; override;
    procedure UpdateDesignPanel(const AForce: Boolean); override;
    procedure DesignPanelToEditNode; override;
    class function SuitsNode(const ANode: TEFNode): Boolean; override;
  public
    procedure Init(const ANode: TEFTree); override;
    constructor Create(AOwner: TComponent); override;
  end;

implementation

{$R *.dfm}

uses
  KIDE.Config,
  EF.Classes, KIDE.Utils, Kitto.AccessControl.DB;

{ TConfigDatabasesNodeFrame }

procedure TConfigAccessControlNodeFrame.ACComboBoxChange(Sender: TObject);
begin
  inherited;
  if SameText(ACComboBox.Text,'DB') then
  begin
    FReadPermissionsCommandTextEdit.Text := EditNode.GetString('ReadPermissionsCommandText',
      DEFAULT_READPERMISSIONCOMMANDTEXT);
    FReadRolesCommandTextEdit.Text := EditNode.GetString('ReadRolesCommandText',
      DEFAULT_READROLESCOMMANDTEXT);
  end
  else
  begin
    FReadPermissionsCommandTextEdit.Text := '';
    FReadRolesCommandTextEdit.Text := '';
  end;
  IsChanged := True;
end;

procedure TConfigAccessControlNodeFrame.CleanupDefaultsToEditNode;
begin
  inherited;
  CleanupTextNode('ReadPermissionsCommandText', DEFAULT_READPERMISSIONCOMMANDTEXT);
  CleanupTextNode('ReadPermissionsCommandText', '');
  CleanupTextNode('ReadRolesCommandText', DEFAULT_READROLESCOMMANDTEXT);
  CleanupTextNode('ReadRolesCommandText', '');
end;

constructor TConfigAccessControlNodeFrame.Create(AOwner: TComponent);
begin
  inherited;
  FSynSQLSyn := TSynSQLSyn.Create(Self);

  FReadPermissionsCommandTextEdit  := CreateSynEditor(Self, ReadPermissionsCommandTextPanel,
    '_ReadPermissionsCommandText', FSynSQLSyn, Font.Size, EditorChange);
  FReadPermissionsCommandTextEdit.Gutter.Visible := False;

  FReadRolesCommandTextEdit := CreateSynEditor(Self, ReadRolesCommandTextPanel,
    '_ReadRolesCommandText', FSynSQLSyn, Font.Size, EditorChange);
  FReadRolesCommandTextEdit.Gutter.Visible := False;

  TKideConfig.Instance.Config.GetNode('AccessControl/AccessControllers').GetChildValues(ACComboBox.Items);
end;

procedure TConfigAccessControlNodeFrame.DesignPanelToEditNode;
begin
  inherited;
  ACPageControl.ActivePageIndex := 0;

  if ACComboBox.Text <> '' then
    TEFNode(EditNode).Value := ACComboBox.Text
  else
    TEFNode(EditNode).Value := NODE_NULL_VALUE;
end;

procedure TConfigAccessControlNodeFrame.Init(const ANode: TEFTree);
begin
  inherited;
end;

class function TConfigAccessControlNodeFrame.SuitsNode(const ANode: TEFNode): Boolean;
begin
  Result := (ANode.Parent is TEFComponentConfig) and SameText(ANode.Name, 'AccessControl');
end;

procedure TConfigAccessControlNodeFrame.UpdateDesignPanel(const AForce: Boolean);
var
  LACType: string;
begin
  inherited;
  ACPageControl.ActivePageIndex := 0;
  LACType := TEFNode(EditNode).AsString;
  ACComboBox.ItemIndex := ACComboBox.Items.IndexOf(LACType);
  ACComboBoxChange(ACComboBox);
end;

initialization
  TEditNodeFrameRegistry.Instance.RegisterClass(TConfigAccessControlNodeFrame.GetClassId, TConfigAccessControlNodeFrame);

finalization
  if Assigned(TEditNodeFrameRegistry.Instance) then
    TEditNodeFrameRegistry.Instance.UnregisterClass(TConfigAccessControlNodeFrame.GetClassId);

end.
