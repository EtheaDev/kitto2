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
unit KIDE.ConfigAuthNodeFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.Actions,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Samples.Spin,
  Vcl.ExtCtrls, Vcl.Tabs, Vcl.ActnList, Vcl.ComCtrls, Vcl.ToolWin, Vcl.StdCtrls,
  SynEdit, SynHighlighterSQL, EF.Tree,
  KIDE.BaseFrameUnit, KIDE.CodeEditorFrameUnit, KIDE.EditNodeBaseFrameUnit;

type
  TConfigAuthNodeFrame = class(TEditNodeBaseFrame)
    AuthDBGroupBox: TGroupBox;
    AuthDBPanel: TPanel;
    _IsPassepartoutEnabled: TCheckBox;
    _PassepartoutPassword: TLabeledEdit;
    AuthTextFileGroupBox: TGroupBox;
    _FileName: TLabeledEdit;
    AuthScrollBox: TScrollBox;
    AuthAutoScrollPanel: TPanel;
    AuthPanel: TGroupBox;
    AuthComboBox: TComboBox;
    _IsClearPassword: TCheckBox;
    DefaultsGroupBox: TGroupBox;
    _Defaults_Password: TLabeledEdit;
    _Defaults_UserName: TLabeledEdit;
    AuthPageControl: TPageControl;
    ReadUserCommandTextTabSheet: TTabSheet;
    SetPasswordCommandTextTabSheet: TTabSheet;
    AfterAuthenticateCommandTextTabSheet: TTabSheet;
    ReadUserCommandTextPanel: TPanel;
    SetPasswordCommandTextPanel: TPanel;
    AfterAuthenticateCommandTextPanel: TPanel;
    procedure AuthComboBoxChange(Sender: TObject);
  private
    FSynSQLSyn: TSynSQLSyn;
    FReadUserCommandTextEdit: TSynEdit;
    FSetPasswordCommandTextEdit: TSynEdit;
    FAfterAuthenticateCommandTextEdit: TSynEdit;
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

procedure TConfigAuthNodeFrame.UpdateEditComponents;
begin
  inherited;
  AuthDBGroupBox.Visible := SameText(AuthComboBox.Text,'DB');
  DefaultsGroupBox.Visible := not SameText(AuthComboBox.Text,'DBServer');
  AuthTextFileGroupBox.Visible := SameText(AuthComboBox.Text,'TextFile');
end;

procedure TConfigAuthNodeFrame.AuthComboBoxChange(Sender: TObject);
begin
  inherited;
  if SameText(AuthComboBox.Text,'DB') then
  begin
    FReadUserCommandTextEdit.Text := EditNode.GetString('ReadUserCommandText',
      DEFAULT_READUSERCOMMANDTEXT);
    FSetPasswordCommandTextEdit.Text := EditNode.GetString('SetPasswordCommandText',
      DEFAULT_SETPASSWORDCOMMANDTEXT);
    FAfterAuthenticateCommandTextEdit.Text := EditNode.GetString('AfterAuthenticateCommandText');
  end
  else
  begin
    FReadUserCommandTextEdit.Text := '';
    FSetPasswordCommandTextEdit.Text := '';
    FAfterAuthenticateCommandTextEdit.Text := '';
  end;
  IsChanged := True;
end;

procedure TConfigAuthNodeFrame.CleanupDefaultsToEditNode;
begin
  inherited;
  CleanupBooleanNode('IsClearPassword');
  CleanupBooleanNode('IsPassepartoutEnabled');
  CleanupTextNode('Defaults/UserName');
  CleanupTextNode('Defaults/Password');
  CleanupOrphanNode('Defaults');
  CleanupTextNode('PassepartoutPassword');
  CleanupTextNode('ReadUserCommandText', DEFAULT_READUSERCOMMANDTEXT);
  CleanupTextNode('ReadUserCommandText');
  CleanupTextNode('SetPasswordCommandText', DEFAULT_SETPASSWORDCOMMANDTEXT);
  CleanupTextNode('SetPasswordCommandText');
  CleanupTextNode('AfterAuthenticateCommandText');
  CleanupTextNode('FileName', DEFAULT_USERLIST_FILENAME);
end;

constructor TConfigAuthNodeFrame.Create(AOwner: TComponent);
begin
  inherited;
  FSynSQLSyn := TSynSQLSyn.Create(Self);
  FReadUserCommandTextEdit := CreateSynEditor(Self, ReadUserCommandTextPanel,
    '_ReadUserCommandText', FSynSQLSyn, Font.Size, EditorChange);
  FReadUserCommandTextEdit.Gutter.Visible := False;

  FSetPasswordCommandTextEdit := CreateSynEditor(Self, SetPasswordCommandTextPanel,
    '_SetPasswordCommandText', FSynSQLSyn, Font.Size, EditorChange);
  FSetPasswordCommandTextEdit.Gutter.Visible := False;

  FAfterAuthenticateCommandTextEdit := CreateSynEditor(Self, AfterAuthenticateCommandTextPanel,
    '_AfterAuthenticateCommandText', FSynSQLSyn, Font.Size, EditorChange);
  FAfterAuthenticateCommandTextEdit.Gutter.Visible := False;

  TKideConfig.Instance.Config.GetNode('Authentication/Authenticators').GetChildValues(AuthComboBox.Items);
end;

procedure TConfigAuthNodeFrame.DesignPanelToEditNode;
begin
  inherited;
  AuthPageControl.ActivePageIndex := 0;
  if AuthComboBox.Text <> '' then
    TEFNode(EditNode).Parent.SetString('Auth', AuthComboBox.Text)
  else
    TEFNode(EditNode).Parent.SetString('Auth', NODE_NULL_VALUE);
end;

procedure TConfigAuthNodeFrame.Init(const ANode: TEFTree);
begin
  inherited;
end;

class function TConfigAuthNodeFrame.SuitsNode(const ANode: TEFNode): Boolean;
begin
  Result := Assigned(ANode.Parent) and
    (ANode.Parent is TEFComponentConfig) and SameText(ANode.Name, 'Auth');
end;

procedure TConfigAuthNodeFrame.UpdateDesignPanel(const AForce: Boolean);
var
  LAuthType: string;
begin
  inherited;
  AuthPageControl.ActivePageIndex := 0;
  _FileName.Text := EditNode.GetString('FileName', DEFAULT_USERLIST_FILENAME);

  //Align ComboBox item
  LAuthType := TEFNode(EditNode).Parent.GetString('Auth', NODE_NULL_VALUE);
  AuthComboBox.ItemIndex := AuthComboBox.Items.IndexOf(LAuthType);
  AuthComboBoxChange(AuthComboBox);
end;

initialization
  TEditNodeFrameRegistry.Instance.RegisterClass(TConfigAuthNodeFrame.GetClassId, TConfigAuthNodeFrame);

finalization
  if Assigned(TEditNodeFrameRegistry.Instance) then
    TEditNodeFrameRegistry.Instance.UnregisterClass(TConfigAuthNodeFrame.GetClassId);

end.
