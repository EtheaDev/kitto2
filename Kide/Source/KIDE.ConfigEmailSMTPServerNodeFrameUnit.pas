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
unit KIDE.ConfigEmailSMTPServerNodeFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, KIDE.EditNodeBaseFrameUnit, System.Actions, Vcl.ActnList,
  Vcl.ExtCtrls, EF.Tree, Vcl.StdCtrls, Vcl.Samples.Spin;

type
  TConfigEmailSMTPServerNodeFrame = class(TEditNodeBaseFrame)
    _HostName: TLabeledEdit;
    _UserName: TLabeledEdit;
    _Password: TLabeledEdit;
    _Port: TSpinEdit;
    PortLabel: TLabel;
    _UseTLS: TCheckBox;
  private
    { Private declarations }
  protected
    class function SuitsNode(const ANode: TEFNode): Boolean; override;
    procedure CleanupDefaultsToEditNode; override;
    procedure DesignPanelToEditNode; override;
  public
    procedure UpdateDesignPanel(const AForce: Boolean = False); override;
  end;

implementation

{$R *.dfm}

uses
  Kitto.Ext.IndyTools;

{ TConfigSMTPServerNodeFrame }

procedure TConfigEmailSMTPServerNodeFrame.CleanupDefaultsToEditNode;
begin
  inherited;
  CleanupTextNode('HostName');
  CleanupTextNode('UserName');
  CleanupTextNode('Password');
  CleanupIntegerNode('Port');
  CleanupBooleanNode('UseTLS', False);
end;

procedure TConfigEmailSMTPServerNodeFrame.DesignPanelToEditNode;
begin
  inherited;
  ;
end;

class function TConfigEmailSMTPServerNodeFrame.SuitsNode(const ANode: TEFNode): Boolean;
begin
  Result := (ANode is TEFNode) and (TEFNode(ANode).Parent is TEFNode) and
    SameText(TEFNode(TEFNode(ANode).Parent).Name, 'SMTP');
end;

procedure TConfigEmailSMTPServerNodeFrame.UpdateDesignPanel(const AForce: Boolean);
begin
  _HostName.Text := '';
  _Port.Value := 0;
  _UseTLS.Checked := False;
  _UserName.Text := '';
  _Password.Text := '';
  inherited;
  _UseTLS.Checked := EditNode.GetBoolean('UseTLS');
end;

initialization
  TEditNodeFrameRegistry.Instance.RegisterClass(TConfigEmailSMTPServerNodeFrame.GetClassId, TConfigEmailSMTPServerNodeFrame);

finalization
  if Assigned(TEditNodeFrameRegistry.Instance) then
    TEditNodeFrameRegistry.Instance.UnregisterClass(TConfigEmailSMTPServerNodeFrame.GetClassId);

end.
