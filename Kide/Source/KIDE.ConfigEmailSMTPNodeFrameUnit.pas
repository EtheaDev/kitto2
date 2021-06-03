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
unit KIDE.ConfigEmailSMTPNodeFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, KIDE.EditNodeBaseFrameUnit, System.Actions, Vcl.ActnList,
  Vcl.ExtCtrls, EF.Tree, Vcl.StdCtrls, Vcl.Samples.Spin, Vcl.ComCtrls,
  KIDE.ConfigEmailSMTPServerNodeFrameUnit;

type
  TConfigEmailSMTPNodeFrame = class(TEditNodeBaseFrame)
    SMTPTabControl: TTabControl;
    procedure SMTPTabControlChange(Sender: TObject);
  private
    SMTPServerNodeFrame: TConfigEmailSMTPServerNodeFrame;
  protected
    class function SuitsNode(const ANode: TEFNode): Boolean; override;
    procedure CleanupDefaultsToEditNode; override;
    procedure UpdateDesignPanel(const AForce: Boolean = False); override;
    procedure DesignPanelToEditNode; override;
  public
    procedure Init(const ANode: TEFTree); override;
  end;

implementation

{$R *.dfm}

uses
  EF.Classes,
  KIDE.Project, Kitto.Ext.IndyTools;

{ TConfigSMTPServerNodeFrame }

procedure TConfigEmailSMTPNodeFrame.CleanupDefaultsToEditNode;
begin
  inherited;
  CleanupOrphanNode('SMTP/Default');
  CleanupOrphanNode('SMTP');
end;

procedure TConfigEmailSMTPNodeFrame.DesignPanelToEditNode;
begin
  inherited;
end;

procedure TConfigEmailSMTPNodeFrame.Init(const ANode: TEFTree);
var
  LDefaultNode: TEFNode;
begin
  inherited;
  LDefaultNode := EditNode.FindNode('Default', True);
  SMTPServerNodeFrame := EmbedEditNodeFrame(SMTPTabControl, TConfigEmailSMTPServerNodeFrame,
    LDefaultNode) as TConfigEmailSMTPServerNodeFrame;
end;

procedure TConfigEmailSMTPNodeFrame.SMTPTabControlChange(Sender: TObject);
var
  LNodeName: string;
  LNode: TEFNode;
begin
  inherited;
  LNodeName := SMTPTabControl.Tabs[SMTPTabControl.TabIndex];
  LNode := EditNode.FindNode(LNodeName);
  SMTPServerNodeFrame.Init(LNode);
  SMTPServerNodeFrame.UpdateDesignPanel(False);
end;

class function TConfigEmailSMTPNodeFrame.SuitsNode(const ANode: TEFNode): Boolean;
begin
  Result := SameText(ANode.Name, 'SMTP') and
    (ANode.Parent is TEFNode) and
    SameText(TEFNode(ANode.Parent).Name, 'Email') and
    Assigned(TEFNode(ANode.Parent).Parent) and
    (TEFNode(ANode.Parent).Parent is TEFComponentConfig);
end;

procedure TConfigEmailSMTPNodeFrame.UpdateDesignPanel(const AForce: Boolean);
var
  I: Integer;
begin
  SMTPTabControl.Tabs.Clear;
  for I := 0 to EditNode.ChildCount -1 do
    SMTPTabControl.Tabs.Add(EditNode.Children[I].Name);
  inherited;
end;

initialization
  TEditNodeFrameRegistry.Instance.RegisterClass(TConfigEmailSMTPNodeFrame.GetClassId, TConfigEmailSMTPNodeFrame);

finalization
  if Assigned(TEditNodeFrameRegistry.Instance) then
    TEditNodeFrameRegistry.Instance.UnregisterClass(TConfigEmailSMTPNodeFrame.GetClassId);

end.
