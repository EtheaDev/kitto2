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
unit KIDE.ConfigEmailNodeFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, KIDE.EditNodeBaseFrameUnit, System.Actions, Vcl.ActnList,
  Vcl.ExtCtrls, EF.Tree, Vcl.StdCtrls, Vcl.Samples.Spin, Vcl.ComCtrls,
  KIDE.ConfigEmailSMTPServerNodeFrameUnit;

type
  TConfigEmailNodeFrame = class(TEditNodeBaseFrame)
    SMTPGroupBox: TGroupBox;
  private
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
  KIDE.Project, Kitto.Ext.IndyTools,
  KIDE.ConfigEmailSMTPNodeFrameUnit;

{ TConfigEmailNodeFrame }

procedure TConfigEmailNodeFrame.CleanupDefaultsToEditNode;
begin
  inherited;
  CleanupOrphanNode('Default');
end;

procedure TConfigEmailNodeFrame.DesignPanelToEditNode;
begin
  inherited;
end;

procedure TConfigEmailNodeFrame.Init(const ANode: TEFTree);
begin
  inherited;
  EmbedEditNodeFrame(SMTPGroupBox, TConfigEmailSMTPNodeFrame,
    EditNode.FindNode('SMTP', True));
end;

class function TConfigEmailNodeFrame.SuitsNode(const ANode: TEFNode): Boolean;
begin
  Result := Assigned(ANode.Parent) and
    (ANode.Parent is TEFComponentConfig) and SameText(ANode.Name, 'Email');
end;

procedure TConfigEmailNodeFrame.UpdateDesignPanel(const AForce: Boolean);
begin
  inherited;
end;

initialization
  TEditNodeFrameRegistry.Instance.RegisterClass(TConfigEmailNodeFrame.GetClassId, TConfigEmailNodeFrame);

finalization
  if Assigned(TEditNodeFrameRegistry.Instance) then
    TEditNodeFrameRegistry.Instance.UnregisterClass(TConfigEmailNodeFrame.GetClassId);

end.
