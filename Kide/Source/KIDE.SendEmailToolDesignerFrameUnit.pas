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
unit KIDE.SendEmailToolDesignerFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, KIDE.EditNodeBaseFrameUnit,
  KIDE.BaseFrameUnit, KIDE.CodeEditorFrameUnit, Vcl.ExtCtrls, Vcl.Tabs,
  System.Actions, Vcl.ActnList, Vcl.ComCtrls, Vcl.ToolWin, Vcl.StdCtrls,
  EF.Tree,
  KIDE.DataToolDesignerFrameUnit,
  Kitto.Ext.IndyTools, Kitto.Ext.Tools;

type
  TSendEmailToolDesignerFrame = class(TDataToolDesignerFrame)
    SendEmailToolGroupBox: TGroupBox;
    ServerPageControl: TPageControl;
    MessageTabSheet: TTabSheet;
    SMTPTabSheet: TTabSheet;
    SMTPPanel: TPanel;
    _SMTP: TComboBox;
    SMTPLabel: TLabel;
    SMTPHostGroupBox: TGroupBox;
    procedure _SMTPChange(Sender: TObject);
  private
    function GetConfigSMTPNode: TEFNode;
  protected
    procedure CleanupDefaultsToEditNode; override;
    procedure UpdateDesignPanel(const AForce: Boolean); override;
    procedure DesignPanelToEditNode; override;
  public
    class function SuitsNode(const ANode: TEFNode): Boolean; override;
    procedure Init(const ANode: TEFTree); override;
  end;

implementation

{$R *.dfm}

uses
  KIDE.Project,
  Kitto.Config, Kitto.Metadata.Views,
  KIDE.SendEmailMessageNodeFrameUnit,
  KIDE.ConfigEmailSMTPServerNodeFrameUnit;

{ TDownloadFileToolDesignerFrame }

procedure TSendEmailToolDesignerFrame.CleanupDefaultsToEditNode;
begin
  inherited;
  CleanupTextNode('SMTP', 'Default');
end;

procedure TSendEmailToolDesignerFrame.DesignPanelToEditNode;
begin
  inherited;
  ServerPageControl.ActivePageIndex := 0;
end;

function TSendEmailToolDesignerFrame.GetConfigSMTPNode: TEFNode;
begin
  inherited;
  Result := TProject.CurrentProject.Config.Config.FindNode('Email/SMTP');
end;

procedure TSendEmailToolDesignerFrame.Init(const ANode: TEFTree);
begin
  inherited;
  EmbedEditNodeFrame(MessageTabSheet, TSendEmailMessageNodeFrame,
    EditNode.GetNode('Message', True));
end;

procedure TSendEmailToolDesignerFrame.UpdateDesignPanel(const AForce: Boolean);
var
  LSMTPNode: TEFNode;
begin
  inherited;
  ServerPageControl.ActivePageIndex := 0;
  LSMTPNode := GetConfigSMTPNode;
  if Assigned(LSMTPNode) then
    LSMTPNode.GetChildNames(_SMTP.Items);
  _SMTP.Text := EditNode.GetString('SMTP', 'Default');
  _SMTPChange(_SMTP);
end;

procedure TSendEmailToolDesignerFrame._SMTPChange(Sender: TObject);
var
  Frame: TConfigEmailSMTPServerNodeFrame;
  LConfigNode: TEFNode;
  LSMTPNode: TEFNode;
begin
  inherited;
  if _SMTP.Text <> '' then
  begin
    LSMTPNode := GetConfigSMTPNode;
    if Assigned(LSMTPNode) then
    begin
      LConfigNode := LSMTPNode.FindNode(_SMTP.Text);
      Frame := EmbedEditNodeFrame(SMTPHostGroupBox, TConfigEmailSMTPServerNodeFrame, LConfigNode)
        as TConfigEmailSMTPServerNodeFrame;
      if Assigned(Frame) then
      begin
        Frame.UpdateDesignPanel;
        Frame.Enabled := False;
      end;
    end;
  end;
  IsChanged := True;
end;

class function TSendEmailToolDesignerFrame.SuitsNode(
  const ANode: TEFNode): Boolean;
var
  LControllerClass: TClass;
begin
  Assert(Assigned(ANode));
  LControllerClass := GetControllerClass(ANode);
  Result := Assigned(LControllerClass) and
    LControllerClass.InheritsFrom(TSendEmailToolController);
end;

initialization
  TEditNodeFrameRegistry.Instance.RegisterClass(TSendEmailToolDesignerFrame.GetClassId, TSendEmailToolDesignerFrame);

finalization
  if Assigned(TEditNodeFrameRegistry.Instance) then
    TEditNodeFrameRegistry.Instance.UnregisterClass(TSendEmailToolDesignerFrame.GetClassId);

end.
