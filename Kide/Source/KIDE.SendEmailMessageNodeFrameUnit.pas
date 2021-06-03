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
unit KIDE.SendEmailMessageNodeFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, KIDE.EditNodeBaseFrameUnit, System.Actions, Vcl.ActnList,
  Vcl.ExtCtrls,
  EF.Tree, Vcl.StdCtrls, Vcl.ComCtrls;

type
  TSendEmailMessageNodeFrame = class(TEditNodeBaseFrame)
    EmailPageControl: TPageControl;
    MessageTabSheet: TTabSheet;
    AttachmentsTabSheet: TTabSheet;
    _From: TLabeledEdit;
    _Subject: TLabeledEdit;
    BodyLabel: TLabel;
    _Body: TMemo;
    DestinationTabSheet: TTabSheet;
    PageControl2: TPageControl;
    ToTabSheet: TTabSheet;
    CCTabSheet: TTabSheet;
    BCCTabSheet: TTabSheet;
  private
    { Private declarations }
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
  Kitto.Ext.IndyTools,
  KIDE.PairsValuesFrameUnit,
  KIDE.SendEmailToolDesignerFrameUnit,
  KIDE.SendEmailDestinationNodeFrameUnit;

{ TSendEmailMessageNodeFrame }

procedure TSendEmailMessageNodeFrame.CleanupDefaultsToEditNode;
begin
  inherited;
  CleanupOrphanNode('To');
  CleanupOrphanNode('CC');
  CleanupOrphanNode('BCC');
  CleanupOrphanNode('Attachments');
end;

procedure TSendEmailMessageNodeFrame.DesignPanelToEditNode;
begin
  inherited;
  EmailPageControl.ActivePageIndex := 0;
end;

procedure TSendEmailMessageNodeFrame.Init(const ANode: TEFTree);
begin
  inherited;
  EmbedEditNodeFrame(ToTabSheet, TSendEmailDestinationNodeFrame,
    EditNode.GetNode('To', True));
  EmbedEditNodeFrame(CCTabSheet, TSendEmailDestinationNodeFrame,
    EditNode.GetNode('CC', True));
  EmbedEditNodeFrame(BCCTabSheet, TSendEmailDestinationNodeFrame,
    EditNode.GetNode('BCC', True));
  EmbedEditNodeFrame(AttachmentsTabSheet, TPairsValuesFrame,
    EditNode.GetNode('Attachments', True));
end;

class function TSendEmailMessageNodeFrame.SuitsNode(const ANode: TEFNode): Boolean;
var
  LNode: TEFNode;
  LControllerClass: TClass;
begin
  Result := False;
  if (ANode is TEFNode) then
  begin
    LNode := TEFNode(ANode);
    if SameText(LNode.Name, 'Message') and Assigned(LNode.Parent) and
      (LNode.Parent is TEFNode) and SameText(TEFNode(LNode.Parent).Name, 'Controller') then
    begin
      LControllerClass := GetControllerClass(TEFNode(LNode.Parent));
      Result := Assigned(LControllerClass) and LControllerClass.InheritsFrom(TSendEmailToolController);
    end;
  end;
end;

procedure TSendEmailMessageNodeFrame.UpdateDesignPanel(const AForce: Boolean);
begin
  inherited;
  EmailPageControl.ActivePageIndex := 0;
end;

initialization
  TEditNodeFrameRegistry.Instance.RegisterClass(TSendEmailMessageNodeFrame.GetClassId, TSendEmailMessageNodeFrame);

finalization
  if Assigned(TEditNodeFrameRegistry.Instance) then
    TEditNodeFrameRegistry.Instance.UnregisterClass(TSendEmailMessageNodeFrame.GetClassId);

end.
