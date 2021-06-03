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
unit KIDE.MobileSettingsDesignerFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, KIDE.EditNodeBaseFrameUnit,
  System.Actions, Vcl.ActnList, Vcl.ExtCtrls, EF.Tree, Vcl.StdCtrls,
  Vcl.Samples.Spin;

type
  TMobileSettingsDesginerFrame = class(TEditNodeBaseFrame)
    ViewPortContentGroupBox: TGroupBox;
    WidthLabel: TLabel;
    _ViewportContent_Width: TSpinEdit;
    user_scalableCheckBox: TCheckBox;
  private
  protected
    class function SuitsNode(const ANode: TEFNode): Boolean; override;
    procedure UpdateDesignPanel(const AForce: Boolean = False); override;
    procedure CleanupDefaultsToEditNode; override;
    procedure DesignPanelToEditNode; override;
  public
  end;

implementation

{$R *.dfm}

uses
  Kitto.Metadata.Views;

{ TMobileSettingsDesginerFrame }

procedure TMobileSettingsDesginerFrame.CleanupDefaultsToEditNode;
begin
  inherited;
  CleanupTextNode('ViewportContent/user-scalable', '0');
  CleanupIntegerNode('ViewportContent/width');
  CleanupOrphanNode('ViewportContent');
end;

procedure TMobileSettingsDesginerFrame.DesignPanelToEditNode;
var
  LUserScalable: string;
begin
  inherited;
  if user_scalableCheckBox.Checked then
    LUserScalable := '1'
  else
    LUserScalable := '0';
  EditNode.SetString('ViewportContent/user-scalable', LUserScalable);
end;

class function TMobileSettingsDesginerFrame.SuitsNode(
  const ANode: TEFNode): Boolean;
begin
  Assert(Assigned(ANode));
  Result := SameText(ANode.Name, 'MobileSettings') and
    (ANode.Parent is TKView);
end;

procedure TMobileSettingsDesginerFrame.UpdateDesignPanel(const AForce: Boolean);
begin
  inherited;
  user_scalableCheckBox.Checked := EditNode.GetString('ViewportContent/user-scalable') = '1';
end;

initialization
  TEditNodeFrameRegistry.Instance.RegisterClass(TMobileSettingsDesginerFrame.GetClassId, TMobileSettingsDesginerFrame);

finalization
  if Assigned(TEditNodeFrameRegistry.Instance) then
    TEditNodeFrameRegistry.Instance.UnregisterClass(TMobileSettingsDesginerFrame.GetClassId);

end.
