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
unit KIDE.FormControllerButtonDesignerFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, KIDE.EditNodeBaseFrameUnit,
  Vcl.ExtCtrls, Vcl.Tabs,
  System.Actions, Vcl.ActnList, Vcl.ComCtrls, Vcl.ToolWin, Vcl.StdCtrls, Vcl.Buttons,
  KIDE.BaseFrameUnit, KIDE.CodeEditorFrameUnit,
  EF.Tree, Vcl.Samples.Spin, Vcl.StdActns,
  KIDE.MainDataModuleUnit;

type
  TFormControllerButtonDesignerFrame = class(TEditNodeBaseFrame)
    _Caption: TLabeledEdit;
    _Tooltip: TLabeledEdit;
  private
    function IsCloneButton: boolean;
  protected
    class function SuitsNode(const ANode: TEFNode): Boolean; override;
    procedure CleanupDefaultsToEditNode; override;
    procedure DesignPanelToEditNode; override;
  public
    procedure UpdateDesignPanel(const AForce: Boolean = False); override;
    constructor Create(AOwner: TComponent); override;
  end;

implementation

{$R *.dfm}

uses
  StrUtils,
  EF.Macros,
  Kitto.Ext.Base,
  Kitto.Ext.Form,
  KIDE.Project, KIDE.Config, KIDE.ViewTableFormControllerDesignerFrameUnit;

{ TViewTableFormControllerDesignerFrame }

procedure TFormControllerButtonDesignerFrame.CleanupDefaultsToEditNode;
begin
  inherited;
  CleanupTextNode('Caption');
  CleanupTextNode('ToolTip');
end;

constructor TFormControllerButtonDesignerFrame.Create(AOwner: TComponent);
begin
  inherited;
end;

procedure TFormControllerButtonDesignerFrame.DesignPanelToEditNode;
begin
  inherited;
  ;
end;function TFormControllerButtonDesignerFrame.IsCloneButton: boolean;
begin
  Result := SameText(TEFNode(EditNode).Name, 'CloneButton');
end;

class function TFormControllerButtonDesignerFrame.SuitsNode(
  const ANode: TEFNode): Boolean;
begin
  Assert(Assigned(ANode));
  Result := MatchText(ANode.Name, ['CloneButton', 'ConfirmButton', 'CancelButton', 'CloseButton'])
    and (ANode.Parent is TEFNode) and
    TViewTableFormControllerDesignerFrame.IsViewTableFormControllerNode(TEFNode(ANode.Parent));
end;

procedure TFormControllerButtonDesignerFrame.UpdateDesignPanel(
  const AForce: Boolean);
begin
  inherited;
  Assert(Assigned(EditNode));
end;

initialization
  TEditNodeFrameRegistry.Instance.RegisterClass(TFormControllerButtonDesignerFrame.GetClassId, TFormControllerButtonDesignerFrame);

finalization
  if Assigned(TEditNodeFrameRegistry.Instance) then
    TEditNodeFrameRegistry.Instance.UnregisterClass(TFormControllerButtonDesignerFrame.GetClassId);

end.
