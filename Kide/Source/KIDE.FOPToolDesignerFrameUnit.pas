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
unit KIDE.FOPToolDesignerFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, KIDE.EditNodeBaseFrameUnit,
  KIDE.BaseFrameUnit, KIDE.CodeEditorFrameUnit, Vcl.ExtCtrls, Vcl.Tabs,
  System.Actions, Vcl.ActnList, Vcl.ComCtrls, Vcl.ToolWin, Vcl.StdCtrls,
  EF.Tree,
  KIDE.DownloadFileToolDesignerFrameUnit,
  Kitto.Ext.FOPTools;

type
  TFOPToolDesignerFrame = class(TDownloadFileToolDesignerFrame)
    FOPToolGroupBox: TGroupBox;
    _TransformFileName: TLabeledEdit;
  private
  protected
    procedure UpdateDesignPanel(const AForce: Boolean = False); override;
    class function SuitsNode(const ANode: TEFNode): Boolean; override;
  public
  end;

implementation

{$R *.dfm}

uses
  Kitto.Metadata.Views;

{ TFOPToolDesignerFrame }

class function TFOPToolDesignerFrame.SuitsNode(
  const ANode: TEFNode): Boolean;
var
  LControllerClass: TClass;
begin
  Assert(Assigned(ANode));
  LControllerClass := GetControllerClass(ANode);
  Result := Assigned(LControllerClass) and
    LControllerClass.InheritsFrom(TFOPToolController);
end;

procedure TFOPToolDesignerFrame.UpdateDesignPanel(
  const AForce: Boolean);
begin
  inherited;
  HideFileNameEdit;
end;

initialization
  TEditNodeFrameRegistry.Instance.RegisterClass(TFOPToolDesignerFrame.GetClassId, TFOPToolDesignerFrame);

finalization
  if Assigned(TEditNodeFrameRegistry.Instance) then
    TEditNodeFrameRegistry.Instance.UnregisterClass(TFOPToolDesignerFrame.GetClassId);

end.
