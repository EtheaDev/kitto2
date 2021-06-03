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
unit KIDE.UploadFileToolDesignerFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, KIDE.EditNodeBaseFrameUnit,
  KIDE.BaseFrameUnit, KIDE.CodeEditorFrameUnit, Vcl.ExtCtrls, Vcl.Tabs,
  System.Actions, Vcl.ActnList, Vcl.ComCtrls, Vcl.ToolWin, Vcl.StdCtrls,
  EF.Tree,
  KIDE.DataToolDesignerFrameUnit,
  //Upload Tools
  Kitto.Ext.StandardControllers, Kitto.Ext.Tools;

type
  TUploadFileToolDesignerFrame = class(TDataToolDesignerFrame)
    UploadToolGroupBox: TGroupBox;
    _Path: TLabeledEdit;
    _ContentType: TLabeledEdit;
    _AcceptedWildcards: TLabeledEdit;
    _MaxUploadSize: TLabeledEdit;
  private
  protected
    class function SuitsNode(const ANode: TEFNode): Boolean; override;
    procedure CleanupDefaultsToEditNode; override;
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

uses
  Kitto.Metadata.Views, Kitto.Ext.Files;

{ TUploadFileToolDesignerFrame }

procedure TUploadFileToolDesignerFrame.CleanupDefaultsToEditNode;
begin
  inherited;
  CleanupTextNode('Path');
  CleanupTextNode('AcceptedWildcards');
  CleanupTextNode('MaxUploadSize');
  CleanupTextNode('ContentType');
end;

class function TUploadFileToolDesignerFrame.SuitsNode(
  const ANode: TEFNode): Boolean;
var
  LControllerClass: TClass;
begin
  Assert(Assigned(ANode));
  LControllerClass := GetControllerClass(ANode);
  Result := Assigned(LControllerClass) and
    LControllerClass.InheritsFrom(TKExtUploadFileController);
end;

initialization
  TEditNodeFrameRegistry.Instance.RegisterClass(TUploadFileToolDesignerFrame.GetClassId, TUploadFileToolDesignerFrame);

finalization
  if Assigned(TEditNodeFrameRegistry.Instance) then
    TEditNodeFrameRegistry.Instance.UnregisterClass(TUploadFileToolDesignerFrame.GetClassId);

end.
