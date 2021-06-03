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
unit KIDE.ReportBuilderToolDesignerFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, KIDE.DownloadFileToolDesignerFrameUnit,
  KIDE.BaseFrameUnit, KIDE.CodeEditorFrameUnit, Vcl.ExtCtrls, Vcl.Tabs, EF.Tree,
  {$IFDEF REPORTBUILDER}Kitto.Ext.ReportBuilderTools,{$ENDIF}
  Vcl.StdCtrls, Kitto.Metadata.Models, System.Actions, Vcl.ActnList,
  Vcl.ComCtrls, Vcl.ToolWin;

type
  TReportBuilderToolDesignerFrame = class(TDownloadFileToolDesignerFrame)
    ReportBuilderlToolGroupBox: TGroupBox;
    _TemplateFileName: TLabeledEdit;
    _Design: TCheckBox;
  strict private
  protected
    class function SuitsNode(const ANode: TEFNode): Boolean; override;
    procedure UpdateDesignPanel(const AForce: Boolean = False); override;
    procedure CleanupDefaultsToEditNode; override;
  public
  end;

implementation

uses
  KIDE.EditNodeBaseFrameUnit;

{$R *.dfm}

{ TReportBuilderToolDesignerFrame }

procedure TReportBuilderToolDesignerFrame.CleanupDefaultsToEditNode;
begin
  inherited;
  CleanupBooleanNode('Design');
end;

class function TReportBuilderToolDesignerFrame.SuitsNode(const ANode: TEFNode): Boolean;
{$IFDEF REPORTBUILDER}
var
  LControllerClass: TClass;
  {$ENDIF}
begin
  {$IFDEF REPORTBUILDER}
    Assert(Assigned(ANode));
    LControllerClass := GetControllerClass(ANode);
    Result := Assigned(LControllerClass) and
      LControllerClass.InheritsFrom(TReportBuilderToolController);
  {$ELSE}
    Result := False
  {$ENDIF}
end;

procedure TReportBuilderToolDesignerFrame.UpdateDesignPanel(const AForce: Boolean);
begin
  inherited;
  HideFileNameEdit;
end;

initialization
  TEditNodeFrameRegistry.Instance.RegisterClass(TReportBuilderToolDesignerFrame.GetClassId, TReportBuilderToolDesignerFrame);

finalization
  if Assigned(TEditNodeFrameRegistry.Instance) then
    TEditNodeFrameRegistry.Instance.UnregisterClass(TReportBuilderToolDesignerFrame.GetClassId);

end.
