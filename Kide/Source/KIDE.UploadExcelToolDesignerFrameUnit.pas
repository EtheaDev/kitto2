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
unit KIDE.UploadExcelToolDesignerFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, KIDE.EditNodeBaseFrameUnit,
  KIDE.BaseFrameUnit, KIDE.CodeEditorFrameUnit, Vcl.ExtCtrls, Vcl.Tabs,
  System.Actions, Vcl.ActnList, Vcl.ComCtrls, Vcl.ToolWin, Vcl.StdCtrls,
  EF.Tree,
  Kitto.Ext.ADOTools, KIDE.UploadFileToolDesignerFrameUnit;

type
  TUploadExcelToolDesignerFrame = class(TUploadFileToolDesignerFrame)
    UploadExcelToolGroupBox: TGroupBox;
    _ExcelRangeName: TLabeledEdit;
  private
  protected
    procedure UpdateDesignPanel(const AForce: Boolean = False); override;
    class function SuitsNode(const ANode: TEFNode): Boolean; override;
    procedure CleanupDefaultsToEditNode; override;
  public
  end;

implementation

{$R *.dfm}

uses
  Kitto.Excel, Kitto.Metadata.Views;

{ TExportExcelToolDesignerFrame }

procedure TUploadExcelToolDesignerFrame.CleanupDefaultsToEditNode;
begin
  inherited;
  CleanupTextNode('ExcelRangeName', EXCEL_DEFAULT_RANGE);
  CleanupTextNode('AcceptedWildcards', DefaultExcelWildcards);
end;

class function TUploadExcelToolDesignerFrame.SuitsNode(
  const ANode: TEFNode): Boolean;
var
  LControllerClass: TClass;
begin
  Assert(Assigned(ANode));
  LControllerClass := GetControllerClass(ANode);
  Result := Assigned(LControllerClass) and
    LControllerClass.InheritsFrom(TUploadExcelToolController);
end;

procedure TUploadExcelToolDesignerFrame.UpdateDesignPanel(
  const AForce: Boolean);
begin
  inherited;
  _ExcelRangeName.Text := EditNode.GetString('ExcelRangeName', EXCEL_DEFAULT_RANGE);
  _AcceptedWildcards.Text := EditNode.GetString('AcceptedWildcards', DefaultExcelWildcards);
end;

initialization
  TEditNodeFrameRegistry.Instance.RegisterClass(TUploadExcelToolDesignerFrame.GetClassId, TUploadExcelToolDesignerFrame);

finalization
  if Assigned(TEditNodeFrameRegistry.Instance) then
    TEditNodeFrameRegistry.Instance.UnregisterClass(TUploadExcelToolDesignerFrame.GetClassId);

end.
