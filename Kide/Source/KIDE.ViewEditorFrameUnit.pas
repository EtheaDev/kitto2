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
unit KIDE.ViewEditorFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, KIDE.MetadataEditorBaseFrameUnit, EF.Tree,
  Vcl.StdCtrls, Vcl.ExtCtrls, Kitto.Metadata.Views, Vcl.ImgList, KIDE.BaseFrameUnit,
  KIDE.CodeEditorFrameUnit, VirtualTrees, Vcl.ActnList,
  Vcl.ToolWin, Vcl.ComCtrls, System.Actions;

type
  TViewEditorFrame = class(TMetadataEditorBaseFrame)
  private
    function GetEditView: TKView;
    property EditView: TKView read GetEditView;
  strict protected
    function GetTreeClass: TEFTreeClass; override;
    function GetEditorProperty(const AName: string): Variant; override;
    function GetPreviewCommand: string; override;
    procedure ValidateEditor; override;
  public
    procedure InitEditor(const AParams: TEFNode); override;
  end;

implementation

{$R *.dfm}

uses
  KIDE.Editor, KIDE.Project, KIDE.ViewValidator, KIDE.MainDataModuleUnit;

{ TViewEditorFrame }

function TViewEditorFrame.GetEditorProperty(const AName: string): Variant;
begin
  if SameText(AName, 'ImageIndex') then
    Result := TProject.CurrentProject.GetViewImageIndex(EditView)
  else
    Result := inherited GetEditorProperty(AName);
end;

function TViewEditorFrame.GetEditView: TKView;
begin
  Result := EditMetadata as TKView;
end;

function TViewEditorFrame.GetPreviewCommand: string;
begin
  Result := Format('view=%s', [EditView.PersistentName]);
end;

function TViewEditorFrame.GetTreeClass: TEFTreeClass;
begin
  Result := TKView;
end;

procedure TViewEditorFrame.InitEditor(const AParams: TEFNode);
begin
  inherited;
end;

procedure TViewEditorFrame.ValidateEditor;
var
  LViewValidator: TViewValidator;
begin
  LViewValidator := TViewValidator.Create;
  try
    LViewValidator.ValidateViews(EditView);
  finally
    FreeAndNil(LViewValidator);
  end;
end;

initialization
  TEditorRegistry.Instance.RegisterClass(TViewEditorFrame.ClassName, TViewEditorFrame);

finalization
  TEditorRegistry.Instance.UnregisterClass(TViewEditorFrame.ClassName);

end.
