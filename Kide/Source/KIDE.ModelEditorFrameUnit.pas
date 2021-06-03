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
unit KIDE.ModelEditorFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, KIDE.MetadataEditorBaseFrameUnit, EF.Tree,
  Kitto.Metadata.Models, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Tabs, Vcl.ComCtrls,
  KIDE.BaseFrameUnit, KIDE.CodeEditorFrameUnit, VirtualTrees, Vcl.ImgList,
  Vcl.ActnList, Vcl.ToolWin, System.Actions;

type
  TModelEditorFrame = class(TMetadataEditorBaseFrame)
  private
    function GetEditModel: TKModel;
    property EditModel: TKModel read GetEditModel;
  strict protected
    function GetTreeClass: TEFTreeClass; override;
    function GetEditorProperty(const AName: string): Variant; override;
    procedure ValidateEditor; override;
  public
    procedure InitEditor(const AParams: TEFNode); override;
  end;

implementation

{$R *.dfm}

uses
  KIDE.Editor, KIDE.ModelValidator;


{ TModelEditorFrame }

function TModelEditorFrame.GetEditModel: TKModel;
begin
  Result := EditMetadata as TKModel;
end;

function TModelEditorFrame.GetEditorProperty(const AName: string): Variant;
begin
  if SameText(AName, 'ImageIndex') then
    Result := 5
  else
    Result := inherited GetEditorProperty(AName);
end;

function TModelEditorFrame.GetTreeClass: TEFTreeClass;
begin
  Result := TKModel;
end;

procedure TModelEditorFrame.InitEditor(const AParams: TEFNode);
begin
  inherited;
end;

procedure TModelEditorFrame.ValidateEditor;
var
  LModelValidator: TModelValidator;
begin
  LModelValidator := TModelValidator.Create;
  try
    LModelValidator.ValidateModels(EditModel);
  finally
    FreeAndNil(LModelValidator);
  end;
end;

initialization
  TEditorRegistry.Instance.RegisterClass(TModelEditorFrame.ClassName, TModelEditorFrame);

finalization
  TEditorRegistry.Instance.UnregisterClass(TModelEditorFrame.ClassName);

end.
