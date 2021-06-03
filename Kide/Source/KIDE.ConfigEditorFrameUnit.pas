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
unit KIDE.ConfigEditorFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ActnList, Vcl.ComCtrls,
  Vcl.ToolWin, Vcl.ExtCtrls,
  EF.Tree,
  Kitto.Config,
  KIDE.BaseFrameUnit, KIDE.PersistentTreeEditorBaseFrameUnit, KIDE.CodeEditorFrameUnit,
  System.Actions;

type
  TConfigEditorFrame = class(TPersistentTreeEditorBaseFrame)
  strict protected
    function GetTreeClass: TEFTreeClass; override;
    function GetEditorProperty(const AName: string): Variant; override;
    procedure Save; override;
    procedure ValidateEditor; override;
  end;

implementation

{$R *.dfm}

uses
  EF.Classes,
  KIDE.Editor, KIDE.ConfigValidator;

{ TConfigEditorFrame }

function TConfigEditorFrame.GetEditorProperty(const AName: string): Variant;
begin
  if SameText(AName, 'ImageIndex') then
    Result := 3
  else
    Result := inherited GetEditorProperty(AName);
end;

function TConfigEditorFrame.GetTreeClass: TEFTreeClass;
begin
  Result := TEFComponentConfig;
end;

procedure TConfigEditorFrame.Save;
begin
  inherited;
  if SameText(ExtractFileName(OriginalFileName), ExtractFileName(TKConfig.Instance.Config.PersistentFileName)) then
    TKConfig.Instance.InvalidateConfig;
end;

procedure TConfigEditorFrame.ValidateEditor;
var
  LConfigValidator: TConfigValidator;
begin
  LConfigValidator := TConfigValidator.Create;
  try
    LConfigValidator.ValidateConfigs(EditObject as TEFComponentConfig);
  finally
    FreeAndNil(LConfigValidator);
  end;
end;

initialization
  TEditorRegistry.Instance.RegisterClass(TConfigEditorFrame.ClassName, TConfigEditorFrame);

finalization
  TEditorRegistry.Instance.UnregisterClass(TConfigEditorFrame.ClassName);

end.
