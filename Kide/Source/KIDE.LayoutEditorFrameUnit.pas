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
unit KIDE.LayoutEditorFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, KIDE.MetadataEditorBaseFrameUnit,
  Vcl.ActnList, Vcl.ComCtrls, Vcl.ToolWin, KIDE.BaseFrameUnit, Kitto.Metadata.Views,
  KIDE.CodeEditorFrameUnit, Vcl.ExtCtrls, EF.Tree, System.Actions;

type
  TLayoutEditorFrame = class(TMetadataEditorBaseFrame)
  private
    function GetEditLayout: TKLayout;
    property EditLayout: TKLayout read GetEditLayout;
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
  KITTO.Metadata.DataView,
  KIDE.Editor, KIDE.DesignMetadata, KIDE.LayoutValidator;

{ TLayoutEditorFrame }

function TLayoutEditorFrame.GetEditLayout: TKLayout;
begin
  Result := EditMetadata as TKLayout;
end;

function TLayoutEditorFrame.GetEditorProperty(const AName: string): Variant;
begin
  if SameText(AName, 'ImageIndex') then
    Result := 8
  else
    Result := inherited GetEditorProperty(AName);
end;

function TLayoutEditorFrame.GetPreviewCommand: string;
var
  LViewTable: TKViewTable;
  LLayoutName: string;
begin
  Result := inherited GetPreviewCommand;
  LLayoutName := Copy(EditLayout.PersistentName,length(LLayoutName)-4,5);
  if SameText(LLayoutName,'_Form') then
  begin
    LViewTable := GetViewTableOfLayout(EditLayout, LLayoutName);
    if Assigned(LViewTable) then
      Result := Format('view=%s&action=%s', [LViewTable.View.PersistentName, 'New']);
  end;
end;

function TLayoutEditorFrame.GetTreeClass: TEFTreeClass;
begin
  Result := TKLayout;
end;

procedure TLayoutEditorFrame.InitEditor(const AParams: TEFNode);
begin
  inherited;
end;

procedure TLayoutEditorFrame.ValidateEditor;
var
  LLayoutValidator: TLayoutValidator;
begin
  LLayoutValidator := TLayoutValidator.Create;
  try
    LLayoutValidator.ValidateLayouts(EditLayout);
  finally
    FreeAndNil(LLayoutValidator);
  end;
end;

initialization
  TEditorRegistry.Instance.RegisterClass(TLayoutEditorFrame.ClassName, TLayoutEditorFrame);

finalization
  TEditorRegistry.Instance.UnregisterClass(TLayoutEditorFrame.ClassName);

end.
