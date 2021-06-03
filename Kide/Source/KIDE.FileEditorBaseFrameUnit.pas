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
unit KIDE.FileEditorBaseFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ImgList, Vcl.ExtCtrls,
  Vcl.ToolWin, Vcl.ComCtrls, Vcl.ActnList, EF.Tree, EF.YAML,
  KIDE.FileTree, KIDE.EditorFrameUnit, KIDE.BaseFrameUnit, KIDE.CodeEditorFrameUnit, KIDE.Editor,
  KIDE.TreeDesignerFrameUnit, KIDE.NodeDesignerFrameUnit, System.Actions;

type
  TFileEditorBaseFrame = class {abstract}(TEditorFrame)
    ClientPanel: TPanel;
    CodeEditorFrame: TCodeEditorFrame;
  strict private
    FEditCode: string;
    FFileNodeHandler: TFileNodeHandler;
  private
    procedure UpdateCodeEditor(const AForce: Boolean);
    procedure InitEditCode;
  strict protected
    function EditorMatchesSpec(const ASpec: string): Boolean; override;
    function EditorSuits(const ASpec: string; const AParams: TEFNode): Boolean; override;
    function GetEditorProperty(const AName: string): Variant; override;
    procedure Save; override;
    procedure Reload; override;
    procedure RefreshEditor; override;
    property FileNodeHandler: TFileNodeHandler read FFileNodeHandler;
    property EditCode: string read FEditCode;
    function IsChanged: Boolean; override;
    function GetTreeClass: TEFTreeClass; virtual; abstract;
    function GetPreviewCommand: string; virtual;
  public
    destructor Destroy; override;
    procedure InitEditor(const AParams: TEFNode); override;
    function IsEditorActionEnabled(const AAction: TEditorAction): Boolean; override;
    function GetSpec: string; override;
  end;

implementation

{$R *.dfm}

uses
  EF.StrUtils, EF.Sys.Windows,
  KIDE.MRUOptions, KIDE.DesignMetadata, KIDE.MainDataModuleUnit, KIDE.Project;

{ TFileEditorBaseFrame }

destructor TFileEditorBaseFrame.Destroy;
begin
  inherited;
end;

procedure TFileEditorBaseFrame.InitEditor(const AParams: TEFNode);
begin
  inherited;
  FFileNodeHandler := AParams.GetObject('Object') as TFileNodeHandler;
  Assert(Assigned(FFileNodeHandler));

  Assert(DirectoryExists(ExtractFilePath(FFileNodeHandler.FileName)));

  InitEditCode;
  UpdateCodeEditor(True);
end;

procedure TFileEditorBaseFrame.InitEditCode;
begin
  CodeEditorFrame.LoadFromFile(FFileNodeHandler.FileName, TEncoding.UTF8);
  FEditCode := CodeEditorFrame.Code;
end;

function TFileEditorBaseFrame.IsChanged: Boolean;
begin
  Result := (FEditCode <> CodeEditorFrame.Code);
end;

function TFileEditorBaseFrame.IsEditorActionEnabled(
  const AAction: TEditorAction): Boolean;
begin
  if AAction in [eaSave, eaReload] then
    Result := IsChanged
  else
    Result := inherited IsEditorActionEnabled(AAction);
end;

procedure TFileEditorBaseFrame.Save;
begin
  inherited;
  CodeEditorFrame.SaveToFile(FileNodeHandler.FileName, TEncoding.UTF8);
  FEditCode := CodeEditorFrame.Code;
end;

procedure TFileEditorBaseFrame.Reload;
begin
  inherited;
  CodeEditorFrame.LoadFromFile(FileNodeHandler.FileName, TEncoding.UTF8);
  FEditCode := CodeEditorFrame.Code;
end;

procedure TFileEditorBaseFrame.RefreshEditor;
begin
  inherited;
  InitEditCode;
  UpdateCodeEditor(True);
end;

procedure TFileEditorBaseFrame.UpdateCodeEditor(const AForce: Boolean);
begin
  if AForce then
    CodeEditorFrame.RefreshCode(FEditCode)
  else
    CodeEditorFrame.Code := FEditCode;
end;

function TFileEditorBaseFrame.EditorMatchesSpec(const ASpec: string): Boolean;
begin
  Result := SameFileName(ASpec, FFileNodeHandler.FileName);
end;

function TFileEditorBaseFrame.EditorSuits(const ASpec: string;
  const AParams: TEFNode): Boolean;
var
  LFileExt: string;
  LObject: TObject;
begin
  Result := False;
  LObject := AParams.GetObject('Object');
  if LObject.InheritsFrom(TFileNodeHandler) then
  begin
    LFileExt := ExtractFileExt(TFileNodeHandler(LObject).FileName);
    Result := MatchStr(LFileExt, ['.css', '.js', '.json', '.yaml', '.html']);
  end;
end;

function TFileEditorBaseFrame.GetEditorProperty(const AName: string): Variant;
begin
  if SameText(AName, 'ShortTitle') then
    Result := ExtractFileName(FFileNodeHandler.FileName)
  else if SameText(AName, 'LongTitle') then
    Result := FFileNodeHandler.FileName
  else if SameText(AName, 'ImageIndex') then
    Result := GetFileImageIndex(ExtractFileFormat(FFileNodeHandler.FileName))
  else
    Result := inherited GetEditorProperty(AName);
end;

function TFileEditorBaseFrame.GetPreviewCommand: string;
begin
  Result := '';
end;

function TFileEditorBaseFrame.GetSpec: string;
begin
  Result := FFileNodeHandler.FileName;
end;

initialization
  TEditorRegistry.Instance.RegisterClass(TFileEditorBaseFrame.ClassName, TFileEditorBaseFrame);

finalization
  TEditorRegistry.Instance.UnregisterClass(TFileEditorBaseFrame.ClassName);

end.
