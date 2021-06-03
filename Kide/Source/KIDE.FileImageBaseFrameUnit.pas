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
unit KIDE.FileImageBaseFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ImgList, Vcl.ExtCtrls,
  Vcl.ToolWin, Vcl.ComCtrls, Vcl.ActnList, EF.Tree, EF.YAML,
  KIDE.FileTree, KIDE.EditorFrameUnit, KIDE.BaseFrameUnit, KIDE.ImageViewerFrameUnit, KIDE.Editor,
  KIDE.TreeDesignerFrameUnit, KIDE.NodeDesignerFrameUnit, System.Actions;

type
  TFileImageBaseFrame = class {abstract}(TEditorFrame)
    ClientPanel: TPanel;
    ImageViewerFrame: TImageViewerFrame;
  strict private
    FFileNodeHandler: TFileNodeHandler;
  private
    procedure InitImageViewer;
  strict protected
    function EditorMatchesSpec(const ASpec: string): Boolean; override;
    function EditorSuits(const ASpec: string; const AParams: TEFNode): Boolean; override;
    function GetEditorProperty(const AName: string): Variant; override;
    procedure Reload; override;
    procedure RefreshEditor; override;
    property FileNodeHandler: TFileNodeHandler read FFileNodeHandler;
    function GetTreeClass: TEFTreeClass; virtual; abstract;
  public
    destructor Destroy; override;
    procedure InitEditor(const AParams: TEFNode); override;
    function GetSpec: string; override;
  end;

implementation

{$R *.dfm}

uses
  EF.StrUtils, EF.Sys.Windows,
  KIDE.MRUOptions, KIDE.DesignMetadata, KIDE.MainDataModuleUnit, KIDE.Project;

{ TFileEditorBaseFrame }

destructor TFileImageBaseFrame.Destroy;
begin
  inherited;
end;

procedure TFileImageBaseFrame.InitEditor(const AParams: TEFNode);
begin
  inherited;
  FFileNodeHandler := AParams.GetObject('Object') as TFileNodeHandler;
  Assert(Assigned(FFileNodeHandler));

  Assert(DirectoryExists(ExtractFilePath(FFileNodeHandler.FileName)));

  InitImageViewer;
end;

procedure TFileImageBaseFrame.InitImageViewer;
begin
  ImageViewerFrame.LoadFromFile(FFileNodeHandler.FileName);
end;

procedure TFileImageBaseFrame.Reload;
begin
  inherited;
  ImageViewerFrame.LoadFromFile(FileNodeHandler.FileName);
end;

procedure TFileImageBaseFrame.RefreshEditor;
begin
  inherited;
  InitImageViewer;
end;

function TFileImageBaseFrame.EditorMatchesSpec(const ASpec: string): Boolean;
begin
  Result := SameFileName(ASpec, FFileNodeHandler.FileName);
end;

function TFileImageBaseFrame.EditorSuits(const ASpec: string;
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
    Result := MatchStr(LFileExt, ['.png','.jpg','.gif','.bmp']);
  end;
end;

function TFileImageBaseFrame.GetEditorProperty(const AName: string): Variant;
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

function TFileImageBaseFrame.GetSpec: string;
begin
  Result := FFileNodeHandler.FileName;
end;

initialization
  TEditorRegistry.Instance.RegisterClass(TFileImageBaseFrame.ClassName, TFileImageBaseFrame);

finalization
  TEditorRegistry.Instance.UnregisterClass(TFileImageBaseFrame.ClassName);

end.
