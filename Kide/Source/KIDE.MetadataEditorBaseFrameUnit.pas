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
unit KIDE.MetadataEditorBaseFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ActnList, Vcl.ToolWin,
  Vcl.ComCtrls, Vcl.ExtCtrls, Vcl.ImgList,
  EF.Tree,
  Kitto.Metadata, VirtualTrees,
  KIDE.PersistentTreeEditorBaseFrameUnit, KIDE.BaseFrameUnit, KIDE.CodeEditorFrameUnit,
  KIDE.TreeEditorBaseFrameUnit, System.Actions;

type
  TMetadataEditorBaseFrame = class {abstract}(TPersistentTreeEditorBaseFrame)
  private
    function GetEditMetadata: TKMetadata;
    function GetOriginalMetadata: TKMetadata;
  strict protected
    procedure Save; override;
    procedure Reload; override;
    property EditMetadata: TKMetadata read GetEditMetadata;
    property OriginalMetadata: TKMetadata read GetOriginalMetadata;
  end;

implementation

{$R *.dfm}

{ TMetadataEditorBaseFrame }

function TMetadataEditorBaseFrame.GetEditMetadata: TKMetadata;
begin
  Result := EditObject as TKMetadata;
end;

function TMetadataEditorBaseFrame.GetOriginalMetadata: TKMetadata;
begin
  Result := OriginalObject as TKMetadata;
end;

procedure TMetadataEditorBaseFrame.Save;
begin
  inherited;
  OriginalMetadata.Catalog.SaveObject(OriginalMetadata);
end;

procedure TMetadataEditorBaseFrame.Reload;
begin
  inherited;
//  EditMetadata.Assign(OriginalMetadata);
end;

end.
