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
unit KIDE.PersistentTreeEditorBaseFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ActnList, Vcl.ComCtrls,
  Vcl.ToolWin, Vcl.ExtCtrls,
  EF.Tree,
  KIDE.TreeEditorBaseFrameUnit, KIDE.BaseFrameUnit, KIDE.CodeEditorFrameUnit,
  System.Actions;

type
  TPersistentTreeEditorBaseFrame = class {abstract}(TTreeEditorBaseFrame)
    procedure ValidateActionUpdate(Sender: TObject);
    procedure ValidateActionExecute(Sender: TObject);
    procedure ActionListUpdate(Action: TBasicAction; var Handled: Boolean);
  private
    function GetEditPersistentTree: TEFPersistentTree;
    function GetOriginalPersistentTree: TEFPersistentTree;
  strict protected
    function GetEditorProperty(const AName: string): Variant; override;
    procedure Save; override;
    procedure Reload; override;
    function GetTreeClass: TEFTreeClass; override;
    procedure RefreshEditor; override;
    procedure ValidateEditor; virtual; abstract;
  public
    property EditPersistentTree: TEFPersistentTree read GetEditPersistentTree;
    property OriginalPersistentTree: TEFPersistentTree read GetOriginalPersistentTree;
  end;

implementation

{$R *.dfm}

uses
  EF.YAML;

{ TPersistentTreeEditorBaseFrame }

procedure TPersistentTreeEditorBaseFrame.ActionListUpdate(Action: TBasicAction;
  var Handled: Boolean);
begin
  inherited;
  ValidateAction.Visible := True;
end;

function TPersistentTreeEditorBaseFrame.GetEditorProperty(
  const AName: string): Variant;
begin
  if SameText(AName, 'ShortTitle') then
    Result := ExtractFileName(OriginalPersistentTree.PersistentName)
  else
    Result := inherited GetEditorProperty(AName);
end;

function TPersistentTreeEditorBaseFrame.GetEditPersistentTree: TEFPersistentTree;
begin
  Result := EditObject as TEFPersistentTree;
end;

function TPersistentTreeEditorBaseFrame.GetOriginalPersistentTree: TEFPersistentTree;
begin
  Result := OriginalObject as TEFPersistentTree;
end;

function TPersistentTreeEditorBaseFrame.GetTreeClass: TEFTreeClass;
begin
  Result := TEFPersistentTree;
end;

procedure TPersistentTreeEditorBaseFrame.RefreshEditor;
begin
  OriginalPersistentTree.LoadFromYamlFile(OriginalPersistentTree.PersistentFileName);
  inherited;
end;

procedure TPersistentTreeEditorBaseFrame.Save;
begin
  inherited;
  TEFYAMLWriter.SaveTree(OriginalPersistentTree, OriginalPersistentTree.PersistentFileName);
end;

procedure TPersistentTreeEditorBaseFrame.ValidateActionExecute(Sender: TObject);
begin
  inherited;
  RefreshEditor;
  ValidateEditor;
end;

procedure TPersistentTreeEditorBaseFrame.ValidateActionUpdate(Sender: TObject);
begin
  inherited;
  ValidateAction.Enabled := (GetEditPersistentTree <> nil) and not IsChanged;
end;

procedure TPersistentTreeEditorBaseFrame.Reload;
begin
  inherited;
  RefreshEditor;
end;

end.
