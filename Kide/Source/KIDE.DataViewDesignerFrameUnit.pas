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
unit KIDE.DataViewDesignerFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, KIDE.SubViewDesignerFrameUnit, Vcl.StdActns, System.Actions,
  Vcl.ActnList, Vcl.Tabs, KIDE.BaseFrameUnit, KIDE.CodeEditorFrameUnit, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.Buttons, Vcl.ComCtrls, Vcl.ToolWin,
  EF.Tree, KIDE.TreeDesignerFrameUnit, Kitto.Metadata.DataView,
  KIDE.ViewDesignerFrameUnit;

type
  TDataViewDesignerFrame = class(TSubViewDesignerFrame)
    MainTableTabSheet: TTabSheet;
    _IsLookup: TCheckBox;
    _Type: TLabeledEdit;
  private
    function GetDataView: TKDataView;
  protected
    class function SuitsTree(const ATree: TEFTree): Boolean; override;
    class function SuitsNode(const ANode: TEFNode): Boolean; override;
    procedure CleanupDefaultsToEditNode; override;
  public
    procedure Init(const ANode: TEFTree); override;
  end;

var
  DataViewDesignerFrame: TDataViewDesignerFrame;

implementation

{$R *.dfm}

uses
  KIDE.EditNodeBaseFrameUnit,
  KIDE.ViewTableDesignerFrameUnit;

{ TDataViewDesignerFrame }

procedure TDataViewDesignerFrame.CleanupDefaultsToEditNode;
begin
  inherited;
  CleanupBooleanNode('IsLookup');
end;

function TDataViewDesignerFrame.GetDataView: TKDataView;
begin
  Result := EditNode as TKDataView;
end;

procedure TDataViewDesignerFrame.Init(const ANode: TEFTree);
begin
  inherited;
  EmbedEditNodeFrame(MainTableTabSheet, TViewTableDesignerFrame,
    ANode.FindNode('MainTable', True));
end;

class function TDataViewDesignerFrame.SuitsTree(const ATree: TEFTree): Boolean;
begin
  Result := ATree is TKDataView;
end;

class function TDataViewDesignerFrame.SuitsNode(const ANode: TEFNode): Boolean;
begin
  Result := False;
end;

initialization
  TEditNodeFrameRegistry.Instance.RegisterClass(TDataViewDesignerFrame.GetClassId, TDataViewDesignerFrame);

finalization
  if Assigned(TEditNodeFrameRegistry.Instance) then
    TEditNodeFrameRegistry.Instance.UnregisterClass(TDataViewDesignerFrame.GetClassId);

end.
