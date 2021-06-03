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
unit KIDE.DefaultTreeDesignerFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.Tabs,
  EF.Tree,
  KIDE.TreeDesignerFrameUnit, KIDE.BaseFrameUnit, KIDE.CodeEditorFrameUnit,
  KIDE.EFTreeFrameUnit, System.Actions, Vcl.ActnList, Vcl.ComCtrls, Vcl.ToolWin,
  KIDE.NodeDesignerFrameUnit, Vcl.StdCtrls;

type
  TDefaultTreeDesignerFrame = class(TTreeDesignerFrame)
  private
    FTreeFrame: TEFTreeFrame;
    procedure TreeFrameChange(const ASender: TEFTreeFrame; const ANode: TEFTree);
    procedure TreeFrameEdited(const ASender: TEFTreeFrame; const ANode: TEFTree);
  strict protected
    procedure UpdateDesignPanel(const AForce: Boolean = False); override;
  protected
    class function SuitsTree(const ATree: TEFTree): Boolean; override;
  public
    procedure AfterConstruction; override;
  end;

implementation

{$R *.dfm}

{ TDefaultTreeDesignerFrame }

procedure TDefaultTreeDesignerFrame.AfterConstruction;
begin
  inherited;
  FTreeFrame := TEFTreeFrame.Create(Self);
  FTreeFrame.ShowAllNodes := True;
  FTreeFrame.OnChange := TreeFrameChange;
  FTreeFrame.OnEdited := TreeFrameEdited;
  FTreeFrame.Parent := DesignPanel;
  FTreeFrame.Align := alClient;
end;

procedure TDefaultTreeDesignerFrame.TreeFrameChange(const ASender: TEFTreeFrame; const ANode: TEFTree);
begin
end;

procedure TDefaultTreeDesignerFrame.TreeFrameEdited(const ASender: TEFTreeFrame; const ANode: TEFTree);
begin
  Apply;
end;

procedure TDefaultTreeDesignerFrame.UpdateDesignPanel(const AForce: Boolean);
begin
  inherited;
  FTreeFrame.EFTree := EditNode;
end;

class function TDefaultTreeDesignerFrame.SuitsTree(const ATree: TEFTree): Boolean;
begin
  Result := Assigned(ATree);
end;

end.
