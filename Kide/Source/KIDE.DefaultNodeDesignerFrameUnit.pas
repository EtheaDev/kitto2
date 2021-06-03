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
unit KIDE.DefaultNodeDesignerFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.Tabs,
  EF.Tree,
  KIDE.TreeDesignerFrameUnit, KIDE.BaseFrameUnit, KIDE.CodeEditorFrameUnit,
  KIDE.EFTreeFrameUnit, System.Actions, Vcl.ActnList, Vcl.ComCtrls, Vcl.ToolWin,
  KIDE.NodeDesignerFrameUnit, KIDE.DefaultTreeDesignerFrameUnit, Vcl.StdCtrls;

type
  TDefaultNodeDesignerFrame = class(TNodeDesignerFrame)
  private
    FTreeFrame: TEFTreeFrame;
  strict protected
    procedure UpdateDesignPanel(const AForce: Boolean); override;
  protected
  public
    procedure AfterConstruction; override;
    procedure Init(const ANode: TEFTree); override;
  end;

implementation

{$R *.dfm}

uses
  KIDE.SimpleNodeDesignerFrameUnit;

{ TDefaultNodeDesignerFrame }

procedure TDefaultNodeDesignerFrame.AfterConstruction;
begin
  inherited;
end;

procedure TDefaultNodeDesignerFrame.Init(const ANode: TEFTree);
begin
  inherited;
  if (ANode is TEFNode) and TSimpleNodeDesignerFrame.SuitsNode(TEFNode(ANode)) then
  begin
    EmbedEditNodeFrame(DesignPanel, TSimpleNodeDesignerFrame, TEFNode(ANode));
  end
  else
  begin
    if not Assigned(FTreeFrame) then
    begin
      FTreeFrame := TEFTreeFrame.Create(Self);
      FTreeFrame.ShowAllNodes := True;
      FTreeFrame.Parent := DesignPanel;
      FTreeFrame.Align := alClient;
    end;
  end;
end;

procedure TDefaultNodeDesignerFrame.UpdateDesignPanel(const AForce: Boolean);
begin
  inherited;
  if Assigned(FTreeFrame) then
    FTreeFrame.EFTree := EditNode;
end;

end.
