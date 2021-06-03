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
unit KIDE.ControllerDesignerFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, KIDE.NodeDesignerFrameUnit,
  KIDE.BaseFrameUnit, KIDE.CodeEditorFrameUnit, Vcl.ExtCtrls, Vcl.Tabs,
  System.Actions, Vcl.ActnList, Vcl.ComCtrls, Vcl.ToolWin, Vcl.StdCtrls, Vcl.Buttons,
  EF.Tree, KIDE.EditNodeBaseFrameUnit;

type
  TControllerDesignerFrame = class(TEditNodeBaseFrame)
    ControllerGroupBox: TGroupBox;
    procedure IntegerKeyPress(Sender: TObject; var Key: Char);
  strict private
  protected
    procedure UpdateDesignPanel(const AForce: Boolean); override;
    class function SuitsNode(const ANode: TEFNode): Boolean; override;
    procedure CleanupDefaultsToEditNode; override;
  public
  end;

implementation

{$R *.dfm}

uses
  EF.Macros,
  Kitto.Ext.Base,
  //View classes for this designer
  Kitto.Ext.DataTool,
  KIDE.Utils;

{ TDownloadFileToolDesignerFrame }

procedure TControllerDesignerFrame.CleanupDefaultsToEditNode;
begin
  inherited;
  CleanupTextNode('ControllerType');
end;

procedure TControllerDesignerFrame.IntegerKeyPress(Sender: TObject;
  var Key: Char);
begin
  //An integer node can containts numbers and MB ok KB letters
  if not CharInSet(Key, ['0'..'9',#8,'M','B','K']) then
    Key := #0;
end;

class function TControllerDesignerFrame.SuitsNode(
  const ANode: TEFNode): Boolean;
var
  LControllerClass: TClass;
begin
  Assert(Assigned(ANode));
  LControllerClass := GetControllerClass(ANode);
  Result := Assigned(LControllerClass) and
    LControllerClass.InheritsFrom(TKExtControllerBase);
end;

procedure TControllerDesignerFrame.UpdateDesignPanel(const AForce: Boolean);
var
  LControllerClassName: string;
begin
  inherited;
  if EditNode is TEFNode then
  begin
    LControllerClassName := GetControllerClassName(TEFNode(EditNode));
    ControllerGroupBox.Caption := Format('%s: %s',[TEFNode(EditNode).Name, LControllerClassName]);
  end;
end;

initialization
  TEditNodeFrameRegistry.Instance.RegisterClass(TControllerDesignerFrame.GetClassId, TControllerDesignerFrame);

finalization
  if Assigned(TEditNodeFrameRegistry.Instance) then
    TEditNodeFrameRegistry.Instance.UnregisterClass(TControllerDesignerFrame.GetClassId);

end.
