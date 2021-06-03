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
unit KIDE.LoginWindowLocalStorageDesignerFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, KIDE.EditNodeBaseFrameUnit,
  Vcl.ExtCtrls, Vcl.Tabs,
  System.Actions, Vcl.ActnList, Vcl.ComCtrls, Vcl.ToolWin, Vcl.StdCtrls, Vcl.Buttons,
  KIDE.BaseFrameUnit, KIDE.CodeEditorFrameUnit,
  EF.Tree, Vcl.Samples.Spin, KIDE.ControllerDesignerFrameUnit;

type
  TLoginWindowLocalStorageDesignerFrame = class(TEditNodeBaseFrame)
    ModeLabel: TLabel;
    _Mode: TComboBox;
    _AskUser_Default: TCheckBox;
    _AskUser: TCheckBox;
  private
  protected
    class function SuitsNode(const ANode: TEFNode): Boolean; override;
    procedure UpdateDesignPanel(const AForce: Boolean = False); override;
    procedure CleanupDefaultsToEditNode; override;
    procedure UpdateEditComponents; override;
  public
    procedure Init(const ANode: TEFTree); override;
  end;

implementation

{$R *.dfm}

uses
  EF.Macros,
  Kitto.Ext.Base,
  Kitto.Ext.AccordionPanel,
  Kitto.Ext.ToolBar,
  Kitto.Ext.Login,
  Kitto.Ext.List,
  Kitto.Ext.Form,
  KIDE.BorderPanelControllerDesignerFrameUnit,
  KIDE.Utils;

{ TWindowControllerDesignerFrame }

procedure TLoginWindowLocalStorageDesignerFrame.CleanupDefaultsToEditNode;
var
  Resizable: Boolean;
begin
  inherited;
  CleanupTextNode('Mode');
  CleanupBooleanNode('AskUser/Default', True);
  CleanupOrphanNode('AskUser');
  CleanupBooleanNode('AskUser');
end;

procedure TLoginWindowLocalStorageDesignerFrame.Init(const ANode: TEFTree);
begin
  inherited;
end;

class function TLoginWindowLocalStorageDesignerFrame.SuitsNode(
  const ANode: TEFNode): Boolean;
var
  LControllerClass: TClass;
begin
  Result := False;
  if SameText(ANode.Name, 'LocalStorage') and (ANode.Parent is TEFNode) then
  begin
    LControllerClass := GetControllerClass(TEFNode(ANode.Parent));
    Result := Assigned(LControllerClass) and
    LControllerClass.InheritsFrom(TKExtLoginPanel);
  end;
end;

procedure TLoginWindowLocalStorageDesignerFrame.UpdateDesignPanel(
  const AForce: Boolean);
begin
  inherited;
  Assert(Assigned(EditNode));
  _AskUser_Default.Checked := EditNode.GetBoolean('AskUser/Default', True);
end;

procedure TLoginWindowLocalStorageDesignerFrame.UpdateEditComponents;
begin
  inherited;
  _AskUser_Default.Visible := _AskUser.Checked;
end;

initialization
  TEditNodeFrameRegistry.Instance.RegisterClass(TLoginWindowLocalStorageDesignerFrame.GetClassId, TLoginWindowLocalStorageDesignerFrame);

finalization
  if Assigned(TEditNodeFrameRegistry.Instance) then
    TEditNodeFrameRegistry.Instance.UnregisterClass(TLoginWindowLocalStorageDesignerFrame.GetClassId);

end.
