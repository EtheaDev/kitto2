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
unit KIDE.LoginWindowControllerDesignerFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, KIDE.EditNodeBaseFrameUnit,
  Vcl.ExtCtrls, Vcl.Tabs,
  System.Actions, Vcl.ActnList, Vcl.ComCtrls, Vcl.ToolWin, Vcl.StdCtrls, Vcl.Buttons,
  KIDE.BaseFrameUnit, KIDE.CodeEditorFrameUnit, KIDE.ControllerDesignerFrameUnit,
  EF.Tree, Vcl.Samples.Spin;

type
  TLoginWindowControllerDesignerFrame = class(TControllerDesignerFrame)
    LoginPageControl: TPageControl;
    WindowTabSheet: TTabSheet;
    LocalStorageTabSheet: TTabSheet;
    BorderPanelTabSheet: TTabSheet;
    ExtraWidthLabel: TLabel;
    _ExtraWidth: TSpinEdit;
    ExtraHeightLabel: TLabel;
    _ExtraHeight: TSpinEdit;
    LabelWidthLabel: TLabel;
    _LabelWidth: TSpinEdit;
    procedure LoginPageControlChange(Sender: TObject);
  private
  protected
    class function SuitsNode(const ANode: TEFNode): Boolean; override;
    procedure UpdateDesignPanel(const AForce: Boolean = False); override;
    procedure CleanupDefaultsToEditNode; override;
    procedure DesignPanelToEditNode; override;
  protected
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
  KIDE.LoginWindowLocalStorageDesignerFrameUnit,
  KIDE.Utils;

{ TWindowControllerDesignerFrame }

procedure TLoginWindowControllerDesignerFrame.CleanupDefaultsToEditNode;
var
  Resizable: Boolean;
begin
  inherited;
  CleanupIntegerNode('ExtraWidth');
  CleanupIntegerNode('ExtraHeight');
  CleanupTextNode('LabelWidth');
  CleanupOrphanNode('LocalStorage');
end;

procedure TLoginWindowControllerDesignerFrame.DesignPanelToEditNode;
begin
  inherited;
  LoginPageControl.ActivePageIndex := 0;
end;

procedure TLoginWindowControllerDesignerFrame.Init(const ANode: TEFTree);
begin
  inherited;
  EmbedEditNodeFrame(LocalStorageTabSheet, TLoginWindowLocalStorageDesignerFrame,
    EditNode.FindNode('LocalStorage'));
end;

procedure TLoginWindowControllerDesignerFrame.LoginPageControlChange(
  Sender: TObject);
begin
  inherited;
  if LoginPageControl.ActivePage = LocalStorageTabSheet then
    EmbedEditNodeFrame(LocalStorageTabSheet, TLoginWindowLocalStorageDesignerFrame,
      EditNode.FindNode('LocalStorage', True), True);
end;

class function TLoginWindowControllerDesignerFrame.SuitsNode(
  const ANode: TEFNode): Boolean;
var
  LControllerClass: TClass;
begin
  Assert(Assigned(ANode));
  LControllerClass := GetControllerClass(ANode);
  Result := Assigned(LControllerClass) and
    LControllerClass.InheritsFrom(TKExtLoginPanel);
end;

procedure TLoginWindowControllerDesignerFrame.UpdateDesignPanel(
  const AForce: Boolean);
begin
  inherited;
  Assert(Assigned(EditNode));
  LoginPageControl.ActivePageIndex := 0;
end;

initialization
  TEditNodeFrameRegistry.Instance.RegisterClass(TLoginWindowControllerDesignerFrame.GetClassId, TLoginWindowControllerDesignerFrame);

finalization
  if Assigned(TEditNodeFrameRegistry.Instance) then
    TEditNodeFrameRegistry.Instance.UnregisterClass(TLoginWindowControllerDesignerFrame.GetClassId);

end.
