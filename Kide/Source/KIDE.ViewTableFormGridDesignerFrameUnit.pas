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
unit KIDE.ViewTableFormGridDesignerFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, KIDE.EditNodeBaseFrameUnit,
  Vcl.ExtCtrls, Vcl.Tabs,
  System.Actions, Vcl.ActnList, Vcl.ComCtrls, Vcl.ToolWin, Vcl.StdCtrls, Vcl.Buttons,
  KIDE.BaseFrameUnit, KIDE.CodeEditorFrameUnit,
  EF.Tree, Vcl.Samples.Spin, Vcl.StdActns,
  KIDE.MainDataModuleUnit;

type
  TViewTableFormGridDesignerFrame = class(TEditNodeBaseFrame)
    FormGridGroupBox: TGroupBox;
    FileOpenLayoutAction: TFileOpen;
    ViewNameSpeedButton: TSpeedButton;
    _Layout: TLabeledEdit;
    procedure FileOpenLayoutActionAccept(Sender: TObject);
    procedure FileOpenLayoutActionBeforeExecute(Sender: TObject);
  private
  protected
    class function SuitsNode(const ANode: TEFNode): Boolean; override;
    procedure UpdateDesignPanel(const AForce: Boolean = False); override;
    procedure CleanupDefaultsToEditNode; override;
    procedure DesignPanelToEditNode; override;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    class function IsViewTableFormOrGridNode(const ANode: TEFNode): Boolean;
  end;

implementation

{$R *.dfm}

uses
  StrUtils,
  EF.Macros,
  Kitto.Ext.Base,
  Kitto.Ext.Form,
  KIDE.Project, KIDE.Config, KIDE.MainTableControllerDesignerFrameUnit;

{ TViewTableFormControllerDesignerFrame }

procedure TViewTableFormGridDesignerFrame.CleanupDefaultsToEditNode;
begin
  inherited;
  CleanupTextNode('Layout');
end;

constructor TViewTableFormGridDesignerFrame.Create(AOwner: TComponent);
begin
  inherited;
end;

procedure TViewTableFormGridDesignerFrame.DesignPanelToEditNode;
begin
  inherited;
end;

procedure TViewTableFormGridDesignerFrame.FileOpenLayoutActionAccept(
  Sender: TObject);
var
  LFileName: string;
begin
  inherited;
  LFileName := ExtractFileName(FileOpenLayoutAction.Dialog.FileName);
  if SameText(ExtractFileExt(LFileName), '.yaml') then
    LFileName := Copy(LFileName, 1, length(LFileName)-5);
  _Layout.Text := LFileName;
end;

procedure TViewTableFormGridDesignerFrame.FileOpenLayoutActionBeforeExecute(
  Sender: TObject);
begin
  inherited;
  FileOpenLayoutAction.Dialog.InitialDir := TProject.CurrentProject.Config.Views.Layouts.Path;
end;

class function TViewTableFormGridDesignerFrame.IsViewTableFormOrGridNode(
  const ANode: TEFNode): Boolean;
begin
  Result := MatchText(ANode.Name, ['Form','Grid']) and (ANode.Parent is TEFNode) and
    TMainTableControllerDesignerFrame.IsViewTableControllerNode(TEFNode(ANode.Parent));
end;

class function TViewTableFormGridDesignerFrame.SuitsNode(
  const ANode: TEFNode): Boolean;
begin
  Assert(Assigned(ANode));
  Result := IsViewTableFormOrGridNode(ANode);
end;

procedure TViewTableFormGridDesignerFrame.UpdateDesignPanel(
  const AForce: Boolean);
begin
  inherited;
  Assert(Assigned(EditNode));
end;

initialization
  TEditNodeFrameRegistry.Instance.RegisterClass(TViewTableFormGridDesignerFrame.GetClassId, TViewTableFormGridDesignerFrame);

finalization
  if Assigned(TEditNodeFrameRegistry.Instance) then
    TEditNodeFrameRegistry.Instance.UnregisterClass(TViewTableFormGridDesignerFrame.GetClassId);

end.
