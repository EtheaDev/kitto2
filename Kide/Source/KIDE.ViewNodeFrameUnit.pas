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
unit KIDE.ViewNodeFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.Actions,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.Tabs,
  Vcl.ActnList, Vcl.ComCtrls, Vcl.ToolWin, Vcl.StdCtrls, Vcl.Buttons, Vcl.StdActns,
  EF.Tree, Kitto.Ext.Base,
  KIDE.TreeDesignerFrameUnit, KIDE.BaseFrameUnit, KIDE.CodeEditorFrameUnit,
  KIDE.EditNodeBaseFrameUnit;

type
  TViewNodeFrame = class(TEditNodeBaseFrame)
    FileOpenAction: TFileOpen;
    VCPageControl: TPageControl;
    ViewTabSheet: TTabSheet;
    Controller: TTabSheet;
    ViewGroupBox: TGroupBox;
    ImageNameSpeedButton: TSpeedButton;
    ImageNameImage: TImage;
    _DisplayLabel: TLabeledEdit;
    ImageNameEdit: TLabeledEdit;
    _Controller: TLabeledEdit;
    procedure ImageNameEditChange(Sender: TObject);
    procedure FileOpenActionAccept(Sender: TObject);
  strict private
  private
    function GetDefaultImageName: string;
    function GetToolControllerClass: TExtToolControllerClass;
  protected
    class function SuitsNode(const ANode: TEFNode): Boolean; override;
    procedure UpdateDesignPanel(const AForce: Boolean = False); override;
    procedure DesignPanelToEditNode; override;
    procedure CleanupDefaultsToEditNode; override;
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

uses
  EF.Macros,
  KIDE.Project, KIDE.Utils,
  Kitto.Ext.Controller,
  //View classes for this designer
  Kitto.Ext.DataTool;

{ TDownloadFileToolDesignerFrame }

procedure TViewNodeFrame.CleanupDefaultsToEditNode;
begin
  inherited;
  CleanupTextNode('ImageName', GetDefaultImageName);
  CleanupTextNode('DisplayLabel');
end;

procedure TViewNodeFrame.DesignPanelToEditNode;
begin
  inherited;
  EditNode.SetString('ImageName', ImageNameEdit.Text)
end;

procedure TViewNodeFrame.FileOpenActionAccept(Sender: TObject);
var
  LFileName: string;
begin
  inherited;
  LFileName := ExtractFileName(FileOpenAction.Dialog.FileName);
  if SameText(ExtractFileExt(LFileName), '.png') then
    LFileName := Copy(LFileName, 1, length(LFileName)-4);
  ImageNameEdit.Text := LFileName;
end;

function TViewNodeFrame.GetDefaultImageName: string;
var
  LControllerClass: TExtToolControllerClass;
  LDefaultImageName: string;
begin
  inherited;
  LControllerClass := GetToolControllerClass;
  if Assigned(LControllerClass) then
    Result := LControllerClass.GetDefaultImageName
  else
    Result := '';
end;

function TViewNodeFrame.GetToolControllerClass: TExtToolControllerClass;
var
  ControllerNode: TEFNode;
  ControllerClass: TClass;
begin
  ControllerNode := EditNode.FindNode('Controller');
  if Assigned(ControllerNode) then
  begin
    ControllerClass := GetClass(ControllerNode.AsString);
    Result := TExtToolControllerClass(ControllerClass);
  end;
end;

procedure TViewNodeFrame.ImageNameEditChange(Sender: TObject);
begin
  inherited;
  ShowImage(ImageNameEdit.Text, ImageNameImage);
end;

class function TViewNodeFrame.SuitsNode(const ANode: TEFNode): Boolean;
var
  ControllerNode: TEFNode;
  ControllerClass: TClass;
begin
  Result := False;
  ControllerNode := ANode.FindNode('Controller');
  if Assigned(ControllerNode) then
  begin
    ControllerClass := GetClass(ControllerNode.AsString);
    Result := Assigned(ControllerClass) and ControllerClass.InheritsFrom(TKExtToolController);
  end;
end;

procedure TViewNodeFrame.UpdateDesignPanel(
  const AForce: Boolean);
begin
  inherited;
  ImageNameEdit.Text := EditNode.GetString('ImageName', GetDefaultImageName);
end;

initialization
  TEditNodeFrameRegistry.Instance.RegisterClass(TViewNodeFrame.GetClassId, TViewNodeFrame);

finalization
  TEditNodeFrameRegistry.Instance.UnregisterClass(TViewNodeFrame.GetClassId);

end.
