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
unit KIDE.TreeFolderNodeDesignerFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.Tabs,
  EF.Tree,
  KIDE.EFTreeFrameUnit, System.Actions, Vcl.ActnList, Vcl.ComCtrls, Vcl.ToolWin,
  KIDE.EditNodeBaseFrameUnit, Vcl.StdCtrls, KIDE.NodeDesignerFrameUnit, KIDE.BaseFrameUnit,
  KIDE.CodeEditorFrameUnit, Vcl.StdActns, Vcl.Buttons;

type
  TTreeFolderNodeDesignerFrame = class(TEditNodeBaseFrame)
    ValueEdit: TLabeledEdit;
    _IsInitiallyCollapsed: TCheckBox;
    ImageNameEdit: TLabeledEdit;
    ImageNameSpeedButton: TSpeedButton;
    ImageNameImage: TImage;
    ImageNameImageLarge: TImage;
    FileOpenAction: TFileOpen;
    procedure FileOpenActionAccept(Sender: TObject);
    procedure ImageNameEditChange(Sender: TObject);
  private
  strict protected
  protected
    procedure CleanupDefaultsToEditNode; override;
    procedure UpdateDesignPanel(const AForce: Boolean); override;
    procedure DesignPanelToEditNode; override;
  public
    class function SuitsNode(const ANode: TEFNode): Boolean; override;
    procedure Init(const ANode: TEFTree); override;
  end;

implementation

{$R *.dfm}

uses
  KIDE.Utils,
  Kitto.Metadata.Views;

{ TTreeFolderNodeDesignerFrame }

procedure TTreeFolderNodeDesignerFrame.CleanupDefaultsToEditNode;
begin
  inherited;
  CleanupBooleanNode('IsInitiallyCollapsed');
end;

procedure TTreeFolderNodeDesignerFrame.DesignPanelToEditNode;
begin
  inherited;
  TEFNode(EditNode).Value := ValueEdit.Text;
  EditNode.SetString('ImageName', ImageNameEdit.Text);
end;

procedure TTreeFolderNodeDesignerFrame.FileOpenActionAccept(Sender: TObject);
var
  LFileName: string;
begin
  inherited;
  LFileName := ExtractFileName(FileOpenAction.Dialog.FileName);
  if SameText(ExtractFileExt(LFileName), '.png') then
    LFileName := Copy(LFileName, 1, length(LFileName)-4);
  ImageNameEdit.Text := LFileName;
end;

procedure TTreeFolderNodeDesignerFrame.ImageNameEditChange(Sender: TObject);
begin
  if (ImageNameEdit.Text = '') then
    ImageNameEdit.Text := 'Folder';
  ShowImage( ImageNameEdit.Text+'_large', ImageNameImageLarge);
  ShowImage(ImageNameEdit.Text, ImageNameImage);
end;

procedure TTreeFolderNodeDesignerFrame.Init(const ANode: TEFTree);
begin
  inherited;
end;

class function TTreeFolderNodeDesignerFrame.SuitsNode(const ANode: TEFNode): Boolean;
begin
  Result := SameText(ANode.Name, 'Folder') and (ANode.Root is TKTreeView) and
    ((ANode.Parent is TKTreeView) or ((ANode.Parent is TEFNode) and SameText(TEFNode(ANode.Parent).Name, 'Folder')));
end;

procedure TTreeFolderNodeDesignerFrame.UpdateDesignPanel(const AForce: Boolean);
begin
  inherited;
  ValueEdit.Text := TEFNode(EditNode).Value;
  ImageNameEdit.Text := EditNode.GetString('ImageName', 'Folder');
end;

initialization
  TEditNodeFrameRegistry.Instance.RegisterClass(TTreeFolderNodeDesignerFrame.GetClassId, TTreeFolderNodeDesignerFrame);

finalization
  if Assigned(TEditNodeFrameRegistry.Instance) then
    TEditNodeFrameRegistry.Instance.UnregisterClass(TTreeFolderNodeDesignerFrame.GetClassId);

end.
