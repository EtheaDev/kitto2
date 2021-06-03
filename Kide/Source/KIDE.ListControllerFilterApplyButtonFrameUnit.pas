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
unit KIDE.ListControllerFilterApplyButtonFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.Actions, Vcl.ActnList,
  Vcl.ExtCtrls, EF.Tree, Vcl.StdCtrls, SynEdit, SynHighlighterSQL,
  KIDE.EditNodeBaseFrameUnit, Vcl.Samples.Spin, Vcl.Buttons, Vcl.StdActns;

type
  TListControllerFilterApplyButtonFrame = class(TEditNodeBaseFrame)
    ImageNameEdit: TLabeledEdit;
    ImageNameSpeedButton: TSpeedButton;
    ImageNameImage: TImage;
    ImageNameImageLarge: TImage;
    FileOpenAction: TFileOpen;
    LabelEdit: TLabeledEdit;
    procedure ImageNameEditChange(Sender: TObject);
    procedure FileOpenActionAccept(Sender: TObject);
  private
  protected
    class function SuitsNode(const ANode: TEFNode): Boolean; override;
    procedure CleanupDefaultsToEditNode; override;
    procedure UpdateDesignPanel(const AForce: Boolean = False); override;
    procedure DesignPanelToEditNode; override;
  public
  end;

implementation

{$R *.dfm}
uses
  KIDE.Utils,
  Kitto.Ext.Filters,
  KIDE.ListControllerFiltersItemsNodeFrameUnit,
  KIDE.ListControllerFiltersNodeFrameUnit;

{ TListControllerFilterApplyButtonFrame }

procedure TListControllerFilterApplyButtonFrame.CleanupDefaultsToEditNode;
begin
  inherited;
  if TEFNode(EditNode).AsString = 'Apply' then
    TEFNode(EditNode).Value := '';
  CleanupTextNode('ImageName');
end;

procedure TListControllerFilterApplyButtonFrame.DesignPanelToEditNode;
begin
  inherited;
  TEFNode(EditNode).AsString := LabelEdit.Text;
end;

procedure TListControllerFilterApplyButtonFrame.FileOpenActionAccept(
  Sender: TObject);
var
  LFileName: string;
begin
  inherited;
  LFileName := ExtractFileName(FileOpenAction.Dialog.FileName);
  if SameText(ExtractFileExt(LFileName), '.png') then
    LFileName := Copy(LFileName, 1, length(LFileName)-4);
  ImageNameEdit.Text := LFileName;
end;

procedure TListControllerFilterApplyButtonFrame.ImageNameEditChange(
  Sender: TObject);
begin
  inherited;
  ShowImage(ImageNameEdit.Text+'_large', ImageNameImageLarge);
  ShowImage(ImageNameEdit.Text, ImageNameImage);
end;

class function TListControllerFilterApplyButtonFrame.SuitsNode(const ANode: TEFNode): Boolean;
begin
  Result := SameText(ANode.Name, 'ApplyButton') and
    (ANode.Parent is TEFNode) and
    SameText(TEFNode(ANode.Parent).Name, 'Items') and
    TListControllerFiltersItemsNodeFrame.IsListControllerFiltersItemsNode(TEFNode(ANode.Parent));
end;

procedure TListControllerFilterApplyButtonFrame.UpdateDesignPanel(
  const AForce: Boolean);
begin
  LabelEdit.Text := '';
  inherited;
  if SameText(TEFNode(EditNode).AsString, '') then
    LabelEdit.Text := 'Apply'
  else
    LabelEdit.Text := TEFNode(EditNode).AsString;
  ImageNameEdit.Text := EditNode.GetString('ImageName');



end;

initialization
  TEditNodeFrameRegistry.Instance.RegisterClass(TListControllerFilterApplyButtonFrame.GetClassId, TListControllerFilterApplyButtonFrame);

finalization
  if Assigned(TEditNodeFrameRegistry.Instance) then
    TEditNodeFrameRegistry.Instance.UnregisterClass(TListControllerFilterApplyButtonFrame.GetClassId);

end.
