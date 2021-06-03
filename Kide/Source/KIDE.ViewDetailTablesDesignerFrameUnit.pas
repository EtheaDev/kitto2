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
unit KIDE.ViewDetailTablesDesignerFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, KIDE.MetadataItemDesignerFrameUnit, System.Actions,
  KIDE.EditNodeBaseFrameUnit, Vcl.ActnList, Vcl.ExtCtrls,
  Kitto.Metadata.DataView, EF.Tree, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.Buttons, Vcl.StdActns,
  Vcl.Samples.Spin;

type
  TViewDetailTablesDesignerFrame = class(TMetadataItemDesignerFrame)
    DetailTablesPageControl: TPageControl;
    DetailTableTabSheet: TTabSheet;
    FileOpenAction: TFileOpen;
    ControllerGroupBox: TGroupBox;
    ControllerStyleComboBox: TComboBox;
    StyleLabel: TLabel;
    StyleHeightLabel: TLabel;
    _Controller_Style_Height: TSpinEdit;
  private
  protected
    class function SuitsNode(const ANode: TEFNode): Boolean; override;
    procedure CleanupDefaultsToEditNode; override;
    procedure UpdateDesignPanel(const AForce: Boolean = False); override;
    procedure DesignPanelToEditNode; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Init(const ANode: TEFTree); override;
  end;

implementation

{$R *.dfm}

uses
  KIDE.Config, KIDE.Project, KIDE.Utils,
  Kitto.Ext.Form, Kitto.Metadata.Models;

{ TViewDetailTablesDesignerFrame }

constructor TViewDetailTablesDesignerFrame.Create(AOwner: TComponent);
begin
  inherited;
  TKideConfig.Instance.Config.GetNode('DetailTablesControllerStyle').GetChildValues(ControllerStyleComboBox.Items);
end;

procedure TViewDetailTablesDesignerFrame.UpdateDesignPanel(const AForce: Boolean);
var
  LStyle: string;
begin
  inherited;
  LStyle := EditNode.GetString('Controller/Style', DEFAULT_DETAIL_STYLE);
  ControllerStyleComboBox.ItemIndex := ControllerStyleComboBox.Items.IndexOf(LStyle);
  _Controller_Style_Height.Value := EditNode.GetInteger('Controller/Style/Height', DEFAULT_DETAIL_PANEL_HEIGHT);
end;

procedure TViewDetailTablesDesignerFrame.DesignPanelToEditNode;
begin
  inherited;
  EditNode.FindNode('Controller', True);
  EditNode.SetString('Controller/Style', ControllerStyleComboBox.Text);
end;

procedure TViewDetailTablesDesignerFrame.CleanupDefaultsToEditNode;
begin
  inherited;
  CleanupIntegerNode('Controller/Style/Height', DEFAULT_DETAIL_PANEL_HEIGHT);
  if not Assigned(EditNode.FindNode('Controller/Style/Height')) then
  begin
    CleanupTextNode('Controller/Style', DEFAULT_DETAIL_STYLE);
  end;
  CleanupOrphanNode('Controller');
end;

procedure TViewDetailTablesDesignerFrame.Init(const ANode: TEFTree);
var
  LTabSheet: TTabSheet;
  LItemsNode, LItemNode: TEFNode;
  I: Integer;
  LFrameClass: TEditNodeBaseFrameClass;
begin
  inherited;
  //Clear all pages
  while DetailTablesPageControl.PageCount > 0 do
    DetailTablesPageControl.Pages[0].Free;
  //bUILD pages based on Filter Items
  LItemsNode := ANode as TEFNode;
  if Assigned(LItemsNode) then
  begin
    for I := 0 to LItemsNode.ChildCount -1 do
    begin
      LItemNode := LItemsNode.Children[I];
      if SameText(LItemNode.Name, 'Table') then
      begin
        LTabSheet := TTabSheet.Create(Self);
        LTabSheet.Name := Format('%s_%d', [LItemNode.Name, I]);
        LTabSheet.PageControl := DetailTablesPageControl;
        LTabSheet.Caption := LItemNode.Name;
        LFrameClass := TEditNodeFrameFactory.Instance.GetEditNodeFrameClass(LItemNode);
        if Assigned(LFrameClass) then
          EmbedEditNodeFrame(LTabSheet, LFrameClass, LItemNode);
      end;
    end;
  end;
end;

class function TViewDetailTablesDesignerFrame.SuitsNode(const ANode: TEFNode): Boolean;
begin
  Result := SameText(ANode.Name, 'DetailTables') and (ANode.Parent is TKViewTable);
end;

initialization
  TEditNodeFrameRegistry.Instance.RegisterClass(TViewDetailTablesDesignerFrame.GetClassId, TViewDetailTablesDesignerFrame);

finalization
  if Assigned(TEditNodeFrameRegistry.Instance) then
    TEditNodeFrameRegistry.Instance.UnregisterClass(TViewDetailTablesDesignerFrame.GetClassId);

end.
