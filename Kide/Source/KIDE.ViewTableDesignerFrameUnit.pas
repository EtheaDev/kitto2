{*******************************************************************}
{                                                                   }
{   Kide2 Editor: GUI for Kitto2                                    }
{                                                                   }
{   Copyright (c) 2012-2017 Ethea S.r.l.                            }
{   ALL RIGHTS RESERVED / TUTTI I DIRITTI RISERVATI                 }
{                                                                   }
{*******************************************************************}
{                                                                   }
{   The entire contents of this file is protected by                }
{   International Copyright Laws. Unauthorized reproduction,        }
{   reverse-engineering, and distribution of all or any portion of  }
{   the code contained in this file is strictly prohibited and may  }
{   result in severe civil and criminal penalties and will be       }
{   prosecuted to the maximum extent possible under the law.        }
{                                                                   }
{   RESTRICTIONS                                                    }
{                                                                   }
{   THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED      }
{   FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE        }
{   COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE       }
{   AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT  }
{   AND PERMISSION FROM ETHEA S.R.L.                                }
{                                                                   }
{   CONSULT THE END USER LICENSE AGREEMENT FOR INFORMATION ON       }
{   ADDITIONAL RESTRICTIONS.                                        }
{                                                                   }
{*******************************************************************}
{                                                                   }
{   Il contenuto di questo file è protetto dalle leggi              }
{   internazionali sul Copyright. Sono vietate la riproduzione, il  }
{   reverse-engineering e la distribuzione non autorizzate di tutto }
{   o parte del codice contenuto in questo file. Ogni infrazione    }
{   sarà perseguita civilmente e penalmente a termini di legge.     }
{                                                                   }
{   RESTRIZIONI                                                     }
{                                                                   }
{   SONO VIETATE, SENZA IL CONSENSO SCRITTO DA PARTE DI             }
{   ETHEA S.R.L., LA COPIA, LA VENDITA, LA DISTRIBUZIONE E IL       }
{   TRASFERIMENTO A TERZI, A QUALUNQUE TITOLO, DEL CODICE SORGENTE  }
{   CONTENUTO IN QUESTO FILE E ALTRI FILE AD ESSO COLLEGATI.        }
{                                                                   }
{   SI FACCIA RIFERIMENTO ALLA LICENZA D'USO PER INFORMAZIONI SU    }
{   EVENTUALI RESTRIZIONI ULTERIORI.                                }
{                                                                   }
{*******************************************************************}
unit KIDE.ViewTableDesignerFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, KIDE.MetadataItemDesignerFrameUnit, System.Actions,
  KIDE.EditNodeBaseFrameUnit, Vcl.ActnList, Vcl.ExtCtrls,
  Kitto.Metadata.DataView, EF.Tree, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.Buttons, Vcl.StdActns;

type
  TViewTableDesignerFrame = class(TMetadataItemDesignerFrame)
    ViewTablePageControl: TPageControl;
    ViewTableTabSheet: TTabSheet;
    DetailTablesTabSheet: TTabSheet;
    RulesTabSheet: TTabSheet;
    ModelLabel: TLabel;
    DisplayLabelEdit: TLabeledEdit;
    PluralDisplayLabelEdit: TLabeledEdit;
    DefaultSortingEdit: TLabeledEdit;
    DefaultFilterEdit: TLabeledEdit;
    ModelComboBox: TComboBox;
    _ImageName: TLabeledEdit;
    ImageNameSpeedButton: TSpeedButton;
    ImageNameImage: TImage;
    ImageNameImageLarge: TImage;
    IsReadOnlyCheckBox: TCheckBox;
    FileOpenAction: TFileOpen;
    _DatabaseRouter: TLabeledEdit;
    _DatabaseRouter_DatabaseName: TLabeledEdit;
    FieldsTabSheet: TTabSheet;
    IsLargeCheckBox: TCheckBox;
    procedure ModelComboBoxChange(Sender: TObject);
    procedure FileOpenActionAccept(Sender: TObject);
    procedure _ImageNameChange(Sender: TObject);
    procedure ViewTablePageControlChange(Sender: TObject);
  private
    function GetViewTable: TKViewTable;
    procedure UpdateModelValues;
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
  KIDE.Project, KIDE.Utils,
  KIDE.ViewDetailTablesDesignerFrameUnit,
  KIDE.ModelRulesNodeFrameUnit,
  Kitto.Metadata.Models,
  KIDE.ViewFieldsDesignerFrameUnit;

{ TViewTableDesignerFrame }

constructor TViewTableDesignerFrame.Create(AOwner: TComponent);
var
  I: Integer;
  LModel: TKModel;
begin
  inherited;
  for I := 0 to  TProject.CurrentProject.Config.Models.ModelCount - 1 do
  begin
    LModel := TProject.CurrentProject.Config.Models[I];
    ModelComboBox.Items.Add(LModel.ModelName);
  end;
end;

procedure TViewTableDesignerFrame.UpdateModelValues;
var
  LViewTable: TKViewTable;
  LReferencedModel: TKModel;
  LModelDetailReference: TKModelDetailReference;
begin
  LViewTable := GetViewTable;
  if LViewTable.HasModelName then
  begin
    if LViewTable.IsDetail then
    begin
      LModelDetailReference := LViewTable.ModelDetailReference;
      LReferencedModel := LModelDetailReference.Model;
    end;
    ModelComboBox.Text := LViewTable.ModelName;
    DisplayLabelEdit.Text := LViewTable.DisplayLabel;
    PluralDisplayLabelEdit.Text := LViewTable.PluralDisplayLabel;
    IsReadOnlyCheckBox.Checked := LViewTable.IsReadOnly;
    IsLargeCheckBox.Checked := LViewTable.IsLarge;
    DefaultSortingEdit.Text := LViewTable.DefaultSorting;
    DefaultFilterEdit.Text := LViewTable.DefaultFilter;
    _ImageName.Text := LViewTable.ImageName;
  end
  else
  begin
    ModelComboBox.Text := '';
    DisplayLabelEdit.Text := '';
    PluralDisplayLabelEdit.Text := '';
    IsReadOnlyCheckBox.Checked := False;
    IsLargeCheckBox.Checked := False;
    DefaultSortingEdit.Text := '';
    DefaultFilterEdit.Text := '';
    _ImageName.Text := '';
  end;
end;

procedure TViewTableDesignerFrame.ViewTablePageControlChange(Sender: TObject);
begin
  inherited;
  if ViewTablePageControl.ActivePage = DetailTablesTabSheet then
  begin
    EmbedEditNodeFrame(DetailTablesTabSheet, TViewDetailTablesDesignerFrame,
      EditNode.FindNode('DetailTables', True), True);
  end
  else if ViewTablePageControl.ActivePage = FieldsTabSheet then
  begin
    EmbedEditNodeFrame(FieldsTabSheet, TViewFieldsDesignerFrame,
      EditNode.GetNode('Fields', True), True);
  end
  else if ViewTablePageControl.ActivePage = RulesTabSheet then
  begin
    EmbedEditNodeFrame(RulesTabSheet, TModelRulesNodeFrame,
      EditNode.GetNode('Rules', True), True);
  end;
end;

procedure TViewTableDesignerFrame.UpdateDesignPanel(const AForce: Boolean);
begin
  inherited;
  ViewTablePageControl.ActivePageIndex := 0;
  UpdateModelValues;
end;

procedure TViewTableDesignerFrame._ImageNameChange(Sender: TObject);
begin
  inherited;
  ShowImage(_ImageName.Text, ImageNameImage);
  IsChanged := True;
end;

procedure TViewTableDesignerFrame.DesignPanelToEditNode;
begin
  inherited;
  ViewTablePageControl.ActivePageIndex := 0;
  EditNode.SetString('DisplayLabel', DisplayLabelEdit.Text);
  EditNode.SetString('PluralDisplayLabel', PluralDisplayLabelEdit.Text);
  EditNode.SetBoolean('IsReadOnly', IsReadOnlyCheckBox.Checked);
  EditNode.SetBoolean('IsLarge', IsLargeCheckBox.Checked);
  EditNode.SetString('DefaultSorting', DefaultSortingEdit.Text);
  EditNode.SetString('DefaultFilter', DefaultFilterEdit.Text);
end;

procedure TViewTableDesignerFrame.FileOpenActionAccept(Sender: TObject);
var
  LFileName: string;
begin
  inherited;
  LFileName := ExtractFileName(FileOpenAction.Dialog.FileName);
  if SameText(ExtractFileExt(LFileName), '.png') then
    LFileName := Copy(LFileName, 1, length(LFileName)-4);
  _ImageName.Text := LFileName;
end;

procedure TViewTableDesignerFrame.CleanupDefaultsToEditNode;
var
  LViewTable: TKViewTable;
begin
  inherited;
  LViewTable := GetViewTable;
  if Assigned(LViewTable) and Assigned(LViewTable.Model) then
  begin
    CleanupTextNode('DisplayLabel', LViewTable.GetDefaultDisplayLabel);
    CleanupTextNode('PluralDisplayLabel', LViewTable.Model.PluralDisplayLabel);
    if LViewTable.HasModelName then
    begin
      CleanupBooleanNode('IsReadOnly', LViewTable.Model.IsReadOnly);
      CleanupBooleanNode('IsLarge', LViewTable.Model.IsLarge);
      CleanupTextNode('DefaultSorting', LViewTable.Model.DefaultSorting);
      CleanupTextNode('ImageName', LViewTable.Model.ImageName);
      CleanupTextNode('DatabaseRouter', LViewTable.Model.GetString('DatabaseRouter'));
      CleanupTextNode('DatabaseName', LViewTable.Model.GetString('DatabaseRouter/DatabaseName'));
    end;
    CleanupOrphanNode('Fields');
    CleanupOrphanNode('DetailTables');
    CleanupOrphanNode('Rules');
  end;
  CleanupTextNode('DefaultFilter');
  CleanupTextNode('DatabaseRouter/DatabaseName');
  CleanupTextNode('DatabaseRouter');
end;

function TViewTableDesignerFrame.GetViewTable: TKViewTable;
begin
  Result := EditNode as TKViewTable;
end;

procedure TViewTableDesignerFrame.Init(const ANode: TEFTree);
begin
  inherited;
  DetailTablesTabSheet.TabVisible := Assigned(ANode.FindNode('DetailTables'));
end;

procedure TViewTableDesignerFrame.ModelComboBoxChange(Sender: TObject);
var
  LModelName: string;
begin
  inherited;
  LModelName := ModelComboBox.Text;
  if not SameText(EditNode.GetString('Model'), LModelName) then
  begin
    EditNode.SetString('Model', LModelName);
    UpdateModelValues;
  end;
  IsChanged := True;
end;

class function TViewTableDesignerFrame.SuitsNode(const ANode: TEFNode): Boolean;
begin
  Result := (ANode is TKViewTable);
  //and Assigned(ANode.FindNode('Model')) and Assigned(TKViewTable(ANode).Model);
end;

initialization
  TEditNodeFrameRegistry.Instance.RegisterClass(TViewTableDesignerFrame.GetClassId, TViewTableDesignerFrame);

finalization
  TEditNodeFrameRegistry.Instance.UnregisterClass(TViewTableDesignerFrame.GetClassId);

end.
