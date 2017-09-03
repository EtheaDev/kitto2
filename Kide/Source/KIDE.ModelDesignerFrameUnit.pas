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
unit KIDE.ModelDesignerFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, KIDE.TreeDesignerFrameUnit,
  System.Actions, Vcl.ActnList, Vcl.ComCtrls, Vcl.ToolWin, KIDE.BaseFrameUnit,
  KIDE.CodeEditorFrameUnit, Vcl.ExtCtrls, Vcl.Tabs, Vcl.StdCtrls,
  EF.Tree, Kitto.metadata.Models, Vcl.Buttons, Vcl.StdActns,
  KIDE.PairsValuesFrameUnit, KIDE.EditNodeBaseFrameUnit;

type
  TModelDesignerFrame = class(TTreeDesignerFrame)
    EditorPageControl: TPageControl;
    ModelTabSheet: TTabSheet;
    RulesTabSheet: TTabSheet;
    DetailReferencesTabSheet: TTabSheet;
    FileOpenAction: TFileOpen;
    ModelScrollBox: TScrollBox;
    ApplicationGroupBox: TGroupBox;
    ImageNameSpeedButton: TSpeedButton;
    ImageNameImage: TImage;
    _ModelName: TLabeledEdit;
    _DisplayLabel: TLabeledEdit;
    _PluralDisplayLabel: TLabeledEdit;
    _PhysicalName: TLabeledEdit;
    _CaptionField: TLabeledEdit;
    _DatabaseRouter: TLabeledEdit;
    _IsLarge: TCheckBox;
    _IsReadOnly: TCheckBox;
    _DefaultSorting: TLabeledEdit;
    _ImageName: TLabeledEdit;
    ModelAutoScrollPanel: TPanel;
    _PluralModelName: TLabeledEdit;
    _DefaultFilter: TLabeledEdit;
    _DatabaseRouter_DatabaseName: TLabeledEdit;
    FieldsTabSheet: TTabSheet;
    procedure FileOpenActionAccept(Sender: TObject);
    procedure _ImageNameChange(Sender: TObject);
    procedure EditorPageControlChange(Sender: TObject);
  private
    function GetEditModel: TKModel;
  protected
    class function SuitsTree(const ATree: TEFTree): Boolean; override;
    procedure CleanupDefaultsToEditNode; override;
    procedure UpdateDesignPanel(const AForce: Boolean = False); override;
    procedure DesignPanelToEditNode; override;
    function GetDetailReferencesNode(const ACreateIfMissing: Boolean = False): TEFNode;
    property EditModel: TKModel read GetEditModel;
  public
    procedure Init(const ANode: TEFTree); override;
  end;

implementation

{$R *.dfm}

uses
  KIDE.Utils,
  KIDE.ModelFieldsDesignerFrameUnit, KIDE.ModelRulesNodeFrameUnit;

{ TModelDesignerFrame }

procedure TModelDesignerFrame.CleanupDefaultsToEditNode;
var
  LDatabaseRouter: TEFNode;
  LDefaultCaptionField: TKModelField;
begin
  inherited;
  LDatabaseRouter := EditNode.FindNode('DatabaseRouter');
  if Assigned(LDatabaseRouter) and (LDatabaseRouter.GetString('DatabaseName') = '') then
    LDatabaseRouter.DeleteNode('DatabaseName');
  CleanupTextNode('DatabaseRouter');
  CleanupTextNode('ImageName', EditModel.DefaultImageName);
  CleanupTextNode('PluralModelName', EditModel.DefaultPluralModelName);
  CleanupTextNode('DisplayLabel', EditModel.DefaultDisplayLabel);
  CleanupTextNode('PhysicalName', EditModel.DefaultPhysicalName);
  CleanupTextNode('PluralDisplayLabel', EditModel.DefaultPluralDisplayLabel);
  LDefaultCaptionField := EditModel.FindDefaultCaptionField;
  CleanupTextNode('CaptionField');
  if Assigned(LDefaultCaptionField) then
    CleanupTextNode('CaptionField', LDefaultCaptionField.FieldName);
  CleanupBooleanNode('IsLarge');
  CleanupBooleanNode('IsReadOnly');
  CleanupTextNode('DefaultSorting', EditModel.DefaultDefaultSorting);
  CleanupOrphanNode('DetailReferences');
  CleanupTextNode('DefaultFilter');
end;

procedure TModelDesignerFrame.DesignPanelToEditNode;
begin
  inherited;
  EditorPageControl.ActivePageIndex := 0;
//  DetailReferencesFrame.DesignPanelToEditNode(GetDetailReferencesNode, True);
end;

procedure TModelDesignerFrame.EditorPageControlChange(Sender: TObject);
begin
  inherited;
  if EditorPageControl.ActivePage = DetailReferencesTabSheet then
  begin
    EmbedEditNodeFrame(DetailReferencesTabSheet, TPairsValuesFrame,
      GetDetailReferencesNode(True), True);
  end
  else if EditorPageControl.ActivePage = FieldsTabSheet then
  begin
    EmbedEditNodeFrame(FieldsTabSheet, TModelFieldsDesignerFrame,
      EditNode.GetNode('Fields', True), True);
  end
  else if EditorPageControl.ActivePage = RulesTabSheet then
  begin
    EmbedEditNodeFrame(RulesTabSheet, TModelRulesNodeFrame,
      EditNode.GetNode('Rules', True), True);
  end;
end;

procedure TModelDesignerFrame.FileOpenActionAccept(Sender: TObject);
var
  LFileName: string;
begin
  inherited;
  LFileName := ExtractFileName(FileOpenAction.Dialog.FileName);
  if SameText(ExtractFileExt(LFileName), '.png') then
    LFileName := Copy(LFileName, 1, length(LFileName)-4);
  _ImageName.Text := LFileName;
end;

function TModelDesignerFrame.GetDetailReferencesNode(
  const ACreateIfMissing: Boolean): TEFNode;
begin
  Assert(Assigned(EditNode));
  Result := EditNode.FindNode('DetailReferences', ACreateIfMissing);
end;

function TModelDesignerFrame.GetEditModel: TKModel;
begin
  Result := EditNode as TKModel;
end;

class function TModelDesignerFrame.SuitsTree(const ATree: TEFTree): Boolean;
begin
  Result := ATree is TKModel;
end;

procedure TModelDesignerFrame.UpdateDesignPanel(const AForce: Boolean);
var
  LCaptionField: TKModelField;
begin
  inherited;
  EditorPageControl.ActivePageIndex := 0;
  _PhysicalName.Text := EditModel.DBTableName;
  _DisplayLabel.Text := EditModel.DisplayLabel;
  _PluralDisplayLabel.Text := EditModel.PluralDisplayLabel;
  _PluralModelName.Text := EditModel.PluralModelName;
  LCaptionField := EditModel.FindCaptionField;
  if Assigned(LCaptionField) then
    _CaptionField.Text := LCaptionField.FieldName
  else
    _CaptionField.Text := '';
  _DefaultSorting.Text := EditModel.DefaultSorting;
  _ImageName.Text := EditModel.ImageName;
end;

procedure TModelDesignerFrame._ImageNameChange(Sender: TObject);
begin
  inherited;
  ShowImage(_ImageName.Text, ImageNameImage);
  IsChanged := True;
end;

procedure TModelDesignerFrame.Init(const ANode: TEFTree);
begin
  inherited;
end;

initialization
  TTreeDesignerFrameRegistry.Instance.RegisterClass(TModelDesignerFrame.GetClassId, TModelDesignerFrame);

finalization
  TTreeDesignerFrameRegistry.Instance.UnregisterClass(TModelDesignerFrame.GetClassId);

end.
