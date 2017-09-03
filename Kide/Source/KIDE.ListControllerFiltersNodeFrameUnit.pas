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
unit KIDE.ListControllerFiltersNodeFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, KIDE.EditNodeBaseFrameUnit, System.Actions, Vcl.ActnList,
  Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.Samples.Spin, Vcl.ComCtrls,
  EF.Tree,
  Kitto.Ext.Filters, Kitto.Ext.List;

type
  TListControllerFiltersNodeFrame = class(TEditNodeBaseFrame)
    FilterPageControl: TPageControl;
    FilterPanelTabSheet: TTabSheet;
    _ColumnWidth: TSpinEdit;
    ColumnWidthLabel: TLabel;
    LabelWidthLabel: TLabel;
    _LabelWidth: TSpinEdit;
    LabelAlignLabel: TLabel;
    LabelAlignComboBox: TComboBox;
    _DisplayLabel: TLabeledEdit;
    ItemsTabSheet: TTabSheet;
    ConnectorRadioGroup: TRadioGroup;
  private
  protected
    class function SuitsNode(const ANode: TEFNode): Boolean; override;
    procedure CleanupDefaultsToEditNode; override;
    procedure UpdateDesignPanel(const AForce: Boolean = False); override;
    procedure DesignPanelToEditNode; override;
    procedure UpdateEditComponents; override;
  public
    class function IsListControllerFilterNode(const ANode: TEFNode): Boolean;
    constructor Create(AOwner: TComponent); override;
    procedure Init(const ANode: TEFTree); override;
  end;

implementation

{$R *.dfm}

uses
  KIDE.ListControllerFiltersItemsNodeFrameUnit,
  KIDE.Config;

{ TListControllerFiltersNodeFrame }

procedure TListControllerFiltersNodeFrame.CleanupDefaultsToEditNode;
begin
  inherited;
  CleanupTextNode('DisplayLabel');
  CleanupIntegerNode('LabelWidth');
  CleanupIntegerNode('ColumnWidth');
  CleanupTextNode('LabelAlign', 'Right');
  CleanupTextNode('Connector', 'or');
end;

constructor TListControllerFiltersNodeFrame.Create(AOwner: TComponent);
begin
  inherited;
  TKideConfig.Instance.Config.GetNode('LabelAligns').GetChildValues(LabelAlignComboBox.Items);
end;

procedure TListControllerFiltersNodeFrame.DesignPanelToEditNode;
begin
  inherited;
  FilterPageControl.ActivePageIndex := 0;
  EditNode.SetString('LabelAlign' ,LabelAlignComboBox.Text);
  EditNode.SetString('Connector', ConnectorRadioGroup.Items[ConnectorRadioGroup.ItemIndex]);
end;

procedure TListControllerFiltersNodeFrame.Init(const ANode: TEFTree);
begin
  inherited;
  EmbedEditNodeFrame(ItemsTabSheet, TListControllerFiltersItemsNodeFrame,
    ANode.FindNode('Items'));
end;

class function TListControllerFiltersNodeFrame.IsListControllerFilterNode(
  const ANode: TEFNode): Boolean;
var
  LClass: TClass;
begin
  Result := False;
  if SameText(ANode.Name, 'Filters') and
    Assigned(ANode.Parent) and (ANode.Parent is TEFNode) then
  begin
    LClass := GetControllerClass(TEFNode(ANode.Parent));
    Result := Assigned(LClass) and LClass.InheritsFrom(TKExtListPanelController);
  end;
end;

class function TListControllerFiltersNodeFrame.SuitsNode(const ANode: TEFNode): Boolean;
begin
  Result := IsListControllerFilterNode(ANode);
end;

procedure TListControllerFiltersNodeFrame.UpdateDesignPanel(const AForce: Boolean);
var
  LLabelAlign, LConnector: string;
begin
  inherited;
  FilterPageControl.ActivePageIndex := 0;
  LLabelAlign := EditNode.GetString('LabelAlign', 'Right');
  LabelAlignComboBox.ItemIndex := LabelAlignComboBox.Items.IndexOf(LLabelAlign);
  LConnector := EditNode.GetString('Connector', 'or');
  ConnectorRadioGroup.ItemIndex := ConnectorRadioGroup.Items.IndexOf(LConnector);
end;

procedure TListControllerFiltersNodeFrame.UpdateEditComponents;
begin
  inherited;
end;

initialization
  TEditNodeFrameRegistry.Instance.RegisterClass(TListControllerFiltersNodeFrame.GetClassId, TListControllerFiltersNodeFrame);

finalization
  TEditNodeFrameRegistry.Instance.UnregisterClass(TListControllerFiltersNodeFrame.GetClassId);

end.
