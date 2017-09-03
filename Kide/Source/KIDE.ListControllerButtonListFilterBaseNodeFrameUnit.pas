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
unit KIDE.ListControllerButtonListFilterBaseNodeFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, KIDE.EditNodeBaseFrameUnit, System.Actions, Vcl.ActnList,
  Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.Samples.Spin, Vcl.ComCtrls,
  EF.Tree,
  Kitto.Ext.Filters, Kitto.Ext.List, KIDE.PanelControllerDesignerFrameUnit;

type
  TListControllerButtonListFilterBaseNodeFrame = class(TEditNodeBaseFrame)
    ListFilterGroupBox: TGroupBox;
    LabelEdit: TLabeledEdit;
    _IsSingleSelect: TCheckBox;
    ConnectorRadioGroup: TRadioGroup;
    ButtonScaleComboBox: TComboBox;
    ButtonScaleLabel: TLabel;
  private
  protected
    class function SuitsNode(const ANode: TEFNode): Boolean; override;
    procedure CleanupDefaultsToEditNode; override;
    procedure UpdateDesignPanel(const AForce: Boolean = False); override;
    procedure DesignPanelToEditNode; override;
  public
    class function IsListControllerButtonListFilterBaseNode(const ANode: TEFNode): Boolean;
    constructor Create(AOwner: TComponent); override;
  end;

implementation

{$R *.dfm}

uses
  KIDE.Config,
  KIDE.ListControllerFiltersNodeFrameUnit,
  KIDE.ListControllerFiltersItemsNodeFrameUnit;

{ TListControllerFiltersNodeFrame }

procedure TListControllerButtonListFilterBaseNodeFrame.CleanupDefaultsToEditNode;
begin
  inherited;
  CleanupBooleanNode('IsSingleSelect');
  CleanupTextNode('Connector', 'or');
  CleanupTextNode('ButtonScale', 'small');
end;

constructor TListControllerButtonListFilterBaseNodeFrame.Create(AOwner: TComponent);
begin
  inherited;
  TKideConfig.Instance.Config.GetNode('ButtonScales').GetChildValues(ButtonScaleComboBox.Items);
end;

procedure TListControllerButtonListFilterBaseNodeFrame.DesignPanelToEditNode;
begin
  inherited;
  TEFNode(EditNode).AsString := LabelEdit.Text;
  EditNode.SetString('Connector', ConnectorRadioGroup.Items[ConnectorRadioGroup.ItemIndex]);
  EditNode.SetString('ButtonScale' ,ButtonScaleComboBox.Text);
end;

class function TListControllerButtonListFilterBaseNodeFrame.IsListControllerButtonListFilterBaseNode(const ANode: TEFNode): Boolean;
var
  LClass: TClass;
begin
  Result := False;
  if (ANode.Parent is TEFNode) and
    TListControllerFiltersItemsNodeFrame.IsListControllerFiltersItemsNode(TEFNode(ANode.Parent)) then
  begin
    LClass := TKExtFilterRegistry.Instance.FindClass(ANode.Name);
    Result := Assigned(LClass) and LClass.InheritsFrom(TKButtonListFilterBase);
  end;
end;

class function TListControllerButtonListFilterBaseNodeFrame.SuitsNode(const ANode: TEFNode): Boolean;
begin
  Result := IsListControllerButtonListFilterBaseNode(ANode);
end;

procedure TListControllerButtonListFilterBaseNodeFrame.UpdateDesignPanel(const AForce: Boolean);
var
  LConnector, LButtonScale: string;
begin
  inherited;
  LabelEdit.Text := TEFNode(EditNode).AsString;
  LConnector := EditNode.GetString('Connector', 'or');
  ConnectorRadioGroup.ItemIndex := ConnectorRadioGroup.Items.IndexOf(LConnector);
  LButtonScale := EditNode.GetString('ButtonScale', 'small');
  ButtonScaleComboBox.ItemIndex := ButtonScaleComboBox.Items.IndexOf(LButtonScale);
end;

initialization
  TEditNodeFrameRegistry.Instance.RegisterClass(TListControllerButtonListFilterBaseNodeFrame.GetClassId, TListControllerButtonListFilterBaseNodeFrame);

finalization
  TEditNodeFrameRegistry.Instance.UnregisterClass(TListControllerButtonListFilterBaseNodeFrame.GetClassId);

end.
