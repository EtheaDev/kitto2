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
unit KIDE.ListControllerListFilterBaseNodeFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, KIDE.EditNodeBaseFrameUnit, System.Actions, Vcl.ActnList,
  Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.Samples.Spin, Vcl.ComCtrls,
  EF.Tree,
  Kitto.Ext.Filters, Kitto.Ext.List, KIDE.PanelControllerDesignerFrameUnit;

type
  TListControllerListFilterBaseNodeFrame = class(TEditNodeBaseFrame)
    ListFilterGroupBox: TGroupBox;
    WidthLabel: TLabel;
    _Width: TSpinEdit;
    ListWidthLabel: TLabel;
    _ListWidth: TSpinEdit;
    AutoCompleteMinCharsLabel: TLabel;
    _AutoCompleteMinChars: TSpinEdit;
    LabelEdit: TLabeledEdit;
  private
  protected
    class function SuitsNode(const ANode: TEFNode): Boolean; override;
    procedure CleanupDefaultsToEditNode; override;
    procedure UpdateDesignPanel(const AForce: Boolean = False); override;
    procedure DesignPanelToEditNode; override;
  public
    class function IsListControllerListFilterBaseNode(const ANode: TEFNode): Boolean;
  end;

implementation

{$R *.dfm}

uses
  KIDE.Config,
  KIDE.ListControllerFiltersNodeFrameUnit,
  KIDE.ListControllerFiltersItemsNodeFrameUnit;

{ TListControllerFiltersNodeFrame }

procedure TListControllerListFilterBaseNodeFrame.CleanupDefaultsToEditNode;
begin
  inherited;
  CleanupIntegerNode('Width', DEFAULT_FILTER_WIDTH);
  CleanupIntegerNode('ListWidth', DEFAULT_FILTER_WIDTH);
  CleanupIntegerNode('AutoCompleteMinChars', 0);
end;

procedure TListControllerListFilterBaseNodeFrame.DesignPanelToEditNode;
begin
  inherited;
  TEFNode(EditNode).AsString := LabelEdit.Text;
end;

class function TListControllerListFilterBaseNodeFrame.IsListControllerListFilterBaseNode(const ANode: TEFNode): Boolean;
var
  LClass: TClass;
begin
  Result := False;
  if (ANode.Parent is TEFNode) and
    TListControllerFiltersItemsNodeFrame.IsListControllerFiltersItemsNode(TEFNode(ANode.Parent)) then
  begin
    LClass := TKExtFilterRegistry.Instance.FindClass(ANode.Name);
    Result := Assigned(LClass) and LClass.InheritsFrom(TKListFilterBase);
  end;
end;

class function TListControllerListFilterBaseNodeFrame.SuitsNode(const ANode: TEFNode): Boolean;
begin
  Result := IsListControllerListFilterBaseNode(ANode);
end;

procedure TListControllerListFilterBaseNodeFrame.UpdateDesignPanel(const AForce: Boolean);
begin
  inherited;
  _Width.Value := EditNode.GetInteger('Width', DEFAULT_FILTER_WIDTH);
  _ListWidth.Value := EditNode.GetInteger('ListWidth', DEFAULT_FILTER_WIDTH);
  _AutoCompleteMinChars.Value := EditNode.GetInteger('AutoCompleteMinChars', 0);
  LabelEdit.Text := TEFNode(EditNode).AsString;
end;

initialization
  TEditNodeFrameRegistry.Instance.RegisterClass(TListControllerListFilterBaseNodeFrame.GetClassId, TListControllerListFilterBaseNodeFrame);

finalization
  TEditNodeFrameRegistry.Instance.UnregisterClass(TListControllerListFilterBaseNodeFrame.GetClassId);

end.
