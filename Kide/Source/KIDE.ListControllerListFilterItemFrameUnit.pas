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
unit KIDE.ListControllerListFilterItemFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.Actions, Vcl.ActnList,
  Vcl.ExtCtrls, EF.Tree, Vcl.StdCtrls,
  KIDE.EditNodeBaseFrameUnit,
  SynEdit, SynHighlighterSQL;

type
  TListControllerListFilterItemFrame = class(TEditNodeBaseFrame)
    LabelEdit: TLabeledEdit;
    _IsDefault: TCheckBox;
    ExpressionPanel: TPanel;
    ExpressionLabel: TLabel;
  private
    FSynSQLSyn: TSynSQLSyn;
    FExpressionEdit: TSynEdit;
  protected
    class function SuitsNode(const ANode: TEFNode): Boolean; override;
    procedure CleanupDefaultsToEditNode; override;
    procedure DesignPanelToEditNode; override;
  public
    procedure UpdateDesignPanel(const AForce: Boolean = False); override;
    constructor Create(AOwner: TComponent); override;
  end;

implementation

{$R *.dfm}
uses
  KIDE.Utils,
  Kitto.Ext.Filters,
  KIDE.ListControllerListFilterNodeFrameUnit;

{ TListControllerListFilterItemFrame }

procedure TListControllerListFilterItemFrame.CleanupDefaultsToEditNode;
begin
  inherited;
  CleanupBooleanNode('IsDefault');
  CleanupTextNode('Expression');
end;

constructor TListControllerListFilterItemFrame.Create(AOwner: TComponent);
begin
  inherited;
  FSynSQLSyn := TSynSQLSyn.Create(Self);
  FExpressionEdit := CreateSynEditor(Self, ExpressionPanel,
    '_Expression', FSynSQLSyn, Font.Size, EditorChange);
end;

procedure TListControllerListFilterItemFrame.DesignPanelToEditNode;
begin
  inherited;
  TEFNode(EditNode).AsString := LabelEdit.Text;
end;

class function TListControllerListFilterItemFrame.SuitsNode(const ANode: TEFNode): Boolean;
begin
  Result := (ANode.Parent is TEFNode) and
    SameText(TEFNode(ANode.Parent).Name, 'Items') and
    (TEFNode(ANode.Parent).Parent is TEFNode) and
    TListControllerListFilterNodeFrame.IsListControllerListFilterNode(TEFNode(TEFNode((ANode.Parent)).Parent));
end;

procedure TListControllerListFilterItemFrame.UpdateDesignPanel(const AForce: Boolean);
begin
  _IsDefault.Checked := False;
  LabelEdit.Text := '';
  inherited;
  LabelEdit.Text := TEFNode(EditNode).AsString;
end;

initialization
  TEditNodeFrameRegistry.Instance.RegisterClass(TListControllerListFilterItemFrame.GetClassId, TListControllerListFilterItemFrame);

finalization
  TEditNodeFrameRegistry.Instance.UnregisterClass(TListControllerListFilterItemFrame.GetClassId);

end.
