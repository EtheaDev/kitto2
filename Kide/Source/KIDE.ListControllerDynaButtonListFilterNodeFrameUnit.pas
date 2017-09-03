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
unit KIDE.ListControllerDynaButtonListFilterNodeFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, System.Actions,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Graphics, Vcl.ComCtrls,
  Vcl.ActnList, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.Samples.Spin,
  EF.Tree,
  KIDE.EditNodeBaseFrameUnit, KIDE.ListControllerButtonListFilterBaseNodeFrameUnit,
  SynEdit, SynHighlighterSQL, KIDE.ListControllerListFilterBaseNodeFrameUnit;


type
  TListControllerDynaButtonListFilterNodeFrame = class(TListControllerButtonListFilterBaseNodeFrame)
    _ExpressionTemplate: TLabeledEdit;
    CommandTextPanel: TPanel;
    CommandTextLabel: TLabel;
  private
    FSynSQLSyn: TSynSQLSyn;
    FCommandTextEdit: TSynEdit;
  protected
    class function SuitsNode(const ANode: TEFNode): Boolean; override;
    procedure CleanupDefaultsToEditNode; override;
    procedure UpdateDesignPanel(const AForce: Boolean = False); override;
    procedure DesignPanelToEditNode; override;
  public
    class function IsListControllerDynaButtonListFilterNode(const ANode: TEFNode): Boolean;
    constructor Create(AOwner: TComponent); override;
  end;

implementation

{$R *.dfm}

uses
  KIDE.Utils,
  Kitto.Ext.Filters,
  KIDE.ListControllerFiltersNodeFrameUnit,
  KIDE.ListControllerFiltersItemsNodeFrameUnit,
  KIDE.ListControllerListFilterItemsFrameUnit;

{ TListControllerListFilterNodeFrame }

procedure TListControllerDynaButtonListFilterNodeFrame.CleanupDefaultsToEditNode;
begin
  inherited;
  CleanupTextNode('ExpressionTemplate');
  CleanupTextNode('CommandText');
end;

constructor TListControllerDynaButtonListFilterNodeFrame.Create(AOwner: TComponent);
begin
  inherited;
  FSynSQLSyn := TSynSQLSyn.Create(Self);
  FCommandTextEdit := CreateSynEditor(Self, CommandTextPanel,
    '_CommandText', FSynSQLSyn, Font.Size, EditorChange);
end;

procedure TListControllerDynaButtonListFilterNodeFrame.DesignPanelToEditNode;
begin
  inherited;
  ;
end;

class function TListControllerDynaButtonListFilterNodeFrame.IsListControllerDynaButtonListFilterNode(
  const ANode: TEFNode): Boolean;
var
  LClass: TClass;
begin
  Result := False;
  if (ANode.Parent is TEFNode) and
    TListControllerFiltersItemsNodeFrame.IsListControllerFiltersItemsNode(TEFNode(ANode.Parent)) then
  begin
    LClass := TKExtFilterRegistry.Instance.FindClass(ANode.Name);
    Result := Assigned(LClass) and LClass.InheritsFrom(TKDynaButtonListFilter);
  end;
end;

class function TListControllerDynaButtonListFilterNodeFrame.SuitsNode(const ANode: TEFNode): Boolean;
begin
  Result := IsListControllerDynaButtonListFilterNode(ANode);
end;

procedure TListControllerDynaButtonListFilterNodeFrame.UpdateDesignPanel(const AForce: Boolean);
begin
  inherited;
end;

initialization
  TEditNodeFrameRegistry.Instance.RegisterClass(TListControllerDynaButtonListFilterNodeFrame.GetClassId, TListControllerDynaButtonListFilterNodeFrame);

finalization
  TEditNodeFrameRegistry.Instance.UnregisterClass(TListControllerDynaButtonListFilterNodeFrame.GetClassId);

end.
