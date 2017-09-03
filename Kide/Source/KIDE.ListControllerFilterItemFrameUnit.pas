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
unit KIDE.ListControllerFilterItemFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.Actions, Vcl.ActnList,
  Vcl.ExtCtrls, EF.Tree, Vcl.StdCtrls, SynEdit, SynHighlighterSQL,
  KIDE.EditNodeBaseFrameUnit;

type
  TListControllerFilterItemFrame = class(TEditNodeBaseFrame)
    LabelEdit: TLabeledEdit;
    ExpressionTemplateGroupBox: TGroupBox;
    _DefaultValue: TLabeledEdit;
  private
    FSynSQLSyn: TSynSQLSyn;
    FExpressionEdit: TSynEdit;
  protected
    class function SuitsNode(const ANode: TEFNode): Boolean; override;
    procedure CleanupDefaultsToEditNode; override;
    procedure DesignPanelToEditNode; override;
  public
    procedure UpdateDesignPanel(const AForce: Boolean = False); override;
    class function IsListControllerFilterItemNode(const ANode: TEFNode): Boolean;
    constructor Create(AOwner: TComponent); override;
    procedure Init(const ANode: TEFTree); override;
  end;

implementation

{$R *.dfm}
uses
  KIDE.Utils,
  Kitto.Ext.Filters,
  KIDE.ListControllerFiltersItemsNodeFrameUnit,
  KIDE.ListControllerFiltersNodeFrameUnit;

{ TListControllerListFilterItemFrame }

procedure TListControllerFilterItemFrame.CleanupDefaultsToEditNode;
begin
  inherited;
  CleanupTextNode('ExpressionTemplate');
  CleanupTextNode('DefaultValue');
end;

constructor TListControllerFilterItemFrame.Create(AOwner: TComponent);
begin
  inherited;
  FSynSQLSyn := TSynSQLSyn.Create(Self);
  FExpressionEdit := CreateSynEditor(Self, ExpressionTemplateGroupBox,
    '_ExpressionTemplate', FSynSQLSyn, Font.Size, EditorChange);
  FExpressionEdit.Gutter.Visible := False;
end;

procedure TListControllerFilterItemFrame.DesignPanelToEditNode;
begin
  inherited;
  TEFNode(EditNode).AsString := LabelEdit.Text;
end;

procedure TListControllerFilterItemFrame.Init(const ANode: TEFTree);
begin
  inherited;
end;

class function TListControllerFilterItemFrame.IsListControllerFilterItemNode(
  const ANode: TEFNode): Boolean;
var
  LClass: TClass;
begin
  Result := False;
  if (ANode.Parent is TEFNode) and
    SameText(TEFNode(ANode.Parent).Name, 'Items') and
    TListControllerFiltersItemsNodeFrame.IsListControllerFiltersItemsNode(TEFNode(ANode.Parent)) then
  begin
    LClass := TKExtFilterRegistry.Instance.FindClass(ANode.Name);
    Result := Assigned(LClass) and
      (LClass.InheritsFrom(TKFreeSearchFilter) or
       LClass.InheritsFrom(TKDateSearchFilter) or
       LClass.InheritsFrom(TKBooleanSearchFilter));
  end;
end;

class function TListControllerFilterItemFrame.SuitsNode(const ANode: TEFNode): Boolean;
begin
  Result := IsListControllerFilterItemNode(ANode);
end;

procedure TListControllerFilterItemFrame.UpdateDesignPanel(const AForce: Boolean);
begin
  LabelEdit.Text := '';
  inherited;
  LabelEdit.Text := TEFNode(EditNode).AsString;
end;

initialization
  TEditNodeFrameRegistry.Instance.RegisterClass(TListControllerFilterItemFrame.GetClassId, TListControllerFilterItemFrame);

finalization
  TEditNodeFrameRegistry.Instance.UnregisterClass(TListControllerFilterItemFrame.GetClassId);

end.
