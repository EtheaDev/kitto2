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
unit KIDE.ListControllerFiltersItemsNodeFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, KIDE.EditNodeBaseFrameUnit, System.Actions, Vcl.ActnList,
  Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.Samples.Spin, Vcl.ComCtrls,
  EF.Tree,
  Kitto.Ext.Filters, Kitto.Ext.List;

type
  TListControllerFiltersItemsNodeFrame = class(TEditNodeBaseFrame)
    ItemsPageControl: TPageControl;
    ItemTabSheet: TTabSheet;
  private
  protected
    class function SuitsNode(const ANode: TEFNode): Boolean; override;
    procedure UpdateDesignPanel(const AForce: Boolean = False); override;
    procedure DesignPanelToEditNode; override;
  public
    class function IsListControllerFiltersItemsNode(const ANode: TEFNode): Boolean;
    constructor Create(AOwner: TComponent); override;
    procedure Init(const ANode: TEFTree); override;
  end;

implementation

{$R *.dfm}

uses
  KIDE.Config,
  KIDE.ListControllerFiltersNodeFrameUnit;

{ TListControllerFiltersNodeFrame }

constructor TListControllerFiltersItemsNodeFrame.Create(AOwner: TComponent);
begin
  inherited;
end;

procedure TListControllerFiltersItemsNodeFrame.DesignPanelToEditNode;
begin
  inherited;
end;

procedure TListControllerFiltersItemsNodeFrame.Init(const ANode: TEFTree);
var
  I: integer;
  LItemsNode, LItemNode: TEFNode;
  LTabSheet: TTabSheet;
  LFrameClass: TEditNodeBaseFrameClass;
begin
  inherited;
  //Clear all pages
  while ItemsPageControl.PageCount > 0 do
    ItemsPageControl.Pages[0].Free;
  //bUILD pages based on Filter Items
  LItemsNode := ANode as TEFNode;
  if Assigned(LItemsNode) then
  begin
    for I := 0 to LItemsNode.ChildCount -1 do
    begin
      LItemNode := LItemsNode.Children[I];
      LTabSheet := TTabSheet.Create(Self);
      LTabSheet.Name := Format('%s_%d', [LItemNode.Name, I]);
      LTabSheet.PageControl := ItemsPageControl;
      LTabSheet.Caption := LItemNode.Name;
      LFrameClass := TEditNodeFrameFractory.Instance.GetEditNodeFrameClass(LItemNode);
      if Assigned(LFrameClass) then
        EmbedEditNodeFrame(LTabSheet, LFrameClass, LItemNode);
    end;
  end;
end;

class function TListControllerFiltersItemsNodeFrame.IsListControllerFiltersItemsNode(
  const ANode: TEFNode): Boolean;
begin
  Result := (ANode is TEFNode) and SameText(TEFNode(ANode).Name, 'Items') and
    (TEFNode(ANode).Parent is TEFNode) and
    TListControllerFiltersNodeFrame.IsListControllerFilterNode(TEFNode(TEFNode(ANode).Parent));
end;

class function TListControllerFiltersItemsNodeFrame.SuitsNode(const ANode: TEFNode): Boolean;
begin
  Result := IsListControllerFiltersItemsNode(ANode);
end;

procedure TListControllerFiltersItemsNodeFrame.UpdateDesignPanel(const AForce: Boolean);
begin
  inherited;
end;

initialization
  TEditNodeFrameRegistry.Instance.RegisterClass(TListControllerFiltersItemsNodeFrame.GetClassId, TListControllerFiltersItemsNodeFrame);

finalization
  TEditNodeFrameRegistry.Instance.UnregisterClass(TListControllerFiltersItemsNodeFrame.GetClassId);

end.
