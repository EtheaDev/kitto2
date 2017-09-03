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
unit KIDE.ListControllerButtonListFilterItemsFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, System.Actions,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Graphics, Vcl.ComCtrls,
  Vcl.ActnList, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.Samples.Spin,
  EF.Tree,
  KIDE.EditNodeBaseFrameUnit, KIDE.ListControllerButtonListFilterNodeFrameUnit,
  KIDE.ListControllerButtonListFilterItemFrameUnit;

type
  TListControllerButtonListFilterItemsFrame = class(TEditNodeBaseFrame)
    ItemsTabControl: TTabControl;
    procedure ItemsTabControlChange(Sender: TObject);
  private
    ListFilterItemFrame: TListControllerButtonListFilterItemFrame;
  protected
    class function SuitsNode(const ANode: TEFNode): Boolean; override;
    procedure UpdateDesignPanel(const AForce: Boolean = False); override;
  public
    class function IsListControllerButtonListFilterItemsNode(const ANode: TEFNode): Boolean;
    procedure Init(const ANode: TEFTree); override;
  end;

implementation

{$R *.dfm}

uses
  Kitto.Ext.Filters,
  KIDE.ListControllerFiltersNodeFrameUnit;

{ TListControllerButtonListFilterNodeFrame }

procedure TListControllerButtonListFilterItemsFrame.Init(const ANode: TEFTree);
begin
  inherited;
  if ANode.ChildCount > 0 then
    ListFilterItemFrame := EmbedEditNodeFrame(ItemsTabControl, TListControllerButtonListFilterItemFrame,
      ANode.Children[0]) as TListControllerButtonListFilterItemFrame;
end;

class function TListControllerButtonListFilterItemsFrame.IsListControllerButtonListFilterItemsNode(
  const ANode: TEFNode): Boolean;
begin
  Result := SameText(ANode.Name, 'Items') and (ANode.Parent is TEFNode) and
    TListControllerButtonListFilterNodeFrame.IsListControllerButtonListFilterNode(TEFNode(ANode.Parent));
end;

procedure TListControllerButtonListFilterItemsFrame.ItemsTabControlChange(Sender: TObject);
var
  LNodeName: string;
  LNode: TEFNode;
begin
  inherited;
  LNodeName := ItemsTabControl.Tabs[ItemsTabControl.TabIndex];
  LNode := EditNode.FindNode(LNodeName);
  ListFilterItemFrame.Init(LNode);
  ListFilterItemFrame.UpdateDesignPanel;
end;

class function TListControllerButtonListFilterItemsFrame.SuitsNode(const ANode: TEFNode): Boolean;
begin
  Result := IsListControllerButtonListFilterItemsNode(ANode);
end;

procedure TListControllerButtonListFilterItemsFrame.UpdateDesignPanel(const AForce: Boolean);
var
  I: Integer;
begin
  inherited;
  ItemsTabControl.Tabs.Clear;
  for I := 0 to EditNode.ChildCount -1 do
    ItemsTabControl.Tabs.Add(EditNode.Children[I].Name);
  if ItemsTabControl.Tabs.Count > 0 then
    ItemsTabControlChange(ItemsTabControl);
end;

initialization
  TEditNodeFrameRegistry.Instance.RegisterClass(TListControllerButtonListFilterItemsFrame.GetClassId, TListControllerButtonListFilterItemsFrame);

finalization
  TEditNodeFrameRegistry.Instance.UnregisterClass(TListControllerButtonListFilterItemsFrame.GetClassId);

end.
