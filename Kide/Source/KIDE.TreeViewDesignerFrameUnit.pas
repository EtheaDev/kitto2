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
unit KIDE.TreeViewDesignerFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, KIDE.SubViewDesignerFrameUnit, Vcl.StdActns, System.Actions,
  Vcl.ActnList, Vcl.Tabs, KIDE.BaseFrameUnit, KIDE.CodeEditorFrameUnit, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.Buttons, Vcl.ComCtrls, Vcl.ToolWin,
  EF.Tree, KIDE.TreeDesignerFrameUnit, Kitto.Metadata.Views,
  KIDE.ViewDesignerFrameUnit;

type
  TTreeViewDesignerFrame = class(TSubViewDesignerFrame)
  private
    function GetTreeView: TKTreeView;
  protected
    class function SuitsTree(const ATree: TEFTree): Boolean; override;
    class function SuitsNode(const ANode: TEFNode): Boolean; override;
    procedure CleanupDefaultsToEditNode; override;
  public
    procedure Init(const ANode: TEFTree); override;
  end;

implementation

{$R *.dfm}

uses
  KIDE.EditNodeBaseFrameUnit;

{ TTreeViewDesignerFrame }

procedure TTreeViewDesignerFrame.CleanupDefaultsToEditNode;
begin
  inherited;
end;

function TTreeViewDesignerFrame.GetTreeView: TKTreeView;
begin
  Result := EditNode as TKTreeView;
end;

procedure TTreeViewDesignerFrame.Init(const ANode: TEFTree);
begin
  inherited;
  ControllerTabSheet.TabVisible := False;
  ControllerComboBox.Visible := False;
  ControllerLabel.Visible := False;
end;

class function TTreeViewDesignerFrame.SuitsTree(const ATree: TEFTree): Boolean;
begin
  Result := ATree is TKTreeView;
end;

class function TTreeViewDesignerFrame.SuitsNode(const ANode: TEFNode): Boolean;
begin
  Result := False;
end;

initialization
  TEditNodeFrameRegistry.Instance.RegisterClass(TTreeViewDesignerFrame.GetClassId, TTreeViewDesignerFrame);

finalization
  TEditNodeFrameRegistry.Instance.UnregisterClass(TTreeViewDesignerFrame.GetClassId);

end.
