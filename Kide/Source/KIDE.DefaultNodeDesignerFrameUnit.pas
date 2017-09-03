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
unit KIDE.DefaultNodeDesignerFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.Tabs,
  EF.Tree,
  KIDE.TreeDesignerFrameUnit, KIDE.BaseFrameUnit, KIDE.CodeEditorFrameUnit,
  KIDE.EFTreeFrameUnit, System.Actions, Vcl.ActnList, Vcl.ComCtrls, Vcl.ToolWin,
  KIDE.NodeDesignerFrameUnit, KIDE.DefaultTreeDesignerFrameUnit, Vcl.StdCtrls;

type
  TDefaultNodeDesignerFrame = class(TNodeDesignerFrame)
  private
    FTreeFrame: TEFTreeFrame;
  strict protected
    procedure UpdateDesignPanel(const AForce: Boolean); override;
  protected
  public
    procedure AfterConstruction; override;
    procedure Init(const ANode: TEFTree); override;
  end;

implementation

{$R *.dfm}

uses
  KIDE.SimpleNodeDesignerFrameUnit;

{ TDefaultNodeDesignerFrame }

procedure TDefaultNodeDesignerFrame.AfterConstruction;
begin
  inherited;
end;

procedure TDefaultNodeDesignerFrame.Init(const ANode: TEFTree);
begin
  inherited;
  if (ANode is TEFNode) and TSimpleNodeDesignerFrame.SuitsNode(TEFNode(ANode)) then
  begin
    EmbedEditNodeFrame(DesignPanel, TSimpleNodeDesignerFrame, TEFNode(ANode));
  end
  else
  begin
    if not Assigned(FTreeFrame) then
    begin
      FTreeFrame := TEFTreeFrame.Create(Self);
      FTreeFrame.ShowAllNodes := True;
      FTreeFrame.Parent := DesignPanel;
      FTreeFrame.Align := alClient;
    end;
  end;
end;

procedure TDefaultNodeDesignerFrame.UpdateDesignPanel(const AForce: Boolean);
begin
  inherited;
  if Assigned(FTreeFrame) then
    FTreeFrame.EFTree := EditNode;
end;

end.
