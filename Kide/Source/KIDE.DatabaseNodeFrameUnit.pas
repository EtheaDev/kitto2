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
unit KIDE.DatabaseNodeFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, KIDE.EditNodeBaseFrameUnit, System.Actions, Vcl.ActnList,
  Vcl.ExtCtrls, Vcl.StdCtrls, KIDE.BaseFrameUnit, KIDE.DatabaseFrameUnit,
  EF.Tree;

type
  TDatabaseNodeFrame = class(TEditNodeBaseFrame)
    ConnectionTypeEdit: TLabeledEdit;
    ConnectionGroupBox: TGroupBox;
    ConnectionNameEdit: TLabeledEdit;
  private
  protected
    class function SuitsNode(const ANode: TEFNode): Boolean; override;
  public
    procedure DesignPanelToEditNode; override;
    procedure UpdateDesignPanel(const AForce: Boolean); override;
    procedure Init(const ANode: TEFTree); override;
  end;

implementation

{$R *.dfm}

uses
  StrUtils,
  EF.Classes,
  KIDE.Utils, KIDE.PairsValuesFrameUnit;

{ TDatabaseNodeFrame }

procedure TDatabaseNodeFrame.DesignPanelToEditNode;
begin
  inherited;
  TEFNode(EditNode).Name := ConnectionNameEdit.Text;
end;

procedure TDatabaseNodeFrame.Init(const ANode: TEFTree);
begin
  inherited;
  EmbedEditNodeFrame(ConnectionGroupBox, TPairsValuesFrame,
    EditNode.GetNode('Connection', True));
end;

class function TDatabaseNodeFrame.SuitsNode(const ANode: TEFNode): Boolean;
begin
  Result := IsDatabaseNode(ANode);
end;

procedure TDatabaseNodeFrame.UpdateDesignPanel;
begin
  inherited;
  ConnectionNameEdit.Text := TEFNode(EditNode).Name;
  ConnectionTypeEdit.Text := TEFNode(EditNode).Value;
end;

initialization
  TEditNodeFrameRegistry.Instance.RegisterClass(TDatabaseNodeFrame.GetClassId, TDatabaseNodeFrame);

finalization
  TEditNodeFrameRegistry.Instance.UnregisterClass(TDatabaseNodeFrame.GetClassId);

end.
