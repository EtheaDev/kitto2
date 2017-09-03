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
unit KIDE.SendEmailDestinationNodeFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, KIDE.EditNodeBaseFrameUnit, System.Actions, Vcl.ActnList,
  Vcl.ExtCtrls, EF.Tree, Vcl.StdCtrls;

type
  TSendEmailDestinationNodeFrame = class(TEditNodeBaseFrame)
    DestinationEdit: TLabeledEdit;
    RecipientsGroupBox: TGroupBox;
  private
  protected
    class function SuitsNode(const ANode: TEFNode): Boolean; override;
    procedure CleanupDefaultsToEditNode; override;
    procedure UpdateDesignPanel(const AForce: Boolean = False); override;
    procedure DesignPanelToEditNode; override;
  public
    procedure Init(const ANode: TEFTree); override;
  end;

implementation

{$R *.dfm}

uses
  KIDE.PairsValuesFrameUnit, Kitto.Ext.IndyTools;

{ TSendEmailDestinationNodeFrame }

procedure TSendEmailDestinationNodeFrame.CleanupDefaultsToEditNode;
begin
  inherited;
end;

procedure TSendEmailDestinationNodeFrame.DesignPanelToEditNode;
begin
  inherited;
  TEFNode(EditNode).AsString := DestinationEdit.Text;
end;

procedure TSendEmailDestinationNodeFrame.Init(const ANode: TEFTree);
var
  PairsValuesFrame: TPairsValuesFrame;
begin
  inherited;
  PairsValuesFrame := EmbedEditNodeFrame(RecipientsGroupBox, TPairsValuesFrame,
    EditNode as TEFNode) as TPairsValuesFrame;
  PairsValuesFrame.FixedKey := 'Recipient';
end;

class function TSendEmailDestinationNodeFrame.SuitsNode(const ANode: TEFNode): Boolean;
var
  LNode: TEFNode;
begin
  Result := False;
  if (ANode is TEFNode) then
  begin
    LNode := TEFNode(ANode);
    if SameText(LNode.Name, 'To') or
      SameText(LNode.Name, 'CC') or
      SameText(LNode.Name, 'BCC') then
    begin
      if Assigned(LNode.Parent) and (LNode.Parent is TEFNode) then
        Result := SameText(TEFNode(LNode.Parent).Name, 'Message');
    end;
  end;
end;

procedure TSendEmailDestinationNodeFrame.UpdateDesignPanel(const AForce: Boolean);
begin
  inherited;
  DestinationEdit.EditLabel.Caption := TEFNode(EditNode).Name;
  DestinationEdit.Text := TEFNode(EditNode).Value;
end;

initialization
  TEditNodeFrameRegistry.Instance.RegisterClass(TSendEmailDestinationNodeFrame.GetClassId, TSendEmailDestinationNodeFrame);

finalization
  TEditNodeFrameRegistry.Instance.UnregisterClass(TSendEmailDestinationNodeFrame.GetClassId);

end.
