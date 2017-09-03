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
unit KIDE.SendEmailMessageNodeFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, KIDE.EditNodeBaseFrameUnit, System.Actions, Vcl.ActnList,
  Vcl.ExtCtrls,
  EF.Tree, Vcl.StdCtrls, Vcl.ComCtrls;

type
  TSendEmailMessageNodeFrame = class(TEditNodeBaseFrame)
    EmailPageControl: TPageControl;
    MessageTabSheet: TTabSheet;
    AttachmentsTabSheet: TTabSheet;
    _From: TLabeledEdit;
    _Subject: TLabeledEdit;
    BodyLabel: TLabel;
    _Body: TMemo;
    DestinationTabSheet: TTabSheet;
    PageControl2: TPageControl;
    ToTabSheet: TTabSheet;
    CCTabSheet: TTabSheet;
    BCCTabSheet: TTabSheet;
  private
    { Private declarations }
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
  Kitto.Ext.IndyTools,
  KIDE.PairsValuesFrameUnit,
  KIDE.SendEmailToolDesignerFrameUnit,
  KIDE.SendEmailDestinationNodeFrameUnit;

{ TSendEmailMessageNodeFrame }

procedure TSendEmailMessageNodeFrame.CleanupDefaultsToEditNode;
begin
  inherited;
  CleanupOrphanNode('To');
  CleanupOrphanNode('CC');
  CleanupOrphanNode('BCC');
  CleanupOrphanNode('Attachments');
end;

procedure TSendEmailMessageNodeFrame.DesignPanelToEditNode;
begin
  inherited;
  EmailPageControl.ActivePageIndex := 0;
end;

procedure TSendEmailMessageNodeFrame.Init(const ANode: TEFTree);
begin
  inherited;
  EmbedEditNodeFrame(ToTabSheet, TSendEmailDestinationNodeFrame,
    EditNode.GetNode('To', True));
  EmbedEditNodeFrame(CCTabSheet, TSendEmailDestinationNodeFrame,
    EditNode.GetNode('CC', True));
  EmbedEditNodeFrame(BCCTabSheet, TSendEmailDestinationNodeFrame,
    EditNode.GetNode('BCC', True));
  EmbedEditNodeFrame(AttachmentsTabSheet, TPairsValuesFrame,
    EditNode.GetNode('Attachments', True));
end;

class function TSendEmailMessageNodeFrame.SuitsNode(const ANode: TEFNode): Boolean;
var
  LNode: TEFNode;
  LControllerClass: TClass;
begin
  Result := False;
  if (ANode is TEFNode) then
  begin
    LNode := TEFNode(ANode);
    if SameText(LNode.Name, 'Message') and Assigned(LNode.Parent) and
      (LNode.Parent is TEFNode) and SameText(TEFNode(LNode.Parent).Name, 'Controller') then
    begin
      LControllerClass := GetControllerClass(TEFNode(LNode.Parent));
      Result := Assigned(LControllerClass) and LControllerClass.InheritsFrom(TSendEmailToolController);
    end;
  end;
end;

procedure TSendEmailMessageNodeFrame.UpdateDesignPanel(const AForce: Boolean);
begin
  inherited;
  EmailPageControl.ActivePageIndex := 0;
end;

initialization
  TEditNodeFrameRegistry.Instance.RegisterClass(TSendEmailMessageNodeFrame.GetClassId, TSendEmailMessageNodeFrame);

finalization
  TEditNodeFrameRegistry.Instance.UnregisterClass(TSendEmailMessageNodeFrame.GetClassId);

end.
