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
unit KIDE.ConfigEmailSMTPNodeFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, KIDE.EditNodeBaseFrameUnit, System.Actions, Vcl.ActnList,
  Vcl.ExtCtrls, EF.Tree, Vcl.StdCtrls, Vcl.Samples.Spin, Vcl.ComCtrls,
  KIDE.ConfigEmailSMTPServerNodeFrameUnit;

type
  TConfigEmailSMTPNodeFrame = class(TEditNodeBaseFrame)
    SMTPTabControl: TTabControl;
    procedure SMTPTabControlChange(Sender: TObject);
  private
    SMTPServerNodeFrame: TConfigEmailSMTPServerNodeFrame;
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
  EF.Classes,
  KIDE.Project, Kitto.Ext.IndyTools;

{ TConfigSMTPServerNodeFrame }

procedure TConfigEmailSMTPNodeFrame.CleanupDefaultsToEditNode;
begin
  inherited;
  CleanupOrphanNode('SMTP/Default');
  CleanupOrphanNode('SMTP');
end;

procedure TConfigEmailSMTPNodeFrame.DesignPanelToEditNode;
begin
  inherited;
end;

procedure TConfigEmailSMTPNodeFrame.Init(const ANode: TEFTree);
var
  LDefaultNode: TEFNode;
begin
  inherited;
  LDefaultNode := EditNode.FindNode('Default', True);
  SMTPServerNodeFrame := EmbedEditNodeFrame(SMTPTabControl, TConfigEmailSMTPServerNodeFrame,
    LDefaultNode) as TConfigEmailSMTPServerNodeFrame;
end;

procedure TConfigEmailSMTPNodeFrame.SMTPTabControlChange(Sender: TObject);
var
  LNodeName: string;
  LNode: TEFNode;
begin
  inherited;
  LNodeName := SMTPTabControl.Tabs[SMTPTabControl.TabIndex];
  LNode := EditNode.FindNode(LNodeName);
  SMTPServerNodeFrame.Init(LNode);
  SMTPServerNodeFrame.UpdateDesignPanel(False);
end;

class function TConfigEmailSMTPNodeFrame.SuitsNode(const ANode: TEFNode): Boolean;
begin
  Result := SameText(ANode.Name, 'SMTP') and
    (ANode.Parent is TEFNode) and
    SameText(TEFNode(ANode.Parent).Name, 'Email') and
    Assigned(TEFNode(ANode.Parent).Parent) and
    (TEFNode(ANode.Parent).Parent is TEFComponentConfig);
end;

procedure TConfigEmailSMTPNodeFrame.UpdateDesignPanel(const AForce: Boolean);
var
  I: Integer;
begin
  SMTPTabControl.Tabs.Clear;
  for I := 0 to EditNode.ChildCount -1 do
    SMTPTabControl.Tabs.Add(EditNode.Children[I].Name);
  inherited;
end;

initialization
  TEditNodeFrameRegistry.Instance.RegisterClass(TConfigEmailSMTPNodeFrame.GetClassId, TConfigEmailSMTPNodeFrame);

finalization
  TEditNodeFrameRegistry.Instance.UnregisterClass(TConfigEmailSMTPNodeFrame.GetClassId);

end.
