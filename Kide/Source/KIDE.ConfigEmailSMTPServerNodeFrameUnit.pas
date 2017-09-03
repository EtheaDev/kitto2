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
unit KIDE.ConfigEmailSMTPServerNodeFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, KIDE.EditNodeBaseFrameUnit, System.Actions, Vcl.ActnList,
  Vcl.ExtCtrls, EF.Tree, Vcl.StdCtrls, Vcl.Samples.Spin;

type
  TConfigEmailSMTPServerNodeFrame = class(TEditNodeBaseFrame)
    _HostName: TLabeledEdit;
    _UserName: TLabeledEdit;
    _Password: TLabeledEdit;
    _Port: TSpinEdit;
    PortLabel: TLabel;
    _UseTLS: TCheckBox;
  private
    { Private declarations }
  protected
    class function SuitsNode(const ANode: TEFNode): Boolean; override;
    procedure CleanupDefaultsToEditNode; override;
    procedure DesignPanelToEditNode; override;
  public
    procedure UpdateDesignPanel(const AForce: Boolean = False); override;
  end;

implementation

{$R *.dfm}

uses
  Kitto.Ext.IndyTools;

{ TConfigSMTPServerNodeFrame }

procedure TConfigEmailSMTPServerNodeFrame.CleanupDefaultsToEditNode;
begin
  inherited;
  CleanupTextNode('HostName');
  CleanupTextNode('UserName');
  CleanupTextNode('Password');
  CleanupIntegerNode('Port');
  CleanupBooleanNode('UseTLS', False);
end;

procedure TConfigEmailSMTPServerNodeFrame.DesignPanelToEditNode;
begin
  inherited;
  ;
end;

class function TConfigEmailSMTPServerNodeFrame.SuitsNode(const ANode: TEFNode): Boolean;
begin
  Result := (ANode is TEFNode) and (TEFNode(ANode).Parent is TEFNode) and
    SameText(TEFNode(TEFNode(ANode).Parent).Name, 'SMTP');
end;

procedure TConfigEmailSMTPServerNodeFrame.UpdateDesignPanel(const AForce: Boolean);
begin
  _HostName.Text := '';
  _Port.Value := 0;
  _UseTLS.Checked := False;
  _UserName.Text := '';
  _Password.Text := '';
  inherited;
  _UseTLS.Checked := EditNode.GetBoolean('UseTLS');
end;

initialization
  TEditNodeFrameRegistry.Instance.RegisterClass(TConfigEmailSMTPServerNodeFrame.GetClassId, TConfigEmailSMTPServerNodeFrame);

finalization
  TEditNodeFrameRegistry.Instance.UnregisterClass(TConfigEmailSMTPServerNodeFrame.GetClassId);

end.
