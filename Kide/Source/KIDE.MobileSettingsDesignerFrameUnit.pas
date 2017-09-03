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
unit KIDE.MobileSettingsDesignerFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, KIDE.EditNodeBaseFrameUnit,
  System.Actions, Vcl.ActnList, Vcl.ExtCtrls, EF.Tree, Vcl.StdCtrls,
  Vcl.Samples.Spin;

type
  TMobileSettingsDesginerFrame = class(TEditNodeBaseFrame)
    ViewPortContentGroupBox: TGroupBox;
    WidthLabel: TLabel;
    _ViewportContent_Width: TSpinEdit;
    user_scalableCheckBox: TCheckBox;
  private
  protected
    class function SuitsNode(const ANode: TEFNode): Boolean; override;
    procedure UpdateDesignPanel(const AForce: Boolean = False); override;
    procedure CleanupDefaultsToEditNode; override;
    procedure DesignPanelToEditNode; override;
  public
  end;

implementation

{$R *.dfm}

uses
  Kitto.Metadata.Views;

{ TMobileSettingsDesginerFrame }

procedure TMobileSettingsDesginerFrame.CleanupDefaultsToEditNode;
begin
  inherited;
  CleanupTextNode('ViewportContent/user-scalable', '0');
  CleanupIntegerNode('ViewportContent/width');
  CleanupOrphanNode('ViewportContent');
end;

procedure TMobileSettingsDesginerFrame.DesignPanelToEditNode;
var
  LUserScalable: string;
begin
  inherited;
  if user_scalableCheckBox.Checked then
    LUserScalable := '1'
  else
    LUserScalable := '0';
  EditNode.SetString('ViewportContent/user-scalable', LUserScalable);
end;

class function TMobileSettingsDesginerFrame.SuitsNode(
  const ANode: TEFNode): Boolean;
begin
  Assert(Assigned(ANode));
  Result := SameText(ANode.Name, 'MobileSettings') and
    (ANode.Parent is TKView);
end;

procedure TMobileSettingsDesginerFrame.UpdateDesignPanel(const AForce: Boolean);
begin
  inherited;
  user_scalableCheckBox.Checked := EditNode.GetString('ViewportContent/user-scalable') = '1';
end;

initialization
  TEditNodeFrameRegistry.Instance.RegisterClass(TMobileSettingsDesginerFrame.GetClassId, TMobileSettingsDesginerFrame);

finalization
  TEditNodeFrameRegistry.Instance.UnregisterClass(TMobileSettingsDesginerFrame.GetClassId);

end.
