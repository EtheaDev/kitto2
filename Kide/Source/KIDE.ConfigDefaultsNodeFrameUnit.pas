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
unit KIDE.ConfigDefaultsNodeFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.Actions,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Samples.Spin,
  Vcl.ExtCtrls, Vcl.Tabs, Vcl.ActnList, Vcl.ComCtrls, Vcl.ToolWin, Vcl.StdCtrls,
  EF.Tree,
  KIDE.BaseFrameUnit, KIDE.CodeEditorFrameUnit, KIDE.EditNodeBaseFrameUnit;

type
  TConfigDefaultsNodeFrame = class(TEditNodeBaseFrame)
    AuthScrollBox: TScrollBox;
    AuthAutoScrollPanel: TPanel;
    GridPanel: TGroupBox;
    WindowGroupBox: TGroupBox;
    _Grid_PageRecordCount: TSpinEdit;
    _Window_Height: TSpinEdit;
    _Window_Width: TSpinEdit;
    PageRecordCountLabel: TLabel;
    DefaultActionComboBox: TComboBox;
    ThemeLabel: TLabel;
    WidthLabel: TLabel;
    HeightLabel: TLabel;
    procedure DefaultActionComboBoxChange(Sender: TObject);
  private
  protected
    class function SuitsNode(const ANode: TEFNode): Boolean; override;
    procedure CleanupDefaultsToEditNode; override;
    procedure UpdateDesignPanel(const AForce: Boolean = False); override;
    procedure DesignPanelToEditNode; override;
    procedure UpdateEditComponents; override;
  public
    procedure Init(const ANode: TEFTree); override;
    constructor Create(AOwner: TComponent); override;
  end;

implementation

{$R *.dfm}

uses
  KIDE.Config,
  EF.Classes, KIDE.Utils,
  Kitto.Config,
  Kitto.Auth, Kitto.Auth.DB, Kitto.Auth.DBServer, Kitto.Auth.OSDB, Kitto.Auth.TextFile,
  Kitto.AccessControl.DB;

{ TConfigDesignerFrame }

procedure TConfigDefaultsNodeFrame.UpdateEditComponents;
begin
  inherited;
end;

procedure TConfigDefaultsNodeFrame.CleanupDefaultsToEditNode;
begin
  inherited;
  CleanupIntegerNode('Grid/PageRecordCount');
  CleanupTextNode('Grid/DefaultAction');
  CleanupIntegerNode('Window/Width');
  CleanupIntegerNode('Window/Height');
  CleanupOrphanNode('Grid');
  CleanupOrphanNode('Window');
end;

constructor TConfigDefaultsNodeFrame.Create(AOwner: TComponent);
begin
  inherited;
  TKideConfig.Instance.Config.GetNode('Defaults/Grid/DefaultActions').GetChildValues(DefaultActionComboBox.Items);
end;

procedure TConfigDefaultsNodeFrame.DefaultActionComboBoxChange(Sender: TObject);
begin
  inherited;
  IsChanged := True;
end;

procedure TConfigDefaultsNodeFrame.DesignPanelToEditNode;
begin
  inherited;
  EditNode.SetString('Grid/DefaultAction' ,DefaultActionComboBox.Text);
end;

procedure TConfigDefaultsNodeFrame.Init(const ANode: TEFTree);
begin
  inherited;
end;

class function TConfigDefaultsNodeFrame.SuitsNode(const ANode: TEFNode): Boolean;
begin
  Result := Assigned(ANode.Parent) and
    (ANode.Parent is TEFComponentConfig) and SameText(ANode.Name, 'Defaults');
end;

procedure TConfigDefaultsNodeFrame.UpdateDesignPanel(const AForce: Boolean);
var
  LDefaultAction: string;
begin
  inherited;
  //Align ComboBox item
  LDefaultAction := TEFNode(EditNode).GetString('Grid/DefaultAction', NODE_NULL_VALUE);
  DefaultActionComboBox.ItemIndex := DefaultActionComboBox.Items.IndexOf(LDefaultAction);
  DefaultActionComboBoxChange(DefaultActionComboBox);
end;

initialization
  TEditNodeFrameRegistry.Instance.RegisterClass(TConfigDefaultsNodeFrame.GetClassId, TConfigDefaultsNodeFrame);

finalization
  TEditNodeFrameRegistry.Instance.UnregisterClass(TConfigDefaultsNodeFrame.GetClassId);

end.
