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
unit KIDE.DataPanelControllerDesignerFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, KIDE.EditNodeBaseFrameUnit,
  Vcl.ExtCtrls, Vcl.Tabs,
  System.Actions, Vcl.ActnList, Vcl.ComCtrls, Vcl.ToolWin, Vcl.StdCtrls, Vcl.Buttons,
  KIDE.BaseFrameUnit, KIDE.CodeEditorFrameUnit, KIDE.PanelControllerDesignerFrameUnit,
  EF.Tree, KIDE.ControllerDesignerFrameUnit, KIDE.BorderPanelControllerDesignerFrameUnit,
  Vcl.Samples.Spin, KIDE.MainDataModuleUnit;

type
  TDataPanelControllerDesignerFrame = class(TBorderPanelControllerDesignerFrame)
    DataPanelGroupBox: TGroupBox;
    TopToolBar: TToolBar;
    RefreshToolButton: TToolButton;
    AddToolButton: TToolButton;
    DupToolButton: TToolButton;
    EditToolButton: TToolButton;
    DeleteToolButton: TToolButton;
    ViewToolButton: TToolButton;
  strict private
  protected
    class function SuitsNode(const ANode: TEFNode): Boolean; override;
    procedure UpdateDesignPanel(const AForce: Boolean = False); override;
    procedure CleanupDefaultsToEditNode; override;
    procedure DesignPanelToEditNode; override;
  protected
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

{$R *.dfm}

uses
  EF.Macros,
  Kitto.Ext.Controller, Kitto.Ext.Base,
  Kitto.Ext.DataPanelLeaf, Kitto.Ext.Form, Kitto.Ext.List;

{ TDataPanelLeafControllerDesignerFrame }

procedure TDataPanelControllerDesignerFrame.CleanupDefaultsToEditNode;
begin
  inherited;
  CleanupBooleanNode('PreventAdding');
  CleanupBooleanNode('AllowDuplicating');
  CleanupBooleanNode('PreventEditing');
  CleanupBooleanNode('PreventDeleting');
  CleanupBooleanNode('AllowViewing');
  CleanupBooleanNode('PreventRefreshing');
end;

constructor TDataPanelControllerDesignerFrame.Create(AOwner: TComponent);
begin
  inherited;
end;

procedure TDataPanelControllerDesignerFrame.DesignPanelToEditNode;
begin
  inherited;
  EditNode.SetBoolean('PreventAdding', not AddToolButton.Down);
  EditNode.SetBoolean('AllowDuplicating', DupToolButton.Down);
  EditNode.SetBoolean('PreventEditing', not EditToolButton.Down);
  EditNode.SetBoolean('PreventDeleting', not DeleteToolButton.Down);
  EditNode.SetBoolean('AllowViewing', ViewToolButton.Down);
  EditNode.SetBoolean('PreventRefreshing', not RefreshToolButton.Down);
end;

class function TDataPanelControllerDesignerFrame.SuitsNode(
  const ANode: TEFNode): Boolean;
var
  LControllerClass: TClass;
begin
  Assert(Assigned(ANode));
  LControllerClass := GetControllerClass(ANode);
  Result := Assigned(LControllerClass) and
    LControllerClass.InheritsFrom(TKExtDataPanelLeafController);
end;

procedure TDataPanelControllerDesignerFrame.UpdateDesignPanel(
  const AForce: Boolean);
var
  LControllerClass: TClass;
begin
  inherited;
  Assert(Assigned(EditNode));
  LControllerClass := GetControllerClass(TEFNode(EditNode));
  AddToolButton.Down := not EditNode.GetBoolean('PreventAdding');
  DupToolButton.Down := EditNode.GetBoolean('AllowDuplicating');
  EditToolButton.Down := not EditNode.GetBoolean('PreventEditing');
  DeleteToolButton.Down := not EditNode.GetBoolean('PreventDeleting');
  ViewToolButton.Down := EditNode.GetBoolean('AllowViewing');
  RefreshToolButton.Down := not EditNode.GetBoolean('PreventRefreshing');
end;

initialization
  TEditNodeFrameRegistry.Instance.RegisterClass(TDataPanelControllerDesignerFrame.GetClassId, TDataPanelControllerDesignerFrame);

finalization
  TEditNodeFrameRegistry.Instance.UnregisterClass(TDataPanelControllerDesignerFrame.GetClassId);

end.
