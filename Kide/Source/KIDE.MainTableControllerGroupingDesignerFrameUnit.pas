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
unit KIDE.MainTableControllerGroupingDesignerFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, KIDE.EditNodeBaseFrameUnit,
  Vcl.ExtCtrls, Vcl.Tabs, Vcl.Samples.Spin,
  System.Actions, Vcl.ActnList, Vcl.ComCtrls, Vcl.ToolWin, Vcl.StdCtrls, Vcl.Buttons,
  KIDE.BaseFrameUnit, KIDE.CodeEditorFrameUnit, KIDE.PanelControllerDesignerFrameUnit,
  EF.Tree, KIDE.ControllerDesignerFrameUnit;

type
  TMainTableControllerGroupingDesignerFrame = class(TEditNodeBaseFrame)
    _FieldName: TLabeledEdit;
    _SortFieldNames: TLabeledEdit;
    _EnableMenu: TCheckBox;
    _StartCollapsed: TCheckBox;
    _ShowName: TCheckBox;
    ShowCountGroupBox: TGroupBox;
    _ShowCount_Template: TLabeledEdit;
    _ShowCount_PluralItemName: TLabeledEdit;
    _ShowCount_ItemName: TLabeledEdit;
    _ShowCount: TCheckBox;
    procedure _ShowCountClick(Sender: TObject);
  strict private
  protected
    class function SuitsNode(const ANode: TEFNode): Boolean; override;
    procedure UpdateDesignPanel(const AForce: Boolean = False); override;
    procedure CleanupDefaultsToEditNode; override;
  protected
  public
  end;

implementation

{$R *.dfm}

uses
  EF.Macros,
  Kitto.Ext.Controller, Kitto.Ext.Base,
  Kitto.Ext.GridPanel, Kitto.Metadata.DataView;

{ TDownloadFileToolDesignerFrame }

procedure TMainTableControllerGroupingDesignerFrame.CleanupDefaultsToEditNode;
begin
  inherited;
  //Cleaning Grouping Node
  CleanupTextNode('SortFieldNames');
  CleanupTextNode('FieldName');
  CleanupBooleanNode('EnableMenu');
  CleanupBooleanNode('StartCollapsed');
  CleanupBooleanNode('ShowName');
  CleanupBooleanNode('ShowCount');
  if not EditNode.GetBoolean('ShowCount') then
  begin
    CleanupBooleanNode('Template');
    CleanupBooleanNode('PluralItemName');
    CleanupBooleanNode('ItemName');
  end;
end;

class function TMainTableControllerGroupingDesignerFrame.SuitsNode(
  const ANode: TEFNode): Boolean;
begin
  Assert(Assigned(ANode));
  Result := SameText(ANode.Name, 'Grouping') and
    (ANode.Parent is TEFNode) and SameText(TEFNode(ANode.Parent).Name, 'Controller') and
    (TEFNode(ANode.Parent).Parent is TKViewTable);
end;

procedure TMainTableControllerGroupingDesignerFrame.UpdateDesignPanel(
  const AForce: Boolean);
begin
  inherited;
end;

procedure TMainTableControllerGroupingDesignerFrame._ShowCountClick(
  Sender: TObject);
begin
  inherited;
  ShowCountGroupBox.Enabled := _ShowCount.Checked;
end;

initialization
  TEditNodeFrameRegistry.Instance.RegisterClass(TMainTableControllerGroupingDesignerFrame.GetClassId, TMainTableControllerGroupingDesignerFrame);

finalization
  TEditNodeFrameRegistry.Instance.UnregisterClass(TMainTableControllerGroupingDesignerFrame.GetClassId);

end.
