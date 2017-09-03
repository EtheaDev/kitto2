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
unit KIDE.MainTableControllerDesignerFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, KIDE.EditNodeBaseFrameUnit,
  Vcl.ExtCtrls, Vcl.Tabs, Vcl.Samples.Spin,
  System.Actions, Vcl.ActnList, Vcl.ComCtrls, Vcl.ToolWin, Vcl.StdCtrls, Vcl.Buttons,
  KIDE.BaseFrameUnit, KIDE.CodeEditorFrameUnit, KIDE.PanelControllerDesignerFrameUnit,
  EF.Tree, KIDE.ControllerDesignerFrameUnit, KIDE.MainTableControllerGroupingDesignerFrameUnit,
  Kitto.Metadata.DataView;

type
  TMainTableControllerDesignerFrame = class(TEditNodeBaseFrame)
    PageControl: TPageControl;
    ControllerTabSheet: TTabSheet;
    GroupingTabSheet: TTabSheet;
    ToolViewsTabSheet: TTabSheet;
    PopUpWindowGroupBox: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    _PopUpWindow_Width: TSpinEdit;
    _PopUpWindow_Height: TSpinEdit;
    GroupBox1: TGroupBox;
    _AutoOpen: TCheckBox;
    FormControllerTabSheet: TTabSheet;
    FormTabSheet: TTabSheet;
    GridTabSheet: TTabSheet;
    _RowClassProvider: TLabeledEdit;
    _PageRecordCount: TSpinEdit;
    PageRecordCountLabel: TLabel;
    _IsMultiSelect: TCheckBox;
    _PagingTools: TCheckBox;
    procedure PageControlChange(Sender: TObject);
  strict private
    function GetViewTable: TKViewTable;
  protected
    class function SuitsNode(const ANode: TEFNode): Boolean; override;
    procedure UpdateDesignPanel(const AForce: Boolean = False); override;
    procedure CleanupDefaultsToEditNode; override;
    procedure DesignPanelToEditNode; override;
  protected
  public
    class function IsViewTableControllerNode(const ANode: TEFNode): boolean;
    procedure Init(const ANode: TEFTree); override;
  end;

implementation

{$R *.dfm}

uses
  EF.Macros,
  Kitto.Ext.Controller, Kitto.Ext.Base, Kitto.Ext.GridPanel,
  KIDE.ViewTableFormGridDesignerFrameUnit,
  KIDE.ViewTableFormControllerDesignerFrameUnit,
  KIDE.ViewTableControllerToolViewsFrameUnit;

{ TMainTableControllerDesignerFrame }

procedure TMainTableControllerDesignerFrame.DesignPanelToEditNode;
begin
  inherited;
  PageControl.ActivePageIndex := 0;
end;

function TMainTableControllerDesignerFrame.GetViewTable: TKViewTable;
begin
  Result := TKViewTable(TEFNode(EditNode).Parent);
end;

procedure TMainTableControllerDesignerFrame.CleanupDefaultsToEditNode;
begin
  inherited;
  CleanupOrphanNode('Grouping');
  CleanupOrphanNode('Form');
  CleanupOrphanNode('Grid');
  CleanupOrphanNode('FormController');

  //Cleaning PopupWindow Node
  CleanupIntegerNode('PopupWindow/Width', DEFAULT_WINDOW_WIDTH);
  CleanupIntegerNode('PopupWindow/Height', DEFAULT_WINDOW_HEIGHT);
  CleanupOrphanNode('PopupWindow');

  CleanupBooleanNode('AutoOpen', not GetViewTable.IsLarge);
  CleanupBooleanNode('PagingTools', GetViewTable.IsLarge);
  CleanupBooleanNode('IsMultiSelect');
  CleanupTextNode('RowClassProvider');
  CleanupIntegerNode('PageRecordCount', DEFAULT_PAGE_RECORD_COUNT);

end;

procedure TMainTableControllerDesignerFrame.Init(const ANode: TEFTree);
begin
  inherited;
end;

class function TMainTableControllerDesignerFrame.IsViewTableControllerNode(
  const ANode: TEFNode): boolean;
begin
  Result := SameText(ANode.Name, 'Controller') and (ANode.Parent is TKViewTable);
end;

procedure TMainTableControllerDesignerFrame.PageControlChange(Sender: TObject);
begin
  inherited;
  if PageControl.ActivePage = GroupingTabSheet then
  begin
    EmbedEditNodeFrame(GroupingTabSheet, TMainTableControllerGroupingDesignerFrame,
      EditNode.FindNode('Grouping', True), True);
  end
  else if PageControl.ActivePage = ToolViewsTabSheet then
  begin
    EmbedEditNodeFrame(ToolViewsTabSheet, TViewTableControllerToolViewsFrame,
      EditNode.FindNode('ToolViews', True), True);
  end
  else if PageControl.ActivePage = FormControllerTabSheet then
  begin
    EmbedEditNodeFrame(FormControllerTabSheet, TViewTableFormControllerDesignerFrame,
      EditNode.FindNode('FormController', True), True);
  end
  else if PageControl.ActivePage = GridTabSheet then
  begin
    EmbedEditNodeFrame(GridTabSheet, TViewTableFormGridDesignerFrame,
      EditNode.FindNode('Grid', True));
  end
  else if PageControl.ActivePage = FormTabSheet then
  begin
    EmbedEditNodeFrame(FormTabSheet, TViewTableFormGridDesignerFrame,
      EditNode.FindNode('Form', True));
  end;
end;

class function TMainTableControllerDesignerFrame.SuitsNode(
  const ANode: TEFNode): Boolean;
begin
  Assert(Assigned(ANode));
  Result := IsViewTableControllerNode(ANode);
end;

procedure TMainTableControllerDesignerFrame.UpdateDesignPanel(
  const AForce: Boolean);
begin
  inherited;
  PageControl.ActivePageIndex := 0;
  _PopUpWindow_Width.Value := EditNode.GetInteger('PopUpWindow/Width', DEFAULT_WINDOW_WIDTH);
  _PopUpWindow_Height.Value := EditNode.GetInteger('PopUpWindow/Height', DEFAULT_WINDOW_HEIGHT);
  _PageRecordCount.Value := EditNode.GetInteger('PageRecordCount', DEFAULT_PAGE_RECORD_COUNT);
  _AutoOpen.Checked := EditNode.GetBoolean('AutoOpen', not GetViewTable.IsLarge);
  _PagingTools.Checked := EditNode.GetBoolean('PagingTools', GetViewTable.IsLarge);
end;

initialization
  TEditNodeFrameRegistry.Instance.RegisterClass(TMainTableControllerDesignerFrame.GetClassId, TMainTableControllerDesignerFrame);

finalization
  TEditNodeFrameRegistry.Instance.UnregisterClass(TMainTableControllerDesignerFrame.GetClassId);

end.
