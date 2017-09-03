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
unit KIDE.ViewDetailTablesDesignerFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, KIDE.MetadataItemDesignerFrameUnit, System.Actions,
  KIDE.EditNodeBaseFrameUnit, Vcl.ActnList, Vcl.ExtCtrls,
  Kitto.Metadata.DataView, EF.Tree, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.Buttons, Vcl.StdActns,
  Vcl.Samples.Spin;

type
  TViewDetailTablesDesignerFrame = class(TMetadataItemDesignerFrame)
    DetailTablesPageControl: TPageControl;
    DetailTableTabSheet: TTabSheet;
    FileOpenAction: TFileOpen;
    ControllerGroupBox: TGroupBox;
    ControllerStyleComboBox: TComboBox;
    StyleLabel: TLabel;
    StyleHeightLabel: TLabel;
    _Controller_Style_Height: TSpinEdit;
  private
  protected
    class function SuitsNode(const ANode: TEFNode): Boolean; override;
    procedure CleanupDefaultsToEditNode; override;
    procedure UpdateDesignPanel(const AForce: Boolean = False); override;
    procedure DesignPanelToEditNode; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Init(const ANode: TEFTree); override;
  end;

implementation

{$R *.dfm}

uses
  KIDE.Config, KIDE.Project, KIDE.Utils,
  Kitto.Ext.Form, Kitto.Metadata.Models;

{ TViewDetailTablesDesignerFrame }

constructor TViewDetailTablesDesignerFrame.Create(AOwner: TComponent);
begin
  inherited;
  TKideConfig.Instance.Config.GetNode('DetailTablesControllerStyle').GetChildValues(ControllerStyleComboBox.Items);
end;

procedure TViewDetailTablesDesignerFrame.UpdateDesignPanel(const AForce: Boolean);
var
  LStyle: string;
begin
  inherited;
  LStyle := EditNode.GetString('Controller/Style', DEFAULT_DETAIL_STYLE);
  ControllerStyleComboBox.ItemIndex := ControllerStyleComboBox.Items.IndexOf(LStyle);
  _Controller_Style_Height.Value := EditNode.GetInteger('Controller/Style/Height', DEFAULT_DETAIL_PANEL_HEIGHT);
end;

procedure TViewDetailTablesDesignerFrame.DesignPanelToEditNode;
begin
  inherited;
  EditNode.FindNode('Controller', True);
  EditNode.SetString('Controller/Style', ControllerStyleComboBox.Text);
end;

procedure TViewDetailTablesDesignerFrame.CleanupDefaultsToEditNode;
begin
  inherited;
  CleanupIntegerNode('Controller/Style/Height', DEFAULT_DETAIL_PANEL_HEIGHT);
  if not Assigned(EditNode.FindNode('Controller/Style/Height')) then
  begin
    CleanupTextNode('Controller/Style', DEFAULT_DETAIL_STYLE);
  end;
  CleanupOrphanNode('Controller');
end;

procedure TViewDetailTablesDesignerFrame.Init(const ANode: TEFTree);
var
  LTabSheet: TTabSheet;
  LItemsNode, LItemNode: TEFNode;
  I: Integer;
  LFrameClass: TEditNodeBaseFrameClass;
begin
  inherited;
  //Clear all pages
  while DetailTablesPageControl.PageCount > 0 do
    DetailTablesPageControl.Pages[0].Free;
  //bUILD pages based on Filter Items
  LItemsNode := ANode as TEFNode;
  if Assigned(LItemsNode) then
  begin
    for I := 0 to LItemsNode.ChildCount -1 do
    begin
      LItemNode := LItemsNode.Children[I];
      if SameText(LItemNode.Name, 'Table') then
      begin
        LTabSheet := TTabSheet.Create(Self);
        LTabSheet.Name := Format('%s_%d', [LItemNode.Name, I]);
        LTabSheet.PageControl := DetailTablesPageControl;
        LTabSheet.Caption := LItemNode.Name;
        LFrameClass := TEditNodeFrameFractory.Instance.GetEditNodeFrameClass(LItemNode);
        if Assigned(LFrameClass) then
          EmbedEditNodeFrame(LTabSheet, LFrameClass, LItemNode);
      end;
    end;
  end;
end;

class function TViewDetailTablesDesignerFrame.SuitsNode(const ANode: TEFNode): Boolean;
begin
  Result := SameText(ANode.Name, 'DetailTables') and (ANode.Parent is TKViewTable);
end;

initialization
  TEditNodeFrameRegistry.Instance.RegisterClass(TViewDetailTablesDesignerFrame.GetClassId, TViewDetailTablesDesignerFrame);

finalization
  TEditNodeFrameRegistry.Instance.UnregisterClass(TViewDetailTablesDesignerFrame.GetClassId);

end.
