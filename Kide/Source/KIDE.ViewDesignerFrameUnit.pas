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
unit KIDE.ViewDesignerFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, KIDE.TreeDesignerFrameUnit,
  KIDE.BaseFrameUnit, KIDE.CodeEditorFrameUnit, Vcl.ExtCtrls, Vcl.Tabs,
  System.Actions, Vcl.ActnList, Vcl.ComCtrls, Vcl.ToolWin, Vcl.StdCtrls, Vcl.Buttons,
  EF.Tree, Kitto.Metadata.Views, Vcl.StdActns, KIDE.NodeDesignerFrameUnit;

type
  TViewDesignerFrame = class(TTreeDesignerFrame)
    FileOpenAction: TFileOpen;
  strict private
  private
  protected
    class function SuitsTree(const ATree: TEFTree): Boolean; override;
    procedure UpdateDesignPanel(const AForce: Boolean = False); override;
    procedure DesignPanelToEditNode; override;
    procedure CleanupDefaultsToEditNode; override;
  public
    procedure Init(const ANode: TEFTree); override;
  end;

implementation

{$R *.dfm}

uses
  EF.Macros,
  KIDE.Project, KIDE.Utils, KIDE.EditNodeBaseFrameUnit,
  Kitto.Ext.Controller, Kitto.Ext.Base,
  //View classes for this designer
  Kitto.Ext.DataTool;

{ TDownloadFileToolDesignerFrame }

procedure TViewDesignerFrame.CleanupDefaultsToEditNode;
begin
  inherited;
end;

procedure TViewDesignerFrame.DesignPanelToEditNode;
begin
  inherited;
end;

class function TViewDesignerFrame.SuitsTree(const ATree: TEFTree): Boolean;
begin
  Result := ATree is TKView;
end;

procedure TViewDesignerFrame.UpdateDesignPanel(const AForce: Boolean);
begin
  inherited;
end;

procedure TViewDesignerFrame.Init(const ANode: TEFTree);
var
  LViewNode: TEFTree;
  LFrameClass: TEditNodeBaseFrameClass;
begin
  inherited;
  LViewNode := EditNode;
  if EditNode is TEFNode then
    LFrameClass := TEditNodeFrameFractory.Instance.GetEditNodeFrameClass(EditNode)
  else
    LFrameClass := TEditNodeFrameFractory.Instance.GetEditNodeFrameClass(EditNode);
  if Assigned(LFrameClass) then
    EmbedEditNodeFrame(DesignPanel, LFrameClass, EditNode);
end;

initialization
  TTreeDesignerFrameRegistry.Instance.RegisterClass(TViewDesignerFrame.GetClassId, TViewDesignerFrame);

finalization
  TTreeDesignerFrameRegistry.Instance.UnregisterClass(TViewDesignerFrame.GetClassId);

end.
