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
unit KIDE.TreeFolderNodeDesignerFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.Tabs,
  EF.Tree,
  KIDE.EFTreeFrameUnit, System.Actions, Vcl.ActnList, Vcl.ComCtrls, Vcl.ToolWin,
  KIDE.EditNodeBaseFrameUnit, Vcl.StdCtrls, KIDE.NodeDesignerFrameUnit, KIDE.BaseFrameUnit,
  KIDE.CodeEditorFrameUnit, Vcl.StdActns, Vcl.Buttons;

type
  TTreeFolderNodeDesignerFrame = class(TEditNodeBaseFrame)
    ValueEdit: TLabeledEdit;
    _IsInitiallyCollapsed: TCheckBox;
    ImageNameEdit: TLabeledEdit;
    ImageNameSpeedButton: TSpeedButton;
    ImageNameImage: TImage;
    ImageNameImageLarge: TImage;
    FileOpenAction: TFileOpen;
    procedure FileOpenActionAccept(Sender: TObject);
    procedure ImageNameEditChange(Sender: TObject);
  private
  strict protected
  protected
    procedure CleanupDefaultsToEditNode; override;
    procedure UpdateDesignPanel(const AForce: Boolean); override;
    procedure DesignPanelToEditNode; override;
  public
    class function SuitsNode(const ANode: TEFNode): Boolean; override;
    procedure Init(const ANode: TEFTree); override;
  end;

implementation

{$R *.dfm}

uses
  KIDE.Utils,
  Kitto.Metadata.Views;

{ TTreeFolderNodeDesignerFrame }

procedure TTreeFolderNodeDesignerFrame.CleanupDefaultsToEditNode;
begin
  inherited;
  CleanupBooleanNode('IsInitiallyCollapsed');
end;

procedure TTreeFolderNodeDesignerFrame.DesignPanelToEditNode;
begin
  inherited;
  TEFNode(EditNode).Value := ValueEdit.Text;
  EditNode.SetString('ImageName', ImageNameEdit.Text);
end;

procedure TTreeFolderNodeDesignerFrame.FileOpenActionAccept(Sender: TObject);
var
  LFileName: string;
begin
  inherited;
  LFileName := ExtractFileName(FileOpenAction.Dialog.FileName);
  if SameText(ExtractFileExt(LFileName), '.png') then
    LFileName := Copy(LFileName, 1, length(LFileName)-4);
  ImageNameEdit.Text := LFileName;
end;

procedure TTreeFolderNodeDesignerFrame.ImageNameEditChange(Sender: TObject);
begin
  if (ImageNameEdit.Text = '') then
    ImageNameEdit.Text := 'Folder';
  ShowImage(ImageNameEdit.Text+'_large', ImageNameImageLarge);
  ShowImage(ImageNameEdit.Text, ImageNameImage);
end;

procedure TTreeFolderNodeDesignerFrame.Init(const ANode: TEFTree);
begin
  inherited;
end;

class function TTreeFolderNodeDesignerFrame.SuitsNode(const ANode: TEFNode): Boolean;
begin
  Result := SameText(ANode.Name, 'Folder') and (ANode.Root is TKTreeView) and
    ((ANode.Parent is TKTreeView) or ((ANode.Parent is TEFNode) and SameText(TEFNode(ANode.Parent).Name, 'Folder')));
end;

procedure TTreeFolderNodeDesignerFrame.UpdateDesignPanel(const AForce: Boolean);
begin
  inherited;
  ValueEdit.Text := TEFNode(EditNode).Value;
  ImageNameEdit.Text := EditNode.GetString('ImageName', 'Folder');
end;

initialization
  TEditNodeFrameRegistry.Instance.RegisterClass(TTreeFolderNodeDesignerFrame.GetClassId, TTreeFolderNodeDesignerFrame);

finalization
  TEditNodeFrameRegistry.Instance.UnregisterClass(TTreeFolderNodeDesignerFrame.GetClassId);

end.
