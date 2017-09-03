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
unit KIDE.ActionToolDesignerFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.Actions,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.Tabs,
  Vcl.ActnList, Vcl.ComCtrls, Vcl.ToolWin, Vcl.StdCtrls, Vcl.Buttons, Vcl.StdActns,
  EF.Tree, Kitto.Ext.Base,
  KIDE.BaseFrameUnit, KIDE.CodeEditorFrameUnit,
  KIDE.EditNodeBaseFrameUnit, KIDE.NodeDesignerFrameUnit;

type
  TActionToolDesignerFrame = class(TEditNodeBaseFrame)
    FileOpenAction: TFileOpen;
    VCPageControl: TPageControl;
    ViewTabSheet: TTabSheet;
    ControllerTabsheet: TTabSheet;
    ViewGroupBox: TGroupBox;
    ImageNameSpeedButton: TSpeedButton;
    ImageNameImageLarge: TImage;
    _DisplayLabel: TLabeledEdit;
    ImageNameEdit: TLabeledEdit;
    _Controller: TLabeledEdit;
    ImageNameImage: TImage;
    _IsVisible: TCheckBox;
    BeforeExecuteTabSheet: TTabSheet;
    procedure ImageNameEditChange(Sender: TObject);
    procedure FileOpenActionAccept(Sender: TObject);
    procedure VCPageControlChange(Sender: TObject);
  strict private
  private
    function GetDefaultImageName: string;
    class function GetToolControllerClass(ANode: TEFNode): TExtToolControllerClass;
  protected
    class function SuitsNode(const ANode: TEFNode): Boolean; override;
    procedure DesignPanelToEditNode; override;
    procedure CleanupDefaultsToEditNode; override;
  public
    procedure UpdateDesignPanel(const AForce: Boolean = False); override;
    procedure Init(const ANode: TEFTree); override;
  end;

implementation

{$R *.dfm}

uses
  EF.Macros,
  KIDE.Project, KIDE.Utils,
  Kitto.Ext.Controller,
  KIDE.PairsValuesFrameUnit,
  Kitto.Ext.DataTool;

{ TDownloadFileToolDesignerFrame }

procedure TActionToolDesignerFrame.CleanupDefaultsToEditNode;
begin
  inherited;
  CleanupTextNode('ImageName');
  CleanupTextNode('ImageName', GetDefaultImageName);
  CleanupTextNode('DisplayLabel');
  CleanupBooleanNode('IsVisible', True);
  CleanupOrphanNode('BeforeExecute');
end;

procedure TActionToolDesignerFrame.DesignPanelToEditNode;
begin
  inherited;
  VCPageControl.ActivePageIndex := 0;
  EditNode.SetString('ImageName', ImageNameEdit.Text)
end;

procedure TActionToolDesignerFrame.FileOpenActionAccept(Sender: TObject);
var
  LFileName: string;
begin
  inherited;
  LFileName := ExtractFileName(FileOpenAction.Dialog.FileName);
  if SameText(ExtractFileExt(LFileName), '.png') then
    LFileName := Copy(LFileName, 1, length(LFileName)-4);
  ImageNameEdit.Text := LFileName;
end;

function TActionToolDesignerFrame.GetDefaultImageName: string;
var
  LControllerClass: TExtToolControllerClass;
begin
  inherited;
  LControllerClass := GetToolControllerClass(TEFNode(EditNode));
  if Assigned(LControllerClass) then
    Result := LControllerClass.GetDefaultImageName
  else
    Result := '';
end;

class function TActionToolDesignerFrame.GetToolControllerClass(ANode: TEFNode): TExtToolControllerClass;
var
  LControllerNode: TEFNode;
  LControllerClass: TClass;
begin
  Result := nil;
  LControllerNode := ANode.FindNode('Controller');
  if Assigned(LControllerNode) then
  begin
    LControllerClass := TKExtControllerRegistry.Instance.FindClass(LControllerNode.AsString);
    if Assigned(LControllerClass) and LControllerClass.InheritsFrom(TKExtToolController) then
      Result := TExtToolControllerClass(LControllerClass);
  end;
end;

procedure TActionToolDesignerFrame.ImageNameEditChange(Sender: TObject);
begin
  inherited;
  if (ImageNameEdit.Text = '') then
    ImageNameEdit.Text := GetDefaultImageName;
  ShowImage(ImageNameEdit.Text+'_large', ImageNameImageLarge);
  ShowImage(ImageNameEdit.Text, ImageNameImage);
  IsChanged := True;
end;

procedure TActionToolDesignerFrame.Init(const ANode: TEFTree);
var
  LControllerNode: TEFNode;
  LFrameClass: TEditNodeBaseFrameClass;
  LPairsValuesFrame: TPairsValuesFrame;
begin
  inherited;
  LControllerNode := ANode.FindNode('Controller');
  if Assigned(LControllerNode) then
  begin
    LFrameClass := TEditNodeFrameFractory.Instance.GetEditNodeFrameClass(LControllerNode);
    if Assigned(LFrameClass) then
      EmbedEditNodeFrame(ControllerTabSheet, LFrameClass, LControllerNode);
  end;
  LPairsValuesFrame := EmbedEditNodeFrame(BeforeExecuteTabSheet, TPairsValuesFrame,
    EditNode.FindNode('BeforeExecute')) as TPairsValuesFrame;
  if Assigned(LPairsValuesFrame) then
    LPairsValuesFrame.FixedKey := 'ToolView';
end;

class function TActionToolDesignerFrame.SuitsNode(const ANode: TEFNode): Boolean;
begin
  Result := (ANode is TEFNode) and Assigned(GetToolControllerClass(TEFNode(ANode)));
end;

procedure TActionToolDesignerFrame.UpdateDesignPanel(
  const AForce: Boolean);
begin
  inherited;
  VCPageControl.ActivePageIndex := 0;
  ImageNameEdit.Text := EditNode.GetString('ImageName', GetDefaultImageName);
  _IsVisible.Checked := EditNode.GetBoolean('IsVisible', True);
end;

procedure TActionToolDesignerFrame.VCPageControlChange(Sender: TObject);
var
  LPairsValuesFrame: TPairsValuesFrame;
begin
  inherited;
  if VCPageControl.ActivePage = BeforeExecuteTabSheet then
  begin
    LPairsValuesFrame := EmbedEditNodeFrame(BeforeExecuteTabSheet, TPairsValuesFrame,
      EditNode.FindNode('BeforeExecute', True), True) as TPairsValuesFrame;
    if Assigned(LPairsValuesFrame) then
      LPairsValuesFrame.FixedKey := 'ToolView';
  end;
  IsChanged := True;
end;

initialization
  TEditNodeFrameRegistry.Instance.RegisterClass(TActionToolDesignerFrame.GetClassId, TActionToolDesignerFrame);

finalization
  TEditNodeFrameRegistry.Instance.UnregisterClass(TActionToolDesignerFrame.GetClassId);

end.
