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
unit KIDE.ViewEditorFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, KIDE.MetadataEditorBaseFrameUnit, EF.Tree,
  Vcl.StdCtrls, Vcl.ExtCtrls, Kitto.Metadata.Views, Vcl.ImgList, KIDE.BaseFrameUnit,
  KIDE.CodeEditorFrameUnit, VirtualTrees, Vcl.ActnList,
  Vcl.ToolWin, Vcl.ComCtrls, System.Actions;

type
  TViewEditorFrame = class(TMetadataEditorBaseFrame)
  private
    function GetEditView: TKView;
    property EditView: TKView read GetEditView;
  strict protected
    function GetTreeClass: TEFTreeClass; override;
    function GetEditorProperty(const AName: string): Variant; override;
    function GetPreviewCommand: string; override;
    procedure ValidateEditor; override;
  public
    procedure InitEditor(const AParams: TEFNode); override;
  end;

implementation

{$R *.dfm}

uses
  KIDE.Editor, KIDE.Project, KIDE.ViewValidator, KIDE.MainDataModuleUnit;

{ TViewEditorFrame }

function TViewEditorFrame.GetEditorProperty(const AName: string): Variant;
begin
  if SameText(AName, 'ImageIndex') then
    Result := TProject.CurrentProject.GetViewImageIndex(EditView)
  else
    Result := inherited GetEditorProperty(AName);
end;

function TViewEditorFrame.GetEditView: TKView;
begin
  Result := EditMetadata as TKView;
end;

function TViewEditorFrame.GetPreviewCommand: string;
begin
  Result := Format('view=%s', [EditView.PersistentName]);
end;

function TViewEditorFrame.GetTreeClass: TEFTreeClass;
begin
  Result := TKView;
end;

procedure TViewEditorFrame.InitEditor(const AParams: TEFNode);
begin
  inherited;
end;

procedure TViewEditorFrame.ValidateEditor;
var
  LViewValidator: TViewValidator;
begin
  LViewValidator := TViewValidator.Create;
  try
    LViewValidator.ValidateViews(EditView);
  finally
    FreeAndNil(LViewValidator);
  end;
end;

initialization
  TEditorRegistry.Instance.RegisterClass(TViewEditorFrame.ClassName, TViewEditorFrame);

finalization
  TEditorRegistry.Instance.UnregisterClass(TViewEditorFrame.ClassName);

end.
