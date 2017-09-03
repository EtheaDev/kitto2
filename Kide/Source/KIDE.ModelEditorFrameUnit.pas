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
unit KIDE.ModelEditorFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, KIDE.MetadataEditorBaseFrameUnit, EF.Tree,
  Kitto.Metadata.Models, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Tabs, Vcl.ComCtrls,
  KIDE.BaseFrameUnit, KIDE.CodeEditorFrameUnit, VirtualTrees, Vcl.ImgList,
  Vcl.ActnList, Vcl.ToolWin, System.Actions;

type
  TModelEditorFrame = class(TMetadataEditorBaseFrame)
  private
    function GetEditModel: TKModel;
    property EditModel: TKModel read GetEditModel;
  strict protected
    function GetTreeClass: TEFTreeClass; override;
    function GetEditorProperty(const AName: string): Variant; override;
    procedure ValidateEditor; override;
  public
    procedure InitEditor(const AParams: TEFNode); override;
  end;

implementation

{$R *.dfm}

uses
  KIDE.Editor, KIDE.ModelValidator;


{ TModelEditorFrame }

function TModelEditorFrame.GetEditModel: TKModel;
begin
  Result := EditMetadata as TKModel;
end;

function TModelEditorFrame.GetEditorProperty(const AName: string): Variant;
begin
  if SameText(AName, 'ImageIndex') then
    Result := 5
  else
    Result := inherited GetEditorProperty(AName);
end;

function TModelEditorFrame.GetTreeClass: TEFTreeClass;
begin
  Result := TKModel;
end;

procedure TModelEditorFrame.InitEditor(const AParams: TEFNode);
begin
  inherited;
end;

procedure TModelEditorFrame.ValidateEditor;
var
  LModelValidator: TModelValidator;
begin
  LModelValidator := TModelValidator.Create;
  try
    LModelValidator.ValidateModels(EditModel);
  finally
    FreeAndNil(LModelValidator);
  end;
end;

initialization
  TEditorRegistry.Instance.RegisterClass(TModelEditorFrame.ClassName, TModelEditorFrame);

finalization
  TEditorRegistry.Instance.UnregisterClass(TModelEditorFrame.ClassName);

end.
