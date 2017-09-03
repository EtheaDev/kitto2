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
unit KIDE.ConfigEditorFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ActnList, Vcl.ComCtrls,
  Vcl.ToolWin, Vcl.ExtCtrls,
  EF.Tree,
  Kitto.Config,
  KIDE.BaseFrameUnit, KIDE.PersistentTreeEditorBaseFrameUnit, KIDE.CodeEditorFrameUnit,
  System.Actions;

type
  TConfigEditorFrame = class(TPersistentTreeEditorBaseFrame)
  strict protected
    function GetTreeClass: TEFTreeClass; override;
    function GetEditorProperty(const AName: string): Variant; override;
    procedure Save; override;
    procedure ValidateEditor; override;
  end;

implementation

{$R *.dfm}

uses
  EF.Classes,
  KIDE.Editor, KIDE.ConfigValidator;

{ TConfigEditorFrame }

function TConfigEditorFrame.GetEditorProperty(const AName: string): Variant;
begin
  if SameText(AName, 'ImageIndex') then
    Result := 3
  else
    Result := inherited GetEditorProperty(AName);
end;

function TConfigEditorFrame.GetTreeClass: TEFTreeClass;
begin
  Result := TEFComponentConfig;
end;

procedure TConfigEditorFrame.Save;
begin
  inherited;
  if SameText(ExtractFileName(OriginalFileName), ExtractFileName(TKConfig.Instance.Config.PersistentFileName)) then
    TKConfig.Instance.InvalidateConfig;
end;

procedure TConfigEditorFrame.ValidateEditor;
var
  LConfigValidator: TConfigValidator;
begin
  LConfigValidator := TConfigValidator.Create;
  try
    LConfigValidator.ValidateConfigs(EditObject as TEFComponentConfig);
  finally
    FreeAndNil(LConfigValidator);
  end;
end;

initialization
  TEditorRegistry.Instance.RegisterClass(TConfigEditorFrame.ClassName, TConfigEditorFrame);

finalization
  TEditorRegistry.Instance.UnregisterClass(TConfigEditorFrame.ClassName);

end.
