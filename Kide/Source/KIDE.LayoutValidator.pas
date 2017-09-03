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
///	<summary>
///	  The Layout validator should flag:
///	  <list type="bullet">
///	  </list>
///	</summary>
unit KIDE.LayoutValidator;

interface

uses
  EF.Classes, Kitto.Metadata.Views,
  KIDE.TreeValidator;

type
  TLayoutValidator = class(TTreeValidator)
  private
    procedure ValidateLayout(const ALayout: TKLayout);
  protected
    procedure InternalExecute; override;
  public
    procedure ValidateLayouts(ASingleLayout: TKLayout);
  end;

implementation

uses
  SysUtils, StrUtils,
  EF.Tree,
  KIDE.Project;

{ TLayoutValidator }

procedure TLayoutValidator.InternalExecute;
begin
  inherited;
  ValidateLayouts(nil);
end;

procedure TLayoutValidator.ValidateLayout(const ALayout: TKLayout);
begin
  Assert(Assigned(ALayout));

  LogIndent;
  try
    Log(Format('Validating Layout %s...', [ALayout.PersistentName]));

    ValidateTree(ALayout);

  finally
    LogOutdent;
  end;
end;

procedure TLayoutValidator.ValidateLayouts(ASingleLayout: TKLayout);
var
  I: Integer;
  LLayout: TKLayout;
begin
  inherited;
  if not Assigned(ASingleLayout) then
    Log('Validating Layouts...');

  for I := 0 to TProject.CurrentProject.Config.Views.Layouts.LayoutCount - 1 do
  begin
    LLayout := TProject.CurrentProject.Config.Views.Layouts.Layouts[I];
    if not Assigned(ASingleLayout) or SameText(ASingleLayout.PersistentName, LLayout.PersistentName) then
      ValidateLayout(LLayout);
  end;

  if ErrorCount > 0 then
    LogWarning('Layout validation complete. Errors were detected.')
  else
    LogInfo('Layout validation complete. No errors detected.');
end;

end.
