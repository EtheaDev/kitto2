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
///	  The config validator should flag:
///	  <list type="bullet">
///	    <item>config without controllers</item>
///	    <item>missing required parameters (requires a means to describe Config
///	    currently not existing)</item>
///	    <item>misplaced nodes (useful to spot typos or when incompatible
///	    changes are done in Kitto)</item>
///	  </list>
///	</summary>
unit KIDE.ConfigValidator;

interface

uses
  EF.Classes,
  KIDE.TreeValidator;

type
  TConfigValidator = class(TTreeValidator)
  private
    procedure ValidateConfig(const AConfig: TEFComponentConfig);
  protected
    procedure InternalExecute; override;
  public
    procedure ValidateConfigs(ASingleConfig: TEFComponentConfig);
  end;

implementation

uses
  SysUtils, StrUtils,
  EF.Tree,
  KIDE.Project;

{ TConfigValidator }

procedure TConfigValidator.InternalExecute;
begin
  inherited;
  ValidateConfigs(nil);
end;

procedure TConfigValidator.ValidateConfig(const AConfig: TEFComponentConfig);
begin
  Assert(Assigned(AConfig));

  LogIndent;
  try
    Log(Format('Validating Config %s...', [AConfig.PersistentName]));

    ValidateTree(AConfig);

    if AConfig.GetString('AppTitle') = '' then
      LogError('Missing AppTitle.');

    if AConfig.FindNode('Databases') = nil then
      LogError('Missing Databases Node: at least one Database Connection must be configured.');

    if AConfig.GetString('FastCGI/TCPPort') = '' then
      LogError('Missing FastCGI/TCPPort: for any kitto application you must define a TCPPort.');

  finally
    LogOutdent;
  end;
end;

procedure TConfigValidator.ValidateConfigs(ASingleConfig: TEFComponentConfig);
var
  I: Integer;
  LConfig: TEFComponentConfig;
begin
  inherited;
  if not Assigned(ASingleConfig) then
    Log('Validating Configs...');

  for I := 0 to TProject.CurrentProject.AppConfigCount - 1 do
  begin
    LConfig := TProject.CurrentProject.AppConfigs[I].Config;
    if not Assigned(ASingleConfig) or SameText(ASingleConfig.PersistentName, LConfig.PersistentName) then
      ValidateConfig(LConfig);
  end;

  if ErrorCount > 0 then
    LogWarning('Config validation complete. Errors were detected.')
  else
    LogInfo('Config validation complete. No errors detected.');
end;

end.
