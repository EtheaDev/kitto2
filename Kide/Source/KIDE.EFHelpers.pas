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
unit KIDE.EFHelpers;

interface

uses
  Types,
  EF.Logger;

type
  TEFLoggerHelper = class helper for TEFLogger
  public
    procedure LogInfo(const AString: string);
    procedure LogWarning(const AString: string);
    procedure LogError(const AString: string);

    class function ExtractTag(const AString: string; out ATag: string): string;
  end;

const
  LOG_TAG_INFO = '{{i}}';
  LOG_TAG_WARNING = '{{w}}';
  LOG_TAG_ERROR = '{{e}}';

implementation

uses
  StrUtils;

{ TEFLoggerHelper }

class function TEFLoggerHelper.ExtractTag(const AString: string;
  out ATag: string): string;
begin
  Result := AString;

  if StartsStr(LOG_TAG_INFO, Result) then
  begin
    ATag := LOG_TAG_INFO;
    Delete(Result, 1, Length(LOG_TAG_INFO));
  end
  else if StartsStr(LOG_TAG_WARNING, Result) then
  begin
    ATag := LOG_TAG_WARNING;
    Delete(Result, 1, Length(LOG_TAG_WARNING));
  end
  else if StartsStr(LOG_TAG_ERROR, Result) then
  begin
    ATag := LOG_TAG_ERROR;
    Delete(Result, 1, Length(LOG_TAG_ERROR));
  end
  else
    ATag := '';
end;

procedure TEFLoggerHelper.LogError(const AString: string);
begin
  Log(LOG_TAG_ERROR + AString);
end;

procedure TEFLoggerHelper.LogInfo(const AString: string);
begin
  Log(LOG_TAG_INFO + AString);
end;

procedure TEFLoggerHelper.LogWarning(const AString: string);
begin
  Log(LOG_TAG_WARNING + AString);
end;

end.
