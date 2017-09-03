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
unit KIDE.Process;

interface

type
  TProcess = class
  strict private
    FErrorCount: Integer;
    FLogIndent: Integer;
  strict
  private
    function GetLogIndentString: string; protected
    procedure LogIndent;
    procedure LogOutdent;
    procedure Log(const AString: string);
    procedure LogError(const AString: string);
    procedure LogWarning(const AString: string);
    procedure LogInfo(const AString: string);
    property ErrorCount: Integer read FErrorCount;
    procedure InternalExecute; virtual; abstract;
  public
    procedure Execute;
  end;

implementation

uses
  SysUtils, StrUtils,
  EF.Logger,
  KIDE.EFHelpers;

{ TProcess }

procedure TProcess.Execute;
begin
  FErrorCount := 0;
  FLogIndent := 0;
  try
    InternalExecute;
  except
    on E: Exception do
    begin
      LogError('Blocking error.');
      LogError(E.Message);
    end;
  end;
end;

function TProcess.GetLogIndentString: string;
begin
  Result := DupeString(' ', FLogIndent);
end;

procedure TProcess.Log(const AString: string);
begin
  TEFLogger.Instance.Log(GetLogIndentString + AString);
end;

procedure TProcess.LogError(const AString: string);
begin
  Inc(FErrorCount);
  TEFLogger.Instance.LogError(GetLogIndentString + AString);
end;

procedure TProcess.LogIndent;
begin
  Inc(FLogIndent, 2);
end;

procedure TProcess.LogInfo(const AString: string);
begin
  TEFLogger.Instance.LogInfo(GetLogIndentString + AString);
end;

procedure TProcess.LogOutdent;
begin
  Dec(FLogIndent, 2);
end;

procedure TProcess.LogWarning(const AString: string);
begin
  TEFLogger.Instance.LogWarning(GetLogIndentString + AString);
end;

end.
