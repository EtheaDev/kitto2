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
unit KIDE.Shell;

interface

///	<summary>
///	  <para>Opens a document with the default application synchronously (AWait
///	  = True) or Asynchronously (AWait = False).</para>
///	  <para>If AWait is True, the function waits that the launched process
///	  finishes, and returns the process' exit code (or -1 in case of
///	  errors).</para>
///	  <para>If AWait is False, the function returns 0 if the call succeeds or
///	  -1 in case of errors.</para>
///	</summary>
function OpenDocument(const AFileName: string; const AWait: Boolean = False): Integer;

///	<summary>
///	  <para>Opens a document with the default edit application synchronously
///	  (AWait = True) or Asynchronously (AWait = False).</para>
///	  <para>If AWait is True, the function waits that the launched process
///	  finishes, and returns the process' exit code (or -1 in case of
///	  errors).</para>
///	  <para>If AWait is False, the function returns 0 if the call succeeds or
///	  -1 in case of errors.</para>
///	</summary>
function EditDocument(const AFileName: string; const AWait: Boolean = False): Integer;

implementation

uses
  ShellAPI, Windows;

function InternalOpenDocument(const AFileName: string; const AVerb: string;
  const AWait: Boolean = False): Integer;
var
  LExecInfo: TShellExecuteInfo;
  LReturnValue: Boolean;
  LUnsignedResult: Cardinal;
begin
  FillChar(LExecInfo, SizeOf(LExecInfo), #0);
  LExecInfo.cbSize := SizeOf(LExecInfo);
  LExecInfo.fMask := SEE_MASK_NOCLOSEPROCESS + SEE_MASK_NOASYNC;
  LExecInfo.lpVerb := PChar('open');
  LExecInfo.nShow := SW_SHOW;
  LExecInfo.lpFile := PChar(AFileName);

  LReturnValue := ShellExecuteEx(@LExecInfo);

  if LReturnValue then
  begin
    if AWait then
    begin
      WaitForSingleObject(LExecInfo.hProcess, INFINITE);
      GetExitCodeProcess(LExecInfo.hProcess, LUnsignedResult);
      Result := LUnsignedResult;
    end
    else
      Result := 0;
    CloseHandle(LExecInfo.hProcess);
  end
  else
    Result := -1;
end;

function OpenDocument(const AFileName: string; const AWait: Boolean = False): Integer;
begin
  Result := InternalOpenDocument(AFileName, 'open', AWait);
end;

function EditDocument(const AFileName: string; const AWait: Boolean = False): Integer;
begin
  Result := InternalOpenDocument(AFileName, 'edit', AWait);
end;

end.
