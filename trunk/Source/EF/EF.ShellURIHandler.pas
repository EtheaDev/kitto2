{*******************************************************************}
{                                                                   }
{   Ethea Foundation                                                }
{   Shell URI Handler                                               }
{                                                                   }
{   Copyright (c) 2006-2010 Ethea Srl                               }
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

{
  Definition of the shell URI handler, which forwards an URI to the Windows
  Shell.
}
unit EF.ShellURIHandler;

interface

uses
  EFURIProcessor;

  type
  {
    Forwards the URI to the Windows Shell. It is registered at the highest
    possible slot in the URI handler registry so that other handlers have a
    chance to handle an URI before it is forwarded to the OS.
  }
  TEFShellURIHandler = class(TEFURIHandler)
  protected
    procedure InternalHandleURI(var AURI: string; var AHandled: Boolean); override;
  end;

implementation

uses
  Windows, Forms, ShellAPI;

{ TEFShellURIHandler }

procedure TEFShellURIHandler.InternalHandleURI(var AURI: string; var AHandled: Boolean);
begin
  inherited;
  AHandled := ShellExecute(Application.Handle, 'open', PChar(AURI), '', '', SW_SHOW) > 32;
end;

initialization
  URIHandlerRegistry.RegisterHandler(TEFShellURIHandler, MaxInt);

finalization
  URIHandlerRegistry.UnregisterHandler(TEFShellURIHandler);

end.
