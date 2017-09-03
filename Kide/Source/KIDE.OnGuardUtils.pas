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
unit KIDE.OnGuardUtils;

interface

uses
  SysUtils,
  EfOnGuard, OnGuard;

const
  OnGuardHelpContext = 950; //Used by ISF

//Ritorna False se l'applicazione non supera il controllo della licenza
function GetRegistrationInformation: boolean;
function GetContactInfo: string;
function GetIdentityString: string;
function CheckRegisteredVersion: boolean;
function CheckMaxUsers: boolean;

//Funzioni di output di valori già calcolati
function GetOnGuardMsg: string;
function GetDaysToEnd: integer;

implementation

uses
  Dialogs, Classes, EF.Localization, KIDE.Utils, ShlObj;

var
  OnGuardMsg: string;
  DaysToEnd: Integer;

function GetOnGuardMsg: string;
begin
{$IFDEF EFONGUARD}
  CheckRegisteredVersion;
  Result := OnGuardMsg;
{$ELSE}
  Result := 'Registered to Ethea - For internal use only';
{$ENDIF}
end;

function GetDaysToEnd: integer;
begin
  Result := DaysToEnd;
end;

function GetRegistrationInformation: boolean;
var
  LSpecialFolder: string;
begin
  LSpecialFolder := GetSpecialFolder(CSIDL_COMMON_DOCUMENTS);
  if not DirectoryExists(LSpecialFolder+'\Ethea') then
    CreateDir(LSpecialFolder+'\Ethea');
  if not DirectoryExists(LSpecialFolder+'\Ethea\KIDE2') then
    CreateDir(LSpecialFolder+'\Ethea\KIDE2');
  OnGuardInfo.IniFileName := LSpecialFolder+'\Ethea\KIDE2\Registration.ini';
  Result := OnGuardInfo.GetRegistrationInformation;
end;

function GetContactInfo: string;
begin
  //Check if application was registered
  if not OnGuardInfo.GetRegistrationInformation then
    Result := ''
  else
    Result := OnGuardInfo.ContactInfo;
end;

function GetIdentityString: string;
begin
  //Check if application was registered
  if not OnGuardInfo.GetRegistrationInformation then
    Result := ''
  else
    Result := OnGuardInfo.IdentityString;
end;

function CheckRegisteredVersion: boolean;
var
  Code: TCodeStatus;
  ExpirationDate: TDateTime;
begin
  Result := False;
  DaysToEnd := -1;
  //Controlla se l'applicazione è stata registrata
  if GetRegistrationInformation then
  begin
    //Esiste il file: Controlla se l'applicazione è valida
    Code := OnGuardInfo.ValidateMultiCodes(ExpirationDate);
    case Code of
      ogValidCode:
      begin
        Result := True;
        if (ExpirationDate <> 0) and (ExpirationDate <> EncodeDate(9999, 1, 1)) then
        begin
          DaysToEnd := Trunc(ExpirationDate) - Trunc(Date);
          if DaysToEnd <= 0 then
          begin
            OnGuardMsg := _('Trial period Expired! Please acquire the software license.');
            Result := False;
          end
          else
            OnGuardMsg := _(Format('(Trial version. Expiration days: %d)', [DaysToEnd]));
        end
        else
          OnGuardMsg := Format(_('Software registered to "%s".'), [OnGuardInfo.IdentityString]);
      end;
      ogInvalidCode: OnGuardMsg := Format(_('Invalid registration key!'), [OnGuardInfo.IdentityString, OnGuardInfo.SerialNumber]);
      ogCodeExpired, ogPastEndDate: OnGuardMsg := _('Wrong activation key!');
      ogNetCountUsed: OnGuardMsg := _('Warning: maximum number of concurrent users exceeded. You cannot continue.');
    end;
  end
  else
  begin
    //non esiste il file per la licenza
    OnGuardMsg := Format(_('Unregistered version! You must acquire license from "%s"'), [OnGuardInfo.ContactInfo]);
    Result := False;
  end;
end;

function CheckMaxUsers: boolean;
var
  Code: TCodeStatus;
begin
  Result := False;
  DaysToEnd := -1;
  //Esiste il file: Controlla se l'applicazione è valida
  Code := OnGuardInfo.ValidateMaxUsersCount;
  case Code of
    ogValidCode: Result := True;
    ogInvalidCode: OnGuardMsg := Format(_('Wrong activation key!'), [OnGuardInfo.IdentityString, OnGuardInfo.SerialNumber]);
    ogNetCountUsed: OnGuardMsg := _('Warning: maximum number of concurrent users exceeded. You cannot continue.');
  end;
end;

end.
