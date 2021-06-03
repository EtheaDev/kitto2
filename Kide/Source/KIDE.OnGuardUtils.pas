{-------------------------------------------------------------------------------
   Copyright 2012-2021 Ethea S.r.l.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
-------------------------------------------------------------------------------}
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
