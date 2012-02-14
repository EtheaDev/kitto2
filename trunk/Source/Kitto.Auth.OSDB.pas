{-------------------------------------------------------------------------------
   Copyright 2012 Ethea S.r.l.

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

///	<summary>Defines the OSDB authenticator and related classes and services.
///	This authenticator tries to authenticate users by means of the OS
///	authentication.</summary>
unit Kitto.Auth.OSDB;

{$I Kitto.Defines.inc}

interface

uses
  DB,
  EF.Tree,
  Kitto.Auth, Kitto.Auth.DB;

type
  ///	<summary>The OSDB authenticator work almost the same as its ancestor
  ///	<see cref="TKDBAuthenticator" />. The main difference is that, if it
  ///	recognizes that the system user is a valid Kitto user, it uses that user
  ///	name to authenticate.</summary>
  TKOSDBAuthenticator = class(TKDBAuthenticator)
  private
    FIsUsingSystemUserName: Boolean;
  protected
    ///	<summary>Disables password matching if the OS user is
    ///	recognized.</summary>
    function IsPasswordMatching(const ASuppliedPasswordHash: string;
      const AStoredPasswordHash: string): Boolean; override;

    ///	<summary>Loads the OS user name into the auth data.</summary>
    procedure InternalBeforeAuthenticate(const AAuthData: TEFNode); override;

    ///	<summary>
    ///	  <para>If the system user is a valid user, no AuthData is needed
    ///	  because we need only the system user name and we can retrieve it
    ///	  internally.</para>
    ///	  <para>Otherwise, the same auth data as the ancestor is used.</para>
    ///	</summary>
    procedure InternalDefineAuthData(const AAuthenticationData: TEFNode); override;

    ///	<summary>Set to True in InternalDefineAuthData if the system user was
    ///	recognized as a valid user name and we are using it to perform
    ///	authentication. False otherwise.</summary>
    property IsUsingSystemUserName: Boolean read FIsUsingSystemUserName;
  end;

implementation

uses
  SysUtils, Classes,
  EF.Intf, EF.Localization, EF.SysUtils, EF.Types;

{ TKOSDBAuthenticator }

function TKOSDBAuthenticator.IsPasswordMatching(const ASuppliedPasswordHash,
  AStoredPasswordHash: string): Boolean;
begin
  if IsUsingSystemUserName then
    // If we are using the system user we don't want to perform password
    // checking since we assume the OS already did.
    Result := True
  else
    Result := inherited IsPasswordMatching(ASuppliedPasswordHash, AStoredPasswordHash);
end;

procedure TKOSDBAuthenticator.InternalBeforeAuthenticate(
  const AAuthData: TEFNode);
begin
  inherited;
  if IsUsingSystemUserName then
  begin
    // Load auth data from OS (password not needed, see IsPasswordMatching).
    AAuthData.SetString('UserName', EF.SysUtils.GetUserName);
    AAuthData.SetString('Password', '');
  end;
end;

procedure TKOSDBAuthenticator.InternalDefineAuthData(
  const AAuthenticationData: TEFNode);
begin
  if not IsValidUserName(EF.SysUtils.GetUserName) then
  begin
    FIsUsingSystemUserName := False;
    inherited;
  end
  else
    FIsUsingSystemUserName := True;
end;

initialization
  TKAuthenticatorRegistry.Instance.RegisterClass('OSDB', TKOSDBAuthenticator);

finalization
  TKAuthenticatorRegistry.Instance.UnregisterClass('OSDB');

end.

