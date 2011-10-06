{
  Defines the OSDB authenticator and related classes and services.
  This authenticator tries to authenticate EW users using the OS accounting
  mechanism.
}
unit Kitto.Auth.OSDB;

{$I Kitto.Defines.inc}

interface

uses
  DB,
  EF.Tree,
  Kitto.Auth, Kitto.Auth.DB;

{ TODO : check comments and refactor }

type
  {
    The OSDB authenticator descends from the DB authenticator and behaves almost
    the same. The main difference is that, if it recognizes that the system user
    is a valid Kitto user, it uses that user name to authenticate.

    In order to allow to override the system user name automatism and let the
    user to be able to run the EWConsole with a specific username/password pair,
    this authenticator checks for command line parameters to be used for authentication.

    The presence of 'UserName' and 'UserPassword' command line parameters overrides
    the system user name authentication mechanism.
  }
  TKOSDBAuthenticator = class(TKDBAuthenticator)
  private
    FUsingSystemUserName: Boolean;
    FUsingCmdLineParams: Boolean;
  protected
    {
      If we have recognized the system user as a valid EW, password matching is
      not needed, because we assume that has already been done by the OS.
      In this case, we simply return True, otherwise we perform the same password
      matching done by authenticator's ancestor (DB authenticator).
    }
    function IsPasswordMatching(const ASuppliedPasswordHash: string;
      const AStoredPasswordHash: string): Boolean; override;

    {
      If we are using the system user as a valid EW user, we use the system user
      name to set the needed authentication data just before performing the
      authentication attempt.

      If we are using command line parameters, we fetch authentication data from
      those parameters.
    }
    procedure InternalBeforeAuthenticate(const AAuthData: TEFNode);
      override;
    {
      If the system user is a valid EW user, no AuthenticationData is needed by
      the OSDB authenticator cause we need only the system user name and we can
      retrieve it internally. Otherwise, the OSDB authenticator needs the same
      authentication data of its ancestor (DB authenticator).

      If authentication command line parameters are found, those are used to perform
      authentication attempt. No matter if the system user name is or is not a
      valid EW user.
    }
    procedure InternalDefineAuthenticationData(const AAuthenticationData: TEFNode);
      override;
    {
      It is set to True (in InternalDefineAuthenticationData method) if the system
      user has been recognized as a valid EW user name and we are using it to
      perform authentication. False otherwise.
    }
    property UsingSystemUserName: Boolean read FUsingSystemUserName;
    {
      It is set to True (in InternalDefineAuthenticationData method) if authentication
      data is available among command line parameters and we are using it to
      perform authentication. False otherwise.
    }
    property UsingCmdLineParams: Boolean read FUsingCmdLineParams;
  end;

implementation

uses
  SysUtils, Classes,
  EF.Intf, EF.Localization, EF.SysUtils, EF.Types, EF.DB.Utils;

{ TKOSDBAuthenticator }

function TKOSDBAuthenticator.IsPasswordMatching(const ASuppliedPasswordHash,
  AStoredPasswordHash: string): Boolean;
begin
  if UsingSystemUserName then
    {
      If we are using the system user as EW user, we don't want to perform password
      checking since we assume the OS already granted user to log in checking it.
    }
    Result := True
  else
    Result := inherited IsPasswordMatching(ASuppliedPasswordHash, AStoredPasswordHash);
end;

procedure TKOSDBAuthenticator.InternalBeforeAuthenticate(
  const AAuthData: TEFNode);
begin
  inherited;
  if UsingSystemUserName then
  begin
    {
      pre-set authentication data from OS info (password is not needed, see
      implementation of the IsPasswordMatching method in this class.
    }
    AAuthData.SetString('UserName', EF.SysUtils.GetUserName);
    AAuthData.SetString('UserPassword', '');
  end
  else
    if UsingCmdLineParams then
    begin
      {
        Fetch authentication data from command line parameters
      }
      AAuthData.SetString('UserName', GetCmdLineParamValue('UserName'));
      AAuthData.SetString('UserPassword', GetCmdLineParamValue('UserPassword'));
    end;
end;

procedure TKOSDBAuthenticator.InternalDefineAuthenticationData(
  const AAuthenticationData: TEFNode);
begin
  // check if command line authentication data is available
  FUsingCmdLineParams := GetCmdLineParamValue('UserName') <> '';

  {
    We call inherited only if we need authentication data from the outside that
    is the same to say that the system user does not correspond to a valid EW user
    or that command line authentication data is not available.
    Calling inherited means that we need the same authentication data of this class
    ancestor (DB Authenticator).
  }

  if FUsingCmdLineParams or (not IsValidUserName(EF.SysUtils.GetUserName)) then
  begin
    FUsingSystemUserName := False;
    if not FUsingCmdLineParams then
      inherited;
  end
  else
    FUsingSystemUserName := True;
end;

initialization
  EWAuthenticatorRegistry.RegisterClass(TKOSDBAuthenticator);

finalization
  EWAuthenticatorRegistry.UnregisterClass(TKOSDBAuthenticator);

end.

