{
  Defines the DBServer authenticator and related classes and services.
  This authenticator uses the database server to authenticate users.
}
unit Kitto.Auth.DBServer;

{$I Kitto.Defines.inc}

interface

uses
  EF.Tree,
  Kitto.Auth;

type
  {
    The DBServer authenticator uses the database server to authenticate users.
    It needs the same items as its ancestor TKClassicAuthenticator.

    In order for this authenticator to work, it is required that the user-name
    and password placeholders in the database connection string stored in any
    EW's configuration files are written as %auth:UserName% and
    %auth:UserPassword%. When Authenticate is called, the authenticator will
    allow macro substitution of those items and try to connect to the database.
  }
  TKDBServerAuthenticator = class(TKClassicAuthenticator)
  protected
    procedure InternalAuthenticate(
      const AAuthenticationData: TEFNode); override;
  end;
  
implementation

uses
  Kitto.Environment;

{ TKDBServerAuthenticator }

procedure TKDBServerAuthenticator.InternalAuthenticate(
  const AAuthenticationData: TEFNode);
begin
  Environment.MainDBConnection.Open;
end;

initialization
  EWAuthenticatorRegistry.RegisterClass(TKDBServerAuthenticator);

finalization
  EWAuthenticatorRegistry.UnregisterClass(TKDBServerAuthenticator);

end.

