unit Auth;

interface

uses
  EF.Tree
  , Kitto.Auth.DB
  ;

type
  TTasKittoAuthenticator = class(TKDBAuthenticator)
  strict protected
    procedure SetPassword(const AValue: string); override;
    procedure BeforeResetPassword(const AParams: TEFNode); override;
  end;

implementation

uses
  SysUtils, EF.Localization, EF.Logger, Kitto.Auth;

{ TTasKittoAuth }

procedure TTasKittoAuthenticator.BeforeResetPassword(const AParams: TEFNode);
begin
  // We should send the generated password to the user here.
  // AParams contains the nodes EmailAddress and Password.
  // In this demo we just log it.
  TEFLogger.Instance.Log(Format(_('Password generated: %s'), [AParams.GetString('Password')]));
end;

procedure TTasKittoAuthenticator.SetPassword(const AValue: string);
begin
  // Example of enforcement of password strength rules.
  if Length(AValue) < 8 then
    raise Exception.Create(_('Password length must be at least 8 characters'));
  inherited;
end;

initialization
  TKAuthenticatorRegistry.Instance.RegisterClass('TasKitto', TTasKittoAuthenticator);

finalization
  TKAuthenticatorRegistry.Instance.UnregisterClass('TasKitto');

end.

