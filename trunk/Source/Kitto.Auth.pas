///	<summary>
///	  Defines the base authenticator and related classes and services.
///	  Authenticators allow the creation of EW applications that require user
///	  anthentication at startup.
///	</summary>
unit Kitto.Auth;

{ TODO : refactoring due; shorten names; add login/logout; reevaluate usefulness of the host and the use of exceptions }

{$I Kitto.Defines.inc}

interface

uses
  Classes,
  EF.Classes, EF.Tree, EF.Environment;

type
  {
    Abstract base authenticator. An authenticator defines a method for user
    authentication, which happens at program startup both in EW (through
    command line parameters) and in EWConsole (through a dialog box).

    Only one authenticator may be active at any one time. Applications wanting
    to use a custom authentication scheme should create and register an
    authenticator, and then make it active through the EW authentication host.
    @seealso(TKAuthenticationHost)
  }
  TKAuthenticator = class(TEFComponent)
  private
    FAuthData: TEFNode;
    FAuthMacroExpander: TEFNodeMacroExpander;
    // Keep a reference in order to be able to call it in the destructor.
    FEnvironment: IEFEnvironment;
  protected
    {
      Called at the beginning of the authentication process, before
      InternalAuthenticate.
    }
    procedure InternalBeforeAuthenticate(
      const AAuthenticationData: TEFNode); virtual;
    {
      Called at the end of the authentication process, in case of
      successful authentication.
    }
    procedure InternalAfterAuthenticate(
      const AAuthenticationData: TEFNode); virtual;
    {
      Implements Authenticate. Descendants should verify that AAuthenticationData
      contains all required items, query whatever authentication mechanism they
      encapsulate, and raise an exception if a match is not found, that is if
      the supplied authentication data is incorrect.
    }
    procedure InternalAuthenticate(const AAuthenticationData: TEFNode); virtual; abstract;
    {
      Implements DefineAuthenticationData.
    }
    procedure InternalDefineAuthenticationData(
      const AAuthenticationData: TEFNode); virtual; abstract;
    class function InternalGetClassId: string; override;
    {
      This function should return an unique user identifier, to be used for
      example for access control. The default implementation returns 'PUBLIC',
      while a descendant will return the user name or something else, as
      required.
    }
    function GetUserId: string; virtual;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
    {
      Receives an empty data packet which it should fill with the definitions
      (names and types, not values) of all required authentication items. The
      system uses this information to build dialog boxes or require command line
      parameters at login time, so that the user can supply any authentication
      data that is needed by the currently active authenticator. The most
      common example of authentication data is a username + password
      combination.

      If no authentication items are defined, then the system will not require
      user authentication but will still call IsAuthentic passing in an empty
      or defaulted data packet.
    }
    procedure DefineAuthenticationData(const AAuthenticationData: TEFNode);
    {
      Checks whether the specified authentication data designates a valid
      user or not, and raises exceptions if the authentication fails.
    }
    procedure Authenticate(const AAuthenticationData: TEFNode);
    {
      Gives access to a copy of the authentication data that was last passed to
      Authenticate (and optionally modified by the object during authentication).
    }
    property AuthenticationData: TEFNode read FAuthData;
    {
      An unique identifier for the currently logged in user. The value depends
      on the particular descendant. By default, it's 'PUBLIC'.
    }
    property UserId: string read GetUserId;
  end;

  {
    Class reference for TKAuthenticator, used by the registry and factory.
  }
  TKAuthenticatorClass = class of TKAuthenticator;

  {
    An abstract authenticator that requires user name and password as
    authentication data.

    This authenticator needs the items:
    @table(
      @row(@cell(UserName)@cell(string))
      @row(@cell(UserPassword)@cell(string))
    )
    How the authentication data is checked is deferred to the concrete
    descendants. The value of the UserName item is also used as the value
    for the UserId property.
  }
  TKClassicAuthenticator = class(TKAuthenticator)
  protected
    procedure InternalDefineAuthenticationData(
      const AAuthData: TEFNode); override;
    function GetUserId: string; override;
  end;

  {
    The Null authenticator does not require authentication data and always
    grants authentication. It is used by default.
  }
  TKNullAuthenticator = class(TKAuthenticator)
  protected
    procedure InternalDefineAuthenticationData(
      const AAuthenticationData: TEFNode); override;
    procedure InternalAuthenticate(
      const AAuthenticationData: TEFNode); override;
  end;

  {
    This class holds a list of registered authenticator classes.
  }
  TKAuthenticatorRegistry = class(TEFRegistry)
  public
    {
      Adds an authenticator class to the registry.
    }
    procedure RegisterClass(const AClass: TKAuthenticatorClass);
    {
      Deletes a previously registered authenticator class from the registry.
    }
    procedure UnregisterClass(const AClass: TKAuthenticatorClass);
  end;

  {
    Uses the registry to create authenticators by class Id.
    It is friend to TKAuthenticatorRegistry.
  }
  TKAuthenticatorFactory = class(TEFFactory)
  public
    {
      Creates and returns an instance of the authenticator class identified
      by AClassId. Raises an exception if said class is not registered.
    }
    function CreateObject(const AClassId: string): TKAuthenticator;
  end;

{
  Singleton authenticator class registry. Used by authenticators and by the
  authenticator factory.
}
function EWAuthenticatorRegistry: TKAuthenticatorRegistry;

{
  Singleton authentication object factory. Used by the authentication host.
}
function EWAuthenticatorFactory: TKAuthenticatorFactory;

type
  {
    Keeps track of the currently active authenticator, manages it and provides
    authentication-related services.
  }
  TKAuthenticationHost = class(TEFComponent)
  private
    FFactory: TKAuthenticatorFactory;
    FCurrentAuthenticator: TKAuthenticator;
    FAuthenticatorId: string;
    function GetCurrentAuthenticator: TKAuthenticator;
    class function GetAuthenticatorIdFromEnvironment: string; static;
  public
    constructor Create(const AFactory: TKAuthenticatorFactory);
    destructor Destroy; override;
    {
      Queries the configuration and returns True if the Null authenticator
      is going to be used, and False otherwise.
    }
    class function IsNullAuthenticator: Boolean;
    {
      Gives access to the current authenticator, created on demand.

      The Id of the authenticator is fetched from the AuthenticatorId
      item of the secure configuration. The default value is the Id of the
      Null authenticator.
    }
    property CurrentAuthenticator: TKAuthenticator
      read GetCurrentAuthenticator;
  end;

implementation

uses
  SysUtils,
  EF.StrUtils, EF.Localization, EF.Types,
  Kitto.Environment;

const
  NULL_AUTHENTICATOR = 'Null';

var
  _EWAuthenticatorRegistry: TKAuthenticatorRegistry;

function EWAuthenticatorRegistry: TKAuthenticatorRegistry;
begin
  if not Assigned(_EWAuthenticatorRegistry) then
    _EWAuthenticatorRegistry := TKAuthenticatorRegistry.Create;
  Result := _EWAuthenticatorRegistry;
end;

var
  _EWAuthenticatorFactory: TKAuthenticatorFactory;

function EWAuthenticatorFactory: TKAuthenticatorFactory;
begin
  if not Assigned(_EWAuthenticatorFactory) then
    _EWAuthenticatorFactory := TKAuthenticatorFactory.Create(EWAuthenticatorRegistry);
  Result := _EWAuthenticatorFactory;
end;

{ TKNullAuthenticator }

procedure TKNullAuthenticator.InternalDefineAuthenticationData(
  const AAuthenticationData: TEFNode);
begin
  // No authentication data required.
end;

procedure TKNullAuthenticator.InternalAuthenticate(
  const AAuthenticationData: TEFNode);
begin
  // Authentication is always granted.
end;

{ TKAuthenticatorRegistry }

procedure TKAuthenticatorRegistry.RegisterClass(const AClass: TKAuthenticatorClass);
begin
  inherited RegisterClass(AClass.GetClassId, AClass);
end;

procedure TKAuthenticatorRegistry.UnregisterClass(const AClass: TKAuthenticatorClass);
begin
  inherited UnregisterClass(AClass.GetClassId);
end;

{ TKAuthenticatorFactory }

function TKAuthenticatorFactory.CreateObject(const AClassId: string): TKAuthenticator;
begin
  Result := inherited CreateObject(AClassId) as TKAuthenticator;
end;

{ TKAuthenticator }

procedure TKAuthenticator.AfterConstruction;
begin
  inherited;
  FAuthData := TEFNode.Create;
  FAuthMacroExpander := TEFNodeMacroExpander.Create(FAuthData, 'Auth');
  FEnvironment := Environment;
  FEnvironment.MacroExpansionEngine.AddExpander(FAuthMacroExpander);
end;

destructor TKAuthenticator.Destroy;
begin
  FEnvironment.MacroExpansionEngine.RemoveExpander(FAuthMacroExpander);
  FreeAndNil(FAuthMacroExpander);
  FreeAndNil(FAuthData);
  inherited;
end;

procedure TKAuthenticator.DefineAuthenticationData(
  const AAuthenticationData: TEFNode);
begin
  Assert(Assigned(AAuthenticationData));

  InternalDefineAuthenticationData(AAuthenticationData);
end;

function TKAuthenticator.GetUserId: string;
begin
  Result := 'PUBLIC';
end;

procedure TKAuthenticator.InternalBeforeAuthenticate(
  const AAuthenticationData: TEFNode);
begin
end;

procedure TKAuthenticator.InternalAfterAuthenticate(
  const AAuthenticationData: TEFNode);
begin
end;

class function TKAuthenticator.InternalGetClassId: string;
begin
  Result := StripPrefixAndSuffix(inherited InternalGetClassId, 'K', 'Authenticator');
end;

procedure TKAuthenticator.Authenticate(
  const AAuthenticationData: TEFNode);
begin
  Assert(Assigned(AAuthenticationData));

  // Make sure the macros are enabled while authenticating.
  AuthenticationData.Assign(AAuthenticationData);
  try
    InternalBeforeAuthenticate(AAuthenticationData);
    InternalAuthenticate(AAuthenticationData);
    InternalAfterAuthenticate(AAuthenticationData);
    // Pick up any data changed by InternalAfterAuthenticate.
    AuthenticationData.Assign(AAuthenticationData);
  except
    // Make sure we don't expand any macro that hasn't passed
    // authentication.
    AuthenticationData.Clear;
    raise;
  end;
end;

{ TKAuthenticationHost }

constructor TKAuthenticationHost.Create(
  const AFactory: TKAuthenticatorFactory);
begin
  inherited Create;
  FFactory := AFactory;
end;

destructor TKAuthenticationHost.Destroy;
begin
  FreeAndNil(FCurrentAuthenticator);
  inherited;
end;

function TKAuthenticationHost.GetCurrentAuthenticator: TKAuthenticator;
var
  LConfig: TEFNode;
  I: Integer;
begin
  if not Assigned(FCurrentAuthenticator) then
  begin
    Assert(Assigned(FFactory));

    if FAuthenticatorId = '' then
      FAuthenticatorId := GetAuthenticatorIdFromEnvironment;
    FCurrentAuthenticator := FFactory.CreateObject(FAuthenticatorId);
    LConfig := Environment.Config.FindNode('Authentication');
    if Assigned(LConfig) then
      for I := 0 to LConfig.ChildCount - 1 do
        FCurrentAuthenticator.Config.AddChild(TEFNode.Clone(LConfig.Children[I]));
  end;
  Result := FCurrentAuthenticator;
end;

class function TKAuthenticationHost.GetAuthenticatorIdFromEnvironment: string;
begin
  Result := Environment.Config.GetExpandedString('Authentication/Type', NULL_AUTHENTICATOR);
end;

class function TKAuthenticationHost.IsNullAuthenticator: Boolean;
begin
  Result := GetAuthenticatorIdFromEnvironment = NULL_AUTHENTICATOR;
end;

{ TKClassicAuthenticator }

function TKClassicAuthenticator.GetUserId: string;
begin
  Result := AuthenticationData.GetString('UserName');
end;

procedure TKClassicAuthenticator.InternalDefineAuthenticationData(
  const AAuthData: TEFNode);
begin
  AAuthData.SetString('UserName', '');
  AAuthData.SetString('UserPassword', '');
  //AAuthenticationData.SetString('Sys/VisibleItemNames',
  //  '"UserName=' + _('User name') + '","UserPassword=' + _('Password') + '"');
end;

initialization
  EWAuthenticatorRegistry.RegisterClass(TKNullAuthenticator);

finalization
  EWAuthenticatorRegistry.UnregisterClass(TKNullAuthenticator);
  FreeAndNil(_EWAuthenticatorFactory);
  FreeAndNil(_EWAuthenticatorRegistry);

end.
