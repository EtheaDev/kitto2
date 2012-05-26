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

///	<summary>
///	  Defines the base authenticator and related classes and services.
///	  Authenticators allow the creation of applications that require user
///	  anthentication at startup.
///	</summary>
unit Kitto.Auth;

{$I Kitto.Defines.inc}

interface

uses
  Classes,
  EF.Macros, EF.Types, EF.Classes, EF.Tree;

type
  ///	<summary>
  ///	  <para>Abstract base authenticator. An authenticator defines a method
  ///	  for user authentication.</para>
  ///	  <para>Only one authenticator may be active at any one time.</para>
  ///	  <para>Applications wanting to use a custom authentication scheme should
  ///	  create and register an authenticator, and then make it active through
  ///	  the configuration.</para>
  ///	</summary>
  TKAuthenticator = class(TEFComponent)
  private
    FAuthData: TEFNode;
    FMacroExpander: TEFMacroExpander;
    FIsAuthenticated: Boolean;
    procedure ClearAuthData;
  protected
    function GetIsAuthenticated: Boolean; virtual;

    ///	<summary>Implements the IsClearPassword property.</summary>
    function GetIsClearPassword: Boolean; virtual;

    ///	<summary>Called at the beginning of the authentication process, before
    ///	InternalAuthenticate.</summary>
    procedure InternalBeforeAuthenticate(
      const AAuthenticationData: TEFNode); virtual;

    ///	<summary>Called at the end of the authentication process, in case of
    ///	successful authentication.</summary>
    procedure InternalAfterAuthenticate(
      const AAuthenticationData: TEFNode); virtual;

    ///	<summary>Implements Authenticate. Descendants should verify that
    ///	AAuthData contains all required items, query whatever authentication
    ///	mechanism they encapsulate, and return True if a match is found and
    ///	False otherwise.</summary>
    function InternalAuthenticate(const AAuthData: TEFNode): Boolean; virtual; abstract;

    ///	<summary>Implements DefineAuthData.</summary>
    procedure InternalDefineAuthData(
      const AAuthenticationData: TEFNode); virtual; abstract;

    ///	<summary>This function should return a unique user identifier, to be
    ///	used for example for access control. The default implementation returns
    ///	'PUBLIC', while a descendant will return the user name or something
    ///	else, as needed.</summary>
    function GetUserName: string; virtual;

    ///	<summary>For password-based authenticators, this function should return
    ///	the current user's password (or hash). The default implementation
    ///	returns a blank string.</summary>
    function GetPassword: string; virtual;

    ///	<summary>Changes the current user's password in whatever underlying
    ///	storage the authenticator uses. Only makes sense for password-based
    ///	authenticator. The default implementation does nothing.</summary>
    ///	<remarks>After calling this method, the user stays authenticated and
    ///	the session's password is updated (the password used when first
    ///	authenticating is lost). This allows a usr to change his password more
    ///	than once per session.</remarks>
    procedure SetPassword(const AValue: string); virtual;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
  public
    ///	<summary>
    ///	  <para>Receives an empty node which it should fill with the
    ///	  definitions (names and types, not values) of all required auth items.
    ///	  The system uses this information at login time, so that the user can
    ///	  supply any auth data needed by the currently active authenticator.
    ///	  The most common example of auth data is a UserName + Password
    ///	  combination.</para>
    ///	  <para>If no auth items are defined, then the system will not prompt
    ///	  the user but will still call Authenticate passing in an empty
    ///	  node.</para>
    ///	</summary>
    procedure DefineAuthData(const AAuthData: TEFNode);

    ///	<summary>Checks whether the specified auth data designates a valid user
    ///	or not, and returns False if the authentication fails.</summary>
    function Authenticate(const AAuthData: TEFNode): Boolean;

    ///	<summary>Gives access to a copy of the auth data that was last passed
    ///	to Authenticate (and possibly modified by the object during
    ///	authentication).</summary>
    property AuthData: TEFNode read FAuthData;

    ///	<summary>Clears AuthData and turns off IsAuthenticated.</summary>
    procedure Logout;

    ///	<summary>A unique identifier for the currently logged in user. The
    ///	value depends on the particular descendant. By default, it's
    ///	'PUBLIC'.</summary>
    property UserName: string read GetUserName;

    ///	<summary>For password-based authenticators, returns the current user's
    ///	password or hash. The default implementation returns a blank
    ///	string.</summary>
    property Password: string read GetPassword write SetPassword;

    ///	<summary>Returns True if the autheticator uses clear passwords, False
    ///	if hashing is used. Only meaningful for password-based authenticators.
    ///	By default, returns True.</summary>
    property IsClearPassword: Boolean read GetIsClearPassword;

    ///	<summary>Returns True if authentication has successfully taken
    ///	place.</summary>
    property IsAuthenticated: Boolean read GetIsAuthenticated;

    ///	<summary>
    ///	  Access to the authenticator macro expander, that expands auth data.
    ///	</summary>
    property MacroExpander: TEFMacroExpander read FMacroExpander;
  end;
  TKAuthenticatorClass = class of TKAuthenticator;

  ///	<summary>
  ///	  <para>An abstract authenticator that requires UserName and Password as
  ///	  auth data.</para>
  ///	  <para>How the auth data is checked is deferred to the concrete
  ///	  descendants. The value of the UserName auth item is also used as the
  ///	  value for the UserName property.</para>
  ///	</summary>
  TKClassicAuthenticator = class(TKAuthenticator)
  protected
    procedure InternalDefineAuthData(
      const AAuthData: TEFNode); override;
    function GetUserName: string; override;
    function GetPassword: string; override;
    procedure SetPassword(const AValue: string); override;
  end;

  ///	<summary>The Null authenticator does not require authentication data and
  ///	always grants authentication. It is used by default.</summary>
  TKNullAuthenticator = class(TKAuthenticator)
  protected
    procedure InternalDefineAuthData(
      const AAuthData: TEFNode); override;
    function InternalAuthenticate(
      const AAuthData: TEFNode): Boolean; override;
    function GetIsAuthenticated: Boolean; override;
  end;

  ///	<summary>This class holds a list of registered authenticator
  ///	classes.</summary>
  TKAuthenticatorRegistry = class(TEFRegistry)
  private
    class var FInstance: TKAuthenticatorRegistry;
    class function GetInstance: TKAuthenticatorRegistry; static;
  public
    class destructor Destroy;
    class property Instance: TKAuthenticatorRegistry read GetInstance;

    ///	<summary>Adds an authenticator class to the registry.</summary>
    procedure RegisterClass(const AId: string; const AClass: TKAuthenticatorClass);
  end;

  ///	<summary>Creates authenticators by Id.</summary>
  TKAuthenticatorFactory = class(TEFFactory)
  private
    class var FInstance: TKAuthenticatorFactory;
    class function GetInstance: TKAuthenticatorFactory; static;
  public
    class destructor Destroy;
    class property Instance: TKAuthenticatorFactory read GetInstance;

    ///	<summary>Creates and returns an instance of the authenticator class
    ///	identified by AClassId. Raises an exception if said class is not
    ///	registered.</summary>
    function CreateObject(const AClassId: string): TKAuthenticator;
  end;

implementation

uses
  SysUtils,
  EF.StrUtils, EF.Localization;

{ TKNullAuthenticator }

procedure TKNullAuthenticator.InternalDefineAuthData(const AAuthData: TEFNode);
begin
  // No authentication data required.
end;

function TKNullAuthenticator.GetIsAuthenticated: Boolean;
begin
  Result := True;
end;

function TKNullAuthenticator.InternalAuthenticate(const AAuthData: TEFNode): Boolean;
begin
  Result := True;
end;

{ TKAuthenticatorRegistry }

class destructor TKAuthenticatorRegistry.Destroy;
begin
  FreeAndNil(FInstance);
end;

class function TKAuthenticatorRegistry.GetInstance: TKAuthenticatorRegistry;
begin
  if FInstance = nil then
    FInstance := TKAuthenticatorRegistry.Create;
  Result := FInstance;
end;

procedure TKAuthenticatorRegistry.RegisterClass(const AId: string; const AClass: TKAuthenticatorClass);
begin
  inherited RegisterClass(AId, AClass);
end;

{ TKAuthenticatorFactory }

function TKAuthenticatorFactory.CreateObject(const AClassId: string): TKAuthenticator;
begin
  Result := inherited CreateObject(AClassId) as TKAuthenticator;
end;

class destructor TKAuthenticatorFactory.Destroy;
begin
  FreeAndNil(FInstance);
end;

class function TKAuthenticatorFactory.GetInstance: TKAuthenticatorFactory;
begin
  if FInstance = nil then
    FInstance := TKAuthenticatorFactory.Create(TKAuthenticatorRegistry.Instance);
  Result := FInstance;
end;

{ TKAuthenticator }

procedure TKAuthenticator.AfterConstruction;
begin
  inherited;
  FAuthData := TEFNode.Create;
  FMacroExpander := TEFTreeMacroExpander.Create(FAuthData, 'Auth');
  FIsAuthenticated := False;
end;

destructor TKAuthenticator.Destroy;
begin
  FreeAndNil(FMacroExpander);
  FreeAndNil(FAuthData);
  inherited;
end;

procedure TKAuthenticator.DefineAuthData(const AAuthData: TEFNode);
var
  I: Integer;
begin
  Assert(Assigned(AAuthData));

  InternalDefineAuthData(AAuthData);
  // Get meaningful defaults.
  for I := 0 to AAuthData.ChildCount - 1 do
    AAuthData.Children[I].AssignValue(Config.FindNode('Defaults/' + AAuthData.Children[I].Name));
end;

function TKAuthenticator.GetIsAuthenticated: Boolean;
begin
  Result := FIsAuthenticated;
end;

function TKAuthenticator.GetIsClearPassword: Boolean;
begin
  Result := True;
end;

function TKAuthenticator.GetPassword: string;
begin
  Result := '';
end;

function TKAuthenticator.GetUserName: string;
begin
  Result := 'PUBLIC';
end;

procedure TKAuthenticator.InternalBeforeAuthenticate(
  const AAuthenticationData: TEFNode);
begin
end;

procedure TKAuthenticator.Logout;
begin
  ClearAuthData;
  FIsAuthenticated := False;
end;

procedure TKAuthenticator.SetPassword(const AValue: string);
begin
end;

procedure TKAuthenticator.ClearAuthData;
begin
  AuthData.Clear;
  DefineAuthData(AuthData);
end;

procedure TKAuthenticator.InternalAfterAuthenticate(
  const AAuthenticationData: TEFNode);
begin
end;

function TKAuthenticator.Authenticate(const AAuthData: TEFNode): Boolean;
begin
  Assert(Assigned(AAuthData));

  Result := False;
  // Make sure the macros are enabled while authenticating.
  AuthData.Assign(AAuthData);
  try
    InternalBeforeAuthenticate(AAuthData);
    Result := InternalAuthenticate(AAuthData);
    FIsAuthenticated := Result;
    if Result then
    begin
      InternalAfterAuthenticate(AAuthData);
      // Pick up any data changed by InternalAfterAuthenticate.
      AuthData.Assign(AAuthData);
    end;
  finally
    if not Result then
      // Make sure we don't expand any macro that hasn't passed
      // authentication.
      Logout;
  end;
end;

{ TKClassicAuthenticator }

function TKClassicAuthenticator.GetPassword: string;
begin
  Result := AuthData.GetString('Password');
end;

function TKClassicAuthenticator.GetUserName: string;
begin
  Result := AuthData.GetString('UserName');
end;

procedure TKClassicAuthenticator.InternalDefineAuthData(
  const AAuthData: TEFNode);
begin
  AAuthData.SetString('UserName', '');
  AAuthData.SetString('Password', '');
end;

procedure TKClassicAuthenticator.SetPassword(const AValue: string);
begin
  inherited;
end;

initialization
  TKAuthenticatorRegistry.Instance.RegisterClass('Null', TKNullAuthenticator);

finalization
  TKAuthenticatorRegistry.Instance.UnregisterClass('Null');

end.

