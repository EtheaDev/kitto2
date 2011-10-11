unit Kitto.Auth.DB;

{$I Kitto.Defines.inc}

interface

uses
  EF.DB, EF.Tree,
  Kitto.Auth;

type
  {
    Represents a user record fetched from the database. It is used internally
    as a helper class.
  }
  TKAuthUser = class
  private
    FName: string;
    FPasswordHash: string;
  public
    property Name: string read FName write FName;
    property PasswordHash: string read FPasswordHash write FPasswordHash;
  end;

  {
    The DB authenticator uses a custom table in the database to authenticate
    users. The table has fixed name and structure. To use a different table
    name or structure, create a descendant.
    The authenticator needs the same items as its ancestor
    TKClassicAuthenticator.

    In order for this authenticator to work, it is required that the database
    contains a table called EW_USERS with the following structure:
    @table(
      @rowHead(@cell(Field Name)@cell(Data Type)@cell(Description))
      @row(@cell(EW_USER_NAME)@cell(string)@cell(Unique user identifier))
      @row(@cell(EW_PASSWORD_HASH)@cell(string)@cell(MD5 hash of the user's password))
      @row(@cell(EW_IS_ACTIVE)@cell(bit, number)@cell(1 for active users))
    )
    When Authenticate is called, the authenticator will fetch the user record
    (if active) and check the supplied password against the MD5 hash stored
    there. You can use clear-text passwords instead of MD5 hashes through the
    IsClearPassword configuration item. You can also override the SQL select
    statement that is used to get the user record through the
    ReadUserCommandText item, which allows you to use a different structure
    without the need to define an inherited authenticator.

    Configuration items (should be specified in SecureConfiguration's slice
    Authentication.Authenticator):
    @table(
      @row(
        @cell(IsClearPassword)@cell(Boolean)@cell(Optional, default False)
        @cell(Set this item to True to signify that the password is stored in
          clear, and not hashed, in the database.))
      @row(
        @cell(ReadUserCommandText)@cell(String)@cell(Optional)
        @cell(A SQL select statement that selects a single user record,
          with the fields EW_USER_NAME and EW_PASSWORD_HASH and a single
          string parameter that will be filled in with the user name.

          Example:
            select UNAME as EW_USER_NAME, pwd as EW_PASSWORD_HASH
            from USERS where ENABLED = 1 and UNAME = :P1

          Any additional fields returned by the statement are kept as
          part of the authentication data, in a Custom slice.

          Example:
            select EW_USER_NAME, Kitto._PASSWORD_HASH, SOMEFIELD
            from EW_USERS where EW_IS_ACTIVE = 1 and EW_USER_NAME = :P1

          The item Custom.SOMEFIELD will be available as part of the
          authentication data (and as such, through the authentication-related
          macros as well) for as long as the user is logged in. The item will
          be of whatever data type SOMEFIELD is. You can have more than one
          such items. This technique is useful to apply fixed user-dependent
          filters to data sets and data partitioning among users, for example.))
      @row(
        @cell(IsPassepartoutEnabled)@cell(Boolean)@cell(Optional, default False)
        @cell(Set this item to True to signify that the passepartout password is
        enabled (that means that a user can authenticate by prompting its own
        password or the passepartout password.)
        If the user is granted access because of the use of PassepartoutPassword
        as login password, the IsPassepartoutAuthentication Boolean item is set
        to True into AuthenticationData (accessible through the EW Authenticator
        that is accessibile through the EW Environment object). Use this configuration
        item to discrimine between a regular user/password login and a user/passepartout
        login.
      @row(
        @cell(PassepartoutPassword)@cell(String)@cell(Optional)
        @cell(A password to be used as a passepartout for every user account.
        Setting this parameter has no effect if IsPassepartoutEnabled is False.
        The password is meant to be clear or hashed with respect to the IsClearPassword
        parameter.)
    )
  }
  TKDBAuthenticator = class(TKClassicAuthenticator)
  protected
    {
      Creates and returns an object with the user data read from the database.
      It is actually a template method that calls a set of virtual methods to
      do its job.
    }
    function CreateAndReadUser(const AUserName: string; const AAuthData: TEFNode): TKAuthUser;
    {
      Creates and returns an empty instance of TKAuthenticationUser. Override
      this method if you need to use a descendant instead.
    }
    function CreateUser: TKAuthUser; virtual;
    {
      Returns the SQL statement to be executed just after authentication
      succeeded.
    }
    function GetAfterAuthenticateCommandText: string; virtual;
    {
      Returns the SQL statement to be used to read the user data from the
      database. Override this method to change the name or the structure of
      the predefined table of users.
    }
    function GetReadUserSQL(const AUserName: string): string; virtual;

    {
      Extracts from AAuthenticationData the supplied password, in order to use
      it in authentication attempt. If AHashNeeded is True, the password hash
      will be returned instead of the clear password.
    }
    function GetSuppliedPasswordHash(const AAuthData: TEFNode;
      const AHashNeeded: Boolean): string; virtual;
    {
      Extracts from AAuthenticationData the supplied user name, in order to use
      it in authentication attempt.
    }
    function GetSuppliedUserName(
      const AAuthData: TEFNode): string; virtual;
    {
      Executes the AfterAuthenticateCommandText, if any provided.
    }
    procedure InternalAfterAuthenticate(const AAuthData: TEFNode); override;
    {
      Performs authentication against the EW database, also considering the
      passepartout mechanism.
    }
    procedure InternalAuthenticate(const AAuthData: TEFNode); override;
    {
      Returns True if the passepartout mechanism is enabled and the supplied
      password is matching the passpartout password.
    }
    function IsPassepartoutAuthentication(
      const ASuppliedPasswordHash: string): Boolean; virtual;
    {
      Returns True if ASuppliedPasswordHash is to be considered correctly matching
      with respect to AStoredPasswordHash value.
      This normally means that they are the same value. In descendant classes, if
      you want to ignore the password matching, simply override this method to
      return True.
    }
    function IsPasswordMatching(const ASuppliedPasswordHash: string;
      const AStoredPasswordHash: string): Boolean; virtual;
    {
      Returns True if AUserName is a valid user name with respect to the database
      specified for authentication. It is implemented using GetReadUserSQL to
      perform a query against the database to see if authentication data is
      available for this user.
    }
    function IsValidUserName(const AUserName: string): Boolean; virtual;
    {
      Reads data from the current record of ADataSet and stores it into AUser.
      Override this method if your table of users has a non-default structure.
      This method is usually overridden together with GetReadUserSQL, and
      possibly CreateUser.
    }
    procedure ReadUserFromRecord(const AUser: TKAuthUser;
      const ADBQuery: TEFDBQuery; const AAuthData: TEFNode); virtual;
  end;

implementation

uses
  SysUtils, Classes, Variants,
  EF.Localization,  EF.Types, EF.StrUtils, EF.DB.Utils,
  Kitto.Types, Kitto.Environment;

{ TKDBAuthenticator }

function TKDBAuthenticator.CreateAndReadUser(
  const AUserName: string; const AAuthData: TEFNode): TKAuthUser;
var
  LQuery: TEFDBQuery;
begin
  Result := CreateUser;
  try
    LQuery := Environment.MainDBConnection.CreateDBQuery;
    try
      LQuery.CommandText := GetReadUserSQL(AUserName);
      if LQuery.Params.Count <> 1 then
        raise EKError.CreateFmt(_('Wrong authentication query text: %s'), [LQuery.CommandText]);
      LQuery.Params[0].AsString := AUserName;
      LQuery.Open;
      try
        if LQuery.DataSet.IsEmpty then
          raise EKError.Create(_('Invalid login.'));
        ReadUserFromRecord(Result, LQuery, AAuthData);
      finally
        LQuery.Close;
      end;
    finally
      FreeAndNil(LQuery);
    end;
  except
    Result.Free;
    raise;
  end;
end;

function TKDBAuthenticator.CreateUser: TKAuthUser;
begin
  Result := TKAuthUser.Create;
end;

function TKDBAuthenticator.IsPasswordMatching(const ASuppliedPasswordHash,
  AStoredPasswordHash: string): Boolean;
begin
  Result := ASuppliedPasswordHash = AStoredPasswordHash;
end;

function TKDBAuthenticator.GetAfterAuthenticateCommandText: string;
begin
  Result := Config.GetExpandedString('AfterAuthenticateCommandText');
end;

function TKDBAuthenticator.GetReadUserSQL(const AUserName: string): string;
begin
  Result := Config.GetString('ReadUserCommandText',
    'select EW_USER_NAME, Kitto._PASSWORD_HASH from EW_USERS ' +
    'where EW_IS_ACTIVE = 1 and EW_USER_NAME = :EW_USER_NAME');
end;

function TKDBAuthenticator.GetSuppliedPasswordHash(
  const AAuthData: TEFNode; const AHashNeeded: Boolean): string;
begin
  Result := Environment.MacroExpansionEngine.Expand(AAuthData.GetString('UserPassword'));
  if AHashNeeded then
    Result := GetStringHash(Result);
end;

function TKDBAuthenticator.GetSuppliedUserName(const AAuthData: TEFNode): string;
begin
  Result := Environment.MacroExpansionEngine.Expand(AAuthData.GetString('UserName'));
end;

procedure TKDBAuthenticator.InternalAfterAuthenticate(const AAuthData: TEFNode);
var
  LCommand: TEFDBCommand;
  LAfterAuthenticateCommandText: string;
begin
  inherited;
  LAfterAuthenticateCommandText := GetAfterAuthenticateCommandText;

  if LAfterAuthenticateCommandText <> '' then
  begin
    LCommand := Environment.MainDBConnection.CreateDBCommand;
    try
      LCommand.CommandText := LAfterAuthenticateCommandText;
      LCommand.Execute;
    finally
      FreeAndNil(LCommand);
    end;
  end;
end;

procedure TKDBAuthenticator.InternalAuthenticate(const AAuthData: TEFNode);
var
  LSuppliedUserName: string;
  LSuppliedPasswordHash: string;
  LStoredPasswordHash: string;
  LPassepartoutAuthentication: Boolean;
begin
  LSuppliedUserName := GetSuppliedUserName(AAuthData);
  LSuppliedPasswordHash := GetSuppliedPasswordHash(AAuthData, not Config.GetBoolean('IsClearPassword'));
  LPassepartoutAuthentication := IsPassepartoutAuthentication(LSuppliedPasswordHash);

  with CreateAndReadUser(LSuppliedUserName, AAuthData) do
  begin
    try
      LStoredPasswordHash := PasswordHash;
    finally
      Free;
    end;
  end;

  if not IsPasswordMatching(LSuppliedPasswordHash, LStoredPasswordHash)
      and (not LPassepartoutAuthentication) then
    raise EKError.Create(_('Invalid login.'));

  if LPassepartoutAuthentication then
    AAuthData.SetBoolean('IsPassepartoutAuthentication', True);
end;

function TKDBAuthenticator.IsPassepartoutAuthentication(const ASuppliedPasswordHash: string): Boolean;
var
  LIsPassepartoutEnabled: Boolean;
  LPassepartoutPassword: string;
begin
  LIsPassepartoutEnabled := Config.GetBoolean('IsPassepartoutEnabled', False);
  if LIsPassepartoutEnabled then
    LPassepartoutPassword := Config.GetString('PassepartoutPassword', '');
  Result := LIsPassepartoutEnabled and (ASuppliedPasswordHash = LPassepartoutPassword);
end;

function TKDBAuthenticator.IsValidUserName(const AUserName: string): Boolean;
var
  LQuery: TEFDBQuery;
begin
  Result := False;

  LQuery := Environment.MainDBConnection.CreateDBQuery;
  try
    LQuery.CommandText := GetReadUserSQL(AUserName);
    if LQuery.Params.Count <> 1 then
      raise EKError.CreateFmt(_('Wrong authentication query text: %s'), [LQuery.CommandText]);
    LQuery.Params[0].AsString := AUserName;
    LQuery.Open;
    try
      if LQuery.DataSet.IsEmpty then
        Result := False
      else
        Result := True;
    finally
      LQuery.Close;
    end;
  finally
    FreeAndNil(LQuery);
  end;
end;

procedure TKDBAuthenticator.ReadUserFromRecord(const AUser: TKAuthUser;
  const ADBQuery: TEFDBQuery; const AAuthData: TEFNode);
var
  I: Integer;
begin
  Assert(Assigned(AUser));
  Assert(Assigned(ADBQuery));

  AUser.Name := ADBQuery.DataSet['EW_USER_NAME'];
  AUser.PasswordHash := ADBQuery.DataSet['EW_PASSWORD_HASH'];

  // First N fields in the dataset go to the defined auth data nodes.
  Assert(ADBQuery.DataSet.FieldCount >= AAuthData.ChildCount);
  for I := 0 to AAuthData.ChildCount - 1 do
    AAuthData.Children[I].AssignFieldValue(ADBQuery.DataSet.Fields[I]);
  // All fields go to auth data under their names.
  AAuthData.AddFieldsAsChildren(ADBQuery.DataSet.Fields);
end;

{ TKAuthenticationUser }

initialization
  EWAuthenticatorRegistry.RegisterClass(TKDBAuthenticator);

finalization
  EWAuthenticatorRegistry.UnregisterClass(TKDBAuthenticator);

end.

