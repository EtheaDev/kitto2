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

/// <summary>
///  Defines the DB authenticator, an authenticator that uses a custom
///  table on the database to store users and passwords.
/// </summary>
unit Kitto.Auth.DB;

{$I Kitto.Defines.inc}

interface

uses
  EF.DB, EF.Tree,
  Kitto.Auth;

const
  DEFAULT_READUSERCOMMANDTEXT =
    'select USER_NAME, PASSWORD_HASH, EMAIL_ADDRESS, MUST_CHANGE_PASSWORD from KITTO_USERS where IS_ACTIVE = 1 and USER_NAME = :USER_NAME';
  DEFAULT_SETPASSWORDCOMMANDTEXT =
    'update KITTO_USERS set PASSWORD_HASH = :PASSWORD_HASH, MUST_CHANGE_PASSWORD = 0 where IS_ACTIVE = 1 and USER_NAME = :USER_NAME';
  DEFAULT_RESETPASSWORDCOMMANDTEXT =
    'update KITTO_USERS set PASSWORD_HASH = :PASSWORD_HASH, MUST_CHANGE_PASSWORD = 1 where IS_ACTIVE = 1 and EMAIL_ADDRESS = :EMAIL_ADDRESS';

type
  /// <summary>User data read from the database. Used internally as a helper
  /// class.</summary>
  TKAuthUser = class
  private
    FName: string;
    FPasswordHash: string;
  public
    property Name: string read FName write FName;
    property PasswordHash: string read FPasswordHash write FPasswordHash;
  end;

  /// <summary>
  ///   <para>The DB authenticator uses a custom table in the database to
  ///   authenticate users. The table has fixed name and structure. To use a
  ///   different table name or structure, set parameters or create a
  ///   descendant. The authenticator needs the same auth items as its ancestor
  ///   <see cref="TKClassicAuthenticator" />.</para>
  ///   <para>In order for this authenticator to work, it is required that the
  ///   database contains a table called KITTO_USERS with the following
  ///   structure:</para>
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Term</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>USER_NAME</term>
  ///       <description>String uniquely identifying the user.</description>
  ///     </item>
  ///     <item>
  ///       <term>EMAIL_ADDRESS</term>
  ///       <description>Unique e-mail address for the user. This is optional,
  ///       and only used by ResetPassword.</description>
  ///     </item>
  ///     <item>
  ///       <term>PASSWORD_HASH</term>
  ///       <description>String containing the MD5 hash of the user's
  ///       password.</description>
  ///     </item>
  ///     <item>
  ///       <term>IS_ACTIVE</term>
  ///       <description>Set to 0 to disable (ignore) a user. Disabled users
  ///       will not be able to log in.</description>
  ///     </item>
  ///   </list>
  ///   <para>When Authenticate is called, the authenticator fetches the user
  ///   record (if active) and checks the supplied password against the MD5
  ///   hash stored there. You can use clear-text passwords instead of MD5
  ///   hashes if you set the IsClearPassword parameter to True. You can also
  ///   override the SQL select statement used to get the user record through
  ///   the ReadUserCommandText parameter, which allows you to use a different
  ///   structure without the need to define an inherited authenticator.</para>
  ///   <para>Parameters:</para>
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Term</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>IsClearPassword</term>
  ///       <description>Set this item to True to signify that the password is
  ///       stored in clear, and not hashed, in the database. Default
  ///       False.</description>
  ///     </item>
  ///     <item>
  ///       <term>ReadUserCommandText</term>
  ///       <description>A SQL select statement that selects a single user
  ///       record, with the fields USER_NAME and PASSWORD_HASH and a single
  ///       string parameter that will be filled in with the user name.
  ///       Example: select UNAME as USER_NAME, pwd as PASSWORD_HASH from USERS
  ///       where ENABLED = 1 and UNAME = :P1 Any additional fields returned by
  ///       the statement are kept as part of the auth data. Example: select
  ///       USER_NAME, PASSWORD_HASH, SOMEFIELD from USERS where IS_ACTIVE = 1
  ///       and USER_NAME = :P1 The item SOMEFIELD will be available as part of
  ///       the auth data (and as such, through the authentication-related
  ///       macros as well) for as long as the user is logged in. The item will
  ///       be of whatever data type SOMEFIELD is. You can have more than one
  ///       such items. This technique is useful to apply fixed user-dependent
  ///       filters to data sets and data partitioning among users, for
  ///       example.</description>
  ///     </item>
  ///     <item>
  ///       <term>IsPassepartoutEnabled</term>
  ///       <description>Set this item to True to signify that the passepartout
  ///       password is enabled (that means that a user can authenticate wither
  ///       with her own password or the passepartout password). If the
  ///       passepartout password is used to log in, the
  ///       IsPassepartoutAuthentication Boolean item is set to True into
  ///       AuthData.</description>
  ///     </item>
  ///     <item>
  ///       <term>PassepartoutPassword</term>
  ///       <description>A password to be used as a passepartout for every user
  ///       account. Setting this parameter has no effect if
  ///       IsPassepartoutEnabled is not True. The value of this parameter
  ///       represents a cleartext or hashed password depending on the value of
  ///       IsClearPassword.</description>
  ///     </item>
  ///     <item>
  ///       <term>AfterAuthenticateCommandText</term>
  ///       <description>Optional SQL statement that will be executed just
  ///       after authentication. Supports macros, even authentication-related
  ///       ones.</description>
  ///     </item>
  ///     <item>
  ///       <term>SetPasswordCommandText</term>
  ///       <description>A SQL update statement that updates the password field
  ///       of a given user's record. The statement must have two params named
  ///       USER_NAME (that will be filled with the current user's name) and
  ///       PASSWORD_HASH (that will be filled with the new password or
  ///       password hash depending on the state of IsClearPassword). By
  ///       default, the standard KITTO_USERS table is updated. This statement
  ///       is executed when the authenticator's Password property is set in
  ///       code.</description>
  ///     </item>
  ///   </list>
  /// </summary>
  TKDBAuthenticator = class(TKClassicAuthenticator)
  strict protected
    function GetIsClearPassword: Boolean; override;
    procedure SetPassword(const AValue: string); override;

    /// <summary>Generates and returns a random password. Apply application-defined
    /// rules by overriding this method.</summary>
    function GenerateRandomPassword: string; virtual;

    /// <summary>Returns True if the passepartout mechanism is enabled and the
    /// supplied password matches the passpartout password.</summary>
    function InternalAuthenticate(const AAuthData: TEFNode): Boolean; override;

    function GetDatabaseName: string;

    /// <summary>Returns the SQL statement to be used to update the password
    /// (or password hash) in a user's record in the database. Override this
    /// method to change the name or the structure of the predefined table of
    /// users.</summary>
    /// <remarks>The statement should have two params named PASSWORD_HASH and
    /// USER_NAME that will be filled in with the data used to locate the
    /// record and update the password.</remarks>
    function GetSetPasswordCommandText: string;

    /// <summary>Returns the SQL statement to be used to reset the password
    /// (or password hash) in a user's record in the database. Override this
    /// method to change the name or the structure of the predefined table of
    /// users.</summary>
    /// <remarks>The statement should have two params named PASSWORD_HASH and
    /// EMAIL_ADDRESS that will be filled in with the data used to locate the
    /// record and update the password.</remarks>
    function GetResetPasswordCommandText: string;

    /// <summary>Creates and returns an object with the user data read from the
    /// database. It is actually a template method that calls a set of virtual
    /// methods to do its job.</summary>
    function CreateAndReadUser(const AUserName: string; const AAuthData: TEFNode): TKAuthUser;

    /// <summary>Executes the AfterAuthenticateCommandText, if any
    /// provided.</summary>
    procedure InternalAfterAuthenticate(const AAuthData: TEFNode); override;

    /// <summary>Creates and returns an empty instance of TKAuthUser. Override
    /// this method if you need to use a descendant instead.</summary>
    function CreateUser: TKAuthUser; virtual;

    /// <summary>Returns the SQL statement to be executed just after
    /// authentication succeeded.</summary>
    function GetAfterAuthenticateCommandText: string; virtual;

    /// <summary>Returns the SQL statement to be used to read the user data
    /// from the database. Override this method to change the name or the
    /// structure of the predefined table of users.</summary>
    function GetReadUserCommandText(const AUserName: string): string; virtual;

    procedure GetSuppliedAuthData(const AAuthData: TEFNode; const AHashNeeded: Boolean;
      out ASuppliedUserName, ASuppliedPasswordHash: string;
      out AIsPassepartoutAuthentication: Boolean); virtual;

    /// <summary>Extracts from AAuthData the supplied password, in order to use
    /// it in an authentication attempt. If AHashNeeded is True, the password
    /// hash will be returned instead of the clear password.</summary>
    function GetSuppliedPasswordHash(const AAuthData: TEFNode;
      const AHashNeeded: Boolean): string; virtual;

    /// <summary>Extracts from AAuthData the supplied user name, in order to
    /// use it in an authentication attempt.</summary>
    function GetSuppliedUserName(const AAuthData: TEFNode): string; virtual;

    /// <summary>True if passepartout mode is enabled and the supplied password
    /// matches the passepartout password.</summary>
    function IsPassepartoutAuthentication(
      const ASuppliedPasswordHash: string): Boolean; virtual;

    /// <summary>Returns True if ASuppliedPasswordHash matches
    /// AStoredPasswordHash. By default this means that they are the same
    /// value. A descendant might use different matching rules or disable
    /// matching altogether by overriding this method.</summary>
    function IsPasswordMatching(const ASuppliedPasswordHash: string;
      const AStoredPasswordHash: string): Boolean; virtual;

    /// <summary>Returns True if AUserName is a valid user name. It is
    /// implemented using GetReadUserSQL to perform a query against the
    /// database to see if authentication data is available for this
    /// user.</summary>
    function IsValidUserName(const AUserName: string): Boolean; virtual;

    /// <summary>
    ///   <para>Reads data from the current record of the specified DB query
    ///   and stores it into AUser.</para>
    ///   <para>Override this method if your table of users has a non-default
    ///   structure.</para>
    ///   <para>This method is usually overridden together with GetReadUserSQL,
    ///   and possibly also CreateUser.</para>
    /// </summary>
    procedure ReadUserFromRecord(const AUser: TKAuthUser;
      const ADBQuery: TEFDBQuery; const AAuthData: TEFNode); virtual;

    /// <summary>
    ///  Called by ResetPassword after generating the new random password
    ///  but before writing it to the database. Override this method to send
    ///  the generated password to the user so he can log in and change it.
    ///  The default implementation does nothing.
    /// </summary>
    /// <param name="AParams">
    ///  Contains all the params passed to ResetPassword plus the generated
    ///  password (in the "Password" node). This method is not supposed to
    ///  modify the params but can add custom ones if needed. They will be passed
    ///  back to the initiator of the password reset flow.
    /// </param>
    procedure BeforeResetPassword(const AParams: TEFNode); virtual;

    /// <summary>
    ///  Called by ResetPassword after writing the new random password
    ///  to the database but before committing the transaction. Override this method
    ///  if you need to perform additional database-related operations at this point
    ///  in time.
    ///  The default implementation does nothing.
    /// </summary>
    /// <param name="AParams">
    ///  Contains all the params passed to ResetPassword plus the generated
    ///  password (in the "Password" node). This method is not supposed to
    ///  modify the params but can add custom ones if needed. They will be passed
    ///  back to the initiator of the password reset flow.
    /// </param>
    procedure AfterResetPassword(const ADBConnection: TEFDBConnection; const AParams: TEFNode); virtual;
  public
    procedure ResetPassword(const AParams: TEFNode); override;
  end;

implementation

uses
  SysUtils
  , Classes
  , Variants
  , DB
  , EF.Localization
  , EF.Logger
  , EF.Types
  , EF.StrUtils
  , Kitto.Types
  , Kitto.Config
  , Kitto.DatabaseRouter
  , Kitto.Web.Session
  ;

{ TKDBAuthenticator }

procedure TKDBAuthenticator.AfterResetPassword(const ADBConnection: TEFDBConnection; const AParams: TEFNode);
begin
end;

procedure TKDBAuthenticator.BeforeResetPassword(const AParams: TEFNode);
begin
end;

function TKDBAuthenticator.CreateAndReadUser(
  const AUserName: string; const AAuthData: TEFNode): TKAuthUser;
var
  LDBConnection: TEFDBConnection;
  LDBQuery: TEFDBQuery;
begin
  Result := nil;
  LDBConnection := TKConfig.Instance.CreateDBConnection(GetDatabaseName);
  try
    LDBQuery := LDBConnection.CreateDBQuery;
    try
      LDBQuery.CommandText := GetReadUserCommandText(AUserName);
      if LDBQuery.Params.Count <> 1 then
        raise EKError.CreateFmt(_('Wrong authentication query text: %s'), [LDBQuery.CommandText]);
      LDBQuery.Params[0].AsString := AUserName;
      LDBQuery.Open;
      try
        if not LDBQuery.DataSet.IsEmpty then
        begin
          Result := TKAuthUser.Create;
          try
            ReadUserFromRecord(Result, LDBQuery, AAuthData);
          except
            Result.Free;
            raise;
          end;
        end;
      finally
        LDBQuery.Close;
      end;
    finally
      FreeAndNil(LDBQuery);
    end;
  finally
    FreeAndNil(LDBConnection);
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

function TKDBAuthenticator.GenerateRandomPassword: string;
begin
  Result := GetRandomString(8, '01');
end;

function TKDBAuthenticator.GetAfterAuthenticateCommandText: string;
begin
  Result := Config.GetExpandedString('AfterAuthenticateCommandText');
end;

function TKDBAuthenticator.GetDatabaseName: string;
var
  LDatabaseRouterNode: TEFNode;
begin
  LDatabaseRouterNode := Config.FindNode('DatabaseRouter');
  if Assigned(LDatabaseRouterNode) then
    Result := TKDatabaseRouterFactory.Instance.GetDatabaseName(
      LDatabaseRouterNode.AsString, Self, LDatabaseRouterNode)
  else
    Result := TKConfig.Instance.DatabaseName;
end;

function TKDBAuthenticator.GetIsClearPassword: Boolean;
begin
  Result := Config.GetBoolean('IsClearPassword');
end;

function TKDBAuthenticator.GetReadUserCommandText(const AUserName: string): string;
begin
  Result := Config.GetString('ReadUserCommandText',
    DEFAULT_READUSERCOMMANDTEXT);
end;

function TKDBAuthenticator.GetResetPasswordCommandText: string;
begin
  Result := Config.GetString('ResetPasswordCommandText', DEFAULT_RESETPASSWORDCOMMANDTEXT);
end;

function TKDBAuthenticator.GetSuppliedPasswordHash(
  const AAuthData: TEFNode; const AHashNeeded: Boolean): string;
begin
  Result := AAuthData.GetString('Password');
  TKConfig.Instance.MacroExpansionEngine.Expand(Result);
  if AHashNeeded then
    Result := GetStringHash(Result);
end;

function TKDBAuthenticator.GetSuppliedUserName(const AAuthData: TEFNode): string;
begin
  Result := AAuthData.GetString('UserName');
  TKConfig.Instance.MacroExpansionEngine.Expand(Result);
end;

procedure TKDBAuthenticator.InternalAfterAuthenticate(const AAuthData: TEFNode);

  function GetLocalDatabaseName: string;
  var
    LDatabaseRouterNode: TEFNode;
  begin
    LDatabaseRouterNode := Config.FindNode('AfterAuthenticateDatabaseRouter');
    if Assigned(LDatabaseRouterNode) then
      Result := TKDatabaseRouterFactory.Instance.GetDatabaseName(
        LDatabaseRouterNode.AsString, Self, LDatabaseRouterNode)
    else
      Result := TKConfig.Instance.DatabaseName;
  end;

var
  LDBConnection: TEFDBConnection;
  LAfterAuthenticateCommandText: string;
begin
  inherited;
  LAfterAuthenticateCommandText := GetAfterAuthenticateCommandText;

  if LAfterAuthenticateCommandText <> '' then
  begin
    LDBConnection := TKConfig.Instance.CreateDBConnection(GetLocalDatabaseName);
    try
      LDBConnection.ExecuteImmediate(LAfterAuthenticateCommandText);
    finally
      FreeAndNil(LDBConnection);
    end;
  end;
end;

procedure TKDBAuthenticator.GetSuppliedAuthData(
  const AAuthData: TEFNode; const AHashNeeded: Boolean;
  out ASuppliedUserName, ASuppliedPasswordHash: string;
  out AIsPassepartoutAuthentication: Boolean);
begin
  ASuppliedUserName := GetSuppliedUserName(AAuthData);
  ASuppliedPasswordHash := GetSuppliedPasswordHash(AAuthData, AHashNeeded);
  AIsPassepartoutAuthentication := IsPassepartoutAuthentication(ASuppliedPasswordHash);
end;

function TKDBAuthenticator.InternalAuthenticate(const AAuthData: TEFNode): Boolean;
var
  LSuppliedUserName: string;
  LSuppliedPasswordHash: string;
  LIsPassepartoutAuthentication: Boolean;
  LUser: TKAuthUser;
begin
  GetSuppliedAuthData(AAuthData, not IsClearPassword,
    LSuppliedUserName, LSuppliedPasswordHash, LIsPassepartoutAuthentication);

  if LSuppliedUserName <> '' then
  begin
    LUser := CreateAndReadUser(LSuppliedUserName, AAuthData);
    try
      if Assigned(LUser) then
      begin
        Result := IsPasswordMatching(LSuppliedPasswordHash, LUser.PasswordHash) or LIsPassepartoutAuthentication;
        if LIsPassepartoutAuthentication then
          AAuthData.SetBoolean('IsPassepartoutAuthentication', True);
      end
      else
        Result := False;
    finally
      FreeAndNil(LUser);
    end;
  end
  else
    Result := False;
end;

function TKDBAuthenticator.IsPassepartoutAuthentication(const ASuppliedPasswordHash: string): Boolean;
var
  LIsPassepartoutEnabled: Boolean;
  LPassepartoutPassword: string;
begin
  LIsPassepartoutEnabled := Config.GetBoolean('IsPassepartoutEnabled', False);
  if LIsPassepartoutEnabled then
    LPassepartoutPassword := Config.GetString('PassepartoutPassword', '');
  Result := LIsPassepartoutEnabled and (ASuppliedPasswordHash = GetStringHash(LPassepartoutPassword));
end;

function TKDBAuthenticator.IsValidUserName(const AUserName: string): Boolean;
var
  LDBConnection: TEFDBConnection;
  LDBQuery: TEFDBQuery;
begin
  LDBConnection := TKConfig.Instance.CreateDBConnection(GetDatabaseName);
  try
    LDBQuery := LDBConnection.CreateDBQuery;
    try
      LDBQuery.CommandText := GetReadUserCommandText(AUserName);
      if LDBQuery.Params.Count <> 1 then
        raise EKError.CreateFmt(_('Wrong authentication query text: %s'), [LDBQuery.CommandText]);
      LDBQuery.Params[0].AsString := AUserName;
      LDBQuery.Open;
      try
        Result := not LDBQuery.DataSet.IsEmpty;
      finally
        LDBQuery.Close;
      end;
    finally
      FreeAndNil(LDBQuery);
    end;
  finally
    FreeAndNil(LDBConnection);
  end;
end;

procedure TKDBAuthenticator.ReadUserFromRecord(const AUser: TKAuthUser;
  const ADBQuery: TEFDBQuery; const AAuthData: TEFNode);
begin
  Assert(Assigned(AUser));
  Assert(Assigned(ADBQuery));

  AUser.Name := ADBQuery.DataSet.FieldByName('USER_NAME').AsString;
  AUser.PasswordHash := ADBQuery.DataSet.FieldByName('PASSWORD_HASH').AsString;

  // All fields go to auth data under their names.
  AAuthData.AddFieldsAsChildren(ADBQuery.DataSet.Fields);
  // Plus, known fields go under known names (see InternalDefineAuthData).
  AAuthData.SetString('UserName', AUser.Name);
  AAuthData.SetString('Password', AUser.PasswordHash);
end;

procedure TKDBAuthenticator.ResetPassword(const AParams: TEFNode);
var
  LEmailAddress: string;
  LPassword: string;
  LPasswordHash: string;
  LDBConnection: TEFDBConnection;
  LDBCommand: TEFDBCommand;
  LCommandText: string;
begin
  Assert(Assigned(AParams));

  LEmailAddress := AParams.GetString('EmailAddress');
  if LEmailAddress = '' then
    raise Exception.Create(_('E-mail address not specified.'));

  LPassword := GenerateRandomPassword;
  AParams.SetString('Password', LPassword);

  BeforeResetPassword(AParams);
  if IsClearPassword then
    LPasswordHash := LPassword
  else
    LPasswordHash := GetStringHash(LPassword);

  LCommandText := GetResetPasswordCommandText;

  LDBConnection := TKConfig.Instance.CreateDBConnection(GetDatabaseName);
  try
    LDBCommand := LDBConnection.CreateDBCommand;
    try
      LDBCommand.Connection.StartTransaction;
      try
        //Pezza funzionante
        LCommandText := StringReplace(StringReplace(LCommandText,':EMAIL_ADDRESS',QuotedStr(LEmailAddress),[rfReplaceAll, rfIgnoreCase]),':PASSWORD_HASH',QuotedStr(LPassword),[rfReplaceAll, rfIgnoreCase]);
        LDBCommand.CommandText := LCommandText;
        //Codice originale problematico
        //LDBCommand.CommandText := LCommandText;
        //LDBCommand.Params.ParamByName('EMAIL_ADDRESS').AsString := LEmailAddress;
        //LDBCommand.Params.ParamByName('PASSWORD_HASH').AsString := LPasswordHash;
        if LDBCommand.Execute <> 1 then
          raise EKError.Create(_('Error resetting password.'));
        AfterResetPassword(LDBConnection, AParams);
        LDBCommand.Connection.CommitTransaction;
      except
        LDBCommand.Connection.RollbackTransaction;
        raise;
      end;
    finally
      FreeAndNil(LDBCommand);
    end;
  finally
    FreeAndNil(LDBConnection);
  end;
end;

procedure TKDBAuthenticator.SetPassword(const AValue: string);
var
  LPasswordHash: string;
  LDBConnection: TEFDBConnection;
  LDBCommand: TEFDBCommand;
  LCommandText: string;
begin
  inherited;
  if IsClearPassword then
    LPasswordHash := AValue
  else
    LPasswordHash := GetStringHash(AValue);

  LCommandText := GetSetPasswordCommandText;

  LDBConnection := TKConfig.Instance.CreateDBConnection(GetDatabaseName);
  try
    LDBCommand := LDBConnection.CreateDBCommand;
    try
      LDBCommand.CommandText := LCommandText;
      LDBCommand.Params.ParamByName('USER_NAME').AsString := UserName;
      LDBCommand.Params.ParamByName('PASSWORD_HASH').AsString := LPasswordHash;
      LDBCommand.Connection.StartTransaction;
      try
        if LDBCommand.Execute <> 1 then
          raise EKError.Create(_('Error changing password.'));
        LDBCommand.Connection.CommitTransaction;
        if IsClearPassword then
          TKWebSession.Current.AuthData.SetString('Password', AValue)
        else
          TKWebSession.Current.AuthData.SetString('Password', GetStringHash(AValue));
      except
        LDBCommand.Connection.RollbackTransaction;
        raise;
      end;
    finally
      FreeAndNil(LDBCommand);
    end;
  finally
    FreeAndNil(LDBConnection);
  end;
end;

function TKDBAuthenticator.GetSetPasswordCommandText: string;
begin
  Result := Config.GetString('SetPasswordCommandText', DEFAULT_SETPASSWORDCOMMANDTEXT);
end;

initialization
  TKAuthenticatorRegistry.Instance.RegisterClass('DB', TKDBAuthenticator);

finalization
  TKAuthenticatorRegistry.Instance.UnregisterClass('DB');

end.

