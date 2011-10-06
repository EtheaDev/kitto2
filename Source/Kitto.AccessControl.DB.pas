unit Kitto.AccessControl.DB;

{$I Kitto.Defines.inc}

interface

uses
  Classes,
  EF.Classes, EF.Tree,
  Kitto.AccessControl, Kitto.Store;

type
  {
    Utility class used by TKDBAccessController. Encapsulates the permission
    storage for a user and all the roles granted to him.
  }
  TKUserPermissionStorage = class(TEFComponent)
  private
    // Field indexes.
    const K_RESOURCE_URI_PATTERN = 0;
    const K_ACCESS_MODES = 1;
    const K_GRANT_VALUE = 2;
  private
    FUserId: string;
    FPermissions: TKStore;
    FReadRolesCommandText: string;
    FReadPermissionsCommandText: string;
    procedure SetUserId(const AValue: string);
    procedure ReloadCurrentUserPermissions;
    procedure LoadGranteePermissions(const AGranteeId: string);
    procedure GetUserRoles(const AUserId: string; const ARoleList: TStrings);
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
    {
      Name of the user whose this object is holding permissions. Set this
      property to have all permissions for a given user and all his roles
      loaded into memory.
    }
    property UserId: string read FUserId write SetUserId;
    {
      Returns the grant value for the specified resource URI and access mode,
      fatching it from the in-memory data. Call this method only after setting
      UserId. If several permission records are found, the value of the last
      one is returned; the search sequence is: first user permissions, then
      roles permissions in role name alphabetical order.

      Standard modes (which are all Boolean) are treated slightly differently:
      If True is found, then the search continues until False is found
      (in which case the method returns False) or all records are processed
      (in which case the method returns True). This allows to selectively
      deny permissions.

      If no matching permission record is found, then Null is returned.
    }
    function GetAccessGrantValue(const AResourceURI, AMode: string): Variant;
    {
      See the same-named configuration item in TKDBAccessController.
    }
    property ReadPermissionsCommandText: string
      read FReadPermissionsCommandText write FReadPermissionsCommandText;
    {
      See the same-named configuration item in TKDBAccessController.
    }
    property ReadRolesCommandText: string
      read FReadRolesCommandText write FReadRolesCommandText;
  end;

  ///	<summary>
  ///	  <para>
  ///	    The DB access controller uses a set of tables in the database to
  ///	    store user permissions. The tables have fixed structures; to use
  ///	    different table names or structures, create a descendant.
  ///	  </para>
  ///	  <para>
  ///	    In order for this access controller to work, it is required that the
  ///	    database contains the following tables:
  ///	  </para>
  ///	  <para>
  ///	    KITTO_USER_ROLES<br />This table holds the roles that are assigned to
  ///	    each user.
  ///	  </para>
  ///	  <para>
  ///	    Fields:<br />USER_NAME (String): Unique user identifier.<br />ROLE_NAM
  ///	    E (String): Unique role identifier.<br />The USER_NAME + ROLE_NAME
  ///	    combination must be unique.
  ///	  </para>
  ///	  <para>
  ///	    KITTO_PERMISSIONS<br />This table holds the permissions granted to
  ///	    each user or role.
  ///	  </para>
  ///	  <para>
  ///	    Fields:<br />RESOURCE_URI_PATTERN (String): Unique resource URI; may
  ///	    contain the wildcards * and ?; may begin with ~, which has the effect
  ///	    of negating the rest of the pattern; may contain macros, which are
  ///	    expanded before evaluation; may contain a regular expression, if
  ///	    prefixed with 'REGEX:', or a negated regular expression ('~REGEX:').
  ///	    <br />GRANTEE_NAME (String): User or role name.<br />ACCESS_MODES
  ///	    (String): Comma-separated list of access modes for which the
  ///	    permission is granted; no spaces allowed.<br />GRANT_VALUE (String):
  ///	    Granted value; use '0' and '1' for Boolean values.<br />The first
  ///	    three fields are unique. The first time an access control method is
  ///	    called, the access controller fetches all permission data for the
  ///	    specified user and all the roles granted to him, and then checks how
  ///	    access is granted and returns the result.
  ///	  </para>
  ///	  <para>
  ///	    Config parameters:
  ///	  </para>
  ///	  <para>
  ///	    ReadPermissionsCommandText (String): A SQL select statement that
  ///	    selects all permission records for a grantee, with the first three
  ///	    fields representing the first tree fields in KITTO_PERMISSIONS and a
  ///	    single string parameter that will be filled in with the grantee name.
  ///	    Example (this is also the default setting): select
  ///	    RESOURCE_URI_PATTERN, ACCESS_MODES, GRANT_VALUE from
  ///	    KITTO_PERMISSIONS where GRANTEE_NAME = :GRANTEE_NAME order by
  ///	    RESOURCE_URI_PATTERN, ACCESS_MODES.
  ///	  </para>
  ///	  <para>
  ///	    ReadRolesCommandText (String): A SQL select statement that selects
  ///	    all roles granted to a user, with a single field representing the 
  ///	    role name and a single string parameter that will be filled in with
  ///	    the user name. Example (this is also the default setting): select
  ///	    ROLE_NAME from KITTO_USER_ROLES where USER_NAME = :USER_NAME order by
  ///	    ROLE_NAME.
  ///	  </para>
  ///	</summary>
  TKDBAccessController = class(TKAccessController)
  private
    FUserPermissions: TStringList;
    procedure ClearAllUserPermissions;
    function EnsureUserPermissions(const AUserId: string): TKUserPermissionStorage;
  protected
    function InternalGetAccessGrantValue(const AUserId: string;
      const AResourceURI: string; const AMode: string): Variant; override;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils, Variants,
  EF.Intf, EF.DB.Utils, EF.RegEx,
  Kitto.Environment;
  
{ TKDBAccessController }

procedure TKDBAccessController.AfterConstruction;
begin
  inherited;
  FUserPermissions := TStringList.Create;
  FUserPermissions.Sorted := True;
  FUserPermissions.Duplicates := dupError;
end;

destructor TKDBAccessController.Destroy;
begin
  ClearAllUserPermissions;
  FreeAndNil(FUserPermissions);
  inherited;
end;

function TKDBAccessController.EnsureUserPermissions(
  const AUserId: string): TKUserPermissionStorage;
var
  LIndex: Integer;
begin
  Assert(AUserId <> '');

  if FUserPermissions.Find(AUserId, LIndex) then
    Result := TKUserPermissionStorage(FUserPermissions.Objects[LIndex])
  else
  begin
    Result := TKUserPermissionStorage.Create;
    try
      Result.ReadPermissionsCommandText := Config.GetString('ReadPermissionsCommandText',
        'select RESOURCE_URI_PATTERN, ACCESS_MODES, GRANT_VALUE '
        + 'from KITTO_PERMISSIONS '
        + 'where GRANTEE_NAME = :GRANTEE_NAME '
        + 'order by RESOURCE_URI_PATTERN, ACCESS_MODES');
      Result.ReadRolesCommandText := Config.GetString('ReadRolesCommandText',
        'select ROLE_NAME from KITTO_USER_ROLES '
        + 'where USER_NAME = :USER_NAME '
        + 'order by ROLE_NAME');
      Result.UserId := AUserId;
      FUserPermissions.AddObject(AUserId, Result);
    except
      FreeAndNil(Result);
      raise;
    end;
  end;
end;

procedure TKDBAccessController.ClearAllUserPermissions;
begin
  while FUserPermissions.Count > 0 do
  begin
    FUserPermissions.Objects[0].Free;
    FUserPermissions.Delete(0);
  end;
end;

function TKDBAccessController.InternalGetAccessGrantValue(const AUserId,
  AResourceURI, AMode: string): Variant;
begin
  Result := EnsureUserPermissions(AUserId).GetAccessGrantValue(AResourceURI, AMode);
end;

{ TKUserPermissionStorage }

procedure TKUserPermissionStorage.AfterConstruction;
begin
  inherited;
  FPermissions := TKStore.Create;
  FPermissions.Header.AddChild('K_RESOURCE_URI_PATTERN').DataType := edtString;
  FPermissions.Header.AddChild('K_ACCESS_MODES').DataType := edtString;
  FPermissions.Header.AddChild('K_GRANT_VALUE').DataType := edtString;
end;

destructor TKUserPermissionStorage.Destroy;
begin
  FreeAndNil(FPermissions);
  inherited;
end;

procedure TKUserPermissionStorage.ReloadCurrentUserPermissions;
var
  LRoles: TStrings;
  LRoleIndex: Integer;
begin
  FPermissions.Records.Clear;
  if FUserId <> '' then
  begin
    LoadGranteePermissions(FUserId);
    LRoles := TStringList.Create;
    try
      GetUserRoles(FUserId, LRoles);
      for LRoleIndex := 0 to LRoles.Count - 1 do
        LoadGranteePermissions(LRoles[LRoleIndex]);
    finally
      LRoles.Free;
    end;
  end;
end;

procedure TKUserPermissionStorage.LoadGranteePermissions(
  const AGranteeId: string);
var
  LPermissionQuery: IEFDBQuery;
begin
  Assert(AGranteeId <> '');
  Assert(FReadPermissionsCommandText <> '');

  LPermissionQuery := Environment.MainDBConnection.CreateDBQuery;
  try
    LPermissionQuery.CommandText := FReadPermissionsCommandText;
    LPermissionQuery.Params[0].AsString := AGranteeId;
    LPermissionQuery.Open;
    try
      FPermissions.Load(LPermissionQuery.DataSet, True);
    finally
      LPermissionQuery.Close;
    end;
  finally
    FreeAndNilEFIntf(LPermissionQuery);
  end;
end;

function TKUserPermissionStorage.GetAccessGrantValue(const AResourceURI, AMode: string): Variant;
var
  LPattern: string;
  I: Integer;
  LRecord: TKRecord;
begin
  Result := Null;
  for I := 0 to FPermissions.RecordCount - 1 do
  begin
    LRecord := FPermissions.Records[I];
    LPattern := LRecord[K_RESOURCE_URI_PATTERN].AsString;
    // Only trigger macro expansion is a macro is found. Ugly, but more
    // efficient.
    if Pos('%', LPattern) > 0 then
      LPattern := Environment.MacroExpansionEngine.Expand(LPattern);

    // A match is found only if the resource URI pattern matches the given
    // resource URI and if the given access mode is equal to or part of the
    // list of stored access modes. The checks for ',' are meant to rule out
    // false positives in an efficient, albeit not very elegant, way.
    if StrMatchesPatternOrRegex(AResourceURI, LPattern)
      and ((Pos(AMode + ',', LRecord[K_ACCESS_MODES].AsString) > 0)
      or (Pos(',' + AMode, LRecord[K_ACCESS_MODES].AsString) > 0)
      or (AMode = LRecord[K_ACCESS_MODES].AsString)) then
    begin
      Result := LRecord[K_GRANT_VALUE].Value;
      // In standard modes (that only support TRUE or FALSE as grant values)
      // a FALSE value has a greater weight, so as soon as we find one we
      // keep it and break out. In all other cases the last pattern is the one
      // that counts.
      if IsStandardMode(AMode) and (Result = ACV_FALSE) then
        Break;
    end;
  end;
end;

procedure TKUserPermissionStorage.GetUserRoles(
  const AUserId: string; const ARoleList: TStrings);
var
  LRoleQuery: IEFDBQuery;
begin
  Assert(AUserId <> '');
  Assert(Assigned(ARoleList));
  Assert(FReadRolesCommandText <> '');

  LRoleQuery := Environment.MainDBConnection.CreateDBQuery;
  try
    LRoleQuery.CommandText := FReadRolesCommandText;
    LRoleQuery.Params[0].AsString := AUserId;
    LRoleQuery.Open;
    try
      while not LRoleQuery.DataSet.Eof do
      begin
        ARoleList.Add(LRoleQuery.DataSet.Fields[0].AsString);
        LRoleQuery.DataSet.Next;
      end;
    finally
      LRoleQuery.Close;
    end;
  finally
    FreeAndNilEFIntf(LRoleQuery);
  end;
end;

procedure TKUserPermissionStorage.SetUserId(const AValue: string);
begin
  if FUserId <> AValue then
  begin
    FUserId := AValue;
    ReloadCurrentUserPermissions;
  end;
end;

initialization
  EWAccessControllerRegistry.RegisterClass(TKDBAccessController);

finalization
  EWAccessControllerRegistry.UnregisterClass(TKDBAccessController);

end.
