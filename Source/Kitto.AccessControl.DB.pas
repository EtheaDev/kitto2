{
  Defines the DB access controller and related classes and services.
  This access controller uses a set of custom tables in the database to
  check user permissions.
}
unit Kitto.AccessControl.DB;

{$I Kitto.Defines.inc}

interface

uses
  Classes, DB, DBClient,
  EF.Classes,
  Kitto.AccessControl;

type
  {
    Utility class used by TKDBAccessController. Encapsulates the permission
    storage for a user and all the roles granted to him.
  }
  TKUserPermissionStorage = class(TEFComponent)
  private
    FUserId: string;
    FPermissions: TClientDataSet;
    FPatternField: TField;
    FModesField: TField;
    FValueField: TField;
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

  {
    The DB access controller uses a set of tables in the database to store
    user permissions. The tables have fixed structures; to use different
    table names or structures, create a descendant.

    In order for this access controller to work, it is required that the
    database contains the following tables:

    EW_USER_ROLES
    @table(
      @rowHead(@cell(Field Name)@cell(Data Type)@cell(Description))
      @row(@cell(EW_USER_NAME)@cell(string)@cell(Unique user identifier))
      @row(@cell(EW_ROLE_NAME)@cell(string)@cell(Unique role identifier))
    )
    EW_USER_NAME + EW_ROLE_NAME are unique. This table holds the roles that
    are assigned to each user.

    EW_PERMISSIONS
    @table(
      @rowHead(@cell(Field Name)@cell(Data Type)@cell(Description))
      @row(@cell(EW_RESOURCE_URI_PATTERN)@cell(string)@cell(Unique resource
        URI; may contain the wildcards * and ?; may begin with ~, which has the
        effect of negating the rest of the pattern; may contain macros, which
        are expanded before evaluation; may contain a regular expression,
        if prefixed with 'REGEX:', or a negated regular expression ('~REGEX:'.))
      @row(@cell(EW_GRANTEE_NAME)@cell(string)@cell(User or role name.))
      @row(@cell(EW_ACCESS_MODES)@cell(string)@cell(Comma-separated list of
        access modes for which the permission is granted; no spaces allowed.))
      @row(@cell(EW_GRANT_VALUE)@cell(string)@cell(Granted value; use '0' and
        '1' for Boolean values.))
    )
    The first three fields are unique. This table holds the permissions granted
    to each user or role.

    The first time an access control method is called, the access controller
    fetches all permission data for the specified user and all the roles granted
    to him, and then checks how access is granted and returns the result.

    Configuration items (should be specified in SecureConfiguration's slice
    AccessControl.AccessController):
    @table(
      @row(
        @cell(ReadPermissionsCommandText)@cell(String)@cell(Optional)
        @cell(A SQL select statement that selects all permission records for a
          grantee, with the fields EW_RESOURCE_URI_PATTERN, Kitto._ACCESS_MODES
          and EW_GRANT_VALUE and a single string parameter that will be filled
          in with the grantee name.

          Example (this is also the default setting):
            select EW_RESOURCE_URI_PATTERN, Kitto._ACCESS_MODES, Kitto._GRANT_VALUE
            from EW_PERMISSIONS
            where EW_GRANTEE_NAME = :EW_GRANTEE_NAME
            order by EW_RESOURCE_URI_PATTERN, Kitto._ACCESS_MODES
      ))@row(
        @cell(ReadRolesCommandText)@cell(String)@cell(Optional)
        @cell(A SQL select statement that selects all roles granted to a user,
          with a single field called EW_ROLE_NAME and a single string parameter
          that will be filled in with the user name.

          Example (this is also the default setting):
            select EW_ROLE_NAME from EW_USER_ROLES
            where EW_USER_NAME = :EW_USER_NAME
            order by EW_ROLE_NAME
      ))
  }
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
        'select EW_RESOURCE_URI_PATTERN, Kitto._ACCESS_MODES, Kitto._GRANT_VALUE '
        + 'from EW_PERMISSIONS '
        + 'where EW_GRANTEE_NAME = :EW_GRANTEE_NAME '
        + 'order by EW_RESOURCE_URI_PATTERN, Kitto._ACCESS_MODES');
      Result.ReadRolesCommandText := Config.GetString('ReadRolesCommandText',
        'select EW_ROLE_NAME from EW_USER_ROLES '
        + 'where EW_USER_NAME = :EW_USER_NAME '
        + 'order by EW_ROLE_NAME');
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
  FPermissions := TClientDataSet.Create(nil);
  FPermissions.FieldDefs.Add('EW_RESOURCE_URI_PATTERN', ftString, 255, True);
  FPermissions.FieldDefs.Add('EW_ACCESS_MODES', ftString, 255, True);
  FPermissions.FieldDefs.Add('EW_GRANT_VALUE', ftString, 255, True);
  FPermissions.CreateDataSet;
  FPatternField := FPermissions.FieldByName('EW_RESOURCE_URI_PATTERN');
  FModesField := FPermissions.FieldByName('EW_ACCESS_MODES');
  FValueField := FPermissions.FieldByName('EW_GRANT_VALUE');
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
  FPermissions.EmptyDataSet;
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
      AppendToDataSet(LPermissionQuery.DataSet, FPermissions);
    finally
      LPermissionQuery.Close;
    end;
  finally
    FreeAndNilEFIntf(LPermissionQuery);
  end;
end;

function TKUserPermissionStorage.GetAccessGrantValue(const AResourceURI,
  AMode: string): Variant;
var
  LPattern: string;
begin
  Result := Null;
  FPermissions.First;
  while not FPermissions.Eof do
  begin
    LPattern := FPatternField.AsString;
    // Only trigger macro expansion is a macro is found. Ugly, but more
    // efficient.
    if Pos('%', LPattern) > 0 then
      LPattern := Environment.MacroExpansionEngine.Expand(LPattern);

    // A match is found only if the resource URI pattern matches the given
    // resource URI and if the given access mode is equal to or part of the
    // list of stored access modes. The checks for ',' are meant to rule out
    // false positives in an efficient, albeit not very elegant, way.
    if StrMatchesPatternOrRegex(AResourceURI, LPattern)
      and ((Pos(AMode + ',', FModesField.AsString) > 0)
      or (Pos(',' + AMode, FModesField.AsString) > 0)
      or (AMode = FModesField.AsString)) then
    begin
      Result := FValueField.Value;
      // In standard modes (that only support TRUE or FALSE as grant values)
      // a FALSE value has a greater weight, so as soon as we find one we
      // keep it and break out. In all other cases the last pattern is the one
      // that counts.
      if IsStandardMode(AMode) and (Result = ACV_FALSE) then
        Break;
    end;
    FPermissions.Next;
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
