unit Kitto.SQL;

{$I Kitto.Defines.inc}

interface

uses
  Classes, DB, Contnrs,
  EF.Classes,  EF.DB,
  Kitto.Metadata.Models, Kitto.Metadata.Views;

type
  TKSQLJoinBuilder = class;

  {
    A set of named queries, each built by the object itself.
    This object can build queries in various ways (see the Add* methods).
  }
  TKSQLQueryBuilder = class(TEFComponent)
  private
    FDBQueryList: TEFDBQueryList;
    FJoinBuilder: TKSQLJoinBuilder;
    function GetDBQuery(const ADBQueryName: string): IEFDBQuery;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
    {
      Creates a new query with given SQL statement and adds it to the list
      under the specified name.
    }
    function AddFromSelectStatement(const ASQL: string; const AName: string): Integer;
    {
      Creates a new query with a SQL statement that selects all fields from
      the specified GUI visual control's GUI field collection. The query is
      added to the list under the specified name (AName), and the query's
      DataSet is tagged with the value of AGUIVisualControlWithFields.
      Returns the index of the new query in the list.
    }
    function AddFromDataViewTable(const AViewTable: TKViewTable;
      const AName: string): Integer;
    {
      Removes the query with the specified name from the list and frees it.
    }
    procedure Remove(const AName: string);
    {
      Returns the index of the DBQuery with the given name, or -1 if it doesn't
      exist.
    }
    function IndexOf(const AName: string): Integer;
    {
      Use this property to access an already added query by name.
    }
    property DBQueries[const AName: string]: IEFDBQuery read GetDBQuery; default;
  end;

  TKSQLJoinType = (jtInner, jtLeft);

  {
    Helper class. Represents a SQL join clause.
  }
  TKSQLJoinClause = class
  private
    FFieldPairs: TStrings;
    FTargetTableName: string;
    FTargetTableAliasName: string;
    FJoinType: TKSQLJoinType;
    FSourceTableName: string;
    FSourceTableAliasName: string;
    function GetJoinKeyword: string;
    function GetTargetTableAliasedName: string;
    function GetSourceTableAliasedName: string;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
    property SourceTableName: string read FSourceTableName write FSourceTableName;
    {
      Optional alias for the table on the left side of the join.
    }
    property SourceTableAliasName: string
      read FSourceTableAliasName write FSourceTableAliasName;
    property TargetTableName: string read FTargetTableName write FTargetTableName;
    {
      Optional alias for the table on the right side of the join.
    }
    property TargetTableAliasName: string
      read FTargetTableAliasName write FTargetTableAliasName;
    property FieldPairs: TStrings read FFieldPairs;
    procedure AddFieldPair(const ASourceFieldName, ATargetFieldName: string);
    property JoinType: TKSQLJoinType read FJoinType write FJoinType;
    function GetAsString: string;
    function EqualsClause(const AClause: TKSQLJoinClause): Boolean;
  end;

  {
    Helper class. Represents a list of SQL join clauses.
  }
  TKSQLJoinClauses = class
  private
    FClauses: TObjectList;
    function GetClause(const AIndex: Integer): TKSQLJoinClause;
    function GetCount: Integer;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
    procedure Clear;
    function GetAsString: string;
    procedure AddClause(const AClause: TKSQLJoinClause);
    procedure RemoveClause(const AJoinClause: TKSQLJoinClause);
    property Count: Integer read GetCount;
    function ClauseExists(const AClause: TKSQLJoinClause): Boolean;
  end;

  {
    A class that build join clauses for a GUI visual control with fields.
  }
  TKSQLJoinBuilder = class
  private
    FViewTable: TKViewTable;
    FJoinClauses: TKSQLJoinClauses;
    procedure AddJoinClause(const ADataViewField: TKViewField);
    {
      Finds the correct foreign key that links the visual control's main table
      with the specified GUI field's real table. If zero or >1 foreign keys
      are found, and there's no params in the GUI field that help choosing one,
      an exception is raised.
    }
    function GetForeignKey(const ADataViewField: TKViewField;
      out ASourceTable: TKModel): TKModelReference;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
    {
      Resets all internal structures.
    }
    procedure Clear;
    {
      Builds and returns a complete SQL from clause for the specified
      data view table, with joins to all required subordinate tables.
    }
    function BuildSQLFromClause(const AViewTable: TKViewTable): string;
  end;

implementation

uses
  SysUtils, StrUtils, Generics.Collections,
  EF.Intf, EF.Localization, EF.Types, EF.StrUtils,
  Kitto.Types, Kitto.Environment;

{ TKSQLQueryBuilder }

procedure TKSQLQueryBuilder.AfterConstruction;
begin
  inherited;
  FDBQueryList := TEFDBQueryList.Create;
  FJoinBuilder := TKSQLJoinBuilder.Create;
end;

destructor TKSQLQueryBuilder.Destroy;
begin
  inherited;
  FreeAndNil(FJoinBuilder);
  FreeAndNil(FDBQueryList);
end;

function TKSQLQueryBuilder.IndexOf(const AName: string): Integer;
begin
  Result := FDBQueryList.IndexOf(AName);
end;

procedure TKSQLQueryBuilder.Remove(const AName: string);
var
  LDBQueryIntf: IEFDBQuery;
begin
  if IndexOf(AName) >= 0 then
  begin
    LDBQueryIntf := GetDBQuery(AName);
    FDBQueryList.Remove(LDBQueryIntf);
    FreeAndNilEFIntf(LDBQueryIntf);
  end;
end;

function TKSQLQueryBuilder.GetDBQuery(const ADBQueryName: string): IEFDBQuery;
begin
  Result := FDBQueryList.GetDBQueryByName(ADBQueryName);
end;

function TKSQLQueryBuilder.AddFromDataViewTable(const AViewTable: TKViewTable;
  const AName: string): Integer;
var
  LDBQueryIntf: IEFDBQuery;
  LCommandText: string;
  LFieldNames: string;
  I: Integer;
begin
  Assert(Assigned(AViewTable));
  Assert(AName <> '');

  LDBQueryIntf := Environment.MainDBConnection.CreateDBQuery;
  try
    LFieldNames := '';
    for I := 0 to AViewTable.FieldCount - 1 do
    begin
      if LFieldNames = '' then
        LFieldNames := AViewTable.Fields[I].AliasedNameOrExpression
      else
        LFieldNames := LFieldNames + ', ' + AViewTable.Fields[I].AliasedNameOrExpression;
    end;
    LCommandText :=
      'select ' +  LFieldNames +
      ' from ' + FJoinBuilder.BuildSQLFromClause(AViewTable);
    if AViewTable.DefaultFilter <> '' then
      LCommandText := LCommandText + ' where (' + AViewTable.DefaultFilter + ')';
    if AViewTable.DefaultSorting <> '' then
      LCommandText := LCommandText + ' order by ' + AViewTable.DefaultSorting;
    LDBQueryIntf.CommandText := Environment.MacroExpansionEngine.Expand(LCommandText);
    Result := FDBQueryList.Add(AName, LDBQueryIntf);
  except
    FreeAndNilEFIntf(LDBQueryIntf);
    raise;
  end;

  Assert(Result >= 0);
end;

function TKSQLQueryBuilder.AddFromSelectStatement(const ASQL, AName: string): Integer;
var
  LDBQueryIntf: IEFDBQuery;
begin
  LDBQueryIntf := Environment.MainDBConnection.CreateDBQuery;
  try
    LDBQueryIntf.CommandText := ASQL;
    Result := FDBQueryList.Add(AName, LDBQueryIntf);
  except
    FreeAndNilEFIntf(LDBQueryIntf);
    raise;
  end;
end;

{ TKSQLJoinBuilder }

function TKSQLJoinBuilder.GetForeignKey(const ADataViewField: TKViewField;
  out ASourceTable: TKModel): TKModelReference;
var
  LTargetTable: TKModel;
  LForeignKeys: TList<TKModelReference>;
  LForeignKeyName: string;
begin
  Assert(Assigned(FViewTable));
  Assert(Assigned(ADataViewField));
  Result := nil;

  ASourceTable := FViewTable.Model;
  LTargetTable := ADataViewField.Model;

  LForeignKeys := TList<TKModelReference>.Create;
  try
    ASourceTable.GetReferencesToModel(LTargetTable, LForeignKeys);

    // Only one FK - must be the one we're after.
    if LForeignKeys.Count = 1 then
      Result := LForeignKeys[0]
    // No FKs - no join possible. In the future we might add here ability
    // to find an indirect path but not right now.
    else if LForeignKeys.Count = 0 then
      raise EEFError.CreateFmt(_('No foreign keys found from table %s to table %s.'),
        [ASourceTable.ModelName, LTargetTable.ModelName])
    else
    begin
      // More than one foreign keys - select one.
      LForeignKeyName := ADataViewField.GetString('ForeignKeyName');
      if LForeignKeyName <> '' then
      begin
        // Exception if the specified FK is not existing or not pointing to the
        // right table.
        Result := ASourceTable.ReferenceByName(LForeignKeyName);
        if Result.ReferencedModel <> LTargetTable then
          raise EEFError.CreateFmt(_('Foreign key %s does not refer to table %s.'), [LForeignKeyName, LTargetTable.ModelName]);
      end;
    end;
  finally
    FreeAndNil(LForeignKeys);
  end;
  if not Assigned(Result) then
    raise EEFError.CreateFmt(_('No suitable foreign key found for data view field %s in table %s.'),
      [ADataViewField.QualifiedName, ASourceTable.ModelName]);
end;

procedure TKSQLJoinBuilder.AddJoinClause(const ADataViewField: TKViewField);
var
  LForeignKey: TKModelReference;
  LJoinClause: TKSQLJoinClause;
  I: Integer;
  LSourceTable: TKModel;
begin
  Assert(Assigned(FViewTable));
  Assert(Assigned(ADataViewField));

{ TODO : rewrite - should generate table aliases if not specified and group all
  fields from the same referred table under a single join clause. }
  if ADataViewField.ModelName <> FViewTable.ModelName then
  begin
    LForeignKey := GetForeignKey(ADataViewField, LSourceTable);
    LJoinClause := TKSQLJoinClause.Create;
    try
      LJoinClause.SourceTableName := FViewTable.ModelName;
      LJoinClause.TargetTableName := LForeignKey.ReferencedModel.ModelName;
      { TODO : until the rewrite, we support a single foreign field for each
        foreign table, because we are using the table name as alias. }
      LJoinClause.TargetTableAliasName := ADataViewField.ModelName;
      if LForeignKey.IsRequired then
        LJoinClause.JoinType := jtInner
      else
        LJoinClause.JoinType := jtLeft;
      for I := 0 to LForeignKey.FieldCount - 1 do
        LJoinClause.AddFieldPair(LForeignKey.Fields[I].FieldName, LForeignKey.ReferencedFields[I].FieldName);
      if FJoinClauses.ClauseExists(LJoinClause) then
        FreeAndNil(LJoinClause)
      else
        FJoinClauses.AddClause(LJoinClause);
    except
      FreeAndNil(LJoinClause);
      raise;
    end;
  end;
end;

procedure TKSQLJoinBuilder.AfterConstruction;
begin
  inherited;
  FJoinClauses := TKSQLJoinClauses.Create;
end;

function TKSQLJoinBuilder.BuildSQLFromClause(const AViewTable: TKViewTable): string;
var
  LFieldIndex: Integer;
begin
  Assert(Assigned(AViewTable));

  FViewTable := AViewTable;
  Clear;
  Result := FViewTable.ModelName;
  for LFieldIndex := 0 to FViewTable.FieldCount - 1 do
    AddJoinClause(FViewTable.Fields[LFieldIndex]);
  if FJoinClauses.Count > 0 then
    Result := Result + sLineBreak + FJoinClauses.GetAsString;
end;

procedure TKSQLJoinBuilder.Clear;
begin
  FJoinClauses.Clear;
end;

destructor TKSQLJoinBuilder.Destroy;
begin
  FreeAndNil(FJoinClauses);
  inherited;
end;

{ TKSQLJoinClauses }

procedure TKSQLJoinClauses.AddClause(const AClause: TKSQLJoinClause);
begin
  Assert(Assigned(AClause));

  FClauses.Add(AClause);
end;

procedure TKSQLJoinClauses.AfterConstruction;
begin
  inherited;
  FClauses := TObjectList.Create(True);
end;

function TKSQLJoinClauses.ClauseExists(const AClause: TKSQLJoinClause): Boolean;
var
  I: Integer;
begin
  Result := False;

  for I := 0 to FClauses.Count - 1 do
  begin
    if GetClause(I).EqualsClause(AClause) then
    begin
      Result := True;
      Break;
    end;
  end;
end;

procedure TKSQLJoinClauses.Clear;
begin
  FClauses.Clear;
end;

destructor TKSQLJoinClauses.Destroy;
begin
  FreeAndNil(FClauses);
  inherited;
end;

function TKSQLJoinClauses.GetAsString: string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to FClauses.Count - 1 do
    Result := Result + GetClause(I).GetAsString + sLineBreak;
end;

function TKSQLJoinClauses.GetClause(const AIndex: Integer): TKSQLJoinClause;
begin
  Result := FClauses[AIndex] as TKSQLJoinClause;
end;

function TKSQLJoinClauses.GetCount: Integer;
begin
  Result := FClauses.Count;
end;

procedure TKSQLJoinClauses.RemoveClause(const AJoinClause: TKSQLJoinClause);
begin
  FClauses.Remove(AJoinClause);
end;

{ TKSQLJoinClause }

procedure TKSQLJoinClause.AddFieldPair(const ASourceFieldName,
  ATargetFieldName: string);
begin
  FFieldPairs.Add(ASourceFieldName + '=' + ATargetFieldName);
end;

procedure TKSQLJoinClause.AfterConstruction;
begin
  inherited;
  FFieldPairs := TStringList.Create;
  FJoinType := jtInner;
end;

destructor TKSQLJoinClause.Destroy;
begin
  FreeAndNil(FFieldPairs);
  inherited;
end;

function TKSQLJoinClause.EqualsClause(const AClause: TKSQLJoinClause): Boolean;
begin
  Assert(Assigned(AClause));

  Result := (FSourceTableName = AClause.SourceTableName) and
    (FTargetTableName = AClause.TargetTableName) and
    (FTargetTableAliasName = AClause.TargetTableAliasName) and
    (FFieldPairs.Text = AClause.FieldPairs.Text);
end;

function TKSQLJoinClause.GetAsString: string;
var
  I: Integer;

  function GetFieldPairSQL(const AIndex: Integer): string;
  begin
    Result := GetSourceTableAliasedName + '.' + FFieldPairs.Names[AIndex] + ' = ' +
      GetTargetTableAliasedName + '.' + FFieldPairs.ValueFromIndex[AIndex];
  end;

begin
  Assert(FSourceTableName <> '');
  Assert(FTargetTableName <> '');
  Assert(FFieldPairs.Count > 0);

  Result := GetJoinKeyword + ' ' + FTargetTableName + ' ' + FTargetTableAliasName + ' on (';
  for I := 0 to FFieldPairs.Count - 1 do
  begin
    if I = 0 then
      Result := Result + GetFieldPairSQL(I)
    else
      Result := Result + ' and ' + GetFieldPairSQL(I);
  end;
  Result := Result + ')';
end;

function TKSQLJoinClause.GetJoinKeyword: string;
begin
  if FJoinType = jtInner then
    Result := 'join'
  else
    Result := 'left join';
end;

function TKSQLJoinClause.GetTargetTableAliasedName: string;
begin
  if FTargetTableAliasName <> '' then
    Result := FTargetTableAliasName
  else
    Result := FTargetTableName;
end;

function TKSQLJoinClause.GetSourceTableAliasedName: string;
begin
  if FSourceTableAliasName <> '' then
    Result := FSourceTableAliasName
  else
    Result := FSourceTableName;
end;

end.

