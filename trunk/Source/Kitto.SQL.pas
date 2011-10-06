unit Kitto.SQL;

{$I Kitto.Defines.inc}

interface

uses
  Classes, Generics.Collections,
  EF.Classes,  EF.DB,
  Kitto.Metadata.Models, Kitto.Metadata.Views;

type
  TKSQLJoinBuilder = class;

  ///	<summary>
  ///	  Builds SQL statements on request.
  ///	</summary>
  TKSQLQueryBuilder = class(TEFComponent)
  private
    FJoinBuilder: TKSQLJoinBuilder;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
    {
      Builds and returns a SQL statement that selects all fields from
      the specified view table.
    }
    function GetSelectStatement(const AViewTable: TKViewTable): string;
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
    FClauses: TObjectList<TKSQLJoinClause>;
    function GetClausesExist: Boolean;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
    procedure Clear;
    function GetAsString: string;
    procedure AddClause(const AClause: TKSQLJoinClause);
    procedure RemoveClause(const AJoinClause: TKSQLJoinClause);
    function ClauseExists(const AClause: TKSQLJoinClause): Boolean;
    property ClausesExist: Boolean read GetClausesExist;
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
    function GetReference(const ADataViewField: TKViewField;
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
  SysUtils, StrUtils,
  EF.Intf, EF.Localization, EF.Types, EF.StrUtils,
  Kitto.Types, Kitto.Environment;

{ TKSQLQueryBuilder }

procedure TKSQLQueryBuilder.AfterConstruction;
begin
  inherited;
  FJoinBuilder := TKSQLJoinBuilder.Create;
end;

destructor TKSQLQueryBuilder.Destroy;
begin
  inherited;
  FreeAndNil(FJoinBuilder);
end;

function TKSQLQueryBuilder.GetSelectStatement(
  const AViewTable: TKViewTable): string;
var
  LFieldNames: string;
  I: Integer;
begin
  Assert(Assigned(AViewTable));

  LFieldNames := '';
  for I := 0 to AViewTable.FieldCount - 1 do
  begin
    if LFieldNames = '' then
      LFieldNames := AViewTable.Fields[I].AliasedNameOrExpression
    else
      LFieldNames := LFieldNames + ', ' + AViewTable.Fields[I].AliasedNameOrExpression;
  end;
  Result :=
    'select ' +  LFieldNames +
    ' from ' + FJoinBuilder.BuildSQLFromClause(AViewTable);
  if AViewTable.DefaultFilter <> '' then
    Result := Result + ' where (' + AViewTable.DefaultFilter + ')';
  if AViewTable.DefaultSorting <> '' then
    Result := Result + ' order by ' + AViewTable.DefaultSorting;
  Result := Environment.MacroExpansionEngine.Expand(Result);
end;

{ TKSQLJoinBuilder }

function TKSQLJoinBuilder.GetReference(const ADataViewField: TKViewField;
  out ASourceTable: TKModel): TKModelReference;
var
  LTargetTable: TKModel;
  LReferences: TList<TKModelReference>;
  LReference: string;
begin
  Assert(Assigned(FViewTable));
  Assert(Assigned(ADataViewField));
  Result := nil;

  ASourceTable := FViewTable.Model;
  LTargetTable := ADataViewField.Model;

  LReferences := TList<TKModelReference>.Create;
  try
    ASourceTable.GetReferencesToModel(LTargetTable, LReferences);

    // Only one FK - must be the one we're after.
    if LReferences.Count = 1 then
      Result := LReferences[0]
    // No FKs - no join possible. In the future we might add here ability
    // to find an indirect path but not right now.
    else if LReferences.Count = 0 then
      raise EEFError.CreateFmt(_('No foreign keys found from table %s to table %s.'),
        [ASourceTable.ModelName, LTargetTable.ModelName])
    else
    begin
      // More than one foreign keys - select one.
      LReference := ADataViewField.GetString('Reference');
      if LReference <> '' then
      begin
        // Exception if the specified FK is not existing or not pointing to the
        // right table.
        Result := ASourceTable.ReferenceByName(LReference);
        if Result.ReferencedModel <> LTargetTable then
          raise EEFError.CreateFmt(_('Foreign key %s does not refer to table %s.'), [LReference, LTargetTable.ModelName]);
      end;
    end;
  finally
    FreeAndNil(LReferences);
  end;
  if not Assigned(Result) then
    raise EEFError.CreateFmt(_('No suitable foreign key found for data view field %s in table %s.'),
      [ADataViewField.QualifiedName, ASourceTable.ModelName]);
end;

procedure TKSQLJoinBuilder.AddJoinClause(const ADataViewField: TKViewField);
var
  LReference: TKModelReference;
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
    LReference := GetReference(ADataViewField, LSourceTable);
    LJoinClause := TKSQLJoinClause.Create;
    try
      LJoinClause.SourceTableName := FViewTable.ModelName;
      LJoinClause.TargetTableName := LReference.ReferencedModel.ModelName;
      { TODO : until the rewrite, we support a single foreign field for each
        foreign table, because we are using the table name as alias. }
      LJoinClause.TargetTableAliasName := ADataViewField.ModelName;
      if LReference.IsRequired then
        LJoinClause.JoinType := jtInner
      else
        LJoinClause.JoinType := jtLeft;
      for I := 0 to LReference.FieldCount - 1 do
        LJoinClause.AddFieldPair(LReference.Fields[I].FieldName, LReference.ReferencedFields[I].FieldName);
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
  if FJoinClauses.ClausesExist then
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
  FClauses := TObjectList<TKSQLJoinClause>.Create(True);
end;

function TKSQLJoinClauses.ClauseExists(const AClause: TKSQLJoinClause): Boolean;
var
  I: Integer;
begin
  Result := False;

  for I := 0 to FClauses.Count - 1 do
  begin
    if FClauses[I].EqualsClause(AClause) then
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
    Result := Result + FClauses[I].GetAsString + sLineBreak;
end;

function TKSQLJoinClauses.GetClausesExist: Boolean;
begin
  Result := FClauses.Count > 0;
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

