unit Kitto.SQL;

{$I Kitto.Defines.inc}

interface

uses
  Classes, Generics.Collections,
  EF.Classes,  EF.Tree, EF.DB,
  Kitto.Metadata.Models, Kitto.Metadata.Views;

type
  ///	<summary>
  ///	  Builds SQL select statements on request.
  ///	</summary>
  TKSQLBuilder = class(TEFComponent)
  private
    FReferenceAliases: TDictionary<TKModelReference, string>;
    FSelectTerms: string;
    FViewTable: TKViewTable;
    procedure Clear;
    procedure AddSelectTerm(const ATerm: string);
    function GetReferencedFieldTerm(const AViewField: TKViewField): string;
    function GetViewFieldReference(
      const AViewField: TKViewField): TKModelreference;
    procedure AddReferenceAlias(const AReference: TKModelReference);
    function GetFromClause: string;
    function BuildJoin(const AReference: TKModelreference): string;

    procedure InternalBuildSelectQuery(const AViewTable: TKViewTable;
      const AFilter: string; const ADBQuery: TEFDBQuery; const AMasterValues: TEFNode = nil;
      const AFrom: Integer = 0; const AFor: Integer = 0);
    procedure InternalBuildCountQuery(const AViewTable: TKViewTable;
      const AFilter: string; const ADBQuery: TEFDBQuery;
      const AMasterValues: TEFNode);
    function GetSelectWhereClause(const AFilter: string;
      const ADBQuery: TEFDBQuery): string;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;

    ///	<summary>
    ///	  <para>Builds in the specified query a select statement that selects
    ///	  all fields from the specified view table with an optional filter
    ///	  clause. Handles joins and table aliases based on model
    ///	  information.</para>
    ///	  <para>If the view table is a detail table, a where clause with
    ///	  parameters for the master fields is added as well, and param values
    ///	  are set based on AMasterValues.</para>
    ///	</summary>
    class procedure BuildSelectQuery(const AViewTable: TKViewTable; const AFilter: string;
      const ADBQuery: TEFDBQuery; const AMasterValues: TEFNode = nil;
      const AFrom: Integer = 0; const AFor: Integer = 0);

    class procedure BuildCountQuery(const AViewTable: TKViewTable;
      const AFilter: string; const ADBQuery: TEFDBQuery;
      const AMasterValues: TEFNode);

    ///	<summary>Builds and returns a SQL statement that selects the specified
    ///	field plus all key fields from the specified field's table. AViewField
    ///	must have an assigned Reference, otherwise an exception is
    ///	raised.</summary>
    class function GetLookupSelectStatement(const AViewField: TKViewField): string;

    ///	<summary>Builds in the specified command an insert statement against
    ///	the specified model's table with a parameter for each value in AValues.
    ///	Also sets the parameter values, so that the command is ready for
    ///	execution.</summary>
    class procedure BuildInsertCommand(const AModel: TKModel;
      const ADBCommand: TEFDBCommand; const AValues: TEFNode);

    ///	<summary>Builds in the specified command an update statement against
    ///	the specified model's table with a parameter for each value in AValues
    /// plus a where clause with a parameter for each key field.
    ///	Also sets the parameter values, so that the command is ready for
    ///	execution. AValues must contain at least the key fields.</summary>
    class procedure BuildUpdateCommand(const AModel: TKModel;
      const ADBCommand: TEFDBCommand; const AValues: TEFNode);

    ///	<summary>Builds in the specified command a delete statement against
    ///	the specified model's table with a where clause with a parameter for
    /// each key field.
    ///	Also sets the parameter values, so that the command is ready for
    ///	execution. AValues must contain at least the key fields.</summary>
    class procedure BuildDeleteCommand(const AModel: TKModel;
      const ADBCommand: TEFDBCommand; const AValues: TEFNode);
  end;

implementation

uses
  SysUtils, StrUtils, DB, Types,
  EF.Intf, EF.Localization, EF.Types, EF.StrUtils, EF.DB.Utils, EF.SQL,
  Kitto.Types, Kitto.Environment;

{ TKSQLQueryBuilder }

class procedure TKSQLBuilder.BuildSelectQuery(const AViewTable: TKViewTable;
  const AFilter: string; const ADBQuery: TEFDBQuery; const AMasterValues: TEFNode;
  const AFrom: Integer; const AFor: Integer);
begin
  Assert(Assigned(AViewTable));

  with TKSQLBuilder.Create do
  begin
    try
      InternalBuildSelectQuery(AViewTable, AFilter, ADBQuery, AMasterValues, AFrom, AFor);
    finally
      Free;
    end;
  end;
end;

class procedure TKSQLBuilder.BuildCountQuery(const AViewTable: TKViewTable;
  const AFilter: string; const ADBQuery: TEFDBQuery; const AMasterValues: TEFNode);
begin
  Assert(Assigned(AViewTable));

  with TKSQLBuilder.Create do
  begin
    try
      InternalBuildCountQuery(AViewTable, AFilter, ADBQuery, AMasterValues);
    finally
      Free;
    end;
  end;
end;

class procedure TKSQLBuilder.BuildInsertCommand(const AModel: TKModel;
  const ADBCommand: TEFDBCommand; const AValues: TEFNode);
var
  LCommandText: string;
  I: Integer;
begin
  Assert(Assigned(AModel));
  Assert(Assigned(ADBCommand));
  Assert(Assigned(AValues));

  if ADBCommand.Prepared then
    ADBCommand.Prepared := False;
  ADBCommand.Params.BeginUpdate;
  try
    ADBCommand.Params.Clear;
    LCommandText := 'insert into ' + AModel.ModelName + ' (';
    for I := 0 to AValues.ChildCount - 1 do
    begin
      if I > 0 then
        LCommandText := LCommandText + ', ';
      LCommandText := LCommandText + AValues[I].Name;
    end;
    LCommandText := LCommandText + ') values (';
    for I := 0 to AValues.ChildCount - 1 do
    begin
      if I > 0 then
        LCommandText := LCommandText + ', ';
      LCommandText := LCommandText + ':' + AValues[I].Name;
      ADBCommand.Params.CreateParam(ftUnknown, AValues[I].Name, ptInput);
    end;
    LCommandText := LCommandText + ')';
    ADBCommand.CommandText := LCommandText;
  finally
    ADBCommand.Params.EndUpdate;
  end;
  for I := 0 to AValues.ChildCount - 1 do
    AssignEFNodeValueToParam(AValues[I], ADBCommand.Params[I]);
end;

class procedure TKSQLBuilder.BuildUpdateCommand(const AModel: TKModel;
  const ADBCommand: TEFDBCommand; const AValues: TEFNode);
var
  LCommandText: string;
  I: Integer;
  LKeyFields: TStringDynArray;
begin
  Assert(Assigned(AModel));
  Assert(Assigned(ADBCommand));
  Assert(Assigned(AValues));

  if ADBCommand.Prepared then
    ADBCommand.Prepared := False;
  ADBCommand.Params.BeginUpdate;
  try
    ADBCommand.Params.Clear;
    LCommandText := 'update ' + AModel.ModelName + ' set ';
    for I := 0 to AValues.ChildCount - 1 do
    begin
      if I > 0 then
        LCommandText := LCommandText + ', ';
      LCommandText := LCommandText + AValues[I].Name + ' = :' + AValues[I].Name;
      ADBCommand.Params.CreateParam(ftUnknown, AValues[I].Name, ptInput);
    end;
    LCommandText := LCommandText + ' where ';
    LKeyFields := AModel.GetKeyFieldNames;
    for I := 0 to Length(LKeyFields) - 1 do
    begin
      if I > 0 then
        LCommandText := LCommandText + ' and ';
      LCommandText := LCommandText + LKeyFields[I] + ' = :P_KEY' + IntToStr(I);
      ADBCommand.Params.CreateParam(ftUnknown, 'P_KEY' + IntToStr(I), ptInput);
    end;
    ADBCommand.CommandText := LCommandText;
  finally
    ADBCommand.Params.EndUpdate;
  end;
  for I := 0 to AValues.ChildCount - 1 do
    AssignEFNodeValueToParam(AValues[I], ADBCommand.Params[I]);
  for I := 0 to Length(LKeyFields) - 1 do
    AssignEFNodeValueToParam(AValues.GetNode(LKeyFields[I]),
      ADBCommand.Params.ParamByName('P_KEY' + IntToStr(I)));
end;

class procedure TKSQLBuilder.BuildDeleteCommand(const AModel: TKModel;
  const ADBCommand: TEFDBCommand; const AValues: TEFNode);
var
  LCommandText: string;
  I: Integer;
  LKeyFields: TStringDynArray;
begin
  Assert(Assigned(AModel));
  Assert(Assigned(ADBCommand));
  Assert(Assigned(AValues));

  if ADBCommand.Prepared then
    ADBCommand.Prepared := False;
  ADBCommand.Params.BeginUpdate;
  try
    ADBCommand.Params.Clear;
    LCommandText := 'delete from ' + AModel.ModelName + ' where ';
    LKeyFields := AModel.GetKeyFieldNames;
    for I := 0 to Length(LKeyFields) - 1 do
    begin
      if I > 0 then
        LCommandText := LCommandText + ' and ';
      LCommandText := LCommandText + LKeyFields[I] + ' = :P_KEY' + IntToStr(I);
      ADBCommand.Params.CreateParam(ftUnknown, 'P_KEY' + IntToStr(I), ptInput);
    end;
    ADBCommand.CommandText := LCommandText;
  finally
    ADBCommand.Params.EndUpdate;
  end;
  for I := 0 to Length(LKeyFields) - 1 do
    AssignEFNodeValueToParam(AValues.GetNode(LKeyFields[I]),
      ADBCommand.Params.ParamByName('P_KEY' + IntToStr(I)));
end;

procedure TKSQLBuilder.AfterConstruction;
begin
  inherited;
  FReferenceAliases := TDictionary<TKModelReference, string>.Create;
end;

destructor TKSQLBuilder.Destroy;
begin
  inherited;
  FreeAndNil(FReferenceAliases);
end;

procedure TKSQLBuilder.Clear;
begin
  FViewTable := nil;
  FSelectTerms := '';
  FReferenceAliases.Clear;
end;

procedure TKSQLBuilder.AddSelectTerm(const ATerm: string);
begin
  if FSelectTerms = '' then
    FSelectTerms := ATerm
  else
    FSelectTerms := FSelectTerms + ', ' + ATerm;
end;

function TKSQLBuilder.GetFromClause: string;
var
  I: Integer;
begin
  Assert(Assigned(FViewTable));

  Result := FViewTable.ModelName;
  for I := 0 to FReferenceAliases.Count - 1 do
    Result := Result + sLineBreak + BuildJoin(FReferenceAliases.Keys.ToArray[I]);
end;

class function TKSQLBuilder.GetLookupSelectStatement(
  const AViewField: TKViewField): string;
begin
  Assert(Assigned(AViewField));
  Assert(AViewField.Reference <> nil);

  Result := 'select '
    + Join(AViewField.Reference.GetReferencedFieldNames, ', ')
    + ', ' + AViewField.ModelField.FieldName
    + ' from ' + AViewField.ModelName
    + ' order by ' + AViewField.ModelField.FieldName;
  if AViewField.Model.DefaultFilter <> '' then
    Result := AddToSQLWhereClause(Result, AViewField.Model.DefaultFilter);
end;

function TKSQLBuilder.BuildJoin(const AReference: TKModelreference): string;

  function GetJoinKeyword: string;
  begin
    if AReference.IsRequired then
      Result := 'join'
    else
      Result := 'left join';
  end;

var
  LRefAlias: string;
  I: Integer;
begin
  Assert(Assigned(AReference));

  Result := GetJoinKeyword + ' ';
  LRefAlias := FReferenceAliases[AReference];
  if LRefAlias = AReference.ReferencedModel.ModelName then
    Result := Result + LRefAlias
  else
    Result := Result + AReference.ReferencedModel.ModelName + ' ' + LRefAlias;
  Result := Result + ' on (';
  for I := 0 to AReference.FieldCount - 1 do
  begin
    Result := Result + AReference.Fields[I].QualifiedFieldName + ' = ' + LRefAlias + '.' + AReference.ReferencedFields[I].FieldName;
    if I < AReference.FieldCount - 1 then
      Result := Result + ' and ';
  end;
  Result := Result + ')';
end;

function TKSQLBuilder.GetReferencedFieldTerm(const AViewField: TKViewField): string;
var
  LReference: TKModelReference;
begin
  Assert(Assigned(FViewTable));
  Assert(Assigned(AViewField));
  Assert(FViewTable.Model <> AViewField.Model);

  if AViewField.Alias = '' then
    raise EKError.CreateFmt(_('View field %s must have an alias.'), [AViewField.AliasedName]);

  LReference := GetViewFieldReference(AViewField);
  AddReferenceAlias(LReference);

  Result := FReferenceAliases[LReference] + '.' +
    AViewField.FieldName + ' ' + AViewField.Alias;
end;

procedure TKSQLBuilder.AddReferenceAlias(const AReference: TKModelReference);
begin
  Assert(Assigned(AReference));

  if not FReferenceAliases.ContainsKey(AReference) then
    FReferenceAliases.Add(AReference, AReference.ReferenceName);
end;

function TKSQLBuilder.GetViewFieldReference(const AViewField: TKViewField): TKModelreference;
begin
  Assert(Assigned(AViewField));

  Result := AViewField.Reference;
  if not Assigned(Result) then
    raise EKError.CreateFmt(_('No reference found for field %s.'), [AViewField.AliasedName]);
end;

function TKSQLBuilder.GetSelectWhereClause(const AFilter: string;
  const ADBQuery: TEFDBQuery): string;
var
  I: Integer;
  LMasterFieldNames: TStringDynArray;
  LDetailFieldNames: TStringDynArray;
  LClause: string;
begin
  Result := '';
  if FViewTable.DefaultFilter <> '' then
    Result := Result + ' where (' + FViewTable.DefaultFilter + ')';
  if AFilter <> '' then
    Result := AddToSQLWhereClause(Result, AFilter);

  if FViewTable.IsDetail then
  begin
    // Get master and detail field names...
    LMasterFieldNames := FViewTable.ReferenceToMaster.GetReferencedFieldNames;
    Assert(Length(LMasterFieldNames) > 0);
    LDetailFieldNames := FViewTable.ReferenceToMaster.GetFieldNames;
    Assert(Length(LDetailFieldNames) = Length(LMasterFieldNames));
    LClause := '';
    for I := 0 to High(LDetailFieldNames) do
    begin
      // ...and alias them.
      LMasterFieldNames[I] := FViewTable.MasterTable.FieldByName(LMasterFieldNames[I]).AliasedName;
      LDetailFieldNames[I] := FViewTable.FieldByName(LDetailFieldNames[I]).AliasedName;
      LClause := LClause + LDetailFieldNames[I] + ' = :' + LMasterFieldNames[I];
      ADBQuery.Params.CreateParam(ftUnknown, LMasterFieldNames[I], ptInput);
      if I < High(LDetailFieldNames) then
        LClause := LClause + ' and ';
    end;
    Result := AddToSQLWhereClause(Result, '(' + LClause + ')');
  end;
end;

procedure TKSQLBuilder.InternalBuildSelectQuery(const AViewTable: TKViewTable;
  const AFilter: string; const ADBQuery: TEFDBQuery; const AMasterValues: TEFNode;
  const AFrom: Integer; const AFor: Integer);
var
  I: Integer;
  LCommandText: string;
begin
  Clear;
  FViewTable := AViewTable;
  for I := 0 to AViewTable.FieldCount - 1 do
  begin
    if AViewTable.Fields[I].Reference = nil then
      AddSelectTerm(AViewTable.Fields[I].QualifiedAliasedNameOrExpression)
    else
      AddSelectTerm(GetReferencedFieldTerm(AViewTable.Fields[I]));
  end;

  if ADBQuery.Prepared then
    ADBQuery.Prepared := False;
  ADBQuery.Params.BeginUpdate;
  try
    ADBQuery.Params.Clear;
    LCommandText :=
      'select ' +  FSelectTerms +
      ' from ' + GetFromClause + GetSelectWhereClause(AFilter, ADBQuery);
    if FViewTable.DefaultSorting <> '' then
      LCommandText := LCommandText + ' order by ' + FViewTable.DefaultSorting;
    LCommandText := ADBQuery.Connection.AddLimitClause(LCommandText, AFrom, AFor);
    ADBQuery.CommandText := Environment.MacroExpansionEngine.Expand(LCommandText);
  finally
    ADBQuery.Params.EndUpdate;
  end;
  for I := 0 to ADBQuery.Params.Count - 1 do
    AssignEFNodeValueToParam(AMasterValues.GetNode(ADBQuery.Params[I].Name), ADBQuery.Params[I]);
end;

procedure TKSQLBuilder.InternalBuildCountQuery(const AViewTable: TKViewTable;
  const AFilter: string; const ADBQuery: TEFDBQuery;
  const AMasterValues: TEFNode);
var
  I: Integer;
  LCommandText: string;
begin
  Clear;
  FViewTable := AViewTable;
{ TODO :
Process all fields to build the from clause. A future refactoring might
build only those that affect the count (outer joins). }
  for I := 0 to AViewTable.FieldCount - 1 do
  begin
    if AViewTable.Fields[I].Reference = nil then
      AddSelectTerm(AViewTable.Fields[I].QualifiedAliasedNameOrExpression)
    else
      AddSelectTerm(GetReferencedFieldTerm(AViewTable.Fields[I]));
  end;

  if ADBQuery.Prepared then
    ADBQuery.Prepared := False;
  ADBQuery.Params.BeginUpdate;
  try
    ADBQuery.Params.Clear;
    LCommandText := 'select count(*) from ' + GetFromClause + GetSelectWhereClause(AFilter, ADBQuery);
    ADBQuery.CommandText := Environment.MacroExpansionEngine.Expand(LCommandText);
  finally
    ADBQuery.Params.EndUpdate;
  end;
  for I := 0 to ADBQuery.Params.Count - 1 do
    AssignEFNodeValueToParam(AMasterValues.GetNode(ADBQuery.Params[I].Name), ADBQuery.Params[I]);
end;

end.

