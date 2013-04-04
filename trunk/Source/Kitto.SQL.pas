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

unit Kitto.SQL;

{$I Kitto.Defines.inc}

interface

uses
  Classes, Generics.Collections,
  EF.Classes,  EF.Tree, EF.DB,
  Kitto.Store, Kitto.Metadata.Models, Kitto.Metadata.DataView;

type
  ///	<summary>
  ///	  Builds SQL select statements on request.
  ///	</summary>
  TKSQLBuilder = class
  private
    FUsedReferenceFields: TList<TKModelField>;
    FSelectTerms: string;
    FViewTable: TKViewTable;
    procedure Clear;
    procedure AddSelectTerm(const ATerm: string);
    procedure AddReferenceFieldTerms(const AViewField: TKViewField);
    function GetFromClause: string;
    function BuildJoin(const AReferenceField: TKModelField): string;

    procedure InternalBuildSelectQuery(const AViewTable: TKViewTable;
      const AFilter: string; const AOrderBy: string; const ADBQuery: TEFDBQuery;
      const AMasterValues: TEFNode = nil; const AFrom: Integer = 0; const AFor: Integer = 0);
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
      const AOrderBy: string; const ADBQuery: TEFDBQuery; const AMasterValues: TEFNode = nil;
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
    ///	the specified record's table with a parameter for each value in AValues.
    ///	Also sets the parameter values, so that the command is ready for
    ///	execution.</summary>
    class procedure BuildInsertCommand(const ADBCommand: TEFDBCommand;
      const ARecord: TKViewTableRecord);

    ///	<summary>Builds in the specified command an update statement against
    ///	the specified record's table with a parameter for each value in ARecord
    /// plus a where clause with a parameter for each key field.
    ///	Also sets the parameter values, so that the command is ready for
    ///	execution. ARecord must contain at least the key fields.</summary>
    class procedure BuildUpdateCommand(const ADBCommand: TEFDBCommand;
      const ARecord: TKViewTableRecord);

    ///	<summary>Builds in the specified command a delete statement against
    ///	the specified record's table with a where clause with a parameter for
    /// each key field.
    ///	Also sets the parameter values, so that the command is ready for
    ///	execution. AValues must contain at least the key fields.</summary>
    class procedure BuildDeleteCommand(const ADBCommand: TEFDBCommand;
      const ARecord: TKViewTableRecord);

    ///	<summary>Builds in the specified query a select statement that selects
    ///	all derived fields from the referenced model, locating the record by
    ///	means of the specified key.</summary>
    ///	<param name="AViewField">Originating reference field.</param>
    ///	<param name="ADBQuery">Query object into which to build the statement
    ///	and the params.</param>
    ///	<param name="AKeyValues">Key values for the referenced model
    ///	row.</param>
    ///	<exception cref="Assert">The field must be a reference field and at
    ///	least one derived field must exist in the view table.</exception>
    class procedure BuildDerivedSelectQuery(const AViewField: TKViewField;
      const ADBQuery: TEFDBQuery; const AKeyValues: string);
  end;

implementation

uses
  SysUtils, StrUtils, DB, Types,
  EF.Intf, EF.Localization, EF.Types, EF.StrUtils, EF.SQL, EF.Macros,
  Kitto.Types;

{ TKSQLQueryBuilder }

class procedure TKSQLBuilder.BuildSelectQuery(const AViewTable: TKViewTable;
  const AFilter: string; const AOrderBy: string; const ADBQuery: TEFDBQuery;
  const AMasterValues: TEFNode; const AFrom: Integer; const AFor: Integer);
begin
  Assert(Assigned(AViewTable));

  with TKSQLBuilder.Create do
  begin
    try
      InternalBuildSelectQuery(AViewTable, AFilter, AOrderBy, ADBQuery, AMasterValues, AFrom, AFor);
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

class procedure TKSQLBuilder.BuildInsertCommand(const ADBCommand: TEFDBCommand;
  const ARecord: TKViewTableRecord);
var
  LCommandText: string;
  I: Integer;
  LViewField: TKViewField;
  LDBColumnNames: string;
  LValueNames: string;
  J: Integer;
  LProcessedRefFields: TList<TKViewField>;

  procedure AddDBColumnName(const ADBColumnName, AParamName: string);
  begin
    if LDBColumnNames = '' then
    begin
      LDBColumnNames := ADBColumnName;
      LValueNames := ':' + AParamName;
    end
    else
    begin
      LDBColumnNames := LDBColumnNames + ', ' + ADBColumnName;
      LValueNames := LValueNames + ', :' + AParamName;
    end;
    ADBCommand.Params.CreateParam(ftUnknown, AParamName, ptInput);
  end;

  function IsRefFieldProcessed(const AViewField: TKViewField): Boolean;
  begin
    Assert(Assigned(AViewField));
    Assert(AViewField.IsReference);

    Result := LProcessedRefFields.Contains(AViewField);
  end;

  procedure MarkRefFieldAsProcessed(const AViewField: TKViewField);
  begin
    Assert(Assigned(AViewField));
    Assert(AViewField.IsReference);

    LProcessedRefFields.Add(AViewField);
  end;

begin
  Assert(Assigned(ADBCommand));
  Assert(Assigned(ARecord));

  if ADBCommand.Prepared then
    ADBCommand.Prepared := False;
  ADBCommand.Params.BeginUpdate;
  try
    ADBCommand.Params.Clear;
    LCommandText := 'insert into ' + ARecord.ViewTable.Model.DBTableName + ' (';
    LDBColumnNames := '';
    LValueNames := '';

    LProcessedRefFields := TList<TKViewField>.Create;
    try
      for I := 0 to ARecord.ChildCount - 1 do
      begin
        LViewField := ARecord[I].ViewField;
        if Assigned(LViewField) and ARecord[I].IsModified and LViewField.CanInsert then
        begin
          if LViewField.IsReference then
          begin
            if not IsRefFieldProcessed(LViewField) then
            begin
              for J := 0 to LViewField.ModelField.FieldCount - 1 do
                AddDBColumnName(LViewField.ModelField.Fields[J].DBColumnName,
                  LViewField.ModelField.Fields[J].FieldName);
              MarkRefFieldAsProcessed(LViewField);
            end
          end
          else
            AddDBColumnName(LViewField.ModelField.DBColumnName, ARecord[I].FieldName);
        end;
      end;
    finally
      FreeAndNil(LProcessedRefFields);
    end;
    LCommandText := LCommandText + LDBColumnNames + ') values (' + LValueNames + ')';
    ADBCommand.CommandText := LCommandText;
  finally
    ADBCommand.Params.EndUpdate;
  end;
  for I := 0 to ADBCommand.Params.Count - 1 do
    ARecord.FieldByName(ADBCommand.Params[I].Name).AssignValueToParam(ADBCommand.Params[I]);
end;

class procedure TKSQLBuilder.BuildUpdateCommand(const ADBCommand: TEFDBCommand;
  const ARecord: TKViewTableRecord);
var
  LCommandText: string;
  I: Integer;
  LKeyFields: TStringDynArray;
  LViewField: TKViewField;
  LDBColumnNames: string;
  LParamName: string;
  J: Integer;
  LProcessedRefFields: TList<TKViewField>;

  procedure AddDBColumnName(const ADBColumnName, AParamName: string);
  begin
    if LDBColumnNames = '' then
      LDBColumnNames := ADBColumnName + ' = :' + AParamName
    else
      LDBColumnNames := LDBColumnNames + ', ' + ADBColumnName + ' = :' + AParamName;
    ADBCommand.Params.CreateParam(ftUnknown, AParamName, ptInput);
  end;

  function IsRefFieldProcessed(const AViewField: TKViewField): Boolean;
  begin
    Assert(Assigned(AViewField));
    Assert(AViewField.IsReference);

    Result := LProcessedRefFields.Contains(AViewField);
  end;

  procedure MarkRefFieldAsProcessed(const AViewField: TKViewField);
  begin
    Assert(Assigned(AViewField));
    Assert(AViewField.IsReference);

    LProcessedRefFields.Add(AViewField);
  end;

begin
  Assert(Assigned(ADBCommand));
  Assert(Assigned(ARecord));

  if ADBCommand.Prepared then
    ADBCommand.Prepared := False;
  ADBCommand.Params.BeginUpdate;
  try
    ADBCommand.Params.Clear;
    LCommandText := 'update ' + ARecord.ViewTable.Model.DBTableName + ' set ';
    LDBColumnNames := '';

    LProcessedRefFields := TList<TKViewField>.Create;
    try
      for I := 0 to ARecord.FieldCount - 1 do
      begin
        LViewField := ARecord[I].ViewField;
        if Assigned(LViewField) and ARecord[I].IsModified and LViewField.CanUpdate and not LViewField.IsKey then
        begin
          if LViewField.IsReference then
          begin
            if not IsRefFieldProcessed(LViewField) then
            begin
              for J := 0 to LViewField.ModelField.FieldCount - 1 do
                AddDBColumnName(LViewField.ModelField.Fields[J].DBColumnName,
                  LViewField.ModelField.Fields[J].FieldName);
              MarkRefFieldAsProcessed(LViewField);
            end
          end
          else
            AddDBColumnName(LViewField.ModelField.DBColumnName, ARecord[I].FieldName);
        end;
      end;
    finally
      FreeAndNil(LProcessedRefFields);
    end;
    if LDBColumnNames = '' then
      LCommandText := ''
    else
    begin
      LCommandText := LCommandText + LDBColumnNames + ' where ';
      LKeyFields := ARecord.ViewTable.Model.GetKeyDBColumnNames;
      for I := 0 to Length(LKeyFields) - 1 do
      begin
        LParamName := ARecord.ViewTable.FieldByDBColumnName(LKeyFields[I]).AliasedName;
        if I > 0 then
          LCommandText := LCommandText + ' and ';
        LCommandText := LCommandText + LKeyFields[I] + ' = :' + LParamName;
        ADBCommand.Params.CreateParam(ftUnknown, LParamName, ptInput);
      end;
      ADBCommand.CommandText := LCommandText;
    end;
  finally
    ADBCommand.Params.EndUpdate;
  end;
  for I := 0 to ADBCommand.Params.Count - 1 do
    ARecord.FieldByName(ADBCommand.Params[I].Name).AssignValueToParam(ADBCommand.Params[I]);
end;

class procedure TKSQLBuilder.BuildDeleteCommand(const ADBCommand: TEFDBCommand;
  const ARecord: TKViewTableRecord);
var
  LCommandText: string;
  I: Integer;
  LKeyFields: TStringDynArray;
  LParamName: string;
begin
  Assert(Assigned(ADBCommand));
  Assert(Assigned(ARecord));

  if ADBCommand.Prepared then
    ADBCommand.Prepared := False;
  ADBCommand.Params.BeginUpdate;
  try
    ADBCommand.Params.Clear;
    LCommandText := 'delete from ' + ARecord.ViewTable.Model.DBTableName + ' where ';
    LKeyFields := ARecord.ViewTable.Model.GetKeyDBColumnNames;
    for I := 0 to Length(LKeyFields) - 1 do
    begin
      LParamName := ARecord.ViewTable.FieldByDBColumnName(LKeyFields[I]).AliasedName;
      if I > 0 then
        LCommandText := LCommandText + ' and ';
      LCommandText := LCommandText + LKeyFields[I] + ' = :' + LParamName;
      ADBCommand.Params.CreateParam(ftUnknown, LParamName, ptInput);
    end;
    ADBCommand.CommandText := LCommandText;
  finally
    ADBCommand.Params.EndUpdate;
  end;
  for I := 0 to ADBCommand.Params.Count - 1 do
    ARecord.FieldByName(ADBCommand.Params[I].Name).AssignValueToParam(ADBCommand.Params[I]);
end;

class procedure TKSQLBuilder.BuildDerivedSelectQuery(const AViewField: TKViewField;
  const ADBQuery: TEFDBQuery; const AKeyValues: string);
var
  LDerivedFields: TArray<TKViewField>;
  LCommandText: string;
  I: Integer;
  LDerivedField: TKViewField;
  LKeyDBColumnNames: TStringDynArray;
  LModel: TKModel;
  LClause: string;
  LDBColumnName: string;
begin
  Assert(Assigned(AViewField));
  Assert(AViewField.IsReference);
  Assert(Assigned(ADBQuery));

  LDerivedFields := AViewField.GetDerivedFields;
  Assert(Length(LDerivedFields) > 0);

  LModel := AViewField.ModelField.ReferencedModel;

  LCommandText := '';
  for LDerivedField in LDerivedFields do
  begin
    if SameText(LDerivedField.FieldName, AViewField.FieldName) then
      LDBColumnName := AViewField.ModelField.ReferencedModel.CaptionField.DBColumnName
    else
      LDBColumnName := LDerivedField.ModelField.DBColumnName;

    if LCommandText = '' then
      LCommandText := LDBColumnName
    else
      LCommandText := LCommandText + ', ' + LDBColumnName;
  end;
  LCommandText := 'select ' + LCommandText + ' from ' + LModel.DBTableName;

  if ADBQuery.Prepared then
    ADBQuery.Prepared := False;
  ADBQuery.Params.BeginUpdate;
  try
    ADBQuery.Params.Clear;

    LKeyDBColumnNames := LModel.GetKeyDBColumnNames;
    Assert(Length(LKeyDBColumnNames) > 0);
    LClause := '';
    for I := 0 to High(LKeyDBColumnNames) do
    begin
      if LClause = '' then
        LClause := LKeyDBColumnNames[I] + ' = :' + LKeyDBColumnNames[I]
      else
        LClause := LClause + ' and ' + LKeyDBColumnNames[I] + ' = :' + LKeyDBColumnNames[I];
      ADBQuery.Params.CreateParam(ftUnknown, LKeyDBColumnNames[I], ptInput);
    end;
    LCommandText := SetSQLWhereClause(LCommandText, LClause);
    ADBQuery.CommandText := TEFMacroExpansionEngine.Instance.Expand(LCommandText);
  finally
    ADBQuery.Params.EndUpdate;
  end;
  { TODO : only single-field keys supported for now }
  Assert(ADBQuery.Params.Count = 1);
  ADBQuery.Params[0].Value := AKeyValues;
end;

procedure TKSQLBuilder.AfterConstruction;
begin
  inherited;
  FUsedReferenceFields := TList<TKModelField>.Create;
end;

destructor TKSQLBuilder.Destroy;
begin
  inherited;
  FreeAndNil(FUsedReferenceFields);
end;

procedure TKSQLBuilder.Clear;
begin
  FViewTable := nil;
  FSelectTerms := '';
  FUsedReferenceFields.Clear;
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

  Result := FViewTable.Model.DBTableName;
  for I := 0 to FUsedReferenceFields.Count - 1 do
    Result := Result + sLineBreak + BuildJoin(FUsedReferenceFields[I]);
end;

class function TKSQLBuilder.GetLookupSelectStatement(
  const AViewField: TKViewField): string;
var
  LLookupModel: TKModel;
  LDefaultFilter: string;
  LLookupModelDefaultFilter: string;
begin
  Assert(Assigned(AViewField));
  Assert(AViewField.IsReference);

  LLookupModel := AViewField.ModelField.ReferencedModel;
  Result := 'select '
    + Join(LLookupModel.GetKeyDBColumnNames(False, True), ', ');
    if not LLookupModel.CaptionField.IsKey then
      Result := Result + ', ' + LLookupModel.CaptionField.AliasedDBColumnName;
    Result := Result + ' from ' + LLookupModel.DBTableName
      + ' order by ' + LLookupModel.CaptionField.DBColumnName;
  LLookupModelDefaultFilter := LLookupModel.DefaultFilter;
  if LLookupModelDefaultFilter <> '' then
    Result := AddToSQLWhereClause(Result, '(' + LLookupModelDefaultFilter + ')');
  LDefaultFilter := AViewField.DefaultFilter;
  if LDefaultFilter <> '' then
    Result := AddToSQLWhereClause(Result, '(' + LDefaultFilter  + ')',
      AViewField.DefaultFilterConnector);
end;

function TKSQLBuilder.BuildJoin(const AReferenceField: TKModelField): string;

  function GetJoinKeyword: string;
  begin
    if AReferenceField.IsRequired then
      Result := 'join'
    else
      Result := 'left join';
  end;

var
  I: Integer;
  LLocalFields: TKModelFieldArray;
  LForeignDBColumnNames: TStringDynArray;
  LCorrelationName: string;
begin
  Assert(Assigned(AReferenceField));

  LCorrelationName := AReferenceField.FieldName;

  Result := GetJoinKeyword + ' ' + AReferenceField.ReferencedModel.DBTableName + ' ' + LCorrelationName + ' on (';
  LLocalFields := AReferenceField.GetReferenceFields;
  Assert(Length(LLocalFields) > 0);
  LForeignDBColumnNames := AReferenceField.ReferencedModel.GetKeyDBColumnNames;
  Assert(Length(LForeignDBColumnNames) = Length(LLocalFields));

  for I := Low(LLocalFields) to High(LLocalFields) do
  begin
    Result := Result + FViewTable.Model.DBTableName + '.' + LLocalFields[I].DBColumnName + ' = '
      + LCorrelationName + '.' + LForeignDBColumnNames[I];
    if I < High(LLocalFields) then
      Result := Result + ' and ';
  end;
  Result := Result + ')';
end;

procedure TKSQLBuilder.AddReferenceFieldTerms(const AViewField: TKViewField);
var
  LFields: TKModelFieldArray;
  I: Integer;
begin
  Assert(Assigned(FViewTable));
  Assert(Assigned(AViewField));
  Assert(AViewField.IsReference);

  if not FUsedReferenceFields.Contains(AViewField.ModelField) then
    FUsedReferenceFields.Add(AViewField.ModelField);

  // Add the caption field of the referenced model as well.
  // The reference field name is used as table alias.
  AddSelectTerm(
    AViewField.FieldName + '.' +
    AViewField.ModelField.ReferencedModel.CaptionField.DBColumnNameOrExpression + ' ' +
    AViewField.ModelField.FieldName);
  LFields := AViewField.ModelField.GetReferenceFields;
  for I := Low(LFields) to High(LFields) do
    AddSelectTerm(FViewTable.Model.DBTableName + '.' + LFields[I].DBColumnName);
end;

function TKSQLBuilder.GetSelectWhereClause(const AFilter: string;
  const ADBQuery: TEFDBQuery): string;
var
  I: Integer;
  LMasterFieldDBColumnNames: TStringDynArray;
  LDetailFieldDBColumnNames: TStringDynArray;
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
    LMasterFieldDBColumnNames := FViewTable.MasterTable.Model.GetKeyDBColumnNames;
    Assert(Length(LMasterFieldDBColumnNames) > 0);
    LDetailFieldDBColumnNames := FViewTable.ModelDetailReference.ReferenceField.GetDBColumnNames;
    Assert(Length(LDetailFieldDBColumnNames) = Length(LMasterFieldDBColumnNames));
    LClause := '';
    for I := 0 to High(LDetailFieldDBColumnNames) do
    begin
      // ...and alias master field names. Don'alias detail field names used in the where clause.
      LMasterFieldDBColumnNames[I] := FViewTable.MasterTable.ApplyFieldAliasedName(LMasterFieldDBColumnNames[I]);
      LClause := LClause + FViewTable.Model.DBTableName + '.' + LDetailFieldDBColumnNames[I] + ' = :' + LMasterFieldDBColumnNames[I];
      ADBQuery.Params.CreateParam(ftUnknown, LMasterFieldDBColumnNames[I], ptInput);
      if I < High(LDetailFieldDBColumnNames) then
        LClause := LClause + ' and ';
    end;
    Result := AddToSQLWhereClause(Result, '(' + LClause + ')');
  end;
end;

procedure TKSQLBuilder.InternalBuildSelectQuery(const AViewTable: TKViewTable;
  const AFilter: string; const AOrderBy: string; const ADBQuery: TEFDBQuery;
  const AMasterValues: TEFNode; const AFrom: Integer; const AFor: Integer);
var
  I: Integer;
  LCommandText: string;
begin
  Clear;
  FViewTable := AViewTable;
  for I := 0 to AViewTable.FieldCount - 1 do
  begin
    if AViewTable.Fields[I].IsReference then
      AddReferenceFieldTerms(AViewTable.Fields[I])
    else
      AddSelectTerm(AViewTable.Fields[I].QualifiedAliasedDBNameOrExpression);
  end;

  if ADBQuery.Prepared then
    ADBQuery.Prepared := False;
  ADBQuery.Params.BeginUpdate;
  try
    ADBQuery.Params.Clear;
    LCommandText :=
      'select ' +  FSelectTerms +
      ' from ' + GetFromClause + GetSelectWhereClause(AFilter, ADBQuery);
    if (AOrderBy <> '') or (FViewTable.DefaultSorting <> '') then
      LCommandText := LCommandText + ' order by ' + IfThen(AOrderBy <> '', AOrderBy, FViewTable.DefaultSorting);
    LCommandText := ADBQuery.Connection.DBEngineType.AddLimitClause(LCommandText, AFrom, AFor);
    ADBQuery.CommandText := TEFMacroExpansionEngine.Instance.Expand(LCommandText);
  finally
    ADBQuery.Params.EndUpdate;
  end;
  Assert((ADBQuery.Params.Count = 0) or Assigned(AMasterValues));
  for I := 0 to ADBQuery.Params.Count - 1 do
    AMasterValues.GetNode(ADBQuery.Params[I].Name).AssignValueToParam(ADBQuery.Params[I]);
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
    if AViewTable.Fields[I].IsReference then
      AddReferenceFieldTerms(AViewTable.Fields[I])
    else
      AddSelectTerm(AViewTable.Fields[I].QualifiedAliasedDBNameOrExpression);
  end;

  if ADBQuery.Prepared then
    ADBQuery.Prepared := False;
  ADBQuery.Params.BeginUpdate;
  try
    ADBQuery.Params.Clear;
    LCommandText := 'select count(*) from ' + GetFromClause + GetSelectWhereClause(AFilter, ADBQuery);
    ADBQuery.CommandText := TEFMacroExpansionEngine.Instance.Expand(LCommandText);
  finally
    ADBQuery.Params.EndUpdate;
  end;
  Assert((ADBQuery.Params.Count = 0) or Assigned(AMasterValues));
  for I := 0 to ADBQuery.Params.Count - 1 do
    AMasterValues.GetNode(ADBQuery.Params[I].Name).AssignValueToParam(ADBQuery.Params[I]);
end;

end.

