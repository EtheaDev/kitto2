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
  SysUtils, Classes, Generics.Collections, DB,
  EF.Classes,  EF.Tree, EF.DB,
  Kitto.Config, Kitto.Store, Kitto.Metadata.Models, Kitto.Metadata.DataView;

type
  /// <summary>
  ///   Builds SQL select statements on request.
  /// </summary>
  TKSQLBuilder = class
  private
    FUsedReferenceFields: TList<TKModelField>;
    FSelectTerms: string;
    FViewTable: TKViewTable;
    FModel: TKModel;
    procedure Clear;
    procedure AddSelectTerm(const ATerm: string);
    procedure AddReferenceFieldTerms(const AViewField: TKViewField);
    function GetFromClause: string;
    function BuildJoin(const AReferenceField: TKModelField): string;
    function GetSelectWhereClause(const AFilter: string; const ADBQuery: TEFDBQuery): string;
    class function AddDBColumnName(var ADBColumnNames, AValueNames: string;
      const ADBCommand: TEFDBCommand; const ADBColumnName,
      AParamName: string): TParam; static;
    procedure AddFilterBy(const AViewField: TKViewField; const ADBQuery: TEFDBQuery; const ARecord: TKViewTableRecord);
    procedure AssignSelectQueryParams(const AViewTable: TKViewTable; const ADBQuery: TEFDBQuery;
      const AMasterValues: TEFNode);
    function GetModelKeyWhereClause(const AModel: TKModel; const ADBQuery: TEFDBQuery): string;

    /// <summary>
    ///  Replaces '{Q}' tags in AString with AQualification plus a dot.
    ///  If AQualification is '', tags are simply removed from the string.
    ///  Returns the modified string.
    /// </summary>
    /// <remarks>
    ///  If AString does not contain any '{Q}' tags, it is assumed to have
    ///  an implicit one at the beginning.
    /// </remarks>
    function ExpandQualification(const AString, AQualification: string): string;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;

    /// <summary>
    ///  <para>
    ///   Builds in the specified query a select statement that selects
    ///   all fields from the specified view table with an optional filter
    ///   clause. Handles joins and table aliases based on model
    ///   information.
    ///  </para>
    ///  <para>
    ///   If the view table is a detail table, a where clause with
    ///   parameters for the master fields is added as well, and param values
    ///   are set based on AMasterValues.
    ///  </para>
    /// </summary>
    procedure BuildSelectQuery(const AViewTable: TKViewTable; const AFilter: string;
      const AOrderBy: string; const ADBQuery: TEFDBQuery; const AMasterValues: TEFNode = nil;
      const AFrom: Integer = 0; const AFor: Integer = 0);

    procedure BuildCountQuery(const AViewTable: TKViewTable;
      const AFilter: string; const ADBQuery: TEFDBQuery;
      const AMasterValues: TEFNode);

    /// <summary>
    ///  Builds in the specified query a select statement that selects
    ///  all fields from a given record of the specified model.
    ///  Handles joins and table aliases based on model information.
    /// </summary>
    procedure BuildSingletonSelectQuery(const AModel: TKModel;
      const ADBQuery: TEFDBQuery; const AKeyValues: Variant); overload;

    /// <summary>
    ///  Builds in the specified query a select statement that selects
    ///  all fields from a given record of the specified view table.
    ///  Handles joins and table aliases based on model information.
    /// </summary>
    procedure BuildSingletonSelectQuery(const AViewTable: TKViewTable;
      const ADBQuery: TEFDBQuery; const AKeyValues: Variant); overload;

    /// <summary>
    ///  Builds and returns a SQL statement that selects the specified
    ///  field plus all key fields from the specified field's table. AViewField
    ///  must be a reference field, otherwise an exception is raised.
    ///  If ASearchString is specified, the statement includes a WHERE clause
    ///  that filters on the specified value.
    /// </summary>
    procedure BuildLookupSelectStatement(const AViewField: TKViewField;
      const ADBQuery: TEFDBQuery; const ASearchString: string; const ARecord: TKViewTableRecord);

    /// <summary>
    ///  Builds in the specified command an insert statement against
    ///  the specified record's table with a parameter for each value in AValues.
    ///  Also sets the parameter values, so that the command is ready for
    ///  execution.
    /// </summary>
    procedure BuildInsertCommand(const ADBCommand: TEFDBCommand; const ARecord: TKViewTableRecord);

    /// <summary>
    ///  Builds in the specified command an update statement against
    ///  the specified record's table with a parameter for each value in ARecord
    ///  plus a where clause with a parameter for each key field.
    ///  Also sets the parameter values, so that the command is ready for
    ///  execution.
    ///  ARecord must contain at least the key fields.
    /// </summary>
    procedure BuildUpdateCommand(const ADBCommand: TEFDBCommand; const ARecord: TKViewTableRecord);

    /// <summary>
    ///  Builds in the specified command a delete statement against
    ///  the specified record's table with a where clause with a parameter for
    ///  each key field.
    ///  Also sets the parameter values, so that the command is ready for
    ///  execution. AValues must contain at least the key fields.
    /// </summary>
    procedure BuildDeleteCommand(const ADBCommand: TEFDBCommand; const ARecord: TKViewTableRecord);

    /// <summary>
    ///  Builds in the specified query a select statement that selects
    ///  all derived fields from the referenced model, locating the record by
    ///  means of the specified key.
    /// </summary>
    /// <param name="AViewField">
    ///  Originating reference field.
    /// </param>
    /// <param name="ADBQuery">
    ///  Query object into which to build the statement
    ///  and the params.
    /// </param>
    /// <param name="AKeyValues">
    ///  Key values for the referenced model row.
    /// </param>
    /// <result>
    ///  True if a query was generated, False if no derived fields were found.
    /// </result>
    /// <exception cref="Assert">
    ///  The field must be a reference field and at
    ///  least one derived field must exist in the view table.
    /// </exception>
    function BuildDerivedSelectQuery(const AViewField: TKViewField;
      const ADBQuery: TEFDBQuery; const AKeyValues: string): Boolean;

    /// <summary>
    ///  Translates the specified field and direction indication into a SQL
    ///  expression compatible with ORDER BY.
    /// </summary>
    function GetSortClause(const AViewField: TKViewField; const AIsDescending: Boolean): string;

    class procedure CreateAndExecute(const AProc: TProc<TKSQLBuilder>);
  end;


implementation

uses
  StrUtils, Types, Variants,
  EF.Intf, EF.Localization, EF.Types, EF.StrUtils, EF.SQL, EF.Macros,
  Kitto.Types;

{ TKSQLQueryBuilder }

procedure TKSQLBuilder.BuildSelectQuery(const AViewTable: TKViewTable;
  const AFilter: string; const AOrderBy: string; const ADBQuery: TEFDBQuery;
  const AMasterValues: TEFNode; const AFrom: Integer; const AFor: Integer);
var
  I: Integer;
  LSelectClause, LFromClause, LWhereClause, LOrderByClause: string;
  LCommandText: string;
begin
  Assert(Assigned(AViewTable));

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
    LSelectClause := 'select ' +  ExpandQualification(FSelectTerms, AViewTable.Model.DBTableName);
    LFromClause := 'from ' + GetFromClause;
    LWhereClause := GetSelectWhereClause(AFilter, ADBQuery);
    if (AOrderBy <> '') or (FViewTable.DefaultSorting <> '') then
      LOrderByClause := 'order by ' + IfThen(AOrderBy <> '', AOrderBy,
        ExpandQualification(FViewTable.DefaultSorting, AViewTable.Model.DBTableName));
    LCommandText := ADBQuery.Connection.DBEngineType.AddLimitClause(
      LSelectClause, LFromClause, LWhereClause, LOrderByClause, AFrom, AFor);
    ADBQuery.CommandText := TEFMacroExpansionEngine.Instance.Expand(LCommandText);
  finally
    ADBQuery.Params.EndUpdate;
  end;
  AssignSelectQueryParams(AViewTable, ADBQuery, AMasterValues);
end;

procedure TKSQLBuilder.BuildSingletonSelectQuery(
  const AViewTable: TKViewTable; const ADBQuery: TEFDBQuery;
  const AKeyValues: Variant);
var
  I: Integer;
  LCommandText: string;
begin
  Assert(Assigned(AViewTable));

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
    LCommandText := 'select ' +  ExpandQualification(FSelectTerms, AViewTable.Model.DBTableName) +
      ' from ' + GetFromClause +
      ' where ' + ExpandQualification(GetModelKeyWhereClause(AViewTable.Model, ADBQuery), AViewTable.Model.DBTableName);
    ADBQuery.CommandText := TEFMacroExpansionEngine.Instance.Expand(LCommandText);
  finally
    ADBQuery.Params.EndUpdate;
  end;
  for I := 0 to ADBQuery.Params.Count - 1 do
    ADBQuery.Params[I].Value := AKeyValues[I];
end;

procedure TKSQLBuilder.BuildSingletonSelectQuery(const AModel: TKModel;
  const ADBQuery: TEFDBQuery; const AKeyValues: Variant);
var
  LCommandText: string;
  I: Integer;
begin
  Assert(Assigned(AModel));

  Clear;
  FModel := AModel;

  FModel.EnumPhysicalFields(
    procedure (AField: TKModelField)
    begin
      AddSelectTerm(AField.AliasedDBColumnNameOrExpression);
    end);

  if ADBQuery.Prepared then
    ADBQuery.Prepared := False;
  ADBQuery.Params.BeginUpdate;
  try
    ADBQuery.Params.Clear;
    LCommandText :=
      'select ' +  FSelectTerms +
      ' from ' + AModel.DBTableName + ' where ' + ExpandQualification(GetModelKeyWhereClause(AModel, ADBQuery), AModel.DBTableName);
    ADBQuery.CommandText := TEFMacroExpansionEngine.Instance.Expand(LCommandText);
  finally
    ADBQuery.Params.EndUpdate;
  end;
  for I := 0 to ADBQuery.Params.Count - 1 do
    ADBQuery.Params[I].Value := AKeyValues[I];
end;

procedure TKSQLBuilder.BuildCountQuery(const AViewTable: TKViewTable;
  const AFilter: string; const ADBQuery: TEFDBQuery; const AMasterValues: TEFNode);
var
  I: Integer;
  LCommandText: string;
begin
  Assert(Assigned(AViewTable));

  Clear;
  FViewTable := AViewTable;
{ TODO :
Process all fields to build the from clause. A future refactoring might
build only those that affect the count (inner joins). }
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
  AssignSelectQueryParams(AViewTable, ADBQuery, AMasterValues);
end;

class function TKSQLBuilder.AddDBColumnName(var ADBColumnNames, AValueNames: string;
  const ADBCommand: TEFDBCommand; const ADBColumnName, AParamName: string): TParam;
begin
  if ADBColumnNames = '' then
  begin
    ADBColumnNames := ADBColumnName;
    AValueNames := ':' + AParamName;
  end
  else
  begin
    ADBColumnNames := ADBColumnNames + ', ' + ADBColumnName;
    AValueNames := AValueNames + ', :' + AParamName;
  end;
  Result := ADBCommand.Params.CreateParam(ftUnknown, AParamName, ptInput);
end;

procedure TKSQLBuilder.BuildInsertCommand(const ADBCommand: TEFDBCommand; const ARecord: TKViewTableRecord);
var
  LCommandText: string;
  I: Integer;
  LViewField: TKViewField;
  LDBColumnNames: string;
  LValueNames: string;
  J: Integer;
  LProcessedRefFields: TList<TKViewField>;

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
      for I := 0 to ARecord.FieldCount - 1 do
      begin
        if TKViewField.IsURLFieldName(ARecord[I].FieldName) then
          Continue;

        LViewField := ARecord[I].ViewField;
        if Assigned(LViewField) and ARecord[I].IsModified and LViewField.CanInsert then
        begin
          if LViewField.IsReference then
          begin
            if not IsRefFieldProcessed(LViewField) then
            begin
              for J := 0 to LViewField.ModelField.FieldCount - 1 do
                AddDBColumnName(LDBColumnNames, LValueNames, ADBCommand,
                  LViewField.ModelField.Fields[J].DBColumnName,
                  LViewField.ModelField.Fields[J].FieldName);
              MarkRefFieldAsProcessed(LViewField);
            end
          end
          else if LViewField.Model = ARecord.ViewTable.Model then
            AddDBColumnName(LDBColumnNames, LValueNames, ADBCommand,
              LViewField.ModelField.DBColumnName, ARecord[I].FieldName);
        end;
      end;
    finally
      FreeAndNil(LProcessedRefFields);
    end;

    // Add model fiels with default values that are not in the view.
    ARecord.ViewTable.Model.EnumFields(
      procedure (AField: TKModelField)
      var
        LDefaultValue: Variant;
      begin
        Assert(Assigned(AField));

        LDefaultValue := AField.DefaultValue;
        if not VarIsNull(LDefaultValue) and (ARecord.ViewTable.FindFieldByModelField(AField) = nil) then
          AddDBColumnName(LDBColumnNames, LValueNames, ADBCommand,
            AField.DBColumnName, AField.DBColumnName).Value := LDefaultValue;
      end
    );

    // Assemble and execute.
    LCommandText := LCommandText + LDBColumnNames + ') values (' + LValueNames + ')';
    ADBCommand.CommandText := LCommandText;
  finally
    ADBCommand.Params.EndUpdate;
  end;
  for I := 0 to ADBCommand.Params.Count - 1 do
    if ADBCommand.Params[I].IsNull then
      ARecord.FieldByName(ADBCommand.Params[I].Name).AssignValueToParam(ADBCommand.Params[I]);
end;

procedure TKSQLBuilder.BuildUpdateCommand(const ADBCommand: TEFDBCommand; const ARecord: TKViewTableRecord);
var
  LCommandText: string;
  I: Integer;
  LKeyFields: TStringDynArray;
  LViewField: TKViewField;
  LDBColumnNames: string;
  J: Integer;
  LProcessedRefFields: TList<TKViewField>;
  LParamNames: TStringDynArray;

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
        if TKViewField.IsURLFieldName(ARecord[I].FieldName) then
          Continue;

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
      LParamNames := ARecord.ViewTable.GetAliasedNames(LKeyFields);
      for I := 0 to Length(LKeyFields) - 1 do
      begin
        if I > 0 then
          LCommandText := LCommandText + ' and ';
        LCommandText := LCommandText + LKeyFields[I] + ' = :' + LParamNames[I];
        ADBCommand.Params.CreateParam(ftUnknown, LParamNames[I], ptInput);
      end;
      ADBCommand.CommandText := LCommandText;
    end;
  finally
    ADBCommand.Params.EndUpdate;
  end;
  for I := 0 to ADBCommand.Params.Count - 1 do
    ARecord.FieldByName(ADBCommand.Params[I].Name).AssignValueToParam(ADBCommand.Params[I]);
end;

procedure TKSQLBuilder.BuildDeleteCommand(const ADBCommand: TEFDBCommand; const ARecord: TKViewTableRecord);
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

function TKSQLBuilder.BuildDerivedSelectQuery(const AViewField: TKViewField;
  const ADBQuery: TEFDBQuery; const AKeyValues: string): Boolean;
var
  LDerivedFields: TArray<TKViewField>;
  LCommandText: string;
  I: Integer;
  LDerivedField: TKViewField;
  LKeyDBColumnNames: TStringDynArray;
  LModel: TKModel;
  LClause: string;
  LDBColumnName: string;
  LKeyValues: TArray<string>;
begin
  Assert(Assigned(AViewField));
  Assert(AViewField.IsReference);
  Assert(Assigned(ADBQuery));
  LDerivedFields := AViewField.GetDerivedFields;
  Assert(Length(LDerivedFields) > 0);

  Result := False;
  LModel := AViewField.ModelField.ReferencedModel;

  LCommandText := '';
  for LDerivedField in LDerivedFields do
  begin
    if SameText(LDerivedField.FieldName, AViewField.FieldName) then
      LDBColumnName := ExpandQualification(LModel.CaptionField.DBColumnNameOrExpression, '') + ' ' + LDerivedField.ModelField.DBColumnName
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
    if Length(LKeyDBColumnNames) <> 1 then
      LKeyValues := AKeyValues.Split([TKConfig.Instance.MultiFieldSeparator], None)
    else
    begin
      SetLength(LKeyValues, 1);
      LKeyValues[0] := AKeyValues;
    end;

    if Length(LKeyValues) = Length(LKeyDBColumnNames) then
    begin
      LClause := '';
      for I := 0 to High(LKeyDBColumnNames) do
      begin
        LDBColumnName := LKeyDBColumnNames[I];
        if LClause = '' then
          LClause := LDBColumnName + ' = :' + LDBColumnName
        else
          LClause := LClause + ' and ' + LDBColumnName + ' = :' + LDBColumnName;
        ADBQuery.Params.CreateParam(ftUnknown, LDBColumnName, ptInput);
      end;
      LCommandText := SetSQLWhereClause(LCommandText, LClause);
      ADBQuery.CommandText := TEFMacroExpansionEngine.Instance.Expand(LCommandText);
      for I := 0 to ADBQuery.Params.Count - 1 do
        ADBQuery.Params[I].Value := LKeyValues[I];
      Result := True;
    end;
  finally
    ADBQuery.Params.EndUpdate;
  end;
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

function TKSQLBuilder.ExpandQualification(const AString, AQualification: string): string;
begin
  if AQualification = '' then
    Result := ReplaceStr(AString, '{Q}', '')
  else
    Result := ReplaceStr(AString, '{Q}', AQualification + '.');
end;

procedure TKSQLBuilder.Clear;
begin
  FViewTable := nil;
  FSelectTerms := '';
  FUsedReferenceFields.Clear;
end;

class procedure TKSQLBuilder.CreateAndExecute(const AProc: TProc<TKSQLBuilder>);
var
  LSQLBuilder: TKSQLBuilder;
begin
  Assert(Assigned(AProc));

  LSQLBuilder := TKSQLBuilder.Create;
  try
    AProc(LSQLBuilder);
  finally
    FreeAndNil(LSQLBuilder);
  end;
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

procedure TKSQLBuilder.BuildLookupSelectStatement(const AViewField: TKViewField;
  const ADBQuery: TEFDBQuery; const ASearchString: string; const ARecord: TKViewTableRecord);
var
  LLookupModel: TKModel;
  LDefaultFilter: string;
  LLookupModelDefaultFilter: string;
  LColumnNames: TStringDynArray;
  LQueryText: string;
  LLookupFilter: string;
begin
  Assert(Assigned(AViewField));
  Assert(Assigned(ADBQuery));
  Assert(AViewField.IsReference);

  LLookupModel := AViewField.ModelField.ReferencedModel;
  LColumnNames := LLookupModel.GetKeyDBColumnNames(False, True);
  LQueryText := 'select ' + Join(LColumnNames, ', ');
  // Ensure caption field is contained in select list.
  if not LLookupModel.CaptionField.IsKey then
    LQueryText := LQueryText + ', ' + ExpandQualification(LLookupModel.CaptionField.AliasedDBColumnNameOrExpression, '');
  LQueryText := LQueryText + ' from ' + LLookupModel.DBTableName;

  LLookupModelDefaultFilter := LLookupModel.DefaultFilter;
  if LLookupModelDefaultFilter <> '' then
    LQueryText := AddToSQLWhereClause(LQueryText, '(' + ExpandQualification(LLookupModelDefaultFilter, '') + ')');

  LDefaultFilter := AViewField.DefaultFilter;
  if LDefaultFilter <> '' then
    LQueryText := AddToSQLWhereClause(LQueryText, '(' + ExpandQualification(LDefaultFilter, '')  + ')', AViewField.DefaultFilterConnector);

  LLookupFilter := AViewField.LookupFilter;
  if LLookupFilter <> '' then
    LQueryText := AddToSQLWhereClause(LQueryText, '(' + ExpandQualification(LLookupFilter, '')  + ')');

  if ASearchString <> '' then
    LQueryText := AddToSQLWhereClause(LQueryText, '(' + AViewField.ModelField.ReferencedModel.CaptionField.DBColumnName + ' like ''%' + ASearchString + '%'')');

  LQueryText := LQueryText + ' order by ' + ExpandQualification(LLookupModel.CaptionField.DBColumnNameOrExpression, '');

  LQueryText := TEFMacroExpansionEngine.Instance.Expand(LQueryText);
  ADBQuery.CommandText := ARecord.ExpandExpression(LQueryText);
  AddFilterBy(AViewField, ADBQuery, ARecord);
end;

procedure TKSQLBuilder.AddFilterBy(const AViewField: TKViewField; const ADBQuery: TEFDBQuery; const ARecord: TKViewTableRecord);
var
  LFilterByFields: TArray<TKFilterByViewField>;
  LClause: string;
  I: Integer;
  LParams: TParams;

  function GetReferenceClause(const AReferenceField: TKModelField{; const AReferencedField: TKModelField}): string;
  var
    I: Integer;
    LParamName: string;
    LParam: TParam;
  begin
    for I := 0 to AReferenceField.FieldCount - 1 do
    begin
      LParamName := AReferenceField.Fields[I].DBColumnName;
      Result := LParamName + ' = :' + LParamName;
      LParam := LParams.CreateParam(ftUnknown, LParamName, ptInput);
      ARecord.FieldByName(AReferenceField.Fields[I].FieldName).AssignValueToParam(LParam);
      if I < AReferenceField.FieldCount - 1 then
        Result := Result + ' and ';
    end;
  end;

begin
  Assert(Assigned(AViewField));
  Assert(Assigned(ADBQuery));
  Assert(Assigned(ARecord));

  LFilterByFields := AViewField.GetFilterByFields;
  if Length(LFilterByFields) > 0 then
  begin
    LParams := TParams.Create(nil);
    try
      LClause := '';
      for I := Low(LFilterByFields) to High(LFilterByFields) do
      begin
        Assert(LFilterByFields[I].SourceField.IsReference);
        LClause := LClause + GetReferenceClause(LFilterByFields[I].SourceField.ModelField);
        if I < High(LFilterByFields) then
          LClause := LClause + ' and ';
      end;
      ADBQuery.CommandText := AddToSQLWhereClause(ADBQuery.CommandText, '(' + LClause + ')');
      ADBQuery.Params.Assign(LParams);
    finally
      FreeAndNil(LParams);
    end;
  end;
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
  LReferencedModelDefaultFilter: string;
  LFieldDefaultFilter: string;
begin
  Assert(Assigned(AReferenceField));

  LCorrelationName := AReferenceField.DBColumnName;

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

  LReferencedModelDefaultFilter := AReferenceField.ReferencedModel.DefaultFilter;
  if LReferencedModelDefaultFilter <> '' then
    Result := Result + ' and (' + ExpandQualification(LReferencedModelDefaultFilter, LCorrelationName) + ')';

  LFieldDefaultFilter := AReferenceField.DefaultFilter;
  if LFieldDefaultFilter <> '' then
    Result := Result + ' and (' + ExpandQualification(LFieldDefaultFilter, LCorrelationName) + ')';

  Result := Result + ')';
end;

procedure TKSQLBuilder.AddReferenceFieldTerms(const AViewField: TKViewField);
var
  LFields: TKModelFieldArray;
  I: Integer;

  function GetQualifiedExpression(const AModelField: TKModelField): string;
  begin
    if AModelField.Expression <> '' then
      Result := ExpandQualification(AModelField.Expression, AViewField.ModelField.DBColumnName)
    else
      Result := AViewField.ModelField.DBColumnName + '.' + AModelField.DBColumnName;
  end;

begin
  Assert(Assigned(FViewTable));
  Assert(Assigned(AViewField));
  Assert(AViewField.IsReference);

  if not FUsedReferenceFields.Contains(AViewField.ModelField) then
    FUsedReferenceFields.Add(AViewField.ModelField);

  // Add the caption field of the referenced model as well.
  // The reference field name is used as table alias.
  AddSelectTerm(GetQualifiedExpression(AViewField.ModelField.ReferencedModel.CaptionField)
    + ' ' + AViewField.ModelField.FieldName);
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
  LClause, LParamName: string;
begin
  Result := '';
  if FViewTable.DefaultFilter <> '' then
    Result := Result + ' where (' + ExpandQualification(FViewTable.DefaultFilter, FViewTable.Model.DBTableName) + ')';
  if AFilter <> '' then
    Result := AddToSQLWhereClause(Result, ExpandQualification(AFilter, FViewTable.Model.DBTableName));

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
      LParamName := LMasterFieldDBColumnNames[I];
      LParamName := FViewTable.MasterTable.ApplyFieldAliasedName(LParamName);
      LClause := LClause + FViewTable.Model.DBTableName + '.' + LDetailFieldDBColumnNames[I] + ' = :' + LParamName;
      ADBQuery.Params.CreateParam(ftUnknown, LParamName, ptInput);
      if I < High(LDetailFieldDBColumnNames) then
        LClause := LClause + ' and ';
    end;
    Result := AddToSQLWhereClause(Result, '(' + LClause + ')');
  end;
end;

function TKSQLBuilder.GetSortClause(const AViewField: TKViewField; const AIsDescending: Boolean): string;
begin
  if AViewField.Expression <> '' then
    Result := ExpandQualification(AViewField.Expression, AViewField.Table.Model.DBTableName)
  else
    Result := AViewField.QualifiedDBNameOrExpression;
  if AIsDescending then
    Result := Result + ' desc';
end;

function TKSQLBuilder.GetModelKeyWhereClause(const AModel: TKModel; const ADBQuery: TEFDBQuery): string;
var
  I: Integer;
  LKeyDBColumnNames: TStringDynArray;
begin
  LKeyDBColumnNames := AModel.GetKeyDBColumnNames();
  Result := '';
  for I := Low(LKeyDBColumnNames) to High(LKeyDBColumnNames) do
  begin
    Result := Result + '{Q}' + LKeyDBColumnNames[I] + ' = :' + LKeyDBColumnNames[I];
    ADBQuery.Params.CreateParam(ftUnknown, LKeyDBColumnNames[I], ptInput);
    if I < High(LKeyDBColumnNames) then
      Result := Result + ' and ';
  end;
end;

procedure TKSQLBuilder.AssignSelectQueryParams(const AViewTable: TKViewTable;
  const ADBQuery: TEFDBQuery; const AMasterValues: TEFNode);
var
  I: Integer;
  LField: TKModelField;
  LParam: TParam;
  LFieldName: string;
begin
  Assert((ADBQuery.Params.Count = 0) or Assigned(AMasterValues));
  for I := 0 to ADBQuery.Params.Count - 1 do
  begin
    LParam := ADBQuery.Params[I];
    LField := AViewTable.Model.FindFieldByPhysicalName(LParam.Name);
    if Assigned(LField) then
    begin
      if LField.IsReference then
        LFieldName := LField.ReferencedModel.Fields[I].FieldName
      else if Assigned(LField.ParentField) and LField.ParentField.IsReference then
        LFieldName := LField.ParentField.ReferencedModel.Fields[I].FieldName
      else
        LFieldName := LField.FieldName;
    end
    else
      LFieldName := LParam.Name;
    AMasterValues.GetNode(LFieldName).AssignValueToParam(LParam);
  end;
end;

end.

