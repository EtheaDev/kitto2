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

    function InternalGetSelectStatement(const AViewTable: TKViewTable): string;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;

    ///	<summary>
    ///	  Builds and returns a SQL statement that selects all fields from the
    ///	  specified view table. Handles joins and table aliases based on model
    ///	  information.
    ///	</summary>
    class function GetSelectStatement(const AViewTable: TKViewTable): string;

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

class function TKSQLBuilder.GetSelectStatement(const AViewTable: TKViewTable): string;
begin
  Assert(Assigned(AViewTable));

  with TKSQLBuilder.Create do
  begin
    try
      Result := InternalGetSelectStatement(AViewTable);
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

function TKSQLBuilder.InternalGetSelectStatement(
  const AViewTable: TKViewTable): string;
var
  I: Integer;
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
  Result :=
    'select ' +  FSelectTerms +
    ' from ' + GetFromClause;
  if AViewTable.DefaultFilter <> '' then
    Result := Result + ' where (' + AViewTable.DefaultFilter + ')';
  if AViewTable.DefaultSorting <> '' then
    Result := Result + ' order by ' + AViewTable.DefaultSorting;
  Result := Environment.MacroExpansionEngine.Expand(Result);
end;

end.

