unit Kitto.SQL;

{$I Kitto.Defines.inc}

interface

uses
  Classes, Generics.Collections,
  EF.Classes,  EF.DB,
  Kitto.Metadata.Models, Kitto.Metadata.Views;

type
  ///	<summary>
  ///	  Builds SQL select statements on request.
  ///	</summary>
  TKSQLQueryBuilder = class(TEFComponent)
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
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;

    ///	<summary>
    ///	  Builds and returns a SQL statement that selects all fields from the
    ///	  specified view table. Handles joins and table aliases based on model
    ///	  information.
    ///	</summary>
    function GetSelectStatement(const AViewTable: TKViewTable): string;
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
  FReferenceAliases := TDictionary<TKModelReference, string>.Create;
end;

destructor TKSQLQueryBuilder.Destroy;
begin
  inherited;
  FreeAndNil(FReferenceAliases);
end;

procedure TKSQLQueryBuilder.Clear;
begin
  FViewTable := nil;
  FSelectTerms := '';
  FReferenceAliases.Clear;
end;

procedure TKSQLQueryBuilder.AddSelectTerm(const ATerm: string);
begin
  if FSelectTerms = '' then
    FSelectTerms := ATerm
  else
    FSelectTerms := FSelectTerms + ', ' + ATerm;
end;

function TKSQLQueryBuilder.GetSelectStatement(const AViewTable: TKViewTable): string;
var
  I: Integer;
begin
  Assert(Assigned(AViewTable));

  Clear;
  FViewTable := AViewTable;
  for I := 0 to AViewTable.FieldCount - 1 do
  begin
    if AViewTable.Fields[I].Model = AViewTable.Model then
      AddSelectTerm(AViewTable.Fields[I].AliasedNameOrExpression)
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

function TKSQLQueryBuilder.GetFromClause: string;
var
  I: Integer;
begin
  Assert(Assigned(FViewTable));

  Result := FViewTable.ModelName;
  for I := 0 to FReferenceAliases.Count - 1 do
    Result := Result + sLineBreak + BuildJoin(FReferenceAliases.Keys.ToArray[I]);
end;

function TKSQLQueryBuilder.BuildJoin(const AReference: TKModelreference): string;

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
    Result := Result + AReference.Fields[I].FieldName + ' = ' + LRefAlias + '.' + AReference.ReferencedFields[I].FieldName;
    if I < AReference.FieldCount - 1 then
      Result := Result + ' and ';
  end;
  Result := Result + ')';
end;

function TKSQLQueryBuilder.GetReferencedFieldTerm(const AViewField: TKViewField): string;
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

procedure TKSQLQueryBuilder.AddReferenceAlias(const AReference: TKModelReference);
var
  LAliasName: string;

  procedure IncAliasName;
  var
    LCounter: Integer;
  begin
    if TryStrToInt(LAliasName[Length(LAliasName)], LCounter) then
    begin
      Assert(LCounter < 9);
      LAliasName[Length(LAliasName)] := IntToStr(LCounter + 1)[1];
    end
    else
      LAliasName := LAliasName + '2';
  end;

begin
  if not FReferenceAliases.ContainsKey(AReference) then
  begin
    LAliasName := AReference.ReferencedModel.ModelName;
    while FReferenceAliases.ContainsValue(LAliasName) do
      IncAliasName;
    FReferenceAliases.Add(AReference, LAliasName);
  end;
end;

function TKSQLQueryBuilder.GetViewFieldReference(const AViewField: TKViewField): TKModelreference;
var
  LReferenceName: string;
  LReferences: TList<TKModelReference>;
begin
  Assert(Assigned(FViewTable));
  Assert(Assigned(AViewField));

  Result := nil;
  LReferences := TList<TKModelReference>.Create;
  try
    FViewTable.Model.GetReferencesToModel(AViewField.Model, LReferences);

    // Only one reference - must be the one we're after.
    if LReferences.Count = 1 then
      Result := LReferences[0]
    else if LReferences.Count > 1 then
    begin
      // More than one reference - select one.
      LReferenceName := AViewField.GetString('Reference');
      if LReferenceName <> '' then
        // Exception if the specified FK is not existing or not pointing to the
        // right table.
        Result := FViewTable.Model.ReferenceByName(LReferenceName);
    end;
  finally
    FreeAndNil(LReferences);
  end;

  if not Assigned(Result) then
    raise EKError.CreateFmt(_('No reference found for field %s.'), [AViewField.AliasedName]);
  if Result.ReferencedModel <> AViewField.Model then
    raise EKError.CreateFmt(_('Incorrect reference %s for field %s.'), [Result.ConstraintName, AViewField.AliasedName]);
end;

end.

