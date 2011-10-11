unit EF.SQL;

{$I EF.Defines.inc}

interface

uses
  Classes, DB,
  EF.Types, EF.DB;

type
  ///	<summary>
  ///	  Static class that holds SQL-statement building routines.
  ///	</summary>
  TEFSQLBuilder = class
  public
    // Builds a parameterized SQL insert command with ATableName as target
    // and AFieldNames as field and param names. AFieldNames is a list of
    // field names.
    class procedure BuildInsertCommand(const ATableName: string;
      const AFieldNames: TStrings; const ACommand: TEFDBCommand);
    // Builds a SQL delete command with ATableName as target
    // and no where clause.
    class procedure BuildEmptyTableCommand(const ATableName: string;
      const ACommand: TEFDBCommand);
    // Builds a parameterized SQL select query with ASelectList in the
    // select clause, ATableName as source and AKeyFieldNames as fields
    // in the where clause.
    class procedure BuildRecordExistsQuery(const ATableName: string;
      const ASelectList: string; const AKeyFieldNames: TStrings;
      const AQuery: TEFDBQuery);
    // Builds a parameterized SQL select query with ALookupResultFieldNames in the
    // select clause, ALookupTableName as source and ALookupKeyFieldNames as fields
    // in the where clause. AKeyFieldNames are used as names for the params in
    // the where clause.
    class procedure BuildLookupQuery(const ALookupTableName,
      ALookupResultFieldNames, ALookupKeyFieldNames,
      AKeyFieldNames: string; const AQuery: TEFDBQuery);
    // Builds a parameterized SQL update command with ATableName as target,
    // AFieldNames as fields in the set clause and AKeyFieldNames as fields
    // in the where clause.
    class procedure BuildUpdateCommand(const ATableName: string;
      const AFieldNames, AKeyFieldNames: TStrings; const ACommand: TEFDBCommand);
    // Builds a parameterized SQL delete command with ATableName as target
    // and AKeyFieldNames as fields in the where clause.
    class procedure BuildDeleteCommand(const ATableName: string;
      const AKeyFieldNames: TStrings; const ACommand: TEFDBCommand);
  end;

{
  Executes the given query and returns an array of values from the first row
  of the result set. The returned array is zero-based and has an element for
  each field in the source select statement.
  If no rows are returned, the result is a single Null.
  The query may contain parameters, in which case you are required to pass
  ADBParams.
}
function SQLLookup(const ADBConnection: TEFDBConnection;
  const ASQLStatement: string; const ADBParams: TParams = nil): Variant; overload;

{
  Executes the given query and returns an array of values from the first row
  of the result set. The returned array is zero-based and has an element for
  each field in the result set.
  If no rows are returned, the result is a single Null.
  The query may contain parameters, in which case they should all be assigned
  values before calling this function.
}
function SQLLookup(const AQuery: TEFDBQuery): Variant; overload;

const
  // Quote character in SQL.
  SQLQuote = '''';

{
  Works like Delphi's QuotedStr, but uses SQLQuote instead of a hard-coded '.
}
function SQLQuotedStr(const AString: string): string;

{
  Removes leading and trailing SQLQuotes from the string.
}
function RemoveSQLQuotes(const AString: string): string;

{
  Returns the SQL where clause from a given SQL statement. The return value
  does not include the "where" keyword.
}
function GetSQLWhereClause(const ASQL: string): string;

{
  Changes the SQL where clause of a given SQL statement. If the statement
  doesn't initially have a where clause, it is added.
}
function SetSQLWhereClause(const ASQL, ANEFClause: string): string;

{
  Adds text to the SQL where clause of a given statement; the nEF text is
  connected to any existing text in the where clause through the specified
  connector.
}
function AddToSQLWhereClause(const ASQL, ANEFClause: string;
  const AConnector: string = 'and'): string;

{
  Returns the SQL into clause from a given SQL statement. This function works
  on the into clause that can precede the from clause in some databases
  (namely SQL Server).
}
function GetSQLIntoClause(const ASQL: string): string;

{
  Returns True if the specified statement is a query, that is it returns
  a result set. By default all select statements (except the
  select ... into form) are queries and everything else is not.
}
function IsQuery(const ASQLStatement: string): Boolean;

const
  {
    Marks the beginning of a part of a SQL statement that the EF parser will ignore.
  }
  EF_PARSER_SKIP_BEGIN_MARKER = '/**EF_PARSER_SKIP_BEGIN**/';

  {
    Marks the end of a part of a SQL statement that the EF parser will ignore.
  }
  EF_PARSER_SKIP_END_MARKER = '/**EF_PARSER_SKIP_END**/';

implementation

uses
  SysUtils, StrUtils, Variants,
  EF.StrUtils, EF.Localization;

{ TEFSQLBuilder }

class procedure TEFSQLBuilder.BuildDeleteCommand(const ATableName: string;
  const AKeyFieldNames: TStrings; const ACommand: TEFDBCommand);
var
  LKeyFieldNameIndex: Integer;
  LCommandText: string;
begin
  Assert(Assigned(AKeyFieldNames));
  Assert(Assigned(ACommand));
  if ATableName = '' then
    raise EEFError.Create(_('Unspecified table name.'));
  if AKeyFieldNames.Count = 0 then
    raise EEFError.Create(_('Unspecified key field name list.'));

  if ACommand.Prepared then
    ACommand.Prepared := False;
  ACommand.Params.BeginUpdate;
  try
    ACommand.Params.Clear;
    LCommandText := 'delete from ' + ATableName + ' where ';
    for LKeyFieldNameIndex := 0 to AKeyFieldNames.Count - 1 do
    begin
      if LKeyFieldNameIndex <> 0 then
        LCommandText := LCommandText + ' and ';
      LCommandText := LCommandText + AKeyFieldNames[LKeyFieldNameIndex]
        + ' = :' + AKeyFieldNames[LKeyFieldNameIndex];
      ACommand.Params.CreateParam(ftUnknown, AKeyFieldNames[LKeyFieldNameIndex], ptInput);
    end;
    ACommand.CommandText := LCommandText;
  finally
    ACommand.Params.EndUpdate;
  end;
end;

class procedure TEFSQLBuilder.BuildEmptyTableCommand(const ATableName: string;
  const ACommand: TEFDBCommand);
begin
  Assert(Assigned(ACommand));
  if ATableName = '' then
    raise EEFError.Create(_('Unspecified table name.'));

  if ACommand.Prepared then
    ACommand.Prepared := False;
  ACommand.CommandText := 'delete from ' + ATableName;
end;

class procedure TEFSQLBuilder.BuildInsertCommand(const ATableName: string;
  const AFieldNames: TStrings; const ACommand: TEFDBCommand);
var
  LFieldNameIndex: Integer;
  LCommandText: string;
begin
  Assert(Assigned(AFieldNames));
  Assert(Assigned(ACommand));
  if ATableName = '' then
    raise EEFError.Create(_('Unspecified table name.'));
  if AFieldNames.Count = 0 then
    raise EEFError.Create(_('Unspecified field name list.'));

  if ACommand.Prepared then
    ACommand.Prepared := False;
  ACommand.Params.BeginUpdate;
  try
    ACommand.Params.Clear;
    LCommandText := 'insert into ' + ATableName + ' (';
    for LFieldNameIndex := 0 to AFieldNames.Count - 1 do
    begin
      if LFieldNameIndex <> 0 then
        LCommandText := LCommandText + ', ';
      LCommandText := LCommandText + AFieldNames[LFieldNameIndex];
    end;
    LCommandText := LCommandText + ') values (';
    for LFieldNameIndex := 0 to AFieldNames.Count - 1 do
    begin
      if LFieldNameIndex <> 0 then
        LCommandText := LCommandText + ', ';
      LCommandText := LCommandText + ':' + AFieldNames[LFieldNameIndex];
      ACommand.Params.CreateParam(ftUnknown, AFieldNames[LFieldNameIndex], ptInput);
    end;
    LCommandText := LCommandText + ')';
    ACommand.CommandText := LCommandText;
  finally
    ACommand.Params.EndUpdate;
  end;
end;

class procedure TEFSQLBuilder.BuildRecordExistsQuery(
  const ATableName: string; const ASelectList: string;
  const AKeyFieldNames: TStrings; const AQuery: TEFDBQuery);
var
  LKeyFieldNameIndex: Integer;
  LCommandText: string;
begin
  Assert(Assigned(AKeyFieldNames));
  Assert(Assigned(AQuery));
  if ATableName = '' then
    raise EEFError.Create(_('Unspecified table name.'));
  if ASelectList = '' then
    raise EEFError.Create(_('Unspecified field name list.'));
  if AKeyFieldNames.Count = 0 then
    raise EEFError.Create(_('Unspecified key field name list.'));

  if AQuery.Prepared then
    AQuery.Prepared := False;
  AQuery.Params.BeginUpdate;
  try
    AQuery.Params.Clear;
    LCommandText := 'select ' + ASelectList + ' from ' + ATableName + ' where ';
    for LKeyFieldNameIndex := 0 to AKeyFieldNames.Count - 1 do
    begin
      if LKeyFieldNameIndex <> 0 then
        LCommandText := LCommandText + ' and ';
      LCommandText := LCommandText + AKeyFieldNames[LKeyFieldNameIndex]
        + ' = :' + AKeyFieldNames[LKeyFieldNameIndex];
      AQuery.Params.CreateParam(ftUnknown, AKeyFieldNames[LKeyFieldNameIndex], ptInput);
    end;
    AQuery.CommandText := LCommandText;
  finally
    AQuery.Params.EndUpdate;
  end;
end;

class procedure TEFSQLBuilder.BuildUpdateCommand(const ATableName: string;
  const AFieldNames, AKeyFieldNames: TStrings; const ACommand: TEFDBCommand);
var
  LFieldNameIndex, LKeyFieldNameIndex: Integer;
  LCommandText: string;
  LSetClause: string;
begin
  Assert(Assigned(AFieldNames));
  Assert(Assigned(AKeyFieldNames));
  Assert(Assigned(ACommand));

  if ATableName = '' then
    raise EEFError.Create(_('Unspecified table name.'));
  if AFieldNames.Count = 0 then
    raise EEFError.Create(_('Unspecified field name list.'));
  if AKeyFieldNames.Count = 0 then
    raise EEFError.Create(_('Unspecified key field name list.'));

  if ACommand.Prepared then
    ACommand.Prepared := False;
  ACommand.Params.BeginUpdate;
  try
    ACommand.Params.Clear;
    LCommandText := 'update ' + ATableName + ' set ';
    LSetClause := '';
    for LFieldNameIndex := 0 to AFieldNames.Count - 1 do
    begin
      // Don't put key fields in the set clause, as we don't support
      // two parameters with the same name in a SQL statement.
      if AKeyFieldNames.IndexOf(AFieldNames[LFieldNameIndex]) < 0 then
      begin
        if LSetClause <> '' then
          LSetClause := LSetClause + ', ';
        LSetClause := LSetClause + AFieldNames[LFieldNameIndex]
          + ' = :' + AFieldNames[LFieldNameIndex];
        ACommand.Params.CreateParam(ftUnknown, AFieldNames[LFieldNameIndex], ptInput);
      end;
    end;
    LCommandText := LCommandText + LSetClause + ' where ';
    for LKeyFieldNameIndex := 0 to AKeyFieldNames.Count - 1 do
    begin
      if LKeyFieldNameIndex <> 0 then
        LCommandText := LCommandText + ' and ';
      LCommandText := LCommandText + AKeyFieldNames[LKeyFieldNameIndex]
        + ' = :' + AKeyFieldNames[LKeyFieldNameIndex];
      ACommand.Params.CreateParam(ftUnknown, AKeyFieldNames[LKeyFieldNameIndex], ptInput);
    end;
    ACommand.CommandText := LCommandText;
  finally
    ACommand.Params.EndUpdate;
  end;
end;

class procedure TEFSQLBuilder.BuildLookupQuery(const ALookupTableName,
  ALookupResultFieldNames, ALookupKeyFieldNames, AKeyFieldNames: string;
  const AQuery: TEFDBQuery);
var
  LStatement: string;

  function BuildSelectStatement(const ASelectFieldNames, ATableNameSpecification: string): string;
  begin
    if Pos('SELECT', AnsiUpperCase(Trim(ATableNameSpecification))) = 1 then
      Result := ATableNameSpecification
    else
      Result := 'select ' + ASelectFieldNames + ' from ' + ATableNameSpecification;
  end;

  procedure AddWhereClause(var AStatement: string; const ALookupKeyFieldNames,
    AKeyFieldNames: string);
  var
    LParamIndex: Integer;
    LParamNameList: TStrings;
    LFieldNameList: TStrings;
  begin
    if Pos('WHERE', AnsiUpperCase(AStatement)) = 0 then
      AStatement := AStatement + ' where '
    else
      AStatement := AStatement + ' and ';
    LParamNameList := TStringList.Create;
    try
      LParamNameList.CommaText := AKeyFieldNames;
      LFieldNameList := TStringList.Create;
      try
        LFieldNameList.CommaText := ALookupKeyFieldNames;
        if LParamNameList.Count <> LFieldNameList.Count then
          raise Exception.CreateFmt(_('List length mismatch between "%s" and "%s".'), [ALookupKeyFieldNames, AKeyFieldNames]);
        for LParamIndex := 0 to LParamNameList.Count - 1 do
        begin
          LStatement := LStatement + LFieldNameList[LParamIndex] + ' = :'
            + LParamNameList[LParamIndex];
          if LParamIndex < LParamNameList.Count - 1 then
            LStatement := LStatement + ' and ';
        end;
      finally
        LFieldNameList.Free;
      end;
    finally
      LParamNameList.Free;
    end;
  end;

begin
  LStatement := BuildSelectStatement(ALookupResultFieldNames, ALookupTableName);
  AddWhereClause(LStatement, ALookupKeyFieldNames, AKeyFieldNames);
  AQuery.CommandText := LStatement;
end;

function SQLLookup(const ADBConnection: TEFDBConnection;
  const ASQLStatement: string; const ADBParams: TParams = nil): Variant;
var
  LQuery: TEFDBQuery;
begin
  LQuery := ADBConnection.CreateDBQuery;
  try
    LQuery.CommandText := ASQLStatement;
    if Assigned(ADBParams) then
      LQuery.Params.AssignValues(ADBParams);
    Result := SQLLookup(LQuery);
  finally
    FreeAndNil(LQuery);
  end;
end;

function SQLLookup(const AQuery: TEFDBQuery): Variant; overload;
var
  LFieldIndex: Integer;
begin
  AQuery.Open;
  try
    if AQuery.DataSet.IsEmpty then
      Result := Null
    else
    begin
      Result := VarArrayCreate([0, AQuery.DataSet.FieldCount - 1], varVariant);
      for LFieldIndex := 0 to AQuery.DataSet.FieldCount - 1 do
        Result[LFieldIndex] := AQuery.DataSet.Fields[LFieldIndex].Value;
    end;
  finally
    AQuery.Close;
  end;
end;

function SQLQuotedStr(const AString: string): string;
begin
  Result := SQLQuote + StringReplace(AString, SQLQuote, SQLQuote + SQLQuote, [rfReplaceAll]) + SQLQuote;
end;

function RemoveSQLQuotes(const AString: string): string;
var
  LStringLength: Integer;
begin
  LStringLength := Length(AString);
  if LStringLength < 2 then
    Result := AString
  else
    if (AString[1] = SQLQuote) and (AString[LStringLength] = SQLQuote) then
      Result := Copy(AString, 2, LStringLength - 2);
end;

const
  sqlSelect = 'select';
  sqlInto = 'into';
  sqlFrom = 'from';
  sqlWhere = 'where';
  sqlOrder = 'order';
  sqlGroup = 'group';
  sqlBy = 'by';
  sqlHaving = 'having';
  sqlPlan = 'plan';
  sqlUnion = 'union';

type
  {
    Strips from a given SQL string all the parts that EF cannot or shouldn't
    parse, and replaces them with known identifier strings. It can then replace
    back the identifier strings with the stripped parts they represent.

    This class is meant to be uses this way: pass a SQL string to it and get back
    the stripped version; parse the stripped version; pass back the stripped
    version (or part of it) to have it "unstripped".
  }
  TEFSQLStripper = class
  private
    FStrippedParts: TStrings;
    function RecursiveStrip(const ASQLString: string): string;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
    {
      Strips any skippable parts from the passed string and replaces them with
      identifiers. Keeps track of each stripped part and its identifier.
    }
    function Strip(const ASQLString: string): string;
    {
      replaces back the identifiers in the passed string with the previously
      stripped parts.
    }
    function Unstrip(const AStrippedSQLString: string): string;
  end;

{ TEFSQLStripper }

procedure TEFSQLStripper.AfterConstruction;
begin
  inherited;
  FStrippedParts := TStringList.Create;
end;

destructor TEFSQLStripper.Destroy;
begin
  FreeAndNil(FStrippedParts);
  inherited;
end;

function TEFSQLStripper.Strip(const ASQLString: string): string;
begin
  FStrippedParts.Clear;
  Result := RecursiveStrip(ASQLString);
end;

function TEFSQLStripper.RecursiveStrip(const ASQLString: string): string;
var
  LBeginPos: Integer;
  LEndPos: Integer;
  LOffset: Integer;
  LStrippedPart: string;
  LId: string;
begin
  LOffset := 1;
  LBeginPos := PosEx(EF_PARSER_SKIP_BEGIN_MARKER, ASQLString, LOffset);
  if LBeginPos = 0 then
    Result := ASQLString
  else
  begin
    LOffset := LBeginPos + Length(EF_PARSER_SKIP_BEGIN_MARKER) + 1;
    LEndPos := PosEx(EF_PARSER_SKIP_END_MARKER, ASQLString, LOffset);
    if LEndPos = 0 then
      raise EEFError.CreateFmt(_('Unmatched SQL skip marker pair in "%s".'), [ASQLString]);
    LStrippedPart := Copy(ASQLString, LBeginPos, LEndPos - LBeginPos + Length(EF_PARSER_SKIP_END_MARKER));
    LId := '/**EF_STRIPPED_PART_' + IntToStr(FStrippedParts.Count) + '**/';
    FStrippedParts.Add(LId + '=' + LStrippedPart);
    Result := RecursiveStrip(Copy(ASQLString, 1, LBeginPos - 1) +
      LId + Copy(ASQLString, LEndPos + Length(EF_PARSER_SKIP_END_MARKER) + 1, MaxInt));
  end;
end;

function TEFSQLStripper.Unstrip(const AStrippedSQLString: string): string;
var
  LPartIndex: Integer;
begin
  Result := AStrippedSQLString;
  for LPartIndex := 0 to FStrippedParts.Count - 1 do
    Result := StringReplace(Result, FStrippedParts.Names[LPartIndex],
      FStrippedParts.ValueFromIndex[LPartIndex], []);
end;

{
  Returns the contents of a given SQL clause. AOtherClauses should contain a
  list of clauses that syntactically may follow the requested clause. 
  Implements the GetSQL*Clause functions.
}
function GetSQLClause(const ASQL, AClause: string;
  const AOtherClauses: array of string): string;
var
  LSQL: string;
  LAfterFoundClause: string;
  LFoundClausePos, LOtherClausesPos: Integer;
  LStripper: TEFSQLStripper;
begin
  Result := '';
  LStripper := TEFSQLStripper.Create;
  try
    LSQL := LStripper.Strip(ASQL);

    LFoundClausePos := WordPos(AClause, LSQL);
    if LFoundClausePos > 0 then
    begin
      LAfterFoundClause := Copy(LSQL, LFoundClausePos + Length(AClause),
        Length(LSQL) - LFoundClausePos);
      LOtherClausesPos := WordPos(AOtherClauses, LAfterFoundClause);
      if LOtherClausesPos > 0 then
        Result := Trim(Copy(LAfterFoundClause, 1, LOtherClausesPos - 1))
      else
        Result := Trim(LAfterFoundClause);
      Result := LStripper.Unstrip(Result);
    end;
  finally
    FreeAndNil(LStripper);
  end;
end;

{
  Replaces the contents of a given SQL clause in a specified SQL statement.
  AOtherClauses should contain a list of clauses that syntactically may
  follow the requested clause. Implements the SetSQL*Clause functions.
}
function SetSQLClause(const ASQL, AClause, ANEFClause: string;
  AOtherClauses: array of string): string;
var
  LSQL: string;
  LFoundClausePos: Integer;
  LCurrentOtherClausePos, LFirstOtherClausePos: Integer;
  LStripper: TEFSQLStripper;

  function TranslateClause(const AClause: string): string;
  begin
    if (AClause = sqlOrder) or (AClause = sqlGroup) then
      Result := AClause + ' ' + sqlBy
    else
      Result := AClause;
  end;

  {
    Returns the position of the first clause, among those in AOtherClauses,
    in the ASQL string. Returns 0 if no clauses are found.
  }
  function GetFirstOtherClausePos(const ASQL: string): Integer;
  var
    LClauseIndex: Integer;
  begin
    Result := MaxInt;
    for LClauseIndex := Low(AOtherClauses) to High(AOtherClauses) do
    begin
      LCurrentOtherClausePos := WordPos(AOtherClauses[LClauseIndex], ASQL);
      if (LCurrentOtherClausePos <> 0) and (LCurrentOtherClausePos < Result) then
        Result := LCurrentOtherClausePos;
    end;
    if Result = MaxInt then
      Result := 0;
  end;

begin
  LStripper := TEFSQLStripper.Create;
  try
    LSQL := LStripper.Strip(ASQL);

    Result := LSQL;
    LFoundClausePos := WordPos(AClause, Result);
    if LFoundClausePos > 0 then
    begin
      Result := Copy(Result, 1, LFoundClausePos - 1);
      if ANEFClause <> '' then
        Result := Result + ' ' + TranslateClause(AClause) + ' ' + ANEFClause;
      // Other clauses are appended to the tail.
      LFirstOtherClausePos := GetFirstOtherClausePos(LSQL);
      if LFirstOtherClausePos <> 0 then
        Result := Result + ' ' + Copy(LSQL, LFirstOtherClausePos, MaxInt);
    end
    else if ANEFClause <> '' then
    begin
      // No clause, but other clauses found - insert my clause before them.
      LFirstOtherClausePos := GetFirstOtherClausePos(LSQL);
      if LFirstOtherClausePos <> 0 then
        Insert(' ' + AClause + ' ' + ANEFClause + ' ', Result, LFirstOtherClausePos)
      else
        // No other clauses - append.
        Result := Result + ' ' + AClause + ' ' + ANEFClause;
    end;
    Result := LStripper.Unstrip(Result);
  finally
    FreeAndNil(LStripper);
  end;
end;

function GetSQLWhereClause(const ASQL: string): string;
begin
  Result := GetSQLClause(
    ASQL, sqlWhere, [sqlGroup, sqlHaving, sqlUnion, sqlPlan, sqlOrder]);
end;

function GetSQLIntoClause(const ASQL: string): string;
begin
  Result := GetSQLClause(
    ASQL, sqlInto, [sqlFrom]);
end;

function SetSQLWhereClause(const ASQL, ANEFClause: string): string;
begin
  Result := SetSQLClause(ASQL, sqlWhere, ANEFClause,
    [sqlGroup, sqlHaving, sqlUnion, sqlPlan, sqlOrder]);
end;

function AddToSQLWhereClause(const ASQL, ANEFClause: string;
  const AConnector: string = 'and'): string;
var
  LCurrentClause: string;
begin
  if ANEFClause <> '' then
  begin
    LCurrentClause := GetSQLWhereClause(ASQL);
    if LCurrentClause = '' then
      LCurrentClause := ANEFClause
    else
      LCurrentClause := LCurrentClause + ' ' + AConnector + ' ' + ANEFClause;
    Result := SetSQLWhereClause(ASQL, LCurrentClause);
  end
  else
    Result := ASQL;
end;

function IsQuery(const ASQLStatement: string): Boolean;
var
  LSQLStatement: string;
  LFirstToken: string;
  LStripper: TEFSQLStripper;
begin
  LStripper := TEFSQLStripper.Create;
  try
    LSQLStatement := LStripper.Strip(ASQLStatement);

    Result := False;
    LSQLStatement := Trim(LSQLStatement);
    LFirstToken := Copy(LSQLStatement, 1, Pos(' ', LSQLStatement) - 1);
    if SameText(LFirstToken, 'select') and (GetSQLIntoClause(LSQLStatement) = '') then
      Result := True;
  finally
    FreeAndNil(LStripper);
  end;
end;

end.
