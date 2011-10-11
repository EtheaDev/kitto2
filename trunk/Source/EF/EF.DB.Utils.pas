unit EF.DB.Utils;

{$I EF.Defines.inc}

interface

uses
  Types, Classes, DB,
  EF.Types, EF.Macros, EF.Classes, EF.Tree, EF.DB;

{
  Copies the value from ASource to ADestination, taking ASource.DataType into
  account.
}
procedure AssignEFNodeValueToParam(const ASource: TEFNode; const ADestination: TParam);

{
  Copies the value from ASource to ADestination, taking ASource.DataType into
  account.
}
procedure AssignParamValueToEFNode(const ASource: TParam; const ADestination: TEFNode);

{
  Copies the name and value from ASource to ADestination, taking
  ASource.DataType into account.
}
procedure AssignEFNodeToParam(const ASource: TEFNode; const ADestination: TParam);

{
  Returns the value of the first column of the first record of the cursor
  returned by ASQLStatement. It is advised to pass a single-column singleton
  SQL statement for efficiency reasons. May return an empty or unassigned
  Variant.
}
function GetSingletonValue(const ADBConnection: TEFDBConnection;
  const ASQLStatement: string): Variant;

{
  Adds to AStrings one item for each record of the cursor returned by
  ASQLStatement. The item is the string representation of the first
  field. Returns the number of added items.
}
function GetStringsFromDB(const ADBConnection: TEFDBConnection;
  const ASQLStatement: string; const AStrings: TStrings): Integer;

function GetFieldValuesAsStrings(const AField: TField): TStringDynArray;

implementation

uses
  SysUtils, Variants, TypInfo, StrUtils, Provider,
  EF.Localization, EF.StrUtils;

procedure AssignEFNodeValueToParam(const ASource: TEFNode; const ADestination: TParam);
begin
  Assert(Assigned(ASource));
  Assert(Assigned(ADestination));

  if ASource.IsNull then
    ADestination.Clear
  else
  begin
    case ASource.DataType of
      edtUnknown, edtString: ADestination.AsString := ASource.AsString;
      edtInteger: ADestination.AsInteger := ASource.AsInteger;
      edtDate: ADestination.AsDate := ASource.AsDate;
      edtTime: ADestination.AsTime := ASource.AsTime;
      edtDateTime: ADestination.AsDateTime := ASource.AsDateTime;
      edtBoolean: ADestination.AsBoolean := ASource.AsBoolean;
      edtCurrency: ADestination.AsCurrency := ASource.AsCurrency;
      edtFloat: ADestination.AsFloat := ASource.AsFloat;
      edtDecimal: ADestination.AsFMTBCD := ASource.AsDecimal;
    else
      raise EEFError.CreateFmt('AssignEFNodeValueToParam: data type %s not supported',
        [EFDataTypeToString(ASource.DataType)]);
    end;
  end;
end;

procedure AssignParamValueToEFNode(const ASource: TParam;
  const ADestination: TEFNode);
begin
  Assert(Assigned(ASource));
  Assert(Assigned(ADestination));

  if ASource.IsNull then
    ADestination.Clear
  else
  begin
    case ASource.DataType of
      ftString{$IFDEF D10+}, ftWideString{$ENDIF}, ftFixedChar: ADestination.AsString := ASource.AsString;
      ftMemo{$IFDEF D10+}, ftWideMemo{$ENDIF}: ADestination.AsString := ASource.AsMemo;
      ftSmallint, ftWord, ftInteger, ftAutoinc: ADestination.AsInteger := ASource.AsInteger;
      ftBoolean: ADestination.AsBoolean := ASource.AsBoolean;
      ftDate: ADestination.AsDate := ASource.AsDate;
      ftTime: ADestination.AsTime := ASource.AsTime;
      ftDateTime, ftTimeStamp: ADestination.AsDateTime := ASource.AsDateTime;
      ftCurrency: ADestination.AsCurrency := ASource.AsCurrency;
      ftFloat: ADestination.AsFloat := ASource.AsFloat;
      ftBCD, ftFMTBcd: ADestination.AsDecimal := ASource.AsFMTBCD;
    else
      raise EEFError.CreateFmt('AssignParamValueToEFNode: data type %s not supported',
        [GetEnumName(TypeInfo(TFieldType), Ord(ASource.DataType))]);
    end;
  end;
end;

procedure AssignEFNodeToParam(const ASource: TEFNode; const ADestination: TParam);
begin
  AssignEFNodeValueToParam(ASource, ADestination);
  ADestination.Name := ASource.Name; 
end;

function GetSingletonValue(const ADBConnection: TEFDBConnection;
  const ASQLStatement: string): Variant;
var
  LQuery: TEFDBQuery;
begin
  Assert(Assigned(ADBConnection));
  Assert(ASQLStatement <> '');

  LQuery := ADBConnection.CreateDBQuery;
  try
    LQuery.CommandText := ASQLStatement;
    LQuery.Open;
    try
      if LQuery.DataSet.IsEmpty then
        Result := Null
      else
        Result := LQuery.DataSet.Fields[0].Value;
    finally
      LQuery.Close;
    end;
  finally
    FreeAndNil(LQuery);
  end;
end;

function GetStringsFromDB(const ADBConnection: TEFDBConnection;
  const ASQLStatement: string; const AStrings: TStrings): Integer;
var
  LQuery: TEFDBQuery;
begin
  Assert(Assigned(ADBConnection));
  Assert(ASQLStatement <> '');
  Assert(Assigned(AStrings));

  LQuery := ADBConnection.CreateDBQuery;
  try
    LQuery.CommandText := ASQLStatement;
    LQuery.Open;
    try
      Result := 0;
      while not LQuery.DataSet.Eof do
      begin
        AStrings.Add(LQuery.DataSet.Fields[0].AsString);
        Inc(Result);
        LQuery.DataSet.Next;
      end;
    finally
      LQuery.Close;
    end;
  finally
    FreeAndNil(LQuery);
  end;
end;

procedure CopyDataSetStructure(const ASourceDataSet, ADestinationDataSet: TDataSet);
var
  LFieldIndex: Integer;

  procedure CreateField(const AFieldDef: TFieldDef; const AOwner: TComponent);
  begin
    AFieldDef.CreateField(AOwner, nil, AFieldDef.Name, True);
  end;

begin
  Assert(Assigned(ADestinationDataSet));
  Assert(not ADestinationDataSet.Active);

  // Clear the destination structure.
  for LFieldIndex := ADestinationDataSet.FieldCount - 1 downto 0 do
    ADestinationDataSet.Fields[LFieldIndex].Free;
  ADestinationDataSet.FieldDefs.Clear;
  if not Assigned(ASourceDataSet) then
    Exit;

  // Copy the structure from fields.
  for LFieldIndex := 0 to ASourceDataSet.FieldCount - 1 do
  begin
    with ASourceDataSet.Fields[LFieldIndex] do
      ADestinationDataSet.FieldDefs.Add(FieldName, DataType, Size, Required);
  end;

  // Failing that, copy from field defs.
  if ADestinationDataSet.FieldDefs.Count = 0 then
  begin
    ASourceDataSet.FieldDefs.Update;
    ADestinationDataSet.FieldDefs := ASourceDataSet.FieldDefs;
  end;
end;

procedure SetDataSetFieldValues(const ADataSet: TDataSet; const AFieldNames: string;
  const AFieldValues: Variant);
var
  LBookmark: TBookmark;
begin
  Assert(Assigned(ADataSet));
  Assert(AFieldNames <> '');

  ADataSet.DisableControls;
  try
    LBookmark := ADataSet.Bookmark;
    try
      ADataSet.First;
      while not ADataSet.Eof do
      begin
        ADataSet.Edit;
        ADataSet[AFieldNames] := AFieldValues;
        ADataSet.Post;
        ADataSet.Next;
      end;
    finally
      ADataSet.Bookmark := LBookmark;
    end;
  finally
    ADataSet.EnableControls;
  end;
end;

function GetEmptyFieldValue(const AFieldType: TFieldType): Variant;
begin
  case AFieldType of
    ftString, ftFixedChar, ftMemo, ftWideString:
      Result := '';
    {$IFDEF D10+}
    ftFixedWideChar, ftWideMemo:
      Result := '';
    {$ENDIF}
    ftSmallint, ftInteger, ftWord, ftFloat, ftBCD, ftCurrency, ftFMTBcd, ftLargeint:
      Result := 0;
    ftBoolean:
      Result := False;
    ftDate, ftTime, ftDateTime, ftTimeStamp:
      Result := 0;
    else
      Result := 0;
  end;
end;

function GetDataSetFieldNames(const ADataSet: TDataSet;
  const ADelimiter: Char = ';'): string;
var
  LFieldNames: TStrings;
begin
  Assert(Assigned(ADataSet));

  LFieldNames := TStringList.Create;
  try
    ADataSet.GetFieldNames(LFieldNames);
    LFieldNames.Delimiter := ADelimiter;
    Result := LFieldNames.DelimitedText;
  finally
    FreeAndNil(LFieldNames);
  end;
end;

function GetFieldValuesAsStrings(const AField: TField): TStringDynArray;
var
  LBookmark: TBookmark;
  LString: string;
begin
  Assert(Assigned(AField));

  LBookmark := AField.DataSet.Bookmark;
  try
    LString := '';
    AField.DataSet.First;
    while not AField.DataSet.Eof do
    begin
      if LString = '' then
        LString := AField.AsString
      else
        LString := LString + '§' + AField.AsString;
      AField.DataSet.Next;
    end;
    Result := Split(LString, '§');
  finally
    AField.DataSet.Bookmark := LBookmark;
  end;
end;

procedure AssignNodeValueToParam(const ASource: TEFNode; const ADestination: TParam);
begin
  Assert(Assigned(ASource));
  Assert(Assigned(ADestination));

  if ASource.IsNull then
    ADestination.Clear
  else
  begin
    ADestination.AsString := ASource.AsString;
    case ASource.DataType of
      edtDecimal: ADestination.AsFMTBCD := ASource.AsDecimal;
      edtBoolean: ADestination.AsBoolean := ASource.AsBoolean;
      edtCurrency: ADestination.AsCurrency := ASource.AsCurrency;
      edtDate: ADestination.AsDate := ASource.AsDate;
      edtDateTime: ADestination.AsDateTime := ASource.AsDateTime;
      edtFloat: ADestination.AsFloat := ASource.AsFloat;
      edtInteger: ADestination.AsInteger := ASource.AsInteger;
      edtTime: ADestination.AsTime := ASource.AsTime;
      edtUnknown, edtString: ADestination.AsString := ASource.AsString;
    else
      raise EEFError.CreateFmt('AssignNodeValueToParam: data type %s not supported',
        [EFDataTypeToString(ASource.DataType)]);
    end;
  end;
end;

end.
