unit Kitto.JSON;

interface

uses
  DB,
  EF.Types, EF.Classes;

///	<summary>
///	  <para>
///	    Builds a JSON representation of the pairs.
///	  </para>
///	  <para>
///	    Example: Param1='Foo', Param2=20
///	  </para>
///	  <para>
///	    Result: '["Foo", "Param1"], ["20", "Param2"]'
///	  </para>
///	  <para>
///	    This particular representation is useful for encoding a data view
///	    field's AllowedValues as a JSON array suitable for an ExtJS combo box.
///     Set AReversed to False to switch keys and values.
///	  </para>
///	</summary>
function PairsToJSON(const APairs: TEFPairs; const AReversed: Boolean = True): string;

{
  Builds a JSON representation of a dataset's fields values.
  Creates the dataset by executing the specified command text against the
  specified DB connection.
  Each record is enclosed in []s and each value is double-quoted.
  Example:
  '["IT", "ITALY"], ["UK", "UNITED KINGDOM"]'
  This particular representation is useful for encoding a GUIField's
  LookupCommandText as a JSON array suitable for an ExtJS combo box.
}
function DataSetToJSON(const ADBConnection: IEFDBConnection; const ACommandText: string): string; overload;

{
  Builds a JSON representation of the dataset's fields values. Each record is enclosed
  in []s and each value is double-quoted.
  Example:
  '["IT", "ITALY"], ["UK", "UNITED KINGDOM"]'
  This particular representation is useful for encoding a GUIField's
  LookupCommandText as a JSON array suitable for an ExtJS combo box.
}
function DataSetToJSON(const ADataSet: TDataSet): string; overload;

implementation

uses
  EF.Intf, EF.StrUtils;

function PairsToJSON(const APairs: TEFPairs; const AReversed: Boolean): string;
var
  I: Integer;
begin
  Result := '';
  for I := Low(APairs) to High(APairs) do
  begin
    if AReversed then
      Result := Result + '["' + APairs[I].Value + '", "' + APairs[I].Key + '"]'
    else
      Result := Result + '["' + APairs[I].Key + '", "' + APairs[I].Value + '"]';
    if I < High(APairs) then
      Result := Result + ',';
  end;
end;

function DataSetToJSON(const ADBConnection: IEFDBConnection; const ACommandText: string): string;
var
  LDBQuery: IEFDBQuery;
begin
  Assert(Assigned(ADBConnection));
  Assert(ADBConnection.IsOpen);

  LDBQuery := ADBConnection.CreateDBQuery;
  try
    LDBQuery.CommandText := ACommandText;
    LDBQuery.Open;
    try
      Result := DataSetToJSON(LDBQuery.DataSet);
    finally
      LDBQuery.Close;
    end;
  finally
    FreeAndNilEFIntf(LDBQuery);
  end;
end;

function DataSetToJSON(const ADataSet: TDataSet): string;
var
  LBookmark: TBookmark;
  I: Integer;
begin
  Assert(Assigned(ADataSet));
  Assert(ADataSet.Active);

  Result := '';
  ADataSet.DisableControls;
  try
    LBookmark := ADataSet.Bookmark;
    try
      ADataSet.First;
      while not ADataSet.Eof do
      begin
        Result := Result + '[';
        for I := 0 to ADataSet.FieldCount - 1 do
        begin
          Result := Result + '"' + ADataSet.Fields[I].AsString + '"';
          if I < ADataSet.FieldCount - 1 then
            Result := Result + ',';
        end;
        Result := Result + '],';
        ADataSet.Next;
      end;
      Result := StripSuffix(Result, ',');
    finally
      ADataSet.Bookmark := LBookmark;
    end;
  finally
    ADataSet.EnableControls;
  end;

end;

end.
