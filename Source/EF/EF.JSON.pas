{-------------------------------------------------------------------------------
   Copyright 2012-2018 Ethea S.r.l.

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

///	<summary>
///	 Support for reading and writing data in JSON format.
///	</summary>
///	<seealso href="http://www.json.org/">
///	 JSON web site
///	</seealso>
unit EF.JSON;

{$I EF.Defines.inc}

interface

uses
  SysUtils
  , StrUtils
  , JSON
  , DB
  , EF.Types
  , EF.DB
  , EF.Tree
  ;

///	<summary>
///	 <para>
///	  Builds a JSON representation of the pairs.
///	 </para>
///	 <para>
///	  Example: Param1='Foo', Param2=20
///	 </para>
///	 <para>
///	  Result: '["Param1", "Foo"], ["Param2", "20"]'
///	 </para>
///	 <para>
///   Set AReversed to True to switch keys and values.
///	 </para>
///	</summary>
function PairsToJSON(const APairs: TEFPairs; const AReversed: Boolean = False): string;

///	<summary>
///	 <para>
///	  Builds a JSON representation of the triples.
///	 </para>
///	 <para>
///	  Example: A B C, One Two Three
///	 </para>
///	 <para>
///	  Result: '["A", "B", "C"], ["One", "Two", '"Three"]'
///	 </para>
///	</summary>
function TriplesToJSON(const ATriples: TEFTriples): string;

///	<summary>
///  Builds a JSON representation of a dataset's fields values. Creates
///	 the dataset by executing the specified command text against the specified
///	 DB connection. Each record is enclosed in []s and each value is double-quoted.
///	</summary>
///	<example>
///	 <para><c>'["IT", "ITALY"], ["UK", "UNITED KINGDOM"]'</c></para>
///	</example>
function DataSetToJSON(const ADBConnection: TEFDBConnection; const ACommandText: string;
  const AKeyFieldsToAggregate: integer = 0): string; overload;

///	<summary>
///  Builds a JSON representation of a dataset's fields values. Each
///  record is enclosed in []s and each value is double-quoted.
/// </summary>
///	<example>
///	 <para><c>'["IT", "ITALY"], ["UK", "UNITED KINGDOM"]'</c></para>
///	</example>
function DataSetToJSON(const ADataSet: TDataSet;
  const AKeyFieldsToAggregate: integer = 0): string; overload;

function QuoteJSONStr(const AString: string): string; inline;

function JSONNullToEmptyStr(const AJSONValue: string): string; inline;

/// <summary>
///  Escapes control characters in the JSON string.
/// </summary>
function JSONEscape(const AString: string): string;

/// <summary>
///  Adds to ATree all pairs in AJSONObject, recursively.
///  All values are treated as strings.
///  Arrays are not supported. If a JSON array is found, an exception is raised.
/// </summary>
procedure LoadJSONObjectInTree(const AJSONObject: TJSONObject; const ATree: TEFTree);

implementation

uses
  Generics.Collections
  , EF.StrUtils
  ;

function PairsToJSON(const APairs: TEFPairs; const AReversed: Boolean): string;
var
  I: Integer;
begin
  Result := '';
  for I := Low(APairs) to High(APairs) do
  begin
    if AReversed then
      Result := Result + '[' + QuoteJSONStr(APairs[I].Value) + ',' + QuoteJSONStr(APairs[I].Key) + ']'
    else
      Result := Result + '[' + QuoteJSONStr(APairs[I].Key) + ',' + QuoteJSONStr(APairs[I].Value) + ']';
    if I < High(APairs) then
      Result := Result + ',';
  end;
end;

function TriplesToJSON(const ATriples: TEFTriples): string;
var
  I: Integer;
begin
  Result := '';
  for I := Low(ATriples) to High(ATriples) do
  begin
    Result := Result + '['
      + QuoteJSONStr(ATriples[I].Value1) + ','
      + QuoteJSONStr(ATriples[I].Value2) + ','
      + QuoteJSONStr(ATriples[I].Value3) +']';
    if I < High(ATriples) then
      Result := Result + ',';
  end;
end;

function DataSetToJSON(const ADBConnection: TEFDBConnection; const ACommandText: string;
  const AKeyFieldsToAggregate: integer = 0): string;
var
  LDBQuery: TEFDBQuery;
begin
  Assert(Assigned(ADBConnection));
  Assert(ADBConnection.IsOpen);

  LDBQuery := ADBConnection.CreateDBQuery;
  try
    LDBQuery.CommandText := ACommandText;
    LDBQuery.Open;
    try
      Result := DataSetToJSON(LDBQuery.DataSet, AKeyFieldsToAggregate);
    finally
      LDBQuery.Close;
    end;
  finally
    FreeAndNil(LDBQuery);
  end;
end;

function DataSetToJSON(const ADataSet: TDataSet; const AKeyFieldsToAggregate: integer = 0): string;
var
  LBookmark: TBookmark;
  I: Integer;
  LKeyValue: string;
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
        if AKeyFieldsToAggregate > 1 then
        begin
          LKeyValue := '';
          for I := 0 to AKeyFieldsToAggregate -1 do
          begin
            LKeyValue := LKeyValue + ADataSet.Fields[I].AsString;
            if I < AKeyFieldsToAggregate -1 then
              LKeyValue := LKeyValue + ',';
          end;
          Result := Result + QuoteJSONStr(LKeyValue);
          if AKeyFieldsToAggregate < ADataSet.FieldCount then
            Result := Result + ',';
          I := AKeyFieldsToAggregate;
        end
        else
          I := 0;
        for I := I to ADataSet.FieldCount - 1 do
        begin
          Result := Result + QuoteJSONStr(ADataSet.Fields[I].AsString);
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

function QuoteJSONStr(const AString: string): string;
begin
  Result := '"' + ReplaceStr(AString, '"', '\"') + '"';
end;

function JSONNullToEmptyStr(const AJSONValue: string): string;
begin
  if SameText(AJSONValue, 'null') then
    Result := ''
  else
    Result := AJSONValue;
end;

function JSONEscape(const AString: string): string;
begin
  Result := ReplaceStr(AString, sLineBreak, '\n');
  Result := ReplaceStr(Result, #10, '\n');
  Result := ReplaceStr(Result, #13, '\n');
end;

procedure AddJSONPair(const AJSONPair: TJSONPair; const ATree: TEFTree);
var
  LNode: TEFNode;
  LJSONPair: TJSONPair;
begin
  if AJSONPair.JsonValue is TJSONArray then
    raise Exception.Create('JSON arrays not supported in TEFTree.');
  LNode := ATree.AddChild(AJSONPair.JsonString.Value);
  if AJSONPair.JsonValue is TJSONObject then
  begin
    for LJSONPair in TJSONObject(AJSONPair.JsonValue) do
      AddJSONPair(LJSONPair, LNode);
  end
  else
    LNode.AsString := AJSONPair.JsonValue.Value;
end;

procedure LoadJSONObjectInTree(const AJSONObject: TJSONObject; const ATree: TEFTree);
var
  LJSONPair: TJSONPair;
begin
  Assert(Assigned(AJSONObject));
  Assert(Assigned(ATree));

  for LJSONPair in AJSONObject do
    AddJSONPair(LJSONPair, ATree);
end;

end.
