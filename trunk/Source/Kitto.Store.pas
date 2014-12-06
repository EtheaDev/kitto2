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

unit Kitto.Store;

{$I Kitto.Defines.inc}

interface

uses
  SysUtils, Types, Classes, DB, Generics.Collections,
  superobject,
  EF.Tree, EF.DB, EF.Types,
  Kitto.Metadata.Models;

type
  TKKey = class;

  TKKeyField = class(TEFNode)
  private
    function GetKey: TKKey;
    function GetFieldName: string;
  public
    property Key: TKKey read GetKey;
    property FieldName: string read GetFieldName;
  end;

  TKKey = class(TEFNode)
  private
    function GetFieldCount: Integer;
    function GetField(I: Integer): TKKeyField;
  protected
    function GetChildClass(const AName: string): TEFNodeClass; override;
  public
    property Fields[I: Integer]: TKKeyField read GetField; default;
    property FieldCount: Integer read GetFieldCount;

    procedure SetFieldNames(const AFieldNames: TStringDynArray);
    function GetFieldNames: TStringDynArray;
  end;

  TKRecord = class;

  TKField = class;

  TKFieldChangeEvent = procedure(const AField: TKField; const AOldValue, ANewValue: Variant) of object;

  TKHeaderField = class;

  TKField = class(TEFNode)
  strict private
    FHeaderField: TKHeaderField;
    FIsModified: Boolean;
    function GetFieldName: string;
    function GetParentRecord: TKRecord;
    function GetJSONName: string;
  strict protected
    function GetXMLTagName: string; virtual;
    function GetName: string; override;
    procedure SetValue(const AValue: Variant); override;
    procedure ValueChanged(const AOldValue: Variant; const ANewValue: Variant); override;
    function GetDataType: TEFDataType; override;
  public
    property HeaderField: TKHeaderField read FHeaderField write FHeaderField;
    procedure MarkAsUnmodified;
    procedure SetToNull; override;
    property IsModified: Boolean read FIsModified;
    property ParentRecord: TKRecord read GetParentRecord;
    function GetAsJSON(const AForDisplay: Boolean): string;
    function GetAsJSONValue(const AForDisplay: Boolean; const AQuote: Boolean = True;
      const AEmptyNulls: Boolean = False): string; virtual;
    function GetAsXML(const AForDisplay: Boolean): string;
    function GetAsXMLValue(const AForDisplay: Boolean;
      const AEmptyNulls: Boolean = False): string; virtual;
    property FieldName: string read GetFieldName;
  end;

  TKStore = class;
  TKRecords = class;

  TKRecordState = (rsNew, rsClean, rsDirty, rsDeleted);

  TKRecord = class(TEFNode)
  strict private
    FBackup: TEFNode;
    { TODO : move state management in view table record? }
    FState: TKRecordState;
    FDetailStores: TObjectList<TKStore>;
    FOnFieldChange: TKFieldChangeEvent;
    function GetRecords: TKRecords;
    function GetKey: TKKey;
    function GetField(I: Integer): TKField;
    function GetFieldCount: Integer;
    function GetDetailsStore(I: Integer): TKStore;
    function GetDetailStoreCount: Integer;
    procedure EnsureDetailStores;
    function GetStore: TKStore;
    function GetIsDeleted: Boolean;
    function GetIsNew: Boolean;
  strict protected
    property State: TKRecordState read FState;
    function GetChildClass(const AName: string): TEFNodeClass; override;

    ///	<summary>
    ///   Called by ReadFromNode after setting all values. Descendants
    ///	  may overwrite some fields.
    /// </summary>
    procedure InternalAfterReadFromNode; virtual;

    ///	<summary>
    ///	  A function that receives in input a store field name and returns the
    ///   corresponding database field name (see ReadFromFields).
    ///   The default implementation assumes that the store and database field
    ///   names have matching names.
    /// </summary>
    function TranslateFieldName(const AFieldName: string): string; virtual;

    function GetXMLTagName: string; virtual;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
    procedure FieldChanged(const AField: TKField; const AOldValue, ANewValue: Variant); virtual;
  public
    property Records: TKRecords read GetRecords;
    property Store: TKStore read GetStore;
    property Key: TKKey read GetKey;
    property Fields[I: Integer]: TKField read GetField; default;
    property FieldCount: Integer read GetFieldCount;
    function FindField(const AFieldName: string): TKField;
    function FieldByName(const AFieldName: string): TKField;
    function MatchesValues(const AValues: TEFNode): Boolean;

    ///	<summary>
    ///	  Reads field values from the current dataset record, applying
    ///   field name translation by calling the provided anonymous method.
    ///	</summary>
    procedure ReadFromFields(const AFields: TFields);

    ///	<summary>Reads any values from the specified node by name. Fields whose
    ///	names are not in the passed node are set to Null.</summary>
    procedure ReadFromNode(const ANode: TEFNode);

    function GetAsJSON(const AForDisplay: Boolean): string;
    function GetAsXML(const AForDisplay: Boolean): string;
    ///	<summary>Expand an expression that contains reference to field of record.
    ///	For example: 'Activity: %Description%'.</summary>
    function ExpandExpression(const AExpression: string): string;

    procedure MarkAsModified;
    procedure MarkAsDeleted;
    procedure MarkAsClean;

    property IsNew: Boolean read GetIsNew;
    property IsDeleted: Boolean read GetIsDeleted;
    property DetailStoreCount: Integer read GetDetailStoreCount;
    property DetailStores[I: Integer]: TKStore read GetDetailsStore;
    function AddDetailStore(const AStore: TKStore): TKStore;

    procedure Backup;
    procedure Restore;

    function ChangesPending: Boolean;

    property OnFieldChange: TKFieldChangeEvent read FOnFieldChange write FOnFieldChange;

    function AddField(const AHeaderField: TKHeaderField): TKField;
  end;

  TKRecords = class(TEFNode)
  private
    FKey: TKKey;
    function GetRecordCount: Integer;
    function GetRecordByIndex(I: Integer): TKRecord;
    procedure SetKey(const AValue: TKKey);
    function GetStore: TKStore;
    function GetRecordCountExceptNewAndDeleted: Integer;
  strict protected
    function GetXMLTagName: string; virtual;
    function GetChildClass(const AName: string): TEFNodeClass; override;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
  public
    procedure Clear; override;
    property Store: TKStore read GetStore;

    property Key: TKKey read FKey write SetKey;
    function FindRecord(const AValues: TEFNode): TKRecord;
    function GetRecord(const AValues: TEFNode): TKRecord;
    property Records[I: Integer]: TKRecord read GetRecordByIndex; default;
    property RecordCount: Integer read GetRecordCount;
    property RecordCountExceptNewAndDeleted: Integer read GetRecordCountExceptNewAndDeleted;

    function Append: TKRecord;
    function AppendAndInitialize: TKRecord;
    procedure Remove(const ARecord: TKRecord);

    function GetAsJSON(const AForDisplay: Boolean;
      const AFrom: Integer = 0; const AFor: Integer = 0): string;

    function GetAsXML(const AForDisplay: Boolean;
      const AFrom: Integer = 0; const AFor: Integer = 0): string;
  end;

  TKHeaderField = class(TEFNode)
  private
    function GetFieldName: string;
  public
    property FieldName: string read GetFieldName;
  end;

  TKHeader = class(TEFNode)
  private
    function GetField(I: Integer): TKHeaderField;
    function GetFieldCount: Integer;
  protected
    function GetChildClass(const AName: string): TEFNodeClass; override;
  public
    ///	<summary>
    ///	  Adds to ARecord one field node with no name and the correct datatype
    ///	  for each header field.
    ///	</summary>
    ///	<remarks>
    ///	  Names are not set in all records in order to save space. Fields are
    ///	  meant to be accessed by name in the header and by position in records.
    ///	</remarks>
    procedure Apply(const ARecord: TKRecord);

    property FieldCount: Integer read GetFieldCount;
    property Fields[I: Integer]: TKHeaderField read GetField;

    function AddField(const AFieldName: string): TKHeaderField;
    procedure GetFieldNames(List: TStrings);
    function FindField(const AFieldName: string): TKHeaderField;
    function FieldByName(const AFieldName: string): TKHeaderField;
  end;

  TKRecordPredicate = TPredicate<TKRecord>;

  TKStore = class(TEFTree)
  strict private
    FChangeNotificationsDisabledCount: Integer;
    FHeader: TKHeader;
    function GetRecords: TKRecords;
    function GetKey: TKKey;
    procedure SetKey(const AValue: TKKey);
    function GetRecordCount: Integer;
    function GetHeader: TKHeader;
    function GetRecordCountExceptNewAndDeleted: Integer;
  strict protected
    function GetChildClass(const AName: string): TEFNodeClass; override;
  public
    destructor Destroy; override;
  public
    procedure DisableChangeNotifications;
    procedure EnableChangeNotifications;
    function ChangeNotificationsEnabled: Boolean;

    property Key: TKKey read GetKey write SetKey;
    property Header: TKHeader read GetHeader;
    property Records: TKRecords read GetRecords;
    property RecordCount: Integer read GetRecordCount;
    property RecordCountExceptNewAndDeleted: Integer read GetRecordCountExceptNewAndDeleted;

    procedure Load(const ADBConnection: TEFDBConnection;
      const ACommandText: string; const AAppend: Boolean = False); overload;
    procedure Load(const ADBQuery: TEFDBQuery; const AAppend: Boolean = False); overload;

    ///	<summary>
    ///   Appends a record and fills it with the specified values.
    ///	</summary>
    function AppendRecord(const AValues: TEFNode): TKRecord;

    ///	<summary>Removes the record from the store, if present.</summary>
    ///	<remarks>Calling this method will NOT trigger any database operation.
    ///	It is meant to cancel pending changes.</remarks>
    ///	<seealso cref="TKRecord.MarkAsDeleted"></seealso>
    procedure RemoveRecord(const ARecord: TKRecord);

    function GetAsJSON(const AForDisplay: Boolean; const AFrom: Integer = 0;
      const AFor: Integer = 0): string;

    function GetAsXML(const AForDisplay: Boolean; const AFrom: Integer = 0;
      const AFor: Integer = 0): string;

    function ChangesPending: Boolean;

    ///	<summary>Iterates all records in the store (regardless of state)
    /// calling APredicate for each record and then calling AProc when
    ///	the predicate returns True. Use the optional predicate to filter
    /// records before passing them to AProc.</summary>
    ///	<param name="AProc">A procedure that receives a TKRecord.</param>
    ///	<param name="AProc">A function that receives a TKRecord and
    /// returns a Boolean indicating whether to include the record in the
    /// enumeration or not. You can pass predefined predicates such as All
    /// and NotDeleted or code your own.</param>
    procedure Iterate(const AProc: TProc<TKRecord>;
      const APredicates: array of TPredicate<TKRecord>);

    ///	<summary>Pass this as a predicate to one of the predicate-accepting
    /// methodse to specify that you want to include all records (including
    /// those marked as deleted).</summary>
    function All: TPredicate<TKRecord>;

    ///	<summary>Pass this as a predicate to one of the predicate-accepting
    /// methodse to specify that you want to include all records except
    /// those marked as deleted.</summary>
    function ExcludeDeleted: TPredicate<TKRecord>;

    ///	<summary>Returns the number of records in which all the specified
    /// predicates hold (that is, all return True for a given record).</summary>
    function Count(const APredicates: array of TPredicate<TKRecord>): Integer; overload;

    ///	<summary>Returns the number of non-deleted records in which the specified
    /// field has the specified value. This is a special case of the more generic
    /// predicate-based Count method.</summary>
    function Count(const AFieldName: string; const AValue: Variant): Integer; overload;

    function Max(const AFieldName: string): Variant;
    function Min(const AFieldName: string): Variant;

    ///	<summary>
    ///   Locates and returns a record from the key values stored in AKey.
    ///   Raises an exception if the record is not found.
    ///	<param name="AKey">
    ///   Object containing at least on top-level pair for each key value.
    /// </param>
    ///	<param name="AFormatSettings">
    ///   Used to interpret string values (all pair values are read as string and
    ///   then converted according to this settings object).
    /// </param>
    ///	<param name="ATranslator">
    ///	  Pass a translation function if key names in AKey do not match
    ///	  wanted child node names and you need to translate them. The function
    ///	  receives the child name and should return the corresponding key name.
    ///	</param>
    ///	<param name="AValueIndex">
    ///   If each pair in AKey contains more than one value, set this param to
    ///   an index >=0 to consider that value. Normally each pair contains a
    ///   single value, so you just don't pass this param.
    /// </param>
    ///	</summary>
    function GetRecord(const AKey: ISuperObject; const AFormatSettings: TFormatSettings;
      const ATranslator: TNameTranslator = nil; const AValueIndex: Integer = -1): TKRecord;
  end;

implementation

uses
  Math, FmtBcd, Variants, StrUtils,
  EF.StrUtils, EF.Localization, EF.JSON, EF.XML,
  Kitto.Types, Kitto.Config;

{ TKStore }

function TKStore.AppendRecord(const AValues: TEFNode): TKRecord;
begin
  DisableChangeNotifications;
  try
    Result := Records.AppendAndInitialize;
    if Assigned(AValues) then
      Result.ReadFromNode(AValues);
  finally
    EnableChangeNotifications;
  end;
end;

function TKStore.ChangeNotificationsEnabled: Boolean;
begin
  Result := FChangeNotificationsDisabledCount <= 0;
end;

function TKStore.ChangesPending: Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to RecordCount - 1 do
  begin
    if Records[I].ChangesPending then
    begin
      Result := True;
      Break;
    end;
  end;
end;

function TKStore.All: TPredicate<TKRecord>;
begin
  Result := function(ARecord: TKRecord): Boolean
            begin
              Result := True;
            end;
end;

procedure TKStore.EnableChangeNotifications;
begin
  Dec(FChangeNotificationsDisabledCount);
end;

function TKStore.ExcludeDeleted: TPredicate<TKRecord>;
begin
  Result := function(ARecord: TKRecord): Boolean
            begin
              Result := not ARecord.IsDeleted;
            end;
end;

procedure TKStore.Iterate(const AProc: TProc<TKRecord>;
  const APredicates: array of TPredicate<TKRecord>);
var
  I: Integer;
  LPredicate: TPredicate<TKRecord>;
  LAllowed: Boolean;
begin
  if Assigned(AProc) then
  begin
    for I := 0 to RecordCount - 1 do
    begin
      LAllowed := True;
      for LPredicate in APredicates do
      begin
        if LAllowed then
          LAllowed := LPredicate(Records[I]);
        if not LAllowed then
          Break;
      end;
      if LAllowed then
        AProc(Records[I]);
    end;
  end;
end;

function TKStore.Count(const APredicates: array of TPredicate<TKRecord>): Integer;
var
  LCount: Integer;
begin
  LCount := 0;
  Iterate(
    procedure (ARecord: TKRecord)
    begin
      Inc(LCount);
    end,
    APredicates);
  Result := LCount;
end;

function TKStore.Count(const AFieldName: string; const AValue: Variant): Integer;
var
  LValue: Variant;
  LPredicate: TPredicate<TKRecord>;
begin
  LValue := AValue;
  LPredicate :=
    function (ARecord: TKRecord): Boolean
    begin
      Result := ARecord.FieldByName(AFieldName).Value = LValue; // Cannot capture AValue.
    end;
  Result := Count([ExcludeDeleted(), LPredicate]);
end;

function TKStore.Max(const AFieldName: string): Variant;
var
  LMaxValue: Variant;
begin
  LMaxValue := Null;
  Iterate(
    procedure (ARecord: TKRecord)
    begin
      if VarIsNull(LMaxValue) or (LMaxValue < ARecord.FieldByName(AFieldName).Value) then
        LMaxValue := ARecord.FieldByName(AFieldName).Value;
    end,
    [ExcludeDeleted()]);
  Result := LMaxValue;
end;

function TKStore.Min(const AFieldName: string): Variant;
var
  LMinValue: Variant;
begin
  LMinValue := Null;
  Iterate(
    procedure (ARecord: TKRecord)
    begin
      if VarIsNull(LMinValue) or (LMinValue < ARecord.FieldByName(AFieldName).Value) then
        LMinValue := ARecord.FieldByName(AFieldName).Value;
    end,
    [ExcludeDeleted()]);
  Result := LMinValue;
end;

destructor TKStore.Destroy;
begin
  FreeAndNil(FHeader);
  inherited;
end;

procedure TKStore.DisableChangeNotifications;
begin
  Inc(FChangeNotificationsDisabledCount);
end;

function TKStore.GetAsJSON(const AForDisplay: Boolean; const AFrom: Integer;
  const AFor: Integer): string;
begin
  Result := Records.GetAsJSON(AForDisplay, AFrom, AFor);
end;

function TKStore.GetAsXML(const AForDisplay: Boolean; const AFrom: Integer;
  const AFor: Integer): string;
begin
  Result := Records.GetAsXML(AForDisplay, AFrom, AFor);
end;

function TKStore.GetChildClass(const AName: string): TEFNodeClass;
begin
  if SameText(AName, 'Header') then
    Result := TKHeader
  else if SameText(AName, 'Records') then
    Result := TKRecords
  else
    Result := inherited GetChildClass(AName);
end;

function TKStore.GetHeader: TKHeader;
begin
  Result := FindChild('Header', True) as TKHeader;
end;

function TKStore.GetKey: TKKey;
begin
  Result := Records.Key;
end;

function TKStore.GetRecord(const AKey: ISuperObject; const AFormatSettings: TFormatSettings;
  const ATranslator: TNameTranslator; const AValueIndex: Integer): TKRecord;
var
  LKeyNode: TEFNode;
begin
  Assert(Assigned(AKey));

  LKeyNode := TEFNode.Create;
  try
    LKeyNode.Assign(Key);
    LKeyNode.SetChildValuesfromSuperObject(AKey, True, AFormatSettings, ATranslator, AValueIndex);
    Result := Records.GetRecord(LKeyNode);
  finally
    FreeAndNil(LKeyNode);
  end;
end;

function TKStore.GetRecordCount: Integer;
begin
  Result := Records.RecordCount;
end;

function TKStore.GetRecordCountExceptNewAndDeleted: Integer;
begin
  Result := Records.RecordCountExceptNewAndDeleted;
end;

function TKStore.GetRecords: TKRecords;
begin
  Result := FindChild('Records', True) as TKRecords;
end;

procedure TKStore.Load(const ADBQuery: TEFDBQuery; const AAppend: Boolean);
var
  LRecord: TKRecord;
begin
  Assert(Assigned(ADBQuery));

  DisableChangeNotifications;
  try
    if not AAppend then
      Records.Clear;
    if not ADBQuery.IsOpen then
      ADBQuery.Open;
    while not ADBQuery.DataSet.Eof do
    begin
      LRecord := Records.AppendAndInitialize;
      LRecord.ReadFromFields(ADBQuery.DataSet.Fields);
      ADBQuery.DataSet.Next;
    end;
  finally
    EnableChangeNotifications;
  end;
end;

procedure TKStore.RemoveRecord(const ARecord: TKRecord);
begin
  Records.Remove(ARecord);
end;

procedure TKStore.Load(const ADBConnection: TEFDBConnection;
  const ACommandText: string; const AAppend: Boolean = False);
var
  LDBQuery: TEFDBQuery;
begin
  Assert(Assigned(ADBConnection));
  Assert(ACommandText <> '');

  LDBQuery := ADBConnection.CreateDBQuery;
  try
    LDBQuery.CommandText := ACommandText;
    LDBQuery.Open;
    try
      Load(LDBQuery, AAppend);
    finally
      LDBQuery.Close;
    end;
  finally
    FreeAndNil(LDBQuery);
  end;
end;

procedure TKStore.SetKey(const AValue: TKKey);
begin
  Records.Key := AValue;
end;

{ TKRecords }

function TKRecords.Append: TKRecord;
begin
  Result := AddChild('Record') as TKRecord;
end;

function TKRecords.AppendAndInitialize: TKRecord;
begin
  Result := Append;
  Store.Header.Apply(Result);
  Assert(Result.FieldCount = Store.Header.ChildCount);
end;

procedure TKRecords.Clear;
begin
  inherited;
  // Must keep the name even when cleared.
  Name := 'Records';
end;

procedure TKRecords.AfterConstruction;
begin
  inherited;
  FKey := TKKey.Create;
end;

destructor TKRecords.Destroy;
begin
  FreeAndNil(FKey);
  inherited;
end;

function TKRecords.FindRecord(const AValues: TEFNode): TKRecord;
var
  I: Integer;
begin
  Assert(Assigned(AValues));

  Result := nil;
  for I := 0 to RecordCount - 1 do
  begin
    if Records[I].MatchesValues(AValues) then
    begin
      Result := Records[I];
      Break;
    end;
  end;
end;

function TKRecords.GetAsJSON(const AForDisplay: Boolean;
  const AFrom: Integer; const AFor: Integer): string;
var
  I: Integer;
  LTo: Integer;
  LCount: Integer;
  LRecordCount: Integer;
begin
  LRecordCount := RecordCountExceptNewAndDeleted;

  if AFor > 0 then
    LTo := Min(LRecordCount - 1, AFrom + AFor - 1)
  else
    LTo := LRecordCount - 1;

  // Loop so that a full page is returned even when there are deleted records.
  Result := '';
  LCount := LTo - AFrom + 1;
  I := AFrom;
  while LCount > 0 do
  begin
    if not Records[I].IsDeleted and not Records[I].IsNew then
    begin
      if Result = '' then
        Result := Records[I].GetAsJSON(AForDisplay)
      else
        Result := Result + ',' + Records[I].GetAsJSON(AForDisplay);
      Dec(LCount);
    end;
    Inc(I);
  end;
  Result := '[' + Result + ']';
end;

function TKRecords.GetAsXML(const AForDisplay: Boolean;
  const AFrom, AFor: Integer): string;
var
  I: Integer;
  LTo: Integer;
  LCount: Integer;
  LRecordCount: Integer;
  LTagName: string;
begin
  LRecordCount := RecordCountExceptNewAndDeleted;
  if AFor > 0 then
    LTo := Min(LRecordCount - 1, AFrom + AFor - 1)
  else
    LTo := LRecordCount - 1;

  // Loop so that a full page is returned even when there are deleted records.
  Result := '';
  LCount := LTo - AFrom + 1;
  I := AFrom;
  while LCount > 0 do
  begin
    if not Records[I].IsDeleted and not Records[I].IsNew then
    begin
      if Result = '' then
        Result := Records[I].GetAsXML(AForDisplay)
      else
        Result := Result + Records[I].GetAsXML(AForDisplay);
      Dec(LCount);
    end;
    Inc(I);
  end;
  LTagName := GetXMLTagName;
  Result := Format(XMLTagFormat,[LTagName, Result, LTagName]);
end;

function TKRecords.GetChildClass(const AName: string): TEFNodeClass;
begin
  Result := TKRecord;
end;

function TKRecords.GetRecord(const AValues: TEFNode): TKRecord;
begin
  Result := FindRecord(AValues);
  if not Assigned(Result) then
    raise EKError.CreateFmt(_('Record not found for predicate {%s}.'), [AValues.GetChildStrings(' and ', '=')]);
end;

function TKRecords.GetRecordCount: Integer;
begin
  Result := ChildCount;
end;

function TKRecords.GetRecordCountExceptNewAndDeleted: Integer;
var
  I: Integer;
begin
  Result := ChildCount;
  for I := 0 to RecordCount - 1 do
    if Records[I].IsDeleted or Records[I].IsNew then
      Dec(Result);
end;

function TKRecords.GetStore: TKStore;
begin
  Result := Parent as TKStore;
end;

function TKRecords.GetXMLTagName: string;
begin
  Result := Name;
end;

procedure TKRecords.Remove(const ARecord: TKRecord);
begin
  RemoveChild(ARecord);
end;

procedure TKRecords.SetKey(const AValue: TKKey);
begin
  FKey.Assign(AValue);
end;

function TKRecords.GetRecordByIndex(I: Integer): TKRecord;
begin
  Result := Children[I] as TKRecord;
end;

{ TKKey }

function TKKey.GetChildClass(const AName: string): TEFNodeClass;
begin
  Result := TKKeyField;
end;

function TKKey.GetField(I: Integer): TKKeyField;
begin
  Result := Children[I] as TKKeyField;
end;

function TKKey.GetFieldCount: Integer;
begin
  Result := ChildCount;
end;

function TKKey.GetFieldNames: TStringDynArray;
var
  I: Integer;
begin
  SetLength(Result, FieldCount);
  for I := 0 to FieldCount - 1 do
    Result[I] := Fields[I].Name;
end;

procedure TKKey.SetFieldNames(const AFieldNames: TStringDynArray);
var
  I: Integer;
begin
  Assert(Length(AFieldNames) > 0);

  Clear;
  for I := Low(AFieldNames) to High(AFieldNames) do
    AddChild(TKKeyField.Create(AFieldNames[I]));
end;

{ TKRecord }

procedure TKRecord.EnsureDetailStores;
begin
  if not Assigned(FDetailStores) then
    FDetailStores := TObjectList<TKStore>.Create;
end;

function TKRecord.ExpandExpression(const AExpression: string): string;
var
  I: Integer;
  Field: TKField;
begin
  Result := AExpression;
  for I := 0 to FieldCount - 1 do
  begin
    Field := Fields[I];
    Result := ReplaceText(Result, Format('{%s}',[Field.FieldName]), Field.AsString);
  end;
end;

function TKRecord.AddDetailStore(const AStore: TKStore): TKStore;
begin
  EnsureDetailStores;
  FDetailStores.Add(AStore);
  Result := AStore;
end;

function TKRecord.AddField(const AHeaderField: TKHeaderField): TKField;
begin
  Result := AddChild(AHeaderField.Name) as TKField;
  Result.HeaderField := AHeaderField;
end;

procedure TKRecord.AfterConstruction;
begin
  inherited;
  FState := rsNew;
end;

procedure TKRecord.Backup;
begin
  if not Assigned(FBackup) then
    FBackup := TEFNode.Create;
  FBackup.Assign(Self);
end;

function TKRecord.ChangesPending: Boolean;
var
  I: Integer;
begin
  Result := FState <> rsClean;
  if not Result then
  begin
    for I := 0 to DetailStoreCount - 1 do
    begin
      if DetailStores[I].ChangesPending then
      begin
        Result := True;
        Break;
      end;
    end;
  end;
end;

destructor TKRecord.Destroy;
begin
  FreeAndNil(FDetailStores);
  FreeAndNil(FBackup);
  inherited;
end;

function TKRecord.FieldByName(const AFieldName: string): TKField;
begin
  Result := FindField(AFieldName);
  if not Assigned(Result) then
    raise EKError.CreateFmt(_('Field %s not found.'), [AFieldName]);
end;

procedure TKRecord.FieldChanged(const AField: TKField; const AOldValue,
  ANewValue: Variant);
begin
  if Assigned(FOnFieldChange) then
    FOnFieldChange(AField, AOldValue, ANewValue);
end;

function TKRecord.FindField(const AFieldName: string): TKField;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FieldCount - 1 do
  begin
    if SameText(Fields[I].FieldName, AFieldName) then
    begin
      Result := Fields[I];
      Break;
    end;
  end;
end;

function TKRecord.GetAsJSON(const AForDisplay: Boolean): string;
var
  I: Integer;
  LJSON: string;
begin
  Result := '';
  for I := 0 to FieldCount - 1 do
  begin
    LJSON := Fields[I].GetAsJSON(AForDisplay);
    if LJSON <> '' then
    begin
      if Result <> '' then
        Result := Result + ',' + LJSON
      else
        Result := LJSON;
    end;
  end;
  Result := '{' + Result + '}';
end;

function TKRecord.GetAsXML(const AForDisplay: Boolean): string;
var
  I: Integer;
  LXML, LTagName: string;
  LDetailStore: TKStore;
begin
  Result := '';
  for I := 0 to FieldCount - 1 do
  begin
    LXML := Fields[I].GetAsXML(AForDisplay);
    if LXML <> '' then
      Result := Result + sLineBreak + LXML
    else
      Result := LXML;
  end;
  for I := 0 to DetailStoreCount -1 do
  begin
    LDetailStore := DetailStores[I];
    Result := Result + LDetailStore.GetAsXML(AForDisplay);
  end;
  LTagName := GetXMLTagName;
  Result := Format(XMLTagFormat,[LTagName, Result, LTagName]);
end;

function TKRecord.GetChildClass(const AName: string): TEFNodeClass;
begin
  Result := TKField;
end;

function TKRecord.GetDetailsStore(I: Integer): TKStore;
begin
  Assert(Assigned(FDetailStores));

  Result := FDetailStores[I];
end;

function TKRecord.GetDetailStoreCount: Integer;
begin
  if Assigned(FDetailStores) then
    Result := FDetailStores.Count
  else
    Result := 0;
end;

function TKRecord.GetField(I: Integer): TKField;
begin
  Result := Children[I] as TKField;
end;

function TKRecord.GetFieldCount: Integer;
begin
  Result := ChildCount;
end;

function TKRecord.GetIsDeleted: Boolean;
begin
  Result := FState = rsDeleted;
end;

function TKRecord.GetIsNew: Boolean;
begin
  Result := FState = rsNew;
end;

function TKRecord.GetKey: TKKey;
begin
  Result := Records.Key;
end;

function TKRecord.GetRecords: TKRecords;
begin
  Result := Parent as TKRecords;
end;

function TKRecord.GetStore: TKStore;
begin
  Result := Records.Store;
end;

function TKRecord.GetXMLTagName: string;
begin
  Result := Name;
end;

procedure TKRecord.MarkAsClean;
begin
  FState := rsClean;
end;

procedure TKRecord.MarkAsDeleted;
begin
  if FState = rsNew then
    FState := rsClean
  else
    FState := rsDeleted;
end;

procedure TKRecord.MarkAsModified;
begin
  if not (FState in [rsNew, rsDeleted]) then
    FState := rsDirty;
end;

function TKRecord.MatchesValues(const AValues: TEFNode): Boolean;
var
  I: Integer;
begin
  Assert(Assigned(AValues));

  Result := True;
  for I := 0 to AValues.ChildCount - 1 do
  begin
    if not FieldByName(AValues[I].Name).EqualsValue(AValues[I].Value) then
    begin
      Result := False;
      Break;
    end;
  end;
end;

procedure TKRecord.ReadFromFields(const AFields: TFields);
var
  I: Integer;
  LDBField: TField;
  LFieldName: string;
  LValue: string;
begin
  Assert(Assigned(AFields));

  Backup;
  try
    for I := 0 to FieldCount - 1 do
    begin
      LDBField := AFields.FindField(TranslateFieldName(Fields[I].FieldName));
      if Assigned(LDBField) then
      begin
        Fields[I].AssignFieldValue(LDBField);
        Fields[I].MarkAsUnmodified;
      end
      // Field not found - might be a concatenation.
      else if Fields[I].FieldName.Contains(TKConfig.Instance.MultiFieldSeparator) then
      begin
        LValue := '';
        for LFieldName in Split(Fields[I].FieldName, TKConfig.Instance.MultiFieldSeparator) do
        begin
          if LValue = '' then
            LValue := AFields.FieldByName(TranslateFieldName(LFieldName)).AsString
          else
            LValue := LValue + TKConfig.Instance.MultiFieldSeparator + AFields.FieldByName(TranslateFieldName(LFieldName)).AsString;
        end;
        Fields[I].AsString := LValue;
        Fields[I].MarkAsUnmodified;
      end;
      // Still not found - must be a reference.
    end;
    FState := rsClean;
  except
    Restore;
    raise;
  end;
end;

procedure TKRecord.ReadFromNode(const ANode: TEFNode);
var
  I: Integer;
begin
  Assert(Assigned(ANode));

  Backup;
  try
    for I := 0 to FieldCount - 1 do
      Fields[I].AssignValue(ANode.FindNode(Fields[I].FieldName));
    InternalAfterReadFromNode;
    if FState = rsClean then
      FState := rsDirty;
  except
    Store.DisableChangeNotifications;
    try
      Restore;
    finally
      Store.EnableChangeNotifications;
    end;
    raise;
  end;
end;

procedure TKRecord.InternalAfterReadFromNode;
begin
end;

procedure TKRecord.Restore;
begin
  Assert(Assigned(FBackup));

  Assign(FBackup);
end;

function TKRecord.TranslateFieldName(const AFieldName: string): string;
begin
  Result := AFieldName;
end;

{ TKField }

function TKField.GetJSONName: string;
begin
  Result := FieldName;
end;

function TKField.GetAsJSON(const AForDisplay: Boolean): string;
begin
  if DataType.SupportsJSON then
  begin
    Result := QuoteJSONStr(GetJSONName) + ':' + GetAsJSONValue(AForDisplay);
    if AForDisplay then
    begin
      Result := ReplaceStr(Result, #13#10, '<br/>');
      Result := ReplaceStr(Result, #10, '<br/>');
      Result := ReplaceStr(Result, #13, '<br/>');
    end;
  end
  else
    Result := '';
end;

function TKField.GetAsJSONValue(const AForDisplay: Boolean; const AQuote: Boolean;
  const AEmptyNulls: Boolean): string;
begin
  Result := DataType.NodeToJSONValue(AForDisplay, Self, TKConfig.JSFormatSettings, AQuote, AEmptyNulls);
end;

function TKField.GetAsXML(const AForDisplay: Boolean): string;
begin
  if DataType.SupportsXML then
    Result := GetAsXMLValue(AForDisplay)
  else
    Result := '';
end;

function TKField.GetAsXMLValue(const AForDisplay: Boolean;
  const AEmptyNulls: Boolean = False): string;
begin
  Result := DataType.NodeToXMLValue(AForDisplay, Self, TKConfig.Instance.UserFormatSettings, AEmptyNulls);
end;

function TKField.GetDataType: TEFDataType;
begin
  Assert(Assigned(FHeaderField));

  Result := FHeaderField.DataType;
end;

function TKField.GetFieldName: string;
begin
  Assert(Assigned(FHeaderField));

  Result := FHeaderField.FieldName;
end;

function TKField.GetName: string;
begin
  // Needed for when the field is passed to clients expecting a plain node.
  Result := FieldName;
end;

function TKField.GetParentRecord: TKRecord;
begin
  Result := Parent as TKRecord;
end;

function TKField.GetXMLTagName: string;
begin
  Result := FieldName;
end;

procedure TKField.MarkAsUnmodified;
begin
  FIsModified := False;
end;

procedure TKField.SetToNull;
begin
  if not VarIsNull(Value) then
  begin
    FIsModified := True;
    if Assigned(ParentRecord) then
      ParentRecord.MarkAsModified;
  end;
  inherited;
end;

procedure TKField.SetValue(const AValue: Variant);
begin
  if not CompareValues(AValue, Value) then
  begin
    FIsModified := True;
    if ParentRecord <> nil then
      ParentRecord.MarkAsModified;
  end;
  inherited;
end;

procedure TKField.ValueChanged(const AOldValue, ANewValue: Variant);
begin
  inherited;
  if Assigned(ParentRecord) and Assigned(ParentRecord.Store) and ParentRecord.Store.ChangeNotificationsEnabled then
    ParentRecord.FieldChanged(Self, AOldValue, ANewValue);
end;

{ TKHeader }

function TKHeader.AddField(const AFieldName: string): TKHeaderField;
begin
  Result := AddChild(AFieldName) as TKHeaderField;
end;

procedure TKHeader.Apply(const ARecord: TKRecord);
var
  I: Integer;
begin
  Assert(Assigned(ARecord));

  ARecord.ClearChildren;
  for I := 0 to FieldCount - 1 do
    ARecord.AddField(Fields[I]);
end;

function TKHeader.FieldByName(const AFieldName: string): TKHeaderField;
begin
  Result := FindField(AFieldName);
  if not Assigned(Result) then
    raise EKError.CreateFmt(_('Field %s not found.'), [AFieldName]);
end;

function TKHeader.FindField(const AFieldName: string): TKHeaderField;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FieldCount - 1 do
  begin
    if SameText(Fields[I].FieldName, AFieldName) then
    begin
      Result := Fields[I];
      break;
    end;
  end;
end;

function TKHeader.GetChildClass(const AName: string): TEFNodeClass;
begin
  Result := TKHeaderField;
end;

function TKHeader.GetField(I: Integer): TKHeaderField;
begin
  Result := Children[I] as TKHeaderField;
end;

function TKHeader.GetFieldCount: Integer;
begin
  Result := ChildCount;
end;

procedure TKHeader.GetFieldNames(List: TStrings);
var
  I: integer;
begin
  List.Clear;
  for I := 0 to FieldCount - 1 do
    List.Add(Fields[I].FieldName);
end;

{ TKHeaderField }

function TKHeaderField.GetFieldName: string;
begin
  Result := Name;
end;

{ TKKeyField }

function TKKeyField.GetFieldName: string;
begin
  Result := Name;
end;

function TKKeyField.GetKey: TKKey;
begin
  Result := Parent as TKKey;
end;

end.
