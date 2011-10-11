  unit Kitto.Store;

interface

uses
  SysUtils, Types, DB,
  EF.Tree, EF.DB;

type
  TKKeyField = class(TEFNode)
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
  end;

  TKRecords = class;
  TKRecord = class;

  TKField = class(TEFNode)
  private
    class var JSFormatSettings: TFormatSettings;
    class constructor Create;
    function GetFieldName: string;
    function GetParentRecord: TKRecord;
    function GetAsJSONValue: string;
    procedure SetAsJSONValue(const AValue: string);
  public
    property AsJSONValue: string read GetAsJSONValue write SetAsJSONValue;
    property ParentRecord: TKRecord read GetParentRecord;

    function GetAsJSON: string;

    property FieldName: string read GetFieldName;
  end;

  TKRecord = class(TEFNode)
  private
    function GetRecords: TKRecords;
    function GetKey: TKKey;
    function GetField(I: Integer): TKField;
    function GetFieldCount: Integer;
  protected
    function GetChildClass(const AName: string): TEFNodeClass; override;
  public
    property Records: TKRecords read GetRecords;
    property Key: TKKey read GetKey;
    property Fields[I: Integer]: TKField read GetField; default;
    property FieldCount: Integer read GetFieldCount;
    function FindField(const AFieldName: string): TKField;
    function FieldByName(const AFieldName: string): TKField;
    function MatchesKeyValues(const AValues: TStringDynArray): Boolean;

    ///	<summary>
    ///	  Clears its fields and reads fields and values from the current
    ///	  dataset record.
    ///	</summary>
    procedure ReadFromFields(const AFields: TFields);

    function GetAsJSON: string;
  end;

  TKStore = class;

  TKRecords = class(TEFNode)
  private
    FKey: TKKey;
    function GetRecordCount: Integer;
    function GetRecord(I: Integer): TKRecord;
    procedure SetKey(const AValue: TKKey);
    function GetStore: TKStore;
  protected
    function GetChildClass(const AName: string): TEFNodeClass; override;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
  public
    property Store: TKStore read GetStore;

    property Key: TKKey read FKey write SetKey;
    function FindRecordByKey(const AValues: TStringDynArray): TKRecord;
    function GetRecordByKey(const AValues: TStringDynArray): TKRecord;
    property Records[I: Integer]: TKRecord read GetRecord; default;
    property RecordCount: Integer read GetRecordCount;

    ///	<summary>
    ///	  Appends a record at the end of the store. Returns a reference to the
    ///	  record just appended.
    ///	</summary>
    ///	<remarks>
    ///	  This method is meant to be used during data loading.
    ///	</remarks>
    function AppendRecord(const ARecord: TKRecord): TKRecord;

    function GetAsJSON(const AFrom: Integer = 0; const AFor: Integer = 0): string;
  end;

  TKHeader = class(TEFNode)
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
  end;

  TKStore = class(TEFTree)
  private
    FHeader: TKHeader;
    function GetRecords: TKRecords;
    function GetKey: TKKey;
    procedure SetKey(const AValue: TKKey);
    function GetRecordCount: Integer;
    function GetHeader: TKHeader;
  protected
    function GetChildClass(const AName: string): TEFNodeClass; override;
  public
    destructor Destroy; override;
  public
    property Key: TKKey read GetKey write SetKey;
    property Header: TKHeader read GetHeader;
    property Records: TKRecords read GetRecords;
    property RecordCount: Integer read GetRecordCount;

    procedure Load(const ADBConnection: TEFDBConnection;
      const ACommandText: string; const AAppend: Boolean = False); overload;
    procedure Load(const ADataSet: TDataSet; const AAppend: Boolean = False); overload;

    function GetAsJSON(const AFrom: Integer = -1; const AFor: Integer = -1): string;
  end;

implementation

uses
  Math, FmtBcd,
  EF.StrUtils, EF.Localization, EF.DB.Utils,
  Kitto.Types, Kitto.Ext.Session;

{ TKStore }

destructor TKStore.Destroy;
begin
  FreeAndNil(FHeader);
  inherited;
end;

function TKStore.GetAsJSON(const AFrom: Integer; const AFor: Integer): string;
begin
  Result := Records.GetAsJSON(AFrom, AFor);
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

function TKStore.GetRecordCount: Integer;
begin
  Result := Records.RecordCount;
end;

function TKStore.GetRecords: TKRecords;
begin
  Result := FindChild('Records', True) as TKRecords;
end;

procedure TKStore.Load(const ADataSet: TDataSet; const AAppend: Boolean);
var
  LRecord: TKRecord;
begin
  Assert(Assigned(ADataSet));

{ TODO : why Clear clears the Key as well? }
  if not AAppend then
    Records.ClearChildren; // Doesn't clear the Key.
  while not ADataSet.Eof do
  begin
    LRecord := Records.AppendRecord(TKRecord.Create);
    Header.Apply(LRecord);
    Assert(LRecord.FieldCount = Header.ChildCount);
    LRecord.ReadFromFields(ADataSet.Fields);
    ADataSet.Next;
  end;
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
      Load(LDBQuery.DataSet, AAppend);
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

function TKRecords.AppendRecord(const ARecord: TKRecord): TKRecord;
begin
  Assert(Assigned(ARecord));

  Result := TKRecord(AddChild(ARecord));
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

function TKRecords.FindRecordByKey(const AValues: TStringDynArray): TKRecord;
var
  I: Integer;
begin
  Assert(Length(AValues) > 0);
  Assert(Length(AValues) = Key.FieldCount);

  Result := nil;
  for I := 0 to RecordCount - 1 do
  begin
    if Records[I].MatchesKeyValues(AValues) then
    begin
      Result := Records[I];
      Break;
    end;
  end;
end;

function TKRecords.GetAsJSON(const AFrom: Integer; const AFor: Integer): string;
var
  I: Integer;
  LTo: Integer;
begin
  if AFor > 0 then
    LTo := Min(RecordCount - 1, AFrom + AFor - 1)
  else
    LTo := RecordCount - 1;

  Result := '[';
  for I := AFrom to LTo do
  begin
    Result := Result + Records[I].GetAsJSON;
    if I < RecordCount - 1 then
      Result := Result + ',';
  end;
  Result := Result + ']';
end;

function TKRecords.GetChildClass(const AName: string): TEFNodeClass;
begin
  Result := TKRecord;
end;

function TKRecords.GetRecordByKey(const AValues: TStringDynArray): TKRecord;
begin
  Result := FindRecordByKey(AValues);
  if not Assigned(Result) then
    raise EKError.CreateFmt(_('Record not found for key %s.'), [AValues]);
end;

function TKRecords.GetRecordCount: Integer;
begin
  Result := ChildCount;
end;

function TKRecords.GetStore: TKStore;
begin
  Result := Parent as TKStore;
end;

procedure TKRecords.SetKey(const AValue: TKKey);
begin
  FKey.Assign(AValue);
end;

function TKRecords.GetRecord(I: Integer): TKRecord;
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

function TKRecord.FieldByName(const AFieldName: string): TKField;
begin
  Result := FindField(AFieldName);
  if not Assigned(Result) then
    raise EKError.CreateFmt(_('Field %s not found.'), [AFieldName]);
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

function TKRecord.GetAsJSON: string;
var
  I: Integer;
begin
  Result := '{';
  for I := 0 to FieldCount - 1 do
  begin
    Result := Result + Fields[I].GetAsJSON;
    if I < FieldCount - 1 then
      Result := Result + ',';
  end;
  Result := Result + '}';
end;

function TKRecord.GetChildClass(const AName: string): TEFNodeClass;
begin
  Result := TKField;
end;

function TKRecord.GetField(I: Integer): TKField;
begin
  Result := Children[I] as TKField;
end;

function TKRecord.GetFieldCount: Integer;
begin
  Result := ChildCount;
end;

function TKRecord.GetKey: TKKey;
begin
  Result := Records.Key;
end;

function TKRecord.GetRecords: TKRecords;
begin
  Result := Parent as TKRecords;
end;

function TKRecord.MatchesKeyValues(const AValues: TStringDynArray): Boolean;
var
  I: Integer;
begin
  Assert(Length(AValues) > 0);
  Assert(Length(AValues) = Key.FieldCount);

  Result := True;
  for I := 0 to Key.FieldCount - 1 do
  begin
    if FieldByName(Key[I].Name).AsString <> AValues[I] then
    begin
      Result := False;
      Break;
    end;
  end;
end;

procedure TKRecord.ReadFromFields(const AFields: TFields);
var
  I: Integer;
begin
  Assert(Assigned(AFields));
  Assert(AFields.Count >= FieldCount);

  for I := 0 to FieldCount - 1 do
    Fields[I].AssignFieldValue(AFields[I]);
end;

{ TKField }

class constructor TKField.Create;
begin
  JSFormatSettings := TFormatSettings.Create;
  JSFormatSettings.DecimalSeparator := '.';
end;

function TKField.GetAsJSON: string;
begin
  Result := '"' + FieldName + '":' + AsJSONValue;
end;

function TKField.GetAsJSONValue: string;
begin
  if IsNull then
    Result := 'null'
  else
  begin
    case DataType of
      edtUnknown, edtString, edtObject, edtInteger: Result := AsString;
      edtDate: Result := DateToStr(AsDate, Session.FormatSettings);
      edtTime: Result := TimeToStr(AsTime, Session.FormatSettings);
      edtDateTime: Result := DateTimeToStr(AsDateTime, Session.FormatSettings);
      edtBoolean: Result := BoolToStr(AsBoolean, True);
      edtCurrency, edtFloat, edtDecimal: Result := FloatToStr(AsFloat, JSFormatSettings);
    end;
  end;
  Result := '"' + Result + '"';
end;

function TKField.GetFieldName: string;
begin
  Result := ParentRecord.Records.Store.Header.Children[Index].Name;
end;

function TKField.GetParentRecord: TKRecord;
begin
  Result := Parent as TKRecord;
end;

procedure TKField.SetAsJSONValue(const AValue: string);
var
  LDateTime: TDateTime;
  LBoolean: Boolean;
  LDouble: Double;
  LInteger: Integer;
begin
  if DataType = edtUnknown then
  begin
    if TryStrToDateTime(AValue, LDateTime, Session.FormatSettings) then
      AsDateTime := LDateTime
    else if TryStrToDate(AValue, LDateTime, Session.FormatSettings) then
      AsDate := LDateTime
    else if TryStrToTime(AValue, LDateTime, Session.FormatSettings) then
      AsTime := LDateTime
    else if TryStrToBool(AValue, LBoolean) then
      AsBoolean := LBoolean
    else if TryStrToInt(AValue, LInteger) then
      AsInteger := LInteger
    else if TryStrToFloat(AValue, LDouble, Session.FormatSettings) then
      AsFloat := LDouble
    else
      AsString := AValue;
  end
  else
  begin
    case DataType of
      edtString: AsString := AValue;
      edtInteger: AsInteger := StrToInt(AValue);
      edtDate: AsDate := StrToDate(AValue, Session.FormatSettings);
      edtTime: AsTime := StrToTime(AValue, Session.FormatSettings);
      edtDateTime: AsDateTime := StrToDateTime(AValue, Session.FormatSettings);
      edtBoolean: AsBoolean := StrToBool(AValue);
      edtCurrency: AsCurrency := StrToCurr(AValue, Session.FormatSettings);
      edtFloat: AsFloat := StrToFloat(AValue, Session.FormatSettings);
      edtDecimal: AsDecimal := StrToBcd(AValue, Session.FormatSettings);
      edtObject: raise EKError.CreateFmt('SetAsJSONValue: Unsupported value %s for data type %s.',
        [AValue, EFDataTypeToString(DataType)]);
    end;
  end;
end;

{ TKHeader }

procedure TKHeader.Apply(const ARecord: TKRecord);
var
  I: Integer;
begin
  Assert(Assigned(ARecord));

  ARecord.ClearChildren;
  for I := 0 to ChildCount - 1 do
    ARecord.AddChild('').DataType := Children[I].DataType;
end;

end.
