unit EF.DB.Utils;

{$I EF.Defines.inc}

interface

uses
  Types, Classes, DB,
  EF.Macros, EF.Classes, EF.Tree, EF.Data, EF.DB;

type
  {
    Slightly enhanced TParams. It can read its valued from a TFields object,
    build itself from a list of param names, merge with another instance,
    and so on. It is used instead of TParams wherever appropriate.
  }
  TEFParamsEx = class(TParams)
  public
    {
      Creates a param for each field in AFields and fills it with the field's
      value.
    }
    procedure ReadFromFields(const AFields: TFields);
    {
      Returns a list of comma-delimited data item names.
    }
    procedure AddItemNames(const AItemNames: TStrings);
    {
      Adds to the current object all params in ASource that are not existing
      yet, with the values in ASource. Existing values are not overwritten
      unless AOverwrite is True.
    }
    procedure Merge(const ASource: TParams; const AOverwrite: Boolean = False);
  end;

{
  Converts a value of type TEFDataType into the corresponding TFieldType.
}
function EFDataTypeToFieldType(const AEFDataType: TEFDataType): TFieldType;
{
  Converts a value of type TFieldType into the corresponding TEFDataType.
}
function FieldTypeToEFDataType(const AFieldType: TFieldType): TEFDataType;

{
  Copies the value from ASource to ADestination, taking ASource.DataType into
  account.
}
procedure AssignEFDataItemValueToDBParam(const ASource: TEFDataItem;
  const ADestination: TParam);

{
  Copies the value from ASource to ADestination, taking ASource.DataType into
  account.
}
procedure AssignDBParamValueToEFDataItem(const ASource: TParam;
  const ADestination: TEFDataItem);

{
  Copies the value from ASource to ADestination, taking ASource.DataType into
  account.
}
procedure AssignEFDataItemValueToDBField(const ASource: TEFDataItem;
  const ADestination: TField);

{
  Copies the value from ASource to ADestination, taking ASource.DataType into
  account.
}
procedure AssignDBFieldValueToEFDataItem(const ASource: TField;
  const ADestination: TEFDataItem);

{
  Copies the name and value from ASource to ADestination, taking
  ASource.DataType into account.
}
procedure AssignEFDataItemToDBParam(const ASource: TEFDataItem;
  const ADestination: TParam);

type
  {
    Base class for field maps. A field map is an associative array that
    map a set of TField references to another set. It is used to copy records
    from a source dataset to a destination dataset without incurring the
    performance loss of repeatedly calling FieldByName.
  }
  TEFCustomFieldMap = class
  private
    FSourceFields: array of TField;
    FDestinationFields: array of TField;
    FFieldCount: Integer;
    function GetDestinationField(const AIndex: Integer): TField;
    function GetSourceField(const AIndex: Integer): TField;
  protected
    procedure SetFieldCount(const AValue: Integer);
  public
    property FieldCount: Integer read FFieldCount;
    property SourceFields[const AIndex: Integer]: TField read GetSourceField;
    property DestinationFields[const AIndex: Integer]: TField read GetDestinationField;
  end;

  {
    Builds the field map upon construction, using FieldByName to build pairs of
    fields with the same name.
  }
  TEFDataSetFieldMap = class(TEFCustomFieldMap)
  public
    constructor Create(const ASourceDataSet, ADestinationDataSet: TDataSet);
  end;

  {
    Uses the field map passed to the constructor in form of arrays of field
    names. An overloaded constructor allows to pass the arrays as comma-separated
    lists of field names.
  }
  TEFArrayFieldMap = class(TEFCustomFieldMap)
  public
    constructor Create(const ASourceDataSet, ADestinationDataSet: TDataSet;
      const ASourceFieldNames, ADestinationFieldNames: array of string); overload;
    constructor Create(const ASourceDataSet, ADestinationDataSet: TDataSet;
      const ASourceFieldNames, ADestinationFieldNames: string); overload;
  end;

type
  {
    Used for events during operations on a pair of datasets.
  }
  TEFDoubleDataSetNotifyEvent = procedure(const ASourceDataSet,
    ADestinationDataSet: TDataSet) of object;

{
  Appends to ADestinationDataSet all records found in ASourceDataSet; only fields
  defined in both datasets are copied. Returns the number of records copied.
  ABeforePost, if specified, is called before posting each record written to
  ADestinationDataSet.
  Set IgnoreErrors to True to ignore instances of EDatabaseError during the
  copy operation.
  Optionally pass a field map to set the rules under which field values are
  copied. Otherwise, one will be manufactured which copies field values by name.
}
function AppendToDataSet(const ASourceDataSet, ADestinationDataSet: TDataSet;
  const ABeforePost: TEFDoubleDataSetNotifyEvent = nil;
  const IgnoreErrors: Boolean = False;
  const AFieldMap: TEFCustomFieldMap = nil): Longint;

{
  Appends to ADestinationDataSet the current record in ASourceDataSet; only fields
  defined in both datasets are copied. ABeforePost, if specified, is called
  before posting the record written to ADestinationDataSet.
  Optionally pass a field map to set the rules under which field values are
  copied. Otherwise, fields are copied based on name. It is strongly suggested
  to specify a field map if many records need to be copied, for performance
  reasons.
}
procedure AppendRecordToDataSet(const ASourceDataSet, ADestinationDataSet: TDataSet;
  const ABeforePost: TEFDoubleDataSetNotifyEvent = nil;
  const AFieldMap: TEFCustomFieldMap = nil);

{
  Copies the field values in the current record of ASourceDataSet to the
  current record of ADestinationDataSet, then posts. Only fields
  defined in both datasets are copied. ABeforePost, if specified, is called
  before posting the record written to ADestinationDataSet.
  Optionally pass a field map to set the rules under which field values are
  copied. Otherwise, fields are copied based on name. It is strongly suggested
  to specify a field map if many records need to be copied, for performance
  reasons.
}
procedure CopyRecordToDataSet(const ASourceDataSet, ADestinationDataSet: TDataSet;
  const ABeforePost: TEFDoubleDataSetNotifyEvent = nil;
  const AFieldMap: TEFCustomFieldMap = nil);

type
  {
    Holds a set of SourceName/DestinationName pairs, used for name translations.
    It is much like a two-way (meaning it is keyed on both SourceName and
    DestinationName) associative array of strings.
  }
  TEFNameMap = class
  private
    FSourceNames: TStringList;
    FDestinationNames: TStringList;
    function GetAsString: string;
    procedure SetAsString(const AValue: string);
    function GetDestinationName(const ASourceName: string): string;
    function GetSourceName(const ADestinationName: string): string;
  public
    {
      Optionally pass an initial string. See the AsString property for details.
    }
    constructor Create(const AAsString: string = '');
    destructor Destroy; override;
    {
      Sets or returns the contents of the whole map in string format. The string
      has items in the form <SourceName=DestinationName>.
      Items are separated by the comma character (see TStrings.CommaText for
      further information).

      Warning: since the internal structures are sorted, setting the map as a
      string and reading it back does not guarantee the string is unchanged.
      Setting this property re-sorts the internal structures, so don't do it
      unless required.
    }
    property AsString: string read GetAsString write SetAsString;
    {
      Efficient access to a SourceName by a DestinationName.
    }
    property SourceNames[const ADestinationName: string]: string read GetSourceName;
    {
      Efficient access to a DestinationName by a SourceName.
    }
    property DestinationNames[const ASourceName: string]: string read GetDestinationName;
    {
      Substitutes each item in AStrings with the DestinationName in this
      name map corresponding to the SourceName that is the item.
    }
    procedure ApplyToStrings(const AStrings: TStrings);
    {
      Changes each param name in ADBParams that happens to be a SourceName with
      the corresponding DestinationName in this name map, if found.
    }
    procedure ApplyToDBParamNames(const ADBParams: TParams);
  end;

  {
    Base class for param maps. A param map is an associative array that
    map a set of source TField references to a set of params.
    It is used to copy records from a source dataset to a destination
    parameterized query or other TParams-based object, without incurring the
    performance loss of repeatedly calling ParamByName.
  }
  TEFCustomParamMap = class
  private
    FSourceFields: array of TField;
    FDestinationParams: array of TParam;
    FParamCount: Integer;
    function GetDestinationParam(const AIndex: Integer): TParam;
    function GetSourceField(const AIndex: Integer): TField;
  public
    property ParamCount: Integer read FParamCount;
    property SourceFields[const AIndex: Integer]: TField read GetSourceField;
    property DestinationParams[const AIndex: Integer]: TParam read GetDestinationParam;
    {
      Returns a list of comma-delimited destination param names.
    }
    procedure AddDestinationParamNames(const AParamNames: TStrings);
  end;

  {
    Builds the field map upon construction, using FieldByName and ParamByName
    to build pairs of fields/params with the same name. If ANameMap is
    specified, it is used to help build the pairs.
  }
  TEFDataSetParamMap = class(TEFCustomParamMap)
  public
    {
      Optionally pass a name map (in which SourceNames contains the field names
      and DestinationNames the param names), which will be used to override
      the default name equality matches.
    }
    constructor Create(const ASourceDataSet: TDataSet;
      const ADestinationParams: TParams; const ANameMap: TEFNameMap = nil);
  end;

{
  Copies all values from ASourceFields to ADestinationParams, optionally using
  the supplied param map (otherwise the assignments are done by name).
  All needed params must exist in advance in ADestinationParams.
}
procedure CopyFieldsToParams(const ASourceFields: TFields;
  const ADestinationParams: TParams; const AParamMap: TEFCustomParamMap = nil);

type
  {
    Copies the values of one or more fields (SourceFieldNames)
    from the current record of SourceDataSet to the corresponding fields
    (DestinationFieldNames) of a DestinationDataSet.
  }
  TEFFieldValueTransfer = class
  private
    FOnlyIfEmpty: boolean;
    FSourceDataSet: TDataset;
    FDestinationDataSet: TDataset;
    FSourceFieldNames: TStrings;
    FDestinationFieldNames: TStrings;
    procedure SetDestinationDataSet(const Value: TDataset);
    procedure SetOnlyIfEmpty(const Value: boolean);
    procedure SetSourceDataSet(const Value: TDataset);
  public
    constructor Create();
    destructor Destroy; override;
    procedure Execute;
    property SourceDataSet: TDataset read FSourceDataSet write SetSourceDataSet;
    property SourceFieldNames: TStrings read FSourceFieldNames;
    property DestinationDataSet: TDataset read FDestinationDataSet write SetDestinationDataSet;
    property DestinationFieldNames: TStrings read FDestinationFieldNames;
    {
      Determine whether to copy the values only if the destination fields are
      empty (True) or not (False).
    }
    property OnlyIfEmpty: Boolean read FOnlyIfEmpty write SetOnlyIfEmpty;
    {
      Helper routine that creates an instance, executes it and frees it.
    }
    class procedure Transfer(const ASourceDataSet: TDataSet;
      const ASourceFieldNameList: string;
      const ADestinationDataSet: TDataSet;
      const ADestinationFieldNameList: string;
      const AOnlyIfEmpty: Boolean = False);
  end;

{
  Returns the value of the first column of the first record of the cursor
  returned by ASQLStatement. It is advised to pass a single-column singleton
  SQL statement for efficiency reasons. May return an empty or unassigned
  Variant.
}
function GetSingletonValue(const ADBConnection: IEFDBConnection;
  const ASQLStatement: string): Variant;

{
  Adds to AStrings one item for each record of the cursor returned by
  ASQLStatement. The item is the string representation of the first
  field. Returns the number of added items.
}
function GetStringsFromDB(const ADBConnection: IEFDBConnection;
  const ASQLStatement: string; const AStrings: TStrings): Integer;

{
  Builds ADestinationDataSet.FieldDefs according to the structure defined
  by ASourceDataSet.Fields or, failing that, ASourceDataSet.FieldDefs.
  Pass nil in ASourceDataSet to cause only the removal of all Fields and
  FieldDefs in ADestinationDataSet.
}
procedure CopyDataSetStructure(const ASourceDataSet, ADestinationDataSet: TDataSet);

{
  Sets the values of one or more fields in all records of ADataSet.
  Preserves the current record if the dataset supports bookmarks.
  See also TDataSet.FieldValues.
}
procedure SetDataSetFieldValues(const ADataSet: TDataSet; const AFieldNames: string;
  const AFieldValues: Variant);

{
  Clears ADestinationParams and creates a nEF input param for each field in
  ASourceFields, with the same name and type and without value.
  Doesn't create params for unsupported field types, such as ftDataSet.
}
procedure CopyFieldStructureToParams(const ASourceFields: TFields;
  const ADestinationParams: TParams);

{
  Strangely enough, TParam has an AssignParam method that will copy
  everything from a source param, and an AssignFieldValue that will
  copy the value from a source field, but no AssignParamValue method.
  This function fills the gap. It is equivalent to calling
  ADestinationParam.AssignParam(ASourceParam), but without copying
  the name.
}
procedure AssignParamValue(const ADestinationParam, ASourceParam: TParam);

{
  Returns 0 for numeric fields, '' for strings, and so on.
}
function GetEmptyFieldValue(const AFieldType: TFieldType): Variant;

{
  Returns the names of all fields in ADataSet as a separted list. The
  separator character is specified in ADelimiter.
}
function GetDataSetFieldNames(const ADataSet: TDataSet;
  const ADelimiter: Char = ';'): string;

type
  TSwapDirection = (sdUp, sdDown);

{
  Swaps the values from the current record of ADataSet with the values
  of the same fields from the previous (if ADirection is sdUp) or next
  (if ADirection is sdDown) record. Pass the names of the fields that need
  their values swapped in a semicolon-separated list in AFieldNames. Pass '*'
  to swap entire records.

  This function is useful to implement a feature of manual reordering of the
  records in a dataset, especially when AFieldNames is '*'.
}
procedure SwapFieldValues(const ADataSet: TDataSet; const AFieldNames: string;
  const ADirection: TSwapDirection);

{
  Returns True if ADataSet's current record is the first record. This is
  different than ADataSet.Bof, which is only True when the dataset's current
  record pointer is *before* the first record.
  This function only supports sequenced datasets (those for which IsSequenced
  returns True), such as the ClientDataSet.
}
function IsDataSetOnFirstRecord(const ADataSet: TDataSet): Boolean;

{
  Returns True if ADataSet's current record is the last record. This is
  different than ADataSet.Eof, which is only True when the dataset's current
  record pointer is *after* the last record.
  This function only supports sequenced datasets (those for which IsSequenced
  returns True), such as the ClientDataSet.
}
function IsDataSetOnLastRecord(const ADataSet: TDataSet): Boolean;

{
  Sets a database field's value even if the field is read-only.
  Restores the read-only state before exiting.
}
procedure SetDBFieldValue(const AField: TField; const AValue: Variant);

{
  For each record in ADataSet adds a row to AStrings. The row is formatted as:
  <FieldName>: <AsString> - <FieldName>: <AsString> - ...
  This function is meant as a debugging aid, or for logging a dataset's
  contents.
}
procedure DataSetToStrings(const ADataSet: TDataSet; const AStrings: TStrings);

{
  Returns a string that represents the current record in ADataSet.
  The string is formatted as:
  <FieldName>: <AsString> - <FieldName>: <AsString> - ...
  This function is meant as a debugging aid, or for logging a dataset record's
  contents.
}
function DataSetRecordToString(const ADataSet: TDataSet): string;

type
  {
    A macro expander that expands all the field values in a dataset's current
    record. Each macro in this format:

    %<NameSpace>:<FieldName>%

    Is expanded to the value of the field <FieldName> of the dataset
    this expander holds a reference to.
    <NameSpace> is a value set upon creation. If it is '', then no ':'
    separator is required in the macros. The need for a name space stems from
    the fact that you can have multiple macro expanders of this kind active at
    the same time, each linked to a different dataset, and use the
    namespace string to partition them.

    Note: Macros in values retrieved through macro expansion are not
    themselves expanded. This is a known current limitation.

    Note: This macro expander is not registered by default, as it needs a
    reference to an external object to work. So, applications will create and
    use this class autonomously (by querying it directly or adding it to an
    expansion engine, without registering it) as required.
  }
  TEFDataSetMacroExpander = class(TEFMacroExpander)
  private
    FDataSet: TDataSet;
    FNameSpace: string;
  protected
    function InternalExpand(const AString: string): string; override;
  public
    constructor Create(const ADataSet: TDataSet;
      const ANameSpace: string); reintroduce;
  end;

type
  {
    Type for items in RecordToString's AOptions argument.
  }
  TEFRecordToStringOption = (rsIncludeInvisibleFields);

  {
    A set of TEFRecordToStringOption options.
  }
  TEFRecordToStringOptions = set of TEFRecordToStringOption;

{
  Returns a string with all the data values of ADataSet's current record.
  You can specify which field names to include as a comma-separated list (*
  means include all fields), in which case the fields in the resulting string
  appear in the specified order (otherwise the order is defined by ADataSet).
  In addition to that, you can also exclude particular fields, by passing their
  names in AExcludeFieldNames as a comma-separated list.
  Fields with Visible = False are not included by default, even if they are
  mentioned in AIncludeFieldNames. In order to include invisible fields, pass
  the option rsIncludeInvisibleFields.
}
function RecordToString(const ADataSet: TDataSet;
  const AIncludeFieldNames: string = '*'; const AExcludeFieldNames: string = '';
  const AOptions: TEFRecordToStringOptions = []): string;

type
  {
    Type for items in StringToRecord's AOptions argument.
  }
  TEFStringToRecordOption = (srForceReadOnlyFields);

  {
    A set of TEFStringToRecordOption options.
  }
  TEFStringToRecordOptions = set of TEFStringToRecordOption;

{
  Writes the contents of AString into ADataSet's current record (which must be
  in edit or insert mode). AString should be encoded in the format created by
  RecordToString (which is subject to change without notice - IOW don't use these
  two functions for data persistence).
  Read-only fields in ADataSet are skipped unless you include
  srForceReadOnlyFields in AOptions.
}
procedure StringToRecord(const AString: string; const ADataSet: TDataSet;
  const AOptions: TEFStringToRecordOptions = []);

{
  Expands all registered macros in the specified string by means of the
  specified macro expansion engine. Then expands macros in the format
  %dataset:<FieldName>% by means of ADataSet's current record.
}
function ExpandDataSetMacros(const AString: string; const ADataSet: TDataSet;
  const AMacroExpansionEngine: TEFMacroExpansionEngine): string;

type
  TEFSmallintField = class(TSmallintField)
  protected
    function GetAsBoolean: Boolean; override;
    procedure SetAsBoolean(Value: Boolean); override;
  end;

function GetFieldValuesAsStrings(const AField: TField): TStringDynArray;

procedure AssignFieldValueToNode(const ASource: TField; const ADestination: TEFNode);
procedure AssignFieldsToNode(const ASource: TFields; const ADestination: TEFNode);
procedure AssignNodeValueToParam(const ASource: TEFNode; const ADestination: TParam);

///	<summary>
///	  Returns True if, for every child node of ANode, a field with the same
///	  name exists in AFields and has the same value.
///	</summary>
///	<param name="ANode">
///	  Parent node.
///	</param>
///	<param name="AFields">
///	  Dataset fields.
///	</param>
///	<param name="AFieldNameMappings">
///	  If AFieldNameMappings is specified, it should contain one item for each
///	  child node in ANode, whose name is used (by sequence number) as the field
///	  name to look for in AFields instead of the node's name.
///	</param>
function NodeMatchesFields(const ANode: TEFNode; const AFields: TFields;
  const AFieldNameMappings: TStrings = nil): Boolean;

implementation

uses
  SysUtils, Variants, TypInfo, StrUtils, Provider,
  EF.Intf, EF.Localization, EF.Types, EF.StrUtils;

function EFDataTypeToFieldType(const AEFDataType: TEFDataType): TFieldType;
begin
  case AEFDataType of
    edtUnknown: Result := ftUnknown;
    edtString: Result := ftString;
    edtInteger: Result := ftInteger;
    edtDate: Result := ftDate;
    edtTime: Result := ftTime;
    edtDateTime: Result := ftDateTime;
    edtBoolean: Result := ftBoolean;
    edtCurrency: Result := ftCurrency;
    edtFloat: Result := ftFloat;
    edtBcd: Result := ftBCD;
  else
    raise EEFError.CreateFmt('EFDataTypeToFieldType: data type %s not supported',
      [EFDataTypeToString(AEFDataType)]);
  end;
end;

function FieldTypeToEFDataType(const AFieldType: TFieldType): TEFDataType;
begin
  case AFieldType of
    ftUnknown: Result := edtUnknown;
    ftString, ftMemo{$IFDEF D10+}, ftWideString, ftWideMemo{$ENDIF}, ftFixedChar: Result := edtString;
    ftInteger, ftSmallInt, ftAutoinc: Result := edtInteger;
    ftDate: Result := edtDate;
    ftTime: Result := edtTime;
    ftDateTime, ftTimeStamp: Result := edtDateTime;
    ftBoolean: Result := edtBoolean;
    ftCurrency: Result := edtCurrency;
    ftFloat: Result := edtFloat;
    ftBCD, ftFMTBcd: Result := edtBcd;
  else
    raise EEFError.CreateFmt('FieldTypeToEFDataType: field type %s not supported',
      [GetEnumName(TypeInfo(TFieldType), Ord(AFieldType))]);
  end;
end;

procedure AssignEFDataItemValueToDBParam(const ASource: TEFDataItem;
  const ADestination: TParam);
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
      edtBcd: ADestination.AsFMTBCD := ASource.AsBcd;
    else
      raise EEFError.CreateFmt('AssignEFDataItemValueToDBParam: data type %s not supported',
        [EFDataTypeToString(ASource.DataType)]);
    end;
  end;
end;

procedure AssignEFDataItemValueToDBField(const ASource: TEFDataItem;
  const ADestination: TField);
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
      edtDate: ADestination.AsDateTime := ASource.AsDate;
      edtTime: ADestination.AsDateTime := ASource.AsTime;
      edtDateTime: ADestination.AsDateTime := ASource.AsDateTime;
      edtBoolean: ADestination.AsBoolean := ASource.AsBoolean;
      edtCurrency: ADestination.AsCurrency := ASource.AsCurrency;
      edtFloat: ADestination.AsFloat := ASource.AsFloat;
      edtBcd: ADestination.AsBCD := ASource.AsBcd;
    else
      raise EEFError.CreateFmt('AssignEFDataItemValueToDBField: data type %s not supported',
        [EFDataTypeToString(ASource.DataType)]);
    end;
  end;
end;

procedure AssignDBFieldValueToEFDataItem(const ASource: TField;
  const ADestination: TEFDataItem);
begin
  Assert(Assigned(ASource));
  Assert(Assigned(ADestination));

  if ASource.IsNull then
    ADestination.Clear
  else
  begin
    case ASource.DataType of
      ftString, ftMemo, ftFixedChar{$IFDEF D10+}, ftWideString, ftWideMemo{$ENDIF}: ADestination.AsString := ASource.AsString;
      ftSmallint, ftWord, ftInteger, ftAutoInc: ADestination.AsInteger := ASource.AsInteger;
      ftBoolean: ADestination.AsBoolean := ASource.AsBoolean;
      ftDate: ADestination.AsDate := ASource.AsDateTime;
      ftTime: ADestination.AsTime := ASource.AsDateTime;
      ftDateTime, ftTimeStamp: ADestination.AsDateTime := ASource.AsDateTime;
      ftCurrency: ADestination.AsCurrency := ASource.AsCurrency;
      ftFloat: ADestination.AsFloat := ASource.AsFloat;
      ftBCD, ftFMTBcd: ADestination.AsBcd := ASource.AsBCD;
    else
      raise EEFError.CreateFmt('AssignDBFieldValueToEFDataItem: data type %s not supported',
        [GetEnumName(TypeInfo(TFieldType), Ord(ASource.DataType))]);
    end;
  end;
end;

procedure AssignDBParamValueToEFDataItem(const ASource: TParam;
  const ADestination: TEFDataItem);
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
      ftBCD, ftFMTBcd: ADestination.AsBcd := ASource.AsFMTBCD;
    else
      raise EEFError.CreateFmt('AssignDBParamValueToEFDataItem: data type %s not supported',
        [GetEnumName(TypeInfo(TFieldType), Ord(ASource.DataType))]);
    end;
  end;
end;

procedure AssignEFDataItemToDBParam(const ASource: TEFDataItem;
  const ADestination: TParam);
begin
  AssignEFDataItemValueToDBParam(ASource, ADestination);
  ADestination.Name := ASource.Name; 
end;

procedure CopyRecordToDataSet(const ASourceDataSet, ADestinationDataSet: TDataSet;
  const ABeforePost: TEFDoubleDataSetNotifyEvent = nil;
  const AFieldMap: TEFCustomFieldMap = nil);
var
  LFieldIndex: Integer;
  LSourceField: TField;
  LDestinationField: TField;
  LFieldCount: Integer;
begin
  if Assigned(AFieldMap) then
    LFieldCount := AFieldMap.FieldCount
  else
    LFieldCount := ADestinationDataSet.FieldCount;
  for LFieldIndex := 0 to Pred(LFieldCount) do
  begin
    if Assigned(AFieldMap) then
    begin
      LDestinationField := AFieldMap.DestinationFields[LFieldIndex];
      LSourceField := AFieldMap.SourceFields[LFieldIndex];
    end
    else
    begin
      LDestinationField := ADestinationDataSet.Fields[LFieldIndex];
      LSourceField := ASourceDataSet.FindField(LDestinationField.FieldName);
    end;
    if Assigned(LSourceField) and not LDestinationField.ReadOnly then
      if not LSourceField.IsNull then
        LDestinationField.Value := LSourceField.Value
      else
        LDestinationField.Clear;
  end;
  if Assigned(ABeforePost) then
    ABeforePost(ASourceDataSet, ADestinationDataSet);
end;

procedure AppendRecordToDataSet(const ASourceDataSet, ADestinationDataSet: TDataSet;
  const ABeforePost: TEFDoubleDataSetNotifyEvent = nil;
  const AFieldMap: TEFCustomFieldMap = nil);
begin
  ADestinationDataSet.Append;
  try
    CopyRecordToDataSet(ASourceDataSet, ADestinationDataSet, ABeforePost, AFieldMap);
    ADestinationDataSet.Post;
  except
    ADestinationDataSet.Cancel;
    raise;
  end;
end;

function AppendToDataSet(const ASourceDataSet, ADestinationDataSet: TDataSet;
  const ABeforePost: TEFDoubleDataSetNotifyEvent = nil;
  const IgnoreErrors: Boolean = False;
  const AFieldMap: TEFCustomFieldMap = nil): Longint;
var
  LSourceDataSetWasOpen, LDestinationDataSetWasOpen: Boolean;
  LFieldMap: TEFCustomFieldMap;
begin
  LSourceDataSetWasOpen := ASourceDataSet.Active;
  LDestinationDataSetWasOpen := ADestinationDataSet.Active;
  if not LSourceDataSetWasOpen then
    ASourceDataSet.Open;
  if not LDestinationDataSetWasOpen then
    ADestinationDataSet.Open;
  ASourceDataSet.First;
  Result := 0;
  if Assigned(AFieldMap) then
    LFieldMap := AFieldMap
  else
    LFieldMap := TEFDataSetFieldMap.Create(ASourceDataSet, ADestinationDataSet);
  try
    while not ASourceDataSet.Eof do
    begin
      if IgnoreErrors then
      begin
        try
          AppendRecordToDataSet(ASourceDataSet, ADestinationDataSet, ABeforePost, LFieldMap);
        except
          on E: EDatabaseError do
          begin
            ;
          end;
        end;
      end
      else
        AppendRecordToDataSet(ASourceDataSet, ADestinationDataSet, ABeforePost, LFieldMap);
      Inc(Result);
      ASourceDataSet.Next;
    end;
  finally
    // Only destroy the field map if it was not externally provided.
    if not Assigned(AFieldMap) then
      LFieldMap.Free;
  end;
  if not LSourceDataSetWasOpen then
    ASourceDataSet.Close;
  if not LDestinationDataSetWasOpen then
    ADestinationDataSet.Close;
end;

procedure CopyFieldsToParams(const ASourceFields: TFields;
  const ADestinationParams: TParams; const AParamMap: TEFCustomParamMap = nil);
var
  LParamIndex: Integer;
  LSourceField: TField;
  LDestinationParam: TParam;
  LParamCount: Integer;
begin
  if Assigned(AParamMap) then
    LParamCount := AParamMap.ParamCount
  else
    LParamCount := ADestinationParams.Count;
  for LParamIndex := 0 to Pred(LParamCount) do begin
    if Assigned(AParamMap) then begin
      LDestinationParam := AParamMap.DestinationParams[LParamIndex];
      LSourceField := AParamMap.SourceFields[LParamIndex];
    end
    else begin
      LDestinationParam := ADestinationParams[LParamIndex];
      LSourceField := ASourceFields.FindField(LDestinationParam.Name);
    end;
    if Assigned(LSourceField) then
      if not LSourceField.IsNull then
        LDestinationParam.Value := LSourceField.Value
      else
      begin
        LDestinationParam.Clear;
        // Note: dbX requires all parameters to have a data type,
        // even if they are null.
        LDestinationParam.DataType := LSourceField.DataType;
      end;
  end;
end;

procedure CopyFieldStructureToParams(const ASourceFields: TFields;
  const ADestinationParams: TParams);
var
  LFieldIndex: Integer;
begin
  ADestinationParams.Clear;
  for LFieldIndex := 0 to ASourceFields.Count - 1 do
  begin
    if not (ASourceFields[LFieldIndex].DataType in [ftDataSet]) then
      ADestinationParams.CreateParam(
        ASourceFields[LFieldIndex].DataType,
        ASourceFields[LFieldIndex].FieldName, ptInput);
  end;
end;

procedure AssignParamValue(const ADestinationParam, ASourceParam: TParam);
var
  LOldParamName: string;
begin
  Assert(Assigned(ADestinationParam));
  Assert(Assigned(ASourceParam));

  LOldParamName := ADestinationParam.Name;
  ADestinationParam.Assign(ASourceParam);
  if ADestinationParam.Name <> LOldParamName then
    ADestinationParam.Name := LOldParamName;
end;

{ TEFCustomFieldMap }

function TEFCustomFieldMap.GetDestinationField(const AIndex: Integer): TField;
begin
  Result := FDestinationFields[AIndex];
end;

function TEFCustomFieldMap.GetSourceField(const AIndex: Integer): TField;
begin
  Result := FSourceFields[AIndex];
end;

procedure TEFCustomFieldMap.SetFieldCount(const AValue: Integer);
begin
  FFieldCount := AValue;
  SetLength(FSourceFields, FFieldCount);
  SetLength(FDestinationFields, FFieldCount);
end;

{ TEFDataSetFieldMap }

constructor TEFDataSetFieldMap.Create(const ASourceDataSet, ADestinationDataSet: TDataSet);
var
  LFieldIndex: Integer;
begin
  inherited Create;
  SetFieldCount(ADestinationDataSet.FieldCount);
  for LFieldIndex := 0 to Pred(FieldCount) do begin
    FDestinationFields[LFieldIndex] := ADestinationDataSet.Fields[LFieldIndex];
    FSourceFields[LFieldIndex] := ASourceDataSet.FindField(FDestinationFields[LFieldIndex].FieldName);
  end;
end;

{ TEFArrayFieldMap }

constructor TEFArrayFieldMap.Create(const ASourceDataSet, ADestinationDataSet: TDataSet;
  const ASourceFieldNames, ADestinationFieldNames: array of string);
var
  LFieldIndex: Integer;
begin
  inherited Create;
  if Length(ASourceFieldNames) <> Length(ADestinationFieldNames) then
    raise EEFError.Create('TEFArrayFieldMap: element count of ASourceFiledNames does not match element count of ADestinationFieldNames.');
  SetFieldCount(Length(ASourceFieldNames));
  for LFieldIndex := 0 to FieldCount - 1 do begin
    FSourceFields[LFieldIndex] := ASourceDataSet.FindField(ASourceFieldNames[LFieldIndex]);
    FDestinationFields[LFieldIndex] := ADestinationDataSet.FindField(ADestinationFieldNames[LFieldIndex]);
  end;
end;

constructor TEFArrayFieldMap.Create(const ASourceDataSet,
  ADestinationDataSet: TDataSet; const ASourceFieldNames,
  ADestinationFieldNames: string);
var
  LSourceTokens, LDestinationTokens: TEFStringTokenizer;
  LFieldIndex: Integer;
begin
  inherited Create;
  LSourceTokens := TEFStringTokenizer.Create(ASourceFieldNames, ',');
  try
    LDestinationTokens := TEFStringTokenizer.Create(ADestinationFieldNames, ',');
    try
      if LSourceTokens.TokenCount <> LDestinationTokens.TokenCount then
        raise EEFError.Create('TEFArrayFieldMap: element count of ASourceFiledNames does not match element count of ADestinationFieldNames.');
      SetFieldCount(LSourceTokens.TokenCount);
      for LFieldIndex := 0 to FieldCount - 1 do begin
        FSourceFields[LFieldIndex] := ASourceDataSet.FindField(LSourceTokens[LFieldIndex]);
        FDestinationFields[LFieldIndex] := ADestinationDataSet.FindField(LDestinationTokens[LFieldIndex]);
      end;
    finally
      FreeAndNil(LDestinationTokens);
    end;
  finally
    FreeAndNil(LSourceTokens);
  end;
end;

{ TEFCustomParamMap }

procedure TEFCustomParamMap.AddDestinationParamNames(
  const AParamNames: TStrings);
var
  LParamIndex: Integer;
begin
  for LParamIndex := Low(FDestinationParams) to High(FDestinationParams) do
    AParamNames.Add(FDestinationParams[LParamIndex].Name);
end;

function TEFCustomParamMap.GetDestinationParam(const AIndex: Integer): TParam;
begin
  Result := FDestinationParams[AIndex];
end;

function TEFCustomParamMap.GetSourceField(const AIndex: Integer): TField;
begin
  Result := FSourceFields[AIndex];
end;

{ TEFDataSetParamMap }

constructor TEFDataSetParamMap.Create(const ASourceDataSet: TDataSet;
  const ADestinationParams: TParams; const ANameMap: TEFNameMap = nil);
var
  LParamIndex: Integer;
  LSourceFieldName: string;
begin
  inherited Create;
  FParamCount := ADestinationParams.Count;
  SetLength(FSourceFields, FParamCount);
  SetLength(FDestinationParams, FParamCount);
  for LParamIndex := 0 to Pred(FParamCount) do begin
    FDestinationParams[LParamIndex] := ADestinationParams[LParamIndex];
    if Assigned(ANameMap) then
      LSourceFieldName := ANameMap.SourceNames[FDestinationParams[LParamIndex].Name]
    else
      LSourceFieldName := '';
    if LSourceFieldName = '' then
      LSourceFieldName := FDestinationParams[LParamIndex].Name;
    FSourceFields[LParamIndex] := ASourceDataSet.FindField(LSourceFieldName);
  end;
end;

{ TEFFieldValueTransfer }

constructor TEFFieldValueTransfer.Create;
begin
  FDestinationFieldNames := TStringList.Create;
  FSourceFieldNames := TStringList.Create;
end;

destructor TEFFieldValueTransfer.Destroy;
begin
  FreeAndNil(FSourceFieldNames);
  FreeAndNil(FDestinationFieldNames);
  inherited;
end;

procedure TEFFieldValueTransfer.Execute;
var
  LFieldIndex: Integer;
  LSourceField: TField;
  LDestinationField: TField;
begin
  // check if all parameters are OK
  if not Assigned(FSourceDataSet) then
    raise EEFError.CreateFmt(_('Unspecified dataset "%s".'), ['SourceDataSet']);
  if not Assigned(FDestinationDataSet) then
    raise EEFError.CreateFmt(_('Unspecified dataset "%s".'), ['DestinationDataSet']);
  if FSourceFieldNames.Count = 0 then
    raise EEFError.Create(_('Unspecified source field names.'));
  if FDestinationFieldNames.Count = 0 then
    raise EEFError.Create(_('Unspecified destination field names.'));
  if FSourceFieldNames.Count <> FDestinationFieldNames.Count then
    raise Exception.CreateFmt(_('List length mismatch between "%s" and "%s".'), [FSourceFieldNames.CommaText, FDestinationFieldNames.CommaText]);

  // Everything OK, proceed.
  if (FSourceFieldNames[0] = '*') and (FDestinationFieldNames[0] = '*') then
  begin
    // Copy all fields without mapping names.
    for LFieldIndex := 0 to FSourceDataSet.FieldCount - 1 do
    begin
      LSourceField := FSourceDataSet.Fields[LFieldIndex];
      LDestinationField := FDestinationDataSet.FieldByName(LSourceField.FieldName);
      if not FOnlyIfEmpty or LDestinationField.IsNull then
        LDestinationField.Assign(LSourceField);
    end;
  end
  else
  begin
    // Copy only specified fields and map names.
    for LFieldIndex := 0 to FSourceFieldNames.Count - 1 do
    begin
      LSourceField := FSourceDataSet.FieldByName(FSourceFieldNames[LFieldIndex]);
      LDestinationField := FDestinationDataSet.FieldByName(FDestinationFieldNames[LFieldIndex]);
      if not FOnlyIfEmpty or (LDestinationField.IsNull) then
        LDestinationField.Assign(LSourceField);
    end;
  end;
end;

procedure TEFFieldValueTransfer.SetDestinationDataSet(
  const Value: TDataset);
begin
  FDestinationDataSet := Value;
end;

procedure TEFFieldValueTransfer.SetOnlyIfEmpty(const Value: boolean);
begin
  FOnlyIfEmpty := Value;
end;

procedure TEFFieldValueTransfer.SetSourceDataSet(const Value: TDataset);
begin
  FSourceDataSet := Value;
end;

class procedure TEFFieldValueTransfer.Transfer(
  const ASourceDataSet: TDataSet; const ASourceFieldNameList: string;
  const ADestinationDataSet: TDataSet;
  const ADestinationFieldNameList: string; const AOnlyIfEmpty: Boolean);
var
  LInstance: TEFFieldValueTransfer;
begin
  LInstance := TEFFieldValueTransfer.Create;
  try
    LInstance.SourceDataSet := ASourceDataSet;
    LInstance.SourceFieldNames.CommaText := ASourceFieldNameList;
    LInstance.DestinationDataSet := ADestinationDataSet;
    LInstance.DestinationFieldNames.CommaText := ADestinationFieldNameList;
    LInstance.OnlyIfEmpty := AOnlyIfEmpty;
    LInstance.Execute;
  finally
    LInstance.Free;
  end;
end;

{ TEFParamsEx }

procedure TEFParamsEx.AddItemNames(const AItemNames: TStrings);
var
  LParamIndex: Integer;
begin
  for LParamIndex := 0 to Count - 1 do
    AItemNames.Add(Items[LParamIndex].Name);
end;

procedure TEFParamsEx.Merge(const ASource: TParams;
  const AOverwrite: Boolean = False);
var
  LParamIndex: Integer;
  LParam: TParam;
begin
  BeginUpdate;
  try
    for LParamIndex := 0 to ASource.Count - 1 do
    begin
      LParam := FindParam(ASource[LParamIndex].Name);
      if not Assigned(LParam) then
        Add.Assign(ASource[LParamIndex])
      else if AOverwrite then
        LParam.Assign(ASource[LParamIndex]);
    end;
  finally
    EndUpdate;
  end;
end;

procedure TEFParamsEx.ReadFromFields(const AFields: TFields);
var
  LNEFParam: TParam;
  LFieldIndex: Integer;
begin
  Clear;
  for LFieldIndex := 0 to AFields.Count - 1 do
  begin
    LNEFParam := Add as TParam;
    LNEFParam.Assign(AFields[LFieldIndex]);
  end;
end;

function GetSingletonValue(const ADBConnection: IEFDBConnection;
  const ASQLStatement: string): Variant;
var
  LQuery: IEFDBQuery;
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
    FreeAndNilEFIntf(LQuery);
  end;
end;

function GetStringsFromDB(const ADBConnection: IEFDBConnection;
  const ASQLStatement: string; const AStrings: TStrings): Integer;
var
  LQuery: IEFDBQuery;
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
    FreeAndNilEFIntf(LQuery);
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

procedure SwapFieldValues(const ADataSet: TDataSet; const AFieldNames: string;
  const ADirection: TSwapDirection);
var
  LFieldNames: string;
  LValues1, LValues2: Variant;
begin
  Assert(Assigned(ADataSet));
  Assert(AFieldNames <> '');

  if AFieldNames = '*' then
    LFieldNames := GetDataSetFieldNames(ADataSet)
  else
    LFieldNames := AFieldNames;

  LValues1 := ADataSet.FieldValues[LFieldNames];

  if ADirection = sdUp then
  begin
    ADataSet.Prior;
    LValues2 := ADataSet.FieldValues[LFieldNames];
    ADataSet.Edit;
    ADataSet.FieldValues[LFieldNames] := LValues1;
    ADataSet.Post;
    ADataSet.Next;
    ADataSet.Edit;
    ADataSet.FieldValues[LFieldNames] := LValues2;
    ADataSet.Post;
    ADataSet.Prior;
  end
  else if ADirection = sdDown then
  begin
    ADataSet.Next;
    LValues2 := ADataSet.FieldValues[LFieldNames];
    ADataSet.Edit;
    ADataSet.FieldValues[LFieldNames] := LValues1;
    ADataSet.Post;
    ADataSet.Prior;
    ADataSet.Edit;
    ADataSet.FieldValues[LFieldNames] := LValues2;
    ADataSet.Post;
    ADataSet.Next;
  end;
end;

function IsDataSetOnLastRecord(const ADataSet: TDataSet): Boolean;
begin
  Assert(ADataSet.IsSequenced);

  Result := ADataSet.RecNo = ADataSet.RecordCount;
end;

function IsDataSetOnFirstRecord(const ADataSet: TDataSet): Boolean;
begin
  Assert(ADataSet.IsSequenced);

  Result := ADataSet.RecNo = 1;
end;

procedure SetDBFieldValue(const AField: TField; const AValue: Variant);
var
  LWasReadOnly: Boolean;
begin
  Assert(Assigned(AField));

  LWasReadOnly := AField.ReadOnly;
  if LWasReadOnly then
    AField.ReadOnly := False;
  try
    AField.Value := AValue;
  finally
    if LWasReadOnly then
      AField.ReadOnly := LWasReadOnly;
  end;
end;

function DataSetRecordToString(const ADataSet: TDataSet): string;
var
  LFieldIndex: Integer;
begin
  Assert(Assigned(ADataSet));

  Result := '';
  for LFieldIndex := 0 to ADataSet.FieldCount - 1 do
    if Result = '' then
      Result := ADataSet.Fields[LFieldIndex].FullName + ': '
        + ADataSet.Fields[LFieldIndex].AsString
    else
      Result := Result + ' - ' + ADataSet.Fields[LFieldIndex].FullName + ': '
        + ADataSet.Fields[LFieldIndex].AsString;
end;

procedure DataSetToStrings(const ADataSet: TDataSet; const AStrings: TStrings);
begin
  Assert(Assigned(ADataSet));
  Assert(Assigned(AStrings));

  while not ADataSet.Eof do
  begin
    AStrings.Add(DataSetRecordToString(ADataSet));
    ADataSet.Next;
  end;
end;

{ TEFDataSetMacroExpander }

constructor TEFDataSetMacroExpander.Create(const ADataSet: TDataSet;
  const ANameSpace: string);
begin
  inherited Create;
  FDataSet := ADataSet;
  FNameSpace := ANameSpace;
  if Trim(FNameSpace) <> '' then
    FNameSpace := FNameSpace + ':'
  else
    FNameSpace := '';
end;

function TEFDataSetMacroExpander.InternalExpand(const AString: string): string;
var
  LItemIndex: Integer;
begin
  Result := inherited InternalExpand(AString);
{ TODO : Support macros in values whose names are expanded from macros. }
  for LItemIndex := 0 to FDataSet.FieldCount - 1 do
    Result := ExpandMacros(Result, '%' + FNameSpace + FDataSet.Fields[LItemIndex].FieldName + '%',
      FDataSet.Fields[LItemIndex].AsString)
end;

const
  EF_RECORD_MARKER = '%%EFRECORD%%';
  EF_NULL_FIELD_VALUE = '%%NULL%%';

function RecordToString(const ADataSet: TDataSet;
  const AIncludeFieldNames: string = '*'; const AExcludeFieldNames: string = '';
  const AOptions: TEFRecordToStringOptions = []): string;
var
  LIncludeFieldNames, LExcludeFieldNames, LResult: TStrings;
  LFieldIndex: Integer;

  procedure AppendFieldValue(const AField: TField);
  var
    LFieldValue: string;
  begin
    if AField.Visible or (rsIncludeInvisibleFields in AOptions) then
    begin
      LResult.Add(AField.FieldName);
      if AField.IsNull then
        LFieldValue := EF_NULL_FIELD_VALUE
      else
        LFieldValue := StringReplace(AField.AsString, sLineBreak, Chr(255), [rfReplaceAll]);
      LResult.Add(LFieldValue);
    end;
  end;

begin
  Assert(Assigned(ADataSet));
  Assert(not ADataSet.IsEmpty);
  Assert(AIncludeFieldNames <> '');

  LResult := TStringList.Create;
  try
    LResult.StrictDelimiter := True;
    LResult.Add(EF_RECORD_MARKER);

    LExcludeFieldNames := TStringList.Create;
    try
      LExcludeFieldNames.StrictDelimiter := True;
      LExcludeFieldNames.CommaText := AExcludeFieldNames;

      if AIncludeFieldNames = '*' then
      begin
        for LFieldIndex := 0 to ADataSet.FieldCount - 1 do
          AppendFieldValue(ADataSet.Fields[LFieldIndex]);
      end
      else
      begin
        LIncludeFieldNames := TStringList.Create;
        try
          LIncludeFieldNames.StrictDelimiter := True;
          LIncludeFieldNames.CommaText := AIncludeFieldNames;

          for LFieldIndex := 0 to LIncludeFieldNames.Count - 1 do
            AppendFieldValue(ADataSet.FieldByName(LIncludeFieldNames[LFieldIndex]));
        finally
          LIncludeFieldNames.Free;
        end;
      end;
    finally
      LExcludeFieldNames.Free;
    end;
    Result := LResult.CommaText;
  finally
    LResult.Free;
  end;
end;

procedure StringToRecord(const AString: string; const ADataSet: TDataSet;
  const AOptions: TEFStringToRecordOptions = []);
var
  LValues: TStrings;
  LIndex: Integer;

  procedure SetFieldValue(const AFieldName, AFieldValue: string);
  var
    LField: TField;

    procedure WriteFieldValue(const AField: TField; const AFieldValue: string);
    begin
      if AFieldValue = EF_NULL_FIELD_VALUE then
        LField.Clear
      else
        LField.AsString := AFieldValue;
    end;

  begin
    LField := ADataSet.FieldByName(AFieldName);

    if LField.ReadOnly and (srForceReadOnlyFields in AOptions) then
    begin
      LField.ReadOnly := False;
      try
        WriteFieldValue(LField, AFieldValue);
      finally
        LField.ReadOnly := True;
      end;
    end
    else if not LField.ReadOnly then
      WriteFieldValue(LField, AFieldValue);
  end;
  
begin
  Assert(Assigned(ADataSet));
  ASsert(AString <> '');

  LValues := TStringList.Create;
  try
    LValues.StrictDelimiter := True;
    LValues.CommaText := AString;

    if (LValues.Count mod 2 <> 1) or (LValues[0] <> EF_RECORD_MARKER) then
      raise EEFError.Create(_('The string is not a valid record.'));

    // The marker is not needed anymore.
    LValues.Delete(0);

    LIndex := 0;
    while LIndex < LValues.Count - 1 do
    begin
      SetFieldValue(LValues[LIndex], LValues[LIndex + 1]);
      Inc(LIndex, 2);
    end;

  finally
    LValues.Free;
  end;
end;

function ExpandDataSetMacros(const AString: string; const ADataSet: TDataSet;
  const AMacroExpansionEngine: TEFMacroExpansionEngine): string;
begin
  Assert(Assigned(ADataSet));
  Assert(Assigned(AMacroExpansionEngine));

  Result := AMacroExpansionEngine.Expand(AString);

  with TEFDataSetMacroExpander.Create(ADataSet, 'dataset') do
  begin
    try
      Result := Expand(Result);
    finally
      Free;
    end;
  end;
end;

{ TEFSmallintField }

function TEFSmallintField.GetAsBoolean: Boolean;
begin
  Result := AsInteger = 1;
end;

procedure TEFSmallintField.SetAsBoolean(Value: Boolean);
begin
  if Value then
    AsInteger := 1
  else
    AsInteger := 0;
end;

{ TEFNameMap }

procedure TEFNameMap.ApplyToDBParamNames(const ADBParams: TParams);
var
  LSourceIndex: Integer;
  LDBParam: TParam;
begin
  for LSourceIndex := 0 to FSourceNames.Count - 1 do
  begin
    LDBParam := ADBParams.FindParam(FSourceNames[LSourceIndex]);
    if Assigned(LDBParam) then
      LDBParam.Name := DestinationNames[LDBParam.Name];
  end;
end;

procedure TEFNameMap.ApplyToStrings(const AStrings: TStrings);
var
  LSourceIndex: Integer;
  LStringIndex: Integer;
begin
  for LSourceIndex := 0 to FSourceNames.Count - 1 do
  begin
    LStringIndex := AStrings.IndexOf(FSourceNames[LSourceIndex]);
    if LStringIndex >= 0 then
      AStrings[LStringIndex] := DestinationNames[FSourceNames[LSourceIndex]];
  end;
end;

constructor TEFNameMap.Create(const AAsString: string = '');
begin
  inherited Create;
  FSourceNames := TStringList.Create;
  FDestinationNames := TStringList.Create;
  AsString := AAsString;
end;

destructor TEFNameMap.Destroy;
begin
  FreeAndNil(FSourceNames);
  FreeAndNil(FDestinationNames);
  inherited;
end;

function TEFNameMap.GetAsString: string;
var
  LSourceIndex: Integer;
  LStrings: TStrings;
begin
  Result := '';
  LStrings := TStringList.Create;
  try
    for LSourceIndex := 0 to FSourceNames.Count - 1 do
      LStrings.Add(FSourceNames[LSourceIndex] + '=' +
        DestinationNames[FSourceNames[LSourceIndex]]);
    Result := LStrings.CommaText;
  finally
    FreeAndNil(LStrings);
  end;
end;

function TEFNameMap.GetDestinationName(const ASourceName: string): string;
var
  LIndex: Integer;
begin
  if FSourceNames.Find(ASourceName, LIndex) then
    Result := FDestinationNames[Integer(FSourceNames.Objects[LIndex])]
  else
    Result := '';
end;

function TEFNameMap.GetSourceName(const ADestinationName: string): string;
var
  LIndex: Integer;
begin
  if FDestinationNames.Find(ADestinationName, LIndex) then
    Result := FSourceNames[Integer(FDestinationNames.Objects[LIndex])]
  else
    Result := '';
end;

procedure TEFNameMap.SetAsString(const AValue: string);
var
  LKeyIndex, LSourceIndex, LDestinationIndex: Integer;
  LEqualPos: Integer;
  LMapStrings: TStringList;
  LSourceName, LDestinationName: string;
begin
  LMapStrings := TStringList.Create;
  try
    // First check if the map string is valid, without destroying the current
    // map contents.
    LMapStrings.CommaText := AValue;
    for LKeyIndex := 0 to LMapStrings.Count - 1 do
    begin
      LEqualPos := Pos('=', LMapStrings[LKeyIndex]);
      if LEqualPos = 0 then
        raise EEFError.CreateFmt(_('String "%s" is not a valid name map.'), [AValue]);
    end;
    // Then parse the string map and set the two-way keys.
    FSourceNames.Clear;
    FDestinationNames.Clear;
    for LKeyIndex := 0 to LMapStrings.Count - 1 do
    begin
      LEqualPos := Pos('=', LMapStrings[LKeyIndex]);
      LSourceName := Trim(Copy(LMapStrings[LKeyIndex], 1, LEqualPos - 1));
      LDestinationName := Trim(Copy(LMapStrings[LKeyIndex], LEqualPos + 1, MaxInt));
      FSourceNames.Add(LSourceName + '=' + LDestinationName);
      FDestinationNames.Add(LDestinationName + '=' + LSourceName);
    end;
    // Establish the two-way link between the elements in FSourceNames and the
    // elements in FDestinationNames.
    FSourceNames.Sort;
    FDestinationNames.Sort;
    for LSourceIndex := 0 to FSourceNames.Count - 1 do
    begin
      LDestinationIndex := FDestinationNames.IndexOfName(FSourceNames.ValueFromIndex[LSourceIndex]);
      FSourceNames.Objects[LSourceIndex] := TObject(LDestinationIndex);
      FDestinationNames.Objects[LDestinationIndex] := TObject(LSourceIndex);
    end;
    // Finally, drop the part after the '=' sign, which was used only to
    // establish the links; no need to re-sort the lists.
    for LKeyIndex := 0 to FSourceNames.Count - 1 do
    begin
      LEqualPos := Pos('=', FSourceNames[LKeyIndex]);
      FSourceNames[LKeyIndex] := Copy(FSourceNames[LKeyIndex], 1, LEqualPos - 1);
      LEqualPos := Pos('=', FDestinationNames[LKeyIndex]);
      FDestinationNames[LKeyIndex] := Copy(FDestinationNames[LKeyIndex], 1, LEqualPos - 1);
    end;
  finally
    FreeAndNil(LMapStrings);
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

procedure AssignFieldValueToNode(const ASource: TField; const ADestination: TEFNode);
begin
  Assert(Assigned(ASource));
  Assert(Assigned(ADestination));

  if ASource.IsNull then
    ADestination.Clear
  else
  begin
    case ASource.DataType of
      ftString, ftMemo, ftFixedChar{$IFDEF D10+}, ftWideString, ftWideMemo{$ENDIF}: ADestination.AsString := ASource.AsString;
      ftSmallint, ftWord, ftInteger, ftAutoInc: ADestination.AsInteger := ASource.AsInteger;
      ftBoolean: ADestination.AsBoolean := ASource.AsBoolean;
      { TODO : support other data types in nodes. }
      //ftDate: ADestination.AsDate := ASource.AsDateTime;
      //ftTime: ADestination.AsTime := ASource.AsDateTime;
      //ftDateTime, ftTimeStamp: ADestination.AsDateTime := ASource.AsDateTime;
      //ftCurrency: ADestination.AsCurrency := ASource.AsCurrency;
      //ftFloat: ADestination.AsFloat := ASource.AsFloat;
      //ftBCD, ftFMTBcd: ADestination.AsBcd := ASource.AsBCD;
    else
      raise EEFError.CreateFmt('AssignFieldValueToNode: data type %s not supported',
        [GetEnumName(TypeInfo(TFieldType), Ord(ASource.DataType))]);
    end;
  end;
end;

procedure AssignFieldsToNode(const ASource: TFields;
  const ADestination: TEFNode);
var
  LFieldIndex: Integer;
begin
  Assert(Assigned(ASource));
  Assert(Assigned(ADestination));

  ADestination.ClearChildren;
  for LFieldIndex := 0 to ASource.Count - 1 do
    AssignFieldValueToNode(ASource[LFieldIndex],
      ADestination.GetNode(ASource[LFieldIndex].FullName, True));
end;

procedure AssignNodeValueToParam(const ASource: TEFNode; const ADestination: TParam);
begin
  Assert(Assigned(ASource));
  Assert(Assigned(ADestination));

  if ASource.IsNull then
    ADestination.Clear
  else
  begin
    { TODO : support data types; maybe refactor M/D relationships so they don't use nodes? }
    ADestination.AsString := ASource.AsString;
//    case ASource.DataType of
//      edtUnknown, edtString: ADestination.AsString := ASource.AsString;
//      edtInteger: ADestination.AsInteger := ASource.AsInteger;
//      edtDate: ADestination.AsDate := ASource.AsDate;
//      edtTime: ADestination.AsTime := ASource.AsTime;
//      edtDateTime: ADestination.AsDateTime := ASource.AsDateTime;
//      edtBoolean: ADestination.AsBoolean := ASource.AsBoolean;
//      edtCurrency: ADestination.AsCurrency := ASource.AsCurrency;
//      edtFloat: ADestination.AsFloat := ASource.AsFloat;
//      edtBcd: ADestination.AsFMTBCD := ASource.AsBcd;
//    else
//      raise EEFError.CreateFmt('AssignEFDataItemValueToDBParam: data type %s not supported',
//        [EFDataTypeToString(ASource.DataType)]);
//    end;
  end;
end;

function NodeMatchesFields(const ANode: TEFNode; const AFields: TFields;
  const AFieldNameMappings: TStrings = nil): Boolean;
var
  I: Integer;
  LNode: TEFNode;
  LField: TField;
  LFieldNode: TEFNode;
  LFieldName: string;
begin
  Assert(Assigned(ANode));
  Assert(Assigned(AFields));
  if Assigned(AFieldNameMappings) then
    Assert(AFieldNameMappings.Count = ANode.ChildCount);

  Result := True;
  for I := 0 to ANode.ChildCount - 1 do
  begin
    LNode := ANode.Children[I];
    if Assigned(AFieldNameMappings) then
      LFieldName := AFieldNameMappings[I]
    else
      LFieldName := LNode.Name;
    LField := AFields.FindField(LFieldName);
    if Assigned(LField) then
    begin
      // Must set same names, otherwise EqualsNode will fail.
      LFieldNode := TEFNode.Create(LNode.Name);
      try
        AssignFieldValueToNode(LField, LFieldNode);
        if not LNode.EqualsNode(LFieldNode) then
        begin
          Result := False;
          Break;
        end;
      finally
        FreeAndNil(LFieldNode);
      end;
    end
    else
    begin
      Result := False;
      Break;
    end;
  end;
end;

initialization
  DefaultFieldClasses[ftSmallint] := TEFSmallintField;

end.
