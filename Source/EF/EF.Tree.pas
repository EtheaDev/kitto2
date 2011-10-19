unit EF.Tree;

interface

uses
  SysUtils, Types, Variants, DB, FmtBcd, Generics.Collections, SyncObjs,
  EF.Types, EF.Macros;

type
  TEFDataType = (edtUnknown, edtString, edtInteger, edtDate, edtTime, edtDateTime,
    edtBoolean, edtCurrency, edtFloat, edtObject, edtDecimal);
  TEFDataTypes = set of TEFDataType;

const
  AllDataTypes = [edtString, edtInteger, edtDate, edtTime, edtDateTime,
    edtBoolean, edtCurrency, edtFloat, edtDecimal];
  NumericDataTypes = [edtInteger, edtCurrency, edtFloat, edtDecimal];
  StringDataTypes = [edtString];

type
  TEFNode = class;
  TEFNodeClass = class of TEFNode;
  TEFNodes = TObjectList<TEFNode>;

  TEFTree = class
  private
    FCriticalSection: TCriticalSection;
    FNodes: TEFNodes;
    function GetChild(I: Integer): TEFNode; overload;
    function GetChildCount: Integer; overload;
  protected
    function GetChildClass(const AName: string): TEFNodeClass; virtual;
    procedure EnterCS; virtual;
    procedure LeaveCS; virtual;
  public
    destructor Destroy; override;
  public
    class function ValueToString(const AValue: Variant): string; static; inline;
    class function ValueToInteger(const AValue: Variant): Integer; static; inline;
    class function ValueToObject(const AValue: Variant): TObject; static; inline;
    class function ValueToBoolean(const AValue: Variant): Boolean; static; inline;
    class function ValueToStringArray(const AValue: Variant): TStringDynArray; static; inline;
    class function ValueToPairs(const AValue: Variant): TEFPairs; static; inline;
    class function ValueToDate(const AValue: Variant): TDate; static; inline;
    class function ValueToTime(const AValue: Variant): TTime; static; inline;
    class function ValueToDateTime(const AValue: Variant): TDateTime; static; inline;
    class function ValueToCurrency(const AValue: Variant): Currency; static; inline;
    class function ValueToFloat(const AValue: Variant): Double; static; inline;
    class function ValueToDecimal(const AValue: Variant): TBcd; static; inline;

    class function StringToValue(const AString: string): Variant; static; inline;
    class function IntegerToValue(const AInteger: Integer): Variant; static; inline;
    class function ObjectToValue(const AObject: TObject): Variant; static; inline;
    class function BooleanToValue(const ABoolean: Boolean): Variant; static; inline;
    class function StringArrayToValue(const AStringArray: TStringDynArray): Variant; static; inline;
    class function PairsToValue(const APairs: TEFPairs): Variant; static; inline;
    class function DateToValue(const ADate: TDate): Variant; static; inline;
    class function TimeToValue(const ATime: TTime): Variant; static; inline;
    class function DateTimeToValue(const ADateTime: TDateTime): Variant; static; inline;
    class function CurrencyToValue(const ACurrency: Currency): Variant; static; inline;
    class function FloatToValue(const AFloat: Double): Variant; static; inline;
    class function DecimalToValue(const ADecimal: TBcd): Variant; static; inline;

    constructor Create; virtual;
    constructor Clone(const ASource: TEFTree); virtual;
    procedure Clear; virtual;
    procedure Assign(const ASource: TEFTree); virtual;

    ///	<summary>
    ///	  Adds a child node. Returns a reference to the added object.
    ///	</summary>
    function AddChild(const ANode: TEFNode): TEFNode; overload;

    ///	<summary>
    ///	  Creates a child node of a type that may depend on the parameters,
    ///	  fills and adds it. Then returns a reference to the added object.
    ///	</summary>
    function AddChild(const AName: string; const AValue: Variant): TEFNode; overload;

    ///	<summary>
    ///	  Creates a child node of a type that may depend on the parameters,
    ///	  sets the name and adds it. Then returns a reference to the added object.
    ///	</summary>
    function AddChild(const AName: string): TEFNode; overload;

    property Children[I: Integer]: TEFNode read GetChild; default;
    property ChildCount: Integer read GetChildCount;
    function FindChild(const AName: string; const ACreateMissingNodes: Boolean = False): TEFNode;
    function ChildByName(const AName: string): TEFNode;
    procedure RemoveChild(const ANode: TEFNode);
    procedure ClearChildren;
    property NodeList: TEFNodes read FNodes;

    function GetChildCount<T: class>: Integer; overload;
    function GetChild<T: class>(const AIndex: Integer): T; overload;

    function FindNode(const APath: string; const ACreateMissingNodes: Boolean = False): TEFNode; virtual;
    function GetNode(const APath: string; const ACreateMissingNodes: Boolean = False): TEFNode;
    procedure DeleteNode(const APath: string);

    function GetValue(const APath: string; const ADefaultValue: Variant): Variant; overload;
    function GetValue(const APath: string): Variant; overload;
    function GetBoolean(const APath: string; const ADefaultValue: Boolean = False): Boolean;
    function GetInteger(const APath: string; const ADefaultValue: Integer = 0): Integer;
    function GetString(const APath: string; const ADefaultValue: string = ''): string;
    function GetDate(const APath: string; const ADefaultValue: TDate = 0): TDate;

    function GetExpandedString(const APath: string; const ADefaultValue: string = ''): string;
    function GetStringArray(const APath: string; const ADefaultValue: TStringDynArray = nil): TStringDynArray;
    function GetPairs(const APath: string; const ADefaultValue: TEFPairs = nil): TEFPairs;

    ///	<summary>
    ///	  Returns a separated list of all children of the specified node in the
    ///	  form Name&lt;AConnector&gt;AsString.
    ///	</summary>
    ///	<param name="APath">
    ///	  Locates the parent node of the strings to extract.
    ///	</param>
    ///	<param name="ASeparator">
    ///	  Separator for name/value pairs (ex. '=').
    ///	</param>
    ///	<param name="AConnector">
    ///	  String used to connect pairs (ex. sLineBreak).
    ///	</param>
    ///	<param name="ADefaultValue">
    ///	  Value to return when the node does not exist.
    ///	</param>
    ///	<returns>
    ///	  A concatenation of all children of the specified node built according
    ///	  to the arguments.
    ///	</returns>
    function GetChildrenAsStrings(const APath: string; const ASeparator: string = sLineBreak;
      const AConnector: string = '='; const ADefaultValue: string = ''): string;

    ///	<summary>
    ///	  Returns an array of all children of the specified node, which must be
    ///   in name=value format, as pairs.
    ///	</summary>
    ///	<param name="APath">
    ///	  Locates the parent node of the strings to extract.
    ///	</param>
    ///	<returns>
    ///	  Array of name/value pairs.
    ///	</returns>
    function GetChildrenAsPairs(const APath: string; const ADefaultValue: TEFPairs = nil): TEFPairs;

    ///	<summary>
    ///	  Same as GetStrings, but returns expanded strings: Each string is
    ///	  passed to the macro expander before concatenation.
    ///	</summary>
    function GetExpandedStrings(const APath: string; const ASeparator: string = sLineBreak;
      const AConnector: string = '='; const ADefaultValue: string = ''): string;

    function GetObject(const APath: string; const ADefaultValue: TObject = nil): TObject;

    procedure SetInteger(const APath: string; const AValue: Integer);
    procedure SetString(const APath: string; const AValue: string);
    procedure SetObject(const APath: string; const AValue: TObject);
    procedure SetBoolean(const APath: string; const AValue: Boolean);

    procedure AddFieldsAsChildren(const AFields: TFields);
  end;

  TEFNode = class(TEFTree)
  private
    FParent: TEFTree;
    FValue: Variant;
    FName: string;
    FDataType: TEFDataType;
    function GetAsString: string;
    function GetAsInteger: Integer;
    function FindNodeFrom(const APath: TStringDynArray; const AIndex: Integer;
      const ACreateMissingNodes: Boolean = False): TEFNode;
    function GetAsExpandedString: string;
    procedure SetAsString(const AValue: string);
    procedure SetAsInteger(const AValue: Integer);
    function GetAsObject: TObject;
    procedure SetAsObject(const AValue: TObject);
    function GetAsBoolean: Boolean;
    procedure SetAsBoolean(const AValue: Boolean);
    function GetAsStringArray: TStringDynArray;
    procedure SetAsStringArray(const AValue: TStringDynArray);
    function GetAsPairs: TEFPairs;
    procedure SetAsPairs(const AValue: TEFPairs);
    function GetAsPair: TEFPair;
    procedure SetAsPair(const AValue: TEFPair);
    function GetValue: Variant;
    function GetIndex: Integer;
    function GetIsNull: Boolean;
    function GetAsDate: TDate;
    procedure SetAsDate(const AValue: TDate);
    function GetAsDateTime: TDateTime;
    function GetAsTime: TTime;
    procedure SetAsDateTime(const AValue: TDateTime);
    procedure SetAsTime(const AValue: TTime);
    function GetAsCurrency: Currency;
    procedure SetAsCurrency(const AValue: Currency);
    function GetAsFloat: Double;
    procedure SetAsFloat(const AValue: Double);
    function GetAsDecimal: TBcd;
    procedure SetAsDecimal(const AValue: TBcd);
    procedure SetDataType(const AValue: TEFDataType);
  protected
    procedure SetName(const AValue: string);
    function GetName: string; virtual;
    procedure SetValue(const AValue: Variant); virtual;
  public
    procedure Assign(const ASource: TEFTree); override;
    procedure AssignValue(const ASource: TEFNode);
    property Parent: TEFTree read FParent;
    property Index: Integer read GetIndex;
    function GetEnumerator: TEnumerator<TEFNode>;
    constructor Create(const AName: string; const AValue: Variant); reintroduce; overload; virtual;
    constructor Create(const AName: string); reintroduce; overload;
    constructor Create; reintroduce; overload;
    constructor Clone(const ASource: TEFTree); override;

    procedure Clear; override;

    ///	<summary>
    ///	  Removes itself from its parent. This causes the parent to free the
    ///	  current object.
    ///	</summary>
    procedure Delete;

    property Name: string read GetName;
    property DataType: TEFDataType read FDataType write SetDataType;
    property Value: Variant read GetValue write SetValue;

    property AsString: string read GetAsString write SetAsString;
    property AsStringArray: TStringDynArray read GetAsStringArray write SetAsStringArray;
    property AsPair: TEFPair read GetAsPair write SetAsPair;
    property AsPairs: TEFPairs read GetAsPairs write SetAsPairs;
    property AsExpandedString: string read GetAsExpandedString;
    property AsInteger: Integer read GetAsInteger write SetAsInteger;
    property AsObject: TObject read GetAsObject write SetAsObject;
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsDate: TDate read GetAsDate write SetAsDate;
    property AsTime: TTime read GetAsTime write SetAsTime;
    property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
    property AsCurrency: Currency read GetAsCurrency write SetAsCurrency;
    property AsFloat: Double read GetAsFloat write SetAsFloat;
    property AsDecimal: TBcd read GetAsDecimal write SetAsDecimal;

    ///	<summary>Parses AValue trying to guess its data type and sets Value and
    ///	DataType accordingly.</summary>
    ///	<param name="AValue">Value to parse, usually read from a Yaml
    ///	stream.</param>
    ///	<param name="AFormatSettings">Format settings to use to parse the
    ///	value. You can use Session.UserFormatSettings, or
    ///	Session.JSFormatSettings, or custom settings.</param>
    ///	<returns>Returns Self to allow for fluent calls.</returns>
    function SetAsYamlValue(const AValue: string; const AFormatSettings: TFormatSettings): TEFNode;

    property IsNull: Boolean read GetIsNull;
    procedure SetToNull;
    function EqualsNode(const ANode: TEFNode): Boolean;
    function EqualsValue(const AValue: Variant): Boolean;

    function GetChildStrings(const ASeparator: string = sLineBreak;
      const AConnector: string = '='; const ADefaultValue: string = ''): string; overload;
    function GetExpandedChildStrings(const ASeparator, AConnector,
      ADefaultValue: string): string;
    function GetChildPairs: TEFPairs;

    procedure AssignFieldValue(const AField: TField);
  end;

  TEFTreeFactory = class
  public
    class function LoadFromFile<T: TEFTree, constructor>(const AFileName: string): T;
  end;

  ///	<summary>
  ///	  <para>
  ///	    A macro expander that expands all the strings contained in a TEFTree
  ///	    object. Each macro in this format:
  ///	  </para>
  ///	  <para>
  ///	    %&lt;NameSpace&gt;:&lt;Path&gt;%
  ///	  </para>
  ///	  <para>
  ///	    is expanded to the string value of the tree node located by the path.
  ///	    This macro expander holds a reference to a TEFTree. &lt;NameSpace&gt;
  ///	    is a value set upon creation. If it is empty, then no ':' separator
  ///	    is required in the macros.
  ///	  </para>
  ///	</summary>
  ///	<remarks>
  ///	  <para>
  ///	    The need for a name space stems from the fact that you can have
  ///	    multiple macro expanders of this kind active at the same time, each
  ///	    linked to a different tree object, and use the namespace string to
  ///	    differentiate them.
  ///	  </para>
  ///	  <para>
  ///	    This macro expander is not registered by default, as it needs a
  ///	    reference to an external object to work. So, applications will create
  ///	    and use this class autonomously (by querying it directly or adding it
  ///	    to an expansion engine, without registering it) as required.
  ///	  </para>
  ///	</remarks>
  TEFTreeMacroExpander = class(TEFMacroExpander)
  private
    FTree: TEFTree;
    FPrefix: string;
  protected
    function InternalExpand(const AString: string): string; override;
  public
    constructor Create(const ATree: TEFTree;
      const ANameSpace: string); reintroduce;
  end;

function StringToEFDataType(const AString: string): TEFDataType;
function EFDataTypeToString(const ADataType: TEFDataType): string;

implementation

uses
  StrUtils, TypInfo,
  EF.Environment, EF.Localization, EF.StrUtils, EF.YAML, EF.VariantUtils;

function EFDataTypeToString(const ADataType: TEFDataType): string;
begin
  Result := StripPrefix(GetEnumName(TypeInfo(TEFDataType), Ord(ADataType)), 'edt');
end;

function StringToEFDataType(const AString: string): TEFDataType;
var
  LValue: Integer;
begin
  if AString = '' then
    Result := edtUnknown
  else
  begin
    LValue := GetEnumValue(TypeInfo(TEFDataType), 'edt' + AString);
    if (LValue < 0) or (LValue > Ord(High(TEFDataType))) then
      raise EEFError.CreateFmt(_('Unknown datatype %s.'), [AString]);
    Result := TEFDataType(LValue);
  end;
end;

function GetVariantDataType(const AVariant: Variant): TEFDataType;
begin
  if VarIsType(AVariant, [varString, varOleStr, varUString]) then
    Result := edtString
  else if VarIsType(AVariant, [varShortInt, varByte, varWord, varLongWord, varSmallint, varInteger, varInt64, varUInt64]) then
    Result := edtInteger
  else if VarIsType(AVariant, [varDate]) then
  begin
    if TDateTime(AVariant) = Trunc(TDateTime(AVariant)) then
      Result := edtDate
    else if TDateTime(AVariant) = Frac(TDateTime(AVariant)) then
      Result := edtTime
    else
      Result := edtDateTime;
  end
  else if VarIsType(AVariant, [varBoolean]) then
    Result := edtBoolean
  else if VarIsType(AVariant, [varCurrency]) then
    Result := edtCurrency
  else if VarIsFMTBcd(AVariant) then
    Result := edtDecimal
  else if VarIsType(AVariant, [varSingle, varDouble]) then
    Result := edtFloat
  else if VarIsType(AVariant, [varObject]) then
    Result := edtObject
  else
    Result := edtUnknown;
end;

{ TEFNode }

procedure TEFNode.Assign(const ASource: TEFTree);
begin
  inherited;
  if Assigned(ASource) then
  begin
    FName := TEFNode(ASource).Name;
    AssignValue(TEFNode(ASource));
  end;
end;

procedure TEFNode.AssignValue(const ASource: TEFNode);
begin
  if Assigned(ASource) then
  begin
    FValue := TEFNode(ASource).Value;
    FDataType := TEFNode(ASource).DataType;
  end
  else
    SetToNull;
end;

procedure TEFNode.AssignFieldValue(const AField: TField);
begin
  Assert(Assigned(AField));

  if AField.IsNull then
    SetToNull
  else
  begin
    if DataType = edtUnknown then
    begin
      case AField.DataType of
        ftString, ftMemo, ftFixedChar, ftWideString, ftWideMemo: AsString := AField.AsString;
        ftSmallint, ftWord, ftInteger, ftAutoInc: AsInteger := AField.AsInteger;
        ftBoolean: AsBoolean := AField.AsBoolean;
        ftDate: AsDate := AField.AsDateTime;
        ftTime: AsTime := AField.AsDateTime;
        ftDateTime, ftTimeStamp: AsDateTime := AField.AsDateTime;
        ftCurrency: AsCurrency := AField.AsCurrency;
        ftFloat: AsFloat := AField.AsFloat;
        ftBCD, ftFMTBcd: AsDecimal := AField.AsBCD;
      else
        raise EEFError.CreateFmt('AssignFieldValueToNode: data type %s not supported',
          [GetEnumName(TypeInfo(TFieldType), Ord(AField.DataType))]);
      end;
    end
    else
    begin
      case DataType of
        edtString: AsString := AField.AsString;
        edtInteger: AsInteger := AField.AsInteger;
        edtBoolean: AsBoolean := AField.AsBoolean;
        edtDate: AsDate := AField.AsDateTime;
        edtTime: AsTime := AField.AsDateTime;
        edtDateTime: AsDateTime := AField.AsDateTime;
        edtCurrency: AsCurrency := AField.AsCurrency;
        edtFloat: AsFloat := AField.AsFloat;
        edtObject: raise EEFError.CreateFmt('AssignFieldValueToNode: data type %s not supported',
          [EFDataTypeToString(DataType)]);
        edtDecimal: AsDecimal := AField.AsBCD;
      end;
    end;
  end;
end;

procedure TEFNode.Clear;
begin
  inherited;
  FName := '';
  FValue := '';
end;

constructor TEFNode.Clone(const ASource: TEFTree);
begin
  Assert((ASource = nil) or (ASource is TEFNode));

  inherited;
  Assign(TEFNode(ASource));
end;

constructor TEFNode.Create(const AName: string);
begin
  Create(AName, Null);
end;

constructor TEFNode.Create;
begin
  Create('', Null);
end;

constructor TEFNode.Create(const AName: string; const AValue: Variant);
begin
  inherited Create;
  FDataType := edtUnknown;
  SetName(AName);
  Value := AValue;
end;

procedure TEFNode.Delete;
begin
  if Assigned(Parent) then
    Parent.RemoveChild(Self);
end;

function TEFNode.GetAsBoolean: Boolean;
begin
  Result := ValueToBoolean(FValue);
end;

function TEFNode.GetAsCurrency: Currency;
begin
  Result := ValueToCurrency(FValue);
end;

function TEFNode.GetAsDate: TDate;
begin
  Result := ValueToDate(FValue);
end;

function TEFNode.GetAsDateTime: TDateTime;
begin
  Result := ValueToDateTime(FValue);
end;

function TEFNode.GetAsDecimal: TBcd;
begin
  Result := ValueToDecimal(FValue);
end;

function TEFNode.GetAsExpandedString: string;
begin
  Result := Environment.MacroExpansionEngine.Expand(ASString);
end;

function TEFNode.GetAsFloat: Double;
begin
  Result := ValueToFloat(FValue);
end;

function TEFNode.GetAsInteger: Integer;
begin
  Result := ValueToInteger(FValue);
end;

function TEFNode.GetAsObject: TObject;
begin
  Result := ValueToObject(FValue);
end;

function TEFNode.GetAsPair: TEFPair;
begin
  Result := TEFPair.Create(Name, AsString);
end;

function TEFNode.GetAsPairs: TEFPairs;
begin
  Result := ValueToPairs(FValue);
end;

function TEFNode.GetAsString: string;
begin
  Result := ValueToString(FValue);
end;

function TEFNode.GetAsStringArray: TStringDynArray;
begin
  Result := ValueToStringArray(FValue);
end;

function TEFNode.GetAsTime: TTime;
begin
  Result := ValueToTime(FValue);
end;

function TEFNode.GetChildStrings(const ASeparator, AConnector,
  ADefaultValue: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to FNodes.Count - 1 do
  begin
    if Result = '' then
      Result := FNodes[I].Name + AConnector + FNodes[I].AsString
    else
      Result := Result + ASeparator + FNodes[I].Name + AConnector + FNodes[I].AsString;
  end;
end;

function TEFNode.GetChildPairs: TEFPairs;
var
  I: Integer;
begin
  SetLength(Result, ChildCount);
  for I := 0 to ChildCount - 1 do
    Result[I] := Children[I].AsPair;
end;

function TEFNode.GetEnumerator: TEnumerator<TEFNode>;
begin
  Result := FNodes.GetEnumerator;
end;

function TEFNode.GetExpandedChildStrings(const ASeparator, AConnector,
  ADefaultValue: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to FNodes.Count - 1 do
  begin
    if Result = '' then
      Result := FNodes[I].Name + AConnector + FNodes[I].AsExpandedString
    else
      Result := Result + ASeparator + FNodes[I].Name + AConnector + FNodes[I].AsExpandedString;
  end;
end;

function TEFNode.GetIndex: Integer;
begin
  if Parent <> nil then
    Result := Parent.NodeList.IndexOf(Self)
  else
    Result := -1;
end;

function TEFNode.GetIsNull: Boolean;
begin
  Result := VarIsNull(FValue);
end;

function TEFNode.GetName: string;
begin
  Result := FName;
end;

function TEFNode.GetValue: Variant;
begin
  Result := FValue;
end;

procedure TEFNode.SetAsBoolean(const AValue: Boolean);
begin
  FValue := BooleanToValue(AValue);
end;

procedure TEFNode.SetAsCurrency(const AValue: Currency);
begin
  FValue := CurrencyToValue(AValue);
  FDataType := edtCurrency;
end;

procedure TEFNode.SetAsDate(const AValue: TDate);
begin
  FValue := DateToValue(AValue);
  FDataType := edtDate;
end;

procedure TEFNode.SetAsDateTime(const AValue: TDateTime);
begin
  FValue := DateTimeToValue(AValue);
  FDataType := edtDateTime;
end;

procedure TEFNode.SetAsDecimal(const AValue: TBcd);
begin
  FValue := DecimalToValue(AValue);
  FDataType := edtDecimal;
end;

procedure TEFNode.SetAsFloat(const AValue: Double);
begin
  FValue := FloatToValue(AValue);
  FDataType := edtFloat;
end;

procedure TEFNode.SetAsInteger(const AValue: Integer);
begin
  FValue := IntegerToValue(AValue);
  FDataType := edtInteger;
end;

procedure TEFNode.SetAsObject(const AValue: TObject);
begin
  FValue := ObjectToValue(AValue);
  FDataType := edtObject;
end;

procedure TEFNode.SetAsPair(const AValue: TEFPair);
begin
  SetName(AValue.Key);
  AsString := AValue.Value;
end;

procedure TEFNode.SetAsPairs(const AValue: TEFPairs);
begin
  FValue := PairsToValue(AValue);
  FDataType := edtString;
end;

procedure TEFNode.SetAsString(const AValue: string);
begin
  FValue := StringToValue(AValue);
  FDataType := edtString;
end;

procedure TEFNode.SetAsStringArray(const AValue: TStringDynArray);
begin
  FValue := StringArrayToValue(AValue);
  FDataType := edtString;
end;

procedure TEFNode.SetAsTime(const AValue: TTime);
begin
  FValue := TimeToValue(AValue);
  FDataType := edtTime;
end;

function TEFNode.SetAsYamlValue(const AValue: string; const AFormatSettings: TFormatSettings): TEFNode;
var
  LDateTime: TDateTime;
  LBoolean: Boolean;
  LDouble: Double;
  LInteger: Integer;
begin
  if DataType = edtUnknown then
  begin
    if TryStrToInt(AValue, LInteger) then
      AsInteger := LInteger
    else if TryStrToFloat(AValue, LDouble, AFormatSettings) then
      AsFloat := LDouble
    else if TryStrToDateTime(AValue, LDateTime, AFormatSettings) then
      AsDateTime := LDateTime
    else if TryStrToDate(AValue, LDateTime, AFormatSettings) then
      AsDate := LDateTime
    else if TryStrToTime(AValue, LDateTime, AFormatSettings) then
      AsTime := LDateTime
    else if TryStrToBool(AValue, LBoolean) then
      AsBoolean := LBoolean
    else
      AsString := AValue;
  end
  else
  begin
    case DataType of
      edtString: AsString := AValue;
      edtInteger: AsInteger := StrToInt(AValue);
      edtDate: AsDate := StrToDate(AValue, AFormatSettings);
      edtTime: AsTime := StrToTime(AValue, AFormatSettings);
      edtDateTime: AsDateTime := StrToDateTime(AValue, AFormatSettings);
      edtBoolean: AsBoolean := StrToBool(AValue);
      edtCurrency: AsCurrency := StrToCurr(AValue, AFormatSettings);
      edtFloat: AsFloat := StrToFloat(AValue, AFormatSettings);
      edtDecimal: AsDecimal := StrToBcd(AValue, AFormatSettings);
      edtObject: raise EEFError.CreateFmt('Invalid value %s for data type %s.',
        [AValue, EFDataTypeToString(DataType)]);
    end;
  end;
  Result := Self;
end;

procedure TEFNode.SetDataType(const AValue: TEFDataType);
begin
  FDataType := AValue;
  SetToNull;
end;

procedure TEFNode.SetName(const AValue: string);
begin
  FName := AValue;
end;

procedure TEFNode.SetValue(const AValue: Variant);
begin
  FValue := AValue;
  FDataType := GetVariantDataType(AValue);
end;

procedure TEFNode.SetToNull;
begin
  FValue := Null;
end;

function TEFNode.FindNodeFrom(const APath: TStringDynArray;
  const AIndex: Integer; const ACreateMissingNodes: Boolean): TEFNode;
var
  LChild: TEFNode;
begin
  if AIndex = Length(APath) then
    Result := Self
  else
  begin
    LChild := FindChild(APath[AIndex], ACreateMissingNodes);
    if Assigned(LChild) then
      Result := LChild.FindNodeFrom(APath, Succ(AIndex), ACreateMissingNodes)
    else
      Result := nil;
  end;
end;

function TEFNode.EqualsNode(const ANode: TEFNode): Boolean;
begin
  if Assigned(ANode)
      and (ANode.Name = FName)
      and ((ANode.IsNull and IsNull) or (ANode.Value = FValue)) then
    Result := True
  else
    Result := False;
end;

function TEFNode.EqualsValue(const AValue: Variant): Boolean;
begin
  Result := Value = AValue;
end;

{ TEFTree }

procedure TEFTree.AddFieldsAsChildren(const AFields: TFields);
var
  I: Integer;
begin
  Assert(Assigned(AFields));

  for I := 0 to AFields.Count - 1 do
    GetNode(AFields[I].FieldName, True).AssignFieldValue(AFields[I]);
end;

function TEFTree.AddChild(const AName: string; const AValue: Variant): TEFNode;
begin
  Result := AddChild(GetChildClass(AName).Create(AName, AValue));
end;

function TEFTree.AddChild(const ANode: TEFNode): TEFNode;
begin
  Assert(Assigned(ANode));

  ANode.FParent := Self;
  FNodes.Add(ANode);
  Result := ANode;
end;

function TEFTree.AddChild(const AName: string): TEFNode;
begin
  Result := AddChild(GetChildClass(AName).Create(AName));
end;

procedure TEFTree.Assign(const ASource: TEFTree);
var
  LNode: TEFNode;
begin
  Clear;
  if Assigned(ASource) then
    for LNode in ASource.FNodes do
      AddChild(GetChildClass(LNode.Name).Clone(LNode));
end;

class function TEFTree.BooleanToValue(const ABoolean: Boolean): Variant;
begin
  Result := ABoolean;
end;

procedure TEFTree.Clear;
begin
  ClearChildren;
end;

procedure TEFTree.ClearChildren;
begin
  FNodes.Clear;
end;

constructor TEFTree.Clone(const ASource: TEFTree);
begin
  Create;
  Assign(ASource);
end;

constructor TEFTree.Create;
begin
  inherited Create;
  FNodes := TEFNodes.Create(True);
end;

class function TEFTree.CurrencyToValue(const ACurrency: Currency): Variant;
begin
  Result := ACurrency;
end;

procedure TEFTree.RemoveChild(const ANode: TEFNode);
begin
  FNodes.Remove(ANode);
end;

class function TEFTree.DateTimeToValue(const ADateTime: TDateTime): Variant;
begin
  Result := ADateTime;
end;

class function TEFTree.DateToValue(const ADate: TDate): Variant;
begin
  Result := Trunc(ADate);
end;

class function TEFTree.DecimalToValue(const ADecimal: TBcd): Variant;
begin
  Result := BcdToDouble(ADecimal);
end;

procedure TEFTree.DeleteNode(const APath: string);
var
  LNode: TEFNode;
begin
  LNode := FindNode(APath);
  if LNode <> nil then
    LNode.Delete;
end;

destructor TEFTree.Destroy;
begin
  FreeAndNil(FNodes);
  FreeAndNil(FCriticalSection);
  inherited;
end;

function TEFTree.GetChild(I: Integer): TEFNode;
begin
  Result := FNodes[I];
end;

function TEFTree.GetChild<T>(const AIndex: Integer): T;
var
  I: Integer;
  LTIndex: Integer;
begin
  Result := nil;
  LTIndex := 0;
  for I := 0 to ChildCount - 1 do
  begin
    // Don't use "is" here. Either a bug in generics implementation or as designed.
    if Children[I].InheritsFrom(T) then
    begin
      if LTIndex = AIndex then
      begin
        Result := T(Children[I]);
        Break;
      end;
      Inc(LTIndex);
    end;
  end;
end;

function TEFTree.FindChild(const AName: string; const ACreateMissingNodes: Boolean): TEFNode;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to ChildCount - 1 do
  begin
    if SameText(Children[I].Name, AName) then
    begin
      Result := Children[I];
      Break;
    end;
  end;
  if (Result = nil) and ACreateMissingNodes then
    Result := AddChild(AName, '');
end;

function TEFTree.ChildByName(const AName: string): TEFNode;
begin
  Result := FindChild(AName);
  if not Assigned(Result) then
    raise EEFError.CreateFmt(_('Node %s not found.'), [AName]);
end;

function TEFTree.GetChildClass(const AName: string): TEFNodeClass;
begin
  Result := TEFNode;
end;

function TEFTree.GetChildCount: Integer;
begin
  Result := FNodes.Count;
end;

function TEFTree.GetChildCount<T>: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to ChildCount - 1 do
  begin
    // Don't use "is" here. Either a bug in generics implementation or as designed.
    if Children[I].InheritsFrom(T) then
      Inc(Result);
  end;
end;

function TEFTree.GetChildrenAsPairs(const APath: string;
  const ADefaultValue: TEFPairs): TEFPairs;
var
  LNode: TEFNode;
begin
  LNode := FindNode(APath);
  if Assigned(LNode) then
    Result := LNode.GetChildPairs
  else
    Result := ADefaultValue;
end;

function TEFTree.FindNode(const APath: string; const ACreateMissingNodes: Boolean): TEFNode;
var
  LPath: TStringDynArray;
  LChild: TEFNode;
begin
  LPath := Split(APath, '/');
  if Length(LPath) = 0 then
    Result := nil
  else
  begin
    EnterCS;
    try
      LChild := FindChild(LPath[0], ACreateMissingNodes);
      if Assigned(LChild) then
      begin
        if Length(LPath) = 1 then
          Result := LChild
        else
          Result := LChild.FindNodeFrom(LPath, 1, ACreateMissingNodes);
      end
      else
        Result := nil;
    finally
      LeaveCS;
    end;
  end;
end;

class function TEFTree.FloatToValue(const AFloat: Double): Variant;
begin
  Result := AFloat;
end;

function TEFTree.GetNode(const APath: string; const ACreateMissingNodes: Boolean): TEFNode;
begin
  Result := FindNode(APath, ACreateMissingNodes);
  if not Assigned(Result) then
    raise EEFError.CreateFmt(_('Node %s not found.'), [APath]);
end;

function TEFTree.GetBoolean(const APath: string;
  const ADefaultValue: Boolean): Boolean;
var
  LNode: TEFNode;
begin
  LNode := FindNode(APath);
  if Assigned(LNode) then
    Result := LNode.AsBoolean
  else
    Result := ADefaultValue;
end;

function TEFTree.GetObject(const APath: string;
  const ADefaultValue: TObject): TObject;
var
  LNode: TEFNode;
begin
  LNode := FindNode(APath);
  if Assigned(LNode) then
    Result := LNode.AsObject
  else
    Result := ADefaultValue;
end;

function TEFTree.GetPairs(const APath: string;
  const ADefaultValue: TEFPairs): TEFPairs;
var
  LNode: TEFNode;
begin
  LNode := FindNode(APath);
  if Assigned(LNode) then
    Result := LNode.AsPairs
  else
    Result := ADefaultValue;
end;

function TEFTree.GetString(const APath, ADefaultValue: string): string;
var
  LNode: TEFNode;
begin
  LNode := FindNode(APath);
  if Assigned(LNode) then
    Result := LNode.AsString
  else
    Result := ADefaultValue;
end;

function TEFTree.GetStringArray(const APath: string;
  const ADefaultValue: TStringDynArray): TStringDynArray;
var
  LNode: TEFNode;
begin
  LNode := FindNode(APath);
  if Assigned(LNode) then
    Result := LNode.AsStringArray
  else
    Result := ADefaultValue;
end;

function TEFTree.GetValue(const APath: string): Variant;
begin
  Result := GetValue(APath, Null);
end;

function TEFTree.GetValue(const APath: string;
  const ADefaultValue: Variant): Variant;
var
  LNode: TEFNode;
begin
  LNode := FindNode(APath);
  if Assigned(LNode) then
    Result := LNode.Value
  else
    Result := ADefaultValue;
end;

function TEFTree.GetExpandedString(const APath, ADefaultValue: string): string;
begin
  Result := Environment.MacroExpansionEngine.Expand(GetString(APath, ADefaultValue));
end;

function TEFTree.GetChildrenAsStrings(const APath, ASeparator, AConnector,
  ADefaultValue: string): string;
var
  LNode: TEFNode;
begin
  LNode := FindNode(APath);
  if Assigned(LNode) then
    Result := LNode.GetChildStrings(ASeparator, AConnector, ADefaultValue)
  else
    Result := ADefaultValue;
end;

function TEFTree.GetDate(const APath: string;
  const ADefaultValue: TDate): TDate;
var
  LNode: TEFNode;
begin
  LNode := FindNode(APath);
  if Assigned(LNode) then
    Result := LNode.AsDate
  else
    Result := ADefaultValue;
end;

class function TEFTree.IntegerToValue(const AInteger: Integer): Variant;
begin
  Result := AInteger;
end;

class function TEFTree.ObjectToValue(const AObject: TObject): Variant;
begin
  Result := NativeInt(Pointer(AObject));
end;

class function TEFTree.PairsToValue(const APairs: TEFPairs): Variant;
begin
  Result := JoinPairs(APairs, ' ');
end;

function TEFTree.GetExpandedStrings(const APath, ASeparator, AConnector,
  ADefaultValue: string): string;
var
  LNode: TEFNode;
begin
  LNode := FindNode(APath);
  if Assigned(LNode) then
    Result := LNode.GetExpandedChildStrings(ASeparator, AConnector, ADefaultValue)
  else
    Result := ADefaultValue;
end;

function TEFTree.GetInteger(const APath: string;
  const ADefaultValue: Integer): Integer;
var
  LNode: TEFNode;
begin
  LNode := FindNode(APath);
  if Assigned(LNode) then
    Result := LNode.AsInteger
  else
    Result := ADefaultValue;
end;

class function TEFTree.ValueToBoolean(const AValue: Variant): Boolean;
begin
  Result := AValue;
end;

class function TEFTree.ValueToCurrency(const AValue: Variant): Currency;
begin
  Result := AValue;
end;

class function TEFTree.ValueToDate(const AValue: Variant): TDate;
begin
  Result := AValue;
end;

class function TEFTree.ValueToDateTime(const AValue: Variant): TDateTime;
begin
  Result := AValue;
end;

class function TEFTree.ValueToDecimal(const AValue: Variant): TBcd;
begin
  Result := VarToBcd(AValue);
end;

class function TEFTree.ValueToFloat(const AValue: Variant): Double;
begin
  Result := AValue;
end;

class function TEFTree.ValueToInteger(const AValue: Variant): Integer;
begin
  Result := AValue;
end;

class function TEFTree.ValueToObject(const AValue: Variant): TObject;
begin
  Result := TObject(NativeInt(AValue));
end;

class function TEFTree.ValueToPairs(const AValue: Variant): TEFPairs;
begin
  Result := SplitPairs(AValue, ' ');
end;

class function TEFTree.ValueToString(const AValue: Variant): string;
begin
  Result := EFVarToStr(AValue);
end;

class function TEFTree.ValueToStringArray(const AValue: Variant): TStringDynArray;
begin
  Result := Split(AValue, ' ');
end;

class function TEFTree.ValueToTime(const AValue: Variant): TTime;
begin
  Result := AValue;
end;

procedure TEFTree.SetBoolean(const APath: string; const AValue: Boolean);
begin
  GetNode(APath, True).AsBoolean := AValue;
end;

procedure TEFTree.SetInteger(const APath: string; const AValue: Integer);
begin
  GetNode(APath, True).AsInteger := AValue;
end;

procedure TEFTree.SetObject(const APath: string; const AValue: TObject);
begin
  GetNode(APath, True).AsObject := AValue;
end;

procedure TEFTree.SetString(const APath, AValue: string);
begin
  GetNode(APath, True).AsString := AValue;
end;

class function TEFTree.StringArrayToValue(const AStringArray: TStringDynArray): Variant;
begin
  Result := Join(AStringArray, ' ');
end;

class function TEFTree.StringToValue(const AString: string): Variant;
begin
  Result := AString;
end;

class function TEFTree.TimeToValue(const ATime: TTime): Variant;
begin
  Result := Frac(ATime);
end;

procedure TEFTree.EnterCS;
begin
  if not Assigned(FCriticalSection) then
    FCriticalSection := TCriticalSection.Create;
  FCriticalSection.Enter;
end;

procedure TEFTree.LeaveCS;
begin
  Assert(Assigned(FCriticalSection));
  FCriticalSection.Leave;
end;

{ TEFTreeFactory }

class function TEFTreeFactory.LoadFromFile<T>(const AFileName: string): T;
var
  LReader: TEFYAMLReader;
begin
  LReader := TEFYAMLReader.Create;
  try
    Result := T.Create;
    LReader.LoadTreeFromFile(Result, AFileName);
  finally
    FreeAndNil(LReader);
  end;
end;

{ TEFTreeMacroExpander }

constructor TEFTreeMacroExpander.Create(const ATree: TEFTree;
  const ANameSpace: string);
begin
  inherited Create;
  FTree := ATree;
  FPrefix := ANameSpace;
  if Trim(FPrefix) <> '' then
    FPrefix := FPrefix + ':'
  else
    FPrefix := '';
end;

function TEFTreeMacroExpander.InternalExpand(const AString: string): string;
var
  LIndex: Integer;
  LStart: Integer;
  LPathStart: Integer;
  LEnd: Integer;
  LNodePath: string;
  LNodeValue: string;
begin
  Result := inherited InternalExpand(AString);

  LIndex := 1;
  repeat
    LStart := PosEx('%' + FPrefix, Result, LIndex);
    if LStart = 0 then
      Exit;
    LPathStart := LStart + 1 + Length(FPrefix);
    LEnd := PosEx('%', Result, LStart + 1);
    if LEnd = 0 then
      Exit;
    LNodePath := Copy(Result, LPathStart, LEnd - LPathStart);
    LNodeValue := FTree.GetExpandedString(StripPrefixAndSuffix(LNodePath, '%', '%'));
    if LNodeValue <> ''  then
    begin
      Delete(Result, LStart, LEnd - LStart + 1);
      Insert(LNodeValue, Result, LStart);
    end;
    LIndex := LEnd + 1;
  until LIndex > Length(Result);
end;

end.
