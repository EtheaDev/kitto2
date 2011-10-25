unit EF.Tree;

interface

uses
  SysUtils, Types, Variants, DB, FmtBcd, Generics.Collections, SyncObjs,
  EF.Types, EF.Macros;

type
  TEFNode = class;

  TEFDataType = class
  protected
    procedure InternalNodeToParam(const ANode: TEFNode; const AParam: TParam); virtual;
    procedure InternalFieldValueToNode(const AField: TField; const ANode: TEFNode); virtual;
    procedure InternalYamlValueToNode(const AYamlValue: string; const ANode: TEFNode;
      const AFormatSettings: TFormatSettings); virtual;
    function InternalNodeToJSONValue(const ANode: TEFNode; const AJSFormatSettings: TFormatSettings): string; virtual;
  public
    class function GetTypeName: string; virtual;

    procedure FieldValueToNode(const AField: TField; const ANode: TEFNode);
    procedure NodeToParam(const ANode: TEFNode; const AParam: TParam);
    procedure YamlValueToNode(const AYamlValue: string; const ANode: TEFNode;
      const AFormatSettings: TFormatSettings);
    function GetDefaultDisplayWidth(const ASize: Integer): Integer; virtual;
    function SupportsEmptyAsNull: Boolean; virtual;
    function IsBlob(const ASize: Integer): Boolean; virtual;
    function NodeToJSONValue(const ANode: TEFNode; const AJSFormatSettings: TFormatSettings): string; virtual;
    function GetJSTypeName: string; virtual;
  end;
  TEFDataTypeClass = class of TEFDataType;

  TEFStringDataType = class(TEFDataType)
  protected
    procedure InternalNodeToParam(const ANode: TEFNode; const AParam: TParam); override;
    procedure InternalFieldValueToNode(const AField: TField; const ANode: TEFNode); override;
    procedure InternalYamlValueToNode(const AYamlValue: string; const ANode: TEFNode;
      const AFormatSettings: TFormatSettings); override;
  public
    function GetDefaultDisplayWidth(const ASize: Integer): Integer; override;
    function SupportsEmptyAsNull: Boolean; override;
    function IsBlob(const ASize: Integer): Boolean; override;
    function GetJSTypeName: string; override;
  end;

  TEFMemoDataType = class(TEFStringDataType)
  end;

  TEFDateDataType = class(TEFDataType)
  protected
    procedure InternalNodeToParam(const ANode: TEFNode; const AParam: TParam); override;
    procedure InternalFieldValueToNode(const AField: TField; const ANode: TEFNode); override;
    procedure InternalYamlValueToNode(const AYamlValue: string; const ANode: TEFNode;
      const AFormatSettings: TFormatSettings); override;
  public
    function GetDefaultDisplayWidth(const ASize: Integer): Integer; override;
    function SupportsEmptyAsNull: Boolean; override;
    function InternalNodeToJSONValue(const ANode: TEFNode; const AJSFormatSettings: TFormatSettings): string; override;
    function GetJSTypeName: string; override;
  end;

  TEFTimeDataType = class(TEFDataType)
  protected
    procedure InternalNodeToParam(const ANode: TEFNode; const AParam: TParam); override;
    procedure InternalFieldValueToNode(const AField: TField; const ANode: TEFNode); override;
    procedure InternalYamlValueToNode(const AYamlValue: string; const ANode: TEFNode;
      const AFormatSettings: TFormatSettings); override;
  public
    function GetDefaultDisplayWidth(const ASize: Integer): Integer; override;
    function SupportsEmptyAsNull: Boolean; override;
    function InternalNodeToJSONValue(const ANode: TEFNode; const AJSFormatSettings: TFormatSettings): string; override;
  end;

  TEFDateTimeDataType = class(TEFDataType)
  protected
    procedure InternalNodeToParam(const ANode: TEFNode; const AParam: TParam); override;
    procedure InternalFieldValueToNode(const AField: TField; const ANode: TEFNode); override;
    procedure InternalYamlValueToNode(const AYamlValue: string; const ANode: TEFNode;
      const AFormatSettings: TFormatSettings); override;
  public
    function GetDefaultDisplayWidth(const ASize: Integer): Integer; override;
    function SupportsEmptyAsNull: Boolean; override;
    function InternalNodeToJSONValue(const ANode: TEFNode; const AJSFormatSettings: TFormatSettings): string; override;
    function GetJSTypeName: string; override;
  end;

  TEFBooleanDataType = class(TEFDataType)
  protected
    procedure InternalNodeToParam(const ANode: TEFNode; const AParam: TParam); override;
    procedure InternalFieldValueToNode(const AField: TField; const ANode: TEFNode); override;
    procedure InternalYamlValueToNode(const AYamlValue: string; const ANode: TEFNode;
      const AFormatSettings: TFormatSettings); override;
  public
    function GetDefaultDisplayWidth(const ASize: Integer): Integer; override;
    function InternalNodeToJSONValue(const ANode: TEFNode; const AJSFormatSettings: TFormatSettings): string; override;
    function GetJSTypeName: string; override;
  end;

  TEFNumericDataTypeBase = class(TEFDataType);

  TEFIntegerDataType = class(TEFNumericDataTypeBase)
  protected
    procedure InternalNodeToParam(const ANode: TEFNode; const AParam: TParam); override;
    procedure InternalFieldValueToNode(const AField: TField; const ANode: TEFNode); override;
    procedure InternalYamlValueToNode(const AYamlValue: string; const ANode: TEFNode;
      const AFormatSettings: TFormatSettings); override;
  public
    function GetDefaultDisplayWidth(const ASize: Integer): Integer; override;
    function GetJSTypeName: string; override;
  end;

  TEFDecimalNumericDataTypeBase = class(TEFDataType);

  TEFCurrencyDataType = class(TEFDecimalNumericDataTypeBase)
  protected
    procedure InternalNodeToParam(const ANode: TEFNode; const AParam: TParam); override;
    procedure InternalFieldValueToNode(const AField: TField; const ANode: TEFNode); override;
    procedure InternalYamlValueToNode(const AYamlValue: string; const ANode: TEFNode;
      const AFormatSettings: TFormatSettings); override;
  public
    function GetDefaultDisplayWidth(const ASize: Integer): Integer; override;
    function SupportsEmptyAsNull: Boolean; override;
    function InternalNodeToJSONValue(const ANode: TEFNode; const AJSFormatSettings: TFormatSettings): string; override;
    function GetJSTypeName: string; override;
  end;

  TEFFloatDataType = class(TEFDecimalNumericDataTypeBase)
  protected
    procedure InternalNodeToParam(const ANode: TEFNode; const AParam: TParam); override;
    procedure InternalFieldValueToNode(const AField: TField; const ANode: TEFNode); override;
    procedure InternalYamlValueToNode(const AYamlValue: string; const ANode: TEFNode;
      const AFormatSettings: TFormatSettings); override;
  public
    function GetDefaultDisplayWidth(const ASize: Integer): Integer; override;
    function SupportsEmptyAsNull: Boolean; override;
    function InternalNodeToJSONValue(const ANode: TEFNode; const AJSFormatSettings: TFormatSettings): string; override;
    function GetJSTypeName: string; override;
  end;

  TEFDecimalDataType = class(TEFDecimalNumericDataTypeBase)
  protected
    procedure InternalNodeToParam(const ANode: TEFNode; const AParam: TParam); override;
    procedure InternalFieldValueToNode(const AField: TField; const ANode: TEFNode); override;
    procedure InternalYamlValueToNode(const AYamlValue: string; const ANode: TEFNode;
      const AFormatSettings: TFormatSettings); override;
  public
    function GetDefaultDisplayWidth(const ASize: Integer): Integer; override;
    function SupportsEmptyAsNull: Boolean; override;
    function InternalNodeToJSONValue(const ANode: TEFNode; const AJSFormatSettings: TFormatSettings): string; override;
    function GetJSTypeName: string; override;
  end;

  TEFObjectDataType = class(TEFDataType)
  protected
    procedure InternalFieldValueToNode(const AField: TField; const ANode: TEFNode); override;
    procedure InternalYamlValueToNode(const AYamlValue: string; const ANode: TEFNode;
      const AFormatSettings: TFormatSettings); override;
  public
    function GetDefaultDisplayWidth(const ASize: Integer): Integer; override;
  end;

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
    function GetRoot: TEFTree; virtual;
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
    function GetRoot: TEFTree; override;
  public
    procedure Assign(const ASource: TEFTree); override;
    procedure AssignValue(const ASource: TEFNode);
    property Parent: TEFTree read FParent;
    property Root: TEFTree read GetRoot;
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
    procedure SetToNull; virtual;
    function EqualsNode(const ANode: TEFNode): Boolean;
    function EqualsValue(const AValue: Variant): Boolean;

    function GetChildStrings(const ASeparator: string = sLineBreak;
      const AConnector: string = '='; const ADefaultValue: string = ''): string; overload;
    function GetChildNames: TStringDynArray;
    function GetExpandedChildStrings(const ASeparator, AConnector,
      ADefaultValue: string): string;
    function GetChildPairs: TEFPairs;

    procedure AssignFieldValue(const AField: TField);
    procedure AssignValueToParam(const AParam: TParam);
    procedure AssignToParam(const AParam: TParam);
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

type
  TEFDataTypeRegistry = class(TEFRegistry)
  private
    class var FInstance: TEFDataTypeRegistry;
    class function GetInstance: TEFDataTypeRegistry; static;
    class destructor Destroy;
  public
    class property Instance: TEFDataTypeRegistry read GetInstance;
    function GetClass(const AId: string): TEFDataTypeClass;
  end;

  TEFDataTypeFactory = class(TEFFactory)
  private
    FDataTypes: TDictionary<string, TEFDataType>;
    class var FInstance: TEFDataTypeFactory;
    class function GetInstance: TEFDataTypeFactory; static;
  public
    class destructor Destroy;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
    class property Instance: TEFDataTypeFactory read GetInstance;

    function GetDataType(const AId: string): TEFDataType;
  end;

implementation

uses
  StrUtils, TypInfo, Math,
  EF.Environment, EF.Localization, EF.StrUtils, EF.YAML, EF.VariantUtils;

{$IF RTLVersion < 23.0}
const
  varObject = $0049;
{$IFEND}

{ TEFDataTypeRegistry }

class destructor TEFDataTypeRegistry.Destroy;
begin
  FreeAndNil(FInstance);
end;

function TEFDataTypeRegistry.GetClass(const AId: string): TEFDataTypeClass;
begin
  Result := TEFDataTypeClass(inherited GetClass(AId));
end;

class function TEFDataTypeRegistry.GetInstance: TEFDataTypeRegistry;
begin
  if FInstance = nil then
    FInstance := TEFDataTypeRegistry.Create;
  Result := FInstance;
end;

{ TEFDataTypeFactory }

procedure TEFDataTypeFactory.AfterConstruction;
begin
  inherited;
  FDataTypes := TDictionary<string, TEFDataType>.Create;
end;

destructor TEFDataTypeFactory.Destroy;
begin
  FreeAndNil(FDataTypes);
  inherited;
end;

class destructor TEFDataTypeFactory.Destroy;
begin
  FreeAndNil(FInstance);
end;

function TEFDataTypeFactory.GetDataType(const AId: string): TEFDataType;
begin
  if not FDataTypes.ContainsKey(AId) then
    FDataTypes.Add(AId, TEFDataType(CreateObject(AId)));
  Result := FDataTypes[AId];
end;

class function TEFDataTypeFactory.GetInstance: TEFDataTypeFactory;
begin
  if FInstance = nil then
    FInstance := TEFDataTypeFactory.Create(TEFDataTypeRegistry.Instance);
  Result := FInstance;
end;

function GetVariantDataType(const AVariant: Variant): TEFDataType;
begin
  if VarIsType(AVariant, [varString, varOleStr, varUString]) then
    Result := TEFDataTypeFactory.Instance.GetDataType('String')
  else if VarIsType(AVariant, [varShortInt, varByte, varWord, varLongWord, varSmallint, varInteger, varInt64, varUInt64]) then
    Result := TEFDataTypeFactory.Instance.GetDataType('Integer')
  else if VarIsType(AVariant, [varDate]) then
  begin
    if TDateTime(AVariant) = Trunc(TDateTime(AVariant)) then
      Result := TEFDataTypeFactory.Instance.GetDataType('Date')
    else if TDateTime(AVariant) = Frac(TDateTime(AVariant)) then
      Result := TEFDataTypeFactory.Instance.GetDataType('Time')
    else
      Result := TEFDataTypeFactory.Instance.GetDataType('DateTime');
  end
  else if VarIsType(AVariant, [varBoolean]) then
    Result := TEFDataTypeFactory.Instance.GetDataType('Boolean')
  else if VarIsType(AVariant, [varCurrency]) then
    Result := TEFDataTypeFactory.Instance.GetDataType('Currency')
  else if VarIsFMTBcd(AVariant) then
    Result := TEFDataTypeFactory.Instance.GetDataType('Decimal')
  else if VarIsType(AVariant, [varSingle, varDouble]) then
    Result := TEFDataTypeFactory.Instance.GetDataType('Float')
  else if VarIsType(AVariant, [varObject]) then
    Result := TEFDataTypeFactory.Instance.GetDataType('Object')
  else
    Result := TEFDataTypeFactory.Instance.GetDataType('String');
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

procedure TEFNode.AssignValueToParam(const AParam: TParam);
begin
  Assert(Assigned(AParam));

  DataType.NodeToParam(Self, AParam);
end;

procedure TEFNode.AssignFieldValue(const AField: TField);
begin
  Assert(Assigned(AField));

  DataType.FieldValueToNode(AField, Self);
end;

procedure TEFNode.AssignToParam(const AParam: TParam);
begin
  AssignValueToParam(AParam);
  AParam.Name := Name;
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

function TEFNode.GetChildNames: TStringDynArray;
var
  I: Integer;
begin
  SetLength(Result, ChildCount);
  for I := 0 to ChildCount - 1 do
    Result[I] := Children[I].Name;
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

function TEFNode.GetRoot: TEFTree;
begin
  if Assigned(Parent) then
    Result := Parent.GetRoot
  else
    Result := inherited GetRoot;
end;

function TEFNode.GetValue: Variant;
begin
  Result := FValue;
end;

procedure TEFNode.SetAsBoolean(const AValue: Boolean);
begin
  Value := BooleanToValue(AValue);
  FDataType := TEFDataTypeFactory.Instance.GetDataType('Boolean');
end;

procedure TEFNode.SetAsCurrency(const AValue: Currency);
begin
  Value := CurrencyToValue(AValue);
  FDataType := TEFDataTypeFactory.Instance.GetDataType('Currency');
end;

procedure TEFNode.SetAsDate(const AValue: TDate);
begin
  Value := DateToValue(AValue);
  FDataType := TEFDataTypeFactory.Instance.GetDataType('Date');
end;

procedure TEFNode.SetAsDateTime(const AValue: TDateTime);
begin
  Value := DateTimeToValue(AValue);
  FDataType := TEFDataTypeFactory.Instance.GetDataType('DateTime');
end;

procedure TEFNode.SetAsDecimal(const AValue: TBcd);
begin
  Value := DecimalToValue(AValue);
  FDataType := TEFDataTypeFactory.Instance.GetDataType('Decimal');
end;

procedure TEFNode.SetAsFloat(const AValue: Double);
begin
  Value := FloatToValue(AValue);
  FDataType := TEFDataTypeFactory.Instance.GetDataType('Float');
end;

procedure TEFNode.SetAsInteger(const AValue: Integer);
begin
  Value := IntegerToValue(AValue);
  FDataType := TEFDataTypeFactory.Instance.GetDataType('Integer');
end;

procedure TEFNode.SetAsObject(const AValue: TObject);
begin
  Value := ObjectToValue(AValue);
  FDataType := TEFDataTypeFactory.Instance.GetDataType('Object');
end;

procedure TEFNode.SetAsPair(const AValue: TEFPair);
begin
  SetName(AValue.Key);
  AsString := AValue.Value;
end;

procedure TEFNode.SetAsPairs(const AValue: TEFPairs);
begin
  Value := PairsToValue(AValue);
  FDataType := TEFDataTypeFactory.Instance.GetDataType('String');
end;

procedure TEFNode.SetAsString(const AValue: string);
begin
  Value := StringToValue(AValue);
  FDataType := TEFDataTypeFactory.Instance.GetDataType('String');
end;

procedure TEFNode.SetAsStringArray(const AValue: TStringDynArray);
begin
  Value := StringArrayToValue(AValue);
  FDataType := TEFDataTypeFactory.Instance.GetDataType('String');
end;

procedure TEFNode.SetAsTime(const AValue: TTime);
begin
  Value := TimeToValue(AValue);
  FDataType := TEFDataTypeFactory.Instance.GetDataType('Time');
end;

function TEFNode.SetAsYamlValue(const AValue: string; const AFormatSettings: TFormatSettings): TEFNode;
begin
  DataType.YamlValueToNode(AValue, Self, AFormatSettings);
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

function TEFTree.GetRoot: TEFTree;
begin
  Result := Self;
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

{ TEFDataType }

procedure TEFDataType.FieldValueToNode(const AField: TField; const ANode: TEFNode);
begin
  Assert(Assigned(AField));
  Assert(Assigned(ANode));

  if AField.IsNull then
    ANode.SetToNull
  else
    InternalFieldValueToNode(AField, ANode);
end;

procedure TEFDataType.InternalFieldValueToNode(const AField: TField;
  const ANode: TEFNode);
begin
  case AField.DataType of
    ftString, ftMemo, ftFixedChar, ftWideString, ftWideMemo: ANode.AsString := AField.AsString;
    ftSmallint, ftWord, ftInteger, ftAutoInc: ANode.AsInteger := AField.AsInteger;
    ftBoolean: ANode.AsBoolean := AField.AsBoolean;
    ftDate: ANode.AsDate := AField.AsDateTime;
    ftTime: ANode.AsTime := AField.AsDateTime;
    ftDateTime, ftTimeStamp: ANode.AsDateTime := AField.AsDateTime;
    ftCurrency: ANode.AsCurrency := AField.AsCurrency;
    ftFloat: ANode.AsFloat := AField.AsFloat;
    ftBCD, ftFMTBcd: ANode.AsDecimal := AField.AsBCD;
  else
    raise EEFError.CreateFmt('TEFDataType.InternalFieldValueToNode: Field data type %s not supported.',
      [GetEnumName(TypeInfo(TFieldType), Ord(AField.DataType))]);
  end;
end;

function TEFDataType.InternalNodeToJSONValue(const ANode: TEFNode;
  const AJSFormatSettings: TFormatSettings): string;
begin
  Result := ANode.AsString;
end;

procedure TEFDataType.InternalNodeToParam(const ANode: TEFNode;
  const AParam: TParam);
begin
  raise EEFError.CreateFmt('%s.InternalNodeToParam: Unsupported call.', [ClassName]);
end;

procedure TEFDataType.InternalYamlValueToNode(const AYamlValue: string;
  const ANode: TEFNode; const AFormatSettings: TFormatSettings);
var
  LInteger: Integer;
  LDouble: Double;
  LDateTime: TDateTime;
  LBoolean: Boolean;
begin
  Assert(Assigned(ANode));

  if TryStrToInt(AYamlValue, LInteger) then
    ANode.AsInteger := LInteger
  else if TryStrToFloat(AYamlValue, LDouble, AFormatSettings) then
    ANode.AsFloat := LDouble
  else if TryStrToDateTime(AYamlValue, LDateTime, AFormatSettings) then
    ANode.AsDateTime := LDateTime
  else if TryStrToDate(AYamlValue, LDateTime, AFormatSettings) then
    ANode.AsDate := LDateTime
  else if TryStrToTime(AYamlValue, LDateTime, AFormatSettings) then
    ANode.AsTime := LDateTime
  else if TryStrToBool(AYamlValue, LBoolean) then
    ANode.AsBoolean := LBoolean
  else
    ANode.AsString := AYamlValue;
end;

function TEFDataType.IsBlob(const ASize: Integer): Boolean;
begin
  Result := False;
end;

function TEFDataType.NodeToJSONValue(const ANode: TEFNode;
  const AJSFormatSettings: TFormatSettings): string;
begin
  Assert(Assigned(ANode));

  if ANode.IsNull then
    Result := 'null'
  else
    Result := '"' + InternalNodeToJSONValue(ANode, AJSFormatSettings) + '"';
end;

procedure TEFDataType.NodeToParam(const ANode: TEFNode; const AParam: TParam);
begin
  Assert(Assigned(ANode));
  Assert(Assigned(AParam));

  if ANode.IsNull then
    AParam.Clear
  else
    InternalNodeToParam(ANode, AParam);
end;

function TEFDataType.SupportsEmptyAsNull: Boolean;
begin
  Result := False;
end;

function TEFDataType.GetDefaultDisplayWidth(const ASize: Integer): Integer;
begin
  Result := 20;
end;

function TEFDataType.GetJSTypeName: string;
begin
  Result := 'auto';
end;

class function TEFDataType.GetTypeName: string;
begin
  Result := StripPrefixAndSuffix(ClassName, 'TEF', 'DataType');
end;

procedure TEFDataType.YamlValueToNode(const AYamlValue: string;
  const ANode: TEFNode; const AFormatSettings: TFormatSettings);
begin
  Assert(Assigned(ANode));

  InternalYamlValueToNode(AYamlValue, ANode, AFormatSettings);
end;

{ TEFIntegerDataType }

function TEFIntegerDataType.GetDefaultDisplayWidth(
  const ASize: Integer): Integer;
begin
  Result := 5;
end;

function TEFIntegerDataType.GetJSTypeName: string;
begin
  Result := 'int';
end;

procedure TEFIntegerDataType.InternalFieldValueToNode(const AField: TField;
  const ANode: TEFNode);
begin
  ANode.AsInteger := AField.AsInteger;
end;

procedure TEFIntegerDataType.InternalNodeToParam(const ANode: TEFNode;
  const AParam: TParam);
begin
  AParam.AsInteger := ANode.AsInteger;
end;

procedure TEFIntegerDataType.InternalYamlValueToNode(const AYamlValue: string;
  const ANode: TEFNode; const AFormatSettings: TFormatSettings);
begin
  ANode.AsInteger := StrToInt(AYamlValue);
end;

{ TEFDateDataType }

function TEFDateDataType.GetDefaultDisplayWidth(const ASize: Integer): Integer;
begin
  Result := 10;
end;

function TEFDateDataType.GetJSTypeName: string;
begin
  Result := 'date';
end;

procedure TEFDateDataType.InternalFieldValueToNode(const AField: TField;
  const ANode: TEFNode);
begin
  ANode.AsDate := AField.AsDateTime;
end;

function TEFDateDataType.InternalNodeToJSONValue(const ANode: TEFNode;
  const AJSFormatSettings: TFormatSettings): string;
begin
  Result := DateToStr(ANode.AsDate, AJSFormatSettings);
end;

procedure TEFDateDataType.InternalNodeToParam(const ANode: TEFNode;
  const AParam: TParam);
begin
  AParam.AsDate := ANode.AsDate;
end;

procedure TEFDateDataType.InternalYamlValueToNode(const AYamlValue: string;
  const ANode: TEFNode; const AFormatSettings: TFormatSettings);
begin
  ANode.AsDate := StrToDate(AYamlValue, AFormatSettings);
end;

function TEFDateDataType.SupportsEmptyAsNull: Boolean;
begin
  Result := True;
end;

{ TEFTimeDataType }

function TEFTimeDataType.GetDefaultDisplayWidth(const ASize: Integer): Integer;
begin
  Result := 8;
end;

procedure TEFTimeDataType.InternalFieldValueToNode(const AField: TField;
  const ANode: TEFNode);
begin
  ANode.AsTime := AField.AsDateTime;
end;

function TEFTimeDataType.InternalNodeToJSONValue(const ANode: TEFNode;
  const AJSFormatSettings: TFormatSettings): string;
begin
  Result := TimeToStr(ANode.AsTime, AJSFormatSettings);
end;

procedure TEFTimeDataType.InternalNodeToParam(const ANode: TEFNode;
  const AParam: TParam);
begin
  AParam.AsTime := ANode.AsTime;
end;

procedure TEFTimeDataType.InternalYamlValueToNode(const AYamlValue: string;
  const ANode: TEFNode; const AFormatSettings: TFormatSettings);
begin
  ANode.AsTime := StrToTime(AYamlValue, AFormatSettings);
end;

function TEFTimeDataType.SupportsEmptyAsNull: Boolean;
begin
  Result := True;
end;

{ TEFDateTimeDataType }

function TEFDateTimeDataType.GetDefaultDisplayWidth(
  const ASize: Integer): Integer;
begin
  Result := 19;
end;

function TEFDateTimeDataType.GetJSTypeName: string;
begin
  Result := 'date';
end;

procedure TEFDateTimeDataType.InternalFieldValueToNode(const AField: TField;
  const ANode: TEFNode);
begin
  ANode.AsDateTime := AField.AsDateTime;
end;

function TEFDateTimeDataType.InternalNodeToJSONValue(const ANode: TEFNode;
  const AJSFormatSettings: TFormatSettings): string;
begin
  Result := DateTimeToStr(ANode.AsDateTime, AJSFormatSettings);
end;

procedure TEFDateTimeDataType.InternalNodeToParam(const ANode: TEFNode;
  const AParam: TParam);
begin
  AParam.AsDateTime := ANode.AsDateTime;
end;

procedure TEFDateTimeDataType.InternalYamlValueToNode(const AYamlValue: string;
  const ANode: TEFNode; const AFormatSettings: TFormatSettings);
begin
  ANode.AsDateTime := StrToDateTime(AYamlValue, AFormatSettings);
end;

function TEFDateTimeDataType.SupportsEmptyAsNull: Boolean;
begin
  Result := True;
end;

{ TEFBooleanDataType }

function TEFBooleanDataType.GetDefaultDisplayWidth(
  const ASize: Integer): Integer;
begin
  Result := 8;
end;

function TEFBooleanDataType.GetJSTypeName: string;
begin
  Result := 'boolean';
end;

procedure TEFBooleanDataType.InternalFieldValueToNode(const AField: TField;
  const ANode: TEFNode);
begin
  ANode.AsBoolean := AField.AsBoolean;
end;

function TEFBooleanDataType.InternalNodeToJSONValue(const ANode: TEFNode;
  const AJSFormatSettings: TFormatSettings): string;
begin
  Result := BoolToStr(ANode.AsBoolean, True);
end;

procedure TEFBooleanDataType.InternalNodeToParam(const ANode: TEFNode;
  const AParam: TParam);
begin
  AParam.AsBoolean := ANode.AsBoolean;
end;

procedure TEFBooleanDataType.InternalYamlValueToNode(const AYamlValue: string;
  const ANode: TEFNode; const AFormatSettings: TFormatSettings);
begin
  ANode.AsBoolean := StrToBool(AYamlValue);
end;

{ TEFCurrencyDataType }

function TEFCurrencyDataType.GetDefaultDisplayWidth(
  const ASize: Integer): Integer;
begin
  Result := 12;
end;

function TEFCurrencyDataType.GetJSTypeName: string;
begin
  Result := 'float';
end;

procedure TEFCurrencyDataType.InternalFieldValueToNode(const AField: TField;
  const ANode: TEFNode);
begin
  ANode.AsCurrency := AField.AsCurrency;
end;

function TEFCurrencyDataType.InternalNodeToJSONValue(const ANode: TEFNode;
  const AJSFormatSettings: TFormatSettings): string;
begin
  Result := FormatCurr(',0.00', ANode.AsCurrency, AJSFormatSettings);
end;

procedure TEFCurrencyDataType.InternalNodeToParam(const ANode: TEFNode;
  const AParam: TParam);
begin
  AParam.AsCurrency := ANode.AsCurrency;
end;

procedure TEFCurrencyDataType.InternalYamlValueToNode(const AYamlValue: string;
  const ANode: TEFNode; const AFormatSettings: TFormatSettings);
begin
  ANode.AsCurrency := StrToCurr(AYamlValue);
end;

function TEFCurrencyDataType.SupportsEmptyAsNull: Boolean;
begin
  Result := True;
end;

{ TEFFloatDataType }

function TEFFloatDataType.GetDefaultDisplayWidth(const ASize: Integer): Integer;
begin
  Result := 10;
end;

function TEFFloatDataType.GetJSTypeName: string;
begin
  Result := 'float';
end;

procedure TEFFloatDataType.InternalFieldValueToNode(const AField: TField;
  const ANode: TEFNode);
begin
  ANode.AsFloat := AField.AsFloat;
end;

function TEFFloatDataType.InternalNodeToJSONValue(const ANode: TEFNode;
  const AJSFormatSettings: TFormatSettings): string;
begin
  Result := FormatFloat(',0.00', ANode.AsFloat, AJSFormatSettings);
end;

procedure TEFFloatDataType.InternalNodeToParam(const ANode: TEFNode;
  const AParam: TParam);
begin
  AParam.AsFloat := ANode.AsFloat;
end;

procedure TEFFloatDataType.InternalYamlValueToNode(const AYamlValue: string;
  const ANode: TEFNode; const AFormatSettings: TFormatSettings);
begin
  ANode.AsFloat := StrToFloat(AYamlValue, AFormatSettings);
end;

function TEFFloatDataType.SupportsEmptyAsNull: Boolean;
begin
  Result := True;
end;

{ TEFObjectDataType }

function TEFObjectDataType.GetDefaultDisplayWidth(
  const ASize: Integer): Integer;
begin
  Result := 10;
end;

procedure TEFObjectDataType.InternalFieldValueToNode(const AField: TField;
  const ANode: TEFNode);
begin
  raise EEFError.Create('TEFObjectDataType.InternalFieldValueToNode: Unsupported call.');
end;

procedure TEFObjectDataType.InternalYamlValueToNode(const AYamlValue: string;
  const ANode: TEFNode; const AFormatSettings: TFormatSettings);
begin
  raise EEFError.Create('TEFObjectDataType.InternalYamlValueToNode: Unsupported call.');
end;

{ TEFDecimalDataType }

function TEFDecimalDataType.GetDefaultDisplayWidth(
  const ASize: Integer): Integer;
begin
  Result := 10;
end;

function TEFDecimalDataType.GetJSTypeName: string;
begin
  Result := 'float';
end;

procedure TEFDecimalDataType.InternalFieldValueToNode(const AField: TField;
  const ANode: TEFNode);
begin
  ANode.AsDecimal := AField.AsBCD;
end;

function TEFDecimalDataType.InternalNodeToJSONValue(const ANode: TEFNode;
  const AJSFormatSettings: TFormatSettings): string;
begin
  Result := FormatFloat(',0.00', BcdToDouble(ANode.AsDecimal), AJSFormatSettings);
end;

procedure TEFDecimalDataType.InternalNodeToParam(const ANode: TEFNode;
  const AParam: TParam);
begin
  AParam.AsFMTBCD := ANode.AsDecimal;
end;

procedure TEFDecimalDataType.InternalYamlValueToNode(const AYamlValue: string;
  const ANode: TEFNode; const AFormatSettings: TFormatSettings);
begin
  ANode.AsDecimal := StrToBcd(AYamlValue, AFormatSettings);
end;

function TEFDecimalDataType.SupportsEmptyAsNull: Boolean;
begin
  Result := True;
end;

{ TEFStringDataType }

function TEFStringDataType.GetDefaultDisplayWidth(
  const ASize: Integer): Integer;
begin
  Result := Max(Min(80, ASize), inherited GetDefaultDisplayWidth(ASize));
end;

function TEFStringDataType.GetJSTypeName: string;
begin
  Result := 'string';
end;

procedure TEFStringDataType.InternalFieldValueToNode(const AField: TField;
  const ANode: TEFNode);
begin
  ANode.AsString := AField.AsString;
end;

procedure TEFStringDataType.InternalNodeToParam(const ANode: TEFNode;
  const AParam: TParam);
begin
  AParam.AsString := ANode.AsString;
end;

procedure TEFStringDataType.InternalYamlValueToNode(const AYamlValue: string;
  const ANode: TEFNode; const AFormatSettings: TFormatSettings);
begin
  ANode.AsString := AYamlValue;
end;

function TEFStringDataType.IsBlob(const ASize: Integer): Boolean;
begin
  { TODO : Support binary blobs as well. }
  Result := ASize = 0;
end;

function TEFStringDataType.SupportsEmptyAsNull: Boolean;
begin
  Result := True;
end;

initialization
  TEFDataTypeRegistry.Instance.RegisterClass(TEFStringDataType.GetTypeName, TEFStringDataType);
  TEFDataTypeRegistry.Instance.RegisterClass(TEFMemoDataType.GetTypeName, TEFMemoDataType);
  TEFDataTypeRegistry.Instance.RegisterClass(TEFIntegerDataType.GetTypeName, TEFIntegerDataType);
  TEFDataTypeRegistry.Instance.RegisterClass(TEFDateDataType.GetTypeName, TEFDateDataType);
  TEFDataTypeRegistry.Instance.RegisterClass(TEFTimeDataType.GetTypeName, TEFTimeDataType);
  TEFDataTypeRegistry.Instance.RegisterClass(TEFDateTimeDataType.GetTypeName, TEFDateTimeDataType);
  TEFDataTypeRegistry.Instance.RegisterClass(TEFBooleanDataType.GetTypeName, TEFBooleanDataType);
  TEFDataTypeRegistry.Instance.RegisterClass(TEFFloatDataType.GetTypeName, TEFFloatDataType);
  TEFDataTypeRegistry.Instance.RegisterClass(TEFCurrencyDataType.GetTypeName, TEFCurrencyDataType);
  TEFDataTypeRegistry.Instance.RegisterClass(TEFDecimalDataType.GetTypeName, TEFDecimalDataType);
  TEFDataTypeRegistry.Instance.RegisterClass(TEFObjectDataType.GetTypeName, TEFObjectDataType);

finalization
  TEFDataTypeRegistry.Instance.UnregisterClass(TEFStringDataType.GetTypeName);
  TEFDataTypeRegistry.Instance.UnregisterClass(TEFMemoDataType.GetTypeName);
  TEFDataTypeRegistry.Instance.UnregisterClass(TEFIntegerDataType.GetTypeName);
  TEFDataTypeRegistry.Instance.UnregisterClass(TEFDateDataType.GetTypeName);
  TEFDataTypeRegistry.Instance.UnregisterClass(TEFTimeDataType.GetTypeName);
  TEFDataTypeRegistry.Instance.UnregisterClass(TEFDateTimeDataType.GetTypeName);
  TEFDataTypeRegistry.Instance.UnregisterClass(TEFBooleanDataType.GetTypeName);
  TEFDataTypeRegistry.Instance.UnregisterClass(TEFFloatDataType.GetTypeName);
  TEFDataTypeRegistry.Instance.UnregisterClass(TEFCurrencyDataType.GetTypeName);
  TEFDataTypeRegistry.Instance.UnregisterClass(TEFDecimalDataType.GetTypeName);
  TEFDataTypeRegistry.Instance.UnregisterClass(TEFObjectDataType.GetTypeName);

end.
