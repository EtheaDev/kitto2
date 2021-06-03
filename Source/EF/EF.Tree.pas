{-------------------------------------------------------------------------------
   Copyright 2012-2021 Ethea S.r.l.

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

/// <summary>
///   Defines the tree and node class that implement a tree of named data
///   values. A tree can represent a Yaml file or a JSON stream, for example.
/// </summary>
unit EF.Tree;

{$I EF.Defines.inc}

interface

uses
  SysUtils
  , Types
  , Classes
  , Variants
  , DB
  , FmtBcd
  , Generics.Collections
  , Generics.Defaults
  , Rtti
  , EF.Types
  , EF.Macros
  ;

const
  NODE_NULL_VALUE = 'Null';

type
  TEFNode = class;

  /// <summary>
  ///  Base class for node data types. The system supports pluggable data
  ///  types. Descendants of this class define how to interpret, store, copy,
  ///  encode and decode data of a particular type.
  /// </summary>
  TEFDataType = class
  strict protected
    procedure InternalNodeToParam(const ANode: TEFNode; const AParam: TParam); virtual;
    procedure InternalFieldValueToNode(const AField: TField; const ANode: TEFNode); virtual;
    procedure InternalNodeToField(const ANode: TEFNode; const AField: TField); virtual;
    procedure InternalYamlValueToNode(const AYamlValue: string; const ANode: TEFNode;
      const AFormatSettings: TFormatSettings); virtual; abstract;
    function InternalFormatNodeValue(const AForDisplay: Boolean;
      const ANode: TEFNode; const AFormatSettings: TFormatSettings): string; virtual;
    procedure InternalJSONValueToNode(const ANode: TEFNode;
      const AValue: string; const AUseJSDateFormat: Boolean;
      const AJSFormatSettings: TFormatSettings); virtual;
  public
    class function GetTypeName: string; virtual;
    class function HasSize: Boolean; virtual;
    class function HasScale: Boolean; virtual;
    class function GetFieldType: TFieldType; virtual;
    class function NeedsQuotes: Boolean; virtual;

    class procedure SetNodeDataTypeAndValueFromYaml(const AYamlValue: string;
      const ANode: TEFNode; const AFormatSettings: TFormatSettings;
      const APreferStrings: Boolean);

    procedure FieldValueToNode(const AField: TField; const ANode: TEFNode);
    procedure NodeToField(const ANode: TEFNode; const AField: TField);
    procedure NodeToParam(const ANode: TEFNode; const AParam: TParam);
    procedure YamlValueToNode(const AYamlValue: string; const ANode: TEFNode;
      const AFormatSettings: TFormatSettings);
    function GetDefaultDisplayWidth(const ASize: Integer): Integer; virtual;
    function GetDefaultColumnAlignment: string; virtual;
    function SupportsEmptyAsNull: Boolean; virtual;
    function GetDefaultEmptyAsNull: Boolean; virtual;
    function SupportsJSON: Boolean; virtual;
    function SupportsXML: Boolean; virtual;
    function IsBlob(const ASize: Integer): Boolean; virtual;
    function IsText: Boolean; virtual;
    function IsBoolean: Boolean; virtual;
    function NodeToJSONValue(const AForDisplay: Boolean; const ANode: TEFNode;
      const AJSFormatSettings: TFormatSettings; const AQuote: Boolean = True;
      const AEmptyNulls: Boolean = False): string;
    function NodeToXMLValue(const AForDisplay: Boolean; const ANode: TEFNode;
      const AFormatSettings: TFormatSettings; const AEmptyNulls: Boolean = False): string;
    procedure JSONValueToNode(const ANode: TEFNode; const AValue: string;
      const AUseJSDateFormat: Boolean;
      const AJSFormatSettings: TFormatSettings); virtual;
    function GetJSTypeName: string; virtual;

    function ValueToString(const AValue: Variant): string; virtual;
    function ValueToInteger(const AValue: Variant): Integer; virtual;
    function ValueToObject(const AValue: Variant): TObject; virtual;
    function ValueToBoolean(const AValue: Variant): Boolean; virtual;
    function ValueToStringArray(const AValue: Variant): TStringDynArray; virtual;
    function ValueToPairs(const AValue: Variant): TEFPairs; virtual;
    function ValueToDate(const AValue: Variant): TDate; virtual;
    function ValueToTime(const AValue: Variant): TTime; virtual;
    function ValueToDateTime(const AValue: Variant): TDateTime; virtual;
    function ValueToChar(const AValue: Variant): Char; virtual;
    function ValueToCurrency(const AValue: Variant): Currency; virtual;
    function ValueToFloat(const AValue: Variant): Double; virtual;
    function ValueToDecimal(const AValue: Variant): TBcd; virtual;
    function ValueToBytes(const AValue: Variant): TBytes; virtual;

    function StringToValue(const AString: string): Variant; virtual;
    function IntegerToValue(const AInteger: Integer): Variant; virtual;
    function ObjectToValue(const AObject: TObject): Variant; virtual;
    function BooleanToValue(const ABoolean: Boolean): Variant; virtual;
    function StringArrayToValue(const AStringArray: TStringDynArray): Variant; virtual;
    function PairsToValue(const APairs: TEFPairs): Variant; virtual;
    function DateToValue(const ADate: TDate): Variant; virtual;
    function TimeToValue(const ATime: TTime): Variant; virtual;
    function DateTimeToValue(const ADateTime: TDateTime): Variant; virtual;
    function CurrencyToValue(const ACurrency: Currency): Variant; virtual;
    function FloatToValue(const AFloat: Double): Variant; virtual;
    function DecimalToValue(const ADecimal: TBcd): Variant; virtual;
    function BytesToValue(const ABytes: TBytes): Variant; virtual;
    function CharToValue(const AChar: Char): Variant; virtual;
  end;
  TEFDataTypeClass = class of TEFDataType;

  TEFStringDataType = class(TEFDataType)
  protected
    procedure InternalNodeToParam(const ANode: TEFNode; const AParam: TParam); override;
    procedure InternalFieldValueToNode(const AField: TField; const ANode: TEFNode); override;
    procedure InternalYamlValueToNode(const AYamlValue: string; const ANode: TEFNode;
      const AFormatSettings: TFormatSettings); override;
  public
    class function GetFieldType: TFieldType; override;
    function GetDefaultDisplayWidth(const ASize: Integer): Integer; override;
    function SupportsEmptyAsNull: Boolean; override;
    function IsBlob(const ASize: Integer): Boolean; override;
    function GetJSTypeName: string; override;
    class function HasSize: Boolean; override;
    function IsText: Boolean; override;
    function GetDefaultEmptyAsNull: Boolean; override;
  end;

  TEFMemoDataType = class(TEFStringDataType)
  public
    function IsBlob(const ASize: Integer): Boolean; override;
    class function HasSize: Boolean; override;
  end;

  TEFBlobDataType = class(TEFDataType)
  protected
    procedure InternalNodeToParam(const ANode: TEFNode; const AParam: TParam); override;
  public
    class function GetFieldType: TFieldType; override;
    function IsBlob(const ASize: Integer): Boolean; override;
    function SupportsJSON: Boolean; override;
  end;

  TEFDateTimeDataTypeBase = class(TEFDataType)
  public
    function GetDefaultEmptyAsNull: Boolean; override;
  end;

  TEFDateDataType = class(TEFDateTimeDataTypeBase)
  protected
    procedure InternalNodeToParam(const ANode: TEFNode; const AParam: TParam); override;
    procedure InternalFieldValueToNode(const AField: TField; const ANode: TEFNode); override;
    procedure InternalYamlValueToNode(const AYamlValue: string; const ANode: TEFNode;
      const AFormatSettings: TFormatSettings); override;
    procedure InternalJSONValueToNode(const ANode: TEFNode;
      const AValue: string; const AUseJSDateFormat: Boolean;
      const AJSFormatSettings: TFormatSettings); override;
  public
    class function GetFieldType: TFieldType; override;
    function GetDefaultDisplayWidth(const ASize: Integer): Integer; override;
    function SupportsEmptyAsNull: Boolean; override;
    function InternalFormatNodeValue(const AForDisplay: Boolean;
      const ANode: TEFNode; const AFormatSettings: TFormatSettings): string; override;
    function GetJSTypeName: string; override;
  end;

  TEFTimeDataType = class(TEFDateTimeDataTypeBase)
  protected
    procedure InternalNodeToParam(const ANode: TEFNode; const AParam: TParam); override;
    procedure InternalFieldValueToNode(const AField: TField; const ANode: TEFNode); override;
    procedure InternalYamlValueToNode(const AYamlValue: string; const ANode: TEFNode;
      const AFormatSettings: TFormatSettings); override;
    procedure InternalJSONValueToNode(const ANode: TEFNode;
      const AValue: string; const AUseJSDateFormat: Boolean;
      const AJSFormatSettings: TFormatSettings); override;
  public
    class function GetFieldType: TFieldType; override;
    function GetDefaultDisplayWidth(const ASize: Integer): Integer; override;
    function SupportsEmptyAsNull: Boolean; override;
    function InternalFormatNodeValue(const AForDisplay: Boolean;
      const ANode: TEFNode; const AFormatSettings: TFormatSettings): string; override;
  end;

  TEFDateTimeDataType = class(TEFDateTimeDataTypeBase)
  protected
    procedure InternalNodeToParam(const ANode: TEFNode; const AParam: TParam); override;
    procedure InternalFieldValueToNode(const AField: TField; const ANode: TEFNode); override;
    procedure InternalYamlValueToNode(const AYamlValue: string; const ANode: TEFNode;
      const AFormatSettings: TFormatSettings); override;
    procedure InternalJSONValueToNode(const ANode: TEFNode;
      const AValue: string; const AUseJSDateFormat: Boolean;
      const AJSFormatSettings: TFormatSettings); override;
  public
    class function GetFieldType: TFieldType; override;
    function GetDefaultDisplayWidth(const ASize: Integer): Integer; override;
    function SupportsEmptyAsNull: Boolean; override;
    function InternalFormatNodeValue(const AForDisplay: Boolean;
      const ANode: TEFNode; const AFormatSettings: TFormatSettings): string; override;
    function GetJSTypeName: string; override;
  end;

  TEFBooleanDataType = class(TEFDataType)
  protected
    procedure InternalNodeToParam(const ANode: TEFNode; const AParam: TParam); override;
    procedure InternalFieldValueToNode(const AField: TField; const ANode: TEFNode); override;
    procedure InternalYamlValueToNode(const AYamlValue: string; const ANode: TEFNode;
      const AFormatSettings: TFormatSettings); override;
    procedure InternalJSONValueToNode(const ANode: TEFNode;
      const AValue: string; const AUseJSDateFormat: Boolean;
      const AJSFormatSettings: TFormatSettings); override;
  public
    class function NeedsQuotes: Boolean; override;
    class function GetFieldType: TFieldType; override;
    function GetDefaultDisplayWidth(const ASize: Integer): Integer; override;
    function InternalFormatNodeValue(const AForDisplay: Boolean;
      const ANode: TEFNode; const AFormatSettings: TFormatSettings): string; override;
    function GetJSTypeName: string; override;
    function IsBoolean: Boolean; override;
  end;

  TEFNumericDataTypeBase = class(TEFDataType)
  strict protected
    function StripThousandSeparator(const AValue: string; const AFormatSettings: TFormatSettings): string;
  public
    class function NeedsQuotes: Boolean; override;
  end;

  TEFIntegerDataType = class(TEFNumericDataTypeBase)
  protected
    procedure InternalNodeToParam(const ANode: TEFNode; const AParam: TParam); override;
    procedure InternalFieldValueToNode(const AField: TField; const ANode: TEFNode); override;
    procedure InternalYamlValueToNode(const AYamlValue: string; const ANode: TEFNode;
      const AFormatSettings: TFormatSettings); override;
    procedure InternalJSONValueToNode(const ANode: TEFNode;
      const AValue: string; const AUseJSDateFormat: Boolean;
      const AJSFormatSettings: TFormatSettings); override;
  public
    class function GetFieldType: TFieldType; override;
    function GetDefaultColumnAlignment: string; override;
    function GetDefaultDisplayWidth(const ASize: Integer): Integer; override;
    function GetJSTypeName: string; override;
  end;

  TEFDecimalNumericDataTypeBase = class(TEFNumericDataTypeBase);

  TEFCurrencyDataType = class(TEFDecimalNumericDataTypeBase)
  protected
    procedure InternalNodeToParam(const ANode: TEFNode; const AParam: TParam); override;
    procedure InternalFieldValueToNode(const AField: TField; const ANode: TEFNode); override;
    procedure InternalYamlValueToNode(const AYamlValue: string; const ANode: TEFNode;
      const AFormatSettings: TFormatSettings); override;
    procedure InternalJSONValueToNode(const ANode: TEFNode;
      const AValue: string; const AUseJSDateFormat: Boolean;
      const AJSFormatSettings: TFormatSettings); override;
  public
    class function GetFieldType: TFieldType; override;
    function GetDefaultColumnAlignment: string; override;
    function GetDefaultDisplayWidth(const ASize: Integer): Integer; override;
    function SupportsEmptyAsNull: Boolean; override;
    function InternalFormatNodeValue(const AForDisplay: Boolean;
      const ANode: TEFNode; const AFormatSettings: TFormatSettings): string; override;
    function GetJSTypeName: string; override;
    class function HasSize: Boolean; override;
    class function HasScale: Boolean; override;
  end;

  TEFFloatDataType = class(TEFDecimalNumericDataTypeBase)
  protected
    procedure InternalNodeToParam(const ANode: TEFNode; const AParam: TParam); override;
    procedure InternalFieldValueToNode(const AField: TField; const ANode: TEFNode); override;
    procedure InternalYamlValueToNode(const AYamlValue: string; const ANode: TEFNode;
      const AFormatSettings: TFormatSettings); override;
    procedure InternalJSONValueToNode(const ANode: TEFNode;
      const AValue: string; const AUseJSDateFormat: Boolean;
      const AJSFormatSettings: TFormatSettings); override;
  public
    class function GetFieldType: TFieldType; override;
    function GetDefaultColumnAlignment: string; override;
    function GetDefaultDisplayWidth(const ASize: Integer): Integer; override;
    function SupportsEmptyAsNull: Boolean; override;
    function InternalFormatNodeValue(const AForDisplay: Boolean;
      const ANode: TEFNode; const AFormatSettings: TFormatSettings): string; override;
    function GetJSTypeName: string; override;
  end;

  TEFDecimalDataType = class(TEFDecimalNumericDataTypeBase)
  protected
    procedure InternalNodeToParam(const ANode: TEFNode; const AParam: TParam); override;
    procedure InternalFieldValueToNode(const AField: TField; const ANode: TEFNode); override;
    procedure InternalYamlValueToNode(const AYamlValue: string; const ANode: TEFNode;
      const AFormatSettings: TFormatSettings); override;
    procedure InternalJSONValueToNode(const ANode: TEFNode;
      const AValue: string; const AUseJSDateFormat: Boolean;
      const AJSFormatSettings: TFormatSettings); override;
  public
    class function GetFieldType: TFieldType; override;
    function GetDefaultColumnAlignment: string; override;
    function GetDefaultDisplayWidth(const ASize: Integer): Integer; override;
    function SupportsEmptyAsNull: Boolean; override;
    function InternalFormatNodeValue(const AForDisplay: Boolean;
      const ANode: TEFNode; const AFormatSettings: TFormatSettings): string; override;
    function GetJSTypeName: string; override;
    class function HasSize: Boolean; override;
    class function HasScale: Boolean; override;
  end;

  /// <summary>
  ///   Stores the address of an object. This is only used for in-memory
  ///   transfers, it is not persistable.
  /// </summary>
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

  TEFNodeCompareFunc = TFunc<TEFNode, TEFNode, Integer>;

  /// <summary>
  ///  The root of a tree. Contains a set of nodes which are in turn trees.
  /// </summary>
  TEFTree = class
  strict private
    FNodes: TEFNodes;
    FAnnotations: TStrings;
    class var FRttiContext: TRttiContext;
    function GetChild(I: Integer): TEFNode; overload;
    function GetChildCount: Integer; overload;
    function GetAnnotations: TStrings;
    function GetAnnotation(const AIndex: Integer): string;
    function GetAnnotationCount: Integer;
    procedure SetAnnotation(const AIndex: Integer; const AValue: string);
    procedure DoSetPropertyFromNode(const AType: TRttiType; const AInstance: TObject;
      const ANode: TEFNode; const APath: string; const APathIsName: Boolean = False);
    function GetRttiType(const AClassType: TClass): TRttiType;
  strict protected
    type
      TComparer = class(TComparer<TEFNode>, IComparer<TEFNode>)
      private
        FCompareFunc: TEFNodeCompareFunc;
      public
        constructor Create(const ACompareFunc: TEFNodeCompareFunc);
        function Compare(const Left, Right: TEFNode): Integer; override;
      end;
    function GetChildClass(const AName: string): TEFNodeClass; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure BeforeSave; virtual;
    function QueryInterface(const IID: TGUID; out Obj): HRESULT; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    type
      TAssignNodeProc = reference to procedure (const ASource, ADestination: TEFNode);

    /// <summary>
    ///  Creates a new tree holding a deep copy of the specified tree.
    /// </summary>
    constructor Clone(const ASource: TEFTree; const AProc: TAssignNodeProc = nil); virtual;

    /// <summary>
    ///  Access to the set of children as a list.
    /// </summary>
    property NodeList: TEFNodes read FNodes;

    function GetRoot: TEFTree; virtual;

    /// <summary>
    ///   Clears the tree, recursively deleting all nodes.
    /// </summary>
    procedure Clear; virtual;

    /// <summary>
    ///  Makes the current object a copy of the specified tree.
    /// </summary>
    procedure Assign(const ASource: TEFTree; const AProc: TAssignNodeProc = nil); virtual;

    /// <summary>
    ///   Adds all nodes in ASource, overwriting any existing nodes with the same
    ///   name. Note: does not copy annotations.
    /// </summary>
    procedure Merge(const ASource: TEFTree); virtual;

    /// <summary>
    ///   Adds a child node. Returns a reference to the added object.
    /// </summary>
    function AddChild(const ANode: TEFNode): TEFNode; overload;

    /// <summary>
    ///   Creates a child node of a type that may depend on the parameters,
    ///   fills and adds it. Then returns a reference to the added object.
    /// </summary>
    function AddChild(const AName: string; const AValue: Variant): TEFNode; overload;

    /// <summary>
    ///   Creates a child node of a type that may depend on the parameters,
    ///   sets the name and adds it. Then returns a reference to the added object.
    /// </summary>
    function AddChild(const AName: string): TEFNode; overload;

    /// <summary>
    ///   Returns the count of direct children (of any type).
    /// </summary>
    property ChildCount: Integer read GetChildCount;

    /// <summary>
    ///  Indexed access to the list of direct children of any type.
    /// </summary>
    property Children[I: Integer]: TEFNode read GetChild; default;

    /// <summary>
    ///  Finds a child node by name. Returns nil if not found.
    ///  <param name="AName">Name of the child node to look for.</param>
    ///  <param name="ACreateMissingNode">Creates the node if not found.</param>
    ///  <param name="ARecursively">If true, searches also in child nodes recursively</param>
    /// </summary>
    function FindChild(const AName: string; const ACreateMissingNode: Boolean = False;
      const ARecursively: Boolean = False): TEFNode; overload;

    /// <summary>Finds a child node with specified name and value and returns a
    ///   reference to it, or nil if the node is not found.</summary>
    ///   <param name="AName">Name of the child node to look for.</param>
    ///   <param name="AValue">Value of the child node to look for.</param>
    ///   <param name="ARecursively">If true, searches also in child nodes recursively</param>
    function FindChildByNameAndValue(const AName: string; const AValue: Variant;
      const ARecursively: Boolean = False): TEFNode;

    /// <summary>Finds a child node with specified value and returns a
    ///   reference to it, or nil if the node is not found.</summary>
    ///   <param name="AValue">Value of the child node to look for.</param>
    ///   <param name="ARecursively">If true, searches also in child nodes recursively</param>
    function FindChildByValue(const AValue: Variant; const ARecursively: Boolean = False): TEFNode;

    /// <summary>
    ///  Returns True if a child with the given name exists, and False otherwise.
    /// </summary>
    function HasChild(const AName: string; const ARecursive: Boolean = False): Boolean; overload;

    /// <summary>
    ///  Returns True if the specified child exists, and False otherwise.
    /// </summary>
    function HasChild(const AChild: TEFNode; const ARecursively: Boolean = False): Boolean; overload;

    type
      /// <summary>
      ///  Type used by FindChildByPredicate and EnumChildren.
      /// </summary>
      TNodePredicate = reference to function (const ANode: TEFNode): Boolean;

    /// <summary>
    ///  Finds a child node by predicate. The predicate function is called
    ///  for each child and should return True if a child qualifies.
    ///  The method returns the first qualifying child (and stops as soon
    ///  as it is found).
    ///  If no qualifying child is found, the method return nil.
    ///   <param name="APredicate">Function for testing purpose</param>
    ///   <param name="ARecursively">If true, searches also in child nodes recursively</param>
    /// </summary>
    function FindChildByPredicate(const APredicate: TNodePredicate;
      const ARecursively: Boolean = False): TEFNode;

    type
      /// <summary>
      ///  Type used by EnumChildren.
      /// </summary>
      TNodeProc = reference to procedure (const ANode: TEFNode);

    /// <summary>
    ///  Calls APredicate for each direct child node. If APredicate returns True,
    ///  calls AProc passing the qualifying child node.
    /// </summary>
    procedure EnumChildren(const AProc: TNodeProc; const APredicate: TNodePredicate = nil);

    /// <summary>
    ///   Finds a child node by name. Raises an exception if not found.
    /// </summary>
    function ChildByName(const AName: string): TEFNode;

    /// <summary>
    ///   Removes the child from the list of children, if present.
    /// </summary>
    procedure RemoveChild(const ANode: TEFNode);

    /// <summary>
    ///   Removes all children, recursively.
    /// </summary>
    procedure ClearChildren;

    /// <summary>
    ///  Returns the count of direct children of a specified type.
    /// </summary>
    function GetChildCount<T: class>: Integer; overload;

    /// <summary>
    ///  Indexed access to a list of children limited to the set of children
    ///  that are of the specified type.
    /// </summary>
    function GetChild<T: class>(const AIndex: Integer): T; overload;

    /// <summary>
    ///  Returns the index of a given child node in a list of children limited
    ///  to the set of children that are of the specified type, or -1 if the specified
    ///  children is not a direct child of the current tree.
    /// </summary>
    function GetChildIndex<T: class>(const AChild: T): Integer;

    /// <summary>
    ///  Searches a node by a path. Separate hierarchy elements with a /.
    /// </summary>
    /// <param name="APath">
    ///  A string path. Example: Node/SubNode.
    /// </param>
    /// <param name="ACreateMissingNodes">
    ///  If True, creates any missing nodes throughout the path and the final
    ///  node as well. This guarantees that the result is not nil.
    /// </param>
    /// <returns>
    ///  The found node, or nil.
    /// </returns>
    function FindNode(const APath: string; const ACreateMissingNodes: Boolean = False): TEFNode; overload; virtual;

    /// <summary>
    ///  Searches a node by trying a sequence of paths in given order.
    /// </summary>
    /// <param name="APaths">
    ///  A sequence of string paths to try. Example: ['Node/SubNode', 'Node/SubNode2'].
    /// </param>
    /// <returns>
    ///  The first node found, or nil.
    /// </returns>
    function FindNode(const APaths: TStringDynArray): TEFNode; overload;

    /// <summary>
    ///  Works like FindNode, but raises an exception if ACreateMissingNodes
    ///  is False and the wanted node does not exist.
    /// </summary>
    /// <param name="ACreateMissingNodes">
    ///  If True, creates any missing nodes throughout the path and the final
    ///  node as well. This guarantees that the result is not nil.
    /// </param>
    /// <returns>
    ///  The found node.
    /// </returns>
    function GetNode(const APath: string; const ACreateMissingNodes: Boolean = False): TEFNode;

    /// <summary>
    ///  Finds a node by path and, if found, deletes it.
    /// </summary>
    procedure DeleteNode(const APath: string);

    /// <summary>
    ///  Finds a node by path and, if found, returns its value, otherwise
    ///  returns ADefaultValue.
    /// </summary>
    function GetValue(const APath: string; const ADefaultValue: Variant): Variant; overload;

    /// <summary>
    ///  Finds a node by path and, if found, returns its value, otherwise
    ///  returns Null.
    /// </summary>
    function GetValue(const APath: string): Variant; overload;

    /// <summary>
    ///  Finds a node by path and, if found, returns its value as a Boolean,
    ///  otherwise returns ADefaultValue.
    /// </summary>
    function GetBoolean(const APath: string; const ADefaultValue: Boolean = False): Boolean;

    /// <summary>
    ///  Finds a node by path and, if found, returns its value as a Double,
    ///  otherwise returns ADefaultValue.
    /// </summary>
    function GetFloat(const APath: string; const ADefaultValue: Double = 0): Double;

    /// <summary>
    ///  Finds a node by path and, if found, returns its value as an Integer,
    ///  otherwise returns ADefaultValue.
    /// </summary>
    function GetInteger(const APath: string; const ADefaultValue: Integer = 0): Integer;

    /// <summary>
    ///  Finds a node by path and, if found, returns its value as a string,
    ///  otherwise returns ADefaultValue.
    /// </summary>
    function GetString(const APath: string; const ADefaultValue: string = ''): string;

    /// <summary>
    ///  Finds a node by path and, if found, returns its value as a char,
    ///  otherwise returns ADefaultValue.
    /// </summary>
    function GetChar(const APath: string; const ADefaultValue: Char = #0): Char;

    /// <summary>
    ///  Finds a node by path and, if found, returns its value as a Date,
    ///  otherwise returns ADefaultValue.
    /// </summary>
    function GetDate(const APath: string; const ADefaultValue: TDate = 0): TDate;

    /// <summary>
    ///  Finds a node by path and, if found, returns its value as an expanded
    ///  string, otherwise returns ADefaultValue.
    /// </summary>
    /// <returns>
    ///  The found node's string with any macros expanded.
    /// <returns>
    /// <remarks>
    ///  If the node is not found, macros in ADefaultValues are expanded as
    ///  well before returning it. This method guarantees that the return
    ///  value has all known macros expanded anyway.
    /// </remarks>
    function GetExpandedString(const APath: string; const ADefaultValue: string = ''): string;

    /// <summary>
    ///   Finds a node by path and, if found, returns its value as a string
    ///   array, otherwise returns ADefaultValue.
    /// </summary>
    function GetStringArray(const APath: string; const ADefaultValue: TStringDynArray = nil): TStringDynArray;

    /// <summary>
    ///   Finds a node by path and, if found, returns its value as a list of
    ///   pairs, otherwise returns ADefaultValue.
    /// </summary>
    function GetPairs(const APath: string; const ADefaultValue: TEFPairs = nil): TEFPairs;

    /// <summary>
    ///   Returns a separated list of all children of the specified node in the
    ///   form Name&lt;AConnector&gt;AsString.
    /// </summary>
    /// <param name="APath">
    ///   Locates the parent node of the strings to extract.
    /// </param>
    /// <param name="ASeparator">
    ///   Separator for name/value pairs (ex. '=').
    /// </param>
    /// <param name="AConnector">
    ///   String used to connect pairs (ex. sLineBreak).
    /// </param>
    /// <param name="ADefaultValue">
    ///   Value to return when the node does not exist.
    /// </param>
    /// <returns>
    ///   A concatenation of all children of the specified node built according
    ///   to the arguments.
    /// </returns>
    function GetChildrenAsStrings(const APath: string; const ASeparator: string = sLineBreak;
      const AConnector: string = '='; const ADefaultValue: string = ''): string; overload;

    /// <summary>
    ///   Returns a list of all children of the specified node in the
    ///   form Name=AsString.
    /// </summary>
    /// <param name="APath">
    ///   Locates the parent node of the strings to extract.
    /// </param>
    /// <param name="AStrings">
    ///   Object to which strings are to be added. Any exiting contents are
    ///   deleted.
    /// </param>
    /// <returns>
    ///   The number of appended items.
    /// </returns>
    function GetChildrenAsStrings(const APath: string; const AStrings: TStrings): Integer; overload;

    /// <summary>Deletes all children nodes and replaces them with one node for
    /// each item in the specified string list. Strings must be in the form
    /// Name=Value.</summary>
    procedure SetChildrenAsStrings(const APath: string; const AStrings: TStrings);

    /// <summary>
    ///   Returns an array of all children of the specified node, which must be
    ///   in name=value format, as pairs.
    /// </summary>
    /// <param name="APath">
    ///   Locates the parent node of the strings to extract.
    /// </param>
    /// <returns>
    ///   Array of name/value pairs.
    /// </returns>
    function GetChildrenAsPairs(const APath: string;
      const AExpandMacrosInValues: Boolean = False;
      const ADefaultValue: TEFPairs = nil): TEFPairs;

    /// <summary>
    ///   Same as GetChildrenAsStrings, but returns expanded strings: Each string is
    ///   passed to the macro expander before concatenation.
    /// </summary>
    function GetChildrenAsExpandedStrings(const APath: string; const ASeparator: string = sLineBreak;
      const AConnector: string = '='; const ADefaultValue: string = ''): string;

    /// <summary>
    ///   Finds a node by path and, if found, returns its value as an object,
    ///   otherwise returns ADefaultValue.
    /// </summary>
    function GetObject(const APath: string; const ADefaultValue: TObject = nil): TObject; overload;
    function GetObject<T: class>(const APath: string): T; overload;

    /// <summary>
    ///   Sets a node value by path. The node is created if it doesn't exist
    ///   yet.
    /// </summary>
    procedure SetInteger(const APath: string; const AValue: Integer);

    /// <summary>
    ///   Sets a node value by path. The node is created if it doesn't exist
    ///   yet.
    /// </summary>
    function SetString(const APath: string; const AValue: string): TEFNode;

    /// <summary>
    ///   Sets a node value by path. The node is created if it doesn't exist
    ///   yet.
    /// </summary>
    function SetFloat(const APath: string; const AValue: Double): TEFNode;

    /// <summary>
    ///   Sets a node value by path. The node is created if it doesn't exist
    ///   yet.
    /// </summary>
    function SetValue(const APath: string; const AValue: Variant): TEFNode;

    /// <summary>
    ///   Sets a node value by path. The node is created if it doesn't exist
    ///   yet.
    /// </summary>
    procedure SetObject(const APath: string; const AValue: TObject);

    /// <summary>
    ///   Sets a node value by path. The node is created if it doesn't exist
    ///   yet.
    /// </summary>
    procedure SetBoolean(const APath: string; const AValue: Boolean);

    /// <summary>
    ///   Sets a node value by path. The node is created if it doesn't exist
    ///   yet.
    /// </summary>
    procedure SetDateTime(const APath: string; const AValue: TDateTime);

    /// <summary>
    ///   Creates a children for each field in AField and sets its value to the
    ///   field's value. Node names are field names. Existing nodes are
    ///   overwritten.
    /// </summary>
    procedure AddFieldsAsChildren(const AFields: TFields);

    property AnnotationCount: Integer read GetAnnotationCount;
    property Annotations[const AIndex: Integer]: string read GetAnnotation write SetAnnotation;
    function AddAnnotation(const AAnnotation: string): Integer;
    procedure AssignAnnotations(const AStrings: TStrings);

    /// <summary>Returns a string-based path.</summary>
    /// <remarks>Returns '' in the tree, and a slash-separated path in the
    /// nodes.</remarks>
    function GetPath: string; virtual;

    /// <summary>
    ///   A reference to the root tree. In this class, returns Self.
    /// </summary>
    property Root: TEFTree read GetRoot;

    procedure Sort(const ACompareFunc: TEFNodeCompareFunc);

    /// <summary>
    ///  Looks for the node specified by APath and, if found, sets the same-named
    ///  property of AInstance to the node's value. If APath is a compound path
    ///  (such as Path/To/Property) then only the last part is used.
    ///  If ApathIsName is True, then the path is assumed not to be compound and
    ///  the code is more efficient.
    /// </summary>
    procedure SetPropertyFromNode(const AInstance: TObject; const APath: string; const APathIsName: Boolean = False);
    /// <summary>
    ///  Same as SetPropertyFromNode but for an array of nodes/properties.
    /// </summary>
    procedure SetPropertiesFromNode(const AInstance: TObject; const APaths: TArray<string>; const APathIsName: Boolean = False);
    /// <summary>
    ///  Calls SetPropertyFromNode for all direct children. If any children don't
    ///  have corresponding properties, then an exception is raised.
    /// </summary>
    procedure SetPropertiesFromChildNodes(const AInstance: TObject);

    /// <summary>
    ///  Tries to read from ASource a value for each child node, interpreting
    ///  it according to the child node's DataType. Read values are stored in
    ///  the child nodes.
    /// </summary>
    /// <param name="ASource">
    ///  Source data. Only top-level nodes are copied. Each node may contain a
    ///  single value. If AValueIndex is >= 0, each node may contain one or more
    ///  comma-separated string values, of which only the one with index
    ///  AValueIndex is read.
    /// </param>
    /// <param name="AUseJSDateFormat">
    ///  True if any dates in source strings are in JS format; False for
    ///  system format.
    /// </param>
    /// <param name="AFormatSettings">
    ///  Custom format settings to decode values.
    /// </param>
    /// <param name="ATranslator">
    ///  Pass a translation function if key names in ASource do not match
    ///  wanted child node names and you need to translate them. The function
    ///  receives the child name and should return the corresponding source name.
    /// </param>
    procedure CopyChildValues(const ASource: TEFTree;
      const AUseJSDateFormat: Boolean; const AFormatSettings: TFormatSettings;
      const ATranslator: TNameTranslator; const AValueIndex: Integer);
  end;

  TEFTreeClass = class of TEFTree;

  /// <summary>A tree that stores a file name (or other logical
  /// identifier).</summary>
  TEFPersistentTree = class(TEFTree)
  strict private
    FPersistentName: string;
    function GetIsPersistent: Boolean;
  strict protected
    function GetPersistentFileName: string; virtual;
    procedure InternalAfterLoad; virtual;
  public
    procedure AfterLoad;

    procedure Assign(const ASource: TEFTree; const AProc: TEFTree.TAssignNodeProc = nil); override;

    property PersistentName: string read FPersistentName write FPersistentName;

    property IsPersistent: Boolean read GetIsPersistent;

    /// <summary>Returns the full path name of the persistent file.</summary>
    property PersistentFileName: string read GetPersistentFileName;
  end;

  /// <summary>
  ///  A node in a tree. Has a name and a value, and can have subnodes.
  /// </summary>
  TEFNode = class(TEFTree)
  private
    FParent: TEFTree;
    function FindNodeFrom(const APath: TStringDynArray; const AIndex: Integer;
      const ACreateMissingNodes: Boolean = False): TEFNode;
    function GetAsChar: Char;
    procedure SetAsChar(const AValue: Char);
  strict private
    FValue: Variant;
    FName: string;
    FDataType: TEFDataType;
    FValueAttributes: string;
    FDataTypeLockCount: Integer;
    function GetAsString: string;
    function GetAsInteger: Integer;
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
    function GetAsBytes: TBytes;
    procedure SetAsBytes(const AValue: TBytes);
    function GetAsExpandedPair: TEFPair;
    function GetIsMultiLineValue: Boolean;
    function GetIsMultiLineWithNLValue: Boolean;
    procedure SetName(const AValue: string);
  strict protected
    function GetName: string; virtual;
    procedure SetValue(const AValue: Variant); virtual;
    function IsDataTypeLocked: Boolean;
    procedure ValueChanging(const AOldValue: Variant; var ANewValue: Variant; var ADoIt: Boolean); virtual;
    procedure ValueChanged(const AOldValue, ANewValue: Variant); virtual;
    function CompareValues(const AValue1, AValue2: Variant): Boolean;
    function GetDataType: TEFDataType; virtual;
  public
    function GetEnumerator: TEnumerator<TEFNode>;
    function GetEmptyAsNull: Boolean; virtual;
  public
    function GetRoot: TEFTree; override;
    function FindNode(const APath: string;
      const ACreateMissingNodes: Boolean = False): TEFNode; override;

    /// <summary>
    ///  Copies everything from ASource, overwriting any existing data. if
    ///  ASource is a node, name and value are copied as well.
    /// </summary>
    procedure Assign(const ASource: TEFTree; const AProc: TEFTree.TAssignNodeProc = nil); override;

    /// <summary>
    ///   Copies the value (and datatype) from the specified node. The name is
    ///   unchanged.
    /// </summary>
    procedure AssignValue(const ASource: TEFNode);

    /// <summary>
    ///   A reference to the parent node, if any.
    /// </summary>
    property Parent: TEFTree read FParent;

    /// <summary>
    ///   Index of the node in the parent's list of node.
    /// </summary>
    /// <value>
    ///   -1 if the node has no parent.
    /// </value>
    property Index: Integer read GetIndex;

    /// <summary>
    ///   Creates a node with specified name and value.
    /// </summary>
    constructor Create(const AName: string; const AValue: Variant); reintroduce; overload; virtual;

    /// <summary>
    ///   Creates a node with specified name and no value.
    /// </summary>
    constructor Create(const AName: string); reintroduce; overload;

    /// <summary>
    ///   Creates a node with no name and no value.
    /// </summary>
    constructor Create; reintroduce; overload;

    /// <summary>
    ///   Creates a new node and assigns the specified node to it.
    /// </summary>
    constructor Clone(const ASource: TEFTree; const AProc: TEFTree.TAssignNodeProc = nil); override;

    /// <summary>
    ///   Deletes all subnodes, recursively.
    /// </summary>
    procedure Clear; override;

    /// <summary>
    ///   Removes itself from its parent. This causes the parent to free the
    ///   current object.
    /// </summary>
    procedure Delete;

    /// <summary>
    ///   Identifies the node among its siblings. Should be unique inside the
    ///   parent.
    /// </summary>
    property Name: string read GetName write SetName;

    /// <summary>Renames the node. Normally shouldn't be used.</summary>
    procedure Rename(const ANewName: string);

    /// <summary>
    ///   A reference to the node's data type. Should be an object managed by
    ///   the data type factory.
    /// </summary>
    property DataType: TEFDataType read GetDataType write SetDataType;

    /// <summary>Increments and returns the DataType lock count. When the
    /// number is &gt; 0, assigning a value through the As... properties will
    /// <b>not</b> change the datatype (unless it's unknown). This also applies
    /// to any other operation that implicitly changes the DataType. Explicitly
    /// setting DataType is still allowed even when locked.</summary>
    function LockDataType: Integer;

    /// <summary>Decrements and returns the DataType lock count.</summary>
    /// <remarks>This call does <b>not</b> guarantee that the DataType will be
    /// unlocked. It will only when the returned value is zero, which happens
    /// when calls to LockDataType and UnlockDataType are balanced.</remarks>
    /// <seealso cref="LockDataType"></seealso>
    function UnlockDataType: Integer;

    /// <summary>
    ///  Plain value of the node.
    /// </summary>
    property Value: Variant read GetValue write SetValue;

    /// <summary>Used for I/O.</summary>
    property ValueAttributes: string read FValueAttributes write FValueAttributes;

    property IsMultiLineValue: Boolean read GetIsMultiLineValue;
    property IsMultiLineWithNLValue: Boolean read GetIsMultiLineWithNLValue;

    /// <summary>
    ///   Node value as a string.
    /// </summary>
    property AsString: string read GetAsString write SetAsString;

    /// <summary>
    ///   Node value as a string.
    /// </summary>
    property AsChar: Char read GetAsChar write SetAsChar;

    /// <summary>
    ///   Node value as a string array.
    /// </summary>
    property AsStringArray: TStringDynArray read GetAsStringArray write SetAsStringArray;

    /// <summary>
    ///   Node value as a pair.
    /// </summary>
    property AsPair: TEFPair read GetAsPair write SetAsPair;

    /// <summary>
    ///   Node value as a pair with expanded macros in the value part.
    /// </summary>
    property AsExpandedPair: TEFPair read GetAsExpandedPair;

    /// <summary>
    ///   Node value as a list of pairs.
    /// </summary>
    property AsPairs: TEFPairs read GetAsPairs write SetAsPairs;

    /// <summary>
    ///   Node value as an expanded string.
    /// </summary>
    property AsExpandedString: string read GetAsExpandedString;

    /// <summary>
    ///   Node value as an Integer.
    /// </summary>
    property AsInteger: Integer read GetAsInteger write SetAsInteger;

    /// <summary>
    ///   Node value as an object.
    /// </summary>
    property AsObject: TObject read GetAsObject write SetAsObject;

    /// <summary>
    ///   Node value as a Boolean.
    /// </summary>
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;

    /// <summary>
    ///   Node value as a Date.
    /// </summary>
    property AsDate: TDate read GetAsDate write SetAsDate;

    /// <summary>
    ///   Node value as a Time.
    /// </summary>
    property AsTime: TTime read GetAsTime write SetAsTime;

    /// <summary>
    ///   Node value as a DateTime.
    /// </summary>
    property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;

    /// <summary>
    ///   Node value as a Currency.
    /// </summary>
    property AsCurrency: Currency read GetAsCurrency write SetAsCurrency;

    /// <summary>
    ///   Node value as a Double floating point value.
    /// </summary>
    property AsFloat: Double read GetAsFloat write SetAsFloat;

    /// <summary>
    ///   Node value as a decimal (BCD) value.
    /// </summary>
    property AsDecimal: TBcd read GetAsDecimal write SetAsDecimal;

    property AsBytes: TBytes read GetAsBytes write SetAsBytes;
    procedure LoadBytesFromStream(const AStream: TStream);

    /// <summary>
    ///  Parses AValue trying to guess its data type and sets Value and
    ///  DataType accordingly.
    /// </summary>
    /// <param name="AValue">Value to parse, usually read from a Yaml stream.</param>
    /// <param name="AFormatSettings">Format settings to use to parse the
    /// value. You can use Session.UserFormatSettings, or
    /// Session.JSFormatSettings, or custom settings.</param>
    /// <returns>Returns Self to allow for fluent calls.</returns>
    function SetAsYamlValue(const AValue: string; const AFormatSettings: TFormatSettings): TEFNode;

    /// <summary>Parses AValue according to DataType and sets its own value
    /// to the parsed value.</summary>
    /// <param name="AValue">Value to parse, usually got from a web request.</param>
    /// <param name="AUseJSDateFormat">Set to True if date/times are specified in JS long format.</param>
    /// <param name="AFormatSettings">Format settings to use to parse the
    /// value. You can use Session.UserFormatSettings, or
    /// Session.JSFormatSettings, or custom settings.</param>
    /// <returns>Returns Self to allow for fluent calls.</returns>
    function SetAsJSONValue(const AValue: string; const AUseJSDateFormat: Boolean;
      const AFormatSettings: TFormatSettings): TEFNode;

    /// <summary>
    ///   True if the node is null. Null is meant as absence of a value. Nodes
    ///   are made null by calling their SetToNull method.
    /// </summary>
    property IsNull: Boolean read GetIsNull;

    /// <summary>
    ///   Sets the node to null, effectively clearing the value.
    /// </summary>
    /// <remarks>
    ///   Children are unaffected.
    /// </remarks>
    procedure SetToNull(const AForceChangeNotification: Boolean = False); virtual;

    /// <summary>
    ///   Returns true if the current node has the same name and value as the
    ///   specified node, or if both are null.
    /// </summary>
    /// <remarks>
    ///   Children are not considered.
    /// </remarks>
    function EqualsNode(const ANode: TEFNode): Boolean;

    /// <summary>
    ///   Returns True if the current node has specified value or if both
    ///   the node and the value are null.
    /// </summary>
    /// <remarks>
    ///   Children are not considered.
    /// </remarks>
    function EqualsValue(const AValue: Variant): Boolean;

    /// <summary>
    ///   Returns all child nodes as a string of name&lt;AConnector&gt;value
    ///   pairs separated by ASeparator.
    /// </summary>
    function GetChildStrings(const ASeparator: string = sLineBreak;
      const AConnector: string = '='; const ADefaultValue: string = ''): string; overload;

    /// <summary>Adds to the specified string list all child nodes as strings
    /// in the form Name=Value. Returns the number of added items.</summary>
    /// <remarks>All existing contents in AStrings are deleted.</remarks>
    function GetChildStrings(const AStrings: TStrings): Integer; overload;

    /// <summary>Adds to the specified string list all child node values.
    /// Returns the number of added items.</summary>
    /// <remarks>All existing contents in AStrings are deleted.</remarks>
    function GetChildValues(const AStrings: TStrings): Integer; overload;

    /// <summary>Adds to the specified string list all child node names.
    /// Returns the number of added items.</summary>
    /// <remarks>All existing contents in AStrings are deleted.</remarks>
    function GetChildNames(const AStrings: TStrings): Integer; overload;

    /// <summary>Deletes all children and adds a new children for each string
    /// in the specified string list. Strings must be in the form
    /// Name=Value.</summary>
    /// <remarks>All existing contents in AStrings are deleted.</remarks>
    procedure SetChildStrings(const AStrings: TStrings);

    /// <summary>
    ///   Returns all child nodes as a string of name&lt;AConnector&gt;value
    ///   pairs separated by ASeparator. Values are expanded.
    /// </summary>
    function GetExpandedChildStrings(const ASeparator, AConnector,
      ADefaultValue: string): string;

    /// <summary>
    ///   Returns all child nodes as name/value pairs.
    /// </summary>
    function GetChildPairs(const AExpandMacrosInValues: Boolean = False): TEFPairs;

    /// <summary>
    ///   Returns an array of names of all direct children of the node.
    /// </summary>
    function GetChildNames: TStringDynArray; overload;

    /// <summary>
    ///   Returns an array of references to all direct children of the node.
    /// </summary>
    function ToArray: TArray<TEFNode>;

    /// <summary>
    ///  Assigns a field's value to the node. May also change or set the
    ///  node's datatype.
    /// </summary>
    procedure AssignFieldValue(const AField: TField);

    /// <summary>
    ///  Assigns the node's value to the specified field.
    /// </summary>
    procedure AssignValueToField(const AField: TField);

    /// <summary>
    ///  Assigns the node's value to the specified param. May also set the
    ///  param's data type.
    /// </summary>
    procedure AssignValueToParam(const AParam: TParam);

    /// <summary>
    ///  Assigns the node's name and value to the specified param. May also
    ///  set the param's data type.
    /// </summary>
    procedure AssignToParam(const AParam: TParam);

    /// <summary>Returns the node's slash-separated path, up to the
    /// root.</summary>
    function GetPath: string; override;
  end;

  /// <summary>
  ///   Creates trees of specified types from files.
  /// </summary>
  TEFTreeFactory = class
  public
    /// <summary>
    ///   Loads the specified yaml file and builds a tree of the specified type
    ///   with all data in it.
    /// </summary>
    class function LoadFromFile<T: TEFTree, constructor>(const AFileName: string): T;

    /// <summary>
    ///   Reloads the specified yaml file into an existing tree, overwriting
    ///   any data in it.
    /// </summary>
    class procedure ReloadFromFile(const ATree: TEFTree; const AFileName: string);
  end;

  /// <summary>
  ///   <para>
  ///     A macro expander that expands all the strings contained in a TEFTree
  ///     object. Each macro in this format:
  ///   </para>
  ///   <para>
  ///     %<NameSpace>:<Path>%
  ///   </para>
  ///   <para>
  ///     is expanded to the string value of the tree node located by the path.
  ///     This macro expander holds a reference to a TEFTree. &lt;NameSpace&gt;
  ///     is a value set upon creation. If it is empty, then no ':' separator
  ///     is required in the macros.
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   <para>
  ///     The need for a name space stems from the fact that you can have
  ///     multiple macro expanders of this kind active at the same time, each
  ///     linked to a different tree object, and use the namespace string to
  ///     differentiate them.
  ///   </para>
  ///   <para>
  ///     This macro expander is not registered by default, as it needs a
  ///     reference to an external object to work. So, applications will create
  ///     and use this class autonomously (by querying it directly or adding it
  ///     to an expansion engine, without registering it) as required.
  ///   </para>
  /// </remarks>
  TEFTreeMacroExpander = class(TEFMacroExpander)
  strict private
    FTree: TEFTree;
    FPrefix: string;
  strict protected
    procedure ExpandTreeMacros(var AString: string; const ATree: TEFTree);
    property Tree: TEFTree read FTree;
    procedure InternalExpand(var AString: string); override;
  public
    constructor Create(const ATree: TEFTree; const ANameSpace: string); reintroduce;
  end;

type
  /// <summary>
  ///   Keeps track of all registered data types.
  /// </summary>
  TEFDataTypeRegistry = class(TEFRegistry)
  private
    class var FInstance: TEFDataTypeRegistry;
    class function GetInstance: TEFDataTypeRegistry; static;
  protected
    class destructor Destroy;
  public
    class property Instance: TEFDataTypeRegistry read GetInstance;
    function GetClass(const AId: string): TEFDataTypeClass;
  end;

  /// <summary>
  ///   Holds a list of registered data types and manages their lifetimes.
  /// </summary>
  TEFDataTypeFactory = class(TEFFactory)
  private
    FDataTypes: TDictionary<string, TEFDataType>;
    class var FInstance: TEFDataTypeFactory;
    class function GetInstance: TEFDataTypeFactory; static;
  public
    class destructor Destroy;
    procedure AfterConstruction; override;
    destructor Destroy; override;
  public
    class property Instance: TEFDataTypeFactory read GetInstance;

    /// <summary>
    ///   Returns a reference to the registered data type specified by name.
    /// </summary>
    function GetDataType(const AId: string): TEFDataType; overload;

    /// <summary>
    ///   Returns a reference to the registered data type specified by class.
    /// </summary>
    function GetDataType(const ADataTypeClass: TEFDataTypeClass): TEFDataType; overload;
  end;

implementation

uses
  StrUtils, TypInfo, Math, DateUtils,
  EF.JSON, EF.XML, EF.Localization, EF.StrUtils, EF.YAML, EF.VariantUtils;

{$IF RTLVersion < 23.0}
const
  varObject = $0049;
{$IFEND}

function JSDateToDateTime(const AJSDate: string): TDateTime;
begin
  Result := EncodeDateTime(StrToInt(Copy(AJSDate, 12, 4)),
    AnsiIndexStr(Copy(AJSDate, 5, 3), ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']) + 1,
    StrToInt(Copy(AJSDate, 9, 2)), StrToInt(Copy(AJSDate, 17, 2)), StrToInt(Copy(AJSDate, 20, 2)),
    StrToInt(Copy(AJSDate, 23, 2)), 0);
end;

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
var
  LDataType: TEFDataType;
begin
  for LDataType in FDataTypes.Values do
    LDataType.Free;
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

function TEFDataTypeFactory.GetDataType(
  const ADataTypeClass: TEFDataTypeClass): TEFDataType;
begin
  Result := GetDataType(ADataTypeClass.GetTypeName);
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
  else if VarIsArray(AVariant) then
    Result := TEFDataTypeFactory.Instance.GetDataType('Blob')
  else
    Result := TEFDataTypeFactory.Instance.GetDataType('String');
end;

{ TEFNode }

procedure TEFNode.Assign(const ASource: TEFTree; const AProc: TEFTree.TAssignNodeProc);
begin
  inherited;
  if Assigned(ASource) and (ASource is TEFNode) then
  begin
    FName := TEFNode(ASource).Name;
    DataType := TEFNode(ASource).DataType;
    LockDataType;
    try
      AssignValue(TEFNode(ASource));
    finally
      UnlockDataType;
    end;
    FValueAttributes := TEFNode(ASource).ValueAttributes;
    if Assigned(AProc) then
      AProc(TEFNode(ASource), Self);
  end;
end;

procedure TEFNode.AssignValue(const ASource: TEFNode);
begin
  if Assigned(ASource) then
    Value := TEFNode(ASource).Value
  else
    SetToNull;
end;

procedure TEFNode.AssignValueToField(const AField: TField);
begin
  Assert(Assigned(AField));

  DataType.NodeToField(Self, AField);
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
  FValue := Unassigned;
end;

constructor TEFNode.Clone(const ASource: TEFTree; const AProc: TEFTree.TAssignNodeProc);
begin
  Assert((ASource = nil) or (ASource is TEFNode));

  inherited;
  Assign(TEFNode(ASource), AProc);
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
  Result := DataType.ValueToBoolean(FValue);
end;

function TEFNode.GetAsBytes: TBytes;
begin
  Result := DataType.ValueToBytes(FValue);
end;

function TEFNode.GetAsChar: Char;
begin
  Result := DataType.ValueToChar(FValue);
end;

function TEFNode.GetAsCurrency: Currency;
begin
  Result := DataType.ValueToCurrency(FValue);
end;

function TEFNode.GetAsDate: TDate;
begin
  Result := DataType.ValueToDate(FValue);
end;

function TEFNode.GetAsDateTime: TDateTime;
begin
  Result := DataType.ValueToDateTime(FValue);
end;

function TEFNode.GetAsDecimal: TBcd;
begin
  Result := DataType.ValueToDecimal(FValue);
end;

function TEFNode.GetAsExpandedPair: TEFPair;
begin
  Result := TEFPair.Create(Name, AsExpandedString);
end;

function TEFNode.GetAsExpandedString: string;
begin
  Result := AsString;
  TEFMacroExpansionEngine.Instance.Expand(Result);
end;

function TEFNode.GetAsFloat: Double;
begin
  Result := DataType.ValueToFloat(FValue);
end;

function TEFNode.GetAsInteger: Integer;
begin
  Result := DataType.ValueToInteger(FValue);
end;

function TEFNode.GetAsObject: TObject;
begin
  Result := DataType.ValueToObject(FValue);
end;

function TEFNode.GetAsPair: TEFPair;
begin
  Result := TEFPair.Create(Name, AsString);
end;

function TEFNode.GetAsPairs: TEFPairs;
begin
  Result := DataType.ValueToPairs(FValue);
end;

function TEFNode.GetAsString: string;
begin
  // Since when getDataType became virtual, this is slowing down load
  // performance by a great deal. Let's try cutting this indirection
  // (limited to string values) for a while.
  //Result := DataType.ValueToString(FValue);
  Result := EFVarToStr(FValue);
  {$IFNDEF KIDE}
  if Result.StartsWith('_(') then
    Result := _(Result);
  {$ENDIF}
end;

function TEFNode.GetAsStringArray: TStringDynArray;
begin
  Result := DataType.ValueToStringArray(FValue);
end;

function TEFNode.GetAsTime: TTime;
begin
  Result := DataType.ValueToTime(FValue);
end;

function TEFNode.GetChildStrings(const AStrings: TStrings): Integer;
begin
  Assert(Assigned(AStrings));

  AStrings.Text := GetChildStrings;
  Result := AStrings.Count;
end;

function TEFNode.GetChildValues(const AStrings: TStrings): Integer;
var
  I: Integer;
begin
  Assert(Assigned(AStrings));

  AStrings.Text := GetChildStrings;
  for I := 0 to AStrings.Count - 1 do
    AStrings[I] := AStrings.ValueFromIndex[I];
  Result := AStrings.Count;
end;

function TEFNode.GetChildNames(const AStrings: TStrings): Integer;
var
  I: Integer;
begin
  Assert(Assigned(AStrings));
  AStrings.Clear;
  for I := 0 to NodeList.Count - 1 do
    AStrings.Add(NodeList[I].Name);
  Result := AStrings.Count;
end;

function TEFNode.GetDataType: TEFDataType;
begin
  if FDataType = nil then
    FDataType := GetVariantDataType(Value);
  Result := FDataType;
end;

function TEFNode.GetChildStrings(const ASeparator, AConnector,
  ADefaultValue: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to NodeList.Count - 1 do
  begin
    if Result = '' then
      Result := NodeList[I].Name + AConnector + NodeList[I].AsString
    else
      Result := Result + ASeparator + NodeList[I].Name + AConnector + NodeList[I].AsString;
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

function TEFNode.GetChildPairs(const AExpandMacrosInValues: Boolean): TEFPairs;
var
  I: Integer;
begin
  SetLength(Result, ChildCount);
  for I := 0 to ChildCount - 1 do
    if AExpandMacrosInValues then
      Result[I] := Children[I].AsExpandedPair
    else
      Result[I] := Children[I].AsPair;
end;

function TEFNode.ToArray: TArray<TEFNode>;
var
  I: Integer;
begin
  SetLength(Result, ChildCount);
  for I := 0 to ChildCount - 1 do
    Result[I] := Children[I];
end;

function TEFNode.UnlockDataType: Integer;
begin
  Dec(FDataTypeLockCount);
  Result := FDataTypeLockCount;
end;

function TEFNode.GetEmptyAsNull: Boolean;
begin
  Result := DataType.GetDefaultEmptyAsNull;
end;

function TEFNode.GetEnumerator: TEnumerator<TEFNode>;
begin
  Result := NodeList.GetEnumerator;
end;

function TEFNode.GetExpandedChildStrings(const ASeparator, AConnector,
  ADefaultValue: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to NodeList.Count - 1 do
  begin
    if Result = '' then
      Result := NodeList[I].Name + AConnector + NodeList[I].AsExpandedString
    else
      Result := Result + ASeparator + NodeList[I].Name + AConnector + NodeList[I].AsExpandedString;
  end;
end;

function TEFNode.GetIndex: Integer;
begin
  if Parent <> nil then
    Result := Parent.NodeList.IndexOf(Self)
  else
    Result := -1;
end;

function TEFNode.GetIsMultiLineValue: Boolean;
begin
  Result := GetIsMultiLineWithNLValue or ContainsText(FValueAttributes, '>');
end;

function TEFNode.GetIsMultiLineWithNLValue: Boolean;
begin
  Result := ContainsText(FValueAttributes, '|')
    or ContainsText(AsString, sLineBreak);
end;

function TEFNode.GetIsNull: Boolean;
begin
  Result := VarIsNull(FValue);
end;

function TEFNode.GetName: string;
begin
  Result := FName;
end;

function TEFNode.GetPath: string;
begin
  if Parent <> nil then
    Result := SmartConcat(Parent.GetPath, '/', Name)
  else
    Result := Name;
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

function TEFNode.IsDataTypeLocked: Boolean;
begin
  Result := (FDataTypeLockCount > 0) and (FDataType <> nil);
end;

procedure TEFNode.LoadBytesFromStream(const AStream: TStream);
var
  LBytesStream: TBytesStream;
begin
  LBytesStream := TBytesStream.Create;
  try
    LBytesStream.CopyFrom(AStream, 0);
    AsBytes := Copy(LBytesStream.Bytes, 0, LBytesStream.Size);
  finally
    FreeAndNil(LBytesStream);
  end;
end;

function TEFNode.LockDataType: Integer;
begin
  Inc(FDataTypeLockCount);
  Result := FDataTypeLockCount;
end;

procedure TEFNode.Rename(const ANewName: string);
begin
  SetName(ANewName);
end;

procedure TEFNode.SetAsBoolean(const AValue: Boolean);
begin
  if not IsDataTypeLocked then
    FDataType := TEFDataTypeFactory.Instance.GetDataType('Boolean');
  Value := DataType.BooleanToValue(AValue);
end;

procedure TEFNode.SetAsBytes(const AValue: TBytes);
begin
  if not IsDataTypeLocked then
    FDataType := TEFDataTypeFactory.Instance.GetDataType('Blob');
  Value := DataType.BytesToValue(AValue);
end;

procedure TEFNode.SetAsChar(const AValue: Char);
begin
  Value := DataType.CharToValue(AValue);
end;

procedure TEFNode.SetAsCurrency(const AValue: Currency);
begin
  if not IsDataTypeLocked then
    FDataType := TEFDataTypeFactory.Instance.GetDataType('Currency');
  Value := DataType.CurrencyToValue(AValue);
end;

procedure TEFNode.SetAsDate(const AValue: TDate);
begin
  if not IsDataTypeLocked then
    FDataType := TEFDataTypeFactory.Instance.GetDataType('Date');
  Value := DataType.DateToValue(AValue);
end;

procedure TEFNode.SetAsDateTime(const AValue: TDateTime);
begin
  if not IsDataTypeLocked then
    FDataType := TEFDataTypeFactory.Instance.GetDataType('DateTime');
  Value := DataType.DateTimeToValue(AValue);
end;

procedure TEFNode.SetAsDecimal(const AValue: TBcd);
begin
  if not IsDataTypeLocked then
    FDataType := TEFDataTypeFactory.Instance.GetDataType('Decimal');
  Value := DataType.DecimalToValue(AValue);
end;

procedure TEFNode.SetAsFloat(const AValue: Double);
begin
  if not IsDataTypeLocked then
    FDataType := TEFDataTypeFactory.Instance.GetDataType('Float');
  Value := DataType.FloatToValue(AValue);
end;

procedure TEFNode.SetAsInteger(const AValue: Integer);
begin
  if not IsDataTypeLocked then
    FDataType := TEFDataTypeFactory.Instance.GetDataType('Integer');
  Value := DataType.IntegerToValue(AValue);
end;

function TEFNode.SetAsJSONValue(const AValue: string;
  const AUseJSDateFormat: Boolean; const AFormatSettings: TFormatSettings): TEFNode;
begin
  DataType.JSONValueToNode(Self, AValue, AUseJSDateFormat, AFormatSettings);
  Result := Self;
end;

procedure TEFNode.SetAsObject(const AValue: TObject);
begin
  if not IsDataTypeLocked then
    FDataType := TEFDataTypeFactory.Instance.GetDataType('Object');
  Value := DataType.ObjectToValue(AValue);
  // Setting Value will revert the data type to integer.
  if not IsDataTypeLocked then
    FDataType := TEFDataTypeFactory.Instance.GetDataType('Object');
end;

procedure TEFNode.SetAsPair(const AValue: TEFPair);
begin
  SetName(AValue.Key);
  AsString := AValue.Value;
end;

procedure TEFNode.SetAsPairs(const AValue: TEFPairs);
begin
  if not IsDataTypeLocked then
    FDataType := TEFDataTypeFactory.Instance.GetDataType('String');
  Value := DataType.PairsToValue(AValue);
end;

procedure TEFNode.SetAsString(const AValue: string);
begin
  if not IsDataTypeLocked then
    FDataType := TEFDataTypeFactory.Instance.GetDataType('String');
  Value := DataType.StringToValue(AValue);
end;

procedure TEFNode.SetAsStringArray(const AValue: TStringDynArray);
begin
  if not IsDataTypeLocked then
    FDataType := TEFDataTypeFactory.Instance.GetDataType('String');
  Value := DataType.StringArrayToValue(AValue);
end;

procedure TEFNode.SetAsTime(const AValue: TTime);
begin
  if not IsDataTypeLocked then
    FDataType := TEFDataTypeFactory.Instance.GetDataType('Time');
  Value := DataType.TimeToValue(AValue);
end;

function TEFNode.SetAsYamlValue(const AValue: string; const AFormatSettings: TFormatSettings): TEFNode;
begin
  DataType.YamlValueToNode(AValue, Self, AFormatSettings);
  Result := Self;
end;

procedure TEFNode.SetChildStrings(const AStrings: TStrings);
var
  I: Integer;
begin
  Assert(Assigned(AStrings));

  ClearChildren;
  for I := 0 to AStrings.Count - 1 do
    AddChild(AStrings.Names[I], AStrings.ValueFromIndex[I]);
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
var
  LOldValue: Variant;
  LNewValue: Variant;
  LDoIt: Boolean;
begin
  LDoIt := True;
  LNewValue := AValue;
  ValueChanging(FValue, LNewValue, LDoIt);
  if LDoIt then
  begin
    LOldValue := FValue;
    FValue := LNewValue;
    if not IsDataTypeLocked then
      FDataType := GetVariantDataType(FValue);
    if (FDataType <> GetVariantDataType(LOldValue)) or not CompareValues(LOldValue, FValue) then
      ValueChanged(LOldValue, FValue);
  end;
end;

function TEFNode.CompareValues(const AValue1, AValue2: Variant): Boolean;
begin
  { TODO : Find an efficient way to compare byte arrays; the <> operator won't do.
    For now, we consider all arrays different. }
  if VarIsArray(AValue1) or VarIsArray(AValue2) then
    Result := False
  else
    Result := VarSameValue(AValue1, AValue2);
end;

procedure TEFNode.SetToNull(const AForceChangeNotification: Boolean);
var
  LOldValue: Variant;
begin
  if AForceChangeNotification or not VarIsNull(FValue) then
  begin
    LOldValue := FValue;
    FValue := Null;
    ValueChanged(LOldValue, FValue);
  end;
end;

procedure TEFNode.ValueChanged(const AOldValue, ANewValue: Variant);
begin
end;

procedure TEFNode.ValueChanging(const AOldValue: Variant; var ANewValue: Variant; var ADoIt: Boolean);
begin
end;

function TEFNode.FindNode(const APath: string;
  const ACreateMissingNodes: Boolean): TEFNode;
begin
  if APath = '' then
    Result := Self
  else
    Result := inherited FindNode(APath, ACreateMissingNodes);
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
  Result := (Value = AValue) or (IsNull and VarIsNull(AValue));
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
  Assert(AName <> '');

  Result := AddChild(GetChildClass(AName).Create(AName, AValue));
end;

function TEFTree.AddChild(const ANode: TEFNode): TEFNode;
begin
  Assert(Assigned(ANode));

  ANode.FParent := Self;
  FNodes.Add(ANode);
  Result := ANode;
end;

function TEFTree.AddAnnotation(const AAnnotation: string): Integer;
begin
  Result := GetAnnotations.Add(AAnnotation);
end;

function TEFTree.AddChild(const AName: string): TEFNode;
begin
  Assert(AName <> '');

  Result := AddChild(GetChildClass(AName).Create(AName));
end;

procedure TEFTree.Assign(const ASource: TEFTree; const AProc: TAssignNodeProc);
var
  LNode: TEFNode;
begin
  Clear;
  if Assigned(ASource) then
  begin
    if ASource.AnnotationCount > 0 then
      GetAnnotations.Assign(ASource.FAnnotations);
    for LNode in ASource.FNodes do
      AddChild(GetChildClass(LNode.Name).Clone(LNode, AProc));
  end;
end;

procedure TEFTree.Merge(const ASource: TEFTree);
var
  LYourNode: TEFNode;
  LMyNode: TEFNode;
begin
  if Assigned(ASource) then
  begin
    for LYourNode in ASource.FNodes do
    begin
      LMyNode := FindNode(LYourNode.Name);
      if Assigned(LMyNode) then
        LMyNode.Assign(LYourNode)
      else
        AddChild(GetChildClass(LYourNode.Name).Clone(LYourNode));
    end;
  end;
end;

function TEFTree.QueryInterface(const IID: TGUID; out Obj): HRESULT;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

procedure TEFTree.AssignAnnotations(const AStrings: TStrings);
begin
  GetAnnotations.Assign(AStrings);
end;

procedure TEFTree.BeforeSave;
var
  I: Integer;
begin
  for I := 0 to ChildCount - 1 do
    Children[I].BeforeSave;
end;

procedure TEFTree.Clear;
begin
  ClearChildren;
end;

procedure TEFTree.ClearChildren;
begin
  FNodes.Clear;
end;

constructor TEFTree.Clone(const ASource: TEFTree; const AProc: TAssignNodeProc);
begin
  Create;
  Assign(ASource, AProc);
end;

constructor TEFTree.Create;
begin
  inherited Create;
  FNodes := TEFNodes.Create(True);
end;

procedure TEFTree.RemoveChild(const ANode: TEFNode);
begin
  FNodes.Remove(ANode);
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
  FreeAndNil(FAnnotations);
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

function TEFTree.FindChild(const AName: string; const ACreateMissingNode: Boolean;
  const ARecursively: Boolean): TEFNode;
begin
  Result := FindChildByPredicate(
    function (const ANode: TEFNode): Boolean
    begin
      Result := SameText(ANode.Name, AName);
    end,
    ARecursively);
  if (Result = nil) and ACreateMissingNode then
    Result := AddChild(AName);
end;

function TEFTree.FindChildByNameAndValue(const AName: string;
  const AValue: Variant; const ARecursively: Boolean): TEFNode;
var
  LValue: Variant;
begin
  LValue := AValue; // works around "cannot capture symbol" error.
  Result := FindChildByPredicate(
    function (const ANode: TEFNode): Boolean
    begin
      Result := SameText(ANode.Name, AName) and SameText(ANode.Value, LValue);
    end,
    ARecursively);
end;

function TEFTree.FindChildByValue(const AValue: Variant; const ARecursively: Boolean): TEFNode;
var
  LValue: Variant;
begin
  LValue := AValue; // works around "cannot capture symbol" error.
  Result := FindChildByPredicate(
    function (const ANode: TEFNode): Boolean
    begin
      Result := SameText(ANode.Value, LValue);
    end,
    ARecursively);
end;

function TEFTree.FindChildByPredicate(const APredicate: TNodePredicate;
  const ARecursively: Boolean): TEFNode;
var
  I: Integer;
  LChildNode: TEFNode;
begin
  Assert(Assigned(APredicate));

  Result := nil;
  for I := 0 to ChildCount - 1 do
  begin
    LChildNode := Children[I];
    if APredicate(LChildNode) then
    begin
      Result := LChildNode;
      Break;
    end;
    if ARecursively and (LChildNode.ChildCount > 0) then
    begin
      Result := LChildNode.FindChildByPredicate(APredicate, ARecursively);
      if Assigned(Result) then
        Break;
    end;
  end;
end;

function TEFTree.FindNode(const APaths: TStringDynArray): TEFNode;
var
  APath: string;
begin
  Result := nil;
  for APath in APaths do
  begin
    Result := FindNode(APath);
    if Assigned(Result) then
      Break;
  end;
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

function TEFTree.GetChildIndex<T>(const AChild: T): Integer;
var
  I: Integer;
  LTIndex: Integer;
begin
  Result := -1;
  for I := 0 to ChildCount - 1 do
  begin
    // Don't use "is" here. Either a bug in generics implementation or as designed.
    if Children[I].InheritsFrom(T) then
    begin
      Inc(Result);
      if T(Children[I]) = AChild then
        Break;
    end;
  end;
end;

function TEFTree.GetChildrenAsPairs(const APath: string;
  const AExpandMacrosInValues: Boolean; const ADefaultValue: TEFPairs): TEFPairs;
var
  LNode: TEFNode;
begin
  LNode := FindNode(APath);
  if Assigned(LNode) then
    Result := LNode.GetChildPairs(AExpandMacrosInValues)
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
    MonitorEnter(Self);
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
      MonitorExit(Self);
    end;
  end;
end;

function TEFTree.GetNode(const APath: string; const ACreateMissingNodes: Boolean): TEFNode;
begin
  Result := FindNode(APath, ACreateMissingNodes);
  if not Assigned(Result) then
    raise EEFError.CreateFmt(_('Node %s not found.'), [APath]);
end;

function TEFTree.GetAnnotation(const AIndex: Integer): string;
begin
  Result := GetAnnotations[AIndex];
end;

function TEFTree.GetAnnotationCount: Integer;
begin
  if not Assigned(FAnnotations) then
    Result := 0
  else
    Result := FAnnotations.Count;
end;

function TEFTree.GetAnnotations: TStrings;
begin
  if not Assigned(FAnnotations) then
    FAnnotations := TStringList.Create;
  Result := FAnnotations;
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

function TEFTree.GetObject(const APath: string; const ADefaultValue: TObject): TObject;
var
  LNode: TEFNode;
begin
  LNode := FindNode(APath);
  if Assigned(LNode) then
    Result := LNode.AsObject
  else
    Result := ADefaultValue;
end;

function TEFTree.GetObject<T>(const APath: string): T;
begin
  Result := T(GetObject(APath, nil));
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

function TEFTree.GetPath: string;
begin
  Result := '';
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

function TEFTree.GetChar(const APath: string;
  const ADefaultValue: Char = #0): Char;
var
  LNode: TEFNode;
begin
  LNode := FindNode(APath);
  if Assigned(LNode) then
    Result := LNode.AsChar
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

function TEFTree.HasChild(const AChild: TEFNode; const ARecursively: Boolean): Boolean;
begin
  Result := Assigned(FindChildByPredicate(
    function (const ANode: TEFNode): Boolean
    begin
      Result := ANode = AChild;
    end,
    ARecursively));
end;

function TEFTree.HasChild(const AName: string; const ARecursive: Boolean): Boolean;
begin
  Result := Assigned(FindChild(AName, False, ARecursive));
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
  Result := GetString(APath, ADefaultValue);
  TEFMacroExpansionEngine.Instance.Expand(Result);
end;

function TEFTree.GetFloat(const APath: string;
  const ADefaultValue: Double): Double;
var
  LNode: TEFNode;
begin
  LNode := FindNode(APath);
  if Assigned(LNode) then
    Result := LNode.AsFloat
  else
    Result := ADefaultValue;
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

function TEFTree.GetChildrenAsStrings(const APath: string;
  const AStrings: TStrings): Integer;
begin
  Assert(Assigned(AStrings));

  AStrings.Text := GetChildrenAsStrings(APath);
  Result := AStrings.Count;
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

function TEFTree.GetChildrenAsExpandedStrings(const APath, ASeparator, AConnector,
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

procedure TEFTree.SetAnnotation(const AIndex: Integer; const AValue: string);
begin
  GetAnnotations[AIndex] := AValue;
end;

procedure TEFTree.SetBoolean(const APath: string; const AValue: Boolean);
begin
  GetNode(APath, True).AsBoolean := AValue;
end;

procedure TEFTree.SetChildrenAsStrings(const APath: string; const AStrings: TStrings);
begin
  GetNode(APath, True).SetChildStrings(AStrings);
end;

procedure TEFTree.SetDateTime(const APath: string; const AValue: TDateTime);
begin
  GetNode(APath, True).AsDateTime := AValue;
end;

function TEFTree.SetFloat(const APath: string; const AValue: Double): TEFNode;
begin
  Result := GetNode(APath, True);
  Result.AsFloat := AValue;
end;

procedure TEFTree.SetInteger(const APath: string; const AValue: Integer);
begin
  GetNode(APath, True).AsInteger := AValue;
end;

procedure TEFTree.SetObject(const APath: string; const AValue: TObject);
begin
  GetNode(APath, True).AsObject := AValue;
end;

procedure TEFTree.DoSetPropertyFromNode(const AType: TRttiType; const AInstance: TObject;
  const ANode: TEFNode; const APath: string; const APathIsName: Boolean);
var
  LParts: TArray<string>;
  LPropertyName: string;
  LProperty: TRttiProperty;
begin
  Assert(Assigned(AType));
  Assert(Assigned(AInstance));
  Assert(not APath.IsEmpty);

  if Assigned(ANode) then
  begin
    if APathIsName or not APath.Contains('/') then
      LPropertyName := APath
    else
    begin
      LParts := APath.Split(['/']);
      LPropertyName := LParts[High(LParts)];
    end;
    LProperty := AType.GetProperty(LPropertyName);
    if not Assigned(LProperty) then
      raise Exception.CreateFmt('Property %s not found', [LPropertyName]);
    LProperty.SetValue(AInstance, TValue.FromVariant(ANode.Value));
  end;
end;

function TEFTree.GetRttiType(const AClassType: TClass): TRttiType;
begin
  Result := FRttiContext.GetType(AClassType);
  if not Assigned(Result) then
    raise Exception.Create('RTTI not available');
end;

procedure TEFTree.SetPropertiesFromChildNodes(const AInstance: TObject);
var
  LType: TRttiType;
begin
  LType := GetRttiType(AInstance.ClassType);
  EnumChildren(
    procedure (const ANode: TEFNode)
    begin
      DoSetPropertyFromNode(LType, AInstance, ANode, ANode.Name, True);
    end
  );
end;

procedure TEFTree.SetPropertiesFromNode(const AInstance: TObject; const APaths: TArray<string>; const APathIsName: Boolean);
var
  LType: TRttiType;
  LPath: string;
begin
  LType := GetRttiType(AInstance.ClassType);
  for LPath in APaths do
    DoSetPropertyFromNode(LType, AInstance, FindNode(LPath), LPath, APathIsName);
end;

procedure TEFTree.SetPropertyFromNode(const AInstance: TObject; const APath: string; const APathIsName: Boolean);
begin
  DoSetPropertyFromNode(GetRttiType(AInstance.ClassType), AInstance, FindNode(APath), APath, APathIsName);
end;

function TEFTree.SetString(const APath, AValue: string): TEFNode;
begin
  Result := GetNode(APath, True);
  Result.AsString := AValue;
end;

function TEFTree.SetValue(const APath: string; const AValue: Variant): TEFNode;
begin
  Result := GetNode(APath, True);
  Result.Value := AValue;
end;

procedure TEFTree.Sort(const ACompareFunc: TEFNodeCompareFunc);
begin
  NodeList.Sort(TComparer.Create(ACompareFunc));
end;

function TEFTree._AddRef: Integer;
begin
  Result := -1;
end;

function TEFTree._Release: Integer;
begin
  Result := -1;
end;

procedure TEFTree.EnumChildren(const AProc: TNodeProc; const APredicate: TNodePredicate);
var
  I: Integer;
begin
  Assert(Assigned(AProc));

  for I := 0 to ChildCount - 1 do
  begin
    if not Assigned(APredicate) or APredicate(Children[I]) then
      AProc(Children[I]);
  end;
end;

procedure TEFTree.CopyChildValues(
  const ASource: TEFTree; const AUseJSDateFormat: Boolean;
  const AFormatSettings: TFormatSettings; const ATranslator: TNameTranslator;
  const AValueIndex: Integer);
var
  I: Integer;
  LChild: TEFNode;
  LName: string;
  LStringValue: string;
  LStringValues: TStringDynArray;

  function Translate(const AName: string): string;
  begin
    if Assigned(ATranslator) then
      Result := ATranslator(AName)
    else
      Result := AName;
  end;

begin
  for I := 0 to ChildCount - 1 do
  begin
    LChild := Children[I];
    LName := Translate(LChild.Name);
    Assert(LName <> '');
    LStringValue := ASource.ChildByName(LName).AsString;
    if LStringValue <> '' then
    begin
      if AValueIndex >= 0 then
      begin
        LStringValues := Split(LStringValue, ',');
        if Length(LStringValues) > AValueIndex then
          LStringValue := LStringValues[AValueIndex]
        else
          LStringValue := '';
      end;
      LChild.SetAsJSONValue(LStringValue, AUseJSDateFormat, AFormatSettings);
    end
    // Checkboxes are not submitted when unchecked, which for us means False.
    else if LChild.DataType is TEFBooleanDataType then
      LChild.AsBoolean := False;
  end;
end;

{ TEFTreeFactory }

class function TEFTreeFactory.LoadFromFile<T>(const AFileName: string): T;
begin
  Result := T.Create;
  ReloadFromFile(Result, AFileName);
end;

class procedure TEFTreeFactory.ReloadFromFile(const ATree: TEFTree; const AFileName: string);
var
  LReader: TEFYAMLReader;
begin
  Assert(Assigned(ATree));

  LReader := TEFYAMLReader.Create;
  try
    LReader.LoadTreeFromFile(ATree, AFileName);
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

procedure TEFTreeMacroExpander.InternalExpand(var AString: string);
begin
  inherited InternalExpand(AString);

  if Assigned(FTree) then
    ExpandTreeMacros(AString, FTree);
end;

procedure TEFTreeMacroExpander.ExpandTreeMacros(var AString: string; const ATree: TEFTree);
var
  LIndex: Integer;
  LStart: Integer;
  LPathStart: Integer;
  LEnd: Integer;
  LNodePath: string;
  LNodeValue: string;
  LNode: TEFNode;
begin
  Assert(Assigned(ATree));

  LIndex := 1;
  repeat
    LStart := PosEx('%' + FPrefix, AString, LIndex);
    if LStart = 0 then
      Exit;
    LPathStart := LStart + 1 + Length(FPrefix);
    LEnd := PosEx('%', AString, LStart + 1);
    if LEnd = 0 then
      Exit;
    LNodePath := Copy(AString, LPathStart, LEnd - LPathStart);
    LNode := ATree.FindNode(StripPrefixAndSuffix(LNodePath, '%', '%'));
    if Assigned(LNode) then
    begin
      LNodeValue := LNode.AsExpandedString;
      Delete(AString, LStart, LEnd - LStart + 1);
      Insert(LNodeValue, AString, LStart);
    end;
    LIndex := LEnd + 1;
  until LIndex > Length(AString);
end;

{ TEFDataType }

procedure TEFDataType.FieldValueToNode(const AField: TField; const ANode: TEFNode);
begin
  Assert(Assigned(AField));
  Assert(Assigned(ANode));

  ANode.LockDataType;
  try
    if AField.IsNull then
      ANode.SetToNull
    else
      InternalFieldValueToNode(AField, ANode);
  finally
    ANode.UnlockDataType;
  end;
end;

procedure TEFDataType.InternalFieldValueToNode(const AField: TField; const ANode: TEFNode);
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
    ftBlob: ANode.AsBytes := AField.AsBytes;
  else
    raise EEFError.CreateFmt('TEFDataType.InternalFieldValueToNode: Field data type %s not supported.',
      [GetEnumName(TypeInfo(TFieldType), Ord(AField.DataType))]);
  end;
end;

procedure TEFDataType.InternalJSONValueToNode(const ANode: TEFNode;
  const AValue: string; const AUseJSDateFormat: Boolean;
  const AJSFormatSettings: TFormatSettings);
begin
  ANode.AsString := AValue;
end;

function TEFDataType.InternalFormatNodeValue(const AForDisplay: Boolean;
  const ANode: TEFNode; const AFormatSettings: TFormatSettings): string;
begin
  Result := ANode.AsString;
end;

procedure TEFDataType.InternalNodeToParam(const ANode: TEFNode; const AParam: TParam);
begin
  raise EEFError.CreateFmt('%s.InternalNodeToParam: Unsupported call.', [ClassName]);
end;

procedure TEFDataType.InternalNodeToField(const ANode: TEFNode; const AField: TField);
begin
  case AField.DataType of
    ftString, ftMemo, ftFixedChar, ftWideString, ftWideMemo: AField.AsString := ANode.AsString;
    ftSmallint, ftWord, ftInteger, ftAutoInc: AField.AsInteger := ANode.AsInteger;
    ftBoolean: AField.AsBoolean := ANode.AsBoolean;
    ftDate: AField.AsDateTime := ANode.AsDate;
    ftTime: AField.AsDateTime := ANode.AsTime;
    ftDateTime, ftTimeStamp: AField.AsDateTime := ANode.AsDateTime;
    ftCurrency: AField.AsCurrency := ANode.AsCurrency;
    ftFloat: AField.AsFloat := ANode.AsFloat;
    ftBCD, ftFMTBcd: AField.AsBCD := ANode.AsDecimal;
    ftBlob: AField.AsBytes := ANode.AsBytes;
  else
    raise EEFError.CreateFmt('TEFDataType.InternalFieldValueToNode: Field data type %s not supported.',
      [GetEnumName(TypeInfo(TFieldType), Ord(AField.DataType))]);
  end;
end;

class procedure TEFDataType.SetNodeDataTypeAndValueFromYaml(const AYamlValue: string;
  const ANode: TEFNode; const AFormatSettings: TFormatSettings;
  const APreferStrings: Boolean);
var
  LInteger: Integer;
  LDouble: Double;
  LDateTime: TDateTime;
  LBoolean: Boolean;
  LCouldBeDateTime: Boolean;
begin
  Assert(Assigned(ANode));

  LCouldBeDateTime := Length(AYamlValue) >= 5;

  // Numbers are treated as strings.
  if APreferStrings then
  begin
    if LCouldBeDateTime and TryStrToDateTime(AYamlValue, LDateTime, AFormatSettings) then
      ANode.AsDateTime := LDateTime
    else if LCouldBeDateTime and TryStrToDate(AYamlValue, LDateTime, AFormatSettings) then
      ANode.AsDate := LDateTime
    else if LCouldBeDateTime and TryStrToTime(AYamlValue, LDateTime, AFormatSettings) then
      ANode.AsTime := LDateTime
    else
      ANode.AsString := AYamlValue;
  end
  else
  begin
    if TryStrToInt(AYamlValue, LInteger) then
      ANode.AsInteger := LInteger
    else if TryStrToFloat(AYamlValue, LDouble, AFormatSettings) then
      ANode.AsFloat := LDouble
    else if LCouldBeDateTime and TryStrToDateTime(AYamlValue, LDateTime, AFormatSettings) then
      ANode.AsDateTime := LDateTime
    else if LCouldBeDateTime and TryStrToDate(AYamlValue, LDateTime, AFormatSettings) then
      ANode.AsDate := LDateTime
    else if LCouldBeDateTime and TryStrToTime(AYamlValue, LDateTime, AFormatSettings) then
      ANode.AsTime := LDateTime
    else if TryStrToBool(AYamlValue, LBoolean) then
      ANode.AsBoolean := LBoolean
    else
      ANode.AsString := AYamlValue;
  end;
end;

function TEFDataType.IsBlob(const ASize: Integer): Boolean;
begin
  Result := False;
end;

function TEFDataType.IsBoolean: Boolean;
begin
  Result := False;
end;

function TEFDataType.IsText: Boolean;
begin
  Result := False;
end;

procedure TEFDataType.JSONValueToNode(const ANode: TEFNode;
  const AValue: string; const AUseJSDateFormat: Boolean;
  const AJSFormatSettings: TFormatSettings);
begin
  Assert(Assigned(ANode));

  if SupportsEmptyAsNull and ANode.GetEmptyAsNull and (AValue = '') then
    ANode.SetToNull
  else if SameText(AValue, 'null') or (AValue = '') then
    ANode.SetToNull
  else
    InternalJSONValueToNode(ANode, AValue, AUseJSDateFormat, AJSFormatSettings);
end;

function TEFDataType.NodeToJSONValue(const AForDisplay: Boolean;
  const ANode: TEFNode; const AJSFormatSettings: TFormatSettings;
  const AQuote: Boolean; const AEmptyNulls: Boolean): string;
begin
  Assert(Assigned(ANode));

  if ANode.IsNull then
    Result := IfThen(AEmptyNulls, '', 'null')
  else
  begin
    Result := JSONEscape(InternalFormatNodeValue(AForDisplay, ANode, AJSFormatSettings));
    if AQuote then
      Result := QuoteJSONStr(Result);
  end;
end;

procedure TEFDataType.NodeToParam(const ANode: TEFNode; const AParam: TParam);
begin
  Assert(Assigned(ANode));
  Assert(Assigned(AParam));

  if ANode.IsNull then
  begin
    AParam.Clear;
    if AParam.DataType = ftUnknown then
      AParam.DataType := GetFieldType;
  end
  else
    InternalNodeToParam(ANode, AParam);
end;

function TEFDataType.NodeToXMLValue(const AForDisplay: Boolean;
  const ANode: TEFNode; const AFormatSettings: TFormatSettings;
  const AEmptyNulls: Boolean = False): string;
var
  LTagName: string;
begin
  Assert(Assigned(ANode));
  LTagName := ANode.Name;
  Result := InternalFormatNodeValue(AForDisplay, ANode, AFormatSettings);

  if XMLHeaderPos(Result) > 0 then
  begin
    ClearXMLHeader(Result);
    ClearDOCTYPE(Result);
    ClearXmlNameSpaces(Result);
  end
  else
    Result := XMLEscape(Result);
  if not ((Result = '') and AEmptyNulls) then
    Result := Format(XMLTagFormat, [LTagName, Result, LTagName]);
end;

class function TEFDataType.NeedsQuotes: Boolean;
begin
  Result := True;
end;

procedure TEFDataType.NodeToField(const ANode: TEFNode; const AField: TField);
begin
  Assert(Assigned(ANode));
  Assert(Assigned(AField));

  if ANode.IsNull then
    AField.Clear
  else
    InternalNodeToField(ANode, AField);
end;

function TEFDataType.SupportsEmptyAsNull: Boolean;
begin
  Result := False;
end;

function TEFDataType.SupportsJSON: Boolean;
begin
  Result := True;
end;

function TEFDataType.SupportsXML: Boolean;
begin
  Result := True;
end;

function TEFDataType.GetDefaultColumnAlignment: string;
begin
  Result := 'left';
end;

function TEFDataType.GetDefaultDisplayWidth(const ASize: Integer): Integer;
begin
  Result := 20;
end;

function TEFDataType.GetDefaultEmptyAsNull: Boolean;
begin
  Result := False;
end;

class function TEFDataType.GetFieldType: TFieldType;
begin
  Result := ftUnknown;
end;

function TEFDataType.GetJSTypeName: string;
begin
  Result := 'auto';
end;

class function TEFDataType.GetTypeName: string;
begin
  Result := StripPrefixAndSuffix(ClassName, 'TEF', 'DataType');
end;

class function TEFDataType.HasScale: Boolean;
begin
  Result := False;
end;

class function TEFDataType.HasSize: Boolean;
begin
  Result := False;
end;

procedure TEFDataType.YamlValueToNode(const AYamlValue: string;
  const ANode: TEFNode; const AFormatSettings: TFormatSettings);
begin
  Assert(Assigned(ANode));

  InternalYamlValueToNode(AYamlValue, ANode, AFormatSettings);
end;

function TEFDataType.ValueToBoolean(const AValue: Variant): Boolean;
begin
  Result := EFVarToBoolean(AValue);
end;

function TEFDataType.ValueToBytes(const AValue: Variant): TBytes;
begin
  Result := AValue;
end;

function TEFDataType.ValueToChar(const AValue: Variant): Char;
var
  LValue: string;
begin
  LValue := VarToStr(AValue);
  if Length(LValue) > 0 then
    Result := LValue[1]
  else
    Result := #0;
end;

function TEFDataType.ValueToCurrency(const AValue: Variant): Currency;
begin
  Result := EFVarToCurrency(AValue);
end;

function TEFDataType.ValueToDate(const AValue: Variant): TDate;
var
  LFormatSetting: TFormatSettings;
begin
  if VarIsStr(AValue) then
  begin
    LFormatSetting.ShortDateFormat := 'yyyy-mm-dd';
    LFormatSetting.DateSeparator := '-';
    Result := StrToDateTime(AValue, LFormatSetting);
  end
  else if VarIsNull(AValue) then
    Result := 0
  else
    Result := AValue;
end;

function TEFDataType.ValueToDateTime(const AValue: Variant): TDateTime;
var
  LFormatSetting: TFormatSettings;
begin
  if VarIsStr(AValue) then
  begin
    LFormatSetting.ShortDateFormat := 'yyyy-mm-dd hh:mm:ss';
    LFormatSetting.DateSeparator := '-';
    LFormatSetting.TimeSeparator := ':';
    Result := StrToDateTime(AValue, LFormatSetting);
  end
  else if VarIsNull(AValue) then
    Result := 0
  else
    Result := AValue;
end;

function TEFDataType.ValueToDecimal(const AValue: Variant): TBcd;
begin
  if not VarIsNull(AValue) then
    Result := VarToBcd(AValue)
  else
    Result := 0;
end;

function TEFDataType.ValueToFloat(const AValue: Variant): Double;
begin
  if VarIsNull(AValue) or VarIsEmpty(AValue) then
    Result := 0
  else
    Result := EFVarToFloat(AValue);
end;

function TEFDataType.ValueToInteger(const AValue: Variant): Integer;
const
  KB = 1024;
  MB = KB * 1024;
begin
  if VarIsNull(AValue) or VarIsEmpty(AValue) then
    Result := 0
  else if EndsStr('MB', AValue) then
    Result := MB * StrToInt(StripSuffix(AValue, 'MB'))
  else if EndsStr('KB', AValue) then
    Result := KB * StrToInt(StripSuffix(AValue, 'KB'))
  else
    Result := EFVarToInt(AValue);
end;

function TEFDataType.ValueToObject(const AValue: Variant): TObject;
begin
  if VarIsNull(AValue) or VarIsEmpty(AValue) then
    Result := nil
  else
    Result := TObject(NativeInt(AValue));
end;

function TEFDataType.ValueToPairs(const AValue: Variant): TEFPairs;
begin
  Result := SplitPairs(AValue, ' ');
end;

function TEFDataType.ValueToString(const AValue: Variant): string;
begin
  Result := EFVarToStr(AValue);
end;

function TEFDataType.ValueToStringArray(const AValue: Variant): TStringDynArray;
begin
  Result := Split(AValue, ' ');
end;

function TEFDataType.ValueToTime(const AValue: Variant): TTime;
var
  LFormatSetting: TFormatSettings;
  LDateTime: TDateTime;
begin
  if VarIsStr(AValue) then
  begin
    LFormatSetting.ShortDateFormat := 'yyyy-mm-dd hh:mm:ss';
    LFormatSetting.DateSeparator := '-';
    LFormatSetting.TimeSeparator := ':';
    LDateTime := StrToDateTime(AValue, LFormatSetting);
    Result := LDateTime;
  end
  else
    Result := TimeOf(EFVarToDateTime(AValue));
end;

function TEFDataType.StringArrayToValue(const AStringArray: TStringDynArray): Variant;
begin
  Result := Join(AStringArray, ' ');
end;

function TEFDataType.StringToValue(const AString: string): Variant;
begin
  Result := AString;
end;

function TEFDataType.TimeToValue(const ATime: TTime): Variant;
var
  LTime: TDateTime;
begin
  LTime := Frac(ATime);
  Result := LTime;
end;

function TEFDataType.BooleanToValue(const ABoolean: Boolean): Variant;
begin
  Result := ABoolean;
end;

function TEFDataType.DateTimeToValue(const ADateTime: TDateTime): Variant;
begin
  Result := ADateTime;
end;

function TEFDataType.DateToValue(const ADate: TDate): Variant;
var
  LDate: TDateTime;
begin
  LDate := Trunc(ADate);
  Result := LDate;
end;

function TEFDataType.DecimalToValue(const ADecimal: TBcd): Variant;
begin
  Result := BcdToDouble(ADecimal);
end;

function TEFDataType.BytesToValue(const ABytes: TBytes): Variant;
begin
  Result := ABytes;
end;

function TEFDataType.CharToValue(const AChar: Char): Variant;
begin
  if AChar = #0 then
    Result := NULL
  else
    Result := AChar;
end;

function TEFDataType.CurrencyToValue(const ACurrency: Currency): Variant;
begin
  Result := ACurrency;
end;

function TEFDataType.FloatToValue(const AFloat: Double): Variant;
begin
  Result := AFloat;
end;

function TEFDataType.IntegerToValue(const AInteger: Integer): Variant;
begin
  Result := AInteger;
end;

function TEFDataType.ObjectToValue(const AObject: TObject): Variant;
begin
  Result := NativeInt(Pointer(AObject));
end;

function TEFDataType.PairsToValue(const APairs: TEFPairs): Variant;
begin
  Result := JoinPairs(APairs, ' ');
end;

{ TEFIntegerDataType }

function TEFIntegerDataType.GetDefaultColumnAlignment: string;
begin
  Result := 'right';
end;

function TEFIntegerDataType.GetDefaultDisplayWidth(
  const ASize: Integer): Integer;
begin
  Result := 5;
end;

class function TEFIntegerDataType.GetFieldType: TFieldType;
begin
  Result := ftInteger;
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

procedure TEFIntegerDataType.InternalJSONValueToNode(const ANode: TEFNode;
  const AValue: string; const AUseJSDateFormat: Boolean;
  const AJSFormatSettings: TFormatSettings);
begin
  ANode.AsInteger := StrToInt(StripThousandSeparator(AValue, AJSFormatSettings));
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

class function TEFDateDataType.GetFieldType: TFieldType;
begin
  Result := ftDate;
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

procedure TEFDateDataType.InternalJSONValueToNode(const ANode: TEFNode;
  const AValue: string; const AUseJSDateFormat: Boolean;
  const AJSFormatSettings: TFormatSettings);
begin
  if AUseJSDateFormat then
    ANode.AsDate := JSDateToDateTime(AValue)
  else
    ANode.AsDate := StrToDateTime(AValue, AJSFormatSettings);
end;

function TEFDateDataType.InternalFormatNodeValue(const AForDisplay: Boolean;
  const ANode: TEFNode; const AFormatSettings: TFormatSettings): string;
begin
  Result := FormatDateTime(AFormatSettings.ShortDateFormat, ANode.AsDate, AFormatSettings);
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
  Result := 6;
end;

class function TEFTimeDataType.GetFieldType: TFieldType;
begin
  Result := ftTime;
end;

procedure TEFTimeDataType.InternalFieldValueToNode(const AField: TField;
  const ANode: TEFNode);
begin
  ANode.AsTime := AField.AsDateTime;
end;

procedure TEFTimeDataType.InternalJSONValueToNode(const ANode: TEFNode;
  const AValue: string; const AUseJSDateFormat: Boolean;
  const AJSFormatSettings: TFormatSettings);
begin
  if AUseJSDateFormat then
    ANode.AsTime := JSDateToDateTime(AValue)
  else
    ANode.AsTime := StrToDateTime(AValue, AJSFormatSettings);
end;

function TEFTimeDataType.InternalFormatNodeValue(const AForDisplay: Boolean;
  const ANode: TEFNode; const AFormatSettings: TFormatSettings): string;
begin
  Result := FormatDateTime(AFormatSettings.ShortTimeFormat, ANode.AsTime, AFormatSettings);
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
  Result := 15;
end;

class function TEFDateTimeDataType.GetFieldType: TFieldType;
begin
  Result := ftDateTime;
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

procedure TEFDateTimeDataType.InternalJSONValueToNode(const ANode: TEFNode;
  const AValue: string; const AUseJSDateFormat: Boolean;
  const AJSFormatSettings: TFormatSettings);
begin
  if AUseJSDateFormat then
    ANode.AsDateTime := JSDateToDateTime(AValue)
  else
    ANode.AsDateTime := StrToDateTime(AValue, AJSFormatSettings);
end;

function TEFDateTimeDataType.InternalFormatNodeValue(const AForDisplay: Boolean;
  const ANode: TEFNode; const AFormatSettings: TFormatSettings): string;
begin
  // DateTimeToStr will omit the time portion if it's empty, but the GUI needs
  // it in all cases.
  Result := DateToStr(ANode.AsDate, AFormatSettings) + ' ' + TimeToStr(ANode.AsTime, AFormatSettings);
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

class function TEFBooleanDataType.GetFieldType: TFieldType;
begin
  Result := ftBoolean;
end;

function TEFBooleanDataType.GetJSTypeName: string;
begin
  Result := 'boolean';
end;

procedure TEFBooleanDataType.InternalFieldValueToNode(const AField: TField;
  const ANode: TEFNode);
begin
  { TODO : create a way to define the mapped values when writing fake boolean fields }
  case AField.DataType of
    ftBoolean: ANode.AsBoolean := AField.AsBoolean;
    ftInteger, ftSmallint: ANode.AsBoolean := AField.AsInteger = 1;
    ftString, ftWideString, ftFixedChar, ftFixedWideChar:
      ANode.AsBoolean := MatchText(AField.AsString, ['true', 't', 'y', 'yes', 'on'])
  else
    inherited;
  end;
end;

procedure TEFBooleanDataType.InternalJSONValueToNode(const ANode: TEFNode;
  const AValue: string; const AUseJSDateFormat: Boolean;
  const AJSFormatSettings: TFormatSettings);
begin
  ANode.AsBoolean := MatchText(AValue, ['on', 'true']);
end;

function TEFBooleanDataType.InternalFormatNodeValue(const AForDisplay: Boolean;
  const ANode: TEFNode; const AFormatSettings: TFormatSettings): string;
begin
  Result := IfThen(ANode.AsBoolean, 'true', 'false');
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

function TEFBooleanDataType.IsBoolean: Boolean;
begin
  Result := True;
end;

class function TEFBooleanDataType.NeedsQuotes: Boolean;
begin
  Result := False;
end;

{ TEFCurrencyDataType }

function TEFCurrencyDataType.GetDefaultColumnAlignment: string;
begin
  Result := 'right';
end;

function TEFCurrencyDataType.GetDefaultDisplayWidth(
  const ASize: Integer): Integer;
begin
  Result := 12;
end;

class function TEFCurrencyDataType.GetFieldType: TFieldType;
begin
  Result := ftCurrency;
end;

function TEFCurrencyDataType.GetJSTypeName: string;
begin
  Result := 'float';
end;

class function TEFCurrencyDataType.HasScale: Boolean;
begin
  Result := True;
end;

class function TEFCurrencyDataType.HasSize: Boolean;
begin
  Result := False;
end;

procedure TEFCurrencyDataType.InternalFieldValueToNode(const AField: TField;
  const ANode: TEFNode);
begin
  ANode.AsCurrency := AField.AsCurrency;
end;

procedure TEFCurrencyDataType.InternalJSONValueToNode(const ANode: TEFNode;
  const AValue: string; const AUseJSDateFormat: Boolean;
  const AJSFormatSettings: TFormatSettings);
begin
  ANode.AsCurrency := StrToFloat(StripThousandSeparator(AValue, AJSFormatSettings), AJSFormatSettings);
end;

function TEFCurrencyDataType.InternalFormatNodeValue(const AForDisplay: Boolean;
  const ANode: TEFNode; const AFormatSettings: TFormatSettings): string;
begin
  if AForDisplay then
    Result := FormatCurr(',0.' + DupeString('0', AFormatSettings.CurrencyDecimals), ANode.AsCurrency, AFormatSettings)
  else
    Result := FormatCurr('0.' + DupeString('0', AFormatSettings.CurrencyDecimals), ANode.AsCurrency, AFormatSettings);
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

function TEFFloatDataType.GetDefaultColumnAlignment: string;
begin
  Result := 'right';
end;

function TEFFloatDataType.GetDefaultDisplayWidth(const ASize: Integer): Integer;
begin
  Result := 10;
end;

class function TEFFloatDataType.GetFieldType: TFieldType;
begin
  Result := ftFloat;
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

procedure TEFFloatDataType.InternalJSONValueToNode(const ANode: TEFNode;
  const AValue: string; const AUseJSDateFormat: Boolean;
  const AJSFormatSettings: TFormatSettings);
begin
  ANode.AsFloat := StrToFloat(StripThousandSeparator(AValue, AJSFormatSettings), AJSFormatSettings);
end;

function TEFFloatDataType.InternalFormatNodeValue(const AForDisplay: Boolean;
  const ANode: TEFNode; const AFormatSettings: TFormatSettings): string;
begin
  if AForDisplay then
    Result := FormatFloat(',0.' + DupeString('0', AFormatSettings.CurrencyDecimals), ANode.AsFloat, AFormatSettings)
  else
    Result := FormatFloat('0.' + DupeString('0', AFormatSettings.CurrencyDecimals), ANode.AsFloat, AFormatSettings);
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

function TEFDecimalDataType.GetDefaultColumnAlignment: string;
begin
  Result := 'right';
end;

function TEFDecimalDataType.GetDefaultDisplayWidth(
  const ASize: Integer): Integer;
begin
  Result := 10;
end;

class function TEFDecimalDataType.GetFieldType: TFieldType;
begin
  Result := ftFMTBCD;
end;

function TEFDecimalDataType.GetJSTypeName: string;
begin
  Result := 'float';
end;

class function TEFDecimalDataType.HasScale: Boolean;
begin
  Result := True;
end;

class function TEFDecimalDataType.HasSize: Boolean;
begin
  Result := True;
end;

procedure TEFDecimalDataType.InternalFieldValueToNode(const AField: TField;
  const ANode: TEFNode);
begin
  ANode.AsDecimal := AField.AsBCD;
end;

procedure TEFDecimalDataType.InternalJSONValueToNode(const ANode: TEFNode;
  const AValue: string; const AUseJSDateFormat: Boolean;
  const AJSFormatSettings: TFormatSettings);
begin
  ANode.AsDecimal := DoubleToBcd(StrToFloat(StripThousandSeparator(AValue, AJSFormatSettings), AJSFormatSettings));
end;

function TEFDecimalDataType.InternalFormatNodeValue(const AForDisplay: Boolean;
  const ANode: TEFNode; const AFormatSettings: TFormatSettings): string;
begin
  if AForDisplay then
    Result := FormatFloat(',0.' + DupeString('0', AFormatSettings.CurrencyDecimals), BcdToDouble(ANode.AsDecimal), AFormatSettings)
  else
    Result := FormatFloat('0.' + DupeString('0', AFormatSettings.CurrencyDecimals), BcdToDouble(ANode.AsDecimal), AFormatSettings);
end;

procedure TEFDecimalDataType.InternalNodeToParam(const ANode: TEFNode;
  const AParam: TParam);
begin
  AParam.AsFMTBCD := ANode.AsDecimal;
end;

procedure TEFDecimalDataType.InternalYamlValueToNode(const AYamlValue: string;
  const ANode: TEFNode; const AFormatSettings: TFormatSettings);
begin
  {$IFDEF D15+}
  ANode.AsDecimal := StrToBcd(AYamlValue, AFormatSettings);
  {$ELSE}
  ANode.AsDecimal := DoubleToBcd(StrToFloat(AYamlValue, AFormatSettings));
  {$ENDIF}
end;

function TEFDecimalDataType.SupportsEmptyAsNull: Boolean;
begin
  Result := True;
end;

{ TEFStringDataType }

function TEFStringDataType.GetDefaultDisplayWidth(
  const ASize: Integer): Integer;
begin
  Result := Min(80, ASize);
end;

function TEFStringDataType.GetDefaultEmptyAsNull: Boolean;
begin
  Result := True;
end;

class function TEFStringDataType.GetFieldType: TFieldType;
begin
  Result := ftWideString;
end;

function TEFStringDataType.GetJSTypeName: string;
begin
  Result := 'string';
end;

class function TEFStringDataType.HasSize: Boolean;
begin
  Result := True;
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
  Result := ASize = 0;
end;

function TEFStringDataType.IsText: Boolean;
begin
  Result := True;
end;

function TEFStringDataType.SupportsEmptyAsNull: Boolean;
begin
  Result := True;
end;

{ TEFMemoDataType }

class function TEFMemoDataType.HasSize: Boolean;
begin
  Result := False;
end;

function TEFMemoDataType.IsBlob(const ASize: Integer): Boolean;
begin
  Result := True;
end;

{ TEFBlobDataType }

class function TEFBlobDataType.GetFieldType: TFieldType;
begin
  Result := ftBlob;
end;

procedure TEFBlobDataType.InternalNodeToParam(const ANode: TEFNode; const AParam: TParam);
var
  LStream: TBytesStream;
begin
  // Don't use AParam.AsBytes as it will set the data type to ftVarBytes, which
  // is not universally supported by drivers.
  LStream := TBytesStream.Create(ANode.AsBytes);
  try
    AParam.LoadFromStream(LStream, ftBlob);
  finally
    FreeAndNil(LStream);
  end;
end;

function TEFBlobDataType.IsBlob(const ASize: Integer): Boolean;
begin
  Result := True;
end;

function TEFBlobDataType.SupportsJSON: Boolean;
begin
  Result := False;
end;

{ TEFPersistentTree }

function TEFPersistentTree.GetIsPersistent: Boolean;
begin
  Result := PersistentName <> '';
end;

function TEFPersistentTree.GetPersistentFileName: string;
begin
  Result := FPersistentName;
end;

procedure TEFPersistentTree.InternalAfterLoad;
begin
end;

procedure TEFPersistentTree.AfterLoad;
begin
  InternalAfterLoad;
end;

procedure TEFPersistentTree.Assign(const ASource: TEFTree; const AProc: TEFTree.TAssignNodeProc);
begin
  inherited;
  if Assigned(ASource) and (ASource is TEFPersistentTree) then
    FPersistentName := TEFPersistentTree(ASource).PersistentName;
end;

{ TEFDateTimeDataTypeBase }

function TEFDateTimeDataTypeBase.GetDefaultEmptyAsNull: Boolean;
begin
  Result := True;
end;

{ TEFNumericDataTypeBase }

class function TEFNumericDataTypeBase.NeedsQuotes: Boolean;
begin
  Result := False;
end;

function TEFNumericDataTypeBase.StripThousandSeparator(const AValue: string;
  const AFormatSettings: TFormatSettings): string;
begin
  Result := ReplaceStr(AValue, AFormatSettings.ThousandSeparator, '');
end;

{ TEFTree.TComparer }

function TEFTree.TComparer.Compare(const Left, Right: TEFNode): Integer;
begin
  Assert(Assigned(FCompareFunc));

  Result := FCompareFunc(Left, Right);
end;

constructor TEFTree.TComparer.Create(const ACompareFunc: TEFNodeCompareFunc);
begin
  Assert(Assigned(ACompareFunc));

  inherited Create;
  FCompareFunc := ACompareFunc;
end;

initialization
  TEFDataTypeRegistry.Instance.RegisterClass(TEFStringDataType.GetTypeName, TEFStringDataType);
  TEFDataTypeRegistry.Instance.RegisterClass(TEFMemoDataType.GetTypeName, TEFMemoDataType);
  TEFDataTypeRegistry.Instance.RegisterClass(TEFBlobDataType.GetTypeName, TEFBlobDataType);
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
  TEFDataTypeRegistry.Instance.UnregisterClass(TEFBlobDataType.GetTypeName);
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
