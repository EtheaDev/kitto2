unit EF.Tree;

interface

uses
  Types, Generics.Collections,
  EF.Types, EF.Macros;

type
  TEFNode = class;
  TEFNodeClass = class of TEFNode;
  TEFNodes = TObjectList<TEFNode>;

  TEFTree = class
  private
    FNodes: TEFNodes;
    function GetChild(I: Integer): TEFNode;
    function GetChildCount: Integer;
  protected
    function GetChildClass(const AName: string): TEFNodeClass; virtual;
  public
    destructor Destroy; override;
  public
    class function RawValueToString(const ARawValue: string): string; static; inline;
    class function RawValueToInteger(const ARawValue: string): Integer; static; inline;
    class function RawValueToObject(const ARawValue: string): TObject; static; inline;
    class function RawValueToBoolean(const ARawValue: string): Boolean; static; inline;
    class function RawValueToStringArray(const ARawValue: string): TStringDynArray; static; inline;
    class function RawValueToPairs(const ARawValue: string): TEFPairs; static; inline;

    class function StringToRawValue(const AString: string): string; static; inline;
    class function IntegerToRawValue(const AInteger: Integer): string; static; inline;
    class function ObjectToRawValue(const AObject: TObject): string; static; inline;
    class function BooleanToRawValue(const ABoolean: Boolean): string; static; inline;
    class function StringArrayToRawValue(const AStringArray: TStringDynArray): string; static; inline;
    class function PairsToRawValue(const APairs: TEFPairs): string; static; inline;

    constructor Create; virtual;
    constructor Clone(const ASource: TEFTree); virtual;
    procedure Clear; virtual;
    procedure Assign(const ASource: TEFTree); virtual;

    {
      Adds a child node. Returns a reference to the added object.
    }
    function AddChild(const ANode: TEFNode): TEFNode; overload;
    {
      Creates a child node of a type that may depend on the parameters,
      fills and adds it. Then returns a reference to the added object.
    }
    function AddChild(const AName: string = ''; const ARawValue: string = ''): TEFNode; overload;
    property Children[I: Integer]: TEFNode read GetChild; default;
    property ChildCount: Integer read GetChildCount;
    function FindChild(const AName: string; const ACreateMissingNodes: Boolean = False): TEFNode;
    function ChildByName(const AName: string): TEFNode;
    procedure RemoveChild(const ANode: TEFNode);
    procedure ClearChildren;
    property NodeList: TEFNodes read FNodes;

    function FindNode(const APath: string; const ACreateMissingNodes: Boolean = False): TEFNode; virtual;
    function GetNode(const APath: string; const ACreateMissingNodes: Boolean = False): TEFNode;
    procedure DeleteNode(const APath: string);

    function GetBoolean(const APath: string; const ADefaultValue: Boolean = False): Boolean;
    function GetInteger(const APath: string; const ADefaultValue: Integer = 0): Integer;
    function GetString(const APath: string; const ADefaultValue: string = ''): string;
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
  end;

  TEFNode = class(TEFTree)
  private
    FParent: TEFTree;
    FRawValue: string;
    FName: string;
    const NULL_VALUE = '$NULL$';
    function GetName: string;
    function GetRawValue: string;
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
    procedure SetRawValue(const AValue: string);
    function GetIndex: Integer;
    function GetIsNull: Boolean;
  protected
    procedure SetName(const AValue: string);
  public
    property Parent: TEFTree read FParent;
    property Index: Integer read GetIndex;
    function GetEnumerator: TEnumerator<TEFNode>;
    constructor Create(const AName: string = ''; const ARawValue: string = ''); reintroduce; virtual;
    constructor Clone(const ASource: TEFTree); override;

    procedure Clear; override;

    ///	<summary>
    ///	  Removes itself from its parent. This causes the parent to free the
    ///	  current object.
    ///	</summary>
    procedure Delete;

    property Name: string read GetName;
    property RawValue: string read GetRawValue write SetRawValue;
    property AsString: string read GetAsString write SetAsString;
    property AsStringArray: TStringDynArray read GetAsStringArray write SetAsStringArray;
    property AsPair: TEFPair read GetAsPair write SetAsPair;
    property AsPairs: TEFPairs read GetAsPairs write SetAsPairs;
    property AsExpandedString: string read GetAsExpandedString;
    property AsInteger: Integer read GetAsInteger write SetAsInteger;
    property AsObject: TObject read GetAsObject write SetAsObject;
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;

    property IsNull: Boolean read GetIsNull;
    procedure SetToNull;
    function EqualsNode(const ANode: TEFNode): Boolean;

    function GetChildStrings(const ASeparator: string = sLineBreak;
      const AConnector: string = '='; const ADefaultValue: string = ''): string; overload;
    function GetExpandedChildStrings(const ASeparator, AConnector,
      ADefaultValue: string): string;
    function GetChildPairs: TEFPairs;
  end;

  TEFTreeFactory = class
  public
    class function LoadFromFile<T: TEFTree, constructor>(const AFileName: string): T;
  end;

  ///	<summary>
  ///	  A macro expander that expands all the data items contained in a TEFNode
  ///	  object. Each macro in this format: %&lt;NameSpace&gt;:&lt;Path&gt;% is
  ///	  expanded to the value of the child node located by the path in the node
  ///	  this expander holds a reference to. &lt;NameSpace&gt; is a value set
  ///	  upon creation. If it is '', then no ':' separator is required in the
  ///	  macros.
  ///	</summary>
  ///	<remarks>
  ///	  <para>
  ///	    The need for a name space stems from the fact that you can have
  ///	    multiple macro expanders of this kind active at the same time, each
  ///	    linked to a different node object, and use the namespace string to
  ///	    differentiate them.
  ///	  </para>
  ///	  <para>
  ///	    Macros in values of nodes retrieved through macro expansion are not
  ///	    themselves expanded. This is a known current limitation.
  ///	  </para>
  ///	  <para>
  ///	    This macro expander is not registered by default, as it needs a
  ///	    reference to an external object to work. So, applications will create
  ///	    and use this class autonomously (by querying it directly or adding it
  ///	    to an expansion engine, without registering it) as required.
  ///	  </para>
  ///	</remarks>
  TEFNodeMacroExpander = class(TEFMacroExpander)
  private
    FNode: TEFNode;
    FPrefix: string;
  protected
    function InternalExpand(const AString: string): string; override;
  public
    constructor Create(const ANode: TEFNode;
      const ANameSpace: string); reintroduce;
  end;

implementation

uses
  SysUtils, StrUtils,
  EF.Environment, EF.Localization, EF.StrUtils, EF.YAML;

{ TEFNode }

procedure TEFNode.Clear;
begin
  inherited;
  FName := '';
  FRawValue := '';
end;

constructor TEFNode.Clone(const ASource: TEFTree);
begin
  Assert(ASource is TEFNode);

  inherited;
  FName := TEFNode(ASource).Name;
  FRawValue := TEFNode(ASource).RawValue;
end;

constructor TEFNode.Create(const AName, ARawValue: string);
begin
  inherited Create;
  FName := AName;
  FRawValue := ARawValue;
end;

procedure TEFNode.Delete;
begin
  if Assigned(Parent) then
    Parent.RemoveChild(Self);
end;

function TEFNode.GetAsBoolean: Boolean;
begin
  Result := RawValueToBoolean(FRawValue);
end;

function TEFNode.GetAsExpandedString: string;
begin
  Result := Environment.MacroExpansionEngine.Expand(ASString);
end;

function TEFNode.GetAsInteger: Integer;
begin
  Result := RawValueToInteger(FRawValue);
end;

function TEFNode.GetAsObject: TObject;
begin
  Result := RawValueToObject(FRawValue);
end;

function TEFNode.GetAsPair: TEFPair;
begin
  Result := TEFPair.Create(Name, AsString);
end;

function TEFNode.GetAsPairs: TEFPairs;
begin
  Result := RawValueToPairs(FRawValue);
end;

function TEFNode.GetAsString: string;
begin
  Result := RawValueToString(FRawValue);
end;

function TEFNode.GetAsStringArray: TStringDynArray;
begin
  Result := RawValueToStringArray(FRawValue);
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
  Result := SameText(FRawValue, NULL_VALUE);
end;

function TEFNode.GetName: string;
begin
  Result := FName;
end;

function TEFNode.GetRawValue: string;
begin
  Result := FRawValue;
end;

procedure TEFNode.SetAsBoolean(const AValue: Boolean);
begin
  FRawValue := BooleanToRawValue(AValue);
end;

procedure TEFNode.SetAsInteger(const AValue: Integer);
begin
  FRawValue := IntegerToRawValue(AValue);
end;

procedure TEFNode.SetAsObject(const AValue: TObject);
begin
  FRawValue := ObjectToRawValue(AValue);
end;

procedure TEFNode.SetAsPair(const AValue: TEFPair);
begin
  SetName(AValue.Key);
  AsString := AValue.Value;
end;

procedure TEFNode.SetAsPairs(const AValue: TEFPairs);
begin
  FRawValue := PairsToRawValue(AValue);
end;

procedure TEFNode.SetAsString(const AValue: string);
begin
  FRawValue := StringToRawValue(AValue);
end;

procedure TEFNode.SetAsStringArray(const AValue: TStringDynArray);
begin
  FRawValue := StringArrayToRawValue(AValue);
end;

procedure TEFNode.SetName(const AValue: string);
begin
  FName := AValue;
end;

procedure TEFNode.SetRawValue(const AValue: string);
begin
  FRawValue := AValue;
end;

procedure TEFNode.SetToNull;
begin
  FRawValue := NULL_VALUE;
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
      and ((ANode.IsNull and IsNull) or (ANode.RawValue = FRawValue)) then
    Result := True
  else
    Result := False;
end;

{ TEFTree }

function TEFTree.AddChild(const AName, ARawValue: string): TEFNode;
begin
  Result := AddChild(GetChildClass(AName).Create(AName, ARawValue));
end;

function TEFTree.AddChild(const ANode: TEFNode): TEFNode;
begin
  Assert(Assigned(ANode));

  ANode.FParent := Self;
  FNodes.Add(ANode);
  Result := ANode;
end;

procedure TEFTree.Assign(const ASource: TEFTree);
var
  LNode: TEFNode;
begin
  Assert(Assigned(ASource));

  Clear;
  for LNode in ASource.FNodes do
    AddChild(GetChildClass(LNode.Name).Clone(LNode));
end;

class function TEFTree.BooleanToRawValue(const ABoolean: Boolean): string;
begin
  Result := BoolToStr(ABoolean, True);
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
  Assert(Assigned(ASource));

  Create;
  Assign(ASource);
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
  inherited;
end;

function TEFTree.GetChild(I: Integer): TEFNode;
begin
  Result := FNodes[I];
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
  end;
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

class function TEFTree.IntegerToRawValue(const AInteger: Integer): string;
begin
  Result := IntToStr(AInteger);
end;

class function TEFTree.ObjectToRawValue(const AObject: TObject): string;
begin
  Result := IntToStr(Integer(Pointer(AObject)));
end;

class function TEFTree.PairsToRawValue(const APairs: TEFPairs): string;
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

class function TEFTree.RawValueToBoolean(const ARawValue: string): Boolean;
begin
  Result := StrToBool(ARawValue);
end;

class function TEFTree.RawValueToInteger(const ARawValue: string): Integer;
begin
  Result := StrToInt(ARawValue);
end;

class function TEFTree.RawValueToObject(const ARawValue: string): TObject;
begin
  Result := TObject(Pointer(StrToInt(ARawValue)));
end;

class function TEFTree.RawValueToPairs(const ARawValue: string): TEFPairs;
begin
  Result := SplitPairs(ARawValue, ' ');
end;

class function TEFTree.RawValueToString(const ARawValue: string): string;
begin
  Result := ARawValue;
end;

class function TEFTree.RawValueToStringArray(const ARawValue: string): TStringDynArray;
begin
  Result := Split(ARawValue, ' ');
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

class function TEFTree.StringArrayToRawValue(const AStringArray: TStringDynArray): string;
begin
  Result := Join(AStringArray, ' ');
end;

class function TEFTree.StringToRawValue(const AString: string): string;
begin
  Result := AString;
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

{ TEFNodeMacroExpander }

constructor TEFNodeMacroExpander.Create(const ANode: TEFNode;
  const ANameSpace: string);
begin
  inherited Create;
  FNode := ANode;
  FPrefix := ANameSpace;
  if Trim(FPrefix) <> '' then
    FPrefix := FPrefix + ':'
  else
    FPrefix := '';
end;

function TEFNodeMacroExpander.InternalExpand(const AString: string): string;
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
    LPathStart := LStart + 1 + Length(FPrefix) + 1;
    LEnd := PosEx('%', Result, LStart + 1);
    if LEnd = 0 then
      Exit;
    LNodePath := Copy(Result, LPathStart + 1, LEnd - LStart + 1);
    LNodeValue := FNode.GetExpandedString(StripPrefixAndSuffix(LNodePath, '%', '%'));
    if LNodeValue <> ''  then
    begin
      Delete(Result, LStart, Length(LNodePath));
      Insert(LNodeValue, Result, LStart);
    end;
    LIndex := LEnd + 1;
  until LIndex > Length(Result);
end;

end.
