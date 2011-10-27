{
  The invocation registry is an object that allows to decouple callers and
  callees. It manages a list of registered "invocable items" (which might
  be global procedures, object methods, or other items - the set is open ended,
  though not really meant to be extended very often)
  and allows to call them. Each invocable item may have a set of named and
  typed parameters, both input and output.
  
  Invocable items are invoked by name, and each name may have more than one
  "invocation target" (this means that you can register several procedures
  under the same name, and they will be called in sequence when the name is
  invoked - think of this feature as multicast global events).

  An invocable item may be required (which means that an error is raised if it's
  not available at call time) or not (which means that no errors are raised).

  The invocation registry optionally works with the plugin manager to
  automatically load a plugin package when an invocable item hosted into that
  package is called. To do that, it uses an external index file that tells
  what plugins contain what invocable items.
  The file is called EFInvocationRegistry.idx and must reside in the executable's
  directory.
  
  By registering dummy invocable items you can also achieve on-demand plugin
  loading.

  This unit also defines a URI handler for the URI processor, which allows to
  call an invocable item through a custom formatted URI (this poses some
  limitations on the parameters that can be passed in the URI string).
}
unit EF.InvocationRegistry;

interface

uses
  SysUtils, Classes, Types,
  EF.PluginManager, EF.URIProcessor;

type
  {
    Defines the set of named and typed parameters that can be passed to
    and by an invocable item. The caller usually creates an instance of this
    class, passes it to the invocable item (through the invocation registry),
    reads back any return parameters and finally destroys it.
  }
  TEFInvocationParams = class
  private
    // Each parameter is stored encoded in an internal string representation.
    FParams: TStrings;
    function GetDateTimeParam(const AParamName: string): TDateTime;
    function GetIntegerParam(const AParamName: string): Integer;
    function GetDoubleParam(const AParamName: string): Double;
    function GetObjectParam(const AParamName: string): TObject;
    function GetStringParam(const AParamName: string): string;
    procedure SetDateTimeParam(const AParamName: string; const AValue: TDateTime);
    procedure SetIntegerParam(const AParamName: string; const AValue: Integer);
    procedure SetDoubleParam(const AParamName: string; const AValue: Double);
    procedure SetObjectParam(const AParamName: string; const AValue: TObject);
    procedure SetStringParam(const AParamName, AValue: string);
    function GetBooleanParam(const AParamName: string): Boolean;
    procedure SetBooleanParam(const AParamName: string; const AValue: Boolean);
    function GetMethodParam(const AParamName: string): TMethod;
    procedure SetMethodParam(const AParamName: string; const AValue: TMethod);
    function GetStringArrayParam(const AParamName: string): TStringDynArray;
    procedure SetStringArrayParam(const AParamName: string; const Value: TStringDynArray);

    function EncodeIntegerParam(const AInteger: Integer): string;
    function EncodeDoubleParam(const ADouble: Double): string;
    function EncodeStringParam(const AString: string): string;
    function EncodeDateTimeParam(const ADateTime: TDateTime): string;
    function EncodeObjectParam(const AObject: TObject): string;
    function EncodeBooleanParam(const ABoolean: Boolean): string;
    function EncodeMethodParam(const AMethod: TMethod): string;
    function EncodePointer(const APointer: Pointer): string;

    function DecodeIntegerParam(const AString: string): Integer;
    function DecodeDoubleParam(const AString: string): Double;
    function DecodeStringParam(const AString: string): string;
    function DecodeDateTimeParam(const AString: string): TDateTime;
    function DecodeObjectParam(const AString: string): TObject;
    function DecodeBooleanParam(const AString: string): Boolean;
    function DecodeMethodParam(const AString: string): TMethod;
    function DecodePointer(const AString: string): Pointer;
  public
    constructor Create;
    destructor Destroy; override;
    {
      Reads or writes parameters by name as integer values.
    }
    property IntegerParams[const AParamName: string]: Integer
      read GetIntegerParam write SetIntegerParam;
    {
      Reads or writes parameters by name as double precision values.
    }
    property DoubleParams[const AParamName: string]: Double
      read GetDoubleParam write SetDoubleParam;
    {
      Reads or writes parameters by name as string values.
    }
    property StringParams[const AParamName: string]: string
      read GetStringParam write SetStringParam; default;
    {
      Reads or writes parameters by name as date/time values.
    }
    property DateTimeParams[const AParamName: string]: TDateTime
      read GetDateTimeParam write SetDateTimeParam;
    {
      Reads or writes parameters by name as object references.
    }
    property ObjectParams[const AParamName: string]: TObject
      read GetObjectParam write SetObjectParam;
    {
      Reads or writes parameters by name as boolean values.
    }
    property BooleanParams[const AParamName: string]: Boolean
      read GetBooleanParam write SetBooleanParam;
    {
      Reads or writes parameters by name as method pointer values.
      Use this type to pass method pointers, such as event handlers.
    }
    property MethodParams[const AParamName: string]: TMethod
      read GetMethodParam write SetMethodParam;
    {
      Reads or writes parameters by name as string array values.
    }
    property StringArrayParams[const AParamName: string]: TStringDynArray
      read GetStringArrayParam write SetStringArrayParam;
    {
      Reads a parameter by name as a string list.
    }
    procedure GetStringsParam(const AParamName: string; const AParamValue: TStrings);
    {
      Writes a parameter by name as a string list.
    }
    procedure SetStringsParam(const AParamName: string; const AValue: TStrings);
    {
      Reads AURI's params setting corresponding parameters in the current object.
      Only string parameters currently supported.
    }
    procedure GetURIParams(const AURI: TEFURI);
    {
      This is a shortcut to EFInvocationRegistry.Invoke. It is useful when you
      don't have a reference to pass to the invocation ragistry.@br
      Example:@br
      @longCode(#
        with TEFInvocationParams.Create do
        try
          StringParams['SomeParam'] := 'SomeString';
          Invoke('SomeInvocableItem');
        finally
          Free;
        end;
      #)
    }
    procedure Invoke(const AInvocableName: string; const AStrict: Boolean = True);
    {
      Raises an exception a given parameter is not specified.
      Note: an empty string is equivalent to an unspecified parameter.
    }
    procedure CheckParam(const AParamName: string);
    {
      Raises an exception if one or more parameters are not specified.
      Note: an empty string is equivalent to an unspecified parameter.
    }
    procedure CheckParams(const AParamNames: array of string);
  end;

  {
    Base class for all invocation targets. An invocable item has a set of
    invocation targets which it forwards the call to.
  }
  TEFInvocationTarget = class
  public
    {
      Invokes the target.
    }
    procedure Invoke(const AParams: TEFInvocationParams); virtual; abstract;
  end;

  {
    Target type used by TEFInvocationProcedureTarget.
  }
  TEFInvocationProcedure = procedure(const AParams: TEFInvocationParams = nil);

  {
    An invocation target that calls a global procedure. The prototype must be
    compatible with TEFInvocationProcedure.
  }
  TEFInvocationProcedureTarget = class(TEFInvocationTarget)
  private
    FTargetProcedure: TEFInvocationProcedure;
  public
    {
      Creates an invocation target that calls the specified global procedure.
    }
    constructor Create(const ATargetProcedure: TEFInvocationProcedure);
    {
      The global procedure that will be called by this invocation target.
    }
    property TargetProcedure: TEFInvocationProcedure
      read FTargetProcedure write FTargetProcedure;
    {
      Invokes the global procedure specified in TargetProcedure.
    }
    procedure Invoke(const AParams: TEFInvocationParams); override;
  end;

  {
    Target type used by TEFInvocationMethodTarget.
  }
  TEFInvocationMethod = procedure(const AParams: TEFInvocationParams = nil) of object;

  {
    An invocation target that calls an object method. The prototype must be
    compatible with TEFInvocationMethod.
  }
  TEFInvocationMethodTarget = class(TEFInvocationTarget)
  private
    FTargetMethod: TEFInvocationMethod;
  public
    {
      Creates an invocation target that calls the specified object method.
    }
    constructor Create(const ATargetMethod: TEFInvocationMethod);
    {
      The object method that will be called by this invocation target.
    }
    property TargetMethod: TEFInvocationMethod read FTargetMethod write FTargetMethod;
    {
      Invokes the object method specified in TargetMethod.
    }
    procedure Invoke(const AParams: TEFInvocationParams); override;
  end;

  TEFInvocationItemList = class;

  {
    An item in a TEFInvocationItemList. Used internally by the
    invocation registry.
  }
  TEFInvocationItem = class
  private
    FName: string;
    FTargets: TStringList;
    function GetTarget(const AIndex: Integer): TEFInvocationTarget;
    function GetTargetCount: Integer;
    function FindMethodTarget(const AMethod: TEFInvocationMethod): Integer;
    function FindProcedureTarget(const AProcedure: TEFInvocationProcedure): Integer;
  public
    constructor Create(const AName: string);
    destructor Destroy; override;
    {
      Adds the current objects to AList. Returns a reference to the added object
      (which is Self) for easier call chaining.
    }
    function AddTo(const AList: TEFInvocationItemList): TEFInvocationItem;
    {
      Name that uniquely identifies the invocable item.
    }
    property Name: string read FName write FName;
    {
      Invokes all targets, in registration order, passing the specified
      parameters to each of them.
    }
    procedure InvokeTargets(const AParams: TEFInvocationParams);
    {
      Returns the number of items in Targets.
    }
    property TargetCount: Integer read GetTargetCount;
    {
      Returns a reference to a target by index.
    }
    property Targets[const AIndex: Integer]: TEFInvocationTarget read GetTarget;
    {
      Adds a target that calls a specified object method.
      Raises an exception if the target item is already in list.
      Pass APosition to force a particular call order, otherwise position 0 is
      implied. All items with the same position number are invoked in
      registration order.
    }
    procedure AddMethodTarget(const AMethod: TEFInvocationMethod;
      const APosition: Integer = 0);
    {
      Removes the target that calls the specified object method.
      Raises an exception if the target is not found.
    }
    procedure RemoveMethodTarget(const AMethod: TEFInvocationMethod);
    {
      Adds a target that calls a specified global procedure.
      Raises an exception if the target is already in list.
      Pass APosition to force a particular call order, otherwise position 0 is
      implied. All items with the same position number are invoked in
      registration order.
    }
    procedure AddProcedureTarget(const AProcedure: TEFInvocationProcedure;
      const APosition: Integer = 0);
    {
      Removes the target that calls the specified global procedure.
      Raises an exception if the target is not found.
    }
    procedure RemoveProcedureTarget(const AProcedure: TEFInvocationProcedure);
  end;

  {
    A list of invocable items. Used internally by the
    invocation registry.
  }
  TEFInvocationItemList = class
  private
    FItems: TStringList;
    function GetItem(const AName: string): TEFInvocationItem;
    function GetCount: Integer;
  protected
    function Add(const AItem: TEFInvocationItem): Integer;
    property Items[const AName: string]: TEFInvocationItem read GetItem;
    function GetItemByIndex(const AIndex: Integer): TEFInvocationItem;
  public
    constructor Create;
    destructor Destroy; override;
    {
      Removes and destroys the item with the given name.
      An exception is raised if the item isn't found.
    }
    procedure Delete(const AInvocableName: string);
    {
      Removes and destroys the given item. An exception is raised
      if the item isn't found.
    }
    procedure Remove(const AInvocationItem: TEFInvocationItem);
    {
      Returns the list index of the item with the given name. Returns -1
      if the item isn't found.
    }
    function GetItemIndex(const AInvocableName: string): Integer; overload;
    {
      Returns the list index of the given item. Returns -1
      if the item isn't found.
    }
    function GetItemIndex(const AItem: TEFInvocationItem): Integer; overload;
    {
      Returns the number of items in list.
    }
    property Count: Integer read GetCount;
    {
      Invokes an item by its index.
    }
    procedure InvokeItem(const AItemIndex: Integer; const AParams: TEFInvocationParams);
    {
      Returns a reference to the item with the given name. Creates a new item
      if it doesn't exist yet.
    }
    function GetInvocationItem(const AInvocableName: string): TEFInvocationItem;
    {
      Returns a reference to the item with the given name. Returns nil
      if it doesn't exist.
    }
    function FindInvocationItem(const AInvocableName: string): TEFInvocationItem;
  end;

  {
    Type of exceptions raised by the invocation registry.
  }
  EEFInvocationError = class(Exception);

  {
    Index of invocable items and plugins. It is loaded from an external file.
    This class is used internally by the invocation registry.
  }
  TEFInvocationRegistryIndex = class
  private
    FIndex: TStringList;
  public
    {
      Creates the index object and loads its contents from the specified file.
    }
    constructor Create(const AIndexFileName: string);
    destructor Destroy; override;
    {
      Adds to APluginNames the names of all plugins that correspond in the index
      to AInvocableName. Returns the number of strings added.
    }
    function GetPluginNames(const AInvocableName: string; const APluginNames: TStrings): Integer;
  end;

  {
    Invocation registry. This is the tip of the iceberg, or the class used
    by application code (together with TEFInvocationParams).@br

    It has an interface for invocable item providers, which allows to register
    and unregister invocable items, and an interface for invocable item callers,
    which allows to call them or check their existence.
  }
  TEFInvocationRegistry = class
  private
    FInvocationItems: TEFInvocationItemList;
    FIndex: TEFInvocationRegistryIndex;
    function GetIndex: TEFInvocationRegistryIndex;
    property Index: TEFInvocationRegistryIndex read GetIndex;
    function GetIndexFileName: string;
    procedure EnsurePluginLoaded(const AInvocableName: string);
  public
    constructor Create;
    destructor Destroy; override;
    {
      Registers an invocable item that is a global procedure.
      Pass APosition to force a particular call order, otherwise position 0 is
      implied. All items with the same position number are invoked in
      registration order.
      This method is called by invocable item providers (callees).
    }
    procedure RegisterProcedure(const AInvocableName: string;
      const AInvProcedure: TEFInvocationProcedure; const APosition: Integer = 0);
    {
      Unregister a previously registered global procedure. An exception is
      raised if the item isn't found.
      This method is called by invocable item providers (callees).
    }
    procedure UnregisterProcedure(const AInvocableName: string;
      const AInvProcedure: TEFInvocationProcedure);
    {
      Registers an invocable item that is an object method.
      Pass APosition to force a particular call order, otherwise position 0 is
      implied. All items with the same position number are invoked in
      registration order.
      This method is called by invocable item providers (callees).
    }
    procedure RegisterMethod(const AInvocableName: string;
      const AInvMethod: TEFInvocationMethod; const APosition: Integer = 0);
    {
      Unregister a previously registered object method. An exception is
      raised if the item isn't found.
    }
    procedure UnregisterMethod(const AInvocableName: string;
      const AInvMethod: TEFInvocationMethod);
    {
      Interface for consumers of invocable items (callers).
    }
    {
      Invokes a registered item. Pass True in AStrict to cause an exception
      when the item isn't found, and False if you want to silently skip
      calls to unregistered items.
      This method is called by invocable item users (callers).
    }
    procedure Invoke(const AInvocableName: string;
      const AParams: TEFInvocationParams = nil; const AStrict: Boolean = True);
    {
      Returns True if the item is registered, that is if it has one or more
      invocation targets.
      This method is called by invocable item users (callers).
    }
    function IsInvocable(const AInvocableName: string): Boolean;
  end;

{
  Singleton access to the invocation registry.
}
function InvocationRegistry: TEFInvocationRegistry;

{
  Utility routine. Raises an exception if AParams is not assigned.
}
procedure CheckInvocationParams(const AParams: TEFInvocationParams);

type
  {
    A URI handler that invokes an invocable item by name and params embedded
    in a URI string. URIs must be in the format:@br
    @bold(
    proc://EFInvocationRegistry/<InvocableName>?<StrParam1>=<StrValue1>&...
    )@br
    where <InvocableName> is the name of the invocable item to call,
    <StrParam1> is the name of the first string parameter to pass, and
    <StrValue1> is its value. Currently only string parameters are supported.
  }
  TEFInvocationRegistryURIHandler = class(TEFURIHandler)
  protected
    {
      If the URI is an invocation registry URI, calls the invocable item
      and sets Handled to True.
    }
    procedure InternalHandleURI(var AURI: string; var AHandled: Boolean); override;
  end;

implementation

uses
  EF.SysUtils;

resourcestring
  msgInvocableProcNotRegistered = 'Unknown procedure "%s".';
  msgInvocableProcAlreadyRegistered = 'Procedure "%s" already registered.';
  msgInvocationParamsNotAssigned = 'Unassigned invocation parameters.';
  msgInvocationParamNotSpecified = 'Unspecified invocation parameter "%s".';
  msgInvocationItemAlreadyInList = 'Item "%s" is already in list.';
  msgInvocationItemNotFound = 'Item "%s" is not in list.';
  msgInvocationParsingError = 'Error parsing string "%s".';

const
  PARAM_PART_SEPARATOR = '_';

var
  _EFInvocationRegistry: TEFInvocationRegistry;
  _EFInvocationParamsFormatSettings: TFormatSettings;

function InvocationRegistry: TEFInvocationRegistry;
begin
  if not Assigned(_EFInvocationRegistry) then
    _EFInvocationRegistry := TEFInvocationRegistry.Create;
  Result := _EFInvocationRegistry;
end;

procedure CheckInvocationParams(const AParams: TEFInvocationParams);
begin
  if not Assigned(AParams) then
    raise Exception.Create(msgInvocationParamsNotAssigned);
end;

{ TEFInvParams }

procedure TEFInvocationParams.CheckParam(const AParamName: string);
begin
  if FParams.Values[AParamName] = '' then
    raise Exception.CreateFmt(msgInvocationParamNotSpecified, [AParamName]);
end;

procedure TEFInvocationParams.CheckParams(const AParamNames: array of string);
var
  LParamNameIndex: Integer;
begin
  for LParamNameIndex := Low(AParamNames) to High(AParamNames) - 1 do
    CheckParam(AParamNames[LParamNameIndex]);
end;

constructor TEFInvocationParams.Create;
begin
  inherited Create;
  FParams := TStringList.Create;
end;

destructor TEFInvocationParams.Destroy;
begin
  FreeAndNil(FParams);
  inherited;
end;

function TEFInvocationParams.EncodePointer(const APointer: Pointer): string;
begin
  Result := '$' + IntToHex(Integer(APointer), 8);
end;

function TEFInvocationParams.DecodePointer(const AString: string): Pointer;
begin
  Result := Pointer(StrToIntDef(AString, 0));
end;

function TEFInvocationParams.DecodeBooleanParam(const AString: string): Boolean;
begin
  Result := StrToBoolDef(AString, False);
end;

function TEFInvocationParams.DecodeDateTimeParam(const AString: string): TDateTime;
begin
  Result := StrToDateTime(AString, _EFInvocationParamsFormatSettings);
end;

function TEFInvocationParams.DecodeIntegerParam(const AString: string): Integer;
begin
  Result := StrToIntDef(AString, 0);
end;

function TEFInvocationParams.DecodeDoubleParam(const AString: string): Double;
begin
  Result := StrToFloatDef(AString, 0);
end;

function TEFInvocationParams.DecodeMethodParam(const AString: string): TMethod;
var
  LSeparatorPosition: Integer;
begin
  if AString = '' then
  begin
    Result.Code := nil;
    Result.Data := nil;
  end
  else
  begin
    LSeparatorPosition := Pos(PARAM_PART_SEPARATOR, AString);
    if LSeparatorPosition = 0 then
      raise Exception.CreateFmt(msgInvocationParsingError, [AString]);
    Result.Code := DecodePointer(Copy(AString, 1, Pred(LSeparatorPosition)));
    Result.Data := DecodePointer(Copy(AString, LSeparatorPosition + Length(PARAM_PART_SEPARATOR), MaxInt));
  end;
end;

function TEFInvocationParams.DecodeObjectParam(const AString: string): TObject;
begin
  Result := DecodePointer(AString);
end;

function TEFInvocationParams.DecodeStringParam(const AString: string): string;
begin
  Result := AString;
end;

function TEFInvocationParams.EncodeBooleanParam(const ABoolean: Boolean): string;
begin
  Result := BoolToStr(ABoolean);
end;

function TEFInvocationParams.EncodeDateTimeParam(const ADateTime: TDateTime): string;
begin
  Result := DateTimeToStr(ADateTime, _EFInvocationParamsFormatSettings);
end;

function TEFInvocationParams.EncodeIntegerParam(const AInteger: Integer): string;
begin
  Result := IntToStr(AInteger);
end;

function TEFInvocationParams.EncodeDoubleParam(const ADouble: Double): string;
begin
  Result := FloatToStr(ADouble);
end;

function TEFInvocationParams.EncodeMethodParam(const AMethod: TMethod): string;
begin
  if (AMethod.Data = nil) or (AMethod.Code = nil) then
    Result := ''
  else
    Result := EncodePointer(AMethod.Code) + PARAM_PART_SEPARATOR
      + EncodePointer(AMethod.Data);
end;

function TEFInvocationParams.EncodeObjectParam(const AObject: TObject): string;
begin
  Result := EncodePointer(AObject);
end;

function TEFInvocationParams.EncodeStringParam(const AString: string): string;
begin
  Result := AString;
end;

function TEFInvocationParams.GetBooleanParam(const AParamName: string): Boolean;
begin
  Result := DecodeBooleanParam(FParams.Values[AParamName]);
end;

function TEFInvocationParams.GetDateTimeParam(const AParamName: string): TDateTime;
begin
  Result := DecodeDateTimeParam(FParams.Values[AParamName]);
end;

function TEFInvocationParams.GetIntegerParam(const AParamName: string): Integer;
begin
  Result := DecodeIntegerParam(FParams.Values[AParamName]);
end;

function TEFInvocationParams.GetDoubleParam(const AParamName: string): Double;
begin
  Result := DecodeDoubleParam(FParams.Values[AParamName]);
end;

function TEFInvocationParams.GetMethodParam(const AParamName: string): TMethod;
begin
  Result := DecodeMethodParam(FParams.Values[AParamName]);
end;

function TEFInvocationParams.GetObjectParam(const AParamName: string): TObject;
begin
  Result := DecodeObjectParam(FParams.Values[AParamName]);
end;

procedure TEFInvocationParams.GetStringsParam(const AParamName: string; const AParamValue: TStrings);
begin
  AParamValue.CommaText := DecodeStringParam(FParams.Values[AParamName]);
end;

function TEFInvocationParams.GetStringArrayParam(const AParamName: string): TStringDynArray;
var
  LTempStrings: TStrings;
  LTempStringIndex: Integer;
begin
  LTempStrings := TStringList.Create;
  try
    GetStringsParam(AParamName, LTempStrings);
    SetLength(Result, LTempStrings.Count);
    for LTempStringIndex := 0 to LTempStrings.Count - 1 do
      Result[LTempStringIndex] := LTempStrings[LTempStringIndex];
  finally
    LTempStrings.Free;
  end;
end;

function TEFInvocationParams.GetStringParam(const AParamName: string): string;
begin
  Result := DecodeStringParam(FParams.Values[AParamName]);
end;

procedure TEFInvocationParams.GetURIParams(const AURI: TEFURI);
var
  LParamIndex: Integer;
begin
  for LParamIndex := 0 to AURI.ParamCount - 1 do
    StringParams[AURI.ParamNames[LParamIndex]] := AURI.ParamValues[LParamIndex];
end;

procedure TEFInvocationParams.Invoke(const AInvocableName: string;
  const AStrict: Boolean = True);
begin
  InvocationRegistry.Invoke(AInvocableName, Self, AStrict);
end;

procedure TEFInvocationParams.SetBooleanParam(const AParamName: string;
  const AValue: Boolean);
begin
  FParams.Values[AParamName] := EncodeBooleanParam(AValue);
end;

procedure TEFInvocationParams.SetDateTimeParam(const AParamName: string;
  const AValue: TDateTime);
begin
  FParams.Values[AParamName] := EncodeDateTimeParam(AValue);
end;

procedure TEFInvocationParams.SetIntegerParam(const AParamName: string;
  const AValue: Integer);
begin
  FParams.Values[AParamName] := EncodeIntegerParam(AValue);
end;

procedure TEFInvocationParams.SetDoubleParam(const AParamName: string;
  const AValue: Double);
begin
  FParams.Values[AParamName] := EncodeDoubleParam(AValue);
end;

procedure TEFInvocationParams.SetMethodParam(const AParamName: string;
  const AValue: TMethod);
begin
  FParams.Values[AParamName] := EncodeMethodParam(AValue);
end;

procedure TEFInvocationParams.SetObjectParam(const AParamName: string;
  const AValue: TObject);
begin
  FParams.Values[AParamName] := EncodeObjectParam(AValue);
end;

procedure TEFInvocationParams.SetStringArrayParam(const AParamName: string;
  const Value: TStringDynArray);
var
  TempStrings: TStrings;
  g: Integer;
begin
  TempStrings := TStringList.Create;
  try
    for g := Low(Value) to High(Value) do
      TempStrings.Add(Value[g]);
    SetStringsParam(AParamName, TempStrings);
  finally
    TempStrings.Free;
  end;
end;

procedure TEFInvocationParams.SetStringParam(const AParamName, AValue: string);
begin
  FParams.Values[AParamName] := EncodeStringParam(AValue);
end;

procedure TEFInvocationParams.SetStringsParam(const AParamName: string;
  const AValue: TStrings);
begin
  FParams.Values[AParamName] := EncodeStringParam(AValue.CommaText);
end;

{ TEFInvocationRegistry }

constructor TEFInvocationRegistry.Create;
begin
  inherited Create;
  FInvocationItems := TEFInvocationItemList.Create;
end;

destructor TEFInvocationRegistry.Destroy;
begin
  FreeAndNil(FInvocationItems);
  FreeAndNil(FIndex);
  inherited;
end;

procedure TEFInvocationRegistry.Invoke(const AInvocableName: string;
  const AParams: TEFInvocationParams = nil; const AStrict: Boolean = True);
var
  LItemIndex: Integer;
begin
  EnsurePluginLoaded(AInvocableName);
  LItemIndex := FInvocationItems.GetItemIndex(AInvocableName);
  if LItemIndex >= 0 then
    FInvocationItems.InvokeItem(LItemIndex, AParams)
  else if AStrict then
    raise EEFInvocationError.CreateFmt(msgInvocableProcNotRegistered, [AInvocableName]);
end;

function TEFInvocationRegistry.IsInvocable(const AInvocableName: string): Boolean;
begin
  EnsurePluginLoaded(AInvocableName);
  Result := FInvocationItems.GetItemIndex(AInvocableName) >= 0;
end;

procedure TEFInvocationRegistry.RegisterMethod(const AInvocableName: string;
  const AInvMethod: TEFInvocationMethod; const APosition: Integer = 0);
begin
  Assert(Assigned(AInvMethod));
  with FInvocationItems.GetInvocationItem(AInvocableName) do
    AddMethodTarget(AInvMethod, APosition);
end;

procedure TEFInvocationRegistry.UnregisterMethod(const AInvocableName: string;
  const AInvMethod: TEFInvocationMethod);
var
  LInvocationItem: TEFInvocationItem;
begin
  LInvocationItem := FInvocationItems.FindInvocationItem(AInvocableName);
  if not Assigned(LInvocationItem) then
    raise Exception.CreateFmt(msgInvocableProcNotRegistered, [AInvocableName]);
  LInvocationItem.RemoveMethodTarget(AInvMethod);
  if LInvocationItem.TargetCount = 0 then
    FInvocationItems.Remove(LInvocationItem);
end;

procedure TEFInvocationRegistry.RegisterProcedure(const AInvocableName: string;
  const AInvProcedure: TEFInvocationProcedure; const APosition: Integer = 0);
begin
  Assert(Assigned(AInvProcedure));
  with FInvocationItems.GetInvocationItem(AInvocableName) do
    AddProcedureTarget(AInvProcedure, APosition);
end;

procedure TEFInvocationRegistry.UnregisterProcedure(const AInvocableName: string;
  const AInvProcedure: TEFInvocationProcedure);
var
  LInvocationItem: TEFInvocationItem;
begin
  LInvocationItem := FInvocationItems.FindInvocationItem(AInvocableName);
  if not Assigned(LInvocationItem) then
    raise Exception.CreateFmt(msgInvocableProcNotRegistered, [AInvocableName]);
  LInvocationItem.RemoveProcedureTarget(AInvProcedure);
  if LInvocationItem.TargetCount = 0 then
    FInvocationItems.Remove(LInvocationItem);
end;

procedure TEFInvocationRegistry.EnsurePluginLoaded(const AInvocableName: string);
var
  PluginNames: TStrings;
  LPluginNameIndex: Integer;
begin
  PluginNames := TStringList.Create;
  try
    if Index.GetPluginNames(AInvocableName, PluginNames) > 0 then
      for LPluginNameIndex := 0 to PluginNames.Count - 1 do
        PluginManager.EnsurePluginLoaded(PluginNames[LPluginNameIndex]);
  finally
    PluginNames.Free;
  end;
end;

function TEFInvocationRegistry.GetIndex: TEFInvocationRegistryIndex;
begin
  if not Assigned(FIndex) then
    FIndex := TEFInvocationRegistryIndex.Create(GetIndexFileName());
  Result := FIndex;
end;

function TEFInvocationRegistry.GetIndexFileName: string;
begin
  Result := ExtractFilePath(ParamStr(0)) + 'EFInvocationRegistry.idx';
end;

{ TEFInvItem }

function TEFInvocationItem.FindMethodTarget(const AMethod: TEFInvocationMethod): Integer;
var
  LTargetIndex: Integer;
begin
  Result := -1;
  for LTargetIndex := 0 to FTargets.Count - 1 do
  begin
    if FTargets.Objects[LTargetIndex] is TEFInvocationMethodTarget then
    begin
      if @TEFInvocationMethodTarget(FTargets.Objects[LTargetIndex]).TargetMethod = @AMethod then
      begin
        Result := LTargetIndex;
        Break;
      end;
    end;
  end;
end;

procedure TEFInvocationItem.AddMethodTarget(const AMethod: TEFInvocationMethod;
  const APosition: Integer = 0);
begin
  if FindMethodTarget(AMethod) < 0 then
    FTargets.AddObject(Format('%10d', [APosition]), TEFInvocationMethodTarget.Create(AMethod))
  else
    raise Exception.CreateFmt(msgInvocableProcAlreadyRegistered, [FName]);
end;

procedure TEFInvocationItem.RemoveMethodTarget(const AMethod: TEFInvocationMethod);
var
  LTargetIndex: Integer;
begin
  LTargetIndex := FindMethodTarget(AMethod);
  if LTargetIndex >= 0 then
  begin
    FTargets.Objects[LTargetIndex].Free;
    FTargets.Delete(LTargetIndex);
  end
  else
    raise Exception.CreateFmt(msgInvocableProcNotRegistered, [FName]);
end;

function TEFInvocationItem.FindProcedureTarget(const AProcedure: TEFInvocationProcedure): Integer;
var
  LTargetIndex: Integer;
begin
  Result := -1;
  for LTargetIndex := 0 to FTargets.Count - 1 do
  begin
    if FTargets.Objects[LTargetIndex] is TEFInvocationProcedureTarget then
    begin
      if @TEFInvocationProcedureTarget(FTargets.Objects[LTargetIndex]).TargetProcedure = @AProcedure then
      begin
        Result := LTargetIndex;
        Break;
      end;
    end;
  end;
end;

procedure TEFInvocationItem.AddProcedureTarget(const AProcedure: TEFInvocationProcedure;
  const APosition: Integer = 0);
begin
  if FindProcedureTarget(AProcedure) < 0 then
    FTargets.AddObject(Format('%10d', [APosition]), TEFInvocationProcedureTarget.Create(AProcedure))
  else
    raise Exception.CreateFmt(msgInvocableProcAlreadyRegistered, [FName]);
end;

procedure TEFInvocationItem.RemoveProcedureTarget(const AProcedure: TEFInvocationProcedure);
var
  LTargetIndex: Integer;
begin
  LTargetIndex := FindProcedureTarget(AProcedure);
  if LTargetIndex >= 0 then
  begin
    FTargets.Objects[LTargetIndex].Free;
    FTargets.Delete(LTargetIndex);
  end
  else
    raise Exception.CreateFmt(msgInvocableProcNotRegistered, [FName]);
end;

function TEFInvocationItem.AddTo(const AList: TEFInvocationItemList): TEFInvocationItem;
begin
  AList.Add(Self);
  Result := Self;
end;

constructor TEFInvocationItem.Create(const AName: string);
begin
  inherited Create;
  FName := AName;
  FTargets := TStringList.Create;
  FTargets.Sorted := True;
  FTargets.Duplicates := dupAccept;
end;

destructor TEFInvocationItem.Destroy;
var
  LTargetIndex: Integer;
begin
  for LTargetIndex := FTargets.Count - 1 downto 0 do
    FTargets.Objects[LTargetIndex].Free;
  FreeAndNil(FTargets);
  inherited;
end;

function TEFInvocationItem.GetTarget(const AIndex: Integer): TEFInvocationTarget;
begin
  Result := TEFInvocationTarget(FTargets.Objects[AIndex]);
end;

function TEFInvocationItem.GetTargetCount: Integer;
begin
  Result := FTargets.Count;
end;

procedure TEFInvocationItem.InvokeTargets(const AParams: TEFInvocationParams);
var
  LTargetIndex: Integer;
begin
  for LTargetIndex := 0 to FTargets.Count - 1 do
    Targets[LTargetIndex].Invoke(AParams);
end;

{ TEFInvocationItemList }

function TEFInvocationItemList.Add(const AItem: TEFInvocationItem): Integer;
var
  LItemIndex: Integer;
begin
  if FItems.Find(AItem.Name, LItemIndex) then
    raise Exception.CreateFmt(msgInvocationItemAlreadyInList, [AItem.Name])
  else
    Result := FItems.AddObject(AItem.Name, AItem);
end;

constructor TEFInvocationItemList.Create;
begin
  inherited Create;
  FItems := TStringList.Create;
  FItems.Sorted := True;
end;

procedure TEFInvocationItemList.Delete(const AInvocableName: string);
var
  LItemIndex: Integer;
begin
  if FItems.Find(AInvocableName, LItemIndex) then begin
    FItems.Objects[LItemIndex].Free;
    FItems.Delete(LItemIndex);
  end
  else
    raise Exception.CreateFmt(msgInvocationItemNotFound, [AInvocableName]);
end;

destructor TEFInvocationItemList.Destroy;
var
  LItemIndex: Integer;
begin
  for LItemIndex := FItems.Count - 1 downto 0 do
    FItems.Objects[LItemIndex].Free;
  FreeAndNil(FItems);
  inherited;
end;

function TEFInvocationItemList.GetItemIndex(const AInvocableName: string): Integer;
begin
  Result := FItems.IndexOf(AInvocableName);
end;

function TEFInvocationItemList.GetItemIndex(const AItem: TEFInvocationItem): Integer;
var
  LItemIndex: Integer;
begin
  Result := -1;
  for LItemIndex := 0 to FItems.Count - 1 do begin
    if FItems.Objects[LItemIndex] = AItem then begin
      Result := LItemIndex;
      Break;
    end;
  end;
end;

function TEFInvocationItemList.GetItem(const AName: string): TEFInvocationItem;
var
  LItemIndex: Integer;
begin
  if FItems.Find(AName, LItemIndex) then
    Result := FItems.Objects[LItemIndex] as TEFInvocationItem
  else
    raise Exception.CreateFmt(msgInvocationItemNotFound, [AName]);
end;

function TEFInvocationItemList.GetItemByIndex(const AIndex: Integer): TEFInvocationItem;
begin
  Result := FItems.Objects[AIndex] as TEFInvocationItem;
end;

function TEFInvocationItemList.GetCount: Integer;
begin
  Result := FItems.Count;
end;

procedure TEFInvocationItemList.InvokeItem(const AItemIndex: Integer; const AParams: TEFInvocationParams);
begin
  GetItemByIndex(AItemIndex).InvokeTargets(AParams);
end;

function TEFInvocationItemList.GetInvocationItem(const AInvocableName: string): TEFInvocationItem;
begin
  Result := FindInvocationItem(AInvocableName);
  if not Assigned(Result) then
    Result := TEFInvocationItem.Create(AInvocableName).AddTo(Self);
end;

function TEFInvocationItemList.FindInvocationItem(const AInvocableName: string): TEFInvocationItem;
var
  LItemIndex: Integer;
begin
  LItemIndex := GetItemIndex(AInvocableName);
  if LItemIndex >= 0 then
    Result := GetItemByIndex(LItemIndex)
  else
    Result := nil;
end;

procedure TEFInvocationItemList.Remove(const AInvocationItem: TEFInvocationItem);
var
  LItemIndex: Integer;
begin
  LItemIndex := GetItemIndex(AInvocationItem);
  if LItemIndex >= 0 then
  begin
    AInvocationItem.Free;
    FItems.Delete(LItemIndex);
  end
  else
    raise Exception.CreateFmt(msgInvocableProcAlreadyRegistered, [AInvocationItem.Name]);
end;

{ TEFInvProcedureTarget }

constructor TEFInvocationProcedureTarget.Create(const ATargetProcedure: TEFInvocationProcedure);
begin
  inherited Create;
  FTargetProcedure := ATargetProcedure;
end;

procedure TEFInvocationProcedureTarget.Invoke(const AParams: TEFInvocationParams);
begin
  FTargetProcedure(AParams);
end;

{ TEFInvMethodTarget }

constructor TEFInvocationMethodTarget.Create(const ATargetMethod: TEFInvocationMethod);
begin
  inherited Create;
  FTargetMethod := ATargetMethod;
end;

procedure TEFInvocationMethodTarget.Invoke(const AParams: TEFInvocationParams);
begin
  FTargetMethod(AParams);
end;

{ TEFInvocationRegistryIndex }

constructor TEFInvocationRegistryIndex.Create(const AIndexFileName: string);
begin
  inherited Create;
  FIndex := TStringList.Create;
  FIndex.Sorted := True;
  FIndex.Duplicates := dupError;
  if FileExists(AIndexFileName) then
    FIndex.LoadFromFile(AIndexFileName);
end;

destructor TEFInvocationRegistryIndex.Destroy;
begin
  FreeAndNil(FIndex);
  inherited;
end;

function TEFInvocationRegistryIndex.GetPluginNames(const AInvocableName: string;
  const APluginNames: TStrings): Integer;
var
  LPluginNames: TStrings;
begin
  LPluginNames := TStringList.Create;
  try
    LPluginNames.CommaText := FIndex.Values[AInvocableName];
    Result := LPluginNames.Count;
    APluginNames.AddStrings(LPluginNames);
  finally
    LPluginNames.Free;
  end;
end;

{ TEFInvocationRegistryURIHandler }

procedure TEFInvocationRegistryURIHandler.InternalHandleURI(var AURI: string;
  var AHandled: Boolean);
var
  LURIObj: TEFURI;
begin
  LURIObj := TEFURI.Create(AURI);
  try
    if (LURIObj.Protocol = 'proc')
      and (LURIObj.Host = 'EFInvocationRegistry')
      and (LURIObj.Document <> '') then
    begin
      // Extract the params and invoke the item.
      with TEFInvocationParams.Create do
      begin
        try
          GetURIParams(LURIObj);
          Invoke(LURIObj.Document);
        finally
          Free;
        end;
      end;
      AHandled := True;
    end;
  finally
    LURIObj.Free;
  end;
end;

initialization
  _EFInvocationParamsFormatSettings := GetFormatSettings;
  _EFInvocationParamsFormatSettings.DateSeparator := '/';
  _EFInvocationParamsFormatSettings.TimeSeparator := ':';
  _EFInvocationParamsFormatSettings.ShortDateFormat := 'yyyy"/"mm"/"dd';
  _EFInvocationParamsFormatSettings.ShortTimeFormat := 'hh":"mm":"ss"';

  URIHandlerRegistry.RegisterHandler(TEFInvocationRegistryURIHandler, 1000);

finalization
  URIHandlerRegistry.UnregisterHandler(TEFInvocationRegistryURIHandler);

  FreeAndNil(_EFInvocationRegistry);

end.
