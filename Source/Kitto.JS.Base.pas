unit Kitto.JS.Base;

interface

uses
  Rtti
  , Generics.Collections
  , EF.ObserverIntf
  , EF.Tree
  , Kitto.JS.Types
  , Kitto.JS.Formatting
  ;

type
  TJSBase = class(TEFSubjectAndObserver)
  private
    FOwner: TJSBase;
    FChildren: TObjectList<TJSBase>;
    FJSName: string;
    FDestroying: Boolean;
    FDestroyingChildren: Boolean;
    procedure SetOwner(const AValue: TJSBase);
  strict protected
    procedure AddChild(const AChild: TJSBase);
    procedure RemoveChild(const AChild: TJSBase); virtual;
    procedure InitDefaults; virtual;
  public
    destructor Destroy; override;
    procedure BeforeDestruction; override;
  public
    constructor Create(const AOwner: TJSBase); virtual;
    constructor CreateSingleton(const AOwner: TJSBase; const AAttributeName: string);
    property Owner: TJSBase read FOwner write SetOwner;

    property JSName: string read FJSName write FJSName;
    class function JSClassName: string; virtual;
    class function JSXType: string; virtual;

    function FindChildByJSName(const AJSName: string): TJSBase;
    procedure FreeAllChildren;

    /// <summary>
    ///  Returns a string representation of the tree of children for debugging purposes.
    /// </summary>
    function GetChildrenNameTree: string;
  end;
  TJSBaseClass = class of TJSBase;

  TJSObjectSpace = class(TJSBase)
  private
    FObjectSequences: TDictionary<string, Cardinal>;
    FSingletons: TDictionary<string, TJSBase>;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
  public
    function GetNextJSName(const AObjectType: string): string;
    function GetSingleton<T: TJSBase>(const AName: string): T;
  end;

  TJSExpression = class(TJSBase)
  private
    FText: string;
    FIsExtracted: Boolean;
    procedure SetText(const AValue: string);
  strict protected
    function InternalExtractText: string; virtual;
  public
    property Text: string read FText write SetText;

    function ExtractText: string;
    property IsExtracted: Boolean read FIsExtracted;
  end;

  TJSFunction = class(TJSExpression)
  strict protected
    function InternalExtractText: string; override;
  end;

  // Represents a config object or a set of method parameters.
  TJSValues = class(TJSBase)
  private
    FIsReadOnly: Boolean;
    FValues: TEFTree;
    FNameValueConnector: string;
    FParamConnector: string;
    FParamValuePrefix: string;
    FParamValueSuffix: string;
    function IsRaw(const AValue: TEFNode): Boolean;
    function IsObjectArray(const AValue: TEFNode): Boolean;
    function IsObject(const AValue: TEFNode): Boolean;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
  public
    property Values: TEFTree read FValues;
    // Can be ': ' or ' = '. Unused for empty or invalid names. Defaults to ': '.
    property NameValueConnector: string read FNameValueConnector write FNameValueConnector;
    // Can be ',' + sLineBreak, or '&'. Defaults to ',' + sLineBreak.
    property ParamConnector: string read FParamConnector write FParamConnector;

    property ParamValuePrefix: string read FParamValuePrefix write FParamValuePrefix;
    property ParamValueSuffix: string read FParamValueSuffix write FParamValueSuffix;

    procedure SetRawValue(const AName, AValue: string);

    // The config does not accept setting values anymore.
    // False during the request that creates the parent object, True after that.
    property IsReadOnly: Boolean read FIsReadOnly;
    /// <summary>
    ///  Raises an exception if the object is read-only. The exception message
    ///  includes the value name.
    /// </summary>
    procedure CheckReadOnly(const AValueName: string);

    procedure FormatTo(const AFormatter: TJSFormatter);
    function AsFormattedText: string;
  end;

function GetMethodName(const AMethod: TJSProcedure): string;

implementation

uses
  SysUtils
  , StrUtils
  , EF.StrUtils
  , Kitto.JS
  ;

function GetMethodName(const AMethod: TJSProcedure): string;
var
  LInfo: TRttiType;
  LMethod: TMethod;
  LRttiMethod: TRttiMethod;
  LObject: TObject;
begin
  Result := '';

  LMethod := TMethod(AMethod);
  LObject := LMethod.Data;

  LInfo := TRttiContext.Create.GetType(LObject.ClassType);
  for LRttiMethod in LInfo.GetMethods do
  begin
    if LRttiMethod.CodeAddress = LMethod.Code then
    begin
      Result := LRttiMethod.Name;
      Break;
    end;
  end;

  if Result = '' then
    raise Exception.Create('Method not found')
end;

{ TJSBase }

procedure TJSBase.AddChild(const AChild: TJSBase);
begin
  FChildren.Add(AChild);
end;

procedure TJSBase.BeforeDestruction;
begin
  inherited;
  FDestroying := True;
end;

constructor TJSBase.Create(const AOwner: TJSBase);
begin
  inherited Create;
  FChildren := TObjectList<TJSBase>.Create;
  FOwner := AOwner;
  FJSName := '';
  if Assigned(FOwner) then
    FOwner.AddChild(Self);
end;

constructor TJSBase.CreateSingleton(const AOwner: TJSBase;
  const AAttributeName: string);
begin
  Assert(Assigned(AOwner));
  Create(AOwner);
  if AAttributeName = '' then
    JSName := JSClassName
  else
    JSName := AAttributeName;
  InitDefaults;
end;

destructor TJSBase.Destroy;
begin
  if Assigned(FOwner) and not FOwner.FDestroying then
    FOwner.RemoveChild(Self);
  FOwner := nil;
  FDestroyingChildren := True;
  try
    FreeAndNil(FChildren);
  finally
    FDestroyingChildren := False;
  end;
  inherited;
end;

function TJSBase.FindChildByJSName(const AJSName: string): TJSBase;
var
  I: Integer;
begin
  Assert(AJSName <> '');

  Result := nil;
  for I := 0 to FChildren.Count - 1 do
  begin
    if FChildren[I].JSName = AJSName then
      Result := FChildren[I]
    else
      Result := FChildren[I].FindChildByJSName(AJSName);
    if Assigned(Result) then
      Break;
  end;
end;

procedure TJSBase.FreeAllChildren;
begin
  FChildren.Clear;
end;

function TJSBase.GetChildrenNameTree: string;
var
  I: Integer;
  LChildrenTree: string;
begin
  Result := '';
  for I := 0 to FChildren.Count - 1 do
  begin
    Result := Result + IfThen(FChildren[I].JSName <> '', FChildren[I].JSName, '?');
    LChildrenTree := FChildren[I].GetChildrenNameTree;
    if LChildrenTree <> '' then
      Result := Result + ' (' + LChildrenTree + ') ';
  end;
end;

procedure TJSBase.InitDefaults;
begin
end;

procedure TJSBase.RemoveChild(const AChild: TJSBase);
begin
  if not FDestroyingChildren then
    FChildren.Extract(AChild); // don't free it
end;

procedure TJSBase.SetOwner(const AValue: TJSBase);
begin
  if AValue <> FOwner then
  begin
    if Assigned(FOwner) then
      FOwner.RemoveChild(Self);
    FOwner := AValue;
    if Assigned(FOwner) then
      FOwner.AddChild(Self);
  end;
end;

class function TJSBase.JSClassName: string;
begin
  Result := 'Object';
end;

class function TJSBase.JSXType: string;
begin
  Result := '';
end;

{ TJSValues }

procedure TJSValues.AfterConstruction;
begin
  inherited;
  FValues := TEFTree.Create;
  FNameValueConnector := ': ';
  FParamConnector := ',' + sLineBreak;
  FIsReadOnly := False;
end;

function TJSValues.AsFormattedText: string;
var
  LFormatter: TJSFormatter;
begin
  LFormatter := TJSFormatter.Create;
  try
    FormatTo(LFormatter);
    Result := LFormatter.FormattedText;
  finally
    FreeAndNil(LFormatter);
  end;
end;

procedure TJSValues.CheckReadOnly(const AValueName: string);
begin
  if FIsReadOnly then
    raise Exception.CreateFmt('Cannot set config value %s. Object was created in a different request.', [AValueName]);
end;

destructor TJSValues.Destroy;
begin
  FreeAndNil(FValues);
  inherited;
end;

function TJSValues.IsRaw(const AValue: TEFNode): Boolean;
begin
  Result := Assigned(AValue) and AValue.GetBoolean('IsRaw');
end;

function TJSValues.IsObject(const AValue: TEFNode): Boolean;
begin
  Result := AValue.DataType is TEFObjectDataType;
end;

function TJSValues.IsObjectArray(const AValue: TEFNode): Boolean;
begin
  Result := IsObject(AValue) and (AValue.AsObject is TJSObjectArray);
end;

procedure TJSValues.SetRawValue(const AName, AValue: string);
var
  LNode: TEFNode;
begin
  CheckReadOnly(AName);
  LNode := FValues.GetNode(AName, True);

  LNode.SetBoolean('IsRaw', True);
  if AValue <> '' then
    LNode.AsString := AValue
  else
    LNode.SetToNull;
end;

procedure TJSValues.FormatTo(const AFormatter: TJSFormatter);
var
  I: Integer;
  LValue: TEFNode;
  LAdded: Boolean;

  function IsValidName(const AName: string): Boolean;
  begin
    // Nodes with number as names (such as method parameters) are considered unnamed.
    Result := (AName <> '') and not IsNumeric(AName);
  end;

  function FormatRaw(const ANode: TEFNode): Boolean;
  begin
    if IsValidName(ANode.Name) then
      AFormatter.AddIndentedPair(ANode.Name, FParamValuePrefix + ANode.AsString + FParamValueSuffix, False, False, FNameValueConnector)
    else
      AFormatter.AddIndented(ANode.AsString);
    Result := True;
  end;

  function FormatObjectConfig(const AName: string; const AObject: TJSObject): Boolean;
  begin
    Result := False;
    if Assigned(AObject) then
    begin
      Result := True;

      if AObject.IsInline then
      begin
        if IsValidName(AName) then
          AFormatter.AddIndented(AName + FNameValueConnector)
        else
          AFormatter.AddIndent;
        if AObject is TJSRaw then
          AFormatter.Add(AObject.JSName)
        else
        begin
          { TODO : what if it's empty? }
          AFormatter.OpenObject;
          AObject.JSConfig.FormatTo(AFormatter);
          AFormatter.CloseObject;
        end;
      end
      else if not AObject.IsInternal then
      begin
        if IsValidName(AName) then
          AFormatter.AddIndented(AName + FNameValueConnector)
        else
          AFormatter.AddIndent;
        AFormatter.AddIndentedLine(AObject.JSName);
      end
      else
        Result := False;
    end;
  end;

  function FormatObjectArrayConfig(const AName: string; const AObjectArray: TJSObjectArray): Boolean;
  var
    LObjectIndex: Integer;
  begin
    if AObjectArray.Count > 0 then
    begin
      if IsValidName(AName) then
        AFormatter.AddIndented(AName + FNameValueConnector)
      else
        AFormatter.AddIndent;
      AFormatter.OpenArray;
      for LObjectIndex := 0 to AObjectArray.Count - 1 do
      begin
        if FormatObjectConfig('', AObjectArray[LObjectIndex]) then
          AFormatter.Add(',');
      end;
      AFormatter.DeleteTrailing(',');
      AFormatter.CloseArray;
      Result := True;
    end
    else
      Result := False;
  end;

  function FormatSimpleValueConfig(const ANode: TEFNode): Boolean;
  var
    LString: string;
  begin
    Result := False;
    LString := ANode.DataType.NodeToJSONValue(False, ANode, AFormatter.FormatSettings, ANode.DataType.NeedsQuotes, True);
    if LString <> '' then
    begin
      if IsValidName(ANode.Name) then
        AFormatter.AddIndented(ANode.Name + FNameValueConnector + FParamValuePrefix + LString + FParamValueSuffix)
      else
        AFormatter.AddIndented(LString);
      Result := True;
    end;
  end;

begin
  for I := 0 to Values.ChildCount - 1 do
  begin
    LValue := Values.Children[I];
    if IsRaw(LValue) then
      LAdded := FormatRaw(LValue)
    else if IsObjectArray(LValue) then
      LAdded := FormatObjectArrayConfig(LValue.Name, LValue.AsObject as TJSObjectArray)
    else if IsObject(LValue) then
      LAdded := FormatObjectConfig(LValue.Name, LValue.AsObject as TJSObject)
    else
      LAdded := FormatSimpleValueConfig(LValue);
    if LAdded then
      AFormatter.Add(FParamConnector);
  end;
  AFormatter.DeleteTrailing(FParamConnector);
  AFormatter.DeleteTrailing(sLineBreak);
  FIsReadOnly := True;
end;

{ TJSObjectSpace }

procedure TJSObjectSpace.AfterConstruction;
begin
  inherited;
  FObjectSequences := TDictionary<string, Cardinal>.Create;
  FSingletons := TDictionary<string, TJSBase>.Create;
end;

destructor TJSObjectSpace.Destroy;
begin
  FreeAndNil(FSingletons);
  FreeAndNil(FObjectSequences);
  inherited;
end;

function TJSObjectSpace.GetNextJSName(const AObjectType: string): string;
var
  LResult: Cardinal;
begin
  if not FObjectSequences.ContainsKey(AObjectType) then
    FObjectSequences.Add(AObjectType, 0);
  LResult := FObjectSequences[AObjectType] + 1;
  FObjectSequences[AObjectType] := LResult;
  Result := AObjectType + IntToStr(LResult);
end;

function TJSObjectSpace.GetSingleton<T>(const AName: string): T;
begin
  if FSingletons.ContainsKey(AName) then
    Result := T(FSingletons[AName])
  else
  begin
    Result := TJSObjectClass(T).CreateSingleton(Self, AName) as T;
    FSingletons.Add(AName, Result);
  end;
end;

{ TJSExpression }

procedure TJSExpression.SetText(const AValue: string);
begin
  FText := AValue;
  FIsExtracted := False;
end;

function TJSExpression.ExtractText: string;
begin
  Assert(not FIsExtracted);
  Result := InternalExtractText;
  FIsExtracted := True;
end;

function TJSExpression.InternalExtractText: string;
begin
  Result := TJS.RemoveLastJSTerminator(Text);
end;

{ TJSFunction }

function TJSFunction.InternalExtractText: string;
begin
  Result := Text;
end;

end.
