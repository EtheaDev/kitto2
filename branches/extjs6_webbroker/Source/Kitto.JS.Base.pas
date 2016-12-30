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
    procedure RemoveChild(const AChild: TJSBase);
  public
    constructor Create(const AOwner: TJSBase); virtual;
    destructor Destroy; override;
    procedure BeforeDestruction; override;

    property Owner: TJSBase read FOwner write SetOwner;
    property JSName: string read FJSName write FJSName;
    class function JSClassName: string; virtual;
    class function JSXType: string; virtual;

    function FindChildByJSName(const AJSName: string): TJSBase;
    procedure FreeAllChildren;
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
  LMethod := TMethod(AMethod);
  LObject := LMethod.Data;

  LInfo := TRttiContext.Create.GetType(LObject.ClassType);
  for LRttiMethod in LInfo.GetMethods do
  begin
    Result := LRttiMethod.Name;
    if LRttiMethod.CodeAddress = LMethod.Code then
      Break;
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
        { TODO : what if it's empty? }
        AFormatter.OpenObject;
        AObject.JSConfig.FormatTo(AFormatter);
        AFormatter.CloseObject;
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

end.
