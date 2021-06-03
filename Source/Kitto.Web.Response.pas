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

unit Kitto.Web.Response;

{$I Kitto.Defines.inc}

interface

uses
  Classes
  , Generics.Collections
  , SysUtils
  , HTTPApp
  , Kitto.JS.Types
  , Kitto.JS.Base
  , Kitto.JS.Formatting
  ;

type
  TJSResponseItems = class;

  TKWebResponse = class
  private
    FResponse: TWebResponse;
    FItems: TStack<TJSResponseItems>;
    FOwnsResponse: Boolean;
    class threadvar FCurrent: TKWebResponse;
    class function GetCurrent: TKWebResponse; static;
    class procedure SetCurrent(const AValue: TKWebResponse); static;
    function GetItems: TJSResponseItems;
    function GetContentType: string;
    procedure SetContentType(const Value: string);
    function GetCustomHeaders: TStrings;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
  public
    class property Current: TKWebResponse read GetCurrent write SetCurrent;
    class procedure ClearCurrent;

    constructor Create(const AResponse: TWebResponse; const AOwnsResponse: Boolean = True);

    property ContentType: string read GetContentType write SetContentType;
    procedure SetCustomHeader(const AName, AValue: string);
    property CustomHeaders: TStrings read GetCustomHeaders;
    procedure SetCookie(const AName, AValue: string; const AExpires: TDateTime);

    function HasItems: Boolean;
    function BranchItems: TJSResponseItems;
    procedure UnbranchItems(const AItems: TJSResponseItems; const AConsolidate: Boolean);
    property Items: TJSResponseItems read GetItems;
    /// <summary>
    ///  Generates code for all the Items and sets Content/ContentStream and ContentType accordingly.
    ///  Then finalizes the response. Called as a final act after all request handlers
    ///  have had a chance at contributing to it.
    /// </summary>
    procedure Send;

    /// <summary>
    ///  Takes care of freeing a previous content stream, if any, before assigning a new one.
    /// </summary>
    procedure ReplaceContentStream(const AStream: TStream);

    function GetJSCode(const AMethod: TProc; const ASilent: Boolean = False): string;
  end;

  TJSResponseItem = class;
  TJSCreateObject = class;
  TJSGetProperty = class;
  TJSSetProperty = class;
  TJSCode = class;
  TJSMethodCall = class;
  TJSAjaxCall = class;

  TJSResponseItems = class(TJSBase)
  private
    FList: TObjectList<TJSResponseItem>;
    FEmittedItems: TList<TJSResponseItem>;
    FCharset: string;
    procedure SortByDependency;
    function GetCount: Integer;
    function GetItem(I: Integer): TJSResponseItem;
    procedure DoSetProperty(const AObject: TJSBase; const ASetValueProc: TProc<TJSSetProperty>);
    function GetEncoding: TEncoding;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
  public
    // Create an object.
    procedure CreateObject(const AObject: TJSBase);

    // Use this method to have object creation statements in the correct order when emitting the response.
    procedure AddObjectDependency(const ADependentObject, ADependedUponObject: TJSBase);

    function CallMethod(const AObject: TJSBase; const AMethodName: string): TJSMethodCall;

    function AjaxCallMethod(const AObject: TJSBase; const AMethodName: string = ''): TJSAjaxCall;

    function GetProperty(const AObject: TJSBase; const APropertyName: string): TJSGetProperty;

{ TODO : replace with single fluent call }
    procedure SetProperty(const AObject: TJSBase; const AName, AValue: string); overload;
    procedure SetProperty(const AObject: TJSBase; const AName: string; const AValue: TJSBase); overload;
    procedure SetProperty(const AObject: TJSBase; const AName: string; const AValue: Boolean); overload;
    procedure SetProperty(const AObject: TJSBase; const AName: string; const AValue: Integer); overload;
    procedure SetProperty(const AObject: TJSBase; const AName: string; const AValue: TDateTime); overload;
    procedure SetProperty(const AObject: TJSBase; const AName: string; const AValue: TJSExpression); overload;

{ TODO : replace with single fluent call }
    function ExecuteJSCode(const AJSCode: string): TJSCode; overload;
    function ExecuteJSCode(const AObject: TJSBase; const AJSCode: string): TJSCode; overload;

    procedure AddJSON(const AJSON: string);

    procedure AddHTML(const AHTML: string);

    function AsFormattedString: string;
    function Consume: string;

    function FindObjectCreateItem(const AObject: TJSBase): TJSCreateObject;

    procedure ForEach(const AProc: TProc<TJSResponseItem>);

    property Items[I: Integer]: TJSResponseItem read GetItem; default;
    property Count: Integer read GetCount;
    procedure Clear;

    function GetContentType: string;

    property Charset: string read FCharset write FCharset;
    property Encoding: TEncoding read GetEncoding;
  end;

  TJSResponseItem = class(TJSBase)
  private
    FSender: TJSBase;
    FDependencies: TList<TJSCreateObject>;
    FEmitted: Boolean;
    FCreationDateTime: TDateTime;
    FCachedText: string;
    function GetDependencyCount: Integer;
    function GetDependency(I: Integer): TJSCreateObject;
    function AllDependenciesEmitted(const AEmittedItems: TList<TJSResponseItem>): Boolean;
    function GetFormattedCode: string;
  strict protected
    FRoot: TJSResponseItems;
    procedure ChangeSender(const ASender: TJSBase);
    procedure InternalFormatTo(const AFormatter: TJSFormatter); virtual;
  public
    constructor Create(const ASender: TJSBase; const ARoot: TJSResponseItems); reintroduce; virtual;
    procedure AfterConstruction; override;
    destructor Destroy; override;
    property Sender: TJSBase read FSender;
    procedure AddDependency(const AItem: TJSCreateObject);
    procedure RemoveDependency(const AItem: TJSCreateObject);
    function GetDependencies: TArray<TJSCreateObject>;
    property DependencyCount: Integer read GetDependencyCount;
    property Dependencies[I: Integer]: TJSCreateObject read GetDependency;
    function DependsOn(const AItem: TJSCreateObject): Boolean;
    function IsCode: Boolean; virtual;

    procedure Emit(const AEmittedItems: TList<TJSResponseItem>);
    procedure UnEmit;

    procedure FormatTo(const AFormatter: TJSFormatter);
    function AsFormattedText: string;

    // Allows the text to be emitted even if the sender is being
    // destroyed. Works by caching the text and clearing the reference to the
    // Sender, effectively inhibiting any further changes to the sender to be
    // reflected in the response. Call this method only when the sender is
    // being destroyed or anyway no longer used.
    procedure UnlinkFromSender;

    function GetDebugDescription: string; virtual;

    function GetContentType: string; virtual;
  end;

  TJSNamedCreateObject = record
    Name: string;
    CreateObject: TJSCreateObject;
  end;

  TJSCreateObject = class(TJSResponseItem)
  private
    FItems: TList<TJSNamedCreateObject>;
  strict protected
    procedure InternalFormatTo(const AFormatter: TJSFormatter); override;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;

    function IsCode: Boolean; override;

    procedure CreateInternalObject(const AAttributeName: string; const ASender: TJSBase);

    function FindObjectCreateItem(const ASender: TJSBase): TJSCreateObject;
  end;

  // Base class for response items that can generate a TJSExpression.
  TJSExpressionResponseItem = class(TJSResponseItem)
  private
    FExpression: TJSExpression;
    FOwnsExpression: Boolean;
    // Used by GetAsFunction.
    FFunctionArgs: string;
    FFunctionReturn: string;
    function GetAsExpression: TJSExpression;
    function GetAsFunction: TJSFunction;
  strict protected
    function IsExpressionExtracted: Boolean;
  public
    destructor Destroy; override;
  public
    property AsExpression: TJSExpression read GetAsExpression;
    property AsFunction: TJSFunction read GetAsFunction;

    function HasExpression: Boolean;

    function FunctionArgs(const AFunctionArgs: string): TJSExpressionResponseItem;
    function FunctionReturn(const AFunctionReturn: string): TJSExpressionResponseItem;
  end;

  TJSMethodCall = class(TJSExpressionResponseItem)
  private
    FCallName: string;
    FParams: TJSValues;
  strict protected
    procedure InternalFormatTo(const AFormatter: TJSFormatter); override;
  public
    procedure AfterConstruction; override;
  public
    property CallName: string read FCallName write FCallName;

    function AddParam(const AValue: string): TJSMethodCall; overload;
    function AddParam(const AValue: Boolean): TJSMethodCall; overload;
    function AddParam(const AValue: Integer): TJSMethodCall; overload;
    function AddParam(const AValue: TJSBase): TJSMethodCall; overload;
    function AddParam(const AValue: TDateTime): TJSMethodCall; overload;
    function AddParam(const AValue: TJSExpression): TJSMethodCall; overload;

    property Params: TJSValues read FParams;

    function GetDebugDescription: string; override;
  end;

  TJSAjaxCall = class(TJSMethodCall)
  private
    FHttpMethod: string;
    FPostData: string;
    procedure AddParams(const AFormatter: TJSFormatter);
  strict protected
    procedure InternalFormatTo(const AFormatter: TJSFormatter); override;
  public
    procedure AfterConstruction; override;
  public

    function SetMethod(const AMethod: TJSProcedure): TJSAjaxCall;
    function Get: TJSAjaxCall;
    function Post(const AData: string): TJSAjaxCall;
    function Event: TJSAjaxCall;

    function AddRawParam(const AName, AValue: string): TJSAjaxCall; overload;
    function AddParam(const AName, AValue: string): TJSAjaxCall; overload;
    function AddParam(const AName: string; const AValue: Boolean): TJSAjaxCall; overload;
    function AddParam(const AName: string; const AValue: Integer): TJSAjaxCall; overload;
    function AddParam(const AName: string; const AValue: TJSBase): TJSAjaxCall; overload;
    function AddParam(const AName: string; const AValue: TDateTime): TJSAjaxCall; overload;
    function AddParam(const AName: string; const AValue: TJSExpression): TJSAjaxCall; overload;
  end;

  TJSGetProperty = class(TJSExpressionResponseItem)
  private
    FPropertyName: string;
  strict protected
    procedure InternalFormatTo(const AFormatter: TJSFormatter); override;
  public
    property PropertyName: string read FPropertyName write FPropertyName;
  end;

  TJSSetProperty = class(TJSResponseItem)
  private
    FNameValue: TJSValues;
  strict protected
    procedure InternalFormatTo(const AFormatter: TJSFormatter); override;
  public
    procedure AfterConstruction; override;
  public
    property NameValue: TJSValues read FNameValue;
  end;

  TJSTextBase = class(TJSExpressionResponseItem)
  strict protected
    FText: string;
    procedure InternalFormatTo(const AFormatter: TJSFormatter); override;
  public
    property Text: string read FText write FText;

    function GetDebugDescription: string; override;
  end;

  TJSCode = class(TJSTextBase)
  strict protected
    procedure InternalFormatTo(const AFormatter: TJSFormatter); override;
  public
    property JSCode: string read FText write FText;
  end;

  TJSON = class(TJSTextBase)
  public
    property JSON: string read FText write FText;
    function GetContentType: string; override;
  end;

  THTML = class(TJSTextBase)
  public
    property HTML: string read FText write FText;
    function GetContentType: string; override;
  end;

implementation

uses
  DateUtils
  , Kitto.Rtti
  , Kitto.JS
  , Kitto.Web.Application
  ;

{ TKWebResponse }

class procedure TKWebResponse.ClearCurrent;
begin
  FreeAndNil(FCurrent);
end;

constructor TKWebResponse.Create(const AResponse: TWebResponse; const AOwnsResponse: Boolean);
begin
  Assert(Assigned(AResponse));
  inherited Create;
  FResponse := AResponse;
  FOwnsResponse := AOwnsResponse;
end;

destructor TKWebResponse.Destroy;
begin
  Assert(FItems.Count = 1);
  FItems.Pop.Free;
  FreeAndNil(FItems);
  if FOwnsResponse then
    FreeAndNil(FResponse);
  inherited;
end;

function TKWebResponse.GetContentType: string;
begin
  Result := FResponse.ContentType;
end;

class function TKWebResponse.GetCurrent: TKWebResponse;
begin
  Result := FCurrent;
end;

function TKWebResponse.GetCustomHeaders: TStrings;
begin
  Result := FResponse.CustomHeaders;
end;

function TKWebResponse.GetItems: TJSResponseItems;
begin
  Assert(FItems.Count > 0);

  Result := FItems.Peek;
end;

function TKWebResponse.HasItems: Boolean;
begin
  Result := Assigned(FItems);
end;

procedure TKWebResponse.Send;
begin
{ TODO : tunnel all responses (including downloads) through the response items? }
  // Don't overwrite custom responses.
  if Items.Count > 0 then
  begin
    //Content := Items.Consume;
    ReplaceContentStream(TStringStream.Create(Items.Consume, Items.Encoding));
    ContentType := Items.GetContentType;
  end;
  if FResponse.Content <> '' then
    FResponse.SendResponse;
end;

procedure TKWebResponse.ReplaceContentStream(const AStream: TStream);
var
  LContentStream: TStream;
begin
  if Assigned(FResponse.ContentStream) then
  begin
    LContentStream := FResponse.ContentStream;
    FResponse.ContentStream := nil;
    FreeAndNil(LContentStream);
  end;
  FResponse.ContentStream := AStream;
end;

procedure TKWebResponse.SetContentType(const Value: string);
begin
  FResponse.ContentType := Value;
end;

procedure TKWebResponse.SetCookie(const AName, AValue: string; const AExpires: TDateTime);
var
  LCookie: TCookie;
begin
  LCookie := FResponse.Cookies.Add;
  LCookie.Name := AName;
  LCookie.Value := AValue;
  LCookie.Path := '/';
  LCookie.Expires := AExpires;
end;

class procedure TKWebResponse.SetCurrent(const AValue: TKWebResponse);
begin
  FCurrent := AValue;
end;

procedure TKWebResponse.SetCustomHeader(const AName, AValue: string);
begin
  FResponse.SetCustomHeader(AName, AValue);
end;

procedure TKWebResponse.AfterConstruction;
begin
  inherited;
  FItems := TStack<TJSResponseItems>.Create;
  FItems.Push(TJSResponseItems.Create(nil));
end;

function TKWebResponse.BranchItems: TJSResponseItems;
begin
  Result := TJSResponseItems.Create(nil);
  FItems.Push(Result);

  Assert(FItems.Count > 0);
end;

procedure TKWebResponse.UnbranchItems(const AItems: TJSResponseItems; const AConsolidate: Boolean);
var
  LSender: TJSBase;
  LBranch: TJSResponseItems;
  LInitialCount: Integer;
begin
  Assert(Assigned(AItems));
  Assert(FItems.Count > 1);
  Assert(AItems = FItems.Peek);

  LInitialCount := FItems.Count;
  LBranch := FItems.Pop;
  if AConsolidate then
  begin
    if LBranch.Count > 0 then
    begin
      LSender := LBranch.Items[0].Sender;
      FItems.Peek.ExecuteJSCode(LSender, LBranch.Consume);
    end;
  end;
  FreeAndNil(LBranch);

  Assert(FItems.Count = LInitialCount - 1);
end;

function TKWebResponse.GetJSCode(const AMethod: TProc; const ASilent: Boolean): string;
var
  LResponseItemBranch: TJSResponseItems;
begin
  LResponseItemBranch := BranchItems;
  try
    AMethod;
    Result := LResponseItemBranch.Consume;
    if ASilent then
      Result := 'try { ' + Result + ' } catch(e) {};';
  finally
    UnbranchItems(LResponseItemBranch, False);
  end;
end;

{ TJSResponseItem }

procedure TJSResponseItem.AddDependency(const AItem: TJSCreateObject);
begin
  if Assigned(AItem) and (AItem <> Self) and not FDependencies.Contains(AItem) then
    FDependencies.Add(AItem);
end;

procedure TJSResponseItem.AfterConstruction;
begin
  inherited;
  FDependencies := TList<TJSCreateObject>.Create;
  FEmitted := False;
  FCreationDateTime := Now;
end;

procedure TJSResponseItem.ChangeSender(const ASender: TJSBase);
begin
  FSender := ASender;
end;

constructor TJSResponseItem.Create(const ASender: TJSBase; const ARoot: TJSResponseItems);
begin
  inherited Create(nil);
  FSender := ASender;
  FRoot := ARoot;
end;

function TJSResponseItem.DependsOn(const AItem: TJSCreateObject): Boolean;
var
  LItem: TJSResponseItem;
begin
  for LItem in FDependencies do
    if LItem = AItem then
      Exit(True);
  Result := False;
end;

destructor TJSResponseItem.Destroy;
begin
  FreeAndNil(FDependencies);
  inherited;
end;

procedure TJSResponseItem.Emit(const AEmittedItems: TList<TJSResponseItem>);
var
  LItem: TJSResponseItem;
begin
  if FEmitted then
    Exit;

  if Assigned(FDependencies) then
    for LItem in FDependencies do
      LItem.Emit(AEmittedItems);
  if AllDependenciesEmitted(AEmittedItems) then
  begin
    AEmittedItems.Add(Self);
    FEmitted := True;
  end
end;

procedure TJSResponseItem.FormatTo(const AFormatter: TJSFormatter);
begin
  Assert(Assigned(AFormatter));

  if FCachedText <> '' then
    AFormatter.Add(FCachedText)
  else
    InternalFormatTo(AFormatter);
end;

function TJSResponseItem.AllDependenciesEmitted(const AEmittedItems: TList<TJSResponseItem>): Boolean;
var
  LItem: TJSResponseItem;
begin
  if Assigned(FDependencies) then
    for LItem in FDependencies do
      if not LItem.FEmitted then
        Exit(False);
  Result := True;
end;

function TJSResponseItem.AsFormattedText: string;
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

function TJSResponseItem.GetContentType: string;
begin
  Result := 'text/javascript';
end;

function TJSResponseItem.GetDebugDescription: string;
begin
  if Assigned(FSender) then
    Result := FSender.JSName + ': ' + ClassName
  else
    Result := 'nil: ' + ClassName;
end;

function TJSResponseItem.GetDependencies: TArray<TJSCreateObject>;
begin
  Result := FDependencies.ToArray;
end;

function TJSResponseItem.GetDependency(I: Integer): TJSCreateObject;
begin
  Result := FDependencies[I];
end;

function TJSResponseItem.GetDependencyCount: Integer;
begin
  Result := FDependencies.Count;
end;

function TJSResponseItem.GetFormattedCode: string;
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

procedure TJSResponseItem.InternalFormatTo(const AFormatter: TJSFormatter);
begin
end;

function TJSResponseItem.IsCode: Boolean;
begin
  Result := True;
end;

procedure TJSResponseItem.RemoveDependency(const AItem: TJSCreateObject);
begin
  Assert(FDependencies.Remove(AItem) >= 0);
end;

procedure TJSResponseItem.UnEmit;
begin
  FEmitted := False;
end;

procedure TJSResponseItem.UnlinkFromSender;
begin
  Assert(Assigned(Sender));
  Assert(FCachedText = '');

  FCachedText := AsFormattedText;
  ChangeSender(nil);
end;

{ TJSCreateObject }

procedure TJSCreateObject.AfterConstruction;
begin
  inherited;
  Assert(Sender is TJSObject);

  FItems := TList<TJSNamedCreateObject>.Create;
end;

destructor TJSCreateObject.Destroy;
var
  I: Integer;
begin
  for I := 0 to FItems.Count - 1 do
    FItems[I].CreateObject.Free;
  FreeAndNil(FItems);
  inherited;
end;

function TJSCreateObject.FindObjectCreateItem(const ASender: TJSBase): TJSCreateObject;
var
  LItem: TJSNamedCreateObject;
begin
  Result := nil;
  for LItem in FItems do
    if LItem.CreateObject.Sender = ASender then
      Exit(LItem.CreateObject);
end;

function TJSCreateObject.IsCode: Boolean;
begin
  Result := False;
end;

procedure TJSCreateObject.CreateInternalObject(const AAttributeName: string; const ASender: TJSBase);
var
  LItem: TJSNamedCreateObject;
begin
  LItem.Name := AAttributeName;
  LItem.CreateObject := TJSCreateObject.Create(ASender, FRoot);
  FItems.Add(LItem);
end;

procedure TJSCreateObject.InternalFormatTo(const AFormatter: TJSFormatter);
var
  LJSName: string;
  LConstructionAdded: Boolean;
begin
  inherited;
  LConstructionAdded := False;
  if not TJSObject(Sender).IsInline and not TJSObject(Sender).IsInternal then
  begin
    LJSName := Sender.JSName;
    AFormatter.AddIndentedLine(LJSName + ' = new ' + Sender.JSClassName + '(');
    AFormatter.Indent.AddIndent;
    LConstructionAdded := True;
  end;

  { TODO : what if it's empty? }
  AFormatter.OpenObject;
  TJSObject(Sender).JSConfig.FormatTo(AFormatter);
  AFormatter.CloseObject;
  if LConstructionAdded then
  begin
    AFormatter.SkipLine.Outdent;
    AFormatter.AddIndentedLine(');');
    AFormatter.AddIndentedLine(LJSName + '.nm = "' + LJSName + '";');
  end;
end;

{ TExtJSCode }

procedure TJSCode.InternalFormatTo(const AFormatter: TJSFormatter);
begin
  inherited;
  AFormatter.AddIndentedLine('');
end;

{ TJSResponseItems }

procedure TJSResponseItems.AddHTML(const AHTML: string);
var
  LItem: THTML;
begin
  LItem := THTML.Create(nil, Self);
  try
    LItem.HTML := AHTML;
    FList.Add(LItem);
  except
    FreeAndNil(LItem);
    raise;
  end;
end;

procedure TJSResponseItems.AddJSON(const AJSON: string);
var
  LItem: TJSON;
begin
  LItem := TJSON.Create(nil, Self);
  try
    LItem.JSON := AJSON;
    FList.Add(LItem);
  except
    FreeAndNil(LItem);
    raise;
  end;
end;

procedure TJSResponseItems.AddObjectDependency(const ADependentObject, ADependedUponObject: TJSBase);
var
  LDependentObjectCreateItem: TJSCreateObject;
begin
  if Assigned(ADependentObject) and Assigned(ADependedUponObject) then
  begin
    LDependentObjectCreateItem := FindObjectCreateItem(ADependentObject);
    if Assigned(LDependentObjectCreateItem) then
      LDependentObjectCreateItem.AddDependency(FindObjectCreateItem(ADependedUponObject));
  end;
end;

procedure TJSResponseItems.AfterConstruction;
begin
  inherited;
  FList := TObjectList<TJSResponseItem>.Create;
  FEmittedItems := TList<TJSResponseItem>.Create;
end;

function TJSResponseItems.AjaxCallMethod(const AObject: TJSBase; const AMethodName: string): TJSAjaxCall;
begin
  Result := TJSAjaxCall.Create(AObject, Self);
  try
    Result.AddDependency(FindObjectCreateItem(AObject));
    Result.CallName := AMethodName;
    FList.Add(Result);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

procedure TJSResponseItems.CreateObject(const AObject: TJSBase);
begin
  Assert(Assigned(AObject));

  FList.Add(TJSCreateObject.Create(AObject, Self));
//  if Assigned(AObject.Owner) then
//    AddObjectDependency(AObject, AObject.Owner);
end;

destructor TJSResponseItems.Destroy;
begin
  FreeAndNil(FEmittedItems);
  Clear;
  FreeAndNil(FList);
  inherited;
end;

function TJSResponseItems.ExecuteJSCode(const AJSCode: string): TJSCode;
begin
  Result := ExecuteJSCode(nil, AJSCode);
end;

function TJSResponseItems.ExecuteJSCode(const AObject: TJSBase; const AJSCode: string): TJSCode;
begin
  if AJSCode <> '' then
  begin
    Result := TJSCode.Create(AObject, Self);
    try
      Result.AddDependency(FindObjectCreateItem(AObject));
      Result.JSCode := AJSCode;
      FList.Add(Result);
    except
      FreeAndNil(Result);
      raise;
    end;
  end
  else
    Result := nil;
end;

procedure TJSResponseItems.SortByDependency;
var
  LList: TList<TJSResponseItem>;
  LTopLevelNodes: TQueue<TJSResponseItem>;
  LItem: TJSResponseItem;
  LDependents: TArray<TJSResponseItem>;
  LDependent: TJSResponseItem;

  procedure GetTopLevelNodes;
  var
    LItem: TJSResponseItem;
  begin
    LTopLevelNodes.Clear;
    for LItem in FList do
      if LItem.DependencyCount = 0 then
        LTopLevelNodes.Enqueue(LItem);
  end;

  function GetDependentNodes(const AItem: TJSCreateObject): TArray<TJSResponseItem>;
  var
    LItem: TJSResponseItem;
  begin
    SetLength(Result, 0);
    for LItem in FList do
    begin
      if LItem.DependsOn(AItem) then
        Result := Result + [LItem];
    end;
  end;

  function DependenciesExist: Boolean;
  var
    LItem: TJSResponseItem;
  begin
    for LItem in FList do
      if LItem.DependencyCount > 0 then
        Exit(True);
    Result := False;
  end;

  function GetDebugString(const AList: TList<TJSResponseItem>): string;
  var
    LItem: TJSResponseItem;
    I: Integer;
  begin
    Result := '';
    for LItem in AList do
    begin
      if LItem.DependencyCount > 0 then
      begin
        Result := Result + LItem.GetDebugDescription + '  DEPENDS ON  ';
        for I := 0 to LItem.DependencyCount - 1 do
          Result := Result + ' ' + LItem.Dependencies[I].GetDebugDescription;
        Result := Result + sLinebreak;
      end;
    end;
  end;

begin
  (* Topological sort
    Courtesy Wikipedia: http://en.wikipedia.org/wiki/Topological_sorting
    //
    L ← Empty list that will contain the sorted elements
    S ← Set of all nodes with no incoming edges
    while S is non-empty do
      remove a node n from S
      insert n into L
      for each node m with an edge e from n to m do
        remove edge e from the graph
      if m has no other incoming edges then
        insert m into S
      if graph has edges then
        return error (graph has at least one cycle)
      else
        return L (a topologically sorted order)
    end;
  *)
  LList := TList<TJSResponseItem>.Create;
  try
    LTopLevelNodes := TQueue<TJSResponseItem>.Create;
    try
      GetTopLevelNodes;
      while LTopLevelNodes.Count > 0 do
      begin
        LItem := LTopLevelNodes.Dequeue;
        LList.Add(LItem);
        if LItem is TJSCreateObject then
        begin
          LDependents := GetDependentNodes(TJSCreateObject(LItem));
          for LDependent in LDependents do
          begin
            LDependent.RemoveDependency(TJSCreateObject(LItem));
            if LDependent.DependencyCount = 0 then
              LTopLevelNodes.Enqueue(LDependent);
          end;
        end;
      end;
      if DependenciesExist then
        raise Exception.CreateFmt('Cannot sort response items by dependency - Graph cycle detected.#13#10%s',
          [GetDebugString(FList)]);
      // Clear the list without freeing the items.
      while FList.Count > 0 do
        FList.Extract(FLIst[0]);
      FList.AddRange(LList.ToArray);
    finally
      FreeAndNil(LTopLevelNodes);
    end;
  finally
    FreeAndNil(LList);
  end;
end;

function TJSResponseItems.AsFormattedString: string;
var
  I: Integer;
  LFormatter: TJSFormatter;
begin
  SortByDependency;
  FEmittedItems.Clear;

  for I := 0 to FList.Count - 1 do
    FList[I].Emit(FEmittedItems);

  LFormatter := TJSFormatter.Create;
  try
    Result := '';
    for I := 0 to FEmittedItems.Count - 1 do
      FEmittedItems[I].FormatTo(LFormatter);
    Result := LFormatter.FormattedText;
  finally
    FreeAndNil(LFormatter);
  end;
  FEmittedItems.Clear;
  for I := 0 to FList.Count - 1 do
    FList[I].UnEmit;
end;

function TJSResponseItems.Consume: string;
begin
  Result := AsFormattedString;
  Clear;
end;

function TJSResponseItems.CallMethod(const AObject: TJSBase; const AMethodName: string): TJSMethodCall;
begin
  Result := TJSMethodCall.Create(AObject, Self);
  try
    Result.AddDependency(FindObjectCreateItem(AObject));
    Result.CallName := AMethodName;
    FList.Add(Result);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

procedure TJSResponseItems.Clear;
begin
  FList.Clear;
end;

function TJSResponseItems.FindObjectCreateItem(const AObject: TJSBase): TJSCreateObject;
var
  LResponseItem: TJSResponseItem;
begin
  Result := nil;
  if Assigned(AObject) then
  begin
    for LResponseItem in FList do
    begin
      if (LResponseItem is TJSCreateObject) then
      begin
        if (TJSCreateObject(LResponseItem).Sender = AObject) then
          Result := TJSCreateObject(LResponseItem)
        else
          // Look for subobjects.
          Result := TJSCreateObject(LResponseItem).FindObjectCreateItem(AObject);
        if Assigned(Result) then
          Break;
      end;
    end;
    if Result = nil then
      Result := FindObjectCreateItem(AObject.Owner);
  end;
end;

procedure TJSResponseItems.ForEach(const AProc: TProc<TJSResponseItem>);
var
  LItem: TJSResponseItem;
begin
  Assert(Assigned(AProc));

  for LItem in FList do
    AProc(LItem);
end;

function TJSResponseItems.GetContentType: string;
begin
  if FList.Count = 0 then
    Result := 'text/html'
  else
    Result := FList[0].GetContentType;
  Result := Result + '; charset=' + FCharset;
end;

function TJSResponseItems.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TJSResponseItems.GetEncoding: TEncoding;
begin
  if FCharset = 'utf-8' then
    Result := TEncoding.UTF8
  else
    Result := TEncoding.ANSI;
end;

function TJSResponseItems.GetItem(I: Integer): TJSResponseItem;
begin
  Result := FList[I];
end;

function TJSResponseItems.GetProperty(const AObject: TJSBase; const APropertyName: string): TJSGetProperty;
begin
  Result := TJSGetProperty.Create(AObject, Self);
  try
    Result.AddDependency(FindObjectCreateItem(AObject));
    Result.PropertyName := APropertyName;
    FList.Add(Result);
  except
    FreeAndNil(Result);
  end;
end;

procedure TJSResponseItems.SetProperty(const AObject: TJSBase; const AName: string; const AValue: TJSBase);
begin
  DoSetProperty(AObject,
    procedure (AItem: TJSSetProperty)
    begin
      AItem.AddDependency(FindObjectCreateItem(AValue));
      AItem.NameValue.Values.SetObject(AName, AValue);
    end);
end;

procedure TJSResponseItems.DoSetProperty(const AObject: TJSBase; const ASetValueProc: TProc<TJSSetProperty>);
var
  LItem: TJSSetProperty;
begin
  Assert(Assigned(ASetValueProc));

  LItem := TJSSetProperty.Create(AObject, Self);
  try
    ASetValueProc(LItem);
    Assert(LItem.NameValue.Values.ChildCount = 1);
    // Qualify just added property name.
    LItem.NameValue.Values.Children[0].Name := AObject.JSName + '.' +
      LItem.NameValue.Values.Children[0].Name;
    FList.Add(LItem);
  except
    FreeAndNil(LItem);
    raise;
  end;
end;

procedure TJSResponseItems.SetProperty(const AObject: TJSBase; const AName, AValue: string);
begin
  DoSetProperty(AObject,
    procedure (AItem: TJSSetProperty)
    begin
      AItem.NameValue.Values.SetString(AName, AValue);
    end);
end;

procedure TJSResponseItems.SetProperty(const AObject: TJSBase; const AName: string; const AValue: TJSExpression);
begin
  DoSetProperty(AObject,
    procedure (AItem: TJSSetProperty)
    begin
      AItem.NameValue.Values.SetObject(AName, AValue);
    end);
end;

procedure TJSResponseItems.SetProperty(const AObject: TJSBase; const AName: string; const AValue: TDateTime);
begin
  DoSetProperty(AObject,
    procedure (AItem: TJSSetProperty)
    begin
      AItem.NameValue.Values.SetDateTime(AName, AValue);
    end);
end;

procedure TJSResponseItems.SetProperty(const AObject: TJSBase; const AName: string; const AValue: Boolean);
begin
  DoSetProperty(AObject,
    procedure (AItem: TJSSetProperty)
    begin
      AItem.NameValue.Values.SetBoolean(AName, AValue);
    end);
end;

procedure TJSResponseItems.SetProperty(const AObject: TJSBase; const AName: string; const AValue: Integer);
begin
  DoSetProperty(AObject,
    procedure (AItem: TJSSetProperty)
    begin
      AItem.NameValue.Values.SetInteger(AName, AValue);
    end);
end;

{ TJSMethodCall }

function TJSMethodCall.AddParam(const AValue: TJSExpression): TJSMethodCall;
begin
  if Assigned(AValue) then
  begin
    AddDependency(FRoot.FindObjectCreateItem(AValue.Owner));
    FParams.SetRawValue(FParams.Values.ChildCount.ToString, AValue.ExtractText);
  end
  else
    FParams.SetRawValue(FParams.Values.ChildCount.ToString, 'null');
  Result := Self;
end;

function TJSMethodCall.AddParam(const AValue: string): TJSMethodCall;
begin
  FParams.Values.SetString(FParams.Values.ChildCount.ToString, AValue);
  Result := Self;
end;

function TJSMethodCall.AddParam(const AValue: Boolean): TJSMethodCall;
begin
  FParams.Values.SetBoolean(FParams.Values.ChildCount.ToString, AValue);
  Result := Self;
end;

function TJSMethodCall.AddParam(const AValue: Integer): TJSMethodCall;
begin
  FParams.Values.SetInteger(FParams.Values.ChildCount.ToString, AValue);
  Result := Self;
end;

function TJSMethodCall.AddParam(const AValue: TJSBase): TJSMethodCall;
begin
  if Assigned(AValue) then
  begin
    AddDependency(FRoot.FindObjectCreateItem(AValue));
    FParams.Values.SetObject(FParams.Values.ChildCount.ToString, AValue);
  end
  else
    FParams.SetRawValue(FParams.Values.ChildCount.ToString, 'null');
  Result := Self;
end;

function TJSMethodCall.AddParam(const AValue: TDateTime): TJSMethodCall;
begin
  FParams.Values.SetDateTime(FParams.Values.ChildCount.ToString, AValue);
  Result := Self;
end;

procedure TJSMethodCall.AfterConstruction;
begin
  inherited;
  FParams := TJSValues.Create(Self);
  // Method params are not named - connector is useless.
  FParams.NameValueConnector := '';
end;

function TJSMethodCall.GetDebugDescription: string;
begin
  Result := inherited GetDebugDescription + '.' + CallName;
end;

procedure TJSMethodCall.InternalFormatTo(const AFormatter: TJSFormatter);
var
  LName: string;
begin
  inherited;
  if not IsExpressionExtracted then
  begin
    Assert(FCallName <> '');
    Assert(Assigned(Sender));
    Assert(Sender.JSName <> '');

    if (Sender is TJSObject) and TJSObject(Sender).IsInline then
      LName := 'getObject("' + Sender.JSName + '")'
    else
      LName := Sender.JSName;

    AFormatter.AddIndented(LName + '.' + FCallName);
    if Params.Values.ChildCount > 0 then
    begin
      AFormatter.OpenRound;
      Params.FormatTo(AFormatter);
      AFormatter.CloseRound.AddLine(';');
    end
    else
      AFormatter.Add('();');
  end;
end;

{ TJSSetProperty }

procedure TJSSetProperty.AfterConstruction;
begin
  inherited;
  FNameValue := TJSValues.Create(Self);
  FNameValue.NameValueConnector := ' = ';
end;

procedure TJSSetProperty.InternalFormatTo(const AFormatter: TJSFormatter);
begin
  Assert(Assigned(Sender));
  Assert(FNameValue.Values.ChildCount = 1);
  inherited;

  NameValue.FormatTo(AFormatter);
  AFormatter.AddLine(';');
end;

{ TJSGetProperty }

procedure TJSGetProperty.InternalFormatTo(const AFormatter: TJSFormatter);
begin
  Assert(Assigned(Sender));
  Assert(FPropertyName <> '');

  inherited;
  if not IsExpressionExtracted then
    AFormatter.AddIndentedLine(Sender.JSName + '.' + FPropertyName + ';');
end;

{ TJSExpressionResponseItem }

function TJSExpressionResponseItem.FunctionArgs(const AFunctionArgs: string): TJSExpressionResponseItem;
begin
  FFunctionArgs := AFunctionArgs;
  Result := Self;
end;

function TJSExpressionResponseItem.FunctionReturn(const AFunctionReturn: string): TJSExpressionResponseItem;
begin
  FFunctionReturn := AFunctionReturn;
  Result := Self;
end;

destructor TJSExpressionResponseItem.Destroy;
begin
  if FOwnsExpression then
    FreeAndNil(FExpression);
  inherited;
end;

function TJSExpressionResponseItem.GetAsExpression: TJSExpression;
begin
  if not Assigned(FExpression) then
  begin
    FOwnsExpression := Sender = nil;
    FExpression := TJSExpression.Create(Sender);
    FExpression.Text := GetFormattedCode;
  end;
  Result := FExpression;
end;

function TJSExpressionResponseItem.GetAsFunction: TJSFunction;
begin
  if not Assigned(FExpression) then
  begin
    FOwnsExpression := Sender = nil;
    FExpression := TJSFunction.Create(Sender);
    FExpression.Text := TJS.WrapInAnonymousFunction(FFunctionArgs, GetFormattedCode, FFunctionReturn);
  end;
  Result := TJSFunction(FExpression);
end;

function TJSExpressionResponseItem.HasExpression: Boolean;
begin
  Result := Assigned(FExpression);
end;

function TJSExpressionResponseItem.IsExpressionExtracted: Boolean;
begin
  Result := HasExpression and FExpression.IsExtracted;
end;

{ TJSAjaxCall }

procedure TJSAjaxCall.InternalFormatTo(const AFormatter: TJSFormatter);
begin
  //inherited;
  if not IsExpressionExtracted then
  begin
    Assert(CallName <> '');
    Assert(Assigned(Sender));

    AFormatter.SkipLine.Indent;
    AFormatter.AddIndented('Ext.Ajax.request(').SkipLine.Indent.AddIndent.OpenObject;
    AFormatter.AddIndentedPairLine('url', TKWebApplication.Current.GetMethodURL(Sender.JSName, CallName));
    AFormatter.AddIndentedPairLine('method', FHttpMethod);
    if (FHttpMethod = 'POST') and (FPostData <> '') then
      AFormatter.AddIndentedPairLine('jsonData', FPostData, False);
    AddParams(AFormatter);
    AFormatter.AddIndentedPairLine('success', 'AjaxSuccess', False);
    AFormatter.AddIndentedPair('failure', 'AjaxFailure', False);
    // Automatically done by ExtJS. Add it as required when using other Ajax implementations.
    //AFormatter.AddIndentedPair('headers', '{"X-Requested-With": "XMLHttpRequest"}', False);
    AFormatter.DeleteTrailing(',');
    AFormatter.CloseObject.SkipLine.Outdent.AddIndentedLine(');');
  end;
end;

function TJSAjaxCall.Get: TJSAjaxCall;
begin
  FHttpMethod := 'GET';
  Result := Self;
end;

function TJSAjaxCall.Post(const AData: string): TJSAjaxCall;
begin
  Assert(AData <> '');

  FHttpMethod := 'POST';
  FPostData := AData;
  Result := Self;
end;

function TJSAjaxCall.SetMethod(const AMethod: TJSProcedure): TJSAjaxCall;
var
  LObject: TObject;
begin
  Assert(Assigned(Sender));

  LObject := TObject(TMethod(AMethod).Data);
  if Assigned(LObject) and (LObject is TJSBase) and (LObject <> Sender) then
    ChangeSender(TJSBase(LObject));
  CallName := GetMethodName(AMethod);
  Result := Self;
end;

procedure TJSAjaxCall.AfterConstruction;
begin
  inherited;
  FHttpMethod := 'GET';
  Params.ParamConnector := ', ';
  Params.ParamValuePrefix := 'encodeURIComponent(';
  Params.ParamValueSuffix := ')';
  Params.NameValueConnector := ': ';
end;

function TJSAjaxCall.Event: TJSAjaxCall;
begin
  Params.Values.SetString('Event', CallName);
  CallName := 'HandleEvent';
  Result := Self;
end;

function TJSAjaxCall.AddParam(const AName: string; const AValue: Integer): TJSAjaxCall;
begin
  FParams.Values.SetInteger(AName, AValue);
  Result := Self;
end;

function TJSAjaxCall.AddParam(const AName: string; const AValue: Boolean): TJSAjaxCall;
begin
  FParams.Values.SetBoolean(AName, AValue);
  Result := Self;
end;

function TJSAjaxCall.AddParam(const AName, AValue: string): TJSAjaxCall;
begin
  FParams.Values.SetString(AName, AValue);
  Result := Self;
end;

function TJSAjaxCall.AddParam(const AName: string; const AValue: TJSExpression): TJSAjaxCall;
begin
  if Assigned(AValue) then
    FParams.SetRawValue(AName, AValue.ExtractText)
  else
    FParams.SetRawValue(AName, '');
  Result := Self;
end;

function TJSAjaxCall.AddParam(const AName: string; const AValue: TDateTime): TJSAjaxCall;
begin
  FParams.Values.SetDateTime(AName, AValue);
  Result := Self;
end;

function TJSAjaxCall.AddParam(const AName: string; const AValue: TJSBase): TJSAjaxCall;
begin
  FParams.Values.SetObject(AName, AValue);
  Result := Self;
end;

procedure TJSAjaxCall.AddParams(const AFormatter: TJSFormatter);
begin
  AFormatter.AddIndentedPairLine('params', '{' + Params.AsFormattedText + '}', False);
end;

function TJSAjaxCall.AddRawParam(const AName, AValue: string): TJSAjaxCall;
begin
  FParams.SetRawValue(AName, AValue);
  Result := Self;
end;

{ TJSTextBase }

function TJSTextBase.GetDebugDescription: string;
begin
  Result := inherited GetDebugDescription + '.' + Text;
end;

procedure TJSTextBase.InternalFormatTo(const AFormatter: TJSFormatter);
begin
  inherited;
  if not IsExpressionExtracted then
    AFormatter.Add(FText);
end;

{ THTML }

function THTML.GetContentType: string;
begin
  Result := 'text/html';
end;

{ TJSON }

function TJSON.GetContentType: string;
begin
  Result := 'application/json';
end;

end.

