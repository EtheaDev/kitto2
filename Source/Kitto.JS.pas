{ -------------------------------------------------------------------------------
  Copyright 2012-2018 Ethea S.r.l.

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
  ------------------------------------------------------------------------------- }

{ -------------------------------------------------------------------------------
  Loosely based on code from ExtPascal
  Author: Wanderlan Santos dos Anjos. wanderlan.anjos@gmail.com
  Home: http://extpascal.googlecode.com
  License: BSD, http://www.opensource.org/licenses/bsd-license.php
  ------------------------------------------------------------------------------- }
unit Kitto.JS;

{$I Kitto.Defines.inc}

interface

uses
  SysUtils
  , Classes
  , Generics.Collections

  , EF.Tree
  , EF.Intf
  , EF.ObserverIntf
  , EF.Localization
  , EF.Macros

  , gnugettext

  , Kitto.Config
  , Kitto.Metadata.Views
  , Kitto.JS.Base
  , Kitto.JS.Types
  , Kitto.JS.Formatting
  ;

type
  TJSObjectArray = class;
  TJSObject = class;
  TJSObjectClass = class of TJSObject;

  TJSObject = class(TJSBase)
  strict private
    { TODO : Maybe we could replace it with an extraction from JSName }
    FAttributeName: string;
    FJSConfig: TJSValues;
    FIsInline: Boolean;
    function GetDownloadJS(const AMethod: TJSProcedure): string;
  strict protected
    function GetObjectNamePrefix: string; virtual;
    function GetJSIdConfigName: string; virtual;
    procedure InitDefaults; override;
    procedure InitInlineDefaults; virtual;
    function CreateConfigObject(const AAttributeName: string): TJSObject; overload;
    // Cannot use a generic here a TJSObject is not completely defined yet.
    function CreateConfigObject(const AClass: TJSObjectClass; const AAttributeName: string): TJSObject; overload;
    function CreateConfigObjectArray(const AAttributeName: string): TJSObjectArray;
    procedure DoHandleEvent(const AEventName: string); virtual;
  protected
    procedure DependsUpon(const AObject: TJSObject);
  public
    constructor Create(const AOwner: TJSBase); override;
    constructor CreateInternal(const AOwner: TJSBase; const AAttributeName: string);
    constructor CreateInline(const AOwner: TJSBase);
    constructor CreateAndAddToArray(const AArray: TJSObjectArray);
    constructor CreateInlineAndAddToArray(const AArray: TJSObjectArray);
    destructor Destroy; override;

    function AsJSObject: TJSObject;

    procedure Delete;

    function IsInternal: Boolean;
    property IsInline: Boolean read FIsInline;

    property JSConfig: TJSValues read FJSConfig;
    // Assigned if the object was created with CreateInternal.
    property AttributeName: string read FAttributeName;

    /// <summary>
    ///  Returns a JS array with the passed JSON content.
    /// </summary>
    function JSArray(const AJSON: string): TJSObject;
    function JSObject(const AJSON: string; const AObjectConstructor: string = ''; const ACurlyBrackets: Boolean = True): TJSObject;
    function JSExpressionFromCodeBlock(const ACode: string): TJSExpression;
    function GetJSCode(const AMethod: TProc; const ASilent: Boolean = False): string;
    function JSExpressionFromExpr(const AExpr: string; const AValues: array of TJSExpression): TJSExpression;

    function GenerateAnonymousFunction(const AArgs, ABody: string; const AReturn: string = ''): TJSExpression; overload;
    function GenerateAnonymousFunction(const ABody: string): TJSExpression; overload;
    function GenerateAnonymousFunction(const AArgs: string; const AExpression: TJSExpression; const AReturn: string = ''): TJSExpression; overload;
    function GenerateAnonymousFunction(const AExpression: TJSExpression): TJSExpression; overload;

    procedure Download(Method: TJSProcedure); overload;

    function GetMethodURL(const AMethod: TJSProcedure): string; overload;
    function GetMethodURL(const AMethodName: string): string; overload;
    class function AddParamsToURL(const AURL, AParams: string): string;

    function CharsToPixels(const AChars: Integer; const AOffset: Integer = 0): TJSExpression;
    function LinesToPixels(const ALines: Integer): TJSExpression;

    function SetConfigItem(const AName, AMethodName: string; const AValue: string): string; overload;
    function SetConfigItem(const AName, AMethodName: string; const AValue: Boolean): Boolean; overload;
    function SetConfigItem(const AName, AMethodName: string; const AValue: Integer): Integer; overload;
    function SetConfigItem(const AName, AMethodName: string; const AValue: TDateTime): TDateTime; overload;
    function SetConfigItem(const AName, AMethodName: string; const AValue: Double): Double; overload;
    function SetConfigItem(const AName, AMethodName: string; const AValue: TJSObject): TJSObject; overload;
    function SetConfigItem(const AName, AMethodName: string; const AValue: TJSExpression): TJSExpression; overload;

    function SetConfigItem(const AName, AValue: string): string; overload;
    function SetConfigItem(const AName: string; const AValue: TJSObject): TJSObject; overload;
    function SetConfigItem(const AName: string; const AValue: Integer): Integer; overload;
    function SetConfigItem(const AName: string; const AValue: Boolean): Boolean; overload;
    function SetConfigItem(const AName: string; const AValue: Double): Double; overload;
    function SetConfigItem(const AName: string; const AValue: TJSExpression): TJSExpression; overload;

    procedure AddItem(const AItems: TJSObjectArray; const AItem: TJSObject); overload;

    function SetConfigItemOrProperty(const AName, AValue: string): string; overload;
    function SetConfigItemOrProperty(const AName: string; const AValue: Boolean): Boolean; overload;

    function SetProperty(const AName: string; const AValue: Integer): Integer; overload;
    function SetProperty(const AName, AValue: string): string; overload;
    function SetProperty(const AName: string; const AValue: TJSExpression): TJSExpression; overload;
    function SetProperty(const AName: string; const AValue: TJSObject): TJSObject; overload;
    function SetProperty(const AName: string; const AValue: Boolean): Boolean; overload;
    function SetProperty(const AName: string; const AValue: TDateTime): TDateTime; overload;

    { TODO : move to request? }
    function ParamAsInteger(const AParamName: string): Integer;
    function ParamAsBoolean(const AParamName: string): Boolean;
    function ParamAsString(const AParamName: string): string;
    function ParamAsObject(const AParamName: string): TJSObject;

    /// <summary>
    ///  Applies all children of the specified tree to the current object's config,
    ///  matching them by name except for the first letter which is converted to lower case.
    ///  Currently only direct children values are copied.
    /// </summary>
    procedure ApplyTreeToConfig(const ATree: TEFTree);

    procedure HandleEvent;
  end;

  /// <summary>
  ///  An object that is rendered as the plain contents of its JSName property,
  ///  such as an inline array.
  /// </summary>
  TJSRaw = class(TJSObject);

  /// <summary>
  ///  A container for JS objects.
  /// </summary>
  IJSContainer = interface(IEFInterface)
    ['{170D8F2B-60A2-4C40-A31F-29C7A9AED295}']
    function AsJSObject: TJSObject;
    procedure AddItem(const AItem: TJSObject);
  end;

  /// <summary>
  ///  Interface for controllers. Controllers manage views to build
  ///  the user interface.
  /// </summary>
  IJSController = interface(IEFInterface)
    ['{FCDFC7CC-E202-4C20-961C-11255CABE497}']

    /// <summary>
    ///  Renders AView according to the Config.
    /// </summary>
    procedure Display;

    function GetConfig: TEFNode;
    property Config: TEFNode read GetConfig;

    function GetView: TKView;
    procedure SetView(const AValue: TKView);
    property View: TKView read GetView write SetView;

    function GetContainer: IJSContainer;
    procedure SetContainer(const AValue: IJSContainer);
    property Container: IJSContainer read GetContainer write SetContainer;

    /// <summary>
    ///  Returns True if the controller should be freed right after
    ///  calling Display because it does all its job inside that method, and
    ///  False if the controller stays on screen and is interactive instead.
    /// </summary>
    function IsSynchronous: Boolean;

    function AsJSObject: TJSObject;
  end;

  /// <summary>
  ///  A container for one or more controllers.
  /// </summary>
  IJSControllerContainer = interface(IJSContainer)
    ['{37EA31CA-544F-4DBD-8C04-D08E30517C99}']

    /// <summary>
    ///  Called after creating a subcontroller to give this container a chance to
    ///  initialize some of its configs or settings.
    /// </summary>
    procedure InitSubController(const ASubController: IJSController);

    /// <summary>
    ///  If the container supports the concept of active controller, this
    ///  method sets the specified controller as the visually active one
    ///  (such as the active page of a tab panel). Otherwise this method does
    ///  nothing.
    /// </summary>
    procedure SetActiveSubController(const ASubController: IJSController);
  end;

  IJSStatusHost = interface(IEFInterface)
    ['{90737203-A4D9-4C31-AFD7-FDBCF5A7B7D2}']
    function ShowBusy: TJSExpression;
    function ClearStatus: TJSExpression;
  end;

  TJSObjectArray = class(TJSObject)
  private
    FObjects: TObjectList<TJSObject>;
    function GetObject(I: Integer): TJSObject;
    function GetCount: Integer;
    function OwnerJSObject: TJSObject;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;

    class function From(const AOwner: TJSBase; const AArray: TArray<TJSObject>): TJSObjectArray;

    property Objects[I: Integer]: TJSObject read GetObject; default;
    function Add(const AObject: TJSObject): Integer;
    function AddInternal(const AObject: TJSObject): Integer;
    function Remove(const AObject: TJSObject): Integer;
    function IndexOf(const AObject: TJSObject): Integer;
    function HasObject(const AObject: TJSObject): Boolean;
    property Count: Integer read GetCount;
  end;

implementation

uses
  StrUtils
  , DateUtils
  , Math
  , Types
  , Character
  , IOUtils
  , Variants
  , REST.Utils
  , System.NetEncoding
  , EF.StrUtils
  , EF.Sys
  , EF.Logger
  , Kitto.AccessControl
  , Kitto.Web.Application
  , Kitto.Web.Server
  , Kitto.Web.Request
  , Kitto.Web.Response
  , Kitto.Web.Session
  , Kitto.Ext.Controller
  ;

var
  _JSFormatSettings: TFormatSettings;

{ TJSObjectArray }

destructor TJSObjectArray.Destroy;
begin
  FreeAndNil(FObjects);
  inherited;
end;

class function TJSObjectArray.From(const AOwner: TJSBase;
  const AArray: TArray<TJSObject>): TJSObjectArray;
var
  LObject: TJSObject;
begin
  Result := TJSObjectArray.Create(AOwner);
  for LObject in AArray do
    Result.Add(LObject);
end;

function TJSObjectArray.Add(const AObject: TJSObject): Integer;
begin
  Assert(Assigned(AObject));
//  Assert(AttributeName <> '');

  Result := AddInternal(AObject);
end;

function TJSObjectArray.GetCount: Integer;
begin
  Result := FObjects.Count;
end;

function TJSObjectArray.GetObject(I: Integer): TJSObject;
begin
  Result := FObjects[I];
end;

function TJSObjectArray.HasObject(const AObject: TJSObject): Boolean;
begin
  Result := IndexOf(AObject) >= 0;
end;

function TJSObjectArray.IndexOf(const AObject: TJSObject): Integer;
begin
  Result := FObjects.IndexOf(AObject);
end;

function TJSObjectArray.OwnerJSObject: TJSObject;
var
  LOwner: TJSBase;
begin
  Result := nil;
  LOwner := Owner;
  while Assigned(LOwner) do
  begin
    if LOwner is TJSObject then
      Exit(TJSObject(LOwner));
    LOwner := LOwner.Owner;
  end;
  Assert(Assigned(Result), 'Object array has no JSObject owner');
end;

function TJSObjectArray.Remove(const AObject: TJSObject): Integer;
begin
  Result := FObjects.Remove(AObject);
end;

function TJSObjectArray.AddInternal(const AObject: TJSObject): Integer;
begin
  Assert(Assigned(AObject));

  Result := FObjects.Add(AObject);
  OwnerJSObject.DependsUpon(AObject);
end;

procedure TJSObjectArray.AfterConstruction;
begin
  inherited;
  FObjects := TObjectList<TJSObject>.Create(False);
end;

{ TJSObject }

function TJSObject.GetMethodURL(const AMethod: TJSProcedure): string;
begin
  Result := GetMethodURL(GetMethodName(AMethod));
end;

function TJSObject.GetMethodURL(const AMethodName: string): string;
begin
  Result := TKWebApplication.Current.GetMethodURL(JSName, AMethodName);
end;

function TJSObject.GetObjectNamePrefix: string;
begin
  Result := 'obj';
end;

procedure TJSObject.HandleEvent;
begin
  DoHandleEvent(ParamAsString('Event'));
end;

procedure TJSObject.AddItem(const AItems: TJSObjectArray; const AItem: TJSObject);
begin
  if not AItems.HasObject(AItem) then
  begin
    AItems.Add(AItem);
    if FJSConfig.IsReadOnly then
      TKWebResponse.Current.Items.CallMethod(Self, 'add').AddParam(AItem);
  end;
end;

class function TJSObject.AddParamsToURL(const AURL, AParams: string): string;
begin
  if AURL.Contains('?') then
    Result := AURL + '&' + AParams
  else
    Result := AURL + '?' + AParams;
end;

procedure TJSObject.ApplyTreeToConfig(const ATree: TEFTree);
var
  I: Integer;
  LNode: TEFNode;
  LDataType: TEFDataType;
  LConfigItemName: string;
  LObject: TJSObject;
begin
  Assert(Assigned(ATree));

  for I := 0 to ATree.ChildCount - 1 do
  begin
    LNode := ATree.Children[I];
    LConfigItemName := FirstLowerCase(LNode.Name);
    // Nodes without values and with children are conventionally subobjects.
    if not VarIsNull(LNode.Value) and (LNode.ChildCount > 0) then
    begin
      LObject := SetConfigItem(LConfigItemName, TJSObject.CreateInline(Self));
      LObject.ApplyTreeToConfig(LNode);
    end
    else
    begin
      LDataType := LNode.DataType;
      if LDataType is TEFIntegerDataType then
        SetConfigItem(LConfigItemName, LNode.AsInteger)
      else if LDataType is TEFBooleanDataType then
        SetConfigItem(LConfigItemName, LNode.AsBoolean)
      else if LDataType is TEFBooleanDataType then
        SetConfigItem(LConfigItemName, LNode.AsBoolean)
      else if LDataType is TEFFloatDataType then
        SetConfigItem(LConfigItemName, LNode.AsFloat)
      else if LDataType is TEFStringDataType then
        SetConfigItem(LConfigItemName, LNode.AsString);
    end;
  end;
end;

function TJSObject.AsJSObject: TJSObject;
begin
  Result := Self;
end;


function TJSObject.CharsToPixels(const AChars: Integer; const AOffset: Integer = 0): TJSExpression;
begin
  Result := JSExpressionFromCodeBlock(Format('charsToPixels(%d, %d)', [AChars, AOffset]));
end;

function TJSObject.LinesToPixels(const ALines: Integer): TJSExpression;
begin
  Result := JSExpressionFromCodeBlock(Format('linesToPixels(%d)', [ALines]));
end;

// Deletes JS object from Browser memory
procedure TJSObject.Delete;
begin
  if Self <> nil then
    TKWebResponse.Current.Items.ExecuteJSCode('try {' + JSName + '.destroy(); delete ' + JSName + ';} catch(e) {};');
end;

procedure TJSObject.DependsUpon(const AObject: TJSObject);
begin
  TKWebResponse.Current.Items.AddObjectDependency(Self, AObject);
end;

destructor TJSObject.Destroy;
begin
  if (TKWebResponse.Current <> nil) and TKWebResponse.Current.HasItems then
    TKWebResponse.Current.Items.ForEach(
      procedure (AItem: TJSResponseItem)
      begin
        if AItem.Sender = Self then
          AItem.UnlinkFromSender;
      end
    );
  inherited;
end;

function TJSObject.GenerateAnonymousFunction(const AArgs, ABody, AReturn: string): TJSExpression;
begin
  Result := JSExpressionFromCodeBlock(TJS.WrapInAnonymousFunction(AArgs, ABody, AReturn));
end;

function TJSObject.GenerateAnonymousFunction(const AArgs: string; const AExpression: TJSExpression; const AReturn: string): TJSExpression;
begin
  Result := GenerateAnonymousFunction(AArgs, AExpression.ExtractText, AReturn);
end;

function TJSObject.GetDownloadJS(const AMethod: TJSProcedure): string;
begin
  if TKWebRequest.Current.IsBrowserIPhone or TKWebRequest.Current.IsBrowserIPad then
    Result := 'window.open("' + GetMethodURL(AMethod) + '");'
  else
    Result := 'Download.src="' + GetMethodURL(AMethod) + '";';
end;

procedure TJSObject.Download(Method: TJSProcedure);
begin
  TKWebResponse.Current.Items.ExecuteJSCode(Self, GetDownloadJS(Method));
end;

constructor TJSObject.Create(const AOwner: TJSBase);
begin
  Assert(TKWebSession.Current <> nil);
  Assert(Assigned(AOwner));
  inherited Create(AOwner);
  FJSConfig := TJSValues.Create(Self);
  JSName := TKWebSession.Current.ObjectSpace.GetNextJSName(GetObjectNamePrefix);
  TKWebResponse.Current.Items.CreateObject(Self);
  InitDefaults;
end;

constructor TJSObject.CreateInternal(const AOwner: TJSBase; const AAttributeName: string);
begin
  Assert(Assigned(AOwner));
  FIsInline := True;

  inherited Create(AOwner);
  FJSConfig := TJSValues.Create(Self);
  FAttributeName := AAttributeName;
  if (Owner.JSName <> '') and (FAttributeName <> '') then
    JSName := Owner.JSName + '.' + FAttributeName
  else
    JSName := TKWebSession.Current.ObjectSpace.GetNextJSName(GetObjectNamePrefix);
  InitDefaults;
end;

constructor TJSObject.CreateInline(const AOwner: TJSBase);
begin
  CreateInternal(AOwner, '');
  InitInlineDefaults;
end;

constructor TJSObject.CreateInlineAndAddToArray(const AArray: TJSObjectArray);
begin
  Assert(Assigned(AArray));

  CreateInline(AArray);
  AArray.Add(Self);
end;

function TJSObject.IsInternal: Boolean;
begin
  Result := JSName.Contains('.');
end;

function TJSObject.SetConfigItem(const AName, AValue: string): string;
begin
  FJSConfig.CheckReadOnly(AName);
  FJSConfig.Values.SetString(AName, AValue);
  Result := AValue;
end;

function TJSObject.SetConfigItem(const AName: string; const AValue: TJSExpression): TJSExpression;
begin
  FJSConfig.CheckReadOnly(AName);
  if Assigned(AValue) then
    FJSConfig.SetRawValue(AName, AValue.ExtractText)
  else
    FJSConfig.SetRawValue(AName, '');
  Result := AValue;
end;

function TJSObject.SetConfigItem(const AName, AMethodName: string;
  const AValue: Double): Double;
begin
  if FJSConfig.IsReadOnly then
    TKWebResponse.Current.Items.CallMethod(Self, AMethodName).AddParam(Avalue)
  else
    FJSConfig.Values.SetFloat(AName, AValue);
  Result := AValue;
end;

function TJSObject.SetConfigItem(const AName, AMethodName, AValue: string): string;
begin
  if FJSConfig.IsReadOnly then
    TKWebResponse.Current.Items.CallMethod(Self, AMethodName).AddParam(Avalue)
  else
    FJSConfig.Values.SetString(AName, AValue);
  Result := AValue;
end;

function TJSObject.SetConfigItem(const AName, AMethodName: string; const AValue: Integer): Integer;
begin
  if FJSConfig.IsReadOnly then
    TKWebResponse.Current.Items.CallMethod(Self, AMethodName).AddParam(Avalue)
  else
    FJSConfig.Values.SetInteger(AName, AValue);
  Result := AValue;
end;

function TJSObject.SetConfigItem(const AName, AMethodName: string; const AValue: Boolean): Boolean;
begin
  if FJSConfig.IsReadOnly then
    TKWebResponse.Current.Items.CallMethod(Self, AMethodName).AddParam(Avalue)
  else
    FJSConfig.Values.SetBoolean(AName, AValue);
  Result := AValue;
end;

constructor TJSObject.CreateAndAddToArray(const AArray: TJSObjectArray);
begin
  Assert(Assigned(AArray));

  Create(AArray);
  AArray.Add(Self);
end;

function TJSObject.CreateConfigObjectArray(const AAttributeName: string): TJSObjectArray;
begin
  Result := TJSObjectArray.CreateInternal(FJSConfig, AAttributeName);
  SetConfigItem(AAttributeName, Result);
end;

function TJSObject.CreateConfigObject(const AAttributeName: string): TJSObject;
begin
  Result := CreateConfigObject(TJSObject, AAttributeName);
end;

function TJSObject.CreateConfigObject(const AClass: TJSObjectClass; const AAttributeName: string): TJSObject;
begin
  Result := AClass.CreateInternal(FJSConfig, AAttributeName);
  SetConfigItem(AAttributeName, Result);
end;

procedure TJSObject.InitDefaults;
begin
  inherited;
  { TODO per gli store è storeId; virtuale? Sì, ma poi però ci liberiamo di JSName tout court e teniamo id.
  forse in qualche caso ci dobbiamo tenere le var globali }

  if (JSName <> '') and not JSName.Contains('.') then
    SetConfigItem(GetJSIdConfigName, JSName);
end;

function TJSObject.GetJSIdConfigName: string;
begin
  Result := 'id';
end;

procedure TJSObject.InitInlineDefaults;
var
  LXType: string;
begin
  LXType := JSXType;
  if LXType <> '' then
  begin
    if LXType.StartsWith('plugin.') then
      SetConfigItem('ptype', LXType.Substring(7))
    else if LXType.Contains('.') then
      SetConfigItem('type', LXType.Split(['.'])[1])
    else
      SetConfigItem('xtype', LXType);
  end;
end;

function TJSObject.JSArray(const AJSON: string): TJSObject;
begin
  Result := TJSRaw.CreateInline(Self);
  Result.JSName := '[' + AJSON + ']';
end;

function TJSObject.JSObject(const AJSON: string; const AObjectConstructor: string; const ACurlyBrackets: Boolean): TJSObject;
begin
  Result := TJSRaw.CreateInline(Self);
  try
    if ACurlyBrackets then
      Result.JSName := '{' + AJSON + '}'
    else
      Result.JSName := AJSON;
    if AObjectConstructor <> '' then
      Result.JSName := 'new ' + AObjectConstructor + '(' + Result.JSName + ')';
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TJSObject.JSExpressionFromExpr(const AExpr: string; const AValues: array of TJSExpression): TJSExpression;
var
  LExpr: string;
  I: Integer;
begin
  LExpr := AExpr;
  for I := Low(AValues) to High(AValues) do
    LExpr := LExpr.Replace('{func' + IntToStr(I) + '}', AValues[I].ExtractText);
  Result := JSExpressionFromCodeBlock(LExpr);
end;

procedure TJSObject.DoHandleEvent(const AEventName: string);
begin
end;

function TJSObject.JSExpressionFromCodeBlock(const ACode: string): TJSExpression;
begin
  Result := TJSExpression.Create(Self);
  Result.Text := ACode;
end;

function TJSObject.GetJSCode(const AMethod: TProc; const ASilent: Boolean): string;
begin
  Result := TKWebResponse.Current.GetJSCode(AMethod, ASilent);
end;

function TJSObject.ParamAsBoolean(const AParamName: string): Boolean;
begin
  Result := SameText(TKWebRequest.Current.GetQueryField(AParamName), 'true');
end;

function TJSObject.ParamAsInteger(const AParamName: string): Integer;
begin
  Result := StrToIntDef(TKWebRequest.Current.GetQueryField(AParamName), 0);
end;

function TJSObject.ParamAsObject(const AParamName: string): TJSObject;
begin
  Result := TJSObject(TKWebSession.Current.ObjectSpace.FindChildByJSName(TKWebRequest.Current.GetQueryField(AParamName)));
end;

function TJSObject.ParamAsString(const AParamName: string): string;
begin
  Result := TKWebRequest.Current.GetQueryField(AParamName);
end;

function TJSObject.SetConfigItem(const AName: string; const AValue: Integer): Integer;
begin
  FJSConfig.CheckReadOnly(AName);
  FJSConfig.Values.SetInteger(AName, AValue);
  Result := AValue;
end;

function TJSObject.SetConfigItem(const AName: string; const AValue: Boolean): Boolean;
begin
  FJSConfig.CheckReadOnly(AName);
  FJSConfig.Values.SetBoolean(AName, AValue);
  Result := AValue;
end;

function TJSObject.SetConfigItem(const AName: string; const AValue: Double): Double;
begin
  FJSConfig.CheckReadOnly(AName);
  FJSConfig.Values.SetFloat(AName, AValue);
  Result := AValue;
end;

function TJSObject.SetConfigItem(const AName, AMethodName: string; const AValue: TJSExpression): TJSExpression;
begin
  if FJSConfig.IsReadOnly then
    TKWebResponse.Current.Items.CallMethod(Self, AMethodName).AddParam(Avalue)
  else if Assigned(AValue) then
    FJSConfig.SetRawValue(AName, AValue.ExtractText)
  else
    FJSConfig.SetRawValue(AName, '');
  Result := AValue;
end;

function TJSObject.SetConfigItem(const AName, AMethodName: string; const AValue: TJSObject): TJSObject;
begin
  if FJSConfig.IsReadOnly then
  begin
    TKWebResponse.Current.Items.CallMethod(Self, AMethodName).AddParam(Avalue);
    Result := AValue;
  end
  else
    Result := SetConfigItem(AName, AValue);
end;

function TJSObject.SetConfigItemOrProperty(const AName: string; const AValue: Boolean): Boolean;
begin
  if FJSConfig.IsReadOnly then
    TKWebResponse.Current.Items.SetProperty(Self, AName, AValue)
  else
    FJSConfig.Values.SetBoolean(AName, AValue);
  Result := AValue;
end;

function TJSObject.SetProperty(const AName: string; const AValue: TJSExpression): TJSExpression;
begin
  TKWebResponse.Current.Items.SetProperty(Self, AName, AValue);
  Result := AValue;
end;

function TJSObject.SetProperty(const AName: string; const AValue: TJSObject): TJSObject;
begin
  TKWebResponse.Current.Items.SetProperty(Self, AName, AValue);
  Result := AValue;
end;

function TJSObject.SetProperty(const AName: string; const AValue: Boolean): Boolean;
begin
  TKWebResponse.Current.Items.SetProperty(Self, AName, AValue);
  Result := AValue;
end;

function TJSObject.SetProperty(const AName, AValue: string): string;
begin
  TKWebResponse.Current.Items.SetProperty(Self, AName, AValue);
  Result := AValue;
end;

function TJSObject.SetProperty(const AName: string; const AValue: Integer): Integer;
begin
  TKWebResponse.Current.Items.SetProperty(Self, AName, AValue);
  Result := AValue;
end;

function TJSObject.SetConfigItemOrProperty(const AName, AValue: string): string;
begin
  if FJSConfig.IsReadOnly then
    TKWebResponse.Current.Items.SetProperty(Self, AName, AValue)
  else
    FJSConfig.Values.SetString(AName, AValue);
  Result := AValue;
end;

function TJSObject.SetConfigItem(const AName: string; const AValue: TJSObject): TJSObject;
begin
  FJSConfig.CheckReadOnly(AName);
  FJSConfig.Values.SetObject(AName, AValue);
  DependsUpon(AValue);
  Result := AValue;
end;

function TJSObject.SetConfigItem(const AName, AMethodName: string; const AValue: TDateTime): TDateTime;
begin
  if FJSConfig.IsReadOnly then
    TKWebResponse.Current.Items.CallMethod(Self, AMethodName).AddParam(Avalue)
  else
    FJSConfig.Values.SetDateTime(AName, AValue);
  Result := AValue;
end;

function TJSObject.SetProperty(const AName: string; const AValue: TDateTime): TDateTime;
begin
  TKWebResponse.Current.Items.SetProperty(Self, AName, AValue);
  Result := AValue;
end;

function TJSObject.GenerateAnonymousFunction(const AExpression: TJSExpression): TJSExpression;
begin
  Result := GenerateAnonymousFunction('', AExpression, '');
end;

function TJSObject.GenerateAnonymousFunction(const ABody: string): TJSExpression;
begin
  Result := GenerateAnonymousFunction('', ABody, '');
end;

initialization
  _JSFormatSettings := TFormatSettings.Create;
  _JSFormatSettings.DecimalSeparator := '.';

finalization

end.

