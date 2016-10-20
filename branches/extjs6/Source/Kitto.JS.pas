{ -------------------------------------------------------------------------------
  Copyright 2016 Ethea S.r.l.

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
  Based on code from ExtPascal
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
  , TypInfo
  , Rtti

  , EF.Tree
  , EF.Intf

  , {$IFNDEF WebServer}FCGIApp{$ELSE}IdExtHTTPServer{$ENDIF}
  , Kitto.JS.Types

  ;

type
  TJS = class
  private
  public
    /// <summary>
    /// Converts a string with param placeholders to a JavaScript string.
    /// Converts a string representing a regular expression to a JavaScript RegExp.
    /// Replaces " to ', #13#10 to <br/> and isolated #13 or #10 to <br/>.
    /// Surrounds the string with " and inserts %0..%9 placeholders.
    /// </summary>
    class function StrToJS(const AString: string; AUseBR: Boolean = False): string;

    /// <summary>
    /// Converts a Pascal enumerated type constant into a JS name, by removing
    /// the lowercase prefix and returning the rest, converted to lowercase.
    /// </summary>
    class function EnumToJSString(const ATypeInfo: PTypeInfo; const AValue: Integer): string;

    /// <summary>
    ///  Generates a padding style declaration with the provided data and returns
    ///  it as a string.
    /// </summary>
    class function GetPadding(const ATop: Integer; const ARight: Integer = 0; const ABottom: Integer = -1;
      const ALeft: Integer = 0; const ACSSUnit: TCSSUnit = cssPX; const AHeader: Boolean = True): string;

    /// <summary>
    ///  Generates a margins style declaration with the provided data and returns
    ///  it as a string.
    /// </summary>
    class function GetMargins(const ATop: Integer; const ARight: Integer = 0; const ABottom: Integer = -1;
      const ALeft: Integer = 0; const ACSSUnit: TCSSUnit = cssPX; const AHeader: Boolean = True): string;

    class function JSDateToDateTime(const AJSDate: string): TDateTime;

    class function RemoveLastJSTerminator(const AJSCode: string): string;

    class function DelphiDateTimeFormatToJSDateTimeFormat(const ADateTimeFormat: string): string;
    class function DelphiDateFormatToJSDateFormat(const ADateFormat: string): string;
    class function DelphiTimeFormatToJSTimeFormat(const ATimeFormat: string): string;

    class function WrapInAnonymousFunction(const AArgs, ABody: string; const AReturn: string = ''): string;
  end;

  TJSFormatter = class
  private
    FCurrentIndent: Integer;
    FFormattedText: string;
    FFormatSettings: TFormatSettings;
    function IndentStr: string; inline;
  public
    procedure AfterConstruction; override;
  public
    property FormatSettings: TFormatSettings read FFormatSettings;

    function Indent: TJSFormatter;
    function Outdent: TJSFormatter;

    function OpenObject: TJSFormatter;
    function CloseObject: TJSFormatter;
    function OpenArray: TJSFormatter;
    function CloseArray: TJSFormatter;
    function OpenRound: TJSFormatter;
    function CloseRound: TJSFormatter;
    function Add(const AString: string): TJSFormatter;
    function AddLine(const ALine: string): TJSFormatter;
    function AddIndent: TJSFormatter;
    function AddIndented(const AString: string): TJSFormatter;
    function AddIndentedLine(const ALine: string): TJSFormatter;
    function AddIndentedPairLine(const AName, AStrValue: string;
      const AQuoteValue: Boolean = True; const AAddComma: Boolean = True): TJSFormatter;
    function AddIndentedPair(const AName, AStrValue: string;
      const AQuoteValue: Boolean = True; const AAddComma: Boolean = True;
      const AConnector: string = ': '): TJSFormatter;
    // Adds empty line
    function SkipLine: TJSFormatter;
    function AddIndentedList(const ALines: TArray<string>): TJSFormatter;
    // Shortcut for OpenObject + AddLines + CloseObject
    function FormatObject(const ALines: TArray<string>): TJSFormatter;
    // Shortcut for OpenArray + AddLines + CloseArray
    function FormatArray(const ALines: TArray<string>): TJSFormatter;

    function DeleteTrailing(const AString: string): TJSFormatter;

    property FormattedText: string read FFormattedText;
  end;

  TJSSession = class;

  {$M+}
  TJSBase = class(TEFNoRefCountObject)
  private
    FJSSession: TJSSession;
    FOwner: TJSBase;
    FChildren: TObjectList<TJSBase>;
    FJSName: string;
    FDestroying: Boolean;
    FDestroyingChildren: Boolean;
    function GetJSSession(const AOwner: TJSBase): TJSSession; overload;
    function GetJSSession: TJSSession; overload;
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
    property JSSession: TJSSession read GetJSSession;

    function FindChildByJSName(const AJSName: string): TJSBase;
    procedure FreeAllChildren;
  end;
  {$M-}

  TJSObjectCatalog = class(TJSBase)
  end;

  TJSExpression = class;
  TJSObjectArray = class;
  TJSResponseItems = class;

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

  TJSMethodCall = class;
  TJSAjaxCall = class;
  TJSFunction = class;

  TJSObject = class(TJSBase)
  private
    // Assigned if the object was created with CreateInternal or CreateInline.
    { TODO : Maybe we could replace it with an extraction from JSName }
    FAttributeName: string;
    FJSName: string;
    FJSConfig: TJSValues;
    procedure FindMethod(const AMethod: TJSProcedure; out AMethodName, AObjectName: string);
    function GetDownloadJS(const AMethod: TJSProcedure): string;
  protected
    function ParamAsInteger(ParamName: string): Integer;
    function ParamAsDouble(ParamName: string): double;
    function ParamAsBoolean(ParamName: string): Boolean;
    function ParamAsString(ParamName: string): string;
    function ParamAsDateTime(ParamName: string): TDateTime;
    function ParamAsObject(ParamName: string): TJSObject;
    procedure CreateJSName;
    function GetObjectNamePrefix: string; virtual;
    procedure InitDefaults; virtual;
    procedure HandleEvent(const AEventName: string); virtual;
    function CreateConfigArray(const AAttributeName: string): TJSObjectArray;
    procedure DependsUpon(const AObject: TJSObject);
  public
    constructor Create(const AOwner: TJSBase); override;
    constructor CreateInternal(const AOwner: TJSBase; const AAttributeName: string);
    constructor CreateInline(const AOwner: TJSBase);
    constructor CreateSingleton(const AOwner: TJSBase; const AAttributeName: string);
    constructor CreateAndAddToArray(const AArray: TJSObjectArray);
    constructor CreateInlineAndAddToArray(const AArray: TJSObjectArray);
    destructor Destroy; override;

    procedure Delete;

    function IsInternal: Boolean;
    function IsInline: Boolean;

    property JSName: string read FJSName;
    class function JSClassName: string; virtual;
    property JSConfig: TJSValues read FJSConfig;
    property AttributeName: string read FAttributeName;

    function JSArray(const AJSON: string; const ASquareBrackets: Boolean = True): TJSObjectArray;
    function JSObject(const AJSON: string; const AObjectConstructor: string = ''; const ACurlyBrackets: Boolean = True): TJSObject;
    function JSExpressionFromCodeBlock(const ACode: string): TJSExpression;
    function GetJSCode(const AMethod: TProc; const ASilent: Boolean = False): string;
    function JSExpressionFromExpr(const AExpr: string; const AValues: array of TJSExpression): TJSExpression;

    function GenerateAnonymousFunction(const AArgs, ABody: string; const AReturn: string = ''): TJSExpression; overload;
    function GenerateAnonymousFunction(const ABody: string): TJSExpression; overload;
    function GenerateAnonymousFunction(const AArgs: string; const AExpression: TJSExpression; const AReturn: string = ''): TJSExpression; overload;
    function GenerateAnonymousFunction(const AExpression: TJSExpression): TJSExpression; overload;

    procedure Download(Method: TJSProcedure); overload;
    function MethodURI(const AMethod: TJSProcedure): string; overload;
    function MethodURI(const AMethodName: string): string; overload;

    function CharsToPixels(const AChars: Integer; const AOffset: Integer = 0): TJSExpression;
    function LinesToPixels(const ALines: Integer): TJSExpression;

    function SetConfigItem(const AName, AMethodName: string; const AValue: string): string; overload;
    function SetConfigItem(const AName, AMethodName: string; const AValue: Boolean): Boolean; overload;
    function SetConfigItem(const AName, AMethodName: string; const AValue: Integer): Integer; overload;
    function SetConfigItem(const AName, AMethodName: string; const AValue: TDateTime): TDateTime; overload;
    function SetConfigItem(const AName, AMethodName: string; const AValue: TJSObject): TJSObject; overload;
    function SetConfigItem(const AName, AMethodName: string; const AValue: TJSExpression): TJSExpression; overload;

    function SetConfigItem(const AName, AValue: string): string; overload;
    function SetConfigItem(const AName: string; const AValue: TJSObject): TJSObject; overload;
    function SetConfigItem(const AName: string; const AValue: Integer): Integer; overload;
    function SetConfigItem(const AName: string; const AValue: Boolean): Boolean; overload;
    function SetConfigItem(const AName: string; const AValue: Double): Double; overload;
    function SetConfigItem(const AName: string; const AValue: TJSExpression): TJSExpression; overload;

    function SetConfigItemOrProperty(const AName, AValue: string): string; overload;
    function SetConfigItemOrProperty(const AName: string; const AValue: Boolean): Boolean; overload;

    function SetProperty(const AName: string; const AValue: Integer): Integer; overload;
    function SetProperty(const AName, AValue: string): string; overload;
    function SetProperty(const AName: string; const AValue: TJSExpression): TJSExpression; overload;
    function SetProperty(const AName: string; const AValue: TJSObject): TJSObject; overload;
    function SetProperty(const AName: string; const AValue: Boolean): Boolean; overload;
    function SetProperty(const AName: string; const AValue: TDateTime): TDateTime; overload;

    function CallMethod(const AName: string): TJSMethodCall; overload;

    function AjaxCallMethod(const AName: string = ''): TJSAjaxCall; overload;
  end;

  TJSObjectClass = class of TJSObject;

  TJSExpression = class(TJSBase)
  private
    FText: string;
    FExtracted: Boolean;
    procedure SetText(const AValue: string);
  strict protected
    function InternalExtractText: string; virtual;
  public
    property Text: string read FText write SetText;

    function ExtractText: string;
  end;

  TJSFunction = class(TJSExpression)
  strict protected
    function InternalExtractText: string; override;
  end;

  /// <summary>
  /// Represents the server side of a user client session.
  /// Holds all objects pertaining to the user session.
  /// </summary>
  TJSSession = class(TWebSession)
  private
    FObjectCatalog: TJSObjectCatalog;
    FObjectSequences: TDictionary<string, Cardinal>;
    FStyles, FLibraries, FLanguage: string;
    FResponseItemsStack: TStack<TJSResponseItems>;
    FSingletons: TDictionary<string, TJSObject>;
    FMobileBrowserDetectionDone: Boolean;
    FIsMobileApple: Boolean;
    FGlobal: TJSObject;
    function GetStyleTag: string;
    function GetResponseItems: TJSResponseItems;
    function GetGlobal: TJSObject;
  protected
    function BeforeHandleRequest: Boolean; override;
    procedure AfterHandleRequest; override;
    procedure OnError(const AMessage, AMethodName, AParams: string); override;
    function GetNextJSName(const AObjectType: string): string;
    function GetUrlHandlerObject: TObject; override;
    function GetMainPageTemplate: string; virtual;
    procedure SetLanguage(const AValue: string); virtual;
    function GetViewportContent: string; virtual;
    function GetManifestFileName: string; virtual;
    function GetCustomJS: string; virtual;
  public
    Theme: string;
    // Sets or gets Ext JS installed theme, default '' that is Ext Blue theme
    ExtPath: string;
    // Installation path of Ext JS framework, below the your Web server document root. Default value is '/ext'
    ExtBuild: string;
    procedure AfterConstruction; override;
    destructor Destroy; override;
    procedure Alert(const AMessage: string); override;
    property Language: string read FLanguage write SetLanguage;
    // Actual language for this session, reads HTTP_ACCEPT_LANGUAGE header
    procedure InitDefaultValues; override;
    procedure SetStyle(const AStyle: string = '');
    procedure SetLibrary(pLibrary: string = ''; CSS: Boolean = False; HasDebug: Boolean = False;
      DisableExistenceCheck: Boolean = False);
    procedure SetCSS(pCSS: string; Check: Boolean = True);
    procedure ErrorMessage(const AMessage: string; const AAction: string = '');
    procedure Refresh; override;

    property ResponseItems: TJSResponseItems read GetResponseItems;
    function HasResponseItems: Boolean;
    function BranchResponseItems: TJSResponseItems;
    procedure UnbranchResponseItems(const AResponseItems: TJSResponseItems; const AConsolidate: Boolean = True);

    function GetSingleton<T: TJSObject>(const AName: string): T;
    function IsMobileApple: Boolean;
    property ObjectCatalog: TJSObjectCatalog read FObjectCatalog;
    property Global: TJSObject read GetGlobal;
  published
    procedure HandleEvent; virtual;
  end;

  TJSObjectArray = class(TJSObject)
  private
    FObjects: TObjectList<TJSObject>;
    function GetObject(I: Integer): TJSObject;
    function GetCount: Integer;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;

    property Objects[I: Integer]: TJSObject read GetObject; default;
    function Add(const AObject: TJSObject): Integer;
    function AddInternal(const AObject: TJSObject): Integer;
    function Remove(const AObject: TJSObject): Integer;
    function IndexOf(const AObject: TJSObject): Integer;
    property Count: Integer read GetCount;
  end;

  TJSResponseItem = class;
  TJSCreateObject = class;
  TJSGetProperty = class;
  TJSSetProperty = class;
  TJSCode = class;

  TJSResponseItems = class(TJSBase)
  private
    FList: TObjectList<TJSResponseItem>;
    FEmittedItems: TList<TJSResponseItem>;
    procedure SortByDependency;
    function GetCount: Integer;
    function GetItem(I: Integer): TJSResponseItem;
    procedure DoSetProperty(const AObject: TJSObject; const ASetValueProc: TProc<TJSSetProperty>);
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
  public
    // Create an object.
    procedure CreateObject(const AObject: TJSObject);

    // Use this method to have object creation statements in the correct order when emitting the response.
    procedure AddObjectDependency(const ADependentObject, ADependedUponObject: TJSObject);

    function CallMethod(const AObject: TJSObject; const AMethodName: string): TJSMethodCall;

    function AjaxCallMethod(const AObject: TJSObject; const AMethodName: string = ''): TJSAjaxCall;

    function GetProperty(const AObject: TJSObject; const APropertyName: string): TJSGetProperty;

{ TODO : replace with single fluent call }
    procedure SetProperty(const AObject: TJSObject; const AName, AValue: string); overload;
    procedure SetProperty(const AObject: TJSObject; const AName: string; const AValue: TJSObject); overload;
    procedure SetProperty(const AObject: TJSObject; const AName: string; const AValue: Boolean); overload;
    procedure SetProperty(const AObject: TJSObject; const AName: string; const AValue: Integer); overload;
    procedure SetProperty(const AObject: TJSObject; const AName: string; const AValue: TDateTime); overload;
    procedure SetProperty(const AObject: TJSObject; const AName: string; const AValue: TJSExpression); overload;

{ TODO : replace with single fluent call }
    function ExecuteJSCode(const AJSCode: string): TJSCode; overload;
    function ExecuteJSCode(const AObject: TJSObject; const AJSCode: string): TJSCode; overload;
    function ExecuteJSCode(const AObject: TJSObject; const AJSCode: string;
      const AAdditionalDependencies: array of TJSObject): TJSCode; overload;

    procedure AddJSON(const AJSON: string);

    procedure AddHTML(const AHTML: string);

    function AsFormattedString: string;
    function Consume: string;

    function FindObjectCreateItem(const AObject: TJSObject): TJSCreateObject;

    property Items[I: Integer]: TJSResponseItem read GetItem; default;
    property Count: Integer read GetCount;
    procedure Clear;
  end;

  TJSResponseItem = class(TJSBase)
  private
    FSender: TJSObject;
    FDependencies: TList<TJSResponseItem>;
    FEmitted: Boolean;
    FCreationDateTime: TDateTime;
    function GetDependencyCount: Integer;
    function GetDependency(I: Integer): TJSResponseItem;
    function AllDependenciesEmitted(const AEmittedItems: TList<TJSResponseItem>): Boolean;
    function GetFormattedCode: string;
  strict protected
    FRoot: TJSResponseItems;
    procedure ChangeSender(const ASender: TJSObject);
  public
    constructor Create(const ASender: TJSObject; const ARoot: TJSResponseItems); reintroduce; virtual;
    procedure AfterConstruction; override;
    destructor Destroy; override;
    property Sender: TJSObject read FSender;
    procedure AddDependency(const AItem: TJSResponseItem);
    procedure RemoveDependency(const AItem: TJSResponseItem);
    function GetDependencies: TArray<TJSResponseItem>;
    property DependencyCount: Integer read GetDependencyCount;
    property Dependencies[I: Integer]: TJSResponseItem read GetDependency;
    function DependsOn(const AItem: TJSResponseItem): Boolean;
    function IsCode: Boolean; virtual;

    procedure Emit(const AEmittedItems: TList<TJSResponseItem>);
    procedure UnEmit;

    procedure FormatTo(const AFormatter: TJSFormatter); virtual;
    function AsFormattedText: string;
  end;

  TJSNamedCreateObject = record
    Name: string;
    CreateObject: TJSCreateObject;
  end;

  TJSCreateObjectParams = record
    JSConfig: TJSValues;
    IsInline: Boolean;
    IsInternal: Boolean;
    JSName: string;
    JSClassName: string;
  end;

  TJSCreateObject = class(TJSResponseItem)
  private
    FCreateParams: TJSCreateObjectParams;
    FItems: TList<TJSNamedCreateObject>;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;

    procedure FormatTo(const AFormatter: TJSFormatter); override;

    function IsCode: Boolean; override;

    procedure CreateInternalObject(const AAttributeName: string; const ASender: TJSObject);

    function FindObjectCreateItem(const ASender: TJSObject): TJSCreateObject;

    // Allows the creation statement to be emitted even if the sender is being
    // destroyed. Takes hold of the sender's config object. As such, this method
    // should be only called by the sender when it's being destroyed.
    procedure UnlinkFromSender(const AConfig: TJSValues);
  end;

  // Base class for response items that can generate a TJSExpression.
  TJSExpressionResponseItem = class(TJSResponseItem)
  private
    FExpression: TJSExpression;
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
  public
    procedure AfterConstruction; override;
  public
    procedure FormatTo(const AFormatter: TJSFormatter); override;
    property CallName: string read FCallName write FCallName;

    function AddParam(const AValue: string): TJSMethodCall; overload;
    function AddParam(const AValue: Boolean): TJSMethodCall; overload;
    function AddParam(const AValue: Integer): TJSMethodCall; overload;
    function AddParam(const AValue: TJSObject): TJSMethodCall; overload;
    function AddParam(const AValue: TDateTime): TJSMethodCall; overload;
    function AddParam(const AValue: TJSExpression): TJSMethodCall; overload;

    property Params: TJSValues read FParams;
  end;

  TJSAjaxCall = class(TJSMethodCall)
  private
    FHttpMethod: string;
    FPostData: string;
    procedure AddParams(const AFormatter: TJSFormatter);
  public
    procedure AfterConstruction; override;
  public
    procedure FormatTo(const AFormatter: TJSFormatter); override;

    function SetMethod(const AMethod: TJSProcedure): TJSAjaxCall;
    function Get: TJSAjaxCall;
    function Post(const AData: string): TJSAjaxCall;
    function Event: TJSAjaxCall;

    function AddRawParam(const AName, AValue: string): TJSAjaxCall; overload;
    function AddParam(const AName, AValue: string): TJSAjaxCall; overload;
    function AddParam(const AName: string; const AValue: Boolean): TJSAjaxCall; overload;
    function AddParam(const AName: string; const AValue: Integer): TJSAjaxCall; overload;
    function AddParam(const AName: string; const AValue: TJSObject): TJSAjaxCall; overload;
    function AddParam(const AName: string; const AValue: TDateTime): TJSAjaxCall; overload;
    function AddParam(const AName: string; const AValue: TJSExpression): TJSAjaxCall; overload;
  end;

  TJSGetProperty = class(TJSExpressionResponseItem)
  private
    FPropertyName: string;
  public
    property PropertyName: string read FPropertyName write FPropertyName;
    procedure FormatTo(const AFormatter: TJSFormatter); override;
  end;

  TJSSetProperty = class(TJSResponseItem)
  private
    FNameValue: TJSValues;
  public
    procedure AfterConstruction; override;
  public
    procedure FormatTo(const AFormatter: TJSFormatter); override;
    property NameValue: TJSValues read FNameValue;
  end;

  TJSTextBase = class(TJSExpressionResponseItem)
  protected
    FText: string;
  public
    property Text: string read FText write FText;
    procedure FormatTo(const AFormatter: TJSFormatter); override;
  end;

  TJSCode = class(TJSTextBase)
  public
    property JSCode: string read FText write FText;
    procedure FormatTo(const AFormatter: TJSFormatter); override;
  end;

  TJSON = class(TJSTextBase)
  public
    property JSON: string read FText write FText;
  end;

  THTML = class(TJSTextBase)
  public
    property HTML: string read FText write FText;
  end;

function GetSession: TJSSession;

implementation

uses
  StrUtils
  , DateUtils
  , Math
  , Types
  , Character
  , Ext.Util // for ExtUtilTextMetrics. Switch to pure JS?
  , EF.StrUtils
  ;

var
  _JSFormatSettings: TFormatSettings;

function GetSession: TJSSession;
begin
  Result := TJSSession(_CurrentWebSession);
end;

{ TJSResponseItem }

procedure TJSResponseItem.AddDependency(const AItem: TJSResponseItem);
begin
  if Assigned(AItem) and (AItem <> Self) and not FDependencies.Contains(AItem) then
    FDependencies.Add(AItem);
end;

procedure TJSResponseItem.AfterConstruction;
begin
  inherited;
  FDependencies := TList<TJSResponseItem>.Create;
  FEmitted := False;
  FCreationDateTime := Now;
end;

procedure TJSResponseItem.ChangeSender(const ASender: TJSObject);
begin
  FSender := ASender;
end;

constructor TJSResponseItem.Create(const ASender: TJSObject; const ARoot: TJSResponseItems);
begin
  inherited Create(nil);
  FSender := ASender;
  FRoot := ARoot;
end;

function TJSResponseItem.DependsOn(const AItem: TJSResponseItem): Boolean;
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

function TJSResponseItem.GetDependencies: TArray<TJSResponseItem>;
begin
  Result := FDependencies.ToArray;
end;

function TJSResponseItem.GetDependency(I: Integer): TJSResponseItem;
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

function TJSResponseItem.IsCode: Boolean;
begin
  Result := True;
end;

procedure TJSResponseItem.RemoveDependency(const AItem: TJSResponseItem);
begin
  Assert(FDependencies.Remove(AItem) >= 0);
end;

procedure TJSResponseItem.UnEmit;
begin
  FEmitted := False;
end;

{ TJSCreateObject }

procedure TJSCreateObject.AfterConstruction;
begin
  inherited;
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

function TJSCreateObject.FindObjectCreateItem(const ASender: TJSObject): TJSCreateObject;
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

procedure TJSCreateObject.UnlinkFromSender(const AConfig: TJSValues);
begin
  Assert(Assigned(Sender));
  Assert(Assigned(AConfig));

  AConfig.Owner := Self;
  FCreateParams.JSConfig := AConfig;
  FCreateParams.IsInline := Sender.IsInline;
  FCreateParams.IsInternal := Sender.IsInternal;
  FCreateParams.JSName := Sender.JSName;
  FCreateParams.JSClassName := Sender.JSClassName;

  ChangeSender(nil);
end;

procedure TJSCreateObject.CreateInternalObject(const AAttributeName: string; const ASender: TJSObject);
var
  LItem: TJSNamedCreateObject;
begin
  LItem.Name := AAttributeName;
  LItem.CreateObject := TJSCreateObject.Create(ASender, FRoot);
  FItems.Add(LItem);
end;

procedure TJSCreateObject.FormatTo(const AFormatter: TJSFormatter);
var
  LJSName: string;
  LConstructionAdded: Boolean;
begin
  inherited;
  LConstructionAdded := False;
  if Assigned(Sender) then
  begin
    if not Sender.IsInline and not Sender.IsInternal then
    begin
      LJSName := Sender.JSName;
      AFormatter.AddIndentedLine(LJSName + ' = new ' + Sender.JSClassName + '(');
      AFormatter.Indent.AddIndent;
      LConstructionAdded := True;
    end;
  end
  else
  begin
    if not FCreateParams.IsInline and not FCreateParams.IsInternal then
    begin
      LJSName := FCreateParams.JSName;
      AFormatter.AddIndentedLine(LJSName + ' = new ' + FCreateParams.JSClassName + '(');
      AFormatter.Indent.AddIndent;
      LConstructionAdded := True;
    end;
  end;

  { TODO : what if it's empty? }
  AFormatter.OpenObject;
  if Assigned(Sender) then
    Sender.JSConfig.FormatTo(AFormatter)
  else
    FCreateParams.JSConfig.FormatTo(AFormatter);
  AFormatter.CloseObject;
  if LConstructionAdded then
  begin
    AFormatter.SkipLine.Outdent;
    AFormatter.AddIndentedLine(');');
    AFormatter.AddIndentedLine(LJSName + '.nm = "' + LJSName + '";');
  end;
end;

{ TExtJSCode }

procedure TJSCode.FormatTo(const AFormatter: TJSFormatter);
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

procedure TJSResponseItems.AddObjectDependency(const ADependentObject, ADependedUponObject: TJSObject);
var
  LDependentObjectCreateItem: TJSCreateObject;
begin
  LDependentObjectCreateItem := FindObjectCreateItem(ADependentObject);
  if Assigned(LDependentObjectCreateItem) then
    LDependentObjectCreateItem.AddDependency(FindObjectCreateItem(ADependedUponObject));
end;

procedure TJSResponseItems.AfterConstruction;
begin
  inherited;
  FList := TObjectList<TJSResponseItem>.Create;
  FEmittedItems := TList<TJSResponseItem>.Create;
end;

function TJSResponseItems.AjaxCallMethod(const AObject: TJSObject; const AMethodName: string): TJSAjaxCall;
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

procedure TJSResponseItems.CreateObject(const AObject: TJSObject);
begin
  Assert(Assigned(AObject));

  FList.Add(TJSCreateObject.Create(AObject, Self));
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

function TJSResponseItems.ExecuteJSCode(const AObject: TJSObject; const AJSCode: string): TJSCode;
begin
  Result := ExecuteJSCode(AObject, AJSCode, []);
end;

function TJSResponseItems.ExecuteJSCode(const AObject: TJSObject; const AJSCode: string;
  const AAdditionalDependencies: array of TJSObject): TJSCode;
var
  I: Integer;
begin
  if AJSCode <> '' then
  begin
    Result := TJSCode.Create(AObject, Self);
    try
      Result.AddDependency(FindObjectCreateItem(AObject));
      Result.JSCode := AJSCode;
      for I := Low(AAdditionalDependencies) to High(AAdditionalDependencies) do
        Result.AddDependency(FindObjectCreateItem(AAdditionalDependencies[I]));
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

  function GetDependentNodes(const AItem: TJSResponseItem): TArray<TJSResponseItem>;
  var
    LItem: TJSResponseItem;
  begin
    SetLength(Result, 0);
    for LItem in FList do
      if LItem.DependsOn(AItem) then
        Result := Result + [LItem];
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

    function GetItemDescription(const AItem: TJSResponseItem): string;
    begin
      Result := AItem.Sender.JSName + '-' + AItem.ClassName;
    end;

  begin
    Result := '';
    for LItem in AList do
    begin
      if LItem.DependencyCount > 0 then
      begin
        Result := Result + GetItemDescription(LItem) + '  DEPENDS ON  ';
        for I := 0 to LItem.DependencyCount - 1 do
          Result := Result + ' ' + GetItemDescription(LItem.Dependencies[I]);
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
        LDependents := GetDependentNodes(LItem);
        for LDependent in LDependents do
        begin
          LDependent.RemoveDependency(LItem);
          if LDependent.DependencyCount = 0 then
            LTopLevelNodes.Enqueue(LDependent);
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

function TJSResponseItems.CallMethod(const AObject: TJSObject; const AMethodName: string): TJSMethodCall;
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

function TJSResponseItems.FindObjectCreateItem(const AObject: TJSObject): TJSCreateObject;
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
  end;
end;

function TJSResponseItems.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TJSResponseItems.GetItem(I: Integer): TJSResponseItem;
begin
  Result := FList[I];
end;

function TJSResponseItems.GetProperty(const AObject: TJSObject; const APropertyName: string): TJSGetProperty;
begin
  Result := TJSGetProperty.Create(AObject, Self);
  try
    Result.PropertyName := APropertyName;
    Result.AddDependency(FindObjectCreateItem(AObject));
    FList.Add(Result);
  except
    FreeAndNil(Result);
  end;
end;

procedure TJSResponseItems.SetProperty(const AObject: TJSObject;
  const AName: string; const AValue: TJSObject);
begin
  DoSetProperty(AObject,
    procedure (AItem: TJSSetProperty)
    begin
      AItem.NameValue.Values.SetObject(AName, AValue);
      AItem.AddDependency(FindObjectCreateItem(AValue));
    end);
end;

procedure TJSResponseItems.DoSetProperty(const AObject: TJSObject; const ASetValueProc: TProc<TJSSetProperty>);
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

procedure TJSResponseItems.SetProperty(const AObject: TJSObject; const AName, AValue: string);
begin
  DoSetProperty(AObject,
    procedure (AItem: TJSSetProperty)
    begin
      AItem.NameValue.Values.SetString(AName, AValue);
    end);
end;

procedure TJSResponseItems.SetProperty(const AObject: TJSObject;
  const AName: string; const AValue: TJSExpression);
begin
  DoSetProperty(AObject,
    procedure (AItem: TJSSetProperty)
    begin
      AItem.NameValue.Values.SetObject(AName, AValue);
    end);
end;

procedure TJSResponseItems.SetProperty(const AObject: TJSObject;
  const AName: string; const AValue: TDateTime);
begin
  DoSetProperty(AObject,
    procedure (AItem: TJSSetProperty)
    begin
      AItem.NameValue.Values.SetDateTime(AName, AValue);
    end);
end;

procedure TJSResponseItems.SetProperty(const AObject: TJSObject;
  const AName: string; const AValue: Boolean);
begin
  DoSetProperty(AObject,
    procedure (AItem: TJSSetProperty)
    begin
      AItem.NameValue.Values.SetBoolean(AName, AValue);
    end);
end;

procedure TJSResponseItems.SetProperty(const AObject: TJSObject;
  const AName: string; const AValue: Integer);
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
    FParams.SetRawValue(FParams.Values.ChildCount.ToString, AValue.ExtractText)
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

function TJSMethodCall.AddParam(const AValue: TJSObject): TJSMethodCall;
begin
  if Assigned(AValue) then
    FParams.Values.SetObject(FParams.Values.ChildCount.ToString, AValue)
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

procedure TJSMethodCall.FormatTo(const AFormatter: TJSFormatter);
begin
  inherited;
  if not IsExpressionExtracted then
  begin
    Assert(FCallName <> '');
    Assert(Assigned(Sender));

    AFormatter.AddIndented(Sender.JSName + '.' + FCallName);
    AFormatter.OpenRound;
    Params.FormatTo(AFormatter);
    AFormatter.CloseRound.AddLine(';');
  end;
end;

{ TExtSetProperty }

procedure TJSSetProperty.AfterConstruction;
begin
  inherited;
  FNameValue := TJSValues.Create(Self);
  FNameValue.NameValueConnector := ' = ';
end;

procedure TJSSetProperty.FormatTo(const AFormatter: TJSFormatter);
begin
  Assert(Assigned(Sender));
  Assert(FNameValue.Values.ChildCount = 1);
  inherited;

  NameValue.FormatTo(AFormatter);
  AFormatter.AddLine(';');
end;

{ TJSGetProperty }

procedure TJSGetProperty.FormatTo(const AFormatter: TJSFormatter);
begin
  Assert(Assigned(Sender));
  Assert(FPropertyName <> '');

  inherited;
  if not IsExpressionExtracted then
    AFormatter.AddIndentedLine(Sender.JSName + '.' + FPropertyName + ';');
end;

{ TJSSession }

procedure TJSSession.SetLanguage(const AValue: string);
begin
  FLanguage := AValue;
end;

procedure TJSSession.SetLibrary(pLibrary: string = ''; CSS: Boolean = False; HasDebug: Boolean = False;
  DisableExistenceCheck: Boolean = False);
var
  Root: string;
begin
  pLibrary := pLibrary.Replace('{ext}', ExtPath);
  if pos(pLibrary + '.js', FLibraries) = 0 then
    if pLibrary = '' then
      FLibraries := '' // Clear FLibraries
    else
    begin
      if DisableExistenceCheck then
        Root := ''
      else
        Root := RequestHeader['DOCUMENT_ROOT'];
      if (Root = '') or ((Root <> '') and FileExists(Root + pLibrary + '.js')) then
      begin
        FLibraries := FLibraries + '<script src="' + pLibrary{$IFDEF DEBUGJS} + IfThen(HasDebug, '-debug', ''){$ENDIF} +
          '.js"></script>'^M^J;
        if CSS then
        begin
          if not DisableExistenceCheck and not FileExists(Root + pLibrary + '.css') then
            // Assume in /css like ux
            pLibrary := ExtractFilePath(pLibrary) + 'css/' + ExtractFileName(pLibrary);
          FLibraries := FLibraries + '<link rel=stylesheet href="' + pLibrary + '.css" />';
        end;
      end
      else
        raise Exception.Create('Library: ' + Root + pLibrary + '.js not found');
    end;
end;

{
  Adds/Removes an user CSS (cascade style sheet) to be used in current response.
  If the WebServer is Apache tests if the CSS file exists.
  Repeated CSS's are ignored.
  @param pCSS CSS file name without extension (.css), but with Path based on Web server document root.
  @param Check Checks if the CSS file exists, default is true.
  If pCSS is '' then all user CSS AND JS libraries to this session will be removed from response.
}
procedure TJSSession.SetCSS(pCSS: string; Check: Boolean = True);
var
  Root: string;
begin
  pCSS := pCSS.Replace('{ext}', ExtPath);
  if pos(pCSS + '.css', FLibraries) = 0 then
    if pCSS = '' then
      FLibraries := '' // Clear FLibraries
    else
    begin
      Root := RequestHeader['DOCUMENT_ROOT'];
      if Check and (Root <> '') and not FileExists(Root + pCSS + '.css') then
        raise Exception.Create('Stylesheet: ' + Root + pCSS + '.css not found')
      else
        FLibraries := FLibraries + '<link rel=stylesheet href="' + pCSS + '.css" />';
    end;
end;

procedure TJSSession.SetStyle(const AStyle: string);
begin
  if Pos(AStyle, FStyles) = 0 then
    if AStyle = '' then
      FStyles := ''
    else
      FStyles := FStyles + AStyle;
end;

procedure TJSSession.UnbranchResponseItems(const AResponseItems: TJSResponseItems; const AConsolidate: Boolean);
var
  LSender: TJSObject;
  LBranch: TJSResponseItems;
  LInitialCount: Integer;
begin
  Assert(Assigned(AResponseItems));
  Assert(FResponseItemsStack.Count > 1);
  Assert(AResponseItems = FResponseItemsStack.Peek);

  LInitialCount := FResponseItemsStack.Count;
  LBranch := FResponseItemsStack.Pop;
  if AConsolidate then
  begin
    if LBranch.Count > 0 then
    begin
      LSender := LBranch.Items[0].Sender;
      FResponseItemsStack.Peek.ExecuteJSCode(LSender, LBranch.Consume);
    end;
  end;
  FreeAndNil(LBranch);

  Assert(FResponseItemsStack.Count = LInitialCount - 1);
end;

// Returns all styles in use in current response
function TJSSession.GetStyleTag: string;
begin
  if FStyles = '' then
    Result := ''
  else
    Result := '<style>' + FStyles + '</style>';
end;

// Returns a object which will be used to handle the page method. We will call it's published method based on PathInfo.
function TJSSession.GetUrlHandlerObject: TObject;
var
  LJSName: string;
begin
  LJSName := Query['Object'];
  if (LJSName = '') or (Query['Event'] <> '') then
    Result := inherited GetUrlHandlerObject
  else
    Result := ObjectCatalog.FindChildByJSName(LJSName);
end;

function TJSSession.GetViewportContent: string;
begin
  Result := '';
end;

procedure TJSSession.ErrorMessage(const AMessage: string; const AAction: string);
begin
  ResponseItems.ExecuteJSCode('Ext.Msg.show({title:"Error",msg:' + TJS.StrToJS(AMessage, True) +
    ',icon:Ext.Msg.ERROR,buttons:Ext.Msg.OK' + IfThen(AAction = '', '', ',fn:function(){' + AAction + '}') + '});');
end;

procedure TJSSession.OnError(const AMessage, AMethodName, AParams: string);
begin
  ResponseItems.Clear;
{$IFDEF DEBUG}
  ErrorMessage(AMessage + '<br/>Method: ' + IfThen(AMethodName = '', 'Home', AMethodName) + IfThen(AParams = '', '',
    '<br/>Params:<br/>' + AnsiReplaceStr(AParams, '&', '<br/>')));
{$ELSE}
  ErrorMessage(AMessage);
{$ENDIF}
end;

procedure TJSSession.Alert(const AMessage: string);
begin
  ErrorMessage(AMessage);
end;

procedure TJSSession.Refresh;
begin
  inherited;
  ObjectCatalog.FreeAllChildren;
  ResponseItems.Clear;
  FObjectSequences.Clear;
  FSingletons.Clear;
end;

function TJSObject.MethodURI(const AMethod: TJSProcedure): string;
var
  LMethodName: string;
  LObjectName: string;
begin
  FindMethod(AMethod, LMethodName, LObjectName);
  Result := JSSession.MethodURI(LMethodName);
  if LObjectName <> '' then
  begin
    if Pos('?', Result) <> 0 then
      Result := Result + '&Object=' + LObjectName
    else
      Result := Result + '?Object=' + LObjectName;
  end;
end;

function TJSObject.MethodURI(const AMethodName: string): string;
begin
  Result := JSSession.MethodURI(AMethodName);
end;

function TJSObject.GetObjectNamePrefix: string;
begin
  Result := 'o';
end;

{
  Does tasks related to the Request that occur before the method call invoked by Browser (PATH-INFO)
  1. Detects the browser language.
  2. If that language has corresponding JS resource file in framework uses it, for example: '/ext/source/locale/ext-lang-?????.js',
  3. Else uses the default language (English).
  4. Identify the browser.
  5. Tests if is an AJAX request.
  6. Tests if cookies are enabled.
  @return False if Cookies are disable or if is Ajax executing the first thread request else returns true.
}
function TJSSession.BeforeHandleRequest: Boolean;
var
  I: Integer;
begin
  Result := True;
  if FLanguage = '' then
  begin // Set language
    FLanguage := RequestHeader['HTTP_ACCEPT_LANGUAGE'];
    I := pos('-', FLanguage);
    if I <> 0 then
    begin
      FLanguage := Copy(FLanguage, I - 2, 2) + '_' + Uppercase(Copy(FLanguage, I + 1, 2));
      if not FileExists(RequestHeader['DOCUMENT_ROOT'] + ExtPath + '/build/classic/locale/locale-' + FLanguage + '.js')
      then
        FLanguage := Copy(FLanguage, 1, 2)
    end;
  end;
  IsAjax := (RequestHeader['HTTP_X_REQUESTED_WITH'] = 'XMLHttpRequest') or IsUpload;
  if IsAjax then
  begin
    if SessionCookie = '' then
    begin
      ErrorMessage('This web application requires Cookies enabled to AJAX works.');
      Result := False;
    end
    else if NewThread or RequiresReload then
    begin
      ErrorMessage('Session expired or lost.<br/>A new session will be created now.', 'window.location.reload()');
      RequiresReload := True;
      Result := False;
    end
  end
  else
    RequiresReload := False;
end;

function TJSSession.GetGlobal: TJSObject;
begin
  if not Assigned(FGlobal) then
    FGlobal := TJSObject.CreateInline(ObjectCatalog);
  Result := FGlobal;
end;

function TJSSession.BranchResponseItems: TJSResponseItems;
begin
  Result := TJSResponseItems.Create(ObjectCatalog);
  FResponseItemsStack.Push(Result);

  Assert(FResponseItemsStack.Count > 0);
end;

destructor TJSSession.Destroy;
begin
  Assert(FResponseItemsStack.Count = 1);
  FResponseItemsStack.Pop.Free;
  FreeAndNil(FResponseItemsStack);
  FreeAndNil(FObjectSequences);
  FreeAndNil(FSingletons);
  FreeAndNil(FGlobal);
  FreeAndNil(FObjectCatalog);
  inherited;
end;

procedure TJSSession.InitDefaultValues;
begin
  inherited;
  ExtPath := '/ext6';
  ExtBuild := 'ext-all';
  Charset := 'utf-8'; // 'iso-8859-1'
  UpLoadPath := '/uploads';
end;

function TJSSession.IsMobileApple: Boolean;
var
  LUserAgent: string;
begin
  if not FMobileBrowserDetectionDone then
  begin
    LUserAgent := RequestHeader['HTTP_USER_AGENT'];
    FIsMobileApple := LUserAgent.Contains('iPhone') or LUserAgent.Contains('iPad');
    FMobileBrowserDetectionDone := True;
  end;
  Result := FIsMobileApple;
end;

// Calls events using Delphi style
procedure TJSSession.HandleEvent;
var
  LObject: TJSObject;
begin
  if Query['Event'] <> '' then
  begin
    LObject := ObjectCatalog.FindChildByJSName(Query['Object']) as TJSObject;
    if not Assigned(LObject) then
      OnError('Object not found in session list. It could be timed out, refresh page and try again', 'HandleEvent', '')
    else
      LObject.HandleEvent(Query['Event']);
  end;
end;

function TJSSession.HasResponseItems: Boolean;
begin
  Result := Assigned(FResponseItemsStack);
end;

procedure TJSSession.AfterConstruction;
begin
  inherited;
  FResponseItemsStack := TStack<TJSResponseItems>.Create;
  FResponseItemsStack.Push(TJSResponseItems.Create(ObjectCatalog));

  FObjectSequences := TDictionary<string, Cardinal>.Create;
  FSingletons := TDictionary<string, TJSObject>.Create;

  FObjectCatalog := TJSObjectCatalog.Create(nil);
  FObjectCatalog.FJSSession := Self;
end;

function TJSSession.GetCustomJS: string;
begin
  Result := '';
end;

function TJSSession.GetMainPageTemplate: string;
begin
  Result := '<%HTMLDeclaration%>' + sLineBreak + '<head>' + sLineBreak + '  <title><%ApplicationTitle%></title>' +
    sLineBreak + '  <%ApplicationIconLink%>' + sLineBreak + '  <%AppleIconLink%>' + sLineBreak +
    '  <meta http-equiv="content-type" content="charset=<%CharSet%>" />' + sLineBreak +
    '  <meta name="viewport=" content="<%ViewportContent%>" />' + sLineBreak +
    '  <meta name="mobile-web-app-capable" content="yes" />' + sLineBreak +
    '  <meta name="apple-mobile-web-app-capable" content="yes" />' + sLineBreak +
    '  <link rel=stylesheet href="<%ExtPath%>/resources/css/<%ExtBuild%>.css" />' + sLineBreak +
    '  <script src="<%ExtPath%>/adapter/ext/ext-base<%DebugSuffix%>.js"></script>' + sLineBreak +
    '  <script src="<%ExtPath%>/<%ExtBuild%><%DebugSuffix%>.js"></script>' + sLineBreak +
{$IFDEF DEBUGJS}
    '  <script src="/codepress/Ext.ux.CodePress.js"></script>' + sLineBreak +
{$ENDIF}
    '  <%ThemeLink%>' + sLineBreak + '  <%LanguageLink%>' + sLineBreak + '  <%StyleTag%>' + sLineBreak +
    '  <%LibraryTags%>' + sLineBreak + '</head>' + sLineBreak + '<body>' + sLineBreak + '<div id="body">' + sLineBreak +
    '  <div id="loading" style="position:absolute;font-family:verdana;top:40%;left:40%">' + sLineBreak +
    '    <img src="<%ExtPath%>/classic/theme-classic/resources/images/shared/loading-balls.gif"/>Loading <%ApplicationTitle%>...'
    + sLineBreak + '  </div>' + sLineBreak + '</div>' + sLineBreak +
    '<noscript>This web application requires JavaScript enabled</noscript>' + sLineBreak + '</body>' + sLineBreak +
    '  <script>' + sLineBreak + '<%CustomJS%>' + sLineBreak +
    'function AjaxError(m){Ext.Msg.show({title:"Ajax Error",msg:m,icon:Ext.Msg.ERROR,buttons:Ext.Msg.OK});};' +
{$IFDEF DEBUGJS}
    'function AjaxSource(t,l,s){var w=new Ext.Window({title:"Ajax error: "+t+", Line: "+' +
    IfThen(Browser = brFirefox, '(l-%%)', '"Use Firefox to debug"') +
    ',width:600,height:400,modal:true,items:[new Ext.ux.CodePress({language:"javascript",readOnly:true,code:s})]});w.show();'
    + 'w.on("resize",function(){w.items.get(0).resize();});};' +
    'function AjaxSuccess(response){try{eval(response.responseText);}catch(err){AjaxSource(err.message,err.lineNumber,response.responseText);}};'
    +
{$ELSE}
    'function AjaxSuccess(response){try{eval(response.responseText);}catch(err){AjaxError(err.message+"<br/>Use DebugJS define to enhance debugging<br/>"+response.responseText);}};'
    +
{$ENDIF}
    'function sleep(ms){var start=new Date().getTime();for(var i=0;i<1e7;i++)if((new Date().getTime()-start)>ms)break;};'
    + sLineBreak + 'function AjaxFailure(){AjaxError("Server unavailable, try later.");};' + sLineBreak +
    'Ext.onReady(function(){' + sLineBreak + 'Ext.get("loading").remove();' + sLineBreak +
    'Ext.BLANK_IMAGE_URL="<%ExtPath%>/resources/images/default/s.gif";' + sLineBreak +
    'TextMetrics=Ext.util.TextMetrics.createInstance("body");' + sLineBreak +
    'Download=Ext.DomHelper.append(document.body,{tag:"iframe",cls:"x-hidden"});' + '<%Response%>});' + sLineBreak +
    '  </script>' + sLineBreak + '</html>';
end;

function TJSSession.GetManifestFileName: string;
begin
  Result := '';
end;

procedure TJSSession.AfterHandleRequest;
var
  LMainPageCode: string;
begin
  if IsDownLoad or IsUpload then
    Exit;

  Response := ResponseItems.Consume;

  if not IsAjax then
  begin
    ContentType := 'text/html; charset=' + Charset;
    LMainPageCode := GetMainPageTemplate;

    // Replace template macros in main page code.
    LMainPageCode := ReplaceText(LMainPageCode, '<%HTMLDeclaration%>', '<?xml version=1.0?>' + sLineBreak +
      '<!doctype html public "-//W3C//DTD XHTML 1.0 Strict//EN">' + sLineBreak +
      '<html xmlns=http://www.w3org/1999/xthml>' + sLineBreak);
    LMainPageCode := ReplaceText(LMainPageCode, '<%ViewportContent%>', GetViewportContent);
    LMainPageCode := ReplaceText(LMainPageCode, '<%ApplicationTitle%>', Application.Title);
    LMainPageCode := ReplaceText(LMainPageCode, '<%ApplicationIconLink%>',
      IfThen(Application.Icon = '', '', '<link rel="shortcut icon" href="' + Application.Icon + '"/>'));
    LMainPageCode := ReplaceText(LMainPageCode, '<%AppleIconLink%>',
      IfThen(Application.Icon = '', '', '<link rel="apple-touch-icon" sizes="120x120" href="' + Application.Icon
      + '"/>'));
    LMainPageCode := ReplaceText(LMainPageCode, '<%CharSet%>', Charset);
    LMainPageCode := ReplaceText(LMainPageCode, '<%ExtPath%>', ExtPath);
    LMainPageCode := ReplaceText(LMainPageCode, '<%ExtBuild%>', ExtBuild);
    LMainPageCode := ReplaceText(LMainPageCode, '<%DebugSuffix%>',
{$IFDEF DebugExtJS}'-debug'{$ELSE}''{$ENDIF});
    LMainPageCode := ReplaceText(LMainPageCode, '<%ManifestLink%>', IfThen(GetManifestFileName = '', '',
      Format('<link rel="manifest" href="%s"/>', [GetManifestFileName])));
    LMainPageCode := ReplaceText(LMainPageCode, '<%ThemeLink%>',
      IfThen(Theme = '', '', '<link rel=stylesheet href="' + ExtPath + '/build/classic/theme-' + Theme +
      '/resources/theme-' + Theme + '-all.css" />'));
    LMainPageCode := ReplaceText(LMainPageCode, '<%LanguageLink%>',
      IfThen(FLanguage = 'en', '', '<script src="' + ExtPath + '/build/classic/locale/locale-' + FLanguage +
      '.js"></script>'));
    LMainPageCode := ReplaceText(LMainPageCode, '<%StyleTag%>', GetStyleTag);
    LMainPageCode := ReplaceText(LMainPageCode, '<%LibraryTags%>', FLibraries);
    LMainPageCode := ReplaceText(LMainPageCode, '<%CustomJS%>', GetCustomJS);
    LMainPageCode := ReplaceText(LMainPageCode, '<%Response%>', Response);
    Response := LMainPageCode;
{$IFDEF DEBUGJS}
    Response := AnsiReplaceStr(Response, '%%', IntToStr(CountStr(^M^J, Response, 'eval('))); // eval() line number
{$ENDIF}
  end
  else
  begin
    if (Response <> '') and (Response[1] = '<') then
      ContentType := 'text/html; charset=' + Charset
    else if (Response <> '') and CharInSet(Response[1], ['{', '[']) then
      ContentType := 'application/json; charset=' + Charset
    else
      ContentType := 'text/javascript; charset=' + Charset;
  end;
end;

function TJSSession.GetNextJSName(const AObjectType: string): string;
var
  LResult: Cardinal;
begin
  if not FObjectSequences.ContainsKey(AObjectType) then
    FObjectSequences.Add(AObjectType, 0);
  LResult := FObjectSequences[AObjectType] + 1;
  FObjectSequences[AObjectType] := LResult;
  Result := AObjectType + IntToStr(LResult);
end;

function TJSSession.GetResponseItems: TJSResponseItems;
begin
  Assert(FResponseItemsStack.Count > 0);

  Result := FResponseItemsStack.Peek;
end;

function TJSSession.GetSingleton<T>(const AName: string): T;
begin
  if FSingletons.ContainsKey(AName) then
    Result := T(FSingletons[AName])
  else
  begin
    Result := TJSObjectClass(T).CreateSingleton(ObjectCatalog, AName) as T;
    FSingletons.Add(AName, Result);
  end;
end;

{ TJSObjectArray }

destructor TJSObjectArray.Destroy;
begin
  FreeAndNil(FObjects);
  inherited;
end;

function TJSObjectArray.Add(const AObject: TJSObject): Integer;
begin
  Assert(Assigned(AObject));
  Assert(AttributeName <> '');

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

function TJSObjectArray.IndexOf(const AObject: TJSObject): Integer;
begin
  Result := FObjects.IndexOf(AObject);
end;

function TJSObjectArray.Remove(const AObject: TJSObject): Integer;
begin
  Result := FObjects.Remove(AObject);
end;

function TJSObjectArray.AddInternal(const AObject: TJSObject): Integer;
begin
  Assert(Assigned(AObject));

  Result := FObjects.Add(AObject);

  if Assigned(Owner) and (Owner is TJSObject) then
    TJSObject(Owner).DependsUpon(AObject)
  else if Assigned(Owner) and Assigned(Owner.Owner) and (Owner.Owner is TJSObject) then
    TJSObject(Owner.Owner).DependsUpon(AObject);
end;

procedure TJSObjectArray.AfterConstruction;
begin
  inherited;
  FObjects := TObjectList<TJSObject>.Create(False);
end;

{ TJSObject }

procedure TJSObject.CreateJSName;
begin
  if Assigned(Owner) and (Owner is TJSObject) and (FAttributeName <> '') then
    FJSName := TJSObject(Owner).JSName + '.' + FAttributeName
  else
    FJSName := JSSession.GetNextJSName(GetObjectNamePrefix);
end;

function TJSObject.CharsToPixels(const AChars: Integer; const AOffset: Integer = 0): TJSExpression;
begin
  // + 16 sort of compensates for text-to-border left and right margins.
  Result := JSExpressionFromExpr(Format('({func0} * %d * 1.2) + %d', [AChars, 16 + AOffset]), [ExtUtilTextMetrics.GetWidth('g')]);
end;

{
  Converts a TExtFormTextArea height in characters to pixels to use in Height property.
  Uses dynamic JS in browser.
  @param Lines TextArea height in characters.
  @return Pixels used by browser to render these Lines
}
function TJSObject.LinesToPixels(const ALines: Integer): TJSExpression;
begin
  Result := JSExpressionFromExpr(Format('{func0} * %d * 1.3', [ALines]), [ExtUtilTextMetrics.GetHeight('W')]);
end;

// Deletes JS object from Browser memory
procedure TJSObject.Delete;
begin
  if Self <> nil then
    GetSession.ResponseItems.ExecuteJSCode(JSName + '.destroy(); delete ' + JSName + ';');
end;

procedure TJSObject.DependsUpon(const AObject: TJSObject);
begin
  JSSession.ResponseItems.AddObjectDependency(Self, AObject);
end;

destructor TJSObject.Destroy;
var
  LCreateItem: TJSCreateObject;
begin
  if (GetSession <> nil) and GetSession.HasResponseItems then
  begin
    LCreateItem := GetSession.ResponseItems.FindObjectCreateItem(Self);
    if Assigned(LCreateItem) then
    begin
      LCreateItem.UnlinkFromSender(FJSConfig);
      FJSConfig := nil;
    end;
  end;
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
var
  LParams, LMethodName, LObjectName: string;
begin
  FindMethod(AMethod, LMethodName, LObjectName);
  LParams := '';
  if LObjectName <> '' then
    LParams := '?Object=' + LObjectName;
  if GetSession.IsMobileApple then
    Result := 'window.open("' + GetSession.MethodURI(LMethodName) + LParams + '");'
  else
    Result := 'Download.src="' + GetSession.MethodURI(LMethodName) + LParams + '";';
end;

procedure TJSObject.Download(Method: TJSProcedure);
begin
  GetSession.ResponseItems.ExecuteJSCode(Self, GetDownloadJS(Method));
end;

constructor TJSObject.Create(const AOwner: TJSBase);
begin
  Assert(GetSession <> nil);
  Assert(Assigned(AOwner));
  inherited Create(AOwner);
  FJSConfig := TJSValues.Create(Self);
  CreateJSName;
  GetSession.ResponseItems.CreateObject(Self);
  InitDefaults;
end;

constructor TJSObject.CreateInternal(const AOwner: TJSBase; const AAttributeName: string);
begin
  Assert(Assigned(AOwner));

  inherited Create(AOwner);
  FJSConfig := TJSValues.Create(Self);
  FAttributeName := AAttributeName;
  FJSName := '';
  InitDefaults;
end;

constructor TJSObject.CreateInline(const AOwner: TJSBase);
begin
  CreateInternal(AOwner, '');
end;

constructor TJSObject.CreateSingleton(const AOwner: TJSBase; const AAttributeName: string);
begin
  Assert(Assigned(AOwner));
  inherited Create(AOwner);
  if AAttributeName = '' then
    FJSName := JSClassName
  else
    FJSName := AAttributeName;
  InitDefaults;
end;

constructor TJSObject.CreateInlineAndAddToArray(const AArray: TJSObjectArray);
begin
  Assert(Assigned(AArray));

  CreateInline(AArray);
  AArray.Add(Self);
end;

class function TJSObject.JSClassName: string;
begin
  Result := 'Object';
end;

function TJSObject.IsInline: Boolean;
begin
  Result := JSName = '';
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

function TJSObject.SetConfigItem(const AName, AMethodName, AValue: string): string;
begin
  if FJSConfig.IsReadOnly then
    CallMethod(AMethodName).AddParam(Avalue)
  else
    FJSConfig.Values.SetString(AName, AValue);
  Result := AValue;
end;

function TJSObject.SetConfigItem(const AName, AMethodName: string; const AValue: Integer): Integer;
begin
  if FJSConfig.IsReadOnly then
    CallMethod(AMethodName).AddParam(Avalue)
  else
    FJSConfig.Values.SetInteger(AName, AValue);
  Result := AValue;
end;

function TJSObject.SetConfigItem(const AName, AMethodName: string; const AValue: Boolean): Boolean;
begin
  if FJSConfig.IsReadOnly then
    CallMethod(AMethodName).AddParam(Avalue)
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

function TJSObject.CreateConfigArray(const AAttributeName: string): TJSObjectArray;
begin
  Result := TJSObjectArray.CreateInternal(FJSConfig, AAttributeName);
  SetConfigItem(AAttributeName, Result);
end;

procedure TJSObject.InitDefaults;
begin
end;

{
  Generates JS code to declare an inline JS Array.
  @param JSON JavaScript Object Notation, the body of Array declaration
  @param SquareBracket If true surrounds the array with []. Default is true.
  @return <link TJSObjectList> to be used in assigns
}
function TJSObject.JSArray(const AJSON: string; const ASquareBrackets: Boolean): TJSObjectArray;
begin
  Result := TJSObjectArray.CreateInline(Self);
  If ASquareBrackets then
    Result.FJSName := '[' + AJSON + ']'
  else
    Result.FJSName := AJSON;
end;

(*
  Generates JS code to declare an inline generic JS object.
  It is necessary in 3 cases:
  1. When the Ext JS documentation informs that the attribute is an object without any particular type (Object),
  as JavaScript language is almost typeless it happens eventually. That would be equivalent to the type Variant of VB or Delphi.
  Examples include data records.
  2. There are omissions in the documentation and attribute actually belongs to a specific class, in this case use the JSObject method,
  do a typecast or declare in ExtFixes.txt file, this allows to register in the Wrapper the omissions of the
  documentation or the framework.
  3. There are omissions in the framework, ie should be a specific class. Read its attributes in description contained
  in the documentation and declare them in ExtFixes.txt for the Wrapper to recognize them or use JSObject method.
  @param JSON JavaScript Object Notation, the body of JS object declaration
  @param ObjectConstructor Instantiate this object with a specific constructor. Default is ''
  @param CurlyBracket If true surrounds the JSON with {}. Default is true.
  @return <link TExtObject> to be used in assigns
*)
function TJSObject.JSObject(const AJSON: string; const AObjectConstructor: string; const ACurlyBrackets: Boolean): TJSObject;
begin
  Result := TJSObject.CreateInline(Self);
  try
    if ACurlyBrackets then
      Result.FJSName := '{' + AJSON + '}'
    else
      Result.FJSName := AJSON;
    if AObjectConstructor <> '' then
      Result.FJSName := 'new ' + AObjectConstructor + '(' + Result.FJSName + ')';
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

procedure TJSObject.HandleEvent(const AEventName: string);
begin
end;

function TJSObject.JSExpressionFromCodeBlock(const ACode: string): TJSExpression;
begin
  Result := TJSExpression.Create(Self);
  Result.Text := ACode;
end;

function TJSObject.GetJSCode(const AMethod: TProc; const ASilent: Boolean): string;
var
  LResponseItemBranch: TJSResponseItems;
begin
  LResponseItemBranch := JSSession.BranchResponseItems;
  try
    AMethod;
    Result := LResponseItemBranch.Consume;
    if ASilent then
      Result := 'try { ' + Result + ' } catch(e) {};';
  finally
    JSSession.UnbranchResponseItems(LResponseItemBranch, False);
  end;
end;

function TJSObject.AjaxCallMethod(const AName: string): TJSAjaxCall;
begin
  Result := GetSession.ResponseItems.AjaxCallMethod(Self, AName);
end;

procedure TJSObject.FindMethod(const AMethod: TJSProcedure; out AMethodName, AObjectName: string);
var
  LObject: TObject;
begin
  LObject := TMethod(AMethod).Data;
  AMethodName := LObject.MethodName(@AMethod);
  if AMethodName = '' then
    raise Exception.Create('Ajax: Method is not published')
  else
  begin
    if LObject is TJSObject then
      AObjectName := TJSObject(LObject).JSName
    else
      AObjectName := '';
  end;
end;

function TJSObject.ParamAsInteger(ParamName: string): Integer;
begin
  Result := StrToIntDef(JSSession.Query[ParamName], 0);
end;

function TJSObject.ParamAsDouble(ParamName: string): double;
begin
  Result := StrToFloatDef(JSSession.Query[ParamName], 0);
end;

function TJSObject.ParamAsBoolean(ParamName: string): Boolean;
begin
  Result := JSSession.Query[ParamName] = 'true';
end;

function TJSObject.ParamAsString(ParamName: string): string;
begin
  Result := JSSession.Query[ParamName];
end;

function TJSObject.ParamAsDateTime(ParamName: string): TDateTime;
begin
  Result := ParamAsDouble(ParamName);
end;

function TJSObject.ParamAsObject(ParamName: string): TJSObject;
begin
  Result := TJSObject(JSSession.ObjectCatalog.FindChildByJSName(JSSession.Query[ParamName]));
end;

{ TJSTextBase }

procedure TJSTextBase.FormatTo(const AFormatter: TJSFormatter);
begin
  inherited;
  if not IsExpressionExtracted then
    AFormatter.Add(FText);
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
    CallMethod(AMethodName).AddParam(Avalue)
  else if Assigned(AValue) then
    FJSConfig.SetRawValue(AName, AValue.ExtractText)
  else
    FJSConfig.SetRawValue(AName, '');
  Result := AValue;
end;

function TJSObject.SetConfigItem(const AName, AMethodName: string; const AValue: TJSObject): TJSObject;
begin
  if FJSConfig.IsReadOnly then
    CallMethod(AMethodName).AddParam(Avalue)
  else
    FJSConfig.Values.SetObject(AName, AValue);
  Result := AValue;
end;

function TJSObject.SetConfigItemOrProperty(const AName: string; const AValue: Boolean): Boolean;
begin
  if FJSConfig.IsReadOnly then
    GetSession.ResponseItems.SetProperty(Self, AName, AValue)
  else
    FJSConfig.Values.SetBoolean(AName, AValue);
  Result := AValue;
end;

function TJSObject.SetProperty(const AName: string; const AValue: TJSExpression): TJSExpression;
begin
  GetSession.ResponseItems.SetProperty(Self, AName, AValue);
  Result := AValue;
end;

function TJSObject.SetProperty(const AName: string; const AValue: TJSObject): TJSObject;
begin
  GetSession.ResponseItems.SetProperty(Self, AName, AValue);
  Result := AValue;
end;

function TJSObject.SetProperty(const AName: string; const AValue: Boolean): Boolean;
begin
  GetSession.ResponseItems.SetProperty(Self, AName, AValue);
  Result := AValue;
end;

function TJSObject.SetProperty(const AName, AValue: string): string;
begin
  GetSession.ResponseItems.SetProperty(Self, AName, AValue);
  Result := AValue;
end;

function TJSObject.SetProperty(const AName: string; const AValue: Integer): Integer;
begin
  GetSession.ResponseItems.SetProperty(Self, AName, AValue);
  Result := AValue;
end;

function TJSObject.SetConfigItemOrProperty(const AName, AValue: string): string;
begin
  if FJSConfig.IsReadOnly then
    GetSession.ResponseItems.SetProperty(Self, AName, AValue)
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
    CallMethod(AMethodName).AddParam(Avalue)
  else
    FJSConfig.Values.SetDateTime(AName, AValue);
  Result := AValue;
end;

function TJSObject.SetProperty(const AName: string; const AValue: TDateTime): TDateTime;
begin
  GetSession.ResponseItems.SetProperty(Self, AName, AValue);
  Result := AValue;
end;

function TJSObject.CallMethod(const AName: string): TJSMethodCall;
begin
  Result := GetSession.ResponseItems.CallMethod(Self, AName);
end;

function TJSObject.GenerateAnonymousFunction(const AExpression: TJSExpression): TJSExpression;
begin
  Result := GenerateAnonymousFunction('', AExpression, '');
end;

function TJSObject.GenerateAnonymousFunction(const ABody: string): TJSExpression;
begin
  Result := GenerateAnonymousFunction('', ABody, '');
end;

{ TJS }

class function TJS.StrToJS(const AString: string; AUseBR: Boolean): string;
var
  I, J: Integer;
  BR: string;
begin
  BR := IfThen(AUseBR, '<br/>', '\n');
  Result := AnsiReplaceStr(AString, '"', '\"');
  Result := AnsiReplaceStr(Result, ^M^J, BR);
  Result := AnsiReplaceStr(Result, ^M, BR);
  Result := AnsiReplaceStr(Result, ^J, BR);
  if (Result <> '') and (Result[1] = #3) then
  begin // Is RegEx
    Delete(Result, 1, 1);
    if pos('/', Result) <> 1 then
      Result := '/' + Result + '/';
  end
  else
  begin
    I := pos('%', Result);
    if (pos(';', Result) = 0) and (I <> 0) and ((Length(Result) > 1) and (I < Length(Result)) and
      CharInSet(Result[I + 1], ['0' .. '9'])) then
    begin // Has param place holder, ";" disable place holder
      J := FirstDelimiter(' "''[]{}><=!*-+/,', Result, I + 2);
      if J = 0 then
        J := Length(Result) + 1;
      if J <> (Length(Result) + 1) then
      begin
        insert('+"', Result, J);
        Result := Result + '"';
      end;
      if I <> 1 then
      begin
        insert('"+', Result, I);
        Result := '"' + Result;
      end;
    end
    else if (I = 1) and (Length(Result) > 1) and CharInSet(Result[2], ['a' .. 'z', 'A' .. 'Z']) then
      Result := Copy(Result, 2, Length(Result))
    else
      Result := '"' + Result + '"'
  end;
end;

class function TJS.EnumToJSString(const ATypeInfo: PTypeInfo; const AValue: Integer): string;
var
  I: Integer;
  JS: string;
begin
  Result := '';
  JS := GetEnumName(ATypeInfo, AValue);
  for I := 1 to Length(JS) do
  begin
    if CharInSet(JS[I], ['A' .. 'Z']) then
    begin
      Result := LowerCase(Copy(JS, I, 100));
      if Result = 'perc' then
        Result := '%';
      Exit;
    end;
  end;
end;

class function TJS.GetPadding(const ATop: Integer; const ARight: Integer; const ABottom: Integer; const ALeft: Integer;
const ACSSUnit: TCSSUnit; const AHeader: Boolean): string;
begin
  Result := Format('%s%d%3:s %2:d%3:s', [IfThen(AHeader, 'padding: ', ''), ATop, ARight,
    EnumToJSString(TypeInfo(TCSSUnit), Ord(ACSSUnit))]);
  if ABottom <> -1 then
    Result := Result + Format(' %d%2:s %1:d%2:s', [ABottom, ALeft, EnumToJSString(TypeInfo(TCSSUnit), Ord(ACSSUnit))]);
end;

class function TJS.JSDateToDateTime(const AJSDate: string): TDateTime;
begin
  Result := EncodeDateTime(StrToInt(Copy(AJSDate, 12, 4)), AnsiIndexStr(Copy(AJSDate, 5, 3),
    ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']) + 1,
    StrToInt(Copy(AJSDate, 9, 2)), StrToInt(Copy(AJSDate, 17, 2)), StrToInt(Copy(AJSDate, 20, 2)),
    StrToInt(Copy(AJSDate, 23, 2)), 0);
end;

class function TJS.RemoveLastJSTerminator(const AJSCode: string): string;
begin
  Result := AJSCode;
  while EndsStr(sLineBreak, Result) do
    Result := Copy(Result, 1, Length(Result) - Length(sLineBreak));
  if (Result <> '') and (Result[Length(Result)] = ';') then
    Delete(Result, Length(Result), 1);
end;

class function TJS.GetMargins(const ATop: Integer; const ARight: Integer; const ABottom: Integer; const ALeft: Integer;
const ACSSUnit: TCSSUnit; const AHeader: Boolean): string;
begin
  Result := Format('%s%d%5:s %2:d%5:s %3:d%5:s %4:d%s', [IfThen(AHeader, 'margin: ', ''), ATop, ARight, ABottom, ALeft,
    EnumToJSString(TypeInfo(TCSSUnit), Ord(ACSSUnit))])
end;

class function TJS.DelphiDateTimeFormatToJSDateTimeFormat(const ADateTimeFormat: string): string;
var
  LFormats: TStringDynArray;
begin
  LFormats := Split(ADateTimeFormat);
  Assert(Length(LFormats) = 2);

  Result := DelphiDateFormatToJSDateFormat(LFormats[0]) + ' ' + DelphiTimeFormatToJSTimeFormat(LFormats[1]);
end;

class function TJS.DelphiDateFormatToJSDateFormat(const ADateFormat: string): string;
begin
  Result := ReplaceText(ADateFormat, 'yyyy', 'Y');
  Result := ReplaceText(Result, 'yy', 'y');
  Result := ReplaceText(Result, 'dd', 'd');
  Result := ReplaceText(Result, 'mm', 'm');
end;

class function TJS.DelphiTimeFormatToJSTimeFormat(const ATimeFormat: string): string;
begin
  Result := ReplaceText(ATimeFormat, 'hh', 'H');
  Result := ReplaceText(Result, 'mm', 'i');
  Result := ReplaceText(Result, 'nn', 'i');
  Result := ReplaceText(Result, 'ss', 's');
end;

class function TJS.WrapInAnonymousFunction(const AArgs, ABody: string; const AReturn: string): string;
begin
  { TODO : formatting }
{ TODO : find the best place where to insert a return statement when not specified? }
  Result := 'function(' + AArgs + ') {' + sLineBreak +
    ABody + ';' + sLineBreak;
  if AReturn <> '' then
    Result := Result + 'return ' + AReturn + ';' + sLineBreak;
  Result := Result + '}' + sLineBreak;
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
end;

{ TJSFormatter }

function TJSFormatter.Add(const AString: string): TJSFormatter;
begin
  FFormattedText := FFormattedText + AString;
  Result := Self;
end;

function TJSFormatter.AddIndent: TJSFormatter;
begin
  Result := Add(IndentStr);
end;

function TJSFormatter.AddIndented(const AString: string): TJSFormatter;
begin
  Result := Add(IndentStr + AString.Replace(sLineBreak, sLineBreak + IndentStr));
end;

function TJSFormatter.AddIndentedLine(const ALine: string): TJSFormatter;
begin
  Result := AddIndented(ALine + sLineBreak);
end;

function TJSFormatter.AddIndentedList(const ALines: TArray<string>): TJSFormatter;
var
  I: Integer;
begin
  for I := Low(ALines) to High(ALines) do
  begin
    if I < High(ALines) then
      AddIndentedLine(ALines[I] + ',')
    else
      AddIndentedLine(ALines[I]);
  end;
  Result := Self;
end;

function TJSFormatter.AddIndentedPair(const AName, AStrValue: string;
  const AQuoteValue, AAddComma: Boolean; const AConnector: string): TJSFormatter;
begin
  Result := AddIndented(AName + AConnector + IfThen(AQuoteValue, '"', '') + AStrValue + IfThen(AQuoteValue, '"', '')
    + IfThen(AAddComma, ',', ''));
end;

function TJSFormatter.AddIndentedPairLine(const AName, AStrValue: string;
  const AQuoteValue: Boolean; const AAddComma: Boolean): TJSFormatter;
begin
  Result := AddIndentedLine(AName + ': ' + IfThen(AQuoteValue, '"', '') + AStrValue + IfThen(AQuoteValue, '"', '')
    + IfThen(AAddComma, ',', ''));
end;

function TJSFormatter.AddLine(const ALine: string): TJSFormatter;
begin
  Result := Add(ALine + sLineBreak);
end;

procedure TJSFormatter.AfterConstruction;
begin
  inherited;
  FFormatSettings := TFormatSettings.Create;
end;

function TJSFormatter.CloseArray: TJSFormatter;
begin
  Result := SkipLine.Outdent.AddIndented(']');
end;

function TJSFormatter.CloseObject: TJSFormatter;
begin
  Result := SkipLine.Outdent.AddIndented('}');
end;

function TJSFormatter.CloseRound: TJSFormatter;
begin
  Result := SkipLine.Outdent.AddIndented(')');
end;

function TJSFormatter.DeleteTrailing(const AString: string): TJSFormatter;
begin
  FFormattedText := StripSuffix(FFormattedText, AString);
  Result := Self;
end;

function TJSFormatter.FormatArray(const ALines: TArray<string>): TJSFormatter;
begin
  Result := OpenArray.AddIndentedList(ALines).CloseArray;
end;

function TJSFormatter.FormatObject(const ALines: TArray<string>): TJSFormatter;
begin
  Result := OpenObject.AddIndentedList(ALines).CloseObject;
end;

function TJSFormatter.Indent: TJSFormatter;
begin
  Inc(FCurrentIndent);
  Result := Self;
end;

function TJSFormatter.IndentStr: string;
begin
  Result := DupeString('  ', FCurrentIndent);
end;

function TJSFormatter.OpenArray: TJSFormatter;
begin
  Result := Add('[').SkipLine.Indent;
end;

function TJSFormatter.OpenObject: TJSFormatter;
begin
  Result := Add('{').SkipLine.Indent;
end;

function TJSFormatter.OpenRound: TJSFormatter;
begin
  Result := Add('(').SkipLine.Indent;
end;

function TJSFormatter.Outdent: TJSFormatter;
begin
  Dec(FCurrentIndent);
  Result := Self;
end;

function TJSFormatter.SkipLine: TJSFormatter;
begin
  Result := Add(sLineBreak);
end;

{ TJSExpression }

procedure TJSExpression.SetText(const AValue: string);
begin
  FText := AValue;
  FExtracted := False;
end;

function TJSExpression.ExtractText: string;
begin
  Assert(not FExtracted);
  Result := InternalExtractText;
  FExtracted := True;
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

{ TJSFunctionResponseItem }

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
//  if HasExpression and not FExpression.FExtracted then
  FreeAndNil(FExpression);
  inherited;
end;

function TJSExpressionResponseItem.GetAsExpression: TJSExpression;
begin
  if not Assigned(FExpression) then
  begin
    FExpression := TJSExpression.Create(nil);
    FExpression.Text := GetFormattedCode;
  end;
  Result := FExpression;
end;

function TJSExpressionResponseItem.GetAsFunction: TJSFunction;
begin
  if not Assigned(FExpression) then
  begin
    FExpression := TJSFunction.Create(nil);
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
  Result := HasExpression and FExpression.FExtracted;
end;

{ TJSAjaxCall }

procedure TJSAjaxCall.FormatTo(const AFormatter: TJSFormatter);
begin
  //inherited;
  if not IsExpressionExtracted then
  begin
    Assert(CallName <> '');

    AFormatter.SkipLine.Indent;
    AFormatter.AddIndented('Ext.Ajax.request(').SkipLine.Indent.AddIndent.OpenObject;
    AFormatter.AddIndentedPairLine('url', GetSession.MethodURI(CallName));
    if FHttpMethod <> 'GET' then
      AFormatter.AddIndentedPairLine('method', FHttpMethod);
    if (FHttpMethod = 'POST') and (FPostData <> '') then
      AFormatter.AddIndentedPairLine('jsonData', FPostData);
    AddParams(AFormatter);
    AFormatter.AddIndentedPairLine('success', 'AjaxSuccess', False);
    AFormatter.AddIndentedPair('failure', 'AjaxFailure', False);
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
  if (LObject is TJSObject) and (LObject <> Sender) then
    ChangeSender(TJSObject(LObject));
  CallName := LObject.MethodName(@AMethod);
  Assert(CallName <> '');
  Result := Self;
end;

procedure TJSAjaxCall.AfterConstruction;
begin
  inherited;
  FHttpMethod := 'GET';
  Params.NameValueConnector := '=';
  Params.ParamConnector := '&';
  Params.ParamValuePrefix := '" + encodeURIComponent(';
  Params.ParamValueSuffix := ') + "';
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

function TJSAjaxCall.AddParam(const AName: string; const AValue: TJSObject): TJSAjaxCall;
begin
  FParams.Values.SetObject(AName, AValue);
  Result := Self;
end;

procedure TJSAjaxCall.AddParams(const AFormatter: TJSFormatter);
begin
{ TODO : expand markers and surround params }
  AFormatter.AddIndentedPairLine('params', Params.AsFormattedText);
end;

function TJSAjaxCall.AddRawParam(const AName, AValue: string): TJSAjaxCall;
begin
  FParams.SetRawValue(AName, AValue);
  Result := Self;
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

function TJSBase.GetJSSession: TJSSession;
begin
  Result := GetJSSession(Owner);
end;

function TJSBase.GetJSSession(const AOwner: TJSBase): TJSSession;
var
  LOwner: TJSBase;
begin
  if FJSSession = nil then
  begin
    LOwner := AOwner;
    while (LOwner <> nil) and (LOwner.Owner <> nil) do
      LOwner := LOwner.Owner;
    if LOwner is TJSObjectCatalog then
      FJSSession := TJSObjectCatalog(LOwner).JSSession as TJSSession;
    if FJSSession = nil then
      raise Exception.CreateFmt('Session not found for object %s of type %s.', [JSName, ClassName]);
  end;
  Result := FJSSession;
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
    FChildren.Extract(Self); // don't free it
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

initialization
  _JSFormatSettings := TFormatSettings.Create;
  _JSFormatSettings.DecimalSeparator := '.';

end.
