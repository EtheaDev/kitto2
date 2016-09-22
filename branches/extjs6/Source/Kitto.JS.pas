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

  , {$IFNDEF WebServer}FCGIApp{$ELSE}IdExtHTTPServer{$ENDIF}
  , Kitto.JS.Types

  ;

type
  TJS = class
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

    /// <summary>
    /// Encapsulates JS commands in an anonymous JS function, finds %0..%9 placeholders
    /// and declares respective event parameters
    /// </summary>
    class function GetJSFunction(const ACode: string): string;
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
    function Add(const AString: string): TJSFormatter;
    function AddLine(const ALine: string): TJSFormatter;
    function AddIndent: TJSFormatter;
    function AddIndented(const AString: string): TJSFormatter;
    function AddIndentedLine(const ALine: string): TJSFormatter;
    function AddIndentedPairLine(const AName, AStrValue: string;
      const AQuoteValue: Boolean = True; const AAddComma: Boolean = True): TJSFormatter;
    function AddIndentedPair(const AName, AStrValue: string;
      const AQuoteValue: Boolean = True; const AAddComma: Boolean = True): TJSFormatter;
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

  TJSObjectCatalog = class(TComponent)
  private
    FSession: TWebSession;
  public
    function FindObject(const AJSName: string): TObject;

    procedure FreeAllObjects;

    property Session: TWebSession read FSession;
  end;

  TJSSession = class;
  TJSFunction = class;
  TJSObjectArray = class;
  TJSResponseItems = class;

  TVarToJSONProc = TProc<string, TObject, Boolean>;

  TJSConfig = class(TComponent)
  private
    FIsReadOnly: Boolean;
    FValues: TEFTree;
    function IsFunction(const AValue: TEFNode): Boolean;
    function IsObjectArray(const AValue: TEFNode): Boolean;
    function IsObject(const AValue: TEFNode): Boolean;
    function GetJSSession: TJSSession;
    property JSSession: TJSSession read GetJSSession;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
  public
    property Values: TEFTree read FValues;

    procedure SetFunctionValue(const AName: string; const AValue: TJSFunction);

    // The config does not accept setting values anymore.
    // False during the request that creates the parent object, True after that.
    property IsReadOnly: Boolean read FIsReadOnly;
    /// <summary>
    ///  Raises an exception if the object is read-only. The exception message
    ///  includes the value name.
    /// </summary>
    procedure CheckReadOnly(const AValueName: string);

    procedure FormatTo(const AFormatter: TJSFormatter);
  end;

  TJSObject = class(TComponent)
  private
    // Assigned if the object was created with CreateInternal or CreateInline.
    { TODO : Maybe we could replace it with an extraction from JSName }
    FAttributeName: string;
    FSession: TJSSession;
    FJSName: string;
    FJSConfig: TJSConfig;
    function FormatParams(MethodName: string; Params: array of const): string;
    procedure AjaxCode(const AMethodName, ARawParams: string; const AParams: array of const;
      const AAdditionalDependencies: array of TJSObject);
    function FindMethod(Method: TJSProcedure; var PascalName, ObjName: string): TJSFunction;
    function GetDownloadJS(const AMethod: TJSProcedure; const AParams: array of const): string;
    function DoGetAjaxCode(const AMethodName, ARawParams: string; const AParams: array of const;
      const AExtraCode: string): string;
  protected
    function GetJSSession: TJSSession; overload;
    function GetJSSession(const AOwner: TComponent): TJSSession; overload;
    function VarToJSON(const AVars: array of const; const ASession: TJSSession = nil): string; overload;
    function VarToJSON(const AVars: array of const; const AProc: TVarToJSONProc): string; overload;
    function VarToJSON(const AList: TJSObjectArray): string; overload;
    function ArrayToJSON(Strs: array of string): string; overload;
    function ArrayToJSON(Ints: array of Integer): string; overload;
    function ParamAsInteger(ParamName: string): Integer;
    function ParamAsDouble(ParamName: string): double;
    function ParamAsBoolean(ParamName: string): Boolean;
    function ParamAsString(ParamName: string): string;
    function ParamAsDateTime(ParamName: string): TDateTime;
    function ParamAsObject(ParamName: string): TJSObject;
    procedure CreateJSName; virtual;
    function GetObjectNamePrefix: string; virtual;
    procedure InitDefaults; virtual;
    procedure HandleEvent(const AEvtName: string); virtual;
    property JSSession: TJSSession read GetJSSession;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function CreateConfigArray(const AAttributeName: string): TJSObjectArray;
    procedure DependsUpon(const AObject: TJSObject);
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateInternal(const AOwner: TJSObject; const AAttributeName: string);
    constructor CreateInline(const AOwner: TJSObject);

    constructor CreateSingleton(const AOwner: TComponent; const AAttributeName: string);
    constructor CreateAndAddToArray(const AArray: TJSObjectArray);
    constructor CreateInlineAndAddToList(const AList: TJSObjectArray);

    property AttributeName: string read FAttributeName;

    function GetConstructionJS: string;

    function IsInternal: Boolean;
    function IsInline: Boolean;

    procedure Delete;
    class function JSClassName: string; virtual;
    function JSArray(const AJSON: string; const ASquareBrackets: Boolean = True): TJSObjectArray;
    function JSObject(const AJSON: string; const AObjectConstructor: string = ''; const ACurlyBrackets: Boolean = True)
      : TJSObject;
    function JSFunction(const AParams, ABody: string): TJSFunction; overload;
    procedure JSFunction(const AName, AParams, ABody: string); overload;
    function JSFunction(const ABody: string): TJSFunction; overload;
    function JSFunction(const AMethod: TJSProcedure; const ASilent: Boolean = False): TJSFunction; overload;
    function JSFunction(const AMethod: TProc; const ASilent: Boolean = False): TJSFunction; overload;
    function JSFunctionFromCodeBlock(const ACode: string): TJSFunction;
    function GetJSFunctionCode(const AMethod: TProc; const ASilent: Boolean = False): string;
    function JSFunctionFromExpr(const AExpr: string; const AValues: array of TJSFunction): TJSFunction;
    procedure JSCode(JS: string; pJSName: string = ''; pOwner: string = '');
    procedure JSSleep(MiliSeconds: Integer);

    function Ajax(const AMethod: TJSProcedure; const AParams: string): TJSFunction; overload;

    function Ajax(const AMethod: TJSProcedure; const AParams: string; const AAdditionalDependencies: array of TJSObject;
      const AIsEvent: Boolean = False): TJSFunction; overload;

    function Ajax(const AMethod: TJSProcedure; const AParams: array of const;
      const AAdditionalDependencies: array of TJSObject; const AIsEvent: Boolean = False): TJSFunction; overload;

    function Ajax(const AMethodName: string; const AParams: array of const;
      const AAdditionalDependencies: array of TJSObject; const AIsEvent: Boolean = False): TJSFunction; overload;

    function Ajax(const AMethodName: string; const AParams: array of const; const AIsEvent: Boolean = False)
      : TJSFunction; overload;

    function Ajax(const AMethod: TJSProcedure): TJSFunction; overload;

    function Ajax(const AMethod: TJSProcedure; const AParams: array of const): TJSFunction; overload;

    // Use these to generate and return js code that performs an ajax call, useful
    // when building js handlers. These methods DO NOT add any code to the
    // current response.
    function GetAjaxCode(const AMethod: TJSProcedure; const AParams: array of const; const AExtraCode: string)
      : string; overload;
    function GetAjaxCode(const AMethodName, ARawParams: string; const AParams: array of const): string; overload;
    function GetAjaxCode(const AMethod: TJSProcedure; const AParams: array of const; const AIsEvent: Boolean = False)
      : string; overload;
    function GetAjaxCode(const AMethod: TJSProcedure; const ARawParams: string; const AParams: array of const)
      : string; overload;
    // Ajax calls with the POST method that passes JSON data in the jsonData option of
    // Ext.Ajax.request. AJsonData can be js code, such as a function call, or a streamed json object.
    function GetPOSTAjaxCode(const AMethodName, ARawParams: string; const AParams: array of const;
      const AJsonData: string): string; overload;
    function GetPOSTAjaxCode(const AMethod: TJSProcedure; const AParams: array of const; const AJsonData: string)
      : string; overload;

    function ExtractJSCommand: string;

    function RequestDownload(Method: TJSProcedure): TJSFunction; overload;
    function RequestDownload(Method: TJSProcedure; Params: array of const): TJSFunction; overload;
    procedure Download(Method: TJSProcedure); overload;
    procedure Download(Method: TJSProcedure; Params: array of const); overload;
    function MethodURI(Method: TJSProcedure; Params: array of const): string; overload;
    function MethodURI(Method: TJSProcedure): string; overload;
    function MethodURI(const AMethodName: string; const AParams: array of const): string; overload;
    function MethodURI(const AMethodName: string): string; overload;
    {
      Converts a length in characters to pixels.
      Uses dynamic JS in browser.
      @param Chars Field length in characters
      @return Pixels used by browser to render these Chars
    }
    function CharsToPixels(const AChars: Integer; const AOffset: Integer = 0): TJSFunction;
    function LinesToPixels(const ALines: Integer): TJSFunction;
    destructor Destroy; override;
    property JSName: string read FJSName;
    function FindJSObject(const AJSName: string): TObject;

    function SetConfigItem(const AName, AMethodName: string; const AValue: string): string; overload;
    function SetConfigItem(const AName, AMethodName: string; const AValue: Boolean): Boolean; overload;
    function SetConfigItem(const AName, AMethodName: string; const AValue: Integer): Integer; overload;
    function SetConfigItem(const AName, AMethodName: string; const AValue: TDateTime): TDateTime; overload;
    function SetConfigItem(const AName, AMethodName: string; const AValue: TJSObject): TJSObject; overload;
    function SetFunctionConfigItem(const AName, AMethodName: string; const AValue: TJSFunction): TJSFunction; overload;

    function SetConfigItem(const AName, AValue: string): string; overload;
    function SetConfigItem(const AName: string; const AValue: TJSObject): TJSObject; overload;
    function SetConfigItem(const AName: string; const AValue: Integer): Integer; overload;
    function SetConfigItem(const AName: string; const AValue: Boolean): Boolean; overload;
    function SetConfigItem(const AName: string; const AValue: Double): Double; overload;
    function SetFunctionConfigItem(const AName: string; const AValue: TJSFunction): TJSFunction; overload;

    function SetConfigItemOrProperty(const AName, AValue: string): string; overload;
    function SetConfigItemOrProperty(const AName: string; const AValue: Boolean): Boolean; overload;

    function SetProperty(const AName: string; const AValue: Integer): Integer; overload;
    function SetProperty(const AName, AValue: string): string; overload;
    function SetProperty(const AName: string; const AValue: TJSFunction): TJSFunction; overload;
    function SetProperty(const AName: string; const AValue: TJSObject): TJSObject; overload;
    function SetProperty(const AName: string; const AValue: Boolean): Boolean; overload;
    function SetProperty(const AName: string; const AValue: TDateTime): TDateTime; overload;

    function CallMethod(const AName: string; const AValue: Boolean): TJSFunction; overload;
    function CallMethod(const AName, AValue: string): TJSFunction; overload;
    function CallMethod(const AName: string; const AValue: TDateTime): TJSFunction; overload;
    function CallMethod(const AName: string): TJSFunction; overload;
    function CallMethod(const AName: string; const AValue: array of const): TJSFunction; overload;
    function CallMethod(const AName: string; const AValue: TJSObject): TJSFunction; overload;
    function CallFunctionMethod(const AName: string; const AValue: TJSFunction): TJSFunction;

    property JSConfig: TJSConfig read FJSConfig;
  end;

  TJSObjectClass = class of TJSObject;

  TJSFunction = class(TJSObject);

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
    function GetStyleTag: string;
    function GetResponseItems: TJSResponseItems;
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
    property Language: string read FLanguage write SetLanguage;
    // Actual language for this session, reads HTTP_ACCEPT_LANGUAGE header
    procedure InitDefaultValues; override;
    procedure SetStyle(const AStyle: string = '');
    procedure SetLibrary(pLibrary: string = ''; CSS: Boolean = False; HasDebug: Boolean = False;
      DisableExistenceCheck: Boolean = False);
    procedure SetCSS(pCSS: string; Check: Boolean = True);
    procedure ErrorMessage(const AMessage: string; const AAction: string = ''); overload;
    procedure ErrorMessage(const AMessage: string; const AAction: TJSFunction); overload;
    procedure Alert(const Msg: string); override;
    procedure Refresh; override;

    property ResponseItems: TJSResponseItems read GetResponseItems;
    function HasResponseItems: Boolean;
    function BranchResponseItems: TJSResponseItems;
    procedure UnbranchResponseItems(const AResponseItems: TJSResponseItems; const AConsolidate: Boolean = True);

    function GetSingleton<T: TJSObject>(const AName: string): T;
    function IsMobileApple: Boolean;
    property ObjectCatalog: TJSObjectCatalog read FObjectCatalog;
  published
    procedure HandleEvent; virtual;
  end;

  TJSObjectArray = class(TJSFunction)
  private
    FObjects: TObjectList<TJSObject>;
    function GetObject(I: Integer): TJSObject;
    function GetOwnerJSObject: TJSObject;
    function GetCount: Integer;
    property OwnerJSObject: TJSObject read GetOwnerJSObject;
  protected
    procedure CreateJSName; override;
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

  TJSResponseItems = class
  private
    FList: TList<TJSResponseItem>;
    FEmittedItems: TList<TJSResponseItem>;
    function GetObjectCreateItem(const AObject: TJSObject): TJSCreateObject;
    // procedure SortByDependency;
    function GetCount: Integer;
    function GetItem(I: Integer): TJSResponseItem;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
  public
    // Create an object.
    procedure CreateObject(const AObject: TJSObject);

    // Create an internal object; could be a config item of an object type.
    procedure CreateInternalObject(const AObject: TJSObject; const AAttributeName: string);

    // Use this method to have object creation statements in the correct order when emitting the response.
    procedure AddObjectDependency(const ADependentObject, ADependedUponObject: TJSObject);

{ TODO :
Progressively get rid of array of const and add explicit call(s) for calling a method
passing one or more TJSFunctions }
    procedure CallMethod(const AObject: TJSObject; const AMethodName: string; const AParams: array of const); overload;

    procedure GetProperty(const AObject: TJSObject; const APropertyName: string);
{ TODO :
Progressively get rid of array of const and add explicit call(s) for setting a function-valued property }
    // AValues must be either a single value of any type or an object value followed by the IsFunction boolean flag.
    procedure SetProperty(const AObject: TJSObject; const APropertyName: string; const AValues: array of const);

    procedure ExecuteJSCode(const AJSCode: string); overload;
    procedure ExecuteJSCode(const AObject: TJSObject; const AJSCode: string); overload;
    procedure ExecuteJSCode(const AObject: TJSObject; const AJSCode: string;
      const AAdditionalDependencies: array of TJSObject); overload;

    procedure AddJSON(const AJSON: string);

    procedure AddHTML(const AHTML: string);

    function AsFormattedString: string;
    function Consume: string;

    // Remove any items sent by the specified object.
    procedure RemoveAll(const AObject: TJSObject);

    procedure Remove(const AItem: TJSResponseItem);

    // Returns the first item with IsCode=True for the specified object, or nil.
    function FindLastCodeItem(const AObject: TJSObject): TJSResponseItem;
    // Returns the first item with IsCode=True for the specified object, or
    // raises an exception.
    function GetLastCodeItem(const AObject: TJSObject): TJSResponseItem;

    function FindObjectCreateItem(const AObject: TJSObject): TJSCreateObject;

    property Items[I: Integer]: TJSResponseItem read GetItem; default;
    property Count: Integer read GetCount;
    procedure Clear;
  end;

  TJSResponseItem = class
  private
    FSender: TJSObject;
    FDependencies: TList<TJSResponseItem>;
    FEmitted: Boolean;
    FCreationDateTime: TDateTime;
    function GetDependencyCount: Integer;
    function GetDependency(I: Integer): TJSResponseItem;
    function AllDependenciesEmitted(const AEmittedItems: TList<TJSResponseItem>): Boolean;
  strict protected
    FRoot: TJSResponseItems;
  public
    constructor Create(const ASender: TJSObject; const ARoot: TJSResponseItems); virtual;
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

  TJSCreateObject = class(TJSResponseItem)
  private
    FItems: TList<TJSNamedCreateObject>;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;

    procedure FormatTo(const AFormatter: TJSFormatter); override;

    function IsCode: Boolean; override;

    procedure CreateInternalObject(const AAttributeName: string; const ASender: TJSObject);

    function FindObjectCreateItem(const ASender: TJSObject): TJSCreateObject;
  end;

  TExtCallMethod = class(TJSResponseItem)
  private
    FCallName: string;
    FCallParams: TArray<string>;
  public
    procedure FormatTo(const AFormatter: TJSFormatter); override;
    property CallName: string read FCallName write FCallName;
    property CallParams: TArray<string> read FCallParams write FCallParams;
  end;

  TExtPropertyBase = class abstract(TJSResponseItem)
  private
    FPropertyName: string;
  public
    property PropertyName: string read FPropertyName write FPropertyName;
  end;

  TExtGetProperty = class(TExtPropertyBase)
  public
    procedure FormatTo(const AFormatter: TJSFormatter); override;
  end;

  TExtSetProperty = class(TExtPropertyBase)
  private
    FPropertyValue: string;
  public
    procedure FormatTo(const AFormatter: TJSFormatter); override;
    property PropertyValue: string read FPropertyValue write FPropertyValue;
  end;

  TExtTextBase = class(TJSResponseItem)
  protected
    FText: string;
  public
    property Text: string read FText write FText;
    procedure FormatTo(const AFormatter: TJSFormatter); override;
  end;

  TExtJSCode = class(TExtTextBase)
  public
    property JSCode: string read FText write FText;
    procedure FormatTo(const AFormatter: TJSFormatter); override;
  end;

  TExtJSON = class(TExtTextBase)
  public
    property JSON: string read FText write FText;
  end;

  TExtHTML = class(TExtTextBase)
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
  , Ext.Util // for ExtUtilTextMetrics. Switch to pure JS?
  , EF.StrUtils
  ;

var
  _JSFormatSettings: TFormatSettings;

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

constructor TJSResponseItem.Create(const ASender: TJSObject; const ARoot: TJSResponseItems);
begin
  inherited Create;
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
  LAddConstruction: Boolean;
begin
  inherited;

  Assert(Assigned(Sender));

  LAddConstruction := not Sender.IsInline and not Sender.IsInternal;
  if LAddConstruction then
  begin
    AFormatter.AddIndentedLine(Sender.JSName + ' = ' + Sender.GetConstructionJS + '(');
    AFormatter.Indent.AddIndent;
  end;
  Sender.JSConfig.FormatTo(AFormatter);
  if LAddConstruction then
  begin
    AFormatter.SkipLine.Outdent;
    AFormatter.AddIndentedLine(');');
    AFormatter.AddIndentedLine(Sender.JSName + '.nm = "' + Sender.JSName + '";');
  end;
end;

{ TExtJSCode }

procedure TExtJSCode.FormatTo(const AFormatter: TJSFormatter);
begin
  inherited;
  AFormatter.AddIndentedLine('');
end;

{ TJSResponseItems }

procedure TJSResponseItems.AddHTML(const AHTML: string);
var
  LItem: TExtHTML;
begin
  LItem := TExtHTML.Create(nil, Self);
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
  LItem: TExtJSON;
begin
  LItem := TExtJSON.Create(nil, Self);
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
  FList := TList<TJSResponseItem>.Create;
  FEmittedItems := TList<TJSResponseItem>.Create;
end;

procedure TJSResponseItems.CallMethod(const AObject: TJSObject; const AMethodName: string;
  const AParams: array of const);
var
  LItem: TExtCallMethod;
begin
  LItem := TExtCallMethod.Create(AObject, Self);
  try
    LItem.AddDependency(FindObjectCreateItem(TJSObject(AObject)));
    LItem.CallName := AMethodName;
    AObject.VarToJSON(AParams,
      procedure(AParam: string; AObjectParam: TObject; AIsFunction: Boolean)
      begin
        LItem.CallParams := LItem.CallParams + [AParam];
        if Assigned(AObjectParam) and (AObjectParam is TJSObject) and not AIsFunction then
          LItem.AddDependency(FindObjectCreateItem(TJSObject(AObjectParam)));
      end);
    FList.Add(LItem);
  except
    FreeAndNil(LItem);
    raise;
  end;
end;

procedure TJSResponseItems.CreateInternalObject(const AObject: TJSObject; const AAttributeName: string);
var
  LObjectCreateItem: TJSCreateObject;
begin
  Assert(Assigned(AObject));
  Assert(Assigned(AObject.Owner));

  LObjectCreateItem := GetObjectCreateItem(AObject.Owner as TJSObject);
  LObjectCreateItem.CreateInternalObject(AAttributeName, AObject);
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

procedure TJSResponseItems.ExecuteJSCode(const AJSCode: string);
begin
  ExecuteJSCode(nil, AJSCode);
end;

procedure TJSResponseItems.ExecuteJSCode(const AObject: TJSObject; const AJSCode: string);
begin
  ExecuteJSCode(AObject, AJSCode, []);
end;

procedure TJSResponseItems.ExecuteJSCode(const AObject: TJSObject; const AJSCode: string;
const AAdditionalDependencies: array of TJSObject);
var
  LItem: TExtJSCode;
  I: Integer;
begin
  if AJSCode <> '' then
  begin
    LItem := TExtJSCode.Create(AObject, Self);
    try
      LItem.AddDependency(FindObjectCreateItem(AObject));
      LItem.JSCode := AJSCode;
      for I := Low(AAdditionalDependencies) to High(AAdditionalDependencies) do
        LItem.AddDependency(FindObjectCreateItem(AAdditionalDependencies[I]));
      FList.Add(LItem);
    except
      FreeAndNil(LItem);
      raise;
    end;
  end;
end;

function TJSResponseItems.AsFormattedString: string;
var
  I: Integer;
  LFormatter: TJSFormatter;
begin
  // SortByDependency;
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

procedure TJSResponseItems.Clear;
begin
  while FList.Count > 0 do
  begin
    FList[0].Free;
    FList.Delete(0);
  end;
end;

function TJSResponseItems.FindObjectCreateItem(const AObject: TJSObject): TJSCreateObject;
var
  LResponseItem: TJSResponseItem;
begin
  Result := nil;
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

function TJSResponseItems.FindLastCodeItem(const AObject: TJSObject): TJSResponseItem;
var
  I: Integer;
begin
  for I := FList.Count - 1 downto 0 do
    if (FList[I].Sender = AObject) and FList[I].IsCode then
      Exit(FList[I]);
  Result := nil;
end;

function TJSResponseItems.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TJSResponseItems.GetItem(I: Integer): TJSResponseItem;
begin
  Result := FList[I];
end;

function TJSResponseItems.GetLastCodeItem(const AObject: TJSObject): TJSResponseItem;
begin
  Result := FindLastCodeItem(AObject);
  if Result = nil then
    raise Exception.CreateFmt('No code item found for object %s.', [AObject.JSName]);
end;

function TJSResponseItems.GetObjectCreateItem(const AObject: TJSObject): TJSCreateObject;
begin
  Assert(Assigned(AObject));

  Result := FindObjectCreateItem(AObject);
  if not Assigned(Result) then
    raise Exception.CreateFmt('Object %s was not created in this request.', [AObject.JSName]);
end;

procedure TJSResponseItems.GetProperty(const AObject: TJSObject; const APropertyName: string);
var
  LItem: TExtGetProperty;
begin
  LItem := TExtGetProperty.Create(AObject, Self);
  try
    LItem.PropertyName := APropertyName;
    LItem.AddDependency(FindObjectCreateItem(AObject));
    FList.Add(LItem);
  except
    FreeAndNil(LItem);
  end;
end;

procedure TJSResponseItems.Remove(const AItem: TJSResponseItem);
begin
  Assert(Assigned(AItem));

  if FList.Remove(AItem) < 0 then
    raise Exception.Create('No item found to remove.');
  AItem.Free;
end;

procedure TJSResponseItems.RemoveAll(const AObject: TJSObject);
var
  I: Integer;
begin
  for I := FList.Count - 1 downto 0 do
    if FList[I].Sender = AObject then
    begin
      FList[I].Free;
      FList.Delete(I);
    end;
end;

procedure TJSResponseItems.SetProperty(const AObject: TJSObject; const APropertyName: string;
const AValues: array of const);
var
  LItem: TExtSetProperty;
begin
  LItem := TExtSetProperty.Create(AObject, Self);
  try
    LItem.AddDependency(FindObjectCreateItem(AObject));
    LItem.PropertyName := APropertyName;
    LItem.PropertyValue := AObject.VarToJSON(AValues);
    AObject.VarToJSON(AValues,
      procedure(AParam: string; AObjectParam: TObject; AIsFunction: Boolean)
      begin
        if Assigned(AObjectParam) and (AObjectParam is TJSObject) and not AIsFunction then
          LItem.AddDependency(FindObjectCreateItem(TJSObject(AObjectParam)));
      end);
    FList.Add(LItem);
  except
    FreeAndNil(LItem);
  end;
end;

{ TExtCallMethod }

procedure TExtCallMethod.FormatTo(const AFormatter: TJSFormatter);
begin
  Assert(Assigned(Sender));
  Assert(FCallName <> '');

  inherited;
  AFormatter.AddIndentedLine(Sender.JSName + '.' + FCallName + '(' + string.Join(',', FCallParams) + ');');;
end;

{ TExtSetProperty }

procedure TExtSetProperty.FormatTo(const AFormatter: TJSFormatter);
begin
  Assert(Assigned(Sender));
  Assert(FPropertyName <> '');
  Assert(FPropertyValue <> '');

  inherited;
  AFormatter.AddIndentedLine(Sender.JSName + '.' + FPropertyName + ' = ' + FPropertyValue + ';');
end;

{ TExtGetProperty }

procedure TExtGetProperty.FormatTo(const AFormatter: TJSFormatter);
begin
  Assert(Assigned(Sender));
  Assert(FPropertyName <> '');

  inherited;
  AFormatter.AddIndentedLine(Sender.JSName + '.' + FPropertyName + ';');
end;

function GetSession: TJSSession;
begin
  Result := TJSSession(_CurrentWebSession);
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
  LObjectName: string;
begin
  LObjectName := Query['Obj'];
  if (LObjectName = '') or (Query['IsEvent'] = '1') then
    Result := inherited GetUrlHandlerObject
  else
    Result := ObjectCatalog.FindObject(LObjectName);
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

procedure TJSSession.ErrorMessage(const AMessage: string; const AAction: TJSFunction);
begin
  ErrorMessage(AMessage, AAction.ExtractJSCommand);
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

procedure TJSSession.Alert(const Msg: string);
begin
  ErrorMessage(Msg)
end;

procedure TJSSession.Refresh;
begin
  inherited;
  ObjectCatalog.FreeAllObjects;
  ResponseItems.Clear;
  FObjectSequences.Clear;
  FSingletons.Clear;
end;

procedure TJSObject.JSSleep(MiliSeconds: Integer);
begin
  JSFunctionFromCodeBlock('sleep(' + IntToStr(MiliSeconds) + ');')
end;

function TJSObject.MethodURI(Method: TJSProcedure; Params: array of const): string;
begin
  Result := MethodURI(Method);
  if Length(Params) <> 0 then
    Result := Result + '&' + FormatParams('TExtObject.MethodURI', Params);
end;

function TJSObject.MethodURI(Method: TJSProcedure): string;
var
  MetName, ObjName: string;
begin
  FindMethod(Method, MetName, ObjName);
  Result := JSSession.MethodURI(MetName);
  if ObjName <> '' then
  begin
    if pos('?', Result) <> 0 then
      Result := Result + '&Obj=' + ObjName
    else
      Result := Result + '?Obj=' + ObjName;
  end;
end;

function TJSObject.MethodURI(const AMethodName: string; const AParams: array of const): string;
begin
  Result := JSSession.MethodURI(AMethodName) + IfThen(Length(AParams) = 0, '', '?' + FormatParams(AMethodName, AParams))
end;

function TJSObject.MethodURI(const AMethodName: string): string;
begin
  Result := JSSession.MethodURI(AMethodName);
end;

function TJSObject.GetObjectNamePrefix: string;
begin
  Result := 'o';
end;

function TJSObject.GetPOSTAjaxCode(const AMethod: TJSProcedure; const AParams: array of const;
const AJsonData: string): string;
var
  LParams: string;
  LMethodName: string;
  LObjectName: string;
begin
  FindMethod(AMethod, LMethodName, LObjectName);
  LParams := IfThen(LObjectName = '', '', 'Obj=' + LObjectName);
  Result := GetPOSTAjaxCode(LMethodName, LParams, AParams, AJsonData);
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

function TJSSession.BranchResponseItems: TJSResponseItems;
begin
  Result := TJSResponseItems.Create;
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
  if Query['IsEvent'] = '1' then
  begin
    LObject := ObjectCatalog.FindObject(Query['Obj']) as TJSObject;
    if not Assigned(LObject) then
      OnError('Object not found in session list. It could be timed out, refresh page and try again', 'HandleEvent', '')
    else
      LObject.HandleEvent(Query['Evt']);
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
  FResponseItemsStack.Push(TJSResponseItems.Create);

  FObjectSequences := TDictionary<string, Cardinal>.Create;
  FSingletons := TDictionary<string, TJSObject>.Create;

  FObjectCatalog := TJSObjectCatalog.Create(nil);
  FObjectCatalog.FSession := Self;
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
    Result := TJSObjectClass(T).CreateSingleton(Self.ObjectCatalog, AName) as T;
    FSingletons.Add(AName, Result);
  end;
end;

{ ExtObjectList }

procedure TJSObjectArray.CreateJSName;
begin
  if Assigned(OwnerJSObject) and (AttributeName <> '') then
    FJSName := OwnerJSObject.JSName + '.' + AttributeName
  else
    FJSName := '';
  Name := '';
end;

// Frees this list and all objects linked in it
destructor TJSObjectArray.Destroy;
begin
  FreeAndNil(FObjects);
  inherited;
end;

function TJSObjectArray.Add(const AObject: TJSObject): Integer;
begin
  Assert(Assigned(AObject));
  Assert(Assigned(OwnerJSObject));
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

function TJSObjectArray.GetOwnerJSObject: TJSObject;
begin
  Result := Owner as TJSObject;
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
  Assert(Assigned(OwnerJSObject));

  Result := FObjects.Add(AObject);
  OwnerJSObject.DependsUpon(AObject);
end;

procedure TJSObjectArray.AfterConstruction;
begin
  inherited;
  FObjects := TObjectList<TJSObject>.Create(False);
end;

{ TJSObject }

procedure TJSObject.CreateJSName;
begin
  if FJSName = '' then
    FJSName := JSSession.GetNextJSName(GetObjectNamePrefix);
  Name := FJSName;
end;

constructor TJSObject.CreateSingleton(const AOwner: TComponent; const AAttributeName: string);
begin
  Assert(Assigned(AOwner));
  inherited Create(AOwner);
  if AAttributeName = '' then
    FJSName := JSClassName
  else
    FJSName := AAttributeName;
  InitDefaults;
end;

function TJSObject.CallMethod(const AName: string; const AValue: Boolean): TJSFunction;
begin
  GetSession.ResponseItems.CallMethod(Self, AName, [AValue]);
  Result := TJSFunction(Self);
end;

function TJSObject.CallMethod(const AName: string): TJSFunction;
begin
  GetSession.ResponseItems.CallMethod(Self, AName, []);
  Result := TJSFunction(Self);
end;

function TJSObject.CallMethod(const AName, AValue: string): TJSFunction;
begin
  GetSession.ResponseItems.CallMethod(Self, AName, [AValue]);
  Result := TJSFunction(Self);
end;

function TJSObject.CallMethod(const AName: string; const AValue: TDateTime): TJSFunction;
begin
  GetSession.ResponseItems.CallMethod(Self, AName, [AValue]);
  Result := TJSFunction(Self);
end;

function TJSObject.CallMethod(const AName: string; const AValue: array of const): TJSFunction;
begin
  GetSession.ResponseItems.CallMethod(Self, AName, AValue);
  Result := TJSFunction(Self);
end;

function TJSObject.CharsToPixels(const AChars: Integer; const AOffset: Integer = 0): TJSFunction;
begin
  // + 16 sort of compensates for text-to-border left and right margins.
  Result := JSFunctionFromExpr(Format('({func0} * %d * 1.2) + %d', [AChars, 16 + AOffset]), [ExtUtilTextMetrics.GetWidth('g')]);
end;

{
  Converts a TExtFormTextArea height in characters to pixels to use in Height property.
  Uses dynamic JS in browser.
  @param Lines TextArea height in characters.
  @return Pixels used by browser to render these Lines
}
function TJSObject.LinesToPixels(const ALines: Integer): TJSFunction;
begin
  Result := JSFunctionFromExpr(Format('{func0} * %d * 1.3', [ALines]), [ExtUtilTextMetrics.GetHeight('W')]);
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

procedure TJSObject.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (AComponent = Self) and (Operation = opRemove) and Assigned(Owner) then
  begin
    // The owner is destroying this object, so we clear anything related from
    // the response. We do this in the destructor for those cases in which a component
    // in destroyed while still being owned, but we must do it here as well
    // for cases in which the owner is freeing its own components (which it
    // does AFTER calling RemoveComponent, thus at a time when it's not the owner
    // anymore.
    GetSession.ResponseItems.RemoveAll(Self);
  end;
end;

// Calls Ext JS <b>destroy()</b> method if it exists else calls the JS <b>delete</b> command
destructor TJSObject.Destroy;
begin
  // See Notification for details.
  if Assigned(Owner) and (GetSession <> nil) and GetSession.HasResponseItems then
    GetSession.ResponseItems.RemoveAll(Self);
  inherited;
end;

function TJSObject.GetAjaxCode(const AMethod: TJSProcedure; const ARawParams: string;
const AParams: array of const): string;
var
  LParams: string;
  LMethodName: string;
  LObjectName: string;
begin
  FindMethod(AMethod, LMethodName, LObjectName);
  LParams := IfThen(LObjectName = '', '', 'Obj=' + LObjectName);
  LParams := LParams + ARawParams;
  Result := GetAjaxCode(LMethodName, LParams, AParams);
end;

function TJSObject.GetAjaxCode(const AMethod: TJSProcedure; const AParams: array of const;
  const AExtraCode: string): string;
var
  LParams: string;
  LMethodName: string;
  LObjectName: string;
begin
  FindMethod(AMethod, LMethodName, LObjectName);
  LParams := IfThen(LObjectName = '', '', 'Obj=' + LObjectName);
  Result := DoGetAjaxCode(LMethodName, LParams, AParams, AExtraCode);
end;

function TJSObject.GetConstructionJS: string;
begin
  Result := 'new ' + JSClassName;
end;

function TJSObject.GetDownloadJS(const AMethod: TJSProcedure; const AParams: array of const): string;
var
  P, LMethodName, LObjectName: string;
begin
  FindMethod(AMethod, LMethodName, LObjectName);
  P := FormatParams(LMethodName, AParams);
  if LObjectName <> '' then
  begin
    if P <> '' then
      P := P + '&';
    P := P + 'Obj=' + LObjectName;
  end;
  if P <> '' then
    P := '?' + P;
  if GetSession.IsMobileApple then
    Result := 'window.open("' + JSSession.MethodURI(LMethodName) + P + '");'
  else
    Result := 'Download.src="' + JSSession.MethodURI(LMethodName) + P + '";';
end;

function TJSObject.GetJSSession(const AOwner: TComponent): TJSSession;
var
  LOwner: TComponent;
begin
  if FSession = nil then
  begin
    LOwner := AOwner;
    while (LOwner <> nil) and (LOwner.Owner <> nil) do
      LOwner := LOwner.Owner;
    if LOwner is TJSObjectCatalog then
      FSession := TJSObjectCatalog(LOwner).Session as TJSSession;
    if FSession = nil then
      raise Exception.CreateFmt('Session not found for object %s of type %s (%s).', [JSName, JSClassName, ClassName]);
  end;
  Result := FSession;
end;

procedure TJSObject.Download(Method: TJSProcedure; Params: array of const);
begin
  GetSession.ResponseItems.ExecuteJSCode(Self, GetDownloadJS(Method, Params));
end;

procedure TJSObject.Download(Method: TJSProcedure);
begin
  Download(Method, []);
end;

{
  Creates a TExtObject and generate corresponding JS code using <link TExtObject.JSCode, Self-translating>
  @param Owner Optional parameter used internally by <link TExtObject.JSObject, JSObject> and <link TExtObject.JSArray, JSArray> only
}
constructor TJSObject.Create(AOwner: TComponent);
begin
  Assert(GetSession <> nil);
  Assert(Assigned(AOwner));
  inherited Create(AOwner);
  FJSConfig := TJSConfig.Create(Self);
  CreateJSName;
  if AttributeName <> '' then
    GetSession.ResponseItems.CreateInternalObject(Self, AttributeName)
  else
    GetSession.ResponseItems.CreateObject(Self);
  InitDefaults;
end;

constructor TJSObject.CreateInternal(const AOwner: TJSObject; const AAttributeName: string);
begin
  Assert(Assigned(AOwner));

  inherited Create(AOwner);
  FAttributeName := AAttributeName;
  FJSConfig := TJSConfig.Create(Self);
  if FAttributeName <> '' then
    FJSName := AOwner.JSName + '.' + FAttributeName
  else
    FJSName := '';
  InitDefaults;
end;

constructor TJSObject.CreateInline(const AOwner: TJSObject);
begin
  CreateInternal(AOwner, '');
end;

constructor TJSObject.CreateInlineAndAddToList(const AList: TJSObjectArray);
begin
  Assert(Assigned(AList));

  CreateInline(AList);
  AList.Add(Self);
end;

class function TJSObject.JSClassName: string;
begin
  Result := 'Ext.Base';
end;

{
  Tests if a class name is parent of this object
  @param CName Class name with "T" prefix
  @return True if CName is parent of this object and false if not
}
function TJSObject.IsInline: Boolean;
begin
  Result := JSName = '';
end;

function TJSObject.IsInternal: Boolean;
begin
  Result := JSName.Contains('.');
end;

function TJSObject.GetJSSession: TJSSession;
begin
  Result := GetJSSession(Owner);
end;

function TJSObject.SetConfigItem(const AName, AValue: string): string;
begin
  FJSConfig.CheckReadOnly(AName);
  FJSConfig.Values.SetString(AName, AValue);
  Result := AValue;
end;

function TJSObject.SetFunctionConfigItem(const AName: string; const AValue: TJSFunction): TJSFunction;
begin
  FJSConfig.CheckReadOnly(AName);
  FJSConfig.SetFunctionValue(AName, AValue);
  Result := AValue;
end;

{
  Converts an array of const to JSON (JavaScript Object Notation) to be used in constructors, JS Arrays or JS Objects
  @param A Array of anytype variables
  @return JSON representation of array
}
function TJSObject.VarToJSON(const AVars: array of const; const ASession: TJSSession): string;
var
  I: Integer;
  LCommand: string;
  LObjectJSName: string;
  LCodeItem: TJSResponseItem;
  LSession: TJSSession;
begin
  Result := '';
  I := 0;

  LSession := ASession;
  if LSession = nil then
    LSession := JSSession;

  while I <= High(AVars) do
  begin
    case AVars[I].VType of
      vtObject:
        begin
          if AVars[I].VObject <> nil then
          begin
            LCodeItem := LSession.ResponseItems.FindLastCodeItem(TJSObject(AVars[I].VObject));
            // Function
            if Assigned(LCodeItem) and AVars[I + 1].VBoolean then
            begin
              Result := Result + TJS.GetJSFunction(LCodeItem.AsFormattedText);
              LSession.ResponseItems.Remove(LCodeItem);
            end
            else
            begin
              // Object
              LObjectJSName := TJSObject(AVars[I].VObject).JSName;
              if Assigned(LCodeItem) then
                LCommand := TJS.RemoveLastJSTerminator(LCodeItem.AsFormattedText)
              else
                LCommand := '';
              Result := Result + LObjectJSName;
            end;
          end
          else
          begin
            if Result = '' then
              Result := 'null'
            else
            begin
              Inc(I, 2);
              Continue;
            end;
          end;
          Inc(I);
        end;
      vtAnsiString:
        Result := Result + TJS.StrToJS(string(AVars[I].VAnsiString));
      vtString:
        Result := Result + TJS.StrToJS(string(AVars[I].VString^));
      vtWideString:
        Result := Result + TJS.StrToJS(string(AVars[I].VWideString));
{$IFDEF UNICODE}
      vtUnicodeString:
        Result := Result + TJS.StrToJS(string(AVars[I].VUnicodeString));
{$ENDIF}
      vtInteger:
        Result := Result + IntToStr(AVars[I].VInteger);
      vtBoolean:
        Result := Result + IfThen(AVars[I].VBoolean, 'true', 'false');
      vtExtended:
        Result := Result + AnsiReplaceStr(FloatToStr(AVars[I].VExtended^), ',', '.');
      vtCurrency:
        Result := Result + CurrToStr(AVars[I].VCurrency^);
      vtInt64:
        Result := Result + IntToStr(AVars[I].VInt64^);
      vtVariant:
        Result := Result + string(AVars[I].VVariant^);
      vtChar:
        Result := Result + string(AVars[I].VChar);
      vtWideChar:
        Result := Result + AVars[I].VWideChar;
    end;
    if I < High(AVars) then
      Result := Result + ',';
    Inc(I);
  end;
  if (Result <> '') and (Result[Length(Result)] = ',') then
    System.Delete(Result, Length(Result), 1);
end;

function TJSObject.VarToJSON(const AVars: array of const; const AProc: TVarToJSONProc): string;
var
  I: Integer;
  LCommand: string;
  LCodeItem: TJSResponseItem;
  LCurrentParam: string;
  LProcCalled: Boolean;
begin
  Result := '';
  I := 0;
  while I <= High(AVars) do
  begin
    LProcCalled := False;
    case AVars[I].VType of
      vtObject:
        begin
          if AVars[I].VObject <> nil then
          begin
            LCodeItem := JSSession.ResponseItems.FindLastCodeItem(TJSObject(AVars[I].VObject));
            if Assigned(LCodeItem) and AVars[I + 1].VBoolean then
            begin
              LCurrentParam := TJS.GetJSFunction(LCodeItem.AsFormattedText);
              Result := Result + LCurrentParam;
              GetSession.ResponseItems.Remove(LCodeItem);
            end
            else
            begin
              LCurrentParam := TJSObject(AVars[I].VObject).JSName;
              if Assigned(LCodeItem) then
                LCommand := TJS.RemoveLastJSTerminator(LCodeItem.AsFormattedText)
              else
                LCommand := '';
              { TODO : Mind the spacing - maybe characterize this type of command with a special class. }
              // if InJSFunction and (Pos(LCurrentParam, Trim(LCommand)) = 1) then
              // ExtSession.ResponseItems.Remove(LCodeItem)
              // else
              Result := Result + LCurrentParam;
            end;
            if Assigned(AProc) then
              AProc(LCurrentParam, AVars[I].VObject, AVars[I + 1].VBoolean);
            LProcCalled := True;
          end
          else
          begin
            LProcCalled := True;
            if Result = '' then
              Result := 'null'
            else
            begin
              Inc(I, 2);
              Continue;
            end;
          end;
          Inc(I);
        end;
      vtAnsiString:
        LCurrentParam := TJS.StrToJS(string(AVars[I].VAnsiString));
      vtString:
        LCurrentParam := TJS.StrToJS(string(AVars[I].VString^));
      vtWideString:
        LCurrentParam := TJS.StrToJS(string(AVars[I].VWideString));
{$IFDEF UNICODE}
      vtUnicodeString:
        LCurrentParam := TJS.StrToJS(string(AVars[I].VUnicodeString));
{$ENDIF}
      vtInteger:
        LCurrentParam := IntToStr(AVars[I].VInteger);
      vtBoolean:
        LCurrentParam := IfThen(AVars[I].VBoolean, 'true', 'false');
      vtExtended:
        LCurrentParam := AnsiReplaceStr(FloatToStr(AVars[I].VExtended^), ',', '.');
      vtCurrency:
        LCurrentParam := CurrToStr(AVars[I].VCurrency^);
      vtInt64:
        LCurrentParam := IntToStr(AVars[I].VInt64^);
      vtVariant:
        LCurrentParam := string(AVars[I].VVariant^);
      vtChar:
        LCurrentParam := string(AVars[I].VChar);
      vtWideChar:
        LCurrentParam := AVars[I].VWideChar;
    end;
    if Assigned(AProc) and not LProcCalled then
    begin
      Result := Result + LCurrentParam;
      AProc(LCurrentParam, nil, False);
    end;
    if I < High(AVars) then
      Result := Result + ',';
    Inc(I);
  end;
  if (Result <> '') and (Result[Length(Result)] = ',') then
    System.Delete(Result, Length(Result), 1);
end;

function TJSObject.RequestDownload(Method: TJSProcedure; Params: array of const): TJSFunction;
begin
  Result := JSFunction(GetDownloadJS(Method, Params));
end;

function TJSObject.SetConfigItem(const AName, AMethodName, AValue: string): string;
begin
  if FJSConfig.IsReadOnly then
    GetSession.ResponseItems.CallMethod(Self, AMethodName, [AValue])
  else
    FJSConfig.Values.SetString(AName, AValue);
  Result := AValue;
end;

function TJSObject.SetConfigItem(const AName, AMethodName: string; const AValue: Integer): Integer;
begin
  if FJSConfig.IsReadOnly then
    GetSession.ResponseItems.CallMethod(Self, AMethodName, [AValue])
  else
    FJSConfig.Values.SetInteger(AName, AValue);
  Result := AValue;
end;

function TJSObject.SetConfigItem(const AName, AMethodName: string; const AValue: Boolean): Boolean;
begin
  if FJSConfig.IsReadOnly then
    GetSession.ResponseItems.CallMethod(Self, AMethodName, [AValue])
  else
    FJSConfig.Values.SetBoolean(AName, AValue);
  Result := AValue;
end;

function TJSObject.RequestDownload(Method: TJSProcedure): TJSFunction;
begin
  Result := RequestDownload(Method, [])
end;

procedure TJSObject.JSCode(JS: string; pJSName: string = ''; pOwner: string = '');
begin
  Assert(False, 'Not implemented - ' + JS);
end;

constructor TJSObject.CreateAndAddToArray(const AArray: TJSObjectArray);
begin
  Assert(Assigned(AArray));

  Create(AArray);
  AArray.Add(Self);
end;

function TJSObject.CreateConfigArray(const AAttributeName: string): TJSObjectArray;
begin
  Result := TJSObjectArray.CreateInternal(Self, AAttributeName);
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
  Result := TJSObjectArray.CreateInternal(Self, 'dummy');
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

function TJSObject.JSFunctionFromExpr(const AExpr: string; const AValues: array of TJSFunction): TJSFunction;
var
  LExpr: string;
  I: Integer;
begin
  LExpr := AExpr;
  for I := Low(AValues) to High(AValues) do
    LExpr := LExpr.Replace('{func' + I.ToString + '}', AValues[I].ExtractJSCommand);
  Result := JSFunctionFromCodeBlock(LExpr);
end;

procedure TJSObject.HandleEvent(const AEvtName: string);
begin
end;

function TJSObject.JSFunction(const AParams, ABody: string): TJSFunction;
var
  LFormatter: TJSFormatter;
begin
  LFormatter := TJSFormatter.Create;
  try
    LFormatter.SkipLine.AddIndent
      .AddIndented('function(').Add(AParams).AddLine(')')
      .Indent.AddIndent.OpenObject
        .AddIndentedLine(ABody)
      .Outdent.AddIndent.CloseObject;
    Result := JSFunctionFromCodeBlock(LFormatter.FormattedText);
  finally
    FreeAndNil(LFormatter);
  end;
end;

function TJSObject.JSFunction(const ABody: string): TJSFunction;
begin
  Result := JSFunction('', ABody);
end;

function TJSObject.JSFunctionFromCodeBlock(const ACode: string): TJSFunction;
begin
  Result := TJSFunction.CreateInline(Self);
  Result.FJSName := ACode;
end;

procedure TJSObject.JSFunction(const AName, AParams, ABody: string);
begin
  GetSession.ResponseItems.ExecuteJSCode(Self, 'function ' + AName + '(' + AParams + ') { ' + ABody + ' };');
end;

function TJSObject.JSFunction(const AMethod: TJSProcedure; const ASilent: Boolean): TJSFunction;
begin
  Result := JSFunction(
    procedure
    begin
      AMethod;
    end, ASilent);
end;

function TJSObject.JSFunction(const AMethod: TProc; const ASilent: Boolean): TJSFunction;
begin
  Result := JSFunction('function() { ' + GetJSFunctionCode(AMethod, ASilent) + ' }');
end;

// Same as above but returns the code as a string instead of executing it.
function TJSObject.GetJSFunctionCode(const AMethod: TProc; const ASilent: Boolean): string;
var
  LResponseItemBranch: TJSResponseItems;
begin
  LResponseItemBranch := JSSession.BranchResponseItems;
  try
    AMethod;
    Result := LResponseItemBranch.Consume;
    if ASilent then
      Result := 'try { ' + Result + ' } catch(e) {};'
  finally
    JSSession.UnbranchResponseItems(LResponseItemBranch, False);
  end;
end;

{
  Invokes an Object Pascal published procedure in AJAX mode.
  To get event parameters use %0, %1 until %9 place holders.<p>
  @param Method Published procedure to invoke
  @return <link TExtFunction> to use in event handlers
  @example <code>
  procedure TSamples.AddTab; begin // published method
  inc(TabIndex);
  with TExtPanel.AddTo(Tabs.Items) do begin
  Title    := 'New Tab ' + IntToStr(TabIndex);
  IconCls  := 'tabs';
  Html     := 'Tab Body ' + IntToStr(TabIndex) + '<br/><br/>blahblah';
  Closable := true;
  if IsAjax then Show;
  Free;
  end;
  end;

  procedure TSamples.AdvancedTabs;
  var
  I : integer;
  begin
  with TExtButton.Create do begin
  RenderTo := 'body';
  Text     := 'Add Tab using AJAX!';
  IconCls  := 'new-tab';
  Handler  := Ajax(AddTab);
  Free;
  end;
  Tabs := TExtTabPanel.Create;
  with Tabs do begin
  RenderTo        := 'body';
  ActiveTabNumber := 0;
  ResizeTabs      := true; // turn on tab resizing
  MinTabWidth     := 115;
  TabWidth        := 135;
  Width           := 600;
  Height          := 150;
  Defaults        := JSObject('autoScroll:true');
  EnableTabScroll := true;
  Plugins         := JSObject('', 'Ext.ux.TabCloseMenu');
  for I := 1 to 7 do AddTab;
  end;
  end;</code>
}
function TJSObject.Ajax(const AMethod: TJSProcedure): TJSFunction;
begin
  Result := Ajax(AMethod, []);
end;

{
  Invokes an Object Pascal published procedure with parameters in AJAX mode.
  To get event parameters use %0 until %9 place holders.<p>
  @param Method Published procedure to invoke
  @param Params Array of Parameters, each parameter is a pair: Name, Value.
  To get them on server side use <link TFCGIThread.Query> array property in AJAX method.
  @return <link TExtFunction> to use in event handlers
  @example <code>
  procedure TSamples.CheckLogin; begin
  if true (*user account verification should be done here*) then
  with TExtWindow.Create do begin
  Title    := 'Login';
  Width    := 380;
  Height   := 140;
  Plain    := true;
  Layout   := 'fit';
  Closable := false;
  with TExtPanel.AddTo(Items) do begin
  Border    := false;
  BodyStyle := 'padding: 5px 8px';
  HTML      := 'Welcome, ' + Query['UserName'] + '.<br/>Password: ' + Query['Password'];
  end;
  Show;
  end
  else
  ExtMessageBox.Alert('Unknown', 'User is not known.');
  end;

  procedure TSamples.Login;
  var
  UserName, Password : TExtFormTextField;
  begin
  FormLogin := TExtWindow.Create;
  with FormLogin do begin
  Title    := 'Login';
  Width    := 380;
  Height   := 140;
  Plain    := true;
  Layout   := 'fit';
  Closable := false;
  with TExtFormFormPanel.AddTo(Items) do begin
  LabelWidth  := 70;
  Border      := false;
  XType       := 'form';
  ButtonAlign := 'right';
  BodyStyle   := 'padding: 10px 15px';
  DefaultType := 'textfield';
  Defaults    := JSObject('width: 250');
  UserName    := TExtFormTextField.Create;
  with UserName.AddTo(Items) do begin
  Name       := 'user';
  FieldLabel := 'Username';
  InputType  := 'textfield';
  end;
  Password := TExtFormTextField.Create;
  with Password.AddTo(Items) do begin
  Name       := 'pass';
  FieldLabel := 'Password';
  InputType  := 'password';
  end;
  with TExtButton.AddTo(Buttons) do begin
  Text    := 'LOGIN';
  Handler := Ajax(CheckLogin, ['UserName', UserName.GetValue, 'Password', Password.GetValue]);
  end;
  end;
  Show;
  end;
  end;
  </code>
}
function TJSObject.Ajax(const AMethod: TJSProcedure; const AParams: array of const): TJSFunction;
var
  LMethodName: string;
  LObjectName: string;
begin
  Result := FindMethod(AMethod, LMethodName, LObjectName);
  AjaxCode(LMethodName, IfThen(LObjectName = '', '', 'Obj=' + LObjectName), AParams, []);
end;

function TJSObject.Ajax(const AMethod: TJSProcedure; const AParams: string;
const AAdditionalDependencies: array of TJSObject; const AIsEvent: Boolean): TJSFunction;
var
  LMethodName: string;
  LObjectName: string;
  LParams: string;
begin
  Result := FindMethod(AMethod, LMethodName, LObjectName);
  LParams := AParams;
  if AIsEvent then
  begin
    LParams := LParams + '&IsEvent=1&Evt=' + LMethodName;
    LMethodName := 'HandleEvent';
  end;
  AjaxCode(LMethodName, AParams + IfThen(LObjectName = '', '', '&Obj=' + LObjectName), [], AAdditionalDependencies);
end;

{
  Discovers the Pascal name and the JavaScript object name for a method
  Raises an exception if method is not published
  @return Self
}
function TJSObject.FindMethod(Method: TJSProcedure; var PascalName, ObjName: string): TJSFunction;
var
  Obj: TObject;
begin
  Obj := TMethod(Method).Data;
  PascalName := Obj.MethodName(@Method);
  if PascalName = '' then
    raise Exception.Create('Ajax: Method is not published')
  else
  begin
    if Obj is TJSObject then
      ObjName := TJSObject(Obj).JSName
    else
      ObjName := '';
    Result := TJSFunction(Self);
  end;
end;

// Ajax with raw string as params
function TJSObject.Ajax(const AMethod: TJSProcedure; const AParams: string): TJSFunction;
var
  AMethodName, AObjectName: string;
begin
  Result := FindMethod(AMethod, AMethodName, AObjectName);
  AjaxCode(AMethodName, '"+' + AParams + IfThen(AObjectName = '', '', '+"&Obj=' + AObjectName), [], []);
end;

function SurroundAjaxParam(Param: string): string;
var
  I: Integer;
begin
  I := pos('%', Param);
  if (I <> 0) and (I <> Length(Param)) then
    if CharInSet(Param[I + 1], ['0' .. '9']) then
      Result := '" + encodeURIComponent(' + Param + ') + "'
    else
      Result := '" + encodeURIComponent(' + Copy(Param, I + 1, Length(Param)) + ') + "'
  else
    Result := Param;
end;

function TJSObject.FormatParams(MethodName: string; Params: array of const): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to high(Params) do
    with Params[I] do
      if Odd(I) then
        case VType of
          vtAnsiString:
            Result := Result + SurroundAjaxParam(string(VAnsiString));
          vtString:
            Result := Result + SurroundAjaxParam(string(VString^));
          vtWideString:
            Result := Result + SurroundAjaxParam(string(VWideString));
{$IFDEF UNICODE}
          vtUnicodeString:
            Result := Result + SurroundAjaxParam(string(VUnicodeString));
{$ENDIF}
          vtObject:
            Result := Result + '"+' + TJSObject(VObject).ExtractJSCommand + '+"';
          vtInteger:
            Result := Result + IntToStr(VInteger);
          vtBoolean:
            Result := Result + IfThen(VBoolean, 'true', 'false');
          vtExtended:
            Result := Result + FloatToStr(VExtended^);
          vtCurrency:
            Result := Result + CurrToStr(VCurrency^);
          vtInt64:
            Result := Result + IntToStr(VInt64^);
          vtVariant:
            Result := Result + string(VVariant^);
          vtChar:
            Result := Result + string(VChar);
          vtWideChar:
            Result := Result + VWideChar;
        end
      else
      begin
        if Result <> '' then
          Result := Result + '&';
        case VType of
          vtAnsiString:
            Result := Result + string(VAnsiString) + '=';
          vtString:
            Result := Result + string(VString^) + '=';
          vtWideString:
            Result := Result + string(VWideString) + '=';
{$IFDEF UNICODE}
          vtUnicodeString:
            Result := Result + string(VUnicodeString) + '=';
{$ENDIF}
          vtChar:
            Result := Result + string(VChar) + '=';
          vtWideChar:
            Result := Result + VWideChar + '=';
        else
          GetSession.ResponseItems.ExecuteJSCode('Ext.Msg.show({title:"Error",msg:"Ajax method: ' + MethodName +
            ' has an invalid parameter name in place #' + IntToStr(I + 1) +
            '",icon:Ext.Msg.ERROR,buttons:Ext.Msg.OK});');
          Exit;
        end;
      end;
end;

procedure TJSObject.AjaxCode(const AMethodName, ARawParams: string; const AParams: array of const;
const AAdditionalDependencies: array of TJSObject);
begin
  GetSession.ResponseItems.ExecuteJSCode(Self, GetAjaxCode(AMethodName, ARawParams, AParams), AAdditionalDependencies);
end;

function TJSObject.DoGetAjaxCode(const AMethodName, ARawParams: string; const AParams: array of const;
  const AExtraCode: string): string;
var
  LParams: string;
  LFormatter: TJSFormatter;
begin
  LParams := ARawParams + IfThen(Length(AParams) > 0,
    IfThen(ARawParams = '', '', '&') + FormatParams(AMethodName, AParams), '');

  { TODO : virtualize ajax code generation }
  LFormatter := TJSFormatter.Create;
  try
    LFormatter.SkipLine.Indent;
    LFormatter.AddIndented('Ext.Ajax.request(').SkipLine.Indent.AddIndent.OpenObject;
    if AExtraCode <> '' then
      LFormatter.AddIndented(AExtraCode); // includes the comma.
    LFormatter.AddIndentedPairLine('url', JSSession.MethodURI(AMethodName));
    LFormatter.AddIndentedPairLine('params', LParams);
    LFormatter.AddIndentedPairLine('success', 'AjaxSuccess', False);
    LFormatter.AddIndentedPairLine('failure', 'AjaxFailure', False);
    LFormatter.DeleteTrailing(',' + sLineBreak);
    LFormatter.CloseObject.SkipLine.Outdent.AddIndentedLine(');');
    Result := LFormatter.FormattedText;
  finally
    FreeAndNil(LFormatter);
  end;
end;

function TJSObject.GetAjaxCode(const AMethodName, ARawParams: string; const AParams: array of const): string;
begin
  Result := DoGetAjaxCode(AMethodName, ARawParams, AParams, 'method: "GET",' + sLineBreak);
end;

function TJSObject.GetPOSTAjaxCode(const AMethodName, ARawParams: string; const AParams: array of const;
  const AJsonData: string): string;
begin
  Result := DoGetAjaxCode(AMethodName, ARawParams, AParams, 'method: "POST",' + sLineBreak + 'jsonData: ' +
    AJsonData + ',' + sLineBreak);
end;

// Internal Ajax generation handler treating IsEvent, when is true HandleEvent will be invoked instead published methods
function TJSObject.Ajax(const AMethodName: string; const AParams: array of const; const AIsEvent: Boolean): TJSFunction;
var
  LParams: string;
  LMethodName: string;
begin
  Result := TJSFunction(Self);
  LMethodName := AMethodName;
  LParams := IfThen(JSName = '', '', 'Obj=' + JSName);
  if AIsEvent then
  begin
    LParams := LParams + '&IsEvent=1&Evt=' + AMethodName;
    LMethodName := 'HandleEvent';
  end;
  AjaxCode(LMethodName, LParams, AParams, []);
end;

function TJSObject.Ajax(const AMethod: TJSProcedure; const AParams: array of const;
const AAdditionalDependencies: array of TJSObject; const AIsEvent: Boolean): TJSFunction;
begin
  GetSession.ResponseItems.ExecuteJSCode(Self, GetAjaxCode(AMethod, AParams, AIsEvent), AAdditionalDependencies);
  Result := TJSFunction(Self);
end;

function TJSObject.GetAjaxCode(const AMethod: TJSProcedure; const AParams: array of const;
const AIsEvent: Boolean): string;
var
  LParams: string;
  LMethodName: string;
  LObjectName: string;
begin
  FindMethod(AMethod, LMethodName, LObjectName);
  LParams := IfThen(LObjectName = '', '', 'Obj=' + LObjectName);
  if AIsEvent then
  begin
    LParams := LParams + '&IsEvent=1&Evt=' + LMethodName;
    LMethodName := 'HandleEvent';
  end;
  Result := GetAjaxCode(LMethodName, LParams, AParams);
end;

function TJSObject.Ajax(const AMethodName: string; const AParams: array of const;
const AAdditionalDependencies: array of TJSObject; const AIsEvent: Boolean): TJSFunction;
var
  LParams: string;
  LMethodName: string;
begin
  Result := TJSFunction(Self);
  LMethodName := AMethodName;
  LParams := IfThen(JSName = '', '', 'Obj=' + JSName);
  if AIsEvent then
  begin
    LParams := LParams + '&IsEvent=1&Evt=' + AMethodName;
    LMethodName := 'HandleEvent';
  end;
  AjaxCode(LMethodName, LParams, AParams, AAdditionalDependencies);
end;

function TJSObject.ExtractJSCommand: string;
var
  LCodeItem: TJSResponseItem;
begin
  LCodeItem := GetSession.ResponseItems.FindLastCodeItem(Self);
  if Assigned(LCodeItem) then
  begin
    Result := TJS.RemoveLastJSTerminator(LCodeItem.AsFormattedText);
    GetSession.ResponseItems.Remove(LCodeItem);
  end
  else
    Result := '';
end;

function TJSObject.VarToJSON(const AList: TJSObjectArray): string;
begin
  Result := AList.JSName;
end;

{
  Converts an <link TArrayOfString, array of strings> to JSON (JavaScript Object Notation) to be used in constructors, JS Arrays or JS Objects
  @param Strs An <link TArrayOfString, array of strings> to convert
  @return JSON representation of Strs
}
function TJSObject.ArrayToJSON(Strs:
{$IF Defined(FPC) or (RTLVersion < 20)}TArrayOfString{$ELSE} array of string{$IFEND}): string;
var
  I: Integer;
begin
  Result := '[';
  for I := 0 to high(Strs) do
  begin
    Result := Result + TJS.StrToJS(Strs[I]);
    if I < high(Strs) then
      Result := Result + ',';
  end;
  Result := Result + ']'
end;

{
  Converts an <link TArrayOfInteger, array of integers> to JSON (JavaScript Object Notation) to be used in constructors, JS Arrays or JS Objects
  @param Ints An <link TArrayOfInteger, array of integers> to convert
  @return JSON representation of Ints
}
function TJSObject.ArrayToJSON(Ints:
{$IF Defined(FPC) or (RTLVersion < 20)}TArrayOfInteger{$ELSE} array of Integer{$IFEND}): string;
var
  I: Integer;
begin
  Result := '[';
  for I := 0 to high(Ints) do
  begin
    Result := Result + IntToStr(Ints[I]);
    if I < high(Ints) then
      Result := Result + ',';
  end;
  Result := Result + ']'
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
  Result := TJSObject(JSSession.ObjectCatalog.FindObject(JSSession.Query[ParamName]));
end;

function TJSObject.FindJSObject(const AJSName: string): TObject;
var
  I: Integer;
begin
  Assert(AJSName <> '');

  Result := nil;
  for I := 0 to ComponentCount - 1 do
  begin
    if Components[I] is TJSObject then
    begin
      if TJSObject(Components[I]).JSName = AJSName then
        Result := TJSObject(Components[I])
      else
        Result := TJSObject(Components[I]).FindJSObject(AJSName);
      if Assigned(Result) then
        Break;
    end;
  end;
end;

{ TExtTextBase }

procedure TExtTextBase.FormatTo(const AFormatter: TJSFormatter);
begin
  inherited;
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

function TJSObject.SetFunctionConfigItem(const AName, AMethodName: string; const AValue: TJSFunction): TJSFunction;
begin
  if FJSConfig.IsReadOnly then
    GetSession.ResponseItems.CallMethod(Self, AMethodName, [AValue, True])
  else
    FJSConfig.SetFunctionValue(AName, AValue);
  Result := AValue;
end;

function TJSObject.SetConfigItem(const AName, AMethodName: string; const AValue: TJSObject): TJSObject;
begin
  if FJSConfig.IsReadOnly then
    GetSession.ResponseItems.CallMethod(Self, AMethodName, [AValue])
  else
    FJSConfig.Values.SetObject(AName, AValue);
  Result := AValue;
end;

function TJSObject.SetConfigItemOrProperty(const AName: string; const AValue: Boolean): Boolean;
begin
  if FJSConfig.IsReadOnly then
    GetSession.ResponseItems.SetProperty(Self, AName, [AValue])
  else
    FJSConfig.Values.SetBoolean(AName, AValue);
  Result := AValue;
end;

function TJSObject.SetProperty(const AName: string; const AValue: TJSFunction): TJSFunction;
begin
  GetSession.ResponseItems.SetProperty(Self, AName, [AValue, True]);
  Result := AValue;
end;

function TJSObject.SetProperty(const AName: string; const AValue: TJSObject): TJSObject;
begin
  GetSession.ResponseItems.SetProperty(Self, AName, [AValue, False]);
  Result := AValue;
end;

function TJSObject.SetProperty(const AName: string; const AValue: Boolean): Boolean;
begin
  GetSession.ResponseItems.SetProperty(Self, AName, [AValue]);
  Result := AValue;
end;

function TJSObject.SetProperty(const AName, AValue: string): string;
begin
  GetSession.ResponseItems.SetProperty(Self, AName, [AValue]);
  Result := AValue;
end;

function TJSObject.SetProperty(const AName: string; const AValue: Integer): Integer;
begin
  GetSession.ResponseItems.SetProperty(Self, AName, [AValue]);
  Result := AValue;
end;

function TJSObject.SetConfigItemOrProperty(const AName, AValue: string): string;
begin
  if FJSConfig.IsReadOnly then
    GetSession.ResponseItems.SetProperty(Self, AName, [AValue])
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
    GetSession.ResponseItems.CallMethod(Self, AMethodName, [AValue])
  else
    FJSConfig.Values.SetDateTime(AName, AValue);
  Result := AValue;
end;

function TJSObject.SetProperty(const AName: string; const AValue: TDateTime): TDateTime;
begin
  GetSession.ResponseItems.SetProperty(Self, AName, [AValue]);
  Result := AValue;
end;

function TJSObject.CallFunctionMethod(const AName: string; const AValue: TJSFunction): TJSFunction;
begin
  GetSession.ResponseItems.CallMethod(Self, AName, [AValue, True]);
  Result := TJSFunction(Self);
end;

function TJSObject.CallMethod(const AName: string; const AValue: TJSObject): TJSFunction;
begin
  GetSession.ResponseItems.CallMethod(Self, AName, [AValue, False]);
  Result := TJSFunction(Self);
end;

{ TJSObjectCatalog }

function TJSObjectCatalog.FindObject(const AJSName: string): TObject;
var
  I: Integer;
begin
  Assert(AJSName <> '');

  Result := FindComponent(AJSName);
  if not Assigned(Result) then
  begin
    for I := 0 to ComponentCount - 1 do
    begin
      if SameText(Components[I].Name, AJSName) then
        Result := Components[I]
      else if Components[I] is TJSObject then
        Result := TJSObject(Components[I]).FindJSObject(AJSName)
      else
        Result := Components[I].FindComponent(AJSName);
      if Assigned(Result) then
        Break;
    end;
  end;
end;

procedure TJSObjectCatalog.FreeAllObjects;
var
  I: Integer;
begin
  for I := ComponentCount - 1 downto 0 do
    Components[I].Free;
  Assert(ComponentCount = 0);
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

class function TJS.GetJSFunction(const ACode: string): string;
var
  I, J: Integer;
  LParams: string;
  LCommandWithoutTerminator: string;
  LCode: string;
begin
  LCode := ACode;
  LParams := '';
  J := -1;
  I := Pos('%', LCode);
  while I <> 0 do
  begin
    if CharInSet(LCode[I + 1], ['0' .. '9']) then
    begin
      LCode[I] := 'P';
      J := max(J, StrToInt(LCode[I + 1]));
    end;
    I := posex('%', LCode, I);
  end;
  for I := 0 to J do
  begin
    LParams := LParams + 'P' + IntToStr(I);
    if I <> J then
      LParams := LParams + ','
  end;
  LCommandWithoutTerminator := TJS.RemoveLastJSTerminator(LCode);
  I := LastDelimiter(';', LCommandWithoutTerminator);
  if (I = 0) and (pos('return ', LCode) <> 1) then
    LCode := 'return ' + LCode
  else
  begin
    Inc(I);
    while (Length(LCode) > I) and CharInSet(LCode[I], [#13, #10]) do
      Inc(I);
    insert('return ', LCode, I);
  end;
  Result := 'function(' + LParams + '){' + LCode + '}';
end;

{ TJSConfig }

procedure TJSConfig.AfterConstruction;
begin
  inherited;
  FValues := TEFTree.Create;
  FIsReadOnly := False;
end;

procedure TJSConfig.CheckReadOnly(const AValueName: string);
begin
  if FIsReadOnly then
    raise Exception.CreateFmt('Cannot set config value %s. Object was created in a different request.', [AValueName]);
end;

destructor TJSConfig.Destroy;
begin
  FreeAndNil(FValues);
  inherited;
end;

function TJSConfig.IsFunction(const AValue: TEFNode): Boolean;
begin
  Result := AValue.GetBoolean('IsFunction');
end;

function TJSConfig.IsObject(const AValue: TEFNode): Boolean;
begin
  Result := AValue.DataType is TEFObjectDataType;
end;

function TJSConfig.IsObjectArray(const AValue: TEFNode): Boolean;
begin
  Result := IsObject(AValue) and (AValue.AsObject is TJSObjectArray);
end;

procedure TJSConfig.SetFunctionValue(const AName: string; const AValue: TJSFunction);
var
  LNode: TEFNode;
  LCodeItem: TJSResponseItem;
begin
  CheckReadOnly(AName);
  LNode := FValues.GetNode(AName, True);

{ TODO : If and when all function calls generate autonomous objects,
  amend this code to extract the js code (perhaps in JSName) and then free the object }
  LCodeItem := JSSession.ResponseItems.FindLastCodeItem(AValue);
  if Assigned(LCodeItem) then
  begin
    LNode.AsString := TJS.GetJSFunction(LCodeItem.AsFormattedText);
    JSSession.ResponseItems.Remove(LCodeItem);
  end;
  LNode.SetBoolean('IsFunction', True);
end;

procedure TJSConfig.FormatTo(const AFormatter: TJSFormatter);
var
  I: Integer;
  LValue: TEFNode;
  LAdded: Boolean;

  procedure AppendValue(var AArray: TArray<string>; const AValue: string);
  begin
    if AValue <> '' then
      AArray := AArray + [AValue];
  end;

  function FormatFunction(const ANode: TEFNode): Boolean;
  begin
    AFormatter.AddIndentedPair(ANode.Name, ANode.AsString, False, False);
    Result := True;
  end;

  function FormatObjectConfig(const AName: string; const AObject: TJSObject): Boolean;
  begin
    Result := True;
    if AObject.IsInline then
    begin
      if AName <> '' then
        AFormatter.AddIndented(AName + ': ');
      AObject.JSConfig.FormatTo(AFormatter);
    end
    else if not AObject.IsInternal then
    begin
      if AName <> '' then
        AFormatter.AddIndented(AName + ': ');
      AFormatter.AddIndentedLine(AObject.JSName);
    end
    else
      Result := False;
  end;

  function FormatObjectArrayConfig(const AName: string; const AObjectList: TJSObjectArray): Boolean;
  var
    LObjectIndex: Integer;
  begin
    if AObjectList.Count > 0 then
    begin
      AFormatter.AddIndented(AName + ': ');
      AFormatter.OpenArray.AddIndent;
      for LObjectIndex := 0 to AObjectList.Count - 1 do
      begin
        if FormatObjectConfig('', AObjectList[LObjectIndex]) then
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
      AFormatter.AddIndented(ANode.Name + ': ' + LString);
      Result := True;
    end;
  end;

begin
  AFormatter.OpenObject;
  for I := 0 to Values.ChildCount - 1 do
  begin
    LValue := Values.Children[I];
    if IsFunction(LValue) then
      LAdded := FormatFunction(LValue)
    else if IsObjectArray(LValue) then
      LAdded := FormatObjectArrayConfig(LValue.Name, LValue.AsObject as TJSObjectArray)
    else if IsObject(LValue) then
      LAdded := FormatObjectConfig(LValue.Name, LValue.AsObject as TJSObject)
    else
      LAdded := FormatSimpleValueConfig(LValue);
    if LAdded then
      AFormatter.Add(',').SkipLine;
  end;
  AFormatter.DeleteTrailing(',' + sLineBreak);
  AFormatter.CloseObject;
end;

function TJSConfig.GetJSSession: TJSSession;
begin
  Assert(Assigned(Owner));
  Assert(Owner is TJSObject);

  Result := TJSObject(Owner).JSSession;
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
  const AQuoteValue, AAddComma: Boolean): TJSFormatter;
begin
  Result := AddIndented(AName + ': ' + IfThen(AQuoteValue, '"', '') + AStrValue + IfThen(AQuoteValue, '"', '')
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

function TJSFormatter.Outdent: TJSFormatter;
begin
  Dec(FCurrentIndent);
  Result := Self;
end;

function TJSFormatter.SkipLine: TJSFormatter;
begin
  Result := Add(sLineBreak);
end;

initialization

_JSFormatSettings := TFormatSettings.Create;
_JSFormatSettings.DecimalSeparator := '.';

end.
