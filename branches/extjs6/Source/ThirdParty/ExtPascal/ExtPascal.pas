{
Classes to JavaScript and Ext JS translating from Object Pascal.
Associates semantic concepts of JavaScript and Ext JS for Object Pascal, such as: Function, Object, List of Objects and Ajax Method.
It's the heart of the automatic translation method from Object Pascal to JavaScript, that I call "Self-translating".
It takes advantage of the fact that JavaScript and Object Pascal are structurally similar languages,
where there is almost one to one parity between its syntax and semantic structures.
-ExtPascal is composed of four main components:-
1. The <color red>Parser</color> (<link ExtToPascal.dpr>) able to scan Ext JS documentation in HTML format and to create the <color red>Wrapper</color>.
2. The <color red>Wrapper</color> programmatically created by Parser. It's in fact a set of units (twelve in Ext JS 2.1), which has the definition of all Ext JS classes, properties, methods and events.
3. The <color red>Self-translating</color> engine, <link ExtPascal.pas, ExtPascal unit>. It's triggered when using the <color red>Wrapper</color>, ie by creating objects, assigning properties and events, and invoking methods.
4. The <link FCGIApp.pas, FastCGI> multithread environment. It implements FastCGI protocol using TCP/IP Sockets and statefull, keep-alive, multithread web application behavior.
<image extpascal>
1. The <color red>Parser</color> reads the HTML documentation of Ext JS,
2. Reads ExtFixes.txt file to fix documentation faults and omissions, and
3. Generates the <color red>Wrapper</color>.
4. With the application running, a browser session does a HTTP request to the Web Server.
5. The Web Server does a FastCGI request to the application that creates a <color red>thread</color> to handle the request.
6. The <color red>thread</color> creates ExtObjects, set properties and call methods from <color red>Wrapper</color> Units.
7. For each these tasks the <color red>Self-translating</color> is invoked
8. Generating JavaScript code that uses Ext JS classes.
9. At end of request processing, the <color red>thread</color> reads and formats all JS generated
10. And sends the response to browser session. New requests can be done begining from step 4.
So the translating is not focused on JavaScript language, but to access widget frameworks made in JavaScript.
In this way the use of (X)HTML, JavaScript and CSS is minimum.
Indeed the <color red>Parser</color> can be adapted to read the documentation of another JavaScript framework, Dojo for example.

ExtPascal has one optional fifth component the <link CGIGateway.dpr>.

Author: Wanderlan Santos dos Anjos, wanderlan.anjos@gmail.com
Date: jun-2008
License: <extlink http://www.opensource.org/licenses/bsd-license.php>BSD</extlink>
}

unit ExtPascal;

//@@Overview
//<copy ExtPascal.pas>

// Enabling SERVICE directive ExtPascal application can be used as a Windows Service,
// In command line use -INSTALL to install the service and - UNINSTALL to uninstall the service
{$IFDEF MSWINDOWS}
{.$DEFINE SERVICE}
{$ENDIF}

// Enabling WebServer directive ExtPascal can be used within an embedded Indy based WebServer
{$IFNDEF SERVICE}
{.$DEFINE WebServer}
{$ENDIF}

// Uses ext-all-debug.js, format all JS/CSS source code to facilitate debugging on browser and locates line error if using Firefox on AJAX responses
{.$DEFINE DEBUGJS}

// Uses CacheFly for performance boost see: http://extjs.com/blog/2008/11/18/ext-cdn-custom-builds-compression-and-fast-performance/
{.$DEFINE CacheFly}

// Uses DebugExtJS to load ext-all-debug.js library instead ext-all.js to debugging purposes
{.$DEFINE DebugExtJS}

interface

uses
  Generics.Collections, SysUtils,
  {$IFNDEF WebServer}FCGIApp{$ELSE}IdExtHTTPServer{$ENDIF},
  Classes, ExtPascalUtils;

type
  TArrayOfString  = array of string;
  TArrayOfInteger = array of Integer;
  TExtObjectList  = class;
  TExtFunction    = class;
  TExtObject = class;

  TExtResponseItem = class;
  TExtResponseItemClass = class of TExtResponseItem;

  TExtCreateObject = class;

  TExtResponseItems = class
  private
    FList: TList<TExtResponseItem>;
    FEmittedItems: TList<TExtResponseItem>;
    function FindObjectCreateItem(const AObject: TExtObject): TExtCreateObject;
    function GetObjectCreateItem(const AObject: TExtObject): TExtCreateObject;
    //procedure SortByDependency;
    function GetCount: Integer;
    function GetItem(I: Integer): TExtResponseItem;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
  public
    // Create an object.
    procedure CreateObject(const AObject: TExtObject);

    // Create an internal object; could be a config items of an object type.
    procedure CreateInternalObject(const AObject: TExtObject; const AAttributeName: string);

    // Set a config item, set a same-named property if not possible.
    // AValues must be either a single value of any type or an object value followed by the IsFunction boolean flag.
    procedure SetConfigItemOrProperty(const AObject: TExtObject; const AItemName: string; const AValues: array of const);

    // Set a config item, raise an exception if not possible.
    // AValues must be either a single value of any type or an object value followed by the IsFunction boolean flag.
    procedure SetConfigItem(const AObject: TExtObject; const AItemName: string; const AValues: array of const); overload;
    // Set a config item, call a method if not possible.
    // AValues must be either a single value of any type or an object value followed by the IsFunction boolean flag.
    procedure SetConfigItem(const AObject: TExtObject; const AItemName, AMethodName: string; const AValues: array of const); overload;

    // Add an object to a list (declare the list if not existing).
    // AValues must be an object value followed by the IsFunction boolean flag.
    procedure AddToList(const AContainer: TExtObject; const AItemName: string; const AValues: array of const); overload;
    // Add an object to a list (declare the list if not existing), call a method if not possible.
    // AValues must be an object value followed by the IsFunction boolean flag.
    procedure AddToList(const AContainer: TExtObject; const AList: TExtObjectList;
      const AItemName, AMethodName: string; const AValues: array of const); overload;

    procedure CallMethod(const AObject: TExtObject; const AMethodName: string;
      const AParams: array of const); overload;

    procedure GetProperty(const AObject: TExtObject; const APropertyName: string);
    // AValues must be either a single value of any type or an object value followed by the IsFunction boolean flag.
    procedure SetProperty(const AObject: TExtObject; const APropertyName: string; const AValues: array of const);

    procedure ExecuteJSCode(const AJSCode: string); overload;
    procedure ExecuteJSCode(const AObject: TExtObject; const AJSCode: string); overload;
    procedure ExecuteJSCode(const AObject: TExtObject; const AJSCode: string;
      const AAdditionalDependencies: array of TExtObject); overload;

    procedure AddJSON(const AJSON: string);

    procedure AddHTML(const AHTML: string);

    function ToString: string; override;
    function Consume: string;

    // Remove any items sent by the specified object.
    procedure RemoveAll(const AObject: TExtObject);

    procedure Remove(const AItem: TExtResponseItem);

    // Returns the first item with IsCode=True for the specified object, or nil.
    function FindLastCodeItem(const AObject: TExtObject): TExtResponseItem;
    // Returns the first item with IsCode=True for the specified object, or
    // raises an exception.
    function GetLastCodeItem(const AObject: TExtObject): TExtResponseItem;

    property Items[I: Integer]: TExtResponseItem read GetItem; default;
    property Count: Integer read GetCount;
    procedure Clear;
  end;

  TExtResponseItem = class
  private
    FParent: TExtResponseItems;
    FSender: TExtObject;
    FDependencies: TList<TExtResponseItem>;
    FEmitted: Boolean;
    FCreationDateTime: TDateTime;
    function GetDependencyCount: Integer;
    function GetDependency(I: Integer): TExtResponseItem;
    function AllDependenciesEmitted(
      const AEmittedItems: TList<TExtResponseItem>): Boolean;
  public
    constructor Create(const AParent: TExtResponseItems; const ASender: TExtObject); virtual;
    procedure AfterConstruction; override;
    destructor Destroy; override;
    property Sender: TExtObject read FSender;
    procedure AddDependency(const AItem: TExtResponseItem);
    procedure RemoveDependency(const AItem: TExtResponseItem);
    function GetDependencies: TArray<TExtResponseItem>;
    property DependencyCount: Integer read GetDependencyCount;
    property Dependencies[I: Integer]: TExtResponseItem read GetDependency;
    function DependsOn(const AItem: TExtResponseItem): Boolean;
    function IsCode: Boolean; virtual;

    procedure Emit(const AEmittedItems: TList<TExtResponseItem>);
    procedure UnEmit;
  end;

  TExtCreateObject = class(TExtResponseItem)
  private
    FConfigItems: TStrings;
    function ConfigItemValuesAsString(const AIndex: Integer): string;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;

    function ToString: string; override;

    procedure SetConfigItem(const AName: string; const AValues: array of const);
    // Creates an empty object-type config item.
    procedure InitObjectConfigItem(const AName: string);

    procedure AddToConfigItem(const AName: string; const AValues: array of const);
    function IsCode: Boolean; override;
  end;

  TExtCallMethod = class(TExtResponseItem)
  private
    FCallName: string;
    FCallParams: TStrings;
    procedure SetCallParams(const AValue: TStrings);
  public
    function ToString: string; override;
    procedure AfterConstruction; override;
    destructor Destroy; override;
    property CallName: string read FCallName write FCallName;
    property CallParams: TStrings read FCallParams write SetCallParams;
  end;

  TExtPropertyBase = class abstract(TExtResponseItem)
  private
    FPropertyName: string;
  public
    property PropertyName: string read FPropertyName write FPropertyName;
  end;

  TExtGetProperty = class(TExtPropertyBase)
  public
    function ToString: string; override;
  end;

  TExtSetProperty = class(TExtPropertyBase)
  private
    FPropertyValue: string;
  public
    function ToString: string; override;
    property PropertyValue: string read FPropertyValue write FPropertyValue;
  end;

  TExtTextBase = class(TExtResponseItem)
  protected
    FText: string;
  public
    property Text: string read FText write FText;
    function ToString: string; override;
  end;

  TExtJSCode = class(TExtTextBase)
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
    property JSCode: string read FText write FText;
    function ToString: string; override;
  end;

  TExtJSON = class(TExtTextBase)
  public
    property JSON: string read FText write FText;
  end;

  TExtHTML = class(TExtTextBase)
  public
    property HTML: string read FText write FText;
  end;

  TVarToJSONProc = TProc<string, TObject, Boolean>;

  TExtSession = class;

  {
  Ancestor of all classes and components of Ext JS framework.
  Each TExtObject has the capability to self-translate to JavaScript during the program execution.
  When a property is assigned or a method is called the <link TExtSession.JSCode, Self-translating> enter in action
  translating these Object Pascal commands to JavaScript.
  }
  TExtObject = class(TComponent)
  private
    // Assigned if the object was created with CreateInternal.
    FExtObjectOwner: TExtObject;
    // Assigned if the object was created with CreateInternal.
    FAttributeName: string;
    FSession: TExtSession;
    function  WriteFunction(Command : string): string;
    //function  GetJSCommand : string;
    //procedure SetJSCommand(const Value : string);
    //function  PopJSCommand : string;
    function FormatParams(MethodName : string; Params : array of const): string;
    procedure AjaxCode(const AMethodName, ARawParams: string; const AParams: array of const;
      const AAdditionalDependencies: array of TExtObject);
    function AddJSReturn(Expression : string; MethodsValues : array of const): string;
    function FindMethod(Method : TExtProcedure; var PascalName, ObjName : string) : TExtFunction;
    function GetDownloadJS(Method: TExtProcedure; Params: array of const): string;
    function DoGetAjaxCode(const AMethodName, ARawParams: string; const AParams: array of const;
      const AExtraCode: string): string;
  protected
    // Set by some classes with custom constructors that need to call the
    // inherited Create with a custom string to be passed to CreateVar.
    FCreateVarArgs: string;
    FJSName    : string;  // Internal JavaScript name generated automatically by <link TExtObject.CreateJSName, CreateJSName>
    function GetExtSession: TExtSession; overload;
    function GetExtSession(const AOwner: TComponent): TExtSession; overload;
    function ExtractJSCommand : string;
    function IsParent(CName : string): boolean;
    function VarToJSON(const AVars: array of const; const ASession: TExtSession = nil): string; overload;
    function VarToJSON(const AVars: array of const; const AProc: TVarToJSONProc): string; overload;
    function VarToJSON(const AList: TExtObjectList): string; overload;
    function ArrayToJSON(Strs : {$IF Defined(FPC) or (RTLVersion < 20)}TArrayOfString{$ELSE}array of string{$IFEND}) : string; overload;
    function ArrayToJSON(Ints : {$IF Defined(FPC) or (RTLVersion < 20)}TArrayOfInteger{$ELSE}array of integer{$IFEND}) : string; overload;
    function ParamAsInteger(ParamName : string) : integer;
    function ParamAsDouble(ParamName : string) : double;
    function ParamAsBoolean(ParamName : string) : boolean;
    function ParamAsString(ParamName : string) : string;
    function ParamAsTDateTime(ParamName : string) : TDateTime;
    function ParamAsObject(ParamName : string) : TExtObject;
    procedure CreateVar(AJSCode : string); virtual;
    procedure CreateJSName; virtual;
    function GetObjectNamePrefix: string; virtual;
    procedure InitDefaults; virtual;
    procedure HandleEvent(const AEvtName : string); virtual;
    property ExtSession: TExtSession read GetExtSession;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor CreateInternal(const AOwner: TExtObject; const AAttributeName: string); virtual;

    constructor Create(AOwner: TComponent); override;
    constructor CreateInline(AOwner: TComponent); virtual;

    constructor CreateWithConfig(AOwner: TComponent; AConfig: TExtObject = nil);
    constructor CreateSingleton(const AOwner: TComponent; const AAttribute: string = '');
    constructor CreateAndAddTo(List: TExtObjectList);
    constructor CreateInlineAndAddTo(List: TExtObjectList);

    constructor Init(AOwner: TComponent; AMethod: TExtFunction); overload;
    constructor Init(const AOwner: TComponent; const ACommand: string); overload;

    property ExtObjectOwner: TExtObject read FExtObjectOwner;
    property AttributeName: string read FAttributeName;

    function GetConstructionJS: string; virtual;

    procedure AddTo(List: TExtObjectList);

    function DestroyJS: TExtFunction; virtual;
    procedure Free(CallDestroyJS : boolean = false);
    procedure Delete;
    class function JSClassName : string; virtual;
    function JSArray(const AJSON: string; const ASquareBrackets: Boolean = True): TExtObjectList;
    function JSObject(const AJSON: string; const AObjectConstructor: string = '';
      const ACurlyBrackets: Boolean = True): TExtObject;
    function  JSFunction(const AParams, ABody: string): TExtFunction; overload;
    procedure JSFunction(const AName, AParams, ABody: string); overload;
    function JSFunction(const ABody: string): TExtFunction; overload;
    function JSFunction(Method : TExtProcedure; Silent : boolean = false) : TExtFunction; overload;
    function JSFunction(const AMethod: TProc; const ASilent: Boolean = False): TExtFunction; overload;
    function JSFunctionInline(const ACode: string): TExtFunction;
    function GetJSFunctionCode(const AMethod: TProc; const ASilent: Boolean = False): string;
    function JSExpression(Expression : string; MethodsValues : array of const) : integer; overload;
    function JSExpression(Method : TExtFunction) : integer; overload;
    function JSString(Expression : string; MethodsValues : array of const) : string; overload;
    function JSString(Method : TExtFunction) : string; overload;
    function JSMethod(Method : TExtFunction) : string;
    procedure JSCode(JS : string; pJSName : string = ''; pOwner : string = '');
    procedure JSSleep(MiliSeconds: integer);

    function Ajax(const AMethod: TExtProcedure; const AParams: string): TExtFunction; overload;

    function Ajax(const AMethod: TExtProcedure; const AParams: string;
      const AAdditionalDependencies: array of TExtObject;
      const AIsEvent: Boolean = False): TExtFunction; overload;

    function Ajax(const AMethod: TExtProcedure; const AParams: array of const;
      const AAdditionalDependencies: array of TExtObject;
      const AIsEvent: Boolean = False): TExtFunction; overload;

    function Ajax(const AMethodName: string; const AParams: array of const;
      const AAdditionalDependencies: array of TExtObject;
      const AIsEvent: Boolean = False): TExtFunction; overload;

    function Ajax(const AMethodName: string; const AParams: array of const;
      const AIsEvent: Boolean = False): TExtFunction; overload;

    function Ajax(const AMethod: TExtProcedure): TExtFunction; overload;

    function Ajax(const AMethod: TExtProcedure; const AParams: array of const): TExtFunction; overload;

    function AjaxExtFunction(const AMethod: TExtProcedure; const AParams: array of TExtFunction): TExtFunction; overload;
    function AjaxSelection(const AMethod: TExtProcedure; const ASelectionModel: TExtObject;
      const AAttributes, ATargetQueries: string; const AParams: array of const): TExtFunction;
    function AjaxForms(const AMethod: TExtProcedure; const AForms: array of TExtObject): TExtFunction;

    // Use these to generate and return js code that performs an ajax call, useful
    // when building js handlers. These methods DO NOT add any code to the
    // current response.
    function GetAjaxCode(const AMethod: TExtProcedure; const AParams: array of const;
      const AExtraCode: string): string; overload;
    function GetAjaxCode(const AMethodName, ARawParams: string;
      const AParams: array of const): string; overload;
    function GetAjaxCode(const AMethod: TExtProcedure;
      const AParams: array of const; const AIsEvent: Boolean = False): string; overload;
    function GetAjaxCode(const AMethod: TExtProcedure; const ARawParams: string;
      const AParams: array of const): string; overload;
    // Ajax calls with the POST method that passes JSON data in the jsonData option of
    // Ext.Ajax.request. AJsonData can be js code, such as a function call, or a streamed json object.
    function GetPOSTAjaxCode(const AMethodName, ARawParams: string;
      const AParams: array of const; const AJsonData: string): string; overload;
    function GetPOSTAjaxCode(const AMethod: TExtProcedure;
      const AParams: array of const; const AJsonData: string): string; overload;

    function RequestDownload(Method : TExtProcedure) : TExtFunction; overload;
    function RequestDownload(Method : TExtProcedure; Params : array of const) : TExtFunction; overload;
    procedure Download(Method : TExtProcedure); overload;
    procedure Download(Method : TExtProcedure; Params : array of const); overload;
    function MethodURI(Method : TExtProcedure; Params : array of const) : string; overload;
    function MethodURI(Method : TExtProcedure) : string; overload;
    function MethodURI(MethodName : string; Params : array of const) : string; overload;
    function MethodURI(MethodName : string) : string; overload;
    {
    Converts a TExtFormField length in characters to pixels to use in Width property.
    Uses dynamic JS in browser.
    @param Chars Field length in characters
    @return Pixels used by browser to render these Chars
    }
    function CharsToPixels(const AChars: Integer; const AOffset: Integer = 0): Integer;
    function LinesToPixels(const ALines: Integer): Integer;
    destructor Destroy; override;
    property JSName: string read FJSName; // JS variable name to this object, it's created automatically when the object is created
    function FindExtObject(const AJSName: string): TObject;

    procedure SetConfigItem(const AName, AMethodName: string; const AValue: array of const); overload;
    function SetConfigItem(const AName, AMethodName: string; const AValue: string): string; overload;
    function SetConfigItem(const AName, AMethodName: string; const AValue: Boolean): Boolean; overload;
    function SetConfigItem(const AName, AMethodName: string; const AValue: Integer): Integer; overload;
    function SetConfigItem(const AName, AMethodName: string; const AValue: TDateTime): TDateTime; overload;
    function SetConfigItem(const AName, AMethodName: string; const AValue: TExtObject): TExtObject; overload;
    function SetFunctionConfigItem(const AName, AMethodName: string; const AValue: TExtFunction): TExtFunction; overload;

    function SetConfigItem(const AName, AValue: string): string; overload;
    function SetConfigItem(const AName: string; const AValue: TExtObject): TExtObject; overload;
    function SetConfigItem(const AName: string; const AValue: Integer): Integer; overload;
    function SetConfigItem(const AName: string; const AValue: Boolean): Boolean; overload;
    function SetConfigItem(const AName: string; const AValue: Double): Double; overload;
    function SetFunctionConfigItem(const AName: string; const AValue: TExtFunction): TExtFunction; overload;

    function SetConfigItemOrProperty(const AName, AValue: string): string; overload;
    function SetConfigItemOrProperty(const AName: string; const AValue: Boolean): Boolean; overload;

    function SetProperty(const AName: string; const AValue: Integer): Integer; overload;
    function SetProperty(const AName, AValue: string): string; overload;
    function SetProperty(const AName: string; const AValue: TExtFunction): TExtFunction; overload;
    function SetProperty(const AName: string; const AValue: TExtObject): TExtObject; overload;
    function SetProperty(const AName: string; const AValue: Boolean): Boolean; overload;
    function SetProperty(const AName: string; const AValue: TDateTime): TDateTime; overload;

    function CallMethod(const AName: string; const AValue: Boolean): TExtFunction; overload;
    function CallMethod(const AName, AValue: string): TExtFunction; overload;
    function CallMethod(const AName: string; const AValue: TDateTime): TExtFunction; overload;
    function CallMethod(const AName: string): TExtFunction; overload;
    function CallMethod(const AName: string; const AValue: array of const): TExtFunction; overload;
    function CallMethod(const AName: string; const AValue: TExtObject): TExtFunction; overload;
    function CallFunctionMethod(const AName: string; const AValue: TExtFunction): TExtFunction;
  end;

  TExtObjectClass = class of TExtObject;

  {
  Basic class descending of TExtObject that defines a JavaScript function. All functions and procedures of Ext JS framework are converted to Pascal functions
  where its return is this class. With this all converted functions by <link ExtPascal.pas, Wrapper> could be assigned to event handlers.
  }
  TExtFunction = class(TExtObject);

  {
  Defines an user session opened in a browser. Each session is a FastCGI thread that owns additional JavaScript and Ext JS resources
  as: theme, charset, language, Ajax, error messages using Ext look, JS libraries and CSS.
  The <color red>"Self-translating"</color> is implemented in this class in <link TExtObject.JSCode, JSCode> method.
  }
  TExtSession = class(TWebSession)
  private
    FObjectSequences: TDictionary<string, Cardinal>;
    FStyles, FLibraries, FLanguage : string;
    JSReturns : TStringList;
    FResponseItemsStack: TStack<TExtResponseItems>;
    Sequence: Cardinal;
    FSingletons: TDictionary<string, TExtObject>;
    FMobileBrowserDetectionDone: Boolean;
    FIsMobileApple: Boolean;
    procedure RelocateVar(JS, JSName : string; I : integer);
    function GetStyleTag: string;
    function GetSequence: string;
    function GetResponseItems: TExtResponseItems;
  protected
    function BeforeHandleRequest : boolean; override;
    procedure AfterHandleRequest; override;
    function GarbageFixName(const Name: string): string; override;
    procedure OnError(const AMessage, AMethodName, AParams : string); override;
    function GetNextJSName(const AObjectType: string): string;
    function GetUrlHandlerObject: TObject; override;
    function JSConcat(PrevCommand, NextCommand : string) : string;
    function GetMainPageTemplate: string; virtual;
    procedure SetLanguage(const AValue: string); virtual;
    function GetViewportContent: string; virtual;
    function GetManifestFileName: string; virtual;
    function GetCustomJS: string; virtual;
  public
    HTMLQuirksMode : boolean; // Defines the (X)HTML DocType. True to Transitional (Quirks mode) or false to Strict. Default is false.
    Theme     : string; // Sets or gets Ext JS installed theme, default '' that is Ext Blue theme
    ExtPath   : string; // Installation path of Ext JS framework, below the your Web server document root. Default value is '/ext'
    ImagePath : string; // Image path below ExtPath, used by <link TExtSession.SetIconCls, SetIconCls> method. Default value is '/images'
    ExtBuild  : string;
    procedure AfterConstruction; override;
    destructor Destroy; override; // Custom <extlink http://www.extjs.com/products/extjs/build/>ExtJS build</extlink>. Default is ext-all.
    property Language : string read FLanguage write SetLanguage; // Actual language for this session, reads HTTP_ACCEPT_LANGUAGE header
    procedure InitDefaultValues; override;
    procedure JSCode(JS : string; JSClassName : string = ''; JSName : string = ''; Owner : string = '');
    procedure JSSleep(MiliSeconds : integer);
    procedure SetStyle(pStyle : string = '');
    procedure SetLibrary(pLibrary : string = ''; CSS : boolean = false; HasDebug : boolean = false; DisableExistenceCheck : boolean = false);
    procedure SetCSS(pCSS : string; Check : boolean = true);
    procedure SetIconCls(Cls : array of string);
    procedure ErrorMessage(const AMessage: string; const AAction: string = ''); overload;
    procedure ErrorMessage(const AMessage: string; const AAction: TExtFunction); overload;
    procedure Alert(const Msg : string); override;
    procedure Refresh; override;

    property ResponseItems: TExtResponseItems read GetResponseItems;
    function HasResponseItems: Boolean;
    function BranchResponseItems: TExtResponseItems;
    procedure UnbranchResponseItems(const AResponseItems: TExtResponseItems;
      const AConsolidate: Boolean = True);

    function GetSingleton<T: TExtObject>(const AName: string): T;
    function IsMobileApple: Boolean;
  published
    procedure HandleEvent; virtual;
  end;

  // List of TExtObjects. The <link ExtPascal.pas, Wrapper> convey the JavaScript Array type to this class
  TExtObjectList = class(TExtFunction)
  private
    FObjects: TObjectList<TExtObject>;
    FAttribute: string;
    function GetObject(I: Integer): TExtObject;
    function GetOwnerExtObject: TExtObject;
    function GetCount: Integer;
    property OwnerExtObject: TExtObject read GetOwnerExtObject;
  protected
    procedure CreateVar(AJSCode: string); override;
    procedure CreateJSName; override;
  public
    procedure AfterConstruction; override;
    constructor CreateAsAttribute(const AOwner: TExtObject; const AAttribute: string);
    destructor Destroy; override;

    property Objects[I: Integer]: TExtObject read GetObject; default;
    function Add(const AObject: TExtObject): Integer;
    function AddInternal(const AObject: TExtObject): Integer;
    function Remove(const AObject: TExtObject): Integer;
    function IndexOf(const AObject: TExtObject): Integer;
    property Count: Integer read GetCount;

    function GetAddMethodName: string;
  end;

//(*DOM-IGNORE-BEGIN
  {
  Classes that can not be documented.
  They are usually JS basic classes without reference in Ext JS documentation by omission or fault.
  }
  THTMLElement = class(TExtObject);
  TStyleSheet = class(TExtObject);
  TRegExp = type string;
  TCSSRule = class(TExtObject);
  TXMLDocument = class(TExtObject);
  TNodeList = class(TExtObjectList);
  TExtDataNode = class(TExtObject);
  TRegion = type string;
  TNativeMenu = TExtObject;
  Tel = type string; // doc fault
  TEvent = class(TExtObject);
  TEventObject = TEvent;
  TExtEventObject = TEventObject;
  THTMLNode = TExtObject;
  TConstructor = class(TExtObject);
  TExtLibRegion = class(TExtObject); //doc fault
  TvisMode = Integer; // doc fault
  TThe = TExtObject; // doc fault
  TThis = TExtObject; // doc fault
  TairNativeMenu = TExtObject;
  TX = TExtObject; // doc fault
  TN1 = TExtObject; // doc fault
  TN2 = TExtObject; // doc fault
  TLayout = TExtObject; // Poor hierarchy definition
  TId = TExtObject; // doc fault
  TiPageX = TExtObject; // doc fault
  TiPageY = TExtObject; // doc fault
  TExtGridGrid = TExtObject; // doc fault
  TTreeSelectionModel = TExtObject; // doc fault
  TSelectionModel = TExtObject; // doc fault
  TDataSource = TExtObject; // doc fault
  TAirNotificationType = TExtObject; // doc fault
  TIterable = TExtObjectList; // doc fault
  TAnything = TExtObject; // doc fault
  TNodeLists = class(TExtObjectList); // doc fault
  TArrays = TExtObjectList; // doc fault
  TExtDirectExceptionEvent = TEvent; // doc fault Ext 3.0
  TExtDirectEvent = TEvent; // doc fault Ext 3.0
  TExtDirectTransaction = TExtObject; // doc fault Ext 3.0
  TDOMElement = TExtObject; // doc fault Ext 3.0
  TRecord = TExtObject; // Ext 3.0 RC2
  TNull = TExtObject; // Ext 3.0 RC2
  TMisc = TExtObject; // doc fault Ext 3.0
  THash = TExtObject; // doc fault Ext 3.1
  TXMLElement = TExtObject; // doc fault Ext 3.1
  TExtListView = TExtObject; // doc fault Ext 3.1
  TExtSlider = TExtObject; // doc fault Ext 3.2
//DOM-IGNORE-END*)

function Session: TExtSession;

const
  DeclareJS    = '/*var*/ '; // Declare JS objects as global
  CommandDelim = #3;         // Internal JS command delimiter
  IdentDelim   = #4;         // Internal JS identifier delimiter
  JSDelim      = #5;         // Internal JSCommand delimiter

implementation

uses
  {$IFDEF MSWINDOWS}{$IF RTLVersion <= 21}Windows,{$IFEND}{$ENDIF}
  ExtPascalClasses, StrUtils, Math, Ext, ExtUtil, ExtGrid, ExtForm;

var
  _JSFormatSettings: TFormatSettings;

function Session: TExtSession;
begin
  Result := TExtSession(_CurrentWebSession);
end;

{ TExtSession }

procedure TExtSession.SetLanguage(const AValue: string);
begin
  FLanguage := AValue;
end;

{
Adds/Removes an user JS library to be used in current response.
If the WebServer is Apache tests if the library exists.
Repeated libraries are ignored.
@param pLibrary JS library without extension (.js), but with Path based on Web server document root.
@param CSS pLibrary has a companion stylesheet (.css) with same path and name.
@param HasDebug Library has a debug, non minified, version. Default is false.
If pLibrary is '' then all user JS libraries to this session will be removed from response.
@example <code>SetLibrary('');</code>
@example <code>SetLibrary(<link ExtPath> + '/examples/tabs/TabCloseMenu');</code>
}
procedure TExtSession.SetLibrary(pLibrary : string = ''; CSS : boolean = false; HasDebug : boolean = false;
  DisableExistenceCheck : boolean = false);
var
  Root : string;
begin
  pLibrary := ReplaceStr(pLibrary, '{ext}', ExtPath);
  if pos(pLibrary + '.js', FLibraries) = 0 then
    if pLibrary = '' then
      FLibraries := '' // Clear FLibraries
    else begin
      if DisableExistenceCheck then
        Root := ''
      else
        Root := RequestHeader['DOCUMENT_ROOT'];
      if (Root = '') or ((Root <> '') and FileExists(Root + pLibrary + '.js')) then begin
        FLibraries := FLibraries + '<script src="' + pLibrary{$IFDEF DEBUGJS}+ IfThen(HasDebug, '-debug', ''){$ENDIF} + '.js"></script>'^M^J;
        if CSS then begin
          if not DisableExistenceCheck and not FileExists(Root + pLibrary + '.css') then // Assume in /css like ux
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
procedure TExtSession.SetCSS(pCSS : string; Check : boolean = true);
var
  Root : string;
begin
  pCSS := ReplaceStr(pCSS, '{ext}', ExtPath);
  if pos(pCSS + '.css', FLibraries) = 0 then
    if pCSS = '' then
      FLibraries := '' // Clear FLibraries
    else begin
      Root := RequestHeader['DOCUMENT_ROOT'];
      if Check and (Root <> '') and not FileExists(Root + pCSS + '.css') then
        raise Exception.Create('Stylesheet: ' + Root + pCSS + '.css not found')
      else
        FLibraries := FLibraries + '<link rel=stylesheet href="' + pCSS + '.css" />';
    end;
end;

{
Creates CSS classes to use with IconCls properties in Buttons. The images are .png files in 16x16 format.
Set <link ImagePath> variable to appropriate value before to use this method. Default is /images.
If the WebServer is Apache tests if each image exists.
Repeated images are ignored.
@param Cls String array with the names of .png files.
@example <code>
SetIconCls(['task', 'objects', 'commit', 'cancel', 'refresh', 'info', 'help', 'pitinnu', 'exit']);
with TExtToolbarButton.AddTo(Items) do begin
  Text    := 'Tasks';
  IconCls := 'task';
  Menu    := TaskMenu;
end;</code>
}
procedure TExtSession.SetIconCls(Cls : array of string);
var
  I : integer;
  Root : string;
begin
  Root := RequestHeader['Document_Root'];
  for I := 0 to high(Cls) do
    if (Root = '') or ((Root <> '') and FileExists(Root + ImagePath + '/' + Cls[I] + '.png')) then
      SetStyle('.' + Cls[I] + '{background-image:url("' + ImagePath + '/' + Cls[I] + '.png") !important}')
    else
      raise Exception.Create('Image file: ' + Root + ImagePath + '/' + Cls[I] + '.png not found');
end;

(*
Adds/Removes a user stylesheet to be used in current response.
Repeated style is ignored.
@param pStyle Styles to apply upon HTML or Ext elements in this response using CSS notation.
If pStyle is '' then all user styles to this session will be removed from response.
@example <code>SetStyle('');</code>
@example <code>SetStyle('img:hover{border:1px solid blue}');</code>
*)
procedure TExtSession.SetStyle(pStyle : string);
begin
  if Pos(pStyle, FStyles) = 0 then
    if pStyle = '' then
      FStyles := ''
    else
      FStyles := FStyles + pStyle
end;

procedure TExtSession.UnbranchResponseItems(
  const AResponseItems: TExtResponseItems; const AConsolidate: Boolean);
var
  LSender: TExtObject;
  LBranch: TExtResponseItems;
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
function TExtSession.GetStyleTag: string;
begin
  if FStyles = '' then
    Result := ''
  else
    Result := '<style>' + {$IFDEF DEBUGJS}BeautifyCSS(FStyles){$ELSE}FStyles{$ENDIF} + '</style>';
end;

// Returns a object which will be used to handle the page method. We will call it's published method based on PathInfo.
function TExtSession.GetUrlHandlerObject: TObject;
var
  LObjectName: string;
begin
  LObjectName := Query['Obj'];
  if (LObjectName = '') or (Query['IsEvent'] = '1') then
    Result := inherited GetUrlHandlerObject
  else
    Result := ObjectCatalog.FindExtObject(LObjectName);
end;

function TExtSession.GetViewportContent: string;
begin
  Result := '';
end;

{
Shows an error message in browser session using Ext JS style.
@param Msg Message text, can to use HTML to formating text.
@param Action Optional action that will be executed after user to click Ok button. Could be JavaScript or ExtPascal commands
@example <code>ErrorMessage('User not found.');</code>
@example <code>ErrorMessage('Context not found.<br/>This Window will be reloaded to fix this issue.', 'window.location.reload()');</code>
}
procedure TExtSession.ErrorMessage(const AMessage: string; const AAction: string);
begin
  ResponseItems.ExecuteJSCode('Ext.Msg.show({title:"Error",msg:' + StrToJS(AMessage, True) +
    ',icon:Ext.Msg.ERROR,buttons:Ext.Msg.OK' + IfThen(AAction = '', '', ',fn:function(){' + AAction + '}') + '});');
end;

{
Shows an error message in browser session using Ext JS style.
@param Msg Message text, can to use HTML to formating text.
@param Action Optional action that will be executed after user to click Ok button. Could be JavaScript or ExtPascal commands
@example <code>ErrorMessage('Illegal operation.<br/>Click OK to Shutdown.', Ajax(Shutdown));</code>
}
procedure TExtSession.ErrorMessage(const AMessage: string; const AAction: TExtFunction);
begin
  ErrorMessage(AMessage, AAction.ExtractJSCommand);
end;

function TExtSession.GarbageFixName(const Name: string): string; begin
  Result := AnsiReplaceStr(Name, IdentDelim, '');
end;

procedure TExtSession.OnError(const AMessage, AMethodName, AParams : string);
begin
  ResponseItems.Clear;
  {$IFDEF DEBUG}
    ErrorMessage(AMessage + '<br/>Method: ' + IfThen(AMethodName = '', 'Home', AMethodName) +
      IfThen(AParams = '', '', '<br/>Params:<br/>' + AnsiReplaceStr(AParams, '&', '<br/>')));
  {$ELSE}
    ErrorMessage(AMessage);
  {$ENDIF}
end;

procedure TExtSession.Alert(const Msg : string); begin
  ErrorMessage(Msg)
end;

{
Self-translating main procedure. Translates Object Pascal code to JavaScript code.
Self-translating (ST) is not a compiler. It's a minimalist (very small, ultra quick and dirty) approach that imitates an online interpreter.
You code in Object Pascal and when the program runs it automatically generates the corresponding JavaScript code.

But there is an essential limitation; it does not create business rules or sophisticated behavior in JavaScript.
So it does not interpret "IF", "WHILE", "CASE", "TRY", etc commands, but "IF", "WHILE", etc realizes a conditional code generation
on Server side as ASP and JSP does it. ST is used to create objects and widgets, to set properties and events and to call methods.
It's analogous to Delphi .dfm file role: to describe a GUI.
There are additional facilities to invoke complex Server side logic using <link TExtObject.Ajax, AJAX>, to define small <link TExtObject.JSFunction, functions> in JavaScript and
to use <link TExtThread.SetLibrary, large JS libraries>. It's enough to create powerful GUIs.
The rest (business rules, database access, etc) should be done in Object Pascal on Server side.

Basic work:
* JS commands are appended to Response.
* JS attributes are found in Response using yours JSName attribute and setted in place.
* If not found, JS attributes are appended to Response.
@param JS JS commands or assigning of attributes or events
@param JSName Optional current JS object name
@param Owner Optional JS object owner for TExtObjectList
}
procedure TExtSession.JSCode(JS : string; JSClassName : string = ''; JSName : string = ''; Owner : string = '');
var
  I, J : integer;
  LObjectName: string;
begin
  if JS <> '' then begin
    if JS[length(JS)] = ';' then begin // Command
      I := pos('.', JS);
      J := pos(IdentDelim, JS);
      if (Pos('Singleton', JSClassName) = 0) and (J > 0) and (J < I) and (Pos(DeclareJS, JS) = 0) then
      begin
        LObjectName := GarbageFixName(Copy(JS, J - 1, I - J + 1));
//        if not Assigned(ObjectCatalog.FindExtObject(LObjectName)) then
//          raise Exception.CreateFmt('%s: Public property or Method: %s.%s requires explicit ''var'' declaration.',
//            [LObjectName, JSClassName, Copy(JS, I + 1, FirstDelimiter('=(', JS, I) - I - 1)]);
      end;
      I := Length(Response) + 1
    end
    else  // set attribute
      if JSName = '' then
        raise Exception.Create('Missing '';'' in command: ' + JS)
      else begin
        I := pos('/*' + JSName + '*/', Response);
        if I = 0 then
          raise Exception.Create('Config Option: ' + JS + '<br/>is refering a previous request,' +
            '<br/>it''s not allowed in AJAX request or JS handler.<br/>Use equivalent Public Property or Method instead.');
        if not CharInSet(Response[I-1], ['{', '[', '(', ';']) then JS := ',' + JS;
      end;
    insert(JS, Response, I);
    if (pos('O' + IdentDelim, JS) <> 0) and (pos('O' + IdentDelim, JSName) <> 0) then begin
      if Owner <> '' then JSName := Owner;
      RelocateVar(JS, JSName, I+length(JS));
    end;
  end;
end;

{
Forces that the JS Object declaration (var) occurs before of its use: method call, get/set attributes, etc,
relocating the declaration to a previous position in Response.
@param JS Command or attribute that uses the JS Object.
@param JSName Current JS Object.
@param I Position in Response string where the JS command ends.
}
procedure TExtSession.Refresh;
begin
  inherited;
  ResponseItems.Clear;
  Sequence := 0;
  FObjectSequences.Clear;
  FSingletons.Clear;
end;

procedure TExtSession.RelocateVar(JS, JSName : string; I : integer);
var
  VarName, VarBody : string;
  J, K : integer;
begin
  J := LastDelimiter(':,', JS);
  if J <> 0 then
    VarName := copy(JS, J+1, posex(IdentDelim, JS, J+3)-J)
  else
    VarName := JS;
  J := posex('/*' + VarName + '*/', Response, I);
  if J > I then begin
    K := pos('/*' + JSName + '*/', Response);
    K := posex(';', Response, K)+1;
    J := posex(';', Response, J);
    VarBody := copy(Response, K, J-K+1);
    J := LastDelimiter(CommandDelim, VarBody)+1;
    VarBody := copy(VarBody, J, length(VarBody));
    delete(Response, K+J-1, length(VarBody));
    insert(VarBody, Response, pos(DeclareJS + JSName + '=new', Response));
  end;
end;

{
Concats two JS commands only to translate nested Object Pascal typecasts as:
@param PrevCommand Command already present in Response that will be concatenated with NextCommand
@param NextCommand Command that will be concatenated with PrevCommand.
@return The JS commands concatenated
@example <code>
TExtSelectionRowModel(GetSelectionModel).SelectFirstRow;
// It's usually could be translated to:
O1.getSelectionModel;
O1.selectFirstRow;
// instead of:
O1.getSelectionModel.selectFirstRow;</code>
}
function TExtSession.JSConcat(PrevCommand, NextCommand : string) : string;
var
  I , J : integer;
begin
  J := ExtPascalUtils.RPosEx(PrevCommand, Response, 1);
  I := Pos('.', NextCommand);
  if (I <> 0) and (J <> 0) then begin
    NextCommand := copy(NextCommand, I, length(NextCommand));
    Result := copy(PrevCommand, 1, length(PrevCommand)-1) + NextCommand;
    delete(Response, J + length(PrevCommand)-1, 1);
    insert(NextCommand, Response, J + length(PrevCommand))
  end
  else
    Result := PrevCommand;
end;

procedure TExtSession.JSSleep(MiliSeconds : integer); begin
  JSCode('sleep(' + IntToStr(MiliSeconds) + ');')
end;

procedure TExtObject.JSSleep(MiliSeconds : integer); begin
  JSCode('sleep(' + IntToStr(MiliSeconds) + ');')
end;

function TExtObject.MethodURI(Method : TExtProcedure; Params : array of const) : string;
begin
  Result := MethodURI(Method);
  if Length(Params) <> 0 then
    Result := Result + '&' + FormatParams('TExtObject.MethodURI', Params);
end;

function TExtObject.MethodURI(Method : TExtProcedure) : string;
var
  MetName, ObjName : string;
begin
  FindMethod(Method, MetName, ObjName);
  Result := ExtSession.MethodURI(MetName);
  if ObjName <> '' then
  begin
    if Pos('?', Result) <> 0 then
      Result := Result + '&Obj=' + ObjName
    else
      Result := Result + '?Obj=' + ObjName;
  end;
end;

function TExtObject.MethodURI(MethodName : string; Params : array of const) : string; begin
  Result := ExtSession.MethodURI(MethodName) + IfThen(length(Params) = 0, '', '?' + FormatParams(MethodName, Params))
end;

function TExtObject.MethodURI(MethodName : string): string; begin
  Result := ExtSession.MethodURI(MethodName)
end;

function TExtObject.GetObjectNamePrefix: string;
begin
  Result := 'o';
end;

function TExtObject.GetPOSTAjaxCode(const AMethod: TExtProcedure;
  const AParams: array of const; const AJsonData: string): string;
var
  LParams: string;
  LMethodName: string;
  LObjectName: string;
begin
  FindMethod(AMethod, LMethodName, LObjectName);
  LParams := IfThen(LObjectName = '', '', 'Obj=' + LObjectName);
  Result :=  GetPOSTAjaxCode(LMethodName, LParams, AParams, AJsonData);
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
function TExtSession.BeforeHandleRequest : boolean;
var
  I : integer;
begin
  Result := true;
  if FLanguage = '' then begin // Set language
    FLanguage := RequestHeader['HTTP_ACCEPT_LANGUAGE'];
    I := pos('-', FLanguage);
    if I <> 0 then begin
      FLanguage := copy(FLanguage, I-2, 2) + '_' + Uppercase(copy(FLanguage, I+1, 2));
      if not FileExists(RequestHeader['DOCUMENT_ROOT'] + ExtPath + '/build/classic/locale/locale-' + FLanguage + '.js') then
        FLanguage := copy(FLanguage, 1, 2)
    end;
  end;
  IsAjax := (RequestHeader['HTTP_X_REQUESTED_WITH'] = 'XMLHttpRequest') or IsUpload;
  if IsAjax then begin
    if SessionCookie = '' then begin
      ErrorMessage('This web application requires Cookies enabled to AJAX works.');
      Result := false;
    end
    else
      if NewThread or RequiresReload then begin
        ErrorMessage('Session expired or lost.<br/>A new session will be created now.', 'window.location.reload()');
        RequiresReload := true;
        Result := false;
      end
  end
  else
    RequiresReload := false;
  JSReturns := TStringList.Create;
end;

function TExtSession.BranchResponseItems: TExtResponseItems;
begin
  Result := TExtResponseItems.Create;
  FResponseItemsStack.Push(Result);

  Assert(FResponseItemsStack.Count > 0);
end;

destructor TExtSession.Destroy;
begin
  Assert(FResponseItemsStack.Count = 1);
  FResponseItemsStack.Pop.Free;
  FreeAndNil(FResponseItemsStack);
  FreeAndNil(FObjectSequences);
  FreeAndNil(FSingletons);
  inherited;
end;

procedure TExtSession.InitDefaultValues; begin
  inherited;
{$IFDEF CacheFly}
  ExtPath       := 'http://extjs.cachefly.net/ext-3.2.1';
{$ELSE}
  ExtPath       := '/ext6';
{$ENDIF}
  ImagePath     := '/images';
  ExtBuild      := 'ext-all';
  Charset       := 'utf-8'; // 'iso-8859-1'
  UpLoadPath    := '/uploads';
end;

function TExtSession.IsMobileApple: Boolean;
var
  LUserAgent: string;
begin
  if not FMobileBrowserDetectionDone then
  begin
    LUserAgent := RequestHeader['HTTP_USER_AGENT'];
    FIsMobileApple :=
      LUserAgent.Contains('iPhone') or
      LUserAgent.Contains('iPad');
    FMobileBrowserDetectionDone := True;
  end;
  Result := FIsMobileApple;
end;

// Calls events using Delphi style
procedure TExtSession.HandleEvent;
var
  LObject: TExtObject;
begin
  if Query['IsEvent'] = '1' then
  begin
    LObject := ObjectCatalog.FindExtObject(Query['Obj']) as TExtObject;
    if not Assigned(LObject) then
      OnError('Object not found in session list. It could be timed out, refresh page and try again', 'HandleEvent', '')
    else
      LObject.HandleEvent(Query['Evt']);
  end;
end;

function TExtSession.HasResponseItems: Boolean;
begin
  Result := Assigned(FResponseItemsStack);
end;

{
Does tasks after Request processing.
1. Extracts Comments, auxiliary delimiters, and sets:
2. HTML body,
3. Title,
4. Application icon,
5. Charset,
6. ExtJS CSS,
7. ExtJS libraries,
8. ExtJS theme,
9. ExtJS language,
10. Additional user styles,
11. Additional user libraries,
12. Additional user JavaScript,
13. ExtJS invoke,
14. Handlers for AJAX response and
15. If DEBUGJS conditional-define is active:
  15.1. Uses CodePress library with syntax highlight,
  15.2. Generates JS code to enhance AJAX debugging, using Firefox, and
  15.3. Formats AJAX response code for easy debugging.
}
procedure TExtSession.AfterConstruction;
begin
  inherited;
  FResponseItemsStack := TStack<TExtResponseItems>.Create;
  FResponseItemsStack.Push(TExtResponseItems.Create);

  FObjectSequences := TDictionary<string, Cardinal>.Create;
  FSingletons := TDictionary<string, TExtObject>.Create;
end;

function TExtSession.GetCustomJS: string;
begin
  Result := '';
end;

function TExtSession.GetMainPageTemplate: string;
begin
  Result := '<%HTMLDeclaration%>' + sLineBreak +
    '<head>' + sLineBreak +
    '  <title><%ApplicationTitle%></title>' + sLineBreak +
    '  <%ApplicationIconLink%>' + sLineBreak +
    '  <%AppleIconLink%>' + sLineBreak +
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
    '  <%ThemeLink%>' + sLineBreak +
    '  <%LanguageLink%>' + sLineBreak +
    '  <%StyleTag%>' + sLineBreak +
    '  <%LibraryTags%>' + sLineBreak +
    '</head>' + sLineBreak +
    '<body>' + sLineBreak +
    '<div id="body">' + sLineBreak +
    '  <div id="loading" style="position:absolute;font-family:verdana;top:40%;left:40%">' + sLineBreak +
    '    <img src="<%ExtPath%>/classic/theme-classic/resources/images/shared/loading-balls.gif"/>Loading <%ApplicationTitle%>...' + sLineBreak +
    '  </div>' + sLineBreak +
    '</div>' + sLineBreak +
    '<noscript>This web application requires JavaScript enabled</noscript>' + sLineBreak +
    '</body>' + sLineBreak +
    '  <script>' + sLineBreak +
    {$IFDEF DEBUGJS}BeautifyJS{$ENDIF}
    ('<%CustomJS%>' + sLineBreak +
    'function AjaxError(m){Ext.Msg.show({title:"Ajax Error",msg:m,icon:Ext.Msg.ERROR,buttons:Ext.Msg.OK});};' +
    {$IFDEF DEBUGJS}
    'function AjaxSource(t,l,s){var w=new Ext.Window({title:"Ajax error: "+t+", Line: "+' + IfThen(Browser=brFirefox, '(l-%%)', '"Use Firefox to debug"') +
    ',width:600,height:400,modal:true,items:[new Ext.ux.CodePress({language:"javascript",readOnly:true,code:s})]});w.show();' +
    'w.on("resize",function(){w.items.get(0).resize();});};' +
    'function AjaxSuccess(response){try{eval(response.responseText);}catch(err){AjaxSource(err.message,err.lineNumber,response.responseText);}};' +
    {$ELSE}
    'function AjaxSuccess(response){try{eval(response.responseText);}catch(err){AjaxError(err.message+"<br/>Use DebugJS define to enhance debugging<br/>"+response.responseText);}};' +
    {$ENDIF}
    'function sleep(ms){var start=new Date().getTime();for(var i=0;i<1e7;i++)if((new Date().getTime()-start)>ms)break;};'+ sLineBreak +
    'function AjaxFailure(){AjaxError("Server unavailable, try later.");};' + sLineBreak +
    'Ext.onReady(function(){' + sLineBreak +
    'Ext.get("loading").remove();' + sLineBreak +
    'Ext.BLANK_IMAGE_URL="<%ExtPath%>/resources/images/default/s.gif";' + sLineBreak +
    'TextMetrics=Ext.util.TextMetrics.createInstance("body");' + sLineBreak +
    'Download=Ext.DomHelper.append(document.body,{tag:"iframe",cls:"x-hidden"});') + '<%Response%>});' + sLineBreak +
    '  </script>' + sLineBreak +
    '</html>';
end;

function TExtSession.GetManifestFileName: string;
begin
  Result := '';
end;

procedure TExtSession.AfterHandleRequest;

  procedure HandleJSReturns;
  var
    I : integer;
  begin
    if (JSReturns <> nil) and (JSReturns.Count <> 0) then
      for I := 0 to JSReturns.Count-1 do begin
        Response := AnsiReplaceStr(Response, JSReturns.Names[I], JSReturns.ValueFromIndex[I]);
        Response := AnsiReplaceStr(Response, IntToStr(StrToInt(JSReturns.Names[I])), JSReturns.ValueFromIndex[I]);
      end;
    FreeAndNil(JSReturns);
  end;

var
  I, J : integer;
  LMainPageCode: string;
begin
  if IsDownLoad or IsUpLoad then
    Exit;

  Response := ResponseItems.Consume;

  I := pos('/*', Response);
  while I <> 0 do begin // Extracts comments
    J := PosEx('*/', Response, I);
    delete(Response, I, J - I + 2);
    I := PosEx('/*', Response, I);
  end;
  HandleJSReturns;
  Response := AnsiReplaceStr(AnsiReplaceStr(Response, CommandDelim, ''), IdentDelim, ''); // Extracts aux delimiters
  if not IsAjax then
  begin
    ContentType := 'text/html; charset=' + Charset;
    LMainPageCode := GetMainPageTemplate;

    // Replace template macros in main page code.
    LMainPageCode := ReplaceText(LMainPageCode, '<%HTMLDeclaration%>',
      IfThen(HTMLQuirksMode, '<!docttype html public><html>',
      '<?xml version=1.0?>' + sLineBreak +
      '<!doctype html public "-//W3C//DTD XHTML 1.0 Strict//EN">' + sLineBreak +
      '<html xmlns=http://www.w3org/1999/xthml>' + sLineBreak));
    LMainPageCode := ReplaceText(LMainPageCode, '<%ViewportContent%>', GetViewportContent);
    LMainPageCode := ReplaceText(LMainPageCode, '<%ApplicationTitle%>', Application.Title);
    LMainPageCode := ReplaceText(LMainPageCode, '<%ApplicationIconLink%>',
      IfThen(Application.Icon = '', '', '<link rel="shortcut icon" href="' + Application.Icon + '"/>'));
    LMainPageCode := ReplaceText(LMainPageCode, '<%AppleIconLink%>',
      IfThen(Application.Icon = '', '', '<link rel="apple-touch-icon" sizes="120x120" href="' + Application.Icon + '"/>'));
    LMainPageCode := ReplaceText(LMainPageCode, '<%CharSet%>', CharSet);
    LMainPageCode := ReplaceText(LMainPageCode, '<%ExtPath%>', ExtPath);
    LMainPageCode := ReplaceText(LMainPageCode, '<%ExtBuild%>', ExtBuild);
    LMainPageCode := ReplaceText(LMainPageCode, '<%DebugSuffix%>', {$IFDEF DebugExtJS}'-debug'{$ELSE}''{$ENDIF});
    LMainPageCode := ReplaceText(LMainPageCode, '<%ManifestLink%>',
      IfThen(GetManifestFileName = '', '', Format('<link rel="manifest" href="%s"/>', [GetManifestFileName])));
    LMainPageCode := ReplaceText(LMainPageCode, '<%ThemeLink%>',
      IfThen(Theme = '', '', '<link rel=stylesheet href="' + ExtPath + '/build/classic/theme-' + Theme + '/resources/theme-' + Theme + '-all.css" />'));
    LMainPageCode := ReplaceText(LMainPageCode, '<%LanguageLink%>',
      IfThen(FLanguage = 'en', '', '<script src="' + ExtPath + '/build/classic/locale/locale-' + FLanguage + '.js"></script>'));
    LMainPageCode := ReplaceText(LMainPageCode, '<%StyleTag%>', GetStyleTag);
    LMainPageCode := ReplaceText(LMainPageCode, '<%LibraryTags%>', FLibraries);
    LMainPageCode := ReplaceText(LMainPageCode, '<%CustomJS%>', GetCustomJS);
    LMainPageCode := ReplaceText(LMainPageCode, '<%Response%>', Response);
    Response := LMainPageCode;
    {$IFDEF DEBUGJS}
    Response := AnsiReplaceStr(Response, '%%', IntToStr(CountStr(^M^J, Response, 'eval('))); // eval() line number
    {$ENDIF}
  end
  else begin
    if (Response <> '') and (Response[1] = '<') then
      ContentType := 'text/html; charset=' + Charset
    else if (Response <> '') and CharInSet(Response[1], ['{', '[']) then
      ContentType := 'application/json; charset=' + Charset
    else
    begin
      ContentType := 'text/javascript; charset=' + Charset;
      {$IFDEF DEBUGJS}
      Response := BeautifyJS(Response);
      {$ENDIF}
    end;
  end;
end;

{
Returns a unique numeric sequence to identify a JS object, list or attribute in this session.
This sequence will be used by Self-translating process imitating a Symbol table entrance.
}
function TExtSession.GetNextJSName(const AObjectType: string): string;
var
  LResult: Cardinal;
begin
  if not FObjectSequences.ContainsKey(AObjectType) then
    FObjectSequences.Add(AObjectType, 0);
  LResult := FObjectSequences[AObjectType] + 1;
  FObjectSequences[AObjectType] := LResult;
  Result := AObjectType + IntToStr(LResult);
end;

function TExtSession.GetResponseItems: TExtResponseItems;
begin
  Assert(FResponseItemsStack.Count > 0);

  Result := FResponseItemsStack.Peek;
end;

function TExtSession.GetSequence : string; begin
  Result := IntToHex(Sequence, 1);
  Inc(Sequence);
end;

function TExtSession.GetSingleton<T>(const AName: string): T;
begin
  if FSingletons.ContainsKey(AName) then
    Result := T(FSingletons[AName])
  else
  begin
    Result := TExtObjectClass(T).CreateSingleton(Self.ObjectCatalog, AName) as T;
    FSingletons.Add(AName, Result);
  end;
end;

{ ExtObjectList }

{
Creates a TExtObjectList instance.
@param pOwner TExtObject that owns this list
@param pAttribute JS attribute name in TExtObject to this list
}
constructor TExtObjectList.CreateAsAttribute(const AOwner: TExtObject; const AAttribute: string);
begin
  Assert(Assigned(AOwner));
  Assert(AAttribute <> '');

  FAttribute := AAttribute;
  inherited Create(AOwner);
end;

procedure TExtObjectList.CreateJSName;
begin
  FJSName := '';
  Name := '';
end;

procedure TExtObjectList.CreateVar(AJSCode: string);
begin
  CreateJSName;
end;

// Frees this list and all objects linked in it
destructor TExtObjectList.Destroy;
begin
  FreeAndNil(FObjects);
  inherited;
end;

function TExtObjectList.Add(const AObject: TExtObject): Integer;
begin
  Assert(Assigned(AObject));
  Assert(Assigned(OwnerExtObject));
  Assert(FAttribute <> '');

  ExtSession.ResponseItems.AddToList(OwnerExtObject, Self, FAttribute, GetAddMethodName(), [AObject, False]);
  Result := AddInternal(AObject);
end;

{
Returns the Ith object in the list, starts with 0.
@param I Position in list
@return <link TExtObject>
}
function TExtObjectList.GetAddMethodName: string;
begin
  // items -> add()
  // buttons -> addButton()
  if (FAttribute = '') or (FAttribute = 'items') then
    Result := 'add'
  else
  begin
    Result := FAttribute;
    if EndsText('s', Result) then
      System.Delete(Result, Length(Result), 1);
    Result[1] := UpperCase(Result[1])[1];
    Result := 'add' + Result;
  end;
end;

function TExtObjectList.GetCount: Integer;
begin
  Result := FObjects.Count;
end;

function TExtObjectList.GetObject(I: Integer): TExtObject;
begin
  Result := FObjects[I];
end;

function TExtObjectList.GetOwnerExtObject: TExtObject;
begin
  Result := Owner as TExtObject;
end;

function TExtObjectList.IndexOf(const AObject: TExtObject): Integer;
begin
  Result := FObjects.IndexOf(AObject);
end;

function TExtObjectList.Remove(const AObject: TExtObject): Integer;
begin
  Result := FObjects.Remove(AObject);
end;

function TExtObjectList.AddInternal(const AObject: TExtObject): Integer;
begin
  Assert(Assigned(AObject));

  Result := FObjects.Add(AObject);
end;

procedure TExtObjectList.AfterConstruction;
begin
  inherited;
  FObjects := TObjectList<TExtObject>.Create(False);
end;

{ ExtObject }

// Set an unique <link TExtObject.JSName, JSName> using <link TExtThread.GetSequence, GetSequence>
procedure TExtObject.CreateJSName;
begin
  if FJSName = '' then
    //FJSName := 'O' + IdentDelim + TExtThread(CurrentWebSession).GetSequence + IdentDelim;
    FJSName := ExtSession.GetNextJSName(GetObjectNamePrefix);
  //Name := TExtThread(CurrentWebSession).GarbageFixName(FJSName);
  Name := FJSName;
end;

{
Creates a singleton TExtObject instance, used usually by Parser only.
@param Attribute JS attribute name in TExtObject to this list in the form 'Owner.attribute'
}
constructor TExtObject.CreateSingleton(const AOwner: TComponent; const AAttribute: string = '');
begin
  Assert(Assigned(AOwner));
  inherited Create(AOwner);
  if AAttribute = '' then
    FJSName := JSClassName
  else
    FJSName := AAttribute;
  InitDefaults;
end;

function TExtObject.CallMethod(const AName: string; const AValue: Boolean): TExtFunction;
begin
  Session.ResponseItems.CallMethod(Self, AName, [AValue]);
  Result := TExtFunction(Self);
end;

function TExtObject.CallMethod(const AName: string): TExtFunction;
begin
  Session.ResponseItems.CallMethod(Self, AName, []);
  Result := TExtFunction(Self);
end;

function TExtObject.CallMethod(const AName, AValue: string): TExtFunction;
begin
  Session.ResponseItems.CallMethod(Self, AName, [AValue]);
  Result := TExtFunction(Self);
end;

function TExtObject.CallMethod(const AName: string; const AValue: TDateTime): TExtFunction;
begin
  Session.ResponseItems.CallMethod(Self, AName, [AValue]);
  Result := TExtFunction(Self);
end;

function TExtObject.CallMethod(const AName: string; const AValue: array of const): TExtFunction;
begin
  Session.ResponseItems.CallMethod(Self, AName, AValue);
  Result := TExtFunction(Self);
end;

function TExtObject.CharsToPixels(const AChars: Integer; const AOffset: Integer = 0): Integer;
begin
  // + 16 sort of compensates for text-to-border left and right margins.
  Result := JSExpression('(%s * %d * 1.2) + %d', [ExtUtilTextMetrics.GetWidth('g'), AChars, 16 + AOffset]);
end;

{
Converts a TExtFormTextArea height in characters to pixels to use in Height property.
Uses dynamic JS in browser.
@param Lines TextArea height in characters.
@return Pixels used by browser to render these Lines
}
function TExtObject.LinesToPixels(const ALines: Integer): Integer;
begin
  Result := JSExpression('%s * %.2f', [ExtUtilTextMetrics.GetHeight('W'), ALines * 0.8]);
end;

procedure TExtObject.CreateVar(AJSCode: string);
begin
  Assert(Assigned(ExtSession));

  { TODO : JSName could be assigned on demand. }
  CreateJSName;

  Session.ResponseItems.CreateObject(Self);
end;

// Deletes JS object from Browser memory
procedure TExtObject.Delete;
begin
  Session.ResponseItems.ExecuteJSCode(JSName + '.destroy(); delete ' + JSName + ';');
end;

procedure TExtObject.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (AComponent = Self) and (Operation = opRemove) and Assigned(Owner) then
  begin
    // The owner is destroying this object, so we clear anything related from
    // the response. We do this in the destructor for those cases in which a component
    // in destroyed while still being owned, but we must do it here as well
    // for cases in which the owner is freeing its own components (which it
    // does AFTER calling RemoveComponent, thus at a time when it's not the owner
    // anymore.
    Session.ResponseItems.RemoveAll(Self);
  end;
end;

// Calls Ext JS <b>destroy()</b> method if it exists else calls the JS <b>delete</b> command
destructor TExtObject.Destroy;
begin
  // See Notification for details.
  if Assigned(Owner) and (Session <> nil) and Session.HasResponseItems then
    Session.ResponseItems.RemoveAll(Self);
  inherited;
end;

function TExtObject.DestroyJS: TExtFunction;
begin
  Delete;
  Result := TExtFunction(Self);
end;

function TExtObject.GetAjaxCode(const AMethod: TExtProcedure;
  const ARawParams: string; const AParams: array of const): string;
var
  LParams: string;
  LMethodName: string;
  LObjectName: string;
begin
  FindMethod(AMethod, LMethodName, LObjectName);
  LParams := IfThen(LObjectName = '', '', 'Obj=' + LObjectName);
  LParams := LParams + ARawParams;
  Result :=  GetAjaxCode(LMethodName, LParams, AParams);
end;

function TExtObject.GetAjaxCode(const AMethod: TExtProcedure;
  const AParams: array of const; const AExtraCode: string): string;
var
  LParams: string;
  LMethodName: string;
  LObjectName: string;
begin
  FindMethod(AMethod, LMethodName, LObjectName);
  LParams := IfThen(LObjectName = '', '', 'Obj=' + LObjectName);
  Result := DoGetAjaxCode(LMethodName, LParams, AParams, AExtraCode);
end;

function TExtObject.GetConstructionJS: string;
begin
  Result := 'new ' + JSClassName;
end;

function TExtObject.GetDownloadJS(Method: TExtProcedure; Params: array of const): string;
var
  P, MetName, ObjName : string;
begin
  FindMethod(Method, MetName, ObjName);
  P := FormatParams(MetName, Params);
  if ObjName <> '' then begin
    if P <> '' then P := P + '&';
    P := P + 'Obj=' + ObjName;
  end;
  if P <> '' then P := '?' + P;
  if Session.IsMobileApple then
    Result := 'window.open("' + ExtSession.MethodURI(MetName) + P + '");'
  else
    Result := 'Download.src="' + ExtSession.MethodURI(MetName) + P + '";';
end;

function TExtObject.GetExtSession(const AOwner: TComponent): TExtSession;
var
  LOwner: TComponent;
begin
  if FSession = nil then
  begin
    LOwner := AOwner;
    while (LOwner <> nil) and (LOwner.Owner <> nil) do
      LOwner := LOwner.Owner;
    if LOwner is TObjectCatalog then
      FSession := TObjectCatalog(LOwner).Session as TExtSession;
    if FSession = nil then
      raise Exception.CreateFmt('Session not found for object %s of type %s (%s).', [JSName, JSClassName, ClassName]);
  end;
  Result := FSession;
end;

procedure TExtObject.Download(Method: TExtProcedure; Params: array of const);
begin
  Session.ResponseItems.ExecuteJSCode(Self, GetDownloadJS(Method, Params));
end;

procedure TExtObject.Download(Method: TExtProcedure);
begin
  Download(Method, []);
end;

{
Creates a TExtObject and generate corresponding JS code using <link TExtObject.JSCode, Self-translating>
@param Owner Optional parameter used internally by <link TExtObject.JSObject, JSObject> and <link TExtObject.JSArray, JSArray> only
}
constructor TExtObject.Create(AOwner: TComponent);
begin
  Assert(Assigned(AOwner));
  inherited Create(AOwner);
  CreateVar(JSClassName + '({});');
  InitDefaults;
end;

{
Used by Parser to build <link TExtObject.InitDefaults, InitDefaults> methods used to initialize public JS properties in a TExtObject
@param Owner TExtObject where this property is declared
@param Attribute Public JS property name
}
constructor TExtObject.CreateInline(AOwner: TComponent);
begin
  Assert(Assigned(AOwner));
  // Inline = don't create the declaration.
  inherited Create(AOwner);
  InitDefaults;
end;

{ TODO : WIP - we want to be able to create some objects inline in order to output more compact code }
constructor TExtObject.CreateInlineAndAddTo(List: TExtObjectList);
begin
  CreateInline(List);
  AddTo(List);
end;

constructor TExtObject.CreateInternal(const AOwner: TExtObject; const AAttributeName: string);
begin
  Assert(Assigned(AOwner));
  Assert(AAttributeName <> '');

  inherited Create(AOwner);
  FExtObjectOwner := AOwner;
  FAttributeName := AAttributeName;
  FJSName := FExtObjectOwner.JSName + '.' + FAttributeName;
  //InitDefaults;
  Session.ResponseItems.CreateInternalObject(Self, AAttributeName);
end;

class function TExtObject.JSClassName : string;
begin
  Result := 'Ext.Base';
end;

{
Tests if a class name is parent of this object
@param CName Class name with "T" prefix
@return True if CName is parent of this object and false if not
}
function TExtObject.IsParent(CName : string) : boolean;
var
  Cls : TClass;
begin
  if (CName <> '') and (CName[1] = 'T') then begin
    Result := true;
    Cls    := ClassType;
    while Cls.ClassName <> 'TExtFunction' do begin
      if Cls.ClassName = CName then exit;
      Cls := Cls.ClassParent
    end;
  end;
  Result := false;
end;

function TExtObject.GetExtSession: TExtSession;
begin
  Result := GetExtSession(Owner);
end;

procedure TExtObject.SetConfigItem(const AName, AMethodName: string; const AValue: array of const);
begin
  Session.ResponseItems.SetConfigItem(Self, AName, AMethodName, AValue);
end;

function TExtObject.SetConfigItem(const AName, AValue: string): string;
begin
  Session.ResponseItems.SetConfigItem(Self, AName, [AValue]);
  Result := AValue;
end;

function TExtObject.SetFunctionConfigItem(const AName: string; const AValue: TExtFunction): TExtFunction;
begin
  Session.ResponseItems.SetConfigItem(Self, AName, [AValue, True]);
  Result := AValue;
end;

{
Converts an array of const to JSON (JavaScript Object Notation) to be used in constructors, JS Arrays or JS Objects
@param A Array of anytype variables
@return JSON representation of array
}
function TExtObject.VarToJSON(const AVars: array of const; const ASession: TExtSession): string;
var
  I: Integer;
  LCommand: string;
  LObjectJSName: string;
  LCodeItem: TExtResponseItem;
  LSession: TExtSession;
begin
  Result := '';
  I := 0;

  LSession := ASession;
  if LSession = nil then
    LSession := ExtSession;

  while I <= High(AVars) do
  begin
    case AVars[I].VType of
      vtObject: begin
        if AVars[I].VObject <> nil then
        begin
          LCodeItem := LSession.ResponseItems.FindLastCodeItem(TExtObject(AVars[I].VObject));
          if Assigned(LCodeItem) and AVars[I + 1].VBoolean then
          begin
            Result := Result + WriteFunction(LCodeItem.ToString);
            LSession.ResponseItems.Remove(LCodeItem);
          end
          else
          begin
            LObjectJSName := TExtObject(AVars[I].VObject).JSName;
            if Assigned(LCodeItem) then
              LCommand := RemoveLastJSTerminator(LCodeItem.ToString)
            else
              LCommand := '';
            Result := Result + LObjectJSName;
          end;
          (*
          LCommand := TExtObject(VObject).PopJSCommand;
          if (LCommand <> '') and A[I+1].VBoolean then begin
            Result := Result + WriteFunction(LCommand);
            TExtThread(CurrentWebSession).RemoveJS(LCommand);
          end
          else begin
            JSName := TExtObject(VObject).JSName;
            if InJSFunction and (pos(JSName, LCommand) = 1) then begin
              Result := Result + copy(LCommand, 1, length(LCommand)-1);
              TExtThread(CurrentWebSession).RemoveJS(LCommand);
            end
            else
              Result := Result + JSName;
          end;
          *)
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
      vtAnsiString: Result := Result + StrToJS(string(AVars[I].VAnsiString));
      vtString:     Result := Result + StrToJS(string(AVars[I].VString^));
      vtWideString: Result := Result + StrToJS(string(AVars[I].VWideString));
      {$IFDEF UNICODE}
      vtUnicodeString: Result := Result + StrToJS(string(AVars[I].VUnicodeString));
      {$ENDIF}
      vtInteger:    Result := Result + IntToStr(AVars[I].VInteger);
      vtBoolean:    Result := Result + IfThen(AVars[I].VBoolean, 'true', 'false');
      vtExtended:   Result := Result + AnsiReplaceStr(FloatToStr(AVars[I].VExtended^), ',', '.');
      vtCurrency:   Result := Result + CurrToStr(AVars[I].VCurrency^);
      vtInt64:      Result := Result + IntToStr(AVars[I].VInt64^);
      vtVariant:    Result := Result + string(AVars[I].VVariant^);
      vtChar:       Result := Result + string(AVars[I].VChar);
      vtWideChar:   Result := Result + AVars[I].VWideChar;
    end;
    if I < High(AVars) then
      Result := Result + ',';
    Inc(I);
  end;
  if (Result <> '') and (Result[Length(Result)] = ',') then
    System.Delete(Result, Length(Result), 1);
end;

function TExtObject.VarToJSON(const AVars: array of const;
  const AProc: TVarToJSONProc): string;
var
  I: Integer;
  LCommand: string;
  LCodeItem: TExtResponseItem;
  LCurrentParam: string;
  LProcCalled: Boolean;
begin
  Result := '';
  I := 0;
  while I <= High(AVars) do
  begin
    LProcCalled := False;
    case AVars[I].VType of
      vtObject: begin
        if AVars[I].VObject <> nil then
        begin
          LCodeItem := ExtSession.ResponseItems.FindLastCodeItem(TExtObject(AVars[I].VObject));
          if Assigned(LCodeItem) and AVars[I + 1].VBoolean then
          begin
            LCurrentParam := WriteFunction(LCodeItem.ToString);
            Result := Result + LCurrentParam;
            Session.ResponseItems.Remove(LCodeItem);
          end
          else
          begin
            LCurrentParam := TExtObject(AVars[I].VObject).JSName;
            if Assigned(LCodeItem) then
              LCommand := RemoveLastJSTerminator(LCodeItem.ToString)
            else
              LCommand := '';
            { TODO : Mind the spacing - maybe characterize this type of command with a special class.}
//            if InJSFunction and (Pos(LCurrentParam, Trim(LCommand)) = 1) then
//              ExtSession.ResponseItems.Remove(LCodeItem)
//            else
              Result := Result + LCurrentParam;
          end;
          if Assigned(AProc) then
            AProc(LCurrentParam, AVars[I].VObject, AVars[I + 1].VBoolean);
          LProcCalled := True;
          (*
          LCommand := TExtObject(VObject).PopJSCommand;
          if (LCommand <> '') and A[I+1].VBoolean then begin
            Result := Result + WriteFunction(LCommand);
            TExtThread(CurrentWebSession).RemoveJS(LCommand);
          end
          else begin
            JSName := TExtObject(VObject).JSName;
            if InJSFunction and (pos(JSName, LCommand) = 1) then begin
              Result := Result + copy(LCommand, 1, length(LCommand)-1);
              TExtThread(CurrentWebSession).RemoveJS(LCommand);
            end
            else
              Result := Result + JSName;
          end;
          *)
        end
        else
        begin
//          if Assigned(AProc) then
//            AProc('null', nil, False);
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
      vtAnsiString: LCurrentParam := StrToJS(string(AVars[I].VAnsiString));
      vtString:     LCurrentParam := StrToJS(string(AVars[I].VString^));
      vtWideString: LCurrentParam := StrToJS(string(AVars[I].VWideString));
      {$IFDEF UNICODE}
      vtUnicodeString: LCurrentParam := StrToJS(string(AVars[I].VUnicodeString));
      {$ENDIF}
      vtInteger:    LCurrentParam := IntToStr(AVars[I].VInteger);
      vtBoolean:    LCurrentParam := IfThen(AVars[I].VBoolean, 'true', 'false');
      vtExtended:   LCurrentParam := AnsiReplaceStr(FloatToStr(AVars[I].VExtended^), ',', '.');
      vtCurrency:   LCurrentParam := CurrToStr(AVars[I].VCurrency^);
      vtInt64:      LCurrentParam := IntToStr(AVars[I].VInt64^);
      vtVariant:    LCurrentParam := string(AVars[I].VVariant^);
      vtChar:       LCurrentParam := string(AVars[I].VChar);
      vtWideChar:   LCurrentParam := AVars[I].VWideChar;
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

function TExtObject.RequestDownload(Method : TExtProcedure; Params : array of const): TExtFunction;
begin
  Result := JSFunction(GetDownloadJS(Method, Params));
end;

function TExtObject.SetConfigItem(const AName, AMethodName, AValue: string): string;
begin
  Session.ResponseItems.SetConfigItem(Self, AName, AMethodName, [AValue]);
  Result := AValue;
end;

function TExtObject.SetConfigItem(const AName, AMethodName: string; const AValue: Integer): Integer;
begin
  Session.ResponseItems.SetConfigItem(Self, AName, AMethodName, [AValue]);
  Result := AValue;
end;

function TExtObject.SetConfigItem(const AName, AMethodName: string; const AValue: Boolean): Boolean;
begin
  Session.ResponseItems.SetConfigItem(Self, AName, AMethodName, [AValue]);
  Result := AValue;
end;

function TExtObject.RequestDownload(Method : TExtProcedure) : TExtFunction; begin
  Result := RequestDownload(Method, [])
end;

{
Starts Self-translating mechanism invoking <link TExtThread.JSCode, JSCode>.
Invokes <link TExtThread.JSConcat, JSConcat> if identify a nested typecast
@param JS JS commands or declarations
@param pJSName Optional, by default is the <link TExtObject.JSName, JSName>, when used by <link TExtObjectList.Add> is the TExtObjectList.JSName
@param pOwner Optional, used by <link TExtObjectList.Add> only to pass the TExtObject owner list
}
procedure TExtObject.JSCode(JS : string; pJSName : string = ''; pOwner : string = '');
begin
  Assert(False, 'Not implemented - ' + JS);
end;

constructor TExtObject.CreateWithConfig(AOwner: TComponent; AConfig: TExtObject);
begin
  if Assigned(AConfig) then
    FCreateVarArgs := JSClassName + '(' + VarToJSON([AConfig, False], GetExtSession(AOwner)) + ');';
  Create(AOwner);
end;

{
Adds this object in a list.
If called as constructor creates the object before adds it to the list.
@param List An instanciated <link TExtObjectList>
}
constructor TExtObject.CreateAndAddTo(List : TExtObjectList);
begin
  Create(List);
  AddTo(List);
end;

// Inits a JS Object with a <link TExtFunction>
constructor TExtObject.Init(AOwner: TComponent; AMethod: TExtFunction);
begin
  Assert(Assigned(AOwner));
  Assert(Assigned(AMethod));
  inherited Create(AOwner);
  CreateJSName;
  JSCode(CommandDelim + DeclareJS + JSName + '=' + AMethod.ExtractJSCommand + ';');
  InitDefaults;
end;

// Inits a JS Object with a JS command
constructor TExtObject.Init(const AOwner: TComponent; const ACommand: string);
begin
  Assert(Assigned(AOwner));
  Assert(ACommand <> '');
  inherited Create(AOwner);
  CreateJSName;
  JSCode(CommandDelim + DeclareJS + JSName + '=' + ACommand + ';');
  InitDefaults;
end;

procedure TExtObject.InitDefaults;
begin
end;

{
Generates JS code to declare an inline JS Array.
@param JSON JavaScript Object Notation, the body of Array declaration
@param SquareBracket If true surrounds the array with []. Default is true.
@return <link TExtObjectList> to be used in assigns
}
function TExtObject.JSArray(const AJSON: string; const ASquareBrackets: Boolean): TExtObjectList;
begin
  Result := TExtObjectList.CreateAsAttribute(Self, 'dummy');
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
function TExtObject.JSObject(const AJSON: string; const AObjectConstructor: string;
  const ACurlyBrackets: Boolean): TExtObject;
begin
  Result := TExtObject.CreateInline(Self);
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

function TExtObject.AddJSReturn(Expression : string; MethodsValues : array of const) : string;
var
  Command : string;
  I : integer;
begin
  with ExtSession do
  begin
    Result := '-$7' + GetSequence + '7';
    for I := 0 to high(MethodsValues) do
      with MethodsValues[I] do
        if VType = vtObject then begin
          Command := TExtFunction(VObject).ExtractJSCommand; // FPC idiosincrasy
          {$IFNDEF UNICODE}
          VAnsiString := pointer(Command);
          VType       := vtAnsiString;
          {$ELSE}
          VUnicodeString := pointer(Command);
          VType          := vtUnicodeString;
          {$ENDIF}
        end;
    JSReturns.Values[Result] := Format(Expression, MethodsValues, _JSFormatSettings);
  end;
end;

{
Lets use a JS expression to set a ExtJS property or parameter on browser side. <link TExtFunction, ExtJS function> in ExtJS properties and parameters.
The expression will be called in the browser side and should returns an integer.
@param Expression JS Expression using or not Delphi Format specifiers: (%s, %d and etc), use %s for ExtPascal methods' results and %d for integers
@param MethodsValues Array of Methods or Integer values to use in Expression
@return Integer Internal value to return used by ExtPascal to generate the corresponding Javascript code
@example <code>
Grid := TExtGridGridPanel.Create;
with Grid do begin
  Width  := JSExpression(Panel.GetInnerWidth);
  Height := JSExpression(Panel.GetInnerHeight);
end;
FormWidth := 40;
with TExtWindow.Create do
  Width := JSExpression('%s * %d', [ExtUtilTextMetrics.GetWidth('g'), FormWidth]);
  // or Width := JSExpression('Ext.util.TextMetrics.getWidth("g") * ' + IntToStr(FormWidth), []);
</code>
}
function TExtObject.JSExpression(Expression : string; MethodsValues : array of const) : integer; begin
  Result := StrToInt(AddJSReturn(Expression, MethodsValues))
end;

// Shortcut version of JSExpression method
function TExtObject.JSExpression(Method : TExtFunction) : integer; begin
  Result := JSExpression('%s', [Method]);
end;

function TExtObject.JSString(Expression : string; MethodsValues : array of const) : string; begin
  Result := AddJSReturn('"+' + Expression + '+"', MethodsValues);
end;

function TExtObject.JSString(Method : TExtFunction) : string; begin
  Result := JSString('%s', [Method]);
end;

// Returns the JS command generated by a method
function TExtObject.JSMethod(Method : TExtFunction) : string; begin
  Result := Method.ExtractJSCommand;
end;

// Used internaly by the code generated by ExtToPascal. The generated code must overrides this method on extended classes where events can be handled
procedure TExtObject.HandleEvent(const AEvtName: string); begin end;

{
Generates JS code to declare an anonymous JS function with parameters
@param Params JS Parameters separated by commas
@param Body JS commands for JS function
@return <link TExtFunction> to use in event handlers
}
function TExtObject.JSFunction(const AParams, ABody: string): TExtFunction;
begin
  Result := TExtFunction.CreateInline(Self);
  Result.FJSName := 'function(' + AParams + ') { ' + ABody + ' }';
end;

{
Generates JS code to declare an anonymous JS function without parameters
@param Body JS commands for JS function
@return <link TExtFunction> to use in event handlers
}
function TExtObject.JSFunction(const ABody: string): TExtFunction;
begin
  Result := JSFunction('', ABody);
end;

function TExtObject.JSFunctionInline(const ACode: string): TExtFunction;
begin
  Result := TExtFunction.CreateInline(Self);
  Result.FJSName := ACode;
end;

{
Declares a named JS function with parameters.
@param Name JS function name
@param Params JS Parameters separated by commas
@param Body JS commands for JS function
}
procedure TExtObject.JSFunction(const AName, AParams, ABody: string);
begin
  Session.ResponseItems.ExecuteJSCode(Self,
    'function ' + AName + '(' + AParams + ') { ' + ABody + ' };');
end;

{
Generates an anonymous JS function from an Object Pascal procedure to use in event handlers.
To get event parameters use %0, %1 until %9 place holders.
@param Method Object Pascal procedure declared on the <link TExtThread> or in a <link TExtObject>
@param Silent Discards JS exceptions and returns immediately
@return <link TExtFunction> to use in event handlers
@example <code>
procedure TSamples.ReadButtonJS; begin
  ExtMessageBox.Alert('Browser Side: Button clicked', 'You clicked the "%0" button')
end;

procedure TSamples.MessageBoxes; begin
  with TExtPanel.Create do begin
    Title    := 'Message Boxes';
    Width    := 700;
    RenderTo := 'body';
    Frame    := true;
    with TExtButton.AddTo(Buttons) do begin
      Text    := 'Confirm Message';
      Handler := ExtMessageBox.Confirm('Confirm', 'Are you sure?', JSFunction(ReadButtonJS));
    end;
  end;
</code>
@example
<code>
procedure TSamples.SelectNodeEventBrowserSide; begin
  ExtMessageBox.Alert('Browser Side', '%0.text')
end;

procedure TSamples.BorderLayout;
var
 Tree : TExtTreeTreePanel;
 Root, Node : TExtTreeTreeNode;
begin
  Tree := TExtTreeTreePanel.Create;
  Tree.Border := false;
  //set root node
  Root := TExtTreeTreeNode.Create;
  with Root do begin
    Text := 'Root';
    AllowChildren := True;
    Expandable := True;
    Expanded := True;
    Leaf := False;
    on('click', JSFunction(SelectNodeEventBrowserSide));
  end;
: : : : : : :</code>
}
function TExtObject.JSFunction(Method : TExtProcedure; Silent : boolean = false) : TExtFunction;
begin
  Result := JSFunction(
    procedure
    begin
      Method;
    end,
    Silent);
end;

// Same as above but with anonymous method.
function TExtObject.JSFunction(const AMethod: TProc; const ASilent: Boolean): TExtFunction;
begin
  Result := TExtFunction.CreateInline(Self);
  Result.FJSName := 'function() { ' + GetJSFunctionCode(AMethod, ASilent) + ' }';
end;

// Same as above but returns the code as a string instead of executing it.
function TExtObject.GetJSFunctionCode(const AMethod: TProc; const ASilent: Boolean): string;
var
  LResponseItemBranch: TExtResponseItems;
begin
  LResponseItemBranch := ExtSession.BranchResponseItems;
  try
    AMethod;
    Result := LResponseItemBranch.Consume;
    if ASilent then
      Result := 'try { ' + Result + ' } catch(e) {};'
  finally
    ExtSession.UnbranchResponseItems(LResponseItemBranch, False);
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
function TExtObject.Ajax(const AMethod: TExtProcedure): TExtFunction;
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
procedure TExtObject.AddTo(List: TExtObjectList);
begin
  List.Add(Self);
end;

function TExtObject.Ajax(const AMethod: TExtProcedure; const AParams: array of const): TExtFunction;
var
  LMethodName: string;
  LObjectName: string;
begin
  Result := FindMethod(AMethod, LMethodName, LObjectName);
  AjaxCode(LMethodName, IfThen(LObjectName = '', '', 'Obj=' + LObjectName), AParams,  []);
end;

function TExtObject.Ajax(const AMethod: TExtProcedure; const AParams: string;
  const AAdditionalDependencies: array of TExtObject;
  const AIsEvent: Boolean): TExtFunction;
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
function TExtObject.FindMethod(Method : TExtProcedure; var PascalName, ObjName : string) : TExtFunction;
var
  Obj : TObject;
begin
  Obj := TMethod(Method).Data;
  PascalName := Obj.MethodName(@Method);
  if PascalName = '' then
    raise Exception.Create('Ajax: Method is not published')
  else begin
    if Obj is TExtObject then
      ObjName := TExtObject(Obj).JSName
    else
      ObjName := '';
    Result := TExtFunction(Self);
  end;
end;

// Ajax with raw string as params
function TExtObject.Ajax(const AMethod: TExtProcedure; const AParams: string) : TExtFunction;
var
  AMethodName, AObjectName: string;
begin
  Result := FindMethod(AMethod, AMethodName, AObjectName);
  AjaxCode(AMethodName, '"+' + AParams + IfThen(AObjectName = '', '', '+"&Obj=' + AObjectName), [], []);
end;

// Ajax with JSFunction as params
function TExtObject.AjaxExtFunction(const AMethod: TExtProcedure; const AParams: array of TExtFunction): TExtFunction;
var
  I: Integer;
  S: string;
begin
  S := '';
  for I := Low(AParams) to High(AParams) do
  begin
    S := S + AParams[I].ExtractJSCommand;
    if I <> High(AParams) then
      S := S + '+"&"+';
  end;
  Result := Ajax(AMethod, S);
end;

function TExtObject.AjaxForms(const AMethod: TExtProcedure; const AForms: array of TExtObject): TExtFunction;
var
  I: Integer;
  LParams: string;
  LResponseItemsBranch: TExtResponseItems;
  LCodeObject: TExtObject;
begin
  LParams := '';
  for I := 0 to High(AForms) do
  begin
    LResponseItemsBranch := ExtSession.BranchResponseItems;
    try
      LCodeObject := AForms[I];
      if AForms[I] is TExtFormField then
      begin
        LParams := LParams + '"' + TExtFormField(AForms[I]).Id + '="+';
        TExtFormField(AForms[I]).GetValue;
      end
      else if AForms[I] is TExtFormFormPanel then
      begin
        LCodeObject := TExtFormFormPanel(AForms[I]).GetForm;
        TExtFormFormPanel(AForms[I]).GetForm.GetValues(True);
      end
      else
        raise Exception.Create('AjaxForms only supports TExtFormField and TExtFormFormPanel.');
      LParams := LParams + '"+' + RemoveLastJSTerminator(LResponseItemsBranch.FindLastCodeItem(LCodeObject).ToString) + '+"';
    finally
      ExtSession.UnbranchResponseItems(LResponseItemsBranch, False);
    end;
    if I <> High(AForms) then
      LParams := LParams + '+"&"+';
  end;
  Result := Ajax(AMethod, LParams, AForms);
end;

function TExtObject.AjaxSelection(const AMethod: TExtProcedure; const ASelectionModel: TExtObject;
  const AAttributes, ATargetQueries: string; const AParams: array of const): TExtFunction;
var
  LMethodName, LObjectName: string;
  I: Integer;
  LParams: string;
  LAttributes: TStringList;
  LTargetQueries: TStringList;
  LBody: string;
  LResponseItemsBranch: TExtResponseItems;
begin
  Assert(Assigned(AMethod));
  Assert(Assigned(ASelectionModel));
  Assert(ASelectionModel is TExtSelectionRowModel);

  LAttributes := TStringList.Create;
  try
    LAttributes.StrictDelimiter := True;
    LAttributes.Delimiter := ',';
    LAttributes.DelimitedText := AAttributes;
    LTargetQueries := TStringList.Create;
    try
      LTargetQueries.StrictDelimiter := True;
      LTargetQueries.Delimiter := ',';
      LTargetQueries.DelimitedText := ATargetQueries;

      Assert(LAttributes.Count > 0);
      Assert(LAttributes.Count = LTargetQueries.Count);

      Result := TExtFunction(Self);
      FindMethod(AMethod, LMethodName, LObjectName);

      LResponseItemsBranch := ExtSession.BranchResponseItems;
      try
        LParams := 'Obj=' + LObjectName;
        for I := 0 to LAttributes.Count - 1 do
        begin
          LResponseItemsBranch.ExecuteJSCode(Self, Format('var Sel%d=[];', [I]));
          LBody := LBody + Format('Sel%d.push(Rec.get("' + Trim(LAttributes[I]) + '"));', [I]);
          LParams := LParams + '&' + FormatParams(LMethodName, [Trim(LTargetQueries[I]), '%' + Format('Sel%d.toString()', [I])]);
        end;
        TExtSelectionRowModel(ASelectionModel).Each(JSFunction('Rec', LBody));
        AjaxCode(LMethodName, LParams, AParams, [ASelectionModel]);
      finally
        ExtSession.UnbranchResponseItems(LResponseItemsBranch);
      end;
    finally
      FreeAndNil(LAttributes);
    end;
  finally
    FreeAndNil(LTargetQueries);
  end;
end;

function SurroundAjaxParam(Param: string): string;
var
  I: Integer;
begin
  I := Pos('%', Param);
  if (I <> 0) and (I <> Length(Param))  then
    if CharInSet(Param[I + 1], ['0'..'9']) then
      Result := '" + encodeURIComponent(' + Param + ') + "'
    else
      Result := '" + encodeURIComponent(' + Copy(Param, I + 1, Length(Param)) + ') + "'
  else
    Result := Param;
end;

function TExtObject.FormatParams(MethodName : string; Params : array of const) : string;
var
  I : integer;
begin
  Result := '';
  for I := 0 to high(Params) do
    with Params[I] do
      if Odd(I) then
        case VType of
          vtAnsiString : Result := Result + SurroundAjaxParam(string(VAnsiString));
          vtString     : Result := Result + SurroundAjaxParam(string(VString^));
          vtWideString : Result := Result + SurroundAjaxParam(string(VWideString));
          {$IFDEF UNICODE}
          vtUnicodeString : Result := Result + SurroundAjaxParam(string(VUnicodeString));
          {$ENDIF}
          vtObject     : Result := Result + '"+' + TExtObject(VObject).ExtractJSCommand + '+"';
          vtInteger    : Result := Result + IntToStr(VInteger);
          vtBoolean    : Result := Result + IfThen(VBoolean, 'true', 'false');
          vtExtended   : Result := Result + FloatToStr(VExtended^);
          vtCurrency   : Result := Result + CurrToStr(VCurrency^);
          vtInt64      : Result := Result + IntToStr(VInt64^);
          vtVariant    : Result := Result + string(VVariant^);
          vtChar       : Result := Result + string(VChar);
          vtWideChar   : Result := Result + VWideChar;
        end
      else begin
        if Result <> '' then Result := Result + '&';
        case VType of
          vtAnsiString : Result := Result + string(VAnsiString) + '=';
          vtString     : Result := Result + string(VString^) + '=';
          vtWideString : Result := Result + string(VWideString) + '=';
          {$IFDEF UNICODE}
          vtUnicodeString : Result := Result + string(VUnicodeString) + '=';
          {$ENDIF}
          vtChar       : Result := Result + string(VChar) + '=';
          vtWideChar   : Result := Result + VWideChar + '=';
        else
          Session.ResponseItems.ExecuteJSCode('Ext.Msg.show({title:"Error",msg:"Ajax method: ' + MethodName +
            ' has an invalid parameter name in place #' + IntToStr(I+1) + '",icon:Ext.Msg.ERROR,buttons:Ext.Msg.OK});');
          exit;
        end;
      end;
end;

procedure TExtObject.AjaxCode(const AMethodName, ARawParams: string; const AParams: array of const;
  const AAdditionalDependencies: array of TExtObject);
begin
  Session.ResponseItems.ExecuteJSCode(Self,
    GetAjaxCode(AMethodName, ARawParams, AParams),
    AAdditionalDependencies);
end;

function TExtObject.DoGetAjaxCode(const AMethodName, ARawParams: string; const AParams: array of const; const AExtraCode: string): string;
var
  LParams: string;
begin
  LParams := IfThen(ARawParams = '', '', ARawParams) + IfThen(Length(AParams) > 0, IfThen(ARawParams = '', '', '&') + FormatParams(AMethodName, AParams), '');

  Result :=
    'Ext.Ajax.request({' + sLineBreak +
    AExtraCode +
    '  url: "' + ExtSession.MethodURI(AMethodName) + '",' + sLinebreak +
    '  params: "' + LParams + '",' + sLineBreak +
    '  success: AjaxSuccess,' + sLineBreak +
    '  failure: AjaxFailure' + sLineBreak +
    '});';
end;

function TExtObject.GetAjaxCode(const AMethodName, ARawParams: string; const AParams: array of const): string;
begin
  Result := DoGetAjaxCode(AMethodName, ARawParams, AParams,
    '  method: "GET",' + sLineBreak
  );
end;

function TExtObject.GetPOSTAjaxCode(const AMethodName, ARawParams: string; const AParams: array of const; const AJsonData: string): string;
begin
  Result := DoGetAjaxCode(AMethodName, ARawParams, AParams,
    '  method: "POST",' + sLineBreak +
    '  jsonData: ' + AJsonData + ',' + sLineBreak
  );
end;

// Internal Ajax generation handler treating IsEvent, when is true HandleEvent will be invoked instead published methods
function TExtObject.Ajax(const AMethodName: string; const AParams: array of const;
  const AIsEvent: Boolean): TExtFunction;
var
  LParams: string;
  LMethodName: string;
begin
  Result  := TExtFunction(Self);
  LMethodName := AMethodName;
  LParams := IfThen(JSName = '', '', 'Obj=' + JSName);
  if AIsEvent then
  begin
    LParams := LParams + '&IsEvent=1&Evt=' + AMethodName;
    LMethodName := 'HandleEvent';
  end;
  AjaxCode(LMethodName, LParams, AParams, []);
end;

function TExtObject.Ajax(const AMethod: TExtProcedure;
  const AParams: array of const;
  const AAdditionalDependencies: array of TExtObject;
  const AIsEvent: Boolean): TExtFunction;
begin
  Session.ResponseItems.ExecuteJSCode(Self,
    GetAjaxCode(AMethod, AParams, AIsEvent),
    AAdditionalDependencies);
  Result := TExtFunction(Self);
end;

function TExtObject.GetAjaxCode(const AMethod: TExtProcedure;
  const AParams: array of const; const AIsEvent: Boolean): string;
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
  Result :=  GetAjaxCode(LMethodName, LParams, AParams);
end;

function TExtObject.Ajax(const AMethodName: string;
  const AParams: array of const;
  const AAdditionalDependencies: array of TExtObject;
  const AIsEvent: Boolean): TExtFunction;
var
  LParams: string;
  LMethodName: string;
begin
  Result  := TExtFunction(Self);
  LMethodName := AMethodName;
  LParams := IfThen(JSName = '', '', 'Obj=' + JSName);
  if AIsEvent then
  begin
    LParams := LParams + '&IsEvent=1&Evt=' + AMethodName;
    LMethodName := 'HandleEvent';
  end;
  AjaxCode(LMethodName, LParams, AParams, AAdditionalDependencies);
end;

{
Encapsulates JS commands in an anonymous JS function, find %0..%9 place holders and declares respective event parameters
@param Command JS command to convert to JS function
@return The code for an anonymous JS function with optional event parameters declared
}
function TExtObject.WriteFunction(Command : string) : string;
var
  I, J: Integer;
  LParams: string;
  LCommandWithoutTerminator: string;
begin
  LParams := '';
  J := -1;
  I := pos('%', Command);
  while I <> 0 do begin
    if CharInSet(Command[I+1], ['0'..'9']) then begin
      Command[I] := 'P';
      J := max(J, StrToInt(Command[I+1]));
    end;
    I := posex('%', Command, I);
  end;
  for I := 0 to J do begin
    LParams := LParams + 'P' + IntToStr(I);
    if I <> J then LParams := LParams + ','
  end;
  LCommandWithoutTerminator := RemoveLastJSTerminator(Command);
  I := LastDelimiter(';', LCommandWithoutTerminator);
  if (I = 0) and (Pos('return ', Command) <> 1) then
    Command := 'return ' + Command
  else
  begin
    Inc(I);
    while (Length(Command) > I) and CharInSet(Command[I], [#13, #10]) do
      Inc(I);
    Insert('return ', Command, I);
  end;
  Result := 'function(' + LParams + '){' + Command + '}';
end;

{
Extracts <link TExtObject.JSCommand, JSCommand> from Response and resets JSCommand
@return JSCommand without the last char ';'
@see TExtThread.RemoveJS
}
function TExtObject.ExtractJSCommand: string;
var
  LCodeItem: TExtResponseItem;
begin
  LCodeItem := Session.ResponseItems.FindLastCodeItem(Self);
  if Assigned(LCodeItem) then
  begin
    Result := RemoveLastJSTerminator(LCodeItem.ToString);
    Session.ResponseItems.Remove(LCodeItem);
  end
  else
    Result := '';
end;

{
Frees TExtObject if object is not destroyed. Automatically calls Free for all components that it does reference.
Free is successful even if called repeatedly to the same object.
@param CallDestroyJS Calls <link TExtObject.DestroyJS, DestroyJS> if true to destroy or delete the JS object too
}
procedure TExtObject.Free(CallDestroyJS : boolean = false); begin
  if (Self <> nil) then begin
    if CallDestroyJS then DestroyJS;
    inherited Free;
  end;
end;

function TExtObject.VarToJSON(const AList: TExtObjectList): string;
begin
  Result := AList.JSName;
end;

{
Converts an <link TArrayOfString, array of strings> to JSON (JavaScript Object Notation) to be used in constructors, JS Arrays or JS Objects
@param Strs An <link TArrayOfString, array of strings> to convert
@return JSON representation of Strs
}
function TExtObject.ArrayToJSON(Strs : {$IF Defined(FPC) or (RTLVersion < 20)}TArrayOfString{$ELSE}array of string{$IFEND}) : string;
var
  I : integer;
begin
  Result := '[';
  for I := 0 to high(Strs) do begin
    Result := Result + StrToJS(Strs[I]);
    if I < high(Strs) then Result := Result + ',';
  end;
  Result := Result + ']'
end;

{
Converts an <link TArrayOfInteger, array of integers> to JSON (JavaScript Object Notation) to be used in constructors, JS Arrays or JS Objects
@param Ints An <link TArrayOfInteger, array of integers> to convert
@return JSON representation of Ints
}
function TExtObject.ArrayToJSON(Ints : {$IF Defined(FPC) or (RTLVersion < 20)}TArrayOfInteger{$ELSE}array of integer{$IFEND}) : string;
var
  I : integer;
begin
  Result := '[';
  for I := 0 to high(Ints) do begin
    Result := Result + IntToStr(Ints[I]);
    if I < high(Ints) then Result := Result + ',';
  end;
  Result := Result + ']'
end;

// Aux function used internaly by ExtToPascal to override HandleEvent method
function TExtObject.ParamAsInteger(ParamName : string) : integer; begin
  Result := StrToIntDef(ExtSession.Query[ParamName], 0);
end;

// Aux function used internaly by ExtToPascal to override HandleEvent method
function TExtObject.ParamAsDouble(ParamName : string) : double; begin
  Result := StrToFloatDef(ExtSession.Query[ParamName], 0);
end;

// Aux function used internaly by ExtToPascal to override HandleEvent method
function TExtObject.ParamAsBoolean(ParamName : string) : boolean; begin
  Result := ExtSession.Query[ParamName] = 'true';
end;

// Aux function used internaly by ExtToPascal to override HandleEvent method
function TExtObject.ParamAsString(ParamName : string) : string; begin
  Result := ExtSession.Query[ParamName];
end;

// Aux function used internaly by ExtToPascal to override HandleEvent method
function TExtObject.ParamAsTDateTime(ParamName : string) : TDateTime; begin
  Result := ParamAsDouble(ParamName);
end;

// Aux function used internaly by ExtToPascal to override HandleEvent method
function TExtObject.ParamAsObject(ParamName : string) : TExtObject; begin
  Result := TExtObject(ExtSession.ObjectCatalog.FindExtObject(ExtSession.Query[ParamName]));
end;

function TExtObject.FindExtObject(const AJSName: string): TObject;
var
  I: Integer;
begin
  Assert(AJSName <> '');

  Result := nil;
  for I := 0 to ComponentCount - 1 do
  begin
    if Components[I] is TExtObject then
    begin
      if TExtObject(Components[I]).JSName = AJSName then
        Result := TExtObject(Components[I])
      else
        Result := TExtObject(Components[I]).FindExtObject(AJSName);
      if Assigned(Result) then
        Break;
    end;
  end;
end;

{ TExtResponseItem }

procedure TExtResponseItem.AddDependency(const AItem: TExtResponseItem);
begin
  if Assigned(AItem) and (AItem <> Self) and not FDependencies.Contains(AItem) then
    FDependencies.Add(AItem);
end;

procedure TExtResponseItem.AfterConstruction;
begin
  inherited;
  FDependencies := TList<TExtResponseItem>.Create;
  FEmitted := False;
  FCreationDateTime := Now;
end;

constructor TExtResponseItem.Create(const AParent: TExtResponseItems; const ASender: TExtObject);
begin
  Assert(Assigned(AParent));

  inherited Create;
  FParent := AParent;
  FSender := ASender;
end;

function TExtResponseItem.DependsOn(const AItem: TExtResponseItem): Boolean;
var
  LItem: TExtResponseItem;
begin
  for LItem in FDependencies do
    if LItem = AItem then
      Exit(True);
  Result := False;
end;

destructor TExtResponseItem.Destroy;
begin
  FreeAndNil(FDependencies);
  inherited;
end;

procedure TExtResponseItem.Emit(const AEmittedItems: TList<TExtResponseItem>);
var
  LItem: TExtResponseItem;
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

function TExtResponseItem.AllDependenciesEmitted(const AEmittedItems: TList<TExtResponseItem>): Boolean;
var
  LItem: TExtResponseItem;
begin
  for LItem in FDependencies do
    if not LItem.FEmitted then
      Exit(False);
  Result := True;
end;

function TExtResponseItem.GetDependencies: TArray<TExtResponseItem>;
begin
  Result := FDependencies.ToArray;
end;

function TExtResponseItem.GetDependency(I: Integer): TExtResponseItem;
begin
  Result := FDependencies[I];
end;

function TExtResponseItem.GetDependencyCount: Integer;
begin
  Result := FDependencies.Count;
end;

function TExtResponseItem.IsCode: Boolean;
begin
  Result := True;
end;

procedure TExtResponseItem.RemoveDependency(const AItem: TExtResponseItem);
begin
  Assert(FDependencies.Remove(AItem) >= 0);
end;

procedure TExtResponseItem.UnEmit;
begin
  FEmitted := False;
end;

{ TExtCreateObject }

procedure TExtCreateObject.AddToConfigItem(const AName: string; const AValues: array of const);
var
  I: Integer;
  LPreviousValue: string;
  LAdditionalValue: string;
begin
  Assert(Assigned(FParent));

  I := FConfigItems.IndexOfName(AName);

  LAdditionalValue := Sender.VarToJSON(AValues,
    procedure (AParam: string; AObjectParam: TObject; AIsFunction: Boolean)
    begin
      if Assigned(AObjectParam) and (AObjectParam is TExtObject) and not AIsFunction then
        AddDependency(FParent.FindObjectCreateItem(TExtObject(AObjectParam)));
    end);

  if I >= 0 then
  begin
    LPreviousValue := FConfigItems.ValueFromIndex[I];
    if LPreviousValue = '' then
      FConfigItems.ValueFromIndex[I] := '[' + LAdditionalValue + ']'
    else
    begin
      Assert(LPreviousValue[Length(LPreviousValue)] = ']');
      Insert(', ' + LAdditionalValue, LPreviousValue, Length(LPreviousValue));
      FConfigItems.ValueFromIndex[I] := LPreviousValue;
    end;
  end
  else
    FConfigItems.Add(AName + '=' + '[' + LAdditionalValue + ']');
end;

procedure TExtCreateObject.AfterConstruction;
begin
  inherited;
  FConfigItems := TStringList.Create;
  // No duplicates allowed, but we'll be checking them manually because
  // the list cannot be sorted.
  //SetConfigItem('id', [Sender.JSName]);
end;

destructor TExtCreateObject.Destroy;
var
  I: Integer;
begin
  for I := 0 to FConfigItems.Count - 1 do
    FConfigItems.Objects[I].Free;
  FreeAndNil(FConfigItems);
  inherited;
end;

procedure TExtCreateObject.InitObjectConfigItem(const AName: string);
var
  I: Integer;
begin
  I := FConfigItems.IndexOfName(AName);
  if I < 0 then
    FConfigItems.AddObject(AName + '=', TStringList.Create)
  else
    raise Exception.CreateFmt('Config item %s already initialized.', [AName]);
end;

function TExtCreateObject.IsCode: Boolean;
begin
  Result := False;
end;

procedure TExtCreateObject.SetConfigItem(const AName: string; const AValues: array of const);
var
  I: Integer;
  LAdditionalValue: string;
  LNameParts: TArray<string>;
  LIsInternal: Boolean;
  J: Integer;
begin
  Assert(Assigned(FParent));

  LNameParts := AName.Split(['.']);
  LIsInternal := Length(LNameParts) > 1;

  I := FConfigItems.IndexOfName(LNameParts[0]);

  if (I < 0) and LIsInternal then
    raise Exception.CreateFmt('Couldn''t find config item %s.', [LNameParts[0]]);

  LAdditionalValue := Sender.VarToJSON(AValues,
    procedure (AParam: string; AObjectParam: TObject; AIsFunction: Boolean)
    begin
      if Assigned(AObjectParam) and (AObjectParam is TExtObject) and not AIsFunction then
        AddDependency(FParent.FindObjectCreateItem(TExtObject(AObjectParam)));
    end);

  if I >= 0 then
  begin
    if LIsInternal then
    begin
      J := TStrings(FConfigItems.Objects[I]).IndexOfName(LNameParts[1]);
      if J >= 0 then
        TStrings(FConfigItems.Objects[I]).ValueFromIndex[J] := LAdditionalValue
      else
        TStrings(FConfigItems.Objects[I]).Add(LNameParts[1] + '=' + LAdditionalValue);
    end
    else
      FConfigItems.ValueFromIndex[I] := LAdditionalValue;
  end
  else
    FConfigItems.Add(AName + '=' + LAdditionalValue);
end;

function TExtCreateObject.ConfigItemValuesAsString(const AIndex: Integer): string;
var
  LStrings: TStrings;
  I: Integer;
begin
  Assert(Assigned(FConfigItems.Objects[AIndex]));
  Assert(FConfigItems.Objects[AIndex] is TStrings);

  LStrings := TStrings(FConfigItems.Objects[AIndex]);
  Result := '';
  for I := 0 to LStrings.Count - 1 do
  begin
    if Result <> '' then
      Result := Result + ', ';
    Result := Result + LStrings.Names[I] + ': ' + LStrings.ValueFromIndex[I];
  end;
  if Result <> '' then
    Result := FConfigItems.Names[AIndex] + ': {' + Result + '}';
end;

function TExtCreateObject.ToString: string;
var
  I: Integer;
  LObjectAsString: string;
begin
  Assert(Assigned(Sender));

  Result := '';
  for I := 0 to FConfigItems.Count - 1 do
  begin
    if FConfigItems.ValueFromIndex[I] <> '' then
      Result := Result + sLineBreak + '  ' + FConfigItems.Names[I] + ': ' + FConfigItems.ValueFromIndex[I] + ','
    else if Assigned(FConfigItems.Objects[I]) then
    begin
      LObjectAsString := ConfigItemValuesAsString(I);
      if LObjectAsString <> '' then
        Result := Result + sLineBreak + '  ' + LObjectAsString + ',';
    end;
  end;
  if Result.EndsWith(',') then
    Result := Result.Substring(0, Result.Length - 1);

  Result :=
    Sender.JSName + ' = ' + Sender.GetConstructionJS + '({' +
    Result + sLineBreak +
    '});' + sLineBreak +
    Sender.JSName + '.nm = "' + Sender.JSName + '";' + sLineBreak;
end;

{ TExtJSCode }

procedure TExtJSCode.AfterConstruction;
begin
  inherited;
end;

destructor TExtJSCode.Destroy;
begin
  inherited;
end;

function TExtJSCode.ToString: string;
begin
  Result := inherited ToString + sLineBreak;
end;

{ TExtResponseItems }

procedure TExtResponseItems.AddHTML(const AHTML: string);
var
  LItem: TExtHTML;
begin
  LItem := TExtHTML.Create(Self, nil);
  try
    LItem.HTML := AHTML;
    FList.Add(LItem);
  except
    FreeAndNil(LItem);
    raise;
  end;
end;

procedure TExtResponseItems.AddJSON(const AJSON: string);
var
  LItem: TExtJSON;
begin
  LItem := TExtJSON.Create(Self, nil);
  try
    LItem.JSON := AJSON;
    FList.Add(LItem);
  except
    FreeAndNil(LItem);
    raise;
  end;
end;

procedure TExtResponseItems.AddToList(const AContainer: TExtObject;
  const AList: TExtObjectList; const AItemName, AMethodName: string;
  const AValues: array of const);
var
  LContainerCreateItem: TExtCreateObject;
begin
  Assert(Assigned(AContainer));
  Assert(AItemName <> '');
  Assert(AMethodName <> '');
  Assert((Length(AValues) = 1) or (Length(AValues) = 2));

  LContainerCreateItem := FindObjectCreateItem(AContainer);
  if Assigned(LContainerCreateItem) then
    LContainerCreateItem.AddToConfigItem(AItemName, AValues)
  else
    CallMethod(AContainer, AMethodName, AValues);
end;

procedure TExtResponseItems.AddToList(const AContainer: TExtObject;
  const AItemName: string; const AValues: array of const);
var
  LContainerCreateItem: TExtCreateObject;
begin
  Assert(Assigned(AContainer));
  Assert(AItemName <> '');
  Assert((Length(AValues) = 1) or (Length(AValues) = 2));

  try
    LContainerCreateItem := GetObjectCreateItem(AContainer);
    LContainerCreateItem.AddToConfigItem(AItemName, AValues);
  except
    on E: Exception do
      raise Exception.CreateFmt('Cannot add object %s to config item %s.%s. %s', [TExtObject(AValues[0].VObject).JSName, AContainer.JSName, AItemName, E.Message]);
  end;
end;

procedure TExtResponseItems.AfterConstruction;
begin
  inherited;
  FList := TList<TExtResponseItem>.Create;
  FEmittedItems := TList<TExtResponseItem>.Create;
end;

procedure TExtResponseItems.CallMethod(const AObject: TExtObject;
  const AMethodName: string; const AParams: array of const);
var
  LItem: TExtCallMethod;
begin
  LItem := TExtCallMethod.Create(Self, AObject);
  try
    LItem.AddDependency(FindObjectCreateItem(TExtObject(AObject)));
    LItem.CallName := AMethodName;
    AObject.VarToJSON(AParams,
      procedure (AParam: string; AObjectParam: TObject; AIsFunction: Boolean)
      begin
        LItem.CallParams.Add(AParam);
        if Assigned(AObjectParam) and (AObjectParam is TExtObject) and not AIsFunction then
          LItem.AddDependency(FindObjectCreateItem(TExtObject(AObjectParam)));
      end
    );
    FList.Add(LItem);
  except
    FreeAndNil(LItem);
    raise;
  end;
end;

procedure TExtResponseItems.CreateInternalObject(const AObject: TExtObject; const AAttributeName: string);
var
  LObjectCreateItem: TExtCreateObject;
begin
  LObjectCreateItem := GetObjectCreateItem(AObject.Owner as TExtObject);
  LObjectCreateItem.InitObjectConfigItem(AAttributeName);
end;

procedure TExtResponseItems.CreateObject(const AObject: TExtObject);
begin
  Assert(Assigned(AObject));

  FList.Add(TExtCreateObject.Create(Self, AObject));
end;

destructor TExtResponseItems.Destroy;
begin
  FreeAndNil(FEmittedItems);
  Clear;
  FreeAndNil(FList);
  inherited;
end;

procedure TExtResponseItems.ExecuteJSCode(const AJSCode: string);
begin
  ExecuteJSCode(nil, AJSCode);
end;

procedure TExtResponseItems.ExecuteJSCode(const AObject: TExtObject;
  const AJSCode: string);
begin
  ExecuteJSCode(AObject, AJSCode, []);
end;

procedure TExtResponseItems.ExecuteJSCode(const AObject: TExtObject; const AJSCode: string;
  const AAdditionalDependencies: array of TExtObject);
var
  LItem: TExtJSCode;
  I: Integer;
begin
  if AJSCode <> '' then
  begin
    LItem := TExtJSCode.Create(Self, AObject);
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

function TExtResponseItems.ToString: string;
var
  I: Integer;
  //LEmittedString: string;

//  procedure AppendStringToStream(const AString: string; const AStream: TStream;
//    const AEncoding: TEncoding);
//  var
//    LBuffer: TBytes;
//  begin
//    Assert(Assigned(AStream));
//
//    if AEncoding <> nil then
//      LBuffer := AEncoding.GetBytes(AString)
//    else
//      LBuffer := TEncoding.Default.GetBytes(AString);
//    AStream.WriteBuffer(LBuffer[0], Length(LBuffer));
//  end;
//
//  procedure StringToTextFile(const AString, AFileName: string;
//    const AEncoding: TEncoding);
//  var
//    LFilePath: string;
//    LStream: TStream;
//  begin
//    LFilePath := ExtractFilePath(AFileName);
//    if LFilePath <> '' then
//      ForceDirectories(LFilePath);
//
//    LStream := TFileStream.Create(AFileName, fmCreate);
//    try
//      AppendStringToStream(AString, LStream, AEncoding);
//    finally
//      LStream.Free;
//    end;
//  end;

begin
  //SortByDependency;
  FEmittedItems.Clear;
  //LEmittedString := '';

  for I := 0 to FList.Count - 1 do
    FList[I].Emit(FEmittedItems);

  //FEmittedItems.AddRange(FList.ToArray);

  //Assert(FEmittedItems.Count = FList.Count);

  Result := '';
  for I := 0 to FEmittedItems.Count - 1 do
  begin
    //Result := Result + '// ' + FormatDateTime('hh:nn:ss.zzz', LItem.FCreationDateTime) + sLineBreak;
    Result := Result + FEmittedItems[I].ToString;
    //LEmittedString := LEmittedString + LItem.Sender.JSName + '-' + LItem.ClassName + sLineBreak;
  end;
  FEmittedItems.Clear;
  for I := 0 to FList.Count - 1 do
    FList[I].UnEmit;
  //StringToTextFile(LEmittedString, 'c:\users\nandod\ethea\kitto\graph.txt', TEncoding.Unicode);
end;

function TExtResponseItems.Consume: string;
begin
  Result := ToString;
  Clear;
end;

procedure TExtResponseItems.Clear;
begin
  while FList.Count > 0 do
  begin
    FList[0].Free;
    FList.Delete(0);
  end;
end;

function TExtResponseItems.FindObjectCreateItem(const AObject: TExtObject): TExtCreateObject;
var
  LResponseItem: TExtResponseItem;
begin
  for LResponseItem in FList do
    if (LResponseItem is TExtCreateObject) and (TExtCreateObject(LResponseItem).Sender = AObject) then
      Exit(TExtCreateObject(LResponseItem));
  Result := nil;
end;

function TExtResponseItems.FindLastCodeItem(const AObject: TExtObject): TExtResponseItem;
var
  I: Integer;
begin
  for I := FList.Count - 1 downto 0 do
    if (FList[I].Sender = AObject) and FList[I].IsCode then
      Exit(FList[I]);
  Result := nil;
end;

function TExtResponseItems.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TExtResponseItems.GetItem(I: Integer): TExtResponseItem;
begin
  Result := FList[I];
end;

function TExtResponseItems.GetLastCodeItem(
  const AObject: TExtObject): TExtResponseItem;
begin
  Result := FindLastCodeItem(AObject);
  if Result = nil then
    raise Exception.CreateFmt('No code item found for object %s.', [AObject.JSName]);
end;

function TExtResponseItems.GetObjectCreateItem(const AObject: TExtObject): TExtCreateObject;
begin
  Assert(Assigned(AObject));

  Result := FindObjectCreateItem(AObject);
  if not Assigned(Result) then
    raise Exception.CreateFmt('Object %s was not created in this request.', [AObject.JSName]);
end;

procedure TExtResponseItems.GetProperty(const AObject: TExtObject;
  const APropertyName: string);
var
  LItem: TExtGetProperty;
begin
  LItem := TExtGetProperty.Create(Self, AObject);
  try
    LItem.PropertyName := APropertyName;
    LItem.AddDependency(FindObjectCreateItem(AObject));
    FList.Add(LItem);
  except
    FreeAndNil(LItem);
  end;
end;

procedure TExtResponseItems.Remove(const AItem: TExtResponseItem);
begin
  Assert(Assigned(AItem));

  if FList.Remove(AItem) < 0 then
    raise Exception.Create('No item found to remove.');
  AItem.Free;
end;

procedure TExtResponseItems.RemoveAll(const AObject: TExtObject);
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

procedure TExtResponseItems.SetConfigItem(const AObject: TExtObject;
  const AItemName: string; const AValues: array of const);
var
  LObjectCreateItem: TExtCreateObject;
  LItemName: string;
begin
  Assert(Assigned(AObject));
  Assert(AItemName <> '');
  Assert((Length(Avalues) = 1) or (Length(AValues) = 2));

  // Internal objects have no top-level create item.
  if Assigned(AObject.ExtObjectOwner) then
  begin
    LObjectCreateItem := FindObjectCreateItem(AObject.ExtObjectOwner);
    LItemName := AObject.AttributeName + '.' + AItemName;
  end
  else
  begin
    LObjectCreateItem := FindObjectCreateItem(AObject);
    LItemName := AItemName;
  end;

  if Assigned(LObjectCreateItem) then
    LObjectCreateItem.SetConfigItem(LItemName, AValues)
  else
    raise Exception.CreateFmt('Cannot set config item %s.%s. Object was not created in this request.', [AObject.JSName, AItemName]);
end;

procedure TExtResponseItems.SetConfigItem(const AObject: TExtObject;
  const AItemName, AMethodName: string; const AValues: array of const);
var
  LObjectCreateItem: TExtCreateObject;
begin
  Assert(Assigned(AObject));
  Assert(AItemName <> '');
  Assert(AMethodName <> '');

  LObjectCreateItem := FindObjectCreateItem(AObject);
  if Assigned(LObjectCreateItem) then
    LObjectCreateItem.SetConfigItem(AItemName, AValues)
  else
    CallMethod(AObject, AMethodName, AValues);
end;

procedure TExtResponseItems.SetConfigItemOrProperty(const AObject: TExtObject;
  const AItemName: string; const AValues: array of const);
var
  LObjectCreateItem: TExtCreateObject;
begin
  Assert(Assigned(AObject));
  Assert(AItemName <> '');

  LObjectCreateItem := FindObjectCreateItem(AObject);
  if Assigned(LObjectCreateItem) then
    LObjectCreateItem.SetConfigItem(AItemName, AValues)
  else
    SetProperty(AObject, AItemName, AValues);
end;

procedure TExtResponseItems.SetProperty(const AObject: TExtObject;
  const APropertyName: string; const AValues: array of const);
var
  LItem: TExtSetProperty;
begin
  LItem := TExtSetProperty.Create(Self, AObject);
  try
    LItem.AddDependency(FindObjectCreateItem(AObject));
    LItem.PropertyName := APropertyName;
    LItem.PropertyValue := AObject.VarToJSON(AValues);
    AObject.VarToJSON(AValues,
      procedure (AParam: string; AObjectParam: TObject; AIsFunction: Boolean)
      begin
        if Assigned(AObjectParam) and (AObjectParam is TExtObject) and not AIsFunction then
          LItem.AddDependency(FindObjectCreateItem(TExtObject(AObjectParam)));
      end
    );
    FList.Add(LItem);
  except
    FreeAndNil(LItem);
  end;
end;

//procedure TExtResponseItems.SortByDependency;
//var
//  LList: TList<TExtResponseItem>;
//  LTopLevelNodes: TQueue<TExtResponseItem>;
//  LItem: TExtResponseItem;
//  LDependents: TArray<TExtResponseItem>;
//  LDependent: TExtResponseItem;
//
//  procedure GetTopLevelNodes;
//  var
//    LItem: TExtResponseItem;
//  begin
//    LTopLevelNodes.Clear;
//    for LItem in FList do
//      if LItem.DependencyCount = 0 then
//        LTopLevelNodes.Enqueue(LItem);
//  end;
//
//  function GetDependentNodes(const AItem: TExtResponseItem): TArray<TExtResponseItem>;
//  var
//    LItem: TExtResponseItem;
//  begin
//    SetLength(Result, 0);
//    for LItem in FList do
//      if LItem.DependsOn(AItem) then
//      begin
//        SetLength(Result, Length(Result) + 1);
//        Result[High(Result)] := LItem;
//      end;
//  end;
//
//  function DependenciesExist: Boolean;
//  var
//    LItem: TExtResponseItem;
//  begin
//    for LItem in FList do
//      if LItem.DependencyCount > 0 then
//        Exit(True);
//    Result := False;
//  end;
//
//  function GetDebugString(const AList: TList<TExtResponseItem>): string;
//  var
//    LItem: TExtResponseItem;
//    I: Integer;
//
//    function GetItemDescription(const AItem: TExtResponseItem): string;
//    begin
//      Result := AItem.Sender.JSName + '-' + AItem.ClassName;
//    end;
//
//  begin
//    Result := '';
//    for LItem in AList do
//    begin
//      if LItem.DependencyCount > 0 then
//      begin
//        Result := Result + GetItemDescription(LItem) + '  DEPENDS ON  ';
//        for I := 0 to LItem.DependencyCount - 1 do
//          Result := Result + ' ' + GetItemDescription(LItem.Dependencies[I]);
//        Result := Result + sLinebreak;
//      end;
//    end;
//  end;
//
//begin
//  (* Topological sort
//     Courtesy Wikipedia: http://en.wikipedia.org/wiki/Topological_sorting
//
//  L ← Empty list that will contain the sorted elements
//  S ← Set of all nodes with no incoming edges
//  while S is non-empty do
//      remove a node n from S
//      insert n into L
//      for each node m with an edge e from n to m do
//          remove edge e from the graph
//          if m has no other incoming edges then
//              insert m into S
//  if graph has edges then
//      return error (graph has at least one cycle)
//  else
//      return L (a topologically sorted order)
//  end;
//  *)
//  LList := TList<TExtResponseItem>.Create;
//  try
//    LTopLevelNodes := TQueue<TExtResponseItem>.Create;
//    try
//      GetTopLevelNodes;
//      while LTopLevelNodes.Count > 0 do
//      begin
//        LItem := LTopLevelNodes.Dequeue;
//        LList.Add(LItem);
//        LDependents := GetDependentNodes(LItem);
//        for LDependent in LDependents do
//        begin
//          LDependent.RemoveDependency(LItem);
//          if LDependent.DependencyCount = 0 then
//            LTopLevelNodes.Enqueue(LDependent);
//        end;
//      end;
//      if DependenciesExist then
//        raise Exception.CreateFmt('Cannot sort response items by dependency - Graph cycle detected.#13#10%s', [GetDebugString(FList)]);
//      FList.Clear;
//      FList.AddRange(LList.ToArray);
//    finally
//      FreeAndNil(LTopLevelNodes);
//    end;
//  finally
//    FreeAndNil(LList);
//  end;
//end;

{ TExtCallMethod }

procedure TExtCallMethod.AfterConstruction;
begin
  inherited;
  FCallParams := TStringList.Create;
end;

destructor TExtCallMethod.Destroy;
begin
  FreeAndNil(FCallParams);
  inherited;
end;

procedure TExtCallMethod.SetCallParams(const AValue: TStrings);
begin
  FCallParams.Assign(AValue);
end;

function TExtCallMethod.ToString: string;
begin
  Assert(Assigned(Sender));
  Assert(FCallName <> '');

  Result := Sender.JSName + '.' +  FCallName + '(' + Join(FCallParams.ToStringArray, ', ') + ');' + sLineBreak;
end;

{ TExtSetProperty }

function TExtSetProperty.ToString: string;
begin
  Assert(Assigned(Sender));
  Assert(FPropertyName <> '');
  Assert(FPropertyValue <> '');

  Result := Sender.JSName + '.' +  FPropertyName + ' = ' + FPropertyValue + ';' + sLineBreak;
end;

{ TExtGetProperty }

function TExtGetProperty.ToString: string;
begin
  Assert(Assigned(Sender));
  Assert(FPropertyName <> '');

  Result := Sender.JSName + '.' + FPropertyName + ';' + sLineBreak;
end;

{ TExtTextBase }

function TExtTextBase.ToString: string;
begin
  Result := FText;
end;

function TExtObject.SetConfigItem(const AName: string; const AValue: Integer): Integer;
begin
  Session.ResponseItems.SetConfigItem(Self, AName, [AValue]);
  Result := AValue;
end;

function TExtObject.SetConfigItem(const AName: string; const AValue: Boolean): Boolean;
begin
  Session.ResponseItems.SetConfigItem(Self, AName, [AValue]);
  Result := AValue;
end;

function TExtObject.SetConfigItem(const AName: string; const AValue: Double): Double;
begin
  Session.ResponseItems.SetConfigItem(Self, AName, [AValue]);
  Result := AValue;
end;

function TExtObject.SetFunctionConfigItem(const AName, AMethodName: string; const AValue: TExtFunction): TExtFunction;
begin
  Session.ResponseItems.SetConfigItem(Self, AName, AMethodName, [AValue, True]);
  Result := AValue;
end;

function TExtObject.SetConfigItem(const AName, AMethodName: string; const AValue: TExtObject): TExtObject;
begin
  Session.ResponseItems.SetConfigItem(Self, AName, AMethodName, [AValue, False]);
  Result := AValue;
end;

function TExtObject.SetConfigItemOrProperty(const AName: string; const AValue: Boolean): Boolean;
begin
  Session.ResponseItems.SetConfigItemOrProperty(Self, AName, [AValue]);
  Result := AValue;
end;

function TExtObject.SetProperty(const AName: string; const AValue: TExtFunction): TExtFunction;
begin
  Session.ResponseItems.SetProperty(Self, AName, [AValue, True]);
  Result := AValue;
end;

function TExtObject.SetProperty(const AName: string; const AValue: TExtObject): TExtObject;
begin
  Session.ResponseItems.SetProperty(Self, AName, [AValue, False]);
  Result := AValue;
end;

function TExtObject.SetProperty(const AName: string; const AValue: Boolean): Boolean;
begin
  Session.ResponseItems.SetProperty(Self, AName, [AValue]);
  Result := AValue;
end;

function TExtObject.SetProperty(const AName, AValue: string): string;
begin
  Session.ResponseItems.SetProperty(Self, AName, [AValue]);
  Result := AValue;
end;

function TExtObject.SetProperty(const AName: string; const AValue: Integer): Integer;
begin
  Session.ResponseItems.SetProperty(Self, AName, [AValue]);
  Result := AValue;
end;

function TExtObject.SetConfigItemOrProperty(const AName, AValue: string): string;
begin
  Session.ResponseItems.SetConfigItemOrProperty(Self, AName, [AValue]);
  Result := AValue;
end;

function TExtObject.SetConfigItem(const AName: string; const AValue: TExtObject): TExtObject;
begin
  Session.ResponseItems.SetConfigItem(Self, AName, [AValue, False]);
  Result := Self;
end;

function TExtObject.SetConfigItem(const AName, AMethodName: string; const AValue: TDateTime): TDateTime;
begin
  Session.ResponseItems.SetConfigItem(Self, AName, [AValue]);
  Result := AValue;
end;

function TExtObject.SetProperty(const AName: string; const AValue: TDateTime): TDateTime;
begin
  Session.ResponseItems.SetProperty(Self, AName, [AValue]);
  Result := AValue;
end;

function TExtObject.CallFunctionMethod(const AName: string; const AValue: TExtFunction): TExtFunction;
begin
  Session.ResponseItems.CallMethod(Self, AName, [AValue, True]);
  Result := TExtFunction(Self);
end;

function TExtObject.CallMethod(const AName: string; const AValue: TExtObject): TExtFunction;
begin
  Session.ResponseItems.CallMethod(Self, AName, [AValue, False]);
  Result := TExtFunction(Self);
end;

initialization
  //ExtUtilTextMetrics.FJSName := 'TextMetrics';
  {$IF RTLVersion > 21}
  _JSFormatSettings := TFormatSettings.Create;
  {$ELSE}
  GetLocaleFormatSettings(GetThreadLocale, _JSFormatSettings);
  {$IFEND}
  _JSFormatSettings.DecimalSeparator := '.';

end.
