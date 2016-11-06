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
  , HTTPApp

  , EF.Tree
  , EF.Intf
  , EF.ObserverIntf
  , EF.Localization
  , EF.Macros

  , gnugettext

  , Kitto.Config
  , Kitto.Metadata.Views
  , Kitto.JS.Types
  , Kitto.JS.Formatting
  ;

type
  TJSSession = class;

  TJSUploadedFile = class
  strict private
    FContext: TObject;
    FFullFileName: string;
    FFileName: string;
    FStream: TBytesStream;
    FOriginalFileName: string;
    function GetBytes: TBytes;
  public
    constructor Create(const AFileName, AFullFileName: string;
      const AContext: TObject; const AOriginalFileName: string = '');
    destructor Destroy; override;
    property FileName: string read FFileName;
    property FullFileName: string read FFullFileName;
    property OriginalFileName: string read FOriginalFileName;
    property Context: TObject read FContext;

    property Bytes: TBytes read GetBytes;
  end;

  {$M+}
  TJSBase = class(TEFSubjectAndObserver)
  private
    FJSSession: TJSSession;
    FOwner: TJSBase;
    FChildren: TObjectList<TJSBase>;
    FJSName: string;
    FDestroying: Boolean;
    FDestroyingChildren: Boolean;
    function GetJSSession: TJSSession;
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
    { TODO : Move to a different class? }
    function AppendObjectURIParam(const AURI, AObjectName: string): string;
  protected
    procedure CreateJSName;
    function GetObjectNamePrefix: string; virtual;
    procedure InitDefaults; virtual;
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

    { TODO : move to request? }
    function ParamAsInteger(const AParamName: string): Integer;
    function ParamAsBoolean(const AParamName: string): Boolean;
    function ParamAsString(const AParamName: string): string;
    function ParamAsObject(const AParamName: string): TJSObject;

    procedure HandleEvent(const AEventName: string); virtual;
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
  ///  This class serves two purposes: redirects localization calls to a
  ///  per-session instance of dxgettext so we can have per-session language
  ///  selection, and configures Kitto's localization scheme based on two text
  ///  domains (the application's default.mo and Kitto's own Kitto.mo). The
  ///  former is located under the application home directory, the latter
  ///  under the system home directory.
  /// </summary>
  TKExtSessionLocalizationTool = class(TEFNoRefCountObject, IInterface,
    IEFInterface, IEFLocalizationTool)
  private const
    KITTO_TEXT_DOMAIN = 'Kitto';
  private
    function GetGnuGettextInstance: TGnuGettextInstance;
  public
    function AsObject: TObject;
    function TranslateString(const AString: string;
      const AIdString: string = ''): string;
    procedure TranslateComponent(const AComponent: TComponent);
    procedure ForceLanguage(const ALanguageId: string);
    function GetCurrentLanguageId: string;
    procedure AfterConstruction; override;
  end;

  TJSRequestInfo = class
    UserAgent: string;
    ClientAddress: string;
  end;

  /// <summary>
  ///  Represents the server side of a user client session.
  ///  Holds all objects pertaining to the user session.
  /// </summary>
  TJSSession = class(TJSBase)
  private
    FSessionId: string;
    FObjectSequences: TDictionary<string, Cardinal>;
    FLibraries, FLanguage: string;
    FResponseItemsStack: TStack<TJSResponseItems>;
    FSingletons: TDictionary<string, TJSObject>;
    FMobileBrowserDetectionDone: Boolean;
    FIsMobileApple: Boolean;
    FGlobal: TJSObject;
    FIsDownload: Boolean;
    FIsUpload: Boolean;
    FUploadPath: string;
    FNameSpace: string;
    FContentType: string;
    FMaxUploadSize: Integer;
    FFileUploadedFullName: string;
    FFileUploaded: string;
    FLastRequestDateTime: TDateTime;
    FRefreshingLanguage: Boolean;
    FHomeController: TObject;
    FLoginController: TObject;
    FViewportWidthInInches: Integer;
    FAutoOpenViewName: string;
    FAuthData: TEFNode;
    FIsAuthenticated: Boolean;
    FAuthMacroExpander: TEFTreeMacroExpander;
    FOpenControllers: TObjectList<TObject>;
    FControllerHostWindow: TJSObject;
    FViewHost: TObject;
    FStatusHost: TObject;
    FUploadedFiles: TObjectList<TJSUploadedFile>;
    FHomeViewNodeName: string;
    FViewportContent: string;
    FIsMobileBrowser: Boolean;
    FViewportWidth: Integer;
    FGettextInstance: TGnuGettextInstance;
    FDynamicScripts: TStringList;
    FDynamicStyles: TStringList;
    FDisplayName: string;
    FLastRequestInfo: TJSRequestInfo;
    function GetDisplayName: string;
    function GetResponseItems: TJSResponseItems;
    function GetGlobal: TJSObject;
    procedure SetNameSpace(const AValue: string);
    procedure SetLanguage(const AValue: string);
    /// <summary>
    ///  If the specifield css file name exists, generates code that
    ///  adds it to the page and adds that code to the current response.
    ///  If called multiple times, only the first time the file is added.
    /// </summary>
    procedure EnsureDynamicStyle(const AStyleBaseName: string);
    /// <summary>
    ///  If the specifield script file name exists, generates code that
    ///  adds it to the page and adds that code to the current response.
    ///  If called multiple times, only the first time the file is added.
    /// </summary>
    procedure EnsureDynamicScript(const AScriptBaseName: string);
  strict protected
    { TODO : temporary - refactoring due }
    procedure AfterNewSession; virtual;
  protected
    function GetNextJSName(const AObjectType: string): string;
    function GetViewportContent: string; virtual;
    function GetManifestFileName: string; virtual;
    function GetCustomJS: string; virtual;
  public
    /// <summary>
    ///  Calls AProc for each uploaded file in list.
    /// </summary>
    procedure EnumUploadedFiles(const AProc: TProc<TJSUploadedFile>);
    /// <summary>
    ///  Called to signal that a new file has been uploaded. The
    ///  descriptor holds information about the file and its context
    ///  (for example which view is going to use it).
    /// </summary>
    /// <remarks>
    ///  The session acquires ownership of the descriptor object.
    /// </remarks>
    procedure AddUploadedFile(const AFileDescriptor: TJSUploadedFile);
    /// <summary>
    ///  Removes a previously added file descriptor. To be called once
    ///  the uploaded file has been processed.
    /// </summary>
    procedure RemoveUploadedFile(const AFileDescriptor: TJSUploadedFile);
    /// <summary>
    ///  Returns the first uploaded file descriptor matching the
    ///  specified context, or nil if no descriptor is found.
    /// </summary>
    function FindUploadedFile(const AContext: TObject): TJSUploadedFile;
    procedure SetLanguageFromQueriesOrConfig(const AConfig: TKConfig);
    property RefreshingLanguage: Boolean read FRefreshingLanguage write FRefreshingLanguage;
    property UploadPath: string read FUploadPath write FUploadPath;
    function BeforeHandleRequest: Boolean; virtual;
    procedure OnError(const AMessage, AMethodName, AParams: string);

    function MethodURI(const AMethodName: string): string; overload;
    function MethodURI(const AMethod: TJSProcedure): string; overload;

    property IsDownload: Boolean read FIsDownload write FIsDownload;
    property IsUpload: Boolean read FIsUpload write FIsUpload;

    // Optional namespace to allow more sessions of the same application
    // in the same web page. It is set as an additional $<namespace> path in the initial URL
    // and then it is a) added as part of the URL to all requests from the session and
    // b) used with the session GUID to locate a request's session.
    // A valid namespace must begin with $.
    property NameSpace: string read FNameSpace write SetNameSpace;

    procedure OnNotFoundError(const AMethodName: string);
    property ContentType: string read FContentType write FContentType;
    property MaxUploadSize: Integer read FMaxUploadSize write FMaxUploadSize;
    property FileUploaded: string read FFileUploaded;
    property FileUploadedFullName: string read FFileUploadedFullName;
    function GetViewportWidthInInches: TJSExpression;
    function GetDefaultViewportWidth: Integer;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
    procedure Alert(const AMessage: string);
    property Language: string read FLanguage write SetLanguage;
    procedure ErrorMessage(const AMessage: string; const AAction: string = '');
    procedure Refresh; virtual;

    property ResponseItems: TJSResponseItems read GetResponseItems;
    function HasResponseItems: Boolean;
    function BranchResponseItems: TJSResponseItems;
    procedure UnbranchResponseItems(const AResponseItems: TJSResponseItems; const AConsolidate: Boolean = True);

    function GetSingleton<T: TJSObject>(const AName: string): T;
    { TODO : move to request }
    function IsMobileApple: Boolean;
    property Global: TJSObject read GetGlobal;

    /// <summary>
    ///  Gives access to a copy of the auth data that was last passed
    ///  to Authenticate (and possibly modified by the object during
    ///  authentication).
    /// </summary>
    property AuthData: TEFNode read FAuthData;
    /// <summary>
    ///  Returns True if authentication has successfully taken
    ///  place.
    /// </summary>
    property IsAuthenticated: Boolean read FIsAuthenticated write FIsAuthenticated;
    property AuthMacroExpander: TEFTreeMacroExpander read FAuthMacroExpander;
    /// <summary>
    ///  A reference to the main view container. Implements IKExtViewHost.
    /// </summary>
    property ViewHost: TObject read FViewHost write FViewHost;
    /// <summary>
    ///  A reference to the status bar to be used for wait messages.
    ///  It is of type TKExtStatusBar.
    /// </summary>
    property StatusHost: TObject read FStatusHost write FStatusHost;
    property ControllerHostWindow: TJSObject read FControllerHostWindow write FControllerHostWindow;
    /// <summary>
    ///  The current session's UUID.
    /// </summary>
    property SessionId: string read FSessionId {temporary} write FSessionId;
    property OpenControllers: TObjectList<TObject> read FOpenControllers;
    property HomeController: TObject read FHomeController write FHomeController;
    property LoginController: TObject read FLoginController write FLoginController;
    property ViewportWidthInInches: Integer read FViewportWidthInInches write FViewportWidthInInches;
    property AutoOpenViewName: string read FAutoOpenViewName write FAutoOpenViewName;
    property HomeViewNodeName: string read FHomeViewNodeName write FHomeViewNodeName;
    property Libraries: string read FLibraries write FLibraries;
    property ViewportContent: string read FViewportContent write FViewportContent;
    /// <summary>
    ///  True if the last request came from a mobile browser.
    ///  The user agent detection is performed once per session and then cached.
    /// </summary>
    function IsMobileBrowser: Boolean;
    /// <summary>
    ///  Viewport width in mobile applications.
    /// </summary>
    property ViewportWidth: Integer read FViewportWidth write FViewportWidth;
    /// <summary>
    ///  True if tooltips are enabled for the session. By default, tooltips
    ///  are enabled for desktop browsers and disabled for mobile browsers.
    /// </summary>
    function TooltipsEnabled: Boolean;
    /// <summary>
    ///  Ensures that existing js and css files with the specified base name
    ///  are dynamically added to the page. If the specified files don't exist
    ///  or were already added, nothing is done.
    /// </summary>
    procedure EnsureSupportFiles(const ABaseName: string);

    /// <summary>
    ///  Ensures that existing js and css files with a base name that depends
    ///  on the specified view are dynamically added to the page.
    ///  If the view has a 'SupportBaseName' attribute, then it is used as the
    //   base name for the support files, otherwise the view's name (if any)
    ///  is used.
    ///  If the specified files don't exist or were already added, nothing is done.
    /// </summary>
    procedure EnsureViewSupportFiles(const AView: TKView);

    /// <summary>
    ///  If the specified object is found in the list of open controllers,
    ///  it is removed from the list. Otherwise nothing happens.
    ///  Used by view hosts to notify the session that a controller was closed.
    /// </summary>
    procedure RemoveController(const AObject: TObject);
    property DisplayName: string read GetDisplayName write FDisplayName;

    property LastRequestInfo: TJSRequestInfo read FLastRequestInfo;
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

    procedure AddJSON(const AJSON: string);

    procedure AddHTML(const AHTML: string);

    function AsFormattedString: string;
    function Consume: string;

    function FindObjectCreateItem(const AObject: TJSObject): TJSCreateObject;

    procedure ForEach(const AProc: TProc<TJSResponseItem>);

    property Items[I: Integer]: TJSResponseItem read GetItem; default;
    property Count: Integer read GetCount;
    procedure Clear;
  end;

  TJSResponseItem = class(TJSBase)
  private
    FSender: TJSObject;
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
    procedure ChangeSender(const ASender: TJSObject);
    procedure InternalFormatTo(const AFormatter: TJSFormatter); virtual;
  public
    constructor Create(const ASender: TJSObject; const ARoot: TJSResponseItems); reintroduce; virtual;
    procedure AfterConstruction; override;
    destructor Destroy; override;
    property Sender: TJSObject read FSender;
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
  end;

  TJSNamedCreateObject = record
    Name: string;
    CreateObject: TJSCreateObject;
  end;

//  TJSCreateObjectParams = record
//    JSConfig: TJSValues;
//    IsInline: Boolean;
//    IsInternal: Boolean;
//    JSName: string;
//    JSClassName: string;
//  end;

  TJSCreateObject = class(TJSResponseItem)
  private
//    FCreateParams: TJSCreateObjectParams;
    FItems: TList<TJSNamedCreateObject>;
  strict protected
    procedure InternalFormatTo(const AFormatter: TJSFormatter); override;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;

    function IsCode: Boolean; override;

    procedure CreateInternalObject(const AAttributeName: string; const ASender: TJSObject);

    function FindObjectCreateItem(const ASender: TJSObject): TJSCreateObject;
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
  strict protected
    procedure InternalFormatTo(const AFormatter: TJSFormatter); override;
  public
    procedure AfterConstruction; override;
  public
    property CallName: string read FCallName write FCallName;

    function AddParam(const AValue: string): TJSMethodCall; overload;
    function AddParam(const AValue: Boolean): TJSMethodCall; overload;
    function AddParam(const AValue: Integer): TJSMethodCall; overload;
    function AddParam(const AValue: TJSObject): TJSMethodCall; overload;
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
    function AddParam(const AName: string; const AValue: TJSObject): TJSAjaxCall; overload;
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
  end;

  THTML = class(TJSTextBase)
  public
    property HTML: string read FText write FText;
  end;

function Session: TJSSession;

implementation

uses
  StrUtils
  , DateUtils
  , Math
  , Types
  , Character
  , REST.Utils
  , EF.StrUtils
  , EF.SysUtils
  , EF.Logger
  , Ext.Util // for ExtUtilTextMetrics. Switch to pure JS?
  , Ext.Base
  , Kitto.AccessControl
  , Kitto.Web
  , Kitto.Web.Request
  , Kitto.Ext.Controller
  ;

var
  _JSFormatSettings: TFormatSettings;

function Session: TJSSession;
begin
  if Assigned(TKWebServer.CurrentSession) then
    Result := TKWebServer.CurrentSession.Content.Objects[
      TKWebServer.CurrentSession.Content.IndexOf(TKWebServer.SESSION_OBJECT)] as TJSSession
  else
    Result := nil;
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

//procedure TJSCreateObject.UnlinkFromSender(const AConfig: TJSValues);
//begin
//  Assert(Assigned(Sender));
//  Assert(Assigned(AConfig));
//
//  AConfig.Owner := Self;
//  FCreateParams.JSConfig := AConfig;
//  FCreateParams.IsInline := Sender.IsInline;
//  FCreateParams.IsInternal := Sender.IsInternal;
//  FCreateParams.JSName := Sender.JSName;
//  FCreateParams.JSClassName := Sender.JSClassName;
//
//  ChangeSender(nil);
//end;

procedure TJSCreateObject.CreateInternalObject(const AAttributeName: string; const ASender: TJSObject);
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
  if not Sender.IsInline and not Sender.IsInternal then
  begin
    LJSName := Sender.JSName;
    AFormatter.AddIndentedLine(LJSName + ' = new ' + Sender.JSClassName + '(');
    AFormatter.Indent.AddIndent;
    LConstructionAdded := True;
  end;

  { TODO : what if it's empty? }
  AFormatter.OpenObject;
  Sender.JSConfig.FormatTo(AFormatter);
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

procedure TJSResponseItems.AddObjectDependency(const ADependentObject, ADependedUponObject: TJSObject);
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

procedure TJSResponseItems.ForEach(const AProc: TProc<TJSResponseItem>);
var
  LItem: TJSResponseItem;
begin
  Assert(Assigned(AProc));
  
  for LItem in FList do
    AProc(LItem);
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
    Result.AddDependency(FindObjectCreateItem(AObject));
    Result.PropertyName := APropertyName;
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
      AItem.AddDependency(FindObjectCreateItem(AValue));
      AItem.NameValue.Values.SetObject(AName, AValue);
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

function TJSMethodCall.GetDebugDescription: string;
begin
  Result := inherited GetDebugDescription + '.' + CallName;
end;

procedure TJSMethodCall.InternalFormatTo(const AFormatter: TJSFormatter);
begin
  inherited;
  if not IsExpressionExtracted then
  begin
    Assert(FCallName <> '');
    Assert(Assigned(Sender));

    AFormatter.AddIndented(Sender.JSName + '.' + FCallName);
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

{ TExtSetProperty }

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

{ TJSSession }

procedure TJSSession.SetNameSpace(const AValue: string);
begin
  FNameSpace := StripSuffix(AValue, '/');
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
  ResponseItems.Clear;
  FGlobal := nil;
  FreeAllChildren;
  FObjectSequences.Clear;
  FSingletons.Clear;

  FHomeController := nil;
  FLoginController := nil;
  FOpenControllers.Clear;
  FViewHost := nil;
  FStatusHost := nil;
  FDynamicScripts.Clear;
  FDynamicStyles.Clear;
  FreeAndNil(FControllerHostWindow);
end;

function TJSObject.MethodURI(const AMethod: TJSProcedure): string;
var
  LMethodName: string;
  LObjectName: string;
begin
  FindMethod(AMethod, LMethodName, LObjectName);
  Result := AppendObjectURIParam(JSSession.MethodURI(LMethodName), LObjectName);
end;

function TJSObject.MethodURI(const AMethodName: string): string;
begin
  Result := AppendObjectURIParam(JSSession.MethodURI(AMethodName), JSName);
end;

function TJSObject.AppendObjectURIParam(const AURI, AObjectName: string): string;
begin
  Result := AURI;
  if AObjectName <> '' then
  begin
    if Pos('?', Result) <> 0 then
      Result := Result + '&Object=' + AObjectName
    else
      Result := Result + '?Object=' + AObjectName;
  end;
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
  FLastRequestDateTime := Now;
  Result := True;
  if FLanguage = '' then
  begin // Set language
    FLanguage := TKWebRequest.Current.GetFieldByName('Accept-Language');
    I := pos('-', FLanguage);
    if I <> 0 then
    begin
      FLanguage := Copy(FLanguage, I - 2, 2) + '_' + Uppercase(Copy(FLanguage, I + 1, 2));
{ TODO : extjs path? }
//      if not FileExists(RequestHeader['DOCUMENT_ROOT'] + ExtPath + '/build/classic/locale/locale-' + FLanguage + '.js') then
//        FLanguage := Copy(FLanguage, 1, 2)
    end;
  end;
//  if IsAjax then
//  begin
//    if NewThread or RequiresReload then
//    begin
//      ErrorMessage('Session expired or lost.<br/>A new session will be created now.', 'window.location.reload()');
//      RequiresReload := True;
//      Result := False;
//    end
//  end
//  else
//    RequiresReload := False;
end;

function TJSSession.GetGlobal: TJSObject;
begin
  if not Assigned(FGlobal) then
    FGlobal := TJSObject.CreateInline(Self);
  Result := FGlobal;
end;

function TJSSession.BranchResponseItems: TJSResponseItems;
begin
  Result := TJSResponseItems.Create(nil);
  FResponseItemsStack.Push(Result);

  Assert(FResponseItemsStack.Count > 0);
end;

destructor TJSSession.Destroy;
var
  LUploadDirectory: string;
begin
  // Delete upload folder only for valid sessions.
  if FSessionId <> '' then
  begin
{ TODO : figure out our document root }
//    LUploadDirectory := ReplaceStr(DocumentRoot + UploadPath, '/', '\');
    if DirectoryExists(LUploadDirectory) then
      DeleteTree(LUploadDirectory);
  end;
  Assert(FResponseItemsStack.Count = 1);
  FResponseItemsStack.Pop.Free;
  FreeAndNil(FResponseItemsStack);
  FreeAndNil(FObjectSequences);
  FreeAndNil(FSingletons);
  FreeAndNil(FGlobal);
  FreeAndNil(FAuthMacroExpander);
  FreeAndNil(FAuthData);
  FreeAndNil(FUploadedFiles);
  FreeAndNil(FHomeController);
  FreeAndNil(FLoginController);
  FreeAndNil(FGettextInstance);
  FreeAndNil(FDynamicScripts);
  FreeAndNil(FDynamicStyles);
  FreeAndNil(FLastRequestInfo);
  inherited;
  // Keep it alive as the inherited call might trigger calls to
  // RemoveController from objects being destroyed.
  FreeAndNil(FOpenControllers);
end;

procedure TJSSession.OnNotFoundError(const AMethodName: string);
begin
  Alert(Format('Method: ''%s'' not found', [AMethodName]));
end;

function TJSSession.IsMobileApple: Boolean;
var
  LUserAgent: string;
begin
  if not FMobileBrowserDetectionDone then
  begin
    LUserAgent := TKWebRequest.Current.GetFieldByName('User-Agent');
    FIsMobileApple := LUserAgent.Contains('iPhone') or LUserAgent.Contains('iPad');
    FMobileBrowserDetectionDone := True;
  end;
  Result := FIsMobileApple;
end;

function TJSSession.TooltipsEnabled: Boolean;
begin
  Result := not Session.IsMobileBrowser;
end;

function TJSSession.IsMobileBrowser: Boolean;
var
  LUserAgent: string;
begin
  if not FMobileBrowserDetectionDone then
  begin
    LUserAgent := TKWebRequest.Current.UserAgent;
    TEFLogger.Instance.Log('UserAgent: ' + LUserAgent);
    FIsMobileBrowser := LUserAgent.Contains('Windows Phone') or
      LUserAgent.Contains('iPhone') or
      LUserAgent.Contains('iPad') or
      LUserAgent.Contains('Android');
    FMobileBrowserDetectionDone := True;
    TEFLogger.Instance.Log('IsMobileBrowser: ' + BoolToStr(FIsMobileBrowser, True));
  end;
  Result := FIsMobileBrowser;
end;

function TJSSession.GetViewportWidthInInches: TJSExpression;
begin
  Result := Global.JSExpressionFromCodeBlock('getViewportWidthInInches()');
end;

function TJSSession.GetDefaultViewportWidth: Integer;
begin
  Result := FViewportWidthInInches * 96;
//  case FViewportWidthInInches of
//    0..4: Result := 320;
//    5..7: Result := 480;
//    8..10: Result := 640;
//  else
//    Result := 0;
//  end;
end;

function TJSSession.MethodURI(const AMethodName: string): string;
begin
  //Assert(Assigned(FApplication));

  Result := '/';//FApplication.BasePath;
  if NameSpace <> '' then
    Result := Result + NameSpace + '/';
  Result := Result + AMethodName;
end;

function TJSSession.MethodURI(const AMethod: TJSProcedure): string;
begin
  { TODO : switch to new-style Rtti. }
  Result := MethodName(@AMethod);
  if Result <> '' then
    Result := MethodURI(Result)
  else
    raise Exception.Create('MethodURI: Method is not published');
end;

procedure TJSSession.EnumUploadedFiles(const AProc: TProc<TJSUploadedFile>);
var
  I: Integer;
begin
  if Assigned(AProc) then
  begin
    for I := FUploadedFiles.Count - 1 downto 0 do
      AProc(FUploadedFiles[I]);
  end;
end;

procedure TJSSession.RemoveUploadedFile(const AFileDescriptor: TJSUploadedFile);
begin
  FUploadedFiles.Remove(AFileDescriptor);
end;

procedure TJSSession.RemoveController(const AObject: TObject);
begin
  FOpenControllers.Remove(AObject);
end;

procedure TJSSession.SetLanguage(const AValue: string);
begin
  FLanguage := AValue;
  TEFLocalizationToolRegistry.CurrentTool.ForceLanguage(FLanguage);
  TEFLogger.Instance.LogFmt('Language %s set.', [FLanguage], TEFLogger.LOG_MEDIUM);
  //Config.Config.SetString('LanguageId', AValue);
end;

procedure TJSSession.SetLanguageFromQueriesOrConfig(const AConfig: TKConfig);
var
  LLanguageId: string;
begin
  LLanguageId := Global.ParamAsString('lang');
  if LLanguageId = '' then
    LLanguageId := AConfig.Config.GetString('LanguageId');
  if LLanguageId <> '' then
    Language := LLanguageId;
end;

procedure TJSSession.AddUploadedFile(const AFileDescriptor: TJSUploadedFile);
begin
  FUploadedFiles.Add(AFileDescriptor);
end;

function TJSSession.GetDisplayName: string;
begin
  Result := FDisplayName;
  if Result = '' then
    Result := SessionId;
end;

function TJSSession.HasResponseItems: Boolean;
begin
  Result := Assigned(FResponseItemsStack);
end;

procedure TJSSession.AfterConstruction;
begin
  inherited;
  FLastRequestInfo := TJSRequestInfo.Create;

  FMobileBrowserDetectionDone := False;

  FDynamicScripts := TStringList.Create;
  FDynamicScripts.Sorted := True;
  FDynamicScripts.Duplicates := dupError;
  FDynamicStyles := TStringList.Create;
  FDynamicStyles.Sorted := True;
  FDynamicStyles.Duplicates := dupError;

  FResponseItemsStack := TStack<TJSResponseItems>.Create;
  FResponseItemsStack.Push(TJSResponseItems.Create(nil));

  FObjectSequences := TDictionary<string, Cardinal>.Create;
  FSingletons := TDictionary<string, TJSObject>.Create;

  FAuthData := TEFNode.Create;
  FAuthMacroExpander := TEFTreeMacroExpander.Create(FAuthData, 'Auth');

  FGettextInstance := TGnuGettextInstance.Create;

  FUploadedFiles := TObjectList<TJSUploadedFile>.Create;
  FOpenControllers := TObjectList<TObject>.Create(False);

  AfterNewSession;
end;

function TJSSession.GetCustomJS: string;
begin
  Result := '';
end;

procedure TJSSession.EnsureDynamicScript(const AScriptBaseName: string);
var
  LIndex: Integer;
  LURL: string;
begin
  if not FDynamicScripts.Find(AScriptBaseName, LIndex) then
  begin
    LURL := TKWebApplication.Current.Config.FindResourceURL(IncludeTrailingPathDelimiter('js') + AScriptBaseName + '.js');
    if LURL <> '' then
    begin
      ResponseItems.ExecuteJSCode(Format('addScriptRef("%s");', [LURL]));
      FDynamicScripts.Add(AScriptBaseName);
    end;
  end;
end;

procedure TJSSession.EnsureDynamicStyle(const AStyleBaseName: string);
var
  LIndex: Integer;
  LURL: string;
begin
  if not FDynamicStyles.Find(AStyleBaseName, LIndex) then
  begin
    LURL := TKWebApplication.Current.Config.FindResourceURL(IncludeTrailingPathDelimiter('js') + AStyleBaseName + '.css');
    if LURL <> '' then
    begin
      ResponseItems.ExecuteJSCode(Format('addLinkRef("%s");', [LURL]));
      FDynamicStyles.Add(AStyleBaseName);
    end;
  end;
end;

procedure TJSSession.EnsureSupportFiles(const ABaseName: string);
begin
  if ABaseName <> '' then
  begin
    EnsureDynamicStyle(ABaseName);
    EnsureDynamicScript(ABaseName);
  end;
end;

procedure TJSSession.EnsureViewSupportFiles(const AView: TKView);
var
  LBaseName: string;
  LBaseNameNode: TEFNode;
begin
  LBaseName := '';
  if Assigned(AView) then
  begin
    LBaseNameNode := AView.FindNode('SupportBaseName');
    if Assigned(LBaseNameNode) then
      LBaseName := LBaseNameNode.AsString
    else
      LBaseName := AView.PersistentName;
  end;
  if LBaseName <> '' then
    EnsureSupportFiles(LBaseName);
end;

function TJSSession.GetManifestFileName: string;
begin
  Result := '';
end;

procedure TJSSession.AfterNewSession;
begin
  FUpLoadPath := '/uploads';
{ TODO : implement }
//  UploadPath := '/uploads/' + Config.AppName + '/' + SessionGUID;
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
    Result := TJSObjectClass(T).CreateSingleton(Self, AName) as T;
    FSingletons.Add(AName, Result);
  end;
end;

function TJSSession.FindUploadedFile(const AContext: TObject): TJSUploadedFile;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FUploadedFiles.Count - 1 do
  begin
    if FUploadedFiles[I].Context = AContext then
    begin
      Result := FUploadedFiles[I];
      Break;
    end;
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
    Session.ResponseItems.ExecuteJSCode(JSName + '.destroy(); delete ' + JSName + ';');
end;

procedure TJSObject.DependsUpon(const AObject: TJSObject);
begin
  JSSession.ResponseItems.AddObjectDependency(Self, AObject);
end;

destructor TJSObject.Destroy;
begin
  if (Session <> nil) and Session.HasResponseItems then
    Session.ResponseItems.ForEach(
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
var
  LParams, LMethodName, LObjectName: string;
begin
  { TODO: see if we can refactor this code to use MethodURI }
  FindMethod(AMethod, LMethodName, LObjectName);
  LParams := '';
  if LObjectName <> '' then
    LParams := '?Object=' + LObjectName;
  if Session.IsMobileApple then
    Result := 'window.open("' + Session.MethodURI(LMethodName) + LParams + '");'
  else
    Result := 'Download.src="' + Session.MethodURI(LMethodName) + LParams + '";';
end;

procedure TJSObject.Download(Method: TJSProcedure);
begin
  Session.ResponseItems.ExecuteJSCode(Self, GetDownloadJS(Method));
end;

constructor TJSObject.Create(const AOwner: TJSBase);
begin
  Assert(Session <> nil);
  Assert(Assigned(AOwner));
  inherited Create(AOwner);
  FJSConfig := TJSValues.Create(Self);
  CreateJSName;
  Session.ResponseItems.CreateObject(Self);
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
  Result := Session.ResponseItems.AjaxCallMethod(Self, AName);
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

function TJSObject.ParamAsBoolean(const AParamName: string): Boolean;
begin
  Result := TKWebRequest.Current.ContentFields.Values[AParamName] = 'true';
end;

function TJSObject.ParamAsInteger(const AParamName: string): Integer;
begin
  Result := StrToIntDef(TKWebRequest.Current.ContentFields.Values[AParamName], 0);
end;

function TJSObject.ParamAsObject(const AParamName: string): TJSObject;
begin
  Result := TJSObject(JSSession.FindChildByJSName(
    TKWebRequest.Current.ContentFields.Values[AParamName]));
end;

function TJSObject.ParamAsString(const AParamName: string): string;
begin
  Result := TKWebRequest.Current.ContentFields.Values[AParamName];
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
    Session.ResponseItems.SetProperty(Self, AName, AValue)
  else
    FJSConfig.Values.SetBoolean(AName, AValue);
  Result := AValue;
end;

function TJSObject.SetProperty(const AName: string; const AValue: TJSExpression): TJSExpression;
begin
  Session.ResponseItems.SetProperty(Self, AName, AValue);
  Result := AValue;
end;

function TJSObject.SetProperty(const AName: string; const AValue: TJSObject): TJSObject;
begin
  Session.ResponseItems.SetProperty(Self, AName, AValue);
  Result := AValue;
end;

function TJSObject.SetProperty(const AName: string; const AValue: Boolean): Boolean;
begin
  Session.ResponseItems.SetProperty(Self, AName, AValue);
  Result := AValue;
end;

function TJSObject.SetProperty(const AName, AValue: string): string;
begin
  Session.ResponseItems.SetProperty(Self, AName, AValue);
  Result := AValue;
end;

function TJSObject.SetProperty(const AName: string; const AValue: Integer): Integer;
begin
  Session.ResponseItems.SetProperty(Self, AName, AValue);
  Result := AValue;
end;

function TJSObject.SetConfigItemOrProperty(const AName, AValue: string): string;
begin
  if FJSConfig.IsReadOnly then
    Session.ResponseItems.SetProperty(Self, AName, AValue)
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
  Session.ResponseItems.SetProperty(Self, AName, AValue);
  Result := AValue;
end;

function TJSObject.CallMethod(const AName: string): TJSMethodCall;
begin
  Result := Session.ResponseItems.CallMethod(Self, AName);
end;

function TJSObject.GenerateAnonymousFunction(const AExpression: TJSExpression): TJSExpression;
begin
  Result := GenerateAnonymousFunction('', AExpression, '');
end;

function TJSObject.GenerateAnonymousFunction(const ABody: string): TJSExpression;
begin
  Result := GenerateAnonymousFunction('', ABody, '');
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

procedure TJSAjaxCall.InternalFormatTo(const AFormatter: TJSFormatter);
begin
  //inherited;
  if not IsExpressionExtracted then
  begin
    Assert(CallName <> '');

    AFormatter.SkipLine.Indent;
    AFormatter.AddIndented('Ext.Ajax.request(').SkipLine.Indent.AddIndent.OpenObject;
    AFormatter.AddIndentedPairLine('url', Sender.MethodURI(CallName));
    if FHttpMethod <> 'GET' then
      AFormatter.AddIndentedPairLine('method', FHttpMethod);
    if (FHttpMethod = 'POST') and (FPostData <> '') then
      AFormatter.AddIndentedPairLine('jsonData', FPostData);
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
  if FJSSession = nil then
    FJSSession := Kitto.JS.Session;
  if FJSSession = nil then
    raise Exception.CreateFmt('Session not found for object %s of type %s.', [JSName, ClassName]);
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

{ TKExtSessionLocalizationTool }

procedure TKExtSessionLocalizationTool.AfterConstruction;
begin
  inherited;
  // Configure the global dxgettext instance.
  GetGnuGettextInstance.bindtextdomain(KITTO_TEXT_DOMAIN,
    TKConfig.SystemHomePath + 'locale');
end;

function TKExtSessionLocalizationTool.AsObject: TObject;
begin
  Result := Self;
end;

procedure TKExtSessionLocalizationTool.ForceLanguage(const ALanguageId: string);
var
  LInstance: TGnuGettextInstance;
begin
  LInstance := GetGnuGettextInstance;
  // Configure the per-session dxgettext instance.
  LInstance.bindtextdomain(KITTO_TEXT_DOMAIN,
    TKConfig.SystemHomePath + 'locale');
  LInstance.UseLanguage(ALanguageId);
end;

function TKExtSessionLocalizationTool.GetCurrentLanguageId: string;
begin
  Result := GetGnuGettextInstance.GetCurrentLanguage;
end;

function TKExtSessionLocalizationTool.GetGnuGettextInstance: TGnuGettextInstance;
begin
  if Session <> nil then
    Result := Session.FGettextInstance
  else
    Result := gnugettext.DefaultInstance;
end;

procedure TKExtSessionLocalizationTool.TranslateComponent(const AComponent: TComponent);
var
  LInstance: TGnuGettextInstance;
begin
  LInstance := GetGnuGettextInstance;
  LInstance.TranslateComponent(AComponent, KITTO_TEXT_DOMAIN);
  LInstance.TranslateComponent(AComponent, 'default');
end;

function TKExtSessionLocalizationTool.TranslateString(const AString,
  AIdString: string): string;
var
  LInstance: TGnuGettextInstance;
begin
  // Look in the Kitto text domain first, then in the application domain.
  LInstance := GetGnuGettextInstance;
  Result := LInstance.dgettext(KITTO_TEXT_DOMAIN, AString);
  if Result = AString then
    Result := LInstance.dgettext('default', AString);
end;

{ TJSUploadedFile }

constructor TJSUploadedFile.Create(const AFileName, AFullFileName: string;
  const AContext: TObject; const AOriginalFileName: string = '');
begin
  inherited Create;
  FFileName := AFileName;
  FFullFileName := AFullFileName;
  FContext := AContext;
  FOriginalFileName := AOriginalFileName;
end;

destructor TJSUploadedFile.Destroy;
begin
  FreeAndNil(FStream);
  inherited;
end;

function TJSUploadedFile.GetBytes: TBytes;
begin
  if not Assigned(FStream) then
  begin
    FStream := TBytesStream.Create;
    FStream.LoadFromFile(FFullFileName);
  end;
  Result := FStream.Bytes;
  // Reset length, as FStream.Bytes for some reason is rounded up.
  SetLength(Result, FStream.Size);
  Assert(FStream.Size = Length(Result));
end;

initialization
  _JSFormatSettings := TFormatSettings.Create;
  _JSFormatSettings.DecimalSeparator := '.';

  TEFLocalizationToolRegistry.RegisterTool(TKExtSessionLocalizationTool.Create);

finalization
  TEFLocalizationToolRegistry.UnregisterTool;

end.
