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
  TJSExpression = class;
  TJSObjectArray = class;
  TJSFunction = class;

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
    procedure InitDefaults; virtual;
    procedure InitInlineDefaults; virtual;
    function CreateConfigObject(const AAttributeName: string): TJSObject;
    function CreateConfigObjectArray(const AAttributeName: string): TJSObjectArray;
    procedure DoHandleEvent(const AEventName: string); virtual;
  protected
    procedure DependsUpon(const AObject: TJSObject);
  public
    constructor Create(const AOwner: TJSBase); override;
    constructor CreateInternal(const AOwner: TJSBase; const AAttributeName: string);
    constructor CreateInline(const AOwner: TJSBase);
    constructor CreateSingleton(const AOwner: TJSBase; const AAttributeName: string);
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

    procedure HandleEvent;
  end;
  TJSObjectClass = class of TJSObject;

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
    property Count: Integer read GetCount;
  end;

  /// <summary>
  ///  This class serves two purposes: redirects localization calls to a
  ///  per-session instance of dxgettext so we can have per-session language
  ///  selection, and configures Kitto's localization scheme based on two text
  ///  domains (the application's default.mo and Kitto's own Kitto.mo). The
  ///  former is located under the application home directory, the latter
  ///  under the system home directory.
  /// </summary>
  TJSLocalizationTool = class(TEFNoRefCountObject, IInterface, IEFInterface, IEFLocalizationTool)
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

  /// <summary>
  ///  Keeps track of some request data to be accessed after the request is destroyed.
  /// </summary>
  TJSRequestInfo = class
    UserAgent: string;
    ClientAddress: string;
    DateTime: TDateTime;
  end;

//  TJSUploadedFile = class
//  strict private
//    FContext: TObject;
//    FFullFileName: string;
//    FFileName: string;
//    FStream: TBytesStream;
//    FOriginalFileName: string;
//    function GetBytes: TBytes;
//  public
//    constructor Create(const AFileName, AFullFileName: string;
//      const AContext: TObject; const AOriginalFileName: string = '');
//    destructor Destroy; override;
//    property FileName: string read FFileName;
//    property FullFileName: string read FFullFileName;
//    property OriginalFileName: string read FOriginalFileName;
//    property Context: TObject read FContext;
//
//    property Bytes: TBytes read GetBytes;
//  end;

  /// <summary>
  ///  Represents the server side of a user client session.
  ///  Holds all objects pertaining to the user session.
  /// </summary>
  TJSSession = class(TJSBase)
  private
    FSessionId: string;
    FObjectSequences: TDictionary<string, Cardinal>;
    FLanguage: string;
    FSingletons: TDictionary<string, TJSObject>;
//    FUploadPath: string;
//    FUploadLocalPath: string;
//    FMaxUploadSize: Integer;
//    FFileUploadedFullName: string;
//    FFileUploaded: string;
    FRefreshingLanguage: Boolean;
    FHomeController: IJSController;
    FLoginController: IJSController;
    FViewportWidthInInches: Integer;
    FAutoOpenViewName: string;
    FAuthData: TEFNode;
    FIsAuthenticated: Boolean;
    FOpenControllers: TList<IJSController>;
    FControllerHostWindow: IJSContainer;
    FControllerContainer: IJSControllerContainer;
    FStatusHost: IJSStatusHost;
//    FUploadedFiles: TObjectList<TJSUploadedFile>;
    FHomeViewNodeName: string;
    FViewportContent: string;
    FViewportWidth: Integer;
    FGettextInstance: TGnuGettextInstance;
    FDynamicScripts: TStringList;
    FDynamicStyles: TStringList;
    FDisplayName: string;
    FLastRequestInfo: TJSRequestInfo;
    FCreationDateTime: TDateTime;
    function GetDisplayName: string;
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
    function GetViewportContent: string; virtual;
    function GetManifestFileName: string; virtual;
  protected
    function GetNextJSName(const AObjectType: string): string;
  public
    /// <summary>
    ///  Calls AProc for each uploaded file in list.
    /// </summary>
//    procedure EnumUploadedFiles(const AProc: TProc<TJSUploadedFile>);
    /// <summary>
    ///  Called to signal that a new file has been uploaded. The
    ///  descriptor holds information about the file and its context
    ///  (for example which view is going to use it).
    /// </summary>
    /// <remarks>
    ///  The session acquires ownership of the descriptor object.
    /// </remarks>
//    procedure AddUploadedFile(const AFileDescriptor: TJSUploadedFile);
    /// <summary>
    ///  Removes a previously added file descriptor. To be called once
    ///  the uploaded file has been processed.
    /// </summary>
//    procedure RemoveUploadedFile(const AFileDescriptor: TJSUploadedFile);
    /// <summary>
    ///  Returns the first uploaded file descriptor matching the
    ///  specified context, or nil if no descriptor is found.
    /// </summary>
//    function FindUploadedFile(const AContext: TObject): TJSUploadedFile;
    procedure SetLanguageFromQueriesOrConfig(const AConfig: TKConfig);
    property RefreshingLanguage: Boolean read FRefreshingLanguage write FRefreshingLanguage;
//    property UploadPath: string read FUploadPath write FUploadPath;
    procedure BeforeHandleRequest;

//    property MaxUploadSize: Integer read FMaxUploadSize write FMaxUploadSize;
//    property FileUploaded: string read FFileUploaded;
//    property FileUploadedFullName: string read FFileUploadedFullName;
    function GetDefaultViewportWidth: Integer;
  public
    constructor Create(const ASessionId: string); reintroduce;
    procedure AfterConstruction; override;
    destructor Destroy; override;

    property CreationDateTime: TDateTime read FCreationDateTime;
    property Language: string read FLanguage write SetLanguage;
//    procedure Refresh;

    function GetSingleton<T: TJSObject>(const AName: string): T;

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

    /// <summary>
    ///  A reference to the main container of controllers.
    /// </summary>
    property ControllerContainer: IJSControllerContainer read FControllerContainer write FControllerContainer;
    /// <summary>
    ///  A reference to the status bar to be used for wait messages.
    ///  It is of type TKExtStatusBar.
    /// </summary>
    property StatusHost: IJSStatusHost read FStatusHost write FStatusHost;
    property ControllerHostWindow: IJSContainer read FControllerHostWindow write FControllerHostWindow;
    /// <summary>
    ///  The current session's UUID.
    /// </summary>
    property SessionId: string read FSessionId;
    property OpenControllers: TList<IJSController> read FOpenControllers;
    property HomeController: IJSController read FHomeController write FHomeController;
    property LoginController: IJSController read FLoginController write FLoginController;
    property ViewportWidthInInches: Integer read FViewportWidthInInches write FViewportWidthInInches;
    property AutoOpenViewName: string read FAutoOpenViewName write FAutoOpenViewName;
    property HomeViewNodeName: string read FHomeViewNodeName write FHomeViewNodeName;
    property ViewportContent: string read FViewportContent write FViewportContent;
    /// <summary>
    ///  Viewport width in mobile applications.
    /// </summary>
    property ViewportWidth: Integer read FViewportWidth write FViewportWidth;
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
    procedure RemoveController(const AController: IJSController);
    property DisplayName: string read GetDisplayName write FDisplayName;

    property LastRequestInfo: TJSRequestInfo read FLastRequestInfo;
  end;

function Session: TJSSession;

implementation

uses
  StrUtils
  , DateUtils
  , Math
  , Types
  , Character
  , IOUtils
  , REST.Utils
  , System.NetEncoding
  , EF.StrUtils
  , EF.SysUtils
  , EF.Logger
  , Kitto.AccessControl
  , Kitto.Web.Application
  , Kitto.Web.Server
  , Kitto.Web.Request
  , Kitto.Web.Response
  , Kitto.Ext.Controller
  ;

var
  _JSFormatSettings: TFormatSettings;

function Session: TJSSession;
begin
  Result := TKWebServer.CurrentKittoSession as TJSSession;
end;

{ TJSSession }

function TJSSession.GetViewportContent: string;
begin
  Result := '';
end;

//procedure TJSSession.Refresh;
//begin
//  inherited;
//  TKWebResponse.Current.Items.Clear;
//  FreeAllChildren;
//  FObjectSequences.Clear;
//  FSingletons.Clear;
//
//  FHomeController := nil;
//  FLoginController := nil;
//  FOpenControllers.Clear;
//  FControllerContainer := nil;
//  FStatusHost := nil;
//  FDynamicScripts.Clear;
//  FDynamicStyles.Clear;
//  FreeAndNilEFIntf(FControllerHostWindow);
//end;

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

procedure TJSSession.BeforeHandleRequest;
var
  I: Integer;
begin
  if FLanguage = '' then
  begin
    FLanguage := TKWebRequest.Current.GetFieldByName('Accept-Language');
    I := Pos('-', FLanguage);
    if I <> 0 then
    begin
      // Convert language code
      FLanguage := Copy(FLanguage, I - 2, 2) + '_' + Uppercase(Copy(FLanguage, I + 1, 2));
{ TODO : extjs path? }
//      if not FileExists(RequestHeader['DOCUMENT_ROOT'] + ExtPath + '/build/classic/locale/locale-' + FLanguage + '.js') then
//        FLanguage := Copy(FLanguage, 1, 2)
    end;
  end;
end;

constructor TJSSession.Create(const ASessionId: string);
begin
  Assert(ASessionId <> '');

  inherited Create(nil);
  FSessionId := ASessionId;
//  FUploadPath := '/uploads/' + FSessionId;
//  FUploadLocalPath := TPath.Combine(TKWebApplication.Current.Config.AppHomePath,
//    TPath.Combine('Uploads', FSessionId));
end;

destructor TJSSession.Destroy;
begin
  // Delete upload folder only for valid sessions.
//  if DirectoryExists(FUploadLocalPath) then
//    DeleteTree(FUploadLocalPath);
  FreeAndNil(FOpenControllers);
  FreeAndNil(FObjectSequences);
  FreeAndNil(FSingletons);
  FreeAndNil(FAuthData);
//  FreeAndNil(FUploadedFiles);
  NilEFIntf(FHomeController);
  NilEFIntf(FLoginController);
  FreeAndNil(FGettextInstance);
  FreeAndNil(FDynamicScripts);
  FreeAndNil(FDynamicStyles);
  FreeAndNil(FLastRequestInfo);
  inherited;
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

//procedure TJSSession.EnumUploadedFiles(const AProc: TProc<TJSUploadedFile>);
//var
//  I: Integer;
//begin
//  if Assigned(AProc) then
//  begin
//    for I := FUploadedFiles.Count - 1 downto 0 do
//      AProc(FUploadedFiles[I]);
//  end;
//end;
//
//procedure TJSSession.RemoveUploadedFile(const AFileDescriptor: TJSUploadedFile);
//begin
//  FUploadedFiles.Remove(AFileDescriptor);
//end;
//
//procedure TJSSession.AddUploadedFile(const AFileDescriptor: TJSUploadedFile);
//begin
//  FUploadedFiles.Add(AFileDescriptor);
//end;
//
//function TJSSession.FindUploadedFile(const AContext: TObject): TJSUploadedFile;
//var
//  I: Integer;
//begin
//  Result := nil;
//  for I := 0 to FUploadedFiles.Count - 1 do
//  begin
//    if FUploadedFiles[I].Context = AContext then
//    begin
//      Result := FUploadedFiles[I];
//      Break;
//    end;
//  end;
//end;

procedure TJSSession.RemoveController(const AController: IJSController);
begin
  if Assigned(FOpenControllers) then
    FOpenControllers.Remove(AController);
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
  LLanguageId := TKWebRequest.Current.GetQueryField('lang');
  if LLanguageId = '' then
    LLanguageId := AConfig.Config.GetString('LanguageId');
  if LLanguageId <> '' then
    Language := LLanguageId;
end;

function TJSSession.GetDisplayName: string;
begin
  Result := FDisplayName;
  if Result = '' then
    Result := SessionId;
end;

procedure TJSSession.AfterConstruction;
begin
  inherited;
  FLastRequestInfo := TJSRequestInfo.Create;
  FCreationDateTime := Now;

  FDynamicScripts := TStringList.Create;
  FDynamicScripts.Sorted := True;
  FDynamicScripts.Duplicates := dupError;
  FDynamicStyles := TStringList.Create;
  FDynamicStyles.Sorted := True;
  FDynamicStyles.Duplicates := dupError;

  FObjectSequences := TDictionary<string, Cardinal>.Create;
  FSingletons := TDictionary<string, TJSObject>.Create;

  FAuthData := TEFNode.Create;

  FGettextInstance := TGnuGettextInstance.Create;

//  FUploadedFiles := TObjectList<TJSUploadedFile>.Create;
  FOpenControllers := TList<IJSController>.Create;
end;

procedure TJSSession.EnsureDynamicScript(const AScriptBaseName: string);
var
  LIndex: Integer;
  LURL: string;
  LResourceName: string;
  LPathName: string;
begin
  if not FDynamicScripts.Find(AScriptBaseName, LIndex) then
  begin
    LResourceName := IncludeTrailingPathDelimiter('js') + AScriptBaseName + '.js';
    LPathName := TKWebApplication.Current.Config.FindResourcePathName(LResourceName);
    if LPathName <> '' then
    begin
      LURL := TKWebApplication.Current.Config.FindResourceURL(LResourceName);
      TKWebResponse.Current.Items.ExecuteJSCode(Format('addScriptRef("%s");', [LURL]));
      FDynamicScripts.Add(AScriptBaseName);
    end;
  end;
end;

procedure TJSSession.EnsureDynamicStyle(const AStyleBaseName: string);
var
  LIndex: Integer;
  LURL: string;
  LResourceName: string;
  LPathName: string;
begin
  if not FDynamicStyles.Find(AStyleBaseName, LIndex) then
  begin
    LResourceName := IncludeTrailingPathDelimiter('js') + AStyleBaseName + '.css';
    LPathName := TKWebApplication.Current.Config.FindResourcePathName(LResourceName);
    if LPathName <> '' then
    begin
      LURL := TKWebApplication.Current.Config.FindResourceURL(LResourceName);
      TKWebResponse.Current.Items.ExecuteJSCode(Format('addLinkRef("%s");', [LURL]));
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

procedure TJSObject.AddItem(const AItems: TJSObjectArray; const AItem: TJSObject);
begin
  AItems.Add(AItem);
  if FJSConfig.IsReadOnly then
    TKWebResponse.Current.Items.CallMethod(Self, 'add').AddParam(AItem);
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
  if (TKWebResponse.Current <> nil) and TKWebResponse.Current.HasResponseItems then
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
  Assert(Session <> nil);
  Assert(Assigned(AOwner));
  inherited Create(AOwner);
  FJSConfig := TJSValues.Create(Self);
  JSName := Session.GetNextJSName(GetObjectNamePrefix);
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
    JSName := Session.GetNextJSName(GetObjectNamePrefix);
  InitDefaults;
end;

constructor TJSObject.CreateInline(const AOwner: TJSBase);
begin
  CreateInternal(AOwner, '');
  InitInlineDefaults;
end;

constructor TJSObject.CreateSingleton(const AOwner: TJSBase; const AAttributeName: string);
begin
  Assert(Assigned(AOwner));
  inherited Create(AOwner);
  if AAttributeName = '' then
    JSName := JSClassName
  else
    JSName := AAttributeName;
  InitDefaults;
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
  Result := TJSObject.CreateInternal(FJSConfig, AAttributeName);
  SetConfigItem(AAttributeName, Result);
end;

procedure TJSObject.InitDefaults;
begin
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
  Result := TJSObject(Session.FindChildByJSName(TKWebRequest.Current.GetQueryField(AParamName)));
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

{ TKExtSessionLocalizationTool }

procedure TJSLocalizationTool.AfterConstruction;
begin
  inherited;
  // Configure the global dxgettext instance.
  GetGnuGettextInstance.bindtextdomain(KITTO_TEXT_DOMAIN,
    TKConfig.SystemHomePath + 'locale');
end;

function TJSLocalizationTool.AsObject: TObject;
begin
  Result := Self;
end;

procedure TJSLocalizationTool.ForceLanguage(const ALanguageId: string);
var
  LInstance: TGnuGettextInstance;
begin
  LInstance := GetGnuGettextInstance;
  // Configure the per-session dxgettext instance.
  LInstance.bindtextdomain(KITTO_TEXT_DOMAIN,
    TKConfig.SystemHomePath + 'locale');
  LInstance.UseLanguage(ALanguageId);
end;

function TJSLocalizationTool.GetCurrentLanguageId: string;
begin
  Result := GetGnuGettextInstance.GetCurrentLanguage;
end;

function TJSLocalizationTool.GetGnuGettextInstance: TGnuGettextInstance;
begin
  if Session <> nil then
    Result := Session.FGettextInstance
  else
    Result := gnugettext.DefaultInstance;
end;

procedure TJSLocalizationTool.TranslateComponent(const AComponent: TComponent);
var
  LInstance: TGnuGettextInstance;
begin
  LInstance := GetGnuGettextInstance;
  LInstance.TranslateComponent(AComponent, KITTO_TEXT_DOMAIN);
  LInstance.TranslateComponent(AComponent, 'default');
end;

function TJSLocalizationTool.TranslateString(const AString,
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

//constructor TJSUploadedFile.Create(const AFileName, AFullFileName: string;
//  const AContext: TObject; const AOriginalFileName: string = '');
//begin
//  inherited Create;
//  FFileName := AFileName;
//  FFullFileName := AFullFileName;
//  FContext := AContext;
//  FOriginalFileName := AOriginalFileName;
//end;
//
//destructor TJSUploadedFile.Destroy;
//begin
//  FreeAndNil(FStream);
//  inherited;
//end;
//
//function TJSUploadedFile.GetBytes: TBytes;
//begin
//  if not Assigned(FStream) then
//  begin
//    FStream := TBytesStream.Create;
//    FStream.LoadFromFile(FFullFileName);
//  end;
//  Result := FStream.Bytes;
//  // Reset length, as FStream.Bytes for some reason is rounded up.
//  SetLength(Result, FStream.Size);
//  Assert(FStream.Size = Length(Result));
//end;

initialization
  _JSFormatSettings := TFormatSettings.Create;
  _JSFormatSettings.DecimalSeparator := '.';

  TEFLocalizationToolRegistry.RegisterTool(TJSLocalizationTool.Create);

finalization
  TEFLocalizationToolRegistry.UnregisterTool;

end.
