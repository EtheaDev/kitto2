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
    function GetDownloadJS(const AMethod: TJSProcedure): string;
  strict protected
    procedure CreateJSName;
    function GetObjectNamePrefix: string; virtual;
    procedure InitDefaults; virtual;
    function CreateConfigObject(const AAttributeName: string): TJSObject;
    function CreateConfigArray(const AAttributeName: string): TJSObjectArray;
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

    procedure Delete;

    function IsInternal: Boolean;
    function IsInline: Boolean;

    property JSConfig: TJSValues read FJSConfig;
    // Assigned if the object was created with CreateInternal.
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
  end;

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

  /// <summary>
  ///  Represents the server side of a user client session.
  ///  Holds all objects pertaining to the user session.
  /// </summary>
  TJSSession = class(TJSBase)
  private
    FSessionId: string;
    FObjectSequences: TDictionary<string, Cardinal>;
    FLibraries, FLanguage: string;
    FSingletons: TDictionary<string, TJSObject>;
    FMobileBrowserDetectionDone: Boolean;
    FIsMobileApple: Boolean;
    FGlobal: TJSObject;
    FIsDownload: Boolean;
    FIsUpload: Boolean;
    FUploadPath: string;
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
    FOpenControllers: TList<TObject>;
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
    function GetGlobal: TJSObject;
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
    procedure BeforeHandleRequest;

    property IsDownload: Boolean read FIsDownload write FIsDownload;
    property IsUpload: Boolean read FIsUpload write FIsUpload;

    property MaxUploadSize: Integer read FMaxUploadSize write FMaxUploadSize;
    property FileUploaded: string read FFileUploaded;
    property FileUploadedFullName: string read FFileUploadedFullName;
    function GetViewportWidthInInches: TJSExpression;
    function GetDefaultViewportWidth: Integer;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;

    property Language: string read FLanguage write SetLanguage;
    procedure Refresh;

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
    property OpenControllers: TList<TObject> read FOpenControllers;
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
  , Kitto.AccessControl
  , Kitto.Web.Application
  , Kitto.Web
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

procedure TJSSession.Refresh;
begin
  inherited;
  TKWebResponse.Current.Items.Clear;
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
  Result := 'o';
end;

procedure TJSObject.HandleEvent;
begin
  DoHandleEvent(ParamAsString('Event'));
end;

procedure TJSSession.BeforeHandleRequest;
var
  I: Integer;
begin
  FLastRequestDateTime := Now;
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
end;

function TJSSession.GetGlobal: TJSObject;
begin
  if not Assigned(FGlobal) then
    FGlobal := TJSObject.CreateInline(Self);
  Result := FGlobal;
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
  FreeAndNil(FOpenControllers);
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
  if Assigned(FOpenControllers) then
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

  FObjectSequences := TDictionary<string, Cardinal>.Create;
  FSingletons := TDictionary<string, TJSObject>.Create;

  FAuthData := TEFNode.Create;
  FAuthMacroExpander := TEFTreeMacroExpander.Create(FAuthData, 'Auth');

  FGettextInstance := TGnuGettextInstance.Create;

  FUploadedFiles := TObjectList<TJSUploadedFile>.Create;
  FOpenControllers := TList<TObject>.Create;

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
      TKWebResponse.Current.Items.ExecuteJSCode(Format('addScriptRef("%s");', [LURL]));
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
    JSName := TJSObject(Owner).JSName + '.' + FAttributeName
  else
    JSName := Session.GetNextJSName(GetObjectNamePrefix);
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
    TKWebResponse.Current.Items.ExecuteJSCode(JSName + '.destroy(); delete ' + JSName + ';');
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
  if Session.IsMobileApple then
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
  CreateJSName;
  TKWebResponse.Current.Items.CreateObject(Self);
  InitDefaults;
end;

constructor TJSObject.CreateInternal(const AOwner: TJSBase; const AAttributeName: string);
begin
  Assert(Assigned(AOwner));

  inherited Create(AOwner);
  FJSConfig := TJSValues.Create(Self);
  FAttributeName := AAttributeName;
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

function TJSObject.CreateConfigArray(const AAttributeName: string): TJSObjectArray;
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
    Result.JSName := '[' + AJSON + ']'
  else
    Result.JSName := AJSON;
end;

function TJSObject.JSObject(const AJSON: string; const AObjectConstructor: string; const ACurlyBrackets: Boolean): TJSObject;
begin
  Result := TJSObject.CreateInline(Self);
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
  Result := TKWebRequest.Current.QueryFields.Values[AParamName] = 'true';
end;

function TJSObject.ParamAsInteger(const AParamName: string): Integer;
begin
  Result := StrToIntDef(TKWebRequest.Current.QueryFields.Values[AParamName], 0);
end;

function TJSObject.ParamAsObject(const AParamName: string): TJSObject;
begin
  Result := TJSObject(Session.FindChildByJSName(
    TKWebRequest.Current.QueryFields.Values[AParamName]));
end;

function TJSObject.ParamAsString(const AParamName: string): string;
begin
  Result := TKWebRequest.Current.QueryFields.Values[AParamName];
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
    TKWebResponse.Current.Items.CallMethod(Self, AMethodName).AddParam(Avalue)
  else
    FJSConfig.Values.SetObject(AName, AValue);
  Result := AValue;
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

  TEFLocalizationToolRegistry.RegisterTool(TJSLocalizationTool.Create);

finalization
  TEFLocalizationToolRegistry.UnregisterTool;

end.
