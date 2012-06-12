{-------------------------------------------------------------------------------
   Copyright 2012 Ethea S.r.l.

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

unit Kitto.Ext.Session;

{$I Kitto.Defines.inc}

interface

uses
  SysUtils, Classes, Generics.Collections,
  ExtPascal, Ext,
  EF.Tree,
  Kitto.Ext.Base, Kitto.Ext.Controller, Kitto.Config, Kitto.Metadata.Views,
  Kitto.Metadata.DataView, Kitto.Ext.Login;

type
  TKExtUploadedFile = class
  private
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

  TKExtSession = class(TExtThread)
  private
    FHomeController: TObject;
    FConfig: TKConfig;
    FLoginWindow: TKExtLoginWindow;
    FViewHost: TExtTabPanel;
    FStatusHost: TKExtStatusBar;
    FSessionId: string;
    FUploadedFiles: TObjectList<TKExtUploadedFile>;
    FOpenControllers: TObjectList<TObject>;
    procedure LoadLibraries;
    procedure DisplayHomeView;
    procedure DisplayLoginWindow;
    function GetConfig: TKConfig;
    procedure ClearStatus;
    function DisplayNewController(const AView: TKView): IKExtController;
    function FindOpenController(const AView: TKView): IKExtController;
    procedure SetViewHostActiveTab(const AObject: TObject);
  protected
    function BeforeHandleRequest: Boolean; override;
    procedure AfterHandleRequest; override;
    procedure AfterNewSession; override;
  public
    constructor Create(AOwner: TObject); override;
    destructor Destroy; override;
    class constructor Create;
    class destructor Destroy;
  public
    ///	<summary>
    ///	  A reference to the panel to be used as the main view container.
    ///	</summary>
    property ViewHost: TExtTabPanel read FViewHost write FViewHost;

    ///	<summary>
    ///	  A reference to the status bar to be used for wait messages.
    ///	</summary>
    property StatusHost: TKExtStatusBar read FStatusHost write FStatusHost;

    procedure DisplayView(const AName: string); overload;
    procedure DisplayView(const AView: TKView); overload;

    procedure InitDefaultValues; override;
    procedure Home; override;

    ///	<summary>Opens the specified URL in a new browser window/tab.</summary>
    procedure Navigate(const AURL:string);
    ///	<summary>
    ///	  <para>
    ///	    Adds to the current session a style class named after AView's
    ///	    ImageName (or the specified custom AImageName) plus a '_img'
    ///	    suffix, that sets background:url to the URL of the view's image.
    ///	  </para>
    ///	  <para>
    ///	    The style class can have an optional custom prefix before the name
    ///	    and custom rules attached to it.
    ///	  </para>
    ///	</summary>
    ///	<returns>
    ///	  Returns the class name so that it can be assigned to a component's
    ///	  IconCls property.
    ///	</returns>
    function SetViewIconStyle(const AView: TKView; const AImageName: string = '';
      const ACustomPrefix: string = ''; const ACustomRules: string = ''): string;

    // Test
    function GetGCObjectCount: Integer;

    procedure Flash(const AMessage: string);

    property Config: TKConfig read GetConfig;

    ///	<summary>Called to signal that a new file has been uploaded. The
    ///	descriptor holds information about the file and its context (for
    ///	example which view is going to use it).</summary>
    ///	<remarks>The session acquires ownership of the descriptor
    ///	object.</remarks>
    procedure AddUploadedFile(const AFileDescriptor: TKExtUploadedFile);

    ///	<summary>Removes a previously added file descriptor. To be called once
    ///	the uploaded file has been processed.</summary>
    procedure RemoveUploadedFile(const AFileDescriptor: TKExtUploadedFile);

    ///	<summary>Returns the first uploaded file descriptor matching the
    ///	specified context, or nil if no descriptor is found.</summary>
    function FindUploadedFile(const AContext: TObject): TKExtUploadedFile;

    ///	<summary>Calls AProc for each uploaded file in list.</summary>
    procedure EnumUploadedFiles(const AProc: TProc<TKExtUploadedFile>);

    ///	<summary>If the specified object is a controller and is found in the
    ///	list of open controllers, it is removed from the list. Otherwise
    ///	nothing happens. Used by view hosts to notify the session that a
    ///	controller was closed.</summary>
    procedure RemoveController(const AObject: TObject);

    ///	<summary>Finds and returns a record from the specified store using the
    ///	key values currently stored in the session query strings.</summary>
    function LocateRecordFromQueries(const AViewTable: TKViewTable;
      const AServerStore: TKViewTableStore): TKViewTableRecord;
  published
    procedure Logout;
  end;

function Session: TKExtSession;

implementation

uses
  StrUtils, ActiveX, ComObj, Types, FmtBcd,
  ExtPascalUtils, ExtForm, FCGIApp,
  EF.Intf, EF.SysUtils, EF.StrUtils, EF.Localization, EF.Macros, EF.Logger,
  Kitto.Auth, Kitto.Types, Kitto.AccessControl,
  Kitto.Ext.Utils;

function Session: TKExtSession;
begin
  Result := TKExtSession(CurrentWebSession);
end;

{ TKExtSession }

function TKExtSession.LocateRecordFromQueries(const AViewTable: TKViewTable;
  const AServerStore: TKViewTableStore): TKViewTableRecord;
var
  LKey: TEFNode;
begin
  Assert(Assigned(AViewTable));
  Assert(Assigned(AServerStore));

  LKey := TEFNode.Create;
  try
    LKey.Assign(AServerStore.Key);
    LKey.SetChildValuesfromStrings(Queries, True, Config.JSFormatSettings,
      function(const AName: string): string
      begin
        Result := AViewTable.FieldByName(AName).AliasedName;
      end);
    Result := AServerStore.Records.GetRecord(LKey);
  finally
    FreeAndNil(LKey);
  end;
end;

function TKExtSession.GetConfig: TKConfig;
begin
  if not Assigned(FConfig) then
    FConfig := TKConfig.Create;
  Result := FConfig;
end;

type
  PGarbage = ^TGarbage;
  TGarbage = record
    Garbage    : TObject;
    Persistent : Boolean;
  end;

function TKExtSession.GetGCObjectCount: Integer;
var
  I: Integer;
  LObject: TObject;
begin
  Result := 0;
  for I := 0 to FGarbageCollector.Count - 1 do
  begin
    LObject := FGarbageCollector.Objects[I];
    if (LObject <> nil) and (PGarbage(LObject)^.Garbage <> nil) then
      Inc(Result);
  end;
end;

destructor TKExtSession.Destroy;
var
  LUploadDirectory: string;
begin
  // Make sure objects find the session threadvar assigned when they are
  // being garbage collected in case the session is being freed by a
  // different thread. Otherwise objects don't mark themselves off the
  // GC upon destruction and risk to be destroyed multiple times.
  CurrentWebSession := Self;
  // Delete upload folder only for valid sessions.
  if FSessionId <> '' then
  begin
    LUploadDirectory := ReplaceStr(DocumentRoot + UploadPath, '/', '\');
    if DirectoryExists(LUploadDirectory) then
      DeleteTree(LUploadDirectory);
  end;
  FreeAndNil(FConfig);
  FreeAndNil(FUploadedFiles);
  FreeAndNil(FHomeController);
  inherited;
  // Keep it alive as the inherited call might trigger calls to
  // RemoveController from objects being destroyed.
  FreeAndNil(FOpenControllers);
end;

class destructor TKExtSession.Destroy;
begin
  TEFMacroExpansionEngine.OnGetInstance := nil;
  TKConfig.OnGetInstance := nil;
end;

procedure TKExtSession.DisplayHomeView;
var
  LHomeView: TKView;
  LIntf: IKExtController;
begin
  FreeAndNil(FHomeController);

  LHomeView := Config.Views.FindViewByNode(Config.Config.FindNode('HomeView'));
  if not Assigned(LHomeView) then
    LHomeView := Config.Views.ViewByName('Home');
  FHomeController := TKExtControllerFactory.Instance.CreateController(LHomeView, nil).AsObject;
  if Supports(FHomeController, IKExtController, LIntf) then
    LIntf.Display;
end;

procedure TKExtSession.Home;
begin
  ExtQuickTips.Init(True);

  if not NewThread then
  begin
    Config.Authenticator.Logout;
    Refresh;
    FHomeController := nil;
    FLoginWindow := nil;
    FOpenControllers.Clear;
    FViewHost := nil;
    FStatusHost := nil;
  end
  else
  begin
    if not IsAjax then
    begin
      LoadLibraries;
      JSCode('kittoInit();');
    end;
  end;

  // Try authentication with default credentials, if any, and skip login
  // window if it succeeds.
  if TKExtLoginWindow.Authenticate(Query['UserName'], Query['Password']) then
    DisplayHomeView
  else
    DisplayLoginWindow;
end;

procedure TKExtSession.DisplayLoginWindow;
begin
  FreeAndNil(FLoginWindow);
  FLoginWindow := TKExtLoginWindow.Create;
  FLoginWindow.OnLogin := DisplayHomeView;
  FLoginWindow.Show;
end;

function TKExtSession.FindUploadedFile(
  const AContext: TObject): TKExtUploadedFile;
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

procedure TKExtSession.Flash(const AMessage: string);
begin
  JSCode('Ext.example.msg("' + _(Config.AppTitle) + '", "' + AMessage + '");');
end;

procedure TKExtSession.LoadLibraries;

  procedure SetRequiredLibrary(const ALibName: string; const AIncludeCSS: Boolean = False);
  var
    LLibURL: string;
  begin
    LLibURL := Config.GetResourceURL(IncludeTrailingPathDelimiter('js') + ALibName + '.js');
    SetLibrary(StripSuffix(LLibURL, '.js'), AIncludeCSS, False, True);
  end;

  procedure SetOptionalLibrary(const ALibName: string; const AIncludeCSS: Boolean = False);
  var
    LLibURL: string;
  begin
    LLibURL := Config.FindResourceURL(IncludeTrailingPathDelimiter('js') + ALibName + '.js');
    if LLibURL <> '' then
      SetLibrary(StripSuffix(LLibURL, '.js'), False, False, True);
    if AIncludeCSS then
    begin
      LLibURL := Config.FindResourceURL(IncludeTrailingPathDelimiter('js') + ALibName + '.css');
      if LLibURL <> '' then
        SetCSS(StripSuffix(LLibURL, '.css'), False);
    end;
  end;

var
  LLibraries: TStringDynArray;
  LLibName: string;
begin
  SetLibrary(ExtPath + '/examples/ux/statusbar/StatusBar');
  SetCSS(ExtPath + '/examples/ux/statusbar/css/statusbar');

  SetLibrary(ExtPath + '/examples/ux/fileuploadfield/FileUploadField');
  SetCSS(ExtPath + '/examples/ux/fileuploadfield/css/fileuploadfield');

  SetLibrary(ExtPath + '/examples/shared/examples'); // For Ext.msg.
  SetLibrary(ExtPath + '/src/locale/ext-lang-' + Language);
  SetRequiredLibrary('DateTimeField');
  SetRequiredLibrary('DefaultButton');
  SetRequiredLibrary('kitto-core', True);
  SetRequiredLibrary('kitto-init');
  SetOptionalLibrary('application', True);

  LLibraries := Config.Config.GetStringArray('JavaScriptLibraries');
  for LLibName in LLibraries do
    SetRequiredLibrary(LLibName);
end;

procedure TKExtSession.Logout;
begin
  Config.Authenticator.Logout;
  Session.JSCode('window.location.reload();');
end;

procedure TKExtSession.Navigate(const AURL: string);
begin
  Response := Format('window.open("%s", "_blank");', [AURL]);
end;

procedure TKExtSession.RemoveController(const AObject: TObject);
begin
  FOpenControllers.Remove(AObject);
end;

procedure TKExtSession.RemoveUploadedFile(
  const AFileDescriptor: TKExtUploadedFile);
begin
  FUploadedFiles.Remove(AFileDescriptor);
end;

procedure TKExtSession.DisplayView(const AName: string);
begin
  Assert(AName <> '');

  DisplayView(Config.Views.ViewByName(AName));
end;

function TKExtSession.DisplayNewController(const AView: TKView): IKExtController;
var
  LIsSynchronous: Boolean;
begin
  Assert(Assigned(AView));

  Result := TKExtControllerFactory.Instance.CreateController(AView, FViewHost);
  LIsSynchronous := Result.IsSynchronous;
  if not LIsSynchronous then
    FOpenControllers.Add(Result.AsObject);
  Result.Display;
  // Synchronous controllers end their life inside Display.
  if LIsSynchronous then
    NilEFIntf(Result);
end;

function TKExtSession.FindOpenController(const AView: TKView): IKExtController;
var
  I: Integer;
begin
  Assert(Assigned(AView));

  Result := nil;
  for I := 0 to FOpenControllers.Count - 1 do
  begin
    if Supports(FOpenControllers[I], IKExtController, Result) then
    begin
      if Result.View = AView then
        Break
      else
        Result := nil;
    end;
  end;
end;

procedure TKExtSession.DisplayView(const AView: TKView);
var
  LController: IKExtController;
begin
  Assert(Assigned(AView));
  Assert(Assigned(FViewHost));

  if AView.IsAccessGranted(ACM_VIEW) then
  begin
    if AView.GetBoolean('Controller/AllowMultipleInstances') then
      LController := DisplayNewController(AView)
    else
    begin
      LController := FindOpenController(AView);
      if not Assigned(LController) then
        LController := DisplayNewController(AView);
    end;
    if Assigned(LController) and LController.SupportsContainer and Assigned(FViewHost) then
      SetViewHostActiveTab(LController.AsObject);
    ClearStatus;
  end;
end;

procedure TKExtSession.SetViewHostActiveTab(const AObject: TObject);
var
  I: Integer;
begin
  Assert(Assigned(FViewHost));
  Assert(Assigned(AObject));

  for I := 0 to FViewHost.Items.Count - 1 do
  begin
    if FViewHost.Items[I] = AObject then
    begin
      FViewHost.SetActiveTab(I);
      Break;
    end;
  end;
end;

procedure TKExtSession.ClearStatus;
begin
  if Assigned(FStatusHost) then
    FStatusHost.ClearStatus;
end;

procedure TKExtSession.EnumUploadedFiles(
  const AProc: TProc<TKExtUploadedFile>);
var
  I: Integer;
begin
  if Assigned(AProc) then
  begin
    for I := 0 to FUploadedFiles.Count - 1 do
      AProc(FUploadedFiles[I]);
  end;
end;

procedure TKExtSession.InitDefaultValues;
var
  LLanguageId: string;
begin
  inherited;
  ExtPath := Config.Config.GetString('Ext/URL', '/ext');
  Charset := Config.Config.GetString('Charset', 'utf-8');
  LLanguageId := Config.Config.GetString('LanguageId');
  if LLanguageId <> '' then
    Language := LLanguageId;
  Theme := Config.Config.GetString('Ext/Theme');
end;

procedure TKExtSession.AddUploadedFile(
  const AFileDescriptor: TKExtUploadedFile);
begin
  FUploadedFiles.Add(AFileDescriptor);
end;

procedure TKExtSession.AfterHandleRequest;
begin
  inherited;
  { TODO : only do this when ADO is used }
  CoUninitialize;
end;

procedure TKExtSession.AfterNewSession;
begin
  inherited;
  FSessionId := Cookie['FCGIThread'];
  TEFLogger.Instance.LogFmt('New session %s.', [FSessionId], TEFLogger.LOG_MEDIUM);
  UpLoadPath := '/uploads/' + Config.AppName + '/' + FSessionId;
end;

function TKExtSession.BeforeHandleRequest: Boolean;
begin
  TEFLogger.Instance.LogStrings('BeforeHandleRequest', Queries, TEFLogger.LOG_DETAILED);
  { TODO : only do this when ADO is used }
  OleCheck(CoInitialize(nil));
  Result := inherited BeforeHandleRequest;
end;

constructor TKExtSession.Create(AOwner: TObject);
begin
  inherited;
  FUploadedFiles := TObjectList<TKExtUploadedFile>.Create;
  FOpenControllers := TObjectList<TObject>.Create(False);
end;

class constructor TKExtSession.Create;
begin
  TEFMacroExpansionEngine.OnGetInstance :=
    function: TEFMacroExpansionEngine
    begin
      if Session <> nil then
        Result := Session.Config.MacroExpansionEngine
      else
        Result := nil;
    end;
  TKConfig.OnGetInstance :=
    function: TKConfig
    begin
      if Session <> nil then
        Result := Session.Config
      else
        Result := nil;
    end;
end;

function TKExtSession.SetViewIconStyle(const AView: TKView; const AImageName: string;
  const ACustomPrefix: string; const ACustomRules: string): string;
var
  LIconURL: string;
  LRule: string;
begin
  Assert(Assigned(AView));

  Result := IfThen(AImageName <> '', AImageName, AView.ImageName);
  LIconURL := Config.GetImageURL(Result);
  Result := ACustomPrefix + Result + '_img';
  // The !important rule allows to use a non-specific selector, so that the icon
  // can be shared by different components.
  // no-repeat is added because some components (such as buttons) repeat by default
  // (others, such as menu items and tree nodes, don't).
  LRule := '.' + Result + ' {background: url(' + LIconURL + ') no-repeat left !important;' + ACustomRules + '}';
  if IsAjax then
    JSCode('addStyleRule("' + LRule + '");')
  else
    SetStyle(LRule);
end;

{ TKExtUploadedFile }

constructor TKExtUploadedFile.Create(const AFileName, AFullFileName: string;
  const AContext: TObject; const AOriginalFileName: string = '');
begin
  inherited Create;
  FFileName := AFileName;
  FFullFileName := AFullFileName;
  FContext := AContext;
  FOriginalFileName := AOriginalFileName;
end;

destructor TKExtUploadedFile.Destroy;
begin
  FreeAndNil(FStream);
  inherited;
end;

function TKExtUploadedFile.GetBytes: TBytes;
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

end.

