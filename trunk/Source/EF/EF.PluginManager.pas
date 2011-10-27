{
  This unit defines a simple mechanism for run-time loading of packages
  called plugins. A package need not any special modification to become a
  plugint, but if it exports a couple of well-known entry points, the plugin
  manager will call them at predefined times.

  To speed up things considerably, the plugin manager disables Delphi's
  built-in package integrity check when a package is loaded. This means that
  the check for duplicate units will not be performed, and loading a package
  with units that duplicate already loaded units will result in strange and
  subtle errors.
  
  Define EFPluginManager_SlowLoad to disable this feature.
}
unit EF.PluginManager;

{$define EFPluginManager_SlowLoad}

interface

uses
  Contnrs, Windows, SysUtils;

type
  {
    Contains information about a loaded plugin. This information is needed to
    unload the plugin when the time comes.
  }
  TEFPluginInfo = class
  private
    FHandle: HMODULE;
    FFileName: string;
  public
    {
      Handle of the loaded module.
    }
    property Handle: HMODULE read FHandle;
    {
      Full package file name.
    }
    property FileName: string read FFileName;
  end;

  TEFPluginManager = class;

  {
    Prototype for all procedures that the plugin manager will call if a
    plugin exportes them.
  }
  TEFPluginProc = procedure(const APluginManager: TEFPluginManager);

  {
    Event type used by TEFPluginManager.
  }
  TEFPluginErrorEvent = procedure (const ASender: TEFPluginManager;
    const APluginFileName: string; const AError: Exception;
    var AContinue: Boolean) of object;

  {
    Event type used by TEFPluginManager.
  }
  TEFBeforeLoadPluginEvent = procedure (const ASender: TEFPluginManager;
    const APluginFileName: string; var AAllow: Boolean) of object;

  {
    Event type used by TEFPluginManager.
  }
  TEFAfterLoadPluginEvent = procedure (const ASender: TEFPluginManager;
    const APluginFileName: string; const ALibraryHandle: HMODULE;
    var AAllow: Boolean) of object;

  {
    Event type used by TEFPluginManager.
  }
  TEFBeforeUnloadPluginEvent = procedure (const ASender: TEFPluginManager;
    const APluginFileName: string; const ALibraryHandle: HMODULE;
    var AAllow: Boolean) of object;

  {
    Event type used by TEFPluginManager.
  }
  TEFAfterUnloadPluginEvent = procedure (const ASender: TEFPluginManager;
    const APluginFileName: string) of object;

  {
    Manages a list of loaded plugins, allows loading and unloading them, and
    provides events to hook into the loading/unloading process.
    It can load and unload a single plugin at a time or a set of them.
  }
  TEFPluginManager = class
  private
    FPluginInfos: TObjectList;
    FFileSpec: string;
    FOnLoadPluginError: TEFPluginErrorEvent;
    FBeforeLoadPlugin: TEFBeforeLoadPluginEvent;
    FAfterLoadPlugin: TEFAfterLoadPluginEvent;
    FAfterUnloadPlugin: TEFAfterUnloadPluginEvent;
    FBeforeUnloadPlugin: TEFBeforeUnloadPluginEvent;
    FOnUnloadPluginError: TEFPluginErrorEvent;
    function GetPluginInfo(const AIndex: Integer): TEFPluginInfo;
    function GetPluginInfoCount: Integer;
    procedure DoLoadPluginError(const APluginFileName: string;
      const AError: Exception; var AContinue: Boolean);
    procedure DoUnloadPluginError(const APluginFileName: string;
      const AError: Exception; var AContinue: Boolean);
  public
    constructor Create;
    destructor Destroy; override;
    {
      Returns an information object about a given loaded plugin, specified
      by index.
    }
    property PluginInfos[const AIndex: Integer]: TEFPluginInfo read GetPluginInfo;
    {
      Number of loaded plugins, each with its associated information object.
    }
    property PluginInfoCount: Integer read GetPluginInfoCount;
    {
      File mask for plugins to load. By default:@br
      <executable path>\EFPlugins\*.bpl
    }
    property FileSpec: string read FFileSpec write FFileSpec;
    {
      Loads and initializes all plugins that match FileSpec.
      Plugin initialization consists in executing any initialize section of all
      units included in the package, and then locating and calling a global
      parameterless procedure called EFInitializePlugin, which the plugin may
      export.
    }
    procedure LoadPlugins;
    {
      Finalizes and unloads all loaded plugins.
    }
    procedure UnloadPlugins;
    {
      Explicitly loads and initializes a given plugin. AFileName can point to
      anywhere (it's not required that the file name matches FileSpec).
      Plugin initialization consists in executing the initialization section
      of all units included in the package, and then locating and calling a
      global parameterless procedure called EFInitializePlugin, which the
      plugin may export.
      If the specified file doesn't exist, then an exception is raised,
      unless AStrict is set to False, in which case no exceptions are raised
      and the method returns False.
      After successfully loading a plugin, the method returns True.
    }
    function LoadPlugin(const AFileName: string; const AStrict: Boolean = True): Boolean;
    {
      Makes sure the plugin is loaded; if it was not loaded before, the plugin
      will be loaded and initialized (through LoadPlugin) now.
    }
    procedure EnsurePluginLoaded(const AFileName: string);
    {
      Finalizes and unloads the plugin loaded at the given index.
      Plugin finalization consists in executing the finalization section
      of all units included in the package, and then locating and calling a
      global parameterless procedure called EFFinalizePlugin, which the
      plugin may export.
    }
    procedure UnloadPlugin(const AIndex: Integer);
    {
      Fired when an error occurs while loading a plugin. Set AContinue to False
      to re-raise the exception, otherwise the plugin is simply skipped.
    }
    property OnLoadPluginError: TEFPluginErrorEvent
      read FOnLoadPluginError write FOnLoadPluginError;
    {
      Fired just before a plugin is loaded. Set AAllow to False to deny loading
      the plugin.
    }
    property BeforeLoadPlugin: TEFBeforeLoadPluginEvent
      read FBeforeLoadPlugin write FBeforeLoadPlugin;
    {
      Fired just after a plugin is loaded, and before it is initialized.
      Set AAllow to False to immediately unload the plgin without initializing
      it and without adding it to the list of loaded plugins.
    }
    property AfterLoadPlugin: TEFAfterLoadPluginEvent
      read FAfterLoadPlugin write FAfterLoadPlugin;
    {
      Fired when an error occurs while unloading a plugin. Set AContinue to False
      to re-raise the exception, otherwise the plugin is simply skipped.
    }
    property OnUnloadPluginError: TEFPluginErrorEvent
      read FOnUnloadPluginError write FOnUnloadPluginError;
    {
      Fired just before a plugin is finalized and unloaded. Set AAllow to False
      to deny finalization and unloading of the plugin.
    }
    property BeforeUnloadPlugin: TEFBeforeUnloadPluginEvent
      read FBeforeUnloadPlugin write FBeforeUnloadPlugin;
    {
      Fired just after a plugin is finalized and unloaded.
    }
    property AfterUnloadPlugin: TEFAfterUnloadPluginEvent
      read FAfterUnloadPlugin write FAfterUnloadPlugin;
  end;

{
  Singleton access to a global plugin manager, created on demand.

  Applications may create other instances (for example, if they need to handle
  any events). This singleton global instance may be used by other parts of
  the library that need to rely on a plugin manager.
}
function PluginManager: TEFPluginManager;

implementation

uses
  SysConst, Classes;

const
  EFPLUGIN_INITIALIZATION_PROC_NAME = 'EFInitializePlugin';
  EFPLUGIN_FINALIZATION_PROC_NAME = 'EFFinalizePlugin';

resourcestring
  msgCouldntLoadPackage = 'Error loading package "%s".';
  msgCouldntFindPackage = 'Couldn''t find package "%s".';

var
  _EFPluginManager: TEFPluginManager;

function PluginManager: TEFPluginManager;
begin
  if not Assigned(_EFPluginManager) then
    _EFPluginManager := TEFPluginManager.Create;
  Result := _EFPluginManager;
end;

{$IFNDEF EFPluginManager_SlowLoad}
procedure FastInitializePackage(const AModule: HMODULE);
type
  TPackageLoad = procedure;
var
  LPackageLoad: TPackageLoad;
begin
  @LPackageLoad := GetProcAddress(AModule, 'Initialize'); // Do not localize
  if Assigned(LPackageLoad) then
    LPackageLoad
  else
    raise EPackageError.CreateFmt(sInvalidPackageFile, [GetModuleName(AModule)]);
end;
{$ENDIF EFPluginManager_SlowLoad}

function EFLoadPackage(const AName: string): HMODULE;
begin
{$IFDEF EFPluginManager_SlowLoad}
  Result := LoadPackage(AName);
{$ELSE}
  Result := SafeLoadLibrary(AName);
  if Result = 0 then
    raise EPackageError.CreateResFmt(@sErrorLoadingPackage,
      [AName, SysErrorMessage(GetLastError)]);
  try
    FastInitializePackage(Result);
  except
    FreeLibrary(Result);
    raise;
  end;
{$ENDIF}
end;

{ TEFPluginManager }

constructor TEFPluginManager.Create;
begin
  inherited Create;
  FPluginInfos := TObjectList.Create(True);
  FFileSpec := ExtractFilePath(ParamStr(0)) + 'EFPlugins\*.bpl';
end;

destructor TEFPluginManager.Destroy;
begin
  UnloadPlugins;
  FPluginInfos.Free;
  inherited;
end;

procedure TEFPluginManager.DoLoadPluginError(const APluginFileName: string;
  const AError: Exception; var AContinue: Boolean);
begin
  if Assigned(FOnLoadPluginError) then
    FOnLoadPluginError(Self, APluginFileName, AError, AContinue);
end;

procedure TEFPluginManager.DoUnloadPluginError(const APluginFileName: string;
  const AError: Exception; var AContinue: Boolean);
begin
  if Assigned(FOnUnloadPluginError) then
    FOnUnloadPluginError(Self, APluginFileName, AError, AContinue);
end;

function TEFPluginManager.GetPluginInfo(const AIndex: Integer): TEFPluginInfo;
begin
  Result := FPluginInfos[AIndex] as TEFPluginInfo;
end;

function TEFPluginManager.GetPluginInfoCount: Integer;
begin
  Result := FPluginInfos.Count;
end;

function TEFPluginManager.LoadPlugin(const AFileName: string; const AStrict: Boolean = True): Boolean;
var
  LLibHandle: HMODULE;
  LPluginInfoIndex: Integer;
  LInitProc: TEFPluginProc;
  LPluginInfo: TEFPluginInfo;
  LAllow, LContinue: Boolean;
begin
  Result := False;
  LPluginInfo := nil;
  LLibHandle := 0;
  LAllow := True;
  if Assigned(FBeforeLoadPlugIn) then
    FBeforeLoadPlugin(Self, AFileName, LAllow);

  if LAllow then
  begin
    try
      if not FileExists(AFileName) then
      begin
        if AStrict then
          raise Exception.CreateFmt(msgCouldntFindPackage, [AFileName])
        else
          Exit;
      end;

      LLibHandle := EFLoadPackage(AFileName);
      if LLibHandle = 0 then
        raise Exception.CreateFmt(msgCouldntLoadPackage, [AFileName]);

      LAllow := True;
      if Assigned(FAfterLoadPlugin) then
        FAfterLoadPlugin(Self, AFileName, LLibHandle, LAllow);
      if not LAllow then
      begin
        UnloadPackage(LLibHandle);
        Exit;
      end;

      LInitProc := GetProcAddress(LLibHandle, EFPLUGIN_INITIALIZATION_PROC_NAME);
      if Assigned(LInitProc) then
        LInitProc(Self);

      LPluginInfo := TEFPluginInfo.Create;
      LPluginInfo.FHandle := LLibHandle;
      LPluginInfo.FFileName := AFileName;
      FPluginInfos.Add(LPluginInfo);
      Result := True;
    except
      on E: Exception do
      begin
        if Assigned(LPluginInfo) then
        begin
          LPluginInfoIndex := FPluginInfos.IndexOf(LPluginInfo);
          if LPluginInfoIndex >= 0 then
            FPluginInfos.Delete(LPluginInfoIndex);
        end;
        if LLibHandle <> 0 then
          UnloadPackage(LLibHandle);
        LContinue := True;
        DoLoadPluginError(AFileName, E, LContinue);
        if not LContinue then
          raise
        else
          Result := False;
      end;
    end;
  end;
end;

procedure TEFPluginManager.LoadPlugins;
var
  LFileName: string;
  LFound: Integer;
  LSearchRec: TSearchRec;
begin
  LFound := FindFirst(FFileSpec, 0, LSearchRec);
  try
    while LFound = 0 do begin
      LFileName := ExtractFilePath(FFileSpec) + LSearchRec.Name;
      LoadPlugin(LFileName);
      LFound := FindNext(LSearchRec);
    end;
  finally
    FindClose(LSearchRec);
  end;
end;

procedure TEFPluginManager.EnsurePluginLoaded(const AFileName: string);
var
  LPluginInfoIndex: Integer;
  LLoaded: Boolean;
begin
  LLoaded := False;
  for LPluginInfoIndex := 0 to PluginInfoCount - 1 do
  begin
    if SameText(PluginInfos[LPluginInfoIndex].FileName, AFileName) then
    begin
      LLoaded := True;
      Break;
    end;
  end;
  if not LLoaded then
    LoadPlugin(AFileName);
end;

procedure TEFPluginManager.UnloadPlugin(const AIndex: Integer);
var
  LPluginInfo: TEFPluginInfo;
  LibHandle: HMODULE;
  LFinalizationProc: TEFPluginProc;
  LAllow, LContinue: Boolean;
  LPluginFileName: string;
begin
  LPluginInfo := PluginInfos[AIndex];
  if Assigned(LPluginInfo) then
  begin
    LPluginFileName := LPluginInfo.FileName;
    LibHandle := LPluginInfo.Handle;
    LAllow := True;
    if Assigned(FBeforeUnloadPlugIn) then
      FBeforeUnloadPlugin(Self, LPluginFileName, LibHandle, LAllow);
    if LAllow then
    begin
      try
        LFinalizationProc := GetProcAddress(LibHandle, EFPLUGIN_FINALIZATION_PROC_NAME);
        if Assigned(LFinalizationProc) then
          LFinalizationProc(Self);
        FPluginInfos.Delete(AIndex);
        UnloadPackage(LibHandle);
        if Assigned(FAfterUnloadPlugin) then
          FAfterUnloadPlugin(Self, LPluginFileName);
      except
        on E: Exception do
        begin
          LContinue := True;
          DoUnloadPluginError(LPluginFileName, E, LContinue);
          if not LContinue then
            raise;
        end;
      end;
    end;
  end;
end;

procedure TEFPluginManager.UnloadPlugins;
var
  LPluginInfoIndex: Integer;
begin
  for LPluginInfoIndex := PluginInfoCount - 1 downto 0 do
    UnloadPlugin(LPluginInfoIndex);
end;

initialization

finalization
  FreeAndNil(_EFPluginManager);

end.

