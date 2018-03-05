unit KIDE.IOTA.ProjectCreator;

interface

uses
  ToolsAPI
  ;

// Avoid warnings from obsolete but apparently still needed IOTAProjectCreator* interfaces.
{$WARN SYMBOL_DEPRECATED OFF}

type
  TIOTAProjectCreator = class(
    TInterfacedObject
    , IOTACreator
    , IOTAProjectCreator50
    , IOTAProjectCreator80
    , IOTAProjectCreator160
    , IOTAProjectCreator
  )
  private
    FProjectFileName: string;
    function GetProjectDir: string;
    function GetProjectName(const AExtension: string): string;
  public
    // IOTACreator
    function GetCreatorType: string;
    function GetExisting: Boolean;
    function GetFileSystem: string;
    function GetOwner: IOTAModule;
    function GetUnnamed: Boolean;

    // IOTAProjectCreator
    function GetFileName: string;
    function GetOptionFileName: string; deprecated;
    function GetShowSource: Boolean;
    procedure NewDefaultModule; deprecated;
    function NewOptionSource(const AProjectName: string): IOTAFile; deprecated;
    procedure NewProjectResource(const AProject: IOTAProject);
    function NewProjectSource(const AProjectName: string): IOTAFile;

    // IOTAProjectCreator50
    procedure NewDefaultProjectModule(const Project: IOTAProject);

    // IOTAProjectCreator80
    function GetProjectPersonality: string;

    // IOTAProjectCreator160
    function GetFrameworkType: string;
    function GetPlatforms: TArray<string>;
    function GetPreferredPlatform: string;
    procedure SetInitialOptions(const NewProject: IOTAProject);
  public
    /// <summary>
    ///  Creates a kitto Delphi Project based on the specified kproj file name.
    /// </summary>
    /// <param name="AProjectFileName">
    ///  Full path and name of the kproj project file. The path is used as a
    ///  reference to create the template-based source files. The contents of
    ///  the kproj might be used as well at some point.
    /// </param>
    constructor Create(const AProjectFileName: string);
  end;

  TIOTAProjectCreatorClass = class of TIOTAProjectCreator;

  TVclIOTAProjectCreator = class(TIOTAProjectCreator)
  end;

  TWindowsServiceIOTAProjectCreator = class(TIOTAProjectCreator)
  end;

implementation

uses
  PlatformAPI
  , SysUtils
  , Types
  , Classes
  , IOUtils
  , KIDE.IOTA.MainFormCreator
  , KIDE.IOTA.Utils
  ;

constructor TIOTAProjectCreator.Create(const AProjectFileName: string);
begin
  inherited Create;
  FProjectFileName := AProjectFileName;
end;

function TIOTAProjectCreator.GetCreatorType: string;
begin
  Result := '';
end;

function TIOTAProjectCreator.GetExisting: Boolean;
begin
  Result := False;
end;

function TIOTAProjectCreator.GetFileSystem: string;
begin
  Result := '';
end;

function TIOTAProjectCreator.GetOwner: IOTAModule;
begin
  Result := FindActiveProjectGroup;
end;

function TIOTAProjectCreator.GetUnnamed: Boolean;
begin
  Result := True;
end;

function TIOTAProjectCreator.GetFileName: string;
begin
  Result := TPath.Combine(GetProjectDir, GetProjectName('dpr'));
end;

function TIOTAProjectCreator.GetOptionFileName: string;
begin
  Result := '';
end;

function TIOTAProjectCreator.GetShowSource: Boolean;
begin
  Result := True;
end;

function TIOTAProjectCreator.NewProjectSource(const AProjectName: string): IOTAFile;
begin
{ TODO : Use different resource names for different project types }
  Result := TKResourceFile.Create('NewKittoProjectGUI_dproj');
end;

function TIOTAProjectCreator.NewOptionSource(const AProjectName: string): IOTAFile; deprecated;
begin
  Result := nil;
end;

procedure TIOTAProjectCreator.NewDefaultModule;
begin
end;

procedure TIOTAProjectCreator.NewProjectResource(const AProject: IOTAProject);
begin
end;

procedure TIOTAProjectCreator.NewDefaultProjectModule(const Project: IOTAProject);
var
  LModuleServices: IOTAModuleServices;
begin
  LModuleServices := BorlandIDEServices as IOTAModuleServices;
  LModuleServices.CreateModule(TIOTAMainFormCreator.Create);
  { TODO : Create other modules such as Tools.pas, Controllers.pas, Config.yaml... }
end;

function TIOTAProjectCreator.GetProjectDir: string;
begin
  Result := ExtractFilePath(FProjectFileName);
end;

function TIOTAProjectCreator.GetProjectName(const AExtension: string): string;
begin
  Result := ChangeFileExt(ExtractFileName(FProjectFileName), AExtension);
end;

function TIOTAProjectCreator.GetProjectPersonality: string;
begin
  Result := sDelphiPersonality;
end;

function TIOTAProjectCreator.GetFrameworkType: string;
begin
  { TODO : Cross-platform support changes needed here }
  Result := sFrameworkTypeVCL;
end;

function TIOTAProjectCreator.GetPlatforms: TArray<string>;
begin
  { TODO : Cross-platform support changes needed here }
  SetLength(Result, 2);
  Result[0] := cWin32Platform;
  Result[1] := cWin64Platform;
end;

function TIOTAProjectCreator.GetPreferredPlatform: string;
begin
  Result := cWin32Platform;
end;

procedure TIOTAProjectCreator.SetInitialOptions(const NewProject: IOTAProject);
begin
end;

end.

