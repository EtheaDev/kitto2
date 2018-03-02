unit Kitto.IDE.ProjectCreator;

interface

uses
  ToolsAPI
  ;

// Avoid warnings from obsolete but apparently still needed IOTAProjectCreator* interfaces.
{$WARN SYMBOL_DEPRECATED OFF}

type
  TKProjectCreator = class(
    TInterfacedObject
    , IOTACreator
    , IOTAProjectCreator50
    , IOTAProjectCreator80
    , IOTAProjectCreator160
    , IOTAProjectCreator
  )
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
  end;

implementation

uses
  PlatformAPI
  , SysUtils
  , Types
  , Classes
  , Kitto.IDE.MainFormCreator
  , Kitto.IDE.Utils
  ;

function TKProjectCreator.GetCreatorType: string;
begin
  Result := '';
end;

function TKProjectCreator.GetExisting: Boolean;
begin
  Result := False;
end;

function TKProjectCreator.GetFileSystem: string;
begin
  Result := '';
end;

function TKProjectCreator.GetOwner: IOTAModule;
begin
  Result := FindActiveProjectGroup;
end;

function TKProjectCreator.GetUnnamed: Boolean;
begin
  Result := True;
end;

function TKProjectCreator.GetFileName: string;
begin
{ TODO : Use different project file names for different project types }
  Result := GetCurrentDir + '\' + 'NewKittoProjectGUI.dpr';
end;

function TKProjectCreator.GetOptionFileName: string;
begin
  Result := '';
end;

function TKProjectCreator.GetShowSource: Boolean;
begin
  Result := True;
end;

function TKProjectCreator.NewProjectSource(const AProjectName: string): IOTAFile;
begin
{ TODO : Use different resource names for different project types }
  Result := TKResourceFile.Create('NewKittoProjectGUI_dproj');
end;

function TKProjectCreator.NewOptionSource(const AProjectName: string): IOTAFile; deprecated;
begin
  Result := nil;
end;

procedure TKProjectCreator.NewDefaultModule;
begin
end;

procedure TKProjectCreator.NewProjectResource(const AProject: IOTAProject);
begin
end;

procedure TKProjectCreator.NewDefaultProjectModule(const Project: IOTAProject);
var
  LModuleServices: IOTAModuleServices;
begin
  LModuleServices := BorlandIDEServices as IOTAModuleServices;
  LModuleServices.CreateModule(TKMainFormCreator.Create);
  { TODO : Create other modules such as Tools.pas, Controllers.pas, Config.yaml... }
end;

function TKProjectCreator.GetProjectPersonality: string;
begin
  Result := sDelphiPersonality;
end;

function TKProjectCreator.GetFrameworkType: string;
begin
  { TODO : Cross-platform support changes needed here }
  Result := sFrameworkTypeVCL;
end;

function TKProjectCreator.GetPlatforms: TArray<string>;
begin
  { TODO : Cross-platform support changes needed here }
  SetLength(Result, 2);
  Result[0] := cWin32Platform;
  Result[1] := cWin64Platform;
end;

function TKProjectCreator.GetPreferredPlatform: string;
begin
  Result := cWin32Platform;
end;

procedure TKProjectCreator.SetInitialOptions(const NewProject: IOTAProject);
begin
end;

end.

