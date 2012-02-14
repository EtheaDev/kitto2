unit KIDE.Project;

interface

uses
  Classes,
  EF.Tree,
  Kitto.Config;

type
  TProjectConfig = class(TKConfig)
  public

  end;

  TProject = class(TEFTree)
  private
    FFileName: string;
    FConfig: TProjectConfig;
    class var FCurrentProject: TProject;
    function GetDirectory: string;
    procedure RefreshConfig;
    class procedure SetCurrentProject(const AValue: TProject); static;
    function GetSourceDirectory: string;
  public
    class property CurrentProject: TProject read FCurrentProject write SetCurrentProject;
    class destructor Destroy;

    class procedure OpenProject(const AFileName: string);
    class procedure CloseProject;
    destructor Destroy; override;

    property FileName: string read FFileName;
    property Directory: string read GetDirectory;
    property SourceDirectory: string read GetSourceDirectory;

    property Config: TProjectConfig read FConfig;

    procedure GetConfigFileNames(const AFileNames: TStrings);
  end;

implementation

uses
  SysUtils,
  EF.SysUtils;

{ TProject }

class procedure TProject.CloseProject;
begin
  FreeAndNil(FCurrentProject);
end;

destructor TProject.Destroy;
begin
  FreeAndNil(FConfig);
  inherited;
end;

class destructor TProject.Destroy;
begin
  CloseProject;
end;

procedure TProject.GetConfigFileNames(const AFileNames: TStrings);
begin
  FindAllFiles('yaml', Config.GetMetadataPath, AFileNames, False, True);
end;

function TProject.GetDirectory: string;
begin
  Result := ExtractFilePath(FileName);
end;

function TProject.GetSourceDirectory: string;
begin
  Result := Directory + '..\Source';
end;

class procedure TProject.OpenProject(const AFileName: string);
begin
  CloseProject;
  FCurrentProject := TEFTreeFactory.LoadFromFile<TProject>(AFileName);
  FCurrentProject.FFileName := AFileName;
  FCurrentProject.RefreshConfig;
end;

procedure TProject.RefreshConfig;
begin
  FreeAndNil(FConfig);
  TProjectConfig.AppHomePath := Directory;
  FConfig := TProjectConfig.Create;
end;

class procedure TProject.SetCurrentProject(const AValue: TProject);
begin
  FreeAndNil(FCurrentProject);
  FCurrentProject := AValue;
end;

end.
