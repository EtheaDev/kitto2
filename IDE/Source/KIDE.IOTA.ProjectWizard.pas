unit KIDE.IOTA.ProjectWizard;

interface

uses
  ToolsAPI
  , KIDE.IOTA.ProjectCreator
  ;

type
  TIOTAProjectWizard = class(
    TNotifierObject
    , IOTAWizard
    , IOTARepositoryWizard
    , IOTARepositoryWizard60
    , IOTARepositoryWizard80
    , IOTAProjectWizard
    , IOTAProjectWizard100
  )
  strict protected
    function GetProjectCreatorClass: TIOTAProjectCreatorClass; virtual; abstract;
  public
    const
      GALLERY_PAGE = 'Kitto Projects';
      ID_STRING = 'Kitto';
    constructor Create;

    // IOTAWizard
    procedure Execute;
    procedure AfterSave;
    procedure BeforeSave;
    procedure Destroyed;
    procedure Modified;
    function GetIDString: string; virtual; abstract;
    function GetName: string; virtual; abstract;
    function GetState: TWizardState;

    // IOTARepositoryWizard
    function GetAuthor: string;
    function GetComment: string; virtual; abstract;
    function GetGlyph: Cardinal;
    function GetPage: string;

    // IOTARepositoryWizard60
    function GetDesigner: string;

    // IOTARepositoryWizard80
    function GetGalleryCategory: IOTAGalleryCategory;
    function GetPersonality: string;

    // IOTAProjectWizard100
    function IsVisible(Project: IOTAProject): Boolean;
  end;

  TVclIOTAProjectWizard = class(TIOTAProjectWizard)
  strict protected
    function GetProjectCreatorClass: TIOTAProjectCreatorClass; override;
  public
    function GetName: string; override;
    function GetIDString: string; override;
    function GetComment: string; override;
  end;

  TWindowsServiceIOTAProjectWizard = class(TIOTAProjectWizard)
  strict protected
    function GetProjectCreatorClass: TIOTAProjectCreatorClass; override;
  public
    function GetName: string; override;
    function GetIDString: string; override;
    function GetComment: string; override;
  end;

implementation

uses
  Windows
  , SysUtils
  , Vcl.Dialogs
  , KIDE.NewProjectWizardForm
  , KIDE.ProjectTemplate
  ;

{ TKProjectWizard }

constructor TIOTAProjectWizard.Create;
var
  LCategoryServices: IOTAGalleryCategoryManager;
begin
  inherited Create;
  LCategoryServices := BorlandIDEServices as IOTAGalleryCategoryManager;
  LCategoryServices.AddCategory(LCategoryServices.FindCategory(sCategoryDelphiNew), ID_STRING, GALLERY_PAGE);
end;

procedure TIOTAProjectWizard.Execute;
var
  LProjectTemplate: TProjectTemplate;
begin
  if TNewProjectWizardForm.ShowDialog(LProjectTemplate) then
  begin
    try
      (BorlandIDEServices as IOTAModuleServices).CreateModule(GetProjectCreatorClass.Create(LProjectTemplate));
    finally
      FreeAndNIl(LProjectTemplate);
    end;
  end;
end;

function TIOTAProjectWizard.GetState: TWizardState;
begin
  Result := [wsEnabled];
end;

procedure TIOTAProjectWizard.AfterSave;
begin
end;

procedure TIOTAProjectWizard.BeforeSave;
begin
end;

procedure TIOTAProjectWizard.Destroyed;
begin
end;

procedure TIOTAProjectWizard.Modified;
begin
end;

function TIOTAProjectWizard.GetAuthor: string;
begin
  Result := 'Kitto Development Team';
end;

function TIOTAProjectWizard.GetGlyph: Cardinal;
begin
  Result := LoadIcon(HInstance, PChar(GetIDString.Replace('.', '')));
end;

function TIOTAProjectWizard.GetPage: string;
begin
  Result := GALLERY_PAGE;
end;

function TIOTAProjectWizard.GetDesigner: string;
begin
  Result := dAny;
end;

function TIOTAProjectWizard.GetGalleryCategory: IOTAGalleryCategory;
begin
  Result := (BorlandIDEServices as IOTAGalleryCategoryManager).FindCategory(ID_STRING);
end;

function TIOTAProjectWizard.GetPersonality: string;
begin
  Result := sDelphiPersonality;
end;

function TIOTAProjectWizard.IsVisible(Project: IOTAProject): Boolean;
begin
{ TODO : There may be cases in which the wizard shouldn't be available }
  Result := True;
end;

{ TWindowsServiceIOTAProjectWizard }

function TWindowsServiceIOTAProjectWizard.GetComment: string;
begin
  Result := 'Creates a new Windows Service Kitto application. This is the preferred option ' +
    'for deployment of a Kitto application in a production Windows-based environment.';
end;

function TWindowsServiceIOTAProjectWizard.GetIDString: string;
begin
  Result := ID_STRING + '.WindowsService.Application';
end;

function TWindowsServiceIOTAProjectWizard.GetName: string;
begin
  Result := 'Kitto Windows Service application';
end;

function TWindowsServiceIOTAProjectWizard.GetProjectCreatorClass: TIOTAProjectCreatorClass;
begin
  Result := TWindowsServiceIOTAProjectCreator;
end;

{ TWindowsGUIIOTAProjectWizard }

function TVclIOTAProjectWizard.GetComment: string;
begin
  Result := 'Creates a new VCL Kitto application. This type of project is mostly ' +
    'useful for debugging and internal use, as the application can be run inside ' +
    'the Delphi debugger and the visual GUI displays lots of useful information.';
end;

function TVclIOTAProjectWizard.GetIDString: string;
begin
  Result := ID_STRING + '.Vcl.Application';
end;

function TVclIOTAProjectWizard.GetName: string;
begin
  Result := 'Kitto VCL application';
end;

function TVclIOTAProjectWizard.GetProjectCreatorClass: TIOTAProjectCreatorClass;
begin
  Result := TVclIOTAProjectCreator;
end;

end.

