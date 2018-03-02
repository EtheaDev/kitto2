unit Kitto.IDE.ProjectWizard;

interface

uses
  ToolsAPI
  ;

type
  TKProjectWizard = class(
    TNotifierObject
    , IOTAWizard
    , IOTARepositoryWizard
    , IOTARepositoryWizard60
    , IOTARepositoryWizard80
    , IOTAProjectWizard
    , IOTAProjectWizard100
  )
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
    function GetIDString: string;
    function GetName: string;
    function GetState: TWizardState;

    // IOTARepositoryWizard
    function GetAuthor: string;
    function GetComment: string;
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

procedure Register;

implementation

uses
  Kitto.IDE.ProjectCreator
  ;

{ TKProjectWizard }

constructor TKProjectWizard.Create;
var
  LCategoryServices: IOTAGalleryCategoryManager;
begin
  inherited Create;
  LCategoryServices := BorlandIDEServices as IOTAGalleryCategoryManager;
  LCategoryServices.AddCategory(LCategoryServices.FindCategory(sCategoryRoot), ID_STRING, GALLERY_PAGE);
end;

procedure TKProjectWizard.Execute;
begin
  (BorlandIDEServices as IOTAModuleServices).CreateModule(TKProjectCreator.Create);
end;

function TKProjectWizard.GetIDString: string;
begin
  Result := ID_STRING + '.NewProject';
end;

function TKProjectWizard.GetName: string;
begin
  Result := 'Kitto New Project Wizard';
end;

function TKProjectWizard.GetState: TWizardState;
begin
  Result := [wsEnabled];
end;

procedure TKProjectWizard.AfterSave;
begin
end;

procedure TKProjectWizard.BeforeSave;
begin
end;

procedure TKProjectWizard.Destroyed;
begin
end;

procedure TKProjectWizard.Modified;
begin
end;

function TKProjectWizard.GetAuthor: string;
begin
  Result := 'Kitto Development Team';
end;

function TKProjectWizard.GetComment: string;
begin
  Result := 'Creates a new Kitto Application';
end;

function TKProjectWizard.GetGlyph: Cardinal;
begin
  { TODO : Add glyph }
  Result := 0;
end;

function TKProjectWizard.GetPage: string;
begin
  Result := GALLERY_PAGE;
end;

function TKProjectWizard.GetDesigner: string;
begin
  Result := dAny;
end;

function TKProjectWizard.GetGalleryCategory: IOTAGalleryCategory;
begin
  Result := (BorlandIDEServices as IOTAGalleryCategoryManager).FindCategory(ID_STRING);
end;

function TKProjectWizard.GetPersonality: string;
begin
  Result := sDelphiPersonality;
end;

function TKProjectWizard.IsVisible(Project: IOTAProject): Boolean;
begin
  Result := True;
end;

procedure Register;
begin
  RegisterPackageWizard(TKProjectWizard.Create);
end;

end.

