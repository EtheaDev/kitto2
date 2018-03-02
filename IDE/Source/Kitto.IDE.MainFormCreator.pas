unit Kitto.IDE.MainFormCreator;

interface

uses
  ToolsAPI
  , Kitto.IDE.Utils
  ;

type
  TKMainFormCreator = class(TInterfacedObject, IOTACreator, IOTAModuleCreator)
  public
    // IOTACreator
    function GetCreatorType: string;
    function GetExisting: Boolean;
    function GetFileSystem: string;
    function GetOwner: IOTAModule;
    function GetUnnamed: Boolean;

    // IOTAModuleCreator
    function GetAncestorName: string;
    function GetImplFileName: string;
    function GetIntfFileName: string;
    function GetFormName: string;
    function GetMainForm: Boolean;
    function GetShowForm: Boolean;
    function GetShowSource: Boolean;
    function NewFormFile(const AFormIdent, AAncestorIdent: string): IOTAFile;
    function NewImplSource(const AModuleIdent, AFormIdent, AAncestorIdent: string): IOTAFile;
    function NewIntfSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
    procedure FormCreated(const FormEditor: IOTAFormEditor);
  end;

implementation

uses
  SysUtils
  ;

function TKMainFormCreator.GetCreatorType: string;
begin
  Result := sForm;
end;

function TKMainFormCreator.GetExisting: Boolean;
begin
  Result := False;
end;

function TKMainFormCreator.GetFileSystem: string;
begin
  Result := '';
end;

function TKMainFormCreator.GetOwner: IOTAModule;
begin
  Result := FindActiveProject;
end;

function TKMainFormCreator.GetUnnamed: Boolean;
begin
  Result := True;
end;

function TKMainFormCreator.GetAncestorName: string;
begin
  Result := 'TForm';
end;

function TKMainFormCreator.GetImplFileName: string;
begin
  Result := GetCurrentDir + '\MainFormUnit.pas';
end;

function TKMainFormCreator.GetIntfFileName: string;
begin
  Result := '';
end;

function TKMainFormCreator.GetFormName: string;
begin
  Result := 'MainForm';
end;

function TKMainFormCreator.GetMainForm: Boolean;
begin
  Result := True;
end;

function TKMainFormCreator.GetShowForm: Boolean;
begin
  Result := True;
end;

function TKMainFormCreator.GetShowSource: Boolean;
begin
  Result := True;
end;

function TKMainFormCreator.NewFormFile(const AFormIdent, AAncestorIdent: string): IOTAFile;
begin
{ TODO : macros/text replace }
  Result := TKResourceFile.Create('MainForm_dfm');
end;

function TKMainFormCreator.NewImplSource(const AModuleIdent, AFormIdent, AAncestorIdent: string): IOTAFile;
begin
{ TODO : macros/text replace }
  Result := TKResourceFile.Create('MainForm_pas');
end;

function TKMainFormCreator.NewIntfSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
begin
  Result := nil;
end;

procedure TKMainFormCreator.FormCreated(const FormEditor: IOTAFormEditor);
begin
end;

end.

