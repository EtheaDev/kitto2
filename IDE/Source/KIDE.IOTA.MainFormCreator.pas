unit KIDE.IOTA.MainFormCreator;

interface

uses
  ToolsAPI
  , KIDE.IOTA.Utils
  ;

type
  TIOTAMainFormCreator = class(TInterfacedObject, IOTACreator, IOTAModuleCreator)
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

function TIOTAMainFormCreator.GetCreatorType: string;
begin
  Result := sForm;
end;

function TIOTAMainFormCreator.GetExisting: Boolean;
begin
  Result := False;
end;

function TIOTAMainFormCreator.GetFileSystem: string;
begin
  Result := '';
end;

function TIOTAMainFormCreator.GetOwner: IOTAModule;
begin
  Result := FindActiveProject;
end;

function TIOTAMainFormCreator.GetUnnamed: Boolean;
begin
  Result := True;
end;

function TIOTAMainFormCreator.GetAncestorName: string;
begin
  Result := 'TForm';
end;

function TIOTAMainFormCreator.GetImplFileName: string;
begin
  Result := GetCurrentDir + '\MainFormUnit.pas';
end;

function TIOTAMainFormCreator.GetIntfFileName: string;
begin
  Result := '';
end;

function TIOTAMainFormCreator.GetFormName: string;
begin
  Result := 'MainForm';
end;

function TIOTAMainFormCreator.GetMainForm: Boolean;
begin
  Result := True;
end;

function TIOTAMainFormCreator.GetShowForm: Boolean;
begin
  Result := True;
end;

function TIOTAMainFormCreator.GetShowSource: Boolean;
begin
  Result := True;
end;

function TIOTAMainFormCreator.NewFormFile(const AFormIdent, AAncestorIdent: string): IOTAFile;
begin
{ TODO : macros/text replace }
  Result := TKResourceFile.Create('MainForm_dfm');
end;

function TIOTAMainFormCreator.NewImplSource(const AModuleIdent, AFormIdent, AAncestorIdent: string): IOTAFile;
begin
{ TODO : macros/text replace }
  Result := TKResourceFile.Create('MainForm_pas');
end;

function TIOTAMainFormCreator.NewIntfSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
begin
  Result := nil;
end;

procedure TIOTAMainFormCreator.FormCreated(const FormEditor: IOTAFormEditor);
begin
end;

end.

