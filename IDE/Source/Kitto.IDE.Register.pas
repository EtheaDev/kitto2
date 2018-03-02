unit Kitto.IDE.Register;

interface

procedure Register;

implementation

uses
  ToolsAPI
  , Kitto.IDE.ProjectWizard
  ;

procedure Register;
begin
  RegisterPackageWizard(TKProjectWizard.Create);
end;

end.

