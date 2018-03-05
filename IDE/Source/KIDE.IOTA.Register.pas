unit KIDE.IOTA.Register;

interface

procedure Register;

implementation

uses
  ToolsAPI
  , KIDE.IOTA.ProjectWizard
  ;

procedure Register;
begin
  // Kitto projects.
  RegisterPackageWizard(TVclIOTAProjectWizard.Create);
  RegisterPackageWizard(TWindowsServiceIOTAProjectWizard.Create);
  // Kitto files.
  { TODO : Implement if required. }
  //RegisterPackageWizard(TKModelIOTAProjectWizard.Create);
  //RegisterPackageWizard(TKViewIOTAProjectWizard.Create);
end;

end.

