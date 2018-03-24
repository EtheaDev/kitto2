unit KIDE.IOTA.Register;

interface

procedure Register;

implementation

uses
  ToolsAPI
  , KIDE.IOTA.ProjectWizard
  , KIDE.YAMLHighlighter
  ;

procedure Register;
begin
  // YAML Syntax Highlighter
  RegisterYAMLHighlighter;

  // Kitto projects.
  RegisterPackageWizard(TVclIOTAProjectWizard.Create);
  RegisterPackageWizard(TWindowsServiceIOTAProjectWizard.Create);
  // Kitto files.
  { TODO : Implement if required. }
  //RegisterPackageWizard(TKModelIOTAProjectWizard.Create);
  //RegisterPackageWizard(TKViewIOTAProjectWizard.Create);
end;

end.

