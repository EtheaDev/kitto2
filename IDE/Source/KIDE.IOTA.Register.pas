unit KIDE.IOTA.Register;

interface

procedure Register;

implementation

uses
  SysUtils
  , ToolsAPI
  , DesignIntf
  , KIDE.IOTA.ProjectWizard
  , KIDE.YAMLHighlighter
  ;

procedure Register;
begin
//  ForceDemandLoadState(dlDisable);

  // YAML Syntax Highlighter
  RegisterYAMLHighlighter;

  // Kitto projects.
//  RegisterPackageWizard(TVclIOTAProjectWizard.Create);
//  RegisterPackageWizard(TWindowsServiceIOTAProjectWizard.Create);
  // Kitto files.
  { TODO : Implement if required. }
  //RegisterPackageWizard(TKModelIOTAProjectWizard.Create);
  //RegisterPackageWizard(TKViewIOTAProjectWizard.Create);

end;

end.

