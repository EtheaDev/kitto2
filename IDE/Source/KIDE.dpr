program KIDE;

uses
  Vcl.Forms,
  Vcl.Themes,
  Vcl.Styles,
  Kitto.Config,
  KIDE.MainFormUnit in 'KIDE.MainFormUnit.pas' {MainForm},
  KIDE.FileTree in 'KIDE.FileTree.pas',
  KIDE.MRUOptions in 'KIDE.MRUOptions.pas',
  KIDE.Project in 'KIDE.Project.pas',
  KIDE.Shell in 'KIDE.Shell.pas',
  DirWatcher in 'DirWatcher.pas',
  KIDE.ModelWizardFormUnit in 'KIDE.ModelWizardFormUnit.pas' {ModelWizardForm},
  KIDE.UpdateLocaleFormUnit in 'KIDE.UpdateLocaleFormUnit.pas' {UpdateLocaleForm},
  KIDE.Utils in 'KIDE.Utils.pas',
  KIDE.gettext in 'KIDE.gettext.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'KIDE';
  TStyleManager.TrySetStyle('Aqua Light Slate');
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
