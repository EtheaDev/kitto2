program KIDE;

uses
  Data.DBXFirebird,
  Data.DBXMSSQL,
  System.SysUtils,
  Vcl.Forms,
  Vcl.Themes,
  Vcl.Styles,
  EF.DB.ADO,
  EF.DB.DBX,
  Kitto.Config,
  KIDE.MainFormUnit in 'KIDE.MainFormUnit.pas' {MainForm},
  KIDE.FileTree in 'KIDE.FileTree.pas',
  KIDE.MRUOptions in 'KIDE.MRUOptions.pas',
  KIDE.Project in 'KIDE.Project.pas',
  KIDE.Shell in 'KIDE.Shell.pas',
  DirWatcher in 'DirWatcher.pas',
  KIDE.BaseWizardFormUnit in 'KIDE.BaseWizardFormUnit.pas' {BaseWizardForm},
  KIDE.UpdateLocaleFormUnit in 'KIDE.UpdateLocaleFormUnit.pas' {UpdateLocaleForm},
  KIDE.Utils in 'KIDE.Utils.pas',
  KIDE.gettext in 'KIDE.gettext.pas',
  KIDE.BaseFormUnit in 'KIDE.BaseFormUnit.pas' {BaseForm},
  KIDE.ModelWizardFormUnit in 'KIDE.ModelWizardFormUnit.pas' {ModelWizardForm},
  KIDE.DatabaseFrameUnit in 'KIDE.DatabaseFrameUnit.pas' {DatabaseFrame: TFrame},
  KIDE.ModelCreator in 'KIDE.ModelCreator.pas',
  KIDE.MetadataHelpers in 'KIDE.MetadataHelpers.pas',
  KIDE.SplashFormUnit in 'KIDE.SplashFormUnit.pas' {SplashForm},
  KIDE.ModelUpdateActionFrameUnit in 'KIDE.ModelUpdateActionFrameUnit.pas' {ModelUpdateActionFrame: TFrame},
  KIDE.WaitFormUnit in 'KIDE.WaitFormUnit.pas' {WaitForm},
  KIDE.TableInfoModelUpdateActionFrameUnit in 'KIDE.TableInfoModelUpdateActionFrameUnit.pas' {TableInfoModelUpdateActionFrame: TFrame},
  KIDE.DetailReferenceUpdateActionFrameUnit in 'KIDE.DetailReferenceUpdateActionFrameUnit.pas' {DetailReferenceUpdateActionFrame: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'KIDE';
  SplashForm := TSplashForm.Create(nil);
  try
    SplashForm.Show;
    SplashForm.Update;
    Sleep(1000); // Remove later.
    TStyleManager.TrySetStyle('Aqua Light Slate');
    Application.CreateForm(TMainForm, MainForm);
  finally
    FreeAndNil(SplashForm);
  end;
  Application.Run;
end.
