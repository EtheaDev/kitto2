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
  DBXDevartInterBase in 'DBXDevartInterBase.pas',
  DBXDevartSQLServer in 'DBXDevartSQLServer.pas',
  DirWatcher in 'DirWatcher.pas',
  KIDE.BaseFormUnit in 'KIDE.BaseFormUnit.pas' {BaseForm},
  KIDE.BaseWizardFormUnit in 'KIDE.BaseWizardFormUnit.pas' {BaseWizardForm},
  KIDE.DatabaseFrameUnit in 'KIDE.DatabaseFrameUnit.pas' {DatabaseFrame: TFrame},
  KIDE.DetailReferenceUpdateActionFrameUnit in 'KIDE.DetailReferenceUpdateActionFrameUnit.pas' {DetailReferenceUpdateActionFrame: TFrame},
  KIDE.FileTree in 'KIDE.FileTree.pas',
  KIDE.gettext in 'KIDE.gettext.pas',
  KIDE.MainFormUnit in 'KIDE.MainFormUnit.pas' {MainForm},
  KIDE.MetadataHelpers in 'KIDE.MetadataHelpers.pas',
  KIDE.ModelCreator in 'KIDE.ModelCreator.pas',
  KIDE.ModelUpdateActionFrameUnit in 'KIDE.ModelUpdateActionFrameUnit.pas' {ModelUpdateActionFrame: TFrame},
  KIDE.ModelWizardFormUnit in 'KIDE.ModelWizardFormUnit.pas' {ModelWizardForm},
  KIDE.MRUOptions in 'KIDE.MRUOptions.pas',
  KIDE.Project in 'KIDE.Project.pas',
  KIDE.Shell in 'KIDE.Shell.pas',
  KIDE.SplashFormUnit in 'KIDE.SplashFormUnit.pas' {SplashForm},
  KIDE.TableInfoModelUpdateActionFrameUnit in 'KIDE.TableInfoModelUpdateActionFrameUnit.pas' {TableInfoModelUpdateActionFrame: TFrame},
  KIDE.UpdateLocaleFormUnit in 'KIDE.UpdateLocaleFormUnit.pas' {UpdateLocaleForm},
  KIDE.Utils in 'KIDE.Utils.pas',
  KIDE.WaitFormUnit in 'KIDE.WaitFormUnit.pas' {WaitForm},
  KIDE.ModelFieldUpdateActionFrameUnit in 'KIDE.ModelFieldUpdateActionFrameUnit.pas' {ModelFieldUpdateActionFrame: TFrame},
  KIDE.ReferenceFieldUpdateActionFrameUnit in 'KIDE.ReferenceFieldUpdateActionFrameUnit.pas' {ReferenceFieldUpdateActionFrame: TFrame},
  KIDE.EFHelpers in 'KIDE.EFHelpers.pas',
  KIDE.ModelValidator in 'KIDE.ModelValidator.pas',
  KIDE.Process in 'KIDE.Process.pas',
  KIDE.ViewValidator in 'KIDE.ViewValidator.pas',
  KIDE.TreeValidator in 'KIDE.TreeValidator.pas',
  KIDE.DataViewWizardFormUnit in 'KIDE.DataViewWizardFormUnit.pas' {DataViewWizardForm},
  KIDE.Config in 'KIDE.Config.pas';

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
