program TasKitto;

uses
  Forms,
  DBXFirebird,
  Kitto.Ext.MainFormUnit,
  UseKitto in '..\..\Source\UseKitto.pas',
  Rules in '..\..\Source\Rules.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := '';
  Application.CreateForm(TKExtMainForm, KExtMainForm);
  Application.Run;
end.
