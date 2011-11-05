program HelloKitto;

uses
  Forms,
  Kitto.Ext.MainFormUnit,
  DBXFirebird,
  UseKitto in '..\..\Source\UseKitto.pas',
  Rules in '..\..\Source\Rules.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'HelloKitto';
  Application.CreateForm(TKExtMainForm, KExtMainForm);
  Application.Run;
end.
