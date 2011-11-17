program HelloKitto;

uses
  Kitto.Ext.Start,
  DBXFirebird,
  DBXMSSQL,
  UseKitto in '..\..\Source\UseKitto.pas',
  Rules in '..\..\Source\Rules.pas';

{$R *.res}

begin
  TKExtStart.Start;
end.
