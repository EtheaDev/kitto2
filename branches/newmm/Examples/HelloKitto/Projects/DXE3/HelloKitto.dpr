program HelloKitto;

uses
  DBXMSSQL,
  DBXFirebird,
  Kitto.Ext.Start,
  UseKitto in '..\..\Source\UseKitto.pas',
  Rules in '..\..\Source\Rules.pas',
  Controllers in '..\..\Source\Controllers.pas';

{$R *.res}

begin
  TKExtStart.Start;
end.
