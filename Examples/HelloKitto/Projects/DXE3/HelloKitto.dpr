program HelloKitto;

uses
  DBXMSSQL,
  DBXFirebird,
  Kitto.Ext.Start,
  UseKitto in '..\..\Source\UseKitto.pas',
  Rules in '..\..\Source\Rules.pas',
  Controllers in '..\..\Source\Controllers.pas',
  Kitto.Ext.TilePanel in '..\..\..\..\Source\Kitto.Ext.TilePanel.pas';

{$R *.res}

begin
  TKExtStart.Start;
end.
