program TasKittoConsole;

{$APPTYPE CONSOLE}

{$WARN DUPLICATE_CTOR_DTOR OFF}

{$R *.res}

uses
  System.SysUtils,
  EF.Logger.TextFile,
  Kitto.Console.Start,
  UseKitto in '..\..\Source\UseKitto.pas',
  Rules in '..\..\Source\Rules.pas',
  Auth in '..\..\Source\Auth.pas';

begin
  TKStart.Start;
end.
