program TasKittoConsole;

{$APPTYPE CONSOLE}

{$WARN DUPLICATE_CTOR_DTOR OFF}

{$R *.res}

uses
  System.SysUtils,
  Kitto.Ext.Start,
  UseKitto in '..\..\Source\UseKitto.pas',
  Rules in '..\..\Source\Rules.pas';

begin
  try
    TKExtStart.Start;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
