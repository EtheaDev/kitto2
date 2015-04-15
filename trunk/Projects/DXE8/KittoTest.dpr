program KittoTest;

uses
  Vcl.Forms,
  EF.YAMLTest in '..\..\Test\EF.YAMLTest.pas',
  Kitto.TestCommon in '..\..\Test\Kitto.TestCommon.pas',
  EF.TreeTest in '..\..\Test\EF.TreeTest.pas';

{$R *.res}

begin
  Application.Initialize;
  RunRegisteredKittoTests;
end.
