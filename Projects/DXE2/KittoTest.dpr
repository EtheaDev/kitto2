program KittoTest;

uses
  Vcl.Forms,
  TestFramework,
  {$IFDEF USE_XML}XMLTestRunner{$ELSE}GUITestRunner{$ENDIF},
  EF.YAMLTest in '..\..\Test\EF.YAMLTest.pas',
  Kitto.TestCommon in '..\..\Test\Kitto.TestCommon.pas';

{$R *.res}

begin
  Application.Initialize;
  RunRegisteredTests;

end.
