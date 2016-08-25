unit Kitto.TestCommon;

interface

function GetWorkDirectory: string;

procedure RunRegisteredKittoTests;

implementation

uses
  {$IFDEF USE_XML}XMLTestRunner{$ELSE}GUITestRunner{$ENDIF},
  SysUtils;

procedure RunRegisteredKittoTests;
begin
  RunRegisteredTests;
end;

function GetWorkDirectory: string;
begin
  Result := ExtractFilePath(ParamStr(0));
end;

end.
