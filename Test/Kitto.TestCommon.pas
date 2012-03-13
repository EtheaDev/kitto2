unit Kitto.TestCommon;

interface

function GetWorkDirectory: string;

implementation

uses
  SysUtils;

function GetWorkDirectory: string;
begin
  Result := ExtractFilePath(ParamStr(0));
end;

end.
