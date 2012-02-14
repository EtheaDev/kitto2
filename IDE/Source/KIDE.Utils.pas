unit KIDE.Utils;

interface

function ExtractLocaleNameFromFileName(const AFileName: string): string;

implementation

uses
  EF.StrUtils;

function ExtractLocaleNameFromFileName(const AFileName: string): string;
var
  P: Integer;
  LLastBackslash: Integer;
begin
  P := Pos('\LC_MESSAGES\', AFileName);
  if P > 0 then
  begin
    Result := Copy(AFileName, 1, P - 1);
    LLastBackslash := RightPos('\', Result);
    Result := Copy(Result, LLastBackslash + 1, MaxInt);
  end
  else
    Result := AFileName;
end;

end.
