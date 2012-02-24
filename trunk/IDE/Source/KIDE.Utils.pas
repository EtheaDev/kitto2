unit KIDE.Utils;

interface

function ExtractLocaleNameFromFileName(const AFileName: string): string;

procedure NotImplemented;

function GetKIDEVersion: string;

implementation

uses
  SysUtils, Dialogs,
  EF.StrUtils, EF.Localization;

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

procedure NotImplemented;
begin
  MessageDlg(_('Not yet implemented.'), mtInformation, [mbOK], 0);
end;

function GetBuildDateTimeAsString: string;
var
  LDateTime: TDateTime;
begin
  if FileAge(ParamStr(0), LDateTime) then
    Result := DateTimeToStr(LDateTime)
  else
    Result := _('unknown datetime');
end;

function GetKIDEVersion: string;
begin
  Result := Format(_('Pre-Release. Built on %s'),
    [GetBuildDateTimeAsString]);
end;

end.
