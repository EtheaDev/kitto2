unit KIDE.EFHelpers;

interface

uses
  Types,
  EF.Logger;

type
  TEFLoggerHelper = class helper for TEFLogger
  public
    procedure LogInfo(const AString: string);
    procedure LogWarning(const AString: string);
    procedure LogError(const AString: string);

    class function ExtractTag(const AString: string; out ATag: string): string;
  end;

const
  LOG_TAG_INFO = '{{i}}';
  LOG_TAG_WARNING = '{{w}}';
  LOG_TAG_ERROR = '{{e}}';

implementation

uses
  StrUtils;

{ TEFLoggerHelper }

class function TEFLoggerHelper.ExtractTag(const AString: string;
  out ATag: string): string;
begin
  Result := AString;

  if StartsStr(LOG_TAG_INFO, Result) then
  begin
    ATag := LOG_TAG_INFO;
    Delete(Result, 1, Length(LOG_TAG_INFO));
  end
  else if StartsStr(LOG_TAG_WARNING, Result) then
  begin
    ATag := LOG_TAG_WARNING;
    Delete(Result, 1, Length(LOG_TAG_WARNING));
  end
  else if StartsStr(LOG_TAG_ERROR, Result) then
  begin
    ATag := LOG_TAG_ERROR;
    Delete(Result, 1, Length(LOG_TAG_ERROR));
  end
  else
    ATag := '';
end;

procedure TEFLoggerHelper.LogError(const AString: string);
begin
  Log(LOG_TAG_ERROR + AString);
end;

procedure TEFLoggerHelper.LogInfo(const AString: string);
begin
  Log(LOG_TAG_INFO + AString);
end;

procedure TEFLoggerHelper.LogWarning(const AString: string);
begin
  Log(LOG_TAG_WARNING + AString);
end;

end.
