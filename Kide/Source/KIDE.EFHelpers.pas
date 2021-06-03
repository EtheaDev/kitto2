{-------------------------------------------------------------------------------
   Copyright 2012-2021 Ethea S.r.l.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
-------------------------------------------------------------------------------}
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
