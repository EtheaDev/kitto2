{-------------------------------------------------------------------------------
   Copyright 2012 Ethea S.r.l.

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

///	<summary>
///	  Regex support. Wraps TPerlRegEx in Delphi versions prior do Delphi XE.
///	</summary>
unit EF.RegEx;

{$I EF.Defines.inc}

interface

///	<summary>
///   Matches a string to a pattern. If the specified pattern includes a regex
///   introducer, the rest is interpreted as a regular expression, otherwise
///   the whole string is interpreted as a (optionally negated) pattern and
///   passed to StrMatchesEx. The regex introducer is either the string
///   'REGEX:' (the function returns True if the string matches the expression)
///   or '~REGEX:' (the function returns True if the string DOESN'T match the
///   expression) at the beginning of the pattern.
///	</summary>
function StrMatchesPatternOrRegex(const AString, APatternOrRegex: string): Boolean;

///	<summary>
///	  Simple regex pattern matching.
///	</summary>
function RegExMatches(const AString, APattern: string): Boolean;

implementation

uses
  SysUtils, SyncObjs,
  {$IFDEF D15+}RegularExpressionsCore{$ELSE}PerlRegEx{$ENDIF},
  EF.StrUtils;

// Creating an instance of this component is costly, so we cache it.
var
  _RegExEngine: TPerlRegEx;
  _CriticalSection: TCriticalSection;

function GetRegExEngine: TPerlRegEx;
begin
  if Assigned(_RegExEngine) then
    Result := _RegExEngine
  else
  begin
    _CriticalSection.Enter;
    try
      _RegExEngine := TPerlRegEx.Create;
      Result := _RegExEngine;
    finally
      _CriticalSection.Enter;
    end;
  end;
end;

function RegExMatches(const AString, APattern: string): Boolean;
var
  LEngine: TPerlRegEx;
begin
  LEngine := GetRegExEngine;
  LEngine.RegEx := UTF8String(APattern);
  LEngine.Subject := UTF8String(AString);
  Result := LEngine.Match;
end;

function RegExDoesntMatch(const AString, APattern: string): Boolean;
begin
  Result := not RegExMatches(AString, APattern);
end;

function StrMatchesPatternOrRegex(const AString, APatternOrRegex: string): Boolean;
const
  REGEX_INTRODUCER = 'REGEX:';
  REGEX_NEGATED_INTRODUCER = '~REGEX:';
var
  LPattern: string;
  LMatchFunction: function(const AString, APattern: string): Boolean;
begin
  LPattern := APatternOrRegex;
  // Regexes are costly to process, so we only support them if explicitly
  // declared.
  if Pos(REGEX_INTRODUCER, LPattern) = 1 then
  begin
    LMatchFunction := RegExMatches;
    Delete(LPattern, 1, Length(REGEX_INTRODUCER));
  end
  else if Pos(REGEX_NEGATED_INTRODUCER, LPattern) = 1 then
  begin
    LMatchFunction := RegExDoesntMatch;
    Delete(LPattern, 1, Length(REGEX_NEGATED_INTRODUCER));
  end
  else
    LMatchFunction := StrMatchesEx;

  Result := LMatchFunction(AString, LPattern);
end;

initialization
  _CriticalSection := TCriticalSection.Create;

finalization
  FreeAndNil(_RegExEngine);
  FreeAndNil(_CriticalSection);

end.

