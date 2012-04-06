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

unit EF.StrUtils;

{$I EF.Defines.inc}

interface

uses
  SysUtils, Types, Classes, DB,
  EF.Types;

///	<summary>
///	  Returns the index of the last occurrence of ASubString in AString.
///	  Returns 0 if ASubString is not contained in AString.
///	</summary>
function RightPos(const ASubString, AString: string): Integer;

///	<summary>
///	  Strips APrefix from the beginning of AString and ASuffix from the end of
///	  it, if found. Returns the stripped string. If a prefix or suffix isn't
///	  found, then that part of the input string is returned unchanged. The
///	  match is case insensitive. Passing '' in APrefix or ASuffix suppresses
///	  stripping of that part.
///	</summary>
function StripPrefixAndSuffix(const AString, APrefix, ASuffix: string): string;

///	<summary>
///	  Strips APrefix from the beginning of AString, if found. Returns the
///	  stripped string. If a prefix isn't found, then the input string is
///	  returned unchanged. The match is case insensitive.
///	</summary>
function StripPrefix(const AString, APrefix: string): string;

///	<summary>
///	  Strips ASuffix from the end of AString, if found. Returns the stripped
///	  string. If a suffix isn't found, then the input string is returned
///	  unchanged. The match is case insensitive.
///	</summary>
function StripSuffix(const AString, ASuffix: string): string;

///	<summary>
///	  Generates a random string of ALength characters in the 'A'..'Z' and
///	  '0'..'9' printable sets.
///	</summary>
function GetRandomString(const ALength: Integer): string;

///	<summary>
///	  Returns True if APattern matches AString. APattern may contain the
///	  following jolly characters: ? matches any one character. * matches any
///	  sequence of zero or more characters. Everything else is compared
///	  literally in a case sensitive manner.
///	</summary>
function StrMatches(const AString, APattern: string): Boolean;

///	<summary>
///	  Interprets a ~ character at the beginning of a pattern as a negation
///	  symbol. Otherwise it's identical to StrMatches.
///	</summary>
function StrMatchesEx(const AString, APattern: string): Boolean;

///	<summary>
///	  Returns the number of occurences of ASubstring in AString.
///	</summary>
function CountSubstrings(const AString, ASubstring: string): Integer;

///	<summary>
///	  Returns True if AString equals at least one of AStrings, False otherwise.
///	  Wraps IndexStr, not present in all supported Delphi versions.
///	</summary>
function MatchStr(const AString: string; const AStrings: array of string): Boolean;

///	<summary>
///	  Creates a new GUID and returns it as a string in a compact format (32
///	  chars).
///	</summary>
function CreateCompactGuidStr: string;

///	<summary>
///	  Creates a new GUID and returns it as a string.
///	</summary>
function CreateGuidStr: string;

///	<summary>
///	  Strips and returns (in ASubstring) the initial part of AString until
///	  ASeparator, or until the end of the string if ASeparator is not found. If
///	  ARemoveSeparator is True, then the separator is stripped from AString as
///	  well (but not included in the returned string anyway). The function
///	  returns True ultil it has reached the end of AString. When the function
///	  returns False, AString is always '', while ASubstring may or may not be
///	  ''.
///	</summary>
function FetchStr(var AString, ASubstring: string; const ASeparator: string = ';';
  const ARemoveSeparator: Boolean = True): Boolean;

///	<summary>
///	  Works like Pos, except that it returns 0 if ASubString is not part of
///	  AString as a whole word.
///	</summary>
///	<remarks>
///	  This function is case insensitive.
///	</remarks>
function WordPos(const AWord, AString: string): Integer; overload;

///	<summary>
///	  Calls the other version of WordPos for each item in AWords. Returns a
///	  value &lt;&gt; 0 the first time that WordPos returns a value &lt;&gt; 0,
///	  otherwise returns 0.
///	</summary>
///	<remarks>
///	  This function is case insensitive.
///	</remarks>
function WordPos(const AWords: array of string;
  const AString: string): Integer; overload;

///	<summary>
///	  If AString is shorter than AFinalLength characters, adds instances of
///	  PadCharacter to the right of the string until it is long exactly
///	  AFinalLength characters. If AString is long exactly AFinalLength
///	  characters, it is returned unchanged. If AString is longer than
///	  AFinalLength characters, an exception is raised.
///	</summary>
function PadRight(const AString: string;
  const AFinalLength: Integer; const APadCharacter: Char = ' '): string;

///	<summary>
///	  If AString is shorter than AFinalLength characters, adds instances of
///	  PadCharacter to the left of the string until it is long exactly
///	  AFinalLength characters. If AString is long exactly AFinalLength
///	  characters, it is returned unchanged. If AString is longer than
///	  AFinalLength characters, an exception is raised.
///	</summary>
function PadLeft(const AString: string;
  const AFinalLength: Integer; const APadCharacter: Char = '0'): string;

///	<summary>
///	  Reads a text file, line by line, and returns the content.
///	</summary>
function TextFileToString(const AFileName: string): string;

///	<summary>
///	  Writes AString to a text file.
///	</summary>
procedure StringToTextFile(const AString, AFileName: string;
  const AEncoding: TEncoding = nil);

///	<summary>
///	  Appends AString to an existing text file. If the file doesn't exist,
///	  works like StringToTextFile.
///	</summary>
procedure AppendStringToTextFile(const AString, AFileName: string);

///	<summary>
///	  Converts 'THIS_IS_A_STRING' to 'ThisIsAString'.
///	</summary>
function UpperUnderscoreToCamel(const AString: string): string;

///	<summary>
///	  Converts 'ThisIsAString' to 'THIS_IS_A_STRING'.
///	</summary>
function CamelToUpperUnderscore(const AString: string): string;

///	<summary>
///	  Converts 'ThisIsAString' to 'This Is A String'.
///	</summary>
function CamelToSpaced(const AString: string): string;

///	<summary>
///	  Tries to make the plural form of a specified singular name acoording to
///	  the rules of the english language.
///	</summary>
function MakePlural(const ASingularName: string): string;

///	<summary>
///	  Returns the index of AValue in AStrings, which contains name=value pairs.
///	</summary>
function GetIndexOfValue(const AStrings: TStrings; const AValue: string): Integer;

///	<summary>
///	  Inserts ASubstring into AString at the specified position and returns the
///	  resulting string.
///	</summary>
function InsertStr(const ASubstring, AString: string; const AIndex: Integer): string;

///	<summary>
///	  Returns the count of uninterrupted leading AChars in AString. Returns 0
///	  if AString does not begin with AChar.
///	</summary>
function CountLeading(const AString: string; const AChar: Char): Integer;

///	<summary>
///	  Converts all tab characters (#9) in AString with sequences of
///	  ASpacesPerTab spaces, and returns the resulting string.
///	</summary>
function TabsToSpaces(const AString: string; const ASpacesPerTab: Integer = 2): string;

///	<summary>
///	  Returns True if AString equals one of the strings in the specified
///	  comma-separated list, and False otherwise.
///	</summary>
function StringInCommaList(const AString, ACommaList: string): Boolean;

///	<summary>
///	  Returns the number of items in ACommaList.
///	</summary>
function CommaListItemCount(const ACommaList: string): Integer;

///	<summary>
///	  Returns ATerm1 + AConcatString + ATerm2. If either Term1 or Term2 is
///	  empty, returns the other non-empty term without adding AConcatString. If
///	  both ATerm1 and ATerm2 are empty, returns an empty string.
///	</summary>
function SmartConcat(const ATerm1, AConcatString, ATerm2: string): string;

///	<summary>
///	  Returns the first value in AValues that's different from AValue. If all
///	  values in AValues are equal to AValue, then the function returns AValue.
///	</summary>
function FirstDifferent(const AValues: array of string; const AValue: string): string;

///	<summary>Returns the MD5 hash of AString, encoded as a sequence of
///	lower-case hex values. The resulting string holds 32 hex digits,
///	corresponding to 16 bytes.</summary>
function GetStringHash(const AString: string): string;

///	<summary>
///	  Splits the specified string based on the specified separator characters
///	  and returns an array with the splitted elements.
///	</summary>
///	<param name="ASeparators">
///	  One or more separator characters.
///	</param>
function Split(const AString: string; const ASeparators: string = ' '): TStringDynArray;

///	<summary>
///	  Joins the elements in the specified array with the specified separator
///	  and returns the resulting string.
///	</summary>
function Join(const AStrings: TStringDynArray; const ASeparator: string = ''): string;

///	<summary>
///	  Splits the specified string based on the specified separator characters
///	  and returns an array of pairs with the splitted elements, assuming the
///	  strings are in pair (name=value) format.
///	</summary>
function SplitPairs(const AString: string; const ASeparators: string = ' '): TEFPairs;

///	<summary>
///	  Joins the pairs producing a string with a list of name=value pairs
///	  separated by the specified separator.
///	</summary>
function JoinPairs(const APairs: TEFPairs; const ASeparator: string = ''): string;

///	<summary>Formats the specified number of bytes in GBs, MBs, KBs or bytes
///	according to the size.</summary>
///	<example>FormatByteSize(2560) yields '2.5 KBs'</example>
function FormatByteSize(const AByteSize: Longint; const AFormatSettings: TFormatSettings): string;

implementation

uses
  StrUtils,
  IdHashMessageDigest, IdHash;

function RightPos(const ASubString, AString: string): Integer;
var
  LCharIndex: Integer;
  LSubStringLength: Integer;
begin
  Result := 0;
  LSubStringLength := Length(ASubString);
  for LCharIndex := Length(AString) - Length(ASubString) + 1 downto 1 do
  begin
    if Copy(AString, LCharIndex, LSubStringLength) = ASubString then
    begin
      Result := LCharIndex;
      Break;
    end;
  end;
end;

function StripPrefixAndSuffix(const AString, APrefix, ASuffix: string): string;
begin
  Result := AString;
  if (APrefix <> '') and AnsiStartsText(APrefix, Result) then
    Delete(Result, 1, Length(APrefix));
  if (ASuffix <> '') and AnsiEndsText(ASuffix, Result) then
    Delete(Result, Length(Result) - Length(ASuffix) + 1, Length(ASuffix));
end;

function StripPrefix(const AString, APrefix: string): string;
begin
  Result := StripPrefixAndSuffix(AString, APrefix, '');
end;

function StripSuffix(const AString, ASuffix: string): string;
begin
  Result := StripPrefixAndSuffix(AString, '', ASuffix);
end;

function GetRandomString(const ALength: Integer): string;
begin
  // If this function is moved out of this unit, then a call to Randomize should
  // be made somewhere in the application. See this unit's initialization section.
  Result := '';
  while Length(Result) < ALength do
    // Randomly decide whether the next character will be a letter or a number.
    if Random(2) = 1 then
      // A random character between '0' and '9'.
      Result := Result + Chr(Random(Ord('9') - Ord('0') + 1) + Ord('0'))
    else
      // A random character between 'A' e 'Z'.
      Result := Result + Chr(Random(Ord('Z') - Ord('A') + 1) + Ord('A'));
end;

function StrMatches(const AString, APattern: string): Boolean;
var
  // Stores the characters of AString except the first one.
  LRestOfString: string;
  // Stores the characters of APattern except the first one.
  LRestOfPattern: string;
begin
  LRestOfString := AString;
  Delete(LRestOfString, 1, 1);
  LRestOfPattern := APattern;
  Delete(LRestOfPattern, 1, 1);
  // Quick exit condition: a single * matches anything.
  // Note that it ain't so for multiple *s.
  if APattern = '*' then
    Result := True
  // An empty pattern matches an empty string.
  else if (AString = '') and (APattern = '') then
    Result := True
  // A non-empty pattern never matches an empty string, unless
  // it is a * (see one of the above cases).
  else if AString = '' then
    Result := False
  // An empty pattern doesn't match a non-empty string.
  else if APattern = '' then
    Result := False
  else
  begin
    // Non-empty pattern and non-empty string: compare pattern and
    // string character by character, taking care of jolly characters
    // in the pattern.
    case APattern[1] of
      '*':
      // Matches anything: match the rest of the pattern or the rest
      // of the string.
      begin
        if StrMatches(AString, LRestOfPattern) then
          Result := True
        else
          Result := StrMatches(LRestOfString, APattern);
      end;
      '?':
        // Matches any one character: advance both string and pattern.
        Result := StrMatches(LRestOfString, LRestOfPattern);
    else
      // No jolly: compare the characters and if they are equal then
      // match the rest, otherwise we don't have a match.
      if (AString[1] = APattern[1]) then
        Result := StrMatches(LRestOfString, LRestOfPattern)
      else
        Result := False;
    end;
  end;
end;

function StrMatchesEx(const AString, APattern: string): Boolean;
begin
  if (APattern <> '') and (APattern[1] = '~') then
    Result := not StrMatches(AString, Copy(APattern, 2, MaxInt))
  else
    Result := StrMatches(AString, APattern);
end;

function CountSubstrings(const AString, ASubstring: string): Integer;
var
  LSubstringPos: Integer;
begin
  Result := 0;
  LSubstringPos := PosEx(ASubstring, AString);
  while LSubstringPos > 0 do
  begin
    if LSubstringPos > 0 then
      Inc(Result);
    LSubstringPos := PosEx(ASubstring, AString, LSubstringPos + Length(ASubString));
  end;
end;

function MatchStr(const AString: string; const AStrings: array of string): Boolean;
begin
  Result := AnsiIndexStr(AString, AStrings) <> -1;
end;

function CreateCompactGuidStr: string;
var
  I: Integer;
  LBuffer: array[0..15] of Byte;
begin
  CreateGUID(TGUID(LBuffer));
  Result := '';
  for I := 0 to 15 do
    Result := Result + IntToHex(LBuffer[I], 2);
end;

function CreateGuidStr: string;
var
  LTempGUID: TGUID;
begin
  CreateGUID(LTempGUID);
  Result := GUIDToString(LTempGUID);
end;

function FetchStr(var AString, ASubstring: string; const ASeparator: string = ';';
  const ARemoveSeparator: Boolean = True): Boolean;
var
  LSeparatorPosition: Integer;
begin
  if AString = '' then
  begin
    ASubString := '';
    Result := False;
  end
  else
  begin
    LSeparatorPosition := Pos(ASeparator, AString);
    if LSeparatorPosition = 0 then
    begin
      // Reached the end of the string.
      ASubstring := AString;
      AString := '';
      Result := False;
    end
    else
    begin
      ASubstring := Copy(AString, 1, LSeparatorPosition - 1);
      if ARemoveSeparator then
        Delete(AString, 1, LSeparatorPosition - 1 + Length(ASeparator))
      else
        Delete(AString, 1, LSeparatorPosition - 1);
      Result := True;
    end;
  end;
end;

function WordPos(const AWord, AString: string): Integer;
const
  SEPARATORS = ' '#8#13#10;
var
  LLeadingChar, LTrailingChar: Char;
  LKeyword: string;
  LIsFound: Boolean;
  LStartPosition: Integer;
begin
  LIsFound := False;
  Result := Pos(AnsiUpperCase(AWord), AnsiUpperCase(AString));
  if Result <> 0 then
  begin
    LKeyword := Copy(AString,Result,Length(AString));

    if not LIsFound then
    begin
      // Check whether the word is at the beginning or end of the string or not.
      if (Result = 1) or (Length(LKeyword) = Length(AWord)) then
        LIsFound := True;
    end;

    if not LIsFound then
    begin
      // Check that the word is a single word.
      LLeadingChar := Copy(AString, Result - 1, 1)[1];
      LTrailingChar := LKeyword[Length(AWord) + 1];
      if (Pos(LLeadingChar, SEPARATORS) <> 0)
          and (Pos(LTrailingChar, SEPARATORS) <> 0) then
        LIsFound := True;
    end;

    if not LIsFound then
    begin
      // Recursion.
      LStartPosition := Result + Length(AWord);
      Result := LStartPosition - 1
        + WordPos(AWord, Copy(AString, LStartPosition, MaxInt));
      if Result <> LStartPosition - 1 then
        LIsFound := True;
    end;

    if not LIsFound and (Result <> 0) then
      Result := 0;
  end;
end;

function WordPos(const AWords: array of string; const AString: string): Integer;
var
  LWordIndex: Integer;
begin
  Result := 0;
  for LWordIndex := Low(AWords) to High(AWords) do
  begin
    Result := WordPos(AWords[LWordIndex], AString);
    if Result <> 0 then
      Break;
  end;
end;

function InternalPad(const AIsLeft: Boolean;
  const AString: string; const AFinalLength: Integer;
  const APadCharacter: Char): string;
var
  LInitialLength: Integer;
begin
  LInitialLength := Length(AString);
  if LInitialLength > AFinalLength then
    raise Exception.CreateFmt('String %s is longer than %d characters. Cannot pad.',
      [AString, AFinalLength]);
  if LInitialLength = AFinalLength then
    Result := AString
  else if AIsLeft then
    Result := DupeString(APadCharacter, AFinalLength - LInitialLength) + AString
  else
    Result := AString + DupeString(APadCharacter, AFinalLength - LInitialLength);
end;

function PadLeft(const AString: string;
  const AFinalLength: Integer; const APadCharacter: Char = '0'): string;
begin
  Result := InternalPad(True, AString, AFinalLength, APadCharacter);
end;

function PadRight(const AString: string;
  const AFinalLength: Integer; const APadCharacter: Char = ' '): string;
begin
  Result := InternalPad(False, AString, AFinalLength, APadCharacter);
end;

function TextFileToString(const AFileName: string): string;
var
  LStrings: TStrings;
begin
  Result := '';
  if FileExists(AFileName) then
  begin
    LStrings := TStringList.Create;
    try
      LStrings.LoadFromFile(AFileName);
      Result := LStrings.Text;
    finally
      FreeAndNil(LStrings);
    end;
  end;
end;

procedure StringToTextFile(const AString, AFileName: string;
  const AEncoding: TEncoding = nil);
var
  LFilePath: string;
  LWriter: TStreamWriter;
  LStrings: TStrings;
begin
  LFilePath := ExtractFilePath(AFileName);
  if LFilePath <> '' then
    ForceDirectories(LFilePath);

  LStrings := TStringList.Create;
  try
    LStrings.Text := AString;
    LStrings.WriteBOM := False;
    LStrings.SaveToFile(AFileName, AEncoding);
  finally
    FreeAndNil(LStrings);
  end;
end;

procedure AppendStringToTextFile(const AString, AFileName: string);
var
  LFile: TextFile;
begin
  if not FileExists(AFileName) then
    StringToTextFile(AString, AFileName)
  else
  begin
    AssignFile(LFile, AFileName);
    try
      Append(LFile);
      Write(LFile, AString);
    finally
      CloseFile(LFile);
    end;
  end;
end;

function CamelToUpperUnderscore(const AString: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Length(AString) do
  begin
    if AString[I] = UpperCase(AString[I]) then
      if I > 1 then
        Result := Result + '_';
    Result := Result + UpperCase(AString[I]);
  end;
end;

function CamelToSpaced(const AString: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Length(AString) do
  begin
    if AString[I] = UpperCase(AString[I]) then
      if I > 1 then
        Result := Result + ' ';
    Result := Result + AString[I];
  end;
end;

function UpperUnderscoreToCamel(const AString: string): string;
var
  I: Integer;
  LNextIsUpper: Boolean;
begin
  Result := '';
  LNextIsUpper := True;
  for I := 1 to Length(AString) do
  begin
    if AString[I] = '_' then
    begin
      LNextIsUpper := True;
      Continue;
    end;
    if LNextIsUpper then
    begin
      Result := Result + UpperCase(AString[I]);
      LNextIsUpper := False;
    end
    else
      Result := Result + LowerCase(AString[I]);
  end;
end;

function MakePlural(const ASingularName: string): string;
begin
  if ASingularName = '' then
    Result := ''
  else
  begin
    if not SameText(ASingularName[Length(ASingularName)], 's') then
    begin
      if SameText(ASingularName[Length(ASingularName)], 'y') then
        Result := Copy(ASingularName, 1, Length(ASingularName) - 1) + 'ies'
      else
        Result := ASingularName + 's';
    end
    else
      Result := ASingularName;
  end;
  if ASingularName = UpperCase(ASingularName) then
    Result := UpperCase(Result);
end;

function GetIndexOfValue(const AStrings: TStrings; const AValue: string): Integer;
begin
  for Result := 0 to AStrings.Count - 1 do
    if AStrings.Values[AStrings.Names[Result]] = AValue then
      Exit;
  Result := -1;
end;

function InsertStr(const ASubstring, AString: string; const AIndex: Integer): string;
begin
  Result := AString;
  Insert(ASubstring, Result, AIndex);
end;

function CountLeading(const AString: string; const AChar: Char): Integer;
var
  I: Integer;
begin
  Result := 0;
  if AString <> '' then
    for I := 1 to Length(AString) do
    begin
      if AString[I] = AChar then
        Inc(Result)
      else
        Break;
    end;
end;

function TabsToSpaces(const AString: string; const ASpacesPerTab: Integer): string;
begin
  Result := ReplaceStr(AString, #9, StringOfChar(' ', ASpacesPerTab));
end;

function GetStringHash(const AString: string): string;
var
  LHash: TIdHashMessageDigest5;
begin
  if AString = '' then
    Result := ''
  else
  begin
    LHash := TIdHashMessageDigest5.Create;
    try
      Result := LowerCase(LHash.HashStringAsHex(AString));
    finally
      FreeAndNil(LHash);
    end;
  end;
end;

function StringInCommaList(const AString, ACommaList: string): Boolean;
var
  LCommaList: TStrings;
begin
  LCommaList := TStringList.Create;
  try
    LCommaList.CommaText := ACommaList;
    Result := LCommaList.IndexOf(AString) >= 0;
  finally
    LCommaList.Free;
  end;
end;

function CommaListItemCount(const ACommaList: string): Integer;
var
  LCommaList: TStrings;
begin
  LCommaList := TStringList.Create;
  try
    LCommaList.CommaText := ACommaList;
    Result := LCommaList.Count;
  finally
    LCommaList.Free;
  end;
end;

function SmartConcat(const ATerm1, AConcatString, ATerm2: string): string;
begin
  if ATerm1 = '' then
    Result := ATerm2
  else if ATerm2 = '' then
    Result := ATerm1
  else
    Result := ATerm1 + AConcatString + ATerm2;
end;

function FirstDifferent(const AValues: array of string; const AValue: string): string;
var
  LValueIndex: Integer;
begin
  Result := AValue;
  for LValueIndex := Low(AValues) to High(AValues) do
  begin
    if AValues[LValueIndex] <> AValue then
    begin
      Result := AValues[LValueIndex];
      Break;
    end;
  end;
end;

function IsoDayOfWeekToDayOfWeek(const ADay: Integer): Integer;
begin
  Assert((ADay >= 1) and (ADay <= 7));
  Result := Succ(ADay);
  if Result > 7 then
    Result := 1;
end;

{$IFNDEF D15+}
function SplitString(const S, Delimiters: string): TStringDynArray;
var
  StartIdx: Integer;
  FoundIdx: Integer;
  SplitPoints: Integer;
  CurrentSplit: Integer;
  i: Integer;
begin
  Result := nil;

  if S <> '' then
  begin
    SplitPoints := 0;
    for i := 1 to Length(S) do
      if IsDelimiter(Delimiters, S, i) then
        Inc(SplitPoints);
    SetLength(Result, SplitPoints + 1);
    StartIdx := 1;
    CurrentSplit := 0;
    repeat
      FoundIdx := FindDelimiter(Delimiters, S, StartIdx);
      if FoundIdx <> 0 then
      begin
        Result[CurrentSplit] := Copy(S, StartIdx, FoundIdx - StartIdx);
        Inc(CurrentSplit);
        StartIdx := FoundIdx + 1;
      end;
    until CurrentSplit = SplitPoints;
    Result[SplitPoints] := Copy(S, StartIdx, Length(S) - StartIdx + 1);
  end;
end;
{$ENDIF}

function Split(const AString: string; const ASeparators: string): TStringDynArray;
begin
  Result := SplitString(AString, ASeparators);
end;

function Join(const AStrings: TStringDynArray; const ASeparator: string): string;
var
  LString: string;
begin
  Result := '';
  for LString in AStrings do
  begin
    if Result = '' then
      Result := LString
    else
      Result := Result + ASeparator + LString;
  end;
end;

function SplitPairs(const AString: string; const ASeparators: string = ' '): TEFPairs;
var
  LStrings: TStringDynArray;
  I: Integer;
  LItem: TStringDynArray;
begin
  LStrings := Split(AString, ASeparators);
  SetLength(Result, Length(LStrings));
  for I := Low(LStrings) to High(LStrings) do
  begin
    LItem := Split(LStrings[I], '=');
    if Length(LItem) = 2 then
      Result[I] := TEFPair.Create(LItem[0], LItem[1])
    else
      Result[I] := TEFPair.Create(LStrings[I], '');
  end;
end;

function JoinPairs(const APairs: TEFPairs; const ASeparator: string = ''): string;
var
  LStrings: TStringDynArray;
  I: Integer;
begin
  SetLength(LStrings, Length(APairs));
  for I := Low(APairs) to High(APairs) do
    LStrings[I] := APairs[I].Key + '=' + APairs[I].Value;
  Result := Join(LStrings, ASeparator);
end;

function FormatByteSize(const AByteSize: Longint;
  const AFormatSettings: TFormatSettings): string;
const
  B = 1;
  KB = 1024 * B;
  MB = 1024 * KB;
  GB = 1024 * MB;
begin
  if AByteSize > GB then
    Result := FormatFloat('#.## GBs', AByteSize / GB, AFormatSettings)
  else if AByteSize > MB then
    Result := FormatFloat('#.## MBs', AByteSize / MB, AFormatSettings)
  else if AByteSize > KB then
    Result := FormatFloat('#.## KBs', AByteSize / KB, AFormatSettings)
  else
    Result := FormatFloat('#.## bytes', AByteSize, AFormatSettings);
end;

initialization
  Randomize;

end.
