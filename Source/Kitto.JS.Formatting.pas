unit Kitto.JS.Formatting;

interface

uses
  SysUtils
  , TypInfo
  , Kitto.JS.Types
  ;

type
  TJS = class
  public
    /// <summary>
    /// Converts a string with param placeholders to a JavaScript string.
    /// Converts a string representing a regular expression to a JavaScript RegExp.
    /// Replaces " to ', #13#10 to <br/> and isolated #13 or #10 to <br/>.
    /// Surrounds the string with " and inserts %0..%9 placeholders.
    /// </summary>
    class function StrToJS(const AString: string; AUseBR: Boolean = False): string;

    /// <summary>
    /// Converts a Pascal enumerated type constant into a JS name, by removing
    /// the lowercase prefix and returning the rest, converted to lowercase.
    /// </summary>
    class function EnumToJSString(const ATypeInfo: PTypeInfo; const AValue: Integer): string;

    /// <summary>
    ///  Generates a padding style declaration with the provided data and returns
    ///  it as a string.
    /// </summary>
    class function GetPadding(const ATop: Integer; const ARight: Integer = 0; const ABottom: Integer = -1;
      const ALeft: Integer = 0; const ACSSUnit: TCSSUnit = cssPX; const AHeader: Boolean = True): string;

    /// <summary>
    ///  Generates a margins style declaration with the provided data and returns
    ///  it as a string.
    /// </summary>
    class function GetMargins(const ATop: Integer; const ARight: Integer = 0; const ABottom: Integer = -1;
      const ALeft: Integer = 0; const ACSSUnit: TCSSUnit = cssPX; const AHeader: Boolean = True): string;

    class function JSDateToDateTime(const AJSDate: string): TDateTime;

    class function RemoveLastJSTerminator(const AJSCode: string): string;

    class function DelphiDateTimeFormatToJSDateTimeFormat(const ADateTimeFormat: string): string;
    class function DelphiDateFormatToJSDateFormat(const ADateFormat: string): string;
    class function DelphiTimeFormatToJSTimeFormat(const ATimeFormat: string): string;

    class function WrapInAnonymousFunction(const AArgs, ABody: string; const AReturn: string = ''): string;
  end;

  TJSFormatter = class
  private
    FCurrentIndent: Integer;
    FFormattedText: string;
    FFormatSettings: TFormatSettings;
    function IndentStr: string; inline;
  public
    procedure AfterConstruction; override;
  public
    property FormatSettings: TFormatSettings read FFormatSettings;

    function Indent: TJSFormatter;
    function Outdent: TJSFormatter;

    function OpenObject: TJSFormatter;
    function CloseObject: TJSFormatter;
    function OpenArray: TJSFormatter;
    function CloseArray: TJSFormatter;
    function OpenRound: TJSFormatter;
    function CloseRound: TJSFormatter;
    function Add(const AString: string): TJSFormatter;
    function AddLine(const ALine: string): TJSFormatter;
    function AddIndent: TJSFormatter;
    function AddIndented(const AString: string): TJSFormatter;
    function AddIndentedLine(const ALine: string): TJSFormatter;
    function AddIndentedPairLine(const AName, AStrValue: string;
      const AQuoteValue: Boolean = True; const AAddComma: Boolean = True): TJSFormatter;
    function AddIndentedPair(const AName, AStrValue: string;
      const AQuoteValue: Boolean = True; const AAddComma: Boolean = True;
      const AConnector: string = ': '): TJSFormatter;
    // Adds empty line
    function SkipLine: TJSFormatter;
    function AddIndentedList(const ALines: TArray<string>): TJSFormatter;
    // Shortcut for OpenObject + AddLines + CloseObject
    function FormatObject(const ALines: TArray<string>): TJSFormatter;
    // Shortcut for OpenArray + AddLines + CloseArray
    function FormatArray(const ALines: TArray<string>): TJSFormatter;

    function DeleteTrailing(const AString: string): TJSFormatter;

    property FormattedText: string read FFormattedText;
  end;

implementation

uses
  Character
  , StrUtils
  , DateUtils
  , Types
  , EF.StrUtils
  ;

{ TJS }

class function TJS.StrToJS(const AString: string; AUseBR: Boolean): string;
var
  I, J: Integer;
  BR: string;
begin
  BR := IfThen(AUseBR, '<br/>', '\n');
  Result := AnsiReplaceStr(AString, '"', '\"');
  Result := AnsiReplaceStr(Result, ^M^J, BR);
  Result := AnsiReplaceStr(Result, ^M, BR);
  Result := AnsiReplaceStr(Result, ^J, BR);
  if (Result <> '') and (Result[1] = #3) then
  begin // Is RegEx
    Delete(Result, 1, 1);
    if pos('/', Result) <> 1 then
      Result := '/' + Result + '/';
  end
  else
  begin
    I := pos('%', Result);
    if (pos(';', Result) = 0) and (I <> 0) and ((Length(Result) > 1) and (I < Length(Result)) and Result[I + 1].IsNumber) then
    begin // Has param place holder, ";" disable place holder
      J := FirstDelimiter(' "''[]{}><=!*-+/,', Result, I + 2);
      if J = 0 then
        J := Length(Result) + 1;
      if J <> (Length(Result) + 1) then
      begin
        insert('+"', Result, J);
        Result := Result + '"';
      end;
      if I <> 1 then
      begin
        insert('"+', Result, I);
        Result := '"' + Result;
      end;
    end
    else if (I = 1) and (Length(Result) > 1) and Result[2].IsLetter then
      Result := Copy(Result, 2, Length(Result))
    else
      Result := '"' + Result + '"'
  end;
end;

class function TJS.EnumToJSString(const ATypeInfo: PTypeInfo; const AValue: Integer): string;
var
  I: Integer;
  JS: string;
begin
  Result := '';
  JS := GetEnumName(ATypeInfo, AValue);
  for I := 1 to Length(JS) do
  begin
    if JS[I].IsLetter and JS[I].IsUpper then
    begin
      Result := LowerCase(Copy(JS, I, 100));
      if Result = 'perc' then
        Result := '%';
      Exit;
    end;
  end;
end;

class function TJS.GetPadding(const ATop: Integer; const ARight: Integer; const ABottom: Integer; const ALeft: Integer;
  const ACSSUnit: TCSSUnit; const AHeader: Boolean): string;
begin
  Result := Format('%s%d%3:s %2:d%3:s', [IfThen(AHeader, 'padding: ', ''), ATop, ARight,
    EnumToJSString(TypeInfo(TCSSUnit), Ord(ACSSUnit))]);
  if ABottom <> -1 then
    Result := Result + Format(' %d%2:s %1:d%2:s', [ABottom, ALeft, EnumToJSString(TypeInfo(TCSSUnit), Ord(ACSSUnit))]);
end;

class function TJS.JSDateToDateTime(const AJSDate: string): TDateTime;
begin
  Result := EncodeDateTime(StrToInt(Copy(AJSDate, 12, 4)), AnsiIndexStr(Copy(AJSDate, 5, 3),
    ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']) + 1,
    StrToInt(Copy(AJSDate, 9, 2)), StrToInt(Copy(AJSDate, 17, 2)), StrToInt(Copy(AJSDate, 20, 2)),
    StrToInt(Copy(AJSDate, 23, 2)), 0);
end;

class function TJS.RemoveLastJSTerminator(const AJSCode: string): string;
begin
  Result := AJSCode;
  while EndsStr(sLineBreak, Result) do
    Result := Copy(Result, 1, Length(Result) - Length(sLineBreak));
  if (Result <> '') and (Result[Length(Result)] = ';') then
    Delete(Result, Length(Result), 1);
end;

class function TJS.GetMargins(const ATop: Integer; const ARight: Integer; const ABottom: Integer; const ALeft: Integer;
const ACSSUnit: TCSSUnit; const AHeader: Boolean): string;
begin
  Result := Format('%s%d%5:s %2:d%5:s %3:d%5:s %4:d%s', [IfThen(AHeader, 'margin: ', ''), ATop, ARight, ABottom, ALeft,
    EnumToJSString(TypeInfo(TCSSUnit), Ord(ACSSUnit))])
end;

class function TJS.DelphiDateTimeFormatToJSDateTimeFormat(const ADateTimeFormat: string): string;
var
  LFormats: TStringDynArray;
begin
  LFormats := Split(ADateTimeFormat);
  Assert(Length(LFormats) = 2);

  Result := DelphiDateFormatToJSDateFormat(LFormats[0]) + ' ' + DelphiTimeFormatToJSTimeFormat(LFormats[1]);
end;

class function TJS.DelphiDateFormatToJSDateFormat(const ADateFormat: string): string;
begin
  Result := ReplaceText(ADateFormat, 'yyyy', 'Y');
  Result := ReplaceText(Result, 'yy', 'y');
  Result := ReplaceText(Result, 'dd', 'd');
  Result := ReplaceText(Result, 'mm', 'm');
end;

class function TJS.DelphiTimeFormatToJSTimeFormat(const ATimeFormat: string): string;
begin
  Result := ReplaceText(ATimeFormat, 'hh', 'H');
  Result := ReplaceText(Result, 'mm', 'i');
  Result := ReplaceText(Result, 'nn', 'i');
  Result := ReplaceText(Result, 'ss', 's');
end;

class function TJS.WrapInAnonymousFunction(const AArgs, ABody: string; const AReturn: string): string;
begin
  { TODO : formatting }
{ TODO : find the best place where to insert a return statement when not specified? }
  Result := 'function(' + AArgs + ') {' + sLineBreak +
    ABody + ';' + sLineBreak;
  if AReturn <> '' then
    Result := Result + 'return ' + AReturn + ';' + sLineBreak;
  Result := Result + '}' + sLineBreak;
end;

{ TJSFormatter }

function TJSFormatter.Add(const AString: string): TJSFormatter;
begin
  FFormattedText := FFormattedText + AString;
  Result := Self;
end;

function TJSFormatter.AddIndent: TJSFormatter;
begin
  Result := Add(IndentStr);
end;

function TJSFormatter.AddIndented(const AString: string): TJSFormatter;
begin
  Result := Add(IndentStr + AString.Replace(sLineBreak, sLineBreak + IndentStr));
end;

function TJSFormatter.AddIndentedLine(const ALine: string): TJSFormatter;
begin
  Result := AddIndented(ALine + sLineBreak);
end;

function TJSFormatter.AddIndentedList(const ALines: TArray<string>): TJSFormatter;
var
  I: Integer;
begin
  for I := Low(ALines) to High(ALines) do
  begin
    if I < High(ALines) then
      AddIndentedLine(ALines[I] + ',')
    else
      AddIndentedLine(ALines[I]);
  end;
  Result := Self;
end;

function TJSFormatter.AddIndentedPair(const AName, AStrValue: string;
  const AQuoteValue, AAddComma: Boolean; const AConnector: string): TJSFormatter;
begin
  Result := AddIndented(AName + AConnector + IfThen(AQuoteValue, '"', '') + AStrValue + IfThen(AQuoteValue, '"', '')
    + IfThen(AAddComma, ',', ''));
end;

function TJSFormatter.AddIndentedPairLine(const AName, AStrValue: string;
  const AQuoteValue: Boolean; const AAddComma: Boolean): TJSFormatter;
begin
  Result := AddIndentedLine(AName + ': ' + IfThen(AQuoteValue, '"', '') + AStrValue + IfThen(AQuoteValue, '"', '')
    + IfThen(AAddComma, ',', ''));
end;

function TJSFormatter.AddLine(const ALine: string): TJSFormatter;
begin
  Result := Add(ALine + sLineBreak);
end;

procedure TJSFormatter.AfterConstruction;
begin
  inherited;
  FFormatSettings := TFormatSettings.Create;
  FFormatSettings.DecimalSeparator := '.';
end;

function TJSFormatter.CloseArray: TJSFormatter;
begin
  Result := SkipLine.Outdent.AddIndented(']');
end;

function TJSFormatter.CloseObject: TJSFormatter;
begin
  Result := SkipLine.Outdent.AddIndented('}');
end;

function TJSFormatter.CloseRound: TJSFormatter;
begin
  Result := SkipLine.Outdent.AddIndented(')');
end;

function TJSFormatter.DeleteTrailing(const AString: string): TJSFormatter;
begin
  FFormattedText := StripSuffix(FFormattedText, AString);
  Result := Self;
end;

function TJSFormatter.FormatArray(const ALines: TArray<string>): TJSFormatter;
begin
  Result := OpenArray.AddIndentedList(ALines).CloseArray;
end;

function TJSFormatter.FormatObject(const ALines: TArray<string>): TJSFormatter;
begin
  Result := OpenObject.AddIndentedList(ALines).CloseObject;
end;

function TJSFormatter.Indent: TJSFormatter;
begin
  Inc(FCurrentIndent);
  Result := Self;
end;

function TJSFormatter.IndentStr: string;
begin
  Result := DupeString('  ', FCurrentIndent);
end;

function TJSFormatter.OpenArray: TJSFormatter;
begin
  Result := Add('[').SkipLine.Indent;
end;

function TJSFormatter.OpenObject: TJSFormatter;
begin
  Result := Add('{').SkipLine.Indent;
end;

function TJSFormatter.OpenRound: TJSFormatter;
begin
  Result := Add('(').SkipLine.Indent;
end;

function TJSFormatter.Outdent: TJSFormatter;
begin
  Dec(FCurrentIndent);
  Result := Self;
end;

function TJSFormatter.SkipLine: TJSFormatter;
begin
  Result := Add(sLineBreak);
end;

end.
