(*
  This module contains a custom syntax highlighter for a YAML file
  Carlo Barazzetta
*)
unit KIDE.YAMLHighlighter;

interface

uses
  ToolsAPI;

const
  YAML_ID_STRING = 'YAML';

type
  // A class to define an new IDE Highlighter for YAML Files
  TYAMLHighlighter = class(TNotifierObject, IOTANotifier, IOTAHighlighter, IOTAHighlighterPreview)
  public
    constructor Create;
    //Implementation of IOTAHighlighter
    function GetIDString: string;
    function GetName: string;
    procedure Tokenize(StartClass: Byte; LineBuf: PAnsiChar; LineBufLen: Word;
      HighlightCodes: POTASyntaxCode);
    function TokenizeLineClass(StartClass: Byte; LineBuf: PAnsiChar;
      LineBufLen: Word): Byte;
    //Implementation of IOTAHighlighterPreview
    function  GetBlockEndCol: Integer;
    function  GetBlockEndLine: Integer;
    function  GetBlockStartCol: Integer;
    function  GetBlockStartLine: Integer;
    function  GetCurrentInstructionLine: Integer;
    function  GetDisabledBreakpointLine: Integer;
    function  GetDisplayName: string;
    function  GetErrorLine: Integer;
    function  GetInvalidBreakpointLine: Integer;
    function  GetSampleSearchText: string;
    function  GetSampleText: string;
    function  GetValidBreakpointLine: Integer;
  end;

  procedure RegisterYAMLHighlighter;
  procedure UnregisterYAMLHighlighter;

implementation

uses
  SysUtils
  , Registry
  , Windows;

var
  isMultipleLine : boolean;

constructor TYAMLHighlighter.Create;
var
  EditOps : IOTAEditOptions;
  iEditOps : Integer;
begin
  EditOps := Nil;
  with (BorlandIDEServices as IOTAEditorServices) do
  begin
   For iEditOps := 0 To EditOptionsCount - 1 Do
      If EditorOptions[iEditOps].IDString = YAML_ID_STRING Then
      begin
        EditOps := EditorOptions[iEditOps];
        EditOps.SyntaxHighlighter := Self;
      end;
  end;
  if not Assigned(EditOps) then
  begin
    EditOps := (BorlandIDEServices As IOTAEditorServices).AddEditOptions(YAML_ID_STRING);
    EditOps.BeginUpdate;
    Try
      EditOps.Extensions := 'yaml;yml';
      EditOps.OptionsName := 'YAML';
      EditOps.SyntaxHighlighter := Self;
      EditOps.BufferOptions.AutoIndent := True;
      EditOps.BufferOptions.UseTabCharacter := False;
      EditOps.BufferOptions.SmartTab := False;
      EditOps.BufferOptions.CursorThroughTabs := True;
      EditOps.BufferOptions.BackspaceUnindents := True;
      EditOps.BufferOptions.KeepTrailingBlanks := False;
      EditOps.BufferOptions.ShowTab := True;
      EditOps.BufferOptions.ShowSpace := False;
      EditOps.BufferOptions.SyntaxHighlight := True;
      EditOps.BufferOptions.ShowLineBreaks := False;
      EditOps.BufferOptions.HighlightCurrentLine := True;
      EditOps.BufferOptions.TabStops := '2';
      EditOps.BlockIndent := 2;
    Finally
      EditOps.EndUpdate;
    End;
  end;
end;

function TYAMLHighlighter.GetSampleText: string;
begin
  Result :=
    ' #first line comment'+sLineBreak+
    ''+sLineBreak+
    'string: String value'+sLineBreak+
    'quoted_string: "String value"'+sLineBreak+
    'integer: 123'+sLineBreak+
    'comment_value: #value'+sLineBreak+
    'no_comment_value: string #value'+sLineBreak+
    ''+sLineBreak+
    'node:'+sLineBreak+
    '  first_item: First value'+sLineBreak+
    '  second_item: 2'+sLineBreak+
    ''+sLineBreak+
    'nodes:'+sLineBreak+
    '  subnode:    A4786'+sLineBreak+
    '    description: Water Bucket (Filled)'+sLineBreak+
    '    price:       1.47'+sLineBreak+
    '    quantity:    4'+sLineBreak+
    '    shipdate:    2012-09-30'+sLineBreak+
    ''+sLineBreak+
    '  subnode:   E1628'+sLineBreak+
    '    description: High Heeled "Ruby" Slippers'+sLineBreak+
    '    size:      8'+sLineBreak+
    '    price:     133.7'+sLineBreak+
    '    quantity:  1'+sLineBreak+
    '    shipdate:    2017-01-28'+sLineBreak+
    ''+sLineBreak+
    'bill-to:  &id001'+sLineBreak+
    '  street: |'+sLineBreak+
    '    123 Tornado Alley'+sLineBreak+
    '    Suite 16'+sLineBreak+
    '  city: East Centerville'+sLineBreak+
    '  state: KS'+sLineBreak+
    ''+sLineBreak+
    'ship-to:  *id001'+sLineBreak+
    ''+sLineBreak+
    'specialDelivery:  >'+sLineBreak+
    '    Follow the Yellow Brick'+sLineBreak+
    '    Road to the Emerald City.'+sLineBreak+
    '    Pay no attention to the'+sLineBreak+
    '    man behind the curtain.'+sLineBreak;
end;

function TYAMLHighlighter.GetBlockEndCol: Integer;
begin
  Result := 26;
end;

function TYAMLHighlighter.GetBlockEndLine: Integer;
begin
  Result := 11;
end;

function TYAMLHighlighter.GetBlockStartCol: Integer;
begin
  Result := 3;
end;

function TYAMLHighlighter.GetBlockStartLine: Integer;
begin
  Result := 10;
end;

function TYAMLHighlighter.GetCurrentInstructionLine: Integer;
begin
  Result := -1;
end;

function TYAMLHighlighter.GetDisabledBreakpointLine: Integer;
begin
  Result := -1;
end;

function TYAMLHighlighter.GetDisplayName: string;
begin
  Result := 'YAML';
end;

function TYAMLHighlighter.GetErrorLine: Integer;
begin
  Result := -1;
end;

function TYAMLHighlighter.GetIDString: string;
begin
  Result := YAML_ID_STRING;
end;

function TYAMLHighlighter.GetInvalidBreakpointLine: Integer;
begin
  Result := -1;
end;

function TYAMLHighlighter.GetName: string;
begin
  Result := 'YAML Files';
end;

function TYAMLHighlighter.GetSampleSearchText: string;
begin
  Result := 'Water';
end;

function TYAMLHighlighter.GetValidBreakpointLine: Integer;
begin
  Result := -1;
end;

//  This function returns true if the given word is a number
function IsNumber(const strWord : AnsiString): Boolean;
var
  i: Integer;
  CurChar: AnsiChar;
begin
  Result := False;
  if length(strWord) = 0 then
    Exit;
  CurChar := strWord[1];
  if CurChar in ['0'..'9','-','.'] then
  begin
    Result := CurChar in ['0'..'9'];
    for i := 1 to length(strWord) - 1 do
    begin
      CurChar := strWord[i];
      if not (CurChar in ['0'..'9','.']) then
      begin
        Result := False;
        Break;
      end
      else
        Result := True;
    end;
  end;
end;

(*
  This method returns the higlighter mark up codes for the given Line Buffer.

  @param   StartClass     as a Byte
  @param   LineBuf        as a PAnsiChar
  @param   LineBufLen     as a Word
  @param   HighlightCodes as a POTASyntaxCode
*)
procedure TYAMLHighlighter.Tokenize(StartClass: Byte; LineBuf: PAnsiChar;
  LineBufLen: Word; HighlightCodes: POTASyntaxCode);

Type
  TBlockType = (btNone, btIdentifier, btString, btNumber, btComment);

Const
  strAllSymbols = ([#33..#255]);
  strChars = (['a'..'z', 'A'..'Z', '%']);
  strNumbers = (['0'..'9']);
  strSeparator = ([':']);
  strBlock = (['>', '|']);
  strSymbols = (strAllSymbols - strChars - strNumbers - strSeparator - strBlock);

Var
  Codes : PAnsiChar;
  i : Integer;
  CurChar : AnsiChar;
  BlockType : TBlockType;
  strToken: AnsiString;
  j: Integer;
  isAfterIdentifier : boolean;
  PosEndIdent : integer;
begin
  PosEndIdent := 0;
  SetLength(strToken, 100);
  BlockType := btNone;
  Codes := PAnsiChar(HighlightCodes);
  FillChar(HighlightCodes^, LineBufLen, $E); // No highlighter
  isAfterIdentifier := False;
  For i := 0 To LineBufLen - 1 Do
    begin
      if (StartClass <> atComment) and (StartClass <> atCharacter) then
      begin
        CurChar := LineBuf[i];
        if ((CurChar In ['#'])) Or (BlockType In [btComment]) then
        begin
          Codes[i] := AnsiChar(atComment);
          BlockType := btComment;
        end
        else if (CurChar In [':'])  then
        begin
          For j := i - 1 DownTo 0 Do
            Codes[j] := AnsiChar(atIdentifier);

          Codes[i] := AnsiChar(atIdentifier);
          isMultipleLine := False;
          isAfterIdentifier := True;
          PosEndIdent := i;
        end
        else if (CurChar In ['"']) or (BlockType In [btString]) then
        begin
          Codes[i] := AnsiChar(atString);
          BlockType := btString;
        end
        else if (CurChar In strChars) then
        begin
          Codes[i] := AnsiChar(atString);
          if isAfterIdentifier then
          begin
            BlockType := btString;
            if (PosEndIdent+1 < i) then
              For j := i - 1 DownTo PosEndIdent+1 Do
                Codes[j] := AnsiChar(atString);
          end;
        end
        else if (CurChar In strNumbers) or (CurChar In ['-','.']) or (BlockType In [btNumber]) then
        begin
          Codes[i] := AnsiChar(atNumber);
          BlockType := btNumber;
        end
        else if CurChar In strSymbols then
          Codes[i] := AnsiChar(atString)
        else if CurChar In strBlock then
          isMultipleLine := True;

        if isMultipleLine or ((not isAfterIdentifier) and not (BlockType In [btComment]))  then
          Codes[i] := AnsiChar(atCharacter);

      end
      else
        Codes[i] := Char(atComment);
    end;
end;

(**

  This method returns the highlighter code for the next line in the editor. Used
  for the block comment.

  @precon  None.
  @postcon Returns the highlighter code for the next line in the editor. Used
           for the block comment.

  @param   StartClass as a Byte
  @param   LineBuf    as a PAnsiChar
  @param   LineBufLen as a Word
  @return  a Byte

**)
function TYAMLHighlighter.TokenizeLineClass(StartClass: Byte; LineBuf: PAnsiChar;
  LineBufLen: Word): Byte;

begin
  Result := StartClass;
(*
  CurChar := #0;
  for i := 0 To LineBufLen - 1 Do
  begin
    LastChar := CurChar;
    CurChar := LineBuf[i];
    Result := atWhiteSpace;
  end;
*)
end;

procedure UnregisterYAMLHighlighter;
var
  I: Integer;
  OTAHighlightServices: IOTAHighlightServices;
begin
  OTAHighlightServices := (BorlandIDEServices As IOTAHighlightServices);
  for I := 0 to OTAHighlightServices.HighlighterCount -1 do
  begin
    if OTAHighlightServices.Highlighter[I].IDString = YAML_ID_STRING then
    begin
      (BorlandIDEServices As IOTAHighlightServices).RemoveHighlighter(I);
      Exit;
    end;
  end;
end;

procedure RegisterYAMLHighlighter;
begin
  //UnregisterYAMLHighlighter;
  (BorlandIDEServices As IOTAHighlightServices).AddHighlighter(TYAMLHighlighter.Create);
end;

Initialization
//  RegisterYAMLHighlighter;

Finalization
  UnregisterYAMLHighlighter;

end.
