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
///	  Simple YAML reader and writer providing persistence for EF trees.
///	</summary>
unit EF.YAML;

interface

uses
  SysUtils, Classes, Generics.Collections,
  EF.Tree;

type
  TEFYAMLValueType = (vtSingleLine, vtMultiLineWithNL, vtMultiLineWithSpace);

  ///	<summary>
  ///	  A parser for yaml data streams.
  ///	</summary>
  TEFYAMLParser = class
  strict private
    FLastAnnotations: TStrings;
    FIndents: TList<Integer>;
    FPrevIndent: Integer;
    FLastValueType: TEFYAMLValueType;
    FLastIndentIncrement: Integer;
    FNextValueType: TEFYAMLValueType;
    FMultiLineFirstLineIndent: Integer;
    FLastValueQuoted: Boolean;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
  public
    ///	<summary>
    ///	  Resets the parser, maeking it ready for parsing a new data stream.
    ///	</summary>
    procedure Reset;

    ///	<summary>
    ///	  Parses a yaml line and returns name and value. Also sets the
    ///	  LastValueType and LastIndentIncrement properties.
    ///	</summary>
    function ParseLine(const ALine: string; out AName, AValue: string): Boolean;

    ///	<summary>
    ///	  Reports the type (not the data type) of the last parsed value, which
    ///	  may be a single-line or multi-line (two kinds) value.
    ///	</summary>
    property LastValueType: TEFYAMLValueType read FLastValueType;

    ///	<summary>
    ///	  Reports the indent increment of the last parsed line.
    ///	</summary>
    property LastIndentIncrement: Integer read FLastIndentIncrement;

    ///	<summary>Contains all the annotations read since last reset. The caller
    ///	is responsible for resetting this list at appropriate times.</summary>
    property LastAnnotations: TStrings read FLastAnnotations;

    ///	<summary>Returns true if the last read value was quoted. Single-line
    ///	values are unquoted when reading; querying this property allows to find
    ///	out if a value was originally quoted (for example, to make sure it is
    ///	quoted back when writing) or not.</summary>
    property LastValueQuoted: Boolean read FLastValueQuoted;
  end;

  ///	<summary>
  ///	  Reads yaml data from files or streams and constructs tree objects.
  ///	</summary>
  TEFYAMLReader = class
  private
    FParser: TEFYAMLParser;
    class var FFormatSettings: TFormatSettings;
    function GetParser: TEFYAMLParser;
  public
    class constructor Create;
    destructor Destroy; override;
  public
    property Parser: TEFYAMLParser read GetParser;

    ///	<summary>
    ///	  Format settings used to parse values.
    ///	</summary>
    class property FormatSettings: TFormatSettings read FFormatSettings write FFormatSettings;
    procedure LoadTreeFromFile(const ATree: TEFTree; const AFileName: string);
    procedure LoadTreeFromStream(const ATree: TEFTree; const AStream: TStream);
    procedure LoadTreeFromString(const ATree: TEFTree; const AString: string);

    class function LoadTree(const AFileName: string): TEFTree; overload;
    class procedure LoadTree(const ATree: TEFTree; const AFileName: string); overload;
    class procedure ReadTree(const ATree: TEFTree; const AString: string);
  end;

  ///	<summary>
  ///	  Writes a tree to a yaml file or stream.
  ///	</summary>
  TEFYAMLWriter = class
  private
    FIndentChars: Integer;
    FSpacingChars: Integer;
    procedure WriteNode(const ANode: TEFNode; const AWriter: TTextWriter;
      const AIndent: Integer);
  public
    procedure AfterConstruction; override;
  public
    procedure SaveTreeToFile(const ATree: TEFTree; const AFileName: string);
    procedure SaveTreeToStream(const ATree: TEFTree; const AStream: TStream);

    class procedure SaveTree(const ATree: TEFTree; const AFileName: string);
    class function TreeAsString(const ATree: TEFTree): string;
  end;

  TEFTreeHelper = class helper for TEFTree
  private
    function GetAsYamlString: string;
    procedure SetAsYamlString(const AValue: string);
  public
    function LoadFromYamlFile(const AFileName: string): TEFTree;
    function SaveToYamlFile(const AFileName: string): TEFTree;
    property AsYamlString: string read GetAsYamlString write SetAsYamlString;
  end;

implementation

uses
  StrUtils,
  EF.Types, EF.StrUtils, EF.SysUtils, EF.Localization;

{ TEFYAMLParser }

procedure TEFYAMLParser.AfterConstruction;
begin
  inherited;
  FIndents := TList<Integer>.Create;
  FLastAnnotations := TStringList.Create;
  Reset;
end;

destructor TEFYAMLParser.Destroy;
begin
  FreeAndNil(FIndents);
  FreeAndNil(FLastAnnotations);
  inherited;
end;

function TEFYAMLParser.ParseLine(const ALine: string; out AName, AValue: string): Boolean;
var
  LLine: string;
  P: Integer;
  LIndent: Integer;

  procedure AddIndent(const AIndent: Integer);
  begin
    if (FIndents.Count = 0) then
      FIndents.Add(AIndent)
    else if (FIndents[FIndents.Count - 1] = AIndent) then
      Exit
    else if (FIndents[FIndents.Count - 1] < AIndent) then
      FIndents.Add(AIndent)
    // top indent > AIndent - check.
    else if FIndents.IndexOf(AIndent) < 0 then
      raise EEFError.CreateFmt('YAML syntax error. Indentation error in line: %s', [ALine]);
  end;

  function FindQuotationEnd(const AString: string): Integer;
  begin
    Result := Pos('":', AString);
    while Result <> 0 do
    begin
      if (Result > 1) and (AString[Result - 1] <> '"') then
        Break
      else
        Result := PosEx('":', AString, Result + 1);
    end;
    if Result = 0 then
      raise EEFError.CreateFmt('YAML syntax error. Bad quoting in line: %s', [ALine]);
  end;

begin
  Result := False;

  FLastValueType := vtSingleLine;
  FLastValueQuoted := False;

  // Handle multi-line values.
  if FNextValueType in [vtMultiLineWithNL, vtMultiLineWithSpace] then
  begin
    LIndent := CountLeading(ALine, ' ');
    // The indentation of the first line in a multi-line value is important
    // because we need to strip exactly that number of spaces from the
    // beginning of all other lines.
    if FMultiLineFirstLineIndent < 0 then
      FMultiLineFirstLineIndent := LIndent;
    // A multi-line value continues as long as lines are indented. Note that in
    // this case we exit WITHOUT updating FPrevIndent.
    if LIndent > FPrevIndent then
    begin
      AName := '';
      AValue := Copy(ALine, FMultiLineFirstLineIndent + 1, MaxInt);
      FLastValueType := FNextValueType;
      Result := True;
      Exit;
    end
    else
    begin
      // Multi-line value finished. Reset variables.
      FLastValueType := vtSingleLine;
      FMultiLineFirstLineIndent := -1;
    end;
  end;

  if Pos(#9, ALine) > 0 then
    raise EEFError.CreateFmt('YAML syntax error. Tab character (#9) not allowed. Use spaces only for indentation. Line: %s', [ALine]);

  LLine := Trim(ALine);
  // Store comments and empty lines as annotations for the next node.
  if (LLine = '') or (LLine[1] = '#') then
  begin
    FLastAnnotations.Add(LLine);
    Exit;
  end;

  P := Pos(':', ALine);
  if P = 0 then
    raise EEFError.CreateFmt('YAML syntax error. Missing ":" in line: %s', [ALine]);

  if Pos('"', LLine) = 1 then // quoted name
  begin
    P := FindQuotationEnd(ALine);
    AName := Copy(ALine, 1, P);
    Inc(P); // Point to the ':' like in the general case.
  end
  else
    AName := Copy(ALine, 1, Pred(P));

  LIndent := CountLeading(AName, ' ');
  AddIndent(LIndent);

  AName := Trim(AnsiDequotedStr(AName, '"'));
  AValue := Trim(Copy(Aline, Succ(P), MaxInt));

  // Watch for special introducers.
  if AValue = '|' then
  begin
    FNextValueType := vtMultiLineWithNL;
  end
  else if AValue = '>' then
  begin
    FNextValueType := vtMultiLineWithSpace
  end
  else
    FNextValueType := vtSingleLine;

  if FNextValueType = vtSingleLine then
  begin
    if (Length(AValue) >= 2) and AnsiStartsStr('"', AValue) and AnsiEndsStr('"', AValue) then
    begin
      FLastValueQuoted := True;
      AValue := AnsiDequotedStr(AValue, '"');
    end;
  end
  else
    AValue := '';
  // Keep track of how many indents we have incremented or decremented.
  // Users of this class will use this information to track nesting.
  FLastIndentIncrement := FIndents.IndexOf(LIndent) - FIndents.IndexOf(FPrevIndent);
  FPrevIndent := LIndent;
  Result := True;
end;

procedure TEFYAMLParser.Reset;
begin
  FIndents.Clear;
  FPrevIndent := 0;
  FMultiLineFirstLineIndent := -1;
  FLastAnnotations.Clear;
  FLastValueQuoted := False;
end;

{ TEFYAMLReader }

class constructor TEFYAMLReader.Create;
begin
  FFormatSettings := GetFormatSettings;
end;

destructor TEFYAMLReader.Destroy;
begin
  FreeAndNil(FParser);
  inherited;
end;

function TEFYAMLReader.GetParser: TEFYAMLParser;
begin
  if not Assigned(FParser) then
    FParser := TEFYAMLParser.Create;
  Result := FParser;
end;

class function TEFYAMLReader.LoadTree(const AFileName: string): TEFTree;
begin
  Result := TEFTree.Create;
  try
    LoadTree(Result, AFileName);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

class procedure TEFYAMLReader.LoadTree(const ATree: TEFTree; const AFileName: string);
var
  LInstance: TEFYAMLReader;
begin
  Assert(AFileName <> '');
  Assert(FileExists(AFileName));
  Assert(Assigned(ATree));

  LInstance := TEFYAMLReader.Create;
  try
    LInstance.LoadTreeFromFile(ATree, AFileName);
  finally
    FreeAndNil(LInstance);
  end;
end;

class procedure TEFYAMLReader.ReadTree(const ATree: TEFTree; const AString: string);
var
  LInstance: TEFYAMLReader;
begin
  Assert(Assigned(ATree));

  LInstance := TEFYAMLReader.Create;
  try
    LInstance.LoadTreeFromString(ATree, AString);
  finally
    FreeAndNil(LInstance);
  end;
end;

procedure TEFYAMLReader.LoadTreeFromString(const ATree: TEFTree;
  const AString: string);
var
  LStream: TStringStream;
begin
  LStream := TStringStream.Create(AString);
  try
    LoadTreeFromStream(ATree, LStream);
  finally
    FreeAndNil(LStream);
  end;
end;

procedure TEFYAMLReader.LoadTreeFromFile(const ATree: TEFTree; const AFileName: string);
var
  LFileStream: TFileStream;
begin
  Assert(Assigned(ATree));

  LFileStream := TFileStream.Create(AFileName, fmOpenRead + fmShareDenyWrite);
  try
    try
      LoadTreeFromStream(ATree, LFileStream);
    except
      on E: Exception do
        raise Exception.CreateFmt(_('Error %s while loading file %s.'),
          [E.Message, AFileName]);
    end;
  finally
    FreeAndNil(LFileStream);
  end;
end;

procedure TEFYAMLReader.LoadTreeFromStream(const ATree: TEFTree; const AStream: TStream);
var
  LStack: TStack<TEFNode>;
  LLine: string;
  LName, LRawValue: string;
  LTop: TEFTree;
  LReader: TStreamReader;
  LCurrentValue: string;
  LNewNode: TEFNode;

  procedure TryPopFromStack(const AAmount: Integer);
  var
    I: Integer;
  begin
    for I := 0 to AAmount do
      if LStack.Count > 0 then
        LStack.Pop;
  end;

begin
  Assert(Assigned(ATree));

  LReader := TStreamReader.Create(AStream);
  try
    LStack := TStack<TEFNode>.Create;
    try
      ATree.ClearChildren;
      Parser.Reset;

      repeat
        LLine := LReader.ReadLine;
        if Parser.ParseLine(LLine, LName, LRawValue) then
        begin
          if Parser.LastValueType = vtSingleLine  then
            TryPopFromStack(-Parser.LastIndentIncrement);
          if LStack.Count = 0 then
          begin
            LTop := ATree;
            if ATree is TEFNode then
            begin
              TEFNode(ATree).Name := LName;
              TEFDataType.SetNodeDataTypeAndValueFromYaml(LRawValue, TEFNode(ATree), FFormatSettings);
              LStack.Push(TEFNode(ATree));
              Continue;
            end;
          end
          else
            LTop := LStack.Peek;
          case Parser.LastValueType of
            vtSingleLine:
            begin
              LNewNode := LTop.AddChild(LName);
              TEFDataType.SetNodeDataTypeAndValueFromYaml(LRawValue, LNewNode, FFormatSettings);
              LNewNode.AssignAnnotations(Parser.LastAnnotations);
              if Parser.LastValueQuoted then
                LNewNode.ValueAttributes := '"'
              else
                LNewNode.ValueAttributes := '';
              Parser.LastAnnotations.Clear;
              LStack.Push(LNewNode);
            end;
            vtMultiLineWithNL:
            begin
              LCurrentValue := (LTop as TEFNode).AsString;
              if LCurrentValue = '' then
                LCurrentValue := LRawValue
              else
                LCurrentValue := LCurrentValue + sLineBreak + LRawValue;
              (LTop as TEFNode).AsString := LCurrentValue;
              (LTop as TEFNode).ValueAttributes := '|';
            end;
            vtMultiLineWithSpace:
            begin
              LCurrentValue := (LTop as TEFNode).AsString;
              // When not preserving line breaks, empty lines mark paragraphs.
              if LRawValue = '' then
                LCurrentValue := LCurrentValue + sLineBreak
              else if LCurrentValue = '' then
                LCurrentValue := LRawValue
              else
                LCurrentValue := LCurrentValue + ' ' + LRawValue;
              (LTop as TEFNode).AsString := LCurrentValue;
              (LTop as TEFNode).ValueAttributes := '>';
            end;
          end;
        end;
      until LReader.EndOfStream;
    finally
      FreeAndNil(LStack);
    end;
  finally
    FreeAndNil(LReader);
  end;
end;

{ TEFYAMLWriter }

procedure TEFYAMLWriter.AfterConstruction;
begin
  inherited;
  FIndentChars := 2;
  FSpacingChars := 1;
end;

class procedure TEFYAMLWriter.SaveTree(const ATree: TEFTree;
  const AFileName: string);
var
  LWriter: TEFYAMLWriter;
begin
  Assert(Assigned(ATree));
  Assert(AFileName <> '');

  LWriter := TEFYAMLWriter.Create;
  try
    LWriter.SaveTreeToFile(ATree, AFileName);
  finally
    FreeAndNil(LWriter);
  end;
end;

class function TEFYAMLWriter.TreeAsString(const ATree: TEFTree): string;
var
  LWriter: TEFYAMLWriter;
  LStream: TBytesStream;
  LBytes: TBytes;
  LPreambleLength: Integer;
  LEncoding: TEncoding;
begin
  Assert(Assigned(ATree));

  LWriter := TEFYAMLWriter.Create;
  try
    // SaveTreeToStream writes the encoding preamble, which is not a problem
    // when writing to a file, but we want to avoid that when getting yaml
    // code as a string.
    LStream := TBytesStream.Create;
    try
      LEncoding := TEncoding.UTF8;
      LWriter.SaveTreeToStream(ATree, LStream);
      LPreambleLength := Length(LEncoding.GetPreamble);
      SetLength(LBytes, LStream.Size - LPreambleLength);
      LStream.Position := LPreambleLength;
      LStream.Read(LBytes[0], Length(LBytes));
      Result := LEncoding.GetString(LBytes);
    finally
      FreeAndNil(LStream);
    end;
  finally
    FreeAndNil(LWriter);
  end;
end;

procedure TEFYAMLWriter.SaveTreeToFile(const ATree: TEFTree; const AFileName: string);
var
  LFileStream: TFileStream;
begin
  Assert(Assigned(ATree));

  LFileStream := TFileStream.Create(AFileName, fmCreate + fmShareExclusive);
  try
    SaveTreeToStream(ATree, LFileStream);
  finally
    FreeAndNil(LFileStream);
  end;
end;

procedure TEFYAMLWriter.SaveTreeToStream(const ATree: TEFTree; const AStream: TStream);
var
  LWriter: TStreamWriter;
  I: Integer;
  LIndent: Integer;
begin
  Assert(Assigned(ATree));

  LIndent := 0;
  LWriter := TStreamWriter.Create(AStream, TEncoding.UTF8);
  try
    ATree.BeforeSave;
    if ATree is TEFNode then
      WriteNode(TEFNode(ATree), LWriter, LIndent)
    else
      for I := 0 to ATree.ChildCount - 1 do
        WriteNode(ATree.Children[I], LWriter, LIndent);
    LWriter.Flush;
  finally
    FreeAndNil(LWriter);
  end;
end;

procedure TEFYAMLWriter.WriteNode(const ANode: TEFNode;
  const AWriter: TTextWriter; const AIndent: Integer);
var
  I: Integer;
  LValue: string;
  LStrings: TStringList;
  LName: string;
begin
  Assert(Assigned(ANode));

  for I := 0 to ANode.AnnotationCount - 1 do
  begin
    LValue := ANode.Annotations[I];
    if LValue = '' then
      AWriter.WriteLine
    else
      AWriter.WriteLine(StringOfChar(' ', AIndent) + LValue);
  end;

  if Pos(':', ANode.Name) <> 0 then
    LName := AnsiQuotedStr(ANode.Name, '"')
  else
    LName := ANode.Name;
  AWriter.Write(StringOfChar(' ', AIndent) + LName + ':');
  { TODO : format value }
  LValue := ANode.AsString;

  if LValue <> '' then
  begin
    if ANode.IsMultiLineValue then
    begin
      LStrings := TStringList.Create;
      try
        if ANode.IsMultiLineWithNLValue then
        begin
          LStrings.Text := LValue;
          AWriter.WriteLine(StringOfChar(' ', FSpacingChars) + '|');
        end
        else
        begin
          LStrings.Text := WrapText(LValue);
          AWriter.WriteLine(StringOfChar(' ', FSpacingChars) + '>');
        end;
        for I := 0 to LStrings.Count - 1 do
          AWriter.WriteLine(StringOfChar(' ', AIndent + FIndentChars) + LStrings[I]);
      finally
        FreeAndNil(LStrings);
      end;
    end
    else if ContainsStr(ANode.ValueAttributes, '"') then
      AWriter.WriteLine(StringOfChar(' ', FSpacingChars) + AnsiQuotedStr(LValue, '"'))
    else
      AWriter.WriteLine(StringOfChar(' ', FSpacingChars) + LValue);
  end
  else
    AWriter.WriteLine;
  for I := 0 to ANode.ChildCount - 1 do
    WriteNode(ANode.Children[I], AWriter, AIndent + FIndentChars);
end;

{ TEFTreeHelper }

function TEFTreeHelper.LoadFromYamlFile(const AFileName: string): TEFTree;
begin
  TEFYAMLReader.LoadTree(Self, AFileName);
  Result := Self;
end;

function TEFTreeHelper.SaveToYamlFile(const AFileName: string): TEFTree;
begin
  TEFYAMLWriter.SaveTree(Self, AFileName);
  Result := Self;
end;

function TEFTreeHelper.GetAsYamlString: string;
begin
  Result := TEFYAMLWriter.TreeAsString(Self);
end;

procedure TEFTreeHelper.SetAsYamlString(const AValue: string);
begin
  TEFYAMLReader.ReadTree(Self, AValue);
end;

end.
