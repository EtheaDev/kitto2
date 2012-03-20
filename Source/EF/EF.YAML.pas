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
  private
    FLastAnnotations: TStrings;
    FIndents: TList<Integer>;
    FPrevIndent: Integer;
    FLastValueType: TEFYAMLValueType;
    FLastIndentIncrement: Integer;
    FNextValueType: TEFYAMLValueType;
    FMultiLineFirstLineIndent: Integer;
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
    destructor Destroy; override;
  public
    property Parser: TEFYAMLParser read GetParser;

    ///	<summary>
    ///	  Format settings used to parse values.
    ///	</summary>
    class property FormatSettings: TFormatSettings read FFormatSettings write FFormatSettings;
    procedure LoadTreeFromFile(const ATree: TEFTree; const AFileName: string);
    procedure LoadTreeFromStream(const ATree: TEFTree; const AStream: TStream);

    class function LoadTree(const AFileName: string): TEFTree;
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
  end;

function EncodeYAMLKey(const AKey: string): string;

implementation

uses
  StrUtils,
  EF.Types, EF.StrUtils, EF.Localization;

function EncodeYAMLKey(const AKey: string): string;
begin
  Result := ReplaceStr(AKey, ':', '§');
end;

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
      raise EEFError.CreateFmt('YAML syntax error. Indentation error in ine: %s', [ALine]);
  end;

begin
  Result := False;

  FLastValueType := vtSingleLine;
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

  AName := Copy(ALine, 1, Pred(P));
  LIndent := CountLeading(AName, ' ');
  AddIndent(LIndent);

  AName := Trim(AName);
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
    AValue := StripPrefixAndSuffix(AValue, '"', '"')
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
end;

{ TEFYAMLReader }

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
var
  LInstance: TEFYAMLReader;
begin
  LInstance := TEFYAMLReader.Create;
  try
    Result := TEFTree.Create;
    try
      LInstance.LoadTreeFromFile(Result, AFileName);
    except
      FreeAndNil(Result);
      raise;
    end;
  finally
    FreeAndNil(LInstance);
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
      ATree.Clear;
      Parser.Reset;

      repeat
        LLine := LReader.ReadLine;
        if Parser.ParseLine(LLine, LName, LRawValue) then
        begin
          if Parser.LastValueType = vtSingleLine  then
            TryPopFromStack(-Parser.LastIndentIncrement);
          if LStack.Count = 0 then
            LTop := ATree
          else
            LTop := LStack.Peek;
          case Parser.LastValueType of
            vtSingleLine:
            begin
              LNewNode := LTop.AddChild(LName).SetAsYamlValue(LRawValue, FFormatSettings);
              LNewNode.AssignAnnotations(Parser.LastAnnotations);
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

  AWriter.Write(StringOfChar(' ', AIndent) + ANode.Name + ':');
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
    else
      AWriter.WriteLine(StringOfChar(' ', FSpacingChars) + LValue);
  end
  else
    AWriter.WriteLine;
  for I := 0 to ANode.ChildCount - 1 do
    WriteNode(ANode.Children[I], AWriter, AIndent + FIndentChars);
end;

end.
