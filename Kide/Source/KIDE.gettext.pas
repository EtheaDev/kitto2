{-------------------------------------------------------------------------------
   Copyright 2012-2017 Ethea S.r.l.

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
unit KIDE.gettext;

interface

uses
  Classes,
  EF.Tree;

type
  TPOFile = class(TStringList)
  private
    FDefaultKeyNames: TStringList;
    FFileName: string;
    FIsChanged: Boolean;
    FCurrentReference: string;
    function FormatString(const AString: string): string;
    function FormatTranslation(const ATranslation: string): string;
    procedure UpdateFromNode(const ANode: TEFNode);
    function IsTranslatableNode(const ANode: TEFNode): Boolean;
  public
    procedure AfterConstruction; override;
    procedure AddMissingString(const AString, AReference: string);
    constructor Load(const AFileName: string);
    procedure Reload;
    procedure Save;
    property FileName: string read FFileName;
    property IsChanged: Boolean read FIsChanged;
    procedure UpdateFromTree(const ATree: TEFTree; const AReference: string);
  end;

function GetdxgettextDirectory: string;

procedure ExtractdxTranslatableStrings(const ADirectory, AOutputFileName: string;
  const AOutput: TStrings);

procedure MergePOFiles(const AInputFileName, AOutputFileName: string;
  const AOutput: TStrings);

function IsEnclosed(const AString: string): Boolean;
function StripEnclosure(const AString: string): string;

implementation

uses
  SysUtils, StrUtils,
  EF.Localization, EF.StrUtils, EF.Sys.Windows,
  Kitto.Config;

function GetdxgettextDirectory: string;
begin
  Result := TKConfig.Instance.Config.GetExpandedString('Localization/dxgettext/Path');
  if (Result = '') or not DirectoryExists(Result) then
    Result := IncludeTrailingPathDelimiter(GetProgramFilesx86Directory) + 'dxgettext';
  if not DirectoryExists(Result) then
    Result := IncludeTrailingPathDelimiter(GetProgramFilesDirectory) + 'dxgettext';
  if not DirectoryExists(Result) then
    raise Exception.CreateFmt(_('Couldn''t find dxgettext installation. Please install dxgettext from %s.'),
      [TKConfig.Instance.Config.GetExpandedString('Localization/dxgettext/URL')]);
  if not FileExists(IncludeTrailingPathDelimiter(Result) + 'dxgettext.exe') then
    raise Exception.CreateFmt(_('Couldn''t find dxgettext.exe. dxgettext installation might be broken. Please install again from %s.'),
      [TKConfig.Instance.Config.GetExpandedString('Localization/dxgettext/URL')]);
{ TODO : offer to locate or download it }
end;

procedure ExtractdxTranslatableStrings(const ADirectory, AOutputFileName: string;
  const AOutput: TStrings);
var
  LCmdLine: string;
begin
  //Exit;
  LCmdLine := '"'+IncludeTrailingPathDelimiter(GetdxgettextDirectory) + 'dxgettext.exe" -q -b "' +
    ADirectory + '" --delphi --so "' + AOutputFileName + '" --nonascii';
  ExecuteApplication(LCmdLine, AOutput);
end;

procedure MergePOFiles(const AInputFileName, AOutputFileName: string;
  const AOutput: TStrings);
var
  LCmdLine: string;
begin
  //Exit;
  LCmdLine := '"'+IncludeTrailingPathDelimiter(GetdxgettextDirectory) + 'msgcat.exe" "' +
    AInputFileName + '" "' + AOutputFileName + '" -o "' + AOutputFileName + '" --no-wrap';
  ExecuteApplication(LCmdLine, AOutput);
end;

function IsEnclosed(const AString: string): Boolean;
begin
  Result := StartsText('_(', AString) and EndsText(')', AString);
end;

function StripEnclosure(const AString: string): string;
begin
  Result := StripPrefixAndSuffix(AString, '_(', ')');
end;

{ TPOFile }

procedure TPOFile.AfterConstruction;
begin
  inherited;
  WriteBOM := False;
  DefaultEncoding := TEncoding.UTF8;
  FDefaultKeyNames := TStringList.Create;
  FDefaultKeyNames.CaseSensitive := False;
  FDefaultKeyNames.Sorted := True;
  FDefaultKeyNames.Duplicates := dupIgnore;
  TKConfig.Instance.Config.GetChildrenAsStrings(
    'Localization/Yaml/DefaultKeyNames', FDefaultKeyNames);
end;

function TPOFile.FormatString(const AString: string): string;
begin
  Result := Format('msgid "%s"', [AString]);
end;

function TPOFile.FormatTranslation(const ATranslation: string): string;
begin
  Result := Format('msgstr "%s"', [ATranslation]);
end;

constructor TPOFile.Load(const AFileName: string);
begin
  Create;
  FIsChanged := False;
  FFileName := AFileName;
  Reload;
end;

procedure TPOFile.Reload;
begin
  if FileExists(FFileName) then
    LoadFromFile(FFileName, TEncoding.UTF8);
end;

procedure TPOFile.Save;
begin
  SaveToFile(FFileName, TEncoding.UTF8);
end;

procedure TPOFile.UpdateFromTree(const ATree: TEFTree; const AReference: string);
var
  I: Integer;
begin
  Assert(Assigned(ATree));

  FCurrentReference := AReference;
  for I := 0 to ATree.ChildCount - 1 do
    UpdateFromNode(ATree.Children[I]);
end;

function TPOFile.IsTranslatableNode(const ANode: TEFNode): Boolean;
begin
  Assert(Assigned(ANode));

  Result := (FDefaultKeyNames.IndexOf(ANode.Name) >= 0) or IsEnclosed(ANode.AsString);
end;

procedure TPOFile.UpdateFromNode(const ANode: TEFNode);
var
  I: Integer;
begin
  Assert(Assigned(ANode));

  if IsTranslatableNode(ANode) then
    AddMissingString(StripEnclosure(ANode.AsString), FCurrentReference);

  for I := 0 to ANode.ChildCount - 1 do
    UpdateFromNode(ANode.Children[I]);
end;

procedure TPOFile.AddMissingString(const AString, AReference: string);
var
  I: Integer;
  LStringLine: string;
begin
  LStringLine := FormatString(AString);
  I := Pos(LStringLine,Self.Text);
  if I <= 0 then
  begin
    I := Pos(FormatString(StringReplace(AString, '"', '\"', [rfReplaceAll, rfIgnoreCase])),Self.Text);  //verify case with string with " saved in .po file as \"
    if I <= 0 then
    begin
      // Add empty line only if needed.
      if (Count > 0) and (Strings[Count - 1] <> '') then
        Add('');
      Add('#: ' + AReference);
      Add(FormatString(AString));
      Add(FormatTranslation(''));
      FIsChanged := True;
    end;
  end;
end;

end.
