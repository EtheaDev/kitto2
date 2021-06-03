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
unit KIDE.CodeEditorFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, KIDE.BaseFrameUnit, Vcl.ComCtrls,
  Vcl.ToolWin, Vcl.ActnList, SynEdit,
  SynHighlighterYAML, SynHighlighterHTML, SynHighlighterCSS, SynHighlighterJScript, SynEditHighlighter,
  Vcl.ImgList, Vcl.StdCtrls, Vcl.ExtCtrls, SynEditOptionsDialog, SynEditRegexSearch, System.Actions,
  SynEditMiscClasses, SynEditSearch, SynEditPrint, FSynHighlightProp, SynEditTypes,
  dlgReplaceText, dlgSearchText, dlgConfirmReplace;

type
  TCodeEditorFrame = class(TBaseFrame)
    ActionList: TActionList;
    ToolBar: TToolBar;
    ApplyActionToolButton: TToolButton;
    ErrorLabel: TLabel;
    Sep1ToolButton: TToolButton;
    UndoActionToolButton: TToolButton;
    RedoActionToolButton: TToolButton;
    UndoAction: TAction;
    RedoAction: TAction;
    Sep2ToolButton: TToolButton;
    OptionsActionToolButton: TToolButton;
    OptionsAction: TAction;
    ApplyAction: TAction;
    CancelAction: TAction;
    CancelActionToolButton: TToolButton;
    EditorColorOptionsAction: TAction;
    EditorColorOptionsButton: TToolButton;
    SearchAction: TAction;
    SearchAgainAction: TAction;
    SearchAndReplaceAction: TAction;
    SearchToolButton: TToolButton;
    SearchAgainToolButton: TToolButton;
    SearchAndReplaceToolButton: TToolButton;
    Sep3ToolButton: TToolButton;
    procedure ApplyActionUpdate(Sender: TObject);
    procedure UndoActionExecute(Sender: TObject);
    procedure RedoActionExecute(Sender: TObject);
    procedure UndoActionUpdate(Sender: TObject);
    procedure RedoActionUpdate(Sender: TObject);
    procedure OptionsActionExecute(Sender: TObject);
    procedure ApplyActionExecute(Sender: TObject);
    procedure CancelActionUpdate(Sender: TObject);
    procedure CancelActionExecute(Sender: TObject);
    procedure EditorColorOptionsActionExecute(Sender: TObject);
    procedure SearchAndReplaceActionExecute(Sender: TObject);
    procedure SearchAndReplaceActionUpdate(Sender: TObject);
    procedure SearchActionExecute(Sender: TObject);
    procedure SearchActionUpdate(Sender: TObject);
    procedure SearchAgainActionExecute(Sender: TObject);
    procedure SearchAgainActionUpdate(Sender: TObject);
  private
    FEditorReadOnly: Boolean;
    FOnApply: TNotifyEvent;
    FOriginalCode: string;
    FOnCancel: TNotifyEvent;
    FEdit: TSynEdit;
    FSynHighlighter: TSynCustomHighlighter;
    FSynPrint: TSynEditPrint;
    FSynSearch: TSynEditSearch;
    FSynRegexSearch: TSynEditRegexSearch;
    FSynOptionsDialog: TSynEditOptionsDialog;
    FgsSearchTextHistory: string;
    FgsReplaceTextHistory: string;
    FgsReplaceText: string;
    FgbSearchFromCaret: Boolean;
    FgsSearchText: string;
    FgbSearchBackwards: Boolean;
    FgbSearchCaseSensitive: Boolean;
    FgbSearchSelectionOnly: Boolean;
    FgbSearchTextAtCaret: Boolean;
    FgbSearchWholeWords: Boolean;
    procedure EditChange(Sender: TObject);
    function GetCode: string;
    procedure SetCode(const AValue: string);
    procedure Validate;
    procedure HideError;
    procedure ShowError(const AMessage: string);
    procedure StoreOptions(const AOptions: TSynEditorOptionsContainer);
    procedure RestoreOptions;
    function PendingChanges: Boolean;
    procedure DoSearchReplaceText(const AReplace, ABackwards: Boolean);
    procedure ShowSearchReplaceDialog(const AReplace: Boolean);
    procedure InitHighlighter(const AExt: string);
    procedure SetEditorReadOnly(const Value: Boolean);
  public
    procedure AfterConstruction; override;

    ///	<summary>Assigns thed edit text. The user is prompted to save any
    ///	pending changes.</summary>
    property Code: string read GetCode write SetCode;

    ///	<summary>Assigns Code silently discarding any pending
    ///	changes.</summary>
    procedure RefreshCode(const ACode: string);

    ///	<summary>Fires OnApply. Also called by the ApplyAction.</summary>
    procedure Apply;

    ///	<summary>Fired to save edits. The handler should read the Code and
    ///	store it somewhere.</summary>
    property OnApply: TNotifyEvent read FOnApply write FOnApply;


    ///	<summary>Fires OnCancel. Also called by the CancelAction.</summary>
    procedure Cancel;

    ///	<summary>Load a file into editor</summary>
    procedure LoadFromFile(const AFileName: string; const AEncoding: TEncoding);

    ///	<summary>Save a file with editor content</summary>
    procedure SaveToFile(const AFileName: string; const AEncoding: TEncoding);

    ///	<summary>Fired to discard all edits. The handler should re-assign the
    ///	original text to Code (by calling RefreshCode in order to bypass any
    ///	pending changes dialog box).</summary>
    property OnCancel: TNotifyEvent read FOnCancel write FOnCancel;

    ///	<summary>Property to set ReadOnly of the internal editor</summary>
    property EditorReadOnly: Boolean read FEditorReadOnly write SetEditorReadOnly;
  end;

implementation

{$R *.dfm}

uses
  System.UITypes,
  EF.StrUtils, EF.Macros, EF.Tree, EF.YAML, EF.Rtti,
  KIDE.Utils, KIDE.MRUOptions, KIDE.MainDataModuleUnit;

{ TCodeEditorFrame }

procedure TCodeEditorFrame.InitHighlighter(const AExt: string);
begin
  FreeAndNil(FSynHighlighter);
  if SameText(AExt, '.yaml') then
  begin
    FSynHighlighter := TSynYAMLSyn.Create(Self);
    TSynYAMLSyn(FSynHighlighter).KeyAttri.Foreground := clBlue;
    TSynYAMLSyn(FSynHighlighter).SymbolAttri.Foreground := clGreen;
    TSynYAMLSyn(FSynHighlighter).StringAttri.Foreground := clTeal;
    HelpContext := 2000;
  end
  else if SameText(AExt, '.css') then
  begin
    FSynHighlighter := TSynCssSyn.Create(Self);
    TSynCssSyn(FSynHighlighter).KeywordAttribute.Foreground := clBlue;
    TSynCssSyn(FSynHighlighter).SymbolAttri.Foreground := clGreen;
    TSynCssSyn(FSynHighlighter).StringAttri.Foreground := clTeal;
    HelpContext := 150;
  end
  else if MatchStr(AExt, ['.js', '.json']) then
  begin
    FSynHighlighter := TSynJScriptSyn.Create(Self);
    TSynJScriptSyn(FSynHighlighter).KeyAttri.Foreground := clBlue;
    TSynJScriptSyn(FSynHighlighter).SymbolAttri.Foreground := clGreen;
    TSynJScriptSyn(FSynHighlighter).StringAttri.Foreground := clTeal;
    HelpContext := 150;
  end
  else if MatchStr(AExt, ['.html', '.htm']) then
  begin
    FSynHighlighter := TSynHTMLSyn.Create(Self);
    TSynHTMLSyn(FSynHighlighter).KeyAttri.Foreground := clBlue;
    TSynHTMLSyn(FSynHighlighter).SymbolAttribute.Foreground := clGreen;
    HelpContext := 150;
  end
  else
    FSynHighlighter := nil;
  if Assigned(FEdit) then
    FEdit.Highlighter := FSynHighlighter;
end;

procedure TCodeEditorFrame.AfterConstruction;
begin
  inherited;
  InitHighlighter('.yaml');

  FEdit := CreateSynEditor(Self, Self, 'CodeEditor', FSynHighlighter);
  FEdit.OnChange := EditChange;
  fEdit.ReadOnly := EditorReadOnly;

  FSynPrint := TSynEditPrint.Create(Self);
  FSynPrint.Copies := 1;
  FSynPrint.Header.DefaultFont.Charset := DEFAULT_CHARSET;
  FSynPrint.Header.DefaultFont.Color := clBlack;
  FSynPrint.Header.DefaultFont.Height := -13;
  FSynPrint.Header.DefaultFont.Name := 'Arial';
  FSynPrint.Header.DefaultFont.Style := [];
  FSynPrint.Footer.DefaultFont.Charset := DEFAULT_CHARSET;
  FSynPrint.Footer.DefaultFont.Color := clBlack;
  FSynPrint.Footer.DefaultFont.Height := -13;
  FSynPrint.Footer.DefaultFont.Name := 'Arial';
  FSynPrint.Footer.DefaultFont.Style := [];
  FSynPrint.Margins.Left := 25.000000000000000000;
  FSynPrint.Margins.Right := 15.000000000000000000;
  FSynPrint.Margins.Top := 25.000000000000000000;
  FSynPrint.Margins.Bottom := 25.000000000000000000;
  FSynPrint.Margins.Header := 15.000000000000000000;
  FSynPrint.Margins.Footer := 15.000000000000000000;
  FSynPrint.Margins.LeftHFTextIndent := 2.000000000000000000;
  FSynPrint.Margins.RightHFTextIndent := 2.000000000000000000;
  FSynPrint.Margins.HFInternalMargin := 0.500000000000000000;
  FSynPrint.Margins.MirrorMargins := False;
  FSynPrint.Font.Charset := DEFAULT_CHARSET;
  FSynPrint.Font.Color := clWindowText;
  FSynPrint.Font.Height := -11;
  FSynPrint.Font.Name := 'Tahoma';
  FSynPrint.Font.Style := [];
  FSynPrint.TabWidth := 8;
  FSynPrint.Color := clWhite;

  FSynSearch := TSynEditSearch.Create(Self);
  FEdit.SearchEngine := FSynSearch;

  FSynRegexSearch := TSynEditRegexSearch.Create(Self);

  FSynOptionsDialog := TSynEditOptionsDialog.Create(Self);
  FSynOptionsDialog.UseExtendedStrings := True;

  ErrorLabel.Height := 0;
  RestoreOptions;
end;

procedure TCodeEditorFrame.EditChange(Sender: TObject);
begin
  inherited;
  HideError;
end;

procedure TCodeEditorFrame.EditorColorOptionsActionExecute(Sender: TObject);
var
  LHighLighterSettingsFileName: string;
begin
  inherited;
  LHighLighterSettingsFileName := GetHighLighterSettingsFileName(FEdit);
  ShowHighLighterSettings(FEdit, LHighLighterSettingsFileName);
end;

function TCodeEditorFrame.GetCode: string;
begin
  Result := FEdit.Text;
end;

procedure TCodeEditorFrame.Validate;
var
  LNode: TEFNode;
begin
  if FEdit.Highlighter = FSynHighlighter then
  begin
    LNode := TEFNode.Create;
    try
      try
        LNode.AsYamlString := Code;
        HideError;
      except
        on E: Exception do
          ShowError(E.Message);
      end;
    finally
      FreeAndNil(LNode);
    end;
  end;
end;

procedure TCodeEditorFrame.Apply;
begin
  Validate;
  if Assigned(FOnApply) then
    FOnApply(Self);
end;

procedure TCodeEditorFrame.ShowError(const AMessage: string);
var
  I: Integer;
begin
  ErrorLabel.Caption := AMessage;
  ErrorLabel.Height := 0;
  ErrorLabel.Visible := True;
  for I := ErrorLabel.Height to 16 do
  begin
    ErrorLabel.Height := I;
    Sleep(20);
  end;
end;

procedure TCodeEditorFrame.UndoActionExecute(Sender: TObject);
begin
  inherited;
  FEdit.Undo;
end;

procedure TCodeEditorFrame.UndoActionUpdate(Sender: TObject);
begin
  inherited;
  (Sender as TAction).Enabled := FEdit.CanUndo;
end;

procedure TCodeEditorFrame.HideError;
var
  I: Integer;
begin
  for I := ErrorLabel.Height downto 0 do
  begin
    ErrorLabel.Height := I;
    Sleep(20);
  end;
  ErrorLabel.Visible := False;
end;

procedure TCodeEditorFrame.LoadFromFile(const AFileName: string; const AEncoding: TEncoding);
begin
  InitHighlighter(ExtractFileExt(AFileName));
  FEdit.Lines.LoadFromFile(AFileName, AEncoding);
end;

procedure TCodeEditorFrame.OptionsActionExecute(Sender: TObject);
var
  LOptions: TSynEditorOptionsContainer;
begin
  inherited;
  LOptions := TSynEditorOptionsContainer.Create(nil);
  try
    LOptions.Assign(FEdit);
    if FSynOptionsDialog.Execute(LOptions) then
    begin
      FEdit.Assign(LOptions);
      StoreOptions(LOptions);
    end;
  finally
    FreeAndNil(LOptions);
  end;
end;

procedure TCodeEditorFrame.StoreOptions(const AOptions: TSynEditorOptionsContainer);
var
  LNode: TEFNode;
begin
  LNode := TMRUOptions.Instance.GetNode('CodeEditor/Options/Yaml', True);
  ObjectToNode(AOptions, LNode);
  TMRUOptions.Instance.Save;
end;

procedure TCodeEditorFrame.RedoActionExecute(Sender: TObject);
begin
  inherited;
  FEdit.Redo;
end;

procedure TCodeEditorFrame.RedoActionUpdate(Sender: TObject);
begin
  inherited;
  (Sender as TAction).Enabled := FEdit.CanRedo;
end;

procedure TCodeEditorFrame.RestoreOptions;
begin
  SynEditorRestoreOptions(FEdit);
end;

procedure TCodeEditorFrame.ApplyActionExecute(Sender: TObject);
begin
  inherited;
  Apply;
end;

procedure TCodeEditorFrame.ApplyActionUpdate(Sender: TObject);
begin
  inherited;
  (Sender as TAction).Enabled := PendingChanges and Assigned(FOnApply);
end;

function TCodeEditorFrame.PendingChanges: Boolean;
begin
  Result := (FOriginalCode <> '') and (FOriginalCode <> FEdit.Text);
end;

procedure TCodeEditorFrame.Cancel;
begin
  if Assigned(FOnCancel) then
    FOnCancel(Self);
end;

procedure TCodeEditorFrame.CancelActionExecute(Sender: TObject);
begin
  inherited;
  Cancel;
end;

procedure TCodeEditorFrame.CancelActionUpdate(Sender: TObject);
begin
  inherited;
  (Sender as TAction).Enabled := (FOriginalCode <> FEdit.Text) and Assigned(FOnCancel);
end;

procedure TCodeEditorFrame.SetCode(const AValue: string);
begin
  if PendingChanges and (MessageDlg('Pending changes in code editor. Discard?', mtWarning,
    [mbYes, mbNo, mbCancel], 0) <> mrYes) then
    Abort;
  RefreshCode(AValue);
end;

procedure TCodeEditorFrame.SetEditorReadOnly(const Value: Boolean);
begin
  if Assigned(FEdit) then
    FEdit.ReadOnly := Value;
  FEditorReadOnly := Value;
end;

procedure TCodeEditorFrame.RefreshCode(const ACode: string);
begin
  FOriginalCode := ACode;
  FEdit.Text := ACode;
end;

procedure TCodeEditorFrame.SearchAgainActionExecute(Sender: TObject);
begin
  FgbSearchFromCaret := True;
  DoSearchReplaceText(False, FgbSearchBackwards);
end;

procedure TCodeEditorFrame.SearchAgainActionUpdate(Sender: TObject);
begin
  SearchAgainAction.Enabled := FgsSearchText <> '';
end;

procedure TCodeEditorFrame.SearchActionUpdate(Sender: TObject);
begin
  SearchAction.Enabled := (FEdit <> nil) and (FEdit.Text <> '');
end;

procedure TCodeEditorFrame.SearchAndReplaceActionUpdate(Sender: TObject);
begin
  SearchAndReplaceAction.Enabled := (FEdit <> nil) and (FEdit.Text <> '');
end;

procedure TCodeEditorFrame.SaveToFile(const AFileName: string; const AEncoding: TEncoding);
begin
  FEdit.Lines.SaveToFile(AFileName, AEncoding);
end;

procedure TCodeEditorFrame.SearchActionExecute(Sender: TObject);
begin
  ShowSearchReplaceDialog(false);
end;

procedure TCodeEditorFrame.SearchAndReplaceActionExecute(Sender: TObject);
begin
  ShowSearchReplaceDialog(true);
end;

procedure TCodeEditorFrame.ShowSearchReplaceDialog(const AReplace: Boolean);
var
  LSearchReplaceDialog: TTextSearchDialog;
  LNode: TEFNode;
begin
  if AReplace then
  begin
    LSearchReplaceDialog := TTextReplaceDialog.Create(Self);
    LNode := TMRUOptions.Instance.GetNode('CodeEditor/SearchReplace', True);
  end
  else
  begin
    LSearchReplaceDialog := TTextSearchDialog.Create(Self);
    LNode := TMRUOptions.Instance.GetNode('CodeEditor/Search', True);
  end;

  with LSearchReplaceDialog do
  try
    //Restore position
    SetBounds(
      LNode.GetInteger('Left',100),
      LNode.GetInteger('Top',100),
      LSearchReplaceDialog.Width,
      LSearchReplaceDialog.Height);

    // assign search options
    SearchBackwards := FgbSearchBackwards;
    SearchCaseSensitive := FgbSearchCaseSensitive;
    SearchFromCursor := FgbSearchFromCaret;
    SearchInSelectionOnly := FgbSearchSelectionOnly;
    // start with last search text
    SearchText := FgsSearchText;
    if FgbSearchTextAtCaret then
    begin
      // if something is selected search for that text
      if FEdit.SelAvail and (FEdit.BlockBegin.Line = FEdit.BlockEnd.Line) then
        SearchText := FEdit.SelText
      else
        SearchText := FEdit.GetWordAtRowCol(FEdit.CaretXY);
    end;
    SearchTextHistory := FgsSearchTextHistory;
    if AReplace then
      with LSearchReplaceDialog as TTextReplaceDialog do
      begin
        ReplaceText := FgsReplaceText;
        ReplaceTextHistory := FgsReplaceTextHistory;
      end;
    SearchWholeWords := FgbSearchWholeWords;
    if ShowModal = mrOK then
    begin
      FgbSearchBackwards := SearchBackwards;
      FgbSearchCaseSensitive := SearchCaseSensitive;
      FgbSearchFromCaret := SearchFromCursor;
      FgbSearchSelectionOnly := SearchInSelectionOnly;
      FgbSearchWholeWords := SearchWholeWords;
      FgsSearchText := SearchText;
      FgsSearchTextHistory := SearchTextHistory;
      if AReplace then
        with LSearchReplaceDialog as TTextReplaceDialog do
        begin
          FgsReplaceText := ReplaceText;
          FgsReplaceTextHistory := ReplaceTextHistory;
        end;
      FgbSearchFromCaret := FgbSearchFromCaret;
      if FgsSearchText <> '' then
      begin
        DoSearchReplaceText(AReplace, FgbSearchBackwards);
        FgbSearchFromCaret := TRUE;
      end;
    end;
    LNode.SetInteger('Left', LSearchReplaceDialog.Left);
    LNode.SetInteger('Top', LSearchReplaceDialog.Top);
    TMRUOptions.Instance.Save;
  finally
    LSearchReplaceDialog.Free;
  end;
end;

procedure TCodeEditorFrame.DoSearchReplaceText(const AReplace: Boolean;
  const ABackwards: Boolean);
var
  Options: TSynSearchOptions;
begin
  if AReplace then
    Options := [ssoPrompt, ssoReplace, ssoReplaceAll]
  else
    Options := [];
  if ABackwards then
    Include(Options, ssoBackwards);
  if FgbSearchCaseSensitive then
    Include(Options, ssoMatchCase);
  if not FgbSearchFromCaret then
    Include(Options, ssoEntireScope);
  if FgbSearchSelectionOnly then
    Include(Options, ssoSelectedOnly);
  if FgbSearchWholeWords then
    Include(Options, ssoWholeWord);
  if FEdit.SearchReplace(FgsSearchText, FgsReplaceText, Options) = 0 then
  begin
    MessageBeep(MB_ICONASTERISK);
    if ssoBackwards in Options then
      FEdit.BlockEnd := FEdit.BlockBegin
    else
      FEdit.BlockBegin := FEdit.BlockEnd;
    FEdit.CaretXY := FEdit.BlockBegin;
  end;

  if ConfirmReplaceDialog <> nil then
    ConfirmReplaceDialog.Free;
end;

end.
