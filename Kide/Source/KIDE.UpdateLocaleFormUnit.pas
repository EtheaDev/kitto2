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
unit KIDE.UpdateLocaleFormUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, KIDE.gettext, Vcl.StdCtrls,
  Vcl.ActnList, Kitto.Metadata, KIDE.BaseFormUnit, System.Actions,
  EF.Tree, Vcl.StdActns;

type
  TUpdateLocaleForm = class(TBaseForm)
    LocalePanel: TPanel;
    FileNamePanel: TPanel;
    StatusPanel: TPanel;
    bbUpdate: TButton;
    ActionList: TActionList;
    UpdateAction: TAction;
    cbUpdateAttributes: TCheckBox;
    edgettextLocation: TLabeledEdit;
    procedure UpdateActionUpdate(Sender: TObject);
    procedure UpdateActionExecute(Sender: TObject);
    procedure SaveActionUpdate(Sender: TObject);
  private
    FFile: TPOFile;
    procedure UpdatedxGetTextPath;
    procedure SetFileName(const AValue: string);
    function GetFileName: string;
    procedure UpdateLineCount;
    procedure SetStatus(const AMessage: string);
    procedure ProcessMetadata(const AMetadata: TKMetadata);
    procedure ProcessingStatus(const AFileName: string);
    function UpdateLocalizableNode(const ANode: TEFTree): Boolean;
    procedure UpdateLocalizableAttributes(const ATree: TEFTree; const AFileName: string);
  public
    class procedure ShowDialog(const AFileName: string);
    destructor Destroy; override;

    property FileName: string read GetFileName write SetFileName;

  end;

implementation

{$R *.dfm}

uses
  EF.Localization, EF.YAML, EF.Sys, EF.StrUtils,
  KIDE.Project, KIDE.Utils;

{ TUpdateLocaleForm }

destructor TUpdateLocaleForm.Destroy;
begin
  FreeAndNil(FFile);
  inherited;
end;

function TUpdateLocaleForm.GetFileName: string;
begin
  if Assigned(FFile) then
    Result := FFile.FileName
  else
    Result := '';
end;

procedure TUpdateLocaleForm.SaveActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := Assigned(FFile) and FFile.IsChanged;
end;

procedure TUpdateLocaleForm.SetFileName(const AValue: string);
begin
  FreeAndNil(FFile);
  FFile := TPOFile.Load(AValue);
  FileNamePanel.Caption := FileName;
  LocalePanel.Caption := Format(_('Locale: %s'), [ExtractLocaleNameFromFileName(FileName)]);
  UpdateLineCount;
end;

procedure TUpdateLocaleForm.ProcessingStatus(const AFileName: string);
begin
  SetStatus(Format(_('Processing %s...'), [AFileName]));
end;


function TUpdateLocaleForm.UpdateLocalizableNode(const ANode: TEFTree): Boolean;
var
  I, J: Integer;
  LNode, LConfigNode: TEFNode;
  LFileName: string;
  LMetadataConfig: TEFPersistentTree;
  LNodeName: string;
  LNodeValue: string;
  LDefaultNodeValue: string;
  LTree: TEFTree;
  LMultiple: Boolean;
begin
  Result := False;
  LFileName := GetMetadataNodeFileName(ANode);
  if FileExists(LFileName) then
  begin
    //Check localizable attributes for subnodes
    LMetadataConfig := TEFPersistentTree.Create;
    TEFYAMLReader.LoadTree(LMetadataConfig, LFileName);
    for I := 0 to LMetadataConfig.ChildCount -1 do
    begin
      //Check required nodes
      LConfigNode := LMetadataConfig.Children[I];
      LNodeName := LConfigNode.Name;
      if LConfigNode.GetBoolean('Localizable') then
      begin
        LMultiple := LConfigNode.GetBoolean('Multiple');
        if not LMultiple then
        begin
          LNodeValue := ANode.GetString(LNodeName);
          LDefaultNodeValue := LConfigNode.GetString('Localizable/default');
          if (LNodeValue <> '') and not KIDE.gettext.IsEnclosed(LNodeValue) then
          begin
            ANode.SetString(LNodeName, '_('+LNodeValue+')');
            Result := True;
          end
          else if (LNodeValue = '') and SameText('self', LDefaultNodeValue) then
          begin
            LNodeValue := TEFNode(ANode).Name;
            if not (ANode is TEFNode) then
              ANode.SetString(LNodeName, '_('+ChangeFileExt(ExtractFileName(LNodeValue),'')+')')
            else
              ANode.SetString(LNodeName, '_('+LNodeValue+')');
            Result := True;
          end
          else if (LNodeValue = '') and (LDefaultNodeValue <> '') then
          begin
            LDefaultNodeValue := StripPrefixAndSuffix(ANode.GetString(LDefaultNodeValue),'_(',')');
            if (LDefaultNodeValue <> '') and (ANode.GetString(LNodeName)='') then
            begin
              ANode.SetString(LNodeName, '_('+LDefaultNodeValue+')');
              Result := True;
            end;
          end;
        end
        else
        begin
          for J := 0 to ANode.ChildCount - 1 do
          begin
            if SameText(ANode.Children[J].Name, LNodeName) then
            begin
              LNodeValue := ANode.Children[J].AsString;
              if (LNodeValue <> '') and not KIDE.gettext.IsEnclosed(LNodeValue) then
              begin
                ANode.Children[J].AsString := '_('+LNodeValue+')';
                Result := True;
              end;
            end;
          end;
        end;
      end;
    end;
  end;
  for I := 0 to ANode.ChildCount - 1 do
    Result := Result or UpdateLocalizableNode(ANode.Children[I]);
end;

procedure TUpdateLocaleForm.UpdateLocalizableAttributes(const ATree: TEFTree; const AFileName: string);
var
  I: Integer;
  Result: Boolean;
  LReference: string;
begin
  LReference := ExtractFileName(AFileName);
  Result := UpdateLocalizableNode(ATree);
  //for I := 0 to ATree.ChildCount - 1 do
  //  Result := Result or UpdateLocalizableNode(ATree.Children[I]);
  if Result (*and (MessageDlg(Format('File "%s" must be updated: do you want to save it?',
  [ExtractFilename(AFileName)]),
  mtWarning, [mbYes, mbNo], 0) = mrYes)*) then
  begin
    TEFYAMLWriter.SaveTree(ATree, AFileName);
  end;
end;

procedure TUpdateLocaleForm.ProcessMetadata(const AMetadata: TKMetadata);
var
  LFileName: string;
begin
  Assert(Assigned(AMetadata));

  LFileName := AMetadata.PersistentFileName;
  ProcessingStatus(LFileName);
  if cbUpdateAttributes.Checked then
    UpdateLocalizableAttributes(AMetadata, LFileName);
  FFile.UpdateFromTree(AMetadata, ExtractFileName(LFileName));
end;

procedure TUpdateLocaleForm.UpdateActionExecute(Sender: TObject);
var
  I: Integer;
  LConfigFileNames: TStrings;
  LReader: TEFYAMLReader;
  LTree: TEFTree;
  LSourceDirectory: string;
  LTempFileName: string;
  LOutput: TStrings;
begin
  //
  // In-memory updates.
  //

  // Models
  for I := 0 to TProject.CurrentProject.Config.Models.ModelCount - 1 do
    ProcessMetadata(TProject.CurrentProject.Config.Models[I]);
  // Views
  for I := 0 to TProject.CurrentProject.Config.Views.ViewCount - 1 do
    ProcessMetadata(TProject.CurrentProject.Config.Views[I]);
  // Layouts
  for I := 0 to TProject.CurrentProject.Config.Views.Layouts.LayoutCount - 1 do
    ProcessMetadata(TProject.CurrentProject.Config.Views.Layouts[I]);
  // Configs
  LConfigFileNames := TStringList.Create;
  try
    TProject.CurrentProject.GetConfigFileNames(LConfigFileNames);
    LReader := TEFYAMLReader.Create;
    try
      LTree := TEFTree.Create;
      try
        for I := 0 to LConfigFileNames.Count - 1 do
        begin
          ProcessingStatus(LConfigFileNames[I]);
          LReader.LoadTreeFromFile(LTree, LConfigFileNames[I]);
          FFile.UpdateFromTree(LTree, ExtractFileName(LConfigFileNames[I]));
        end;
      finally
        FreeAndNil(LTree);
      end;
    finally
      FreeAndNil(LReader);
    end;
  finally
    FreeAndNil(LConfigFileNames);
  end;

  //
  // On-disk updates.
  //
  FFile.Save;
  // Source files.
  LSourceDirectory := TProject.CurrentProject.SourceDirectory;
  ProcessingStatus(LSourceDirectory);
  LTempFileName := GetTempFileName('.po');
  try
    LOutput := TStringList.Create;
    try
      ExtractdxTranslatableStrings(LSourceDirectory, LTempFileName, LOutput);
      { TODO : use a nicer two-level error dialog. Make it generic at least for command output. }
      if not FileExists(LTempFileName) or (LOutput.Count > 0) then
        raise Exception.CreateFmt(_('Error extracting translations from source files. %s'), [LOutput.Text]);
      LOutput.Clear;
      MergePOFiles(LTempFileName, FFile.FileName, LOutput);
      if LOutput.Count > 0 then
        raise Exception.CreateFmt(_('Error merging translations. %s'), [LOutput.Text]);
      DeleteFile(LTempFileName);
      // For now we just close - no use in reloading.
      //FFile.Reload;
      SetStatus(_('Done.'));
      //Close;
    finally
      FreeAndNil(LOutput);
    end;
  finally
    if FileExists(LTempFileName) then
      DeleteFile(LTempFileName);
  end;
end;

procedure TUpdateLocaleForm.UpdateActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := Assigned(FFile);
end;

procedure TUpdateLocaleForm.UpdatedxGetTextPath;
var
  LPath: string;
begin
  if GetdxgettextDirectory(LPath) then
    edgettextLocation.Text := LPath
  else
    edgettextLocation.Text := '';
end;

procedure TUpdateLocaleForm.UpdateLineCount;
begin
  SetStatus(Format(_('Lines: %d'), [FFile.Count]));
end;

procedure TUpdateLocaleForm.SetStatus(const AMessage: string);
begin
  StatusPanel.Caption := AMessage;
  StatusPanel.Update;
end;

class procedure TUpdateLocaleForm.ShowDialog(const AFileName: string);
var
  LForm: TUpdateLocaleForm;
begin
  LForm := TUpdateLocaleForm.Create(Application);
  try
    LForm.FileName := AFileName;
    LForm.UpdatedxGetTextPath;
    LForm.ShowModal;
  finally
    FreeAndNil(LForm);
  end;
end;

end.
