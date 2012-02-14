unit KIDE.UpdateLocaleFormUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, KIDE.gettext, Vcl.StdCtrls,
  Vcl.ActnList, Kitto.Metadata;

type
  TUpdateLocaleForm = class(TForm)
    LocalePanel: TPanel;
    FileNamePanel: TPanel;
    StatusPanel: TPanel;
    Button1: TButton;
    ActionList: TActionList;
    UpdateAction: TAction;
    procedure UpdateActionUpdate(Sender: TObject);
    procedure UpdateActionExecute(Sender: TObject);
    procedure SaveActionUpdate(Sender: TObject);
  private
    FFile: TPOFile;
    procedure SetFileName(const AValue: string);
    function GetFileName: string;
    procedure UpdateLineCount;
    procedure SetStatus(const AMessage: string);
    procedure ProcessMetadata(const AMetadata: TKMetadata);
    procedure ProcessingStatus(const AFileName: string);
  public
    class procedure ShowDialog(const AFileName: string);
    destructor Destroy; override;

    property FileName: string read GetFileName write SetFileName;

  end;

implementation

{$R *.dfm}

uses
  EF.Localization, EF.Tree, EF.YAML, EF.SysUtils,
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

procedure TUpdateLocaleForm.ProcessMetadata(const AMetadata: TKMetadata);
begin
  Assert(Assigned(AMetadata));

  ProcessingStatus(AMetadata.PersistentFileName);
  FFile.UpdateFromTree(AMetadata, ExtractFileName(AMetadata.PersistentFileName));
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
      Close;
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
    LForm.ShowModal;
  finally
    FreeAndNil(LForm);
  end;
end;

end.
