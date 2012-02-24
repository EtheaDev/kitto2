unit KIDE.FileTree;

interface

uses
  Vcl.ComCtrls,
  Kitto.Metadata, Kitto.Metadata.Models, Kitto.Metadata.Views,
  KIDE.Project;

type
  TFileActionMetadata = record
    DisplayLabel: string;
    Hint: string;
    ImageIndex: Integer;
  public
    procedure Clear;
  end;

  // Base class for file and folder node handlers.
  TFileNodeHandler = class abstract
  protected
    function GetFileName: string; virtual;
  public
    property FileName: string read GetFileName;
    function GetActionCount: Integer; virtual;
    function GetActionMetadata(const AIndex: Integer): TFileActionMetadata; virtual;
    procedure ExecuteAction(const AIndex: Integer); virtual;
  end;

  TMetadataFileNodeHandler = class abstract(TFileNodeHandler)
  protected
    FMetadata: TKMetadata;
    function GetFileName: string; override;
  end;

  TModelFileNodeHandler = class(TMetadataFileNodeHandler)
  private
    function GetModel: TKModel;
  public
    property Model: TKModel read GetModel;
    constructor Create(const AModel: TKModel);
  end;

  TViewFileNodeHandler = class(TMetadataFileNodeHandler)
  private
    function GetView: TKView;
  public
    property View: TKView read GetView;
    constructor Create(const AView: TKView);
  end;

  TLayoutFileNodeHandler = class(TMetadataFileNodeHandler)
  private
    function GetLayout: TKLayout;
  public
    property Layout: TKLayout read GetLayout;
    constructor Create(const ALayout: TKLayout);
  end;

  TGenericFileNodeHandler = class(TFileNodeHandler)
  private
    FFileName: string;
  protected
    function GetFileName: string; override;
  public
    constructor Create(const AFileName: string);
  end;

  TConfigFileNodeHandler = class(TGenericFileNodeHandler);

  TLocaleFileNodeHandler = class(TGenericFileNodeHandler)
  public
    function GetActionCount: Integer; override;
    function GetActionMetadata(const AIndex: Integer): TFileActionMetadata; override;
    procedure ExecuteAction(const AIndex: Integer); override;
  end;

  TGenericFolderNodeHandler = class(TFileNodeHandler);

  TModelsFolderNodeHandler = class(TGenericFolderNodeHandler)
  public
    function GetActionCount: Integer; override;
    function GetActionMetadata(const AIndex: Integer): TFileActionMetadata; override;
    procedure ExecuteAction(const AIndex: Integer); override;
  end;

  TFileTreeNode = class(TTreeNode)
  private
    FHandler: TFileNodeHandler;
    function GetActionCount: Integer;
    function GetActionMetadata(const AIndex: Integer): TFileActionMetadata;
  public
    procedure DefaultAction;
    destructor Destroy; override;

    property ActionCount: Integer read GetActionCount;
    property ActionMetadata[const AIndex: Integer]: TFileActionMetadata read GetActionMetadata;
    procedure ExecuteAction(const AIndex: Integer);
  end;

procedure RefreshFilesTreeView(const ATreeView: TTreeView; const AProject: TProject);

implementation

uses
  SysUtils, StrUtils, Classes,
  EF.SysUtils, EF.StrUtils, EF.Localization,
  KIDE.Shell, KIDE.Utils, KIDE.UpdateLocaleFormUnit, KIDE.ModelWizardFormUnit;

procedure EditFile(const AFileName: string);
begin
  OpenDocument(AFileName, False);
end;

procedure RefreshFilesTreeView(const ATreeView: TTreeView; const AProject: TProject);
var
  LParentNode: TFileTreeNode;
  I: Integer;
  LModel: TKModel;
  LNode: TFileTreeNode;
  LView: TKView;
  LFileNames: TStrings;
  LLayout: TKLayout;
begin
  Assert(Assigned(ATreeView));

  ATreeView.Items.Clear;

  if not Assigned(AProject) then
    Exit;

  // Config files
  LFileNames := TStringList.Create;
  try
    AProject.GetConfigFileNames(LFileNames);
    for I := 0 to LFileNames.Count - 1 do
    begin
      LNode := ATreeView.Items.AddChild(nil, ExtractFileName(LFileNames[I])) as TFileTreeNode;
      LNode.ImageIndex := 3;
      LNode.SelectedIndex := LNode.ImageIndex;
      LNode.FHandler := TConfigFileNodeHandler.Create(LFileNames[I]);
    end;
  finally
    FreeAndNil(LFileNames);
  end;

  // Models
  LParentNode := ATreeView.Items.AddChild(nil, _('Models')) as TFileTreeNode;
  LParentNode.ImageIndex := 0;
  LParentNode.SelectedIndex := LParentNode.ImageIndex;
  LParentNode.FHandler := TModelsFolderNodeHandler.Create;
  for I := 0 to AProject.Config.Models.ModelCount - 1 do
  begin
    LModel := AProject.Config.Models[I];
    LNode := ATreeView.Items.AddChild(LParentNode, LModel.PersistentName) as TFileTreeNode;
    LNode.ImageIndex := 5;
    LNode.SelectedIndex := LNode.ImageIndex;
    LNode.FHandler := TModelFileNodeHandler.Create(LModel);
  end;

  // Views
  LParentNode := ATreeView.Items.AddChild(nil, _('Views')) as TFileTreeNode;
  LParentNode.ImageIndex := 0;
  LParentNode.SelectedIndex := LParentNode.ImageIndex;
  for I := 0 to AProject.Config.Views.ViewCount - 1 do
  begin
    LView := AProject.Config.Views[I];
    LNode := ATreeView.Items.AddChild(LParentNode, LView.PersistentName) as TFileTreeNode;
    LNode.ImageIndex := 4;
    LNode.SelectedIndex := LNode.ImageIndex;
    LNode.FHandler := TViewFileNodeHandler.Create(LView);
  end;
  // Layouts
  LParentNode := ATreeView.Items.AddChild(LParentNode, _('Layouts')) as TFileTreeNode;
  LParentNode.ImageIndex := 0;
  LParentNode.SelectedIndex := LParentNode.ImageIndex;
  for I := 0 to AProject.Config.Views.Layouts.LayoutCount - 1 do
  begin
    LLayout := AProject.Config.Views.Layouts[I];
    LNode := ATreeView.Items.AddChild(LParentNode, LLayout.PersistentName) as TFileTreeNode;
    LNode.ImageIndex := 8;
    LNode.SelectedIndex := LNode.ImageIndex;
    LNode.FHandler := TLayoutFileNodeHandler.Create(LLayout);
  end;

  // Locale files
  LParentNode := ATreeView.Items.AddChild(nil, _('Locales')) as TFileTreeNode;
  LParentNode.ImageIndex := 0;
  LParentNode.SelectedIndex := LParentNode.ImageIndex;
  LFileNames := TStringList.Create;
  try
    FindAllFiles('po', AProject.Config.AppHomePath + 'Locale', LFileNames, True, True);
    for I := 0 to LFileNames.Count - 1 do
    begin
      if SameText(ExtractFileName(LFileNames[I]), 'default.po') then
      begin
        LNode := ATreeView.Items.AddChild(LParentNode, ExtractLocaleNameFromFileName(LFileNames[I])) as TFileTreeNode;
        { TODO : use a different flag for each language }
        LNode.ImageIndex := 10;
        LNode.SelectedIndex := LNode.ImageIndex;
        LNode.FHandler := TLocaleFileNodeHandler.Create(LFileNames[I]);
      end;
    end;
  finally
    FreeAndNil(LFileNames);
  end;

  ATreeView.FullExpand;
  ATreeView.Refresh;
end;

{ TModelFileNodeHandler }

constructor TModelFileNodeHandler.Create(const AModel: TKModel);
begin
  inherited Create;
  FMetadata := AModel;
end;

function TModelFileNodeHandler.GetModel: TKModel;
begin
  Result := FMetadata as TKModel;
end;

{ TViewFileNodeHandler }

constructor TViewFileNodeHandler.Create(const AView: TKView);
begin
  inherited Create;
  FMetadata := AView;
end;

function TViewFileNodeHandler.GetView: TKView;
begin
  Result := FMetadata as TKView;
end;

{ TGenericFileNodeHandler }

constructor TGenericFileNodeHandler.Create(const AFileName: string);
begin
  inherited Create;
  FFileName := AFileName;
end;

function TGenericFileNodeHandler.GetFileName: string;
begin
  Result := FFileName;
end;

{ TFileTreeNode }

procedure TFileTreeNode.DefaultAction;
begin
  if Assigned(FHandler) then
    FHandler.ExecuteAction(0);
end;

destructor TFileTreeNode.Destroy;
begin
  FreeAndNil(FHandler);
  inherited;
end;

procedure TFileTreeNode.ExecuteAction(const AIndex: Integer);
begin
  if Assigned(FHandler) then
    FHandler.ExecuteAction(AIndex);
end;

function TFileTreeNode.GetActionCount: Integer;
begin
  if Assigned(FHandler) then
    Result := FHandler.GetActionCount
  else
    Result := 0;
end;

function TFileTreeNode.GetActionMetadata(const AIndex: Integer): TFileActionMetadata;
begin
  if Assigned(FHandler) then
    Result := FHandler.GetActionMetadata(AIndex)
  else
    Result.Clear;
end;

{ TFileNodeHandler }

procedure TFileNodeHandler.ExecuteAction(const AIndex: Integer);
begin
  case AIndex of
    0:
    begin
      if not FileExists(FileName) then
        raise Exception.CreateFmt(_('File %s not found.'), [FileName]);
      EditFile(FileName);
    end;
  end;
end;

function TFileNodeHandler.GetActionCount: Integer;
begin
  Result := 1;
end;

function TFileNodeHandler.GetActionMetadata(const AIndex: Integer): TFileActionMetadata;
begin
  Result.Clear;
  case AIndex of
    0:
    begin
      Result.DisplayLabel := _('Edit source');
      Result.Hint := _('Edit the file through an external application');
      Result.ImageIndex := 11;
    end;
  else
    Result.Clear;
  end;
end;

function TFileNodeHandler.GetFileName: string;
begin
  Result := '';
end;

{ TMetadataFileNodeHandler }

function TMetadataFileNodeHandler.GetFileName: string;
begin
  if Assigned(FMetadata) then
    Result := FMetadata.PersistentFileName
  else
    Result := '';
end;

{ TLayoutFileNodeHandler }

constructor TLayoutFileNodeHandler.Create(const ALayout: TKLayout);
begin
  inherited Create;
  FMetadata := ALayout;
end;

function TLayoutFileNodeHandler.GetLayout: TKLayout;
begin
  Result := FMetadata as TKLayout;
end;

{ TFileActionMetadata }

procedure TFileActionMetadata.Clear;
begin
  DisplayLabel := '';
  Hint := '';
  ImageIndex := -1;
end;

{ TLocaleFileNodeHandler }

procedure TLocaleFileNodeHandler.ExecuteAction(const AIndex: Integer);
begin
  if AIndex = GetActionCount - 1 then
    TUpdateLocaleForm.ShowDialog(FileName)
  else
    inherited;
end;

function TLocaleFileNodeHandler.GetActionCount: Integer;
begin
  Result := inherited GetActionCount + 1;
end;

function TLocaleFileNodeHandler.GetActionMetadata(
  const AIndex: Integer): TFileActionMetadata;
begin
  if AIndex = GetActionCount - 1 then
  begin
    Result.Clear;
    Result.DisplayLabel := _('Update...');
    Result.Hint := _('Updates the file with all translatable strings found in metadata files');
    Result.ImageIndex := 12;
  end
  else
    Result := inherited GetActionMetadata(AIndex);
end;

{ TModelsFolderNodeHandler }

procedure TModelsFolderNodeHandler.ExecuteAction(const AIndex: Integer);
begin
  if AIndex = GetActionCount - 1 then
    TModelWizardForm.ShowDialog
  else
    inherited;
end;

function TModelsFolderNodeHandler.GetActionCount: Integer;
begin
  Result := 1;
end;

function TModelsFolderNodeHandler.GetActionMetadata(
  const AIndex: Integer): TFileActionMetadata;
begin
  if AIndex = GetActionCount - 1 then
  begin
    Result.Clear;
    Result.DisplayLabel := _('Update...');
    Result.Hint := _('Updates models according to existing database tables');
    Result.ImageIndex := 7;
  end
  else
    Result := inherited GetActionMetadata(AIndex);
end;

end.
