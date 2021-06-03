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
unit KIDE.FileTree;

interface

uses
  Generics.Collections, Vcl.ComCtrls, SysUtils,
  EF.Tree,
  Kitto.Config, Kitto.Metadata, Kitto.Metadata.Models, Kitto.Metadata.Views,
  KIDE.Project, KIDE.Editor;

type
  TFileNodeHandler = class;

  TFileNodeHandlerList = class(TList<TFileNodeHandler>)
  private
    function GetIsHomogeneous: Boolean;
  public
    property IsHomogeneous: Boolean read GetIsHomogeneous;
  end;

  TFileAction = record
    DisplayLabel: string;
    Hint: string;
    ImageIndex: Integer;
    EnableOnHomogeneousMultiSelect: Boolean;
    EnableOnHeterogenousMultiSelect: Boolean;
    RefreshAfterExecute: Boolean;
    Proc: TProc<IEditContext, TFileNodeHandlerList>;
    function EnableOnMultiSelect: Boolean;
  public
    procedure Clear;

    function IsEnabled(const ASelection: TFileNodeHandlerList): Boolean;
    procedure Execute(const AContext: IEditContext;
      const ASelection: TFileNodeHandlerList);
  end;

  ///	<summary>Base class for file and folder node handlers.</summary>
  TFileNodeHandler = class abstract
  private
  protected
    FActions: TList<TFileAction>;
    function GetFileName: string; virtual;
    procedure InitActions; virtual;

    procedure AddSeparatorAction;
    procedure AddEditSourceAction;
    procedure AddEditSourceIntoIDEAction;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;

    property FileName: string read GetFileName;
    function GetActionCount: Integer;
    function GetActionMetadata(const AIndex: Integer): TFileAction;
    procedure ExecuteAction(const AContext: IEditContext; const AIndex: Integer;
      const ASelection: TFileNodeHandlerList); virtual;
  end;

  ///	<summary>Base class for persistent tree (such as config and metadata
  ///	object) node handlers.</summary>
  TEFPersistentTreeFileNodeHandler = class abstract(TFileNodeHandler)
  strict protected
    FTree: TEFPersistentTree;
    function GetFileName: string; override;
    procedure AddDesignAction(const AHint: string);
    procedure AddDeleteAction(const AHint: string; const AImageIndex: Integer);
    procedure AddValidateAction(const AHint: string);
    procedure CallPassingTreeList(const AProc: TProc<TList<TEFPersistentTree>>;
      const ASelection: TFileNodeHandlerList);
  public
    property Tree: TEFPersistentTree read FTree;
  end;

  TConfigFileNodeHandler = class(TEFPersistentTreeFileNodeHandler)
  protected
    procedure InitActions; override;
  public
    constructor Create(const AConfig: TKConfig);
  end;

  TMetadataFileNodeHandler = class abstract(TEFPersistentTreeFileNodeHandler)
  private
    function GetMetadata: TKMetadata;
  public
    property Metadata: TKMetadata read GetMetadata;
  end;

  TModelFileNodeHandler = class(TMetadataFileNodeHandler)
  private
    function GetModel: TKModel;
    procedure CallPassingModelList(const AProc: TProc<TKModelList>;
      const ASelection: TFileNodeHandlerList);
  protected
    procedure InitActions; override;
  public
    property Model: TKModel read GetModel;
    constructor Create(const AModel: TKModel);
  end;

  TViewFileNodeHandler = class(TMetadataFileNodeHandler)
  private
    function GetView: TKView;
    procedure CallPassingViewList(const AProc: TProc<TKViewList>;
      const ASelection: TFileNodeHandlerList);
  protected
    procedure InitActions; override;
  public
    property View: TKView read GetView;
    constructor Create(const AView: TKView);
  end;

  TLayoutFileNodeHandler = class(TMetadataFileNodeHandler)
  private
    function GetLayout: TKLayout;
  protected
    procedure InitActions; override;
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

  TLocaleFileNodeHandler = class(TGenericFileNodeHandler)
  protected
    procedure InitActions; override;
  end;

  TGenericFolderNodeHandler = class abstract(TGenericFileNodeHandler)
  protected
    procedure AddOpenFolderAction;
  public
    property Path: string read GetFileName;
  end;

  TConfigsFolderNodeHandler = class(TGenericFolderNodeHandler)
  protected
    procedure InitActions; override;
  end;

  TModelsFolderNodeHandler = class(TGenericFolderNodeHandler)
  protected
    procedure InitActions; override;
  end;

  TViewsFolderNodeHandler = class(TGenericFolderNodeHandler)
  private
    procedure ExecuteNewAction(const AContext: IEditContext;
      const AAction: TFileAction; const ASelection: TFileNodeHandlerList);
  protected
    procedure InitActions; override;
  end;

  TLayoutsFolderNodeHandler = class(TGenericFolderNodeHandler)
  protected
    procedure InitActions; override;
  end;

  TResourceFileNodeHandler = class(TGenericFileNodeHandler)
  protected
    procedure InitActions; override;
  end;

  TResourceFolderNodeHandler = class(TGenericFolderNodeHandler)
  protected
    procedure InitActions; override;
  end;


  ///	<summary>Visually represents a node in the IDE's tree view; delegates
  ///	operations to an internal object handler.</summary>
  TFileTreeNode = class(TTreeNode)
  private
    FHandler: TFileNodeHandler;
    function GetActionCount: Integer;
    function GetActionMetadata(const AIndex: Integer): TFileAction;
  public
    destructor Destroy; override;

    property Handler: TFileNodeHandler read FHandler;

    property ActionCount: Integer read GetActionCount;
    property ActionMetadata[const AIndex: Integer]: TFileAction read GetActionMetadata;
    procedure ExecuteAction(const AContext: IEditContext; const AIndex: Integer;
      const ASelection: TFileNodeHandlerList);
    procedure ExecuteDefaultAction(const AContext: IEditContext;
      const ASelection: TFileNodeHandlerList);
  end;

procedure BuildMetadataTreeView(const ATreeView: TTreeView; const AProject: TProject);

procedure BuildResourcesTreeView(const ATreeView: TTreeView; const AProject: TProject);

implementation

uses
  StrUtils, Classes, Controls, Dialogs,
  EF.Classes, EF.Sys.Windows, EF.StrUtils, EF.Localization, EF.YAML,
  KIDE.Config, KIDE.Shell, KIDE.Utils, KIDE.UpdateLocaleFormUnit, KIDE.ModelWizardFormUnit,
  KIDE.DataViewWizardFormUnit, KIDE.MainDataModuleUnit,
  KIDE.TreeValidator, KIDE.ModelValidator, KIDE.ViewValidator, KIDE.ConfigValidator,
  KIDE.LayoutValidator;

procedure EditFile(const AFileName: string);
begin
  EditDocument(AFileName, False);
end;

procedure BuildMetadataTreeView(const ATreeView: TTreeView; const AProject: TProject);
var
  LParentNode: TFileTreeNode;
  I: Integer;
  LModel: TKModel;
  LNode: TFileTreeNode;
  LView: TKView;
  LLayout: TKLayout;
begin
  Assert(Assigned(ATreeView));

  ATreeView.Items.Clear;

  if not Assigned(AProject) then
    Exit;

  // Configs
  LParentNode := ATreeView.Items.AddChild(nil, _('Configs')) as TFileTreeNode;
  LParentNode.ImageIndex := FOLDER_PICTURE;
  LParentNode.SelectedIndex := LParentNode.ImageIndex;
  LParentNode.FHandler := TConfigsFolderNodeHandler.Create(AProject.Config.GetMetadataPath);
  for I := 0 to AProject.AppConfigCount - 1 do
  begin
    LNode := ATreeView.Items.AddChild(LParentNode, ExtractFileName(AProject.AppConfigs[I].Config.PersistentName)) as TFileTreeNode;
    LNode.ImageIndex := CONFIG_PICTURE;
    LNode.SelectedIndex := LNode.ImageIndex;
    LNode.FHandler := TConfigFileNodeHandler.Create(AProject.AppConfigs[I]);
  end;

  // Models
  LParentNode := ATreeView.Items.AddChild(nil, _('Models')) as TFileTreeNode;
  LParentNode.ImageIndex := FOLDER_PICTURE;
  LParentNode.SelectedIndex := LParentNode.ImageIndex;
  LParentNode.FHandler := TModelsFolderNodeHandler.Create(AProject.Config.Models.Path);
  for I := 0 to AProject.Config.Models.ModelCount - 1 do
  begin
    LModel := AProject.Config.Models[I];
    LNode := ATreeView.Items.AddChild(LParentNode, LModel.PersistentName) as TFileTreeNode;
    LNode.ImageIndex := MODEL_PICTURE;
    LNode.SelectedIndex := LNode.ImageIndex;
    LNode.FHandler := TModelFileNodeHandler.Create(LModel);
  end;

  // Views
  LParentNode := ATreeView.Items.AddChild(nil, _('Views')) as TFileTreeNode;
  LParentNode.ImageIndex := FOLDER_PICTURE;
  LParentNode.SelectedIndex := LParentNode.ImageIndex;
  LParentNode.FHandler := TViewsFolderNodeHandler.Create(AProject.Config.Views.Path);
  for I := 0 to AProject.Config.Views.ViewCount - 1 do
  begin
    LView := AProject.Config.Views[I];
    LNode := ATreeView.Items.AddChild(LParentNode, LView.PersistentName) as TFileTreeNode;
    LNode.ImageIndex :=AProject.GetViewImageIndex(LView);
    LNode.SelectedIndex := LNode.ImageIndex;
    LNode.FHandler := TViewFileNodeHandler.Create(LView);
  end;

  // Layouts
  LParentNode := ATreeView.Items.AddChild(nil, _('Layouts')) as TFileTreeNode;
  LParentNode.ImageIndex := FOLDER_PICTURE;
  LParentNode.SelectedIndex := LParentNode.ImageIndex;
  LParentNode.FHandler := TLayoutsFolderNodeHandler.Create(AProject.Config.Views.Layouts.Path);
  for I := 0 to AProject.Config.Views.Layouts.LayoutCount - 1 do
  begin
    LLayout := AProject.Config.Views.Layouts[I];
    LNode := ATreeView.Items.AddChild(LParentNode, LLayout.PersistentName) as TFileTreeNode;
    LNode.ImageIndex := AProject.GetLayoutImageIndex(LLayout);
    LNode.SelectedIndex := LNode.ImageIndex;
    LNode.FHandler := TLayoutFileNodeHandler.Create(LLayout);
  end;

  ATreeView.FullExpand;
  if ATreeView.Items.Count > 0 then
    ATreeView.Select(ATreeView.Items[0]);
  ATreeView.Refresh;
end;

procedure AddResourceDirectoryNodes(const ATreeView: TTreeView;
  const AParentNode: TFileTreeNode; const APath: string); forward;

procedure AddResourceFileNode(const ATreeView: TTreeView;
  const AParentNode: TFileTreeNode; const APath: string; const ASearchRec: TSearchRec);
var
  LNode: TFileTreeNode;
begin
  Assert(Assigned(ATreeView));

  if (ASearchRec.Attr and faDirectory) = faDirectory then
  begin
    LNode := ATreeView.Items.AddChild(AParentNode, ASearchRec.Name) as TFileTreeNode;
    LNode.ImageIndex := 0;
    LNode.SelectedIndex := LNode.ImageIndex;
    LNode.FHandler := TResourceFolderNodeHandler.Create(APath + ASearchRec.Name);
    AddResourceDirectoryNodes(ATreeView, LNode, IncludeTrailingPathDelimiter(APath + ASearchRec.Name))
  end
  else
  begin
    LNode := ATreeView.Items.AddChild(AParentNode, ASearchRec.Name) as TFileTreeNode;
    LNode.ImageIndex := GetFileImageIndex(ExtractFileFormat(ASearchRec.Name));
    LNode.SelectedIndex := LNode.ImageIndex;
    LNode.FHandler := TResourceFileNodeHandler.Create(APath + ASearchRec.Name);
  end;
end;

procedure AddResourceDirectoryNodes(const ATreeView: TTreeView;
  const AParentNode: TFileTreeNode; const APath: string);
var
  LSearchRec: TSearchRec;
begin
  if FindFirst(APath + '*', faAnyFile, LSearchRec) = 0 then
  begin
    repeat
      if (LSearchRec.Name <> '.') and (LSearchRec.Name <> '..') then
        AddResourceFileNode(ATreeView, AParentNode, APath, LSearchRec);
    until FindNext(LSearchRec) <> 0;

    FindClose(LSearchRec);
  end;
end;

procedure BuildResourcesTreeView(const ATreeView: TTreeView; const AProject: TProject);
var
  LParentNode: TFileTreeNode;
  LFileNames: TStrings;
  I: Integer;
  LNode: TFileTreeNode;
begin
  Assert(Assigned(ATreeView));

  ATreeView.Items.Clear;

  if not Assigned(AProject) then
    Exit;

  // Resource files
  AddResourceDirectoryNodes(ATreeView, nil, AProject.GetResourcesPath);

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
        LNode.ImageIndex := LANGUAGES_PICTURE;
        LNode.SelectedIndex := LNode.ImageIndex;
        LNode.FHandler := TLocaleFileNodeHandler.Create(LFileNames[I]);
      end;
    end;
  finally
    FreeAndNil(LFileNames);
  end;

  ATreeView.FullExpand;
  if ATreeView.Items.Count > 0 then
    ATreeView.Select(ATreeView.Items[0]);
  ATreeView.Refresh;
end;

{ TModelFileNodeHandler }

procedure TModelFileNodeHandler.CallPassingModelList(
  const AProc: TProc<TKModelList>; const ASelection: TFileNodeHandlerList);
var
  LList: TKModelList;
  I: Integer;
begin
  LList := TKModelList.Create;
  try
    for I := 0 to ASelection.Count - 1 do
      LList.Add((ASelection[I] as TModelFileNodeHandler).Model);
    AProc(LList);
  finally
    FreeAndNil(LList);
  end;
end;

constructor TModelFileNodeHandler.Create(const AModel: TKModel);
begin
  inherited Create;
  FTree := AModel;
end;

function TModelFileNodeHandler.GetModel: TKModel;
begin
  Result := Metadata as TKModel;
end;

procedure TModelFileNodeHandler.InitActions;
var
  LAction: TFileAction;
begin
  inherited;
  AddDesignAction(_('Open Model in the designer'));

  LAction.Clear;
  LAction.DisplayLabel := _('Update...');
  LAction.Hint := _('Updates selected model(s) according to existing database tables');
  LAction.ImageIndex := MODEL_WIZARD_PICTURE;
  LAction.EnableOnHomogeneousMultiSelect := True;
  LAction.RefreshAfterExecute := True;
  LAction.Proc :=
    procedure (AContext: IEditContext; ASelection: TFileNodeHandlerList)
    begin
      CallPassingModelList(
        procedure(AList: TKModelList)
        begin
          TModelWizardForm.ShowDialog(AList);
        end, ASelection);
    end;
  FActions.Add(LAction);

  LAction.Clear;
  LAction.DisplayLabel := _('Create DataView...');
  LAction.Hint := _('Creates a DataView based on the selected model(s)');
  LAction.ImageIndex := DATA_WIZARD_PICTURE;
  LAction.EnableOnHomogeneousMultiSelect := True;
  LAction.RefreshAfterExecute := True;
  LAction.Proc :=
    procedure (AContext: IEditContext; ASelection: TFileNodeHandlerList)
    begin
      CallPassingModelList(
      procedure(AList: TKModelList)
      begin
        TDataViewWizardForm.ShowDialog(AList);
      end, ASelection)
    end;
  FActions.Add(LAction);

  AddSeparatorAction;

  AddDeleteAction(_('Delete Model'), DELETE_MODEL_PICTURE);

  AddSeparatorAction;

  AddEditSourceAction;

  AddSeparatorAction;

  AddValidateAction(_('Validate Model'));
end;

{ TViewFileNodeHandler }

constructor TViewFileNodeHandler.Create(const AView: TKView);
begin
  inherited Create;
  FTree := AView;
end;

function TViewFileNodeHandler.GetView: TKView;
begin
  Result := Tree as TKView;
end;

procedure TViewFileNodeHandler.CallPassingViewList(const AProc: TProc<TKViewList>;
  const ASelection: TFileNodeHandlerList);
var
  LViewList: TKViewList;
  I: Integer;
begin
  LViewList := TKViewList.Create;
  try
    for I := 0 to ASelection.Count - 1 do
      LViewList.Add((ASelection[I] as TViewFileNodeHandler).View);
    AProc(LViewList);
  finally
    FreeAndNil(LViewList);
  end;
end;

procedure TViewFileNodeHandler.InitActions;
begin
  inherited;
  AddDesignAction(_('Open View in the designer'));

  AddSeparatorAction;

  AddDeleteAction(_('Delete View'), DELETE_VIEW_PICTURE);

  AddSeparatorAction;

  AddEditSourceAction;

  AddSeparatorAction;

  AddValidateAction(_('Validate View'));
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

procedure TFileTreeNode.ExecuteDefaultAction(const AContext: IEditContext;
  const ASelection: TFileNodeHandlerList);
begin
  if Assigned(FHandler) and (FHandler.GetActionCount > 0) then
    FHandler.ExecuteAction(AContext, 0, ASelection);
end;

destructor TFileTreeNode.Destroy;
begin
  FreeAndNil(FHandler);
  inherited;
end;

procedure TFileTreeNode.ExecuteAction(const AContext: IEditContext;
  const AIndex: Integer; const ASelection: TFileNodeHandlerList);
begin
  if Assigned(FHandler) then
    FHandler.ExecuteAction(AContext, AIndex, ASelection);
end;

function TFileTreeNode.GetActionCount: Integer;
begin
  if Assigned(FHandler) then
    Result := FHandler.GetActionCount
  else
    Result := 0;
end;

function TFileTreeNode.GetActionMetadata(const AIndex: Integer): TFileAction;
begin
  if Assigned(FHandler) then
    Result := FHandler.GetActionMetadata(AIndex)
  else
    Result.Clear;
end;

{ TFileNodeHandler }

procedure TFileNodeHandler.AddEditSourceAction;
var
  LAction: TFileAction;
begin
  LAction.Clear;
  LAction.DisplayLabel := _('Edit source');
  LAction.Hint := _('Edit the file through an external application');
  LAction.ImageIndex := EDIT_FILE;
  LAction.Proc :=
    procedure (AContext: IEditContext; ASelection: TFileNodeHandlerList)
    begin
      if not FileExists(FileName) then
        raise Exception.CreateFmt(_('File %s not found.'), [FileName]);
      EditFile(FileName);
    end;
  FActions.Add(LAction);
end;

procedure TFileNodeHandler.AddEditSourceIntoIDEAction;
var
  LFileExt: string;
  LAction: TFileAction;
begin
  LFileExt := ExtractFileExt(FileName);
  if MatchStr(LFileExt, ['.css', '.js', '.json', '.html', '.htm']) then
  begin
    LAction.Clear;
    LAction.DisplayLabel := _('Modify');
    LAction.Hint := _('Edit file into KIDE');
    if SameText(LFileExt, '.css') then
      LAction.ImageIndex := EDIT_STYLE
    else
      LAction.ImageIndex := EDIT_SCRIPT;
    LAction.Proc :=
      procedure (AContext: IEditContext; ASelection: TFileNodeHandlerList)
      var
        LParams: TEFNode;
      begin
        if not FileExists(FileName) then
          raise Exception.CreateFmt(_('File %s not found.'), [FileName]);
        LParams := TEFNode.Create;
        try
          LParams.SetObject('Object', Self);
          LParams.SetString('ObjectFileName', FileName);
          AContext.OpenEditor(FileName, LParams);
        finally
          FreeAndNil(LParams);
        end;
      end;
    FActions.Add(LAction);
  end
  else if MatchStr(LFileExt, ['.png','.jpg','.gif','.bmp']) then
  begin
    LAction.Clear;
    LAction.DisplayLabel := _('View Image');
    LAction.Hint := _('View image into KIDE');
    LAction.ImageIndex := FILE_IMAGE;
    LAction.Proc :=
      procedure (AContext: IEditContext; ASelection: TFileNodeHandlerList)
      var
        LParams: TEFNode;
      begin
        if not FileExists(FileName) then
          raise Exception.CreateFmt(_('File %s not found.'), [FileName]);
        LParams := TEFNode.Create;
        try
          LParams.SetObject('Object', Self);
          LParams.SetString('ObjectFileName', FileName);
          AContext.OpenEditor(FileName, LParams);
        finally
          FreeAndNil(LParams);
        end;
      end;
    FActions.Add(LAction);
  end;
end;

procedure TFileNodeHandler.AddSeparatorAction;
var
  LAction: TFileAction;
begin
  LAction.Clear;
  LAction.DisplayLabel := '-';
  FActions.Add(LAction);
end;

procedure TFileNodeHandler.AfterConstruction;
begin
  inherited;
  FActions := TList<TFileAction>.Create;
  InitActions;
end;

destructor TFileNodeHandler.Destroy;
begin
  FreeAndNil(FActions);
  inherited;
end;

procedure TFileNodeHandler.ExecuteAction(const AContext: IEditContext;
  const AIndex: Integer; const ASelection: TFileNodeHandlerList);
begin
  FActions[AIndex].Execute(AContext, ASelection);
end;

function TFileNodeHandler.GetActionCount: Integer;
begin
  Result := FActions.Count;
end;

function TFileNodeHandler.GetActionMetadata(const AIndex: Integer): TFileAction;
begin
  Result := FActions[AIndex];
end;

function TFileNodeHandler.GetFileName: string;
begin
  Result := '';
end;

procedure TFileNodeHandler.InitActions;
begin
  AddEditSourceIntoIDEAction;
end;

{ TMetadataFileNodeHandler }

function TMetadataFileNodeHandler.GetMetadata: TKMetadata;
begin
  Result := inherited Tree as TKMetadata;
end;

{ TLayoutFileNodeHandler }

constructor TLayoutFileNodeHandler.Create(const ALayout: TKLayout);
begin
  inherited Create;
  FTree := ALayout;
end;

function TLayoutFileNodeHandler.GetLayout: TKLayout;
begin
  Result := Tree as TKLayout;
end;

procedure TLayoutFileNodeHandler.InitActions;
begin
  inherited;
  AddDesignAction(_('Open Layout in the designer'));

  AddSeparatorAction;

  AddDeleteAction(_('Delete Layout'), DELETE_LAYOUT_PICTURE);

  AddSeparatorAction;

  AddEditSourceAction;

  AddSeparatorAction;

  AddValidateAction(_('Validate Layout'));
end;

{ TFileActionMetadata }

procedure TFileAction.Clear;
begin
  DisplayLabel := '';
  Hint := '';
  ImageIndex := -1;
  EnableOnHomogeneousMultiSelect := False;
  EnableOnHeterogenousMultiSelect := False;
  RefreshAfterExecute := False;
end;

function TFileAction.EnableOnMultiSelect: Boolean;
begin
  Result := EnableOnHomogeneousMultiSelect or EnableOnHeterogenousMultiSelect;
end;

procedure TFileAction.Execute(const AContext: IEditContext;
  const ASelection: TFileNodeHandlerList);
begin
  if Assigned(Proc) then
    Proc(AContext, ASelection);
end;

function TFileAction.IsEnabled(
  const ASelection: TFileNodeHandlerList): Boolean;
begin
  Assert(Assigned(ASelection));

  case ASelection.Count of
    0: Result := False;
    1: Result := True;
    else
    begin
      if ASelection.IsHomogeneous then
        Result := EnableOnMultiSelect
      else
        Result := EnableOnHeterogenousMultiSelect;
    end;
  end;
end;

{ TLocaleFileNodeHandler }

procedure TLocaleFileNodeHandler.InitActions;
var
  LAction: TFileAction;
begin
  inherited;
  LAction.Clear;
  LAction.DisplayLabel := _('Update...');
  LAction.Hint := _('Updates the file with all translatable strings found in metadata files');
  LAction.ImageIndex := 12;
  LAction.Proc :=
    procedure (AContext: IEditContext; ASelection: TFileNodeHandlerList)
    begin
      TUpdateLocaleForm.ShowDialog(FileName);
    end;
  FActions.Add(LAction);

  AddSeparatorAction;
  AddEditSourceAction;
end;

{ TModelsFolderNodeHandler }

procedure TModelsFolderNodeHandler.InitActions;
var
  LAction: TFileAction;
begin
  inherited;
  LAction.Clear;
  LAction.DisplayLabel := _('New Model...');
  LAction.Hint := _('Create a new empty Model');
  LAction.ImageIndex := NEW_MODEL_PICTURE;
  LAction.RefreshAfterExecute := True;
  LAction.Proc :=
    procedure (AContext: IEditContext; ASelection: TFileNodeHandlerList)
    var
      LDialog: TSaveDialog;
      LModel: TKModel;
    begin
      Assert(ASelection.Count = 1);

      LDialog := TSaveDialog.Create(nil);
      try
        LDialog.Title := _('Name the new Model');
        LDialog.InitialDir := (ASelection[0] as TGenericFolderNodeHandler).Path;
        LDialog.Options := [ofOverwritePrompt, ofNoChangeDir, ofPathMustExist, ofEnableSizing];
        LDialog.Filter := _('Yaml files (*.yaml)|*.yaml');
        LDialog.DefaultExt := '.yaml';
        if LDialog.Execute then
        begin
          LModel := TKModels.DefaultModelClassType.Create;
          try
            LModel.PersistentName := ChangeFileExt(ExtractFileName(LDialog.FileName), '');
            LModel.SetString('ModelName', LModel.PersistentName);
            LModel.FieldCount; // Creates the Fields node.
            TProject.CurrentProject.Config.Models.AddObject(LModel);
            TProject.CurrentProject.Config.Models.SaveObject(LModel);
          except
            FreeAndNil(LModel);
            raise;
          end;
        end;
      finally
        FreeAndNil(LDialog);
      end;
    end;
  FActions.Add(LAction);

  AddSeparatorAction;

  LAction.Clear;
  LAction.DisplayLabel := _('Update...');
  LAction.Hint := _('Updates models according to existing database tables');
  LAction.ImageIndex := MODEL_WIZARD_PICTURE;
  LAction.Proc :=
    procedure (AContext: IEditContext; ASelection: TFileNodeHandlerList)
    begin
      TModelWizardForm.ShowDialog(nil);
    end;
  FActions.Add(LAction);

  AddSeparatorAction;
  AddOpenFolderAction;
end;

{ TGenericFolderNodeHandler }

procedure TGenericFolderNodeHandler.AddOpenFolderAction;
var
  LAction: TFileAction;
begin
  LAction.Clear;
  LAction.DisplayLabel := _('Open directory');
  LAction.Hint := _('Open the directory in Windows Explorer');
  LAction.ImageIndex := EDIT_FILE;
  LAction.Proc :=
    procedure (AContext: IEditContext; ASelection: TFileNodeHandlerList)
    begin
      if not DirectoryExists(FileName) then
        raise Exception.CreateFmt(_('Directory %s not found.'), [FileName]);
      EditFile(FileName);
    end;
  FActions.Add(LAction);
end;

{ TFileNodeHandlerList }

function TFileNodeHandlerList.GetIsHomogeneous: Boolean;
var
  LItem: TFileNodeHandler;
  LClassType: TClass;
begin
  Result := Count = 1;
  if Count > 1 then
  begin
    Result := True;
    LClassType := nil;
    for LItem in Self do
    begin
      if LClassType = nil then
        LClassType := LItem.ClassType
      else if LClassType <> LItem.ClassType then
      begin
        Result := False;
        Break;
      end;
    end;
  end;
end;

{ TViewsFolderNodeHandler }

procedure TViewsFolderNodeHandler.ExecuteNewAction(const AContext: IEditContext;
  const AAction: TFileAction; const ASelection: TFileNodeHandlerList);
var
  LDialog: TSaveDialog;
  LView: TKView;
  LProject: TProject;
  I: Integer;
  LModel: TKModel;
  LChildNode, LNode: TEFNode;
begin
  Assert(ASelection.Count = 1);

  LDialog := TSaveDialog.Create(nil);
  try
    LDialog.Title := AAction.DisplayLabel + ' - ' + _('Name the new View');
    LDialog.InitialDir := (ASelection[0] as TGenericFolderNodeHandler).Path;
    LDialog.Options := [ofOverwritePrompt, ofNoChangeDir, ofPathMustExist, ofEnableSizing];
    LDialog.Filter := _('Yaml files (*.yaml)|*.yaml');
    LDialog.DefaultExt := '.yaml';
    if AAction.ImageIndex = TREE_VIEW then
      LDialog.FileName := IncludeTrailingPathDelimiter(LDialog.InitialDir)+'MainMenu.yaml'
    else if AAction.ImageIndex = HOME_VIEW then
      LDialog.FileName := IncludeTrailingPathDelimiter(LDialog.InitialDir)+'Home.yaml';
    if LDialog.Execute then
    begin
      LView := TKView.Create;
      try
        LView.PersistentName := ChangeFileExt(ExtractFileName(LDialog.FileName), '');
        LProject := TProject.CurrentProject;
        if AAction.ImageIndex = TREE_VIEW then
        begin
          LView.SetString('Type', 'Tree');
          LView.SetString('Folder', 'Menu');
          LNode := LView.GetNode('Folder');
          for I := 0 to LProject.Config.Models.ModelCount - 1 do
          begin
            LModel := LProject.Config.Models[I];
            LChildNode := LNode.AddChild('View');
            LChildNode.Value := 'Build AutoList';
            LChildNode := LChildNode.AddChild('Model');
            LChildNode.Value := LModel.ModelName;
          end;
          LNode := LView.AddChild('Folder');
          LNode.Value := 'User';
          LNode.SetBoolean('IsInitiallyCollapsed', True);
          LChildNode := LNode.AddChild('View');
          LChildNode.SetString('Controller', 'ChangePassword');
          LChildNode := LNode.AddChild('View');
          LChildNode.SetString('Controller', 'Logout');
        end
        else if AAction.ImageIndex = HOME_VIEW then
        begin
          LView.LoadFromYamlFile(TKideConfig.Instance.MetadataTemplatePath +'HomeView.yaml');
        end;
        TProject.CurrentProject.Config.Views.AddObject(LView);
        TProject.CurrentProject.Config.Views.SaveObject(LView);
      except
        FreeAndNil(LView);
        raise;
      end;
    end;
  finally
    FreeAndNil(LDialog);
  end;
end;

procedure TViewsFolderNodeHandler.InitActions;
var
  LNewAction, LNewTreeViewAction, LNewHomeViewAction: TFileAction;
begin
  inherited;
  LNewAction.Clear;
  LNewAction.DisplayLabel := _('New View...');
  LNewAction.Hint := _('Create a new empty View');
  LNewAction.ImageIndex := NEW_VIEW_PICTURE;
  LNewAction.RefreshAfterExecute := True;
  LNewAction.Proc :=
    procedure (AContext: IEditContext; ASelection: TFileNodeHandlerList)
    begin
      ExecuteNewAction(AContext, LNewAction, ASelection);
    end;
  FActions.Add(LNewAction);

  LNewTreeViewAction.DisplayLabel := _('New TreeView...');
  LNewTreeViewAction.Hint := _('Create a new empty TreeView for the menu');
  LNewTreeViewAction.ImageIndex := TREE_VIEW;
  LNewTreeViewAction.RefreshAfterExecute := True;
  LNewTreeViewAction.Proc :=
    procedure (AContext: IEditContext; ASelection: TFileNodeHandlerList)
    begin
      ExecuteNewAction(AContext, LNewTreeViewAction, ASelection);
    end;
  FActions.Add(LNewTreeViewAction);

  LNewHomeViewAction.DisplayLabel := _('New HomeView...');
  LNewHomeViewAction.Hint := _('Create a new classic HomeView for the application');
  LNewHomeViewAction.ImageIndex := HOME_VIEW;
  LNewHomeViewAction.RefreshAfterExecute := True;
  LNewHomeViewAction.Proc :=
    procedure (AContext: IEditContext; ASelection: TFileNodeHandlerList)
    begin
      ExecuteNewAction(AContext, LNewHomeViewAction, ASelection);
    end;
  FActions.Add(LNewHomeViewAction);

  AddSeparatorAction;

  AddOpenFolderAction;
end;

{ TLayoutsFolderNodeHandler }

procedure TLayoutsFolderNodeHandler.InitActions;
var
  LAction: TFileAction;
begin
  inherited;
  LAction.Clear;
  LAction.DisplayLabel := _('New Layout...');
  LAction.Hint := _('Create a new empty Layout');
  LAction.ImageIndex := NEW_LAYOUT_PICTURE;
  LAction.RefreshAfterExecute := True;
  LAction.Proc :=
    procedure (AContext: IEditContext; ASelection: TFileNodeHandlerList)
    var
      LDialog: TSaveDialog;
      LLayout: TKLayout;
    begin
      Assert(ASelection.Count = 1);

      LDialog := TSaveDialog.Create(nil);
      try
        LDialog.Title := _('Name the new Layout');
        LDialog.InitialDir := (ASelection[0] as TGenericFolderNodeHandler).Path;
        LDialog.Options := [ofOverwritePrompt, ofNoChangeDir, ofPathMustExist, ofEnableSizing];
        LDialog.Filter := _('Yaml files (*.yaml)|*.yaml');
        LDialog.DefaultExt := '.yaml';
        if LDialog.Execute then
        begin
          LLayout := TKLayout.Create;
          try
            LLayout.PersistentName := ChangeFileExt(ExtractFileName(LDialog.FileName), '');
            TProject.CurrentProject.Config.Views.Layouts.AddObject(LLayout);
            TProject.CurrentProject.Config.Views.Layouts.SaveObject(LLayout);
          except
            FreeAndNil(LLayout);
            raise;
          end;
        end;
      finally
        FreeAndNil(LDialog);
      end;
    end;
  FActions.Add(LAction);

  AddSeparatorAction;

  AddOpenFolderAction;
end;

{ TResourceFolderNodeHandler }

procedure TResourceFolderNodeHandler.InitActions;
begin
  inherited;
  AddOpenFolderAction;
end;

{ TResourceFileNodeHandler }

procedure TResourceFileNodeHandler.InitActions;
begin
  inherited;
  AddEditSourceAction;
end;

{ TConfigFileNodeHandler }

constructor TConfigFileNodeHandler.Create(const AConfig: TKConfig);
begin
  inherited Create;
  FTree := AConfig.Config;
end;

procedure TConfigFileNodeHandler.InitActions;
begin
  inherited;
  AddDesignAction(_('Open Config in the designer'));

  AddSeparatorAction;

  AddDeleteAction(_('Delete Config'), DELETE_CONFIG_PICTURE);

  AddSeparatorAction;

  AddEditSourceAction;

  AddSeparatorAction;

  AddValidateAction(_('Validate Config'));
end;

{ TEFPersistentTreeFileNodeHandler }

procedure TEFPersistentTreeFileNodeHandler.AddDesignAction(const AHint: string);
var
  LAction: TFileAction;
  LTree: TEFPersistentTree;
begin
  LAction.Clear;
  LAction.DisplayLabel := _('Design');
  LAction.Hint := AHint;
  LAction.ImageIndex := DESIGN_PICTURE;
  LAction.EnableOnHomogeneousMultiSelect := True;
  LAction.Proc :=
    procedure (AContext: IEditContext; ASelection: TFileNodeHandlerList)
    begin
      CallPassingTreeList(
        procedure(AList: TList<TEFPersistentTree>)
        var
          I: Integer;
          LParams: TEFNode;
        begin
          for I := 0 to AList.Count - 1 do
          begin
            LParams := TEFNode.Create;
            try
              LTree := AList[I];
              LParams.SetObject('Object', LTree);
              LParams.SetString('ObjectFileName', LTree.PersistentFileName);
              TEFTreeFactory.ReloadFromFile(LTree, LTree.PersistentFileName);
              AContext.OpenEditor(LTree.PersistentFileName, LParams);
            finally
              FreeAndNil(LParams);
            end;
          end;
        end, ASelection);
    end;
  FActions.Add(LAction);
end;

procedure TEFPersistentTreeFileNodeHandler.AddDeleteAction(const AHint: string;
  const AImageIndex: Integer);
var
  LAction: TFileAction;
begin
  LAction.Clear;
  LAction.DisplayLabel := _('Delete');
  LAction.Hint := AHint;
  LAction.ImageIndex := AImageIndex;
  LAction.EnableOnHomogeneousMultiSelect := True;
  LAction.RefreshAfterExecute := True;
  LAction.Proc :=
    procedure (AContext: IEditContext; ASelection: TFileNodeHandlerList)
    begin
      CallPassingTreeList(
        procedure(AList: TList<TEFPersistentTree>)
        var
          I: Integer;
        begin
          if MessageDlg(Format(_('%d selected object(s) will be permanently deleted. Are you sure you want to continue?'), [AList.Count]), mtWarning, [mbYes, mbNo], 0, mbNo) =  mrYes then
          begin
            for I := 0 to AList.Count - 1 do
            begin
              if AList[I] is TKMetadata then
              begin
                TKMetadata(AList[I]).Catalog.MarkObjectAsDisposed(TKMetadata(AList[I]));
                TKMetadata(AList[I]).Catalog.SaveAll;
              end
              else
              begin
                if (AList[I] is TEFComponentConfig) and (TProject.CurrentProject.AppConfigCount = 1) then
                begin
                  MessageDlg(_('Cannot delete a project''s last config file.'), mtError, [mbCancel], 0);
                  Break;
                end;
                if FileExists(AList[I].PersistentFileName) then
                  DeleteFile(AList[I].PersistentFileName);
                if AList[I] is TEFComponentConfig then
                  TProject.CurrentProject.RefreshConfigs;
              end;
            end;
          end;
        end, ASelection);
    end;
  FActions.Add(LAction);
end;

procedure TEFPersistentTreeFileNodeHandler.AddValidateAction(const AHint: string);
var
  LAction: TFileAction;
  LValidator: TTreeValidator;
begin
  LAction.Clear;
  LAction.DisplayLabel := _('Validate');
  LAction.Hint := AHint;
  LAction.ImageIndex := VALIDATE_PICTURE;
  LAction.EnableOnHomogeneousMultiSelect := True;
  LAction.RefreshAfterExecute := True;
  LAction.Proc :=
    procedure (AContext: IEditContext; ASelection: TFileNodeHandlerList)
    begin
      CallPassingTreeList(
        procedure(AList: TList<TEFPersistentTree>)
        var
          I: Integer;
        begin
          for I := 0 to AList.Count - 1 do
          begin
            if AList[I] is TKModel then
            begin
              LValidator := TModelValidator.Create;
              try
                TModelValidator(LValidator).ValidateModels(TKModel(AList[I]));
              finally
                LValidator.Free;
              end;
            end
            else if AList[I] is TKView then
            begin
              LValidator := TViewValidator.Create;
              try
                TViewValidator(LValidator).ValidateViews(TKView(AList[I]));
              finally
                LValidator.Free;
              end;
            end
            else if AList[I] is TEFComponentConfig then
            begin
              LValidator := TConfigValidator.Create;
              try
                TConfigValidator(LValidator).ValidateConfigs(TEFComponentConfig(AList[I]));
              finally
                LValidator.Free;
              end;
            end
            else if AList[I] is TKLayout then
            begin
              LValidator := TLayoutValidator.Create;
              try
                TLayoutValidator(LValidator).ValidateLayouts(TKLayout(AList[I]));
              finally
                LValidator.Free;
              end;
            end;
          end;
        end, ASelection);
    end;
  FActions.Add(LAction);
end;

procedure TEFPersistentTreeFileNodeHandler.CallPassingTreeList(
  const AProc: TProc<TList<TEFPersistentTree>>;
  const ASelection: TFileNodeHandlerList);
var
  LList: TList<TEFPersistentTree>;
  I: Integer;
begin
  LList := TList<TEFPersistentTree>.Create;
  try
    for I := 0 to ASelection.Count - 1 do
      LList.Add((ASelection[I] as TEFPersistentTreeFileNodeHandler).Tree);
    AProc(LList);
  finally
    FreeAndNil(LList);
  end;
end;

function TEFPersistentTreeFileNodeHandler.GetFileName: string;
begin
  if Assigned(FTree) then
    Result := FTree.PersistentFileName
  else
    Result := '';
end;

{ TConfigsFolderNodeHandler }

procedure TConfigsFolderNodeHandler.InitActions;
var
  LAction: TFileAction;
begin
  inherited;
  LAction.Clear;
  LAction.DisplayLabel := _('New Config...');
  LAction.Hint := _('Create a new empty Config');
  LAction.ImageIndex := NEW_CONFIG_PICTURE;
  LAction.RefreshAfterExecute := True;
  LAction.Proc :=
    procedure (AContext: IEditContext; ASelection: TFileNodeHandlerList)
    var
      LDialog: TSaveDialog;
      LConfig: TKConfig;
      LBaseConfigFileName: string;
    begin
      Assert(ASelection.Count = 1);

      LDialog := TSaveDialog.Create(nil);
      try
        LDialog.Title := _('Name the new Config');
        LDialog.InitialDir := (ASelection[0] as TGenericFolderNodeHandler).Path;
        LDialog.Options := [ofOverwritePrompt, ofNoChangeDir, ofPathMustExist, ofEnableSizing];
        LDialog.Filter := _('Yaml files (*.yaml)|*.yaml');
        LDialog.DefaultExt := '.yaml';
        if LDialog.Execute then
        begin
          LBaseConfigFileName := TKConfig.BaseConfigFileName;
          try
            TKConfig.BaseConfigFileName := ChangeFileExt(ExtractFileName(LDialog.FileName), '.yaml');

            LConfig := TKConfig.Create;
            try
              LConfig.Config.SetString('AppTitle', '');
              TEFYAMLWriter.SaveTree(LConfig.Config, LDialog.FileName);
              TProject.CurrentProject.RefreshConfigs;
            except
              FreeAndNil(LConfig);
              raise;
            end;
          finally
            TKConfig.BaseConfigFileName := LBaseConfigFileName;
          end;
        end;
      finally
        FreeAndNil(LDialog);
      end;
    end;
  FActions.Add(LAction);

  AddSeparatorAction;

  AddOpenFolderAction;
end;

end.
