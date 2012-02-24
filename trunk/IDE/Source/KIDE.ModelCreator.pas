unit KIDE.ModelCreator;

interface

uses
  SysUtils, Classes, Generics.Collections,
  EF.DB,
  Kitto.Metadata.Models;

type
  TModelUpdateOptions = record
    AddModels: Boolean;
    ModelNameFilter: string;
    UpdateModels: Boolean;
    DeleteModels: Boolean;
    DeleteFields: Boolean;

    function IsNewModelAllowed(const AName: string): Boolean;
  end;

  TModelUpdateAction = class;

  TModelUpdateList = class(TObjectList<TModelUpdateAction>)
  strict private
    FOnLog: TProc<string>;
    procedure DoLog(const AString: string);
  public
    property OnLog: TProc<string> read FOnLog write FOnLog;
    procedure Execute;
  end;

  TModelUpdateAction = class
  strict private
    FModels: TKModels;
    FModel: TKModel;
    FSubActions: TModelUpdateList;
    FIsActive: Boolean; protected
    FOnLog: TProc<string>;
    property Models: TKModels read FModels;
    procedure DoLog(const AString: string);
  strict protected
    procedure InternalExecute; virtual;
    function GetAsString: string; virtual;
    function GetImageIndex: Integer; virtual;
  public
    constructor Create(const AModels: TKModels; const AModel: TKModel);

    destructor Destroy; override;
    property Model: TKModel read FModel write FModel;
    property SubActions: TModelUpdateList read FSubActions;

    property AsString: string read GetAsString;
    property ImageIndex: Integer read GetImageIndex;
    property IsActive: Boolean read FIsActive write  FIsActive;
    property OnLog: TProc<string> read FOnLog write FOnLog;
    procedure Execute;
  end;

  ///	<summary>Base class for actions that create and update models.</summary>
  TTableInfoModelUpdateAction = class(TModelUpdateAction)
  strict protected
    FOptions: TModelUpdateOptions;
    FTableInfo: TEFDBTableInfo;
    procedure CreateAddUpdateFieldSubActions;
    procedure SetPrimaryKey;
    procedure CreateDeleteFieldSubActions;
    //procedure ApplyForeignKeys(const ATableInfo: TEFDBTableInfo; const AOptions: TModelUpdateOptions);
  public
    constructor Create(const AModels: TKModels; const ATableInfo: TEFDBTableInfo;
      const AOptions: TModelUpdateOptions);
  end;

  TAddModel = class(TTableInfoModelUpdateAction)
  strict private
    procedure Process;
  strict protected
    procedure InternalExecute; override;
    function GetAsString: string; override;
    function GetImageIndex: Integer; override;
  public
    constructor Create(const AModels: TKModels; const ATableInfo: TEFDBTableInfo;
      const AOptions: TModelUpdateOptions);
  end;

  TModifyModel = class(TTableInfoModelUpdateAction)
  strict private
    procedure Process;
  strict protected
    function GetAsString: string; override;
    procedure InternalExecute; override;
    function GetImageIndex: Integer; override;
  public
    function IsEffective: Boolean;
    constructor Create(const AModels: TKModels; const AModel: TKModel;
      const ATableInfo: TEFDBTableInfo; const AOptions: TModelUpdateOptions);
  end;

  TDeleteModel = class(TModelUpdateAction)
  strict private
    procedure Process;
  strict protected
    procedure InternalExecute; override;
    function GetAsString: string; override;
    constructor Create(const AModels: TKModels; const AModel: TKModel);
    function GetImageIndex: Integer; override;
  end;

  (*
  { TODO -cV2 : put logic inside DeleteModel action? }
  TDeleteReferringReferences = class(TModelUpdateAction)
  public
    procedure AfterConstruction; override;
  end;
  *)

  TDetailReferenceUpdateAction = class(TModelUpdateAction)
  strict private
    FDetailReference: TKModelDetailReference;
  public
    constructor Create(const AModels: TKModels; const AModel: TKModel;
      const ADetailReference: TKModelDetailReference);
    property DetailReference: TKModelDetailReference read FDetailReference write FDetailReference;
  end;

  TDeleteDetailReference = class(TDetailReferenceUpdateAction)
  strict protected
    procedure InternalExecute; override;
  end;

  ///	<summary>Base class for actions that create and update model fields.</summary>
  TModelFieldUpdateAction = class(TModelUpdateAction)
  strict private
    FField: TKModelField;
  public
    constructor Create(const AModels: TKModels; const AModel: TKModel;
      const AField: TKModelField);
    property Field: TKModelField read FField write FField;
  end;

  TAddField = class(TModelFieldUpdateAction)
  strict private
    FColumnInfo: TEFDBColumnInfo;
  strict protected
    procedure InternalExecute; override;
    function GetAsString: string; override;
    function GetImageIndex: Integer; override;
  public
    constructor Create(const AModels: TKModels; const AModel: TKModel;
      const AColumnInfo: TEFDBColumnInfo);
  end;

  TModifyField = class(TModelFieldUpdateAction)
  strict private
    FColumnInfo: TEFDBColumnInfo;
  strict protected
    procedure InternalExecute; override;
    function GetAsString: string; override;
    function GetImageIndex: Integer; override;
  public
    constructor Create(const AModels: TKModels; const AModel: TKModel;
      const AField: TKModelField; const AColumnInfo: TEFDBColumnInfo);
  end;

  TDeleteField = class(TModelFieldUpdateAction)
  strict protected
    procedure InternalExecute; override;
    function GetAsString: string; override;
    function GetImageIndex: Integer; override;
  end;

  TModelCreator = class
  strict private
    FOptions: TModelUpdateOptions;
    FModels: TKModels;
    FDBInfo: TEFDBInfo;
    FList: TModelUpdateList;
    FProcessedTableNames: TStringList;
    procedure AddUpdateDeleteModels;
    procedure AddUpdateModel(const ATableInfo: TEFDBTableInfo);
  private
    function IsProcessedTableName(const ATableName: string): Boolean;
  public
    ///	<summary>Diffs the database structure and the specified model catalog,
    ///	and creates a list of proposed modifications to the models based on the
    ///	specified options. The caller is responsible for freeing the
    ///	list.</summary>
    function CreateModelUpdateList(const AModels: TKModels;
      const ADBInfo: TEFDBInfo; const AOptions: TModelUpdateOptions): TModelUpdateList;
    procedure AfterConstruction; override;
    destructor Destroy; override;
  end;

implementation

uses
  Types,
  EF.StrUtils, EF.Localization,
  KIDE.MetadataHelpers;

function BeautifyName(const AName: string): string;
begin
  { TODO : allow to customize the beautifying function }
  Result := AName;
  if (Result = UpperCase(Result)) or (Pos('_', Result) > 0) then
    Result := UpperUnderscoreToCamel(Result);
end;

{ TModelCreator }

destructor TModelCreator.Destroy;
begin
  FreeAndNil(FProcessedTableNames);
  inherited;
end;

procedure TModelCreator.AfterConstruction;
begin
  inherited;
  FProcessedTableNames := TStringList.Create;
  FProcessedTableNames.CaseSensitive := False;
end;

function TModelCreator.CreateModelUpdateList(const AModels: TKModels;
  const ADBInfo: TEFDBInfo; const AOptions: TModelUpdateOptions): TModelUpdateList;
begin
  Assert(Assigned(AModels));
  Assert(Assigned(ADBInfo));

  FModels := AModels;
  FDBInfo := ADBInfo;
  FOptions := AOptions;

  FList := TModelUpdateList.Create;
  try
    FDBInfo.InvalidateInfo;
    AddUpdateDeleteModels;
    Result := FList;
  except
    FreeAndNil(FList);
    raise;
  end;
end;

procedure TModelCreator.AddUpdateDeleteModels;
var
  I: Integer;
  LModel: TKModel;
begin
  Assert(Assigned(FModels));
  Assert(Assigned(FList));
  Assert(Assigned(FDBInfo));
  Assert(Assigned(FDBInfo.Schema));

  FProcessedTableNames.Clear;

  // Add and update models based on table info objects.
  if FOptions.AddModels or FOptions.UpdateModels  then
    for I := 0 to FDBInfo.Schema.TableCount - 1 do
      AddUpdateModel(FDBInfo.Schema.Tables[I]);

  // Delete no longer existing models.
  if FOptions.DeleteModels then
  begin
    for I := 0 to FModels.ModelCount - 1 do
    begin
      LModel := FModels[I];
      if not Assigned(FDBInfo.Schema.FindTable(LModel.DBTableName)) then
      begin
        // Before deleting the table, we should also delete any references
        // pointing to it. This is required in order to delete the table.
        //FList.Add(TDeleteReferringReferences.Create(FModels, LModel));
        FList.Add(TDeleteModel.Create(FModels, LModel));
      end;
    end;
  end;
end;

function TModelCreator.IsProcessedTableName(const ATableName: string): Boolean;
begin
  Result := FProcessedTableNames.IndexOf(ATableName) >= 0;
end;

procedure TModelCreator.AddUpdateModel(const ATableInfo: TEFDBTableInfo);
var
  LModel: TKModel;
  LModifyModel: TModifyModel;
begin
  Assert(Assigned(ATableInfo));

  if not IsProcessedTableName(ATableInfo.Name) then
  begin
    FProcessedTableNames.Add(ATableInfo.Name);

    LModel := FModels.FindModel(ATableInfo.Name);
    if not Assigned(LModel) and FOptions.AddModels and FOptions.IsNewModelAllowed(ATableInfo.Name) then
      FList.Add(TAddModel.Create(FModels, ATableInfo, FOptions))
    else if FOptions.UpdateModels then
    begin
      LModifyModel := TModifyModel.Create(FModels, LModel, ATableInfo, FOptions);
      try
        if LModifyModel.IsEffective then
          FList.Add(LModifyModel)
        else
          FreeAndNil(LModifyModel);
      except
        FreeAndNil(LModifyModel);
        raise;
      end;
    end;
  end;
end;

{ TModelUpdateOptions }

function TModelUpdateOptions.IsNewModelAllowed(const AName: string): Boolean;
var
  LPatterns: TStringDynArray;
  I: Integer;
begin
  Result := False;
  LPatterns := Split(ModelNameFilter);
  for I := Low(LPatterns) to High(LPatterns) do
  begin
    if StrMatchesEx(AName, LPatterns[I]) then
    begin
      Result := True;
      Break;
    end;
  end;
end;

{ TModelUpdateAction }

constructor TModelUpdateAction.Create(const AModels: TKModels; const AModel: TKModel);
begin
  Assert(Assigned(AModels));

  inherited Create;
  FIsActive := True;
  FSubActions := TModelUpdateList.Create;
  FModels := AModels;
  FModel := AModel;
end;

destructor TModelUpdateAction.Destroy;
begin
  FreeAndNil(FSubActions);
  inherited;
end;

procedure TModelUpdateAction.DoLog(const AString: string);
begin
  if Assigned(FOnLog) then
    FOnLog(AString);
end;

procedure TModelUpdateAction.Execute;
var
  LAction: TModelUpdateAction;
begin
  if FIsActive then
  begin
    InternalExecute;
    for LAction in FSubActions do
      LAction.Execute;
  end
  else
    DoLog(_('Skipping action'));
end;

function TModelUpdateAction.GetAsString: string;
begin
  Result := _('<unknown>');
end;

function TModelUpdateAction.GetImageIndex: Integer;
begin
  Result := 4;
end;

procedure TModelUpdateAction.InternalExecute;
begin
end;

{ TDeleteModel }

constructor TDeleteModel.Create(const AModels: TKModels; const AModel: TKModel);
begin
  inherited Create(AModels, AModel);
  Process;
end;

function TDeleteModel.GetAsString: string;
begin
  Assert(Assigned(Model));

  Result := Model.ModelName;
end;

function TDeleteModel.GetImageIndex: Integer;
begin
  Result := 6;
end;

procedure TDeleteModel.InternalExecute;
begin
  inherited;
  Assert(Assigned(Model));
  Assert(Assigned(Models));

  DoLog(Format(_('Deleting model %s.'), [Model.ModelName]));
  Models.MarkObjectAsDisposed(Model);
end;

procedure TDeleteModel.Process;
begin
end;

{ TDeleteReferringReferences }

(*
procedure TDeleteReferringReferences.AfterConstruction;
var
  LReferenceIndex: Integer;
  LModelIndex: Integer;
  LReferringModel: TKModel;
begin
  inherited;
  Assert(Assigned(Model));
  Assert(Assigned(Models));

  for LModelIndex := 0 to Models.ModelCount - 1 do
  begin
    LReferringModel := Models[LModelIndex];
    if LReferringModel <> Model then
    begin
      // First scan for reference fields.
      for LReferenceIndex := LReferringModel.FieldCount - 1 downto 0 do
      begin
        if LReferringModel.Fields[LReferenceIndex].ReferencedModel = Model then
          SubActions.Add(TDeleteField.Create(Models, LReferringModel, LReferringModel.Fields[LReferenceIndex]));
      end;
      // Then scan for detail references.
      for LReferenceIndex := LReferringModel.DetailReferenceCount - 1 downto 0 do
      begin
        if LReferringModel.DetailReferences[LReferenceIndex].ReferenceField.Model = Model then
          SubActions.Add(TDeleteDetailReference.Create(Models, LReferringModel, LReferringModel.DetailReferences[LReferenceIndex]));
      end;
    end;
  end;
end;
*)

{ TDetailReferenceUpdateAction }

constructor TDetailReferenceUpdateAction.Create(const AModels: TKModels;
  const AModel: TKModel; const ADetailReference: TKModelDetailReference);
begin
  Assert(Assigned(ADetailReference));

  inherited Create(AModels, AModel);
  FDetailReference := ADetailReference;
end;

{ TModelFieldUpdateAction }

constructor TModelFieldUpdateAction.Create(const AModels: TKModels;
  const AModel: TKModel; const AField: TKModelField);
begin
  inherited Create(AModels, AModel);
  FField := AField;
end;

{ TDeleteDetailReference }

procedure TDeleteDetailReference.InternalExecute;
begin
  inherited;
  Assert(Assigned(DetailReference));

  Model.DeleteDetailReference(DetailReference);
end;

{ TDeleteField }

function TDeleteField.GetAsString: string;
begin
  Assert(Assigned(Field));

  Result := Field.FieldName;
end;

function TDeleteField.GetImageIndex: Integer;
begin
  Result := 9;
end;

procedure TDeleteField.InternalExecute;
begin
  inherited;
  Assert(Assigned(Model));
  Assert(Assigned(Field));

  Model.DeleteField(Field);
end;

{ TTableInfoModelUpdateAction }

(*
procedure TTableInfoModelUpdateAction.ApplyForeignKeys(
  const ATableInfo: TEFDBTableInfo; const AOptions: TModelUpdateOptions);
var
  I: Integer;
  LForeignKey: TEFDBForeignKeyInfo;
  LReferenceField: TKModelField;

  function FindReferenceField: TKModelField;
  begin
    Result := Model.FindReferenceField(LForeignKey.Name);
    if not Assigned(Result) then
      Result := Model.FindReferenceField(LForeignKey.ForeignTableName,
        Split(LForeignKey.ColumnNames.Text, sLineBreak));
  end;

begin
  // Add new foreign keys and update existing ones.
  for I := 0 to ATableInfo.ForeignKeyCount - 1 do
  begin
    LForeignKey := ATableInfo.ForeignKeys[I];
    LReferenceField := FindReferenceField;
    if not Assigned(LReferenceField) then
    begin
      LReferenceField := Model.AddReferenceField(
      // Avoid logging the "updating foreign key" line.
      AOptions.DisableLogging;
    end;
    LReferenceField.AssignForeignKeyInfo(LForeignKey, AOptions);
    AOptions.EnableLogging;
  end;

  // Delete no longer existing foreign keys.
  if AOptions.DeleteForeignKeys then
  begin
    for I := ForeignKeyCount - 1 downto 0 do
    begin
      if not Assigned(ATableInfo.FindForeignKey(ForeignKeys[I].Name)) then
      begin
        AOptions.LogFmt(Self, _('Deleting Foreign Key %s.%s (-> %s)'),
          [Name, ForeignKeys[I].Name, ForeignKeys[I].ForeignTableName]);
        DeleteForeignKey(I);
      end;
    end;
  end;
end;
*)

constructor TTableInfoModelUpdateAction.Create(const AModels: TKModels;
  const ATableInfo: TEFDBTableInfo; const AOptions: TModelUpdateOptions);
begin
  inherited Create(AModels, nil);
  FTableInfo := ATableInfo;
  FOptions := AOptions;
end;

procedure TTableInfoModelUpdateAction.CreateDeleteFieldSubActions;
var
  I: Integer;
  LField: TKModelField;
  LColumnInfo: TEFDBColumnInfo;
begin
  Assert(Assigned(FTableInfo));

  for I := Model.FieldCount - 1 downto 0 do
  begin
    LField := Model.Fields[I];
    LColumnInfo := FTableInfo.FindColumn(LField.Name);
    if not Assigned(LColumnInfo) then
      SubActions.Add(TDeleteField.Create(Models, Model, LField));
  end;
end;

procedure TTableInfoModelUpdateAction.CreateAddUpdateFieldSubActions;
var
  I: Integer;
  LColumnInfo: TEFDBColumnInfo;
  LModelField: TKModelField;
begin
  Assert(Assigned(FTableInfo));

  for I := 0 to FTableInfo.ColumnCount - 1 do
  begin
    LColumnInfo := FTableInfo.Columns[I];
    if Assigned(Model) then
      LModelField := Model.FindFieldByPhysicalName(LColumnInfo.Name)
    else
      LModelField := nil;

    if not Assigned(LModelField) then
      SubActions.Add(TAddField.Create(Models, Model, LColumnInfo))
    else
    begin
      // Contained fields are not matched here, because getting their
      // datatype and size requires looking at the reference table which might
      // not exists ATM. Contained (reference) fields are accounted for when
      // processing foreign keys instead.
      if not LModelField.IsContained and not LModelField.EqualsColumnInfo(LColumnInfo) then
        SubActions.Add(TModifyField.Create(Models, Model, LModelField, LColumnInfo));
    end;
  end;
end;

procedure TTableInfoModelUpdateAction.SetPrimaryKey;
var
  I: Integer;
begin
  Assert(Assigned(FTableInfo.PrimaryKey));

  for I := 0 to Model.FieldCount - 1 do
    Model.Fields[I].SetIsKey(FTableInfo.PrimaryKey.ColumnNames.IndexOf(Model.Fields[I].FieldName) >= 0);
end;

{ TAddModel }

constructor TAddModel.Create(const AModels: TKModels;
  const ATableInfo: TEFDBTableInfo; const AOptions: TModelUpdateOptions);
begin
  inherited Create(AModels, ATableInfo, AOptions);
  Process;
end;

function TAddModel.GetAsString: string;
begin
  Assert(Assigned(FTableInfo));

  Result := BeautifyName(FTableInfo.Name);
end;

function TAddModel.GetImageIndex: Integer;
begin
  Result := 5;
end;

procedure TAddModel.InternalExecute;
var
  I: Integer;
  LModelName: string;
begin
  Assert(Assigned(FTableInfo));

  LModelName := BeautifyName(FTableInfo.Name);
  DoLog(Format(_('Adding model %s.'), [LModelName]));
  Model := TKModel.Create;
  try
    Model.SetModelName(LModelName);
    Model.SetString('PhysicalTableName', FTableInfo.Name);
    // Pass newly created model to subactions.
    for I := 0 to SubActions.Count - 1 do
      SubActions[I].Model := Model;
    inherited; // execute SubActions.
    SetPrimaryKey;
    Models.AddObject(Model);
  except
    Model.Free;
    Model := nil;
    raise;
  end;
end;

procedure TAddModel.Process;
begin
  CreateAddUpdateFieldSubActions;
  if FOptions.DeleteFields then
    CreateDeleteFieldSubActions;
  //ApplyForeignKeys(ATableInfo, AOptions);
end;

{ TModifyModel }

constructor TModifyModel.Create(const AModels: TKModels; const AModel: TKModel;
  const ATableInfo: TEFDBTableInfo; const AOptions: TModelUpdateOptions);
begin
  inherited Create(AModels, ATableInfo, AOptions);
  Model := AModel;
  Process;
end;

function TModifyModel.GetAsString: string;
begin
  Assert(Assigned(Model));

  Result := Model.ModelName;
end;

function TModifyModel.GetImageIndex: Integer;
begin
  Result := 7;
end;

procedure TModifyModel.InternalExecute;
begin
  Assert(Assigned(Model));
  DoLog(Format(_('Modifying model %s.'), [Model.ModelName]));
  inherited; // execute SubActions.
  Assert(Assigned(FTableInfo));

  SetPrimaryKey;
end;

function TModifyModel.IsEffective: Boolean;
begin
{ TODO :
return True if any changes (either in sub-actions or the main action)
were generated in ProcessTableInfo, otherwise False. }
  Result := True;
end;

procedure TModifyModel.Process;
begin
  CreateAddUpdateFieldSubActions;

  if FOptions.DeleteFields then
    CreateDeleteFieldSubActions;
end;

{ TAddField }

constructor TAddField.Create(const AModels: TKModels; const AModel: TKModel;
  const AColumnInfo: TEFDBColumnInfo);
begin
  Assert(Assigned(AColumnInfo));

  inherited Create(AModels, AModel, nil);
  FColumnInfo := AColumnInfo;
end;

function TAddField.GetAsString: string;
begin
  Assert(Assigned(FColumnInfo));

  Result := BeautifyName(FColumnInfo.Name);
end;

function TAddField.GetImageIndex: Integer;
begin
  Result := 8;
end;

procedure TAddField.InternalExecute;
var
  LField: TKModelField;
  LFieldName: string;
begin
  inherited;
  Assert(Assigned(Model));
  Assert(Assigned(FColumnInfo));

  LFieldName := BeautifyName(FColumnInfo.Name);
  DoLog(Format(_('Adding field %s to Model %s.'), [LFieldName, Model.ModelName]));
  LField := TKModelField.Create(LFieldName);
  try
    LField.SetString('PhysicalColumnName', FColumnInfo.Name);
    LField.SetFieldSpec(FColumnInfo.DataType, FColumnInfo.Size,
      FColumnInfo.IsRequired, FColumnInfo.IsKey, '');
    Model.AddField(LField);
  except
    FreeAndNil(LField);
    raise;
  end;
end;

{ TModifyField }

constructor TModifyField.Create(const AModels: TKModels; const AModel: TKModel;
  const AField: TKModelField; const AColumnInfo: TEFDBColumnInfo);
begin
  Assert(Assigned(AColumnInfo));

  inherited Create(AModels, AModel, AField);
  FColumnInfo := AColumnInfo;
end;

function TModifyField.GetAsString: string;
begin
  Assert(Assigned(Field));

  Result := Field.FieldName;
end;

function TModifyField.GetImageIndex: Integer;
begin
  Result := 10;
end;

procedure TModifyField.InternalExecute;
begin
  inherited;
  Assert(Assigned(Model));
  Assert(Assigned(Field));
  Assert(Assigned(FColumnInfo));

  DoLog(Format(_('Modifying field %s.%s.'), [Model.ModelName, Field.FieldName]));
  Field.SetFieldSpec(FColumnInfo.DataType, FColumnInfo.Size,
    FColumnInfo.IsRequired, FColumnInfo.IsKey, '');
end;

{ TModelUpdateList }

procedure TModelUpdateList.DoLog(const AString: string);
begin
  if Assigned(FOnLog) then
    FOnLog(AString);
end;

procedure TModelUpdateList.Execute;
var
  LAction: TModelUpdateAction;
begin
  for LAction in ToArray do
  begin
    LAction.OnLog :=
      procedure (AString: string)
      begin
        DoLog(AString);
      end;
    LAction.Execute;
  end;
  DoLog(_('Model update complete. Click Finish to save the changes.'));
end;

end.
