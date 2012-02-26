unit KIDE.ModelCreator;

interface

uses
  SysUtils, Classes, Generics.Collections,
  EF.DB,
  Kitto.Metadata.Models;

type
  TModelUpdateOptions = record
  private
    function IsAllowed(AName, AFilter: string): Boolean;
  public
    AddModels: Boolean;
    ModelNameFilter: string;
    UpdateModels: Boolean;
    DeleteModels: Boolean;
    DeleteFields: Boolean;
    DeleteReferences: Boolean;
    AddDetails: Boolean;
    DetailNameFilter: string;
    DeleteDetails: Boolean;

    function IsNewModelAllowed(const AName: string): Boolean;
    function IsDetailForeignKey(const AForeignKeyName: string): Boolean;
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
    function CreateAddUpdateFieldSubActions: Boolean;
    procedure SetPrimaryKey;
    function CreateDeleteFieldSubActions: Boolean;
    function CreateAddUpdateReferenceFieldSubActions: Boolean;
    function CreateDeleteReferenceFieldSubActions: Boolean;
    function CreateAddUpdateDetailSubActions: Boolean;
    function CreateDeleteDetailsSubActions: Boolean;
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
    FIsEffective: Boolean;
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

  TDetailReferenceUpdateAction = class(TModelUpdateAction)
  strict private
    FDetailReference: TKModelDetailReference;
  public
    constructor Create(const AModels: TKModels; const AModel: TKModel;
      const ADetailReference: TKModelDetailReference);
    property DetailReference: TKModelDetailReference read FDetailReference write FDetailReference;
  end;

  TAddDetailReference = class(TDetailReferenceUpdateAction)
  strict private
    FForeignKeyInfo: TEFDBForeignKeyInfo;
  strict protected
    procedure InternalExecute; override;
    function GetAsString: string; override;
    function GetImageIndex: Integer; override;
  public
    constructor Create(const AModels: TKModels; const AModel: TKModel;
      const AForeignKeyInfo: TEFDBForeignKeyInfo);
  end;

  TModifyDetailReference = class(TDetailReferenceUpdateAction)
  strict private
    FForeignKeyInfo: TEFDBForeignKeyInfo;
  strict protected
    procedure InternalExecute; override;
    function GetAsString: string; override;
    function GetImageIndex: Integer; override;
  public
    constructor Create(const AModels: TKModels; const AModel: TKModel;
      const ADetailReference: TKModelDetailReference;
      const AForeignKeyInfo: TEFDBForeignKeyInfo);
  end;

  TDeleteDetailReference = class(TDetailReferenceUpdateAction)
  strict protected
    procedure InternalExecute; override;
    function GetAsString: string; override;
    function GetImageIndex: Integer; override;
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

  TReferenceFieldUpdateAction = class(TModelFieldUpdateAction)
  strict protected
    ///	<summary>Adds to the specified field all subfields listed in the
    ///	specified foreign key info object. Any fields already existing in the
    ///	model, if not already part of the specified reference field, are moved
    ///	inside it.</summary>
    procedure CreateMoveReferenceSubFields(const AField: TKModelField;
      const AForeignKeyInfo: TEFDBForeignKeyInfo);
  end;

  TAddReferenceField = class(TReferenceFieldUpdateAction)
  strict private
    FForeignKeyInfo: TEFDBForeignKeyInfo;
  strict protected
    procedure InternalExecute; override;
    function GetAsString: string; override;
    function GetImageIndex: Integer; override;
  public
    constructor Create(const AModels: TKModels; const AModel: TKModel;
      const AForeignKeyInfo: TEFDBForeignKeyInfo);
  end;

  TModifyReferenceField = class(TReferenceFieldUpdateAction)
  strict private
    FForeignKeyInfo: TEFDBForeignKeyInfo;
  strict protected
    procedure InternalExecute; override;
    function GetAsString: string; override;
    function GetImageIndex: Integer; override;
  public
    constructor Create(const AModels: TKModels; const AModel: TKModel;
      const AReferenceField: TKModelField; const AForeignKeyInfo: TEFDBForeignKeyInfo);
  end;

  TDeleteReferenceField = class(TReferenceFieldUpdateAction)
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
  EF.StrUtils, EF.Localization, EF.Tree,
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

    LModel := FModels.FindModelByPhysicalName(ATableInfo.Name);
    if not Assigned(LModel) and FOptions.AddModels then
    begin
      if FOptions.IsNewModelAllowed(ATableInfo.Name) then
        FList.Add(TAddModel.Create(FModels, ATableInfo, FOptions));
    end
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
begin
  Result := IsAllowed(AName, ModelNameFilter);
end;

function TModelUpdateOptions.IsAllowed(AName, AFilter: string): Boolean;
var
  LPatterns: TStringDynArray;
  I: Integer;
begin
  Result := False;
  LPatterns := Split(AFilter);
  for I := Low(LPatterns) to High(LPatterns) do
  begin
    if StrMatchesEx(AName, LPatterns[I]) then
    begin
      Result := True;
      Break;
    end;
  end;
end;

function TModelUpdateOptions.IsDetailForeignKey(const AForeignKeyName: string): Boolean;
begin
  Result := IsAllowed(AForeignKeyName, DetailNameFilter);
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

function TDeleteDetailReference.GetAsString: string;
begin
  Assert(Assigned(DetailReference));

  Result := DetailReference.Name;
end;

function TDeleteDetailReference.GetImageIndex: Integer;
begin
  Result := 15;
end;

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

function TTableInfoModelUpdateAction.CreateDeleteDetailsSubActions: Boolean;
var
  I: Integer;
  LDetail: TKModelDetailReference;
  LForeignKeyInfo: TEFDBForeignKeyInfo;
begin
  Assert(Assigned(FTableInfo));

  Result := False;
  for I := Model.DetailReferenceCount - 1 downto 0 do
  begin
    LDetail := Model.DetailReferences[I];
    LForeignKeyInfo := FTableInfo.SchemaInfo.FindForeignKey(LDetail.DBForeignKeyName);
    if not Assigned(LForeignKeyInfo) then
    begin
      SubActions.Add(TDeleteDetailReference.Create(Models, Model, LDetail));
      Result := True;
    end;
  end;
end;

function TTableInfoModelUpdateAction.CreateDeleteFieldSubActions: Boolean;
var
  I: Integer;
  LField: TKModelField;
  LColumnInfo: TEFDBColumnInfo;
begin
  Assert(Assigned(FTableInfo));

  Result := False;
  for I := Model.FieldCount - 1 downto 0 do
  begin
    LField := Model.Fields[I];
    LColumnInfo := FTableInfo.FindColumn(LField.DBColumnName);
    if not Assigned(LColumnInfo) then
    begin
      SubActions.Add(TDeleteField.Create(Models, Model, LField));
      Result := True;
    end;
  end;
end;

function TTableInfoModelUpdateAction.CreateDeleteReferenceFieldSubActions: Boolean;
var
  I: Integer;
  LField: TKModelField;
  LColumnInfo: TEFDBColumnInfo;
begin
  Assert(Assigned(FTableInfo));

  Result := False;
  for I := Model.FieldCount - 1 downto 0 do
  begin
    LField := Model.Fields[I];
    if LField.IsReference then
    begin
      LColumnInfo := FTableInfo.FindColumn(LField.DBColumnName);
      if not Assigned(LColumnInfo) then
      begin
        SubActions.Add(TDeleteReferenceField.Create(Models, Model, LField));
        Result := True;
      end;
    end;
  end;
end;

function TTableInfoModelUpdateAction.CreateAddUpdateDetailSubActions: Boolean;
var
  I: Integer;
  LForeignKeyList: TObjectList<TEFDBForeignKeyInfo>;
  LForeignKeyInfo: TEFDBForeignKeyInfo;
  LDetail: TKModelDetailReference;
begin
  Assert(Assigned(FTableInfo));

  Result := False;
  LForeignKeyList := TObjectList<TEFDBForeignKeyInfo>.Create(False);
  try
    FTableInfo.GetReferencingForeignKeys(LForeignKeyList);
    for I := 0 to LForeignKeyList.Count - 1 do
    begin
      LForeignKeyInfo := LForeignKeyList[I];
      if FOptions.IsDetailForeignKey(LForeignKeyInfo.Name) then
      begin
        if Assigned(Model) then
          LDetail := Model.FindDetailReferenceByPhysicalName(LForeignKeyInfo.Name)
        else
          LDetail := nil;

        if not Assigned(LDetail) then
          SubActions.Add(TAddDetailReference.Create(Models, Model, LForeignKeyInfo))
        else
        begin
          if not LDetail.EqualsForeignKeyInfo(LForeignKeyInfo) then
            SubActions.Add(TModifyDetailReference.Create(Models, Model, LDetail, LForeignKeyInfo));
        end;
      end;
    end;
  finally
    FreeAndNil(LForeignKeyList);
  end;
end;

function TTableInfoModelUpdateAction.CreateAddUpdateFieldSubActions: Boolean;
var
  I: Integer;
  LColumnInfo: TEFDBColumnInfo;
  LModelField: TKModelField;
begin
  Assert(Assigned(FTableInfo));

  Result := False;
  for I := 0 to FTableInfo.ColumnCount - 1 do
  begin
    LColumnInfo := FTableInfo.Columns[I];
    if Assigned(Model) then
      LModelField := Model.FindFieldByPhysicalName(LColumnInfo.Name)
    else
      LModelField := nil;

    if not Assigned(LModelField) then
    begin
      SubActions.Add(TAddField.Create(Models, Model, LColumnInfo));
      Result := True;
    end
    else
    begin
      // Contained fields are not matched here, because getting their
      // datatype and size requires looking at the reference table which might
      // not exists ATM. Contained (reference) fields are accounted for when
      // processing foreign keys instead.
      if not LModelField.IsContained and not LModelField.EqualsColumnInfo(LColumnInfo) then
      begin
        SubActions.Add(TModifyField.Create(Models, Model, LModelField, LColumnInfo));
        Result := True;
      end;
    end;
  end;
end;

function TTableInfoModelUpdateAction.CreateAddUpdateReferenceFieldSubActions: Boolean;
var
  I: Integer;
  LForeignKeyInfo: TEFDBForeignKeyInfo;
  LReferenceField: TKModelField;
begin
  Assert(Assigned(FTableInfo));

  Result := False;
  for I := 0 to FTableInfo.ForeignKeyCount - 1 do
  begin
    LForeignKeyInfo := FTableInfo.ForeignKeys[I];
    if Assigned(Model) then
      LReferenceField := Model.FindFieldByPhysicalName(LForeignKeyInfo.Name)
    else
      LReferenceField := nil;

    if not Assigned(LReferenceField) then
      SubActions.Add(TAddReferenceField.Create(Models, Model, LForeignKeyInfo))
    else
    begin
      if not LReferenceField.IsReference then
        raise Exception.CreateFmt('Field %s (%s) is not a reference field.',
          [LReferenceField.FieldName, LReferenceField.QualifiedDBColumnName]);
      if not LReferenceField.EqualsForeignKeyInfo(LForeignKeyInfo) then
        SubActions.Add(TModifyReferenceField.Create(Models, Model, LReferenceField, LForeignKeyInfo));
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
    if LModelName <> FTableInfo.Name then
      Model.SetString('PhysicalName', FTableInfo.Name);
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
  CreateAddUpdateReferenceFieldSubActions;
  if FOptions.DeleteReferences then
    CreateDeleteReferenceFieldSubActions;
  if FOptions.AddDetails then
    CreateAddUpdateDetailSubActions;
  if FOptions.DeleteDetails then
    CreateDeleteDetailsSubActions;
end;

{ TModifyModel }

constructor TModifyModel.Create(const AModels: TKModels; const AModel: TKModel;
  const ATableInfo: TEFDBTableInfo; const AOptions: TModelUpdateOptions);
begin
  inherited Create(AModels, ATableInfo, AOptions);
  FIsEffective := False;
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
  Result := FIsEffective;
end;

procedure TModifyModel.Process;
begin
  FIsEffective := FIsEffective and CreateAddUpdateFieldSubActions;
  if FOptions.DeleteFields then
    FIsEffective := FIsEffective and CreateDeleteFieldSubActions;
  FIsEffective := FIsEffective and CreateAddUpdateReferenceFieldSubActions;
  if FOptions.DeleteReferences then
    FIsEffective := FIsEffective and CreateDeleteReferenceFieldSubActions;
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
    if LFieldName <> FColumnInfo.Name then
      LField.SetString('PhysicalName', FColumnInfo.Name);
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

{ TAddReferenceField }

constructor TAddReferenceField.Create(const AModels: TKModels;
  const AModel: TKModel; const AForeignKeyInfo: TEFDBForeignKeyInfo);
begin
  Assert(Assigned(AForeignKeyInfo));

  inherited Create(AModels, AModel, nil);
  FForeignKeyInfo := AForeignKeyInfo;
end;

function TAddReferenceField.GetAsString: string;
begin
  Assert(Assigned(FForeignKeyInfo));

  Result := BeautifyName(FForeignKeyInfo.Name);
end;

function TAddReferenceField.GetImageIndex: Integer;
begin
  Result := 11;
end;

procedure TAddReferenceField.InternalExecute;
begin
  inherited;
  Assert(Assigned(Model));
  Assert(Assigned(FForeignKeyInfo));

  Field := TKModelField.Create(BeautifyName(FForeignKeyInfo.Name));
  Field.SetFieldSpec(TEFDataTypeFactory.Instance.GetDataType(TKReferenceDataType.GetTypeName),
    0, FForeignKeyInfo.IsRequired, False, BeautifyName(FForeignKeyInfo.ForeignTableName));
  Model.AddField(Field);
  CreateMoveReferenceSubFields(Field, FForeignKeyInfo);
end;

{ TModifyReferenceField }

constructor TModifyReferenceField.Create(const AModels: TKModels;
  const AModel: TKModel; const AReferenceField: TKModelField;
  const AForeignKeyInfo: TEFDBForeignKeyInfo);
begin
  Assert(Assigned(AForeignKeyInfo));

  inherited Create(AModels, AModel, AReferenceField);
  FForeignKeyInfo := AForeignKeyInfo;
end;

function TModifyReferenceField.GetAsString: string;
begin
  Assert(Assigned(Field));

  Result := Field.FieldName;
end;

function TModifyReferenceField.GetImageIndex: Integer;
begin
  Result := 13;
end;

procedure TModifyReferenceField.InternalExecute;
begin
  inherited;
  raise Exception.Create('Modifying reference fields not yet implemented.');
end;

{ TDeleteReferenceField }

function TDeleteReferenceField.GetAsString: string;
begin
  Assert(Assigned(Field));

  Result := Field.FieldName;
end;

function TDeleteReferenceField.GetImageIndex: Integer;
begin
  Result := 12;
end;

procedure TDeleteReferenceField.InternalExecute;
begin
  inherited;
  Assert(Assigned(Model));
  Assert(Assigned(Field));

  Model.DeleteField(Field);
end;

{ TReferenceFieldUpdateAction }

procedure TReferenceFieldUpdateAction.CreateMoveReferenceSubFields(
  const AField: TKModelField; const AForeignKeyInfo: TEFDBForeignKeyInfo);
var
  I: Integer;
  LField: TKModelField;
  LField2: TKModelField;
begin
  Assert(Assigned(AField));
  Assert(Assigned(AForeignKeyInfo));

  for I := 0 to AForeignKeyInfo.ColumnCount - 1 do
  begin
    LField := Model.FindField(AForeignKeyInfo.ColumnNames[I]);
    if not Assigned(LField) then
    begin
      LField := TKModelField.Create(BeautifyName(AForeignKeyInfo.ColumnNames[I]));
      LField.SetString('PhysicalName', AForeignKeyInfo.ColumnNames[I]);
      AField.AddField(LField);
    end
    else if LField.ParentField <> AField then
    begin
      LField2 := TKModelField.Clone(LField);
      LField2.SetString('PhysicalName', AForeignKeyInfo.ColumnNames[I]);
      AField.AddField(LField2);
      if Assigned(LField.ParentField) then
        LField.ParentField.DeleteField(LField)
      else
        Model.DeleteField(LField);
    end;
  end;
end;

{ TAddDetailReference }

constructor TAddDetailReference.Create(const AModels: TKModels;
  const AModel: TKModel; const AForeignKeyInfo: TEFDBForeignKeyInfo);
begin
  Assert(Assigned(AForeignKeyInfo));

  inherited Create(AModels, AModel, nil);
  FForeignKeyInfo := AForeignKeyInfo;
end;

function TAddDetailReference.GetAsString: string;
begin
  Assert(Assigned(FForeignKeyInfo));

  Result := BeautifyName(FForeignKeyInfo.Name);
end;

function TAddDetailReference.GetImageIndex: Integer;
begin
  Result := 14;
end;

procedure TAddDetailReference.InternalExecute;
begin
  inherited;

end;

{ TModifyDetailReference }

constructor TModifyDetailReference.Create(const AModels: TKModels;
  const AModel: TKModel; const ADetailReference: TKModelDetailReference;
  const AForeignKeyInfo: TEFDBForeignKeyInfo);
begin
  Assert(Assigned(AForeignKeyInfo));

  inherited Create(AModels, AModel, ADetailReference);
  FForeignKeyInfo := AForeignKeyInfo;
end;

function TModifyDetailReference.GetAsString: string;
begin
  Assert(Assigned(DetailReference));

  Result := DetailReference.Name;
end;

function TModifyDetailReference.GetImageIndex: Integer;
begin
  Result := 16;
end;

procedure TModifyDetailReference.InternalExecute;
begin
  inherited;

end;

end.
