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
unit KIDE.ModelCreator;

interface

uses
  Vcl.Controls,
  SysUtils, Classes, Generics.Collections,
  EF.DB, EF.Tree, Kitto.Config,
  Kitto.Metadata.Models;

type
  TAllowModelEvent = procedure(const ATableName: string; var AAllow: Boolean) of object;

  TModelUpdateOptions = record
  private
    FOnAllowedModel: TAllowModelEvent;
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
    UseViews: Boolean;
    PreservePhisicalNames: Boolean;

    function IsNewModelAllowed(const AName: string): Boolean;
    function IsUpdateModelAllowed(const AName: string): Boolean;
    function IsDeleteModelAllowed(const AName: string): Boolean;
    function IsDetailForeignKey(const AForeignKeyName: string): Boolean;
    function SamePhisicalName(const AName, APhisicalName: string): Boolean;
    property AcceptUdateModel: TAllowModelEvent read FOnAllowedModel write FOnAllowedModel;
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
    FMetadata: TEFTree;
    FModels: TKModels;
    FModel: TKModel;
    FSubActions: TModelUpdateList;
    FIsActive: Boolean;
  private
    function GetMetadata: TEFTree; protected
    FOnLog: TProc<string>;
    property Models: TKModels read FModels;
    procedure DoLog(const AString: string);
  strict protected
    procedure InternalExecute; virtual;
    function GetAsString: string; virtual;
    function GetImageIndex: Integer; virtual;
    procedure InitMetadata; virtual;
    procedure AddSubAction(const AAction: TModelUpdateAction);
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

    procedure DisplayPropertyFrame(const AContainer: TWinControl);
    property Metadata: TEFTree read GetMetadata;
  end;

  ///	<summary>Base class for actions that create and update models.</summary>
  TTableInfoModelUpdateAction = class(TModelUpdateAction)
  strict private
    FOptions: TModelUpdateOptions;
    FTableInfo: TEFDBTableInfo;
    FConnectionName: string;
  strict protected
    function CreateAddUpdateFieldSubActions: Boolean;
    procedure SetPrimaryKey;
    function CreateDeleteFieldSubActions: Boolean;
    function CreateAddUpdateReferenceFieldSubActions: Boolean;
    function CreateDeleteReferenceFieldSubActions: Boolean;
    function CreateAddUpdateDetailSubActions: Boolean;
    function CreateDeleteDetailsSubActions: Boolean;
    procedure InitMetadata; override;
    function GetAsString: string; override;
  public
    property Options: TModelUpdateOptions read FOptions write FOptions;
    property TableInfo: TEFDBTableInfo read FTableInfo;
    property ConnectionName: string read FConnectionName;
    constructor Create(const AConnectionName: string; const AModels: TKModels;
      const ATableInfo: TEFDBTableInfo; const AOptions: TModelUpdateOptions);
  end;

  TAddModel = class(TTableInfoModelUpdateAction)
  strict private
    procedure Process;
  strict protected
    procedure InternalExecute; override;
    function GetImageIndex: Integer; override;
  public
    constructor Create(const AConnectionName: string; const AModels: TKModels;
      const ATableInfo: TEFDBTableInfo; const AOptions: TModelUpdateOptions);
  end;

  TModifyModel = class(TTableInfoModelUpdateAction)
  strict private
    FIsEffective: Boolean;
    procedure Process;
  strict protected
    procedure InternalExecute; override;
    function GetImageIndex: Integer; override;
  public
    function IsEffective: Boolean;
    constructor Create(const AModels: TKModels; const AModel: TKModel;
      const AConnectionName: string; const ATableInfo: TEFDBTableInfo; const AOptions: TModelUpdateOptions);
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
  strict protected
    procedure Process; virtual;
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
    procedure InitMetadata; override;
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
    procedure InitMetadata; override;
  public
    constructor Create(const AModels: TKModels; const AModel: TKModel;
      const ADetailReference: TKModelDetailReference;
      const AForeignKeyInfo: TEFDBForeignKeyInfo);
  end;

  TDeleteDetailReference = class(TDetailReferenceUpdateAction)
  strict protected
    procedure InitMetadata; override;
    procedure InternalExecute; override;
    function GetAsString: string; override;
    function GetImageIndex: Integer; override;
    constructor Create(const AModels: TKModels; const AModel: TKModel;
      const ADetailReference: TKModelDetailReference);
  end;

  ///	<summary>Base class for actions that create and update model fields.</summary>
  TModelFieldUpdateAction = class(TModelUpdateAction)
  strict private
    FField: TKModelField;
  strict protected
    procedure Process; virtual;
  public
    constructor Create(const AModels: TKModels; const AModel: TKModel;
      const AField: TKModelField);
    property Field: TKModelField read FField write FField;
  end;

  TAddField = class(TModelFieldUpdateAction)
  strict private
    FColumnInfo: TEFDBColumnInfo;
    FOptions: TModelUpdateOptions;
  strict protected
    procedure InternalExecute; override;
    function GetAsString: string; override;
    function GetImageIndex: Integer; override;
    procedure InitMetadata; override;
  public
    constructor Create(const AModels: TKModels; const AModel: TKModel;
      const AColumnInfo: TEFDBColumnInfo; const AOptions: TModelUpdateOptions);
  end;

  TModifyField = class(TModelFieldUpdateAction)
  strict private
    FColumnInfo: TEFDBColumnInfo;
    FOptions: TModelUpdateOptions;
  strict protected
    procedure InternalExecute; override;
    function GetAsString: string; override;
    function GetImageIndex: Integer; override;
    procedure InitMetadata; override;
  public
    constructor Create(const AModels: TKModels; const AModel: TKModel;
      const AField: TKModelField; const AColumnInfo: TEFDBColumnInfo;
      const AOptions: TModelUpdateOptions);
  end;

  TDeleteField = class(TModelFieldUpdateAction)
  strict private
    FOptions: TModelUpdateOptions;
  strict protected
    procedure InternalExecute; override;
    function GetAsString: string; override;
    function GetImageIndex: Integer; override;
    constructor Create(const AModels: TKModels; const AModel: TKModel;
      const AField: TKModelField; const AOptions: TModelUpdateOptions);
  end;

  TReferenceFieldUpdateAction = class(TModelFieldUpdateAction)
  strict private
    FOptions: TModelUpdateOptions;
  strict protected
    ///	<summary>Adds to the specified field all subfields listed in the
    ///	specified foreign key info object. Any fields already existing in the
    ///	model, if not already part of the specified reference field, are moved
    ///	inside it.</summary>
    procedure CreateMoveReferenceSubFields(const AField: TKModelField;
      const AForeignKeyInfo: TEFDBForeignKeyInfo; const AOptions: TModelUpdateOptions);
  end;

  TAddReferenceField = class(TReferenceFieldUpdateAction)
  strict private
    FForeignKeyInfo: TEFDBForeignKeyInfo;
    FOptions: TModelUpdateOptions;
  strict protected
    procedure InternalExecute; override;
    function GetAsString: string; override;
    function GetImageIndex: Integer; override;
    procedure InitMetadata; override;
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
  public
    constructor Create(const AModels: TKModels; const AModel: TKModel;
      const AField: TKModelField);
  end;

  TModelCreator = class
  strict private
    FOptions: TModelUpdateOptions;
    FModels: TKModels;
    FDBInfo: TEFDBInfo;
    FConnectionName: string;
    FList: TModelUpdateList;
    FProcessedTableNames: TStringList;
    procedure AddUpdateDeleteModels;
    procedure AddUpdateModel(const AConnectionName: string;
      const ATableInfo: TEFDBTableInfo);
  private
    function IsProcessedTableName(const ATableName: string): Boolean;
  public
    ///	<summary>Diffs the database structure and the specified model catalog,
    ///	and creates a list of proposed modifications to the models based on the
    ///	specified options. The caller is responsible for freeing the
    ///	list.</summary>
    function CreateModelUpdateList(const AConnectionName: string; const AModels: TKModels;
      const ADBInfo: TEFDBInfo; const AOptions: TModelUpdateOptions): TModelUpdateList;
    procedure AfterConstruction; override;
    destructor Destroy; override;
  end;

implementation

uses
  Types, StrUtils,
  EF.StrUtils, EF.Localization,
  KIDE.Project, KIDE.MetadataHelpers, KIDE.ModelUpdateActionFrameUnit;

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

function TModelCreator.CreateModelUpdateList(const AConnectionName: string;
  const AModels: TKModels; const ADBInfo: TEFDBInfo;
  const AOptions: TModelUpdateOptions): TModelUpdateList;
begin
  Assert(AConnectionName <> '');
  Assert(Assigned(AModels));
  Assert(Assigned(ADBInfo));

  FModels := AModels;
  FDBInfo := ADBInfo;
  FOptions := AOptions;

  FList := TModelUpdateList.Create;
  try
    FConnectionName := AConnectionName;
    FDBInfo.InvalidateInfo;
    FDBInfo.ViewsAsTables := FOptions.UseViews;
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
  if FOptions.AddModels or FOptions.UpdateModels then
    for I := 0 to FDBInfo.Schema.TableCount - 1 do
      AddUpdateModel(FConnectionName, FDBInfo.Schema.Tables[I]);

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

procedure TModelCreator.AddUpdateModel(const AConnectionName: string;
  const ATableInfo: TEFDBTableInfo);
var
  LModel: TKModel;
  LModifyModel: TModifyModel;
begin
  Assert(Assigned(ATableInfo));
  Assert(AConnectionName <> '');

  if not IsProcessedTableName(ATableInfo.Name) then
  begin
    FProcessedTableNames.Add(ATableInfo.Name);

    LModel := FModels.FindModelByPhysicalName(ATableInfo.Name);
    if not Assigned(LModel) and FOptions.AddModels then
    begin
      if FOptions.IsNewModelAllowed(ATableInfo.Name) then
        FList.Add(TAddModel.Create(AConnectionName, FModels, ATableInfo, FOptions));
    end
    else if FOptions.UpdateModels then
    begin
      if FOptions.IsUpdateModelAllowed(ATableInfo.Name) then
      begin
        LModifyModel := TModifyModel.Create(FModels, LModel, AConnectionName, ATableInfo, FOptions);
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
end;

{ TModelUpdateOptions }

function TModelUpdateOptions.IsNewModelAllowed(const AName: string): Boolean;
begin
  Result := IsAllowed(AName, ModelNameFilter);
end;

function TModelUpdateOptions.IsUpdateModelAllowed(const AName: string): Boolean;
begin
  Result := False;
  if Assigned(AcceptUdateModel) then
    AcceptUdateModel(AName, Result);
end;

function TModelUpdateOptions.IsDeleteModelAllowed(const AName: string): Boolean;
begin
  Result := False;
  if Assigned(AcceptUdateModel) then
    AcceptUdateModel(AName, Result);
end;

function TModelUpdateOptions.SamePhisicalName(const AName, APhisicalName: string): Boolean;
begin
  Result := not PreservePhisicalNames and SameText(AName, APhisicalName);
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

procedure TModelUpdateAction.AddSubAction(const AAction: TModelUpdateAction);
begin
  Assert(Assigned(AAction));

  AAction.OnLog :=
    procedure (AString: string)
    begin
      DoLog('  ' + AString);
    end;
  SubActions.Add(AAction);
end;

constructor TModelUpdateAction.Create(const AModels: TKModels; const AModel: TKModel);
begin
  Assert(Assigned(AModels));

  inherited Create;
  FSubActions := TModelUpdateList.Create;
  FIsActive := True;
  FModels := AModels;
  FModel := AModel;
end;

destructor TModelUpdateAction.Destroy;
begin
  FreeAndNil(FSubActions);
  FreeAndNil(FMetadata);
  inherited;
end;

procedure TModelUpdateAction.DisplayPropertyFrame(const AContainer: TWinControl);
begin
  Assert(Assigned(AContainer));

  CreateModelUpdateActionFrame(Self, AContainer);
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

function TModelUpdateAction.GetMetadata: TEFTree;
begin
  if not Assigned(FMetadata) then
    FMetadata := TEFTree.Create;
  Result := FMetadata;
end;

procedure TModelUpdateAction.InitMetadata;
begin
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
  InitMetadata;
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
          AddSubAction(TDeleteField.Create(Models, LReferringModel, LReferringModel.Fields[LReferenceIndex]));
      end;
      // Then scan for detail references.
      for LReferenceIndex := LReferringModel.DetailReferenceCount - 1 downto 0 do
      begin
        if LReferringModel.DetailReferences[LReferenceIndex].ReferenceField.Model = Model then
          AddSubAction(TDeleteDetailReference.Create(Models, LReferringModel, LReferringModel.DetailReferences[LReferenceIndex]));
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

procedure TDetailReferenceUpdateAction.Process;
begin
  InitMetadata;
end;

{ TModelFieldUpdateAction }

constructor TModelFieldUpdateAction.Create(const AModels: TKModels;
  const AModel: TKModel; const AField: TKModelField);
begin
  inherited Create(AModels, AModel);
  FField := AField;
end;

procedure TModelFieldUpdateAction.Process;
begin
  InitMetadata;
end;

{ TDeleteDetailReference }

constructor TDeleteDetailReference.Create(const AModels: TKModels;
  const AModel: TKModel; const ADetailReference: TKModelDetailReference);
begin
  inherited Create(AModels, AModel, ADetailReference);
  Process;
end;

function TDeleteDetailReference.GetAsString: string;
begin
  Assert(Assigned(DetailReference));

  Result := DetailReference.Name;
end;

function TDeleteDetailReference.GetImageIndex: Integer;
begin
  Result := 15;
end;

procedure TDeleteDetailReference.InitMetadata;
begin
  inherited;
  Assert(Assigned(DetailReference));

  Metadata.SetString('DetailReferenceName', DetailReference.Name);
  Metadata.SetString('DetailModelName', DetailReference.DetailModelName);
  Metadata.SetString('ForeignKeyName', DetailReference.DBForeignKeyName);
end;

procedure TDeleteDetailReference.InternalExecute;
begin
  inherited;
  Assert(Assigned(DetailReference));

  DoLog(Format(_('Deleting detail reference %s.'), [DetailReference.Name]));
  Model.DeleteDetailReference(DetailReference);
end;

{ TDeleteField }

constructor TDeleteField.Create(const AModels: TKModels; const AModel: TKModel;
  const AField: TKModelField; const AOptions: TModelUpdateOptions);
begin
  inherited Create(AModels, AModel, AField);
  FOptions := AOptions;
  Process;
end;

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

constructor TTableInfoModelUpdateAction.Create(const AConnectionName: string;
  const AModels: TKModels; const ATableInfo: TEFDBTableInfo; const AOptions: TModelUpdateOptions);
begin
  inherited Create(AModels, nil);
  FTableInfo := ATableInfo;
  FConnectionName := AConnectionName;
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
      AddSubAction(TDeleteDetailReference.Create(Models, Model, LDetail));
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
      AddSubAction(TDeleteField.Create(Models, Model, LField));
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
        AddSubAction(TDeleteReferenceField.Create(Models, Model, LField));
        Result := True;
      end;
    end;
  end;
end;

function TTableInfoModelUpdateAction.GetAsString: string;
begin
  Result := Metadata.GetString('ModelName');
end;

procedure TTableInfoModelUpdateAction.InitMetadata;
begin
  inherited;
  Assert(Assigned(TableInfo));

  Metadata.SetString('ModelName', BeautifyName(TableInfo.Name));
  Metadata.SetString('PhysicalName', TableInfo.Name);
  if FConnectionName <> TProject.CurrentProject.Config.DatabaseName then
  begin
    Metadata.SetString('DatabaseRouter', 'Static');
    Metadata.SetString('DatabaseRouter/DatabaseName', FConnectionName);
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
          AddSubAction(TAddDetailReference.Create(Models, Model, LForeignKeyInfo))
        else
        begin
          if not LDetail.EqualsForeignKeyInfo(LForeignKeyInfo) then
            AddSubAction(TModifyDetailReference.Create(Models, Model, LDetail, LForeignKeyInfo));
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
    // Skip fields that are part of foreign keys. They'll be dealt with in the
    // FK pass.
    if not LColumnInfo.IsForeignKey then
    begin
      if Assigned(Model) then
        LModelField := Model.FindFieldByPhysicalName(LColumnInfo.Name)
      else
        LModelField := nil;

      if not Assigned(LModelField) then
      begin
        AddSubAction(TAddField.Create(Models, Model, LColumnInfo, Options));
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
          AddSubAction(TModifyField.Create(Models, Model, LModelField, LColumnInfo, Options));
          Result := True;
        end;
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
      AddSubAction(TAddReferenceField.Create(Models, Model, LForeignKeyInfo))
    else
    begin
      if not LReferenceField.IsReference then
        raise Exception.CreateFmt('Field %s (%s) is not a reference field.',
          [LReferenceField.FieldName, LReferenceField.QualifiedDBColumnName]);
      if not LReferenceField.EqualsForeignKeyInfo(LForeignKeyInfo) then
        AddSubAction(TModifyReferenceField.Create(Models, Model, LReferenceField, LForeignKeyInfo));
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

constructor TAddModel.Create(const AConnectionName: string; const AModels: TKModels;
  const ATableInfo: TEFDBTableInfo; const AOptions: TModelUpdateOptions);
begin
  inherited Create(AConnectionName, AModels, ATableInfo, AOptions);
  Process;
end;

function TAddModel.GetImageIndex: Integer;
begin
  Result := 5;
end;

procedure TAddModel.InternalExecute;
var
  I: Integer;
  LModelName: string;
  LPhysicalName: string;
  LNewNode, LDatabaseRouterNode: TEFNode;
begin
  Assert(Assigned(TableInfo));

  LModelName := Metadata.GetString('ModelName');
  DoLog(Format(_('Adding model %s.'), [LModelName]));
  Model := TKModels.DefaultModelClassType.Create;
  try
    Model.SetModelName(LModelName);
    LPhysicalName := Metadata.GetString('PhysicalName');
    if not Options.SamePhisicalName(LModelName, LPhysicalName) then
      Model.SetString('PhysicalName', LPhysicalName);
    LDatabaseRouterNode := Metadata.FindNode('DatabaseRouter');
    if Assigned(LDatabaseRouterNode) then
    begin
      LNewNode := Model.AddChild('DatabaseRouter');
      LNewNode.Assign(LDatabaseRouterNode);
    end;

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
  InitMetadata;
  CreateAddUpdateFieldSubActions;
  if Options.DeleteFields then
    CreateDeleteFieldSubActions;
  CreateAddUpdateReferenceFieldSubActions;
  if Options.DeleteReferences then
    CreateDeleteReferenceFieldSubActions;
  if Options.AddDetails then
    CreateAddUpdateDetailSubActions;
  if Options.DeleteDetails then
    CreateDeleteDetailsSubActions;
end;

{ TModifyModel }

constructor TModifyModel.Create(const AModels: TKModels; const AModel: TKModel;
  const AConnectionName: string; const ATableInfo: TEFDBTableInfo; const AOptions: TModelUpdateOptions);
begin
  inherited Create(AConnectionName, AModels, ATableInfo, AOptions);
  FIsEffective := False;
  Model := AModel;
  Process;
end;

function TModifyModel.GetImageIndex: Integer;
begin
  Result := 7;
end;

procedure TModifyModel.InternalExecute;
begin
  Assert(Assigned(Model));
  DoLog(Format(_('Modifying model %s.'), [Metadata.GetString('ModelName')]));
  inherited; // execute SubActions.
  Assert(Assigned(TableInfo));

  SetPrimaryKey;
end;

function TModifyModel.IsEffective: Boolean;
begin
  Result := FIsEffective;
end;

procedure TModifyModel.Process;
begin
  InitMetadata;
  FIsEffective := FIsEffective or CreateAddUpdateFieldSubActions;
  if Options.DeleteFields then
    FIsEffective := FIsEffective or CreateDeleteFieldSubActions;
  FIsEffective := FIsEffective or CreateAddUpdateReferenceFieldSubActions;
  if Options.DeleteReferences then
    FIsEffective := FIsEffective or CreateDeleteReferenceFieldSubActions;
end;

{ TAddField }

constructor TAddField.Create(const AModels: TKModels; const AModel: TKModel;
  const AColumnInfo: TEFDBColumnInfo; const AOptions: TModelUpdateOptions);
begin
  Assert(Assigned(AColumnInfo));

  inherited Create(AModels, AModel, nil);
  FColumnInfo := AColumnInfo;
  FOptions := AOptions;
  Process;
end;

function TAddField.GetAsString: string;
begin
  Result := Metadata.GetString('FieldName');
end;

function TAddField.GetImageIndex: Integer;
begin
  Result := 8;
end;

procedure TAddField.InitMetadata;
begin
  inherited;
  Assert(Assigned(FColumnInfo));

  Metadata.SetString('FieldName', BeautifyName(FColumnInfo.Name));
  Metadata.SetString('PhysicalName', FColumnInfo.Name);

  Metadata.SetString('DataType', FColumnInfo.DataType.GetTypeName);
  Metadata.SetInteger('Size', FColumnInfo.Size);
  Metadata.SetInteger('Scale', FColumnInfo.Scale);
  Metadata.SetBoolean('IsRequired', FColumnInfo.IsRequired);
  Metadata.SetBoolean('IsKey', FColumnInfo.IsKey);
end;

procedure TAddField.InternalExecute;
var
  LField: TKModelField;
  LFieldName: string;
begin
  inherited;
  Assert(Assigned(Model));
  Assert(Assigned(FColumnInfo));

  LFieldName := Metadata.GetString('FieldName');

  DoLog(Format(_('Adding field %s to Model %s.'), [LFieldName, Model.ModelName]));
  LField := TKModelField.Create(LFieldName);
  try
    Model.AddField(LField);
    if not FOptions.SamePhisicalName(LFieldName, FColumnInfo.Name) then
      LField.SetString('PhysicalName', FColumnInfo.Name);
    LField.SetFieldSpec(
      Metadata.GetString('DataType'), Metadata.GetInteger('Size'),
      Metadata.GetInteger('Scale'), Metadata.GetBoolean('IsRequired'),
      Metadata.GetBoolean('IsKey'), '');
  except
    FreeAndNil(LField);
    raise;
  end;
end;

{ TModifyField }

constructor TModifyField.Create(const AModels: TKModels; const AModel: TKModel;
  const AField: TKModelField; const AColumnInfo: TEFDBColumnInfo;
  const AOptions: TModelUpdateOptions);
begin
  Assert(Assigned(AColumnInfo));

  inherited Create(AModels, AModel, AField);
  FColumnInfo := AColumnInfo;
  FOptions := AOptions;
  Process;
end;

function TModifyField.GetAsString: string;
begin
  Result := Metadata.GetString('FieldName');
end;

function TModifyField.GetImageIndex: Integer;
begin
  Result := 10;
end;

procedure TModifyField.InitMetadata;
begin
  inherited;
  Assert(Assigned(FColumnInfo));
  Assert(Assigned(Field));

  Metadata.SetString('FieldName', Field.FieldName);
  Metadata.SetString('PhysicalName', FColumnInfo.Name);

  Metadata.SetString('DataType', FColumnInfo.DataType.GetTypeName);
  Metadata.SetInteger('Size', FColumnInfo.Size);
  Metadata.SetInteger('Scale', FColumnInfo.Scale);
  Metadata.SetBoolean('IsRequired', FColumnInfo.IsRequired);
  Metadata.SetBoolean('IsKey', FColumnInfo.IsKey);
end;

procedure TModifyField.InternalExecute;
begin
  inherited;
  Assert(Assigned(Model));
  Assert(Assigned(Field));
  Assert(Assigned(FColumnInfo));

  DoLog(Format(_('Modifying field %s.%s.'), [Model.ModelName, Field.FieldName]));

  Field.SetFieldSpec(
    Metadata.GetString('DataType'), Metadata.GetInteger('Size'),
    Metadata.GetInteger('Scale'), Metadata.GetBoolean('IsRequired'),
    Metadata.GetBoolean('IsKey'), '');
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
  Process;
end;

function TAddReferenceField.GetAsString: string;
begin
  Result := Metadata.GetString('ReferenceName');
end;

function TAddReferenceField.GetImageIndex: Integer;
begin
  Result := 11;
end;

procedure TAddReferenceField.InitMetadata;
var
  LReferencedModel: TKModel;
  LReferenceName: string;

  function GetCountOfForeignKeysReferencingTable: Integer;
  var
    LList: TObjectList<TEFDBForeignKeyInfo>;
  begin
    LList := TObjectList<TEFDBForeignKeyInfo>.Create(False);
    try
      FForeignKeyInfo.TableInfo.GetForeignKeysTo(FForeignKeyInfo.ForeignTableName, LList);
      Result := LList.Count;
    finally
      FreeAndNil(LList);
    end;
  end;

begin
  inherited;
  Assert(Assigned(FForeignKeyInfo));
  Assert(Assigned(Models));

  if FForeignKeyInfo.ColumnCount = 1 then
  begin
    // Try to create a good name for single-field references.
    // The names of the parent and child fields must differ.
    // Use the field name without the trailing 'Id', if there's one.
    // Otherwise add 'Ref' suffix.
    LReferenceName := BeautifyName(FForeignKeyInfo.ColumnNames[0]);
    if EndsStr('Id', LReferenceName) then
      LReferenceName := StripSuffix(LReferenceName, 'Id')
    else
      LReferenceName := LReferenceName + 'Ref';
  end
  else if GetCountOfForeignKeysReferencingTable = 1 then
  begin
    LReferencedModel := Models.FindModelByPhysicalName(FForeignKeyInfo.ForeignTableName);
    if Assigned(LReferencedModel) then
      LReferenceName := LReferencedModel.ModelName
    else
      LReferenceName := BeautifyName(FForeignKeyInfo.ForeignTableName);
  end
  else
    LReferenceName := BeautifyName(FForeignKeyInfo.Name);

  Metadata.SetString('ReferenceName', LReferenceName);
  Metadata.SetString('ForeignKeyName', FForeignKeyInfo.Name);
  Metadata.SetString('ForeignKeyFields', FForeignKeyInfo.ColumnNames.Text);
  Metadata.SetBoolean('IsRequired', FForeignKeyInfo.IsRequired);
end;

procedure TAddReferenceField.InternalExecute;
var
  LFieldName: string;
  LPhysicalName: string;
  LField: TKModelField;
begin
  inherited;
  Assert(Assigned(Model));
  Assert(Assigned(FForeignKeyInfo));

  LFieldName := Metadata.GetString('ReferenceName');
  DoLog(Format(_('Adding reference field %s to Model %s.'), [LFieldName, Model.ModelName]));
  LField := TKModelField.Create(LFieldName);
  try
    Model.AddField(LField);
    LField.SetFieldSpec(TKReferenceDataType.GetTypeName, 0, 0,
      Metadata.GetBoolean('IsRequired'), False,
      BeautifyName(FForeignKeyInfo.ForeignTableName));
    LPhysicalName := Metadata.GetString('ForeignKeyName');
    if not FOptions.SamePhisicalName(LPhysicalName, LFieldName) then
      LField.SetString('PhysicalName', LPhysicalName);
    Field := LField;
  except
    FreeAndNil(LField);
    raise;
  end;
  CreateMoveReferenceSubFields(Field, FForeignKeyInfo, FOptions);
end;

{ TModifyReferenceField }

constructor TModifyReferenceField.Create(const AModels: TKModels;
  const AModel: TKModel; const AReferenceField: TKModelField;
  const AForeignKeyInfo: TEFDBForeignKeyInfo);
begin
  Assert(Assigned(AForeignKeyInfo));

  inherited Create(AModels, AModel, AReferenceField);
  FForeignKeyInfo := AForeignKeyInfo;
  Process;
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

constructor TDeleteReferenceField.Create(const AModels: TKModels;
  const AModel: TKModel; const AField: TKModelField);
begin
  inherited Create(AModels, AModel, AField);
  Process;
end;

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
  const AField: TKModelField; const AForeignKeyInfo: TEFDBForeignKeyInfo;
  const AOptions: TModelUpdateOptions);
var
  I: Integer;
  LField: TKModelField;
  LField2: TKModelField;
begin
  Assert(Assigned(AField));
  Assert(Assigned(AForeignKeyInfo));
  FOptions := AOptions;

  for I := 0 to AForeignKeyInfo.ColumnCount - 1 do
  begin
    LField := Model.FindFieldByPhysicalName(AForeignKeyInfo.ColumnNames[I]);
    if not Assigned(LField) then
    begin
      LField := TKModelField.Create(BeautifyName(AForeignKeyInfo.ColumnNames[I]));
      try
        AField.AddField(LField);
        if not FOptions.SamePhisicalName(LField.FieldName, AForeignKeyInfo.ColumnNames[I]) then
          LField.SetString('PhysicalName', AForeignKeyInfo.ColumnNames[I]);
      except
        FreeAndNil(LField);
        raise;
      end;
    end
    else if LField.ParentField <> AField then
    begin
      LField2 := TKModelField.Clone(LField);
      if not FOptions.SamePhisicalName(LField2.FieldName, AForeignKeyInfo.ColumnNames[I]) then
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
  Process;
end;

function TAddDetailReference.GetAsString: string;
begin
  Result := Metadata.GetString('DetailReferenceName');
end;

function TAddDetailReference.GetImageIndex: Integer;
begin
  Result := 14;
end;

procedure TAddDetailReference.InitMetadata;
var
  LReferencedModel: TKModel;
begin
  inherited;
  Assert(Assigned(FForeignKeyInfo));
  Assert(Assigned(Models));

  LReferencedModel := Models.FindModelByPhysicalName(FForeignKeyInfo.TableInfo.Name);
  if Assigned(LReferencedModel) then
  begin
    Metadata.SetString('DetailReferenceName', Pluralize(LReferencedModel.ModelName));
    Metadata.SetString('DetailModelName', LReferencedModel.ModelName);
  end
  else
  begin
    Metadata.SetString('DetailReferenceName', Pluralize(BeautifyName(FForeignKeyInfo.TableInfo.Name)));
    Metadata.SetString('DetailModelName', BeautifyName(FForeignKeyInfo.TableInfo.Name));
  end;
  Metadata.SetString('ForeignKeyName', FForeignKeyInfo.Name);
end;

procedure TAddDetailReference.InternalExecute;
var
  LDetailReference: TKModelDetailReference;
  LDetailReferenceName: string;
  LDetailModel: TKModel;
  LRefField: TKModelField;
begin
  inherited;
  Assert(Assigned(Model));
  Assert(Assigned(Models));
  Assert(Assigned(FForeignKeyInfo));

  LDetailReferenceName := Metadata.GetString('DetailReferenceName');
  DoLog(Format(_('Adding detail reference %s to Model %s.'), [LDetailReferenceName, Model.ModelName]));
  LDetailModel := Models.FindModelByPhysicalName(FForeignKeyInfo.TableInfo.Name);
  if not Assigned(LDetailModel) then
    DoLog(Format(_('Couldn''t find model for table %s. Skipping...'), [FForeignKeyInfo.TableInfo.Name]))
  else
  begin
    LDetailReference := TKModelDetailReference.Create(LDetailReferenceName, LDetailModel.ModelName);
    try
      LDetailReference.SetString('PhysicalName', FForeignKeyInfo.Name);
      LRefField := LDetailModel.FindReferenceField(FForeignKeyInfo.Name);
      if Assigned(LRefField) then
        LDetailReference.SetString('ReferenceField', LRefField.FieldName)
      else
        DoLog(Format(_('Couldn''t find a reference field in detail model %s for foreign key %s. Detail reference will be created without one.'),
          [LDetailModel.ModelName, FForeignKeyInfo.Name]));
      Model.AddDetailReference(LDetailReference);
    except
      FreeAndNil(LDetailReference);
      raise;
    end;
  end;
end;

{ TModifyDetailReference }

constructor TModifyDetailReference.Create(const AModels: TKModels;
  const AModel: TKModel; const ADetailReference: TKModelDetailReference;
  const AForeignKeyInfo: TEFDBForeignKeyInfo);
begin
  Assert(Assigned(AForeignKeyInfo));

  inherited Create(AModels, AModel, ADetailReference);
  FForeignKeyInfo := AForeignKeyInfo;
  Process;
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

procedure TModifyDetailReference.InitMetadata;
begin
  inherited;
  Assert(Assigned(DetailReference));

  Metadata.SetString('DetailReferenceName', DetailReference.Name);
  Metadata.SetString('DetailModelName', DetailReference.DetailModelName);
  Metadata.SetString('ForeignKeyName', DetailReference.DBForeignKeyName);
end;

procedure TModifyDetailReference.InternalExecute;
begin
  inherited;
  Assert(Assigned(Model));
  Assert(Assigned(DetailReference));
  Assert(Assigned(FForeignKeyInfo));

  DoLog(Format(_('Modifying detail reference %s.'), [DetailReference.Name]));
  DetailReference.Rename(Metadata.GetString('DetailReferenceName'));
end;

end.
