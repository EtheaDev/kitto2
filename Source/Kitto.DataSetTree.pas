///	<summary>
///	  A dataset tree used to store master/detail data and manage updates, plus
///	  related classes and services. The dataset tree manages data retrieval
///	  from a database connection based on settings collected from a view table;
///	  data caching; creation of update statements.
///	</summary>
unit Kitto.DataSetTree;

{$I Kitto.Defines.inc}

interface

uses
  Classes, Provider, Contnrs, DB, DBClient,
  EF.Classes, EF.Data, EF.Tree,
  Kitto.Metadata.Views, Kitto.SQL;

type
  TKDataSetProvider = class(TDataSetProvider)
  end;

  {
    Base class for concrete master and detail datasets used in the GUI as
    data store.
  }
  TKDataSet = class(TClientDataSet)
  private
    FProvider: TKDataSetProvider;
    FDBQuery: IEFDBQuery;
    FDBConnection: IEFDBConnection;
    FDataViewTable: TKViewTable;
    FReconcileErrorMessage: string;
    function RecreateProvider: TKDataSetProvider;
    procedure RecreateDBQuery(const ACommandText: string);
    function GetDBQueryCommandText: string;
    procedure SetDBQueryCommandText(const AValue: string);
    function GetInternalDataSet: TDataSet;
    procedure InternalDataSetAfterOpen(DataSet: TDataSet);
    procedure BooleanGetText(Sender: TField; var Text: string;
      DisplayText: Boolean);
    procedure BooleanSetText(Sender: TField; const Text: string);
    function BooleanToStringFieldValue(const ABoolean: Boolean): string;
    procedure DecorateFields;
    function StringToBooleanFieldValue(const AString: string): Boolean;
    procedure CopyInternalDataSetProviderFlags;
    procedure FireEvent(const AEventName, AEventParams: string;
      const AField: TField = nil);
    procedure SelfReconcileError(DataSet: TCustomClientDataSet;
      E: EReconcileError; UpdateKind: TUpdateKind;
      var Action: TReconcileAction);
    procedure DateTimeGetText(Sender: TField; var Text: string;
      DisplayText: Boolean);
    procedure SetDataViewTable(const AValue: TKViewTable);
  protected
    procedure SetDBConnection(const AValue: IEFDBConnection); virtual;
    procedure DoAfterOpen; override;
    procedure DoOnNewRecord; override;
    procedure DoBeforeInsert; override;
    procedure DoAfterInsert; override;
    procedure DoBeforeEdit; override;
    procedure DoAfterEdit; override;
    procedure DoBeforePost; override;
    procedure DoAfterPost; override;
    procedure DoBeforeDelete; override;
    procedure DoAfterDelete; override;
    {
      Fired before an event is fired. Inherited classes may pass additional
      params.
    }
    procedure CustomizeEventParams(const AParams: TEFNode); virtual;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
    {
      A reference to the database connection to be used for database access.
      Set this property before setting CommandText.
    }
    property DBConnection: IEFDBConnection read FDBConnection write SetDBConnection;

    ///	<summary>
    ///	  A reference to the data view table for which the dataset is
    ///	  constructed. Useful in order to set some dataset field properties
    ///	  according to the data view fields.
    ///	</summary>
    property DataViewTable: TKViewTable read FDataViewTable write SetDataViewTable;
    {
      The query that retrieves the data. Set this property before opening the
      dataset.
    }
    property DBQueryCommandText: string read GetDBQueryCommandText
      write SetDBQueryCommandText;
    {
      Opens the dataset, fetches the data and the closes the underlying query
      in order to save resources and allow to disconnect from the database in
      a briefcase model.
    }
    procedure OpenDataSetAndCloseQuery; virtual;
    {
      Wraps a call to OpenDataSetAndCloseQuery in a transaction.
    }
    procedure OpenDataSetAndCloseQueryInTransaction;
    {
      Accesses the internal query dataset. To be deprecated.
    }
    property InternalDataSet: TDataSet read GetInternalDataSet;
    {
      Automatically builds and sets DBQueryCommandText based on DataViewTable.
      DataViewTable must be set before calling this method.
    }
    procedure BuildCommandTextFromDataViewTable(
      const AKeySet: TEFNode = nil;
      const AAdditionalFilter: string = ''); virtual;
    {
      Accesses the internal query. Currently used only to set param values
      before the query is opened.
    }
    property DBQuery: IEFDBQuery read FDBQuery;
    {
      Writes all pending changes in the dataset to the database.
      Pass True in AInserting if you are creating a new record, False
      if you are editing an existing record.
    }
    procedure WriteChanges(const AInserting: Boolean); virtual;
  end;

  TKDetailDataSet = class;
  TKMasterDataSet = class;

  {
    Stores the structure (field name mappings, not data) of a master/detail
    relationship and offers related services.
  }
  TKDetailDataSetKey = class
  private
    FMasterFieldNames: TStrings;
    FDetailFieldNames: TStrings;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
    {
      Resets MasterFieldNames and DetailFieldNames.
    }
    procedure Reset;
    {
      Ordered list of master field names in the key.
    }
    property MasterFieldNames: TStrings read FMasterFieldNames;
    {
      Ordered list of detail field names in the key. Each Nth field name matches
      the Nth field name in MasterFieldNames.
    }
    property DetailFieldNames: TStrings read FDetailFieldNames;
    {
      Returns True if the specified detail dataset is actually a detail of
      the specified master dataset's current record.
    }
    function Matches(const ADetailDataSet: TKDetailDataSet;
      const AMasterDataSet: TDataSet): Boolean;
    {
      Copies all key values from the current record of a master
      dataset to a destination detail data packet. The destination uses
      *detail* field names.
    }
    procedure CopyValues(const AMasterSource: TDataSet;
      const ADetailDestination: TEFNode); overload;
    {
      Copies all key values from the current record of a master
      dataset to the current record of a detail dataset.
      The destination uses *detail* field names.
    }
    procedure CopyValues(const AMasterSource: TDataSet;
      const ADetailDestination: TDataSet); overload;
    {
      Returns a parameterized SQL predicate that can be appended to a detail
      dataset SQL statement to select only the detail records of a particular
      master record.
    }
    function GetDetailSQLPredicate(const ADetailTable, AMasterTable: TKViewTable): string;
  end;

  {
    A list of homogeneous detail datasets, one for each record of the same
    master dataset. Each list represents a detail form of the parent master
    GUI form.
  }
  TKDetailDataSetList = class(TComponent)
  private
    FDataSets: TObjectList;
    FKey: TKDetailDataSetKey;
    FViewTable: TKViewTable;
    function GetDataSet(const AIndex: Integer): TKDetailDataSet;
    function GetDataSetCount: Integer;
    procedure SetViewTable(const AValue: TKViewTable);
    procedure UpdateKeyFromDataViewTables;
    function CreateDetailDataSet: TKDetailDataSet;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
    property DataSetCount: Integer read GetDataSetCount;
    property DataSets[const AIndex: Integer]: TKDetailDataSet read GetDataSet;
    {
      Returns the detail dataset corresponding to the master key whose values
      are read from the specified dataset's current record. If the detail
      dataset does not exist, it is created on demand.
    }
    function GetDataSetByMasterKey(const AMasterDataSet: TDataSet): TKDetailDataSet;
    {
      Defines the link with the master dataset in terms of field name mappings.
      It is applied to all detail datasets in the list.
    }
    property Key: TKDetailDataSetKey read FKey;
    {
      A reference to the GUI form. Should be set upon object creation.
    }
    property ViewTable: TKViewTable read FViewTable write SetViewTable;
  end;

  {
    A dataset that may have details.
  }
  TKMasterDataSet = class(TKDataSet)
  private
    FDetailDataSetLists: TObjectList;
    function GetDetailDataSetListCount: Integer;
    function GetDetailDataSetList(const AIndex: Integer): TKDetailDataSetList;
    procedure FreeDetailDataSetLists;
  protected
    procedure SetDBConnection(const AValue: IEFDBConnection); override;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
    property DetailDataSetLists[const AIndex: Integer]: TKDetailDataSetList read GetDetailDataSetList;
    property DetailDataSetListCount: Integer read GetDetailDataSetListCount;
    {
      Resets all lists of detail datasets. Any pending changes are lost.
    }
    procedure RecreateDetailDataSetLists;
    {
      Returns a reference to the detail dataset list represented by the
      specified view table. Raises an exception if no matching list is found.
    }
    function DetailDataSetListByViewTable(const AViewTable: TKViewTable): TKDetailDataSetList;
    {
      Returns a reference to the detail dataset list represented by the
      GUI form that has the specified MainTableName.
      Raises an exception if no matching list is found.
    }
    //function DetailDataSetListByTableName(const ATableName: string): TKDetailDataSetList;
    {
      Writes all pending changes in the details and master datasets to the database.
    }
    procedure WriteChanges(const AInserting: Boolean); override;
  end;

  {
    A dataset that is a detail and may also have its own subdetails.
  }
  TKDetailDataSet = class(TKMasterDataSet)
  private
    FMasterKeyValues: TEFNode;
    FMasterKey: TKDetailDataSetKey;
    function GetMasterDataSet: TKMasterDataSet;
  protected
    procedure DoOnNewRecord; override;
    procedure CustomizeEventParams(const AParams: TEFNode); override;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
    procedure BuildCommandTextFromDataViewTable(
      const AKeySet: TEFNode = nil;
      const AAdditionalFilter: string = ''); override;
    procedure OpenDataSetAndCloseQuery; override;
    {
      The master record key values. If the dataset is not empty, these values
      should be the same as those found on each dataset record.
    }
    property MasterKeyValues: TEFNode
      read FMasterKeyValues write FMasterKeyValues;
    {
      A reference to an externally managed object that contains a description
      of the binding with the master dataset. Should be set as soon as possible
      after creating the object.
    }
    property MasterKey: TKDetailDataSetKey read FMasterKey write FMasterKey;
    {
      Returns a reference to the master dataset.
    }
    property MasterDataSet: TKMasterDataSet read GetMasterDataSet;
  end;

  {
    Manages a tree of related datasets, based on a tree of GUI forms,
    as a whole.
  }
  TKDataSetTree = class(TEFComponent)
  private
    FDBQueryBuilder: TKSQLQueryBuilder;
    FMasterDataSet: TKMasterDataSet;
    FDataViewTable: TKViewTable;
    FDBConnection: IEFDBConnection;
    function GetMasterDataSet: TKMasterDataSet;
    procedure RecreateMasterDataSet;
    procedure SetDBConnection(const AValue: IEFDBConnection);
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
    {
      A database connection to be used for database operations. Must be supplied
      before doing anything with the object.
    }
    property DBConnection: IEFDBConnection read FDBConnection write SetDBConnection;
    {
      Discards any data and pending updates; destroys and recreates
      all internal objects. Pass nil in AGUIForm to reset the object without
      recreating anything. Optionally specify AAdditionalFilter and/or AKeySet
      to build a filtered query (for example when you need to select a single
      record).
    }
    procedure Reset(const ADataViewTable: TKViewTable = nil;
      const AAdditionalFilter: string = '';
      const AKeySet: TEFNode = nil);
    {
      Accesses the master data set. Only access this property after calling
      Reset at least once.
    }
    property MasterDataSet: TKMasterDataSet read GetMasterDataSet;
    {
      Writes all pending changes in the tree to the database.
      Pass True in AInsertingMasterRecord if you are applying the creation
      of a new master record; False if you are editing an existing record.
    }
    procedure WriteChanges(const AInsertingMasterRecord: Boolean);
  end;

implementation

uses
  SysUtils, Variants,
  EF.Intf, EF.Localization, EF.DB.Utils, EF.Types, EF.SQL,
  Kitto.Metadata.Models, Kitto.Environment, Kitto.AccessControl;

const
  MAX_RECORDS_DEFAULT = 10000;
  MASTER_DATASET_NAME = 'Master';
  KEY_VALUE_PARAM_PREFIX = 'P_';
  MASTER_PARAM_PREFIX = 'M_';

function KeySetToSQL(const AKeySet: TEFNode): string;
var
  LIndex: Integer;
begin
  if not Assigned(AKeySet) or (AKeySet.ChildCount = 0) then
  begin
    Result := '';
    Exit;
  end;

  Result := '(';
  for LIndex := 0 to AKeySet.ChildCount - 1 do
  begin
    Result := Result + '(' + AKeySet.Children[LIndex].Name + '= :'
      + KEY_VALUE_PARAM_PREFIX + IntToStr(LIndex) + ')';
    if LIndex <> AKeySet.ChildCount - 1 then
      Result := Result + ' and ';
  end;
  Result := Result + ')';
end;

procedure CopyKeySetValuesToParams(const ASourceKeySet: TEFNode;
  const ADestinationParams: TParams);
var
  LIndex: Integer;
begin
  for LIndex := 0 to ASourceKeySet.ChildCount - 1 do
    AssignNodeValueToParam(ASourceKeySet.Children[LIndex],
      ADestinationParams.ParamByName(KEY_VALUE_PARAM_PREFIX + IntToStr(LIndex)));
end;

{ TKDataSet }

procedure TKDataSet.AfterConstruction;
begin
  inherited;
  PacketRecords := MAX_RECORDS_DEFAULT;
  FetchOnDemand := False;
  SetProvider(RecreateProvider);
end;

procedure TKDataSet.SelfReconcileError(
  DataSet: TCustomClientDataSet; E: EReconcileError; UpdateKind: TUpdateKind;
  var Action: TReconcileAction);
begin
  FReconcileErrorMessage := E.Message;
  Action := raAbort;
end;

function TKDataSet.RecreateProvider: TKDataSetProvider;
begin
  FreeAndNil(FProvider);

  FProvider := TKDataSetProvider.Create(Self);
  try
    FProvider.Options := [poNoReset];
    FProvider.UpdateMode := upWhereKeyOnly;
    Result := FProvider;
  except
    FreeAndNil(FProvider);
    raise;
  end;
end;

procedure TKDataSet.SetDBQueryCommandText(const AValue: string);
begin
  RecreateDBQuery(AValue);
end;

procedure TKDataSet.SetDataViewTable(const AValue: TKViewTable);
begin
  FDataViewTable := AValue;
  if Assigned(FDataViewTable) then
    PacketRecords := FDataViewTable.GetInteger('MaxRecords', PacketRecords);
end;

procedure TKDataSet.SetDBConnection(const AValue: IEFDBConnection);
begin
  FDBConnection := AValue;
end;

procedure TKDataSet.RecreateDBQuery(const ACommandText: string);
begin
  Assert(Assigned(FDBConnection));
  Assert(Assigned(FProvider));

  Close;
  FreeAndNilEFIntf(FDBQuery);

  FDBQuery := FDBConnection.CreateDBQuery;
  try
    FDBQuery.CommandText := ACommandText;
    FDBQuery.DataSet.AfterOpen := InternalDataSetAfterOpen;
    FProvider.DataSet := FDBQuery.DataSet;
  except
    FreeAndNilEFIntf(FDBQuery);
    raise;
  end;
end;

procedure TKDataSet.InternalDataSetAfterOpen(DataSet: TDataSet);
var
  LDataViewField: TKViewField;
  LFieldIndex: Integer;
  LFieldName: string;
  LField: TField;
begin
  Assert(Assigned(FDataViewTable));

  for LFieldIndex := 0 to FDataViewTable.FieldCount - 1 do
  begin
    LDataViewField := FDataViewTable.Fields[LFieldIndex];
    LFieldName := LDataViewField.AliasedName;
    LField := DataSet.FindField(LFieldName);
    if LField <> nil then
    begin
      if LField.Origin = '' then
        LField.Origin := LDataViewField.QualifiedName;

      // Fields that are not part of the main table shouldn't be updated.
      if LDataViewField.ModelName <> FDataViewTable.ModelName then
        LField.ProviderFlags := LField.ProviderFlags - [pfInUpdate]
      else
      begin
        // Mark primary key fields.
        if LDataViewField.ModelField.IsKey then
          LField.ProviderFlags := LField.ProviderFlags + [pfInKey];
        // Exclude autoinc and other generated fields from the update.
        if LDataViewField.ModelField.IsGenerated then
          LField.ProviderFlags := LField.ProviderFlags - [pfInUpdate];
      end;
    end;
  end;
end;

destructor TKDataSet.Destroy;
begin
  FreeAndNil(FProvider);
  FreeAndNilEFIntf(FDBQuery);
  inherited;
end;

procedure TKDataSet.DoAfterDelete;
begin
  inherited;
  FireEvent('AfterDelete', '');
end;

procedure TKDataSet.DoAfterEdit;
var
  LFieldIndex: Integer;
begin
  inherited;
  FireEvent('AfterEdit', '');
  for LFieldIndex := 0 to FieldCount - 1 do
    FireEvent('AfterEdit', Fields[LFieldIndex].FieldName, Fields[LFieldIndex]);
end;

procedure TKDataSet.DoAfterInsert;
var
  LFieldIndex: Integer;
begin
  inherited;
  FireEvent('AfterInsert', '');
  for LFieldIndex := 0 to FieldCount - 1 do
    FireEvent('AfterInsert', Fields[LFieldIndex].FieldName, Fields[LFieldIndex]);
end;

procedure TKDataSet.DoAfterOpen;
begin
  DecorateFields;
  inherited;
end;

procedure TKDataSet.DoAfterPost;
begin
  inherited;
  FireEvent('AfterPost', '');
end;

procedure TKDataSet.DoBeforeDelete;
begin
  inherited;
  FireEvent('BeforeDelete', '');
end;

procedure TKDataSet.DoBeforeEdit;
begin
  inherited;
  FireEvent('BeforeEdit', '');
end;

procedure TKDataSet.DoBeforeInsert;
begin
  inherited;
  FireEvent('BeforeInsert', '');
end;

procedure TKDataSet.DoBeforePost;

  function IsEmpty(const AField: TField): Boolean;
  begin
    Result := not AField.IsNull
      and ((AField.AsString = '') or ((AField.DataType in [ftDate, ftTime, ftDateTime]) and (AField.AsDateTime = 0)));
  end;

  procedure SetEmptyFieldAsNull(const AField: TField);
  var
    LDataViewField: TKViewField;
    LOnChange: TFieldNotifyEvent;
  begin
    if IsEmpty(AField) then
    begin
      LDataViewField := FDataViewTable.FieldByAliasedName(AField.FieldName);
      if LDataViewField.EmptyAsNull then
      begin
        LOnChange := AField.OnChange;
        try
          AField.Clear;
        finally
          AField.OnChange := LOnChange;
        end;
      end;
    end;
  end;

var
  I: Integer;
begin
  inherited;
  FireEvent('BeforePost', '');

  Assert(Assigned(FDataViewTable));
  for I := 0 to FieldCount - 1 do
    SetEmptyFieldAsNull(Fields[I]);
end;

procedure TKDataSet.DoOnNewRecord;
var
  LGUIFieldIndex: Integer;
  LDataViewField: TKViewField;
begin
  inherited;
  Assert(Assigned(FDataViewTable));

  for LGUIFieldIndex := 0 to FDataViewTable.FieldCount - 1 do
  begin
    LDataViewField := FDataViewTable.Fields[LGUIFieldIndex];
    if not VarIsNull(LDataViewField.DefaultValue) then
      FieldByName(LDataViewField.AliasedName).Value :=
        Environment.MacroExpansionEngine.Expand(LDataViewField.DefaultValue);
  end;
end;

function TKDataSet.GetDBQueryCommandText: string;
begin
  Result := FDBQuery.CommandText;
end;

function TKDataSet.GetInternalDataSet: TDataSet;
begin
  Assert(Assigned(FDBQuery));

  Result := FDBQuery.DataSet;
end;

procedure TKDataSet.OpenDataSetAndCloseQuery;
begin
  if Active then
    Close;
  if FDBQuery.IsOpen then
    FDBQuery.Close;
  FDBQuery.Open;
  SetProvider(FProvider);
  Open;
  CopyInternalDataSetProviderFlags;
  FDBQuery.Close;
end;

procedure TKDataSet.OpenDataSetAndCloseQueryInTransaction;
begin
  DBConnection.StartTransaction;
  try
    OpenDataSetAndCloseQuery;
    DBConnection.CommitTransaction;
  except
    DBConnection.RollbackTransaction;
    raise;
  end;
end;

procedure TKDataSet.CopyInternalDataSetProviderFlags;
var
  I: Integer;
begin
  for I := 0 to FDBQuery.DataSet.FieldCount - 1 do
    FieldByName(FDBQuery.DataSet.Fields[I].FieldName).ProviderFlags :=
      FDBQuery.DataSet.Fields[I].ProviderFlags;
end;

procedure TKDataSet.CustomizeEventParams(
  const AParams: TEFNode);
begin
end;

procedure TKDataSet.DecorateFields;
var
  LFieldIndex: Integer;
  LFieldName: string;
  LDataViewField: TKViewField;
  LField: TField;
begin
  Assert(Assigned(FDataViewTable));

  for LFieldIndex := 0 to FDataViewTable.FieldCount - 1 do
  begin
    // Get a reference to the data view field and dataset field.
    LDataViewField := FDataViewTable.Fields[LFieldIndex];
    LFieldName := LDataViewField.AliasedName;
    LField := FieldByName(LFieldName);

    if LField.Origin = '' then
      LField.Origin := LDataViewField.QualifiedName;

    // Fields that are not part of the main table shouldn't be updated.
    if LDataViewField.ModelName <> FDataViewTable.ModelName then
      LField.ProviderFlags := LField.ProviderFlags - [pfInUpdate]
    // Mark primary key fields.
    else
    begin
      if LDataViewField.ModelField.IsKey then
        LField.ProviderFlags := LField.ProviderFlags + [pfInKey];
      // Exclude autoinc and other generated fields from the update.
      if LDataViewField.ModelField.IsGenerated then
      begin
        LField.ProviderFlags := LField.ProviderFlags - [pfInUpdate];
        // Auto-inc fields should be marked as read-only, so that
        // TDataSet.CheckRequiredFields doesn't fail when posting a record without
        // a value for an auto-inc field.
        LField.ReadOnly := True;
      end;
    end;

    LField.DisplayLabel := _(LDataViewField.DisplayLabel);
    LField.Visible := LDataViewField.IsVisible;
      { TODO : implement }
      //and Environment.IsAccessGranted(LDataViewField.GetResourceURI, ACM_READ);
    LField.Required := LDataViewField.IsRequired;

    if LDataViewField.DataType = edtBoolean then
    begin
      LField.OnGetText := BooleanGetText;
      LField.OnSetText := BooleanSetText;
    end
    else if LDataViewField.DataType in [edtDate, edtDateTime, edtTime] then
      LField.OnGetText := DateTimeGetText;
  end;
end;

procedure TKDataSet.BooleanGetText(Sender: TField;
  var Text: string; DisplayText: Boolean);
begin
  Text := BooleanToStringFieldValue(Sender.AsBoolean);
end;

procedure TKDataSet.BooleanSetText(Sender: TField;
  const Text: string);
begin
  Sender.AsBoolean := StringToBooleanFieldValue(Text);
end;

function TKDataSet.StringToBooleanFieldValue(
  const AString: string): Boolean;
begin
  Result := StrToBool(AString);
end;

type
  TBreakDateTimeField = class(TDateTimeField);

procedure TKDataSet.DateTimeGetText(Sender: TField;
  var Text: string; DisplayText: Boolean);
var
  LDataViewField: TKViewField;
begin
  Assert(Assigned(FDataViewTable));

  if Sender.AsDateTime <= 0 then
  begin
    LDataViewField := FDataViewTable.FieldByAliasedName(Sender.FieldName);
    if LDataViewField.EmptyAsNull then
      Text := ''
    else
      TBreakDateTimeField(Sender).GetText(Text, DisplayText)
  end
  else
    TBreakDateTimeField(Sender).GetText(Text, DisplayText);
end;

procedure TKDataSet.WriteChanges(const AInserting: Boolean);
var
  LLocalMessage: string;
  LOldReconcileErrorHandler: TReconcileErrorEvent;
begin
  LOldReconcileErrorHandler := OnReconcileError;
  try
    OnReconcileError := SelfReconcileError;

    if Active then
    begin
      if State in dsEditModes then
        Post;
      ApplyUpdates(0);
      if FReconcileErrorMessage <> '' then
      begin
        LLocalMessage := FReconcileErrorMessage;
        FReconcileErrorMessage := '';
        raise EEFError.Create(LLocalMessage);
      end;
    end;
  finally
    OnReconcileError := LOldReconcileErrorHandler;
  end;
end;

function TKDataSet.BooleanToStringFieldValue(
  const ABoolean: Boolean): string;
begin
  Result := BoolToStr(ABoolean, True);
end;

procedure TKDataSet.BuildCommandTextFromDataViewTable(
  const AKeySet: TEFNode; const AAdditionalFilter: string);
var
  LQueryBuilder: TKSQLQueryBuilder;
  LDBQueryCommandText: string;
begin
  Assert(Assigned(FDataViewTable));

  LQueryBuilder := TKSQLQueryBuilder.Create;
  try
    LQueryBuilder.AddFromDataViewTable(FDataViewTable, MASTER_DATASET_NAME);
    LDBQueryCommandText := LQueryBuilder.DBQueries[MASTER_DATASET_NAME].CommandText;

    if AAdditionalFilter <> '' then
      LDBQueryCommandText := AddToSQLWhereClause(LDBQueryCommandText, AAdditionalFilter);

    if Assigned(AKeySet) then
    begin
      LDBQueryCommandText := AddToSQLWhereClause(LDBQueryCommandText, KeySetToSQL(AKeySet));
      DBQueryCommandText := LDBQueryCommandText; // creates DBQuery.
      CopyKeySetValuesToParams(AKeySet, DBQuery.Params);
    end
    else
      DBQueryCommandText := LDBQueryCommandText;

  finally
    FreeAndNil(LQueryBuilder);
  end;
end;

procedure TKDataSet.FireEvent(const AEventName,
  AEventParams: string; const AField: TField = nil);
var
  LParams: TEFNode;
begin
  Assert(Assigned(FDataViewTable));

  LParams := TEFNode.Create;
  try
    LParams.SetObject('DataSet', Self);
    LParams.SetObject('Field', AField);
    LParams.SetObject('DBConnection', FDBConnection.AsObject);
    CustomizeEventParams(LParams);
    { TODO : reimplement triggers/events, or rules. }
    //FDataViewTable.FireEvent(LParams, AEventName, AEventParams);
  finally
    LParams.Free;
  end;
end;

{ TKMasterDataSet }

procedure TKMasterDataSet.AfterConstruction;
begin
  inherited;
  FDetailDataSetLists := TObjectList.Create(True);
end;

destructor TKMasterDataSet.Destroy;
begin
  FreeDetailDataSetLists;
  FreeAndNil(FDetailDataSetLists);
  inherited;
end;

function TKMasterDataSet.DetailDataSetListByViewTable(
  const AViewTable: TKViewTable): TKDetailDataSetList;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to DetailDataSetListCount - 1 do
  begin
    if DetailDataSetLists[I].ViewTable = AViewTable then
    begin
      Result := DetailDataSetLists[I];
      Break;
    end;
  end;
  if not Assigned(Result) then
    raise EEFError.CreateFmt(_('No detail dataset list for view table %s.'), [AViewTable.DisplayLabel]);
end;

//function TKMasterDataSet.DetailDataSetListByTableName(
//  const ATableName: string): TKDetailDataSetList;
//var
//  I: Integer;
//begin
//  Result := nil;
//  for I := 0 to DetailDataSetListCount - 1 do
//  begin
//    if DetailDataSetLists[I].GUIForm.MainTableName = ATableName then
//    begin
//      Result := DetailDataSetLists[I];
//      Break;
//    end;
//  end;
//  if not Assigned(Result) then
//    raise EEFError.CreateFmt(_('No detail dataset list for table %s.'), [ATableName]);
//end;

procedure TKMasterDataSet.FreeDetailDataSetLists;
begin
  FDetailDataSetLists.Clear;
end;

function TKMasterDataSet.GetDetailDataSetListCount: Integer;
begin
  Result := FDetailDataSetLists.Count;
end;

procedure TKMasterDataSet.RecreateDetailDataSetLists;
var
  I: Integer;
  LNewDataSetList: TKDetailDataSetList;
begin
  Assert(Assigned(DataViewTable));

  FreeDetailDataSetLists;

  for I := 0 to DataViewTable.DetailTableCount - 1 do
  begin
    LNewDataSetList := TKDetailDataSetList.Create(Self);
    try
      LNewDataSetList.ViewTable := DataViewTable.DetailTables[I];
      FDetailDataSetLists.Add(LNewDataSetList);
    except
      FreeAndNil(LNewDataSetList);
      raise;
    end;
  end;
end;

procedure TKMasterDataSet.SetDBConnection(const AValue: IEFDBConnection);
var
  LListIndex: Integer;
  LDataSetIndex: Integer;
begin
  inherited;
  for LListIndex := 0 to FDetailDataSetLists.Count - 1 do
    for LDataSetIndex := 0 to TKDetailDataSetList(FDetailDataSetLists[LListIndex]).DataSetCount - 1 do
      TKDetailDataSetList(FDetailDataSetLists[LListIndex]).DataSets[LDataSetIndex].DBConnection := AValue;
end;

procedure TKMasterDataSet.WriteChanges(const AInserting: Boolean);

  procedure WriteDetailChanges;
  var
    LListIndex: Integer;
    LDataSetIndex: Integer;
  begin
    for LListIndex := 0 to DetailDataSetListCount - 1 do
      for LDataSetIndex := 0 to DetailDataSetLists[LListIndex].DataSetCount - 1 do
        DetailDataSetLists[LListIndex].DataSets[LDataSetIndex].WriteChanges(False);
  end;

begin
  if AInserting then
    inherited; // Create master record before trying to create details.
  WriteDetailChanges;
  if not AInserting then
    inherited;
end;

function TKMasterDataSet.GetDetailDataSetList(
  const AIndex: Integer): TKDetailDataSetList;
begin
  Result := FDetailDataSetLists[AIndex] as TKDetailDataSetList;
end;

{ TKDataSetTree }

procedure TKDataSetTree.AfterConstruction;
begin
  inherited;
  FDBQueryBuilder := TKSQLQueryBuilder.Create;
end;

destructor TKDataSetTree.Destroy;
begin
  FreeAndNil(FMasterDataSet);
  FreeAndNil(FDBQueryBuilder);
  inherited;
end;

function TKDataSetTree.GetMasterDataSet: TKMasterDataSet;
begin
  Assert(Assigned(FMasterDataSet));

  Result := FMasterDataSet;
end;

procedure TKDataSetTree.RecreateMasterDataSet;
begin
  Assert(Assigned(FDBQueryBuilder));

  FreeAndNil(FMasterDataSet);
  FMasterDataSet := TKMasterDataSet.Create(nil);
  try
    FMasterDataSet.DBConnection := FDBConnection;
    FMasterDataSet.DataViewTable := FDataViewTable;
  except
    FreeAndNil(FMasterDataSet);
    raise;
  end;
end;

procedure TKDataSetTree.Reset(const ADataViewTable: TKViewTable;
  const AAdditionalFilter: string; const AKeySet: TEFNode);
begin
  FDataViewTable := ADataViewTable;
  RecreateMasterDataSet;
  FMasterDataSet.BuildCommandTextFromDataViewTable(AKeySet, AAdditionalFilter);
end;

procedure TKDataSetTree.SetDBConnection(const AValue: IEFDBConnection);
begin
  FDBConnection := AValue;
  if Assigned(FMasterDataSet) then
    FMasterDataSet.DBConnection := AValue;
end;

procedure TKDataSetTree.WriteChanges(const AInsertingMasterRecord: Boolean);
begin
  Assert(Assigned(FMasterDataSet));
  Assert(FMasterDataSet.Active);

  FMasterDataSet.WriteChanges(AInsertingMasterRecord);
end;

{ TKDetailDataSetList }

procedure TKDetailDataSetList.AfterConstruction;
begin
  inherited;
  FDataSets := TObjectList.Create(True);
  FKey := TKDetailDataSetKey.Create;
end;

destructor TKDetailDataSetList.Destroy;
begin
  FreeAndNil(FDataSets);
  FreeAndNil(FKey);
  inherited;
end;

function TKDetailDataSetList.GetDataSet(
  const AIndex: Integer): TKDetailDataSet;
begin
  Result := FDataSets[AIndex] as TKDetailDataSet;
end;

function TKDetailDataSetList.GetDataSetByMasterKey(
  const AMasterDataSet: TDataSet): TKDetailDataSet;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FDataSets.Count - 1 do
  begin
    if Key.Matches(TKDetailDataSet(FDataSets[I]), AMasterDataSet) then
    begin
      Result := TKDetailDataSet(FDataSets[I]);
      Break;
    end;
  end;

  if Result = nil then
  begin
    Result := CreateDetailDataSet;
    try
      Key.CopyValues(AMasterDataSet, Result.MasterKeyValues);
      Result.BuildCommandTextFromDataViewTable;
    except
      FreeAndNil(Result);
      raise;
    end;
  end;
end;

function TKDetailDataSetList.CreateDetailDataSet: TKDetailDataSet;
begin
  Assert(Assigned(FViewTable));

  Result := TKDetailDataSet.Create(Self);
  try
    Result.DBConnection := Environment.MainDBConnection;
    Result.MasterKey := FKey;
    Result.DataViewTable := FViewTable;
    FDataSets.Add(Result);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TKDetailDataSetList.GetDataSetCount: Integer;
begin
  Result := FDataSets.Count;
end;

procedure TKDetailDataSetList.SetViewTable(const AValue: TKViewTable);
var
  I: Integer;
begin
  FViewTable := AValue;
  for I := 0 to FDataSets.Count - 1 do
    TKDetailDataSet(FDataSets[I]).DataViewTable := FViewTable;
  UpdateKeyFromDataViewTables;
end;

procedure TKDetailDataSetList.UpdateKeyFromDataViewTables;
var
  LMasterDBTable: TKModel;
  LDetailDBTable: TKModel;
  LForeignKeyIndex: Integer;
  LDBForeignKey: TKModelReference;
  LFieldIndex: Integer;
  LMasterDataViewField: TKViewField;
  LDetailDataViewField: TKViewField;
begin
  FKey.Reset;

  if Assigned(FViewTable) then
  begin
    LMasterDBTable := FViewTable.MasterTable.Model;
    Assert(Assigned(LMasterDBTable));
    LDetailDBTable := FViewTable.Model;
    Assert(Assigned(LDetailDBTable));

    for LForeignKeyIndex := 0 to LDetailDBTable.ReferenceCount - 1 do
    begin
      LDBForeignKey := LDetailDBTable.References[LForeignKeyIndex];
      if LDBForeignKey.ReferencedModel = LMasterDBTable then
      begin
        for LFieldIndex := 0 to LDBForeignKey.FieldCount - 1 do
        begin
          LMasterDataViewField := FViewTable.MasterTable.FieldByName(LDBForeignKey.ReferencedFields[LFieldIndex].FieldName);
          LDetailDataViewField := FViewTable.MasterTable.FieldByName(LDBForeignKey.Fields[LFieldIndex].FieldName);
          FKey.MasterFieldNames.Add(LMasterDataViewField.AliasedName);
          FKey.DetailFieldNames.Add(LDetailDataViewField.AliasedName);
        end;
        Break;
      end;
    end;
    if FKey.MasterFieldNames.Count = 0 then
      raise EEFError.CreateFmt(_('Couldn''t find a foreign key from table "%s" to table "%s".'),
        [LDetailDBTable.ModelName, LMasterDBTable.ModelName]);
  end;
end;

{ TEFDataSetKey }

procedure TKDetailDataSetKey.AfterConstruction;
begin
  inherited;
  FDetailFieldNames := TStringList.Create;
  FMasterFieldNames := TStringList.Create;
end;

destructor TKDetailDataSetKey.Destroy;
begin
  FreeAndNil(FDetailFieldNames);
  FreeAndNil(FMasterFieldNames);
  inherited;
end;

function TKDetailDataSetKey.GetDetailSQLPredicate(
  const ADetailTable, AMasterTable: TKViewTable): string;
var
  I: Integer;
begin
  Assert(FMasterFieldNames.Count > 0);
  Assert(FDetailFieldNames.Count = FMasterFieldNames.Count);
  Assert(Assigned(ADetailTable));
  Assert(Assigned(AMasterTable));

  Result := '';
  for I := 0 to FMasterFieldNames.Count - 1 do
  begin
    if Result <> '' then
      Result := Result + ' and ';
    Result := Result + ADetailTable.FieldByName(FDetailFieldNames[I]).Name + ' = :'
      + MASTER_PARAM_PREFIX + IntToStr(I);
  end;

  if Result = '' then
    raise EEFError.CreateFmt(_('No master/detail binding defined between %s and %s.'),
      [AMasterTable.ModelName, ADetailTable.ModelName]);
end;

function TKDetailDataSetKey.Matches(
  const ADetailDataSet: TKDetailDataSet;
  const AMasterDataSet: TDataSet): Boolean;
begin
  Assert(Assigned(ADetailDataSet));
  Assert(Assigned(AMasterDataSet));
  Assert(FMasterFieldNames.Count = ADetailDataSet.MasterKeyValues.ChildCount);
  Assert(FMasterFieldNames.Count <= AMasterDataSet.FieldCount);

  Result := NodeMatchesFields(ADetailDataSet.MasterKeyValues, AMasterDataSet.Fields, FMasterFieldNames);
end;

procedure TKDetailDataSetKey.Reset;
begin
  FMasterFieldNames.Clear;
  FDetailFieldNames.Clear;
end;

procedure TKDetailDataSetKey.CopyValues(const AMasterSource: TDataSet;
  const ADetailDestination: TEFNode);
var
  I: Integer;
begin
  Assert(Assigned(AMasterSource));
  Assert(Assigned(ADetailDestination));
  Assert(FMasterFieldNames.Count = FDetailFieldNames.Count);

  for I := 0 to FMasterFieldNames.Count - 1 do
  begin
    AssignFieldValueToNode(AMasterSource.FieldByName(FMasterFieldNames[I]),
      ADetailDestination.AddChild(TEFNode.Create(FDetailFieldNames[I])));
  end;
end;

procedure TKDetailDataSetKey.CopyValues(const AMasterSource,
  ADetailDestination: TDataSet);
var
  I: Integer;
begin
  Assert(Assigned(AMasterSource));
  Assert(Assigned(ADetailDestination));
  Assert(FMasterFieldNames.Count = FDetailFieldNames.Count);

  for I := 0 to FMasterFieldNames.Count - 1 do
  begin
    ADetailDestination.FieldByName(FDetailFieldNames[I]).Value :=
      AMasterSource.FieldByName(FMasterFieldNames[I]).Value;
  end;
end;

{ TKDetailDataSet }

procedure TKDetailDataSet.AfterConstruction;
begin
  inherited;
  FMasterKeyValues := TEFNode.Create;
end;

procedure TKDetailDataSet.BuildCommandTextFromDataViewTable(
  const AKeySet: TEFNode; const AAdditionalFilter: string);
begin
  inherited;
  Assert(Assigned(FMasterKey));

  DBQueryCommandText := AddToSQLWhereClause(DBQueryCommandText,
    FMasterKey.GetDetailSQLPredicate(FDataViewTable, FDataViewTable.MasterTable));
end;

procedure TKDetailDataSet.CustomizeEventParams(
  const AParams: TEFNode);
begin
  inherited;
  AParams.SetObject('MasterDataSet', MasterDataSet);
end;

destructor TKDetailDataSet.Destroy;
begin
  FreeAndNil(FMasterKeyValues);
  inherited;
end;

procedure TKDetailDataSet.OpenDataSetAndCloseQuery;
var
  I: Integer;
begin
  for I := 0 to FMasterKeyValues.ChildCount - 1 do
    AssignNodeValueToParam(FMasterKeyValues.Children[I],
      FDBQuery.Params.ParamByName(MASTER_PARAM_PREFIX + IntToStr(I)));
  inherited;
end;

procedure TKDetailDataSet.DoOnNewRecord;
begin
  inherited;
  FMasterKey.CopyValues(GetMasterDataSet, Self);
end;

function TKDetailDataSet.GetMasterDataSet: TKMasterDataSet;
begin
  Result := TKMasterDataSet(Owner.Owner);
end;

end.

