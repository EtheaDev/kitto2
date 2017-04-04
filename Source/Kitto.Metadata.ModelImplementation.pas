{-------------------------------------------------------------------------------
   Copyright 2013 Ethea S.r.l.

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

unit Kitto.Metadata.ModelImplementation;

{$I Kitto.Defines.inc}

interface

uses
  SysUtils,
  EF.Tree,
  Kitto.Metadata.Models, Kitto.Metadata.DataView;

type
  TKModelFieldHelper = class helper for TKModelField
  private
  public
    procedure SetFieldSpec(const ADataType: string; const ASize, AScale: Integer;
      const AIsRequired, AIsKey: Boolean; const AReferencedModel: string);

    procedure SetIsKey(const AValue: Boolean);
    procedure AddField(const AField: TKModelField);
    procedure DeleteField(const AField: TKModelField);
  end;

  TKModelDetailReferencesHelper = class helper for TKModelDetailReferences
  public
    procedure AddDetailReference(const ADetailReference: TKModelDetailReference);
    procedure DeleteDetailReference(const ADetailReference: TKModelDetailReference);
  end;

  /// <summary>
  ///  The default model implementation. Performs data I/O from a database
  ///  through SQL.
  /// </summary>
  { TODO : Refactor: SQL management should be moved out of the store and into this class. }
  TKDefaultModel = class(TKModel)
  strict protected
    procedure AddDetailReference(const ADetailReference: TKModelDetailReference);
    procedure DeleteDetailReference(const ADetailReference: TKModelDetailReference);
    function AddField(const AField: TKModelField): TKModelField; overload;
    /// <summary>
    ///  Helper method to add a field. Useful for dynamic models.
    /// </summary>
    function AddField(const AFieldName, ADataType: string;
      const ASize: Integer = 0; const AScale: Integer = 0;
      const AIsRequired: Boolean = False; const AIsKey: Boolean = False;
      const AReferencedModel: string = ''): TKModelField; overload;
    procedure DeleteField(const AField: TKModelField);
  strict protected
    /// <summary>
    ///  Loads requested records into the store. Override this method to add
    ///  custom additional behaviour (by calling inherited and then do something
    ///  else) or populate the store in a custom way (by not calling inherited).
    /// </summary>
    function InternalLoadRecords(const AStore: TKViewTableStore;
      const AFilter, ASort: string; const AStart, ALimit: Integer;
      const AForEachRecord: TProc<TKViewTableRecord>): Integer; virtual;
    /// <summary>
    ///  Saves all modified records in the specified store. Override this method to add
    ///  custom additional behaviour (by calling inherited and then do something
    ///  else) or persist the store in a custom way (by not calling inherited).
    /// </summary>
    procedure InternalSaveRecords(const AStore: TKViewTableStore;
      const APersist: Boolean; const AUseTransaction: Boolean; const AAfterPersist: TProc); virtual;
    /// <summary>
    ///  Called by SaveRecord just before applying any Before rules to the record.
    ///  This is called regardless of the value of SaveRecord's APersist argument.
    ///  Set ADoIt to False to prevent applying the rules.
    ///  The default implementation does nothing.
    /// </summary>
    procedure BeforeApplyBeforeRulesToRecord(const ARecord: TKViewTableRecord; var ADoIt: Boolean); virtual;
    /// <summary>
    ///  Called by SaveRecord just after applying any of the record's Before
    ///  rules (only if BeforeApplyBeforeRulesToRecord didn't set ADoIt to False).
    ///  The default implementation does nothing.
    /// </summary>
    procedure AfterApplyBeforeRulesToRecord(const ARecord: TKViewTableRecord); virtual;
    /// <summary>
    ///  Called by SaveRecord just before persisting the record (only if
    ///  SaveRecord's APersist argument is True).
    ///  Set ADoIt to False to prevent persisting the record.
    ///  The default implementation does nothing.
    /// </summary>
    procedure BeforePersistRecord(const ARecord: TKViewTableRecord; var AUseTransactions: Boolean; var ADoIt: Boolean); virtual;
    /// <summary>
    ///  Called by SaveRecord to persist the record (only if BeforePersistRecord
    ///  didn't set ADoIt to False).
    ///  The default implementation just calls the record's Save method.
    /// </summary>
    procedure PersistRecord(const ARecord: TKViewTableRecord; const AUseTransactions: Boolean); virtual;
    /// <summary>
    ///  Called by SaveRecord just after persisting the record (only if
    ///  BeforePersistRecord didn't set ADoIt to False).
    ///  The default implementation does nothing.
    /// </summary>
    procedure AfterPersistRecord(const ARecord: TKViewTableRecord; const AUseTransactions: Boolean); virtual;
    /// <summary>
    ///  Called by SaveRecord just before applying any After rules to the record.
    ///  This is called regardless of the value of SaveRecord's APersist argument.
    ///  Set ADoIt to False to prevent applying the rules.
    ///  The default implementation does nothing.
    /// </summary>
    procedure BeforeApplyAfterRulesToRecord(const ARecord: TKViewTableRecord; var ADoIt: Boolean); virtual;
    /// <summary>
    ///  Called by SaveRecord just after applying any of the record's After
    ///  rules (only if BeforeApplyAfterRulesToRecord didn't set ADoIt to False).
    ///  The default implementation does nothing.
    /// </summary>
    procedure AfterApplyAfterRulesToRecord(const ARecord: TKViewTableRecord); virtual;
  public
    /// <summary>
    ///  Requires that AStore is a TKViewTableStore and calls InternalLoadRecords.
    /// </summary>
    function LoadRecords(const AStore: TEFTree; const AFilterExpression: string;
      const ASortExpression: string; const AStart: Integer = 0; const ALimit: Integer = 0;
      const AForEachRecord: TProc<TEFNode> = nil): Integer; override;

    /// <summary>
    ///  Requires that AStore is a TKViewTableStore and calls InternalSaveRecords.
    /// </summary>
    procedure SaveRecords(const AStore: TEFTree; const APersist: Boolean;
      const AAfterPersist: TProc; const AUseTransaction: Boolean = True); override;

    /// <summary>
    ///  Persists the specified record. Calls various protected virtual methods.
    /// </summary>
    procedure SaveRecord(const ARecord: TEFNode; const APersist: Boolean;
      const AAfterPersist: TProc; const AUseTransaction: Boolean = True); override;
  end;

implementation

uses
  TypInfo,
  EF.DB, Kitto.Config, KItto.SQL, Kitto.Store, Kitto.Types;

{ TKModelFieldHelper }

procedure TKModelFieldHelper.SetIsKey(const AValue: Boolean);
var
  LDataType: string;
  LSize, LScale: Integer;
  LIsRequired: Boolean;
  LIsKey: Boolean;
  LReferencedModel: string;
begin
  GetFieldSpec(LDataType, LSize, LScale, LIsRequired, LIsKey, LReferencedModel);
  SetFieldSpec(LDataType, LSize, LScale, LIsRequired, AValue, LReferencedModel);
end;

procedure TKModelFieldHelper.AddField(const AField: TKModelField);
begin
  Assert(Assigned(AField));

  GetFields.AddChild(AField);
end;

procedure TKModelFieldHelper.DeleteField(const AField: TKModelField);
begin
  Assert(Assigned(AField));

  GetFields.RemoveChild(AField);
end;

procedure TKModelFieldHelper.SetFieldSpec(const ADataType: string;
  const ASize, AScale: Integer; const AIsRequired: Boolean; const AIsKey: Boolean;
  const AReferencedModel: string);
var
  LSpec: string;
begin
  LSpec := ADataType;
  if AReferencedModel <> '' then
    LSpec := LSpec + '(' + AReferencedModel + ')'
  else if ASize <> 0 then
  begin
    LSpec := LSpec + '(' + IntToStr(ASize);
    if AScale > 0 then
      LSpec := LSpec + ', ' + IntToStr(AScale);
    LSpec := LSpec + ')';
  end;
  if AIsRequired then
    LSpec := LSpec + ' not null';
  if AIsKey then
    LSpec := LSpec + ' primary key';
  AsString := LSpec;
end;

{ TKModelDetailReferencesHelper }

procedure TKModelDetailReferencesHelper.AddDetailReference(
  const ADetailReference: TKModelDetailReference);
begin
  Assert(Assigned(ADetailReference));

  AddChild(ADetailReference);
end;

procedure TKModelDetailReferencesHelper.DeleteDetailReference(
  const ADetailReference: TKModelDetailReference);
begin
  Assert(Assigned(ADetailReference));

  RemoveChild(ADetailReference);
end;

{ TKDefaultModel }

procedure TKDefaultModel.AddDetailReference(
  const ADetailReference: TKModelDetailReference);
begin
  Assert(Assigned(ADetailReference));

  GetDetailReferences.AddDetailReference(ADetailReference);
end;

function TKDefaultModel.AddField(const AFieldName, ADataType: string;
  const ASize, AScale: Integer; const AIsRequired, AIsKey: Boolean;
  const AReferencedModel: string): TKModelField;
begin
  Result := AddField(TKModelField.Create(AFieldName));
  Result.SetFieldSpec(ADataType, ASize, AScale, AIsRequired, AIsKey, AReferencedModel);
end;

function TKDefaultModel.AddField(const AField: TKModelField): TKModelField;
begin
  Assert(Assigned(AField));

  Result := GetFields.AddChild(AField) as TKModelField;
end;

procedure TKDefaultModel.DeleteDetailReference(
  const ADetailReference: TKModelDetailReference);
begin
  Assert(Assigned(ADetailReference));

  GetDetailReferences.DeleteDetailReference(ADetailReference);
end;

procedure TKDefaultModel.DeleteField(const AField: TKModelField);
begin
  Assert(Assigned(AField));

  GetFields.RemoveChild(AField);
end;

function TKDefaultModel.LoadRecords(const AStore: TEFTree;
  const AFilterExpression, ASortExpression: string; const AStart, ALimit: Integer;
  const AForEachRecord: TProc<TEFNode>): Integer;
begin
  Assert(Assigned(AStore));
  Assert(AStore is TKViewTableStore);

  Result := InternalLoadRecords(TKViewTableStore(AStore), AFilterExpression, ASortExpression, AStart, ALimit,
    procedure (ARecord: TKViewTableRecord)
    begin
      if Assigned(AForEachRecord) then
        AForEachRecord(ARecord);
      ARecord.MarkAsClean;
    end);
end;

function TKDefaultModel.InternalLoadRecords(const AStore: TKViewTableStore;
  const AFilter, ASort: string; const AStart, ALimit: Integer;
  const AForEachRecord: TProc<TKViewTableRecord>): Integer;
begin
  Assert(Assigned(AStore));

  Result := AStore.Load(AFilter, ASort, AStart, ALimit, AForEachRecord);
end;

procedure TKDefaultModel.AfterApplyAfterRulesToRecord(const ARecord: TKViewTableRecord);
begin
end;

procedure TKDefaultModel.AfterApplyBeforeRulesToRecord(const ARecord: TKViewTableRecord);
begin
end;

procedure TKDefaultModel.AfterPersistRecord(const ARecord: TKViewTableRecord;
  const AUseTransactions: Boolean);
begin
end;

procedure TKDefaultModel.BeforeApplyAfterRulesToRecord(
  const ARecord: TKViewTableRecord; var ADoIt: Boolean);
begin
end;

procedure TKDefaultModel.BeforeApplyBeforeRulesToRecord(
  const ARecord: TKViewTableRecord; var ADoIt: Boolean);
begin
end;

procedure TKDefaultModel.BeforePersistRecord(const ARecord: TKViewTableRecord;
  var AUseTransactions, ADoIt: Boolean);
begin
end;

procedure TKDefaultModel.PersistRecord(const ARecord: TKViewTableRecord;
  const AUseTransactions: Boolean);
var
  LDBCommand: TEFDBCommand;
  LRowsAffected: Integer;
  LDBConnection: TEFDBConnection;

  procedure PersistDetailStores;
  var
    I: Integer;
  begin
    { TODO : implement cascade delete? }
    for I := 0 to ARecord.DetailStoreCount - 1 do
      SaveRecords(ARecord.DetailStores[I], True, nil);
  end;

begin
  Assert(Assigned(ARecord));

  if ARecord.State = rsClean then
  begin
    // If the record is not dirty, we don't need to persist it - still we may
    // have dirty detail records.
    PersistDetailStores;
    Exit;
  end;

  // Take care of any instructions to clear fields.
  ARecord.HandleSetToNullInstructions;

  // BEFORE rules are applied before calling this method.
  LDBConnection := TKConfig.Instance.CreateDBConnection(ARecord.Store.ViewTable.DatabaseName);
  try
    if AUseTransactions then
      LDBConnection.StartTransaction;
    try
      LDBCommand := LDBConnection.CreateDBCommand;
      try
        TKSQLBuilder.CreateAndExecute(
          procedure (ASQLBuilder: TKSQLBuilder)
          begin
            case ARecord.State of
              rsNew: ASQLBuilder.BuildInsertCommand(LDBCommand, ARecord);
              rsDirty: ASQLBuilder.BuildUpdateCommand(LDBCommand, ARecord);
              rsDeleted: ASQLBuilder.BuildDeleteCommand(LDBCommand, ARecord);
            else
              raise EKError.CreateFmt('Unexpected record state %s.', [GetEnumName(TypeInfo(TKRecordState), Ord(ARecord.State))]);
            end;
          end);
        if LDBCommand.CommandText <> '' then
        begin
          LRowsAffected := LDBCommand.Execute;
          if LRowsAffected <> 1 then
            raise EKError.CreateFmt('Update error. Rows affected: %d.', [LRowsAffected]);
        end;
        PersistDetailStores;
        ARecord.ApplyAfterRules;
        if AUseTransactions then
          LDBConnection.CommitTransaction;
        // Take care of any cleared external files.
        ARecord.HandleDeleteFileInstructions;
        ARecord.MarkAsClean;
      finally
        FreeAndNil(LDBCommand);
      end;
    except
      if AUseTransactions then
        LDBConnection.RollbackTransaction;
      raise;
    end;
  finally
    FreeAndNil(LDBConnection);
  end;
end;

procedure TKDefaultModel.SaveRecord(const ARecord: TEFNode;
  const APersist: Boolean; const AAfterPersist: TProc;
  const AUseTransaction: Boolean = True);
var
  LRecord: TKViewTableRecord;
  LUseTransactions: Boolean;
  LDoIt: Boolean;
begin
  inherited;
  Assert(Assigned(ARecord));
  Assert(ARecord is TKViewTableRecord);

  LRecord := TKViewTableRecord(ARecord);

  LDoIt := True;
  BeforeApplyBeforeRulesToRecord(LRecord, LDoIt);
  if LDoIt then
  begin
    LRecord.ApplyBeforeRules;
    AfterApplyBeforeRulesToRecord(LRecord);
  end;

  if APersist then
  begin
    LUseTransactions := AUseTransaction;
    LDoIt := True;
    BeforePersistRecord(LRecord, LUseTransactions, LDoIt);
    if LDoIt then
    begin
      PersistRecord(LRecord, LUseTransactions);
      AfterPersistRecord(LRecord, LUseTransactions);
      if Assigned(AAfterPersist) then
        AAfterPersist;
    end;
  end;

  LDoIt := True;
  BeforeApplyAfterRulesToRecord(LRecord, LDoIt);
  if LDoIt then
  begin
    LRecord.ApplyAfterRules;
    AfterApplyAfterRulesToRecord(LRecord);
  end;
end;

procedure TKDefaultModel.SaveRecords(const AStore: TEFTree;
  const APersist: Boolean; const AAfterPersist: TProc;
  const AUseTransaction: Boolean = True);
begin
  Assert(Assigned(AStore));
  Assert(AStore is TKViewTableStore);

  InternalSaveRecords(TKViewTableStore(AStore), APersist, AUseTransaction, AAfterPersist);
end;

procedure TKDefaultModel.InternalSaveRecords(const AStore: TKViewTableStore;
  const APersist: Boolean; const AUseTransaction: Boolean; const AAfterPersist: TProc);
var
  I: Integer;
  LDBConnection: TEFDBConnection;
  LUseTransaction: Boolean;
begin
  LDBConnection := TKConfig.Instance.CreateDBConnection(AStore.ViewTable.DatabaseName);
  try
    if AUseTransaction then
      LDBConnection.StartTransaction;
    try
      //Save any record in a single transaction only if the connection is not is transaction
      LUseTransaction := not LDBConnection.IsInTransaction;
      for I := 0 to AStore.RecordCount - 1 do
        SaveRecord(AStore.Records[I], APersist, AAfterPersist, LUseTransaction);
      if AUseTransaction then
        LDBConnection.CommitTransaction;
    except
      if AUseTransaction then
        LDBConnection.RollbackTransaction;
      raise;
    end;
  finally
    FreeAndNil(LDBConnection);
  end;
end;

initialization
  TKModels.DefaultModelClassType := TKDefaultModel;
  //TKModelRegistry.Instance.RegisterClass('Model', TKDefaultModel);

finalization
  //TKModelRegistry.Instance.UnregisterClass('Model');
  TKModels.ResetDefaultModelClassType;

end.
