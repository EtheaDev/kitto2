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

  ///	<summary>
  ///	  The default model implementation.
  ///	</summary>
  { TODO : Refactor: SQL management should be moved out of the store and into this class. }
  TKDefaultModel = class(TKModel)
  strict protected
    procedure AddDetailReference(const ADetailReference: TKModelDetailReference);
    procedure DeleteDetailReference(const ADetailReference: TKModelDetailReference);
    function AddField(const AField: TKModelField): TKModelField; overload;
    ///	<summary>
    ///   Helper method to add a field. Useful for dynamic models.
    ///	</summary>
    function AddField(const AFieldName, ADataType: string;
      const ASize: Integer = 0; const AScale: Integer = 0;
      const AIsRequired: Boolean = False; const AIsKey: Boolean = False;
      const AReferencedModel: string = ''): TKModelField; overload;
    procedure DeleteField(const AField: TKModelField);
  strict protected
    ///	<summary>
    ///	  Loads requested records into the store. Override this method to add
    ///   custom additional behaviour (by calling inherited and then do something
    ///   else) or populate the store in a custom way (by not calling inherited).
    ///	</summary>
    function InternalLoadRecords(const AStore: TKViewTableStore;
      const AFilterExpression, ASortExpression: string; const AStart,
      ALimit: Integer): Integer; virtual;
    ///	<summary>
    ///   Called by SaveRecord just before marking the record as modified (which
    ///   will guarantee that it is then actually saved). Set ADoIt to False to
    ///   prevent marking the record as modified.
    ///   The default implementation does nothing.
    ///	</summary>
    procedure BeforeMarkRecordAsModified(const ARecord: TKViewTableRecord; var ADoIt: Boolean); virtual;
    ///	<summary>
    ///   Called by SaveRecord just after marking the record as modified (only
    ///   if BeforeMarkRecordAsModified didn't set ADoIt to False).
    ///   The default implementation does nothing.
    ///	</summary>
    procedure AfterMarkRecordAsModified(const ARecord: TKViewTableRecord); virtual;
    ///	<summary>
    ///   Called by SaveRecord just before applying any Before rules to the record.
    ///   This is called regardless of the value of SaveRecord's APersist argument.
    ///   Set ADoIt to False to prevent applying the rules.
    ///   The default implementation does nothing.
    ///	</summary>
    procedure BeforeApplyBeforeRulesToRecord(const ARecord: TKViewTableRecord; var ADoIt: Boolean); virtual;
    ///	<summary>
    ///   Called by SaveRecord just after applying any of the record's Before
    ///   rules (only if BeforeApplyBeforeRulesToRecord didn't set ADoIt to False).
    ///   The default implementation does nothing.
    ///	</summary>
    procedure AfterApplyBeforeRulesToRecord(const ARecord: TKViewTableRecord); virtual;
    ///	<summary>
    ///   Called by SaveRecord just before persisting the record (only if
    ///   SaveRecord's APersist argument is True).
    ///   Set ADoIt to False to prevent persisting the record.
    ///   The default implementation does nothing.
    ///	</summary>
    procedure BeforePersistRecord(const ARecord: TKViewTableRecord; var AUseTransactions: Boolean; var ADoIt: Boolean); virtual;
    ///	<summary>
    ///   Called by SaveRecord to persist the record (only if BeforePersistRecord
    ///   didn't set ADoIt to False).
    ///   The default implementation just calls the record's Save method.
    ///	</summary>
    procedure PersistRecord(const ARecord: TKViewTableRecord; const AUseTransactions: Boolean); virtual;
    ///	<summary>
    ///   Called by SaveRecord just after persisting the record (only if
    ///   BeforePersistRecord didn't set ADoIt to False).
    ///   The default implementation does nothing.
    ///	</summary>
    procedure AfterPersistRecord(const ARecord: TKViewTableRecord; const AUseTransactions: Boolean); virtual;
  public
    ///	<summary>
    ///	  Requires that AStore is a TKViewTableStore and calls InternalLoadRecords.
    ///	</summary>
    function LoadRecords(const AStore: TEFTree; const AFilterExpression: string;
      const ASortExpression: string; const AStart: Integer = 0; const ALimit: Integer = 0): Integer; override;

    procedure SaveRecord(const ARecord: TEFNode; const APersist: Boolean;
      const AAfterPersist: TProc); override;
  end;

implementation

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
    if AScale <> 0 then
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
  const AFilterExpression, ASortExpression: string; const AStart,
  ALimit: Integer): Integer;
var
  LStore: TKViewTableStore;
begin
  Assert(Assigned(AStore));
  Assert(AStore is TKViewTableStore);

  LStore := TKViewTableStore(AStore);

  Result := InternalLoadRecords(LStore, AFilterExpression, ASortExpression, AStart, ALimit);
end;

function TKDefaultModel.InternalLoadRecords(const AStore: TKViewTableStore;
  const AFilterExpression, ASortExpression: string; const AStart,
  ALimit: Integer): Integer;
begin
  Assert(Assigned(AStore));

  if (AStart <> 0) or (ALimit <> 0) then
    Result := AStore.LoadPage(AFilterExpression, ASortExpression, AStart, ALimit)
  else
  begin
    AStore.Load(AFilterExpression, ASortExpression);
    Result := AStore.RecordCount;
  end;
end;

procedure TKDefaultModel.AfterApplyBeforeRulesToRecord(const ARecord: TKViewTableRecord);
begin
end;

procedure TKDefaultModel.AfterMarkRecordAsModified(const ARecord: TKViewTableRecord);
begin
end;

procedure TKDefaultModel.AfterPersistRecord(const ARecord: TKViewTableRecord;
  const AUseTransactions: Boolean);
begin
end;

procedure TKDefaultModel.BeforeApplyBeforeRulesToRecord(
  const ARecord: TKViewTableRecord; var ADoIt: Boolean);
begin
end;

procedure TKDefaultModel.BeforeMarkRecordAsModified(const ARecord: TKViewTableRecord;
  var ADoIt: Boolean);
begin
end;

procedure TKDefaultModel.BeforePersistRecord(const ARecord: TKViewTableRecord;
  var AUseTransactions, ADoIt: Boolean);
begin
end;

procedure TKDefaultModel.PersistRecord(const ARecord: TKViewTableRecord;
  const AUseTransactions: Boolean);
begin
  Assert(Assigned(ARecord));

  ARecord.Save(AUseTransactions);
end;

procedure TKDefaultModel.SaveRecord(const ARecord: TEFNode;
  const APersist: Boolean; const AAfterPersist: TProc);
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
  BeforeMarkRecordAsModified(LRecord, LDoIt);
  if LDoIt then
  begin
    LRecord.MarkAsModified;
    AfterMarkRecordAsModified(LRecord);
  end;

  LDoIt := True;
  BeforeApplyBeforeRulesToRecord(LRecord, LDoIt);
  if LDoIt then
  begin
    LRecord.ApplyBeforeRules;
    AfterApplyBeforeRulesToRecord(LRecord);
  end;

  if APersist then
  begin
    LUseTransactions := True;
    LDoIt := True;
    BeforePersistRecord(LRecord, LDoIt, LUseTransactions);
    if LDoIt then
    begin
      { TODO : Add support for calling virtual methods before and after applying After rules. }
      PersistRecord(LRecord, LUseTransactions);
      AfterPersistRecord(LRecord, LUseTransactions);
      if Assigned(AAfterPersist) then
        AAfterPersist;
    end;
  end;
end;

initialization
  TKModels.DefaultModelClassType := TKDefaultModel;
  //TKModelRegistry.Instance.RegisterClass('Model', TKDefaultModel);

finalization
  //TKModelRegistry.Instance.UnregisterClass('Model');
  TKModels.ResetDefaultModelClassType;

end.
