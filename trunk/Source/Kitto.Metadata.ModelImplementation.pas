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
  TKDefaultModel = class(TKModel)
  strict protected
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
    procedure SaveRecord(const ARecord: TEFNode; const APersist: Boolean;
      const AAfterPersist: TProc); override;
  end;

implementation

{ TKDefaultModel }

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
