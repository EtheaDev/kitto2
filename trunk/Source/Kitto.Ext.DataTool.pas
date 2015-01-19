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

unit Kitto.Ext.DataTool;

interface

uses
  SysUtils,
  Kitto.Metadata.DataView,
  Kitto.Ext.Base;

{ TODO : refactor these two classes to keep duplicated code to a minimum }
type
  TKExtDataToolController = class(TKExtToolController)
  strict private
    function GetServerRecord: TKViewTableRecord;
    function GetServerStore: TKViewTableStore;
    function GetViewTable: TKViewTable;
  strict protected
    procedure AfterExecuteTool; override;
    property ServerStore: TKViewTableStore read GetServerStore;
    property ServerRecord: TKViewTableRecord read GetServerRecord;
    property ViewTable: TKViewTable read GetViewTable;

    procedure RefreshData(const AAllRecords: Boolean = False);

    procedure ExecuteInTransaction(const AProc: TProc);

    procedure EnumerateSelectedRecords(const AProc: TProc<TKViewTableRecord>);
  end;

  TKExtDataWindowToolController = class(TKExtWindowToolController)
  strict private
    function GetServerRecord: TKViewTableRecord;
    function GetServerStore: TKViewTableStore;
    function GetViewTable: TKViewTable;
  strict protected
    procedure AfterExecuteTool; override;
    property ServerStore: TKViewTableStore read GetServerStore;
    property ServerRecord: TKViewTableRecord read GetServerRecord;
    property ViewTable: TKViewTable read GetViewTable;

    procedure RefreshData(const AAllRecords: Boolean = False);

    procedure ExecuteInTransaction(const AProc: TProc);

    procedure EnumerateSelectedRecords(const AProc: TProc<TKViewTableRecord>);
  end;

implementation

uses
  StrUtils,
  EF.Tree, EF.DB, EF.StrUtils,
  Kitto.Config, Kitto.Ext.Session;

{ TKExtDataToolController }

procedure TKExtDataToolController.AfterExecuteTool;
var
  LAutoRefresh: string;
begin
  inherited;
  LAutoRefresh := Config.GetString('AutoRefresh');
  if MatchText(LAutoRefresh, ['Current', 'All']) then
    RefreshData(SameText(LAutoRefresh, 'All'));
end;

procedure TKExtDataToolController.EnumerateSelectedRecords(
  const AProc: TProc<TKViewTableRecord>);
var
  LKey: TEFNode;
  LRecordCount: Integer;
  I: Integer;
begin
  Assert(Assigned(AProc));

  LKey := TEFNode.Create;
  try
    LKey.Assign(ServerStore.Key);
    Assert(LKey.ChildCount > 0);
    LRecordCount := Length(Split(Session.Queries.Values[LKey[0].Name], ','));
    for I := 0 to LRecordCount - 1 do
      AProc(ServerStore.GetRecord(Session.GetQueries, Session.Config.JSFormatSettings, I));
  finally
    FreeAndNil(LKey);
  end;
end;

procedure TKExtDataToolController.ExecuteInTransaction(const AProc: TProc);
var
  LDBConnection: TEFDBConnection;
begin
  Assert(Assigned(AProc));

  LDBConnection := TKConfig.Instance.DBConnections[ViewTable.DatabaseName];
  LDBConnection.StartTransaction;
  try
    AProc;
    LDBConnection.CommitTransaction;
  except
    LDBConnection.RollbackTransaction;
    raise;
  end;
end;

function TKExtDataToolController.GetServerRecord: TKViewTableRecord;
begin
  Result := Config.GetObject('Sys/Record') as TKViewTableRecord;
end;

function TKExtDataToolController.GetServerStore: TKViewTableStore;
begin
  Result := Config.GetObject('Sys/ServerStore') as TKViewTableStore;
end;

function TKExtDataToolController.GetViewTable: TKViewTable;
begin
  Result := Config.GetObject('Sys/ViewTable') as TKViewTable;
end;

procedure TKExtDataToolController.RefreshData(const AAllRecords: Boolean);
begin
  if AAllRecords then
    NotifyObservers('RefreshAllRecords')
  else
    NotifyObservers('RefreshCurrentRecord');
end;

{ TKExtDataWindowToolController }

procedure TKExtDataWindowToolController.AfterExecuteTool;
var
  LAutoRefresh: string;
begin
  inherited;
  LAutoRefresh := Config.GetString('AutoRefresh');
  if MatchText(LAutoRefresh, ['Current', 'All']) then
    RefreshData(SameText(LAutoRefresh, 'All'));
end;

procedure TKExtDataWindowToolController.EnumerateSelectedRecords(
  const AProc: TProc<TKViewTableRecord>);
var
  LKey: TEFNode;
  LRecordCount: Integer;
  I: Integer;
begin
  Assert(Assigned(AProc));

  LKey := TEFNode.Create;
  try
    LKey.Assign(ServerStore.Key);
    Assert(LKey.ChildCount > 0);
    LRecordCount := Length(EF.StrUtils.Split(Session.Queries.Values[LKey[0].Name], ','));
    for I := 0 to LRecordCount - 1 do
      AProc(ServerStore.GetRecord(Session.GetQueries, Session.Config.JSFormatSettings, I));
  finally
    FreeAndNil(LKey);
  end;
end;

procedure TKExtDataWindowToolController.ExecuteInTransaction(const AProc: TProc);
var
  LDBConnection: TEFDBConnection;
begin
  Assert(Assigned(AProc));

  LDBConnection := TKConfig.Instance.DBConnections[ViewTable.DatabaseName];
  LDBConnection.StartTransaction;
  try
    AProc;
    LDBConnection.CommitTransaction;
  except
    LDBConnection.RollbackTransaction;
    raise;
  end;
end;

function TKExtDataWindowToolController.GetServerRecord: TKViewTableRecord;
begin
  Result := Config.GetObject('Sys/Record') as TKViewTableRecord;
end;

function TKExtDataWindowToolController.GetServerStore: TKViewTableStore;
begin
  Result := Config.GetObject('Sys/ServerStore') as TKViewTableStore;
end;

function TKExtDataWindowToolController.GetViewTable: TKViewTable;
begin
  Result := Config.GetObject('Sys/ViewTable') as TKViewTable;
end;

procedure TKExtDataWindowToolController.RefreshData(const AAllRecords: Boolean);
begin
  if AAllRecords then
    NotifyObservers('RefreshAllRecords')
  else
    NotifyObservers('RefreshCurrentRecord');
end;

end.
