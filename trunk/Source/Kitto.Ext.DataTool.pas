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
    procedure ExecuteTool; override;
    property ServerStore: TKViewTableStore read GetServerStore;
    property ServerRecord: TKViewTableRecord read GetServerRecord;
    property ViewTable: TKViewTable read GetViewTable;

    procedure RefreshData(const AAllRecords: Boolean = False);

    procedure ExecuteInTransaction(const AProc: TProc);

    procedure EnumSelectedRecords(const AProc: TProc<TKViewTableRecord>);

    function ExpandServerRecordValues(const AString: string): string;
  end;

  TKExtDataWindowToolController = class(TKExtWindowToolController)
  strict private
    function GetServerRecord: TKViewTableRecord;
    function GetServerStore: TKViewTableStore;
    function GetViewTable: TKViewTable;
  strict protected
    procedure DoDisplay; override;
    procedure AfterExecuteTool; override;
    property ServerStore: TKViewTableStore read GetServerStore;
    property ServerRecord: TKViewTableRecord read GetServerRecord;
    property ViewTable: TKViewTable read GetViewTable;

    procedure RefreshData(const AAllRecords: Boolean = False);

    procedure ExecuteInTransaction(const AProc: TProc);

    procedure EnumSelectedRecords(const AProc: TProc<TKViewTableRecord>);
  end;

  ///	<summary>
  ///  Base class for launch a command batch or an executable
  /// </summary>
  TKExtDataCmdToolController = class(TKExtDataToolController)
  strict private
  strict
  private
    function GetBatchFileName: string;
  private
    function GetParameters: string; protected
    procedure ExecuteTool; override;
    procedure AfterExecuteTool; override;
  public
    class function GetDefaultImageName: string;
  published
    property BatchFileName: string read GetBatchFileName;
    property Parameters: string read GetParameters;
  end;

implementation

uses
  StrUtils,
  EF.Tree, EF.DB, EF.StrUtils, EF.SysUtils, EF.Localization,
  Kitto.Config, Kitto.Ext.Session, Kitto.Ext.Controller,
  EF.Shell;

procedure LoadRecordDetails(const ARecord: TKViewTableRecord);
begin
  if Assigned(ARecord) then
    ARecord.LoadDetailStores;
end;

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

procedure TKExtDataToolController.EnumSelectedRecords(
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

function TKExtDataToolController.ExpandServerRecordValues(const AString: string): string;
var
  LRecord: TKViewTableRecord;
begin
  Result := AString;
  LRecord := ServerRecord;
  if LRecord <> nil then
    Result := LRecord.ExpandExpression(Result);
end;

procedure TKExtDataToolController.ExecuteTool;
begin
  inherited;
  if Config.GetBoolean('RequireDetails') then
    LoadRecordDetails(ServerRecord);
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

procedure TKExtDataWindowToolController.EnumSelectedRecords(
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

procedure TKExtDataWindowToolController.DoDisplay;
begin
  inherited;
  if Config.GetBoolean('RequireDetails') then
    LoadRecordDetails(ServerRecord);
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

{ TKExtDataCmdToolController }

procedure TKExtDataCmdToolController.AfterExecuteTool;
begin
  inherited;
  Session.Flash(_('Command executed succesfully.'));
end;

procedure TKExtDataCmdToolController.ExecuteTool;
var
  LBatchCommand, LParameters: string;
begin
  inherited;
  LBatchCommand := BatchFileName;
  Assert(LBatchCommand <> '','BatchFileName is mandatory');
  if not FileExists(LBatchCommand) then
    raise Exception.CreateFmt('File not found %s', [BatchFileName]);

  LParameters := Parameters;
  if LParameters <> '' then
    LBatchCommand := LBatchCommand + ' ' + LParameters;

  //Execute file
//  if ExecuteApplication(LBatchCommand, True) <> 0 then
  if OpenDocument(LBatchCommand, True) <> 0 then
    raise Exception.CreateFmt('Error executing %s', [ExtractFileName(BatchFileName)]);
end;

function TKExtDataCmdToolController.GetBatchFileName: string;
begin
  Result := Config.GetExpandedString('BatchFileName');
end;

class function TKExtDataCmdToolController.GetDefaultImageName: string;
begin
  Result := 'execute_command';
end;

function TKExtDataCmdToolController.GetParameters: string;
begin
  Result := Config.GetExpandedString('Parameters');
end;

initialization
  TKExtControllerRegistry.Instance.RegisterClass('ExecuteCmdTool', TKExtDataCmdToolController);

finalization
  TKExtControllerRegistry.Instance.UnregisterClass('ExecuteCmdTool');

end.
