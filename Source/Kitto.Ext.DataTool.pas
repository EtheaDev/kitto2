{-------------------------------------------------------------------------------
   Copyright 2012-2018 Ethea S.r.l.

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
  Kitto.Ext.Base, Kitto.JS.Controller;

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

  TKExtDataPanelToolController = class(TKExtPanelToolController)
  strict private
    FSelectedRecords: TArray<TKViewTableRecord>;
    function GetServerRecord: TKViewTableRecord;
    function GetServerStore: TKViewTableStore;
    function GetViewTable: TKViewTable;
    procedure StoreSelectedRecords;
  strict protected
    procedure DoDisplay; override;
    procedure AfterExecuteTool; override;
    property ServerStore: TKViewTableStore read GetServerStore;
    property ServerRecord: TKViewTableRecord read GetServerRecord;
    property ViewTable: TKViewTable read GetViewTable;
    property SelectedRecords: TArray<TKViewTableRecord> read FSelectedRecords;
    procedure RefreshData(const AAllRecords: Boolean = False);

    procedure ExecuteInTransaction(const AProc: TProc);
  end;

  ///	<summary>
  ///  Executes a command or executable file and waits for its completion
  ///  before returning.
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
    class function GetDefaultImageName: string; override;
  //published
    property BatchFileName: string read GetBatchFileName;
    property Parameters: string read GetParameters;
  end;

implementation

uses
  StrUtils
  , EF.Tree
  , EF.DB
  , EF.StrUtils
  , EF.Sys
  , EF.Localization
  , Kitto.Config
  , Kitto.Web.Application
  , Kitto.Web.Request
  ;

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
    LRecordCount := Length(Split(ParamAsString(LKey[0].Name), ','));
    for I := 0 to LRecordCount - 1 do
      AProc(ServerStore.GetRecord(TKWebRequest.Current.QueryTree, TKWebApplication.Current.Config.JSFormatSettings, I));
  finally
    FreeAndNil(LKey);
  end;
end;

procedure TKExtDataToolController.ExecuteInTransaction(const AProc: TProc);
var
  LDBConnection: TEFDBConnection;
begin
  Assert(Assigned(AProc));

  LDBConnection := TKConfig.Instance.CreateDBConnection(ViewTable.DatabaseName);
  try
    LDBConnection.StartTransaction;
    try
      AProc;
      LDBConnection.CommitTransaction;
    except
      LDBConnection.RollbackTransaction;
      raise;
    end;
  finally
    FreeAndNil(LDBConnection);
  end;
end;

function TKExtDataToolController.ExpandServerRecordValues(const AString: string): string;
var
  LRecord: TKViewTableRecord;
begin
  Result := AString;
  LRecord := ServerRecord;
  if (LRecord = nil) and (ServerStore <> nil) and (ServerStore.RecordCount > 0) then
    LRecord := ServerStore.Records[0];
  if LRecord <> nil then
    LRecord.ExpandExpression(Result);
end;

procedure TKExtDataToolController.ExecuteTool;
begin
  inherited;
  if Config.GetBoolean('RequireDetails') and Assigned(ServerRecord) then
    ServerRecord.LoadDetailStores;
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
    ServerRecord.Refresh;
end;

{ TKExtDataPanelToolController }

procedure TKExtDataPanelToolController.AfterExecuteTool;
var
  LAutoRefresh: string;
begin
  inherited;
  LAutoRefresh := Config.GetString('AutoRefresh');
  if MatchText(LAutoRefresh, ['Current', 'All']) then
    RefreshData(SameText(LAutoRefresh, 'All'));
end;

procedure TKExtDataPanelToolController.ExecuteInTransaction(const AProc: TProc);
var
  LDBConnection: TEFDBConnection;
begin
  Assert(Assigned(AProc));

  LDBConnection := TKConfig.Instance.CreateDBConnection(ViewTable.DatabaseName);
  try
    LDBConnection.StartTransaction;
    try
      AProc;
      LDBConnection.CommitTransaction;
    except
      LDBConnection.RollbackTransaction;
      raise;
    end;
  finally
    FreeAndNil(LDBConnection);
  end;
end;

procedure TKExtDataPanelToolController.StoreSelectedRecords;
var
  LKey: TEFNode;
  LRecordCount: Integer;
  I: Integer;
begin
  SetLength(FSelectedRecords, 0);
  LKey := TEFNode.Create;
  try
    LKey.Assign(ServerStore.Key);
    Assert(LKey.ChildCount > 0);
    LRecordCount := Length(EF.StrUtils.Split(TKWebRequest.Current.GetQueryField((LKey[0].Name)), ','));
    SetLength(FSelectedRecords, LRecordCount);
    for I := 0 to LRecordCount - 1 do
      FSelectedRecords[I] := ServerStore.GetRecord(TKWebRequest.Current.QueryTree, TKWebApplication.Current.Config.JSFormatSettings, I);
  finally
    FreeAndNil(LKey);
  end;
end;

procedure TKExtDataPanelToolController.DoDisplay;
begin
  inherited;
  StoreSelectedRecords;
  if Config.GetBoolean('RequireDetails') and Assigned(ServerRecord) then
    ServerRecord.LoadDetailStores;
end;

function TKExtDataPanelToolController.GetServerRecord: TKViewTableRecord;
begin
  Result := Config.GetObject('Sys/Record') as TKViewTableRecord;
end;

function TKExtDataPanelToolController.GetServerStore: TKViewTableStore;
begin
  Result := Config.GetObject('Sys/ServerStore') as TKViewTableStore;
end;

function TKExtDataPanelToolController.GetViewTable: TKViewTable;
begin
  Result := Config.GetObject('Sys/ViewTable') as TKViewTable;
end;

procedure TKExtDataPanelToolController.RefreshData(const AAllRecords: Boolean);
begin
  if AAllRecords then
    NotifyObservers('RefreshAllRecords')
  else
    ServerRecord.Refresh;
end;

{ TKExtDataCmdToolController }

procedure TKExtDataCmdToolController.AfterExecuteTool;
begin
  inherited;
  TKWebApplication.Current.Toast(_('Command executed succesfully.'));
end;

procedure TKExtDataCmdToolController.ExecuteTool;
var
  LBatchFileName, LBatchCommand, LParameters: string;
begin
  inherited;
  LBatchFileName := ExpandServerRecordValues(BatchFileName);
  Assert(LBatchFileName <> '','BatchFileName is mandatory');
  if not FileExists(LBatchFileName) then
    raise Exception.CreateFmt('File not found %s', [LBatchFileName]);

  LParameters := ExpandServerRecordValues(Parameters);
  if LParameters <> '' then
    LBatchCommand := LBatchFileName + ' ' + LParameters
  else
    LBatchCommand := LBatchFileName;

  if EFSys.ExecuteCommand(LBatchCommand) <> 0 then
    raise Exception.CreateFmt('Error executing %s', [ExtractFileName(LBatchFileName)]);
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
  TJSControllerRegistry.Instance.RegisterClass('ExecuteCmdTool', TKExtDataCmdToolController);

finalization
  TJSControllerRegistry.Instance.UnregisterClass('ExecuteCmdTool');

end.
