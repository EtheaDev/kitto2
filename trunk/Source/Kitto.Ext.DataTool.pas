unit Kitto.Ext.DataTool;

interface

uses
  Kitto.Metadata.DataView,
  Kitto.Ext.Base;

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
  end;

implementation

uses
  SysUtils, StrUtils;

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

end.
