unit Kitto.Ext.DataPanel;

interface

uses
  EF.Data, EF.Classes,
  Kitto.Metadata.Views, Kitto.DataSetTree, Kitto.Ext.Base;

type
  TKExtDataPanel = class(TKExtPanelController)
  private
    FDataSetTree: TKDataSetTree;
    FOwnsDataSetTree: Boolean;
    FDataSet: TKMasterDataSet;
    FViewTable: TKViewTable;
    function GetDataSet: TKMasterDataSet;
    procedure InitDataSetTree;
    function GetView: TKDataView;
  protected
    function AutoOpenDataSet: Boolean;
    procedure OpenDataSet; virtual;
    procedure CheckCanRead;
    procedure CreateToolbar; virtual;
    procedure DoDisplay; override;
    procedure InitComponents; virtual;
    property DataSetTree: TKDataSetTree read FDataSetTree;
    property DataSet: TKMasterDataSet read GetDataSet;
    property View: TKDataView read GetView;
    property ViewTable: TKViewTable read FViewTable;
    function GetFilterExpression: string; virtual;
  public
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils, StrUtils,
  Ext,
  Kitto.Environment, Kitto.AccessControl;

{ TKExtDataPanel }

destructor TKExtDataPanel.Destroy;
begin
  if FOwnsDataSetTree then
    FreeAndNil(FDataSetTree);
  inherited;
end;

function TKExtDataPanel.GetDataSet: TKMasterDataSet;
begin
  Assert(View <> nil);

  if not Assigned(FDataSet) then
  begin
    FDataSet := Config.GetObject('Sys/DataSet') as TKMasterDataSet;
    if not Assigned(FDataSet) then
    begin
      Assert(Assigned(FDataSetTree));
      FDataSet := FDataSetTree.MasterDataSet;
    end;
  end;
  Result := FDataSet;
end;

function TKExtDataPanel.GetFilterExpression: string;
begin
  Result := '';
end;

function TKExtDataPanel.GetView: TKDataView;
begin
  Result := inherited GetView as TKDataView;
end;

procedure TKExtDataPanel.DoDisplay;
begin
  inherited;
  Assert(View is TKDataView);

  FViewTable := Config.GetObject('Sys/ViewTable') as TKViewTable;
  if FViewTable = nil then
    FViewTable := View.MainTable;
  Assert(Assigned(FViewTable));

  Layout := lyBorder;
  Border := False;
  Header := False;

  InitComponents;
  InitDataSetTree;
  CheckCanRead;
  OpenDataSet;
end;

procedure TKExtDataPanel.CheckCanRead;
begin
  Assert(View <> nil);

{ TODO : implement GetResourceURI everywhere. }
  //Environment.CheckAccessGranted(View.GetResourceURI, ACM_READ);
end;

procedure TKExtDataPanel.CreateToolbar;
begin
end;

procedure TKExtDataPanel.InitComponents;
begin
end;

procedure TKExtDataPanel.InitDataSetTree;
begin
  Assert(View <> nil);
  Assert(Assigned(FViewTable));

  FDataSetTree := Config.GetObject('Sys/DataSetTree') as TKDataSetTree;
  FOwnsDataSetTree := not Assigned(FDataSetTree);
  if FOwnsDataSetTree then
  begin
    FDataSetTree := TKDataSetTree.Create;
    FDataSetTree.DBConnection := Environment.MainDBConnection;
    DataSetTree.Reset(FViewTable, GetFilterExpression);
  end;
end;

procedure TKExtDataPanel.OpenDataSet;
begin
  if not DataSet.Active then
  begin
    if ViewTable.IsDetail then
      DataSet.OpenDataSetAndCloseQuery
    else if AutoOpenDataSet then
      DataSet.OpenDataSetAndCloseQueryInTransaction
    else
      ; // show search tools that will open the dataset later.
  end;
end;

function TKExtDataPanel.AutoOpenDataSet: Boolean;
begin
  Assert(ViewTable <> nil);

  Result := ViewTable.GetBoolean('Controller/AutoOpen', True);
end;

end.
