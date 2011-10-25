unit Kitto.Ext.DataPanel;

{$I Kitto.Defines.inc}

interface

uses
  EF.Classes,
  Kitto.Metadata.DataView, Kitto.Ext.Base, Kitto.Store;

type
  TKExtDataPanelController = class(TKExtPanelControllerBase)
  private
    FServerStore: TKViewTableStore;
    FViewTable: TKViewTable;
    FOwnsServerStore: Boolean;
    procedure InitServerStore;
    function GetView: TKDataView;
    function GetServerStore: TKViewTableStore;
  protected
    function AutoLoadData: Boolean;
    procedure LoadData; virtual;
    procedure CheckCanRead;
    procedure CreateToolbar; virtual;
    procedure DoDisplay; override;
    procedure InitComponents; virtual;
    procedure InitDefaults; override;
    property View: TKDataView read GetView;
    property ServerStore: TKViewTableStore read GetServerStore;
    property ViewTable: TKViewTable read FViewTable;
  public
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils, StrUtils,
  Ext,
  Kitto.Environment, Kitto.AccessControl, Kitto.Ext.Session;

{ TKExtDataPanelController }

destructor TKExtDataPanelController.Destroy;
begin
  if FOwnsServerStore then
    FreeAndNil(FServerStore);
  inherited;
end;

function TKExtDataPanelController.GetServerStore: TKViewTableStore;
begin
  Assert(Assigned(FServerStore));

  Result := FServerStore;
end;

function TKExtDataPanelController.GetView: TKDataView;
begin
  Result := inherited GetView as TKDataView;
end;

procedure TKExtDataPanelController.DoDisplay;
begin
  inherited;
  IconCls := Session.SetViewIconStyle(View);

  Assert(View is TKDataView);

  FViewTable := Config.GetObject('Sys/ViewTable') as TKViewTable;
  if FViewTable = nil then
    FViewTable := View.MainTable;
  Assert(Assigned(FViewTable));

  FServerStore := Config.GetObject('Sys/ServerStore') as TKViewTableStore;
  if FServerStore = nil then
  begin
    FServerStore := FViewTable.CreateStore;
    FOwnsServerStore := True;
  end
  else
    FOwnsServerStore := False;

  Border := False;
  Header := Config.GetBoolean('ShowHeader');

  InitComponents;
  InitServerStore;
  CheckCanRead;
  LoadData;
end;

procedure TKExtDataPanelController.CheckCanRead;
begin
  Assert(View <> nil);

  Environment.CheckAccessGranted(View.GetResourceURI, ACM_READ);
end;

procedure TKExtDataPanelController.CreateToolbar;
begin
end;

procedure TKExtDataPanelController.InitComponents;
begin
end;

procedure TKExtDataPanelController.InitDefaults;
begin
  inherited;
  Layout := lyBorder;
end;

procedure TKExtDataPanelController.InitServerStore;
begin
  Assert(View <> nil);
  Assert(Assigned(FViewTable));

{ TODO : ??? }
end;

procedure TKExtDataPanelController.LoadData;
begin
  { TODO : load store }
end;

function TKExtDataPanelController.AutoLoadData: Boolean;
begin
  Assert(ViewTable <> nil);

  Result := ViewTable.GetBoolean('Controller/AutoOpen', True);
end;

end.
