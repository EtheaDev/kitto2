unit Kitto.Ext.DataPanel;

{$I Kitto.Defines.inc}

interface

uses
  EF.Classes,
  Kitto.Metadata.Views, Kitto.Ext.Base, Kitto.Store;

type
  TKExtDataPanelController = class(TKExtPanelControllerBase)
  private
    FServerStore: TKStore;
    FViewTable: TKViewTable;
    FOwnsServerStore: Boolean;
    procedure InitServerStore;
    function GetView: TKDataView;
    function GetServerStore: TKStore;
  protected
    function AutoLoadData: Boolean;
    procedure LoadData; virtual;
    procedure CheckCanRead;
    procedure CreateToolbar; virtual;
    procedure DoDisplay; override;
    procedure InitComponents; virtual;
    property View: TKDataView read GetView;
    property ServerStore: TKStore read GetServerStore;
    property ViewTable: TKViewTable read FViewTable;
    function GetFilterExpression: string; virtual;
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

function TKExtDataPanelController.GetFilterExpression: string;
begin
  Result := '';
end;

function TKExtDataPanelController.GetServerStore: TKStore;
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

  FServerStore := Config.GetObject('Sys/ServerStore') as TKStore;
  if FServerStore = nil then
  begin
    FServerStore := FViewTable.CreateStore;
    FOwnsServerStore := True;
  end
  else
    FOwnsServerStore := False;

  Layout := lyBorder;
  Border := False;
  Header := False;

  InitComponents;
  InitServerStore;
  CheckCanRead;
  LoadData;
end;

procedure TKExtDataPanelController.CheckCanRead;
begin
  Assert(View <> nil);

{ TODO : implement GetResourceURI everywhere. }
  //Environment.CheckAccessGranted(View.GetResourceURI, ACM_READ);
end;

procedure TKExtDataPanelController.CreateToolbar;
begin
end;

procedure TKExtDataPanelController.InitComponents;
begin
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
