{-------------------------------------------------------------------------------
   Copyright 2012 Ethea S.r.l.

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

unit Kitto.Ext.DataPanel;

{$I Kitto.Defines.inc}

interface

uses
  Ext, ExtPascal, ExtPascalUtils, ExtData,
  EF.Classes,
  Kitto.Metadata.Views, Kitto.Metadata.DataView, Kitto.Store,
  Kitto.Ext.Base, Kitto.Ext.Controller, Kitto.Ext.BorderPanel;

type
  TKExtDataActionButton = class(TKExtActionButton)
  strict private
    FServerStore: TKViewTableStore;
    FViewTable: TKViewTable;
  strict protected
    procedure InitController(const AController: IKExtController); override;
  public
    property ViewTable: TKViewTable read FViewTable write FViewTable;
    property ServerStore: TKViewTableStore read FServerStore write FServerStore;
  published
    procedure ExecuteActionOnSelectedRows;
  end;

  ///	<summary>Base class for controllers that handle database records (either
  ///	single records or record sets).</summary>
  TKExtDataPanelController = class abstract(TKExtBorderPanelController)
  strict private
    FServerStore: TKViewTableStore;
    FClientStore: TExtDataStore;
    FClientReader: TExtDataJsonReader;
    FViewTable: TKViewTable;
    FOwnsServerStore: Boolean;
    function GetView: TKDataView;
    function GetMaxRecords: Integer;
  strict protected
    procedure CheckCanRead;
//    function GetRefreshJSCode: string; virtual;
    function GetOrderByClause: string; virtual;
    procedure SetViewTable(const AValue: TKViewTable); virtual;
    procedure CreateToolbar; virtual;
    procedure DoDisplay; override;
    procedure InitComponents; virtual;
    procedure InitDefaults; override;
    procedure InitSubController(const AController: IKExtController); override;
    property View: TKDataView read GetView;
    property ClientStore: TExtDataStore read FClientStore;
    property ClientReader: TExtDataJsonReader read FClientReader;
    function CreateClientStore: TExtDataStore; virtual;
    function CreateClientReader: TExtDataJsonReader; virtual;
    function AddActionButton(const AView: TKView;
      const AToolbar: TExtToolbar): TKExtActionButton; override;
    function GetSelectConfirmCall(const AMessage: string;
      const AMethod: TExtProcedure): string; virtual;
    function GetSelectCall(const AMethod: TExtProcedure): TExtFunction; virtual;
    function AutoLoadData: Boolean; virtual;
    function GetParentDataPanel: TKExtDataPanelController;
    function GetRootDataPanel: TKExtDataPanelController;
    function FindViewLayout(const ALayoutName: string): TKLayout;
  public
    destructor Destroy; override;
    property ViewTable: TKViewTable read FViewTable write SetViewTable;
    property ServerStore: TKViewTableStore read FServerStore write FServerStore;
    function GetFilterExpression: string; virtual;
  published
    procedure GetRecordPage;
    procedure LoadData; virtual; abstract;
  end;

implementation

uses
  SysUtils, StrUtils, Math,
  EF.StrUtils, EF.Tree, EF.Localization,
  Kitto.Types, Kitto.AccessControl, Kitto.Ext.Session;

{ TKExtDataPanelController }

//function TKExtDataPanelController.GetRefreshJSCode: string;
//begin
//  Assert(Assigned(ClientStore));
//
//  Result := ClientStore.JSName + '.load({params:{start:0,limit:0,Obj:"' + JSName + '"}});';
//end;

destructor TKExtDataPanelController.Destroy;
begin
  if FOwnsServerStore then
    FreeAndNil(FServerStore);
  inherited;
end;

function TKExtDataPanelController.GetView: TKDataView;
begin
  Result := inherited GetView as TKDataView;
end;

procedure TKExtDataPanelController.DoDisplay;
var
  LViewTable: TKViewTable;
begin
  IconCls := Session.SetViewIconStyle(View);

  Assert(View is TKDataView);

  LViewTable := Config.GetObject('Sys/ViewTable') as TKViewTable;
  if LViewTable = nil then
    LViewTable := View.MainTable;
  Assert(Assigned(LViewTable));

  FServerStore := Config.GetObject('Sys/ServerStore') as TKViewTableStore;
  if FServerStore = nil then
  begin
    FServerStore := LViewTable.CreateStore;
    FOwnsServerStore := True;
  end
  else
    FOwnsServerStore := False;
  Assert(Assigned(FServerStore));

  ViewTable := LViewTable;

  inherited; // Creates subcontrollers.

  InitComponents;
end;

function TKExtDataPanelController.FindViewLayout(
  const ALayoutName: string): TKLayout;
var
  LLayoutName: string;
  LLayoutNode: TEFNode;
begin
  LLayoutNode := ViewTable.FindNode('Controller/' + ALayoutName + '/Layout');
  if Assigned(LLayoutNode) then
  begin
    LLayoutName := LLayoutNode.AsString;
    if LLayoutName <> '' then
      Result := View.Catalog.Layouts.FindLayout(LLayoutName)
    else
      Result := View.Catalog.Layouts.FindLayoutByNode(LLayoutNode);
  end
  else
    Result := ViewTable.FindLayout(ALayoutName);
end;

function TKExtDataPanelController.GetSelectConfirmCall(const AMessage: string; const AMethod: TExtProcedure): string;
begin
  raise EKError.Create(_('Actions that require selection are not supported in this controller.'));
end;

function TKExtDataPanelController.GetSelectCall(const AMethod: TExtProcedure): TExtFunction;
begin
  raise EKError.Create(_('Actions that require selection are not supported in this controller.'));
end;

function TKExtDataPanelController.AddActionButton(const AView: TKView;
  const AToolbar: TExtToolbar): TKExtActionButton;
var
  LConfirmationMessage: string;
  LRequireSelection: Boolean;
  LConfirmationJS: string;
begin
  Assert(Assigned(AView));
  Assert(Assigned(AToolbar));
  Assert(Assigned(ViewTable));
  Assert(Assigned(ServerStore));

  Result := TKExtDataActionButton.CreateAndAddTo(AToolbar.Items);
  Result.View := AView;
  Result.ActionObserver := Self;
  TKExtDataActionButton(Result).ViewTable := ViewTable;
  TKExtDataActionButton(Result).ServerStore := ServerStore;

  // A Tool may or may not have a confirmation message and may or may not require
  // a selected row. We must handle all combinations.
  LConfirmationMessage := AView.GetExpandedString('Controller/ConfirmationMessage');
  LRequireSelection := AView.GetBoolean('Controller/RequireSelection', True);

  if LRequireSelection then
    LConfirmationJS := GetSelectConfirmCall(LConfirmationMessage, TKExtDataActionButton(Result).ExecuteActionOnSelectedRows)
  else
    LConfirmationJS := GetConfirmCall(LConfirmationMessage, Result.ExecuteButtonAction);

  if LConfirmationMessage <> '' then
    Result.Handler := JSFunction(LConfirmationJS)
  else if LRequireSelection then
    Result.On('click', GetSelectCall(TKExtDataActionButton(Result).ExecuteActionOnSelectedRows))
  else
    Result.On('click', Ajax(Result.ExecuteButtonAction, []));
end;

function TKExtDataPanelController.CreateClientReader: TExtDataJsonReader;

  procedure DoAddReaderField(const AReader: TExtDataJsonReader; const AName, AType: string);
  var
    LField: TExtDataField;
  begin
    LField := TExtDataField.CreateAndAddTo(AReader.Fields);
    LField.Name := AName;
    LField.&Type := AType;
  end;

  procedure AddReaderField(const AReader: TExtDataJsonReader; const AViewField: TKViewField);
  var
    I: Integer;
  begin
    DoAddReaderField(AReader, AViewField.AliasedName, AViewField.DataType.GetJSTypeName);
    if AViewField.IsReference then
    begin
      for I := 0 to AViewField.ModelField.FieldCount - 1 do
        DoAddReaderField(AReader, AViewField.ModelField.Fields[I].FieldName,
          AViewField.ModelField.Fields[I].DataType.GetJSTypeName);
    end;
  end;
var
  I: Integer;

begin
  Assert(Assigned(ViewTable));

  Result := TExtDataJsonReader.Create(Self, JSObject('')); // Must pass '' otherwise invalid code is generated.
  Result.Root := 'Root';
  Result.TotalProperty := 'Total';
  Result.MessageProperty := 'Msg';
  Result.SuccessProperty := 'Success';

  for I := 0 to ViewTable.FieldCount - 1 do
    AddReaderField(Result, ViewTable.Fields[I]);
end;

function TKExtDataPanelController.CreateClientStore: TExtDataStore;
begin
  Result := TExtDataStore.Create(Self);
  Result.RemoteSort := False;
  Result.Url := MethodURI(GetRecordPage);
  Result.On('exception', JSFunction('proxy, type, action, options, response, arg', 'loadError(type, action, response);'));
end;

procedure TKExtDataPanelController.GetRecordPage;
var
  LStart: Integer;
  LLimit: Integer;
  LTotal: Integer;
  LData: string;
begin
  try
    // Don't refresh if there are pending changes.
    if ServerStore.ChangesPending then
    begin
      LTotal := ServerStore.RecordCountExceptNewAndDeleted;
      LData := ServerStore.GetAsJSON(True);
    end
    else
    begin
      LStart := Session.QueryAsInteger['start'];
      LLimit := Session.QueryAsInteger['limit'];

      LTotal := ViewTable.Model.LoadRecords(ServerStore, GetRootDataPanel.GetFilterExpression, GetOrderByClause, LStart, LLimit);
      if (LStart <> 0) or (LLimit <> 0) then
        LData := ServerStore.GetAsJSON(True)
      else
        // When loading all records, apply a limit on the display.
        { TODO : If there's a limit on the display of records, try to pass it over and only load
          needed records into the store. }
        LData := ServerStore.GetAsJSON(True, 0, Min(GetMaxRecords(), LTotal));
    end;
    Session.ResponseItems.AddJSON(Format('{Success: true, Total: %d, Root: %s}', [LTotal, LData]));
  except
    on E: Exception do
    begin
      Session.ResponseItems.Clear;
      Session.ResponseItems.AddJSON(Format('{Success: false, Msg: "%s", Root: []}', [E.Message]));
    end;
  end;
end;

function TKExtDataPanelController.GetRootDataPanel: TKExtDataPanelController;
begin
  Result := GetParentDataPanel;
  if Result = nil then
    Result := Self
  else
  begin
    while (Result.GetParentDataPanel <> nil) do
      Result := Result.GetParentDataPanel;
  end;
end;

function TKExtDataPanelController.GetMaxRecords: Integer;
begin
  Assert(ViewTable <> nil);

  if ViewTable.Model.IsLarge then
    Result := ViewTable.GetInteger('MaxRecords', 100)
  else
    Result := 1000;
end;

function TKExtDataPanelController.GetFilterExpression: string;
begin
  Result := '';
end;

function TKExtDataPanelController.GetOrderByClause: string;
begin
  { TODO : provide default ordering when not grouping? }
  Result := '';
end;

function TKExtDataPanelController.GetParentDataPanel: TKExtDataPanelController;
begin
  Result := TKExtDataPanelController(Config.GetObject('Sys/ParentDataPanel'));
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
end;

procedure TKExtDataPanelController.InitSubController(
  const AController: IKExtController);
begin
  inherited;
  Assert(Assigned(AController));
  Assert(Assigned(FViewTable));
  Assert(Assigned(FServerStore));

  AController.Config.SetObject('Sys/ViewTable', FViewTable);
  AController.Config.SetObject('Sys/ServerStore', FServerStore);
end;

procedure TKExtDataPanelController.SetViewTable(const AValue: TKViewTable);
begin
  FViewTable := AValue;

  FClientStore := CreateClientStore;
  FClientReader := CreateClientReader;
  FClientStore.Reader := FClientReader;
end;

{ TKExtDataActionButton }

procedure TKExtDataActionButton.ExecuteActionOnSelectedRows;
var
  LRecord: TKViewTableRecord;
  LController: IKExtController;
begin
  Assert(Assigned(View));
  Assert(Assigned(FViewTable));
  Assert(Assigned(FServerStore));
  Assert(Assigned(ActionObserver));

  LRecord := FServerStore.GetRecord(Session.GetQueries, Session.Config.JSFormatSettings, 0);
  LController := TKExtControllerFactory.Instance.CreateController(
    Session.ObjectCatalog, View, nil, nil, ActionObserver);
  InitController(LController);
  LController.Config.SetObject('Sys/Record', LRecord);
  LController.Display;
end;

procedure TKExtDataActionButton.InitController(
  const AController: IKExtController);
begin
  inherited;
  Assert(Assigned(FViewTable));
  Assert(Assigned(FServerStore));
  Assert(Assigned(AController));

  AController.Config.SetObject('Sys/ServerStore', FServerStore);
  AController.Config.SetObject('Sys/ViewTable', FViewTable);
end;

function TKExtDataPanelController.AutoLoadData: Boolean;
begin
  Assert(ViewTable <> nil);

  Result := ViewTable.GetBoolean('Controller/AutoOpen', not ViewTable.Model.IsLarge);
end;

procedure TKExtDataPanelController.CheckCanRead;
begin
  Assert(ViewTable <> nil);

  Session.Config.CheckAccessGranted(ViewTable.GetResourceURI, ACM_READ);
end;

end.
