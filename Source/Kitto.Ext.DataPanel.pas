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
  Ext, ExtData,
  EF.Classes,
  Kitto.Metadata.DataView, Kitto.Store,
  Kitto.Ext.Base, Kitto.Ext.Controller, Kitto.Ext.BorderPanel;

type
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
    function GetFilterExpression: string; virtual;
//    function GetRefreshJSCode: string; virtual;
    function GetOrderByClause: string; virtual;
    procedure SetViewTable(const AValue: TKViewTable); virtual;
    procedure CheckCanRead;
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
  public
    destructor Destroy; override;
    procedure LoadData; virtual; abstract;
    procedure RefilterData(const AFilterExpression: string); virtual; abstract;
    property ViewTable: TKViewTable read FViewTable write SetViewTable;
    property ServerStore: TKViewTableStore read FServerStore write FServerStore;
  published
    procedure GetRecordPage;
    procedure RefreshData; virtual; abstract;
  end;

implementation

uses
  ExtPascal,
  SysUtils, StrUtils, Math,
  EF.Tree, EF.Localization,
  Kitto.AccessControl, Kitto.Ext.Session;

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
  CheckCanRead;
end;

procedure TKExtDataPanelController.CheckCanRead;
begin
  Assert(View <> nil);

  Session.Config.CheckAccessGranted(View.GetResourceURI, ACM_READ);
end;

function TKExtDataPanelController.CreateClientReader: TExtDataJsonReader;

  procedure DoAddReaderField(const AReader: TExtDataJsonReader; const AName, AType: string);
  var
    LField: TExtDataField;
  begin
    LField := TExtDataField.AddTo(AReader.Fields);
    LField.Name := AName;
    LField.Type_ := AType;
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

  Result := TExtDataJsonReader.Create(JSObject('')); // Must pass '' otherwise invalid code is generated.
  Result.Root := 'Root';
  Result.TotalProperty := 'Total';

  for I := 0 to ViewTable.FieldCount - 1 do
    AddReaderField(Result, ViewTable.Fields[I]);
end;

function TKExtDataPanelController.CreateClientStore: TExtDataStore;
begin
  Result := TExtDataStore.Create;
  Result.RemoteSort := False;
  Result.Url := MethodURI(GetRecordPage);
end;

procedure TKExtDataPanelController.GetRecordPage;
var
  LStart: Integer;
  LLimit: Integer;
  LTotal: Integer;
  LData: string;
begin
  // Don't refresh if there are pending changes.
  if ServerStore.ChangesPending then
  begin
    LTotal := ServerStore.RecordCount;
    LData := ServerStore.GetAsJSON(True);
  end
  else
  begin
    LStart := Session.QueryAsInteger['start'];
    LLimit := Session.QueryAsInteger['limit'];

    if (LStart <> 0) or (LLimit <> 0) then
    begin
      LTotal := ServerStore.LoadPage(GetFilterExpression, GetOrderByClause, LStart, LLimit);
      LData := ServerStore.GetAsJSON(True);
    end
    else
    begin
      ServerStore.Load(GetFilterExpression, GetOrderByClause);
      LTotal := ServerStore.RecordCount;
      LData := ServerStore.GetAsJSON(True, 0, Min(GetMaxRecords(), ServerStore.RecordCount));
    end;
  end;
  Session.Response := Format('{Total:%d,Root:%s}', [LTotal, LData]);
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

end.
