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

unit Kitto.Ext.DataPanelLeaf;

{$I Kitto.Defines.inc}

interface

uses
  Kitto.Ext.DataPanel;

type
  ///	<summary>Base class for concrete data panels that handle database records.</summary>
  TKExtDataPanelLeafController = class abstract(TKExtDataPanelController)
  strict private
    FFilterExpression: string;
  strict protected
    procedure CheckCanRead;
    function AutoLoadData: Boolean; virtual;
    procedure DoDisplay; override;
    function GetFilterExpression: string; override;
    procedure AddTopToolbarButtons; override;
  public
    procedure RefilterData(const AFilterExpression: string); override;
    procedure LoadData; override;
  published
    procedure RefreshData; override;
  end;

implementation

uses
  Ext, ExtPascal,
  EF.Localization,
  Kitto.AccessControl, Kitto.Ext.Session;

{ TKExtDataPanelLeafController }

procedure TKExtDataPanelLeafController.DoDisplay;
begin
  inherited;
  CheckCanRead;
  if AutoLoadData then
    LoadData;
end;

function TKExtDataPanelLeafController.GetFilterExpression: string;
begin
  Result := FFilterExpression;
end;

procedure TKExtDataPanelLeafController.LoadData;
begin
  inherited;
  RefreshData;
end;

procedure TKExtDataPanelLeafController.RefilterData(
  const AFilterExpression: string);
begin
  inherited;
  FFilterExpression := AFilterExpression;
  RefreshData;
end;

procedure TKExtDataPanelLeafController.RefreshData;
begin
  inherited;
  Assert(Assigned(ClientStore));

  ClientStore.Load(JSObject('params:{start:0,limit:0,Obj:"' + JSName + '"}'));
end;

procedure TKExtDataPanelLeafController.AddTopToolbarButtons;
var
  LRefreshButton: TExtButton;
begin
  TExtToolbarSpacer.AddTo(TopToolbar.Items);
  LRefreshButton := TExtButton.AddTo(TopToolbar.Items);
  LRefreshButton.Tooltip := _('Refresh');
  LRefreshButton.Icon := Session.Config.GetImageURL('refresh');
  LRefreshButton.Handler := Ajax(TKExtDataPanelController(Config.GetObject('Sys/RefreshHandler', Self)).RefreshData);
  LRefreshButton.Tooltip := _('Refresh data');
  inherited;
end;

function TKExtDataPanelLeafController.AutoLoadData: Boolean;
begin
  Assert(ViewTable <> nil);

  Result := ViewTable.GetBoolean('Controller/AutoOpen', not ViewTable.Model.IsLarge);
end;

procedure TKExtDataPanelLeafController.CheckCanRead;
begin
  Assert(ViewTable <> nil);

  Session.Config.CheckAccessGranted(ViewTable.GetResourceURI, ACM_READ);
end;

end.
