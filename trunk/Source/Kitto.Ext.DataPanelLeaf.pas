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
  strict protected
    procedure AddTopToolbarButtons; override;
  published
    procedure LoadData; override;
  end;

implementation

uses
  Ext, ExtPascal,
  EF.Localization,
  Kitto.AccessControl, Kitto.Ext.Session;

{ TKExtDataPanelLeafController }

procedure TKExtDataPanelLeafController.LoadData;
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
  LRefreshButton.Icon := Session.Config.GetImageURL('refresh');
  LRefreshButton.Handler := Ajax(TKExtDataPanelController(Config.GetObject('Sys/ParentDataPanel', Self)).LoadData);
  LRefreshButton.Tooltip := _('Refresh data');
  inherited;
end;

end.
