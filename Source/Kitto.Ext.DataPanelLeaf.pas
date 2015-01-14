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
  Ext,
  EF.ObserverIntf,
  Kitto.Ext.DataPanel;

type
  ///	<summary>Base class for concrete data panels that handle database records.</summary>
  TKExtDataPanelLeafController = class abstract(TKExtDataPanelController)
  strict private
    FRefreshButton: TExtButton;
  strict protected
    procedure PerformDelayedClick(const AButton: TExtButton);
    procedure AddTopToolbarButtons; override;
    procedure ExecuteNamedAction(const AActionName: string); override;
  public
    procedure UpdateObserver(const ASubject: IEFSubject;
      const AContext: string = ''); override;
  published
    procedure LoadData; override;
    procedure DoDisplay; override;
  end;

implementation

uses
  ExtPascal,
  EF.Localization,
  Kitto.AccessControl, Kitto.Ext.Session;

{ TKExtDataPanelLeafController }

procedure TKExtDataPanelLeafController.PerformDelayedClick(const AButton: TExtButton);
begin
  if Assigned(AButton) then
    AButton.On('render', JSFunction(AButton.PerformClick));
end;

procedure TKExtDataPanelLeafController.DoDisplay;
var
  p: Integer;
  LActionCommand, LSingleAction: string;
begin
  inherited;
  LActionCommand := Session.Queries.Values['action'];
  while LActionCommand <> '' do
  begin
    p := pos(' ', LActionCommand);
    if p > 0 then
    begin
      LSingleAction := Copy(LActionCommand,1,p-1);
      LActionCommand := Copy(LActionCommand,p+1,MaxInt);
    end
    else
    begin
      LSingleAction := LActionCommand;
      LActionCommand := '';
    end;
    ExecuteNamedAction(LSingleAction);
  end;
end;

procedure TKExtDataPanelLeafController.ExecuteNamedAction(
  const AActionName: string);
begin
  inherited;
  if (AActionName = 'Refresh') then
    PerformDelayedClick(FRefreshButton)
  { TODO : Support custom actions as well? }
  else
    inherited;
end;

procedure TKExtDataPanelLeafController.LoadData;
begin
  inherited;
  Assert(Assigned(ClientStore));

  ClientStore.Load(JSObject('params:{start:0,limit:0,Obj:"' + JSName + '"}'));
end;

procedure TKExtDataPanelLeafController.UpdateObserver(
  const ASubject: IEFSubject; const AContext: string);
begin
  inherited;
  if (AContext = 'RefreshAllRecords') or (AContext = 'RefreshCurrentRecord') then
    TKExtDataPanelController(Config.GetObject('Sys/ParentDataPanel', Self)).LoadData;
end;

procedure TKExtDataPanelLeafController.AddTopToolbarButtons;
begin
  TExtToolbarSpacer.CreateAndAddTo(TopToolbar.Items);
  FRefreshButton := TExtButton.CreateAndAddTo(TopToolbar.Items);
  FRefreshButton.Icon := Session.Config.GetImageURL('refresh');
  FRefreshButton.Handler := Ajax(TKExtDataPanelController(Config.GetObject('Sys/ParentDataPanel', Self)).LoadData);
  FRefreshButton.Tooltip := _('Refresh data');
  inherited;
end;

end.
