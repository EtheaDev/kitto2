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
  SysUtils,
  Ext.Base,
  EF.ObserverIntf,
  Kitto.Ext.Base, Kitto.Ext.Controller, Kitto.Ext.DataPanel;

type
  /// <summary>
  ///  Base class for concrete data panels that handle database records.
  /// </summary>
  TKExtDataPanelLeafController = class abstract(TKExtDataPanelController)
  strict private
    FRefreshButton: TKExtButton;
  strict protected
    function IsClientStoreAutoLoadEnabled: Boolean; virtual;
    procedure AddTopToolbarButtons; override;
    procedure ExecuteNamedAction(const AActionName: string); override;
    function GetParentDataPanel: TKExtDataPanelController;
    function IsActionSupported(const AActionName: string): Boolean; override;
  public
    procedure InitActionController(const AAction: TKExtActionButton; const AController: IKExtController); override;
    procedure UpdateObserver(const ASubject: IEFSubject; const AContext: string = ''); override;
  published
    procedure LoadData; override;
    procedure DoDisplay; override;
  end;

implementation

uses
  StrUtils
  , EF.Localization
  , Kitto.Metadata.Views
  , Kitto.AccessControl
  , Kitto.Web.Response
  ;

{ TKExtDataPanelLeafController }

procedure TKExtDataPanelLeafController.DoDisplay;
var
  LActionCommand: string;
begin
  inherited;
  LActionCommand := ParamAsString('action');
(* TODO multiple actions separated by comma: actually don't work
  while LActionCommand <> '' do
  begin
    p := pos(',', LActionCommand);
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
*)
  if LActionCommand <> '' then
  begin
{ TODO : cannot change query fields - put mutable data elsewhere }
//    Session.Query['action'] := '';
    ExecuteNamedAction(LActionCommand);
  end;
end;

procedure TKExtDataPanelLeafController.ExecuteNamedAction(const AActionName: string);
begin
  if (AActionName = 'Refresh') and Assigned(FRefreshButton) then
    FRefreshButton.PerformClick
  else
    inherited;
end;

function TKExtDataPanelLeafController.GetParentDataPanel: TKExtDataPanelController;
begin
  Result := TKExtDataPanelController(Config.GetObject('Sys/ParentDataPanel', Self));

  Assert(Assigned(Result));
end;

procedure TKExtDataPanelLeafController.InitActionController(
  const AAction: TKExtActionButton; const AController: IKExtController);
var
  LParentDataPanel: TKExtDataPanelController;
begin
  inherited;
  LParentDataPanel := GetParentDataPanel;
  if Assigned(LParentDataPanel) then
    LParentDataPanel.InitActionController(AAction, AController);
end;

function TKExtDataPanelLeafController.IsActionSupported(const AActionName: string): Boolean;
begin
  Result := MatchText(AActionName, ['Refresh']);
end;

procedure TKExtDataPanelLeafController.LoadData;
begin
  inherited;
  if not IsClientStoreAutoLoadEnabled then
  begin
    Assert(Assigned(ClientStore));

    ClientStore.Load(JSObject('params:{start:0,limit:0,Obj:"' + JSName + '"}'));
  end;
end;

function TKExtDataPanelLeafController.IsClientStoreAutoLoadEnabled: Boolean;
begin
  Result := False;
end;

procedure TKExtDataPanelLeafController.UpdateObserver(
  const ASubject: IEFSubject; const AContext: string);
begin
  inherited;
  if AContext = 'RefreshAllRecords' then
    GetParentDataPanel.LoadData;
end;

procedure TKExtDataPanelLeafController.AddTopToolbarButtons;
begin
  inherited;
//  TExtToolbarSpacer.CreateAndAddTo(TopToolbar.Items);
  FRefreshButton := AddTopToolbarButton('Refresh', _('Refresh data'), 'refresh', False);
  if Assigned(FRefreshButton) then
  begin
    //FRefreshButton.On('click', Ajax(GetParentDataPanel.LoadData));
    FRefreshButton.On('click', TKWebResponse.Current.Items.AjaxCallMethod(Self).SetMethod(GetParentDataPanel.LoadData).AsFunction);
    if ViewTable.GetBoolean('Controller/PreventRefreshing') then
      FRefreshButton.Hidden := True;
  end;
end;

end.
