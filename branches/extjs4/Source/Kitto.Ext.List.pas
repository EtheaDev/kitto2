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

unit Kitto.Ext.List;

{$I Kitto.Defines.inc}

interface

uses
  Classes,
  Ext, ExtPascal,
  EF.Tree, EF.ObserverIntf,
  Kitto.Metadata.DataView, Kitto.Ext.Base, Kitto.Ext.DataPanelComposite;

type
  TKExtFilterPanel = class(TKExtPanelBase)
  private
    FConnector: string;
    FOnChange: TNotifyEvent;
    procedure DoChange;
  protected
    procedure InitDefaults; override;
  public
    procedure Configure(const AConfig: TEFNode);
    function GetFilterExpression: string;
    procedure UpdateObserver(const ASubject: IEFSubject;
      const AContext: string = ''); override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  ///	<summary>Provides the filtering feature for contained multi-record data
  ///	panels.</summary>
  TKExtListPanelController = class(TKExtDataPanelCompositeController)
  strict private
    FFilterPanel: TKExtFilterPanel;
    FCenterControllerConfig: TEFNode;
    function GetCenterRegionControllerConfig: TEFNode;
    procedure CreateFilterPanel;
    procedure FilterPanelChange(Sender: TObject);
  strict protected
    procedure InitComponents; override;
    function GetRegionControllerName(const ARegion: TExtBoxComponentRegion): string; override;
    function GetRegionControllerConfig(const ARegion: TExtBoxComponentRegion): TEFNode; override;
    function GetFilterExpression: string; override;
    procedure SetViewTable(const AValue: TKViewTable); override;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  EF.Localization,
  Kitto.Ext.Session, Kitto.Ext.Controller, Kitto.Ext.Filters;

{ TKExtFilterPanel }

procedure TKExtFilterPanel.Configure(const AConfig: TEFNode);
var
  LItems: TEFNode;
  I: Integer;
begin
  Assert(Assigned(AConfig));

  Title := _(AConfig.GetString('DisplayLabel', _('Search')));
  FConnector := AConfig.GetString('Connector', 'and');

  LItems := AConfig.GetNode('Items');
  for I := 0 to LItems.ChildCount - 1 do
  begin
    // Currently unused.
//    LItems.Children[I].SetString('Sys/ApplyJSCode', GetRefreshJSCode);
    TKExtFilterFactory.Instance.CreateFilter(LItems.Children[I], Self, Items);
  end;
end;

procedure TKExtFilterPanel.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

function TKExtFilterPanel.GetFilterExpression: string;
var
  LIntf: IKExtFilter;
  I: Integer;
  LExpression: string;
begin
  Result := '';
  for I := 0 to Items.Count - 1 do
  begin
    if Supports(Items[I], IKExtFilter, LIntf) then
    begin
      LExpression := LIntf.GetExpression;
      if LExpression <> '' then
      begin
        if Result = '' then
          Result := '(' + LExpression + ')'
        else
          Result := Result + ' ' + FConnector + ' ' + '(' + LExpression + ')';
      end;
    end;
  end;
end;

procedure TKExtFilterPanel.InitDefaults;
begin
  inherited;
  Border := False;
  Layout := lyForm;
  Collapsible := True;
  Frame := True;
  AutoHeight := True;
end;

procedure TKExtFilterPanel.UpdateObserver(const ASubject: IEFSubject;
  const AContext: string);
begin
  inherited;
  if AContext = 'FilterChanged' then
    DoChange;
end;

{ TKExtListPanelController }

function TKExtListPanelController.GetFilterExpression: string;
begin
  if Assigned(FFilterPanel) then
    Result := FFilterPanel.GetFilterExpression
  else
    Result := inherited GetFilterExpression;
end;

procedure TKExtListPanelController.CreateFilterPanel;
var
  LItems: TEFNode;
begin
  LItems := Config.FindNode('Filters/Items');
  if Assigned(LItems) and (LItems.ChildCount > 0) then
  begin
    FFilterPanel := TKExtFilterPanel.AddTo(Items);
    FFilterPanel.Region := rgNorth;
    FFilterPanel.OnChange := FilterPanelChange;
    FFilterPanel.Configure(LItems.Parent as TEFNode);
  end;
end;

procedure TKExtListPanelController.FilterPanelChange(Sender: TObject);
begin
  RefilterData((Sender as TKExtFilterPanel).GetFilterExpression);
end;

procedure TKExtListPanelController.AfterConstruction;
begin
  inherited;
  FCenterControllerConfig := TEFNode.Create('CenterController', 'GridPanel');
end;

destructor TKExtListPanelController.Destroy;
begin
  FreeAndNil(FCenterControllerConfig);
  inherited;
end;

function TKExtListPanelController.GetCenterRegionControllerConfig: TEFNode;
begin
  Result := Config.FindNode('CenterController');
  if Result = nil then
    Result := FCenterControllerConfig;
end;

function TKExtListPanelController.GetRegionControllerConfig(
  const ARegion: TExtBoxComponentRegion): TEFNode;
begin
  if ARegion = rgCenter then
    Result := GetCenterRegionControllerConfig
  else if (ARegion = rgNorth) and Assigned(Config.FindNode('Filters/Items')) then
    // Preserve the filter panel's region.
    Result := nil
  else
    Result := inherited GetRegionControllerConfig(ARegion);
end;

function TKExtListPanelController.GetRegionControllerName(
  const ARegion: TExtBoxComponentRegion): string;
begin
  if ARegion = rgCenter then
    Result := 'GridPanel'
  else if (ARegion = rgNorth) and Assigned(Config.FindNode('Filters/Items')) then
    // Preserve the filter panel's region.
    Result := ''
  else
    Result := inherited GetRegionControllerName(ARegion);
end;

procedure TKExtListPanelController.InitComponents;
begin
  inherited;
  Title := _(Session.Config.MacroExpansionEngine.Expand(ViewTable.PluralDisplayLabel));
end;

procedure TKExtListPanelController.SetViewTable(const AValue: TKViewTable);
begin
  inherited;
  CreateFilterPanel;
end;

initialization
  TKExtControllerRegistry.Instance.RegisterClass('List', TKExtListPanelController);

finalization
  TKExtControllerRegistry.Instance.UnregisterClass('List');

end.
