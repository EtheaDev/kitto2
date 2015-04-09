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
    FView: TKDataView;
    FLiveMode: Boolean;
    procedure DoChange;
    function IsFilterVisible(const AResourceName: string): Boolean;
    function GetFilterResourceURI(const AResourceName: string): string;
    function IsFilterReadOnly(const AResourceName: string): Boolean;
    procedure InvalidateFilter(const AId: string);
  protected
    procedure InitDefaults; override;
  public
    procedure Configure(const AViewTable: TKViewTable; const AConfig: TEFNode);
    function GetFilterExpression: string;
    procedure UpdateObserver(const ASubject: IEFSubject; const AContext: string = ''); override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  /// <summary>
  ///  Provides the filtering feature for contained multi-record data panels.
  /// </summary>
  TKExtListPanelController = class(TKExtDataPanelCompositeController)
  strict private
    FFilterPanel: TKExtFilterPanel;
    FCenterControllerConfig: TEFNode;
    function GetCenterRegionControllerConfig: TEFNode;
    function GetCenterRegionDefaultControllerType: string;
    procedure CreateFilterPanel;
    procedure FilterPanelChange(Sender: TObject);
  strict protected
    procedure InitComponents; override;
    function GetRegionControllerName(const ARegion: TExtBoxComponentRegion): string; override;
    function GetRegionControllerConfig(const ARegion: TExtBoxComponentRegion): TEFNode; override;
    procedure SetViewTable(const AValue: TKViewTable); override;
  protected
    procedure InitDefaults; override;
  public
    function GetFilterExpression: string; override;
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils, StrUtils,
  EF.Localization, EF.StrUtils,
  Kitto.Types, Kitto.Config, Kitto.AccessControl,
  Kitto.Ext.Session, Kitto.Ext.Controller, Kitto.Ext.Filters;

{ TKExtFilterPanel }

function TKExtFilterPanel.GetFilterResourceURI(const AResourceName: string): string;
begin
  Assert(Assigned(FView));

  Result := FView.GetResourceURI + '/Filters/' + AResourceName;
end;

function TKExtFilterPanel.IsFilterVisible(const AResourceName: string): Boolean;
begin
  if AResourceName = '' then
    Result := True
  else
    Result := TKConfig.Instance.IsAccessGranted(GetFilterResourceURI(AResourceName), ACM_VIEW);
end;

function TKExtFilterPanel.IsFilterReadOnly(const AResourceName: string): Boolean;
begin
  if AResourceName = '' then
    Result := False
  else
    Result := not TKConfig.Instance.IsAccessGranted(GetFilterResourceURI(AResourceName), ACM_MODIFY);
end;

procedure TKExtFilterPanel.Configure(const AViewTable: TKViewTable; const AConfig: TEFNode);
var
  LItems: TEFNode;
  I: Integer;
  LNode: TEFNode;
  LFilterResourceName: string;
  LCurrentPanel: TKExtFilterPanel;

  function CreateColumnBreak(const ANode: TEFNode): TKExtFilterPanel;
  var
    LColumnWidth: Double;
  begin
    Result := TKExtFilterPanel.CreateAndAddTo(Items);
    try
      Result.Border := False;
      Result.Layout := lyForm;
      Result.Collapsible := False;
      Result.Frame := False;
      Result.FConnector := FConnector;
      Result.LabelWidth := LabelWidth;
      if Assigned(ANode) then
      begin
        LColumnWidth := ANode.GetFloat('ColumnWidth');
        if LColumnWidth <> 0 then
          Result.ColumnWidth := LColumnWidth;
      end;
    except
      FreeAndNil(Result);
      raise;
    end;
  end;

  procedure SetLabelWidthAndAlign(const ANode: TEFNode; const ADefaultLabelAlign: TExtFormFormPanelLabelAlign);
  var
    LLabelNode: TEFNode;
    LWidth: Integer;
  begin
    LLabelNode := ANode.FindNode('LabelWidth');
    if Assigned(LLabelNode) then
    begin
      if not TryStrToInt(LLabelNode.AsExpandedString, LWidth) then
        raise EKError.CreateFmt(_('Invalid value %s. Valid values: whole numbers.'), [LLabelNode.AsString]);
      LCurrentPanel.LabelWidth := LWidth;
    end;
    LLabelNode := ANode.FindNode('LabelAlign');
    if Assigned(LLabelNode) then
      LCurrentPanel.LabelAlign := OptionAsLabelAlign(LLabelNode.AsString)
    else
      LCurrentPanel.LabelAlign := ADefaultLabelAlign;
  end;

begin
  Assert(Assigned(AViewTable));
  Assert(Assigned(AConfig));

  Title := _(AConfig.GetString('DisplayLabel', _('Search')));
  FConnector := AConfig.GetString('Connector', 'and');
  FView := AViewTable.View;

  LItems := AConfig.GetNode('Items');
  if LItems.FindNode('ColumnBreak') <> nil then
  begin
    Layout := lyHbox;
    LCurrentPanel := CreateColumnBreak(nil);
  end
  else
    LCurrentPanel := Self;

  FLiveMode := LItems.FindNode('ApplyButton') = nil;
  SetLabelWidthAndAlign(AConfig, laRight);
  Self.LabelAlign := LCurrentPanel.LabelAlign;
  Self.LabelWidth := LCurrentPanel.LabelWidth;

  for I := 0 to LItems.ChildCount - 1 do
  begin
    LNode := LItems.Children[I];
    LFilterResourceName := LNode.GetExpandedString('ResourceName');
    if IsFilterVisible(LFilterResourceName) then
    begin
      LNode.SetBoolean('Sys/IsReadOnly', IsFilterReadOnly(LFilterResourceName));
      if SameText(LNode.Name, 'ColumnBreak') then
      begin
        LCurrentPanel := CreateColumnBreak(LNode);
        SetLabelWidthAndAlign(LNode, Self.LabelAlign);
      end
      else
        TKExtFilterFactory.Instance.CreateFilter(LNode, Self, LCurrentPanel.Items, AViewTable);
    end;
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
  LItem: TExtObject;
begin
  Result := '';
  for I := 0 to Items.Count - 1 do
  begin
    LItem := Items[I];
    if Supports(LItem, IKExtFilter, LIntf) then
      LExpression := LIntf.GetExpression
    else if LItem is TKExtFilterPanel then
      LExpression := TKExtFilterPanel(LItem).GetFilterExpression;
    if LExpression <> '' then
    begin
      if Result = '' then
        Result := '(' + LExpression + ')'
      else
        Result := Result + ' ' + FConnector + ' ' + '(' + LExpression + ')';
    end;
  end;
end;

procedure TKExtFilterPanel.InitDefaults;
begin
  inherited;
  Border := False;
  Layout := lyForm;
  Collapsible := True;
  Collapsed := False;
  Frame := True;
  AutoHeight := True;
end;

procedure TKExtFilterPanel.UpdateObserver(const ASubject: IEFSubject; const AContext: string);
begin
  inherited;
  if AContext = IfThen(FLiveMode, 'FilterChanged', 'FilterApplied') then
    DoChange;
  if AContext.StartsWith('FilterInvalidated ') then
    InvalidateFilter(AContext.Split([' '])[1]);
end;

procedure TKExtFilterPanel.InvalidateFilter(const AId: string);
var
  I: Integer;
  LIntf: IKExtFilter;
begin
  for I := 0 to Items.Count - 1 do
  begin
    if Items[I] is TKExtFilterPanel then
      TKExtFilterPanel(Items[I]).InvalidateFilter(AId)
    else if Supports(Items[I], IKExtFilter, LIntf) then
      if SameText(LIntf.GetId, AId) then
        LIntf.Invalidate;
  end;
end;

{ TKExtListPanelController }

function TKExtListPanelController.GetFilterExpression: string;
begin
  Result := inherited GetFilterExpression;
  if Assigned(FFilterPanel) then
    Result := SmartConcat(Result, ' and ', FFilterPanel.GetFilterExpression);
end;

procedure TKExtListPanelController.CreateFilterPanel;
var
  LItems: TEFNode;
begin
  if not ViewTable.IsDetail then
  begin
    LItems := Config.FindNode('Filters/Items');
    if Assigned(LItems) and (LItems.ChildCount > 0) then
    begin
      FFilterPanel := TKExtFilterPanel.CreateAndAddTo(Items);
      FFilterPanel.Region := rgNorth;
      FFilterPanel.OnChange := FilterPanelChange;
      FFilterPanel.Configure(ViewTable, LItems.Parent as TEFNode);
      FFilterPanel.On('afterrender', DoLayout);
    end;
  end;
end;

procedure TKExtListPanelController.FilterPanelChange(Sender: TObject);
begin
  LoadData;
end;

destructor TKExtListPanelController.Destroy;
begin
  FreeAndNil(FCenterControllerConfig);
  inherited;
end;

function TKExtListPanelController.GetCenterRegionControllerConfig: TEFNode;
begin
  Result := Config.FindNode(GetRegionControllerName(rgCenter) + 'Controller');
  if Result <> nil then
  begin
    if Result.AsString = '' then
      Result.AsString := GetCenterRegionDefaultControllerType;
  end
  else
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
  if Title = '' then
    Title := _(Session.Config.MacroExpansionEngine.Expand(ViewTable.PluralDisplayLabel));
end;

procedure TKExtListPanelController.InitDefaults;
begin
  inherited;
  FCenterControllerConfig := TEFNode.Create(GetRegionControllerName(rgCenter) + 'Controller', GetCenterRegionDefaultControllerType);
end;

function TKExtListPanelController.GetCenterRegionDefaultControllerType: string;
begin
  Result := 'GridPanel';
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
