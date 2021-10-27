{-------------------------------------------------------------------------------
   Copyright 2012-2021 Ethea S.r.l.

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
  Ext.Base,
  EF.Tree, EF.ObserverIntf,
  Kitto.JS.Controller, Kitto.Metadata.DataView, Kitto.Ext.Base,
  Kitto.Ext.DataPanelComposite;

type
  TKExtFilterPanel = class(TKExtPanelBase)
  private
    FConnector: string;
    FOnChange: TNotifyEvent;
    FView: TKDataView;
    FLiveMode: Boolean;
    procedure DoChange;
    function IsFilterVisible(const AACName: string): Boolean;
    function GetFilterACURI(const AACName: string): string;
    function IsFilterReadOnly(const AACName: string): Boolean;
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
    procedure CreateFilterPanel;
    procedure FilterPanelChange(Sender: TObject);
  strict protected
    procedure InitComponents; override;
    procedure SetViewTable(const AValue: TKViewTable); override;
    function GetRegionDefaultControllerClass(const ARegion: string): string; override;
    function GetObjectNamePrefix: string; override;
  public
    function GetFilterExpression: string; override;
  end;

implementation

uses
  SysUtils
  , StrUtils
  , EF.Localization
  , EF.Macros
  , EF.StrUtils
  , Kitto.Types
  , Kitto.Config
  , Kitto.Auth
  , Kitto.AccessControl
  , Kitto.Web.Application
  , Kitto.Ext.Filters
  ;

{ TKExtFilterPanel }

function TKExtFilterPanel.GetFilterACURI(const AACName: string): string;
begin
  Assert(Assigned(FView));

  Result := FView.GetACURI + '/Filters/' + AACName;
end;

function TKExtFilterPanel.IsFilterVisible(const AACName: string): Boolean;
begin
  if AACName = '' then
    Result := True
  else
    Result := TKAccessController.Current.IsAccessGranted(TKAuthenticator.Current.UserName, GetFilterACURI(AACName), ACM_VIEW);
end;

function TKExtFilterPanel.IsFilterReadOnly(const AACName: string): Boolean;
begin
  if AACName = '' then
    Result := False
  else
    Result := not TKAccessController.Current.IsAccessGranted(TKAuthenticator.Current.UserName, GetFilterACURI(AACName), ACM_MODIFY);
end;

procedure TKExtFilterPanel.Configure(const AViewTable: TKViewTable; const AConfig: TEFNode);
var
  LItems: TEFNode;
  I: Integer;
  LNode: TEFNode;
  LFilterACName: string;
  LCurrentPanel: TKExtFilterPanel;

  function CreateColumnBreak(const ANode: TEFNode): TKExtFilterPanel;
  var
    LColumnWidth: Double;
  begin
    Result := TKExtFilterPanel.CreateAndAddToArray(Items);
    try
      Result.Border := False;
      Result.Layout := 'form';
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

  procedure SetLabelWidthAndAlign(const ANode: TEFNode; const ADefaultLabelAlign: TExtContainerLabelAlign);
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
    Layout := 'hbox';
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
    LFilterACName := LNode.GetExpandedString('ACName');
    if LFilterACName = '' then
      LFilterACName := LNode.GetExpandedString('ResourceName');
    if IsFilterVisible(LFilterACName) then
    begin
      LNode.SetBoolean('Sys/IsReadOnly', IsFilterReadOnly(LFilterACName));
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
  Layout := 'form';
  Collapsible := True;
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

function TKExtListPanelController.GetObjectNamePrefix: string;
begin
  Result := 'list';
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
      FFilterPanel := TKExtFilterPanel.CreateAndAddToArray(Items);
      FFilterPanel.Collapsible := Config.GetBoolean('Filters/Collapsible', True);
      FFilterPanel.Collapsed := Config.GetBoolean('Filters/Collapsed', False);
      FFilterPanel.Region := 'north';
      FFilterPanel.OnChange := FilterPanelChange;
      FFilterPanel.Configure(ViewTable, LItems.Parent as TEFNode);
      FFilterPanel.On('afterrender', GenerateAnonymousFunction(UpdateLayout));
    end;
  end;
end;

procedure TKExtListPanelController.FilterPanelChange(Sender: TObject);
begin
  LoadData;
end;

procedure TKExtListPanelController.InitComponents;
var
  LTitle: string;
begin
  inherited;
  if Title = '' then
  begin
    LTitle := ViewTable.PluralDisplayLabel;
    TEFMacroExpansionEngine.Instance.Expand(LTitle);
    Title := _(LTitle);
  end;
end;

function TKExtListPanelController.GetRegionDefaultControllerClass(const ARegion: string): string;
begin
  if ARegion = 'Center' then
    Result := 'GridPanel'
  else
    Result := inherited GetRegionDefaultControllerClass(ARegion);
end;

procedure TKExtListPanelController.SetViewTable(const AValue: TKViewTable);
begin
  inherited;
  CreateFilterPanel;
end;

initialization
  TJSControllerRegistry.Instance.RegisterClass('List', TKExtListPanelController);

finalization
  TJSControllerRegistry.Instance.UnregisterClass('List');

end.
