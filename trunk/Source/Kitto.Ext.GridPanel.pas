{-------------------------------------------------------------------------------
   Copyright 2011 Ethea S.r.l.

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

unit Kitto.Ext.GridPanel;

{$I Kitto.Defines.inc}

interface

uses
  ExtPascal, Ext, ExtForm, ExtData, ExtGrid, ExtPascalUtils,
  EF.ObserverIntf,
  Kitto.Metadata.DataView, Kitto.Store, Kitto.Types,
  Kitto.Ext.Base;

type
  TKExtFilterPanel = class(TKExtPanelBase)
  private
    FConnector: string;
  protected
    procedure InitDefaults; override;
  public
    property Connector: string read FConnector write FConnector;
    function GetFilterExpression: string;
  end;

  TKExtGridPanel = class(TKExtPanelBase)
  private
    FGridPanel: TExtGridEditorGridPanel;
    FViewTable: TKViewTable;
    FIsAddAllowed: Boolean;
    FIsEditAllowed: Boolean;
    FIsDeleteAllowed: Boolean;
    FStore: TExtDataStore;
    FReader: TExtDataJsonReader;
    FGridView: TExtGridGridView;
    FEditHostWindow: TKExtModalWindow;
    FPagingToolbar: TExtPagingToolbar;
    FTopToolbar: TExtToolbar;
    FFilterPanel: TKExtFilterPanel;
    FPageRecordCount: Integer;
    FServerStore: TKViewTableStore;
    FSelModel: TExtGridRowSelectionModel;
    FIsAddVisible: Boolean;
    FIsDeleteVisible: Boolean;
    procedure SetViewTable(const AValue: TKViewTable);
    procedure CreateFilterPanel;
    procedure CreateStoreAndView;
    function GetRefreshJSCode: string;
    function GetFilterExpression: string;
    function GetGroupingFieldName: string;
    function CreateTopToolbar: TExtToolbar;
    function CreatePagingToolbar: TExtPagingToolbar;
    function LocateRecordFromSession: TKViewTableRecord;
    procedure ShowEditWindow(const ARecord: TKRecord;
      const AEditMode: TKEditMode);
    function GetSelectConfirmCall(const AMessage: string; const AMethod: TExtProcedure): string;
    function IsReadOnly: Boolean;
    function GetOrderByClause: string;
    procedure InitFieldsAndColumns;
  protected
    procedure InitDefaults; override;
  public
    procedure UpdateObserver(const ASubject: IEFSubject;
      const AContext: string = ''); override;
    property ViewTable: TKViewTable read FViewTable write SetViewTable;
    property ServerStore: TKViewTableStore read FServerStore write FServerStore;
    procedure LoadData;
  published
    procedure GetRecordPage;
    procedure EditViewRecord;
    procedure NewRecord(This: TExtButton; E: TExtEventObjectSingleton);
    procedure DeleteCurrentRecord;
    procedure RefreshData;
  end;

implementation

uses
  SysUtils, StrUtils, Math,
  EF.Tree, EF.StrUtils, EF.Localization,
  Kitto.Metadata.Models, Kitto.Metadata.Views, Kitto.Rules, Kitto.AccessControl,
  Kitto.Ext.Filters, Kitto.Ext.Session, Kitto.Ext.Utils, Kitto.Ext.Controller;

{ TKExtGridPanel }

procedure TKExtGridPanel.CreateFilterPanel;
var
  LItems: TEFNode;
  I: Integer;
begin
  Assert(ViewTable <> nil);

  LItems := ViewTable.FindNode('Controller/Filters/Items');
  if Assigned(LItems) and (LItems.ChildCount > 0) then
  begin
    FFilterPanel := TKExtFilterPanel.AddTo(Items);
    FFilterPanel.Region := rgNorth;
    FFilterPanel.Title := _(ViewTable.GetString('Controller/Filters/DisplayLabel', 'Filters'));
    FFilterPanel.Collapsible := True;
    FFilterPanel.Frame := True;
    FFilterPanel.Connector := ViewTable.GetString('Controller/Filters/Connector', 'and');
    FFilterPanel.AutoHeight := True;
    for I := 0 to LItems.ChildCount - 1 do
    begin
      // Currently unused.
      LItems.Children[I].SetString('Sys/ApplyJSCode', GetRefreshJSCode);
      TKExtFilterFactory.Instance.CreateFilter(LItems.Children[I], Self, FFilterPanel.Items);
    end;
  end;
end;

function TKExtGridPanel.GetFilterExpression: string;
begin
  if Assigned(FFilterPanel) then
    Result := FFilterPanel.GetFilterExpression
  else
    Result := '';
end;

function TKExtGridPanel.GetOrderByClause: string;
var
  LSortFieldName: string;
begin
  LSortFieldName := ViewTable.GetString('Controller/Grouping/SortFieldName', GetGroupingFieldName);
  if LSortFieldName <> '' then
    Result := ViewTable.FieldByName(LSortFieldName).QualifiedNameOrExpression
  else
    Result := ''
end;

procedure TKExtGridPanel.GetRecordPage;
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
    LData := ServerStore.GetAsJSON;
  end
  else
  begin
    LStart := Session.QueryAsInteger['start'];
    LLimit := Session.QueryAsInteger['limit'];

    if (LStart <> 0) or (LLimit <> 0) then
    begin
      LTotal := ServerStore.LoadPage(GetFilterExpression, GetOrderByClause, LStart, LLimit);
      LData := ServerStore.GetAsJSON;
    end
    else
    begin
      ServerStore.Load(GetFilterExpression, GetOrderByClause);
      LTotal := ServerStore.RecordCount;
      LData := ServerStore.GetAsJSON(0, Min(MAX_RECORD_COUNT, ServerStore.RecordCount));
    end;
  end;
  Session.Response := Format('{Total:%d,Root:%s}', [LTotal, LData]);
end;

function TKExtGridPanel.GetRefreshJSCode: string;
begin
  Assert(Assigned(FStore));

  if Assigned(FPagingToolbar) then
    Result := FPagingToolbar.JSName + '.dorefresh();'
  else
    Result := FStore.JSName + '.load({params:{start:0,limit:' + IntToStr(FPageRecordCount) + ',Obj:"' + JSName + '"}});';
end;

function TKExtGridPanel.GetGroupingFieldName: string;
begin
  Result := ViewTable.GetString('Controller/Grouping/FieldName');
end;

procedure TKExtGridPanel.CreateStoreAndView;
var
  LGroupingMenu: Boolean;
  LCountTemplate: string;
  LGroupingFieldName: string;
begin
  { TODO : investigate the row body feature }
  LGroupingFieldName := GetGroupingFieldName;
  LGroupingMenu := ViewTable.GetBoolean('Controller/Grouping/EnableMenu');
  if (LGroupingFieldName <> '') or LGroupingMenu then
  begin
    FGridView := TExtGridGroupingView.Create;
    TExtGridGroupingView(FGridView).EmptyGroupText := _('No data to display in this group.');
    { TODO : use singular and plural display labels of the form }
    //TExtGridGroupingView(FGridView).GroupTextTpl := '{text} ({[values.rs.length]} {[values.rs.length > 1 ? "Items" : "Item"]})';
    TExtGridGroupingView(FGridView).StartCollapsed := ViewTable.GetBoolean('Controller/Grouping/StartCollapsed');
    TExtGridGroupingView(FGridView).EnableGroupingMenu := LGroupingMenu;
    TExtGridGroupingView(FGridView).EnableNoGroups := LGroupingMenu;
    TExtGridGroupingView(FGridView).HideGroupedColumn := True;
    TExtGridGroupingView(FGridView).ShowGroupName := ViewTable.GetBoolean('Controller/Grouping/ShowName');
    if ViewTable.GetBoolean('Controller/Grouping/ShowCount') then
    begin
      LCountTemplate := ViewTable.GetString('Controller/Grouping/ShowCount/Template',
        '{text} ({[values.rs.length]} {[values.rs.length > 1 ? "%ITEMS%" : "%ITEM%"]})');
      LCountTemplate := ReplaceText(LCountTemplate, '%ITEMS%',
        _(ViewTable.GetString('Controller/Grouping/ShowCount/PluralItemName', ViewTable.PluralDisplayLabel)));
      LCountTemplate := ReplaceText(LCountTemplate, '%ITEM%',
        _(ViewTable.GetString('Controller/Grouping/ShowCount/ItemName', ViewTable.DisplayLabel)));
      TExtGridGroupingView(FGridView).GroupTextTpl := LCountTemplate;
    end;
    FStore := TExtDataGroupingStore.Create;
    //TExtDataGroupingStore(FStore).GroupOnSort := True;
    if LGroupingFieldName <> '' then
    begin
      TExtDataGroupingStore(FStore).GroupField := LGroupingFieldName;
      FStore.RemoteSort := True;
    end;
  end
  else
  begin
    FGridView := TExtGridGridView.Create;
    FStore := TExtDataStore.Create;
{ TODO :
Do remote sort when paging is enabled. Local sort makes no sense with paging.
Note: remote sort passes params sort and dir. }
    FStore.RemoteSort := False;
  end;
  FGridView.EmptyText := _('No data to display.');
  FGridView.EnableRowBody := True;
  { TODO : make it configurable? }
  FGridView.ForceFit := False;

  FStore.Url := MethodURI(GetRecordPage);
  FReader := TExtDataJsonReader.Create(JSObject('')); // Must pass '' otherwise invalid code is generated.
  FReader.Root := 'Root';
  FReader.TotalProperty := 'Total';
  FStore.Reader := FReader;

  FStore.On('load', FSelModel.SelectFirstRow);

  FGridPanel.Store := FStore;
  FGridPanel.View := FGridView;
end;

procedure TKExtGridPanel.InitDefaults;
begin
  inherited;
  Layout := lyBorder;
  Border := False;
  Header := False;
  FGridPanel := TExtGridEditorGridPanel.AddTo(Items);
  FGridPanel.Border := False;
  FGridPanel.Header := False;
  FGridPanel.Region := rgCenter;
  FSelModel := TExtGridRowSelectionModel.Create;
  FSelModel.SingleSelect := True;
  FSelModel.Grid := FGridPanel;
  FGridPanel.SelModel := FSelModel;
  FGridPanel.StripeRows := True;
  FGridPanel.Frame := False;
  FGridPanel.AutoScroll := True;
  FGridPanel.AutoWidth := True;
  FGridPanel.ColumnLines := True;
  FGridPanel.TrackMouseOver := True;
end;

procedure TKExtGridPanel.InitFieldsAndColumns;
var
  I: Integer;
  LLayout: TKLayout;
  LViewField: TKViewField;
  LLayoutName: string;

  procedure AddGridColumn(const AViewField: TKViewField);
  var
    LColumn: TExtGridColumn;
    LColumnWidth: Integer;

    function CreateColumn: TExtGridColumn;
    var
      LDataType: TEFDataType;
      LFormat: string;
    begin
      LDataType := AViewField.DataType;
      if LDataType is TKReferenceDataType then
        LDataType := AViewField.ModelField.ReferencedModel.CaptionField.DataType;

      if LDataType is TEFBooleanDataType then
      begin
        // Don't use TExtGridBooleanColumn here, otherwise the renderer will be inneffective.
        Result := TExtGridColumn.AddTo(FGridPanel.Columns);
        Result.RendererExtFunction := JSFunction('v',
          'return String.format(''<div class="x-grid3-check-col{0}"></div>'', v ? "-on" : '''');');
      end
      else if LDataType is TEFDateDataType then
      begin
        Result := TExtGridDateColumn.AddTo(FGridPanel.Columns);
        LFormat := AViewField.DisplayFormat;
        if LFormat = '' then
          LFormat := Session.Config.UserFormatSettings.ShortDateFormat;
        TExtGridDateColumn(Result).Format := DelphiDateFormatToJSDateFormat(LFormat);
      end
      else if LDataType is TEFTimeDataType then
      begin
        Result := TExtGridColumn.AddTo(FGridPanel.Columns);
        LFormat := AViewField.DisplayFormat;
        if LFormat = '' then
          LFormat := Session.Config.UserFormatSettings.ShortTimeFormat;
        Result.RendererExtFunction := Result.JSFunction('v',
          Format('return formatTime(v, "%s");', [DelphiTimeFormatToJSTimeFormat(LFormat)]));
      end
      else if LDataType is TEFDateTimeDataType then
      begin
        Result := TExtGridDateColumn.AddTo(FGridPanel.Columns);
        LFormat := AViewField.DisplayFormat;
        if LFormat = '' then
          LFormat := Session.Config.UserFormatSettings.ShortDateFormat + ' ' +
            Session.Config.UserFormatSettings.ShortTimeFormat;
        TExtGridDateColumn(Result).Format := DelphiDateTimeFormatToJSDateTimeFormat(LFormat);
      end
      else if LDataType is TEFIntegerDataType then
      begin
        Result := TExtGridNumberColumn.AddTo(FGridPanel.Columns);
        TExtGridNumberColumn(Result).Format := '0';
        Result.Align := alRight;
      end
      else if (LDataType is TEFFloatDataType) or (LDataType is TEFDecimalDataType) then
      begin
        Result := TExtGridNumberColumn.AddTo(FGridPanel.Columns);
        TExtGridNumberColumn(Result).Format := AdaptExtNumberFormat('0.00', Session.Config.UserFormatSettings);
        Result.Align := alRight;
      end
      else if LDataType is TEFCurrencyDataType then
      begin
        Result := TExtGridNumberColumn.AddTo(FGridPanel.Columns);
        TExtGridNumberColumn(Result).Format := AdaptExtNumberFormat('0,0.00', Session.Config.UserFormatSettings);
        Result.Align := alRight;
      end
      else
        Result := TExtGridColumn.AddTo(FGridPanel.Columns);
    end;

  begin
    LColumn := CreateColumn;
    LColumn.Sortable := not AViewField.IsBlob;
    LColumn.Header := AViewField.DisplayLabel;
    LColumn.DataIndex := AViewField.AliasedName;

    LColumnWidth := AViewField.DisplayWidth;
    if LColumnWidth = 0 then
      LColumnWidth := Min(IfThen(AViewField.Size = 0, 40, AViewField.Size), 40);
    LColumn.Width := CharsToPixels(LColumnWidth);

    { TODO : add in-place editing as an option. }
    LColumn.Editable := False;
    //LColumn.Editor := ...
    LColumn.Hidden := not ViewTable.IsFieldVisible(AViewField);
  end;

  procedure AddColumn(const AViewField: TKViewField);
  begin
    if ViewTable.IsFieldVisible(AViewField) or (AViewField.AliasedName = GetGroupingFieldName) then
      AddGridColumn(AViewField);
  end;

  procedure DoAddReaderField(const AName, AType: string);
  var
    LField: TExtDataField;
  begin
    LField := TExtDataField.AddTo(FReader.Fields);
    LField.Name := AName;
    LField.Type_ := AType;
  end;

  procedure AddReaderField(const AViewField: TKViewField);
  var
    I: Integer;
  begin
    DoAddReaderField(AViewField.AliasedName, AViewField.DataType.GetJSTypeName);
    if AViewField.IsReference then
    begin
      for I := 0 to AViewField.ModelField.FieldCount - 1 do
        DoAddReaderField(AViewField.ModelField.Fields[I].FieldName, AViewField.ModelField.Fields[I].DataType.GetJSTypeName);
    end;
  end;

begin
  Assert(ViewTable <> nil);

  LLayoutName := ViewTable.GetString('Controller/List/Layout');
  if LLayoutName <> '' then
    LLayout := ViewTable.View.Catalog.Layouts.FindLayout(LLayoutName)
  else
    LLayout := ViewTable.FindLayout('List');

  if LLayout <> nil then
  begin
    for I := 0 to LLayout.ChildCount - 1 do
      AddColumn(ViewTable.FieldByAliasedName(LLayout.Children[I].AsString));
    // Add key fields anyway if they are not part of the layout.
    // If their IsVisible is False at the view or model level
    // they won't be added as grid columns anyway.
    for I := 0 to ViewTable.FieldCount - 1 do
    begin
      LViewField := ViewTable.Fields[I];
      if (LLayout.FindChild(LViewField.AliasedName) = nil) and LViewField.IsKey then
        AddColumn(LViewField);
    end;
  end
  else
  begin
    for I := 0 to ViewTable.FieldCount - 1 do
      AddColumn(ViewTable.Fields[I]);
  end;
  // All fields should be in the reader in all cases.
  for I := 0 to ViewTable.FieldCount - 1 do
    AddReaderField(ViewTable.Fields[I]);

  FGridPanel.AutoExpandColumn := ViewTable.GetString('Controller/AutoExpandFieldName');
end;

procedure TKExtGridPanel.NewRecord(This: TExtButton; E: TExtEventObjectSingleton);
begin
  ShowEditWindow(nil, emNewRecord);
end;

procedure TKExtGridPanel.EditViewRecord;
begin
  { TODO : Make sure the view/edit button is disabled on the client when there are no records. }
  if ServerStore.RecordCount = 0 then
    Exit;
  ShowEditWindow(LocateRecordFromSession, emEditCurrentRecord);
end;

function TKExtGridPanel.IsReadOnly: Boolean;
begin
  Result := ViewTable.View.GetBoolean('IsReadOnly')
    or ViewTable.IsReadOnly
    or ViewTable.View.GetBoolean('Controller/PreventEditing')
    or not ViewTable.IsAccessGranted(ACM_MODIFY);
end;

procedure TKExtGridPanel.ShowEditWindow(const ARecord: TKRecord;
  const AEditMode: TKEditMode);
var
  LFormControllerType: string;
  LFormController: IKExtController;
begin
  Assert((AEditMode = emNewrecord) or Assigned(ARecord));
  Assert(ViewTable <> nil);

  if Assigned(FEditHostWindow) then
    FEditHostWindow.Free(True);
  FEditHostWindow := TKExtModalWindow.Create;
  FEditHostWindow.Width := ViewTable.GetInteger('Controller/PopupWindow/Width', FEditHostWindow.Width);
  FEditHostWindow.Height := ViewTable.GetInteger('Controller/PopupWindow/Height', FEditHostWindow.Height);
  FEditHostWindow.ResizeHandles := 'n s';
  FEditHostWindow.Closable := False;

  if AEditMode = emNewRecord then
    FEditHostWindow.Title := Format(_('New %s'), [ViewTable.DisplayLabel])
  else if IsReadOnly then
    FEditHostWindow.Title := ViewTable.DisplayLabel
  else
    FEditHostWindow.Title := Format(_('Edit %s'), [ViewTable.DisplayLabel]);
  //FEditHostWindow.On('close', Ajax(EditWindowClosed, ['Window', '%0.nm']));

  LFormControllerType := ViewTable.View.GetString('Controller/FormController', 'Form');
  LFormController := TKExtControllerFactory.Instance.CreateController(ViewTable.View, FEditHostWindow, Self, LFormControllerType);
  LFormController.OwnsView := False;
  LFormController.Config.SetObject('Sys/ServerStore', ServerStore);
  if Assigned(ARecord) then
    LFormController.Config.SetObject('Sys/Record', ARecord);
  LFormController.Config.SetObject('Sys/ViewTable', ViewTable);
  LFormController.Config.SetObject('Sys/HostWindow', FEditHostWindow);
  if AEditMode = emNewRecord then
    LFormController.Config.SetString('Sys/Operation', 'Add');
  LFormController.Display;
  FEditHostWindow.Show;
  //JSCode('showWindow(' + FEditHostWindow.JSName + ', ' + TExtObject(LFormController.AsObject).JSName + ');');
end;

procedure TKExtGridPanel.SetViewTable(const AValue: TKViewTable);
var
  LKeyFieldNames: string;
begin
  Assert(Assigned(AValue));

  FViewTable := AValue;

  Title := FViewTable.PluralDisplayLabel;

  FIsAddVisible := not ViewTable.GetBoolean('Controller/PreventAdding');
  FIsAddAllowed := FIsAddVisible and ViewTable.IsAccessGranted(ACM_ADD);
  FIsEditAllowed := not ViewTable.GetBoolean('Controller/PreventEditing')
    and ViewTable.IsAccessGranted(ACM_MODIFY);
  FIsDeleteVisible := not ViewTable.GetBoolean('Controller/PreventDeleting');
  FIsDeleteAllowed := FIsDeleteVisible and ViewTable.IsAccessGranted(ACM_DELETE);

  CreateStoreAndView;
  CreateFilterPanel;

  LKeyFieldNames := Join(ViewTable.GetKeyFieldAliasedNames(), ',');
  FGridPanel.On('rowdblclick', AjaxSelection(EditViewRecord, FSelModel, LKeyFieldNames, LKeyFieldNames, []));

  // By default show paging toolbar for large models.
  if ViewTable.GetBoolean('Controller/PagingTools', ViewTable.Model.IsLarge) then
  begin
    FPageRecordCount := ViewTable.GetInteger('Controller/PageRecordCount', MAX_RECORD_COUNT);
    FGridPanel.Bbar := CreatePagingToolbar;
  end;

  FTopToolbar := CreateTopToolbar;
  if FTopToolbar.Items.Count = 0 then
    FreeAndNil(FTopToolbar)
  else
    FGridPanel.Tbar := FTopToolbar;

  InitFieldsAndColumns;
end;

procedure TKExtGridPanel.UpdateObserver(const ASubject: IEFSubject;
  const AContext: string);
begin
  inherited;
  if (AContext = 'FilterChanged') then
    RefreshData;
  if (AContext = 'Confirmed') and Supports(ASubject.AsObject, IKExtController) then
    RefreshData;
end;

procedure TKExtGridPanel.LoadData;
begin
//  if FGridPanel.Columns.Count = 0 then
//    InitFieldsAndColumns;
  RefreshData;
end;

function TKExtGridPanel.LocateRecordFromSession: TKViewTableRecord;
var
  LKey: TEFNode;
begin
  LKey := TEFNode.Create;
  try
    LKey.Assign(ServerStore.Key);
    LKey.SetChildValuesfromStrings(Session.Queries, True, Session.Config.JSFormatSettings,
      function(const AName: string): string
      begin
        Result := ViewTable.FieldByName(AName).AliasedName;
      end);
    Result := ServerStore.Records.GetRecord(LKey);
  finally
    FreeAndNil(LKey);
  end;
end;

procedure TKExtGridPanel.DeleteCurrentRecord;
var
  LRecord: TKViewTableRecord;
begin
  Assert(ViewTable <> nil);

  // Apply BEFORE rules now even though actual save migh be deferred.
  LRecord := LocateRecordFromSession;
  LRecord.MarkAsDeleted;
  try
    LRecord.ApplyBeforeRules;
  except
    on E: EKValidationError do
    begin
      LRecord.MarkAsClean;
      ExtMessageBox.Alert(Session.Config.AppTitle, E.Message);
      Exit;
    end;
  end;

  if not ViewTable.IsDetail then
  begin
    LRecord.Save(True);
    Session.Flash(Format(_('%s deleted.'), [ViewTable.DisplayLabel]));
  end;
  RefreshData;
end;

procedure TKExtGridPanel.RefreshData;
begin
  Assert(Assigned(FStore));

  if Assigned(FPagingToolbar) then
    FPagingToolbar.DoRefresh
  else
    FStore.Load(JSObject('params:{start:0,limit:' + IntToStr(FPageRecordCount) + ',Obj:"' + JSName + '"}'));
end;

function TKExtGridPanel.CreatePagingToolbar: TExtPagingToolbar;
begin
  Assert(ViewTable <> nil);

  FPagingToolbar := TExtPagingToolbar.Create;
  FPagingToolbar.Store := FGridPanel.Store;
  FPagingToolbar.DisplayInfo := False;
  FPagingToolbar.PageSize := FPageRecordCount;
  Result := FPagingToolbar;
  FPagingToolbar.Store := nil; // Avoid double destruction of the store.
end;

function TKExtGridPanel.CreateTopToolbar: TExtToolbar;
var
  LNewButton: TExtButton;
  LEditButton: TExtButton;
  LDeleteButton: TExtButton;
  LRefreshButton: TExtButton;
  LKeyFieldNames: string;
begin
  Assert(ViewTable <> nil);

  Result := TExtToolbar.Create;

  if not IsReadOnly and FIsAddVisible then
  begin
    LNewButton := TExtButton.AddTo(Result.Items);
    LNewButton.Text := Format(_('New %s'), [ViewTable.DisplayLabel]);
    LNewButton.Icon := Session.Config.GetImageURL('new_record');
    if not FIsAddAllowed then
      LNewButton.Disabled := True
    else
      LNewButton.OnClick := NewRecord;
    TExtToolbarSpacer.AddTo(Result.Items);
  end;

  LEditButton := TExtButton.AddTo(Result.Items);
  if IsReadOnly then
  begin
    LEditButton.Text := Format(_('View %s'), [ViewTable.DisplayLabel]);
    LEditButton.Icon := Session.Config.GetImageURL('view_record');
  end
  else
  begin
    LEditButton.Text := Format(_('Edit %s'), [ViewTable.DisplayLabel]);
    LEditButton.Icon := Session.Config.GetImageURL('edit_record');
  end;
  if not FIsEditAllowed then
    LEditButton.Disabled := True
  else
  begin
    LKeyFieldNames := Join(ViewTable.GetKeyFieldAliasedNames, ',');
    LEditButton.On('click', AjaxSelection(EditViewRecord, FSelModel, LKeyFieldNames, LKeyFieldNames, []));
    TExtGridRowSelectionModel(FSelModel).On('selectionchange', JSFunction(
      's', Format('%s.setDisabled(s.getCount() == 0);', [LEditButton.JSName])));
  end;
  TExtToolbarSpacer.AddTo(Result.Items);

  if not IsReadOnly and FIsDeleteVisible then
  begin
    LDeleteButton := TExtButton.AddTo(Result.Items);
    LDeleteButton.Text := Format(_('Delete %s'), [ViewTable.DisplayLabel]);
    LDeleteButton.Icon := Session.Config.GetImageURL('delete_record');
    if not FIsDeleteAllowed then
      LDeleteButton.Disabled := True
    else
    begin
      LDeleteButton.Handler := JSFunction(GetSelectConfirmCall(
        Format(_('Selected %s will be deleted. Are you sure?'), [ViewTable.DisplayLabel]), DeleteCurrentRecord));
      TExtGridRowSelectionModel(FSelModel).On('selectionchange', JSFunction(
      's', Format('%s.setDisabled(s.getCount() == 0);', [LDeleteButton.JSName])));
    end;
    TExtToolbarSpacer.AddTo(Result.Items);
  end;

  LRefreshButton := TExtButton.AddTo(Result.Items);
  LRefreshButton.Text := _('Refresh');
  LRefreshButton.Icon := Session.Config.GetImageURL('refresh');
  LRefreshButton.Handler := Ajax(RefreshData);
  LRefreshButton.Tooltip := _('Refresh data');
  //TExtToolbarSpacer.AddTo(Result.Items);
end;

function TKExtGridPanel.GetSelectConfirmCall(const AMessage: string; const AMethod: TExtProcedure): string;
begin
  Result := Format('confirmCall("%s", "%s", ajaxSingleSelection, {methodURL: "%s", selModel: %s, fieldNames: "%s"});',
    [Session.Config.AppTitle, AMessage, MethodURI(AMethod), FSelModel.JSName,
    Join(ViewTable.GetKeyFieldAliasedNames, ',')]);
end;

{ TKExtFilterPanel }

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
end;

end.
