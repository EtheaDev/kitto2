unit Kitto.Ext.GridPanel;

interface

uses
  ExtPascal, Ext, ExtForm, ExtData, ExtGrid, ExtPascalUtils,
  EF.ObserverIntf,
  Kitto.Metadata.DataView, Kitto.Store, Kitto.Types,
  Kitto.Ext.Base;

type
  TKExtFilterPanel = class(TExtFormFormPanel)
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
    procedure InitFieldsAndColumns;
    procedure SetViewTable(const AValue: TKViewTable);
    procedure CreateFilterPanel;
    procedure CreateStoreAndView;
    function GetRefreshJSCode: string;
    function GetFilterExpression: string;
    function GetGroupingFieldName: string;
    function CreateTopToolbar: TExtToolbar;
    function CreatePagingToolbar: TExtPagingToolbar;
    function LocateRecordFromSession: TKViewTableRecord;
    procedure RefreshData;
    procedure EditOrViewCurrentRecord;
    procedure ShowEditWindow(const ARecord: TKRecord;
      const AEditMode: TKEditMode);
    function GetSelectConfirmCall(const AMessage: string; const AMethod: TExtProcedure): string;
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
    procedure RowDblClick;
    procedure NewRecord(This: TExtButton; E: TExtEventObjectSingleton);
    procedure DeleteCurrentRecord;
  end;

implementation

uses
  SysUtils, StrUtils, Math,
  EF.Tree, EF.StrUtils, EF.Localization,
  Kitto.Metadata.Models, Kitto.Metadata.Views, Kitto.Environment, Kitto.Rules,
  Kitto.AccessControl,
  Kitto.Ext.Filters, Kitto.Ext.Session, Kitto.Ext.Utils, Kitto.Ext.Controller;

  const
  { TODO : should we just fetch everything when grouping is enabled? }
  DEFAULT_GROUPING_PAGE_RECORD_COUNT = 1000;

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
    FFilterPanel.Border := False;
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
    LData := ServerStore.GetAsJSON(True);
  end
  else
  begin
    LStart := Session.QueryAsInteger['start'];
    LLimit := Session.QueryAsInteger['limit'];

    if (LStart <> 0) or (LLimit <> 0) then
    begin
      LTotal := ServerStore.LoadPage(GetFilterExpression, LStart, LLimit);
      LData := ServerStore.GetAsJSON(True);
    end
    else
    begin
      ServerStore.Load(GetFilterExpression);
      LTotal := ServerStore.RecordCount;
      LData := ServerStore.GetAsJSON(True, 0, Min(MAX_RECORD_COUNT, ServerStore.RecordCount));
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
      LCountTemplate := ViewTable.MinifyFieldNames(LCountTemplate);
      TExtGridGroupingView(FGridView).GroupTextTpl := LCountTemplate;
    end;
    FStore := TExtDataGroupingStore.Create;
    //TExtDataGroupingStore(FStore).GroupOnSort := True;
    if LGroupingFieldName <> '' then
    begin
      TExtDataGroupingStore(FStore).GroupField := ViewTable.FieldByAliasedName(LGroupingFieldName).GetMinifiedName;
      FStore.RemoteSort := False;
      FStore.SortInfo := JSObject('field:"' +
        ViewTable.FieldByAliasedName(ViewTable.GetString('Controller/Grouping/SortFieldName', LGroupingFieldName)).GetMinifiedName + '"');
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

  FGridPanel.Store := FStore;
  FGridPanel.View := FGridView;
end;

procedure TKExtGridPanel.InitDefaults;
var
  LSelModel: TExtGridRowSelectionModel;
begin
  inherited;
  Layout := lyBorder;
  Border := False;
  Header := False;
  FGridPanel := TExtGridEditorGridPanel.AddTo(Items);
  FGridPanel.Border := False;
  FGridPanel.Header := False;
  FGridPanel.Region := rgCenter;
  LSelModel := TExtGridRowSelectionModel.Create;
  LSelModel.SingleSelect := True;
  FGridPanel.SelModel := LSelModel;
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
    begin
      LDataType := AViewField.DataType;
      if LDataType is TKReferenceDataType then
        LDataType := AViewField.ModelField.ReferencedModel.CaptionField.DataType;

      if LDataType is TEFBooleanDataType then
      begin
        Result := TExtGridBooleanColumn.AddTo(FGridPanel.Columns);
        Result.RendererExtFunction := JSFunction('V', 'return "<div class=''x-grid3-check-col"+(V?"-on":"")+"''></div>";');
      end
      else if LDataType is TEFDateDataType then
      begin
        Result := TExtGridDateColumn.AddTo(FGridPanel.Columns);
        TExtGridDateColumn(Result).Format := DelphiDateFormatToJSDateFormat(Environment.UserFormatSettings.ShortDateFormat);
      end
      else if LDataType is TEFTimeDataType then
      begin
        Result := TExtGridColumn.AddTo(FGridPanel.Columns);
        //TExtGridDateColumn(Result).Format := DelphiTimeFormatToJSTimeFormat(Environment.UserFormatSettings.ShortTimeFormat);
      end
      else if LDataType is TEFDateTimeDataType then
      begin
        Result := TExtGridDateColumn.AddTo(FGridPanel.Columns);
        TExtGridDateColumn(Result).Format :=
          DelphiDateFormatToJSDateFormat(Environment.UserFormatSettings.ShortDateFormat) + ' ' +
          DelphiTimeFormatToJSTimeFormat(Environment.UserFormatSettings.ShortTimeFormat);
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
        TExtGridNumberColumn(Result).Format := AdaptExtNumberFormat('0.00', Environment.UserFormatSettings);
        Result.Align := alRight;
      end
      else if LDataType is TEFCurrencyDataType then
      begin
        Result := TExtGridNumberColumn.AddTo(FGridPanel.Columns);
        TExtGridNumberColumn(Result).Format := AdaptExtNumberFormat('0,0.00', Environment.UserFormatSettings);
        Result.Align := alRight;
      end
      else
        Result := TExtGridColumn.AddTo(FGridPanel.Columns);
    end;

  begin
    LColumn := CreateColumn;
    LColumn.Sortable := not AViewField.IsBlob;
    LColumn.Header := AViewField.DisplayLabel;
    //LColumn.Id := AViewField.GetMinifiedName;
    LColumn.DataIndex := AViewField.GetMinifiedName;

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

  procedure AddReaderField(const AViewField: TKViewField);
  var
    LField: TExtDataField;
  begin
    LField := TExtDataField.AddTo(FReader.Fields);
    LField.Name := AViewField.GetMinifiedName;
    LField.Type_ := AViewField.DataType.GetJSTypeName;
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

procedure TKExtGridPanel.RowDblClick;
begin
  EditOrViewCurrentRecord;
end;

procedure TKExtGridPanel.EditOrViewCurrentRecord;
begin
  ShowEditWindow(LocateRecordFromSession, emEditCurrentRecord);
end;

procedure TKExtGridPanel.ShowEditWindow(const ARecord: TKRecord;
  const AEditMode: TKEditMode);
var
  LFormControllerType: string;
  LFormController: IKExtController;

  function IsReadOnly: Boolean;
  begin
    Result := ViewTable.View.GetBoolean('IsReadOnly')
      or ViewTable.IsReadOnly
      or ViewTable.View.GetBoolean('Controller/PreventEditing')
      or not ViewTable.IsAccessGranted(ACM_MODIFY);
  end;

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

  FIsAddAllowed := not ViewTable.GetBoolean('Controller/PreventAdding')
    and ViewTable.IsAccessGranted(ACM_ADD);
  FIsEditAllowed := not ViewTable.GetBoolean('Controller/PreventEditing')
    and ViewTable.IsAccessGranted(ACM_MODIFY);
  FIsDeleteAllowed := not ViewTable.GetBoolean('Controller/PreventDeleting')
    and ViewTable.IsAccessGranted(ACM_DELETE);

  CreateStoreAndView;
  CreateFilterPanel;

  LKeyFieldNames := Join(ViewTable.GetKeyFieldAliasedNames(True), ',');
  FGridPanel.On('rowdblclick', AjaxSelection(RowDblClick, FGridPanel.SelModel, LKeyFieldNames, LKeyFieldNames, []));

  // By default show paging toolbar if the model is large unless the view is grouped.
  if ViewTable.GetBoolean('Controller/PagingTools', ViewTable.Model.IsLarge and (GetGroupingFieldName = '')) then
  begin
    if GetGroupingFieldName <> '' then
      FPageRecordCount := ViewTable.GetInteger('Controller/PageRecordCount', DEFAULT_GROUPING_PAGE_RECORD_COUNT)
    else
      FPageRecordCount := ViewTable.GetInteger('Controller/PageRecordCount', MAX_RECORD_COUNT);
    FGridPanel.Bbar := CreatePagingToolbar;
  end;

  FTopToolbar := CreateTopToolbar;
  if FTopToolbar.Items.Count = 0 then
    FreeAndNil(FTopToolbar)
  else
    FGridPanel.Tbar := FTopToolbar;
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
  if FGridPanel.Columns.Count = 0 then
    InitFieldsAndColumns;
  RefreshData;
end;

function TKExtGridPanel.LocateRecordFromSession: TKViewTableRecord;
var
  LKey: TEFNode;
begin
  Assert(ServerStore.RecordCount > 0);

  LKey := TEFNode.Create;
  try
    LKey.Assign(ServerStore.Key);
    LKey.SetChildValuesfromStrings(Session.Queries, True, Environment.JSFormatSettings,
      function(const AName: string): string
      begin
        Result := ViewTable.FieldByName(AName).GetMinifiedName;
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
    LRecord.Backup;
    try
      LRecord.ApplyBeforeRules;
    except
      LRecord.Restore;
      raise;
    end;
  except
    on E: EKValidationError do
    begin
      LRecord.MarkAsClean;
      ExtMessageBox.Alert(Environment.AppTitle, E.Message);
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
  LDeleteButton: TExtButton;
begin
  Assert(ViewTable <> nil);

  Result := TExtToolbar.Create;
  // Add buttons.
  if not ViewTable.IsReadOnly then
  begin
    LNewButton := TExtButton.AddTo(Result.Items);
    begin
      LNewButton.Text := Format(_('New %s'), [ViewTable.DisplayLabel]);
      LNewButton.Icon := Environment.GetImageURL('new_record_16');
      LNewButton.Disabled := not FIsAddAllowed;
      if not LNewButton.Disabled then
        LNewButton.OnClick := NewRecord;
    end;
    TExtToolbarSpacer.AddTo(Result.Items);
    LDeleteButton := TExtButton.AddTo(Result.Items);
    begin
      LDeleteButton.Text := Format(_('Delete %s'), [ViewTable.DisplayLabel]);
      LDeleteButton.Icon := Environment.GetImageURL('delete_record_16');
      LDeleteButton.Disabled := not FIsDeleteAllowed;
      if not LDeleteButton.Disabled then
        LDeleteButton.Handler := JSFunction(GetSelectConfirmCall(
          Format(_('Selected %s will be deleted. Are you sure?'), [ViewTable.DisplayLabel]), DeleteCurrentRecord));
    end;
  end;
end;

function TKExtGridPanel.GetSelectConfirmCall(const AMessage: string; const AMethod: TExtProcedure): string;
begin
  Result := Format('confirmCall("%s", "%s", ajaxSingleSelection, {methodURL: "%s", selModel: %s, fieldNames: "%s"});',
    [Environment.AppTitle, AMessage, MethodURI(AMethod), FGridPanel.SelModel.JSName,
    Join(ViewTable.GetKeyFieldAliasedNames(True), ',')]);
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
end;

end.
