unit Kitto.Ext.List;

interface

uses
  Ext, ExtGrid, ExtData, ExtForm,
  EF.ObserverIntf, EF.Classes,
  Kitto.Ext.Base, Kitto.Ext.DataPanel, Kitto.Types, Kitto.Ext.Controller,
  Kitto.Metadata.Views, Kitto.Store;

type
  TKExtFilterPanel = class(TExtFormFormPanel)
  private
    FConnector: string;
  public
    property Connector: string read FConnector write FConnector;
    function GetFilterExpression: string;
  end;

  TKExtListPanelController = class(TKExtDataPanelController)
  private
    FGridPanel: TExtGridEditorGridPanel;
    FPageRecordCount: Integer;
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
    FMasterRecord: TKViewTableRecord;
    procedure InitFieldsAndColumns;
    // Opens an edit window on the specified record.
    procedure ShowEditWindow(const ARecord: TKRecord; const AEditMode: TKEditMode);
    function LocateRecordFromSession: TKRecord;
    procedure RefreshData;
    function CreatePagingToolbar: TExtPagingToolbar;
    procedure EditOrViewCurrentRecord;
    function CreateTopToolbar: TExtToolbar;
    procedure CreateFilterPanel;
    procedure CreateStoreAndView;
    function GetRefreshJSCode: string;
    function GetGroupingFieldName: string;
  protected
    procedure LoadData; override;
    procedure InitComponents; override;
    function GetFilterExpression: string; override;
  public
    destructor Destroy; override;
    procedure UpdateObserver(const ASubject: IEFSubject;
      const AContext: string = ''); override;
  published
    procedure GetRecordPage;
    procedure RowDblClick;
    //procedure EditWindowClosed;
    procedure NewRecord(This: TExtButton; E: TExtEventObjectSingleton);
    procedure DeleteCurrentRecord;
  end;

implementation

uses
  SysUtils, Math, StrUtils, Variants, Types,
  ExtPascal, ExtPascalUtils,
  EF.Intf, EF.Localization, EF.StrUtils, EF.Tree, EF.SQL,
  Kitto.Environment, Kitto.AccessControl, Kitto.Ext.Session, Kitto.Ext.Utils,
  Kitto.JSON, Kitto.Ext.Filters;

const
  { TODO : should we just fetch everything when grouping is enabled? }
  DEFAULT_GROUPING_PAGE_RECORD_COUNT = 1000;

{ TKExtListPanelController }

procedure TKExtListPanelController.DeleteCurrentRecord;
begin
  Assert(ViewTable <> nil);

  LocateRecordFromSession.MarkAsDeleted;
  if not ViewTable.IsDetail then
  begin
    ServerStore.Save(Environment.MainDBConnection, ViewTable.Model);
    Session.Flash(_('Record deleted.'));
  end;
  RefreshData;
end;

destructor TKExtListPanelController.Destroy;
begin
  //FreeAndNil(FEditHostWindow);
  inherited;
end;

//procedure TKExtListPanel.EditWindowClosed;
//begin
//  FreeAndNil(FEditHostWindow);
//end;

procedure TKExtListPanelController.GetRecordPage;
var
  LStart: Integer;
  LLimit: Integer;
  LTotal: Integer;
  LData: string;
begin
{ TODO : Fully refreshing at each page change might be inefficient. }
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
  Session.Response := Format('{Total:%d,Root:%s}', [LTotal, LData]);
end;

function TKExtListPanelController.CreatePagingToolbar: TExtPagingToolbar;
begin
  Assert(ViewTable <> nil);

  FPagingToolbar := TExtPagingToolbar.Create;
  FPagingToolbar.Store := FGridPanel.Store;
  FPagingToolbar.DisplayInfo := False;
  FPagingToolbar.PageSize := FPageRecordCount;
  Result := FPagingToolbar;
  FPagingToolbar.Store := nil; // Avoid double destruction of the store.
end;

function TKExtListPanelController.GetFilterExpression: string;
begin
  if Assigned(FFilterPanel) then
    Result := FFilterPanel.GetFilterExpression
  else
    Result := '';
end;

procedure TKExtListPanelController.CreateFilterPanel;
var
  LItems: TEFNode;
  I: Integer;
begin
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

function TKExtListPanelController.CreateTopToolbar: TExtToolbar;
var
  LNewButton: TExtButton;
  LDeleteButton: TExtButton;
  LKeyFieldNames: string;
begin
  Assert(ViewTable <> nil);

  Result := TExtToolbar.Create;
  // Add buttons.
  if not ViewTable.IsReadOnly then
  begin
    LNewButton := TExtButton.AddTo(Result.Items);
    begin
      LNewButton.Text := 'New Record';
      LNewButton.Icon := Environment.GetImageURL('new_record_16');
      LNewButton.Disabled := not FIsAddAllowed;
      if not LNewButton.Disabled then
        LNewButton.OnClick := NewRecord;
    end;
    TExtToolbarSpacer.AddTo(Result.Items);
    LDeleteButton := TExtButton.AddTo(Result.Items);
    begin
      LDeleteButton.Text := 'Delete Record';
      LDeleteButton.Icon := Environment.GetImageURL('delete_record_16');
      LDeleteButton.Disabled := not FIsDeleteAllowed;
      if not LDeleteButton.Disabled then
      begin
        LKeyFieldNames := Join(ViewTable.GetKeyFieldAliasedNames(True), ',');
        { TODO : Add client-side confirmation request. }
        LDeleteButton.On('click', AjaxSelection(DeleteCurrentRecord,
          TExtGridRowSelectionModel(FGridPanel.SelModel), LKeyFieldNames, LKeyFieldNames, []));
      end;
    end;
  end;
end;

function TKExtListPanelController.GetGroupingFieldName: string;
begin
  Result := ViewTable.GetString('Controller/Grouping/FieldName');
end;

procedure TKExtListPanelController.InitComponents;
var
  LSelModel: TExtGridRowSelectionModel;
  LKeyFieldNames: string;
begin
  inherited;
  Title := Environment.MacroExpansionEngine.Expand(ViewTable.PluralDisplayLabel);

  if ViewTable.IsDetail then
  begin
    FMasterRecord := Config.GetObject('Sys/MasterRecord') as TKViewTableRecord;
    Assert(Assigned(FMasterRecord));
    ServerStore.MasterRecord := FMasterRecord;
  end
  else
    FMasterRecord := nil;

  { TODO : implement resource URIs }
  FIsAddAllowed := not ViewTable.GetBoolean('Controller/PreventAdding')
    and ViewTable.IsAccessGranted(ACM_ADD);
  FIsEditAllowed := not ViewTable.GetBoolean('Controller/PreventEditing')
    and ViewTable.IsAccessGranted(ACM_MODIFY);
  FIsDeleteAllowed := not ViewTable.GetBoolean('Controller/PreventDeleting')
    and ViewTable.IsAccessGranted(ACM_DELETE);

  CreateStoreAndView;
  CreateFilterPanel;

  FGridPanel := TExtGridEditorGridPanel.AddTo(Items);
  FGridPanel.Region := rgCenter;
  FGridPanel.Border := False;
  FGridPanel.Header := False;
  FGridPanel.Store := FStore;
  FGridPanel.View := FGridView;
  LSelModel := TExtGridRowSelectionModel.Create;
  LSelModel.SingleSelect := True;
  FGridPanel.SelModel := LSelModel;
  FGridPanel.StripeRows := True;
  FGridPanel.Frame := True;
  FGridPanel.AutoScroll := True;
  FGridPanel.AutoWidth := True;
  FGridPanel.ColumnLines := True;
  FGridPanel.TrackMouseOver := True;
  LKeyFieldNames := Join(ViewTable.GetKeyFieldAliasedNames(True), ',');
  FGridPanel.On('rowdblclick', AjaxSelection(RowDblClick, LSelModel, LKeyFieldNames, LKeyFieldNames, []));

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

procedure TKExtListPanelController.CreateStoreAndView;
var
  LGroupingFieldName: string;
  LGroupingMenu: Boolean;
  LCountTemplate: string;
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
        '{text} ({[values.rs.length]} {[values.rs.length > 1 ? "%ITEMS%" : "%ITEM"]})');
      LCountTemplate := ReplaceText(LCountTemplate, '%ITEMS%', _(ViewTable.GetString('Controller/Grouping/ShowCount/PluralItemName', 'items')));
      LCountTemplate := ReplaceText(LCountTemplate, '%ITEM%', _(ViewTable.GetString('Controller/Grouping/ShowCount/ItemName', 'item')));
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
end;

procedure TKExtListPanelController.InitFieldsAndColumns;
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
    begin
      case AViewField.DataType of
        edtBoolean: Result := TExtGridBooleanColumn.AddTo(FGridPanel.Columns);
        edtDate:
        begin
          Result := TExtGridDateColumn.AddTo(FGridPanel.Columns);
          TExtGridDateColumn(Result).Format := DelphiDateFormatToJSDateFormat(Environment.UserFormatSettings.ShortDateFormat);
        end;
        edtTime:
        begin
          Result := TExtGridDateColumn.AddTo(FGridPanel.Columns);
          TExtGridDateColumn(Result).Format := DelphiDateFormatToJSDateFormat(Environment.UserFormatSettings.ShortTimeFormat);
        end;
        edtDateTime:
        begin
          Result := TExtGridDateColumn.AddTo(FGridPanel.Columns);
          TExtGridDateColumn(Result).Format :=
            DelphiDateFormatToJSDateFormat(Environment.UserFormatSettings.ShortDateFormat) + ' ' +
            DelphiTimeFormatToJSTimeFormat(Environment.UserFormatSettings.ShortTimeFormat);
        end;
        edtInteger:
        begin
          Result := TExtGridNumberColumn.AddTo(FGridPanel.Columns);
          TExtGridNumberColumn(Result).Format := '0';
        end;
        edtFloat, edtDecimal:
        begin
          Result := TExtGridNumberColumn.AddTo(FGridPanel.Columns);
          TExtGridNumberColumn(Result).Format := AdaptExtNumberFormat('0.00', Environment.UserFormatSettings);
        end;
        edtCurrency:
        begin
          Result := TExtGridNumberColumn.AddTo(FGridPanel.Columns);
          TExtGridNumberColumn(Result).Format := AdaptExtNumberFormat('0,0.00', Environment.UserFormatSettings);
        end;
      else
        Result := TExtGridColumn.AddTo(FGridPanel.Columns);
      end;
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

    if AViewField.DataType = edtBoolean then
    begin
      //LColumn.Align := alCenter;
      LColumn.RendererExtFunction := JSFunction('V', 'return "<div class=''x-grid3-check-col"+(V?"-on":"")+"''></div>";');
    end
    { TODO : implement images and hidden text }
    else if (AViewField.GetString('ImageNames') <> '') then
    begin
      if AViewField.GetBoolean('HideText') then
      begin
        LColumn.Align := alCenter;
      end
      else
      begin
        LColumn.Align := alLeft;
      end;
    end
    else if AViewField.DataType in [edtInteger, edtCurrency, edtFloat, edtDecimal] then
    begin
      LColumn.Align := alRight;
    end
    else
    begin
      LColumn.Align := alLeft;
    end;
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
    case AViewField.DataType of
      edtUnknown: LField.Type_ := 'auto';
      edtString: LField.Type_ := 'string';
      edtInteger: LField.Type_ := 'int';
      edtDate, edtTime, edtDateTime: LField.Type_ := 'date';
      edtBoolean: LField.Type_ := 'boolean';
      edtCurrency, edtFloat, edtDecimal: LField.Type_ := 'float';
      edtObject: raise EKError.CreateFmt(_('Data type %s not supported in JavaScript.'),
        [EFDataTypeToString(AViewField.DataType)]);
    end;
  end;

begin
  Assert(ViewTable <> nil);

  LLayoutName := ViewTable.GetString('Controller/List/Layout');
  if LLayoutName <> '' then
    LLayout := View.Catalog.Layouts.FindLayout(LLayoutName)
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

procedure TKExtListPanelController.RowDblClick;
begin
  EditOrViewCurrentRecord;
end;

procedure TKExtListPanelController.EditOrViewCurrentRecord;
begin
  ShowEditWindow(LocateRecordFromSession, emEditCurrentRecord);
end;

procedure TKExtListPanelController.ShowEditWindow(const ARecord: TKRecord;
  const AEditMode: TKEditMode);
var
  LFormControllerType: string;
  LFormController: IKExtController;

  function IsReadOnly: Boolean;
  begin
    Result := View.GetBoolean('IsReadOnly') or ViewTable.IsReadOnly or View.GetBoolean('Controller/PreventEditing')
      or not ViewTable.IsAccessGranted(ACM_MODIFY);
  end;

begin
  Assert((AEditMode = emNewrecord) or Assigned(ARecord));
  Assert(View <> nil);
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

  LFormControllerType := View.GetString('Controller/FormController', 'Form');
  LFormController := TKExtControllerFactory.Instance.CreateController(View, FEditHostWindow, Self, LFormControllerType);
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
end;

procedure TKExtListPanelController.UpdateObserver(const ASubject: IEFSubject;
  const AContext: string);
begin
  inherited;
  if (AContext = 'FilterChanged') then
    RefreshData;
  if (AContext = 'Confirmed') and Supports(ASubject.AsObject, IKExtController) then
    RefreshData;
end;

function TKExtListPanelController.LocateRecordFromSession: TKRecord;
var
  LKey: TEFNode;
begin
  Assert(ServerStore.RecordCount > 0);

  LKey := TEFNode.Create;
  try
    LKey.Assign(ServerStore.Key);
    Session.GetQueryValues(LKey, True,
      function(const AName: string): string
      begin
        Result := ViewTable.FieldByName(AName).GetMinifiedName;
      end);
    Result := ServerStore.Records.GetRecord(LKey);
  finally
    FreeAndNil(LKey);
  end;
end;

procedure TKExtListPanelController.NewRecord(This: TExtButton; E: TExtEventObjectSingleton);
begin
  ShowEditWindow(nil, emNewRecord);
end;

procedure TKExtListPanelController.LoadData;
begin
  inherited;
  Assert(Assigned(FGridPanel));

  if FGridPanel.Columns.Count = 0 then
    InitFieldsAndColumns;
  if AutoLoadData then
    RefreshData;
end;

function TKExtListPanelController.GetRefreshJSCode: string;
begin
  Assert(Assigned(FStore));

  if Assigned(FPagingToolbar) then
    Result := FPagingToolbar.JSName + '.dorefresh();'
  else
    Result := FStore.JSName + '.load({params:{start:0,limit:' + IntToStr(FPageRecordCount) + ',Obj:"' + JSName + '"}});';
end;

procedure TKExtListPanelController.RefreshData;
begin
  Assert(Assigned(FStore));

  if Assigned(FPagingToolbar) then
    FPagingToolbar.DoRefresh
  else
    FStore.Load(JSObject('params:{start:0,limit:' + IntToStr(FPageRecordCount) + ',Obj:"' + JSName + '"}'));
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

initialization
  TKExtControllerRegistry.Instance.RegisterClass('List', TKExtListPanelController);

finalization
  TKExtControllerRegistry.Instance.UnregisterClass('List');

end.
