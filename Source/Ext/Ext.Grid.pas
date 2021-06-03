unit Ext.Grid;

interface

uses
  Classes
  , StrUtils
  , Kitto.JS
  , Ext.Base
  , Ext.Data
  ;

type
  TExtGridColumn = class;
  TExtGridNumberColumn = class;
  TExtViewTable = class;
  TExtGridBooleanColumn = class;
  TExtGridDateColumn = class;
  TExtGridAbstractSelectionModel = class;
  TExtSelectionRowModel = class;
  TExtGridGridPanel = class;

  TExtGridColumnAlign = (caLeft, caRight, caCenter);

  TExtGridColumn = class(TExtObject)
  private
    FAlign: TExtGridColumnAlign;
    FHideable: Boolean;
    FWidth: Integer;
    FHeader: string;
    FHidden: Boolean;
    FDataIndex: string;
    FRendererFunc: TExtExpression;
    FEmptyGroupText: string;
    FSortable: Boolean;
    FEditor: TExtFormField;
    FCss: string;
    FFixed: Boolean;
    FWidthFunc: TExtExpression;
    FGroupRenderer: TExtExpression;
    FEditable: Boolean;
    FRenderer: string;
    FGroupName: string;
    FDisabled: Boolean;
    FDisabledCls: string;
    FMenuDisabled: Boolean;
    procedure SetAlign(const AValue: TExtGridColumnAlign);
    procedure SetCss(const AValue: string);
    procedure SetDataIndex(const AValue: string);
    procedure SetEditable(const AValue: Boolean);
    procedure SetEditor(AValue: TExtFormField);
    procedure SetEmptyGroupText(const AValue: string);
    procedure SetFixed(const AValue: Boolean);
    procedure SetGroupName(const AValue: string);
    procedure SetGroupRenderer(const AValue: TExtExpression);
    procedure SetHeader(const AValue: string);
    procedure SetHidden(const AValue: Boolean);
    procedure SetHideable(const AValue: Boolean);
    procedure SetRenderer(const AValue: string);
    procedure SetSortable(const AValue: Boolean);
    procedure SetWidth(const AValue: Integer);
    procedure SetRendererFunc(const AValue: TExtExpression);
    procedure SetWidthFunc(const AValue: TExtExpression);
    procedure SetDisabled(const AValue: Boolean);
    procedure SetDisabledCls(const AValue: string);
    procedure SetMenuDisabled(const AValue: Boolean);
  protected
    procedure InitDefaults; override;
    function GetObjectNamePrefix: string; override;
  public
    class function JSClassName: string; override;
    class function JSXType: string; override;
    property Align: TExtGridColumnAlign read FAlign write SetAlign;
    property Css: string read FCss write SetCss;
    property DataIndex: string read FDataIndex write SetDataIndex;
    property Disabled: Boolean read FDisabled write SetDisabled;
    property DisabledCls: string read FDisabledCls write SetDisabledCls;
    property Editable: Boolean read FEditable write SetEditable;
    property Editor: TExtFormField read FEditor write SetEditor;
    property EmptyGroupText: string read FEmptyGroupText write SetEmptyGroupText;
    property Fixed: Boolean read FFixed write SetFixed;
    property GroupName: string read FGroupName write SetGroupName;
    property GroupRenderer: TExtExpression read FGroupRenderer write SetGroupRenderer;
    property Header: string read FHeader write SetHeader;
    property Hidden: Boolean read FHidden write SetHidden;
    property Hideable: Boolean read FHideable write SetHideable;
    property Renderer: string read FRenderer write SetRenderer;
    property RendererFunc: TExtExpression read FRendererFunc write SetRendererFunc;
    property Sortable: Boolean read FSortable write SetSortable;
    property Width: Integer read FWidth write SetWidth;
    property WidthFunc: TExtExpression read FWidthFunc write SetWidthFunc;
    property MenuDisabled: Boolean read FMenuDisabled write SetMenuDisabled;
  end;

  TExtGridNumberColumn = class(TExtGridColumn)
  private
    FFormat: string; // '0,000.00'
    procedure SetFormat(const AValue: string);
  protected
    procedure InitDefaults; override;
  public
    class function JSClassName: string; override;
    class function JSXType: string; override;
    property Format: string read FFormat write SetFormat;
  end;

  TExtViewTable = class(TExtBase)
  private
    FAutoFill: Boolean;
    FForceFit: Boolean;
    FDisableSelection: Boolean;
    FGetRowClass: TExtExpression;
    FEmptyText: string;
    FEnableRowBody: Boolean;
    procedure SetAutoFill(const AValue: Boolean);
    procedure SetEmptyText(const AValue: string);
    procedure SetEnableRowBody(const AValue: Boolean);
    procedure SetForceFit(const AValue: Boolean);
    procedure SetGetRowClass(const AValue: TExtExpression);
    procedure SetDisableSelection(const AValue: Boolean);
  protected
    procedure InitDefaults; override;
    function GetObjectNamePrefix: string; override;
  public
    class function JSClassName: string; override;
    property AutoFill: Boolean read FAutoFill write SetAutoFill;
    property DisableSelection: Boolean read FDisableSelection write SetDisableSelection;
    property EmptyText: string read FEmptyText write SetEmptyText;
    property EnableRowBody: Boolean read FEnableRowBody write SetEnableRowBody;
    property ForceFit: Boolean read FForceFit write SetForceFit;
    property GetRowClass: TExtExpression read FGetRowClass write SetGetRowClass;
  end;

  TExtGridView = class(TExtViewTable)
  public
    class function JSClassName: string; override;
  end;

  TExtGridBooleanColumn = class(TExtGridColumn)
  public
    class function JSClassName: string; override;
    class function JSXType: string; override;
  end;

  TExtGridDateColumn = class(TExtGridColumn)
  private
    FFormat: string; // 'm/d/Y'
    procedure SetFormat(const AValue: string);
  protected
    procedure InitDefaults; override;
  public
    class function JSClassName: string; override;
    class function JSXType: string; override;
    property Format: string read FFormat write SetFormat;
  end;

  TExtGridCheckColumn = class(TExtGridColumn)
  public
    class function JSClassName: string; override;
    class function JSXType: string; override;
  end;

  TExtGridAbstractSelectionModel = class(TExtEvented)
  private
    FStore: TExtDataStore;
    procedure SetStore(const AValue: TExtDataStore);
  strict protected
    function GetObjectNamePrefix: string; override;
  public
    class function JSClassName: string; override;
    property Store: TExtDataStore read FStore write SetStore;
  end;

  TExtSelectionRowModel = class(TExtGridAbstractSelectionModel)
  private
    FMode: string;
    procedure SetMode(const AValue: string);
  protected
    procedure InitDefaults; override;
  public
    const
      SIMPLE_SELECT = 'SIMPLE';
      SINGLE_SELECT = 'SINGLE';
      MULTI_SELECT = 'MULTI';
    class function JSClassName: string; override;
    function Each(const AFunction: TExtExpression; const AScope: TExtObject = nil): TExtExpression;
    function GetCount: TExtExpression;
    function GetSelected: TExtExpression;
    function GetSelections: TExtExpression;
    function Select(const AIndex: Integer; const AKeepExisting: Boolean = False): TExtExpression;
    property Mode: string read FMode write SetMode;
  end;

  TExtGridGroupingView = class(TExtGridView)
  private
    FGroupMode: string;
    FEnableGrouping: Boolean;
    FShowGroupsText: string;
    FHideGroupedColumn: Boolean;
    FGroupTextTpl: string;
    FEnableNoGroups: Boolean;
    FEmptyGroupText: string;
    FShowGroupName: Boolean;
    FEnableGroupingMenu: Boolean;
    FGroupRenderer: TExtExpression;
    FStartCollapsed: Boolean;
    procedure SetEmptyGroupText(const AValue: string);
    procedure SetEnableGrouping(const AValue: Boolean);
    procedure SetEnableGroupingMenu(const AValue: Boolean);
    procedure SetEnableNoGroups(const AValue: Boolean);
    procedure SetGroupMode(const AValue: string);
    procedure SetGroupRenderer(const AValue: TExtExpression);
    procedure SetGroupTextTpl(const AValue: string);
    procedure SetHideGroupedColumn(const AValue: Boolean);
    procedure SetShowGroupName(const AValue: Boolean);
    procedure SetShowGroupsText(const AValue: string);
    procedure SetStartCollapsed(const AValue: Boolean);
  protected
    procedure InitDefaults; override;
  public
    class function JSClassName: string; override;
    property EmptyGroupText: string read FEmptyGroupText write SetEmptyGroupText;
    property EnableGrouping: Boolean read FEnableGrouping write SetEnableGrouping;
    property EnableGroupingMenu: Boolean read FEnableGroupingMenu
      write SetEnableGroupingMenu;
    property EnableNoGroups: Boolean read FEnableNoGroups write SetEnableNoGroups;
    property GroupMode: string read FGroupMode write SetGroupMode;
    property GroupRenderer: TExtExpression read FGroupRenderer write SetGroupRenderer;
    property GroupTextTpl: string read FGroupTextTpl write SetGroupTextTpl;
    property HideGroupedColumn: Boolean read FHideGroupedColumn
      write SetHideGroupedColumn;
    property ShowGroupName: Boolean read FShowGroupName write SetShowGroupName;
    property ShowGroupsText: string read FShowGroupsText write SetShowGroupsText;
    property StartCollapsed: Boolean read FStartCollapsed write SetStartCollapsed;
  end;

  TExtGridCheckboxSelectionModel = class(TExtSelectionRowModel)
  public
    class function JSClassName: string; override;
  end;

  TExtGridGridPanel = class(TExtPanel)
  private
    FAutoExpandColumn: string;
    FColumnLines: Boolean;
    FColumns: TJSObjectArray;
    FDisableSelection: Boolean;
    FEnableHdMenu: Boolean;
    FFeatures: TJSObjectArray;
    FSelModel: TExtObject;
    FStore: TExtDataStore;
    FStripeRows: Boolean;
    FTrackMouseOver: Boolean;
    FView: TExtObject;
    FViewConfig: TExtObject;
    FAutoLoad: Boolean;
    procedure SetAutoExpandColumn(const AValue: string);
    procedure SetColumnLines(const AValue: Boolean);
    procedure SetDisableSelection(const AValue: Boolean);
    procedure SetEnableHdMenu(AValue: Boolean);
    procedure SetSelModel(const AValue: TExtObject);
    procedure SetStore(const AValue: TExtDataStore);
    procedure SetStripeRows(const AValue: Boolean);
    procedure SetTrackMouseOver(const AValue: Boolean);
    procedure SetView(const AValue: TExtObject);
    procedure SetAutoLoad(const AValue: boolean);
  protected
    procedure InitDefaults; override;
    function GetObjectNamePrefix: string; override;
  public
    class function JSClassName: string; override;
    property AutoExpandColumn: string read FAutoExpandColumn write SetAutoExpandColumn;
    property ColumnLines: Boolean read FColumnLines write SetColumnLines;
    property Columns: TJSObjectArray read FColumns;
    property DisableSelection: Boolean read FDisableSelection write SetDisableSelection;
    property EnableHdMenu: Boolean read FEnableHdMenu write SetEnableHdMenu;
    property Features: TJSObjectArray read FFeatures;
    property SelModel: TExtObject read FSelModel write SetSelModel;
    property Store: TExtDataStore read FStore write SetStore;
    property StripeRows: Boolean read FStripeRows write SetStripeRows;
    property TrackMouseOver: Boolean read FTrackMouseOver write SetTrackMouseOver;
    property View: TExtObject read FView write SetView;
    property ViewConfig: TExtObject read FViewConfig;
    property AutoLoad: boolean read FAutoLoad write SetAutoLoad;
  end;

  TExtGridFeatureFeature = class(TExtBase)
  end;

  TExtGridGroupingFeature = class(TExtGridFeatureFeature)
  private
    FStartCollapsed: Boolean;
    FEnableGroupingMenu: Boolean;
    FEnableNoGroups: Boolean;
    FHideGroupedHeader: Boolean;
    FShowSummaryRow: Boolean;
    procedure SetStartCollapsed(const AValue: Boolean);
    procedure SetEnableGroupingMenu(const AValue: Boolean);
    procedure SetEnableNoGroups(const AValue: Boolean);
    procedure SetHideGroupedHeader(const AValue: Boolean);
    procedure SetShowSummaryRow(const AValue: Boolean);
  protected
    procedure InitDefaults; override;
  public
    class function JSClassName: string; override;
    property StartCollapsed: Boolean read FStartCollapsed write SetStartCollapsed;
    property EnableGroupingMenu: Boolean read FEnableGroupingMenu write SetEnableGroupingMenu;
    property EnableNoGroups: Boolean read FEnableNoGroups write SetEnableNoGroups;
    property HideGroupedHeader: Boolean read FHideGroupedHeader write SetHideGroupedHeader;
    property ShowSummaryRow: Boolean read FShowSummaryRow write SetShowSummaryRow;
  end;

  TExtGridPluginEditing = class(TExtPluginAbstract)
  end;

  TExtGridPluginCellEditing = class(TExtGridPluginEditing)
  private
    FClicksToEdit: Integer;
    procedure SetClicksToEdit(const AValue: Integer);
  public
    class function JSClassName: string; override;
    class function JSXType: string; override;
    property ClicksToEdit: Integer read FClicksToEdit write SetClicksToEdit;
  end;

  TExtGridPluginRowEditing = class(TExtGridPluginEditing)
  public
    class function JSClassName: string; override;
  end;

function OptionAsGridColumnAlign(const AAlign: string): TExtGridColumnAlign;

implementation

uses
  SysUtils
  , EF.Types
  , EF.Localization
  , Kitto.JS.Formatting
  , Kitto.Web.Response
  ;

function OptionAsGridColumnAlign(const AAlign: string): TExtGridColumnAlign;
begin
  //alLeft, alRight, alCenter
  if SameText(AAlign, 'Left') then
    Result := caLeft
  else if SameText(AAlign, 'Right') then
    Result := caRight
  else if SameText(AAlign, 'Center') then
    Result := caCenter
  else
    raise EEFError.CreateFmt(_('Invalid value %s. Valid values: "Left", "Right", "Center".'), [AAlign]);
end;

procedure TExtGridColumn.SetAlign(const AValue: TExtGridColumnAlign);
begin
  FAlign := AValue;
  SetConfigItem('align', TJS.EnumToJSString(TypeInfo(TExtGridColumnAlign), Ord(AValue)));
end;

procedure TExtGridColumn.SetCss(const AValue: string);
begin
  FCss := SetConfigItem('css', AValue);
end;

procedure TExtGridColumn.SetDataIndex(const AValue: string);
begin
  FDataIndex := SetConfigItem('dataIndex', AValue);
end;

procedure TExtGridColumn.SetDisabled(const AValue: Boolean);
begin
  FDisabled := SetConfigItem('disabled', 'setDisabled', AValue);
end;

procedure TExtGridColumn.SetDisabledCls(const AValue: string);
begin
  FDisabledCls := SetConfigItem('disabledCls', AValue);
end;

procedure TExtGridColumn.SetEditable(const AValue: Boolean);
begin
  FEditable := SetConfigItem('editable', AValue);
end;

procedure TExtGridColumn.SetEditor(AValue: TExtFormField);
begin
  FEditor := AValue;
  SetConfigItem('editor', 'setEditor', AValue);
end;

procedure TExtGridColumn.SetEmptyGroupText(const AValue: string);
begin
  FEmptyGroupText := SetConfigItem('emptyGroupText', AValue);
end;

procedure TExtGridColumn.SetFixed(const AValue: Boolean);
begin
  FFixed := SetConfigItem('fixed', AValue);
end;

procedure TExtGridColumn.SetGroupName(const AValue: string);
begin
  FGroupName := SetConfigItem('groupName', AValue);
end;

procedure TExtGridColumn.SetGroupRenderer(const AValue: TExtExpression);
begin
  FGroupRenderer := SetConfigItem('groupRenderer', AValue);
end;

procedure TExtGridColumn.SetHeader(const AValue: string);
begin
  FHeader := SetConfigItem('header', AValue);
end;

procedure TExtGridColumn.SetHidden(const AValue: Boolean);
begin
  FHidden := SetConfigItem('hidden', 'setHidden', AValue);
end;

procedure TExtGridColumn.SetHideable(const AValue: Boolean);
begin
  FHideable := SetConfigItem('hideable', AValue);
end;

procedure TExtGridColumn.SetMenuDisabled(const AValue: Boolean);
begin
  FMenuDisabled := SetConfigItem('menuDisabled', AValue);
end;

procedure TExtGridColumn.SetRenderer(const AValue: string);
begin
  FRenderer := SetConfigItem('renderer', AValue);
end;

procedure TExtGridColumn.SetSortable(const AValue: Boolean);
begin
  FSortable := SetConfigItem('sortable', AValue);
end;

procedure TExtGridColumn.SetWidth(const AValue: Integer);
begin
  FWidth := SetConfigItem('width', AValue);
end;

procedure TExtGridColumn.SetWidthFunc(const AValue: TExtExpression);
begin
  FWidthFunc := SetConfigItem('width', AValue);
end;

procedure TExtGridColumn.SetRendererFunc(const AValue: TExtExpression);
begin
  FRendererFunc := SetConfigItem('renderer', AValue);
end;

class function TExtGridColumn.JSClassName: string;
begin
  Result := 'Ext.grid.column.Column';
end;

class function TExtGridColumn.JSXType: string;
begin
  Result := 'gridcolumn';
end;

function TExtGridColumn.GetObjectNamePrefix: string;
begin
  Result := 'col';
end;

procedure TExtGridColumn.InitDefaults;
begin
  inherited;
  FEditor := TExtFormField.CreateInternal(Self, 'editor');
  FHideable := true;
end;

procedure TExtGridNumberColumn.SetFormat(const AValue: string);
begin
  FFormat := SetConfigItem('format', AValue);
end;

class function TExtGridNumberColumn.JSClassName: string;
begin
  Result := 'Ext.grid.column.Number';
end;

class function TExtGridNumberColumn.JSXType: string;
begin
  Result := 'numbercolumn';
end;

procedure TExtGridNumberColumn.InitDefaults;
begin
  inherited;
  FFormat := '0,000.00';
end;

procedure TExtViewTable.SetAutoFill(const AValue: Boolean);
begin
  FAutoFill := SetConfigItem('autoFill', AValue);
end;

procedure TExtViewTable.SetDisableSelection(const AValue: Boolean);
begin
  FDisableSelection := SetConfigItem('disableSelection', AValue);
end;

procedure TExtViewTable.SetEmptyText(const AValue: string);
begin
  FEmptyText := SetConfigItem('emptyText', AValue);
end;

procedure TExtViewTable.SetEnableRowBody(const AValue: Boolean);
begin
  FEnableRowBody := SetConfigItem('enableRowBody', AValue);
end;

procedure TExtViewTable.SetForceFit(const AValue: Boolean);
begin
  FForceFit := SetConfigItem('forceFit', AValue);
end;

procedure TExtViewTable.SetGetRowClass(const AValue: TExtExpression);
begin
  FGetRowClass := SetConfigItem('getRowClass', AValue);
end;

class function TExtViewTable.JSClassName: string;
begin
  Result := 'Ext.view.Table';
end;

procedure TExtViewTable.InitDefaults;
begin
  inherited;
  DisableSelection := False;
end;

function TExtViewTable.GetObjectNamePrefix: string;
begin
  Result := 'viewtable';
end;

class function TExtGridBooleanColumn.JSClassName: string;
begin
  Result := 'Ext.grid.column.Boolean';
end;

procedure TExtGridDateColumn.SetFormat(const AValue: string);
begin
  FFormat := SetConfigItem('format', AValue);
end;

class function TExtGridDateColumn.JSClassName: string;
begin
  Result := 'Ext.grid.column.Date';
end;

class function TExtGridDateColumn.JSXType: string;
begin
  Result := 'datecolumn';
end;

procedure TExtGridDateColumn.InitDefaults;
begin
  inherited;
  FFormat := 'm/d/Y';
end;

procedure TExtGridAbstractSelectionModel.SetStore(const AValue: TExtDataStore);
begin
  FStore.Free;
  FStore := TExtDataStore(SetConfigItem('store', AValue));
end;

class function TExtGridAbstractSelectionModel.JSClassName: string;
begin
  Result := 'Ext.grid.AbstractSelectionModel';
end;

function TExtGridAbstractSelectionModel.GetObjectNamePrefix: string;
begin
  Result := 'selmodel';
end;

procedure TExtSelectionRowModel.SetMode(const AValue: string);
begin
  FMode := SetConfigItem('mode', AValue);
end;

class function TExtSelectionRowModel.JSClassName: string;
begin
  Result := 'Ext.selection.RowModel';
end;

function TExtSelectionRowModel.Each(const AFunction: TExtExpression; const AScope: TExtObject): TExtExpression;
begin
  Result := TKWebResponse.Current.Items.CallMethod(Self, 'each')
    .AddParam(AFunction)
    .AddParam(AScope)
    .AsExpression;
end;

function TExtSelectionRowModel.GetCount: TExtExpression;
begin
  Result := TKWebResponse.Current.Items.CallMethod(Self, 'getCount').AsExpression;
end;

function TExtSelectionRowModel.GetSelected: TExtExpression;
begin
  Result := TKWebResponse.Current.Items.CallMethod(Self, 'getSelected').AsExpression;
end;

function TExtSelectionRowModel.GetSelections: TExtExpression;
begin
  Result := TKWebResponse.Current.Items.CallMethod(Self, 'getSelections').AsExpression;
end;

procedure TExtSelectionRowModel.InitDefaults;
begin
  inherited;
  FMode := SINGLE_SELECT;
end;

function TExtSelectionRowModel.Select(const AIndex: Integer; const AKeepExisting: Boolean): TExtExpression;
begin
  Result := TKWebResponse.Current.Items.CallMethod(Self, 'select')
    .AddParam(AIndex)
    .AddParam(AKeepExisting)
    .AsExpression;
end;

procedure TExtGridGroupingView.SetEmptyGroupText(const AValue: string);
begin
  FEmptyGroupText := SetConfigItem('emptyGroupText', AValue);
end;

procedure TExtGridGroupingView.SetEnableGrouping(const AValue: Boolean);
begin
  FEnableGrouping := SetConfigItem('enableGrouping', AValue);
end;

procedure TExtGridGroupingView.SetEnableGroupingMenu(const AValue: Boolean);
begin
  FEnableGroupingMenu := SetConfigItem('enableGroupingMenu', AValue);
end;

procedure TExtGridGroupingView.SetEnableNoGroups(const AValue: Boolean);
begin
  FEnableNoGroups := SetConfigItem('enableNoGroups', AValue);
end;

procedure TExtGridGroupingView.SetGroupMode(const AValue: string);
begin
  FGroupMode := SetConfigItem('groupMode', AValue);
end;

procedure TExtGridGroupingView.SetGroupRenderer(const AValue: TExtExpression);
begin
  FGroupRenderer := SetConfigItem('groupRenderer', AValue);
end;

procedure TExtGridGroupingView.SetGroupTextTpl(const AValue: string);
begin
  FGroupTextTpl := SetConfigItem('groupTextTpl', AValue);
end;

procedure TExtGridGroupingView.SetHideGroupedColumn(const AValue: Boolean);
begin
  FHideGroupedColumn := SetConfigItem('hideGroupedColumn', AValue);
end;

procedure TExtGridGroupingView.SetShowGroupName(const AValue: Boolean);
begin
  FShowGroupName := SetConfigItem('showGroupName', AValue);
end;

procedure TExtGridGroupingView.SetShowGroupsText(const AValue: string);
begin
  FShowGroupsText := SetConfigItem('showGroupsText', AValue);
end;

procedure TExtGridGroupingView.SetStartCollapsed(const AValue: Boolean);
begin
  FStartCollapsed := SetConfigItem('startCollapsed', AValue);
end;

class function TExtGridGroupingView.JSClassName: string;
begin
  Result := 'Ext.grid.GroupingView';
end;

procedure TExtGridGroupingView.InitDefaults;
begin
  inherited;
  FEnableGrouping := true;
  FEnableGroupingMenu := true;
  FEnableNoGroups := true;
  FGroupTextTpl := '{text}';
  FShowGroupName := true;
  FShowGroupsText := 'Show in Groups';
end;

class function TExtGridCheckboxSelectionModel.JSClassName: string;
begin
  Result := 'Ext.grid.CheckboxSelectionModel';
end;

procedure TExtGridGridPanel.SetAutoExpandColumn(const AValue: string);
begin
  FAutoExpandColumn := SetConfigItem('autoExpandColumn', AValue);
end;

procedure TExtGridGridPanel.SetAutoLoad(const AValue: boolean);
begin
  FAutoLoad := SetConfigItem('autoLoad', AValue);
end;

procedure TExtGridGridPanel.SetColumnLines(const AValue: Boolean);
begin
  FColumnLines := SetConfigItem('columnLines', AValue);
end;

procedure TExtGridGridPanel.SetDisableSelection(const AValue: Boolean);
begin
  FDisableSelection := SetConfigItem('disableSelection', AValue);
end;

procedure TExtGridGridPanel.SetEnableHdMenu(AValue: Boolean);
begin
  FEnableHdMenu := SetConfigItem('enableHdMenu', AValue);
end;

procedure TExtGridGridPanel.SetSelModel(const AValue: TExtObject);
begin
  FSelModel := SetConfigItem('selModel', AValue);
end;

procedure TExtGridGridPanel.SetStore(const AValue: TExtDataStore);
begin
  FStore.Free;
  FStore := TExtDataStore(SetConfigItem('store', AValue));
end;

procedure TExtGridGridPanel.SetStripeRows(const AValue: Boolean);
begin
  FStripeRows := SetConfigItem('stripeRows', AValue);
end;

procedure TExtGridGridPanel.SetTrackMouseOver(const AValue: Boolean);
begin
  FTrackMouseOver := SetConfigItem('trackMouseOver', AValue);
end;

procedure TExtGridGridPanel.SetView(const AValue: TExtObject);
begin
  FView.Free;
  FView := SetConfigItem('view', AValue);
end;

class function TExtGridGridPanel.JSClassName: string;
begin
  Result := 'Ext.grid.Panel';
end;

procedure TExtGridGridPanel.InitDefaults;
begin
  inherited;
  FColumns := CreateConfigObjectArray('columns');
  FFeatures := CreateConfigObjectArray('features');
  FViewConfig := CreateConfigObject('viewConfig');
  AutoLoad := True;
end;

function TExtGridGridPanel.GetObjectNamePrefix: string;
begin
  Result := 'gridpnl';
end;

{ TExtGridView }

class function TExtGridView.JSClassName: string;
begin
  Result := 'Ext.grid.View';
end;

{ TExtGridGroupingFeature }

procedure TExtGridGroupingFeature.InitDefaults;
begin
  inherited;
  FStartCollapsed := False;
  FEnableGroupingMenu := True;
  FEnableNoGroups := True;
  FHideGroupedHeader := False;
  FShowSummaryRow := False;
end;

class function TExtGridGroupingFeature.JSClassName: string;
begin
  Result := 'Ext.grid.feature.Grouping';
end;

procedure TExtGridGroupingFeature.SetEnableGroupingMenu(const AValue: Boolean);
begin
  FEnableGroupingMenu := AValue;
  SetConfigItem('enableGroupingMenu', AValue);
end;

procedure TExtGridGroupingFeature.SetEnableNoGroups(const AValue: Boolean);
begin
  FEnableNoGroups := AValue;
  SetConfigItem('enableNoGroups', AValue);
end;

procedure TExtGridGroupingFeature.SetHideGroupedHeader(const AValue: Boolean);
begin
  FHideGroupedHeader := AValue;
  SetConfigItem('hideGroupedHeader', AValue);
end;

procedure TExtGridGroupingFeature.SetShowSummaryRow(const AValue: Boolean);
begin
  FShowSummaryRow := AValue;
  SetConfigItem('showSummaryRow', AValue);
end;

procedure TExtGridGroupingFeature.SetStartCollapsed(const AValue: Boolean);
begin
  FStartCollapsed := AValue;
  SetConfigItem('startCollapsed', AValue);
end;

{ TExtGridPluginCellEditing }

class function TExtGridPluginCellEditing.JSClassName: string;
begin
  Result := 'Ext.grid.plugin.CellEditing';
end;

class function TExtGridPluginCellEditing.JSXType: string;
begin
  Result := 'plugin.cellediting';
end;

procedure TExtGridPluginCellEditing.SetClicksToEdit(const AValue: Integer);
begin
  FClicksToEdit := AValue;
  SetConfigItem('clicksToEdit', AValue);
end;

{ TExtGridPluginRowEditing }

class function TExtGridPluginRowEditing.JSClassName: string;
begin
  Result := 'Ext.grid.plugin.RowEditing';
end;

class function TExtGridBooleanColumn.JSXType: string;
begin
  Result := 'booleancolumn';
end;

{ TExtGridCheckColumn }

class function TExtGridCheckColumn.JSClassName: string;
begin
  Result := 'Ext.grid.column.Check';
end;

class function TExtGridCheckColumn.JSXType: string;
begin
  Result := 'checkcolumn';
end;

end.
