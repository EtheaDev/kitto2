unit Ext.Menu;

interface

uses
  StrUtils, Kitto.Ext, Ext.Base;

type
  TExtMenuMenuMgrSingleton = class;
  TExtMenuBaseItem = class;
  TExtMenuSeparator = class;
  TExtMenuTextItem = class;
  TExtMenuItem = class;
  TExtMenuCheckItem = class;
  TExtMenuMenu = class;
  TExtMenuColorMenu = class;
  TExtMenuDateMenu = class;

  TExtMenuMenuMgrSingleton = class(TExtObject)
  public
    class function JSClassName: string; override;
  end;

  // Procedural types for events TExtMenuBaseItem
  TExtMenuBaseItemOnActivate = procedure(This: TExtMenuBaseItem) of object;
  TExtMenuBaseItemOnClick = procedure(This: TExtMenuBaseItem; E: TExtEventObjectSingleton)
    of object;
  TExtMenuBaseItemOnDeactivate = procedure(This: TExtMenuBaseItem) of object;

  TExtMenuBaseItem = class(TExtComponent)
  private
    FActiveClass: string; // 'x-menu-item-active'
    FCanActivate: Boolean;
    FClickHideDelay: Integer; // 1
    FHandler: TExtExpression;
    FHideOnClick: Boolean; // true
    FScope: TExtObject;
    FParentMenu: TExtMenuMenu;
    FOnActivate: TExtMenuBaseItemOnActivate;
    FOnClick: TExtMenuBaseItemOnClick;
    FOnDeactivate: TExtMenuBaseItemOnDeactivate;
    procedure _SetHandler(const AValue: TExtExpression);
    procedure SetOnClick(const AValue: TExtMenuBaseItemOnClick);
  protected
    procedure InitDefaults; override;
    function GetObjectNamePrefix: string; override;
  public
    procedure HandleEvent(const AEvtName: string); override;
    class function JSClassName: string; override;
    function SetHandler(const AHandler: TExtExpression; const AScope: TExtObject): TExtExpression;
    property Handler: TExtExpression read FHandler write _SetHandler;
    property OnClick: TExtMenuBaseItemOnClick read FOnClick write SetOnClick;
  end;

  TExtMenuSeparator = class(TExtMenuBaseItem)
  public
    class function JSClassName: string; override;
  end;

  TExtMenuTextItem = class(TExtMenuBaseItem)
  public
    class function JSClassName: string; override;
  end;

  TExtMenuItem = class(TExtMenuBaseItem)
  private
    FIconCls: string;
    FMenu: TExtMenuMenu;
    FText: string;
    procedure SetIconCls(const AValue: string);
    procedure SetMenu(AValue: TExtMenuMenu);
    procedure SetText(const AValue: string);
  protected
    procedure InitDefaults; override;
  public
    class function JSClassName: string; override;
    property IconCls: string read FIconCls write SetIconCls;
    property Menu: TExtMenuMenu read FMenu write SetMenu;
    property Text: string read FText write SetText;
  end;

  TExtMenuCheckItem = class(TExtMenuItem)
  private
    FChecked: Boolean;
    procedure _SetChecked(const AValue: Boolean);
  public
    class function JSClassName: string; override;
    function SetChecked(const AChecked: Boolean; const ASuppressEvent: Boolean = False): TExtExpression;
    property Checked: Boolean read FChecked write _SetChecked;
  end;

  // Procedural types for events TExtMenuMenu
  TExtMenuMenuOnClick = procedure(This: TExtMenuMenu; MenuItem: TExtMenuItem;
    E: TExtEventObjectSingleton) of object;
  TExtMenuMenuOnItemclick = procedure(BaseItem: TExtMenuBaseItem;
    E: TExtEventObjectSingleton) of object;
  TExtMenuMenuOnMouseout = procedure(This: TExtMenuMenu; E: TExtEventObjectSingleton;
    MenuItem: TExtMenuItem) of object;
  TExtMenuMenuOnMouseover = procedure(This: TExtMenuMenu; E: TExtEventObjectSingleton;
    MenuItem: TExtMenuItem) of object;

  TExtMenuMenu = class(TExtContainer)
  private
    FOnClick: TExtMenuMenuOnClick;
    FOnItemclick: TExtMenuMenuOnItemclick;
    procedure SetOnClick(const AValue: TExtMenuMenuOnClick);
    procedure SetOnItemclick(const AValue: TExtMenuMenuOnItemclick);
  public
    procedure HandleEvent(const AEvtName: string); override;
    class function JSClassName: string; override;
    property OnClick: TExtMenuMenuOnClick read FOnClick write SetOnClick;
    property OnItemclick: TExtMenuMenuOnItemclick read FOnItemclick write SetOnItemclick;
  end;

  // Procedural types for events TExtMenuColorMenu
  TExtMenuColorMenuOnSelect = procedure(Palette: TExtColorPalette; Color: string)
    of object;

  TExtMenuColorMenu = class(TExtMenuMenu)
  public
    class function JSClassName: string; override;
  end;

  // Procedural types for events TExtMenuDateMenu
  TExtMenuDateMenuOnSelect = procedure(Picker: TExtDatePicker; Date: TDateTime) of object;

  TExtMenuDateMenu = class(TExtMenuMenu)
  public
    class function JSClassName: string; override;
  end;

function ExtMenuMenuMgr: TExtMenuMenuMgrSingleton;

implementation

uses
  Kitto.JS
  ;

function ExtMenuMenuMgr: TExtMenuMenuMgrSingleton;
begin
  if Session <> nil then
    Result := Session.GetSingleton<TExtMenuMenuMgrSingleton>(TExtMenuMenuMgrSingleton.JSClassName)
  else
    Result := nil;
end;

class function TExtMenuMenuMgrSingleton.JSClassName: string;
begin
  Result := 'Ext.menu.MenuMgr';
end;

procedure TExtMenuBaseItem._SetHandler(const AValue: TExtExpression);
begin
  FHandler.Free;
  FHandler := SetConfigItem('handler', 'setHandler', AValue);
end;

procedure TExtMenuBaseItem.SetOnClick(const AValue: TExtMenuBaseItemOnClick);
begin
  RemoveAllListeners('click');
  if Assigned(AValue) then
    //On('click', Ajax('click', ['This', '%0.nm', 'E', '%1.nm'], true));
    &On('click', AjaxCallMethod('click')
      .Event
      .AddRawParam('This', 'sender.nm')
      .AddRawParam('E', 'e.nm')
      .FunctionArgs('sender, e')
      .AsFunction);
  FOnClick := AValue;
end;

class function TExtMenuBaseItem.JSClassName: string;
begin
  Result := 'Ext.menu.BaseItem';
end;

procedure TExtMenuBaseItem.InitDefaults;
begin
  inherited;
  FActiveClass := 'x-menu-item-active';
  FClickHideDelay := 1;
  FHideOnClick := true;
  FScope := TExtObject.CreateInternal(Self, 'scope');
  FParentMenu := TExtMenuMenu.CreateInternal(Self, 'parentMenu');
end;

function TExtMenuBaseItem.SetHandler(const AHandler: TExtExpression; const AScope: TExtObject): TExtExpression;
begin
  FHandler.Free;
  FHandler := AHandler;
  Result := CallMethod('setHandler')
    .AddParam(AHandler)
    .AddParam(AScope)
    .AsExpression;
end;

function TExtMenuBaseItem.GetObjectNamePrefix: string;
begin
  Result := 'mi';
end;

procedure TExtMenuBaseItem.HandleEvent(const AEvtName: string);
begin
  inherited;
  if (AEvtName = 'activate') and Assigned(FOnActivate) then
    FOnActivate(TExtMenuBaseItem(ParamAsObject('This')))
  else if (AEvtName = 'click') and Assigned(FOnClick) then
    FOnClick(TExtMenuBaseItem(ParamAsObject('This')), nil)
  else if (AEvtName = 'deactivate') and Assigned(FOnDeactivate) then
    FOnDeactivate(TExtMenuBaseItem(ParamAsObject('This')));
end;

class function TExtMenuSeparator.JSClassName: string;
begin
  Result := 'Ext.menu.Separator';
end;

class function TExtMenuTextItem.JSClassName: string;
begin
  Result := 'Ext.menu.TextItem';
end;

procedure TExtMenuItem.SetIconCls(const AValue: string);
begin
  FIconCls := SetConfigItem('iconCls', AValue);
end;

procedure TExtMenuItem.SetMenu(AValue: TExtMenuMenu);
begin
  FMenu.Free;
  FMenu := TExtMenuMenu(SetConfigItem('menu', AValue));
end;

procedure TExtMenuItem.SetText(const AValue: string);
begin
  FText := SetConfigItem('text', 'setText', AValue);
end;

class function TExtMenuItem.JSClassName: string;
begin
  Result := 'Ext.menu.Item';
end;

procedure TExtMenuItem.InitDefaults;
begin
  inherited;
  FCanActivate := true;
  FMenu := TExtMenuMenu.CreateInternal(Self, 'menu');
end;

procedure TExtMenuCheckItem._SetChecked(const AValue: Boolean);
begin
  FChecked := SetConfigItem('checked', 'setChacked', AValue);
end;

class function TExtMenuCheckItem.JSClassName: string;
begin
  Result := 'Ext.menu.CheckItem';
end;

function TExtMenuCheckItem.SetChecked(const AChecked: Boolean; const ASuppressEvent: Boolean): TExtExpression;
begin
  FChecked := AChecked;
  Result := CallMethod('setChecked')
    .AddParam(AChecked)
    .AddParam(ASuppressEvent)
    .AsExpression;
end;

procedure TExtMenuMenu.SetOnClick(const AValue: TExtMenuMenuOnClick);
begin
  RemoveAllListeners('click');
  if Assigned(AValue) then
    //On('click', Ajax('click', ['This', '%0.nm', 'MenuItem', '%1.nm', 'E', '%2.nm'], true));
    &On('click', AjaxCallMethod('click')
      .Event
      .AddRawParam('This', 'sender.nm')
      .AddRawParam('MenuItem', 'item.nm')
      .AddRawParam('E', 'e.nm')
      .FunctionArgs('sender, item, e')
      .AsFunction);
  FOnClick := AValue;
end;

procedure TExtMenuMenu.SetOnItemclick(const AValue: TExtMenuMenuOnItemclick);
begin
  RemoveAllListeners('itemclick');
  if Assigned(AValue) then
    //On('itemclick', Ajax('itemclick', ['BaseItem', '%0.nm', 'E', '%1.nm'], true));
    &On('itemclick', AjaxCallMethod('itemclick')
      .Event
      .AddRawParam('BaseItem', 'item.nm')
      .AddRawParam('E', 'e.nm')
      .FunctionArgs('item, e')
      .AsFunction);
  FOnItemclick := AValue;
end;

class function TExtMenuMenu.JSClassName: string;
begin
  Result := 'Ext.menu.Menu';
end;

procedure TExtMenuMenu.HandleEvent(const AEvtName: string);
begin
  inherited;
  if (AEvtName = 'click') and Assigned(FOnClick) then
    FOnClick(TExtMenuMenu(ParamAsObject('This')), TExtMenuItem(ParamAsObject('MenuItem')), nil)
  else if (AEvtName = 'itemclick') and Assigned(FOnItemclick) then
    FOnItemclick(TExtMenuBaseItem(ParamAsObject('BaseItem')), nil);
end;

class function TExtMenuColorMenu.JSClassName: string;
begin
  Result := 'Ext.menu.ColorMenu';
end;

class function TExtMenuDateMenu.JSClassName: string;
begin
  Result := 'Ext.menu.DateMenu';
end;

end.

