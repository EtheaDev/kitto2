unit Ext.Base;

interface

uses
  Classes
  , SysUtils
  , StrUtils
  , Generics.Collections
  , Kitto.JS.Base
  , Kitto.JS
  , Kitto.JS.Types
  ;

type
  TExtObject = TJSObject;
  TExtObjectClass = TJSObjectClass;
  TExtExpression = TJSExpression;

  TExtMessageType = (emtInfo, emtWarning, emtError);

  TExtBase = class(TExtObject)
  public
    class function JSClassName: string; override;
  end;

  TExtEvented = class(TExtBase)
  public
    class function JSClassName: string; override;
    function &On(const AEventName: string; const AHandler: TExtExpression;
      const AScope: TExtObject = nil; const AOptions: TExtObject = nil): TExtExpression;
    function FireEvent(const AEventName: string;
      const AArgs: TArray<TExtObject>): TExtExpression;
    function RemoveAllListeners(const AEventName: string): TExtExpression;
  end;

  TExtWidget = class(TExtEvented)
  private
    FCls: string;
    procedure SetCls(const AValue: string);
  public
    class function JSClassName: string; override;

    property Cls: string read FCls write SetCls;
    procedure ToFront;
  end;

  TExtEventObjectSingleton = class;
  TExtTemplate = class;
  TExtElement = class;
  TExtAction = class;
  TExtSplitBar = class;
  TExtComponent = class;
  TExtLayer = class;
  TExtXTemplate = class;
  TExtEditor = class;
  TExtColorPalette = class;
  TExtDatePicker = class;
  TExtBoxComponent = class;
  TExtToolbarItem = class;
  TExtProgressBar = class;
  TExtSpacer = class;
  TExtContainer = class;
  TExtButton = class;
  TExtViewView = class;
  TExtViewport = class;
  TExtPanel = class;
  TExtSplitButton = class;
  TExtToolbarSpacer = class;
  TExtToolbarTextItem = class;
  TExtToolbar = class;
  TExtToolbarSeparator = class;
  TExtTip = class;
  TExtToolbarFill = class;
  TExtButtonGroup = class;
  TExtCycleButton = class;
  TExtWindow = class;
  TExtTabPanel = class;
  TExtPagingToolbar = class;
  TExtToolTip = class;
  TExtQuickTip = class;
  TExtMessageBoxSingleton = class;

  TExtFormField = TExtBoxComponent;
  TExtMenuCheckItem = TExtComponent;
  TExtMenuMenu = TExtContainer;
  TExtDataRecord = TExtObject;

  TExtDataStore = TExtBase;

  TExtEventObjectSingleton = class(TExtObject)
  end;

  TExtTemplate = class(TExtObject)
  public
    class function JSClassName: string; override;
  end;

  TExtElement = class(TExtObject)
  public
    class function JSClassName: string; override;
    function AddClass(const AClassName: string): TExtExpression;
  end;

  TExtAction = class(TExtObject)
  private
    FHandler: TExtExpression;
    FHidden: Boolean;
    FText: string;
    procedure _SetText(const AValue: string);
  public
    class function JSClassName: string; override;
    function SetHandler(const AFn: TExtExpression; const AScope: TExtObject): TExtExpression;
    function SetHidden(const AHidden: Boolean): TExtExpression;
    function SetText(const AText: string): TExtExpression;
    property Text: string read FText write _SetText;
  end;

  TExtDirectTransaction = class(TExtObject)
  public
    class function JSClassName: string; override;
  end;

  TExtSplitBar = class(TExtBase)
  public
    class function JSClassName: string; override;
  end;

  // Procedural types for events TExtComponent
  TExtComponentAfterRender = reference to procedure(const AThis: TExtComponent);

  TExtComponent = class(TExtWidget)
  private
    FMinSize: Integer;
    FItemId: string;
    FLabelStyle: string;
    FCollapseMode: string;
    FPlugins: TJSObjectArray;
    FLoader: TExtObject;
    FOverCls: string;
    FHtml: string;
    FMaxWidth: Integer;
    FLabelSeparator: string;
    FPadding: string;
    FHidden: Boolean;
    FId: string;
    FMinWidth: Integer;
    FSplit: Boolean;
    FFieldLabel: string;
    FAfterRenderHandlers: TList<TExtComponentAfterRender>;
    FMaxSize: Integer;
    FItemCls: string;
    FDisabled: Boolean;
    FStyle: string;
    FTpl: string;
    FFlex: Integer;
    FRenderTo: string;
    FRenderToExpression: TExtExpression;
    FFloating: Boolean;
    FDraggable: Boolean;
    FPluginsString: string;
    FBodyPadding: string;
    procedure _SetDisabled(const AValue: Boolean);
    procedure SetFieldLabel(const AValue: string);
    procedure SetHidden(const AValue: Boolean);
    procedure SetHtml(const AValue: string);
    procedure SetId(const AValue: string);
    procedure SetItemCls(const AValue: string);
    procedure SetItemId(const AValue: string);
    procedure SetLabelSeparator(const AValue: string);
    procedure SetLabelStyle(const AValue: string);
    procedure SetOverCls(const AValue: string);
    procedure SetStyle(const AValue: string);
    procedure SetTpl(const AValue: string);
    procedure SetSplit(const AValue: Boolean);
    procedure SetCollapseMode(const AValue: string);
    procedure SetMinWidth(const AValue: Integer);
    procedure SetMaxWidth(const AValue: Integer);
    procedure SetMinSize(const AValue: Integer);
    procedure SetMaxSize(const AValue: Integer);
    procedure SetPadding(const AValue: string);
    procedure SetAfterRender(const AValue: TExtComponentAfterRender);
    function GetLoader: TExtObject;
    function GetPlugins: TJSObjectArray;
    procedure SetFlex(const AValue: Integer);
    procedure SetRenderTo(const AValue: string);
    procedure SetFloating(const AValue: Boolean);
    procedure SetDraggable(const AValue: Boolean);
    procedure SetRenderToExpression(const AValue: TExtExpression);
    procedure SetPluginsString(const AValue: string);
    procedure SetBodyPadding(const AValue: string);
    function GetAfterRender: TExtComponentAfterRender;
  protected
    procedure InitDefaults; override;
  public
    procedure DoHandleEvent(const AEventName: string); override;
    class function JSClassName: string; override;
    function AddCls(const AClsName: string): TExtExpression;
    function Disable: TExtExpression;
    function Enable: TExtExpression;
    function Focus(const ASelectText: Boolean = False; const ADelay: Boolean = False): TExtExpression; overload;
    function Focus(const ASelectText: Boolean; const ADelay: Integer): TExtExpression; overload;
    function Hide: TExtExpression;
    function SetDisabled(const AValue: Boolean): TExtExpression;
    function SetVisible(const AValue: Boolean): TExtExpression;
    function SetWidth(const AValue: Integer): TExtExpression;
    function Show(const AAnimateTarget: string = ''): TExtExpression; overload;
    function Show(const AAnimateTarget: TExtExpression): TExtExpression; overload;
    // Kitto specific.
    function ShowFloating(const AIsModal: Boolean): TExtExpression;
    // Kitto specific.
    function UpdateHostWindowTitle(const ATitle: string): TExtExpression;
    destructor Destroy; override;
    property BodyPadding: string read FBodyPadding write SetBodyPadding;
    property CollapseMode: string read FCollapseMode write SetCollapseMode;
    property Disabled: Boolean read FDisabled write _SetDisabled;
    property Draggable: Boolean read FDraggable write SetDraggable;
    property FieldLabel: string read FFieldLabel write SetFieldLabel;
    property Flex: Integer read FFlex write SetFlex;
    property Floating: Boolean read FFloating write SetFloating;
    property Hidden: Boolean read FHidden write SetHidden;
    property Html: string read FHtml write SetHtml;
    property Id: string read FId write SetId;
    property ItemCls: string read FItemCls write SetItemCls;
    property ItemId: string read FItemId write SetItemId;
    property LabelSeparator: string read FLabelSeparator write SetLabelSeparator;
    property LabelStyle: string read FLabelStyle write SetLabelStyle;
    property Loader: TExtObject read GetLoader;
    property MinWidth: Integer read FMinWidth write SetMinWidth;
    property MaxWidth: Integer read FMaxWidth write SetMaxWidth;
    property MinSize: Integer read FMinSize write SetMinSize;
    property MaxSize: Integer read FMaxSize write SetMaxSize;
    property OverCls: string read FOverCls write SetOverCls;
    property Padding: string read FPadding write SetPadding;
    property Plugins: TJSObjectArray read GetPlugins;
    property PluginsString: string read FPluginsString write SetPluginsString;
    property RenderTo: string read FRenderTo write SetRenderTo;
    property RenderToExpression: TExtExpression read FRenderToExpression write SetRenderToExpression;
    property Style: string read FStyle write SetStyle;
    property Split: Boolean read FSplit write SetSplit;
    property Tpl: string read FTpl write SetTpl;
    property AfterRender: TExtComponentAfterRender read GetAfterRender write SetAfterRender;
    procedure AddAfterRenderHandler(const AHandler: TExtComponentAfterRender);
  end;

  TExtLayer = class(TExtElement)
  private
    FZindex: Integer; // 11000
    procedure _SetZindex(const AValue: Integer);
  protected
    procedure InitDefaults; override;
  public
    class function JSClassName: string; override;
    function SetZIndex(const AZindex: Integer): TExtExpression;
    property Zindex: Integer read FZindex write _SetZindex;
  end;

  TExtXTemplate = class(TExtTemplate)
  public
    class function JSClassName: string; override;
  end;

  TExtEditor = class(TExtComponent)
  private
    FValue: string;
    procedure _SetValue(const AValue: string);
  public
    class function JSClassName: string; override;
    function SetValue(const AValue: string): TExtExpression;
    property Value: string read FValue write _SetValue;
  end;

  TExtColorPalette = class(TExtComponent)
  public
    class function JSClassName: string; override;
  end;

  TExtDatePicker = class(TExtComponent)
  private
    FMaxDate: TDateTime;
    FMinDate: TDateTime;
    procedure _SetMaxDate(const AValue: TDateTime);
    procedure _SetMinDate(const AValue: TDateTime);
  public
    class function JSClassName: string; override;
    function SetMaxDate(const AValue: TDateTime): TExtExpression;
    function SetMinDate(const AValue: TDateTime): TExtExpression;
    property MaxDate: TDateTime read FMaxDate write _SetMaxDate;
    property MinDate: TDateTime read FMinDate write _SetMinDate;
  end;

  TExtBoxComponent = class(TExtComponent)
  private
    FAutoHeight: Boolean;
    FWidthString: string;
    FWidth: Integer;
    FAutoScroll: Boolean;
    FHeightFunc: TExtExpression;
    FMargins: string;
    FAnchor: string;
    FAutoWidth: Boolean;
    FHeightString: string;
    FRegion: string;
    FHeight: Integer;
    FWidthExpression: TExtExpression;
    FOwnerCt: TExtContainer;
    FResizable: Boolean;
    procedure SetAnchor(const AValue: string);
    procedure SetAutoHeight(const AValue: Boolean);
    procedure _SetAutoScroll(const AValue: Boolean);
    procedure SetAutoWidth(const AValue: Boolean);
    procedure SetHeight(const AValue: Integer);
    procedure SetMargins(AValue: string);
    procedure SetRegion(const AValue: string);
    procedure SetWidth(const AValue: Integer);
    procedure SetHeightString(const AValue: string);
    procedure SetWidthString(const AValue: string);
    procedure SetWidthExpression(const AValue: TExtExpression);
    procedure SetHeightFunc(const AValue: TExtExpression);
    procedure SetOwnerCt(const AValue: TExtContainer);
    procedure SetResizable(const AValue: Boolean);
  public
    class function JSClassName: string; override;
    property Anchor: string read FAnchor write SetAnchor;
    property AutoHeight: Boolean read FAutoHeight write SetAutoHeight;
    property AutoScroll: Boolean read FAutoScroll write _SetAutoScroll;
    property AutoWidth: Boolean read FAutoWidth write SetAutoWidth;
    property Height: Integer read FHeight write SetHeight;
    property HeightString: string read FHeightString write SetHeightString;
    property HeightFunc: TExtExpression read FHeightFunc write SetHeightFunc;
    property Margins: string read FMargins write SetMargins;
    property OwnerCt: TExtContainer read FOwnerCt write SetOwnerCt;
    property Region: string read FRegion write SetRegion;
    property Resizable: Boolean read FResizable write SetResizable;
    property Width: Integer read FWidth write SetWidth;
    property WidthString: string read FWidthString write SetWidthString;
    property WidthExpression: TExtExpression read FWidthExpression write SetWidthExpression;
  end;

  TExtToolbarItem = class(TExtBoxComponent)
  public
    class function JSClassName: string; override;
  end;

  TExtProgressBar = class(TExtBoxComponent)
  public
    class function JSClassName: string; override;
  end;

  TExtSpacer = class(TExtBoxComponent)
  protected
    procedure InitDefaults; override;
  public
    class function JSClassName: string; override;
  end;

  TExtContainerLabelAlign = (laLeft, laRight, laTop);

  TExtContainer = class(TExtBoxComponent, IJSContainer)
  private
    FActiveItem: string;
    FActiveItemNumber: Integer;
    FAutoDestroy: Boolean; // true
    FDefaults: TExtObject;
    FItems: TJSObjectArray;
    FLayoutConfig: TExtObject;
    FLayout: string;
    FColumnWidth: Double;
    FLabelWidth: Integer;
    FLabelAlign: TExtContainerLabelAlign;
    FHideLabels: Boolean;
    procedure SetActiveItem(const AValue: string);
    procedure SetActiveItemNumber(const AValue: Integer);
    procedure SetColumnWidth(const AValue: Double);
    procedure SetLabelWidth(const AValue: Integer);
    procedure SetLabelAlign(const AValue: TExtContainerLabelAlign);
    procedure SetLayout(const AValue: string);
    procedure SetHideLabels(const AValue: Boolean);
    function GetDefaults: TExtObject;
    function GetLayoutConfig: TExtObject;
  protected
    procedure InitDefaults; override;
    procedure RemoveChild(const AChild: TJSBase); override;
  public
    class function JSClassName: string; override;
    function UpdateLayout(const AShallow: Boolean; const AForce: Boolean): TExtExpression; overload;
    function UpdateLayout: TExtExpression; overload;
    function Remove(const AComponent: TExtComponent; const AAutoDestroy: Boolean = False): TExtExpression;
    property ActiveItem: string read FActiveItem write SetActiveItem;
    property ActiveItemNumber: Integer read FActiveItemNumber write SetActiveItemNumber;
    property Defaults: TExtObject read GetDefaults;
    property HideLabels: Boolean read FHideLabels write SetHideLabels;
    property Items: TJSObjectArray read FItems;
    property LabelAlign: TExtContainerLabelAlign read FLabelAlign write SetLabelAlign;
    property LabelWidth: Integer read FLabelWidth write SetLabelWidth;
    property LayoutConfig: TExtObject read GetLayoutConfig;
    property Layout: string read FLayout write SetLayout;
    property ColumnWidth: Double read FColumnWidth write SetColumnWidth;
    // IJSContainer
    function AsJSObject: TJSObject;
    procedure AddItem(const AItem: TJSObject); overload; virtual;
    // Calls the specified proc once for each of the Items.
    procedure Apply(const AProc: TProc<TExtObject>);
  end;

  TExtButton = class(TExtBoxComponent)
  private
    FAllowDepress: Boolean;
    FDisabled: Boolean;
    FEnableToggle: Boolean;
    FFormBind: Boolean;
    FHandleMouseEvents: Boolean; // true
    FHandler: TExtExpression;
    FHidden: Boolean;
    FIcon: string;
    FIconCls: string;
    FMenu: TExtBase;
    FMinWidth: Integer;
    FPressed: Boolean;
    FScale: string;
    FTemplate: TExtTemplate;
    FText: string;
    FToggleGroup: string;
    FTooltip: string;
    FBtnEl: TExtElement;
    FDisabledFunc: TExtExpression;
    procedure SetAllowDepress(const AValue: Boolean);
    procedure SetDisabled(const AValue: Boolean);
    procedure SetEnableToggle(const AValue: Boolean);
    procedure SetFormBind(const AValue: Boolean);
    procedure _SetHandler(const AValue: TExtExpression);
    procedure SetHidden(const AValue: Boolean);
    procedure _SetIcon(const AValue: string);
    procedure SetIconCls(const AValue: string);
    procedure SetMenu(AValue: TExtBase);
    procedure SetPressed(const AValue: Boolean);
    procedure SetScale(const AValue: string);
    procedure _SetText(const AValue: string);
    procedure SetToggleGroup(const AValue: string);
    procedure _SetTooltip(const AValue: string);
    procedure SetMinWidth(const AValue: Integer);
    procedure SetDisabledFunc(const AValue: TExtExpression);
  protected
    procedure InitDefaults; override;
    function GetObjectNamePrefix: string; override;
  public
    class function JSClassName: string; override;
    function GetPressed(const AGroup: string): TExtExpression;
    function Pressed_: TExtExpression;
    function GetTemplateArgs: TExtExpression;
    function GetText: TExtExpression;
    function HasVisibleMenu: TExtExpression;
    function HideMenu: TExtExpression;
    function PerformClick: TExtExpression;
    function SetHandler(const AHandler: TExtExpression; const AScope: TExtObject = nil): TExtExpression;
    function SetText(const AText: string): TExtExpression;
    function SetTooltip(const ATooltip: string): TExtExpression; overload;
    function SetTooltip(const ATooltip: TExtObject): TExtExpression; overload;
    property AllowDepress: Boolean read FAllowDepress write SetAllowDepress;
    property Disabled: Boolean read FDisabled write SetDisabled;
    property DisabledFunc: TExtExpression read FDisabledFunc write SetDisabledFunc;
    property EnableToggle: Boolean read FEnableToggle write SetEnableToggle;
    property FormBind: Boolean read FFormBind write SetFormBind;
    property Handler: TExtExpression read FHandler write _SetHandler;
    property Hidden: Boolean read FHidden write SetHidden;
    property Icon: string read FIcon write _SetIcon;
    property IconCls: string read FIconCls write SetIconCls;
    property Menu: TExtBase read FMenu write SetMenu;
    property MinWidth: Integer read FMinWidth write SetMinWidth;
    property Pressed: Boolean read FPressed write SetPressed;
    property Scale: string read FScale write SetScale;
    property Text: string read FText write _SetText;
    property ToggleGroup: string read FToggleGroup write SetToggleGroup;
    property Tooltip: string read FTooltip write _SetTooltip;
  end;

  TExtViewView = class(TExtBoxComponent)
  private
    FEmptyText: string;
    FItemSelector: string;
    FMultiSelect: Boolean;
    FOverItemCls: string;
    FSelectedClass: string; // 'x-view-selected'
    FSimpleSelect: Boolean;
    FSingleSelect: Boolean;
    FStore: TExtDataStore;
    FTpl: string;
    FTplArray: TJSObjectArray;
    FTrackOver: Boolean;
    procedure SetEmptyText(AValue: string);
    procedure SetItemSelector(const AValue: string);
    procedure SetMultiSelect(const AValue: Boolean);
    procedure SetOverItemCls(const AValue: string);
    procedure SetSimpleSelect(const AValue: Boolean);
    procedure SetSingleSelect(const AValue: Boolean);
    procedure _SetStore(const AValue: TExtDataStore);
    procedure SetTpl(AValue: string);
    procedure SetSelectedClass(const AValue: string);
    procedure SetTrackOver(const AValue: Boolean);
  strict protected
    function GetObjectNamePrefix: string; override;
  protected
    procedure InitDefaults; override;
  public
    class function JSClassName: string; override;
    function SetStore(const AStore: TExtDataStore): TExtExpression;
    property EmptyText: string read FEmptyText write SetEmptyText;
    property ItemSelector: string read FItemSelector write SetItemSelector;
    property MultiSelect: Boolean read FMultiSelect write SetMultiSelect;
    property OverItemCls: string read FOverItemCls write SetOverItemCls;
    property SelectedClass: string read FSelectedClass write SetSelectedClass;
    property SimpleSelect: Boolean read FSimpleSelect write SetSimpleSelect;
    property SingleSelect: Boolean read FSingleSelect write SetSingleSelect;
    property Store: TExtDataStore read FStore write _SetStore;
    property Tpl: string read FTpl write SetTpl;
    property TrackOver: Boolean read FTrackOver write SetTrackOver;
  end;

  TExtViewport = class(TExtContainer)
  protected
    function GetObjectNamePrefix: string; override;
  public
    class function JSClassName: string; override;
  end;

  TExtPanel = class(TExtContainer)
  private
    FFrame: Boolean;
    FBodyStyle: string;
    FTbar: TExtObject;
    FAutoLoadString: string;
    FAutoLoad: TExtObject;
    FHeader: Boolean;
    FCollapsed: Boolean;
    FCollapsible: Boolean;
    FClosable: Boolean;
    FTitle: string;
    FBbar: TExtObject;
    FFbar: TExtObject;
    FBorder: Boolean;
    FAnimCollapse: Boolean;
    FFooter: Boolean;
    FIconCls: string;
    FAutoLoadBoolean: Boolean;
    FMinButtonWidth: Integer;
    procedure SetAnimCollapse(const AValue: Boolean);
    procedure SetAutoLoadString(const AValue: string);
    procedure SetAutoLoadBoolean(const AValue: Boolean);
    procedure SetBbar(const AValue: TExtObject);
    procedure SetBodyStyle(const AValue: string);
    procedure SetBorder(const AValue: Boolean);
    procedure SetClosable(const AValue: Boolean);
    procedure SetCollapsible(const AValue: Boolean);
    procedure SetFooter(const AValue: Boolean);
    procedure SetFrame(const AValue: Boolean);
    procedure SetHeader(const AValue: Boolean);
    procedure SetIconCls(const AValue: string);
    procedure _SetTitle(AValue: string);
    procedure SetCollapsed(const AValue: Boolean);
    procedure SetTbar(const AValue: TExtObject);
    procedure SetMinButtonWidth(const AValue: Integer);
    function GetAutoLoad: TExtObject;
    procedure SetFbar(const AValue: TExtObject);
  protected
    procedure InitDefaults; override;
    function GetObjectNamePrefix: string; override;
  public
    class function JSClassName: string; override;
    class function JSXType: string; override;
    function Close: TExtExpression;
    function Collapse(const AAnimate: Boolean): TExtExpression;
    function Expand(const AAnimate: Boolean): TExtExpression;
    function SetTitle(const ATitle: string; const AIconCls: string = ''): TExtExpression;
    property AnimCollapse: Boolean read FAnimCollapse write SetAnimCollapse;
    property AutoLoad: TExtObject read GetAutoLoad;
    property AutoLoadString: string read FAutoLoadString write SetAutoLoadString;
    property AutoLoadBoolean: Boolean read FAutoLoadBoolean write SetAutoLoadBoolean;
    property Bbar: TExtObject read FBbar write SetBbar;
    property BodyStyle: string read FBodyStyle write SetBodyStyle;
    property Border: Boolean read FBorder write SetBorder;
    property Closable: Boolean read FClosable write SetClosable;
    property Collapsible: Boolean read FCollapsible write SetCollapsible;
    property Collapsed: Boolean read FCollapsed write SetCollapsed;
    property Fbar: TExtObject read FFbar write SetFbar;
    property Footer: Boolean read FFooter write SetFooter;
    property Frame: Boolean read FFrame write SetFrame;
    property Header: Boolean read FHeader write SetHeader;
    property IconCls: string read FIconCls write SetIconCls;
    property MinButtonWidth: Integer read FMinButtonWidth write SetMinButtonWidth;
    property Tbar: TExtObject read FTbar write SetTbar;
    property Title: string read FTitle write _SetTitle;
  end;

  TExtSplitButton = class(TExtButton)
  private
    FArrowHandler: TExtExpression;
    procedure _SetArrowHandler(const AValue: TExtExpression);
  public
    class function JSClassName: string; override;
    function SetArrowHandler(const AHandler: TExtExpression; const AScope: TExtObject = nil): TExtExpression;
    property ArrowHandler: TExtExpression read FArrowHandler write _SetArrowHandler;
  end;

  TExtToolbarSpacer = class(TExtToolbarItem)
  protected
    function GetObjectNamePrefix: string; override;
  public
    class function JSClassName: string; override;
  end;

  TExtToolbarTextItem = class(TExtToolbarItem)
  private
    FText: string;
    procedure _SetText(const AValue: string);
  protected
    procedure InitDefaults; override;
  public
    class function JSClassName: string; override;
    function SetText(const AText: string): TExtExpression;
    property Text: string read FText write _SetText;
  end;

  TExtToolbar = class(TExtContainer)
  protected
    function GetObjectNamePrefix: string; override;
  public
    class function JSClassName: string; override;
    class function JSXType: string; override;
  end;

  TExtToolbarSeparator = class(TExtToolbarItem)
  protected
    procedure InitDefaults; override;
  public
    class function JSClassName: string; override;
  end;

  TExtTip = class(TExtPanel)
  public
    class function JSClassName: string; override;
  end;

  TExtToolbarFill = class(TExtToolbarSpacer)
  protected
    function GetObjectNamePrefix: string; override;
  public
    class function JSClassName: string; override;
    class function JSXType: string; override;
  end;

  TExtButtonGroup = class(TExtPanel)
  public
    class function JSClassName: string; override;
  end;

  TExtCycleButton = class(TExtSplitButton)
  public
    class function JSClassName: string; override;
  end;

  TExtWindow = class(TExtPanel)
  private
    FAnimateTarget: string;
    FAnimateTargetElement: TExtElement;
    FBaseCls: string; // 'x-window'
    FClosable: Boolean; // true
    FConstrain: Boolean;
    FExpandOnShow: Boolean; // true
    FInitHidden: Boolean; // true
    FMaximizable: Boolean;
    FMaximized: Boolean;
    FMinHeight: Integer; // 100
    FMinWidth: Integer; // 200
    FPlain: Boolean;
    FResizeHandles: string; // 'all'
    procedure _SetAnimateTarget(const AValue: string);
    procedure SetClosable(const AValue: Boolean);
    procedure SetConstrain(const AValue: Boolean);
    procedure SetMaximizable(const AValue: Boolean);
    procedure SetMaximized(const AValue: Boolean);
    procedure SetPlain(const AValue: Boolean);
    procedure SetResizeHandles(const AValue: string);
  protected
    procedure InitDefaults; override;
    function GetObjectNamePrefix: string; override;
  public
    class function JSClassName: string; override;
    function Close: TExtExpression;
    procedure SetAnimateTarget(const AElement: string);
    function Show(const AAnimateTarget: string = ''; const ACallback: TExtExpression = nil;
      const AScope: TExtObject = nil): TExtExpression; overload;
    function Show(const AAnimateTarget: TExtElement; const ACallback: TExtExpression = nil;
      const AScope: TExtObject = nil): TExtExpression; overload;
    property AnimateTarget: string read FAnimateTarget write _SetAnimateTarget;
    property Closable: Boolean read FClosable write SetClosable;
    property Constrain: Boolean read FConstrain write SetConstrain;
    property Maximizable: Boolean read FMaximizable write SetMaximizable;
    property Maximized: Boolean read FMaximized write SetMaximized;
    property Plain: Boolean read FPlain write SetPlain;
    property ResizeHandles: string read FResizeHandles write SetResizeHandles;
  end;

  // Procedural types for events TExtTabPanel
  TExtTabPanelOnTabChange = procedure(ATabPanel: TExtTabPanel; ANewTab: TExtComponent) of object;

  TExtTabPanel = class(TExtPanel)
  private
    FOnTabChange: TExtTabPanelOnTabChange;
    FLayoutOnTabChange: Boolean;
    FActiveTab: string;
    FDeferredRender: Boolean;
    FActiveTabNumber: Integer;
    procedure _SetActiveTab(const AValue: string);
    procedure SetActiveTabNumber(const AValue: Integer);
    procedure SetDeferredRender(const AValue: Boolean);
    procedure SetLayoutOnTabChange(const AValue: Boolean);
    procedure SetOnTabChange(const AValue: TExtTabPanelOnTabchange);
  protected
    function GetObjectNamePrefix: string; override;
  public
    procedure DoHandleEvent(const AEvtName: string); override;
    class function JSClassName: string; override;
    function GetActiveTab: TExtExpression;
    function SetActiveTab(const AItem: string): TExtExpression; overload;
    function SetActiveTab(const AItem: Integer): TExtExpression; overload;
    function SetActiveTab(const AItem: TExtComponent): TExtExpression; overload;
    property ActiveTab: string read FActiveTab write _SetActiveTab;
    property ActiveTabNumber: Integer read FActiveTabNumber write SetActiveTabNumber;
    property DeferredRender: Boolean read FDeferredRender write SetDeferredRender;
    property LayoutOnTabChange: Boolean read FLayoutOnTabChange
      write SetLayoutOnTabChange;
    property OnTabchange: TExtTabPanelOnTabchange read FOnTabChange write SetOnTabChange;
  end;

  TExtPagingToolbar = class(TExtToolbar)
  private
    FPageSize: Integer;
    FDisplayInfo: Boolean;
    FStore: TExtDataStore;
    procedure SetDisplayInfo(const AValue: Boolean);
    procedure SetPageSize(const AValue: Integer);
    procedure SetStore(const AValue: TExtDataStore);
  public
    class function JSClassName: string; override;
    function MoveFirst: TExtExpression;
    function MoveLast: TExtExpression;
    function MoveNext: TExtExpression;
    function MovePrevious: TExtExpression;
    property DisplayInfo: Boolean read FDisplayInfo write SetDisplayInfo;
    property PageSize: Integer read FPageSize write SetPageSize;
    property Store: TExtDataStore read FStore write SetStore;
  end;

  TExtToolTip = class(TExtTip)
  private
    FTRackMouse: Boolean;
    procedure SetTrackMouse(const AValue: Boolean);
  public
    class function JSClassName: string; override;
    property TrackMouse: Boolean read FTRackMouse write SetTrackMouse;
  end;

  TExtQuickTip = class(TExtToolTip)
  public
    class function JSClassName: string; override;
  end;

  TExtPluginAbstract = class(TExtObject)
  end;

  TExtMessageBoxSingleton = class(TExtObject)
  public
    class function JSClassName: string; override;
    function Alert(const ATitle: string; const AMsg: string;
      const AFn: TExtExpression = nil; const AScope: TExtObject = nil): TExtExpression;
    function ShowMessage(const AMessageType: TExtMessageType;
      const ATitle: string; const AMsg: string;
      const AFn: TExtExpression = nil; const AScope: TExtObject = nil): TExtExpression;
  end;

function ExtMessageBox: TExtMessageBoxSingleton;
function LabelAlignAsOption(const AValue: TExtContainerLabelAlign): string;

implementation

uses
  Kitto.JS.Formatting
  , Kitto.Web.Response
  , Kitto.Web.Application
  , Kitto.Web.Session
  ;

function ExtMessageBox: TExtMessageBoxSingleton;
begin
  if TKWebSession.Current <> nil then
    Result := TKWebSession.Current.ObjectSpace.GetSingleton<TExtMessageBoxSingleton>(TExtMessageBoxSingleton.JSClassName)
  else
    Result := nil;
end;

function LabelAlignAsOption(const AValue: TExtContainerLabelAlign): string;
begin
  case AValue of
    laLeft : Result := 'left';
    laTop : Result := 'top';
    laRight : Result := 'right';
  end;
end;

class function TExtBase.JSClassName: string;
begin
  Result := 'Ext.Base';
end;

class function TExtTemplate.JSClassName: string;
begin
  Result := 'Ext.Template';
end;

class function TExtElement.JSClassName: string;
begin
  Result := 'Ext.Element';
end;

function TExtElement.AddClass(const AClassName: string): TExtExpression;
begin
  Result := TKWebResponse.Current.Items.CallMethod(Self, 'addClass').AddParam(AClassName).AsExpression;
end;

procedure TExtAction._SetText(const AValue: string);
begin
  FText := SetConfigItem('text', 'setText', AValue);
end;

class function TExtAction.JSClassName: string;
begin
  Result := 'Ext.Action';
end;

function TExtAction.SetHandler(const AFn: TExtExpression; const AScope: TExtObject): TExtExpression;
begin
  FHandler := AFn;
  Result := TKWebResponse.Current.Items.CallMethod(Self, 'setHandler')
    .AddParam(AFn)
    .AddParam(AScope)
    .AsExpression;
end;

function TExtAction.SetHidden(const AHidden: Boolean): TExtExpression;
begin
  FHidden := AHidden;
  Result := TKWebResponse.Current.Items.CallMethod(Self, 'setHidden').AddParam(AHidden).AsExpression;
end;

function TExtAction.SetText(const AText: string): TExtExpression;
begin
  FText := AText;
  Result := TKWebResponse.Current.Items.CallMethod(Self, 'setText').AddParam(AText).AsExpression;
end;

class function TExtDirectTransaction.JSClassName: string;
begin
  Result := 'Ext.Direct.Transaction';
end;

class function TExtSplitBar.JSClassName: string;
begin
  Result := 'Ext.SplitBar';
end;

procedure TExtComponent._SetDisabled(const AValue: Boolean);
begin
  FDisabled := SetConfigItem('disabled', 'setDisabled', AValue);
end;

procedure TExtComponent.SetFieldLabel(const AValue: string);
begin
  FFieldLabel := SetConfigItem('fieldLabel', AValue);
end;

procedure TExtComponent.SetFlex(const AValue: Integer);
begin
  FFlex := SetConfigItem('flex', AValue);
end;

procedure TExtComponent.SetFloating(const AValue: Boolean);
begin
  FFloating := SetConfigItem('floating', AValue);
end;

procedure TExtComponent.SetHidden(const AValue: Boolean);
begin
  FHidden := SetConfigItem('hidden', 'setHidden', AValue);
end;

procedure TExtComponent.SetHtml(const AValue: string);
begin
  FHtml := SetConfigItem('html', 'update', AValue);
end;

procedure TExtComponent.SetId(const AValue: string);
begin
  FId := SetConfigItem('id', AValue);
end;

procedure TExtComponent.SetItemCls(const AValue: string);
begin
  FItemCls := SetConfigItem('itemCls', AValue);
end;

procedure TExtComponent.SetItemId(const AValue: string);
begin
  FItemId := SetConfigItem('itemId', AValue);
end;

procedure TExtComponent.SetLabelSeparator(const AValue: string);
begin
  FLabelSeparator := SetConfigItem('labelSeparator', AValue);
end;

procedure TExtComponent.SetLabelStyle(const AValue: string);
begin
  FLabelStyle := SetConfigItem('labelStyle', AValue);
end;

procedure TExtComponent.SetOverCls(const AValue: string);
begin
  FOverCls := SetConfigItem('overCls', AValue);
end;

procedure TExtComponent.SetPadding(const AValue: string);
begin
  FPadding := SetConfigItem('padding', AValue);
end;

procedure TExtComponent.SetPluginsString(const AValue: string);
begin
  FPluginsString := SetConfigItem('plugins', AValue);
end;

procedure TExtComponent.SetRenderTo(const AValue: string);
begin
  FRenderTo := SetConfigItem('renderTo', AValue);
end;

procedure TExtComponent.SetRenderToExpression(const AValue: TExtExpression);
begin
  FRenderToExpression := SetConfigItem('renderTo', AValue);
end;

procedure TExtComponent.SetStyle(const AValue: string);
begin
  FStyle := SetConfigItem('style', AValue);
end;

procedure TExtComponent.SetTpl(const AValue: string);
begin
  FTpl := SetConfigItem('tpl', AValue);
end;

procedure TExtComponent.SetSplit(const AValue: Boolean);
begin
  FSplit := AValue;
  SetConfigItem('split', AValue);
end;

procedure TExtComponent.SetCollapseMode(const AValue: string);
begin
  FCollapseMode := AValue;
  SetConfigItem('collapseMode', AValue);
end;

procedure TExtComponent.SetMinWidth(const AValue: Integer);
begin
  FMinWidth := AValue;
  SetConfigItem('minWidth', AValue);
end;

procedure TExtComponent.SetMaxWidth(const AValue: Integer);
begin
  FMaxWidth := SetConfigItem('maxWidth', AValue);
end;

procedure TExtComponent.SetMinSize(const AValue: Integer);
begin
  FMinSize := SetConfigItem('minSize', AValue);
end;

procedure TExtComponent.SetMaxSize(const AValue: Integer);
begin
  FMaxSize := SetConfigItem('maxSize', AValue);
end;

class function TExtComponent.JSClassName: string;
begin
  Result := 'Ext.Component';
end;

procedure TExtComponent.InitDefaults;
begin
  inherited;
  FLabelSeparator := ':';
  FAfterRenderHandlers := TList<TExtComponentAfterRender>.Create;
end;

procedure TExtComponent.AddAfterRenderHandler(const AHandler: TExtComponentAfterRender);
begin
  if Assigned(AHandler) then
  begin
    &On('afterrender', TKWebResponse.Current.Items.AjaxCallMethod(Self, 'afterrender')
      .Event
      .AddRawParam('This', 'sender.nm')
      .FunctionArgs('sender')
      .AsFunction);
    FAfterRenderHandlers.Add(AHandler);
  end;
end;

function TExtComponent.AddCls(const AClsName: string): TExtExpression;
begin
  Result := TKWebResponse.Current.Items.CallMethod(Self, 'addCls').AddParam(AClsName).AsExpression;
end;

function TExtComponent.Focus(const ASelectText: Boolean; const ADelay: Boolean): TExtExpression;
begin
  Result := TKWebResponse.Current.Items.CallMethod(Self, 'focus')
    .AddParam(ASelectText)
    .AddParam(ADelay)
    .AsExpression;
end;

function TExtComponent.Focus(const ASelectText: Boolean; const ADelay: Integer): TExtExpression;
begin
  Result := TKWebResponse.Current.Items.CallMethod(Self, 'focus')
    .AddParam(ASelectText)
    .AddParam(ADelay)
    .AsExpression;
end;

function TExtComponent.GetAfterRender: TExtComponentAfterRender;
begin
  if FAfterRenderHandlers.Count > 0 then
    Result := FAfterRenderHandlers[0]
  else
    Result := nil;
end;

function TExtComponent.GetLoader: TExtObject;
begin
  if not Assigned(FLoader) then
    FLoader := CreateConfigObject('loader');
  Result := FLoader;
end;

function TExtComponent.GetPlugins: TJSObjectArray;
begin
  if not Assigned(FPlugins) then
    FPlugins := CreateConfigObjectArray('plugins');
  Result := FPlugins;
end;

function TExtComponent.Hide: TExtExpression;
begin
  Result := TKWebResponse.Current.Items.CallMethod(Self, 'hide').AsExpression;
end;

function TExtComponent.SetDisabled(const AValue: Boolean): TExtExpression;
begin
  Result := TKWebResponse.Current.Items.CallMethod(Self, 'setDisabled')
    .AddParam(AValue)
    .AsExpression;
end;

procedure TExtComponent.SetDraggable(const AValue: Boolean);
begin
  FDraggable := SetConfigItem('draggable', AValue);
end;

function TExtComponent.SetVisible(const AValue: Boolean): TExtExpression;
begin
  Result := TKWebResponse.Current.Items.CallMethod(Self, 'setVisible')
    .AddParam(AValue)
    .AsExpression;
end;

function TExtComponent.SetWidth(const AValue: Integer): TExtExpression;
begin
  Result := TKWebResponse.Current.Items.CallMethod(Self, 'setWidth')
    .AddParam(AValue)
    .AsExpression;
end;

function TExtComponent.Show(const AAnimateTarget: TExtExpression): TExtExpression;
begin
  Result := TKWebResponse.Current.Items.CallMethod(Self, 'show')
    .AddParam(AAnimateTarget)
    .AsExpression;
end;

function TExtComponent.ShowFloating(const AIsModal: Boolean): TExtExpression;
begin
  Result := TKWebResponse.Current.Items.CallMethod(Self, 'showFloating')
    .AddParam(AIsModal)
    .AsExpression;
end;

function TExtComponent.UpdateHostWindowTitle(const ATitle: string): TExtExpression;
begin
  Result := TKWebResponse.Current.Items.CallMethod(Self, 'updateHostWindowTitle')
    .AddParam(ATitle)
    .AsExpression;
end;

function TExtComponent.Show(const AAnimateTarget: string = ''): TExtExpression;
begin
  Result := TKWebResponse.Current.Items.CallMethod(Self, 'show').AddParam(AAnimateTarget).AsExpression;
end;

procedure TExtComponent.SetAfterRender(const AValue: TExtComponentAfterRender);
begin
  RemoveAllListeners('afterrender');
  FAfterRenderHandlers.Clear;
  AddAfterRenderHandler(AValue);
end;

procedure TExtComponent.SetBodyPadding(const AValue: string);
begin
  FPadding := SetConfigItem('bodyPadding', AValue);
end;

destructor TExtComponent.Destroy;
begin
  FreeAndNil(FAfterRenderHandlers);
  inherited;
end;

function TExtComponent.Disable: TExtExpression;
begin
  Result := TKWebResponse.Current.Items.CallMethod(Self, 'disable').AsExpression;
end;

procedure TExtComponent.DoHandleEvent(const AEventName: string);
var
  LHandler: TExtComponentAfterRender;
begin
  inherited;
  if (AEventName = 'afterrender') and (FAfterRenderHandlers.Count > 0) then
    for LHandler in FAfterRenderHandlers do
      LHandler(TExtComponent(ParamAsObject('This')));
end;

function TExtComponent.Enable: TExtExpression;
begin
  Result := TKWebResponse.Current.Items.CallMethod(Self, 'enable').AsExpression;
end;

procedure TExtLayer._SetZindex(const AValue: Integer);
begin
  FZindex := SetConfigItem('zindex', 'setZindex', AValue);
end;

class function TExtLayer.JSClassName: string;
begin
  Result := 'Ext.Layer';
end;

procedure TExtLayer.InitDefaults;
begin
  inherited;
  FZindex := 11000;
end;

function TExtLayer.SetZIndex(const AZindex: Integer): TExtExpression;
begin
  Result := TKWebResponse.Current.Items.CallMethod(Self, 'setZindex')
    .AddParam(AZindex)
    .AsExpression;
end;

class function TExtXTemplate.JSClassName: string;
begin
  Result := 'Ext.XTemplate';
end;

procedure TExtEditor._SetValue(const AValue: string);
begin
  FValue := AValue;
  SetConfigItem('value', 'setValue', AValue);
end;

class function TExtEditor.JSClassName: string;
begin
  Result := 'Ext.Editor';
end;

function TExtEditor.SetValue(const AValue: string): TExtExpression;
begin
  FValue := AValue;
  Result := TKWebResponse.Current.Items.CallMethod(Self, 'setValue')
    .AddParam(AValue)
    .AsExpression;
end;

class function TExtColorPalette.JSClassName: string;
begin
  Result := 'Ext.ColorPalette';
end;

procedure TExtDatePicker._SetMaxDate(const AValue: TDateTime);
begin
  FMaxDate := AValue;
  SetConfigItem('maxDate', 'setMaxDate', AValue);
end;

procedure TExtDatePicker._SetMinDate(const AValue: TDateTime);
begin
  FMinDate := AValue;
  SetConfigItem('minDate', 'setMinDate', AValue);
end;

class function TExtDatePicker.JSClassName: string;
begin
  Result := 'Ext.DatePicker';
end;

function TExtDatePicker.SetMaxDate(const AValue: TDateTime): TExtExpression;
begin
  FMaxDate := AValue;
  Result := TKWebResponse.Current.Items.CallMethod(Self, 'setMaxDate')
    .AddParam(AValue)
    .AsExpression;
end;

function TExtDatePicker.SetMinDate(const AValue: TDateTime): TExtExpression;
begin
  FMinDate := AValue;
  Result := TKWebResponse.Current.Items.CallMethod(Self, 'setMinDate')
    .AddParam(AValue)
    .AsExpression;
end;

procedure TExtBoxComponent.SetAnchor(const AValue: string);
begin
  FAnchor := SetConfigItem('anchor', AValue);
end;

procedure TExtBoxComponent.SetAutoHeight(const AValue: Boolean);
begin
  FAutoHeight := SetConfigItem('autoHeight', AValue);
end;

procedure TExtBoxComponent._SetAutoScroll(const AValue: Boolean);
begin
  FAutoScroll := SetConfigItem('autoScroll', 'setAutoScroll', AValue);
end;

procedure TExtBoxComponent.SetAutoWidth(const AValue: Boolean);
begin
  FAutoWidth := SetConfigItem('autoWidth', AValue);
end;

procedure TExtBoxComponent.SetHeight(const AValue: Integer);
begin
  FHeight := SetConfigItem('height', 'setHeight', AValue);
end;

procedure TExtBoxComponent.SetHeightFunc(const AValue: TExtExpression);
begin
  FHeightFunc := SetConfigItem('height', AValue);
end;

procedure TExtBoxComponent.SetHeightString(const AValue: string);
begin
  FHeightString := SetConfigItem('height', 'setHeight', AValue);
end;

procedure TExtBoxComponent.SetMargins(AValue: string);
begin
  FMargins := SetConfigItem('margins', AValue);
end;

procedure TExtBoxComponent.SetOwnerCt(const AValue: TExtContainer);
begin
  FOwnerCt := TExtContainer(SetConfigItem('ownerCt', AValue));
end;

procedure TExtBoxComponent.SetRegion(const AValue: string);
begin
  FRegion := AValue;
  SetConfigItem('region', AValue);
end;

procedure TExtBoxComponent.SetResizable(const AValue: Boolean);
begin
  FResizable := SetConfigItem('resizable', 'setResizable', AValue);
end;

procedure TExtBoxComponent.SetWidth(const AValue: Integer);
begin
  FWidth := SetConfigItem('width', 'setWidth', AValue);
end;

procedure TExtBoxComponent.SetWidthExpression(const AValue: TExtExpression);
begin
  FWidthExpression := SetConfigItem('width', AValue);
end;

procedure TExtBoxComponent.SetWidthString(const AValue: string);
begin
  FWidthString := SetConfigItem('width', 'setWidth', AValue);
end;

class function TExtBoxComponent.JSClassName: string;
begin
  Result := 'Ext.Component';
end;

class function TExtToolbarItem.JSClassName: string;
begin
  Result := 'Ext.Toolbar.Item';
end;

class function TExtProgressBar.JSClassName: string;
begin
  Result := 'Ext.ProgressBar';
end;

class function TExtSpacer.JSClassName: string;
begin
  Result := 'Ext.Spacer';
end;

procedure TExtSpacer.InitDefaults;
begin
  inherited;
end;

procedure TExtContainer.SetActiveItem(const AValue: string);
begin
  FActiveItem := SetConfigItem('activeItem', AValue);
end;

procedure TExtContainer.SetActiveItemNumber(const AValue: Integer);
begin
  FActiveItemNumber := SetConfigItem('activeItemNumber', AValue);
end;

procedure TExtContainer.SetLabelAlign(const AValue: TExtContainerLabelAlign);
begin
  FLabelAlign := AValue;
  Defaults.SetConfigItem('labelAlign', LabelAlignAsOption(AValue));
end;

procedure TExtContainer.SetLabelWidth(const AValue: Integer);
begin
  FLabelWidth := AValue;
  Defaults.SetConfigItem('labelWidth', AValue);
end;

procedure TExtContainer.SetLayout(const AValue: string);
begin
  FLayout := SetConfigItem('layout', AValue);
end;

procedure TExtContainer.SetColumnWidth(const AValue: Double);
begin
  FColumnWidth := SetConfigItem('columnWidth', AValue);
end;

procedure TExtContainer.SetHideLabels(const AValue: Boolean);
begin
  FHideLabels := SetConfigItem('hideLabels', AValue);
end;

class function TExtContainer.JSClassName: string;
begin
  Result := 'Ext.Container';
end;

procedure TExtContainer.AddItem(const AItem: TJSObject);
begin
  AddItem(Items, AItem);
end;

function TExtContainer.AsJSObject: TJSObject;
begin
  Result := Self;
end;

function TExtContainer.GetDefaults: TExtObject;
begin
  if not Assigned(FDefaults) then
    FDefaults := CreateConfigObject('defaults');
  Result := FDefaults;
end;

function TExtContainer.GetLayoutConfig: TExtObject;
begin
  if not Assigned(FLayoutConfig) then
    FLayoutConfig := CreateConfigObject('layoutConfig');
  Result := FLayoutConfig;
end;

procedure TExtContainer.InitDefaults;
begin
  inherited;
  FAutoDestroy := True;
  FItems := CreateConfigObjectArray('items');
end;

function TExtContainer.UpdateLayout: TExtExpression;
begin
  Result := UpdateLayout(False, False);
end;

function TExtContainer.UpdateLayout(const AShallow: Boolean; const AForce: Boolean): TExtExpression;
begin
  Result := TKWebResponse.Current.Items.CallMethod(Self, 'updateLayout')
    .AddParam(AShallow)
    .AddParam(AForce)
    .AsExpression;
end;

function TExtContainer.Remove(const AComponent: TExtComponent; const AAutoDestroy: Boolean): TExtExpression;
begin
  Result := TKWebResponse.Current.Items.CallMethod(Self, 'remove')
    .AddParam(AComponent)
    .AddParam(AAutoDestroy)
    .AsExpression;
end;

procedure TExtContainer.RemoveChild(const AChild: TJSBase);
begin
  inherited;
  if AChild is TJSObject then
    Items.Remove(TJSObject(AChild));
end;

procedure TExtContainer.Apply(const AProc: TProc<TExtObject>);
var
  I: Integer;
begin
  Assert(Assigned(AProc));

  for I := 0 to Items.Count - 1 do
  begin
    AProc(Items[I]);
    if Items[I] is TExtContainer then
      TExtContainer(Items[I]).Apply(AProc);
  end;
end;

{ TExtButton }

procedure TExtButton.SetAllowDepress(const AValue: Boolean);
begin
  FAllowDepress := SetConfigItem('allowDepress', AValue);
end;

procedure TExtButton.SetDisabled(const AValue: Boolean);
begin
  FDisabled := SetConfigItem('disabled', 'setDisabled', AValue);
end;

procedure TExtButton.SetDisabledFunc(const AValue: TExtExpression);
begin
  FDisabledFunc := TKWebResponse.Current.Items.CallMethod(Self, 'setDisabled').AddParam(AValue).AsExpression;
end;

procedure TExtButton.SetEnableToggle(const AValue: Boolean);
begin
  FEnableToggle := SetConfigItem('enableToggle', AValue);
end;

procedure TExtButton.SetFormBind(const AValue: Boolean);
begin
  FFormBind := SetConfigItem('formBind', AValue);
end;

procedure TExtButton._SetHandler(const AValue: TExtExpression);
begin
  FHandler := SetConfigItem('handler', 'setHandler', AValue);
end;

procedure TExtButton.SetHidden(const AValue: Boolean);
begin
  FHidden := SetConfigItemOrProperty('hidden', AValue);
end;

procedure TExtButton._SetIcon(const AValue: string);
begin
  FIcon := SetConfigItem('icon', 'setIcon', AValue);
end;

procedure TExtButton.SetIconCls(const AValue: string);
begin
  FIconCls := SetConfigItem('iconCls', AValue);
end;

procedure TExtButton.SetMenu(AValue: TExtBase);
begin
  FMenu.Free;
  FMenu := TExtBase(SetConfigItem('menu', AValue));
end;

procedure TExtButton.SetMinWidth(const AValue: Integer);
begin
  FMinWidth := SetConfigItem('minWidth', AValue);
end;

procedure TExtButton.SetPressed(const AValue: Boolean);
begin
  FPressed := SetConfigItemOrProperty('pressed', AValue);
end;

procedure TExtButton.SetScale(const AValue: string);
begin
  FScale := SetConfigItem('scale', AValue);
end;

procedure TExtButton._SetText(const AValue: string);
begin
  FText := SetConfigItem('text', 'setText', AValue);
end;

procedure TExtButton.SetToggleGroup(const AValue: string);
begin
  FToggleGroup := SetConfigItem('toggleGroup', AValue);
end;

procedure TExtButton._SetTooltip(const AValue: string);
begin
  FTooltip := SetConfigItem('tooltip', 'setTooltip', AValue);
end;

class function TExtButton.JSClassName: string;
begin
  Result := 'Ext.Button';
end;

procedure TExtButton.InitDefaults;
begin
  inherited;
  FHandleMouseEvents := true;
  FMenu := TExtBase.CreateInternal(Self, 'menu');
  FTemplate := TExtTemplate.CreateInternal(Self, 'template');
  FBtnEl := TExtElement.CreateInternal(Self, 'btnEl');
end;

function TExtButton.GetObjectNamePrefix: string;
begin
  Result := 'btn';
end;

function TExtButton.GetPressed(const AGroup: string): TExtExpression;
begin
  Result := TKWebResponse.Current.Items.CallMethod(Self, 'getPressed')
    .AddParam(AGroup)
    .AsExpression;
end;

function TExtButton.PerformClick: TExtExpression;
begin
  Result := FireEvent('click', nil);
end;

function TExtButton.Pressed_: TExtExpression;
begin
  Result := TKWebResponse.Current.Items.GetProperty(Self, 'pressed').AsExpression;
end;

function TExtButton.GetTemplateArgs: TExtExpression;
begin
  Result := TKWebResponse.Current.Items.CallMethod(Self, 'getTemplateArgs')
    .AsExpression;
end;

function TExtButton.GetText: TExtExpression;
begin
  Result := TKWebResponse.Current.Items.CallMethod(Self, 'getText')
    .AsExpression;
end;

function TExtButton.HasVisibleMenu: TExtExpression;
begin
  Result := TKWebResponse.Current.Items.CallMethod(Self, 'hasVisibleMenu')
    .AsExpression;
end;

function TExtButton.HideMenu: TExtExpression;
begin
  Result := TKWebResponse.Current.Items.CallMethod(Self, 'hideMenu')
    .AsExpression;
end;

function TExtButton.SetHandler(const AHandler: TExtExpression; const AScope: TExtObject): TExtExpression;
begin
  FHandler := AHandler;
  Result := TKWebResponse.Current.Items.CallMethod(Self, 'setHandler')
    .AddParam(AHandler)
    .AddParam(AScope)
    .AsExpression;
end;

function TExtButton.SetText(const AText: string): TExtExpression;
begin
  FText := AText;
  Result := TKWebResponse.Current.Items.CallMethod(Self, 'setText')
    .AddParam(AText)
    .AsExpression;
end;

function TExtButton.SetTooltip(const ATooltip: string): TExtExpression;
begin
  FTooltip := ATooltip;
  Result := TKWebResponse.Current.Items.CallMethod(Self, 'setTooltip')
    .AddParam(ATooltip)
    .AsExpression;
end;

function TExtButton.SetTooltip(const ATooltip: TExtObject): TExtExpression;
begin
  Result := TKWebResponse.Current.Items.CallMethod(Self, 'setTooltip')
    .AddParam(ATooltip)
    .AsExpression;
end;

procedure TExtViewView.SetEmptyText(AValue: string);
begin
  FEmptyText := SetConfigItem('emptyText', AValue);
end;

procedure TExtViewView.SetItemSelector(const AValue: string);
begin
  FItemSelector := SetConfigItem('itemSelector', AValue);
end;

procedure TExtViewView.SetMultiSelect(const AValue: Boolean);
begin
  FMultiSelect := SetConfigItem('multiSelect', AValue);
end;

procedure TExtViewView.SetOverItemCls(const AValue: string);
begin
  FOverItemCls := SetConfigItem('overItemCls', AValue);
end;

procedure TExtViewView.SetSelectedClass(const AValue: string);
begin
  FSelectedClass := SetConfigItem('selectedClass', AValue);
end;

procedure TExtViewView.SetSimpleSelect(const AValue: Boolean);
begin
  FSimpleSelect := SetConfigItem('simpleSelect', AValue);
end;

procedure TExtViewView.SetSingleSelect(const AValue: Boolean);
begin
  FSingleSelect := SetConfigItem('singleSelect', AValue);
end;

procedure TExtViewView._SetStore(const AValue: TExtDataStore);
begin
  FStore.Free;
  FStore := TExtDataStore(SetConfigItem('store', 'setStore', AValue));
end;

procedure TExtViewView.SetTpl(AValue: string);
begin
  FTpl := SetConfigItem('tpl', AValue);
end;

procedure TExtViewView.SetTrackOver(const AValue: Boolean);
begin
  FTrackOver := SetConfigItem('trackOver', AValue);
end;

class function TExtViewView.JSClassName: string;
begin
  Result := 'Ext.view.View';
end;

function TExtViewView.GetObjectNamePrefix: string;
begin
  Result := 'dv';
end;

procedure TExtViewView.InitDefaults;
begin
  inherited;
  FSelectedClass := 'x-view-selected';
  FTplArray := CreateConfigObjectArray('tpl');
end;

function TExtViewView.SetStore(const AStore: TExtDataStore): TExtExpression;
begin
  FreeAndNil(FStore);
  FStore := AStore;
  Result := TKWebResponse.Current.Items.CallMethod(Self, 'setStore')
    .AddParam(AStore)
    .AsExpression;
end;

class function TExtViewport.JSClassName: string;
begin
  Result := 'Ext.Viewport';
end;

function TExtViewport.GetObjectNamePrefix: string;
begin
  Result := 'vp';
end;

procedure TExtPanel.SetAnimCollapse(const AValue: Boolean);
begin
  FAnimCollapse := SetConfigItem('animCollapse', AValue);
end;

procedure TExtPanel.SetAutoLoadString(const AValue: string);
begin
  FAutoLoadString := SetConfigItem('autoLoad', AValue);
end;

procedure TExtPanel.SetAutoLoadBoolean(const AValue: Boolean);
begin
  FAutoLoadBoolean := SetConfigItem('autoLoad', AValue);
end;

procedure TExtPanel.SetBbar(const AValue: TExtObject);
begin
  FBbar.Free;
  FBbar := SetConfigItem('bbar', AValue);
end;

procedure TExtPanel.SetBodyStyle(const AValue: string);
begin
  FBodyStyle := SetConfigItem('bodyStyle', AValue);
end;

procedure TExtPanel.SetBorder(const AValue: Boolean);
begin
  FBorder := AValue;
  SetConfigItem('border', AValue);
end;

procedure TExtPanel.SetClosable(const AValue: Boolean);
begin
  FClosable := SetConfigItem('closable', AValue);
end;

procedure TExtPanel.SetCollapsible(const AValue: Boolean);
begin
  FCollapsible := SetConfigItem('collapsible', AValue);
end;

procedure TExtPanel.SetFbar(const AValue: TExtObject);
begin
  FFbar.Free;
  FFbar := SetConfigItem('fbar', AValue);
end;

procedure TExtPanel.SetFooter(const AValue: Boolean);
begin
  FFooter := SetConfigItem('footer', AValue);
end;

procedure TExtPanel.SetFrame(const AValue: Boolean);
begin
  FFrame := SetConfigItem('frame', AValue);
end;

procedure TExtPanel.SetHeader(const AValue: Boolean);
begin
  FHeader := AValue;
  SetConfigItem('header', AValue);
end;

procedure TExtPanel.SetIconCls(const AValue: string);
begin
  FIconCls := SetConfigItem('iconCls', AValue);
end;

procedure TExtPanel.SetMinButtonWidth(const AValue: Integer);
begin
  FMinButtonWidth := SetConfigItem('minButtonWidth', AValue);
end;

procedure TExtPanel._SetTitle(AValue: string);
begin
  FTitle := SetConfigItem('title', 'setTitle', AValue);
end;

procedure TExtPanel.SetCollapsed(const AValue: Boolean);
begin
  FCollapsed := SetConfigItemorProperty('collapsed', AValue);
end;

class function TExtPanel.JSClassName: string;
begin
  Result := 'Ext.Panel';
end;

class function TExtPanel.JSXType: string;
begin
  Result := 'panel';
end;

procedure TExtPanel.InitDefaults;
begin
  inherited;
  FAnimCollapse := true;
  FBbar := CreateConfigObjectArray('bbar');
  FBorder := true;
  FFbar := CreateConfigObjectArray('fbar');
  FMinButtonWidth := 75;
  FHeader := true;
end;

function TExtPanel.Close: TExtExpression;
begin
  Result := TKWebResponse.Current.Items.CallMethod(Self, 'close')
    .AsExpression;
end;

function TExtPanel.Collapse(const AAnimate: Boolean): TExtExpression;
begin
  Result := TKWebResponse.Current.Items.CallMethod(Self, 'collapse')
    .AddParam(AAnimate)
    .AsExpression;
end;

function TExtPanel.Expand(const AAnimate: Boolean): TExtExpression;
begin
  Result := TKWebResponse.Current.Items.CallMethod(Self, 'exand')
    .AddParam(AAnimate)
    .AsExpression;
end;

function TExtPanel.GetAutoLoad: TExtObject;
begin
  if not Assigned(FAutoLoad) then
    FAutoLoad := CreateConfigObject('autoLoad');
  Result := FAutoLoad;
end;

function TExtPanel.GetObjectNamePrefix: string;
begin
  Result := 'pnl';
end;

procedure TExtPanel.SetTbar(const AValue: TExtObject);
begin
  FTbar.Free;
  FTbar := SetConfigItem('tbar', AValue);
end;

function TExtPanel.SetTitle(const ATitle: string; const AIconCls: string): TExtExpression;
begin
  FTitle := ATitle;
  FIconCls := AIconCls;
  Result := TKWebResponse.Current.Items.CallMethod(Self, 'setTitle')
    .AddParam(ATitle)
    .AddParam(AIconCls)
    .AsExpression;
end;

procedure TExtSplitButton._SetArrowHandler(const AValue: TExtExpression);
begin
  FArrowHandler := SetConfigItem('arrowHandler', 'setArrowHandler', AValue);
end;

class function TExtSplitButton.JSClassName: string;
begin
  Result := 'Ext.SplitButton';
end;

function TExtSplitButton.SetArrowHandler(const AHandler: TExtExpression; const AScope: TExtObject): TExtExpression;
begin
  FArrowHandler := AHandler;
  Result := TKWebResponse.Current.Items.CallMethod(Self, 'setArrowHandler')
    .AddParam(AHandler)
    .AddParam(AScope)
    .AsExpression;
end;

class function TExtToolbarSpacer.JSClassName: string;
begin
  Result := 'Ext.Toolbar.Spacer';
end;

function TExtToolbarSpacer.GetObjectNamePrefix: string;
begin
  Result := 'spacer';
end;

procedure TExtToolbarTextItem._SetText(const AValue: string);
begin
  FText := SetConfigItem('text', 'setText', AValue);
end;

class function TExtToolbarTextItem.JSClassName: string;
begin
  Result := 'Ext.Toolbar.TextItem';
end;

procedure TExtToolbarTextItem.InitDefaults;
begin
  inherited;
end;

function TExtToolbarTextItem.SetText(const AText: string): TExtExpression;
begin
  FText := AText;
  Result := TKWebResponse.Current.Items.CallMethod(Self, 'setText').AddParam(AText).AsExpression;
end;

class function TExtToolbar.JSClassName: string;
begin
  Result := 'Ext.Toolbar';
end;

class function TExtToolbar.JSXType: string;
begin
  Result := 'toolbar';
end;

function TExtToolbar.GetObjectNamePrefix: string;
begin
  Result := 'tb';
end;

class function TExtToolbarSeparator.JSClassName: string;
begin
  Result := 'Ext.Toolbar.Separator';
end;

procedure TExtToolbarSeparator.InitDefaults;
begin
  inherited;
end;

class function TExtTip.JSClassName: string;
begin
  Result := 'Ext.Tip';
end;

function TExtToolbarFill.GetObjectNamePrefix: string;
begin
  Result := 'fill';
end;

class function TExtToolbarFill.JSClassName: string;
begin
  Result := 'Ext.Toolbar.Fill';
end;

class function TExtToolbarFill.JSXType: string;
begin
  Result := 'tbfill';
end;

class function TExtButtonGroup.JSClassName: string;
begin
  Result := 'Ext.ButtonGroup';
end;

class function TExtCycleButton.JSClassName: string;
begin
  Result := 'Ext.CycleButton';
end;

procedure TExtWindow._SetAnimateTarget(const AValue: string);
begin
  FAnimateTarget := SetConfigItem('animateTarget', 'setAnimateTarget', AValue);
end;

procedure TExtWindow.SetClosable(const AValue: Boolean);
begin
  FClosable := SetConfigItem('closable', AValue);
end;

procedure TExtWindow.SetConstrain(const AValue: Boolean);
begin
  FConstrain := SetConfigItem('constrain', AValue);
end;

procedure TExtWindow.SetMaximizable(const AValue: Boolean);
begin
  FMaximizable := SetConfigItem('maximizable', AValue);
end;

procedure TExtWindow.SetMaximized(const AValue: Boolean);
begin
  FMaximized := SetConfigItem('maximized', AValue);
end;

procedure TExtWindow.SetPlain(const AValue: Boolean);
begin
  FPlain := SetConfigItem('plain', AValue);
end;

procedure TExtWindow.SetResizeHandles(const AValue: string);
begin
  FResizeHandles := SetConfigItem('resizeHandles', AValue);
end;

class function TExtWindow.JSClassName: string;
begin
  Result := 'Ext.Window';
end;

procedure TExtWindow.InitDefaults;
begin
  inherited;
  FAnimateTargetElement := TExtElement.CreateInternal(Self, 'animateTarget');
  FBaseCls := 'x-window';
  FClosable := true;
  FExpandOnShow := true;
  FInitHidden := true;
  FMinHeight := 100;
  FMinWidth := 200;
  FResizeHandles := 'all';
  FBbar := CreateConfigObjectArray('bbar');
end;

function TExtWindow.Close: TExtExpression;
begin
  Result := TKWebResponse.Current.Items.CallMethod(Self, 'close').AsExpression;
end;

procedure TExtWindow.SetAnimateTarget(const AElement: string);
begin
  FAnimateTarget := SetConfigItem('animateTarget', 'setAnimateTarget', AElement);
end;

function TExtWindow.Show(const AAnimateTarget: string; const ACallback: TExtExpression;
  const AScope: TExtObject): TExtExpression;
begin
  Result := TKWebResponse.Current.Items.CallMethod(Self, 'show')
    .AddParam(AAnimateTarget)
    .AddParam(ACallback)
    .AddParam(AScope)
    .AsExpression;
end;

function TExtWindow.Show(const AAnimateTarget: TExtElement; const ACallback: TExtExpression;
  const AScope: TExtObject): TExtExpression;
begin
  Result := TKWebResponse.Current.Items.CallMethod(Self, 'show')
    .AddParam(AAnimateTarget)
    .AddParam(ACallback)
    .AddParam(AScope)
    .AsExpression;
end;

function TExtWindow.GetObjectNamePrefix: string;
begin
  Result := 'win';
end;

procedure TExtTabPanel._SetActiveTab(const AValue: string);
begin
  FActiveTab := SetConfigItem('activeTab', 'setActiveTab', AValue);
end;

procedure TExtTabPanel.SetActiveTabNumber(const AValue: Integer);
begin
  FActiveTabNumber := SetConfigItem('activeTab', AValue);
end;

procedure TExtTabPanel.SetDeferredRender(const AValue: Boolean);
begin
  FDeferredRender := SetConfigItem('deferredRender', AValue);
end;

procedure TExtTabPanel.SetLayoutOnTabChange(const AValue: Boolean);
begin
  FLayoutOnTabChange := SetConfigItem('layoutOnTabChange', AValue);
end;

procedure TExtTabPanel.SetOnTabChange(const AValue: TExtTabPanelOnTabchange);
begin
  RemoveAllListeners('tabchange');
  if Assigned(AValue) then
    //On('tabchange', Ajax('tabchange', ['This', '%0.nm', 'Tab', '(%1 ? %1.nm : null)'], True));
    &On('tabchange', TKWebResponse.Current.Items.AjaxCallMethod(Self, 'tabchange')
      .Event
      .AddRawParam('TabPanel', 'tabPanel.nm')
      .AddRawParam('NewTab', '(newCard ? newCard.nm : null)')
      .AddRawParam('OldTab', '(oldCard ? oldCard.nm : null)')
      .FunctionArgs('tabPanel, newCard, oldCard')
      .AsFunction);
  FOnTabChange := AValue;
end;

class function TExtTabPanel.JSClassName: string;
begin
  Result := 'Ext.TabPanel';
end;

function TExtTabPanel.GetActiveTab: TExtExpression;
begin
  Result := TKWebResponse.Current.Items.CallMethod(Self, 'getActiveTab').AsExpression;
end;

function TExtTabPanel.GetObjectNamePrefix: string;
begin
  Result := 'tabpnl';
end;

function TExtTabPanel.SetActiveTab(const AItem: string): TExtExpression;
begin
  Result := TKWebResponse.Current.Items.CallMethod(Self, 'setActiveTab')
    .AddParam(AItem)
    .AsExpression;
end;

function TExtTabPanel.SetActiveTab(const AItem: Integer): TExtExpression;
begin
  Result := TKWebResponse.Current.Items.CallMethod(Self, 'setActiveTab')
    .AddParam(AItem)
    .AsExpression;
end;

procedure TExtTabPanel.DoHandleEvent(const AEvtName: string);
begin
  inherited;
  if (AEvtName = 'tabchange') and Assigned(FOnTabChange) then
    FOnTabChange(TExtTabPanel(ParamAsObject('TabPanel')), TExtComponent(ParamAsObject('NewTab')));
end;

procedure TExtPagingToolbar.SetDisplayInfo(const AValue: Boolean);
begin
  FDisplayInfo := SetConfigItem('displayInfo', AValue);
end;

procedure TExtPagingToolbar.SetPageSize(const AValue: Integer);
begin
  FPageSize := SetConfigItem('pageSize', AValue);
end;

procedure TExtPagingToolbar.SetStore(const AValue: TExtDataStore);
begin
  FStore.Free;
  FStore := TExtDataStore(SetConfigItem('store', AValue));
end;

class function TExtPagingToolbar.JSClassName: string;
begin
  Result := 'Ext.PagingToolbar';
end;

function TExtPagingToolbar.MoveFirst: TExtExpression;
begin
  Result := TKWebResponse.Current.Items.CallMethod(Self, 'moveFirst').AsExpression;
end;

function TExtPagingToolbar.MoveLast: TExtExpression;
begin
  Result := TKWebResponse.Current.Items.CallMethod(Self, 'moveLast').AsExpression;
end;

function TExtPagingToolbar.MoveNext: TExtExpression;
begin
  Result := TKWebResponse.Current.Items.CallMethod(Self, 'moveNext').AsExpression;
end;

function TExtPagingToolbar.MovePrevious: TExtExpression;
begin
  Result := TKWebResponse.Current.Items.CallMethod(Self, 'movePrevious').AsExpression;
end;

class function TExtToolTip.JSClassName: string;
begin
  Result := 'Ext.tip.ToolTip';
end;

class function TExtQuickTip.JSClassName: string;
begin
  Result := 'Ext.tip.QuickTip';
end;

class function TExtMessageBoxSingleton.JSClassName: string;
begin
  Result := 'Ext.MessageBox';
end;

function TExtMessageBoxSingleton.Alert(const ATitle: string; const AMsg: string;
  const AFn: TExtExpression = nil; const AScope: TExtObject = nil): TExtExpression;
begin
  Result := TKWebResponse.Current.Items.CallMethod(Self, 'alert').AddParam(ATitle).AddParam(AMsg)
    .AddParam(AFn).AddParam(AScope).AsExpression;
end;

function TExtMessageBoxSingleton.ShowMessage(const AMessageType: TExtMessageType;
  const ATitle: string; const AMsg: string;
  const AFn: TExtExpression = nil; const AScope: TExtObject = nil): TExtExpression;
begin
  Result := TKWebResponse.Current.Items.CallMethod(Self, 'alert').AddParam(ATitle).AddParam(AMsg)
    .AddParam(AFn).AddParam(AScope).AsExpression;
end;

function TExtTabPanel.SetActiveTab(const AItem: TExtComponent): TExtExpression;
begin
  Result := TKWebResponse.Current.Items.CallMethod(Self, 'setActiveTab')
    .AddParam(AItem)
    .AsExpression;
end;

procedure TExtToolTip.SetTrackMouse(const AValue: Boolean);
begin
  FTrackMouse := SetConfigItem('trackMouse', AValue);
end;

{ TExtEvented }

function TExtEvented.FireEvent(const AEventName: string; const AArgs: TArray<TExtObject>): TExtExpression;
var
  LMethod: TJSMethodCall;
  LObject: TExtObject;
begin
  LMethod := TKWebResponse.Current.Items.CallMethod(Self, 'fireEvent').AddParam(AEventName);
  for LObject in AArgs do
    LMethod.AddParam(LObject);
  Result := LMethod.AsExpression;
end;

function TExtEvented.&On(const AEventName: string; const AHandler: TExtExpression;
  const AScope: TExtObject = nil; const AOptions: TExtObject = nil): TExtExpression;
begin
  Result := TKWebResponse.Current.Items.CallMethod(Self, 'on')
    .AddParam(AEventName)
    .AddParam(AHandler)
    .AddParam(AScope)
    .AddParam(AOptions)
    .AsExpression;
end;

function TExtEvented.RemoveAllListeners(const AEventName: string): TExtExpression;
begin
  Result := TKWebResponse.Current.Items.ExecuteJSCode(Self, Format('if (%s.events.%s) delete (%s.events.%s)',
    [JSName, AEventName, JSName, AEventName])).AsExpression;
end;

class function TExtEvented.JSClassName: string;
begin
  Result := 'Ext.Evented';
end;

{ TExtWidget }

class function TExtWidget.JSClassName: string;
begin
  Result := 'Ext.Widget';
end;

procedure TExtWidget.SetCls(const AValue: string);
begin
  FCls := SetConfigItem('cls', 'setCls', AValue);
end;

procedure TExtWidget.ToFront;
begin
  TKWebResponse.Current.Items.CallMethod(Self, 'toFront');
end;

end.

