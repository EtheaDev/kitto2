unit ExtensibleCalendar;

interface

uses
  StrUtils, ExtUtil, ExtPascal, ExtPascalUtils, Ext, ExtData;

type
  TExtCalendarBoxLayoutTemplate = class;
  TExtensibleCalendarPanel = class;
  TExtCalendarPicker = class;
  TExtCalendarView = class;
  TExtCalendarDateRangeField = class;
  TExtCalendarDayBodyTemplate = class;
  TExtCalendarDayBodyView = class;
  TExtCalendarDayHeaderTemplate = class;
  TExtCalendarDayHeaderView = class;
  TExtCalendarDayView = class;
  TExtCalendarDayViewTemplate = class;
  TExtCalendarEventEditForm = class;
  TExtCalendarEventEditWindow = class;
  TExtCalendarEventMappings = class;
  TExtCalendarMonthView = class;
  TExtCalendarMonthViewTemplate = class;
  TExtCalendarReminderField = class;
  TExtCalendarStatusProxy = class;
  TExtCalendarWeekView = class;

  TExtCalendarEventRecord = class(TExtObject);

  // Procedural types for events TExtCalendarView
  TExtCalendarViewOnDatechange = procedure(This : TExtCalendarView; StartDate : TDateTime; ViewStart : TDateTime; ViewEnd : TDateTime) of object;
  TExtCalendarViewOnDayout = procedure(This : TExtCalendarView; Dt : TDateTime; El : TExtElement) of object;
  TExtCalendarViewOnDayover = procedure(This : TExtCalendarView; Dt : TDateTime; El : TExtElement) of object;
  TExtCalendarViewOnEventclick = procedure(This : TExtCalendarView; Rec : TExtCalendarEventRecord; El : THTMLNode) of object;
  TExtCalendarViewOnEventdelete = procedure(This : TExtCalendarView; Rec : TExtCalendarEventRecord) of object;
  TExtCalendarViewOnEventmove = procedure(This : TExtCalendarView; Rec : TExtCalendarEventRecord) of object;
  TExtCalendarViewOnEventout = procedure(This : TExtCalendarView; Rec : TExtCalendarEventRecord; El : THTMLNode) of object;
  TExtCalendarViewOnEventover = procedure(This : TExtCalendarView; Rec : TExtCalendarEventRecord; El : THTMLNode) of object;
  TExtCalendarViewOnEventsrendered = procedure(This : TExtCalendarView) of object;
  TExtCalendarViewOnInitdrag = procedure(This : TExtCalendarView) of object;
  TExtCalendarViewOnRangeselect = procedure(This : TExtCalendarView; Dates : TExtObject; Callback : TExtFunction) of object;

  TExtCalendarView = class(TExtUtilObservable)
  private
    FDdCreateEventText : String;
    FDdMoveEventText : String;
    FDdResizeEventText : String;
    FEnableAddFx : Boolean;
    FEnableDD : Boolean;
    FEnableFx : Boolean;
    FEnableRemoveFx : Boolean;
    FEnableUpdateFx : Boolean;
    FMonitorResize : Boolean;
    FSpansHavePriority : Boolean;
    FStartDay : Integer;
    FTrackMouseOver : Boolean;
    FOnDatechange : TExtCalendarViewOnDatechange;
    FOnDayout : TExtCalendarViewOnDayout;
    FOnDayover : TExtCalendarViewOnDayover;
    FOnEventclick : TExtCalendarViewOnEventclick;
    FOnEventdelete : TExtCalendarViewOnEventdelete;
    FOnEventmove : TExtCalendarViewOnEventmove;
    FOnEventout : TExtCalendarViewOnEventout;
    FOnEventover : TExtCalendarViewOnEventover;
    FOnEventsrendered : TExtCalendarViewOnEventsrendered;
    FOnInitdrag : TExtCalendarViewOnInitdrag;
    FOnRangeselect : TExtCalendarViewOnRangeselect;
    procedure SetFDdCreateEventText(Value : String);
    procedure SetFDdMoveEventText(Value : String);
    procedure SetFDdResizeEventText(Value : String);
    procedure SetFEnableAddFx(Value : Boolean);
    procedure SetFEnableDD(Value : Boolean);
    procedure SetFEnableFx(Value : Boolean);
    procedure SetFEnableRemoveFx(Value : Boolean);
    procedure SetFEnableUpdateFx(Value : Boolean);
    procedure SetFMonitorResize(Value : Boolean);
    procedure SetFSpansHavePriority(Value : Boolean);
    procedure SetFStartDay(Value : Integer);
    procedure SetFTrackMouseOver(Value : Boolean);
    procedure SetFOnDatechange(Value : TExtCalendarViewOnDatechange);
    procedure SetFOnDayout(Value : TExtCalendarViewOnDayout);
    procedure SetFOnDayover(Value : TExtCalendarViewOnDayover);
    procedure SetFOnEventclick(Value : TExtCalendarViewOnEventclick);
    procedure SetFOnEventdelete(Value : TExtCalendarViewOnEventdelete);
    procedure SetFOnEventmove(Value : TExtCalendarViewOnEventmove);
    procedure SetFOnEventout(Value : TExtCalendarViewOnEventout);
    procedure SetFOnEventover(Value : TExtCalendarViewOnEventover);
    procedure SetFOnEventsrendered(Value : TExtCalendarViewOnEventsrendered);
    procedure SetFOnInitdrag(Value : TExtCalendarViewOnInitdrag);
    procedure SetFOnRangeselect(Value : TExtCalendarViewOnRangeselect);
  protected
    procedure HandleEvent(const AEvtName: string); override;
  public
    class function JSClassName : string; override;
    function GetEventEls(EventId : String) : TExtFunction;
    function GetEventIdFromEl(El : THTMLElement) : TExtFunction; overload;
    function GetEventIdFromEl(El : String) : TExtFunction; overload;
    function GetEventSelectorCls(EventId : String; ForSelect : Boolean) : TExtFunction;
    function GetStartDate : TExtFunction;
    function HighlightEvent(Els : TExtCompositeElement; Color : TExtObject = nil; O : TExtObject = nil) : TExtFunction;
    function IsToday : TExtFunction;
    function MoveDays(Value : Integer) : TExtFunction;
    function MoveMonths(Value : Integer) : TExtFunction;
    function MoveNext : TExtFunction;
    function MovePrev : TExtFunction;
    function MoveTo(Dt : TDateTime) : TExtFunction;
    function MoveToday : TExtFunction;
    function MoveWeeks(Value : Integer) : TExtFunction;
    function SetStartDate(Dt : TDateTime) : TExtFunction;
    function SetStore(Store : TExtDataStore) : TExtFunction;
    property DdCreateEventText : String read FDdCreateEventText write SetFDdCreateEventText;
    property DdMoveEventText : String read FDdMoveEventText write SetFDdMoveEventText;
    property DdResizeEventText : String read FDdResizeEventText write SetFDdResizeEventText;
    property EnableAddFx : Boolean read FEnableAddFx write SetFEnableAddFx;
    property EnableDD : Boolean read FEnableDD write SetFEnableDD;
    property EnableFx : Boolean read FEnableFx write SetFEnableFx;
    property EnableRemoveFx : Boolean read FEnableRemoveFx write SetFEnableRemoveFx;
    property EnableUpdateFx : Boolean read FEnableUpdateFx write SetFEnableUpdateFx;
    property MonitorResize : Boolean read FMonitorResize write SetFMonitorResize;
    property SpansHavePriority : Boolean read FSpansHavePriority write SetFSpansHavePriority;
    property StartDay : Integer read FStartDay write SetFStartDay;
    property TrackMouseOver : Boolean read FTrackMouseOver write SetFTrackMouseOver;
    property OnDatechange : TExtCalendarViewOnDatechange read FOnDatechange write SetFOnDatechange;
    property OnDayout : TExtCalendarViewOnDayout read FOnDayout write SetFOnDayout;
    property OnDayover : TExtCalendarViewOnDayover read FOnDayover write SetFOnDayover;
    property OnEventclick : TExtCalendarViewOnEventclick read FOnEventclick write SetFOnEventclick;
    property OnEventdelete : TExtCalendarViewOnEventdelete read FOnEventdelete write SetFOnEventdelete;
    property OnEventmove : TExtCalendarViewOnEventmove read FOnEventmove write SetFOnEventmove;
    property OnEventout : TExtCalendarViewOnEventout read FOnEventout write SetFOnEventout;
    property OnEventover : TExtCalendarViewOnEventover read FOnEventover write SetFOnEventover;
    property OnEventsrendered : TExtCalendarViewOnEventsrendered read FOnEventsrendered write SetFOnEventsrendered;
    property OnInitdrag : TExtCalendarViewOnInitdrag read FOnInitdrag write SetFOnInitdrag;
    property OnRangeselect : TExtCalendarViewOnRangeselect read FOnRangeselect write SetFOnRangeselect;
  end;

  TExtCalendarDateRangeField = class(TExtUtilObservable)
  private
    FToText : String;
    procedure SetFToText(Value : String);
  public
    class function JSClassName : string; override;
    function GetValue : TExtFunction;
    function SetValue(V : TExtObjectList) : TExtFunction; overload;
    function SetValue(V : TExtObject) : TExtFunction; overload;
    function SetValue(V : TDateTime) : TExtFunction; overload;
    property ToText : String read FToText write SetFToText;
  end;

  // Procedural types for events TExtensibleCalendarPanel
  TExtensibleCalendarPanelOnDatechange = procedure(This : TExtensibleCalendarPanel; StartDate : TDateTime; ViewStart : TDateTime; ViewEnd : TDateTime) of object;
  TExtensibleCalendarPanelOnDayclick = procedure(This : TExtensibleCalendarPanel; Dt : TDateTime; Allday : Boolean; El : TExtElement) of object;
  TExtensibleCalendarPanelOnEventadd = procedure(This : TExtensibleCalendarPanel; Rec : TExtCalendarEventRecord) of object;
  TExtensibleCalendarPanelOnEventcancel = procedure(This : TExtensibleCalendarPanel; Rec : TExtCalendarEventRecord) of object;
  TExtensibleCalendarPanelOnEventclick = procedure(This : TExtensibleCalendarPanel; Rec : TExtCalendarEventRecord; El : THTMLNode) of object;
  TExtensibleCalendarPanelOnEventdelete = procedure(This : TExtensibleCalendarPanel; Rec : TExtCalendarEventRecord) of object;
  TExtensibleCalendarPanelOnEventmove = procedure(This : TExtensibleCalendarPanel; Rec : TExtCalendarEventRecord) of object;
  TExtensibleCalendarPanelOnEventout = procedure(This : TExtensibleCalendarPanel; Rec : TExtCalendarEventRecord; El : THTMLNode) of object;
  TExtensibleCalendarPanelOnEventover = procedure(This : TExtensibleCalendarPanel; Rec : TExtCalendarEventRecord; El : THTMLNode) of object;
  TExtensibleCalendarPanelOnEventresize = procedure(This : TExtensibleCalendarPanel; Rec : TExtCalendarEventRecord) of object;
  TExtensibleCalendarPanelOnEventsrendered = procedure(This : TExtensibleCalendarPanel) of object;
  TExtensibleCalendarPanelOnEventupdate = procedure(This : TExtensibleCalendarPanel; Rec : TExtCalendarEventRecord) of object;
  TExtensibleCalendarPanelOnInitdrag = procedure(This : TExtensibleCalendarPanel) of object;
  TExtensibleCalendarPanelOnRangeselect = procedure(This : TExtensibleCalendarPanel; Dates : TExtObject; Callback : TExtFunction) of object;
  TExtensibleCalendarPanelOnViewchange = procedure(This : TExtensibleCalendarPanel; View : TExtCalendarView; Info : TExtObject) of object;

  TExtensibleCalendarPanel = class(TExtPanel)
  private
    FDayText : String;
    FMonthText : String;
    FShowDayView : Boolean;
    FShowMonthView : Boolean;
    FShowNavBar : Boolean;
    FShowTime : Boolean;
    FShowTodayText : Boolean;
    FShowWeekView : Boolean;
    FTodayText : String;
    FWeekText : String;
    FActiveItem: Integer;
    FOnDatechange : TExtensibleCalendarPanelOnDatechange;
    FOnDayclick : TExtensibleCalendarPanelOnDayclick;
    FOnEventadd : TExtensibleCalendarPanelOnEventadd;
    FOnEventcancel : TExtensibleCalendarPanelOnEventcancel;
    FOnEventclick : TExtensibleCalendarPanelOnEventclick;
    FOnEventdelete : TExtensibleCalendarPanelOnEventdelete;
    FOnEventmove : TExtensibleCalendarPanelOnEventmove;
    FOnEventout : TExtensibleCalendarPanelOnEventout;
    FOnEventover : TExtensibleCalendarPanelOnEventover;
    FOnEventresize : TExtensibleCalendarPanelOnEventresize;
    FOnEventsrendered : TExtensibleCalendarPanelOnEventsrendered;
    FOnEventupdate : TExtensibleCalendarPanelOnEventupdate;
    FOnInitdrag : TExtensibleCalendarPanelOnInitdrag;
    FOnRangeselect : TExtensibleCalendarPanelOnRangeselect;
    FOnViewchange : TExtensibleCalendarPanelOnViewchange;
    FCalendarStore: TExtDataStore;
    FEventStore: TExtDataStore;
    FGoText: string;
    FJumpToText: string;
    FReadOnly: Boolean;
    FShowMultiDayView: Boolean;
    FShowMultiWeekView: Boolean;
    procedure SetDayText(const AValue: string);
    procedure SetMonthText(Value : String);
    procedure SetShowDayView(Value : Boolean);
    procedure SetShowMonthView(Value : Boolean);
    procedure SetShowNavBar(Value : Boolean);
    procedure SetShowTime(Value : Boolean);
    procedure SetShowTodayText(Value : Boolean);
    procedure SetShowWeekView(Value : Boolean);
    procedure SetTodayText(Value : String);
    procedure SetWeekText(Value : String);
    procedure SetFOnDatechange(Value : TExtensibleCalendarPanelOnDatechange);
    procedure SetFOnDayclick(Value : TExtensibleCalendarPanelOnDayclick);
    procedure SetFOnEventadd(Value : TExtensibleCalendarPanelOnEventadd);
    procedure SetFOnEventcancel(Value : TExtensibleCalendarPanelOnEventcancel);
    procedure SetFOnEventclick(Value : TExtensibleCalendarPanelOnEventclick);
    procedure SetFOnEventdelete(Value : TExtensibleCalendarPanelOnEventdelete);
    procedure SetFOnEventmove(Value : TExtensibleCalendarPanelOnEventmove);
    procedure SetFOnEventout(Value : TExtensibleCalendarPanelOnEventout);
    procedure SetFOnEventover(Value : TExtensibleCalendarPanelOnEventover);
    procedure SetFOnEventresize(Value : TExtensibleCalendarPanelOnEventresize);
    procedure SetFOnEventsrendered(Value : TExtensibleCalendarPanelOnEventsrendered);
    procedure SetFOnEventupdate(Value : TExtensibleCalendarPanelOnEventupdate);
    procedure SetFOnInitdrag(Value : TExtensibleCalendarPanelOnInitdrag);
    procedure SetFOnRangeselect(Value : TExtensibleCalendarPanelOnRangeselect);
    procedure SetFOnViewchange(Value : TExtensibleCalendarPanelOnViewchange);
    procedure SetActiveItem(const AValue: Integer);
    procedure SetCalendarStore(const AValue: TExtDataStore);
    procedure SetEventStore(const AValue: TExtDataStore);
    procedure SetGoText(const AValue: string);
    procedure SetJumpToText(const AValue: string);
    procedure SetReadOnly(const AValue: Boolean);
    procedure SetShowMultiDayView(const AValue: Boolean);
    procedure SetShowMultiWeekView(const AValue: Boolean);
  protected
    procedure HandleEvent(const AEvtName: string); override;
  public
    class function JSClassName : string; override;
    function GetActiveView : TExtFunction;
    function HideEditForm : TExtFunction;
    function SetStartDate(Dt : TDateTime) : TExtFunction;
    function ShowEditForm(RecordJS : TExtCalendarEventRecord) : TExtFunction;
    property ActiveItem: Integer read FActiveItem write SetActiveItem;
    property DayText : String read FDayText write SetDayText;
    property MonthText : String read FMonthText write SetMonthText;
    property GoText: string read FGoText write SetGoText;
    property JumpToText: string read FJumpToText write SetJumpToText;
    property ShowDayView : Boolean read FShowDayView write SetShowDayView;
    property ShowMultiDayView : Boolean read FShowMultiDayView write SetShowMultiDayView;
    property ShowMonthView : Boolean read FShowMonthView write SetShowMonthView;
    property ShowNavBar : Boolean read FShowNavBar write SetShowNavBar;
    property ShowTime : Boolean read FShowTime write SetShowTime;
    property ShowTodayText : Boolean read FShowTodayText write SetShowTodayText;
    property ShowWeekView : Boolean read FShowWeekView write SetShowWeekView;
    property ShowMultiWeekView : Boolean read FShowMultiWeekView write SetShowMultiWeekView;
    property EventStore: TExtDataStore read FEventStore write SetEventStore;
    property CalendarStore: TExtDataStore read FCalendarStore write SetCalendarStore;
    property TodayText : String read FTodayText write SetTodayText;
    property WeekText : String read FWeekText write SetWeekText;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly;
    property OnDatechange : TExtensibleCalendarPanelOnDatechange read FOnDatechange write SetFOnDatechange;
    property OnDayclick : TExtensibleCalendarPanelOnDayclick read FOnDayclick write SetFOnDayclick;
    property OnEventadd : TExtensibleCalendarPanelOnEventadd read FOnEventadd write SetFOnEventadd;
    property OnEventcancel : TExtensibleCalendarPanelOnEventcancel read FOnEventcancel write SetFOnEventcancel;
    property OnEventclick : TExtensibleCalendarPanelOnEventclick read FOnEventclick write SetFOnEventclick;
    property OnEventdelete : TExtensibleCalendarPanelOnEventdelete read FOnEventdelete write SetFOnEventdelete;
    property OnEventmove : TExtensibleCalendarPanelOnEventmove read FOnEventmove write SetFOnEventmove;
    property OnEventout : TExtensibleCalendarPanelOnEventout read FOnEventout write SetFOnEventout;
    property OnEventover : TExtensibleCalendarPanelOnEventover read FOnEventover write SetFOnEventover;
    property OnEventresize : TExtensibleCalendarPanelOnEventresize read FOnEventresize write SetFOnEventresize;
    property OnEventsrendered : TExtensibleCalendarPanelOnEventsrendered read FOnEventsrendered write SetFOnEventsrendered;
    property OnEventupdate : TExtensibleCalendarPanelOnEventupdate read FOnEventupdate write SetFOnEventupdate;
    property OnInitdrag : TExtensibleCalendarPanelOnInitdrag read FOnInitdrag write SetFOnInitdrag;
    property OnRangeselect : TExtensibleCalendarPanelOnRangeselect read FOnRangeselect write SetFOnRangeselect;
    property OnViewchange : TExtensibleCalendarPanelOnViewchange read FOnViewchange write SetFOnViewchange;
  end;

  TExtCalendarPicker = class(TExtUtilObservable)
  public
    class function JSClassName : string; override;
  end;

  // Procedural types for events TExtCalendarDayHeaderTemplate
  TExtCalendarDayHeaderTemplateOnDatechange = procedure(This : TExtCalendarDayHeaderTemplate; StartDate : TDateTime; ViewStart : TDateTime; ViewEnd : TDateTime) of object;
  TExtCalendarDayHeaderTemplateOnDayclick = procedure(This : TExtCalendarDayHeaderTemplate; Dt : TDateTime; Allday : Boolean; El : TExtElement) of object;
  TExtCalendarDayHeaderTemplateOnDayout = procedure(This : TExtCalendarDayHeaderTemplate; Dt : TDateTime; El : TExtElement) of object;
  TExtCalendarDayHeaderTemplateOnDayover = procedure(This : TExtCalendarDayHeaderTemplate; Dt : TDateTime; El : TExtElement) of object;
  TExtCalendarDayHeaderTemplateOnEditdetails = procedure(This : TExtCalendarDayHeaderTemplate; Rec : TExtCalendarEventRecord) of object;
  TExtCalendarDayHeaderTemplateOnEventadd = procedure(This : TExtCalendarDayHeaderTemplate; Rec : TExtCalendarEventRecord) of object;
  TExtCalendarDayHeaderTemplateOnEventcancel = procedure(This : TExtCalendarDayHeaderTemplate; Rec : TExtCalendarEventRecord) of object;
  TExtCalendarDayHeaderTemplateOnEventclick = procedure(This : TExtCalendarDayHeaderTemplate; Rec : TExtCalendarEventRecord; El : THTMLNode) of object;
  TExtCalendarDayHeaderTemplateOnEventdelete = procedure(This : TExtCalendarDayHeaderTemplate; Rec : TExtCalendarEventRecord) of object;
  TExtCalendarDayHeaderTemplateOnEventmove = procedure(This : TExtCalendarDayHeaderTemplate; Rec : TExtCalendarEventRecord) of object;
  TExtCalendarDayHeaderTemplateOnEventout = procedure(This : TExtCalendarDayHeaderTemplate; Rec : TExtCalendarEventRecord; El : THTMLNode) of object;
  TExtCalendarDayHeaderTemplateOnEventover = procedure(This : TExtCalendarDayHeaderTemplate; Rec : TExtCalendarEventRecord; El : THTMLNode) of object;
  TExtCalendarDayHeaderTemplateOnEventresize = procedure(This : TExtCalendarDayHeaderTemplate; Rec : TExtCalendarEventRecord) of object;
  TExtCalendarDayHeaderTemplateOnEventsrendered = procedure(This : TExtCalendarDayHeaderTemplate) of object;
  TExtCalendarDayHeaderTemplateOnEventupdate = procedure(This : TExtCalendarDayHeaderTemplate; Rec : TExtCalendarEventRecord) of object;
  TExtCalendarDayHeaderTemplateOnInitdrag = procedure(This : TExtCalendarDayHeaderTemplate) of object;
  TExtCalendarDayHeaderTemplateOnRangeselect = procedure(This : TExtCalendarDayHeaderTemplate; Dates : TExtObject; Callback : TExtFunction) of object;
  TExtCalendarDayHeaderTemplateOnViewchange = procedure(This : TExtCalendarDayHeaderTemplate; View : TExtCalendarView; Info : TExtObject) of object;
  TExtCalendarDayHeaderTemplateOnWeekclick = procedure(This : TExtCalendarDayHeaderTemplate; Dt : TDateTime) of object;

  TExtCalendarDayHeaderTemplate = class(TExtUtilObservable)
  private
    FAddEventCls : String;
    FDayCount : Integer;
    FDayText : String;
    FDdCreateEventText : String;
    FDdMoveEventText : String;
    FDdResizeEventText : String;
    FEnableAddFx : Boolean;
    FEnableDD : Boolean;
    FEnableFx : Boolean;
    FEnableRemoveFx : Boolean;
    FEnableUpdateFx : Boolean;
    FMonitorResize : Boolean;
    FMonthText : String;
    FMoveEventCls : String;
    FShowDayView : Boolean;
    FShowHeader : Boolean;
    FShowMonthView : Boolean;
    FShowNavBar : Boolean;
    FShowTime : Boolean;
    FShowTodayText : Boolean;
    FShowWeekLinks : Boolean;
    FShowWeekNumbers : Boolean;
    FShowWeekView : Boolean;
    FSpansHavePriority : Boolean;
    FStartDay : Integer;
    FTodayText : String;
    FToText : String;
    FTrackMouseOver : Boolean;
    FWeekLinkOverClass : String;
    FWeekText : String;
    FOnDatechange : TExtCalendarDayHeaderTemplateOnDatechange;
    FOnDayclick : TExtCalendarDayHeaderTemplateOnDayclick;
    FOnDayout : TExtCalendarDayHeaderTemplateOnDayout;
    FOnDayover : TExtCalendarDayHeaderTemplateOnDayover;
    FOnEditdetails : TExtCalendarDayHeaderTemplateOnEditdetails;
    FOnEventadd : TExtCalendarDayHeaderTemplateOnEventadd;
    FOnEventcancel : TExtCalendarDayHeaderTemplateOnEventcancel;
    FOnEventclick : TExtCalendarDayHeaderTemplateOnEventclick;
    FOnEventdelete : TExtCalendarDayHeaderTemplateOnEventdelete;
    FOnEventmove : TExtCalendarDayHeaderTemplateOnEventmove;
    FOnEventout : TExtCalendarDayHeaderTemplateOnEventout;
    FOnEventover : TExtCalendarDayHeaderTemplateOnEventover;
    FOnEventresize : TExtCalendarDayHeaderTemplateOnEventresize;
    FOnEventsrendered : TExtCalendarDayHeaderTemplateOnEventsrendered;
    FOnEventupdate : TExtCalendarDayHeaderTemplateOnEventupdate;
    FOnInitdrag : TExtCalendarDayHeaderTemplateOnInitdrag;
    FOnRangeselect : TExtCalendarDayHeaderTemplateOnRangeselect;
    FOnViewchange : TExtCalendarDayHeaderTemplateOnViewchange;
    FOnWeekclick : TExtCalendarDayHeaderTemplateOnWeekclick;
    procedure SetFAddEventCls(Value : String);
    procedure SetFDayCount(Value : Integer);
    procedure SetFDayText(Value : String);
    procedure SetFDdCreateEventText(Value : String);
    procedure SetFDdMoveEventText(Value : String);
    procedure SetFDdResizeEventText(Value : String);
    procedure SetFEnableAddFx(Value : Boolean);
    procedure SetFEnableDD(Value : Boolean);
    procedure SetFEnableFx(Value : Boolean);
    procedure SetFEnableRemoveFx(Value : Boolean);
    procedure SetFEnableUpdateFx(Value : Boolean);
    procedure SetFMonitorResize(Value : Boolean);
    procedure SetFMonthText(Value : String);
    procedure SetFMoveEventCls(Value : String);
    procedure SetShowDayView(Value : Boolean);
    procedure SetFShowHeader(Value : Boolean);
    procedure SetFShowMonthView(Value : Boolean);
    procedure SetFShowNavBar(Value : Boolean);
    procedure SetFShowTime(Value : Boolean);
    procedure SetFShowTodayText(Value : Boolean);
    procedure SetFShowWeekLinks(Value : Boolean);
    procedure SetFShowWeekNumbers(Value : Boolean);
    procedure SetFShowWeekView(Value : Boolean);
    procedure SetFSpansHavePriority(Value : Boolean);
    procedure SetFStartDay(Value : Integer);
    procedure SetFTodayText(Value : String);
    procedure SetFToText(Value : String);
    procedure SetFTrackMouseOver(Value : Boolean);
    procedure SetFWeekLinkOverClass(Value : String);
    procedure SetFWeekText(Value : String);
    procedure SetFOnDatechange(Value : TExtCalendarDayHeaderTemplateOnDatechange);
    procedure SetFOnDayclick(Value : TExtCalendarDayHeaderTemplateOnDayclick);
    procedure SetFOnDayout(Value : TExtCalendarDayHeaderTemplateOnDayout);
    procedure SetFOnDayover(Value : TExtCalendarDayHeaderTemplateOnDayover);
    procedure SetFOnEditdetails(Value : TExtCalendarDayHeaderTemplateOnEditdetails);
    procedure SetFOnEventadd(Value : TExtCalendarDayHeaderTemplateOnEventadd);
    procedure SetFOnEventcancel(Value : TExtCalendarDayHeaderTemplateOnEventcancel);
    procedure SetFOnEventclick(Value : TExtCalendarDayHeaderTemplateOnEventclick);
    procedure SetFOnEventdelete(Value : TExtCalendarDayHeaderTemplateOnEventdelete);
    procedure SetFOnEventmove(Value : TExtCalendarDayHeaderTemplateOnEventmove);
    procedure SetFOnEventout(Value : TExtCalendarDayHeaderTemplateOnEventout);
    procedure SetFOnEventover(Value : TExtCalendarDayHeaderTemplateOnEventover);
    procedure SetFOnEventresize(Value : TExtCalendarDayHeaderTemplateOnEventresize);
    procedure SetFOnEventsrendered(Value : TExtCalendarDayHeaderTemplateOnEventsrendered);
    procedure SetFOnEventupdate(Value : TExtCalendarDayHeaderTemplateOnEventupdate);
    procedure SetFOnInitdrag(Value : TExtCalendarDayHeaderTemplateOnInitdrag);
    procedure SetFOnRangeselect(Value : TExtCalendarDayHeaderTemplateOnRangeselect);
    procedure SetFOnViewchange(Value : TExtCalendarDayHeaderTemplateOnViewchange);
    procedure SetFOnWeekclick(Value : TExtCalendarDayHeaderTemplateOnWeekclick);
  protected
    procedure HandleEvent(const AEvtName: string); override;
  public
    class function JSClassName : string; override;
    function GetActiveView : TExtFunction;
    function GetEventAllDayTemplate : TExtFunction;
    function GetEventEls(EventId : String) : TExtFunction;
    function GetEventIdFromEl(El : THTMLElement) : TExtFunction; overload;
    function GetEventIdFromEl(El : String) : TExtFunction; overload;
    function GetEventSelectorCls(EventId : String; ForSelect : Boolean) : TExtFunction;
    function GetStartDate : TExtFunction;
    function GetValue : TExtFunction;
    function HideEditForm : TExtFunction;
    function HighlightEvent(Els : TExtCompositeElement; Color : TExtObject = nil; O : TExtObject = nil) : TExtFunction;
    function IsToday : TExtFunction;
    function MoveDays(Value : Integer) : TExtFunction;
    function MoveMonths(Value : Integer) : TExtFunction;
    function MoveNext : TExtFunction;
    function MovePrev : TExtFunction;
    function MoveTo(Dt : TDateTime) : TExtFunction;
    function MoveToday : TExtFunction;
    function MoveWeeks(Value : Integer) : TExtFunction;
    function ScrollTo(Y : Integer; Defer : Boolean = false) : TExtFunction;
    function SetStartDate(Dt : TDateTime) : TExtFunction;
    function SetStore(Store : TExtDataStore) : TExtFunction;
    function SetValue(V : TExtObjectList) : TExtFunction; overload;
    function SetValue(V : TExtObject) : TExtFunction; overload;
    function SetValue(V : TDateTime) : TExtFunction; overload;
    function Show(O : TExtObject; AnimateTarget : String = '') : TExtFunction; overload;
    function Show(O : TExtDataRecord; AnimateTarget : String = '') : TExtFunction; overload;
    function ShowEditForm(RecordJS : TExtCalendarEventRecord) : TExtFunction;
    function Unregister(El : TExtObjectList) : TExtFunction; overload;
    function Unregister(El : String) : TExtFunction; overload;
    function UpdateMsg(Msg : String) : TExtFunction;
    property AddEventCls : String read FAddEventCls write SetFAddEventCls;
    property DayCount : Integer read FDayCount write SetFDayCount;
    property DayText : String read FDayText write SetFDayText;
    property DdCreateEventText : String read FDdCreateEventText write SetFDdCreateEventText;
    property DdMoveEventText : String read FDdMoveEventText write SetFDdMoveEventText;
    property DdResizeEventText : String read FDdResizeEventText write SetFDdResizeEventText;
    property EnableAddFx : Boolean read FEnableAddFx write SetFEnableAddFx;
    property EnableDD : Boolean read FEnableDD write SetFEnableDD;
    property EnableFx : Boolean read FEnableFx write SetFEnableFx;
    property EnableRemoveFx : Boolean read FEnableRemoveFx write SetFEnableRemoveFx;
    property EnableUpdateFx : Boolean read FEnableUpdateFx write SetFEnableUpdateFx;
    property MonitorResize : Boolean read FMonitorResize write SetFMonitorResize;
    property MonthText : String read FMonthText write SetFMonthText;
    property MoveEventCls : String read FMoveEventCls write SetFMoveEventCls;
    property ShowDayView : Boolean read FShowDayView write SetShowDayView;
    property ShowHeader : Boolean read FShowHeader write SetFShowHeader;
    property ShowMonthView : Boolean read FShowMonthView write SetFShowMonthView;
    property ShowNavBar : Boolean read FShowNavBar write SetFShowNavBar;
    property ShowTime : Boolean read FShowTime write SetFShowTime;
    property ShowTodayText : Boolean read FShowTodayText write SetFShowTodayText;
    property ShowWeekLinks : Boolean read FShowWeekLinks write SetFShowWeekLinks;
    property ShowWeekNumbers : Boolean read FShowWeekNumbers write SetFShowWeekNumbers;
    property ShowWeekView : Boolean read FShowWeekView write SetFShowWeekView;
    property SpansHavePriority : Boolean read FSpansHavePriority write SetFSpansHavePriority;
    property StartDay : Integer read FStartDay write SetFStartDay;
    property TodayText : String read FTodayText write SetFTodayText;
    property ToText : String read FToText write SetFToText;
    property TrackMouseOver : Boolean read FTrackMouseOver write SetFTrackMouseOver;
    property WeekLinkOverClass : String read FWeekLinkOverClass write SetFWeekLinkOverClass;
    property WeekText : String read FWeekText write SetFWeekText;
    property OnDatechange : TExtCalendarDayHeaderTemplateOnDatechange read FOnDatechange write SetFOnDatechange;
    property OnDayclick : TExtCalendarDayHeaderTemplateOnDayclick read FOnDayclick write SetFOnDayclick;
    property OnDayout : TExtCalendarDayHeaderTemplateOnDayout read FOnDayout write SetFOnDayout;
    property OnDayover : TExtCalendarDayHeaderTemplateOnDayover read FOnDayover write SetFOnDayover;
    property OnEditdetails : TExtCalendarDayHeaderTemplateOnEditdetails read FOnEditdetails write SetFOnEditdetails;
    property OnEventadd : TExtCalendarDayHeaderTemplateOnEventadd read FOnEventadd write SetFOnEventadd;
    property OnEventcancel : TExtCalendarDayHeaderTemplateOnEventcancel read FOnEventcancel write SetFOnEventcancel;
    property OnEventclick : TExtCalendarDayHeaderTemplateOnEventclick read FOnEventclick write SetFOnEventclick;
    property OnEventdelete : TExtCalendarDayHeaderTemplateOnEventdelete read FOnEventdelete write SetFOnEventdelete;
    property OnEventmove : TExtCalendarDayHeaderTemplateOnEventmove read FOnEventmove write SetFOnEventmove;
    property OnEventout : TExtCalendarDayHeaderTemplateOnEventout read FOnEventout write SetFOnEventout;
    property OnEventover : TExtCalendarDayHeaderTemplateOnEventover read FOnEventover write SetFOnEventover;
    property OnEventresize : TExtCalendarDayHeaderTemplateOnEventresize read FOnEventresize write SetFOnEventresize;
    property OnEventsrendered : TExtCalendarDayHeaderTemplateOnEventsrendered read FOnEventsrendered write SetFOnEventsrendered;
    property OnEventupdate : TExtCalendarDayHeaderTemplateOnEventupdate read FOnEventupdate write SetFOnEventupdate;
    property OnInitdrag : TExtCalendarDayHeaderTemplateOnInitdrag read FOnInitdrag write SetFOnInitdrag;
    property OnRangeselect : TExtCalendarDayHeaderTemplateOnRangeselect read FOnRangeselect write SetFOnRangeselect;
    property OnViewchange : TExtCalendarDayHeaderTemplateOnViewchange read FOnViewchange write SetFOnViewchange;
    property OnWeekclick : TExtCalendarDayHeaderTemplateOnWeekclick read FOnWeekclick write SetFOnWeekclick;
  end;

  TExtCalendarDayBodyTemplate = class(TExtUtilObservable)
  public
    class function JSClassName : string; override;
  end;

  // Procedural types for events TExtCalendarDayBodyView
  TExtCalendarDayBodyViewOnDayclick = procedure(This : TExtCalendarDayBodyView; Dt : TDateTime; Allday : Boolean; El : TExtElement) of object;
  TExtCalendarDayBodyViewOnEventresize = procedure(This : TExtCalendarDayBodyView; Rec : TExtCalendarEventRecord) of object;

  TExtCalendarDayBodyView = class(TExtUtilObservable)
  private
    FOnDayclick : TExtCalendarDayBodyViewOnDayclick;
    FOnEventresize : TExtCalendarDayBodyViewOnEventresize;
    procedure SetFOnDayclick(Value : TExtCalendarDayBodyViewOnDayclick);
    procedure SetFOnEventresize(Value : TExtCalendarDayBodyViewOnEventresize);
  protected
    procedure HandleEvent(const AEvtName: string); override;
  public
    class function JSClassName : string; override;
    function GetEventAllDayTemplate : TExtFunction;
    function ScrollTo(Y : Integer; Defer : Boolean = false) : TExtFunction;
    property OnDayclick : TExtCalendarDayBodyViewOnDayclick read FOnDayclick write SetFOnDayclick;
    property OnEventresize : TExtCalendarDayBodyViewOnEventresize read FOnEventresize write SetFOnEventresize;
  end;

  TExtCalendarBoxLayoutTemplate = class(TExtUtilObservable)
  public
    class function JSClassName : string; override;
  end;

  TExtCalendarStatusProxy = class(TExtUtilObservable)
  private
    FAddEventCls : String;
    FMoveEventCls : String;
    procedure SetFAddEventCls(Value : String);
    procedure SetFMoveEventCls(Value : String);
  public
    class function JSClassName : string; override;
    function UpdateMsg(Msg : String) : TExtFunction;
    property AddEventCls : String read FAddEventCls write SetFAddEventCls;
    property MoveEventCls : String read FMoveEventCls write SetFMoveEventCls;
  end;

  TExtCalendarWeekView = class(TExtUtilObservable)
  private
    FDayCount : Integer;
    procedure SetFDayCount(Value : Integer);
  public
    class function JSClassName : string; override;
    property DayCount : Integer read FDayCount write SetFDayCount;
  end;

  TExtCalendarReminderField = class(TExtUtilObservable)
  public
    class function JSClassName : string; override;
  end;

  TExtCalendarDayViewTemplate = class(TExtUtilObservable)
  public
    class function JSClassName : string; override;
  end;

  // Procedural types for events TExtCalendarEventEditForm
  TExtCalendarEventEditFormOnEventadd = procedure(This : TExtCalendarEventEditForm; Rec : TExtCalendarEventRecord) of object;
  TExtCalendarEventEditFormOnEventcancel = procedure(This : TExtCalendarEventEditForm; Rec : TExtCalendarEventRecord) of object;
  TExtCalendarEventEditFormOnEventdelete = procedure(This : TExtCalendarEventEditForm; Rec : TExtCalendarEventRecord) of object;
  TExtCalendarEventEditFormOnEventupdate = procedure(This : TExtCalendarEventEditForm; Rec : TExtCalendarEventRecord) of object;

  TExtCalendarEventEditForm = class(TExtUtilObservable)
  private
    FOnEventadd : TExtCalendarEventEditFormOnEventadd;
    FOnEventcancel : TExtCalendarEventEditFormOnEventcancel;
    FOnEventdelete : TExtCalendarEventEditFormOnEventdelete;
    FOnEventupdate : TExtCalendarEventEditFormOnEventupdate;
    procedure SetFOnEventadd(Value : TExtCalendarEventEditFormOnEventadd);
    procedure SetFOnEventcancel(Value : TExtCalendarEventEditFormOnEventcancel);
    procedure SetFOnEventdelete(Value : TExtCalendarEventEditFormOnEventdelete);
    procedure SetFOnEventupdate(Value : TExtCalendarEventEditFormOnEventupdate);
  protected
    procedure HandleEvent(const AEvtName: string); override;
  public
    class function JSClassName : string; override;
    property OnEventadd : TExtCalendarEventEditFormOnEventadd read FOnEventadd write SetFOnEventadd;
    property OnEventcancel : TExtCalendarEventEditFormOnEventcancel read FOnEventcancel write SetFOnEventcancel;
    property OnEventdelete : TExtCalendarEventEditFormOnEventdelete read FOnEventdelete write SetFOnEventdelete;
    property OnEventupdate : TExtCalendarEventEditFormOnEventupdate read FOnEventupdate write SetFOnEventupdate;
  end;

  // Procedural types for events TExtCalendarDayHeaderView
  TExtCalendarDayHeaderViewOnDayclick = procedure(This : TExtCalendarDayHeaderView; Dt : TDateTime; Allday : Boolean; El : TExtElement) of object;

  TExtCalendarDayHeaderView = class(TExtUtilObservable)
  private
    FOnDayclick : TExtCalendarDayHeaderViewOnDayclick;
    procedure SetFOnDayclick(Value : TExtCalendarDayHeaderViewOnDayclick);
  protected
    procedure HandleEvent(const AEvtName: string); override;
  public
    class function JSClassName : string; override;
    property OnDayclick : TExtCalendarDayHeaderViewOnDayclick read FOnDayclick write SetFOnDayclick;
  end;

  TExtCalendarDayView = class(TExtUtilObservable)
  private
    FDayCount : Integer;
    FDdCreateEventText : String;
    FDdMoveEventText : String;
    FShowTime : Boolean;
    FShowTodayText : Boolean;
    FTodayText : String;
    procedure SetFDayCount(Value : Integer);
    procedure SetFDdCreateEventText(Value : String);
    procedure SetFDdMoveEventText(Value : String);
    procedure SetFShowTime(Value : Boolean);
    procedure SetFShowTodayText(Value : Boolean);
    procedure SetFTodayText(Value : String);
  public
    class function JSClassName : string; override;
    function GetStartDate : TExtFunction;
    function IsToday : TExtFunction;
    function MoveDays(Value : Integer) : TExtFunction;
    function MoveNext : TExtFunction;
    function MovePrev : TExtFunction;
    function MoveTo(Dt : TDateTime) : TExtFunction;
    function MoveToday : TExtFunction;
    function SetStartDate(Dt : TDateTime) : TExtFunction;
    property DayCount : Integer read FDayCount write SetFDayCount;
    property DdCreateEventText : String read FDdCreateEventText write SetFDdCreateEventText;
    property DdMoveEventText : String read FDdMoveEventText write SetFDdMoveEventText;
    property ShowTime : Boolean read FShowTime write SetFShowTime;
    property ShowTodayText : Boolean read FShowTodayText write SetFShowTodayText;
    property TodayText : String read FTodayText write SetFTodayText;
  end;

  // Procedural types for events TExtCalendarMonthView
  TExtCalendarMonthViewOnDayclick = procedure(This : TExtCalendarMonthView; Dt : TDateTime; Allday : Boolean; El : TExtElement) of object;
  TExtCalendarMonthViewOnWeekclick = procedure(This : TExtCalendarMonthView; Dt : TDateTime) of object;

  TExtCalendarMonthView = class(TExtUtilObservable)
  private
    FShowHeader : Boolean;
    FShowTime : Boolean;
    FShowTodayText : Boolean;
    FShowWeekLinks : Boolean;
    FShowWeekNumbers : Boolean;
    FTodayText : String;
    FWeekLinkOverClass : String;
    FOnDayclick : TExtCalendarMonthViewOnDayclick;
    FOnWeekclick : TExtCalendarMonthViewOnWeekclick;
    procedure SetFShowHeader(Value : Boolean);
    procedure SetFShowTime(Value : Boolean);
    procedure SetFShowTodayText(Value : Boolean);
    procedure SetFShowWeekLinks(Value : Boolean);
    procedure SetFShowWeekNumbers(Value : Boolean);
    procedure SetFTodayText(Value : String);
    procedure SetFWeekLinkOverClass(Value : String);
    procedure SetFOnDayclick(Value : TExtCalendarMonthViewOnDayclick);
    procedure SetFOnWeekclick(Value : TExtCalendarMonthViewOnWeekclick);
  protected
    procedure HandleEvent(const AEvtName: string); override;
  public
    class function JSClassName : string; override;
    property ShowHeader : Boolean read FShowHeader write SetFShowHeader;
    property ShowTime : Boolean read FShowTime write SetFShowTime;
    property ShowTodayText : Boolean read FShowTodayText write SetFShowTodayText;
    property ShowWeekLinks : Boolean read FShowWeekLinks write SetFShowWeekLinks;
    property ShowWeekNumbers : Boolean read FShowWeekNumbers write SetFShowWeekNumbers;
    property TodayText : String read FTodayText write SetFTodayText;
    property WeekLinkOverClass : String read FWeekLinkOverClass write SetFWeekLinkOverClass;
    property OnDayclick : TExtCalendarMonthViewOnDayclick read FOnDayclick write SetFOnDayclick;
    property OnWeekclick : TExtCalendarMonthViewOnWeekclick read FOnWeekclick write SetFOnWeekclick;
  end;

  TExtCalendarMonthViewTemplate = class(TExtUtilObservable)
  public
    class function JSClassName : string; override;
  end;

  // Procedural types for events TExtCalendarEventEditWindow
  TExtCalendarEventEditWindowOnEditdetails = procedure(This : TExtCalendarEventEditWindow; Rec : TExtCalendarEventRecord) of object;
  TExtCalendarEventEditWindowOnEventadd = procedure(This : TExtCalendarEventEditWindow; Rec : TExtCalendarEventRecord) of object;
  TExtCalendarEventEditWindowOnEventcancel = procedure(This : TExtCalendarEventEditWindow; Rec : TExtCalendarEventRecord) of object;
  TExtCalendarEventEditWindowOnEventdelete = procedure(This : TExtCalendarEventEditWindow; Rec : TExtCalendarEventRecord) of object;
  TExtCalendarEventEditWindowOnEventupdate = procedure(This : TExtCalendarEventEditWindow; Rec : TExtCalendarEventRecord) of object;

  TExtCalendarEventEditWindow = class(TExtUtilObservable)
  private
    FOnEditdetails : TExtCalendarEventEditWindowOnEditdetails;
    FOnEventadd : TExtCalendarEventEditWindowOnEventadd;
    FOnEventcancel : TExtCalendarEventEditWindowOnEventcancel;
    FOnEventdelete : TExtCalendarEventEditWindowOnEventdelete;
    FOnEventupdate : TExtCalendarEventEditWindowOnEventupdate;
    procedure SetFOnEditdetails(Value : TExtCalendarEventEditWindowOnEditdetails);
    procedure SetFOnEventadd(Value : TExtCalendarEventEditWindowOnEventadd);
    procedure SetFOnEventcancel(Value : TExtCalendarEventEditWindowOnEventcancel);
    procedure SetFOnEventdelete(Value : TExtCalendarEventEditWindowOnEventdelete);
    procedure SetFOnEventupdate(Value : TExtCalendarEventEditWindowOnEventupdate);
  protected
    procedure HandleEvent(const AEvtName: string); override;
  public
    class function JSClassName : string; override;
    function Show(O : TExtObject; AnimateTarget : String = '') : TExtFunction; overload;
    function Show(O : TExtDataRecord; AnimateTarget : String = '') : TExtFunction; overload;
    property OnEditdetails : TExtCalendarEventEditWindowOnEditdetails read FOnEditdetails write SetFOnEditdetails;
    property OnEventadd : TExtCalendarEventEditWindowOnEventadd read FOnEventadd write SetFOnEventadd;
    property OnEventcancel : TExtCalendarEventEditWindowOnEventcancel read FOnEventcancel write SetFOnEventcancel;
    property OnEventdelete : TExtCalendarEventEditWindowOnEventdelete read FOnEventdelete write SetFOnEventdelete;
    property OnEventupdate : TExtCalendarEventEditWindowOnEventupdate read FOnEventupdate write SetFOnEventupdate;
  end;

  TExtCalendarEventMappings = class(TExtUtilObservable)
  public
    class function JSClassName : string; override;
  end;

implementation

procedure TExtCalendarView.SetFDdCreateEventText(Value : String); begin
  FDdCreateEventText := Value;
  JSCode('ddCreateEventText:' + VarToJSON([Value]));
end;

procedure TExtCalendarView.SetFDdMoveEventText(Value : String); begin
  FDdMoveEventText := Value;
  JSCode('ddMoveEventText:' + VarToJSON([Value]));
end;

procedure TExtCalendarView.SetFDdResizeEventText(Value : String); begin
  FDdResizeEventText := Value;
  JSCode('ddResizeEventText:' + VarToJSON([Value]));
end;

procedure TExtCalendarView.SetFEnableAddFx(Value : Boolean); begin
  FEnableAddFx := Value;
  JSCode('enableAddFx:' + VarToJSON([Value]));
end;

procedure TExtCalendarView.SetFEnableDD(Value : Boolean); begin
  FEnableDD := Value;
  JSCode('enableDD:' + VarToJSON([Value]));
end;

procedure TExtCalendarView.SetFEnableFx(Value : Boolean); begin
  FEnableFx := Value;
  JSCode('enableFx:' + VarToJSON([Value]));
end;

procedure TExtCalendarView.SetFEnableRemoveFx(Value : Boolean); begin
  FEnableRemoveFx := Value;
  JSCode('enableRemoveFx:' + VarToJSON([Value]));
end;

procedure TExtCalendarView.SetFEnableUpdateFx(Value : Boolean); begin
  FEnableUpdateFx := Value;
  JSCode('enableUpdateFx:' + VarToJSON([Value]));
end;

procedure TExtCalendarView.SetFMonitorResize(Value : Boolean); begin
  FMonitorResize := Value;
  JSCode('monitorResize:' + VarToJSON([Value]));
end;

procedure TExtCalendarView.SetFSpansHavePriority(Value : Boolean); begin
  FSpansHavePriority := Value;
  JSCode('spansHavePriority:' + VarToJSON([Value]));
end;

procedure TExtCalendarView.SetFStartDay(Value : Integer); begin
  FStartDay := Value;
  JSCode('startDay:' + VarToJSON([Value]));
end;

procedure TExtCalendarView.SetFTrackMouseOver(Value : Boolean); begin
  FTrackMouseOver := Value;
  JSCode('trackMouseOver:' + VarToJSON([Value]));
end;

procedure TExtCalendarView.SetFOnDatechange(Value : TExtCalendarViewOnDatechange); begin
  if Assigned(FOnDatechange) then
    JSCode(JSName+'.events ["datechange"].listeners=[];');
  if Assigned(Value) then
    AddListener('datechange', Ajax('datechange', ['This', '%0.nm','StartDate', '%1','ViewStart', '%2','ViewEnd', '%3'], true));
  FOnDatechange := Value;
end;

procedure TExtCalendarView.SetFOnDayout(Value : TExtCalendarViewOnDayout); begin
  if Assigned(FOnDayout) then
    JSCode(JSName+'.events ["dayout"].listeners=[];');
  if Assigned(Value) then
    AddListener('dayout', Ajax('dayout', ['This', '%0.nm','Dt', '%1','El', '%2.nm'], true));
  FOnDayout := Value;
end;

procedure TExtCalendarView.SetFOnDayover(Value : TExtCalendarViewOnDayover); begin
  if Assigned(FOnDayover) then
    JSCode(JSName+'.events ["dayover"].listeners=[];');
  if Assigned(Value) then
    AddListener('dayover', Ajax('dayover', ['This', '%0.nm','Dt', '%1','El', '%2.nm'], true));
  FOnDayover := Value;
end;

procedure TExtCalendarView.SetFOnEventclick(Value : TExtCalendarViewOnEventclick); begin
  if Assigned(FOnEventclick) then
    JSCode(JSName+'.events ["eventclick"].listeners=[];');
  if Assigned(Value) then
    AddListener('eventclick', Ajax('eventclick', ['This', '%0.nm','Rec', '%1.nm','El', '%2.nm'], true));
  FOnEventclick := Value;
end;

procedure TExtCalendarView.SetFOnEventdelete(Value : TExtCalendarViewOnEventdelete); begin
  if Assigned(FOnEventdelete) then
    JSCode(JSName+'.events ["eventdelete"].listeners=[];');
  if Assigned(Value) then
    AddListener('eventdelete', Ajax('eventdelete', ['This', '%0.nm','Rec', '%1.nm'], true));
  FOnEventdelete := Value;
end;

procedure TExtCalendarView.SetFOnEventmove(Value : TExtCalendarViewOnEventmove); begin
  if Assigned(FOnEventmove) then
    JSCode(JSName+'.events ["eventmove"].listeners=[];');
  if Assigned(Value) then
    AddListener('eventmove', Ajax('eventmove', ['This', '%0.nm','Rec', '%1.nm'], true));
  FOnEventmove := Value;
end;

procedure TExtCalendarView.SetFOnEventout(Value : TExtCalendarViewOnEventout); begin
  if Assigned(FOnEventout) then
    JSCode(JSName+'.events ["eventout"].listeners=[];');
  if Assigned(Value) then
    AddListener('eventout', Ajax('eventout', ['This', '%0.nm','Rec', '%1.nm','El', '%2.nm'], true));
  FOnEventout := Value;
end;

procedure TExtCalendarView.SetFOnEventover(Value : TExtCalendarViewOnEventover); begin
  if Assigned(FOnEventover) then
    JSCode(JSName+'.events ["eventover"].listeners=[];');
  if Assigned(Value) then
    AddListener('eventover', Ajax('eventover', ['This', '%0.nm','Rec', '%1.nm','El', '%2.nm'], true));
  FOnEventover := Value;
end;

procedure TExtCalendarView.SetFOnEventsrendered(Value : TExtCalendarViewOnEventsrendered); begin
  if Assigned(FOnEventsrendered) then
    JSCode(JSName+'.events ["eventsrendered"].listeners=[];');
  if Assigned(Value) then
    AddListener('eventsrendered', Ajax('eventsrendered', ['This', '%0.nm'], true));
  FOnEventsrendered := Value;
end;

procedure TExtCalendarView.SetFOnInitdrag(Value : TExtCalendarViewOnInitdrag); begin
  if Assigned(FOnInitdrag) then
    JSCode(JSName+'.events ["initdrag"].listeners=[];');
  if Assigned(Value) then
    AddListener('initdrag', Ajax('initdrag', ['This', '%0.nm'], true));
  FOnInitdrag := Value;
end;

procedure TExtCalendarView.SetFOnRangeselect(Value : TExtCalendarViewOnRangeselect); begin
  if Assigned(FOnRangeselect) then
    JSCode(JSName+'.events ["rangeselect"].listeners=[];');
  if Assigned(Value) then
    AddListener('rangeselect', Ajax('rangeselect', ['This', '%0.nm','Dates', '%1.nm','Callback', '%2.nm'], true));
  FOnRangeselect := Value;
end;

class function TExtCalendarView.JSClassName : string; begin
  Result := 'Ext.calendar.CalendarView';
end;

function TExtCalendarView.GetEventEls(EventId : String) : TExtFunction; begin
  JSCode(JSName + '.getEventEls(' + VarToJSON([EventId]) + ');', 'TExtCalendarView');
  Result := Self;
end;

function TExtCalendarView.GetEventIdFromEl(El : THTMLElement) : TExtFunction; begin
  JSCode(JSName + '.GetEventIdFromEl(' + VarToJSON([El, false]) + ');', 'TExtCalendarView');
  Result := Self;
end;

function TExtCalendarView.GetEventIdFromEl(El : String) : TExtFunction; begin
  JSCode(JSName + '.getEventIdFromEl(' + VarToJSON([El]) + ');', 'TExtCalendarView');
  Result := Self;
end;

function TExtCalendarView.GetEventSelectorCls(EventId : String; ForSelect : Boolean) : TExtFunction; begin
  JSCode(JSName + '.getEventSelectorCls(' + VarToJSON([EventId, ForSelect]) + ');', 'TExtCalendarView');
  Result := Self;
end;

function TExtCalendarView.GetStartDate : TExtFunction; begin
  JSCode(JSName + '.getStartDate();', 'TExtCalendarView');
  Result := Self;
end;

function TExtCalendarView.HighlightEvent(Els : TExtCompositeElement; Color : TExtObject = nil; O : TExtObject = nil) : TExtFunction; begin
  JSCode(JSName + '.highlightEvent(' + VarToJSON([Els, false, Color, false, O, false]) + ');', 'TExtCalendarView');
  Result := Self;
end;

function TExtCalendarView.IsToday : TExtFunction; begin
  JSCode(JSName + '.isToday();', 'TExtCalendarView');
  Result := Self;
end;

function TExtCalendarView.MoveDays(Value : Integer) : TExtFunction; begin
  JSCode(JSName + '.moveDays(' + VarToJSON([Value]) + ');', 'TExtCalendarView');
  Result := Self;
end;

function TExtCalendarView.MoveMonths(Value : Integer) : TExtFunction; begin
  JSCode(JSName + '.moveMonths(' + VarToJSON([Value]) + ');', 'TExtCalendarView');
  Result := Self;
end;

function TExtCalendarView.MoveNext : TExtFunction; begin
  JSCode(JSName + '.moveNext();', 'TExtCalendarView');
  Result := Self;
end;

function TExtCalendarView.MovePrev : TExtFunction; begin
  JSCode(JSName + '.movePrev();', 'TExtCalendarView');
  Result := Self;
end;

function TExtCalendarView.MoveTo(Dt : TDateTime) : TExtFunction; begin
  JSCode(JSName + '.moveTo(' + VarToJSON([Dt]) + ');', 'TExtCalendarView');
  Result := Self;
end;

function TExtCalendarView.MoveToday : TExtFunction; begin
  JSCode(JSName + '.moveToday();', 'TExtCalendarView');
  Result := Self;
end;

function TExtCalendarView.MoveWeeks(Value : Integer) : TExtFunction; begin
  JSCode(JSName + '.moveWeeks(' + VarToJSON([Value]) + ');', 'TExtCalendarView');
  Result := Self;
end;

function TExtCalendarView.SetStartDate(Dt : TDateTime) : TExtFunction; begin
  JSCode(JSName + '.setStartDate(' + VarToJSON([Dt]) + ');', 'TExtCalendarView');
  Result := Self;
end;

function TExtCalendarView.SetStore(Store : TExtDataStore) : TExtFunction; begin
  JSCode(JSName + '.setStore(' + VarToJSON([Store, false]) + ');', 'TExtCalendarView');
  Result := Self;
end;

procedure TExtCalendarView.HandleEvent(const AEvtName : string); begin
  inherited;
  if (AEvtName = 'datechange') and Assigned(FOnDatechange) then
    FOnDatechange(TExtCalendarView(ParamAsObject('This')), ParamAsTDateTime('StartDate'), ParamAsTDateTime('ViewStart'), ParamAsTDateTime('ViewEnd'))
  else if (AEvtName = 'dayout') and Assigned(FOnDayout) then
    FOnDayout(TExtCalendarView(ParamAsObject('This')), ParamAsTDateTime('Dt'), TExtElement(ParamAsObject('El')))
  else if (AEvtName = 'dayover') and Assigned(FOnDayover) then
    FOnDayover(TExtCalendarView(ParamAsObject('This')), ParamAsTDateTime('Dt'), TExtElement(ParamAsObject('El')))
  else if (AEvtName = 'eventclick') and Assigned(FOnEventclick) then
    FOnEventclick(TExtCalendarView(ParamAsObject('This')), TExtCalendarEventRecord(ParamAsObject('Rec')), THTMLNode(ParamAsObject('El')))
  else if (AEvtName = 'eventdelete') and Assigned(FOnEventdelete) then
    FOnEventdelete(TExtCalendarView(ParamAsObject('This')), TExtCalendarEventRecord(ParamAsObject('Rec')))
  else if (AEvtName = 'eventmove') and Assigned(FOnEventmove) then
    FOnEventmove(TExtCalendarView(ParamAsObject('This')), TExtCalendarEventRecord(ParamAsObject('Rec')))
  else if (AEvtName = 'eventout') and Assigned(FOnEventout) then
    FOnEventout(TExtCalendarView(ParamAsObject('This')), TExtCalendarEventRecord(ParamAsObject('Rec')), THTMLNode(ParamAsObject('El')))
  else if (AEvtName = 'eventover') and Assigned(FOnEventover) then
    FOnEventover(TExtCalendarView(ParamAsObject('This')), TExtCalendarEventRecord(ParamAsObject('Rec')), THTMLNode(ParamAsObject('El')))
  else if (AEvtName = 'eventsrendered') and Assigned(FOnEventsrendered) then
    FOnEventsrendered(TExtCalendarView(ParamAsObject('This')))
  else if (AEvtName = 'initdrag') and Assigned(FOnInitdrag) then
    FOnInitdrag(TExtCalendarView(ParamAsObject('This')))
  else if (AEvtName = 'rangeselect') and Assigned(FOnRangeselect) then
    FOnRangeselect(TExtCalendarView(ParamAsObject('This')), TExtObject(ParamAsObject('Dates')), TExtFunction(ParamAsObject('Callback')));
end;

procedure TExtCalendarDateRangeField.SetFToText(Value : String); begin
  FToText := Value;
  JSCode('toText:' + VarToJSON([Value]));
end;

class function TExtCalendarDateRangeField.JSClassName : string; begin
  Result := 'Ext.calendar.DateRangeField';
end;

function TExtCalendarDateRangeField.GetValue : TExtFunction; begin
  JSCode(JSName + '.getValue();', 'TExtCalendarDateRangeField');
  Result := Self;
end;

function TExtCalendarDateRangeField.SetValue(V : TExtObjectList) : TExtFunction; begin
  JSCode(JSName + '.SetValue(' + VarToJSON(V) + ');', 'TExtCalendarDateRangeField');
  Result := Self;
end;

function TExtCalendarDateRangeField.SetValue(V : TExtObject) : TExtFunction; begin
  JSCode(JSName + '.SetValue(' + VarToJSON([V, false]) + ');', 'TExtCalendarDateRangeField');
  Result := Self;
end;

function TExtCalendarDateRangeField.SetValue(V : TDateTime) : TExtFunction; begin
  JSCode(JSName + '.setValue(' + VarToJSON([V]) + ');', 'TExtCalendarDateRangeField');
  Result := Self;
end;

procedure TExtensibleCalendarPanel.SetActiveItem(const AValue: Integer);
begin
  FActiveItem := AValue;
  ExtSession.ResponseItems.SetConfigItem(Self, 'activeItem', [AValue]);
end;

procedure TExtensibleCalendarPanel.SetCalendarStore(const AValue: TExtDataStore);
begin
  FCalendarStore.Free;
  FCalendarStore := AValue;
  ExtSession.ResponseItems.SetConfigItem(Self, 'calendarStore', [AValue, False]);
end;

procedure TExtensibleCalendarPanel.SetDayText(const AValue: string);
begin
  FDayText := AValue;
  ExtSession.ResponseItems.SetConfigItem(Self, 'dayText', [AValue]);
end;

procedure TExtensibleCalendarPanel.SetEventStore(const AValue: TExtDataStore);
begin
  FEventStore.Free;
  FEventStore := AValue;
  ExtSession.ResponseItems.SetConfigItem(Self, 'eventStore', [AValue, False]);
end;

procedure TExtensibleCalendarPanel.SetMonthText(Value : String);
begin
  FMonthText := Value;
  ExtSession.ResponseItems.SetConfigItem(Self, 'monthText', [Value]);
end;

procedure TExtensibleCalendarPanel.SetReadOnly(const AValue: Boolean);
begin
  FReadOnly := AValue;
  ExtSession.ResponseItems.SetConfigItem(Self, 'readOnly', [AValue]);
end;

procedure TExtensibleCalendarPanel.SetShowDayView(Value : Boolean);
begin
  FShowDayView := Value;
  ExtSession.ResponseItems.SetConfigItem(Self, 'showDayView', [Value]);
end;

procedure TExtensibleCalendarPanel.SetShowMonthView(Value : Boolean);
begin
  FShowMonthView := Value;
  ExtSession.ResponseItems.SetConfigItem(Self, 'showMonthView', [Value]);
end;

procedure TExtensibleCalendarPanel.SetShowMultiDayView(const AValue: Boolean);
begin
  FShowMultiDayView := AValue;
  ExtSession.ResponseItems.SetConfigItem(Self, 'showMultiDayView', [AValue]);
end;

procedure TExtensibleCalendarPanel.SetShowMultiWeekView(const AValue: Boolean);
begin
  FShowMultiWeekView := AValue;
  ExtSession.ResponseItems.SetConfigItem(Self, 'showMultiWeekView', [AValue]);
end;

procedure TExtensibleCalendarPanel.SetShowNavBar(Value : Boolean);
begin
  FShowNavBar := Value;
  ExtSession.ResponseItems.SetConfigItem(Self, 'showNavBar', [Value]);
end;

procedure TExtensibleCalendarPanel.SetShowTime(Value : Boolean); begin
  FShowTime := Value;
  ExtSession.ResponseItems.SetConfigItem(Self, 'showTime', [Value]);
end;

procedure TExtensibleCalendarPanel.SetShowTodayText(Value : Boolean); begin
  FShowTodayText := Value;
  ExtSession.ResponseItems.SetConfigItem(Self, 'showTodayText', [Value]);
end;

procedure TExtensibleCalendarPanel.SetShowWeekView(Value : Boolean); begin
  FShowWeekView := Value;
  ExtSession.ResponseItems.SetConfigItem(Self, 'showWeekView', [Value]);
end;

procedure TExtensibleCalendarPanel.SetTodayText(Value : String); begin
  FTodayText := Value;
  ExtSession.ResponseItems.SetConfigItem(Self, 'todayText', [Value]);
end;

procedure TExtensibleCalendarPanel.SetWeekText(Value : String); begin
  FWeekText := Value;
  ExtSession.ResponseItems.SetConfigItem(Self, 'weekText', [Value]);
end;

procedure TExtensibleCalendarPanel.SetFOnDatechange(Value : TExtensibleCalendarPanelOnDatechange); begin
  if Assigned(FOnDatechange) then
    JSCode(JSName+'.events ["datechange"].listeners=[];');
  if Assigned(Value) then
    AddListener('datechange', Ajax('datechange', ['This', '%0.nm','StartDate', '%1','ViewStart', '%2','ViewEnd', '%3'], true));
  FOnDatechange := Value;
end;

procedure TExtensibleCalendarPanel.SetFOnDayclick(Value : TExtensibleCalendarPanelOnDayclick); begin
  if Assigned(FOnDayclick) then
    RemoveAllListeners('dayclick');
  if Assigned(Value) then
    AddListener('dayclick', Ajax('dayclick', ['This', '%0.nm','Dt', '%1','Allday', '%2','El', '%3.nm'], True));
  FOnDayclick := Value;
end;

procedure TExtensibleCalendarPanel.SetFOnEventadd(Value : TExtensibleCalendarPanelOnEventadd); begin
  if Assigned(FOnEventadd) then
    JSCode(JSName+'.events ["eventadd"].listeners=[];');
  if Assigned(Value) then
    AddListener('eventadd', Ajax('eventadd', ['This', '%0.nm','Rec', '%1.nm'], true));
  FOnEventadd := Value;
end;

procedure TExtensibleCalendarPanel.SetFOnEventcancel(Value : TExtensibleCalendarPanelOnEventcancel); begin
  if Assigned(FOnEventcancel) then
    JSCode(JSName+'.events ["eventcancel"].listeners=[];');
  if Assigned(Value) then
    AddListener('eventcancel', Ajax('eventcancel', ['This', '%0.nm','Rec', '%1.nm'], true));
  FOnEventcancel := Value;
end;

procedure TExtensibleCalendarPanel.SetFOnEventclick(Value : TExtensibleCalendarPanelOnEventclick); begin
  if Assigned(FOnEventclick) then
    JSCode(JSName+'.events ["eventclick"].listeners=[];');
  if Assigned(Value) then
    AddListener('eventclick', Ajax('eventclick', ['This', '%0.nm','Rec', '%1.nm','El', '%2.nm'], true));
  FOnEventclick := Value;
end;

procedure TExtensibleCalendarPanel.SetFOnEventdelete(Value : TExtensibleCalendarPanelOnEventdelete); begin
  if Assigned(FOnEventdelete) then
    JSCode(JSName+'.events ["eventdelete"].listeners=[];');
  if Assigned(Value) then
    AddListener('eventdelete', Ajax('eventdelete', ['This', '%0.nm','Rec', '%1.nm'], true));
  FOnEventdelete := Value;
end;

procedure TExtensibleCalendarPanel.SetFOnEventmove(Value : TExtensibleCalendarPanelOnEventmove); begin
  if Assigned(FOnEventmove) then
    JSCode(JSName+'.events ["eventmove"].listeners=[];');
  if Assigned(Value) then
    AddListener('eventmove', Ajax('eventmove', ['This', '%0.nm','Rec', '%1.nm'], true));
  FOnEventmove := Value;
end;

procedure TExtensibleCalendarPanel.SetFOnEventout(Value : TExtensibleCalendarPanelOnEventout); begin
  if Assigned(FOnEventout) then
    JSCode(JSName+'.events ["eventout"].listeners=[];');
  if Assigned(Value) then
    AddListener('eventout', Ajax('eventout', ['This', '%0.nm','Rec', '%1.nm','El', '%2.nm'], true));
  FOnEventout := Value;
end;

procedure TExtensibleCalendarPanel.SetFOnEventover(Value : TExtensibleCalendarPanelOnEventover); begin
  if Assigned(FOnEventover) then
    JSCode(JSName+'.events ["eventover"].listeners=[];');
  if Assigned(Value) then
    AddListener('eventover', Ajax('eventover', ['This', '%0.nm','Rec', '%1.nm','El', '%2.nm'], true));
  FOnEventover := Value;
end;

procedure TExtensibleCalendarPanel.SetFOnEventresize(Value : TExtensibleCalendarPanelOnEventresize); begin
  if Assigned(FOnEventresize) then
    JSCode(JSName+'.events ["eventresize"].listeners=[];');
  if Assigned(Value) then
    AddListener('eventresize', Ajax('eventresize', ['This', '%0.nm','Rec', '%1.nm'], true));
  FOnEventresize := Value;
end;

procedure TExtensibleCalendarPanel.SetFOnEventsrendered(Value : TExtensibleCalendarPanelOnEventsrendered); begin
  if Assigned(FOnEventsrendered) then
    JSCode(JSName+'.events ["eventsrendered"].listeners=[];');
  if Assigned(Value) then
    AddListener('eventsrendered', Ajax('eventsrendered', ['This', '%0.nm'], true));
  FOnEventsrendered := Value;
end;

procedure TExtensibleCalendarPanel.SetFOnEventupdate(Value : TExtensibleCalendarPanelOnEventupdate); begin
  if Assigned(FOnEventupdate) then
    JSCode(JSName+'.events ["eventupdate"].listeners=[];');
  if Assigned(Value) then
    AddListener('eventupdate', Ajax('eventupdate', ['This', '%0.nm','Rec', '%1.nm'], true));
  FOnEventupdate := Value;
end;

procedure TExtensibleCalendarPanel.SetFOnInitdrag(Value : TExtensibleCalendarPanelOnInitdrag); begin
  if Assigned(FOnInitdrag) then
    JSCode(JSName+'.events ["initdrag"].listeners=[];');
  if Assigned(Value) then
    AddListener('initdrag', Ajax('initdrag', ['This', '%0.nm'], true));
  FOnInitdrag := Value;
end;

procedure TExtensibleCalendarPanel.SetFOnRangeselect(Value : TExtensibleCalendarPanelOnRangeselect); begin
  if Assigned(FOnRangeselect) then
    JSCode(JSName+'.events ["rangeselect"].listeners=[];');
  if Assigned(Value) then
    AddListener('rangeselect', Ajax('rangeselect', ['This', '%0.nm','Dates', '%1.nm','Callback', '%2.nm'], true));
  FOnRangeselect := Value;
end;

procedure TExtensibleCalendarPanel.SetFOnViewchange(Value : TExtensibleCalendarPanelOnViewchange); begin
  if Assigned(FOnViewchange) then
    JSCode(JSName+'.events ["viewchange"].listeners=[];');
  if Assigned(Value) then
    AddListener('viewchange', Ajax('viewchange', ['This', '%0.nm','View', '%1.nm','Info', '%2.nm'], true));
  FOnViewchange := Value;
end;

procedure TExtensibleCalendarPanel.SetGoText(const AValue: string);
begin
  FGoText := AValue;
  ExtSession.ResponseItems.SetConfigItem(Self, 'goText', [AValue]);
end;

procedure TExtensibleCalendarPanel.SetJumpToText(const AValue: string);
begin
  FJumpToText := AValue;
  ExtSession.ResponseItems.SetConfigItem(Self, 'jumpToText', [AValue]);
end;

class function TExtensibleCalendarPanel.JSClassName : string; begin
  Result := 'Ext.ensible.cal.CalendarPanel';
end;

function TExtensibleCalendarPanel.GetActiveView : TExtFunction; begin
  JSCode(JSName + '.getActiveView();', 'TExtensibleCalendarPanel');
  Result := Self;
end;

function TExtensibleCalendarPanel.HideEditForm : TExtFunction; begin
  JSCode(JSName + '.hideEditForm();', 'TExtensibleCalendarPanel');
  Result := Self;
end;

function TExtensibleCalendarPanel.SetStartDate(Dt : TDateTime) : TExtFunction; begin
  JSCode(JSName + '.setStartDate(' + VarToJSON([Dt]) + ');', 'TExtensibleCalendarPanel');
  Result := Self;
end;

function TExtensibleCalendarPanel.ShowEditForm(RecordJS : TExtCalendarEventRecord) : TExtFunction; begin
  JSCode(JSName + '.showEditForm(' + VarToJSON([RecordJS, false]) + ');', 'TExtensibleCalendarPanel');
  Result := Self;
end;

procedure TExtensibleCalendarPanel.HandleEvent(const AEvtName : string); begin
  inherited;
  if (AEvtName = 'datechange') and Assigned(FOnDatechange) then
    FOnDatechange(TExtensibleCalendarPanel(ParamAsObject('This')), ParamAsTDateTime('StartDate'), ParamAsTDateTime('ViewStart'), ParamAsTDateTime('ViewEnd'))
  else if (AEvtName = 'dayclick') and Assigned(FOnDayclick) then
    FOnDayclick(TExtensibleCalendarPanel(ParamAsObject('This')), ParamAsTDateTime('Dt'), ParamAsBoolean('Allday'), TExtElement(ParamAsObject('El')))
  else if (AEvtName = 'eventadd') and Assigned(FOnEventadd) then
    FOnEventadd(TExtensibleCalendarPanel(ParamAsObject('This')), TExtCalendarEventRecord(ParamAsObject('Rec')))
  else if (AEvtName = 'eventcancel') and Assigned(FOnEventcancel) then
    FOnEventcancel(TExtensibleCalendarPanel(ParamAsObject('This')), TExtCalendarEventRecord(ParamAsObject('Rec')))
  else if (AEvtName = 'eventclick') and Assigned(FOnEventclick) then
    FOnEventclick(TExtensibleCalendarPanel(ParamAsObject('This')), TExtCalendarEventRecord(ParamAsObject('Rec')), THTMLNode(ParamAsObject('El')))
  else if (AEvtName = 'eventdelete') and Assigned(FOnEventdelete) then
    FOnEventdelete(TExtensibleCalendarPanel(ParamAsObject('This')), TExtCalendarEventRecord(ParamAsObject('Rec')))
  else if (AEvtName = 'eventmove') and Assigned(FOnEventmove) then
    FOnEventmove(TExtensibleCalendarPanel(ParamAsObject('This')), TExtCalendarEventRecord(ParamAsObject('Rec')))
  else if (AEvtName = 'eventout') and Assigned(FOnEventout) then
    FOnEventout(TExtensibleCalendarPanel(ParamAsObject('This')), TExtCalendarEventRecord(ParamAsObject('Rec')), THTMLNode(ParamAsObject('El')))
  else if (AEvtName = 'eventover') and Assigned(FOnEventover) then
    FOnEventover(TExtensibleCalendarPanel(ParamAsObject('This')), TExtCalendarEventRecord(ParamAsObject('Rec')), THTMLNode(ParamAsObject('El')))
  else if (AEvtName = 'eventresize') and Assigned(FOnEventresize) then
    FOnEventresize(TExtensibleCalendarPanel(ParamAsObject('This')), TExtCalendarEventRecord(ParamAsObject('Rec')))
  else if (AEvtName = 'eventsrendered') and Assigned(FOnEventsrendered) then
    FOnEventsrendered(TExtensibleCalendarPanel(ParamAsObject('This')))
  else if (AEvtName = 'eventupdate') and Assigned(FOnEventupdate) then
    FOnEventupdate(TExtensibleCalendarPanel(ParamAsObject('This')), TExtCalendarEventRecord(ParamAsObject('Rec')))
  else if (AEvtName = 'initdrag') and Assigned(FOnInitdrag) then
    FOnInitdrag(TExtensibleCalendarPanel(ParamAsObject('This')))
  else if (AEvtName = 'rangeselect') and Assigned(FOnRangeselect) then
    FOnRangeselect(TExtensibleCalendarPanel(ParamAsObject('This')), TExtObject(ParamAsObject('Dates')), TExtFunction(ParamAsObject('Callback')))
  else if (AEvtName = 'viewchange') and Assigned(FOnViewchange) then
    FOnViewchange(TExtensibleCalendarPanel(ParamAsObject('This')), TExtCalendarView(ParamAsObject('View')), TExtObject(ParamAsObject('Info')));
end;

class function TExtCalendarPicker.JSClassName : string; begin
  Result := 'Ext.calendar.CalendarPicker';
end;

class function TExtCalendarDayHeaderTemplate.JSClassName : string; begin
  Result := 'Ext.calendar.DayHeaderTemplate';
end;

procedure TExtCalendarDayHeaderTemplate.SetFAddEventCls(Value : String); begin
  FAddEventCls := Value;
  JSCode('addEventCls:' + VarToJSON([Value]));
end;

procedure TExtCalendarDayHeaderTemplate.SetFDayCount(Value : Integer); begin
  FDayCount := Value;
  JSCode('dayCount:' + VarToJSON([Value]));
end;

procedure TExtCalendarDayHeaderTemplate.SetFDayText(Value : String); begin
  FDayText := Value;
  JSCode('dayText:' + VarToJSON([Value]));
end;

procedure TExtCalendarDayHeaderTemplate.SetFDdCreateEventText(Value : String); begin
  FDdCreateEventText := Value;
  JSCode('ddCreateEventText:' + VarToJSON([Value]));
end;

procedure TExtCalendarDayHeaderTemplate.SetFDdMoveEventText(Value : String); begin
  FDdMoveEventText := Value;
  JSCode('ddMoveEventText:' + VarToJSON([Value]));
end;

procedure TExtCalendarDayHeaderTemplate.SetFDdResizeEventText(Value : String); begin
  FDdResizeEventText := Value;
  JSCode('ddResizeEventText:' + VarToJSON([Value]));
end;

procedure TExtCalendarDayHeaderTemplate.SetFEnableAddFx(Value : Boolean); begin
  FEnableAddFx := Value;
  JSCode('enableAddFx:' + VarToJSON([Value]));
end;

procedure TExtCalendarDayHeaderTemplate.SetFEnableDD(Value : Boolean); begin
  FEnableDD := Value;
  JSCode('enableDD:' + VarToJSON([Value]));
end;

procedure TExtCalendarDayHeaderTemplate.SetFEnableFx(Value : Boolean); begin
  FEnableFx := Value;
  JSCode('enableFx:' + VarToJSON([Value]));
end;

procedure TExtCalendarDayHeaderTemplate.SetFEnableRemoveFx(Value : Boolean); begin
  FEnableRemoveFx := Value;
  JSCode('enableRemoveFx:' + VarToJSON([Value]));
end;

procedure TExtCalendarDayHeaderTemplate.SetFEnableUpdateFx(Value : Boolean); begin
  FEnableUpdateFx := Value;
  JSCode('enableUpdateFx:' + VarToJSON([Value]));
end;

procedure TExtCalendarDayHeaderTemplate.SetFMonitorResize(Value : Boolean); begin
  FMonitorResize := Value;
  JSCode('monitorResize:' + VarToJSON([Value]));
end;

procedure TExtCalendarDayHeaderTemplate.SetFMonthText(Value : String); begin
  FMonthText := Value;
  JSCode('monthText:' + VarToJSON([Value]));
end;

procedure TExtCalendarDayHeaderTemplate.SetFMoveEventCls(Value : String); begin
  FMoveEventCls := Value;
  JSCode('moveEventCls:' + VarToJSON([Value]));
end;

procedure TExtCalendarDayHeaderTemplate.SetShowDayView(Value : Boolean); begin
  FShowDayView := Value;
  JSCode('showDayView:' + VarToJSON([Value]));
end;

procedure TExtCalendarDayHeaderTemplate.SetFShowHeader(Value : Boolean); begin
  FShowHeader := Value;
  JSCode('showHeader:' + VarToJSON([Value]));
end;

procedure TExtCalendarDayHeaderTemplate.SetFShowMonthView(Value : Boolean); begin
  FShowMonthView := Value;
  JSCode('showMonthView:' + VarToJSON([Value]));
end;

procedure TExtCalendarDayHeaderTemplate.SetFShowNavBar(Value : Boolean); begin
  FShowNavBar := Value;
  JSCode('showNavBar:' + VarToJSON([Value]));
end;

procedure TExtCalendarDayHeaderTemplate.SetFShowTime(Value : Boolean); begin
  FShowTime := Value;
  JSCode('showTime:' + VarToJSON([Value]));
end;

procedure TExtCalendarDayHeaderTemplate.SetFShowTodayText(Value : Boolean); begin
  FShowTodayText := Value;
  JSCode('showTodayText:' + VarToJSON([Value]));
end;

procedure TExtCalendarDayHeaderTemplate.SetFShowWeekLinks(Value : Boolean); begin
  FShowWeekLinks := Value;
  JSCode('showWeekLinks:' + VarToJSON([Value]));
end;

procedure TExtCalendarDayHeaderTemplate.SetFShowWeekNumbers(Value : Boolean); begin
  FShowWeekNumbers := Value;
  JSCode('showWeekNumbers:' + VarToJSON([Value]));
end;

procedure TExtCalendarDayHeaderTemplate.SetFShowWeekView(Value : Boolean); begin
  FShowWeekView := Value;
  JSCode('showWeekView:' + VarToJSON([Value]));
end;

procedure TExtCalendarDayHeaderTemplate.SetFSpansHavePriority(Value : Boolean); begin
  FSpansHavePriority := Value;
  JSCode('spansHavePriority:' + VarToJSON([Value]));
end;

procedure TExtCalendarDayHeaderTemplate.SetFStartDay(Value : Integer); begin
  FStartDay := Value;
  JSCode('startDay:' + VarToJSON([Value]));
end;

procedure TExtCalendarDayHeaderTemplate.SetFTodayText(Value : String); begin
  FTodayText := Value;
  JSCode('todayText:' + VarToJSON([Value]));
end;

procedure TExtCalendarDayHeaderTemplate.SetFToText(Value : String); begin
  FToText := Value;
  JSCode('toText:' + VarToJSON([Value]));
end;

procedure TExtCalendarDayHeaderTemplate.SetFTrackMouseOver(Value : Boolean); begin
  FTrackMouseOver := Value;
  JSCode('trackMouseOver:' + VarToJSON([Value]));
end;

procedure TExtCalendarDayHeaderTemplate.SetFWeekLinkOverClass(Value : String); begin
  FWeekLinkOverClass := Value;
  JSCode('weekLinkOverClass:' + VarToJSON([Value]));
end;

procedure TExtCalendarDayHeaderTemplate.SetFWeekText(Value : String); begin
  FWeekText := Value;
  JSCode('weekText:' + VarToJSON([Value]));
end;

procedure TExtCalendarDayHeaderTemplate.SetFOnDatechange(Value : TExtCalendarDayHeaderTemplateOnDatechange); begin
  if Assigned(FOnDatechange) then
    JSCode(JSName+'.events ["datechange"].listeners=[];');
  if Assigned(Value) then
    AddListener('datechange', Ajax('datechange', ['This', '%0.nm','StartDate', '%1','ViewStart', '%2','ViewEnd', '%3'], true));
  FOnDatechange := Value;
end;

procedure TExtCalendarDayHeaderTemplate.SetFOnDayclick(Value : TExtCalendarDayHeaderTemplateOnDayclick); begin
  if Assigned(FOnDayclick) then
    JSCode(JSName+'.events ["dayclick"].listeners=[];');
  if Assigned(Value) then
    AddListener('dayclick', Ajax('dayclick', ['This', '%0.nm','Dt', '%1','Allday', '%2','El', '%3.nm'], true));
  FOnDayclick := Value;
end;

procedure TExtCalendarDayHeaderTemplate.SetFOnDayout(Value : TExtCalendarDayHeaderTemplateOnDayout); begin
  if Assigned(FOnDayout) then
    JSCode(JSName+'.events ["dayout"].listeners=[];');
  if Assigned(Value) then
    AddListener('dayout', Ajax('dayout', ['This', '%0.nm','Dt', '%1','El', '%2.nm'], true));
  FOnDayout := Value;
end;

procedure TExtCalendarDayHeaderTemplate.SetFOnDayover(Value : TExtCalendarDayHeaderTemplateOnDayover); begin
  if Assigned(FOnDayover) then
    JSCode(JSName+'.events ["dayover"].listeners=[];');
  if Assigned(Value) then
    AddListener('dayover', Ajax('dayover', ['This', '%0.nm','Dt', '%1','El', '%2.nm'], true));
  FOnDayover := Value;
end;

procedure TExtCalendarDayHeaderTemplate.SetFOnEditdetails(Value : TExtCalendarDayHeaderTemplateOnEditdetails); begin
  if Assigned(FOnEditdetails) then
    JSCode(JSName+'.events ["editdetails"].listeners=[];');
  if Assigned(Value) then
    AddListener('editdetails', Ajax('editdetails', ['This', '%0.nm','Rec', '%1.nm'], true));
  FOnEditdetails := Value;
end;

procedure TExtCalendarDayHeaderTemplate.SetFOnEventadd(Value : TExtCalendarDayHeaderTemplateOnEventadd); begin
  if Assigned(FOnEventadd) then
    JSCode(JSName+'.events ["eventadd"].listeners=[];');
  if Assigned(Value) then
    AddListener('eventadd', Ajax('eventadd', ['This', '%0.nm','Rec', '%1.nm'], true));
  FOnEventadd := Value;
end;

procedure TExtCalendarDayHeaderTemplate.SetFOnEventcancel(Value : TExtCalendarDayHeaderTemplateOnEventcancel); begin
  if Assigned(FOnEventcancel) then
    JSCode(JSName+'.events ["eventcancel"].listeners=[];');
  if Assigned(Value) then
    AddListener('eventcancel', Ajax('eventcancel', ['This', '%0.nm','Rec', '%1.nm'], true));
  FOnEventcancel := Value;
end;

procedure TExtCalendarDayHeaderTemplate.SetFOnEventclick(Value : TExtCalendarDayHeaderTemplateOnEventclick); begin
  if Assigned(FOnEventclick) then
    JSCode(JSName+'.events ["eventclick"].listeners=[];');
  if Assigned(Value) then
    AddListener('eventclick', Ajax('eventclick', ['This', '%0.nm','Rec', '%1.nm','El', '%2.nm'], true));
  FOnEventclick := Value;
end;

procedure TExtCalendarDayHeaderTemplate.SetFOnEventdelete(Value : TExtCalendarDayHeaderTemplateOnEventdelete); begin
  if Assigned(FOnEventdelete) then
    JSCode(JSName+'.events ["eventdelete"].listeners=[];');
  if Assigned(Value) then
    AddListener('eventdelete', Ajax('eventdelete', ['This', '%0.nm','Rec', '%1.nm'], true));
  FOnEventdelete := Value;
end;

procedure TExtCalendarDayHeaderTemplate.SetFOnEventmove(Value : TExtCalendarDayHeaderTemplateOnEventmove); begin
  if Assigned(FOnEventmove) then
    JSCode(JSName+'.events ["eventmove"].listeners=[];');
  if Assigned(Value) then
    AddListener('eventmove', Ajax('eventmove', ['This', '%0.nm','Rec', '%1.nm'], true));
  FOnEventmove := Value;
end;

procedure TExtCalendarDayHeaderTemplate.SetFOnEventout(Value : TExtCalendarDayHeaderTemplateOnEventout); begin
  if Assigned(FOnEventout) then
    JSCode(JSName+'.events ["eventout"].listeners=[];');
  if Assigned(Value) then
    AddListener('eventout', Ajax('eventout', ['This', '%0.nm','Rec', '%1.nm','El', '%2.nm'], true));
  FOnEventout := Value;
end;

procedure TExtCalendarDayHeaderTemplate.SetFOnEventover(Value : TExtCalendarDayHeaderTemplateOnEventover); begin
  if Assigned(FOnEventover) then
    JSCode(JSName+'.events ["eventover"].listeners=[];');
  if Assigned(Value) then
    AddListener('eventover', Ajax('eventover', ['This', '%0.nm','Rec', '%1.nm','El', '%2.nm'], true));
  FOnEventover := Value;
end;

procedure TExtCalendarDayHeaderTemplate.SetFOnEventresize(Value : TExtCalendarDayHeaderTemplateOnEventresize); begin
  if Assigned(FOnEventresize) then
    JSCode(JSName+'.events ["eventresize"].listeners=[];');
  if Assigned(Value) then
    AddListener('eventresize', Ajax('eventresize', ['This', '%0.nm','Rec', '%1.nm'], true));
  FOnEventresize := Value;
end;

procedure TExtCalendarDayHeaderTemplate.SetFOnEventsrendered(Value : TExtCalendarDayHeaderTemplateOnEventsrendered); begin
  if Assigned(FOnEventsrendered) then
    JSCode(JSName+'.events ["eventsrendered"].listeners=[];');
  if Assigned(Value) then
    AddListener('eventsrendered', Ajax('eventsrendered', ['This', '%0.nm'], true));
  FOnEventsrendered := Value;
end;

procedure TExtCalendarDayHeaderTemplate.SetFOnEventupdate(Value : TExtCalendarDayHeaderTemplateOnEventupdate); begin
  if Assigned(FOnEventupdate) then
    JSCode(JSName+'.events ["eventupdate"].listeners=[];');
  if Assigned(Value) then
    AddListener('eventupdate', Ajax('eventupdate', ['This', '%0.nm','Rec', '%1.nm'], true));
  FOnEventupdate := Value;
end;

procedure TExtCalendarDayHeaderTemplate.SetFOnInitdrag(Value : TExtCalendarDayHeaderTemplateOnInitdrag); begin
  if Assigned(FOnInitdrag) then
    JSCode(JSName+'.events ["initdrag"].listeners=[];');
  if Assigned(Value) then
    AddListener('initdrag', Ajax('initdrag', ['This', '%0.nm'], true));
  FOnInitdrag := Value;
end;

procedure TExtCalendarDayHeaderTemplate.SetFOnRangeselect(Value : TExtCalendarDayHeaderTemplateOnRangeselect); begin
  if Assigned(FOnRangeselect) then
    JSCode(JSName+'.events ["rangeselect"].listeners=[];');
  if Assigned(Value) then
    AddListener('rangeselect', Ajax('rangeselect', ['This', '%0.nm','Dates', '%1.nm','Callback', '%2.nm'], true));
  FOnRangeselect := Value;
end;

procedure TExtCalendarDayHeaderTemplate.SetFOnViewchange(Value : TExtCalendarDayHeaderTemplateOnViewchange); begin
  if Assigned(FOnViewchange) then
    JSCode(JSName+'.events ["viewchange"].listeners=[];');
  if Assigned(Value) then
    AddListener('viewchange', Ajax('viewchange', ['This', '%0.nm','View', '%1.nm','Info', '%2.nm'], true));
  FOnViewchange := Value;
end;

procedure TExtCalendarDayHeaderTemplate.SetFOnWeekclick(Value : TExtCalendarDayHeaderTemplateOnWeekclick); begin
  if Assigned(FOnWeekclick) then
    JSCode(JSName+'.events ["weekclick"].listeners=[];');
  if Assigned(Value) then
    AddListener('weekclick', Ajax('weekclick', ['This', '%0.nm','Dt', '%1'], true));
  FOnWeekclick := Value;
end;

function TExtCalendarDayHeaderTemplate.GetActiveView : TExtFunction; begin
  JSCode(JSName + '.getActiveView();', 'TExtCalendarDayHeaderTemplate');
  Result := Self;
end;

function TExtCalendarDayHeaderTemplate.GetEventAllDayTemplate : TExtFunction; begin
  JSCode(JSName + '.getEventAllDayTemplate();', 'TExtCalendarDayHeaderTemplate');
  Result := Self;
end;

function TExtCalendarDayHeaderTemplate.GetEventEls(EventId : String) : TExtFunction; begin
  JSCode(JSName + '.getEventEls(' + VarToJSON([EventId]) + ');', 'TExtCalendarDayHeaderTemplate');
  Result := Self;
end;

function TExtCalendarDayHeaderTemplate.GetEventIdFromEl(El : THTMLElement) : TExtFunction; begin
  JSCode(JSName + '.GetEventIdFromEl(' + VarToJSON([El, false]) + ');', 'TExtCalendarDayHeaderTemplate');
  Result := Self;
end;

function TExtCalendarDayHeaderTemplate.GetEventIdFromEl(El : String) : TExtFunction; begin
  JSCode(JSName + '.getEventIdFromEl(' + VarToJSON([El]) + ');', 'TExtCalendarDayHeaderTemplate');
  Result := Self;
end;

function TExtCalendarDayHeaderTemplate.GetEventSelectorCls(EventId : String; ForSelect : Boolean) : TExtFunction; begin
  JSCode(JSName + '.getEventSelectorCls(' + VarToJSON([EventId, ForSelect]) + ');', 'TExtCalendarDayHeaderTemplate');
  Result := Self;
end;

function TExtCalendarDayHeaderTemplate.GetStartDate : TExtFunction; begin
  JSCode(JSName + '.getStartDate();', 'TExtCalendarDayHeaderTemplate');
  Result := Self;
end;

function TExtCalendarDayHeaderTemplate.GetValue : TExtFunction; begin
  JSCode(JSName + '.getValue();', 'TExtCalendarDayHeaderTemplate');
  Result := Self;
end;

function TExtCalendarDayHeaderTemplate.HideEditForm : TExtFunction; begin
  JSCode(JSName + '.hideEditForm();', 'TExtCalendarDayHeaderTemplate');
  Result := Self;
end;

function TExtCalendarDayHeaderTemplate.HighlightEvent(Els : TExtCompositeElement; Color : TExtObject = nil; O : TExtObject = nil) : TExtFunction; begin
  JSCode(JSName + '.highlightEvent(' + VarToJSON([Els, false, Color, false, O, false]) + ');', 'TExtCalendarDayHeaderTemplate');
  Result := Self;
end;

function TExtCalendarDayHeaderTemplate.IsToday : TExtFunction; begin
  JSCode(JSName + '.isToday();', 'TExtCalendarDayHeaderTemplate');
  Result := Self;
end;

function TExtCalendarDayHeaderTemplate.MoveDays(Value : Integer) : TExtFunction; begin
  JSCode(JSName + '.moveDays(' + VarToJSON([Value]) + ');', 'TExtCalendarDayHeaderTemplate');
  Result := Self;
end;

function TExtCalendarDayHeaderTemplate.MoveMonths(Value : Integer) : TExtFunction; begin
  JSCode(JSName + '.moveMonths(' + VarToJSON([Value]) + ');', 'TExtCalendarDayHeaderTemplate');
  Result := Self;
end;

function TExtCalendarDayHeaderTemplate.MoveNext : TExtFunction; begin
  JSCode(JSName + '.moveNext();', 'TExtCalendarDayHeaderTemplate');
  Result := Self;
end;

function TExtCalendarDayHeaderTemplate.MovePrev : TExtFunction; begin
  JSCode(JSName + '.movePrev();', 'TExtCalendarDayHeaderTemplate');
  Result := Self;
end;

function TExtCalendarDayHeaderTemplate.MoveTo(Dt : TDateTime) : TExtFunction; begin
  JSCode(JSName + '.moveTo(' + VarToJSON([Dt]) + ');', 'TExtCalendarDayHeaderTemplate');
  Result := Self;
end;

function TExtCalendarDayHeaderTemplate.MoveToday : TExtFunction; begin
  JSCode(JSName + '.moveToday();', 'TExtCalendarDayHeaderTemplate');
  Result := Self;
end;

function TExtCalendarDayHeaderTemplate.MoveWeeks(Value : Integer) : TExtFunction; begin
  JSCode(JSName + '.moveWeeks(' + VarToJSON([Value]) + ');', 'TExtCalendarDayHeaderTemplate');
  Result := Self;
end;

function TExtCalendarDayHeaderTemplate.ScrollTo(Y : Integer; Defer : Boolean = false) : TExtFunction; begin
  JSCode(JSName + '.scrollTo(' + VarToJSON([Y, Defer]) + ');', 'TExtCalendarDayHeaderTemplate');
  Result := Self;
end;

function TExtCalendarDayHeaderTemplate.SetStartDate(Dt : TDateTime) : TExtFunction; begin
  JSCode(JSName + '.setStartDate(' + VarToJSON([Dt]) + ');', 'TExtCalendarDayHeaderTemplate');
  Result := Self;
end;

function TExtCalendarDayHeaderTemplate.SetStore(Store : TExtDataStore) : TExtFunction; begin
  JSCode(JSName + '.setStore(' + VarToJSON([Store, false]) + ');', 'TExtCalendarDayHeaderTemplate');
  Result := Self;
end;

function TExtCalendarDayHeaderTemplate.SetValue(V : TExtObjectList) : TExtFunction; begin
  JSCode(JSName + '.SetValue(' + VarToJSON(V) + ');', 'TExtCalendarDayHeaderTemplate');
  Result := Self;
end;

function TExtCalendarDayHeaderTemplate.SetValue(V : TExtObject) : TExtFunction; begin
  JSCode(JSName + '.SetValue(' + VarToJSON([V, false]) + ');', 'TExtCalendarDayHeaderTemplate');
  Result := Self;
end;

function TExtCalendarDayHeaderTemplate.SetValue(V : TDateTime) : TExtFunction; begin
  JSCode(JSName + '.setValue(' + VarToJSON([V]) + ');', 'TExtCalendarDayHeaderTemplate');
  Result := Self;
end;

function TExtCalendarDayHeaderTemplate.Show(O : TExtObject; AnimateTarget : String = '') : TExtFunction; begin
  JSCode(JSName + '.Show(' + VarToJSON([O, false, AnimateTarget]) + ');', 'TExtCalendarDayHeaderTemplate');
  Result := Self;
end;

function TExtCalendarDayHeaderTemplate.Show(O : TExtDataRecord; AnimateTarget : String = '') : TExtFunction; begin
  JSCode(JSName + '.show(' + VarToJSON([O, false, AnimateTarget]) + ');', 'TExtCalendarDayHeaderTemplate');
  Result := Self;
end;

function TExtCalendarDayHeaderTemplate.ShowEditForm(RecordJS : TExtCalendarEventRecord) : TExtFunction; begin
  JSCode(JSName + '.showEditForm(' + VarToJSON([RecordJS, false]) + ');', 'TExtCalendarDayHeaderTemplate');
  Result := Self;
end;

function TExtCalendarDayHeaderTemplate.Unregister(El : TExtObjectList) : TExtFunction; begin
  JSCode(JSName + '.Unregister(' + VarToJSON(El) + ');', 'TExtCalendarDayHeaderTemplate');
  Result := Self;
end;

function TExtCalendarDayHeaderTemplate.Unregister(El : String) : TExtFunction; begin
  JSCode(JSName + '.unregister(' + VarToJSON([El]) + ');', 'TExtCalendarDayHeaderTemplate');
  Result := Self;
end;

function TExtCalendarDayHeaderTemplate.UpdateMsg(Msg : String) : TExtFunction; begin
  JSCode(JSName + '.updateMsg(' + VarToJSON([Msg]) + ');', 'TExtCalendarDayHeaderTemplate');
  Result := Self;
end;

procedure TExtCalendarDayHeaderTemplate.HandleEvent(const AEvtName : string); begin
  inherited;
  if (AEvtName = 'datechange') and Assigned(FOnDatechange) then
    FOnDatechange(TExtCalendarDayHeaderTemplate(ParamAsObject('This')), ParamAsTDateTime('StartDate'), ParamAsTDateTime('ViewStart'), ParamAsTDateTime('ViewEnd'))
  else if (AEvtName = 'dayclick') and Assigned(FOnDayclick) then
    FOnDayclick(TExtCalendarDayHeaderTemplate(ParamAsObject('This')), ParamAsTDateTime('Dt'), ParamAsBoolean('Allday'), TExtElement(ParamAsObject('El')))
  else if (AEvtName = 'dayout') and Assigned(FOnDayout) then
    FOnDayout(TExtCalendarDayHeaderTemplate(ParamAsObject('This')),ParamAsTDateTime('Dt'), TExtElement(ParamAsObject('El')))
  else if (AEvtName = 'dayover') and Assigned(FOnDayover) then
    FOnDayover(TExtCalendarDayHeaderTemplate(ParamAsObject('This')),ParamAsTDateTime('Dt'), TExtElement(ParamAsObject('El')))
  else if (AEvtName = 'editdetails') and Assigned(FOnEditdetails) then
    FOnEditdetails(TExtCalendarDayHeaderTemplate(ParamAsObject('This')),TExtCalendarEventRecord(ParamAsObject('Rec')))
  else if (AEvtName = 'eventadd') and Assigned(FOnEventadd) then
    FOnEventadd(TExtCalendarDayHeaderTemplate(ParamAsObject('This')),TExtCalendarEventRecord(ParamAsObject('Rec')))
  else if (AEvtName = 'eventcancel') and Assigned(FOnEventcancel) then
    FOnEventcancel(TExtCalendarDayHeaderTemplate(ParamAsObject('This')),TExtCalendarEventRecord(ParamAsObject('Rec')))
  else if (AEvtName = 'eventclick') and Assigned(FOnEventclick) then
    FOnEventclick(TExtCalendarDayHeaderTemplate(ParamAsObject('This')),TExtCalendarEventRecord(ParamAsObject('Rec')), THTMLNode(ParamAsObject('El')))
  else if (AEvtName = 'eventdelete') and Assigned(FOnEventdelete) then
    FOnEventdelete(TExtCalendarDayHeaderTemplate(ParamAsObject('This')),TExtCalendarEventRecord(ParamAsObject('Rec')))
  else if (AEvtName = 'eventmove') and Assigned(FOnEventmove) then
    FOnEventmove(TExtCalendarDayHeaderTemplate(ParamAsObject('This')),TExtCalendarEventRecord(ParamAsObject('Rec')))
  else if (AEvtName = 'eventout') and Assigned(FOnEventout) then
    FOnEventout(TExtCalendarDayHeaderTemplate(ParamAsObject('This')),TExtCalendarEventRecord(ParamAsObject('Rec')), THTMLNode(ParamAsObject('El')))
  else if (AEvtName = 'eventover') and Assigned(FOnEventover) then
    FOnEventover(TExtCalendarDayHeaderTemplate(ParamAsObject('This')),TExtCalendarEventRecord(ParamAsObject('Rec')), THTMLNode(ParamAsObject('El')))
  else if (AEvtName = 'eventresize') and Assigned(FOnEventresize) then
    FOnEventresize(TExtCalendarDayHeaderTemplate(ParamAsObject('This')),TExtCalendarEventRecord(ParamAsObject('Rec')))
  else if (AEvtName = 'eventsrendered') and Assigned(FOnEventsrendered) then
    FOnEventsrendered(TExtCalendarDayHeaderTemplate(ParamAsObject('This')))
  else if (AEvtName = 'eventupdate') and Assigned(FOnEventupdate) then
    FOnEventupdate(TExtCalendarDayHeaderTemplate(ParamAsObject('This')),TExtCalendarEventRecord(ParamAsObject('Rec')))
  else if (AEvtName = 'initdrag') and Assigned(FOnInitdrag) then
    FOnInitdrag(TExtCalendarDayHeaderTemplate(ParamAsObject('This')))
  else if (AEvtName = 'rangeselect') and Assigned(FOnRangeselect) then
    FOnRangeselect(TExtCalendarDayHeaderTemplate(ParamAsObject('This')),TExtObject(ParamAsObject('Dates')), TExtFunction(ParamAsObject('Callback')))
  else if (AEvtName = 'viewchange') and Assigned(FOnViewchange) then
    FOnViewchange(TExtCalendarDayHeaderTemplate(ParamAsObject('This')),TExtCalendarView(ParamAsObject('View')), TExtObject(ParamAsObject('Info')))
  else if (AEvtName = 'weekclick') and Assigned(FOnWeekclick) then
    FOnWeekclick(TExtCalendarDayHeaderTemplate(ParamAsObject('This')),ParamAsTDateTime('Dt'));
end;

class function TExtCalendarDayBodyTemplate.JSClassName : string; begin
  Result := 'Ext.calendar.DayBodyTemplate';
end;

procedure TExtCalendarDayBodyView.SetFOnDayclick(Value : TExtCalendarDayBodyViewOnDayclick); begin
  if Assigned(FOnDayclick) then
    JSCode(JSName+'.events ["dayclick"].listeners=[];');
  if Assigned(Value) then
    AddListener('dayclick', Ajax('dayclick', ['This', '%0.nm','Dt', '%1','Allday', '%2','El', '%3.nm'], true));
  FOnDayclick := Value;
end;

procedure TExtCalendarDayBodyView.SetFOnEventresize(Value : TExtCalendarDayBodyViewOnEventresize); begin
  if Assigned(FOnEventresize) then
    JSCode(JSName+'.events ["eventresize"].listeners=[];');
  if Assigned(Value) then
    AddListener('eventresize', Ajax('eventresize', ['This', '%0.nm','Rec', '%1.nm'], true));
  FOnEventresize := Value;
end;

class function TExtCalendarDayBodyView.JSClassName : string; begin
  Result := 'Ext.calendar.DayBodyView';
end;

function TExtCalendarDayBodyView.GetEventAllDayTemplate : TExtFunction; begin
  JSCode(JSName + '.getEventAllDayTemplate();', 'TExtCalendarDayBodyView');
  Result := Self;
end;

function TExtCalendarDayBodyView.ScrollTo(Y : Integer; Defer : Boolean = false) : TExtFunction; begin
  JSCode(JSName + '.scrollTo(' + VarToJSON([Y, Defer]) + ');', 'TExtCalendarDayBodyView');
  Result := Self;
end;

procedure TExtCalendarDayBodyView.HandleEvent(const AEvtName : string); begin
  inherited;
  if (AEvtName = 'dayclick') and Assigned(FOnDayclick) then
    FOnDayclick(TExtCalendarDayBodyView(ParamAsObject('This')), ParamAsTDateTime('Dt'), ParamAsBoolean('Allday'), TExtElement(ParamAsObject('El')))
  else if (AEvtName = 'eventresize') and Assigned(FOnEventresize) then
    FOnEventresize(TExtCalendarDayBodyView(ParamAsObject('This')), TExtCalendarEventRecord(ParamAsObject('Rec')));
end;

class function TExtCalendarBoxLayoutTemplate.JSClassName : string; begin
  Result := 'Ext.calendar.BoxLayoutTemplate';
end;

procedure TExtCalendarDayHeaderView.SetFOnDayclick(Value : TExtCalendarDayHeaderViewOnDayclick); begin
  if Assigned(FOnDayclick) then
    JSCode(JSName+'.events ["dayclick"].listeners=[];');
  if Assigned(Value) then
    AddListener('dayclick', Ajax('dayclick', ['This', '%0.nm','Dt', '%1','Allday', '%2','El', '%3.nm'], true));
  FOnDayclick := Value;
end;

class function TExtCalendarDayHeaderView.JSClassName : string; begin
  Result := 'Ext.calendar.DayHeaderView';
end;

procedure TExtCalendarDayHeaderView.HandleEvent(const AEvtName : string); begin
  inherited;
  if (AEvtName = 'dayclick') and Assigned(FOnDayclick) then
    FOnDayclick(TExtCalendarDayHeaderView(ParamAsObject('This')), ParamAsTDateTime('Dt'), ParamAsBoolean('Allday'), TExtElement(ParamAsObject('El')));
end;

procedure TExtCalendarDayView.SetFDayCount(Value : Integer); begin
  FDayCount := Value;
  JSCode('dayCount:' + VarToJSON([Value]));
end;

procedure TExtCalendarDayView.SetFDdCreateEventText(Value : String); begin
  FDdCreateEventText := Value;
  JSCode('ddCreateEventText:' + VarToJSON([Value]));
end;

procedure TExtCalendarDayView.SetFDdMoveEventText(Value : String); begin
  FDdMoveEventText := Value;
  JSCode('ddMoveEventText:' + VarToJSON([Value]));
end;

procedure TExtCalendarDayView.SetFShowTime(Value : Boolean); begin
  FShowTime := Value;
  JSCode('showTime:' + VarToJSON([Value]));
end;

procedure TExtCalendarDayView.SetFShowTodayText(Value : Boolean); begin
  FShowTodayText := Value;
  JSCode('showTodayText:' + VarToJSON([Value]));
end;

procedure TExtCalendarDayView.SetFTodayText(Value : String); begin
  FTodayText := Value;
  JSCode('todayText:' + VarToJSON([Value]));
end;

class function TExtCalendarDayView.JSClassName : string; begin
  Result := 'Ext.calendar.DayView';
end;

function TExtCalendarDayView.GetStartDate : TExtFunction; begin
  JSCode(JSName + '.getStartDate();', 'TExtCalendarDayView');
  Result := Self;
end;

function TExtCalendarDayView.IsToday : TExtFunction; begin
  JSCode(JSName + '.isToday();', 'TExtCalendarDayView');
  Result := Self;
end;

function TExtCalendarDayView.MoveDays(Value : Integer) : TExtFunction; begin
  JSCode(JSName + '.moveDays(' + VarToJSON([Value]) + ');', 'TExtCalendarDayView');
  Result := Self;
end;

function TExtCalendarDayView.MoveNext : TExtFunction; begin
  JSCode(JSName + '.moveNext();', 'TExtCalendarDayView');
  Result := Self;
end;

function TExtCalendarDayView.MovePrev : TExtFunction; begin
  JSCode(JSName + '.movePrev();', 'TExtCalendarDayView');
  Result := Self;
end;

function TExtCalendarDayView.MoveTo(Dt : TDateTime) : TExtFunction; begin
  JSCode(JSName + '.moveTo(' + VarToJSON([Dt]) + ');', 'TExtCalendarDayView');
  Result := Self;
end;

function TExtCalendarDayView.MoveToday : TExtFunction; begin
  JSCode(JSName + '.moveToday();', 'TExtCalendarDayView');
  Result := Self;
end;

function TExtCalendarDayView.SetStartDate(Dt : TDateTime) : TExtFunction; begin
  JSCode(JSName + '.setStartDate(' + VarToJSON([Dt]) + ');', 'TExtCalendarDayView');
  Result := Self;
end;

procedure TExtCalendarMonthView.SetFShowHeader(Value : Boolean); begin
  FShowHeader := Value;
  JSCode('showHeader:' + VarToJSON([Value]));
end;

procedure TExtCalendarMonthView.SetFShowTime(Value : Boolean); begin
  FShowTime := Value;
  JSCode('showTime:' + VarToJSON([Value]));
end;

procedure TExtCalendarMonthView.SetFShowTodayText(Value : Boolean); begin
  FShowTodayText := Value;
  JSCode('showTodayText:' + VarToJSON([Value]));
end;

procedure TExtCalendarMonthView.SetFShowWeekLinks(Value : Boolean); begin
  FShowWeekLinks := Value;
  JSCode('showWeekLinks:' + VarToJSON([Value]));
end;

procedure TExtCalendarMonthView.SetFShowWeekNumbers(Value : Boolean); begin
  FShowWeekNumbers := Value;
  JSCode('showWeekNumbers:' + VarToJSON([Value]));
end;

procedure TExtCalendarMonthView.SetFTodayText(Value : String); begin
  FTodayText := Value;
  JSCode('todayText:' + VarToJSON([Value]));
end;

procedure TExtCalendarMonthView.SetFWeekLinkOverClass(Value : String); begin
  FWeekLinkOverClass := Value;
  JSCode('weekLinkOverClass:' + VarToJSON([Value]));
end;

procedure TExtCalendarMonthView.SetFOnDayclick(Value : TExtCalendarMonthViewOnDayclick); begin
  if Assigned(FOnDayclick) then
    JSCode(JSName+'.events ["dayclick"].listeners=[];');
  if Assigned(Value) then
    AddListener('dayclick', Ajax('dayclick', ['This', '%0.nm','Dt', '%1','Allday', '%2','El', '%3.nm'], true));
  FOnDayclick := Value;
end;

procedure TExtCalendarMonthView.SetFOnWeekclick(Value : TExtCalendarMonthViewOnWeekclick); begin
  if Assigned(FOnWeekclick) then
    JSCode(JSName+'.events ["weekclick"].listeners=[];');
  if Assigned(Value) then
    AddListener('weekclick', Ajax('weekclick', ['This', '%0.nm','Dt', '%1'], true));
  FOnWeekclick := Value;
end;

class function TExtCalendarMonthView.JSClassName : string; begin
  Result := 'Ext.calendar.MonthView';
end;

procedure TExtCalendarMonthView.HandleEvent(const AEvtName : string); begin
  inherited;
  if (AEvtName = 'dayclick') and Assigned(FOnDayclick) then
    FOnDayclick(TExtCalendarMonthView(ParamAsObject('This')), ParamAsTDateTime('Dt'), ParamAsBoolean('Allday'), TExtElement(ParamAsObject('El')))
  else if (AEvtName = 'weekclick') and Assigned(FOnWeekclick) then
    FOnWeekclick(TExtCalendarMonthView(ParamAsObject('This')), ParamAsTDateTime('Dt'));
end;

class function TExtCalendarMonthViewTemplate.JSClassName : string; begin
  Result := 'Ext.calendar.MonthViewTemplate';
end;

procedure TExtCalendarEventEditWindow.SetFOnEditdetails(Value : TExtCalendarEventEditWindowOnEditdetails); begin
  if Assigned(FOnEditdetails) then
    JSCode(JSName+'.events ["editdetails"].listeners=[];');
  if Assigned(Value) then
    AddListener('editdetails', Ajax('editdetails', ['This', '%0.nm','Rec', '%1.nm'], true));
  FOnEditdetails := Value;
end;

procedure TExtCalendarEventEditWindow.SetFOnEventadd(Value : TExtCalendarEventEditWindowOnEventadd); begin
  if Assigned(FOnEventadd) then
    JSCode(JSName+'.events ["eventadd"].listeners=[];');
  if Assigned(Value) then
    AddListener('eventadd', Ajax('eventadd', ['This', '%0.nm','Rec', '%1.nm'], true));
  FOnEventadd := Value;
end;

procedure TExtCalendarEventEditWindow.SetFOnEventcancel(Value : TExtCalendarEventEditWindowOnEventcancel); begin
  if Assigned(FOnEventcancel) then
    JSCode(JSName+'.events ["eventcancel"].listeners=[];');
  if Assigned(Value) then
    AddListener('eventcancel', Ajax('eventcancel', ['This', '%0.nm','Rec', '%1.nm'], true));
  FOnEventcancel := Value;
end;

procedure TExtCalendarEventEditWindow.SetFOnEventdelete(Value : TExtCalendarEventEditWindowOnEventdelete); begin
  if Assigned(FOnEventdelete) then
    JSCode(JSName+'.events ["eventdelete"].listeners=[];');
  if Assigned(Value) then
    AddListener('eventdelete', Ajax('eventdelete', ['This', '%0.nm','Rec', '%1.nm'], true));
  FOnEventdelete := Value;
end;

procedure TExtCalendarEventEditWindow.SetFOnEventupdate(Value : TExtCalendarEventEditWindowOnEventupdate); begin
  if Assigned(FOnEventupdate) then
    JSCode(JSName+'.events ["eventupdate"].listeners=[];');
  if Assigned(Value) then
    AddListener('eventupdate', Ajax('eventupdate', ['This', '%0.nm','Rec', '%1.nm'], true));
  FOnEventupdate := Value;
end;

class function TExtCalendarEventEditWindow.JSClassName : string; begin
  Result := 'Ext.calendar.EventEditWindow';
end;

function TExtCalendarEventEditWindow.Show(O : TExtObject; AnimateTarget : String = '') : TExtFunction; begin
  JSCode(JSName + '.Show(' + VarToJSON([O, false, AnimateTarget]) + ');', 'TExtCalendarEventEditWindow');
  Result := Self;
end;

function TExtCalendarEventEditWindow.Show(O : TExtDataRecord; AnimateTarget : String = '') : TExtFunction; begin
  JSCode(JSName + '.show(' + VarToJSON([O, false, AnimateTarget]) + ');', 'TExtCalendarEventEditWindow');
  Result := Self;
end;

procedure TExtCalendarEventEditWindow.HandleEvent(const AEvtName : string); begin
  inherited;
  if (AEvtName = 'editdetails') and Assigned(FOnEditdetails) then
    FOnEditdetails(TExtCalendarEventEditWindow(ParamAsObject('This')), TExtCalendarEventRecord(ParamAsObject('Rec')))
  else if (AEvtName = 'eventadd') and Assigned(FOnEventadd) then
    FOnEventadd(TExtCalendarEventEditWindow(ParamAsObject('This')), TExtCalendarEventRecord(ParamAsObject('Rec')))
  else if (AEvtName = 'eventcancel') and Assigned(FOnEventcancel) then
    FOnEventcancel(TExtCalendarEventEditWindow(ParamAsObject('This')), TExtCalendarEventRecord(ParamAsObject('Rec')))
  else if (AEvtName = 'eventdelete') and Assigned(FOnEventdelete) then
    FOnEventdelete(TExtCalendarEventEditWindow(ParamAsObject('This')), TExtCalendarEventRecord(ParamAsObject('Rec')))
  else if (AEvtName = 'eventupdate') and Assigned(FOnEventupdate) then
    FOnEventupdate(TExtCalendarEventEditWindow(ParamAsObject('This')), TExtCalendarEventRecord(ParamAsObject('Rec')));
end;

class function TExtCalendarEventMappings.JSClassName : string; begin
  Result := 'Ext.calendar.EventMappings';
end;

procedure TExtCalendarStatusProxy.SetFAddEventCls(Value : String); begin
  FAddEventCls := Value;
  JSCode('addEventCls:' + VarToJSON([Value]));
end;

procedure TExtCalendarStatusProxy.SetFMoveEventCls(Value : String); begin
  FMoveEventCls := Value;
  JSCode('moveEventCls:' + VarToJSON([Value]));
end;

class function TExtCalendarStatusProxy.JSClassName : string; begin
  Result := 'Ext.calendar.StatusProxy';
end;

function TExtCalendarStatusProxy.UpdateMsg(Msg : String) : TExtFunction; begin
  JSCode(JSName + '.updateMsg(' + VarToJSON([Msg]) + ');', 'TExtCalendarStatusProxy');
  Result := Self;
end;

procedure TExtCalendarWeekView.SetFDayCount(Value : Integer); begin
  FDayCount := Value;
  JSCode('dayCount:' + VarToJSON([Value]));
end;

class function TExtCalendarWeekView.JSClassName : string; begin
  Result := 'Ext.calendar.WeekView';
end;

class function TExtCalendarReminderField.JSClassName : string; begin
  Result := 'Ext.calendar.ReminderField';
end;

class function TExtCalendarDayViewTemplate.JSClassName : string; begin
  Result := 'Ext.calendar.DayViewTemplate';
end;

procedure TExtCalendarEventEditForm.SetFOnEventadd(Value : TExtCalendarEventEditFormOnEventadd); begin
  if Assigned(FOnEventadd) then
    JSCode(JSName+'.events ["eventadd"].listeners=[];');
  if Assigned(Value) then
    AddListener('eventadd', Ajax('eventadd', ['This', '%0.nm','Rec', '%1.nm'], true));
  FOnEventadd := Value;
end;

procedure TExtCalendarEventEditForm.SetFOnEventcancel(Value : TExtCalendarEventEditFormOnEventcancel); begin
  if Assigned(FOnEventcancel) then
    JSCode(JSName+'.events ["eventcancel"].listeners=[];');
  if Assigned(Value) then
    AddListener('eventcancel', Ajax('eventcancel', ['This', '%0.nm','Rec', '%1.nm'], true));
  FOnEventcancel := Value;
end;

procedure TExtCalendarEventEditForm.SetFOnEventdelete(Value : TExtCalendarEventEditFormOnEventdelete); begin
  if Assigned(FOnEventdelete) then
    JSCode(JSName+'.events ["eventdelete"].listeners=[];');
  if Assigned(Value) then
    AddListener('eventdelete', Ajax('eventdelete', ['This', '%0.nm','Rec', '%1.nm'], true));
  FOnEventdelete := Value;
end;

procedure TExtCalendarEventEditForm.SetFOnEventupdate(Value : TExtCalendarEventEditFormOnEventupdate); begin
  if Assigned(FOnEventupdate) then
    JSCode(JSName+'.events ["eventupdate"].listeners=[];');
  if Assigned(Value) then
    AddListener('eventupdate', Ajax('eventupdate', ['This', '%0.nm','Rec', '%1.nm'], true));
  FOnEventupdate := Value;
end;

class function TExtCalendarEventEditForm.JSClassName : string; begin
  Result := 'Ext.calendar.EventEditForm';
end;

procedure TExtCalendarEventEditForm.HandleEvent(const AEvtName : string); begin
  inherited;
  if (AEvtName = 'eventadd') and Assigned(FOnEventadd) then
    FOnEventadd(TExtCalendarEventEditForm(ParamAsObject('This')), TExtCalendarEventRecord(ParamAsObject('Rec')))
  else if (AEvtName = 'eventcancel') and Assigned(FOnEventcancel) then
    FOnEventcancel(TExtCalendarEventEditForm(ParamAsObject('This')), TExtCalendarEventRecord(ParamAsObject('Rec')))
  else if (AEvtName = 'eventdelete') and Assigned(FOnEventdelete) then
    FOnEventdelete(TExtCalendarEventEditForm(ParamAsObject('This')), TExtCalendarEventRecord(ParamAsObject('Rec')))
  else if (AEvtName = 'eventupdate') and Assigned(FOnEventupdate) then
    FOnEventupdate(TExtCalendarEventEditForm(ParamAsObject('This')), TExtCalendarEventRecord(ParamAsObject('Rec')));
end;

end.
