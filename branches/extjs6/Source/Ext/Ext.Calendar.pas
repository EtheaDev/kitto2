unit Ext.Calendar;

interface

uses
  StrUtils,
  Ext.Util,
  Ext.Base,
  Ext.Data,
  Kitto.Ext;

type
  TExtCalendarBoxLayoutTemplate = class;
  TExtCalendarPanel = class;
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
  THTMLNode = TExtObject;

  TExtCalendarEventRecord = class(TExtObject);

  TExtCalendarView = class(TExtUtilObservable)
  public
    class function JSClassName: string; override;
  end;

  TExtCalendarDateRangeField = class(TExtUtilObservable)
  public
    class function JSClassName: string; override;
  end;

  TExtCalendarPanel = class(TExtPanel)
  private
    FDayText: String;
    FMonthText: String;
    FShowDayView: Boolean;
    FShowMonthView: Boolean;
    FShowNavBar: Boolean;
    FShowTime: Boolean;
    FShowTodayText: Boolean;
    FShowWeekView: Boolean;
    FTodayText: String;
    FWeekText: String;
    FActiveItem: Integer;
    FCalendarStore: TExtDataStore;
    FEventStore: TExtDataStore;
    FGoText: string;
    FJumpToText: string;
    FReadOnly: Boolean;
    FShowMultiDayView: Boolean;
    FShowMultiWeekView: Boolean;
    procedure SetDayText(const AValue: string);
    procedure SetMonthText(const AValue: string);
    procedure SetShowDayView(const AValue: Boolean);
    procedure SetShowMonthView(const AValue: Boolean);
    procedure SetShowNavBar(const AValue: Boolean);
    procedure SetShowTime(const AValue: Boolean);
    procedure SetShowTodayText(const AValue: Boolean);
    procedure SetShowWeekView(const AValue: Boolean);
    procedure SetTodayText(const AValue: string);
    procedure SetWeekText(const AValue: string);
    procedure SetActiveItem(const AValue: Integer);
    procedure SetCalendarStore(const AValue: TExtDataStore);
    procedure SetEventStore(const AValue: TExtDataStore);
    procedure SetGoText(const AValue: string);
    procedure SetJumpToText(const AValue: string);
    procedure SetReadOnly(const AValue: Boolean);
    procedure SetShowMultiDayView(const AValue: Boolean);
    procedure SetShowMultiWeekView(const AValue: Boolean);
  public
    class function JSClassName: string; override;
    property ActiveItem: Integer read FActiveItem write SetActiveItem;
    property DayText: String read FDayText write SetDayText;
    property MonthText: String read FMonthText write SetMonthText;
    property GoText: string read FGoText write SetGoText;
    property JumpToText: string read FJumpToText write SetJumpToText;
    property ShowDayView: Boolean read FShowDayView write SetShowDayView;
    property ShowMultiDayView: Boolean read FShowMultiDayView write SetShowMultiDayView;
    property ShowMonthView: Boolean read FShowMonthView write SetShowMonthView;
    property ShowNavBar: Boolean read FShowNavBar write SetShowNavBar;
    property ShowTime: Boolean read FShowTime write SetShowTime;
    property ShowTodayText: Boolean read FShowTodayText write SetShowTodayText;
    property ShowWeekView: Boolean read FShowWeekView write SetShowWeekView;
    property ShowMultiWeekView: Boolean read FShowMultiWeekView write SetShowMultiWeekView;
    property EventStore: TExtDataStore read FEventStore write SetEventStore;
    property CalendarStore: TExtDataStore read FCalendarStore write SetCalendarStore;
    property TodayText: String read FTodayText write SetTodayText;
    property WeekText: String read FWeekText write SetWeekText;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly;
  end;

  TExtCalendarPicker = class(TExtUtilObservable)
  public
    class function JSClassName: string; override;
  end;

  TExtCalendarDayHeaderTemplate = class(TExtUtilObservable)
  public
    class function JSClassName: string; override;
  end;

  TExtCalendarDayBodyTemplate = class(TExtUtilObservable)
  public
    class function JSClassName: string; override;
  end;

  TExtCalendarDayBodyView = class(TExtUtilObservable)
  public
    class function JSClassName: string; override;
  end;

  TExtCalendarBoxLayoutTemplate = class(TExtUtilObservable)
  public
    class function JSClassName: string; override;
  end;

  TExtCalendarStatusProxy = class(TExtUtilObservable)
  public
    class function JSClassName: string; override;
  end;

  TExtCalendarWeekView = class(TExtUtilObservable)
  public
    class function JSClassName: string; override;
  end;

  TExtCalendarReminderField = class(TExtUtilObservable)
  public
    class function JSClassName: string; override;
  end;

  TExtCalendarDayViewTemplate = class(TExtUtilObservable)
  public
    class function JSClassName: string; override;
  end;

  TExtCalendarEventEditForm = class(TExtUtilObservable)
  public
    class function JSClassName: string; override;
  end;

  TExtCalendarDayHeaderView = class(TExtUtilObservable)
  public
    class function JSClassName: string; override;
  end;

  TExtCalendarDayView = class(TExtUtilObservable)
  public
    class function JSClassName: string; override;
  end;

  TExtCalendarMonthView = class(TExtUtilObservable)
  public
    class function JSClassName: string; override;
  end;

  TExtCalendarMonthViewTemplate = class(TExtUtilObservable)
  public
    class function JSClassName: string; override;
  end;

  TExtCalendarEventEditWindow = class(TExtUtilObservable)
  public
    class function JSClassName: string; override;
  end;

  TExtCalendarEventMappings = class(TExtUtilObservable)
  public
    class function JSClassName: string; override;
  end;

implementation

class function TExtCalendarView.JSClassName: string;
begin
  Result := 'Ext.calendar.CalendarView';
end;

class function TExtCalendarDateRangeField.JSClassName: string;
begin
  Result := 'Ext.calendar.DateRangeField';
end;

procedure TExtCalendarPanel.SetActiveItem(const AValue: Integer);
begin
  FActiveItem := SetConfigItem('activeItem', AValue);
end;

procedure TExtCalendarPanel.SetCalendarStore(const AValue: TExtDataStore);
begin
  FCalendarStore.Free;
  FCalendarStore := TExtDataStore(SetConfigItem('calendarStore', AValue));
end;

procedure TExtCalendarPanel.SetDayText(const AValue: string);
begin
  FDayText := SetConfigItem('dayText', AValue);
end;

procedure TExtCalendarPanel.SetEventStore(const AValue: TExtDataStore);
begin
  FEventStore.Free;
  FEventStore := TExtDataStore(SetConfigItem('eventStore', AValue));
end;

procedure TExtCalendarPanel.SetMonthText(const AValue: string);
begin
  FMonthText := SetConfigItem('monthText', AValue);
end;

procedure TExtCalendarPanel.SetReadOnly(const AValue: Boolean);
begin
  FReadOnly := SetConfigItem('readOnly', AValue);
end;

procedure TExtCalendarPanel.SetShowDayView(const AValue: Boolean);
begin
  FShowDayView := SetConfigItem('showDayView', AValue);
end;

procedure TExtCalendarPanel.SetShowMonthView(const AValue: Boolean);
begin
  FShowMonthView := SetConfigItem('showMonthView', AValue);
end;

procedure TExtCalendarPanel.SetShowMultiDayView(const AValue: Boolean);
begin
  FShowMultiDayView := SetConfigItem('showMultiDayView', AValue);
end;

procedure TExtCalendarPanel.SetShowMultiWeekView(const AValue: Boolean);
begin
  FShowMultiWeekView := SetConfigItem('showMultiWeekView', AValue);
end;

procedure TExtCalendarPanel.SetShowNavBar(const AValue: Boolean);
begin
  FShowNavBar := SetConfigItem('showNavBar', AValue);
end;

procedure TExtCalendarPanel.SetShowTime(const AValue: Boolean);
begin
  FShowTime := SetConfigItem('showTime', AValue);
end;

procedure TExtCalendarPanel.SetShowTodayText(const AValue: Boolean);
begin
  FShowTodayText := SetConfigItem('showTodayText', AValue);
end;

procedure TExtCalendarPanel.SetShowWeekView(const AValue: Boolean);
begin
  FShowWeekView := SetConfigItem('showWeekView', AValue);
end;

procedure TExtCalendarPanel.SetTodayText(const AValue: string);
begin
  FTodayText := SetConfigItem('todayText', AValue);
end;

procedure TExtCalendarPanel.SetWeekText(const AValue: string);
begin
  FWeekText := SetConfigItem('weekText', AValue);
end;

procedure TExtCalendarPanel.SetGoText(const AValue: string);
begin
  FGoText := SetConfigItem('goText', AValue);
end;

procedure TExtCalendarPanel.SetJumpToText(const AValue: string);
begin
  FJumpToText := SetConfigItem('jumpToText', AValue);
end;

class function TExtCalendarPanel.JSClassName: string;
begin
  Result := 'Ext.calendar.CalendarPanel';
end;

class function TExtCalendarPicker.JSClassName: string;
begin
  Result := 'Ext.calendar.CalendarPicker';
end;

class function TExtCalendarDayHeaderTemplate.JSClassName: string;
begin
  Result := 'Ext.calendar.DayHeaderTemplate';
end;

class function TExtCalendarDayBodyTemplate.JSClassName: string;
begin
  Result := 'Ext.calendar.DayBodyTemplate';
end;

class function TExtCalendarDayBodyView.JSClassName: string;
begin
  Result := 'Ext.calendar.DayBodyView';
end;

class function TExtCalendarBoxLayoutTemplate.JSClassName: string;
begin
  Result := 'Ext.calendar.BoxLayoutTemplate';
end;

class function TExtCalendarDayHeaderView.JSClassName: string;
begin
  Result := 'Ext.calendar.DayHeaderView';
end;

class function TExtCalendarDayView.JSClassName: string;
begin
  Result := 'Ext.calendar.DayView';
end;

class function TExtCalendarMonthView.JSClassName: string;
begin
  Result := 'Ext.calendar.MonthView';
end;

class function TExtCalendarMonthViewTemplate.JSClassName: string;
begin
  Result := 'Ext.calendar.MonthViewTemplate';
end;

class function TExtCalendarEventEditWindow.JSClassName: string;
begin
  Result := 'Ext.calendar.EventEditWindow';
end;

class function TExtCalendarEventMappings.JSClassName: string;
begin
  Result := 'Ext.calendar.EventMappings';
end;

class function TExtCalendarStatusProxy.JSClassName: string;
begin
  Result := 'Ext.calendar.StatusProxy';
end;

class function TExtCalendarWeekView.JSClassName: string;
begin
  Result := 'Ext.calendar.WeekView';
end;

class function TExtCalendarReminderField.JSClassName: string;
begin
  Result := 'Ext.calendar.ReminderField';
end;

class function TExtCalendarDayViewTemplate.JSClassName: string;
begin
  Result := 'Ext.calendar.DayViewTemplate';
end;

class function TExtCalendarEventEditForm.JSClassName: string;
begin
  Result := 'Ext.calendar.EventEditForm';
end;

end.
