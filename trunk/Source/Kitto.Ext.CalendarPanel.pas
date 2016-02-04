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

unit Kitto.Ext.CalendarPanel;

{$I Kitto.Defines.inc}

interface

uses
  Ext, ExtCalendar, ExtData,
  EF.Tree,
  Kitto.Metadata.DataView, Kitto.Ext.Base, Kitto.Ext.DataPanelLeaf;

type
  TKExtCalendarPanel = class(TKExtDataPanelLeafController)
  strict private
    FCalendarPanel: TExtCalendarPanel;
    procedure CreateAndInitCalendar;
  strict
  private
    function GetStartDateFieldName: string;
  private
    function GetEndDateFieldName: string; protected
    procedure SetViewTable(const AValue: TKViewTable); override;
    function IsClientStoreAutoLoadEnabled: Boolean; override;
    function GetRecordPageFilter: string; override;
  end;

implementation

uses
  SysUtils, StrUtils, Types,
  EF.Localization, EF.Macros, EF.StrUtils, EF.SQL,
  Kitto.Types, Kitto.Ext.Utils, Kitto.Metadata.Models, Kitto.Ext.Session,
  Kitto.Ext.Controller;

{ TKExtCalendarPanel }

procedure TKExtCalendarPanel.CreateAndInitCalendar;
begin
  Assert(ClientStore <> nil);

  FCalendarPanel := TExtCalendarPanel.CreateAndAddTo(Items);
  FCalendarPanel.Region := rgCenter;
  FCalendarPanel.Border := False;

  FCalendarPanel.ActiveItem := 2; // month view


  // Some optional CalendarPanel configs to experiment with:
  FCalendarPanel.ShowDayView := False;
  FCalendarPanel.ShowWeekView := False;
  FCalendarPanel.ShowMonthView := True;
  FCalendarPanel.ShowNavBar := True;
  FCalendarPanel.ShowTodayText := True;
  FCalendarPanel.ShowTime := True;
  FCalendarPanel.EventStore := ClientStore;
//  FCalendarPanel.CalendarStore := TExtDataArrayStore.Create(Self);
//  FCalendarPanel.CalendarStore.StoreId := 'calendarStore';
  //FCalendarPanel.CalendarStore.idProperty := 'id';
end;

function TKExtCalendarPanel.GetRecordPageFilter: string;
var
  LStartDateStr: string;
  LEndDateStr: string;
  LFilter: string;

  function ParseJSDate(const ADateMDY: string): TDateTime;
  var
    LParts: TStringDynArray;
  begin
    LParts := EF.StrUtils.Split(ADateMDY, '-');
    if Length(LParts) = 3 then
      Result := EncodeDate(LParts[2].ToInteger, LParts[0].ToInteger, LParts[1].ToInteger)
    else
      Result := 0;
  end;

begin
  Result := inherited GetRecordPageFilter;

  LStartDateStr := Session.Query['start'];
  LEndDateStr := Session.Query['end'];
  if (LStartDateStr <> '') and (LEndDateStr <> '') then
  begin
    LStartDateStr := Session.Config.DBConnections[ViewTable.DatabaseName].DBEngineType.FormatDateTime(ParseJSDate(LStartDateStr));
    LEndDateStr := Session.Config.DBConnections[ViewTable.DatabaseName].DBEngineType.FormatDateTime(ParseJSDate(LEndDateStr));
    LFilter := GetStartDateFieldName + ' between ' + SQLQuotedStr(LStartDateStr) + ' and ' + SQLQuotedStr(LEndDateStr) +
      ' or ' + SQLQuotedStr(LStartDateStr) + ' between ' + GetStartDateFieldName + ' and ' + GetEndDateFieldName;
    if Result = '' then
      Result := LFilter
    else
      Result := Result + ' and (' + LFilter + ')';
  end;
end;

function TKExtCalendarPanel.GetStartDateFieldName: string;
begin
  Result := 'StartDate';
end;

function TKExtCalendarPanel.GetEndDateFieldName: string;
begin
  Result := 'EndDate';
end;

function TKExtCalendarPanel.IsClientStoreAutoLoadEnabled: Boolean;
begin
  // We don't need to call the store's load method, as the calendar
  // panel does that automatically.
  Result := True;
end;

procedure TKExtCalendarPanel.SetViewTable(const AValue: TKViewTable);
begin
  inherited;

  Assert(Assigned(AValue));

  CreateAndInitCalendar;
end;

initialization
  TKExtControllerRegistry.Instance.RegisterClass('CalendarPanel', TKExtCalendarPanel);

  TKExtSession.AddAdditionalRef('/examples/calendar/resources/css/calendar', True);
  TKExtSession.AddAdditionalRef('/examples/calendar/src/Ext.calendar');
  TKExtSession.AddAdditionalRef('/examples/calendar/src/templates/DayHeaderTemplate');
  TKExtSession.AddAdditionalRef('/examples/calendar/src/templates/DayBodyTemplate');
  TKExtSession.AddAdditionalRef('/examples/calendar/src/templates/DayViewTemplate');
  TKExtSession.AddAdditionalRef('/examples/calendar/src/templates/BoxLayoutTemplate');
  TKExtSession.AddAdditionalRef('/examples/calendar/src/templates/MonthViewTemplate');
  TKExtSession.AddAdditionalRef('/examples/calendar/src/dd/CalendarScrollManager');
  TKExtSession.AddAdditionalRef('/examples/calendar/src/dd/StatusProxy');
  TKExtSession.AddAdditionalRef('/examples/calendar/src/dd/CalendarDD');
  TKExtSession.AddAdditionalRef('/examples/calendar/src/dd/DayViewDD');
  TKExtSession.AddAdditionalRef('/examples/calendar/src/EventRecord');
  TKExtSession.AddAdditionalRef('/examples/calendar/src/widgets/CalendarPicker');
  TKExtSession.AddAdditionalRef('/examples/calendar/src/WeekEventRenderer');
  TKExtSession.AddAdditionalRef('/examples/calendar/src/views/CalendarView');
  TKExtSession.AddAdditionalRef('/examples/calendar/src/views/DayHeaderView');
  TKExtSession.AddAdditionalRef('/examples/calendar/src/views/DayBodyView');
  TKExtSession.AddAdditionalRef('/examples/calendar/src/views/DayView');
  TKExtSession.AddAdditionalRef('/examples/calendar/src/views/MonthDayDetailView');
  TKExtSession.AddAdditionalRef('/examples/calendar/src/views/MonthView');
  TKExtSession.AddAdditionalRef('/examples/calendar/src/views/WeekView');
  TKExtSession.AddAdditionalRef('/examples/calendar/src/widgets/DateRangeField');
  TKExtSession.AddAdditionalRef('/examples/calendar/src/widgets/ReminderField');
  TKExtSession.AddAdditionalRef('/examples/calendar/src/EventEditForm');
  TKExtSession.AddAdditionalRef('/examples/calendar/src/EventEditWindow');
  TKExtSession.AddAdditionalRef('/examples/calendar/src/CalendarPanel');

finalization
  TKExtControllerRegistry.Instance.UnregisterClass('CalendarPanel');

end.
