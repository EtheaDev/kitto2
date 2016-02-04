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
  strict protected
    procedure SetViewTable(const AValue: TKViewTable); override;
  end;

implementation

uses
  SysUtils, StrUtils,
  EF.Localization, EF.Macros,
  Kitto.Types, Kitto.Ext.Utils, Kitto.Metadata.Models, Kitto.Ext.Session,
  Kitto.Ext.Controller;

{ TKExtCalendarPanel }

procedure TKExtCalendarPanel.CreateAndInitCalendar;
var
  LOption, LFieldName: string;
begin
  Assert(ClientStore <> nil);

  FCalendarPanel := TExtCalendarPanel.CreateAndAddTo(Items);
  FCalendarPanel.Region := rgCenter;
  FCalendarPanel.Border := False;

  FCalendarPanel.ActiveItem := 2; // month view


  // Some optional CalendarPanel configs to experiment with:
  FCalendarPanel.ShowDayView := false;
  FCalendarPanel.ShowWeekView := false;
  FCalendarPanel.ShowMonthView := true;
  FCalendarPanel.ShowNavBar := True;
  FCalendarPanel.ShowTodayText := True;
  FCalendarPanel.ShowTime := True;
  FCalendarPanel.EventStore := ClientStore;
//  FCalendarPanel.CalendarStore := TExtDataArrayStore.Create(Self);
//  FCalendarPanel.CalendarStore.StoreId := 'calendarStore';
  //FCalendarPanel.CalendarStore.idProperty := 'id';
end;

procedure TKExtCalendarPanel.SetViewTable(const AValue: TKViewTable);
begin
  inherited;

  Assert(Assigned(AValue));

  CreateAndInitCalendar;
end;

initialization
  TKExtControllerRegistry.Instance.RegisterClass('CalendarPanel', TKExtCalendarPanel);

finalization
  TKExtControllerRegistry.Instance.UnregisterClass('CalendarPanel');

end.
