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
  Ext.Base
  , Ext.Calendar
  , Ext.Data
  , EF.Tree
  , Kitto.JS
  , Kitto.JS.Types
  , Kitto.Metadata.DataView
  , Kitto.Ext.Base
  , Kitto.Ext.DataPanelLeaf
  ;

type
  TKExtCalendarPanel = class(TKExtDataPanelLeafController)
  strict private
    FCalendarPanel: TExtCalendarPanel;
    FCalendarStore: TExtDataStore;
    FCalendarReader: TExtDataJsonReader;
    procedure CreateAndInitCalendar;
    function GetStartDateDBName: string;
    function GetEndDateDBName: string;
    function GetDateFieldNameForNewRecords: string;
    function CreateCalendarReader: TExtDataJsonReader;
    function CreateCalendarStore: TExtDataStore;
  strict protected
    function GetSelectCall(const AMethod: TJSProcedure): TExtExpression; override;
    function GetSelectConfirmCall(const AMessage: string; const AMethod: TJSProcedure): string; override;
    property CalendarStore: TExtDataStore read FCalendarStore;
    procedure SetViewTable(const AValue: TKViewTable); override;
    function IsClientStoreAutoLoadEnabled: Boolean; override;
    function GetRecordPageFilter: string; override;
    function IsActionSupported(const AActionName: string): Boolean; override;
    procedure SetNewRecordDefaultValues(const ANode: TEFNode); override;
  //published
    procedure GetCalendarRecords;
    procedure CalendarDayClick(This: TExtCalendarPanel; Dt: TDateTime; Allday: Boolean; El: TExtElement);
  public
    procedure LoadData; override;
  end;

implementation

uses
  SysUtils
  , StrUtils
  , Types
  , {$IFDEF D21+}JSON{$ELSE}DBXJSON{$ENDIF}
  , EF.Localization
  , EF.Macros
  , EF.StrUtils
  , EF.SQL
  , Kitto.Types
  , Kitto.Metadata.Models
  , Kitto.Web.Application
  , Kitto.Web.Response
  , Kitto.Ext.Utils
  , Kitto.Ext.Controller
  ;

{ TKExtCalendarPanel }

procedure TKExtCalendarPanel.CalendarDayClick(This: TExtCalendarPanel; Dt: TDateTime; Allday: Boolean; El: TExtElement);
begin
  NewRecord;
end;

procedure TKExtCalendarPanel.CreateAndInitCalendar;
begin
  Assert(ClientStore <> nil);

  FCalendarPanel := TExtCalendarPanel.CreateAndAddToArray(Items);
  FCalendarPanel.Region := rgCenter;
  FCalendarPanel.Border := False;

  FCalendarPanel.DayText := _('Day');
  FCalendarPanel.MonthText := _('Month');
  FCalendarPanel.GoText := _('Go');
  FCalendarPanel.JumpToText := _('Jump to:');
  // Disables the built-in editors.
  //FCalendarPanel.ReadOnly := True;

  FCalendarPanel.ShowDayView := True;
  FCalendarPanel.ShowMultiDayView := True; // 3 days
  FCalendarPanel.ShowWeekView := True;
  FCalendarPanel.ShowMultiWeekView := True; // 2 weeks
  FCalendarPanel.ShowMonthView := True;
  FCalendarPanel.ActiveItem := 4; // month view

  FCalendarPanel.ShowNavBar := True;
  FCalendarPanel.ShowTodayText := True;
  FCalendarPanel.ShowTime := True;

  FCalendarPanel.&On('dayclick',
    TKWebResponse.Current.Items.AjaxCallMethod(Self).SetMethod(NewRecord)
      .AddRawParam('m', 'dt.getMonth() + 1')
      .AddRawParam('y', 'dt.getFullYear()')
      .AddRawParam('m', 'dt.getDate()')
      .AddRawParam('allday', 'allday')
      .FunctionArgs('cal, dt, allday, el')
      .FunctionReturn('false')
      .AsFunction);

  FCalendarPanel.EventStore := ClientStore;
  FCalendarPanel.CalendarStore := CalendarStore;
end;

function TKExtCalendarPanel.GetRecordPageFilter: string;
var
  LStartDateStr: string;
  LEndDateStr: string;
  LFilter: string;

  function ParseJSDate(const ADateYMD: string): TDateTime;
  var
    LParts: TStringDynArray;
  begin
    LParts := EF.StrUtils.Split(ADateYMD, '-');
    if Length(LParts) = 3 then
      Result := EncodeDate(StrToInt(LParts[0]), StrToInt(LParts[1]), StrToInt(LParts[2]))
    else
      Result := 0;
  end;

begin
  Result := inherited GetRecordPageFilter;

  LStartDateStr := ParamAsString('start');
  LEndDateStr := ParamAsString('end');
  if (LStartDateStr <> '') and (LEndDateStr <> '') then
  begin
    LStartDateStr := TKWebApplication.Current.Config.DBConnections[ViewTable.DatabaseName].DBEngineType.FormatDateTime(ParseJSDate(LStartDateStr));
    LEndDateStr := TKWebApplication.Current.Config.DBConnections[ViewTable.DatabaseName].DBEngineType.FormatDateTime(ParseJSDate(LEndDateStr));
    LFilter := GetStartDateDBName + ' between ' + SQLQuotedStr(LStartDateStr) + ' and ' + SQLQuotedStr(LEndDateStr) +
      ' or ' + SQLQuotedStr(LStartDateStr) + ' between ' + GetStartDateDBName + ' and ' + GetEndDateDBName;
    if Result = '' then
      Result := LFilter
    else
      Result := Result + ' and (' + LFilter + ')';
  end;
end;

function TKExtCalendarPanel.GetSelectCall(const AMethod: TJSProcedure): TExtExpression;
begin
  Result := GenerateAnonymousFunction(Format('ajaxCalendarSelection("yes", "", {params: {methodURL: "%s", calendarPanel: %s, fieldNames: "%s"}});',
    [GetMethodURL(AMethod), FCalendarPanel.JSName, Join(ViewTable.GetKeyFieldAliasedNames, ',')]));
end;

function TKExtCalendarPanel.GetSelectConfirmCall(const AMessage: string;
  const AMethod: TJSProcedure): string;
begin
  Result := Format('selectCalendarConfirmCall("%s", "%s", %s, "%s", {methodURL: "%s", calendarPanel: %s, fieldNames: "%s"});',
    [_(TKWebApplication.Current.Config.AppTitle), AMessage, FCalendarPanel.JSName, ViewTable.Model.CaptionField.FieldName,
      GetMethodURL(AMethod), FCalendarPanel.JSName, Join(ViewTable.GetKeyFieldAliasedNames, ',')]);
end;

function TKExtCalendarPanel.GetStartDateDBName: string;
begin
  Result := ViewTable.FieldByAliasedName('StartDate').DBNameOrExpression;
end;

function TKExtCalendarPanel.GetEndDateDBName: string;
begin
  Result := ViewTable.FieldByAliasedName('EndDate').DBNameOrExpression;
end;

procedure TKExtCalendarPanel.SetNewRecordDefaultValues(const ANode: TEFNode);
var
  LDay: Integer;
  LMonth: Integer;
  LYear: Integer;
begin
  LDay := ParamAsInteger('d');
  LMonth := ParamAsInteger('m');
  LYear := ParamAsInteger('y');

  ANode.GetNode('Sys/DefaultValues/' + GetDateFieldNameForNewRecords, True).AsDateTime :=  EncodeDate(LYear, LMonth, LDay);
end;

procedure TKExtCalendarPanel.GetCalendarRecords;
var
  LJSONArray: TJSONArray;

  procedure AddItem(const AId: Integer; const ATitle, ADescription: string;
    const AColorId: Integer; AIsHidden: Boolean);
  var
    LObject: TJSONObject;
  begin
    LObject := TJSONObject.Create;
    LObject.AddPair('CalendarId', TJSONNumber.Create(AId));
    LObject.AddPair('Title', ATitle);
    LObject.AddPair('Description', ADescription);
    LObject.AddPair('ColorId', TJSONNumber.Create(AColorId));
    {$IFDEF D22+}
    LObject.AddPair('IsHidden', TJSONBool.Create(AIsHidden));
    {$ELSE}
    if AIsHidden then
      LObject.AddPair('IsHidden', TJSONTrue.Create)
    else
      LObject.AddPair('IsHidden', TJSONFalse.Create);
    {$ENDIF}
    if AIsHidden then
    LJSONArray.AddElement(LObject);
  end;

begin
  LJSONArray := TJSONArray.Create;
  try
    AddItem(1, 'First', 'First Calendar', 16711680, False);
    AddItem(2, 'Second', 'Second Calendar', 65280, False);
    AddItem(3, 'Third', 'Third Calendar', 255, False);
    AddItem(4, 'Fourth', 'Fourth Calendar', 16711680, False);
    AddItem(5, 'Fifth', 'Fifth Calendar', 65280, False);
    AddItem(6, 'Sixth', 'Sixth Calendar', 255, False);
    AddItem(7, 'Seventh', 'Seventh Calendar', 16711680, False);
    AddItem(8, 'Eighth', 'Eighth Calendar', 65280, False);
    AddItem(9, 'Ninth', 'Ninth Calendar', 255, False);
    AddItem(10, 'Tenth', 'Tenth Calendar', 65280, False);
    TKWebResponse.Current.Items.AddJSON(Format('{Success: true, Total: %d, Root: %s}',
      [3, {$IFDEF D21+}LJSONArray.ToJSON{$ELSE}LJSONArray.ToString{$ENDIF}]));
  finally
    FreeAndNil(LJSONArray);
  end;
end;

function TKExtCalendarPanel.GetDateFieldNameForNewRecords: string;
begin
  Result := ViewTable.GetString('Controller/DateFieldName', 'StartDate');
end;

function TKExtCalendarPanel.IsActionSupported(const AActionName: string): Boolean;
begin
  Result := True;
end;

function TKExtCalendarPanel.IsClientStoreAutoLoadEnabled: Boolean;
begin
  // We don't need to call the store's load method, as the calendar
  // panel does that automatically.
  Result := False;
end;

procedure TKExtCalendarPanel.LoadData;
begin
  inherited;
  if Assigned(CalendarStore) then
  { TODO : empty object argument still needed? }
    CalendarStore.Load(JSObject(''));
end;

procedure TKExtCalendarPanel.SetViewTable(const AValue: TKViewTable);
begin
  inherited;

  Assert(Assigned(AValue));

  FCalendarStore := CreateCalendarStore;
  FCalendarReader := CreateCalendarReader;
  FCalendarStore.Proxy.Reader := FCalendarReader;

  CreateAndInitCalendar;
end;

function TKExtCalendarPanel.CreateCalendarReader: TExtDataJsonReader;

  procedure AddReaderField(const AReader: TExtDataJsonReader; const AName, AType: string; const AUseNull: Boolean);
  var
    LField: TExtDataField;
  begin
    LField := TExtDataField.CreateInlineAndAddToArray(AReader.Fields);
    LField.Name := AName;
    LField.&Type := AType;
    LField.UseNull := AUseNull;
  end;

begin
  Assert(Assigned(ViewTable));

  Result := TExtDataJsonReader.Create(Self);
  Result.RootProperty := 'Root';
  Result.TotalProperty := 'Total';
  Result.MessageProperty := 'Msg';
  Result.SuccessProperty := 'Success';

  AddReaderField(Result, 'CalendarId', 'int', False);
  AddReaderField(Result, 'Title', 'string', False);
  AddReaderField(Result, 'Description', 'string', False);
  AddReaderField(Result, 'ColorId', 'string', False);
  AddReaderField(Result, 'Hidden', 'boolean', False);
end;

function TKExtCalendarPanel.CreateCalendarStore: TExtDataStore;
var
  LProxy: TExtDataAjaxProxy;
begin
  Result := TExtDataStore.Create(Self);
  Result.RemoteSort := False;
  LProxy := TExtDataAjaxProxy.Create(Result);
  LProxy.Url := GetMethodURL(GetCalendarRecords);
  Result.Proxy := LProxy;
  Result.On('exception', GenerateAnonymousFunction('proxy, type, action, options, response, arg', 'loadError(type, action, response);'));
end;

initialization
  TKExtControllerRegistry.Instance.RegisterClass('CalendarPanel', TKExtCalendarPanel);

finalization
  TKExtControllerRegistry.Instance.UnregisterClass('CalendarPanel');

end.
