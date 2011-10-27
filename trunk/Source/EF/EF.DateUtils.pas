{
  Date management classes and routines.
}
unit EF.DateUtils;

interface

uses
  Classes;

{
  Fills AList with all the string representations of dates from start to
  end of the month of ADate. If specified, ADateFormat is used to format
  date value, otherwise system's default is used.
  If AClearList is True (default), the AList is cleared before starting adding
  date values.
  The function returns number of dates added.
}
function FillMonthDayList(const ADate: TDateTime; const AList: TStrings;
  const AClearList: Boolean = True; const ADateFormat: string = ''): Integer;

{
  Fills AList with all the string representations of dates from AStartDate to
  AEndDate. If specified, ADateFormat is used to format date value, otherwise
  system's default is used.
  If AClearList is True (default), the AList is cleared before starting adding
  date values.
  The function returns number of dates added.
}
function AddIntervalToDayList(const AStartDate, AEndDate: TDateTime;
  const AList: TStrings; const AExcludeDuplicates: Boolean = True;
  const AClearList: Boolean = False; const ADateFormat: string = ''): Integer;

{
  Deletes form AList all days between AStartDate and AEndDate (including bounds).
  Returns the number of entries deleted.
}
function RemoveIntervalFromDayList(const AStartDate, AEndDate: TDateTime;
  const AList: TStrings; const ADateFormat: string = ''): Integer;

implementation

uses
  SysUtils, DateUtils;

function FillMonthDayList(const ADate: TDateTime; const AList: TStrings;
  const AClearList: Boolean = True; const ADateFormat: string = ''): Integer;
var
  LStartOfTheMonth: TDateTime;
  LEndOfTheMonth: TDateTime;

begin
  LStartOfTheMonth := StartOfTheMonth(ADate);
  LEndOfTheMonth := EndOfTheMonth(ADate);

  Result := AddIntervalToDayList(LStartOfTheMonth, LEndOfTheMonth, AList,
    True, AClearList, ADateFormat);
end;

function RemoveIntervalFromDayList(const AStartDate, AEndDate: TDateTime;
  const AList: TStrings; const ADateFormat: string = ''): Integer;
var
  LDaysBetween: Integer;
  LDay: Integer;
  LStringRepresentation: string;
  LIndex: Integer;
begin
  Assert(Assigned(AList));
  Assert(AEndDate >= AStartDate);

  LDaysBetween := DaysBetween(AStartDate, AEndDate);
  Result := 0;
  for LDay := 0 to LDaysBetween do
  begin
    if ADateFormat = '' then
      LStringRepresentation := DateToStr(AStartDate + LDay)
    else
      LStringRepresentation := FormatDateTime(ADateFormat, AStartDate + LDay);

    LIndex := AList.IndexOf(LStringRepresentation);
    if LIndex > -1 then
    begin
      AList.Delete(LIndex);
      Inc(Result);
    end;
  end;
end;

function AddIntervalToDayList(const AStartDate, AEndDate: TDateTime;
  const AList: TStrings; const AExcludeDuplicates: Boolean = True;
  const AClearList: Boolean = False; const ADateFormat: string = ''): Integer;
var
  LDay: Integer;
  LStringRepresentation: string;
  LDaysBetween: Integer;
begin
  Assert(Assigned(AList));
  Assert(AEndDate >= AStartDate);

  if AClearList then
    AList.Clear;

  LDaysBetween := DaysBetween(AStartDate, AEndDate);
  Result := 0;
  for LDay := 0 to LDaysBetween do
  begin
    if ADateFormat = '' then
      LStringRepresentation := DateToStr(AStartDate + LDay)
    else
      LStringRepresentation := FormatDateTime(ADateFormat, AStartDate + LDay);
    if (AExcludeDuplicates = False) or (AList.IndexOf(LStringRepresentation) = -1) then
    begin
      AList.Add(LStringRepresentation);
      Inc(Result);
    end;
  end;
end;

end.
