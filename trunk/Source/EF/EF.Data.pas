unit EF.Data;

{$I EF.Defines.inc}

interface

uses
  Classes, Contnrs, FMTBcd,
  EF.Macros, EF.Types;

type
  {
    Type of a data item.

    Note: edtObject is an object pointer, and it is stored as a memory address.
    As such, it is only useful for in-memory data passing.
  }
  TEFDataType = (edtUnknown, edtString, edtInteger, edtDate, edtTime, edtDateTime,
    edtBoolean, edtCurrency, edtFloat, edtObject, edtBcd);

const
  AllDataTypes = [edtString, edtInteger, edtDate, edtTime, edtDateTime,
    edtBoolean, edtCurrency, edtFloat, edtBcd];
  NumericDataTypes = [edtInteger, edtCurrency, edtFloat, edtBcd];
  StringDataTypes = [edtString];

type
  TEFDataItem = class(TPersistent)
  private
    FRawValue: string;
    FDataType: TEFDataType;
    FName: string;
    FIsNull: Boolean;
    function GetAsString: string;
    function GetAsInteger: Integer;
    function GetAsDate: TDateTime;
    function GetAsDateTime: TDateTime;
    function GetAsTime: TDateTime;
    function GetAsBoolean: Boolean;
    function GetAsCurrency: Currency;
    function GetAsFloat: Double;
    function GetAsBcd: TBcd;
    function GetAsObject: TObject;
    function GetAsVariant: Variant;
    procedure SetAsString(const AValue: string);
    procedure SetAsInteger(const AValue: Integer);
    procedure SetAsDate(const AValue: TDateTime);
    procedure SetAsDateTime(const AValue: TDateTime);
    procedure SetAsTime(const AValue: TDateTime);
    procedure SetAsBoolean(const AValue: Boolean);
    procedure SetAsCurrency(const AValue: Currency);
    procedure SetAsFloat(const AValue: Double);
    procedure SetAsBcd(const AValue: TBcd);
    procedure SetAsObject(const AValue: TObject);
    procedure SetAsVariant(const AValue: Variant);
    function GetDataTypeAsString: string;
    procedure SetRawValue(const AValue: string);
  public
    constructor Create; overload;
    constructor Create(const AName: string;
      const ADataType: TEFDataType = edtUnknown;
      const ARawValue: string = ''); overload;
    constructor Create(const AName, ADataTypeAsString: string;
      const ARawValue: string = ''); overload;
    {
      This is a copy-constructor.
    }
    constructor Create(const ADataItem: TEFDataItem); overload;
    {
      Conversion routines that use the internal settings. Other classes in the
      system use these routines to convert data in a format suitable for
      TEFDataItem.
      Converts ADate into TEFDataItem's string date representation.
    }
    class function DateToStr(const ADate: TDateTime): string;
    {
      Converts ADateTime into TEFDataItem's string date/time representation.
    }
    class function DateTimeToStr(const ADateTime: TDateTime): string;
    {
      Converts ATime into TEFDataItem's string time representation.
    }
    class function TimeToStr(const ATime: TDateTime): string;
    {
      Converts AInt into TEFDataItem's string integer representation.
    }
    class function IntToStr(const AInt: Integer): string;
    {
      Converts ABoolean into TEFDataItem's string boolean representation.
    }
    class function BooleanToStr(const ABoolean: Boolean): string;
    {
      Converts ACurrency into TEFDataItem's string currency representation.
    }
    class function CurrencyToStr(const ACurrency: Currency): string;
    {
      Converts AFloat into TEFDataItem's string float representation.
    }
    class function FloatToStr(const AFloat: Double): string;
    {
      Converts ABcd into TEFDataItem's string bcd representation.
    }
    class function BcdToStr(const ABcd: TBcd): string;
    {
      Converts AObject into TEFDataItem's string object representation.
    }
    class function ObjectToStr(const AObject: TObject): string;
    {
      Converts ADateStr from TEFDataItem's string date representation.
    }
    class function StrToDate(const ADateStr: string): TDateTime;
    {
      Converts ADateTimeStr from TEFDataItem's string date/time representation.
    }
    class function StrToDateTime(const ADateTimeStr: string): TDateTime;
    {
      Converts ATimeStr from TEFDataItem's string time representation.
    }
    class function StrToTime(const ATimeStr: string): TDateTime;
    {
      Converts AIntStr from TEFDataItem's string integer representation.
    }
    class function StrToInt(const AIntStr: string): Integer;
    {
      Converts ABooleanStr from TEFDataItem's string boolean representation.
    }
    class function StrToBoolean(const ABooleanStr: string): Boolean;
    {
      Converts ACurrencyStr from TEFDataItem's string currency representation.
    }
    class function StrToCurrency(const ACurrencyStr: string): Currency;
    {
      Converts AFloatStr from TEFDataItem's string float representation.
    }
    class function StrToFloat(const AFloatStr: string): Double;
    {
      Converts ABcdStr from TEFDataItem's string bcd representation.
    }
    class function StrToBcd(const ABcdStr: string): TBcd;
    {
      Converts AObjectStr from TEFDataItem's string object representation.
    }
    class function StrToObject(const AObjectStr: string): TObject;
    {
      The name of the data item. Might be a column name in a database table,
      a parameter name, a number, or whatever identifies the data item in the
      containing structure.
    }
    property Name: string read FName write FName;
    {
      Data item's type.
    }
    property DataType: TEFDataType read FDataType;
    {
      Accesses DataType as a string, using the enum name without the 'edt' prefix.
    }
    property DataTypeAsString: string read GetDataTypeAsString;
    {
      Properties to access the data item
    }
    property AsString: string read GetAsString write SetAsString;
    property AsInteger: Integer read GetAsInteger write SetAsInteger;
    property AsDate: TDateTime read GetAsDate write SetAsDate;
    property AsTime: TDateTime read GetAsTime write SetAsTime;
    property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsCurrency: Currency read GetAsCurrency write SetAsCurrency;
    property AsFloat: Double read GetAsFloat write SetAsFloat;
    property AsBcd: TBcd read GetAsBcd write SetAsBcd;
    property AsObject: TObject read GetAsObject write SetAsObject;
    property AsVariant: Variant read GetAsVariant write SetAsVariant;
    property RawValue: string read FRawValue write SetRawValue;
    {
      Clears the value, making the data item null. If you want to set an
      empty value instead, use the As* properties.
    }
    procedure Clear;
    {
      Copies the name, value and data type from ASource.
    }
    procedure Assign(Source: TPersistent); override;
    {
      Copies the value and data type from ASource.
    }
    procedure AssignValue(const ASource: TEFDataItem);
    {
      Returns the current data item name, data type and value in string format.
      The format is opaque, and may only be stored as a string and passed back
      to the FromString method.
    }
    function ToString: string; {$IFDEF D12+}override;{$ENDIF}
    {
      Reads name, data type and value from a string representation obtained
      by calling the ToString method. The format is opaque.
    }
    procedure FromString(const AString: string);
    {
      Returns True if the current object has the same name and data type
      as AItem, and False otherwise.
    }
    function DefinitionEquals(const AItem: TEFDataItem): Boolean;
    {
      Returns True if the current object has the same name, data type and
      value/null flag as AItem, and False otherwise.
    }
    function EqualsDataItem(const AItem: TEFDataItem): Boolean;
    {
      Returns True if the data item is null. Call Clear to set a data item to
      null. A data item is null just after creation.
    }
    property IsNull: Boolean read FIsNull;
  end;

{
  Returns a string representation of a given data type. The string is the
  name of the enum symbol minus the edt prefix.
}
function EFDataTypeToString(const ADataType: TEFDataType): string;

{
  Returns a data type given its string representation. The string is the
  name of the enum symbol minus the edt prefix.
}
function StringToEFDataType(const AString: string): TEFDataType;

implementation

uses
  SysUtils, StrUtils, TypInfo, Variants, DateUtils,
  EF.SysUtils, EF.VariantUtils, EF.Localization;

var
  {
    Used by instances of TEFDataItem.
  }
  _DataItemFormatSettings: TFormatSettings;

function EFDataTypeToString(const ADataType: TEFDataType): string;
begin
  Result := Copy(GetEnumName(TypeInfo(TEFDataType), Ord(ADataType)), 4, MaxInt);
end;

function StringToEFDataType(const AString: string): TEFDataType;
var
  LValue: Integer;
begin
  LValue := GetEnumValue(TypeInfo(TEFDataType), 'edt' + AString);
  if (LValue < 0) or (LValue > Ord(High(TEFDataType))) then
    LValue := 0;
    //raise EEFError.CreateFmt(_(SInvalidDataType), [AString]);
  Result := TEFDataType(LValue);
end;

{ TEFDataItem }

procedure TEFDataItem.Assign(Source: TPersistent);
begin
  Assert(Assigned(Source));

  if Source is TEFDataItem then
  begin
    AssignValue(TEFDataItem(Source));
    FName := TEFDataItem(Source).FName;
  end
  else
    inherited;
end;

procedure TEFDataItem.AssignValue(const ASource: TEFDataItem);
begin
  Assert(Assigned(ASource));

  FDataType := ASource.FDataType;
  FRawValue := ASource.FRawValue;
  FIsNull := ASource.IsNull;
end;

procedure TEFDataItem.Clear;
begin
  FRawValue := '';
  FIsNull := True;
end;

constructor TEFDataItem.Create;
begin
  inherited Create;
  FDataType := edtUnknown;
  Clear;
end;

constructor TEFDataItem.Create(const AName: string;
  const ADataType: TEFDataType = edtUnknown;
  const ARawValue: string = '');
begin
  Create;
  FName := AName;
  FDataType := ADataType;
  if ARawValue <> '' then
    RawValue := ARawValue;
end;

constructor TEFDataItem.Create(const AName, ADataTypeAsString: string;
  const ARawValue: string = '');
begin
  Create(AName, StringToEFDataType(ADataTypeAsString), ARawValue);
end;

constructor TEFDataItem.Create(const ADataItem: TEFDataItem);
begin
  Create;
  Assign(ADataItem);
end;

class function TEFDataItem.DateToStr(const ADate: TDateTime): string;
begin
  Result := SysUtils.DateToStr(Trunc(ADate), _DataItemFormatSettings);
end;

function TEFDataItem.DefinitionEquals(const AItem: TEFDataItem): Boolean;
begin
  if Assigned(AItem)
      and (AItem.DataType = FDataType)
      and (AItem.Name = FName) then
    Result := True
  else
    Result := False;
end;

function TEFDataItem.EqualsDataItem(const AItem: TEFDataItem): Boolean;
begin
  if Assigned(AItem)
      and (AItem.DataType = FDataType)
      and (AItem.Name = FName)
      and ((AItem.IsNull and FIsNull) or (AItem.RawValue = FRawValue)) then
    Result := True
  else
    Result := False;
end;

class function TEFDataItem.DateTimeToStr(const ADateTime: TDateTime): string;
begin
  Result := SysUtils.DateTimeToStr(ADateTime, _DataItemFormatSettings);
end;

class function TEFDataItem.TimeToStr(const ATime: TDateTime): string;
begin
  Result := SysUtils.DateToStr(TimeOf(ATime), _DataItemFormatSettings);
end;

class function TEFDataItem.IntToStr(const AInt: Integer): string;
begin
  Result := SysUtils.IntToStr(AInt);
end;

class function TEFDataItem.BooleanToStr(const ABoolean: Boolean): string;
begin
  // Use hardcoded strings so that we're safe from changes in BoolToStr.
  Result := IfThen(ABoolean, 'True', 'False');
end;

class function TEFDataItem.CurrencyToStr(const ACurrency: Currency): string;
begin
  Result := SysUtils.CurrToStr(ACurrency, _DataItemFormatSettings);
end;

class function TEFDataItem.FloatToStr(const AFloat: Double): string;
begin
  Result := SysUtils.FloatToStr(AFloat, _DataItemFormatSettings);
end;

class function TEFDataItem.BcdToStr(const ABcd: TBcd): string;
begin
  Result := FMTBcd.BcdToStr(ABcd);
end;

class function TEFDataItem.ObjectToStr(const AObject: TObject): string;
begin
  Result := SysUtils.IntToStr(Integer(Pointer(AObject)));
end;

class function TEFDataItem.StrToDate(const ADateStr: string): TDateTime;
begin
  Result := Trunc(SysUtils.StrToDateDef(ADateStr, 0, _DataItemFormatSettings));
end;

class function TEFDataItem.StrToDateTime(const ADateTimeStr: string): TDateTime;
begin
  Result := SysUtils.StrToDateTimeDef(ADateTimeStr, 0, _DataItemFormatSettings);
end;

class function TEFDataItem.StrToTime(const ATimeStr: string): TDateTime;
begin
  Result := TimeOf(SysUtils.StrToTimeDef(ATimeStr, 0, _DataItemFormatSettings));
end;

class function TEFDataItem.StrToInt(const AIntStr: string): Integer;
begin
  Result := SysUtils.StrToIntDef(AIntStr, 0);
end;

class function TEFDataItem.StrToBoolean(const ABooleanStr: string): Boolean;
begin
  Result := ABooleanStr = 'True';
end;

class function TEFDataItem.StrToCurrency(const ACurrencyStr: string): Currency;
begin
  Result := SysUtils.StrToCurrDef(ACurrencyStr, 0, _DataItemFormatSettings);
end;

class function TEFDataItem.StrToFloat(const AFloatStr: string): Double;
begin
  Result := SysUtils.StrToFloatDef(AFloatStr, 0, _DataItemFormatSettings);
end;

class function TEFDataItem.StrToBcd(const ABcdStr: string): TBcd;
begin
  Result := FMTBcd.StrToBcd(ABcdStr);
end;

class function TEFDataItem.StrToObject(const AObjectStr: string): TObject;
begin
  Result := TObject(Pointer(SysUtils.StrToInt(AObjectStr)));
end;

function TEFDataItem.GetAsDate: TDateTime;
begin
  Result := Self.StrToDate(FRawValue);
end;

function TEFDataItem.GetAsDateTime: TDateTime;
begin
  Result := Self.StrToDateTime(FRawValue);
end;

function TEFDataItem.GetAsInteger: Integer;
begin
  Result := Self.StrToInt(FRawValue);
end;

function TEFDataItem.GetAsString: string;
begin
  Result := FRawValue;
end;

function TEFDataItem.GetAsTime: TDateTime;
begin
  // Drop the date part, as Self.StrToTime wouldn't.
  if FDataType = edtDateTime then
    Result := TimeOf(Self.StrToDateTime(FRawValue))
  else
    Result := Self.StrToTime(FRawValue);
end;

function TEFDataItem.GetAsVariant: Variant;
begin
  if IsNull then
    Result := Null
  else
  begin
    case FDataType of
      edtUnknown, edtString: Result := AsString;
      edtInteger: Result := AsInteger;
      edtDate: Result := AsDate;
      edtTime: Result := AsTime;
      edtDateTime: Result := AsDateTime;
      edtBoolean: Result := AsBoolean;
      edtCurrency: Result := AsCurrency;
      edtFloat: Result := AsFloat;
      edtBcd: Result := VarFMTBcdCreate(AsBcd);
    else
      raise EEFError.CreateFmt('TEFDataItem.GetAsVariant: data type %s not supported.',
        [EFDataTypeToString(FDataType)]);
    end;
  end;
end;

function TEFDataItem.GetDataTypeAsString: string;
begin
  Result := EFDataTypeToString(DataType);
end;

function TEFDataItem.GetAsBoolean: Boolean;
begin
  Result := Self.StrToBoolean(FRawValue);
end;

function TEFDataItem.GetAsCurrency: Currency;
begin
  Result := Self.StrToCurrency(FRawValue);
end;

function TEFDataItem.GetAsFloat: Double;
begin
  Result := Self.StrToFloat(FRawValue);
end;

function TEFDataItem.GetAsBcd: TBcd;
begin
  Result := Self.StrToBcd(FRawValue);
end;

function TEFDataItem.GetAsObject: TObject;
begin
  Result := Self.StrToObject(FRawValue);
end;

procedure TEFDataItem.SetAsDate(const AValue: TDateTime);
begin
  FDataType := edtDate;
  RawValue := Self.DateToStr(AValue);
end;

procedure TEFDataItem.SetAsDateTime(const AValue: TDateTime);
begin
  FDataType := edtDateTime;
  RawValue := Self.DateTimeToStr(AValue);
end;

procedure TEFDataItem.SetAsTime(const AValue: TDateTime);
begin
  FDataType := edtTime;
  RawValue := Self.TimeToStr(AValue);
end;

procedure TEFDataItem.SetAsVariant(const AValue: Variant);
begin
  case VarType(AValue) of
    varEmpty, varNull: Clear;
    varShortInt, varByte, varWord, varLongWord, varSmallint, varInteger, varInt64: AsInteger := AValue;
    varSingle, varDouble: AsFloat := AValue;
    varCurrency: AsCurrency := AValue;
    varDate: AsDateTime := AValue;
    varBoolean: AsBoolean := AValue;
    varString: AsString := AValue;
    varOleStr: AsString := AValue;
  else
    raise EEFError.CreateFmt('TEFDataItem.SetAsVariant: variant type %d not supported.',
      [VarType(AValue)]);
  end;
end;

procedure TEFDataItem.SetRawValue(const AValue: string);
begin
  FRawValue := AValue;
  FIsNull := False;
end;

procedure TEFDataItem.SetAsInteger(const AValue: Integer);
begin
  FDataType := edtInteger;
  RawValue := Self.IntToStr(AValue);
end;

procedure TEFDataItem.SetAsString(const AValue: string);
begin
  FDataType := edtString;
  RawValue := AValue;
end;

procedure TEFDataItem.SetAsBoolean(const AValue: Boolean);
begin
  FDataType := edtBoolean;
  RawValue := Self.BooleanToStr(AValue);
end;

procedure TEFDataItem.SetAsCurrency(const AValue: Currency);
begin
  FDataType := edtCurrency;
  RawValue := Self.CurrencyToStr(AValue);
end;

procedure TEFDataItem.SetAsFloat(const AValue: Double);
begin
  FDataType := edtFloat;
  RawValue := Self.FloatToStr(AValue);
end;

procedure TEFDataItem.SetAsBcd(const AValue: TBcd);
begin
  FDataType := edtBcd;
  RawValue := Self.BcdToStr(AValue);
end;

procedure TEFDataItem.SetAsObject(const AValue: TObject);
begin
  FDataType := edtObject;
  RawValue := Self.ObjectToStr(AValue);
end;

procedure TEFDataItem.FromString(const AString: string);
var
  LSeparatorPos: Integer;
  LTypeSeparatorPos: Integer;
begin
  LSeparatorPos := Pos('=', AString);
  if LSeparatorPos > 0 then
  begin
    LTypeSeparatorPos := Pos(':', AString);
    if (LTypeSeparatorPos > 0) and (LTypeSeparatorPos < LSeparatorPos) then
    begin
      FName := Copy(AString, 1, LTypeSeparatorPos - 1);
      FDataType := StringToEFDataType(
        Copy(AString, LTypeSeparatorPos + 1, LSeparatorPos - LTypeSeparatorPos - 1));
      RawValue := Copy(AString, LSeparatorPos + 1, MaxInt);
    end
    else
    begin
      FName := Copy(AString, 1, LSeparatorPos - 1);
      AsString := Copy(AString, LSeparatorPos + 1, MaxInt);
    end;
  end
  else
  begin
    FName := AString;
    AsString := '';
  end;
end;

function TEFDataItem.ToString: string;
begin
  Result := FName + ':' + EFDataTypeToString(FDataType) + '=' + FRawValue;
end;

initialization
  _DataItemFormatSettings := GetFormatSettings;
  _DataItemFormatSettings.ShortDateFormat := 'yyyy-mm-dd';
  _DataItemFormatSettings.ShortTimeFormat := 'hh:nn:ss';
  _DataItemFormatSettings.DecimalSeparator := '.';
  _DataItemFormatSettings.DateSeparator := '-';
  _DataItemFormatSettings.TimeSeparator := ':';

finalization

end.
