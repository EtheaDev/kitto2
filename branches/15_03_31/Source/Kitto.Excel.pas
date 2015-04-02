{-------------------------------------------------------------------------------
   Copyright 2014 Ethea S.r.l.

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

unit Kitto.Excel;

interface

uses
  SysUtils, Classes,
  DB, ADODB, ADOX_TypeLibrary,
  Kitto.Metadata.DataView, EF.Tree;

const
  EXCEL_FILE_EXT = '.xls';
  EXCEL_TEMPLATE_EXT = '.xlt';
  EXCEL_NEW_FILE_EXT = '.xlsx';
  EXCEL_NEW_TEMPLATE_EXT = '.xltx';

type
  TExcelVersion = (ex2000, ex2007);

  TExportAcceptRecordEvent = procedure(ARecord: TKViewTableRecord; var AAccept: boolean) of object;
  TExportAcceptFieldEvent = procedure(AViewField: TKViewField; var AAccept: boolean) of object;

  TExportAcceptDataRecordEvent = procedure(ADataSet: TDataSet; var AAccept: boolean) of object;
  TExportAcceptDataFieldEvent = procedure(AField: TField; var AAccept: boolean) of object;

  TKExtExcelEngine = class(TComponent)
  strict private
    procedure CreateExcelSheet(const AViewTable: TKViewTable;
      AConnectionString, AExcelRangeName: string); overload;
    procedure CreateExcelSheet(const ADataSet: TDataSet;
      AConnectionString, AExcelRangeName: string); overload;
    function GetConnectionString(const ExcelFileName: string): string;
    function IsValidField(const AViewField: TKViewField;
      AAcceptFieldEvent: TExportAcceptFieldEvent = nil): Boolean; overload;
    function IsValidDataField(const AField: TField;
      AAcceptFieldEvent: TExportAcceptDataFieldEvent = nil): Boolean; overload;
    procedure AddExcelColumn(ATable: _Table; const AColumnName: string;
      const ADataType: DataTypeEnum; const ASize: Integer);
  strict protected
    procedure GetADOXDataType(const ADataType: TEFDataType;
      const AFieldSize: Integer; out AADOXDataType: DataTypeEnum);
    procedure GetADOXFieldType(const Field : TField;
  out AADOXDataType : DataTypeEnum);
  public
    procedure CreateFileByTable(const AFileName: string; const ATable: TKViewTable;
      const AExcelRangeName: string = ''; AAcceptFieldEvent: TExportAcceptFieldEvent = nil);
    procedure CreateFileByDataSet(const AFileName: string; const ADataSet: TDataSet;
      const AExcelRangeName: string = ''; AAcceptFieldEvent: TExportAcceptDataFieldEvent = nil);
    procedure FillAdoTable(const AExcelFileName, AExcelRangeName: string;
      const ATableStore: TKViewTableStore;
      AAcceptRecordEvent: TExportAcceptRecordEvent = nil;
      AAcceptFieldEvent: TExportAcceptFieldEvent = nil);
    procedure FillAdoTableByDataSet(const AExcelFileName, AExcelRangeName: string;
      const ADataSet: TDataSet;
      AAcceptRecordEvent: TExportAcceptDataRecordEvent = nil;
      AAcceptFieldEvent: TExportAcceptDataFieldEvent = nil);

  end;

implementation

uses
  Math,
  EF.StrUtils, EF.DB, EF.SysUtils,
  Kitto.Metadata.Models, Kitto.Config, Kitto.Utils;

const
  ADO_EXCEL_2000 = 'Excel 8.0';
  ADO_EXCEL_2007 = 'Excel 12.0 Xml';

  MAX_EXCEL_STRING_COLUMN_SIZE = 255;
  EXCEL_ADDITIONAL_OPTION = 'IMEX=1;HDR=YES';
  EXCEL_CONN_STRING_12 = 'Provider=Microsoft.ACE.OLEDB.12.0;Data Source=%s;Extended Properties=%s';
  EXCEL_CONN_STRING = 'Provider=Microsoft.Jet.OLEDB.4.0;Data Source=%s;Extended Properties=%s';

  AExcelVersion : Array[TExcelVersion] of string =
    (ADO_EXCEL_2000,
     ADO_EXCEL_2007);

  AExcelExtension : Array[TExcelVersion] of string =
   (EXCEL_FILE_EXT,
     EXCEL_NEW_FILE_EXT);

  AExcelTemplateExtension : Array[TExcelVersion] of string =
   (EXCEL_TEMPLATE_EXT,
     EXCEL_NEW_TEMPLATE_EXT);

{ TKExtExcelEngine }

function TKExtExcelEngine.GetConnectionString(const ExcelFileName: string): string;
var
  FileExt: string;
  ExcelVersion: TExcelVersion;
  ConnectionString: string;
  AdditionalOptions: string;
begin
  FileExt := ExtractFileExt(ExcelFileName);
  if SameText(FileExt, EXCEL_FILE_EXT) then
  begin
    ExcelVersion := ex2000;
    ConnectionString := EXCEL_CONN_STRING;
    AdditionalOptions := '';
  end
  else
  begin
    ExcelVersion := ex2007;
    ConnectionString := EXCEL_CONN_STRING_12;
    AdditionalOptions := '';
  end;
  Result := Format(ConnectionString,
    [ExcelFileName,AExcelVersion[ExcelVersion]+AdditionalOptions]);
end;

procedure TKExtExcelEngine.GetADOXDataType(const ADataType: TEFDataType;
  const AFieldSize: Integer; out AADOXDataType: DataTypeEnum);
begin
  if ADataType is TEFStringDataType then
    AADOXDataType := adVarWChar
  else if ADataType is TEFMemoDataType then
    AADOXDataType := adLongVarWChar
  else if ADataType is TEFIntegerDataType then
    AADOXDataType := adInteger
  else if (ADataType is TEFDateDataType) or (ADataType is TEFTimeDataType) or (ADataType is TEFDateTimeDataType) then
    AADOXDataType := adDate
  else if ADataType is TEFBooleanDataType then
    AADOXDataType := adBoolean
  else if (ADataType is TEFFloatDataType) or (ADataType is TEFDecimalDataType) then
    AADOXDataType := adDouble
  else if ADataType is TEFCurrencyDataType then
    AADOXDataType := adCurrency
  else if AFieldSize <= MAX_EXCEL_STRING_COLUMN_SIZE then
    AADOXDataType := adVarWChar
  else
    AADOXDataType := adLongVarWChar;
end;

procedure TKExtExcelEngine.GetAdoXFieldType(const Field : TField;
  out AADOXDataType : DataTypeEnum);
begin
  case Field.DataType of
    ftFixedChar, ftString, ftGuid : AADOXDataType := adVarWChar;
    ftWideString : AADOXDataType := adVarWChar;
    ftMemo : AADOXDataType := adLongVarWChar;
    ftLargeint : AADOXDataType := adBigInt;
    ftAutoInc, ftInteger : AADOXDataType := adInteger;
    ftSmallint, ftWord : AADOXDataType := adSmallInt;
    ftFloat, ftBCD : AADOXDataType := adDouble;
    ftCurrency : AADOXDataType := adCurrency;
    ftFMTBcd : AADOXDataType := adDouble;
    ftBoolean : AADOXDataType := adBoolean;
    ftDate, ftTime, ftDateTime, ftTimeStamp : AADOXDataType := adDate;
  else
    AADOXDataType := adVarWChar;
  end;
  if Field.Size > MAX_EXCEL_STRING_COLUMN_SIZE then
    AADOXDataType := adLongVarWChar;
end;

procedure TKExtExcelEngine.AddExcelColumn(ATable: _Table;
  const AColumnName: string; const ADataType: DataTypeEnum; const ASize: Integer);
var
  LColumn: _Column;
begin
  LColumn := CoColumn.Create;
  with LColumn do
  begin
    Set_Name(AColumnName);
    Set_Type_(ADataType);
    if ASize <> 0 then
      Set_DefinedSize(Min(ASize,MAX_EXCEL_STRING_COLUMN_SIZE));
  end;
  //add column to table
  ATable.Columns.Append(LColumn, ADataType, ASize);
  LColumn := nil;
end;

procedure TKExtExcelEngine.CreateExcelSheet(const AViewTable: TKViewTable;
  AConnectionString, AExcelRangeName: string);
var
  LCatalog: _Catalog;
  LTable: _Table;
  LFieldIndex: Integer;
  LViewField: TKViewField;
  LColumnName: string;
  LADOXDataType: DataTypeEnum;
  LFieldSize: Integer;

begin
  //WorkBook creation (database)
  LCatalog := CoCatalog.Create;
  LCatalog.Set_ActiveConnection(AConnectionString);
  //WorkSheet creation (table)
  LTable := CoTable.Create;
  LTable.Set_Name(AExcelRangeName);

  //Columns creation (fields)
  for LFieldIndex := 0 to AViewTable.FieldCount - 1 do
  begin
    LViewField := AViewTable.Fields[LFieldIndex];
    if IsValidField(LViewField) then
    begin
      LColumnName := NormalizeColumnName(LViewField.FieldName);
      LFieldSize := LViewField.Size;
      GetADOXDataType(LViewField.ActualDataType, LFieldSize, LADOXDataType);
      AddExcelColumn(LTable, LColumnName, LADOXDataType, LFieldSize);
    end;
  end;

  //add table to database
  LCatalog.Tables.Append(LTable);

  LTable := nil;
  LCatalog := nil;
end;

procedure TKExtExcelEngine.CreateExcelSheet(const ADataSet: TDataSet;
  AConnectionString, AExcelRangeName: string);
var
  LCatalog: _Catalog;
  LTable: _Table;
  LFieldIndex: Integer;
  LField: TField;
  LColumnName: string;
  LADOXDataType: DataTypeEnum;
  LFieldSize: Integer;

begin
  //WorkBook creation (database)
  LCatalog := CoCatalog.Create;
  LCatalog.Set_ActiveConnection(AConnectionString);
  //WorkSheet creation (table)
  LTable := CoTable.Create;
  LTable.Set_Name(AExcelRangeName);

  //Columns creation (fields)
  for LFieldIndex := 0 to ADataSet.FieldCount - 1 do
  begin
    LField := ADataSet.Fields[LFieldIndex];
    if IsValidDataField(LField) then
    begin
      LColumnName := NormalizeColumnName(LField.FieldName);
      LFieldSize := LField.Size;
      GetADOXFieldType(LField, LADOXDataType);
      AddExcelColumn(LTable, LColumnName, LADOXDataType, LFieldSize);
    end;
  end;

  //add table to database
  LCatalog.Tables.Append(LTable);

  LTable := nil;
  LCatalog := nil;
end;

function TKExtExcelEngine.IsValidField(const AViewField: TKViewField;
  AAcceptFieldEvent: TExportAcceptFieldEvent = nil): Boolean;
var
  LDummy: DataTypeEnum;
begin
  Result := True;
  if Assigned(AViewField) and (AViewField.IsVisible) then
    GetADOXDataType(AViewField.ActualDataType, AViewField.Size, LDummy);
  if Assigned(AAcceptFieldEvent) then
    AAcceptFieldEvent(AViewField, Result);
end;

function TKExtExcelEngine.IsValidDataField(const AField: TField;
  AAcceptFieldEvent: TExportAcceptDataFieldEvent = nil): Boolean;
var
  LDummy: DataTypeEnum;
begin
  Result := True;
  if Assigned(AField) and (AField.Visible) then
    GetADOXFieldType(AField, LDummy);
  if Assigned(AAcceptFieldEvent) then
    AAcceptFieldEvent(AField, Result);
end;

procedure TKExtExcelEngine.CreateFileByTable(const AFileName: string;
  const ATable: TKViewTable; const AExcelRangeName: string;
  AAcceptFieldEvent: TExportAcceptFieldEvent);
var
  LConnectionString: string;
begin
  LConnectionString := GetConnectionString(AFileName);
  CreateExcelSheet(ATable, LConnectionString, AExcelRangeName)
end;

procedure TKExtExcelEngine.CreateFileByDataSet(const AFileName: string; const ADataSet: TDataSet;
  const AExcelRangeName: string = ''; AAcceptFieldEvent: TExportAcceptDataFieldEvent = nil);
var
  LConnectionString: string;
begin
  LConnectionString := GetConnectionString(AFileName);
  CreateExcelSheet(ADataSet, LConnectionString, AExcelRangeName)
end;

procedure TKExtExcelEngine.FillAdoTable(const AExcelFileName, AExcelRangeName: string;
  const ATableStore: TKViewTableStore;
  AAcceptRecordEvent: TExportAcceptRecordEvent = nil;
  AAcceptFieldEvent: TExportAcceptFieldEvent = nil);
var
  LFirstAccepted: Boolean;
  LRecordIndex, LFieldIndex: Integer;
  LAcceptRecord, LAcceptField: Boolean;
  LRecord: TKViewTableRecord;
  LDestField: TField;
  LSourceField: TKViewTableField;
  LAdoTable: TAdoTable;

  function FindValidField(const AFieldName: string): TKViewTableField;
  var
    LFieldIndex: Integer;
    LField: TKViewTableField;
  begin
    Result := nil;
    for LFieldIndex := 0 to LRecord.FieldCount - 1 do
    begin
      LField := LRecord.Fields[LFieldIndex];
      if Assigned(LField) and Assigned(LField.ViewField) and
        SameText(NormalizeColumnName(LField.ViewField.FieldName), NormalizeColumnName(AFieldName)) then
      begin
        Result := LField;
        Break;
      end;
    end;
  end;
begin
  //Opens the Ado table using a range defined inside the excel file
  LAdoTable := TAdoTable.Create(nil);
  LAdoTable.CursorType := ctStatic;
  LAdoTable.ConnectionString := GetConnectionString(AExcelFileName);
  LAdoTable.TableName := AExcelRangeName;
  LAdoTable.Open;
  try
    LFirstAccepted := False;
    //Fill the Ado dataset using the records of the store
    for LRecordIndex := 0 to ATableStore.RecordCount -1 do
    begin
      //only the first record of the range is edited, then appends new records
      LRecord := ATableStore.Records[LRecordIndex];
      LAcceptRecord := not LRecord.IsDeleted;
      if Assigned(AAcceptRecordEvent) then
        AAcceptRecordEvent(LRecord, LAcceptRecord);
      if LAcceptRecord then
      begin
        if not LFirstAccepted then
          LAdoTable.Edit
        else
          LAdoTable.Append;
        LFirstAccepted := True;
        try
          //Update ado tabel fields by the structure of the range
          for LFieldIndex := 0 to LAdoTable.FieldCount - 1 do
          begin
            LDestField := LAdoTable.Fields[LFieldIndex];
            LSourceField := FindValidField(LDestField.FieldName);
            LAcceptField := Assigned(LSourceField) and IsValidField(LSourceField.ViewField,
              AAcceptFieldEvent);
            if LAcceptField then
            begin
              if LSourceField.ViewField.ActualDataType is TEFMemoDataType then
                LDestField.AsString := StringReplace(LSourceField.AsString, sLineBreak, chr(10), [rfReplaceAll])
              else
                LSourceField.AssignValueToField(LDestField);
            end
            else
              LDestField.Clear;
          end;
          LAdoTable.Post;
        except
          LAdoTable.Cancel;
          raise;
        end;
      end;
    end;
  finally
    LAdoTable.Close;
    LAdoTable.Free;
  end;
end;

procedure TKExtExcelEngine.FillAdoTableByDataSet(const AExcelFileName, AExcelRangeName: string;
  const ADataSet: TDataSet;
  AAcceptRecordEvent: TExportAcceptDataRecordEvent = nil;
  AAcceptFieldEvent: TExportAcceptDataFieldEvent = nil);
var
  LFirstAccepted: Boolean;
  LRecordIndex, LFieldIndex: Integer;
  LAcceptRecord, LAcceptField: Boolean;
  LDestField: TField;
  LSourceField: TField;
  LAdoTable: TAdoTable;

  function FindValidField(const AFieldName: string): TField;
  var
    LFieldIndex: Integer;
    LField: TField;
  begin
    Result := nil;
    for LFieldIndex := 0 to ADataSet.FieldCount - 1 do
    begin
      LField := ADataSet.Fields[LFieldIndex];
      if Assigned(LField) and
        SameText(NormalizeColumnName(LField.FieldName), NormalizeColumnName(AFieldName)) then
      begin
        Result := LField;
        Break;
      end;
    end;
  end;

begin
  //Opens the Ado table using a range defined inside the excel file
  LAdoTable := TAdoTable.Create(nil);
  LAdoTable.CursorType := ctStatic;
  LAdoTable.ConnectionString := GetConnectionString(AExcelFileName);
  LAdoTable.TableName := AExcelRangeName;
  LAdoTable.Open;
  try
    LFirstAccepted := False;
    //Fill the Ado dataset using the records of the store
    ADataSet.First;
    while not ADataSet.Eof do
    begin
      //only the first record of the range is edited, then appends new records
      LAcceptRecord := True;
      if Assigned(AAcceptRecordEvent) then
        AAcceptRecordEvent(ADataSet, LAcceptRecord);
      if LAcceptRecord then
      begin
        if not LFirstAccepted then
          LAdoTable.Edit
        else
          LAdoTable.Append;
        LFirstAccepted := True;
        try
          //Update ado tabel fields by the structure of the range
          for LFieldIndex := 0 to LAdoTable.FieldCount - 1 do
          begin
            LDestField := LAdoTable.Fields[LFieldIndex];
            LSourceField := FindValidField(LDestField.FieldName);
            LAcceptField := Assigned(LSourceField) and IsValidDataField(LSourceField,
              AAcceptFieldEvent);
            if LAcceptField then
            begin
              if LSourceField is TMemoField then
                LDestField.AsString := StringReplace(LSourceField.AsString, sLineBreak, chr(10), [rfReplaceAll])
              else
                LDestField.Value := LSourceField.Value;
            end
            else
              LDestField.Clear;
          end;
          LAdoTable.Post;
        except
          LAdoTable.Cancel;
          raise;
        end;
      end;
      ADataSet.Next;
    end;
  finally
    LAdoTable.Close;
    LAdoTable.Free;
  end;
end;

end.
