{-------------------------------------------------------------------------------
   Copyright 2012-2021 Ethea S.r.l.

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
  SysUtils
  , Classes
  , DB
  , ADODB
  , ADOX_TypeLibrary
  , EF.Tree
  , Kitto.Metadata.DataView
  ;

const
  EXCEL_FILE_EXT = '.xls';
  EXCEL_TEMPLATE_EXT = '.xlt';
  EXCEL_NEW_FILE_EXT = '.xlsx';
  EXCEL_NEW_TEMPLATE_EXT = '.xltx';
  EXCEL_DEFAULT_RANGE = 'DataRange';

type
  TExcelVersion = (ex2000, ex2007);

  TAcceptViewFieldEvent = procedure(AViewField: TKViewField; var AAccept: Boolean) of object;
  TAcceptDataFieldEvent = procedure(AField: TField; var AAccept: Boolean) of object;

  TKExcelEngine = class
  protected
    function ValidColumnName(Field: TField): string;
    function GetConnectionString(const ExcelFileName: string): string;
    function OpenExcelRange(const AExcelFileName, AExcelRangeName: string): TAdoTable;
    procedure CloseExcelRange(const AAdoTable: TAdoTable);
    function IsAcceptedViewField(const AViewField: TKViewField;
      AAcceptFieldEvent: TAcceptViewFieldEvent = nil): Boolean; overload;
    function IsAccepterDataField(const AField: TField;
      AAcceptFieldEvent: TAcceptDataFieldEvent = nil): Boolean; overload;
  end;

  TAcceptExportRecordEvent = procedure(ARecord: TKViewTableRecord; var AAccept: Boolean) of object;
  TAcceptExportDataRecordEvent = procedure(ADataSet: TDataSet; var AAccept: Boolean) of object;

  TKExcelExportEngine = class(TKExcelEngine)
  strict private
    procedure CreateExcelSheet(const AViewTableStore: TKViewTableStore;
      AConnectionString, AExcelRangeName: string;
      AAcceptFieldEvent: TAcceptViewFieldEvent = nil;
      AUseDisplayLabels: Boolean = False); overload;
    procedure CreateExcelSheet(const ADataSet: TDataSet;
      AConnectionString, AExcelRangeName: string;
      AAcceptFieldEvent: TAcceptDataFieldEvent = nil;
      AUseDisplayLabels: Boolean = False); overload;
    function AddExcelColumn(ATable: _Table; const AColumnName: string;
      const ADataType: DataTypeEnum; const ASize: Integer): Boolean;
    procedure ForceZeroValue(Field: TField);
    function IsFieldToForceZero(Field: TField): Boolean;
  strict protected
    procedure GetADOXDataType(const ADataType: TEFDataType;
      const AFieldSize: Integer; out AADOXDataType: DataTypeEnum);
    procedure GetADOXFieldType(const Field : TField;
      out AADOXDataType : DataTypeEnum);
  public
    procedure CreateFileByTable(const AFileName: string;
      const ATableStore: TKViewTableStore;
      const AExcelRangeName: string = '';
      const AAcceptFieldEvent: TAcceptViewFieldEvent = nil;
      const AUseDisplayLabels: Boolean = False);
    procedure CreateFileByDataSet(const AFileName: string;
      const ADataSet: TDataSet;
      const AExcelRangeName: string = '';
      const AAcceptFieldEvent: TAcceptDataFieldEvent = nil;
      const AUseDisplayLabels: Boolean = False);
    procedure FillAdoTable(const AExcelFileName, AExcelRangeName: string;
      const ATableStore: TKViewTableStore;
      AAcceptRecordEvent: TAcceptExportRecordEvent = nil;
      AAcceptFieldEvent: TAcceptViewFieldEvent = nil;
      AUseDisplayLabels: Boolean = false);
    procedure FillAdoTableByDataSet(const AExcelFileName, AExcelRangeName: string;
      const ADataSet: TDataSet;
      AAcceptRecordEvent: TAcceptExportDataRecordEvent = nil;
      AAcceptFieldEvent: TAcceptDataFieldEvent = nil;
      AUseDisplayLabels: Boolean = false);
  end;

  TAcceptImportRecordEvent = procedure(const ADataSet: TDataSet; var AAccept: Boolean) of object;
  TImportSetFieldValue = procedure(const ADestFieldName: string; const AValue : Variant) of object;

  TImportBeforePostRecordEvent = procedure(const AAdoTable: TAdoTable;
    const ANewRecord: TKViewTableRecord) of object;

  TImportDatabaseErrorEvent = procedure(E : Exception; RecordNumber : integer;
    var IgnoreError : Boolean) of object;

  TKExcelImportEngine = class(TKExcelEngine)
  strict private
    FOnDatabaseError: TImportDatabaseErrorEvent;
  protected
  public
    function GetFieldMappingName(const ASourceField: TField;
      const AFieldMappings: TStringList): string;
    procedure ImportFileIntoViewTable(
      const AFileName: string;
      const AViewTable: TKViewTable;
      const AFieldMappings: TStringList = nil;
      const AExcelRangeName: string = '';
      const AOnAcceptRecord: TAcceptImportRecordEvent = nil;
      const AOnAcceptField: TAcceptDataFieldEvent = nil;
      const AOnSetFieldValue: TImportSetFieldValue = nil;
      const AOnBeforePostRecord: TImportBeforePostRecordEvent = nil);
    property OnDatabaseError : TImportDatabaseErrorEvent read FOnDatabaseError write FOnDatabaseError;
  end;

implementation

uses
  Math
  , Variants
  , EF.StrUtils
  , EF.DB
  , EF.Sys
  , Kitto.Metadata.Models
  , Kitto.Config
  , EF.Macros;

const
  ADO_EXCEL_2000 = 'Excel 8.0';
  ADO_EXCEL_2007 = 'Excel 12.0 Xml';

  MAX_EXCEL_STRING_COLUMN_SIZE = 255;
  EXCEL_ADDITIONAL_OPTION = 'IMEX=1;HDR=YES';
  EXCEL_CONN_STRING_12 = 'Provider=Microsoft.ACE.OLEDB.12.0;Data Source=%s;Extended Properties="%s"';
  EXCEL_CONN_STRING = 'Provider=Microsoft.Jet.OLEDB.4.0;Data Source=%s;Extended Properties=''%s''';

  AExcelVersion : Array[TExcelVersion] of string =
    (ADO_EXCEL_2000,
     ADO_EXCEL_2007);

  AExcelExtension : Array[TExcelVersion] of string =
   (EXCEL_FILE_EXT,
     EXCEL_NEW_FILE_EXT);

  AExcelTemplateExtension : Array[TExcelVersion] of string =
   (EXCEL_TEMPLATE_EXT,
     EXCEL_NEW_TEMPLATE_EXT);

{ TKExcelExportEngine }

procedure TKExcelExportEngine.GetADOXDataType(const ADataType: TEFDataType;
  const AFieldSize: Integer; out AADOXDataType: DataTypeEnum);
begin
  if ADataType is TEFMemoDataType then
    AADOXDataType := adLongVarWChar
  else if ADataType is TEFStringDataType then
    AADOXDataType := adVarWChar
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
  if AFieldSize > MAX_EXCEL_STRING_COLUMN_SIZE then
    AAdoXDataType := adLongVarWChar;
end;

procedure TKExcelExportEngine.GetAdoXFieldType(const Field : TField;
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

function TKExcelExportEngine.IsFieldToForceZero(Field: TField): Boolean;
begin
  Result := Field.DataType in [ftDate, ftTime, ftDateTime, ftTimeStamp,
    ftLargeint, ftAutoInc, ftInteger, ftSmallint, ftWord,
    ftFloat, ftBCD, ftCurrency, ftFMTBcd, ftBoolean];
end;

procedure TKExcelExportEngine.ForceZeroValue(Field: TField);
begin
  case Field.DataType of
    ftFixedChar, ftString, ftGuid, ftWideString, ftMemo, ftWideMemo: Field.Value := ' ';
    ftLargeint, ftAutoInc, ftInteger, ftSmallint, ftWord,
    ftFloat, ftBCD, ftCurrency, ftFMTBcd: Field.Value := 0;
    ftDate, ftTime, ftDateTime, ftTimeStamp: Field.Value := 0;
    ftBoolean: Field.Value := False;
  end;
end;

function TKExcelExportEngine.AddExcelColumn(ATable: _Table;
  const AColumnName: string; const ADataType: DataTypeEnum; const ASize: Integer): Boolean;
var
  LColumn: _Column;
  I: integer;
begin
  //Check if column does not exists
  Result := False;
  for I := 0 to ATable.Columns.Count -1 do
  begin
    if SameText(AColumnName, ATable.Columns[I].Name) then
      Exit;
  end;
  LColumn := CoColumn.Create;
  Try
    with LColumn do
    begin
      Set_Name(AColumnName);
      Set_Type_(ADataType);
      if ASize <> 0 then
        Set_DefinedSize(Min(ASize,MAX_EXCEL_STRING_COLUMN_SIZE));
    end;
    ATable.Columns.Append(LColumn, ADataType, ASize);
  Finally
    LColumn := nil;
  End;
  Result := True;
end;

procedure TKExcelExportEngine.CreateExcelSheet(const AViewTableStore: TKViewTableStore;
  AConnectionString, AExcelRangeName: string;
  AAcceptFieldEvent: TAcceptViewFieldEvent = nil;
  AUseDisplayLabels: Boolean = False);
var
  LCatalog: _Catalog;
  LTable: _Table;
  LFieldIndex: Integer;
  LViewTableHeaderField: TKViewTableHeaderField;
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
  for LFieldIndex := 0 to AViewTableStore.Header.FieldCount - 1 do
  begin
    LViewTableHeaderField := AViewTableStore.Header.Fields[LFieldIndex];
    LViewField := LViewTableHeaderField.ViewField;
    if Assigned(LViewField) then
    begin
      if IsAcceptedViewField(LViewField, AAcceptFieldEvent) then
      begin
        if not AUseDisplayLabels then
          LColumnName := LViewTableHeaderField.FieldName
        else
          LColumnName := LViewField.DisplayLabel;
        LFieldSize := LViewField.Size;
        GetADOXDataType(LViewField.ActualDataType, LFieldSize, LADOXDataType);
        AddExcelColumn(LTable, LColumnName, LADOXDataType, LFieldSize);
      end;
    end;
  end;

  //add table to database
  LCatalog.Tables.Append(LTable);

  LTable := nil;
  LCatalog := nil;
end;

procedure TKExcelExportEngine.CreateExcelSheet(const ADataSet: TDataSet;
  AConnectionString, AExcelRangeName: string;
  AAcceptFieldEvent: TAcceptDataFieldEvent = nil;
  AUseDisplayLabels: Boolean = False);
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
    if IsAccepterDataField(LField, AAcceptFieldEvent) then
    begin
      if not AUseDisplayLabels then
        LColumnName := LField.FieldName
      else
        LColumnName := LField.DisplayLabel;
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

procedure TKExcelExportEngine.CreateFileByTable(const AFileName: string;
  const ATableStore: TKViewTableStore;
  const AExcelRangeName: string = '';
  const AAcceptFieldEvent: TAcceptViewFieldEvent = nil;
  const AUseDisplayLabels: Boolean = False);
var
  LConnectionString: string;
begin
  LConnectionString := GetConnectionString(AFileName);
  CreateExcelSheet(ATableStore, LConnectionString, AExcelRangeName,
    AAcceptFieldEvent, AUseDisplayLabels);
end;

procedure TKExcelExportEngine.CreateFileByDataSet(const AFileName: string;
  const ADataSet: TDataSet;
  const AExcelRangeName: string = '';
  const AAcceptFieldEvent: TAcceptDataFieldEvent = nil;
  const AUseDisplayLabels: Boolean = False);
var
  LConnectionString: string;
begin
  LConnectionString := GetConnectionString(AFileName);
  CreateExcelSheet(ADataSet, LConnectionString, AExcelRangeName,
    AAcceptFieldEvent, AUseDisplayLabels)
end;

procedure TKExcelExportEngine.FillAdoTable(const AExcelFileName, AExcelRangeName: string;
  const ATableStore: TKViewTableStore;
  AAcceptRecordEvent: TAcceptExportRecordEvent = nil;
  AAcceptFieldEvent: TAcceptViewFieldEvent = nil;
  AUseDisplayLabels: Boolean = false);
var
  LFirstAccepted: Boolean;
  LRecordIndex, LFieldIndex: Integer;
  LAcceptRecord, LAcceptField: Boolean;
  LRecord: TKViewTableRecord;
  LDestField: TField;
  LSourceField: TKViewTableField;
  LAdoTable: TAdoTable;
  LZeroValueForced: Boolean;

  function FindValidField(const AField: TField;
    const AUseDisplayLabel: Boolean): TKViewTableField;
  var
    LFieldIndex: Integer;
    LField: TKViewTableField;
    LFieldId, LTableFieldId: string;
  begin
    Result := nil;
    for LFieldIndex := 0 to LRecord.FieldCount - 1 do
    begin
      LField := LRecord.Fields[LFieldIndex];
      if not AUseDisplayLabel then
      begin
        LFieldId := AField.FieldName;
        LTableFieldId := LField.HeaderField.FieldName;
      end
      else
      begin
        LFieldId := AField.DisplayLabel;
        LTableFieldId := LField.ViewField.DisplayLabel;
      end;
      if Assigned(LField) and Assigned(LField.ViewField) and
        SameText(LTableFieldId, LFieldId) then
      begin
        Result := LField;
        Break;
      end;
    end;
  end;

begin
  //Opens the Ado table using a range defined inside the excel file
  LAdoTable := OpenExcelRange(AExcelFileName, AExcelRangeName);
  TEFMacroExpansionEngine.Instance.DisableForCurrentThread;
  try
    LFirstAccepted := False;
    LZeroValueForced := False;
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
            LSourceField := FindValidField(LDestField, AUseDisplayLabels);
            LAcceptField := Assigned(LSourceField) and IsAcceptedViewField(LSourceField.ViewField,
              AAcceptFieldEvent);
            if LAcceptField then
            begin
              LZeroValueForced := IsFieldToForceZero(LDestField) or LZeroValueForced;
              if LSourceField.ViewField.ActualDataType is TEFMemoDataType then
              begin
                if not VarIsNull(LSourceField.Value) then
                  LDestField.AsString := StringReplace(LSourceField.AsString, sLineBreak, chr(10), [rfReplaceAll])
                else
                  ForceZeroValue(LDestField);
              end
              else if LSourceField.ViewField.ActualDataType is TEFBooleanDataType then
              begin
                if LDestField is TNumericField then
                  LDestField.AsInteger := Ord(LSourceField.AsBoolean)
                else
                  LDestField.AsBoolean := LSourceField.AsBoolean;
              end
              else if not VarIsNull(LSourceField.Value) then
                LSourceField.AssignValueToField(LDestField)
              else
                ForceZeroValue(LDestField);
            end
            else
              ForceZeroValue(LDestField);
          end;
          LAdoTable.Post;
        except
          LAdoTable.Cancel;
          raise;
        end;
      end;
    end;
  finally
    CloseExcelRange(LAdoTable);
    TEFMacroExpansionEngine.Instance.EnableForCurrentThread;
  end;

  if LZeroValueForced then
  begin
    LAdoTable := OpenExcelRange(AExcelFileName, AExcelRangeName);
    TEFMacroExpansionEngine.Instance.DisableForCurrentThread;
    Try
      //Reassign only blank values with NULL
      LAdoTable.First;
      for LRecordIndex := 0 to ATableStore.RecordCount -1 do
      begin
        LRecord := ATableStore.Records[LRecordIndex];
        LAdoTable.Edit;
        try
          //Update ado tabel fields by the structure of the range
          for LFieldIndex := 0 to LAdoTable.FieldCount - 1 do
          begin
            LDestField := LAdoTable.Fields[LFieldIndex];
            LSourceField := FindValidField(LDestField, AUseDisplayLabels);
            LAcceptField := Assigned(LSourceField) and IsAcceptedViewField(LSourceField.ViewField,
              AAcceptFieldEvent);
            if LAcceptField and IsFieldToForceZero(LDestField) and LSourceField.IsNull then
              LDestField.Clear;
          end;
          LAdoTable.Post;
        except
          LAdoTable.Cancel;
          raise;
        end;
        LAdoTable.Next;
      end;
    finally
      LAdoTable.Close;
      LAdoTable.Free;
      TEFMacroExpansionEngine.Instance.EnableForCurrentThread;
    end;
  end;
end;

procedure TKExcelExportEngine.FillAdoTableByDataSet(const AExcelFileName, AExcelRangeName: string;
  const ADataSet: TDataSet;
  AAcceptRecordEvent: TAcceptExportDataRecordEvent = nil;
  AAcceptFieldEvent: TAcceptDataFieldEvent = nil;
  AUseDisplayLabels: Boolean = false);
var
  LFirstAccepted: Boolean;
  LFieldIndex: Integer;
  LAcceptRecord, LAcceptField: Boolean;
  LDestField: TField;
  LSourceField: TField;
  LAdoTable: TAdoTable;
  LZeroValueForced: Boolean;

  function FindValidField(const AField: TField;
    const AUseDisplayLabel: Boolean): TField;
  var
    LFieldIndex: Integer;
    LField: TField;
    LFieldId: string;
  begin
    Result := nil;
    for LFieldIndex := 0 to ADataSet.FieldCount - 1 do
    begin
      if not AUseDisplayLabel then
        LFieldId := AField.FieldName
      else
        LFieldId := AField.DisplayLabel;
      LField := ADataSet.Fields[LFieldIndex];
      if Assigned(LField) and
        SameText(LField.FieldName, LFieldId) then
      begin
        Result := LField;
        Break;
      end;
    end;
  end;

begin
  //Opens the Ado table using a range defined inside the excel file
  LAdoTable := OpenExcelRange(AExcelFileName, AExcelRangeName);
  try
    LFirstAccepted := False;
    LZeroValueForced := False;
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
            LSourceField := FindValidField(LDestField, AUseDisplayLabels);
            LAcceptField := Assigned(LSourceField) and IsAccepterDataField(LSourceField,
              AAcceptFieldEvent);
            if LAcceptField then
            begin
              LZeroValueForced := IsFieldToForceZero(LDestField) or LZeroValueForced;
              if LSourceField is TMemoField then
              begin
                if not VarIsNull(LSourceField.Value) then
                  LDestField.AsString := StringReplace(LSourceField.AsString, sLineBreak, chr(10), [rfReplaceAll])
                else
                  ForceZeroValue(LDestField);
              end
              else if LSourceField is TBooleanField then
              begin
                if LDestField is TNumericField then
                  LDestField.AsInteger := Ord(LSourceField.AsBoolean)
                else
                  LDestField.AsBoolean := LSourceField.AsBoolean;
              end
              else if not VarIsNull(LSourceField.Value) then
                LDestField.Value := LSourceField.Value
              else
                ForceZeroValue(LDestField);
            end
            else
              ForceZeroValue(LDestField);
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
  if LZeroValueForced then
  begin
    LAdoTable := OpenExcelRange(AExcelFileName, AExcelRangeName);
    Try
      //Reassign only blank values with NULL
      LAdoTable.First;
      ADataSet.First;
      while not ADataSet.Eof do
      begin
        LAdoTable.Edit;
        try
          //Update ado tabel fields by the structure of the range
          for LFieldIndex := 0 to LAdoTable.FieldCount - 1 do
          begin
            LDestField := LAdoTable.Fields[LFieldIndex];
            LSourceField := FindValidField(LDestField, AUseDisplayLabels);
            LAcceptField := Assigned(LSourceField) and IsAccepterDataField(LSourceField,
              AAcceptFieldEvent);
            if LAcceptField and IsFieldToForceZero(LDestField) and LSourceField.IsNull then
              LDestField.Clear;
          end;
          LAdoTable.Post;
        except
          LAdoTable.Cancel;
          raise;
        end;
        LAdoTable.Next;
        ADataSet.Next;
      end;
    finally
      LAdoTable.Close;
      LAdoTable.Free;
    end;
  end;
end;

{ TKExcelImportEngine }

function TKExcelImportEngine.GetFieldMappingName(const ASourceField: TField;
  const AFieldMappings: TStringList): string;
var
  LEqualPos : integer;
  LSourceFieldNameMap, LDestFieldNameMap: string;
  I: Integer;
begin
  Result := ASourceField.FieldName;
  if AFieldMappings.Count > 0 then
  begin
    //If mapping is defined, only mapped fields are accepted
    Result := '';
    for I := 0 to AFieldMappings.Count -1 do
    begin
      LEqualPos := pos('=', AFieldMappings.Strings[i]);
      if LEqualPos <= 1 then
        Continue;
      LSourceFieldNameMap := Copy(AFieldMappings.Strings[i],1,LEqualPos-1);
      LDestFieldNameMap := Copy(AFieldMappings.Strings[i],LEqualPos+1,MaxInt);
      if SameText(LSourceFieldNameMap, ASourceField.FieldName) and (LDestFieldNameMap <> '') then
      begin
        Result := LDestFieldNameMap;
        break;
      end;
    end;
  end;
end;

procedure TKExcelImportEngine.ImportFileIntoViewTable(
  const AFileName: string;
  const AViewTable: TKViewTable;
  const AFieldMappings: TStringList = nil;
  const AExcelRangeName: string = '';
  const AOnAcceptRecord: TAcceptImportRecordEvent = nil;
  const AOnAcceptField: TAcceptDataFieldEvent = nil;
  const AOnSetFieldValue: TImportSetFieldValue = nil;
  const AOnBeforePostRecord: TImportBeforePostRecordEvent = nil);
var
  LStore: TKViewTableStore;
  LAdoTable: TAdoTable;
  LAddedRecord: TKViewTableRecord;
  LDefaultValues: TEFNode;
  LAccept : Boolean;
  LIgnoreError : Boolean;
  J : integer;
  LSourceField : TField;
  LDestFieldName: string;
  LDestField: TKViewTableField;
  LFieldValue: Variant;
begin
  LAdoTable := OpenExcelRange(AFileName, AExcelRangeName);
  LStore := nil;
  Try
    LStore := AViewTable.CreateStore;
    LAdoTable.First;
    //Ciclo sui records e li aggiungo al dataset di destinazione in base alla mappatura
    while not LAdoTable.Eof do
    begin
      LAddedRecord := nil;
      //If the first field of the Excel Table is empty the record is not accepted
      LAccept := not LAdoTable.Fields[0].IsNull;
      if Assigned(AOnAcceptRecord) then
        AOnAcceptRecord(LAdoTable, LAccept);
      if LAccept then
      begin
        //Create a new record into ViewTable
        LAddedRecord := LStore.Records.AppendAndInitialize;

        LDefaultValues := AViewTable.GetDefaultValues;
        try
          LAddedRecord.Store.DoWithChangeNotificationsDisabled(
            procedure
            begin
              LAddedRecord.ReadFromNode(LDefaultValues);
            end);
          LAddedRecord.ApplyNewRecordRulesAndFireEvents(AViewTable, False);
        finally
          FreeAndNil(LDefaultValues);
        end;

        try
          for j := 0 to LAdoTable.FieldCount - 1 do
          begin
            LSourceField := LAdoTable.Fields[j];
            //Verify if field is accepted
            LDestFieldName := GetFieldMappingName(LSourceField, AFieldMappings);
            if LDestFieldName <> '' then
              LDestField := LAddedRecord.FindField(LDestFieldName)
            else
              LDestField := nil;
            //Verify if target field exists
            LAccept := (LDestFieldName <> '') and (LDestField <> nil);
            if LAccept and Assigned(AOnAcceptField) then
              AOnAcceptField(LSourceField, LAccept);
            if LAccept then
            begin
              LFieldValue := LSourceField.Value;
              if Assigned(AOnSetFieldValue) then
                AOnSetFieldValue(LDestFieldName, LFieldValue);
              LDestField.Value := LFieldValue;
            end;
          end;
          LAddedRecord.ApplyNewRecordRules;
        except
          on E: Exception do
          begin
            if Assigned(FOnDatabaseError) then
            begin
              LIgnoreError := False;
              FOnDatabaseError(E, LAdoTable.RecNo, LIgnoreError);
            end
            else
              LIgnoreError := False;
            if not LIgnoreError then
              raise;
          end;
        end;
      end;
      if Assigned(AOnBeforePostRecord) then
        AOnBeforePostRecord(LAdoTable, LAddedRecord);
      LAdoTable.Next;
    end;
    //Store all records
    AViewTable.Model.SaveRecords(LStore, True, nil);

  Finally
    FreeAndNil(LStore);
    CloseExcelRange(LAdoTable);
  End;
end;

{ TKExcelEngine }

procedure TKExcelEngine.CloseExcelRange(const AAdoTable: TAdoTable);
begin
  AAdoTable.Close;
  AAdoTable.ConnectionString := '';
  AAdoTable.Free;
end;

function TKExcelEngine.GetConnectionString(const ExcelFileName: string): string;
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

function TKExcelEngine.OpenExcelRange(const AExcelFileName, AExcelRangeName: string): TAdoTable;
begin
  //Opens the Ado table using a range defined inside the excel file
  Result := TAdoTable.Create(nil);
  try
    Result.CursorType := ctStatic;
    Result.ConnectionString := GetConnectionString(AExcelFileName);
    Result.TableName := AExcelRangeName;
    Result.Open;
  except
    Result.Free;
    raise;
  end;
end;

function TKExcelEngine.ValidColumnName(Field: TField): string;
begin
  Result := StringReplace(Field.FieldName, ' ', '_', [rfReplaceAll]);
  Result := StringReplace(Field.FieldName, '.', '_', [rfReplaceAll]);
//  Result := KillChars(Result,['(',')','!','\','=',CR,LF]);
end;

function TKExcelEngine.IsAcceptedViewField(const AViewField: TKViewField;
  AAcceptFieldEvent: TAcceptViewFieldEvent = nil): Boolean;
begin
  Assert(Assigned(AViewField));
  Result := AViewField.IsVisible;
  if Assigned(AAcceptFieldEvent) then
    AAcceptFieldEvent(AViewField, Result);
end;

function TKExcelEngine.IsAccepterDataField(const AField: TField;
  AAcceptFieldEvent: TAcceptDataFieldEvent = nil): Boolean;
begin
  Assert(Assigned(AField));
  Result := AField.Visible;
  if Assigned(AAcceptFieldEvent) then
    AAcceptFieldEvent(AField, Result);
end;

end.
