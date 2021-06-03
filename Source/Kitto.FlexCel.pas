{-------------------------------------------------------------------------------
   Copyright 2019 Ethea S.r.l.

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
unit Kitto.FlexCel;


interface

uses
  SysUtils, Classes,
  DB,
  //FlexCel
  FlexCel.XlsAdapter, FlexCel.Report, VCL.FlexCel.Core, FlexCel.Render,
  __FlexCelTypes, _UVirtualDataTable, _UReportEnums.TDisposeMode,
  Kitto.Metadata.DataView, EF.Tree;

const
  EXCEL_FILE_EXT = '.xls';
  EXCEL_TEMPLATE_EXT = '.xlt';
  EXCEL_NEW_FILE_EXT = '.xlsx';
  EXCEL_NEW_TEMPLATE_EXT = '.xltx';
  EXCEL_DEFAULT_RANGE = 'DataRange';
  MAX_EXCEL_STRING_COLUMN_SIZE = 255;
  DEF_FmtFCInt = '0';
  DEF_FmtFCDouble = '0.000000';
  DEF_FmtFCCurrency = '#,##0.00 €;-#,##0.00 €';

type
  TExcelVersion = (ex2000, ex2007);

  TAcceptViewFieldEvent = procedure(AViewField: TKViewField; var AAccept: boolean) of object;
  TAcceptDataFieldEvent = procedure(AField: TField; var AAccept: boolean) of object;

  TKFlexCelEngine = class
  protected
    function ValidColumnName(Field: TField): string;
    function IsAcceptedViewField(const AViewField: TKViewField;
      AAcceptFieldEvent: TAcceptViewFieldEvent = nil): Boolean; overload;
    function IsAccepterDataField(const AField: TField;
      AAcceptFieldEvent: TAcceptDataFieldEvent = nil): Boolean; overload;
  end;

  TAcceptExportRecordEvent = procedure(ARecord: TKViewTableRecord; var AAccept: boolean) of object;
  TAcceptExportDataRecordEvent = procedure(ADataSet: TDataSet; var AAccept: boolean) of object;

  TKFlexCelExportEngine = class(TKFlexCelEngine)
  strict private
    FFmtFCString : TFlxFormat;
    FFmtFCMemo : TFlxFormat;
    FFmtFCInteger : TFlxFormat;
    FFmtFCDouble : TFlxFormat;
    FFmtFCCurrency : TFlxFormat;
    FFmtFCDate : TFlxFormat;
    FFmtFCTime : TFlxFormat;
    FFmtFCDateTime : TFlxFormat;
    procedure CreateExcelSheet(var AExcelFile : TExcelFile;
    const AViewTableStore: TKViewTableStore;
      AExcelRangeName: string;
      AAcceptFieldEvent: TAcceptViewFieldEvent = nil;
      AUseDisplayLabels: Boolean = False); overload;
    procedure CreateExcelSheet(var AExcelFile : TExcelFile;
      const ADataSet: TDataSet;
      AExcelRangeName: string;
      AAcceptFieldEvent: TAcceptDataFieldEvent = nil;
      AUseDisplayLabels: Boolean = False); overload;
    procedure AddExcelColumn(var ExcelFile : TExcelFile; ColName : string; pos :integer);
    procedure ForceZeroValue(Field: TField);
    function IsFieldToForceZero(Field: TField): Boolean;
    procedure ImpostaFormatFlexCel(AExcelFile : TExcelFile);
  strict protected
    procedure GetFormatFlexCelField(AField: TField;
      out AFmtFlexCel: TFlxFormat; out AFieldSize : integer); overload;
    procedure GetFormatFlexCelDataType(const ADataType: TEFDataType;
      const AFieldSize : integer; out AFmtFlexCel: TFlxFormat); overload;
  public
    procedure CreateFileByTable(const AFileName: string;
      const ATableStore: TKViewTableStore;
      const AExcelRangeName: string = '';
      const AAcceptRecordEvent: TAcceptExportRecordEvent = nil;
      const AAcceptFieldEvent: TAcceptViewFieldEvent = nil;
      const AUseDisplayLabels: Boolean = False);
    procedure CreateFileByDataSet(const AFileName: string;
      const ADataSet: TDataSet;
      const AExcelRangeName: string = '';
      const AAcceptRecordEvent: TAcceptExportDataRecordEvent = nil;
      const AAcceptFieldEvent: TAcceptDataFieldEvent = nil;
      const AUseDisplayLabels: Boolean = False);
    procedure CreateFileByTableWithTemplate(const AExcelFileName, ATemlateName, AExcelRangeName: string;
      const ATableStore: TKViewTableStore;
      AAcceptRecordEvent: TAcceptExportRecordEvent = nil;
      AAcceptFieldEvent: TAcceptViewFieldEvent = nil;
      AUseDisplayLabels: Boolean = false);
    procedure CreateFileByDataSetWithTemplate(const AExcelFileName, ATemlateName, AExcelRangeName: string;
      const ADataSet: TDataSet;
      AAcceptRecordEvent: TAcceptExportDataRecordEvent = nil;
      AAcceptFieldEvent: TAcceptDataFieldEvent = nil;
      AUseDisplayLabels: Boolean = false);
  end;

  TKFlexCelVirtualTable = class(TVirtualDataTable)
  strict protected
    function Get_ColumnCount: Int32; override;
  public
    FViewTableStore: TKViewTableStore;
    FAcceptRecordEvent : TAcceptExportRecordEvent;

    constructor Create(const aTableName: UTF16String; const aCreatedBy: TVirtualDataTable; const aTableStore: TKViewTableStore; const AAcceptRecordEvent: TAcceptExportRecordEvent = nil); overload;

    function CreateState(const sort: UTF16String; const masterDetailLinks: TMasterDetailLinkArray; const splitLink: TSplitLink): TVirtualDataTableState;  override;

    function GetColumn(const columnName: UTF16String): Int32; override;

    function GetColumnName(const columnIndex: Int32): UTF16String; override;

    function GetColumnCaption(const columnIndex: Int32): UTF16String; override;

  end;

  TKFlexCelVirtualTableState = class(TVirtualDataTableState)
  strict private
     FViewTableStore: TKViewTableStore;
     FKRecord: TKViewTableRecord;
     FPosRecord : integer;
     FAcceptRecordEvent : TAcceptExportRecordEvent;
  strict protected
    function Get_RowCount: Int32; override;
 public
    constructor Create(const aTableData: TVirtualDataTable; const aTableStore: TKViewTableStore; const AAcceptRecordEvent: TAcceptExportRecordEvent = nil); overload;

    function Eof: Boolean; override;
    procedure MoveFirst; override;
    procedure MoveNext; override;
    function GetValue(const column: Int32): TReportValue; overload; override;
    function GetValue(const row: Int32; const column: Int32): TReportValue; overload; override;
  end;

implementation

uses
  Math, Variants,
  EF.StrUtils, EF.DB,
  Kitto.Metadata.Models, Kitto.Config, EF.Macros;

const
  ADO_EXCEL_2000 = 'Excel 8.0';
  ADO_EXCEL_2007 = 'Excel 12.0 Xml';

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

function GetReportValue(const AField: TKViewTableField): TReportValue;
begin
  if AField.ViewField.ActualDataType is TEFMemoDataType then
   exit(AField.AsString)
  else if AField.ViewField.ActualDataType is TEFStringDataType then
    exit(AField.AsString)
  else if AField.ViewField.ActualDataType is TEFIntegerDataType then
    exit(AField.AsInteger)
  else if (AField.ViewField.ActualDataType is TEFDateDataType) then
    exit(AField.AsDateTime)
  else if (AField.ViewField.ActualDataType is TEFDateTimeDataType) then
    exit(AField.AsDateTime)
  else if (AField.ViewField.ActualDataType is TEFTimeDataType) then
    exit(AField.AsDateTime)
  else if AField.ViewField.ActualDataType is TEFBooleanDataType then
    exit(AField.AsBoolean)
  else if (AField.ViewField.ActualDataType is TEFFloatDataType) or (AField.ViewField.ActualDataType is TEFDecimalDataType) then
    exit(AField.AsFloat)
  else if AField.ViewField.ActualDataType is TEFCurrencyDataType then
    exit(AField.AsCurrency);

  Result := TCellValue(AField.Value);
end;

{ TKFlexCelExportEngine }

procedure TKFlexCelExportEngine.GetFormatFlexCelField(AField: TField;
  out AFmtFlexCel: TFlxFormat; out AFieldSize: integer);
begin
  case AField.DataType of
    ftFixedChar, ftString, ftGuid : AFmtFlexCel := FFmtFCString;
    ftWideString : AFmtFlexCel := FFmtFCString;
    ftMemo : AFmtFlexCel := FFmtFCMemo;
    ftSmallint, ftWord, ftLargeint, ftAutoInc, ftInteger : AFmtFlexCel := FFmtFCInteger;
    ftFloat, ftBCD : AFmtFlexCel := FFmtFCDouble;
    ftCurrency : AFmtFlexCel := FFmtFCCurrency;
    ftFMTBcd : AFmtFlexCel := FFmtFCDouble;
    ftBoolean :
    begin
   //   if FBooleanAsInteger then
 //       FmtFlexCel := FFmtFCInteger
  //    else
        AFmtFlexCel := FFmtFCString;
    end;
    ftDate, ftDateTime : AFmtFlexCel := FFmtFCDate;
    ftTime : AFmtFlexCel := FFmtFCTime;
    ftTimeStamp : AFmtFlexCel := FFmtFCDateTime
  else
    AFmtFlexCel := FFmtFCString;
  end;

  AFieldSize := AField.Size;
  if AFieldSize > MAX_EXCEL_STRING_COLUMN_SIZE then
    AFmtFlexCel := FFmtFCMemo;
end;

procedure TKFlexCelExportEngine.ImpostaFormatFlexCel(AExcelFile: TExcelFile);
begin
  //recupera il default format
  FFmtFCString := AExcelFile.GetDefaultFormat;

  FFmtFCMemo := AExcelFile.GetDefaultFormat;
  //Modifica testo a capo
  FFmtFCMemo.WrapText := True;
  FFmtFCInteger := AExcelFile.GetDefaultFormat;
  //Modifica format campo numerico
  FFmtFCInteger.Format := DEF_FmtFCInt;

  FFmtFCDouble := AExcelFile.GetDefaultFormat;
  //Modifica format campo numerico con decimali
  FFmtFCDouble.Format := DEF_FmtFCDouble;

  FFmtFCCurrency := AExcelFile.GetDefaultFormat;
  //Modifica format campo valuta
  FFmtFCCurrency.Format := DEF_FmtFCCurrency;

  FFmtFCDate := AExcelFile.GetDefaultFormat;
  //Modifica format campo data
  FFmtFCDate.Format := FormatSettings.ShortDateFormat;

  FFmtFCTime := AExcelFile.GetDefaultFormat;
  //Modifica format campo ora
  FFmtFCTime.Format := FormatSettings.LongTimeFormat;

  FFmtFCDateTime := AExcelFile.GetDefaultFormat;
  //Modifica format campo data-ora
  FFmtFCDateTime.Format := FormatSettings.ShortDateFormat+' '+FormatSettings.LongTimeFormat;

end;

function TKFlexCelExportEngine.IsFieldToForceZero(Field: TField): Boolean;
begin
  Result := Field.DataType in [ftDate, ftTime, ftDateTime, ftTimeStamp,
    ftLargeint, ftAutoInc, ftInteger, ftSmallint, ftWord,
    ftFloat, ftBCD, ftCurrency, ftFMTBcd, ftBoolean];
end;

procedure TKFlexCelExportEngine.ForceZeroValue(Field: TField);
begin
  case Field.DataType of
    ftFixedChar, ftString, ftGuid, ftWideString, ftMemo, ftWideMemo: Field.Value := ' ';
    ftLargeint, ftAutoInc, ftInteger, ftSmallint, ftWord,
    ftFloat, ftBCD, ftCurrency, ftFMTBcd: Field.Value := 0;
    ftDate, ftTime, ftDateTime, ftTimeStamp: Field.Value := 0;
    ftBoolean: Field.Value := False;
  end;
end;

procedure TKFlexCelExportEngine.GetFormatFlexCelDataType(const ADataType: TEFDataType;
  const AFieldSize: integer; out AFmtFlexCel: TFlxFormat);
begin
  if ADataType is TEFMemoDataType then
    AFmtFlexCel := FFmtFCMemo
  else if ADataType is TEFStringDataType then
    AFmtFlexCel := FFmtFCString
  else if ADataType is TEFIntegerDataType then
    AFmtFlexCel := FFmtFCInteger
  else if (ADataType is TEFDateDataType) then
    AFmtFlexCel := FFmtFCDate
  else if (ADataType is TEFDateTimeDataType) then
    AFmtFlexCel := FFmtFCDateTime
  else if (ADataType is TEFTimeDataType) then
    AFmtFlexCel := FFmtFCTime
  else if ADataType is TEFBooleanDataType then
    AFmtFlexCel := FFmtFCString
  else if (ADataType is TEFFloatDataType) or (ADataType is TEFDecimalDataType) then
    AFmtFlexCel := FFmtFCDouble
  else if ADataType is TEFCurrencyDataType then
    AFmtFlexCel := FFmtFCCurrency
  else if AFieldSize <= MAX_EXCEL_STRING_COLUMN_SIZE then
    AFmtFlexCel := FFmtFCString
  else
    AFmtFlexCel := FFmtFCMemo;
  if AFieldSize > MAX_EXCEL_STRING_COLUMN_SIZE then
    AFmtFlexCel := FFmtFCMemo;
end;

procedure TKFlexCelExportEngine.AddExcelColumn(var ExcelFile : TExcelFile; ColName : string; pos :integer);
var
  PosFmt : integer;
begin
  PosFmt :=  ExcelFile.AddFormat(FFmtFCString) ;
  ExcelFile.SetCellValue(1, pos, ColName, PosFmt );
end;

procedure TKFlexCelExportEngine.CreateExcelSheet(var AExcelFile : TExcelFile;
  const AViewTableStore: TKViewTableStore;
  AExcelRangeName: string;
  AAcceptFieldEvent: TAcceptViewFieldEvent = nil;
  AUseDisplayLabels: Boolean = False);
var
  LFieldIndex: Integer;
  LViewTableHeaderField: TKViewTableHeaderField;
  LViewField: TKViewField;
  LColumnName: string;
  NumColExcel, j : integer;
begin
  AExcelFile.NewFile;

  //cancello tutti i fogli TRANNE UNO
  for j := AExcelFile.SheetCount-1 downto 1 do
    AExcelFile.DeleteSheet(1);
  //Attivo l'unico foglio rimasto
  AExcelFile.ActiveSheet := AExcelFile.SheetCount;
  //lo rinomino
  AExcelFile.SheetName := AExcelRangeName;

  NumColExcel := 0;
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

        NumColExcel := NumColExcel + 1;
        AddExcelColumn(AExcelFile, LColumnName, NumColExcel);
      end;
    end;
  end;
end;

procedure TKFlexCelExportEngine.CreateExcelSheet(var AExcelFile : TExcelFile;
  const ADataSet: TDataSet;
  AExcelRangeName: string;
  AAcceptFieldEvent: TAcceptDataFieldEvent = nil;
  AUseDisplayLabels: Boolean = False);
var
  LFieldIndex: Integer;
  LField: TField;
  LColumnName: string;
  NumColExcel, j : integer;
begin
  AExcelFile.NewFile;

  //cancello tutti i fogli TRANNE UNO
  for j := AExcelFile.SheetCount-1 downto 1 do
    AExcelFile.DeleteSheet(1);
  //Attivo l'unico foglio rimasto
  AExcelFile.ActiveSheet := AExcelFile.SheetCount;
  //lo rinomino
  AExcelFile.SheetName := AExcelRangeName;

  NumColExcel := 0;

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
      NumColExcel := NumColExcel + 1;
      AddExcelColumn(AExcelFile, LColumnName, NumColExcel);
    end;
  end;
end;

procedure TKFlexCelExportEngine.CreateFileByTable(const AFileName: string;
  const ATableStore: TKViewTableStore;
  const AExcelRangeName: string = '';
  const AAcceptRecordEvent: TAcceptExportRecordEvent = nil;
  const AAcceptFieldEvent: TAcceptViewFieldEvent = nil;
  const AUseDisplayLabels: Boolean = False);
var
  LRecordIndex, LFieldIndex: Integer;
  LAcceptRecord, LAcceptField: Boolean;
  LRecord: TKViewTableRecord;
  LSourceField: TKViewTableField;
  LZeroValueForced: Boolean; //TO_DO
  LExcelFile : TExcelFile;
  RowNum : integer;
  FmtFlexCel : TFLXFormat;
  FieldSize : integer;
  PosFmt : integer;
  NumColExcel : integer;
begin
  LExcelFile := TXlsFile.Create(1, TExcelFileFormat.v2016, True);
  TEFMacroExpansionEngine.Instance.DisableForCurrentThread;
  Try
    ImpostaFormatFlexCel(LExcelFile);

    CreateExcelSheet(LExcelFile, ATableStore, AExcelRangeName, AAcceptFieldEvent, AUseDisplayLabels);
    LZeroValueForced := False;
    RowNum := 1;
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
        RowNum := RowNum + 1;
        NumColExcel := 0;
        //Update ado tabel fields by the structure of the range
        for LFieldIndex := 0 to LRecord.FieldCount - 1 do
        begin
          LSourceField := LRecord.Fields[LFieldIndex];
          LAcceptField := Assigned(LSourceField) and IsAcceptedViewField(LSourceField.ViewField,
            AAcceptFieldEvent);

          if LAcceptField then
          begin
            NumColExcel := NumColExcel + 1;
            FieldSize := LSourceField.ViewField.Size;
            GetFormatFlexCelDataType(LSourceField.ViewField.ActualDataType, FieldSize, FmtFlexCel );
            PosFmt :=  LExcelFile.AddFormat(FmtFlexCel) ;

            //TODO boolean to number
         //   if (LSourceField.ViewField.ActualDataType is TEFBooleanDataType) then
           //   LExcelFile.SetCellValue(RowNum, NumColExcel, Ord(LSourceField.AsBoolean), PosFmt)
            //else
              LExcelFile.SetCellValue(RowNum, NumColExcel, LSourceField.Value, PosFmt);

           // if FmtFlexCel = FFmtFCMemo then
           //   LExcelFile.AutofitCol(NumColExcel, False, 1);
          end
        end;
      end;
    end;

    LExcelFile.Save(AFileName);
  Finally
    LExcelFile.Free;
    TEFMacroExpansionEngine.Instance.EnableForCurrentThread;
  End;
end;

procedure TKFlexCelExportEngine.CreateFileByDataSet(const AFileName: string;
  const ADataSet: TDataSet;
  const AExcelRangeName: string = '';
  const AAcceptRecordEvent: TAcceptExportDataRecordEvent = nil;
  const AAcceptFieldEvent: TAcceptDataFieldEvent = nil;
  const AUseDisplayLabels: Boolean = False);
var
  LAcceptRecord, LAcceptField: Boolean;
  LDestField: TField;
  LSourceField: TField;
  LZeroValueForced: Boolean;
  LExcelFile : TExcelFile;
  RowNum : integer;
  FmtFlexCel : TFLXFormat;
  FieldSize : integer;
  PosFmt : integer;
  NumColExcel : integer;
  J : integer;
begin
  LExcelFile := TXlsFile.Create(1, TExcelFileFormat.v2016, True);
  Try
    //Imposta i format per FlexCel
    ImpostaFormatFlexCel(LExcelFile);
    //Create del foglio excel
    CreateExcelSheet(LExcelFile, ADataSet, AExcelRangeName, AAcceptFieldEvent, AUseDisplayLabels);

    if not ADataSet.Active then
      ADataSet.Open
    else
      ADataSet.First;

    with ADataSet do
    begin
      First;
      RowNum := 0;
      while not Eof do
      begin
        //only the first record of the range is edited, then appends new records
        LAcceptRecord := True;
        if Assigned(AAcceptRecordEvent) then
          AAcceptRecordEvent(ADataSet, LAcceptRecord);
        if LAcceptRecord then
        begin
          RowNum := RowNum + 1;
          NumColExcel := 0;
          for J := 0 to FieldCount - 1 do
          begin
            LSourceField := Fields[J];
            LAcceptField := Assigned(LSourceField) and IsAccepterDataField(LSourceField, AAcceptFieldEvent);
            if LAcceptField then
            begin
              NumColExcel := NumColExcel + 1;
              GetFormatFlexCelField(LSourceField,FmtFlexCel, FieldSize );
              PosFmt :=  LExcelFile.AddFormat(FmtFlexCel) ;

            //  if (SourceField.DataType = ftBoolean) then
           //     LExcelFile.SetCellValue(RowNum, NumColExcel, Ord(LSourceField.AsBoolean), PosFmt)
          //    else
                LExcelFile.SetCellValue(RowNum, NumColExcel, LSourceField.Value, PosFmt);
            end;
          end;
        end;
        Next;
      end;
    end;

    LExcelFile.Save(AFileName);
  Finally
    LExcelFile.Free;
  End;
end;

procedure TKFlexCelExportEngine.CreateFileByTableWithTemplate(const AExcelFileName, ATemlateName, AExcelRangeName: string;
  const ATableStore: TKViewTableStore;
  AAcceptRecordEvent: TAcceptExportRecordEvent = nil;
  AAcceptFieldEvent: TAcceptViewFieldEvent = nil;
  AUseDisplayLabels: Boolean = false);
var
  LReport : TFlexCelReport;
begin
  //Crea XLS da template
  LReport := TFlexCelReport.Create(True);
  Try
    LReport.AddTable(AExcelRangeName, TKFlexCelVirtualTable.Create(AExcelRangeName, nil, ATableStore), TDisposeMode.DisposeAfterRun);
    LReport.Run(ATemlateName,
               AExcelFileName);
  Finally
    LReport.Free;
  end;
end;

procedure TKFlexCelExportEngine.CreateFileByDataSetWithTemplate(const AExcelFileName, ATemlateName, AExcelRangeName: string;
  const ADataSet: TDataSet;
  AAcceptRecordEvent: TAcceptExportDataRecordEvent = nil;
  AAcceptFieldEvent: TAcceptDataFieldEvent = nil;
  AUseDisplayLabels: Boolean = false);
var
  LReport : TFlexCelReport;
begin
  //Crea XLS da template
  LReport := TFlexCelReport.Create(True);
  Try
    LReport.AddTable(AExcelRangeName, ADataSet);
    LReport.Run(ATemlateName,
               AExcelFileName);
  Finally
    LReport.Free;
  end;
end;

{ TKFlexCelEngine }

function TKFlexCelEngine.ValidColumnName(Field: TField): string;
begin
  Result := StringReplace(Field.FieldName, ' ', '_', [rfReplaceAll]);
  Result := StringReplace(Field.FieldName, '.', '_', [rfReplaceAll]);
//  Result := KillChars(Result,['(',')','!','\','=',CR,LF]);
end;

function TKFlexCelEngine.IsAcceptedViewField(const AViewField: TKViewField;
  AAcceptFieldEvent: TAcceptViewFieldEvent = nil): Boolean;
begin
  Assert(Assigned(AViewField));
  Result := AViewField.IsVisible;
  if Assigned(AAcceptFieldEvent) then
    AAcceptFieldEvent(AViewField, Result);
end;

function TKFlexCelEngine.IsAccepterDataField(const AField: TField;
  AAcceptFieldEvent: TAcceptDataFieldEvent = nil): Boolean;
begin
  Assert(Assigned(AField));
  Result := AField.Visible;
  if Assigned(AAcceptFieldEvent) then
    AAcceptFieldEvent(AField, Result);
end;

{ TKFlexCelVirtualTable }

function TKFlexCelVirtualTable.Get_ColumnCount: Int32;
begin
  Result := FViewTableStore.Header.FieldCount;
end;

constructor TKFlexCelVirtualTable.Create(const aTableName: UTF16String;
  const aCreatedBy: TVirtualDataTable; const aTableStore: TKViewTableStore;
  const AAcceptRecordEvent: TAcceptExportRecordEvent = nil);
begin
  inherited Create(aTableName, aCreatedBy);
  FViewTableStore := aTableStore;
  FAcceptRecordEvent := AAcceptRecordEvent;
end;

function TKFlexCelVirtualTable.CreateState(const sort: UTF16String;
  const masterDetailLinks: TMasterDetailLinkArray;
  const splitLink: TSplitLink): TVirtualDataTableState;
begin
  Result := TKFlexCelVirtualTableState.Create(Self, FViewTableStore, FAcceptRecordEvent);
end;

function TKFlexCelVirtualTable.GetColumn(const columnName: UTF16String): Int32;
var
  LFieldIndex: Integer;
  LField: TKViewTableHeaderField;
  LFieldId, LTableFieldId: string;
begin
  Result := -1;
  for LFieldIndex := 0 to FViewTableStore.Header.FieldCount - 1 do
  begin
    LField := FViewTableStore.Header.Fields[LFieldIndex];

//    if not AUseDisplayLabel then
//    begin
      LFieldId := columnName;
      LTableFieldId := LField.FieldName;
//    end
//    else
//    begin
//      LFieldId :=  columnName;
//      LTableFieldId := LField.ViewField.DisplayLabel;
//    end;

    if Assigned(LField) and Assigned(LField.ViewField) and
      SameText(LTableFieldId, LFieldId) then
    begin
      Result := LField.Index;
      Break;
    end;
  end;
end;

function TKFlexCelVirtualTable.GetColumnName(const columnIndex: Int32): UTF16String;
begin
  Result := FViewTableStore.Header.Fields[columnIndex].FieldName;
end;

function TKFlexCelVirtualTable.GetColumnCaption(const columnIndex: Int32): UTF16String;
begin
  Result := FViewTableStore.Header.Fields[columnIndex].ViewField.DisplayLabel;
end;

{ TKFlexCelVirtualTableState }

function TKFlexCelVirtualTableState.GetValue(const column: Int32): TReportValue;
begin
  Result := GetReportValue(FKRecord.Fields[column]);
end;

constructor TKFlexCelVirtualTableState.Create(
  const aTableData: TVirtualDataTable; const aTableStore: TKViewTableStore;
  const AAcceptRecordEvent: TAcceptExportRecordEvent = nil);
begin
  inherited Create(aTableData);
  FViewTableStore := aTableStore;
  FAcceptRecordEvent := AAcceptRecordEvent;
end;

function TKFlexCelVirtualTableState.Eof: Boolean;
begin
  Result := not (FPosRecord < FViewTableStore.Records.RecordCount)
end;

function TKFlexCelVirtualTableState.GetValue(const row,
  column: Int32): TReportValue;
begin
  Result := GetReportValue(FViewTableStore.Records[row].Fields[column]);
end;

function TKFlexCelVirtualTableState.Get_RowCount: Int32;
begin
  Result := FViewTableStore.Records.RecordCount;
end;

procedure TKFlexCelVirtualTableState.MoveFirst;
var
  LAcceptRecord : Boolean;
begin
  FPosRecord := 0;
  while (FPosRecord < FViewTableStore.Records.RecordCount) do
  begin
    FKRecord := FViewTableStore.Records[FPosRecord];
    LAcceptRecord := not FKRecord.IsDeleted;

    if Assigned(FAcceptRecordEvent) then
      FAcceptRecordEvent(FKRecord, LAcceptRecord);

    if LAcceptRecord then
      Break;

    Inc(FPosRecord);
  end;
end;

procedure TKFlexCelVirtualTableState.MoveNext;
var
  LAcceptRecord : Boolean;
begin
  while (FPosRecord < FViewTableStore.Records.RecordCount) do
  begin
    Inc(FPosRecord);
    FKRecord := FViewTableStore.Records[FPosRecord];
    LAcceptRecord := not FKRecord.IsDeleted;

    if Assigned(FAcceptRecordEvent) then
      FAcceptRecordEvent(FKRecord, LAcceptRecord);

    if LAcceptRecord then
      Break;
  end;
end;

end.
