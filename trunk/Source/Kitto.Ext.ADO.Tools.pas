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

unit Kitto.Ext.ADO.Tools;

interface

uses
  DB, ADODB, ADOX_TypeLibrary,
  SysUtils, Classes,
  Kitto.Ext.Controller, Kitto.Ext.DataTool, Kitto.Ext.Base, Kitto.Ext.Tools,
  Kitto.Metadata.DataView, Kitto.Ext.StandardControllers;

type
  TExcelVersion = (ex2000, ex2007);

  TExportExcelToolController = class(TExportFileToolController)
  private
    FTempFileName: string;
    procedure StoreToExcelViaAdo(const AStore: TKViewTableStore);
    function GetExcelRangeName: string;
    procedure CreateExcelSheet(AConnectionString, AExcelRangeName: string);
    function GetConnectionString(ExcelFileName: string): string;
    procedure GetAdoXFieldType(const AFieldType: TFieldType;
      const AFieldSize: integer; out AdoXDataType: DataTypeEnum);
    function ValidField(AViewField: TKViewField): boolean;
    procedure DeleteTempFile;
  protected
    procedure ExecuteTool; override;
    function GetDefaultFileExtension: string; override;
  public
  published
    procedure DownloadFile;
    property ExcelRangeName: string read GetExcelRangeName;
  end;

implementation

uses
  Math,
  Ext, EF.DB, EF.SysUtils, EF.Tree,
  Kitto.Metadata.Models, Kitto.Ext.Session, Kitto.Config;

const
  ADO_EXCEL_2000 = 'Excel 8.0';
  ADO_EXCEL_2007 = 'Excel 12.0 Xml';

  MAX_EXCEL_STRING_COLUMN_SIZE = 255;
  EXCEL_FILE_EXT = '.xls';
  EXCEL_TEMPLATE_EXT = '.xlt';
  EXCEL_NEW_FILE_EXT = '.xlsx';
  EXCEL_NEW_TEMPLATE_EXT = '.xltx';
  EXCEL_ADDITIONAL_OPTION = 'IMEX=1;HDR=YES';
  EXCEL_CONN_STRING_12 = 'Provider=Microsoft.ACE.OLEDB.12.0;Data Source=%s;Extended Properties=''%s''';
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

{ TExportExcelToolController }

procedure TExportExcelToolController.DeleteTempFile;
begin
  if FileExists(FTempFileName) then
    DeleteFile(FTempFileName);
end;

procedure TExportExcelToolController.DownloadFile;
begin
  DownloadBinaryFile(FTempFileName);
end;

procedure TExportExcelToolController.ExecuteTool;
begin
  inherited;
  StoreToExcelViaAdo(ServerStore);
  Download(DownloadFile);
end;

function TExportExcelToolController.GetConnectionString(ExcelFileName: string): string;
var
  FileExt: string;
  ExcelVersion: TExcelVersion;
  ConnectionString: string;
  AdditionalOptions: string;
begin
  FileExt := ExtractFileExt(ExcelFileName);
  if FileExt = '' then
    FileExt := GetFileExtension;
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

function TExportExcelToolController.GetDefaultFileExtension: string;
begin
  Result := EXCEL_NEW_FILE_EXT;
end;

function TExportExcelToolController.GetExcelRangeName: string;
var
  LNode: TEFNode;
begin
  LNode := Config.FindNode('ExcelRangeName');
  if Assigned(LNode) then
    Result := LNode.AsString
  else
    Result := 'DataRange';
end;

procedure TExportExcelToolController.GetAdoXFieldType(const AFieldType: TFieldType;
      const AFieldSize: integer; out AdoXDataType: DataTypeEnum);
begin
  case AFieldType of
    ftFixedChar, ftString, ftGuid : AdoXDataType := adVarWChar;
    ftWideString : AdoXDataType := adVarWChar;
    ftMemo : AdoXDataType := adLongVarWChar;
    ftLargeint : AdoXDataType := adBigInt;
    ftAutoInc, ftInteger : AdoXDataType := adInteger;
    ftSmallint, ftWord : AdoXDataType := adSmallInt;
    ftFloat, ftBCD : AdoXDataType := adDouble;
    ftCurrency : AdoXDataType := adCurrency;
    ftFMTBcd : AdoXDataType := adDouble;
    ftBoolean : AdoXDataType := adBoolean;
    ftDate, ftTime, ftDateTime, ftTimeStamp : AdoXDataType := adDate;
  else
    AdoXDataType := adVarWChar;
  end;
  if AFieldSize > MAX_EXCEL_STRING_COLUMN_SIZE then
    AdoXDataType := adLongVarWChar;
end;

procedure TExportExcelToolController.CreateExcelSheet(
  AConnectionString, AExcelRangeName: string);
var
  cat: _Catalog;
  tbl: _Table;
  col: _Column;
  LFieldIndex: integer;
  LViewField: TKViewField;
  LColumnName: string;
  LFieldType: TFieldType;
  ADOXFieldType : DataTypeEnum;
  LViewTable: TKViewTable;
  LFieldSize : integer;
  Accept : boolean;

  procedure AddExcelColumn(const ColName : string;
    FieldType : DataTypeEnum; Size : Integer);
  begin
    col := nil;
    col := CoColumn.Create;
    with col do
      begin
        Set_Name(ColName);
        Set_Type_(FieldType);
        if Size <> 0 then
          Set_DefinedSize(Min(Size,MAX_EXCEL_STRING_COLUMN_SIZE));
      end;
    //add column to table
    tbl.Columns.Append(col, FieldType, Size);
  end;

begin
  LViewTable := ViewTable;
  //WorkBook creation (database)
  cat := CoCatalog.Create;
  cat.Set_ActiveConnection(AConnectionString);
  //WorkSheet creation (table)
  tbl := CoTable.Create;
  tbl.Set_Name(AExcelRangeName);

  //Columns creation (fields)
  for LFieldIndex := 0 to LViewTable.FieldCount - 1 do
  begin
    LViewField := LViewTable.Fields[LFieldIndex];
    LFieldType := LViewField.ActualDataType.AsFieldType;
    if ValidField(LViewField) then
    begin
      LColumnName := NormalizeColumName(LViewField.DisplayLabel);
      if LFieldType <> ftUnknown then
      begin
        LFieldSize := LViewField.Size;
        GetAdoXFieldType(LFieldType, LFieldSize, ADOXFieldType);
        AddExcelColumn(LColumnName, ADOXFieldType, LFieldSize);
      end;
    end;
  end;

  //add table to database
  cat.Tables.Append(tbl);

  col := nil;
  tbl := nil;
  cat := nil;
end;

function TExportExcelToolController.ValidField(AViewField: TKViewField): boolean;
var
  LFieldType: TFieldType;
begin
  Result := False;
  if Assigned(AViewField) and (AViewField.IsVisible) then
  begin
    LFieldType := AViewField.ActualDataType.AsFieldType;
    Result := LFieldType <> ftUnknown;
  end;
end;

procedure TExportExcelToolController.StoreToExcelViaAdo(const AStore: TKViewTableStore);
var
  AcceptRecord, AcceptField: boolean;
  FirstAccepted: boolean;
  LRecordIndex, LFieldIndex: integer;
  LRecord: TKViewTableRecord;
  LValue: Variant;
  LDestField : TField;
  LSourceField: TKViewTableField;
  LConnectionString, LExcelRangeName: string;
  FAdoTable : TAdoTable;

  function FindValidField(const FieldName : string) : TKViewTableField;
  var
    LFieldIndex: integer;
    LViewTableField: TKViewTableField;
  begin
    Result := nil;
    for LFieldIndex := 0 to LRecord.FieldCount - 1 do
    begin
      LViewTableField := LRecord.Fields[LFieldIndex];
      if Assigned(LViewTableField) and
        SameText(NormalizeColumName(LViewTableField.ViewField.AliasedName), NormalizeColumName(FieldName)) then
      begin
        Result := LViewTableField;
        break;
      end;
    end;
  end;
begin
  FTempFileName := EF.SysUtils.GetTempFileName(GetFileExtension);
  DeleteTempFile;
  try
    LConnectionString := GetConnectionString(FTempFileName);
    LExcelRangeName := ExcelRangeName;

    FAdoTable := nil;
    //Se non ho un template scrivo il file Excel creando la struttura attraverso ADOX
    if (TemplateFileName = '') then
      CreateExcelSheet(LConnectionString, LExcelRangeName)
    else
      //Salvo il Template file come file di output
      CopyFile(TemplateFileName, FTempFileName);

    //A questo punto ho il file Excel creato: mi collego con una FAdoTable e ci infilo i dati
    FAdoTable := TAdoTable.Create(nil);
    try
      //Apro il file Excel copiato
      FAdoTable.CursorType := ctStatic;
      FAdoTable.ConnectionString := LConnectionString;
      FAdoTable.TableName := LExcelRangeName;
      FAdoTable.Open;

      FirstAccepted := False;
      //Ciclo sui record dello store per riempire il dataset
      for LRecordIndex := 0 to AStore.RecordCount -1 do
      begin
        //Ciclo sui records: il primo record lo modifico, dal secondo in poi append
        LRecord := AStore.Records[LRecordIndex];
        AcceptRecord := not LRecord.IsDeleted;
        if AcceptRecord then
        begin
          if not FirstAccepted then
            FAdoTable.Edit
          else
            FAdoTable.Append;
          FirstAccepted := True;
          try
            //Aggiorno i campi: comanda sempre il template
            for LFieldIndex := 0 to FAdoTable.FieldCount - 1 do
            begin
              LDestField := FAdoTable.Fields[LFieldIndex];
              LSourceField := FindValidField(LDestField.FieldName);
              AcceptField := ValidField(LSourceField.ViewField);
              if AcceptField then
              begin
                if LSourceField.ViewField.ActualDataType.AsFieldType = ftMemo then
                  LDestField.AsString := StringReplace(LSourceField.AsString, sLineBreak, chr(10), [rfReplaceAll])
                else
                  LSourceField.AssignValueToField(LDestField);
              end
              else
                LDestField.Clear;
            end;
            FAdoTable.Post;
          Except
            FAdoTable.Cancel;
            raise;
          end;
        end;
      end;
    finally
      FAdoTable.Close;
      FAdoTable.Free; //libero il file Excel
    end;
  Except
    DeleteTempFile;
    raise;
  end;
end;

initialization
  TKExtControllerRegistry.Instance.RegisterClass('ExportExcelTool', TExportExcelToolController);

finalization
  TKExtControllerRegistry.Instance.UnregisterClass('ExportExcelTool');

end.
