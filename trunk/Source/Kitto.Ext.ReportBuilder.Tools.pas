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

unit Kitto.Ext.ReportBuilder.Tools;

interface

uses
  SysUtils, Classes,
  ppReport, Data.DB,
  SqlExpr, daDBExpress, EF.DB.DBX, //Enable DBExpress for ReportBuilder and Kitto
  ADODB, daADO, EF.DB.ADO, //Enable ADO for ReportBuilder and Kitto
  Kitto.Ext.Controller, Kitto.Ext.DataTool, Kitto.Ext.Base, Kitto.Ext.Tools,
  Kitto.Metadata.DataView, Kitto.Ext.StandardControllers;

type

  TReportBuilderToolController = class(TKExtDownloadFileController)
  strict private
    procedure CreatePipelines(APipeLineList: TStringList);
    procedure OnGetAutoSearchValues(Sender: TObject);
    function GetSQLConnection: TCustomConnection;
  protected
    function GetDefaultFileName: string; override;
    function GetDefaultFileExtension: string; override;
    procedure PrepareFile(const AFileName: string); override;
    property ViewTable;
  public
    class function GetDefaultImageName: string;
  published
    property SQLConnection: TCustomConnection read GetSQLConnection;
  end;

  TEFppReport = class(TppReport)
  private
    GettingSearchValues : boolean;
  public
    procedure PrintToDevices; override;
    procedure DoOnGetAutoSearchValues; override;
    function DisplayAutoSearchDialog: Boolean; override;
  end;

implementation

uses
  Math,
  daIDE, //Enable DADE
  raIDE, //Enable RAP
  ppChrtUI, //Enable TeeChart
  ppCtrls, ppDBPipe, ppPDFDevice, ppASField,
  ppClass, ppPrintr, ppBands, ppTypes, ppEndUsr, ppSubRpt, ppComm, myChkBox, ppDevice, ppFilDev,
  Ext, EF.DB, EF.SysUtils, EF.Tree,
  Kitto.Metadata.Models, Kitto.Ext.Session, Kitto.Config;

{ TReportBuilderToolController }

procedure TReportBuilderToolController.OnGetAutoSearchValues(Sender: TObject);
var
  NotAssigned : boolean;
  i : integer;
  AutoSearchField : TppAutoSearchField;
  LReport: TEfppReport;

  function GetStandardSearchValue(AutoSearchField: TppAutoSearchField): boolean;
  var
    LRecord: TKViewTableRecord;
    LField: TKViewTableField;
  begin
    Result := False;
    LRecord := ServerRecord;
    //in LRecord c'è il record master
    if LRecord = nil then
      Exit;
    //Di default, il valore da cercare viene preso dal record che abbia TableName e FieldName
    //corrispondenti all'AutoSearchField
    if not SameText(LRecord.ViewTable.Model.DBTableName, AutoSearchField.TableName) then
      Exit;

    LField := LRecord.FindField(AutoSearchField.FieldName);
    if Assigned(LField) then
    begin
      //Assegno il valore da ricercare
      AutoSearchField.SearchExpression := LField.AsString;
      Result := True;
      Exit;
    end;
  end;

begin
  LReport := Sender as TEFppReport;
  NotAssigned := False;
  for i := 0 to LReport.AutoSearchFieldCount -1 do
  begin
    AutoSearchField := LReport.AutoSearchFields[i];
    //Svuoto il parametro perche'' a design potrebbe essere stato indicato
    AutoSearchField.SearchExpression := '';
    //Se il campo e'' obbligatorio ma non viene valorizzato me lo segno in NotAssigned
    if (not GetStandardSearchValue(AutoSearchField) and AutoSearchField.Mandatory) then
      Assert(not NotAssigned, Format('Report parameter %s is not assigned',[AutoSearchField.FieldName]));
  end;
end;

procedure TReportBuilderToolController.CreatePipelines(APipeLineList: TStringList);

  procedure AddDataPipeLine(
    DataSet : TDataSet;
    const MasterField : string;
    SingleRecord : boolean);
  var
    i : integer;
    Field : TField;
    Include : boolean;
    DataSource : TDataSource;
    PipeLineName : string;
    PipeRequired : boolean;
    ppDBPipeLine : TppDBPipeLine;
  begin
    PipeRequired := false;
    //Ciclo sui campi per vedere se almeno uno andrà stampato: serve il pipeline
    if not DataSet.Active then
      DataSet.Open;
    for i := 0 to DataSet.FieldCount -1 do
    begin
      Field := DataSet.Fields[i];
      if not (Field is TDataSetField) then
      begin
        Include := True;
        //if Assigned(OnIncludeField) then
        //  OnIncludeField(DataSet,MasterField,Field,Include);
        if Include then
        begin
          PipeRequired := true;
          Break;
        end;
      end;
    end;

    //Creo il pipeline con il datasource agganciato al dataset
    if PipeRequired then
    begin
      DataSource := TDataSource.Create(nil);
(*
      FDataSourceList.Add(DataSource);
      DataSource.DataSet := DataSet;
      PipeLineName := StrSubstChar('.','_',MasterField+PIPE_SUFFIX);
      ppDBPipeLine := CreateDataPipeLine(self,PipeLineName, DataSource, SingleRecord);
      ppDBPipeLine.OpenDataSource := True;
      APipeLineList.AddObject(PipeLineName, ppDBPipeLine);
*)
    end;

    //ciclo sui campi del nesteddataset o detaildataset
    if not DataSet.Active then
      DataSet.Open;
    for i := 0 to DataSet.FieldCount -1 do
    begin
      Field := DataSet.Fields[i];
      if (Field is TDataSetField) then
      begin
        Include := True;
        //if Assigned(OnIncludeField) then
        //  OnIncludeField(DataSet,MasterField,Field,Include);
        if Include then
        begin
          AddDataPipeLine(TDataSetField(Field).NestedDataSet,
            MasterField+'.'+Field.FieldName, False);
        end;
      end;
    end;
  end;
var
  j : integer;
begin
  APipeLineList.Clear;
  //Se il documento non è di un solo record (master) passo False a SingleRecord
  //in modo che il pipeline non dovrà ciclare sui records
(*
  if SingleMasterRecord then
    AddDataPipeLine(MasterDataSet, MasterAliasName, True)
  else
    AddDataPipeLine(MasterDataSet, MasterAliasName, False);

  //creo i pipelines aggiuntivi per ogni detail dataset
  for j := 0 to DetailDataSets.Count -1 do
  begin
    AddDataPipeLine(DetailDataSets.Items[j].DetailDataSet,
      DetailDataSets.Items[j].DetailAliasName,
      False);
  end;
*)
end;

function TReportBuilderToolController.GetDefaultFileExtension: string;
begin
  Result := '.pdf';
end;

class function TReportBuilderToolController.GetDefaultImageName: string;
begin
  Result := 'pdf_document';
end;

function TReportBuilderToolController.GetSQLConnection: TCustomConnection;
var
  LDBConnection: TEFDBConnection;
  LInternalConnection: TObject;
begin
  LDBConnection := TKConfig.Instance.DBConnections[ViewTable.DatabaseName];
  if Assigned(LDBConnection) then
    LInternalConnection := LDBConnection.GetConnection;
  if LInternalConnection is TSQLConnection then
    Result := TSQLConnection(LInternalConnection)
  else if LInternalConnection is TADOConnection then
    Result := TADOConnection(LInternalConnection)
  else
    Result := nil;
end;

function TReportBuilderToolController.GetDefaultFileName: string;
var
  LFileExtension: string;
begin
  LFileExtension := ExtractFileExt(ClientFileName);
  if LFileExtension = '' then
    LFileExtension := GetDefaultFileExtension;
  Result := EF.SysUtils.GetTempFileName(LFileExtension);
  AddTempFilename(Result);
end;

procedure TReportBuilderToolController.PrepareFile(const AFileName: string);
var
  LStore: TKViewTableStore;
  LReport: TEFppReport;
  AcceptRecord, AcceptField: boolean;
  FirstAccepted: boolean;
  LRecordIndex, LFieldIndex: integer;
  LRecord: TKViewTableRecord;
  LDestField : TField;
  LSourceField: TKViewTableField;
  LPDFDevice: TppFileDevice;
  LPipeLineList: TStringList;

  function FindValidField(const FieldName : string) : TKViewTableField;
  var
    LFieldIndex: integer;
    LViewTableField: TKViewTableField;
    LModelField: TKModelField;
  begin
    Result := nil;
    for LFieldIndex := 0 to LRecord.FieldCount - 1 do
    begin
      LViewTableField := LRecord.Fields[LFieldIndex];
      if Assigned(LViewTableField) and Assigned(LViewTableField.ViewField) and
        SameText(NormalizeColumName(LViewTableField.ViewField.FieldName), NormalizeColumName(FieldName)) then
      begin
        Result := LViewTableField;
        break;
      end;
    end;
  end;
begin
  LRecord := ServerRecord;

  LReport := nil;
  LPDFDevice := nil;
  LPipeLineList := nil;
  try
    LPipeLineList := TStringList.Create(True);

    //Se non ho un template non posso procedere
    Assert(TemplateFileName <> '','ReportBuilder TemplateFileName is mandatory');

    //Apro il template del report
    LReport := TEFppReport.Create(self);
    LReport.AutoStop := True;
    LReport.ShowPrintDialog := False;
    LReport.Template.Format := ftASCII;
    LReport.Template.FileName := TemplateFileName;
    LReport.Template.LoadFromFile;

    //Imposto la connessione
    TdaChildSQLClientDataSet.Create(nil);

    //Genero il report su file temporaneo in PDF
    LPDFDevice := TppPDFDevice.Create(nil);
    LPDFDevice.FileName := AFileName;
    LPDFDevice.Publisher := LReport.Publisher;
    //Nomino e aggiungo il componente della connessione in modo che RB lo trovi
    SQLConnection.Name := ViewTable.DatabaseName;
    InsertComponent(SQLConnection);
    Try
      if LReport.AutoSearchFieldCount > 0 then
      begin
        LReport.OnGetAutoSearchValues := OnGetAutoSearchValues;
        LReport.ShowAutoSearchDialog := True;
      end;
      LReport.PrintToDevices;
    Finally
      //Resetto e rimuovo il componente della connessione
      SQLConnection.Name := '';
      RemoveComponent(SQLConnection);
    End;
  finally
    LPDFDevice.Free;
    LReport.Free;
    LPipeLineList.Free;
  end;
end;

{ TEFppReport }

function TEFppReport.DisplayAutoSearchDialog: Boolean;
begin
  //Non devo fare inherited per non mostrare la form di ricerca dei parametri
  if Assigned(OnGetAutoSearchValues) then
    Result := True
  else
    Result := inherited DisplayAutoSearchDialog;
end;

procedure TEFppReport.DoOnGetAutoSearchValues;
begin
  inherited;
  if not GettingSearchValues then
  begin
    GettingSearchValues := True;
    Try
      inherited DoOnGetAutoSearchValues;
    Finally
      GettingSearchValues := False;
    End;
  end;
end;

procedure TEFppReport.PrintToDevices;
begin
  if Assigned(OnGetAutoSearchValues) then
    DoOnGetAutoSearchValues;
  inherited;
end;

initialization
  TKExtControllerRegistry.Instance.RegisterClass('ReportBuilderTool', TReportBuilderToolController);

finalization
  TKExtControllerRegistry.Instance.UnregisterClass('ReportBuilderTool');

end.
