{-------------------------------------------------------------------------------
   Copyright 2012-2018 Ethea S.r.l.

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
unit Kitto.ReportBuilder;

interface

{$I Kitto.Defines.inc}

uses
  SysUtils, Classes,
  ppImageDevice, //Enable images
  ppReport, Data.DB,
  SqlExpr, daDBExpress, EF.DB.DBX, //Enable DBExpress for ReportBuilder and Kitto
  ADODB, daADO, EF.DB.ADO, //Enable ADO for ReportBuilder and Kitto
{$IFDEF D20+}
  daFireDac,
  EF.DB.FD, FireDAC.Stan.Intf, FireDAC.Comp.Client, //FireDac support
  FireDAC.Phys.MSSQL, FireDAC.Phys.MSSQLMeta, //FireDac support for MS-SQL
  FireDAC.Phys.IBBase, FireDAC.Phys.FB, //FireDac support for Firebird
{$ENDIF}
  Kitto.JS.Controller, Kitto.Ext.DataTool, Kitto.Ext.Base, Kitto.Ext.Tools,
  Kitto.Store, Kitto.Metadata.DataView, Kitto.Ext.StandardControllers;

type

  TKReportBuilderEngine = class(TComponent)
  strict private
    FServerRecord: TKViewTableRecord;
    FServerStore: TKViewTableStore;
    procedure CreatePipelines(APipeLineList: TStringList);
    procedure OnGetAutoSearchValues(Sender: TObject);
    function GetSQLConnection(const ADatabaseName: string): TCustomConnection;
    {$IFDEF RB_DESIGNER}
    procedure ShowDesigner(AReport: TppReport;
      ASQLConnection: TCustomConnection);
    {$ENDIF}
  public
    procedure BuildReport(const ATemplateFileName, APDFFileName: string;
      const AServerStore: TKViewTableStore; const AServerRecord: TKViewTableRecord;
      const AUsePipeLine: Boolean; const ADatabaseName: string = '';
      const ADesign: Boolean = False);
  end;

implementation

uses
{$IFDEF RB_DESIGNER}
  //Enable Report Designer
  ppEndUsr, ppDsgnDB, ppDBJIT, ppModule, daDataModule, ppParameter, ppDesignLayer,
  ppFormWrapper, ppRptExp, ppCache, ppRelatv, ppProd, daDataManager, ppCTDsgn,
  ppTmplat, ppForms, ppRptExpCommon, ppExpDlg, ppDesignLayoutMenu, ppRptWiz,
  daDataView, daDataWizard, daDataWizardManager,
  Forms, ZLib,
{$ENDIF}
  Math,
  daIDE, //Enable DADE
  raIDE, //Enable RAP
  ppChrtUI, //Enable TeeChart
  ppCtrls, ppPDFDevice, ppASField,
  ppClass, ppPrintr, ppBands, ppTypes, ppSubRpt, ppComm, myChkBox, ppDevice, ppFilDev,
  EF.DB, EF.Sys, EF.Tree, EF.Localization,
  Kitto.RB.RapFunc,
  Kitto.Metadata.Models, Kitto.Config,
  Kitto.RB.ppStorePipe;

{ TKReportBuilderEngine }

procedure TKReportBuilderEngine.OnGetAutoSearchValues(Sender: TObject);
var
  NotAssigned : boolean;
  i : integer;
  AutoSearchField : TppAutoSearchField;
  LReport: TppReport;

  function GetStandardSearchValue(AutoSearchField: TppAutoSearchField): boolean;
  var
    LField: TKViewTableField;
  begin
    Result := False;
    //LRecord contains current record
    if FServerRecord = nil then
      Exit;
    //Matching TableName and FieldName of the AutoSearch field
    if not SameText(FServerRecord.ViewTable.Model.DBTableName, AutoSearchField.TableName) then
      Exit;

    LField := FServerRecord.FindField(AutoSearchField.FieldName);
    if Assigned(LField) then
    begin
      //Assiging search value
      AutoSearchField.SearchExpression := LField.AsString;
      Result := True;
      Exit;
    end;
  end;

begin
  LReport := Sender as TppReport;
  NotAssigned := False;
  for i := 0 to LReport.AutoSearchFieldCount -1 do
  begin
    AutoSearchField := LReport.AutoSearchFields[i];
    //Empty search value because at design-time can be filled with a sample value
    AutoSearchField.SearchExpression := '';
    if (not GetStandardSearchValue(AutoSearchField) and AutoSearchField.Mandatory) then
      Assert(not NotAssigned, Format('Report parameter %s is not assigned',[AutoSearchField.FieldName]));
  end;
end;

procedure TKReportBuilderEngine.CreatePipelines(APipeLineList: TStringList);
var
  LPipeLineName : string;
  LPipeLine : TppKStorePipeline;
  LDetailTableStore: TKViewTableStore;
  I: Integer;
begin
  APipeLineList.Clear;

  LPipeLineName := FServerStore.ViewTable.ModelName;
  LPipeLine := TppKStorePipeline.CreatePipeLine(self, FServerStore, FServerRecord, LPipeLineName);
  APipeLineList.AddObject(LPipeLineName, LPipeLine);
  if Assigned(FServerRecord) then
  begin
    FServerRecord.LoadDetailStores;
    for I := 0 to FServerRecord.DetailStoreCount -1 do
    begin
      LDetailTableStore := FServerRecord.DetailStores[I];
      LPipeLineName := LDetailTableStore.ViewTable.ModelName;
      LPipeLine := TppKStorePipeline.CreatePipeLine(self, LDetailTableStore, nil, LPipeLineName);
      APipeLineList.AddObject(LPipeLineName, LPipeLine);
    end;
  end;
end;

{$IFDEF RB_DESIGNER}
procedure TKReportBuilderEngine.ShowDesigner(AReport: TppReport;
  ASQLConnection: TCustomConnection);

begin
  TThread.Synchronize(nil,
    procedure
    var
      ppDesigner : TppDesigner;
      LDriverName: string;
    begin
      ppDesigner := TppDesigner.Create(nil);
      try
        if ASQLConnection is TSQLConnection then
        begin
          ppDesigner.DataSettings.SessionType := 'dbExpressSession';
          LDriverName := TSQLConnection(ASQLConnection).DriverName;
          ppDesigner.DataSettings.DatabaseName := ASQLConnection.Name;
          if pos('ORACLE', UpperCase(LDriverName)) > 0 then
            ppDesigner.DataSettings.DatabaseType := dtOracle
          else if pos('SQLSERVER', UpperCase(LDriverName)) > 0 then
            ppDesigner.DataSettings.DatabaseType := dtMSSQLServer
          else if pos('INTERBASE', UpperCase(LDriverName)) > 0 then
            ppDesigner.DataSettings.DatabaseType := dtInterBase
          else if pos('FIREBIRD', UpperCase(LDriverName)) > 0 then
            ppDesigner.DataSettings.DatabaseType := dtFirebird
          else
            ppDesigner.DataSettings.DatabaseType := dtOther;
        end
        {$IFDEF D20+}
        else if ASQLConnection is TFDConnection then
        begin
          ppDesigner.DataSettings.SessionType := 'FireDACSession';
          ppDesigner.DataSettings.DatabaseName := ASQLConnection.Name;
          case TFDConnection(ASQLConnection).RDBMSKind of
            TFDRDBMSKinds.Oracle:     ppDesigner.DataSettings.DatabaseType := dtOracle;
            TFDRDBMSKinds.MSSQL:      ppDesigner.DataSettings.DatabaseType := dtMSSQLServer;
            TFDRDBMSKinds.MSAccess:   ppDesigner.DataSettings.DatabaseType := dtMSAccess;
            TFDRDBMSKinds.MySQL:      ppDesigner.DataSettings.DatabaseType := dtMySQL;
            TFDRDBMSKinds.Interbase:  ppDesigner.DataSettings.DatabaseType := dtInterBase;
            TFDRDBMSKinds.PostgreSQL: ppDesigner.DataSettings.DatabaseType := dtPostgreSQL;
          else
            ppDesigner.DataSettings.DatabaseType := dtOther;
          end;
        end
        {$ENDIF}
        else if ASQLConnection is TADOConnection then
        begin
          ppDesigner.DataSettings.SessionType := 'ADOSession';
          ppDesigner.DataSettings.DatabaseName := ASQLConnection.Name;
          ppDesigner.DataSettings.DatabaseType := dtMSSQLServer;
        end;
        ppDesigner.Report := AReport;
        Screen.ActiveForm.SetFocus;
        ppDesigner.ShowModal;
      finally
        ppDesigner.Free;
      end;
    end);
end;
{$ENDIF}

function TKReportBuilderEngine.GetSQLConnection(const ADatabaseName: string): TCustomConnection;
var
  LDBConnection: TEFDBConnection;
  LInternalConnection: TObject;
begin
  LDBConnection := TKConfig.Instance.CreateDBConnection(ADatabaseName);
  try
    if not LDBConnection.IsOpen then
      LDBConnection.Open;
    LInternalConnection := LDBConnection.GetConnection;
    if LInternalConnection is TSQLConnection then
      Result := TSQLConnection(LInternalConnection)
    else if LInternalConnection is TADOConnection then
      Result := TADOConnection(LInternalConnection)
    else
      Result := nil;
  finally
    FreeAndNil(LDBConnection);
  end;
end;

procedure TKReportBuilderEngine.BuildReport(const ATemplateFileName, APDFFileName: string;
  const AServerStore: TKViewTableStore; const AServerRecord: TKViewTableRecord;
  const AUsePipeLine: Boolean; const ADatabaseName: string = '';
  const ADesign: Boolean = False);
var
  LReport: TppReport;
  LPipeLineList: TStringList;
  LSQLConnection: TCustomConnection;
  LDatabaseName: string;
begin
  Assert(ATemplateFileName <> '','ReportBuilder TemplateFileName is mandatory');
  Assert(Assigned(AServerStore), 'ServerStore is mandatory for ReportBuilder Engine');
  FServerRecord := AServerRecord;
  FServerStore := AServerStore;
  if ADatabaseName <> '' then
    LDatabaseName := ADatabaseName
  else
    LDatabaseName := FServerStore.ViewTable.DatabaseName;
  LSQLConnection := GetSQLConnection(LDatabaseName);

  //Setting folders for image management
  UpdateRAPEnvironment(TKConfig.AppHomePath+'Resources',
    TKConfig.SystemHomePath+'Resources');

  LReport := nil;
  LPipeLineList := nil;
  try
    if AUsePipeline then
    begin
      LPipeLineList := TStringList.Create(True);
      CreatePipelines(LPipeLineList);
    end;

    //Settings reports
    LReport := TppReport.Create(self);
    LReport.Name := 'ppReport';
    LReport.AutoStop := True;
    LReport.ShowPrintDialog := False;
    LReport.Template.Format := ftASCII;
    LReport.Template.FileName := ATemplateFileName;
    if not FileExists(ATemplateFileName) and ADesign then
      LReport.Template.SaveToFile;
    LReport.Template.LoadFromFile;

    //Generating report via device to create a PDF file
    LReport.AllowPrintToFile := True;
    LReport.ShowPrintDialog := False;
    LReport.DeviceType := 'PDF';
    LReport.TextFileName := APDFFileName;

    //Workaround: name and add report so ReportBuilder can find and share the connection
    lSQLConnection.Name := LDatabaseName;
    InsertComponent(LSQLConnection);
    try
      if LReport.AutoSearchFieldCount > 0 then
      begin
        LReport.OnGetAutoSearchValues := OnGetAutoSearchValues;
        LReport.ShowAutoSearchDialog := True;
      end;
      //Build a blank report if not data was found
      LReport.NoDataBehaviors := [ndBlankReport];
      {$IFDEF RB_DESIGNER}
      if ADesign then
        ShowDesigner(LReport, LSQLConnection)
      else
      {$ENDIF}
        OnGetAutoSearchValues(LReport);
        //LReport.PrintToDevices;
        LReport.Print;
    finally
      //Reset workaround
      LSQLConnection.Name := '';
      RemoveComponent(LSQLConnection);
    end;
  finally
    LReport.Free;
    if AUsePipeline then
      LPipeLineList.Free;
  end;
end;

end.
