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

unit Kitto.Ext.ADOTools;

interface

uses
  SysUtils, Classes, ADODB, DB, Generics.Collections,
  EF.Tree, Kitto.Excel,
  Kitto.Ext.Controller, Kitto.Ext.DataTool, Kitto.Ext.Base, Kitto.Ext.Tools,
  Kitto.Metadata.DataView, Kitto.Ext.StandardControllers;

type
  TExportExcelToolController = class(TKExtDownloadFileController)
  strict private
    FExportExcelEngine: TKExcelExportEngine;
    function GetExcelRangeName: string;
    function GetTemplateFileName: string;
    function GetUseDisplayLabels: boolean;
  strict protected
    function GetDefaultFileName: string; override;
    function GetDefaultFileExtension: string; override;
    procedure PrepareFile(const AFileName: string); override;
    procedure AcceptRecord(ARecord: TKViewTableRecord; var AAccept: boolean); virtual;
    procedure AcceptField(AViewField: TKViewField; var AAccept: boolean); virtual;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
    class function GetDefaultImageName: string; override;
    property ExportEngine: TKExcelExportEngine read FExportExcelEngine;
  published
    property ExcelRangeName: string read GetExcelRangeName;
    property TemplateFileName: string read GetTemplateFileName;
    property UseDisplayLabels: boolean read GetUseDisplayLabels;
  end;

  TImportExcelToolController = class(TKExtUploadFileController)
  strict private
    FImportExcelEngine: TKExcelImportEngine;
    FFieldMapping: TStringList;
    FAddedRecord: TKViewTableRecord;
    FAddedRecords: TObjectList<TKViewTableRecord>;
    function GetExcelRangeName: string;
    function GetUseDisplayLabels: boolean;
    function GetFieldMappings: TStringList;
  private
    procedure RollbackAddedRecords;
  protected
    function GetWildCard: string; override;
    procedure ProcessUploadedFile(const AFileName: string); override;
    procedure AcceptRecord(const ARecord: TDataSet; var AAccept: boolean); virtual;
    procedure AcceptField(AField: TField; var AAccept: boolean); virtual;
    procedure SetFieldValue(const ADestFieldName: string; const AValue: Variant); virtual;
    procedure PostRecord(const AAdoTable: TAdoTable; const PostRecord: boolean); virtual;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
    class function GetDefaultImageName: string; override;
    property ImportEngine: TKExcelImportEngine read FImportExcelEngine;
  published
    property ExcelRangeName: string read GetExcelRangeName;
    property UseDisplayLabels: boolean read GetUseDisplayLabels;
    property FieldMappings: TStringList read GetFieldMappings;
  end;

implementation

uses
  Math,
  Ext, EF.DB, EF.SysUtils, EF.StrUtils,
  Kitto.Metadata.Models, Kitto.Ext.Session, Kitto.Config;

{ TExportExcelToolController }

procedure TExportExcelToolController.AfterConstruction;
begin
  inherited;
  FExportExcelEngine := TKExcelExportEngine.Create(self);
end;

destructor TExportExcelToolController.Destroy;
begin
  FExportExcelEngine.Free;
  inherited;
end;

function TExportExcelToolController.GetDefaultFileExtension: string;
begin
  Result := EXCEL_NEW_FILE_EXT;
end;

class function TExportExcelToolController.GetDefaultImageName: string;
begin
  Result := 'excel_document';
end;

function TExportExcelToolController.GetExcelRangeName: string;
begin
  Result := Config.GetString('ExcelRangeName', 'DataRange');
end;

function TExportExcelToolController.GetTemplateFileName: string;
begin
  Result := Config.GetExpandedString('TemplateFileName');
end;

function TExportExcelToolController.GetUseDisplayLabels: Boolean;
begin
  Result := Config.GetBoolean('UseDisplayLabels');
end;

procedure TExportExcelToolController.AcceptRecord(ARecord: TKViewTableRecord; var AAccept: boolean);
begin
  if AAccept and Assigned(ServerRecord) then
    AAccept := ARecord = ServerRecord;
end;

procedure TExportExcelToolController.AcceptField(AViewField: TKViewField; var AAccept: boolean);
begin
  AAccept := AViewField.IsVisible;
end;

procedure TExportExcelToolController.PrepareFile(const AFileName: string);
var
  LStore: TKViewTableStore;
begin
  inherited;
  LStore := ServerStore;
  //if not using a template file we must built the structure of a new excel file using ADOX
  if (TemplateFileName = '') then
    FExportExcelEngine.CreateFileByTable(AFileName, LStore, ExcelRangeName,
      AcceptField, UseDisplayLabels)
  else
  begin
    Assert(FileExists(TemplateFileName),
      Format('Excel template file "%s" not found!',[TemplateFileName]));
    //Save the template file as the output file
    CopyFile(TemplateFileName, AFileName);
  end;

  //Now the output file is ready: filling data
  FExportExcelEngine.FillAdoTable(AFileName, ExcelRangeName, LStore,
    AcceptRecord, AcceptField, UseDisplayLabels);
end;

function TExportExcelToolController.GetDefaultFileName: string;
var
  LFileExtension: string;
begin
  LFileExtension := ExtractFileExt(ClientFileName);
  if LFileExtension = '' then
    LFileExtension := GetDefaultFileExtension;
  Result := EF.SysUtils.GetTempFileName(LFileExtension);
  AddTempFilename(Result);
end;

{ TImportExcelToolController }

procedure TImportExcelToolController.AfterConstruction;
begin
  inherited;
  FImportExcelEngine := TKExcelImportEngine.Create(self);
  FAddedRecords := TObjectList<TKViewTableRecord>.Create(False);
end;

destructor TImportExcelToolController.Destroy;
begin
  FreeAndNil(FAddedRecords);
  FreeAndNil(FImportExcelEngine);
  FreeAndNil(FFieldMapping);
  inherited;
end;

class function TImportExcelToolController.GetDefaultImageName: string;
begin
  Result := 'excel_document';
end;

function TImportExcelToolController.GetWildCard: string;
begin
  Result := Format('*%s;*%s', [EXCEL_NEW_FILE_EXT, EXCEL_FILE_EXT]);
end;

function TImportExcelToolController.GetExcelRangeName: string;
begin
  Result := Config.GetString('ExcelRangeName', 'DataRange');
end;

function TImportExcelToolController.GetUseDisplayLabels: Boolean;
begin
  Result := Config.GetBoolean('UseDisplayLabels');
end;

procedure TImportExcelToolController.AcceptRecord(
  const ARecord: TDataSet; var AAccept: boolean);
var
  LDefaultValues: TEFNode;
begin
  //If the first field of the Excel Table is empty the record is not accepted
  AAccept := not ARecord.Fields[0].IsNull;
  if AAccept then
  begin
    FAddedRecord := ServerStore.Records.AppendAndInitialize;
    FAddedRecords.Add(FAddedRecord);
    {TODO: copied from TKExtFormPanelController.StartOperation: need refactoring }
    LDefaultValues := nil;
    try
      LDefaultValues := ViewTable.GetDefaultValues;
      FAddedRecord.Store.DisableChangeNotifications;
      try
        FAddedRecord.ReadFromNode(LDefaultValues);
      finally
        FAddedRecord.Store.EnableChangeNotifications;
      end;
      ViewTable.Model.BeforeNewRecord(FAddedRecord, False);
      FAddedRecord.ApplyNewRecordRules;
      ViewTable.Model.AfterNewRecord(FAddedRecord);
    finally
      FreeAndNil(LDefaultValues);
    end;
  end;
end;

procedure TImportExcelToolController.AcceptField(
  AField: TField; var AAccept: boolean);
var
  LFieldNameMap: string;
begin
  LFieldNameMap := FImportExcelEngine.GetFieldMapping(AField);
  AAccept := FAddedRecord.FindField(LFieldNameMap) <> nil;
end;

function TImportExcelToolController.GetFieldMappings: TStringList;
var
  FieldMappingsNode: TEFNode;
  FieldMappingNode: TEFNode;
  I: Integer;
begin
  if not Assigned(FFieldMapping) then
  begin
    FFieldMapping := TStringList.Create;
    try
      FieldMappingsNode := Config.FindNode('FieldMappings');
      if Assigned(FieldMappingsNode) then
      begin
        for I := 0 to FieldMappingsNode.ChildCount -1 do
        begin
          FieldMappingNode := FieldMappingsNode.Children[I];
          FFieldMapping.Add(FieldMappingNode.Name+'='+FieldMappingNode.AsString);
        end;
      end;
    except
      FFieldMapping.Free;
      raise;
    end;
  end;
  Result := FFieldMapping;
end;

procedure TImportExcelToolController.PostRecord(
  const AAdoTable: TAdoTable; const PostRecord: boolean);
begin
  FAddedRecord.ApplyNewRecordRules;
end;

procedure TImportExcelToolController.ProcessUploadedFile(const AFileName: string);
var
  LFileName: string;
begin
  inherited;
  LFileName := AFileName;
  //LFileName := 'C:\Users\Public\Documents\UploadTest.xls';
  FImportExcelEngine.FieldMappings := FieldMappings;
  try
    FAddedRecord := nil;
    FAddedRecords.Clear;
    FImportExcelEngine.ImportFile(LFileName, SetFieldValue, PostRecord, ExcelRangeName,
      AcceptRecord, AcceptField, UseDisplayLabels);
    ViewTable.Model.SaveRecords(ServerStore, True, nil);
  except
    RollbackAddedRecords;
    raise;
  end;
end;

procedure TImportExcelToolController.RollbackAddedRecords;
var
  I: Integer;
  LAddedRecord: TKViewTableRecord;
begin
  for I := FAddedRecords.Count -1 downto 0 do
  begin
    LAddedRecord := FAddedRecords.Items[I];
    ServerStore.Records.Remove(LAddedRecord);
  end;
  FAddedRecords.Clear;
end;

procedure TImportExcelToolController.SetFieldValue(const ADestFieldName: string;
  const AValue: Variant);
begin
  FAddedRecord.FieldByName(ADestFieldName).Value := AValue;
end;

initialization
  TKExtControllerRegistry.Instance.RegisterClass('ExportExcelTool', TExportExcelToolController);
  TKExtControllerRegistry.Instance.RegisterClass('ImportExcelTool', TImportExcelToolController);

finalization
  TKExtControllerRegistry.Instance.UnregisterClass('ExportExcelTool');
  TKExtControllerRegistry.Instance.UnregisterClass('ImportExcelTool');

end.
