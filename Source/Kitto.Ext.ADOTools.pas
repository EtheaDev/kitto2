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

unit Kitto.Ext.ADOTools;

interface

uses
  SysUtils
  , Classes
  , ADODB
  , DB
  , Generics.Collections
  , HTTPApp
  , EF.Tree
  , Kitto.Excel
  , Kitto.Metadata.DataView
  , Kitto.JS.Controller
  , Kitto.Ext.DataTool
  , Kitto.Ext.Base
  , Kitto.Ext.Tools
  , Kitto.Ext.Files
  ;

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
  //published
    property ExcelRangeName: string read GetExcelRangeName;
    property TemplateFileName: string read GetTemplateFileName;
    property UseDisplayLabels: boolean read GetUseDisplayLabels;
  end;

  TUploadExcelToolController = class(TKExtUploadFileController)
  strict private
    FImportExcelEngine: TKExcelImportEngine;
    FFieldMappings: TStringList;
    function GetExcelRangeName: string;
    function GetFieldMappings: TStringList;
  private
  protected
    function GetAcceptedWildcards: string; override;
    procedure ProcessUploadedFile(const AFile: TAbstractWebRequestFile); override;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
    class function GetDefaultImageName: string; override;
    property ImportEngine: TKExcelImportEngine read FImportExcelEngine;
  //published
    property ExcelRangeName: string read GetExcelRangeName;
    property FieldMappings: TStringList read GetFieldMappings;
  end;

function DefaultExcelWildcards: string;

implementation

uses
  IOUtils
  , Math
  , EF.DB
  , EF.Sys
  , EF.StrUtils
  , Ext.Base
  , Kitto.Metadata.Models
  , Kitto.Config
  ;

function DefaultExcelWildcards: string;
begin
  Result := Format('*%s *%s', [EXCEL_NEW_FILE_EXT, EXCEL_FILE_EXT]);
end;

{ TExportExcelToolController }

procedure TExportExcelToolController.AfterConstruction;
begin
  inherited;
  FExportExcelEngine := TKExcelExportEngine.Create;
end;

destructor TExportExcelToolController.Destroy;
begin
  FreeAndNil(FExportExcelEngine);
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
  Result := Config.GetString('ExcelRangeName', EXCEL_DEFAULT_RANGE);
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
    TFile.Copy(TemplateFileName, AFileName);
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
  Result := GetTempFileName(LFileExtension);
  AddTempFilename(Result);
end;

{ TUploadExcelToolController }

procedure TUploadExcelToolController.AfterConstruction;
begin
  inherited;
  FImportExcelEngine := TKExcelImportEngine.Create;
end;

destructor TUploadExcelToolController.Destroy;
begin
  FreeAndNil(FImportExcelEngine);
  FreeAndNil(FFieldMappings);
  inherited;
end;

class function TUploadExcelToolController.GetDefaultImageName: string;
begin
  Result := 'excel_document';
end;

function TUploadExcelToolController.GetAcceptedWildcards: string;
begin
  Result := inherited GetAcceptedWildcards;
  if Result = '' then
    Result := DefaultExcelWildcards;
end;

function TUploadExcelToolController.GetExcelRangeName: string;
begin
  Result := Config.GetString('ExcelRangeName', EXCEL_DEFAULT_RANGE);
end;

function TUploadExcelToolController.GetFieldMappings: TStringList;
begin
  if not Assigned(FFieldMappings) then
  begin
    FFieldMappings := TStringList.Create;
    Config.GetChildrenAsStrings('FieldMappings', FFieldMappings);
  end;
  Result := FFieldMappings;
end;

procedure TUploadExcelToolController.ProcessUploadedFile(const AFile: TAbstractWebRequestFile);
var
  LFileName: string;
  LFileStream: TFileStream;
begin
  inherited;
  LFileName := GetTempFileName(ExtractFileExt(AFile.FileName));
  try
    LFileStream := TFileStream.Create(LFileName, fmCreate or fmShareExclusive);
    try
      LFileStream.CopyFrom(AFile.Stream, 0);
    finally
      FreeAndNil(LFileStream);
    end;
    FImportExcelEngine.ImportFileIntoViewTable(LFileName, ViewTable, FieldMappings, ExcelRangeName);
  finally
    if FileExists(LFileName) then
      DeleteFile(LFileName);
  end;
end;

initialization
  TJSControllerRegistry.Instance.RegisterClass('ExportExcelTool', TExportExcelToolController);
  TJSControllerRegistry.Instance.RegisterClass('UploadExcelTool', TUploadExcelToolController);
  // Backward compatibility- we don't know how many users of this there are
  TJSControllerRegistry.Instance.RegisterClass('ImportExcelTool', TUploadExcelToolController);

finalization
  TJSControllerRegistry.Instance.UnregisterClass('ExportExcelTool');
  TJSControllerRegistry.Instance.UnregisterClass('ImportExcelTool');
  TJSControllerRegistry.Instance.UnregisterClass('UploadExcelTool');

end.
