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
  SysUtils, Classes,
  EF.Tree, Kitto.Excel,
  Kitto.Ext.Controller, Kitto.Ext.DataTool, Kitto.Ext.Base, Kitto.Ext.Tools,
  Kitto.Metadata.DataView, Kitto.Ext.StandardControllers;

type
  TExportExcelToolController = class(TKExtDownloadFileController)
  strict private
    FExportExcelEngine: TKExtExcelEngine;
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
    class function GetDefaultImageName: string;
    property ExportEngine: TKExtExcelEngine read FExportExcelEngine;
  published
    property ExcelRangeName: string read GetExcelRangeName;
    property TemplateFileName: string read GetTemplateFileName;
    property UseDisplayLabels: boolean read GetUseDisplayLabels;
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
  FExportExcelEngine := TKExtExcelEngine.Create(self);
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



initialization
  TKExtControllerRegistry.Instance.RegisterClass('ExportExcelTool', TExportExcelToolController);

finalization
  TKExtControllerRegistry.Instance.UnregisterClass('ExportExcelTool');

end.
