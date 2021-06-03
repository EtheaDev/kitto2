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

unit Kitto.Ext.FlexCelTools;

interface

uses
  SysUtils
  , Classes
  , ADODB
  , DB
  , Generics.Collections
  , HTTPApp
  , EF.Tree
  , Kitto.FlexCel
  , Kitto.Metadata.DataView
  , Kitto.JS.Controller
  , Kitto.Ext.DataTool
  , Kitto.Ext.Base
  , Kitto.Ext.Tools
  , Kitto.Ext.Files
  ;

type
  TExportFlexCelToolController = class(TKExtDownloadFileController)
  strict private
    FExportExcelEngine: TKFlexCelExportEngine;
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
    property ExportEngine: TKFlexCelExportEngine read FExportExcelEngine;
//  published
    property ExcelRangeName: string read GetExcelRangeName;
    property TemplateFileName: string read GetTemplateFileName;
    property UseDisplayLabels: boolean read GetUseDisplayLabels;
  end;

function DefaultExcelWildcards: string;

implementation

uses
  Math,
  EF.DB, EF.StrUtils, EF.Sys,
  Kitto.Metadata.Models, Kitto.Config;

function DefaultExcelWildcards: string;
begin
  Result := Format('*%s *%s', [EXCEL_NEW_FILE_EXT, EXCEL_FILE_EXT]);
end;

{ TExportFlexCelToolController }

procedure TExportFlexCelToolController.AfterConstruction;
begin
  inherited;
  FExportExcelEngine := TKFlexCelExportEngine.Create;
end;

destructor TExportFlexCelToolController.Destroy;
begin
  FreeAndNil(FExportExcelEngine);
  inherited;
end;

function TExportFlexCelToolController.GetDefaultFileExtension: string;
begin
  Result := EXCEL_NEW_FILE_EXT;
end;

class function TExportFlexCelToolController.GetDefaultImageName: string;
begin
  Result := 'excel_document';
end;

function TExportFlexCelToolController.GetExcelRangeName: string;
begin
  Result := Config.GetString('ExcelRangeName', EXCEL_DEFAULT_RANGE);
end;

function TExportFlexCelToolController.GetTemplateFileName: string;
begin
  Result := Config.GetExpandedString('TemplateFileName');
end;

function TExportFlexCelToolController.GetUseDisplayLabels: Boolean;
begin
  Result := Config.GetBoolean('UseDisplayLabels');
end;

procedure TExportFlexCelToolController.AcceptRecord(ARecord: TKViewTableRecord; var AAccept: boolean);
begin
  if AAccept and Assigned(ServerRecord) then
    AAccept := ARecord = ServerRecord;
end;

procedure TExportFlexCelToolController.AcceptField(AViewField: TKViewField; var AAccept: boolean);
begin
  AAccept := AViewField.IsVisible;
end;

procedure TExportFlexCelToolController.PrepareFile(const AFileName: string);
var
  LStore: TKViewTableStore;
begin
  inherited;
  LStore := ServerStore;
  //if not using a template file we must built the structure of a new excel file using FlexCel
  if (TemplateFileName = '') then
  begin
    FExportExcelEngine.CreateFileByTable(AFileName, LStore, ExcelRangeName,
      AcceptRecord, AcceptField, UseDisplayLabels);
  end
  else
  begin
    FExportExcelEngine.CreateFileByTableWithTemplate(AFileName, TemplateFileName,
      ExcelRangeName, LStore, AcceptRecord, AcceptField, UseDisplayLabels);
  end;
end;

function TExportFlexCelToolController.GetDefaultFileName: string;
var
  LFileExtension: string;
begin
  LFileExtension := ExtractFileExt(ClientFileName);
  if LFileExtension = '' then
    LFileExtension := GetDefaultFileExtension;
  Result := GetTempFileName(LFileExtension);
  AddTempFilename(Result);

end;

initialization
  TJSControllerRegistry.Instance.RegisterClass('ExportFlexCelTool', TExportFlexCelToolController);

finalization
  TJSControllerRegistry.Instance.UnregisterClass('ExportFlexCelTool');

end.
