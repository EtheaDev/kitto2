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
unit Kitto.Ext.ReportBuilderTools;

interface

uses
  SysUtils, Classes,
  Kitto.ReportBuilder,
  Kitto.JS.Controller, Kitto.Ext.DataTool, Kitto.Ext.Base, Kitto.Ext.Tools,
  Kitto.Store, Kitto.Metadata.DataView, Kitto.Ext.StandardControllers;

type

  TReportBuilderToolController = class(TKExtDownloadFileController)
  strict private
    FReportBuilderEngine: TKReportBuilderEngine;
    function GetDesign: boolean;
    function GetUsePipeLines: boolean;
    function GetDatabaseName: string;
  strict protected
    function GetTemplateFileName: string; virtual;  
    function GetDefaultFileName: string; override;
    function GetDefaultFileExtension: string; override;
    procedure PrepareFile(const AFileName: string); override;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
    class function GetDefaultImageName: string; override;
    property ReportBuilderEngine: TKReportBuilderEngine read FReportBuilderEngine;
    property TemplateFileName: string read GetTemplateFileName;
    property Design: boolean read GetDesign;
    property UsePipeLines: Boolean read GetUsePipeLines;
    property DatabaseName: string read GetDatabaseName;
  end;

implementation

uses
  EF.DB, EF.Sys, EF.Tree, EF.Localization,
  Kitto.Metadata.Models, Kitto.Config;

{ TReportBuilderToolController }

procedure TReportBuilderToolController.AfterConstruction;
begin
  inherited;
  FReportBuilderEngine := TKReportBuilderEngine.Create(nil);
end;

function TReportBuilderToolController.GetDefaultFileExtension: string;
begin
  Result := '.pdf';
end;

class function TReportBuilderToolController.GetDefaultImageName: string;
begin
  Result := 'pdf_document';
end;

function TReportBuilderToolController.GetDesign: boolean;
begin
  Result := Config.GetBoolean('Design', False);
end;

function TReportBuilderToolController.GetTemplateFileName: string;
begin
  Result := Config.GetExpandedString('TemplateFileName');
end;

destructor TReportBuilderToolController.Destroy;
begin
  FReportBuilderEngine.Free;
  inherited;
end;

function TReportBuilderToolController.GetDatabaseName: string;
begin
  Result := Config.GetString('DatabaseName');
end;

function TReportBuilderToolController.GetUsePipeLines: boolean;
begin
  Result := Config.GetBoolean('UsePipeLines', False);
end;

function TReportBuilderToolController.GetDefaultFileName: string;
var
  LFileExtension: string;
begin
  LFileExtension := ExtractFileExt(ClientFileName);
  if LFileExtension = '' then
    LFileExtension := GetDefaultFileExtension;
  Result := GetTempFileName(LFileExtension);
  AddTempFilename(Result);
end;

procedure TReportBuilderToolController.PrepareFile(const AFileName: string);
begin
  ReportBuilderEngine.BuildReport(TemplateFileName,
    AFileName, ServerStore, ServerRecord, UsePipeLines, DatabaseName, Design);
end;

initialization
  TJSControllerRegistry.Instance.RegisterClass('ReportBuilderTool', TReportBuilderToolController);

finalization
  TJSControllerRegistry.Instance.UnregisterClass('ReportBuilderTool');

end.
