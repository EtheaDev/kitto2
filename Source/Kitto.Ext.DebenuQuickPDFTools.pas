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

unit Kitto.Ext.DebenuQuickPDFTools;

interface

uses
  DB,
  //DebenuPDFLibraryLite1112_TypeLibrary, old library 11.12
  DebenuPDFLibraryLite1114_TLB
  , Kitto.DebenuQuickPDF
  , SysUtils
  , Classes
  , System.UITypes
  , EF.Tree
  , Kitto.Ext.Files
  , Kitto.Ext.DataTool
  , Kitto.Ext.Base
  , Kitto.Ext.Tools
  , Kitto.Metadata.DataView
  , Kitto.Ext.StandardControllers;

type


  TPDFMergeProgressEvent = procedure (const FileName: string; NewStartPage, PageCount: Integer) of Object;
  TPDFAcceptFileEvent = procedure (const FileName: string; var Accept: boolean) of Object;

type
  TMergePDFToolController = class(TKExtDownloadFileController)
  strict private
    FMergePDFEngine : TKMergePDFEngine;
    function GetLayoutFileName: string;
    function GetBaseFileName: string;
  protected
    function GetDefaultFileName: string; override;
    procedure PrepareFile(const AFileName: string); override;
    function GetDefaultFileExtension: string; override;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
    class function GetDefaultImageName: string; override;
    property LayoutFileName: string read GetLayoutFileName;
    property BaseFileName: string read GetBaseFileName;
  end;

implementation

uses
  Windows
  , EF.Sys
  , System.Math
  , System.TypInfo
  , System.UIConsts
  , EF.Classes
  , EF.StrUtils
  , EF.Localization
  , EF.DB
  , EF.Macros
  , Kitto.JS.Controller
  , Kitto.Metadata.Models
  , Kitto.Config;

{ TMergePDFToolController }


procedure TMergePDFToolController.AfterConstruction;
begin
  inherited;
  FMergePDFEngine := TKMergePDFEngine.Create(nil);
end;

destructor TMergePDFToolController.Destroy;
begin
  FreeAndNil(FMergePDFEngine);
  inherited;
end;

function TMergePDFToolController.GetBaseFileName: string;
begin
  Result := Config.GetExpandedString('BaseFileName');
  ServerRecord.ExpandExpression(Result);
end;

function TMergePDFToolController.GetDefaultFileExtension: string;
begin
  Result := '.pdf';
end;

function TMergePDFToolController.GetDefaultFileName: string;
var
  LFileExtension: string;
begin
  LFileExtension := ExtractFileExt(ClientFileName);
  if LFileExtension = '' then
    LFileExtension := GetDefaultFileExtension;
  Result := GetTempFileName(LFileExtension);
  AddTempFilename(Result);
end;

class function TMergePDFToolController.GetDefaultImageName: string;
begin
  Result := 'pdf_document';
end;

function TMergePDFToolController.GetLayoutFileName: string;
begin
  Result := Config.GetExpandedString('LayoutFileName');
end;

procedure TMergePDFToolController.PrepareFile(const AFileName: string);
var
  LRecord: TKViewTableRecord;
  LBaseFileName, LLayoutFileName: string;

begin
  LRecord := ServerRecord;
  Assert(Assigned(LRecord), '"MergePDFTool controller works only on single record');
  LBaseFileName := BaseFileName;
  LLayoutFileName := LayoutFileName;
  FMergePDFEngine.MergePDF(AFileName, LLayoutFileName, LBaseFileName, LRecord);
end;

initialization
  TJSControllerRegistry.Instance.RegisterClass('MergePDFTool', TMergePDFToolController);

finalization
  TJSControllerRegistry.Instance.UnregisterClass('MergePDFTool');

end.
