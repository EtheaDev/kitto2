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

unit Kitto.Ext.XSLTools;

interface

uses
  msxml
  , ActiveX
  , Windows
  , SysUtils
  , Classes
  , DB
  , EF.Tree
  , Kitto.Ext.Controller
  , Kitto.Ext.DataTool
  , Kitto.Ext.Base
  , Kitto.Ext.Tools
  , Kitto.Metadata.DataView
  , Kitto.Ext.StandardControllers
  ;

type
  TXSLTransformer = class(TObject)
  public
    class function ApplyXslToHtml(const ATransformFileName: string;
      AStore: TKViewTableStore; ARecord: TKViewTableRecord): string;
  end;

  TXSLToolController = class(TKExtDownloadFileController)
  strict private
    function GetTransformFileName: string; protected
    function GetDefaultFileName: string; override;
    procedure PrepareFile(const AFileName: string); override;
    function GetDefaultFileExtension: string; override;
  public
    class function GetDefaultImageName: string; override;
  //published
    property TransformFileName: string read GetTransformFileName;
  end;

implementation

uses
  System.Math
  , System.TypInfo
  , System.UIConsts
  , System.UITypes

  , Ext.Base

  , EF.Classes
  , EF.StrUtils
  , EF.Localization
  , EF.DB
  , EF.Sys
  , EF.Macros
  , EF.XML

  , Kitto.Metadata.Models
  , Kitto.Config
  ;

function TransformXMLText(const AXMLText, AXSLText: string): string;
var
  LXMLDocument: IXMLDOMDocument;
  LXSLDocument: IXMLDOMDocument;
  LOutputStreamAdapter: TStreamAdapter;
begin
  Result := '';

  // Load XML text into a DOM.
  LXMLDocument := CoDOMDocument.Create;
  LXMLDocument.loadXML(AXMLText);

  // Load XSL stylesheet text into a DOM.
  LXSLDocument := CoDOMDocument.Create;
  LXSLDocument.loadXML(AXSLText);

  // Generate output.
  LOutputStreamAdapter := TStreamAdapter.Create(TStringStream.Create('', TEncoding.UTF8), soOwned);
  LXMLDocument.transformNodeToObject(LXSLDocument, LOutputStreamAdapter as IStream);
  Result := (LOutputStreamAdapter.Stream as TStringStream).DataString;
end;

{ TXSLToolController }

function TXSLToolController.GetDefaultFileExtension: string;
begin
  Result := '.html';
end;

function TXSLToolController.GetDefaultFileName: string;
var
  LFileExtension: string;
begin
  LFileExtension := ExtractFileExt(ClientFileName);
  if LFileExtension = '' then
    LFileExtension := GetDefaultFileExtension;
  Result := GetTempFileName(LFileExtension);
  AddTempFilename(Result);
end;

class function TXSLToolController.GetDefaultImageName: string;
begin
  Result := 'html_document';
end;

function TXSLToolController.GetTransformFileName: string;
begin
  Result := Config.GetExpandedString('TransformFileName');
end;

procedure TXSLToolController.PrepareFile(const AFileName: string);
var
  LHTMLText: string;
  LFileStream: TStringStream;
begin
  Assert(TransformFileName <> '','XSL TransformFileName is mandatory');
  LHTMLText := TXSLTransformer.ApplyXslToHtml(TransformFileName, ServerStore, ServerRecord);

  LFileStream := TStringStream.Create(LHTMLText, TEncoding.UTF8);
  try
    //Save output file
    LFileStream.SaveToFile(AFileName);
  finally
    LFileStream.Free;
  end;

  //Add to temporary files so it will cleaned up
  AddTempFilename(AFileName);
end;

{ TXSLTransformer }

class function TXSLTransformer.ApplyXslToHtml(const ATransformFileName: string; AStore: TKViewTableStore;
  ARecord: TKViewTableRecord): string;
var
  LXSLFileName: string;
  LXMLContent, LXSLContent: string;
  LHTMLText: string;
  LFileStream: TStringStream;
begin
  Assert(ATransformFileName <> '','XSL TransformFileName is mandatory');

  //Build XML data file
  if Assigned(ARecord) then
    LXMLContent := XMLHeader + ARecord.GetAsXML(True)
  else
    LXMLContent := XMLHeader + AStore.GetAsXML(True);

  LFileStream := TStringStream.Create(LXMLContent, TEncoding.UTF8);
  try
    //Load XSL file and macro-substitute some elements
    LXSLFileName := ATransformFileName;
    LFileStream.LoadFromFile(LXSLFileName);
    LXSLContent := LFileStream.DataString;

    //Expand macros contained into xsl file like:
    // %FILENAME_TO_URL(%APP_PATH%ReportTemplates/logo.jpg)%
    // or %DATE% or %TIME%
    LXSLContent := TEFMacroExpansionEngine.Instance.Expand(LXSLContent);

    //Transform XSL + XML to HTML via MSXML
    LHTMLText := TransformXMLText(LXMLContent, LXSLContent);

    //Save output file
    LFileStream.Position := 0;
    LFileStream.WriteString(LHTMLText);
    Result := LFileStream.DataString;

  finally
    LFileStream.Free;
  end;
end;

initialization
  TKExtControllerRegistry.Instance.RegisterClass('XSLTool', TXSLToolController);

finalization
  TKExtControllerRegistry.Instance.UnregisterClass('XSLTool');

end.
