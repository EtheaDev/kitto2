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

///	<summary>
///	  Support for generating documents by FOP (Formatting Objects Processor)
///	</summary>
///	<seealso href="http://xmlgraphics.apache.org/fop/">
///	  Apache FOP Project
///	</seealso>
unit EF.FOP;

interface

uses
  SysUtils, Classes;

const
  FOP_BATCH = 'FOP.BAT';
  XSL_FILE_EXT = '.xsl';
  XML_FILE_EXT = '.xml';
  XSLFO_FILE_EXT = '.fo';
  FOPPDF_FILE_EXT = '.pdf';
  FOPPCL_FILE_EXT = '.pcl';
  FOPPS_FILE_EXT = '.ps';
  FOPTXT_FILE_EXT = '.txt';
  FOPRTF_FILE_EXT = '.rtf';

type
  EFopEngineError = class(Exception);

  TFopOutputType = (otPdf,otPcl,otPs,otTxt,otRtf);

const
  AFopOutputParam : Array[TFopOutputType] of string =
    ('-pdf','-pcl','-ps','-txt','-rtf');

  AFopOutputExtension : Array[TFopOutputType] of string =
    (FOPPDF_FILE_EXT,FOPPCL_FILE_EXT,FOPPS_FILE_EXT,FOPTXT_FILE_EXT,FOPRTF_FILE_EXT);

Type
  TDocumentEvent = procedure ( OutputFile : string; var DeleteAfter : Boolean ) of Object;

  { TEFFopReport }

  TEFFopReport = class(TComponent)
  private
    FXMLDataFile: string;
    FBeforePrint: TNotifyEvent;
    FAfterPrint: TNotifyEvent;
    FOutputFile: string;
    FOnShowDocument: TDocumentEvent;
    FOnPrintDocument: TDocumentEvent;
    FFopPath: string;
    FXSL_FOFile: string;
    FDebugMode: boolean;
    FFOPBatch: string;
    FXSLReportFile: string;
    FBeforePreview: TNotifyEvent;
    FAfterPreview: TNotifyEvent;
    FBeforeGenerateXML: TNotifyEvent;
    FOnGenerateXML: TNotifyEvent;
    FShowCommand : Boolean;
    FDeleteXMLAfterCreate: Boolean;
    FDeleteAfterShow: Boolean;
    FDeleteAfterPrint: Boolean;
    FBeforeCreatePDF: TNotifyEvent;
    FAfterCreatePDF: TNotifyEvent;
    FFOPOutputType: TFopOutputType;
    procedure SetXMLDataFile(const AValue: string);
    procedure SetXSL_FOFile(const AValue: string);
    procedure SetXSLReportFile(const AValue: string);
    procedure SetOutputFile(const AValue: string);
    procedure ShowDocument(const AFileName : string);
    procedure PrintDocument(const AFileName : string);
    procedure GenerateFOP( APreview, APrint : boolean );
    procedure SetFopPath(const AValue: string);
    procedure UpdateOutputType;
  public
    function GetOutpustring: string;
    function GetOutputFileExt: string;
    ///	<summary>Generate file by FOP engine and call OnPrintDocument event.</summary>
    procedure Print;
    ///	<summary>Generate file by FOP engine and call OnShowDocument event to open it.</summary>
    procedure Preview;
    ///	<summary>Generate file by FOP engine.</summary>
    procedure Build;
    constructor Create(AOwner : TComponent); override;
  published
    property FOPBatch : string read FFOPBatch write FFOPBatch;
    property DebugMode : boolean read FDebugMode write FDebugMode default False;
    property FOPPath : string read FFopPath write SetFopPath;
    property XMLDataFile : string read FXMLDataFile write SetXMLDataFile;
    property XSLReportFile : string read FXSLReportFile write SetXSLReportFile;
    property XSL_FOFile : string read FXSL_FOFile write SetXSL_FOFile;
    property OutputFile : string read FOutputFile write SetOutputFile;
    property BeforeGenerateXML : TNotifyEvent read FBeforeGenerateXML write FBeforeGenerateXML;
    property OnGenerateXML : TNotifyEvent read FOnGenerateXML write FOnGenerateXML;
    property BeforePrint : TNotifyEvent read FBeforePrint write FBeforePrint;
    property AfterPrint : TNotifyEvent read FAfterPrint write FAfterPrint;
    property BeforePreview : TNotifyEvent read FBeforePreview write FBeforePreview;
    property AfterPreview : TNotifyEvent read FAfterPreview write FAfterPreview;
    property OnShowDocument : TDocumentEvent read FOnShowDocument write FOnShowDocument;
    property OnPrintDocument : TDocumentEvent read FOnPrintDocument write FOnPrintDocument;
    property ShowCommand : Boolean read FShowCommand write FShowCommand default False;
    property DeleteXMLAfterCreate : Boolean read FDeleteXMLAfterCreate write FDeleteXMLAfterCreate default False;
    property DeleteAfterShow : Boolean read FDeleteAfterShow write FDeleteAfterShow default False;
    property DeleteAfterPrint : Boolean read FDeleteAfterPrint write FDeleteAfterPrint default False;
    property BeforeCreatePDF : TNotifyEvent read FBeforeCreatePDF write FBeforeCreatePDF;
    property AfterCreatePDF : TNotifyEvent read FAfterCreatePDF write FAfterCreatePDF;
    property FOPOutputType : TFopOutputType read FFOPOutputType write FFOPOutputType default otPdf;
  end;

implementation

uses
  IOUtils
  , EF.Sys
  , EF.Localization
  {$IFDEF POSIX}
  , Posix.Unistd
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  , EF.Shell
  {$ENDIF}
  ;

function PDF_FILE_NOT_BUILT: string; begin Result := _('Error. Cannot generate file by FOP engine: %s'); end;
function ERR_PDF_FILE_ACCESS_DENIED: string; begin Result := _('Error. Cannot create file %s by FOP engine: access denied'); end;
function PRINT_FILE: string; begin Result := _('Generating and printing file "%s" ...'); end;
function NO_FILE_FO_XML: string; begin Result := _('FO file or XML file not specified'); end;
function NO_FILE_FO_XML_XSL: string; begin Result := _('FO file or XML file or XSL file not specified'); end;
function NO_FOP_PATH: string; begin Result := _('FOP engine folder not specified.'); end;
function NO_FOP_BATCH: string; begin Result := _('FOP engine command not specified.'); end;
function PRINT_NOT_AVAILABLE: string; begin Result := _('PDF direct printing is not allowed'); end;
function ERR_FOP: string; begin Result := _('Error in PDF creator engine execution (FOP engine)'); end;
function BATCH_NOT_FOUND: string; begin Result := _('Batch Files "%s" for starting FOP engine not found.'); end;

{ TEFFopReport }

constructor TEFFopReport.Create(AOwner: TComponent);
begin
  inherited;
  FFOPBatch := FOP_BATCH;
  FFOPOutputType := otPdf;
end;

// ritorna il nome del file di output da generare
function TEFFopReport.GetOutpustring : string;
begin
  if FOutputFile <> '' then // se è specificato il file generare, ritorna quello
  begin
    Result := FOutputFile;
  end
  else // se non è specificato il file PDF da generare, lo ricava dal nome del file FO o XML
  begin
    if FXSL_FOFile <> '' then
      Result := ChangeFileExt(FXSL_FOFile,GetOutputFileExt)
    else if FXMLDataFile <> '' then
      Result := ChangeFileExt(FXMLDataFile,GetOutputFileExt)
    else
      Result := '';
  end;
end;

function TEFFopReport.GetOutputFileExt: string;
begin
  Result := AFopOutputExtension[FFOPOutputType];
end;

procedure TEFFopReport.GenerateFOP( APreview, APrint : boolean );
var
  StrDebug, StrParams, StrOutPut : string;
  FileName : string;
  BatchFileName : string;
  BatchCommand : string;
begin
  if Assigned(BeforeGenerateXML) then
    BeforeGenerateXML(Self);

  if Assigned(OnGenerateXML) then
    OnGenerateXML(Self);

  if Assigned(BeforeCreatePDF) then
    BeforeCreatePDF(Self);

  if FFopPath = '' then
    raise EFopEngineError.Create( NO_FOP_PATH );
  if FFopBatch = '' then
    raise EFopEngineError.Create( NO_FOP_BATCH );

  BatchFileName := IncludeTrailingPathDelimiter(FFopPath)+FFopBatch;

  if not FileExists(BatchFileName) then
    raise EFopEngineError.CreateFmt( BATCH_NOT_FOUND, [BatchFileName] );

  if FDebugMode then
    StrDebug := '-d'
  else
    StrDebug := '';

  FileName := GetOutpustring;
  StrOutPut := AFopOutputParam[FFOPOutputType]+' "'+FileName+'"';

  if FileName = '' then
    raise EFopEngineError.Create( NO_FILE_FO_XML )
  else if FileExists(FileName) then
  begin
    if not SysUtils.DeleteFile(PChar(FileName)) then
      Raise EInOutError.CreateFmt(ERR_PDF_FILE_ACCESS_DENIED,[FileName]);
  end;

  //1) if XSL_FO file specified, build fo file to the ooutput (PDF/PCL/PS/RTF)
  if FXSL_FOFile <> '' then
  begin
    StrParams := StrDebug+' "'+FXSL_FOFile+'" '+StrOutPut;
  end
  //2) if XML and XSL files specified, transform and build to the ooutput (PDF/PCL/PS/RTF)
  else if (FXMLDataFile <> '') and (FXSLReportFile <> '') then
  begin
    StrParams := StrDebug+' '+'-xsl "'+FXSLReportFile+'" '+'-xml "'+FXMLDataFile+'" '+StrOutPut;
  end
  else
    raise EFopEngineError.Create( NO_FILE_FO_XML_XSL );

  //run batch command
  BatchCommand := '"'+BatchFileName+'" '+StrParams;

  if EFSys.ExecuteCommand(TPath.Combine(FFopPath, BatchCommand)) <> 0 then
    raise Exception.Create(ERR_FOP);

  // verify if the file PDF/PCL/PS/RTF was generated
  if not FileExists(FileName) then
    raise EFopEngineError.CreateFmt(PDF_FILE_NOT_BUILT,[FileName]);

  // delete XML file id necessary
  if FDeleteXMLAfterCreate then
  begin
    if (FXMLDataFile <> '') and FileExists(FXMLDataFile) then
      DeleteFile( PChar(FXMLDataFile) );
  end;

  if Assigned(AfterCreatePDF) then
    AfterCreatePDF(Self);

  if APreview then
  begin
    if Assigned(BeforePreview) then
      BeforePreview(Self);
    ShowDocument(FileName);
    if Assigned(AfterPreview) then
      AfterPreview(Self);
  end
  else if APrint then
  begin
    if Assigned(BeforePrint) then
      BeforePrint(Self);
    PrintDocument( FileName );
    if Assigned(AfterPrint) then
      AfterPrint(Self);
  end;
end;

procedure TEFFopReport.Preview;
begin
  GenerateFOP(True, False);
end;

procedure TEFFopReport.Print;
begin
  GenerateFOP(False, True);
end;

procedure TEFFopReport.SetOutputFile(const AValue: string);
begin
  FOutputFile := AValue;
  UpdateOutputType;
end;

procedure TEFFopReport.SetXMLDataFile(const AValue: string);
begin
  FXMLDataFile := AValue;
  FXSL_FOFile := '';
end;

procedure TEFFopReport.UpdateOutputType;
var
  i : TFopOutputType;
  FileExt : string;
begin
  if FOutputFile = '' then
    Exit
  else
    FileExt := ExtractFileExt(FOutputFile);
  for i := Low(TFopOutputType) to High(TFopOutputType) do
  begin
    if SameText(FileExt, AFopOutputExtension[i] ) then
    begin
      FFOPOutputType := i;
      break;
    end;
  end;
end;

procedure TEFFopReport.SetXSL_FOFile(const AValue: string);
begin
  FXSL_FOFile := AValue;
  FXMLDataFile := '';
  FXSLReportFile := '';
end;

procedure TEFFopReport.SetXSLReportFile(const AValue: string);
begin
  FXSLReportFile := AValue;
  FXSL_FOFile := '';
end;

procedure TEFFopReport.ShowDocument(const AFileName : string);
var
  LDeleteAfterShow: boolean;
begin
  if Assigned(FOnShowDocument) then
  begin
    LDeleteAfterShow := FDeleteAfterShow;
    FOnShowDocument( AFileName, LDeleteAfterShow );
    if LDeleteAfterShow then
      DeleteFile(AFileName);
  end
  {$IFDEF MSWINDOWS}
  else
    OpenDocument(AFileName, False);
  {$ENDIF}
end;

procedure TEFFopReport.PrintDocument(const AFileName : string);
var
  LDeleteAfterPrint: boolean;
begin
  if Assigned(FOnPrintDocument) then
  begin
    LDeleteAfterPrint := FDeleteAfterPrint;
    FOnPrintDocument( AFileName, LDeleteAfterPrint );
    if LDeleteAfterPrint then
      DeleteFile(AFileName);
  end
  else
    raise EFopEngineError.Create(PRINT_NOT_AVAILABLE);
end;

procedure TEFFopReport.Build;
begin
  GenerateFOP(False, False);
end;

procedure TEFFopReport.SetFopPath(const AValue: string);
begin
  //se mi arriva un nome di file estraggo la path
  if (csDesigning in ComponentState) and FileExists(AValue) then
    FFopPath := ExtractFilePath(AValue)
  else
    FFopPath := IncludeTrailingPathDelimiter(AValue);
end;

end.
