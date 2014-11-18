{-------------------------------------------------------------------------------
   Copyright 2013 Ethea S.r.l.

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

unit Kitto.Ext.Tools;

interface

uses
  SysUtils, Classes,
  Kitto.Ext.Controller, Kitto.Ext.DataTool, Kitto.Ext.Base,
  Kitto.Metadata.DataView, Kitto.Ext.StandardControllers;

type
  TExportTextToolController = class(TKExtDownloadFileController)
  strict private
    function GetDelimiter: char;
    function GetFixedLength: boolean;
    function GetIncludeHeader: boolean;
    function GetQuoteChar: char;
  strict protected
    function GetDefaultFileExtension: string; override;
    function CreateStream: TStream; override;
    function GetDefaultIncludeHeader: boolean; virtual;
    function GetDefaultFixedLength: boolean; virtual;
    function GetDefaultDelimiter: char; virtual;
    function GetDefaultQuoteChar: char; virtual;
  public
    class function GetDefaultImageName: string;
  published
    property IncludeHeader: boolean read GetIncludeHeader;
    property FixedLength: boolean read GetFixedLength;
    property Delimiter: char read GetDelimiter;
    property QuoteChar: char read GetQuoteChar;
  end;

  TExportCSVToolController = class(TExportTextToolController)
  strict protected
    function GetDefaultFileExtension: string; override;
    function GetDefaultIncludeHeader: boolean; override;
    function GetDefaultFixedLength: boolean; override;
  end;

implementation

uses
  StrUtils,
  Ext, EF.DB, EF.Tree,
  Kitto.Ext.Session, Kitto.Config;

{ TExportCSVToolController }

function TExportCSVToolController.GetDefaultFixedLength: boolean;
begin
  Result := False;
end;

function TExportCSVToolController.GetDefaultIncludeHeader: boolean;
begin
  Result := True;
end;

function TExportCSVToolController.GetDefaultFileExtension: string;
begin
  Result := '.csv';
end;

{ TExportTextToolController }

function TExportTextToolController.GetDefaultDelimiter: char;
begin
  Result := ';';
end;

function TExportTextToolController.GetDefaultFixedLength: boolean;
begin
  Result := True;
end;

class function TExportTextToolController.GetDefaultImageName: string;
begin
  Result := 'text_document';
end;

function TExportTextToolController.GetDefaultIncludeHeader: boolean;
begin
  Result := False;
end;

function TExportTextToolController.GetDefaultQuoteChar: char;
begin
  Result := '"';
end;

function TExportTextToolController.GetDelimiter: char;
begin
  Result := Config.GetChar('Delimiter', GetDefaultDelimiter);
end;

function TExportTextToolController.GetFixedLength: boolean;
begin
  Result := Config.GetBoolean('FixedLength', GetDefaultFixedLength);
end;

function TExportTextToolController.GetIncludeHeader: boolean;
begin
  Result := Config.GetBoolean('IncludeHeader', GetDefaultIncludeHeader);
end;

function TExportTextToolController.GetQuoteChar: char;
begin
  Result := Config.GetChar('QuoteChar',GetDefaultQuoteChar);
end;

function TExportTextToolController.GetDefaultFileExtension: string;
begin
  Result := '.txt';
end;

function TExportTextToolController.CreateStream: TStream;
var
  LStore: TKViewTableStore;
  LFixedLength: boolean;
  LDelimiter, LQuoteChar: Char;
  LIncludeHeader: boolean;
  LRecordIndex: Integer;
  LValue, LLine: string;
  LRecord: TKViewTableRecord;
  LFieldIndex: Integer;
  LField: TKViewTableField;
  LViewField: TKViewField;
  LContent: string;

  procedure AddRow(const ARowData: string);
  begin
    if LContent <> '' then
      LContent := LContent + sLineBreak;
    LContent := LContent + ARowData;
  end;

  function FormatValue(const ALine: string; const ASize: integer): string;
  begin
    Result := Copy(ALine,1,ASize)+StringOfChar(' ', ASize - Length(ALine));
  end;

begin
  LStore := ServerStore;
  LFixedLength := FixedLength;
  LDelimiter := Delimiter;
  LQuoteChar := QuoteChar;
  LIncludeHeader := IncludeHeader;
  LContent := '';
  if LIncludeHeader then
  begin
    // Header.
    LLine := '';
    for LFieldIndex := 0 to LStore.Header.FieldCount - 1 do
    begin
      LViewField := LStore.Header.Fields[LFieldIndex].ViewField;
      if Assigned(LViewField) then
      begin
        LValue := NormalizeColumName(LViewField.DisplayLabel);
        if LFixedLength then
          LLine := LLine + FormatValue(LValue, LViewField.DisplayWidth)
        else
        begin
          if LFieldIndex <> 0 then
            LLine := LLine + LDelimiter;
          LLine := LLine + LQuoteChar + LValue + LQuoteChar;
        end;
      end;
    end;
    AddRow(LLine);
  end;

  // Rows.
  for LRecordIndex := 0 to LStore.RecordCount -1 do
  begin
    LRecord := LStore.Records[LRecordIndex];
    if not LRecord.IsDeleted then
    begin
      LLine := '';
      for LFieldIndex := 0 to LRecord.FieldCount - 1 do
      begin
        LField := LRecord.Fields[LFieldIndex];
        if Assigned(LField.ViewField) then
        begin
          LValue := LField.GetAsJSONValue(True, False, True);
          if LFixedLength then
            LLine := LLine + FormatValue(LValue, LField.ViewField.DisplayWidth)
          else
          begin
            if LFieldIndex <> 0 then
              LLine := LLine + LDelimiter;
            LLine := LLine + LQuoteChar + LValue + LQuoteChar;
          end;
        end;
      end;
      AddRow(LLine);
    end;
  end;
  Result := TStringStream.Create(LContent);
end;

initialization
  TKExtControllerRegistry.Instance.RegisterClass('ExportCSVTool', TExportCSVToolController);
  TKExtControllerRegistry.Instance.RegisterClass('ExportTextTool', TExportTextToolController);

finalization
  TKExtControllerRegistry.Instance.UnregisterClass('ExportCSVTool');
  TKExtControllerRegistry.Instance.UnregisterClass('ExportTextTool');

end.
