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
  TExportFileToolController = class(TKExtDataToolController)
  private
  protected
    function ValidColumnName(const FieldName: string): string; virtual;
    function GetOutputFileName: string; virtual;
    function GetFileExtension: string;
    function GetDefaultFileExtension: string; virtual;
  published
    property OutputFileName: string read GetOutputFileName;
  end;

  TExportTextToolController = class(TExportFileToolController)
  private
    function StoreToTextFile(const AStore: TKViewTableStore;
      const AFixedLength: boolean;
      const ADelimiter: Char; const AQuoteChar: Char;
      const AIncludeHeader: boolean): string;
    function GetDelimiter: char;
    function GetFixedLength: boolean;
    function GetIncludeHeader: boolean;
    function GetQuoteChar: char;
  protected
    FTextContent: string;
    procedure ExecuteTool; override;
    function GetDefaultFileExtension: string; override;
    function GetDefaultIncludeHeader: boolean; virtual;
    function GetDefaultFixedLength: boolean; virtual;
    function GetDefaultDelimiter: char; virtual;
    function GetDefaultQuoteChar: char; virtual;
  published
    procedure DownloadTextFile;
    property IncludeHeader: boolean read GetIncludeHeader;
    property FixedLength: boolean read GetFixedLength;
    property Delimiter: char read GetDelimiter;
    property QuoteChar: char read GetQuoteChar;
  end;

  TExportCSVToolController = class(TExportTextToolController)
  private
  protected
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

{ TExportFileToolController }

function TExportFileToolController.GetDefaultFileExtension: string;
begin
  Result := '.txt';
end;

function TExportFileToolController.GetFileExtension: string;
begin
  Result := ExtractFileExt(OutputFileName);
end;

function TExportFileToolController.GetOutputFileName: string;
var
  LNode: TEFNode;
begin
  //Check specific OutputFileName node
  LNode := Config.FindNode('OutputFileName');
  if Assigned(LNode) then
    Result := LNode.AsString
  else
    Result := ViewTable.PluralDisplayLabel + GetDefaultFileExtension;
end;

function TExportFileToolController.ValidColumnName(const FieldName: string): string;
begin
  Result := StringReplace(FieldName, ' ','_',[rfReplaceAll]);
  Result := StringReplace(Result, '.','_',[rfReplaceAll]);
end;

{ TExportTextToolController }

procedure TExportTextToolController.DownloadTextFile;
var
  LTextDataStream: TStringStream;
begin
  LTextDataStream := TStringStream.Create(FTextContent);
  try
    LTextDataStream.Seek(0, soFromBeginning);
    Session.DownloadStream(LTextDataStream, GetOutputFileName);
  finally
    FreeAndNil(LTextDataStream);
  end;
end;

procedure TExportTextToolController.ExecuteTool;
begin
  inherited;
  FTextContent := StoreToTextFile(ServerStore, FixedLength, Delimiter, QuoteChar, IncludeHeader);
  Download(DownloadTextFile);
end;

function TExportTextToolController.GetDefaultDelimiter: char;
begin
  Result := ';';
end;

function TExportTextToolController.GetDefaultFixedLength: boolean;
begin
  Result := True;
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
var
  LNode: TEfNode;
begin
  LNode := Config.FindNode('Delimiter');
  if Assigned(LNode) then
    Result := LNode.AsChar
  else
    Result := GetDefaultDelimiter;
end;

function TExportTextToolController.GetFixedLength: boolean;
var
  LNode: TEfNode;
begin
  LNode := Config.FindNode('FixedLength');
  if Assigned(LNode) then
    Result := LNode.AsBoolean
  else
    Result := GetDefaultFixedLength;
end;

function TExportTextToolController.GetIncludeHeader: boolean;
var
  LNode: TEfNode;
begin
  LNode := Config.FindNode('IncludeHeader');
  if Assigned(LNode) then
    Result := LNode.AsBoolean
  else
    Result := GetDefaultIncludeHeader;
end;

function TExportTextToolController.GetQuoteChar: char;
var
  LNode: TEfNode;
begin
  LNode := Config.FindNode('QuoteChar');
  if Assigned(LNode) then
    Result := LNode.AsChar
  else
    Result := GetDefaultQuoteChar;
end;

function TExportTextToolController.GetDefaultFileExtension: string;
begin
  Result := '.txt';
end;

function TExportTextToolController.StoreToTextFile(
  const AStore: TKViewTableStore; const AFixedLength: boolean; const ADelimiter,
  AQuoteChar: Char; const AIncludeHeader: boolean): string;
var
  LRecordIndex: Integer;
  LValue, LLine: string;
  LRecord: TKViewTableRecord;
  LFieldIndex: Integer;
  LField: TKViewTableField;
  LStringList: TStringList;
  LViewField: TKViewField;

  procedure AddRow(const ARowData: string);
  begin
    if Result <> '' then
      Result := Result + sLineBreak;
    Result := Result + ARowData;
  end;

  function FormatLine(const ALine: string; const ASize: integer): string;
  begin
    Result := Copy(ALine,1,ASize)+StringOfChar(' ', ASize - Length(ALine));
  end;

begin
  Result := '';
  LStringList := TStringList.Create;
  try
    if not AFixedLength then
    begin
      LStringList.Delimiter := ADelimiter;
      LStringList.QuoteChar := AQuoteChar;
      LStringList.StrictDelimiter := True;
    end;

    if AIncludeHeader then
    begin
      // Header.
      LLine := '';
      for LFieldIndex := 0 to AStore.Header.FieldCount - 1 do
      begin
        LViewField := AStore.Header.Fields[LFieldIndex].ViewField;
        if Assigned(LViewField) then
        begin
          LValue := ValidColumnName(LViewField.DisplayLabel);
          if AFixedLength then
            LLine := LLine + FormatLine(LValue, LViewField.DisplayWidth)
          else
            LStringList.Add(LValue);
        end;
      end;
      if AFixedLength then
        AddRow(LLine)
      else
        AddRow(LStringList.DelimitedText);
    end;

    // Rows.
    for LRecordIndex := 0 to AStore.RecordCount -1 do
    begin
      LStringList.Clear;
      LRecord := AStore.Records[LRecordIndex];
      if not LRecord.IsDeleted then
      begin
        LLine := '';
        for LFieldIndex := 0 to LRecord.FieldCount - 1 do
        begin
          LField := LRecord.Fields[LFieldIndex];
          if Assigned(LField.ViewField) then
          begin
            LValue := LField.GetAsJSONValue(True, False, True);
            if AFixedLength then
              LLine := LLine + FormatLine(LValue, LField.ViewField.DisplayWidth)
            else
              LStringList.Add(LValue);
          end;
        end;
      if AFixedLength then
        AddRow(LLine)
      else
        AddRow(LStringList.DelimitedText);
      end;
    end;
  finally
    FreeAndNil(LStringList);
  end;
end;

initialization
  TKExtControllerRegistry.Instance.RegisterClass('ExportCSVTool', TExportCSVToolController);
  TKExtControllerRegistry.Instance.RegisterClass('ExportTextTool', TExportTextToolController);

finalization
  TKExtControllerRegistry.Instance.UnregisterClass('ExportCSVTool');
  TKExtControllerRegistry.Instance.UnregisterClass('ExportTextTool');

end.
