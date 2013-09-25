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
  TExportCSVToolController = class(TKExtDataToolController)
  private
    FCSVData: string;
    function StoreToCSV(const AStore: TKViewTableStore;
      const ADelimiter: Char = ';'; const AQuoteChar: Char = '"'): string;
  protected
    procedure ExecuteTool; override;
  published
    procedure DownloadFile;
  end;

implementation

uses
  Ext, EF.DB,
  Kitto.Ext.Session, Kitto.Config;

{ TExportCSVToolController }

procedure TExportCSVToolController.DownloadFile;
var
  LCSVDataStream: TStringStream;
begin
  LCSVDataStream := TStringStream.Create(FCSVData);
  try
    LCSVDataStream.Seek(0, soFromBeginning);
    Session.DownloadStream(LCSVDataStream, ViewTable.PluralDisplayLabel + '.csv');
  finally
    FreeAndNil(LCSVDataStream);
  end;
end;

procedure TExportCSVToolController.ExecuteTool;
begin
  inherited;
  FCSVData := StoreToCSV(ServerStore);
  Download(DownloadFile);
end;

function TExportCSVToolController.StoreToCSV(const AStore: TKViewTableStore;
  const ADelimiter, AQuoteChar: Char): string;
var
  LRecordIndex: Integer;
  LRecord: TKViewTableRecord;
  LFieldIndex: Integer;
  LField: TKViewTableField;
  LStringList: TStringList;

  procedure AddCSVRow(const ARowData: string);
  begin
    if Result <> '' then
      Result := Result + sLineBreak;
    Result := Result + ARowData;
  end;

begin
  Result := '';
  LStringList := TStringList.Create;
  try
    LStringList.Delimiter := ADelimiter;
    LStringList.QuoteChar := AQuoteChar;
    LStringList.StrictDelimiter := True;

    // Header.
    for LFieldIndex := 0 to AStore.Header.FieldCount - 1 do
    begin
      if Assigned(AStore.Header.Fields[LFieldIndex].ViewField) and
          AStore.Header.Fields[LFieldIndex].ViewField.IsVisible then
        LStringList.Add(AStore.Header.Fields[LFieldIndex].ViewField.DisplayLabel);
    end;
    AddCSVRow(LStringList.DelimitedText);

    // Rows.
    for LRecordIndex := 0 to AStore.RecordCount -1 do
    begin
      LStringList.Clear;
      LRecord := AStore.Records[LRecordIndex];
      if not LRecord.IsDeleted then
      begin
        for LFieldIndex := 0 to LRecord.FieldCount - 1 do
        begin
          LField := LRecord.Fields[LFieldIndex];
          if Assigned(LField.ViewField) and LField.ViewField.IsVisible then
            LStringList.Add(LField.GetAsJSONValue(True, False, True));
        end;
        AddCSVRow(LStringList.DelimitedText);
      end;
    end;
  finally
    FreeAndNil(LStringList);
  end;
end;

initialization
  TKExtControllerRegistry.Instance.RegisterClass('ExportCSVTool', TExportCSVToolController);

finalization
  TKExtControllerRegistry.Instance.UnregisterClass('ExportCSVTool');

end.
