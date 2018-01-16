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
unit Kitto.RB.ppStorePipe;

interface

uses
  SysUtils, Classes, Variants, Contnrs, Graphics,
  //ReportBuilder
  ppTypes, ppCharacter, ppDB, ppComm, ppRTTI, ppUtils, ppCollectionBase,
  //Kitto Store
  EF.Tree, Kitto.Store;

type
  TppKStorePipeline = class(TppCustomDataPipeline)
    strict private
      FKStore: TKStore;
      FKRecord, FSingleRecord: TKRecord;
      FKField: TKField;
      FKHeaderField: TKHeaderField;
      FActive: boolean;
      FBOF: boolean;
      FEOF: boolean;
      FPicture: TPicture;
      function SetCurrentField(AFieldName: String): Boolean;
      function SetCurrentHeaderField(AFieldName: String): Boolean;
    protected
      procedure CloseDataSet; override;
      function  GetActive: Boolean; override;
      function  CheckBOF: Boolean; override;
      function  CheckEOF: Boolean; override;
      procedure GotoFirstRecord; override;
      procedure GotoLastRecord; override;
      function IsValidBookmark(aBookmark: NativeInt): Boolean; virtual;
      procedure OpenDataSet; override;
      function TraverseBy(aIncrement: Integer): Integer; override;
    public
      constructor CreatePipeline(aOwner: TComponent; AKStore: TKStore;
        AKRecord: TKRecord; const LPipeLineName: string);
      constructor Create(aOwner: TComponent); override;
      destructor Destroy; override;
      procedure FreeBookmark(aBookmark: NativeInt); override;
      function GetBookmark: NativeInt; override;
      function GetDataSetName: String; override;
      function GetFieldIsCalculated(aFieldName: String): Boolean; override;
      function GetFieldNames(aFieldNameList: TStrings): Boolean; override;
      function GetFieldAsDouble(aFieldName: String): Double; override;
      function GetFieldAsGuid(aFieldName: String): TGUID; override;
      procedure GetFieldAsStream(aFieldName: String; aStream: TStream); override;
      function GetFieldAsPicture(aFieldName: String): TPicture; override;
      function GetFieldAsString(aFieldName: String): String; override;
      function GetFieldDataType(aFieldName: String): TppDataType; override;
      function GetFieldIsNull(aFieldName: String): Boolean; override;
      function GetFieldValue(aFieldName: string): Variant; override;
      procedure GotoBookmark(aBookmark: NativeInt); override;
    published
      property MoveBy;
      property RangeEnd;
      property RangeEndCount;
      property RangeBegin;
      property SkipWhenNoRecords;
      property UserName;
      property Visible;
      property AfterClose;
      property AfterOpen;
      property BeforeClose;
      property BeforeOpen;
      property OnDataChange;
      property OnFirst;
      property OnGotoBookmark;
      property OnLast;
      property OnMasterRecordPositionChange;
      property OnNext;
      property OnPrior;
      property OnRecordPositionChange;
      property OnTraversal;
  end;

  {TraTppKStorePipelineRTTI}
  TraTppKStorePipelineRTTI = class(TraTppDataPipelineRTTI)
    public
      class function  GetPropRec(aClass: TClass; const aPropName: String; var aPropRec: TraPropRec): Boolean; override;
      class function  GetPropValue(aObject: TObject; const aPropName: String; var aValue): Boolean; override;
      class function  RefClass: TClass; override;
      class function  SetPropValue(aObject: TObject; const aPropName: String; var aValue): Boolean; override;
  end;

  function ppConvertFieldType(aFieldType: TEFDataType): TppDataType;

implementation

{ TppKStorePipeline }

constructor TppKStorePipeline.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FActive  := False;
  FEOF := True;
  FBOF := True;
  FKRecord := nil;
  AutoCreateFields := True;
end;

constructor TppKStorePipeline.CreatePipeline(aOwner: TComponent;
  AKStore: TKStore; AKRecord: TKRecord; const LPipeLineName: string);
begin
  Create(aOwner);
  FKStore := AKStore;
  FSingleRecord := AKRecord;
  FKRecord := AKRecord;
  Name := LPipeLineName;
  CreateDefaultFields;
end;

destructor TppKStorePipeline.Destroy;
begin
  Destroying;
  Close;
  inherited Destroy;
end;

procedure TppKStorePipeline.FreeBookmark(aBookmark: NativeInt);
begin
  ;
end;

function TppKStorePipeline.GetActive: Boolean;
begin
  Result := FActive;
end;

function TppKStorePipeline.GetFieldNames(aFieldNameList: TStrings): Boolean;
begin
  Result := True;
  if (FKStore <> nil) and (FKStore.Header <> nil) then
  begin
    FKStore.Header.GetFieldNames(aFieldNameList);
  end
  else
  begin
    aFieldNameList.Clear;
    Result := False;
  end;
  if not(CreatingDefaultFields) then
    Result := inherited GetFieldNames(aFieldNameList);
end;

function TppKStorePipeline.GetDataSetName: String;
begin
  if FKRecord <> nil then
    Result := FKRecord.Name
  else
    Result := '';
end;

function TppKStorePipeline.GetFieldAsDouble(aFieldName: String): Double;
begin
  Result := 0;
  if not SetCurrentField(aFieldName) or (ppdaNoRecords in State) then
    Exit;
  Result := FKField.AsFloat;
end;

function TppKStorePipeline.GetFieldAsGuid(aFieldName: String): TGUID;
begin
  Result := cNullGuid;
  if not SetCurrentField(aFieldName) or (ppdaNoRecords in State) then
    Exit;
  Result := StringToGUID(GetFieldAsString(aFieldName));
end;

function TppKStorePipeline.GetFieldAsPicture(aFieldName: String): TPicture;
var
  lStream: TMemoryStream;
begin
  lStream := TMemoryStream.Create;
  try
    GetFieldAsStream(aFieldName, lStream);
    lStream.Position := 0;
    // force type to bitmap - will only work for bitmaps
    FPicture.Bitmap.LoadFromStream(lStream);
  finally
    lStream.Free;
  end;
  Result := FPicture;
end;

function TppKStorePipeline.GetFieldAsString(aFieldName: String): String;
begin
  Result := '';
  if not SetCurrentField(aFieldName) or (ppdaNoRecords in State) then
    Exit;
  Result := FKField.AsString;
end;

function TppKStorePipeline.GetFieldDataType(aFieldName: String): TppDataType;
begin
  Result := dtNotKnown;
  // optimization, use TppField if possible
  if SetFieldName(aFieldName) then
    Result := CurrentField.DataType;

  if (Result = dtNotKnown) then
  begin
    // try to use TKHeaderField info
    if SetCurrentHeaderField(aFieldName) then
      Result := ppConvertFieldType(FKHeaderField.DataType);
  end;
end;

function TppKStorePipeline.GetFieldIsCalculated(aFieldName: String): Boolean;
begin
  Result := False;
end;

function TppKStorePipeline.GetFieldIsNull(aFieldName: String): Boolean;
begin
  Result := True;
  if not SetCurrentField(aFieldName) or (ppdaNoRecords in State) then
    Exit;
  Result := FKField.IsNull;
end;

function TppKStorePipeline.GetFieldValue(aFieldName: string): Variant;
var
  lField: TKField;
begin
  Result := Null;

  if (FKRecord <> nil) then
    lField := FKRecord.FindField(aFieldName)
  else
    lField := nil;

  if (lField = nil) or (ppdaNoRecords in State) then
    Exit;

  if (lField.IsNull) then
    Exit;

  case GetFieldDataType(aFieldName) of
    dtString, dtChar, dtMemo:
      Result := lField.AsString;
    dtDouble, dtSingle, dtExtended:
      Result := lField.AsFloat;
    dtInteger, dtLongInt:
      Result := lField.AsInteger;
    dtLargeInt:
      Result := lField.AsInteger;
    dtCurrency:
      Result := lField.AsCurrency;
    dtDate, dtTime, dtDateTime:
      Result := lField.AsDateTime;
    dtBoolean:
      Result := lField.AsBoolean;
    dtGUID:
      Result := lField.AsString;
  else
    Result := lField.AsString;
  end;
end;

function TppKStorePipeline.SetCurrentField(AFieldName: String): Boolean;
begin
  Result := False;
  if (FKRecord <> nil) then
  begin
    FKField := FKRecord.FindField(AFieldName);
    Result := FKField <> nil;
  end;
end;

function TppKStorePipeline.SetCurrentHeaderField(AFieldName: String): Boolean;
begin
  Result := False;
  if (FKStore <> nil) and (FKStore.Header <> nil) then
  begin
    FKHeaderField := FKStore.Header.FindField(AFieldName);
    Result := FKHeaderField <> nil;
  end;
end;

function TppKStorePipeline.GetBookmark: NativeInt;
begin
  if (FKRecord = nil) then
    Result := 0
  else
    Result := FKRecord.Index + 1;
end;

procedure TppKStorePipeline.GotoBookmark(aBookmark: NativeInt);
begin
  if (FKStore = nil) then
    Exit;
  if IsValidBookmark(aBookmark) then
    FKRecord := FKStore.Records[aBookmark-1];
  inherited GotoBookmark(aBookmark);
end;

function TppKStorePipeline.IsValidBookmark(aBookmark: NativeInt): Boolean;
begin
  Result := Assigned(FKStore) and (FKStore.RecordCount > aBookmark);
end;

procedure TppKStorePipeline.GetFieldAsStream(aFieldName: String; aStream: TStream);
var
  LStream: TBytesStream;
begin
  if not(SetCurrentField(aFieldName)) then
    Exit;

  if (FKField.DataType is TEFBlobDataType) and not (FKField.IsNull) then
  begin
    LStream := TBytesStream.Create(FKField.AsBytes);
    try
      LStream.Position := 0;
      if Lstream.Size > 0 then
        aStream.CopyFrom(LStream, Lstream.Size);
    finally
      FreeAndNil(LStream);
    end;
  end;
end;

function TppKStorePipeline.CheckBOF: Boolean;
begin
  Result := (FKStore.RecordCount = 0) or FBOF;
end;

function TppKStorePipeline.CheckEOF: Boolean;
begin
  Result := (FKStore.RecordCount = 0) or FEOF;
end;

procedure TppKStorePipeline.OpenDataSet;
begin
  if not Assigned(FKStore) and not Assigned(FSingleRecord) then
    Exit;

  if Assigned(FSingleRecord) then
  begin
    FKRecord := FSingleRecord;
    FBOF := False;
    FEOF := False;
  end
  else if (FKStore.RecordCount > 0) then
  begin
    FKRecord := FKStore.Records[0];
    FBOF := False;
    FEOF := False;
  end
  else
  begin
    FKRecord := nil;
    FBOF := True;
    FEOF := True;
  end;
  FActive := True;
end;

procedure TppKStorePipeline.CloseDataSet;
begin
  FActive := False;
  FKRecord := nil;
  FBOF := True;
  FEOF := True;
end;

procedure TppKStorePipeline.GotoFirstRecord;
begin
  if FSingleRecord <> nil then
  begin
    FKRecord := FSingleRecord;
    FEOF := False;
  end
  else if (FKStore <> nil) and (FKStore.RecordCount > 0) then
  begin
    FKRecord := FKStore.Records[0];
    FEOF := False;
  end;
  FBOF := True;
  inherited GotoFirstRecord;
end;

function TppKStorePipeline.TraverseBy(aIncrement: Integer): Integer;
var
  LRecNo: integer;
begin
  if (FSingleRecord = nil) and (FKStore <> nil) and (FKStore.RecordCount > 0) and (FKRecord <> nil) then
  begin
    LRecNo := FKRecord.Index + aIncrement;
    if (LRecNo >= 0) and (LRecNo < FKStore.RecordCount) then
    begin
      Result := aIncrement;
      FKRecord := FKStore.Records[LRecNo];
      FBOF := False;
      FEOF := False;
    end
    else if (LRecNo < 0) then
    begin
      FEOF := False;
      FBOF := True;
      Result := 0;
    end
    else
    begin
      FEOF := True;
      FBOF := False;
      Result := 0;
    end;
  end
  else
  begin
    Result := 0;
    if aIncrement > 0 then
    begin
      FBOF := False;
      FEOF := True;
    end
    else if aIncrement < 0 then
    begin
      FEOF := False;
      FBOF := True;
    end;
  end;
end;

procedure TppKStorePipeline.GotoLastRecord;
begin
  if FSingleRecord <> nil then
  begin
    FKRecord := FSingleRecord;
    FBOF := False;
  end
  else if (FKStore <> nil) and (FKStore.RecordCount > 0) then
  begin
    FKRecord := FKStore.Records[FKStore.RecordCount-1];
    FBOF := False;
  end;
  FEOF := True;
  inherited GotoLastRecord;
end;

function ppConvertFieldType(aFieldType: TEFDataType): TppDataType;
begin
  if aFieldType is TEFStringDataType then Result := dtString
  else if aFieldType is TEFDateDataType then Result := dtDate
  else if aFieldType is TEFTimeDataType then Result := dtTime
  else if aFieldType is TEFDateTimeDataType then Result := dtDateTime
  else if aFieldType is TEFIntegerDataType then Result := dtInteger
  else if aFieldType is TEFCurrencyDataType then Result := dtCurrency
  else if aFieldType is TEFFloatDataType then Result := dtDouble
  else if aFieldType is TEFDecimalDataType then Result := dtDouble
  else if aFieldType is TEFBooleanDataType then Result := dtBoolean
  else if aFieldType is TEFMemoDataType then Result := dtMemo
  else if aFieldType is TEFBlobDataType then Result := dtBLOB
  else Result := dtString;
end;

{ TraTppKStorePipelineRTTI }

class function TraTppKStorePipelineRTTI.RefClass: TClass;
begin
  Result := TppKStorePipeline;
end;

class function TraTppKStorePipelineRTTI.GetPropRec(aClass: TClass; const aPropName: String; var aPropRec: TraPropRec): Boolean;
begin
  Result := True;
  if (CompareText(aPropName, 'MoveBy') = 0) then
    PropToRec(aPropName, daInteger, False, aPropRec)
  else if (CompareText(aPropName, 'RangeEnd') = 0) then
    EnumPropToRec(aPropName, 'TppRangeEndType', False, aPropRec)
  else if (CompareText(aPropName, 'RangeEndCount') = 0) then
    PropToRec(aPropName, daInteger, False, aPropRec)
  else if (CompareText(aPropName, 'RangeBegin') = 0) then
    EnumPropToRec(aPropName, 'TppRangeBeginType', False, aPropRec)
  else
    Result := inherited GetPropRec(aClass, aPropName, aPropRec);
end;

class function TraTppKStorePipelineRTTI.GetPropValue(aObject: TObject; const aPropName: String; var aValue): Boolean;
begin
  Result := True;
  if (CompareText(aPropName, 'MoveBy') = 0) then
    Integer(aValue) := TppKStorePipeline(aObject).MoveBy
  else if (CompareText(aPropName, 'RangeBegin') = 0) then
    Integer(aValue) := Ord(TppKStorePipeline(aObject).RangeBegin)
  else if (CompareText(aPropName, 'RangeEndCount') = 0) then
    Integer(aValue) := TppKStorePipeline(aObject).RangeEndCount
  else if (CompareText(aPropName, 'RangeEnd') = 0) then
    Integer(aValue) := Ord(TppKStorePipeline(aObject).RangeEnd)
  else
    Result := inherited GetPropValue(aObject, aPropName, aValue);
end;

class function TraTppKStorePipelineRTTI.SetPropValue(aObject: TObject; const aPropName: String; var aValue): Boolean;
begin
  Result := True;
  if (CompareText(aPropName, 'MoveBy') = 0) then
    TppKStorePipeline(aObject).MoveBy := Integer(aValue)
  else if (CompareText(aPropName, 'RangeBegin') = 0) then
    TppKStorePipeline(aObject).RangeBegin := TppRangeBeginType(aValue)
  else if (CompareText(aPropName, 'RangeEndCount') = 0) then
    TppKStorePipeline(aObject).RangeEndCount := Integer(aValue)
  else if (CompareText(aPropName, 'RangeEnd') = 0) then
    TppKStorePipeline(aObject).RangeEnd := TppRangeEndType(aValue)
  else
    Result := inherited SetPropValue(aObject, aPropName, aValue);
end;

initialization
  RegisterClass(TppKStorePipeline);
  raRegisterRTTI(TraTppKStorePipelineRTTI);

finalization
  UnRegisterClass(TppKStorePipeline);
  raUnRegisterRTTI(TraTppKStorePipelineRTTI);

end.
