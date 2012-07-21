{-------------------------------------------------------------------------------
   Copyright 2012 Ethea S.r.l.

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
///	  DBExpress-based database access layer.
///	</summary>
unit EF.DB.DBX;

{$I EF.Defines.inc}

interface

uses
  Classes, DB, Contnrs,
  DBXCommon, SqlExpr,
  EF.Tree, EF.DB;

type
  ///	<summary>
  ///	  Retrieves metadata from a database through DBX. Currently, only
  ///	  Firebird is supported.
  ///	</summary>
  TEFDBDBXInfo = class(TEFDBInfo)
  private
    FConnection: TSQLConnection;
    FServerCharSet: string;
    function DBXDataTypeToEFDataType(const ADBXDataType: TDBXType): TEFDataType;
    procedure SetForeignKeyColumns(const AForeignKeyInfo: TEFDBForeignKeyInfo);
    procedure StoreServerCharSet;
    procedure RestoreServerCharSet;
  protected
    procedure BeforeFetchInfo; override;
    procedure FetchTables(const ASchemaInfo: TEFDBSchemaInfo); override;
    procedure FetchTableColumns(const ATableInfo: TEFDBTableInfo);
    procedure FetchTablePrimaryKey(const ATableInfo: TEFDBTableInfo);
    procedure FetchTableForeignKeys(const ATableInfo: TEFDBTableInfo);
  public
    constructor Create(const AConnection: TSQLConnection);
    property Connection: TSQLConnection read FConnection write FConnection;
  end;

  TEFDBDBXQuery = class;

  TEFSQLConnection = class(TSQLConnection)
  end;

  TEFDBDBXConnection = class(TEFDBConnection)
  private
    FConnection: TEFSQLConnection;
    FTransaction: TDBXTransaction;
    FConnectionString: TStrings;
    FFetchSequenceGeneratorValueQuery: TEFDBDBXQuery;
    FLastSequenceName: string;

    ///	<summary>
    ///	  Handles FConnectionString.OnChange; parses the connection string and
    ///	  configures the internal connection accordingly.
    ///	</summary>
    procedure OnConnectionStringChange(Sender: TObject);

    ///	<summary>
    ///	  Returns the SQL statement used to get the next value from a sequence
    ///	  generator. Currently supports only Oracle and IB/Fb.
    ///	</summary>
    function GetFetchSequenceGeneratorValueSQL(
      const ASequenceName: string): string;

    ///	<summary>
    ///	  Returns a query to be used to fetch a sequence generator value. The
    ///	  query is created upon first access and kept prepared until the
    ///	  connection is closed or destroyed.
    ///	</summary>
    function GetFetchSequenceGeneratorValueQuery(const ASequenceName: string): TEFDBDBXQuery;
  protected
    function CreateDBEngineType: TEFDBEngineType; override;
    procedure InternalOpen; override;
    procedure InternalClose; override;
    function InternalCreateDBInfo: TEFDBInfo; override;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
  public
    function IsOpen: Boolean; override;
    function ExecuteImmediate(const AStatement: string): Integer; override;
    procedure StartTransaction; override;
    procedure CommitTransaction; override;
    procedure RollbackTransaction; override;
    function IsInTransaction: Boolean; override;
    function GetLastAutoincValue(const ATableName: string = ''): Int64; override;
    function FetchSequenceGeneratorValue(const ASequenceName: string): Int64; override;
    function CreateDBCommand: TEFDBCommand; override;
    function CreateDBQuery: TEFDBQuery; override;
  end;

  ///	<summary>Customized TSQLQuery used inside TEFDBXCommand and
  ///	TEFDBDBXQuery.</summary>
  TEFSQLQuery = class(TSQLQuery)
  private

    ///	<summary>
    ///	  Params of unknown type trigger the DBX error "No value for parameter
    ///	  X". Detail queries of empty master datasets have params with unknown
    ///	  type, so we patch the data type to avoid the error.
    ///	</summary>
    procedure FixUnknownParamTypes;
  protected
    procedure InternalOpen; override;
  public
    function ExecSQL(ExecDirect: Boolean = False): Integer; override;
  end;

  TEFDBDBXCommand = class(TEFDBCommand)
  private
    FQuery: TEFSQLQuery;
    FCommandText: string;
  protected
    procedure ConnectionChanged; override;
    function GetCommandText: string; override;
    procedure SetCommandText(const AValue: string); override;
    function GetPrepared: Boolean; override;
    procedure SetPrepared(const AValue: Boolean); override;
    function GetParams: TParams; override;
    procedure SetParams(const AValue: TParams); override;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
  public
    function Execute: Integer; override;
  end;

  TEFDBDBXQuery = class(TEFDBQuery)
  private
    FQuery: TEFSQLQuery;
    FCommandText: string;
  protected
    procedure ConnectionChanged; override;
    function GetCommandText: string; override;
    procedure SetCommandText(const AValue: string); override;
    function GetPrepared: Boolean; override;
    procedure SetPrepared(const AValue: Boolean); override;
    function GetParams: TParams; override;
    procedure SetParams(const AValue: TParams); override;
    function GetDataSet: TDataSet; override;
    function GetMasterSource: TDataSource; override;
    procedure SetMasterSource(const AValue: TDataSource); override;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
  public
    ///	<summary>Execute and Open are synonims in this class. Execute always
    ///	returns 0.</summary>
    function Execute: Integer; override;
    procedure Open; override;
    procedure Close; override;
    function IsOpen: Boolean; override;
  end;

  TEFDBDBXAdapter = class(TEFDBAdapter)
  protected
    function InternalCreateDBConnection: TEFDBConnection; override;
    class function InternalGetClassId: string; override;
  end;

implementation

uses
  SysUtils, StrUtils, DBXMetaDataNames,
  EF.StrUtils, EF.Localization, EF.Types;

function FetchParam(const AParams: TStrings; const AParamName: string): string;
var
  LParamIndex: Integer;
begin
  LParamIndex := AParams.IndexOfName(AParamName);
  if LParamIndex >= 0 then
  begin
    Result := AParams.ValueFromIndex[LParamIndex];
    AParams.Delete(LParamIndex);
  end
  else
    Result := '';
end;

{ TEFDBDBXConnection }

procedure TEFDBDBXConnection.AfterConstruction;
begin
  inherited;
  FConnection := TEFSQLConnection.Create(nil);
  FConnection.LoginPrompt := False;
  FConnection.AfterConnect := AfterConnectionOpen;
  FConnectionString := TStringList.Create;
  TStringList(FConnectionString).OnChange := OnConnectionStringChange;
end;

destructor TEFDBDBXConnection.Destroy;
begin
  FreeAndNil(FFetchSequenceGeneratorValueQuery);
  if Assigned(FConnection) then
    FConnection.Close;
  FreeAndNil(FConnection);
  FreeAndNIl(FConnectionString);
  inherited;
end;

procedure TEFDBDBXConnection.InternalClose;
begin
  if FConnection.Connected then
    FConnection.Close;
end;

function TEFDBDBXConnection.InternalCreateDBInfo: TEFDBInfo;
begin
  Result := TEFDBDBXInfo.Create(FConnection);
end;

procedure TEFDBDBXConnection.CommitTransaction;
begin
  if FConnection.InTransaction then 
    FConnection.CommitFreeAndNil(FTransaction);
end;

function TEFDBDBXConnection.CreateDBCommand: TEFDBCommand;
begin
  Result := TEFDBDBXCommand.Create;
  try
    Result.Connection := Self;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TEFDBDBXConnection.CreateDBEngineType: TEFDBEngineType;
begin
  if ContainsText(FConnection.DriverName, 'MSSQL') or ContainsText(FConnection.DriverName, 'SQLServer') then
    Result := TEFSQLServerDBEngineType.Create
  else if ContainsText(FConnection.DriverName, 'Firebird') or ContainsText(FConnection.DriverName, 'InterBase') then
    Result := TEFFirebirdDBEngineType.Create
  else
    Result := inherited CreateDBEngineType;
end;

function TEFDBDBXConnection.CreateDBQuery: TEFDBQuery;
begin
  Result := TEFDBDBXQuery.Create;
  try
    Result.Connection := Self;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TEFDBDBXConnection.ExecuteImmediate(const AStatement: string): Integer;
begin
  Assert(Assigned(FConnection));

  if AStatement = '' then
    raise EEFError.Create(_('Unspecified Statement text.'));

  Result := FConnection.ExecuteDirect(AStatement);
end;

function TEFDBDBXConnection.GetFetchSequenceGeneratorValueQuery(
  const ASequenceName: string): TEFDBDBXQuery;
begin
  { TODO :
This remembers the last used sequence fetch query.
A better implementation would be to keep a list of them. }
  if (ASequenceName <> FLastSequenceName) and (FLastSequenceName <> '') then
  begin
    FreeAndNil(FFetchSequenceGeneratorValueQuery);
    FLastSequenceName := '';
  end;

  if not Assigned(FFetchSequenceGeneratorValueQuery) then
  begin
    FFetchSequenceGeneratorValueQuery := TEFDBDBXQuery.Create;
    try
      FFetchSequenceGeneratorValueQuery.Connection := Self;
      FFetchSequenceGeneratorValueQuery.CommandText :=
        GetFetchSequenceGeneratorValueSQL(ASequenceName);
      FFetchSequenceGeneratorValueQuery.Prepared := True;
      FLastSequenceName := ASequenceName;
    except
      FreeAndNil(FFetchSequenceGeneratorValueQuery);
      raise;
    end;
  end;
  // Re-prepare the query in case it was unprepared (for example as a
  // consequence of closing the connection).
  if not FFetchSequenceGeneratorValueQuery.Prepared then
    FFetchSequenceGeneratorValueQuery.Prepared := True;
  Result := FFetchSequenceGeneratorValueQuery;
end;

function TEFDBDBXConnection.FetchSequenceGeneratorValue(
  const ASequenceName: string): Int64;
var
  LQuery: TEFDBDBXQuery;
begin
  if ASequenceName = '' then
    raise EEFError.Create(_('Unspecified Sequence name.'));
  LQuery := GetFetchSequenceGeneratorValueQuery(ASequenceName);
  LQuery.Open;
  try
    Result := StrToInt64(LQuery.DataSet.Fields[0].AsString);
  finally
    LQuery.Close;
  end;
end;

function TEFDBDBXConnection.GetFetchSequenceGeneratorValueSQL(
  const ASequenceName: string): string;
begin
  if Pos('ORACLE', UpperCase(FConnection.DriverName)) > 0 then
    Result := 'select ' + ASequenceName + '.NEXTVAL from DUAL'
  else
    Result := 'select GEN_ID(' + ASequenceName + ', 1) from RDB$DATABASE';
end;

function TEFDBDBXConnection.GetLastAutoincValue(
  const ATableName: string = ''): Int64;
begin
  // Auto-inc fields currently not supported in dbExpress.
  Result := 0;
end;

function TEFDBDBXConnection.IsInTransaction: Boolean;
begin
  Result := FConnection.InTransaction; 
end;

function TEFDBDBXConnection.IsOpen: Boolean;
begin
  if FConnection = nil then
    Result := False
  else
    Result := FConnection.Connected;
end;

procedure TEFDBDBXConnection.OnConnectionStringChange(Sender: TObject);
var
  LConnectionName: string;
  LParams: TStrings;
begin
  inherited;
  LConnectionName := FConnectionString.Values['ConnectionName'];
  if LConnectionName <> '' then
  begin
    FConnection.ConnectionName := LConnectionName;
    FConnection.LoadParamsOnConnect := True;
  end
  else
  begin
    FConnection.LoadParamsOnConnect := False;
    // These properties need to be provided explicitly.
    // Note: setting DriverName clears Params, so we need to assign them later.
    LParams := TStringList.Create;
    try
      LParams.Assign(FConnectionString);
      // Drop the useless param.
      FetchParam(LParams, 'ConnectionName');
      FConnection.DriverName := FetchParam(LParams, 'DriverName');
      FConnection.LibraryName := FetchParam(LParams, 'LibraryName');
      FConnection.VendorLib := FetchParam(LParams, 'VendorLib');
      FConnection.GetDriverFunc := FetchParam(LParams, 'GetDriverFunc');
      FConnection.Params.Assign(LParams);
    finally
      LParams.Free;
    end;
  end;
end;

procedure TEFDBDBXConnection.InternalOpen;
begin
  if not FConnection.Connected then
  begin
    // Explicitly setting DriverName and ConnectionName should force DBX to
    // re-read the params. Just setting Params is not enough.
    FConnection.ConnectionName := Config.GetExpandedString('Connection/ConnectionName');
    FConnection.DriverName := Config.GetExpandedString('Connection/DriverName');
    FConnection.Params.Text := Config.GetChildrenAsExpandedStrings('Connection');
    FConnection.Open;
  end;
end;

procedure TEFDBDBXConnection.RollbackTransaction;
begin
  if FConnection.InTransaction then
    FConnection.RollbackFreeAndNil(FTransaction);
end;

procedure TEFDBDBXConnection.StartTransaction;
begin
  if not IsOpen then
    Open;
  if not FConnection.InTransaction then
    FTransaction := FConnection.BeginTransaction;
end;

{ TEFSQLQuery }

procedure TEFSQLQuery.FixUnknownParamTypes;
var
  LParamIndex: Integer;
begin
  for LParamIndex := 0 to ParamCount - 1 do
    if Params[LParamIndex].DataType = ftUnknown then
      Params[LParamIndex].DataType := ftInteger;
  inherited;
end;

function TEFSQLQuery.ExecSQL(ExecDirect: Boolean): Integer;
begin
  FixUnknownParamTypes;
  Result := inherited ExecSQL(ExecDirect);
end;

procedure TEFSQLQuery.InternalOpen;
begin
  FixUnknownParamTypes;
  inherited;
end;

{ TEFDBDBXCommand }

procedure TEFDBDBXCommand.AfterConstruction;
begin
  inherited;
  FQuery := TEFSQLQuery.Create(nil);
end;

destructor TEFDBDBXCommand.Destroy;
begin
  FreeAndNil(FQuery);
  inherited;
end;

procedure TEFDBDBXCommand.ConnectionChanged;
begin
  inherited;
  FQuery.SQLConnection := (Connection.AsObject as TEFDBDBXConnection).FConnection;
end;

function TEFDBDBXCommand.Execute: Integer;
begin
  inherited;
  Connection.DBEngineType.BeforeExecute(FQuery.SQL.Text, FQuery.Params);
  FQuery.ExecSQL;
  Result := FQuery.RowsAffected;
end;

function TEFDBDBXCommand.GetCommandText: string;
begin
  Result := FCommandText;
end;

function TEFDBDBXCommand.GetParams: TParams;
begin
  Result := FQuery.Params;
end;

function TEFDBDBXCommand.GetPrepared: Boolean;
begin
  Result := FQuery.Prepared;
end;

procedure TEFDBDBXCommand.SetCommandText(const AValue: string);
begin
  FCommandText := AValue;
  FQuery.SQL.Text := ExpandCommandText(FCommandText);
end;

procedure TEFDBDBXCommand.SetParams(const AValue: TParams);
begin
  FQuery.Params.Assign(AValue);
end;

procedure TEFDBDBXCommand.SetPrepared(const AValue: Boolean);
begin
  FQuery.Prepared := AValue;
end;

{ TEFDBDBXQuery }

procedure TEFDBDBXQuery.AfterConstruction;
begin
  inherited;
  FQuery := TEFSQLQuery.Create(nil);
end;

destructor TEFDBDBXQuery.Destroy;
begin
  FreeAndNil(FQuery);
  inherited;
end;

procedure TEFDBDBXQuery.Close;
begin
  FQuery.Close;
end;

procedure TEFDBDBXQuery.ConnectionChanged;
begin
  inherited;
  FQuery.SQLConnection := (Connection.AsObject as TEFDBDBXConnection).FConnection;
end;

function TEFDBDBXQuery.Execute: Integer;
begin
  inherited;
  Open;
  Result := 0;
end;

function TEFDBDBXQuery.GetCommandText: string;
begin
  Result := FCommandText;
end;

function TEFDBDBXQuery.GetDataSet: TDataSet;
begin
  Result := FQuery;
end;

function TEFDBDBXQuery.GetMasterSource: TDataSource;
begin
  Result := FQuery.DataSource;
end;

function TEFDBDBXQuery.GetParams: TParams;
begin
  Result := FQuery.Params;
end;

function TEFDBDBXQuery.GetPrepared: Boolean;
begin
  Result := FQuery.Prepared;
end;

function TEFDBDBXQuery.IsOpen: Boolean;
begin
  Result := FQuery.Active;
end;

procedure TEFDBDBXQuery.Open;
begin
  Connection.Open;
  Connection.DBEngineType.BeforeExecute(FQuery.SQL.Text, FQuery.Params);
  DataSet.Open;
end;

procedure TEFDBDBXQuery.SetCommandText(const AValue: string);
begin
  FCommandText := AValue;
  FQuery.SQL.Text := ExpandCommandText(FCommandText);
end;

procedure TEFDBDBXQuery.SetMasterSource(const AValue: TDataSource);
begin
  FQuery.DataSource := AValue;
end;

procedure TEFDBDBXQuery.SetParams(const AValue: TParams);
begin
  FQuery.Params.Assign(AValue);
end;

procedure TEFDBDBXQuery.SetPrepared(const AValue: Boolean);
begin
  if AValue then
    FQuery.Prepared := True;
end;

{ TEFDBDBXAdapter }

function TEFDBDBXAdapter.InternalCreateDBConnection: TEFDBConnection;
begin
  Result := TEFDBDBXConnection.Create;
end;

class function TEFDBDBXAdapter.InternalGetClassId: string;
begin
  Result := 'DBX';
end;

{ TEFDBDBXInfo }

procedure TEFDBDBXInfo.BeforeFetchInfo;
begin
  inherited;
  Assert(Assigned(FConnection));
end;

procedure TEFDBDBXInfo.FetchTables(const ASchemaInfo: TEFDBSchemaInfo);
var
  LCommand: TDBXCommand;
  LReader: TDBXReader;
  LTableInfo: TEFDBTableInfo;
  LCommandText: string;
begin
  StoreServerCharSet;
  try
    FConnection.Open;
    try
      LCommand := FConnection.DBXConnection.CreateCommand;
      try
        LCommand.CommandType := TDBXCommandTypes.DbxMetaData;
        LCommandText := TDBXMetaDataCommands.GetTables + ' % ' + TDBXMetaDataTableTypes.Table;
        if ViewsAsTables then
          LCommandText := LCommandText + ' ' + TDBXMetaDataTableTypes.View;
        LCommand.Text := LCommandText;
        LReader := LCommand.ExecuteQuery;
        try
          while LReader.Next do
          begin
            LTableInfo := TEFDBTableInfo.Create;
            try
              LTableInfo.Name := LReader.Value[TDBXTablesIndex.TableName].AsString;
              FetchTableColumns(LTableInfo);
              if SameText(LReader.Value[TDBXTablesIndex.TableType].AsString, 'TABLE') then
              begin
                FetchTablePrimaryKey(LTableInfo);
                FetchTableForeignKeys(LTableInfo);
              end;
              ASchemaInfo.AddTable(LTableInfo);
            except
              FreeAndNil(LTableInfo);
              raise;
            end;
          end;
        finally
          FreeAndNil(LReader);
        end;
      finally
        FreeAndNil(LCommand);
      end;
    finally
      FConnection.Close;
    end;
  finally
    RestoreServerCharSet;
  end;
end;

procedure TEFDBDBXInfo.StoreServerCharSet;
begin
  Assert(Assigned(FConnection));

  // Workaround for Firebird driver bug
  // http://qc.embarcadero.com/wc/qcmain.aspx?d=90414
  FServerCharSet := FConnection.Params.Values['ServerCharSet'];
  if SameText(FConnection.DriverName, 'Firebird') then
  begin
    FConnection.Params.Values['ServerCharSet'] := 'NONE';
    FConnection.Close;
  end;
end;

procedure TEFDBDBXInfo.RestoreServerCharSet;
begin
  Assert(Assigned(FConnection));

  // Workaround for Firebird driver bug
  // http://qc.embarcadero.com/wc/qcmain.aspx?d=90414
  if SameText(FConnection.DriverName, 'Firebird') then
    FConnection.Params.Values['ServerCharSet'] := FServerCharSet;
end;

constructor TEFDBDBXInfo.Create(const AConnection: TSQLConnection);
begin
  inherited Create;
  FConnection := AConnection;
end;

function TEFDBDBXInfo.DBXDataTypeToEFDataType(const ADBXDataType: TDBXType): TEFDataType;
var
  LClass: TEFDataTypeClass;
begin
  with TDBXDataTypes do
  begin
    case ADBXDataType of
      DateType: LClass := TEFDateDataType;
      TimeType: LClass := TEFTimeDataType;
      DateTimeType, TimeStampType: LClass := TEFDateTimeDataType;
      BlobType, BytesType, VarBytesType,
        BinaryBlobType: LClass := TEFBlobDataType;
      BooleanType: LClass := TEFBooleanDataType;
      Int16Type, Int32Type, UInt16Type,
        UInt32Type, Int8Type, UInt8Type: LClass := TEFIntegerDataType;
      DoubleType, SingleType: LClass := TEFFloatDataType;
      BcdType: LClass := TEFDecimalDataType;
      CurrencyType: LClass := TEFCurrencyDataType;
    else
      LClass := TEFStringDataType;
    end;
  end;
  Result := TEFDataTypeFactory.Instance.GetDataType(LClass);
end;

procedure TEFDBDBXInfo.FetchTableColumns(const ATableInfo: TEFDBTableInfo);
var
  LCommand: TDBXCommand;
  LReader: TDBXReader;
  LColumnInfo: TEFDBColumnInfo;

  function PatchSize(const ASize: Integer): Integer;
  begin
    if LColumnInfo.DataType.HasSize then
      Result := ASize
    else
      Result := 0;
  end;

  function PatchScale(const AScale: Integer): Integer;
  begin
    if LColumnInfo.DataType.HasScale then
      Result := AScale
    else
      Result := 0;
  end;

begin
  LCommand := FConnection.DBXConnection.CreateCommand;
  try
    LCommand.CommandType := TDBXCommandTypes.DbxMetaData;
    LCommand.Text := TDBXMetaDataCommands.GetColumns + ' ' + ATableInfo.Name;
    LReader := LCommand.ExecuteQuery;
    try
      while LReader.Next do
      begin
        LColumnInfo := TEFDBColumnInfo.Create;
        try
          LColumnInfo.Name := LReader.Value[TDBXColumnsIndex.ColumnName].AsString;
          LColumnInfo.DataType := DBXDataTypeToEFDataType(LReader.Value[TDBXColumnsIndex.DbxDataType].AsInt32);
          LColumnInfo.Size := PatchSize(LReader.Value[TDBXColumnsIndex.Precision].AsInt32);
          LColumnInfo.Scale := PatchScale(LReader.Value[TDBXColumnsIndex.Scale].AsInt32);
          LColumnInfo.IsRequired := not LReader.Value[TDBXColumnsIndex.IsNullable].AsBoolean;
          ATableInfo.AddColumn(LColumnInfo);
        except
          FreeAndNil(LColumnInfo);
        end;
      end;
    finally
      FreeAndNil(LReader);
    end;
  finally
    FreeAndNil(LCommand);
  end;
end;

procedure TEFDBDBXInfo.FetchTablePrimaryKey(const ATableInfo: TEFDBTableInfo);
var
  LCommand: TDBXCommand;
  LReader: TDBXReader;
  LCommand2: TDBXCommand;
  LReader2: TDBXReader;
begin
  LCommand := FConnection.DBXConnection.CreateCommand;
  try
    LCommand.CommandType := TDBXCommandTypes.DbxMetaData;
    LCommand.Text := TDBXMetaDataCommands.GetIndexes + ' ' + ATableInfo.Name;
    LReader := LCommand.ExecuteQuery;
    try
      while LReader.Next do
      begin
        if LReader.Value[TDBXIndexesIndex.IsPrimary].AsBoolean then
        begin
          ATableInfo.PrimaryKey.Name := LReader.Value[TDBXIndexesIndex.ConstraintName].AsString;
          ATableInfo.PrimaryKey.ColumnNames.Clear;
          LCommand2 := FConnection.DBXConnection.CreateCommand;
          try
            LCommand2.CommandType := TDBXCommandTypes.DbxMetaData;
            LCommand2.Text := TDBXMetaDataCommands.GetIndexColumns + ' ' +
              ATableInfo.Name + ' ' + LReader.Value[TDBXIndexesIndex.IndexName].AsString;
            LReader2 := LCommand2.ExecuteQuery;
            try
              while LReader2.Next do
                ATableInfo.PrimaryKey.ColumnNames.Add(
                  LReader2.Value[TDBXIndexColumnsIndex.ColumnName].AsString);
            finally
              FreeAndNil(LReader2);
            end;
          finally
            FreeAndNil(LCommand2);
          end;
        end;
      end;
    finally
      FreeAndNil(LReader);
    end;
  finally
    FreeAndNil(LCommand);
  end;
end;

procedure TEFDBDBXInfo.FetchTableForeignKeys(const ATableInfo: TEFDBTableInfo);
var
  LCommand: TDBXCommand;
  LReader: TDBXReader;
  LForeignKeyInfo: TEFDBForeignKeyInfo;
begin
  LCommand := FConnection.DBXConnection.CreateCommand;
  try
    LCommand.CommandType := TDBXCommandTypes.DbxMetaData;
    LCommand.Text := TDBXMetaDataCommands.GetForeignKeys + ' ' + ATableInfo.Name;
    LReader := LCommand.ExecuteQuery;
    try
      while LReader.Next do
      begin
        LForeignKeyInfo := TEFDBForeignKeyInfo.Create;
        try
          LForeignKeyInfo.Name := LReader.Value[TDBXForeignKeysIndex.ForeignKeyName].AsString;
          ATableInfo.AddForeignKey(LForeignKeyInfo);
          SetForeignKeyColumns(LForeignKeyInfo);
        except
          FreeAndNil(LForeignKeyInfo);
          raise;
        end;
      end;
    finally
      FreeAndNil(LReader);
    end;
  finally
    FreeAndNil(LCommand);
  end;
end;

procedure TEFDBDBXInfo.SetForeignKeyColumns(const AForeignKeyInfo: TEFDBForeignKeyInfo);
var
  LCommand: TDBXCommand;
  LReader: TDBXReader;
begin
  LCommand := FConnection.DBXConnection.CreateCommand;
  try
    LCommand.CommandType := TDBXCommandTypes.DbxMetaData;
    LCommand.Text := TDBXMetaDataCommands.GetForeignKeyColumns + ' '
      + AForeignKeyInfo.TableInfo.Name + ' ' + AForeignKeyInfo.Name;
    LReader := LCommand.ExecuteQuery;
    try
      while LReader.Next do
      begin
        if AForeignKeyInfo.ForeignTableName = '' then
          AForeignKeyInfo.ForeignTableName := LReader.Value[TDBXForeignKeyColumnsIndex.PrimaryTableName].AsString;
        AForeignKeyInfo.ColumnNames.Add(LReader.Value[TDBXForeignKeyColumnsIndex.ColumnName].AsString);
        AForeignKeyInfo.ForeignColumnNames.Add(LReader.Value[TDBXForeignKeyColumnsIndex.PrimaryColumnName].AsString);
      end;
    finally
      FreeAndNil(LReader);
    end;
  finally
    FreeAndNil(LCommand);
  end;
end;

initialization
  TEFDBAdapterRegistry.Instance.RegisterDBAdapter(TEFDBDBXAdapter.GetClassId, TEFDBDBXAdapter.Create);

finalization
  TEFDBAdapterRegistry.Instance.UnregisterDBAdapter(TEFDBDBXAdapter.GetClassId);

end.
