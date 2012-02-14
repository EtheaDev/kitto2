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
  EF.DB;

type
  ///	<summary>
  ///	  Retrieves metadata from a database through DBX. Currently, only
  ///	  Firebird is supported.
  ///	</summary>
  TEFDBDBXInfo = class(TEFDBInfo)
  private
    FConnection: TSQLConnection;
  protected
    procedure BeforeFetchInfo; override;
    procedure FetchTables(const ASchema: TEFDBSchemaInfo); override;
    procedure FetchTableColumns(const ATable: TEFDBTableInfo);
    procedure FetchTableForeignKeys(const ATable: TEFDBTableInfo);
    procedure FetchTablePrimaryKey(const ATable: TEFDBTableInfo);
    procedure GetIndexSegments(const AIndexName: string; const AList: TStrings);
  public
    property Connection: TSQLConnection read FConnection write FConnection;
  end;

  TEFDBDBXQuery = class;

  TEFSQLConnection = class(TSQLConnection)
  end;

  TEFDBDBXConnection = class(TEFDBConnection)
  private
    FConnection: TSQLConnection;
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
    function InternalCreateDBInfo: TEFDBInfo; override;
  end;

implementation

uses
  SysUtils, StrUtils,
  EF.StrUtils, EF.Localization, EF.Types, EF.Tree;

function FbDataTypeToEFDataType(const AIBFbDataType: string): string;
begin
  { TODO : Only text blobs supported for now. }
  if (AIBFbDataType = 'TEXT') or (AIBFbDataType = 'VARYING') or (AIBFbDataType = 'BLOB') then
    Result := 'String'
  else if (AIBFbDataType = 'SHORT') or (AIBFbDataType = 'LONG')
      or (AIBFbDataType = 'INT64') or (AIBFbDataType = 'QUAD') then
    Result := 'Integer'
  else if (AIBFbDataType = 'DATE') then
    Result := 'Date'
  else if (AIBFbDataType = 'TIME') then
    Result := 'Time'
  else if (AIBFbDataType = 'TIMESTAMP') then
    Result := 'DateTime'
  else if (AIBFbDataType = 'FLOAT') or (AIBFbDataType = 'DOUBLE') then
    Result := 'Float'
  else
    Result := 'String';
end;

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
  FreeAndNil(FConnection);
  FreeAndNIl(FConnectionString);
  inherited;
end;

procedure TEFDBDBXConnection.InternalClose;
begin
  if FConnection.Connected then
    FConnection.Close;
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
    // Explicitly setting DriverName and ConnectionName should enable DBX's
    // re-reading of the params. Only setting those does not suffice.
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

function TEFDBDBXAdapter.InternalCreateDBInfo: TEFDBInfo;
begin
  Result := TEFDBDBXInfo.Create;
end;

{ TEFDBDBXInfo }

procedure TEFDBDBXInfo.BeforeFetchInfo;
begin
  inherited;
  Assert(Assigned(FConnection));
end;

procedure TEFDBDBXInfo.FetchTables(const ASchema: TEFDBSchemaInfo);
var
  LTableQuery: TSQLQuery;
  LTable: TEFDBTableInfo;
begin
  LTableQuery := TSQLQuery.Create(nil);
  try
    LTableQuery.SQLConnection := FConnection;
    LTableQuery.SQL.Add('select RDB$RELATION_NAME TABLE_NAME');
    LTableQuery.SQL.Add('from RDB$RELATIONS');
    LTableQuery.SQL.Add('where RDB$SYSTEM_FLAG = 0 or RDB$SYSTEM_FLAG is null');
    LTableQuery.SQL.Add('and RDB$VIEW_BLR is null');
    LTableQuery.SQL.Add('order by RDB$RELATION_NAME');
    LTableQuery.Open;
    while not LTableQuery.Eof do
    begin
      LTable := TEFDBTableInfo.Create;
      try
        LTable.Name := Trim(LTableQuery.FieldByName('TABLE_NAME').AsString);
        FetchTableColumns(LTable);
        FetchTablePrimaryKey(LTable);
        FetchTableForeignKeys(LTable);
        ASchema.AddTable(LTable);
      except
        FreeAndNil(LTable);
      end;
      LTableQuery.Next;
    end;
  finally
    LTableQuery.Free;
  end;
end;

procedure TEFDBDBXInfo.FetchTableColumns(const ATable: TEFDBTableInfo);
var
  LColumnQuery: TSQLQuery;
  LColumn: TEFDBColumnInfo;
begin
  LColumnQuery := TSQLQuery.Create(nil);
  try
    LColumnQuery.SQLConnection := FConnection;
    LColumnQuery.SQL.Add('select RF.RDB$FIELD_NAME COLUMN_NAME, T.RDB$TYPE_NAME DATA_TYPE,');
    LColumnQuery.SQL.Add('  F.RDB$CHARACTER_LENGTH CHARACTER_MAXIMUM_LENGTH,');
    LColumnQuery.SQL.Add('  coalesce(RF.RDB$NULL_FLAG, 0) IS_NOT_NULL');
    LColumnQuery.SQL.Add('from RDB$RELATION_FIELDS RF');
    LColumnQuery.SQL.Add('join RDB$FIELDS F on RF.RDB$FIELD_SOURCE = F.RDB$FIELD_NAME');
    LColumnQuery.SQL.Add('join RDB$TYPES T on F.RDB$FIELD_TYPE = T.RDB$TYPE and T.RDB$FIELD_NAME = ''RDB$FIELD_TYPE''');
    LColumnQuery.SQL.Add('where RF.RDB$RELATION_NAME = ' + QuotedStr(ATable.Name));
    LColumnQuery.SQL.Add('order by RF.RDB$FIELD_POSITION');
    LColumnQuery.Open;
    while not LColumnQuery.Eof do
    begin
      LColumn := TEFDBColumnInfo.Create;
      try
        LColumn.Name := Trim(LColumnQuery.FieldByName('COLUMN_NAME').AsString);
        LColumn.DataType := TEFDataTypeFactory.Instance.GetDataType(
          FbDataTypeToEFDataType(Trim(LColumnQuery.FieldByName('DATA_TYPE').AsString)));
        LColumn.Size := LColumnQuery.FieldByName('CHARACTER_MAXIMUM_LENGTH').AsInteger;
        LColumn.IsRequired := LColumnQuery.FieldByName('IS_NOT_NULL').AsInteger = 1;
        ATable.AddColumn(LColumn);
      except
        FreeAndNil(LColumn);
      end;
      LColumnQuery.Next;
    end;
  finally
    LColumnQuery.Free;
  end;
end;

procedure TEFDBDBXInfo.FetchTablePrimaryKey(const ATable: TEFDBTableInfo);
var
  LPrimaryKeyQuery: TSQLQuery;
begin
  LPrimaryKeyQuery := TSQLQuery.Create(nil);
  try
    LPrimaryKeyQuery.SQLConnection := FConnection;
    LPrimaryKeyQuery.SQL.Add('select RC.RDB$CONSTRAINT_NAME PK_NAME, S.RDB$FIELD_NAME COLUMN_NAME');
    LPrimaryKeyQuery.SQL.Add('from RDB$RELATION_CONSTRAINTS RC');
    LPrimaryKeyQuery.SQL.Add('join RDB$INDICES I on RC.RDB$INDEX_NAME = I.RDB$INDEX_NAME');
    LPrimaryKeyQuery.SQL.Add('join RDB$INDEX_SEGMENTS S on I.RDB$INDEX_NAME = S.RDB$INDEX_NAME');
    LPrimaryKeyQuery.SQL.Add('where RC.RDB$RELATION_NAME = ' + QuotedStr(ATable.Name));
    LPrimaryKeyQuery.SQL.Add('and RC.RDB$CONSTRAINT_TYPE = ''PRIMARY KEY''');
    LPrimaryKeyQuery.SQL.Add('order by S.RDB$FIELD_POSITION');
    LPrimaryKeyQuery.Open;
    while not LPrimaryKeyQuery.Eof do
    begin
      if ATable.PrimaryKey.Name = '' then
        ATable.PrimaryKey.Name := Trim(LPrimaryKeyQuery.FieldByName('PK_NAME').AsString)
      else if ATable.PrimaryKey.Name <> Trim(LPrimaryKeyQuery.FieldByName('PK_NAME').AsString) then
        raise EEFError.Create('Error fetching primary key data for table ' + ATable.Name);
      ATable.PrimaryKey.ColumnNames.Add(Trim(LPrimaryKeyQuery.FieldByName('COLUMN_NAME').AsString));
      LPrimaryKeyQuery.Next;
    end;
  finally
    LPrimaryKeyQuery.Free;
  end;
end;

procedure TEFDBDBXInfo.GetIndexSegments(const AIndexName: string;
  const AList: TStrings);
var
  LSegments: TSQLQuery;
begin
  Assert(Assigned(AList));

  LSegments := TSQLQuery.Create(nil);
  try
    LSegments.SQLConnection := FConnection;
    LSegments.SQL.Add('select S.RDB$FIELD_NAME FIELD_NAME');
    LSegments.SQL.Add('from RDB$INDEX_SEGMENTS S');
    LSegments.SQL.Add('where S.RDB$INDEX_NAME = ' + QuotedStr(AIndexName));
    LSegments.SQL.Add('order by S.RDB$FIELD_POSITION');
    LSegments.Open;
    try
      while not LSegments.Eof do
      begin
        AList.Add(Trim(LSegments.FieldByName('FIELD_NAME').AsString));
        LSegments.Next;
      end;
    finally
      LSegments.Close;
    end;
  finally
    FreeAndNil(LSegments);
  end;
end;

procedure TEFDBDBXInfo.FetchTableForeignKeys(const ATable: TEFDBTableInfo);
var
  LConstraints: TSQLQuery;
  LForeignKey: TEFDBForeignKeyInfo;
begin
  LConstraints := TSQLQuery.Create(nil);
  try
    LConstraints.SQLConnection := FConnection;
    LConstraints.SQL.Add('select RC.RDB$CONSTRAINT_NAME FK_NAME, RC2.RDB$RELATION_NAME PK_TABLE_NAME,');
    LConstraints.SQL.Add('  RC.RDB$INDEX_NAME INDEX_NAME, RC2.RDB$INDEX_NAME FOREIGN_INDEX_NAME');
    LConstraints.SQL.Add('from RDB$RELATION_CONSTRAINTS RC');
    LConstraints.SQL.Add('join RDB$REF_CONSTRAINTS REFC on RC.RDB$CONSTRAINT_NAME = REFC.RDB$CONSTRAINT_NAME');
    LConstraints.SQL.Add('join RDB$RELATION_CONSTRAINTS RC2 on REFC.RDB$CONST_NAME_UQ = RC2.RDB$CONSTRAINT_NAME');
    LConstraints.SQL.Add('where RC.RDB$RELATION_NAME = ' + QuotedStr(ATable.Name));
    LConstraints.SQL.Add('and RC.RDB$CONSTRAINT_TYPE = ''FOREIGN KEY''');
    LConstraints.SQL.Add('order by RC.RDB$CONSTRAINT_NAME');
    LConstraints.Open;
    try
      while not LConstraints.Eof do
      begin
        LForeignKey := TEFDBForeignKeyInfo.Create;
        try
          LForeignKey.Name := Trim(LConstraints.FieldByName('FK_NAME').AsString);
          LForeignKey.ForeignTableName := Trim(LConstraints.FieldByName('PK_TABLE_NAME').AsString);
          GetIndexSegments(Trim(LConstraints.FieldByName('INDEX_NAME').AsString), LForeignKey.ColumnNames);
          GetIndexSegments(Trim(LConstraints.FieldByName('FOREIGN_INDEX_NAME').AsString), LForeignKey.ForeignColumnNames);
          ATable.AddForeignKey(LForeignKey);
        except
          FreeAndNil(LForeignKey);
          raise;
        end;
        LConstraints.Next;
      end;
    finally
      LConstraints.Close;
    end;
  finally
    LConstraints.Free;
  end;
end;

initialization
  TEFDBAdapterRegistry.Instance.RegisterDBAdapter('DBX', TEFDBDBXAdapter.Create);

finalization
  TEFDBAdapterRegistry.Instance.UnregisterDBAdapter('DBX');

end.
