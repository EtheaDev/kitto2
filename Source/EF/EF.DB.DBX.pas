unit EF.DB.DBX;

{$I EF.Defines.inc}

interface

uses
  Classes, DB, Contnrs,
  DBXCommon, SqlExpr,
  EF.Intf, EF.Classes,  EF.DB;

type
  {
    Retrieves metadata from a database through DBX.
    Currently, only Firebird is supported.
  }
  TEFDBDBXInfo = class(TEFDBInfo, IEFDBInfo)
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

  {
    Encapsulates a DBX DB connection.

    Configuration items:
    @table(
      @row(
        @cell(ConnectionString)@cell(string (optional))
        @cell(Sets the value of the ConnectionString property.)
      )
    )
  }
  TEFDBDBXConnection = class(TEFDBConnection, IEFInterface, IEFDBConnection)
  private
    FConnection: TSQLConnection;
    FTransaction: TDBXTransaction;
    FConnectionString: TStrings;
    FFetchSequenceGeneratorValueQuery: TEFDBDBXQuery;
    FLastSequenceName: string;
    {
      Handles FConnectionString.OnChange; parses the connection string
      and configures the internal connection accordingly.
    }
    procedure OnConnectionStringChange(Sender: TObject);
    {
      Returns the SQL statement used to get the next value from a sequence
      generator. Currently supports only Oracle and IB/Fb.
    }
    function GetFetchSequenceGeneratorValueSQL(
      const ASequenceName: string): string;
    {
      Returns a query to be used to fetch a sequence generator value.
      The query is created upon first access and kept prepared until
      the connection is closed or destroyed.
    }
    function GetFetchSequenceGeneratorValueQuery(const ASequenceName: string): TEFDBDBXQuery;
  protected
    // IEFDBConnection
    procedure Open;
    procedure Close;
    function IsOpen: Boolean;
    function ExecuteImmediate(const AStatement: string): Integer;
    procedure StartTransaction;
    procedure CommitTransaction;
    procedure RollbackTransaction;
    function IsInTransaction: Boolean;
    function GetLastAutoincValue(const ATableName: string = ''): Int64;
    function FetchSequenceGeneratorValue(const ASequenceName: string): Int64;
    function CreateDBCommand: IEFDBCommand; override;
    function CreateDBQuery: IEFDBQuery;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
  end;

  {
    Base class for DBX DB components.
  }
  TEFDBDBXComponent = class(TEFComponent, IEFInterface, IEFDBComponent)
  private
    FConnection: IEFDBConnection;
  protected
    {
      Called whenever the connection changes. Descendants override it to
      link internal components to the new connection.
    }
    procedure ConnectionChanged; virtual;
  public
    // IEFDBComponent
    function GetConnection: IEFDBConnection;
    procedure SetConnection(const AValue: IEFDBConnection);
    property Connection: IEFDBConnection
      read GetConnection write SetConnection;
  end;

  {
    Customized TSQLQuery used inside TEFDBXCommand and TEFDBDBXQuery.
  }
  TEFSQLQuery = class(TSQLQuery)
  private
    {
      Params of unknown type trigger the DBX error "No value for parameter X".
      Detail queries of empty master datasets have params with unknown type,
      so we patch the data type to avoid the error.
    }
    procedure FixUnknownParamTypes;
  protected
    procedure InternalOpen; override;
  public
    function ExecSQL(ExecDirect: Boolean = False): Integer; override;
  end;

  {
    Encapsulates a DB command.
  }
  TEFDBDBXCommand = class(TEFDBDBXComponent, IEFInterface,
    IEFDBComponent, IEFDBCommand)
  private
    FQuery: TEFSQLQuery;
  protected
    procedure ConnectionChanged; override;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    // IEFDBCommand
    function GetCommandText: string;
    procedure SetCommandText(const AValue: string);
    property CommandText: string read GetCommandText write SetCommandText;
    function GetPrepared: Boolean;
    procedure SetPrepared(const AValue: Boolean);
    property Prepared: Boolean read GetPrepared write SetPrepared;
    function GetParams: TParams;
    procedure SetParams(const AValue: TParams);
    property Params: TParams read GetParams write SetParams;
    function Execute: Integer;
  end;

  {
    Encapsulates a DB query, that is a DB command with an associated result set.
  }
  TEFDBDBXQuery = class(TEFDBDBXComponent, IEFInterface,
    IEFDBComponent, IEFDBQuery, IEFDBCommand)
  private
    FQuery: TEFSQLQuery;
  protected
    procedure ConnectionChanged; override;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    // IEFDBQuery
    function GetCommandText: string;
    procedure SetCommandText(const AValue: string);
    property CommandText: string read GetCommandText write SetCommandText;
    function GetPrepared: Boolean;
    procedure SetPrepared(const AValue: Boolean);
    property Prepared: Boolean read GetPrepared write SetPrepared;
    function GetParams: TParams;
    procedure SetParams(const AValue: TParams);
    property Params: TParams read GetParams write SetParams;
    {
      Execute and Open are synonims in this class. Execute always returns 0.
    }
    function Execute: Integer;
    procedure Open;
    procedure Close;
    function IsOpen: Boolean;
    function GetDataSet: TDataSet;
    property DataSet: TDataSet read GetDataSet;
    function GetMasterSource: TDataSource;
    procedure SetMasterSource(const AValue: TDataSource);
    property MasterSource: TDataSource read GetMasterSource write SetMasterSource;
  end;

  {
    A factory that creates DBX DB objects.
  }
  TEFDBDBXAdapter = class(TEFDBAdapter)
  protected
    function InternalCreateDBConnection: IEFDBConnection; override;
    function InternalCreateDBInfo: IEFDBInfo; override;
  end;

implementation

uses
  SysUtils, RTLConsts, TypInfo,
  EF.Localization, EF.Types, EF.Data;

function FbDataTypeToEFDataType(const AIBFbDataType: string): TEFDataType;
begin
  { TODO : Only text blobs supported for now. }
  if (AIBFbDataType = 'TEXT') or (AIBFbDataType = 'VARYING') or (AIBFbDataType = 'BLOB') then
    Result := edtString
  else if (AIBFbDataType = 'SHORT') or (AIBFbDataType = 'LONG')
      or (AIBFbDataType = 'INT64') or (AIBFbDataType = 'QUAD') then
    Result := edtInteger
  else if (AIBFbDataType = 'DATE') then
    Result := edtDate
  else if (AIBFbDataType = 'TIME') then
    Result := edtTime
  else if (AIBFbDataType = 'TIMESTAMP') then
    Result := edtDateTime
  else if (AIBFbDataType = 'FLOAT') or (AIBFbDataType = 'DOUBLE') then
    Result := edtFloat
  else
    Result := edtUnknown;
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

procedure TEFDBDBXConnection.Close;
begin
  if FConnection.Connected then
    FConnection.Close;
end;

procedure TEFDBDBXConnection.CommitTransaction;
begin
  if FConnection.InTransaction then 
    FConnection.CommitFreeAndNil(FTransaction);
end;

function TEFDBDBXConnection.CreateDBCommand: IEFDBCommand;
begin
  Result := TEFDBDBXCommand.Create;
  try
    Result.Connection := Self;
  except
    FreeAndNilEFIntf(Result);
    raise;
  end;
end;

function TEFDBDBXConnection.CreateDBQuery: IEFDBQuery;
begin
  Result := TEFDBDBXQuery.Create;
  try
    Result.Connection := Self;
  except
    FreeAndNilEFIntf(Result);
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
  // consequence of closing the connection.
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

procedure TEFDBDBXConnection.Open;
begin
  if not FConnection.Connected then
  begin
    // Explicitly setting DriverName and ConnectionName should enable DBX's
    // re-reading of the params. Only setting those does not suffice.
    FConnection.DriverName := Config.GetExpandedString('Connection/ConnectionName');
    FConnection.DriverName := Config.GetExpandedString('Connection/DriverName');
    FConnection.Params.Text := Config.GetExpandedStrings('Connection');
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

{ TEFDBDBXComponent }

procedure TEFDBDBXComponent.ConnectionChanged;
begin
end;

function TEFDBDBXComponent.GetConnection: IEFDBConnection;
begin
  Result := FConnection;
end;

procedure TEFDBDBXComponent.SetConnection(const AValue: IEFDBConnection);
begin
  FConnection := AValue;
  ConnectionChanged;
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

procedure TEFDBDBXCommand.BeforeDestruction;
begin
  inherited;
  FreeAndNil(FQuery);
end;

procedure TEFDBDBXCommand.ConnectionChanged;
begin
  inherited;
  FQuery.SQLConnection := (Connection.AsObject as TEFDBDBXConnection).FConnection;
end;

function TEFDBDBXCommand.Execute: Integer;
begin
  FQuery.ExecSQL;
  Result := FQuery.RowsAffected;
end;

function TEFDBDBXCommand.GetCommandText: string;
begin
  Result := FQuery.SQL.Text;
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
  FQuery.SQL.Text := AValue;
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

procedure TEFDBDBXQuery.BeforeDestruction;
begin
  inherited;
  FreeAndNil(FQuery);
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
  Open;
  Result := 0;
end;

function TEFDBDBXQuery.GetCommandText: string;
begin
  Result := FQuery.SQL.Text;
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
  DataSet.Open;
end;

procedure TEFDBDBXQuery.SetCommandText(const AValue: string);
begin
  FQuery.SQL.Text := AValue;
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

function TEFDBDBXAdapter.InternalCreateDBConnection: IEFDBConnection;
begin
  Result := TEFDBDBXConnection.Create;
end;

function TEFDBDBXAdapter.InternalCreateDBInfo: IEFDBInfo;
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
        LColumn.DataType := FbDataTypeToEFDataType(Trim(LColumnQuery.FieldByName('DATA_TYPE').AsString));
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
