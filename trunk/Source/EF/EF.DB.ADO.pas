unit EF.DB.ADO;

{$I EF.Defines.inc}

interface

uses
  Classes, Contnrs, DB, ADODB,
  EF.Intf, EF.Classes,  EF.DB;

type
  {
    Utility class used to adapt ADO's TParameters to the standard TParams.
  }
  TEFDBADOParams = class(TParams)
  public
    {
      Sets the Value property of every parameter in ADestination whose name
      matches the name of a parameter in the current object to that of
      the current object's parameter.
    }
    procedure AssignValuesTo(const ADestination: TParameters);
  end;

  {
    Retrieves metadata from a database through ADO. Currently only
    SQL Server is supported.
  }
  TEFDBADOInfo = class(TEFDBInfo, IEFDBInfo)
  private
    FConnection: TADOConnection;
  protected
    procedure BeforeFetchInfo; override;
    procedure FetchTables(const ASchema: TEFDBSchemaInfo); override;
    procedure FetchTableColumns(const ATable: TEFDBTableInfo);
    procedure FetchTableForeignKeys(const ATable: TEFDBTableInfo);
    procedure FetchTablePrimaryKey(const ATable: TEFDBTableInfo);
  public
    property Connection: TADOConnection read FConnection write FConnection;
  end;

  type TEFDBADOQueryClass = class of TEFDBADOQuery;

  {
    Encapsulates both a DB connection and a DB transaction.

    Configuration items:
    
      ConnectionString: string (required)
        ADO database connection string.
  }
  TEFDBADOConnection = class(TEFDBConnection, IEFInterface,
    IEFDBConnection)
  private
    FConnection: TADOConnection;
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
    function FetchSequenceGeneratorValue(const ASequenceName: string): Int64;
    function GetLastAutoincValue(const ATableName: string = ''): Int64;
    function CreateDBCommand: IEFDBCommand; override;
    function CreateDBQuery: IEFDBQuery;
  protected
    function GetEFDBADOQueryClass: TEFDBADOQueryClass; virtual;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
  end;

  {
    Base class for ADO DB components.
  }
  TEFDBADOComponent = class(TEFComponent, IEFInterface, IEFDBComponent)
  private
    FConnection: IEFDBConnection;
  protected
    {
      Called whenever the connection changes. Descendants override it to
      link internal components to the new connection.
    }
    procedure ConnectionChanged; virtual;
    {
      Descendants should call this method before executing a statement
      against the database.
    }
    procedure InternalBeforeExecute; virtual;
  public
    // IEFDBComponent
    function GetConnection: IEFDBConnection;
    procedure SetConnection(const AValue: IEFDBConnection);
    property Connection: IEFDBConnection
      read GetConnection write SetConnection;
  end;

  // Encapsulates a DB command.
  TEFDBADOCommand = class(TEFDBADOComponent, IEFInterface,
    IEFDBComponent, IEFDBCommand)
  private
    FCommand: TADOCommand;
    FParams: TEFDBADOParams;
    FCommandText: string;
    // Copies the values in FParams to FCommand.Parameters.
    procedure UpdateInternalCommandParams;
    // Updates FCommand's command, if necessary, and prepares the command.
    procedure UpdateInternalCommandCommandText;
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

  // Encapsulates a DB query, that is a DB command with an associated result set.
  TEFDBADOQuery = class(TEFDBADOComponent, IEFInterface,
    IEFDBComponent, IEFDBQuery, IEFDBCommand)
  private
    FQuery: TADOQuery;
    FParams: TEFDBADOParams;
    FCommandText: string;
    // Copies the values in FParams to FQuery.Parameters.
    procedure UpdateInternalQueryParams;
    // Updates FQuery's command, if necessary, and prepares the query.
    procedure UpdateInternalQueryCommandText;
  protected
    procedure ConnectionChanged; override;
    procedure SetQueryProperties; virtual;
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
    // Execute and Open are synonims in this class. Execute always returns 0.
    function Execute: Integer;
    procedure Open; virtual;
    procedure Close;
    function IsOpen: Boolean;
    function GetDataSet: TDataSet;
    property DataSet: TDataSet read GetDataSet;
    function GetMasterSource: TDataSource;
    procedure SetMasterSource(const AValue: TDataSource);
    property MasterSource: TDataSource read GetMasterSource write SetMasterSource;
  end;

  // A DB adapter that creates ADO objects.
  TEFDBADOAdapter = class(TEFDBAdapter)
  protected
    function InternalCreateDBConnection: IEFDBConnection; override;
    function InternalCreateDBInfo: IEFDBInfo; override;
  end;

implementation

uses
  SysUtils, StrUtils, Variants, ADOInt,
  EF.VariantUtils, EF.DB.Utils, EF.Data, EF.Types;

function ADODataTypeToEFDataType(const AADODataType: Integer): TEFDataType;
begin
  case AADODataType of
    adTinyInt, adSmallInt, adError, adInteger, adUnsignedInt, adBigInt,
      adUnsignedBigInt, adUnsignedTinyInt, adUnsignedSmallInt: Result := edtInteger;
    adSingle, adDouble: Result := edtFloat;
    adCurrency: Result := edtCurrency;
    adBoolean: Result := edtBoolean;
    adDBDate: Result := edtDate;
    adDBTime: Result := edtTime;
    adDate, adDBTimeStamp, adFileTime, adDBFileTime: Result := edtDateTime;
    adChar, adVarChar, adWChar, adBSTR, adVarWChar, adLongVarChar,
      adLongVarWChar: Result := edtString;
    adDecimal, adNumeric, adVarNumeric: Result := edtBcd;
  else
    Result := edtUnknown;
  end;
end;

{ TEFDBADOParams }

procedure TEFDBADOParams.AssignValuesTo(const ADestination: TParameters);
var
  LParamIndex: Integer;
  LDestinationParameter: TParameter;
begin
  // Causes for errors such as "parameter incomplete or undefined" are
  // column name and type mismatches.
  for LParamIndex := 0 to Count - 1 do
  begin
    LDestinationParameter := ADestination.FindParam(Items[LParamIndex].Name);
    if Assigned(LDestinationParameter) then
      LDestinationParameter.Value := Items[LParamIndex].Value;
  end;
end;

{ TEFDBADOConnection }

procedure TEFDBADOConnection.AfterConstruction;
begin
  inherited;
  FConnection := TADOConnection.Create(nil);
  FConnection.LoginPrompt := False;
  FConnection.AfterConnect := AfterConnectionOpen;
end;

procedure TEFDBADOConnection.Open;
begin
  if not FConnection.Connected then
  begin
    FConnection.ConnectionString := Config.GetExpandedString('Connection/ConnectionString');
    { TODO : reimplement setting object properties from EF tree nodes via RTTI }
    //Configuration.SetObjectProperties(FConnection, 'InternalConnection.');
    FConnection.Open;
  end;
end;

procedure TEFDBADOConnection.Close;
begin
  if FConnection.Connected then
    FConnection.Close;
end;

procedure TEFDBADOConnection.StartTransaction;
begin
  if not IsOpen then
    Open;
  if not FConnection.InTransaction then
    FConnection.BeginTrans;
end;

procedure TEFDBADOConnection.CommitTransaction;
begin
  if FConnection.InTransaction then
    FConnection.CommitTrans;
end;

procedure TEFDBADOConnection.RollbackTransaction;
begin
  if FConnection.InTransaction then
    FConnection.RollbackTrans;
end;

function TEFDBADOConnection.CreateDBCommand: IEFDBCommand;
begin
  Result := TEFDBADOCommand.Create;
  try
    Result.Connection := Self;
    Open;
  except
    FreeAndNilEFIntf(Result);
    raise;
  end;
end;

function TEFDBADOConnection.CreateDBQuery: IEFDBQuery;
begin
  Result := GetEFDBADOQueryClass.Create;
  try
    Result.Connection := Self;
    Open;
  except
    FreeAndNilEFIntf(Result);
    raise;
  end;
end;

destructor TEFDBADOConnection.Destroy;
begin
  FreeAndNil(FConnection);
  inherited;
end;

function TEFDBADOConnection.IsInTransaction: Boolean;
begin
  Result := FConnection.InTransaction;
end;

function TEFDBADOConnection.ExecuteImmediate(const AStatement: string): Integer;
begin
  Open;
  FConnection.Execute(AStatement, Result);
end;

function TEFDBADOConnection.FetchSequenceGeneratorValue(
  const ASequenceName: string): Int64;
begin
  // Currently not supported.
  Result := 0;
end;

function TEFDBADOConnection.GetEFDBADOQueryClass: TEFDBADOQueryClass;
begin
  Result := TEFDBADOQuery;
end;

function TEFDBADOConnection.GetLastAutoincValue(
  const ATableName: string = ''): Int64;
begin
  // Only supported with SQL Server.
  Result := EFVarToInt(GetSingletonValue(Self, 'select @@IDENTITY'));
end;

function TEFDBADOConnection.IsOpen: Boolean;
begin
  Result := FConnection.Connected;
end;

{ TEFDBADOComponent }

procedure TEFDBADOComponent.ConnectionChanged;
begin
end;

function TEFDBADOComponent.GetConnection: IEFDBConnection;
begin
  Result := FConnection;
end;

procedure TEFDBADOComponent.InternalBeforeExecute;
begin
  if not Connection.IsOpen then
    Connection.Open;
end;

procedure TEFDBADOComponent.SetConnection(const AValue: IEFDBConnection);
begin
  FConnection := AValue;
  ConnectionChanged;
end;

{ TEFDBADOCommand }

procedure TEFDBADOCommand.AfterConstruction;
begin
  inherited;
  FCommand := TADOCommand.Create(nil);
  FParams := TEFDBADOParams.Create(nil);
end;

procedure TEFDBADOCommand.BeforeDestruction;
begin
  inherited;
  FreeAndNil(FCommand);
  FreeAndNil(FParams);
end;

function TEFDBADOCommand.Execute: Integer;
begin
  UpdateInternalCommandCommandText;
  UpdateInternalCommandParams;
  InternalBeforeExecute;
  FCommand.Execute(Result, EmptyParam);
end;

function TEFDBADOCommand.GetCommandText: string;
begin
  Result := FCommandText;
end;

function TEFDBADOCommand.GetParams: TParams;
begin
  Result := FParams;
end;

function TEFDBADOCommand.GetPrepared: Boolean;
begin
  Result := FCommand.Prepared;
end;

procedure TEFDBADOCommand.SetCommandText(const AValue: string);
var
  LThrowaway: string;
begin
  FCommandText := AValue;
  LThrowaway := FCommandText;
  UniqueString(LThrowaway);
  // Note: ParseSQL incorrectly behaves as its first parameter was passed
  // by reference and modifies it. So we pass a throwaway string to it.
  FParams.ParseSQL(LThrowaway, True);
end;

procedure TEFDBADOCommand.SetParams(const AValue: TParams);
begin
  FParams.Assign(AValue);
end;

procedure TEFDBADOCommand.SetPrepared(const AValue: Boolean);
begin
  FCommand.Prepared := AValue;
end;

procedure TEFDBADOCommand.ConnectionChanged;
begin
  inherited;
  FCommand.Connection := (Connection.AsObject as TEFDBADOConnection).FConnection;
  FCommand.CommandTimeout := FCommand.Connection.CommandTimeout;
end;

procedure TEFDBADOCommand.UpdateInternalCommandCommandText;
begin
  if FCommand.CommandText <> FCommandText then
  begin
    FCommand.CommandText := FCommandText;
    FCommand.Prepared := True;
  end;
end;

procedure TEFDBADOCommand.UpdateInternalCommandParams;
begin
  FParams.AssignValuesTo(FCommand.Parameters);
end;

{ TEFDBADOQuery }

procedure TEFDBADOQuery.AfterConstruction;
begin
  inherited;
  FQuery := TADOQuery.Create(nil);
  SetQueryProperties;
  FParams := TEFDBADOParams.Create(nil);
end;

procedure TEFDBADOQuery.BeforeDestruction;
begin
  inherited;
  FreeAndNil(FQuery);
  FreeAndNil(FParams);
end;

procedure TEFDBADOQuery.ConnectionChanged;
begin
  inherited;
  FQuery.Connection := (Connection.AsObject as TEFDBADOConnection).FConnection;
  FQuery.CommandTimeout := FQuery.Connection.CommandTimeout;
end;

function TEFDBADOQuery.Execute: Integer;
begin
  Open;
  Result := 0;
end;

procedure TEFDBADOQuery.Open;
begin
  UpdateInternalQueryCommandText;
  UpdateInternalQueryParams;
  InternalBeforeExecute;
  DataSet.Open;
end;

procedure TEFDBADOQuery.Close;
begin
  DataSet.Close;
end;

function TEFDBADOQuery.GetCommandText: string;
begin
  Result := FCommandText;
end;

function TEFDBADOQuery.GetDataSet: TDataSet;
begin
  Result := FQuery;
end;

function TEFDBADOQuery.GetMasterSource: TDataSource;
begin
  Result := FQuery.DataSource;
end;

function TEFDBADOQuery.GetParams: TParams;
begin
  Result := FParams;
end;

function TEFDBADOQuery.GetPrepared: Boolean;
begin
  Result := FQuery.Prepared;
end;

function TEFDBADOQuery.IsOpen: Boolean;
begin
  Result := DataSet.Active;
end;

procedure TEFDBADOQuery.SetCommandText(const AValue: string);
var
  LThrowaway: string;
begin
  FCommandText := AValue;
  LThrowaway := FCommandText;
  UniqueString(LThrowaway);
  // Note: ParseSQL incorrectly behaves as its first parameter was passed
  // by reference and modifies it. So we pass a throwaway string to it.
  FParams.ParseSQL(LThrowaway, True);
end;

procedure TEFDBADOQuery.SetMasterSource(const AValue: TDataSource);
begin
  FQuery.DataSource := AValue;
end;

procedure TEFDBADOQuery.SetParams(const AValue: TParams);
begin
  FParams.Assign(AValue);
end;

procedure TEFDBADOQuery.SetPrepared(const AValue: Boolean);
begin
  FQuery.Prepared := AValue;
end;

procedure TEFDBADOQuery.SetQueryProperties;
begin
  FQuery.CursorLocation := clUseServer;
  FQuery.CursorType := ctKeyset;
end;

procedure TEFDBADOQuery.UpdateInternalQueryCommandText;
begin
  if FQuery.SQL.Text <> FCommandText then
  begin
    FQuery.SQL.Text := FCommandText;
    FQuery.Prepared := True;
  end;
end;

procedure TEFDBADOQuery.UpdateInternalQueryParams;
begin
  FParams.AssignValuesTo(FQuery.Parameters);
end;

{ TEFDBADOFactory }

function TEFDBADOAdapter.InternalCreateDBConnection: IEFDBConnection;
begin
  Result := TEFDBADOConnection.Create;
end;

{ TEFDBADOInfo }

procedure TEFDBADOInfo.BeforeFetchInfo;
begin
  inherited;
  Assert(Assigned(FConnection));
end;

procedure TEFDBADOInfo.FetchTables(const ASchema: TEFDBSchemaInfo);
var
  LTableDataSet: TADODataSet;
  LTable: TEFDBTableInfo;
begin
  LTableDataSet := TADODataSet.Create(nil);
  try
    FConnection.OpenSchema(siTables,
      VarArrayOf([Unassigned, Unassigned, Unassigned, Unassigned]),
      EmptyParam, LTableDataSet);
    while not LTableDataSet.Eof do
    begin
      if MatchStr(LTableDataSet.FieldByName('TABLE_TYPE').AsString, ['TABLE', 'VIEF']) then
      begin
        LTable := TEFDBTableInfo.Create;
        try
          LTable.Name := LTableDataSet.FieldByName('TABLE_NAME').AsString;
          FetchTableColumns(LTable);
          if LTableDataSet.FieldByName('TABLE_TYPE').AsString = 'TABLE' then
          begin
            FetchTablePrimaryKey(LTable);
            FetchTableForeignKeys(LTable);
          end;
          ASchema.AddTable(LTable);
        except
          FreeAndNil(LTable);
        end;
      end;
      LTableDataSet.Next;
    end;
    LTableDataSet.Close;
  finally
    LTableDataSet.Free;
  end;
end;

procedure TEFDBADOInfo.FetchTableColumns(const ATable: TEFDBTableInfo);
var
  LColumnDataSet: TADODataSet;
  LColumn: TEFDBColumnInfo;
begin
  LColumnDataSet := TADODataSet.Create(nil);
  try
    FConnection.OpenSchema(siColumns,
      VarArrayOf([Unassigned, Unassigned, ATable.Name, Unassigned]),
      EmptyParam, LColumnDataSet);
    while not LColumnDataSet.Eof do
    begin
      LColumn := TEFDBColumnInfo.Create;
      try
        LColumn.Name := LColumnDataSet.FieldByName('COLUMN_NAME').AsString;
        LColumn.DataType := ADODataTypeToEFDataType(LColumnDataSet.FieldByName('DATA_TYPE').AsInteger);
        LColumn.Size := LColumnDataSet.FieldByName('CHARACTER_MAXIMUM_LENGTH').AsInteger;
        LColumn.IsRequired := not LColumnDataSet.FieldByName('IS_NULLABLE').AsBoolean;
        ATable.AddColumn(LColumn);
      except
        FreeAndNil(LColumn);
      end;
      LColumnDataSet.Next;
    end;
  finally
    LColumnDataSet.Free;
  end;
end;

procedure TEFDBADOInfo.FetchTablePrimaryKey(const ATable: TEFDBTableInfo);
var
  LPrimaryKeyDataSet: TADODataSet;
begin
  LPrimaryKeyDataSet := TADODataSet.Create(nil);
  try
    FConnection.OpenSchema(siPrimaryKeys,
      VarArrayOf([Unassigned, Unassigned, ATable.Name]),
      EmptyParam, LPrimaryKeyDataSet);
    while not LPrimaryKeyDataSet.Eof do
    begin
      if ATable.PrimaryKey.Name = '' then
        ATable.PrimaryKey.Name := LPrimaryKeyDataSet.FieldByName('PK_NAME').AsString
      else if ATable.PrimaryKey.Name <> LPrimaryKeyDataSet.FieldByName('PK_NAME').AsString then
        raise EEFError.Create('Error fetching primary key data for table ' + ATable.Name);
      ATable.PrimaryKey.ColumnNames.Add(LPrimaryKeyDataSet.FieldByName('COLUMN_NAME').AsString);
      LPrimaryKeyDataSet.Next;
    end;
  finally
    LPrimaryKeyDataSet.Free;
  end;
end;

procedure TEFDBADOInfo.FetchTableForeignKeys(const ATable: TEFDBTableInfo);
var
  LForeignKeyDataSet: TADODataSet;
  LForeignKey: TEFDBForeignKeyInfo;
begin
  LForeignKeyDataSet := TADODataSet.Create(nil);
  try
    FConnection.OpenSchema(siForeignKeys,
      VarArrayOf([Unassigned, Unassigned, Unassigned, Unassigned, Unassigned, ATable.Name]),
      EmptyParam, LForeignKeyDataSet);
    try
      while not LForeignKeyDataSet.Eof do
      begin
        LForeignKey := ATable.FindForeignKey(LForeignKeyDataSet.FieldByName('FK_NAME').AsString);
        if not Assigned(LForeignKey) then
        begin
          LForeignKey := TEFDBForeignKeyInfo.Create;
          LForeignKey.Name := LForeignKeyDataSet.FieldByName('FK_NAME').AsString;
          ATable.AddForeignKey(LForeignKey);
        end;
        LForeignKey.ForeignTableName := LForeignKeyDataSet.FieldByName('PK_TABLE_NAME').AsString;
        LForeignKey.ColumnNames.Add(LForeignKeyDataSet.FieldByName('FK_COLUMN_NAME').AsString);
        LForeignKey.ForeignColumnNames.Add(LForeignKeyDataSet.FieldByName('PK_COLUMN_NAME').AsString);

        LForeignKeyDataSet.Next;
      end;
    except
      FreeAndNil(LForeignKey);
      raise;
    end;
  finally
    LForeignKeyDataSet.Free;
  end;
end;

function TEFDBADOAdapter.InternalCreateDBInfo: IEFDBInfo;
begin
  Result := TEFDBADOInfo.Create;
end;

initialization
  TEFDBAdapterRegistry.Instance.RegisterDBAdapter('ADO', TEFDBADOAdapter.Create);

finalization
  TEFDBAdapterRegistry.Instance.UnregisterDBAdapter('ADO');

end.
