{-------------------------------------------------------------------------------
   Copyright 2014 Ethea S.r.l.

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
///	  FireDac-based database access layer.
///	</summary>
unit EF.DB.FD;

{$I EF.Defines.inc}

interface

uses
  Classes, DB, Contnrs,
  FireDAC.Comp.Client,
  {$IFDEF IBFB_SUPPORT}FireDAC.Phys.IBBase, FireDAC.Phys.FB, FireDAC.Phys.IB,{$ENDIF}
  {$IFDEF MSSQL_SUPPORT}FireDAC.Phys.MSSQL, FireDAC.Phys.MSSQLMeta,{$ENDIF}
  {$IFDEF SYBASE_SUPPORT}FireDAC.Phys.ASA, FireDAC.Phys.ASAWrapper,{$ENDIF}
  {$IFDEF ORACLE_SUPPORT}FireDAC.Phys.ORACLE, FireDAC.Phys.ORACLEMeta,{$ENDIF}
  {$IFDEF PGSQL_SUPPORT}FireDAC.Phys.PG, FireDAC.Phys.PGWrapper,{$ENDIF}
  {$IFDEF MYSQL_SUPPORT}FireDAC.Phys.MYSQL, FireDAC.Phys.MYSQLWrapper,{$ENDIF}
  {$IFDEF SQLITE_SUPPORT}FireDAC.Phys.SQLite, FireDAC.Phys.SQLiteWrapper,{$ENDIF}
  FireDAC.Stan.Def, FireDAC.Stan.Option, FireDAC.Stan.Param, FireDAC.Stan.Intf, FireDAC.Phys.Intf,
  FireDAC.DApt.Intf, FireDAC.Stan.Async, FireDAC.DApt, FireDAC.Comp.DataSet,
  EF.Tree, EF.DB;

type
  ///	<summary>
  ///	  Utility class used to adapt FireDAC's TFDParams to the standard TParams.
  ///	</summary>
  TEFDBFDParams = class(TParams)
  public
    ///	<summary>
    ///	  Sets the Value property of every parameter in ADestination whose name
    ///	  matches the name of a parameter in the current object to that of the
    ///	  current object's parameter.
    ///	</summary>
    procedure AssignValuesTo(const ADestination: TFDParams);
  end;

  ///	<summary>
  ///	  Retrieves metadata from a database through FireDAC.
  ///	</summary>
  TEFDBFDInfo = class(TEFDBInfo)
  private
    FConnection: TFDConnection;
    function FDDataTypeToEFDataType(const AFDDataType: TFDDataType): TEFDataType;
  protected
    procedure BeforeFetchInfo; override;
    procedure FetchTables(const ASchema: TEFDBSchemaInfo); override;
    procedure FetchTableColumns(const ATable: TEFDBTableInfo);
    procedure FetchTableForeignKeys(const ATable: TEFDBTableInfo);
    procedure FetchTablePrimaryKey(const ATable: TEFDBTableInfo);
  public
    constructor Create(const AConnection: TFDConnection);
    property Connection: TFDConnection read FConnection write FConnection;
  end;

  TEFDBFDQueryClass = class of TEFDBFDQuery;

  TEFDBFDConnection = class(TEFDBConnection)
  private
    FConnection: TFDConnection;
    FConnectionString: TStrings;
    function GetDriverId: string;
    function GetIsolation: string;
  protected
    function GetQueryClass: TEFDBFDQueryClass; virtual;
    function CreateDBEngineType: TEFDBEngineType; override;
    procedure InternalOpen; override;
    procedure InternalClose; override;
    function InternalCreateDBInfo: TEFDBInfo; override;
    property DriverId: string read GetDriverId;
    property Isolation: string read GetIsolation;
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
    function FetchSequenceGeneratorValue(const ASequenceName: string): Int64; override;
    function GetLastAutoincValue(const ATableName: string = ''): Int64; override;
    function CreateDBCommand: TEFDBCommand; override;
    function CreateDBQuery: TEFDBQuery; override;
    function GetConnection: TObject; override;
  end;

  TEFDBFDCommand = class(TEFDBCommand)
  private
    FCommand: TFDQuery;
    FParams: TEFDBFDParams;
    FCommandText: string;
    // Copies the values in FParams to FCommand.Parameters.
    procedure UpdateInternalCommandParams;
    // Updates FCommand's command, if necessary, and prepares the command.
    procedure UpdateInternalCommandCommandText;
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

  TEFDBFDQuery = class(TEFDBQuery)
  private
    FQuery: TFDQuery;
    FParams: TEFDBFDParams;
    FCommandText: string;
    // Copies the values in FParams to FQuery.Parameters.
    procedure UpdateInternalQueryParams;
    // Updates FQuery's command, if necessary, and prepares the query.
    procedure UpdateInternalQueryCommandText;
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

  TEFDBFDAdapter = class(TEFDBAdapter)
  protected
    function InternalCreateDBConnection: TEFDBConnection; override;
    class function InternalGetClassId: string; override;
  end;

implementation

uses
  SysUtils, StrUtils, TypInfo,
  //DBXMetaDataNames,
  EF.StrUtils, EF.Localization, EF.Types;

function TEFDBFDInfo.FDDataTypeToEFDataType(const AFDDataType: TFDDataType): TEFDataType;
var
  LClass: TEFDataTypeClass;
begin
  case AFDDataType of
    dtBoolean: LClass := TEFBooleanDataType;
    dtSByte, dtInt16, dtInt32, dtInt64, dtByte,
    dtUInt16, dtUInt32, dtUInt64: LClass := TEFIntegerDataType;
    dtSingle, dtDouble, dtExtended: LClass := TEFFloatDataType;
    dtCurrency, dtBCD, dtFmtBCD: LClass := TEFDecimalDataType;
    dtDateTime, dtDate, dtDateTimeStamp: LClass := TEFDateDataType;
    dtTime, dtTimeIntervalFull, dtTimeIntervalYM, dtTimeIntervalDS: LClass := TEFTimeDataType;
    dtAnsiString, dtWideString, dtByteString: LClass := TEFStringDataType;
    dtBlob: LClass := TEFBlobDataType;
    dtMemo, dtWideMemo, dtXML: LClass := TEFMemoDataType;
  else
    LClass := TEFStringDataType;
  end;
  Result := TEFDataTypeFactory.Instance.GetDataType(LClass);
end;

{ TEFDBADOParams }

procedure TEFDBFDParams.AssignValuesTo(const ADestination: TFDParams);
var
  LParamIndex: Integer;
  LDestinationParameter: TFDParam;
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

{ TEFDBFDConnection }

procedure TEFDBFDConnection.AfterConstruction;
begin
  inherited;
  FConnection := TFDConnection.Create(nil);
  FConnection.LoginPrompt := False;
  FConnection.AfterConnect := AfterConnectionOpen;
  FConnectionString := TStringList.Create;
end;

procedure TEFDBFDConnection.InternalOpen;
var
  LDriverId: string;
  LIsolation: Integer;
begin
  inherited;
  FConnection.Params.Clear;
  LDriverId := DriverId;
  //Common FireDAC parameters
  FConnection.Params.Values['DriverID'] := LDriverId;
  //Isolation Level
  LIsolation := GetEnumValue(TypeInfo(TFDTxIsolation),'xi'+Isolation);
  FConnection.TxOptions.Isolation := TFDTxIsolation(LIsolation);

  if SameText(LDriverID, 'MSSQL') then
  begin
    // Use Specific parameters for MS-SQL
    FConnection.Params.Values['Server'] := Config.GetExpandedString('Connection/Server');
    FConnection.Params.Values['User_Name'] := Config.GetExpandedString('Connection/User_Name');
    FConnection.Params.Values['Password'] := Config.GetExpandedString('Connection/Password');
    FConnection.Params.Values['ApplicationName'] := Config.GetExpandedString('Connection/ApplicationName');
    FConnection.Params.Values['Database'] := Config.GetExpandedString('Connection/Database');
    FConnection.Params.Values['OSAuthent'] := Config.GetString('Connection/OSAuthent', 'No');
    FConnection.Params.Values['MARS'] := 'Yes';
  end
  else if SameText(LDriverID, 'FB') then
  begin
    // Use Specific parameters for Firebird
    FConnection.Params.Values['Server'] := Config.GetExpandedString('Connection/Server');
    FConnection.Params.Values['User_Name'] := Config.GetExpandedString('Connection/User_Name');
    FConnection.Params.Values['Password'] := Config.GetExpandedString('Connection/Password');
    FConnection.Params.Values['CharacterSet'] := Config.GetExpandedString('Connection/CharacterSet');
    FConnection.Params.Values['Database'] := Config.GetExpandedString('Connection/Database');
    FConnection.Params.Values['Protocol'] := Config.GetExpandedString('Connection/Protocol');
  end;

  FConnection.Open;
end;

procedure TEFDBFDConnection.InternalClose;
begin
  if FConnection.Connected then
    FConnection.Close;
end;

function TEFDBFDConnection.InternalCreateDBInfo: TEFDBInfo;
begin
  Result := TEFDBFDInfo.Create(FConnection);
end;

procedure TEFDBFDConnection.StartTransaction;
begin
  if not IsOpen then
    Open;
  if not FConnection.InTransaction then
    FConnection.StartTransaction;
end;

procedure TEFDBFDConnection.CommitTransaction;
begin
  if FConnection.InTransaction then
    FConnection.Commit;
end;

procedure TEFDBFDConnection.RollbackTransaction;
begin
  if FConnection.InTransaction then
    FConnection.Rollback;
end;

function TEFDBFDConnection.CreateDBCommand: TEFDBCommand;
begin
  Result := TEFDBFDCommand.Create;
  try
    Result.Connection := Self;
    Open;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TEFDBFDConnection.CreateDBEngineType: TEFDBEngineType;
var
  LDriverId: string;
begin
  LDriverId := DriverId;
  if SameText(LDriverId, 'MSSQL') then
    Result := TEFSQLServerDBEngineType.Create
  else if SameText(LDriverId, 'FB') or SameText(LDriverId, 'IB') then
    Result := TEFFirebirdDBEngineType.Create
  else
    Result := inherited CreateDBEngineType;
end;

function TEFDBFDConnection.CreateDBQuery: TEFDBQuery;
begin
  Result := GetQueryClass.Create;
  try
    Result.Connection := Self;
    Open;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

destructor TEFDBFDConnection.Destroy;
begin
  FreeAndNil(FConnection);
  FreeAndNIl(FConnectionString);
  inherited;
end;

function TEFDBFDConnection.IsInTransaction: Boolean;
begin
  Result := FConnection.InTransaction; 
end;

function TEFDBFDConnection.ExecuteImmediate(const AStatement: string): Integer;
begin
  Assert(Assigned(FConnection));

  if AStatement = '' then
    raise EEFError.Create(_('Unspecified Statement text.'));

  Result := FConnection.ExecSQL(AStatement);
end;

function TEFDBFDConnection.FetchSequenceGeneratorValue(
  const ASequenceName: string): Int64;
begin
  // Currently not supported.
  Result := 0;
end;

function TEFDBFDConnection.GetQueryClass: TEFDBFDQueryClass;
begin
  Result := TEFDBFDQuery;
end;

function TEFDBFDConnection.GetConnection: TObject;
begin
  Result := FConnection;
end;

function TEFDBFDConnection.GetDriverId: string;
begin
  Result := Config.GetExpandedString('Connection/DriverID');
end;

function TEFDBFDConnection.GetIsolation: string;
begin
  Result := Config.GetExpandedString('Connection/Isolation','ReadCommitted');
end;

function TEFDBFDConnection.GetLastAutoincValue(
  const ATableName: string = ''): Int64;
begin
  // Auto-inc fields currently not supported in dbExpress.
  Result := 0;
end;

function TEFDBFDConnection.IsOpen: Boolean;
begin
  if FConnection = nil then
    Result := False
  else
    Result := FConnection.Connected;
end;

{ TEFDBFDCommand }

procedure TEFDBFDCommand.AfterConstruction;
begin
  inherited;
  FCommand := TFDQuery.Create(nil);
  FParams := TEFDBFDParams.Create(nil);
end;

destructor TEFDBFDCommand.Destroy;
begin
  FreeAndNil(FCommand);
  FreeAndNil(FParams);
  inherited;
end;

function TEFDBFDCommand.Execute: Integer;
begin
  UpdateInternalCommandCommandText;
  Connection.DBEngineType.BeforeExecute(FCommandText, FParams);
  UpdateInternalCommandParams;
  inherited;
  FCommand.Execute;
  Result := FCommand.RowsAffected;
end;

function TEFDBFDCommand.GetCommandText: string;
begin
  Result := FCommandText;
end;

function TEFDBFDCommand.GetParams: TParams;
begin
  Result := FParams;
end;

function TEFDBFDCommand.GetPrepared: Boolean;
begin
  Result := FCommand.Prepared;
end;

procedure TEFDBFDCommand.SetCommandText(const AValue: string);
var
  LThrowaway: string;
begin
  FCommandText := AValue;
  LThrowaway := FCommandText;
  UniqueString(LThrowaway);
  // Note: ParseSQL incorrectly behaves as if its first parameter was passed
  // by reference and modifies it. So we must pass a disposable string to it.
  FParams.ParseSQL(LThrowaway, True);
end;

procedure TEFDBFDCommand.SetParams(const AValue: TParams);
begin
  FParams.Assign(AValue);
end;

procedure TEFDBFDCommand.SetPrepared(const AValue: Boolean);
begin
  FCommand.Prepared := AValue;
end;

procedure TEFDBFDCommand.ConnectionChanged;
begin
  inherited;
  FCommand.Connection := (Connection.AsObject as TEFDBFDConnection).FConnection;
end;

procedure TEFDBFDCommand.UpdateInternalCommandCommandText;
begin
  if FCommand.SQL.Text <> FCommandText then
    FCommand.SQL.Text := ExpandCommandText(FCommandText);
end;

procedure TEFDBFDCommand.UpdateInternalCommandParams;
begin
  FParams.AssignValuesTo(FCommand.Params);
end;

{ TEFDBFDQuery }

procedure TEFDBFDQuery.AfterConstruction;
begin
  inherited;
  FQuery := TFDQuery.Create(nil);
  FParams := TEFDBFDParams.Create(nil);
end;

destructor TEFDBFDQuery.Destroy;
begin
  FreeAndNil(FQuery);
  FreeAndNil(FParams);
  inherited;
end;

procedure TEFDBFDQuery.ConnectionChanged;
begin
  inherited;
  FQuery.Connection := (Connection.AsObject as TEFDBFDConnection).FConnection;
end;

function TEFDBFDQuery.Execute: Integer;
begin
  inherited;
  Open;
  Result := 0;
end;

procedure TEFDBFDQuery.Open;
begin
  UpdateInternalQueryCommandText;
  Connection.DBEngineType.BeforeExecute(FCommandText, FParams);
  UpdateInternalQueryParams;
  InternalBeforeExecute;
  try
    FQuery.Open;
  except
    on E: Exception do
    begin
      raise EEFError.Create(_(Format('Error "%s" opening query: %s.',
        [E.Message, FCommandText])));
    end
    else
      raise;
  end;
end;

procedure TEFDBFDQuery.Close;
begin
  FQuery.Close;
end;

function TEFDBFDQuery.GetCommandText: string;
begin
  Result := FCommandText;
end;

function TEFDBFDQuery.GetDataSet: TDataSet;
begin
  Result := FQuery;
end;

function TEFDBFDQuery.GetMasterSource: TDataSource;
begin
  Result := FQuery.DataSource;
end;

function TEFDBFDQuery.GetParams: TParams;
begin
  Result := FParams;
end;

function TEFDBFDQuery.GetPrepared: Boolean;
begin
  Result := FQuery.Prepared;
end;

function TEFDBFDQuery.IsOpen: Boolean;
begin
  Result := FQuery.Active;
end;

procedure TEFDBFDQuery.SetCommandText(const AValue: string);
var
  LThrowaway: string;
begin
  FCommandText := AValue;
  LThrowaway := FCommandText;
  UniqueString(LThrowaway);
  // Note: ParseSQL incorrectly behaves as if its first parameter was passed
  // by reference and modifies it. So we pass a disposable string to it.
  FParams.ParseSQL(LThrowaway, True);
end;

procedure TEFDBFDQuery.SetMasterSource(const AValue: TDataSource);
begin
  FQuery.DataSource := AValue;
end;

procedure TEFDBFDQuery.SetParams(const AValue: TParams);
begin
  FParams.Assign(AValue);
end;

procedure TEFDBFDQuery.SetPrepared(const AValue: Boolean);
begin
  FQuery.Prepared := AValue;
end;

procedure TEFDBFDQuery.UpdateInternalQueryCommandText;
begin
  FQuery.SQL.Text := ExpandCommandText(FCommandText);
end;

procedure TEFDBFDQuery.UpdateInternalQueryParams;
begin
  FParams.AssignValuesTo(FQuery.Params);
end;

{ TEFDBFDInfo }

procedure TEFDBFDInfo.BeforeFetchInfo;
begin
  inherited;
  Assert(Assigned(FConnection));
end;

procedure TEFDBFDInfo.FetchTables(const ASchema: TEFDBSchemaInfo);
(*
var
  LTableDataSet: TADODataSet;
  LTable: TEFDBTableInfo;
  LTableType: string;
*)
begin
(*
  LTableDataSet := TADODataSet.Create(nil);
  try
    FConnection.OpenSchema(siTables,
      VarArrayOf([Unassigned, Unassigned, Unassigned, Unassigned]),
      EmptyParam, LTableDataSet);
    while not LTableDataSet.Eof do
    begin
      LTableType := LTableDataSet.FieldByName('TABLE_TYPE').AsString;
      if SameText(LTableType, 'Table') or (ViewsAsTables and SameText(LTableType, 'VIEW')) then
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
*)
end;

constructor TEFDBFDInfo.Create(const AConnection: TFDConnection);
begin
  inherited Create;
  FConnection := AConnection;
end;

procedure TEFDBFDInfo.FetchTableColumns(const ATable: TEFDBTableInfo);
(*
var
  LColumnDataSet: TADODataSet;
  LColumn: TEFDBColumnInfo;
*)
begin
(*
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
        LColumn.DataType := TEFDataTypeFactory.Instance.GetDataType(
          ADODataTypeToEFDataType(LColumnDataSet.FieldByName('DATA_TYPE').AsInteger));
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
*)
end;

procedure TEFDBFDInfo.FetchTablePrimaryKey(const ATable: TEFDBTableInfo);
(*
var
  LPrimaryKeyDataSet: TADODataSet;
*)
begin
(*
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
*)
end;

procedure TEFDBFDInfo.FetchTableForeignKeys(const ATable: TEFDBTableInfo);
(*
var
  LForeignKeyDataSet: TADODataSet;
  LForeignKey: TEFDBForeignKeyInfo;
*)
begin
(*
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
*)
end;

{ TEFDBFDAdapter }

function TEFDBFDAdapter.InternalCreateDBConnection: TEFDBConnection;
begin
  Result := TEFDBFDConnection.Create;
end;

class function TEFDBFDAdapter.InternalGetClassId: string;
begin
  Result := 'FD';
end;

initialization
  TEFDBAdapterRegistry.Instance.RegisterDBAdapter(TEFDBFDAdapter.GetClassId, TEFDBFDAdapter.Create);

finalization
  TEFDBAdapterRegistry.Instance.UnregisterDBAdapter(TEFDBFDAdapter.GetClassId);

end.
