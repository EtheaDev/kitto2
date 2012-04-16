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
///	  ADO-based database access layer.
///	</summary>
unit EF.DB.ADO;

{$I EF.Defines.inc}

interface

uses
  Classes, Contnrs, DB, ADODB,
  EF.DB;

type
  ///	<summary>
  ///	  Utility class used to adapt ADO's TParameters to the standard TParams.
  ///	</summary>
  TEFDBADOParams = class(TParams)
  public
    ///	<summary>
    ///	  Sets the Value property of every parameter in ADestination whose name
    ///	  matches the name of a parameter in the current object to that of the
    ///	  current object's parameter.
    ///	</summary>
    procedure AssignValuesTo(const ADestination: TParameters);
  end;

  ///	<summary>
  ///	  Retrieves metadata from a database through ADO. Currently only SQL
  ///	  Server is supported.
  ///	</summary>
  TEFDBADOInfo = class(TEFDBInfo)
  private
    FConnection: TADOConnection;
  protected
    procedure BeforeFetchInfo; override;
    procedure FetchTables(const ASchema: TEFDBSchemaInfo); override;
    procedure FetchTableColumns(const ATable: TEFDBTableInfo);
    procedure FetchTableForeignKeys(const ATable: TEFDBTableInfo);
    procedure FetchTablePrimaryKey(const ATable: TEFDBTableInfo);
  public
    constructor Create(const AConnection: TADOConnection);
    property Connection: TADOConnection read FConnection write FConnection;
  end;

  TEFDBADOQueryClass = class of TEFDBADOQuery;

  TEFDBADOConnection = class(TEFDBConnection)
  private
    FConnection: TADOConnection;
  protected
    function GetQueryClass: TEFDBADOQueryClass; virtual;
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
    function FetchSequenceGeneratorValue(const ASequenceName: string): Int64; override;
    function GetLastAutoincValue(const ATableName: string = ''): Int64; override;
    function CreateDBCommand: TEFDBCommand; override;
    function CreateDBQuery: TEFDBQuery; override;
  end;

  TEFDBADOCommand = class(TEFDBCommand)
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

  TEFDBADOQuery = class(TEFDBQuery)
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
    // Execute and Open are synonims in this class. Execute always returns 0.
    function Execute: Integer; override;
    procedure Open; override;
    procedure Close; override;
    function IsOpen: Boolean; override;
  end;

  TEFDBADOAdapter = class(TEFDBAdapter)
  protected
    function InternalCreateDBConnection: TEFDBConnection; override;
  end;

implementation

uses
  SysUtils, StrUtils, Variants, ADOInt,
  EF.VariantUtils, EF.Types, EF.Tree, EF.SQL;

function ADODataTypeToEFDataType(const AADODataType: Integer): string;
begin
  case AADODataType of
    adTinyInt, adSmallInt, adError, adInteger, adUnsignedInt, adBigInt,
      adUnsignedBigInt, adUnsignedTinyInt, adUnsignedSmallInt: Result := 'Integer';
    adSingle, adDouble: Result := 'Float';
    adCurrency: Result := 'Currency';
    adBoolean: Result := 'Boolean';
    adDBDate: Result := 'Date';
    adDBTime: Result := 'Time';
    adDate, adDBTimeStamp, adFileTime, adDBFileTime: Result := 'DateTime';
    adChar, adVarChar, adWChar, adBSTR, adVarWChar, adLongVarChar,
      adLongVarWChar: Result := 'String';
    adDecimal, adNumeric, adVarNumeric: Result := 'Decimal';
  else
    Result := 'String';
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

procedure TEFDBADOConnection.InternalOpen;
var
  LConnectionString: string;
begin
  if not FConnection.Connected then
  begin
    // Use a ConnectionString if specified, otherwise build one by concatenating
    // child nodes as other database providers do.
    LConnectionString := Config.GetExpandedString('Connection/ConnectionString');
    if LConnectionString <> '' then
      FConnection.ConnectionString := LConnectionString
     else
      FConnection.ConnectionString := Config.GetChildrenAsExpandedStrings('Connection', ';');
    { TODO : implement setting object properties from EF tree nodes via RTTI }
    //Config.SetObjectProperties(FConnection, 'InternalConnection');
    FConnection.Open;
  end;
end;

procedure TEFDBADOConnection.InternalClose;
begin
  if FConnection.Connected then
    FConnection.Close;
end;

function TEFDBADOConnection.InternalCreateDBInfo: TEFDBInfo;
begin
  Result := TEFDBADOInfo.Create(FConnection);
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

function TEFDBADOConnection.CreateDBCommand: TEFDBCommand;
begin
  Result := TEFDBADOCommand.Create;
  try
    Result.Connection := Self;
    Open;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TEFDBADOConnection.CreateDBEngineType: TEFDBEngineType;
begin
  { TODO : Change type based on connection string. }
  Result := TEFSQLServerDBEngineType.Create;
end;

function TEFDBADOConnection.CreateDBQuery: TEFDBQuery;
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

function TEFDBADOConnection.GetQueryClass: TEFDBADOQueryClass;
begin
  Result := TEFDBADOQuery;
end;

function TEFDBADOConnection.GetLastAutoincValue(
  const ATableName: string = ''): Int64;
begin
  // Only supported with SQL Server.
  Result := EFVarToInt(GetSingletonValue('select @@IDENTITY'));
end;

function TEFDBADOConnection.IsOpen: Boolean;
begin
  if FConnection = nil then
    Result := False
  else
    Result := FConnection.Connected;
end;

{ TEFDBADOCommand }

procedure TEFDBADOCommand.AfterConstruction;
begin
  inherited;
  FCommand := TADOCommand.Create(nil);
  FParams := TEFDBADOParams.Create(nil);
end;

destructor TEFDBADOCommand.Destroy;
begin
  FreeAndNil(FCommand);
  FreeAndNil(FParams);
  inherited;
end;

function TEFDBADOCommand.Execute: Integer;
begin
  UpdateInternalCommandCommandText;
  UpdateInternalCommandParams;
  inherited;
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
  // Note: ParseSQL incorrectly behaves as if its first parameter was passed
  // by reference and modifies it. So we must pass a disposable string to it.
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
    FCommand.CommandText := ExpandCommandText(FCommandText);
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

destructor TEFDBADOQuery.Destroy;
begin
  FreeAndNil(FQuery);
  FreeAndNil(FParams);
  inherited;
end;

procedure TEFDBADOQuery.ConnectionChanged;
begin
  inherited;
  FQuery.Connection := (Connection.AsObject as TEFDBADOConnection).FConnection;
  FQuery.CommandTimeout := FQuery.Connection.CommandTimeout;
end;

function TEFDBADOQuery.Execute: Integer;
begin
  inherited;
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
  // Note: ParseSQL incorrectly behaves as if its first parameter was passed
  // by reference and modifies it. So we pass a disposable string to it.
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
  FQuery.CursorLocation := clUseClient;
  FQuery.CursorType := ctStatic;
end;

procedure TEFDBADOQuery.UpdateInternalQueryCommandText;
begin
  FQuery.SQL.Text := ExpandCommandText(FCommandText);
  FQuery.Prepared := True;
end;

procedure TEFDBADOQuery.UpdateInternalQueryParams;
begin
  FParams.AssignValuesTo(FQuery.Parameters);
end;

{ TEFDBADOFactory }

function TEFDBADOAdapter.InternalCreateDBConnection: TEFDBConnection;
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
  LTableType: string;
begin
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
end;

constructor TEFDBADOInfo.Create(const AConnection: TADOConnection);
begin
  inherited Create;
  FConnection := AConnection;
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

initialization
  TEFDBAdapterRegistry.Instance.RegisterDBAdapter('ADO', TEFDBADOAdapter.Create);

finalization
  TEFDBAdapterRegistry.Instance.UnregisterDBAdapter('ADO');

end.
