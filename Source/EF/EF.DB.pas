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
///	  DB-related classes. This unit defines the base classes that implement
///	  EF's abstract data access framework.
///	</summary>
unit EF.DB;

{$I EF.Defines.inc}

interface

uses
  Types, SysUtils, DB, Classes, Generics.Collections,
  EF.Intf, EF.Classes, EF.Tree;

type
  TEFDBSchemaInfo = class;

  ///	<summary>
  ///	  A base class for database metadata info, useful for reverse engineering
  ///	  of databases and generic database metadata access.
  ///	</summary>
  TEFDBInfo = class(TEFComponent)
  private
    FSchema: TEFDBSchemaInfo;
    FIsInfoFetched: Boolean;
    procedure EnsureInfo;
    procedure FetchInfo;
  protected
    procedure BeforeFetchInfo; virtual;
    function GetSchema: TEFDBSchemaInfo;
    procedure FetchTables(const ASchema: TEFDBSchemaInfo); virtual; abstract;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
    property Schema: TEFDBSchemaInfo read GetSchema;
    procedure InvalidateInfo;
  end;

  ///	<summary>
  ///	  Base class for database metadata items, such as tables, columns and
  ///	  constraints.
  ///	</summary>
  TEFDBItemInfo = class(TPersistent)
  private
    FName: string;
  public
    property Name: string read FName write FName;
  end;

  ///	<summary>
  ///	  Contains enough information to define a table's primary key. It is not
  ///	  used alone, but together with TEFDBTableInfo.
  ///	</summary>
  TEFDBPrimaryKeyInfo = class(TEFDBItemInfo)
  private
    FColumnNames: TStrings;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
    property ColumnNames: TStrings read FColumnNames;
  end;

  ///	<summary>
  ///	  Contains enough information to define a table's column. It is not used
  ///	  alone, but together with TEFDBTableInfo.
  ///	</summary>
  ///	<remarks>
  ///	  Domains are not supported. Only plain field types are recognized.
  ///	</remarks>
  TEFDBColumnInfo = class(TEFDBItemInfo)
  private
    FIsRequired: Boolean;
    FDataType: TEFDataType;
    FSize: Integer;
  public
    property DataType: TEFDataType read FDataType write FDataType;

    ///	<summary>
    ///	  For string fields, this is the length in characters; for other data
    ///	  types, it's 0.
    ///	</summary>
    property Size: Integer read FSize write FSize;
    property IsRequired: Boolean read FIsRequired write FIsRequired;
  end;

  ///	<summary>
  ///	  Contains enough information to define a table's foreign key. It is not
  ///	  used alone, but together with TEFDBTableInfo.
  ///	</summary>
  TEFDBForeignKeyInfo = class(TEFDBItemInfo)
  private
    FColumnNames: TStrings;
    FForeignColumnNames: TStrings;
    FForeignTableName: string;
    function GetColumnCount: Integer;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
    property ColumnNames: TStrings read FColumnNames;
    property ForeignTableName: string read FForeignTableName write FForeignTableName;
    property ForeignColumnNames: TStrings read FForeignColumnNames;
    property ColumnCount: Integer read GetColumnCount;
  end;

  ///	<summary>
  ///	  Contains enough information to describe a database table.
  ///	</summary>
  TEFDBTableInfo = class(TEFDBItemInfo)
  private
    FColumns: TObjectList<TEFDBColumnInfo>;
    FPrimaryKey: TEFDBPrimaryKeyInfo;
    FForeignKeys: TObjectList<TEFDBForeignKeyInfo>;
    function GetColumnCount: Integer;
    function GetColumns(const AIndex: Integer): TEFDBColumnInfo;
    function GetForeignKeys(const AIndex: Integer): TEFDBForeignKeyInfo;
    function GetForeignKeyCount: Integer;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
    property Columns[const AIndex: Integer]: TEFDBColumnInfo read GetColumns;
    property ColumnCount: Integer read GetColumnCount;
    function FindColumn(const AColumnName: string): TEFDBColumnInfo;
    function AddColumn(const AColumn: TEFDBColumnInfo): Integer;
    property PrimaryKey: TEFDBPrimaryKeyInfo read FPrimaryKey;
    property ForeignKeys[const AIndex: Integer]: TEFDBForeignKeyInfo read GetForeignKeys;
    property ForeignKeyCount: Integer read GetForeignKeyCount;
    function FindForeignKey(const AForeignKeyName: string): TEFDBForeignKeyInfo;
    function AddForeignKey(const AForeignKey: TEFDBForeignKeyInfo): Integer;
  end;

  ///	<summary>
  ///	  Contains all database schema objects.
  ///	</summary>
  TEFDBSchemaInfo = class(TEFDBItemInfo)
  private
    FTables: TObjectList<TEFDBTableInfo>;
    function GetTables(const AIndex: Integer): TEFDBTableInfo;
    function GetTableCount: Integer;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
  public
    property Tables[const AIndex: Integer]: TEFDBTableInfo read GetTables;
    property TableCount: Integer read GetTableCount;

    ///	<summary>
    ///	  Returns a reference to the table with the given name, or nil.
    ///	</summary>
    function FindTable(const ATableName: string): TEFDBTableInfo;

    ///	<summary>
    ///	  Appends a new table to the end of the list. Returns the index in the
    ///	  list of the newly added table.
    ///	</summary>
    function AddTable(const ATable: TEFDBTableInfo): Integer;

    ///	<summary>
    ///	  Deletes all tables in the list.
    ///	</summary>
    procedure ClearTables;

    ///	<summary>
    ///	  Deletes all objects. Currently it is the same as ClearTables.
    ///	</summary>
    procedure Clear;
  end;

  TEFDBConnection = class;

  ///	<summary>
  ///	  Base class for components linked to a database connection, such as
  ///	  commands and queries.
  ///	</summary>
  TEFDBComponent = class(TEFComponent)
  private
    FConnection: TEFDBConnection;
  protected
    ///	<summary>Ensures the connection is open. Descendants should call this
    ///	method before executing a command.</summary>
    procedure InternalBeforeExecute;
    function GetConnection: TEFDBConnection;
    procedure SetConnection(const AValue: TEFDBConnection);

    ///	<summary>Called whenever the connection changes. Descendants override
    ///	it to link internal components to the new connection.</summary>
    procedure ConnectionChanged; virtual;

    ///	<summary>Expands database-specific macros. Called before executing a
    ///	command.</summary>
    function ExpandCommandText(const ACommandText: string): string;
  public
    ///	<summary>
    ///	  Database connection this component is linked to.
    ///	</summary>
    property Connection: TEFDBConnection read GetConnection write SetConnection;
  end;

  ///	<summary>Base class for database commands that don't return data.</summary>
  TEFDBCommand = class(TEFDBComponent)
  protected
    function GetCommandText: string; virtual; abstract;
    procedure SetCommandText(const AValue: string); virtual; abstract;
    function GetPrepared: Boolean; virtual; abstract;
    procedure SetPrepared(const AValue: Boolean); virtual; abstract;
    function GetParams: TParams; virtual; abstract;
    procedure SetParams(const AValue: TParams); virtual; abstract;
  public
    ///	<summary>Contains the text of the statement to execute.</summary>
    property CommandText: string read GetCommandText write SetCommandText;

    ///	<summary>Manages prepared statements.</summary>
    ///	<remarks>Not all database benefit from statement preparation.</remarks>
    property Prepared: Boolean read GetPrepared write SetPrepared;

    ///	<summary>Optional param values for CommandText.</summary>
    property Params: TParams read GetParams write SetParams;

    ///	<summary>Executes the command and returns the number of affected
    ///	records, assuming the database is able to give this
    ///	information.</summary>
    function Execute: Integer; virtual;
  end;

  ///	<summary>Base class for database commands that return data.</summary>
  TEFDBQuery = class(TEFDBCommand)
  protected
    function GetMasterSource: TDataSource; virtual; abstract;
    procedure SetMasterSource(const AValue: TDataSource); virtual; abstract;
    function GetDataSet: TDataSet; virtual; abstract;
  public
    ///	<summary>Opens the DataSet and starts returning data.</summary>
    procedure Open; virtual; abstract;

    ///	<summary>Frees the memory used by the data and closes or frees the
    ///	DataSet.</summary>
    procedure Close; virtual; abstract;

    ///	<summary>Returns True if the DataSet is available.</summary>
    function IsOpen: Boolean; virtual; abstract;

    ///	<summary>Detail queries need a reference to their master query's
    ///	DataSource for parameter binding.</summary>
    property MasterSource: TDataSource read GetMasterSource write SetMasterSource;

    ///	<summary>The data buffer.</summary>
    ///	<remarks>Don't access DataSet before calling Open, or checking IsOpen:
    ///	some descendants might not make it available in advance.</remarks>
    property DataSet: TDataSet read GetDataSet;

    ///	<summary>
    ///	  Returns an array with all string values of the specified field in the
    ///	  query's dataset. The query must be open.
    ///	</summary>
    function GetFieldValuesAsStrings(const AField: TField): TStringDynArray;

    ///	<summary>
    ///	  Opens he query and returns an array of values from the first row of
    ///	  the result set, then closes the dataset. The returned array is
    ///	  zero-based and has an element for each field in the result set. If no
    ///	  rows are returned, the result is a single Null. The query may contain
    ///	  parameters, in which case they should all be assigned values before
    ///	  calling this function.
    ///	</summary>
    function SQLLookup: Variant;
  end;

  ///	<summary>Descendants of this class encapsulate differences among
  ///	different DB engines, mainly SQL dialect differences.</summary>
  TEFDBEngineType = class(TEFComponent)
  public
    ///	<summary>
    ///	  <para>Adds a limit clause to the specified SQL statement, which must
    ///	  be a select statement. The method transforms the select statement in
    ///	  a way that is compatible to what the database expects for a limited
    ///	  query. For standard-compliant databases such as Firebird you would
    ///	  add a "rows M to N" clause after the order by clase. Others will
    ///	  require some degree of query rewriting.</para>
    ///	  <para>The default implementation returns the standard-compliant
    ///	  version.</para>
    ///	  <para>If both AFrom and ATo are 0, the statement is returned
    ///	  unchanged.</para>
    ///	</summary>
    function AddLimitClause(const ACommandText: string;
      const AFrom, AFor: Integer): string; virtual;

    ///	<summary>
    ///	  <para>Expands database-specific macros. Called before executing a
    ///	  command.</para>
    ///	  <para>Supported macros:</para>
    ///	  <list type="table">
    ///	    <listheader>
    ///	      <term>Macro Name</term>
    ///	      <description>Expands to</description>
    ///	    </listheader>
    ///	    <item>
    ///	      <term>%DB.CURRENT_DATE%</term>
    ///	      <description>current_datetime, or getdate(), and so
    ///	      on.</description>
    ///	    </item>
    ///	  </list>
    ///	</summary>
    function ExpandCommandText(const ACommandText: string): string; virtual;
  end;

  TEFSQLServerDBEngineType = class(TEFDBEngineType)
  public
    function AddLimitClause(const ACommandText: string; const AFrom: Integer;
      const AFor: Integer): string; override;
    function ExpandCommandText(const ACommandText: string): string; override;
  end;

  ///	<summary>
  ///	  A base class for database connections.
  ///	</summary>
  TEFDBConnection = class(TEFComponent)
  private
    FDBEngineType: TEFDBEngineType;
    FStandardFormatSettings: TFormatSettings;
    function GetDBEngineType: TEFDBEngineType;
  protected
    function GetStandardFormatSettings: TFormatSettings;
    procedure AfterConnectionOpen(Sender: TObject);
    function CreateDBEngineType: TEFDBEngineType; virtual;
    procedure InternalOpen; virtual; abstract;
    procedure InternalClose; virtual; abstract;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
  public
    property DBEngineType: TEFDBEngineType read GetDBEngineType;

    ///	<summary>Connects to the database.</summary>
    procedure Open; virtual;

    ///	<summary>Closes the connection to the database. As a result, open
    ///	datasets might get closed as well or not, depending on the
    ///	implementation.</summary>
    procedure Close; virtual;

    ///	<summary>Returns True if the connection to the database is open, False
    ///	otherwise.</summary>
    function IsOpen: Boolean; virtual; abstract;

    ///	<summary>Executes a command and returns the number of affected records
    ///	(if the database is able to return it).</summary>
    ///	<remarks>Descendants may either make use of predefined functionality in
    ///	a given database library, or create an instance of a command class, set
    ///	its CommandText and Execute the command, then destroy the object and
    ///	return.</remarks>
    function ExecuteImmediate(const AStatement: string): Integer; virtual; abstract;

    ///	<summary>Starts a new transaction.</summary>
    procedure StartTransaction; virtual; abstract;

    ///	<summary>Commits and ends a previously started transaction.</summary>
    procedure CommitTransaction; virtual; abstract;

    ///	<summary>Rollbacks and ends a previously started transaction.</summary>
    procedure RollbackTransaction; virtual; abstract;

    ///	<summary>Tells whether a transaction was started or not.</summary>
    function IsInTransaction: Boolean; virtual; abstract;

    ///	<summary>Fetches and returns a nEF sequence generator value. Use only
    ///	with databases that support sequence generators (IB/Fb,	Oracle).</summary>
    function FetchSequenceGeneratorValue(const ASequenceName: string): Int64; virtual; abstract;

    ///	<summary>Returns the last generated auto-inc value for a given table
    ///	(or globally, if the database doesn't support getting auto-inc values
    ///	per table).</summary>
    ///  <remarks>Use only with databases that support auto-inc semantics,
    /// and according to the particular database specification.</remarks>
    function GetLastAutoincValue(const ATableName: string = ''): Int64; virtual; abstract;

    ///	<summary>Creates and returns an instance of the concrete command class,
    /// linked to this connection.</summary>
    function CreateDBCommand: TEFDBCommand; virtual; abstract;

    ///	<summary>Creates and returns an instance of the concrete query class,
    /// linked to this connection.</summary>
    function CreateDBQuery: TEFDBQuery; virtual; abstract;

    ///	<summary>
    ///	  Returns the value of the first column of the first record of the
    ///	  cursor returned by ASQLStatement. It is advised to pass a
    ///	  single-column singleton SQL statement for efficiency reasons. May
    ///	  return an empty or unassigned Variant.
    ///	</summary>
    function GetSingletonValue(const ASQLStatement: string): Variant;

    ///	<summary>
    ///	  Executes the given SQL statement and returns an array of values from
    ///	  the first row of the result set. The returned array is zero-based and
    ///	  has an element for each field in the source select statement. If no
    ///	  rows are returned, the result is a single Null. The query may contain
    ///	  parameters, in which case you are required to pass ADBParams.
    ///	</summary>
    function SQLLookup(const ASQLStatement: string; const ADBParams: TParams = nil): Variant;
  end;

  ///	<summary>
  ///	  Base class for a data access adapter. Descendants encapsulate specific
  ///	  data access libraries or databases.
  ///	</summary>
  ///	<remarks>
  ///	  Once a DB connection is created through an adapter, use methods in the
  ///	  connection object to create other kinds of components like commands and
  ///	  queries.
  ///	</remarks>
  TEFDBAdapter = class(TEFComponent)
  protected
    function InternalCreateDBConnection: TEFDBConnection; virtual; abstract;
    function InternalCreateDBInfo: TEFDBInfo; virtual; abstract;
  public
    function CreateDBConnection: TEFDBConnection;
    function CreateDBInfo: TEFDBInfo;
  end;
  TEFDBAdapterClass = class of TEFDBAdapter;

  ///	<summary>
  ///	  A registry of TEFDBAdapter objects. Gives access to the DB adapters.
  ///	</summary>
  TEFDBAdapterRegistry = class
  private
    FAdapters: TDictionary<string, TEFDBAdapter>;
    FDefaultAdapter: TEFDBAdapter;
    class var FInstance: TEFDBAdapterRegistry;
    class function GetInstance: TEFDBAdapterRegistry; static;
    function GetDBAdapter(const AId: string): TEFDBAdapter;
    procedure AdaptersValueNotify(Sender: TObject; const Item: TEFDBAdapter;
      Action: TCollectionNotification);
  public
    class destructor Destroy;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    procedure RegisterDBAdapter(const AId: string; const ADBAdapter: TEFDBAdapter);
    procedure UnregisterDBAdapter(const AId: string);

    ///	<summary>
    ///	  Returns an adapter by its Id. If you pass '' and only one adapter is
    ///	  registered, then the method returns a reference to that adapter,
    ///	  otherwise an exception is raised.
    ///	</summary>
    property DBAdapters[const AId: string]: TEFDBAdapter read GetDBAdapter; default;

    class property Instance: TEFDBAdapterRegistry read GetInstance;
  end;

implementation

uses
  Variants, Contnrs, FMTBcd, StrUtils,
  EF.SysUtils, EF.Localization, EF.Types, EF.SQL, EF.StrUtils, EF.Logger;

{ TEFDBAdapterRegistry }

class function TEFDBAdapterRegistry.GetInstance: TEFDBAdapterRegistry;
begin
  if not Assigned(FInstance) then
    FInstance := TEFDBAdapterRegistry.Create;
  Result := FInstance;
end;

procedure TEFDBAdapterRegistry.AfterConstruction;
begin
  inherited;
  FAdapters := TDictionary<string, TEFDBAdapter>.Create;
  FAdapters.OnValueNotify := AdaptersValueNotify;
end;

procedure TEFDBAdapterRegistry.AdaptersValueNotify(Sender: TObject;
  const Item: TEFDBAdapter; Action: TCollectionNotification);
begin
  if Action = cnRemoved then
    Item.Free;
end;

procedure TEFDBAdapterRegistry.BeforeDestruction;
var
  LAdapter: TEFDBAdapter;
begin
  inherited;
  for LAdapter in FAdapters.Values do
    LAdapter.Free;
  FAdapters.Clear;
  FreeAndNil(FAdapters);
end;

class destructor TEFDBAdapterRegistry.Destroy;
begin
  FreeAndNil(FInstance);
end;

function TEFDBAdapterRegistry.GetDBAdapter(const AId: string): TEFDBAdapter;
begin
  if (AId = '') and (FDefaultAdapter <> nil) then
    Result := FDefaultAdapter
  else
    Result := FAdapters[AId];
end;

procedure TEFDBAdapterRegistry.RegisterDBAdapter(const AId: string;
  const ADBAdapter: TEFDBAdapter);
begin
  FAdapters.Add(AId, ADBAdapter);
  if FDefaultAdapter = nil then
    FDefaultAdapter := ADBAdapter;
end;

procedure TEFDBAdapterRegistry.UnregisterDBAdapter(const AId: string);
begin
  FAdapters.Remove(AId);
  if not FAdapters.ContainsValue(FDefaultAdapter) then
    FDefaultAdapter := nil;
end;

{ TEFDBAdapter }

function TEFDBAdapter.CreateDBConnection: TEFDBConnection;
begin
  Result := InternalCreateDBConnection;
end;

function TEFDBAdapter.CreateDBInfo: TEFDBInfo;
begin
  Result := InternalCreateDBInfo;
end;

{ TEFDBConnection }

procedure TEFDBConnection.AfterConnectionOpen(Sender: TObject);
var
  LCommandText: string;
  LCommand: TEFDBCommand;
begin
  LCommandText := Config.GetExpandedString('Config/AfterOpenCommandText');
  if LCommandText <> '' then
  begin
    LCommand := CreateDBCommand;
    try
      LCommand.CommandText := LCommandText;
      LCommand.Execute;
    finally
      FreeAndNil(LCommand);
    end;
  end;
end;

procedure TEFDBConnection.AfterConstruction;
begin
  inherited;
  FStandardFormatSettings := GetFormatSettings;
  FStandardFormatSettings.DecimalSeparator := '.';
  FStandardFormatSettings.DateSeparator := '-';
  FStandardFormatSettings.TimeSeparator := ':';
end;

destructor TEFDBConnection.Destroy;
begin
  if IsOpen then
    Close;
  FreeAndNil(FDBEngineType);
  inherited;
end;

function TEFDBConnection.GetDBEngineType: TEFDBEngineType;
begin
  if not IsOpen then
    Open;
  if not Assigned(FDBEngineType) then
    FDBEngineType := CreateDBEngineType;
  Result := FDBEngineType;
end;

procedure TEFDBConnection.Close;
begin
  TEFLogger.Instance.Log('Closing DB connection.', TEFLogger.LOG_MEDIUM);
  InternalClose;
  FreeAndNil(FDBEngineType);
end;

function TEFDBConnection.CreateDBEngineType: TEFDBEngineType;
begin
  Result := TEFDBEngineType.Create;
end;

function TEFDBConnection.GetSingletonValue(
  const ASQLStatement: string): Variant;
var
  LQuery: TEFDBQuery;
begin
  Assert(ASQLStatement <> '');

  LQuery := CreateDBQuery;
  try
    LQuery.CommandText := ASQLStatement;
    LQuery.Open;
    try
      if LQuery.DataSet.IsEmpty then
        Result := Null
      else
        Result := LQuery.DataSet.Fields[0].Value;
    finally
      LQuery.Close;
    end;
  finally
    FreeAndNil(LQuery);
  end;
end;

function TEFDBConnection.SQLLookup(const ASQLStatement: string;
  const ADBParams: TParams): Variant;
var
  LQuery: TEFDBQuery;
begin
  LQuery := CreateDBQuery;
  try
    LQuery.CommandText := ASQLStatement;
    if Assigned(ADBParams) then
      LQuery.Params.AssignValues(ADBParams);
    Result := LQuery.SQLLookup;
  finally
    FreeAndNil(LQuery);
  end;
end;

function TEFDBConnection.GetStandardFormatSettings: TFormatSettings;
begin
  Result := FStandardFormatSettings;
end;

procedure TEFDBConnection.Open;
begin
  TEFLogger.Instance.Log('Opening DB connection.', TEFLogger.LOG_MEDIUM);
  InternalOpen;
end;

{ TEFDBInfo }

procedure TEFDBInfo.AfterConstruction;
begin
  inherited;
  FSchema := TEFDBSchemaInfo.Create;
end;

procedure TEFDBInfo.BeforeFetchInfo;
begin
end;

destructor TEFDBInfo.Destroy;
begin
  FreeAndNil(FSchema);
  inherited;
end;

procedure TEFDBInfo.EnsureInfo;
begin
  if not FIsInfoFetched then
  begin
    FetchInfo;
    FIsInfoFetched := True;
  end;
end;

procedure TEFDBInfo.FetchInfo;
begin
  BeforeFetchInfo;
  FSchema.ClearTables;
  FetchTables(FSchema);
end;

function TEFDBInfo.GetSchema: TEFDBSchemaInfo;
begin
  EnsureInfo;
  Result := FSchema;
end;

procedure TEFDBInfo.InvalidateInfo;
begin
  FIsInfoFetched := False;
end;

{ TEFDBTableInfo }

function TEFDBTableInfo.AddColumn(const AColumn: TEFDBColumnInfo): Integer;
begin
  Result := FColumns.Add(AColumn);
end;

function TEFDBTableInfo.AddForeignKey(
  const AForeignKey: TEFDBForeignKeyInfo): Integer;
begin
  Result := FForeignKeys.Add(AForeignKey);
end;

procedure TEFDBTableInfo.AfterConstruction;
begin
  inherited;
  FColumns := TObjectList<TEFDBColumnInfo>.Create(True);
  FPrimaryKey := TEFDBPrimaryKeyInfo.Create;
  FForeignKeys := TObjectList<TEFDBForeignKeyInfo>.Create(True);
end;

destructor TEFDBTableInfo.Destroy;
begin
  FreeAndNil(FColumns);
  FreeAndNil(FPrimaryKey);
  FreeAndNil(FForeignKeys);
  inherited;
end;

function TEFDBTableInfo.FindColumn(const AColumnName: string): TEFDBColumnInfo;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FColumns.Count - 1 do
  begin
    if SameText(FColumns[I].Name, AColumnName) then
    begin
      Result := FColumns[I];
      Break;
    end;
  end;
end;

function TEFDBTableInfo.FindForeignKey(
  const AForeignKeyName: string): TEFDBForeignKeyInfo;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FForeignKeys.Count - 1 do
  begin
    if SameText(FForeignKeys[I].Name, AForeignKeyName) then
    begin
      Result := FForeignKeys[I];
      Break;
    end;
  end;
end;

function TEFDBTableInfo.GetColumnCount: Integer;
begin
  Result := FColumns.Count;
end;

function TEFDBTableInfo.GetColumns(const AIndex: Integer): TEFDBColumnInfo;
begin
  Result := FColumns[AIndex];
end;

function TEFDBTableInfo.GetForeignKeyCount: Integer;
begin
  Result := FForeignKeys.Count;
end;

function TEFDBTableInfo.GetForeignKeys(
  const AIndex: Integer): TEFDBForeignKeyInfo;
begin
  Result := FForeignKeys[AIndex];
end;

{ TEFDBPrimaryKeyInfo }

procedure TEFDBPrimaryKeyInfo.AfterConstruction;
begin
  inherited;
  FColumnNames := TStringList.Create;
end;

destructor TEFDBPrimaryKeyInfo.Destroy;
begin
  FreeAndNil(FColumnNames);
  inherited;
end;

{ TEFDBForeignKeyInfo }

procedure TEFDBForeignKeyInfo.AfterConstruction;
begin
  inherited;
  FColumnNames := TStringList.Create;
  FForeignColumnNames := TStringList.Create;
end;

destructor TEFDBForeignKeyInfo.Destroy;
begin
  FreeAndNil(FColumnNames);
  FreeAndNil(FForeignColumnNames);
  inherited;
end;

function TEFDBForeignKeyInfo.GetColumnCount: Integer;
begin
  Result := FColumnNames.Count;
end;

{ TEFDBSchemaInfo }

function TEFDBSchemaInfo.AddTable(const ATable: TEFDBTableInfo): Integer;
begin
  Result := FTables.Add(ATable);
end;

procedure TEFDBSchemaInfo.AfterConstruction;
begin
  inherited;
  FTables := TObjectList<TEFDBTableInfo>.Create(True);
end;

procedure TEFDBSchemaInfo.Clear;
begin
  ClearTables;
end;

procedure TEFDBSchemaInfo.ClearTables;
begin
  FTables.Clear;
end;

destructor TEFDBSchemaInfo.Destroy;
begin
  FreeAndNil(FTables);
  inherited;
end;

function TEFDBSchemaInfo.FindTable(const ATableName: string): TEFDBTableInfo;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to TableCount - 1 do
  begin
    if SameText(Tables[I].Name, ATableName) then
    begin
      Result := Tables[I];
      Break;
    end;
  end;
end;

function TEFDBSchemaInfo.GetTableCount: Integer;
begin
  Result := FTables.Count;
end;

function TEFDBSchemaInfo.GetTables(const AIndex: Integer): TEFDBTableInfo;
begin
  Result := FTables[AIndex];
end;

{ TEFDBComponent }

procedure TEFDBComponent.ConnectionChanged;
begin
end;

function TEFDBComponent.ExpandCommandText(const ACommandText: string): string;
begin
  if Assigned(Connection) then
    Result := Connection.DBEngineType.ExpandCommandText(ACommandText)
  else
    Result := ACommandText;
end;

function TEFDBComponent.GetConnection: TEFDBConnection;
begin
  Result := FConnection;
end;

procedure TEFDBComponent.InternalBeforeExecute;
begin
  Assert(Assigned(FConnection));

  if not FConnection.IsOpen then
    FConnection.Open;
end;

procedure TEFDBComponent.SetConnection(const AValue: TEFDBConnection);
begin
  FConnection := AValue;
  ConnectionChanged;
end;

{ TEFDBCommand }

function TEFDBCommand.Execute: Integer;
begin
  InternalBeforeExecute;
  Result := 0;
end;

{ TEFDBQuery }

function TEFDBQuery.GetFieldValuesAsStrings(const AField: TField): TStringDynArray;
var
  LBookmark: TBookmark;
  LString: string;
begin
  Assert(Assigned(AField));
  Assert(AField.DataSet = DataSet);

  LBookmark := AField.DataSet.Bookmark;
  try
    LString := '';
    AField.DataSet.First;
    while not AField.DataSet.Eof do
    begin
      if LString = '' then
        LString := AField.AsString
      else
        LString := LString + '§' + AField.AsString;
      AField.DataSet.Next;
    end;
    Result := Split(LString, '§');
  finally
    AField.DataSet.Bookmark := LBookmark;
  end;
end;

function TEFDBQuery.SQLLookup: Variant;
var
  LFieldIndex: Integer;
begin
  Open;
  try
    if DataSet.IsEmpty then
      Result := Null
    else
    begin
      Result := VarArrayCreate([0, DataSet.FieldCount - 1], varVariant);
      for LFieldIndex := 0 to DataSet.FieldCount - 1 do
        Result[LFieldIndex] := DataSet.Fields[LFieldIndex].Value;
    end;
  finally
    Close;
  end;
end;

{ TEFDBEngineType }

function TEFDBEngineType.AddLimitClause(const ACommandText: string; const AFrom,
  AFor: Integer): string;
var
  LOrderByClause: string;
begin
  Result := ACommandText;
  if (AFrom <> 0) or (AFor <> 0) then
  begin
    LOrderByClause := GetSQLOrderByClause(ACommandText);
    if LOrderByClause <> '' then
      Result := SetSQLOrderByClause(ACommandText, LOrderByClause + ' ' +
        Format(' rows %d to %d', [AFrom + 1, AFrom + 1 + AFor - 1]))
    else
      raise EEFError.Create('Cannot add limit clause without order by clause.');
  end
  else
    Result := ACommandText;
end;

function TEFDBEngineType.ExpandCommandText(const ACommandText: string): string;
begin
  Result := ReplaceText(ACommandText, '%DB.CURRENT_DATE%', 'current_date');
end;

{ TEFSQLServerDBEngineType }

function TEFSQLServerDBEngineType.AddLimitClause(const ACommandText: string;
  const AFrom, AFor: Integer): string;
var
  LSelectClause: string;
  LOrderByClause: string;
  LFromClause: string;
  LWhereClause: string;
begin
  if (AFrom <> 0) or (AFor <> 0) then
  begin
    LSelectClause := GetSQLSelectClause(ACommandText);
    LFromClause := GetSQLFromClause(ACommandText);
    LWhereClause := GetSQLWhereClause(ACommandText);
    if LWhereClause <> '' then
      LWhereClause := ' where ' + LWhereClause;
    LOrderByClause := GetSQLOrderByClause(ACommandText);
    if LOrderByClause <> '' then
      LOrderByClause := ' order by ' + LOrderByClause
    else
      raise EEFError.Create('Cannot add limit clause without order by clause.');
{ TODO :
Don't select the __ROWNUM field to save bandwidth?
Select clause massaging would be required. }
    Result := Format('select * from (select %s, row_number() over (%s) as __ROWNUM ' +
      'from %s%s) as __OUTER where __OUTER.__ROWNUM between %d and %d',
      [LSelectClause, LOrderByClause, LFromClause, LWhereClause, AFrom + 1, AFrom + 1 + AFor - 1]);
  end
  else
    Result := inherited AddLimitClause(ACommandText, AFrom, AFor);
end;

function TEFSQLServerDBEngineType.ExpandCommandText(const ACommandText: string): string;
begin
  Result := ReplaceText(inherited ExpandCommandText(ACommandText), '%DB.CURRENT_DATE%', 'getdate()');
end;

end.
