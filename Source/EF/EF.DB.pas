unit EF.DB;

{$I EF.Defines.inc}

interface

uses
  SysUtils, DB, Classes, Generics.Collections,
  EF.Intf, EF.Classes, EF.Tree;

type
  TEFDBSchemaInfo = class;

  ///	<summary>
  ///	  A base class for database metadata info providers.
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

  {
    Base class for database metadata items, such as tables, columns and
    constraints.
  }
  TEFDBItemInfo = class(TPersistent)
  private
    FName: string;
  public
    property Name: string read FName write FName;
  end;

  {
    Contains enough information to define a table's primary key.
    It is not used alone, but together with TEFDBTableInfo.
  }
  TEFDBPrimaryKeyInfo = class(TEFDBItemInfo)
  private
    FColumnNames: TStrings;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
    property ColumnNames: TStrings read FColumnNames;
  end;

  {
    Contains enough information to define a table's column.
    It is not used alone, but together with TEFDBTableInfo.

    Note: domains are not yet supported.
  }
  TEFDBColumnInfo = class(TEFDBItemInfo)
  private
    FIsRequired: Boolean;
    FDataType: TEFDataType;
    FSize: Integer;
  public
    property DataType: TEFDataType read FDataType write FDataType;
    {
      For string fields, this is the length in characters; for other data types,
      it's 0.
    }
    property Size: Integer read FSize write FSize;
    property IsRequired: Boolean read FIsRequired write FIsRequired;
  end;

  {
    Contains enough information to define a table's foreign key.
    It is not used alone, but together with TEFDBTableInfo.
  }
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

  {
    Contains enough information to define a database table.
  }
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

  {
    Contains all database schema objects.
  }
  TEFDBSchemaInfo = class(TEFDBItemInfo)
  private
    FTables: TObjectList<TEFDBTableInfo>;
    function GetTables(const AIndex: Integer): TEFDBTableInfo;
    function GetTableCount: Integer;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
    {
      Indexed access to  the list of tables.
    }
    property Tables[const AIndex: Integer]: TEFDBTableInfo read GetTables;
    {
      Number of tables in list.
    }
    property TableCount: Integer read GetTableCount;
    {
      Returns a reference to the table with the given name, or nil.
    }
    function FindTable(const ATableName: string): TEFDBTableInfo;
    {
      Appends a nEF table to the end of the list. Returns the index
      in the list of the nEFly added table.
    }
    function AddTable(const ATable: TEFDBTableInfo): Integer;
    {
      Deletes all tables in list.
    }
    procedure ClearTables;
    {
      Deletes all objects. Currently it is the same as ClearTables.
    }
    procedure Clear;
  end;

  TEFDBConnection = class;

  ///	<summary>Base class for components linked to a database
  ///	connection.</summary>
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
  public
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
  end;

  ///	<summary>
  ///	  A base class for database connections.
  ///	</summary>
  TEFDBConnection = class(TEFComponent)
  private
    FStandardFormatSettings: TFormatSettings;
  protected
    function GetStandardFormatSettings: TFormatSettings;
    procedure AfterConnectionOpen(Sender: TObject);
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
  public
    ///	<summary>Connects to the database.</summary>
    procedure Open; virtual; abstract;

    ///	<summary>Closes the connection to the database. As a result, open
    ///	datasets might get closed as well or not, depending on the
    ///	implementation.</summary>
    procedure Close; virtual; abstract;

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

    ///	<summary>Formats the specified value according to the database rules
    ///	for SQL. The value formatted in this way can be used in queries. This
    ///	method should use the passed node's DataType to decide how to format
    ///	the value, which it then returns as a string.</summary>
    function FormatValue(const AValue: TEFNode): string; virtual;
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
  Contnrs, FMTBcd, StrUtils,
  EF.SysUtils, EF.Localization, EF.Types, EF.SQL;

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
  I: Integer;
begin
  inherited;
  for I := FAdapters.Count - 1 downto 0 do
    FAdapters.Values.ToArray[I].Free;
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
  inherited;
end;

function TEFDBConnection.FormatValue(const AValue: TEFNode): string;
begin
  Assert(Assigned(AValue));

  if AValue.AsString = '' then
    Result := ''
  else
  begin
{ TODO : implement data types }
    case AValue.DataType of
      edtString:
        Result := SQLQuotedStr(AValue.AsString);
      edtInteger:
        Result := AValue.AsString;
      //edtCurrency:
        //Result := FormatCurr('', AValue.AsCurrency, GetStandardFormatSettings);
      //edtFloat:
        //Result := FormatFloat('', AValue.AsFloat, GetStandardFormatSettings);
      //edtBcd:
        //Result := FormatFloat('', BcdToDouble(AValue.AsBcd), GetStandardFormatSettings);
      edtBoolean:
        Result := IfThen(AValue.AsBoolean, '1', '0');
      //edtDate:
        //Result := SQLQuotedStr(FormatDateTime('yyyy/mm/dd', AValue.AsDate, GetStandardFormatSettings));
      //edtTime:
        //Result := SQLQuotedStr(FormatDateTime('hh:nn:ss', AValue.AsTime, GetStandardFormatSettings));
      //edtDateTime:
        //Result := SQLQuotedStr(FormatDateTime('yyyy/mm/dd hh:nn:ss', AValue.AsDateTime, GetStandardFormatSettings));
    else
      Result := SQLQuotedStr(AValue.AsString);
    end;
  end;
end;

function TEFDBConnection.GetStandardFormatSettings: TFormatSettings;
begin
  Result := FStandardFormatSettings;
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

end.
