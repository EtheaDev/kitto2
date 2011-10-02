unit EF.DB;

{$I EF.Defines.inc}

interface

uses
  SysUtils, DB, Classes, Generics.Collections,
  EF.Intf, EF.Classes, EF.Data;

type
  TEFDBSchemaInfo = class;

  IEFDBInfo = interface(IEFComponent)
    ['{9F973AEB-F635-4C61-89DE-D1D85A396CF1}']
    {
      Access to the metadata information on the current database's metadata.
    }
    function GetSchema: TEFDBSchemaInfo;
    property Schema: TEFDBSchemaInfo read GetSchema;
    {
      Ensures fresh metadata info is read from the database the next time Schema
      is accessed.
    }
    procedure InvalidateInfo;
  end;

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

  ///	<summary>
  ///	  A base class for database connections.
  ///	</summary>
  TEFDBConnection = class(TEFComponent)
  private
    FStandardFormatSettings: TFormatSettings;
  protected
    function GetStandardFormatSettings: TFormatSettings;
    procedure AfterConnectionOpen(Sender: TObject);
    function CreateDBCommand: IEFDBCommand; virtual; abstract;
  public
    procedure AfterConstruction; override;

    ///	<summary>
    ///	  A default implementation for IEFDBConnection's FormatValue method.
    ///	  Formats values in a way that should work for most database systems.
    ///	</summary>
    function FormatValue(const AValue: TEFDataItem): string; virtual;
  end;

  {
    A collection of named objects that implement IEFDBCommand.
    This objects owns the objects it contains, and frees them
    upon destruction.
  }
  TEFDBCommandList = class(TEFNoRefCountObject)
  private
    FCommandList: TStringList;
    function GetDBCommand(const AIndex: Integer): IEFDBCommand;
  protected
    function GetCount: Integer;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
    {
      Returns the number of objects in list.
    }
    property Count: Integer read GetCount;
    {
      Returns references to the contained objects by index.
    }
    property DBCommands[const AIndex: Integer]: IEFDBCommand read GetDBCommand; default;
    {
      Adds an object under the given name.
    }
    function Add(const AName: string; const ADBCommand: IEFDBCommand): Integer;
    {
      Removes the object, but doesn't free it.
    }
    procedure Remove(const ADBCommand: IEFDBCommand);
    {
      Returns a reference to the object with the specified name.
    }
    function GetDBCommandByName(const AName: string): IEFDBCommand;
    {
      Returns the index of the object with the specified name.
      Raises an exception if the object is not in list.
    }
    function GetDBCommandIndexByName(const AName: string): Integer;
    {
      Returns a reference to the object with the specified name.
    }
    function IndexOf(const AName: string): Integer;
    {
      Removes and frees all commands in the list.
    }
    procedure Clear;
  end;

  {
    A collection of named objects that implement IEFDBQuery.
    See TEFDBCommandList for details.
  }
  TEFDBQueryList = class(TEFDBCommandList)
  private
    function GetDBQuery(const AIndex: Integer): IEFDBQuery;
  public
    function Add(const AName: string; const ADBQuery: IEFDBQuery): Integer;
    procedure Remove(const ADBQuery: IEFDBQuery);
    property DBQueries[const AIndex: Integer]: IEFDBQuery read GetDBQuery; default;
    function GetDBQueryByName(const AName: string): IEFDBQuery;
    function GetDBQueryIndexByName(const AName: string): Integer;
  end;

  {
    Base class for a factory of EF DB connection components.
    Applications (or, more frequently, Addin packages) will define one or
    more descendant classes to use as factories for the database components.
    Each descendant factory encapsulates the creation of one particular breed
    of database connection components (dbX, ADO, and so on).
    Once a DB connection is created through a factory, use methods in the
    connection object to create other kinds of components like commands and
    queries.
  }
  TEFDBAdapter = class(TEFComponent)
  protected
    function InternalCreateDBConnection: IEFDBConnection; virtual; abstract;
    function InternalCreateDBInfo: IEFDBInfo; virtual; abstract;
  public
    function CreateDBConnection: IEFDBConnection;
    function CreateDBInfo: IEFDBInfo;
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

function TEFDBAdapter.CreateDBConnection: IEFDBConnection;
begin
  Result := InternalCreateDBConnection;
end;

function TEFDBAdapter.CreateDBInfo: IEFDBInfo;
begin
  Result := InternalCreateDBInfo;
end;

{ TEFDBCommandList }

function TEFDBCommandList.Add(const AName: string;
  const ADBCommand: IEFDBCommand): Integer;
begin
  Result := FCommandList.AddObject(AName, ADBCommand.AsObject);
end;

procedure TEFDBCommandList.AfterConstruction;
begin
  inherited;
  FCommandList := TStringList.Create;
  FCommandList.Duplicates := dupError;
  FCommandList.Sorted := True;
end;

destructor TEFDBCommandList.Destroy;
begin
  Clear;
  FreeAndNil(FCommandList);
  inherited;
end;

procedure TEFDBCommandList.Clear;
var
  LCommandIndex: Integer;
  LDBCommandIntf: IEFDBCommand;
begin
  for LCommandIndex := FCommandList.Count - 1 downto 0 do
  begin
    LDBCommandIntf := DBCommands[LCommandIndex];
    FCommandList.Delete(LCommandIndex);
    FreeAndNilEFIntf(LDBCommandIntf);
  end;
end;

function TEFDBCommandList.GetCount: Integer;
begin
  Result := FCommandList.Count;
end;

function TEFDBCommandList.GetDBCommandByName(
  const AName: string): IEFDBCommand;
begin
  Result := GetDBCommand(GetDBCommandIndexByName(AName));
end;

function TEFDBCommandList.GetDBCommandIndexByName(
  const AName: string): Integer;
begin
  Result := FCommandList.IndexOf(AName);
  if Result < 0 then
    raise EEFError.CreateFmt(_('Item %s not found.'), [AName]);
end;

function TEFDBCommandList.GetDBCommand(const AIndex: Integer): IEFDBCommand;
begin
  Result := (FCommandList.Objects[AIndex] as TEFNoRefCountObject) as IEFDBCommand;
end;

function TEFDBCommandList.IndexOf(const AName: string): Integer;
begin
  Result := FCommandList.IndexOf(AName);
end;

procedure TEFDBCommandList.Remove(const ADBCommand: IEFDBCommand);
begin
  FCommandList.Delete(FCommandList.IndexOfObject(ADBCommand.AsObject));
end;

{ TEFDBQueryList }

function TEFDBQueryList.Add(const AName: string;
  const ADBQuery: IEFDBQuery): Integer;
begin
  Result := inherited Add(AName, ADBQuery);
end;

function TEFDBQueryList.GetDBQueryByName(const AName: string): IEFDBQuery;
begin
  Result := inherited GetDBCommandByName(AName) as IEFDBQuery;
end;

function TEFDBQueryList.GetDBQueryIndexByName(const AName: string): Integer;
begin
  Result := inherited GetDBCommandIndexByName(AName);
end;

function TEFDBQueryList.GetDBQuery(const AIndex: Integer): IEFDBQuery;
begin
  Result := inherited DBCommands[AIndex] as IEFDBQuery;
end;

procedure TEFDBQueryList.Remove(const ADBQuery: IEFDBQuery);
begin
  inherited Remove(ADBQuery);
end;

{ TEFDBConnection }

procedure TEFDBConnection.AfterConnectionOpen(Sender: TObject);
var
  LCommandText: string;
  LCommand: IEFDBCommand;
begin
  LCommandText := Config.GetExpandedString('Config/AfterOpenCommandText');
  if LCommandText <> '' then
  begin
    LCommand := CreateDBCommand;
    try
      LCommand.CommandText := LCommandText;
      LCommand.Execute;
    finally
      FreeAndNilEFIntf(LCommand);
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

function TEFDBConnection.FormatValue(const AValue: TEFDataItem): string;
begin
  Assert(Assigned(AValue));

  if AValue.AsString = '' then
    Result := ''
  else
  begin
    case AValue.DataType of
      edtString:
        Result := SQLQuotedStr(AValue.AsString);
      edtInteger:
        Result := AValue.AsString;
      edtCurrency:
        Result := FormatCurr('', AValue.AsCurrency, GetStandardFormatSettings);
      edtFloat:
        Result := FormatFloat('', AValue.AsFloat, GetStandardFormatSettings);
      edtBcd:
        Result := FormatFloat('', BcdToDouble(AValue.AsBcd), GetStandardFormatSettings);
      edtBoolean:
        Result := IfThen(AValue.AsBoolean, '1', '0');
      edtDate:
        Result := SQLQuotedStr(FormatDateTime('yyyy/mm/dd', AValue.AsDate, GetStandardFormatSettings));
      edtTime:
        Result := SQLQuotedStr(FormatDateTime('hh:nn:ss', AValue.AsTime, GetStandardFormatSettings));
      edtDateTime:
        Result := SQLQuotedStr(FormatDateTime('yyyy/mm/dd hh:nn:ss', AValue.AsDate, GetStandardFormatSettings));
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

end.
