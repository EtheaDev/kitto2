unit DBXDevartSQLServer;

interface

uses
  Classes, SysUtils, DbxDynalink,
{$IF DEFINED(CLR)}
  DBXDynalinkManaged,
{$ELSE}
  DBXDynalinkNative,
{$IFEND}
  DBXCommon;

const
  sDriverName = 'DevartSQLServer';
  sDriverNameCompact = 'DevartSQLServerCompact';

  sEnableBCD = 'EnableBCD';
  sFetchAll = 'FetchAll';
  sLongStrings = 'LongStrings';
  sUseUnicode = 'UseUnicode';

type
{$IF DEFINED(CLR)}
  TDBXDevartSQLServerDriver = class(TDBXDynalinkDriverManaged)
{$ELSE}
  TDBXDevartSQLServerDriver = class(TDBXDynalinkDriverNative)
{$IFEND}
  protected
    function GetDriverLoaderClass: TDBXDynalinkDriverCommonLoaderClass; virtual;
  public
    constructor Create(DBXDriverDef: TDBXDriverDef); override;
  end;

  TDBXDevartSQLServerCompactDriver = class(TDBXDevartSQLServerDriver)
  public
    constructor Create(DBXDriverDef: TDBXDriverDef); override;
  end;

  TDBXDevartSQLServerProperties = class(TDBXProperties)
  strict private
    function GetHostName: string;
    procedure SetHostName(const Value: string);
    function GetDataBase: string;
    procedure SetDataBase(const Value: string);
    function GetUserName: string;
    procedure SetUserName(const Value: string);
    function GetPassword: string;
    procedure SetPassword(const Value: string);

    function GetBlobSize: Integer;
    procedure SetBlobSize(const Value: Integer);
    function GetLongStrings: Boolean;
    procedure SetLongStrings(const Value: Boolean);
    function GetEnableBCD: Boolean;
    procedure SetEnableBCD(const Value: Boolean);
    function GetFetchAll: Boolean;
    procedure SetFetchAll(const Value: Boolean);

    function GetUseUnicode: Boolean;
    procedure SetUseUnicode(const Value: Boolean);

  public
    // cannot fill driver properties in the constructor
    // because TDBXProperties.Clone will duplicate all properties
    //constructor Create(DBXContext: TDBXContext); override;

  published
    property HostName: string read GetHostName write SetHostName;
    property DataBase: string read GetDataBase write SetDataBase;
    property UserName: string read GetUserName write SetUserName;
    property Password: string read GetPassword write SetPassword;

    property BlobSize: Integer read GetBlobSize write SetBlobSize;
    property LongStrings: Boolean read GetLongStrings write SetLongStrings;
    property EnableBCD: Boolean read GetEnableBCD write SetEnableBCD;
    property FetchAll: Boolean read GetFetchAll write SetFetchAll;

    property UseUnicode: Boolean read GetUseUnicode write SetUseUnicode;
  end;

implementation

uses
  DbxPlatform;

{ TDBXSQLServerDevartDriver }

constructor TDBXDevartSQLServerDriver.Create(DBXDriverDef: TDBXDriverDef);
begin
  inherited Create(DBXDriverDef, GetDriverLoaderClass);

  InitDriverProperties(TDBXDevartSQLServerProperties.Create(DBXDriverDef.FDBXContext));

  DriverProperties[TDBXPropertyNames.SchemaOverride] := '%.dbo';
  DriverProperties[TDBXPropertyNames.DriverUnit] := 'DBXDevartSQLServer';
  DriverProperties[TDBXPropertyNames.DriverPackageLoader] := 'TDBXDynalinkDriverLoader,DBXCommonDriver120.bpl';
  DriverProperties[TDBXPropertyNames.DriverAssemblyLoader] := 'Devart.DbxSda.DriverLoader.TCRDynalinkDriverLoader,Devart.DbxSda.DriverLoader,Version=1.0.0.5,Culture=neutral,PublicKeyToken=09af7300eec23701';
  DriverProperties[TDBXPropertyNames.MetaDataAssemblyLoader] := 'Borland.Data.TDBXMSSQLMetaDataCommandFactory,Borland.Data.DbxMSSQLDriver,Version=12.0.0.0,Culture=neutral,PublicKeyToken=' + TDBXPlatform.GetPublicKeyToken;
  DriverProperties[TDBXPropertyNames.ProductName] := 'MSSQL';

  DriverProperties[TDBXPropertyNames.GetDriverFunc] := 'getSQLDriverSQLServer';
  DriverProperties[TDBXPropertyNames.LibraryName] := 'dbexpsda40.dll';
  DriverProperties[TDBXPropertyNames.VendorLib] := 'sqloledb.dll';

  DriverProperties[TDBXPropertyNames.HostName] := '';
  DriverProperties[TDBXPropertyNames.Database] := '';
  DriverProperties[TDBXPropertyNames.UserName] := '';
  DriverProperties[TDBXPropertyNames.Password] := '';

  DriverProperties[TDBXPropertyNames.ErrorResourceFile] := '';
  DriverProperties[TDBXDynalinkPropertyNames.LocaleCode] := '0000';
  DriverProperties[TDBXPropertyNames.IsolationLevel] := 'ReadCommited';

  DriverProperties[TDBXPropertyNames.MaxBlobSize] := '-1';
  DriverProperties[sLongStrings] := 'True';
  DriverProperties[sEnableBCD] := 'True';
  DriverProperties[sFetchAll] := 'True';

  DriverProperties[sUseUnicode] := 'True';
end;

function TDBXDevartSQLServerDriver.GetDriverLoaderClass: TDBXDynalinkDriverCommonLoaderClass;
begin
  Result := TDBXDynalinkDriverLoader;
end;

{ TDBXDevartSQLServerCompactDriver  }

constructor TDBXDevartSQLServerCompactDriver.Create(DBXDriverDef: TDBXDriverDef);
begin
  inherited;

  DriverProperties[TDBXPropertyNames.GetDriverFunc] := 'getSQLDriverSQLServerCompact';
  DriverProperties[TDBXPropertyNames.VendorLib] := 'sqlceoledb30.dll';
end;

{ TDBXDevartSQLServerProperties }

function TDBXDevartSQLServerProperties.GetHostName: string;
begin
  Result := Values[TDBXPropertyNames.HostName];
end;

procedure TDBXDevartSQLServerProperties.SetHostName(const Value: string);
begin
  Values[TDBXPropertyNames.HostName] := Value;
end;

function TDBXDevartSQLServerProperties.GetDataBase: string;
begin
  Result := Values[TDBXPropertyNames.Database];
end;

procedure TDBXDevartSQLServerProperties.SetDataBase(const Value: string);
begin
  Values[TDBXPropertyNames.Database] := Value;
end;

function TDBXDevartSQLServerProperties.GetUserName: string;
begin
  Result := Values[TDBXPropertyNames.UserName];
end;

procedure TDBXDevartSQLServerProperties.SetUserName(const Value: string);
begin
  Values[TDBXPropertyNames.UserName] := Value;
end;

function TDBXDevartSQLServerProperties.GetPassword: string;
begin
  Result := Values[TDBXPropertyNames.Password];
end;

procedure TDBXDevartSQLServerProperties.SetPassword(const Value: string);
begin
  Values[TDBXPropertyNames.Password] := Value;
end;

function TDBXDevartSQLServerProperties.GetBlobSize: Integer;
begin
  Result := StrToIntDef(Values[TDBXPropertyNames.MaxBlobSize], -1);
end;

procedure TDBXDevartSQLServerProperties.SetBlobSize(const Value: Integer);
begin
  Values[TDBXPropertyNames.MaxBlobSize] := IntToStr(Value);
end;

function TDBXDevartSQLServerProperties.GetEnableBCD: Boolean;
begin
  Result := StrToBoolDef(Values[sEnableBCD], True);
end;

function TDBXDevartSQLServerProperties.GetLongStrings: Boolean;
begin
  Result := StrToBoolDef(Values[sLongStrings], True);
end;

procedure TDBXDevartSQLServerProperties.SetLongStrings(const Value: Boolean);
begin
  Values[sLongStrings] := BoolToStr(Value, True);
end;

procedure TDBXDevartSQLServerProperties.SetEnableBCD(const Value: Boolean);
begin
  Values[sEnableBCD] := BoolToStr(Value, True);
end;

function TDBXDevartSQLServerProperties.GetFetchAll: Boolean;
begin
  Result := StrToBoolDef(Values[sFetchAll], True);
end;

procedure TDBXDevartSQLServerProperties.SetFetchAll(const Value: Boolean);
begin
  Values[sFetchAll] := BoolToStr(Value, True);
end;

function TDBXDevartSQLServerProperties.GetUseUnicode: Boolean;
begin
  Result := StrToBoolDef(Values[sUseUnicode], True);
end;

procedure TDBXDevartSQLServerProperties.SetUseUnicode(const Value: Boolean);
begin
  Values[sUseUnicode] := BoolToStr(Value, True);
end;

initialization
  TDBXDriverRegistry.RegisterDriverClass(sDriverName, TDBXDevartSQLServerDriver);
  TDBXDriverRegistry.RegisterDriverClass(sDriverNameCompact, TDBXDevartSQLServerCompactDriver);
end.

