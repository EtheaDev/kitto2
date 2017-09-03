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
  public
    // cannot fill driver properties in the constructor
    // because TDBXProperties.Clone will duplicate all properties
    //constructor Create(DBXContext: TDBXContext); override;
  end;

const
  sDriverName = 'DevartSQLServer';
  sDriverNameCompact = 'DevartSQLServerCompact';

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

  DriverProperties[TDBXPropertyNames.MaxBlobSize] := '-1';
  DriverProperties[TDBXPropertyNames.ErrorResourceFile] := '';
  DriverProperties[TDBXDynalinkPropertyNames.LocaleCode] := '0000';
  DriverProperties[TDBXPropertyNames.IsolationLevel] := 'ReadCommited';
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

initialization
  TDBXDriverRegistry.RegisterDriverClass(sDriverName, TDBXDevartSQLServerDriver);
  TDBXDriverRegistry.RegisterDriverClass(sDriverNameCompact, TDBXDevartSQLServerCompactDriver);
end.

