unit DBXDevartOracle;

interface

uses
  DbxDynalink,
{$IF DEFINED(CLR)}
  DBXDynalinkManaged,
{$ELSE}
  DBXDynalinkNative,
{$IFEND}
  DBXCommon, Classes, SysUtils;


type

{$IF DEFINED(CLR)}
  TDBXDevartOracleDriver = class(TDBXDynalinkDriverManaged)
{$ELSE}
  TDBXDevartOracleDriver = class(TDBXDynalinkDriverNative)
{$IFEND}
  protected
    function GetDriverLoaderClass: TDBXDynalinkDriverCommonLoaderClass; virtual;
  public
    constructor Create(DBXDriverDef: TDBXDriverDef); override;
  end;

  TDBXDevartOracleDirectDriver = class(TDBXDevartOracleDriver)
  public
    constructor Create(DBXDriverDef: TDBXDriverDef); override;
  end;

  TDBXDevartOracleProperties = class(TDBXProperties)
  public
    // cannot fill driver properties in the contructor
    // because TDBXProperties.Clone will duplicate all properties
    //constructor Create(DBXContext: TDBXContext); override;
  end;

const
  sDriverName = 'DevartOracle';
  sDriverNameDirect = 'DevartOracleDirect';  

implementation

uses
  DbxPlatform;

{ TDBXDevartOracleDriver }

constructor TDBXDevartOracleDriver.Create(DBXDriverDef: TDBXDriverDef);
begin
  inherited Create(DBXDriverDef, GetDriverLoaderClass);

  InitDriverProperties(TDBXDevartOracleProperties.Create(DBXDriverDef.FDBXContext));

  DriverProperties[TDBXPropertyNames.DriverUnit] := 'DBXDevartOracle';
  DriverProperties[TDBXPropertyNames.DriverPackageLoader] := 'TDBXDynalinkDriverLoader,DBXCommonDriver120.bpl';
  DriverProperties[TDBXPropertyNames.DriverAssemblyLoader] := 'Devart.DbxOda.DriverLoader.TCRDynalinkDriverLoader,Devart.DbxOda.DriverLoader,Version=1.0.0.1,Culture=neutral,PublicKeyToken=09af7300eec23701';
  DriverProperties[TDBXPropertyNames.MetaDataAssemblyLoader] := 'Borland.Data.TDBXOracleMetaDataCommandFactory,Borland.Data.DbxOracleDriver,Version=12.0.0.0,Culture=neutral,PublicKeyToken=' + TDBXPlatform.GetPublicKeyToken;
  DriverProperties[TDBXPropertyNames.ProductName] := 'Oracle';

  DriverProperties[TDBXPropertyNames.GetDriverFunc] := 'getSQLDriverORA';
  DriverProperties[TDBXPropertyNames.LibraryName] := 'dbexpoda40.dll';
  DriverProperties[TDBXPropertyNames.VendorLib] := 'oci.dll';

  DriverProperties[TDBXPropertyNames.Database] := 'Database Name';
  DriverProperties[TDBXPropertyNames.UserName] := 'user';
  DriverProperties[TDBXPropertyNames.Password] := 'password';

  DriverProperties[TDBXPropertyNames.MaxBlobSize] := '-1';
  DriverProperties[TDBXPropertyNames.ErrorResourceFile] := '';
  DriverProperties[TDBXDynalinkPropertyNames.LocaleCode] := '0000';
  DriverProperties[TDBXPropertyNames.IsolationLevel] := 'ReadCommited';

  DriverProperties['UseUnicode'] := 'True';
  DriverProperties['UnicodeEnvironment'] := 'True';
end;

function TDBXDevartOracleDriver.GetDriverLoaderClass: TDBXDynalinkDriverCommonLoaderClass;
begin
  Result := TDBXDynalinkDriverLoader;
end;

{ TDBXDevartOracleDirectDriver }

constructor TDBXDevartOracleDirectDriver.Create(DBXDriverDef: TDBXDriverDef);
begin
  inherited;

  DriverProperties[TDBXPropertyNames.GetDriverFunc] := 'getSQLDriverORADirect';
  DriverProperties[TDBXPropertyNames.VendorLib] := 'dbexpoda40.dll';
  DriverProperties['UnicodeEnvironment'] := 'False';
end;

initialization
  TDBXDriverRegistry.RegisterDriverClass(sDriverName, TDBXDevartOracleDriver);
  TDBXDriverRegistry.RegisterDriverClass(sDriverNameDirect, TDBXDevartOracleDirectDriver);
end.

