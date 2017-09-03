unit DBXDevartInterBase;

interface

uses
  SysUtils, Classes, 
  DbxDynalink,
{$IF DEFINED(CLR)}
  DBXDynalinkManaged,
{$ELSE}
  DBXDynalinkNative,
{$IFEND}
  DBXCommon;

const
  sDriverName = 'DevartInterBase';

  sCharLength = 'CharLength';
  sCharset = 'Charset';
  sEnableBCD = 'EnableBCD';
  sFetchAll = 'FetchAll';
  sInterBaseTransIsolation = 'DevartInterBase TransIsolation';
  sLongStrings = 'LongStrings';
  sOptimizedNumerics = 'OptimizedNumerics';
  sRoleName = 'RoleName';
  sSQLDialect = 'SQLDialect';
  sUseQuoteChar = 'UseQuoteChar';
  sUseUnicode = 'UseUnicode';
  sWaitOnLocks = 'WaitOnLocks';

type

{$IF DEFINED(CLR)}
  TDBXDevartInterBaseDriver = class(TDBXDynalinkDriverManaged)
{$ELSE}
  TDBXDevartInterBaseDriver = class(TDBXDynalinkDriverNative)
{$IFEND}
  protected
    function GetDriverLoaderClass: TDBXDynalinkDriverCommonLoaderClass; virtual;
  public
    constructor Create(DBXDriverDef: TDBXDriverDef); override;
  end;

  TDBXDevartInterBaseProperties = class(TDBXProperties)
  strict private
    function GetDataBase: string;
    procedure SetDataBase(const Value: string);
    function GetUserName: string;
    procedure SetUserName(const Value: string);
    function GetPassword: string;
    procedure SetPassword(const Value: string);

    function GetRoleName: string;
    procedure SetRoleName(const Value: string);
    function GetSQLDialect: Integer;
    procedure SetSQLDialect(const Value: Integer);
    function GetBlobSize: Integer;
    procedure SetBlobSize(const Value: Integer);
    function GetErrorResourceFile: string;
    procedure SetErrorResourceFile(const Value: string);
    function GetLocaleCode: string;
    procedure SetLocaleCode(const Value: string);
    function GetInterBaseTransIsolation: string;
    procedure SetInterBaseTransIsolation(const Value: string);
    function GetWaitOnLocks: Boolean;
    procedure SetWaitOnLocks(const Value: Boolean);
    function GetCharset: string;
    procedure SetCharset(const Value: string);
    function GetCharLength: Integer;
    procedure SetCharLength(const Value: Integer);
    function GetEnableBCD: Boolean;
    procedure SetEnableBCD(const Value: Boolean);
    function GetOptimizedNumerics: Boolean;
    procedure SetOptimizedNumerics(const Value: Boolean);
    function GetLongStrings: Boolean;
    procedure SetLongStrings(const Value: Boolean);
    function GetUseQuoteChar: Boolean;
    procedure SetUseQuoteChar(const Value: Boolean);
    function GetFetchAll: Boolean;
    procedure SetFetchAll(const Value: Boolean);

    function GetUseUnicode: Boolean;
    procedure SetUseUnicode(const Value: Boolean);

  public
    // cannot fill driver properties in the constructor
    // because TDBXProperties.Clone will duplicate all properties
    //constructor Create(DBXContext: TDBXContext); override;

  published
    property DataBase: string read GetDataBase write SetDataBase;
    property UserName: string read GetUserName write SetUserName;
    property Password: string read GetPassword write SetPassword;

    property RoleName: string read GetRoleName write SetRoleName;
    property SQLDialect: Integer read GetSQLDialect write SetSQLDialect;
    property BlobSize: Integer read GetBlobSize write SetBlobSize;
    property ErrorResourceFile: string read GetErrorResourceFile write SetErrorResourceFile;
    property LocaleCode: string read GetLocaleCode write SetLocaleCode;
    property InterBaseTransIsolation: string read GetInterBaseTransIsolation write SetInterBaseTransIsolation;
    property WaitOnLocks: Boolean read GetWaitOnLocks write SetWaitOnLocks;
    property Charset: string read GetCharset write SetCharset;
    property CharLength: Integer read GetCharLength write SetCharLength;
    property EnableBCD: Boolean read GetEnableBCD write SetEnableBCD;
    property OptimizedNumerics:Boolean read GetOptimizedNumerics write SetOptimizedNumerics;
    property LongStrings: Boolean read GetLongStrings write SetLongStrings;
    property UseQuoteChar: Boolean read GetUseQuoteChar write SetUseQuoteChar;
    property FetchAll: Boolean read GetFetchAll write SetFetchAll;

    property UseUnicode: Boolean read GetUseUnicode write SetUseUnicode;
  end;

implementation

uses
  DbxPlatform;

{ TDBXDevartInterBaseDriver }

constructor TDBXDevartInterBaseDriver.Create(DBXDriverDef: TDBXDriverDef);
begin
  inherited Create(DBXDriverDef, GetDriverLoaderClass);

  InitDriverProperties(TDBXDevartInterBaseProperties.Create(DBXDriverDef.FDBXContext));

  DriverProperties[TDBXPropertyNames.DriverUnit] := 'DBXDevartInterBase';
  DriverProperties[TDBXPropertyNames.DriverPackageLoader] := 'TDBXDynalinkDriverLoader,DBXCommonDriver120.bpl';
  DriverProperties[TDBXPropertyNames.DriverAssemblyLoader] := 'Devart.DbxIda.DriverLoader.TCRDynalinkDriverLoader,Devart.DbxIda.DriverLoader,Version=1.0.0.5,Culture=neutral,PublicKeyToken=09af7300eec23701';
  DriverProperties[TDBXPropertyNames.MetaDataAssemblyLoader] := 'Borland.Data.TDBXInterbaseMetaDataCommandFactory,Borland.Data.DbxInterBaseDriver,Version=12.0.0.0,Culture=neutral,PublicKeyToken=' + TDBXPlatform.GetPublicKeyToken;
  DriverProperties[TDBXPropertyNames.ProductName] := 'Interbase';

  DriverProperties[TDBXPropertyNames.GetDriverFunc] := 'getSQLDriverInterBase';
  DriverProperties[TDBXPropertyNames.LibraryName] := 'dbexpida40.dll';
  DriverProperties[TDBXPropertyNames.VendorLib] := 'gds32.dll';

  DriverProperties[TDBXPropertyNames.Database] := 'DataBase Name';
  DriverProperties[TDBXPropertyNames.UserName] := 'user';
  DriverProperties[TDBXPropertyNames.Password] := 'password';

  DriverProperties[sRoleName] := '';
  DriverProperties[sSQLDialect] := '3';
  DriverProperties[TDBXPropertyNames.MaxBlobSize] := '-1';
  DriverProperties[TDBXPropertyNames.ErrorResourceFile] := '';
  DriverProperties[TDBXDynalinkPropertyNames.LocaleCode] := '0000';
  DriverProperties[sInterBaseTransIsolation] := 'ReadCommitted';
  DriverProperties[sWaitOnLocks] := 'True';
  DriverProperties[sCharset] := '';
  DriverProperties[sCharLength] := '1';
  DriverProperties[sEnableBCD] := 'True';
  DriverProperties[sOptimizedNumerics] := 'True';
  DriverProperties[sLongStrings] := 'True';
  DriverProperties[sUseQuoteChar] := 'False';
  DriverProperties[sFetchAll] := 'False';

  DriverProperties[sUseUnicode] := 'True';
end;

function TDBXDevartInterBaseDriver.GetDriverLoaderClass: TDBXDynalinkDriverCommonLoaderClass;
begin
  Result := TDBXDynalinkDriverLoader;
end;

{ TDBXDevartInterBaseProperties }

function TDBXDevartInterBaseProperties.GetDataBase: string;
begin
  Result := Values[TDBXPropertyNames.Database];
end;

procedure TDBXDevartInterBaseProperties.SetDataBase(const Value: string);
begin
  Values[TDBXPropertyNames.Database] := Value;
end;

function TDBXDevartInterBaseProperties.GetUserName: string;
begin
  Result := Values[TDBXPropertyNames.UserName];
end;

procedure TDBXDevartInterBaseProperties.SetUserName(const Value: string);
begin
  Values[TDBXPropertyNames.UserName] := Value;
end;

function TDBXDevartInterBaseProperties.GetPassword: string;
begin
  Result := Values[TDBXPropertyNames.Password];
end;

procedure TDBXDevartInterBaseProperties.SetPassword(const Value: string);
begin
  Values[TDBXPropertyNames.Password] := Value;
end;

function TDBXDevartInterBaseProperties.GetRoleName: string;
begin
  Result := Values[sRoleName];
end;

procedure TDBXDevartInterBaseProperties.SetRoleName(const Value: string);
begin
  Values[sRoleName] := Value;
end;

function TDBXDevartInterBaseProperties.GetSQLDialect: Integer;
begin
  Result := StrToIntDef(Values[sSQLDialect], 3);
end;

procedure TDBXDevartInterBaseProperties.SetSQLDialect(const Value: Integer);
begin
  Values[sCharLength] := IntToStr(Value);
end;

function TDBXDevartInterBaseProperties.GetBlobSize: Integer;
begin
  Result := StrToIntDef(Values[TDBXPropertyNames.MaxBlobSize], -1);
end;

procedure TDBXDevartInterBaseProperties.SetBlobSize(const Value: Integer);
begin
  Values[TDBXPropertyNames.MaxBlobSize] := IntToStr(Value);
end;

function TDBXDevartInterBaseProperties.GetInterBaseTransIsolation: string;
begin
  Result := Values[sInterBaseTransIsolation];
end;

function TDBXDevartInterBaseProperties.GetErrorResourceFile: string;
begin
  Result := Values[TDBXPropertyNames.ErrorResourceFile];
end;

procedure TDBXDevartInterBaseProperties.SetErrorResourceFile(const Value: string);
begin
  Values[TDBXPropertyNames.ErrorResourceFile] := Value;
end;

function TDBXDevartInterBaseProperties.GetLocaleCode: string;
begin
  Result := Values[TDBXDynalinkPropertyNames.LocaleCode];
end;

procedure TDBXDevartInterBaseProperties.SetLocaleCode(const Value: string);
begin
  Values[TDBXDynalinkPropertyNames.LocaleCode] := Value;
end;

procedure TDBXDevartInterBaseProperties.SetInterBaseTransIsolation(const Value: string);
begin
  Values[sInterBaseTransIsolation] := Value;
end;

function TDBXDevartInterBaseProperties.GetWaitOnLocks: Boolean;
begin
  Result := StrToBoolDef(Values[sWaitOnLocks], True);
end;

procedure TDBXDevartInterBaseProperties.SetWaitOnLocks(const Value: Boolean);
begin
  Values[sWaitOnLocks] := BoolToStr(Value, True);
end;

function TDBXDevartInterBaseProperties.GetCharset: string;
begin
  Result := Values[sCharset];
end;

procedure TDBXDevartInterBaseProperties.SetCharset(const Value: string);
begin
  Values[sCharset] := Value;
end;

function TDBXDevartInterBaseProperties.GetCharLength: Integer;
begin
  Result := StrToIntDef(Values[sCharLength], 1);
end;

procedure TDBXDevartInterBaseProperties.SetCharLength(const Value: Integer);
begin
  Values[sCharLength] := IntToStr(Value);
end;

function TDBXDevartInterBaseProperties.GetEnableBCD: Boolean;
begin
  Result := StrToBoolDef(Values[sEnableBCD], True);
end;

procedure TDBXDevartInterBaseProperties.SetEnableBCD(const Value: Boolean);
begin
  Values[sEnableBCD] := BoolToStr(Value, True);
end;

function TDBXDevartInterBaseProperties.GetOptimizedNumerics: Boolean;
begin
  Result := StrToBoolDef(Values[sOptimizedNumerics], True);
end;

procedure TDBXDevartInterBaseProperties.SetOptimizedNumerics(const Value: Boolean);
begin
  Values[sOptimizedNumerics] := BoolToStr(Value, True);
end;

function TDBXDevartInterBaseProperties.GetLongStrings: Boolean;
begin
  Result := StrToBoolDef(Values[sLongStrings], True);
end;

procedure TDBXDevartInterBaseProperties.SetLongStrings(const Value: Boolean);
begin
  Values[sLongStrings] := BoolToStr(Value, True);
end;

function TDBXDevartInterBaseProperties.GetUseQuoteChar: Boolean;
begin
  Result := StrToBoolDef(Values[sUseQuoteChar], False);
end;

procedure TDBXDevartInterBaseProperties.SetUseQuoteChar(const Value: Boolean);
begin
  Values[sUseQuoteChar] := BoolToStr(Value, True);
end;

function TDBXDevartInterBaseProperties.GetFetchAll: Boolean;
begin
  Result := StrToBoolDef(Values[sFetchAll], False);
end;

procedure TDBXDevartInterBaseProperties.SetFetchAll(const Value: Boolean);
begin
  Values[sFetchAll] := BoolToStr(Value, True);
end;

function TDBXDevartInterBaseProperties.GetUseUnicode: Boolean;
begin
  Result := StrToBoolDef(Values[sUseUnicode], True);
end;

procedure TDBXDevartInterBaseProperties.SetUseUnicode(const Value: Boolean);
begin
  Values[sUseUnicode] := BoolToStr(Value, True);
end;

initialization
  TDBXDriverRegistry.RegisterDriverClass(sDriverName, TDBXDevartInterBaseDriver);
end.
