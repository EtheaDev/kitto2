unit KIDE.MRUOptions;

interface

uses
  Classes,
  EF.Tree;

type
  TStringsHelper = class helper for TStrings
  public
    function IndexOfValue(const AValue: string): Integer;
  end;

  TMRUOptions = class(TEFTree)
  private
    FFileName: string;
    const MRU_SIZE = 20;
    class var FInstance: TMRUOptions;
    class function GetInstance: TMRUOptions; static;
    class destructor Destroy;
  public
    class property Instance: TMRUOptions read GetInstance;

    procedure StoreString(const AKey, AValue: string);
    procedure StoreInteger(const AKey: string; const AValue: Integer);
    procedure StoreBoolean(const AKey: string; const AValue: Boolean);
    procedure StoreMRUItem(const AKey, AValue: string);

    procedure Save;
  end;

implementation

uses
  SysUtils, Types,
  EF.Macros, EF.YAML;

{ TKIMRUOptions }

class destructor TMRUOptions.Destroy;
begin
  FreeAndNil(FInstance);
end;

class function TMRUOptions.GetInstance: TMRUOptions;
var
  LFileName: string;
begin
  if not Assigned(FInstance) then
  begin
    LFileName := TEFMacroExpansionEngine.Instance.Expand('%APPDATA%\KIDE\MRU.yaml');
    if FileExists(LFileName) then
      FInstance := TEFTreeFactory.LoadFromFile<TMRUOptions>(LFileName)
    else
      FInstance := TMRUOptions.Create;
    FInstance.FFileName := LFileName;
  end;
  Result := FInstance;
end;

procedure TMRUOptions.Save;
var
  LWriter: TEFYAMLWriter;
begin
  ForceDirectories(ExtractFilePath(FFileName));
  LWriter := TEFYAMLWriter.Create;
  try
    LWriter.SaveTreeToFile(Self, FFileName);
  finally
    FreeAndNil(LWriter);
  end;
end;

procedure TMRUOptions.StoreBoolean(const AKey: string; const AValue: Boolean);
begin
  SetBoolean(AKey, AValue);
  Save;
end;

procedure TMRUOptions.StoreInteger(const AKey: string; const AValue: Integer);
begin
  SetInteger(AKey, AValue);
  Save;
end;

procedure TMRUOptions.StoreMRUItem(const AKey, AValue: string);
var
  LItems: TStrings;
  LItemIndex: Integer;
begin
  LItems := TStringList.Create;
  try
    GetChildrenAsStrings(AKey, LItems);
    LItemIndex := LItems.IndexOfValue(AValue);
    if LItemIndex >= 0 then
      LItems.Delete(LItemIndex);

    while LItems.Count >= MRU_SIZE do
      LItems.Delete(LItems.Count - 1);

    LItems.Insert(0, 'Item=' + AValue);

    SetChildrenAsStrings(AKey, LItems);
    Save;
  finally
    FreeAndNil(LItems);
  end;
end;

procedure TMRUOptions.StoreString(const AKey, AValue: string);
begin
  SetString(AKey, AValue);
  Save;
end;

{ TStringsHelper }

function TStringsHelper.IndexOfValue(const AValue: string): Integer;
begin
  for Result := 0 to Count - 1 do
    if CompareStrings(ValueFromIndex[Result], AValue) = 0 then
      Exit;
  Result := -1;
end;

end.
