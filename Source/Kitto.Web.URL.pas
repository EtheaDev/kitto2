unit Kitto.Web.URL;

interface

uses
  Classes
  , IdURI
  ;

type
  TKURL = class(TIdURI)
  private
    FParsedParams: TStrings;
  public
    constructor Create(const AURI: string; const AParams: TStrings); reintroduce;
    function ParamByName(const AName: string): string;
    destructor Destroy; override;

    // Returns the last path segment. If the path has only one segment, returns ''.
    function ExtractObjectName: string;
  end;

implementation

uses
  SysUtils
  , StrUtils
  , IdGlobal
  , EF.StrUtils
  ;

{ TKURL }

constructor TKURL.Create(const AURI: string; const AParams: TStrings);
begin
  inherited Create(AURI);
  FParsedParams := TStringList.Create;
  FParsedParams.Assign(AParams);
end;

destructor TKURL.Destroy;
begin
  FreeAndNil(FParsedParams);
  inherited;
end;

function TKURL.ExtractObjectName: string;
var
  LPathSegments: TArray<string>;
begin
  LPathSegments := StripPrefix(Path, '/').Split(['/']);
  if Length(LPathSegments) > 1 then
    Result := LPathSegments[High(LPathSegments)]
  else
    Result := '';
end;

function TKURL.ParamByName(const AName: string): string;
begin
  Result := FParsedParams.Values[AName];
end;

end.
