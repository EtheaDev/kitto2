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

    /// <summary>
    ///  Returns the last path segment. If the path has only one segment, returns ''.
    /// </summary>
    function ExtractObjectName: string;

    /// <summary>
    ///  Adds a trailing / if not present already.
    /// </summary>
    class function IncludeTrailingPathDelimiter(const APath: string): string;
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

class function TKURL.IncludeTrailingPathDelimiter(const APath: string): string;
const
  PATH_DELIMITER = '/';

  function IsPathDelimiter(const AString: string; AIndex: Integer): Boolean;
  begin
    Result := (AIndex >= Low(string)) and (AIndex <= High(AString)) and (AString[AIndex] = PATH_DELIMITER)
      and (ByteType(AString, AIndex) = mbSingleByte);
  end;

begin
  Result := APath;
  if not IsPathDelimiter(Result, High(Result)) then
    Result := Result + PATH_DELIMITER;
end;

function TKURL.ParamByName(const AName: string): string;
begin
  Result := FParsedParams.Values[AName];
end;

end.
