unit Kitto.Web.URL;

interface

uses
  Classes
  , IdURI
  ;

type
  TKWebURL = class(TIdURI)
  public
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
  , EF.StrUtils
  ;

{ TWebKURL }

function TKWebURL.ExtractObjectName: string;
var
  LPathSegments: TArray<string>;
begin
  LPathSegments := StripPrefix(Path, '/').Split(['/']);
  if Length(LPathSegments) > 1 then
    Result := LPathSegments[High(LPathSegments)]
  else
    Result := '';
end;

class function TKWebURL.IncludeTrailingPathDelimiter(const APath: string): string;
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

end.
