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
    procedure ParseParams;
  public
    function ParamByName(const AName: string): string;
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils
  , StrUtils
  , IdGlobal
  ;

{ TKURL }

destructor TKURL.Destroy;
begin
  FreeAndNil(FParsedParams);
  inherited;
end;

function TKURL.ParamByName(const AName: string): string;
begin
  if not Assigned(FParsedParams) then
    ParseParams;
  Result := FParsedParams.Values[AName];
end;

procedure TKURL.ParseParams;
var
  I: Integer;
begin
  Assert(not Assigned(FParsedParams));

  FParsedParams := TStringList.Create;
  FParsedParams.Delimiter := '&';
  FParsedParams.StrictDelimiter := True;

  FParsedParams.DelimitedText := Params;

  for I := 0 to FParsedParams.Count - 1 do
  begin
    FParsedParams[I] := ReplaceStr(FParsedParams[I], '+', ' ');
    FParsedParams[I] := URLDecode(FParsedParams[I], IndyTextEncoding_UTF8);
  end;
end;

end.
