unit Kitto.Web.Request;

interface

uses
  IdHttpWebBrokerBridge
  , superobject
  ;

type
  TKWebRequest = class(TIdHTTPAppRequest)
  private
    class threadvar FCurrent: TKWebRequest;
    function GetIsAjax: Boolean;
    class function GetCurrent: TKWebRequest; static;
    class procedure SetCurrent(const AValue: TKWebRequest); static;
    function GetIsRefresh: Boolean;
  public
    class property Current: TKWebRequest read GetCurrent write SetCurrent;
    class procedure ClearCurrent;
    property IsAjax: Boolean read GetIsAjax;
    property IsRefresh: Boolean read GetIsRefresh;

    /// <summary>
    ///  Returns all request query fields (names and decoded values) as an ISuperObject.
    ///  Note: All values are treated as strings.
    /// </summary>
    function GetQueryFields: ISuperObject;

    /// <summary>
    ///  Decodes and returns the value of the query field with the given name.
    /// </summary>
    function GetQueryField(const AName: string): string;

    function IsBrowserIPhone: Boolean;
    function IsBrowserIPad: Boolean;
    function IsMobileBrowser: Boolean;
  end;

implementation

uses
  SysUtils
  , NetEncoding
  , EF.Logger
  ;

{ TKWebRequest }

class procedure TKWebRequest.ClearCurrent;
begin
  FreeAndNil(FCurrent);
end;

class function TKWebRequest.GetCurrent: TKWebRequest;
begin
  Result := FCurrent;
end;

function TKWebRequest.GetIsAjax: Boolean;
begin
  Result := GetFieldByName('X-Requested-With') = 'XMLHttpRequest';
end;

function TKWebRequest.GetIsRefresh: Boolean;
begin
  Result := not IsAjax and (GetFieldByName('Cache-Control') = 'max-age=0');
end;

function TKWebRequest.GetQueryField(const AName: string): string;
begin
  Result := TNetEncoding.URL.Decode(QueryFields.Values[AName], [TURLEncoding.TDecodeOption.PlusAsSpaces]);
end;

function TKWebRequest.GetQueryFields: ISuperObject;
var
  I: Integer;
begin
  Result := SO();
  for I := 0 to QueryFields.Count - 1 do
    Result.S[QueryFields.Names[I]] := GetQueryField(QueryFields.Names[I]);
end;

function TKWebRequest.IsBrowserIPhone: Boolean;
begin
  Result := UserAgent.Contains('iPhone');
end;

function TKWebRequest.IsBrowserIPad: Boolean;
begin
  Result := UserAgent.Contains('iPad');
end;

function TKWebRequest.IsMobileBrowser: Boolean;
var
  LUserAgent: string;
begin
  LUserAgent := UserAgent;
  TEFLogger.Instance.Log('UserAgent: ' + LUserAgent);
  Result := LUserAgent.Contains('Windows Phone') or
    LUserAgent.Contains('iPhone') or
    LUserAgent.Contains('iPad') or
    LUserAgent.Contains('Android');
end;

class procedure TKWebRequest.SetCurrent(const AValue: TKWebRequest);
begin
  FCurrent := AValue;
end;

end.
