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
    ///  Returns all request query param name and values as an ISuperObject.
    ///  Note: All values are treated as strings.
    /// </summary>
    function GetQueryFields: ISuperObject;
  end;

implementation

uses
  SysUtils
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

function TKWebRequest.GetQueryFields: ISuperObject;
var
  I: Integer;
begin
  Result := SO();
  for I := 0 to QueryFields.Count - 1 do
    Result.S[QueryFields.Names[I]] := QueryFields.ValueFromIndex[I];
end;

class procedure TKWebRequest.SetCurrent(const AValue: TKWebRequest);
begin
  FCurrent := AValue;
end;

end.
