unit Kitto.Web.Response;

interface

uses
  IdHTTPWebBrokerBridge;

type
  TKWebResponse = class(TIdHTTPAppResponse)
  private
    class threadvar FCurrent: TKWebResponse;
    class function GetCurrent: TKWebResponse; static;
    class procedure SetCurrent(const AValue: TKWebResponse); static;
  public
    class property Current: TKWebResponse read GetCurrent write SetCurrent;
    class procedure ClearCurrent;
  end;

implementation

uses
  SysUtils
  ;

{ TKWebResponse }

class procedure TKWebResponse.ClearCurrent;
begin
  FreeAndNil(FCurrent);
end;

class function TKWebResponse.GetCurrent: TKWebResponse;
begin
  Result := FCurrent;
end;

class procedure TKWebResponse.SetCurrent(const AValue: TKWebResponse);
begin
  FCurrent := AValue;
end;

end.
