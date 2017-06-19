unit Kitto.Web.Request;

{$I Kitto.Defines.inc}

interface

uses
  IdHttpWebBrokerBridge
  { TODO : Not available yet }
  {$IFNDEF LINUX}
  , ReqMulti
  {$ENDIF}
  , EF.Tree
  ;

type
  TKWebRequest = class(TIdHTTPAppRequest)
  private
    FQueryTree: TEFTree;
    FJSONContentTree: TEFTree;
    class threadvar FCurrent: TKWebRequest;
    function GetIsAjax: Boolean;
    class function GetCurrent: TKWebRequest; static;
    class procedure SetCurrent(const AValue: TKWebRequest); static;
    function GetIsRefresh: Boolean;
    function GetQueryTree: TEFTree;
    function GetJSONContentTree: TEFTree;
    function GetAcceptLanguage: string;
  public
    destructor Destroy; override;
  public
    class property Current: TKWebRequest read GetCurrent write SetCurrent;
    class procedure ClearCurrent;
    property IsAjax: Boolean read GetIsAjax;
    property IsRefresh: Boolean read GetIsRefresh;

    /// <summary>
    ///  Returns all request query fields (names and decoded values) as a tree
    ///  object, which is only alive as long as the request object is.
    ///  Note: All values are treated as strings.
    /// </summary>
    property QueryTree: TEFTree read GetQueryTree;

    /// <summary>
    ///  Parses the request content as a JSON object and returns the data as a
    ///  tree object, which is only alive as long as the request object is.
    ///  Note: All values are treated as strings.
    /// </summary>
    property JSONContentTree: TEFTree read GetJSONContentTree;

    /// <summary>
    ///  Decodes and returns the value of the query field with the given name.
    /// </summary>
    function GetQueryField(const AName: string): string;

    function IsBrowserIPhone: Boolean;
    function IsBrowserIPad: Boolean;
    function IsMobileBrowser: Boolean;

    property AcceptLanguage: string read GetAcceptLanguage;
  end;

implementation

uses
  SysUtils
  , JSON
  , NetEncoding
  , EF.JSON
  , EF.Logger
  ;

{ TKWebRequest }

class procedure TKWebRequest.ClearCurrent;
begin
  FreeAndNil(FCurrent);
end;

destructor TKWebRequest.Destroy;
begin
  FreeAndNil(FQueryTree);
  FreeAndNil(FJSONContentTree);
  inherited;
end;

function TKWebRequest.GetAcceptLanguage: string;
begin
  Result := GetFieldByName('Accept-Language');
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

function TKWebRequest.GetJSONContentTree: TEFTree;
var
  LJSON: TJSONValue;
begin
  if not Assigned(FJSONContentTree) then
  begin
    FJSONContentTree := TEFTree.Create;
    LJSON := TJSONObject.ParseJSONValue(Content);
    try
      Assert(Assigned(LJSON));
      Assert(LJSON is TJSONObject);
      LoadJSONObjectInTree(TJSONObject(LJSON), FJSONContentTree);
    finally
      FreeAndNil(LJSON);
    end;
  end;
  Result := FJSONContentTree;
end;

function TKWebRequest.GetQueryField(const AName: string): string;
begin
{$IFDEF D24+}
  Result := TNetEncoding.URL.Decode(QueryFields.Values[AName], [TURLEncoding.TDecodeOption.PlusAsSpaces]);
{$ELSE}
  Result := TNetEncoding.URL.Decode(QueryFields.Values[AName]);
{$ENDIF}
end;

function TKWebRequest.GetQueryTree: TEFTree;
var
  I: Integer;
begin
  if not Assigned(FQueryTree) then
  begin
    FQueryTree := TEFTree.Create;
    for I := 0 to QueryFields.Count - 1 do
      FQueryTree.AddChild(QueryFields.Names[I], GetQueryField(QueryFields.Names[I]));
  end;
  Result := FQueryTree;
end;

function TKWebRequest.IsBrowserIPhone: Boolean;
begin
  Result := string(UserAgent).Contains('iPhone');
end;

function TKWebRequest.IsBrowserIPad: Boolean;
begin
  Result := string(UserAgent).Contains('iPad');
end;

function TKWebRequest.IsMobileBrowser: Boolean;
var
  LUserAgent: string;
begin
  LUserAgent := UserAgent;
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
