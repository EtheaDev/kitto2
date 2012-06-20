unit IdExtHTTPServer;

interface

uses
  {$IFNDEF MSWINDOWS}cthreads,{$ENDIF}
  Classes, IdCustomHTTPServer, IdHTTPServer, ExtPascalClasses, ExtPascalUtils;

type
  TIdWebSession = class(TCustomWebSession)
  private
    procedure SetCustomResponseHeaders;
  protected
    function CanCallAfterHandleRequest : Boolean; override;
    function CanHandleUrlPath : Boolean; override;
    procedure DoLogout; override;
    procedure DoSetCookie(const Name, ValueRaw : string); override;
    class function GetCurrentWebSession : TCustomWebSession; override;
    function GetDocumentRoot : string; override;
    function GetRequestHeader(const Name : string) : string; override;
    function GetWebServer : string; override;
    procedure SendResponse(const Msg : string); override;
    function TryToServeFile : Boolean; override;
    function UploadBlockType(const Buffer : AnsiString; var MarkPos : Integer) : TUploadBlockType; override;
    function UploadNeedUnknownBlock : Boolean; override;
  public
    constructor Create(AOwner : TObject); override;
  end;

  TWebSession = class(TIdWebSession);

  TIdWebApplication = class(TCustomWebApplication)
  private
    FServer: TIdHTTPServer;
  public
    constructor Create(ATitle : string; ASessionClass : TCustomWebSessionClass; APort : word = 80;
                       AMaxIdleMinutes : word = 30; AMaxConns : integer = 1000); reintroduce;
    procedure DoRun; override;
  end;

var
  Application : TIdWebApplication; // Indy web application object

threadvar
  CurrentWebSession : TIdWebSession; // current Indy web session object

function CreateWebApplication(const ATitle: string; ASessionClass: TCustomWebSessionClass; APort: Word = 80;
                              AMaxIdleMinutes : Word = 30; AShutdownAfterLastThreadDown : Boolean = False;
                              AMaxConns : Integer = 1000): TIdWebApplication;

implementation

uses
  {$IFDEF MSWINDOWS}Windows, Messages,{$ENDIF} StrUtils, SysUtils,
  IdGlobal, IdGlobalProtocols, IdCookie, IdContext;

function CreateWebApplication(const ATitle : string; ASessionClass:  TCustomWebSessionClass; APort: Word = 80;
                              AMaxIdleMinutes : Word = 30; AShutdownAfterLastThreadDown : Boolean = False;
                              AMaxConns : Integer = 1000) : TIdWebApplication;
begin
  Result := TIdWebApplication.Create(ATitle, ASessionClass, APort, AMaxIdleMinutes, AMaxConns);
end;

type
  TIdWebHTTPServer = class(TIdHTTPServer)
  protected
    procedure InitComponent; override;
    procedure CommandGet(AContext : TIdContext; ARequestInfo : TIdHTTPRequestInfo; AResponseInfo : TIdHTTPResponseInfo);
  end;

  TIdWebHTTPSessionList = class(TIdHTTPDefaultSessionList)
  private
    FOwner : TIdWebHTTPServer;
  public
    constructor Create(const AOwner : TIdWebHTTPServer); reintroduce;
    function CreateSession(const RemoteIP, SessionID : string) : TIdHTTPSession; override;
  end;

  TIdWebHTTPSession = class(TIdHTTPSession)
  private
    FCurrentRequest   : TIdHTTPRequestInfo;
    FCurrentResponse  : TIdHTTPResponseInfo;
    FSession : TIdWebSession;
    procedure ParseCookies(IdCookies : TIdCookies);
  public
    constructor CreateInitialized(AOwner : TIdHTTPCustomSessionList; const SessionID, RemoteIP : string); override;
    destructor Destroy; override;
    procedure HandleRequest(ARequest : TIdHTTPRequestInfo; AResponse : TIdHTTPResponseInfo);
  end;

{ TIdWebHTTPSessionList }

constructor TIdWebHTTPSessionList.Create(const AOwner : TIdWebHTTPServer); begin
  FOwner := AOwner;
  inherited Create(AOwner);
end;

function TIdWebHTTPSessionList.CreateSession(const RemoteIP, SessionID : string) : TIdHTTPSession; begin
  Result := TIdWebHTTPSession.CreateInitialized(Self, SessionID, RemoteIP);
  SessionList.Add(Result);
end;

{ TIdWebHTTPServer }

procedure TIdWebHTTPServer.CommandGet(AContext : TIdContext; ARequestInfo :  TIdHTTPRequestInfo; AResponseInfo : TIdHTTPResponseInfo);
var
  I : Integer;
begin
  if pos('multipart/form-data', ARequestInfo.ContentType) <> 0 then
    with TIdWebHTTPSession(ARequestInfo.Session) do begin
      I := 0;
      FSession.UploadPrepare(ARequestInfo.ContentType, ARequestInfo.UnparsedParams, I);
      FSession.UploadWriteFile(ARequestInfo.UnparsedParams, I);
      FCurrentResponse := AResponseInfo;
      FCurrentResponse.ContentText := FSession.Response;
    end;
  TIdWebHTTPSession(ARequestInfo.Session).HandleRequest(ARequestInfo, AResponseInfo);
end;

procedure TIdWebHTTPServer.InitComponent; begin
  inherited;
  SessionState := True;
  AutoStartSession := True;
  SessionList.Free;
  FSessionList := TIdWebHTTPSessionList.Create(Self);
end;

{ TIdWebHTTPSession }

constructor TIdWebHTTPSession.CreateInitialized(AOwner : TIdHTTPCustomSessionList; const SessionID, RemoteIP : string); begin
  inherited;
  FSession := TIdWebSession(Application.SessionClass.Create(Self));
  FSession.FApplication := Application;
  FSession.AfterNewSession;
end;

destructor TIdWebHTTPSession.Destroy; begin
  FSession.Free;
  inherited;
end;

procedure TIdWebHTTPSession.HandleRequest(ARequest : TIdHTTPRequestInfo; AResponse : TIdHTTPResponseInfo); begin
  CurrentWebSession := FSession;
  FCurrentRequest   := ARequest;
  FCurrentResponse  := AResponse;
  ParseCookies(ARequest.Cookies);
  if FSession.Cookie['FCGIThread'] = '' then
    FSession.SetCookie('FCGIThread', SessionID);
  if not FSession.IsUpload then begin
    FSession.ContentType := 'text/html';
    FSession.Response := '';
    FSession.FCustomResponseHeaders.Clear;
    FSession.SetQueryText(FCurrentRequest.UnParsedParams, False, False);
  end;
  with FSession do begin
    FPathInfo := FCurrentRequest.Document;
    if (FPathInfo <> '') and (FPathInfo[1] = '/') then Delete(FPathInfo, 1, 1);
    FScriptName := Query['SCRIPT_NAME'];
    if (FScriptName = '') or (FScriptName[Length(FScriptName)] <> '/') then FScriptName := FScriptName + '/';
  end;
  FSession.HandleRequest(FCurrentRequest.UnParsedParams);
  if not FSession.IsUpload then begin
    TIdWebSession(FSession).SetCustomResponseHeaders;
    if not Assigned(AResponse.ContentStream) and (FSession.Response <> '') and (AResponse.ResponseNo <> 304) then
      FCurrentResponse.ContentText := FSession.Response;
  end;
  FCurrentResponse.ContentType := FSession.ContentType;
  FSession.IsDownload := false;
  FSession.IsUpload   := false;
end;

procedure TIdWebHTTPSession.ParseCookies(IdCookies : TIdCookies);
var
  I: Integer;
begin
  FSession.FCookies.Clear;
  for I := 0 to IdCookies.Count - 1 do
    FSession.FCookies.Values[IdCookies[I].CookieName] := IdCookies[I].Value;
end;

{ TIdWebApplication }

constructor TIdWebApplication.Create(ATitle : string; ASessionClass : TCustomWebSessionClass;
                                     APort, AMaxIdleMinutes : word; AMaxConns : integer); begin
  inherited Create(ATitle, ASessionClass, APort, AMaxIdleMinutes, AMaxConns);
  Assert(Assigned(ASessionClass) and ASessionClass.InheritsFrom(TIdWebSession));
  FServer := TIdWebHTTPServer.Create(nil);
  with TIdWebHTTPServer(FServer) do begin
    OnCommandGet   := CommandGet;
    SessionTimeOut := AMaxIdleMinutes;
    MaxConnections := AMaxConns;
    ServerSoftware := ATitle;
    DefaultPort    := APort;
    {$IFNDEF MSWINDOWS}
    with Bindings do begin
      Clear;
      Add;
      Items[0].SetPeer('127.0.0.1', APort, id_IPV4);
    end;
    {$ENDIF}
  end;
end;

procedure TIdWebApplication.DoRun;
{$IFDEF MSWINDOWS}
var
  Msg : TMsg;
  Unicode : boolean;
{$ENDIF}
begin
  TIdWebHTTPServer(FServer).Startup;
  while not Terminated do
    {$IFDEF MSWINDOWS}
    if PeekMessage(Msg, 0, 0, 0, PM_NOREMOVE) then begin
      Unicode := (Msg.hwnd <> 0) and IsWindowUnicode(Msg.hwnd);
      if Unicode then
        PeekMessageW(Msg, 0, 0, 0, PM_REMOVE)
      else
        PeekMessage(Msg, 0, 0, 0, PM_REMOVE);
      if Msg.Message = WM_QUIT then exit;
      TranslateMessage(Msg);
      if Unicode then
        DispatchMessageW(Msg)
      else
        DispatchMessage(Msg);
    end
    else
    {$ENDIF}
      sleep(10);
end;

{ TIdWebSession }

constructor TIdWebSession.Create(AOwner : TObject); begin
  inherited Create(AOwner);
  FOwner := AOwner as TIdWebHTTPSession;
end;

function TIdWebSession.CanCallAfterHandleRequest : Boolean; begin
  Result := not IsUpload;
end;

function TIdWebSession.CanHandleUrlPath : Boolean; begin
  Result := True;
end;

procedure TIdWebSession.DoLogout; begin
  inherited;
  TIdWebHTTPSession(FOwner).FLastTimeStamp := 0;
end;

procedure TIdWebSession.DoSetCookie(const Name, ValueRaw : string);
var
  LCookie: TIdCookie;
begin
  with TIdWebHTTPSession(FOwner) do begin
    LCookie := FCurrentResponse.Cookies.Add;
    with LCookie do begin
      CookieName := 'FCGIThread';
      Value      := ValueRaw;
      Path       := '/';    {Do not Localize}
      // By default the cookies wil be valid until the user has closed his browser window.
    end;
    //FCurrentRequest.Cookies.Add.Assign(LCookie);
    with FCurrentRequest.Cookies.Add do begin
      CookieName := 'FCGIThread';
      Value{CookieText} := ValueRaw;
      Path := '/';    {Do not Localize}
      // By default the cookies wil be valid until the user has closed his browser window.
    end;
    //FCurrentRequest.Cookies.AddSrcCookie(ValueRaw);
    //FCurrentRequest.Cookies.AddClientCookie(ValueRaw);
  end;
end;

class function TIdWebSession.GetCurrentWebSession : TCustomWebSession; begin
  Result := CurrentWebSession;
end;

function TIdWebSession.GetDocumentRoot : string; begin
  Result := '.';
end;

function TIdWebSession.GetRequestHeader(const Name : string): string;
var
  HeaderName : string;
begin
  HeaderName := Name;
  if pos('HTTP_', HeaderName) = 1 then HeaderName := copy(HeaderName, 6, MaxInt);
  HeaderName := AnsiReplaceStr(HeaderName, '_', '-');
  with TIdWebHTTPSession(FOwner) do 
    if FCurrentRequest = nil then
      Result := ''
    else
      if FCurrentRequest.RawHeaders = nil then
        if FCurrentRequest.CustomHeaders = nil then
          Result := ''
        else
          Result := FCurrentRequest.CustomHeaders.Values[HeaderName]
      else
        Result := FCurrentRequest.RawHeaders.Values[HeaderName];
end;

function TIdWebSession.GetWebServer : string; begin
  Result := 'Embedded'; // or maybe 'Embedded Indy' ?
end;

procedure TIdWebSession.SendResponse(const Msg : string); begin
  with TIdWebHTTPSession(FOwner).FCurrentResponse do begin
    ContentText := Msg;
    WriteContent;
  end;
end;

procedure TIdWebSession.SetCustomResponseHeaders;
var
  i: Integer;
begin
  for i := 0 to FCustomResponseHeaders.Count - 1 do
    with TIdWebHTTPSession(FOwner).FCurrentResponse.CustomHeaders do
      Values[FCustomResponseHeaders.Names[i]] := FCustomResponseHeaders.ValueFromIndex[i];
end;

function TIdWebSession.TryToServeFile : Boolean;

  function CheckIfFileIsModified(FileName : string) : Boolean;
  const
    FCompareDateFmt = 'yyyymmddhhnnss';
  var
    FFileDateTime: TDateTime;
  begin
    Result := True;
    with TIdWebHTTPSession(FOwner).FCurrentRequest.RawHeaders do
      if (Values['if-Modified-Since'] <> '') then begin
        FFileDateTime := GetGMTDateByName(FileName);
        Result := not SameText(FormatDateTime(FCompareDateFmt, FFileDateTime),
          FormatDateTime(FCompareDateFmt, StrInternetToDateTime(Values['if-Modified-Since'])));
      end;
  end;

var
  FileName : string;
  FileDateTime : TDateTime;
begin
  FileName := ExtractFilePath(ParamStr(0));
  FileName := StringReplace(FileName, ExtractFileDrive(FileName), '', []);
  with TIdWebHTTPSession(FOwner).FCurrentRequest do
    if (Length(Document) > 1) and (Document[1]in ['/', '\']) then
      FileName := FileName + Copy(Document, 2, MaxInt)
    else
      FileName := FileName + Document;
  FileName := ExpandFilename(FileName);
  with TIdWebHTTPSession(FOwner).FCurrentResponse do
    if FileExists(FileName) then begin
      Result := True;
      if CheckIfFileIsModified(FileName) then begin
        ContentStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
        FreeContentStream := True;
        ContentLength := ContentStream.Size;
        FileDateTime := GetGMTDateByName(FileName);
        LastModified := FileDateTime;
        Self.ContentType := DownloadContentType(FileName, 'text/html');
      end
      else
        ResponseNo := 304; // Not Modified, use cache version
    end
    else begin
      ResponseNo := 404; // Not found
      Result := False;
    end;
end;

function TIdWebSession.UploadBlockType(const Buffer : AnsiString; var MarkPos : Integer) : TUploadBlockType; begin
  Result := ubtBegin;
end;

function TIdWebSession.UploadNeedUnknownBlock : Boolean; begin
  Result := False;
end;

end.
