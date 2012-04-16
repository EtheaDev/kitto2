{
FCGIApp unit implements, in my opinion, the best behavior for Web applications: statefull, multi-threaded, blocking and non-multiplexed connection.
This is a native and full Object Pascal implementation that doesn't depend on DLLs or external libraries.
This unit is based on <extlink http://www.fastcgi.com/devkit/doc/fcgi-spec.html>FastCGI specs</extlink>, read it for more details.
The initial state in a <link TFCGIApplication, FastCGI application> is a listening socket, through which it accepts connections from a Web server.
After a FastCGI application <link TFCGIApplication.DoRun, accepts a connection on its listening socket>,
a <link TFCGIThread> is <link TFCGIThread.Create, created> that executes the FCGI protocol to <link TFCGIThread.ReadRequestHeader, receive> and <link TFCGIThread.SendResponse, send> data.
As the actual Web paradigm is based on non-related requests, FCGIApp uses a Cookie to relate requests of a same browser session.
This cookie is a <link TFCGIThread.SetCurrentFCGIThread, GUID that is associated> to actual <link TFCGIThread, Thread> address.
In this way a statefull and multi-thread behavior is provided.
-Limitations and architectural decisions:-
1. Multiplexing is not supported. Multiplexing don't works with Apache anyway. Indeed Apache don't supports Filter role.
2. Only Sockets is supported, because is more flexible providing the ability to run applications remotely, named pipes is not.
   So IIS is not natively supported, use <link CGIGateway.dpr> instead.
3. Only Responder role is implemented.
4. Event-driven paradigm is not supported, instead FCGIApp uses extensively multi-thread approach.
Author: Wanderlan Santos dos Anjos (wanderlan.anjos@gmail.com)
Date: apr-2008
License: BSD<extlink http://www.opensource.org/licenses/bsd-license.php>BSD</extlink>
}
unit FCGIApp;

// directives for config file support
{.$DEFINE HAS_CONFIG}
{$IFDEF HAS_CONFIG}
  {.$DEFINE CONFIG_MUST_EXIST}  // directive to make config file becomes mandatory
{$ENDIF}

interface

uses
  {$IFNDEF MSWINDOWS}cthreads,{$ENDIF}
  {$IFDEF HAS_CONFIG}IniFiles,{$ENDIF}
  BlockSocket, SysUtils, SyncObjs, Classes, ExtPascalClasses, ExtPascalUtils;

type
  TFCGISession = class(TCustomWebSession)
  protected
    function CanCallAfterHandleRequest : Boolean; override;
    function CanHandleUrlPath : Boolean; override;
    procedure DoLogout; override;
    procedure DoSetCookie(const Name, ValueRaw : string); override;
    class function GetCurrentWebSession : TCustomWebSession; override;
    function GetDocumentRoot : string; override;
    function GetRequestHeader(const Name : string) : string; override;
    function GetWebServer : string; override;
    procedure SendResponse(const Msg : AnsiString); override;
    function UploadBlockType(const Buffer : AnsiString; var MarkPos : Integer) : TUploadBlockType; override;
    function UploadNeedUnknownBlock : Boolean; override;
  public
    constructor Create(AOwner : TObject); override;
    destructor Destroy; override;
  end;

  TWebSession = class(TFCGISession);

  {
  Statefull and multi-thread behavior for FastCGI applications. This class has a garbage collector that frees idle threads.
  The initial state in a FastCGI application is a listening socket, through which it accepts connections from a Web server.
  After a FastCGI application <link TFCGIApplication.DoRun, accepts a connection on its listening socket>,
  a <link TFCGIThread> is <link TFCGIThread.Create, created> that executes the FCGI protocol to <link TFCGIThread.ReadRequestHeader, receive> and <link TFCGIThread.SendResponse, send> data.
  }
  TFCGIApplication = class(TCustomWebApplication)
  private
    FExeName      : string;
    Threads       : TStringList;
    FThreadsCount : integer;
    // Configurable options
    MaxIdleTime : TDateTime;
    WebServers  : TStringList;
    procedure GarbageThreads;
  protected
    function GetTerminated: Boolean; override;
  public
    GarbageNow : boolean; // Set to true to trigger the garbage colletor
    Shutdown   : boolean; // Set to true to shutdown the application after the last thread to end, default is false
    AccessThreads : TCriticalSection;
    {$IFDEF HAS_CONFIG}
    procedure ReadConfig;
    function Reconfig(AReload : Boolean = true) : Boolean; override;
    {$ENDIF}
    property ExeName : string read FExeName;
    procedure DoRun; override;
    function CanConnect(Address : string) : boolean;
    function GetThread(I : integer) : TThread;
    function ThreadsCount : integer;
    function ReachedMaxConns : boolean;
    procedure OnPortInUseError; virtual;
    constructor Create(pTitle : string; ASessionClass : TCustomWebSessionClass; pPort : word = 2014; pMaxIdleMinutes : word = 30;
                       pShutdownAfterLastThreadDown : boolean = false; pMaxConns : integer = 1000); reintroduce;
    destructor Destroy; override;
    procedure TerminateAllThreads;
  end;

var
  Application : TFCGIApplication = nil; // FastCGI application object

threadvar
  CurrentWebSession : TFCGISession; // current FastCGI session object

function CurrentFCGIThread : TFCGISession; deprecated;

function CreateWebApplication(const ATitle : string; ASessionClass : TCustomWebSessionClass; APort : Word = 2014;
                              AMaxIdleMinutes : Word = 30; AShutdownAfterLastThreadDown : Boolean = False;
                              AMaxConns : Integer = 1000) : TFCGIApplication;

implementation

uses
  StrUtils, Math;

function CurrentFCGIThread : TFCGISession; begin
  Result := CurrentWebSession;
end;

function CreateWebApplication(const ATitle : string; ASessionClass : TCustomWebSessionClass; APort : Word = 2014;
                              AMaxIdleMinutes : Word = 30; AShutdownAfterLastThreadDown : Boolean = False;
                              AMaxConns : Integer = 1000) : TFCGIApplication; begin
  Result := TFCGIApplication.Create(ATitle, ASessionClass, APort, AMaxIdleMinutes, AShutdownAfterLastThreadDown, AMaxConns);
end;

type
  // FastCGI record types, i.e. the general function that the record performs
  TRecType = (rtBeginRequest = 1, rtAbortRequest, rtEndRequest, rtParams, rtStdIn, rtStdOut, rtStdErr, rtData, rtGetValues, rtGetValuesResult, rtUnknown);
  // FastCGI roles, only Responder role is supported in this FCGIApp version
  TRole = (rResponder = 1, rAuthorizer, rFilter);
  // FastCGI level status code
  TProtocolStatus = (psRequestComplete, psCantMPXConn, psOverloaded, psUnknownRole, psBusy);
  // HTTP request methods
  TRequestMethod = (rmGet, rmPost, rmHead, rmPut, rmDelete);

  {
  Each browser session generates a TFCGIThread. On first request it is <link TFCGIThread.Create, created> and a Cookie is associated with it.
  On subsequent requests this <link TFCGIThread.SetCurrentFCGIThread, Cookie is read to recover the original thread address>.
  Each request <link TFCGIThread.Execute, is interpreted as a FastCGI record and executed according> to its <link TRecType, record type>.
  }
  TFCGIThread = class(TThread)
  private
    FRequestID : word; // FastCGI request ID for this thread
    FRole : TRole; // FastCGI Thread role
    FRequestMethod : TRequestMethod; // Current HTTP request method
    FSocket : TBlockSocket; // Current socket for current FastCGI request
    FGarbage,
    FKeepConn : boolean; // Not used
    FRequest : string;
    FRequestHeader : TStringList;
    FSession : TFCGISession;
    FLastAccess : TDateTime;
    function CompleteRequestHeaderInfo(Buffer : AnsiString; I : integer) : boolean;
  protected
    procedure AddParam(var S : string; Param : array of string);
    procedure ReadRequestHeader(var RequestHeader : TStringList; Stream : AnsiString; ParseCookies : Boolean = False);
    procedure ReadBeginRequest(var FCGIHeader; Content : AnsiString);
    procedure GetValues(Content : AnsiString);
    function HandleRequest(pRequest : AnsiString) : AnsiString;
    function SetCurrentFCGIThread : boolean;
  public
    BrowserCache : boolean; // If false generates 'cache-control:no-cache' in HTTP header, default is false
    AccessThread : TCriticalSection;
    property Role : TRole read FRole; // FastCGI role for the current request
    property Request : string read FRequest; // Request body string
    property LastAccess : TDateTime read FLastAccess; // Last TDateTime access of this thread
    property RequestMethod : TRequestMethod read FRequestMethod; // HTTP request method for the current request
    constructor Create(NewSocket : integer); virtual;
    destructor Destroy; override;
    procedure SendResponse(S : AnsiString; pRecType : TRecType = rtStdOut);
    procedure Execute; override;
    procedure SendEndRequest(Status : TProtocolStatus = psRequestComplete);
    procedure SetResponseHeader(Header : string);
  end;

threadvar
  _CurrentFCGIThread : TFCGIThread; // Current FastCGI thread address assigned by <link TFCGIThread.SetCurrentFCGIThread, SetCurrentFCGIThread> method

type
  // FastCGI header
  TFCGIHeader = packed record
    Version : byte;     // FastCGI protocol version, ever constant 1
    RecType : TRecType; // FastCGI record type
    ID,                 // Is zero if management request else is data request. Used also to determine if the session is being multiplexed
    Len     : word;     // FastCGI record length
    PadLen  : byte;     // Pad length to complete the alignment boundery that is 8 bytes on FastCGI protocol
    Filler  : byte;     // Pad field
  end;

  // <link TRecType, Begin request> record
  TBeginRequest = packed record
    Header   : TFCGIHeader; // FastCGI header
    Filler   : byte;        // Pad field
    Role     : TRole;       // FastCGI role
    KeepConn : boolean;     // Keep connection
    Filler2  : array[1..5] of byte; // Pad field
  end;

{
Converts a Request string into a FastCGI Header
@param Buffer Input buffer to convert
@param FCGIHeader FastCGI header converted from Buffer
@see MoveFromFCGIHeader
}
procedure MoveToFCGIHeader(var Buffer : AnsiChar; var FCGIHeader : TFCGIHeader); begin
  move(Buffer, FCGIHeader, sizeof(TFCGIHeader));
  {$IFNDEF FPC_BIG_ENDIAN}
  FCGIHeader.ID  := swap(FCGIHeader.ID);
  FCGIHeader.Len := swap(FCGIHeader.Len);
  {$ENDIF}
end;

{
Converts a Request string into a FastCGI Header
@param Buffer Input buffer to convert
@param FCGIHeader FastCGI header converted from Buffer
@see MoveToFCGIHeader
}
procedure MoveFromFCGIHeader(FCGIHeader : TFCGIHeader; var Buffer : AnsiChar); begin
  {$IFNDEF FPC_BIG_ENDIAN}
  FCGIHeader.ID  := swap(FCGIHeader.ID);
  FCGIHeader.Len := swap(FCGIHeader.Len);
  {$ENDIF}
  move(FCGIHeader, Buffer, sizeof(TFCGIHeader));
end;

{
Creates a TFCGIThread to handle a new request to be read from the NewSocket parameter
@param NewSocket Socket to read a new request
}
constructor TFCGIThread.Create(NewSocket : integer); begin
  if Application.FThreadsCount < 0 then Application.FThreadsCount := 0;
  inc(Application.FThreadsCount);
  FSocket := TBlockSocket.Create(NewSocket);
  FRequestHeader := TStringList.Create;
  FRequestHeader.StrictDelimiter := true;
  FSession := TFCGISession(Application.SessionClass.Create(Self));
  FSession.FApplication := Application;
  FSession.ContentType := 'text/html';
  AccessThread := TCriticalSection.Create;
  inherited Create(false);
end;

// Destroys the TFCGIThread invoking the Thread Garbage Collector to free the associated objects
destructor TFCGIThread.Destroy; begin
  AccessThread.Free;
  FSession.Free;
  FRequestHeader.Free;
  dec(Application.FThreadsCount);
  {$IFDEF MSWINDOWS}inherited;{$ENDIF} // Collateral effect of Unix RTL FPC bug
end;

{
Appends or cleans HTTP response header. The HTTP response header is sent using <link TFCGIThread.SendResponse, SendResponse> method.
@param Header Use '' to clean response header else Header parameter is appended to response header
}
procedure TFCGIThread.SetResponseHeader(Header : string); begin
  FSession.FCustomResponseHeaders.Add(Header);
end;

{
Sends a FastCGI response record to the Web Server. Puts the HTTP header in front of response, generates the FastCGI header and sends using sockets.
@param S String to format using FastCGI protocol
@param pRecType FastCGI record type
@see MoveFromFCGIHeader
@see TBlockSocket.SendString
}
procedure TFCGIThread.SendResponse(S : AnsiString; pRecType : TRecType = rtStdOut);
const
  MAX_BUFFER = 65536 - sizeof(TFCGIHeader);
var
  FCGIHeader : TFCGIHeader;
  Buffer : AnsiString;
  I : integer;
begin
  if pRecType = rtStdOut then begin
    if FRequestMethod = rmHead then S := '';
    FSession.CustomResponseHeaders['content-type'] := FSession.ContentType;
    if not BrowserCache and not FSession.IsDownload then FSession.CustomResponseHeaders['cache-control'] := 'no-cache';
    S := AnsiString(FSession.FCustomResponseHeaders.Text) + ^M^J + S;
    FSession.FCustomResponseHeaders.Clear;
    FSession.ContentType := 'text/html';
  end;
  fillchar(FCGIHeader, sizeof(FCGIHeader), 0);
  with FCGIHeader do begin
    Version := 1;
    ID := IfThen(pRecType in [rtGetValuesResult, rtUnknown], 0, FRequestID);
    RecType := pRecType;
    I := 1;
    repeat
      Len := IfThen((length(S)-I+1) <= MAX_BUFFER, length(S)-I+1, MAX_BUFFER);
      PadLen := Len mod 8;
      SetLength(Buffer, sizeof(TFCGIHeader) + Len + PadLen);
      MoveFromFCGIHeader(FCGIHeader, Buffer[1]);
      move(S[I], Buffer[sizeof(TFCGIHeader) + 1], Len);
      inc(I, Len);
      FSocket.SendString(Buffer);
    until I > length(S);
  end;
end;

{
Sends an end request record to the Web Server and ends this thread.
@param Status Status of request. Default is psRequestComplete
}
procedure TFCGIThread.SendEndRequest(Status : TProtocolStatus = psRequestComplete); begin
  if Status <> psRequestComplete then begin
    case Status of
      psCantMPXConn : FSession.Alert('Multiplexing is not allowed.');
      psOverloaded  : FSession.Alert('Maximum connection limit is ' + IntToStr(Application.MaxConns) + ' and was reached.');
      psUnknownRole : FSession.Alert('Unknown FastCGI Role received.');
      psBusy        : ;//Alert('Session is busy, try later.');
    end;
    SendResponse(FSession.EncodeResponse);
  end;
  SendResponse(#0#0#0#0#0#0#0#0, rtEndRequest);
  Terminate;
end;

{
Reads the begin request record from FastCGI request to the thread.
@param FCGIHeader FastCGI Header
@param Content Additional bytes from FastCGI request
}
procedure TFCGIThread.ReadBeginRequest(var FCGIHeader; Content : AnsiString);
var
  BeginRequest : TBeginRequest;
begin
  FLastAccess := Now;
  BeginRequest.Header := TFCGIHeader(FCGIHeader);
  move(Content[1], BeginRequest.Filler, sizeof(BeginRequest)-sizeof(TFCGIHeader));
  if BeginRequest.Role in [rResponder..rFilter] then begin
    FRequestID := BeginRequest.Header.ID;
    FRole      := BeginRequest.Role;
    FKeepConn  := BeginRequest.KeepConn; // can't close socket if true
  end
  else
    SendEndRequest(psUnknownRole);
end;

{
Reads HTTP headers and cookies from FastCGI rtParams record type
@param RequestHeader List of HTTP headers to initialize
@param Stream rtParams record type body
@param Cookies List of HTTP cookies to initialize
}
procedure TFCGIThread.ReadRequestHeader(var RequestHeader : TStringList; Stream : AnsiString; ParseCookies : Boolean = False);
var
  I, Pos : integer;
  Len    : array[0..1] of integer;
  Param  : array[0..1] of AnsiString;
begin
  RequestHeader.Clear;
  if ParseCookies then FSession.FCookies.Clear;
  Pos := 1;
  while Pos < length(Stream) do begin
    for I := 0 to 1 do begin
      Len[I] := byte(Stream[Pos]);
      if Len[I] > 127 then begin
        Len[I] := ((byte(Stream[Pos]) and $7F) shl 24) + (byte(Stream[Pos+1]) shl 16) + (byte(Stream[Pos+2]) shl 8) + byte(Stream[Pos+3]);
        inc(Pos, 4);
      end
      else
        inc(Pos);
      SetLength(Param[I], Len[I]);
    end;
    if Len[0] > 0 then move(Stream[Pos], Param[0][1], Len[0]);
    inc(Pos, Len[0]);
    if Len[1] > 0 then move(Stream[Pos], Param[1][1], Len[1]);
    inc(Pos, Len[1]);
    if Param[0] = 'HTTP_COOKIE' then begin
      if ParseCookies then FSession.FCookies.DelimitedText := URLDecode(string(Param[1]))
    end
    else
      RequestHeader.Values[string(Param[0])] := string(Param[1]);
  end;
end;

// Sets FLastAccess, FPathInfo, FRequestMethod and FQuery internal fields
function TFCGIThread.CompleteRequestHeaderInfo(Buffer : AnsiString; I : integer) : boolean;
var
  ReqMet, CT : string;
begin
  FLastAccess := Now;
  with FSession do begin
    FPathInfo := FRequestHeader.Values['PATH_INFO'];
    if FPathInfo = '' then  // Windows 2003 Server bug
      FPathInfo := copy(FRequestHeader.Values['SCRIPT_NAME'], length(ScriptName) + 1, 100)
    else
      FPathInfo := copy(FPathInfo, 2, 100);
  end;
  ReqMet := FRequestHeader.Values['REQUEST_METHOD'];
  case ReqMet[1] of
    'G' : FRequestMethod := rmGet;
    'P' :
      if ReqMet = 'POST' then
        FRequestMethod := rmPost
      else
        FRequestMethod := rmPut;
    'H' : FRequestMethod := rmHead;
    'D' : FRequestMethod := rmDelete;
  end;
  FSession.SetQueryText(FRequestHeader.Values['QUERY_STRING'], True, False);
  FSession.IsUpload := false;
  CT := FSession.RequestHeader['CONTENT_TYPE'];
  if pos('multipart/form-data', CT) <> 0 then FSession.UploadPrepare(CT, Buffer, I);
  Result := FSession.IsUpload
end;

{
Adds a pair Name/Value to a FastCGI rtGetValuesResult <link TRecType, record type>
@param S Body of rtGetValuesResult <link TRecType, record type>
@param Param Pair Name/Value
}
procedure TFCGIThread.AddParam(var S : string; Param : array of string);
var
  I, J   : integer;
  Len    : array[0..1] of integer;
  Format : array[0..1] of integer;
begin
  for I := 0 to 1 do begin
    Len[I] := length(Param[I]);
    if Len[I] <= 127 then
      Format[I] := 1
    else
      Format[I] := 4;
  end;
  J := length(S);
  SetLength(S, J + Len[0] + Format[0] + Len[1] + Format[1]);
  inc(J);
  for I := 0 to 1 do begin
    if Format[I] = 1 then
      S[J] := char(Len[I])
    else begin
      S[J]   := char(((Len[I] shr 24) and $FF) + $80);
      S[J+1] := char( (Len[I] shr 16) and $FF);
      S[J+2] := char( (Len[I] shr  8) and $FF);
      S[J+3] := char(  Len[I] and $FF);
    end;
    inc(J, Format[I]);
  end;
  move(Param[0][1], S[J], Len[0]);
  move(Param[1][1], S[J + Len[0]], Len[1]);
end;

{
Handles the FastCGI rtGetValues record type and sends a rtgetValuesResult record type
@param Body of rtGetValues record type
}
procedure TFCGIThread.GetValues(Content : AnsiString);
var
  Values : TStringList;
  GetValuesResult : string;
begin
  if Content = '' then exit;
  Values := TStringList.Create;
  ReadRequestHeader(Values, Content);
  GetValuesResult := '';
  if Values.IndexOf('FCGI_MAX_CONNS')  <> -1 then AddParam(GetValuesResult, ['FCGI_MAX_CONNS', IntToStr(Application.MaxConns)]);
  if Values.IndexOf('FCGI_MAX_REQS')   <> -1 then AddParam(GetValuesResult, ['FCGI_MAX_REQS',  IntToStr(Application.MaxConns)]);
  if Values.IndexOf('FCGI_MPXS_CONNS') <> -1 then AddParam(GetValuesResult, ['FCGI_MPXS_CONNS', '0']);
  Values.Free;
  SendResponse(AnsiString(GetValuesResult), rtGetValuesResult);
end;

{
Sets the context of current thread to the context of associated session using a cookie (<b>FCGIThread</b>).
When a browser session sends its first request this method associates the current browser session, this first thread, to a new cookie (<b>FCGIThread</b>),
whose value is a <extlink http://en.wikipedia.org/wiki/GUID>GUID</extlink>.
In subsequent requests this cookie is the key to find the browser session, i.e. the original <link TFCGIThread, Thread>.
In this way a statefull and multi-thread behavior is provided.
@return False if it fails to find the session associated with the cookie, for example if the session already expired.
}
function TFCGIThread.SetCurrentFCGIThread : boolean;
var
  Thread : string;
  GUID : TGUID;
  I : integer;
begin
  Result := true;
  Application.AccessThreads.Enter;
  try
    Thread := FSession.Cookie['FCGIThread'];
    if Thread = '' then begin
      CreateGUID(GUID);
      Thread := GUIDToString(GUID);
      FSession.SetCookie('FCGIThread', Thread);
      I := -1
    end
    else
      I := Application.Threads.IndexOf(Thread);
    if I = -1 then begin
      FSession.NewThread := true;
      AccessThread.Enter;
      if Application.ReachedMaxConns then begin
        SendEndRequest(psOverloaded);
        Result := false;
      end
      else begin
        Application.Threads.AddObject(Thread, Self);
        FSession.AfterNewSession;
      end;
      with FSession do begin
        FScriptName := Self.FRequestHeader.Values['SCRIPT_NAME'];
        if (FScriptName = '') or (FScriptName[length(FScriptName)] <> '/') then FScriptName := FScriptName + '/';
      end;
    end
    else begin
      _CurrentFCGIThread := TFCGIThread(Application.Threads.Objects[I]);
      _CurrentFCGIThread.AccessThread.Enter;
      CurrentWebSession := _CurrentFCGIThread.FSession;
      StrToTStrings(FRequestHeader.DelimitedText, _CurrentFCGIThread.FRequestHeader);
      StrToTStrings(FSession.FCookies.DelimitedText, _CurrentFCGIThread.FSession.FCookies);
      _CurrentFCGIThread.FSession.NewThread := false;
      _CurrentFCGIThread.FSession.FCustomResponseHeaders.Clear;
      _CurrentFCGIThread.FSession.ContentType := 'text/html';
      FreeOnTerminate := True;
    end;
  finally
    Application.AccessThreads.Leave;
  end;
end;

{
The thread main loop.<p>
On receive a request, each request, on its execution cycle, does:
  * <link MoveToFCGIHeader, Reads its FCGI header>
  * Depending on <link TRecType, record type> does:
    * <link TFCGIThread.ReadBeginRequest, Starts a request> or
    * <link TFCGIThread.Logout, Aborts the request> or
    * <link TFCGIThread.SendEndRequest, Ends the request> or
    * <link TFCGIThread.ReadRequestHeader, Reads HTTP headers> or
    * <link TFCGIThread.HandleRequest, Handles the request> with these internal steps:
      * <link TFCGIThread.BeforeHandleRequest, BeforeHandleRequest>
      * The <link TFCGIThread.HandleRequest, HandleRequest> own method
      * <link TFCGIThread.AfterHandleRequest, AfterHandleRequest>
}
procedure TFCGIThread.Execute;
var
  FCGIHeader : TFCGIHeader;
  Buffer, Content : AnsiString;
  I : integer;
begin
  _CurrentFCGIThread := Self;
  CurrentWebSession := FSession;
  FRequest := '';
  try
    if Application.CanConnect(string(FSocket.GetHostAddress)) then
      repeat
        if FSocket.WaitingData > 0 then begin
          Buffer := FSocket.RecvPacket;
          if FSocket.Error <> 0 then
            Terminate
          else begin
            I := 1;
            while I <= length(Buffer) do begin
              MoveToFCGIHeader(Buffer[I], FCGIHeader);
              if (FRequestID <> 0) and (FCGIHeader.ID <> 0) and (FCGIHeader.ID <> FRequestID) then
                SendEndRequest(psCantMPXConn)
              else begin
                inc(I, sizeof(FCGIHeader));
                Content := copy(Buffer, I, FCGIHeader.Len);
                case FCGIHeader.RecType of
                  rtBeginRequest : ReadBeginRequest(FCGIHeader, Content);
                  rtAbortRequest : FSession.Logout;
                  rtGetValues    : GetValues(Content);
                  rtParams, rtStdIn, rtData :
                    if Content = '' then begin
                      if FCGIHeader.RecType = rtParams then begin
                        ReadRequestHeader(FRequestHeader, AnsiString(FRequest), True);
                        if SetCurrentFCGIThread then begin
                          FSession.IsUpload := _CurrentFCGIThread.CompleteRequestHeaderInfo(Buffer, I);
                          FSession.MaxUploadSize := _CurrentFCGIThread.FSession.MaxUploadSize;
                          if FSession.IsUpload then begin
                            FSession.FFileUploaded := _CurrentFCGIThread.FSession.FFileUploaded;
                            FSession.FFileUploadedFullName := _CurrentFCGIThread.FSession.FFileUploadedFullName;
                            FSession.Response    := _CurrentFCGIThread.FSession.Response;
                            FSession.UploadMark  := _CurrentFCGIThread.FSession.UploadMark;
                          end;
                        end
                        else
                          break;
                      end
                      else begin
                        _CurrentFCGIThread.FSession.IsUpload := FSession.IsUpload;
                        _CurrentFCGIThread.FSession.Response := FSession.Response;
                        FSession.Response := string(_CurrentFCGIThread.HandleRequest(AnsiString(FRequest)));
                        FSession.FCustomResponseHeaders.Text := _CurrentFCGIThread.FSession.FCustomResponseHeaders.Text;
                        FSession.ContentType := _CurrentFCGIThread.FSession.ContentType;
                        FGarbage := _CurrentFCGIThread.FGarbage;
                        FSession.IsDownload := _CurrentFCGIThread.FSession.IsDownload;
                        if (FSession.Response <> '') or (RequestMethod in [rmGet, rmHead]) then begin
                          if FSession.IsDownload then
                            SendResponse(AnsiString(FSession.Response))
                          else
                            SendResponse(FSession.EncodeResponse);
                        end;
                        SendEndRequest;
                      end;
                      FRequest := '';
                    end
                    else
                      if FSession.IsUpLoad then
                        FSession.UploadWriteFile(Content)
                      else
                        FRequest := FRequest + string(Content);
                else
                  SendResponse(AnsiChar(FCGIHeader.RecType), rtUnknown);
                  Buffer := '';
                  sleep(200);
                  break;
                end;
              end;
              inc(I, FCGIHeader.Len + FCGIHeader.PadLen);
            end;
          end;
        end
        else
          sleep(5);
      until Terminated
    else
      Terminate;
  except
    on E : Exception do begin
      Content := AnsiString(E.ClassName + ': ' + E.Message + ' at ' + IntToStr(integer(ExceptAddr)));
      SendResponse(Content);
      SendResponse(Content, rtStdErr);
      SendEndRequest;
    end;
  end;
  _CurrentFCGIThread.AccessThread.Leave;
  FSocket.Free;
  if FGarbage then begin
    _CurrentFCGIThread.FLastAccess := 0;
    FLastAccess := 0;
    Application.GarbageNow := true;
  end;
  {$IFNDEF MSWINDOWS}EndThread(0){$ENDIF} // Unix RTL FPC bug
end;

{
Calls the published method indicated by PathInfo. Before calls <link TFCGIThread.BeforeHandleRequest, BeforeHandleRequest> method and after calls <link TFCGIThread.AfterHandleRequest, AfterHandleRequest> method.
If PathInfo is null then <link TFCGIThread.Home, Home> method will be called.
The published method will use the Request as input and the Response as output.
<link TFCGIThread.OnError, OnError> method is called if an exception is raised in published method.
<link TFCGIThread.OnNotFoundError, OnNotFoundError> method is called if the published method is not declared in this thread.
@param pRequest Request body assigned to FRequest field or to <link TFCGIThread.Query, Query> array if FRequestMethod is <link TRequestMethod, rmPost>, it is the input to the published method
@return Response body to <link TFCGIThread.SendResponse, send>
}
function TFCGIThread.HandleRequest(pRequest : AnsiString) : AnsiString; begin
  if (FRequestMethod = rmPost) and (Pos(AnsiString('='), pRequest) <> 0) then
    FSession.SetQueryText(string(pRequest), True, True)
  else
    FRequest := string(pRequest);
  if not FSession.IsUpload then FSession.Response := '';
  FSession.IsDownload := false;
  FSession.HandleRequest(pRequest);
  if FSession.IsDownload or (FSession.IsUpload and (FSession.Browser = brIE)) then
    Result := AnsiString(FSession.Response)
  else
    Result := FSession.EncodeResponse;
end;

{$IFDEF HAS_CONFIG}

// Reads FastCGI port and Password from the configuration file
procedure TFCGIApplication.ReadConfig;
var
  ConfigFile : string;
begin
  FConfig := nil;
  ConfigFile := ChangeFileExt(ParamStr(0), {$IFDEF MSWINDOWS}'.ini'{$ELSE}'.conf'{$ENDIF});
  if FileExists(ConfigFile) then begin
    FConfig := TINIFile.Create(ConfigFile);
    // changing below options requires restart
    FPort := Config.ReadInteger('FCGI', 'Port', Port);
    FPassword := Config.ReadString('FCGI', 'Password', Password);
  end;
end;

{
Reads MaxIdleTime, MaxConns, Shutdown and WServers fields from the config file
@param AReload If true reload from disk
}
function TFCGIApplication.Reconfig(AReload : Boolean = true) : Boolean;
var
  H, M, S, MS : word;
  ConfigFile, WServers : string;
begin
  Result := true;
  if HasConfig then begin
    // force refresh in-memory data
    if AReload then begin
      ConfigFile := Config.FileName;
      Config.Free;
      Sleep(100);
      FConfig := TINIFile.Create(ConfigFile);
    end;
    DecodeTime(MaxIdleTime, H, M, S, MS);
    MaxIdleTime := EncodeTime(0, Config.ReadInteger('FCGI', 'MaxIdle', M), 0, 0);
    FMaxConns   := Config.ReadInteger('FCGI', 'MaxConn', MaxConns);
    Shutdown    := Config.ReadBool('FCGI', 'AutoOff', Shutdown);
    WServers    := Config.ReadString('FCGI', 'InServers', '');
    if WServers <> '' then begin
      if WebServers = nil then WebServers := TStringList.Create;
      WebServers.DelimitedText := WServers;
    end;
  end;
end;
{$ENDIF}

// Frees a TFCGIApplication
destructor TFCGIApplication.Destroy; begin
  {$IFDEF HAS_CONFIG}Config.Free;{$ENDIF}
  Threads.Free;
  AccessThreads.Free;
  WebServers.Free;
  inherited;
end;

{
Creates a FastCGI application instance.
@param pTitle Application title used by <link TExtThread.AfterHandleRequest, AfterHandleRequest>
@param pFCGIThreadClass Thread class type to create when a new request arrives
@param pPort TCP/IP port used to comunicate with the Web Server, default is 2014
@param pMaxIdleMinutes Minutes of inactivity before the end of the thread, releasing it from memory, default is 30 minutes
@param pShutdownAfterLastThreadDown If true Shutdown the application after the last thread to end, default is false. Good for commercial CGI hosting.
@param pMaxConns Maximum accepted connections, default is 1000
}
constructor TFCGIApplication.Create(pTitle : string; ASessionClass: TCustomWebSessionClass; pPort : word = 2014;
                                    pMaxIdleMinutes : word = 30; pShutdownAfterLastThreadDown : boolean = false;
                                    pMaxConns : integer = 1000);
var
  WServers : string;
begin
  inherited Create(pTitle, ASessionClass, pPort, pMaxIdleMinutes, pMaxConns);
  Assert(Assigned(ASessionClass) and ASessionClass.InheritsFrom(TFCGISession));
  Threads := TStringList.Create;
  AccessThreads := TCriticalSection.Create;
  MaxIdleTime := EncodeTime(0, pMaxIdleMinutes, 0, 0);
  Shutdown := pShutdownAfterLastThreadDown;
  WServers := GetEnvironmentVariable('FCGI_WEB_SERVER_ADDRS');
  if WServers <> '' then begin
    WebServers := TStringList.Create;
    WebServers.DelimitedText := WServers;
  end;
  FExeName := ExtractFileName(ParamStr(0));
  {$IFDEF HAS_CONFIG}
  ReadConfig;
  Reconfig(false);
  {$ENDIF}
end;

{
Tests if Address parameter is an IP address in WebServers list
@param Address IP address to find
@return True if Address is in WebServers list
}
function TFCGIApplication.CanConnect(Address : string) : boolean; begin
  Result := (WebServers = nil) or (WebServers.IndexOf(Address) <> -1)
end;

{
Tests if MaxConns (max connections), default is 1000, was reached
@return True if was reached
}
function TFCGIApplication.ReachedMaxConns : boolean; begin
  Result := Threads.Count >= MaxConns
end;

// Thread Garbage Collector. Frees all expired threads
procedure TFCGIApplication.GarbageThreads;
var
  I : integer;
  Thread : TFCGIThread;
begin
  for I := Threads.Count-1 downto 0 do begin
    Thread := TFCGIThread(Threads.Objects[I]);
    if (Now - Thread.LastAccess) > MaxIdleTime then begin
      AccessThreads.Enter;
      Thread.Free;
      Threads.Delete(I);
      AccessThreads.Leave;
    end;
  end;
end;

// Frees all threads regardless of expiration
procedure TFCGIApplication.TerminateAllThreads;
var
  I: Integer;
  Thread: TFCGIThread;
begin
  AccessThreads.Enter;
  try
    for I := Threads.Count - 1 downto 0 do begin
      Thread := TFCGIThread(Threads.Objects[I]);
      Thread.Free;
      Threads.Delete(I);
    end;
  finally
    AccessThreads.Leave;
  end;
end;

function TFCGIApplication.GetTerminated : Boolean; begin
  Result := inherited GetTerminated or (Shutdown and (ThreadsCount = 0));
end;

{
Returns the Ith thread
@param I Index of the thread to return
}
function TFCGIApplication.GetThread(I : integer) : TThread; begin
  Result := TThread(Threads.Objects[I])
end;

{
Handles "Port #### already in use" error. Occurs when the port is already in use for another service or application.
Can be overrided in descendent thread class. It shall be overrided if the application is a service.
@see Create
@see DoRun
}
procedure TFCGIApplication.OnPortInUseError; begin
  writeln('Port: ', Port, ' already in use.');
  sleep(10000);
end;

{
The application main loop. Listens a socket port, through which it accepts connections from a Web server.
For each connection a <link TFCGIThread> is <link TFCGIThread.Create, created> that executes the FCGI protocol
to <link TFCGIThread.ReadRequestHeader, receive> and <link TFCGIThread.SendResponse, send> data.
@param OwnerThread Optional parameter to use with a service thread, see ExtPascalSamples.pas.
When this thread is terminated the application is terminated too.
}
procedure TFCGIApplication.DoRun;
var
  NewSocket, I : integer;
begin
  {$IFDEF HAS_CONFIG}
  {$IFDEF CONFIG_MUST_EXIST}
  if not HasConfig then begin
    writeln('Config: Required configuration file is not found.');
    sleep(10000);
    exit;
  end;
  {$ENDIF}
  if HasConfig then
    if not Config.ReadBool('FCGI', 'Enabled', true) then begin
      writeln('Config: Application is being disabled by config file.');
      sleep(10000);
      exit;
    end;
  {$ENDIF}
  I := 0;
  FThreadsCount := -1;
  with TBlockSocket.Create do begin
    try
      Bind(Port, 1000);
      if Error = 0 then
        repeat
          NewSocket := Accept(250);
          if NewSocket <> 0 then TFCGIThread.Create(NewSocket);
          if ((I mod 40) = 0) or GarbageNow then begin // A garbage for each 10 seconds
            GarbageThreads;
            GarbageNow := false;
            I := 0;
          end;
          inc(I);
        until Terminated
      else
        OnPortInUseError;
    finally
      Free;
    end;
  end;
end;

// Returns the number of active threads
function TFCGIApplication.ThreadsCount : integer; begin
  Result := FThreadsCount
end;

{ TFCGISession }

constructor TFCGISession.Create(AOwner : TObject); begin
  inherited Create(AOwner);
  FOwner := AOwner as TFCGIThread;
end;

function TFCGISession.CanCallAfterHandleRequest : Boolean; begin
  Result := True;
end;

function TFCGISession.CanHandleUrlPath : Boolean; begin
  Result := not IsUpload or (Pos('success:true', Response) <> 0);
end;

destructor TFCGISession.Destroy;
begin
  // Make sure objects find the session threadvar assigned when they are
  // being garbage collected in case the session is being freed by a
  // different thread. Otherwise objects don't mark themselves off the
  // GC upon destruction and risk to be destroyed multiple times.
  CurrentWebSession := Self;
  inherited;
end;

procedure TFCGISession.DoLogout; begin
  inherited;
  TFCGIThread(FOwner).FGarbage := True;
end;

procedure TFCGISession.DoSetCookie(const Name, ValueRaw : string); begin
  CustomResponseHeaders['Set-Cookie'] := Name + '=' + ValueRaw;
end;

class function TFCGISession.GetCurrentWebSession : TCustomWebSession; begin
  Result := CurrentWebSession;
end;

function TFCGISession.GetDocumentRoot : string; begin
  Result := RequestHeader['DOCUMENT_ROOT'];
  if (Result <> '') and CharInSet(Result[Length(Result)], ['/', '\']) then
    Delete(Result, Length(Result), 1);
end;

function TFCGISession.GetRequestHeader(const Name : string) : string; begin
  Result := TFCGIThread(FOwner).FRequestHeader.Values[Name];
end;

function TFCGISession.GetWebServer : string; begin
  Result := RequestHeader['Server_Software'];
  if Result = '' then Result := 'Embedded';
end;

procedure TFCGISession.SendResponse(const Msg : AnsiString); begin
  TFCGIThread(FOwner).SendResponse(Msg);
end;

function TFCGISession.UploadBlockType(const Buffer : AnsiString; var MarkPos : Integer) : TUploadBlockType; begin
  MarkPos := Pos(UploadMark, Buffer);
  case MarkPos of
    0 : Result := ubtMiddle;
    1 : Result := ubtBegin;
  else
    Result := ubtEnd;
  end;
end;

function TFCGISession.UploadNeedUnknownBlock : Boolean; begin
  Result := True;
end;

initialization

finalization
  if Application <> nil then Application.Free;
end.
