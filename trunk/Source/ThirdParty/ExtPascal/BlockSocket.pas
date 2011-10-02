{
Implements <link TBlockSocket> class for TCP/IP protocol
Author: Wanderlan Santos dos Anjos (wanderlan.anjos@gmail.com)
Date: jul-2008
License: BSD<extlink http://www.opensource.org/licenses/bsd-license.php>BSD</extlink>
}
unit BlockSocket;

interface

uses
  {$IFDEF FPC}Sockets{$ELSE}SocketsDelphi{$ENDIF};

type
  // Implements a block <extlink http://en.wikipedia.org/wiki/Internet_socket>socket</extlink> as a class
  TBlockSocket = class
  private
    Socket : TSocket;
		RemoteSin : TInetSockAddr;
  public
    constructor Create(S : integer = 0);
		destructor Destroy; override;
    procedure Bind(pPort, BackLog : word);
		function Accept(Timeout : integer) : integer;
    procedure Connect(Host : string; pPort : word);
		procedure Purge;
		procedure Close;
    function RecvPacket : AnsiString;
		function RecvString(Timeout : integer = 300) : AnsiString;
    procedure SendString(const Data: AnsiString);
    function WaitingData : cardinal;
    function CanRead(Timeout: Integer): Boolean;
    function Error : integer;
    function GetHostAddress : string;
  end;

implementation

uses
  SysUtils,
  {$IFDEF MSWINDOWS}
    Windows, {$IFDEF FPC}WinSock2{$ELSE}WinSockDelphi{$ENDIF}
  {$ELSE}
    {$DEFINE IOCtlSocket:=fpIOCtl}{$DEFINE FD_Zero:=fpFD_Zero}{$DEFINE FD_Set:=fpFD_Set}{$DEFINE Select:=fpSelect}
    BaseUnix, TermIO
  {$ENDIF};

{
Puts the socket in listening state, used on the server side
@param Port Port to listen
@param Backlog The maximum length of the queue of pending connections
@see Error
}
procedure TBlockSocket.Bind(pPort, BackLog : word); begin
  with RemoteSin do begin
    Sin_Family := AF_INET;
    Sin_Addr.s_Addr := 0;
    Sin_Port   := htons(pPort);
  end;
  {$IFNDEF MSWINDOWS}fpSetSockOpt(Socket, SOL_SOCKET, SO_REUSEADDR, @RemoteSin, SizeOf(RemoteSin));{$ENDIF} // remedy socket port locking on Posix platforms
  fpBind(Socket, @RemoteSin, sizeof(RemoteSin));
  fpListen(Socket, BackLog);
end;

{
Attempts to establish a new TCP connection, used on the client side
@param Host IP address to the server machine
@param Port Port number to connect
@see Error
}
procedure TBlockSocket.Connect(Host : string; pPort : word); begin
  with RemoteSin do begin
    Sin_Family := AF_INET;
    Sin_Addr   := StrToNetAddr(Host);
    Sin_Port   := htons(pPort);
  end;
  fpConnect(Socket, @RemoteSin, sizeof(RemoteSin));
end;

{
Creates a new socket stream
@param S Assigns an existing socket to the TBlockSocket
}
constructor TBlockSocket.Create(S : integer = 0); begin
  {$IFNDEF MSWINDOWS}fpSetErrNo(0);{$ENDIF}
	if S = 0 then
  	Socket := fpSocket(AF_INET, SOCK_STREAM, 0)
	else
		Socket := S;
end;

// Closes the socket and frees the object
destructor TBlockSocket.Destroy; begin
  Close;
	inherited;
end;

// Returns the host IP address
function TBlockSocket.GetHostAddress: string;
var
  Tam : integer;
  Addr: SockAddr;
begin
  Tam := sizeof(Addr);
  fpGetSockName(Socket, @Addr, @Tam);
  Result := NetAddrToStr(Addr.Sin_Addr);
end;

// Closes the socket
procedure TBlockSocket.Close; begin
  CloseSocket(Socket);
end;

{
Attempts to accept a new TCP connection from the client and creates a new socket to handle this new connection, used on server side
@param Timeout Time in milliseconds to wait to accept a new connection
@return A new socket handler or 0 if none connection was accepted
}
function TBlockSocket.Accept(Timeout : integer) : integer;
var
	Tam : integer;
begin
  if CanRead(Timeout) then begin
    sleep(0);
    Tam := sizeof(RemoteSin);
    Result := fpAccept(Socket, @RemoteSin, @Tam)
  end
  else
    Result := 0;
end;

// Returns number of bytes waiting to read
function TBlockSocket.WaitingData : cardinal;
var
  Tam : dword;
begin
  sleep(1);
	if IOCtlSocket(Socket, FIONREAD, @Tam) <> 0 then
    Result := 0
  else
    Result := Tam;
end;

{
Tests if data are available for reading within the timeout
@param Timeout Max time to wait until returns
@return True if data are available, false otherwise
}
function TBlockSocket.CanRead(Timeout : Integer) : Boolean;
var
  FDS: TFDSet;
  TimeV: TTimeVal;
begin
  FD_Zero(FDS);
  FD_Set(Socket, FDS);
  TimeV.tv_usec := (Timeout mod 1000) * 1000;
  TimeV.tv_sec := Timeout div 1000;
  Result := Select(Socket + 1, @FDS, nil, nil, @TimeV) > 0;
end;

// Cleans the socket input stream
procedure TBlockSocket.Purge;
var
	Tam : cardinal;
	Buffer : pointer;
begin
  Tam := WaitingData;
	if Tam = 0 then exit;
	getmem(Buffer, Tam);
	fpRecv(Socket, Buffer, Tam, 0);
	freemem(Buffer, Tam);
end;

// Receives the next available tcp packet
function TBlockSocket.RecvPacket : AnsiString;
var
	Tam : integer;
begin
  Tam := WaitingData;
  SetLength(Result, Tam);
  {$IFNDEF MSWINDOWS}fpSetErrNo(0);{$ENDIF}
  fpRecv(Socket, @Result[1], Tam, 0);
  Sleep(1); // required on some environments to prevent streaming truncation
end;

{
Returns the socket input stream as a string
@param Timeout Max time to wait until some data is available for reading. Default is 300 miliseconds
@see Error
}
function TBlockSocket.RecvString(Timeout : integer = 300) : AnsiString; begin
  Result := '';
  if CanRead(Timeout) then
    while WaitingData <> 0 do
      Result := Result + RecvPacket
end;

{
Sends a string by the socket
@param Data String to send
@see Error
}
procedure TBlockSocket.SendString(const Data : AnsiString); begin
	fpSend(Socket, @Data[1], length(Data), 0);
end;

{
Use Error method to test if others TBlockSocket methods succeded
@return 0 if success or socket error code otherwise
}
function TBlockSocket.Error : integer; begin
  Result := SocketError
end;

end.
