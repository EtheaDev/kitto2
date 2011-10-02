// Source adapted from the Free Pascal run time library (sockets.pp) by wanderlan.anjos@gmail.com to Delphi
unit SocketsDelphi;

interface

uses
  WinSockDelphi;

type
  cint8                  = shortint;           pcint8                 = ^cint8;
  cuint8                 = byte;               pcuint8                = ^cuint8;
  cchar                  = cint8;              pcchar                 = ^cchar;
  cschar                 = cint8;              pcschar                = ^cschar;
  cuchar                 = cuint8;             pcuchar                = ^cuchar;

  cint16                 = smallint;           pcint16                = ^cint16;
  cuint16                = word;               pcuint16               = ^cuint16;
  cshort                 = cint16;             pcshort                = ^cshort;
  csshort                = cint16;             pcsshort               = ^csshort;
  cushort                = cuint16;            pcushort               = ^cushort;

  cint32                 = longint;            pcint32                = ^cint32;
  cuint32                = longword;           pcuint32               = ^cuint32;
  cint                   = cint32;             pcint                  = ^cint;
  csint                  = cint32;             pcsint                 = ^csint;
  cuint                  = cuint32;            pcuint                 = ^cuint;
  csigned                = cint;               pcsigned               = ^csigned;
  cunsigned              = cuint;              pcunsigned             = ^cunsigned;

  cint64                 = int64;              pcint64                = ^cint64;
  clonglong              = cint64;             pclonglong             = ^clonglong;
  cslonglong             = cint64;             pcslonglong            = ^cslonglong;

  cbool                  = longbool;           pcbool                 = ^cbool;

  cfloat                 = single;             pcfloat                = ^cfloat;
  cdouble                = double;             pcdouble               = ^cdouble;
  clongdouble            = extended;           pclongdouble           = ^clongdouble;

  size_t  = cuint32;
  ssize_t = cint32;
  tsocklen= cint;
  psocklen= ^tsocklen;

const
  EsockEINTR           = WSAEINTR;
  EsockEBADF           = WSAEBADF;
  EsockEFAULT          = WSAEFAULT;
  EsockEINVAL          = WSAEINVAL;
  EsockEACCESS         = WSAEACCES;
  EsockEMFILE          = WSAEMFILE;
  EsockEMSGSIZE        = WSAEMSGSIZE;
  EsockENOBUFS         = WSAENOBUFS;
  EsockENOTCONN        = WSAENOTCONN;
  EsockENOTSOCK        = WSAENOTSOCK;
  EsockEPROTONOSUPPORT = WSAEPROTONOSUPPORT;
  EsockEWOULDBLOCK     = WSAEWOULDBLOCK;

type
{$ifdef SOCK_HAS_SINLEN}
  sa_family_t = cuchar;
{$else}
  sa_family_t = cushort;
{$endif}

const
  { Socket types }
  SOCK_STREAM     = 1;               { stream (connection) socket   }
  SOCK_DGRAM      = 2;               { datagram (conn.less) socket  }
  SOCK_RAW        = 3;               { raw socket                   }
  SOCK_RDM        = 4;               { reliably-delivered message   }
  SOCK_SEQPACKET  = 5;               { sequential packet socket     }

  INADDR_ANY   = CARDINAL(0);
  INADDR_NONE  = CARDINAL($FFFFFFFF);

  { Two constants to determine whether part of soket is for in or output }
  S_IN = 0;
  S_OUT = 1;

type
  pin_addr = ^in_addr;
  in_addr = packed record
    case boolean of
       true: (s_addr  : cuint32);         // inaddr_t=cuint32
       false: (s_bytes : packed array[1..4] of byte);
  end;

  TIn_addr = in_addr;

  TInAddr = in_addr;
  PInAddr = pin_addr;

  {pin_addrbytes = ^in_addrbytes;
  in_addrbytes = packed array [1..4] of byte;}

  psockaddr_in = ^sockaddr_in;
  sockaddr_in = packed record
    case boolean of
     false : (
  {$ifdef SOCK_HAS_SINLEN}
     sin_len     : cuchar;
  {$endif}
     sin_family  : sa_family_t;
     sin_port    : cushort;
     sin_addr    : in_addr;
     xpad         : array [0..7] of char; { to get to the size of sockaddr... }
      );
   true: (
  {$ifdef SOCK_HAS_SINLEN}
     len     : cuchar;
  {$endif}
     family  : sa_family_t;
     port    : cushort;
     addr    : cardinal;
     pad         : array [0..7] of char; { to get to the size of sockaddr... }
      );
  end;

  TInetSockAddr = sockaddr_in;
  PInetSockAddr = psockaddr_in;

  psockaddr = ^sockaddr;
  sockaddr = packed record // if sa_len is defined, sa_family_t is smaller
  {$ifdef SOCK_HAS_SINLEN}
     sa_len     : cuchar;
  {$endif}
    case integer of
      0: (sa_family: sa_family_t;
          sa_data: packed array[0..13] of cuint8);
      1: (sin_family: sa_family_t;
          sin_port: cushort;
          sin_addr: in_addr;
          sin_zero: packed array[0..7] of cuint8);
  end;

  TSockAddr = sockaddr;

  plinger = ^linger;
  linger = packed record
    l_onoff  : cint;	(* Linger active		*)
    l_linger : cint;	(* How long to linger for	*)
  end;

  TLinger = linger;

  pin6_addr = ^in6_addr;
  in6_addr = packed record
  case byte of
    0: (u6_addr8  : array[0..15] of byte);
    1: (u6_addr16 : array[0..7] of Word);
    2: (u6_addr32 : array[0..3] of Cardinal);
    3: (s6_addr8  : array[0..15] of shortint);
    4: (s6_addr   : array[0..15] of shortint);
    5: (s6_addr16 : array[0..7] of smallint);
    6: (s6_addr32 : array[0..3] of LongInt);
  end;

  Tin6_addr = in6_addr;

  TIn6Addr = in6_addr;
  PIn6Addr = pin6_addr;

  psockaddr_in6 = ^sockaddr_in6;
  sockaddr_in6 = packed Record
    {$ifdef SOCK_HAS_SINLEN}  // as per RFC 2553
      sin6_len    : cuint8;
    {$endif}
    sin6_family   : sa_family_t;
    sin6_port     : cuint16;
    sin6_flowinfo : cuint32;
    sin6_addr     : in6_addr;
    sin6_scope_id : cuint32;
  end;

  TInetSockAddr6 = sockaddr_in6;
  PInetSockAddr6 = psockaddr_in6;

  TSockPairArray = Array[0..1] of Longint;
  TSockArray     = Array[1..2] of Longint;              //legacy

  psockaddr_un = ^sockaddr_un;
  sockaddr_un = packed record
    {$ifdef SOCK_HAS_SINLEN}
      sun_len     : cuint8;
    {$endif}
    sun_family    : sa_family_t;
    sun_path      : array[0..107] of char;
  end;

  Tsocket=longint;   {To easy porting code from Kylix libc unit to sockets unit.}

function socketerror : cint;

function  fpsocket      (domain:cint; xtype:cint; protocol: cint):cint;
function  fprecv        (s:cint; buf: pointer; len: size_t; flags: cint):ssize_t;
function  fpsend        (s:cint; msg:pointer; len:size_t; flags:cint):ssize_t;
function  fpbind        (s:cint; addrx : psockaddr; addrlen : tsocklen):cint;
function  fplisten      (s:cint; backlog : cint):cint;
function  fpaccept      (s:cint; addrx : psockaddr; addrlen : plongint):cint;
function  fpconnect     (s:cint; name  : psockaddr; namelen : tsocklen):cint;
function  fpgetsockname (s:cint; name  : psockaddr; namelen : psocklen):cint;

{ Utility routines}
function NetAddrToStr (Entry : in_addr) : AnsiString;
function HostAddrToStr(Entry : in_addr) : AnsiString;
function StrToHostAddr(IP : AnsiString) : in_addr ;
function StrToNetAddr (IP : AnsiString) : in_addr;

const
	NoAddress : in_addr  = (s_addr:0);
	NoNet     : in_addr  = (s_addr:0);
 	NoAddress6: in6_addr = (u6_addr16:(0,0,0,0,0,0,0,0));
  NoNet6    : in6_addr = (u6_addr16:(0,0,0,0,0,0,0,0));

  PF_UNSPEC       = AF_UNSPEC;
  PF_UNIX         = AF_UNIX;
  PF_INET         = AF_INET;
  PF_IMPLINK      = AF_IMPLINK;
  PF_PUP          = AF_PUP;
  PF_CHAOS        = AF_CHAOS;
  PF_NS           = AF_NS;
  PF_IPX          = AF_IPX;
  PF_ISO          = AF_ISO;
  PF_OSI          = AF_OSI;
  PF_ECMA         = AF_ECMA;
  PF_DATAKIT      = AF_DATAKIT;
  PF_CCITT        = AF_CCITT;
  PF_SNA          = AF_SNA;
  PF_DECnet       = AF_DECnet;
  PF_DLI          = AF_DLI;
  PF_LAT          = AF_LAT;
  PF_HYLINK       = AF_HYLINK;
  PF_APPLETALK    = AF_APPLETALK;
  PF_VOICEVIEW    = AF_VOICEVIEW;
  PF_FIREFOX      = AF_FIREFOX;
  PF_UNKNOWN1     = AF_UNKNOWN1;
  PF_BAN          = AF_BAN;
  PF_ATM          = AF_ATM;
  PF_INET6        = AF_INET6;
  PF_MAX          = AF_MAX;

  SOL_SOCKET = $ffff;

implementation

function socketerror:cint; begin
  result:=wsagetlasterror;
end;

function fpsocket(domain:cint; xtype:cint; protocol: cint):cint; begin
  fpSocket:=WinSockDelphi.Socket(Domain,xtype,ProtoCol);
end;

function fpsend(s:cint; msg:pointer; len:size_t; flags:cint):ssize_t; begin
  fpSend:=WinSockDelphi.Send(S,msg,len,flags);
end;

function fprecv(s:cint; buf: pointer; len: size_t; flags: cint):ssize_t; begin
  fpRecv:=WinSockDelphi.Recv(S,Buf,Len,Flags);
end;

function fpconnect(s:cint; name : psockaddr; namelen : tsocklen):cint; begin
  fpConnect := WinSockDelphi.Connect(S,WinSockDelphi.PSockAddr(name),nameLen);
end;

function fpbind(s:cint; addrx : psockaddr; addrlen : tsocklen):cint; begin
  fpbind:=WinSockDelphi.Bind(S,WinSockDelphi.PSockAddr(Addrx),AddrLen);
end;

function fplisten(s:cint; backlog : cint):cint; begin
  fplisten:=WinSockDelphi.Listen(S,backlog);
end;

function fpaccept(s:cint; addrx : psockaddr; addrlen : plongint):cint; begin
  fpAccept:=WinSockDelphi.Accept(S,WinSockDelphi.PSockAddr(Addrx), AddrLen);
end;

function fpgetsockname(s:cint; name : psockaddr; namelen : psocklen):cint; begin
  fpGetSockName:=WinSockDelphi.GetSockName(S, WinSockDelphi.PSockAddr(name)^,nameLen^);
end;

type
  array4int = array[1..4] of byte;

function NetAddrToStr (Entry : in_addr) : AnsiString;
var
  Dummy : Ansistring;
  i,j   : Longint;
begin
  NetAddrToStr:='';
  j:=entry.s_addr;
  for I:=1 to 4 do begin
    Str(array4int(j)[i],Dummy);
    NetAddrToStr:=result+Dummy;
    if I<4 then NetAddrToStr:=result+'.';
  end;
end;

function HostAddrToStr (Entry : in_addr) : AnsiString;
var
  x: in_addr;
begin
  x.s_addr:=htonl(entry.s_addr);
  HostAddrToStr:=NetAddrToStr(x);
end;

function StrToHostAddr(IP : AnsiString) : in_addr ;
var
  Dummy : AnsiString;
  I,j,k : Longint;
begin
  Result.s_addr:=0;
  for I:=1 to 4 do begin
    if I<4 then begin
      J:=Pos('.',IP);
      If J=0 then exit;
      Dummy:=Copy(IP,1,J-1);
      Delete (IP,1,J);
    end
    else
      Dummy:=IP;
    val(Dummy,k,J);
    array4int(Result.s_addr)[i]:=k;
    if J<>0 then Exit;
  end;
  Result.s_addr := ntohl(Result.s_addr);
end;

function StrToNetAddr(IP : AnsiString) : in_addr; begin
  StrToNetAddr.s_addr:=htonl(StrToHostAddr(IP).s_addr);
end;

// Winsocket stack needs an init. and cleanup code
var
  wsadata : twsadata;
initialization
  WSAStartUp($2,wsadata);
finalization
  WSACleanUp;
end.
