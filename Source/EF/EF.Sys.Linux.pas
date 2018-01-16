unit EF.Sys.Linux;

interface

implementation

uses
  SysUtils
  , Classes
  , Posix.Base
  , Posix.Fcntl
  , EF.Sys
  ;

type
  TEFSysLinux = class(TEFSys)
  public
    function GetUserName: string; override;
    // Execute and wait.
    function ExecuteCommand(const ACommand: string): Integer; override;
  end;

{ TEFSysLinux }

// http://man7.org/linux/man-pages/man3/system.3p.html
function system(const command: MarshaledAString): Integer; cdecl; external libc name _PU + 'system';

function TEFSysLinux.ExecuteCommand(const ACommand: string): Integer;
begin
  Result := system(@(TEncoding.ASCII.GetBytes(ACommand) + [0])[0]);
end;
function TEFSysLinux.GetUserName: string;
begin
  Result := GetEnvironmentVariable('USER');
end;

initialization
  EF.Sys.EFSys := TEFSysLinux.Create;

end.
