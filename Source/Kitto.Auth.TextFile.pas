{
  Defines the file-based authenticator and related classes and services.
  This authenticator uses an external file containing user names and password
  hashes to authenticate users.
}
unit Kitto.Auth.TextFile;

{$I Kitto.Defines.inc}

interface

uses
  Classes,
  EF.Data, EF.Tree,
  Kitto.Auth;

type
  {
    The File authenticator uses an external file to authenticate
    users. The file should be a text file with a line for each user, in
    the format:

    <user name>=<password hash>

    Conventionally, insert a # character at the beginning of a line to
    temporarily disable a user. All lines beginning with # are ignored by
    the authenticator.

    The authenticator needs the same items as its ancestor
    TKClassicAuthenticator.

    In order for this authenticator to work, it is required that the
    following file exists:

    %HOME_PATH%\FileAuthenticator.txt

    You can override the file name through the secure configuration item
    Authentication.Authenticator.FileName (may contain macros).

    When Authenticate is called, the authenticator will fetch the file data
    (which is not cached, meaning it will be read anew at every authentication
    request) and check the supplied credentials
    against the user name and relevant password MD5 hash.

    Configuration items (should be specified in SecureConfiguration's slice
    Authentication.Authenticator):
    @table(
      @row(@cell(
        IsClearPassword)@cell(
        Boolean)@cell(
        Set this item to true to signify that the password is stored in
        clear, and not hashed, in the external file. Default False.)
      )@row(@cell(
        FileName)@cell(
        String)@cell(
        Overrides the predefined user list file name. May contain macros.)
      )
    )
  }
  TKFileAuthenticator = class(TKClassicAuthenticator)
  private
    FUserList: TStrings;
  protected
    {
      Re-reads the contents of the user list from the external file and
      loads them into the supplied string list object.
    }
    procedure RefreshUserList(const AUserList: TStrings); virtual;
    {
      Returns the name of the external file (full path, may contain macros).
    }
    function GetUserListFileName: string; virtual;
    procedure InternalAuthenticate(const AAuthData: TEFNode); override;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  EF.Intf, EF.Localization, EF.Types, EF.StrUtils,
  Kitto.Environment;

{ TKFileAuthenticator }

procedure TKFileAuthenticator.AfterConstruction;
begin
  inherited;
  FUserList := TStringList.Create;
end;

destructor TKFileAuthenticator.Destroy;
begin
  FreeAndNil(FUserList);
  inherited;
end;

function TKFileAuthenticator.GetUserListFileName: string;
begin
  Result := Config.GetExpandedString('FileName', '%HOME_PATH%\FileAuthenticator.txt');
end;

procedure TKFileAuthenticator.InternalAuthenticate(
  const AAuthData: TEFNode);
var
  LSuppliedPasswordHash: string;
  LStoredPasswordHash: string;
  LUserName: string;
begin
  LSuppliedPasswordHash := Environment.MacroExpansionEngine.Expand(
    AAuthData.GetString('UserPassword'));

  if not Config.GetBoolean('IsClearPassword') then
    LSuppliedPasswordHash := GetStringHash(LSuppliedPasswordHash);

  LUserName := Environment.MacroExpansionEngine.Expand(
    AAuthData.GetString('UserName'));

  RefreshUserList(FUserList);

  LStoredPasswordHash := FUserList.Values[LUserName];

  if LSuppliedPasswordHash <> LStoredPasswordHash then
    raise EEFError.Create(_('Invalid login.'));
end;

procedure TKFileAuthenticator.RefreshUserList(const AUserList: TStrings);
var
  LFileName: string;
  LLineIndex: Integer;
begin
  LFileName := GetUserListFileName;

  if not FileExists(LFileName) then
    raise EEFError.CreateFmt(_('File %s not found.'), [LFileName]);

  AUserList.LoadFromFile(LFileName);

  // Remove comments.
  for LLineIndex := AUserList.Count - 1 downto 0 do
    if Pos('#', Trim(AUserList[LLineIndex])) = 1 then
      AUserList.Delete(LLineIndex);
end;

initialization
  EWAuthenticatorRegistry.RegisterClass(TKFileAuthenticator);

finalization
  EWAuthenticatorRegistry.UnregisterClass(TKFileAuthenticator);

end.

