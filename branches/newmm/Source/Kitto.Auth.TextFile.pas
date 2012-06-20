{-------------------------------------------------------------------------------
   Copyright 2012 Ethea S.r.l.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
-------------------------------------------------------------------------------}

///	<summary>
///	  <para>Defines the file-based authenticator and related classes and
///	  services.</para>
///	  <para>This authenticator uses an external file containing user names and
///	  password hashes to authenticate users.</para>
///	</summary>
unit Kitto.Auth.TextFile;

{$I Kitto.Defines.inc}

interface

uses
  Classes,
  EF.Tree,
  Kitto.Auth;

type
  ///	<summary>
  ///	  <para>The TextFile authenticator uses an external text file to
  ///	  authenticate users. The file should have a line for each user, in the
  ///	  format:</para>
  ///	  <para><c>&lt;user name&gt;=&lt;password hash&gt;</c></para>
  ///	  <para>By convention, a # character at the beginning of a line disables
  ///	  a user. All lines beginning with # are ignored by the
  ///	  authenticator.</para>
  ///	  <para>The authenticator needs the same auth items as its ancestor
  ///	  TKClassicAuthenticator.</para>
  ///	  <para>In order for this authenticator to work, it is required that the
  ///	  following file exists:</para>
  ///	  <para><c>%HOME_PATH%\Auth.txt</c></para>
  ///	  <para>You can override the file name by means of the FileName parameter
  ///	  (may contain macros).</para>
  ///	  <para>When Authenticate is called, the authenticator fetches the file
  ///	  data (which is not cached, meaning it is read anew at every
  ///	  authentication request) and check the supplied credentials against the
  ///	  user name and relevant password MD5 hash.</para>
  ///	  <para>Parameters:</para>
  ///	  <list type="table">
  ///	    <listheader>
  ///	      <term>Term</term>
  ///	      <description>Description</description>
  ///	    </listheader>
  ///	    <item>
  ///	      <term>IsClearPassword</term>
  ///	      <description>Set this item to true to signify that the password is
  ///	      stored in clear, and not hashed, in the external file. Default
  ///	      False.</description>
  ///	    </item>
  ///	    <item>
  ///	      <term>FileName</term>
  ///	      <description>Overrides the predefined user list file name. May
  ///	      contain macros.</description>
  ///	    </item>
  ///	  </list>
  ///	</summary>
  TKTextFileAuthenticator = class(TKClassicAuthenticator)
  private
    FUserList: TStrings;
  protected
    function InternalAuthenticate(const AAuthData: TEFNode): Boolean; override;
  protected
    ///	<summary>Re-reads the contents of the user list from the external file
    ///	and loads them into the supplied string list object.</summary>
    procedure RefreshUserList(const AUserList: TStrings); virtual;

    ///	<summary>Returns the name of the external file (full path, may contain
    ///	macros).</summary>
    function GetUserListFileName: string; virtual;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  EF.Intf, EF.Localization, EF.Types, EF.StrUtils,
  Kitto.Config;

{ TKTextFileAuthenticator }

procedure TKTextFileAuthenticator.AfterConstruction;
begin
  inherited;
  FUserList := TStringList.Create;
end;

destructor TKTextFileAuthenticator.Destroy;
begin
  FreeAndNil(FUserList);
  inherited;
end;

function TKTextFileAuthenticator.GetUserListFileName: string;
begin
  Result := Config.GetExpandedString('FileName', '%HOME_PATH%\FileAuthenticator.txt');
end;

function  TKTextFileAuthenticator.InternalAuthenticate(
  const AAuthData: TEFNode): Boolean;
var
  LSuppliedPasswordHash: string;
  LStoredPasswordHash: string;
  LUserName: string;
begin
  LSuppliedPasswordHash := TKConfig.Instance.MacroExpansionEngine.Expand(
    AAuthData.GetString('Password'));

  if not Config.GetBoolean('IsClearPassword') then
    LSuppliedPasswordHash := GetStringHash(LSuppliedPasswordHash);

  LUserName := TKConfig.Instance.MacroExpansionEngine.Expand(
    AAuthData.GetString('UserName'));

  RefreshUserList(FUserList);

  LStoredPasswordHash := FUserList.Values[LUserName];

  Result := LSuppliedPasswordHash = LStoredPasswordHash;
end;

procedure TKTextFileAuthenticator.RefreshUserList(const AUserList: TStrings);
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
  TKAuthenticatorRegistry.Instance.RegisterClass('TextFile', TKTextFileAuthenticator);

finalization
  TKAuthenticatorRegistry.Instance.UnregisterClass('TextFile');

end.

