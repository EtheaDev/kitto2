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
///	  <para>Defines the DBServer authenticator and related classes and
///	  services.</para>
///	  <para>This authenticator uses the database server to authenticate
///	  users.</para>
///	</summary>
unit Kitto.Auth.DBServer;

{$I Kitto.Defines.inc}

interface

uses
  EF.Tree,
  Kitto.Auth;

type
  ///	<summary>
  ///	  <para>The DBServer authenticator uses the database server to
  ///	  authenticate users.</para>
  ///	  <para>It needs the same items as its ancestor <see cref=
  ///	  "TKClassicAuthenticator" />.</para>
  ///	  <para>In order for this authenticator to work, it is required that the
  ///	  user-name and password placeholders in the database connection strings
  ///	  stored in Config.yaml are written as <c>%Auth:UserName%</c> and
  ///	  <c>%Auth:Password%</c>. When Authenticate is called, the authenticator
  ///	  will allow macro substitution of these items and try to connect to the
  ///	  database.</para>
  ///	</summary>
  TKDBServerAuthenticator = class(TKClassicAuthenticator)
  protected
    function InternalAuthenticate(const AAuthData: TEFNode): Boolean; override;
  end;
  
implementation

uses
  Kitto.Config;

{ TKDBServerAuthenticator }

function TKDBServerAuthenticator.InternalAuthenticate(const AAuthData: TEFNode): Boolean;
begin
  try
    TKConfig.Instance.DefaultDBConnection.Open;
    Result := True;
  except
    Result := False;
  end;
end;

initialization
  TKAuthenticatorRegistry.Instance.RegisterClass('DBServer', TKDBServerAuthenticator);

finalization
  TKAuthenticatorRegistry.Instance.UnregisterClass('DBServer');

end.

