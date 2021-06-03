{-------------------------------------------------------------------------------
   Copyright 2019 Ethea S.r.l.

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
/// <summary>
///  Custom Authenticator for Taskitto, derived from TKDBAuthenticator
///  table on the database to store users and passwords.
///  Enforcement of password strength rules.
///
/// </summary>
unit Auth;

interface

uses
  EF.Tree
  , EF.DB
  , Kitto.Auth.DB
  , Kitto.Config
  , RegularExpressions
  ;

type
  TTasKittoAuthenticator = class(TKDBAuthenticator)
  strict private
    FAutoLogin: Boolean;
    FRedirectErrorUrl: string;
    procedure SendResetPasswordEmail(const AUserName, AEmailAddress, APassword: string);
    function HasRemoteLoginRequest: Boolean;
  protected
    /// <summary>Generates and returns a random password compatible with special rules defined in SetPassword.</summary>
    function GenerateRandomPassword: string; override;
    /// <summary>Try to extract suppliedusername and suppliedpassword from url.</summary>
    procedure GetSuppliedAuthData(const AAuthData: TEFNode; const AHashNeeded: Boolean;
      out ASuppliedUserName, ASuppliedPasswordHash: string;
      out AIsPassepartoutAuthentication: Boolean); override;
    procedure SetPassword(const AValue: string); override;
    /// <summary>Example of sending email for reset password.</summary>
    procedure AfterResetPassword(const ADBConnection: TEFDBConnection; const AParams: TEFNode); override;
    /// <summary>Raise an exception in auto-login fails.</summary>
    function InternalAuthenticate(const AAuthData: TEFNode): Boolean; override;
    /// <summary>Form auto-login redirect to another site.</summary>
  public
    procedure Logout; override;
  end;

implementation

uses
  SysUtils, EF.Localization, EF.Logger, EF.StrUtils,
  Kitto.Auth, Kitto.Web.Request, Kitto.Web.Session, Kitto.Web.Application,
  IdSMTP, IdMessage, IdEmailAddress,
  IdAttachmentFile, IdExplicitTLSClientServerBase, IdSSLOpenSSL, IdText;

{ TTasKittoAuth }


function TTasKittoAuthenticator.GenerateRandomPassword: string;
begin
  Result := GetRandomString(8)+'!';
end;

procedure TTasKittoAuthenticator.GetSuppliedAuthData(const AAuthData: TEFNode;
  const AHashNeeded: Boolean; out ASuppliedUserName,
  ASuppliedPasswordHash: string; out AIsPassepartoutAuthentication: Boolean);
var
  LSourceIP, LLanguage: string;
begin
  if HasRemoteLoginRequest then
  begin
    //Remote login sample: check for LoginUserName and LoginPassword supplied at URL level
    AIsPassepartoutAuthentication := False;
    ASuppliedUserName := TKWebRequest.Current.GetQueryField('LoginUserName');
    ASuppliedPasswordHash := TKWebRequest.Current.GetQueryField('LoginPassword');
    if AHashNeeded then
      ASuppliedPasswordHash := GetStringHash(ASuppliedPasswordHash);
    LSourceIP := TKWebSession.Current.LastRequestInfo.ClientAddress;
    LLanguage := TKWebRequest.Current.GetQueryField('Language');
    if LLanguage <> '' then
      TKWebSession.Current.Language := LLanguage;
    //Supplied Url for error redirect handled by InternalAuthenticate
    //the error response page: www.ethea.it it's only an example
    FRedirectErrorUrl := 'http://www.ethea.it';
    //Notify with logger the remote login event
    TEFLogger.Instance.Log(Format('Remote Login: UserName: %s', [ASuppliedUserName]),
      TEFLogger.Instance.LOG_LOW);
  end
  else
    inherited;
end;

function TTasKittoAuthenticator.HasRemoteLoginRequest: Boolean;
begin
  Result := (TKWebRequest.Current.GetQueryField('LoginUserName') <> '') and
    (TKWebRequest.Current.GetQueryField('LoginPassword') <> '');
end;

function TTasKittoAuthenticator.InternalAuthenticate(
  const AAuthData: TEFNode): Boolean;
begin
  Result := inherited InternalAuthenticate(AAuthData);
  if not Result and HasRemoteLoginRequest then
  begin
    //If autologin fails then raise a special ERedirectError to redirect ULR
    //provided at AAuthData by GetSuppliedAuthData
    FAutoLogin := False; //Reset autologin
    raise ERedirectError.Create(FRedirectErrorUrl);
    raise Exception.Create(FRedirectErrorUrl);
  end
  else
    FAutoLogin := HasRemoteLoginRequest;
end;

procedure TTasKittoAuthenticator.Logout;
begin
  if IsAuthenticated and FAutoLogin then
  begin
    inherited; //ClearAuthData
    FAutoLogin := False; //Reset autologin flag
    //raise ERedirectError.Create('http://www.google.it'); //Redirect to logout site
    raise Exception.Create('http://www.google.it'); //Redirect to logout site
  end
  else
  begin
    FAutoLogin := False; //Reset autologin flag
    inherited; //Reload Home
  end;
end;

procedure TTasKittoAuthenticator.SendResetPasswordEmail(const AUserName, AEmailAddress, APassword: string);
var
  LSMTP: TIdSMTP;
  LIdSSLIOHandler: TIdSSLIOHandlerSocketOpenSSL;
  LMessage: TIdMessage;
  LRecipient: TIdEMailAddressItem;
  LServerNode: TEFNode;
  LMessageNode: TEFNode;
  LBody: string;
  LHTMLBody: string;

  procedure AddTextPart(const AContent, AContentType: string; const AParentPart: Integer = -1);
  begin
    with TIdText.Create(LMessage.MessageParts, nil) do
    begin
      Body.Text := AContent;
      ContentType := AContentType;
      ParentPart := AParentPart;
    end;
  end;

begin
  inherited;
  LIdSSLIOHandler := nil;
  LSMTP := TIdSMTP.Create(nil);
  try
    LSMTP.AuthType := satDefault;
    LServerNode := TKConfig.Instance.Config.FindNode('Email/SMTP/' + Config.GetString('SMTP', 'Default'));
    LMessageNode := Config.FindNode('ResetMailMessage');

    Assert(Assigned(LServerNode));
    Assert(Assigned(LMessageNode));

    LSMTP.Host := LServerNode.GetExpandedString('HostName');
    //For this demo, if the SMTP parameters are not configured we just log it.
    if LSMTP.Host = '' then
    begin
      TEFLogger.Instance.Log(Format(_('Password generated for user %s: %s'), [AUserName, APassword]));
      Exit;
    end;

    LSMTP.Username := LServerNode.GetExpandedString('UserName');
    LSMTP.Password := LServerNode.GetExpandedString('Password');
    LSMTP.Port := LServerNode.GetInteger('Port');
    if (LServerNode.GetBoolean('UseTLS')) then
    begin
      LIdSSLIOHandler := TIdSSLIOHandlerSocketOpenSSL.Create;
      LSMTP.IOHandler := LIdSSLIOHandler;
      LSMTP.UseTLS := utUseRequireTLS;
    end
    else
      LSMTP.UseTLS := utNoTLSSupport;

    LMessage := TIdMessage.Create;
    try
      // From
      LMessage.From.Text := LMessageNode.GetExpandedString('From');

      // To
      LRecipient := LMessage.Recipients.Add;
      LRecipient.Text := AEmailAddress;

      // Subject
      LMessage.Subject := LMessageNode.GetExpandedString('Subject');

      // Body in Text
      LBody := LMessageNode.GetExpandedString('Body');
      // Substitute Template #UserName#
      LBody := StringReplace(LBody, '#UserName#', AUserName, [rfReplaceAll]);
      // Substitute Template #TempPassword#
      LBody := StringReplace(LBody, '#TempPassword#', APassword, [rfReplaceAll]);

      //Body in HTML
      LHTMLBody := LMessageNode.GetExpandedString('HTMLBody');
      if LHTMLBody <> '' then
      begin
        // Substitute Template #UserName#
        LHTMLBody := StringReplace(LHTMLBody, '#UserName#', AUserName, [rfReplaceAll]);
        // Substitute Template #TempPassword#
        LHTMLBody := StringReplace(LHTMLBody, '#TempPassword#', APassword, [rfReplaceAll]);
        // HTML body present.
        if LBody = '' then
        begin
          // HTML only.
            LMessage.ContentType := 'text/html';
            LMessage.Body.Text := LHTMLBody;
        end
        else
        begin
          // HTML + plaintext
          // no attachments
          LMessage.ContentType := 'multipart/alternative';
          AddTextPart(LBody, 'text/plain');
          AddTextPart(LHTMLBody, 'text/html');
        end;
      end
      else
      begin
        LMessage.ContentType := 'text/plain';
        LMessage.Body.Text := LBody;
      end;

      LSMTP.Connect;
      try
        LSMTP.Send(LMessage);
      finally
        LSMTP.Disconnect;
      end;
    finally
      FreeAndNil(LMessage);
    end;
  finally
    LSMTP.Free;
    LIdSSLIOHandler.Free;
  end;
end;

procedure TTasKittoAuthenticator.AfterResetPassword(const ADBConnection: TEFDBConnection; const AParams: TEFNode);
var
  LUserName, LEmailAddress, LPassword: string;
begin
  // We should send the generated password to the user here.
  // AParams contains the nodes EmailAddress and Password.
  // configure Email parameters in config and ResetMailMessage Node
  LUserName := AParams.GetString('UserName');
  LEmailAddress := AParams.GetString('EmailAddress');
  LPassword := AParams.GetString('Password');
  SendResetPasswordEmail(LUserName, LEmailAddress, LPassword);
end;

procedure TTasKittoAuthenticator.SetPassword(const AValue: string);
var
  LValidatePasswordNode: TEFNode;
  LRegEx: string;
  LErrorMsg: string;
  LRegularExpression : TRegEx;
  LMatch: TMatch;
begin
  // Example of enforcement of password strength rules.
  LValidatePasswordNode := Config.FindNode('ValidatePassword');
  Assert(Assigned(LValidatePasswordNode));
  LErrorMsg := LValidatePasswordNode.GetExpandedString('Message','Minimun 8 characters');
  LRegEx := LValidatePasswordNode.GetExpandedString('RegEx','^[ -~]{8,63}$');
  LRegularExpression.Create(LRegEx);
  LMatch := LRegularExpression.Match(AValue);
  if not LMatch.Success then
    raise Exception.Create(LErrorMsg);
  inherited;
end;

initialization
  TKAuthenticatorRegistry.Instance.RegisterClass('TasKitto', TTasKittoAuthenticator);

finalization
  TKAuthenticatorRegistry.Instance.UnregisterClass('TasKitto');

end.
