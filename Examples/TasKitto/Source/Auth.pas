unit Auth;

interface

uses
  EF.Tree
  , Kitto.Auth.DB
  , Kitto.Config
  , RegularExpressions
  ;

type
  TTasKittoAuthenticator = class(TKDBAuthenticator)
  strict private
    procedure SendResetPasswordEmail(const AEmailAddress, APassword: string);
  protected
    procedure SetPassword(const AValue: string); override;
    procedure BeforeResetPassword(const AParams: TEFNode); override;
  end;

implementation

uses
  SysUtils, EF.Localization, EF.Logger, Kitto.Auth,
  IdSMTP, IdMessage, IdEmailAddress,
  IdAttachmentFile, IdExplicitTLSClientServerBase, IdSSLOpenSSL, IdText;

{ TTasKittoAuth }


procedure TTasKittoAuthenticator.SendResetPasswordEmail(const AEmailAddress, APassword: string);
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
      TEFLogger.Instance.Log(Format(_('Password generated: %s'), [APassword]));
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
      // Substitute Template #TempPassword#
      LBody := StringReplace(LBody, '#TempPassword#', APassword, [rfReplaceAll]);

      //Body in HTML
      LHTMLBody := LMessageNode.GetExpandedString('HTMLBody');
      if LHTMLBody <> '' then
      begin
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

procedure TTasKittoAuthenticator.BeforeResetPassword(const AParams: TEFNode);
begin
  // We should send the generated password to the user here.
  // AParams contains the nodes EmailAddress and Password.
  // configure Email parameters in config and ResetMailMessage Node
  SendResetPasswordEmail(AParams.GetString('EmailAddress'), AParams.GetString('Password'));
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
