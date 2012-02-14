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
///	  Utility classes for sending emails through Indy.
///	</summary>
unit EF.Emailer;

{$I EF.Defines.inc}

interface

uses
  SysUtils, Classes,
  IdMessage, IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient,
  IdMessageClient, IdSMTP, IdExplicitTLSClientServerBase, IdSMTPBase;

type
  ///	<summary>
  ///	  SMTP server configuration data.
  ///	</summary>
  TEFSMTPServerParams = class(TPersistent)
  private
    FUserId: string;
    FPort: Integer;
    FPassword: string;
    FHostName: string;
  public
    procedure Assign(Source: TPersistent); override;
    property HostName: string read FHostName write FHostName;
    property Port: Integer read FPort write FPort;

    ///	<summary>
    ///	  Only set this property if authentication is used.
    ///	</summary>
    property UserId: string read FUserId write FUserId;

    ///	<summary>
    ///	  Only set this property if authentication is used.
    ///	</summary>
    property Password: string read FPassword write FPassword;

    ///	<summary>
    ///	  Returns True if the object has both UserId and Password set.
    ///	</summary>
    function HasAuthInfo: Boolean;
  end;

  ///	<summary>
  ///	  Describes an email sender.
  ///	</summary>
  TEFEmailSender = class(TPersistent)
  private
    FName: string;
    FReturnAddress: string;
    FEmailAddress: string;
  public
    procedure Assign(Source: TPersistent); override;

    ///	<summary>
    ///	  Sender name.
    ///	</summary>
    property Name: string read FName write FName;

    ///	<summary>
    ///	  Sender e-mail address.
    ///	</summary>
    property EmailAddress: string read FEmailAddress write FEmailAddress;

    ///	<summary>
    ///	  Optional reply-to address.
    ///	</summary>
    property ReturnAddress: string read FReturnAddress write FReturnAddress;

    ///	<summary>
    ///	  Checks that all required properties are set, and raises exceptions if
    ///	  the checks fail.
    ///	</summary>
    procedure CheckValid;
  end;

  ///	<summary>
  ///	  Describes an email message.
  ///	</summary>
  TEFEmailMessage = class(TPersistent)
  private
    FToAddresses: string;
    FIsHTML: Boolean;
    FAttachmentFileNames: TStrings;
    FBody: string;
    FCCAddresses: string;
    FSubject: string;
    FBCCAddresses: string;
    procedure SetAttachmentFileNames(const AValue: TStrings);
  public
    constructor Create;
    destructor Destroy; override;

    ///	<summary>
    ///	  Subject line.
    ///	</summary>
    property Subject: string read FSubject write FSubject;

    ///	<summary>
    ///	  List of recipients, in RFC format.
    ///	</summary>
    property ToAddresses: string read FToAddresses write FToAddresses;

    ///	<summary>
    ///	  List of CC recipients, in RFC format.
    ///	</summary>
    property CCAddresses: string read FCCAddresses write FCCAddresses;

    ///	<summary>
    ///	  List of BCC recipients, in RFC format.
    ///	</summary>
    property BCCAddresses: string read FBCCAddresses write FBCCAddresses;

    ///	<summary>
    ///	  Message body (text or HTML).
    ///	</summary>
    property Body: string read FBody write FBody;

    ///	<summary>
    ///	  Optional list of files to attach to the message before it is sent.
    ///	</summary>
    property AttachmentFileNames: TStrings
      read FAttachmentFileNames write SetAttachmentFileNames;

    ///	<summary>
    ///	  Set this property to True to tell the e-mailer that this is a HTML
    ///	  message.
    ///	</summary>
    property IsHTML: Boolean read FIsHTML write FISHTML;
  end;

  ///	<summary>
  ///	  Sends e-mail messages through a SMTP server.
  ///	</summary>
  TEFEmailer = class(TComponent)
    FIdSMTP: TIdSMTP;
    FIdMessage: TIdMessage;
  private
    FConnected: Boolean;
    FSMTPServerParams: TEFSMTPServerParams;
    procedure BuildBodyText(const AText: string; const AIsHTML: Boolean);
    procedure BuildAttachments(const AAttachmentFileNames: TStrings);
    procedure SetConnected(const Value: Boolean);
    procedure SetSMTPServerParams(const Value: TEFSMTPServerParams);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property SMTPServerParams: TEFSMTPServerParams
      read FSMTPServerParams write SetSMTPServerParams;

    ///	<summary>
    ///	  Connection management is automatic each time SendMail is called. If
    ///	  you need to call SendMail several times in a row, it is more
    ///	  efficient to manage the connection from the caller and doing it only
    ///	  once.
    ///	</summary>
    property Connected: Boolean read FConnected write SetConnected;

    ///	<summary>
    ///	  Sends AEmailMessage using the sender information provided in
    ///	  AEmailSender and through the server specified in the SMTPServerParams
    ///	  property.
    ///	</summary>
    procedure SendMail(const AEmailSender: TEFEmailSender;
      const AEmailMessage: TEFEmailMessage);
  end;

implementation

uses
  IdAttachmentFile, IdText,
  EF.Localization;
  
{ TEFEmailer }

procedure TEFEmailer.SendMail(const AEmailSender: TEFEmailSender;
  const AEmailMessage: TEFEmailMessage);
begin
  AEmailSender.CheckValid;
  with FIdMessage do begin
    From.Address := AEmailSender.EmailAddress;
    From.Name := AEmailSender.Name;
    if AEmailSender.ReturnAddress = '' then begin
      ReceiptRecipient.Address := '';
      ReceiptRecipient.Name := '';
    end
    else begin
      ReceiptRecipient.Address := AEmailSender.ReturnAddress;
      ReceiptRecipient.Name := AEmailSender.Name;
    end;
    // composizione del messaggio
    Subject := AEmailMessage.Subject;
    Recipients.EMailAddresses := AEmailMessage.ToAddresses;
    CCList.EMailAddresses := AEmailMessage.CCAddresses;
    BccList.EMailAddresses := AEmailMessage.BCCAddresses;
    MessageParts.Clear;
    BuildBodyText(AEmailMessage.Body, AEmailMessage.IsHTML);
    BuildAttachments(AEmailMessage.AttachmentFileNames);
  end;
  // Informazioni per l'identificazione del server ed eventuale autenticazione
  with FIdSMTP do begin
    Host := FSMTPServerParams.HostName;
    Port := FSMTPServerParams.Port;
    if FSMTPServerParams.HasAuthInfo then
    begin
      AuthType := satDefault;
      Username := SMTPServerParams.UserID;
      Password := SMTPServerParams.Password;
    end
    else
    begin
      AuthType := satNone;
      Username := '';
      Password := '';
    end;
    if not FIdSMTP.Connected then
      Connect;
    try
      try
        Send(FIdMessage);
      except
        Disconnect;
        raise
      end;
    finally
      if not FConnected then
        Disconnect;
    end;
  end;
end;

procedure TEFEmailer.SetConnected(const Value: Boolean);
begin
  FConnected := Value;
  if not Value and FIdSMTP.Connected then
    FIdSMTP.Disconnect;
end;

procedure TEFEmailer.SetSMTPServerParams(const Value: TEFSMTPServerParams);
begin
  FSMTPServerParams.Assign(Value)
end;

constructor TEFEmailer.Create(AOwner: TComponent);
begin
  inherited;
  FIdSMTP := TIdSMTP.Create(Self);
  FIdMessage := TIdMessage.Create(Self);
end;

destructor TEFEmailer.Destroy;
begin
  if FIdSMTP.Connected then
    FIdSMTP.Disconnect;
  FreeAndNil(FIdMessage);
  FreeAndNil(FIdSMTP);
  inherited;
end;

procedure TEFEmailer.BuildAttachments(const AAttachmentFileNames: TStrings);
var
  LAttachmentIndex: Integer;
begin
  for LAttachmentIndex := 0 to AAttachmentFileNames.Count - 1 do
    if AAttachmentFileNames[LAttachmentIndex] <> '' then
      TIdAttachmentFile.Create(FIdMessage.MessageParts, AAttachmentFileNames[LAttachmentIndex]);
end;

procedure TEFEmailer.BuildBodyText(const AText: string; const AIsHTML: Boolean);
begin
  FIdMessage.Body.Clear;
  FIdMessage.Body.Text := AText;
  if AIsHTML then begin
    // HTML messages need a multipart body (text + html).
    with TIdText.Create(FIdMessage.MessageParts, FIdMessage.Body) do
      ContentType := 'text/plain';
    with TIdText.Create(FIdMessage.MessageParts, FIdMessage.Body) do
      ContentType := 'text/html';
    FIdMessage.Body.Clear;
  end;
end;

{ TEFSMTPServerParams }

procedure TEFSMTPServerParams.Assign(Source: TPersistent);
begin
  if Source is TEFSMTPServerParams then
  begin
    FUserId := TEFSMTPServerParams(Source).UserId;
    FPort := TEFSMTPServerParams(Source).Port;
    FPassword := TEFSMTPServerParams(Source).Password;
    FHostName := TEFSMTPServerParams(Source).HostName;
  end
  else
    inherited;
end;

function TEFSMTPServerParams.HasAuthInfo: Boolean;
begin
  Result := (FUserId <> '') and (FPassword <> '');
end;

{ TEFEmailSender }

procedure TEFEmailSender.Assign(Source: TPersistent);
begin
  if Source is TEFEmailSender then
  begin
    FName := TEFEmailSender(Source).Name;
    FReturnAddress := TEFEmailSender(Source).ReturnAddress;
    FEmailAddress := TEFEmailSender(Source).EmailAddress;
  end
  else
    inherited;
end;

procedure TEFEmailSender.CheckValid;
begin
  if (EmailAddress = '') or (Name = '') then
    raise Exception.Create(_('Missing sender name or email address.'));
end;

{ TEFEmailMessage }

constructor TEFEmailMessage.Create;
begin
  inherited;
  FAttachmentFileNames := TStringList.Create;
end;

destructor TEFEmailMessage.Destroy;
begin
  FreeAndNil(FAttachmentFileNames);
  inherited;
end;

procedure TEFEmailMessage.SetAttachmentFileNames(const AValue: TStrings);
begin
  FAttachmentFileNames.Assign(AValue);
end;

end.
