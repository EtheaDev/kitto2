unit EF.Emailer;

interface

uses
  SysUtils, Classes,
  IdMessage, IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient,
  IdMessageClient, IdSMTP, IdExplicitTLSClientServerBase, IdSMTPBase;

type
  {
    Holds all data needed to use a SMTP Server.
  }
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
    // Only set this property if authentication is used.
    property UserId: string read FUserId write FUserId;
    // Only set this property if authentication is used.
    property Password: string read FPassword write FPassword;
    // Returns True if the object has both UserId and Password set.
    function HasAuthInfo: Boolean;
  end;

  {
    Holds all data needed to describe the sender of an e-mail message.
  }
  TEFEmailSender = class(TPersistent)
  private
    FName: string;
    FReturnAddress: string;
    FEmailAddress: string;
  public
    procedure Assign(Source: TPersistent); override;
    // Sender name.
    property Name: string read FName write FName;
    // Sender e-mail address.
    property EmailAddress: string read FEmailAddress write FEmailAddress;
    // Optional reply-to address.
    property ReturnAddress: string read FReturnAddress write FReturnAddress;
    // Checks that all required properties are set, and raises exceptions if
    // the checks fail.
    procedure CheckValid;
  end;

  {
    Holds all data needed to describe an e-mail message.
  }
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
    // Subject line.
    property Subject: string read FSubject write FSubject;
    // List of recipients, in RFC format.
    property ToAddresses: string read FToAddresses write FToAddresses;
    // List of CC recipients, in RFC format.
    property CCAddresses: string read FCCAddresses write FCCAddresses;
    // List of BCC recipients, in RFC format.
    property BCCAddresses: string read FBCCAddresses write FBCCAddresses;
    // Message body (text or HTML).
    property Body: string read FBody write FBody;
    // Optional list of files to attach to the message before it is sent.
    property AttachmentFileNames: TStrings
      read FAttachmentFileNames write SetAttachmentFileNames;
    // Set this property to True to tell the e-mailer that this is a HTML
    // message.
    property IsHTML: Boolean read FIsHTML write FISHTML;
  end;

  {
    Sends e-mail messages through a SMTP server.
  }
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
    // Sets all required server params before attempting to connect or send.
    property SMTPServerParams: TEFSMTPServerParams
      read FSMTPServerParams write SetSMTPServerParams;
    // Connection management is automatic each time SendMail is called. If you
    // need to call SendMail several times in a row, it is more efficient to
    // manage the connection from the caller. Example:
    // Connected := True;
    // try
    //   for I := ... do
    //     SendMail(...);
    // finally
    //   Connected := False;
    // end;
    property Connected: Boolean read FConnected write SetConnected;
    // Sends AEmailMessage using the sender information provided in AEmailSender
    // and through the server specified in the SMTPServerParams property.
    procedure SendMail(const AEmailSender: TEFEmailSender;
      const AEmailMessage: TEFEmailMessage);
  end;

implementation

uses
  IdAttachmentFile, IdText;
  
resourcestring
  SMissingEmailSender = 'Missing sender name or e-mail address.';

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
      AuthType := atDefault;
      Username := SMTPServerParams.UserID;
      Password := SMTPServerParams.Password;
    end
    else
    begin
      AuthType := atNone;
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
    raise Exception.Create(SMissingEmailSender);
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
