unit Rules;

interface

uses
  Kitto.Rules
  , Kitto.Store
  , Kitto.Auth
  , Kitto.Config
  , Ext.Base
  , EF.Tree
  , EF.DB
  ;

type
  TDefaultPhaseStartTime = class(TKRuleImpl)
  public
    procedure NewRecord(const ARecord: TKRecord); override;
  end;

  TKKittoUserCheck = class(TKRuleImpl)
  public
    procedure AfterAddOrUpdate(const ARecord: TKRecord); override;
    procedure BeforeAddOrUpdate(const ARecord: TKRecord); override;
  end;

  function GenerateRandomPassword: string;

implementation

uses
  SysUtils, Variants,
  EF.Localization, EF.Logger, EF.StrUtils,
  Kitto.Metadata.DataView,
  IdSMTP,
  IdMessage,
  IdEmailAddress,
  IdSSLOpenSSL,
  IdText,
  IdExplicitTLSClientServerBase;

{ TDefaultPhaseStartTime }

procedure TDefaultPhaseStartTime.NewRecord(const ARecord: TKRecord);
var
  LLastDate: Variant;
begin
  inherited;
  LLastDate := ARecord.Store.Max('END_DATE');
  if VarIsNull(LLastDate) then
    ARecord.FieldByName('START_DATE').AsDate := Date
  else
    ARecord.FieldByName('START_DATE').AsDate := LLastDate + 1;
end;

{ TKittoUserCheck }

procedure TKKittoUserCheck.AfterAddOrUpdate(const ARecord: TKRecord);
begin
  inherited;
  if not TKAuthenticator.Current.Current.IsAuthenticated then
  begin
    ExtMessageBox.ShowMessage(emtInfo, _('New User'), _('An e-mail with login instructions has been sent'));
  end;
end;

procedure TKKittoUserCheck.BeforeAddOrUpdate(const ARecord: TKRecord);
var
  LUserName, LEmailAddress, LPassword: string;
  LCommandText: string;
  LQuery: TEFDBQuery;
  LUserLocked: boolean;
  LCommand: TEFDbCommand;

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
  //Check if it's a new user
  if not TKAuthenticator.Current.Current.IsAuthenticated then
  begin
    LPassword := ARecord.FieldByName('PASSWORD_HASH').AsString;
    if LPassword = '' then
    begin
      //If password field remains empty, I generate a random one
      LPassword := GenerateRandomPassword;
      ARecord.FieldByName('PASSWORD_HASH').AsString := LPassword;
    end;
    //Registering a new user:
    LUserName := ARecord.FieldByName('USER_NAME').AsString;
    LEmailAddress := ARecord.FieldByName('EMAIL_ADDRESS').AsString;
    //Checking if user name already exist
    LUserLocked := False;
    Try
      LCommandText := 'SELECT MUST_CHANGE_PASSWORD, EMAIL_ADDRESS FROM KITTO_USERS WHERE USER_NAME = :ID';
      LQuery := TKConfig.Database.CreateDBQuery;
      LQuery.CommandText := LCommandText;
      try
        LQuery.Params.ParamByName('ID').AsString := ARecord.FieldByName('USER_NAME').AsString;
        LQuery.Open;
        if not LQuery.DataSet.IsEmpty then
        begin
          if (LQuery.DataSet.FieldByName('MUST_CHANGE_PASSWORD').AsInteger = 1) then
            LUserLocked := True
          else
            RaiseError('The User Name you have chosen is not available');
        end;
      finally
        LQuery.Close;
      end;
    finally
      FreeAndNil(LQuery);
    end;

    //If user is locked (has to change password but can't login) this lets him to register with the same user name, deleting the locked account
    if LUserLocked then begin
      LCommand := TKConfig.Database.CreateDBCommand;
      try
        LCommand.Connection.StartTransaction;
        try
          LCommand.CommandText := 'DELETE FROM KITTO_USERS WHERE USER_NAME = '''+ARecord.FieldByName('USER_NAME').AsString+'''';
          LCommand.Execute;
          LCommand.Connection.CommitTransaction;
        except
          LCommand.Connection.RollbackTransaction;
        end;
      finally
        FreeAndNil(LCommand);
      end;
    end;

    //Set password to change
    ARecord.FieldByName('MUST_CHANGE_PASSWORD').AsBoolean := True;
    //Set user actived
    ARecord.FieldByName('IS_ACTIVE').AsBoolean := True;
    //Insert the password on the record
    ARecord.FieldByName('PASSWORD_HASH').AsString := LPassword;

    //Send email with password
    LIdSSLIOHandler := nil;
    LSMTP := TIdSMTP.Create(nil);
    try
      LSMTP.AuthType := satDefault;
      LServerNode := TKConfig.Instance.Config.FindNode('Email/SMTP/' + TKConfig.Instance.Config.GetString('SMTP', 'Default'));
      LMessageNode := TKConfig.Instance.Config.FindNode('Auth/NewUserMailMessage');

      Assert(Assigned(LServerNode));
      Assert(Assigned(LMessageNode));

      LSMTP.Host := LServerNode.GetExpandedString('HostName');
      //For this demo, if the SMTP parameters are not configured we just log it.
      if LSMTP.Host = '' then
      begin
        TEFLogger.Instance.Log(Format(_('Password generated for user %s: %s'), [LUserName, LPassword]));
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
        LRecipient.Text := LEmailAddress;

        // Subject
        LMessage.Subject := LMessageNode.GetExpandedString('Subject');

        // Body in Text
        LBody := LMessageNode.GetExpandedString('Body');
        // Substitute Template #UserName#
        LBody := StringReplace(LBody, '#UserName#', LUserName, [rfReplaceAll]);
        // Substitute Template #TempPassword#
        LBody := StringReplace(LBody, '#TempPassword#', LPassword, [rfReplaceAll]);

        //Body in HTML
        LHTMLBody := LMessageNode.GetExpandedString('HTMLBody');
        if LHTMLBody <> '' then
        begin
          // Substitute Template #UserName#
          LHTMLBody := StringReplace(LHTMLBody, '#UserName#', LUserName, [rfReplaceAll]);
          // Substitute Template #TempPassword#
          LHTMLBody := StringReplace(LHTMLBody, '#TempPassword#', LPassword, [rfReplaceAll]);
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
end;

function GenerateRandomPassword: string;
begin
  Result := GetRandomString(8)+'!';
end;

initialization
  TKRuleImplRegistry.Instance.RegisterClass(TDefaultPhaseStartTime.GetClassId, TDefaultPhaseStartTime);
  TKRuleImplRegistry.Instance.RegisterClass(TKKittoUserCheck.GetClassId, TKKittoUserCheck);
finalization
  TKRuleImplRegistry.Instance.UnregisterClass(TDefaultPhaseStartTime.GetClassId);
  TKRuleImplRegistry.Instance.UnregisterClass(TKKittoUserCheck.GetClassId);

end.
