{-------------------------------------------------------------------------------
   Copyright 2012-2021 Ethea S.r.l.

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

unit Kitto.Ext.IndyTools;

interface

uses
  SysUtils,
  IdSMTP, IdMessage, IdEmailAddress,
  EF.Tree,
  Kitto.Config, Kitto.Ext.DataTool, xmldoc;

type
  TSendEmailToolController = class(TKExtDataToolController)
  strict protected
    procedure ExecuteTool; override;
  public
    class function GetDefaultImageName: string; override;
  end;

implementation

uses
  IdAttachmentFile
  , IdExplicitTLSClientServerBase
  , IdSSLOpenSSL
  , IdText
  , EF.Localization
  , Kitto.Web.Application
  , Kitto.JS.Controller
  ;

{ TSendEmailToolController }

procedure TSendEmailToolController.ExecuteTool;
var
  LSMTP: TIdSMTP;
  LMessage: TIdMessage;
  LRecipient: TIdEMailAddressItem;
  LAttachments: TEFNode;
  LRelatedAttachments: TEFNode;
  I: Integer;
  LAttachment: TIdAttachmentFile;
  LSender: TIdEMailAddressItem;
  LAddressNode: TEFNode;
  LChildNode: TEFNode;
  LServerNode: TEFNode;
  LSingleAddress: string;
  LIdSSLIOHandler: TIdSSLIOHandlerSocketOpenSSL;
  LAddress, LText: string;
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

  procedure AddFileAttachmentParts(const AAttachmentsNode: TEFNode; const AParentPart: Integer = -1);
  var
    I: Integer;
    LFileName: string;
  begin
    for I := 0 to AAttachmentsNode.ChildCount - 1 do
    begin
      LFileName := ExpandServerRecordValues(AAttachmentsNode.Children[I].AsExpandedString);
      if not FileExists(LFileName) then
        raise Exception.CreateFmt('File not found %s', [LFileName]);
      LAttachment := TIdAttachmentFile.Create(LMessage.MessageParts, LFileName);
      LAttachment.FileName := ExpandServerRecordValues(AAttachmentsNode.Children[I].Name);
      LAttachment.ParentPart := AParentPart;
      LAttachment.ContentID := AAttachmentsNode.Children[I].GetExpandedString('ContentId');
    end;
  end;

begin
  inherited;
  LIdSSLIOHandler := nil;
  LSMTP := TIdSMTP.Create(nil);
  try
    LSMTP.AuthType := satDefault;
    LServerNode := TKConfig.Instance.Config.FindNode('Email/SMTP/' + Config.GetString('SMTP', 'Default'));
    if Assigned(LServerNode) then
      LServerNode.Merge(Config.FindNode('SMTP'))
    else
      LServerNode := Config.GetNode('SMTP');
    LSMTP.Host := ExpandServerRecordValues(LServerNode.GetExpandedString('HostName'));
    LSMTP.Username := ExpandServerRecordValues(LServerNode.GetExpandedString('UserName'));
    LSMTP.Password := ExpandServerRecordValues(LServerNode.GetExpandedString('Password'));
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
      // Try single sender first.
      LAddressNode := Config.GetNode('Message/From');
      LMessage.From.Text := ExpandServerRecordValues(LAddressNode.AsExpandedString);
      if LMessage.From.Text = '' then
      begin
        // Multiple senders.
        for I := 0 to LAddressNode.ChildCount - 1 do
        begin
          LChildNode := LAddressNode[I];
          if SameText(LChildNode.Name, 'Sender') then
          begin
            LText := ExpandServerRecordValues(LChildNode.AsExpandedString);
            if LText <> '' then
            begin
              LSender := LMessage.FromList.Add;
              LSender.Text := LText;
            end
            else
            begin
              LAddress := ExpandServerRecordValues(LChildNode.GetExpandedString('Address'));
              if LAddress <> '' then
              begin
                LSender := LMessage.FromList.Add;
                LSender.Name := ExpandServerRecordValues(LChildNode.GetExpandedString('Name'));
                LSender.Address := LAddress;
              end;
            end;
          end;
        end;
      end;

      // Try single recipient first.
      LAddressNode := Config.GetNode('Message/To');
      LSingleAddress := ExpandServerRecordValues(LAddressNode.AsExpandedString);
      if LSingleAddress <> '' then
      begin
        LRecipient := LMessage.Recipients.Add;
        LRecipient.Text := LSingleAddress;
      end
      else
      begin
        // Multiple recipients.
        for I := 0 to LAddressNode.ChildCount - 1 do
        begin
          LChildNode := LAddressNode[I];
          if SameText(LChildNode.Name, 'Recipient') then
          begin
            LText := ExpandServerRecordValues(LChildNode.AsExpandedString);
            if LText <> '' then
            begin
              LRecipient := LMessage.Recipients.Add;
              LRecipient.Text := LText;
            end
            else
            begin
              LAddress := ExpandServerRecordValues(LChildNode.GetExpandedString('Address'));
              if LAddress <> '' then
              begin
                LRecipient := LMessage.Recipients.Add;
                LRecipient.Name := ExpandServerRecordValues(LChildNode.GetExpandedString('Name'));
                LRecipient.Address := LAddress;
              end;
            end;
          end;
        end;
      end;

      // Optional CCs.
      LAddressNode := Config.FindNode('Message/CC');
      if Assigned(LAddressNode) then
      begin
        LSingleAddress := ExpandServerRecordValues(LAddressNode.AsExpandedString);
        if LSingleAddress <> '' then
        begin
          LRecipient := LMessage.CCList.Add;
          LRecipient.Text := LSingleAddress;
        end
        else
        begin
          // Multiple CC recipients.
          for I := 0 to LAddressNode.ChildCount - 1 do
          begin
            LChildNode := LAddressNode[I];
            if SameText(LChildNode.Name, 'Recipient') then
            begin
              LText := ExpandServerRecordValues(LChildNode.AsExpandedString);
              if LText <> '' then
              begin
                LRecipient := LMessage.CCList.Add;
                LRecipient.Text := LText;
              end
              else
              begin
                LAddress := ExpandServerRecordValues(LChildNode.GetExpandedString('Address'));
                if LAddress <> '' then
                begin
                  LRecipient := LMessage.CCList.Add;
                  LRecipient.Name := ExpandServerRecordValues(LChildNode.GetExpandedString('Name'));
                  LRecipient.Address := LAddress;
                end;
              end;
            end;
          end;
        end;
      end;

      // Optional BCCs.
      LAddressNode := Config.FindNode('Message/BCC');
      if Assigned(LAddressNode) then
      begin
        LSingleAddress := ExpandServerRecordValues(LAddressNode.AsExpandedString);
        if LSingleAddress <> '' then
        begin
          LRecipient := LMessage.BccList.Add;
          LRecipient.Text := LSingleAddress;
        end
        else
        begin
          // Multiple BCC recipients.
          for I := 0 to LAddressNode.ChildCount - 1 do
          begin
            LChildNode := LAddressNode[I];
            if SameText(LChildNode.Name, 'Recipient') then
            begin
              LText := ExpandServerRecordValues(LChildNode.AsExpandedString);
              if LText <> '' then
              begin
                LRecipient := LMessage.BccList.Add;
                LRecipient.Text := LText;
              end
              else
              begin
                LAddress := ExpandServerRecordValues(LChildNode.GetExpandedString('Address'));
                if LAddress <> '' then
                begin
                  LRecipient := LMessage.BccList.Add;
                  LRecipient.Name := ExpandServerRecordValues(LChildNode.GetExpandedString('Name'));
                  LRecipient.Address := LAddress;
                end;
              end;
            end;
          end;
        end;
      end;

      // Subject
      LMessage.Subject := ExpandServerRecordValues(Config.GetExpandedString('Message/Subject'));

      // Body and attachments.
      LBody := ExpandServerRecordValues(Config.GetExpandedString('Message/Body'));
      LHTMLBody := ExpandServerRecordValues(Config.GetExpandedString('Message/HTMLBody'));
      LAttachments := Config.FindNode('Message/Attachments');
      LRelatedAttachments := Config.FindNode('Message/RelatedAttachments');
      if LHTMLBody <> '' then
      begin
        // HTML body present.
        if LBody = '' then
        begin
          // HTML only.
          // no attachments
          if not Assigned(LAttachments) and not Assigned(LRelatedAttachments) then
          begin
            LMessage.ContentType := 'text/html';
            LMessage.Body.Text := LHTMLBody;
          end
          // related attachments only
          else if not Assigned(LAttachments) and Assigned(LRelatedAttachments) then
          begin
            LMessage.ContentType := 'multipart/related; type="text/html"';
            AddTextPart(LHTMLBody, 'text/html');
            AddFileAttachmentParts(LRelatedAttachments);
          end
          // unrelated attachments only
          else if Assigned(LAttachments) and not Assigned(LRelatedAttachments) then
          begin
            LMessage.ContentType := 'multipart/mixed';
            AddTextPart(LHTMLBody, 'text/html');
            AddFileAttachmentParts(LAttachments);
          end
          // both related and unrelated attachments.
          else
          begin
            LMessage.ContentType := 'multipart/mixed';
            AddTextPart('', 'multipart/related');
            AddTextPart(LHTMLBody, 'text/html', 0);
            AddFileAttachmentParts(LRelatedAttachments, 0);
            AddFileAttachmentParts(LAttachments);
          end;
        end
        else
        begin
          // HTML + plaintext
          // no attachments
          if not Assigned(LAttachments) and not Assigned(LRelatedAttachments) then
          begin
            LMessage.ContentType := 'multipart/alternative';
            AddTextPart(LBody, 'text/plain');
            AddTextPart(LHTMLBody, 'text/html');
          end
          // related attachments only
          else if not Assigned(LAttachments) and Assigned(LRelatedAttachments) then
          begin
            LMessage.ContentType := 'multipart/related; type="multipart/alternative"';
            AddTextPart('', 'multipart/alternative');
            AddTextPart(LBody, 'text/plain', 0);
            AddTextPart(LHTMLBody, 'text/html', 0);
            AddFileAttachmentParts(LRelatedAttachments);
          end
          // unrelated attachments only
          else if Assigned(LAttachments) and not Assigned(LRelatedAttachments) then
          begin
            LMessage.ContentType := 'multipart/mixed';
            AddTextPart('', 'multipart/alternative');
            AddTextPart(LBody, 'text/plain', 0);
            AddTextPart(LHTMLBody, 'text/html', 0);
            AddFileAttachmentParts(LAttachments);
          end
          // both related and unrelated attachments.
          else
          begin
            LMessage.ContentType := 'multipart/mixed';
            AddTextPart('', 'multipart/related; type="multipart/alternative"');
            AddTextPart('', 'multipart/alternative', 0);
            AddTextPart(LBody, 'text/plain', 1);
            AddTextPart(LHTMLBody, 'text/html', 1);
            AddFileAttachmentParts(LRelatedAttachments, 0);
            AddFileAttachmentParts(LAttachments);
          end;
        end;
      end
      else
      begin
        if not Assigned(LAttachments) then
        begin
          LMessage.ContentType := 'text/plain';
          LMessage.Body.Text := LBody;
        end
        else
        begin
          LMessage.ContentType := 'multipart/mixed';
          AddTextPart(LBody, 'text/plain');
          AddFileAttachmentParts(LAttachments);
        end;
      end;

      LSMTP.Connect;
      try
        LSMTP.Send(LMessage);
      finally
        LSMTP.Disconnect;
      end;
      TKWebApplication.Current.Toast(_('E-mail message sent successfully.'));
    finally
      FreeAndNil(LMessage);
    end;
  finally
    FreeAndNil(LSMTP);
    LIdSSLIOHandler.Free;
  end;
end;

class function TSendEmailToolController.GetDefaultImageName: string;
begin
  Result := 'email_go';
end;

initialization
  TJSControllerRegistry.Instance.RegisterClass('SendEmail', TSendEmailToolController);

finalization
  TJSControllerRegistry.Instance.UnregisterClass('SendEmail');

end.
