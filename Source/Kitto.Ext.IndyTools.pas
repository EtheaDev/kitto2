{-------------------------------------------------------------------------------
   Copyright 2015 Ethea S.r.l.

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
  Kitto.Config, Kitto.Ext.DataTool;

type
  TSendEmailToolController = class(TKExtDataToolController)
  strict protected
    procedure ExecuteTool; override;
  end;

implementation

uses
  IdAttachmentFile,
  EF.Localization,
  Kitto.Ext.Controller, KItto.Ext.Session;

{ TSendEmailToolController }

procedure TSendEmailToolController.ExecuteTool;
var
  LSMTP: TIdSMTP;
  LMessage: TIdMessage;
  LRecipient: TIdEMailAddressItem;
  LAttachments: TEFNode;
  I: Integer;
  LAttachment: TIdAttachmentFile;
  LSender: TIdEMailAddressItem;
  LAddressNode: TEFNode;
  LFileName: string;
  LServerNode: TEFNode;
  LSingleAddress: string;
begin
  inherited;
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

    LMessage := TIdMessage.Create;
    try
      // Try single sender first.
      LAddressNode := Config.GetNode('Message/From');
      LMessage.From.Text := LAddressNode.AsExpandedString;
      if LMessage.From.Text = '' then
      begin
        // Multiple senders.
        for I := 0 to LAddressNode.ChildCount - 1 do
        begin
          LSender := LMessage.FromList.Add;
          LSender.Text := ExpandServerRecordValues(LAddressNode[I].AsExpandedString);
          if LSender.Text = '' then
          begin
            LSender.Name := ExpandServerRecordValues(LAddressNode[I].GetExpandedString('Name'));
            LSender.Address := ExpandServerRecordValues(LAddressNode[I].GetExpandedString('Address'));
          end;
        end;
      end;

      // Try single recipient first.
      LAddressNode := Config.GetNode('Message/To');
      LSingleAddress := LAddressNode.AsExpandedString;
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
          LRecipient := LMessage.Recipients.Add;
          LRecipient.Text := ExpandServerRecordValues(LAddressNode[I].AsExpandedString);
          if LRecipient.Text = '' then
          begin
            LRecipient.Name := ExpandServerRecordValues(LAddressNode[I].GetExpandedString('Name'));
            LRecipient.Address := ExpandServerRecordValues(LAddressNode[I].GetExpandedString('Address'));
          end;
        end;
      end;

      // Optional CCs.
      LAddressNode := Config.FindNode('Message/CC');
      if Assigned(LAddressNode) then
      begin
        LSingleAddress := LAddressNode.AsExpandedString;
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
            LRecipient := LMessage.CCList.Add;
            LRecipient.Text := ExpandServerRecordValues(LAddressNode[I].AsExpandedString);
            if LRecipient.Text = '' then
            begin
              LRecipient.Name := ExpandServerRecordValues(LAddressNode[I].GetExpandedString('Name'));
              LRecipient.Address := ExpandServerRecordValues(LAddressNode[I].GetExpandedString('Address'));
            end;
          end;
        end;
      end;

      // Optional BCCs.
      LAddressNode := Config.FindNode('Message/BCC');
      if Assigned(LAddressNode) then
      begin
        LSingleAddress := LAddressNode.AsExpandedString;
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
            LRecipient := LMessage.CCList.Add;
            LRecipient.Text := ExpandServerRecordValues(LAddressNode[I].AsExpandedString);
            if LRecipient.Text = '' then
            begin
              LRecipient.Name := ExpandServerRecordValues(LAddressNode[I].GetExpandedString('Name'));
              LRecipient.Address := ExpandServerRecordValues(LAddressNode[I].GetExpandedString('Address'));
            end;
          end;
        end;
      end;

      LMessage.Subject := ExpandServerRecordValues(Config.GetExpandedString('Message/Subject'));

      LMessage.Body.Text := ExpandServerRecordValues(Config.GetExpandedString('Message/Body'));

      LAttachments := Config.FindNode('Message/Attachments');
      if Assigned(LAttachments) then
      begin
        for I := 0 to LAttachments.ChildCount - 1 do
        begin
          LFileName := ExpandServerRecordValues(LAttachments.Children[I].AsExpandedString);
          if not FileExists(LFileName) then
            raise Exception.CreateFmt('File not found %s', [LFileName]);
          LAttachment := TIdAttachmentFile.Create(LMessage.MessageParts, LFileName);
          LAttachment.FileName := ExpandServerRecordValues(LAttachments.Children[I].Name);
        end;
      end;

      LSMTP.Connect;
      try
        LSMTP.Send(LMessage);
      finally
        LSMTP.Disconnect;
      end;
      Session.Flash(_('E-mail message sent successfully.'));
    finally
      FreeAndNil(LMessage);
    end;
  finally
    FreeAndNil(LSMTP);
  end;
end;

initialization
  TKExtControllerRegistry.Instance.RegisterClass('SendEmail', TSendEmailToolController);

finalization
  TKExtControllerRegistry.Instance.UnregisterClass('SendEmail');

end.