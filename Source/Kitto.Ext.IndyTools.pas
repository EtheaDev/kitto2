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
  Kitto.Ext.DataTool;

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
  LFromNode: TEFNode;
  LFileName: string;
  LToNode: TEFNode;
begin
  inherited;
  LSMTP := TIdSMTP.Create(nil);
  try
    LSMTP.AuthType := satDefault;
    LSMTP.Host := ExpandServerRecordValues(Config.GetExpandedString('Server/HostName'));
    LSMTP.Username := ExpandServerRecordValues(Config.GetExpandedString('Server/UserName'));
    LSMTP.Password := ExpandServerRecordValues(Config.GetExpandedString('Server/Password'));

    LMessage := TIdMessage.Create;
    try
      // Senders.
      LFromNode := Config.GetNode('Message/From');
      for I := 0 to LFromNode.ChildCount - 1 do
      begin
        LSender := LMessage.FromList.Add;
        LSender.Name := ExpandServerRecordValues(LFromNode[I].GetExpandedString('Name'));
        LSender.Address := ExpandServerRecordValues(LFromNode[I].GetExpandedString('Address'));
      end;

      // Recipients.
      LToNode := Config.GetNode('Message/To');
      for I := 0 to LFromNode.ChildCount - 1 do
      begin
        LRecipient := LMessage.Recipients.Add;
        LRecipient.Name := ExpandServerRecordValues(LToNode[I].GetExpandedString('Name'));
        LRecipient.Address := ExpandServerRecordValues(LToNode[I].GetExpandedString('Address'));
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
