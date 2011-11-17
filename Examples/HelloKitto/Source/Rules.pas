unit Rules;

interface

uses
  Kitto.Rules, KItto.Store;

type
  TCheckDuplicateInvitations = class(TKRuleImpl)
  public
    procedure BeforeAdd(const ARecord: TKRecord); override;
  end;

implementation

uses
  EF.Localization,
  Kitto.Metadata.DataView;

{ TCheckDuplicateInvitations }

procedure TCheckDuplicateInvitations.BeforeAdd(const ARecord: TKRecord);
begin
  if ARecord.Store.Count('INVITEE_ID', ARecord.FieldByName('INVITEE_ID').Value) > 1 then
    RaiseError(_('Cannot invite the same girl twice.'));
end;

initialization
  TKRuleImplRegistry.Instance.RegisterClass(TCheckDuplicateInvitations.GetClassId, TCheckDuplicateInvitations);

finalization
  TKRuleImplRegistry.Instance.UnregisterClass(TCheckDuplicateInvitations.GetClassId);

end.
