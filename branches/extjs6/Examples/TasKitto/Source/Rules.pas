unit Rules;

interface

uses
  Kitto.Rules, Kitto.Store;

type
  TDefaultPhaseStartTime = class(TKRuleImpl)
  public
    procedure NewRecord(const ARecord: TKRecord); override;
  end;

implementation

uses
  SysUtils, Variants,
  EF.Localization,
  Kitto.Metadata.DataView;

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

initialization
  TKRuleImplRegistry.Instance.RegisterClass(TDefaultPhaseStartTime.GetClassId, TDefaultPhaseStartTime);

finalization
  TKRuleImplRegistry.Instance.UnregisterClass(TDefaultPhaseStartTime.GetClassId);

end.
