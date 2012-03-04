unit KIDE.DetailReferenceUpdateActionFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, KIDE.ModelUpdateActionFrameUnit,
  Vcl.StdCtrls, Vcl.ExtCtrls, KIDE.ModelCreator;

type
  TDetailReferenceUpdateActionFrame = class(TModelUpdateActionFrame)
    DetailReferenceNameEdit: TLabeledEdit;
    ForeignKeyNameEdit: TLabeledEdit;
    DetailModelNameEdit: TLabeledEdit;
  strict protected
    procedure SetModelUpdateAction(const AValue: TModelUpdateAction); override;
  public
    procedure SaveToAction; override;
  end;

implementation

{$R *.dfm}

{ TDetailReferenceUpdateActionFrame }

procedure TDetailReferenceUpdateActionFrame.SaveToAction;
begin
  inherited;
  ModelUpdateAction.Metadata.SetString('DetailReferenceName', DetailReferenceNameEdit.Text);
end;

procedure TDetailReferenceUpdateActionFrame.SetModelUpdateAction(
  const AValue: TModelUpdateAction);
begin
  inherited;
  if Assigned(AValue) then
  begin
    DetailReferenceNameEdit.Text := AValue.Metadata.GetString('DetailReferenceName');
    DetailModelNameEdit.Text := AValue.Metadata.GetString('DetailModelName');
    ForeignKeyNameEdit.Text := AValue.Metadata.GetString('ForeignKeyName');
  end;
end;

end.
