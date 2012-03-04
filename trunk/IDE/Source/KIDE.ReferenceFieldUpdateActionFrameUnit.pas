unit KIDE.ReferenceFieldUpdateActionFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, KIDE.ModelUpdateActionFrameUnit,
  Vcl.StdCtrls, Vcl.ExtCtrls, KIDE.ModelCreator;

type
  TReferenceFieldUpdateActionFrame = class(TModelUpdateActionFrame)
    ReferenceNameEdit: TLabeledEdit;
    ForeignKeyNameEdit: TLabeledEdit;
    IsRequiredCheckBox: TCheckBox;
  private
  strict protected
    procedure SetModelUpdateAction(const AValue: TModelUpdateAction); override;
  public
    procedure SaveToAction; override;
  end;

implementation

{$R *.dfm}

{ TReferenceFieldUpdateActionFrame }

procedure TReferenceFieldUpdateActionFrame.SaveToAction;
begin
  inherited;
  ModelUpdateAction.Metadata.SetString('ReferenceName', ReferenceNameEdit.Text);
  ModelUpdateAction.Metadata.SetString('ForeignKeyName', ForeignKeyNameEdit.Text);
  ModelUpdateAction.Metadata.SetBoolean('IsRequired', IsRequiredCheckBox.Checked);
end;

procedure TReferenceFieldUpdateActionFrame.SetModelUpdateAction(
  const AValue: TModelUpdateAction);
begin
  inherited;
  if Assigned(AValue) then
  begin
    ReferenceNameEdit.Text := AValue.Metadata.GetString('ReferenceName');
    ReferenceNameEdit.ReadOnly := not (AValue is TAddReferenceField);
    ForeignKeyNameEdit.Text := AValue.Metadata.GetString('ForeignKeyName');
    IsRequiredCheckBox.Checked := AValue.Metadata.GetBoolean('IsRequired');
  end;
end;

end.
