unit KIDE.TableInfoModelUpdateActionFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, KIDE.ModelUpdateActionFrameUnit,
  Vcl.StdCtrls, Vcl.ExtCtrls, KIDE.ModelCreator;

type
  TTableInfoModelUpdateActionFrame = class(TModelUpdateActionFrame)
    ModelNameEdit: TLabeledEdit;
    PhysicalNameEdit: TLabeledEdit;
  strict protected
    procedure SetModelUpdateAction(const AValue: TModelUpdateAction); override;
  public
    procedure SaveToAction; override;
  end;

implementation

{$R *.dfm}

{ TTableInfoModelUpdateActionFrame }

procedure TTableInfoModelUpdateActionFrame.SaveToAction;
begin
  inherited;
  ModelUpdateAction.Metadata.SetString('ModelName', ModelNameEdit.Text);
end;

procedure TTableInfoModelUpdateActionFrame.SetModelUpdateAction(
  const AValue: TModelUpdateAction);
begin
  inherited;
  if Assigned(AValue) then
  begin
    ModelNameEdit.Text := AValue.Metadata.GetString('ModelName');
    ModelNameEdit.ReadOnly := not (AValue is TAddModel);
    PhysicalNameEdit.Text := AValue.Metadata.GetString('PhysicalName');
  end;
end;

end.
