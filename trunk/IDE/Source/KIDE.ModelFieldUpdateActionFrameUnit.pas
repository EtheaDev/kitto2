unit KIDE.ModelFieldUpdateActionFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, KIDE.ModelUpdateActionFrameUnit,
  Vcl.StdCtrls, Vcl.ExtCtrls, KIDE.ModelCreator;

type
  TModelFieldUpdateActionFrame = class(TModelUpdateActionFrame)
    FieldNameEdit: TLabeledEdit;
    PhysicalNameEdit: TLabeledEdit;
    DataTypeComboBox: TComboBox;
    Label2: TLabel;
    SizeEdit: TLabeledEdit;
    IsRequiredCheckBox: TCheckBox;
    IsKeyCheckBox: TCheckBox;
  strict
  private
    procedure FillDataTypeComboBoxList; protected
    procedure SetModelUpdateAction(const AValue: TModelUpdateAction); override;
  public
    procedure SaveToAction; override;
    constructor Create(AOwner: TComponent); override;
  end;

implementation

{$R *.dfm}

uses
  Types,
  EF.Tree;

{ TModelFieldUpdateActionFrame }

constructor TModelFieldUpdateActionFrame.Create(AOwner: TComponent);
begin
  inherited;
  FillDataTypeComboBoxList;
end;

procedure TModelFieldUpdateActionFrame.FillDataTypeComboBoxList;
var
  LNames: TArray<string>;
  LName: string;
begin
  DataTypeComboBox.Items.Clear;
  LNames := TEFDataTypeRegistry.Instance.GetDataTypeNames;
  for LName in LNames do
    DataTypeComboBox.Items.Add(LName);
end;

procedure TModelFieldUpdateActionFrame.SaveToAction;
begin
  inherited;
  ModelUpdateAction.Metadata.SetString('FieldName', FieldNameEdit.Text);
  ModelUpdateAction.Metadata.SetString('DataType', DataTypeComboBox.Text);
  ModelUpdateAction.Metadata.SetInteger('Size', StrToInt(SizeEdit.Text));
  ModelUpdateAction.Metadata.SetBoolean('IsRequired', IsRequiredCheckBox.Checked);
  ModelUpdateAction.Metadata.SetBoolean('IsKey', IsKeyCheckBox.Checked);
end;

procedure TModelFieldUpdateActionFrame.SetModelUpdateAction(
  const AValue: TModelUpdateAction);
begin
  inherited;
  if Assigned(AValue) then
  begin
    FieldNameEdit.Text := AValue.Metadata.GetString('FieldName');
    FieldNameEdit.ReadOnly := not (AValue is TAddField);
    PhysicalNameEdit.Text := AValue.Metadata.GetString('PhysicalName');

    DataTypeComboBox.Text := AValue.Metadata.GetString('DataType');
    SizeEdit.Text := Avalue.Metadata.GetString('Size');
    IsRequiredCheckBox.Checked := AValue.Metadata.GetBoolean('IsRequired');
    IsKeyCheckBox.Checked := AValue.Metadata.GetBoolean('IsKey');
  end;
end;

end.
