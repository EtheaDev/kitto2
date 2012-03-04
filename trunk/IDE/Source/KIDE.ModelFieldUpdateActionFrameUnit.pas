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
    ScaleEdit: TLabeledEdit;
    procedure ControlChange(Sender: TObject);
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

procedure TModelFieldUpdateActionFrame.ControlChange(Sender: TObject);
var
  LDataType: TEFDataType;
begin
  inherited;
  if TEFDataTypeRegistry.Instance.HasClass(DataTypeComboBox.Text) then
  begin
    LDataType := TEFDataTypeFactory.Instance.GetDataType(DataTypeComboBox.Text);
    SizeEdit.Enabled := LDataType.HasSize;
    ScaleEdit.Enabled := LDataType.HasScale;
  end;
  if IsKeyCheckBox.Checked then
    IsRequiredCheckBox.Checked := True;
  if not IsRequiredCheckBox.Checked then
    IsKeyCheckBox.Checked := False;
end;

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
  ModelUpdateAction.Metadata.SetInteger('Size', StrToIntDef(SizeEdit.Text, 0));
  ModelUpdateAction.Metadata.SetInteger('Scale', StrToIntDef(ScaleEdit.Text, 0));
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
    ScaleEdit.Text := Avalue.Metadata.GetString('Scale');
    IsRequiredCheckBox.Checked := AValue.Metadata.GetBoolean('IsRequired');
    IsKeyCheckBox.Checked := AValue.Metadata.GetBoolean('IsKey');
  end;
end;

end.
