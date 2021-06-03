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
    ForeignKeyFieldsMemo: TMemo;
    ForeignKeyFieldsLabel: TLabel;
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
    ForeignKeyFieldsMemo.Lines.Text := AValue.Metadata.GetString('ForeignKeyFields');
    IsRequiredCheckBox.Checked := AValue.Metadata.GetBoolean('IsRequired');
  end;
end;

end.
