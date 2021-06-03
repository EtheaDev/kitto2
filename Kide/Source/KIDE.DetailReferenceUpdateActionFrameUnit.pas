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
