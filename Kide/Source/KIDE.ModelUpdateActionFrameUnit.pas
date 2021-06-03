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
unit KIDE.ModelUpdateActionFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, KIDE.ModelCreator,
  Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TModelUpdateActionFrame = class(TFrame)
    Label1: TLabel;
  strict private
    FModelUpdateAction: TModelUpdateAction;
  strict protected
    procedure SetModelUpdateAction(const AValue: TModelUpdateAction); virtual;
  public
    property ModelUpdateAction: TModelUpdateAction read FModelUpdateAction
      write SetModelUpdateAction;

    procedure SaveToAction; virtual;
  end;
  TModelUpdateActionFrameClass = class of TModelUpdateActionFrame;

function CreateModelUpdateActionFrame(const AAction: TModelUpdateAction;
  const AContainer: TWinControl): TModelUpdateActionFrame;

implementation

{$R *.dfm}

uses
  KIDE.TableInfoModelUpdateActionFrameUnit,
  KIDE.DetailReferenceUpdateActionFrameUnit,
  KIDE.ModelFieldUpdateActionFrameUnit,
  KIDE.ReferenceFieldUpdateActionFrameUnit;

function CreateModelUpdateActionFrame(const AAction: TModelUpdateAction;
  const AContainer: TWinControl): TModelUpdateActionFrame;
var
  LClass: TModelUpdateActionFrameClass;
begin
  Assert(Assigned(AAction));
  Assert(Assigned(AContainer));

  // Factory.
  if AAction is TTableInfoModelUpdateAction then
    LClass := TTableInfoModelUpdateActionFrame
  else if AAction is TDetailReferenceUpdateAction then
    LClass := TDetailReferenceUpdateActionFrame
  else if (AAction is TAddField) or (AAction is TModifyField) then
    LClass := TModelFieldUpdateActionFrame
  else if (AAction is TAddReferenceField) or (AAction is TModifyReferenceField) then
    LClass := TReferenceFieldUpdateActionFrame
  else
    LClass := TModelUpdateActionFrame;

  Result := LClass.Create(AContainer);
  try
    Result.Parent := AContainer;
    Result.Align := alClient;
    Result.ModelUpdateAction := AAction;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

{ TModelUpdateActionFrame }

procedure TModelUpdateActionFrame.SaveToAction;
begin
end;

procedure TModelUpdateActionFrame.SetModelUpdateAction(const AValue: TModelUpdateAction);
begin
  FModelUpdateAction := AValue;
end;

end.
