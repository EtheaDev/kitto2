unit KIDE.ModelUpdateActionFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, KIDE.ModelCreator,
  Vcl.StdCtrls;

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
  KIDE.ModelFieldUpdateActionFrameUnit;

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
