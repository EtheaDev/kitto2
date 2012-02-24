unit KIDE.MetadataHelpers;

interface

uses
  EF.Tree, EF.DB,
  Kitto.Metadata.Models;

type
  TKModelHelper = class helper for TKModel
  public
    procedure SetModelName(const AModelName: string);
    procedure DeleteDetailReference(const ADetailReference: TKModelDetailReference);
    procedure DeleteField(const AField: TKModelField);
    procedure AddField(const AField: TKModelField);
  end;

  TKModelFieldHelper = class helper for TKModelField
  private
  public
    procedure SetFieldSpec(const ADataType: TEFDataType; const ASize: Integer;
      const AIsRequired, AIsKey: Boolean; const AReferencedModel: string);

    procedure SetIsKey(const AValue: Boolean);
    function EqualsColumnInfo(const AColumnInfo: TEFDBColumnInfo): Boolean;
  end;

  TKModelDetailReferencesHelper = class helper for TKModelDetailReferences
  public
    procedure DeleteDetailReference(const ADetailReference: TKModelDetailReference);
  end;

implementation

uses
  Types, SysUtils;

{ TKModelHelper }

procedure TKModelHelper.AddField(const AField: TKModelField);
begin
  Assert(Assigned(AField));

  GetFields.AddChild(AField);
end;

procedure TKModelHelper.DeleteDetailReference(
  const ADetailReference: TKModelDetailReference);
begin
  Assert(Assigned(ADetailReference));

  GetDetailReferences.DeleteDetailReference(ADetailReference);
end;

procedure TKModelHelper.DeleteField(const AField: TKModelField);
begin
  Assert(Assigned(AField));

  GetFields.RemoveChild(AField);
end;

procedure TKModelHelper.SetModelName(const AModelName: string);
begin
  SetString('ModelName', AModelName);
  PersistentName := AModelName;
end;

{ TKModelDetailReferencesHelper }

procedure TKModelDetailReferencesHelper.DeleteDetailReference(
  const ADetailReference: TKModelDetailReference);
begin
  Assert(Assigned(ADetailReference));

  RemoveChild(ADetailReference);
end;

{ TKModelFieldHelper }

procedure TKModelFieldHelper.SetIsKey(const AValue: Boolean);
var
  LDataType: TEFDataType;
  LSize: Integer;
  LIsRequired: Boolean;
  LIsKey: Boolean;
  LReferencedModel: string;
begin
  GetFieldSpec(LDataType, LSize, LIsRequired, LIsKey, LReferencedModel);
  SetFieldSpec(LDataType, LSize, LIsRequired, AValue, LReferencedModel);
end;

function TKModelFieldHelper.EqualsColumnInfo(
  const AColumnInfo: TEFDBColumnInfo): Boolean;
begin
  Result := False;
  if Assigned(AColumnInfo) then
    Result := (DataType = AColumnInfo.DataType) and (Size = AColumnInfo.Size)
      and (IsRequired = AColumnInfo.IsRequired);
end;

procedure TKModelFieldHelper.SetFieldSpec(const ADataType: TEFDataType;
  const ASize: Integer; const AIsRequired: Boolean; const AIsKey: Boolean;
  const AReferencedModel: string);
var
  LSpec: string;
begin
  LSpec := ADataType.GetTypeName;
  if ADataType is TKReferenceDataType then
    LSpec := LSpec + '(' + AReferencedModel + ')'
  else if ASize <> 0 then
    LSpec := LSpec + '(' + IntToStr(ASize) + ')';
  if AIsRequired then
    LSpec := LSpec + ' not null';
  if AIsKey then
    LSpec := LSpec + ' primary key';
  AsString := LSpec;
end;

end.
