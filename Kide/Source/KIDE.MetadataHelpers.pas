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
unit KIDE.MetadataHelpers;

interface

uses
  EF.DB,
  Kitto.Metadata.Models, Kitto.Metadata.DataView;

type
  TKModelHelper = class helper for TKModel
  public
    procedure SetModelName(const AModelName: string);
    procedure AddDetailReference(const ADetailReference: TKModelDetailReference);
    procedure DeleteDetailReference(const ADetailReference: TKModelDetailReference);
    procedure AddField(const AField: TKModelField);
    procedure DeleteField(const AField: TKModelField);
  end;

  TKModelFieldHelper = class helper for TKModelField
  private
  public
    procedure SetFieldSpec(const ADataType: string; const ASize, AScale: Integer;
      const AIsRequired, AIsKey: Boolean; const AReferencedModel: string);

    procedure SetIsKey(const AValue: Boolean);
    function EqualsColumnInfo(const AColumnInfo: TEFDBColumnInfo): Boolean;
    function EqualsForeignKeyInfo(const AForeignKeyInfo: TEFDBForeignKeyInfo): Boolean;
    procedure AddField(const AField: TKModelField);
    procedure DeleteField(const AField: TKModelField);
  end;

  TKModelDetailReferencesHelper = class helper for TKModelDetailReferences
  public
    procedure AddDetailReference(const ADetailReference: TKModelDetailReference);
    procedure DeleteDetailReference(const ADetailReference: TKModelDetailReference);
  end;

  TKModelDetailReferenceHelper = class helper for TKModelDetailReference
  public
    function EqualsForeignKeyInfo(const AForeignKeyInfo: TEFDBForeignKeyInfo): Boolean;
  end;

implementation

uses
  Types, SysUtils;

{ TKModelHelper }

procedure TKModelHelper.AddDetailReference(
  const ADetailReference: TKModelDetailReference);
begin
  Assert(Assigned(ADetailReference));

  GetDetailReferences.AddDetailReference(ADetailReference);
end;

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

procedure TKModelDetailReferencesHelper.AddDetailReference(
  const ADetailReference: TKModelDetailReference);
begin
  Assert(Assigned(ADetailReference));

  AddChild(ADetailReference);
end;

procedure TKModelDetailReferencesHelper.DeleteDetailReference(
  const ADetailReference: TKModelDetailReference);
begin
  Assert(Assigned(ADetailReference));

  RemoveChild(ADetailReference);
end;

{ TKModelFieldHelper }

procedure TKModelFieldHelper.SetIsKey(const AValue: Boolean);
var
  LDataType: string;
  LSize, LScale: Integer;
  LIsRequired: Boolean;
  LIsKey: Boolean;
  LReferencedModel: string;
begin
  GetFieldSpec(LDataType, LSize, LScale, LIsRequired, LIsKey, LReferencedModel);
  SetFieldSpec(LDataType, LSize, LScale, LIsRequired, AValue, LReferencedModel);
end;

procedure TKModelFieldHelper.AddField(const AField: TKModelField);
begin
  Assert(Assigned(AField));

  GetFields.AddChild(AField);
end;

procedure TKModelFieldHelper.DeleteField(const AField: TKModelField);
begin
  Assert(Assigned(AField));

  GetFields.RemoveChild(AField);
end;

function TKModelFieldHelper.EqualsColumnInfo(
  const AColumnInfo: TEFDBColumnInfo): Boolean;
begin
  Result := False;
  if Assigned(AColumnInfo) then
    Result := SameText(DBColumnName, AColumnInfo.Name)
      and (DataType = AColumnInfo.DataType)
      and (Size = AColumnInfo.Size)
      and (IsRequired = AColumnInfo.IsRequired);
end;

function TKModelFieldHelper.EqualsForeignKeyInfo(
  const AForeignKeyInfo: TEFDBForeignKeyInfo): Boolean;

  function EqualsColumnNames: Boolean;
  var
    I: Integer;
  begin
    Result := FieldCount = AForeignKeyInfo.ColumnCount;
    if Result then
    begin
      for I := 0 to FieldCount - 1 do
      begin
        if AForeignKeyInfo.ColumnNames.IndexOf(Fields[I].DBColumnName) < 0 then
        begin
          Result := False;
          Break;
        end;
      end;
    end;
  end;

begin
  Result := False;
  if Assigned(AForeignKeyInfo) then
    Result := SameText(DBColumnName, AForeignKeyInfo.Name) and EqualsColumnNames;
end;

procedure TKModelFieldHelper.SetFieldSpec(const ADataType: string;
  const ASize, AScale: Integer; const AIsRequired: Boolean; const AIsKey: Boolean;
  const AReferencedModel: string);
var
  LSpec: string;
begin
  LSpec := ADataType;
  if AReferencedModel <> '' then
    LSpec := LSpec + '(' + AReferencedModel + ')'
  else if ASize <> 0 then
  begin
    LSpec := LSpec + '(' + IntToStr(ASize);
    if AScale <> 0 then
      LSpec := LSpec + ', ' + IntToStr(AScale);
    LSpec := LSpec + ')';
  end;
  if AIsRequired then
    LSpec := LSpec + ' not null';
  if AIsKey then
    LSpec := LSpec + ' primary key';
  AsString := LSpec;
end;

{ TKModelDetailReferenceHelper }

function TKModelDetailReferenceHelper.EqualsForeignKeyInfo(
  const AForeignKeyInfo: TEFDBForeignKeyInfo): Boolean;
begin
  Result := False;
  if Assigned(AForeignKeyInfo) then
    Result := SameText(DBForeignKeyName, AForeignKeyInfo.Name);
end;

end.
