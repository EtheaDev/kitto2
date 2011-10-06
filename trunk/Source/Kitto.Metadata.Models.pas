unit Kitto.Metadata.Models;

interface

uses
  Types, Classes, Generics.Collections,
  EF.Classes, EF.Tree,
  Kitto.Metadata;

type
  TKModel = class;

  TKModelField = class(TKMetadataItem)
  private
    function GetFieldName: string;
    function GetDataType: TEFDataType;
    function GetSize: Integer;
    function GetIsRequired: Boolean;
    function GetQualifiedFieldName: string;
    function GetModel: TKModel;
    function GetDisplayLabel: string;
    function GetIsVisible: Boolean;
    function BeautifyFieldName(const AFieldName: string): string;
    function GetDisplayWidth: Integer;
    function GetIsReadOnly: Boolean;
    function GetIsKey: Boolean;
    function GetIsGenerated: Boolean;
    function GetEmptyAsNull: Boolean;
    function GetDefaultValue: string;
    function GetExpression: string;
  protected
    procedure GetFieldSpec(out ADataType: TEFDataType; out ASize: Integer;
      out AIsRequired: Boolean);
    procedure SetFieldSpec(const ADataType: TEFDataType;
      const ASize: Integer; const AIsRequired: Boolean);
  public
    property Model: TKModel read GetModel;
    property FieldName: string read GetFieldName;
    property QualifiedFieldName: string read GetQualifiedFieldName;
    property DataType: TEFDataType read GetDataType;
    property Size: Integer read GetSize;

    ///	<summary>
    ///	  Default requiredness status of this field in views. Defaults to
    ///   the database nullability status of the column.
    ///	</summary>
    property IsRequired: Boolean read GetIsRequired;

    ///	<summary>
    ///	  Default visibility status of this field in views. Defaults to True.
    ///	</summary>
    property IsVisible: Boolean read GetIsVisible;

    ///	<summary>
    ///	  Default read-only status of this field in views. Defaults to False.
    ///	</summary>
    property IsReadOnly: Boolean read GetIsReadOnly;

    ///	<summary>
    ///	  Returns True if the field is auto-generated at the database level,
    ///   such as an auto-increment field. Default is False.
    ///	</summary>
    property IsGenerated: Boolean read GetIsGenerated;

    property Expression: string read GetExpression;

    ///	<summary>
    ///	  Indicates that an empty value input by the user should be converted
    ///	  to null when writing to the database. For string/date/time/datetime
    ///	  fields defaults to True (False if the field is required); for other
    ///	  data types defaults to False.
    ///	</summary>
    ///	<remarks>
    ///	  Only relevant for string/date/time/datetime fields. In other cases,
    ///	  empty values are always converted to null. If the field is not of one
    ///	  of these types, this property always returns True.
    ///	</remarks>
    property EmptyAsNull: Boolean read GetEmptyAsNull;

    ///	<summary>
    ///	  Default label for this field in views. Defaults to a beautified field
    ///	  name. The beautifying function can be customized.
    ///	</summary>
    property DisplayLabel: string read GetDisplayLabel;

    ///	<summary>
    ///	  Default width for this field in views. Defaults to the field size for
    ///	  string fields, and reasonable sizes for other field types.
    ///	</summary>
    property DisplayWidth: Integer read GetDisplayWidth;

    ///	<summary>
    ///	  Optional value to set for the field when a new record is created.
    ///	</summary>
    property DefaultValue: string read GetDefaultValue;

    property IsKey: Boolean read GetIsKey;
  end;

  TKModelFields = class(TKMetadataItem)
  protected
    function GetChildClass(const AName: string): TEFNodeClass; override;
  public
    function FieldByName(const AName: string): TKModelField;
    function FindField(const AName: string): TKModelField;
  end;

  TKModelConstraint = class(TKMetadataItem)
  private
    function GetModel: TKModel;
    function GetQualifiedContraintName: string;
  protected
    function GetConstraintName: string; virtual; abstract;
  public
    property ConstraintName: string read GetConstraintName;
    property QualifiedContraintName: string read GetQualifiedContraintName;
    property Model: TKModel read GetModel;
  end;

  TKModelKey = class(TKModelConstraint)
  private
    function GetField(I: Integer): TKModelField;
    function GetFieldCount: Integer;
  protected
    function GetConstraintName: string; override;
    function GetFields: TEFNode;
  public
    property FieldCount: Integer read GetFieldCount;
    property Fields[I: Integer]: TKModelField read GetField;
    function GetFieldNames: TStringDynArray;
    function HasField(const AFieldName: string): Boolean;
  end;

  TKModelReference = class(TKModelConstraint)
  private
    function GetField(I: Integer): TKModelField;
    function GetFieldCount: Integer;
    function GetReferencedModel: TKModel;
    function GetReeferencedModelName: string;
    function GetReferencedField(I: Integer): TKModelField;
    function GetIsRequired: Boolean;
    property ReferencedModelName: string read GetReeferencedModelName;
  protected
    function GetConstraintName: string; override;
    function GetFields: TEFNode;
  public
    property FieldCount: Integer read GetFieldCount;
    property Fields[I: Integer]: TKModelField read GetField;
    property ReferencedFields[I: Integer]: TKModelField read GetReferencedField;
    property ReferencedModel: TKModel read GetReferencedModel;

    ///	<summary>
    ///	  True if all fields are required.
    ///	</summary>
    property IsRequired: Boolean read GetIsRequired;
  end;

  TKModelReferences = class(TKMetadataItem)
  protected
    function GetChildClass(const AName: string): TEFNodeClass; override;
  public
    function ReferenceByName(const AName: string): TKModelReference;
    function FindReference(const AName: string): TKModelReference;
  end;

  TKModels = class;

  TKModel = class(TKMetadata)
  private
    FModels: TKModels;
    function GetFieldCount: Integer;
    function GetField(I: Integer): TKModelField;
    function GetReferenceCount: Integer;
    function GetReference(I: Integer): TKModelReference;
    function GetModelName: string;
    function GetKey: TKModelKey;
    function GetDisplayLabel: string;
    function GetPluralDisplayLabel: string;
    function BeautifyModelName(const AModelName: string): string;
    function GetIsReadOnly: Boolean;
    function GetDefaultSorting: string;
  protected
    function GetFields: TKModelFields;
    function GetChildClass(const AName: string): TEFNodeClass; override;
    function GetReferences: TKModelReferences;
  public
    property Catalog: TKModels read FModels;

    property ModelName: string read GetModelName;
    property DisplayLabel: string read GetDisplayLabel;
    property PluralDisplayLabel: string read GetPluralDisplayLabel;

    property FieldCount: Integer read GetFieldCount;
    property Fields[I: Integer]: TKModelField read GetField;
    function FieldByName(const AName: string): TKModelField;
    function FindField(const AName: string): TKModelField;

    property Key: TKModelKey read GetKey;

    property ReferenceCount: Integer read GetReferenceCount;
    property References[I: Integer]: TKModelReference read GetReference;
    function ReferenceByName(const AName: string): TKModelReference;
    function FindReference(const AName: string): TKModelReference;
    procedure GetReferencesToModel(const AModel: TKModel; const AList: TList<TKModelReference>);

    property IsReadOnly: Boolean read GetIsReadOnly;

    ///	<summary>
    ///	  Optional fixed order by expression to apply when building the select
    ///	  SQL statement to display data. Should refer to fields through
    ///	  qualified names. Defaults to the list of fields in the key, if any.
    ///	</summary>
    property DefaultSorting: string read GetDefaultSorting;
  end;

  TKModels = class(TKMetadataCatalog)
  private
    function GetModelCount: Integer;
    function GetModel(I: Integer): TKModel;
  protected
    procedure AfterCreateObject(const AObject: TKMetadata); override;
    function GetObjectClassType: TKMetadataClass; override;
  public
    property ModelCount: Integer read GetModelCount;
    property Models[I: Integer]: TKModel read GetModel;
    function ModelByName(const AName: string): TKModel;
    function FindModel(const AName: string): TKModel;
  end;

implementation

uses
  SysUtils, StrUtils,
  EF.StrUtils;

function Pluralize(const AName: string): string;
begin
  Result := AName;
  if Result  <> '' then
  begin
    if EndsText('y', Result) then
      Result := StripSuffix(Result, 'y') + 'ies'
    else
      Result := Result + 's';
  end;
end;

{ TKModel }

function TKModel.FieldByName(const AName: string): TKModelField;
begin
  Result := GetFields.FieldByName(AName);
end;

function TKModel.FindField(const AName: string): TKModelField;
begin
  Result := GetFields.FindField(AName);
end;

function TKModel.FindReference(const AName: string): TKModelReference;
begin
  Result := GetReferences.FindReference(AName);
end;

function TKModel.ReferenceByName(const AName: string): TKModelReference;
begin
  Result := GetReferences.ReferenceByName(AName);
end;

function TKModel.GetField(I: Integer): TKModelField;
begin
  Result := GetFields[I] as TKModelField;
end;

function TKModel.GetFieldCount: Integer;
begin
  Result := GetFields.ChildCount;
end;

function TKModel.GetFields: TKModelFields;
begin
  Result := FindChild('Fields', True) as TKModelFields;
end;

function TKModel.GetReference(I: Integer): TKModelReference;
begin
  Result := GetReferences[I] as TKModelReference;
end;

function TKModel.GetReferenceCount: Integer;
begin
  Result := GetReferences.ChildCount;
end;

function TKModel.GetReferences: TKModelReferences;
begin
  Result := FindChild('References', True) as TKModelReferences;
end;

procedure TKModel.GetReferencesToModel(const AModel: TKModel;
  const AList: TList<TKModelReference>);
var
  I: Integer;
begin
  Assert(Assigned(AList));

  AList.Clear;
  for I := 0 to ReferenceCount - 1 do
  begin
    if References[I].ReferencedModel = AModel then
      AList.Add(References[I]);
  end;
end;

function TKModel.GetIsReadOnly: Boolean;
begin
  Result := GetBoolean('IsReadOnly');
end;

function TKModel.GetPluralDisplayLabel: string;
begin
  Result := GetString('PluralDisplayLabel');
  if Result = '' then
    Result := Pluralize(BeautifyModelName(ModelName));
end;

function TKModel.GetKey: TKModelKey;
begin
  Result := FindChild('Key', True) as TKModelKey;
end;

function TKModel.GetModelName: string;
begin
  Result := PersistentName;
end;

function TKModel.GetChildClass(const AName: string): TEFNodeClass;
begin
  if SameText(AName, 'Fields') then
    Result := TKModelFields
  else if SameText(AName, 'Key') then
    Result := TKModelKey
  else if SameText(AName, 'References') then
    Result := TKModelReferences
  else
    Result := inherited GetChildClass(AName);
end;

function TKModel.GetDefaultSorting: string;
var
  LFieldNames: TStringDynArray;
  I: Integer;
begin
  Result := GetString('DefaultSorting');
  if Result = '' then
  begin
    SetLength(LFieldNames, Key.FieldCount);
    for I := 0 to High(LFieldNames) do
      LFieldNames[I] := Key.Fields[I].QualifiedFieldName;
    Result := Join(LFieldNames, ', ');
  end;
end;

function TKModel.GetDisplayLabel: string;
begin
  Result := GetString('DisplayLabel');
  if Result = '' then
    Result := BeautifyModelName(ModelName);
end;

function TKModel.BeautifyModelName(const AModelName: string): string;
begin
  { TODO : allow to customize the beautifying function }
  Result := CamelToSpaced(UpperUnderscoreToCamel(AModelName));
end;

{ TKModelField }

procedure TKModelField.GetFieldSpec(out ADataType: TEFDataType; out ASize: Integer;
  out AIsRequired: Boolean);
var
  LStrings: TStringDynArray;
begin
  AIsRequired := EndsText(' not null', AsString);
  LStrings := SplitString(StripSuffix(AsString, ' not null'), '()');
  if Length(LStrings) > 0 then
    ADataType := StringToEFDataType(LStrings[0])
  else
    ADataType := edtUnknown;
  if Length(LStrings) > 1 then
    ASize := StrToInt(LStrings[1])
  else
    ASize := 0;
end;

procedure TKModelField.SetFieldSpec(const ADataType: TEFDataType;
  const ASize: Integer; const AIsRequired: Boolean);
var
  LDef: string;
begin
  LDef := EFDataTypeToString(ADataType);
  if ASize <> 0 then
    LDef := LDef + '(' + IntToStr(ASize) + ')';
  AsString := LDef;
end;

function TKModelField.GetIsGenerated: Boolean;
begin
  Result := GetBoolean('IsGenerated', False);
end;

function TKModelField.GetIsKey: Boolean;
begin
  Result := Model.Key.HasField(FieldName);
end;

function TKModelField.GetIsReadOnly: Boolean;
begin
  Result := GetBoolean('IsReadOnly', False);
end;

function TKModelField.GetIsRequired: Boolean;
var
  LDataType: TEFDataType;
  LSize: Integer;
begin
  GetFieldSpec(LDataType, LSize, Result);
end;

function TKModelField.GetIsVisible: Boolean;
begin
  Result := GetBoolean('IsVisible', True);
end;

function TKModelField.GetQualifiedFieldName: string;
begin
  Result := Model.ModelName + '.' + FieldName;
end;

function TKModelField.BeautifyFieldName(const AFieldName: string): string;
begin
  { TODO : allow to customize the beautifying function }
  Result := CamelToSpaced(UpperUnderscoreToCamel(AFieldName));
end;

function TKModelField.GetDataType: TEFDataType;
var
  LSize: Integer;
  LIsRequired: Boolean;
begin
  GetFieldSpec(Result, LSize, LIsRequired);
end;

function TKModelField.GetDefaultValue: string;
begin
  Result := GetString('DefaultValue');
end;

function TKModelField.GetDisplayLabel: string;
begin
  Result := GetString('DisplayLabel');
  if Result = '' then
    Result := BeautifyFieldName(FieldName);
end;

function TKModelField.GetDisplayWidth: Integer;
begin
  Result := GetInteger('DisplayWidth', -1);
  if Result = -1 then
  begin
    case DataType of
      edtString: Result := Size;
      edtInteger: Result := 5;
      edtDate: Result := 10;
      edtTime: Result := 8;
      edtDateTime: Result := 19;
      edtBoolean: Result := 5;
      edtCurrency, edtFloat, edtDecimal: Result := 12;
      edtObject: Result := 10;
    else
      Result := 20;
    end;
  end;
end;

function TKModelField.GetEmptyAsNull: Boolean;
var
  LNode: TEFNode;
begin
  if DataType in [edtString, edtDate, edtTime, edtDateTime] then
  begin
    LNode := FindChild('EmptyAsNull', False);
    if Assigned(LNode) then
      Result := LNode.AsBoolean
    else
      Result := not IsRequired;
  end
  else
    Result := True;
end;

function TKModelField.GetExpression: string;
begin
  Result := GetString('Expression');
end;

function TKModelField.GetFieldName: string;
begin
  Result := Name;
end;

function TKModelField.GetSize: Integer;
var
  LDataType: TEFDataType;
  LIsRequired: Boolean;
begin
  GetFieldSpec(LDataType, Result, LIsRequired);
end;

function TKModelField.GetModel: TKModel;
begin
  Assert(Assigned(Parent));
  Assert(Parent is TEFNode);
  Assert(Assigned(TEFNode(Parent).Parent));

  Result := TEFNode(Parent).Parent as TKModel;

  Assert(Assigned(Result));
end;

{ TKModelFields }

function TKModelFields.FieldByName(const AName: string): TKModelField;
begin
  Result := ChildByName(AName) as TKModelField;
end;

function TKModelFields.FindField(const AName: string): TKModelField;
begin
  Result := FindChild(AName) as TKModelField;
end;

function TKModelFields.GetChildClass(const AName: string): TEFNodeClass;
begin
  Result := TKModelField;
end;

{ TKModelKey }

function TKModelKey.GetFields: TEFNode;
begin
  Result := FindChild('Fields', True);
end;

function TKModelKey.HasField(const AFieldName: string): Boolean;
begin
  Result := MatchText(AFieldName, GetFieldNames);
end;

function TKModelKey.GetConstraintName: string;
begin
  Result := AsString;
end;

function TKModelKey.GetField(I: Integer): TKModelField;
begin
  Result := Model.FieldByName(GetFields[I].Name);
end;

function TKModelKey.GetFieldCount: Integer;
begin
  Result := GetFields.ChildCount;
end;

function TKModelKey.GetFieldNames: TStringDynArray;
var
  I: Integer;
begin
  SetLength(Result, GetFieldCount);
  for I := 0 to High(Result) do
    Result[I] := Fields[I].FieldName;
end;

{ TKModelConstraint }

function TKModelConstraint.GetQualifiedContraintName: string;
begin
  Result := Model.ModelName + '.' + ConstraintName;
end;

function TKModelConstraint.GetModel: TKModel;
var
  LParent: TEFTree;
begin
  LParent := Parent;
  while Assigned(LParent) and (LParent is TEFNode) do
    LParent := TEFNode(Parent).Parent;

  Result := LParent as TKModel;

  Assert(Assigned(Result));
end;

{ TKModelReference }

function TKModelReference.GetField(I: Integer): TKModelField;
begin
  Result := Model.FieldByName(GetFields[I].Name);
end;

function TKModelReference.GetFieldCount: Integer;
begin
  Result := GetFields.ChildCount;
end;

function TKModelReference.GetFields: TEFNode;
begin
  Result := FindChild('Fields');
end;

function TKModelReference.GetReferencedField(I: Integer): TKModelField;
begin
  Result := ReferencedModel.FieldByName(GetFields[I].AsString);
end;

function TKModelReference.GetReferencedModel: TKModel;
begin
  Result := Model.Catalog.ModelByName(ReferencedModelName);
end;

function TKModelReference.GetReeferencedModelName: string;
begin
  Result := AsString;
end;

function TKModelReference.GetConstraintName: string;
begin
  Result := Name;
end;

function TKModelReference.GetIsRequired: Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := 0 to FieldCount - 1 do
  begin
    if not Fields[I].IsRequired then
    begin
      Result := False;
      Break;
    end;
  end;
end;

{ TKModels }

procedure TKModels.AfterCreateObject(const AObject: TKMetadata);
begin
  inherited;
  (AObject as TKModel).FModels := Self;
end;

function TKModels.FindModel(const AName: string): TKModel;
begin
  Result := FindObject(AName) as TKModel;
end;

function TKModels.GetObjectClassType: TKMetadataClass;
begin
  Result := TKModel;
end;

function TKModels.GetModel(I: Integer): TKModel;
begin
  Result := Objects[I] as TKModel;
end;

function TKModels.GetModelCount: Integer;
begin
  Result := ObjectCount;
end;

function TKModels.ModelByName(const AName: string): TKModel;
begin
  Result := ObjectByName(AName) as TKModel;
end;

{ TKModelReferences }

function TKModelReferences.FindReference(const AName: string): TKModelReference;
begin
  Result := FindChild(AName) as TKModelReference;
end;

function TKModelReferences.ReferenceByName(const AName: string): TKModelReference;
begin
  Result := ChildByName(AName) as TKModelReference;
end;

function TKModelReferences.GetChildClass(const AName: string): TEFNodeClass;
begin
  Result := TKModelReference;
end;

end.
