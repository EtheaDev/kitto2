{-------------------------------------------------------------------------------
   Copyright 2012 Ethea S.r.l.

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

unit Kitto.Metadata.Models;

{$I Kitto.Defines.inc}

interface

uses
  Types, SysUtils, Classes, DB, Generics.Collections,
  EF.Classes, EF.Tree, EF.Types,
  Kitto.Metadata;

type
  TKReferenceDataType = class(TEFDataType)
  protected
    procedure InternalFieldValueToNode(const AField: TField; const ANode: TEFNode); override;
    procedure InternalYamlValueToNode(const AYamlValue: string; const ANode: TEFNode;
      const AFormatSettings: TFormatSettings); override;
  public
    class function GetTypeName: string; override;
  end;

  TKModel = class;

  TKRule = class(TKMetadataItem)
  end;

  TKRules = class(TKMetadataItem)
  private
    function GetRule(I: Integer): TKRule;
    function GetRuleCount: Integer;
  protected
    function GetChildClass(const AName: string): TEFNodeClass; override;
  public
    property RuleCount: Integer read GetRuleCount;
    property Rules[I: Integer]: TKRule read GetRule; default;

    ///	<summary>Returns True if there's a rule of the same type as the passed
    ///	one.</summary>
    function HasRule(const ARule: TKRule): Boolean;
  end;

  TKModelFields = class;

  TKModelField = class;

  TKModelFieldPredicate = reference to function(const AField: TKModelField): Boolean;

  TKModelField = class(TKMetadataItem)
  strict private
    function GetFieldName: string;
    function GetDataType: TEFDataType;
    function GetSize: Integer;
    function GetIsRequired: Boolean;
    function GetQualifiedDBColumnName: string;
    function GetModel: TKModel;
    function GetDisplayLabel: string;
    function GetIsVisible: Boolean;
    function BeautifyFieldName(const AFieldName: string): string;
    function GetDisplayWidth: Integer;
    function GetIsReadOnly: Boolean;
    function GetIsKey: Boolean;
    function GetIsGenerated: Boolean;
    function GetEmptyAsNull: Boolean;
    function GetDefaultValue: Variant;
    function GetExpression: string;
    function GetAllowedValues: TEFPairs;
    function GetRules: TKRules;
    function GetDecimalPrecision: Integer;
    function GetCanUpdate: Boolean;
    function GetCanInsert: Boolean;
    function GetIsReference: Boolean;
    function GetReferencedModel: TKModel;
    function GetReferenceFieldNames: TStringDynArray;
    function GetReferencedModelName: string;
    function GetParentField: TKModelField;
    function GetField(I: Integer): TKModelField;
    function GetFieldCount: Integer;
    function GetHint: string;
    function GetEditFormat: string;
    function GetDisplayFormat: string;
    function GetQualifiedDBColumnNameOrExpression: string;
    function GetFieldNameOrExpression: string;
    function GetIsContained: Boolean;
    function GetDBColumnName: string;
    function GetPhysicalName: string;
    function GetAliasedDBColumnName: string;
  strict private
    function GetFileNameField: string; protected
    function GetChildClass(const AName: string): TEFNodeClass; override;
    ///	<summary>Returns all main field properties at once.</summary>
    procedure GetFieldSpec(out ADataType: string; out ASize, ADecimalPrecision: Integer;
      out AIsRequired: Boolean; out AIsKey: Boolean; out AReferencedModel: string);
  strict protected
    function GetFields: TKModelFields;
  public
    procedure BeforeSave; override;
  public
    property Model: TKModel read GetModel;
    property FieldName: string read GetFieldName;
    property PhysicalName: string read GetPhysicalName;

    ///	<summary>Returns the PhysicalName or, if not specified, the
    ///	FieldName.</summary>
    property DBColumnName: string read GetDBColumnName;

    ///	<summary>Returns the physical column name (DBColumnName) plus, if the
    ///	field has a different llogical name, a space and the logical name
    ///	(FieldName).</summary>
    property AliasedDBColumnName: string read GetAliasedDBColumnName;

    property FieldNameOrExpression: string read GetFieldNameOrExpression;
    property QualifiedDBColumnName: string read GetQualifiedDBColumnName;
    property QualifiedDBColumnNameOrExpression: string read GetQualifiedDBColumnNameOrExpression;

    property DataType: TEFDataType read GetDataType;
    property Size: Integer read GetSize;
    property DecimalPrecision: Integer read GetDecimalPrecision;
    property EditFormat: string read GetEditFormat;
    property DisplayFormat: string read GetDisplayFormat;

    ///	<summary>If the field is contained (as with local children of a
    ///	reference fields), returns the parent field, otherwise nil.</summary>
    property ParentField: TKModelField read GetParentField;

    ///	<summary>Returns True if the field has a ParentField.</summary>
    property IsContained: Boolean read GetIsContained;

    ///	<summary>True if the field is a reference.</summary>
    property IsReference: Boolean read GetIsReference;

    ///	<summary>If the field is part of a reference field, returns the number
    ///	of physical fields that make up the reference.</summary>
    property FieldCount: Integer read GetFieldCount;

    ///	<summary>If the field is part of a reference field, returns the
    ///	physical fields that make up the reference.</summary>
    property Fields[I: Integer]: TKModelField read GetField;

    function FieldByName(const AName: string): TKModelField;
    function FindField(const AName: string): TKModelField;
    function FindFieldByPhysicalName(const APhysicalName: string): TKModelField;
    function FindFieldByPredicate(const APredicate: TKModelFieldPredicate): TKModelField;

    ///	<summary>Returna the names of the sub-fields, if any.</summary>
    function GetFieldNames: TStringDynArray;

    ///	<summary>If the field is a reference, returns the referenced model's
    ///	name, otherwise ''.</summary>
    property ReferencedModelName: string read GetReferencedModelName;

    ///	<summary>If the field is a reference, returns the referenced model,
    ///	otherwise nil.</summary>
    property ReferencedModel: TKModel read GetReferencedModel;

    ///	<summary>If the field is a reference, returns the reference field names
    ///	(that is the names of the fields, in the underlying table, that make up
    ///	the foreign key to the referenced model.</summary>
    property ReferenceFieldNames: TStringDynArray read GetReferenceFieldNames;

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

    ///	<summary>A field that is not a physical field but rather computed by a
    ///	SQL expression will have the expression stored in this
    ///	property.</summary>
    property Expression: string read GetExpression;

    property CanInsert: Boolean read GetCanInsert;
    property CanUpdate: Boolean read GetCanUpdate;

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

    ///	<summary>If the field has a fixed list of allowed values, it is stored
    ///	here. Each value has as an associated label.</summary>
    ///	<remarks>Only string fields are currently supported.</remarks>
    property AllowedValues: TEFPairs read GetAllowedValues;

    ///	<summary>
    ///	  Default label for this field in views. Defaults to a beautified field
    ///	  name. The beautifying function can be customized.
    ///	</summary>
    property DisplayLabel: string read GetDisplayLabel;

    property Hint: string read GetHint;

    ///	<summary>
    ///	  Default width for this field in views. Defaults to the field size for
    ///	  string fields, and reasonable sizes for other field types.
    ///	</summary>
    property DisplayWidth: Integer read GetDisplayWidth;

    ///	<summary>
    ///	  Optional value to set for the field when a new record is created.
    ///	</summary>
    property DefaultValue: Variant read GetDefaultValue;

    property IsKey: Boolean read GetIsKey;

    property Rules: TKRules read GetRules;

    ///	<summary>For blob or file reference fields, optionally specifies the
    ///	name of another field in the same model that will store the
    ///	original file name upon upload.</summary>
    property FileNameField: string read GetFileNameField;
  end;

  TKModelFields = class(TKMetadataItem)
  private
    function GetParentField: TKModelField;
    function GetField(I: Integer): TKModelField;
    function GetFieldCount: Integer;
  protected
    function GetChildClass(const AName: string): TEFNodeClass; override;
  public
    function FieldByName(const AName: string): TKModelField;
    function FindField(const AName: string): TKModelField;
    function FindFieldByPhysicalName(const APhysicalName: string): TKModelField;
    function FindFieldByPredicate(const APredicate: TKModelFieldPredicate): TKModelField;
    property FieldCount: Integer read GetFieldCount;
    property Fields[I: Integer]: TKModelField read GetField; default;
    function GetFieldNames: TStringDynArray;

    ///	<summary>If the fields are contained inside a parent field, this
    ///	property returns the parent field, otherwise nil.</summary>
    property ParentField: TKModelField read GetParentField;
  end;

  TKModelSubobject = class(TKMetadataItem)
  private
    function GetModel: TKModel;
  public
    property Model: TKModel read GetModel;
  end;

  TKModelDetailReference = class(TKModelSubobject)
  private
    function GetDetailReferenceName: string;
    function GetReferenceField: TKModelField;
    function GetDetailModel: TKModel;
    function GetDetailModelName: string;
    function GetReferenceFieldName: string;
    function GetDisplayLabel: string;
    function BeautifyDetailName(const ADetailName: string): string;
    function GetDBForeignKeyName: string;
    function GetPhysicalName: string;
  public
    property PhysicalName: string read GetPhysicalName;
    ///	<summary>Returns PhysicalName.</summary>
    property DBForeignKeyName: string read GetDBForeignKeyName;
    property DisplayLabel: string read GetDisplayLabel;
    property DetailReferenceName: string read GetDetailReferenceName;
    property DetailModel: TKModel read GetDetailModel;
    property DetailModelName: string read GetDetailModelName;

    ///	<summary>Returns the counterpart reference field in the detail model.
    ///	If there's only one reference field from the detail model to this
    ///	master model, then it's assumed it is the one being looked for,
    ///	otherwise its name is fetched from the ReferenceField parameter of the
    ///	DetailReference.</summary>
    property ReferenceField: TKModelField read GetReferenceField;
    property ReferenceFieldName: string read GetReferenceFieldName;
  end;

  TKModelDetailReferences = class(TKMetadataItem)
  private
    function GetDetailReference(I: Integer): TKModelDetailReference;
    function GetDetailReferenceCount: Integer;
  protected
    function GetChildClass(const AName: string): TEFNodeClass; override;
  public
    property DetailReferences[I: Integer]: TKModelDetailReference read GetDetailReference; default;
    property DetailReferenceCount: Integer read GetDetailReferenceCount;
    function DetailReferenceByName(const AName: string): TKModelDetailReference;
    function FindDetailReference(const AName: string): TKModelDetailReference;
    function FindDetailReferenceToModel(const AModel: TKModel): TKModelDetailReference;
    function FindDetailReferenceToField(const AField: TKModelField): TKModelDetailReference;
    function FindDetailReferenceByPhysicalName(const APhysicalName: string): TKModelDetailReference;
  end;

  TKModels = class;

  TKModel = class(TKMetadata)
  private
    function GetFieldCount: Integer;
    function GetField(I: Integer): TKModelField;
    function GetModelName: string;
    function GetDisplayLabel: string;
    function GetPluralDisplayLabel: string;
    function GetIsReadOnly: Boolean;
    function GetDefaultSorting: string;
    function GetIsLarge: Boolean;
    function GetDefaultFilter: string;
    function GetRules: TKRules;
    function GetDetailReference(I: Integer): TKModelDetailReference;
    function GetDetailReferenceCount: Integer;
    function GetImageName: string;
    function GetCaptionField: TKModelField;
    function GetCaptionFieldName: string;
    function GetKeyField(I: Integer): TKModelField;
    function GetKeyFieldCount: Integer;
    function GetCatalog: TKModels;
    function GetDBTableName: string;
    function GetPhysicalName: string;
    const DEFAULT_IMAGE_NAME = 'default_model';
    class function BeautifyModelName(const AModelName: string): string;
  protected
    function GetFields: TKModelFields;
    function GetChildClass(const AName: string): TEFNodeClass; override;
    function GetDetailReferences: TKModelDetailReferences;
  public
    procedure BeforeSave; override;
  public
    property Catalog: TKModels read GetCatalog;

    property ModelName: string read GetModelName;

    property PhysicalName: string read GetPhysicalName;

    ///	<summary>Returns the physical database table name (PhysicalName
    ///	property) or, if not specified, the ModelName. It is the name to be
    ///	used to update the physical table.</summary>
    property DBTableName: string read GetDBTableName;
    property DisplayLabel: string read GetDisplayLabel;
    property PluralDisplayLabel: string read GetPluralDisplayLabel;
    property ImageName: string read GetImageName;

    property FieldCount: Integer read GetFieldCount;
    property Fields[I: Integer]: TKModelField read GetField;
    function FieldByName(const AName: string): TKModelField;
    function FindField(const AName: string): TKModelField;
    function FindFieldByPhysicalName(const APhysicalName: string): TKModelField;

    ///	<summary>Returns an array of key physical field names.</summary>
    ///	<param name="AQualify">If True, each field name is prefixed with the
    ///	table name and a dot.</param>
    ///	<param name="AAlias">If True, makes it so that every field that has a
    ///	different physical name is aliased (meaning the physical name is
    ///	output, followed by a space and the FieldName).</param>
    function GetKeyDBColumnNames(const AQualify: Boolean = False;
      const AAlias: Boolean = False): TStringDynArray;
    property KeyFieldCount: Integer read GetKeyFieldCount;
    property KeyFields[I: Integer]: TKModelField read GetKeyField;

    ///	<summary>Returns an array of key field names.</summary>
    function GetKeyFieldNames: TStringDynArray;
    property DetailReferenceCount: Integer read GetDetailReferenceCount;
    property DetailReferences[I: Integer]: TKModelDetailReference read GetDetailReference;
    function DetailReferenceByName(const AName: string): TKModelDetailReference;
    function FindDetailReference(const AName: string): TKModelDetailReference;
    function FindDetailReferenceByPhysicalName(const APhysicalName: string): TKModelDetailReference;

    ///	<summary>If there's exactly one detail reference to the specified
    ///	model, returns it, otherwise returns nil.</summary>
    function FindDetailReferenceByModel(const AModel: TKModel): TKModelDetailReference;

    ///	<summary>Returns the first found detail reference to the specified
    ///	reference field. If not found, returns nil.</summary>
    function FindDetailReferenceByField(const AField: TKModelField): TKModelDetailReference;

    ///	<summary>If there's exactly one field referencing the specified model,
    ///	it is returned. Otherwise the method returns nil.</summary>
    function FindReferenceField(const AModel: TKModel): TKModelField; overload;

    ///	<summary>Finds and returns a field referencing a model with the
    ///	specified name and all subfields listed in the specified field name
    ///	array (and only these), otherwise nil.</summary>
    ///	<param name="AModelName">Name of the referenced model.</param>
    ///	<param name="AFieldNames">Names of the referencing fields.</param>
    ///	<remarks>If AFieldNames contains only one item, a same-named reference
    ///	field with no subfields does qualify as a return value. If AFieldNames
    ///	contains several items (indicating a multi-field reference) then the
    ///	name of the field does not matter and only the subfields
    ///	contribute.</remarks>
    function FindReferenceField(const AModelName: string;
      const AFieldNames: TStringDynArray): TKModelField; overload;

    ///	<summary>If a reference field with a ForeignKeyName property equals to
    ///	the specified name exists, it is returned. Otherwise nil is
    ///	returned.</summary>
    ///	<param name="AForeignKeyName">Name of the database-level foreign
    ///	key.</param>
    ///	<remarks>Not all reference fields have a ForeignKeyName set.</remarks>
    function FindReferenceField(const AForeignKeyName: string): TKModelField; overload;

    property IsReadOnly: Boolean read GetIsReadOnly;
    property DefaultFilter: string read GetDefaultFilter;

    ///	<summary>True if the model's underlying data store is a large une. Used
    ///	to decide the kind of lookup combo box to create. Se this to True if
    ///	the cardinality of the underlying database table exceeds what you are
    ///	comfortable to put in an Ajax response (which typically contains
    ///	several lookup sets).</summary>
    property IsLarge: Boolean read GetIsLarge;

    ///	<summary>
    ///	  Optional fixed order by expression to apply when building the select
    ///	  SQL statement to display data. Should refer to fields through
    ///	  qualified names. Defaults to the list of fields in the key, if any.
    ///	</summary>
    property DefaultSorting: string read GetDefaultSorting;

    property CaptionFieldName: string read GetCaptionFieldName;
    property CaptionField: TKModelField read GetCaptionField;

    property Rules: TKRules read GetRules;
  end;

  TKModelList = class(TList<TKModel>)
  public
    procedure AddModelNamesToStrings(const AStrings: TStrings);
  end;

  TKModels = class(TKMetadataCatalog)
  private
    function GetModelCount: Integer;
    function GetModel(I: Integer): TKModel;
  protected
    function GetObjectClassType: TKMetadataClass; override;
  public
    property ModelCount: Integer read GetModelCount;
    property Models[I: Integer]: TKModel read GetModel; default;
    function ModelByName(const AName: string): TKModel;
    function FindModel(const AName: string): TKModel;
    function FindModelByPhysicalName(const APhysicalName: string): TKModel;
  end;

///	<summary>Returns the input value unless it's a supported literal, in which
///	case evaluates the literal and returns it. Used by model and view fields to
///	compute default values.</summary>
function EvalExpression(const AExpression: Variant): Variant;

function Pluralize(const AName: string): string;

implementation

uses
  StrUtils, Variants,
  EF.StrUtils, EF.VariantUtils, EF.Localization,
  Kitto.Types, Kitto.Config;

function Pluralize(const AName: string): string;
begin
  Result := AName;
  if Result  <> '' then
  begin
    if EndsText('y', Result) then
      Result := StripSuffix(Result, 'y') + 'ies'
    else if not EndsText('s', Result) then
      Result := Result + 's';
  end;
end;

function EvalExpression(const AExpression: Variant): Variant;
begin
  Result := AExpression;
  if SameText(EFVarToStr(Result), '{date}') then
    Result := Date
  else if SameText(EFVarToStr(Result), '{now}') then
    Result := Now;
end;

{ TKModel }

procedure TKModel.BeforeSave;
begin
  inherited;
  // Avoid storing the DetailReferences node if it's empty.
  if GetDetailReferences.DetailReferenceCount = 0 then
    DeleteNode('DetailReferences');
end;

function TKModel.DetailReferenceByName(const AName: string): TKModelDetailReference;
begin
  Result := GetDetailReferences.DetailReferenceByName(AName);
end;

function TKModel.FieldByName(const AName: string): TKModelField;
begin
  Result := GetFields.FieldByName(AName);
end;

function TKModel.FindDetailReference(const AName: string): TKModelDetailReference;
begin
  Result := GetDetailReferences.FindDetailReference(AName);
end;

function TKModel.FindDetailReferenceByField(const AField: TKModelField): TKModelDetailReference;
begin
  Result := GetDetailReferences.FindDetailReferenceToField(AField);
end;

function TKModel.FindDetailReferenceByPhysicalName(
  const APhysicalName: string): TKModelDetailReference;
begin
  Result := GetDetailReferences.FindDetailReferenceByPhysicalName(APhysicalName);
end;

function TKModel.FindDetailReferenceByModel(const AModel: TKModel): TKModelDetailReference;
begin
  Result := GetDetailReferences.FindDetailReferenceToModel(AModel);
end;

function TKModel.FindField(const AName: string): TKModelField;
begin
  Result := GetFields.FindField(AName);
end;

function TKModel.FindFieldByPhysicalName(const APhysicalName: string): TKModelField;
begin
  Result := GetFields.FindFieldByPhysicalName(APhysicalName);
end;

function TKModel.FindReferenceField(const AForeignKeyName: string): TKModelField;
var
  I: Integer;
begin
  Assert(AForeignKeyName <> '');

  Result := nil;
  for I := 0 to FieldCount - 1 do
  begin
    if Fields[I].IsReference and SameText(Fields[I].PhysicalName, AForeignKeyName) then
    begin
      Result := Fields[I];
      Break;
    end;
  end;
end;

function TKModel.FindReferenceField(const AModelName: string;
  const AFieldNames: TStringDynArray): TKModelField;
var
  LFieldIdx: Integer;
  LSubFieldIdx: Integer;
  LFound: Boolean;
begin
  Assert(AModelName <> '');
  Assert(Length(AFieldNames) > 0);

  Result := nil;
  for LFieldIdx := 0 to FieldCount - 1 do
  begin
    if Fields[LFieldIdx].IsReference and SameText(Fields[LFieldIdx].ReferencedModelName, AModelName) then
    begin
      if (Length(AFieldNames) = 1) and SameText(Fields[LFieldIdx].FieldName, AFieldNames[0]) then
      begin
        Result := Fields[LFieldIdx];
        Break;
      end
      else if (Length(AFieldNames) > 1) and (Length(AFieldNames) = Fields[LFieldIdx].FieldCount) then
      begin
        LFound := True;
        for LSubFieldIdx := Low(AFieldNames) to High(AFieldNames) do
        begin
          if Fields[LFieldIdx].FindField(AFieldNames[LSubFieldIdx]) = nil then
          begin
            LFound := False;
            Break;
          end;
        end;
        if LFound then
        begin
          Result := Fields[LFieldIdx];
          Break;
        end;
      end;
    end;
  end;
end;

function TKModel.FindReferenceField(const AModel: TKModel): TKModelField;
var
  I: Integer;
  LCount: Integer;
begin
  Assert(Assigned(AModel));

  Result := nil;
  LCount := 0;
  for I := 0 to FieldCount - 1 do
  begin
    if Fields[I].IsReference and (Fields[I].ReferencedModel = AModel) then
    begin
      Result := Fields[I];
      Inc(LCount);
      if LCount > 1 then
        Break;
    end;
  end;
  if LCount <> 1 then
    Result := nil;
end;

function TKModel.GetField(I: Integer): TKModelField;
begin
  Result := GetFields[I] as TKModelField;
end;

function TKModel.GetFieldCount: Integer;
begin
  Result := GetFields.FieldCount;
end;

function TKModel.GetFields: TKModelFields;
begin
  Result := FindChild('Fields', True) as TKModelFields;
end;

function TKModel.GetRules: TKRules;
begin
  Result := GetNode('Rules', True) as TKRules;
end;

function TKModel.GetImageName: string;
begin
  Result := GetString('ImageName');
  if Result = '' then
    Result := DEFAULT_IMAGE_NAME;
end;

function TKModel.GetIsLarge: Boolean;
begin
  Result := GetBoolean('IsLarge');
end;

function TKModel.GetIsReadOnly: Boolean;
begin
  Result := GetBoolean('IsReadOnly');
end;

function TKModel.GetPhysicalName: string;
begin
  Result := GetString('PhysicalName');
end;

function TKModel.GetPluralDisplayLabel: string;
begin
  Result := GetString('PluralDisplayLabel');
  if Result = '' then
    Result := Pluralize(DisplayLabel);
end;

function TKModel.GetKeyField(I: Integer): TKModelField;
var
  LKeyFieldNames: TStringDynArray;
begin
  LKeyFieldNames := GetKeyFieldNames;
  Assert(Length(LKeyFieldNames) > I);
  Result := FieldByName(LKeyFieldNames[I]);
end;

function TKModel.GetKeyFieldCount: Integer;
begin
  Result := Length(GetKeyFieldNames);
end;

function TKModel.GetKeyDBColumnNames(const AQualify: Boolean;
  const AAlias: Boolean): TStringDynArray;
var
  I: Integer;
  J: Integer;
begin
  SetLength(Result, FieldCount);
  J := 0;
  for I := 0 to FieldCount - 1 do
  begin
    if Fields[I].IsKey then
    begin
      if AQualify then
        Result[J] := Fields[I].QualifiedDBColumnName
      else
        Result[J] := Fields[I].DBColumnName;
      if AAlias and not SameText(Fields[I].DBColumnName, Fields[I].FieldName) then
        Result[J] := Result[J] + ' ' + Fields[I].FieldName;
      Inc(J);
    end;
  end;
  SetLength(Result, J);
end;

function TKModel.GetKeyFieldNames: TStringDynArray;
var
  I: Integer;
  J: Integer;
begin
  SetLength(Result, FieldCount);
  J := 0;
  for I := 0 to FieldCount - 1 do
  begin
    if Fields[I].IsKey then
    begin
      Result[J] := Fields[I].FieldName;
      Inc(J);
    end;
  end;
  SetLength(Result, J);
end;

function TKModel.GetModelName: string;
begin
  Result := PersistentName;
end;

function TKModel.GetCaptionField: TKModelField;
var
  LFieldName: string;
  I: Integer;
begin
  Result := nil;
  LFieldName := CaptionFieldName;
  if LFieldName <> '' then
    Result := FieldByName(LFieldName)
  else
  begin
    for I := 0 to FieldCount - 1 do
    begin
     if not Fields[I].IsKey then
     begin
       Result := Fields[I];
       Break;
     end;
    end;
    if (Result = nil) and (FieldCount > 0) then
      Result := Fields[0];
    if Result = nil then
      raise EKError.CreateFmt('Cannot determine CaptionField for model %s.',
        [ModelName]);
  end;
end;

function TKModel.GetCaptionFieldName: string;
begin
  Result := GetString('CaptionField');
end;

function TKModel.GetCatalog: TKModels;
begin
  Result := inherited Catalog as TKModels;
end;

function TKModel.GetChildClass(const AName: string): TEFNodeClass;
begin
  if SameText(AName, 'Fields') then
    Result := TKModelFields
  else if SameText(AName, 'DetailReferences') then
    Result := TKModelDetailReferences
  else if SameText(AName, 'Rules') then
    Result := TKRules
  else
    Result := inherited GetChildClass(AName);
end;

function TKModel.GetDefaultSorting: string;
begin
  Result := GetString('DefaultSorting');
  if Result = '' then
    Result := Join(GetKeyDBColumnNames(True), ', ');
end;

function TKModel.GetDetailReference(I: Integer): TKModelDetailReference;
begin
  Result := GetDetailReferences[I];
end;

function TKModel.GetDetailReferenceCount: Integer;
begin
  Result := GetDetailReferences.DetailReferenceCount;
end;

function TKModel.GetDetailReferences: TKModelDetailReferences;
begin
  Result := FindChild('DetailReferences', True) as TKModelDetailReferences;
end;

function TKModel.GetDBTableName: string;
begin
  Result := PhysicalName;
  if Result = '' then
    Result := ModelName;
end;

function TKModel.GetDefaultFilter: string;
begin
  Result := GetString('DefaultFilter');
end;

function TKModel.GetDisplayLabel: string;
begin
  Result := GetString('DisplayLabel');
  if Result = '' then
    Result := BeautifyModelName(ModelName);
end;

class function TKModel.BeautifyModelName(const AModelName: string): string;
begin
  { TODO : allow to customize the beautifying function }
  Result := AModelName;
  if (Result = UpperCase(Result)) or (Pos('_', Result) > 0) then
    Result := UpperUnderscoreToCamel(Result);
  Result := CamelToSpaced(Result);
end;

{ TKModelField }

procedure TKModelField.GetFieldSpec(out ADataType: string; out ASize, ADecimalPrecision: Integer;
  out AIsRequired: Boolean; out AIsKey: Boolean; out AReferencedModel: string);
var
  LStrings: TStringDynArray;
  LDataType: TEFDataType;
begin
  AIsRequired := ContainsText(AsString, ' not null');
  AIsKey := ContainsText(AsString, ' primary key');
  LStrings := Split(StripSuffix(StripSuffix(AsString, ' primary key'), ' not null'), '(,)');
  while (Length(LStrings) > 0) and (Trim(LStrings[High(LStrings)]) = '') do
    SetLength(LStrings, Length(LStrings) - 1);

  if Length(LStrings) > 0 then
    ADataType := LStrings[0]
  else
    ADataType := TEFStringDataType.GetTypeName;
  if Length(LStrings) > 1 then
  begin
    LDataType := TEFDataTypeFactory.Instance.GetDataType(ADataType);
    if LDataType is TKReferenceDataType then
    begin
      ASize := 0;
      ADecimalPrecision := 0;
      AReferencedModel := LStrings[1];
    end
    else
    begin
      ASize := StrToInt(Trim(LStrings[1]));
      if Length(LStrings) > 2 then
        ADecimalPrecision := StrToInt(Trim(LStrings[2]));
      AReferencedModel := '';
    end;
  end
  else
  begin
    ASize := 0;
    ADecimalPrecision := 0;
    AReferencedModel := '';
  end;
end;

function TKModelField.GetFileNameField: string;
begin
  Result := GetString('FileNameField');
end;

function TKModelField.GetHint: string;
begin
  Result := GetString('Hint');
end;

function TKModelField.GetIsContained: Boolean;
begin
  Result := ParentField <> nil;
end;

function TKModelField.GetIsGenerated: Boolean;
begin
  Result := GetBoolean('IsGenerated', False);
end;

function TKModelField.GetIsKey: Boolean;
var
  LDataType: string;
  LSize, LDecimalPrecision: Integer;
  LIsRequired: Boolean;
  LReferencedModel: string;
  LParentField: TKModelField;
begin
  LParentField := ParentField;
  if Assigned(LParentField) then
    Result := ParentField.IsKey
  else
    GetFieldSpec(LDataType, LSize, LDecimalPrecision, LIsRequired, Result, LReferencedModel);
end;

function TKModelField.GetIsReadOnly: Boolean;
begin
  Result := GetBoolean('IsReadOnly', False);
end;

function TKModelField.GetIsReference: Boolean;
begin
  Result := DataType is TKReferenceDataType;
end;

function TKModelField.GetIsRequired: Boolean;
var
  LDataType: string;
  LSize, LDecimalPrecision: Integer;
  LIsKey: Boolean;
  LReferencedModel: string;
  LParentField: TKModelField;
begin
  LParentField := ParentField;
  if Assigned(LParentField) then
    Result := ParentField.IsRequired
  else
    GetFieldSpec(LDataType, LSize, LDecimalPrecision, Result, LIsKey, LReferencedModel);
end;

function TKModelField.GetIsVisible: Boolean;
begin
  Result := GetBoolean('IsVisible', True);
end;

function TKModelField.GetQualifiedDBColumnName: string;
begin
  Result := Model.DBTableName + '.' + DBColumnName;
end;

function TKModelField.GetQualifiedDBColumnNameOrExpression: string;
begin
  if Expression <> '' then
    Result := Expression
  else
    Result := QualifiedDBColumnName;
end;

function TKModelField.GetReferencedModel: TKModel;
begin
  if IsReference then
    Result := Model.Catalog.ModelByName(ReferencedModelName)
  else
    Result := nil;
end;

function TKModelField.GetReferencedModelName: string;
var
  LDataType: string;
  LSize, LDecimalPrecision: Integer;
  LIsRequired: Boolean;
  LIsKey: Boolean;
  LParentField: TKModelField;
begin
  LParentField := ParentField;
  if Assigned(LParentField) and (LParentField.IsReference) then
    Result := ParentField.ReferencedModelName
  else
    GetFieldSpec(LDataType, LSize, LDecimalPrecision, LIsRequired, LIsKey, Result);
end;

function TKModelField.GetReferenceFieldNames: TStringDynArray;
begin
  if IsReference then
    Result := GetNode('Fields').GetChildNames
  else
    Result := nil;
end;

function TKModelField.GetRules: TKRules;
begin
  Result := GetNode('Rules', True) as TKRules;
end;

function TKModelField.BeautifyFieldName(const AFieldName: string): string;
begin
  { TODO : allow to customize the beautifying function }
  Result := AFieldName;
  if (Result = UpperCase(Result)) or (Pos('_', Result) > 0) then
    Result := UpperUnderscoreToCamel(Result);
  Result := CamelToSpaced(Result);
end;

procedure TKModelField.BeforeSave;
begin
  inherited;
  // Avoid storing the Fields node if it's empty.
  if GetFields.FieldCount = 0 then
    DeleteNode('Fields');
end;

function TKModelField.FieldByName(const AName: string): TKModelField;
begin
  Result := GetFields.FieldByName(AName);
end;

function TKModelField.FindField(const AName: string): TKModelField;
begin
  Result := GetFields.FindField(AName);
end;

function TKModelField.FindFieldByPhysicalName(
  const APhysicalName: string): TKModelField;
begin
  Result := GetFields.FindFieldByPhysicalName(APhysicalName);
end;

function TKModelField.FindFieldByPredicate(
  const APredicate: TKModelFieldPredicate): TKModelField;
begin
  Result := FindChildByPredicate(
    function (const ANode: TEFNode): Boolean
    begin
      Result := APredicate(ANode as TKModelField);
    end) as TKModelField;
end;

function TKModelField.GetAliasedDBColumnName: string;
begin
  Result := DBColumnName;
  if Result <> FieldName then
    Result := Result + ' ' + FieldName;
end;

function TKModelField.GetAllowedValues: TEFPairs;
begin
  Result := GetChildrenAsPairs('AllowedValues');
end;

function TKModelField.GetCanUpdate: Boolean;
begin
  Result := Expression = '';
end;

function TKModelField.GetCanInsert: Boolean;
begin
  Result := CanUpdate;
end;

function TKModelField.GetChildClass(const AName: string): TEFNodeClass;
begin
  if SameText(AName, 'Rules') then
    Result := TKRules
  else if SameText(AName, 'Fields') then
    Result := TKModelFields
  else
    Result := inherited GetChildClass(AName);
end;

function TKModelField.GetDataType: TEFDataType;
var
  LSize, LDecimalPrecision: Integer;
  LIsRequired: Boolean;
  LIsKey: Boolean;
  LReferencedModel: string;
  LParentField: TKModelField;
  LDataType: string;
begin
  LParentField := ParentField;
  if Assigned(LParentField) and ParentField.IsReference then
    Result := ParentField.ReferencedModel.KeyFields[Index].DataType
  else
  begin
    GetFieldSpec(LDataType, LSize, LDecimalPrecision, LIsRequired, LIsKey, LReferencedModel);
    Result := TEFDataTypeFactory.Instance.GetDataType(LDataType)
  end;
end;

function TKModelField.GetDBColumnName: string;
begin
  Result := PhysicalName;
  if Result = '' then
    Result := FieldName;
end;

function TKModelField.GetDecimalPrecision: Integer;
var
  LDataType: string;
  LSize: Integer;
  LIsRequired: Boolean;
  LIsKey: Boolean;
  LReferencedModel: string;
  LParentField: TKModelField;
begin
  LParentField := ParentField;
  if Assigned(LParentField) and ParentField.IsReference then
    Result := ParentField.ReferencedModel.KeyFields[Index].DecimalPrecision
  else
    GetFieldSpec(LDataType, LSize, Result, LIsRequired, LIsKey, LReferencedModel);
  if Result = 0 then
    Result := 2;
end;

function TKModelField.GetDefaultValue: Variant;
begin
  Result := EvalExpression(GetValue('DefaultValue'));
  if DataType is TEFStringDataType then
    Result := TKConfig.Instance.MacroExpansionEngine.Expand(EFVarToStr(Result));
end;

function TKModelField.GetDisplayFormat: string;
begin
  Result := GetString('DisplayFormat');
end;

function TKModelField.GetDisplayLabel: string;
begin
  Result := GetString('DisplayLabel');
  if Result = '' then
    Result := BeautifyFieldName(FieldName);
end;

function TKModelField.GetDisplayWidth: Integer;
begin
  Result := GetInteger('DisplayWidth');
  if Result = 0 then
    Result := DataType.GetDefaultDisplayWidth(Size);
end;

function TKModelField.GetEditFormat: string;
begin
  Result := GetString('EditFormat');
end;

function TKModelField.GetEmptyAsNull: Boolean;
var
  LNode: TEFNode;
begin
  if DataType.SupportsEmptyAsNull then
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

function TKModelField.GetField(I: Integer): TKModelField;
begin
  Result := GetFields[I];
end;

function TKModelField.GetFieldCount: Integer;
begin
  Result := GetFields.ChildCount;
end;

function TKModelField.GetFieldName: string;
begin
  Result := Name;
end;

function TKModelField.GetFieldNameOrExpression: string;
begin
  if Expression <> '' then
    Result := Expression
  else
    Result := FieldName;
end;

function TKModelField.GetFieldNames: TStringDynArray;
begin
  Result := GetFields.GetFieldNames;
end;

function TKModelField.GetFields: TKModelFields;
begin
  Result := GetNode('Fields', True) as TKModelFields;
end;

function TKModelField.GetSize: Integer;
var
  LDataType: string;
  LDecimalPrecision: Integer;
  LIsRequired: Boolean;
  LIsKey: Boolean;
  LReferencedModel: string;
  LParentField: TKModelField;
begin
  LParentField := ParentField;
  if Assigned(LParentField) and ParentField.IsReference then
    Result := ParentField.ReferencedModel.KeyFields[Index].Size
  else
    GetFieldSpec(LDataType, Result, LDecimalPrecision, LIsRequired, LIsKey, LReferencedModel);
end;

function TKModelField.GetModel: TKModel;
begin
  Result := GetRoot as TKModel;
end;

function TKModelField.GetParentField: TKModelField;
begin
  if Assigned(Parent) and (Parent is TKModelFields) then
    Result := TKModelFields(Parent).ParentField
  else
    Result := nil;
end;

function TKModelField.GetPhysicalName: string;
begin
  Result := GetString('PhysicalName');
end;

{ TKModelFields }

function TKModelFields.FieldByName(const AName: string): TKModelField;
begin
  Result := FindField(AName);
  if not Assigned(Result) then
    raise EKError.CreateFmt('Field %s not found.', [AName]);
end;

function TKModelFields.FindField(const AName: string): TKModelField;
var
  I: Integer;
begin
  Result := FindChild(AName) as TKModelField;
  if not Assigned(Result) then
  begin
    for I := 0 to FieldCount - 1 do
    begin
      Result := Fields[I].FindField(AName);
      if Assigned(Result) then
        Exit;
    end;
  end;
end;

function TKModelFields.FindFieldByPhysicalName(
  const APhysicalName: string): TKModelField;
var
  I: Integer;
begin
  Result := FindChildByPredicate(
    function (const ANode: TEFNode): Boolean
    begin
      Result := (ANode as TKModelField).PhysicalName = APhysicalName;
    end) as TKModelField;
  if not Assigned(Result) then
  begin
    for I := 0 to FieldCount - 1 do
    begin
      Result := Fields[I].FindFieldByPhysicalName(APhysicalName);
      if Assigned(Result) then
        Exit;
    end;
  end;
end;

function TKModelFields.FindFieldByPredicate(
  const APredicate: TKModelFieldPredicate): TKModelField;
var
  I: Integer;
  LPredicate: TEFTree.TPredicate;
begin
  LPredicate :=
    function (const ANode: TEFNode): Boolean
    begin
      Result := APredicate(ANode as TKModelField);
    end;
  Result := FindChildByPredicate(LPredicate) as TKModelField;
  if not Assigned(Result) then
  begin
    for I := 0 to FieldCount - 1 do
    begin
      Result := Fields[I].FindFieldByPredicate(APredicate);
      if Assigned(Result) then
        Exit;
    end;
  end;
end;

function TKModelFields.GetChildClass(const AName: string): TEFNodeClass;
begin
  Result := TKModelField;
end;

function TKModelFields.GetField(I: Integer): TKModelField;
begin
  Result := Children[I] as TKModelField;
end;

function TKModelFields.GetFieldCount: Integer;
begin
  Result := ChildCount;
end;

function TKModelFields.GetFieldNames: TStringDynArray;
begin
  Result := GetChildNames;
end;

function TKModelFields.GetParentField: TKModelField;
begin
  if Assigned(Parent) and (Parent is TKModelField) then
    Result := TKModelField(Parent)
  else
    Result := nil;
end;

{ TKModelSubobject }

function TKModelSubobject.GetModel: TKModel;
var
  LParent: TEFTree;
begin
  LParent := Parent;
  while Assigned(LParent) and (LParent is TEFNode) do
    LParent := TEFNode(Parent).Parent;

  Result := LParent as TKModel;

  Assert(Assigned(Result));
end;

{ TKModels }

function TKModels.FindModel(const AName: string): TKModel;
begin
  Result := FindObject(AName) as TKModel;
end;

function TKModels.FindModelByPhysicalName(const APhysicalName: string): TKModel;
begin
  Result := FindObjectByPredicate(
    function (const AObject: TKMetadata): Boolean
    begin
      Result := SameText((AObject as TKModel).PhysicalName, APhysicalName);
    end) as TKModel;
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

{ TKRules }

function TKRules.GetChildClass(const AName: string): TEFNodeClass;
begin
  Result := TKRule;
end;

function TKRules.GetRule(I: Integer): TKRule;
begin
  Result := Children[I] as TKRule;
end;

function TKRules.GetRuleCount: Integer;
begin
  Result := ChildCount;
end;

function TKRules.HasRule(const ARule: TKRule): Boolean;
begin
  Result := Assigned(ARule) and Assigned(FindChild(ARule.Name));
end;

{ TKModelDetailReference }

function TKModelDetailReference.BeautifyDetailName(const ADetailName: string): string;
begin
  { TODO : allow to customize the beautifying function }
  Result := ADetailName;
  if (Result = UpperCase(Result)) or (Pos('_', Result) > 0) then
    Result := UpperUnderscoreToCamel(Result);
  Result := CamelToSpaced(Result);
end;

function TKModelDetailReference.GetDBForeignKeyName: string;
begin
  Result := PhysicalName;
end;

function TKModelDetailReference.GetDetailModel: TKModel;
begin
  Result := Model.Catalog.ModelByName(DetailModelName);
end;

function TKModelDetailReference.GetDetailModelName: string;
begin
  Result := AsString;
end;

function TKModelDetailReference.GetDetailReferenceName: string;
begin
  Result := Name;
end;

function TKModelDetailReference.GetDisplayLabel: string;
begin
  Result := GetString('DisplayLabel');
  if (Result = '') and (DetailModel <> nil) then
    Result := DetailModel.DisplayLabel;
  if Result = '' then
    Result := BeautifyDetailName(DetailReferenceName);
end;

function TKModelDetailReference.GetPhysicalName: string;
begin
  Result := GetString('PhysicalName');
end;

function TKModelDetailReference.GetReferenceField: TKModelField;
begin
  Result := DetailModel.FindReferenceField(Model);
  if Result = nil then
  begin
    if ReferenceFieldName <> '' then
      Result := DetailModel.FieldByName(ReferenceFieldName);
    if not Assigned(Result) or not Result.IsReference or not (Result.ReferencedModel = Model) then
      raise EKError.CreateFmt('No reference field in detail model %s to detail reference %s in master model %s.',
        [DetailModel.ModelName, DetailReferenceName, Model.ModelName]);
  end;
end;

function TKModelDetailReference.GetReferenceFieldName: string;
begin
  Result := GetString('ReferenceField');
end;

{ TKModelDetails }

function TKModelDetailReferences.DetailReferenceByName(const AName: string): TKModelDetailReference;
begin
  Result := ChildByName(AName) as TKModelDetailReference;
end;

function TKModelDetailReferences.FindDetailReference(const AName: string): TKModelDetailReference;
begin
  Result := FindChild(AName) as TKModelDetailReference;
end;

function TKModelDetailReferences.FindDetailReferenceByPhysicalName(
  const APhysicalName: string): TKModelDetailReference;
begin
  Result := FindChildByPredicate(
    function (const ANode: TEFNode): Boolean
    begin
      Result := (ANode as TKModelDetailReference).PhysicalName = APhysicalName;
    end) as TKModelDetailReference;
end;

function TKModelDetailReferences.FindDetailReferenceToField(
  const AField: TKModelField): TKModelDetailReference;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to DetailReferenceCount - 1 do
  begin
    if DetailReferences[I].ReferenceField = AField then
    begin
      Result := DetailReferences[I];
      Break;
    end;
  end;
end;

function TKModelDetailReferences.FindDetailReferenceToModel(
  const AModel: TKModel): TKModelDetailReference;
var
  I: Integer;
  LCount: Integer;
begin
  Result := nil;
  LCount := 0;
  for I := 0 to DetailReferenceCount - 1 do
  begin
    if DetailReferences[I].DetailModel = AModel then
    begin
      Result := DetailReferences[I];
      Inc(LCount);
    end;
  end;
  if LCount <> 1 then
    Result := nil;
end;

function TKModelDetailReferences.GetChildClass(const AName: string): TEFNodeClass;
begin
  Result := TKModelDetailReference;
end;

function TKModelDetailReferences.GetDetailReference(I: Integer): TKModelDetailReference;
begin
  Result := Children[I] as TKModelDetailReference;
end;

function TKModelDetailReferences.GetDetailReferenceCount: Integer;
begin
  Result := ChildCount;
end;

{ TKReferenceDataType }

class function TKReferenceDataType.GetTypeName: string;
begin
  Result := 'Reference';
end;

procedure TKReferenceDataType.InternalFieldValueToNode(const AField: TField;
  const ANode: TEFNode);
begin
  ANode.Value := AField.Value;
end;

procedure TKReferenceDataType.InternalYamlValueToNode(const AYamlValue: string;
  const ANode: TEFNode; const AFormatSettings: TFormatSettings);
begin
  raise EEFError.CreateFmt('%s.InternalYamlValueToNode: Unsupported call.', [ClassName]);
end;

{ TKModelList }

procedure TKModelList.AddModelNamesToStrings(const AStrings: TStrings);
var
  LModel: TKModel;
begin
  for LModel in Self do
    AStrings.Add(LModel.ModelName);
end;

initialization
  TEFDataTypeRegistry.Instance.RegisterClass(TKReferenceDataType.GetTypeName, TKReferenceDataType);

finalization
  TEFDataTypeRegistry.Instance.UnregisterClass(TKReferenceDataType.GetTypeName);

end.
