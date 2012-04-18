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

unit Kitto.Metadata.DataView;

{$I Kitto.Defines.inc}

interface

uses
  Types, SysUtils, Generics.Collections,
  EF.Types, EF.Tree,
  Kitto.Metadata, Kitto.Metadata.Models, Kitto.Metadata.Views, Kitto.Store,
  Kitto.Rules;

type
  TKDataView = class;

  TKViewTable = class;

  TKViewTables = class(TKMetadataItem)
  private
    function GetTable: TKViewTable;
    function GetView: TKView;
  protected
    function GetChildClass(const AName: string): TEFNodeClass; override;
  public
    property Table: TKViewTable read GetTable;
    property View: TKView read GetView;
  end;

  TKViewField = class(TKMetadataItem)
  private
    function GetAliasedName: string;
    function GetTable: TKViewTable;
    function GetIsVisible: Boolean;
    function GetModelField: TKModelField;
    function GetDisplayLabel: string;
    function GetDisplayWidth: Integer;
    function GetDataType: TEFDataType;
    function GetIsRequired: Boolean;
    function GetIsReadOnly: Boolean;
    function GetQualifiedDBName: string;
    function GetModelName: string;
    function GetFieldName: string;
    function GetEmptyAsNull: Boolean;
    function GetDefaultValue: Variant;
    function GetModel: TKModel;
    function GetExpression: string;
    function GetAlias: string;
    function GetQualifiedAliasedDBNameOrExpression: string;
    function GetIsKey: Boolean;
    function GetSize: Integer;
    function GetIsBlob: Boolean;
    function GetAllowedValues: TEFPairs;
    function GetRules: TKRules;
    function GetDecimalPrecision: Integer;
    function GetFieldNamesForUpdate: string;
    function GetCanInsert: Boolean;
    function GetCanUpdate: Boolean;
    function GetIsReference: Boolean;
    function GetIsDetailReference: Boolean;
    function GetHint: string;
    function GetEditFormat: string;
    function GetDisplayFormat: string;
    function GetQualifiedDBNameOrExpression: string;
    function GetReferenceField: TKModelField;
    function GetBlankValue: Boolean;
    function GetReferenceName: string;
    function GetDisplayTemplate: string;
    function GetFileNameField: string;
  strict protected
    function GetChildClass(const AName: string): TEFNodeClass; override;
  public
    function FindNode(const APath: string; const ACreateMissingNodes: Boolean = False): TEFNode; override;
    function IsAccessGranted(const AMode: string): Boolean; override;
    function GetResourceURI: string; override;

    property Table: TKViewTable read GetTable;
    property Model: TKModel read GetModel;

    ///	<summary>Returns a reference to the model field, or raises an exception
    ///	if the model field is not found.</summary>
    property ModelField: TKModelField read GetModelField;

    ///	<summary>Returns a reference to the model field, or nil.</summary>
    function FindModelField: TKModelField;
    property Alias: string read GetAlias;
    property AliasedName: string read GetAliasedName;
    property QualifiedAliasedDBNameOrExpression: string read GetQualifiedAliasedDBNameOrExpression;
    property QualifiedDBName: string read GetQualifiedDBName;
    property QualifiedDBNameOrExpression: string read GetQualifiedDBNameOrExpression;
    property AllowedValues: TEFPairs read GetAllowedValues;

    property CanInsert: Boolean read GetCanInsert;
    property CanUpdate: Boolean read GetCanUpdate;

    ///	<summary>
    ///	  <para>If the field is referenced, returns the names of the key fields
    ///	  in the reference, separated by
    ///	  TKConfig.Instance.MultiFieldSeparator.</para>
    ///	  <para>Otherwise, returns the FieldName.</para>
    ///	</summary>
    ///	<exception cref="EKError">The field is referenced and the reference has
    ///	more than one key field.</exception>
    property FieldNamesForUpdate: string read GetFieldNamesForUpdate;

    ///	<summary>Returns True if the field is a reference field, that is if the
    ///	field is part of the containing view table's model and the underlying
    ///	model field is a reference field.</summary>
    property IsReference: Boolean read GetIsReference;

    ///	<summary>Returns the field's DataType, unless it's a reference field,
    ///	in which case returns the data type of the referenced model's
    ///	CaptionField.</summary>
    function GetActualDataType: TEFDataType;

    ///	<summary>If the field is from a different model than the table's model,
    ///	returns the model field that references its model, otherwise returns
    ///	nil.</summary>
    property ReferenceField: TKModelField read GetReferenceField;

    ///	<summary>
    ///	  <para>Returns True if the field is the reference field of
    ///	  MasterTable's model's detail reference to this vie wtable's
    ///	  model.</para>
    ///	  <para>IOW, returns True if the view table is a detail table and the
    ///	  field is (part of) the link to its master table.</para>
    ///	</summary>
    property IsDetailReference: Boolean read GetIsDetailReference;

    ///	<summary>Creates a store with the current field and all key fields of
    ///	the referenced model. If reference = nil, an exception is
    ///	raised.</summary>
    function CreateReferenceStore: TKStore;

    ///	<summary>
    ///	  Extract and returns the model name from the Name. If no model name is
    ///	  specified (because the field is part of the main model), returns the
    ///	  main model name.
    ///	</summary>
    property ModelName: string read GetModelName;

    ///	<summary>
    ///	  Extract and returns the reference name from the Name, or '' if the
    ///	  field is not a reference field.
    ///	</summary>
    property ReferenceName: string read GetReferenceName;

    ///	<summary>
    ///	  Extract and returns the field name without the model name qualifier.
    ///	  If the field is part of the main model, this is equal to Name.
    ///	</summary>
    property FieldName: string read GetFieldName;

    property IsKey: Boolean read GetIsKey;
    property IsVisible: Boolean read GetIsVisible;
    property IsRequired: Boolean read GetIsRequired;
    property IsReadOnly: Boolean read GetIsReadOnly;
    property EmptyAsNull: Boolean read GetEmptyAsNull;
    property DefaultValue: Variant read GetDefaultValue;
    property Expression: string read GetExpression;

    property DisplayLabel: string read GetDisplayLabel;
    property Hint: string read GetHint;
    property DisplayWidth: Integer read GetDisplayWidth;
    property DecimalPrecision: Integer read GetDecimalPrecision;
    property DataType: TEFDataType read GetDataType;
    property Size: Integer read GetSize;
    property IsBlob: Boolean read GetIsBlob;
    property EditFormat: string read GetEditFormat;
    property DisplayFormat: string read GetDisplayFormat;
    property BlankValue: Boolean read GetBlankValue;
    property DisplayTemplate: string read GetDisplayTemplate;

    property Rules: TKRules read GetRules;

    procedure ApplyRules(const AApplyProc: TProc<TKRuleImpl>);

    ///	<summary>If the field is a reference field, creates and returns a list
    ///	of view fields in the current view table from the same referenced
    ///	model.</summary>
    ///	<exception cref="Assert">Violation if IsReference is False.</exception>
    ///	<example>If the field is a reference field called City, then all fields
    ///	called City.* are added to the returned list.</example>
    function GetDerivedFields: TArray<TKViewField>;

    ///	<summary>Creates and loads a store with a record containing all derived
    ///	values for a reference field. The caller is responsible for freeing the
    ///	store object.</summary>
    ///	<param name="AKeyValues">Key values for the record to fetch.</param>
    ///	<exception cref="Assert">If the field is not a reference field, an
    ///	assertion violation is raised.</exception>
    function CreateDerivedFieldsStore(const AKeyValues: string): TKStore;

    ///	<summary>For blob or file reference fields, optionally specifies the
    ///	name of another field in the same view table that will store the
    ///	original file name upon upload.</summary>
    property FileNameField: string read GetFileNameField;
  end;

  TKViewFields = class(TKMetadataItem)
  private
    function GetTable: TKViewTable;
  protected
    function GetChildClass(const AName: string): TEFNodeClass; override;
    function GetField(I: Integer): TKViewField;
    function GetFieldCount: Integer;
  public
    property Table: TKViewTable read GetTable;
    property FieldCount: Integer read GetFieldCount;
    property Fields[I: Integer]: TKViewField read GetField; default;
    function FieldByAliasedName(const AAliasedName: string): TKViewField;
    function FindFieldByAliasedName(const AAliasedName: string): TKViewField;
  end;

  TKViewTableRecord = class;
  TKViewTableRecords = class;

  TKViewTableStore = class(TKStore)
  private
    FMasterRecord: TKViewTableRecord;
    FViewTable: TKViewTable;
    procedure SetupFields;
    function GetRecords: TKViewTableRecords;
  protected
    function GetChildClass(const AName: string): TEFNodeClass; override;
  public
    constructor Create(const AViewTable: TKViewTable); reintroduce;
    property MasterRecord: TKViewTableRecord read FMasterRecord write FMasterRecord;
    property ViewTable: TKViewTable read FViewTable;
    property Records: TKViewTableRecords read GetRecords;

    procedure Load(const AFilter: string; const AOrderBy: string);

    ///	<summary>Loads a page of data according to AFrom and AFor arguments,
    ///	and returns the total number of records in all pages.</summary>
    ///	<param name="AFilter">Additional SQL filter.</param>
    ///	<param name="AFrom">Number of the first record to retrieve
    ///	(0-based).</param>
    ///	<param name="ATo">Maximum count of records to retrieve.</param>
    ///	<remarks>
    ///	  <para>This method will perform two database queries, one to get the
    ///	  total count and one to get the requested data page.</para>
    ///	  <para>If AFrom or ATo are 0, the method calls <see cref=
    ///	  "Load" />.</para>
    ///	</remarks>
    function LoadPage(const AFilter: string; const AOrderBy: string; const AFrom, AFor: Integer): Integer;

    ///	<summary>Appends a record and fills it with the specified
    ///	values.</summary>
    function AppendRecord(const AValues: TEFNode): TKViewTableRecord;

    procedure Save(const AUseTransaction: Boolean);
  end;

  TKViewTableHeader = class(TKHeader)
  protected
    function GetChildClass(const AName: string): TEFNodeClass; override;
  end;

  TKViewTableHeaderField = class(TKHeaderField)
  end;

  TKViewTableField = class(TKField)
  private
    function GetParentRecord: TKViewTableRecord;
    function GetViewField: TKViewField;
  strict protected
    function GetAsJSONValue(const AForDisplay: Boolean): string; override;
  public
    property ParentRecord: TKViewTableRecord read GetParentRecord;
    property ViewField: TKViewField read GetViewField;
  end;

  TKViewTableRecord = class(TKRecord)
  private
    function GetRecords: TKViewTableRecords;
    function GetDetailsStore(I: Integer): TKViewTableStore;
    function GetStore: TKViewTableStore;
    function GetViewTable: TKViewTable;
    function GetField(I: Integer): TKViewTableField;
  protected
    procedure InternalAfterReadFromNode; override;
    function GetChildClass(const AName: string): TEFNodeClass; override;
  public
    property Records: TKViewTableRecords read GetRecords;
    property Store: TKViewTableStore read GetStore;
    property ViewTable: TKViewTable read GetViewTable;
    procedure EnsureDetailStores;
    property DetailStores[I: Integer]: TKViewTableStore read GetDetailsStore;
    function AddDetailStore(const AStore: TKViewTableStore): TKViewTableStore;
    procedure Save(const AUseTransaction: Boolean);
    procedure SetDetailFieldValues(const AMasterRecord: TKViewTableRecord);

    procedure ApplyNewRecordRules;
    procedure ApplyBeforeRules;
    procedure ApplyAfterRules;

    property Fields[I: Integer]: TKViewTableField read GetField; default;
    function FindField(const AFieldName: string): TKViewTableField;
    function FieldByName(const AFieldName: string): TKViewTableField;
  end;

  TKViewTableRecords = class(TKRecords)
  private
    function GetStore: TKViewTableStore;
    function GetRecord(I: Integer): TKViewTableRecord; overload;
  protected
    function GetChildClass(const AName: string): TEFNodeClass; override;
  public
    property Store: TKViewTableStore read GetStore;
    function Append: TKViewTableRecord;
    property Records[I: Integer]: TKViewTableRecord read GetRecord; default;

    function FindRecord(const AValues: TEFNode): TKViewTableRecord;
    function GetRecord(const AValues: TEFNode): TKViewTableRecord; overload;
  end;

  TKViewTable = class(TKMetadataItem)
  private
    function GetIsDetail: Boolean;
    function GetField(I: Integer): TKViewField;
    function GetFieldCount: Integer;
    function GetModelName: string;
    function GetModel: TKModel;
    function GetDetailTableCount: Integer;
    function GetDetailTable(I: Integer): TKViewTable;
    function GetDisplayLabel: string;
    function GetPluralDisplayLabel: string;
    function GetIsReadOnly: Boolean;
    function GetMasterTable: TKViewTable;
    function GetDefaultSorting: string;
    function GetDefaultFilter: string;
    function GetView: TKDataView;
    function GetRules: TKRules;
    function GetImageName: string;
    function GetModelDetailReferenceName: string;
    function GetModelDetailReference: TKModelDetailReference;
  protected
    function GetChildClass(const AName: string): TEFNodeClass; override;
    function GetFields: TKViewFields;
    function GetDetailTables: TKViewTables;
  public
    procedure BeforeSave; override;
    function FindNode(const APath: string;
      const ACreateMissingNodes: Boolean = False): TEFNode; override;

    property ModelName: string read GetModelName;

    property ImageName: string read GetImageName;

    property IsDetail: Boolean read GetIsDetail;
    property MasterTable: TKViewTable read GetMasterTable;

    ///	<summary>If the view table is a detail, this property contains the name
    ///	of the detail reference in the master view table's model. Otherwise
    ///	it's empty.</summary>
    property ModelDetailReferenceName: string read GetModelDetailReferenceName;

    ///	<summary>If the view table is a detail, this property returns the model
    ///	detail reference in the master view table's model. Otherwise raises an
    ///	exception.</summary>
    ///	<remarks>Check IsDetail before calling this method, if you want to
    ///	avoid exceptions.</remarks>
    property ModelDetailReference: TKModelDetailReference read GetModelDetailReference;

    property DisplayLabel: string read GetDisplayLabel;
    property PluralDisplayLabel: string read GetPluralDisplayLabel;

    property Model: TKModel read GetModel;

    property FieldCount: Integer read GetFieldCount;
    property Fields[I: Integer]: TKViewField read GetField;
    function GetFieldNames: TStringDynArray;
    function FindField(const AName: string): TKViewField;
    function FieldByName(const AName: string): TKViewField;
    function FieldByAliasedName(const AName: string): TKViewField;
    function FindFieldByAliasedName(const AAliasedName: string): TKViewField;
    function FindFieldByDBColumnName(const ADBColumnName: string): TKViewField;
    function FieldByDBColumnName(const ADBColumnName: string): TKViewField;
    function GetKeyFieldAliasedNames: TStringDynArray;
    function GetFieldArray(AFilter: TFunc<TKViewField, Boolean>): TArray<TKViewField>;

    function IsFieldVisible(const AField: TKViewField): Boolean;

    property IsReadOnly: Boolean read GetIsReadOnly;

    ///	<summary>
    ///	  Optional fixed filter expression to apply when building the select
    ///	  SQL statement to display data. Should refer to fields through
    ///	  qualified names. Defaults to ''.
    ///	</summary>
    property DefaultFilter: string read GetDefaultFilter;

    ///	<summary>
    ///	  Optional fixed order by expression to apply when building the select
    ///	  SQL statement to display data. Should refer to fields through
    ///	  qualified names (or ordinal numbers for expression-based fields).
    ///   Defaults to Model.DefaultSorting.
    ///	</summary>
    property DefaultSorting: string read GetDefaultSorting;

    property DetailTableCount: Integer read GetDetailTableCount;
    property DetailTables[I: Integer]: TKViewTable read GetDetailTable;
    function DetailTableByName(const AName: string): TKViewTable;
    procedure AddDetailTable(const AViewTable: TKViewTable);

    property View: TKDataView read GetView;

    ///	<summary>Returns an array with any view table's model name
    /// (except the view's MainTable, which is assumed
    ///	implicit) downto and including the current view table's
    ///	name.</summary>
    ///	<example>
    ///	  <para>Called on the Parties view's MainTable yields an empty array</para>
    ///	  <para>Called on its only detail table yields 'Invitation'</para>
    ///	</example>
    function GetNamePath: TStringDynArray;

    ///	<summary>
    ///	  Finds and returns a reference to a layout named after the view's
    ///	  PersistentName plus an underscore ('_') and the specified kind. If no
    ///	  layout exists under that name, returns nil.
    ///	</summary>
    ///	<param name="AKind">
    ///	  Kind of layout to look for. Common kinds are 'List' and 'Form'.
    ///	</param>
    function FindLayout(const AKind: string): TKLayout;

    ///	<summary>
    ///	  Creates and returns a store with the view's metadata.
    ///	</summary>
    function CreateStore: TKViewTableStore;

    ///	<summary>Creates and returns a node with one child for each default
    ///	value as specified in the view table or model. Any default expression
    ///	is evaluated at this time.</summary>
    ///	<remarks>The caller is responsible for freeing the returned node
    ///	object.</remarks>
    function GetDefaultValues: TEFNode;

    function GetResourceURI: string; override;

    function IsAccessGranted(const AMode: string): Boolean; override;

    property Rules: TKRules read GetRules;
    procedure ApplyRules(const AApplyProc: TProc<TKRuleImpl>);

    ///	<summary>If the specified field exists in the view table, the method
    ///	returns its AliasedName, otherwise the specified field name is
    ///	returned.</summary>
    function ApplyFieldAliasedName(const AFieldName: string): string;
  end;

  TKDataView = class(TKView)
  private
    function GetMainTable: TKViewTable;
  protected
    function GetChildClass(const AName: string): TEFNodeClass; override;
    function GetDisplayLabel: string; override;
    function GetImageName: string; override;
  public
    property MainTable: TKViewTable read GetMainTable;
  end;

  TKFileReferenceDataType = class(TEFStringDataType)
  public
    class function GetTypeName: string; override;
  end;

implementation

uses
  StrUtils, Variants, TypInfo,
  EF.DB, EF.StrUtils, EF.VariantUtils, EF.Macros,
  Kitto.SQL, Kitto.Types, Kitto.Config, Kitto.AccessControl;

{ TKDataView }

function TKDataView.GetChildClass(const AName: string): TEFNodeClass;
begin
  if SameText(AName, 'MainTable') then
    Result := TKViewTable
  else
    Result := inherited GetChildClass(AName);
end;

function TKDataView.GetDisplayLabel: string;
begin
  Result := inherited GetDisplayLabel;
  if Result = '' then
    Result := MainTable.PluralDisplayLabel;
end;

function TKDataView.GetImageName: string;
begin
  Result := inherited GetImageName;
  if Result = DEFAULT_IMAGE_NAME then
    Result := MainTable.ImageName;
end;

function TKDataView.GetMainTable: TKViewTable;
begin
  Result := GetNode('MainTable', True) as TKViewTable;
end;

{ TKViewTable }

procedure TKViewTable.AddDetailTable(const AViewTable: TKViewTable);
begin
  Assert(Assigned(AViewTable));

  GetDetailTables.AddChild(AViewTable);
end;

function TKViewTable.ApplyFieldAliasedName(const AFieldName: string): string;
var
  LViewField: TKViewField;
begin
  LViewField := FindField(AFieldName);
  if Assigned(LViewField) then
    Result := LViewField.AliasedName
  else
    Result := AFieldName;
end;

procedure TKViewTable.ApplyRules(const AApplyProc: TProc<TKRuleImpl>);
var
  I: Integer;
  LRuleImpl: TKRuleImpl;
  LRule: TKRule;
begin
  Assert(Assigned(AApplyProc));

  // Apply rules at the View level.
  for I := 0 to Rules.RuleCount - 1 do
  begin
    LRule := Rules[I];
    LRuleImpl := TKRuleImplFactory.Instance.CreateObject(LRule.Name);
    try
      LRuleImpl.Rule := LRule;
      AApplyProc(LRuleImpl);
    finally
      FreeAndNil(LRuleImpl);
    end;
  end;
  // Always apply rules at the model level as well. View-level record rules
  // augment model-level rules but cannot overwrite or disable them.
  for I := 0 to Model.Rules.RuleCount - 1 do
  begin
    LRule := Model.Rules[I];
    if not Rules.HasRule(LRule) then
    begin
      LRuleImpl := TKRuleImplFactory.Instance.CreateObject(LRule.Name);
      try
        LRuleImpl.Rule := LRule;
        AApplyProc(LRuleImpl);
      finally
        FreeAndNil(LRuleImpl);
      end;
    end;
  end;
end;

procedure TKViewTable.BeforeSave;
begin
  inherited;
  if DetailTableCount = 0 then
    DeleteNode('DetailTables');
end;

function TKViewTable.CreateStore: TKViewTableStore;
begin
  Result := TKViewTableStore.Create(Self);
end;

function TKViewTable.DetailTableByName(const AName: string): TKViewTable;
begin
  Result := GetDetailTables.ChildByName(AName) as TKViewTable;
end;

function TKViewTable.FieldByAliasedName(
  const AName: string): TKViewField;
begin
  Result := GetFields.FieldByAliasedName(AName) as TKViewField;
end;

function TKViewTable.FieldByName(const AName: string): TKViewField;
begin
  Result := GetFields.ChildByName(AName) as TKViewField;
end;

function TKViewTable.FieldByDBColumnName(
  const ADBColumnName: string): TKViewField;
begin
  Result := FindFieldByDBColumnName(ADBColumnName);
  if not Assigned(Result) then
    raise EKError.CreateFmt('Couldn''t find a field with DB column name %s.',
      [ADBColumnName]);
end;

function TKViewTable.FindFieldByDBColumnName(const ADBColumnName: string): TKViewField;
begin
  Result := GetFields.FindChildByPredicate(
    function(const ANode: TEFNode): Boolean
    begin
      Result := SameText(TKViewField(ANode).ModelField.DBColumnName, ADBColumnName);
    end) as TKViewField;
end;

function TKViewTable.FindField(const AName: string): TKViewField;
begin
  Result := GetFields.FindChild(AName) as TKViewField;
end;

function TKViewTable.FindFieldByAliasedName(const AAliasedName: string): TKViewField;
begin
  Result := GetFields.FindFieldByAliasedName(AAliasedName);
end;

function TKViewTable.GetChildClass(const AName: string): TEFNodeClass;
begin
  if SameText(AName, 'Fields') then
    Result := TKViewFields
  else if SameText(AName, 'DetailTables') then
    Result := TKViewTables
  else if SameText(AName, 'Rules') then
    Result := TKRules
  else
    Result := inherited GetChildClass(AName);
end;

function TKViewTable.GetModel: TKModel;
begin
  Result := View.Catalog.Models.FindModel(ModelName);
end;

function TKViewTable.GetModelDetailReference: TKModelDetailReference;
begin
  Result := nil;
  if Assigned(MasterTable) then
  begin
    Result := MasterTable.Model.FindDetailReferenceByModel(Model);
    if not Assigned(Result) and (ModelDetailReferenceName <> '') then
      Result := MasterTable.Model.FindDetailReference(ModelDetailReferenceName);
  end;
  if not Assigned(Result) then
    raise EKError.CreateFmt('Couldn''t find detail reference from %s to %s.',
      [MasterTable.ModelName, ModelName]);
end;

function TKViewTable.GetModelDetailReferenceName: string;
begin
  Result := GetString('DetailReference');
end;

function TKViewTable.GetDefaultSorting: string;
begin
  Result := GetString('DefaultSorting');
  if Result = '' then
    Result := Model.DefaultSorting;
end;

function TKViewTable.GetDefaultValues: TEFNode;
var
  I: Integer;
  LValue: Variant;
begin
  Result := TEFNode.Create;
  try
    for I := 0 to FieldCount - 1 do
    begin
      LValue := Fields[I].DefaultValue;
      if not VarIsNull(LValue) and (EFVarToStr(LValue) <> '') then
        Result.AddChild(Fields[I].FieldName, LValue);
    end;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TKViewTable.GetDefaultFilter: string;
begin
  Result := GetString('DefaultFilter');
  if Result = '' then
    Result := Model.DefaultFilter;
end;

function TKViewTable.GetDetailTableCount: Integer;
begin
  Result := GetDetailTables.GetChildCount<TKViewTable>;
end;

function TKViewTable.GetDetailTables: TKViewTables;
begin
  Result := GetNode('DetailTables', True) as TKViewTables;
end;

function TKViewTable.GetDisplayLabel: string;
begin
  Result := GetString('DisplayLabel');
  if Result = '' then
  begin
    if IsDetail then
      Result := ModelDetailReference.DisplayLabel
    else
      Result := Model.DisplayLabel;
  end;
end;

function TKViewTable.GetField(I: Integer): TKViewField;
begin
  Result := GetFields[I];
end;

function TKViewTable.GetFieldArray(AFilter: TFunc<TKViewField, Boolean>): TArray<TKViewField>;
var
  LItems: Integer;
  I: Integer;
begin
  Setlength(Result, FieldCount);
  LItems := 0;
  for I := 0 to FieldCount - 1 do
  begin
    if AFilter(Fields[I]) then
    begin
      Result[LItems] := Fields[I];
      Inc(LItems);
    end;
  end;
  SetLength(Result, LItems);
end;

function TKViewTable.GetFieldCount: Integer;
begin
  Result := GetFields.FieldCount;
end;

function TKViewTable.GetFieldNames: TStringDynArray;
var
  I: Integer;
begin
  SetLength(Result, FieldCount);
  for I := 0 to High(Result) do
    Result[I] := Fields[I].FieldName;

  if Length(Result) = 0 then
  begin
    SetLength(Result, Model.FieldCount);
    for I := 0 to High(Result) do
      Result[I] := Model.Fields[I].FieldName;
  end;
end;

function TKViewTable.GetFields: TKViewFields;

  procedure CreateDefaultFields;
  var
    I: Integer;
  begin
    for I := 0 to Model.FieldCount - 1 do
      GetNode('Fields').AddChild(TKViewField.Create(Model.Fields[I].FieldName));
  end;

begin
  Result := GetNode('Fields', True) as TKViewFields;
  if Result.FieldCount = 0 then
    CreateDefaultFields;
end;

function TKViewTable.GetImageName: string;
begin
  Result := GetString('ImageName');
  if Result = '' then
    Result := Model.ImageName;
end;

function TKViewTable.GetIsDetail: Boolean;
begin
  // MainTable has the view as parent, other tables have the collection.
  Result := Parent is TKViewTables;
end;

function TKViewTable.GetIsReadOnly: Boolean;
var
  LNode: TEFNode;
begin
  LNode := FindNode('IsReadOnly');
  if Assigned(LNode) then
    Result := LNode.AsBoolean
  else
    Result := Model.IsReadOnly;
end;

function TKViewTable.GetMasterTable: TKViewTable;
begin
  if Parent is TKViewTables then
    Result := TKViewTables(Parent).Table
  else
    Result := nil;
end;

function TKViewTable.GetPluralDisplayLabel: string;
begin
  Result := GetString('PluralDisplayLabel');
  if Result = ''then
    Result := Model.PluralDisplayLabel;
end;

function TKViewTable.GetResourceURI: string;
begin
  Result := View.GetResourceURI;
  if Result <> '' then
    Result := Result + Join(GetNamePath, '/');
end;

function TKViewTable.GetRules: TKRules;
begin
  Result := GetNode('Rules', True) as TKRules;
end;

function TKViewTable.GetKeyFieldAliasedNames: TStringDynArray;
var
  I: Integer;
  LViewField: TKViewField;
begin
  Result := Model.GetKeyFieldNames;
  // Apply aliasing.
  for I := Low(Result) to High(Result) do
  begin
    LViewField := FindField(Result[I]);
    if Assigned(LViewField) then
      Result[I] := LViewField.AliasedName;
  end;
end;

function TKViewTable.FindLayout(const AKind: string): TKLayout;

  function GetViewTablePathName: string;
  var
    LMasterTable: TKViewTable;
  begin
    if MasterTable = nil then
      Result := View.PersistentName
    else
    begin
      Result := Model.ModelName;
      LMasterTable := MasterTable;
      while Assigned(LMasterTable) do
      begin
        Result := LMasterTable.Model.ModelName + '.' + Result;
        LMasterTable := LMasterTable.MasterTable;
      end;
      Result := StripSuffix(View.PersistentName + '.' + Result, '.');
    end;
  end;

begin
  Result := View.Catalog.Layouts.FindLayout(GetViewTablePathName + '_' + AKind);
end;

function TKViewTable.FindNode(const APath: string;
  const ACreateMissingNodes: Boolean): TEFNode;
begin
  Result := inherited FindNode(APath, ACreateMissingNodes);
  if not Assigned(Result) then
    // ACreateMissingNodes is False here.
    Result := Model.FindNode(APath, False);
end;

function TKViewTable.GetDetailTable(I: Integer): TKViewTable;
begin
  Result := GetDetailTables.GetChild<TKViewTable>(I);
end;

function TKViewTable.GetModelName: string;
begin
  Result := GetNode('Model', True).AsString;
end;

function TKViewTable.GetNamePath: TStringDynArray;
begin
  if MasterTable <> nil then
  begin
    Result := MasterTable.GetNamePath;
    SetLength(Result, Length(Result) + 1);
    Result[High(Result)] := ModelName;
  end
  else
    SetLength(Result, 0);
end;

function TKViewTable.GetView: TKDataView;
begin
  if Parent is TKDataView then
    Result := TKDataView(Parent)
  else if MasterTable <> nil then
    Result := MasterTable.View
  else if Parent is TKViewTables then
    Result := TKViewTables(Parent).View as TKDataView
  else
    raise EKError.Create('Structure error. View not found for TKViewTable.');
end;

function TKViewTable.IsAccessGranted(const AMode: string): Boolean;
begin
  Result := TKConfig.Instance.IsAccessGranted(GetResourceURI, AMode)
    // A dataview and its main table currently share the same resource URI,
    // so it's useless to test it twice.
    //and TKConfig.Instance.IsAccessGranted(View.GetResourceURI, AMode)
    and TKConfig.Instance.IsAccessGranted(Model.GetResourceURI, AMode);
end;

function TKViewTable.IsFieldVisible(const AField: TKViewField): Boolean;
begin
  Assert(Assigned(AField));

  Result := AField.IsVisible
    or MatchText(AField.AliasedName, GetStringArray('Controller/VisibleFields'));
end;

{ TKViewTables }

function TKViewTables.GetChildClass(const AName: string): TEFNodeClass;
begin
  if SameText(AName, 'Table') then
    Result := TKViewTable
  else
    Result := inherited GetChildClass(AName);
end;

function TKViewTables.GetTable: TKViewTable;
begin
  Result := Parent as TKViewTable;
end;

function TKViewTables.GetView: TKView;
begin
  if Parent is TKViewTable then
    Result := TKViewTable(Parent).View
  else if Parent is TKView then
    Result := TKView(Parent)
  else
    raise EKError.Create('Structure error. View not found for TKViewTables.');
end;

{ TKViewFields }

function TKViewFields.FieldByAliasedName(
  const AAliasedName: string): TKViewField;
begin
  Result := FindFieldByAliasedName(AAliasedName);
  if not Assigned(Result) then
    raise EKError.CreateFmt('ViewField %s not found.', [AAliasedName]);
end;

function TKViewFields.FindFieldByAliasedName(
  const AAliasedName: string): TKViewField;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FieldCount - 1 do
  begin
    if SameText(Fields[I].AliasedName, AAliasedName) then
    begin
      Result := Fields[I];
      Break;
    end;
  end;
end;

function TKViewFields.GetChildClass(const AName: string): TEFNodeClass;
begin
  Result := TKViewField;
end;

function TKViewFields.GetField(I: Integer): TKViewField;
begin
  Result := Children[I] as TKViewField;
end;

function TKViewFields.GetFieldCount: Integer;
begin
  Result := ChildCount;
end;

function TKViewFields.GetTable: TKViewTable;
begin
  Result := Parent as TKViewTable;
end;

{ TKViewField }

procedure TKViewField.ApplyRules(const AApplyProc: TProc<TKRuleImpl>);
var
  I: Integer;
  LRuleImpl: TKRuleImpl;
  LRule: TKRule;
begin
  Assert(Assigned(AApplyProc));

  // Apply rules at the View level.
  for I := 0 to Rules.RuleCount - 1 do
  begin
    LRule := Rules[I];
    LRuleImpl := TKRuleImplFactory.Instance.CreateObject(LRule.Name);
    try
      LRuleImpl.Rule := LRule;
      AApplyProc(LRuleImpl);
    finally
      FreeAndNil(LRuleImpl);
    end;
  end;
  // Apply rules at the model level that are not overwritten in the view.
  // A field-level rule of given type (such as Maxlength) generally cannot
  // be applied twice, and we don't have a way yet to avoid setting config
  // options twice on a rule-by-rule basis.
  for I := 0 to ModelField.Rules.RuleCount - 1 do
  begin
    LRule := ModelField.Rules[I];
    if not Rules.HasRule(LRule) then
    begin
      LRuleImpl := TKRuleImplFactory.Instance.CreateObject(LRule.Name);
      try
        LRuleImpl.Rule := LRule;
        AApplyProc(LRuleImpl);
      finally
        FreeAndNil(LRuleImpl);
      end;
    end;
  end;
end;

function TKViewField.CreateDerivedFieldsStore(
  const AKeyValues: string): TKStore;
var
  LDerivedFields: TArray<TKViewField>;
  LDerivedField: TKViewField;
  LDBQuery: TEFDBQuery;
begin
  Assert(IsReference);

  Result := TKStore.Create;
  try
    // Set header.
    LDerivedFields := GetDerivedFields;
    if Length(LDerivedFields) > 0 then
      for LDerivedField in LDerivedFields do
        Result.Header.AddChild(LDerivedField.AliasedName).DataType := LDerivedField.DataType;
    // Get data.
    LDBQuery := TKConfig.Instance.DefaultDBConnection.CreateDBQuery;
    try
      TKSQLBuilder.BuildDerivedSelectQuery(Self, LDBQuery, AKeyValues);
      Result.Load(LDBQuery);
    finally
      FreeAndNil(LDBQuery);
    end;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TKViewField.CreateReferenceStore: TKStore;
var
  I: Integer;
  LField: TKModelField;
  LCaptionField: TKModelField;
begin
  Assert(IsReference);

  Result := TKStore.Create;
  try
    for I := 0 to ModelField.ReferencedModel.KeyFieldCount - 1 do
    begin
      LField := ModelField.ReferencedModel.KeyFields[I];
      Result.Key.AddChild(LField.FieldName).DataType := LField.DataType;
      Result.Header.AddChild(LField.FieldName).DataType := LField.DataType;
    end;
    LCaptionField := ModelField.ReferencedModel.CaptionField;
    if Result.Header.FindChild(LCaptionField.FieldName) = nil then
      Result.Header.AddChild(LCaptionField.FieldName).DataType := LCaptionField.DataType;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TKViewField.FindModelField: TKModelField;
begin
  Result := Model.FindField(FieldName);
end;

function TKViewField.FindNode(const APath: string;
  const ACreateMissingNodes: Boolean): TEFNode;
begin
  Result := inherited FindNode(APath, ACreateMissingNodes);
  if not Assigned(Result) then
    // ACreateMissingNodes is False here.
    Result := ModelField.FindNode(APath, False);
end;

function TKViewField.GetActualDataType: TEFDataType;
begin
  Result := DataType;
  if Result is TKReferenceDataType then
    Result := ModelField.ReferencedModel.CaptionField.DataType;
end;

function TKViewField.GetAlias: string;
begin
  Result := AsString;
end;

function TKViewField.GetAliasedName: string;
begin
  Result := Alias;
  if Result = '' then
    Result := Name;
  if (Result = '') or (Pos('.', Result) > 0) then
    raise EKError.CreateFmt('ViewField %s must have an alias.', [Name]);
end;

function TKViewField.GetAllowedValues: TEFPairs;
begin
  Result := GetChildrenAsPairs('AllowedValues');
  if Result = nil then
    Result := ModelField.AllowedValues;
end;

function TKViewField.GetBlankValue: Boolean;
var
  LNode: TEFNode;
begin
  LNode := FindNode('BlankValue');
  if LNode = nil then
    Result := ModelField.GetBoolean('BlankValue')
  else
    Result := LNode.AsBoolean;
end;

function TKViewField.GetCanInsert: Boolean;
var
  LNode: TEFNode;
begin
  LNode := FindNode('CanInsert');
  if LNode = nil then
    Result := ModelField.CanInsert
  else
    Result := LNode.AsBoolean;
end;

function TKViewField.GetCanUpdate: Boolean;
var
  LNode: TEFNode;
begin
  LNode := FindNode('CanUpdate');
  if LNode = nil then
    Result := ModelField.CanUpdate
  else
    Result := LNode.AsBoolean;
end;

function TKViewField.GetChildClass(const AName: string): TEFNodeClass;
begin
  if SameText(AName, 'Rules') then
    Result := TKRules
  else
    Result := inherited GetChildClass(AName);
end;

function TKViewField.GetQualifiedAliasedDBNameOrExpression: string;
var
  LExpression: string;
begin
  if Name = '' then
    raise EKError.Create('Missing field name.');
  LExpression := Expression;
  if LExpression <> '' then
    Result := LExpression + ' ' + FieldName
  else
  begin
    Result := QualifiedDBName;
    if Alias <> '' then
      Result := Result + ' ' + Alias;
  end;
end;

function TKViewField.GetQualifiedDBNameOrExpression: string;
begin
  if IsReference then
    Result := FieldName + '.' + ModelField.ReferencedModel.CaptionField.FieldNameOrExpression
  else if Expression <> '' then
    Result := Expression
  else
    Result := QualifiedDBName;
end;

function TKViewField.GetDataType: TEFDataType;
var
  LDataTypeName: string;
begin
  LDataTypeName := GetString('DataType');
  if LDataTypeName = '' then
    Result := ModelField.DataType
  else
    Result := TEFDataTypeFactory.Instance.GetDataType(LDataTypeName);
end;

function TKViewField.GetModelField: TKModelField;
begin
  Result := Model.FieldByName(FieldName);
end;

function TKViewField.GetModel: TKModel;
begin
  Result := Table.Model.Catalog.ModelByName(ModelName);
end;

function TKViewField.GetDecimalPrecision: Integer;
begin
  Result := GetInteger('DecimalPrecision');
  if Result = 0 then
    Result := ModelField.DecimalPrecision;
end;

function TKViewField.GetDefaultValue: Variant;
begin
  Result := EvalExpression(GetValue('DefaultValue'));
  if VarIsNull(Result) then
    Result := ModelField.DefaultValue;
  if DataType is TEFStringDataType then
    Result := TEFMacroExpansionEngine.Instance.Expand(Result);
end;

function TKViewField.GetDerivedFields: TArray<TKViewField>;
begin
  Assert(IsReference);

  Result := Table.GetFieldArray(
    function (AField: TKViewField): Boolean
    begin
      Result := AField.ReferenceName = FieldName;
    end
  );
end;

function TKViewField.GetDisplayFormat: string;
begin
  Result := GetString('DisplayFormat');
  if Result = '' then
    Result := ModelField.DisplayFormat;
  if Result = '' then
    Result := EditFormat;
end;

function TKViewField.GetDisplayLabel: string;
begin
  Result := GetString('DisplayLabel');
  if Result = '' then
    Result := ModelField.DisplayLabel;
end;

function TKViewField.GetDisplayTemplate: string;
var
  LNode: TEFNode;
begin
  LNode := FindNode('DisplayTemplate');
  if LNode = nil then
    Result := ModelField.GetString('DisplayTemplate')
  else
    Result := LNode.AsString;
end;

function TKViewField.GetDisplayWidth: Integer;
begin
  Result := GetInteger('DisplayWidth');
  if Result = 0 then
    Result := ModelField.DisplayWidth;
end;

function TKViewField.GetEditFormat: string;
begin
  Result := GetString('EditFormat');
  if Result = '' then
    Result := ModelField.EditFormat;
end;

function TKViewField.GetEmptyAsNull: Boolean;
var
  LNode: TEFNode;
begin
  LNode := FindNode('EmptyAsNull');
  if LNode = nil then
    Result := ModelField.EmptyAsNull
  else
    Result := LNode.AsBoolean;
end;

function TKViewField.GetExpression: string;
begin
  Result := GetString('Expression');
end;

function TKViewField.GetFieldName: string;
var
  LNameParts: TStringDynArray;
begin
  LNameParts := Split(Name, '.');
  if Length(LNameParts) = 1 then
    Result := Name
  else if Length(LNameParts) = 2 then
    Result := LNameParts[1]
  else
    raise EKError.CreateFmt('Couldn''t determine field name for field %s.', [Name]);
end;

function TKViewField.GetFieldNamesForUpdate: string;
begin
  if IsReference then
    Result := Join(ModelField.GetFieldNames, TKConfig.Instance.MultiFieldSeparator)
  else
    Result := FieldName;
end;

function TKViewField.GetFileNameField: string;
begin
  Result := GetString('FileNameField');
  if Result = '' then
    Result := ModelField.FileNameField;
end;

function TKViewField.GetHint: string;
begin
  Result := GetString('Hint');
  if Result = '' then
    Result := ModelField.Hint;
end;

function TKViewField.GetIsVisible: Boolean;
begin
  Result := GetBoolean('IsVisible', True);
end;

function TKViewField.GetQualifiedDBName: string;
begin
  if Pos('.', Name) > 0 then
    Result := Name
  else
    Result := Model.DBTableName + '.' + ModelField.DBColumnName;
end;

function TKViewField.GetSize: Integer;
begin
  Result := GetInteger('Size');
  if Result = 0 then
    Result := ModelField.Size;
end;

function TKViewField.GetIsBlob: Boolean;
begin
  Result := DataType.IsBlob(Size);
end;

function TKViewField.GetIsDetailReference: Boolean;
begin
  Result := Table.IsDetail and (Table.ModelDetailReference.ReferenceField = ModelField);
end;

function TKViewField.GetIsKey: Boolean;
begin
  Result := (ModelName = Table.ModelName) and ModelField.IsKey;
end;

function TKViewField.GetIsReadOnly: Boolean;
begin
  Result := GetBoolean('IsReadOnly');
end;

function TKViewField.GetIsReference: Boolean;
begin
  Result := (Table.Model = Model) and ModelField.IsReference;
end;

function TKViewField.GetIsRequired: Boolean;
var
  LNode: TEFNode;
begin
  LNode := FindNode('IsRequired');
  if LNode = nil then
    Result := ModelField.IsRequired
  else
    Result := LNode.AsBoolean or ModelField.IsRequired;
end;

function TKViewField.GetTable: TKViewTable;
begin
  Result := (Parent as TKViewFields).Table;
end;

function TKViewField.IsAccessGranted(const AMode: string): Boolean;
begin
  Result := TKConfig.Instance.IsAccessGranted(GetResourceURI, AMode)
    and TKConfig.Instance.IsAccessGranted(ModelField.GetResourceURI, AMode);
end;

function TKViewField.GetReferenceName: string;
var
  LNameParts: TStringDynArray;
begin
  LNameParts := Split(Name, '.');
  if Length(LNameParts) = 1 then
    // <field name>
    Result := ''
  else if Length(LNameParts) = 2 then
    // <reference name>.<field name>
    Result := LNameParts[0]
  else
    raise EKError.CreateFmt('Couldn''t determine referenced model name for field %s.', [Name]);
end;

function TKViewField.GetResourceURI: string;
begin
  Result := Table.GetResourceURI;
  if Result <> '' then
    Result := Result + '/' + AliasedName;
end;

function TKViewField.GetModelName: string;
var
  LNameParts: TStringDynArray;
begin
  LNameParts := Split(Name, '.');
  if Length(LNameParts) = 1 then
    // <field name>
    Result := Table.ModelName
  else if Length(LNameParts) = 2 then
    // <reference name>.<field name>
    Result := Table.Model.FieldByName(LNameParts[0]).ReferencedModel.ModelName
  else
    raise EKError.CreateFmt('Couldn''t determine model name for field %s.', [Name]);
end;

function TKViewField.GetReferenceField: TKModelField;
begin
  if Model <> Table.Model then
    Result := Table.Model.FindReferenceField(Model)
  else
    Result := nil;
end;

function TKViewField.GetRules: TKRules;
begin
  Result := GetNode('Rules', True) as TKRules;
end;

{ TKViewTableStore }

function TKViewTableStore.AppendRecord(
  const AValues: TEFNode): TKViewTableRecord;
begin
  Result := inherited AppendRecord(AValues) as TKViewTableRecord;
  // The above will cause InternalAfterReadFromNode to be called.
  // InternalAfterReadFromNode will call SetDetailFieldValues itself.
  //if Assigned(FMasterRecord) then
  //  Result.SetDetailFieldValues(FMasterRecord);
end;

constructor TKViewTableStore.Create(const AViewTable: TKViewTable);
begin
  Assert(Assigned(AViewTable));

  inherited Create;
  FViewTable := AViewTable;
  SetupFields;
end;

procedure TKViewTableStore.Save(const AUseTransaction: Boolean);
var
  I: Integer;
begin
  if AUseTransaction then
    TKConfig.Instance.DefaultDBConnection.StartTransaction;
  try
    for I := 0 to RecordCount - 1 do
      Records[I].Save(False);
    if AUseTransaction then
      TKConfig.Instance.DefaultDBConnection.CommitTransaction;
  except
    if AUseTransaction then
      TKConfig.Instance.DefaultDBConnection.RollbackTransaction;
    raise;
  end;
end;

procedure TKViewTableStore.SetupFields;
var
  LViewFieldIndex: Integer;
  LModelFieldIndex: Integer;
  LViewField: TKViewField;
  LModelField: TKModelField;

  procedure SetupField(const AName: string; const ADataType: TEFDataType;
    const AIsKey, AIsAccessGranted: Boolean);
  begin
    if AIsAccessGranted or AIsKey then
    begin
      // Set field names and data types both in key and header.
      if AIsKey then
        Key.AddChild(AName).DataType := ADataType;
      Header.AddChild(AName).DataType := ADataType;
    end;
  end;

begin
  for LViewFieldIndex := 0 to FViewTable.FieldCount - 1 do
  begin
    LViewField := FViewTable.Fields[LViewFieldIndex];
    // Expand reference fields.
    if LViewField.IsReference then
    begin
      for LModelFieldIndex := 0 to LViewField.ModelField.FieldCount - 1 do
      begin
        LModelField := LViewField.ModelField.Fields[LModelFieldIndex];
        SetupField(LModelField.FieldName, LModelField.DataType, LModelField.IsKey, LModelField.IsAccessGranted(ACM_READ));
      end;
    end;
    SetupField(LViewField.AliasedName, LViewField.DataType, LViewField.IsKey, LViewField.IsAccessGranted(ACM_READ));
  end;
end;

function TKViewTableStore.GetChildClass(const AName: string): TEFNodeClass;
begin
  if SameText(AName, 'Header') then
    Result := TKViewTableHeader
  else if SameText(AName, 'Records') then
    Result := TKViewTableRecords
  else
    Result := inherited GetChildClass(AName);
end;

function TKViewTableStore.GetRecords: TKViewTableRecords;
begin
  Result := inherited Records as TKViewTableRecords;
end;

procedure TKViewTableStore.Load(const AFilter: string; const AOrderBy: string);
var
  LDBQuery: TEFDBQuery;
begin
  Assert(Assigned(FViewTable));

  LDBQuery := TKConfig.Instance.DefaultDBConnection.CreateDBQuery;
  try
    TKSQLBuilder.BuildSelectQuery(FViewTable, AFilter, AOrderBy, LDBQuery, FMasterRecord);
    inherited Load(LDBQuery);
  finally
    FreeAndNil(LDBQuery);
  end;
end;

function TKViewTableStore.LoadPage(const AFilter: string; const AOrderBy: string;
  const AFrom, AFor: Integer): Integer;
var
  LDBQuery: TEFDBQuery;
begin
  if (AFrom = 0) and (AFor = 0) then
  begin
    Load(AFilter, AOrderBy);
    Result := RecordCount;
  end
  else
  begin
    LDBQuery := TKConfig.Instance.DefaultDBConnection.CreateDBQuery;
    try
      TKSQLBuilder.BuildCountQuery(FViewTable, AFilter, LDBQuery, FMasterRecord);
      LDBQuery.Open;
      try
        Result := LDBQuery.DataSet.Fields[0].AsInteger;
      finally
        LDBQuery.Close;
      end;
      TKSQLBuilder.BuildSelectQuery(FViewTable, AFilter, AOrderBy, LDBQuery, FMasterRecord, AFrom, AFor);
      inherited Load(LDBQuery);
    finally
      FreeAndNil(LDBQuery);
    end;
  end;
end;

{ TKViewTableHeader }

function TKViewTableHeader.GetChildClass(const AName: string): TEFNodeClass;
begin
  Result := TKViewTableHeaderField;
end;

{ TKViewTableRecords }

function TKViewTableRecords.Append: TKViewTableRecord;
begin
  Result := inherited Append as TKViewTableRecord;
end;

function TKViewTableRecords.FindRecord(const AValues: TEFNode): TKViewTableRecord;
begin
  Result := inherited FindRecord(AValues) as TKViewTableRecord;
end;

function TKViewTableRecords.GetChildClass(const AName: string): TEFNodeClass;
begin
  Result := TKViewTableRecord;
end;

function TKViewTableRecords.GetRecord(
  const AValues: TEFNode): TKViewTableRecord;
begin
  Result := inherited GetRecord(AValues) as TKViewTableRecord;
end;

function TKViewTableRecords.GetRecord(I: Integer): TKViewTableRecord;
begin
  Result := inherited Records[I] as TKViewTableRecord;
end;

function TKViewTableRecords.GetStore: TKViewTableStore;
begin
  Result := inherited Store as TKViewTableStore;
end;

{ TKViewTableRecord }

function TKViewTableRecord.AddDetailStore(const AStore: TKViewTableStore): TKViewTableStore;
begin
  Result := inherited AddDetailStore(AStore) as TKViewTableStore;
end;

procedure TKViewTableRecord.ApplyAfterRules;
begin
  Backup;
  try
    case State of
      rsNew: ViewTable.ApplyRules(
        procedure (ARuleImpl: TKRuleImpl)
        begin
          ARuleImpl.AfterAdd(Self);
        end);
      rsDirty: ViewTable.ApplyRules(
        procedure (ARuleImpl: TKRuleImpl)
        begin
          ARuleImpl.AfterUpdate(Self);
        end);
      rsDeleted: ViewTable.ApplyRules(
        procedure (ARuleImpl: TKRuleImpl)
        begin
          ARuleImpl.AfterDelete(Self);
        end);
    end;
  except
    Restore;
    raise;
  end;
end;

procedure TKViewTableRecord.ApplyBeforeRules;
begin
  Backup;
  try
    case State of
      rsNew: ViewTable.ApplyRules(
        procedure (ARuleImpl: TKRuleImpl)
        begin
          ARuleImpl.BeforeAdd(Self);
        end);
      rsDirty: ViewTable.ApplyRules(
        procedure (ARuleImpl: TKRuleImpl)
        begin
          ARuleImpl.BeforeUpdate(Self);
        end);
      rsDeleted: ViewTable.ApplyRules(
        procedure (ARuleImpl: TKRuleImpl)
        begin
          ARuleImpl.BeforeDelete(Self);
        end);
    end;
  except
    Restore;
    raise;
  end;
end;

procedure TKViewTableRecord.ApplyNewRecordRules;
begin
  ViewTable.ApplyRules(
    procedure (ARuleImpl: TKRuleImpl)
    begin
      ARuleImpl.NewRecord(Self);
    end);
end;

procedure TKViewTableRecord.EnsureDetailStores;
var
  I: Integer;
  LStore: TKViewTableStore;
begin
  if DetailStoreCount = 0 then
  begin
    for I := 0 to Records.Store.ViewTable.DetailTableCount - 1 do
    begin
      LStore := Records.Store.ViewTable.DetailTables[I].CreateStore;
      LStore.MasterRecord := Self;
      AddDetailStore(LStore);
    end;
  end;
end;

function TKViewTableRecord.FieldByName(const AFieldName: string): TKViewTableField;
begin
  Result := inherited FieldByName(AFieldName) as TKViewTableField;
end;

function TKViewTableRecord.FindField(const AFieldName: string): TKViewTableField;
begin
  Result := inherited FindField(AFieldName) as TKViewTableField;
end;

function TKViewTableRecord.GetChildClass(const AName: string): TEFNodeClass;
begin
  Result := TKViewTableField;
end;

function TKViewTableRecord.GetDetailsStore(I: Integer): TKViewTableStore;
begin
  Result := inherited DetailStores[I] as TKViewTableStore;
end;

function TKViewTableRecord.GetField(I: Integer): TKViewTableField;
begin
  Result := inherited Fields[I] as TKViewTableField;
end;

function TKViewTableRecord.GetRecords: TKViewTableRecords;
begin
  Result := inherited Records as TKViewTableRecords;
end;

function TKViewTableRecord.GetStore: TKViewTableStore;
begin
  Result := Records.Store;
end;

function TKViewTableRecord.GetViewTable: TKViewTable;
begin
  Result := Store.ViewTable;
end;

procedure TKViewTableRecord.InternalAfterReadFromNode;
begin
  inherited;
  if Records.Store.MasterRecord <> nil then
    SetDetailFieldValues(Records.Store.MasterRecord);
end;

procedure TKViewTableRecord.Save(const AUseTransaction: Boolean);
var
  LDBCommand: TEFDBCommand;
  LRowsAffected: Integer;
  I: Integer;
begin
  if State = rsClean then
    Exit;

  // BEFORE rules are applied before calling this method.
  if AUseTransaction then
    TKConfig.Instance.DefaultDBConnection.StartTransaction;
  try
    LDBCommand := TKConfig.Instance.DefaultDBConnection.CreateDBCommand;
    try
      case State of
        rsNew: TKSQLBuilder.BuildInsertCommand(Records.Store.ViewTable, LDBCommand, Self);
        rsDirty: TKSQLBuilder.BuildUpdateCommand(Records.Store.ViewTable, LDBCommand, Self);
        rsDeleted: TKSQLBuilder.BuildDeleteCommand(Records.Store.ViewTable, LDBCommand, Self);
      else
        raise EKError.CreateFmt('Unexpected record state %s.', [GetEnumName(TypeInfo(TKRecordState), Ord(State))]);
      end;
      LRowsAffected := LDBCommand.Execute;
      if LRowsAffected <> 1 then
        raise EKError.CreateFmt('Update error. Rows affected: %d.', [LRowsAffected]);
      { TODO : implement cascade delete? }
      for I := 0 to DetailStoreCount - 1 do
        DetailStores[I].Save(False);
      ApplyAfterRules;
      if AUseTransaction then
        TKConfig.Instance.DefaultDBConnection.CommitTransaction;
      MarkAsClean;
    finally
      FreeAndNil(LDBCommand);
    end;
  except
    if AUseTransaction then
      TKConfig.Instance.DefaultDBConnection.RollbackTransaction;
    raise;
  end;
end;

procedure TKViewTableRecord.SetDetailFieldValues(
  const AMasterRecord: TKViewTableRecord);
var
  LMasterFieldNames: TStringDynArray;
  LDetailFieldNames: TStringDynArray;
  I: Integer;
begin
  Assert(Records.Store.ViewTable.IsDetail);
  // Get master and detail field names...
  LMasterFieldNames := Records.Store.ViewTable.MasterTable.Model.GetKeyFieldNames;
  Assert(Length(LMasterFieldNames) > 0);
  LDetailFieldNames := Records.Store.ViewTable.ModelDetailReference.ReferenceField.GetFieldNames;
  Assert(Length(LDetailFieldNames) = Length(LMasterFieldNames));
  for I := 0 to High(LDetailFieldNames) do
  begin
    // ...alias them...
    LMasterFieldNames[I] := Records.Store.ViewTable.MasterTable.ApplyFieldAliasedName(LMasterFieldNames[I]);
    LDetailFieldNames[I] := Records.Store.ViewTable.ApplyFieldAliasedName(LDetailFieldNames[I]);
    // ... and copy values.
    GetNode(LDetailFieldNames[I]).AssignValue(AMasterRecord.GetNode(LMasterFieldNames[I]));
  end;
end;

{ TKViewTableField }

function TKViewTableField.GetAsJSONValue(const AForDisplay: Boolean): string;
var
  LDisplayTemplate: string;

  function Unquote(const AString: string): string;
  begin
    if (Length(AString) >= 2) and (AString[1] = '"') and (AString[Length(AString)] = '"') then
      Result := Copy(AString, 2, Length(AString) - 2)
    else
      Result := AString;
  end;

  function ReplaceFieldValues(const AString: string): string;
  var
    I: Integer;
    LField: TKViewTableField;
  begin
    Result := AString;
    for I := 0 to ViewField.Table.FieldCount - 1 do
    begin
      if ViewField.Table.Fields[I] <> ViewField then
      begin
        LField := ParentRecord.FindField(ViewField.Table.Fields[I].FieldName);
        if Assigned(LField) then
          Result := ReplaceText(Result, '{' + ViewField.Table.Fields[I].FieldName + '}',
            Unquote(LField.GetAsJSONValue(True)));
      end;
    end;
  end;

begin
  Result := inherited GetAsJSONValue(AForDisplay);
  if AForDisplay and Assigned(ViewField) then
  begin
    LDisplayTemplate := ViewField.DisplayTemplate;
    if LDisplayTemplate <> '' then
    begin
      // Replace other field values, this field's value and add back quotes.
      Result := '"' + ReplaceFieldValues(
        ReplaceText(LDisplayTemplate, '{value}', Unquote(Result))) + '"';
    end;
  end;
end;

function TKViewTableField.GetParentRecord: TKViewTableRecord;
begin
  Result := inherited ParentRecord as TKViewTableRecord;
end;

function TKViewTableField.GetViewField: TKViewField;
begin
  Result := ParentRecord.ViewTable.FindFieldByAliasedName(FieldName);
end;

{ TKFileReferenceDataType }

class function TKFileReferenceDataType.GetTypeName: string;
begin
  Result := 'FileReference';
end;

initialization
  TKMetadataRegistry.Instance.RegisterClass('Data', TKDataView);
  TEFDataTypeRegistry.Instance.RegisterClass(TKFileReferenceDataType.GetTypeName, TKFileReferenceDataType);

finalization
  TKMetadataRegistry.Instance.UnregisterClass('Data');
  TEFDataTypeRegistry.Instance.UnregisterClass(TKFileReferenceDataType.GetTypeName);

end.
