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

unit Kitto.Metadata.DataView;

{$I Kitto.Defines.inc}

interface

uses
  Types
  , SysUtils
  , DB
  , Generics.Collections
  , EF.Types
  , EF.Tree
  , Kitto.Metadata
  , Kitto.Metadata.Models
  , Kitto.Metadata.Views
  , Kitto.Store
  , Kitto.Rules
  ;

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

  TKViewField = class;

  TKViewFieldArray = TArray<TKViewField>;

  TKFilterByViewField = record
    DestinationField: TKViewField;
    SourceField: TKViewField;
    ForeignFieldName: string;
  end;

  TKViewField = class(TKMetadataItem)
  private
    function GetAliasedName: string;
    function GetTable: TKViewTable;
    function GetIsVisible: Boolean;
    function GetModelField: TKModelField;
    function GetDisplayLabel: string;
    function GetDisplayWidth: Integer;
    function GetIsRequired: Boolean;
    function GetIsReadOnly: Boolean;
    function GetQualifiedDBName: string;
    function GetModelName: string;
    function GetFieldName: string;
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
    function GetAutoCompleteMinChars: Integer;
    function GetReferenceName: string;
    function GetDisplayTemplate: string;
    function GetFileNameField: string;
    function GetDefaultFilter: string;
    function GetDefaultFilterConnector: string;
    function GetIsPassword: Boolean;
    function GetDBName: string;
    function GetAliasedDBName: string;
    function GetQualifiedAliasedDBName: string;
    function GetLookupFilter: string;
    function GetDBNameOrExpression: string;
    function GetFieldType: TFieldType;
    function GetIsPicture: Boolean;
    const URL_PREFIX = '_URL_';
  strict protected
    function GetChildClass(const AName: string): TEFNodeClass; override;
    function GetDataType: TEFDataType; override;
  public
    function HasModelField: boolean;
    function GetEmptyAsNull: Boolean; override;
    function FindNode(const APath: string; const ACreateMissingNodes: Boolean = False): TEFNode; override;
    function IsAccessGranted(const AMode: string): Boolean; override;
    function GetResourceURI: string; override;
    function CanEditField(const AInsertOperation: Boolean): Boolean;
    property Table: TKViewTable read GetTable;
    property Model: TKModel read GetModel;

    /// <summary>
    ///  Returns a reference to the model field, or raises an exception
    ///  if the model field is not found.
    /// </summary>
    property ModelField: TKModelField read GetModelField;

    /// <summary>
    ///  Returns a reference to the model field, or nil.
    /// </summary>
    function FindModelField: TKModelField;
    property Alias: string read GetAlias;
    property AliasedName: string read GetAliasedName;
    property QualifiedAliasedDBNameOrExpression: string read GetQualifiedAliasedDBNameOrExpression;
    property DBName: string read GetDBName;
    property AliasedDBName: string read GetAliasedDBName;
    property QualifiedAliasedDBName: string read GetQualifiedAliasedDBName;
    property QualifiedDBName: string read GetQualifiedDBName;
    property QualifiedDBNameOrExpression: string read GetQualifiedDBNameOrExpression;
    property DBNameOrExpression: string read GetDBNameOrExpression;
    property AllowedValues: TEFPairs read GetAllowedValues;

    property CanInsert: Boolean read GetCanInsert;
    property CanUpdate: Boolean read GetCanUpdate;

    /// <summary>
    ///  If the field is referenced, returns the names of the key fields
    ///  in the reference, separated by TKConfig.Instance.MultiFieldSeparator.
    ///  Otherwise, returns the FieldName.
    /// </summary>
    property FieldNamesForUpdate: string read GetFieldNamesForUpdate;

    /// <summary>
    ///  Returns True if the field is a reference field, that is if the
    ///  field is part of the containing view table's model and the underlying
    ///  model field is a reference field.
    /// </summary>
    property IsReference: Boolean read GetIsReference;

    /// <summary>
    ///  Optional filter to use when creating select lists. Only applies to
    ///  reference fields.
    /// </summary>
    property DefaultFilter: string read GetDefaultFilter;

    /// <summary>
    ///  Optional filter to use when creating lookup lists. Only applies to
    ///  reference fields.
    /// </summary>
    property LookupFilter: string read GetLookupFilter;

    /// <summary>
    ///  Specifies the logical connector to use when appending the
    ///  DefaultFilter to an existing WHERE clause (for example, a referenced
    ///  model's own DefaultFilter). Defaults to 'and'; another common value
    ///  is 'or'.
    /// </summary>
    property DefaultFilterConnector: string read GetDefaultFilterConnector;

    /// <summary>
    ///  Returns the field's DataType, unless it's a reference field,
    ///  in which case returns the data type of the referenced model's
    ///  CaptionField.
    /// </summary>
    function GetActualDataType: TEFDataType;

    /// <summary>
    ///  If the field is from a different model than the table's model,
    ///  returns the model field that references its model, otherwise returns
    ///  nil.
    /// </summary>
    property ReferenceField: TKModelField read GetReferenceField;

    /// <summary>
    ///  <para>
    ///   Returns True if the field is the reference field of
    ///   MasterTable's model's detail reference to this vie wtable's
    ///   model.
    ///  </para>
    ///  <para>
    ///   IOW, returns True if the view table is a detail table and the
    ///   field is (part of) the link to its master table.
    ///  </para>
    /// </summary>
    property IsDetailReference: Boolean read GetIsDetailReference;

    /// <summary>
    ///  Creates a store with the current field, all key fields of
    ///  the referenced model and a concatenation of all key fields (if > 1)
    ///  stored as a single field (used by lookups).
    ///  If the field is not a reference field, an exception is raised.
    /// </summary>
    function CreateReferenceStore: TKStore;

    /// <summary>
    ///  Creates a store with all fields from the referenced model and fetches
    ///  the record specified by the key values.
    /// </summary>
    function CreateReferencedModelStore(const AKeyValues: Variant): TKStore;

    /// <summary>
    ///  Extract and returns the model name from the Name. If no model name is
    ///  specified (because the field is part of the main model), returns the
    ///  main model name.
    /// </summary>
    property ModelName: string read GetModelName;

    /// <summary>
    ///  Extract and returns the reference name from the Name, or '' if the
    ///  field is not a reference field.
    /// </summary>
    property ReferenceName: string read GetReferenceName;

    /// <summary>
    ///  Extract and returns the field name without the model name qualifier.
    ///  If the field is part of the main model, this is equal to Name.
    /// </summary>
    property FieldName: string read GetFieldName;

    property IsKey: Boolean read GetIsKey;
    property IsVisible: Boolean read GetIsVisible;
    property IsRequired: Boolean read GetIsRequired;
    property IsReadOnly: Boolean read GetIsReadOnly;
    property IsPassword: Boolean read GetIsPassword;
    property EmptyAsNull: Boolean read GetEmptyAsNull;
    property DefaultValue: Variant read GetDefaultValue;

    /// <summary>
    ///  Appends to ANode one child node for each default value. Regular fields
    ///  have at most one default value (returned by the DefaultValue property),
    ///  while reference fields can have more (one for each physical constituent
    ///  field). Model fields are queries for default values automatically.
    /// </summary>
    procedure AppendDefaultValues(const ANode: TEFNode);

    property Expression: string read GetExpression;

    property DisplayLabel: string read GetDisplayLabel;
    property Hint: string read GetHint;
    property DisplayWidth: Integer read GetDisplayWidth;
    property DecimalPrecision: Integer read GetDecimalPrecision;
    property DataType: TEFDataType read GetDataType;
    property FieldType: TFieldType read GetFieldType;
    property ActualDataType: TEFDataType read GetActualDataType;
    property Size: Integer read GetSize;
    property IsBlob: Boolean read GetIsBlob;
    property EditFormat: string read GetEditFormat;
    property DisplayFormat: string read GetDisplayFormat;
    property BlankValue: Boolean read GetBlankValue;
    property AutoCompleteMinChars: Integer read GetAutoCompleteMinChars;
    property DisplayTemplate: string read GetDisplayTemplate;

    property Rules: TKRules read GetRules;
    /// <summary>
    ///  Calls the specified function once for each view-level and then each
    ///  model-level rule that is not overridden at the view level.
    ///  The function should do something with the rule implementation object
    ///  it receives (most commonly apply the rule) and return True as long as
    ///  the call chain should proceed, or False to stop the rule enumeration.
    ///  Generally the function will return True in the most common cases.
    /// </summary>
    procedure EnumRules(const AEnumFunc: TFunc<TKRuleImpl, Boolean>);
    /// <summary>
    ///  True if the either the view or the model field has at least one
    ///  server-side rule. Used to decide whether to make an async call
    ///  or not when the field changes on the client.
    /// </summary>
    function HasServerSideRules: Boolean;

    /// <summary>
    ///  If the field is a reference field, creates and returns an array
    ///  of view fields in the current view table from the same referenced
    ///  model (including itself).
    /// </summary>
    /// <exception cref="Assert">
    ///  Violation if IsReference is False.
    /// </exception>
    /// <example>
    ///  If the field is a reference field called City, then all fields
    ///  called City.* are added to the returned array, plus itself (City).
    /// </example>
    function GetDerivedFields: TArray<TKViewField>;

    /// <summary>
    ///  Creates and loads a store with a record containing all derived
    ///  values for a reference field. The caller is responsible for freeing the
    ///  store object.
    /// </summary>
    /// <param name="AKeyValues">
    ///  Key values for the record to fetch.
    /// </param>
    /// <exception cref="Assert">
    ///  If the field is not a reference field, an
    ///  assertion violation is raised.
    /// </exception>
    function CreateDerivedFieldsStore(const AKeyValues: string): TKStore;

    /// <summary>
    ///  For blob or file reference fields, optionally specifies the
    ///  name of another field in the same view table that will store the
    ///  original file name upon upload.
    /// </summary>
    property FileNameField: string read GetFileNameField;

    /// <summary>
    ///  Returns True if any view fields exist in the view table that
    ///  have this field's model field as reference field. Used to discover
    ///  if there are any fields that need to be refreshed when the current
    ///  field's value changes.
    /// </summary>
    function DerivedFieldsExist: Boolean;

    function GetColorsAsPairs: TEFPairs;

    function GetFilterByFields: TArray<TKFilterByViewField>;

    /// <summary>
    ///  Translate and returns a sort clause suitable for ordering a set of
    ///  records on this field.
    /// </summary>
    function BuildSortClause(const AIsDescending: Boolean): string;

    property IsPicture: Boolean read GetIsPicture;

    function GetURLFieldName: string;
    class function IsURLFieldName(const AFieldName: string): Boolean;
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
  end;

  TKViewTableRecord = class;
  TKViewTableRecords = class;

  TKViewTableHeader = class;

  TKViewTableStore = class(TKStore)
  strict private
    FMasterRecord: TKViewTableRecord;
    FViewTable: TKViewTable;
    procedure SetupFields;
    function GetRecords: TKViewTableRecords;
    function GetHeader: TKViewTableHeader;
  strict protected
    function GetChildClass(const AName: string): TEFNodeClass; override;
  public
    constructor Create(const AViewTable: TKViewTable); reintroduce;
    property MasterRecord: TKViewTableRecord read FMasterRecord write FMasterRecord;
    property ViewTable: TKViewTable read FViewTable;
    property Header: TKViewTableHeader read GetHeader;
    property Records: TKViewTableRecords read GetRecords;

    /// <summary>
    ///  Loads a page of data according to AFrom and AFor arguments,
    ///  and returns the total number of records in all pages.
    /// </summary>
    /// <param name="AFilter">Additional SQL filter.</param>
    /// <param name="AFrom">Number of the first record to retrieve (0-based).</param>
    /// <param name="ATo">Maximum count of records to retrieve.</param>
    /// <remarks>
    ///  <para>
    ///   This method will perform two database queries, one to get the
    ///   total count and one to get the requested data page.
    ///  </para>
    ///  <para>
    ///   If AFrom or ATo are 0, the method calls <see cref="Load" />.
    ///  </para>
    /// </remarks>
    function Load(const AFilter: string = ''; const ASort: string = '';
      const AFrom: Integer = 0; const AFor: Integer = 0;
      const AForEachRecord: TProc<TKVIewTableRecord> = nil): Integer; overload;

    /// <summary>
    ///  Appends a record and fills it with the specified values.
    /// </summary>
    function AppendRecord(const AValues: TEFNode): TKViewTableRecord;

    /// <summary>
    ///  Locates and returns a record from the key values stored in AKey.
    ///  Raises an exception if the record is not found.
    /// </summary>
    /// <param name="AKey">
    ///  Object containing at least one top-level pair for each key value.
    /// </param>
    /// <param name="AFormatSettings">
    ///  Used to interpret string values (all pair values are read as string and
    ///  then converted according to this settings object).
    /// </param>
    /// <param name="AValueIndex">
    ///  If each pair in AKey contains more than one value, set this param to
    ///  an index >=0 to consider that value. Normally each pair contains a
    ///  single value, so you just don't pass this param.
    /// </param>
    function GetRecord(const AKey: TEFTree; const AFormatSettings: TFormatSettings;
      const AValueIndex: Integer = -1): TKViewTableRecord;
    function FindRecord(const AKey: TEFTree; const AFormatSettings: TFormatSettings;
      const AValueIndex: Integer = -1): TKViewTableRecord;
  end;

  TKViewTableHeaderField = class;

  TKViewTableHeader = class(TKHeader)
  private
    function GetField(I: Integer): TKViewTableHeaderField;
  protected
    function GetChildClass(const AName: string): TEFNodeClass; override;
  public
    function AddField(const AFieldName: string): TKViewTableHeaderField;
    property Fields[I: Integer]: TKViewTableHeaderField read GetField;
    function FindField(const AFieldName: string): TKViewTableHeaderField;
    function FieldByName(const AFieldName: string): TKViewTableHeaderField;
  end;

  TKViewTableHeaderField = class(TKHeaderField)
  strict private
    FViewField: TKViewField;
    // Cached value from FViewField.
    FViewFieldIsPassword: Boolean;
    // Cached value from FViewField.
    FViewFieldDisplayTemplate: string;
    FModelField: TKModelField;
  public
    // Returns Self.
    function SetViewField(const AValue: TKViewField; const AOverrideDataType: TEFDataType = nil): TKViewTableHeaderField;
    // Returns Self.
    function SetModelField(const AValue: TKModelField): TKViewTableHeaderField;

    property ViewField: TKViewField read FViewField;
    property ViewFieldIsPassword: Boolean read FViewFieldIsPassword;
    property ViewFieldDisplayTemplate: string read FViewFieldDisplayTemplate;
    /// <summary>
    /// If the ViewField is a reference field, this property stores either:
    /// - ViewField.ModelField (if it's the reference field itself, e.g. Customer), or
    /// - The actual model field (if it's a physical field that was expanded
    /// as part of the reference, e.g. Customer_Id).
    /// If ViewField is not a reference field, this property always stores ViewField.ModelField.
    /// </summary>
    property ModelField: TKModelField read FModelField;
  end;

  TKViewTableField = class(TKField)
  strict private
    function GetParentRecord: TKViewTableRecord;
    function GetHeaderField: TKViewTableHeaderField;
    function GetViewField: TKViewField;
  private
    function GetModelField: TKModelField;
  strict protected
    function GetDecimalPrecision: Integer; override;
  public
    function GetEmptyAsNull: Boolean; override;
    function GetAsJSONValue(const AForDisplay: Boolean; const AQuote: Boolean = True;
      const AEmptyNulls: Boolean = False): string; override;
    property ParentRecord: TKViewTableRecord read GetParentRecord;
    property HeaderField: TKViewTableHeaderField read GetHeaderField;
    property ViewField: TKViewField read GetViewField;
    /// <summary>
    /// <seealso>TKViewTableHeaderField.ModelField</seealso>
    /// </summary>
    property ModelField: TKModelField read GetModelField;
    /// <summary>
    ///  True if ViewField.IsReference and (ModelField <> ViewField.ModelField).
    /// </summary>
    function IsPhysicalPartOfReference: Boolean;
  end;

  TKViewTableRecord = class(TKRecord)
  strict private
    function GetRecords: TKViewTableRecords;
    function GetDetailsStore(I: Integer): TKViewTableStore;
    function GetStore: TKViewTableStore;
    function GetViewTable: TKViewTable;
    function GetField(I: Integer): TKViewTableField;
  private
    FReferenceViewFieldBeingChanged: TKViewField;
  strict protected
    procedure InternalAfterReadFromNode; override;
    function GetChildClass(const AName: string): TEFNodeClass; override;
    function GetXMLTagName: string; override;
    function TranslateFieldName(const AFieldName: string): string; override;
  public
    procedure FieldChanging(const AField: TKField; const AOldValue: Variant;
      var ANewValue: Variant; var ADoIt: Boolean); override;
    procedure FieldChanged(const AField: TKField; const AOldValue, ANewValue: Variant); override;
    property Records: TKViewTableRecords read GetRecords;
    property Store: TKViewTableStore read GetStore;
    property ViewTable: TKViewTable read GetViewTable;
    procedure EnsureDetailStores;
    procedure LoadDetailStores;
    property DetailStores[I: Integer]: TKViewTableStore read GetDetailsStore;
    function AddDetailStore(const AStore: TKViewTableStore): TKViewTableStore;
    procedure Refresh(const AStrict: Boolean = False);
    procedure SetDetailFieldValues(const AMasterRecord: TKViewTableRecord);
    function FindDetailStoreByModelName(const AModelName: string; const AForceLoad: Boolean = False): TKViewTableStore;
    function GetDetailStoreByModelName(const AModelName: string; const AForceLoad: Boolean = False): TKViewTableStore;
    procedure HandleDeleteFileInstructions;

    procedure ApplyNewRecordRules;
    // Same as above but fires the view table's model's Before/AfterNewRecord events.
    procedure ApplyNewRecordRulesAndFireEvents(const AViewTable: TKViewTable; const AIsCloned: Boolean);
    procedure ApplyEditRecordRules;
    procedure ApplyAfterShowEditWindowRules;
    procedure ApplyDuplicateRecordRules;
    procedure ApplyBeforeRules;
    procedure ApplyAfterRules;

    property Fields[I: Integer]: TKViewTableField read GetField; default;
    function FindField(const AFieldName: string): TKViewTableField;
    function FieldByName(const AFieldName: string): TKViewTableField;

    /// <summary>
    ///  Replaces all field name markers in AText with the current field values
    ///  in JSON format.
    ///  A field name marker is in the form {FieldName}.
    ///  Pass True in AEmptyNulls to expand null fields to empty strings,
    ///  otherwise they are expanded as 'null'.
    ///  If ASender is specified, then the value of the corresponding field
    ///  is not expanded (useful to avoid infinite recursion when this method
    ///  is called by TKViewTableField.GetAsJSONValue).
    /// </summary>
    procedure ExpandFieldJSONValues(var AText: string;
      const AEmptyNulls: Boolean; const ASender: TKViewField = nil);

    /// <summary>
    ///  Replaces occurrencess of {FieldName} tags in the specified string
    ///  with actual field values, formatted as strings.
    ///  If the record's store has a master record, this method also replaces
    ///  occurrences of {MasterRecord.FieldName} with string representations of
    ///  master record field values.
    /// </summary>
    procedure ExpandExpression(var AExpression: string); override;
  end;

  TKViewTableRecords = class(TKRecords)
  strict private
    function GetStore: TKViewTableStore;
    function GetRecord(I: Integer): TKViewTableRecord; overload;
  strict protected
    function GetChildClass(const AName: string): TEFNodeClass; override;
    function GetXMLTagName: string; override;
  public
    property Store: TKViewTableStore read GetStore;
    function Append: TKViewTableRecord;
    function AppendAndInitialize: TKViewTableRecord;
    property Records[I: Integer]: TKViewTableRecord read GetRecord; default;

    function FindRecord(const AValues: TEFNode): TKViewTableRecord;
    function GetRecord(const AValues: TEFNode): TKViewTableRecord; overload;
  end;

  TKViewTable = class(TKMetadataItem)
  strict private
    FFindingNode: Boolean;
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
    function GetDatabaseName: string;
  strict
  private
    function GetIsLarge: Boolean; protected
    function GetChildClass(const AName: string): TEFNodeClass; override;
    function GetFields: TKViewFields;
    function GetDetailTables: TKViewTables;
  public
    function GetDefaultDisplayLabel: string;

    function FindNode(const APath: string;
      const ACreateMissingNodes: Boolean = False): TEFNode; override;

    property ModelName: string read GetModelName;

    property ImageName: string read GetImageName;

    property IsDetail: Boolean read GetIsDetail;
    property MasterTable: TKViewTable read GetMasterTable;

    function HasModelName: Boolean;

    /// <summary>
    ///   If the view table is a detail, this property contains the name
    ///   of the detail reference in the master view table's model. Otherwise
    ///   it's empty.
    /// </summary>
    property ModelDetailReferenceName: string read GetModelDetailReferenceName;

    /// <summary>If the view table is a detail, this property returns the model
    /// detail reference in the master view table's model. Otherwise raises an
    /// exception.</summary>
    /// <remarks>Check IsDetail before calling this method, if you want to
    /// avoid exceptions.</remarks>
    property ModelDetailReference: TKModelDetailReference read GetModelDetailReference;

    property DisplayLabel: string read GetDisplayLabel;
    property PluralDisplayLabel: string read GetPluralDisplayLabel;

    property Model: TKModel read GetModel;

    property FieldCount: Integer read GetFieldCount;
    property Fields[I: Integer]: TKViewField read GetField;
    function GetFieldNames: TStringDynArray;
    function FindField(const AName: string): TKViewField;
    function FieldByName(const AName: string): TKViewField;
    function FieldByAliasedName(const AAliasedName: string): TKViewField;
    function FindFieldByAliasedName(const AAliasedName: string): TKViewField;
    function FindFieldByDBColumnName(const ADBColumnName: string): TKViewField;
    function FieldByDBColumnName(const ADBColumnName: string): TKViewField;
    /// <summary>
    ///  Returns the view field matching the specified model field, or nil.
    /// </summary>
    function FindFieldByModelField(const AModelField: TKModelField): TKViewField;
    /// <summary>
    ///  Returns the view field matching the specified model field, or raises
    ///  an exception.
    /// </summary>
    function FieldByModelField(const AModelField: TKModelField): TKViewField;


    function FieldsByModelFields(const AModelFields: TKModelFieldArray): TKViewFieldArray;

    /// <summary>
    ///  If a field with the specified name is contained in a parent field (at the model level),
    ///  returns a reference to the parent field, otherwise nil.
    /// </summary>
    function FindParentField(const AFieldName: string): TKViewField;
    function GetKeyFieldAliasedNames: TStringDynArray;
    function GetFieldArray(AFilter: TFunc<TKViewField, Boolean>): TArray<TKViewField>;

    function IsFieldVisible(const AField: TKViewField): Boolean;

    property IsReadOnly: Boolean read GetIsReadOnly;

    /// <summary>
    ///  Optional fixed filter expression to apply when building the select
    ///  SQL statement to display data. Should refer to fields through
    ///  qualified names. Defaults to ''.
    /// </summary>
    property DefaultFilter: string read GetDefaultFilter;

    /// <summary>
    ///  Optional fixed ORDER BY expression to apply when building the select
    ///  SQL statement to display data. Should refer to fields through
    ///  qualified names (or ordinal numbers for expression-based fields).
    ///  Defaults to Model.DefaultSorting.
    /// </summary>
    property DefaultSorting: string read GetDefaultSorting;

    property DetailTableCount: Integer read GetDetailTableCount;
    property DetailTables[I: Integer]: TKViewTable read GetDetailTable;
    function DetailTableByName(const AName: string): TKViewTable;
    function GetDetailTableIndex(const AViewTable: TKViewTable): Integer;
    procedure AddDetailTable(const AViewTable: TKViewTable);

    property View: TKDataView read GetView;

    /// <summary>
    ///  Returns an array with any view table's model name
    ///  (except the view's MainTable, which is assumed
    ///  implicit) downto and including the current view table's name.
    /// </summary>
    /// <example>
    ///  <para>Called on the Parties view's MainTable yields an empty array</para>
    ///  <para>Called on its only detail table yields 'Invitation'</para>
    /// </example>
    function GetNamePath: TStringDynArray;

    /// <summary>
    ///   Finds and returns a reference to a layout named after the view's
    ///   PersistentName plus an underscore ('_') and the specified kind. If no
    ///   layout exists under that name, returns nil.
    /// </summary>
    /// <param name="AKind">
    ///   Kind of layout to look for. Common kinds are 'List' and 'Form'.
    /// </param>
    function FindLayout(const AKind: string): TKLayout;

    /// <summary>
    ///  Creates and returns a store with the view's metadata.
    /// </summary>
    function CreateStore: TKViewTableStore;

    /// <summary>
    ///  Creates and returns a node with one child for each default
    ///  value as specified in the view table or model. Any default expression
    ///  is evaluated at this time.
    /// </summary>
    /// <remarks>
    ///  The caller is responsible for freeing the returned node object.
    /// </remarks>
    function GetDefaultValues(const AKeyOnly: Boolean = False): TEFNode;

    function GetResourceURI: string; override;

    function IsAccessGranted(const AMode: string): Boolean; override;

    property Rules: TKRules read GetRules;
    procedure ApplyRules(const AApplyProc: TProc<TKRuleImpl>);

    /// <summary>
    ///  If the specified field exists in the view table, the method
    ///  returns its AliasedName, otherwise the specified field name is
    ///  returned.
    /// </summary>
    function ApplyFieldAliasedName(const AFieldName: string): string;

    property DatabaseName: string read GetDatabaseName;

    function GetFilterByFields(APredicate: TFunc<TKFilterByViewField, Boolean>): TArray<TKFilterByViewField>;

    /// <summary>
    ///  True if the underlying data store is a large one. Set this to True at the
    ///  view table level to override the setting in the model (for example if
    ///  the view table is filtered).
    /// </summary>
    property IsLarge: Boolean read GetIsLarge;
  end;

  TKDataView = class(TKView)
  private
    function GetMainTable: TKViewTable;
    function GetDatabaseName: string;
  protected
    function GetChildClass(const AName: string): TEFNodeClass; override;
    function GetDisplayLabel: string; override;
    function GetDefaultImageName: string; override;
    function GetImageName: string; override;
  public
    property MainTable: TKViewTable read GetMainTable;
    property DatabaseName: string read GetDatabaseName;
  end;

  TKFileReferenceDataType = class(TEFStringDataType)
  public
    class function GetTypeName: string; override;
  end;

  TKHTMLMemoDataType = class(TEFMemoDataType)
  public
    class function GetTypeName: string; override;
  end;

implementation

uses
  StrUtils
  , Variants
  , TypInfo
  , IOUtils
  , Math
  , EF.Localization
  , EF.DB
  , EF.StrUtils
  , EF.VariantUtils
  , EF.Macros
  , EF.JSON
  , Kitto.SQL
  , Kitto.Types
  , Kitto.Config
  , KItto.Auth
  , Kitto.AccessControl
  , Kitto.DatabaseRouter
  ;

{ TKDataView }

function TKDataView.GetChildClass(const AName: string): TEFNodeClass;
begin
  if SameText(AName, 'MainTable') then
    Result := TKViewTable
  else
    Result := inherited GetChildClass(AName);
end;

function TKDataView.GetDatabaseName: string;
var
  LDatabaseRouterNode: TEFNode;
begin
  LDatabaseRouterNode := FindNode('DatabaseRouter');
  if Assigned(LDatabaseRouterNode) then
    Result := TKDatabaseRouterFactory.Instance.GetDatabaseName(
      LDatabaseRouterNode.AsString, Self, LDatabaseRouterNode)
  else
    Result := '';
end;

function TKDataView.GetDisplayLabel: string;
begin
  Result := inherited GetDisplayLabel;
  if Result = '' then
    Result := MainTable.PluralDisplayLabel;
end;

function TKDataView.GetDefaultImageName: string;
begin
  Result := MainTable.ImageName;
end;

function TKDataView.GetImageName: string;
begin
  Result := inherited GetImageName;
  if Result = DEFAULT_IMAGE_NAME then
    Result := DefaultImageName;
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

function TKViewTable.CreateStore: TKViewTableStore;
begin
  Result := TKViewTableStore.Create(Self);
end;

function TKViewTable.DetailTableByName(const AName: string): TKViewTable;
begin
  Result := GetDetailTables.ChildByName(AName) as TKViewTable;
end;

function TKViewTable.FieldByAliasedName(const AAliasedName: string): TKViewField;
begin
  Result := FindFieldByAliasedName(AAliasedName);
  if not Assigned(Result) then
    raise EKError.CreateFmt('ViewField %s not found.', [AAliasedName]);
end;

function TKViewTable.FieldByName(const AName: string): TKViewField;
begin
  Result := GetFields.ChildByName(AName) as TKViewField;
end;

function TKViewTable.FieldsByModelFields(const AModelFields: TKModelFieldArray): TKViewFieldArray;
var
  I: Integer;
begin
  SetLength(Result, Length(AModelFields));
  for I := Low(Result) to High(Result) do
    Result[I] := FieldByModelField(AModelFields[I]);
end;

function TKViewTable.FieldByDBColumnName(const ADBColumnName: string): TKViewField;
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

function TKViewTable.FindFieldByModelField(const AModelField: TKModelField): TKViewField;
begin
  Result := GetFields.FindChildByPredicate(
    function(const ANode: TEFNode): Boolean
    begin
      Result := TKViewField(ANode).ModelField = AModelField;
    end) as TKViewField;
end;

function TKViewTable.FieldByModelField(const AModelField: TKModelField): TKViewField;
begin
  Result := FindFieldByModelField(AModelField);
  if not Assigned(Result) then
    raise EKError.CreateFmt('ViewField not found for model field %s.', [AModelField.Name]);
end;

function TKViewTable.FindField(const AName: string): TKViewField;
begin
  Result := GetFields.FindChild(AName) as TKViewField;
end;

function TKViewTable.FindFieldByAliasedName(const AAliasedName: string): TKViewField;
begin
  Result := GetFields.FindChildByPredicate(
    function(const ANode: TEFNode): Boolean
    begin
      Result := SameText(TKViewField(ANode).AliasedName, AAliasedName);
    end) as TKViewField;
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

function TKViewTable.GetModelName: string;
var
  LModelNameNode: TEFNode;
begin
  LModelNameNode := FindNode('Model');
  if Assigned(LModelNameNode) and (LModelNameNode.AsString <> '') then
    Result := LModelNameNode.AsExpandedString
  else if Assigned(MasterTable) and (ModelDetailReferenceName <> '') then
    Result := MasterTable.Model.DetailReferenceByName(ModelDetailReferenceName).DetailModelName
  else
    Result := '';
end;

function TKViewTable.HasModelName: Boolean;
begin
  Result := ModelName <> '';
end;

function TKViewTable.GetModel: TKModel;
begin
  Result := View.Catalog.Models.ModelByName(ModelName);
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

function TKViewTable.GetDefaultValues(const AKeyOnly: Boolean): TEFNode;
var
  I: Integer;
begin
  Result := TEFNode.Create;
  try
    for I := 0 to FieldCount - 1 do
      if not AKeyOnly or Fields[I].IsKey then
        Fields[I].AppendDefaultValues(Result);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TKViewTable.GetDatabaseName: string;
var
  LDatabaseRouterNode: TEFNode;
begin
  LDatabaseRouterNode := FindNode('DatabaseRouter');
  if Assigned(LDatabaseRouterNode) then
    Result := TKDatabaseRouterFactory.Instance.GetDatabaseName(
      LDatabaseRouterNode.AsString, Self, LDatabaseRouterNode)
  else
  begin
    Result := View.DatabaseName;
    if Result = '' then
      Result := Model.DatabaseName;
  end;
end;

function TKViewTable.GetDefaultFilter: string;
begin
  Result := GetString('DefaultFilter');
  if Result <> '' then
    Result := '(' + Result + ')';
end;

function TKViewTable.GetDefaultDisplayLabel: string;
begin
  if IsDetail then
    Result := ModelDetailReference.DisplayLabel
  else
    Result := Model.DisplayLabel;
end;

function TKViewTable.GetDetailTableCount: Integer;
begin
  Result := GetDetailTables.GetChildCount<TKViewTable>;
end;

function TKViewTable.GetDetailTableIndex(const AViewTable: TKViewTable): Integer;
begin
  Result := GetDetailTables.GetChildIndex(AViewTable);
end;

function TKViewTable.GetDetailTables: TKViewTables;
begin
  Result := GetNode('DetailTables', True) as TKViewTables;
end;

function TKViewTable.GetDisplayLabel: string;
begin
  Result := GetString('DisplayLabel');
  if Result = '' then
    Result := GetDefaultDisplayLabel;
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
var
  LFields: TKViewFields;

  procedure CreateDefaultFields;
  var
    I, J, K: Integer;
    LModelField: TKModelField;
    LAutoAddFields: TEFNode;
    LViewField: TKViewField;
    LAutoAddField: TEFNode;
  begin
    for I := 0 to Model.FieldCount - 1 do
    begin
      LModelField := Model.Fields[I];
      LViewField := TKViewField.Create(LModelField.FieldName);
      LFields.AddChild(LViewField);
      LAutoAddFields := LModelField.FindNode('AutoAddFields');
      if Assigned(LAutoAddFields) then
      begin
        for J := 0 to LAutoAddFields.ChildCount - 1 do
        begin
          LAutoAddField := LAutoAddFields.Children[J];
          LViewField := TKViewField.Create(LModelField.FieldName + '.' + LAutoAddField.Name, LAutoAddField.AsExpandedString);
          for K := 0 to LAutoAddField.ChildCount - 1 do
            LViewField.AddChild(TEFNode.Clone(LAutoAddField.Children[K]));
          LFields.AddChild(LViewField);
        end;
      end;
    end;
  end;

begin
  LFields := GetNode('Fields', True) as TKViewFields;
  if LFields.FieldCount = 0 then
    CreateDefaultFields;
  Result := LFields;
end;

function TKViewTable.GetFilterByFields(APredicate: TFunc<TKFilterByViewField, Boolean>): TArray<TKFilterByViewField>;
var
  I: Integer;
  LFilterByFields: TArray<TKFilterByViewField>;
  LFieldIdx: Integer;
begin
  if not Assigned(APredicate) then
    APredicate :=
      function (AFilterByViewField: TKFilterByViewField): Boolean
      begin
        Result := True;
      end;

  SetLength(Result, 0);
  for I := 0 to FieldCount - 1 do
  begin
    LFilterByFields := Fields[I].GetFilterByFields;
    for LFieldIdx := Low(LFilterByFields) to High(LFilterByFields) do
    begin
      if APredicate(LFilterByFields[LFieldIdx]) then
      begin
        SetLength(Result, Length(Result) + 1);
        Result[High(Result)] := LFilterByFields[LFieldIdx];
      end;
    end;
  end;
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

function TKViewTable.GetIsLarge: Boolean;
var
  LNode: TEFNode;
begin
  LNode := FindNode('IsLarge');
  if Assigned(LNode) then
    Result := LNode.AsBoolean
  else
    Result := Model.IsLarge;
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
  if Result = '' then
    Result := Model.PluralDisplayLabel;
end;

function TKViewTable.GetResourceURI: string;
begin
  Result := View.GetResourceURI;
  if Result <> '' then
    Result := Result + '/' + Join(GetNamePath, '/');
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
var
  LDefaultLayoutName: string;
begin
  LDefaultLayoutName := SmartConcat(View.PersistentName, '.', Join(GetNamePath, '.')) + '_' + AKind;
  Result := View.Catalog.Layouts.FindLayout(LDefaultLayoutName);
end;

function TKViewTable.FindNode(const APath: string;
  const ACreateMissingNodes: Boolean): TEFNode;
begin
  Result := inherited FindNode(APath, ACreateMissingNodes);
  if not Assigned(Result) and not FFindingNode then
  begin
    // Prevent Stack-overflow when testing HasModelName
    FFindingNode := True;
    try
      if HasModelName then
        Result := Model.FindNode(APath, False);
    finally
      FFindingNode := False;
    end;
  end;
end;

function TKViewTable.FindParentField(const AFieldName: string): TKViewField;
var
  LModelField: TKModelField;
  LFieldName: string;
begin
  LFieldName := AFieldName;
  if LFieldName.Contains(TKConfig.Instance.MultiFieldSeparator) then
    LFieldName := LFieldName.Split([TKConfig.Instance.MultiFieldSeparator], TStringSplitOptions.None)[0];
  // We can safely assume that all fields in a composite field share the same parent field.
  LModelField := Model.FindField(LFieldName);
  if Assigned(LModelField) and Assigned(LModelField.ParentField) then
    Result := FindFieldByModelField(LModelField.ParentField)
  else
    Result := nil;
end;

function TKViewTable.GetDetailTable(I: Integer): TKViewTable;
begin
  Result := GetDetailTables.GetChild<TKViewTable>(I);
end;

function TKViewTable.GetNamePath: TStringDynArray;
begin
  if MasterTable <> nil then
  begin
    Result := MasterTable.GetNamePath;
    SetLength(Result, Length(Result) + 1);
    Result[High(Result)] := MasterTable.GetModelDetailReferenceName;
    if Result[High(Result)] = '' then
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
  Result := TKAccessController.Current.IsAccessGranted(TKAuthenticator.Current.UserName, GetACURI, AMode)
    // A dataview and its main table currently share the same resource URI,
    // so it's useless to test it twice.
    //and TKConfig.Instance.IsAccessGranted(View.GetACURI, AMode)
    and TKAccessController.Current.IsAccessGranted(TKAuthenticator.Current.UserName, Model.GetACURI, AMode);
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

procedure TKViewField.AppendDefaultValues(const ANode: TEFNode);
var
  LValue: Variant;
  LDefaultValueNode: TEFNode;
  I: Integer;
  LConcatenation: string;
  LStrValue: string;
begin
  Assert(Assigned(ANode));

  // Get regular default value.
  LValue := DefaultValue;
  if not VarIsNull(LValue) and (EFVarToStr(LValue) <> '') then
    ANode.AddChild(FieldName, LValue)
  else if IsReference then
  begin
    // Get model-level multi-column default values.
    for I := 0 to ModelField.FieldCount - 1 do
    begin
      LValue := ModelField.Fields[I].DefaultValue;
      if not VarIsNull(LValue) and (EFVarToStr(LValue) <> '') then
        ANode.AddChild(ModelField.Fields[I].FieldName, LValue);
    end;
    // Get view-level multi-column default values (override).
    LDefaultValueNode := FindNode('DefaultValue');
    if Assigned(LDefaultValueNode) then
    begin
      for I := 0 to LDefaultValueNode.ChildCount - 1 do
        ANode.SetValue(LDefaultValueNode.Children[I].Name, LDefaultValueNode.Children[I].Value);
    end;
    // Store values in concatenated form as well in case of multi-valued references.
    if ModelField.FieldCount > 1 then
    begin
      LConcatenation := '';
      for I := 0 to ModelField.FieldCount - 1 do
      begin
        LStrValue := ANode.GetString(ModelField.Fields[I].FieldName);
        // Note: we need to keep the concatenated value and wipe the single values
        // in order for their change handler to be triggered later when the compound value
        // is expanded back to the single fields.
        ANode.DeleteNode(ModelField.Fields[I].FieldName);
        if LConcatenation = '' then
          LConcatenation := LStrValue
        else
          LConcatenation := LConcatenation + TKConfig.Instance.MultiFieldSeparator + LStrValue;
      end;
      ANode.SetString(Join(ModelField.GetFieldNames, TKConfig.Instance.MultiFieldSeparator), LConcatenation);
    end;
  end;
end;

procedure TKViewField.EnumRules(const AEnumFunc: TFunc<TKRuleImpl, Boolean>);
var
  I: Integer;
  LRuleImpl: TKRuleImpl;
  LRule: TKRule;
  LInterrupted: Boolean;
begin
  Assert(Assigned(AEnumFunc));

  LInterrupted := False;
  // Apply rules at the View level.
  for I := 0 to Rules.RuleCount - 1 do
  begin
    LRule := Rules[I];
    LRuleImpl := TKRuleImplFactory.Instance.CreateObject(LRule.Name);
    try
      LRuleImpl.Rule := LRule;
      LInterrupted := AEnumFunc(LRuleImpl);
      if LInterrupted then
        Break;
    finally
      FreeAndNil(LRuleImpl);
    end;
  end;

  if not LInterrupted then
  begin
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
          LInterrupted := AEnumFunc(LRuleImpl);
          if LInterrupted then
            Break;
        finally
          FreeAndNil(LRuleImpl);
        end;
      end;
    end;
  end;
end;

function TKViewField.BuildSortClause(const AIsDescending: Boolean): string;
var
  LResult: string;
begin
  TKSQLBuilder.CreateAndExecute(
    procedure (ASQLBuilder: TKSQLBuilder)
    begin
      LResult := ASQLBuilder.GetSortClause(Self, AIsDescending);
    end);
  Result := LResult;
end;

function TKViewField.CanEditField(const AInsertOperation: Boolean): Boolean;
var
  LReadOnly: Boolean;

  function CanEdit: Boolean;
  begin
    if AInsertOperation then
      Result := CanInsert
    else
      Result := CanUpdate
  end;

begin
  LReadOnly :=
    IsReadOnly
    or not IsAccessGranted(ACM_MODIFY)
    or not CanEdit
    or Table.IsReadOnly
    or (Model <> Table.Model);
  Result := not LReadOnly;
end;

function TKViewField.CreateDerivedFieldsStore(const AKeyValues: string): TKStore;
var
  LDerivedFields: TArray<TKViewField>;
  LStore: TKStore;
begin
  Assert(IsReference);

  LStore := TKStore.Create;
  try
    // Set header.
    LDerivedFields := GetDerivedFields;
    Assert(Length(LDerivedFields) > 0);

    LStore.DoWithChangeNotificationsDisabled(
      procedure
      var
        LDerivedField: TKViewField;
        LDBConnection: TEFDBConnection;
        LDBQuery: TEFDBQuery;
        LHasDerivedFields: Boolean;
      begin
        for LDerivedField in LDerivedFields do
          LStore.Header.AddField(LDerivedField.AliasedName).DataType := LDerivedField.DataType;
        // Get data.
        LDBConnection := TKConfig.Instance.CreateDBConnection(Table.DatabaseName);
        try
          LDBQuery := LDBConnection.CreateDBQuery;
          try
            TKSQLBuilder.CreateAndExecute(
              procedure (ASQLBuilder: TKSQLBuilder)
              begin
                LHasDerivedFields := ASQLBuilder.BuildDerivedSelectQuery(Self, LDBQuery, AKeyValues);
              end);
            if LHasDerivedFields then
              LStore.Load(LDBQuery, False, True);
          finally
            FreeAndNil(LDBQuery);
          end;
        finally
          FreeAndNil(LDBConnection);
        end;
      end);
    Result := LStore;
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
  LKeyFieldNames: string;
begin
  Assert(IsReference);

  Result := TKStore.Create;
  try
    for I := 0 to ModelField.ReferencedModel.KeyFieldCount - 1 do
    begin
      LField := ModelField.ReferencedModel.KeyFields[I];
      Result.Key.AddChild(LField.FieldName).DataType := LField.DataType;
      Result.Header.AddField(LField.FieldName).DataType := LField.DataType;
    end;
    LCaptionField := ModelField.ReferencedModel.CaptionField;
    if Result.Header.FindChild(LCaptionField.FieldName) = nil then
      Result.Header.AddField(LCaptionField.FieldName).DataType := LCaptionField.DataType;

    if ModelField.ReferencedModel.KeyFieldCount > 1 then
    begin
      // Let's assume a concatenation is a string.
      LKeyFieldNames := Join(ModelField.ReferencedModel.GetKeyFieldNames, TKConfig.Instance.MultiFieldSeparator);
      Result.Header.AddField(LKeyFieldNames).DataType := TEFDataTypeFactory.Instance.GetDataType('String');
    end;
  except
    FreeAndNil(Result);
    raise;
  end;
end;


function TKViewField.CreateReferencedModelStore(const AKeyValues: Variant): TKStore;
var
  LDBConnection: TEFDBConnection;
  LDBQuery: TEFDBQuery;
  LStore: TKStore;
begin
  Assert(IsReference);

  LStore := TKStore.Create;
  try
    // Metadata.
    ModelField.ReferencedModel.EnumPhysicalFields(
      procedure (AField: TKModelField)
      begin
        if AField.IsKey then
          LStore.Key.AddChild(AField.FieldName).DataType := AField.DataType;
        LStore.Header.AddField(AField.FieldName).DataType := AField.DataType;
      end);

    // Get data.
    LDBConnection := TKConfig.Instance.CreateDBConnection(Table.DatabaseName);
    try
      LDBQuery := LDBConnection.CreateDBQuery;
      try
        with TKSQLBuilder.Create do
          try
            BuildSingletonSelectQuery(ModelField.ReferencedModel, LDBQuery, AKeyValues);
          finally
            Free;
          end;
        LStore.Load(LDBQuery, False, True);
      finally
        FreeAndNil(LDBQuery);
      end;
    finally
      FreeAndNil(LDBConnection);
    end;
    Result := LStore;
  except
    FreeAndNil(LStore);
    raise;
  end;
end;

function TKViewField.DerivedFieldsExist: Boolean;
var
  LViewTable: TKViewTable;
  LModelField: TKModelField;
  I: Integer;
begin
  Result := False;
  LViewTable := Table;
  LModelField := ModelField;
  for I := 0 to LViewTable.FieldCount - 1 do
  begin
    if LViewTable.Fields[I].ReferenceField = LModelField then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

function TKViewField.FindModelField: TKModelField;
begin
  Result := Model.FindField(FieldName);
end;

function TKViewField.HasModelField: boolean;
var
  LModelField: TKModelField;
begin
  LModelField := Model.FindField(FieldName);
  Result := Assigned(LModelField);
end;

function TKViewField.HasServerSideRules: Boolean;
var
  LServerSideRuleFound: Boolean;
begin
  Result := (Rules.RuleCount > 0) or (ModelField.Rules.RuleCount > 0);
  if Result then
  begin
    LServerSideRuleFound := False;
    EnumRules(
      function (ARuleImpl: TKRuleImpl): Boolean
      begin
        LServerSideRuleFound := not ARuleImpl.IsClientSide;
        Result := not LServerSideRuleFound;
      end);
    Result := LServerSideRuleFound;
  end;
end;

function TKViewField.FindNode(const APath: string;
  const ACreateMissingNodes: Boolean): TEFNode;
begin
  Result := inherited FindNode(APath, ACreateMissingNodes);
  if not Assigned(Result) and HasModelField then
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

function TKViewField.GetAliasedDBName: string;
begin
  Result := Alias;
  if Result = '' then
    Result := ReplaceStr(DBName, '.', '_');
  if Result = '' then
    raise EKError.CreateFmt('ViewField %s must have an alias.', [Name]);
end;

function TKViewField.GetAliasedName: string;
begin
  Result := Alias;
  if Result = '' then
    Result := ReplaceStr(Name, '.', '_');
  if Result = '' then
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
    Result := ModelField.BlankValue
  else
    Result := LNode.AsBoolean;
end;

function TKViewField.GetAutoCompleteMinChars: Integer;
var
  LNode: TEFNode;
begin
  LNode := FindNode('AutoCompleteMinChars');
  if LNode = nil then
    Result := ModelField.AutoCompleteMinChars
  else
    Result := LNode.AsInteger;
end;

function TKViewField.GetCanInsert: Boolean;
var
  LNode: TEFNode;
begin
  LNode := FindNode('CanInsert');
  if LNode = nil then
  begin
    if pos('.',Name) > 0 then
      Result := False
    else
      Result := ModelField.CanInsert;
  end
  else
    Result := LNode.AsBoolean and ModelField.CanActuallyModify;
end;

function TKViewField.GetCanUpdate: Boolean;
var
  LNode: TEFNode;
begin
  LNode := FindNode('CanUpdate');
  if LNode = nil then
  begin
    if pos('.',Name) > 0 then
      Result := False
    else
      Result := ModelField.CanUpdate;
  end
  else
    Result := LNode.AsBoolean and ModelField.CanActuallyModify;
end;

function TKViewField.GetChildClass(const AName: string): TEFNodeClass;
begin
  if SameText(AName, 'Rules') then
    Result := TKRules
  else
    Result := inherited GetChildClass(AName);
end;

function TKViewField.GetColorsAsPairs: TEFPairs;
begin
  Result := GetChildrenAsPairs('Colors', True);
end;

function TKViewField.GetQualifiedAliasedDBName: string;
begin
  Result := Alias;
  if Result = '' then
    Result := ReplaceStr(QualifiedDBName, '.', '_');
  if Result = '' then
    raise EKError.CreateFmt('ViewField %s must have an alias.', [Name]);
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
    Result := DBName + '.' + ModelField.ReferencedModel.CaptionField.DBColumnNameOrExpression
  else if Expression <> '' then
    Result := Expression
  else
    Result := QualifiedDBName;
end;

function TKViewField.GetFieldType: TFieldType;
begin
  if IsReference then
    Result := ModelField.ReferencedModel.CaptionField.DataType.GetFieldType
  else
    Result := GetDataType.GetFieldType;
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

function TKViewField.GetDBName: string;
var
  LNameParts: TArray<string>;
begin
  if Name.Contains('.') then
  begin
    LNameParts := Name.Split(['.']);
    Result := LNameParts[High(LNameParts)];
  end
  else
    Result := ModelField.DBColumnName;
end;

function TKViewField.GetDBNameOrExpression: string;
begin
  if IsReference then
    Result := ModelField.ReferencedModel.CaptionField.DBColumnNameOrExpression
  else if Expression <> '' then
    Result := Expression
  else
    Result := DBName;
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

function TKViewField.GetDefaultFilter: string;
begin
  Result := GetString('DefaultFilter');
  if Result = '' then
    Result := ModelField.DefaultFilter;
end;

function TKViewField.GetDefaultFilterConnector: string;
begin
  Result := GetString('DefaultFilterConnector');
  if Result = '' then
    Result := ModelField.DefaultFilterConnector;
end;

function TKViewField.GetDefaultValue: Variant;
var
  LStringValue: string;
begin
  Result := EvalExpression(GetValue('DefaultValue'));
  if VarIsNull(Result) then
    Result := ModelField.DefaultValue;
  if DataType is TEFStringDataType then
  begin
    LStringValue := Result;
    TEFMacroExpansionEngine.Instance.Expand(LStringValue);
    Result := LStringValue;
  end;
end;

function TKViewField.GetDerivedFields: TArray<TKViewField>;
begin
  Assert(IsReference);

  Result := Table.GetFieldArray(
    function (AField: TKViewField): Boolean
    begin
      Result := (AField = Self) or (AField.ReferenceName = FieldName);
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
    Result := ModelField.DisplayTemplate
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
  LNameParts: TArray<string>;
begin
  LNameParts := Name.Split(['.']);
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

function TKViewField.GetFilterByFields: TArray<TKFilterByViewField>;
var
  I: Integer;
  LNode: TEFNode;
  LModelFieldPairs: TArray<TKFilterByField>;
  LModelFields: TKModelFieldArray;
  LViewFields: TKViewFieldArray;
begin
  SetLength(Result, 0);
  LNode := FindNode('FilterBy');
  if Assigned(LNode) then
  begin
    SetLength(Result, LNode.ChildCount);
    for I := Low(Result) to High(Result) do
    begin
      Result[I].DestinationField := Self;
      Result[I].SourceField := Table.FieldByName(LNode[I].Name);
      Result[I].ForeignFieldName := LNode[I].AsString;
    end;
  end
  else
  begin
    // Convert modelfield-level definition to viewfield-level.
    LModelFieldPairs := ModelField.GetFilterByFields;
    SetLength(LModelFields, Length(LModelFieldPairs));
    for I := Low(LModelFields) to High(LModelFields) do
      LModelFields[I] := LModelFieldPairs[I].Key;
    LViewFields := Table.FieldsByModelFields(LModelFields);
    SetLength(Result, Length(LViewFields));
    for I := Low(Result) to High(Result) do
    begin
      Result[I].DestinationField := Self;
      Result[I].SourceField := LViewFields[I];
      Result[I].ForeignFieldName := LModelFieldPairs[I].Value;
    end;
  end;
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

function TKViewField.GetLookupFilter: string;
begin
  Result := GetString('LookupFilter');
  if Result = '' then
    Result := ModelField.LookupFilter;
end;

function TKViewField.GetQualifiedDBName: string;
var
  LReferencedModelName: string;
  LReferenceField: TKViewField;
  LNameParts: TArray<string>;
begin
  LNameParts := Name.Split(['.']);
  if Length(LNameParts) > 1 then
  begin
    LReferencedModelName := LNameParts[0];
    LReferenceField := Table.FieldByName(LReferencedModelName);
    if LReferenceField.IsReference then
      Result := LReferenceField.ModelField.DBColumnName + '.' + ModelField.DBColumnName
    else
      Result := Name;
  end
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
  Result := (ModelName = Table.ModelName) and HasModelField and ModelField.IsKey;
end;

function TKViewField.GetIsPassword: Boolean;
begin
  Result := GetBoolean('IsPassword');
end;

function TKViewField.GetIsPicture: Boolean;
begin
  Result := GetBoolean('IsPicture');
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

function TKViewField.GetURLFieldName: string;
begin
  Result := URL_PREFIX + AliasedName;
end;

function TKViewField.IsAccessGranted(const AMode: string): Boolean;
begin
  Result := TKAccessController.Current.IsAccessGranted(TKAuthenticator.Current.UserName, GetACURI, AMode)
    and TKAccessController.Current.IsAccessGranted(TKAuthenticator.Current.UserName, ModelField.GetACURI, AMode);
end;

class function TKViewField.IsURLFieldName(const AFieldName: string): Boolean;
begin
  Result := AFieldName.StartsWith(URL_PREFIX);
end;

function TKViewField.GetReferenceName: string;
var
  LNameParts: TArray<string>;
begin
  LNameParts := Name.Split(['.']);
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
  LNameParts: TArray<string>;
  LReferencedModel: TKModel;
begin
  LNameParts := Name.Split(['.']);
  if Length(LNameParts) = 1 then
    // <field name>
    Result := Table.ModelName
  else if Length(LNameParts) = 2 then
  begin
    // <reference name>.<field name>
    LReferencedModel := Table.Model.FieldByName(LNameParts[0]).ReferencedModel;
    if Assigned(LReferencedModel) then
      Result := LReferencedModel.ModelName
    else
      Result := '';
  end;
  if Result = '' then
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

function TKViewTableStore.AppendRecord(const AValues: TEFNode): TKViewTableRecord;
begin
  Result := inherited AppendRecord(AValues) as TKViewTableRecord;
  // The above will cause InternalAfterReadFromNode to be called, which
  // in turn will call SetDetailFieldValues itself, but only if there are
  // actual values. Otherwise we must set default values explicitly.
  if Assigned(FMasterRecord) and not Assigned(AValues) then
    Result.SetDetailFieldValues(FMasterRecord);
end;

constructor TKViewTableStore.Create(const AViewTable: TKViewTable);
begin
  Assert(Assigned(AViewTable));

  inherited Create;
  FViewTable := AViewTable;
  SetupFields;
end;

function TKViewTableStore.FindRecord(const AKey: TEFTree;
  const AFormatSettings: TFormatSettings;
  const AValueIndex: Integer): TKViewTableRecord;
begin
  Result := inherited FindRecord(AKey, AFormatSettings,
    function(const AName: string): string
    var
      LField: TKViewField;
    begin
      LField := ViewTable.FindField(AName);
      if Assigned(LField) then
        Result := LField.AliasedName
      else
        Result := AName;
    end,
    AValueIndex) as TKViewTableRecord;
end;

procedure TKViewTableStore.SetupFields;
var
  LViewFieldIndex: Integer;
  LViewField: TKViewField;
  LFieldNames: string;
  LModelFieldIndex: Integer;
  LModelField: TKModelField;

  procedure SetupField(const AViewField: TKViewField; const AName: string;
    const ADataType: TEFDataType; const AIsKey, AIsAccessGranted: Boolean;
    const AModelField: TKModelField);
  begin
    Assert(Assigned(AViewField));

    if AIsAccessGranted or AIsKey then
    begin
      // Set field names and data types both in key and header.
      if AIsKey and not Assigned(Key.FindChild(AName)) then
        Key.AddChild(AName).DataType := ADataType;
      if not Assigned(Header.FindChild(AName)) then
        Header.AddField(AName).SetViewField(AViewField, ADataType).SetModelField(AModelField);
    end;
  end;

begin
  for LViewFieldIndex := 0 to FViewTable.FieldCount - 1 do
  begin
    LViewField := FViewTable.Fields[LViewFieldIndex];
    SetupField(LViewField, LViewField.AliasedName, LViewField.DataType,
      LViewField.IsKey and not LViewField.IsReference, LViewField.IsAccessGranted(ACM_READ),
      LViewField.ModelField);
    // URL fields for datatypes that provide downloadable content, such as images.
    if LViewField.IsPicture then
      SetupField(LViewField, LViewField.GetURLFieldName,
        TEFDataTypeFactory.Instance.GetDataType('String'),
        False, LViewField.IsAccessGranted(ACM_READ), LViewField.ModelField);
    // Expand reference fields. Also keep the reference field itself (above)
    // as it will hold the user-readable reference description.
    // For each reference field we create:
    // 1) the reference field itself (see above);
    // 2) a field for each field that is part of the reference (see below);
    // 3) a field that is the concatenation of all fields at (2).
    if LViewField.IsReference then
    begin
      Assert(LViewField.ModelField.FieldCount > 0);

      for LModelFieldIndex := 0 to LViewField.ModelField.FieldCount - 1 do
      begin
        LModelField := LViewField.ModelField.Fields[LModelFieldIndex];
        SetupField(LViewField, LModelField.FieldName, LModelField.DataType,
          LModelField.IsKey and not LModelField.IsReference, LModelField.IsAccessGranted(ACM_READ),
          LModelField);
      end;

      // Let's assume a concatenation is a string.
      // By convention, we only use the first field for AC.
      LFieldNames := Join(LViewField.ModelField.GetFieldNames, TKConfig.Instance.MultiFieldSeparator);
      SetupField(LViewField, LFieldNames, TEFDataTypeFactory.Instance.GetDataType('String'),
        False, LViewField.ModelField.Fields[0].IsAccessGranted(ACM_READ), LViewField.ModelField);
    end;
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

function TKViewTableStore.GetHeader: TKViewTableHeader;
begin
  Result := inherited Header as TKViewTableHeader;
end;

function TKViewTableStore.GetRecord(const AKey: TEFTree; const AFormatSettings: TFormatSettings;
  const AValueIndex: Integer): TKViewTableRecord;
begin
  Result := inherited GetRecord(AKey, AFormatSettings,
    function(const AName: string): string
    var
      LField: TKViewField;
    begin
      LField := ViewTable.FindField(AName);
      if Assigned(LField) then
        Result := LField.AliasedName
      else
        Result := AName;
    end,
    AValueIndex) as TKViewTableRecord;
end;

function TKViewTableStore.GetRecords: TKViewTableRecords;
begin
  Result := inherited Records as TKViewTableRecords;
end;

function TKViewTableStore.Load(const AFilter, ASort: string;
  const AFrom, AFor: Integer; const AForEachRecord: TProc<TKVIewTableRecord>): Integer;
var
  LDBConnection: TEFDBConnection;
  LDBQuery: TEFDBQuery;
begin
  Assert(Assigned(FViewTable));

  LDBConnection := TKConfig.Instance.CreateDBConnection(ViewTable.DatabaseName);
  try
    LDBQuery := LDBConnection.CreateDBQuery;
    try
      if (AFrom = 0) and (AFor = 0) then
      begin
        TKSQLBuilder.CreateAndExecute(
          procedure (ASQLBuilder: TKSQLBuilder)
          begin
            ASQLBuilder.BuildSelectQuery(FViewTable, AFilter, ASort, LDBQuery, FMasterRecord);
          end);
        inherited Load(LDBQuery, False, False,
          procedure (ARecord: Kitto.Store.TKRecord)
          begin
            if Assigned(AForEachRecord) then
              AForEachRecord(ARecord as TKViewTableRecord);
          end);
        Result := RecordCount;
      end
      else
      begin
        TKSQLBuilder.CreateAndExecute(
          procedure (ASQLBuilder: TKSQLBuilder)
          begin
            ASQLBuilder.BuildCountQuery(FViewTable, AFilter, LDBQuery, FMasterRecord);
          end);
        LDBQuery.Open;
        try
          Result := LDBQuery.DataSet.Fields[0].AsInteger;
        finally
          LDBQuery.Close;
        end;
        TKSQLBuilder.CreateAndExecute(
          procedure (ASQLBuilder: TKSQLBuilder)
          begin
            ASQLBuilder.BuildSelectQuery(FViewTable, AFilter, ASort, LDBQuery, FMasterRecord, AFrom, AFor);
          end);
        inherited Load(LDBQuery, False, False,
          procedure (ARecord: Kitto.Store.TKRecord)
          begin
            if Assigned(AForEachRecord) then
              AForEachRecord(ARecord as TKViewTableRecord);
          end);
      end;
    finally
      FreeAndNil(LDBQuery);
    end;
  finally
    FreeAndNil(LDBConnection);
  end;
end;

{ TKViewTableHeader }

function TKViewTableHeader.AddField(const AFieldName: string): TKViewTableHeaderField;
begin
  Result := inherited AddField(AFieldName) as TKViewTableHeaderField;
end;

function TKViewTableHeader.FieldByName(const AFieldName: string): TKViewTableHeaderField;
begin
  Result := inherited FieldByName(AFieldName) as TKViewTableHeaderField;
end;

function TKViewTableHeader.FindField(const AFieldName: string): TKViewTableHeaderField;
begin
  Result := inherited FindField(AFieldName) as TKViewTableHeaderField;
end;

function TKViewTableHeader.GetChildClass(const AName: string): TEFNodeClass;
begin
  Result := TKViewTableHeaderField;
end;

function TKViewTableHeader.GetField(I: Integer): TKViewTableHeaderField;
begin
  Result := inherited Fields[I] as TKViewTableHeaderField;
end;

{ TKViewTableRecords }

function TKViewTableRecords.Append: TKViewTableRecord;
begin
  Result := inherited Append as TKViewTableRecord;
end;

function TKViewTableRecords.AppendAndInitialize: TKViewTableRecord;
begin
  Result := inherited AppendAndInitialize as TKViewTableRecord;
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

function TKViewTableRecords.GetXMLTagName: string;
begin
  Result := Store.ViewTable.Model.PluralModelName;
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

procedure TKViewTableRecord.ApplyEditRecordRules;
begin
  ViewTable.ApplyRules(
    procedure (ARuleImpl: TKRuleImpl)
    begin
      ARuleImpl.EditRecord(Self);
    end);
end;

procedure TKViewTableRecord.ApplyAfterShowEditWindowRules;
begin
  ViewTable.ApplyRules(
    procedure (ARuleImpl: TKRuleImpl)
    begin
      ARuleImpl.AfterShowEditWindow(Self);
    end);
end;

procedure TKViewTableRecord.ApplyDuplicateRecordRules;
begin
  ViewTable.ApplyRules(
    procedure (ARuleImpl: TKRuleImpl)
    begin
      ARuleImpl.DuplicateRecord(Self);
    end);
end;

procedure TKViewTableRecord.ApplyNewRecordRules;
begin
  ViewTable.ApplyRules(
    procedure (ARuleImpl: TKRuleImpl)
    begin
      ARuleImpl.NewRecord(Self);
    end);
  MarkAsNew;
end;

procedure TKViewTableRecord.ApplyNewRecordRulesAndFireEvents(const AViewTable: TKViewTable; const AIsCloned: Boolean);
begin
  Assert(Assigned(AViewTable));

  AViewTable.Model.BeforeNewRecord(Self, AIsCloned);
  Self.ApplyNewRecordRules;
  AViewTable.Model.AfterNewRecord(Self);
end;

procedure TKViewTableRecord.LoadDetailStores;
var
  I: Integer;
  LRecordIndex: Integer;
begin
  EnsureDetailStores;
  for I := 0 to DetailStoreCount - 1 do
  begin
    DetailStores[I].Load;
    for LRecordIndex := 0 to DetailStores[I].RecordCount - 1 do
      DetailStores[I].Records[LRecordIndex].LoadDetailStores;
  end;
end;

procedure TKViewTableRecord.Refresh(const AStrict: Boolean = False);
var
  LDBConnection: TEFDBConnection;
  LDBQuery: TEFDBQuery;
begin
  Assert(Assigned(ViewTable));

  LDBConnection := TKConfig.Instance.CreateDBConnection(ViewTable.DatabaseName);
  try
    LDBQuery := LDBConnection.CreateDBQuery;
    try
      TKSQLBuilder.CreateAndExecute(
        procedure (ASQLBuilder: TKSQLBuilder)
        begin
          ASQLBuilder.BuildSingletonSelectQuery(ViewTable, LDBQuery, GetFieldValues(Store.ViewTable.Model.GetKeyFieldNames));
        end);
      LDBQuery.Open;
      if AStrict and LDBQuery.DataSet.IsEmpty then
        raise Exception.Create('Record not found');
      if not LDBQuery.DataSet.IsEmpty then
        ReadFromFields(LDBQuery.DataSet.Fields);
      MarkAsClean;
    finally
      FreeAndNil(LDBQuery);
    end;
  finally
    FreeAndNil(LDBConnection);
  end;
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

procedure TKViewTableRecord.FieldChanged(const AField: TKField; const AOldValue, ANewValue: Variant);
var
  LField: TKViewTableField;
  LStore: TKStore;
  I: Integer;
  LDerivedField: TKViewTableField;
  LViewField: TKViewField;
  LFieldNames: TArray<string>;
  LFieldValues: TArray<string>;
  LFilteredByFields: TArray<TKFilterByViewField>;
begin
  Assert(AField is TKViewTableField);

  LField := TKViewTableField(AField);
  LViewField := LField.ViewField;

  // Forward the changed value to the multi-valued field constituent parts.
  if LField.IsCompositeField then
  begin
    LFieldNames := LField.FieldName.Split([TKConfig.Instance.MultiFieldSeparator], TStringSplitOptions.None);
    // Test for null and '' as well, since compound fields are always treated as
    // strings and never set to null.
    if VarIsNull(ANewValue) or (ANewValue = '') then
    begin
      for I := Low(LFieldNames) to High(LFieldNames) do
        FieldByName(LFieldNames[I]).SetToNull;
    end
    else
    begin
      LFieldValues := string(ANewValue).Split([TKConfig.Instance.MultiFieldSeparator], TStringSplitOptions.None);
      Assert(Length(LFieldNames) = Length(LFieldValues));
      for I := Low(LFieldNames) to High(LFieldNames) do
        FieldByName(LFieldNames[I]).Value := LFieldValues[I];
    end;
  end;

  // See if LField is part of a reference. LField.ViewField gets a reference
  // to the reference ViewField, if any, or the corresponding ViewField otherwise.
  if LViewField.IsReference and not LField.IsPartOfCompositeField then
  begin
    if LField.FieldName <> LViewField.AliasedName then
    begin
      // Avoid re-entry.
      if FReferenceViewFieldBeingChanged = LViewField then
        Exit;

      FReferenceViewFieldBeingChanged := LViewField;
      try
        // Get derived values.
        { TODO : Optimization: If ANewValue is null or unassigned, you don't even need to create the store and query the database. }
        LStore := LViewField.CreateDerivedFieldsStore(EFVarToStr(ANewValue));
        try
          // Copy values to fields.
          for I := 0 to LStore.Header.FieldCount - 1 do
          begin
            LDerivedField := FindField(TranslateFieldName(LStore.Header.Fields[I].FieldName));
            if Assigned(LDerivedField) then
            begin
              if LStore.RecordCount > 0 then
                LDerivedField.AssignValue(LStore.Records[0].Fields[I])
              else
                LDerivedField.SetToNull;
            end;
          end;
          LField.ViewField.EnumRules(
            function (ARuleImpl: TKRuleImpl): Boolean
            begin
              ARuleImpl.AfterRefreshReferenceField(LField);
              Result := True;
            end);
        finally
          FreeAndNil(LStore);
        end;
      finally
        FReferenceViewFieldBeingChanged := nil;
      end;
    end;
    // Clear filtered-by fields only when changing the view field itself (not
    // any underlying real fields).
    if LField.FieldName = LViewField.AliasedName then
    begin
      LFilteredByFields := LViewField.Table.GetFilterByFields(
        function (AFilterByViewField: TKFilterByViewField): Boolean
        begin
          Result := AFilterByViewField.SourceField = LViewField;
          if Result then
          begin
            // Select all destination fields and clear them, forcing the change
            // event since we need it to update the UI.
            EnumChildren(
              procedure (const ANode: TEFNode)
              begin
                ANode.SetToNull(True);
              end,
              function (const ANode: TEFNode): Boolean
              begin
                Result := TKVIewTableField(ANode).ViewField = AFilterByViewField.DestinationField;
              end
            );
          end;
        end);
    end;
  end;
  inherited;
end;

procedure TKViewTableRecord.FieldChanging(const AField: TKField;
  const AOldValue: Variant; var ANewValue: Variant; var ADoIt: Boolean);
var
  LField: TKViewTableField;
  LOldValue: Variant;
  LNewValue: Variant;
  LDoIt: Boolean;
begin
  inherited;
  Assert(AField is TKViewTableField);

  LField := TKViewTableField(AField);
  LOldValue := AOldValue;
  LNewValue := ANewValue;
  LDoIt := ADoIt;
  LField.ViewField.EnumRules(
    function (ARuleImpl: TKRuleImpl): Boolean
    begin
      ARuleImpl.BeforeFieldChange(AField, LOldValue, LNewValue, LDoIt);
      Result := True;
    end);
  ADoIt := LDoIt;
  if ADoIt then
    ANewValue := LNewValue;
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

function TKViewTableRecord.GetXMLTagName: string;
begin
  Result := ViewTable.ModelName;
end;

procedure TKViewTableRecord.HandleDeleteFileInstructions;
begin
  EnumFields(
    function (AField: TKField): Boolean
    var
      LFileToDelete: string;
    begin
      if (AField.DataType is TKFileReferenceDataType) and (AField.IsNull) then
      begin
        LFileToDelete := AField.GetString('Sys/DeleteFile');
        if FileExists(LFileToDelete) then
          TFile.Delete(LFileToDelete);
      end;
      Result := True;
    end
  );
end;

procedure TKViewTableRecord.InternalAfterReadFromNode;
begin
  inherited;
  if Records.Store.MasterRecord <> nil then
    SetDetailFieldValues(Records.Store.MasterRecord);
end;

procedure TKViewTableRecord.ExpandExpression(var AExpression: string);
var
  I: Integer;
  LField: TKViewTableField;
begin
  inherited ExpandExpression(AExpression);
  if Assigned(Store) and Assigned(Store.MasterRecord) then
  begin
    for I := 0 to Store.MasterRecord.FieldCount - 1 do
    begin
      LField := Store.MasterRecord.Fields[I];
      ReplaceAllCaseSensitive(AExpression, Format('{MasterRecord.%s}',[LField.FieldName]), LField.AsString);
    end;
  end;
end;

procedure TKViewTableRecord.ExpandFieldJSONValues(var AText: string;
  const AEmptyNulls: Boolean; const ASender: TKViewField = nil);
var
  I: Integer;
  LField: TKViewTableField;
begin
  for I := 0 to FieldCount - 1 do
  begin
    LField := Fields[I];
    if LField.ViewField <> ASender then
    begin
      ReplaceAllCaseSensitive(AText, '{' + LField.FieldName + '}', LField.GetAsJSONValue(True, False, True));
    end;
  end;
end;

procedure TKViewTableRecord.SetDetailFieldValues(const AMasterRecord: TKViewTableRecord);
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

function TKViewTableRecord.TranslateFieldName(const AFieldName: string): string;
var
  LViewField: TKViewField;
  LFieldIndex: Integer;
begin
  LViewField := FieldByName(AFieldName).ViewField;
  if LViewField.IsReference then
  begin
    LFieldIndex := IndexStr(AFieldName, LViewField.ModelField.GetFieldNames);
    if LFieldIndex >= 0 then
      Result := LViewField.ModelField.Fields[LFieldIndex].DBColumnName
    else
      Result := inherited TranslateFieldName(AFieldName);
  end
  else
    Result := LViewField.AliasedDBName;
end;

function TKViewTableRecord.FindDetailStoreByModelName(const AModelName: string;
  const AForceLoad: Boolean): TKViewTableStore;
var
  I: Integer;
  LDetailTableStore: TKViewTableStore;
begin
  Result := nil;
  for I := 0 to DetailStoreCount -1 do
  begin
    LDetailTableStore := DetailStores[I];
    if SameText(LDetailTableStore.ViewTable.ModelName, AModelName) then
    begin
      Result := LDetailTableStore;
      if (Result.RecordCount = 0) or AForceLoad then
        Result.Load;
      Break;
    end;
  end;
end;

function TKViewTableRecord.GetDetailStoreByModelName(const AModelName: string;
  const AForceLoad: Boolean): TKViewTableStore;
begin
  Result := FindDetailStoreByModelName(AModelName, AForceLoad);
  if not Assigned(Result) then
    raise EKError.CreateFmt(_('DetailStore %s not found.'), [AModelName]);
end;

{ TKViewTableField }

function TKViewTableField.GetAsJSONValue(const AForDisplay: Boolean; const AQuote: Boolean;
  const AEmptyNulls: Boolean): string;
const
  PASSWORD_CHARS = 8;
var
  LDisplayTemplate: string;
  LViewField: TKViewField;
begin
  Result := inherited GetAsJSONValue(AForDisplay, False, AEmptyNulls);
  LViewField := ViewField;
  if AForDisplay and Assigned(LViewField) then
  begin
    // Use value of IsPassword cached in the header for best speed.
    if HeaderField.ViewFieldIsPassword then
      Result := DupeString('*', Min(LViewField.DisplayWidth, PASSWORD_CHARS))
    else
    begin
      // Use value of DisplayTemplate cached in the header for best speed.
      LDisplayTemplate := HeaderField.ViewFieldDisplayTemplate;
      if LDisplayTemplate <> '' then
      begin
        // Replace other field values, this field's value and add back quotes.
        ReplaceAllCaseSensitive(LDisplayTemplate, '{value}', Result);
        ParentRecord.ExpandFieldJSONValues(LDisplayTemplate, AEmptyNulls, LViewField);
        Result := LDisplayTemplate;
      end;
    end;
  end;
  if AQuote and not SameText(Result, 'null') then
    Result := QuoteJSONStr(Result);
end;

function TKViewTableField.GetDecimalPrecision: Integer;
begin
  Result := ViewField.DecimalPrecision;
end;

function TKViewTableField.GetEmptyAsNull: Boolean;
begin
  Result := ViewField.EmptyAsNull;
end;

function TKViewTableField.GetHeaderField: TKViewTableHeaderField;
begin
  Result := inherited HeaderField as TKViewTableHeaderField;
end;

function TKViewTableField.GetModelField: TKModelField;
begin
  Result := HeaderField.ModelField;
end;

function TKViewTableField.GetParentRecord: TKViewTableRecord;
begin
  Result := inherited ParentRecord as TKViewTableRecord;
end;

function TKViewTableField.GetViewField: TKViewField;
begin
  Result := HeaderField.ViewField;
end;

function TKViewTableField.IsPhysicalPartOfReference: Boolean;
begin
  Result := ViewField.IsReference and (ModelField <> ViewField.ModelField);
end;

{ TKFileReferenceDataType }

class function TKFileReferenceDataType.GetTypeName: string;
begin
  Result := 'FileReference';
end;

{ TKViewTableHeaderField }

function TKViewTableHeaderField.SetModelField(const AValue: TKModelField): TKViewTableHeaderField;
begin
  FModelField := AValue;
  Result := Self;
end;

function TKViewTableHeaderField.SetViewField(const AValue: TKViewField; const AOverrideDataType: TEFDataType = nil): TKViewTableHeaderField;
begin
  FViewField := AValue;
  if Assigned(FViewField) then
  begin
    FViewFieldIsPassword := FViewField.IsPassword;
    FViewFieldDisplayTemplate := FViewField.DisplayTemplate;
    if AOverrideDataType = nil then
      DataType := FViewField.DataType
    else
      DataType := AOverrideDataType
  end
  else
  begin
    FViewFieldIsPassword := False;
    FViewFieldDisplayTemplate := '';
    DataType := nil;
  end;
  Result := Self;
end;

{ TKHTMLMemoDataType }

class function TKHTMLMemoDataType.GetTypeName: string;
begin
  Result := 'HTMLMemo';
end;

initialization
  TKViewRegistry.Instance.RegisterClass(TKMetadata.SYS_PREFIX + 'Data', TKDataView);
  TEFDataTypeRegistry.Instance.RegisterClass(TKFileReferenceDataType.GetTypeName, TKFileReferenceDataType);
  TEFDataTypeRegistry.Instance.RegisterClass(TKHTMLMemoDataType.GetTypeName, TKHTMLMemoDataType);

finalization
  // For some reason, the unit defining TKViewRegistry is finalized before this one.
  // I need to avlid creating TKViewRegistry.Instance at this point.
  if TKViewRegistry.HasInstance then
    TKViewRegistry.Instance.UnregisterClass(TKMetadata.SYS_PREFIX + 'Data');
  TEFDataTypeRegistry.Instance.UnregisterClass(TKFileReferenceDataType.GetTypeName);
  TEFDataTypeRegistry.Instance.UnregisterClass(TKHTMLMemoDataType.GetTypeName);

end.
