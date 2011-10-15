unit Kitto.Metadata.Views;

interface

uses
  Types,
  EF.Classes, EF.Types, EF.Tree,
  Kitto.Metadata, Kitto.Metadata.Models, Kitto.Store;

type
  TKViews = class;

  TKView = class(TKMetadata)
  private
    FViews: TKViews;
    function GetControllerType: string;
    function GetImageName: string;
  protected
    function GetChildClass(const AName: string): TEFNodeClass; override;
    function GetDisplayLabel: string; virtual;
  public
    property Catalog: TKViews read FViews;

    property DisplayLabel: string read GetDisplayLabel;
    property ImageName: string read GetImageName;

    property ControllerType: string read GetControllerType;
  end;

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
    function GetQualifiedName: string;
    function GetModelName: string;
    function GetFieldName: string;
    function GetEmptyAsNull: Boolean;
    function GetDefaultValue: Variant;
    function GetModel: TKModel;
    function GetExpression: string;
    function GetAlias: string;
    function GetQualifiedAliasedNameOrExpression: string;
    function GetIsKey: Boolean;
    function GetSize: Integer;
    function GetIsBlob: Boolean;
    function GetReference: TKModelReference;
    function GetAllowedValues: TEFPairs;
    function GetRules: TKRules;
    function GetDecimalPrecision: Integer;
  protected
    function GetChildClass(const AName: string): TEFNodeClass; override;
  public
    function FindNode(const APath: string; const ACreateMissingNodes: Boolean = False): TEFNode; override;

    property Table: TKViewTable read GetTable;
    property Model: TKModel read GetModel;
    property ModelField: TKModelField read GetModelField;
    property Alias: string read GetAlias;
    property AliasedName: string read GetAliasedName;
    property QualifiedAliasedNameOrExpression: string read GetQualifiedAliasedNameOrExpression;
    property QualifiedName: string read GetQualifiedName;
    property AllowedValues: TEFPairs read GetAllowedValues;

    ///	<summary>If the view field is referenced, returns its reference,
    ///	otherwise returns nil.</summary>
    property Reference: TKModelReference read GetReference;

    ///	<summary>
    ///	  Extract and returns the model name from the Name. If no model name is
    ///	  specified (because the field is part of the main model), returns the
    ///	  main model name.
    ///	</summary>
    property ModelName: string read GetModelName;

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
    property DisplayWidth: Integer read GetDisplayWidth;
    property DecimalPrecision: Integer read GetDecimalPrecision;
    property DataType: TEFDataType read GetDataType;
    property Size: Integer read GetSize;
    property IsBlob: Boolean read GetIsBlob;

    ///	<summary>Creates a store with the current field and all key fields of
    ///	the referenced model. If reference = nil, an exception is
    ///	raised.</summary>
    function CreateStore: TKStore;

    property Rules: TKRules read GetRules;
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
  end;

  TKDataView = class;

  TKLayouts = class;

  TKLayout = class(TKMetadata)
  private
    FLayouts: TKLayouts;
  end;

  TKViewTable = class(TKMetadataItem)
  private
    function GetIsDetail: Boolean;
    function GetField(I: Integer): TKViewField;
    function GetFieldCount: Integer;
    function GetModelName: string;
    function GetModel: TKModel;
    function GetDetailTableCount: Integer;
    function GetTable(I: Integer): TKViewTable;
    function GetDisplayLabel: string;
    function GetPluralDisplayLabel: string;
    function GetIsReadOnly: Boolean;
    function GetMasterTable: TKViewTable;
    function GetDefaultSorting: string;
    function GetDefaultFilter: string;
    function GetView: TKDataView;
    function GetRules: TKRules;
  protected
    function GetChildClass(const AName: string): TEFNodeClass; override;
    function GetFields: TKViewFields;
    function GetDetailTables: TKViewTables;
  public
    property ModelName: string read GetModelName;

    property IsDetail: Boolean read GetIsDetail;
    property MasterTable: TKViewTable read GetMasterTable;

    property DisplayLabel: string read GetDisplayLabel;
    property PluralDisplayLabel: string read GetPluralDisplayLabel;

    property Model: TKModel read GetModel;

    property FieldCount: Integer read GetFieldCount;
    property Fields[I: Integer]: TKViewField read GetField;
    function GetFieldNames: TStringDynArray;
    function FindField(const AName: string): TKViewField;
    function FieldByName(const AName: string): TKViewField;
    function FieldByAliasedName(const AName: string): TKViewField;
    function GetKeyFieldAliasedNames: TStringDynArray;
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
    property DetailTables[I: Integer]: TKViewTable read GetTable;
    function DetailTableByName(const AName: string): TKViewTable;

    property View: TKDataView read GetView;

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
    function CreateStore: TKStore;

    ///	<summary>Creates and returns a node with one child for each default
    ///	value as specified in the view table or model. Any default expression
    ///	is evaluated at this time.</summary>
    ///	<remarks>The caller is responsible for freeing the returned node
    ///	object.</remarks>
    function GetDefaultValues: TEFNode;

    function GetResourceURI: string; override;

    function IsAccessGranted(const AMode: string): Boolean; override;

    property Rules: TKRules read GetRules;
  end;

  TKDataView = class(TKView)
  private
    function GetMainTable: TKViewTable;
  protected
    function GetChildClass(const AName: string): TEFNodeClass; override;
    function GetDisplayLabel: string; override;
  public
    property MainTable: TKViewTable read GetMainTable;
  end;

  ///	<summary>
  ///	  A catalog of views.
  ///	</summary>
  TKViews = class(TKMetadataCatalog)
  private
    FLayouts: TKLayouts;
    function GetLayouts: TKLayouts;
    function BuildView(const ANode: TEFNode;
      const AViewBuilderName: string): TKView;
  protected
    procedure AfterCreateObject(const AObject: TKMetadata); override;
    function GetObjectClassType: TKMetadataClass; override;
    procedure SetPath(const AValue: string); override;
  public
    destructor Destroy; override;
  public
    function ViewByName(const AName: string): TKView;
    function FindView(const AName: string): TKView;

    function ViewByNode(const ANode: TEFNode): TKView;
    function FindViewByNode(const ANode: TEFNode): TKView;

    property Layouts: TKLayouts read GetLayouts;
    procedure Open; override;
    procedure Close; override;
  end;

  ///	<summary>
  ///	  A catalog of layouts. Internally used by the catalog of views.
  ///	</summary>
  TKLayouts = class(TKMetadataCatalog)
  protected
    procedure AfterCreateObject(const AObject: TKMetadata); override;
    function GetObjectClassType: TKMetadataClass; override;
  public
    function LayoutByName(const AName: string): TKLayout;
    function FindLayout(const AName: string): TKLayout;
  end;

  ///	<summary>
  ///	  A view that executes an action.
  ///	</summary>
  TKActionView = class(TKView)

  end;

  ///	<summary>The type of nodes in a tree view.</summary>
  TKTreeViewNode = class(TEFNode)
  private
    function GetTreeViewNodeCount: Integer;
    function GetTreeViewNode(I: Integer): TKTreeViewNode;
  protected
    function GetChildClass(const AName: string): TEFNodeClass; override;
  public
    property TreeViewNodeCount: Integer read GetTreeViewNodeCount;
    property TreeViewNodes[I: Integer]: TKTreeViewNode read GetTreeViewNode;
  end;

  ///	<summary>A node in a tree view that is a folder (i.e. contains other
  ///	nodes and doesn't represent a view).</summary>
  TKTreeViewFolder = class(TKTreeViewNode);

  ///	<summary>
  ///	  A view that is a tree of views. Contains views and folders, which
  ///  in turn contain views.
  ///	</summary>
  TKTreeView = class(TKView)
  private
    function GetTreeViewNode(I: Integer): TKTreeViewNode;
    function GetTreeViewNodeCount: Integer;
  protected
    function GetChildClass(const AName: string): TEFNodeClass; override;
  public
    property TreeViewNodeCount: Integer read GetTreeViewNodeCount;
    property TreeViewNodes[I: Integer]: TKTreeViewNode read GetTreeViewNode;
  end;

  TKViewBuilder = class(TKMetadata)
  public
    function BuildView: TKView; virtual; abstract;
  end;

  TKViewBuilderClass = class of TKViewBuilder;

  TKViewBuilderRegistry = class(TEFRegistry)
  private
    class var FInstance: TKViewBuilderRegistry;
    class function GetInstance: TKViewBuilderRegistry; static;
    class destructor Destroy;
  public
    class property Instance: TKViewBuilderRegistry read GetInstance;
    function GetClass(const AId: string): TKViewBuilderClass;
  end;

  TKViewBuilderFactory = class(TEFFactory)
  private
    class var FInstance: TKViewBuilderFactory;
    class function GetInstance: TKViewBuilderFactory; static;
  protected
    function DoCreateObject(const AClass: TClass): TObject; override;
  public
    class destructor Destroy;
  public
    class property Instance: TKViewBuilderFactory read GetInstance;

    function CreateObject(const AId: string): TKViewBuilder; reintroduce;
  end;

implementation

uses
  SysUtils, StrUtils, Variants,
  EF.StrUtils,
  Kitto.Types, Kitto.Environment;

{ TKViews }

procedure TKViews.AfterCreateObject(const AObject: TKMetadata);
begin
  inherited;
  (AObject as TKView).FViews := Self;
end;

procedure TKViews.Close;
begin
  inherited;
  if Assigned(FLayouts) then
    FLayouts.Close;
end;

destructor TKViews.Destroy;
begin
  FreeAndNil(FLayouts);
  inherited;
end;

function TKViews.FindView(const AName: string): TKView;
begin
  Result := FindObject(AName) as TKView;
end;

function TKViews.FindViewByNode(const ANode: TEFNode): TKView;
var
  LWords: TStringDynArray;
begin
  if Assigned(ANode) then
  begin
    LWords := Split(ANode.AsString);
    if Length(LWords) >= 2 then
    begin
      // Two words: the first one is the verb.
      if SameText(LWords[0], 'Build') then
      begin
        Result := BuildView(ANode, LWords[1]);
        Exit;
      end;
    end;
  end;
  Result := FindObjectByNode(ANode) as TKView;
end;

function TKViews.BuildView(const ANode: TEFNode; const AViewBuilderName: string): TKView;
var
  LViewBuilder: TKViewBuilder;
begin
  Assert(Assigned(ANode));
  Assert(AViewBuilderName <> '');

  LViewBuilder := TKViewBuilderFactory.Instance.CreateObject(AViewBuilderName);
  try
    LViewBuilder.Assign(ANode);
    Result := LViewBuilder.BuildView;
  finally
    FreeAndNil(LViewBuilder);
  end;
end;

function TKViews.GetLayouts: TKLayouts;
begin
  if not Assigned(FLayouts) then
    FLayouts := TKLayouts.Create;
  Result := FLayouts;
end;

function TKViews.GetObjectClassType: TKMetadataClass;
begin
  Result := TKView;
end;

procedure TKViews.Open;
begin
  inherited;
  Layouts.Open;
end;

procedure TKViews.SetPath(const AValue: string);
begin
  inherited;
  Layouts.Path := IncludeTrailingPathDelimiter(AValue) + 'Layouts';
end;

function TKViews.ViewByName(const AName: string): TKView;
begin
  Result := ObjectByName(AName) as TKView;
end;

function TKViews.ViewByNode(const ANode: TEFNode): TKView;
begin
  Result := FindViewByNode(ANode);
  if not Assigned(Result) then
    if Assigned(ANode) then
      ObjectNotFound(ANode.Name + ':' + ANode.AsString)
    else
      ObjectNotFound('<nil>');
end;

{ TKLayouts }

procedure TKLayouts.AfterCreateObject(const AObject: TKMetadata);
begin
  inherited;
  (AObject as TKLayout).FLayouts := Self;
end;

function TKLayouts.FindLayout(const AName: string): TKLayout;
begin
  Result := FindObject(AName) as TKLayout;
end;

function TKLayouts.GetObjectClassType: TKMetadataClass;
begin
  Result := TKLayout;
end;

function TKLayouts.LayoutByName(const AName: string): TKLayout;
begin
  Result := ObjectByName(AName) as TKLayout;
end;

{ TKView }

function TKView.GetChildClass(const AName: string): TEFNodeClass;
begin
  if SameText(AName, 'MainTable') then
    Result := TKViewTable
  else
    Result := inherited GetChildClass(AName);
end;

function TKView.GetControllerType: string;
begin
  Result := GetString('Controller');
end;

function TKView.GetDisplayLabel: string;
begin
  Result := GetString('DisplayLabel');
end;

function TKView.GetImageName: string;
begin
  Result := GetString('ImageName');
  if Result = '' then
    Result := 'default_view';
end;

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

function TKDataView.GetMainTable: TKViewTable;
begin
  Result := GetNode('MainTable', True) as TKViewTable;
end;

{ TKViewTable }

function TKViewTable.CreateStore: TKStore;
var
  I: Integer;
begin
  Result := TKStore.Create;
  try
    // Set field names and data types both in key and header.
    for I := 0 to FieldCount - 1 do
    begin
      if Fields[I].IsKey then
        Result.Key.AddChild(Fields[I].AliasedName).DataType := Fields[I].DataType;
      Result.Header.AddChild(Fields[I].AliasedName).DataType := Fields[I].DataType;
    end;
  except
    FreeAndNil(Result);
    raise;
  end;
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

function TKViewTable.FindField(const AName: string): TKViewField;
begin
  Result := GetFields.FindChild(AName) as TKViewField;
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
  Result := Environment.Models.FindModel(ModelName);
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
      if not VarIsNull(LValue) then
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
  Result := GetDetailTables.ChildCount;
end;

function TKViewTable.GetDetailTables: TKViewTables;
begin
  Result := GetNode('DetailTables', True) as TKViewTables;
end;

function TKViewTable.GetDisplayLabel: string;
begin
  Result := GetString('DisplayLabel');
  if Result = '' then
    Result := Model.DisplayLabel;
end;

function TKViewTable.GetField(I: Integer): TKViewField;
begin
  Result := GetFields[I];
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

  function GetPath: string;
  var
    LParent: TEFTree;
  begin
    Result := '';
    LParent := Parent;
    while (LParent is TKViewTables) or (LParent is TKViewTable) do
    begin
      if LParent is TKViewTable then
        Result := Result + '/' + TKViewTable(LParent).ModelName;
      LParent := (LParent as TEFNode).Parent;
    end;
  end;

begin
  Result := View.GetResourceURI + GetPath;
end;

function TKViewTable.GetRules: TKRules;
begin
  Result := GetNode('Rules', True) as TKRules;
end;

function TKViewTable.GetKeyFieldAliasedNames: TStringDynArray;
var
  I: Integer;
  LDataViewField: TKViewField;
begin
  Result := Model.GetKeyFieldNames;
  // Apply aliasing.
  for I := Low(Result) to High(Result) do
  begin
    LDataViewField := FindField(Result[I]);
    if Assigned(LDataViewField) then
      Result[I] := LDataViewField.AliasedName;
  end;
end;

function TKViewTable.FindLayout(const AKind: string): TKLayout;
begin
  Result := Environment.Views.Layouts.FindLayout(View.PersistentName + '_' + AKind);
end;

function TKViewTable.GetTable(I: Integer): TKViewTable;
begin
  Result := GetDetailTables.Children[I] as TKViewTable;
end;

function TKViewTable.GetModelName: string;
begin
  Result := GetNode('Model', True).AsString;
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
  Result := Environment.IsAccessGranted(GetResourceURI, AMode)
    and Environment.IsAccessGranted(View.GetResourceURI, AMode)
    and Environment.IsAccessGranted(Model.GetResourceURI, AMode);
end;

function TKViewTable.IsFieldVisible(const AField: TKViewField): Boolean;
begin
  Assert(Assigned(AField));

  Result := AField.IsVisible
    or MatchText(AField.AliasedName, GetStringArray('Controller/VisibleFields'));
end;

{ TKDataViewTables }

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

{ TKDataViewFields }

function TKViewFields.FieldByAliasedName(
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
  if not Assigned(Result) then
    raise EKError.CreateFmt('ViewField %s not found.', [AAliasedName]);
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

function TKViewField.CreateStore: TKStore;
var
  I: Integer;
  LField: TKModelField;
begin
  Assert(Reference <> nil);

  Result := TKStore.Create;
  try
    for I := 0 to Reference.FieldCount - 1 do
    begin
      LField := Reference.ReferencedFields[I];
      Result.Key.AddChild(LField.FieldName).DataType := LField.DataType;
      Result.Header.AddChild(LField.FieldName).DataType := LField.DataType;
    end;
    if Result.Header.FindChild(AliasedName) = nil then
      Result.Header.AddChild(AliasedName).DataType := DataType;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TKViewField.FindNode(const APath: string;
  const ACreateMissingNodes: Boolean): TEFNode;
begin
  Result := inherited FindNode(APath, ACreateMissingNodes);
  if not Assigned(Result) then
    // ACreateMissingNodes is False here.
    Result := ModelField.FindNode(APath, False);
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

function TKViewField.GetChildClass(const AName: string): TEFNodeClass;
begin
  if SameText(AName, 'Rules') then
    Result := TKRules
  else
    Result := inherited GetChildClass(AName);
end;

function TKViewField.GetQualifiedAliasedNameOrExpression: string;
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
    Result := QualifiedName;
    if Alias <> '' then
      Result := Result + ' ' + Alias;
  end;
end;

function TKViewField.GetDataType: TEFDataType;
begin
  Result := StringToEFDataType(GetString('DataType'));
  if Result = edtUnknown then
    Result := ModelField.DataType;
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
end;

function TKViewField.GetDisplayLabel: string;
begin
  Result := GetString('DisplayLabel');
  if Result = '' then
    Result := ModelField.DisplayLabel;
end;

function TKViewField.GetDisplayWidth: Integer;
begin
  Result := GetInteger('DisplayWidth');
  if Result = 0 then
    Result := ModelField.DisplayWidth;
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

function TKViewField.GetIsVisible: Boolean;
begin
  Result := GetBoolean('IsVisible', True);
end;

function TKViewField.GetQualifiedName: string;
begin
  if Pos('.', Name) > 0 then
    Result := Name
  else
    Result := GetModelName + '.' + GetFieldName;
end;

function TKViewField.GetSize: Integer;
begin
  Result := GetInteger('Size');
  if Result = 0 then
    Result := ModelField.Size;
end;

function TKViewField.GetIsBlob: Boolean;
begin
  { TODO : add support for binary blobs. }
  Result := (DataType = edtString) and (Size = 0);
end;

function TKViewField.GetIsKey: Boolean;
begin
  Result := (ModelName = Table.ModelName) and ModelField.IsKey;
end;

function TKViewField.GetIsReadOnly: Boolean;
begin
  Result := GetBoolean('IsReadOnly');
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
    Result := Table.Model.ReferenceByName(LNameParts[0]).ReferencedModel.ModelName
  else
    raise EKError.CreateFmt('Couldn''t determine model name for field %s.', [Name]);
end;

function TKViewField.GetReference: TKModelReference;
var
  LNameParts: TStringDynArray;
begin
  LNameParts := Split(Name, '.');
  if Length(LNameParts) = 1 then
    // <field name>
    Result := nil
  else if Length(LNameParts) = 2 then
    // <reference name>.<field name>
    Result := Table.Model.ReferenceByName(LNameParts[0])
  else
    raise EKError.CreateFmt('Couldn''t determine reference for field %s.', [Name]);
end;

function TKViewField.GetRules: TKRules;
begin
  Result := GetNode('Rules', True) as TKRules;
end;

{ TKTreeViewNode }

function TKTreeViewNode.GetChildClass(const AName: string): TEFNodeClass;
begin
  if SameText(AName, 'Folder') then
    Result := TKTreeViewFolder
  else if SameText(AName, 'View') then
    Result := TKTreeViewNode
  else
    Result := inherited GetChildClass(AName);
end;

function TKTreeViewNode.GetTreeViewNode(I: Integer): TKTreeViewNode;
begin
  Result := GetChild<TKTreeViewNode>(I);
end;

function TKTreeViewNode.GetTreeViewNodeCount: Integer;
begin
  Result := GetChildCount<TKTreeViewNode>;
end;

{ TKTreeView }

function TKTreeView.GetChildClass(const AName: string): TEFNodeClass;
begin
  if SameText(AName, 'Folder') then
    Result := TKTreeViewFolder
  else if SameText(AName, 'View') then
    Result := TKTreeViewNode
  else
    Result := inherited GetChildClass(AName);
end;

function TKTreeView.GetTreeViewNode(I: Integer): TKTreeViewNode;
begin
  Result := GetChild<TKTreeViewNode>(I);
end;

function TKTreeView.GetTreeViewNodeCount: Integer;
begin
  Result := GetChildCount<TKTreeViewNode>;
end;

{ TKViewBuilderRegistry }

class destructor TKViewBuilderRegistry.Destroy;
begin
  FreeAndNil(FInstance);
end;

function TKViewBuilderRegistry.GetClass(const AId: string): TKViewBuilderClass;
begin
  Result := TKViewBuilderClass(inherited GetClass(AId));
end;

class function TKViewBuilderRegistry.GetInstance: TKViewBuilderRegistry;
begin
  if FInstance = nil then
    FInstance := TKViewBuilderRegistry.Create;
  Result := FInstance;
end;

{ TKViewBuilderFactory }

function TKViewBuilderFactory.CreateObject(const AId: string): TKViewBuilder;
begin
  Result := inherited CreateObject(AId) as TKViewBuilder;
end;

class destructor TKViewBuilderFactory.Destroy;
begin
  FreeAndNil(FInstance);
end;

function TKViewBuilderFactory.DoCreateObject(const AClass: TClass): TObject;
begin
  // Must use the virtual constructor in TEFTree.
  Result := TKViewBuilderClass(AClass).Create;
end;

class function TKViewBuilderFactory.GetInstance: TKViewBuilderFactory;
begin
  if FInstance = nil then
    FInstance := TKViewBuilderFactory.Create(TKViewBuilderRegistry.Instance);
  Result := FInstance;
end;

initialization
  TKMetadataRegistry.Instance.RegisterClass('Data', TKDataView);
  TKMetadataRegistry.Instance.RegisterClass('Tree', TKTreeView);

finalization
  TKMetadataRegistry.Instance.UnregisterClass('Data');
  TKMetadataRegistry.Instance.UnregisterClass('Tree');

end.


