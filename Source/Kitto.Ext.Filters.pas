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

/// <summary>
///  Filters are used by the List controller to implement a user interface for
///  data filtering. Several filters can be combined in a single controller.
///  Filters are configured in the view, below the Controller node.
/// </summary>
unit Kitto.Ext.Filters;

{$I Kitto.Defines.inc}

interface

uses
  Types
  , DB
  , Ext.Base
  , Ext.Form
  , Ext.Data
  , EF.Types
  , EF.Tree
  , EF.ObserverIntf
  , Kitto.JS
  , Kitto.DatabaseRouter
  , Kitto.Store
  , Kitto.Metadata.Views
  , Kitto.Metadata.DataView
  , Kitto.Ext.Base
  , Kitto.JS.Controller
  , Kitto.Ext.LookupField
  ;

const
  DEFAULT_FILTER_WIDTH = 20;

type
  /// <summary>
  ///  All filters must implement this interface.
  /// </summary>
  IKExtFilter = interface(IEFSubject)
    ['{A7E485F3-FA8B-4446-BC8D-71E92188DA57}']

    /// <summary>
    ///  Builds and returns the filter expression.
    /// </summary>
    /// <returns>
    ///  The expression build according to the filter state.
    /// </returns>
    /// <remarks>
    ///  Returns '' if the filter has no expression in its current state.
    /// </remarks>
    function GetExpression: string;

    /// <summary>
    ///  Returns a reference to the object implementing the filter.
    /// </summary>
    /// <remarks>
    ///  A filter MUST be a TExtObject descendant.
    /// </remarks>
    function AsExtObject: TExtObject;

    /// <summary>
    ///  Called to pass the filter's configuration data, extracted as a node
    ///  from the view.
    /// </summary>
    /// <param name="AConfig">
    ///  A tree of parameters that configure the filter.
    /// </param>
    procedure SetConfig(const AConfig: TEFNode);

    /// <summary>
    ///  Called to initialize the view table upon which the filter is working.
    /// </summary>
    /// <param name="AViewTable">
    ///  A reference to the view table.
    /// </param>
    procedure SetViewTable(const AViewTable: TKViewTable);

    /// <summary>
    ///  Expands any occurrences of tags representing the filter's current value(s)
    ///  with the actual value, if applicable. Tags are in the form {FilterId},
    ///  where FilterId is the Id attribute of the filter, if any.
    /// </summary>
    /// <remarks>
    ///  Only single-select string-based filters support this kind of tag expansion.
    ///  Only filters with an Id support it.
    /// </remarks>
    /// <param name="AString">
    ///  The input string that is returned with tags expanded. Unknown tags are left as-is.
    /// </param>
    function ExpandValues(const AString: string): string;

    /// <summary>
    ///  Returns a unique Id that identifies the filter in a set of filters.
    ///  Used for features that require filters referencing each other.
    ///  It is usually specified as a subnode named Id in the filter configuration.
    /// </summary>
    function GetId: string;

    /// <summary>
    ///  Filters that support lists of values should reload the list (or mark
    ///  it for reload) when this method is called. It is used to have a filter
    ///  that depends on the value of another filter.
    /// </summary>
    procedure Invalidate;
  end;

  /// <summary>
  ///  Keeps track of all registered filter classes. All new filters must be
  ///  registered before they can be used in controllers.
  /// </summary>
  TKExtFilterRegistry = class(TEFRegistry)
  private
    class var FInstance: TKExtFilterRegistry;
    class function GetInstance: TKExtFilterRegistry; static;
  protected
    procedure BeforeRegisterClass(const AId: string; const AClass: TClass); override;
  public
    class property Instance: TKExtFilterRegistry read GetInstance;
    class destructor Destroy;
    function GetClass(const AId: string): TExtObjectClass;
  end;

  /// <summary>
  ///  Queries the registry to create filter objects by class Id. It is
  ///  friend to TKControllerRegistry.
  /// </summary>
  TKExtFilterFactory = class(TEFFactory)
  private
    FContainer: TJSObjectArray;
    class var FInstance: TKExtFilterFactory;
    class function GetInstance: TKExtFilterFactory; static;
    function CreateObject(const AId: string; const AContainer: TJSObjectArray): IKExtFilter;
  protected
    function DoCreateObject(const AClass: TClass): TObject; override;
    class destructor Destroy;
  public
    class property Instance: TKExtFilterFactory read GetInstance;

    function CreateFilter(const AFilterConfig: TEFNode; const AObserver: IEFObserver;
      const AContainer: TJSObjectArray; const AViewTable: TKViewTable): IKExtFilter;
  end;

  /// <summary>
  ///  Base class for list-based filters.
  /// </summary>
  TKListFilterBase = class(TKExtFormComboBox)
  strict protected
    FViewTable: TKViewTable;
    FServerStore: TKStore;
  public
    destructor Destroy; override;
  public
    procedure SetConfig(const AConfig: TEFNode); virtual;
    function AsExtObject: TExtObject;
    procedure SetViewTable(const AViewTable: TKViewTable);
    function GetId: string;
    procedure Invalidate;
  //published
    procedure GetRecordPage; virtual; abstract;
  end;

  /// <summary>
  ///  Static list. Displays a combo box with a list of mutually
  ///  exclusive filter expressions. When an item is selected, the corresponding
  ///  expression is applied.
  /// </summary>
  /// <remarks>
  ///  <para>
  ///   If no item has IsDefault set to True, then the first item is the
  ///   default item.
  ///  </para>
  ///  <para>
  ///   An empty Expression means no filter (thus, all records
  ///   retrieved).
  ///  </para>
  /// </remarks>
  /// <example>
  ///  <code>
  ///   Items:
  ///     List: Shipping date
  ///       Items:
  ///         Today: Goods shipped today
  ///           Expression: datediff(day, GOODS.SHIPPING_DATE, getdate()) = 0
  ///         Everything: Everything else
  ///           IsDefault: True
  ///  </code>
  /// </example>
  TKListFilter = class(TKListFilterBase, IKExtFilter)
  private
    FActiveIndex: Integer;
    FItems: TEFNode;
    procedure ComboBoxSelect(Combo: TExtFormComboBox; RecordJS: TExtDataRecord; Index: Integer);
    procedure ComboBoxChange(This: TExtFormField; NewValue, OldValue: string);
  public
    function GetExpression: string;
    procedure SetConfig(const AConfig: TEFNode); override;
    function ExpandValues(const AString: string): string;
  //published
    procedure GetRecordPage; override;
  end;

  /// <summary>
  ///  Dynamic list. Displays a list of items obtained by executing a
  ///  query. When an item is selected, the corresponding value is merged into a
  ///  specified expression template to build the filter expression.
  /// </summary>
  /// <example>
  ///  <code>
  ///   Items:
  ///     DynaList: Employee
  ///       CommandText: |
  ///         select distinct SSN, LAST_NAME + ' ' + FIRST_NAME + ' (' + SSN + ')'
  ///         from CERTIFICATES order by 1
  ///       ExpressionTemplate: SSN = '{value}'
  ///  </code>
  /// </example>
  TKDynaListFilter = class(TKListFilterBase, IKExtFilter)
  strict private
    FCurrentValue: string;
    const VALUE_FIELD = 0;
    const DISPLAY_FIELD = 1;
  public
    function GetExpression: string;
    procedure SetConfig(const AConfig: TEFNode); override;
    function ExpandValues(const AString: string): string;
  //published
    procedure ValueChanged;
    procedure GetRecordPage; override;
  end;

  /// <summary>
  ///  <para>
  ///   Free search. Displays an edit for a free search criteria input.
  ///  </para>
  ///  <para>
  ///   AutoSearchAfterChars determines the number of characters that can
  ///   be entered before the search automatically fires. Default is 0
  ///   characters (no auto search).
  ///  </para>
  /// </summary>
  TKFreeSearchFilter = class(TKExtFormTextField, IKExtFilter)
  strict private
    FCurrentValue: string;
    FViewTable: TKViewTable;
    procedure FieldChange(This: TExtFormField; NewValue, OldValue: string);
  public
    procedure SetConfig(const AConfig: TEFNode);
    function AsExtObject: TExtObject;
    function GetExpression: string;
    procedure SetViewTable(const AViewTable: TKViewTable);
    function ExpandValues(const AString: string): string;
    function GetId: string;
    procedure Invalidate;
  end;

  /// <summary>
  ///  <para>
  ///   Date search. Displays a calendar edit for a search of a date value.
  ///  </para>
  /// </summary>
  TKDateSearchFilter = class(TKExtFormDateField, IKExtFilter)
  strict private
    FCurrentValue: TDateTime;
    FViewTable: TKViewTable;
    procedure FieldChange(This: TExtFormField; NewValue, OldValue: string);
  public
    procedure SetConfig(const AConfig: TEFNode);
    function AsExtObject: TExtObject;
    function GetExpression: string;
    procedure SetViewTable(const AViewTable: TKViewTable);
    function ExpandValues(const AString: string): string;
    function GetId: string;
    procedure Invalidate;
  end;

  /// <summary>
  ///  <para>
  ///   Boolean search. Displays a checkbox that applies a filter
  ///   defined in the ExpressionTemplate.
  ///  </para>
  /// </summary>
  TKBooleanSearchFilter = class(TKExtFormCheckBoxField, IKExtFilter)
  strict private
    FIsCurrentValue: Boolean;
    FViewTable: TKViewTable;
    procedure FieldChecked(This: TExtFormCheckBox; Checked: boolean);
  public
    procedure SetConfig(const AConfig: TEFNode);
    function AsExtObject: TExtObject;
    function GetExpression: string;
    procedure SetViewTable(const AViewTable: TKViewTable);
    function ExpandValues(const AString: string): string;
    function GetId: string;
    procedure Invalidate;
  end;

   /// <summary>
  ///  <para>
  ///   Numeric search. Displays an edit for a free search criteria numeric input.
  ///  </para>
  ///  <para>
  ///   AutoSearchAfterChars determines the number of characters that can
  ///   be entered before the search automatically fires. Default is 0
  ///   characters (no auto search).
  ///  </para>
  /// </summary>
  TKNumericSearchFilter = class(TKExtFormNumberField, IKExtFilter)
  strict private
    FCurrentValue: string;
    FConfig: TEFNode;
    FViewTable: TKViewTable;
    procedure FieldChange(This: TExtFormField; NewValue, OldValue: string);
  public
    procedure SetConfig(const AConfig: TEFNode);
    function AsExtObject: TExtObject;
    function GetExpression: string;
    procedure SetViewTable(const AViewTable: TKViewTable);
    function ExpandValues(const AString: string): string;
    function GetId: string;
    procedure Invalidate;
  end;

  TKButtonListFilterBase = class(TKExtPanelBase)
  strict private
    FSelected: TArray<Boolean>;
  strict protected
    FItems: TEFNode;
    FViewTable: TKViewTable;
    function GetACName: string;
    function RetrieveItems: TEFNode; virtual; abstract;
    function GetItemExpression(const AItemIndex: Integer): string; virtual; abstract;
    function IsSingleSelect: Boolean;
    function IsButtonVisible(const AACName: string): Boolean; virtual;
  public
    procedure SetConfig(const AConfig: TEFNode);
    function AsExtObject: TExtObject;
    function GetExpression: string;
    procedure SetViewTable(const AViewTable: TKViewTable);
    function ExpandValues(const AString: string): string;
    function GetId: string;
    procedure Invalidate;
  //published
    procedure ButtonClick;
  end;

  /// <summary>
  ///  <para>
  ///   Similar to a ListFilter, but uses a set of buttons instead of a
  ///   combo box.
  ///  </para>
  ///  <para>
  ///   The difference is that several buttons can be pressed at once, in
  ///   which case their expressions are concatenated with a Connector (default
  ///   'or').
  ///  </para>
  ///  <para>
  ///   All filters with IsDefault set to True are renderd as initially
  ///   pressed buttons.
  ///  </para>
  /// </summary>
  TKButtonListFilter = class(TKButtonListFilterBase, IKExtFilter)
  strict protected
    function RetrieveItems: TEFNode; override;
    function GetItemExpression(const AItemIndex: Integer): string; override;
  end;

  /// <summary>
  ///  Dynamic button list. Builds its set of buttons by executing a
  ///  query. When an item is selected, the corresponding value is merged into a
  ///  specified expression template to build the filter expression.
  /// </summary>
  /// <example>
  ///  <code>
  ///   Items:
  ///     DynaButtonList: Employee
  ///       CommandText: |
  ///         select TYPE_ID, DESCRIPTION
  ///         from TYPES order by 1
  ///       ExpressionTemplate: {Q}TYPE_ID = '{value}'
  ///  </code>
  /// </example>
  /// <seealso>
  ///  TKButtonListFilter
  /// </seealso>
  TKDynaButtonListFilter = class(TKButtonListFilterBase, IKExtFilter)
  strict private
    const VALUE_FIELD = 0;
    const DISPLAY_FIELD = 1;
  strict protected
    function RetrieveItems: TEFNode; override;
    function GetItemExpression(const AItemIndex: Integer): string; override;
  public
    destructor Destroy; override;
  end;

  /// <summary>
  ///  A button that causes the filter expression to be rebuilt for the entire
  ///  panel. When an item of this type is present, the filter expression is
  ///  no longer automatically rebuilt every time a filter item changes.
  /// </summary>
  TKFilterApplyButton = class(TKExtButton, IKExtFilter)
  public
    function GetExpression: string;
    procedure SetConfig(const AConfig: TEFNode);
    function AsExtObject: TExtObject;
    procedure SetViewTable(const AViewTable: TKViewTable);
    function ExpandValues(const AString: string): string;
    function  GetId: string;
    procedure Invalidate;
  //published
    procedure ButtonClick;
  end;

  /// <summary>
  ///  A spacer, to be used for layout purposes when using multiple columns.
  /// </summary>
  TKFilterSpacer = class(TKExtPanelBase, IKExtFilter)
  public
    function GetExpression: string;
    procedure SetConfig(const AConfig: TEFNode);
    function AsExtObject: TExtObject;
    procedure SetViewTable(const AViewTable: TKViewTable);
    function ExpandValues(const AString: string): string;
    function GetId: string;
    procedure Invalidate;
  end;

  /// <summary>
  ///  A filter that mimics the lookup editor. A trigger button opens a view
  ///  that allows the user to select a record whose values can then be used in
  ///  building a filter expression. The ReferenceFieldName parameter must
  ///  contain the name of a reference ViewField. A view with IsLookup=True
  ///  must exist based on the referenced model.
  /// </summary>
  /// <example>
  ///  <code>
  ///   Items:
  ///     Lookup: By Job
  ///       ReferenceFieldName: JobRef
  ///       ExpressionTemplate: (Employee.JOB_CODE = '{JobCode}') and (Employee.JOB_GRADE = '{JobGrade}') and (Employee.JOB_COUNTRY = '{JobCountry}')
  ///  </code>
  /// </example>
  /// <seealso>
  ///  TKButtonListFilter
  /// </seealso>
  TKLookupFilter = class(TKExtLookupField, IKExtFilter)
  private
    FCurrentValues: TKViewTableStore;
    FViewTable: TKViewTable;
  strict protected
    procedure LookupConfirmed(const ARecord: TKViewTableRecord); override;
  public
    destructor Destroy; override;
    function GetExpression: string;
    procedure SetConfig(const AConfig: TEFNode);
    procedure SetViewTable(const AViewTable: TKViewTable);
    function ExpandValues(const AString: string): string;
    function GetId: string;
    procedure Invalidate;
    function AsExtObject: TExtObject;
  //published
    procedure DoClear; override;
  end;

implementation

uses
  SysUtils
  , Math
  , StrUtils
  , EF.Localization
  , EF.DB
  , EF.StrUtils
  , EF.JSON
  , EF.SQL
  , Kitto.Types
  , Kitto.Config
  , Kitto.Auth
  , Kitto.AccessControl
  , Kitto.JS.Formatting
  , Kitto.Web.Application
  , Kitto.Web.Request
  , Kitto.Web.Response
  , Kitto.Ext.Utils
  ;

function GetDefaultFilter(const AItems: TEFNode): TEFNode;
var
  I: Integer;
begin
  Assert(Assigned(AItems));
  Assert(AItems.ChildCount > 0);

  Result := nil;
  for I := 0 to AItems.ChildCount - 1 do
  begin
    if AItems.Children[I].GetBoolean('IsDefault') then
    begin
      Result := AItems.Children[I];
      Break;
    end;
  end;
end;

function GetDatabaseName(const AConfig: TEFTree; const ACallerContext: TObject;
  const ADefaultDatabaseName: string): string;
var
  LDatabaseRouterNode: TEFNode;
begin
  LDatabaseRouterNode := AConfig.FindNode('DatabaseRouter');
  if Assigned(LDatabaseRouterNode) then
    Result := TKDatabaseRouterFactory.Instance.GetDatabaseName(
      LDatabaseRouterNode.AsString, ACallerContext, LDatabaseRouterNode)
  else
    Result := ADefaultDatabaseName;
end;

function ExpandFilterValues(const AList: TJSObjectArray; const AString: string): string;
var
  I: Integer;
  LFilter: IKExtFilter;
begin
  Result := AString;
  for I := 0 to AList.Count - 1 do
  begin
    if Supports(AList[I], IKExtFilter, LFilter) then
      Result := LFilter.ExpandValues(Result);
  end;
end;

{ TKExtFilterRegistry }

procedure TKExtFilterRegistry.BeforeRegisterClass(const AId: string;
  const AClass: TClass);
begin
  if not Supports(AClass, IKExtFilter) or not  AClass.InheritsFrom(TextObject) then
    raise EKError.CreateFmt('Cannot register class %s (Id %s). Class does not support IKExtFilter or does not inherit from TExtObject.', [AClass.ClassName, AId]);
  inherited;
end;

class destructor TKExtFilterRegistry.Destroy;
begin
  FreeAndNil(FInstance);
end;

function TKExtFilterRegistry.GetClass(const AId: string): TExtObjectClass;
begin
  Result := TExtObjectClass(inherited GetClass(AId));
end;

class function TKExtFilterRegistry.GetInstance: TKExtFilterRegistry;
begin
  if FInstance = nil then
    FInstance := TKExtFilterRegistry.Create;
  Result := FInstance;
end;

{ TKExtFilterFactory }

function TKExtFilterFactory.CreateFilter(const AFilterConfig: TEFNode;
  const AObserver: IEFObserver; const AContainer: TJSObjectArray;
  const AViewTable: TKViewTable): IKExtFilter;
begin
  Assert(AFilterConfig <> nil);
  Assert(Assigned(AContainer));

  Result := CreateObject(AFilterConfig.Name, AContainer);
  if Assigned(AObserver) then
    Result.AttachObserver(AObserver);
  Result.SetViewTable(AViewTable);
  Result.SetConfig(AFilterConfig);
end;

function TKExtFilterFactory.CreateObject(const AId: string;
  const AContainer: TJSObjectArray): IKExtFilter;
var
  LObject: TObject;
begin
  FContainer := AContainer;
  LObject := inherited CreateObject(AId);
  if not Supports(LObject, IKExtFilter, Result) then
    raise EKError.CreateFmt('Class %s not found.', [AId]);
end;

class destructor TKExtFilterFactory.Destroy;
begin
  FreeAndNil(FInstance);
end;

function TKExtFilterFactory.DoCreateObject(const AClass: TClass): TObject;
begin
  Result := TExtObjectClass(AClass).CreateAndAddToArray(FContainer);
end;

class function TKExtFilterFactory.GetInstance: TKExtFilterFactory;
begin
  if FInstance = nil then
    FInstance := TKExtFilterFactory.Create(TKExtFilterRegistry.Instance);
  Result := FInstance;
end;

{ TKListFilterBase }

function TKListFilterBase.AsExtObject: TExtObject;
begin
  Result := Self;
end;

destructor TKListFilterBase.Destroy;
begin
  FreeAndNil(FServerStore);
  inherited;
end;

function TKListFilterBase.GetId: string;
begin
  Result := Config.GetExpandedString('Id');
end;

procedure TKListFilterBase.Invalidate;
begin
  SetRawValue('');
  // Force the combo to refresh its list at next drop down.
  Store.RemoveAll();
  TKWebResponse.Current.Items.ExecuteJSCode(Format('delete %s.lastQuery;', [JSName]));
end;

procedure TKListFilterBase.SetConfig(const AConfig: TEFNode);
var
  I: Integer;
  LFieldNames: TStringDynArray;
  LWidth: Integer;
  LListWidth: Integer;
  LProxy: TExtDataAjaxProxy;
  LReader: TExtDataJsonReader;
begin
  Assert(Assigned(AConfig));

  FieldLabel := _(AConfig.AsString);
  Config.Assign(AConfig);
  TriggerAction := 'all';
  Editable := True;
  LazyRender := True;
  SelectOnFocus := True;
  AutoSelect := False;
  AllowBlank := True;
  QueryMode := 'remote';
  FServerStore := TKStore.Create;
  Store := TExtDataStore.Create(Owner);
  FServerStore.Header.AddField('Id');
  FServerStore.Header.AddField('Description');
  SetLength(LFieldNames,1);
  LFieldNames[0] := 'Id';
  FServerStore.Key.SetFieldNames(LFieldNames);
  LProxy := TExtDataAjaxProxy.Create(Store);
  LProxy.Url := GetMethodURL(GetRecordPage);
  Store.Proxy := LProxy;
  LReader := TExtDataJsonReader.Create(Self);
  LReader.RootProperty := 'Root';
  LReader.TotalProperty := 'Total';
  LProxy.Reader := LReader;
  for I := 0 to FServerStore.Header.FieldCount - 1 do
    with TExtDataField.CreateInlineAndAddToArray(Store.Proxy.Reader.Fields) do
      Name := FServerStore.Header.Fields[I].FieldName;
  ValueField := 'Id';
  DisplayField := 'Description';

  LWidth := Config.GetInteger('Width', DEFAULT_FILTER_WIDTH);
  WidthExpression := CharsToPixels(LWidth);
  LListWidth := Config.GetInteger('ListWidth', -1);
  if LListWidth = -1 then
    LListWidth := LWidth;
  ListWidthFunc := CharsToPixels(LListWidth);
  TypeAhead := True;
  MinChars := Config.GetInteger('AutoCompleteMinChars', 4);
end;

procedure TKListFilterBase.SetViewTable(const AViewTable: TKViewTable);
begin
  FViewTable := AViewTable;
end;

{ TKListFilter }

procedure TKListFilter.SetConfig(const AConfig: TEFNode);
var
  LDefaultFilter: TEFNode;
  I: Integer;
  LRecord: TKRecord;
begin
  inherited;
  FItems := Config.GetNode('Items');
  Assert(Assigned(FItems));

  Assert(Assigned(FServerStore));
  FServerStore.Records.Clear;
  for I := 0 to FItems.ChildCount - 1 do
  begin
    LRecord := FServerStore.AppendRecord(nil);
    LRecord.Fields[0].AsInteger := I;
    LRecord.Fields[1].AsString := _(FItems.Children[I].AsExpandedString);
  end;

  //PageSize := 10;
  //Resizable := True;
  //MinHeight := LinesToPixels(5);
  LDefaultFilter := GetDefaultFilter(FItems);
  if Assigned(LDefaultFilter) then
  begin
    SetValue(LDefaultFilter.Name);
    FActiveIndex := LDefaultFilter.Index;
  end
  else
    FActiveIndex := -1;
  if Config.GetBoolean('Sys/IsReadOnly') then
    Disabled := True
  else
  begin
    OnSelect := ComboBoxSelect;
    OnChange := ComboBoxChange;
  end;
  if FActiveIndex >= 0 then
    SetValue(FServerStore.Records[FActiveIndex].Fields[1].AsString);
end;

procedure TKListFilter.ComboBoxSelect(Combo: TExtFormComboBox; RecordJS: TExtDataRecord; Index: Integer);
begin
  if FActiveIndex <> Index then
  begin
    FActiveIndex := Index;
    NotifyObservers('FilterChanged');
  end;
end;

function TKListFilter.ExpandValues(const AString: string): string;
begin
  Result := AString;
end;

procedure TKListFilter.ComboBoxChange(This: TExtFormField; NewValue: string; OldValue: string);
begin
  if NewValue = '' then
  begin
    FActiveIndex := -1;
    NotifyObservers('FilterChanged');
  end;
end;

function TKListFilter.GetExpression: string;
begin
  Assert(Assigned(FItems));

  if (FActiveIndex >= 0) and (FActiveIndex < FItems.ChildCount) then
    Result := FItems.Children[FActiveIndex].GetExpandedString('Expression')
  else
    Result := '';
end;

procedure TKListFilter.GetRecordPage;
begin
  inherited;
  Assert(Assigned(FServerStore));

  TKWebResponse.Current.Items.AddJSON('{Total: ' + IntToStr(FServerStore.RecordCount)
    + ', Root: ' + FServerStore.GetAsJSON(False) + '}');
end;

{ TKDynaListFilter }

procedure TKDynaListFilter.ValueChanged;
var
  LNewValue: string;
  LInvalidate: string;
begin
  LNewValue := ParamAsString('Value');
  if FCurrentValue <> LNewValue then
  begin
    FCurrentValue := LNewValue;
    NotifyObservers('FilterChanged');
    for LInvalidate in Config.GetStringArray('Invalidates') do
      NotifyObservers('FilterInvalidated ' + LInvalidate);
  end;
end;

function TKDynaListFilter.ExpandValues(const AString: string): string;
begin
  Result := ReplaceText(AString, '{' + GetId + '}', FCurrentValue);
end;

function TKDynaListFilter.GetExpression: string;
begin
  if FCurrentValue <> '' then
    Result := ReplaceText(Config.GetExpandedString('ExpressionTemplate'), '{value}', FCurrentValue)
  else
    Result := '';
end;

procedure TKDynaListFilter.GetRecordPage;
var
  LStart: Integer;
  LLimit: Integer;
  LPageRecordCount: Integer;
  LDBConnection: TEFDBConnection;
  LDBQuery: TEFDBQuery;
  LCommandText: string;
  LQuery: string;
  LQueryExpression: string;
begin
  inherited;

  Assert(Assigned(FServerStore));

  LDBConnection := TKConfig.Instance.CreateDBConnection(GetDatabaseName(Config, Self, FViewTable.DatabaseName));
  try
    LDBQuery := LDBConnection.CreateDBQuery;
    try
      LCommandText := ExpandFilterValues(Owner as TJSObjectArray, Config.GetExpandedString('CommandText'));
      LQuery := ParamAsString('query');
      if LQuery <> '' then
        LQueryExpression := ReplaceStr(Config.GetExpandedString('QueryTemplate'), '{queryValue}', LQuery)
      else
        LQueryExpression := '';
      LDBQuery.CommandText := ReplaceStr(LCommandText, '{query}', LQueryExpression);
      LDBQuery.Open;
      try
        Assert(LDBQuery.DataSet.FieldCount = 2);
        FServerStore.Load(LDBQuery, False, True);
      finally
        LDBQuery.Close;
      end;
    finally
      FreeAndNil(LDBQuery);
    end;
  finally
    FreeAndNil(LDBConnection);
  end;

  LStart := ParamAsInteger('start');
  LLimit := ParamAsInteger('limit');
  LPageRecordCount := Min(LLimit, FServerStore.RecordCount - LStart);

  TKWebResponse.Current.Items.AddJSON('{Total: ' + IntToStr(FServerStore.RecordCount)
    + ', Root: ' + FServerStore.GetAsJSON(False, LStart, LPageRecordCount) + '}');
end;

procedure TKDynaListFilter.SetConfig(const AConfig: TEFNode);
begin
  inherited;
  //PageSize := 10;
  //Resizable := True;
  //MinHeight := LinesToPixels(5);
  if Config.GetBoolean('Sys/IsReadOnly') then
    Disabled := True
  else
  begin
    //On('change', Ajax(ValueChanged, ['Value', GetEncodedValue()]));
    &On('change', TKWebResponse.Current.Items.AjaxCallMethod(Self).SetMethod(ValueChanged)
      .AddParam('Value', GetEncodedValue).AsFunction);
    //On('select', Ajax(ValueChanged, ['Value', GetEncodedValue()]));
    &On('select', TKWebResponse.Current.Items.AjaxCallMethod(Self).SetMethod(ValueChanged)
      .AddParam('Value', GetEncodedValue).AsFunction);
    On('blur', GenerateAnonymousFunction(Format('fireChangeIfEmpty(%s);', [JSName])));
  end;
  FCurrentValue := AConfig.GetExpandedString('DefaultValue');
  if FCurrentValue <> '' then
    SetRawValue(FCurrentValue);
end;

{ TKFreeSearchFilter }

function TKFreeSearchFilter.AsExtObject: TExtObject;
begin
  Result := Self;
end;

procedure TKFreeSearchFilter.SetConfig(const AConfig: TEFNode);
var
  LAutoSearchAfterChars: Integer;
begin
  Assert(Assigned(AConfig));

  FieldLabel := _(AConfig.AsString);
  Config.Assign(AConfig);

  LAutoSearchAfterChars := AConfig.GetInteger('AutoSearchAfterChars', 0);
  if LAutoSearchAfterChars <> 0 then
  begin
    // Auto-fire change event when at least MinChars characters are typed.
    EnableKeyEvents := True;
    On('keyup', GenerateAnonymousFunction(Format('fireChangeAfterNChars(%s, %d);', [JSName, LAutoSearchAfterChars])));
  end;
  WidthExpression := CharsToPixels(AConfig.GetInteger('Width', DEFAULT_FILTER_WIDTH));
  FCurrentValue := AConfig.GetExpandedString('DefaultValue');
  if FCurrentValue <> '' then
    SetValue(FCurrentValue);
  if Config.GetBoolean('Sys/IsReadOnly') then
    Disabled := True
  else
    OnChange := FieldChange;
end;

procedure TKFreeSearchFilter.SetViewTable(const AViewTable: TKViewTable);
begin
  FViewTable := AViewTable;
end;

function TKFreeSearchFilter.ExpandValues(const AString: string): string;
begin
  Result := AString;
end;

procedure TKFreeSearchFilter.FieldChange(This: TExtFormField; NewValue: string; OldValue: string);
begin
  if FCurrentValue <> NewValue then
  begin
    FCurrentValue := NewValue;
    NotifyObservers('FilterChanged');
  end;
end;

function TKFreeSearchFilter.GetExpression: string;
begin
  if FCurrentValue <> '' then
    Result := ReplaceText(Config.GetExpandedString('ExpressionTemplate'), '{value}', ReplaceStr(FCurrentValue, '''', ''''''))
  else
    Result := '';
end;

function TKFreeSearchFilter.GetId: string;
begin
  Result := Config.GetExpandedString('Id');
end;

procedure TKFreeSearchFilter.Invalidate;
begin
  SetValue('');
end;

{ TKDateSearchFilter }

function TKDateSearchFilter.AsExtObject: TExtObject;
begin
  Result := Self;
end;

procedure TKDateSearchFilter.SetConfig(const AConfig: TEFNode);
var
  LDefaultValue, LFormat: string;
begin
  Assert(Assigned(AConfig));

  FieldLabel := _(AConfig.AsString);
  Config.Assign(AConfig);

  WidthExpression := CharsToPixels(AConfig.GetInteger('Width', 12));
  LDefaultValue := AConfig.GetExpandedString('DefaultValue');
  if LDefaultValue <> '' then
  begin
    FCurrentValue := StrToDate(LDefaultValue, TKWebApplication.Current.Config.UserFormatSettings);
    SetValue(DateToStr(FCurrentValue, TKWebApplication.Current.Config.UserFormatSettings));
  end
  else
    FCurrentValue := 0;
  LFormat := TKWebApplication.Current.Config.UserFormatSettings.ShortDateFormat;
  Format := TJS.DelphiDateFormatToJSDateFormat(LFormat);
  AltFormats := TJS.DelphiDateFormatToJSDateFormat(TKWebApplication.Current.Config.JSFormatSettings.ShortDateFormat);

  if TKWebRequest.Current.IsMobileBrowser then
    Editable := False;

  if Config.GetBoolean('Sys/IsReadOnly') then
    Disabled := True
  else
    OnChange := FieldChange;
end;

procedure TKDateSearchFilter.SetViewTable(const AViewTable: TKViewTable);
begin
  FViewTable := AViewTable;
end;

function TKDateSearchFilter.ExpandValues(const AString: string): string;
begin
  Result := AString;
end;

procedure TKDateSearchFilter.FieldChange(This: TExtFormField; NewValue, OldValue: string);
var
  LNewValue: TDateTime;
begin
  if NewValue <> '' then
    LNewValue := TJS.JSDateToDateTime(NewValue)
  else
    LNewValue := 0;
  if FCurrentValue <> LNewValue then
  begin
    FCurrentValue := LNewValue;
    NotifyObservers('FilterChanged');
  end;
end;

function TKDateSearchFilter.GetExpression: string;
var
  LDBConnection: TEFDBConnection;
  LDateTimeValue: string;
begin
  //A zero date is considered blank
  if FCurrentValue <> 0 then
  begin
    LDBConnection := TKConfig.Instance.CreateDBConnection(GetDatabaseName(Config, Self, FViewTable.DatabaseName));
    try
      LDateTimeValue := LDBConnection.DBEngineType.FormatDateTime(FCurrentValue);
    finally
      FreeAndNil(LDBConnection);
    end;
    Result := ReplaceText(Config.GetExpandedString('ExpressionTemplate'), '{value}', LDateTimeValue);
  end
  else
    Result := '';
end;

function TKDateSearchFilter.GetId: string;
begin
  Result := Config.GetExpandedString('Id');
end;

procedure TKDateSearchFilter.Invalidate;
begin
  SetValue('');
end;

{ TKBooleanSearchFilter }

function TKBooleanSearchFilter.AsExtObject: TExtObject;
begin
  Result := Self;
end;

procedure TKBooleanSearchFilter.SetConfig(const AConfig: TEFNode);
begin
  Assert(Assigned(AConfig));

  FieldLabel := _(AConfig.AsString);
  Config.Assign(AConfig);

  FIsCurrentValue := False;

  if Config.GetBoolean('Sys/IsReadOnly') then
    Disabled := True
  else
    OnCheck := FieldChecked;
end;

procedure TKBooleanSearchFilter.SetViewTable(const AViewTable: TKViewTable);
begin
  FViewTable := AViewTable;
end;

function TKBooleanSearchFilter.ExpandValues(const AString: string): string;
begin
  Result := AString;
end;

procedure TKBooleanSearchFilter.FieldChecked(This: TExtFormCheckBox; Checked: boolean);
begin
  if FIsCurrentValue <> Checked then
  begin
    FIsCurrentValue := Checked;
    NotifyObservers('FilterChanged');
  end;
end;

function TKBooleanSearchFilter.GetExpression: string;
begin
  if FIsCurrentValue then
    Result := Config.GetExpandedString('ExpressionTemplate')
  else
    Result := '';
end;

function TKBooleanSearchFilter.GetId: string;
begin
  Result := Config.GetExpandedString('Id');
end;

procedure TKBooleanSearchFilter.Invalidate;
begin
  SetValue(False);
end;

{ TKButtonListFilterBase }

procedure TKButtonListFilterBase.Invalidate;
begin
  { TODO : implement }
end;

function TKButtonListFilterBase.IsButtonVisible(const AACName: string): Boolean;
var
  LACURI: string;
begin
  Result := True;
  if Assigned(FViewTable) then
  begin
    LACURI := FViewTable.View.GetACURI + '/Filters/' +  GetACName + '/' + AACName;
    Result := TKAccessController.Current.IsAccessGranted(TKAuthenticator.Current.UserName, LACURI, ACM_VIEW);
  end;
end;

function TKButtonListFilterBase.IsSingleSelect: Boolean;
begin
  Result := Config.GetBoolean('IsSingleSelect');
end;

procedure TKButtonListFilterBase.SetConfig(const AConfig: TEFNode);
var
  I: Integer;
  LIsDefaultSet: Boolean;
  LButtons: TArray<TKExtButton>;

  function GetACName(const AIndex: Integer): string;
  begin
    Result := FItems.Children[AIndex].GetExpandedString('ACName');
    if Result = '' then
      Result := FItems.Children[AIndex].GetExpandedString('ResourceName');
  end;

begin
  Assert(Assigned(AConfig));

  FieldLabel := _(AConfig.AsString);
  Config.Assign(AConfig);

  FItems := RetrieveItems;

  Layout := 'column';

  SetLength(FSelected, FItems.ChildCount);
  SetLength(LButtons, FItems.ChildCount);
  LIsDefaultSet := False;
  for I := 0 to FItems.ChildCount - 1 do
  begin
    LButtons[I] := TKExtButton.CreateAndAddToArray(Items);
    LButtons[I].Scale := Config.GetString('ButtonScale', 'small');
    LButtons[I].Text := _(FItems.Children[I].AsString);
    LButtons[I].AllowDepress := not IsSingleSelect;
    LButtons[I].EnableToggle := True;
    if IsButtonVisible(GetACName(I)) then
    begin
      if IsSingleSelect then
        LButtons[I].ToggleGroup := IntToStr(NativeInt(Pointer(Self)));
      // In single select mode, only press the first default button.
      if not IsSingleSelect or not LIsDefaultSet then
      begin
        if FItems.Children[I].GetBoolean('IsDefault') then
        begin
          LButtons[I].Pressed := True;
          FSelected[I] := True;
          LIsDefaultSet := True;
        end;
      end;
      if Config.GetBoolean('Sys/IsReadOnly') then
        LButtons[I].Disabled := True
      else
        LButtons[I].On('click', TKWebResponse.Current.Items.AjaxCallMethod(Self).SetMethod(ButtonClick)
          .AddParam('Index', I)
          .AddParam('Pressed', LButtons[I].Pressed_).AsFunction);
    end
    else
      LButtons[I].Hidden := True;
  end;
end;

procedure TKButtonListFilterBase.SetViewTable(const AViewTable: TKViewTable);
begin
  FViewTable := AViewTable;
end;

function TKButtonListFilterBase.AsExtObject: TExtObject;
begin
  Result := Self;
end;

procedure TKButtonListFilterBase.ButtonClick;
var
  LPressed: Boolean;
  LIndex: Integer;
  I: Integer;
  LSelected: TArray<Boolean>;

  procedure StoreSelection;
  var
    I: Integer;
  begin
    SetLength(LSelected, Length(FSelected));
    for I := Low(FSelected) to High(FSelected) do
      LSelected[I] := FSelected[I];
  end;

  function SelectionChanged: Boolean;
  var
    I: Integer;
  begin
    Result := False;
    for I := Low(FSelected) to High(FSelected) do
      if FSelected[I] <> LSelected[I] then
        Exit(True);
  end;

begin
  LIndex := ParamAsInteger('Index');
  LPressed := ParamAsBoolean('Pressed');
  StoreSelection;
  FSelected[LIndex] := LPressed;
  // Deselect other options.
  if IsSingleSelect then
  begin
    for I := 0 to High(FSelected) do
      if I <> LIndex then
        FSelected[I] := False;
  end;
  if SelectionChanged then
    NotifyObservers('FilterChanged');
end;

function TKButtonListFilterBase.ExpandValues(const AString: string): string;
begin
  Result := AString;
end;

function TKButtonListFilterBase.GetACName: string;
begin
  Result := Config.GetExpandedString('ACName');
  if Result = '' then
    Result := Config.GetExpandedString('ResourceName');
end;

function TKButtonListFilterBase.GetExpression: string;
var
  I: Integer;
  LConnector: string;
  LExpression: string;
begin
  Assert(Assigned(FItems));

  Result := '';
  for I := 0 to High(FSelected) do
  begin
    LConnector := Config.GetString('Connector', 'or');
    if FSelected[I] then
    begin
      LExpression := GetItemExpression(I);
      if LExpression <> '' then
      begin
        LExpression := '(' + LExpression + ')';

        if Result = '' then
          Result := LExpression
        else
          Result := Result + ' ' + LConnector + ' ' + LExpression;
      end;
    end;
  end;
  if Result <> '' then
    Result := '(' + Result + ')';
end;

function TKButtonListFilterBase.GetId: string;
begin
  Result := Config.GetExpandedString('Id');
end;

{ TKButtonListFilter }

function TKButtonListFilter.GetItemExpression(const AItemIndex: Integer): string;
begin
  Result := FItems.Children[AItemIndex].GetExpandedString('Expression');
end;

function TKButtonListFilter.RetrieveItems: TEFNode;
begin
  Result := Config.GetNode('Items');
end;

{ TKDynaButtonListFilter }

destructor TKDynaButtonListFilter.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

function TKDynaButtonListFilter.GetItemExpression(const AItemIndex: Integer): string;
begin
  if FItems[AItemIndex].GetString('Value') <> '' then
    Result := ReplaceText(Config.GetExpandedString('ExpressionTemplate'), '{value}', FItems[AItemIndex].GetExpandedString('Value'))
  else
    Result := '';
end;

function TKDynaButtonListFilter.RetrieveItems: TEFNode;
var
  LDBConnection: TEFDBConnection;
  LDBQuery: TEFDBQuery;
  LItem: TEFNode;
begin
  Result := TEFNode.Create('Items');
  try
    LDBConnection := TKConfig.Instance.CreateDBConnection(GetDatabaseName(Config, Self, FViewTable.DatabaseName));
    try
      LDBQuery := LDBConnection.CreateDBQuery;
      try
        LDBQuery.CommandText := Config.GetExpandedString('CommandText');
        LDBQuery.Open;
        try
          Assert(LDBQuery.DataSet.FieldCount = 2);
          while not LDBQuery.DataSet.Eof do
          begin
            LItem := Result.AddChild('Item', LDBQuery.DataSet.Fields[DISPLAY_FIELD].DisplayText);
            LItem.SetValue('Value', LDBQuery.DataSet.Fields[VALUE_FIELD].Value);
            //LItem.SetInteger('Width', CharsToPixels(AConfig.GetInteger('Width', GetLargestFieldWidth(LDBQuery.DataSet.Fields[DISPLAY_FIELD]) + TRIGGER_WIDTH)));
            LDBQuery.DataSet.Next;
          end;
        finally
          LDBQuery.Close;
        end;
      finally
        FreeAndNil(LDBQuery);
      end;
    finally
      FreeAndNil(LDBConnection);
    end;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

{ TKFilterApplyButton }

function TKFilterApplyButton.AsExtObject: TExtObject;
begin
  Result := Self;
end;

procedure TKFilterApplyButton.ButtonClick;
begin
  NotifyObservers('FilterApplied');
end;

function TKFilterApplyButton.ExpandValues(const AString: string): string;
begin
  Result := AString;
end;

function TKFilterApplyButton.GetExpression: string;
begin
  Result := '';
end;

function TKFilterApplyButton.GetId: string;
begin
  Result := '';
end;

procedure TKFilterApplyButton.Invalidate;
begin
end;

procedure TKFilterApplyButton.SetConfig(const AConfig: TEFNode);
var
  LText: string;
begin
  inherited;
  Assert(Assigned(AConfig));
  LText := AConfig.AsExpandedString;
  if LText = '' then
    LText := _('Apply');
  Text := LText;
  SetIconAndScale(AConfig.GetString('ImageName'), AConfig.GetString('ButtonScale', 'small'));
  //Handler := Ajax(ButtonClick);
  Handler := TKWebResponse.Current.Items.AjaxCallMethod(Self).SetMethod(ButtonClick).AsFunction;
  // The click event is not always fired when the focus is on a text filter,
  // so we increase the probability that the button has the focus when it is clicked.
  &On('mouseover', GenerateAnonymousFunction(JSName + '.focus();'));
end;

procedure TKFilterApplyButton.SetViewTable(const AViewTable: TKViewTable);
begin
end;

{ TKFilterSpacer }

function TKFilterSpacer.AsExtObject: TExtObject;
begin
  Result := Self;
end;

function TKFilterSpacer.ExpandValues(const AString: string): string;
begin
  Result := AString;
end;

function TKFilterSpacer.GetExpression: string;
begin
  Result := '';
end;

function TKFilterSpacer.GetId: string;
begin
  Result := '';
end;

procedure TKFilterSpacer.Invalidate;
begin
end;

procedure TKFilterSpacer.SetConfig(const AConfig: TEFNode);
var
  LCode: string;
begin
  Assert(Assigned(AConfig));

  WidthExpression := CharsToPixels(AConfig.GetInteger('Width', DEFAULT_FILTER_WIDTH));
  // Hide keeping set width.
  Title := '&nbsp;';

  LCode := 'var e = ' + JSName + '.getEl(); if (e) e.setOpacity(0);';
  &On('afterrender', GenerateAnonymousFunction(LCode));
  TKWebResponse.Current.Items.ExecuteJSCode(Self, LCode);
end;

procedure TKFilterSpacer.SetViewTable(const AViewTable: TKViewTable);
begin
end;

{ TKLookupFilter }

function TKLookupFilter.AsExtObject: TExtObject;
begin
  Result := Self;
end;

procedure TKLookupFilter.DoClear;
begin
  FreeAndNil(FCurrentValues);
  SetValue('');
  NotifyObservers('FilterChanged');
end;

destructor TKLookupFilter.Destroy;
begin
  FreeAndNil(FCurrentValues);
  inherited;
end;

function TKLookupFilter.ExpandValues(const AString: string): string;
begin
  Result := AString;
end;

function TKLookupFilter.GetExpression: string;
begin
  if Assigned(FCurrentValues) and (FCurrentValues.RecordCount > 0) then
  begin
    Result := Config.GetExpandedString('ExpressionTemplate');
    FCurrentValues.Records[0].ExpandExpression(Result);
  end
  else
    Result := '';
end;

function TKLookupFilter.GetId: string;
begin
  Result := Config.GetExpandedString('Id');
end;

procedure TKLookupFilter.Invalidate;
begin
  FreeAndNil(FCurrentValues);
  SetValue('');
end;

procedure TKLookupFilter.LookupConfirmed(const ARecord: TKViewTableRecord);
begin
  inherited;
  FreeAndNil(FCurrentValues);
  FCurrentValues := ARecord.ViewTable.CreateStore;
  FCurrentValues.AppendRecord(ARecord);
  SetValue(FCurrentValues.Records[0].FieldByName(ViewField.ModelField.ReferencedModel.CaptionField.FieldName).AsString);
  NotifyObservers('FilterChanged');
end;

procedure TKLookupFilter.SetConfig(const AConfig: TEFNode);
begin
  Assert(Assigned(FViewTable));
  Assert(Assigned(AConfig));

  FieldLabel := _(AConfig.AsString);
  Config.Assign(AConfig);

  WidthExpression := CharsToPixels(AConfig.GetInteger('Width', DEFAULT_FILTER_WIDTH));
  if Config.GetBoolean('Sys/IsReadOnly') then
    ReadOnly := True;
  SetViewField(FViewTable.FieldByAliasedName(Config.GetString('ReferenceFieldName')));
end;

procedure TKLookupFilter.SetViewTable(const AViewTable: TKViewTable);
begin
  FViewTable := AViewTable;
end;

{ TKNumericSearchFilter }

function TKNumericSearchFilter.AsExtObject: TExtObject;
begin
  Result := Self;
end;

function TKNumericSearchFilter.ExpandValues(const AString: string): string;
begin
  Result := AString;
end;

procedure TKNumericSearchFilter.FieldChange(This: TExtFormField; NewValue,
  OldValue: string);
begin
  if FCurrentValue <> NewValue then
  begin
    FCurrentValue := NewValue;
    NotifyObservers('FilterChanged');
  end;
end;

function TKNumericSearchFilter.GetExpression: string;
begin
  if FCurrentValue <> '' then
    Result := ReplaceText(FConfig.GetExpandedString('ExpressionTemplate'), '{value}', ReplaceStr(FCurrentValue, '''', ''''''))
  else
    Result := '';
end;

function TKNumericSearchFilter.GetId: string;
begin
  Assert(Assigned(FConfig));

  Result := FConfig.GetExpandedString('Id');
end;

procedure TKNumericSearchFilter.Invalidate;
begin
  SetValue('');
end;

procedure TKNumericSearchFilter.SetConfig(const AConfig: TEFNode);
var
  LAutoSearchAfterChars: Integer;
begin
  Assert(Assigned(AConfig));
  FConfig := AConfig;

  LAutoSearchAfterChars := AConfig.GetInteger('AutoSearchAfterChars', 0);
  if LAutoSearchAfterChars <> 0 then
  begin
    // Auto-fire change event when at least MinChars characters are typed.
    EnableKeyEvents := True;
    On('keyup', GenerateAnonymousFunction(Format('fireChangeAfterNChars(%s, %d);', [JSName, LAutoSearchAfterChars])));
  end;
  FieldLabel := _(AConfig.AsString);
  WidthExpression := CharsToPixels(AConfig.GetInteger('Width', DEFAULT_FILTER_WIDTH));
  FCurrentValue := AConfig.GetExpandedString('DefaultValue');
  if FCurrentValue <> '' then
    SetValue(FCurrentValue);
  if FConfig.GetBoolean('Sys/IsReadOnly') then
    Disabled := True
  else
    OnChange := FieldChange;
end;

procedure TKNumericSearchFilter.SetViewTable(const AViewTable: TKViewTable);
begin
  FViewTable := AViewTable;
end;

initialization
  TKExtFilterRegistry.Instance.RegisterClass('List', TKListFilter);
  TKExtFilterRegistry.Instance.RegisterClass('DynaList', TKDynaListFilter);
  TKExtFilterRegistry.Instance.RegisterClass('FreeSearch', TKFreeSearchFilter);
  TKExtFilterRegistry.Instance.RegisterClass('ButtonList', TKButtonListFilter);
  TKExtFilterRegistry.Instance.RegisterClass('DynaButtonList', TKDynaButtonListFilter);
  TKExtFilterRegistry.Instance.RegisterClass('DateSearch', TKDateSearchFilter);
  TKExtFilterRegistry.Instance.RegisterClass('BooleanSearch', TKBooleanSearchFilter);
  TKExtFilterRegistry.Instance.RegisterClass('ApplyButton', TKFilterApplyButton);
  TKExtFilterRegistry.Instance.RegisterClass('Spacer', TKFilterSpacer);
  TKExtFilterRegistry.Instance.RegisterClass('Lookup', TKLookupFilter);
  TKExtFilterRegistry.Instance.RegisterClass('NumericSearch', TKNumericSearchFilter);

finalization
  TKExtFilterRegistry.Instance.UnregisterClass('List');
  TKExtFilterRegistry.Instance.UnregisterClass('DynaList');
  TKExtFilterRegistry.Instance.UnregisterClass('FreeSearch');
  TKExtFilterRegistry.Instance.UnregisterClass('ButtonList');
  TKExtFilterRegistry.Instance.UnregisterClass('DynaButtonList');
  TKExtFilterRegistry.Instance.UnregisterClass('DateSearch');
  TKExtFilterRegistry.Instance.UnregisterClass('BooleanSearch');
  TKExtFilterRegistry.Instance.UnregisterClass('ApplyButton');
  TKExtFilterRegistry.Instance.UnregisterClass('Spacer');
  TKExtFilterRegistry.Instance.UnregisterClass('Lookup');
  TKExtFilterRegistry.Instance.UnregisterClass('NumericSearch');

end.
