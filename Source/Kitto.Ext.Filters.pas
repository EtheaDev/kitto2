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

/// <summary>
///  Filters are used by the List controller to implement a user interface for
///  data filtering. Several filters can be combined in a single controller.
///  Filters are configured in the view, below the Controller node.
/// </summary>
unit Kitto.Ext.Filters;

{$I Kitto.Defines.inc}

interface

uses
  Types, DB,
  Ext, ExtPascal, ExtForm, ExtData,
  EF.Types, EF.Tree, EF.ObserverIntf,
  Kitto.DatabaseRouter, Kitto.Store, Kitto.Metadata.DataView, Kitto.Ext.Base;

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
    FContainer: TExtObjectList;
    class var FInstance: TKExtFilterFactory;
    class function GetInstance: TKExtFilterFactory; static;
    function CreateObject(const AId: string; const AContainer: TExtObjectList): IKExtFilter;
  protected
    function DoCreateObject(const AClass: TClass): TObject; override;
    class destructor Destroy;
  public
    class property Instance: TKExtFilterFactory read GetInstance;

    function CreateFilter(const AFilterConfig: TEFNode; const AObserver: IEFObserver;
      const AContainer: TExtObjectList; const AViewTable: TKViewTable): IKExtFilter;
  end;

  /// <summary>
  ///  Base class for list-based filters.
  /// </summary>
  TKListFilterBase = class(TKExtFormComboBox)
  strict protected
    FConfig: TEFNode;
    FViewTable: TKViewTable;
    FServerStore: TKStore;
    const TRIGGER_WIDTH = 4;
  public
    procedure SetConfig(const AConfig: TEFNode); virtual;
    function AsExtObject: TExtObject;
    procedure SetViewTable(const AViewTable: TKViewTable);
    function GetId: string;
    procedure Invalidate;
  published
    procedure GetRecordPage;
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
  published
    procedure ValueChanged;
  end;

  /// <summary>
  ///  <para>
  ///   Free search. Displays an edit for a free search criteria input.
  ///  </para>
  ///  <para>
  ///   AutoSearchAfterChars determines the number of characters that can
  ///   be entered before the search fires. Default is 4 characters.
  ///  </para>
  /// </summary>
  TKFreeSearchFilter = class(TKExtFormTextField, IKExtFilter)
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

  /// <summary>
  ///  <para>
  ///   Date search. Displays a calendar edit for a search of a date value.
  ///  </para>
  /// </summary>
  TKDateSearchFilter = class(TKExtFormDateField, IKExtFilter)
  strict private
    FCurrentValue: TDateTime;
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

  /// <summary>
  ///  <para>
  ///   Boolean search. Displays a checkbox for activate a particular filter
  ///   defined in Expression Template.
  ///  </para>
  /// </summary>
  TKBooleanSearchFilter = class(TKExtFormCheckBoxField, IKExtFilter)
  strict private
    FCurrentValue: Boolean;
    FConfig: TEFNode;
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

  TKButtonListFilterBase = class(TKExtPanelBase)
  strict private
    FSelected: TArray<Boolean>;
  strict protected
    FConfig: TEFNode;
    FItems: TEFNode;
    FViewTable: TKViewTable;
    function RetrieveItems: TEFNode; virtual; abstract;
    function GetItemExpression(const AItemIndex: Integer): string; virtual; abstract;
    function IsSingleSelect: Boolean;
    function IsButtonVisible(const AResourceName: string): Boolean; virtual;
  public
    procedure SetConfig(const AConfig: TEFNode);
    function AsExtObject: TExtObject;
    function GetExpression: string;
    procedure SetViewTable(const AViewTable: TKViewTable);
    function ExpandValues(const AString: string): string;
    function GetId: string;
    procedure Invalidate;
  published
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

  TKFilterApplyButton = class(TKExtButton, IKExtFilter)
  public
    function GetExpression: string;
    procedure SetConfig(const AConfig: TEFNode);
    function AsExtObject: TExtObject;
    procedure SetViewTable(const AViewTable: TKViewTable);
    function ExpandValues(const AString: string): string;
    function  GetId: string;
    procedure Invalidate;
  published
    procedure ButtonClick;
  end;

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

implementation

uses
  SysUtils, Math, StrUtils,
  ExtPascalUtils,
  EF.Localization,  EF.DB, EF.StrUtils, EF.JSON,
  Kitto.Types, Kitto.Config, KItto.AccessControl, Kitto.Ext.Session, Kitto.Ext.Utils;

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

function GetDatabaseName(const AConfig: TEFNode; const ACallerContext: TObject;
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

function ExpandFilterValues(const AList: TExtObjectList; const AString: string): string;
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
  const AObserver: IEFObserver; const AContainer: TExtObjectList;
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
  const AContainer: TExtObjectList): IKExtFilter;
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
  Result := TExtObjectClass(AClass).CreateAndAddTo(FContainer);
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

function TKListFilterBase.GetId: string;
begin
  Assert(Assigned(FConfig));

  Result := FConfig.GetExpandedString('Id');
end;

procedure TKListFilterBase.GetRecordPage;
var
  LStart: Integer;
  LLimit: Integer;
  LPageRecordCount: Integer;
  LDBQuery: TEFDBQuery;
begin
  Assert(Assigned(FServerStore));

  LDBQuery := Session.Config.DBConnections[GetDatabaseName(FConfig, Self, FViewTable.DatabaseName)].CreateDBQuery;
  try
    LDBQuery.CommandText := ExpandFilterValues(Owner as TExtObjectList, FConfig.GetExpandedString('CommandText'));
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

  LStart := Session.QueryAsInteger['start'];
  LLimit := Session.QueryAsInteger['limit'];
  LPageRecordCount := Min(LLimit, FServerStore.RecordCount - LStart);

  ExtSession.ResponseItems.AddJSON('{Total: ' + IntToStr(FServerStore.RecordCount)
    + ', Root: ' + FServerStore.GetAsJSON(False, LStart, LPageRecordCount) + '}');
end;

procedure TKListFilterBase.Invalidate;
begin
  SetRawValue('');
  // Force the combo to refresh its list at next drop down.
  Store.RemoveAll();
  Store.TotalLength := 0;
  Session.ResponseItems.ExecuteJSCode(Format('%s.lastQuery = null;', [JSName]));
end;

procedure TKListFilterBase.SetConfig(const AConfig: TEFNode);
var
  I: Integer;
begin
  Assert(Assigned(AConfig));

  FConfig := AConfig;
  FieldLabel := _(AConfig.AsString);
  TriggerAction := 'all';
  Editable := True;
  LazyRender := True;
  SelectOnFocus := True;
  AutoSelect := False;
  AllowBlank := True;
  Mode := 'remote';
  FServerStore := TKStore.Create;
  Store := TExtDataStore.Create(Self);
  FServerStore.Header.AddField('Id');
  FServerStore.Header.AddField('Description');
  FServerStore.Key.SetFieldNames(['Id']);
  Store.Url := MethodURI(GetRecordPage);
  Store.Reader := TExtDataJsonReader.Create(Self, JSObject('')); // Must pass '' otherwise invalid code is generated.
  TExtDataJsonReader(Store.Reader).Root := 'Root';
  TExtDataJsonReader(Store.Reader).TotalProperty := 'Total';
  for I := 0 to FServerStore.Header.FieldCount - 1 do
    with TExtDataField.CreateAndAddTo(Store.Reader.Fields) do
      Name := FServerStore.Header.Fields[I].FieldName;
  ValueField := 'Id';
  DisplayField := 'Description';
  Width := CharsToPixels(FConfig.GetInteger('Width', DEFAULT_FILTER_WIDTH));
  TypeAhead := True;
  MinChars := FConfig.GetInteger('AutoCompleteMinChars', 4);
end;

procedure TKListFilterBase.SetViewTable(const AViewTable: TKViewTable);
begin
  FViewTable := AViewTable;
end;

{ TKListFilter }

procedure TKListFilter.SetConfig(const AConfig: TEFNode);
var
  LDefaultFilter: TEFNode;
begin
  inherited;
  FItems := FConfig.GetNode('Items');
  Assert(Assigned(FItems));
  //PageSize := 10;
  //Resizable := True;
  //MinListWidth := LFieldWidth;
  //MinHeight := LinesToPixels(5);
  LDefaultFilter := GetDefaultFilter(FItems);
  if Assigned(LDefaultFilter) then
  begin
    SetValue(LDefaultFilter.Name);
    FActiveIndex := LDefaultFilter.Index;
  end
  else
    FActiveIndex := -1;
  if FConfig.GetBoolean('Sys/IsReadOnly') then
    Disabled := True
  else
  begin
    OnSelect := ComboBoxSelect;
    OnChange := ComboBoxChange;
  end;
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
    for LInvalidate in FConfig.GetStringArray('Invalidates') do
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
    Result := ReplaceText(FConfig.GetExpandedString('ExpressionTemplate'), '{value}', FCurrentValue)
  else
    Result := '';
end;

procedure TKDynaListFilter.SetConfig(const AConfig: TEFNode);
begin
  inherited;
  //PageSize := 10;
  //Resizable := True;
  //MinListWidth := LFieldWidth;
  //MinHeight := LinesToPixels(5);
  if FConfig.GetBoolean('Sys/IsReadOnly') then
    Disabled := True
  else
  begin
    On('change', Ajax(ValueChanged, ['Value', GetEncodedValue()]));
    On('select', Ajax(ValueChanged, ['Value', GetEncodedValue()]));
    On('blur', JSFunction(Format('fireChangeIfEmpty(%s);', [JSName])));
  end;
  FCurrentValue := '';
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
  FConfig := AConfig;

  LAutoSearchAfterChars := AConfig.GetInteger('AutoSearchAfterChars', 4);
  if LAutoSearchAfterChars <> 0 then
  begin
    // Auto-fire change event when at least MinChars characters are typed.
    EnableKeyEvents := True;
    On('keyup', JSFunction(Format('fireChangeAfterNChars(%s, %d);', [JSName, LAutoSearchAfterChars])));
  end;
  FieldLabel := _(AConfig.AsString);
  Width := CharsToPixels(AConfig.GetInteger('Width', DEFAULT_FILTER_WIDTH));
  FCurrentValue := '';
  if FConfig.GetBoolean('Sys/IsReadOnly') then
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
    Result := ReplaceText(FConfig.GetExpandedString('ExpressionTemplate'), '{value}', ReplaceStr(FCurrentValue, '''', ''''''))
  else
    Result := '';
end;

function TKFreeSearchFilter.GetId: string;
begin
  Assert(Assigned(FConfig));

  Result := FConfig.GetExpandedString('Id');
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
  LFormat: string;
begin
  Assert(Assigned(AConfig));
  FConfig := AConfig;
  FieldLabel := _(AConfig.AsString);
  Width := CharsToPixels(AConfig.GetInteger('Width', 12));
  FCurrentValue := 0;
  LFormat := Session.Config.UserFormatSettings.ShortDateFormat;
  Format := DelphiDateFormatToJSDateFormat(LFormat);
  AltFormats := DelphiDateFormatToJSDateFormat(Session.Config.JSFormatSettings.ShortDateFormat);

  if Session.IsMobileBrowser then
    Editable := False;

  if FConfig.GetBoolean('Sys/IsReadOnly') then
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
    LNewValue := JSDateToDateTime(NewValue)
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
  LDateTimeValue: string;
begin
  //A zero date is considered blank
  if FCurrentValue <> 0 then
  begin
    LDateTimeValue := Session.Config.DBConnections[GetDatabaseName(FConfig, Self, FViewTable.DatabaseName)].DBEngineType.FormatDateTime(FCurrentValue);
    Result := ReplaceText(FConfig.GetExpandedString('ExpressionTemplate'), '{value}', LDateTimeValue);
  end
  else
    Result := '';
end;

function TKDateSearchFilter.GetId: string;
begin
  Assert(Assigned(FConfig));

  Result := FConfig.GetExpandedString('Id');
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
  FConfig := AConfig;
  FieldLabel := _(AConfig.AsString);
  FCurrentValue := False;

  if FConfig.GetBoolean('Sys/IsReadOnly') then
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
  if FCurrentValue <> Checked then
  begin
    FCurrentValue := Checked;
    NotifyObservers('FilterChanged');
  end;
end;

function TKBooleanSearchFilter.GetExpression: string;
begin
  //A zero date is considered blank
  if FCurrentValue then
    Result := FConfig.GetExpandedString('ExpressionTemplate')
  else
    Result := '';
end;

function TKBooleanSearchFilter.GetId: string;
begin
  Assert(Assigned(FConfig));

  Result := FConfig.GetExpandedString('Id');
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

function TKButtonListFilterBase.IsButtonVisible(const AResourceName: string): Boolean;
var
  LResourceURI: string;
begin
  Result := True;
  if Assigned(FViewTable) and Assigned(FConfig) then
  begin
    LResourceURI := FViewTable.View.GetResourceURI + '/Filters/' +  FConfig.GetExpandedString('ResourceName') + '/' + AResourceName;
    Result := TKConfig.Instance.IsAccessGranted(LResourceURI, ACM_VIEW);
  end;
end;

function TKButtonListFilterBase.IsSingleSelect: Boolean;
begin
  Result := FConfig.GetBoolean('IsSingleSelect');
end;

procedure TKButtonListFilterBase.SetConfig(const AConfig: TEFNode);
var
  I: Integer;
  LIsDefaultSet: Boolean;
  LButtons: TArray<TKExtButton>;
begin
  Assert(Assigned(AConfig));

  FConfig := AConfig;
  FieldLabel := _(AConfig.AsString);

  FItems := RetrieveItems;

  Layout := lyColumn;

  SetLength(FSelected, FItems.ChildCount);
  SetLength(LButtons, FItems.ChildCount);
  LIsDefaultSet := False;
  for I := 0 to FItems.ChildCount - 1 do
  begin
    LButtons[I] := TKExtButton.CreateAndAddTo(Items);
    LButtons[I].Scale := Config.GetString('ButtonScale', 'small');
    LButtons[I].Text := _(FItems.Children[I].AsString);
    LButtons[I].AllowDepress := not IsSingleSelect;
    LButtons[I].EnableToggle := True;
    if IsButtonVisible(FItems.Children[I].GetExpandedString('ResourceName')) then
    begin
      if IsSingleSelect then
        LButtons[I].ToggleGroup := IntToStr(Integer(Pointer(Self)));
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
      if FConfig.GetBoolean('Sys/IsReadOnly') then
        LButtons[I].Disabled := True
      else
        LButtons[I].On('click', Ajax(ButtonClick, ['Index', I, 'Pressed', LButtons[I].Pressed_]));
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
    LConnector := FConfig.GetString('Connector', 'or');
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
  Assert(Assigned(FConfig));

  Result := FConfig.GetExpandedString('Id');
end;

{ TKButtonListFilter }

function TKButtonListFilter.GetItemExpression(const AItemIndex: Integer): string;
begin
  Result := FItems.Children[AItemIndex].GetExpandedString('Expression');
end;

function TKButtonListFilter.RetrieveItems: TEFNode;
begin
  Result := FConfig.GetNode('Items');
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
    Result := ReplaceText(FConfig.GetExpandedString('ExpressionTemplate'), '{value}', FItems[AItemIndex].GetExpandedString('Value'))
  else
    Result := '';
end;

function TKDynaButtonListFilter.RetrieveItems: TEFNode;
var
  LDBQuery: TEFDBQuery;
  LItem: TEFNode;
begin
  Result := TEFNode.Create('Items');
  try
    LDBQuery := Session.Config.DBConnections[GetDatabaseName(FConfig, Self, FViewTable.DatabaseName)].CreateDBQuery;
    try
      LDBQuery.CommandText := FConfig.GetExpandedString('CommandText');
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
  On('click', Ajax(ButtonClick));
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
begin
  Assert(Assigned(AConfig));

  Width := CharsToPixels(AConfig.GetInteger('Width', DEFAULT_FILTER_WIDTH));
  // Hide keeping set width.
  Title := '&nbsp;';
  //On('afterrender', JSFunction(JSName + '.getEl().setOpacity(0);'));
  Session.ResponseItems.ExecuteJSCode(Self, JSName + '.getEl().setOpacity(0);');
end;

procedure TKFilterSpacer.SetViewTable(const AViewTable: TKViewTable);
begin
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

end.
