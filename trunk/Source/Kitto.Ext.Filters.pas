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

///	<summary>
///	  Filters are used by the List controller to implement a user interface for
///	  data filtering. Several filters can be combined in a single controller.
///	  Filters are configured in the view, below the Controller node.
///	</summary>
unit Kitto.Ext.Filters;

{$I Kitto.Defines.inc}

interface

uses
  Types, DB,
  Ext, ExtPascal, ExtForm, ExtData,
  EF.Types, EF.Tree, EF.ObserverIntf,
  Kitto.Ext.Base;

type
  ///	<summary>
  ///	  All filters must implement this interface.
  ///	</summary>
  IKExtFilter = interface(IEFSubject)
    ['{A7E485F3-FA8B-4446-BC8D-71E92188DA57}']

    ///	<summary>
    ///	  Builds and returns the filter expression.
    ///	</summary>
    ///	<returns>
    ///	  The expression build according to the filter state.
    ///	</returns>
    ///	<remarks>
    ///	  Returns '' if the filter has no expression in its current state.
    ///	</remarks>
    function GetExpression: string;

    ///	<summary>
    ///	  Returns a reference to the object implementing the filter.
    ///	</summary>
    ///	<remarks>
    ///	  A filter MUST be a TExtObject descendant.
    ///	</remarks>
    function AsExtObject: TExtObject;

    ///	<summary>
    ///	  Called to pass the filter's configuration data, extracted as a node
    ///	  from the view.
    ///	</summary>
    ///	<param name="AConfig">
    ///	  A tree of parameters that configure the filter.
    ///	</param>
    procedure SetConfig(const AConfig: TEFNode);
  end;

  ///	<summary>
  ///	  Keeps track of all registered filter classes. All new filters must be
  ///	  registered before they can be used in controllers.
  ///	</summary>
  TKExtFilterRegistry = class(TEFRegistry)
  private
    class var FInstance: TKExtFilterRegistry;
    class function GetInstance: TKExtFilterRegistry; static;
  protected
    procedure BeforeRegisterClass(const AId: string; const AClass: TClass);
      override;
  public
    class property Instance: TKExtFilterRegistry read GetInstance;
    class destructor Destroy;
    function GetClass(const AId: string): TExtObjectClass;
  end;

  ///	<summary>
  ///	  Queries the registry to create filter objects by class Id. It is
  ///	  friend to TKControllerRegistry.
  ///	</summary>
  TKExtFilterFactory = class(TEFFactory)
  private
    FContainer: TExtObjectList;
    class var FInstance: TKExtFilterFactory;
    class function GetInstance: TKExtFilterFactory; static;
    function CreateObject(const AId: string;
      const AContainer: TExtObjectList): IKExtFilter;
  protected
    function DoCreateObject(const AClass: TClass): TObject; override;
    class destructor Destroy;
  public
    class property Instance: TKExtFilterFactory read GetInstance;

    function CreateFilter(const AFilterConfig: TEFNode; const AObserver: IEFObserver;
      const AContainer: TExtObjectList): IKExtFilter;
  end;

  ///	<summary>
  ///	  Base class for list-based filters.
  ///	</summary>
  TKListFilterBase = class(TKExtFormComboBox)
  protected
    FConfig: TEFNode;
    const TRIGGER_WIDTH = 4;
  public
    procedure SetConfig(const AConfig: TEFNode); virtual;
    function AsExtObject: TExtObject;
  end;

  ///	<summary>Static list. Displays a combo box with a list of mutually
  ///	exclusive filter expressions. When an item is selected, the corresponding
  ///	expression is applied.</summary>
  ///	<remarks>
  ///	  <para>If no item has IsDefault set to True, then the first item is the
  ///	  default item.</para>
  ///	  <para>An empty Expression means no filter (thus, all records
  ///	  retrieved).</para>
  ///	</remarks>
  ///	<example>
  ///	  <code>
  ///	Items:
  ///	  List: Shipping date
  ///	    Items:
  ///	      Today: Goods shipped today
  ///	        Expression: datediff(day, GOODS.SHIPPING_DATE, getdate()) = 0
  ///	      Everything: Everything else
  ///	        IsDefault: True
  ///	  </code>
  ///	</example>
  TKListFilter = class(TKListFilterBase, IKExtFilter)
  private
    FActiveIndex: Integer;
    FItems: TEFNode;
    function GetLargestFilterDisplayLabelWidth: Integer;
    procedure ComboBoxSelect(Combo: TExtFormComboBox; RecordJS: TExtDataRecord; Index: Integer);
  public
    function GetExpression: string;
    procedure SetConfig(const AConfig: TEFNode); override;
  end;

  ///	<summary>Dynamic list. Displays a list of items obtained by executing a
  ///	query. When an item is selected, the corresponding value is merged into a
  ///	specified expression template to build the filter expression.</summary>
  ///	<example>
  ///	  <code>
  ///	Items:
  ///	  DynaList: Employee
  ///	    CommandText: |
  ///	      select distinct SSN, LAST_NAME + ' ' + FIRST_NAME + ' (' + SSN + ')'
  ///	      from CERTIFICATES order by 1
  ///	    ExpressionTemplate: SSN = '{value}'
  ///	</code>
  ///	</example>
  TKDynaListFilter = class(TKListFilterBase, IKExtFilter)
  private
    FValues: TStringDynArray;
    FCurrentValue: string;
    const VALUE_FIELD = 0;
    const DISPLAY_FIELD = 1;
    function GetLargestFieldWidth(const AField: TField): Integer;
//    procedure ComboBoxChange(This: TExtFormField; NewValue, OldValue: string);
//    procedure ComboBoxSelect(Combo: TExtFormComboBox; RecordJS: TExtDataRecord;
//      Index: Integer);
  public
    function GetExpression: string;
    procedure SetConfig(const AConfig: TEFNode); override;
  published
    procedure Select;
  end;

  TKFreeSearchFilter = class(TKExtFormTextField, IKExtFilter)
  private
    FCurrentValue: string;
    FConfig: TEFNode;
    procedure FieldChange(This: TExtFormField; NewValue, OldValue: string);
  public
    procedure SetConfig(const AConfig: TEFNode);
    function AsExtObject: TExtObject;
    function GetExpression: string;
  end;

  ///	<summary>
  ///	  <para>Similar to a ListFilter, but uses a set of buttons instead of a
  ///	  combo box.</para>
  ///	  <para>The difference is that several buttons can be pressed at once, in
  ///	  which case their expressions are concatenated with a Connector (default
  ///	  'or').</para>
  ///	  <para>All filters with IsDefault set to True are renderd as initially
  ///	  pressed buttons.</para>
  ///	</summary>
{ TODO :
Add parameter (or separate filter class) to allow several buttons
pressed at the same time to generate a combined expression
through a connector. }
  TKButtonListFilter = class(TKExtPanelBase, IKExtFilter)
  private
    FConfig: TEFNode;
    FItems: TEFNode;
    FSelected: array of Boolean;
  public
    procedure SetConfig(const AConfig: TEFNode);
    function AsExtObject: TExtObject;
    function GetExpression: string;
  published
    procedure ButtonClick;
  end;

implementation

uses
  SysUtils, Math, StrUtils,
  EF.Localization,  EF.DB, EF.StrUtils,
  Kitto.Types, Kitto.JSON, Kitto.Ext.Session;

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
  if (Result = nil) and (AItems.ChildCount > 0) then
    Result := AItems.Children[0];
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
  const AObserver: IEFObserver; const AContainer: TExtObjectList): IKExtFilter;
begin
  Assert(AFilterConfig <> nil);
  Assert(Assigned(AContainer));

  Result := CreateObject(AFilterConfig.Name, AContainer);
  if Assigned(AObserver) then
    Result.AttachObserver(AObserver);
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
  Result := TExtObjectClass(AClass).AddTo(FContainer);
  // If this AddTo call is ever changed to Create, don't forget to add a call
  // to InitDefaults because TExtObject.Create doesn't do that.
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

procedure TKListFilterBase.SetConfig(const AConfig: TEFNode);
begin
  Assert(Assigned(AConfig));

  FConfig := AConfig;
  FieldLabel := _(AConfig.AsString);
end;

{ TKListFilter }

function TKListFilter.GetLargestFilterDisplayLabelWidth: Integer;
var
  I: Integer;
  LLength: Integer;
begin
  Assert(Assigned(FItems));

  Result := 10;
  for I := 0 to FItems.ChildCount - 1 do
  begin
    LLength := Length(FItems.Children[I].AsString);
    if LLength > Result then
      Result := LLength;
  end;
end;

procedure TKListFilter.SetConfig(const AConfig: TEFNode);
var
  LDefaultFilter: TEFNode;
begin
  inherited;
  FItems := FConfig.GetNode('Items');
  Assert(Assigned(FItems));

  Width := CharsToPixels(GetLargestFilterDisplayLabelWidth + TRIGGER_WIDTH);
  //ForceSelection := True;
  TriggerAction := 'all';
  Editable := False;
  LazyRender := True;
  SelectOnFocus := False;
  Mode := 'local';
  { TODO : design a naming scheme that allows master/details and maybe more instances of the same view }
  StoreArray := JSArray(PairsToJSON(FItems.GetChildPairs, False));
  //PageSize := 10;
  //Resizable := True;
  //MinListWidth := LFieldWidth;
  //MinHeight := LinesToPixels(5);
{ TODO :
In order to save a trip by calling the refresh code directly,
we should include status information from all filters. Doable,
by generating more JS code, but not now. }
  //On('select', JSFunction(AConfig.GetString('Sys/ApplyJSCode')));
  OnSelect := ComboBoxSelect;
  LDefaultFilter := GetDefaultFilter(FItems);
  if Assigned(LDefaultFilter) then
  begin
    SetValue(LDefaultFilter.Name);
    FActiveIndex := LDefaultFilter.Index;
  end
  else if FConfig.ChildCount > 0 then
    FActiveIndex := 0
  else
    FActiveIndex := -1;
end;

procedure TKListFilter.ComboBoxSelect(Combo: TExtFormComboBox; RecordJS: TExtDataRecord; Index: Integer);
begin
  FActiveIndex := Index;
  NotifyObservers('FilterChanged');
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

//procedure TKDynaListFilter.ComboBoxSelect(Combo: TExtFormComboBox;
//  RecordJS: TExtDataRecord; Index: Integer);
//begin
//  FCurrentValue := FValues[Index];
//  NotifyObservers('FilterChanged');
//end;

procedure TKDynaListFilter.Select;
begin
  FCurrentValue := ParamAsString('Value');
  NotifyObservers('FilterChanged');
end;

function TKDynaListFilter.GetExpression: string;
begin
  if FCurrentValue <> '' then
    Result := ReplaceText(FConfig.GetExpandedString('ExpressionTemplate'), '{value}', FCurrentValue)
  else
    Result := '';
end;

function TKDynaListFilter.GetLargestFieldWidth(const AField: TField): Integer;
var
  LWidth: Integer;
begin
  Assert(Assigned(AField));

  Result := 10;
  AField.DataSet.First;
  while not AField.DataSet.Eof do
  begin
    LWidth := Length(AField.AsString);
    if LWidth > Result then
      Result := LWidth;
    AField.DataSet.Next;
  end;
  Result := Min(80, Result);
end;

procedure TKDynaListFilter.SetConfig(const AConfig: TEFNode);
var
  LDBQuery: TEFDBQuery;
begin
  inherited;
  //ForceSelection := True;
  TriggerAction := 'all';
  Editable := True;
  TypeAhead := True;
  LazyRender := True;
  SelectOnFocus := True;
  AutoSelect := False;
  ForceSelection := True;
  Mode := 'local';
  LDBQuery := Session.Config.MainDBConnection.CreateDBQuery;
  try
    LDBQuery.CommandText := FConfig.GetExpandedString('CommandText');
    LDBQuery.Open;
    try
      Assert(LDBQuery.DataSet.FieldCount = 2);
      FValues := LDBQuery.GetFieldValuesAsStrings(LDBQuery.DataSet.Fields[VALUE_FIELD]);
      Width := CharsToPixels(GetLargestFieldWidth(LDBQuery.DataSet.Fields[DISPLAY_FIELD]) + TRIGGER_WIDTH);
      { TODO : Future enhancement: make loading optionally dynamic }
      StoreArray := JSArray(DataSetToJSON(LDBQuery.DataSet));
    finally
      LDBQuery.Close;
    end;
  finally
    FreeAndNil(LDBQuery);
  end;
  //PageSize := 10;
  //Resizable := True;
  //MinListWidth := LFieldWidth;
  //MinHeight := LinesToPixels(5);
{ TODO :
In order to save a trip by calling the refresh code directly,
we should include status information from all filters. Doable,
by generating more JS code, but not now. }
  //On('select', JSFunction(AConfig.GetString('Sys/ApplyJSCode')));
  //OnChange := ComboBoxChange;
  // OnSelect only passes the relative index in the filtered list.
  //OnSelect := ComboBoxSelect;
  On('select', Ajax(Select, ['Value', GetValue()]));
  FCurrentValue := '';
end;

//procedure TKDynaListFilter.ComboBoxChange(This: TExtFormField; NewValue: string; OldValue: string);
//begin
//  FCurrentValue := NewValue;
//  NotifyObservers('FilterChanged');
//end;

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
  Width := CharsToPixels(AConfig.GetInteger('Width', 20));
  OnChange := FieldChange;
  FCurrentValue := '';
end;

procedure TKFreeSearchFilter.FieldChange(This: TExtFormField; NewValue: string; OldValue: string);
begin
  FCurrentValue := NewValue;
  NotifyObservers('FilterChanged');
end;

function TKFreeSearchFilter.GetExpression: string;
begin
  if FCurrentValue <> '' then
    Result := ReplaceText(FConfig.GetExpandedString('ExpressionTemplate'), '{value}', FCurrentValue)
  else
    Result := '';
end;

{ TKButtonListFilter }

function TKButtonListFilter.AsExtObject: TExtObject;
begin
  Result := Self;
end;

function TKButtonListFilter.GetExpression: string;
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
      LExpression := '(' + FItems.Children[I].GetExpandedString('Expression') + ')';
      if Result = '' then
        Result := LExpression
      else
        Result := Result + ' ' + LConnector + ' ' + LExpression;
    end;
  end;
  if Result <> '' then
    Result := '(' + Result + ')';
end;

procedure TKButtonListFilter.SetConfig(const AConfig: TEFNode);
var
  I: Integer;
  LButton: TExtButton;
  //LGroupName: string;
begin
  Assert(Assigned(AConfig));

  FConfig := AConfig;
  FieldLabel := _(AConfig.AsString);

  FItems := FConfig.GetNode('Items');
  Assert(Assigned(FItems));

  Layout := lyColumn;
  //LGroupName := IntToStr(Integer(Pointer(Self)));

  SetLength(FSelected, FItems.ChildCount);
  for I := 0 to FItems.ChildCount - 1 do
  begin
    LButton := TExtButton.AddTo(Items);
    LButton.Text := _(FItems.Children[I].AsString);
    LButton.AllowDepress := True;
    LButton.EnableToggle := True;
    //LButton.ToggleGroup := LGroupName;
    if FItems.Children[I].GetBoolean('IsDefault') then
    begin
      LButton.Pressed_ := True;
      FSelected[I] := True;
    end;
{ TODO :
In order to save a trip by calling the refresh code directly,
we should include status information from all filters. Doable,
by generating more JS code, but not now. }
  //LButton.On('click', JSFunction(AConfig.GetString('Sys/ApplyJSCode')));
    LButton.On('click', Ajax(ButtonClick, ['Index', I, 'Pressed', LButton.Pressed__]));
  end;
end;

procedure TKButtonListFilter.ButtonClick;
begin
  FSelected[ParamAsInteger('Index')] := ParamAsBoolean('Pressed');
  NotifyObservers('FilterChanged');
end;

initialization
  TKExtFilterRegistry.Instance.RegisterClass('List', TKListFilter);
  TKExtFilterRegistry.Instance.RegisterClass('DynaList', TKDynaListFilter);
  TKExtFilterRegistry.Instance.RegisterClass('FreeSearch', TKFreeSearchFilter);
  TKExtFilterRegistry.Instance.RegisterClass('ButtonList', TKButtonListFilter);

finalization
  TKExtFilterRegistry.Instance.UnregisterClass('List');
  TKExtFilterRegistry.Instance.UnregisterClass('DynaList');
  TKExtFilterRegistry.Instance.UnregisterClass('FreeSearch');
  TKExtFilterRegistry.Instance.UnregisterClass('ButtonList');

end.
