unit Kitto.Ext.Filters;

interface

uses
  Types, DB,
  ExtPascal, ExtForm, ExtData,
  EF.Classes, EF.Tree, EF.ObserverIntf,
  Kitto.Ext.Base;

type
  IKExtFilter = interface(IEFSubject)
    ['{A7E485F3-FA8B-4446-BC8D-71E92188DA57}']
    function GetExpression: string;
    function AsExtObject: TExtObject;
    procedure SetConfig(const AConfig: TEFNode);
  end;

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
  public
    class destructor Destroy;
  public
    class property Instance: TKExtFilterFactory read GetInstance;

    function CreateFilter(const AFilterConfig: TEFNode; const AObserver: IEFObserver;
      const AContainer: TExtObjectList): IKExtFilter;
  end;

  TKListBaseFilter = class(TKExtFormComboBox)
  protected
    FConfig: TEFNode;
    const TRIGGER_WIDTH = 4;
  public
    procedure SetConfig(const AConfig: TEFNode); virtual;
    function AsExtObject: TExtObject;
  end;

  TKListFilter = class(TKListBaseFilter, IKExtFilter)
  private
    FActiveIndex: Integer;
    FItems: TEFNode;
    function GetLargestFilterDisplayLabelWidth: Integer;
    function GetDefaultFilter: TEFNode;
    procedure ComboBoxSelect(Combo: TExtFormComboBox; RecordJS: TExtDataRecord;
      Index: Integer);
  public
    function GetExpression: string;
    procedure SetConfig(const AConfig: TEFNode); override;
  end;

  TKDynaListFilter = class(TKListBaseFilter, IKExtFilter)
  private
    FValues: TStringDynArray;
    var FCurrentValue: string;
    const VALUE_FIELD = 0;
    const DISPLAY_FIELD = 1;
    function GetLargestFieldWidth(const AField: TField): Integer;
    //procedure ComboBoxChange(This: TExtFormField; NewValue, OldValue: string);
    procedure ComboBoxSelect(Combo: TExtFormComboBox; RecordJS: TExtDataRecord;
      Index: Integer);
  public
    function GetExpression: string;
    procedure SetConfig(const AConfig: TEFNode); override;
  end;


implementation

uses
  SysUtils, Math, StrUtils,
  EF.Localization,  EF.Intf, EF.DB.Utils,
  Kitto.Types, Kitto.JSON, Kitto.Environment;

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

{ TKListBaseFilter }

function TKListBaseFilter.AsExtObject: TExtObject;
begin
  Result := Self;
end;

procedure TKListBaseFilter.SetConfig(const AConfig: TEFNode);
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

function TKListFilter.GetDefaultFilter: TEFNode;
var
  I: Integer;
begin
  Assert(Assigned(FItems));

  Result := nil;
  for I := 0 to FItems.ChildCount - 1 do
  begin
    if FItems.Children[I].GetBoolean('IsDefault') then
    begin
      Result := FItems.Children[I];
      Break;
    end;
  end;
  if (Result = nil) and (FItems.ChildCount > 0) then
    Result := FItems.Children[0];
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
  LDefaultFilter := GetDefaultFilter;
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

procedure TKDynaListFilter.ComboBoxSelect(Combo: TExtFormComboBox;
  RecordJS: TExtDataRecord; Index: Integer);
begin
  FCurrentValue := FValues[Index];
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
  LDBQuery: IEFDBQuery;
begin
  inherited;
  //ForceSelection := True;
  TriggerAction := 'all';
  Editable := True;
  TypeAhead := True;
  LazyRender := True;
  SelectOnFocus := False;
  Mode := 'local';
  LDBQuery := Environment.MainDBConnection.CreateDBQuery;
  try
    LDBQuery.CommandText := FConfig.GetExpandedString('CommandText');
    LDBQuery.Open;
    try
      Assert(LDBQuery.DataSet.FieldCount = 2);
      FValues := GetFieldValuesAsStrings(LDBQuery.DataSet.Fields[VALUE_FIELD]);
      Width := CharsToPixels(GetLargestFieldWidth(LDBQuery.DataSet.Fields[DISPLAY_FIELD]) + TRIGGER_WIDTH);
      { TODO : Future enhancement: make loading optionally dynamic }
      StoreArray := JSArray(DataSetToJSON(LDBQuery.DataSet));
    finally
      LDBQuery.Close;
    end;
  finally
    FreeAndNilEFIntf(LDBQuery);
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
  OnSelect := ComboBoxSelect;
  FCurrentValue := '';
end;

//procedure TKDynaListFilter.ComboBoxChange(This: TExtFormField; NewValue: string; OldValue: string);
//begin
//  FCurrentValue := NewValue;
//  NotifyObservers('FilterChanged');
//end;

initialization
  TKExtFilterRegistry.Instance.RegisterClass('List', TKListFilter);
  TKExtFilterRegistry.Instance.RegisterClass('DynaList', TKDynaListFilter);

finalization
  TKExtFilterRegistry.Instance.UnregisterClass('List');
  TKExtFilterRegistry.Instance.UnregisterClass('DynaList');

end.
