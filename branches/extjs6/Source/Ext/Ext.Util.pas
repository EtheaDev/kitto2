unit Ext.Util;

interface

uses
  Classes, StrUtils, Kitto.Ext;

type
  TExtUtilObservable = class;
  TExtUtilJSONSingleton = class;
  TExtUtilTextMetricsSingleton = class;
  TExtUtilFormatSingleton = class;
  TExtUtilCSSSingleton = class;
  TExtUtilCookiesSingleton = class;
  TExtUtilDelayedTask = class;

  TExtUtilObservable = class(TExtFunction)
  private
    FListeners: TExtObject;
    procedure SetFListeners(Value: TExtObject);
  protected
    procedure InitDefaults; override;
  public
    class function JSClassName: string; override;
    function ObservableCapture(O: TExtUtilObservable; Fn: TExtFunction;
      Scope: TExtObject = nil): TExtFunction;
    function ObservableObserveClass(C: TExtFunction; Listeners: TExtObject): TExtFunction;
    function ObservableReleaseCapture(O: TExtUtilObservable): TExtFunction;
    function AddEvents(O: TExtObject; Optional: string): TExtFunction; overload;
    function AddEvents(O: string; Optional: string): TExtFunction; overload;
    function AddListener(const AEventName: string; const AHandler: TExtFunction;
      const AScope: TExtObject = nil; const AOptions: TExtObject = nil): TExtFunction;
    function EnableBubble(Events: string): TExtFunction; overload;
    function EnableBubble(Events: TExtObjectList): TExtFunction; overload;
    function FireEvent(const AEventName: string; const AArgs: TExtObjectList): TExtFunction;
    function HasListener(EventName: string): TExtFunction;
    function &On(const AEventName: string; const AHandler: TExtFunction;
      const AScope: TExtObject = nil; const AOptions: TExtObject = nil): TExtFunction;
    function RemoveAllListeners(const AEventName: string): TExtFunction;
    function PurgeListeners: TExtFunction;
    function RelayEvents(O: TExtObject; Events: TExtObjectList): TExtFunction;
    function RemoveListener(EventName: string; Handler: TExtFunction;
      Scope: TExtObject = nil): TExtFunction;
    function ResumeEvents: TExtFunction;
    function SuspendEvents(QueueSuspended: Boolean): TExtFunction;
    function Un(EventName: string; Handler: TExtFunction; Scope: TExtObject = nil)
      : TExtFunction;
    property Listeners: TExtObject read FListeners write SetFListeners;
  end;

  TExtUtilJSONSingleton = class(TExtFunction)
  public
    class function JSClassName: string; override;
    function Decode(Json: string): TExtFunction;
    function Encode(O: string): TExtFunction;
    function EncodeDate(D: TDateTime): TExtFunction;
  end;

  TExtUtilTextMetricsSingleton = class(TExtFunction)
  public
    class function JSClassName: string; override;
    function Bind(El: string): TExtFunction; overload;
    function CreateInstance(El: string; FixedWidth: Integer = 0): TExtFunction; overload;
    function GetHeight(const AText: string): TExtFunction;
    function GetSize(Text: string): TExtFunction;
    function GetWidth(const AText: string): TExtFunction;
    function Measure(El: string; Text: string; FixedWidth: Integer = 0)
      : TExtFunction; overload;
    function SetFixedWidth(Width: Integer): TExtFunction;
  end;

  TExtUtilFormatSingleton = class(TExtFunction)
  public
    class function JSClassName: string; override;
    function Capitalize(Value: string): TExtFunction;
    function Date(Value: string; Format: string = ''): TExtFunction; overload;
    function Date(Value: TDateTime; Format: string = ''): TExtFunction; overload;
    function DateRenderer(Format: string): TExtFunction;
    function DefaultValue(Value: string; DefaultValue: string): TExtFunction;
    function Ellipsis(Value: string; Length: Integer; Word: Boolean): TExtFunction;
    function FileSize(Size: Integer): TExtFunction; overload;
    function FileSize(Size: string): TExtFunction; overload;
    function HtmlDecode(Value: string): TExtFunction;
    function HtmlEncode(Value: string): TExtFunction;
    function Lowercase(Value: string): TExtFunction;
    function Math: TExtFunction;
    function Nl2br(The: string): TExtFunction;
    function Number(V: Integer; Format: string): TExtFunction;
    function NumberRenderer(Format: string): TExtFunction;
    function Plural(Value: Integer; Singular: string; Plural: string = ''): TExtFunction;
    function Round(Value: Integer; Precision: Integer): TExtFunction; overload;
    function Round(Value: string; Precision: Integer): TExtFunction; overload;
    function StripScripts(Value: string): TExtFunction;
    function StripTags(Value: string): TExtFunction;
    function Substr(Value: string; Start: Integer; Length: Integer): TExtFunction;
    function Trim(Value: string): TExtFunction;
    function Undef(Value: string): TExtFunction;
    function Uppercase(Value: string): TExtFunction;
    function UsMoney(Value: Integer): TExtFunction; overload;
    function UsMoney(Value: string): TExtFunction; overload;
  end;

  TExtUtilCSSSingleton = class(TExtFunction)
  public
    class function JSClassName: string; override;
    function CreateStyleSheet(CssText: string; Id: string): TExtFunction;
    function GetRule(Selector: string; RefreshCache: Boolean): TExtFunction; overload;
    function GetRule(Selector: TExtObjectList; RefreshCache: Boolean)
      : TExtFunction; overload;
    function GetRules(RefreshCache: Boolean): TExtFunction;
    function RefreshCache: TExtFunction;
    function RemoveStyleSheet(Id: string): TExtFunction;
    function SwapStyleSheet(Id: string; Url: string): TExtFunction;
    function UpdateRule(Selector: string; PropertyJS: string; Value: string)
      : TExtFunction; overload;
    function UpdateRule(Selector: TExtObjectList; PropertyJS: string; Value: string)
      : TExtFunction; overload;
  end;

  TExtUtilCookiesSingleton = class(TExtFunction)
  public
    class function JSClassName: string; override;
    function Clear(Name: string): TExtFunction;
    function Get(Name: string): TExtFunction;
    function SetJS(Name: string; Value: string; Expires: TExtObject = nil;
      Path: string = ''; Domain: string = ''; Secure: Boolean = false): TExtFunction;
  end;

  TExtUtilDelayedTask = class(TExtFunction)
  public
    class function JSClassName: string; override;
    function Cancel: TExtFunction;
    function Delay(Delay: Integer; NewFn: TExtFunction = nil; NewScope: TExtObject = nil;
      NewArgs: TExtObjectList = nil): TExtFunction;
  end;

function ExtUtilJSON: TExtUtilJSONSingleton;
function ExtUtilTextMetrics: TExtUtilTextMetricsSingleton;
function ExtUtilFormat: TExtUtilFormatSingleton;
function ExtUtilCSS: TExtUtilCSSSingleton;
function ExtUtilCookies: TExtUtilCookiesSingleton;

implementation

uses
  SysUtils;

function ExtUtilJSON: TExtUtilJSONSingleton;
begin
  if (GetSession <> nil) then
    Result := GetSession.GetSingleton<TExtUtilJSONSingleton>(TExtUtilJSONSingleton.JSClassName)
  else
    Result := nil;
end;

function ExtUtilTextMetrics: TExtUtilTextMetricsSingleton;
begin
  if (GetSession <> nil) then
    // 'TextMetrics' is the name of the custom instance we create and bind
    // to the <body> element.
    Result := GetSession.GetSingleton<TExtUtilTextMetricsSingleton>('TextMetrics')
  else
    Result := nil;
end;

function ExtUtilFormat: TExtUtilFormatSingleton;
begin
  if (GetSession <> nil) then
    Result := GetSession.GetSingleton<TExtUtilFormatSingleton>(TExtUtilFormatSingleton.JSClassName)
  else
    Result := nil;
end;

function ExtUtilCSS: TExtUtilCSSSingleton;
begin
  if (GetSession <> nil) then
    Result := GetSession.GetSingleton<TExtUtilCSSSingleton>(TExtUtilCSSSingleton.JSClassName)
  else
    Result := nil;
end;

function ExtUtilCookies: TExtUtilCookiesSingleton;
begin
  if (GetSession <> nil) then
    Result := GetSession.GetSingleton<TExtUtilCookiesSingleton>(TExtUtilCookiesSingleton.JSClassName)
  else
    Result := nil;
end;

procedure TExtUtilObservable.SetFListeners(Value: TExtObject);
begin
  FListeners.Free;
  FListeners := Value;
  JSCodeBlock('listeners:' + VarToJSON([Value, false]));
end;

class function TExtUtilObservable.JSClassName: string;
begin
  Result := 'Ext.util.Observable';
end;

procedure TExtUtilObservable.InitDefaults;
begin
  inherited;
  FListeners := TExtObject.CreateInternal(Self, 'listeners');
end;

function TExtUtilObservable.ObservableCapture(O: TExtUtilObservable; Fn: TExtFunction;
  Scope: TExtObject = nil): TExtFunction;
begin
  JSCode(JSName + '.Observable.capture(' + VarToJSON([O, false, Fn, true, Scope, false]) +
    ');', 'TExtUtilObservable');
  Result := Self;
end;

function TExtUtilObservable.ObservableObserveClass(C: TExtFunction; Listeners: TExtObject)
  : TExtFunction;
begin
  JSCode(JSName + '.Observable.observeClass(' + VarToJSON([C, true, Listeners, false]) +
    ');', 'TExtUtilObservable');
  Result := Self;
end;

function TExtUtilObservable.ObservableReleaseCapture(O: TExtUtilObservable): TExtFunction;
begin
  JSCode(JSName + '.Observable.releaseCapture(' + VarToJSON([O, false]) + ');',
    'TExtUtilObservable');
  Result := Self;
end;

function TExtUtilObservable.AddEvents(O: TExtObject; Optional: string): TExtFunction;
begin
  JSCode(JSName + '.addEvents(' + VarToJSON([O, false, Optional]) + ');',
    'TExtUtilObservable');
  Result := Self;
end;

function TExtUtilObservable.AddEvents(O: string; Optional: string): TExtFunction;
begin
  JSCode(JSName + '.addEvents(' + VarToJSON([O, Optional]) + ');', 'TExtUtilObservable');
  Result := Self;
end;

function TExtUtilObservable.AddListener(const AEventName: string; const AHandler: TExtFunction;
  const AScope: TExtObject = nil; const AOptions: TExtObject = nil): TExtFunction;
begin
  Result := CallMethod('addListener', [AEventName, AHandler, True, AScope, False, AOptions, False]);
end;

function TExtUtilObservable.EnableBubble(Events: string): TExtFunction;
begin
  JSCode(JSName + '.enableBubble(' + VarToJSON([Events]) + ');', 'TExtUtilObservable');
  Result := Self;
end;

function TExtUtilObservable.EnableBubble(Events: TExtObjectList): TExtFunction;
begin
  JSCode(JSName + '.enableBubble(' + VarToJSON(Events) + ');', 'TExtUtilObservable');
  Result := Self;
end;

function TExtUtilObservable.FireEvent(const AEventName: string; const AArgs: TExtObjectList): TExtFunction;
begin
  Result := CallMethod('fireEvent', [AEventName, AArgs, False]);
end;

function TExtUtilObservable.HasListener(EventName: string): TExtFunction;
begin
  JSCode(JSName + '.hasListener(' + VarToJSON([EventName]) + ');', 'TExtUtilObservable');
  Result := Self;
end;

function TExtUtilObservable.&On(const AEventName: string; const AHandler: TExtFunction;
  const AScope: TExtObject; const AOptions: TExtObject): TExtFunction;
begin
  Result := CallMethod('on', [AEventName, AHandler, True, AScope, False, AOptions, False]);
end;

function TExtUtilObservable.PurgeListeners: TExtFunction;
begin
  JSCode(JSName + '.purgeListeners();', 'TExtUtilObservable');
  Result := Self;
end;

function TExtUtilObservable.RelayEvents(O: TExtObject; Events: TExtObjectList)
  : TExtFunction;
begin
  JSCode(JSName + '.relayEvents(' + VarToJSON([O, false]) + ',' + VarToJSON(Events) +
    ');', 'TExtUtilObservable');
  Result := Self;
end;

function TExtUtilObservable.RemoveAllListeners(const AEventName: string): TExtFunction;
begin
  GetSession.ResponseItems.ExecuteJSCode(Self, Format('if (%s.events.%s) delete (%s.events.%s)',
    [JSName, AEventName, JSName, AEventName]));
  Result := Self;
end;

function TExtUtilObservable.RemoveListener(EventName: string; Handler: TExtFunction;
  Scope: TExtObject = nil): TExtFunction;
begin
  JSCode(JSName + '.removeListener(' + VarToJSON([EventName, Handler, true, Scope, false])
    + ');', 'TExtUtilObservable');
  Result := Self;
end;

function TExtUtilObservable.ResumeEvents: TExtFunction;
begin
  JSCode(JSName + '.resumeEvents();', 'TExtUtilObservable');
  Result := Self;
end;

function TExtUtilObservable.SuspendEvents(QueueSuspended: Boolean): TExtFunction;
begin
  JSCode(JSName + '.suspendEvents(' + VarToJSON([QueueSuspended]) + ');',
    'TExtUtilObservable');
  Result := Self;
end;

function TExtUtilObservable.Un(EventName: string; Handler: TExtFunction;
  Scope: TExtObject = nil): TExtFunction;
begin
  JSCode(JSName + '.un(' + VarToJSON([EventName, Handler, true, Scope, false]) + ');',
    'TExtUtilObservable');
  Result := Self;
end;

class function TExtUtilJSONSingleton.JSClassName: string;
begin
  Result := 'Ext.util.JSON';
end;

function TExtUtilJSONSingleton.Decode(Json: string): TExtFunction;
begin
  JSCode(JSName + '.decode(' + VarToJSON([Json]) + ');', 'TExtUtilJSONSingleton');
  Result := Self;
end;

function TExtUtilJSONSingleton.Encode(O: string): TExtFunction;
begin
  JSCode(JSName + '.encode(' + VarToJSON([O]) + ');', 'TExtUtilJSONSingleton');
  Result := Self;
end;

function TExtUtilJSONSingleton.EncodeDate(D: TDateTime): TExtFunction;
begin
  JSCode(JSName + '.encodeDate(' + VarToJSON([D]) + ');', 'TExtUtilJSONSingleton');
  Result := Self;
end;

class function TExtUtilTextMetricsSingleton.JSClassName: string;
begin
  Result := 'Ext.util.TextMetrics';
end;

function TExtUtilTextMetricsSingleton.Bind(El: string): TExtFunction;
begin
  JSCode(JSName + '.bind(' + VarToJSON([El]) + ');', 'TExtUtilTextMetricsSingleton');
  Result := Self;
end;

function TExtUtilTextMetricsSingleton.CreateInstance(El: string; FixedWidth: Integer = 0)
  : TExtFunction;
begin
  JSCode(JSName + '.createInstance(' + VarToJSON([El, FixedWidth]) + ');',
    'TExtUtilTextMetricsSingleton');
  Result := Self;
end;

function TExtUtilTextMetricsSingleton.GetHeight(const AText: string): TExtFunction;
begin
  Result := CallMethod('getHeight', AText);
end;

function TExtUtilTextMetricsSingleton.GetSize(Text: string): TExtFunction;
begin
  JSCode(JSName + '.getSize(' + VarToJSON([Text]) + ');', 'TExtUtilTextMetricsSingleton');
  Result := Self;
end;

function TExtUtilTextMetricsSingleton.GetWidth(const AText: string): TExtFunction;
begin
  Result := CallMethod('getWidth', AText);
end;

function TExtUtilTextMetricsSingleton.Measure(El: string; Text: string;
  FixedWidth: Integer = 0): TExtFunction;
begin
  JSCode(JSName + '.measure(' + VarToJSON([El, Text, FixedWidth]) + ');',
    'TExtUtilTextMetricsSingleton');
  Result := Self;
end;

function TExtUtilTextMetricsSingleton.SetFixedWidth(Width: Integer): TExtFunction;
begin
  JSCode(JSName + '.setFixedWidth(' + VarToJSON([Width]) + ');',
    'TExtUtilTextMetricsSingleton');
  Result := Self;
end;

class function TExtUtilFormatSingleton.JSClassName: string;
begin
  Result := 'Ext.util.Format';
end;

function TExtUtilFormatSingleton.Capitalize(Value: string): TExtFunction;
begin
  JSCode(JSName + '.capitalize(' + VarToJSON([Value]) + ');', 'TExtUtilFormatSingleton');
  Result := Self;
end;

function TExtUtilFormatSingleton.Date(Value: string; Format: string = ''): TExtFunction;
begin
  JSCode(JSName + '.date(' + VarToJSON([Value, Format]) + ');',
    'TExtUtilFormatSingleton');
  Result := Self;
end;

function TExtUtilFormatSingleton.Date(Value: TDateTime; Format: string = '')
  : TExtFunction;
begin
  JSCode(JSName + '.date(' + VarToJSON([Value, Format]) + ');',
    'TExtUtilFormatSingleton');
  Result := Self;
end;

function TExtUtilFormatSingleton.DateRenderer(Format: string): TExtFunction;
begin
  JSCode(JSName + '.dateRenderer(' + VarToJSON([Format]) + ');',
    'TExtUtilFormatSingleton');
  Result := Self;
end;

function TExtUtilFormatSingleton.DefaultValue(Value: string; DefaultValue: string)
  : TExtFunction;
begin
  JSCode(JSName + '.defaultValue(' + VarToJSON([Value, DefaultValue]) + ');',
    'TExtUtilFormatSingleton');
  Result := Self;
end;

function TExtUtilFormatSingleton.Ellipsis(Value: string; Length: Integer; Word: Boolean)
  : TExtFunction;
begin
  JSCode(JSName + '.ellipsis(' + VarToJSON([Value, Length, Word]) + ');',
    'TExtUtilFormatSingleton');
  Result := Self;
end;

function TExtUtilFormatSingleton.FileSize(Size: Integer): TExtFunction;
begin
  JSCode(JSName + '.fileSize(' + VarToJSON([Size]) + ');', 'TExtUtilFormatSingleton');
  Result := Self;
end;

function TExtUtilFormatSingleton.FileSize(Size: string): TExtFunction;
begin
  JSCode(JSName + '.fileSize(' + VarToJSON([Size]) + ');', 'TExtUtilFormatSingleton');
  Result := Self;
end;

function TExtUtilFormatSingleton.HtmlDecode(Value: string): TExtFunction;
begin
  JSCode(JSName + '.htmlDecode(' + VarToJSON([Value]) + ');', 'TExtUtilFormatSingleton');
  Result := Self;
end;

function TExtUtilFormatSingleton.HtmlEncode(Value: string): TExtFunction;
begin
  JSCode(JSName + '.htmlEncode(' + VarToJSON([Value]) + ');', 'TExtUtilFormatSingleton');
  Result := Self;
end;

function TExtUtilFormatSingleton.Lowercase(Value: string): TExtFunction;
begin
  JSCode(JSName + '.lowercase(' + VarToJSON([Value]) + ');', 'TExtUtilFormatSingleton');
  Result := Self;
end;

function TExtUtilFormatSingleton.Math: TExtFunction;
begin
  JSCode(JSName + '.math();', 'TExtUtilFormatSingleton');
  Result := Self;
end;

function TExtUtilFormatSingleton.Nl2br(The: string): TExtFunction;
begin
  JSCode(JSName + '.nl2br(' + VarToJSON([The]) + ');', 'TExtUtilFormatSingleton');
  Result := Self;
end;

function TExtUtilFormatSingleton.Number(V: Integer; Format: string): TExtFunction;
begin
  JSCode(JSName + '.number(' + VarToJSON([V, Format]) + ');', 'TExtUtilFormatSingleton');
  Result := Self;
end;

function TExtUtilFormatSingleton.NumberRenderer(Format: string): TExtFunction;
begin
  JSCode(JSName + '.numberRenderer(' + VarToJSON([Format]) + ');',
    'TExtUtilFormatSingleton');
  Result := Self;
end;

function TExtUtilFormatSingleton.Plural(Value: Integer; Singular: string;
  Plural: string = ''): TExtFunction;
begin
  JSCode(JSName + '.plural(' + VarToJSON([Value, Singular, Plural]) + ');',
    'TExtUtilFormatSingleton');
  Result := Self;
end;

function TExtUtilFormatSingleton.Round(Value: Integer; Precision: Integer): TExtFunction;
begin
  JSCode(JSName + '.round(' + VarToJSON([Value, Precision]) + ');',
    'TExtUtilFormatSingleton');
  Result := Self;
end;

function TExtUtilFormatSingleton.Round(Value: string; Precision: Integer): TExtFunction;
begin
  JSCode(JSName + '.round(' + VarToJSON([Value, Precision]) + ');',
    'TExtUtilFormatSingleton');
  Result := Self;
end;

function TExtUtilFormatSingleton.StripScripts(Value: string): TExtFunction;
begin
  JSCode(JSName + '.stripScripts(' + VarToJSON([Value]) + ');',
    'TExtUtilFormatSingleton');
  Result := Self;
end;

function TExtUtilFormatSingleton.StripTags(Value: string): TExtFunction;
begin
  JSCode(JSName + '.stripTags(' + VarToJSON([Value]) + ');', 'TExtUtilFormatSingleton');
  Result := Self;
end;

function TExtUtilFormatSingleton.Substr(Value: string; Start: Integer; Length: Integer)
  : TExtFunction;
begin
  JSCode(JSName + '.substr(' + VarToJSON([Value, Start, Length]) + ');',
    'TExtUtilFormatSingleton');
  Result := Self;
end;

function TExtUtilFormatSingleton.Trim(Value: string): TExtFunction;
begin
  JSCode(JSName + '.trim(' + VarToJSON([Value]) + ');', 'TExtUtilFormatSingleton');
  Result := Self;
end;

function TExtUtilFormatSingleton.Undef(Value: string): TExtFunction;
begin
  JSCode(JSName + '.undef(' + VarToJSON([Value]) + ');', 'TExtUtilFormatSingleton');
  Result := Self;
end;

function TExtUtilFormatSingleton.Uppercase(Value: string): TExtFunction;
begin
  JSCode(JSName + '.uppercase(' + VarToJSON([Value]) + ');', 'TExtUtilFormatSingleton');
  Result := Self;
end;

function TExtUtilFormatSingleton.UsMoney(Value: Integer): TExtFunction;
begin
  JSCode(JSName + '.usMoney(' + VarToJSON([Value]) + ');', 'TExtUtilFormatSingleton');
  Result := Self;
end;

function TExtUtilFormatSingleton.UsMoney(Value: string): TExtFunction;
begin
  JSCode(JSName + '.usMoney(' + VarToJSON([Value]) + ');', 'TExtUtilFormatSingleton');
  Result := Self;
end;

class function TExtUtilCSSSingleton.JSClassName: string;
begin
  Result := 'Ext.util.CSS';
end;

function TExtUtilCSSSingleton.CreateStyleSheet(CssText: string; Id: string): TExtFunction;
begin
  JSCode(JSName + '.createStyleSheet(' + VarToJSON([CssText, Id]) + ');',
    'TExtUtilCSSSingleton');
  Result := Self;
end;

function TExtUtilCSSSingleton.GetRule(Selector: string; RefreshCache: Boolean)
  : TExtFunction;
begin
  JSCode(JSName + '.getRule(' + VarToJSON([Selector, RefreshCache]) + ');',
    'TExtUtilCSSSingleton');
  Result := Self;
end;

function TExtUtilCSSSingleton.GetRule(Selector: TExtObjectList; RefreshCache: Boolean)
  : TExtFunction;
begin
  JSCode(JSName + '.getRule(' + VarToJSON(Selector) + ',' + VarToJSON([RefreshCache]) +
    ');', 'TExtUtilCSSSingleton');
  Result := Self;
end;

function TExtUtilCSSSingleton.GetRules(RefreshCache: Boolean): TExtFunction;
begin
  JSCode(JSName + '.getRules(' + VarToJSON([RefreshCache]) + ');',
    'TExtUtilCSSSingleton');
  Result := Self;
end;

function TExtUtilCSSSingleton.RefreshCache: TExtFunction;
begin
  JSCode(JSName + '.refreshCache();', 'TExtUtilCSSSingleton');
  Result := Self;
end;

function TExtUtilCSSSingleton.RemoveStyleSheet(Id: string): TExtFunction;
begin
  JSCode(JSName + '.removeStyleSheet(' + VarToJSON([Id]) + ');', 'TExtUtilCSSSingleton');
  Result := Self;
end;

function TExtUtilCSSSingleton.SwapStyleSheet(Id: string; Url: string): TExtFunction;
begin
  JSCode(JSName + '.swapStyleSheet(' + VarToJSON([Id, Url]) + ');',
    'TExtUtilCSSSingleton');
  Result := Self;
end;

function TExtUtilCSSSingleton.UpdateRule(Selector: string; PropertyJS: string;
  Value: string): TExtFunction;
begin
  JSCode(JSName + '.updateRule(' + VarToJSON([Selector, PropertyJS, Value]) + ');',
    'TExtUtilCSSSingleton');
  Result := Self;
end;

function TExtUtilCSSSingleton.UpdateRule(Selector: TExtObjectList; PropertyJS: string;
  Value: string): TExtFunction;
begin
  JSCode(JSName + '.updateRule(' + VarToJSON(Selector) + ',' +
    VarToJSON([PropertyJS, Value]) + ');', 'TExtUtilCSSSingleton');
  Result := Self;
end;

class function TExtUtilCookiesSingleton.JSClassName: string;
begin
  Result := 'Ext.util.Cookies';
end;

function TExtUtilCookiesSingleton.Clear(Name: string): TExtFunction;
begin
  JSCode(JSName + '.clear(' + VarToJSON([name]) + ');', 'TExtUtilCookiesSingleton');
  Result := Self;
end;

function TExtUtilCookiesSingleton.Get(Name: string): TExtFunction;
begin
  JSCode(JSName + '.get(' + VarToJSON([name]) + ');', 'TExtUtilCookiesSingleton');
  Result := Self;
end;

function TExtUtilCookiesSingleton.SetJS(Name: string; Value: string;
  Expires: TExtObject = nil; Path: string = ''; Domain: string = '';
  Secure: Boolean = false): TExtFunction;
begin
  JSCode(JSName + '.set(' + VarToJSON([name, Value, Expires, false, Path, Domain, Secure])
    + ');', 'TExtUtilCookiesSingleton');
  Result := Self;
end;

class function TExtUtilDelayedTask.JSClassName: string;
begin
  Result := 'Ext.util.DelayedTask';
end;

function TExtUtilDelayedTask.Cancel: TExtFunction;
begin
  JSCode(JSName + '.cancel();', 'TExtUtilDelayedTask');
  Result := Self;
end;

function TExtUtilDelayedTask.Delay(Delay: Integer; NewFn: TExtFunction = nil;
  NewScope: TExtObject = nil; NewArgs: TExtObjectList = nil): TExtFunction;
begin
  JSCode(JSName + '.delay(' + VarToJSON([Delay, NewFn, true, NewScope, false]) + ',' +
    VarToJSON(NewArgs) + ');', 'TExtUtilDelayedTask');
  Result := Self;
end;

end.
