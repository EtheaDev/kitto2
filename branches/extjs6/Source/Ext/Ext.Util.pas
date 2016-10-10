unit Ext.Util;

interface

uses
  Kitto.Ext;

type
  TExtUtilObservable = class;
  TExtUtilTextMetricsSingleton = class;

  TExtUtilObservable = class(TExtObject)
  public
    class function JSClassName: string; override;
    function AddListener(const AEventName: string; const AHandler: TExtFunction;
      const AScope: TExtObject = nil; const AOptions: TExtObject = nil): TExtFunction;
    function FireEvent(const AEventName: string; const AArgs: TExtObjectList): TExtFunction;
    function &On(const AEventName: string; const AHandler: TExtFunction;
      const AScope: TExtObject = nil; const AOptions: TExtObject = nil): TExtFunction;
    function RemoveAllListeners(const AEventName: string): TExtFunction;
  end;

  TExtUtilTextMetricsSingleton = class(TExtObject)
  public
    class function JSClassName: string; override;
    function GetHeight(const AText: string): TExtFunction;
    function GetSize(const AText: string): TExtFunction;
    function GetWidth(const AText: string): TExtFunction;
  end;

function ExtUtilTextMetrics: TExtUtilTextMetricsSingleton;

implementation

uses
  SysUtils;

function ExtUtilTextMetrics: TExtUtilTextMetricsSingleton;
begin
  if (GetSession <> nil) then
    // 'TextMetrics' is the name of the custom instance we create and bind
    // to the <body> element.
    Result := GetSession.GetSingleton<TExtUtilTextMetricsSingleton>('TextMetrics')
  else
    Result := nil;
end;

class function TExtUtilObservable.JSClassName: string;
begin
  Result := 'Ext.util.Observable';
end;

function TExtUtilObservable.AddListener(const AEventName: string; const AHandler: TExtFunction;
  const AScope: TExtObject = nil; const AOptions: TExtObject = nil): TExtFunction;
begin
  Result := CallMethod('addListener')
    .AddParam(AEventName)
    .AddFunctionParam(AHandler)
    .AddParam(AScope)
    .AddParam(AOptions)
    .AsFunction;
end;

function TExtUtilObservable.FireEvent(const AEventName: string; const AArgs: TExtObjectList): TExtFunction;
begin
  Result := CallMethod('fireEvent')
    .AddParam(AEventName)
    .AddParam(AArgs)
    .AsFunction;
end;

function TExtUtilObservable.&On(const AEventName: string; const AHandler: TExtFunction;
  const AScope: TExtObject; const AOptions: TExtObject): TExtFunction;
begin
  Result := CallMethod('on')
    .AddParam(AEventName)
    .AddFunctionParam(AHandler)
    .AddParam(AScope)
    .AddParam(AOptions)
    .AsFunction;
end;

function TExtUtilObservable.RemoveAllListeners(const AEventName: string): TExtFunction;
begin
  Result := GetSession.ResponseItems.ExecuteJSCode(Self, Format('if (%s.events.%s) delete (%s.events.%s)',
    [JSName, AEventName, JSName, AEventName])).AsFunction;
end;

class function TExtUtilTextMetricsSingleton.JSClassName: string;
begin
  Result := 'Ext.util.TextMetrics';
end;

function TExtUtilTextMetricsSingleton.GetHeight(const AText: string): TExtFunction;
begin
  Result := CallMethod('getHeight')
    .AddParam(AText)
    .AsFunction;
end;

function TExtUtilTextMetricsSingleton.GetSize(const AText: string): TExtFunction;
begin
  Result := CallMethod('getSize')
    .AddParam(AText)
    .AsFunction;
end;

function TExtUtilTextMetricsSingleton.GetWidth(const AText: string): TExtFunction;
begin
  Result := CallMethod('getWidth')
    .AddParam(AText)
    .AsFunction;
end;

end.
