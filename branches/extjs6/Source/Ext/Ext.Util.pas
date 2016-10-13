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
    function AddListener(const AEventName: string; const AHandler: TExtExpression;
      const AScope: TExtObject = nil; const AOptions: TExtObject = nil): TExtExpression;
    function FireEvent(const AEventName: string; const AArgs: TExtObjectList): TExtExpression;
    function &On(const AEventName: string; const AHandler: TExtExpression;
      const AScope: TExtObject = nil; const AOptions: TExtObject = nil): TExtExpression;
    function RemoveAllListeners(const AEventName: string): TExtExpression;
  end;

  TExtUtilTextMetricsSingleton = class(TExtObject)
  public
    class function JSClassName: string; override;
    function GetHeight(const AText: string): TExtExpression;
    function GetSize(const AText: string): TExtExpression;
    function GetWidth(const AText: string): TExtExpression;
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

function TExtUtilObservable.AddListener(const AEventName: string; const AHandler: TExtExpression;
  const AScope: TExtObject = nil; const AOptions: TExtObject = nil): TExtExpression;
begin
  Result := CallMethod('addListener')
    .AddParam(AEventName)
    .AddParam(AHandler)
    .AddParam(AScope)
    .AddParam(AOptions)
    .AsExpression;
end;

function TExtUtilObservable.FireEvent(const AEventName: string; const AArgs: TExtObjectList): TExtExpression;
begin
  Result := CallMethod('fireEvent')
    .AddParam(AEventName)
    .AddParam(AArgs)
    .AsExpression;
end;

function TExtUtilObservable.&On(const AEventName: string; const AHandler: TExtExpression;
  const AScope: TExtObject; const AOptions: TExtObject): TExtExpression;
begin
  Result := CallMethod('on')
    .AddParam(AEventName)
    .AddParam(AHandler)
    .AddParam(AScope)
    .AddParam(AOptions)
    .AsExpression;
end;

function TExtUtilObservable.RemoveAllListeners(const AEventName: string): TExtExpression;
begin
  Result := GetSession.ResponseItems.ExecuteJSCode(Self, Format('if (%s.events.%s) delete (%s.events.%s)',
    [JSName, AEventName, JSName, AEventName])).AsExpression;
end;

class function TExtUtilTextMetricsSingleton.JSClassName: string;
begin
  Result := 'Ext.util.TextMetrics';
end;

function TExtUtilTextMetricsSingleton.GetHeight(const AText: string): TExtExpression;
begin
  Result := CallMethod('getHeight')
    .AddParam(AText)
    .AsExpression;
end;

function TExtUtilTextMetricsSingleton.GetSize(const AText: string): TExtExpression;
begin
  Result := CallMethod('getSize')
    .AddParam(AText)
    .AsExpression;
end;

function TExtUtilTextMetricsSingleton.GetWidth(const AText: string): TExtExpression;
begin
  Result := CallMethod('getWidth')
    .AddParam(AText)
    .AsExpression;
end;

end.
