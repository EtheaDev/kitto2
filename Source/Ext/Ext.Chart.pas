unit Ext.Chart;

interface

uses
  StrUtils
  , Kitto.JS
  , Ext.Base
  , Ext.Data
  ;

type
  TExtChartLegendSpriteLegend = class(TExtObject)
  private
    FDocked: string;
    FPadding: Integer;
    FToggleable: Boolean;
    procedure SetDocked(const AValue: string);
    procedure SetPadding(const AValue: Integer);
    procedure SetToggleable(const AValue: Boolean);
  strict protected
    procedure InitDefaults; override;
  public
    class function JSClassName: string; override;
    property Docked: string read FDocked write SetDocked;
    property Padding: Integer read FPadding write SetPadding;
    property Toggleable: Boolean read FToggleable write SetToggleable;
  end;

  TExtChartToolTip = class(TExtToolTip)
  private
    FRendererFunc: TExtExpression;
    FRenderer: string;
    procedure SetRenderer(const AValue: string);
    procedure SetRendererFunc(const AValue: TExtExpression);
  public
    property Renderer: string read FRenderer write SetRenderer;
    property RendererFunc: TExtExpression read FRendererFunc write SetRendererFunc;
  end;

  TExtChartSeriesStyle = class(TExtObject)
  private
    FImage: String;
    FMode: String;
    FColor: String;
    procedure SetImage(const AValue: String);
    procedure SetMode(const AValue: String);
    procedure SetColor(const AValue: String);
  public
    class function JSClassName: string; override;
    property Image: String read FImage write SetImage;
    property Mode: String read FMode write SetMode;
    property Color: String read FColor write SetColor;
  end;

  TExtChartAbstractChart = class;

  TExtChartAxis = class(TExtObject)
  private
    FLabelFunction: String;
    FTitle: String;
    procedure SetLabelFunction(const AValue: String);
    procedure SetTitle(const AValue: String);
  public
    class function JSClassName: string; override;
    class function JSXType: string; override;
    property LabelFunction: String read FLabelFunction write SetLabelFunction;
    property Title: String read FTitle write SetTitle;
  end;

  TExtChartCategoryAxis = class(TExtChartAxis)
  public
    class function JSClassName: string; override;
    class function JSXType: string; override;
  end;

  TExtChartNumericAxis = class(TExtChartAxis)
  private
    FMajorUnit: Integer;
    FMinorUnit: Integer;
    FMaximum: Integer;
    FMinimum: Integer;
    FStackingEnabled: Boolean;
    procedure SetMajorUnit(const AValue: Integer);
    procedure SetMinorUnit(const AValue: Integer);
    procedure SetMaximum(const AValue: Integer);
    procedure SetMinimum(const AValue: Integer);
    procedure SetStackingEnabled(const AValue: Boolean);
  public
    class function JSClassName: string; override;
    class function JSXType: string; override;
    property MajorUnit: Integer read FMajorUnit write SetMajorUnit;
    property MinorUnit: Integer read FMinorUnit write SetMinorUnit;
    property Maximum: Integer read FMaximum write SetMaximum;
    property Minimum: Integer read FMinimum write SetMinimum;
    property StackingEnabled: Boolean read FStackingEnabled write SetStackingEnabled;
  end;

  TExtChartTimeAxis = class(TExtChartAxis)
  private
    FMajorTimeUnit: String;
    FMajorUnit: Integer;
    FMinorUnit: Integer;
    FMaximum: Integer;
    FMinimum: TDateTime;
    FStackingEnabled: Boolean;
    procedure SetMajorTimeUnit(const AValue: String);
    procedure SetMajorUnit(const AValue: Integer);
    procedure SetMinorUnit(const AValue: Integer);
    procedure SetMaximum(const AValue: Integer);
    procedure SetMinimum(const AValue: TDateTime);
    procedure SetStackingEnabled(const AValue: Boolean);
  public
    class function JSClassName: string; override;
    class function JSXType: string; override;
    property MajorTimeUnit: String read FMajorTimeUnit write SetMajorTimeUnit;
    property MajorUnit: Integer read FMajorUnit write SetMajorUnit;
    property MinorUnit: Integer read FMinorUnit write SetMinorUnit;
    property Maximum: Integer read FMaximum write SetMaximum;
    property Minimum: TDateTime read FMinimum write SetMinimum;
    property StackingEnabled: Boolean read FStackingEnabled write SetStackingEnabled;
  end;

  TExtChartAxis3D = class(TExtChartAxis)
  public
    class function JSClassName: string; override;
    class function JSXType: string; override;
  end;

  TExtChartCategory3DAxis = class(TExtChartAxis3D)
  public
    class function JSClassName: string; override;
    class function JSXType: string; override;
  end;

  TExtChartNumeric3DAxis = class(TExtChartAxis3D)
  public
    class function JSClassName: string; override;
    class function JSXType: string; override;
  end;

  TExtChartTime3DAxis = class(TExtChartAxis3D)
  public
    class function JSClassName: string; override;
    class function JSXType: string; override;
  end;

  TExtChartLabel = class(TExtObject)
  private
    FCalloutLine: TExtObject;
    function GetCalloutLine: TExtObject;
  public
    property CalloutLine: TExtObject read GetCalloutLine;
  end;

  TExtChartSeries = class(TExtObject)
  private
    FTitle: String;
    FStyle: TExtChartSeriesStyle;
    FHighlight: Boolean;
    FToolTip: TExtChartToolTip;
    FLabel: TExtChartLabel;
    procedure SetTitle(const AValue: String);
    procedure SetStyle(const AValue: TExtChartSeriesStyle);
    procedure SetHighlight(const AValue: Boolean);
    function GetToolTip: TExtChartToolTip;
  strict protected
    procedure InitDefaults; override;
  public
    class function JSClassName: string; override;
    property Title: String read FTitle write SetTitle;
    property Highlight: Boolean read FHighlight write SetHighlight;
    property &Label: TExtChartLabel read FLabel;
    property Style: TExtChartSeriesStyle read FStyle write SetStyle;
    property ToolTip: TExtChartToolTip read GetToolTip;

    class function CreateInlineByType(const AType: string; AChart: TExtChartAbstractChart): TExtChartSeries;
  end;

  TExtChartPolarSeries = class(TExtChartSeries)
  private
    FAngleField: string;
    procedure SetAngleField(const AValue: string);
  public
    property AngleField: string read FAngleField write SetAngleField;
  end;

  TExtChartPieSeries = class(TExtChartPolarSeries)
  public
    class function JSClassName: string; override;
    class function JSXType: string; override;
  end;

  TExtChartPie3DSeries = class(TExtChartPolarSeries)
  public
    class function JSClassName: string; override;
    class function JSXType: string; override;
  end;

  TExtChartRadarSeries = class(TExtChartPolarSeries)
  public
    class function JSClassName: string; override;
    class function JSXType: string; override;
  end;

  TExtChartGaugeSeries = class(TExtChartPolarSeries)
  public
    class function JSClassName: string; override;
    class function JSXType: string; override;
  end;

  TExtChartCartesianSeries = class(TExtChartSeries)
  private
    FXField: string;
    FYField: string;
    procedure SetXField(const AValue: string);
    procedure SetYField(const AValue: string);
  public
    class function JSClassName: string; override;
    property XField: string read FXField write SetXField;
    property YField: string read FYField write SetYField;
  end;

  TExtChartCandleStickSeries = class(TExtChartCartesianSeries)
  public
    class function JSClassName: string; override;
    class function JSXType: string; override;
  end;

  TExtChartScatterSeries = class(TExtChartCartesianSeries)
  public
    class function JSClassName: string; override;
    class function JSXType: string; override;
  end;

  TExtChartLineSeries = class(TExtChartCartesianSeries)
  public
    class function JSClassName: string; override;
    class function JSXType: string; override;
  end;

  TExtChartStackedCartesianSeries = class(TExtChartCartesianSeries)
  end;

  TExtChartBarSeries = class(TExtChartStackedCartesianSeries)
  public
    class function JSClassName: string; override;
    class function JSXType: string; override;
  end;

  TExtChartAreaSeries = class(TExtChartStackedCartesianSeries)
  public
    class function JSClassName: string; override;
    class function JSXType: string; override;
  end;

  TExtChartAbstractChart = class(TExtBoxComponent)
  private
    FStore: TExtDataStore;
    FSeries: TJSObjectArray;
    FLegend: TExtChartLegendSpriteLegend;
    FLegendBool: Boolean;
    FInteractions: TJSObjectArray;
    FInsetPadding: Integer;
    FInnerPadding: Integer;
    FTheme: string;
    procedure SetStore(const AValue: TExtDataStore);
    procedure SetLegendBool(const AValue: Boolean);
    function GetLegend: TExtChartLegendSpriteLegend;
    function GetInteractions: TJSObjectArray;
    procedure SetInnerPadding(const AValue: Integer);
    procedure SetInsetPadding(const AValue: Integer);
    procedure SetTheme(const AValue: string);
  protected
    procedure InitDefaults; override;
  public
    class function JSClassName: string; override;
    property InnerPadding: Integer read FInnerPadding write SetInnerPadding;
    property InsetPadding: Integer read FInsetPadding write SetInsetPadding;
    property Legend: TExtChartLegendSpriteLegend read GetLegend;
    property LegendBool: Boolean read FLegendBool write SetLegendBool;
    property Store: TExtDataStore read FStore write SetStore;
    property Series: TJSObjectArray read FSeries;
    property Theme: string read FTheme write SetTheme;
    property Interactions: TJSObjectArray read GetInteractions;

    class function CreateByType(const AType: string; const AAddTo: TJSObjectArray): TExtChartAbstractChart;
  end;

  TExtChartCartesianChart = class(TExtChartAbstractChart)
  private
    FAxes: TJSObjectArray;
  strict protected
    procedure InitDefaults; override;
  public
    class function JSClassName: string; override;
    property Axes: TJSObjectArray read FAxes;
  end;

  TExtChartPolarChart = class(TExtChartAbstractChart)
  public
    class function JSClassName: string; override;
  end;

  TExtChartSpaceFillingChart = class(TExtChartCartesianChart)
  public
    class function JSClassName: string; override;
  end;

implementation

uses
  SysUtils
  ;

procedure TExtChartSeries.SetTitle(const AValue: String);
begin
  FTitle := SetConfigItem('title', AValue);
end;

procedure TExtChartSeries.SetHighlight(const AValue: Boolean);
begin
  FHighlight := SetConfigItem('highlight', AValue);
end;

procedure TExtChartSeries.SetStyle(const AValue: TExtChartSeriesStyle);
begin
  FStyle.Free;
  FStyle := TExtChartSeriesStyle(SetProperty('style', AValue));
end;

class function TExtChartSeries.CreateInlineByType(const AType: string;
  AChart: TExtChartAbstractChart): TExtChartSeries;
begin
  Assert(Assigned(AChart));
  Assert(AType <> '');

  // Polar
  if AType = 'Pie' then
    Result := TExtChartPieSeries.CreateInlineAndAddToArray(AChart.Series)
  else if AType = 'Pie3D' then
    Result := TExtChartPie3DSeries.CreateInlineAndAddToArray(AChart.Series)
  else if AType = 'Radar' then
    Result := TExtChartRadarSeries.CreateInlineAndAddToArray(AChart.Series)
  // Cartesian
  else if AType = 'Line' then
    Result := TExtChartLineSeries.CreateInlineAndAddToArray(AChart.Series)
  else if AType = 'Area' then
    Result := TExtChartAreaSeries.CreateInlineAndAddToArray(AChart.Series)
  else if AType = 'Bar' then
    Result := TExtChartBarSeries.CreateInlineAndAddToArray(AChart.Series)
  else if AType = 'CandleStick' then
    Result := TExtChartCandleStickSeries.CreateInlineAndAddToArray(AChart.Series)
  else if AType = 'Scatter' then
    Result := TExtChartScatterSeries.CreateInlineAndAddToArray(AChart.Series)
  // SpaceFilling
  else if AType = 'Gauge' then
    Result := TExtChartGaugeSeries.CreateInlineAndAddToArray(AChart.Series)
  else
    raise Exception.CreateFmt('Unknown chart series type %s', [AType]);
end;

function TExtChartSeries.GetToolTip: TExtChartToolTip;
begin
  if not Assigned(FToolTip) then
    FToolTip := TExtChartToolTip(CreateConfigObject(TExtChartToolTip, 'tooltip'));
  Result := FToolTip;
end;

procedure TExtChartSeries.InitDefaults;
begin
  inherited;
  FLabel := TExtChartLabel(CreateConfigObject(TExtChartLabel, 'label'));
end;

class function TExtChartSeries.JSClassName: string;
begin
  Result := 'Ext.chart.series.Series';
end;

procedure TExtChartSeriesStyle.SetImage(const AValue: String);
begin
  FImage := SetConfigItem('image', 'setImage', AValue);
end;

procedure TExtChartSeriesStyle.SetMode(const AValue: String);
begin
  FMode := SetConfigItem('mode', AValue);
end;

procedure TExtChartSeriesStyle.SetColor(const AValue: String);
begin
  FColor := SetConfigItem('color', AValue);
end;

class function TExtChartSeriesStyle.JSClassName: string;
begin
  Result := 'Object';
end;

class function TExtChartAxis.JSXType: string;
begin
  Result := 'axis';
end;

procedure TExtChartAxis.SetLabelFunction(const AValue: String);
begin
  FLabelFunction := SetProperty('labelFunction', AValue);
end;

procedure TExtChartAxis.SetTitle(const AValue: String);
begin
  FTitle := SetConfigItem('title', AValue);
end;

class function TExtChartAxis.JSClassName: string;
begin
  Result := 'Ext.chart.axis.Axis';
end;

class function TExtChartNumericAxis.JSXType: string;
begin
  Result := 'axis.numeric';
end;

procedure TExtChartNumericAxis.SetMajorUnit(const AValue: Integer);
begin
  FMajorUnit := SetProperty('majorUnit', AValue);
end;

procedure TExtChartNumericAxis.SetMaximum(const AValue: Integer);
begin
  FMaximum := SetProperty('maximum', AValue);
end;

procedure TExtChartNumericAxis.SetMinimum(const AValue: Integer);
begin
  FMinimum := SetProperty('minimum', AValue);
end;

procedure TExtChartNumericAxis.SetMinorUnit(const AValue: Integer);
begin
  FMinorUnit := SetProperty('minorUnit', AValue);
end;

procedure TExtChartNumericAxis.SetStackingEnabled(const AValue: Boolean);
begin
  FStackingEnabled := SetProperty('stackingEnabled', AValue);
end;

class function TExtChartNumericAxis.JSClassName: string;
begin
  Result := 'Ext.chart.axis.Numeric';
end;

class function TExtChartTimeAxis.JSXType: string;
begin
  Result := 'axis.time';
end;

procedure TExtChartTimeAxis.SetMajorTimeUnit(const AValue: String);
begin
  FMajorTimeUnit := SetProperty('majorTimeUnit', AValue);
end;

procedure TExtChartTimeAxis.SetMajorUnit(const AValue: Integer);
begin
  FMajorUnit := SetProperty('majorUnit', AValue);
end;

procedure TExtChartTimeAxis.SetMinorUnit(const AValue: Integer);
begin
  FMinorUnit := SetProperty('minorUnit', AValue);
end;

procedure TExtChartTimeAxis.SetMaximum(const AValue: Integer);
begin
  FMaximum := SetProperty('maximum', AValue);
end;

procedure TExtChartTimeAxis.SetMinimum(const AValue: TDateTime);
begin
  FMinimum := SetProperty('minimum', AValue);
end;

procedure TExtChartTimeAxis.SetStackingEnabled(const AValue: Boolean);
begin
  FStackingEnabled := SetProperty('stackingEnabled', AValue);
end;

class function TExtChartTimeAxis.JSClassName: string;
begin
  Result := 'Ext.chart.axis.Time';
end;

class function TExtChartPieSeries.JSClassName: string;
begin
  Result := 'Ext.chart.series.Pie';
end;

procedure TExtChartCartesianSeries.SetXField(const AValue: String);
begin
  FXField := SetConfigItem('xField', AValue);
end;

procedure TExtChartCartesianSeries.SetYField(const AValue: String);
begin
  FYField := SetConfigItem('yField', AValue);
end;

class function TExtChartCartesianSeries.JSClassName: string;
begin
  Result := 'Ext.chart.series.Cartesian';
end;

class function TExtChartCategoryAxis.JSClassName: string;
begin
  Result := 'Ext.chart.axis.Category';
end;

class function TExtChartLineSeries.JSClassName: string;
begin
  Result := 'Ext.chart.series.Line';
end;

class function TExtChartBarSeries.JSClassName: string;
begin
  Result := 'Ext.chart.series.Bar';
end;

procedure TExtChartAbstractChart.SetInnerPadding(const AValue: Integer);
begin
  FInnerPadding := SetConfigItem('innerPadding', AValue);
end;

procedure TExtChartAbstractChart.SetInsetPadding(const AValue: Integer);
begin
  FInsetPadding := SetConfigItem('insetPadding', AValue);
end;

procedure TExtChartAbstractChart.SetLegendBool(const AValue: Boolean);
begin
  FLegendBool := SetConfigItem('legend', AValue);
end;

procedure TExtChartAbstractChart.SetStore(const AValue: TExtDataStore);
begin
  FStore.Free;
  FStore := TExtDataStore(SetConfigItem('store', AValue));
end;

procedure TExtChartAbstractChart.SetTheme(const AValue: string);
begin
  FTheme := SetConfigItem('theme', AValue);
end;

class function TExtChartAbstractChart.JSClassName: string;
begin
  Result := 'Ext.chart.AbstractChart';
end;

class function TExtChartAbstractChart.CreateByType(const AType: string;
  const AAddTo: TJSObjectArray): TExtChartAbstractChart;
begin
  Assert(AType <> '');
  Assert(Assigned(AAddTo));

  if AType = 'Polar' then
    Result := TExtChartPolarChart.CreateAndAddToArray(AAddTo)
  else if AType = 'Cartesian' then
    Result := TExtChartCartesianChart.CreateAndAddToArray(AAddTo)
  else if AType = 'SpaceFilling' then
    Result := TExtChartSpaceFillingChart.CreateAndAddToArray(AAddTo)
  else
    raise Exception.CreateFmt('Unknown chart type %s', [AType]);
end;

function TExtChartAbstractChart.GetInteractions: TJSObjectArray;
begin
  if not Assigned(FInteractions) then
    FInteractions := CreateConfigObjectArray('interactions');
  Result := FInteractions;
end;

function TExtChartAbstractChart.GetLegend: TExtChartLegendSpriteLegend;
begin
  if not Assigned(FLegend) then
    FLegend := TExtChartLegendSpriteLegend(CreateConfigObject(TExtChartLegendSpriteLegend, 'legend'));
  Result := FLegend;
end;

procedure TExtChartAbstractChart.InitDefaults;
begin
  inherited;
  FSeries := CreateConfigObjectArray('series');
  FTheme := 'default';
end;

procedure TExtChartCartesianChart.InitDefaults;
begin
  inherited;
  FAxes := CreateConfigObjectArray('axes');
end;

class function TExtChartCartesianChart.JSClassName: string;
begin
  Result := 'Ext.chart.CartesianChart';
end;

class function TExtChartPolarChart.JSClassName: string;
begin
  Result := 'Ext.chart.PolarChart';
end;

class function TExtChartSpaceFillingChart.JSClassName: string;
begin
  Result := 'Ext.chart.SpaceFillingChart';
end;

{ TExtChartToolTip }

procedure TExtChartToolTip.SetRenderer(const AValue: string);
begin
  FRenderer := SetConfigItem('renderer', AValue);
end;

procedure TExtChartToolTip.SetRendererFunc(const AValue: TExtExpression);
begin
  FRendererFunc := SetConfigItem('renderer', AValue);
end;

{ TExtChartLegendLegend }

procedure TExtChartLegendSpriteLegend.InitDefaults;
begin
  inherited;
  FDocked := 'bottom';
  FPadding := 10;
end;

class function TExtChartLegendSpriteLegend.JSClassName: string;
begin
  Result := 'Ext.chart.legend.SpriteLegend';
end;

procedure TExtChartLegendSpriteLegend.SetDocked(const AValue: string);
begin
  FDocked := SetConfigItem('docked', AValue);
end;

procedure TExtChartLegendSpriteLegend.SetPadding(const AValue: Integer);
begin
  FPadding := SetConfigItem('padding', AValue);
end;

procedure TExtChartLegendSpriteLegend.SetToggleable(const AValue: Boolean);
begin
  FToggleable := SetConfigItem('toggleable', AValue);
end;

{ TExtChartAreaSeries }

class function TExtChartAreaSeries.JSClassName: string;
begin
  Result := 'Ext.chart.series.Area';
end;

class function TExtChartAreaSeries.JSXType: string;
begin
  Result := 'series.area';
end;

{ TExtChartCandleStickSeries }

class function TExtChartCandleStickSeries.JSClassName: string;
begin
  Result := 'Ext.chart.series.CandleStick';
end;

class function TExtChartCandleStickSeries.JSXType: string;
begin
  Result := 'series.candlestick';
end;

{ TExtChartScatterSeries }

class function TExtChartScatterSeries.JSClassName: string;
begin
  Result := 'Ext.chart.series.Scatter';
end;

class function TExtChartScatterSeries.JSXType: string;
begin
  Result := 'series.scatter';
end;

{ TExtChartPie3DSeries }

class function TExtChartPie3DSeries.JSClassName: string;
begin
  Result := 'Ext.chart.series.Pie3D';
end;

class function TExtChartPie3DSeries.JSXType: string;
begin
  Result := 'series.pie3d';
end;

{ TExtChartRadarSeries }

class function TExtChartRadarSeries.JSClassName: string;
begin
  Result := 'Ext.chart.series.Radar';
end;

class function TExtChartRadarSeries.JSXType: string;
begin
  Result := 'series.radar';
end;

{ TExtChartGaugeSeries }

class function TExtChartGaugeSeries.JSClassName: string;
begin
  Result := 'Ext.chart.series.Gauge';
end;

class function TExtChartGaugeSeries.JSXType: string;
begin
  Result := 'series.gauge';
end;

{ TExtChartPolarSeries }

procedure TExtChartPolarSeries.SetAngleField(const AValue: string);
begin
  FAngleField := SetConfigItem('angleField', AValue);
end;

class function TExtChartPieSeries.JSXType: string;
begin
  Result := 'series.pie';
end;

class function TExtChartLineSeries.JSXType: string;
begin
  Result := 'series.line';
end;

class function TExtChartBarSeries.JSXType: string;
begin
  Result := 'series.bar';
end;

class function TExtChartCategoryAxis.JSXType: string;
begin
  Result := 'axis.category';
end;

{ TExtChartAxis3D }

class function TExtChartAxis3D.JSClassName: string;
begin
  Result := 'Ext.chart.axis.Axis3D';
end;

class function TExtChartAxis3D.JSXType: string;
begin
  Result := 'axis3d';
end;

{ TExtChartCategory3DAxis }

class function TExtChartCategory3DAxis.JSClassName: string;
begin
  Result := 'Ext.chart.axis.Category3D';
end;

class function TExtChartCategory3DAxis.JSXType: string;
begin
  Result := 'axis.category3d';
end;

{ TExtChartNumeric3DAxis }

class function TExtChartNumeric3DAxis.JSClassName: string;
begin
  Result := 'Ext.chart.axis.Numeric3D';
end;

class function TExtChartNumeric3DAxis.JSXType: string;
begin
  Result := 'axis.numeric3d';
end;

{ TExtChartTime3DAxis }

class function TExtChartTime3DAxis.JSClassName: string;
begin
  Result := 'Ext.chart.axis.Time3D';
end;

class function TExtChartTime3DAxis.JSXType: string;
begin
  Result := 'axis.time3d';
end;

{ TExtChartLabel }

function TExtChartLabel.GetCalloutLine: TExtObject;
begin
  if not Assigned(FCalloutLine) then
    FCalloutLine := CreateConfigObject('calloutLine');
  Result := FCalloutLine;
end;

end.
