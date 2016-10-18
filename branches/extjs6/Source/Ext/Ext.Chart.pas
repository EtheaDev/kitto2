unit Ext.Chart;

interface

uses
  StrUtils,
  Kitto.Ext,
  Ext.Base,
  Ext.Data;

type
  TExtChartStyle = class;
  TExtChartSeries = class;
  TExtChartSeriesStyle = class;
  TExtChartAxis = class;
  TExtChartNumericAxis = class;
  TExtChartTimeAxis = class;
  TExtChartPieSeries = class;
  TExtChartCartesianSeries = class;
  TExtChartCategoryAxis = class;
  TExtChartLineSeries = class;
  TExtChartBarSeries = class;
  TExtChartColumnSeries = class;
  TExtChartChart = class;
  TExtChartCartesianChart = class;
  TExtChartPieChart = class;
  TExtChartStackedColumnChart = class;
  TExtChartColumnChart = class;
  TExtChartBarChart = class;
  TExtChartStackedBarChart = class;
  TExtChartLineChart = class;

  TExtChartStyle = class(TExtObject)
  public
    class function JSClassName: string; override;
  end;

  TExtChartSeries = class(TExtObject)
  private
    FDisplayName: String;
    FTypeJS: String;
    FStyle: TExtChartSeriesStyle;
    procedure SetDisplayName(const AValue: String);
    procedure SetTypeJS(const AValue: String);
    procedure SetStyle(const AValue: TExtChartSeriesStyle);
  public
    class function JSClassName: string; override;
    property DisplayName: String read FDisplayName write SetDisplayName;
    property TypeJS: String read FTypeJS write SetTypeJS;
    property Style: TExtChartSeriesStyle read FStyle write SetStyle;
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

  TExtChartAxis = class(TExtObject)
  private
    FLabelFunction: String;
    FTitle: String;
    procedure SetLabelFunction(const AValue: String);
    procedure SetTitle(const AValue: String);
  public
    class function JSClassName: string; override;
    property LabelFunction: String read FLabelFunction write SetLabelFunction;
    property Title: String read FTitle write SetTitle;
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
    property MajorTimeUnit: String read FMajorTimeUnit write SetMajorTimeUnit;
    property MajorUnit: Integer read FMajorUnit write SetMajorUnit;
    property MinorUnit: Integer read FMinorUnit write SetMinorUnit;
    property Maximum: Integer read FMaximum write SetMaximum;
    property Minimum: TDateTime read FMinimum write SetMinimum;
    property StackingEnabled: Boolean read FStackingEnabled write SetStackingEnabled;
  end;

  TExtChartPieSeries = class(TExtChartSeries)
  public
    class function JSClassName: string; override;
  end;

  TExtChartCartesianSeries = class(TExtChartSeries)
  private
    FXField: String;
    FYField: String;
    procedure SetXField(const AValue: String);
    procedure SetYField(const AValue: String);
  public
    class function JSClassName: string; override;
    property XField: String read FXField write SetXField;
    property YField: String read FYField write SetYField;
  end;

  TExtChartCategoryAxis = class(TExtChartAxis)
  public
    class function JSClassName: string; override;
  end;

  TExtChartLineSeries = class(TExtChartCartesianSeries)
  public
    class function JSClassName: string; override;
  end;

  TExtChartBarSeries = class(TExtChartCartesianSeries)
  public
    class function JSClassName: string; override;
  end;

  TExtChartColumnSeries = class(TExtChartCartesianSeries)
  public
    class function JSClassName: string; override;
  end;

  TExtChartChart = class(TExtBoxComponent)
  private
    FChartStyle: TExtObject;
    FUrl: String;
    FStore: TExtDataStore;
    FYField: String;
    FXField: String;
    FXAxis: TExtChartAxis;
    FYAxis: TExtChartAxis;
    FTipRenderer: TExtExpression;
    FSeries: TExtObjectArray;
    procedure SetChartStyle(const AValue: TExtObject);
    procedure SetUrl(const AValue: String);
    procedure SetStore(const AValue: TExtDataStore);
    procedure SetYField(const AValue: String);
    procedure SetXField(const AValue: String);
    procedure SetXAxis(const AValue: TExtChartAxis);
    procedure SetYAxis(const AValue: TExtChartAxis);
    procedure SetTipRenderer(const AValue: TExtExpression);
  protected
    procedure InitDefaults; override;
  public
    class function JSClassName: string; override;
    class function CHART_URL: String;
    property ChartStyle: TExtObject read FChartStyle write SetChartStyle;
    property Url: String read FUrl write SetUrl;
    property Store: TExtDataStore read FStore write SetStore;
    property YField: String read FYField write SetYField;
    property XField: String read FXField write SetXField;
    property XAxis: TExtChartAxis read FXAxis write SetXAxis;
    property YAxis: TExtChartAxis read FYAxis write SetYAxis;
    property TipRenderer: TExtExpression read FTipRenderer write SetTipRenderer;
    property Series: TExtObjectArray read FSeries;
  end;

  TExtChartCartesianChart = class(TExtChartChart)
  protected
    procedure InitDefaults; override;
  public
    class function JSClassName: string; override;
  end;

  TExtChartPieChart = class(TExtChartChart)
  private
    FDataField: String;
    FCategoryField: String;
    procedure SetDataField(const AValue: String);
    procedure SetCategoryField(const AValue: String);
  protected
    procedure InitDefaults; override;
  public
    class function JSClassName: string; override;
    property DataField: String read FDataField write SetDataField;
    property CategoryField: String read FCategoryField write SetCategoryField;
  end;

  TExtChartStackedColumnChart = class(TExtChartCartesianChart)
  protected
    procedure InitDefaults; override;
  public
    class function JSClassName: string; override;
  end;

  TExtChartColumnChart = class(TExtChartCartesianChart)
  protected
    procedure InitDefaults; override;
  public
    class function JSClassName: string; override;
  end;

  TExtChartBarChart = class(TExtChartCartesianChart)
  protected
    procedure InitDefaults; override;
  public
    class function JSClassName: string; override;
  end;

  TExtChartStackedBarChart = class(TExtChartCartesianChart)
  protected
    procedure InitDefaults; override;
  public
    class function JSClassName: string; override;
  end;

  TExtChartLineChart = class(TExtChartCartesianChart)
  protected
    procedure InitDefaults; override;
  public
    class function JSClassName: string; override;
  end;

implementation

class function TExtChartStyle.JSClassName: string;
begin
  Result := 'Object';
end;

procedure TExtChartSeries.SetDisplayName(const AValue: String);
begin
  FDisplayName := SetConfigItem('displayName', AValue);
end;

procedure TExtChartSeries.SetTypeJS(const AValue: String);
begin
  FTypeJS := SetConfigItem('type', AValue);
end;

procedure TExtChartSeries.SetStyle(const AValue: TExtChartSeriesStyle);
begin
  FStyle.Free;
  FStyle := TExtChartSeriesStyle(SetProperty('style', AValue));
end;

class function TExtChartSeries.JSClassName: string;
begin
  Result := 'Ext.chart.Series';
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
  Result := 'Ext.chart.Axis';
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
  Result := 'Ext.chart.NumericAxis';
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
  Result := 'Ext.chart.TimeAxis';
end;

class function TExtChartPieSeries.JSClassName: string;
begin
  Result := 'Ext.chart.PieSeries';
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
  Result := 'Ext.chart.CartesianSeries';
end;

class function TExtChartCategoryAxis.JSClassName: string;
begin
  Result := 'Ext.chart.CategoryAxis';
end;

class function TExtChartLineSeries.JSClassName: string;
begin
  Result := 'Ext.chart.LineSeries';
end;

class function TExtChartBarSeries.JSClassName: string;
begin
  Result := 'Ext.chart.BarSeries';
end;

class function TExtChartColumnSeries.JSClassName: string;
begin
  Result := 'Ext.chart.ColumnSeries';
end;

procedure TExtChartChart.SetChartStyle(const AValue: TExtObject);
begin
  FChartStyle.Free;
  FChartStyle := SetConfigItem('chartStyle', AValue);
end;

procedure TExtChartChart.SetUrl(const AValue: String);
begin
  FUrl := SetConfigItem('url', AValue);
end;

procedure TExtChartChart.SetStore(const AValue: TExtDataStore);
begin
  FStore.Free;
  FStore := AValue;
  SetConfigItem('store', AValue);
end;

procedure TExtChartChart.SetYField(const AValue: String);
begin
  FYField := SetConfigItem('yField', AValue);
end;

procedure TExtChartChart.SetXField(const AValue: String);
begin
  FXField := SetConfigItem('xField', AValue);
end;

procedure TExtChartChart.SetXAxis(const AValue: TExtChartAxis);
begin
  FXAxis.Free;
  FXAxis := TExtChartAxis(SetProperty('xAxis', AValue));
end;

procedure TExtChartChart.SetYAxis(const AValue: TExtChartAxis);
begin
  FYAxis.Free;
  FYAxis := TExtChartAxis(SetProperty('yAxis', AValue));
end;

procedure TExtChartChart.SetTipRenderer(const AValue: TExtExpression);
begin
  FTipRenderer := SetConfigItem('tipRenderer', 'setTipRenderer', AValue);
end;

class function TExtChartChart.JSClassName: string;
begin
  Result := 'Ext.chart.Chart';
end;

class function TExtChartChart.CHART_URL: String;
begin
  Result := ''
end;

procedure TExtChartChart.InitDefaults;
begin
  inherited;
  FChartStyle := TExtChartStyle.CreateInternal(Self, 'chartStyle');
  FStore := TExtDataStore.CreateInternal(Self, 'store');
  FXAxis := TExtChartAxis.CreateInternal(Self, 'xAxis');
  FYAxis := TExtChartAxis.CreateInternal(Self, 'yAxis');
  FSeries := CreateConfigArray('series');
end;

class function TExtChartCartesianChart.JSClassName: string;
begin
  Result := 'Ext.chart.CartesianChart';
end;

procedure TExtChartCartesianChart.InitDefaults;
begin
  inherited;
end;

procedure TExtChartPieChart.SetDataField(const AValue: String);
begin
  FDataField := SetConfigItem('dataField', AValue);
end;

procedure TExtChartPieChart.SetCategoryField(const AValue: String);
begin
  FCategoryField := SetConfigItem('categoryField', AValue);
end;

class function TExtChartPieChart.JSClassName: string;
begin
  Result := 'Ext.chart.PieChart';
end;

procedure TExtChartPieChart.InitDefaults;
begin
  inherited;
end;

class function TExtChartStackedColumnChart.JSClassName: string;
begin
  Result := 'Ext.chart.StackedColumnChart';
end;

procedure TExtChartStackedColumnChart.InitDefaults;
begin
  inherited;
end;

class function TExtChartColumnChart.JSClassName: string;
begin
  Result := 'Ext.chart.ColumnChart';
end;

procedure TExtChartColumnChart.InitDefaults;
begin
  inherited;
end;

class function TExtChartBarChart.JSClassName: string;
begin
  Result := 'Ext.chart.BarChart';
end;

procedure TExtChartBarChart.InitDefaults;
begin
  inherited;
end;

class function TExtChartStackedBarChart.JSClassName: string;
begin
  Result := 'Ext.chart.StackedBarChart';
end;

procedure TExtChartStackedBarChart.InitDefaults;
begin
  inherited;
end;

class function TExtChartLineChart.JSClassName: string;
begin
  Result := 'Ext.chart.LineChart';
end;

procedure TExtChartLineChart.InitDefaults;
begin
  inherited;
end;

end.
