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

unit Kitto.Ext.ChartPanel;

{$I Kitto.Defines.inc}

interface

uses
  Ext.Base, Ext.Chart, Ext.Data,
  EF.Tree,
  Kitto.Metadata.DataView, Kitto.Ext.Base, Kitto.Ext.DataPanelLeaf;

type
  TKExtChartPanel = class(TKExtDataPanelLeafController)
  strict private
    FChart: TExtChartChart;
    procedure CreateAndInitChart(const AChartType: string);
    procedure CreateAndInitSeries(const AConfigNode: TEFNode);
    function GetLabelRenderer(const AFieldName: string): string;
    function CreateAndInitChartAxis(const AFieldName: string;
      const AConfigNode: TEFNode): TExtChartAxis;
    function CreateAndInitAxis(const AFieldName: string;
      const AConfigNode: TEFNode): TExtChartAxis;
  strict protected
    procedure SetViewTable(const AValue: TKViewTable); override;
  end;

implementation

uses
  SysUtils, StrUtils,
  EF.Localization, EF.Macros,
  Kitto.Types, Kitto.Metadata.Models, Kitto.JS,
  Kitto.Ext.Utils, Kitto.Ext.Session, Kitto.Ext.Controller;

{ TKExtChartPanel }

function TKExtChartPanel.GetLabelRenderer(const AFieldName: string): string;
var
  LViewField: TKViewField;
  LFormat: string;
  LDataType: TEFDataType;
begin
  Assert(AFieldName <> '');

  LViewField := ViewTable.FieldByName(AFieldName);
  LDataType := LViewField.GetActualDataType;
  LFormat := LViewField.DisplayFormat;
  if LDataType is TEFIntegerDataType then
  begin
    if LFormat = '' then
      LFormat := '0,000'; // '0';
    Result := Format('Ext.util.Format.numberRenderer("%s")', [AdaptExtNumberFormat(LFormat, Session.Config.UserFormatSettings)]);
  end
  else if (LDataType is TEFFloatDataType) or (LDataType is TEFDecimalDataType) then
  begin
    if LFormat = '' then
      LFormat := '0,000.' + DupeString('0', LViewField.DecimalPrecision);
    Result := Format('Ext.util.Format.numberRenderer("%s")', [AdaptExtNumberFormat(LFormat, Session.Config.UserFormatSettings)]);
  end
  else if LDataType is TEFCurrencyDataType then
  begin
    if LFormat = '' then
      LFormat := '0,000.00';
    Result := Format('Ext.util.Format.numberRenderer("%s")', [AdaptExtNumberFormat(LFormat, Session.Config.UserFormatSettings)]);
  end
  else if LDataType is TEFDateDataType then
  begin
    LFormat := LViewField.DisplayFormat;
    if LFormat = '' then
      LFormat := Session.Config.UserFormatSettings.ShortDateFormat;
    Result := Format('Ext.util.Format.dateRenderer("%s")', [TJS.DelphiDateFormatToJSDateFormat(LFormat)]);
  end
  else if LDataType is TEFTimeDataType then
  begin
    LFormat := LViewField.DisplayFormat;
    if LFormat = '' then
      LFormat := Session.Config.UserFormatSettings.ShortTimeFormat;
    Result := Format('function (v) { return formatTime(v, "%s"); }', [TJS.DelphiTimeFormatToJSTimeFormat(LFormat)]);
  end
  else if LDataType is TEFDateTimeDataType then
  begin
    LFormat := LViewField.DisplayFormat;
    if LFormat = '' then
      LFormat := Session.Config.UserFormatSettings.ShortDateFormat + ' ' +
        Session.Config.UserFormatSettings.ShortTimeFormat;
    Result := Format('Ext.util.Format.dateRenderer("%s")', [TJS.DelphiDateTimeFormatToJSDateTimeFormat(LFormat)])+
      Format('function (v) { return formatTime(v, "%s"); }', [TJS.DelphiTimeFormatToJSTimeFormat(LFormat)]);
  end
  else
    Result := '';
end;

function TKExtChartPanel.CreateAndInitChartAxis(const AFieldName: string;
  const AConfigNode: TEFNode): TExtChartAxis;
var
  LDataType: TEFDataType;
begin
  Assert(AFieldName <> '');

  LDataType := ViewTable.FieldByName(AFieldName).GetActualDataType;

  if LDataType is TEFDateTimeDataTypeBase then
  begin
    Result := TExtChartTimeAxis.Create(Self);
    TExtChartTimeAxis(Result).StackingEnabled := True;
    if Assigned(AConfigNode) then
    begin
      if AConfigNode.HasChild('MajorTimeUnit') then
        TExtChartTimeAxis(Result).MajorTimeUnit := AConfigNode.GetString('MajorTimeUnit');
      if AConfigNode.HasChild('MajorUnit') then
        TExtChartTimeAxis(Result).MajorUnit := AConfigNode.GetInteger('MajorUnit');
      if AConfigNode.HasChild('MinorUnit') then
        TExtChartTimeAxis(Result).MinorUnit := AConfigNode.GetInteger('MinorUnit');
      if AConfigNode.HasChild('Max') then
        TExtChartTimeAxis(Result).Maximum := AConfigNode.GetInteger('Max');
      if AConfigNode.HasChild('Min') then
        TExtChartTimeAxis(Result).Minimum := AConfigNode.GetInteger('Min');
    end;
  end
  else if LDataType is TEFNumericDataTypeBase then
  begin
    Result := TExtChartNumericAxis.Create(Self);
    TExtChartNumericAxis(Result).StackingEnabled := True;
    if Assigned(AConfigNode) then
    begin
      if AConfigNode.HasChild('Max') then
        TExtChartNumericAxis(Result).Maximum := AConfigNode.GetInteger('Max');
      if AConfigNode.HasChild('Min') then
        TExtChartNumericAxis(Result).Minimum := AConfigNode.GetInteger('Min');
      if AConfigNode.HasChild('MajorUnit') then
        TExtChartNumericAxis(Result).MajorUnit := AConfigNode.GetInteger('MajorUnit');
      if AConfigNode.HasChild('MinorUnit') then
        TExtChartNumericAxis(Result).MinorUnit := AConfigNode.GetInteger('MinorUnit');
    end;
  end
  else
    Result := TExtChartCategoryAxis.Create(Self);
end;

procedure TKExtChartPanel.CreateAndInitSeries(const AConfigNode: TEFNode);
var
  I: Integer;

  procedure CreateAndInitASeries(const AConfigNode: TEFNode);
  var
    LOption: string;
    LStyle: TEFNode;
    LSeries: TExtChartSeries;

    function TranslateSeriesType(const ASeriesType: string): string;
    begin
      Result := AnsiLowerCase(ASeriesType);
    end;

  begin
    Assert(Assigned(AConfigNode));

    if FChart is TExtChartPieChart then
    begin
      LSeries := TExtChartPieSeries.CreateAndAddToArray(FChart.Series);
    end
    else
    begin
      LSeries := TExtChartCartesianSeries.CreateAndAddToArray(FChart.Series);
      LOption := AConfigNode.GetString('XField');
      if LOption <> '' then
        TExtChartCartesianSeries(LSeries).XField := LOption;
      LOption := AConfigNode.GetString('YField');
      if LOption <> '' then
        TExtChartCartesianSeries(LSeries).YField := LOption;
    end;
    LOption := TranslateSeriesType(AConfigNode.GetString('Type'));
    if LOption <> '' then
      LSeries.TypeJS := LOption;
    LOption := _(AConfigNode.GetString('DisplayName'));
    if LOption <> '' then
      LSeries.DisplayName := LOption;
    LStyle := AConfigNode.FindNode('Style');
    if Assigned(LStyle) then
    begin
      LSeries.Style := TExtChartSeriesStyle.Create(Self);
      LOption := LStyle.GetString('Color');
      if LOption <> '' then
        LSeries.Style.Color := LOption;
      LOption := LStyle.GetString('Image');
      if LOption <> '' then
        LSeries.Style.Image := TEFMacroExpansionEngine.Instance.Expand(LOption);
      LOption := LStyle.GetString('Mode');
      if LOption <> '' then
        LSeries.Style.Mode := LOption;
    end;
  end;

begin
  if Assigned(AConfigNode) then
  begin
    for I := 0 to AConfigNode.ChildCount - 1 do
      CreateAndInitASeries(AConfigNode.Children[I]);
  end;
end;

function TKExtChartPanel.CreateAndInitAxis(const AFieldName: string;
  const AConfigNode: TEFNode): TExtChartAxis;
var
  LOption: string;
begin
  Result := CreateAndInitChartAxis(AFieldName, AConfigNode);
  LOption := GetLabelRenderer(AFieldName);
  if LOption <> '' then
    Result.LabelFunction := LOption;
  if Assigned(AConfigNode) then
  begin
    LOption := _(AConfigNode.GetString('Title'));
    if LOption <> '' then
      Result.Title := LOption;
  end;
end;

procedure TKExtChartPanel.CreateAndInitChart(const AChartType: string);

  function GetAxisField(const AAxis: string): string;
  var
    LSeries: TEFNode;
    I: Integer;
  begin
    Result := Config.GetString(Format('Chart/Axes/%s/Field', [AAxis]));
    if Result = '' then
    begin
      LSeries := Config.FindNode('Chart/Series');
      if not Assigned(LSeries) then
        raise EKError.CreateFmt('A chart''s %s axis must either have a Field or one or more Series.', [AAxis]);
      for I := 0 to LSeries.ChildCount - 1 do
      begin
        Result := LSeries.Children[I].GetString(Format('%sField', [AAxis]));
        if Result <> '' then
          Break;
      end;
      if Result = '' then
        raise EKError.CreateFmt('No valid series found for chart''s %s axis. At least one series with a %sField specification needed.', [AAxis, AAxis]);
    end;
  end;

  procedure CreateDefaultXYAxes;
  begin
    FChart.XField := GetAxisField('X');
    FChart.XAxis := CreateAndInitAxis(FChart.XField, Config.FindNode('Chart/Axes/X'));
    FChart.YField := GetAxisField('Y');
    FChart.YAxis := CreateAndInitAxis(FChart.YField, Config.FindNode('Chart/Axes/Y'));
  end;

var
  LOption, LFieldName: string;
begin
  Assert(ClientStore <> nil);

  if SameText(AChartType, 'Line') then
  begin
    FChart := TExtChartLineChart.CreateAndAddToArray(Items);
    CreateDefaultXYAxes;
  end
  else if SameText(AChartType, 'Bar') then
  begin
    FChart := TExtChartBarChart.CreateAndAddToArray(Items);
    CreateDefaultXYAxes;
  end
  else if SameText(AChartType, 'Column') then
  begin
    FChart := TExtChartColumnChart.CreateAndAddToArray(Items);
    CreateDefaultXYAxes;
  end
  else if SameText(AChartType, 'StackedBar') then
  begin
    FChart := TExtChartStackedBarChart.CreateAndAddToArray(Items);
    CreateDefaultXYAxes;
  end
  else if SameText(AChartType, 'StackedColumn') then
  begin
    FChart := TExtChartStackedColumnChart.CreateAndAddToArray(Items);
    CreateDefaultXYAxes;
  end
  else if SameText(AChartType, 'Pie') then
  begin
    FChart := TExtChartPieChart.CreateAndAddToArray(Items);
    LFieldName := Config.GetString('Chart/DataField');
    TExtChartPieChart(FChart).DataField := LFieldName;
    LFieldName := Config.GetString('Chart/CategoryField');
    TExtChartPieChart(FChart).CategoryField := LFieldName;
  end
  else
    raise EKError.CreateFmt(_('Unknown chart type %s.'), [AChartType]);
  FChart.Store := ClientStore;
  FChart.Url := Format('%s/resources/charts.swf', [Session.ExtPath]);
  FChart.Region := rgCenter;
  LOption := Config.GetExpandedString('Chart/ChartStyle');
  if LOption <> '' then
    FChart.ChartStyle := JSObject(LOption, '', False);
  LOption := Config.GetExpandedString('Chart/TipRenderer');
  if LOption <> '' then
    FChart.TipRenderer := JSFunction('chart, record, index, series', LOption);
  if FChart is TExtChartCartesianChart then
    CreateAndInitSeries(Config.FindNode('Chart/Series'));
end;

procedure TKExtChartPanel.SetViewTable(const AValue: TKViewTable);
begin
  inherited;

  Assert(Assigned(AValue));

  CreateAndInitChart(Config.GetString('Chart/Type'));

  { TODO : Provide a way to hook clicks to ajax calls }
  //  FChart.Listeners := JSObject(Format('itemclick: function(o){ var rec = %s.getAt(o.index); ' +
  //    'alert("You chose {0}.", rec.get("%s"));}', [FChart.Store.JSName, FChart.XField]));
end;

initialization
  TKExtControllerRegistry.Instance.RegisterClass('ChartPanel', TKExtChartPanel);

finalization
  TKExtControllerRegistry.Instance.UnregisterClass('ChartPanel');

end.
