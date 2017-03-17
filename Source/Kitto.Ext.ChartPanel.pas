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
    FChart: TExtChartAbstractChart;
    FCHartType: string;
    procedure CreateAndInitChart;
    procedure CreateAndInitSeries(const AConfigNode: TEFNode);
    function GetLabelRenderer(const AFieldName: string): string;
    function CreateAndInitChartAxis(const AFieldName: string;
      const AConfigNode: TEFNode): TExtChartAxis;
    function CreateAndInitAxis(const AConfigNode: TEFNode): TExtChartAxis;
    function GetDefaultSeriesType(const AChartType: string): string;
    procedure InitLegend(const AConfigNode: TEFNode);
  strict protected
    procedure SetViewTable(const AValue: TKViewTable); override;
  end;

implementation

uses
  SysUtils
  , StrUtils
  , Generics.Collections
  , EF.Localization
  , EF.Macros
  , Kitto.Types
  , Kitto.Metadata.Models
  , Kitto.JS
  , Kitto.JS.Formatting
  , Kitto.Web.Application
  , Kitto.Ext.Utils
  , Kitto.Ext.Controller
  ;

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
    Result := Format('Ext.util.Format.numberRenderer("%s")', [AdaptExtNumberFormat(LFormat, TKWebApplication.Current.Config.UserFormatSettings)]);
  end
  else if (LDataType is TEFFloatDataType) or (LDataType is TEFDecimalDataType) then
  begin
    if LFormat = '' then
      LFormat := '0,000.' + DupeString('0', LViewField.DecimalPrecision);
    Result := Format('Ext.util.Format.numberRenderer("%s")', [AdaptExtNumberFormat(LFormat, TKWebApplication.Current.Config.UserFormatSettings)]);
  end
  else if LDataType is TEFCurrencyDataType then
  begin
    if LFormat = '' then
      LFormat := '0,000.00';
    Result := Format('Ext.util.Format.numberRenderer("%s")', [AdaptExtNumberFormat(LFormat, TKWebApplication.Current.Config.UserFormatSettings)]);
  end
  else if LDataType is TEFDateDataType then
  begin
    LFormat := LViewField.DisplayFormat;
    if LFormat = '' then
      LFormat := TKWebApplication.Current.Config.UserFormatSettings.ShortDateFormat;
    Result := Format('Ext.util.Format.dateRenderer("%s")', [TJS.DelphiDateFormatToJSDateFormat(LFormat)]);
  end
  else if LDataType is TEFTimeDataType then
  begin
    LFormat := LViewField.DisplayFormat;
    if LFormat = '' then
      LFormat := TKWebApplication.Current.Config.UserFormatSettings.ShortTimeFormat;
    Result := Format('function (v) { return formatTime(v, "%s"); }', [TJS.DelphiTimeFormatToJSTimeFormat(LFormat)]);
  end
  else if LDataType is TEFDateTimeDataType then
  begin
    LFormat := LViewField.DisplayFormat;
    if LFormat = '' then
      LFormat := TKWebApplication.Current.Config.UserFormatSettings.ShortDateFormat + ' ' +
        TKWebApplication.Current.Config.UserFormatSettings.ShortTimeFormat;
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

function TKExtChartPanel.GetDefaultSeriesType(const AChartType: string): string;
begin
  if AChartType = 'Polar' then
    Result := 'Pie' // Polar(Pie3D, Radar)
  else if AChartType = 'Cartesian' then
    Result := 'Line' // StackedCartesian (Area, Bar), CandleStick, Scatter
  else if AChartType = 'SpaceFilling' then
    Result := 'Gauge';
end;

procedure TKExtChartPanel.CreateAndInitSeries(const AConfigNode: TEFNode);
var
  LOption: string;
//    LStyle: TEFNode;
  LSeries: TExtChartSeries;
  LType: string;
  LRenderer: string;
begin
  Assert(Assigned(AConfigNode));

  LType := AConfigNode.GetString('Type', GetDefaultSeriesType(FChartType));
  LSeries := TExtChartSeries.CreateInlineByType(LType, FChart);

  LOption := AConfigNode.GetString('Label/Field');
  if LOption <> '' then
  begin
    LSeries.&Label.SetConfigItem('field', LOption);
    LRenderer := GetLabelRenderer(LOption);
    if LRenderer <> '' then
      LSeries.&Label.SetConfigItem('renderer', JSExpressionFromCodeBlock(LRenderer));

    if AConfigNode.GetBoolean('Label/Field/CalloutLine') then
    begin
      LSeries.&Label.CalloutLine.SetConfigItem('length', 60);
      LSeries.&Label.CalloutLine.SetConfigItem('width', 2);
    end;
  end;
  LSeries.Highlight := True;
  //LSeries.ToolTip.TrackMouse := True;
  //LSeries.ToolTip.Renderer := ...

  if LSeries is TExtChartPolarSeries then
  begin
    LOption := AConfigNode.GetString('AngleField');
    if LOption <> '' then
      TExtChartPolarSeries(LSeries).AngleField := LOption;
  end
  else if LSeries is TExtChartCartesianSeries then
  begin
    LOption := AConfigNode.GetString('XField');
    if LOption <> '' then
      TExtChartCartesianSeries(LSeries).XField := LOption;
    LOption := AConfigNode.GetString('YField');
    if LOption <> '' then
      TExtChartCartesianSeries(LSeries).YField := LOption;
  end;

  LOption := _(AConfigNode.GetString('Title'));
  if LOption <> '' then
    LSeries.Title := LOption;
//    LStyle := AConfigNode.FindNode('Style');
//    if Assigned(LStyle) then
//    begin
//      LSeries.Style := TExtChartSeriesStyle.Create(Self);
//      LOption := LStyle.GetString('Color');
//      if LOption <> '' then
//        LSeries.Style.Color := LOption;
//      LOption := LStyle.GetString('Image');
//      if LOption <> '' then
//        LSeries.Style.Image := TEFMacroExpansionEngine.Instance.Expand(LOption);
//      LOption := LStyle.GetString('Mode');
//      if LOption <> '' then
//        LSeries.Style.Mode := LOption;
//    end;
//    LSeries.Highlight := AConfigNode.GetBoolean('Highlight');
end;

function TKExtChartPanel.CreateAndInitAxis(const AConfigNode: TEFNode): TExtChartAxis;
var
  LFieldName: string;
  LOption: string;
begin
  LFieldName := AConfigNode.GetString('FieldName'); // temporary
  Result := CreateAndInitChartAxis(LFieldName, AConfigNode);
  LOption := GetLabelRenderer(LFieldName);
  if LOption <> '' then
    Result.LabelFunction := LOption;
  if Assigned(AConfigNode) then
  begin
    LOption := _(AConfigNode.GetString('Title'));
    if LOption <> '' then
      Result.Title := LOption;
  end;
end;

procedure TKExtChartPanel.CreateAndInitChart;
var
  LNode: TEFNode;
  I: Integer;
  LOption: TEFNode;

//  function GetAxisField(const AAxis: string): string;
//  var
//    LSeries: TEFNode;
//    I: Integer;
//  begin
//    Result := Config.GetString(Format('Chart/Axes/%s/Field', [AAxis]));
//    if Result = '' then
//    begin
//      LSeries := Config.FindNode('Chart/Series');
//      if not Assigned(LSeries) then
//        raise EKError.CreateFmt('A chart''s %s axis must either have a Field or one or more Series.', [AAxis]);
//      for I := 0 to LSeries.ChildCount - 1 do
//      begin
//        Result := LSeries.Children[I].GetString(Format('%sField', [AAxis]));
//        if Result <> '' then
//          Break;
//      end;
//      if Result = '' then
//        raise EKError.CreateFmt('No valid series found for chart''s %s axis. At least one series with a %sField specification needed.', [AAxis, AAxis]);
//    end;
//  end;

begin
  Assert(ClientStore <> nil);

  FChart := TExtChartAbstractChart.CreateByType(FChartType, Items);
  FChart.Store := ClientStore;
  FChart.Region := rgCenter;

  LOption := Config.FindNode('Chart/InnerPadding');
  if Assigned(LOption) then
    FChart.InnerPadding := LOption.AsInteger;

  LOption := Config.FindNode('Chart/InsetPadding');
  if Assigned(LOption) then
    FChart.InsetPadding := LOption.AsInteger;

  FChart.Theme := Config.GetString('Chart/Theme', 'default-gradients');

  LNode := Config.FindNode('Chart/Axes');
  if Assigned(LNode) then
  begin
    for I := 0 to LNode.ChildCount - 1 do
      CreateAndInitAxis(LNode.Children[I]);
  end;

  LNode := Config.FindNode('Chart/Series');
  if Assigned(LNode) then
  begin
    for I := 0 to LNode.ChildCount - 1 do
      CreateAndInitSeries(LNode.Children[I]);
  end;

  InitLegend(Config.FindNode('Chart/Legend'));
//  LOption := Config.GetExpandedString('Chart/TipRenderer');
//  if LOption <> '' then
//    FChart.TipRenderer := GenerateAnonymousFunction('chart, record, index, series', LOption);
  { TODO : Provide a way to hook clicks to ajax calls }
  //  FChart.Listeners := JSObject(Format('itemclick: function(o){ var rec = %s.getAt(o.index); ' +
  //    'alert("You chose {0}.", rec.get("%s"));}', [FChart.Store.JSName, FChart.XField]));
end;

procedure TKExtChartPanel.InitLegend(const AConfigNode: TEFNode);
var
  LOption: TEFNode;
begin
  if Assigned(AConfigNode) then
  begin
    if AConfigNode.DataType.IsBoolean then
      FChart.LegendBool := AConfigNode.AsBoolean
    else
    begin
      LOption := AConfigNode.FindChild('Docked');
      if Assigned(LOption) then
        FChart.Legend.Docked := LOption.AsString.ToLower;
      LOption := AConfigNode.FindChild('Padding');
      if Assigned(LOption) then
        FChart.Legend.Padding := LOption.AsInteger;
      LOption := AConfigNode.FindChild('Toggleable');
      if Assigned(LOption) then
        FChart.Legend.Toggleable := LOption.AsBoolean;
    end;
  end;
end;

procedure TKExtChartPanel.SetViewTable(const AValue: TKViewTable);
begin
  inherited;

  Assert(Assigned(AValue));

  FChartType := Config.GetString('Chart/Type');
  CreateAndInitChart;
end;

initialization
  TKExtControllerRegistry.Instance.RegisterClass('ChartPanel', TKExtChartPanel);

finalization
  TKExtControllerRegistry.Instance.UnregisterClass('ChartPanel');

end.
