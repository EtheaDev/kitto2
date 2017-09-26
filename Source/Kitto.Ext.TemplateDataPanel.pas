{-------------------------------------------------------------------------------
   Copyright 2012-2017 Ethea S.r.l.

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

unit Kitto.Ext.TemplateDataPanel;

{$I Kitto.Defines.inc}

interface

uses
  Ext.Base
  , Ext.Chart
  , Ext.Data
  , EF.Tree
  , Kitto.Metadata.DataView
  , Kitto.JS.Types
  , Kitto.Ext.Base
  , Kitto.Ext.DataPanelLeaf
  ;

type
  TKExtTemplateDataPanel = class(TKExtDataPanelLeafController)
  strict private
    FView: TExtViewView;
    procedure SetupTemplate;
    function ProcessTemplate(const ATemplate: string): string;
  strict protected
    procedure InitDefaults; override;
    function CreateClientStore: TExtDataStore; override;
    procedure SetViewTable(const AValue: TKViewTable); override;
    procedure AddTopToolbarToolViewButtons; override;
    function IsActionSupported(const AActionName: string): Boolean; override;
    function GetSelectCall(const AMethod: TJSProcedure): TExtExpression; override;
    function GetSelectConfirmCall(const AMessage: string; const AMethod: TJSProcedure): string; override;
  end;

implementation

uses
  Classes
  , SysUtils
  , StrUtils
  , EF.Localization
  , EF.Macros
  , EF.StrUtils
  , Kitto.Types
  , Kitto.Metadata.Models
  , Kitto.Metadata.Views
  , Kitto.Web.Application
  , Kitto.Ext.Utils
  , Kitto.Ext.Controller
  ;

{ TKExtTemplateDataPanel }

procedure TKExtTemplateDataPanel.AddTopToolbarToolViewButtons;
begin
  inherited AddToolViewButtons(ViewTable.FindNode('Controller/ToolViews'), TopToolbar);
end;

function TKExtTemplateDataPanel.CreateClientStore: TExtDataStore;
begin
  Result := inherited CreateClientStore;
  FView.Store := Result;
end;

procedure TKExtTemplateDataPanel.SetupTemplate;
var
  LFileName: string;
  LTemplate: string;
begin
  LFileName := TKWebApplication.Current.Config.FindResourcePathName(Config.GetExpandedString('TemplateFileName'));
  if LFileName <> '' then
    FView.Tpl := ProcessTemplate(TEFMacroExpansionEngine.Instance.Expand(TextFileToString(LFileName, TEncoding.UTF8)))
  else
  begin
    LTemplate := Config.GetExpandedString('Template');
    if LTemplate = '' then
      FView.Tpl := _('TemplateFileName or Template parameters not specified.')
    else
      FView.Tpl := ProcessTemplate(LTemplate);
  end;
end;

function TKExtTemplateDataPanel.GetSelectCall(const AMethod: TJSProcedure): TExtExpression;
begin
  Result := GenerateAnonymousFunction(Format('ajaxDataViewSelection("yes", "", {params: {methodURL: "%s", dataView: %s, fieldNames: "%s"}});',
    [GetMethodURL(AMethod), FView.JSName, Join(ViewTable.GetKeyFieldAliasedNames, ',')]));
end;

function TKExtTemplateDataPanel.GetSelectConfirmCall(const AMessage: string; const AMethod: TJSProcedure): string;
begin
  Result := Format('selectDataViewConfirmCall("%s", "%s", %s, "%s", {methodURL: "%s", dataView: %s, fieldNames: "%s"});',
    [_(TKWebApplication.Current.Config.AppTitle), AMessage, FView.JSName, ViewTable.Model.CaptionField.FieldName, GetMethodURL(AMethod),
    FView.JSName, Join(ViewTable.GetKeyFieldAliasedNames, ',')]);
end;

function TKExtTemplateDataPanel.ProcessTemplate(const ATemplate: string): string;
var
  I: Integer;
begin
  Assert(Assigned(ViewTable));

  Result := ATemplate;
  for I := 0 to ViewTable.FieldCount - 1 do
  begin
    if ViewTable.Fields[I].IsPicture then
      Result := ReplaceText(Result, '{' + ViewTable.Fields[I].AliasedName + '}',
        '{' + ViewTable.Fields[I].GetURLFieldName + '}');
  end;
end;

procedure TKExtTemplateDataPanel.InitDefaults;
begin
  inherited;
  FView := TExtViewView.CreateAndAddToArray(Items);
  FView.EmptyText := _('No data to display.');
  FView.Region := rgCenter;
  FView.AutoScroll := True;
end;

function TKExtTemplateDataPanel.IsActionSupported(const AActionName: string): Boolean;
begin
  Result := True;
end;

procedure TKExtTemplateDataPanel.SetViewTable(const AValue: TKViewTable);
begin
  Assert(Assigned(AValue));
  Assert(Assigned(FView));

  inherited;
  FView.Id := Config.GetString('TemplateView/Id', 'templatedataview');
  FView.ItemSelector := Config.GetString('TemplateView/SelectorClass', 'div.thumb-wrap');
  FView.OverItemCls := Config.GetString('TemplateView/OverClass', 'x-item-over');
  FView.TrackOver := True;
  if ViewTable.GetBoolean('Controller/IsMultiSelect', False) then
    FView.MultiSelect := True
  else
    FView.SingleSelect := True;
  SetupTemplate;
end;

initialization
  TKExtControllerRegistry.Instance.RegisterClass('TemplateDataPanel', TKExtTemplateDataPanel);

finalization
  TKExtControllerRegistry.Instance.UnregisterClass('TemplateDataPanel');

end.
