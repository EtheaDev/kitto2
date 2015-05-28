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

unit Kitto.Ext.TemplateDataPanel;

{$I Kitto.Defines.inc}

interface

uses
  Ext, ExtChart, ExtData, ExtPascal, ExtPascalUtils,
  EF.Tree,
  Kitto.Metadata.DataView, Kitto.Ext.Base, Kitto.Ext.DataPanelLeaf;

type
  TKExTemplateDataPanel = class(TKExtDataPanelLeafController)
  strict private
    FDataView: TExtDataView;
    procedure CreateTemplateView;
    function ProcessTemplate(const ATemplate: string): string;
  strict protected
    procedure InitDefaults; override;
    procedure SetViewTable(const AValue: TKViewTable); override;
    procedure AddTopToolbarToolViewButtons; override;
    function IsActionSupported(const AActionName: string): Boolean; override;
  published
  end;

implementation

uses
  Classes,
  SysUtils, StrUtils,
  EF.Localization, EF.Macros, EF.StrUtils,
  Kitto.Types, Kitto.Ext.Utils, Kitto.Metadata.Models, Kitto.Metadata.Views,
  Kitto.Ext.Session, Kitto.Ext.Controller, Kitto.Ext.XSLTools;

{ TKExTemplateDataPanel }

procedure TKExTemplateDataPanel.AddTopToolbarToolViewButtons;
begin
  inherited AddToolViewButtons(ViewTable.FindNode('Controller/ToolViews'), TopToolbar);
end;

procedure TKExTemplateDataPanel.CreateTemplateView;
var
  LFileName: string;
  LTemplate: string;
begin
  LFileName := Session.Config.FindResourcePathName(Config.GetExpandedString('TemplateFileName'));
  if LFileName <> '' then
    FDataView.Tpl := ProcessTemplate(TEFMacroExpansionEngine.Instance.Expand(TextFileToString(LFileName, TEncoding.UTF8)))
  else
  begin
    LTemplate := Config.GetExpandedString('Template');
    if LTemplate = '' then
      FDataView.Tpl := 'TemplateFileName or Template parameters not specified.'
    else
      FDataView.Tpl := ProcessTemplate(LTemplate);
  end;
  FDataView.Store := ClientStore;
end;

function TKExTemplateDataPanel.ProcessTemplate(const ATemplate: string): string;
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

procedure TKExTemplateDataPanel.InitDefaults;
begin
  inherited;
  FDataView := TExtDataView.CreateAndAddTo(Items);
  FDataView.EmptyText := _('No data to display.');
  FDataView.Region := rgCenter;
  FDataView.AutoScroll := True;
end;

function TKExTemplateDataPanel.IsActionSupported(const AActionName: string): Boolean;
begin
  { TODO : Implement GetSelectCall in order to support actions that require selection. }
  Result := MatchText(AActionName, ['Add']);
end;

procedure TKExTemplateDataPanel.SetViewTable(const AValue: TKViewTable);
begin
  Assert(Assigned(AValue));
  inherited;
  FDataView.Id := Config.GetString('TemplateView/Id');
  FDataView.ItemSelector := Config.GetString('TemplateView/SelectorClass');
  FDataView.OverClass := Config.GetString('TemplateView/OverClass');
  if ViewTable.GetBoolean('Controller/IsMultiSelect', False) then
    FDataView.MultiSelect := True
  else
    FDataView.SingleSelect := True;
//  FDataView.On('dblclick', JSFunction('this, idx, node, e', GetAjaxCode(
//    DefaultAction, )
  CreateTemplateView;
end;

initialization
  TKExtControllerRegistry.Instance.RegisterClass('TemplateDataPanel', TKExTemplateDataPanel);

finalization
  TKExtControllerRegistry.Instance.UnregisterClass('TemplateDataPanel');

end.
