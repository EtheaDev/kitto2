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
  Ext, ExtChart, ExtData,
  EF.Tree,
  Kitto.Metadata.DataView, Kitto.Ext.Base, Kitto.Ext.DataPanelLeaf;

type
  TKExTemplateDataPanel = class(TKExtDataPanelLeafController)
  strict private
    FDataView: TExtDataView;
    procedure CreateTemplateView;
  strict protected
    procedure InitDefaults; override;
    procedure SetViewTable(const AValue: TKViewTable); override;
    procedure AddTopToolbarToolViewButtons; override;
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
  LTemplateContent: string;
begin
  LTemplateContent := Config.GetExpandedString('Template');
  if FileExists(LTemplateContent) then
  begin
    FDataView.Tpl := TEFMacroExpansionEngine.Instance.Expand(TextFileToString(LTemplateContent, 
      TEncoding.UTF8))
  end
  else
    FDataView.Tpl := LTemplateContent;
  FDataView.Store := ClientStore;
end;

procedure TKExTemplateDataPanel.InitDefaults;
begin
  inherited;
  FDataView := TExtDataView.CreateAndAddTo(Items);
  FDataView.EmptyText := _('No data to display.');
  FDataView.Region := rgCenter;
  FDataView.Store := ClientStore;
  FDataView.AutoScroll := True;
end;

procedure TKExTemplateDataPanel.SetViewTable(const AValue: TKViewTable);
begin
  Assert(Assigned(AValue));
  inherited;
  CreateTemplateView;
end;

initialization
  TKExtControllerRegistry.Instance.RegisterClass('TemplateDataPanel', TKExTemplateDataPanel);

finalization
  TKExtControllerRegistry.Instance.UnregisterClass('TemplateDataPanel');

end.
