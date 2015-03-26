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
    function CreateClientStore: TExtDataStore; override;
  published
  end;

implementation

uses
  Classes,
  SysUtils, StrUtils,
  EF.Localization, EF.Macros,
  Kitto.Types, Kitto.Ext.Utils, Kitto.Metadata.Models, Kitto.Metadata.Views,
  Kitto.Ext.Session, Kitto.Ext.Controller, Kitto.Ext.XSLTools;

{ TKExTemplateDataPanel }

function TKExTemplateDataPanel.CreateClientStore: TExtDataStore;
begin
  Result := inherited CreateClientStore;
end;

procedure TKExTemplateDataPanel.CreateTemplateView;
var
  LTemplateBody: string;
begin
  //LTemplateBody := Config.GetString('Controller/Template');
  LTemplateBody := '<p>Total: {Total}</p>';
(*
   '<tpl for="Root">'+
   '<div class="x-grid3-row-table" id="{ACTIVITY_ID}">Activity: {DESCRIPTION} - Phase: {PHASE}</div>'+
   '</tpl>'+
   '<div class="x-clear"></div>';
*)
  //FTemplate := TExtXTemplate.Create(Self, LTemplateBody);
  FDataView := TExtDataView.CreateAndAddTo(Items);
  FDataView.Tpl := LTemplateBody;
  FDataView.EmptyText := _('No data to display.');
  FDataView.AutoHeight := True;
  FDataView.Region := rgCenter;
  FDataView.Store := ClientStore;
end;

procedure TKExTemplateDataPanel.InitDefaults;
begin
  inherited;
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
