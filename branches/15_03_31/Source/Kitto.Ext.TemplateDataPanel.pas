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

procedure TKExTemplateDataPanel.CreateTemplateView;
begin
  FDataView.Tpl := Config.GetExpandedString('Template');
  //FDataView.StoreArray := JSArray('{id: "1", descr: "one"}, {id: "2", descr: "two"}, {id: "3", descr: "three"}');
  FDataView.Store := ClientStore;
end;

procedure TKExTemplateDataPanel.InitDefaults;
begin
  inherited;
  FDataView := TExtDataView.CreateAndAddTo(Items);
  FDataView.EmptyText := _('No data to display.');
  FDataView.AutoHeight := False;
  FDataView.Region := rgCenter;
  FDataView.Store := ClientStore;
  FDataView.AutoScroll := True;
  FDataView.AutoWidth := True;
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
