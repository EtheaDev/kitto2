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

unit Kitto.Ext.List;

{$I Kitto.Defines.inc}

interface

uses
  Kitto.Ext.DataPanel, Kitto.Ext.GridPanel;

type
  TKExtListPanelController = class(TKExtDataPanelController)
  private
    FGridPanel: TKExtGridPanel;
  protected
    procedure LoadData; override;
    procedure InitComponents; override;
  end;

implementation

uses
  Ext,
  EF.Localization,
  Kitto.Ext.Session, Kitto.Metadata.DataView,
  Kitto.Ext.Controller;

{ TKExtListPanelController }

procedure TKExtListPanelController.InitComponents;
begin
  inherited;
  Title := _(Session.Config.MacroExpansionEngine.Expand(ViewTable.PluralDisplayLabel));

  FGridPanel := TKExtGridPanel.AddTo(Items);
  FGridPanel.ServerStore := ServerStore;
  FGridPanel.ViewTable := ViewTable;
end;

procedure TKExtListPanelController.LoadData;
begin
  Assert(Assigned(FGridPanel));

  FGridPanel.LoadData;
end;

initialization
  TKExtControllerRegistry.Instance.RegisterClass('List', TKExtListPanelController);

finalization
  TKExtControllerRegistry.Instance.UnregisterClass('List');

end.
