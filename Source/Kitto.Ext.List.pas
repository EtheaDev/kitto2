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
  Kitto.Ext.Session, Kitto.Metadata.DataView,
  Kitto.Ext.Controller;

{ TKExtListPanelController }

procedure TKExtListPanelController.InitComponents;
begin
  inherited;
  Title := Session.Config.MacroExpansionEngine.Expand(ViewTable.PluralDisplayLabel);

  FGridPanel := TKExtGridPanel.AddTo(Items);
  FGridPanel.ServerStore := ServerStore;
  FGridPanel.ViewTable := ViewTable;
end;

procedure TKExtListPanelController.LoadData;
begin
  inherited;
  Assert(Assigned(FGridPanel));

  FGridPanel.LoadData;
end;

initialization
  TKExtControllerRegistry.Instance.RegisterClass('List', TKExtListPanelController);

finalization
  TKExtControllerRegistry.Instance.UnregisterClass('List');

end.
