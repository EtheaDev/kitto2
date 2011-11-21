unit Controllers;

interface

uses
  Kitto.Ext.Controller, Kitto.Ext.DataTool, Kitto.Ext.Base;

type
  TTestRowAction = class(TKExtDataToolController)
  protected
    procedure ExecuteTool; override;
  published
    procedure Callback;
  end;

implementation

uses
  Ext;

{ TTestRowAction }

procedure TTestRowAction.Callback;
begin
  ExtMessageBox.Alert('Test Action', 'This is a callback');
end;

procedure TTestRowAction.ExecuteTool;
begin
  inherited;
  ExtMessageBox.Alert('Test Action', 'This is a custom action', Ajax(Callback));
end;

initialization
  TKExtControllerRegistry.Instance.RegisterClass('TestRowAction', TTestRowAction);

finalization
  TKExtControllerRegistry.Instance.UnregisterClass('TestRowAction');

end.
