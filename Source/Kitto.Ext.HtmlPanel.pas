unit Kitto.Ext.HtmlPanel;

interface

uses
  Kitto.Ext.Base, Kitto.Ext.Controller;

type
  TKExtHtmlPanelController = class(TKExtPanelControllerBase)
  protected
    procedure DoDisplay; override;
  end;

implementation

uses
  Ext,
  Kitto.Environment;

{ TKExtHtmlPanelController }

procedure TKExtHtmlPanelController.DoDisplay;
begin
  inherited;
  Title := 'Test';
end;

initialization
  TKControllerRegistry.Instance.RegisterClass('HtmlPanel', TKExtHtmlPanelController);

finalization
  TKControllerRegistry.Instance.UnregisterClass('HtmlPanel');

end.

