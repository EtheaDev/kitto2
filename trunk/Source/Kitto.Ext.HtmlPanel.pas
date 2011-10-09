unit Kitto.Ext.HtmlPanel;

{$I Kitto.Defines.inc}

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
  SysUtils,
  Ext,
  EF.StrUtils,
  Kitto.Environment;

{ TKExtHtmlPanelController }

procedure TKExtHtmlPanelController.DoDisplay;
var
  LFileName: string;
  LFullFileName: string;
begin
  inherited;
  Title := View.DisplayLabel;
  AutoScroll := True;

  LFileName := View.GetExpandedString('Controller/FileName');
  if LFileName <> '' then
  begin
    LFullFileName := Environment.FindResourcePathName(LFileName);
    if LFullFileName <> '' then
      Html := Environment.MacroExpansionEngine.Expand(TextFileToString(LFullFileName))
    else
      Html := Format('File %s not found.', [LFileName]);
  end
  else
    Html := 'FileName not specified.';
end;

initialization
  TKExtControllerRegistry.Instance.RegisterClass('HtmlPanel', TKExtHtmlPanelController);

finalization
  TKExtControllerRegistry.Instance.UnregisterClass('HtmlPanel');

end.

