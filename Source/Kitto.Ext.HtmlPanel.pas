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
  LHtml: string;
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
  begin
    LHtml := View.GetExpandedString('Controller/Html');
    if LHtml = '' then
      Html := 'FileName or Html parameters not specified.'
    else
      Html := LHtml;
  end
end;

initialization
  TKExtControllerRegistry.Instance.RegisterClass('HtmlPanel', TKExtHtmlPanelController);

finalization
  TKExtControllerRegistry.Instance.UnregisterClass('HtmlPanel');

end.

