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
  EF.StrUtils, EF.Macros,
  Kitto.Ext.Session;

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
    LFullFileName := Session.Config.FindResourcePathName(LFileName);
    if LFullFileName <> '' then
      Html := TEFMacroExpansionEngine.Instance.Expand(TextFileToString(LFullFileName))
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

