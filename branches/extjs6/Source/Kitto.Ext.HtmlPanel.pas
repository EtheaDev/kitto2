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
    function GetDefaultAllowClose: Boolean; override;
    procedure DoDisplay; override;
  end;

implementation

uses
  SysUtils,
  Ext,
  EF.StrUtils, EF.Macros, EF.Localization,
  Kitto.Ext.Session, Kitto.Metadata.Views;

{ TKExtHtmlPanelController }

procedure TKExtHtmlPanelController.DoDisplay;
var
  LFileName: string;
  LHtml: string;
  LView: TKView;
begin
  inherited;
  if (Title = '') then
  begin
    LView := View;
    if Assigned(LView) then
      Title := _(LView.DisplayLabel);
  end;
  AutoScroll := False;

  LFileName := Config.GetExpandedString('FileName');
  if LFileName <> '' then
    LoadHtml(LFileName)
  else
  begin
    LHtml := Config.GetExpandedString('Html');
    if LHtml = '' then
      Html := 'FileName or Html parameters not specified.'
    else
      Html := LHtml;
  end
end;

function TKExtHtmlPanelController.GetDefaultAllowClose: Boolean;
begin
  Result := False;
end;

initialization
  TKExtControllerRegistry.Instance.RegisterClass('HtmlPanel', TKExtHtmlPanelController);

finalization
  TKExtControllerRegistry.Instance.UnregisterClass('HtmlPanel');

end.

