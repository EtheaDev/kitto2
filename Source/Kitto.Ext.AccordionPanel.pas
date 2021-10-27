{-------------------------------------------------------------------------------
   Copyright 2012-2021 Ethea S.r.l.

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

unit Kitto.Ext.AccordionPanel;

{$I Kitto.Defines.inc}

interface

uses
  Kitto.Ext.Panel
  ;

type
  ///	<summary>
  ///  Displays subviews/controllers in an accordion.
  /// </summary>
  ///	<remarks>
  ///  All contained views and controllers must have ShowHeader=True.
  /// </remarks>
  TKExtAccordionPanelController = class(TKExtPanelControllerBase)
  protected
    procedure DoDisplay; override;
  end;

implementation

uses
  Kitto.JS.Controller
  ;

{ TKExtAccordionPanelController }

procedure TKExtAccordionPanelController.DoDisplay;
begin
  inherited;
  Layout := 'accordion';
  { TODO : make these customizable }
  MinSize := 20;
  MaxSize := 400;
  LayoutConfig.SetConfigItem('animate', True);
  DisplaySubViews;
  if Items.Count > 0 then
    &On('afterrender', GenerateAnonymousFunction(JSName + '.getLayout().setActiveItem(0);'));
end;

initialization
  TJSControllerRegistry.Instance.RegisterClass('AccordionPanel', TKExtAccordionPanelController);

finalization
  TJSControllerRegistry.Instance.UnregisterClass('AccordionPanel');

end.

