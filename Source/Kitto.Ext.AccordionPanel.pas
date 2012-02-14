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

unit Kitto.Ext.AccordionPanel;

{$I Kitto.Defines.inc}

interface

uses
  Ext,
  Kitto.Ext.Base, Kitto.Metadata.Views;

type
  TKExtAccordionPanelController = class(TKExtPanelControllerBase)
  private
    procedure DisplaySubViews;
  protected
    procedure DoDisplay; override;
  end;

implementation

uses
  ExtPascal, ExtLayout,
  EF.Tree,
  Kitto.Ext.Controller, Kitto.Ext.Session;

{ TKExtAccordionPanelController }

procedure TKExtAccordionPanelController.DoDisplay;
begin
  inherited;
  Layout := lyAccordion;
  { TODO : make these customizable }
  Width := 180;
  MinSize := 20;
  MaxSize := 400;

  LayoutConfig := JSObject('animate:true');
  DisplaySubViews;
end;

procedure TKExtAccordionPanelController.DisplaySubViews;
var
  LController: IKExtController;
  LViews: TEFNode;
  I: Integer;
begin
  LViews := View.FindNode('Controller/SubViews');
  if Assigned(LViews) then
  begin
    for I := 0 to LViews.ChildCount - 1 do
    begin
      LController := TKExtControllerFactory.Instance.CreateController(
        Session.Config.Views.ViewByNode(LViews.Children[I]), Self);
      LController.Display;
    end;
    if Items.Count > 0 then
      On('afterrender', JSFunction(JSName + '.getLayout().setActiveItem(0);'));
  end;
end;

initialization
  TKExtControllerRegistry.Instance.RegisterClass('AccordionPanel', TKExtAccordionPanelController);

finalization
  TKExtControllerRegistry.Instance.UnregisterClass('AccordionPanel');

end.

