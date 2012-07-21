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
  ///	<summary>Displays subviews/controllers in an accordion.</summary>
  ///	<remarks>All contained views and controllers must have
  ///	ShowHeader=True.</remarks>
  TKExtAccordionPanelController = class(TKExtPanelControllerBase)
  private
    procedure DisplaySubViewsAndControllers;
  protected
    procedure DoDisplay; override;
  end;

implementation

uses
  SysUtils,
  ExtPascal, ExtLayout,
  EF.Tree, EF.Localization,
  Kitto.Types, Kitto.AccessControl, Kitto.Ext.Controller, Kitto.Ext.Session;

{ TKExtAccordionPanelController }

procedure TKExtAccordionPanelController.DoDisplay;
begin
  inherited;
  Layout := lyAccordion;
  { TODO : make these customizable }
  MinSize := 20;
  MaxSize := 400;

  LayoutConfig := JSObject('animate:true');
  DisplaySubViewsAndControllers;
end;

procedure TKExtAccordionPanelController.DisplaySubViewsAndControllers;
var
  LController: IKExtController;
  LViews: TEFNode;
  I: Integer;
  LView: TKView;
begin
  LViews := Config.FindNode('SubViews');
  if Assigned(LViews) then
  begin
    for I := 0 to LViews.ChildCount - 1 do
    begin
      if SameText(LViews.Children[I].Name, 'View') then
      begin
        LView := Session.Config.Views.ViewByNode(LViews.Children[I]);
        if LView.IsAccessGranted(ACM_VIEW) then
        begin
          LController := TKExtControllerFactory.Instance.CreateController(LView, Self);
          LController.Display;
        end;
      end
      else if SameText(LViews.Children[I].Name, 'Controller') then
      begin
        LController := TKExtControllerFactory.Instance.CreateController(View, Self, LViews.Children[I]);
        InitSubController(LController);
        LController.Display;
      end
      else
        raise EKError.Create(_('AccordionPanel''s SubViews node may only contain View or Controller subnodes.'));
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

