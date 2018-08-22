{-------------------------------------------------------------------------------
   Copyright 2012-2018 Ethea S.r.l.

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
  Kitto.Metadata.Views
  , Kitto.Ext.Panel
  ;

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
  SysUtils
  , EF.Tree
  , EF.Localization
  , Kitto.Types
  , Kitto.AccessControl
  , Kitto.JS
  , Kitto.JS.Controller
  , Ext.Base
  , Kitto.Web.Application
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
  DisplaySubViewsAndControllers;
end;

procedure TKExtAccordionPanelController.DisplaySubViewsAndControllers;
var
  LController: IJSController;
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
        LView := TKWebApplication.Current.Config.Views.ViewByNode(LViews.Children[I]);
        if LView.IsAccessGranted(ACM_VIEW) then
        begin
          LController := TJSControllerFactory.Instance.CreateController(Self, LView, Self);
          LController.Display;
        end;
      end
      else if SameText(LViews.Children[I].Name, 'Controller') then
      begin
        LController := TJSControllerFactory.Instance.CreateController(Self, View, Self, LViews.Children[I]);
        InitSubController(LController);
        LController.Display;
      end
      else
        raise EKError.Create(_('AccordionPanel''s SubViews node may only contain View or Controller subnodes.'));
    end;
    if Items.Count > 0 then
      &On('afterrender', GenerateAnonymousFunction(JSName + '.getLayout().setActiveItem(0);'));
  end;
end;

initialization
  TJSControllerRegistry.Instance.RegisterClass('AccordionPanel', TKExtAccordionPanelController);

finalization
  TJSControllerRegistry.Instance.UnregisterClass('AccordionPanel');

end.

