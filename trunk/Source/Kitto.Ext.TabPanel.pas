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

unit Kitto.Ext.TabPanel;

{$I Kitto.Defines.inc}

interface

uses
  Ext,
  EF.Tree,
  Kitto.Ext.Base, Kitto.Ext.Controller, Kitto.Metadata.Views;

type
  TKExtTabPanelController = class;

  ///	<summary>
  ///	  A tab panel that knows when its hosted panels are closed. Used
  ///   by the TabPanel controller.
  ///	</summary>
  TKExtTabPanel = class(TExtTabPanel)
  private
    FConfig: TEFTree;
    FView: TKView;
    FOwner: TKExtTabPanelController;
  protected
    procedure InitDefaults; override;
  public
    procedure DisplaySubViewsAndControllers;
    destructor Destroy; override;
  published
    procedure PanelClosed;
  end;

  TKExtTabPanelController = class(TKExtPanelControllerBase)
  strict private
    FTabPanel: TKExtTabPanel;
  strict protected
    procedure InitDefaults; override;
    procedure DoDisplay; override;
  public
    procedure InitSubController(const AController: IKExtController); override;
  end;

implementation

uses
  SysUtils,
  ExtPascal,
  EF.Localization,
  Kitto.AccessControl, Kitto.Types,
  Kitto.Ext.Session;

{ TKExtTabPanelController }

procedure TKExtTabPanelController.DoDisplay;
begin
  inherited;
  FTabPanel.FConfig := Config;
  FTabPanel.FOwner := Self;
  FTabPanel.FView := View;
  FTabPanel.DisplaySubViewsAndControllers;
end;

procedure TKExtTabPanelController.InitDefaults;
begin
  inherited;
  Layout := lyFit;
  FTabPanel := TKExtTabPanel.AddTo(Items);
end;

procedure TKExtTabPanelController.InitSubController(
  const AController: IKExtController);
begin
  inherited;
end;

{ TKExtTabPanel }

procedure TKExtTabPanel.InitDefaults;
begin
  inherited;
  if Session.ViewHost = nil then
    Session.ViewHost := Self;
  Border := False;
  { TODO : remove this once all controllers set it by themselves. }
  Defaults := JSObject('autoscroll:true');
  EnableTabScroll := True;
  // Layout problems in tabbed views if DeferredRender=False.
  DeferredRender := True;
end;

destructor TKExtTabPanel.Destroy;
begin
  if Session.ViewHost = Self then
    Session.ViewHost := nil;
  inherited;
end;

procedure TKExtTabPanel.DisplaySubViewsAndControllers;
var
  LController: IKExtController;
  LViews: TEFNode;
  I: Integer;
  LView: TKView;
begin
  Assert(Assigned(FOwner));
  Assert(Assigned(FConfig));
  Assert(Assigned(FView));

  LViews := FConfig.FindNode('SubViews');
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
        LController := TKExtControllerFactory.Instance.CreateController(FView, Self, LViews.Children[I]);
        FOwner.InitSubController(LController);
        LController.Display;
      end
      else
        raise EKError.Create(_('TabPanel''s SubViews node may only contain View or Controller subnodes.'));
    end;
    if Items.Count > 0 then
      SetActiveTab(0);
  end;
end;

procedure TKExtTabPanel.PanelClosed;
var
  LPanel: TExtObject;
begin
  LPanel := ParamAsObject('Panel') as TExtObject;
  Items.Remove(LPanel);
  LPanel.Free;
end;

initialization
  TKExtControllerRegistry.Instance.RegisterClass('TabPanel', TKExtTabPanelController);

finalization
  TKExtControllerRegistry.Instance.UnregisterClass('TabPanel');

end.
