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
  Kitto.Ext.Base, Kitto.Metadata.Views;

type
  ///	<summary>
  ///	  A tab panel that knows when its hosted panels are closed. Used
  ///   by the TabPanel controller.
  ///	</summary>
  TKExtTabPanel = class(TExtTabPanel)
  private
    FView: TKView;
    procedure DisplaySubViews;
    procedure SetView(const AValue: TKView);
  protected
    procedure InitDefaults; override;
  public
    procedure AfterConstruction; override;
    property View: TKView read FView write SetView;
  published
    procedure PanelClosed;
  end;

  TKExtTabPanelController = class(TKExtPanelControllerBase)
  private
    FTabPanel: TKExtTabPanel;
  protected
    procedure InitDefaults; override;
    procedure DoDisplay; override;
  end;

implementation

uses
  SysUtils,
  ExtPascal,
  EF.Tree,
  Kitto.AccessControl,
  Kitto.Ext.Controller, Kitto.Ext.Session;

{ TKExtTabPanelController }

procedure TKExtTabPanelController.DoDisplay;
begin
  inherited;
  FTabPanel.View := View;
end;

procedure TKExtTabPanelController.InitDefaults;
begin
  inherited;
  Layout := lyFit;

  FTabPanel := TKExtTabPanel.AddTo(Items);
end;

{ TKExtTabPanel }

procedure TKExtTabPanel.InitDefaults;
begin
  inherited;
  Border := False;
  { TODO : remove this once all controllers set it by themselves. }
  Defaults := JSObject('autoscroll:true');
  EnableTabScroll := True;
  // Layout problems in tabbed views if DeferredRender=False.
  DeferredRender := True;
end;

procedure TKExtTabPanel.AfterConstruction;
begin
  inherited;
  Session.ViewHost := Self;
end;

procedure TKExtTabPanel.DisplaySubViews;
var
  LController: IKExtController;
  LViews: TEFNode;
  I: Integer;
  LView: TKView;
begin
  Assert(Assigned(FView));

  LViews := View.FindNode('Controller/SubViews');
  if Assigned(LViews) then
  begin
    for I := 0 to LViews.ChildCount - 1 do
    begin
      LView := Session.Config.Views.ViewByNode(LViews.Children[I]);
      if LView.IsAccessGranted(ACM_VIEW) then
      begin
        LController := TKExtControllerFactory.Instance.CreateController(LView, Self);
        LController.Display;
      end;
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

procedure TKExtTabPanel.SetView(const AValue: TKView);
begin
  Assert(Assigned(AValue));

  FView := AValue;
  DisplaySubViews;
end;

initialization
  TKExtControllerRegistry.Instance.RegisterClass('TabPanel', TKExtTabPanelController);

finalization
  TKExtControllerRegistry.Instance.UnregisterClass('TabPanel');

end.
