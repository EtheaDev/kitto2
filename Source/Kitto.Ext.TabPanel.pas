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
  Ext.Base
  , EF.Tree
  , Kitto.Metadata.Views
  , Kitto.JS
  , Kitto.Ext.Base
  , Kitto.Ext.Controller
  ;

type
  TKExtTabPanelController = class;

  /// <summary>
  ///  A tab panel that knows when its hosted panels are closed. Used
  ///  by the TabPanel controller.
  /// </summary>
  TKExtTabPanel = class(TExtTabPanel, IKExtPanelHost, IJSControllerContainer)
  private
    FConfig: TEFTree;
    FView: TKView;
    FOwner: TKExtTabPanelController;
  strict protected
    property Config: TEFTree read FConfig;
    property View: TKView read FView;
    procedure InitDefaults; override;
    function TabsVisible: Boolean; virtual;
    procedure ApplyTabSize;
    function GetDefaultTabSize: string; virtual;
    procedure TabChange(ATabPanel: TExtTabPanel; ANewTab, AOldTab: TExtComponent); virtual;
    procedure InitSubController(const ASubController: IJSController);
    procedure SetActiveSubController(const ASubController: IJSController);
  public
    procedure SetAsControllerContainer; virtual;
    function AsExtContainer: TExtContainer;
    procedure DisplaySubViewsAndControllers; virtual;
    destructor Destroy; override;
    procedure ClosePanel(const APanel: TExtComponent);
    function AsObject: TObject;
  //published
    procedure PanelClosed;
  end;
  TKExtTabPanelClass = class of TKExtTabPanel;

  TKExtTabPanelController = class(TKExtPanelControllerBase)
  private
    FTabPanel: TKExtTabPanel;
  strict protected
    procedure InitDefaults; override;
    procedure DoDisplay; override;
    function GetTabPanelClass: TKExtTabPanelClass; virtual;
    function GetDefaultTabIconsVisible: Boolean; virtual;
  protected
    function TabIconsVisible: Boolean;
    procedure InitSubController(const ASubController: IJSController); override;
    procedure SetActiveSubController(const ASubController: IJSController); override;
  end;

implementation

uses
  SysUtils
  , EF.Localization
  , Kitto.AccessControl
  , Kitto.Types
  , Kitto.Web.Application
  , Kitto.Web.Session
  ;

{ TKExtTabPanelController }

procedure TKExtTabPanelController.DoDisplay;
begin
  inherited;
  FTabPanel.FConfig := Config;
  FTabPanel.FOwner := Self;
  FTabPanel.FView := View;
  FTabPanel.SetAsControllerContainer;
  FTabPanel.DisplaySubViewsAndControllers;
end;

function TKExtTabPanelController.GetDefaultTabIconsVisible: Boolean;
begin
  Result := True;
end;

function TKExtTabPanelController.GetTabPanelClass: TKExtTabPanelClass;
begin
  Result := TKExtTabPanel;
end;

procedure TKExtTabPanelController.InitDefaults;
begin
  inherited;
  Layout := lyFit;
  FTabPanel := GetTabPanelClass.CreateAndAddToArray(Items);
end;

procedure TKExtTabPanelController.InitSubController(const ASubController: IJSController);
begin
  inherited;
  ASubController.Config.SetBoolean('Sys/ShowIcon', TabIconsVisible);
end;

procedure TKExtTabPanelController.SetActiveSubController(const ASubController: IJSController);
begin

end;

function TKExtTabPanelController.TabIconsVisible: Boolean;
begin
  Result := Config.GetBoolean('TabIconsVisible', GetDefaultTabIconsVisible);
end;

{ TKExtTabPanel }

procedure TKExtTabPanel.InitDefaults;
begin
  inherited;
  Border := False;
  { TODO : remove this once all controllers set it by themselves. }
  Defaults.SetConfigItem('autoscroll', True);
  // Layout problems in tabbed views if DeferredRender=False.
  DeferredRender := True;
end;

procedure TKExtTabPanel.InitSubController(const ASubController: IJSController);
begin
  inherited;
end;

procedure TKExtTabPanel.SetAsControllerContainer;
begin
  Assert(Assigned(Config));
  Assert(Session <> nil);

  if Config.GetBoolean('IsControllerContainer', True) then
    if (Session.ControllerContainer = nil) then
      Session.ControllerContainer := Self;
end;

function TKExtTabPanel.AsExtContainer: TExtContainer;
begin
  Result := Self;
end;

function TKExtTabPanel.AsObject: TObject;
begin
  Result := Self;
end;

procedure TKExtTabPanel.ClosePanel(const APanel: TExtComponent);
begin
  Remove(APanel, True);
  Items.Remove(APanel);
  APanel.Free;
end;

destructor TKExtTabPanel.Destroy;
begin
  if (Session <> nil) and Assigned(Session.ControllerContainer) and (Session.ControllerContainer.AsJSObject = Self) then
    Session.ControllerContainer := nil;
  inherited;
end;

procedure TKExtTabPanel.DisplaySubViewsAndControllers;
var
  LController: IJSController;
  LViews: TEFNode;
  I: Integer;
  LView: TKView;
begin
  Assert(Assigned(FOwner));
  Assert(Assigned(Config));
  Assert(Assigned(FView));

  if TabsVisible then
  begin
    ApplyTabSize;
    EnableTabScroll := True;
  end
  else
    AddCls('tab-strip-hidden');

  LViews := Config.FindNode('SubViews');
  if Assigned(LViews) then
  begin
    OnTabChange := TabChange;
    for I := 0 to LViews.ChildCount - 1 do
    begin
      if SameText(LViews.Children[I].Name, 'View') then
      begin
        LView := TKWebApplication.Current.Config.Views.ViewByNode(LViews.Children[I]);
        if LView.IsAccessGranted(ACM_VIEW) then
        begin
          LController := TKExtControllerFactory.Instance.CreateController(Self, LView, Self);
          FOwner.InitSubController(LController);
          LController.Display;
        end;
      end
      else if SameText(LViews.Children[I].Name, 'Controller') then
      begin
        LController := TKExtControllerFactory.Instance.CreateController(Self, FView, Self, LViews.Children[I]);
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
  LPanel: TExtComponent;
begin
  LPanel := ParamAsObject('Panel') as TExtComponent;
  Items.Remove(LPanel);
  LPanel.Free;
end;

procedure TKExtTabPanel.SetActiveSubController(const ASubController: IJSController);
begin
  SetActiveTab(ASubController.AsObject as TExtComponent);
end;

procedure TKExtTabPanel.ApplyTabSize;
begin
  AddCls('tab-strip-' + Config.GetString('TabSize', GetDefaultTabSize));
end;

function TKExtTabPanel.GetDefaultTabSize: string;
begin
  Result := 'normal';
end;

procedure TKExtTabPanel.TabChange(ATabPanel: TExtTabPanel; ANewTab, AOldTab: TExtComponent);
var
  LIntf: IKExtActivable;
begin
  if Assigned(ANewTab) and Supports(ANewTab, IKExtActivable, LIntf) then
    LIntf.Activate;
end;

function TKExtTabPanel.TabsVisible: Boolean;
begin
  Result := Config.GetBoolean('TabsVisible', True);
end;

initialization
  TKExtControllerRegistry.Instance.RegisterClass('TabPanel', TKExtTabPanelController);

finalization
  TKExtControllerRegistry.Instance.UnregisterClass('TabPanel');

end.
