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

unit Kitto.Ext.TabPanel;

{$I Kitto.Defines.inc}

interface

uses
  EF.Tree
  , Kitto.Metadata.Views
  , Kitto.JS
  , Kitto.JS.Controller
  , Ext.Base
  , Kitto.Ext.Base
  , Kitto.Ext.Panel
  ;

type
  TKExtTabPanelController = class;

  /// <summary>
  ///  A tab panel that knows when its hosted panels are closed. Used
  ///  by the TabPanel controller.
  /// </summary>
  TKExtTabPanel = class(TExtTabPanel)
  private
    FView: TKView;
  strict protected
    procedure ItemAdded(const AItems: TJSObjectArray; const AItem: TJSObject); override;
    property View: TKView read FView;
    procedure InitDefaults; override;
    function TabsVisible: Boolean; virtual;
    procedure TabChange(ATabPanel: TExtTabPanel; ANewTab: TExtComponent);
  public
    procedure SetAsControllerContainer; virtual;
    function AsExtContainer: TExtContainer;
    procedure DisplaySubViewsAsTabs; virtual;
    destructor Destroy; override;
    function AsObject: TObject;
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
    function GetObjectNamePrefix: string; override;
  protected
    function TabIconsVisible: Boolean;
    procedure InitSubController(const ASubController: IJSController); override;
  end;

implementation

uses
  SysUtils
  , EF.Localization
  , Kitto.AccessControl
  , Kitto.Types
  , Kitto.Web.Application
  , Kitto.Web.Response
  , Kitto.Web.Session
  ;

{ TKExtTabPanelController }

procedure TKExtTabPanelController.DoDisplay;
begin
  inherited;
  Layout := 'fit';
  FTabPanel.Config.Assign(Config);
  FTabPanel.FView := View;
  FTabPanel.SetAsControllerContainer;
  FTabPanel.DisplaySubViewsAsTabs;
  if FTabPanel.Items.Count > 0 then
    FTabPanel.SetActiveTab(0);
end;

function TKExtTabPanelController.GetDefaultTabIconsVisible: Boolean;
begin
  Result := True;
end;

function TKExtTabPanelController.GetObjectNamePrefix: string;
begin
  Result := 'tab';
end;

function TKExtTabPanelController.GetTabPanelClass: TKExtTabPanelClass;
begin
  Result := TKExtTabPanel;
end;

procedure TKExtTabPanelController.InitDefaults;
begin
  inherited;
  FTabPanel := GetTabPanelClass.CreateAndAddToArray(Items);
end;

procedure TKExtTabPanelController.InitSubController(const ASubController: IJSController);
begin
  inherited;
  ASubController.Config.SetBoolean('Sys/ShowIcon', TabIconsVisible);
end;

function TKExtTabPanelController.TabIconsVisible: Boolean;
begin
  Result := Config.GetBoolean('TabIconsVisible', GetDefaultTabIconsVisible);
end;

{ TKExtTabPanel }

procedure TKExtTabPanel.InitDefaults;
begin
  inherited;
  OnTabChange := TabChange;
end;

procedure TKExtTabPanel.ItemAdded(const AItems: TJSObjectArray; const AItem: TJSObject);
begin
  inherited;
  Assert(Assigned(AItem));
  Assert(AItems = Items);
  Assert(AItem is TExtComponent);

  // Don't use the index here, or dependencies will not be correctly determined
  // and the setActiveTab() call might end up being emitted before the add() call.
  SetActiveTab(TExtComponent(AItem));
end;

procedure TKExtTabPanel.SetAsControllerContainer;
begin
  Assert(Assigned(Config));
  Assert(TKWebSession.Current <> nil);

  if Config.GetBoolean('IsControllerContainer', True) then
    if (TKWebSession.Current.ControllerContainer = nil) then
      TKWebSession.Current.ControllerContainer := Self;
end;

function TKExtTabPanel.AsExtContainer: TExtContainer;
begin
  Result := Self;
end;

function TKExtTabPanel.AsObject: TObject;
begin
  Result := Self;
end;

destructor TKExtTabPanel.Destroy;
begin
  if (TKWebSession.Current <> nil) and Assigned(TKWebSession.Current.ControllerContainer) and (TKWebSession.Current.ControllerContainer.AsJSObject = Self) then
    TKWebSession.Current.ControllerContainer := nil;
  inherited;
end;

procedure TKExtTabPanel.DisplaySubViewsAsTabs;
begin
  TJSControllerUtils.DisplaySubControllers(Config, View, TKWebApplication.Current.Config.Views, Self, Self);
end;

procedure TKExtTabPanel.TabChange(ATabPanel: TExtTabPanel; ANewTab: TExtComponent);
var
  LActivable: IJSActivable;
begin
  if Assigned(ANewTab) and Supports(ANewTab, IJSActivable, LActivable) then
    LActivable.Activate;
end;

function TKExtTabPanel.TabsVisible: Boolean;
begin
  Result := Config.GetBoolean('TabsVisible', True);
end;

initialization
  TJSControllerRegistry.Instance.RegisterClass('TabPanel', TKExtTabPanelController);

finalization
  TJSControllerRegistry.Instance.UnregisterClass('TabPanel');

end.
