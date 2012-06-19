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

unit Kitto.Ext.TreePanel;

{$I Kitto.Defines.inc}

interface

uses
  ExtTree,
  EF.Tree,
  Kitto.Ext.Base, Kitto.Ext.Controller, Kitto.Metadata.Views, Kitto.Ext.Utils;

type
  ///	<summary>
  ///	  A tree panel that can display a tree view with clickable nodes. Used
  ///   by the TreePanel controller.
  ///	</summary>
  TKExtTreePanel = class(TExtTreeTreePanel)
  private
    FView: TKView;
    FTreeViewRenderer: TKExtTreeViewRenderer;
    FConfig: TEFNode;
    procedure SetView(const AValue: TKView);
  protected
    procedure InitDefaults; override;
  public
    destructor Destroy; override;
    property View: TKView read FView write SetView;
  published
    procedure DisplayView;
  end;

  TKExtTreePanelController = class(TKExtPanelControllerBase)
  private
    FTreePanel: TKExtTreePanel;
  protected
    procedure DoDisplay; override;
    procedure InitDefaults; override;
  end;

implementation

uses
  SysUtils,
  Ext,
  EF.Localization,
  Kitto.Ext.Session;

{ TKExtTreePanelController }

procedure TKExtTreePanelController.DoDisplay;
begin
  inherited;
  Title := _(View.DisplayLabel);
  FTreePanel.FConfig := Config;
  FTreePanel.View := View;
end;

procedure TKExtTreePanelController.InitDefaults;
begin
  inherited;
  Layout := lyFit;

  FTreePanel := TKExtTreePanel.AddTo(Items);
end;

{ TKExtTreePanel }

destructor TKExtTreePanel.Destroy;
begin
  FreeAndNil(FTreeViewRenderer);
  inherited;
end;

procedure TKExtTreePanel.InitDefaults;
begin
  inherited;
  Root := TExtTreeTreeNode.Create;
  RootVisible := False;
  AutoScroll := True;
  Border := False;
end;

procedure TKExtTreePanel.SetView(const AValue: TKView);
begin
  Assert(Assigned(AValue));

  FView := AValue;
  if not Assigned(FTreeViewRenderer) then
    FTreeViewRenderer := TKExtTreeViewRenderer.Create;
  FTreeViewRenderer.RenderAsTree(Session.Config.Views.ViewByNode(FConfig.GetNode('TreeView')) as TKTreeView,
    Root, Self, DisplayView);
end;

procedure TKExtTreePanel.DisplayView;
begin
  Session.DisplayView(TKView(Session.QueryAsInteger['View']));
end;

initialization
  TKExtControllerRegistry.Instance.RegisterClass('TreePanel', TKExtTreePanelController);

finalization
  TKExtControllerRegistry.Instance.UnregisterClass('TreePanel');

end.

