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

unit Kitto.Ext.ToolBar;

{$I Kitto.Defines.inc}

interface

uses
  Ext, ExtUx,
  Kitto.Ext.Base, Kitto.Metadata.Views, Kitto.Ext.Utils;

type
  TKExtToolBarController = class(TKExtPanelControllerBase)
  private
    FToolBar: TExtToolbar;
    FTreeViewRenderer: TKExtTreeViewRenderer;
  protected
    procedure InitDefaults; override;
    procedure DoDisplay; override;
  public
    destructor Destroy; override;
  published
    procedure DisplayView;
  end;

implementation

uses
  SysUtils,
  ExtPascal,
  EF.Tree,
  Kitto.Ext.Controller, Kitto.Ext.Session;

{ TKExtToolBarController }

destructor TKExtToolBarController.Destroy;
begin
  FreeAndNil(FTreeViewRenderer);
  inherited;
end;

procedure TKExtToolBarController.DisplayView;
begin
  Session.DisplayView(TKView(Session.QueryAsInteger['View']));
end;

procedure TKExtToolBarController.DoDisplay;
begin
  inherited;
  if not Assigned(FTreeViewRenderer) then
    FTreeViewRenderer := TKExtTreeViewRenderer.Create;
  FTreeViewRenderer.RenderAsButtons(Session.Config.Views.ViewByNode(Config.GetNode('TreeView')) as TKTreeView,
    FToolBar, Self, DisplayView);
end;

procedure TKExtToolBarController.InitDefaults;
begin
  inherited;
  Layout := lyFit;
  Height := 28;

  FToolBar := TExtToolbar.AddTo(Items);
end;

initialization
  TKExtControllerRegistry.Instance.RegisterClass('ToolBar', TKExtToolBarController);

finalization
  TKExtControllerRegistry.Instance.UnregisterClass('ToolBar');

end.
