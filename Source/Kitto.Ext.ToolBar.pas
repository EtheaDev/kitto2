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

unit Kitto.Ext.ToolBar;

{$I Kitto.Defines.inc}

interface

uses
  Kitto.Metadata.Views
  , Ext.Base
  , Ext.Ux
  , Kitto.Ext.Base
  , Kitto.Ext.Panel
  , Kitto.Ext.Utils
  ;

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
  //published
    procedure DisplayView;
  end;

implementation

uses
  SysUtils
  , EF.Tree
  , Kitto.Web.Application
  , Kitto.JS.Controller
  ;

{ TKExtToolBarController }

destructor TKExtToolBarController.Destroy;
begin
  FreeAndNil(FTreeViewRenderer);
  inherited;
end;

procedure TKExtToolBarController.DisplayView;
begin
  TKWebApplication.Current.DisplayView(TKView(ParamAsInteger('View')));
end;

procedure TKExtToolBarController.DoDisplay;
var
  LTreeView: TKTreeView;
  LNode: TEFNode;
begin
  inherited;
  if not Assigned(FTreeViewRenderer) then
    FTreeViewRenderer := TKExtTreeViewRenderer.Create;
  LNode := Config.GetNode('TreeView');
  LTreeView := TKWebApplication.Current.Config.Views.ViewByNode(LNode) as TKTreeView;
  FTreeViewRenderer.RenderAsButtons(LTreeView, FToolBar, Self, DisplayView);
end;

procedure TKExtToolBarController.InitDefaults;
begin
  inherited;
  Layout := 'fit';
//  Height := 28;

  FToolBar := TExtToolbar.CreateAndAddToArray(Items);
end;

initialization
  TJSControllerRegistry.Instance.RegisterClass('ToolBar', TKExtToolBarController);

finalization
  TJSControllerRegistry.Instance.UnregisterClass('ToolBar');

end.
