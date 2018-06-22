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

unit Kitto.Ext.TreePanel;

{$I Kitto.Defines.inc}

interface

uses
  EF.Tree
  , Kitto.Metadata.Views
  , Kitto.JS.Controller
  , Ext.Tree
  , Kitto.Ext.Panel
  , Kitto.Ext.Utils
  ;

type
  TKExtTreeTreeNode = class(TExtTreeTreeNode)
  private
    FView: TKView;
    procedure SetView(const AValue: TKView);
  public
    property View: TKView read FView write SetView;
  end;

  ///	<summary>
  ///	 A tree panel that can display a tree view with clickable nodes. Used
  ///  by the TreePanel controller.
  ///	</summary>
  TKExtTreePanel = class(TExtTreeTreePanel)
  private
    FView: TKView;
    FTreeViewRenderer: TKExtTreeViewRenderer;
    FTreeView: TKTreeView;
    procedure SetView(const AValue: TKView);
    procedure AddNode(const ANode: TKTreeViewNode; const ADisplayLabel: string;
      const AParent: TExtTreeTreeNode);
  protected
    procedure InitDefaults; override;
  public
    destructor Destroy; override;
    property View: TKView read FView write SetView;
  //published
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
  SysUtils
  , NetEncoding
  , Ext.Base
  , EF.Localization
  , Kitto.Config
  , Kitto.Auth
  , Kitto.AccessControl
  , Kitto.Web.Application
  , Kitto.Web.Response
  , Kitto.JS
  ;

{ TKExtTreeTreeNode }

procedure TKExtTreeTreeNode.SetView(const AValue: TKView);
begin
  FView := AValue;
  if Assigned(FView) then
  begin
    Expandable := False;
    Expanded := False;
    Leaf := True;
    SetConfigItem('viewId', IntToStr(Integer(FView)));
  end;
end;

{ TKExtTreePanelController }

procedure TKExtTreePanelController.DoDisplay;
begin
  inherited;
  Title := _(View.DisplayLabel);
  FTreePanel.Config.Assign(Config);
  FTreePanel.View := View;
end;

procedure TKExtTreePanelController.InitDefaults;
begin
  inherited;
  Layout := lyFit;

  FTreePanel := TKExtTreePanel.CreateAndAddToArray(Items);
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
  Root := TExtTreeTreeNode.CreateInline(Self);
  RootVisible := False;
  AutoScroll := True;
  Border := False;
end;

procedure TKExtTreePanel.SetView(const AValue: TKView);
var
  LViewNode: TEFNode;
begin
  Assert(Assigned(AValue));

  FView := AValue;
  if not Assigned(FTreeViewRenderer) then
    FTreeViewRenderer := TKExtTreeViewRenderer.Create;
  LViewNode := Config.GetNode('TreeView');
  FTreeView := TKWebApplication.Current.Config.Views.ViewByNode(LViewNode) as TKTreeView;
  Assert(Assigned(FTreeView));
  FTreeViewRenderer.Render(FTreeView,
    procedure (ANode: TKTreeViewNode; ADisplayLabel: string)
    begin
      AddNode(ANode, ADisplayLabel, Root);
    end,
    { TODO : remove these parameters }
    Self, nil);
  &On('itemclick', TKWebResponse.Current.Items.AjaxCallMethod(Self).SetMethod(DisplayView)
    .AddRawParam('View', 'record.data.viewId')
    .FunctionArgs('view, record, item, index')
    .AsFunction);
end;

procedure TKExtTreePanel.DisplayView;
var
  LViewId: string;
begin
  LViewId := ParamAsString('View');
  if (LViewId <> '') and (LViewId <> 'undefined') then
    TKWebApplication.Current.DisplayView(TKView(StrToInt(LViewId)));
end;

procedure TKExtTreePanel.AddNode(const ANode: TKTreeViewNode;
  const ADisplayLabel: string; const AParent: TExtTreeTreeNode);
var
  LExtNode: TKExtTreeTreeNode;
  I: Integer;
  LIsEnabled: Boolean;
  LView: TKView;
  LSubNode: TKTreeViewNode;
  LDisplayLabel: string;
  LOriginalNode: TKTreeViewNode;
begin
  Assert(Assigned(ANode));
  Assert(Assigned(AParent));

  LOriginalNode := TKTreeViewNode(ANode.GetObject('Sys/SourceNode'));
  if LOriginalNode = nil then
    LOriginalNode := ANode;

  LView := LOriginalNode.FindView(TKWebApplication.Current.Config.Views);
  if Assigned(LView) then
    LIsEnabled := LView.IsAccessGranted(ACM_RUN)
  else
    LIsEnabled := TKAccessController.Current.IsAccessGranted(TKAuthenticator.Current.UserName, LOriginalNode.GetACURI(FTreeView), ACM_RUN);
  LExtNode := TKExtTreeTreeNode.CreateInlineAndAddToArray(AParent.Children);
  try
    LExtNode.View := LView;
    if Assigned(LExtNode.View) then
    begin
      LExtNode.IconCls := TKWebApplication.Current.SetViewIconStyle(LExtNode.View, GetTreeViewNodeImageName(LOriginalNode, LExtNode.View));
      LExtNode.Disabled := not LIsEnabled;
    end;
    LExtNode.Text := TNetEncoding.HTML.Encode(ADisplayLabel);
    if TKWebApplication.Current.TooltipsEnabled then
      LExtNode.Qtip := LExtNode.Text;
    if LOriginalNode.TreeViewNodeCount > 0 then
    begin
      for I := 0 to LOriginalNode.TreeViewNodeCount - 1 do
      begin
        LSubNode := LOriginalNode.TreeViewNodes[I];
        LDisplayLabel := _(LSubNode.GetString('DisplayLabel', GetDisplayLabelFromNode(LSubNode, TKWebApplication.Current.Config.Views)));
        AddNode(LSubNode, LDisplayLabel, LExtNode);
      end;
      LExtNode.Expandable := True;
      if LOriginalNode is TKTreeViewFolder then
        LExtNode.Expanded := not TKTreeViewFolder(LOriginalNode).IsInitiallyCollapsed
      else
        LExtNode.Expanded := True;
      LExtNode.Leaf := False;
    end;
  except
    FreeAndNil(LExtNode);
    raise;
  end;
end;

initialization
  TJSControllerRegistry.Instance.RegisterClass('TreePanel', TKExtTreePanelController);

finalization
  TJSControllerRegistry.Instance.UnregisterClass('TreePanel');

end.

