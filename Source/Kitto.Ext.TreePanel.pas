unit Kitto.Ext.TreePanel;

{$I Kitto.Defines.inc}

interface

uses
  ExtTree,
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
  Kitto.Ext.Session;

{ TKExtTreePanelController }

procedure TKExtTreePanelController.DoDisplay;
begin
  inherited;
  Title := View.DisplayLabel;
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
  FTreeViewRenderer.RenderAsTree(Session.Config.Views.ViewByNode(FView.GetNode('Controller/TreeView')) as TKTreeView,
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

