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
  Kitto.Ext.Controller, Kitto.Environment, Kitto.Ext.Session;

{ TKExtToolBarController }

destructor TKExtToolBarController.Destroy;
begin
  FreeAndNil(FTreeViewRenderer);
  inherited;
end;

procedure TKExtToolBarController.DisplayView;
begin
  Session.DisplayView(Session.Query['Name']);
end;

procedure TKExtToolBarController.DoDisplay;
begin
  inherited;
  if not Assigned(FTreeViewRenderer) then
    FTreeViewRenderer := TKExtTreeViewRenderer.Create;
  FTreeViewRenderer.RenderAsButtons(Environment.Views.ViewByNode(View.GetNode('Controller/TreeView')) as TKTreeView,
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
