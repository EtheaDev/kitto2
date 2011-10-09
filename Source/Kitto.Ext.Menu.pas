unit Kitto.Ext.Menu;

interface

uses
  Ext, ExtUx, ExtTree,
  EF.ObserverIntf,
  Kitto.Metadata.Views, Kitto.Ext.Base, Kitto.Ext.Utils;

type
  ///	<summary>
  ///	  A tab panel that knows when its hosted panels are closed.
  ///	</summary>
  TKExtTabPanel = class(TExtTabPanel)
  published
    procedure PanelClosed;
  end;

  ///	<summary>
  ///	  A menu panel, with toolbar, status bar and navigation tree. Hosts views
  ///	  as tab pages.
  ///	</summary>
  TKExtMenuPanel = class(TKExtPanelControllerBase)
  private
    FToolbar: TExtToolbar;
    FTabPanel: TKExtTabPanel;
    FStatusBar: TExtUxStatusBar;
    FNavigationPanel: TExtPanel;
    FMenuRenderer: TKExtTreeViewRenderer;
    FMenuPanel: TExtPanel;
    FTreePanel: TExtTreeTreePanel;
    FWelcomePanel: TExtPanel;
    procedure CreateWelcomeFrames;
    procedure DisplayAutoViews;
  protected
    procedure DoDisplay; override;
  public
    destructor Destroy; override;
    procedure UpdateObserver(const ASubject: IEFSubject; const AContext: string = ''); override;
  published
    ///	<summary>
    ///	  Displays a view. Query params:<br />Name: Identifies the view to
    ///	  display.
    ///	</summary>
    procedure DisplayView;

    ///	<summary>
    ///   Displays memory status info. No query params.
    ///	</summary>
    procedure ShowMemoryStatus;
  end;

  TKExtMenuWindow = class(TKExtWindowControllerBase)
  private
    FMainMenuPanel: TKExtMenuPanel;
  protected
    procedure DoDisplay; override;
  end;

  TKExtMenuViewport = class(TKExtViewportControllerBase)
  private
    FMainMenuPanel: TKExtMenuPanel;
  protected
    procedure DoDisplay; override;
  end;

implementation

uses
  SysUtils, Types, Classes,
  ExtPascal, ExtPascalUtils,
  EF.StrUtils,
  Kitto.Ext.Session, Kitto.Environment, Kitto.Ext.Controller;

{ TKExtMenuPanel }

destructor TKExtMenuPanel.Destroy;
begin
  FreeAndNil(FMenuRenderer);
  inherited;
end;

procedure TKExtMenuPanel.DisplayView;
var
  LController: IKExtController;
begin
  LController := TKControllerFactory.Instance.CreateController(
    Environment.Views.ViewByName(Session.Query['Name']), FTabPanel, Self);
  LController.Display;
  if Session.QueryAsBoolean['AutoCollapseMenu'] then
    FMenuPanel.Collapse(True);
  FTabPanel.SetActiveTab(FTabPanel.Items.Count - 1);
end;

procedure TKExtMenuPanel.DoDisplay;
var
  LMenu: TKTreeView;
  LMenuName: string;
  LTopToolbarName: string;
begin
  inherited;
  Header := False;

  FMenuRenderer := TKExtTreeViewRenderer.Create(Self);

  // Top toolbar.
  Layout := lyBorder;
  FToolbar := TExtToolbar.Create;
  FToolbar.Region := rgNorth;
  FToolbar.Height := 28;
  LTopToolbarName := View.GetExpandedString('Controller/TopToolbarView');
  if LTopToolbarName <> '' then
  begin
    if FMenuRenderer.RenderAsButtons(Environment.Views.ViewByName(LTopToolbarName) as TKTreeView, FToolbar) = 0 then
    begin
      FToolbar.Free(True);
      FToolbar := nil;
    end
    else
      Items.Add(FToolbar);
  end;

  // Bottom status bar.
  FStatusBar := TExtUxStatusBar.AddTo(Items);
  FStatusBar.Region := rgSouth;
  FStatusBar.Height := 28;
  if not Environment.AuthenticationHost.IsNullAuthenticator then
  begin
    FStatusBar.DefaultText := 'No user connected.';
    FStatusBar.Text := Format('<p>User: %s</p>', [Environment.AuthenticationHost.CurrentAuthenticator.UserId]);
    FStatusBar.IconCls := 'x-status-valid';
//    with TExtButton.AddTo(FStatusBar.Items) do begin
//      Text := 'Change password';
//      { TODO : use a built-in GUI element or GUIHandler? }
//      Handler := Self.Ajax('ExecuteGUIElement', ['Id', 'change_password']);
//    end;
  end;
  // Other statusbar buttons here.
//  with TExtButton.AddTo(FStatusBar.Items) do begin
//    Text := 'Memory Status';
//    Handler := Ajax(ShowMemoryStatus, []);
//  end;

  // No right region for now.
  (*
  with TExtPanel.AddTo(Items) do begin
    Region  := rgEast;
    Split   := true;
    Height  := 100;
    Title   := 'East Side';
    Margins := SetMargins(0, 5);
    Width   := 225;
    Layout  := lyFit;
    MinSize := 175;
    MaxSize := 400;
    Collapsible := true;
    with TExtTabPanel.AddTo(Items) do begin
      Border := false;
      TabPosition := 'bottom';
      ActiveTabNumber := 1;
      with TExtPanel.AddTo(Items) do begin
        Html  := '<p>A TabPanel component can be a region.</p>';
        Title := 'A Tab';
        AutoScroll := true;
      end;
      with TExtGridPropertyGrid.AddTo(Items) do begin
        Title    := 'Property Grid';
        Closable := true;
        Source   := JSObject('"(name)":"Property Grid",grouping:false,autoFitColumns:true,productionQuality:false,' +
          'created:new Date(Date.parse("10/15/2006")),tested:false,version:.01,borderWidth:1');
      end;
    end;
  end;
  *)

  LMenuName := View.GetExpandedString('Controller/MenuView');
  if LMenuName <> '' then
  begin
    LMenu := Environment.Views.ViewByName(LMenuName) as TKTreeView;
    if LMenu.ViewRefs.Count > 0 then
    begin
      // Left: navigation/menu.
      FNavigationPanel := TExtPanel.AddTo(Items);
      FNavigationPanel.Region := rgWest;
      FNavigationPanel.Id := 'NavigationPanel';
      FNavigationPanel.Split := True;
      FNavigationPanel.Width := 180;
      FNavigationPanel.Title := 'Navigation';
      FNavigationPanel.Layout := lyAccordion;
      FNavigationPanel.LayoutConfig := JSObject('animate:true');
      FNavigationPanel.MinSize := 20;
      FNavigationPanel.MaxSize := 400;
      FNavigationPanel.Collapsible  := true;
      FNavigationPanel.Border := True;

      FMenuPanel := TExtPanel.AddTo(FNavigationPanel.Items);
      FMenuPanel.Layout := lyFit;
      FMenuPanel.Title := 'Main Menu';

      FTreePanel := TExtTreeTreePanel.AddTo(FMenuPanel.Items);
      FTreePanel.Border := False;
      FTreePanel.Root := TExtTreeTreeNode.Create;
      FTreePanel.RootVisible := False;
      FMenuRenderer.RenderAsTree(Environment.Views.ViewByName(View.GetString('Controller/MenuView')) as TKTreeView, FTreePanel.Root);
      FTreePanel.AutoScroll := True;

//      with TExtPanel.AddTo(FNavigationPanel.Items) do begin
//        Title   := 'Additional Menu';
//        Html    := '<p>Another tree of elements.</p>';
//        Border  := False;
//      end;
//      with TExtPanel.AddTo(FNavigationPanel.Items) do begin
//        Title   := 'Settings';
//        Html    := '<p>Change password, etc.</p>';
//        Border  := false;
//      end;
    end;
  end;

  // Center: displays view is tabs.
  FTabPanel := TKExtTabPanel.AddTo(Items);
  FTabPanel.Region := rgCenter;
  FTabPanel.Border := False;
  FTabPanel.Defaults := JSObject('autoscroll:true');
  FTabPanel.EnableTabScroll := True;
  FTabPanel.DeferredRender := True;

  CreateWelcomeFrames;
  DisplayAutoViews;

  if FTabPanel.Items.Count > 0 then
    FTabPanel.SetActiveTab(0);
end;

procedure TKExtMenuPanel.CreateWelcomeFrames;
var
  LWelcomeFileName: string;
begin
  LWelcomeFileName := Environment.FindResourcePathName('Welcome.html');
  if LWelcomeFileName <> '' then
  begin
    FWelcomePanel := TExtPanel.AddTo(FTabPanel.Items);
    FWelcomePanel.Title := 'Welcome';
    FWelcomePanel.Html := Environment.MacroExpansionEngine.Expand(TextFileToString(LWelcomeFileName));
    FWelcomePanel.Closable := True;
    FWelcomePanel.AutoScroll := True
  end
  else
    FWelcomePanel := nil;
end;

procedure TKExtMenuPanel.DisplayAutoViews;
var
  LViews: TStringDynArray;
  LViewName: string;
  LController: IKExtController;
begin
  { TODO : allow to specify initial views by access role as well }
  LViews := View.GetStringArray('Controller/AutoViews');
  for LViewName in LViews do
  begin
    LController := TKControllerFactory.Instance.CreateController(
      Environment.Views.ViewByName(LViewName), FTabPanel, Self);
    LController.Display;
  end;
end;

procedure TKExtMenuPanel.ShowMemoryStatus;
begin
  ExtMessageBox.Alert(Title, Format('Objects in GC: %d', [Session.GetGCObjectCount]))
end;

procedure TKExtMenuPanel.UpdateObserver(const ASubject: IEFSubject;
  const AContext: string);
begin
  inherited;
//  if (ASubject.AsObject is TKExtGUIHandler) and (AContext = 'ProvideGUIFinished') then
//  begin
//    ASubject.DetachObserver(Self);
//    ASubject.AsObject.Free;
//  end;
end;

{ TKExtHostPanel }

procedure TKExtTabPanel.PanelClosed;
var
  LPanel: TExtObject;
begin
  LPanel := ParamAsObject('Panel') as TExtObject;
  Items.Remove(LPanel);
  LPanel.Free;
end;

{ TKExtMenuWindow }

procedure TKExtMenuWindow.DoDisplay;
begin
  Title := Environment.Config.GetString('AppTitle');
  Layout := lyFit;
  Width := Environment.Config.GetInteger('Controller/WindowWidth', 800);
  Height := Environment.Config.GetInteger('Controller/WindowHeight', 600);
  Constrain := True;
  Closable := False;
  FMainMenuPanel := TKExtMenuPanel.AddTo(Items);
  FMainMenuPanel.View := View;
  FMainMenuPanel.Display;
  inherited;
end;

{ TKExtMenuViewport }

procedure TKExtMenuViewport.DoDisplay;
begin
  Layout := lyFit;
  FMainMenuPanel := TKExtMenuPanel.AddTo(Items);
  FMainMenuPanel.View := View;
  FMainMenuPanel.Display;
  inherited;
end;

initialization
  TKControllerRegistry.Instance.RegisterClass('MenuWindow', TKExtMenuWindow);
  TKControllerRegistry.Instance.RegisterClass('MenuViewport', TKExtMenuViewport);

finalization
  TKControllerRegistry.Instance.UnregisterClass('MenuWindow');
  TKControllerRegistry.Instance.UnregisterClass('MenuViewport');

end.
