unit Kitto.Ext.TabPanel;

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
  ExtPascal,
  EF.Tree,
  Kitto.Ext.Controller, Kitto.Environment;

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
  Defaults := JSObject('autoscroll:true');
  EnableTabScroll := True;
  DeferredRender := True;
end;

procedure TKExtTabPanel.DisplaySubViews;
var
  LController: IKExtController;
  LViews: TEFNode;
  I: Integer;
begin
  Assert(Assigned(FView));

  LViews := View.GetNode('Controller/SubViews');
  for I := 0 to LViews.ChildCount - 1 do
  begin
    LController := TKControllerFactory.Instance.CreateController(
      Environment.Views.ViewByName(LViews.Children[I].Name), Self);
    LController.Display;
  end;
  if Items.Count > 0 then
    SetActiveTab(0);
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
  FView := AValue;
  DisplaySubViews;
end;

initialization
  TKControllerRegistry.Instance.RegisterClass('TabPanel', TKExtTabPanelController);

finalization
  TKControllerRegistry.Instance.UnregisterClass('TabPanel');

end.
