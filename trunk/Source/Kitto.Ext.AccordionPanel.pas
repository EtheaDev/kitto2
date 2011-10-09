unit Kitto.Ext.AccordionPanel;

{$I Kitto.Defines.inc}

interface

uses
  Ext,
  Kitto.Ext.Base, Kitto.Metadata.Views;

type
  ///	<summary>
  ///	  A panel that hosts subpanels in an accordion layout. Used
  ///   by the AccordionPanel controller.
  ///	</summary>
  TKExtAccordionPanel = class(TExtPanel)
  private
    FView: TKView;
    procedure DisplaySubViews;
    procedure SetView(const AValue: TKView);
  protected
    procedure InitDefaults; override;
  public
    property View: TKView read FView write SetView;
  end;

  TKExtAccordionPanelController = class(TKExtPanelControllerBase)
  private
    FAccordionPanel: TKExtAccordionPanel;
  protected
    procedure InitDefaults; override;
    procedure DoDisplay; override;
  end;

implementation

uses
  ExtPascal, ExtLayout,
  EF.Tree,
  Kitto.Ext.Controller, Kitto.Environment;

{ TKExtAccordionPanelController }

procedure TKExtAccordionPanelController.DoDisplay;
begin
  inherited;
  Title := View.DisplayLabel;
  Border := False;
  { TODO : make these customizable }
  Width := 180;
  MinSize := 20;
  MaxSize := 400;
  Collapsible := True;
  Split := True;
  FAccordionPanel.View := View;
end;

procedure TKExtAccordionPanelController.InitDefaults;
begin
  inherited;
  Layout := lyFit;

  FAccordionPanel := TKExtAccordionPanel.AddTo(Items);
end;

{ TKExtAccordionPanel }

procedure TKExtAccordionPanel.InitDefaults;
begin
  inherited;
  Layout := lyAccordion;
  LayoutConfig := JSObject('animate:true');
  Border := False;
end;

procedure TKExtAccordionPanel.SetView(const AValue: TKView);
begin
  Assert(Assigned(AValue));

  FView := AValue;
  DisplaySubViews;
end;

procedure TKExtAccordionPanel.DisplaySubViews;
var
  LController: IKExtController;
  LViews: TEFNode;
  I: Integer;
begin
  Assert(Assigned(FView));

  LViews := View.FindNode('Controller/SubViews');
  if Assigned(LViews) then
  begin
    for I := 0 to LViews.ChildCount - 1 do
    begin
      LController := TKExtControllerFactory.Instance.CreateController(
        Environment.Views.ViewByNode(LViews.Children[I]), Self);
      LController.Display;
    end;
    if Items.Count > 0 then
      On('afterrender', JSFunction(JSName + '.getLayout().setActiveItem(0);'));
      //TExtLayoutAccordionLayout(GetLayout).SetActiveItem(0);
  end;
end;

initialization
  TKExtControllerRegistry.Instance.RegisterClass('AccordionPanel', TKExtAccordionPanelController);

finalization
  TKExtControllerRegistry.Instance.UnregisterClass('AccordionPanel');

end.

