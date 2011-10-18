unit Kitto.Ext.Viewport;

interface

uses
  Kitto.Ext.Base, Kitto.Ext.Controller;

type
  TKExtViewportController = class(TKExtViewportControllerBase)
  private
    FController: IKExtController;
    procedure CreateSubControllers;
  protected
    procedure DoDisplay; override;
  public
    destructor Destroy; override;
  end;

implementation

uses
  Ext,
  Kitto.Environment, Kitto.Metadata.Views;

{ TKExtViewportController }

destructor TKExtViewportController.Destroy;
begin
  // Prevent the compiler from calling _Release.
  Pointer(FController) := nil;
  inherited;
end;

procedure TKExtViewportController.DoDisplay;
begin
  inherited;
  CreateSubControllers;
end;

procedure TKExtViewportController.CreateSubControllers;
var
  LCenterView: TKView;
begin
  Assert(Assigned(View));

  LCenterView := Environment.Views.ViewByNode(View.GetNode('Controller/CenterView'));
  FController := TKExtControllerFactory.Instance.CreateController(LCenterView, Self);
  FController.Display;
end;

initialization
  TKExtControllerRegistry.Instance.RegisterClass('Viewport', TKExtViewportController);

finalization
  TKExtControllerRegistry.Instance.UnregisterClass('Viewport');

end.

