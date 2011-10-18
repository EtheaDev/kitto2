unit Kitto.Ext.Viewport;

interface

uses
  Kitto.Ext.Base, Kitto.Ext.Controller;

type
  TKExtViewportController = class(TKExtViewportControllerBase)
  private
    FController: IKExtController;
    procedure CreateSubController;
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
  CreateSubController;
end;

procedure TKExtViewportController.CreateSubController;
var
  LSubView: TKView;
begin
  Assert(Assigned(View));

  LSubView := Environment.Views.ViewByNode(View.GetNode('Controller/SubView'));
  FController := TKExtControllerFactory.Instance.CreateController(LSubView, Self);
  FController.Display;
end;

initialization
  TKExtControllerRegistry.Instance.RegisterClass('Viewport', TKExtViewportController);

finalization
  TKExtControllerRegistry.Instance.UnregisterClass('Viewport');

end.

