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
  Kitto.Environment;

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
  Layout := lyFit;
  CreateSubController;
end;

procedure TKExtViewportController.CreateSubController;
var
  LSubViewName: string;
begin
  Assert(Assigned(View));

  LSubViewName := View.GetString('Controller/SubView');
  if LSubViewName <> '' then
  begin
    FController := TKControllerFactory.Instance.CreateController(
      Environment.Views.ViewByName(LSubViewName), Self);
    FController.Display;
  end;
end;

initialization
  TKControllerRegistry.Instance.RegisterClass('Viewport', TKExtViewportController);

finalization
  TKControllerRegistry.Instance.UnregisterClass('Viewport');

end.

