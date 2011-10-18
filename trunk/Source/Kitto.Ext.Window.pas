unit Kitto.Ext.Window;

interface

uses
  Kitto.Ext.Base, Kitto.Ext.Controller;

type
  TKExtWindowController = class(TKExtWindowControllerBase)
  private
    FController: IKExtController;
    procedure CreateSubController;
  protected
    procedure DoDisplay; override;
    procedure InitDefaults; override;
  public
    destructor Destroy; override;
  end;

implementation

uses
  Ext,
  EF.Localization,
  Kitto.Environment, Kitto.Metadata.Views;

{ TKExtWindowController }

destructor TKExtWindowController.Destroy;
begin
  // Prevent the compiler from calling _Release.
  Pointer(FController) := nil;
  inherited;
end;

procedure TKExtWindowController.DoDisplay;
begin
  Title := _(View.GetExpandedString('DisplayLabel'));
  Width := View.GetInteger('Controller/Width', 800);
  Height := View.GetInteger('Controller/Height', 600);
  CreateSubController;
  inherited;
end;

procedure TKExtWindowController.InitDefaults;
begin
  inherited;
  Constrain := True;
  Closable := False;
end;

procedure TKExtWindowController.CreateSubController;
var
  LSubView: TKView;
begin
  Assert(Assigned(View));

  LSubView := Environment.Views.ViewByNode(View.GetNode('Controller/SubView'));
  FController := TKExtControllerFactory.Instance.CreateController(LSubView, Self);
  FController.Display;
end;

initialization
  TKExtControllerRegistry.Instance.RegisterClass('Window', TKExtWindowController);

finalization
  TKExtControllerRegistry.Instance.UnregisterClass('Window');

end.
