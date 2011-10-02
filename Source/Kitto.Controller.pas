unit Kitto.Controller;

{$I Kitto.Defines.inc}

interface

uses
  Classes,
  EF.ObserverIntf, EF.Classes,
  Kitto.Types, Kitto.Metadata.Views;

type
  ///	<summary>
  ///	  Base interface for controllers. Controllers manages views to build
  ///   the user interface.
  ///	</summary>
  IKController = interface(IEFComponent)
    ['{FCDFC7CC-E202-4C20-961C-11255CABE497}']

    ///	<summary>
    ///	  Renders AView according to the Config.
    ///	</summary>
    procedure Display;

    function GetView: TKView;
    procedure SetView(const AValue: TKView);
    property View: TKView read GetView write SetView;
  end;

  ///	<summary>
  ///	  Holds a list of registered controller classes.
  ///	</summary>
  ///	<remarks>
  ///	  Classes passed to RegisterClass and UnregisterClass must implement
  ///	  IKController, otherwise an exception is raised.
  ///	</remarks>
  TKControllerRegistry = class(TEFRegistry)
  private
    class var FInstance: TKControllerRegistry;
    class function GetInstance: TKControllerRegistry; static;
  protected
    procedure BeforeRegisterClass(const AId: string; const AClass: TClass);
      override;
  public
    class destructor Destroy;
    class property Instance: TKControllerRegistry read GetInstance;
  end;

  ///	<summary>
  ///	  Queries the registry to create controllers by class Id. It is
  ///	  friend to TKControllerRegistry.
  ///	</summary>
  TKControllerFactory = class(TEFFactory)
  private
    class var FInstance: TKControllerFactory;
    class function GetInstance: TKControllerFactory; static;
  protected
    function DoCreateObject(const AClass: TClass): TObject; override;
  public
    class destructor Destroy;
    class property Instance: TKControllerFactory read GetInstance;

    function CreateObject(const AId: string): IKController;
    function CreateController(const AView: TKView; const AObserver: IEFObserver = nil): IKController;
  end;

implementation

uses
  SysUtils,
  ExtPascal;

{ TKControllerRegistry }

procedure TKControllerRegistry.BeforeRegisterClass(const AId: string;
  const AClass: TClass);
begin
  if not Supports(AClass, IKController) then
    raise EKError.CreateFmt('Cannot register class %s (Id %s). Class does not support IKController.', [AClass.ClassName, AId]);
  inherited;
end;

class destructor TKControllerRegistry.Destroy;
begin
  FreeAndNil(FInstance);
end;

class function TKControllerRegistry.GetInstance: TKControllerRegistry;
begin
  if FInstance = nil then
    FInstance := TKControllerRegistry.Create;
  Result := FInstance;
end;

{ TKControllerFactory }

class destructor TKControllerFactory.Destroy;
begin
  FreeAndNil(FInstance);
end;

type
  TBreakExtObject = class(TExtObject);

function TKControllerFactory.DoCreateObject(const AClass: TClass): TObject;
begin
  if AClass.InheritsFrom(TExtObject) then
  begin
    Result := TExtObjectClass(AClass).Create;
    { TODO : fix virtual construction in ExtPascal! }
    TBreakExtObject(Result).InitDefaults;
  end
  else
    Result := inherited DoCreateObject(AClass);
end;

class function TKControllerFactory.GetInstance: TKControllerFactory;
begin
  if FInstance = nil then
    FInstance := TKControllerFactory.Create(TKControllerRegistry.Instance);
  Result := TKControllerFactory(FInstance);
end;

function TKControllerFactory.CreateObject(const AId: string): IKController;
var
  LObject: TObject;
begin
  LObject := inherited CreateObject(AId);
  if not Supports(LObject, IKController, Result) then
    raise EKError.CreateFmt('Class %s not found.', [AId]);
end;

function TKControllerFactory.CreateController(const AView: TKView; const AObserver: IEFObserver): IKController;
begin
  Assert(AView <> nil);

  Result := CreateObject(AView.ControllerType);
  Result.View := AView;
end;

end.

