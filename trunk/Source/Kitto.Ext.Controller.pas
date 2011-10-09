unit Kitto.Ext.Controller;

{$I Kitto.Defines.inc}

interface

uses
  Classes,
  Ext, ExtPascal,
  EF.ObserverIntf, EF.Classes,
  Kitto.Types, Kitto.Metadata.Views;

type
  ///	<summary>
  ///	  Base interface for controllers. Controllers manages views to build
  ///   the user interface.
  ///	</summary>
  IKExtController = interface(IEFComponent)
    ['{FCDFC7CC-E202-4C20-961C-11255CABE497}']

    ///	<summary>
    ///	  Renders AView according to the Config.
    ///	</summary>
    procedure Display;

    function GetView: TKView;
    procedure SetView(const AValue: TKView);
    property View: TKView read GetView write SetView;

    function GetContainer: TExtContainer;
    procedure SetContainer(const AValue: TExtContainer);
    property Container: TExtContainer read GetContainer write SetContainer;
  end;

  ///	<summary>
  ///	  Holds a list of registered controller classes.
  ///	</summary>
  ///	<remarks>
  ///	  Classes passed to RegisterClass and UnregisterClass must implement
  ///	  IKController, otherwise an exception is raised.
  ///	</remarks>
 { TODO :
allow to overwrite registrations in order to override predefined controllers;
keep track of all classes registered under the same name to handle
de-registration gracefully. }
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
    procedure RegisterClass(const AId: string; const AClass: TExtObjectClass);
  end;

  ///	<summary>
  ///	  Queries the registry to create controllers by class Id. It is
  ///	  friend to TKControllerRegistry.
  ///	</summary>
  TKControllerFactory = class
  private
    class var FInstance: TKControllerFactory;
    class function GetInstance: TKControllerFactory; static;
  public
    class destructor Destroy;
    class property Instance: TKControllerFactory read GetInstance;

    function CreateController(const AView: TKView; const AContainer: TExtContainer;
      const AObserver: IEFObserver = nil; const ACustomType: string = ''): IKExtController;
  end;

implementation

uses
  SysUtils;

{ TKControllerRegistry }

procedure TKControllerRegistry.BeforeRegisterClass(const AId: string;
  const AClass: TClass);
begin
  if not AClass.InheritsFrom(TExtObject) or not Supports(AClass, IKExtController) then
    raise EKError.CreateFmt('Cannot register class %s (Id %s). Class is not a TExtObject descendant or does not support IKController.', [AClass.ClassName, AId]);
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

procedure TKControllerRegistry.RegisterClass(const AId: string;
  const AClass: TExtObjectClass);
begin
  inherited RegisterClass(AId, AClass);
end;

{ TKControllerFactory }

class destructor TKControllerFactory.Destroy;
begin
  FreeAndNil(FInstance);
end;

class function TKControllerFactory.GetInstance: TKControllerFactory;
begin
  if FInstance = nil then
    FInstance := TKControllerFactory.Create;
  Result := TKControllerFactory(FInstance);
end;

type
  TBreakExtObject = class(TExtObject);

function TKControllerFactory.CreateController(const AView: TKView;
  const AContainer: TExtContainer; const AObserver: IEFObserver;
  const ACustomType: string): IKExtController;
var
  LClass: TExtObjectClass;
  LIntf: IEFSubject;
  LObject: TExtObject;
  LType: string;
begin
  Assert(AView <> nil);

  LType := ACustomType;
  if LType = '' then
    LType := AView.ControllerType;

  if LType = '' then
    raise EKError.Create('Cannot create controller. Unspecified type.');

  LClass := TExtObjectClass(TKControllerRegistry.Instance.GetClass(LType));

  if Assigned(AContainer) then
    LObject := LClass.AddTo(AContainer.Items)
  else
  begin
    LObject := LClass.Create;
    { TODO : fix virtual construction in ExtPascal! }
    TBreakExtObject(LObject).InitDefaults;
  end;

  if not Supports(LObject, IKExtController, Result) then
    raise EKError.Create('Object does not support IKController.');

  Result.View := AView;
  Result.Container := AContainer;
  if Assigned(AObserver) and Supports(Result.AsObject, IEFSubject, LIntf) then
    Lintf.AttachObserver(AObserver);
end;

end.

