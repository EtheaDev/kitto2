{-------------------------------------------------------------------------------
   Copyright 2012 Ethea S.r.l.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
-------------------------------------------------------------------------------}

unit Kitto.Ext.Controller;

{$I Kitto.Defines.inc}

interface

uses
  SysUtils, Classes,
  Ext, ExtPascal,
  EF.Intf, EF.ObserverIntf, EF.Tree, EF.Types,
  Kitto.Types, Kitto.Metadata.Views;

type
  ///	<summary>
  ///	  Base interface for controllers. Controllers manages views to build
  ///   the user interface.
  ///	</summary>
  IKExtController = interface(IEFInterface)
    ['{FCDFC7CC-E202-4C20-961C-11255CABE497}']

    ///	<summary>
    ///	  Renders AView according to the Config.
    ///	</summary>
    procedure Display;

    function GetConfig: TEFNode;
    property Config: TEFNode read GetConfig;

    function GetView: TKView;
    procedure SetView(const AValue: TKView);
    property View: TKView read GetView write SetView;

    function GetContainer: TExtContainer;
    procedure SetContainer(const AValue: TExtContainer);
    property Container: TExtContainer read GetContainer write SetContainer;

    function SupportsContainer: Boolean;
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
  TKExtControllerRegistry = class(TEFRegistry)
  private
    class var FInstance: TKExtControllerRegistry;
    class function GetInstance: TKExtControllerRegistry; static;
  protected
    procedure BeforeRegisterClass(const AId: string; const AClass: TClass);
      override;
  public
    class destructor Destroy;
    class property Instance: TKExtControllerRegistry read GetInstance;
    procedure RegisterClass(const AId: string; const AClass: TExtObjectClass);
  end;

  ///	<summary>
  ///	  Queries the registry to create controllers by class Id. It is
  ///	  friend to TKControllerRegistry.
  ///	</summary>
  TKExtControllerFactory = class
  private
    class var FInstance: TKExtControllerFactory;
    class function GetInstance: TKExtControllerFactory; static;
  public
    class destructor Destroy;
    class property Instance: TKExtControllerFactory read GetInstance;

    ///	<summary>Creates a controller for the specified view.</summary>
    ///	<param name="AView">A reference to the view to control. The view
    ///	object's lifetime is managed externally.</param>
    ///	<param name="AContainer">Visual container to which to add the newly
    ///	created controller.</param>
    ///	<param name="AConfig">Optional controller config node. If not
    ///	specified, it is taken from the view's 'Controller' node.</param>
    ///	<param name="AObserver">Optional observer that will receive events
    ///	posted by the controller.</param>
    ///	<param name="ACustomType">Custom controller type, used to override the
    ///	one specified in the view.</param>
    function CreateController(const AView: TKView; const AContainer: TExtContainer;
      const AConfig: TEFNode = nil; const AObserver: IEFObserver = nil;
      const ACustomType: string = ''): IKExtController;
  end;

implementation

{ TKExtControllerRegistry }

procedure TKExtControllerRegistry.BeforeRegisterClass(const AId: string;
  const AClass: TClass);
begin
  if not AClass.InheritsFrom(TExtObject) or not Supports(AClass, IKExtController) then
    raise EKError.CreateFmt('Cannot register class %s (Id %s). Class is not a TExtObject descendant or does not support IKController.', [AClass.ClassName, AId]);
  inherited;
end;

class destructor TKExtControllerRegistry.Destroy;
begin
  FreeAndNil(FInstance);
end;

class function TKExtControllerRegistry.GetInstance: TKExtControllerRegistry;
begin
  if FInstance = nil then
    FInstance := TKExtControllerRegistry.Create;
  Result := FInstance;
end;

procedure TKExtControllerRegistry.RegisterClass(const AId: string;
  const AClass: TExtObjectClass);
begin
  inherited RegisterClass(AId, AClass);
end;

{ TKExtControllerFactory }

class destructor TKExtControllerFactory.Destroy;
begin
  FreeAndNil(FInstance);
end;

class function TKExtControllerFactory.GetInstance: TKExtControllerFactory;
begin
  if FInstance = nil then
    FInstance := TKExtControllerFactory.Create;
  Result := FInstance;
end;

type
  TBreakExtObject = class(TExtObject);

function TKExtControllerFactory.CreateController(const AView: TKView;
  const AContainer: TExtContainer; const AConfig: TEFNode;
  const AObserver: IEFObserver; const ACustomType: string): IKExtController;
var
  LClass: TExtObjectClass;
  LIntf: IEFSubject;
  LObject: TExtObject;
  LType: string;
begin
  Assert(AView <> nil);

  LType := ACustomType;
  if LType = '' then
    if Assigned(AConfig) then
      LType := AConfig.AsExpandedString;
  if LType = '' then
    LType := AView.ControllerType;

  if LType = '' then
    raise EKError.Create('Cannot create controller. Unspecified type.');

  LClass := TExtObjectClass(TKExtControllerRegistry.Instance.GetClass(LType));

  LObject := LClass.Create;

  if not Supports(LObject, IKExtController, Result) then
    raise EKError.Create('Object does not support IKController.');

  { TODO : fix virtual construction in ExtPascal! }
  TBreakExtObject(LObject).InitDefaults;
  if Assigned(AContainer) and Result.SupportsContainer then
    LObject.AddTo(AContainer.Items);

  if AConfig <> nil then
    Result.Config.Assign(AConfig)
  else
    Result.Config.Assign(AView.FindNode('Controller'));
  Result.View := AView;
  if Result.SupportsContainer then
    Result.Container := AContainer;
  if Assigned(AObserver) and Supports(Result.AsObject, IEFSubject, LIntf) then
    LIntf.AttachObserver(AObserver);
end;

end.

