{-------------------------------------------------------------------------------
   Copyright 2012-2018 Ethea S.r.l.

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

unit Kitto.JS.Controller;

{$I Kitto.Defines.inc}

interface

uses
  SysUtils
  , Classes
  , EF.Intf
  , EF.ObserverIntf
  , EF.Tree
  , EF.Types
  , Kitto.JS.Base
  , Kitto.Types
  , Kitto.Metadata.Views
  , Kitto.JS
  ;

type
  /// <summary>
  ///  Holds a list of registered controller classes.
  /// </summary>
  /// <remarks>
  ///  Classes passed to RegisterClass and UnregisterClass must implement
  ///  IJSController, otherwise an exception is raised.
  /// </remarks>
 { TODO :
allow to overwrite registrations in order to override predefined controllers;
keep track of all classes registered under the same name to handle
de-registration gracefully. }
  TJSControllerRegistry = class(TEFRegistry)
  private
    class var FInstance: TJSControllerRegistry;
    class function GetInstance: TJSControllerRegistry; static;
  protected
    procedure BeforeRegisterClass(const AId: string; const AClass: TClass); override;
  public
    class destructor Destroy;
  public
    class property Instance: TJSControllerRegistry read GetInstance;
    procedure RegisterClass(const AId: string; const AClass: TJSObjectClass);
    function GetClass(const AId: string): TJSObjectClass;
  end;

  /// <summary>
  ///  Queries the registry to create controllers by class Id. It is
  ///  friend to TJSControllerRegistry.
  /// </summary>
  TJSControllerFactory = class
  private
    class var FInstance: TJSControllerFactory;
    class function GetInstance: TJSControllerFactory; static;
  public
    class destructor Destroy;
    class property Instance: TJSControllerFactory read GetInstance;

    /// <summary>
    ///  Creates a controller for the specified view.
    /// </summary>
    /// <param name="AOwner">
    ///  Owner for the created object (only used if AContainer is nil,
    ///  otherwise the container is the owner).
    /// </param>
    /// <param name="AView">
    ///  A reference to the view to control. The view object's lifetime is
    ///  managed externally.
    /// </param>
    /// <param name="AContainer">
    ///  Visual container to which to add the newly created controller.
    /// </param>
    /// <param name="AConfig">
    ///  Optional controller config node. If not specified, it is taken from
    ///  the view's 'Controller' node.
    /// </param>
    /// <param name="AObserver">
    ///  Optional observer that will receive events posted by the controller.
    /// </param>
    /// <param name="ACustomType">
    ///  Custom controller type, used to override the one specified in the view.
    /// </param>
    function CreateController(const AOwner: TJSBase; const AView: TKView;
      const AContainer: IJSContainer; const AConfig: TEFNode = nil;
      const AObserver: IEFObserver = nil; const ACustomType: string = ''): IJSController;
  end;

implementation

uses
  EF.Localization
  ;

{ TJSControllerRegistry }

procedure TJSControllerRegistry.BeforeRegisterClass(const AId: string; const AClass: TClass);
begin
  if not Supports(AClass, IJSController) then
    raise EKError.CreateFmt('Cannot register class %s (Id %s). Class does not support IJSController.', [AClass.ClassName, AId]);
  inherited;
end;

class destructor TJSControllerRegistry.Destroy;
begin
  FreeAndNil(FInstance);
end;

function TJSControllerRegistry.GetClass(const AId: string): TJSObjectClass;
begin
  Result := TJSObjectClass(inherited GetClass(AId));
end;

class function TJSControllerRegistry.GetInstance: TJSControllerRegistry;
begin
  if FInstance = nil then
    FInstance := TJSControllerRegistry.Create;
  Result := FInstance;
end;

procedure TJSControllerRegistry.RegisterClass(const AId: string; const AClass: TJSObjectClass);
begin

  inherited RegisterClass(AId, AClass);
end;

{ TJSControllerFactory }

class destructor TJSControllerFactory.Destroy;
begin
  FreeAndNil(FInstance);
end;

class function TJSControllerFactory.GetInstance: TJSControllerFactory;
begin
  if FInstance = nil then
    FInstance := TJSControllerFactory.Create;
  Result := FInstance;
end;

function TJSControllerFactory.CreateController(const AOwner: TJSBase; const AView: TKView;
  const AContainer: IJSContainer; const AConfig: TEFNode = nil;
  const AObserver: IEFObserver = nil; const ACustomType: string = ''): IJSController;
var
  LSubject: IEFSubject;
  LObject: TJSObject;
  LOwner: TJSBase;

  function GetClassByType: TJSObjectClass;
  var
    LType: string;
  begin
    LType := ACustomType;
    if LType = '' then
      if Assigned(AConfig) then
        LType := AConfig.AsExpandedString;
    if LType = '' then
      LType := AView.ControllerType;

    if LType = '' then
      raise EKError.CreateFmt(_('Cannot create controller for view %s. Unspecified type.'), [AView.PersistentName]);
    Result := TJSControllerRegistry.Instance.GetClass(LType);
  end;


begin
  Assert(AOwner <> nil);
  Assert(AView <> nil);

  if Assigned(AContainer) then
    LOwner := AContainer.AsJSObject
  else
    LOwner := AOwner;

  LObject := GetClassByType.Create(LOwner);
  try
    // This extract the interface reference into Result.
    if not Supports(LObject, IJSController, Result) then
      raise EKError.Create(_('Object does not support IJSController.'));

    if Assigned(AObserver) and Supports(Result.AsObject, IEFSubject, LSubject) then
      LSubject.AttachObserver(AObserver);

    { TODO : examine callers - see if we can merge here or possibly remove the AConfig argument. }
    if AConfig <> nil then
      Result.Config.Assign(AConfig)
    else
      Result.Config.Assign(AView.FindNode('Controller'));

    Result.View := AView;

    // Calls InitSubController. Do this only after everything else is set up.
    if Assigned(AContainer) then
    begin
      // For some reason assigning Container in AddItem causes it to be missing
      // when Display is called on a subcontroller. Refactoring needed here.
      Result.Container := AContainer;
      AContainer.AddItem(LObject);
    end;
  except
    FreeAndNil(LObject);
    raise;
  end;
end;

end.

