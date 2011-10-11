///	<summary>
///	  Defines the base access controller and related classes and services.
///	  Access control allows the creation of EW applications that restrict
///	  access to resources (such as GUI elements) for certain users.
///	</summary>
unit Kitto.AccessControl;

{$I Kitto.Defines.inc}

interface

uses
  EF.Classes,
  Kitto.Types;

type
  {
    Abstract base access controller. An access controller tells if and how a
    user (identified by a string Id) is allowed to access a resource
    (identified by a URI).

    Only one access controller may be active at any one time. Applications
    wanting to use a custom access control policy should create and register
    an access controller, and then make it active through the EW access
    control host.
    @seealso(TKAccessControlHost)
  }
  TKAccessController = class(TEFComponent)
  protected
    class function InternalGetClassId: string; override;
    {
      Implements GetAccessGrantValue; descendants must return a value that
      depends on AMode. In most cases this value will have to be True if the
      specified auser is allowed to access the resource and False otherwise.
    }
    function InternalGetAccessGrantValue(
      const AUserId, AResourceURI, AMode: string): Variant; virtual; abstract;
  public
    {
      Returns the access grant value for the specified resource, mode and user.
      This methods tells if the user can access the resource in the specified
      mode, and how the access is granted. Generally, the return value would be
      a Boolean, but - depending on the mode - it can be anything.

      See the access control documentation for information about how to
      construct a resource URI, where to get the user Id and what modes are
      available.
    }
    function GetAccessGrantValue(const AUserId, AResourceURI, AMode: string;
      const ADefaultValue: Variant): Variant;
  end;

  {
    Class reference for TKAccessController, used by the registry and factory.
  }
  TKAccessControllerClass = class of TKAccessController;

  {
    Base class for EW access control errors.
  }
  EKAccessControlError = class(EKError);

  {
    Exception raised when access to a certain resource is deniend.
  }
  EKAccessDeniedError = class(EKAccessControlError);

  {
    This class holds a list of registered access controller classes.
  }
  TKAccessControllerRegistry = class(TEFRegistry)
  public
    {
      Adds an access controller class to the registry.
    }
    procedure RegisterClass(const AClass: TKAccessControllerClass);
    {
      Deletes a previously registered access controller class from the registry.
    }
    procedure UnregisterClass(const AClass: TKAccessControllerClass);
  end;

  {
    Uses the registry to create access controllers by class Id.
    It is friend to TKAccessControllerRegistry.
  }
  TKAccessControllerFactory = class(TEFFactory)
  public
    {
      Creates and returns an instance of the access controller class identified
      by AClassId. Raises an exception if said class is not registered.
    }
    function CreateObject(const AClassId: string): TKAccessController;
  end;

{
  Singleton access controller class registry. Used by access controllers and
  by the access controller factory.
}
function EWAccessControllerRegistry: TKAccessControllerRegistry;

{
  Singleton access controller object factory. Used by the access control host.
}
function EWAccessControllerFactory: TKAccessControllerFactory;

type
  {
    Keeps track of the currently active access controller, manages it and
    provides access control-related services.
  }
  TKAccessControlHost = class(TEFComponent)
  private
    FFactory: TKAccessControllerFactory;
    FCurrentAccessController: TKAccessController;
    FAccessControllerId: string;
    function GetCurrentAccessController: TKAccessController;
  public
    {
      Accepts a reference to the factory that will be used to create the correct
      access controller according to the secure configuration.
    }
    constructor Create(const AFactory: TKAccessControllerFactory);
    destructor Destroy; override;
    {
      Gives access to the current access controller, created on demand.

      The Id of the access controller is fetched from the AccessControllerId
      item of the secure configuration. Default is 'Null'.
    }
    property CurrentAccessController: TKAccessController
      read GetCurrentAccessController;
  end;

const
  {
   Ability to view a GUI element in a GUI.
  }
  ACM_VIEW = 'VIEW';
  {
    Ability to execute a GUI element in a GUI; ability to run an EW program.
  }
  ACM_RUN = 'RUN';
  {
    Ability to display existing database records in a form GUI element.
  }
  ACM_READ = 'READ';
  {
    Ability to create new records in a form GUI element.
  }
  ACM_ADD = 'ADD';
  {
    Ability to modify existing database records in a form GUI element.
  }
  ACM_MODIFY = 'MODIFY';
  {
    Ability to delete records in a form GUI element.
  }
  ACM_DELETE = 'DELETE';
  {
    Enables the “Delete all” feature in a form GUI element.
  }
  ACM_DELETE_ALL = 'DELETE_ALL';
  {
    Unlimited access on a resource.
  }
  ACM_ALL = 'ALL';

  {
    This value is considered True (grant) when stored in a permission.
  }
  ACV_TRUE = 1;

  {
    This value is considered False (deny) when stored in a permission.
  }
  ACV_FALSE = 0;

{
  Returns True if the specified access mode is a standard mode, that is one
  of the ACM_* constants (except ACM_ALL).
}
function IsStandardMode(const AMode: string): Boolean;

type
  {
    The Null access controller always grants access. It is used by default.
  }
  TKNullAccessController = class(TKAccessController)
  protected
    function InternalGetAccessGrantValue(
      const AUserId, AResourceURI, AMode: string): Variant; override;
  end;

implementation

uses
  SysUtils, Variants,
  EF.StrUtils, EF.Tree,
  Kitto.Environment;

const
  NULL_ACCESS_CONTROLLER = 'Null';

var
  _EWAccessControllerRegistry: TKAccessControllerRegistry;

function EWAccessControllerRegistry: TKAccessControllerRegistry;
begin
  if not Assigned(_EWAccessControllerRegistry) then
    _EWAccessControllerRegistry := TKAccessControllerRegistry.Create;
  Result := _EWAccessControllerRegistry;
end;

var
  _EWAccessControllerFactory: TKAccessControllerFactory;

function EWAccessControllerFactory: TKAccessControllerFactory;
begin
  if not Assigned(_EWAccessControllerFactory) then
    _EWAccessControllerFactory := TKAccessControllerFactory.Create(EWAccessControllerRegistry);
  Result := _EWAccessControllerFactory;
end;

function IsStandardMode(const AMode: string): Boolean;
begin
  Result := MatchStr(AMode, [ACM_VIEW, ACM_RUN, ACM_READ, ACM_ADD, ACM_MODIFY,
    ACM_DELETE, ACM_DELETE_ALL]);
end;

{ TKAccessControllerRegistry }

procedure TKAccessControllerRegistry.RegisterClass(const AClass: TKAccessControllerClass);
begin
  inherited RegisterClass(AClass.GetClassId, AClass);
end;

procedure TKAccessControllerRegistry.UnregisterClass(const AClass: TKAccessControllerClass);
begin
  inherited UnregisterClass(AClass.GetClassId);
end;

{ TKAccessControllerFactory }

function TKAccessControllerFactory.CreateObject(const AClassId: string): TKAccessController;
begin
  Result := inherited CreateObject(AClassId) as TKAccessController;
end;

{ TKAccessController }

function TKAccessController.GetAccessGrantValue(const AUserId, AResourceURI,
  AMode: string; const ADefaultValue: Variant): Variant;
var
  LAllResult: Variant;
begin
  Result := InternalGetAccessGrantValue(AUserId, AResourceURI, AMode);
  // If no permission was found and a standard mode was specified,
  // try the catch-all-standard-modes mode. If a TRUE permission was found,
  // give a chance to revoke it (only for standard modes).
  if IsStandardMode(AMode) and (VarIsNull(Result) or (Result = ACV_TRUE)) then
  begin
    LAllResult := InternalGetAccessGrantValue(AUserId, AResourceURI, ACM_ALL);
    if not VarIsNull(LAllResult) then
      Result := LAllResult;
  end;
  if VarIsNull(Result) then
    Result := ADefaultValue;
end;

class function TKAccessController.InternalGetClassId: string;
begin
  Result := StripPrefixAndSuffix(inherited InternalGetClassId, 'K', 'AccessController');
end;

{ TKAccessControlHost }

constructor TKAccessControlHost.Create(
  const AFactory: TKAccessControllerFactory);
begin
  inherited Create;
  FFactory := AFactory;
end;

destructor TKAccessControlHost.Destroy;
begin
  FreeAndNil(FCurrentAccessController);
  inherited;
end;

function TKAccessControlHost.GetCurrentAccessController: TKAccessController;
var
  LConfig: TEFNode;
  I: Integer;
begin
  if not Assigned(FCurrentAccessController) then
  begin
    Assert(Assigned(FFactory));

    if FAccessControllerId = '' then
      FAccessControllerId := Environment.Config.GetString('AccessControl', NULL_ACCESS_CONTROLLER);
    FCurrentAccessController := FFactory.CreateObject(FAccessControllerId);
    LConfig := Environment.Config.FindNode('AccessControl');
    if Assigned(LConfig) then
      for I := 0 to LConfig.ChildCount - 1 do
        FCurrentAccessController.Config.AddChild(TEFNode.Clone(LConfig.Children[I]));
  end;
  Result := FCurrentAccessController;
end;

{ TKNullAccessController }

function TKNullAccessController.InternalGetAccessGrantValue(
  const AUserId, AResourceURI, AMode: string): Variant;
begin
  if IsStandardMode(AMode) then
    Result := ACV_TRUE
  else
    Result := Null;
end;

initialization
  EWAccessControllerRegistry.RegisterClass(TKNullAccessController);

finalization
  EWAccessControllerRegistry.UnregisterClass(TKNullAccessController);
  FreeAndNil(_EWAccessControllerFactory);
  FreeAndNil(_EWAccessControllerRegistry);

end.
