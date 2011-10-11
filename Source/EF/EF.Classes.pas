unit EF.Classes;

{$I EF.Defines.inc}

interface

uses
  Types, SysUtils, Classes, DB, Generics.Collections,
  EF.Intf, EF.Types, EF.Macros, EF.Tree, EF.ObserverIntf;

const
  {
    Default value for TEFComponent.LogLevel.
  }
  DEFAULT_LOG_LEVEL = 1;

type
  IEFTreeItem = interface(IEFInterface)
    ['{1C109A68-2108-4679-A76A-FC019B883A8B}']
    {
      Reparents the specified item under the current object. The implementor
      decides where the nEF item will go and how to unlink it from its previous
      parent if any, as required. Items that are containers might host
      reparented items as children, others as siblings.
      AAfter optionally specifies a sibling item after which to add AItem
      in the container's collection.
    }
    procedure ReparentItem(const AItem: IEFTreeItem;
      const AAfter: IEFTreeItem = nil);
  end;

  TEFComponent = class(TEFSubjectAndObserver)
  private
    FOnLog: TEFLogEvent;
    FLogLevel: Integer;
    FObservers: TInterfaceList;
    FConfig: TEFNode;
    function GetConfig: TEFNode;
  protected
    {
      Override this method to enable Config auto-load upon first request.
      The default implementation returns '', which disables auto-load.
    }
    function GetConfigFileName: string; virtual;
    {
      Implements GetClassId.
    }
    class function InternalGetClassId: string; virtual;
    {
      Implements GetId.
    }
    function InternalGetId: string; virtual;
    {
      Fires OnLog.
    }
    procedure DoLog(const AString: string; const ALogLevel: Integer = DEFAULT_LOG_LEVEL); overload;
    {
      Calls DoLog for each line in AStrings, pre-pending ALinePrefix to
      each line.
    }
    procedure DoLog(const AStrings: TStrings; const ALinePrefix: string = '';
      const ALogLevel: Integer = DEFAULT_LOG_LEVEL); overload;
    {
      Checks that a proprerty has an assigned/valid value, and raises a
      generic exception otherwise. ACondition should evaluate to True if the
      property is assigned/valid, and to False otherwise. In this case, an
      exception with APropertyName in the message text is raised.
    }
    procedure CheckProperty(const ACondition: Boolean; const APropertyName: string);
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
  public
    {
      Returns an identification string based on the class name. It can be used
      by a registry to hold string identifiers for different classes.
      By default it returns the class name without any 'T' or 'TEF' prefix.
    }
    class function GetClassId: string;
    {
      Returns an Id for the object. By default it returns the same as GetClassId.
    }
    function GetId: string;
    {
      From IEFInterface.
    }
    function AsObject: TObject;
    {
      Fired each time the object needs to log information about its work.
    }
    property OnLog: TEFLogEvent read FOnLog write FOnLog;
    {
      Sets the logging level for this component. Only messages that
      have a level lower than or equal to this setting will be logged.
    }
    property LogLevel: Integer read FLogLevel write FLogLevel default DEFAULT_LOG_LEVEL;

    property Config: TEFNode read GetConfig;
  end;

  TEFComponentClass = class of TEFComponent;

  ///	<summary>
  ///	  Holds a list of registered classes and provides class references by
  ///	  string Ids.
  ///	</summary>
  TEFRegistry = class
  private
    FClasses: TDictionary<string, TClass>;
  protected
    {
      Called at the beginning of RegisterClass.
    }
    procedure BeforeRegisterClass(const AId: string; const AClass: TClass); virtual;
    {
      Called at the end of RegisterClass, after successful registration.
    }
    procedure AfterRegisterClass(const AId: string; const AClass: TClass); virtual;
    {
      Called at the beginning of UnregisterClass.
    }
    procedure BeforeUnregisterClass(const AId: string; const AClass: TClass); virtual;
    {
      Called at the end of UnregisterClass, after successful unregistration.
    }
    procedure AfterUnregisterClass(const AId: string; const AClass: TClass); virtual;
    {
      Holds the list of registered classes, each of which represented by its
      Id and holding the class reference in Objects[]. Descendants may need to
      access this object in order to add functionality.
    }
    property Classes: TDictionary<string, TClass> read FClasses;
  public
    constructor Create;
    destructor Destroy; override;
    {
      Adds a class to the registry.
    }
    procedure RegisterClass(const AId: string; const AClass: TClass);
    {
      Deletes a previously registered class from the registry.
    }
    procedure UnregisterClass(const AId: string);
    {
      Returns True if a given class Id is registered.
    }
    function HasClass(const AId: string): Boolean;
    {
      Returns a class reference to the class identified by AClassId, if registered.
      Otherwise an exception is raised.
    }
    function GetClass(const AId: string): TClass;
    {
      Returns a class reference to the class identified by AClassId, if registered.
      Otherwise returns nil.
    }
    function FindClass(const AId: string): TClass;
  end;

  {
    Uses the registry to create objects by class id.
    It is friend to a descendant of TEFRegistry.
  }
  TEFFactory = class
  private
    FRegistry: TEFRegistry;
  protected
    function DoCreateObject(const AClass: TClass): TObject; virtual;
    {
      A reference to the registry.
    }
    property Registry: TEFRegistry read Fregistry;
    {
      Called at the beginning of CreateObject, before checking if the class Id
      is registered.
    }
    procedure BeforeCreateObject(const AId: string); virtual;
    {
      Called after CreateObject has created the object and before it returns
      it to the caller.
    }
    procedure AfterCreateObject(const AId: string;
      const AClass: TClass; const AObject: TObject); virtual;
  public
    constructor Create(const ARegistry: TEFRegistry);
    {
      Creates and returns an instance of the class identified
      by AClassId. Raises an exception if said class is not registered.
    }
    function CreateObject(const AId: string): TObject;
    {
      Returns True if a given class Id is registered.
    }
    function HasClass(const AId: string): Boolean;
  end;

implementation

uses
  StrUtils, TypInfo,
  EF.SysUtils, EF.StrUtils, EF.Localization, EF.YAML;

{ TEFComponent }

procedure TEFComponent.AfterConstruction;
begin
  inherited;
  FLogLevel := DEFAULT_LOG_LEVEL;
  FObservers := TInterfaceList.Create;
end;

function TEFComponent.AsObject: TObject;
begin
  Result := Self;
end;

procedure TEFComponent.CheckProperty(const ACondition: Boolean;
  const APropertyName: string);
begin
  if not ACondition then
    raise EEFError.CreateFmt(_('Unspecified property "%s".'), [APropertyName]);
end;

destructor TEFComponent.Destroy;
begin
  FreeAndNil(FConfig);
  FreeAndNil(FObservers);
  inherited;
end;

procedure TEFComponent.DoLog(const AString: string; const ALogLevel: Integer = DEFAULT_LOG_LEVEL);
begin
  if (FLogLevel >= ALogLevel) and Assigned(FOnLog) then
    FOnLog(Self, '[' + GetId + '] ' + AString);
end;

procedure TEFComponent.DoLog(const AStrings: TStrings; const ALinePrefix: string = '';
  const ALogLevel: Integer = DEFAULT_LOG_LEVEL);
var
  LLogLineIndex: Integer;
begin
  Assert(Assigned(AStrings));

  for LLogLineIndex := 0 to AStrings.Count - 1 do
    DoLog(ALinePrefix + AStrings[LLogLineIndex], ALogLevel);
end;

class function TEFComponent.GetClassId: string;
begin
  Result := InternalGetClassId;
end;

function TEFComponent.GetConfig: TEFNode;
var
  LConfigFileName: string;
begin
  if not Assigned(FConfig) then
  begin
    LConfigFileName := GetConfigFileName;
    if LConfigFileName <> '' then
      FConfig := TEFTreeFactory.LoadFromFile<TEFNode>(LConfigFileName)
    else
      FConfig := TEFNode.Create;
  end;
  Result := FConfig;
end;

function TEFComponent.GetConfigFileName: string;
begin
  Result := '';
end;

function TEFComponent.GetId: string;
begin
  Result := InternalGetId;
end;

class function TEFComponent.InternalGetClassId: string;
begin
  // Strips standard prefix from the class name.
  // If the class name doesn't start with TEF, then let's at least
  // remove the T.
  Result := StripPrefixAndSuffix(ClassName, 'T', '');
  Result := StripPrefixAndSuffix(Result, 'EF', '');
end;

function TEFComponent.InternalGetId: string;
begin
  Result := GetClassId;
end;

{ TEFRegistry }

procedure TEFRegistry.BeforeRegisterClass(const AId: string; const AClass: TClass);
begin
end;

procedure TEFRegistry.AfterRegisterClass(const AId: string; const AClass: TClass);
begin
end;

procedure TEFRegistry.BeforeUnregisterClass(const AId: string; const AClass: TClass);
begin
end;

procedure TEFRegistry.AfterUnregisterClass(const AId: string; const AClass: TClass);
begin
end;

constructor TEFRegistry.Create;
begin
  inherited Create;
  FClasses := TDictionary<string, TClass>.Create;
end;

destructor TEFRegistry.Destroy;
begin
  FreeAndNil(FClasses);
  inherited;
end;

function TEFRegistry.FindClass(const AId: string): TClass;
begin
  if HasClass(AId) then
    Result := GetClass(AId)
  else
    Result := Default(TClass);
end;

function TEFRegistry.GetClass(const AId: string): TClass;
begin
  if HasClass(AId) then
    Result := FClasses[AId]
  else
    raise EEFError.CreateFmt('Class %s not found.', [AId]);
end;

procedure TEFRegistry.RegisterClass(const AId: string; const AClass: TClass);
begin
  BeforeRegisterClass(AId, AClass);
  FClasses.Add(AId, AClass);
  AfterRegisterClass(AId, AClass);
end;

function TEFRegistry.HasClass(const AId: string): Boolean;
begin
  Result := FClasses.ContainsKey(AId);
end;

procedure TEFRegistry.UnregisterClass(const AId: string);
begin
  Assert(AId <> '');

  if FClasses.ContainsKey(AId) then
    FClasses.Remove(AId);
end;

{ TEFFactory }

procedure TEFFactory.BeforeCreateObject(const AId: string);
begin
end;

procedure TEFFactory.AfterCreateObject(const AId: string;
  const AClass: TClass; const AObject: TObject);
begin
end;

constructor TEFFactory.Create(const ARegistry: TEFRegistry);
begin
  Assert(Assigned(ARegistry));

  inherited Create;
  FRegistry := ARegistry;
end;

function TEFFactory.CreateObject(const AId: string): TObject;
var
  LClass: TClass;
begin
  LClass := FRegistry.GetClass(AId);
  if Assigned(LClass) then
  begin
    BeforeCreateObject(AId);
    Result := DoCreateObject(LClass);
    AfterCreateObject(AId, LClass, Result);
  end
  else
    raise EEFError.CreateFmt(_('%s: Unknown class "%s".'), [ClassName, AId]);
end;

function TEFFactory.DoCreateObject(const AClass: TClass): TObject;
begin
  Result := AClass.Create;
end;

function TEFFactory.HasClass(const AId: string): Boolean;
begin
  Result := FRegistry.HasClass(AId);
end;

end.
