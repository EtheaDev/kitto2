unit EF.Types;

{$I EF.Defines.inc}

interface

uses
  SysUtils, Generics.Collections;

type
  {
    Many components in EF have OnLog events of this type.
  }
  TEFLogEvent = procedure (const ASender: TObject; const AString: string;
    const ALogLevel: Integer = 1) of object;

  {
    Base class for all EF exceptions.
  }
  EEFError = class(Exception)
  private
    FAdditionalInfo: string;
  public
    constructor CreateWithAdditionalInfo(const AMessage, AAdditionalInfo: string);
    {
      An EF exception may optionally have additional information over what's
      displayed in the Message. The value of this property is set upon
      creation through the CreatEFithAdditionalInfo constructor.
    }
    property AdditionalInfo: string read FAdditionalInfo;
  end;

  TEFPair = TPair<string, string>;
  TEFPairs = array of TEFPair;

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
  EF.Localization;

{ EEFError }

constructor EEFError.CreateWithAdditionalInfo(const AMessage,
  AAdditionalInfo: string);
begin
  inherited Create(AMessage);
  FAdditionalInfo := AAdditionalInfo;
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

