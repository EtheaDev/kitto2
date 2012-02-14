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

///	<summary>
///	  Generally useful types.
///	</summary>
unit EF.Types;

{$I EF.Defines.inc}

interface

uses
  SysUtils, Generics.Collections;

type
  ///	<summary>
  ///	  Many components in EF have OnLog events of this type.
  ///	</summary>
  TEFLogEvent = procedure (const ASender: TObject; const AString: string;
    const ALogLevel: Integer = 1) of object;

  ///	<summary>
  ///	  Base class for all EF exceptions.
  ///	</summary>
  EEFError = class(Exception)
  private
    FAdditionalInfo: string;
  public
    ///	<summary>
    ///	  Creates the exception with an additional message.
    ///	</summary>
    constructor CreateWithAdditionalInfo(const AMessage, AAdditionalInfo: string);

    ///	<summary>
    ///	  An EF exception may optionally have additional information over
    ///	  what's displayed in the Message. The value of this property is set
    ///	  upon creation through the CreateWithAdditionalInfo constructor.
    ///	</summary>
    property AdditionalInfo: string read FAdditionalInfo;
  end;

  TEFPair = TPair<string, string>;
  TEFPairs = array of TEFPair;

  ///	<summary>
  ///	  Holds a list of registered classes and provides class references by
  ///	  string Ids.
  ///	</summary>
  TEFRegistry = class abstract
  private
    FClasses: TDictionary<string, TClass>;
  protected
    ///	<summary>
    ///	  Called at the beginning of RegisterClass.
    ///	</summary>
    procedure BeforeRegisterClass(const AId: string; const AClass: TClass); virtual;

    ///	<summary>
    ///	  Called at the end of RegisterClass, after successful registration.
    ///	</summary>
    procedure AfterRegisterClass(const AId: string; const AClass: TClass); virtual;

    ///	<summary>
    ///	  Called at the beginning of UnregisterClass.
    ///	</summary>
    procedure BeforeUnregisterClass(const AId: string; const AClass: TClass); virtual;

    ///	<summary>
    ///	  Called at the end of UnregisterClass, after successful unregistration.
    ///	</summary>
    procedure AfterUnregisterClass(const AId: string; const AClass: TClass); virtual;

    ///	<summary>
    ///	  Holds the list of registered classes, each of which represented by
    ///	  its Id. Descendants may need to access this object in order to add
    ///   functionality.
    ///	</summary>
    property Classes: TDictionary<string, TClass> read FClasses;
  public
    constructor Create;
    destructor Destroy; override;
  public
    ///	<summary>
    ///	  Adds a class to the registry.
    ///	</summary>
    procedure RegisterClass(const AId: string; const AClass: TClass);

    ///	<summary>
    ///	  Deletes a previously registered class from the registry.
    ///	</summary>
    procedure UnregisterClass(const AId: string);

    ///	<summary>
    ///	  Returns True if a given class Id is registered.
    ///	</summary>
    function HasClass(const AId: string): Boolean;

    ///	<summary>
    ///	  Returns a class reference to the class identified by AClassId, if
    ///	  registered. Otherwise an exception is raised.
    ///	</summary>
    function GetClass(const AId: string): TClass;

    ///	<summary>
    ///	  Returns a class reference to the class identified by AClassId, if
    ///	  registered. Otherwise returns nil.
    ///	</summary>
    function FindClass(const AId: string): TClass;
  end;

  ///	<summary>
  ///	  Uses the registry to create objects by class id. It is friend to a
  ///	  descendant of TEFRegistry.
  ///	</summary>
  TEFFactory = class abstract
  private
    FRegistry: TEFRegistry;
  protected
    ///	<summary>
    ///	  Creates an object of the specified class.
    ///	</summary>
    function DoCreateObject(const AClass: TClass): TObject; virtual;

    ///	<summary>
    ///	  A reference to the registry being used.
    ///	</summary>
    property Registry: TEFRegistry read Fregistry;

    ///	<summary>
    ///	  Called at the beginning of CreateObject, before checking if the class
    ///	  Id is registered.
    ///	</summary>
    procedure BeforeCreateObject(const AId: string); virtual;

    ///	<summary>
    ///	  Called after CreateObject has created the object and before it
    ///	  returns it to the caller.
    ///	</summary>
    procedure AfterCreateObject(const AId: string;
      const AClass: TClass; const AObject: TObject); virtual;
  public
    ///	<summary>
    ///	  Pass a reference to the registry to use when constructing a factory.
    ///	</summary>
    constructor Create(const ARegistry: TEFRegistry);

    ///	<summary>
    ///	  Creates and returns an instance of the class identified by AClassId.
    ///	  Raises an exception if said class is not registered.
    ///	</summary>
    function CreateObject(const AId: string): TObject;

    ///	<summary>
    ///	  Returns True if a given class Id is registered.
    ///	</summary>
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
  if HasClass(UpperCase(AId)) then
    Result := FClasses[UpperCase(AId)]
  else
    raise EEFError.CreateFmt('Class %s not found.', [AId]);
end;

procedure TEFRegistry.RegisterClass(const AId: string; const AClass: TClass);
begin
  BeforeRegisterClass(AId, AClass);
  FClasses.Add(UpperCase(AId), AClass);
  AfterRegisterClass(AId, AClass);
end;

function TEFRegistry.HasClass(const AId: string): Boolean;
begin
  Result := FClasses.ContainsKey(UpperCase(AId));
end;

procedure TEFRegistry.UnregisterClass(const AId: string);
begin
  Assert(AId <> '');

  if FClasses.ContainsKey(UpperCase(AId)) then
    FClasses.Remove(UpperCase(AId));
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

