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
  SysUtils, Generics.Collections, Generics.Defaults;

type
  ///	<summary>
  ///	  Generic prototype used for functions that translate names, such as field or alias names.
  ///	</summary>
  TNameTranslator = reference to function (const AName: string): string;

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

  TEFTriple = record
    Value1: string;
    Value2: string;
    Value3: string;
    constructor Create(const AValue1, AVAlue2, AVAlue3: string);
    procedure AssignPair({$IFDEF D15+}const {$ENDIF}APair: TEFPair);
  end;
  TEFTriples = array of TEFTriple;

  ///	<summary>
  ///	  Holds a list of registered classes and provides class references by
  ///	  string Ids.
  ///	</summary>
  TEFRegistry = class abstract
  strict private
    FClasses: TDictionary<string, TClass>;
  strict protected
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

    ///	<summary>
    ///	  Raises the "class not found" exception.
    ///	</summary>
    procedure ClassNotFound(const AClassId: string);
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
    ///	  Returns the Id under which a class was registered, or ''.
    ///	</summary>
    function FindClassId(const AClass: TClass): string;

    ///	<summary>
    ///	  Returns a class reference to the class identified by AClassId, if
    ///	  registered. Otherwise returns nil.
    ///	</summary>
    function FindClass(const AId: string): TClass;

    ///	<summary>
    ///   Returns a sorted array with all registered class Ids.
    ///	</summary>
    function GetClassIds: TArray<string>;
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

  TEFArray = class(Generics.Collections.TArray)
  public
    class function Contains<T>(const Values: array of T; const Item: T;
      const Comparer: IEqualityComparer<T>; out ItemIndex: Integer): Boolean;
      overload; static;
    class function Contains<T>(const Values: array of T; const Item: T;
      out ItemIndex: Integer): Boolean; overload; static;
    class function Contains<T>(const Values: array of T; const Item: T): Boolean;
      overload; static;
    class function IndexOf<T>(const Values: array of T; const Item: T;
      const Comparer: IEqualityComparer<T>): Integer; overload; static;
    class function IndexOf<T>(const Values: array of T; const Item: T): Integer;
      overload; static;
  end;

implementation

uses
  {$IFDEF D22+}System.Hash,{$ENDIF}
  EF.Localization;

{ EEFError }

constructor EEFError.CreateWithAdditionalInfo(const AMessage,
  AAdditionalInfo: string);
begin
  inherited Create(AMessage);
  FAdditionalInfo := AAdditionalInfo;
end;

type
  TCaseInsEqualityComparer = class(TEqualityComparer<string>)
  public
    function Equals(const Left, Right: string): Boolean; override;
    function GetHashCode(const Value: string): Integer; override;
  end;

function TCaseInsEqualityComparer.Equals(const Left, Right: string): Boolean;
begin
  Result := SameText(Left, Right);
end;

function TCaseInsEqualityComparer.GetHashCode(const Value: string): Integer;
var
  LString: string;
begin
  LString := UpperCase(Value);
  {$IFDEF D22+}
  Result := THashBobJenkins.GetHashValue(LString);
  {$ELSE}
  Result := BobJenkinsHash(LString[1], Length(LString) * SizeOf(LString[1]), 0);
  {$ENDIF}
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
  FClasses := TDictionary<string, TClass>.Create(TCaseInsEqualityComparer.Create);
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

function TEFRegistry.FindClassId(const AClass: TClass): string;
var
  LClass: TPair<string, TClass>;
begin
  Result := '';
  for LClass in FClasses do
  begin
    if LClass.Value = AClass then
    begin
      Result := LClass.Key;
      Break;
    end;
  end;
end;

function TEFRegistry.GetClass(const AId: string): TClass;
begin
  Result := nil; // Avoids warning.
  if HasClass(AId) then
    Result := FClasses[AId]
  else
    ClassNotFound(AId);
end;

procedure TEFRegistry.ClassNotFound(const AClassId: string);
begin
  raise EEFError.CreateFmt('Class %s not found.', [AClassId]);
end;

function TEFRegistry.GetClassIds: TArray<string>;
{$IFDEF D15+}
begin
  Result := Classes.Keys.ToArray;
{$ELSE}
var
  LIndex: Integer;
  LClassId: string;
begin
  SetLength(Result, Classes.Count);
  LIndex := 0;
  for LClassId in Classes.Keys do
  begin
    Result[LIndex] := LClassId;
    Inc(LIndex);
  end;
{$ENDIF}
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

{ TEFTriple }

procedure TEFTriple.AssignPair({$IFDEF D15+}const {$ENDIF}APair: TEFPair);
begin
  Value1 := APair.Key;
  Value2 := APair.Value;
  Value3 := '';
end;

constructor TEFTriple.Create(const AValue1, AVAlue2, AVAlue3: string);
begin
  Value1 := AValue1;
  Value2 := AValue2;
  Value3 := AValue3;
end;

{ TEFArray }

class function TEFArray.Contains<T>(const Values: array of T; const Item: T;
  const Comparer: IEqualityComparer<T>; out ItemIndex: Integer): Boolean;
var
  Index: Integer;
begin
  for Index := 0 to high(Values) do begin
    if Comparer.Equals(Values[Index], Item) then begin
      ItemIndex := Index;
      Result := True;
      exit;
    end;
  end;
  ItemIndex := -1;
  Result := False;
end;

class function TEFArray.Contains<T>(const Values: array of T; const Item: T;
  out ItemIndex: Integer): Boolean;
begin
  Result := Contains<T>(Values, Item, TEqualityComparer<T>.Default, ItemIndex);
end;

class function TEFArray.Contains<T>(const Values: array of T; const Item: T): Boolean;
var
  ItemIndex: Integer;
begin
  Result := Contains<T>(Values, Item, ItemIndex);
end;

class function TEFArray.IndexOf<T>(const Values: array of T; const Item: T;
  const Comparer: IEqualityComparer<T>): Integer;
begin
  Contains<T>(Values, Item, Comparer, Result);
end;

class function TEFArray.IndexOf<T>(const Values: array of T; const Item: T): Integer;
begin
  Contains<T>(Values, Item, Result);
end;

end.

