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
///	  Defines a useful base TEFComponent base class and related classes.
///	</summary>
unit EF.Classes;

{$I EF.Defines.inc}

interface

uses
  Classes,
  EF.Intf, EF.Types, EF.Tree, EF.ObserverIntf;

type
  ///	<summary>
  ///	  <para>
  ///	    Abstract class for configurable objects with logging and observer
  ///	    capability. A TEFComponent:<br />- has class-level and object-level 
  ///	    identification methods;<br />- has a Config that it can load from a
  ///	    file;<br />- can be a subject and observer;<br />- has logging
  ///	    capability.
  ///	  </para>
  ///	  <para>
  ///	    Used as a base class for various kinds of objects in EF.
  ///	  </para>
  ///	</summary>
  TEFComponent = class(TEFSubjectAndObserver)
  private
    FOnLog: TEFLogEvent;
    FLogLevel: Integer;
    FConfig: TEFNode;
    function GetConfig: TEFNode;
  public
    const DEFAULT_LOG_LEVEL = 1;
  protected
    ///	<summary>
    ///	  Override this method to enable Config auto-load upon first request.
    ///	  The default implementation returns '', which disables auto-load.
    ///	</summary>
    function GetConfigFileName: string; virtual;

    ///	<summary>
    ///	  Implements GetClassId. Override this method to give the class a
    ///	  custom string Id.
    ///	</summary>
    ///	<remarks>
    ///	  Identifying classes by string-based Ids is useful in registration
    ///	  frameworks, in which class Ids are read from a file and corresponding
    ///	  class references must be retrieved from a registry object.
    ///	</remarks>
    class function InternalGetClassId: string; virtual;

    ///	<summary>
    ///	  Implements GetId. Override this method to give the class a custom
    ///	  object-level string Id.
    ///	</summary>
    ///	<remarks>
    ///	  Identifying objects by string-based Ids is useful in registration
    ///	  frameworks, in which object Ids are read from a file and
    ///	  corresponding objects must be retrieved from a registry or factory
    ///	  object.
    ///	</remarks>
    function InternalGetId: string; virtual;

    ///	<summary>
    ///	  Fires OnLog.
    ///	</summary>
    procedure DoLog(const AString: string; const ALogLevel: Integer = DEFAULT_LOG_LEVEL); overload;

    ///	<summary>
    ///	  Calls DoLog for each line in AStrings, pre-pending ALinePrefix to
    ///	  each line.
    ///	</summary>
    ///	<remarks>
    ///	  By using this method, OnLog is called once for each string in
    ///	  AStrings.
    ///	</remarks>
    procedure DoLog(const AStrings: TStrings; const ALinePrefix: string = '';
      const ALogLevel: Integer = DEFAULT_LOG_LEVEL); overload;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
    function AsObject: TObject;
  public

    ///	<summary>
    ///	  <para>
    ///	    Returns an identification string based on the class name.
    ///	  </para>
    ///	  <para>
    ///	    By default, returns the class name without any 'T' or 'TEF' prefix.
    ///	  </para>
    ///	</summary>
    ///	<remarks>
    ///	  Identifying classes by string-based Ids is useful in registration
    ///	  frameworks, in which class Ids are read from a file and corresponding
    ///	  class references must be retrieved from a registry object.
    ///	</remarks>
    class function GetClassId: string;

    ///	<summary>
    ///	  Returns an identification string for the object. By default returns
    ///	  the same value as GetClassId.
    ///	</summary>
    ///	<remarks>
    ///	  Identifying objects by string-based Ids is useful in registration
    ///	  frameworks, in which object Ids are read from a file and
    ///	  corresponding objects must be retrieved from a registry or factory
    ///	  object.
    ///	</remarks>
    function GetId: string;

    ///	<summary>
    ///	  Fired each time the object needs to log information about its work.
    ///	</summary>
    property OnLog: TEFLogEvent read FOnLog write FOnLog;

    ///	<summary>
    ///	  Gets or sets the logging level for this component. Only messages that
    ///	  have a level lower than or equal to this setting will be logged.
    ///	</summary>
    property LogLevel: Integer read FLogLevel write FLogLevel default DEFAULT_LOG_LEVEL;

    ///	<summary>
    ///	  An internal tree-like config object used to set values that affect
    ///	  the object's operations.
    ///	</summary>
    property Config: TEFNode read GetConfig;

    ///	<summary>Invalidates the internal config object so that it is
    ///	re-created at next access.</summary>
    procedure InvalidateConfig;
  end;
  TEFComponentClass = class of TEFComponent;

implementation

uses
  SysUtils,
  EF.StrUtils;

{ TEFComponent }

procedure TEFComponent.AfterConstruction;
begin
  inherited;
  FLogLevel := DEFAULT_LOG_LEVEL;
end;

function TEFComponent.AsObject: TObject;
begin
  Result := Self;
end;

destructor TEFComponent.Destroy;
begin
  FreeAndNil(FConfig);
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
  // If the class name doesn't start with TEF, then let's at least
  // remove the T.
  Result := StripPrefix(ClassName, 'T');
  Result := StripPrefix(Result, 'EF');
end;

function TEFComponent.InternalGetId: string;
begin
  Result := GetClassId;
end;

procedure TEFComponent.InvalidateConfig;
begin
  FreeAndNil(FConfig);
end;

end.
