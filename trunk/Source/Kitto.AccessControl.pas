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
///	  Defines the base access controller and related classes and services.
///	  Access control allows the creation of EW applications that restrict
///	  access to resources (such as GUI elements) for certain users.
///	</summary>
unit Kitto.AccessControl;

{$I Kitto.Defines.inc}

interface

uses
  Classes,
  EF.Types, EF.Classes,
  Kitto.Types;

type
  ///	<summary>
  ///	  <para>Abstract base access controller. An access controller tells if
  ///	  and how a user (identified by a string Id) is allowed to access a
  ///	  resource (identified by a URI).</para>
  ///	  <para>Only one access controller may be active at any one time.</para>
  ///	  <para>Applications wanting to use a custom access control policy should
  ///	  create and register an access controller, and then make it active in
  ///	  Config.yaml.</para>
  ///	</summary>
  TKAccessController = class(TEFComponent)
  strict private
    FLogWriter: TStreamWriter;
    FLogHeader: Boolean;
    FLogSeparator: string;
    FLogDelimiter: string;
    procedure WriteLog(const AUserId, AResourceURI, AMode, ADefaultValue,
      AResult: string);
  strict protected
    class function InternalGetClassId: string; override;

    ///	<summary>Implements GetAccessGrantValue; descendants must return a
    ///	value that depends on AMode. In most cases this value will have to be
    ///	True if the specified auser is allowed to access the resource and False
    ///	otherwise, but it can be a value of any type.</summary>
    function InternalGetAccessGrantValue(
      const AUserId, AResourceURI, AMode: string): Variant; virtual; abstract;

    ///	<summary>Implements Init.</summary>
    procedure InternalInit; virtual;
  public
    ///	<summary>
    ///	  <para>Returns the access grant value for the specified resource, mode
    ///	  and user.</para>
    ///	  <para>This methods tells if the user can access the resource in the
    ///	  specified mode, and how the access is granted. Generally, the return
    ///	  value would be a Boolean, but - depending on the mode - it can be
    ///	  anything.</para>
    ///	  <para>See the access control documentation for information about how
    ///	  to construct a resource URI, where to get the user Id and what modes
    ///	  are available.</para>
    ///	</summary>
    function GetAccessGrantValue(const AUserId, AResourceURI, AMode: string;
      const ADefaultValue: Variant): Variant;

    ///	<summary>Shortcut for GetAccessGrantValue for Boolean values. Returns
    ///	True if a value is granted and it equals ACV_TRUE.</summary>
    function IsAccessGranted(const AUserId, AResourceURI, AMode: string): Boolean;

    ///	<summary>Returns True if the specified access mode is a standard mode,
    ///	that is one of the ACM_* constants (except ACM_ALL).</summary>
    class function IsStandardMode(const AMode: string): Boolean;

    ///	<summary>Called by the system after setting all config
    ///	values.</summary>
    procedure Init;
  end;
  TKAccessControllerClass = class of TKAccessController;

  ///	<summary>Exception raised when access to a certain resource is
  ///	deniend.</summary>
  EKAccessDeniedError = class(EKError);

  ///	<summary>This class holds a list of registered access controller
  ///	classes.</summary>
  TKAccessControllerRegistry = class(TEFRegistry)
  private
    class var FInstance: TKAccessControllerRegistry;
    class function GetInstance: TKAccessControllerRegistry; static;
  public
    class destructor Destroy;
    class property Instance: TKAccessControllerRegistry read GetInstance;

    ///	<summary>Adds an access controller class to the registry.</summary>
    procedure RegisterClass(const AId: string; const AClass: TKAccessControllerClass);
  end;

  ///	<summary>Uses the registry to create access controllers by class
  ///	Id.</summary>
  TKAccessControllerFactory = class(TEFFactory)
  private
    class var FInstance: TKAccessControllerFactory;
    class function GetInstance: TKAccessControllerFactory; static;
  public
    class destructor Destroy;
    class property Instance: TKAccessControllerFactory read GetInstance;

    ///	<summary>Creates and returns an instance of the access controller class
    ///	identified by AClassId. Raises an exception if said class is not
    ///	registered.</summary>
    function CreateObject(const AClassId: string): TKAccessController;
  end;

const
  ///	<summary>Ability to view a resource, such as a view. Resources for which
  ///	access is not granted in this mode are invisible to the user.</summary>
  ACM_VIEW = 'VIEW';

  ///	<summary>Ability to execute a resource. If a user has access to a
  ///	resource in VIEW mode but not RUN mode, he can see the menu item (or
  ///	button) but not click it to display the resource.</summary>
  ACM_RUN = 'RUN';

  ///	<summary>Ability to display data (such as a Model).</summary>
  ACM_READ = 'READ';

  ///	<summary>Ability to create new records in a model.</summary>
  ACM_ADD = 'ADD';

  ///	<summary>Ability to modify existing model records.</summary>
  ACM_MODIFY = 'MODIFY';

  ///	<summary>Ability to delete model records.</summary>
  ACM_DELETE = 'DELETE';

  ///	<summary>Unlimited access on a resource.</summary>
  ACM_ALL = 'ALL';

  ///	<summary>This value is considered True (grant) when stored in a
  ///	permission.</summary>
  ACV_TRUE = 1;

  ///	<summary>This value is considered False (deny) when stored in a
  ///	permission.</summary>
  ACV_FALSE = 0;

type
  ///	<summary>The Null access controller always grants access. It is used by
  ///	default.</summary>
  TKNullAccessController = class(TKAccessController)
  protected
    function InternalGetAccessGrantValue(
      const AUserId, AResourceURI, AMode: string): Variant; override;
  end;

implementation

uses
  SysUtils, StrUtils, Variants,
  EF.SysUtils, EF.StrUtils, EF.VariantUtils, EF.Tree;

{ TKAccessControllerRegistry }

class destructor TKAccessControllerRegistry.Destroy;
begin
  FreeAndNil(FInstance);
end;

class function TKAccessControllerRegistry.GetInstance: TKAccessControllerRegistry;
begin
  if FInstance = nil then
    FInstance := TKAccessControllerRegistry.Create;
  Result := FInstance;
end;

procedure TKAccessControllerRegistry.RegisterClass(const AId: string; const AClass: TKAccessControllerClass);
begin
  inherited RegisterClass(AId, AClass);
end;

{ TKAccessControllerFactory }

function TKAccessControllerFactory.CreateObject(const AClassId: string): TKAccessController;
begin
  Result := inherited CreateObject(AClassId) as TKAccessController;
end;

class destructor TKAccessControllerFactory.Destroy;
begin
  FreeAndNil(FInstance);
end;

class function TKAccessControllerFactory.GetInstance: TKAccessControllerFactory;
begin
  if FInstance = nil then
    FInstance := TKAccessControllerFactory.Create(TKAccessControllerRegistry.Instance);
  Result := FInstance;
end;

{ TKAccessController }

class function TKAccessController.IsStandardMode(const AMode: string): Boolean;
begin
  Result := MatchStr(AMode, [ACM_VIEW, ACM_RUN, ACM_READ, ACM_ADD, ACM_MODIFY,
    ACM_DELETE]);
end;

function TKAccessController.GetAccessGrantValue(const AUserId, AResourceURI,
  AMode: string; const ADefaultValue: Variant): Variant;
var
  LAllResult: Variant;
begin
  Assert(AUserId <> '');
  Assert(AMode <> '');

  // Empty URIs identify nameless objects, access to which is always granted
  // and we don't even need to log it.
  if AResourceURI = '' then
    Result := ACV_TRUE
  else
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
    if Assigned(FLogWriter) then
      WriteLog(AUserId, AResourceURI, AMode, EFVarToStr(ADefaultValue), EFVarToStr(Result));
  end;
end;

procedure TKAccessController.WriteLog(const AUserId, AResourceURI,
  AMode, ADefaultValue, AResult: string);

  function Delimit(const AString: string): string;
  begin
    if FLogDelimiter <> '' then
      Result := AnsiQuotedStr(AString, FlogDelimiter[1])
    else
      Result := AString;
  end;

begin
  Assert(Assigned(FLogWriter));

  if FLogHeader then
  begin
      FLogWriter.WriteLine(
        Delimit('DateTime') + FLogSeparator +
        Delimit('UserId') + FLogSeparator +
        Delimit('ResourceURI') + FLogSeparator +
        Delimit('Mode') + FLogSeparator +
        Delimit('DefaultValue') + FLogSeparator +
        Delimit('Result'));
    FLogHeader := False;
  end;
  FLogWriter.WriteLine(
    Delimit(DateTimeToStr(Now)) + FLogSeparator +
    Delimit(AUserId) + FLogSeparator +
    Delimit(AResourceURI) + FLogSeparator +
    Delimit(AMode) + FLogSeparator +
    Delimit(IfThen(ADefaultValue = '', '<null>', ADefaultValue)) + FLogSeparator +
    Delimit(IfThen(AResult = '', '<null>', AResult)));
end;

procedure TKAccessController.Init;
begin
  InternalInit;
end;

class function TKAccessController.InternalGetClassId: string;
begin
  Result := StripPrefixAndSuffix(inherited InternalGetClassId, 'K', 'AccessController');
end;

procedure TKAccessController.InternalInit;
var
  LLogFileName: string;
begin
  FreeAndNil(FLogWriter);
  LLogFileName := Config.GetExpandedString('Log/FileName');
  if LLogFileName <> '' then
  begin
    if not DirectoryExists(ExtractFilePath(LLogFileName)) then
      raise EKError.CreateFmt('Cannot create Access Controller log file %s. Directory does not exist.', [LLogFileName]);

    FLogWriter := TStreamWriter.Create(LLogFileName, FileExists(LLogFileName));
    FLogSeparator := Config.GetExpandedString('Log/FieldSeparator', #9);
    FLogDelimiter := Config.GetExpandedString('Log/FieldDelimiter', '');
    FLogHeader := Config.GetBoolean('Log/IncludeHeader', True) and (GetFileSize(LLogFileName) <= 0);
  end;
end;

function TKAccessController.IsAccessGranted(const AUserId, AResourceURI,
  AMode: string): Boolean;
begin
  Result := GetAccessGrantValue(AUserId, AResourceURI, AMode, Null) = ACV_TRUE;
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
  TKAccessControllerRegistry.Instance.RegisterClass('Null', TKNullAccessController);

finalization
  TKAccessControllerRegistry.Instance.UnregisterClass('Null');

end.
