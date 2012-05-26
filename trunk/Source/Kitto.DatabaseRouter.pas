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
///	  Defines the base database router and related classes and services.
///	  Database routers allow to use several databases at the same time in a
///   single application. Models, views and specific command texts can be
///   routed to different databases.
///	</summary>
unit Kitto.DatabaseRouter;

interface

{$I Kitto.Defines.inc}

uses
  Classes,
  EF.Macros, EF.Types, EF.Classes, EF.Tree;

type
  ///	<summary>
  ///	  <para>Abstract base database router. A database router knows how to
  ///	  route requests to a database among many that can be defined in the
  ///	  config file.</para>
  ///	  <para>Applications can define, register and use custom routers. The
  ///	  default router simply routes requests to the database whose name is
  ///	  specified in its params.</para>
  ///	</summary>
  TKDatabaseRouter = class(TEFComponent)
  protected
    function InternalGetDatabaseName(const ACallerContext: TObject;
      const AParams: TEFTree): string; virtual; abstract;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
  public
    ///	<summary>Returns a database name suitable for the specified context and
    ///	params.</summary>
    function GetDatabaseName(const ACallerContext: TObject;
      const AParams: TEFTree): string;
  end;
  TKDatabaseRouterClass = class of TKDatabaseRouter;

  ///	<summary>A database router that routes everything to the database
  ///	specified in its DatabaseName param.</summary>
  ///	<remarks>If no DatabaseName param is specified, 'Main' is
  ///	assumed.</remarks>
  TKStaticDatabaseRouter = class(TKDatabaseRouter)
  protected
    function InternalGetDatabaseName(const ACallerContext: TObject;
      const AParams: TEFTree): string; override;
  end;

  ///	<summary>This class holds a list of registered database router
  ///	classes.</summary>
  TKDatabaseRouterRegistry = class(TEFRegistry)
  private
    class var FInstance: TKDatabaseRouterRegistry;
    class function GetInstance: TKDatabaseRouterRegistry; static;
  public
    class destructor Destroy;
    class property Instance: TKDatabaseRouterRegistry read GetInstance;

    ///	<summary>Adds a database router class to the registry.</summary>
    procedure RegisterClass(const AId: string; const AClass: TKDatabaseRouterClass);
  end;

  ///	<summary>Creates database routers by Id.</summary>
  TKDatabaseRouterFactory = class(TEFFactory)
  private
    class var FInstance: TKDatabaseRouterFactory;
    class function GetInstance: TKDatabaseRouterFactory; static;
  public
    class destructor Destroy;
    class property Instance: TKDatabaseRouterFactory read GetInstance;

    ///	<summary>Creates and returns an instance of the database router class
    ///	identified by AClassId. Raises an exception if said class is not
    ///	registered.</summary>
    ///	<param name="AClassId">Passing '' uses the default database
    ///	router.</param>
    function CreateObject(const AClassId: string): TKDatabaseRouter;

    ///	<summary>Shortcut method that creates a database router, returns the
    ///	result of its GetDatabaseName method and frees it
    ///	automatically.</summary>
    ///	<param name="AClassId">Passing '' uses the default database
    ///	router.</param>
    function GetDatabaseName(const AClassId: string;
      ACallerContext: TObject; const AParams: TEFTree): string;
  end;

implementation

uses
  SysUtils,
  EF.StrUtils, EF.Localization;

{ TKDatabaseRouterRegistry }

class destructor TKDatabaseRouterRegistry.Destroy;
begin
  FreeAndNil(FInstance);
end;

class function TKDatabaseRouterRegistry.GetInstance: TKDatabaseRouterRegistry;
begin
  if FInstance = nil then
    FInstance := TKDatabaseRouterRegistry.Create;
  Result := FInstance;
end;

procedure TKDatabaseRouterRegistry.RegisterClass(const AId: string; const AClass: TKDatabaseRouterClass);
begin
  inherited RegisterClass(AId, AClass);
end;

{ TKDatabaseRouterFactory }

function TKDatabaseRouterFactory.CreateObject(const AClassId: string): TKDatabaseRouter;
begin
  if AClassId = '' then
    Result := inherited CreateObject('Static') as TKDatabaseRouter
  else
    Result := inherited CreateObject(AClassId) as TKDatabaseRouter;
end;

class destructor TKDatabaseRouterFactory.Destroy;
begin
  FreeAndNil(FInstance);
end;

function TKDatabaseRouterFactory.GetDatabaseName(const AClassId: string;
  ACallerContext: TObject; const AParams: TEFTree): string;
var
  LRouter: TKDatabaseRouter;
begin
  LRouter := CreateObject(AClassId);
  try
    Result := LRouter.GetDatabaseName(ACallerContext, AParams);
  finally
    FreeAndNil(LRouter);
  end;
end;

class function TKDatabaseRouterFactory.GetInstance: TKDatabaseRouterFactory;
begin
  if FInstance = nil then
    FInstance := TKDatabaseRouterFactory.Create(TKDatabaseRouterRegistry.Instance);
  Result := FInstance;
end;

{ TKDatabaseRouter }

procedure TKDatabaseRouter.AfterConstruction;
begin
  inherited;
end;

destructor TKDatabaseRouter.Destroy;
begin
  inherited;
end;

function TKDatabaseRouter.GetDatabaseName(const ACallerContext: TObject;
  const AParams: TEFTree): string;
begin
  Result := InternalGetDatabaseName(ACallerContext, AParams);
end;

{ TKStaticDatabaseRouter }

function TKStaticDatabaseRouter.InternalGetDatabaseName(
  const ACallerContext: TObject; const AParams: TEFTree): string;
begin
  Result := AParams.GetExpandedString('DatabaseName', 'Main');
end;

initialization
  TKDatabaseRouterRegistry.Instance.RegisterClass('Static', TKStaticDatabaseRouter);

finalization
  TKDatabaseRouterRegistry.Instance.UnregisterClass('Static');

end.
