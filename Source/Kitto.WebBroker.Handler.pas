{-------------------------------------------------------------------------------
   Copyright 2012-2017 Ethea S.r.l.

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

unit Kitto.WebBroker.Handler;

{$I Kitto.Defines.inc}

interface

uses
  HTTPApp
  , Kitto.Web.Engine
  ;

type
  /// <summary>
  ///  Singleton object that maintains a global instance of a Kitto engine.
  ///  Together with TKWebModule, provides for zero-code implementation of
  ///  Webbroker-based Kitto apps (just create a standard WebBroker app and
  ///  add the Kitto.WebBroker.WebModule unit to the uses clause).
  /// </summary>
  TKWebBrokerHandler = class
  private
    FEngine: TKWebEngine;
    class var FCurrent: TKWebBrokerHandler;
    class function GetCurrent: TKWebBrokerHandler; static;
    function GetEngine: TKWebEngine;
  public
    class constructor Create;
    class destructor Destroy;
    procedure AfterConstruction; override;
    destructor Destroy; override;
  public
    class property Current: TKWebBrokerHandler read GetCurrent;

    property Engine: TKWebEngine read GetEngine;
  end;

implementation

uses
  SysUtils
  ;

{ TKWebBrokerHandler }

procedure TKWebBrokerHandler.AfterConstruction;
begin
  inherited;
  FEngine := TKWebEngine.Create;
end;

class constructor TKWebBrokerHandler.Create;
begin
  FCurrent := TKWebBrokerHandler.Create;
end;

class destructor TKWebBrokerHandler.Destroy;
begin
  FreeAndNil(FCurrent);
end;

destructor TKWebBrokerHandler.Destroy;
begin
  FreeAndNil(FEngine);
  inherited;
end;

class function TKWebBrokerHandler.GetCurrent: TKWebBrokerHandler;
begin
  Result := FCurrent;
end;

function TKWebBrokerHandler.GetEngine: TKWebEngine;
begin
  if not Assigned(FEngine) then
  begin
    FEngine := TKWebEngine.Create;
    FEngine.Active := True;
  end;
  Result := FEngine;
end;

end.
