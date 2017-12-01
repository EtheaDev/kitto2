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

unit Kitto.Web.Engine;

interface

uses
  SysUtils
  , Generics.Collections
  , EF.ObserverIntf
  , Kitto.Web.Request
  , Kitto.Web.Response
  , Kitto.Web.Routes
  , Kitto.Web.Session
  , Kitto.Web.URL
  ;

type
  /// <summary>
  ///  Kitto engine route. Handles sub-routes (such as the application route)
  ///  and manages a list of active sessions. Also keeps the current session
  ///  in TKWebSession updated. It is normally embedded in a TKWebServer but can
  ///  be used as-is (for example inside an ISAPI dll or Apache module).
  /// </summary>
  TKWebEngine = class(TKWebRouteList)
  private
    FSessions: TObjectDictionary<string, TKWebSession>;
    function GetSession(const ASessionId: string): TKWebSession;
  protected
    procedure BeforeHandleRequest(const ARequest: TKWebRequest; const AResponse: TKWebResponse;
      const AURL: TKWebURL; var AIsAllowed: Boolean); override;
    procedure AfterHandleRequest(const ARequest: TKWebRequest;
      const AResponse: TKWebResponse; const AURL: TKWebURL); override;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
  public
    property Sessions[const ASessionId: string]: TKWebSession read GetSession;
    function GetSessions: TArray<TKWebSession>;
    function AddNewSession(const ASessionId: string): TKWebSession;
    function RecreateSession(const ASessionId: string): TKWebSession;
    procedure RemoveSession(const ASessionId: string);
  end;

implementation

uses
  StrUtils
  , Classes
  {$IFDEF MSWINDOWS}
  , ComObj
  , ActiveX
  {$ENDIF}
  , IOUtils
  , EF.DB
  , EF.Logger
  , EF.StrUtils
  , Kitto.Config
  , Kitto.Web.Types
  ;

{ TKWebEngine }

procedure TKWebEngine.AfterConstruction;
begin
  inherited;
  FSessions := TObjectDictionary<string, TKWebSession>.Create([doOwnsValues]);
end;

destructor TKWebEngine.Destroy;
begin
  FreeAndNil(FSessions);
  inherited;
end;

procedure TKWebEngine.BeforeHandleRequest(const ARequest: TKWebRequest;
  const AResponse: TKWebResponse; const AURL: TKWebURL; var AIsAllowed: Boolean);
begin
  {$IFDEF MSWINDOWS}
  if EF.DB.IsCOMNeeded then
    OleCheck(CoInitialize(nil));
  {$ENDIF}
  inherited;
end;

procedure TKWebEngine.AfterHandleRequest(const ARequest: TKWebRequest;
  const AResponse: TKWebResponse; const AURL: TKWebURL);
begin
  inherited;
  {$IFDEF MSWINDOWS}
  if EF.DB.IsCOMNeeded then
    CoUninitialize;
  {$ENDIF}
end;

function TKWebEngine.GetSession(const ASessionId: string): TKWebSession;
begin
  Result := FSessions.Items[ASessionId];
end;

function TKWebEngine.GetSessions: TArray<TKWebSession>;
begin
  MonitorEnter(FSessions);
  try
    Result := FSessions.Values.ToArray;
  finally
    MonitorExit(FSessions);
  end;
end;

function TKWebEngine.AddNewSession(const ASessionId: string): TKWebSession;
begin
  Assert(ASessionId <> '');

  Result := TKWebSession.Create(ASessionId);
  MonitorEnter(FSessions);
  try
    FSessions.Add(ASessionId, Result);
    TKWebSession.Current := Result;
  finally
    MonitorExit(FSessions);
  end;
end;

function TKWebEngine.RecreateSession(const ASessionId: string): TKWebSession;
begin
  MonitorEnter(FSessions);
  try
    Result := TKWebSession.Create(ASessionId);
    FSessions[ASessionId] := Result;
    TKWebSession.Current := Result;
  finally
    MonitorExit(FSessions);
  end;
end;

procedure TKWebEngine.RemoveSession(const ASessionId: string);
begin
  Assert(ASessionId <> '');

  MonitorEnter(FSessions);
  try
    FSessions.Remove(ASessionId);
    TKWebSession.Current := nil;
  finally
    MonitorExit(FSessions);
  end;
end;

end.

