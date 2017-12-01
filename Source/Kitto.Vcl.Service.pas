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

unit Kitto.Vcl.Service;

interface

uses
  Windows
  , Messages
  , SysUtils
  , Classes
  , Vcl.SvcMgr
  , EF.Logger
  , Kitto.Types
  , Kitto.Web.Server
  , Kitto.Web.Application
  ;

type
  TKServiceLogEndpoint = class(TEFLogEndpoint)
  private
    FOnLog: TKLogEvent;
  protected
    procedure DoLog(const AString: string); override;
  public
    property OnLog: TKLogEvent read FOnLog write FOnLog;
  end;

  TKService = class(TService)
    procedure ServiceStart(Sender: TService; var Started: Boolean);
    procedure ServiceShutdown(Sender: TService);
    procedure ServiceCreate(Sender: TObject);
    procedure ServiceDestroy(Sender: TObject);
    procedure ServiceStop(Sender: TService; var Stopped: Boolean);
    procedure ServiceAfterInstall(Sender: TService);
  private
    FServer: TKWebServer;
    FLogEndPoint: TKServiceLogEndpoint;
    procedure DoLog(const AString: string);
    procedure SetDescription(const ADescription: string);
  public
    function GetServiceController: TServiceController; override;
  end;

var
  KService: TKService;

implementation

{$R *.dfm}

uses
  Winapi.WinSvc
  , Kitto.Vcl.Start
  ;

procedure ServiceController(CtrlCode: DWord); stdcall;
begin
  KService.Controller(CtrlCode);
end;

{ TKService }

procedure TKService.SetDescription(const ADescription: string);
var
  LSCManager: SC_HANDLE;
  LService: SC_HANDLE;
  LDescription: SERVICE_DESCRIPTION;
begin
  LSCManager := OpenSCManager(nil, nil, SC_MANAGER_ALL_ACCESS);
  if LSCManager <> 0 then
  begin
    LService := OpenService(LSCManager, PChar(Name), STANDARD_RIGHTS_REQUIRED or SERVICE_CHANGE_CONFIG);
    if LService <> 0 then
    begin
      LDescription.lpDescription := PChar(ADescription);
      ChangeServiceConfig2(LService, SERVICE_CONFIG_DESCRIPTION, @LDescription);
      CloseServiceHandle(LService);
    end;
    CloseServiceHandle(LSCManager);
  end;
end;

function TKService.GetServiceController: TServiceController;
begin
  Result := ServiceController;
end;

procedure TKService.ServiceAfterInstall(Sender: TService);
begin
  SetDescription(TKStart.ServiceDescription);
end;

procedure TKService.ServiceCreate(Sender: TObject);
begin
  FServer := TKWebServer.Create(nil);
  FServer.Engine.AddRoute(TKWebApplication.Create);

  FLogEndPoint := TKServiceLogEndpoint.Create;
  FLogEndPoint.OnLog := DoLog;
end;

procedure TKService.ServiceDestroy(Sender: TObject);
begin
  FServer.Active := False;
  FreeAndNil(FServer);
  FreeAndNil(FLogEndPoint);
end;

procedure TKService.ServiceShutdown(Sender: TService);
begin
  TEFLogger.Instance.LogDetailed('Service shutdown. Turning off web server...');
  FServer.Active := False;
  TEFLogger.Instance.LogDetailed('Service shutdown. Web server off.');
end;

procedure TKService.ServiceStart(Sender: TService; var Started: Boolean);
begin
  TEFLogger.Instance.LogDetailed('Service start. Updating description...');
  SetDescription(TKStart.ServiceDescription);
  TEFLogger.Instance.LogDetailed('Service start. Turning on web server...');
  FServer.Active := True;
  TEFLogger.Instance.LogDetailed('Service start. Web server on.');
end;

procedure TKService.ServiceStop(Sender: TService; var Stopped: Boolean);
begin
  TEFLogger.Instance.LogDetailed('Service stop. Turning off web server...');
  FServer.Active := False;
  TEFLogger.Instance.LogDetailed('Service stop. Web server off.');
end;

procedure TKService.DoLog(const AString: string);
begin
  LogMessage(AString);
end;

{ TKServiceLogEndpoint }

procedure TKServiceLogEndpoint.DoLog(const AString: string);
begin
  if Assigned(FOnLog) then
    FOnLog(AString);
end;

end.
