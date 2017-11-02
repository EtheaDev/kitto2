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
  ;

type
  TKService = class(TService)
    procedure ServiceStart(Sender: TService; var Started: Boolean);
    procedure ServiceStop(Sender: TService; var Stopped: Boolean);
    procedure ServiceShutdown(Sender: TService);
  private
//    FThread: TKExtAppThread;
//    function CreateThread: TKExtAppThread;
    procedure StopAndFreeThread;
  public
    function GetServiceController: TServiceController; override;
  end;

var
  KService: TKService;

implementation

{$R *.dfm}

uses
  EF.Logger;

procedure ServiceController(CtrlCode: DWord); stdcall;
begin
  KService.Controller(CtrlCode);
end;

{ TKService }

procedure TKService.StopAndFreeThread;
begin
//  if Assigned(FThread) then
//  begin
//    FThread.Terminate;
//    FThread.WaitFor;
//    FreeAndNil(FThread);
//  end;
end;

//function TKExtService.CreateThread: TKExtAppThread;
//begin
//  Result := TKExtAppThread.Create(True);
//  Result.Configure;
//end;

function TKService.GetServiceController: TServiceController;
begin
  Result := ServiceController;
end;

procedure TKService.ServiceShutdown(Sender: TService);
begin
  TEFLogger.Instance.Log('Service shutdown.');
  StopAndFreeThread;
end;

procedure TKService.ServiceStart(Sender: TService; var Started: Boolean);
begin
//  TEFLogger.Instance.Log('Service start. Creating thread...');
//  FThread := CreateThread;
//  TEFLogger.Instance.Log('Starting thread...');
//  FThread.Start;
end;

procedure TKService.ServiceStop(Sender: TService; var Stopped: Boolean);
begin
  StopAndFreeThread;
end;

end.
