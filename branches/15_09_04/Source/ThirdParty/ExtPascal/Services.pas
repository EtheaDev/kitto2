{
Slim Services and EventLog support for Windows
Author: Wanderlan Santos dos Anjos, wanderlan.anjos@gmail.com
Date: jun-2008
License: <extlink http://www.opensource.org/licenses/bsd-license.php>BSD</extlink>
}
unit Services;

interface

uses
  {$IFDEF FPC}Windows{$ELSE}WinSvc{$ENDIF}, Classes;

{$R *.res}

type
	TFuncBool  = function : boolean; // Procedural type to stop and start functions, used in <link TService.Run, Run> method.
  TEventType = (EventError = 1, EventWarning = 2, EventInformation = 4); // Event severity

  // Implements Windows Service with <link TService.ReportEventLog, report EventLog> support
  TService = class
  private
  	FName       : pchar;
    FDescription,
	  FParamStr   : string;
    FManager,
    FService    : SC_Handle;
    FTimeout,
    FExitCode,
	  FParamCount : integer;
	  FSource     : THandle;
	  FStatus     : TServiceStatus;
	  FStopEvent  : THandle;
    FReportStartStop : boolean;
	  FStatusHandle    : Service_Status_Handle;
    FServiceThreads  : array[0..10] of TThread;
    FMaxThreads	     : integer;
    FServiceBegin,
    FServiceEnd		   : TFuncBool;
    procedure StopNow;
    function GetName : string;
    function ReportNoError(Estado : integer) : boolean;
    function ReportServiceStatus(CurrentState, Win32ExitCode, CheckPoint, WaitHint: integer): boolean;
  public
    constructor Create(ServiceName : string; Description : string = '');
    destructor Destroy; override;
    function GetServiceError: integer;
    function GetServiceErrorMessage: string;
    function GetState : cardinal;
    function  Install : boolean;
    function  Uninstall : boolean;
    procedure Insert(Exec: string);
    procedure Delete;
    function  Run(ServThreads : array of TThread; ServBegin : TFuncBool = nil; ServEnd : TFuncBool = nil) : boolean;
    function  Exists : boolean;
    function  Stop : integer;
    function  Start : integer;
    function  ReportStart : boolean;
    function  ReportStop  : boolean;
    procedure ReportEventLog(EventType : TEventType; EventCode : word; Message : string);
    procedure Reset;
    property Timeout : integer read FTimeout write FTimeout; // Time before to generate an error. Default 20000 milliseconds
    property ExitCode : integer read FExitCode write FExitCode; // Exit code to return to Service Manager
    property Name : string read GetName; // Service Name
    property ParamStr : string read FParamStr; // Parameter list passed when the service was started
    property ParamCount : integer read FParamCount; // Number of parameters passed when the service was started
  end;

var
  Service : TService; // Global var, use it to initialize a service

implementation

uses
	{$IFNDEF FPC}Windows,{$ENDIF} SysUtils, Registry;

function TService.GetName : string; begin
  Result := string(FName);
end;

// Closes service handle
procedure TService.Reset; begin
  CloseServiceHandle(FService);
  FService := 0;
end;

// Returns if service is initialized
function TService.Exists : boolean; begin
	Result := FService <> 0;
end;

{
Installs a service.
@param Exec Executable file with path
@exception RaiseLastOSError if not succeded
@see Install
@see Delete
}
procedure TService.Insert(Exec : string); begin
  FService := CreateService(FManager, FName, FName, SERVICE_ALL_ACCESS, SERVICE_WIN32_OWN_PROCESS, SERVICE_AUTO_START,
                            SERVICE_ERROR_NORMAL, pchar(Exec), nil, nil, nil, nil, nil);
  if not Exists then RaiseLastOSError;
  with TRegistry.Create do begin
    Access  := KEY_ALL_ACCESS;
    RootKey := HKey_Local_Machine;
    OpenKey('\SYSTEM\CurrentControlSet\Services\EventLog\Application\' + FName, true);
    WriteString('EventMessageFile', Exec);
    OpenKey('\SYSTEM\CurrentControlSet\Services\' + FName, true);
    WriteString('Description', FDescription);
    Free;
  end;
end;

{
Installs a service using command line. In command line use \<application\> -INSTALL to install a service
@return True if succeded else False
@see Insert
@see Uninstall
}
function TService.Install : boolean; begin
  if FindCmdLineSwitch('INSTALL', ['-', '/'], true) and (FService = 0) then begin
    Insert(system.ParamStr(0));
    Result := true;
  end
  else
    Result := false
end;

{
Uninstalls a service.
@exception RaiseLastOSError if not succeded
@see Insert
@see Uninstall
}
procedure TService.Delete; begin
  if not Exists then RaiseLastOSError;
  if not DeleteService(FService) then RaiseLastOSError;
  with TRegistry.Create do begin
    Access  := KEY_ALL_ACCESS;
    RootKey := HKey_Local_Machine;
    DeleteKey('\SYSTEM\CurrentControlSet\Services\EventLog\Application\' + FName);
    Free;
  end;
end;

{
Uninstalls a service using command line. In command line use \<application\> -UNINSTALL to uninstall a service
@return True if succeded else False
@see Delete
@see Install
}
function TService.Uninstall : boolean; begin
  if FindCmdLineSwitch('UNINSTALL', ['-', '/'], true) then begin
    Delete;
    Result := true;
  end
  else
    Result := false
end;

// Returns last error code
function TService.GetServiceError : integer; begin
  Result := GetLastError;
  if Result = 0 then Result := -1
end;

// Returns last error message
function TService.GetServiceErrorMessage : string; begin
  Result := SysErrorMessage(GetServiceError)
end;

{
Stops the service
@return 0 if succeeded else last error code
}
function TService.Stop : integer; begin
	Result := 0;
  if Exists then begin
		if not ControlService(FService, SERVICE_CONTROL_STOP, FStatus) then Result := GetLastError;
	end
	else
		Result := GetServiceError;
end;

{
Starts the service
@return 0 if succeeded else last error code
}
function TService.Start : integer;
const
	Param : PAnsiChar = nil;
begin
	Result := 0;
  if FService = 0 then FService := OpenService(FManager, FName, SERVICE_ALL_ACCESS);
	if Exists then begin
	  if not StartServiceA(FService, 0, Param) then Result := GetServiceError;
  end
	else
		Result := GetServiceError;
end;

function TService.GetState : cardinal; begin
  if QueryServiceStatus(FService, FStatus) then
    Result := FStatus.dwCurrentState
  else
    Result := 77;
end;

{
Writes an event log.
@param EventType
@param EventCode User code
@param Message User message
}
procedure TService.ReportEventLog(EventType : TEventType; EventCode : word; Message : string);
var
	Mensagem : pchar;
begin
	Mensagem := pchar(Message);
	ReportEvent(FSource, word(EventType), 1000 + EventCode, 0, nil, 1, 0, @Mensagem, nil);
end;

// StopNow can be used within the service to stop the service
procedure TService.StopNow; begin
  SetLastError(0);
  SetEvent(FStopEvent)
end;

function TService.ReportServiceStatus(CurrentState, Win32ExitCode, CheckPoint, WaitHint : integer) : boolean; begin
  SetLastError(0);
	with FStatus do begin
		dwServiceType := SERVICE_WIN32_OWN_PROCESS;
		dwServiceSpecificExitCode := 0;
		// Desabilita requisições até o serviço estar startado
		if CurrentState = SERVICE_START_PENDING then
			dwControlsAccepted := 0
		else
			dwControlsAccepted := SERVICE_ACCEPT_STOP + SERVICE_ACCEPT_PAUSE_CONTINUE;
		dwCurrentState := CurrentState;
		dwCheckPoint 	 := CheckPoint;
		dwWaitHint     := WaitHint;
		if ExitCode = 0 then
			dwWin32ExitCode := Win32ExitCode
		else begin
			dwWin32ExitCode := ERROR_SERVICE_SPECIFIC_ERROR;
			dwServiceSpecificExitCode := ExitCode;
		end;
		// Manda o status do service para o service manager.
		Result := SetServiceStatus(FStatusHandle, FStatus);
		if not Result then StopNow;
	end;
end;

function TService.ReportNoError(Estado : integer) : boolean; begin
	Result := ReportServiceStatus(Estado, NO_ERROR, 0, 0)
end;

// Reports that the service is in start pending status. Use it when to initialize a service.
function TService.ReportStart : boolean;
const
	ChkPoint : integer = 0;
begin
  Result := false;
  if FReportStartStop and Exists then begin
    inc(ChkPoint);
    Result := ReportServiceStatus(SERVICE_START_PENDING, NO_ERROR, ChkPoint, Timeout);
  end;
end;

// Reports that the service is in stop pending status. Use it when to stop a service
function TService.ReportStop : boolean;
const
	ChkPoint : integer = 0;
begin
  Result := false;
  if FReportStartStop and Exists then begin
    inc(ChkPoint);
    Result := ReportServiceStatus(SERVICE_STOP_PENDING, NO_ERROR, ChkPoint, Timeout);
  end;
end;

// Is called by Windows Server Manager
procedure ServController(Command : integer); stdcall;
var
	I : integer;
begin
  with Service do
    case Command of
      SERVICE_CONTROL_PAUSE: if FStatus.dwCurrentState = SERVICE_RUNNING then begin
        for I := 0 to FMaxThreads do
          FServiceThreads[I].Suspend;
        ReportNoError(SERVICE_PAUSED);
      end;
      SERVICE_CONTROL_CONTINUE: if FStatus.dwCurrentState = SERVICE_PAUSED then begin
        for I := 0 to FMaxThreads do
          FServiceThreads[I].Resume;
        ReportNoError(SERVICE_RUNNING);
      end;
      SERVICE_CONTROL_STOP: begin
        FReportStartStop := true;
        ReportStop;
        // Request all threads to terminate
        for I := 0 to FMaxThreads do
          FServiceThreads[I].Terminate;
        // Wait to termination and free them
        for I := 0 to FMaxThreads do
          with FServiceThreads[I] do begin
            WaitFor;
            Free;
          end;
        ReportStop;
        StopNow;
      end;
    else
      ReportNoError(SERVICE_RUNNING);
    end;
end;

{
Starts the service, telling Service Manager each step of the process,
then resumes the service threads, waits the stop event and back to
StartServiceCtrlDispatcher in RunService
}
procedure ServiceMain(ArgC : integer; ArgV : pchar); stdcall;
var
	I : integer;
  InitOk : boolean;
begin
  with Service do begin
    FParamCount := ArgC;
    FParamStr 	:= strpas(ArgV);
    SetLastError(0);
    FStatusHandle := RegisterServiceCtrlHandler(FName, @ServController);
    if FStatusHandle <> 0 then begin
      if ReportStart then begin
        // Cria o evento que irá sinalizar o fim do service
        SetLastError(0);
        FStopEvent := CreateEvent(nil, true, false, nil);
        if FStopEvent <> 0 then begin
          // Roda a rotina de inicialização do Service
          InitOk := true;
          if @FServiceBegin <> nil then InitOk := FServiceBegin;
          if InitOk then begin
            ReportStart;
            FReportStartStop := false;
            // Starta as threads do service
            for I := 0 to FMaxThreads do
              FServiceThreads[I].Resume;
            ReportEventLog(EventInformation, 0, 'Started');
            if ReportNoError(SERVICE_RUNNING) then
              // Espera indefinidamente até o StopEvent ocorrer
              WaitForSingleObject(FStopEvent, INFINITE);
            FReportStartStop := true;
            ReportStop;
            // Desaloca as Threads
            for I := 0 to FMaxThreads do
              FServiceThreads[I].Terminate;
            ReportEventLog(EventInformation, 1, 'Stopped');
            ReportStop;
            SetLastError(0);
            if @FServiceEnd <> nil then FServiceEnd; // Roda a rotina de finalização
          end;
          CloseHandle(FStopEvent);
        end;
      end;
      ReportServiceStatus(SERVICE_STOPPED, GetLastError, 0, 0);
    end;
  end;
end;

{
Runs a service. Calls StartServiceCtrlDispatcher to register a main service thread.
When the API returns, the service was stopped, then halt.
@param
@param ServBegin Function called before to start the service. It should return true if initializing was Ok.
@param ServEnd Function called after to stop the service.
}
function TService.Run(ServThreads : array of TThread; ServBegin  : TFuncBool = nil; ServEnd : TFuncBool = nil) : boolean;
var
	ServTable : array[0..1] of TServiceTableEntry;
	I : integer;
begin
	FServiceBegin := ServBegin;
	FServiceEnd   := ServEnd;
	FMaxThreads   := high(ServThreads);
	for I := 0 to FMaxThreads do
		FServiceThreads[I] := ServThreads[I];
	fillchar(ServTable, sizeof(ServTable), 0);
	with ServTable[0] do begin
		lpServiceName := FName;
		lpServiceProc := @ServiceMain
	end;
  SetLastError(0);
	Result := StartServiceCtrlDispatcher({$IFDEF FPC}@{$ENDIF}ServTable[0])
end;

{
Creates a new service, but not installs it. Use <link TService.Insert> to install.
@param ServiceName to show in Service Manager
@param Description to show in Service Manager
}
constructor TService.Create(ServiceName : string; Description : string = ''); begin
  inherited Create;
  FName        := pchar(ServiceName);
  FDescription := Description;
  FSource      := OpenEventLog(nil, FName);
  FManager     := OpenSCManager(nil, nil, SC_MANAGER_ALL_ACCESS);
  if FManager = 0 then RaiseLastOSError;
  FService  := OpenService(FManager, FName, SERVICE_ALL_ACCESS);
  FTimeout  := 20000;
  FReportStartStop := true;
end;

// Frees a service but not uninstalls it. Use <link TService.Delete> method to uninstall.
destructor TService.Destroy; begin
  CloseServiceHandle(FService);
	CloseEventLog(FSource);
  CloseServiceHandle(FManager);
end;

end.
