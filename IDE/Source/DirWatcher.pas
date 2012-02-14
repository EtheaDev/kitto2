unit DirWatcher;

interface

uses
  SysUtils,
  Classes,
  Windows,
  Messages;

const
  MAX_BUFFER = 65536;
  CM_DIRECTORY_EVENT = WM_USER + 4242;

type
  TFileNotifyInformation = record
    NextEntryOffset: DWORD;
    Action : DWORD;
    FileNameLength: DWORD;
    FileName: Array[0..MAX_PATH] Of WCHAR;
  end;
  PFileNotifyInformation = ^TFileNotifyInformation;

  TActionToWatch = (
                     awChangeFileName,
                     awChangeDirName,
                     awChangeAttributes,
                     awChangeSize,
                     awChangeLastWrite,
                     awChangeLastAccess,
                     awChangeCreation,
                     awChangeSecurity
                   );

  TActionsToWatch = set of TActionToWatch;

  TDirectoryAction = (daUnknown, daFileAdded, daFileRemoved, daFileModified, daFileRenamedOldName, daFileRenamedNewName);

  TDirectoryChangeEvent = procedure(Sender: TObject; Action: TDirectoryAction; FileName: AnsiString) of object;



  TDirectoryMonitorWorkerThread = class(TThread)
  private
    FWatchSubFolders: Boolean;
    FPathToWatch: AnsiString;
    FActionsToWatch: TActionsToWatch;
    FNotifyMask: DWORD;
    FDirHandle: THandle;
    FChangeHandle: THandle;
    FShutdownHandle: THandle;

    FBuffer: Pointer;
    FBufferLength: Cardinal;
    FOnDirectoryChange: TDirectoryChangeEvent;


    FNotifyFileName: AnsiString;
    FNotifyAction: TDirectoryAction;

    procedure DoOnDirectoryChange;
    function GetNotifyMask: DWORD;
    function GetNotifyAction(SystemAction: DWORD): TDirectoryAction;
  public
    property OnDirectoryChange: TDirectoryChangeEvent read FOnDirectoryChange write FOnDirectoryChange;

    constructor Create(const PathToWatch: AnsiString; ActionsToWatch: TActionsToWatch; WatchSubFolders: Boolean);
    destructor Destroy; override;

    procedure Execute; override;
    procedure ShutDown;
  end;

  TDirectoryMonitor = class
  private
    FDirectoryToWatch: AnsiString;
    FWorkerThread: TDirectoryMonitorWorkerThread;
    FOnDirectoryChange: TDirectoryChangeEvent;
    FOptions: TActionsToWatch;
    FWatchSubFolders: Boolean;
    FRunning: Boolean;

    procedure SetDirToWatch(Value: AnsiString);
    procedure DoOnDirectoryChange(Sender: TObject; Action: TDirectoryAction; FileName: AnsiString);
  public
    property WorkerThread: TDirectoryMonitorWorkerThread read FWorkerThread;
    property DirectoryToWatch: AnsiString read FDirectoryToWatch write SetDirToWatch;
  published
    property OnDirectoryChange: TDirectoryChangeEvent read FOnDirectoryChange write FOnDirectoryChange;
    property Options: TActionsToWatch read FOptions write FOptions;
    property WatchSubFolders: Boolean read FWatchSubFolders write FWatchSubFolders default True;

    procedure Start;
    procedure Stop;

    constructor Create;
    destructor Destroy; override;
  end;


implementation

{$region 'TDirectoryMonitorWorkerThread'}
constructor TDirectoryMonitorWorkerThread.Create(const PathToWatch: AnsiString;
                                                 ActionsToWatch: TActionsToWatch;
                                                 WatchSubFolders: Boolean);
begin
  inherited Create(True);
  FreeOnTerminate := false;

  FWatchSubFolders := WatchSubFolders;
  FPathToWatch := PathToWatch;
  FActionsToWatch := ActionsToWatch;
  FNotifyMask := GetNotifyMask;

  FChangeHandle := CreateEvent(nil, FALSE, FALSE, nil);
  FDirHandle := CreateFile(PChar(FPathToWatch),
                           FILE_LIST_DIRECTORY OR GENERIC_READ,
                           FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE,
                           nil,
                           OPEN_EXISTING,
                           FILE_FLAG_BACKUP_SEMANTICS or FILE_FLAG_OVERLAPPED,
                           0);

  FShutdownHandle := CreateEvent(nil, FALSE, FALSE, nil);

  GetMem(FBuffer, MAX_BUFFER);
  FBufferLength := MAX_BUFFER;
end;

destructor TDirectoryMonitorWorkerThread.Destroy;
begin
  if FDirHandle <> INVALID_HANDLE_VALUE then
    CloseHandle(FDirHandle);

  if FChangeHandle <> 0 then
    CloseHandle(FChangeHandle);

  if FShutdownHandle <> 0 then
    CloseHandle(FShutdownHandle);

  FreeMem(FBuffer, MAX_BUFFER);
  inherited Destroy;
end;

procedure TDirectoryMonitorWorkerThread.DoOnDirectoryChange;
begin
  if Assigned(FOnDirectoryChange) then
  try
    try
      if (FNotifyAction <> daUnknown)
      and (Trim(FNotifyFileName) <> EmptyStr) then
      try
        FOnDirectoryChange(Self, FNotifyAction, FNotifyFileName);
      except
      end;
    finally
      FNotifyAction := daUnknown;
      FNotifyFileName := EmptyStr;
    end;
  except
  end;
end;

function TDirectoryMonitorWorkerThread.GetNotifyAction(SystemAction: DWORD): TDirectoryAction;
begin
  case SystemAction of
    FILE_ACTION_ADDED: Result := daFileAdded;
    FILE_ACTION_REMOVED: Result := daFileRemoved;
    FILE_ACTION_MODIFIED: Result := daFileModified;
    FILE_ACTION_RENAMED_OLD_NAME: Result := daFileRenamedOldName;
    FILE_ACTION_RENAMED_NEW_NAME: Result := daFileRenamedNewName;
  else
    Result := daUnknown;
  end;
end;

procedure TDirectoryMonitorWorkerThread.Execute;
var
  bytesRead: DWORD;
  FNI: PFileNotifyInformation;
  nextOffset: DWORD;
  buffer: array[0 .. MAX_BUFFER - 1] of byte;
  overlap: TOverlapped;
  events: array[0..1] of THandle;
  waitResult: DWORD;
begin
  if FDirHandle <> INVALID_HANDLE_VALUE then
  begin
    FillChar(overlap, SizeOf(TOverlapped), 0);
    overlap.hEvent := fChangeHandle;

    events[0] := fChangeHandle;
    events[1] := fShutdownHandle;

    while not Terminated do
    begin
      FillChar(buffer, SizeOf(buffer), 0);
      if ReadDirectoryChangesW(
                                FDirHandle,
                                @buffer[0],
                                MAX_BUFFER,
                                True,
                                GetNotifyMask,
                                @bytesRead,
                                @overlap,
                                nil) then
      begin
        waitResult := WaitForMultipleObjects(2, @events[0], FALSE, INFINITE);
        if waitResult = WAIT_OBJECT_0 then
        begin
           FNI := @buffer[0];
          repeat
            nextOffset := FNI.NextEntryOffset;
            FNotifyFileName := WideCharLenToString(@FNI.FileName, FNI.FileNameLength);
            SetLength(FNotifyFileName, StrLen(PChar(FNotifyFileName)));
            FNotifyAction := GetNotifyAction(FNI.Action);
            Synchronize(DoOnDirectoryChange);

            PByte(FNI) := PByte(DWORD(FNI) + nextOffset);
          until (nextOffset = 0) or Terminated;
        end;
      end;
    end;
  end;
end;

function TDirectoryMonitorWorkerThread.GetNotifyMask: DWORD;
begin
  Result := 0;
  if awChangeFileName in FActionsToWatch then
    Result := Result or FILE_NOTIFY_CHANGE_FILE_NAME;
  if awChangeDirName in FActionsToWatch then
    Result := Result or FILE_NOTIFY_CHANGE_DIR_NAME;
  if awChangeSize in FActionsToWatch then
    Result := Result or FILE_NOTIFY_CHANGE_SIZE;
  if awChangeAttributes in FActionsToWatch then
      Result := Result or FILE_NOTIFY_CHANGE_ATTRIBUTES;
  if awChangeLastWrite in FActionsToWatch then
    Result := Result or FILE_NOTIFY_CHANGE_LAST_WRITE;
  if awChangeSecurity in FActionsToWatch then
    Result := Result or FILE_NOTIFY_CHANGE_SECURITY;
  if awChangeLastAccess in FActionsToWatch then
    Result := Result or FILE_NOTIFY_CHANGE_LAST_ACCESS;
  if awChangeCreation in FActionsToWatch then
    Result := Result or FILE_NOTIFY_CHANGE_CREATION;
end;

procedure TDirectoryMonitorWorkerThread.ShutDown;
begin
  Terminate;
  if FShutdownHandle <> 0 then
    SetEvent(FShutdownHandle);
end;
{$endregion 'TDirectoryMonitorWorkerThread'}

{$region 'TDirectoryMonitor'}
constructor TDirectoryMonitor.Create;
begin
  inherited Create;
  FDirectoryToWatch := EmptyStr;
  FWatchSubFolders := True;
  FWorkerThread := nil;
  FRunning := false;
  FOptions := [
                awChangeFileName,
                awChangeDirName,
                awChangeAttributes,
                awChangeSize,
                awChangeLastWrite,
                awChangeLastAccess,
                awChangeCreation,
                awChangeSecurity
              ];
end;

destructor TDirectoryMonitor.Destroy;
begin
  Stop;
  inherited Destroy;
end;

procedure TDirectoryMonitor.DoOnDirectoryChange(Sender: TObject; Action: TDirectoryAction; FileName: AnsiString);
begin
  if Assigned(FOnDirectoryChange) then
  try
    FOnDirectoryChange(Self, Action, FileName);
  except
  end;
end;

procedure TDirectoryMonitor.SetDirToWatch(Value: AnsiString);
var
  wasRunning: Boolean;
begin
  wasRunning := FRunning and (FWorkerThread <> nil) and Assigned(FWorkerThread);

  if wasRunning then
    Stop;

  FDirectoryToWatch := Trim(Value);

  if wasRunning then
    Start;
end;

procedure TDirectoryMonitor.Start;
begin
  if not FRunning and ((FWorkerThread = nil) or not Assigned(FWorkerThread)) then
  begin
    if (FOptions <> [])
    and (FDirectoryToWatch <> EmptyStr)
    and DirectoryExists(FDirectoryToWatch) then
    begin
      FWorkerThread := TDirectoryMonitorWorkerThread.Create(FDirectoryToWatch, FOptions, FWatchSubFolders);
      FWorkerThread.OnDirectoryChange := DoOnDirectoryChange;
      FWorkerThread.Resume;
      FRunning := true;
    end;

  end;
end;

procedure TDirectoryMonitor.Stop;
begin
  if FRunning and (FWorkerThread <> nil) and Assigned(FWorkerThread) then
  begin
    FWorkerThread.OnDirectoryChange := nil;
    FWorkerThread.ShutDown;
    try
      try
        FWorkerThread.WaitFor
      finally
        try
          FreeAndNil(FWorkerThread);
        except
        end;
        FRunning := false;
        FWorkerThread := nil;
      end;
    except
    end;
  end;
end;
{$endregion 'TDirectoryMonitor'}

end.

