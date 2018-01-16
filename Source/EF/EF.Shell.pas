unit EF.Shell;

interface

{
  Deletes all files in APath using the Windows shell API.
}
procedure ShellDeleteAllFiles(const APath: string);

{
  Opens a document with the default application synchronously (AWait = True)
  or Asynchronously (AWait = False).
  If AWait is True, the function waits that the launched process finishes,
  and returns the process' exit code (or -1 in case of errors).
  If AWait is False, the function returns 0 if the call succeeds or -1 in case
  of errors.
}
function OpenDocument(const AFileName: string; const AWait: Boolean = False): Integer;

implementation

uses
  SysUtils, Windows, ShellAPI,
  EF.Localization;

procedure ShellDeleteAllFiles(const APath: string);
var
  LFileOpStruct: TSHFileOpStruct;
begin
  FillChar(LFileOpStruct, SizeOf(LFileOpStruct), 0);
  with LFileOpStruct do begin
    Wnd := 0;
    wFunc := FO_DELETE;
    pFrom := PChar(APath + '*.*'#0);
    fFlags := FOF_NOCONFIRMATION + FOF_SILENT;
  end;
  if ShFileOperation(LFileOpStruct) <> 0 then
    raise Exception.CreateFmt(_('Error deleting files from folder "%s".'), [APath]);
end;

function OpenDocument(const AFileName: string; const AWait: Boolean = False): Integer;
var
  LExecInfo: TShellExecuteInfo;
  LReturnValue: Boolean;
  LUnsignedResult: Cardinal;
begin
  FillChar(LExecInfo, SizeOf(LExecInfo), #0);
  LExecInfo.cbSize := SizeOf(LExecInfo);
  LExecInfo.fMask := SEE_MASK_NOCLOSEPROCESS + SEE_MASK_NOASYNC;
  LExecInfo.lpVerb := PChar('open');
  LExecInfo.nShow := SW_SHOW;
  LExecInfo.lpFile := PChar(AFileName);

  LReturnValue := ShellExecuteEx(@LExecInfo);

  if LReturnValue then
  begin
    if AWait then
    begin
      WaitForSingleObject(LExecInfo.hProcess, INFINITE);
      GetExitCodeProcess(LExecInfo.hProcess, LUnsignedResult);
      Result := LUnsignedResult;
    end
    else
      Result := 0;
    CloseHandle(LExecInfo.hProcess);
  end
  else
    Result := -1;
end;

end.
