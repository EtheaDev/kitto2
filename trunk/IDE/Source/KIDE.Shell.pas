unit KIDE.Shell;

interface

///	<summary>
///	  <para>Opens a document with the default application synchronously (AWait
///	  = True) or Asynchronously (AWait = False).</para>
///	  <para>If AWait is True, the function waits that the launched process
///	  finishes, and returns the process' exit code (or -1 in case of
///	  errors).</para>
///	  <para>If AWait is False, the function returns 0 if the call succeeds or
///	  -1 in case of errors.</para>
///	</summary>
function OpenDocument(const AFileName: string; const AWait: Boolean = False): Integer;

///	<summary>
///	  <para>Opens a document with the default edit application synchronously
///	  (AWait = True) or Asynchronously (AWait = False).</para>
///	  <para>If AWait is True, the function waits that the launched process
///	  finishes, and returns the process' exit code (or -1 in case of
///	  errors).</para>
///	  <para>If AWait is False, the function returns 0 if the call succeeds or
///	  -1 in case of errors.</para>
///	</summary>
function EditDocument(const AFileName: string; const AWait: Boolean = False): Integer;

implementation

uses
  ShellAPI, Windows;

function InternalOpenDocument(const AFileName: string; const AVerb: string;
  const AWait: Boolean = False): Integer;
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

function OpenDocument(const AFileName: string; const AWait: Boolean = False): Integer;
begin
  Result := InternalOpenDocument(AFileName, 'open', AWait);
end;

function EditDocument(const AFileName: string; const AWait: Boolean = False): Integer;
begin
  Result := InternalOpenDocument(AFileName, 'edit', AWait);
end;

end.
