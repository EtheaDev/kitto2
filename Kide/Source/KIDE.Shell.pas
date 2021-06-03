{-------------------------------------------------------------------------------
   Copyright 2012-2021 Ethea S.r.l.

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
