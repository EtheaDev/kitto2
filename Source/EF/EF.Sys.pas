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

unit EF.Sys;

interface

uses
  SysUtils
  , Classes
  ;

type
  TEFSys = class
  public
    /// <summary>
    ///  Returns the name of the currently logged on user.
    /// </summary>
    function GetUserName: string; virtual; abstract;

    /// <summary>
    ///  Executes an application synchronously and returns the process'
    ///  exit code (or <> 0 in case of errors).
    /// </summary>
    function ExecuteCommand(const AFileName: string): Integer; virtual; abstract;
  end;

var
  EFSys: TEFSys;

/// <summary>
///  Expands all environment variables found in AString and returns the
///  resulting string. It is a wrapper around the ExpandEnvironmentStrings API
///  function. Environment variable names should be enclosed between %
///  characters and are case-insensitive.
/// </summary>
procedure ExpandEnvironmentVariables(var AString: string);

/// <summary>
///  Tells whether APath is an absolute (True) or relative (False) path.
/// </summary>
function IsAbsolutePath(const APath: string): Boolean;

/// <summary>
///  Scans the command line and returns the value of the parameter that
///  follows the one with the specificed name (with leading - or /). If the
///  parameter is not found, ADefaultValue is returned.
/// </summary>
function GetCmdLineParamValue(const AParamName: string; const ADefaultValue: string = ''): string;

/// <summary>
///  Returns the size of the named file without opening the file. If the file
///  doesn't exist, returns -1.
/// </summary>
/// <remarks>
///  Currently this function doesn't support files bigger than 2 GBs.
/// </remarks>
function GetFileSize(const AFileName: string): Longint;

/// <summary>
///  Returns AFileName's OS timestamp.
/// </summary>
function GetFileDateTime(const AFileName: string): TDateTime;

/// <summary>
///  Returns a random file name which doesn't exists in APath and has
///  extension AExtension. Keeps generating random names until it finds a free
///  name.
/// </summary>
function GetUniqueFileName(const APath, AExtension: string): string; overload;

/// <summary>
///  Returns a Unique file name starting from ADefaultFileName.
///  If the file already exists adds random names until it finds a free name.
/// </summary>
function GetUniqueFileName(const ADefaultFileName: string): string; overload;

/// <summary>
///  Returns a random directory name which doesn't exists in APath
///  Keeps generating random names until it finds a free name.
/// </summary>
function GetUniqueDirectoryName(const APath: string): string;

/// <summary>
///  Returns the randomly generated name of a non-existing file in the
///  system's temporary directory. You can pass a custom extension, otherwise
///  the file name will have .tmp extension.
/// </summary>
function GetTempFileName(const AFileExtension: string = '.tmp'): string;

/// <summary>
///  Tries to determine the type of the data stream by interpreting the
///  first few bytes of the specified binary content. Currently supports several
///  graphic image types and the pdf type.</summary>
///  <param name="ABytes">Data stream. No need to pass all data; the first 10
///   bytes are enough. If not enough data is passed, the default type is
///   returned.
///  </param>
///  <param name="ADefault">
///   Default value that will be returned if the data
///   stream is not recognized.
///  </param>
/// <returns>
///  A lowercase string (such as 'gif' or 'pdf') suitable as a file
///  extension (without the dot).
/// </returns>
function GetDataType(const ABytes: TBytes; const ADefault: string): string;

procedure StreamToFile(const AStream: TStream; const AFileName: string);

implementation

uses
  StrUtils
  , IOUtils
  {$IFDEF MSWINDOWS}
  , EF.Sys.Windows
  {$ENDIF}
  {$IFDEF LINUX}
  , EF.Sys.Linux
  {$ENDIF}
  , EF.StrUtils
  ;

procedure StreamToFile(const AStream: TStream; const AFileName: string);
var
  LFileStream: TFileStream;
begin
  Assert(Assigned(AStream));

  LFileStream := TFileStream.Create(AFileName, fmCreate or fmShareExclusive);
  try
    LFileStream.CopyFrom(AStream, 0);
  finally
    FreeAndNil(LFileStream);
  end;
end;

function GetDataType(const ABytes: TBytes; const ADefault: string): string;
const
  MIN_BYTES = 10;
begin
  Result := ADefault;

  if Length(ABytes) >= MIN_BYTES then
  begin
    if ((ABytes[0] = 66) and (ABytes[1] = 77)) or ((ABytes[8] = 66) and (ABytes[9] = 77)) then
      Result := 'bmp'
    else if ((ABytes[0] = 73) and (ABytes[1] = 73) and (ABytes[2] = 42) and (ABytes[3] = 0))
        or ((ABytes[0] = 77) and (ABytes[1] = 77) and (ABytes[2] = 42) and (ABytes[3] = 0)) then
      Result := 'tif'
    else if (ABytes[0] = $FF) and (ABytes[1] = $D8) then
      Result := 'jpg'
    else if (ABytes[0] = $89) and (ABytes[1] = $50) and (ABytes[2] = $4E) and (ABytes[3] = $47)
        and (ABytes[4] = $0D) and (ABytes[5] = $0A) and (ABytes[6] = $1A) and (ABytes[7] = $0A) then
      Result := 'png'
    else if (ABytes[0] = 177) and (ABytes[1] = 104) and (ABytes[2] = 222) and (ABytes[3] = 58) then
      Result := 'dcx'
    else if ABytes[0] = 10 then
      Result := 'pcx'
    else if ((ABytes[0] = 215) and (ABytes[1] = 205) and (ABytes[2] = 198) and (ABytes[3] = 154))
        or ((ABytes[0] = 1) and (ABytes[1] = 0) and (ABytes[2] = 0) and (ABytes[3] = 0)) then
      Result := 'emf'
    else if (ABytes[0] = $47) and (ABytes[1] = $49) and (ABytes[2] = $46) then
      Result := 'gif'
    else if (ABytes[0] = 0) and (ABytes[1] = 0) and (ABytes[2] = 1) and (ABytes[3] = 0) then
      Result := 'ico'
    else if (ABytes[0] = $25) and (ABytes[1] = $50) and (ABytes[2] = $44) and (ABytes[3] = $46) then
      Result := 'pdf';
  end;
end;

function GetUniqueFileName(const APath, AExtension: string): string;
begin
  repeat
    Result := TPath.Combine(APath, GetRandomString(8) + AExtension);
  until not FileExists(Result);
end;

function GetUniqueFileName(const ADefaultFileName: string): string;
var
  LExtension: string;
begin
  Result := ADefaultFileName;
  while FileExists(Result) do
  begin
    LExtension := ExtractFileExt(ADefaultFileName);
    Result := ChangeFileExt(ADefaultFileName, '_' + GetRandomString(8) + LExtension);
  end;
end;

function GetUniqueDirectoryName(const APath: string): string;
begin
  repeat
    Result := TPath.Combine(APath, GetRandomString(8));
  until not DirectoryExists(Result);
end;

function GetTempFileName(const AFileExtension: string = '.tmp'): string;
begin
  Result := GetUniqueFileName(TPath.GetTempPath, AFileExtension);
end;

procedure ExpandEnvironmentVariables(var AString: string);
var
  LPos1: Integer;
  LPos2: Integer;
  LVariableValue, LValue: string;
begin
  LPos1 := Pos('%', AString);
  if LPos1 > 0 then
  begin
    LPos2 := PosEx('%', AString, LPos1 + 1);
    if LPos2 > 0 then
    begin
      LVariableValue := GetEnvironmentVariable(Copy(AString, LPos1 + 1, LPos2 - LPos1 - 1));
      if LVariableValue <> '' then
      begin
        LValue := Copy(AString, LPos2 + 1, MaxInt);
        ExpandEnvironmentVariables(LValue);
        AString := Copy(AString, 1, LPos1 - 1) + LVariableValue + LValue;
      end
      else
      begin
        LValue := Copy(AString, LPos2 + 1, MaxInt);
        ExpandEnvironmentVariables(LValue);
        AString := Copy(AString, 1, LPos2) + LValue;
      end;
    end;
  end;
end;

function IsAbsolutePath(const APath: string): Boolean;
begin
  Result := (Pos(':', APath) <> 0) or (Pos('\\', APath) = 1) or (Pos('/', APath) = 1);
end;

function GetCmdLineParamValue(const AParamName: string; const ADefaultValue: string = ''): string;
var
  LParamIndex: Integer;
begin
  Result := ADefaultValue;
  // Skip the last parameter, because it could never have one after itself.
  for LParamIndex := 1 to ParamCount - 1 do
  begin
    if SameText(ParamStr(LParamIndex), '/' + AParamName)
      or SameText(ParamStr(LParamIndex), '-' + AParamName) then
    begin
      Result := ParamStr(Succ(LParamIndex));
      Break;
    end;
  end;
end;

function GetFileSize(const AFileName: string): Longint;
var
  LSearchRec: TSearchRec;
begin
  if FindFirst(ExpandFileName(AFileName), faAnyFile, LSearchRec) = 0 then
    Result := LSearchRec.Size
  else
    Result := -1;
end;

function GetFileDateTime(const AFileName: string): TDateTime;
begin
  {$IF CompilerVersion < 18.0}
  Result := FileDateToDateTime(FileAge(AFileName));
  {$ELSE}
  if not FileAge(AFileName, Result) then
    raise Exception.CreateFmt(('Couldn''t determine the age of file "%s".'), [AFileName])
  {$ENDIF}
end;

initialization

finalization
  FreeAndNil(EFSys);

end.
