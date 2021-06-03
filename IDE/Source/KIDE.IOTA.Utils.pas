{ -------------------------------------------------------------------------------
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
  ------------------------------------------------------------------------------- }

{ -------------------------------------------------------------------------------
  Based on code by David Hoyle
  http://www.davidghoyle.co.uk/
  ------------------------------------------------------------------------------- }
unit KIDE.IOTA.Utils;

interface

uses
  ToolsAPI
  ;

/// <summary>
///  Returns a reference to the currently open Project Group in the IDE.
///  If there is no Project Group opened, returns nil.
/// </summary>
function FindActiveProjectGroup: IOTAProjectGroup;

/// <summary>
///  Returns a reference to the active Project in the Project Manager.
///  If there is no active Project, returns nil.
/// </summary>
function FindActiveProject: IOTAProject;

/// <summary>
///  Returns a reference to the specified Project's Source Module.
///  Cannot return nil unless the specified project is not open.
/// </summary>
function GetProjectModule(const AProject: IOTAProject): IOTAModule;

/// <summary>
///  Returns a reference to the active Source Editor.
///  If there is no active Source Editor, returns nil.
/// </summary>
function FindActiveSourceEditor: IOTASourceEditor;

/// <summary>
///  Returns a reference to the Source Editor for the specified Module.
///  If there is no Editor, returns nil.
/// </summary>
function FindSourceEditor(const AModule: IOTAModule): IOTASourceEditor;

/// <summary>
///  Returns the specified Source Editor's contents as a string.
/// </summary>
function GetEditorContentAsString(const ASourceEditor: IOTASourceEditor): string;

type
  /// <summary>
  ///  Represents a source file whose contents are read from a resource.
  ///  It is used to load templates embedded in the design-time package binary
  ///  when creating projects in the wizards.
  /// </summary>
  TKResourceFile = class(TInterfacedObject, IOTAFile)
  strict private
    FResourceName: string;
  public
    constructor Create(const AResourceName: string);

    // IOTAFile
    function GetAge: TDateTime;
    function GetSource: string;
  end;

  TKStringFile = class(TInterfacedObject, IOTAFile)
  strict private
    FSource: string;
  public
    constructor Create(const ASource: string);

    // IOTAFile
    function GetAge: TDateTime;
    function GetSource: string;

    property Source: string read GetSource write FSource;
  end;

implementation

uses
  Types
  , SysUtils
  , Classes
  ;

function FindActiveProjectGroup: IOTAProjectGroup;
var
  I: Integer;
  LModuleServices: IOTAModuleServices;
  LModule: IOTAModule;
  LProjectGroup: IOTAProjectGroup;
begin
  Result := nil;
  LModuleServices := BorlandIDEServices as IOTAModuleServices;
  for I := 0 to LModuleServices.ModuleCount - 1 do
  begin
    LModule := LModuleServices.Modules[I];
    if LModule.QueryInterface(IOTAProjectGroup, LProjectGroup) = S_OK then
      Break;
  end;
  Result := LProjectGroup;
end;

function FindActiveProject: IOTAProject;
var
  LProjectGroup: IOTAProjectGroup;
begin
  LProjectGroup := FindActiveProjectGroup;
  if LProjectGroup <> nil then
    Result := LProjectGroup.ActiveProject;
end;

function GetProjectModule(const AProject: IOTAProject): IOTAModule;
var
  I: Integer;
  LModuleServices: IOTAModuleServices;
  LModule: IOTAModule;
  LProject: IOTAProject;
begin
  Assert(Assigned(AProject));

  Result := nil;
  LProject := nil;
  LModuleServices := BorlandIDEServices as IOTAModuleServices;
  for I := 0 to LModuleServices.ModuleCount - 1 do
  begin
    LModule := LModuleServices.Modules[I];
    if (LModule.QueryInterface(IOTAProject, LProject) = S_OK) and (AProject = LProject) then
      Break;
  end;
  Result := LProject;
end;

function FindSourceEditor(const AModule: IOTAModule): IOTASourceEditor;
var
  I: Integer;
  LFileCount: Integer;
begin
  Result := nil;
  if AModule = nil then
    Exit;

  LFileCount := AModule.GetModuleFileCount;
  for I := 0 to LFileCount - 1 do
  begin
    if AModule.GetModuleFileEditor(I).QueryInterface(IOTASourceEditor, Result) = S_OK then
      Break;
  end;
end;

function FindActiveSourceEditor: IOTASourceEditor;
var
  LModule: IOTAModule;
begin
  Result := NIL;
  if BorlandIDEServices = nil then
    Exit;

  LModule := (BorlandIDEServices as IOTAModuleServices).CurrentModule;
  Result := FindSourceEditor(LModule);
end;

function GetEditorContentAsString(const ASourceEditor: IOTASourceEditor): string;
const
  BUFFER_SIZE = 4096;
var
  LReader: IOTAEditReader;
  LPosition: Integer;
  LBytesRead: Integer;
  LBufferString: RawByteString;
begin
  Result := '';
  LReader := ASourceEditor.CreateReader;

  LPosition := 0;
  repeat
    SetLength(LBufferString, BUFFER_SIZE);
    LBytesRead := LReader.GetText(LPosition, PAnsiChar(LBufferString), BUFFER_SIZE);
    SetLength(LBufferString, LBytesRead);
    Result := Result + string(LBufferString);
    Inc(LPosition, LBytesRead);
  until LBytesRead < BUFFER_SIZE;
end;

{ TKResourceFile }

constructor TKResourceFile.Create(const AResourceName: string);
begin
  Assert(AResourceName <> '');

  inherited Create;
  FResourceName := AResourceName;
end;

function TKResourceFile.GetAge: TDateTime;
begin
  Result := -1;
end;

function TKResourceFile.GetSource: string;
var
  LResourceStream: TResourceStream;
  LBytesStream: TBytesStream;
begin
  { TODO : macros/text replace }
  LResourceStream := TResourceStream.Create(HInstance, FResourceName, RT_RCDATA);
  try
    if LResourceStream.Size = 0 then
      raise Exception.CreateFmt('Resource %s is empty', [FResourceName]);

    LBytesStream := TBytesStream.Create;
    try
      LBytesStream.CopyFrom(LResourceStream, LResourceStream.Size);
      Result := TEncoding.UTF8.GetString(Copy(LBytesStream.Bytes, 0, LBytesStream.Size));
    finally
      FreeAndNil(LBytesStream);
    end;
  finally
    FreeAndNil(LResourceStream);
  end;
end;

{ TKStringFile }

constructor TKStringFile.Create(const ASource: string);
begin
  inherited Create;
  FSource := ASource;
end;

function TKStringFile.GetAge: TDateTime;
begin
  Result := -1;
end;

function TKStringFile.GetSource: string;
begin
  Result := FSource;
end;

end.

