{-------------------------------------------------------------------------------
   Copyright 2012 Ethea S.r.l.

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

unit EF.Logger.TextFile;

interface

uses
  EF.Tree, EF.Macros, EF.Logger, EF.ObserverIntf, EF.Streams;

type
  TEFTextFileLogEndpoint = class(TEFLogEndpoint)
  strict private
    FStream: TEFTextStream;
    FFileName: string;
  class var
    FInstance: TEFTextFileLogEndpoint;
    function GetStream: TEFTextStream;
  strict protected
    procedure DoLog(const AString: string); override;
    procedure Configure(const AConfig: TEFNode;
      const AMacroExpansionEngine: TEFMacroExpansionEngine); override;
    property Stream: TEFTextStream read GetStream;
    procedure SetFileName(const AValue: string);
    function GetConfigPath: string; override;
  public
    class procedure CreateSingletonInstance;
    class procedure FreeSingletonInstance;
    procedure AfterConstruction; override;
    property FileName: string read FFileName write SetFileName;
  end;

implementation

uses
  SysUtils, Classes;

{ TEFTextFileLogger }

procedure TEFTextFileLogEndpoint.AfterConstruction;
begin
  inherited;
  FFileName := ChangeFileExt(ParamStr(0), '.log');
end;

procedure TEFTextFileLogEndpoint.Configure(const AConfig: TEFNode;
  const AMacroExpansionEngine: TEFMacroExpansionEngine);
begin
  inherited;
  if IsEnabled and Assigned(AConfig) and Assigned(AMacroExpansionEngine) then
    // Explicitly calling Expoand here makes sure macros are
    // expanded even now that we have no session thus no macros.
    FileName := AMacroExpansionEngine.Expand(AConfig.GetString(GetConfigPath + 'FileName', FileName));
end;

class procedure TEFTextFileLogEndpoint.CreateSingletonInstance;
begin
  FInstance := TEFTextFileLogEndpoint.Create;
end;

class procedure TEFTextFileLogEndpoint.FreeSingletonInstance;
begin
  FreeAndNil(FInstance);
end;

procedure TEFTextFileLogEndpoint.DoLog(const AString: string);
begin
  if IsEnabled then
    Stream.WriteLn(FormatDateTime('[yyyy-mm-dd hh:nn:ss.zzz] ', Now()) +  AString);
end;

function TEFTextFileLogEndpoint.GetConfigPath: string;
begin
  Result := 'TextFile/';
end;

function TEFTextFileLogEndpoint.GetStream: TEFTextStream;
var
  LCreateFlag: Integer;
begin
  if not Assigned(FStream) then
  begin
    if FileExists(FFileName) then
      LCreateFlag := 0
    else
    begin
      LCreateFlag := fmCreate;
      ForceDirectories(ExtractFilePath(FFileName));
    end;
    FStream := TEFTextStream.Create(TFileStream.Create(FFileName, LCreateFlag or fmOpenWrite or fmShareDenyWrite));
    FStream.Seek(0, soFromEnd);
  end;
  Result := FStream;
end;

procedure TEFTextFileLogEndpoint.SetFileName(const AValue: string);
begin
  if AValue <> FFileName then
  begin
    FFileName := AValue;
    FreeAndNil(FStream);
  end;
end;

initialization
  TEFTextFileLogEndpoint.CreateSingletonInstance;

finalization
  TEFTextFileLogEndpoint.FreeSingletonInstance;

end.
