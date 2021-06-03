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
unit KIDE.Config;

interface

uses
  Kitto.Config;

type
  TKideConfig = class(TKConfig)
  strict private
    function GetTemplatePath: string;
    function GetMetadataTemplatePath: string;
    class function GetInstance: TKideConfig; static;
  strict protected
    function GetConfigFileName: string; override;
  public
    class property Instance: TKideConfig read GetInstance;
    property TemplatePath: string read GetTemplatePath;
    property MetadataTemplatePath: string read GetMetadataTemplatePath;
  end;

implementation

uses
  SysUtils,
  Kitto.AccessControl;

{ TKideConfig }

function TKideConfig.GetConfigFileName: string;
begin
  Result := ExtractFilePath(ParamStr(0)) + 'Config.yaml';
end;

class function TKideConfig.GetInstance: TKideConfig;
begin
  Result := TKConfig.Instance as TKideConfig;
end;

function TKideConfig.GetTemplatePath: string;
begin
  Result := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)) + 'ProjectTemplates');
end;

function TKideConfig.GetMetadataTemplatePath: string;
begin
  Result := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)) + 'MetadataTemplates');
end;

initialization
  TKConfig.SetConfigClass(TKideConfig);
  //activation for memory leaks
  ReportMemoryLeaksOnShutdown := DebugHook <> 0;

end.
