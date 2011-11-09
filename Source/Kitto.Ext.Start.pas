{-------------------------------------------------------------------------------
   Copyright 2011 Ethea S.r.l.

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

unit Kitto.Ext.Start;

interface

type
  TKExtStart = class
  private
  class var
    FServiceName: string;
    FServiceDisplayName: string;
  public
    class constructor Create;
    class property ServiceName: string read FServiceName write FServiceName;
    class property ServiceDisplayName: string read FServiceDisplayName write FServiceDisplayName;
    class procedure Start;
  end;

implementation

uses
  SysUtils, Forms, Classes, SvcMgr, ShlObj,
  EF.SysUtils, EF.Logger,
  Kitto.Config,
  Kitto.Ext.MainFormUnit, Kitto.Ext.Service;

{ TKExtStart }

class constructor TKExtStart.Create;
var
  LConfig: TKConfig;
begin
  FServiceName := TKConfig.AppName;
  LConfig := TKConfig.Create;
  try
    FServiceDisplayName := LConfig.AppTitle;
  finally
    FreeAndNil(LConfig);
  end;
end;

class procedure TKExtStart.Start;
var
  LConfig: TKConfig;
begin
  if IsUserAnAdmin then
  begin
    LConfig := TKConfig.Create;
    try
      TEFLogger.LogFileName :=
        // Explicitly calling Expoand here makes sure macros are
        // expanded even now that we have no session thus no macros.
        LConfig.MacroExpansionEngine.Expand(LConfig.Config.GetString('Log/FileName'));
      TEFLogger.LogLevel := LConfig.Config.GetInteger('Log/Level', TEFLogger.LogLevel);
    finally
      FreeAndNil(LConfig);
    end;
    TEFLogger.Log('Starting as service.');
    if not SvcMgr.Application.DelayInitialize or SvcMgr.Application.Installing then
      SvcMgr.Application.Initialize;
    SvcMgr.Application.CreateForm(TKExtService, KExtService);
    KExtService.Name := FServiceName;
    KExtService.DisplayName := FServiceDisplayName;
    SvcMgr.Application.Run;
  end
  else
  begin
    TEFLogger.LogFileName := ''; // No logging in interactive mode.
{ TODO : Log to screen in interactive mode. }
    TEFLogger.Log('Starting as application.');
    Forms.Application.Initialize;
    Forms.Application.CreateForm(TKExtMainForm, KExtMainForm);
    Forms.Application.Run;
  end;
end;

end.
