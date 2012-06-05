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

unit Kitto.Ext.Start;

interface

type
  TKExtStart = class
  private
  class var
    FServiceName: string;
    FServiceDisplayName: string;
  public
    class property ServiceName: string read FServiceName write FServiceName;
    class property ServiceDisplayName: string read FServiceDisplayName write FServiceDisplayName;
    class procedure Start;
  end;

implementation

uses
  SysUtils, Forms, Classes, SvcMgr, ShlObj,
  EF.SysUtils, EF.Logger, EF.Localization,
  Kitto.Config,
  Kitto.Ext.MainFormUnit, Kitto.Ext.Service;

{ TKExtStart }

class procedure TKExtStart.Start;

  procedure Configure;
  var
    LConfig: TKConfig;
  begin
    LConfig := TKConfig.Create;
    try
      TEFLogger.Instance.Configure(LConfig.Config.FindNode('Log'), LConfig.MacroExpansionEngine);
      FServiceName := TKConfig.AppName;
      FServiceDisplayName := _(LConfig.AppTitle);
    finally
      FreeAndNil(LConfig);
    end;
  end;

begin
  Configure;

  if IsUserAnAdmin and not FindCmdLineSwitch('a') then
  begin
    TEFLogger.Instance.Log('Starting as service.');
    if not SvcMgr.Application.DelayInitialize or SvcMgr.Application.Installing then
      SvcMgr.Application.Initialize;
    SvcMgr.Application.CreateForm(TKExtService, KExtService);
    KExtService.Name := FServiceName;
    KExtService.DisplayName := FServiceDisplayName;
    SvcMgr.Application.Run;
  end
  else
  begin
    TEFLogger.Instance.Log('Starting as application.');
    Forms.Application.Initialize;
    Forms.Application.CreateForm(TKExtMainForm, KExtMainForm);
    Forms.Application.Run;
  end;
end;

end.
