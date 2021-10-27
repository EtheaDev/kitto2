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

unit Kitto.Vcl.Start;

{$I Kitto.Defines.inc}

interface

type
  TKStart = class
  public
    class procedure Start;
  end;

implementation

uses
  SysUtils
  , Classes
  , EF.Tree
  , EF.Logger
  , EF.Macros
  , Kitto.Config
  , EF.Sys
  {$IFDEF MSWINDOWS}
  , EF.Sys.Windows
  , Vcl.Forms
  , Vcl.SvcMgr
  , Vcl.Themes
  , Vcl.Styles
  , Kitto.Vcl.MainForm
  , Kitto.Vcl.Service
  {$ENDIF}
  ;

{ TKStart }

class procedure TKStart.Start;

  procedure Configure;
  var
    LConfig: TKConfig;
    LLogNode: TEFNode;
    LCustomConfig: string;
  begin
    LCustomConfig := GetCmdLineParamValue('c');
    if LCustomConfig <> '' then
      TKConfig.BaseConfigFileName := LCustomConfig;
    LConfig := TKConfig.Create;
    try
      LLogNode := LConfig.Config.FindNode('Log');
      TEFLogger.Instance.Configure(LLogNode, TEFMacroExpansionEngine.Instance);
      TEFLogger.Instance.Log(Format('Using configuration: %s',[LConfig.BaseConfigFileName]));
    finally
      FreeAndNil(LConfig);
    end;
  end;

begin
  Configure;
  {$IFDEF MSWINDOWS}
  if IsRunningAsService or IsInstallingService then
  begin
    TEFLogger.Instance.Log('Starting as service.');
    if not Vcl.SvcMgr.Application.DelayInitialize or Vcl.SvcMgr.Application.Installing then
      Vcl.SvcMgr.Application.Initialize;
    Vcl.SvcMgr.Application.CreateForm(TKService, KService);
    Vcl.SvcMgr.Application.Run;
  end
  else
  begin
    TEFLogger.Instance.Log('Starting as application.');
    Vcl.Forms.Application.Initialize;
    Vcl.Forms.Application.CreateForm(TKMainForm, KMainForm);
    Vcl.Forms.Application.Run;
  end;
  {$ELSE}
  TEFLogger.Instance.Log('Start not yet supported on this platform.');
  {$ENDIF}
end;

initialization
  {$IFDEF MSWINDOWS}
  {$WARN SYMBOL_PLATFORM OFF}
  ReportMemoryLeaksOnShutdown := DebugHook <> 0;
  {$ENDIF}

end.
