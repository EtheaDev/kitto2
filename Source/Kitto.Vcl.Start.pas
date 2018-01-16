{-------------------------------------------------------------------------------
   Copyright 2012-2018 Ethea S.r.l.

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
  private
    class var FIsService: Boolean;
  public
    class procedure Start;

    class property IsService: Boolean read FIsService;
  end;

implementation

uses
  SysUtils
  , Classes
  , EF.Logger
  , Kitto.Config
  {$IFDEF MSWINDOWS}
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
  begin
    LConfig := TKConfig.Create;
    try
      TEFLogger.Instance.Configure(LConfig.Config.FindNode('Log'), LConfig.MacroExpansionEngine);
    finally
      FreeAndNil(LConfig);
    end;
  end;

begin
  Configure;

  FIsService := not FindCmdLineSwitch('a');
  if FIsService then
  begin
    {$IFDEF MSWINDOWS}
    TEFLogger.Instance.Log('Starting as service.');
    if not Vcl.SvcMgr.Application.DelayInitialize or Vcl.SvcMgr.Application.Installing then
      Vcl.SvcMgr.Application.Initialize;
    Vcl.SvcMgr.Application.CreateForm(TKService, KService);
    Vcl.SvcMgr.Application.Run;
    {$ELSE}
    TEFLogger.Instance.Log('Services not yet supported on this platform.');
    {$ENDIF}
  end
  else
  begin
    {$IFDEF MSWINDOWS}
    TEFLogger.Instance.Log('Starting as application.');
    Vcl.Forms.Application.Initialize;
    Vcl.Forms.Application.CreateForm(TKMainForm, KMainForm);
    Vcl.Forms.Application.Run;
    {$ELSE}
    TEFLogger.Instance.Log('GUI applications not yet supported on this platform.');
    {$ENDIF}
  end;
end;

initialization
  {$IFDEF MSWINDOWS}
  {$WARN SYMBOL_PLATFORM OFF}
  ReportMemoryLeaksOnShutdown := DebugHook <> 0;
  {$ENDIF}

end.
