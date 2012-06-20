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

unit Kitto.Ext.Application;

{$I Kitto.Defines.inc}

interface

uses
  Classes,
  FCGIApp;

type
  TKExtAppThread = class(TThread)
  private
    FAppTitle: string;
    FTCPPort: Integer;
    FSessionTimeout: Integer;
  protected
    procedure Execute; override;
  public
    destructor Destroy; override;
  public
    procedure Configure;

    property AppTitle: string read FAppTitle;
    property TCPPort: Integer read FTCPPort;
    property SessionTimeout: Integer read FSessionTimeout;
  end;

implementation

uses
  SysUtils,
  EF.Logger, EF.Localization,
  Kitto.Config,
  Kitto.Ext.Session;

{ TKExtAppThread }

procedure TKExtAppThread.Configure;
var
  LConfig: TKConfig;
begin
  LConfig := TKConfig.Create;
  try
    TEFLogger.Instance.Log('Configuring thread...');
    FAppTitle := LConfig.AppTitle;
    FTCPPort := LConfig.Config.GetInteger('FastCGI/TCPPort', 2014);
    FSessionTimeout := LConfig.Config.GetInteger('FastCGI/SessionTimeout', 30);
    TEFLogger.Instance.Log('AppTitle: ' + _(FAppTitle));
    TEFLogger.Instance.LogFmt('TCPPort: %d', [FTCPPort]);
    TEFLogger.Instance.LogFmt('SessionTimeout: %d', [FSessionTimeout]);
  finally
    FreeAndNil(LConfig);
  end;
end;

destructor TKExtAppThread.Destroy;
begin
  Application.TerminateAllThreads;
  FreeAndNil(Application);
  inherited;
end;

procedure TKExtAppThread.Execute;
begin
  FreeAndNil(Application);
  Application := CreateWebApplication(_(FAppTitle), TKExtSession, FTCPPort, FSessionTimeout);
  Application.Run(Self);
end;

end.
