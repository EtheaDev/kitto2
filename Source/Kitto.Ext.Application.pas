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

    property AppTitle: string read FAppTitle write FAppTitle;
    property TCPPort: Integer read FTCPPort write FTCPPort;
    property SessionTimeout: Integer read FSessionTimeout write FSessionTimeout;
  end;

implementation

uses
  SysUtils,
  Kitto.Ext.Session;

{ TKExtAppThread }

destructor TKExtAppThread.Destroy;
begin
  Application.TerminateAllThreads;
  FreeAndNil(Application);
  inherited;
end;

procedure TKExtAppThread.Execute;
begin
  FreeAndNil(Application);
  Application := CreateWebApplication(FAppTitle, TKExtSession, FTCPPort, FSessionTimeout);
  Application.Run(Self);
end;

end.
