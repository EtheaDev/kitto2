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

unit Kitto.Console.Start;

{$I Kitto.Defines.inc}

interface

uses
  EF.Logger
  ;

type
  TKStart = class
  public
    class procedure Start;
  end;

  TKConsoleLogEndpoint = class(TEFLogEndpoint)
  protected
    procedure DoLog(const AString: string); override;
  end;

implementation

uses
  SysUtils
  , EF.Localization
  , EF.Macros
  , EF.Sys
  , EF.Tree
  , Kitto.Config
  , Kitto.Web.Server
  , Kitto.Web.Application
  ;

{ TKStart }

class procedure TKStart.Start;
var
  LLogEndPoint: TKConsoleLogEndpoint;
  LServer: TKWebServer;
  LApplication: TKWebApplication;

  procedure Configure;
  var
    LConfig: TKConfig;
    LLogNode: TEFNode;
  begin
    LConfig := TKConfig.Create;
    try
      LLogNode := LConfig.Config.FindNode('Log');
      TEFLogger.Instance.Configure(LLogNode, TEFMacroExpansionEngine.Instance);
      if Assigned(LLogNode) and LLogNode.GetBoolean('Console/IsEnabled') then
        LLogEndPoint := TKConsoleLogEndpoint.Create;

    finally
      FreeAndNil(LConfig);
    end;
  end;

begin
  LLogEndPoint := nil;
  Configure;
  try
    WriteLn(Format(_('Build date: %s'), [DateTimeToStr(GetFileDateTime(ParamStr(0)))]));
    TEFLogger.Instance.LogDetailed('Console start. Initializing web server...');
    LServer := TKWebServer.Create(nil);
    try
      LApplication := TKWebApplication.Create;
      LServer.Engine.AddRoute(LApplication);
      TEFLogger.Instance.LogDetailed('Console start. Turning on web server...');
      LServer.Active := True;
      TEFLogger.Instance.LogDetailed('Console start. Web server on.');
      TEFLogger.Instance.Log(Format(_('Home URL: %s'), [LApplication.GetHomeURL(LServer.DefaultPort)]));
      WriteLn('Press enter to exit.');
      ReadLn;
      LServer.Active := False;
    finally
      FreeAndNil(LServer);
    end;
  finally
    FreeAndNil(LLogEndPoint);
  end;
end;

{ TKConsoleLogEndpoint }

procedure TKConsoleLogEndpoint.DoLog(const AString: string);
begin
  inherited;
  Writeln(AString);
end;

end.
