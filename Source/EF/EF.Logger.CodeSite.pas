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

unit EF.Logger.CodeSite;

interface

uses
  EF.Tree, EF.Logger, EF.ObserverIntf;

type
  TEFCodeSiteLogEndpoint = class(TEFLogEndpoint)
  class var
    FInstance: TEFCodeSiteLogEndpoint;
  protected
    procedure DoLog(const AString: string); override;
    function GetConfigPath: string; override;
  public
    class procedure CreateSingletonInstance;
    class procedure FreeSingletonInstance;
  end;

implementation

uses
  SysUtils,
  CodeSiteLogging;

{ TEFCodeSiteLogEndpoint }

class procedure TEFCodeSiteLogEndpoint.CreateSingletonInstance;
begin
  FInstance := TEFCodeSiteLogEndpoint.Create;
end;

class procedure TEFCodeSiteLogEndpoint.FreeSingletonInstance;
begin
  FreeAndNil(FInstance);
end;

function TEFCodeSiteLogEndpoint.GetConfigPath: string;
begin
  Result := 'CodeSite/';
end;

procedure TEFCodeSiteLogEndpoint.DoLog(const AString: string);
begin
  if IsEnabled then
    CodeSite.Send(AString);
end;

initialization
  TEFCodeSiteLogEndpoint.CreateSingletonInstance;

finalization
  TEFCodeSiteLogEndpoint.FreeSingletonInstance;

end.
