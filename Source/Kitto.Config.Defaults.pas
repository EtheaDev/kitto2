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

unit Kitto.Config.Defaults;

{$I Kitto.Defines.inc}

interface

type
  TKDefaults = class
  public
    class function GetSingleSpacing: Integer;
    class function GetDoubleSpacing: Integer;
  end;

implementation

uses
  Kitto.Config
  ;

{ TKDefaults }

class function TKDefaults.GetDoubleSpacing: Integer;
begin
  Result := TKConfig.Instance.Config.GetInteger('Defaults/Spacing/Double', GetSingleSpacing * 2);
end;

class function TKDefaults.GetSingleSpacing: Integer;
begin
  Result := TKConfig.Instance.Config.GetInteger('Defaults/Spacing/Single', 10);
end;

end.
