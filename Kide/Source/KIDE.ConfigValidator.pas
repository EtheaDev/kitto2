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
///	<summary>
///	  The config validator should flag:
///	  <list type="bullet">
///	    <item>config without controllers</item>
///	    <item>missing required parameters (requires a means to describe Config
///	    currently not existing)</item>
///	    <item>misplaced nodes (useful to spot typos or when incompatible
///	    changes are done in Kitto)</item>
///	  </list>
///	</summary>
unit KIDE.ConfigValidator;

interface

uses
  EF.Classes,
  KIDE.TreeValidator;

type
  TConfigValidator = class(TTreeValidator)
  private
    procedure ValidateConfig(const AConfig: TEFComponentConfig);
  protected
    procedure InternalExecute; override;
  public
    procedure ValidateConfigs(ASingleConfig: TEFComponentConfig);
  end;

implementation

uses
  SysUtils, StrUtils,
  EF.Tree,
  KIDE.Project;

{ TConfigValidator }

procedure TConfigValidator.InternalExecute;
begin
  inherited;
  ValidateConfigs(nil);
end;

procedure TConfigValidator.ValidateConfig(const AConfig: TEFComponentConfig);
begin
  Assert(Assigned(AConfig));

  LogIndent;
  try
    Log(Format('Validating Config %s...', [AConfig.PersistentName]));

    ValidateTree(AConfig);

    if AConfig.GetString('AppTitle') = '' then
      LogWarning('Missing AppTitle.');

    if AConfig.FindNode('Databases') = nil then
      LogWarning('Missing Databases Node: at least one Database Connection must be configured to access data.');

    if AConfig.GetString('Server/Port') = '' then
      LogWarning('Missing Server/Port: any kitto application uses a different Server/Port: if not defined default port 8080 is used.');

    if AConfig.GetString('ExtJS/Theme') = '' then
      LogWarning('Missing ExtJS/Theme: any kitto application can use a specific theme: if not defined default theme "triton" is used.');

  finally
    LogOutdent;
  end;
end;

procedure TConfigValidator.ValidateConfigs(ASingleConfig: TEFComponentConfig);
var
  I: Integer;
  LConfig: TEFComponentConfig;
begin
  inherited;
  if not Assigned(ASingleConfig) then
    Log('Validating Configs...');

  for I := 0 to TProject.CurrentProject.AppConfigCount - 1 do
  begin
    LConfig := TProject.CurrentProject.AppConfigs[I].Config;
    if not Assigned(ASingleConfig) or SameText(ASingleConfig.PersistentName, LConfig.PersistentName) then
      ValidateConfig(LConfig);
  end;

  if ErrorCount > 0 then
    LogWarning('Config validation complete. Errors were detected.')
  else
    LogInfo('Config validation complete. No errors detected.');
end;

end.
