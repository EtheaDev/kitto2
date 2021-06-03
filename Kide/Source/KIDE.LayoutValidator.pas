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
///	  The Layout validator should flag:
///	  <list type="bullet">
///	  </list>
///	</summary>
unit KIDE.LayoutValidator;

interface

uses
  EF.Classes, Kitto.Metadata.Views,
  KIDE.TreeValidator;

type
  TLayoutValidator = class(TTreeValidator)
  private
    procedure ValidateLayout(const ALayout: TKLayout);
  protected
    procedure InternalExecute; override;
  public
    procedure ValidateLayouts(ASingleLayout: TKLayout);
  end;

implementation

uses
  SysUtils, StrUtils,
  EF.Tree,
  KIDE.Project;

{ TLayoutValidator }

procedure TLayoutValidator.InternalExecute;
begin
  inherited;
  ValidateLayouts(nil);
end;

procedure TLayoutValidator.ValidateLayout(const ALayout: TKLayout);
begin
  Assert(Assigned(ALayout));

  LogIndent;
  try
    Log(Format('Validating Layout %s...', [ALayout.PersistentName]));

    ValidateTree(ALayout);

  finally
    LogOutdent;
  end;
end;

procedure TLayoutValidator.ValidateLayouts(ASingleLayout: TKLayout);
var
  I: Integer;
  LLayout: TKLayout;
begin
  inherited;
  if not Assigned(ASingleLayout) then
    Log('Validating Layouts...');

  for I := 0 to TProject.CurrentProject.Config.Views.Layouts.LayoutCount - 1 do
  begin
    LLayout := TProject.CurrentProject.Config.Views.Layouts.Layouts[I];
    if not Assigned(ASingleLayout) or SameText(ASingleLayout.PersistentName, LLayout.PersistentName) then
      ValidateLayout(LLayout);
  end;

  if ErrorCount > 0 then
    LogWarning('Layout validation complete. Errors were detected.')
  else
    LogInfo('Layout validation complete. No errors detected.');
end;

end.
