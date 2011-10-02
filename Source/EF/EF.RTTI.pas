unit EF.RTTI;

{$I EF.Defines.inc}

interface

uses
  Classes;

///	<summary>
///	  Finds the specified component in the specified owner and returns the
///	  value of the specified property as a string. If no component or property
///	  is found, returns ''.
///	</summary>
function GetComponentPropertyValue(const AOwner: TComponent;
  const AComponentName, APropertyName: string): string;

implementation

uses
  TypInfo;

function GetComponentPropertyValue(const AOwner: TComponent;
  const AComponentName, APropertyName: string): string;
var
  LComponent: TComponent;
  LPropInfo: PPropInfo;
begin
  Assert(Assigned(AOwner));

  Result := '';
  LComponent := AOwner.FindComponent(AComponentName);
  if Assigned(LComponent) then
  begin
    LPropInfo := GetPropInfo(LComponent, APropertyName);
    if LPropInfo <> nil then
      Result := GetPropValue(LComponent, LPropInfo);
  end;
end;

end.

