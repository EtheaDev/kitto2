{-------------------------------------------------------------------------------
   Copyright 2015 Ethea S.r.l.

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
///	 superobject integration in EF trees. Include this unit to add
///  superobject-related features to TEFTree and descendants.
///	</summary>
unit EF.superobject;

interface

uses
  SysUtils,
  EF.Tree, EF.Types,
  superobject;

type
  TEFTreeHelper = class helper for TEFTree
  public
    /// <summary>
    ///   Tries to read from ASuperObject a value for each child node interpreting
    ///   it according to the child node's DataType. Read values are stored in
    ///   the child nodes.
    /// </summary>
    /// <param name="ASuperObject">
    ///   Source data. Only top-level pairs are used. Each pair may contain one
    ///   value. If AValueIndex is >= 0, each string may contain one or more
    ///   comma-separated values, of which only the one with index
    ///   AValueIndex is read.
    /// </param>
    /// <param name="AUseJSDateFormat">
    ///   True if any dates in source strings are in JS format; False for
    ///   system format.
    /// </param>
    /// <param name="AFormatSettings">
    ///   Custom format settings to decode values.
    /// </param>
    /// <param name="ATranslator">
    ///   Pass a translation function if key names in ASuperObject do not match
    ///   wanted child node names and you need to translate them. The function
    ///   receives the child name and should return the corresponding key name.
    /// </param>
    procedure SetChildValuesfromSuperObject(const ASuperObject: ISuperObject;
      const AUseJSDateFormat: Boolean; const AFormatSettings: TFormatSettings;
      const ATranslator: TNameTranslator; const AValueIndex: Integer = -1);
  end;

implementation

uses
  System.Types,
  EF.StrUtils;

{ TEFTreeHelper }

procedure TEFTreeHelper.SetChildValuesfromSuperObject(
  const ASuperObject: ISuperObject; const AUseJSDateFormat: Boolean;
  const AFormatSettings: TFormatSettings; const ATranslator: TNameTranslator;
  const AValueIndex: Integer);
var
  I: Integer;
  LChild: TEFNode;
  LName: string;
  LStringValue: string;
  LStringValues: TStringDynArray;

  function Translate(const AName: string): string;
  begin
    if Assigned(ATranslator) then
      Result := ATranslator(AName)
    else
      Result := AName;
  end;

begin
  for I := 0 to ChildCount - 1 do
  begin
    LChild := Children[I];
    LName := Translate(LChild.Name);
    Assert(LName <> '');
    LStringValue := ASuperObject.S[LName];
    if LStringValue <> '' then
    begin
      if AValueIndex >= 0 then
      begin
        LStringValues := Split(LStringValue, ',');
        if Length(LStringValues) > AValueIndex then
          LStringValue := LStringValues[AValueIndex]
        else
          LStringValue := '';
      end;
      LChild.SetAsJSONValue(LStringValue, AUseJSDateFormat, AFormatSettings);
    end
    // Checkboxes are not submitted when unchecked, which for us means False.
    else if LChild.DataType is TEFBooleanDataType then
      LChild.AsBoolean := False;
  end;
end;

end.
