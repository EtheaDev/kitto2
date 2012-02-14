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

///	<summary>
///	  Variant-related utility routines.
///	</summary>
unit EF.VariantUtils;

interface

///	<summary>
///	  Converts AVariant to a string, returning '' for null/empty variants.
///	</summary>
function EFVarToStr(const AVariant: Variant): string;

///	<summary>
///	  Converts AVariant to a TDateTime, returning 0 for null/empty variants.
///	</summary>
function EFVarToDateTime(const AVariant: Variant): TDateTime;

///	<summary>
///	  Converts AVariant to an Integer, returning 0 for null/empty variants.
///	</summary>
function EFVarToInt(const AVariant: Variant): Integer;

///	<summary>
///	  Converts AVariant to a Double, returning 0 for null/empty variants.
///	</summary>
function EFVarToFloat(const AVariant: Variant): Double;

///	<summary>
///	  Converts AVariant to a Currency, returning 0 for null/empty variants.
///	</summary>
function EFVarToCurrency(const AVariant: Variant): Currency;

///	<summary>Returns True if ATo is greater than or equal to AFrom, and False
///	otherwise.</summary>
///	<remarks>If one or both values are not Null, the function returns
///	True.</remarks>
function IsRange(const AFrom, ATo: Variant): Boolean;

implementation

uses
  Variants;

function EFVarToStr(const AVariant: Variant): string;
begin
  if VarIsNull(AVariant) or VarIsEmpty(AVariant) then
    Result := ''
  else
    Result := AVariant;
end;

function EFVarToDateTime(const AVariant: Variant): TDateTime;
begin
  if VarIsNull(AVariant) or VarIsEmpty(AVariant) then
    Result := 0
  else
    Result := VarToDateTime(AVariant);
end;

function EFVarToInt(const AVariant: Variant): Integer;
begin
  if VarIsNull(AVariant) or VarIsEmpty(AVariant) then
    Result := 0
  else
    Result := AVariant;
end;

function EFVarToFloat(const AVariant: Variant): Double;
begin
  if VarIsNull(AVariant) or VarIsEmpty(AVariant) then
    Result := 0
  else
    Result := AVariant;
end;

function EFVarToCurrency(const AVariant: Variant): Currency;
begin
  if VarIsNull(AVariant) or VarIsEmpty(AVariant) then
    Result := 0
  else
    Result := AVariant;
end;

function IsRange(const AFrom, ATo: Variant): Boolean;
begin
  if VarIsNull(AFrom) or VarIsNull(ATo) then
    Result := True
  else
    Result := AFrom <= ATo;
end;

end.
