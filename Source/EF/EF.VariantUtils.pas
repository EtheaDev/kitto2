unit EF.VariantUtils;

interface

{
  Converts AVariant to a string, returning '' for null/empty variants.
}
function EFVarToStr(const AVariant: Variant): string;

{
  Converts AVariant to a TDateTime, returning 0 for null/empty variants.
}
function EFVarToDateTime(const AVariant: Variant): TDateTime;

{
  Converts AVariant to an Integer, returning 0 for null/empty variants.
}
function EFVarToInt(const AVariant: Variant): Integer;

{
  Converts AVariant to a Double, returning 0 for null/empty variants.
}
function EFVarToFloat(const AVariant: Variant): Double;

{
  Converts AVariant to a Currency, returning 0 for null/empty variants.
}
function EFVarToCurrency(const AVariant: Variant): Currency;

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

end.
