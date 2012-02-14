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

///	<summary>Ext-specific rule implementations.</summary>
unit Kitto.Ext.Rules;

{$I Kitto.Defines.inc}

interface

uses
  ExtForm,
  Kitto.Metadata.Models, Kitto.Rules;

type
  ///	<summary>Base class for all ext-specific rule implementation
  ///	classes.</summary>
  TKExtRuleImpl = class(TKRuleImpl)
  protected
    class function GetClassId: string; override;
  public
    ///	<summary>Rules that work by setting properties or events in a form
    ///	field do that in this method.</summary>
    procedure ApplyToFormField(const AField: TExtFormField); virtual;

    ///	<summary>Client side validation before submit.</summary>
    procedure BeforeSubmit; virtual;
  end;

  ///	<summary>Enforces a minimum allowed value for numeric fields. Can be used
  ///	as a field-level rule.</summary>
  ///	<example>
  ///	  <code lang="Delphi">
  ///	Rules:
  ///	  # Only positive numbers.
  ///	  MinValue: 0
  ///	</code>
  ///	</example>
  TKExtMinValue = class(TKExtRuleImpl)
  protected
    procedure SetRule(const AValue: TKRule); override;
    function InternalGetErrorMessage: string; override;
  public
    procedure ApplyToFormField(const AField: TExtFormField); override;
  end;

  ///	<summary>Enforces a maximum allowed value for numeric fields. Can be used
  ///	as a field-level rule.</summary>
  ///	<example>
  ///	  <code lang="Delphi">
  ///	Rules:
  ///	  # Only allows scores 1-10.
  ///	  MinValue: 1
  ///	  MaxValue: 10
  ///	</code>
  ///	</example>
  TKExtMaxValue = class(TKExtRuleImpl)
  protected
    procedure SetRule(const AValue: TKRule); override;
    function InternalGetErrorMessage: string; override;
  public
    procedure ApplyToFormField(const AField: TExtFormField); override;
  end;

  ///	<summary>Enforces a minimum character length for fields. Can be used as a
  ///	field-level rule.</summary>
  ///	<example>
  ///	  <code lang="Delphi">
  ///	Rules:
  ///	  # A fixed-length input.
  ///	  MinLength: 5
  ///	  Maxlength: 5
  ///	</code>
  ///	</example>
  TKExtMinLength = class(TKExtRuleImpl)
  protected
    procedure SetRule(const AValue: TKRule); override;
    function InternalGetErrorMessage: string; override;
  public
    procedure ApplyToFormField(const AField: TExtFormField); override;
  end;

  ///	<summary>Enforces a maximum character length for fields. Can be used as a
  ///	field-level rule.</summary>
  ///	<example>
  ///	  <code>
  ///	Rules:
  ///	  # Limit input in memo field.
  ///	  Maxlength: 400
  ///	</code>
  ///	</example>
  TKExtMaxLength = class(TKExtRuleImpl)
  protected
    procedure SetRule(const AValue: TKRule); override;
    function InternalGetErrorMessage: string; override;
  public
    procedure ApplyToFormField(const AField: TExtFormField); override;
  end;

  ///	<summary>Sets a specified javascript function as a validation function
  ///	for a field or a record.</summary>
  ///	<remarks>
  ///	  <para>For single fields, the javascript function receives the field
  ///	  value in the <c>value</c> argument, and it should return true if
  ///	  validation passes and an error message otherwise.</para>
  ///	  <para>For records, the rule is not yet implemented.</para>
  ///	  <para>The function may hard-code the error message or use the
  ///	  <c>{errorMessage}</c> placeholder to use the rule message (stored in
  ///	  the <c>ErrorMessage</c> rule parameter). In the latter case, the
  ///	  message can be localized.</para>
  ///	</remarks>
  ///	<example>
  ///	  <code lang="Delphi">
  ///	Rules:
  ///	  # Only allows strings starting with "B".
  ///	  CharFilter: /^B*./
  ///	  Template: |
  ///	    if (value.indexOf("B") == 0)
  ///	      return true;
  ///	    return "{errorMessage}";
  ///	  ErrorMessage: Value must start with "B".
  ///	</code>
  ///	</example>
  TKExtTemplate = class(TKExtRuleImpl)
  protected
    procedure SetRule(const AValue: TKRule); override;
  public
    procedure ApplyToFormField(const AField: TExtFormField); override;
  end;

  ///	<summary>Constrains a field's input to a mask or other validity predicate
  ///	using ExtJs's <c>vtype</c> config option. Among the available vtypes:
  ///	alpha, alpha_space, alphanum, email, url. Others can be added in
  ///	application.js.</summary>
  ///	<example>
  ///	  <code lang="Delphi">
  ///	Rules:
  ///	  # Only allow alpha characters and the space.
  ///	  SubType: alpha_space
  ///	</code>
  ///	  <para>Example of adding a new vtype to ExtJS (from
  ///	  kitto-core.js):</para>
  ///	  <code lang="Delphi">
  ///	// Additional form vtypes.
  ///	var
  ///	  codice_fiscale_re = /^[a-zA-Z]{6}[0-9]{2}[abcdehlmprstABCDEHLMPRST]{1}[0-9]{2}([a-zA-Z]{1}[0-9]{3})[a-zA-Z]{1}$/,
  ///	  partita_iva_re = /^[0-9]{11}$/,
  ///	  alpha_space_re = /^[a-zA-Z_ ]+$/;
  ///	Ext.apply(Ext.form.VTypes, {
  ///	    codice_fiscale: function(val, field) {
  ///	      return codice_fiscale_re.test(val) || partita_iva_re.test(val);
  ///	    },
  ///	    codice_fiscaleText: 'Codice Fiscale non formalmente corretto.',
  ///	    codice_fiscaleMask: /[a-zA-Z0-9]/i,
  ///	    alpha_space: function(val, field) {
  ///	      return alpha_space_re.test(val);
  ///	    },
  ///	    alpha_spaceText: 'This field only accepts letters and spaces.',
  ///	    alpha_spaceMask: /[a-z ]/i
  ///	});
  ///	</code>
  ///	</example>
  TKExtSubType = class(TKExtRuleImpl)
  protected
    procedure SetRule(const AValue: TKRule); override;
  public
    procedure ApplyToFormField(const AField: TExtFormField); override;
  end;

  ///	<summary>
  ///	  <para>Applies a predefined char filter to a field. Predefined char
  ///	  filters are defined as variables of type regex in any included
  ///	  javascript library file (such as application.js, always included by
  ///	  default).</para>
  ///	  <para>Use this rule if you want to share a char filter regex among
  ///	  several instances (which is also more efficient, as the regex will only
  ///	  be compiled once). If you only need it once, you can use the simpler
  ///	  CustomCharFilter rule instead.</para>
  ///	</summary>
  ///	<example>
  ///	  <para>Rule definition:</para>
  ///	  <code lang="Delphi">
  ///	Rules:
  ///	  CharFilter: PECodeCharFilter
  ///	</code>
  ///	  <para>Mask definition:</para>
  ///	  <code lang="Delphi">
  ///	# Allows only P, E, digits and the dot.
  ///	var PECodeCharFilter = /[PE0-9.]/
  ///	</code>
  ///	</example>
  TKExtCharFilter = class(TKExtRuleImpl)
  protected
    procedure SetRule(const AValue: TKRule); override;
    function GetMask: string; virtual;
  public
    procedure ApplyToFormField(const AField: TExtFormField); override;
  end;

  ///	<summary>Applies a custom rexex as a char filter to a field.</summary>
  ///	<example>
  ///	  <code lang="Delphi">
  ///	Rules:
  ///	  CharFilter: [PE0-9.]
  ///	</code>
  ///	</example>
  TKExtCustomCharFilter = class(TKExtCharFilter)
  protected
    function GetMask: string; override;
  end;

  ///	<summary>Converts character input to upper case while typing or upon
  ///	field change.</summary>
  ///	<remarks>This rule is a base class, not registered and not meant to be
  ///	used directly.</remarks>
  TKExtForceCase = class(TKExtRuleImpl)
  protected
    procedure SetEventListener(const AField: TExtFormTextField); virtual; abstract;
  public
    procedure ApplyToFormField(const AField: TExtFormField); override;
  end;

  ///	<summary>Converts character input to upper case while typing.</summary>
  ///	<example>
  ///	  <code lang="Delphi">
  ///	Rules:
  ///	  ForceUpperCase:
  ///	</code>
  ///	</example>
  TKExtForceUpperCase = class(TKExtForceCase)
  protected
    procedure SetEventListener(const AField: TExtFormTextField); override;
  end;

  ///	<summary>Converts character input to lower case while typing.</summary>
  ///	<example>
  ///	  <code lang="Delphi">
  ///	Rules:
  ///	  ForceLowerCase:
  ///	</code>
  ///	</example>
  TKExtForceLowerCase = class(TKExtForceCase)
  protected
    procedure SetEventListener(const AField: TExtFormTextField); override;
  end;

  ///	<summary>Converts character input to camel caps (first letter of each
  ///	word upper case, rest lower case) upon field change.</summary>
  ///	<example>
  ///	  <code lang="Delphi">
  ///	Rules:
  ///	  ForceCamelCaps:
  ///	</code>
  ///	</example>
  TKExtForceCamelCaps = class(TKExtForceCase)
  protected
    procedure SetEventListener(const AField: TExtFormTextField); override;
  end;

implementation

uses
  SysUtils, StrUtils,
  ExtPascalUtils,
  EF.Tree, EF.Localization, EF.StrUtils,
  Kitto.Ext.Session;

{ TKExtRuleImpl }

procedure TKExtRuleImpl.ApplyToFormField(const AField: TExtFormField);
begin
end;

procedure TKExtRuleImpl.BeforeSubmit;
begin
end;

class function TKExtRuleImpl.GetClassId: string;
begin
  Result := StripPrefix(inherited GetClassId, 'Ext');
end;

{ TKExtMinValue }

procedure TKExtMinValue.ApplyToFormField(const AField: TExtFormField);
begin
  Assert(Assigned(AField));
  Assert(Assigned(Rule));

  inherited;
  if AField is TExtFormNumberField then
  begin
    TExtFormNumberField(AField).MinValue := Rule.AsInteger;
    TExtFormNumberField(AField).MinText := GetErrorMessage;
  end;
end;

function TKExtMinValue.InternalGetErrorMessage: string;
begin
  Result := Format(_('Value too small. Minimum value is %s.'), [Rule.AsExpandedString]);
end;

procedure TKExtMinValue.SetRule(const AValue: TKRule);
begin
  inherited;
  CheckRuleValueParam;
end;

{ TKExtMaxValue }

procedure TKExtMaxValue.ApplyToFormField(const AField: TExtFormField);
begin
  Assert(Assigned(AField));
  Assert(Assigned(Rule));

  inherited;
  if AField is TExtFormNumberField then
  begin
    TExtFormNumberField(AField).MaxValue := Rule.AsInteger;
    TExtFormNumberField(AField).MaxText := GetErrorMessage;
  end;
end;

function TKExtMaxValue.InternalGetErrorMessage: string;
begin
  Result := Format(_('Value too large. Maximum value is %s.'), [Rule.AsExpandedString]);
end;

procedure TKExtMaxValue.SetRule(const AValue: TKRule);
begin
  inherited;
  CheckRuleValueParam;
end;

{ TKExtMinLength }

procedure TKExtMinLength.ApplyToFormField(const AField: TExtFormField);
begin
  Assert(Assigned(AField));
  Assert(Assigned(Rule));

  inherited;
  if AField is TExtFormTextField then
  begin
    TExtFormTextField(AField).MinLength := Rule.AsInteger;
    TExtFormTextField(AField).MinLengthText := GetErrorMessage;
  end;
end;

function TKExtMinLength.InternalGetErrorMessage: string;
begin
  Result := Format(_('Value too short. Minimum length is %s characters.'), [Rule.AsExpandedString]);
end;

procedure TKExtMinLength.SetRule(const AValue: TKRule);
begin
  inherited;
  CheckRuleValueParam;
end;

{ TKExtMaxLength }

procedure TKExtMaxLength.ApplyToFormField(const AField: TExtFormField);
begin
  Assert(Assigned(AField));
  Assert(Assigned(Rule));

  inherited;
  if AField is TExtFormTextField then
  begin
    TExtFormTextField(AField).MaxLength := Rule.AsInteger;
    TExtFormTextField(AField).MaxLengthText := GetErrorMessage;
  end;
end;

function TKExtMaxLength.InternalGetErrorMessage: string;
begin
  Result := Format(_('Value too long. Maximum length is %s characters.'), [Rule.AsExpandedString]);
end;

procedure TKExtMaxLength.SetRule(const AValue: TKRule);
begin
  inherited;
  CheckRuleValueParam;
end;

{ TKExtTemplate }

procedure TKExtTemplate.ApplyToFormField(const AField: TExtFormField);
begin
  Assert(Assigned(AField));
  Assert(Assigned(Rule));

  inherited;
  if AField is TExtFormTextField then
  begin
    TExtFormTextField(AField).Validator :=
      TExtFormTextField(AField).JSFunction('value',
      ReplaceStr(Session.Config.MacroExpansionEngine.Expand(Rule.AsExpandedString), '{errorMessage}', StrToJS(GetErrorMessage)));
  end;
end;

procedure TKExtTemplate.SetRule(const AValue: TKRule);
begin
  inherited;
  CheckRuleValueParam;
end;

{ TKExtSubType }

procedure TKExtSubType.ApplyToFormField(const AField: TExtFormField);
begin
  Assert(Assigned(AField));
  Assert(Assigned(Rule));

  inherited;
  if AField is TExtFormTextField then
  begin
    TExtFormTextField(AField).Vtype := Rule.AsExpandedString;
    // Only set a custom error message if specified.
    if Rule.GetString('ErrorMessage') <> '' then
      TExtFormTextField(AField).VtypeText := StrToJS(GetErrorMessage);
  end;
end;

procedure TKExtSubType.SetRule(const AValue: TKRule);
begin
  inherited;
  CheckRuleValueParam;
end;

{ TKExtCharFilter }

procedure TKExtCharFilter.ApplyToFormField(const AField: TExtFormField);
begin
  Assert(Assigned(AField));
  Assert(Assigned(Rule));

  inherited;
  { TODO :
Fix ExtPascal rewrite code or reimplement as named regex
rendered in a dynamically included script. }
  if AField is TExtFormTextField then
    TExtFormTextField(AField).MaskRe := GetMask;
end;

function TKExtCharFilter.GetMask: string;
begin
  Result := Rule.AsExpandedString;
end;

procedure TKExtCharFilter.SetRule(const AValue: TKRule);
begin
  inherited;
  CheckRuleValueParam;
end;

{ TKExtCustomCharFilter }

function TKExtCustomCharFilter.GetMask: string;
begin
  { TODO : Verify that double quotes pass through making the regex interpreted as a astring. }
  Result := '"' + inherited GetMask + '"';
end;

{ TKExtForceCase }

procedure TKExtForceCase.ApplyToFormField(const AField: TExtFormField);
begin
  Assert(Assigned(AField));
  Assert(Assigned(Rule));

  inherited;
  if AField is TExtFormTextField then
  begin
    if not TExtFormTextField(AField).EnableKeyEvents then
      TExtFormTextField(AField).EnableKeyEvents := True;
    SetEventListener(TExtFormTextField(AField));
  end;
end;

{ TKExtForceUpperCase }

procedure TKExtForceUpperCase.SetEventListener(const AField: TExtFormTextField);
begin
  AField.On('keyup', AField.JSFunction('field, e', 'field.setValue(field.getRawValue().toUpperCase());'));
end;

{ TKExtForceLowerCase }

procedure TKExtForceLowerCase.SetEventListener(const AField: TExtFormTextField);
begin
  AField.On('keyup', AField.JSFunction('field, e', 'field.setValue(field.getRawValue().toLowerCase());'));
end;

{ TKExtForceCamelCaps }

procedure TKExtForceCamelCaps.SetEventListener(const AField: TExtFormTextField);
begin
  AField.On('change', AField.JSFunction('field, newValue, oldValue', 'field.setValue(newValue.capitalize());'));
end;

initialization
  TKRuleImplRegistry.Instance.RegisterClass(TKExtMinValue.GetClassId, TKExtMinValue);
  TKRuleImplRegistry.Instance.RegisterClass(TKExtMaxValue.GetClassId, TKExtMaxValue);
  TKRuleImplRegistry.Instance.RegisterClass(TKExtMinLength.GetClassId, TKExtMinLength);
  TKRuleImplRegistry.Instance.RegisterClass(TKExtMaxLength.GetClassId, TKExtMaxLength);
  TKRuleImplRegistry.Instance.RegisterClass(TKExtTemplate.GetClassId, TKExtTemplate);
  TKRuleImplRegistry.Instance.RegisterClass(TKExtSubType.GetClassId, TKExtSubType);
  TKRuleImplRegistry.Instance.RegisterClass(TKExtCharFilter.GetClassId, TKExtCharFilter);
  TKRuleImplRegistry.Instance.RegisterClass(TKExtCustomCharFilter.GetClassId, TKExtCustomCharFilter);
  TKRuleImplRegistry.Instance.RegisterClass(TKExtForceUpperCase.GetClassId, TKExtForceUpperCase);
  TKRuleImplRegistry.Instance.RegisterClass(TKExtForceLowerCase.GetClassId, TKExtForceLowerCase);
  TKRuleImplRegistry.Instance.RegisterClass(TKExtForceCamelCaps.GetClassId, TKExtForceCamelCaps);

finalization
  TKRuleImplRegistry.Instance.UnregisterClass(TKExtMinValue.GetClassId);
  TKRuleImplRegistry.Instance.UnregisterClass(TKExtMaxValue.GetClassId);
  TKRuleImplRegistry.Instance.UnregisterClass(TKExtMinLength.GetClassId);
  TKRuleImplRegistry.Instance.UnregisterClass(TKExtMaxLength.GetClassId);
  TKRuleImplRegistry.Instance.UnregisterClass(TKExtTemplate.GetClassId);
  TKRuleImplRegistry.Instance.UnregisterClass(TKExtSubType.GetClassId);
  TKRuleImplRegistry.Instance.UnregisterClass(TKExtCharFilter.GetClassId);
  TKRuleImplRegistry.Instance.UnregisterClass(TKExtCustomCharFilter.GetClassId);
  TKRuleImplRegistry.Instance.UnregisterClass(TKExtForceUpperCase.GetClassId);
  TKRuleImplRegistry.Instance.UnregisterClass(TKExtForceLowerCase.GetClassId);
  TKRuleImplRegistry.Instance.UnregisterClass(TKExtForceCamelCaps.GetClassId);

end.
