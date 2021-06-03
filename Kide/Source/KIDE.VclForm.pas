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
unit KIDE.VclForm;

interface

uses
  Classes, StdCtrls, ExtCtrls,
  KIDE.VclObjects;

Type
  // Enumerated types for properties
  TVclFormFieldInputType = (itText, itButton, itCheckbox, itFile, itHidden,
    itImage, itPassword, itRadio, itReset, itSubmit);

  TVclFormField = class;
  TVclFormTextField = class;
  TVclFormCompositeField = class;
  TVclFormNumberField = class;
(*
  TVclFormFieldSet = class;
  TVclFormFormPanel = class;
  TVclFormDateField = class;
  TVclFormComboBox = class;
  TVclFormTimeField = class;
*)
  TVclBoxComponent = class(TPanel)
  protected
    procedure InitDefaults; virtual;
  end;

  TVclContainer = class(TVclBoxComponent)
  protected
    procedure InitDefaults; override;
  end;

  TVclElement = class(TLabel);

  TVclFormField = class(TVclBoxComponent)
  private
    FDisabled: Boolean;
    FInputType: TVclFormFieldInputType; // 'text'
    FMsgTarget: string;
    FPreventMark: Boolean;
    FReadOnly: Boolean;
    FSubmitValue: Boolean;
    FTabIndex: Integer;
    FValue: string;
    FLabelJS: TVclElement;
  protected
    procedure InitDefaults; override;
  public
    property InputType: TVclFormFieldInputType read FInputType write FInputType;
    property MsgTarget: string read FMsgTarget write FMsgTarget;
    property &ReadOnly: Boolean read FReadOnly write FReadOnly;
    property TabIndex: Integer read FTabIndex write FTabIndex;
    property Value: string read FValue write FValue;
    property LabelJS: TVclElement read FLabelJS write FLabelJS;
  end;

  TVclFormLabel = class(TVclBoxComponent)
  private
    FText: string;
    procedure SetFForId(Value: string);
    procedure SetHtml(AValue: string);
    procedure SetText(const AValue: string);
  protected
    procedure InitDefaults; override;
  public
    property Text: string read FText write SetText;
  end;

  TVclFormTextField = class(TVclFormField)
  private
    FAllowBlank: Boolean; // true
    FEmptyText: string;
    FGrow: Boolean;
    FGrowMax: Integer;
    FGrowMin: Integer;
    FMaxLength: Integer;
    FMaxLengthText: string;
    // 'The maximum length for this field is {maxLength}'
    FMinLength: Integer; // 0
    FMinLengthText: string;
    // 'The minimum length for this field is {minLength}'
    FRegexText: string;
    FSelectOnFocus: Boolean;
    procedure SetAllowBlank(const AValue: Boolean);
    procedure SetEmptyText(const AValue: string);
    procedure SetGrow(const AValue: Boolean);
    procedure SetMaxLength(const AValue: Integer);
    procedure SetMaxLengthText(const AValue: string);
    procedure SetMinLength(const AValue: Integer);
    procedure SetMinLengthText(const AValue: string);
    procedure SetFRegexText(Value: string);
    procedure SetSelectOnFocus(const AValue: Boolean);
    procedure SetVtype(const AValue: string);
    procedure SetFVtypeText(const AValue: string);
  protected
    procedure InitDefaults; override;
  public
    property AllowBlank: Boolean read FAllowBlank write SetAllowBlank;
    property EmptyText: string read FEmptyText write SetEmptyText;
    property Grow: Boolean read FGrow write SetGrow;
    property GrowMax: Integer read FGrowMax write FGrowMax;
    property GrowMin: Integer read FGrowMin write FGrowMin;
    property MaxLength: Integer read FMaxLength write SetMaxLength;
    property MaxLengthText: string read FMaxLengthText write SetMaxLengthText;
    property MinLength: Integer read FMinLength write SetMinLength;
    property MinLengthText: string read FMinLengthText write SetMinLengthText;
    property RegexText: string read FRegexText write SetFRegexText;
    property SelectOnFocus: Boolean read FSelectOnFocus write SetSelectOnFocus;
  end;

  TVclFormCompositeField = class(TVclFormField)
  private
    FY: Integer;
    FDefaultMargins: string;
    FFieldErrors: TVclObjectList;
    FInnerCt: TVclContainer;
    FIsComposite: Boolean;
    FItems: TVclObjectList;
    procedure SetFY(Value: Integer);
    procedure SetFIsComposite(Value: Boolean);
    procedure SetFItems(Value: TVclObjectList);
  protected
    procedure InitDefaults; override;
  public
    property Y: Integer read FY write SetFY;
    property IsComposite: Boolean read FIsComposite write SetFIsComposite;
    property Items: TVclObjectList read FItems write SetFItems;
  end;

  TVclFormNumberField = class(TVclFormTextField)
  private
    FAllowDecimals: Boolean; // true
    FAllowNegative: Boolean; // true
    FBaseChars: string; // '0123456789'
    FDecimalPrecision: Integer; // 2
    FDecimalSeparator: string; // '.'
    procedure SetAllowDecimals(const AValue: Boolean);
    procedure SetAllowNegative(AValue: Boolean);
    procedure SetFBaseChars(Value: string);
    procedure SetDecimalPrecision(const AValue: Integer);
    procedure SetDecimalSeparator(const AValue: string);
    procedure SetFFieldClass(Value: string);
  protected
    procedure InitDefaults; override;
  public
    property AllowDecimals: Boolean read FAllowDecimals write SetAllowDecimals;
    property AllowNegative: Boolean read FAllowNegative write SetAllowNegative;
    property BaseChars: string read FBaseChars write SetFBaseChars;
    property DecimalPrecision: Integer read FDecimalPrecision write SetDecimalPrecision;
    property DecimalSeparator: string read FDecimalSeparator write SetDecimalSeparator;
  end;
implementation

{ TVclBoxComponent }

procedure TVclBoxComponent.InitDefaults;
begin
  SetBounds(0,0,100,50);
end;

procedure TVclFormTextField.SetAllowBlank(const AValue: Boolean);
begin
  FAllowBlank := AValue;
end;

procedure TVclFormTextField.SetEmptyText(const AValue: string);
begin
  FEmptyText := AValue;
end;

procedure TVclFormTextField.SetGrow(const AValue: Boolean);
begin
  FGrow := AValue;
end;

procedure TVclFormTextField.SetMaxLength(const AValue: Integer);
begin
  FMaxLength := AValue;
end;

procedure TVclFormTextField.SetMaxLengthText(const AValue: string);
begin
  FMaxLengthText := AValue;
end;

procedure TVclFormTextField.SetMinLength(const AValue: Integer);
begin
  FMinLength := AValue;
end;

procedure TVclFormTextField.SetMinLengthText(const AValue: string);
begin
  FMinLengthText := AValue;
end;

procedure TVclFormTextField.InitDefaults;
begin
  inherited;
  FAllowBlank := true;
  FGrowMax := 800;
  FGrowMin := 30;
  FMinLength := 0;
end;

{ TVclFormCompositeField }

procedure TVclFormCompositeField.SetFY(Value: Integer);
begin
  FY := Value;
end;

procedure TVclFormCompositeField.SetFItems(Value: TVclObjectList);
begin
  FItems := Value;
end;

procedure TVclFormCompositeField.InitDefaults;
begin
  inherited;
  FItems := TVclObjectList.CreateAsAttribute(Self, 'items');
end;

{ TVclFormNumberField }

procedure TVclFormNumberField.SetAllowDecimals(const AValue: Boolean);
begin
  FAllowDecimals := AValue;
  ExtSession.ResponseItems.SetConfigItem(Self, 'allowDecimals', [AValue]);
end;

procedure TVclFormNumberField.SetAllowNegative(AValue: Boolean);
begin
  FAllowNegative := AValue;
  ExtSession.ResponseItems.SetConfigItem(Self, 'allowNegative', [AValue]);
end;

procedure TVclFormNumberField.SetFBaseChars(Value: string);
begin
  FBaseChars := Value;
  JSCode('baseChars:' + VarToJSON([Value]));
end;

procedure TVclFormNumberField.SetDecimalPrecision(const AValue: Integer);
begin
  FDecimalPrecision := AValue;
  ExtSession.ResponseItems.SetConfigItem(Self, 'decimalPrecision', [AValue]);
end;

procedure TVclFormNumberField.SetDecimalSeparator(const AValue: string);
begin
  FDecimalSeparator := AValue;
  ExtSession.ResponseItems.SetConfigItem(Self, 'decimalSeparator', [AValue]);
end;

procedure TVclFormNumberField.SetFFieldClass(Value: string);
begin
  FFieldClass := Value;
  JSCode('fieldClass:' + VarToJSON([Value]));
end;

procedure TVclFormNumberField.SetMaxText(const AValue: string);
begin
  FMaxText := AValue;
  ExtSession.ResponseItems.SetConfigItem(Self, 'maxText', [AValue]);
end;

procedure TVclFormNumberField._SetMaxValue(const AValue: Integer);
begin
  FMaxValue := AValue;
  ExtSession.ResponseItems.SetConfigItem(Self, 'maxValue', 'setMaxValue', [AValue]);
end;

procedure TVclFormNumberField.SetMinText(const AValue: string);
begin
  FMinText := AValue;
  ExtSession.ResponseItems.SetConfigItem(Self, 'minText', [AValue]);
end;

procedure TVclFormNumberField._SetMinValue(const AValue: Integer);
begin
  FMinValue := AValue;
  ExtSession.ResponseItems.SetConfigItem(Self, 'minValue', 'setMinValue', [AValue]);
end;

procedure TVclFormNumberField.SetNanText(const AValue: string);
begin
  FNanText := AValue;
  ExtSession.ResponseItems.SetConfigItem(Self, 'nanText', [AValue]);
end;

class function TVclFormNumberField.JSClassName: string;
begin
  Result := 'Ext.form.NumberField';
end;

procedure TVclFormNumberField.InitDefaults;
begin
  inherited;
  FAllowDecimals := true;
  FAllowNegative := true;
  FBaseChars := '0123456789';
  FDecimalPrecision := 2;
  FDecimalSeparator := '.';
end;

end.
