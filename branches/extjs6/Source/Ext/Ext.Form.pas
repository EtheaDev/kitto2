unit Ext.Form;

interface

uses
  StrUtils, Kitto.Ext, Ext.Util, Ext.Base, Ext.Data;

type
  TExtFormAction = class;
  TExtFormBasicForm = class;
  TExtFormActionSubmit = class;
  TExtFormActionLoad = class;
  TExtFormActionDirectLoad = class;
  TExtFormActionDirectSubmit = class;
  TExtFormField = class;
  TExtFormLabel = class;
  TExtFormHidden = class;
  TExtFormTextField = class;
  TExtFormSliderField = class;
  TExtFormDisplayField = class;
  TExtFormCompositeField = class;
  TExtFormCheckbox = class;
  TExtFormCheckboxGroup = class;
  TExtFormTextArea = class;
  TExtFormTriggerField = class;
  TExtFormNumberField = class;
  TExtFormFieldContainer = class;
  TExtFormFieldSet = class;
  TExtFormFormPanel = class;
  TExtFormRadioGroup = class;
  TExtFormRadio = class;
  TExtFormDateField = class;
  TExtFormComboBox = class;
  TExtFormTwinTriggerField = class;
  TExtFormTimeField = class;

  TExtFormAction = class(TExtObject)
  private
    FFailure: TExtExpression;
    FSuccess: TExtExpression;
    FWaitMsg: string;
    FTimeout: Integer;
    FWaitTitle: string;
    FUrl: string;
    procedure SetFailure(const AValue: TExtExpression);
    procedure SetSuccess(const AValue: TExtExpression);
    procedure SetTimeout(const AValue: Integer);
    procedure SetUrl(const AValue: string);
    procedure SetWaitMsg(const AValue: string);
    procedure SetWaitTitle(const AValue: string);
  public
    class function JSClassName: string; override;
    class function CLIENT_INVALID: string;
    class function CONNECT_FAILURE: string;
    class function LOAD_FAILURE: string;
    class function SERVER_INVALID: string;
    property Failure: TExtExpression read FFailure write SetFailure;
    property Success: TExtExpression read FSuccess write SetSuccess;
    property Timeout: Integer read FTimeout write SetTimeout;
    property Url: string read FUrl write SetUrl;
    property WaitMsg: string read FWaitMsg write SetWaitMsg;
    property WaitTitle: string read FWaitTitle write SetWaitTitle;
  end;

  TExtFormBasicForm = class(TExtUtilObservable)
  private
    FApi: TExtObject;
    FBaseParams: TExtObject;
    FErrorReader: TExtDataDataReader;
    FUrl: string;
    procedure SetUrl(const AValue: string);
  protected
    procedure InitDefaults; override;
  public
    class function JSClassName: string; override;
    function GetFieldValues(const ADirtyOnly: Boolean = False): TExtExpression;
    function GetValues(const AAsString: Boolean = False): TExtExpression;
    function Load(const AOptions: TExtObject): TExtExpression;
    function Submit(const AOptions: TExtObject): TExtExpression;
    property Url: string read FUrl write SetUrl;
  end;

  TExtFormActionSubmit = class(TExtFormAction)
  public
    class function JSClassName: string; override;
  end;

  TExtFormActionLoad = class(TExtFormAction)
  public
    class function JSClassName: string; override;
  end;

  TExtFormActionDirectLoad = class(TExtFormActionLoad)
  public
    class function JSClassName: string; override;
  end;

  TExtFormActionDirectSubmit = class(TExtFormActionSubmit)
  public
    class function JSClassName: string; override;
  end;

  // Procedural types for events TExtFormField
  TExtFormFieldOnChange = procedure(This: TExtFormField; NewValue: string;
    OldValue: string) of object;

  // Enumerated types for properties
  TExtFormFieldInputType = (itText, itButton, itCheckbox, itFile, itHidden,
    itImage, itPassword, itRadio, itReset, itSubmit);

  TExtFormField = class(TExtBoxComponent)
  private
    FOnChange: TExtFormFieldOnChange;
    FName: string;
    FCls: string;
    FLabelAlign: TExtContainerLabelAlign;
    FAutoCreate: string;
    FInputType: TExtFormFieldInputType;
    FLabelWidth: Integer;
    FValue: string;
    FSubmitValue: Boolean;
    FDisabled: Boolean;
    FReadOnly: Boolean;
    FMsgTarget: string;
    FStartValue: string;
    procedure SetAutoCreate(const AValue: string);
    procedure SetCls(const AValue: string);
    procedure SetDisabled(const AValue: Boolean);
    procedure SetInputType(const AValue: TExtFormFieldInputType);
    procedure SetMsgTarget(const AValue: string);
    procedure _SetName(const AValue: string);
    procedure _SetReadOnly(const AValue: Boolean);
    procedure SetSubmitValue(const AValue: Boolean);
    procedure _SetValue(const AValue: string);
    procedure SetLabelAlign(const AValue: TExtContainerLabelAlign);
    procedure SetLabelWidth(const AValue: Integer);
    procedure SetStartValue(const AValue: string);
    procedure SetOnChange(const AValue: TExtFormFieldOnChange);
  protected
    procedure HandleEvent(const AEvtName: string); override;
    function GetObjectNamePrefix: string; override;
  public
    class function JSClassName: string; override;
    function GetRawValue: TExtExpression;
    function GetValue: TExtExpression;
    function SetRawValue(const AValue: string): TExtExpression;
    function SetReadOnly(const AReadOnly: Boolean): TExtExpression;
    function SetValue(const AValue: string): TExtExpression;
    property AutoCreate: string read FAutoCreate write SetAutoCreate;
    property Cls: string read FCls write SetCls;
    property Disabled: Boolean read FDisabled write SetDisabled;
    property InputType: TExtFormFieldInputType read FInputType
      write SetInputType;
    property LabelAlign: TExtContainerLabelAlign read FLabelAlign write SetLabelAlign;
    property LabelWidth: Integer read FLabelWidth write SetLabelWidth;
    property MsgTarget: string read FMsgTarget write SetMsgTarget;
    property Name: string read FName write _SetName;
    property &ReadOnly: Boolean read FReadOnly write _SetReadOnly;
    property StartValue: string read FStartValue write SetStartValue;
    property SubmitValue: Boolean read FSubmitValue write SetSubmitValue;
    property Value: string read FValue write _SetValue;
    property OnChange: TExtFormFieldOnChange read FOnChange write SetOnChange;
  end;

  TExtFormLabel = class(TExtBoxComponent)
  private
    FHtml: string;
    FText: string;
    procedure SetHtml(AValue: string);
    procedure _SetText(const AValue: string);
  public
    class function JSClassName: string; override;
    function SetText(const AText: string; const AEncode: Boolean = False): TExtExpression;
    property Html: string read FHtml write SetHtml;
    property Text: string read FText write _SetText;
  end;

  TExtFormHidden = class(TExtFormField)
  protected
    procedure InitDefaults; override;
  public
    class function JSClassName: string; override;
  end;

  TExtFormTextField = class(TExtFormField)
  private
    FMaxLength: Integer;
    FGrow: Boolean;
    FEmptyClass: string;
    FMinLengthText: string;
    FEnableKeyEvents: Boolean;
    FVtypeText: string;
    FMinLength: Integer;
    FMaskRe: string;
    FAllowBlank: Boolean;
    FVtype: string;
    FSelectOnFocus: Boolean;
    FBlankText: string;
    FEmptyText: string;
    FMaxLengthText: string;
    FValidator: TExtExpression;
    procedure SetAllowBlank(const AValue: Boolean);
    procedure SetBlankText(const AValue: string);
    procedure SetEmptyClass(const AValue: string);
    procedure SetEmptyText(const AValue: string);
    procedure SetEnableKeyEvents(const AValue: Boolean);
    procedure SetGrow(const AValue: Boolean);
    procedure SetMaxLength(const AValue: Integer);
    procedure SetMaxLengthText(const AValue: string);
    procedure SetMinLength(const AValue: Integer);
    procedure SetMinLengthText(const AValue: string);
    procedure SetSelectOnFocus(const AValue: Boolean);
    procedure SetVtype(const AValue: string);
    procedure SetVtypeText(const AValue: string);
    procedure SetValidator(const AValue: TExtExpression);
    procedure SetMaskRe(const AValue: string);
  protected
    procedure InitDefaults; override;
    function GetObjectNamePrefix: string; override;
  public
    class function JSClassName: string; override;
    property AllowBlank: Boolean read FAllowBlank write SetAllowBlank;
    property BlankText: string read FBlankText write SetBlankText;
    property EmptyClass: string read FEmptyClass write SetEmptyClass;
    property EmptyText: string read FEmptyText write SetEmptyText;
    property EnableKeyEvents: Boolean read FEnableKeyEvents write SetEnableKeyEvents;
    property Grow: Boolean read FGrow write SetGrow;
    property MaskRe: string read FMaskRe write SetMaskRe;
    property MaxLength: Integer read FMaxLength write SetMaxLength;
    property MaxLengthText: string read FMaxLengthText write SetMaxLengthText;
    property MinLength: Integer read FMinLength write SetMinLength;
    property MinLengthText: string read FMinLengthText write SetMinLengthText;
    property SelectOnFocus: Boolean read FSelectOnFocus write SetSelectOnFocus;
    property Validator: TExtExpression read FValidator write SetValidator;
    property Vtype: string read FVtype write SetVtype;
    property VtypeText: string read FVtypeText write SetVtypeText;
  end;

  TExtFormSliderField = class(TExtFormField)
  public
    class function JSClassName: string; override;
  end;

  TExtFormDisplayField = class(TExtFormField)
  public
    class function JSClassName: string; override;
  end;

  TExtFormCompositeField = class(TExtFormField)
  private
    FItems: TExtObjectList;
  protected
    function GetObjectNamePrefix: string; override;
    procedure InitDefaults; override;
  public
    class function JSClassName: string; override;
    property Items: TExtObjectList read FItems;
  end;

  // Procedural types for events TExtFormCheckbox
  TExtFormCheckboxOnCheck = procedure(This: TExtFormCheckbox; Checked: Boolean) of object;

  TExtFormCheckbox = class(TExtFormField)
  private
    FOnCheck: TExtFormCheckboxOnCheck;
    FBoxLabel: string;
    FChecked: Boolean;
    procedure SetBoxLabel(const AValue: string);
    procedure SetChecked(const AValue: Boolean);
    procedure SetOnCheck(Value: TExtFormCheckboxOnCheck);
  protected
    procedure InitDefaults; override;
    procedure HandleEvent(const AEvtName: string); override;
    function GetObjectNamePrefix: string; override;
  public
    class function JSClassName: string; override;
    function GetValue: TExtExpression;
    function SetValue(const AChecked: Boolean): TExtExpression; overload;
    function SetValue(const AChecked: string): TExtExpression; overload;
    property BoxLabel: string read FBoxLabel write SetBoxLabel;
    property Checked: Boolean read FChecked write SetChecked;
    property OnCheck: TExtFormCheckboxOnCheck read FOnCheck write SetOnCheck;
  end;

  // Procedural types for events TExtFormCheckboxGroup
  TExtFormCheckboxGroupOnChange = procedure(This: TExtFormCheckboxGroup;
    Checked: TExtObjectList) of object;

  TExtFormCheckboxGroup = class(TExtFormField)
  public
    class function JSClassName: string; override;
  end;

  TExtFormTextArea = class(TExtFormTextField)
  protected
    function GetObjectNamePrefix: string; override;
  public
    class function JSClassName: string; override;
  end;

  TExtFormTriggerField = class(TExtFormTextField)
  private
    FTriggerClass: string;
    FEditable: Boolean;
    procedure _SetEditable(const AValue: Boolean);
    procedure SetTriggerClass(const AValue: string);
  protected
    procedure InitDefaults; override;
  public
    class function JSClassName: string; override;
    function SetEditable(const AValue: Boolean): TExtExpression;
    function SetReadOnly(const AValue: Boolean): TExtExpression;
    property Editable: Boolean read FEditable write _SetEditable;
    property TriggerClass: string read FTriggerClass write SetTriggerClass;
  end;

  TExtFormNumberField = class(TExtFormTextField)
  private
    FAllowDecimals: Boolean; // true
    FAllowNegative: Boolean; // true
    FBaseChars: string; // '0123456789'
    FDecimalPrecision: Integer; // 2
    FDecimalSeparator: string; // '.'
    FFieldClass: string; // 'x-form-field x-form-num-field'
    FMaxText: string; // 'The maximum value for this field is {maxValue}'
    FMaxValue: Integer;
    FMinText: string; // 'The minimum value for this field is {minValue}'
    FMinValue: Integer;
    FNanText: string; // '{value} is not a valid number'
    procedure SetAllowDecimals(const AValue: Boolean);
    procedure SetAllowNegative(AValue: Boolean);
    procedure SetDecimalPrecision(const AValue: Integer);
    procedure SetDecimalSeparator(const AValue: string);
    procedure SetMaxText(const AValue: string);
    procedure _SetMaxValue(const AValue: Integer);
    procedure SetMinText(const AValue: string);
    procedure _SetMinValue(const AValue: Integer);
    procedure SetNanText(const AValue: string);
  protected
    procedure InitDefaults; override;
    function GetObjectNamePrefix: string; override;
  public
    class function JSClassName: string; override;
    function SetMaxValue(const AValue: Integer): TExtExpression;
    function SetMinValue(const AValue: Integer): TExtExpression;
    property AllowDecimals: Boolean read FAllowDecimals write SetAllowDecimals;
    property AllowNegative: Boolean read FAllowNegative write SetAllowNegative;
    property DecimalPrecision: Integer read FDecimalPrecision write SetDecimalPrecision;
    property DecimalSeparator: string read FDecimalSeparator write SetDecimalSeparator;
    property MaxText: string read FMaxText write SetMaxText;
    property MaxValue: Integer read FMaxValue write _SetMaxValue;
    property MinText: string read FMinText write SetMinText;
    property MinValue: Integer read FMinValue write _SetMinValue;
    property NanText: string read FNanText write SetNanText;
  end;

  TExtFormFieldSet = class(TExtPanel)
  private
    FCollapsible: Boolean;
    FLabelWidth: Integer;
    procedure SetCollapsible(const AValue: Boolean);
    procedure SetLabelWidth(AValue: Integer);
  protected
    function GetObjectNamePrefix: string; override;
  public
    class function JSClassName: string; override;
    property Collapsible: Boolean read FCollapsible write SetCollapsible;
    property LabelWidth: Integer read FLabelWidth write SetLabelWidth;
  end;

  TExtFormFormPanel = class(TExtPanel)
  private
    FMonitorValid: Boolean;
    FLabelSeparator: string;
    FForm: TExtFormBasicForm;
    FFileUpload: Boolean;
    FLabelPad: Integer;
    procedure SetLabelSeparator(const AValue: string);
    procedure SetMonitorValid(const AValue: Boolean);
    procedure SetFileUpload(const AValue: Boolean);
    procedure SetLabelPad(const AValue: Integer);
  protected
    procedure InitDefaults; override;
  public
    class function JSClassName: string; override;
    function Load(const AOptions: TExtObject): TExtExpression;
    property Form: TExtFormBasicForm read FForm;
    property LabelPad: Integer read FLabelPad write SetLabelPad;
    property LabelSeparator: string read FLabelSeparator write SetLabelSeparator;
    property MonitorValid: Boolean read FMonitorValid write SetMonitorValid;
    property FileUpload: Boolean read FFileUpload write SetFileUpload;
  end;

  // Procedural types for events TExtFormRadioGroup
  TExtFormRadioGroupOnChange = procedure(This: TExtFormRadioGroup;
    Checked: TExtFormRadio) of object;

  TExtFormRadioGroup = class(TExtFormCheckboxGroup)
  public
    class function JSClassName: string; override;
  end;

  TExtFormRadio = class(TExtFormCheckbox)
  public
    class function JSClassName: string; override;
  end;

  TExtFormDateField = class(TExtFormTriggerField)
  private
    FMinValue: TDateTime;
    FDisabledDates: TExtObjectList;
    FFormat: string;
    FMaxValue: TDateTime;
    FAltFormats: string;
    FDisabledDays: TExtObjectList;
    procedure SetAltFormats(const AValue: string);
    procedure _SetDisabledDates(const AValue: TExtObjectList);
    procedure _SetDisabledDays(const AValue: TExtObjectList);
    procedure SetFormat(const AValue: string);
    procedure _SetMaxValue(const AValue: TDateTime);
    procedure _SetMinValue(const AValue: TDateTime);
  protected
    procedure InitDefaults; override;
    function GetObjectNamePrefix: string; override;
  public
    class function JSClassName: string; override;
    function GetValue: TExtExpression;
    function SetDisabledDates(const ADisabledDates: TExtObjectList): TExtExpression;
    function SetDisabledDays(const ADisabledDays: TExtObjectList): TExtExpression;
    function SetMaxValue(const AValue: TDateTime): TExtExpression;
    function SetMinValue(const AValue: TDateTime): TExtExpression;
    function SetValue(const ADate: string): TExtExpression; overload;
    function SetValue(const ADate: TDateTime): TExtExpression; overload;
    property AltFormats: string read FAltFormats write SetAltFormats;
    property DisabledDates: TExtObjectList read FDisabledDates
      write _SetDisabledDates;
    property DisabledDays: TExtObjectList read FDisabledDays
      write _SetDisabledDays;
    property Format: string read FFormat write SetFormat;
    property MaxValue: TDateTime read FMaxValue write _SetMaxValue;
    property MinValue: TDateTime read FMinValue write _SetMinValue;
  end;

  // Procedural types for events TExtFormComboBox
  TExtFormComboBoxOnSelect = procedure(Combo: TExtFormComboBox;
    RecordJS: TExtDataRecord; Index: Integer) of object;

  TExtFormComboBox = class(TExtFormTriggerField)
  private
    FOnSelect: TExtFormComboBoxOnSelect;
    FSelectedClass: string;
    FTypeAheadDelay: Integer;
    FValueField: string;
    FForceSelection: Boolean;
    FListConfig: TExtObject;
    FHiddenValue: string;
    FMode: string;
    FListWidth: Integer;
    FTypeAhead: Boolean;
    FMinHeight: Integer;
    FAutoSelect: Boolean;
    FLazyRender: Boolean;
    FTriggerAction: string;
    FQueryDelay: Integer;
    FMinChars: Integer;
    FDisplayField: string;
    FHiddenName: string;
    FValueNotFoundText: string;
    FMinListWidth: Integer;
    FStoreArray: TExtObjectList;
    FPageSize: Integer;
    FQueryParam: string;
    FListWidthFunc: TExtExpression;
    FMinHeightFunc: TExtExpression;
    FStore: TExtDataStore;
    procedure SetAutoSelect(const AValue: Boolean);
    procedure SetDisplayField(const AValue: string);
    procedure SetForceSelection(const AValue: Boolean);
    procedure SetHiddenName(const AValue: string);
    procedure SetHiddenValue(const AValue: string);
    procedure SetLazyRender(const AValue: Boolean);
    procedure SetListWidth(const AValue: Integer);
    procedure SetMinChars(const AValue: Integer);
    procedure SetMinHeight(const AValue: Integer);
    procedure SetMinListWidth(const AValue: Integer);
    procedure SetMode(const AValue: string);
    procedure SetPageSize(const AValue: Integer);
    procedure SetQueryDelay(const AValue: Integer);
    procedure SetQueryParam(const AValue: string);
    procedure SetSelectedClass(const AValue: string);
    procedure SetStore(const AValue: TExtDataStore);
    procedure SetStoreArray(const AValue: TExtObjectList);
    procedure SetTriggerAction(const AValue: string);
    procedure SetTypeAhead(const AValue: Boolean);
    procedure SetTypeAheadDelay(const AValue: Integer);
    procedure SetValueField(const AValue: string);
    procedure SetValueNotFoundText(const AValue: string);
    procedure SetMinHeightFunc(const AValue: TExtExpression);
    procedure SetListWidthFunc(const AValue: TExtExpression);
    procedure SetOnSelect(const AValue: TExtFormComboBoxOnSelect);
  protected
    procedure InitDefaults; override;
    procedure HandleEvent(const AEvtName: string); override;
    function GetObjectNamePrefix: string; override;
  public
    class function JSClassName: string; override;
    function ClearValue: TExtExpression;
    function Expand: TExtExpression;
    function GetListParent: TExtExpression;
    function GetStore: TExtExpression;
    function GetValue: TExtExpression;
    function IsExpanded: TExtExpression;
    function SetValue(const AValue: string): TExtExpression;
    property AutoSelect: Boolean read FAutoSelect write SetAutoSelect;
    property DisplayField: string read FDisplayField write SetDisplayField;
    property ForceSelection: Boolean read FForceSelection write SetForceSelection;
    property HiddenName: string read FHiddenName write SetHiddenName;
    property HiddenValue: string read FHiddenValue write SetHiddenValue;
    property LazyRender: Boolean read FLazyRender write SetLazyRender;
    property ListConfig: TExtObject read FListConfig;
    property ListWidth: Integer read FListWidth write SetListWidth;
    property ListWidthFunc: TExtExpression read FListWidthFunc write SetListWidthFunc;
    property MinChars: Integer read FMinChars write SetMinChars;
    property MinHeight: Integer read FMinHeight write SetMinHeight;
    property MinHeightFunc: TExtExpression read FMinHeightFunc write SetMinHeightFunc;
    property MinListWidth: Integer read FMinListWidth write SetMinListWidth;
    property Mode: string read FMode write SetMode;
    property PageSize: Integer read FPageSize write SetPageSize;
    property QueryDelay: Integer read FQueryDelay write SetQueryDelay;
    property QueryParam: string read FQueryParam write SetQueryParam;
    property SelectedClass: string read FSelectedClass write SetSelectedClass;
    property Store: TExtDataStore read FStore write SetStore;
    property StoreArray: TExtObjectList read FStoreArray write SetStoreArray;
    property TriggerAction: string read FTriggerAction write SetTriggerAction;
    property TypeAhead: Boolean read FTypeAhead write SetTypeAhead;
    property TypeAheadDelay: Integer read FTypeAheadDelay
      write SetTypeAheadDelay;
    property ValueField: string read FValueField write SetValueField;
    property ValueNotFoundText: string read FValueNotFoundText
      write SetValueNotFoundText;
    property OnSelect: TExtFormComboBoxOnSelect read FOnSelect write SetOnSelect;
  end;

  TExtFormTwinTriggerField = class(TExtFormTriggerField)
  private
    FTrigger1Class: string;
    FTrigger2Class: string;
    FTriggerConfig: TExtObject;
    procedure SetTrigger1Class(const AValue: string);
    procedure SetTrigger2Class(const AValue: string);
  protected
    procedure InitDefaults; override;
  public
    class function JSClassName: string; override;
    property Trigger1Class: string read FTrigger1Class write SetTrigger1Class;
    property Trigger2Class: string read FTrigger2Class write SetTrigger2Class;
    property TriggerConfig: TExtObject read FTriggerConfig;
  end;

  TExtFormTimeField = class(TExtFormComboBox)
  private
    FAltFormats: string;
    // 'g:ia/g:iA/g:i a/g:i A/h:i/g:i/H:i/ga/ha/gA/h a/g a/g A/gi/hi/gia/hia/g/H/gi a/hi a/giA/hiA/gi A/hi A'
    FFormat: string; // 'g:i A'
    FIncrement: Integer; // 15
    FInvalidText: string; // '{value} is not a valid time'
    FMaxText: string; // 'The time in this field must be equal to or before {0}'
    FMaxValue: TDateTime;
    FMinText: string; // 'The time in this field must be equal to or after {0}'
    FMinValue: TDateTime;
    procedure SetAltFormats(const AValue: string);
    procedure SetFormat(const AValue: string);
    procedure _SetMaxValue(const AValue: TDateTime);
    procedure _SetMinValue(const AValue: TDateTime);
  protected
    procedure InitDefaults; override;
    function GetObjectNamePrefix: string; override;
  public
    class function JSClassName: string; override;
    function SetMaxValue(const AValue: TDateTime): TExtExpression; overload;
    function SetMinValue(const AValue: TDateTime): TExtExpression; overload;
    property AltFormats: string read FAltFormats write SetAltFormats;
    property Format: string read FFormat write SetFormat;
    property MaxValue: TDateTime read FMaxValue write _SetMaxValue;
    property MinValue: TDateTime read FMinValue write _SetMinValue;
  end;

  TExtFormFieldContainer = class(TExtContainer)
  public
    class function JSClassName: string; override;
  end;

implementation

uses
  Kitto.JS;

procedure TExtFormAction.SetFailure(const AValue: TExtExpression);
begin
  FFailure := SetConfigItem('failure', AValue);
end;

procedure TExtFormAction.SetSuccess(const AValue: TExtExpression);
begin
  FSuccess := SetConfigItem('success', AValue);
end;

procedure TExtFormAction.SetTimeout(const AValue: Integer);
begin
  FTimeout := SetConfigItem('timeout', AValue);
end;

procedure TExtFormAction.SetUrl(const AValue: string);
begin
  FUrl := SetConfigItem('url', AValue);
end;

procedure TExtFormAction.SetWaitMsg(const AValue: string);
begin
  FWaitMsg := SetConfigItem('waitMsg', AValue);
end;

procedure TExtFormAction.SetWaitTitle(const AValue: string);
begin
  FWaitTitle := SetConfigItem('waitTitle', AValue);
end;

class function TExtFormAction.JSClassName: string;
begin
  Result := '';
end;

class function TExtFormAction.CLIENT_INVALID: string;
begin
  Result := ''
end;

class function TExtFormAction.CONNECT_FAILURE: string;
begin
  Result := ''
end;

class function TExtFormAction.LOAD_FAILURE: string;
begin
  Result := ''
end;

class function TExtFormAction.SERVER_INVALID: string;
begin
  Result := ''
end;

procedure TExtFormBasicForm.SetUrl(const AValue: string);
begin
  FUrl := SetConfigItem('url', AValue);
end;

class function TExtFormBasicForm.JSClassName: string;
begin
  Result := 'Ext.form.BasicForm';
end;

procedure TExtFormBasicForm.InitDefaults;
begin
  inherited;
  FApi := TExtObject.CreateInternal(Self, 'api');
  FBaseParams := TExtObject.CreateInternal(Self, 'baseParams');
  FErrorReader := TExtDataDataReader.CreateInternal(Self, 'errorReader');
end;

function TExtFormBasicForm.GetFieldValues(const ADirtyOnly: Boolean): TExtExpression;
begin
  Result := CallMethod('getFieldValues')
    .AddParam(ADirtyOnly)
    .AsExpression;
end;

function TExtFormBasicForm.GetValues(const AAsString: Boolean): TExtExpression;
begin
  Result := CallMethod('getValues')
    .AddParam(AAsString)
    .AsExpression;
end;

function TExtFormBasicForm.Load(const AOptions: TExtObject): TExtExpression;
begin
  Result := CallMethod('load')
    .AddParam(AOptions)
    .AsExpression;
end;

function TExtFormBasicForm.Submit(const AOptions: TExtObject): TExtExpression;
begin
  Result := CallMethod('submit')
    .AddParam(AOptions)
    .AsExpression;
end;

class function TExtFormActionSubmit.JSClassName: string;
begin
  // Don't use real class name. This object is never created
  // on the server: using object allows to create it as a JS generator helper.
  Result := 'Object';
end;

class function TExtFormActionLoad.JSClassName: string;
begin
  // Don't use real class name. This object is never created
  // on the server: using object allows to create it as a JS generator helper.
  Result := 'Object';
end;

class function TExtFormActionDirectLoad.JSClassName: string;
begin
  Result := 'Ext.form.Action.DirectLoad';
end;

class function TExtFormActionDirectSubmit.JSClassName: string;
begin
  Result := 'Ext.form.Action.DirectSubmit';
end;

procedure TExtFormField.SetCls(const AValue: string);
begin
  FCls := SetConfigItem('cls', AValue);
end;

procedure TExtFormField.SetDisabled(const AValue: Boolean);
begin
  FDisabled := SetConfigItem('disabled', 'setDisabled', AValue);
end;

procedure TExtFormField.SetAutoCreate(const AValue: string);
begin
  FAutoCreate := SetConfigItem('autoCreate', AValue);
end;

procedure TExtFormField.SetInputType(const AValue: TExtFormFieldInputType);
begin
  FInputType := AValue;
  SetConfigItem('inputType', TJS.EnumToJSString(TypeInfo(TExtFormFieldInputType), Ord(AValue)));
end;

procedure TExtFormField.SetLabelAlign(const AValue: TExtContainerLabelAlign);
begin
  FLabelAlign := AValue;
  SetConfigItem('labelAlign', TJS.EnumToJSString(TypeInfo(TExtContainerLabelAlign), Ord(AValue)));
end;

procedure TExtFormField.SetLabelWidth(const AValue: Integer);
begin
  FLabelWidth := SetConfigItem('labelWidth', AValue);
end;

procedure TExtFormField.SetMsgTarget(const AValue: string);
begin
  FMsgTarget := SetConfigItem('msgTarget', AValue);
end;

procedure TExtFormField._SetName(const AValue: string);
begin
  FName := SetConfigItem('name', AValue);
end;

procedure TExtFormField._SetReadOnly(const AValue: Boolean);
begin
  FReadOnly := SetConfigItem('readOnly', 'setReadOnly', AValue);
end;

procedure TExtFormField.SetSubmitValue(const AValue: Boolean);
begin
  FSubmitValue := SetConfigItem('submitValue', AValue);
end;

procedure TExtFormField._SetValue(const AValue: string);
begin
  FValue := SetConfigItem('value', 'setValue', AValue);
end;

procedure TExtFormField.SetStartValue(const AValue: string);
begin
  FStartValue := SetConfigItem('startValue', AValue);
end;

class function TExtFormField.JSClassName: string;
begin
  Result := 'Ext.form.Field';
end;

function TExtFormField.GetObjectNamePrefix: string;
begin
  Result := 'ffld';
end;

function TExtFormField.GetRawValue: TExtExpression;
begin
  Result := CallMethod('getRawValue').AsExpression;
end;

function TExtFormField.GetValue: TExtExpression;
begin
  Result := CallMethod('getValue').AsExpression;
end;

function TExtFormField.SetRawValue(const AValue: string): TExtExpression;
begin
  Result := CallMethod('setRawValue')
    .AddParam(AValue)
    .AsExpression;
end;

function TExtFormField.SetReadOnly(const AReadOnly: Boolean): TExtExpression;
begin
  Result := CallMethod('setReadOnly')
    .AddParam(AReadOnly)
    .AsExpression;
end;

function TExtFormField.SetValue(const AValue: string): TExtExpression;
begin
  Result := CallMethod('setValue')
    .AddParam(AValue)
    .AsExpression;
end;

procedure TExtFormField.HandleEvent(const AEvtName: string);
begin
  inherited;
  if (AEvtName = 'change') and Assigned(FOnChange) then
    FOnChange(TExtFormField(ParamAsObject('This')), ParamAsString('NewValue'),
      ParamAsString('OldValue'));
end;

procedure TExtFormField.SetOnChange(const AValue: TExtFormFieldOnChange);
begin
  RemoveAllListeners('change');
  if Assigned(AValue) then
    //On('change', Ajax('change', ['This', '%0.nm', 'NewValue', '%1', 'OldValue', '%2'], True));
    &On('change', AjaxCallMethod('change')
      .Event
      .AddRawParam('This', 'sender.nm')
      .AddRawParam('NewValue', 'new')
      .AddRawParam('OldValue', 'old')
      .FunctionArgs('sender, new, old')
      .AsFunction);
  FOnChange := AValue;
end;

procedure TExtFormLabel.SetHtml(AValue: string);
begin
  FHtml := SetConfigItem('html', AValue);
end;

procedure TExtFormLabel._SetText(const AValue: string);
begin
  FText := SetConfigItem('text', 'setText', AValue);
end;

class function TExtFormLabel.JSClassName: string;
begin
  Result := 'Ext.form.Label';
end;

function TExtFormLabel.SetText(const AText: string; const AEncode: Boolean): TExtExpression;
begin
  FText := AText;
  Result := CallMethod('setText')
    .AddParam(AText)
    .AddParam(AEncode)
    .AsExpression;
end;

class function TExtFormHidden.JSClassName: string;
begin
  Result := 'Ext.form.Hidden';
end;

procedure TExtFormHidden.InitDefaults;
begin
  inherited;
end;

procedure TExtFormTextField.SetAllowBlank(const AValue: Boolean);
begin
  FAllowBlank := AValue;
  SetConfigItem('allowBlank', AValue);
end;

procedure TExtFormTextField.SetBlankText(const AValue: string);
begin
  FBlankText := SetConfigItem('blankText', AValue);
end;

procedure TExtFormTextField.SetEmptyClass(const AValue: string);
begin
  FEmptyClass := SetConfigItem('emptyClass', AValue);
end;

procedure TExtFormTextField.SetEmptyText(const AValue: string);
begin
  FEmptyText := SetConfigItem('emptyText', AValue);
end;

procedure TExtFormTextField.SetEnableKeyEvents(const AValue: Boolean);
begin
  FEnableKeyEvents := SetConfigItem('enableKeyEvents', AValue);
end;

procedure TExtFormTextField.SetGrow(const AValue: Boolean);
begin
  FGrow := SetConfigItem('grow', AValue);
end;

procedure TExtFormTextField.SetMaskRe(const AValue: string);
begin
  FMaskRe := SetConfigItem('maskRe', AValue);
end;

procedure TExtFormTextField.SetMaxLength(const AValue: Integer);
begin
  FMaxLength := SetConfigItem('maxLength', AValue);
end;

procedure TExtFormTextField.SetMaxLengthText(const AValue: string);
begin
  FMaxLengthText := SetConfigItem('maxLengthText', AValue);
end;

procedure TExtFormTextField.SetMinLength(const AValue: Integer);
begin
  FMinLength := SetConfigItem('minLength', AValue);
end;

procedure TExtFormTextField.SetMinLengthText(const AValue: string);
begin
  FMinLengthText := SetConfigItem('minLengthText', AValue);
end;

procedure TExtFormTextField.SetSelectOnFocus(const AValue: Boolean);
begin
  FSelectOnFocus := SetConfigItem('selectOnFocus', AValue);
end;

procedure TExtFormTextField.SetValidator(const AValue: TExtExpression);
begin
  FValidator := SetConfigItem('validator', AValue);
end;

procedure TExtFormTextField.SetVtype(const AValue: string);
begin
  FVtype := SetConfigItem('vtype', AValue);
end;

procedure TExtFormTextField.SetVtypeText(const AValue: string);
begin
  FVtypeText := SetConfigItem('vtypeText', AValue);
end;

class function TExtFormTextField.JSClassName: string;
begin
  Result := 'Ext.form.TextField';
end;

procedure TExtFormTextField.InitDefaults;
begin
  inherited;
  FAllowBlank := true;
  FBlankText := 'This field is required';
  FEmptyClass := 'x-form-empty-field';
  FMaxLengthText := 'The maximum length for this field is {maxLength}';
  FMinLength := 0;
  FMinLengthText := 'The minimum length for this field is {minLength}';
end;

function TExtFormTextField.GetObjectNamePrefix: string;
begin
  Result := 'txtfld';
end;

class function TExtFormSliderField.JSClassName: string;
begin
  Result := 'Ext.form.SliderField';
end;

class function TExtFormDisplayField.JSClassName: string;
begin
  Result := 'Ext.form.DisplayField';
end;

procedure TExtFormCompositeField.InitDefaults;
begin
  inherited;
  FItems := CreateConfigArray('items');
end;

class function TExtFormCompositeField.JSClassName: string;
begin
  Result := 'Ext.form.CompositeField';
end;

function TExtFormCompositeField.GetObjectNamePrefix: string;
begin
  Result := 'compfld';
end;

procedure TExtFormCheckbox.SetBoxLabel(const AValue: string);
begin
  FBoxLabel := SetConfigItem('boxLabel', AValue);
end;

procedure TExtFormCheckbox.SetChecked(const AValue: Boolean);
begin
  FChecked := SetConfigItem('checked', AValue);
end;

procedure TExtFormCheckbox.SetOnCheck(Value: TExtFormCheckboxOnCheck);
begin
  RemoveAllListeners('check');
  if Assigned(Value) then
    //On('check', Ajax('check', ['This', '%0.nm', 'Checked', '%1'], True));
    &On('check', AjaxCallMethod('check')
      .Event
      .AddRawParam('This', 'sender.nm')
      .AddRawParam('Checked', 'checked')
      .FunctionArgs('sender, checked')
      .AsFunction);
  FOnCheck := Value;
end;

class function TExtFormCheckbox.JSClassName: string;
begin
  Result := 'Ext.form.Checkbox';
end;

procedure TExtFormCheckbox.InitDefaults;
begin
  inherited;
  FAutoCreate := 'input';
end;

function TExtFormCheckbox.GetObjectNamePrefix: string;
begin
  Result := 'chkbox';
end;

function TExtFormCheckbox.GetValue: TExtExpression;
begin
  Result := CallMethod('getValue').AsExpression;
end;

function TExtFormCheckbox.SetValue(const AChecked: Boolean): TExtExpression;
begin
  Result := CallMethod('setValue').AddParam(AChecked).AsExpression;
end;

function TExtFormCheckbox.SetValue(const AChecked: string): TExtExpression;
begin
  Result := CallMethod('setValue').AddParam(AChecked).AsExpression;
end;

procedure TExtFormCheckbox.HandleEvent(const AEvtName: string);
begin
  inherited;
  if (AEvtName = 'check') and Assigned(FOnCheck) then
    FOnCheck(TExtFormCheckbox(ParamAsObject('This')),
      ParamAsBoolean('Checked'));
end;

class function TExtFormCheckboxGroup.JSClassName: string;
begin
  Result := 'Ext.form.CheckboxGroup';
end;

class function TExtFormTextArea.JSClassName: string;
begin
  Result := 'Ext.form.TextArea';
end;

function TExtFormTextArea.GetObjectNamePrefix: string;
begin
  Result := 'txtarea';
end;

procedure TExtFormTriggerField._SetEditable(const AValue: Boolean);
begin
  FEditable := SetConfigItem('editable', 'setEditable', AValue);
end;

procedure TExtFormTriggerField.SetTriggerClass(const AValue: string);
begin
  FTriggerClass := SetConfigItem('triggerClass', AValue);
end;

class function TExtFormTriggerField.JSClassName: string;
begin
  Result := 'Ext.form.TriggerField';
end;

procedure TExtFormTriggerField.InitDefaults;
begin
  inherited;
  FEditable := true;
end;

function TExtFormTriggerField.SetEditable(const AValue: Boolean): TExtExpression;
begin
  Result := CallMethod('setEditable').AddParam(AValue).AsExpression;
end;

function TExtFormTriggerField.SetReadOnly(const AValue: Boolean): TExtExpression;
begin
  Result := CallMethod('setReadOnly').AddParam(AValue).AsExpression;
end;

procedure TExtFormNumberField.SetAllowDecimals(const AValue: Boolean);
begin
  FAllowDecimals := SetConfigItem('allowDecimals', AValue);
end;

procedure TExtFormNumberField.SetAllowNegative(AValue: Boolean);
begin
  FAllowNegative := SetConfigItem('allowNegative', AValue);
end;

procedure TExtFormNumberField.SetDecimalPrecision(const AValue: Integer);
begin
  FDecimalPrecision := SetConfigItem('decimalPrecision', AValue);
end;

procedure TExtFormNumberField.SetDecimalSeparator(const AValue: string);
begin
  FDecimalSeparator := SetConfigItem('decimalSeparator', AValue);
end;

procedure TExtFormNumberField.SetMaxText(const AValue: string);
begin
  FMaxText := SetConfigItem('maxText', AValue);
end;

procedure TExtFormNumberField._SetMaxValue(const AValue: Integer);
begin
  FMaxValue := SetConfigItem('maxValue', 'setMaxValue', AValue);
end;

procedure TExtFormNumberField.SetMinText(const AValue: string);
begin
  FMinText := SetConfigItem('minText', AValue);
end;

procedure TExtFormNumberField._SetMinValue(const AValue: Integer);
begin
  FMinValue := SetConfigItem('minValue', 'setMinValue', AValue);
end;

procedure TExtFormNumberField.SetNanText(const AValue: string);
begin
  FNanText := SetConfigItem('nanText', AValue);
end;

class function TExtFormNumberField.JSClassName: string;
begin
  Result := 'Ext.form.NumberField';
end;

function TExtFormNumberField.GetObjectNamePrefix: string;
begin
  Result := 'numfld';
end;

procedure TExtFormNumberField.InitDefaults;
begin
  inherited;
  FAllowDecimals := true;
  FAllowNegative := true;
  FBaseChars := '0123456789';
  FDecimalPrecision := 2;
  FDecimalSeparator := '.';
  FFieldClass := 'x-form-field x-form-num-field';
  FMaxText := 'The maximum value for this field is {maxValue}';
  FMinText := 'The minimum value for this field is {minValue}';
  FNanText := '{value} is not a valid number';
end;

function TExtFormNumberField.SetMaxValue(const AValue: Integer): TExtExpression;
begin
  FMaxValue := AValue;
  Result := CallMethod('setMaxValue').AddParam(AValue).AsExpression;
end;

function TExtFormNumberField.SetMinValue(const AValue: Integer): TExtExpression;
begin
  FMinValue := AValue;
  Result := CallMethod('setMinValue').AddParam(AValue).AsExpression;
end;

procedure TExtFormFieldSet.SetCollapsible(const AValue: Boolean);
begin
  FCollapsible := SetConfigItem('collapsible', AValue);
end;

procedure TExtFormFieldSet.SetLabelWidth(AValue: Integer);
begin
  FLabelWidth := SetConfigItem('labelWidth', AValue);
end;

class function TExtFormFieldSet.JSClassName: string;
begin
  Result := 'Ext.form.FieldSet';
end;

function TExtFormFieldSet.GetObjectNamePrefix: string;
begin
  Result := 'fldset';
end;

procedure TExtFormFormPanel.SetLabelPad(const AValue: Integer);
begin
  FLabelPad := SetConfigItem('labelPad', AValue);
end;

procedure TExtFormFormPanel.SetLabelSeparator(const AValue: string);
begin
  FLabelSeparator := SetConfigItem('labelSeparator', AValue);
end;

procedure TExtFormFormPanel.SetMonitorValid(const AValue: Boolean);
begin
  FMonitorValid := SetConfigItem('monitorValid', AValue);
end;

procedure TExtFormFormPanel.SetFileUpload(const AValue: Boolean);
begin
  FFileUpload := SetConfigItem('fileUpload', AValue);
end;

class function TExtFormFormPanel.JSClassName: string;
begin
  Result := 'Ext.form.FormPanel';
end;

procedure TExtFormFormPanel.InitDefaults;
begin
  inherited;
  FLabelPad := 5;
  FForm := TExtFormBasicForm.CreateInternal(Self, 'getForm()');
end;

function TExtFormFormPanel.Load(const AOptions: TExtObject): TExtExpression;
begin
  Result := CallMethod('load').AddParam(AOptions).AsExpression;
end;

class function TExtFormRadioGroup.JSClassName: string;
begin
  Result := 'Ext.form.RadioGroup';
end;

class function TExtFormRadio.JSClassName: string;
begin
  Result := 'Ext.form.Radio';
end;

procedure TExtFormDateField.SetAltFormats(const AValue: string);
begin
  FAltFormats := SetConfigItem('altFormats', AValue);
end;

procedure TExtFormDateField._SetDisabledDates(const AValue: TExtObjectList);
begin
  FDisabledDates := TExtObjectList(SetConfigItem('disabledDates', 'setDisabledDates', AValue));
end;

procedure TExtFormDateField._SetDisabledDays(const AValue: TExtObjectList);
begin
  FDisabledDays.Free;
  FDisabledDays := TExtObjectList(SetConfigItem('disabledDays', 'setDisabledDays', AValue));
end;

procedure TExtFormDateField.SetFormat(const AValue: string);
begin
  FFormat := SetConfigItem('format', AValue);
end;

procedure TExtFormDateField._SetMaxValue(const AValue: TDateTime);
begin
  FMaxValue := SetConfigItem('maxValue', 'setMaxValue', AValue);
end;

procedure TExtFormDateField._SetMinValue(const AValue: TDateTime);
begin
  FMinValue := SetConfigItem('minValue', 'setMinValue', AValue);
end;

class function TExtFormDateField.JSClassName: string;
begin
  Result := 'Ext.form.DateField';
end;

procedure TExtFormDateField.InitDefaults;
begin
  inherited;
  FAltFormats :=
    'm/d/Y|n/j/Y|n/j/y|m/j/y|n/d/y|m/j/Y|n/d/Y|m-d-y|m-d-Y|m/d|m-d|md|mdy|mdY|d|Y-m-d';
  FDisabledDates := CreateConfigArray('disabledDates');
  FDisabledDays := CreateConfigArray('disabledDays');
  FFormat := 'm/d/Y';
  FTriggerClass := 'x-form-date-trigger';
end;

function TExtFormDateField.GetObjectNamePrefix: string;
begin
  Result := 'datefld';
end;

function TExtFormDateField.GetValue: TExtExpression;
begin
  Result := CallMethod('getValue').AsExpression;
end;

function TExtFormDateField.SetDisabledDates(const ADisabledDates: TExtObjectList): TExtExpression;
begin
  FDisabledDates.Free;
  FDisabledDates := ADisabledDates;
  Result := CallMethod('setDisabledDates').AddParam(ADisabledDates).AsExpression;
end;

function TExtFormDateField.SetDisabledDays(const ADisabledDays: TExtObjectList): TExtExpression;
begin
  FDisabledDays.Free;
  FDisabledDays := ADisabledDays;
  Result := CallMethod('setDisabledDays').AddParam(ADisabledDays).AsExpression;
end;

function TExtFormDateField.SetMaxValue(const AValue: TDateTime): TExtExpression;
begin
  FMaxValue := AValue;
  Result := CallMethod('setMaxValue').AddParam(AValue).AsExpression;
end;

function TExtFormDateField.SetMinValue(const AValue: TDateTime): TExtExpression;
begin
  FMinValue := AValue;
  Result := CallMethod('setMinValue').AddParam(AValue).AsExpression;
end;

function TExtFormDateField.SetValue(const ADate: string): TExtExpression;
begin
  Result := CallMethod('setValue').AddParam(ADate).AsExpression;
end;

function TExtFormDateField.SetValue(const ADate: TDateTime): TExtExpression;
begin
  Result := CallMethod('setValue').AddParam(ADate).AsExpression;
end;

procedure TExtFormComboBox.SetAutoSelect(const AValue: Boolean);
begin
  FAutoSelect := SetConfigItem('autoSelect', AValue);
end;

procedure TExtFormComboBox.SetDisplayField(const AValue: string);
begin
  FDisplayField := SetConfigItem('displayField', AValue);
end;

procedure TExtFormComboBox.SetForceSelection(const AValue: Boolean);
begin
  FForceSelection := SetConfigItem('forceSelection', AValue);
end;

procedure TExtFormComboBox.SetHiddenName(const AValue: string);
begin
  FHiddenName := SetConfigItem('hiddenName', AValue);
end;

procedure TExtFormComboBox.SetHiddenValue(const AValue: string);
begin
  FHiddenValue := SetConfigItem('hiddenValue', AValue);
end;

procedure TExtFormComboBox.SetLazyRender(const AValue: Boolean);
begin
  FLazyRender := SetConfigItem('lazyRender', AValue);
end;

procedure TExtFormComboBox.SetListWidth(const AValue: Integer);
begin
  FListWidth := SetConfigItem('listWidth', AValue);
end;

procedure TExtFormComboBox.SetListWidthFunc(const AValue: TExtExpression);
begin
  FListWidthFunc := SetConfigItem('listWidth', AValue);
end;

procedure TExtFormComboBox.SetMinChars(const AValue: Integer);
begin
  FMinChars := SetConfigItem('minChars', AValue);
end;

procedure TExtFormComboBox.SetMinHeight(const AValue: Integer);
begin
  FMinHeight := SetConfigItem('minHeight', AValue);
end;

procedure TExtFormComboBox.SetMinHeightFunc(const AValue: TExtExpression);
begin
  FMinHeightFunc := SetConfigItem('minHeight', AValue);
end;

procedure TExtFormComboBox.SetMinListWidth(const AValue: Integer);
begin
  FMinListWidth := SetConfigItem('minListWidth', AValue);
end;

procedure TExtFormComboBox.SetMode(const AValue: string);
begin
  FMode := SetConfigItem('mode', AValue);
end;

procedure TExtFormComboBox.SetPageSize(const AValue: Integer);
begin
  FPageSize := SetConfigItem('pageSize', AValue);
end;

procedure TExtFormComboBox.SetQueryDelay(const AValue: Integer);
begin
  FQueryDelay := SetConfigItem('queryDelay', AValue);
end;

procedure TExtFormComboBox.SetQueryParam(const AValue: string);
begin
  FQueryParam := SetConfigItem('queryParam', AValue);
end;

procedure TExtFormComboBox.SetSelectedClass(const AValue: string);
begin
  FSelectedClass := SetConfigItem('selectedClass', AValue);
end;

procedure TExtFormComboBox.SetStore(const AValue: TExtDataStore);
begin
  FStore.Free;
  FStore := TExtDataStore(SetConfigItem('store', AValue));
end;

procedure TExtFormComboBox.SetStoreArray(const AValue: TExtObjectList);
begin
  FStoreArray.Free;
  FStoreArray := TExtObjectList(SetConfigItem('store', AValue));
end;

procedure TExtFormComboBox.SetTriggerAction(const AValue: string);
begin
  FTriggerAction := SetConfigItem('triggerAction', AValue);
end;

procedure TExtFormComboBox.SetTypeAhead(const AValue: Boolean);
begin
  FTypeAhead := SetConfigItem('typeAhead', AValue);
end;

procedure TExtFormComboBox.SetTypeAheadDelay(const AValue: Integer);
begin
  FTypeAheadDelay := SetConfigItem('typeAheadDelay', AValue);
end;

procedure TExtFormComboBox.SetValueField(const AValue: string);
begin
  FValueField := SetConfigItem('valueField', AValue);
end;

procedure TExtFormComboBox.SetValueNotFoundText(const AValue: string);
begin
  FValueNotFoundText := SetConfigItem('valueNotFoundText', AValue);
end;

class function TExtFormComboBox.JSClassName: string;
begin
  Result := 'Ext.form.ComboBox';
end;

procedure TExtFormComboBox.InitDefaults;
begin
  inherited;
  FAutoSelect := true;
  FDisplayField := 'output/Ext.form.ComboBox.html#Ext.form.ComboBox-mode';
  FListConfig := TExtObject.CreateInternal(Self, 'listConfig');
  FMinChars := 4;
  FMinHeight := 90;
  FMinListWidth := 70;
  FPageSize := 0;
  FQueryDelay := 500;
  FQueryParam := 'query';
  FSelectOnFocus := true;
  FSelectedClass := 'x-combo-selected';
  FStore := TExtDataStore.CreateInternal(Self, 'store');
  FStoreArray := CreateConfigArray('store');
  FTriggerClass := 'x-form-arrow-trigger';
  FTypeAheadDelay := 250;
  FValueField := 'output/Ext.form.ComboBox.html#Ext.form.ComboBox-mode';
end;

function TExtFormComboBox.ClearValue: TExtExpression;
begin
  Result := CallMethod('clearValue').AsExpression;
end;

function TExtFormComboBox.Expand: TExtExpression;
begin
  Result := CallMethod('expand').AsExpression;
end;

procedure TExtFormComboBox.SetOnSelect(const AValue: TExtFormComboBoxOnSelect);
begin
  RemoveAllListeners('select');
  if Assigned(AValue) then
//    On('select', Ajax('select', ['Combo', '%0.nm', 'RecordJS', '%1.nm', 'Index', '%2'], true));
    &On('select', AjaxCallMethod('select')
      .Event
      .AddRawParam('Combo', 'combo.nm')
      .AddRawParam('RecordJS', 'record.nm') // should not work but it's not used.
      .AddRawParam('Index', 'index')
      .FunctionArgs('combo, record, index')
      .AsFunction);
  FOnSelect := AValue;
end;

function TExtFormComboBox.GetListParent: TExtExpression;
begin
  Result := CallMethod('getListParent').AsExpression;
end;

function TExtFormComboBox.GetObjectNamePrefix: string;
begin
  Result := 'combo';
end;

function TExtFormComboBox.GetStore: TExtExpression;
begin
  Result := CallMethod('getStore').AsExpression;
end;

function TExtFormComboBox.GetValue: TExtExpression;
begin
  Result := CallMethod('getValue').AsExpression;
end;

function TExtFormComboBox.IsExpanded: TExtExpression;
begin
  Result := CallMethod('isExpanded').AsExpression;
end;

function TExtFormComboBox.SetValue(const AValue: string): TExtExpression;
begin
  Result := CallMethod('setValue').AddParam(AValue).AsExpression;
end;

procedure TExtFormComboBox.HandleEvent(const AEvtName: string);
begin
  inherited;
  if (AEvtName = 'select') and Assigned(FOnSelect) then
    FOnSelect(TExtFormComboBox(ParamAsObject('Combo')),
      TExtDataRecord(ParamAsObject('RecordJS')), ParamAsInteger('Index'));
end;

procedure TExtFormTwinTriggerField.SetTrigger1Class(const AValue: string);
begin
  FTrigger1Class := SetConfigItem('trigger1Class', AValue);
end;

procedure TExtFormTwinTriggerField.SetTrigger2Class(const AValue: string);
begin
  FTrigger2Class := SetConfigItem('trigger2Class', AValue);
end;

class function TExtFormTwinTriggerField.JSClassName: string;
begin
  Result := 'Ext.form.TwinTriggerField';
end;

procedure TExtFormTwinTriggerField.InitDefaults;
begin
  inherited;
  FTriggerConfig := TExtObject.CreateInternal(Self, 'triggerConfig');
end;

procedure TExtFormTimeField.SetAltFormats(const AValue: string);
begin
  FAltFormats := SetConfigItem('altFormats', AValue);
end;

procedure TExtFormTimeField.SetFormat(const AValue: string);
begin
  FFormat := SetConfigItem('format', AValue);
end;

procedure TExtFormTimeField._SetMaxValue(const AValue: TDateTime);
begin
  FMaxValue := SetConfigItem('maxValue', 'setMaxValue', AValue);
end;

procedure TExtFormTimeField._SetMinValue(const AValue: TDateTime);
begin
  FMinValue := SetConfigItem('minValue', 'setMinValue', AValue);
end;

class function TExtFormTimeField.JSClassName: string;
begin
  Result := 'Ext.form.TimeField';
end;

function TExtFormTimeField.GetObjectNamePrefix: string;
begin
  Result := 'timefld';
end;

procedure TExtFormTimeField.InitDefaults;
begin
  inherited;
  FAltFormats :=
    'g:ia/g:iA/g:i a/g:i A/h:i/g:i/H:i/ga/ha/gA/h a/g a/g A/gi/hi/gia/hia/g/H/gi a/hi a/giA/hiA/gi A/hi A';
  FFormat := 'g:i A';
  FIncrement := 15;
  FInvalidText := '{value} is not a valid time';
  FMaxText := 'The time in this field must be equal to or before {0}';
  FMinText := 'The time in this field must be equal to or after {0}';
end;

function TExtFormTimeField.SetMaxValue(const AValue: TDateTime): TExtExpression;
begin
  FMaxValue := AValue;
  Result := CallMethod('setMaxValue').AddParam(AValue).AsExpression;
end;

function TExtFormTimeField.SetMinValue(const AValue: TDateTime): TExtExpression;
begin
  FMinValue := AValue;
  Result := CallMethod('setMinValue').AddParam(AValue).AsExpression;
end;

{ TExtFormFieldContainer }

class function TExtFormFieldContainer.JSClassName: string;
begin
  Result := 'Ext.form.FieldContainer';
end;

end.
