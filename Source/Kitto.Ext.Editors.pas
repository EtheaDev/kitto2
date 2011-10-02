unit Kitto.Ext.Editors;

interface

uses
  DB, Generics.Collections,
  Ext, ExtPascal, ExtForm,
  EF.Intf, EF.Data, EF.Classes, EF.Tree,
  Kitto.Ext.Base, Kitto.Metadata.Views;

type
  IKExtEditItem = interface(IEFInterface)
    ['{4F5A1E4E-D5A1-44FE-93DC-E1ABF1209CE1}']
    procedure SetOption(const AName, AValue: string);
    function AsExtObject: TExtObject;
  end;

  IKExtEditContainer = interface(IKExtEditItem)
    ['{1E03E482-8BBC-4750-B0D4-CB5E83126A5E}']
    procedure AddChild(const AEditItem: IKExtEditItem);
  end;

  IKExtEditor = interface(IKExtEditItem)
    ['{FF091C2F-A987-4D00-B985-9C00AE37CA5A}']
    function AsExtFormField: TExtFormField;
    procedure SetValueFromField(const AField: TField);
  end;

  TKExtEditPanel = class(TExtFormFormPanel, IKExtEditItem, IKExtEditContainer)
  protected
    procedure InitDefaults; override;
  public
    function AsObject: TObject;
    function QueryInterface(const IID: TGUID; out Obj): HRESULT; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure AddChild(const AEditItem: IKExtEditItem);
    procedure SetOption(const AName, AValue: string);
    function AsExtObject: TExtObject;
  end;

  TKExtFormFieldSet = class(TExtFormFieldSet, IKExtEditItem, IKExtEditContainer)
  public
    function AsObject: TObject;
    function QueryInterface(const IID: TGUID; out Obj): HRESULT; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure AddChild(const AEditItem: IKExtEditItem);
    procedure SetOption(const AName, AValue: string);
    function AsExtObject: TExtObject;
  end;

  TKExtFormCompositeField = class(TExtFormCompositeField, IKExtEditItem, IKExtEditContainer)
  public
    function AsObject: TObject; inline;
    function QueryInterface(const IID: TGUID; out Obj): HRESULT; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure AddChild(const AEditItem: IKExtEditItem);
    procedure SetOption(const AName, AValue: string);
    function AsExtObject: TExtObject; inline;
  end;

  TKExtFormContainer = class(TExtContainer, IKExtEditItem)
  protected
    procedure InitDefaults; override;
    function InternalSetOption(const AName, AValue: string): Boolean; virtual;
  public
    function AsObject: TObject; inline;
    function QueryInterface(const IID: TGUID; out Obj): HRESULT; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure AddChild(const AEditItem: IKExtEditItem);
    procedure SetOption(const AName, AValue: string);
    function AsExtObject: TExtObject; inline;
  end;

  TKExtFormRow = class(TKExtFormContainer, IKExtEditContainer)
  protected
    procedure InitDefaults; override;
  end;

  ///	<summary>
  ///	  Encapsulates a field in a row. Does NOT implement the container
  ///	  interface, as it is a commodity class only.
  ///	</summary>
  TKExtFormRowField = class(TKExtFormContainer, IKExtEditor)
  private
    FEditor: IKExtEditor;
  protected
    procedure InitDefaults; override;
  public
    function Encapsulate(const AValue: IKExtEditor): IKExtEditor;

    function AsExtFormField: TExtFormField;
    procedure SetValueFromField(const AField: TField);
    procedure SetOption(const AName, AValue: string);
  end;

{ TODO : support the CheckboxGroup and Radiogroup containers? }

  TKExtFormNumberField = class(TExtFormNumberField, IKExtEditItem, IKExtEditor)
  public
    function AsObject: TObject; inline;
    function QueryInterface(const IID: TGUID; out Obj): HRESULT; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure SetOption(const AName, AValue: string);
    function AsExtObject: TExtObject; inline;
    function AsExtFormField: TExtFormField; inline;
    procedure SetValueFromField(const AField: TField);
  end;

  TKExtFormTextField = class(TExtFormTextField, IKExtEditItem, IKExtEditor)
  public
    function AsObject: TObject; inline;
    function QueryInterface(const IID: TGUID; out Obj): HRESULT; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure SetOption(const AName, AValue: string);
    function AsExtObject: TExtObject; inline;
    function AsExtFormField: TExtFormField; inline;
    procedure SetValueFromField(const AField: TField);
  end;

  TKExtFormTextArea = class(TExtFormTextArea, IKExtEditItem, IKExtEditor)
  public
    function AsObject: TObject; inline;
    function QueryInterface(const IID: TGUID; out Obj): HRESULT; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure SetOption(const AName, AValue: string);
    function AsExtObject: TExtObject; inline;
    function AsExtFormField: TExtFormField; inline;
    procedure SetValueFromField(const AField: TField);
  end;

  TKExtFormCheckbox = class(TExtFormCheckbox, IKExtEditItem, IKExtEditor)
  public
    function AsObject: TObject; inline;
    function QueryInterface(const IID: TGUID; out Obj): HRESULT; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure SetOption(const AName, AValue: string);
    function AsExtObject: TExtObject; inline;
    function AsExtFormField: TExtFormField; inline;
    procedure SetValueFromField(const AField: TField);
  end;

  TKExtFormDateField = class(TExtFormDateField, IKExtEditItem, IKExtEditor)
  public
    function AsObject: TObject; inline;
    function QueryInterface(const IID: TGUID; out Obj): HRESULT; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure SetOption(const AName, AValue: string);
    function AsExtObject: TExtObject; inline;
    function AsExtFormField: TExtFormField; inline;
    procedure SetValueFromField(const AField: TField);
  end;

  TKExtFormTimeField = class(TExtFormTimeField, IKExtEditItem, IKExtEditor)
  public
    function AsObject: TObject; inline;
    function QueryInterface(const IID: TGUID; out Obj): HRESULT; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure SetOption(const AName, AValue: string);
    function AsExtObject: TExtObject; inline;
    function AsExtFormField: TExtFormField; inline;
    procedure SetValueFromField(const AField: TField);
  end;

  TKExtFormDateTimeField = class(TExtFormField, IKExtEditItem, IKExtEditor)
  private
    FTimeFormat: string;
    FDateFormat: string;
    FDateConfig: TExtObject;
    FTimeConfig: TExtObject;
    procedure SetDateFormat(const AValue: string);
    procedure SetTimeFormat(const AValue: string);
    procedure SetDateConfig(const AValue: TExtObject);
    procedure SetTimeConfig(const AValue: TExtObject);
  public
    destructor Destroy; override;
    function JSClassName: string; override;
  public
    function AsObject: TObject; inline;
    function QueryInterface(const IID: TGUID; out Obj): HRESULT; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure SetOption(const AName, AValue: string);
    function AsExtObject: TExtObject; inline;
    function AsExtFormField: TExtFormField; inline;
    procedure SetValueFromField(const AField: TField);

    property DateFormat: string read FDateFormat write SetDateFormat;
    property DateConfig: TExtObject read FDateConfig write SetDateConfig;
    property TimeFormat: string read FTimeFormat write SetTimeFormat;
    property TimeConfig: TExtObject read FTimeConfig write SetTimeConfig;
  end;

  TKExtFormComboBoxEditor = class(TKExtFormComboBox, IKExtEditItem, IKExtEditor)
  public
    procedure SetOption(const AName, AValue: string);
    function AsExtObject: TExtObject; inline;
    function AsExtFormField: TExtFormField; inline;
    procedure SetValueFromField(const AField: TField);
  end;

  TKExtLayoutDefaults = record
    MemoWidth: Integer;
    MaxFieldWidth: Integer;
    MinFieldWidth: Integer;
    RequiredLabelFormat: string;
    MsgTarget: string;
    procedure Init;
  end;

  TKExtLayoutOnNewEditor = reference to procedure (AEditor: IKExtEditor);

  ///	<summary>
  ///	  Creates editor based on layouts. Can synthesize a default layout if
  ///	  missing.
  ///	</summary>
  TKExtLayoutProcessor = class
  private
    FViewTable: TKViewTable;
    FForceReadOnly: Boolean;
    FDataSet: TDataSet;
    FOnNewEditor: TKExtLayoutOnNewEditor;
    FFormPanel: TKExtEditPanel;
    FFocusEditor: IKExtEditor;
    FDefaults: TKExtLayoutDefaults;
    FCurrentEditItem: IKExtEditItem;
    FEditContainers: TStack<IKExtEditContainer>;

    function CreateEditItem(const AName, AValue: string;
      const AContainer: IKExtEditContainer): IKExtEditItem;

    function CreateEditor(const AFieldName: string;
      const AContainer: IKExtEditContainer): IKExtEditor;
    function CreateFieldSet(const ATitle: string): IKExtEditItem;
    function CreateCompositeField(const ALabel: string): IKExtEditItem;
    procedure SetGlobalOption(const AName, AValue: string);
    procedure LayoutError(const AErrorMessage: string);
    function CreateRow: IKExtEditItem;
    procedure CreateEditorsFromLayout(const ALayout: TKLayout);
    procedure ProcessLayoutNode(const ANode: TEFNode);
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
  public
    // Set all properties before calling the CreateEditors methods.
    property ViewTable: TKViewTable read FViewTable write FViewTable;
    property ForceReadOnly: Boolean read FForceReadOnly write FForceReadOnly;
    property FormPanel: TKExtEditPanel read FFormPanel write FFormPanel;
    property DataSet: TDataSet read FDataSet write FDataSet;
    property OnNewEditor: TKExtLayoutOnNewEditor read FOnNewEditor write FOnNewEditor;

    ///	<summary>
    ///	  Creates editors according to the specified layout or a default layout.
    ///	</summary>
    ///	<param name="ALayout">
    ///	  Layout used to create the editors. Pass nil to manufacture a default
    ///	  layout.
    ///	</param>
    procedure CreateEditors(const ALayout: TKLayout);

    ///	<summary>
    ///	  A reference to the first editor to focus. Only valid after calling
    ///	  CreateEditors method.
    ///	</summary>
    property FocusEditor: IKExtEditor read FFocusEditor;
  end;

implementation

uses
  SysUtils, Classes, Math, StrUtils,
  EF.StrUtils, EF.Localization, EF.YAML, EF.Types,
  Kitto.Ext.Utils, Kitto.JSON, Kitto.Environment, Kitto.Ext.Session;

const
  {
    String fields of this size or longer are represented by multiline edit
    controls.
  }
  MULTILINE_EDIT_THRESHOLD = 200;

var
  _JSFormatSettings: TFormatSettings;

procedure InvalidOption(const AName, AValue: string);
begin
  raise EEFError.CreateFmt(_('Unknown or misplaced option %s: %s.'), [AName, AValue]);
end;

function OptionAsFloat(const AOptionValue: string): Double;
begin
  // Floats in Yaml always use the dot as decimal separator.
  if not TryStrToFloat(AOptionValue, Result, _JSFormatSettings) then
    raise EEFError.CreateFmt(_('Invalid value %s. Valid values: decimal numbers.'), [AOptionValue]);
end;

function OptionAsIntegerOrPerc(const AOptionValue: string): string;
var
  LNumber: Integer;
begin
  Result := AOptionValue;
  if EndsStr('%', Result) then
  begin
    if not TryStrToInt(Copy(Result, 1, Length(Result) - 1), LNumber) then
      raise EEFError.CreateFmt(_('Invalid value %s. Valid values: whole numbers or percentages.'), [Result]);
    if (LNumber < 0) or (LNumber > 100) then
      raise EEFError.CreateFmt(_('Invalid percentage %s. Valid percentages are 0% to 100%.'), [Result]);

  end
  else if not TryStrToInt(Result, LNumber) then
    raise EEFError.CreateFmt(_('Invalid value %s. Valid values: whole numbers or percentages.'), [Result]);
end;

function OptionAsInteger(const AOptionValue: string): Integer;
begin
  if not TryStrToInt(AOptionValue, Result) then
    raise EEFError.CreateFmt(_('Invalid value %s. Valid values: whole numbers.'), [AOptionValue]);
end;

function OptionAsBoolean(const AOptionValue: string): Boolean;
begin
  if SameText(AOptionValue, 'True') then
    Result := True
  else if SameText(AOptionValue, 'False') then
    Result := False
  else
    raise EEFError.CreateFmt(_('Invalid value %s. Valid values: "True", "False".'), [AOptionValue]);
end;

function OptionAsLabelAlign(const AOptionValue: string): TExtFormFormPanelLabelAlign;
begin
  if SameText(AOptionValue, 'Left') then
    Result := laLeft
  else if SameText(AOptionValue, 'Top') then
    Result := laTop
  else if SameText(AOptionValue, 'Right') then
    Result := laRight
  else
    raise EEFError.CreateFmt(_('Invalid value %s. Valid values: "Left", "Top", "Right".'), [AOptionValue]);
end;

function OptionAsString(const AOptionValue: string; const AAllowedValues: array of string): string;

  function FormatAllowedValues: string;
  var
    I: Integer;
  begin
    for I := Low(AAllowedValues) to High(AAllowedValues) do
    if Result = '' then
      Result := AAllowedValues[I]
    else
      Result := Result + ', ' + AAllowedValues[I];
  end;

begin
  if not MatchText(AOptionValue, AAllowedValues) then
    raise EEFError.CreateFmt(_('Invalid value %s. Valid values: %s'), [AOptionValue, FormatAllowedValues]);
  Result := AOptionValue;
end;

function SetExtFormFieldOption(const AFormField: TExtFormField; const AName, AValue: string): Boolean;
begin
  Result := True;
  if SameText(AName, 'Anchor') then
    AFormField.Anchor := AValue
  else if SameText(AName, 'CharWidth') then
    AFormField.Width := AFormField.CharsToPixels(OptionAsInteger(AValue))
  else if SameText(AName, 'Width') then
    AFormField.SetWidth(OptionAsIntegerOrPerc(AValue))
  else
    Result := False;
end;

{ TKExtLayoutProcessor }

procedure TKExtLayoutProcessor.LayoutError(const AErrorMessage: string);
begin
  raise EEFError.CreateFmt(_('Layout parsing error. %s.'), [AErrorMessage]);
end;

procedure TKExtLayoutProcessor.CreateEditorsFromLayout(const ALayout: TKLayout);
var
  I: Integer;
begin
  Assert(Assigned(FViewTable));
  Assert(Assigned(FFormPanel));
  Assert(Assigned(FDataSet));
  Assert(Assigned(ALayout));

  FCurrentEditItem := nil;
  FEditContainers.Clear;
  for I := 0 to ALayout.ChildCount - 1 do
    ProcessLayoutNode(ALayout.Children[I]);
end;

procedure TKExtLayoutProcessor.ProcessLayoutNode(const ANode: TEFNode);
var
  LViewField: TKViewField;
  I: Integer;
  LIntf: IKExtEditContainer;
begin
  Assert(Assigned(FViewTable));
  Assert(Assigned(ANode));

  // Skip invisible fields.
  if SameText(ANode.Name, 'Field') then
  begin
    LViewField := FViewTable.FieldByAliasedName(ANode.AsString);
    if not ViewTable.IsFieldVisible(LViewField) then
      Exit;
  end;

  if MatchText(ANode.Name, ['Field', 'FieldSet', 'CompositeField', 'Row']) then
  begin
    { TODO : make FFormPanel implement the container and item interfaces, to simplify code? }

    if FEditContainers.Count > 0 then
      FCurrentEditItem := CreateEditItem(ANode.Name, ANode.AsString, FEditContainers.Peek)
    else
    begin
      FCurrentEditItem := CreateEditItem(ANode.Name, ANode.AsString, nil);
      FFormPanel.Items.Add(FCurrentEditItem.AsExtObject);
    end;

    if Supports(FCurrentEditItem, IKExtEditContainer, LIntf) then
      FEditContainers.Push(LIntf);
  end
  else
  begin
    // Unknown name - must be an option.
    if ANode.AsString = '' then
      LayoutError(Format(_('Option %s must have a value.'), [ANode.Name]));
    if ANode.ChildCount > 0 then
      LayoutError(Format(_('Option node %s cannot have child nodes.'), [ANode.Name]));

    if Assigned(FCurrentEditItem) then
      FCurrentEditItem.SetOption(ANode.Name, ANode.AsString)
    else
      SetGlobalOption(ANode.Name, ANode.AsString)
  end;
  for I := 0 to ANode.ChildCount - 1 do
    ProcessLayoutNode(ANode.Children[I]);

  if Assigned(LIntf) then // Pushed, so pop it.
    FEditContainers.Pop;
end;

procedure TKExtLayoutProcessor.CreateEditors(const ALayout: TKLayout);
var
  I: Integer;
begin
  Assert(Assigned(FViewTable));
  Assert(Assigned(FFormPanel));
  Assert(Assigned(FDataSet));

  FFocusEditor := nil;

  if Assigned(ALayout) then
    CreateEditorsFromLayout(ALayout)
  else
  begin
    for I := 0 to FDataSet.FieldCount - 1 do
    begin
      if FViewTable.IsFieldVisible(FViewTable.FieldByAliasedName(FDataSet.Fields[I].FieldName)) then
        FFormPanel.AddChild(CreateEditor(FDataSet.Fields[I].FieldName, nil));
    end;
  end;
  if Assigned(FFocusEditor) then
    FFormPanel.On('afterrender', FFormPanel.JSFunction(FFocusEditor.AsExtFormField.JSName + '.focus(false, 1000);'));
end;

function TKExtLayoutProcessor.CreateEditItem(const AName,
  AValue: string; const AContainer: IKExtEditContainer): IKExtEditItem;
begin
  if SameText(AName, 'Field') then
    Result := CreateEditor(AValue, AContainer)
  else if SameText(AName, 'FieldSet') then
    Result := CreateFieldSet(_(AValue))
  else if SameText(AName, 'CompositeField') then
    Result := CreateCompositeField(_(AValue))
  else if SameText(AName, 'Row') then
    Result := CreateRow
  else
    raise EEFError.CreateFmt(_('Unknown edit item type %s.'), [AName]);
  if Assigned(AContainer) then
    Acontainer.AddChild(Result);
end;

function TKExtLayoutProcessor.CreateEditor(const AFieldName: string;
  const AContainer: IKExtEditContainer): IKExtEditor;
const
  TRIGGER_WIDTH = 4;
  SPACER_WIDTH = 1;
var
  LFieldWidth: Integer;
  LMaxLength: Integer;
  LIsReadOnly: Boolean;
  LIsRequired: Boolean;
  LLabel: string;
  LLookupCommandText: string;
  LComboBox: TKExtFormComboBoxEditor;
  LTextArea: TKExtFormTextArea;
  LCheckbox: TKExtFormCheckbox;
  LDateField: TKExtFormDateField;
  LDateTimeField: TKExtFormDateTimeField;
  LTextField: TKExtFormTextField;
  LField: TField;
  LDataField: TKViewField;
  LRowField: TKExtFormRowField;
  LNumberField: TKExtFormNumberField;
  LNode: TEFNode;
  LForceCase: string;
  LAllowedValues: TEFPairs;
  LTimeField: TKExtFormTimeField;
begin
  LField := FDataSet.FieldByName(AFieldName);
  LDataField := FViewTable.FieldByAliasedName(AFieldName);

  // Store common properties.
  LFieldWidth := LDataField.DisplayWidth;
  LMaxLength := LField.Size;
  if LFieldWidth = 0 then
    // Blobs have Size = 0.
    LFieldWidth := Min(IfThen(LField.Size = 0, FDefaults.MemoWidth, LField.Size), FDefaults.MaxFieldWidth);
  // Minimum cap - avoids too short combo boxes.
  // Add 1 to compensate for Ext's imprecise conversion to pixels.
  LFieldWidth := Max(LFieldWidth, FDefaults.MinFieldWidth) + 1;

  LIsReadOnly := LDataField.IsReadOnly or ViewTable.IsReadOnly or FForceReadOnly;
  LIsRequired := LDataField.IsRequired;
  LLabel := _(LDataField.DisplayLabel);
  if not LIsReadOnly and LIsRequired then
    LLabel := Format(FDefaults.RequiredLabelFormat, [LLabel]);

  if AContainer is TKExtFormRow then
    LRowField := TKExtFormRowField.Create
  else
    LRowField := nil;

  LLookupCommandText := LDataField.GetString('Lookup/CommandText');
  LAllowedValues := LDataField.GetChildrenAsPairs('Lookup/AllowedValues');
  if (LLookupCommandText <> '') or (Length(LAllowedValues) > 0) then
  begin
    LComboBox := TKExtFormComboBoxEditor.Create;
    try
      if not Assigned(LRowField) then
        LComboBox.Width := FFormPanel.CharsToPixels(LFieldWidth + TRIGGER_WIDTH)
      else
        LRowField.Width := FFormPanel.CharsToPixels(LFieldWidth + TRIGGER_WIDTH);
      LComboBox.TriggerAction := 'all';
      LComboBox.TypeAhead := True;
      LComboBox.LazyRender := True;
      LComboBox.SelectOnFocus := False;
      LComboBox.Mode := 'local';
      // Enable the combo box to post its hidden value instead of the visible description.
      LComboBox.HiddenName := LField.FieldName;
      LComboBox.Id := LField.FieldName + '_DX';
      if LLookupCommandText <> '' then
        LComboBox.StoreArray := FFormPanel.JSArray(DataSetToJSON(Environment.MainDBConnection, LLookupCommandText))
      else
        LComboBox.StoreArray := FFormPanel.JSArray(PairsToJSON(LAllowedValues));
      //LComboBox.PageSize := 10;
      //LComboBox.Resizable := True;
      //LComboBox.MinListWidth := LFieldWidth;
      //LComboBox.MinHeight := LinesToPixels(5);
      if not LIsReadOnly then
        LComboBox.ForceSelection := LIsRequired;
      Result := LComboBox;
    except
      LComboBox.Free;
      raise;
    end;
  end
  else if (LField.DataType in [ftMemo, ftWideMemo, ftFmtMemo, ftOraClob])
    or ((LField.DataType in [ftString, ftWideString]) and (LField.Size div SizeOf(Char) >= MULTILINE_EDIT_THRESHOLD)) then
  begin
    LTextArea := TKExtFormTextArea.Create;
    try
      if not Assigned(LRowField) then
        LTextArea.Width := FFormPanel.CharsToPixels(LFieldWidth)
      else
        LRowField.Width := FFormPanel.CharsToPixels(LFieldWidth);
      LTextArea.Height := FFormPanel.LinesToPixels(LDataField.GetInteger('EditLines', 5));
      LTextArea.AutoScroll := True;
      // Set this if it's the last field.
      //Anchor := '100%';
      if not LIsReadOnly then
      begin
        if LField.Size > 0 then
          LTextArea.MaxLength  := LField.Size;
        LTextArea.AllowBlank := not LIsRequired;
      end;
      LTextArea.Grow := True;
      Result := LTextArea;
    except
      LTextArea.Free;
      raise;
    end;
  end
  else if LDataField.DataType = edtBoolean then
  begin
    LCheckbox := TKExtFormCheckbox.Create;
    try
      LCheckbox.BoxLabel := '';//LLabel;
      Result := LCheckbox;
    except
      LCheckbox.Free;
      raise;
    end;
  end
  else if LDataField.DataType = edtDate then
  begin
    LDateField := TKExtFormDateField.Create;
    try
      if not Assigned(LRowField) then
        LDateField.Width := FFormPanel.CharsToPixels(LFieldWidth + TRIGGER_WIDTH)
      else
        LRowField.Width := FFormPanel.CharsToPixels(LFieldWidth + TRIGGER_WIDTH);
      // Don't use Delphi format here.
      LDateField.Format := DelphiDateFormatToJSDateFormat(Session.FormatSettings.ShortDateFormat);
      if not LIsReadOnly then
        LDateField.AllowBlank := not LIsRequired;
      Result := LDateField;
    except
      LDateField.Free;
      raise;
    end;
  end
  else if LDataField.DataType = edtTime then
  begin
    LTimeField := TKExtFormTimeField.Create;
    try
      if not Assigned(LRowField) then
        LTimeField.Width := FFormPanel.CharsToPixels(LFieldWidth + TRIGGER_WIDTH)
      else
        LRowField.Width := FFormPanel.CharsToPixels(LFieldWidth + TRIGGER_WIDTH);
      // Don't use Delphi format here.
      LTimeField.Format := DelphiDateFormatToJSDateFormat(Session.FormatSettings.ShortTimeFormat);
      if not LIsReadOnly then
        LTimeField.AllowBlank := not LIsRequired;
      Result := LTimeField;
    except
      LTimeField.Free;
      raise;
    end;
  end
  else if LDataField.DataType = edtDateTime then
  begin
    LDateTimeField := TKExtFormDateTimeField.Create;
    try
      if not Assigned(LRowField) then
        LDateTimeField.Width := FFormPanel.CharsToPixels(LFieldWidth + (2 * TRIGGER_WIDTH) + SPACER_WIDTH)
      else
        LRowField.Width := FFormPanel.CharsToPixels(LFieldWidth + (2 * TRIGGER_WIDTH) + SPACER_WIDTH);
      // Don't use Delphi format here.
      LDateTimeField.DateFormat := DelphiDateFormatToJSDateFormat(Session.FormatSettings.ShortDateFormat);
      { TODO : localize? }
      LDateTimeField.TimeFormat := 'g:i:s';
      if not LIsReadOnly then
      begin
        LDateTimeField.DateConfig := FFormPanel.JSObject('allowBlank:false');
        LDateTimeField.TimeConfig := FFormPanel.JSObject('allowBlank:false');
      end
      else
      begin
        LDateTimeField.DateConfig := FFormPanel.JSObject('readOnly:true');
        LDateTimeField.TimeConfig := FFormPanel.JSObject('readOnly:true');
      end;
      Result := LDateTimeField;
    except
      LDateTimeField.Free;
      raise;
    end;
  end
  else if LDataField.DataType in [edtInteger, edtCurrency, edtFloat, edtBcd] then
  begin
    LNumberField := TKExtFormNumberField.Create;
    try
      if not Assigned(LRowField) then
        LNumberField.Width := FFormPanel.CharsToPixels(LFieldWidth)
      else
        LRowField.Width := FFormPanel.CharsToPixels(LFieldWidth);
      if not LIsReadOnly then
      begin
        LNumberField.AllowDecimals := LDataField.DataType in [edtCurrency, edtFloat, edtBcd];
        LNumberField.AllowNegative := True;
        if not LNumberField.AllowDecimals then
        begin
          LNode := LDataField.FindNode('MaxValue');
          if Assigned(LNode) then
            LNumberField.MaxValue := LNode.AsInteger;
          LNode := LDataField.FindNode('MinValue');
          if Assigned(LNode) then
            LNumberField.MinValue := LNode.AsInteger;
        end;
      end;
      Result := LNumberField;
    except
      LNumberField.Free;
      raise;
    end;
  end
  else
  begin
    { TODO : support lookups with buttons and custom buttons as well (through field-level realms?) }
    LTextField := TKExtFormTextField.Create;
    try
      if not Assigned(LRowField) then
        LTextField.Width := FFormPanel.CharsToPixels(LFieldWidth)
      else
        LRowField.Width := FFormPanel.CharsToPixels(LFieldWidth);
      if not LIsReadOnly then
      begin
        if LMaxLength <> 0 then
          LTextField.MaxLength := LMaxLength;
        LTextField.AllowBlank := not LIsRequired;
      end;
      Result := LTextField;
    except
      LTextField.Free;
      raise;
    end;
  end;

  // Setup some validation.
  if not LIsReadOnly and (Result.AsExtFormField is TExtFormTextField) then
  begin
    LNode := LDataField.FindNode('MaxLength');
    if Assigned(LNode) then
      TExtFormTextField(Result).MaxLength := LNode.AsInteger;
    LNode := LDataField.FindNode('MinLength');
    if Assigned(LNode) then
      TExtFormTextField(Result).MinLength := LNode.AsInteger;
    LNode := LDataField.FindNode('Validator');
    if Assigned(LNode) then
      TExtFormTextField(Result).Validator := FFormPanel.JSFunction('value', LNode.AsString);
    { TODO : refactor this to unified field-level rules }
    LNode := LDataField.FindNode('ValidationType'); // alpha alphanum email url
    if Assigned(LNode) then
      TExtFormTextField(Result).Vtype := LNode.AsString;

    LForceCase := LDataField.GetString('ForceCase'); // upper lower caps none
    if LForceCase <> '' then
      TExtFormTextField(Result).EnableKeyEvents := True;
    if SameText(LForceCase, 'Upper')  then
      TExtFormTextField(Result).On('keyup', FFormPanel.JSFunction('field, e', 'field.setValue(field.getRawValue().toUpperCase());'))
    else if SameText(LForceCase, 'Lower')  then
      TExtFormTextField(Result).On('keyup', FFormPanel.JSFunction('field, e', 'field.setValue(field.getRawValue().toLowerCase());'))
    else if SameText(LForceCase, 'Caps')  then
      TExtFormTextField(Result).On('change', FFormPanel.JSFunction('field, newValue, oldValue', 'field.setValue(newValue.capitalize());'));
  end;

  Result.AsExtFormField.AutoScroll := False; // Don't display a h. scrollbar for larger fields.
  if Result.AsExtFormField.Id = '' then
    Result.AsExtFormField.Id := LField.FieldName;
  Result.AsExtFormField.Name := LField.FieldName;
  Result.AsExtFormField.ReadOnly := LIsReadOnly;
  // Don't disable: it will be missing from the POST and SaveChanges does not handle that yet.
  { TODO : make disabling configurable - watch out for disabled fields changed by triggers (they should be POSTed) }
  //Disabled := ReadOnly;
  Result.AsExtFormField.FieldLabel := LLabel;
  Result.AsExtFormField.MsgTarget := LowerCase(FDefaults.MsgTarget);

  if Assigned(FOnNewEditor) then
    FOnNewEditor(Result);

  if (FFocusEditor = nil) and not Result.AsExtFormField.ReadOnly and not Result.AsExtFormField.Disabled then
    FFocusEditor := Result;

  if Assigned(LRowField) then
    Result := LRowField.Encapsulate(Result);
end;

function TKExtLayoutProcessor.CreateFieldSet(const ATitle: string): IKExtEditItem;
var
  LFieldSet: TKExtFormFieldSet;
begin
  LFieldSet := TKExtFormFieldSet.Create;
  LFieldSet.Title := ATitle;
  LFieldSet.Collapsible := False;
  LFieldSet.Anchor := '-10';
  Result := LFieldSet;
end;

procedure TKExtLayoutProcessor.AfterConstruction;
begin
  inherited;
  FDefaults.Init;
  FEditContainers := TStack<IKExtEditContainer>.Create;
end;

function TKExtLayoutProcessor.CreateCompositeField(const ALabel: string): IKExtEditItem;
var
  LCompositeField: TKExtFormCompositeField;
begin
  LCompositeField := TKExtFormCompositeField.Create;
  if ALabel <> '' then
    LCompositeField.FieldLabel := ALabel;
  LCompositeField.Anchor := '-32';
  Result := LCompositeField;
end;

function TKExtLayoutProcessor.CreateRow: IKExtEditItem;
var
  LRow: TKExtFormRow;
begin
  LRow := TKExtFormRow.Create;
  LRow.Anchor := '-32';
  Result := LRow;
end;

destructor TKExtLayoutProcessor.Destroy;
begin
  FreeAndNil(FEditContainers);
  inherited;
end;

procedure TKExtLayoutProcessor.SetGlobalOption(const AName, AValue: string);
begin
  if SameText(AName, 'MemoWidth') then
    FDefaults.MemoWidth := OptionAsInteger(AValue)
  else if SameText(AName, 'MaxFieldWidth') then
    FDefaults.MaxFieldWidth := OptionAsInteger(AValue)
  else if SameText(AName, 'MinFieldWidth') then
    FDefaults.MinFieldWidth := OptionAsInteger(AValue)
  else if SameText(AName, 'RequiredLabelFormat') then
    FDefaults.RequiredLabelFormat := AValue
  else if SameText(AName, 'MsgTarget') then
    FDefaults.MsgTarget := OptionAsString(AValue, ['Qtip', 'Title', 'Under', 'Side'])
  else
    FFormPanel.SetOption(AName, AValue);
end;

{ TKExtLayoutDefaults }

procedure TKExtLayoutDefaults.Init;
begin
  MemoWidth := 60;
  MaxFieldWidth := 60;
  MinFieldWidth := 10;
  RequiredLabelFormat := '<b>%s*</b>';
  MsgTarget := 'Under'; // qtip title under side
end;

{ TKExtEditPanel }

procedure TKExtEditPanel.AddChild(const AEditItem: IKExtEditItem);
begin
  Items.Add(AEditItem.AsExtObject);
end;

function TKExtEditPanel.AsExtObject: TExtObject;
begin
  Result := Self;
end;

function TKExtEditPanel.AsObject: TObject;
begin
  Result := Self;
end;

procedure TKExtEditPanel.InitDefaults;
begin
  inherited;
  LabelAlign := laTop;
end;

function TKExtEditPanel.QueryInterface(const IID: TGUID; out Obj): HRESULT;
begin
  if GetInterface(IID, Obj) then Result := 0 else Result := E_NOINTERFACE;
end;

procedure TKExtEditPanel.SetOption(const AName, AValue: string);
begin
  if SameText(AName, 'LabelWidth') then
    LabelWidth := OptionAsInteger(AValue)
  else if SameText(AName, 'LabelAlign') then
    LabelAlign := OptionAsLabelAlign(AValue)
  else if SameText(AName, 'LabelSeparator') then
    LabelSeparator := AValue
  else if SameText(AName, 'LabelPad') then
    LabelPad := OptionAsInteger(AValue)
  else
    InvalidOption(AName, AValue);
end;

function TKExtEditPanel._AddRef: Integer;
begin
  Result := 0;
end;

function TKExtEditPanel._Release: Integer;
begin
  Result := 0;
end;

{ TKExtFormFieldSet }

procedure TKExtFormFieldSet.AddChild(const AEditItem: IKExtEditItem);
begin
  Items.Add(AEditItem.AsExtObject);
end;

function TKExtFormFieldSet.AsExtObject: TExtObject;
begin
  Result := Self;
end;

function TKExtFormFieldSet.AsObject: TObject;
begin
  Result := Self;
end;

function TKExtFormFieldSet.QueryInterface(const IID: TGUID; out Obj): HRESULT;
begin
  if GetInterface(IID, Obj) then Result := 0 else Result := E_NOINTERFACE;
end;

procedure TKExtFormFieldSet.SetOption(const AName, AValue: string);
begin
  if SameText(AName, 'LabelWidth') then
    LabelWidth := OptionAsInteger(AValue)
  else if SameText(AName, 'Collapsible') then
    Collapsible := OptionAsBoolean(AValue)
  else if SameText(AName, 'Collapsed') then
    Collapsed := OptionAsBoolean(AValue)
  else
    InvalidOption(AName, AValue);
end;

function TKExtFormFieldSet._AddRef: Integer;
begin
  Result := 0;
end;

function TKExtFormFieldSet._Release: Integer;
begin
  Result := 0;
end;

{ TKExtFormCompositeField }

procedure TKExtFormCompositeField.AddChild(const AEditItem: IKExtEditItem);
begin
  Items.Add(AEditItem.AsExtObject);
end;

function TKExtFormCompositeField.AsExtObject: TExtObject;
begin
  Result := Self;
end;

function TKExtFormCompositeField.AsObject: TObject;
begin
  Result := Self;
end;

function TKExtFormCompositeField.QueryInterface(const IID: TGUID; out Obj): HRESULT;
begin
  if GetInterface(IID, Obj) then Result := 0 else Result := E_NOINTERFACE;
end;

procedure TKExtFormCompositeField.SetOption(const AName, AValue: string);
begin
  InvalidOption(AName, AValue);
end;

function TKExtFormCompositeField._AddRef: Integer;
begin
  Result := 0;
end;

function TKExtFormCompositeField._Release: Integer;
begin
  Result := 0;
end;

{ TKExtFormTextField }

function TKExtFormTextField.AsExtFormField: TExtFormField;
begin
  Result := Self;
end;

function TKExtFormTextField.AsExtObject: TExtObject;
begin
  Result := Self;
end;

function TKExtFormTextField.AsObject: TObject;
begin
  Result := Self;
end;

function TKExtFormTextField.QueryInterface(const IID: TGUID; out Obj): HRESULT;
begin
  if GetInterface(IID, Obj) then Result := 0 else Result := E_NOINTERFACE;
end;

procedure TKExtFormTextField.SetOption(const AName, AValue: string);
begin
  if not SetExtFormFieldOption(AsExtFormField, AName, AValue) then
    InvalidOption(AName, AValue);
end;

procedure TKExtFormTextField.SetValueFromField(const AField: TField);
begin
  Assert(Assigned(AField));

  Value := AField.AsString;
end;

function TKExtFormTextField._AddRef: Integer;
begin
  Result := 0;
end;

function TKExtFormTextField._Release: Integer;
begin
  Result := 0;
end;

{ TKExtFormTextArea }

function TKExtFormTextArea.AsExtFormField: TExtFormField;
begin
  Result := Self;
end;

function TKExtFormTextArea.AsExtObject: TExtObject;
begin
  Result := Self;
end;

function TKExtFormTextArea.AsObject: TObject;
begin
  Result := Self;
end;

function TKExtFormTextArea.QueryInterface(const IID: TGUID; out Obj): HRESULT;
begin
  if GetInterface(IID, Obj) then Result := 0 else Result := E_NOINTERFACE;
end;

procedure TKExtFormTextArea.SetOption(const AName, AValue: string);
begin
  if not SetExtFormFieldOption(AsExtFormField, AName, AValue) then
  begin
    if SameText(AName, 'Lines') then
      Height := LinesToPixels(OptionAsInteger(AValue))
    else
      InvalidOption(AName, AValue);
  end;
end;

procedure TKExtFormTextArea.SetValueFromField(const AField: TField);
begin
  Assert(Assigned(AField));

  Value := AField.AsString;
end;

function TKExtFormTextArea._AddRef: Integer;
begin
  Result := 0;
end;

function TKExtFormTextArea._Release: Integer;
begin
  Result := 0;
end;

{ TKExtFormCheckbox }

function TKExtFormCheckbox.AsExtFormField: TExtFormField;
begin
  Result := Self;
end;

function TKExtFormCheckbox.AsExtObject: TExtObject;
begin
  Result := Self;
end;

function TKExtFormCheckbox.AsObject: TObject;
begin
  Result := Self;
end;

function TKExtFormCheckbox.QueryInterface(const IID: TGUID; out Obj): HRESULT;
begin
  if GetInterface(IID, Obj) then Result := 0 else Result := E_NOINTERFACE;
end;

procedure TKExtFormCheckbox.SetOption(const AName, AValue: string);
begin
  if not SetExtFormFieldOption(AsExtFormField, AName, AValue) then
    InvalidOption(AName, AValue);
end;

procedure TKExtFormCheckbox.SetValueFromField(const AField: TField);
begin
  Assert(Assigned(AField));
  Assert(AField.DataType in [ftBoolean, ftSmallint, ftInteger]);

  SetValue(AField.AsBoolean);
end;

function TKExtFormCheckbox._AddRef: Integer;
begin
  Result := 0;
end;

function TKExtFormCheckbox._Release: Integer;
begin
  Result := 0;
end;

{ TKExtFormDateField }

function TKExtFormDateField.AsExtFormField: TExtFormField;
begin
  Result := Self;
end;

function TKExtFormDateField.AsExtObject: TExtObject;
begin
  Result := Self;
end;

function TKExtFormDateField.AsObject: TObject;
begin
  Result := Self;
end;

function TKExtFormDateField.QueryInterface(const IID: TGUID; out Obj): HRESULT;
begin
  if GetInterface(IID, Obj) then Result := 0 else Result := E_NOINTERFACE;
end;

procedure TKExtFormDateField.SetOption(const AName, AValue: string);
begin
  if not SetExtFormFieldOption(AsExtFormField, AName, AValue) then
    InvalidOption(AName, AValue);
end;

procedure TKExtFormDateField.SetValueFromField(const AField: TField);
begin
  Assert(Assigned(AField));
  Assert(AField.DataType in [ftDate, ftDateTime]);

  if AField.AsDateTime = 0 then
    Value := ''
  else
    Value := DateToStr(AField.AsDateTime, Session.FormatSettings);
end;

function TKExtFormDateField._AddRef: Integer;
begin
  Result := 0;
end;

function TKExtFormDateField._Release: Integer;
begin
  Result := 0;
end;

{ TKExtFormComboBoxEditor }

function TKExtFormComboBoxEditor.AsExtFormField: TExtFormField;
begin
  Result := Self;
end;

function TKExtFormComboBoxEditor.AsExtObject: TExtObject;
begin
  Result := Self;
end;

procedure TKExtFormComboBoxEditor.SetOption(const AName, AValue: string);
begin
  if not SetExtFormFieldOption(AsExtFormField, AName, AValue) then
  begin
    if SameText(AName, 'Resizable') then
      Resizable := OptionAsBoolean(AValue)
    else
      InvalidOption(AName, AValue);
  end;
end;

procedure TKExtFormComboBoxEditor.SetValueFromField(const AField: TField);
begin
  Assert(Assigned(AField));

  Value := AField.AsString;
end;

{ TKExtFormContainer }

procedure TKExtFormContainer.AddChild(const AEditItem: IKExtEditItem);
begin
  Items.Add(AEditItem.AsExtObject);
end;

function TKExtFormContainer.AsExtObject: TExtObject;
begin
  Result := Self;
end;

function TKExtFormContainer.AsObject: TObject;
begin
  Result := Self;
end;

procedure TKExtFormContainer.InitDefaults;
begin
  inherited;
  AutoScroll := False;
end;

function TKExtFormContainer.InternalSetOption(const AName,
  AValue: string): Boolean;
begin
  Result := True;
  if SameText(AName, 'Layout') then
    LayoutString := AValue
  else if SameText(AName, 'ColumnWidth') then
    ColumnWidth := OptionAsFloat(AValue)
  else if SameText(AName, 'CharWidth') then
    Width := CharsToPixels(OptionAsInteger(AValue))
  else if SameText(AName, 'Width') then
    SetWidth(OptionAsIntegerOrPerc(AValue))
  else if SameText(AName, 'Anchor') then
    Anchor := AValue
  else
    Result := False;
end;

function TKExtFormContainer.QueryInterface(const IID: TGUID; out Obj): HRESULT;
begin
  if GetInterface(IID, Obj) then Result := 0 else Result := E_NOINTERFACE;
end;

procedure TKExtFormContainer.SetOption(const AName, AValue: string);
begin
  if not InternalSetOption(AName, AValue) then
    InvalidOption(AName, AValue);
end;

function TKExtFormContainer._AddRef: Integer;
begin
  Result := 0;
end;

function TKExtFormContainer._Release: Integer;
begin
  Result := 0;
end;

{ TKExtFormRow }

procedure TKExtFormRow.InitDefaults;
begin
  inherited;
  Layout := lyColumn;
end;

{ TKExtFormRowField }

function TKExtFormRowField.AsExtFormField: TExtFormField;
begin
  Result := FEditor.AsExtFormField;
end;

procedure TKExtFormRowField.InitDefaults;
begin
  inherited;
  Layout := lyForm;
end;

function TKExtFormRowField.Encapsulate(const AValue: IKExtEditor): IKExtEditor;
begin
  Assert(Assigned(AValue));

  FEditor := AValue;
  FEditor.AsExtFormField.SetWidth('100%');
  Items.Add(FEditor.AsExtObject);
  FEditor.AsExtFormField.Anchor := '-5';
  Result := Self;
end;

procedure TKExtFormRowField.SetOption(const AName, AValue: string);
begin
  if not InternalSetOption(AName, AValue) then
    FEditor.SetOption(AName, AValue);
end;

procedure TKExtFormRowField.SetValueFromField(const AField: TField);
begin
  FEditor.SetValueFromField(AField);
end;

{ TKExtFormNumberField }

function TKExtFormNumberField.AsExtFormField: TExtFormField;
begin
  Result := Self;
end;

function TKExtFormNumberField.AsExtObject: TExtObject;
begin
  Result := Self;
end;

function TKExtFormNumberField.AsObject: TObject;
begin
  Result := Self;
end;

function TKExtFormNumberField.QueryInterface(const IID: TGUID; out Obj): HRESULT;
begin
  if GetInterface(IID, Obj) then Result := 0 else Result := E_NOINTERFACE;
end;

procedure TKExtFormNumberField.SetOption(const AName, AValue: string);
begin
  if not SetExtFormFieldOption(AsExtFormField, AName, AValue) then
    InvalidOption(AName, AValue);
end;

procedure TKExtFormNumberField.SetValueFromField(const AField: TField);
begin
  Assert(Assigned(AField));

  Value := AField.AsString;
end;

function TKExtFormNumberField._AddRef: Integer;
begin
  Result := 0;
end;

function TKExtFormNumberField._Release: Integer;
begin
  Result := 0;
end;

{ TKExtFormDateTimeField }

function TKExtFormDateTimeField.AsExtFormField: TExtFormField;
begin
  Result := Self;
end;

function TKExtFormDateTimeField.AsExtObject: TExtObject;
begin
  Result := Self;
end;

function TKExtFormDateTimeField.AsObject: TObject;
begin
  Result := Self;
end;

destructor TKExtFormDateTimeField.Destroy;
begin
  FreeAndNil(FDateConfig);
  FreeAndNil(FTimeConfig);
  inherited;
end;

function TKExtFormDateTimeField.JSClassName: string;
begin
  Result := 'Ext.ux.form.DateTime';
end;

function TKExtFormDateTimeField.QueryInterface(const IID: TGUID; out Obj): HRESULT;
begin
  if GetInterface(IID, Obj) then Result := 0 else Result := E_NOINTERFACE;
end;

procedure TKExtFormDateTimeField.SetOption(const AName, AValue: string);
begin
  if not SetExtFormFieldOption(AsExtFormField, AName, AValue) then
    InvalidOption(AName, AValue);
end;

procedure TKExtFormDateTimeField.SetValueFromField(const AField: TField);
begin
  Assert(Assigned(AField));
  Assert(AField.DataType in [ftDateTime, ftTimeStamp]);

  if AField.AsDateTime = 0 then
    Value := ''
  else
    Value := DateTimeToStr(AField.AsDateTime, Session.FormatSettings);
end;

function TKExtFormDateTimeField._AddRef: Integer;
begin
  Result := 0;
end;

function TKExtFormDateTimeField._Release: Integer;
begin
  Result := 0;
end;

procedure TKExtFormDateTimeField.SetDateConfig(const AValue: TExtObject);
begin
  FDateConfig := AValue;
  AValue.DeleteFromGarbage;
  JSCode('dateConfig:' + VarToJSON([AValue]));
end;

procedure TKExtFormDateTimeField.SetDateFormat(const AValue: string);
begin
  FTimeFormat := AValue;
  JSCode('dateFormat:' + VarToJSON([AValue]));
end;

procedure TKExtFormDateTimeField.SetTimeConfig(const AValue: TExtObject);
begin
  FTimeConfig := AValue;
  AValue.DeleteFromGarbage;
  JSCode('timeConfig:' + VarToJSON([AValue]));
end;

procedure TKExtFormDateTimeField.SetTimeFormat(const AValue: string);
begin
  FTimeFormat := AValue;
  JSCode('timeFormat:' + VarToJSON([AValue]));
end;

{ TKExtFormTimeField }

function TKExtFormTimeField.AsExtFormField: TExtFormField;
begin
  Result := Self;
end;

function TKExtFormTimeField.AsExtObject: TExtObject;
begin
  Result := Self;
end;

function TKExtFormTimeField.AsObject: TObject;
begin
  Result := Self;
end;

function TKExtFormTimeField.QueryInterface(const IID: TGUID; out Obj): HRESULT;
begin
  if GetInterface(IID, Obj) then Result := 0 else Result := E_NOINTERFACE;
end;

procedure TKExtFormTimeField.SetOption(const AName, AValue: string);
begin
  if not SetExtFormFieldOption(AsExtFormField, AName, AValue) then
    InvalidOption(AName, AValue);
end;

procedure TKExtFormTimeField.SetValueFromField(const AField: TField);
begin
  Assert(Assigned(AField));
  Assert(AField.DataType in [ftDateTime, ftTime]);

  if AField.AsDateTime = 0 then
    Value := ''
  else
    Value := TimeToStr(AField.AsDateTime, Session.FormatSettings);
end;

function TKExtFormTimeField._AddRef: Integer;
begin
  Result := 0;
end;

function TKExtFormTimeField._Release: Integer;
begin
  Result := 0;
end;

initialization
  _JSFormatSettings := TFormatSettings.Create;
  _JSFormatSettings.DecimalSeparator := '.';

end.
