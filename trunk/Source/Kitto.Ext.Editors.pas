unit Kitto.Ext.Editors;

{$I Kitto.Defines.inc}

interface

uses
  Generics.Collections,
  Ext, ExtPascal, ExtForm, ExtData,
  EF.Intf, EF.Classes, EF.Tree,
  Kitto.Ext.Base, Kitto.Metadata.Views, Kitto.Store;

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
    FCharWidth: Integer;
    procedure SetCharWidth(const AValue: Integer);
  protected
    procedure InitDefaults; override;
  public
    property CharWidth: Integer read FCharWidth write SetCharWidth;

    function Encapsulate(const AValue: IKExtEditor): IKExtEditor;

    function AsExtFormField: TExtFormField;
    procedure SetOption(const AName, AValue: string);
    destructor Destroy; override;
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

    property DateFormat: string read FDateFormat write SetDateFormat;
    property DateConfig: TExtObject read FDateConfig write SetDateConfig;
    property TimeFormat: string read FTimeFormat write SetTimeFormat;
    property TimeConfig: TExtObject read FTimeConfig write SetTimeConfig;
  end;

  TKExtFormComboBoxEditor = class(TKExtFormComboBox, IKExtEditItem, IKExtEditor)
  private
    FServerStore: TKStore;
    FLookupCommandText: string;
    procedure SetupServerStore(const AViewField: TKViewField;
      const ALookupCommandText: string);
  public
    procedure SetOption(const AName, AValue: string);
    function AsExtObject: TExtObject; inline;
    function AsExtFormField: TExtFormField; inline;
    destructor Destroy; override;
  published
    procedure GetRecordPage;
  end;

  TKExtLayoutDefaults = record
    MemoWidth: Integer;
    MaxFieldWidth: Integer;
    MinFieldWidth: Integer;
    RequiredLabelFormat: string;
    MsgTarget: string;
    procedure Init;
  end;

  ///	<summary>
  ///	  Creates editor based on layouts. Can synthesize a default layout if
  ///	  missing.
  ///	</summary>
  TKExtLayoutProcessor = class
  private
    FViewTable: TKViewTable;
    FForceReadOnly: Boolean;
    FFormPanel: TKExtEditPanel;
    FFocusField: TExtFormField;
    FDefaults: TKExtLayoutDefaults;
    FCurrentEditItem: IKExtEditItem;
    FEditContainers: TStack<IKExtEditContainer>;
    FStoreHeader: TKHeader;
    function TryCreateCheckBox(const AViewField: TKViewField): IKExtEditor;
    function TryCreateDateField(const AViewField: TKViewField;
      const ARowField: TKExtFormRowField; const AFieldWidth: Integer;
      const AIsReadOnly: Boolean): IKExtEditor;
    function TryCreateTimeField(const AViewField: TKViewField;
      const ARowField: TKExtFormRowField; const AFieldWidth: Integer;
      const AIsReadOnly: Boolean): IKExtEditor;
    function TryCreateDateTimeField(const AViewField: TKViewField;
      const ARowField: TKExtFormRowField; const AFieldWidth: Integer;
      const AIsReadOnly: Boolean): IKExtEditor;
    function TryCreateNumberField(const AViewField: TKViewField;
      const ARowField: TKExtFormRowField; const AFieldWidth: Integer;
      const AIsReadOnly: Boolean): IKExtEditor;
    function CreateTextField(const AViewField: TKViewField;
      const ARowField: TKExtFormRowField; const AFieldWidth: Integer;
      const AIsReadOnly: Boolean): IKExtEditor;
    const TRIGGER_WIDTH = 4;
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
    function GetLookupCommandText(const AViewField: TKViewField): string;
    function TryCreateComboBox(const AViewField: TKViewField;
      const ARowField: TKExtFormRowField; const AFieldWidth: Integer;
      const AIsReadOnly: Boolean): IKExtEditor;
    function TryCreateTextArea(const AViewField: TKViewField;
      const ARowField: TKExtFormRowField; const AFieldWidth: Integer;
      const AIsReadOnly: Boolean): IKExtEditor;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
  public
    // Set all properties before calling the CreateEditors methods.
    property ViewTable: TKViewTable read FViewTable write FViewTable;
    property ForceReadOnly: Boolean read FForceReadOnly write FForceReadOnly;
    property FormPanel: TKExtEditPanel read FFormPanel write FFormPanel;
    property StoreHeader: TKHeader read FStoreHeader write FStoreHeader;

    ///	<summary>
    ///	  Creates editors according to the specified layout or a default layout.
    ///	</summary>
    ///	<param name="ALayout">
    ///	  Layout used to create the editors. Pass nil to manufacture a default
    ///	  layout.
    ///	</param>
    procedure CreateEditors(const ALayout: TKLayout);

    ///	<summary>
    ///	  A reference to the first field to focus. Only valid after calling
    ///	  CreateEditors method.
    ///	</summary>
    property FocusField: TExtFormField read FFocusField;
  end;

implementation

uses
  SysUtils, Classes, Math, StrUtils,
  EF.StrUtils, EF.Localization, EF.YAML, EF.Types, EF.SQL,
  Kitto.Ext.Utils, Kitto.JSON, Kitto.Environment, Kitto.Ext.Session, Kitto.SQL,
  Kitto.Metadata.Models, Kitto.Types;

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
  Assert(Assigned(ALayout));

  FCurrentEditItem := nil;
  FEditContainers.Clear;
  FFormPanel.LabelAlign := laTop;
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
  LFieldName: string;
begin
  Assert(Assigned(FViewTable));
  Assert(Assigned(FFormPanel));
  Assert(Assigned(FStoreHeader));

  FFocusField := nil;

  if Assigned(ALayout) then
    CreateEditorsFromLayout(ALayout)
  else
  begin
    FFormPanel.LabelAlign := laLeft;
    for I := 0 to FStoreHeader.FieldCount - 1 do
    begin
      LFieldName := FStoreHeader.Fields[I].FieldName;
      if FViewTable.IsFieldVisible(FViewTable.FieldByAliasedName(LFieldName)) then
        FFormPanel.AddChild(CreateEditor(LFieldName, nil));
    end;
  end;
  if Assigned(FFocusField) then
    FFormPanel.On('afterrender', FFormPanel.JSFunction(FFocusField.JSName + '.focus(false, 1000);'));
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

function TKExtLayoutProcessor.GetLookupCommandText(const AViewField: TKViewField): string;
var
  LReference: TKModelReference;
begin
  Result := AViewField.GetString('LookupCommandText');
  if Result = '' then
  begin
    LReference := AViewField.Reference;
    if Assigned(LReference) then
    begin
      Result := TKSQLBuilder.GetLookupSelectStatement(AViewField);
      Result := AddToSQLWhereClause(Result, AViewField.ModelField.FieldName + ' like ''{query}%''');
    end;
  end;
end;

function TKExtLayoutProcessor.TryCreateComboBox(const AViewField: TKViewField;
  const ARowField: TKExtFormRowField; const AFieldWidth: Integer;
  const AIsReadOnly: Boolean): IKExtEditor;
var
  LLookupCommandText: string;
  LAllowedValues: TEFPairs;
  LComboBox: TKExtFormComboBoxEditor;
begin
  LLookupCommandText := GetLookupCommandText(AViewField);
  LAllowedValues := AViewField.GetChildrenAsPairs('AllowedValues');
  if (LLookupCommandText <> '') or (Length(LAllowedValues) > 0) then
  begin
    LComboBox := TKExtFormComboBoxEditor.Create;
    try
      if not Assigned(ARowField) then
        LComboBox.Width := LComboBox.CharsToPixels(AFieldWidth + TRIGGER_WIDTH)
      else
        ARowField.CharWidth := AFieldWidth + TRIGGER_WIDTH;
      LComboBox.TriggerAction := 'all';
      LComboBox.TypeAhead := True;
      LComboBox.LazyRender := True;
      LComboBox.SelectOnFocus := False;
      // Enable the combo box to post its hidden value instead of the visible description.
      LComboBox.HiddenName := AViewField.AliasedName;
      //LComboBox.Id := LDataField.AliasedName + '_DX';

      if Length(LAllowedValues) > 0 then
        LComboBox.StoreArray := LComboBox.JSArray(PairsToJSON(LAllowedValues))
      else // LLookupCommandText <> ''
      begin
        if (AViewField.Reference <> nil) and AViewField.Reference.ReferencedModel.IsLarge then
          LComboBox.SetupServerStore(AViewField, LLookupCommandText)
        else
        begin
          LComboBox.Mode := 'local';
          LComboBox.StoreArray := LComboBox.JSArray(DataSetToJSON(Environment.MainDBConnection, LLookupCommandText));
        end;
      end;
      if not AIsReadOnly then
        LComboBox.ForceSelection := AViewField.IsRequired;
      Result := LComboBox;
    except
      LComboBox.Free;
      raise;
    end;
  end
  else
    Result := nil;
end;

function TKExtLayoutProcessor.TryCreateTextArea(const AViewField: TKViewField;
  const ARowField: TKExtFormRowField; const AFieldWidth: Integer;
  const AIsReadOnly: Boolean): IKExtEditor;
var
  LTextArea: TKExtFormTextArea;
begin
  if (AViewField.DataType = edtString) and ((AViewField.Size = 0) or (AViewField.Size div SizeOf(Char) >= MULTILINE_EDIT_THRESHOLD)) then
  begin
    LTextArea := TKExtFormTextArea.Create;
    try
      if not Assigned(ARowField) then
        LTextArea.Width := LTextArea.CharsToPixels(AFieldWidth)
      else
        ARowField.CharWidth := AFieldWidth;
      LTextArea.Height := LTextArea.LinesToPixels(AViewField.GetInteger('EditLines', 5));
      LTextArea.AutoScroll := True;
      // Set this if it's the last field.
      //Anchor := '100%';
      if not AIsReadOnly then
      begin
        if AViewField.Size > 0 then
          LTextArea.MaxLength  := AViewField.Size;
        LTextArea.AllowBlank := not AViewField.IsRequired;
      end;
      LTextArea.Grow := True;
      Result := LTextArea;
    except
      LTextArea.Free;
      raise;
    end;
  end
  else
    Result := nil;
end;

function TKExtLayoutProcessor.TryCreateCheckBox(const AViewField: TKViewField): IKExtEditor;
var
  LCheckbox: TKExtFormCheckbox;
begin
  if AViewField.DataType = edtBoolean then
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
  else
    Result := nil;
end;

function TKExtLayoutProcessor.TryCreateDateField(const AViewField: TKViewField;
  const ARowField: TKExtFormRowField; const AFieldWidth: Integer;
  const AIsReadOnly: Boolean): IKExtEditor;
var
  LDateField: TKExtFormDateField;
begin
  if AViewField.DataType = edtDate then
  begin
    LDateField := TKExtFormDateField.Create;
    try
      if not Assigned(ARowField) then
        LDateField.Width := LDateField.CharsToPixels(AFieldWidth + TRIGGER_WIDTH)
      else
        ARowField.CharWidth := AFieldWidth + TRIGGER_WIDTH;
      LDateField.Format := DelphiDateFormatToJSDateFormat(Session.UserFormatSettings.ShortDateFormat);
      LDateField.AltFormats := DelphiDateFormatToJSDateFormat(Session.JSFormatSettings.ShortDateFormat);
      if not AIsReadOnly then
        LDateField.AllowBlank := not AViewField.IsRequired;
      Result := LDateField;
    except
      LDateField.Free;
      raise;
    end;
  end
  else
    Result := nil;
end;

function TKExtLayoutProcessor.TryCreateTimeField(const AViewField: TKViewField;
  const ARowField: TKExtFormRowField; const AFieldWidth: Integer;
  const AIsReadOnly: Boolean): IKExtEditor;
var
  LTimeField: TKExtFormTimeField;
begin
  if AViewField.DataType = edtTime then
  begin
    LTimeField := TKExtFormTimeField.Create;
    try
      if not Assigned(ARowField) then
        LTimeField.Width := LTimeField.CharsToPixels(AFieldWidth + TRIGGER_WIDTH)
      else
        ARowField.CharWidth := AFieldWidth + TRIGGER_WIDTH;
      // Don't use Delphi format here.
      LTimeField.Format := DelphiTimeFormatToJSTimeFormat(Session.UserFormatSettings.ShortTimeFormat);
      if not AIsReadOnly then
        LTimeField.AllowBlank := not AViewField.IsRequired;
      Result := LTimeField;
    except
      LTimeField.Free;
      raise;
    end;
  end
  else
    Result := nil;
end;

function TKExtLayoutProcessor.TryCreateDateTimeField(const AViewField: TKViewField;
  const ARowField: TKExtFormRowField; const AFieldWidth: Integer;
  const AIsReadOnly: Boolean): IKExtEditor;
const
  SPACER_WIDTH = 1;
var
  LDateTimeField: TKExtFormDateTimeField;
begin
  if AViewField.DataType = edtDateTime then
  begin
    LDateTimeField := TKExtFormDateTimeField.Create;
    try
      if not Assigned(ARowField) then
        LDateTimeField.Width := LDateTimeField.CharsToPixels(AFieldWidth + (2 * TRIGGER_WIDTH) + SPACER_WIDTH)
      else
        ARowField.CharWidth := AFieldWidth + (2 * TRIGGER_WIDTH) + SPACER_WIDTH;
      // Don't use Delphi format here.
      LDateTimeField.DateFormat := DelphiDateFormatToJSDateFormat(Session.UserFormatSettings.ShortDateFormat);
      LDateTimeField.TimeFormat := DelphiTimeFormatToJSTimeFormat(Session.UserFormatSettings.ShortTimeFormat);
      if not AIsReadOnly then
      begin
        LDateTimeField.DateConfig := LDateTimeField.JSObject('allowBlank:false');
        LDateTimeField.TimeConfig := LDateTimeField.JSObject('allowBlank:false');
      end
      else
      begin
        LDateTimeField.DateConfig := LDateTimeField.JSObject('readOnly:true');
        LDateTimeField.TimeConfig := LDateTimeField.JSObject('readOnly:true');
      end;
      Result := LDateTimeField;
    except
      LDateTimeField.Free;
      raise;
    end;
  end
  else
    Result := nil;
end;

function TKExtLayoutProcessor.TryCreateNumberField(const AViewField: TKViewField;
  const ARowField: TKExtFormRowField; const AFieldWidth: Integer;
  const AIsReadOnly: Boolean): IKExtEditor;
var
  LNumberField: TKExtFormNumberField;
  LNode: TEFNode;
begin
  if AViewField.DataType in [edtInteger, edtCurrency, edtFloat, edtDecimal] then
  begin
    LNumberField := TKExtFormNumberField.Create;
    try
      if not Assigned(ARowField) then
        LNumberField.Width := LNumberField.CharsToPixels(AFieldWidth)
      else
        ARowField.CharWidth := AFieldWidth;
      if not AIsReadOnly then
      begin
        LNumberField.AllowDecimals := AViewField.DataType in [edtCurrency, edtFloat, edtDecimal];
        LNumberField.DecimalSeparator := Session.UserFormatSettings.DecimalSeparator;
        LNumberField.AllowNegative := True;
        if LNumberField.AllowDecimals then
          LNumberField.DecimalPrecision := 2
        else
        begin
          LNode := AViewField.FindNode('MaxValue');
          if Assigned(LNode) then
            LNumberField.MaxValue := LNode.AsInteger;
          LNode := AViewField.FindNode('MinValue');
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
    Result := nil;
end;

function TKExtLayoutProcessor.CreateTextField(const AViewField: TKViewField;
  const ARowField: TKExtFormRowField; const AFieldWidth: Integer;
  const AIsReadOnly: Boolean): IKExtEditor;
var
  LTextField: TKExtFormTextField;
begin
  LTextField := TKExtFormTextField.Create;
  try
    if not Assigned(ARowField) then
      LTextField.Width := LTextField.CharsToPixels(AFieldWidth)
    else
      ARowField.CharWidth := AFieldWidth;
    if not AIsReadOnly then
    begin
      if AViewField.Size <> 0 then
        LTextField.MaxLength := AViewField.Size;
      LTextField.AllowBlank := not AViewField.IsRequired;
    end;
    Result := LTextField;
  except
    LTextField.Free;
    raise;
  end;
end;

function TKExtLayoutProcessor.CreateEditor(const AFieldName: string;
  const AContainer: IKExtEditContainer): IKExtEditor;
var
  LFieldWidth: Integer;
  LIsReadOnly: Boolean;
  LLabel: string;
  LViewField: TKViewField;
  LRowField: TKExtFormRowField;
  LForceCase: string;
  LNode: TEFNode;
begin
  LViewField := FViewTable.FieldByAliasedName(AFieldName);

  // Store common properties.
  LFieldWidth := LViewField.DisplayWidth;
  if LFieldWidth = 0 then
    // Blobs have Size = 0.
    LFieldWidth := Min(IfThen(LViewField.Size = 0, FDefaults.MemoWidth, LViewField.Size), FDefaults.MaxFieldWidth);
  // Minimum cap - avoids too short combo boxes.
  LFieldWidth := Max(LFieldWidth, FDefaults.MinFieldWidth);

  LIsReadOnly := LViewField.IsReadOnly or ViewTable.IsReadOnly or FForceReadOnly;
  LLabel := _(LViewField.DisplayLabel);
  if not LIsReadOnly and LViewField.IsRequired then
    LLabel := Format(FDefaults.RequiredLabelFormat, [LLabel]);

  if AContainer is TKExtFormRow then
    LRowField := TKExtFormRowField.Create
  else
    LRowField := nil;

  Result := TryCreateComboBox(LViewField, LRowField, LFieldWidth, LIsReadOnly);
  if Result = nil then
    Result := TryCreateTextArea(LViewField, LRowField, LFieldWidth, LIsReadOnly);
  if Result = nil then
    Result := TryCreateCheckBox(LViewField);
  if Result = nil then
    Result := TryCreateDateField(LViewField, LRowField, LFieldWidth, LIsReadOnly);
  if Result = nil then
    Result := TryCreateTimeField(LViewField, LRowField, LFieldWidth, LIsReadOnly);
  if Result = nil then
    Result := TryCreateDateTimeField(LViewField, LRowField, LFieldWidth, LIsReadOnly);
  if Result = nil then
    Result := TryCreateNumberField(LViewField, LRowField, LFieldWidth, LIsReadOnly);
  if Result = nil then
    Result := CreateTextField(LViewField, LRowField, LFieldWidth, LIsReadOnly);

{ TODO : refactor }
  // Setup some validation.
  if not LIsReadOnly and (Result.AsExtFormField is TExtFormTextField) then
  begin
    LNode := LViewField.FindNode('MaxLength');
    if Assigned(LNode) then
      TExtFormTextField(Result).MaxLength := LNode.AsInteger;
    LNode := LViewField.FindNode('MinLength');
    if Assigned(LNode) then
      TExtFormTextField(Result).MinLength := LNode.AsInteger;
    LNode := LViewField.FindNode('Validator');
    if Assigned(LNode) then
      TExtFormTextField(Result).Validator := TExtFormTextField(Result).JSFunction('value', LNode.AsString);
    { TODO : refactor this to unified field-level rules }
    LNode := LViewField.FindNode('ValidationType'); // alpha alphanum email url
    if Assigned(LNode) then
      TExtFormTextField(Result).Vtype := LNode.AsString;

    LForceCase := LViewField.GetString('ForceCase'); // upper lower caps none
    if LForceCase <> '' then
      TExtFormTextField(Result).EnableKeyEvents := True;
    if SameText(LForceCase, 'Upper')  then
      TExtFormTextField(Result).On('keyup', TExtFormTextField(Result).JSFunction('field, e', 'field.setValue(field.getRawValue().toUpperCase());'))
    else if SameText(LForceCase, 'Lower')  then
      TExtFormTextField(Result).On('keyup', TExtFormTextField(Result).JSFunction('field, e', 'field.setValue(field.getRawValue().toLowerCase());'))
    else if SameText(LForceCase, 'Caps')  then
      TExtFormTextField(Result).On('change', TExtFormTextField(Result).JSFunction('field, newValue, oldValue', 'field.setValue(newValue.capitalize());'));
  end;

  Result.AsExtFormField.AutoScroll := False; // Don't display a h. scrollbar for larger fields.
//  if Result.AsExtFormField.Id = '' then
//    Result.AsExtFormField.Id := LViewField.AliasedName;
  Result.AsExtFormField.Name := LViewField.AliasedName;
  Result.AsExtFormField.ReadOnly := LIsReadOnly;
  // Don't disable: it will be missing from the POST and SaveChanges does not handle that yet.
  { TODO : make disabling configurable - watch out for disabled fields changed by triggers (they should be POSTed) }
  //Disabled := ReadOnly;
  Result.AsExtFormField.FieldLabel := LLabel;
  Result.AsExtFormField.MsgTarget := LowerCase(FDefaults.MsgTarget);

  if (FFocusField = nil) and not Result.AsExtFormField.ReadOnly and not Result.AsExtFormField.Disabled then
    FFocusField := Result.AsExtFormField;

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
  MinFieldWidth := 5;
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
  Result := -1;
end;

function TKExtEditPanel._Release: Integer;
begin
  Result := -1;
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
  Result := -1;
end;

function TKExtFormFieldSet._Release: Integer;
begin
  Result := -1;
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
  Result := -1;
end;

function TKExtFormCompositeField._Release: Integer;
begin
  Result := -1;
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

function TKExtFormTextField._AddRef: Integer;
begin
  Result := -1;
end;

function TKExtFormTextField._Release: Integer;
begin
  Result := -1;
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

function TKExtFormTextArea._AddRef: Integer;
begin
  Result := -1;
end;

function TKExtFormTextArea._Release: Integer;
begin
  Result := -1;
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

function TKExtFormCheckbox._AddRef: Integer;
begin
  Result := -1;
end;

function TKExtFormCheckbox._Release: Integer;
begin
  Result := -1;
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

function TKExtFormDateField._AddRef: Integer;
begin
  Result := -1;
end;

function TKExtFormDateField._Release: Integer;
begin
  Result := -1;
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

destructor TKExtFormComboBoxEditor.Destroy;
begin
  FreeAndNil(FServerStore);
  inherited;
end;

procedure TKExtFormComboBoxEditor.GetRecordPage;
var
  LStart: Integer;
  LLimit: Integer;
  LPageRecordCount: Integer;
begin
  Assert(Assigned(FServerStore));

{ TODO : Fully refreshing at each page change might be inefficient.
  Shall we provide a switch to turn it off on a form-by-form basis, or use
  FIRST/SKIP/ROWS to only fetch relevant rows in a database-dependent way?
  For now, let's just stick with full refresh always. }
  FServerStore.Load(Environment.MainDBConnection,
    ReplaceStr(FLookupCommandText, '{query}', ReplaceStr(Session.Query['query'], '''', '''''')));

  LStart := Session.QueryAsInteger['start'];
  LLimit := Session.QueryAsInteger['limit'];
  LPageRecordCount := Min(Max(LLimit, DEFAULT_PAGE_RECORD_COUNT), FServerStore.RecordCount - LStart);

  Session.Response := '{Total:' + IntToStr(FServerStore.RecordCount) + ',Root:'
    + FServerStore.GetAsJSON(LStart, LPageRecordCount) + '}';
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

procedure TKExtFormComboBoxEditor.SetupServerStore(const AViewField: TKViewField;
  const ALookupCommandText: string);
var
  I: Integer;
begin
  Assert(Assigned(AViewField));
  Assert(Assigned(AViewField.Reference));
  Assert(ALookupCommandText <> '');

  FLookupCommandText := ALookupCommandText;
  FreeAndNil(FServerStore);
  FServerStore := AViewField.CreateStore;
  Store := TExtDataStore.Create;
  Store.Url := MethodURI(GetRecordPage);
  Store.Reader := TExtDataJsonReader.Create(JSObject('')); // Must pass '' otherwise invalid code is generated.
  TExtDataJsonReader(Store.Reader).Root := 'Root';
  TExtDataJsonReader(Store.Reader).TotalProperty := 'Total';
  for I := 0 to FServerStore.Header.FieldCount - 1 do
    with TExtDataField.AddTo(Store.Reader.Fields) do
      Name := FServerStore.Header.Fields[I].FieldName;
  ValueField := FServerStore.Header.Fields[0].FieldName;
  DisplayField := FServerStore.Header.Fields[1].FieldName;
  MinChars := 4;
  //PageSize := 20;
  //Resizable := True;
  //MinHeight := LinesToPixels(5);
  Mode := 'remote';
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
  Result := -1;
end;

function TKExtFormContainer._Release: Integer;
begin
  Result := -1;
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

destructor TKExtFormRowField.Destroy;
begin
  NilEFIntf(FEditor);
  inherited;
end;

function TKExtFormRowField.Encapsulate(const AValue: IKExtEditor): IKExtEditor;
begin
  Assert(Assigned(AValue));

  FEditor := AValue;
  Items.Add(FEditor.AsExtObject);
  FEditor.AsExtFormField.Width := CharsToPixels(FCharWidth - 1);
  //FEditor.AsExtFormField.SetWidth('100%');
  //FEditor.AsExtFormField.Anchor := '-5';
  Result := Self;
end;

procedure TKExtFormRowField.SetCharWidth(const AValue: Integer);
begin
  FCharWidth := AValue;
  Width := CharsToPixels(AValue);
end;

procedure TKExtFormRowField.SetOption(const AName, AValue: string);
begin
  if not InternalSetOption(AName, AValue) then
    FEditor.SetOption(AName, AValue);
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

function TKExtFormNumberField._AddRef: Integer;
begin
  Result := -1;
end;

function TKExtFormNumberField._Release: Integer;
begin
  Result := -1;
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

function TKExtFormDateTimeField._AddRef: Integer;
begin
  Result := -1;
end;

function TKExtFormDateTimeField._Release: Integer;
begin
  Result := -1;
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

function TKExtFormTimeField._AddRef: Integer;
begin
  Result := -1;
end;

function TKExtFormTimeField._Release: Integer;
begin
  Result := -1;
end;

initialization
  _JSFormatSettings := TFormatSettings.Create;
  _JSFormatSettings.DecimalSeparator := '.';

end.
