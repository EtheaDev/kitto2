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

unit Kitto.Ext.Editors;

{$I Kitto.Defines.inc}

interface

uses
  SysUtils, Classes, Generics.Collections,
  Ext, ExtPascal, ExtForm, ExtData, ExtUxForm,
  EF.Intf, EF.Classes, EF.Tree,
  Kitto.Ext.Base, Kitto.Metadata.Views, Kitto.Metadata.DataView, Kitto.Store;

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

    function GetRecordField: TKViewTableField;
    procedure SetRecordField(const AValue: TKViewTableField);
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
    FRecordField: TKViewTableField;
    procedure SetCharWidth(const AValue: Integer);
  protected
    procedure InitDefaults; override;
  public
    destructor Destroy; override;
  public
    function Encapsulate(const AValue: IKExtEditor): IKExtEditor;
    property CharWidth: Integer read FCharWidth write SetCharWidth;
    function AsExtFormField: TExtFormField;
    procedure SetOption(const AName, AValue: string);
    function GetRecordField: TKViewTableField;
    procedure SetRecordField(const AValue: TKViewTableField);
  end;

{ TODO : support the CheckboxGroup and Radiogroup containers? }

  TKExtFormNumberField = class(TExtFormNumberField, IKExtEditItem, IKExtEditor)
  private
    FRecordField: TKViewTableField;
  public
    function AsObject: TObject; inline;
    function QueryInterface(const IID: TGUID; out Obj): HRESULT; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure SetOption(const AName, AValue: string);
    function AsExtObject: TExtObject; inline;
    function AsExtFormField: TExtFormField; inline;
    function GetRecordField: TKViewTableField;
    procedure SetRecordField(const AValue: TKViewTableField);
  end;

  TKExtFormTextField = class(TExtFormTextField, IKExtEditItem, IKExtEditor)
  private
    FRecordField: TKViewTableField;
  public
    function AsObject: TObject; inline;
    function QueryInterface(const IID: TGUID; out Obj): HRESULT; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure SetOption(const AName, AValue: string);
    function AsExtObject: TExtObject; inline;
    function AsExtFormField: TExtFormField; inline;
    function GetRecordField: TKViewTableField;
    procedure SetRecordField(const AValue: TKViewTableField);
  end;

  TKExtFormTextArea = class(TExtFormTextArea, IKExtEditItem, IKExtEditor)
  private
    FRecordField: TKViewTableField;
  public
    function AsObject: TObject; inline;
    function QueryInterface(const IID: TGUID; out Obj): HRESULT; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure SetOption(const AName, AValue: string);
    function AsExtObject: TExtObject; inline;
    function AsExtFormField: TExtFormField; inline;
    function GetRecordField: TKViewTableField;
    procedure SetRecordField(const AValue: TKViewTableField);
  end;

  TKExtFormCheckbox = class(TExtFormCheckbox, IKExtEditItem, IKExtEditor)
  private
    FRecordField: TKViewTableField;
  public
    function AsObject: TObject; inline;
    function QueryInterface(const IID: TGUID; out Obj): HRESULT; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure SetOption(const AName, AValue: string);
    function AsExtObject: TExtObject; inline;
    function AsExtFormField: TExtFormField; inline;
    function GetRecordField: TKViewTableField;
    procedure SetRecordField(const AValue: TKViewTableField);
  end;

  TKExtFormDateField = class(TExtFormDateField, IKExtEditItem, IKExtEditor)
  private
    FRecordField: TKViewTableField;
  public
    function AsObject: TObject; inline;
    function QueryInterface(const IID: TGUID; out Obj): HRESULT; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure SetOption(const AName, AValue: string);
    function AsExtObject: TExtObject; inline;
    function AsExtFormField: TExtFormField; inline;
    function GetRecordField: TKViewTableField;
    procedure SetRecordField(const AValue: TKViewTableField);
  end;

  TKExtFormTimeField = class(TExtFormTimeField, IKExtEditItem, IKExtEditor)
  private
    FRecordField: TKViewTableField;
  public
    function AsObject: TObject; inline;
    function QueryInterface(const IID: TGUID; out Obj): HRESULT; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure SetOption(const AName, AValue: string);
    function AsExtObject: TExtObject; inline;
    function AsExtFormField: TExtFormField; inline;
    function GetRecordField: TKViewTableField;
    procedure SetRecordField(const AValue: TKViewTableField);
  end;

  TKExtFormDateTimeField = class(TExtFormField, IKExtEditItem, IKExtEditor)
  private
    FTimeFormat: string;
    FDateFormat: string;
    FDateConfig: TExtObject;
    FTimeConfig: TExtObject;
    FAltDateFormats: string;
    FAllowBlank: Boolean;
    FAltTimeFormats: string;
    FRecordField: TKViewTableField;
    procedure SetDateFormat(const AValue: string);
    procedure SetTimeFormat(const AValue: string);
    procedure SetAltDateFormats(const AValue: string);
    procedure SetAllowBlank(const AValue: Boolean);
    procedure SetAltTimeFormats(const AValue: string);
    //procedure SetDateConfig(const AValue: TExtObject);
    //procedure SetTimeConfig(const AValue: TExtObject);
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
    function GetRecordField: TKViewTableField;
    procedure SetRecordField(const AValue: TKViewTableField);

    property DateFormat: string read FDateFormat write SetDateFormat;
    //property DateConfig: TExtObject read FDateConfig write SetDateConfig;
    property TimeFormat: string read FTimeFormat write SetTimeFormat;
    //property TimeConfig: TExtObject read FTimeConfig write SetTimeConfig;
    property AltDateFormats: string read FAltDateFormats write SetAltDateFormats;
    property AltTimeFormats: string read FAltTimeFormats write SetAltTimeFormats;
    property AllowBlank: Boolean read FAllowBlank write SetAllowBlank;
  end;

  TKExtFormComboBoxEditor = class(TKExtFormComboBox, IKExtEditItem, IKExtEditor)
  private
    FServerStore: TKStore;
    FLookupCommandText: string;
    FRecordField: TKViewTableField;
    procedure SetupServerStore(const AField: TKViewTableField;
      const ALookupCommandText: string);
  protected
    procedure InitDefaults; override;
  public
    procedure SetOption(const AName, AValue: string);
    function AsExtObject: TExtObject; inline;
    function AsExtFormField: TExtFormField; inline;
    destructor Destroy; override;
    function GetRecordField: TKViewTableField;
    procedure SetRecordField(const AValue: TKViewTableField);
  published
    procedure GetRecordPage;
  end;

  TKExtFormFileUploadField = class(TExtUxFormFileUploadField, IKExtEditItem, IKExtEditor)
  private
    FRecordField: TKViewTableField;
  public
    function AsObject: TObject; inline;
    function QueryInterface(const IID: TGUID; out Obj): HRESULT; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure SetOption(const AName, AValue: string);
    function AsExtObject: TExtObject; inline;
    function AsExtFormField: TExtFormField; inline;
    function GetRecordField: TKViewTableField;
    procedure SetRecordField(const AValue: TKViewTableField);
  end;

  TKExtFormFileEditor = class(TExtPanel, IKExtEditItem, IKExtEditor)
  strict private
    FDescriptionField: TExtFormTextField;
    FWindow: TKExtModalWindow;
    FDownloadButton: TExtButton;
    FIsReadOnly: Boolean;
    FClearButton: TExtButton;
    FTotalWidth: Integer;
    FPictureView: TExtPanel;
    FImageWidth: Integer;
    FImageHeight: Integer;
    function GetContentDescription: string;
    procedure CreateGUI;
    procedure UpdateGUI(const AUpdatePicture: Boolean);
    procedure PictureViewAfterRender(This: TExtComponent);
  strict protected
    FRecordField: TKViewTableField;
    FLastUploadedFullFileName: string;
    FLastUploadedOriginalFileName: string;
    function GetCurrentServerFileName: string; virtual; abstract;
    function GetCurrentClientFileName: string; virtual; abstract;
    function GetCurrentContentSize: Integer; virtual; abstract;
    procedure FileUploaded(const AFileName: string); virtual;
    procedure DownloadFile(const AServerFileName, AClientFileName: string); virtual; abstract;
    procedure DownloadThumbnailedFile(const AServerFileName, AClientFileName: string); virtual; abstract;
    procedure ClearContents; virtual;
    procedure DownloadThumbnailedStream(const AStream: TStream; const AFileName: string);
  public
    function AsObject: TObject; inline;
    function QueryInterface(const IID: TGUID; out Obj): HRESULT; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure SetOption(const AName, AValue: string);
    function AsExtObject: TExtObject; inline;
    function AsExtFormField: TExtFormField; inline;
    function GetRecordField: TKViewTableField;
    procedure SetRecordField(const AValue: TKViewTableField);
    property IsReadOnly: Boolean read FIsReadOnly write FIsReadOnly;
    property TotalWidth: Integer read FTotalWidth write FTotalWidth;
  published
    procedure ShowUploadFileDialog;
    procedure Upload;
    procedure Clear;
    procedure PostUpload;
    procedure StartDownload;
    procedure DownloadFieldData;
    procedure GetImage;
    procedure GetImageContent;
  end;

  TKExtFormFileReferenceEditor = class(TKExtFormFileEditor)
  strict private
    function GetFieldPath: string;
  strict protected
    procedure ClearContents; override;
    procedure DownloadFile(const AServerFileName, AClientFileName: string); override;
    procedure DownloadThumbnailedFile(const AServerFileName, AClientFileName: string); override;
    function GetCurrentServerFileName: string; override;
    function GetCurrentClientFileName: string; override;
    function GetCurrentContentSize: Integer; override;
    procedure FileUploaded(const AFileName: string); override;
  end;

  TKExtFormFileBlobEditor = class(TKExtFormFileEditor)
  strict protected
    procedure ClearContents; override;
    function GetCurrentServerFileName: string; override;
    function GetCurrentClientFileName: string; override;
    function GetCurrentContentSize: Integer; override;
    procedure FileUploaded(const AFileName: string); override;
    procedure DownloadFile(const AServerFileName, AClientFileName: string); override;
    procedure DownloadThumbnailedFile(const AServerFileName, AClientFileName: string); override;
  end;

  TKExtLayoutDefaults = record
    MemoWidth: Integer;
    MaxFieldWidth: Integer;
    MinFieldWidth: Integer;
    RequiredLabelTemplate: string;
    MsgTarget: string;
    procedure Init;
  end;

  ///	<summary>
  ///	  Creates editor based on layouts. Can synthesize a default layout if
  ///	  missing.
  ///	</summary>
  TKExtLayoutProcessor = class
  private
    FDataRecord: TKViewTableRecord;
    FForceReadOnly: Boolean;
    FFormPanel: TKExtEditPanel;
    FFocusField: TExtFormField;
    FDefaults: TKExtLayoutDefaults;
    FCurrentEditItem: IKExtEditItem;
    FEditContainers: TStack<IKExtEditContainer>;
    FOnFieldChange: TExtFormFieldOnChange;
    FOnNewEditor: TProc<IKExtEditor>;
    const TRIGGER_WIDTH = 4;
    function GetViewTable: TKViewTable;
    function DerivedFieldsExist(const AViewField: TKViewField): Boolean;
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
    function TryCreateFileEditor(const AViewField: TKViewField;
      const ARowField: TKExtFormRowField; const AFieldWidth: Integer;
      const AIsReadOnly: Boolean; const ALabel: string): IKExtEditor;
    function CreateTextField(const AViewField: TKViewField;
      const ARowField: TKExtFormRowField; const AFieldWidth: Integer;
      const AIsReadOnly: Boolean): IKExtEditor;
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
    function TryCreateComboBox(const AField: TKViewTableField;
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
    property DataRecord: TKViewTableRecord read FDataRecord write FDataRecord;
    property ViewTable: TKViewTable read GetViewTable;
    property ForceReadOnly: Boolean read FForceReadOnly write FForceReadOnly;
    property FormPanel: TKExtEditPanel read FFormPanel write FFormPanel;
    property OnFieldChange: TExtFormFieldOnChange read FOnFieldChange write FOnFieldChange;
    property OnNewEditor: TProc<IKExtEditor> read FOnNewEditor write FOnNewEditor;

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
  Types, Math, StrUtils, Windows, Graphics, jpeg, pngimage,
  EF.SysUtils, EF.StrUtils, EF.Localization, EF.YAML, EF.Types, EF.SQL, EF.JSON,
  Kitto.SQL, Kitto.Metadata.Models, Kitto.Types, Kitto.AccessControl,
  Kitto.Rules, Kitto.Ext.Utils, Kitto.Ext.Session, Kitto.Ext.Rules;

const
  {
    String fields of this size or longer are represented by multiline edit
    controls.
  }
  MULTILINE_EDIT_THRESHOLD = 200;

procedure InvalidOption(const AName, AValue: string);
begin
  raise EEFError.CreateFmt(_('Unknown or misplaced option %s: %s.'), [AName, AValue]);
end;

function OptionAsFloat(const AOptionValue: string): Double;
begin
  // Floats in Yaml always use the dot as decimal separator.
  if not TryStrToFloat(AOptionValue, Result, Session.Config.JSFormatSettings) then
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
  Assert(Assigned(ANode));

  // Skip invisible fields.
  if SameText(ANode.Name, 'Field') then
  begin
    LViewField := ViewTable.FieldByAliasedName(ANode.AsString);
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
begin
  Assert(Assigned(FFormPanel));

  FFocusField := nil;

  if Assigned(ALayout) then
    CreateEditorsFromLayout(ALayout)
  else
  begin
    FFormPanel.LabelAlign := laLeft;
    for I := 0 to ViewTable.FieldCount - 1 do
    begin
      if ViewTable.IsFieldVisible(ViewTable.Fields[I]) and ViewTable.Fields[I].IsAccessGranted(ACM_READ) then
        FFormPanel.AddChild(CreateEditor(ViewTable.Fields[I].AliasedName, nil));
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
begin
  if AViewField.IsReference then
  begin
    Result := TKSQLBuilder.GetLookupSelectStatement(AViewField);
    if AViewField.ModelField.ReferencedModel.IsLarge then
      Result := AddToSQLWhereClause(Result, AViewField.ModelField.ReferencedModel.CaptionField.DBColumnName + ' like ''{query}%''');
  end
  else
    Result := '';
end;

function TKExtLayoutProcessor.GetViewTable: TKViewTable;
begin
  Assert(Assigned(FDataRecord));

  Result := FDataRecord.ViewTable;
end;

function TKExtLayoutProcessor.TryCreateComboBox(const AField: TKViewTableField;
  const ARowField: TKExtFormRowField; const AFieldWidth: Integer;
  const AIsReadOnly: Boolean): IKExtEditor;
var
  LLookupCommandText: string;
  LAllowedValues: TEFPairs;
  LComboBox: TKExtFormComboBoxEditor;
  I: Integer;
begin
  LLookupCommandText := GetLookupCommandText(AField.ViewField);
  LAllowedValues := AField.ViewField.GetChildrenAsPairs('AllowedValues', True);
  // Translate allowed value descriptions if needed.
  for I := Low(LAllowedValues) to High(LAllowedValues) do
    LAllowedValues[I].Value := _(LAllowedValues[I].Value);
  if (LLookupCommandText <> '') or (Length(LAllowedValues) > 0) then
  begin
    LComboBox := TKExtFormComboBoxEditor.Create;
    try
      if not Assigned(ARowField) then
        LComboBox.Width := LComboBox.CharsToPixels(AFieldWidth + TRIGGER_WIDTH)
      else
        ARowField.CharWidth := AFieldWidth + TRIGGER_WIDTH;
      // Enable the combo box to post its hidden value instead of the visible description.
      LComboBox.HiddenName := AField.ViewField.FieldNamesForUpdate;

      if Length(LAllowedValues) > 0 then
        LComboBox.StoreArray := LComboBox.JSArray(PairsToJSON(LAllowedValues))
      else // LLookupCommandText <> ''
      begin
        if AField.ViewField.IsReference and AField.ViewField.ModelField.ReferencedModel.IsLarge then
          LComboBox.SetupServerStore(AField, LLookupCommandText)
        else
        begin
          LComboBox.Mode := 'local';
          LComboBox.StoreArray := LComboBox.JSArray(DataSetToJSON(Session.Config.DBConnections[AField.ViewField.Table.DatabaseName], LLookupCommandText));
        end;
      end;
      if not AIsReadOnly then
        //LComboBox.ForceSelection := AViewField.IsRequired
        LComboBox.ForceSelection := True
      else
        LComboBox.ReadOnly := True;
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
  if AViewField.IsBlob or (AViewField.Size div SizeOf(Char) >= MULTILINE_EDIT_THRESHOLD) then
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
  if AViewField.DataType is TEFBooleanDataType then
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
  LFormat: string;
begin
  if AViewField.DataType is TEFDateDataType then
  begin
    LDateField := TKExtFormDateField.Create;
    try
      if not Assigned(ARowField) then
        LDateField.Width := LDateField.CharsToPixels(AFieldWidth + TRIGGER_WIDTH)
      else
        ARowField.CharWidth := AFieldWidth + TRIGGER_WIDTH;
      LFormat := AViewField.EditFormat;
      if LFormat = '' then
        LFormat := Session.Config.UserFormatSettings.ShortDateFormat;
      LDateField.Format := DelphiDateFormatToJSDateFormat(LFormat);
      LDateField.AltFormats := DelphiDateFormatToJSDateFormat(Session.Config.JSFormatSettings.ShortDateFormat);
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
  LFormat: string;
begin
  if AViewField.DataType is TEFTimeDataType then
  begin
    LTimeField := TKExtFormTimeField.Create;
    try
      if not Assigned(ARowField) then
        LTimeField.Width := LTimeField.CharsToPixels(AFieldWidth + TRIGGER_WIDTH)
      else
        ARowField.CharWidth := AFieldWidth + TRIGGER_WIDTH;

      LFormat := AViewField.EditFormat;
      if LFormat = '' then
        LFormat := Session.Config.UserFormatSettings.ShortTimeFormat;
      LTimeField.Format := DelphiTimeFormatToJSTimeFormat(LFormat);
      LTimeField.AltFormats := DelphiTimeFormatToJSTimeFormat(Session.Config.JSFormatSettings.ShortTimeFormat);
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
  LFormats: TStringDynArray;
  LDateFormat: string;
  LTimeFormat: string;
begin
  if AViewField.DataType is TEFDateTimeDataType then
  begin
    LDateTimeField := TKExtFormDateTimeField.Create;
    try
      if not Assigned(ARowField) then
        LDateTimeField.Width := LDateTimeField.CharsToPixels(AFieldWidth + (2 * TRIGGER_WIDTH) + SPACER_WIDTH)
      else
        ARowField.CharWidth := AFieldWidth + (2 * TRIGGER_WIDTH) + SPACER_WIDTH;
      LFormats := Split(AViewField.EditFormat, ' ');
      LDateFormat := IfThen(Length(LFormats) > 0, LFormats[0], Session.Config.UserFormatSettings.ShortDateFormat);
      LTimeFormat := IfThen(Length(LFormats) > 1, LFormats[1], Session.Config.UserFormatSettings.ShortTimeFormat);
      LDateTimeField.DateFormat := DelphiDateFormatToJSDateFormat(LDateFormat);
      LDateTimeField.AltDateFormats := DelphiDateFormatToJSDateFormat(Session.Config.JSFormatSettings.ShortDateFormat);
      LDateTimeField.TimeFormat := DelphiTimeFormatToJSTimeFormat(Session.Config.UserFormatSettings.ShortTimeFormat);
      LDateTimeField.AltTimeFormats := DelphiTimeFormatToJSTimeFormat(Session.Config.JSFormatSettings.ShortTimeFormat);
      if not AIsReadOnly then
        LDateTimeField.AllowBlank := not AViewField.IsRequired;
//      if not AIsReadOnly then
//      begin
//        LDateTimeField.DateConfig := LDateTimeField.JSObject('allowBlank:false');
//        LDateTimeField.TimeConfig := LDateTimeField.JSObject('allowBlank:false');
//      end
//      else
//      begin
//        LDateTimeField.DateConfig := LDateTimeField.JSObject('readOnly:true');
//        LDateTimeField.TimeConfig := LDateTimeField.JSObject('readOnly:true');
//      end;
      Result := LDateTimeField;
    except
      LDateTimeField.Free;
      raise;
    end;
  end
  else
    Result := nil;
end;

function TKExtLayoutProcessor.TryCreateFileEditor(const AViewField: TKViewField;
  const ARowField: TKExtFormRowField; const AFieldWidth: Integer;
  const AIsReadOnly: Boolean; const ALabel: string): IKExtEditor;
var
  LFileEditor: TKExtFormFileEditor;
begin
  if (AViewField.DataType is TEFBlobDataType) or (AViewField.DataType is TKFileReferenceDataType) then
  begin
    if AViewField.DataType is TEFBlobDataType then
      LFileEditor := TKExtFormFileBlobEditor.Create
    else
      LFileEditor := TKExtFormFileReferenceEditor.Create;
    try
      LFileEditor.IsReadOnly := AIsReadOnly;
      LFileEditor.FieldLabel := ALabel;
      if not Assigned(ARowField) then
        LFileEditor.TotalWidth := AFieldWidth
      else
        ARowField.CharWidth := AFieldWidth;
      Result := LFileEditor;
    except
      LFileEditor.Free;
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
begin
  if AViewField.DataType is TEFNumericDataTypeBase then
  begin
    LNumberField := TKExtFormNumberField.Create;
    try
      if not Assigned(ARowField) then
        LNumberField.Width := LNumberField.CharsToPixels(AFieldWidth)
      else
        ARowField.CharWidth := AFieldWidth;
      if not AIsReadOnly then
      begin
        LNumberField.AllowDecimals := AViewField.DataType is TEFDecimalNumericDataTypeBase;
        LNumberField.DecimalSeparator := Session.Config.UserFormatSettings.DecimalSeparator;
        LNumberField.AllowNegative := True;
        if LNumberField.AllowDecimals then
          LNumberField.DecimalPrecision := AViewField.DecimalPrecision;
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
  LFormField: TExtFormField;
  LRecordField: TKViewTableField;
begin
  Assert(Assigned(FDataRecord));

  LViewField := ViewTable.FieldByAliasedName(AFieldName);
  LRecordField := FDataRecord.FieldByName(AFieldName);

  // Store common properties.
  LFieldWidth := LViewField.DisplayWidth;
  if LFieldWidth = 0 then
    // Blobs have Size = 0.
    LFieldWidth := Min(IfThen(LViewField.Size = 0, FDefaults.MemoWidth, LViewField.Size), FDefaults.MaxFieldWidth);
  // Minimum cap - avoids too short combo boxes.
  LFieldWidth := Max(LFieldWidth, FDefaults.MinFieldWidth);

  LIsReadOnly := LViewField.IsReadOnly or not LViewField.IsAccessGranted(ACM_MODIFY)
    or not LViewField.CanUpdate or ViewTable.IsReadOnly or FForceReadOnly or (LViewField.Model <> LViewField.Table.Model);
  if not LIsReadOnly and LViewField.IsDetailReference then
    LIsReadOnly := True;

  LLabel := _(LViewField.DisplayLabel);
  if not LIsReadOnly and LViewField.IsRequired then
    LLabel := ReplaceText(FDefaults.RequiredLabelTemplate, '{label}', LLabel);

  if AContainer is TKExtFormRow then
  begin
    LRowField := TKExtFormRowField.Create;
    LRowField.SetRecordField(LRecordField);
  end
  else
    LRowField := nil;

  Result := TryCreateFileEditor(LViewField, LRowField, LFieldWidth, LIsReadOnly, LLabel);
  if Result = nil then
    Result := TryCreateComboBox(LRecordField, LRowField, LFieldWidth, LIsReadOnly);
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

  Result.SetRecordField(LRecordField);

  LFormField := Result.AsExtFormField;
  if Assigned(LFormField) then
  begin
    if LFormField is TExtFormTextField then
    begin
      if LViewField.Hint <> '' then
        TExtFormTextField(LFormField).EmptyText := _(LViewField.Hint);
    end;

    if not LIsReadOnly then
      LViewField.ApplyRules(
        procedure (ARuleImpl: TKRuleImpl)
        begin
          if ARuleImpl is TKExtRuleImpl then
            TKExtRuleImpl(ARuleImpl).ApplyToFormField(LFormField);
        end);

    LFormField.SubmitValue := not LIsReadOnly;

    if LIsReadOnly then
      LFormField.Cls := 'x-form-readonly';
    LFormField.Name := LViewField.AliasedName;
    LFormField.ReadOnly := LIsReadOnly;
    LFormField.FieldLabel := LLabel;
    LFormField.MsgTarget := LowerCase(FDefaults.MsgTarget);

    if DerivedFieldsExist(LViewField) then
      LFormField.OnChange := FOnFieldChange;

    if (FFocusField = nil) and not LFormField.ReadOnly and not LFormField.Disabled then
      FFocusField := LFormField;
  end;

  if Assigned(LRowField) then
    Result := LRowField.Encapsulate(Result);

  if Assigned(FOnNewEditor) then
    FOnNewEditor(Result);
end;

function TKExtLayoutProcessor.DerivedFieldsExist(const AViewField: TKViewField): Boolean;
var
  LViewTable: TKViewTable;
  LModelField: TKModelField;
  I: Integer;
begin
  Result := False;
  LViewTable := AViewField.Table;
  LModelField := AViewField.ModelField;
  for I := 0 to LViewTable.FieldCount - 1 do
  begin
    if LViewTable.Fields[I].ReferenceField = LModelField then
    begin
      Result := True;
      Exit;
    end;
  end;
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
  else if SameText(AName, 'RequiredLabelTemplate') then
    FDefaults.RequiredLabelTemplate := AValue
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
  RequiredLabelTemplate := '<b>{label}*</b>';
  MsgTarget := 'Qtip'; // qtip title under side
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

function TKExtFormTextField.GetRecordField: TKViewTableField;
begin
  Result := FRecordField;
end;

function TKExtFormTextField.QueryInterface(const IID: TGUID; out Obj): HRESULT;
begin
  if GetInterface(IID, Obj) then Result := 0 else Result := E_NOINTERFACE;
end;

procedure TKExtFormTextField.SetRecordField(const AValue: TKViewTableField);
begin
  FRecordField := AValue;
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

function TKExtFormTextArea.GetRecordField: TKViewTableField;
begin
  Result := FRecordField;
end;

function TKExtFormTextArea.QueryInterface(const IID: TGUID; out Obj): HRESULT;
begin
  if GetInterface(IID, Obj) then Result := 0 else Result := E_NOINTERFACE;
end;

procedure TKExtFormTextArea.SetRecordField(const AValue: TKViewTableField);
begin
  FRecordField := AValue;
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

function TKExtFormCheckbox.GetRecordField: TKViewTableField;
begin
  Result := FRecordField;
end;

function TKExtFormCheckbox.QueryInterface(const IID: TGUID; out Obj): HRESULT;
begin
  if GetInterface(IID, Obj) then Result := 0 else Result := E_NOINTERFACE;
end;

procedure TKExtFormCheckbox.SetRecordField(const AValue: TKViewTableField);
begin
  FRecordField := AValue;
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

function TKExtFormDateField.GetRecordField: TKViewTableField;
begin
  Result := FRecordField;
end;

function TKExtFormDateField.QueryInterface(const IID: TGUID; out Obj): HRESULT;
begin
  if GetInterface(IID, Obj) then Result := 0 else Result := E_NOINTERFACE;
end;

procedure TKExtFormDateField.SetRecordField(const AValue: TKViewTableField);
begin
  FRecordField := AValue;
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

function TKExtFormComboBoxEditor.GetRecordField: TKViewTableField;
begin
  Result := FRecordField;
end;

procedure TKExtFormComboBoxEditor.GetRecordPage;
var
  LStart: Integer;
  LLimit: Integer;
  LPageRecordCount: Integer;
begin
  Assert(Assigned(FServerStore));

  FServerStore.Load(Session.Config.DBConnections[GetRecordField.ViewField.Table.DatabaseName],
    ReplaceStr(FLookupCommandText, '{query}', ReplaceStr(Session.Query['query'], '''', '''''')));

  LStart := Session.QueryAsInteger['start'];
  LLimit := Session.QueryAsInteger['limit'];
  LPageRecordCount := Min(Max(LLimit, 100), FServerStore.RecordCount - LStart);

  Session.Response := '{Total:' + IntToStr(FServerStore.RecordCount) + ',Root:'
    + FServerStore.GetAsJSON(False, LStart, LPageRecordCount) + '}';
end;

procedure TKExtFormComboBoxEditor.InitDefaults;
begin
  inherited;
  TriggerAction := 'all';
  TypeAhead := True;
  LazyRender := True;
  SelectOnFocus := False;
end;

procedure TKExtFormComboBoxEditor.SetRecordField(const AValue: TKViewTableField);
begin
  FRecordField := AValue;
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

procedure TKExtFormComboBoxEditor.SetupServerStore(const AField: TKViewTableField;
  const ALookupCommandText: string);
var
  I: Integer;
begin
  Assert(Assigned(AField));
  Assert(AField.ViewField.IsReference);
  Assert(ALookupCommandText <> '');

  FLookupCommandText := ALookupCommandText;
  FreeAndNil(FServerStore);
  FServerStore := AField.ViewField.CreateReferenceStore;
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
  // This is a hack to show the current display value in the combo box
  // before dropping down the list and loading the data, which is good.
  // The BasicForm will set the value, the combo box will not find the
  // corresponding display value because the list is not loaded yet,
  // and it will render this value.
  ValueNotFoundText := AField.AsString;
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
  //AutoScroll := False;
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

function TKExtFormRowField.GetRecordField: TKViewTableField;
begin
  Result := FRecordField;
end;

procedure TKExtFormRowField.SetCharWidth(const AValue: Integer);
begin
  FCharWidth := AValue;
  Width := CharsToPixels(AValue);
end;

procedure TKExtFormRowField.SetRecordField(const AValue: TKViewTableField);
begin
  FRecordField := AValue;
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

function TKExtFormNumberField.GetRecordField: TKViewTableField;
begin
  Result := FRecordField;
end;

function TKExtFormNumberField.QueryInterface(const IID: TGUID; out Obj): HRESULT;
begin
  if GetInterface(IID, Obj) then Result := 0 else Result := E_NOINTERFACE;
end;

procedure TKExtFormNumberField.SetRecordField(const AValue: TKViewTableField);
begin
  FRecordField := AValue;
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

function TKExtFormDateTimeField.GetRecordField: TKViewTableField;
begin
  Result := FRecordField;
end;

function TKExtFormDateTimeField.JSClassName: string;
begin
  Result := 'Ext.ux.form.DateTimeField';
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

procedure TKExtFormDateTimeField.SetAllowBlank(const AValue: Boolean);
begin
  FAllowBlank := AValue;
  JSCode('allowBlank:' + VarToJSON([AValue]));
end;

procedure TKExtFormDateTimeField.SetAltDateFormats(const AValue: string);
begin
  FAltDateFormats := AValue;
  JSCode('altDateFormats:' + VarToJSON([AValue]));
end;

procedure TKExtFormDateTimeField.SetAltTimeFormats(const AValue: string);
begin
  FAltTimeFormats := AValue;
  JSCode('altTimeFormats:' + VarToJSON([AValue]));
end;

//procedure TKExtFormDateTimeField.SetDateConfig(const AValue: TExtObject);
//begin
//  FDateConfig := AValue;
//  AValue.DeleteFromGarbage;
//  JSCode('dateConfig:' + VarToJSON([AValue]));
//end;

procedure TKExtFormDateTimeField.SetDateFormat(const AValue: string);
begin
  FTimeFormat := AValue;
  JSCode('dateFormat:' + VarToJSON([AValue]));
end;

procedure TKExtFormDateTimeField.SetRecordField(const AValue: TKViewTableField);
begin
  FRecordField := AValue;
end;

//procedure TKExtFormDateTimeField.SetTimeConfig(const AValue: TExtObject);
//begin
//  FTimeConfig := AValue;
//  AValue.DeleteFromGarbage;
//  JSCode('timeConfig:' + VarToJSON([AValue]));
//end;

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

function TKExtFormTimeField.GetRecordField: TKViewTableField;
begin
  Result := FRecordField;
end;

function TKExtFormTimeField.QueryInterface(const IID: TGUID; out Obj): HRESULT;
begin
  if GetInterface(IID, Obj) then Result := 0 else Result := E_NOINTERFACE;
end;

procedure TKExtFormTimeField.SetRecordField(const AValue: TKViewTableField);
begin
  FRecordField := AValue;
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

{ TKExtFormFileUploadField }

function TKExtFormFileUploadField.AsExtFormField: TExtFormField;
begin
  Result := Self;
end;

function TKExtFormFileUploadField.AsExtObject: TExtObject;
begin
  Result := Self;
end;

function TKExtFormFileUploadField.AsObject: TObject;
begin
  Result := Self;
end;

function TKExtFormFileUploadField.GetRecordField: TKViewTableField;
begin
  Result := FRecordField;
end;

function TKExtFormFileUploadField.QueryInterface(const IID: TGUID; out Obj): HRESULT;
begin
  if GetInterface(IID, Obj) then Result := 0 else Result := E_NOINTERFACE;
end;

procedure TKExtFormFileUploadField.SetRecordField(const AValue: TKViewTableField);
begin
  FRecordField := AValue;
end;

procedure TKExtFormFileUploadField.SetOption(const AName, AValue: string);
begin
  if not SetExtFormFieldOption(AsExtFormField, AName, AValue) then
    InvalidOption(AName, AValue);
end;

function TKExtFormFileUploadField._AddRef: Integer;
begin
  Result := 0;
end;

function TKExtFormFileUploadField._Release: Integer;
begin
  Result := 0;
end;

{ TKExtFormFileEditor }

function TKExtFormFileEditor.AsExtFormField: TExtFormField;
begin
  Result := nil;
end;

function TKExtFormFileEditor.AsExtObject: TExtObject;
begin
  Result := Self;
end;

function TKExtFormFileEditor.AsObject: TObject;
begin
  Result := Self;
end;

procedure TKExtFormFileEditor.Clear;
begin
  ClearContents;
  UpdateGUI(True);
end;

procedure TKExtFormFileEditor.ClearContents;
begin
  FLastUploadedFullFileName := '';
  FLastUploadedOriginalFileName := '';
end;

procedure TKExtFormFileEditor.DownloadFieldData;
var
  LServerFileName: string;
begin
  inherited;
  LServerFileName := GetCurrentServerFileName;
  if LServerFileName <> '' then
    DownloadFile(LServerFileName, GetCurrentClientFileName);
end;

procedure TKExtFormFileEditor.DownloadThumbnailedStream(const AStream: TStream;
  const AFileName: string);
var
  LFileExt: string;
  LTempFileName: string;
  LStream: TFileStream;

  procedure WriteTempFile;
  var
    LFileStream: TFileStream;
  begin
    LFileStream := TFileStream.Create(LTempFileName, fmCreate);
    try
      AStream.Position := 0;
      LFileStream.CopyFrom(AStream, AStream.Size);
      AStream.Position := 0;
    finally
      FreeAndNil(LFileStream);
    end;
  end;

  procedure TransformTempFileToThumbnail(const AMaxWidth, AMaxHeight: Integer;
    const AImageClass: TGraphicClass);
  var
    LImage: TGraphic;
    LScale: Extended;
    LBitmap: TBitmap;
  begin
    LImage := AImageClass.Create;
    try
      LImage.LoadFromFile(LTempFileName);
      if (LImage.Height <= AMaxHeight) and (LImage.Width <= AMaxWidth) then
        Exit;
      if LImage.Height > LImage.Width then
        LScale := AMaxHeight / LImage.Height
      else
        LScale := AMaxWidth / LImage.Width;
      LBitmap := TBitmap.Create;
      try
        LBitmap.Width := Round(LImage.Width * LScale);
        LBitmap.Height := Round(LImage.Height * LScale);
        LBitmap.Canvas.StretchDraw(LBitmap.Canvas.ClipRect, LImage);

        LImage.Assign(LBitmap);
        LImage.SaveToFile(LTempFileName);
      finally
        LBitmap.Free;
      end;
    finally
      LImage.Free;
    end;
  end;

begin
  LFileExt := ExtractFileExt(AFileName);
  if FRecordField.ViewField.GetBoolean('IsPicture') and MatchText(LFileExt, ['.jpg', '.jpeg', '.png']) then
  begin
    LTempFileName := GetTempFileName(LFileExt);
    try
      WriteTempFile;
      if MatchText(LFileExt, ['.jpg', '.jpeg']) then
        TransformTempFileToThumbnail(FImageWidth, FImageHeight, TJPEGImage)
      else
        TransformTempFileToThumbnail(FImageWidth, FImageHeight, TPngImage);

      LStream := TFileStream.Create(LTempFileName, fmOpenRead + fmShareDenyWrite);
      try
        Session.DownloadStream(LStream, AFileName);
      finally
        FreeAndNil(LStream);
      end;
    finally
      if FileExists(LTempFileName) then
        DeleteFile(LTempFileName);
    end;
  end
  else
    Session.DownloadStream(AStream, AFileName);
end;

procedure TKExtFormFileEditor.FileUploaded(const AFileName: string);
begin
end;

function TKExtFormFileEditor.GetRecordField: TKViewTableField;
begin
  Result := FRecordField;
end;

function TKExtFormFileEditor.QueryInterface(const IID: TGUID; out Obj): HRESULT;
begin
  if GetInterface(IID, Obj) then Result := 0 else Result := E_NOINTERFACE;
end;

procedure TKExtFormFileEditor.PictureViewAfterRender(This: TExtComponent);
begin
  Assert(Assigned(FPictureView));

  FPictureView.Load(JSObject(Format('url:"%s"', [MethodURI(GetImageContent)])));
end;

procedure TKExtFormFileEditor.CreateGUI;
var
  LPanel: TExtPanel;
  LUploadButton: TExtButton;
  LToolbar: TExtToolbar;
  LButtonCount: Integer;
  LIsPicture: Boolean;
begin
  Layout := lyForm;

  LIsPicture := FRecordField.ViewField.GetBoolean('IsPicture');

  LPanel := TExtPanel.AddTo(Items);
  FImageWidth := FRecordField.ViewField.GetInteger('IsPicture/Thumbnail/Width', 100);
  FImageHeight := FRecordField.ViewField.GetInteger('IsPicture/Thumbnail/Height', 100);

  if LIsPicture then
  begin
    LPanel.Layout := lyColumn;
    FPictureView := TExtPanel.AddTo(LPanel.Items);
    FPictureView.Frame := True;
    FPictureView.OnAfterrender := PictureViewAfterRender;

    LToolbar := TExtToolbar.AddTo(LPanel.Items);
    // Version below puts the toolbar at the bottom (in which case we should adjust the height as well)
    //LToolbar := TExtToolbar.Create;
    //FPictureView.Bbar := LToolbar;
  end
  else
  begin
    LPanel.Layout := lyHbox;
    FDescriptionField := TExtFormTextField.AddTo(LPanel.Items);
    FDescriptionField.ReadOnly := True;
    FDescriptionField.Cls := 'x-form-readonly';

    LToolbar := TExtToolbar.AddTo(LPanel.Items);
  end;

  LToolbar.Style := 'background: none; border: none;';

  FDownloadButton := TExtButton.AddTo(LToolbar.Items);
  FDownloadButton.Tooltip := _('Download file');
  FDownloadButton.Icon := Session.Config.GetImageURL('download');
  FDownloadButton.Handler := Ajax(StartDownload);

  LButtonCount := 1;
  if not FIsReadOnly then
  begin
    //FDownloadButton.Margins := '0 5 0 0';
    LUploadButton := TExtButton.AddTo(LToolbar.Items);
    LUploadButton.Tooltip := _('Upload file');
    LUploadButton.Icon := Session.Config.GetImageURL('upload');
    LUploadButton.Handler := Ajax(ShowUploadFileDialog);
    //LUploadButton.Margins := '0 5 0 0';
    Inc(LButtonCount);

    FClearButton := TExtButton.AddTo(LToolbar.Items);
    FClearButton.Tooltip := _('Clear field');
    FClearButton.Icon := Session.Config.GetImageURL('clear');
    FClearButton.Handler := Ajax(Clear);
    Inc(LButtonCount);
  end
  else
    FClearButton := nil;

  if Assigned(FDescriptionField) then
    // Keep 3 characters per button, leave the rest to the text field.
    FDescriptionField.Width := CharsToPixels(FTotalWidth - (3 * LButtonCount))
  else if Assigned(FPictureView) then
  begin
    if FPictureView.Frame then
    begin
      FPictureView.Width := FImageWidth + 10;
      FPictureView.Height := FImageHeight + 10;
    end
    else
    begin
      FPictureView.Width := FImageWidth;
      FPictureView.Height := FImageHeight;
    end;
  end;
end;

procedure TKExtFormFileEditor.SetRecordField(const AValue: TKViewTableField);
begin
  Assert(Assigned(AValue));

  FRecordField := AValue;
  CreateGUI;
  UpdateGUI(False);
end;

procedure TKExtFormFileEditor.GetImageContent;
begin
  Session.ContentType := 'text/html';
  if GetCurrentServerFileName = '' then
    Session.Response := _('Empty')
  else
    // Add dummy paraneter to the URL to force the browser to refresh the image
    // after an upload.
    Session.Response := Format('<img src="%s&time=%s">', [MethodURI(GetImage),
      FormatDateTime('yyyymmddhhnnsszzz', Now())]);
end;

procedure TKExtFormFileEditor.UpdateGUI(const AUpdatePicture: Boolean);
var
  LIsEmpty: Boolean;
begin
  LIsEmpty := GetCurrentServerFileName = '';
  if Assigned(FDescriptionField) then
    FDescriptionField.Value := GetContentDescription;
  if AUpdatePicture and Assigned(FPictureView) then
    PictureViewAfterRender(FPictureView);
  FDownloadButton.SetDisabled(LIsEmpty);
  if Assigned(FClearButton) then
    FClearButton.SetDisabled(LIsEmpty);
end;

procedure TKExtFormFileEditor.SetOption(const AName, AValue: string);
begin
  if not SetExtFormFieldOption(AsExtFormField, AName, AValue) then
    InvalidOption(AName, AValue);
end;

procedure TKExtFormFileEditor.ShowUploadFileDialog;
var
  LUploadButton: TExtButton;
  LFormPanel: TExtFormFormPanel;
  LSubmitAction: TExtFormActionSubmit;
  LUploadFormField: TKExtFormFileUploadField;
begin
  FreeAndNil(FWindow);
  FWindow := TKExtModalWindow.Create;
  FWindow.Width := 400;
  FWindow.Height := 100;
  FWindow.Closable := True;
  FWindow.Title := _('File upload');

  LFormPanel := TExtFormFormPanel.AddTo(FWindow.Items);
  LFormPanel.Region := rgCenter;
  LFormPanel.Frame := True;
  LFormPanel.FileUpload := True;
  LUploadFormField := TKExtFormFileUploadField.AddTo(LFormPanel.Items);
  LUploadFormField.FieldLabel := _(FRecordField.ViewField.DisplayLabel);
  LUploadFormField.EmptyText := _('Select a file to upload');
  LUploadFormField.AllowBlank := False;
  LUploadFormField.Anchor := '0 5 0 0';
  LUploadButton := TExtButton.AddTo(LFormPanel.Buttons);
  LUploadButton.Text := _('Upload');

  LSubmitAction := TExtFormActionSubmit.Create;
  LSubmitAction.Url := MethodURI(Upload);
  LSubmitAction.WaitMsg := _('File upload in progress...');
  LSubmitAction.WaitTitle := _('Please wait...');
  LSubmitAction.Success := Ajax(PostUpload);
  LSubmitAction.Failure := ExtMessageBox.Alert(_('File upload error'), '%1.result.message');
  LUploadButton.Handler := TExtFormBasicForm(LFormPanel.GetForm).Submit(LSubmitAction);

  Session.MaxUploadSize := FRecordField.ViewField.GetInteger('MaxUploadSize', MaxLongint);
  FWindow.Show;
end;

procedure TKExtFormFileEditor.StartDownload;
begin
  Download(DownloadFieldData);
end;

procedure TKExtFormFileEditor.Upload;
var
  LFileName: string;
begin
  LFileName := Session.FileUploadedFullName;
  { TODO : Check the file against limitations such as type and size }
  if (LFileName <> '') and FileExists(LFileName) then
    FileUploaded(LFileName);
  { success:true or success:false + errors }
end;

function TKExtFormFileEditor.GetContentDescription: string;
var
  LFileName: string;
begin
  LFileName := GetCurrentServerFileName;
  if LFileName <> '' then
    Result := Format(_('%s file (%s)'),
      [StripPrefix(ExtractFileExt(LFileName), '.'),
      FormatByteSize(GetCurrentContentSize, Session.Config.UserFormatSettings)])
  else
    Result := _('Empty');
end;

procedure TKExtFormFileEditor.GetImage;
var
  LFileName: string;
begin
  inherited;
  LFileName := GetCurrentServerFileName;
  if LFileName <> '' then
    DownloadThumbnailedFile(LFileName,
      IfThen(FLastUploadedOriginalFileName <> '', FLastUploadedOriginalFileName, ExtractFileName(LFileName)));
end;

procedure TKExtFormFileEditor.PostUpload;
begin
  FWindow.Close;
  UpdateGUI(True);
end;

function TKExtFormFileEditor._AddRef: Integer;
begin
  Result := 0;
end;

function TKExtFormFileEditor._Release: Integer;
begin
  Result := 0;
end;

{ TKExtFormFileBlobEditor }

function TKExtFormFileBlobEditor.GetCurrentServerFileName: string;
var
  LCaptionField: TKViewTableField;
begin
  if FLastUploadedFullFileName <> '' then
    Result := FLastUploadedFullFileName
  else if FRecordField.IsNull then
    Result := ''
  else
  begin
    Result := FRecordField.ViewField.GetExpandedString('DefaultFileName');
    if Result = '' then
    begin
      LCaptionField := FRecordField.ParentRecord.FindField(FRecordField.ViewField.ModelField.Model.CaptionField.FieldName);
      if Assigned(LCaptionField) then
        Result := LCaptionField.AsString
      else
        Result := FRecordField.FieldName;
    end;
    Result := ChangeFileExt(Result, '.' + GetDataType(FRecordField.AsBytes, ExtractFileFormat(Result)));
  end;
end;

function TKExtFormFileBlobEditor.GetCurrentClientFileName: string;
begin
  Result := FLastUploadedOriginalFileName;
  if Result = '' then
    Result := ExtractFileName(GetCurrentServerFileName);
end;

procedure TKExtFormFileBlobEditor.ClearContents;
begin
  inherited;
  FRecordField.SetToNull;
end;

procedure TKExtFormFileBlobEditor.DownloadFile(const AServerFileName, AClientFileName: string);
var
  LStream: TStream;
begin
  inherited;
  if FileExists(AServerFileName) then
    LStream := TFileStream.Create(AServerFileName, fmOpenRead + fmShareDenyWrite)
  else if not FRecordField.IsNull then
    LStream := TBytesStream.Create(FRecordField.AsBytes);
  try
    Session.DownloadStream(LStream, AClientFileName);
  finally
    FreeAndNil(LStream);
  end;
end;

procedure TKExtFormFileBlobEditor.DownloadThumbnailedFile(
  const AServerFileName, AClientFileName: string);
var
  LStream: TStream;
begin
  inherited;
  if FileExists(AServerFileName) then
    LStream := TFileStream.Create(AServerFileName, fmOpenRead + fmShareDenyWrite)
  else if not FrecordField.IsNull then
    LStream := TBytesStream.Create(FRecordField.AsBytes);
  try
    DownloadThumbnailedStream(LStream, AClientFileName);
  finally
    FreeAndNil(LStream);
  end;
end;

procedure TKExtFormFileBlobEditor.FileUploaded(const AFileName: string);
begin
  inherited;
  FLastUploadedOriginalFileName := ExtractFileName(AFileName);
  FLastUploadedFullFileName := GetUniqueFileName(ExtractFilePath(AFileName),
    ExtractFileExt(AFileName));
  // Don't rename: move, since the files could be on different drives.
  CopyFile(AFileName, FLastUploadedFullFileName);
  DeleteFile(AFileName);
  Session.AddUploadedFile(TKExtUploadedFile.Create(
    Session.FileUploaded, FLastUploadedFullFileName, FRecordField.ViewField));
end;

function TKExtFormFileBlobEditor.GetCurrentContentSize: Integer;
begin
  if FLastUploadedFullFileName <> '' then
    Result := GetFileSize(FLastUploadedFullFileName)
  else
    Result := Length(FRecordField.AsBytes);
end;

{ TKExtFormFileReferenceEditor }

function TKExtFormFileReferenceEditor.GetFieldPath: string;
begin
  inherited;
  Result := FRecordField.ViewField.GetExpandedString('Path');
  if Result = '' then
    raise Exception.CreateFmt('Path not specified for file reference field %s.', [FRecordField.ViewField.FieldName]);
  if not DirectoryExists(Result) then
    raise Exception.CreateFmt('Directory %s not found field for file reference field %s.', [FRecordField.ViewField.FieldName]);
end;

procedure TKExtFormFileReferenceEditor.ClearContents;
begin
  inherited;
  if FRecordField.AsString <> '' then
  begin
    FRecordField.SetString('Sys/DeleteFile', IncludeTrailingPathDelimiter(GetFieldPath) + FRecordField.AsString);
    FRecordField.SetToNull;
  end;
end;

procedure TKExtFormFileReferenceEditor.DownloadFile(const AServerFileName, AClientFileName: string);
var
  LStream: TFileStream;
begin
  inherited;
  LStream := TFileStream.Create(AServerFileName, fmOpenRead + fmShareDenyWrite);
  try
    Session.DownloadStream(LStream, AClientFileName);
  finally
    FreeAndNil(LStream);
  end;
end;

procedure TKExtFormFileReferenceEditor.DownloadThumbnailedFile(
  const AServerFileName, AClientFileName: string);
var
  LStream: TFileStream;
begin
  inherited;
  LStream := TFileStream.Create(AServerFileName, fmOpenRead + fmShareDenyWrite);
  try
    DownloadThumbnailedStream(LStream, AClientFileName);
  finally
    FreeAndNil(LStream);
  end;
end;

procedure TKExtFormFileReferenceEditor.FileUploaded(const AFileName: string);
begin
  inherited;
  FLastUploadedFullFileName := GetUniqueFileName(GetFieldPath, ExtractFileExt(AFileName));
  { TODO : copy instead of renaming if paths are on different disks }
  RenameFile(AFileName, FLastUploadedFullFileName);

  Session.AddUploadedFile(TKExtUploadedFile.Create(ExtractFileName(FLastUploadedFullFileName),
    FLastUploadedFullFileName, FRecordField.ViewField, Session.FileUploaded));
  FRecordField.DeleteNode('Sys/DeleteFile');
end;

function TKExtFormFileReferenceEditor.GetCurrentClientFileName: string;
var
  LFileNameField: string;
  LCaptionField: TKViewTableField;
begin
  if FLastUploadedOriginalFileName <> '' then
    Result := FLastUploadedOriginalFileName
  else
  begin
    LFileNameField := FRecordField.ViewField.FileNameField;
    if LFileNameField <> '' then
      Result := FRecordField.ParentRecord.FieldByName(LFileNameField).AsString;
    if Result = '' then
    begin
      Result := FRecordField.ViewField.GetExpandedString('DefaultFileName');
      if Result = '' then
      begin
        LCaptionField := FRecordField.ParentRecord.FindField(FRecordField.ViewField.ModelField.Model.CaptionField.FieldName);
        if Assigned(LCaptionField) then
          Result := LCaptionField.AsString + ExtractFileExt(GetCurrentServerFileName)
        else
          Result := FRecordField.FieldName;
      end;
    end;
  end;
end;

function TKExtFormFileReferenceEditor.GetCurrentContentSize: Integer;
var
  LFileName: string;
begin
  LFileName := GetCurrentServerFileName;
  if FileExists(LFileName) then
    Result := GetFileSize(LFileName)
  else
    Result := 0;
end;

function TKExtFormFileReferenceEditor.GetCurrentServerFileName: string;
var
  LFileNameField: string;
begin
  if FLastUploadedFullFileName <> '' then
    Result := FLastUploadedFullFileName
  else
  begin
    LFileNameField := FRecordField.ViewField.FileNameField;
    if LFileNameField <> '' then
      Result := FRecordField.ParentRecord.FieldByName(LFileNameField).AsString;
    if Result = '' then
      Result := FRecordField.AsString;
    Result := IncludeTrailingPathDelimiter(GetFieldPath) + Result;
  end;
end;

end.
