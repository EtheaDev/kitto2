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
  Ext, ExtPascal, ExtForm, ExtGrid, ExtData, ExtUxForm,
  EF.Intf, EF.Classes, EF.Tree, EF.ObserverIntf,
  Kitto.Ext.Base, Kitto.Metadata.Views, Kitto.Metadata.DataView, Kitto.Store,
  Kitto.Ext.Session;

type
  IKExtEditItem = interface(IEFInterface)
    ['{4F5A1E4E-D5A1-44FE-93DC-E1ABF1209CE1}']
    procedure SetOption(const ANode: TEFNode);
    function AsExtObject: TExtObject;
    procedure RefreshValue;
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
    property RecordField: TKViewTableField read GetRecordField write SetRecordField;
  end;

  /// <summary>
  ///   Interface implemented by editors that need to be notified after
  ///   data is loaded. The combo box uses it to fix its predefined value,
  ///   for example.
  /// </summary>
  IKExtEditorAfterLoad = interface(IKExtEditor)
    ['{83C3D76B-A968-479A-AD67-3A7ADCFDB1F0}']
    procedure AfterLoad;
  end;

  TKExtEditPage = class;

  TKExtEditPanel = class(TExtFormFormPanel);

  TKExtEditPage = class(TExtPanel, IKExtEditItem, IKExtEditContainer)
  strict private
    FEditPanel: TKExtEditPanel;
    FDataRecord: TKViewTableRecord;
    FUnexpandedTitle: string;
    procedure SetUnexpandedTitle(const AValue: string);
  protected
    procedure InitDefaults; override;
  public
    function AsObject: TObject;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure AddChild(const AEditItem: IKExtEditItem);
    procedure SetOption(const ANode: TEFNode);
    function AsExtObject: TExtObject;
    procedure RefreshValue;

    property EditPanel: TKExtEditPanel read FEditPanel write FEditPanel;
    property DataRecord: TKViewTableRecord read FDataRecord write FDataRecord;
    property UnexpandedTitle: string read FUnexpandedTitle write SetUnexpandedTitle;
  end;

  TKExtFormFieldSet = class(TExtFormFieldSet, IKExtEditItem, IKExtEditContainer)
  strict private
    FDataRecord: TKViewTableRecord;
    FUnexpandedTitle: string;
  private
    procedure SetUnexpandedTitle(const AValue: string);
  protected
    procedure InitDefaults; override;
  public
    function AsObject: TObject;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure AddChild(const AEditItem: IKExtEditItem);
    procedure SetOption(const ANode: TEFNode);
    function AsExtObject: TExtObject;
    procedure RefreshValue;
    property DataRecord: TKViewTableRecord read FDataRecord write FDataRecord;
    property UnexpandedTitle: string read FUnexpandedTitle write SetUnexpandedTitle;
  end;

  TKExtFormCompositeField = class(TExtFormCompositeField, IKExtEditItem, IKExtEditContainer)
  public
    function AsObject: TObject; inline;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure AddChild(const AEditItem: IKExtEditItem);
    procedure SetOption(const ANode: TEFNode);
    function AsExtObject: TExtObject; inline;
    procedure RefreshValue;
  end;

  TKExtFormContainer = class(TExtContainer, IKExtEditItem)
  protected
    procedure InitDefaults; override;
    function InternalSetOption(const ANode: TEFNode): Boolean; virtual;
  public
    function AsObject: TObject; inline;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure AddChild(const AEditItem: IKExtEditItem);
    procedure SetOption(const ANode: TEFNode);
    function AsExtObject: TExtObject; inline;
    procedure RefreshValue;
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
    function InternalSetOption(const ANode: TEFNode): Boolean; override;
  public
    destructor Destroy; override;
  public
    function Encapsulate(const AValue: IKExtEditor): IKExtEditor;
    property CharWidth: Integer read FCharWidth write SetCharWidth;
    function AsExtFormField: TExtFormField;
    function GetRecordField: TKViewTableField;
    procedure SetRecordField(const AValue: TKViewTableField);
    procedure RefreshValue;
  end;

{ TODO : support the CheckboxGroup and Radiogroup containers? }

  TKExtFormNumberField = class(TExtFormNumberField, IKExtEditItem, IKExtEditor)
  private
    FRecordField: TKViewTableField;
    procedure FieldChange(This: TExtFormField; NewValue, OldValue: string);
  public
    function AsObject: TObject; inline;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure SetOption(const ANode: TEFNode);
    function AsExtObject: TExtObject; inline;
    function AsExtFormField: TExtFormField; inline;
    function GetRecordField: TKViewTableField;
    procedure SetRecordField(const AValue: TKViewTableField);
    procedure RefreshValue;
  end;

  TKExtFormTextField = class(TExtFormTextField, IKExtEditItem, IKExtEditor)
  private
    FRecordField: TKViewTableField;
    procedure FieldChange(This: TExtFormField; NewValue, OldValue: string);
  public
    function AsObject: TObject; inline;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure SetOption(const ANode: TEFNode);
    function AsExtObject: TExtObject; inline;
    function AsExtFormField: TExtFormField; inline;
    function GetRecordField: TKViewTableField;
    procedure SetRecordField(const AValue: TKViewTableField);
    procedure RefreshValue;
  end;

  TKExtFormTextArea = class(TExtFormTextArea, IKExtEditItem, IKExtEditor)
  private
    FRecordField: TKViewTableField;
    procedure FieldChange(This: TExtFormField; NewValue, OldValue: string);
  public
    function AsObject: TObject; inline;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure SetOption(const ANode: TEFNode);
    function AsExtObject: TExtObject; inline;
    function AsExtFormField: TExtFormField; inline;
    function GetRecordField: TKViewTableField;
    procedure SetRecordField(const AValue: TKViewTableField);
    procedure RefreshValue;
  end;

  TKExtFormCheckbox = class(TExtFormCheckbox, IKExtEditItem, IKExtEditor)
  private
    FRecordField: TKViewTableField;
    procedure FieldChange(This: TExtFormField; NewValue, OldValue: string);
  public
    function AsObject: TObject; inline;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure SetOption(const ANode: TEFNode);
    function AsExtObject: TExtObject; inline;
    function AsExtFormField: TExtFormField; inline;
    function GetRecordField: TKViewTableField;
    procedure SetRecordField(const AValue: TKViewTableField);
    procedure RefreshValue;
  end;

  TKExtFormDateField = class(TExtFormDateField, IKExtEditItem, IKExtEditor)
  private
    FRecordField: TKViewTableField;
    procedure FieldChange(This: TExtFormField; NewValue, OldValue: string);
  public
    function AsObject: TObject; inline;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure SetOption(const ANode: TEFNode);
    function AsExtObject: TExtObject; inline;
    function AsExtFormField: TExtFormField; inline;
    function GetRecordField: TKViewTableField;
    procedure SetRecordField(const AValue: TKViewTableField);
    procedure RefreshValue;
  end;

  TKExtFormTimeField = class(TExtFormTimeField, IKExtEditItem, IKExtEditor)
  private
    FRecordField: TKViewTableField;
    procedure FieldChange(This: TExtFormField; NewValue, OldValue: string);
  public
    function AsObject: TObject; inline;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure SetOption(const ANode: TEFNode);
    function AsExtObject: TExtObject; inline;
    function AsExtFormField: TExtFormField; inline;
    function GetRecordField: TKViewTableField;
    procedure SetRecordField(const AValue: TKViewTableField);
    procedure RefreshValue;
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
    procedure FieldChange(This: TExtFormField; NewValue, OldValue: string);
  public
    destructor Destroy; override;
    class function JSClassName: string; override;
  public
    function AsObject: TObject; inline;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure SetOption(const ANode: TEFNode);
    function AsExtObject: TExtObject; inline;
    function AsExtFormField: TExtFormField; inline;
    function GetRecordField: TKViewTableField;
    procedure SetRecordField(const AValue: TKViewTableField);
    procedure RefreshValue;

    property DateFormat: string read FDateFormat write SetDateFormat;
    //property DateConfig: TExtObject read FDateConfig write SetDateConfig;
    property TimeFormat: string read FTimeFormat write SetTimeFormat;
    //property TimeConfig: TExtObject read FTimeConfig write SetTimeConfig;
    property AltDateFormats: string read FAltDateFormats write SetAltDateFormats;
    property AltTimeFormats: string read FAltTimeFormats write SetAltTimeFormats;
    property AllowBlank: Boolean read FAllowBlank write SetAllowBlank;
  end;

  TKExtFormComboBoxEditor = class(TKExtFormComboBox, IKExtEditItem, IKExtEditor, IKExtEditorAfterLoad)
  private
    FServerStore: TKStore;
    FLookupCommandText: string;
    FRecordField: TKViewTableField;
    procedure SetupServerStore(const AViewField: TKViewField; const ALookupCommandText: string);
    procedure FieldChange(This: TExtFormField; NewValue, OldValue: string);
  protected
    procedure InitDefaults; override;
  public
    destructor Destroy; override;
  public
    procedure SetOption(const ANode: TEFNode);
    function AsExtObject: TExtObject; inline;
    function AsExtFormField: TExtFormField; inline;
    function GetRecordField: TKViewTableField;
    procedure SetRecordField(const AValue: TKViewTableField);
    procedure RefreshValue;
    procedure AfterLoad;
  published
    procedure GetRecordPage;
  end;

  TKExtFormFileUploadField = class(TExtUxFormFileUploadField, IKExtEditItem, IKExtEditor)
  private
    FRecordField: TKViewTableField;
  public
    function AsObject: TObject; inline;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure SetOption(const ANode: TEFNode);
    function AsExtObject: TExtObject; inline;
    function AsExtFormField: TExtFormField; inline;
    function GetRecordField: TKViewTableField;
    procedure SetRecordField(const AValue: TKViewTableField);
    procedure RefreshValue;
  end;

  TKExtFormFileEditor = class(TKExtPanelBase, IKExtEditItem, IKExtEditor)
  strict private
    FDescriptionField: TExtFormTextField;
    FWindow: TKExtModalWindow;
    FDownloadButton: TExtButton;
    FIsReadOnly: Boolean;
    FClearButton: TExtButton;
    FTotalCharWidth: Integer;
    FPictureView: TExtPanel;
    FImageWidth: Integer;
    FImageHeight: Integer;
    function GetContentDescription: string;
    procedure CreateGUI;
    procedure UpdateGUI(const AUpdatePicture: Boolean);
    procedure PictureViewAfterRender(This: TExtComponent);
    procedure SetTotalCharWidth(const AValue: Integer);
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
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure SetOption(const ANode: TEFNode);
    function AsExtObject: TExtObject; inline;
    function AsExtFormField: TExtFormField; inline;
    function GetRecordField: TKViewTableField;
    procedure SetRecordField(const AValue: TKViewTableField);
    procedure RefreshValue;
    property IsReadOnly: Boolean read FIsReadOnly write FIsReadOnly;
    property TotalCharWidth: Integer read FTotalCharWidth write SetTotalCharWidth;
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
    procedure FileUploaded(const AFileName: string); override;
    function GetCurrentClientFileName: string; override;
    function GetCurrentContentSize: Integer; override;
    function GetCurrentServerFileName: string; override;
  end;

  TKExtFormFileBlobEditor = class(TKExtFormFileEditor)
  strict protected
    procedure ClearContents; override;
    procedure DownloadFile(const AServerFileName, AClientFileName: string); override;
    procedure DownloadThumbnailedFile(const AServerFileName, AClientFileName: string); override;
    procedure FileUploaded(const AFileName: string); override;
    function GetCurrentClientFileName: string; override;
    function GetCurrentContentSize: Integer; override;
    function GetCurrentServerFileName: string; override;
  end;

  TKExtLayoutDefaults = record
    MemoWidth: Integer;
    MaxFieldWidth: Integer;
    MinFieldWidth: Integer;
    RequiredLabelTemplate: string;
    MsgTarget: string;
    procedure Init;
  end;

  type
    TKExtEditOperation = (eoUpdate, eoInsert);

  TKExtEditorManager = class;

  ///	<summary>
  ///	  Creates editors based on layouts. Can synthesize a default layout if
  ///	  missing.
  ///	</summary>
  TKExtLayoutProcessor = class
  strict private
    FDataRecord: TKViewTableRecord;
    FForceReadOnly: Boolean;
    FFormPanel: TKExtEditPanel;
    FMainEditPage: TKExtEditPage;
    FCurrentEditPage: TKExtEditPage;
    FFocusField: TExtFormField;
    FDefaults: TKExtLayoutDefaults;
    FCurrentEditItem: IKExtEditItem;
    FEditContainers: TStack<IKExtEditContainer>;
    FOnNewEditItem: TProc<IKExtEditItem>;
    FOperation: TKExtEditOperation;
    FTabPanel: TExtTabPanel;
    FEditorManager: TKExtEditorManager;
    function GetSession: TKExtSession;
    procedure SetMainEditPage(const AValue: TKExtEditPage);
    procedure FinalizeCurrentEditPage;
    function CreatePageBreak(const ATitle: string): IKExtEditItem;
    function GetViewTable: TKViewTable;
    function CreateEditItem(const ANode: TEFNode;
      const AContainer: IKExtEditContainer): IKExtEditItem;
    function CreateEditor(const AFieldName: string;
      const AContainer: IKExtEditContainer;
      const AOptions: TEFNode = nil): IKExtEditor;
    function CreateFieldSet(const ATitle: string): IKExtEditItem;
    function CreateCompositeField(const ALabel: string): IKExtEditItem;
    procedure SetGlobalOption(const ANode: TEFNode);
    procedure LayoutError(const AErrorMessage: string);
    function CreateRow: IKExtEditItem;
    procedure CreateEditorsFromLayout(const ALayout: TKLayout);
    procedure ProcessLayoutNode(const ANode: TEFNode);
    property Session: TKExtSession read GetSession;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
  private
    procedure SetOperation(const AValue: TKExtEditOperation);
  public
    // Set all properties before calling the CreateEditors methods.
    property DataRecord: TKViewTableRecord read FDataRecord write FDataRecord;
    property ViewTable: TKViewTable read GetViewTable;
    property ForceReadOnly: Boolean read FForceReadOnly write FForceReadOnly;
    property FormPanel: TKExtEditPanel read FFormPanel write FFormPanel;
    property TabPanel: TExtTabPanel read FTabPanel write FTabPanel;
    property MainEditPage: TKExtEditPage read FMainEditPage write SetMainEditPage;
    property OnNewEditItem: TProc<IKExtEditItem> read FOnNewEditItem write FOnNewEditItem;
    property Operation: TKExtEditOperation read FOperation write SetOperation;

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

  ///	<summary>
  ///	  Creates editors for edit forms and in-place editors for grids.
  ///	  Used by the layout processor; can be used directly.
  ///	</summary>

  TKExtEditorManager = class
  strict private
    FOnGetSession: TKExtSessionGetEvent;
    FOperation: TKExtEditOperation;
    const TRIGGER_WIDTH = 2;
    function TryCreateComboBox(const AOwner: TComponent; const AViewField: TKViewField;
      const ARowField: TKExtFormRowField; const AFieldCharWidth: Integer;
      const AIsReadOnly: Boolean): IKExtEditor;
    function TryCreateTextArea(const AOwner: TComponent; const AViewField: TKViewField;
      const ARowField: TKExtFormRowField; const AFieldCharWidth: Integer;
      const AIsReadOnly: Boolean): IKExtEditor;
    function TryCreateCheckBox(const AOwner: TComponent; const AViewField: TKViewField;
      const AIsReadOnly: Boolean): IKExtEditor;
    function TryCreateDateField(const AOwner: TComponent; const AViewField: TKViewField;
      const ARowField: TKExtFormRowField; const AFieldCharWidth: Integer;
      const AIsReadOnly: Boolean): IKExtEditor;
    function TryCreateTimeField(const AOwner: TComponent; const AViewField: TKViewField;
      const ARowField: TKExtFormRowField; const AFieldCharWidth: Integer;
      const AIsReadOnly: Boolean): IKExtEditor;
    function TryCreateDateTimeField(const AOwner: TComponent; const AViewField: TKViewField;
      const ARowField: TKExtFormRowField; const AFieldCharWidth: Integer;
      const AIsReadOnly: Boolean): IKExtEditor;
    function TryCreateNumberField(const AOwner: TComponent; const AViewField: TKViewField;
      const ARowField: TKExtFormRowField; const AFieldCharWidth: Integer;
      const AIsReadOnly: Boolean): IKExtEditor;
    function TryCreateFileEditor(const AOwner: TComponent; const AViewField: TKViewField;
      const ARowField: TKExtFormRowField; const AFieldCharWidth: Integer;
      const AIsReadOnly: Boolean; const ALabel: string): IKExtEditor;
    function CreateTextField(const AOwner: TComponent; const AViewField: TKViewField;
      const ARowField: TKExtFormRowField; const AFieldCharWidth: Integer;
      const AIsReadOnly: Boolean): IKExtEditor;
    function GetLookupCommandText(const AViewField: TKViewField): string;
    function GetSession: TKExtSession;
    property Session: TKExtSession read GetSession;
  public
    property Operation: TKExtEditOperation read FOperation write FOperation;
    property OnGetSession: TKExtSessionGetEvent read FOnGetSession write FOnGetSession;
    function CreateEditor(const AOwner: TComponent; const AViewField: TKViewField;
      const ARowField: TKExtFormRowField; const AFieldCharWidth: Integer;
      const AIsReadOnly: Boolean; const ALabel: string = ''): IKExtEditor;
    ///	<summary>
    ///	  Creates an in-place editor for the specified field.
    ///	</summary>
    function CreateGridCellEditor(const AOwner: TComponent;
      const AViewField: TKViewField): TExtFormField;
  end;

implementation

uses
  Types, Math, StrUtils, Windows, Graphics, jpeg, pngimage,
  EF.SysUtils, EF.StrUtils, EF.Localization, EF.YAML, EF.Types, EF.SQL, EF.JSON,
  EF.DB, EF.Macros,
  Kitto.SQL, Kitto.Metadata.Models, Kitto.Types, Kitto.AccessControl,
  Kitto.Rules, Kitto.Ext.Utils, Kitto.Ext.Rules, Kitto.Config;

const
  {
    String fields of this size or longer are represented by multiline edit
    controls.
  }
  MULTILINE_EDIT_THRESHOLD = 200;

procedure InvalidOption(const ANode: TEFNode);
begin
  raise EEFError.CreateFmt(_('Unknown or misplaced option %s: %s.'), [ANode.Name, ANode.AsString]);
end;

function OptionAsIntegerOrPerc(const ANode: TEFNode): string;
var
  LNumber: Integer;
begin
  Result := ANode.AsString;
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

function OptionAsLabelAlign(const ANode: TEFNode): TExtFormFormPanelLabelAlign;
begin
  if SameText(ANode.AsString, 'Left') then
    Result := laLeft
  else if SameText(ANode.AsString, 'Top') then
    Result := laTop
  else if SameText(ANode.AsString, 'Right') then
    Result := laRight
  else
    raise EEFError.CreateFmt(_('Invalid value %s. Valid values: "Left", "Top", "Right".'), [ANode.AsString]);
end;

function OptionAsString(const ANode: TEFNode; const AAllowedValues: array of string): string;

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
  if not MatchText(ANode.AsString, AAllowedValues) then
    raise EEFError.CreateFmt(_('Invalid value %s. Valid values: %s'), [ANode.AsString, FormatAllowedValues]);
  Result := ANode.AsString;
end;

function SetExtFormFieldOption(const AFormField: TExtFormField; const ANode: TEFNode): Boolean;
begin
  Result := True;
  if SameText(ANode.Name, 'Anchor') then
    AFormField.Anchor := ANode.AsString
  else if SameText(ANode.Name, 'CharWidth') then
    AFormField.Width := AFormField.CharsToPixels(ANode.AsInteger)
  else if SameText(ANode.Name, 'Width') then
    AFormField.WidthString := OptionAsIntegerOrPerc(ANode)
  else
    Result := False;
end;

function IsChangeHandlerNeeded(const AViewField: TKViewField): Boolean;
begin
  { TODO : Consider server-side field-level rules as well. }
  { TODO : Consider dependencies such as field names used in layout elements
    (such as field set titles). In order to do that, build a dependency list.
    Also, this function should be moved inside the layout processor. }
  if AViewField.FileNameField <> '' then
    Result := True
  else if AViewField.DerivedFieldsExist then
    Result := True
  else if AViewField.GetBoolean('NotifyChange') then // temporary
    Result := True
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
  Assert(Assigned(FCurrentEditPage));
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
  LIntf: IKExtEditContainer;

  procedure ProcessChildNodes;
  var
    I: Integer;
  begin
    for I := 0 to ANode.ChildCount - 1 do
      ProcessLayoutNode(ANode.Children[I]);
  end;

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
      FCurrentEditItem := CreateEditItem(ANode, FEditContainers.Peek)
    else
    begin
      FCurrentEditItem := CreateEditItem(ANode, nil);
      FCurrentEditPage.Items.Add(FCurrentEditItem.AsExtObject);
    end;
    if Supports(FCurrentEditItem, IKExtEditContainer, LIntf) then
      FEditContainers.Push(LIntf);
  end
  // Page breaks are not editors nor containers.
  else if SameText(ANode.Name, 'PageBreak') then
  begin
    if FEditContainers.Count > 0 then
      raise Exception.Create('PageBreak must be a top-level node in a layout.');
    CreatePageBreak(_(ANode.Value));
    ProcessChildNodes;
    Exit;
  end
  // Unknown name - must be an option.
  else if SameText(ANode.Name, 'DisplayLabel') then
    // DisplayLabel is handled earlier by CreateEditItem, so we just ignore it here.
    Exit
  else
  begin
    if ANode.AsString = '' then
      LayoutError(Format(_('Option %s must have a value.'), [ANode.Name]));
    if ANode.ChildCount > 0 then
      LayoutError(Format(_('Option node %s cannot have child nodes.'), [ANode.Name]));

    if Assigned(FCurrentEditItem) then
      FCurrentEditItem.SetOption(ANode)
    else
      SetGlobalOption(ANode)
  end;

  ProcessChildNodes;

  if Assigned(LIntf) then // Pushed, so pop it.
    FEditContainers.Pop;
end;

procedure TKExtLayoutProcessor.CreateEditors(const ALayout: TKLayout);
var
  I: Integer;
  LEditor: IKExtEditor;
begin
  Assert(Assigned(FCurrentEditPage));

  FFocusField := nil;

  if Assigned(ALayout) then
    CreateEditorsFromLayout(ALayout)
  else
  begin
    FFormPanel.LabelAlign := laLeft;
    for I := 0 to ViewTable.FieldCount - 1 do
    begin
      if ViewTable.IsFieldVisible(ViewTable.Fields[I]) and ViewTable.Fields[I].IsAccessGranted(ACM_READ) then
      begin
        LEditor := CreateEditor(ViewTable.Fields[I].AliasedName, nil);
        FCurrentEditPage.AddChild(LEditor);
        if Assigned(FOnNewEditItem) then
          FOnNewEditItem(LEditor);
      end;
    end;
  end;
  FinalizeCurrentEditPage;
end;

procedure TKExtLayoutProcessor.FinalizeCurrentEditPage;
begin
  Assert(Assigned(FCurrentEditPage));

  if Assigned(FFocusField) then
    FCurrentEditPage.On('afterrender', FCurrentEditPage.JSFunction(FFocusField.JSName + '.focus(false, 1000);'));
  FFocusField := nil;
end;

function TKExtLayoutProcessor.CreateEditItem(const ANode: TEFNode;
  const AContainer: IKExtEditContainer): IKExtEditItem;
begin
  if SameText(ANode.Name, 'Field') then
    Result := CreateEditor(ANode.Value, AContainer, ANode)
  else if SameText(ANode.Name, 'FieldSet') then
    Result := CreateFieldSet(_(ANode.Value))
  else if SameText(ANode.Name, 'CompositeField') then
    Result := CreateCompositeField(_(ANode.Value))
  else if SameText(ANode.Name, 'Row') then
    Result := CreateRow
  else
    raise EEFError.CreateFmt(_('Unknown edit item type %s.'), [ANode.Name]);
  if Assigned(AContainer) then
    AContainer.AddChild(Result);
  if Assigned(FOnNewEditItem) then
    FOnNewEditItem(Result);
end;

function TKExtLayoutProcessor.GetSession: TKExtSession;
begin
  Assert(Assigned(FCurrentEditPage));

  Result := FCurrentEditPage.Session;
end;

function TKExtLayoutProcessor.GetViewTable: TKViewTable;
begin
  Assert(Assigned(FDataRecord));

  Result := FDataRecord.ViewTable;
end;

function TKExtLayoutProcessor.CreateEditor(const AFieldName: string;
  const AContainer: IKExtEditContainer; const AOptions: TEFNode): IKExtEditor;
var
  LFieldCharWidth: Integer;
  LIsReadOnly: Boolean;
  LLabel: string;
  LViewField: TKViewField;
  LRowField: TKExtFormRowField;
  LFormField: TExtFormField;
  LRecordField: TKViewTableField;

  function CanEditField: Boolean;
  begin
    if FOperation = eoUpdate then
      Result := LViewField.CanUpdate
    else
      Result := LViewField.CanInsert
  end;

begin
  Assert(Assigned(FDataRecord));
  Assert(Assigned(FCurrentEditPage));

  LViewField := ViewTable.FieldByAliasedName(AFieldName);
  LRecordField := FDataRecord.FieldByName(LViewField.AliasedName);

  // Store common properties.
  LFieldCharWidth := LViewField.DisplayWidth;
  if LFieldCharWidth = 0 then
    // Blobs have Size = 0.
    LFieldCharWidth := Min(IfThen(LViewField.Size = 0, FDefaults.MemoWidth, LViewField.Size), FDefaults.MaxFieldWidth);
  // Minimum cap - avoids too short combo boxes.
  LFieldCharWidth := Max(LFieldCharWidth, FDefaults.MinFieldWidth);

  LIsReadOnly :=
    LViewField.IsReadOnly
    or not LViewField.IsAccessGranted(ACM_MODIFY)
    or not CanEditField
    or ViewTable.IsReadOnly
    or FForceReadOnly
    or (LViewField.Model <> LViewField.Table.Model);

  if not LIsReadOnly and LViewField.IsDetailReference then
    LIsReadOnly := True;

  LLabel := '';
  if Assigned(AOptions) then
    LLabel := _(AOptions.GetString('DisplayLabel'));
  if LLabel = '' then
    LLabel := _(LViewField.DisplayLabel);
  if not LIsReadOnly and LViewField.IsRequired then
    LLabel := ReplaceText(FDefaults.RequiredLabelTemplate, '{label}', LLabel);

  if AContainer is TKExtFormRow then
  begin
    LRowField := TKExtFormRowField.Create(FCurrentEditPage);
    LRowField.SetRecordField(LRecordField);
  end
  else
    LRowField := nil;

  Result := FEditorManager.CreateEditor(FCurrentEditPage,
    LViewField, LRowField, LFieldCharWidth, LIsReadOnly, LLabel);

  if Assigned(LRowField) then
    LRowField.Encapsulate(Result);

  Result.SetRecordField(LRecordField);

  LFormField := Result.AsExtFormField;
  if Assigned(LFormField) then
  begin
    LFormField.FieldLabel := LLabel;
    //LFormField.SubmitValue := not LIsReadOnly;
    LFormField.MsgTarget := LowerCase(FDefaults.MsgTarget);

    if (FFocusField = nil) and not LFormField.ReadOnly and not LFormField.Disabled then
      FFocusField := LFormField;
  end;

  if Assigned(LRowField) then
    Result := LRowField;
end;

function TKExtLayoutProcessor.CreateFieldSet(const ATitle: string): IKExtEditItem;
var
  LFieldSet: TKExtFormFieldSet;
begin
  Assert(Assigned(FCurrentEditPage));
  Assert(Assigned(FDataRecord));

  LFieldSet := TKExtFormFieldSet.Create(FCurrentEditPage);
  LFieldSet.Collapsible := False;
  LFieldSet.DataRecord := FDataRecord;
  LFieldSet.UnexpandedTitle := ATitle;

  Result := LFieldSet;
end;

function TKExtLayoutProcessor.CreatePageBreak(const ATitle: string): IKExtEditItem;
var
  LPageBreak: TKExtEditPage;
begin
  Assert(Assigned(FFormPanel));
  Assert(Assigned(FTabPanel));
  Assert(Assigned(FDataRecord));

  FinalizeCurrentEditPage;

  LPageBreak := TKExtEditPage.CreateAndAddTo(FTabPanel.Items);
  LPageBreak.EditPanel := FFormPanel;
  LPageBreak.DataRecord := FDataRecord;
  LPageBreak.UnexpandedTitle := ATitle;
  FCurrentEditPage := LPageBreak;

  Result := LPageBreak;
end;

procedure TKExtLayoutProcessor.AfterConstruction;
begin
  inherited;
  FDefaults.Init;
  FEditContainers := TStack<IKExtEditContainer>.Create;
  FEditorManager := TKExtEditorManager.Create;
  FEditorManager.OnGetSession :=
    procedure(out ASession: TKExtSession)
    begin
      ASession := Session;
    end;
end;

function TKExtLayoutProcessor.CreateCompositeField(const ALabel: string): IKExtEditItem;
var
  LCompositeField: TKExtFormCompositeField;
begin
  Assert(Assigned(FCurrentEditPage));

  LCompositeField := TKExtFormCompositeField.Create(FCurrentEditPage);
  if ALabel <> '' then
    LCompositeField.FieldLabel := ALabel;
  LCompositeField.Anchor := '-32';
  Result := LCompositeField;
end;

function TKExtLayoutProcessor.CreateRow: IKExtEditItem;
var
  LRow: TKExtFormRow;
begin
  Assert(Assigned(FCurrentEditPage));

  LRow := TKExtFormRow.Create(FCurrentEditPage);
  Result := LRow;
end;

destructor TKExtLayoutProcessor.Destroy;
begin
  FreeAndNil(FEditContainers);
  FreeAndNil(FEditorManager);
  inherited;
end;

procedure TKExtLayoutProcessor.SetGlobalOption(const ANode: TEFNode);
begin
  if SameText(ANode.Name, 'MemoWidth') then
    FDefaults.MemoWidth := ANode.AsInteger
  else if SameText(ANode.Name, 'MaxFieldWidth') then
    FDefaults.MaxFieldWidth := ANode.AsInteger
  else if SameText(ANode.Name, 'MinFieldWidth') then
    FDefaults.MinFieldWidth := ANode.AsInteger
  else if SameText(ANode.Name, 'RequiredLabelTemplate') then
    FDefaults.RequiredLabelTemplate := ANode.AsString
  else if SameText(ANode.Name, 'MsgTarget') then
    FDefaults.MsgTarget := OptionAsString(ANode, ['Qtip', 'Title', 'Under', 'Side'])
  else
    FCurrentEditPage.SetOption(ANode);
end;

procedure TKExtLayoutProcessor.SetMainEditPage(const AValue: TKExtEditPage);
begin
  FMainEditPage := AValue;
  if Assigned(FMainEditPage) and not Assigned(FCurrentEditPage) then
    FCurrentEditPage := FMainEditPage;
end;

procedure TKExtLayoutProcessor.SetOperation(const AValue: TKExtEditOperation);
begin
  FOperation := AValue;
  FEditorManager.Operation := FOperation;
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

{ TKExtEditPage }

procedure TKExtEditPage.AddChild(const AEditItem: IKExtEditItem);
begin
  Items.Add(AEditItem.AsExtObject);
end;

function TKExtEditPage.AsExtObject: TExtObject;
begin
  Result := Self;
end;

function TKExtEditPage.AsObject: TObject;
begin
  Result := Self;
end;

procedure TKExtEditPage.InitDefaults;
begin
  inherited;
  BodyStyle := 'background:none';
  Layout := lyForm;
  // Leave room for the scroll bar on the right.
  PaddingString := '5px 20px 5px 5px';
  AutoScroll := True;
end;

procedure TKExtEditPage.RefreshValue;
var
  LTitle: string;
begin
  Assert(Assigned(FDataRecord));

  LTitle := FDataRecord.ExpandFieldJSONValues(FUnexpandedTitle, True);
  if Title <> LTitle then
    Title := LTitle;
end;

procedure TKExtEditPage.SetOption(const ANode: TEFNode);
begin
  if SameText(ANode.Name, 'LabelWidth') then
    LabelWidth := ANode.AsInteger
  else if SameText(ANode.Name, 'LabelAlign') then
  begin
    Assert(Assigned(FEditPanel));
    FEditPanel.LabelAlign := OptionAsLabelAlign(ANode);
  end
  else if SameText(ANode.Name, 'LabelSeparator') then
    LabelSeparator := ANode.AsString
  else if SameText(ANode.Name, 'LabelPad') then
  begin
    Assert(Assigned(FEditPanel));
    FEditPanel.LabelPad := Anode.AsInteger;
  end
  else
    InvalidOption(ANode);
end;

procedure TKExtEditPage.SetUnexpandedTitle(const AValue: string);
begin
  FUnexpandedTitle := AValue;
  RefreshValue;
end;

function TKExtEditPage._AddRef: Integer;
begin
  Result := -1;
end;

function TKExtEditPage._Release: Integer;
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

procedure TKExtFormFieldSet.InitDefaults;
begin
  inherited;
//  On('beforecollapse', JSFunction(
//    'this.kPreviousHeight = this.getHeight();'
//  ), Self);
//
//  On('collapse', JSFunction(
//    'if ("kPreviousHeight" in this && this.getTopOwner() instanceof Ext.Window) ' +
//    '  this.getTopOwner().setClippedHeight(this.getTopOwner().getHeight() - this.kPreviousHeight + this.getHeight());'
//  ), Self);
//
//  On('beforeexpand', JSFunction(
//    'this.kPreviousHeight = this.getHeight();'
//  ), Self);
//
//  On('expand', JSFunction(
//    'if ("kPreviousHeight" in this && this.getTopOwner() instanceof Ext.Window) ' +
//    '  this.getTopOwner().setClippedHeight(this.getTopOwner().getHeight() - this.kPreviousHeight + this.getHeight());'
//  ), Self);
  On('expand', JSFunction('this.getTopOwner().doLayout();'), Self);
end;

procedure TKExtFormFieldSet.RefreshValue;
var
  LTitle: string;
begin
  Assert(Assigned(FDataRecord));

  LTitle := FDataRecord.ExpandFieldJSONValues(FUnexpandedTitle, True);
  if Title <> LTitle then
    Title := LTitle;
end;

procedure TKExtFormFieldSet.SetOption(const ANode: TEFNode);
begin
  if SameText(ANode.Name, 'LabelWidth') then
    LabelWidth := ANode.AsInteger
  else if SameText(ANode.Name, 'Collapsible') then
    Collapsible := ANode.AsBoolean
  else if SameText(ANode.Name, 'Collapsed') then
  begin
    // We need to defer expanding/collapsing the field set to give
    // compound contained editors (such as the file editors) a chance to
    // layout correctly.
    if ANode.AsBoolean then
      On('afterrender', JSFunction(JSName + '.collapse(true);'))
    else
      On('afterrender', JSFunction(JSName + '.expand(true);'))
  end
  else
    InvalidOption(ANode);
end;

procedure TKExtFormFieldSet.SetUnexpandedTitle(const AValue: string);
begin
  FUnexpandedTitle := AValue;
  RefreshValue;
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

procedure TKExtFormCompositeField.RefreshValue;
begin
end;

procedure TKExtFormCompositeField.SetOption(const ANode: TEFNode);
begin
  InvalidOption(ANode);
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

procedure TKExtFormTextField.RefreshValue;
begin
  SetValue(JSONNullToEmptyStr(FRecordField.GetAsJSONValue(False, False)));
end;

procedure TKExtFormTextField.SetRecordField(const AValue: TKViewTableField);
begin
  FRecordField := AValue;
  if IsChangeHandlerNeeded(FRecordField.ViewField) then
    OnChange := FieldChange;
end;

procedure TKExtFormTextField.FieldChange(This: TExtFormField; NewValue: string;
  OldValue: string);
begin
  FRecordField.SetAsJSONValue(NewValue, False, Session.Config.UserFormatSettings);
end;

procedure TKExtFormTextField.SetOption(const ANode: TEFNode);
begin
  if not SetExtFormFieldOption(AsExtFormField, ANode) then
    InvalidOption(ANode);
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

procedure TKExtFormTextArea.FieldChange(This: TExtFormField; NewValue,
  OldValue: string);
begin
  FRecordField.SetAsJSONValue(NewValue, False, Session.Config.UserFormatSettings);
end;

function TKExtFormTextArea.GetRecordField: TKViewTableField;
begin
  Result := FRecordField;
end;

procedure TKExtFormTextArea.RefreshValue;
begin
  SetValue(JSONNullToEmptyStr(FRecordField.GetAsJSONValue(False, False)));
end;

procedure TKExtFormTextArea.SetRecordField(const AValue: TKViewTableField);
begin
  FRecordField := AValue;
  if IsChangeHandlerNeeded(FRecordField.ViewField) then
    OnChange := FieldChange;
end;

procedure TKExtFormTextArea.SetOption(const ANode: TEFNode);
begin
  if not SetExtFormFieldOption(AsExtFormField, ANode) then
  begin
    if SameText(ANode.Name, 'Lines') then
      Height := LinesToPixels(ANode.AsInteger)
    else
      InvalidOption(ANode);
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

procedure TKExtFormCheckbox.FieldChange(This: TExtFormField; NewValue,
  OldValue: string);
begin
  FRecordField.SetAsJSONValue(NewValue, False, Session.Config.UserFormatSettings);
end;

function TKExtFormCheckbox.GetRecordField: TKViewTableField;
begin
  Result := FRecordField;
end;

procedure TKExtFormCheckbox.RefreshValue;
begin
  SetValue(JSONNullToEmptyStr(FRecordField.GetAsJSONValue(False, False)));
end;

procedure TKExtFormCheckbox.SetRecordField(const AValue: TKViewTableField);
begin
  FRecordField := AValue;
  if IsChangeHandlerNeeded(FRecordField.ViewField) then
    OnChange := FieldChange;
end;

procedure TKExtFormCheckbox.SetOption(const ANode: TEFNode);
begin
  if not SetExtFormFieldOption(AsExtFormField, ANode) then
    InvalidOption(ANode);
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

procedure TKExtFormDateField.FieldChange(This: TExtFormField; NewValue,
  OldValue: string);
begin
  FRecordField.SetAsJSONValue(NewValue, False, Session.Config.UserFormatSettings);
end;

function TKExtFormDateField.GetRecordField: TKViewTableField;
begin
  Result := FRecordField;
end;

procedure TKExtFormDateField.RefreshValue;
begin
  SetValue(JSONNullToEmptyStr(FRecordField.GetAsJSONValue(False, False)));
end;

procedure TKExtFormDateField.SetRecordField(const AValue: TKViewTableField);
begin
  FRecordField := AValue;
  if IsChangeHandlerNeeded(FRecordField.ViewField) then
    OnChange := FieldChange;
end;

procedure TKExtFormDateField.SetOption(const ANode: TEFNode);
begin
  if not SetExtFormFieldOption(AsExtFormField, ANode) then
    InvalidOption(ANode);
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

procedure TKExtFormComboBoxEditor.AfterLoad;
begin
  SetRawValue(JSONNullToEmptyStr(FRecordField.GetAsJSONValue(False, False)));
end;

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
  LQuery: string;
  LDBConnection: TEFDBConnection;
begin
  Assert(Assigned(FServerStore));

  LQuery := ReplaceStr(Session.Query['query'], '''', '''''');
  LDBConnection := Session.Config.DBConnections[GetRecordField.ViewField.Table.DatabaseName];

  FServerStore.Load(LDBConnection, ReplaceStr(FLookupCommandText, '{query}', LQuery));

  LStart := Session.QueryAsInteger['start'];
  LLimit := Session.QueryAsInteger['limit'];
  LPageRecordCount := Min(LLimit, FServerStore.RecordCount - LStart);

  ExtSession.ResponseItems.AddJSON('{Total: ' + IntToStr(FServerStore.RecordCount)
    + ', Root: ' + FServerStore.GetAsJSON(False, LStart, LPageRecordCount) + '}');
end;

procedure TKExtFormComboBoxEditor.InitDefaults;
begin
  inherited;
  TriggerAction := 'all';
  TypeAhead := True;
  LazyRender := True;
  SelectOnFocus := False;
end;

procedure TKExtFormComboBoxEditor.RefreshValue;
begin
  SetValue(JSONNullToEmptyStr(FRecordField.GetAsJSONValue(False, False)));
//  if Mode = 'remote' then
//    AfterLoad;
end;

procedure TKExtFormComboBoxEditor.SetRecordField(const AValue: TKViewTableField);
begin
  FRecordField := AValue;
  if IsChangeHandlerNeeded(FRecordField.ViewField) then
    OnChange := FieldChange;
end;

procedure TKExtFormComboBoxEditor.FieldChange(This: TExtFormField; NewValue: string;
  OldValue: string);
begin
  FRecordField.SetAsJSONValue(NewValue, False, Session.Config.UserFormatSettings);
end;

procedure TKExtFormComboBoxEditor.SetOption(const ANode: TEFNode);
begin
  if not SetExtFormFieldOption(AsExtFormField, ANode) then
  begin
    if SameText(ANode.Name, 'Resizable') then
      Resizable := ANode.AsBoolean
    else
      InvalidOption(ANode);
  end;
end;

procedure TKExtFormComboBoxEditor.SetupServerStore(const AViewField: TKViewField; const ALookupCommandText: string);
var
  I: Integer;
begin
  Assert(Assigned(AViewField));
  Assert(AViewField.IsReference);
  Assert(ALookupCommandText <> '');

  FLookupCommandText := ALookupCommandText;
  FreeAndNil(FServerStore);
  FServerStore := AViewField.CreateReferenceStore;
  Store := TExtDataStore.Create(Self);
  Store.Url := MethodURI(GetRecordPage);
  Store.Reader := TExtDataJsonReader.Create(Self, JSObject('')); // Must pass '' otherwise invalid code is generated.
  TExtDataJsonReader(Store.Reader).Root := 'Root';
  TExtDataJsonReader(Store.Reader).TotalProperty := 'Total';
  for I := 0 to FServerStore.Header.FieldCount - 1 do
    with TExtDataField.CreateAndAddTo(Store.Reader.Fields) do
      Name := FServerStore.Header.Fields[I].FieldName;
  ValueField := FServerStore.Header.Fields[0].FieldName;
  DisplayField := AViewField.ModelField.ReferencedModel.CaptionField.FieldName;
  { TODO : make these configurable. }
  MinChars := 4;
  PageSize := 100;
  MinListWidth := 250; // Enough to accomodate all buttons.
  Resizable := True;
  MinHeight := LinesToPixels(5);
  Mode := 'remote';
  TypeAhead := True;
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

function TKExtFormContainer.InternalSetOption(const ANode: TEFNode): Boolean;
begin
  Result := True;
  if SameText(ANode.Name, 'Layout') then
    LayoutString := ANode.AsString
  else if SameText(ANode.Name, 'ColumnWidth') then
    ColumnWidth := ANode.AsFloat
  else if SameText(ANode.Name, 'CharWidth') then
    Width := CharsToPixels(ANode.AsInteger)
  else if SameText(ANode.Name, 'Width') then
    WidthString := OptionAsIntegerOrPerc(ANode)
  else if SameText(ANode.Name, 'Anchor') then
    Anchor := ANode.AsString
  else
    Result := False;
end;

procedure TKExtFormContainer.RefreshValue;
begin
end;

procedure TKExtFormContainer.SetOption(const ANode: TEFNode);
begin
  if not InternalSetOption(ANode) then
    InvalidOption(ANode);
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

function TKExtFormRowField.InternalSetOption(const ANode: TEFNode): Boolean;
begin
  // Widths are set for both the container and the contained editor.
  if SameText(ANode.Name, 'ColumnWidth') then
  begin
    ColumnWidth := ANode.AsFloat;
    FEditor.SetOption(ANode);
  end
  else if SameText(ANode.Name, 'CharWidth') then
    CharWidth := ANode.AsInteger
  else if SameText(ANode.Name, 'Width') then
    WidthString := OptionAsIntegerOrPerc(ANode)
  else
    FEditor.SetOption(ANode);
  Result := True;
end;

procedure TKExtFormRowField.RefreshValue;
begin
  Assert(Assigned(FEditor));

  FEditor.RefreshValue;
end;

destructor TKExtFormRowField.Destroy;
begin
  NilEFIntf(FEditor);
  inherited;
end;

function TKExtFormRowField.Encapsulate(const AValue: IKExtEditor): IKExtEditor;
var
  LNode: TEFNode;
begin
  Assert(Assigned(AValue));

  FEditor := AValue;
  Items.Add(FEditor.AsExtObject);
  LNode := TEFNode.Create('Anchor');
  try
    LNode.AsInteger := -5;
    FEditor.SetOption(LNode);
  finally
    FreeAndNil(LNode);
  end;

  Result := Self;
end;

function TKExtFormRowField.GetRecordField: TKViewTableField;
begin
  Result := FRecordField;
end;

procedure TKExtFormRowField.SetCharWidth(const AValue: Integer);
begin
  FCharWidth := AValue;
  Width := CharsToPixels(AValue, 5);
end;

procedure TKExtFormRowField.SetRecordField(const AValue: TKViewTableField);
begin
  FRecordField := AValue;
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

procedure TKExtFormNumberField.FieldChange(This: TExtFormField; NewValue,
  OldValue: string);
begin
  FRecordField.SetAsJSONValue(NewValue, False, Session.Config.UserFormatSettings);
end;

function TKExtFormNumberField.GetRecordField: TKViewTableField;
begin
  Result := FRecordField;
end;

procedure TKExtFormNumberField.RefreshValue;
begin
  SetValue(JSONNullToEmptyStr(FRecordField.GetAsJSONValue(False, False)));
end;

procedure TKExtFormNumberField.SetRecordField(const AValue: TKViewTableField);
begin
  FRecordField := AValue;
  if IsChangeHandlerNeeded(FRecordField.ViewField) then
    OnChange := FieldChange;
end;

procedure TKExtFormNumberField.SetOption(const ANode: TEFNode);
begin
  if not SetExtFormFieldOption(AsExtFormField, ANode) then
    InvalidOption(ANode);
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

procedure TKExtFormDateTimeField.FieldChange(This: TExtFormField; NewValue,
  OldValue: string);
begin
  FRecordField.SetAsJSONValue(NewValue, False, Session.Config.UserFormatSettings);
end;

function TKExtFormDateTimeField.GetRecordField: TKViewTableField;
begin
  Result := FRecordField;
end;

class function TKExtFormDateTimeField.JSClassName: string;
begin
  Result := 'Ext.ux.form.DateTimeField';
end;

procedure TKExtFormDateTimeField.RefreshValue;
begin
  SetValue(JSONNullToEmptyStr(FRecordField.GetAsJSONValue(False, False)));
end;

procedure TKExtFormDateTimeField.SetOption(const ANode: TEFNode);
begin
  if not SetExtFormFieldOption(AsExtFormField, ANode) then
    InvalidOption(ANode);
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
  ExtSession.ResponseItems.SetConfigItem(Self, 'allowBlank', [AValue]);
end;

procedure TKExtFormDateTimeField.SetAltDateFormats(const AValue: string);
begin
  FAltDateFormats := AValue;
  ExtSession.ResponseItems.SetConfigItem(Self, 'altDateFormats', [AValue]);
end;

procedure TKExtFormDateTimeField.SetAltTimeFormats(const AValue: string);
begin
  FAltTimeFormats := AValue;
  ExtSession.ResponseItems.SetConfigItem(Self, 'altTimeFormats', [AValue]);
end;

procedure TKExtFormDateTimeField.SetDateFormat(const AValue: string);
begin
  FTimeFormat := AValue;
  ExtSession.ResponseItems.SetConfigItem(Self, 'dateFormat', [AValue]);
end;

procedure TKExtFormDateTimeField.SetRecordField(const AValue: TKViewTableField);
begin
  FRecordField := AValue;
  if IsChangeHandlerNeeded(FRecordField.ViewField) then
    OnChange := FieldChange;
end;

procedure TKExtFormDateTimeField.SetTimeFormat(const AValue: string);
begin
  FTimeFormat := AValue;
  ExtSession.ResponseItems.SetConfigItem(Self, 'timeFormat', [AValue]);
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

procedure TKExtFormTimeField.FieldChange(This: TExtFormField; NewValue,
  OldValue: string);
begin
  FRecordField.SetAsJSONValue(NewValue, False, Session.Config.UserFormatSettings);
end;

function TKExtFormTimeField.GetRecordField: TKViewTableField;
begin
  Result := FRecordField;
end;

procedure TKExtFormTimeField.RefreshValue;
begin
  SetValue(JSONNullToEmptyStr(FRecordField.GetAsJSONValue(False, False)));
end;

procedure TKExtFormTimeField.SetRecordField(const AValue: TKViewTableField);
begin
  FRecordField := AValue;
  if IsChangeHandlerNeeded(FRecordField.ViewField) then
    OnChange := FieldChange;
end;

procedure TKExtFormTimeField.SetOption(const ANode: TEFNode);
begin
  if not SetExtFormFieldOption(AsExtFormField, ANode) then
    InvalidOption(ANode);
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

procedure TKExtFormFileUploadField.RefreshValue;
begin
  SetValue(JSONNullToEmptyStr(FRecordField.GetAsJSONValue(False, False)));
end;

procedure TKExtFormFileUploadField.SetRecordField(const AValue: TKViewTableField);
begin
  FRecordField := AValue;
end;

procedure TKExtFormFileUploadField.SetOption(const ANode: TEFNode);
begin
  if not SetExtFormFieldOption(AsExtFormField, ANode) then
    InvalidOption(ANode);
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
var
  LFileNameField: string;
  LFileNameFieldReference: TKViewTableField;
begin
  FLastUploadedFullFileName := '';
  FLastUploadedOriginalFileName := '';

  LFileNameField := FRecordField.ViewField.FileNameField;
  if LFileNameField <> ''then
  begin
    LFileNameFieldReference := FRecordField.ParentRecord.FieldByName(LFileNameField);
    // Must clear the field both now, to have an exact picture in real time, and later
    // (through the SetToNull directive), when the form is submitted.
    LFileNameFieldReference.SetBoolean('Sys/SetToNull', True);
    LFileNameFieldReference.SetToNull;
  end;
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
var
  LFileNameField: string;
  LFileNameFieldReference: TKViewTableField;
begin
  LFileNameField := FRecordField.ViewField.FileNameField;
  if LFileNameField <> ''then
  begin
    LFileNameFieldReference := FRecordField.ParentRecord.FieldByName(LFileNameField);
    LFileNameFieldReference.DeleteNode('Sys/SetToNull');
    LFileNameFieldReference.AsString := Session.FileUploaded;
  end;
end;

function TKExtFormFileEditor.GetRecordField: TKViewTableField;
begin
  Result := FRecordField;
end;

procedure TKExtFormFileEditor.RefreshValue;
begin
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

  LPanel := TExtPanel.CreateAndAddTo(Items);
  FImageWidth := FRecordField.ViewField.GetInteger('IsPicture/Thumbnail/Width', 100);
  FImageHeight := FRecordField.ViewField.GetInteger('IsPicture/Thumbnail/Height', 100);

  if LIsPicture then
  begin
    LPanel.Layout := lyColumn;
    FPictureView := TExtPanel.CreateAndAddTo(LPanel.Items);
    FPictureView.Frame := True;
    FPictureView.OnAfterrender := PictureViewAfterRender;

    LToolbar := TExtToolbar.CreateAndAddTo(LPanel.Items);
    // Version below puts the toolbar at the bottom (in which case we should adjust the height as well)
    //LToolbar := TExtToolbar.Create;
    //FPictureView.Bbar := LToolbar;
  end
  else
  begin
    LPanel.Layout := lyHbox;
    FDescriptionField := TExtFormTextField.CreateAndAddTo(LPanel.Items);
    FDescriptionField.ReadOnly := True;
    FDescriptionField.Cls := 'x-form-readonly';

    LToolbar := TExtToolbar.CreateAndAddTo(LPanel.Items);
  end;

  LToolbar.Style := 'background: none; border: none;';

  FDownloadButton := TExtButton.CreateAndAddTo(LToolbar.Items);
  FDownloadButton.Tooltip := _('Download file');
  FDownloadButton.Icon := Session.Config.GetImageURL('download');
  FDownloadButton.Handler := Ajax(StartDownload);

  LButtonCount := 1;
  if not FIsReadOnly then
  begin
    LUploadButton := TExtButton.CreateAndAddTo(LToolbar.Items);
    LUploadButton.Tooltip := _('Upload file');
    LUploadButton.Icon := Session.Config.GetImageURL('upload');
    LUploadButton.Handler := Ajax(ShowUploadFileDialog);
    Inc(LButtonCount);

    FClearButton := TExtButton.CreateAndAddTo(LToolbar.Items);
    FClearButton.Tooltip := _('Clear field');
    FClearButton.Icon := Session.Config.GetImageURL('clear');
    FClearButton.Handler := Ajax(Clear);
    Inc(LButtonCount);
  end
  else
    FClearButton := nil;

  if Assigned(FDescriptionField) then
    // Keep 3 characters per button, leave the rest to the text field.
    FDescriptionField.Width := CharsToPixels(FTotalCharWidth - (3 * LButtonCount))
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
    Width := FPictureView.Width + (22 * LButtonCount);
  end;
end;

procedure TKExtFormFileEditor.SetRecordField(const AValue: TKViewTableField);
begin
  Assert(Assigned(AValue));

  FRecordField := AValue;
  CreateGUI;
  UpdateGUI(False);
end;

procedure TKExtFormFileEditor.SetTotalCharWidth(const AValue: Integer);
begin
  FTotalCharWidth := AValue;
end;

procedure TKExtFormFileEditor.GetImageContent;
begin
  if GetCurrentServerFileName = '' then
    ExtSession.ResponseItems.AddHTML('<p>' + _('Empty') + '</p>')
  else
    // Add dummy paraneter to the URL to force the browser to refresh the image
    // after an upload.
    ExtSession.ResponseItems.AddHTML(Format('<img src="%s&time=%s">', [MethodURI(GetImage),
      FormatDateTime('yyyymmddhhnnsszzz', Now())]));
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

procedure TKExtFormFileEditor.SetOption(const ANode: TEFNode);
begin
  if SameText(ANode.Name, 'CharWidth') then
    TotalCharWidth := Anode.AsInteger
  else if SameText(ANode.Name, 'Anchor') then
    Anchor := ANode.AsString
  else
    InvalidOption(ANode);
end;

procedure TKExtFormFileEditor.ShowUploadFileDialog;
var
  LUploadButton: TExtButton;
  LFormPanel: TExtFormFormPanel;
  LSubmitAction: TExtFormActionSubmit;
  LUploadFormField: TKExtFormFileUploadField;
begin
  FreeAndNil(FWindow);
  FWindow := TKExtModalWindow.Create(Self);
  FWindow.Width := 400;
  FWindow.Height := 100;
  FWindow.Closable := True;
  FWindow.Title := _('File upload');

  LFormPanel := TExtFormFormPanel.CreateAndAddTo(FWindow.Items);
  LFormPanel.Region := rgCenter;
  LFormPanel.Frame := True;
  LFormPanel.FileUpload := True;
  LUploadFormField := TKExtFormFileUploadField.CreateAndAddTo(LFormPanel.Items);
  LUploadFormField.FieldLabel := _(FRecordField.ViewField.DisplayLabel);
  LUploadFormField.EmptyText := _('Select a file to upload');
  LUploadFormField.AllowBlank := False;
  LUploadFormField.Anchor := '0 5 0 0';
  LUploadButton := TExtButton.CreateAndAddTo(LFormPanel.Buttons);
  LUploadButton.Text := _('Upload');

  LSubmitAction := TExtFormActionSubmit.Create(FWindow);
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
  { TODO : Check the file against limitations such as type and size}
  if (LFileName <> '') and FileExists(LFileName) then
    FileUploaded(LFileName);
  { success:true or success:false + errors }
end;

function TKExtFormFileEditor.GetContentDescription: string;
var
  LFileName: string;
begin
  LFileName := ExtractFileName(GetCurrentServerFileName);
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
  LFileNameField: string;
begin
  if FLastUploadedFullFileName <> '' then
    Result := FLastUploadedFullFileName
  else if FRecordField.IsNull then
    Result := ''
  else
  begin
    LFileNameField := FRecordField.ViewField.FileNameField;
    if LFileNameField <> '' then
      Result := FRecordField.ParentRecord.FieldByName(LFileNameField).AsString;
    if Result = '' then
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
var
  LUploadedFile: TKExtUploadedFile;
begin
  inherited;
  FRecordField.SetToNull;
  LUploadedFile := Session.FindUploadedFile(FRecordField.ViewField);
  while Assigned(LUploadedFile) do
  begin
    Session.RemoveUploadedFile(LUploadedFile);
    LUploadedFile := Session.FindUploadedFile(FRecordField.ViewField);
  end;
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
var
  LUploadedFile: TKExtUploadedFile;
begin
  inherited;
  FLastUploadedOriginalFileName := ExtractFileName(AFileName);
  FLastUploadedFullFileName := GetUniqueFileName(ExtractFilePath(AFileName),
    ExtractFileExt(AFileName));
  // Don't rename: move, since the files could be on different drives.
  CopyFile(AFileName, FLastUploadedFullFileName);
  DeleteFile(AFileName);
  LUploadedFile := TKExtUploadedFile.Create(
    Session.FileUploaded, FLastUploadedFullFileName, FRecordField.ViewField,
    Session.FileUploaded);
  Session.AddUploadedFile(LUploadedFile);
  FRecordField.AsBytes := LUploadedFile.Bytes;
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
  Result := IncludeTrailingPathDelimiter(FRecordField.ViewField.GetExpandedString('Path'));
  if Result = '' then
    raise Exception.CreateFmt('Path not specified for file reference field %s.', [FRecordField.ViewField.FieldName]);
  if not DirectoryExists(Result) then
    raise Exception.CreateFmt('Directory %s not found for file reference field %s.', [Result, FRecordField.ViewField.FieldName]);
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
var
  LFileName: string;
begin
  inherited;
  FLastUploadedFullFileName := GetUniqueFileName(GetFieldPath, ExtractFileExt(AFileName));
  // Don't rename: move, since the files could be on different drives.
  CopyFile(AFileName, FLastUploadedFullFileName);
  DeleteFile(AFileName);

  LFileName := ExtractFileName(FLastUploadedFullFileName);
  Session.AddUploadedFile(TKExtUploadedFile.Create(LFileName,
    FLastUploadedFullFileName, FRecordField.ViewField, Session.FileUploaded));
  FRecordField.AsString := LFileName;
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
begin
  if FLastUploadedFullFileName <> '' then
    Result := FLastUploadedFullFileName
  else
    Result := IncludeTrailingPathDelimiter(GetFieldPath) + FRecordField.AsString;
end;

{ TKExtEditorManager }

function TKExtEditorManager.CreateGridCellEditor(const AOwner: TComponent;
  const AViewField: TKViewField): TExtFormField;
begin
  Result := CreateEditor(AOwner, AViewField, nil, AViewField.DisplayWidth, False).AsExtFormField;
end;

function TKExtEditorManager.CreateEditor(const AOwner: TComponent;
  const AViewField: TKViewField; const ARowField: TKExtFormRowField;
  const AFieldCharWidth: Integer; const AIsReadOnly: Boolean;
  const ALabel: string): IKExtEditor;
var
  LFormField: TExtFormField;
begin
  Result := TryCreateFileEditor(AOwner, AViewField, ARowField, AFieldCharWidth, AIsReadOnly, ALabel);
  if Result = nil then
    Result := TryCreateComboBox(AOwner, AViewField, ARowField, AFieldCharWidth, AIsReadOnly);
  if Result = nil then
    Result := TryCreateTextArea(AOwner, AViewField, ARowField, AFieldCharWidth, AIsReadOnly);
  if Result = nil then
    Result := TryCreateCheckBox(AOwner, AViewField, AIsReadOnly);
  if Result = nil then
    Result := TryCreateDateField(AOwner, AViewField, ARowField, AFieldCharWidth, AIsReadOnly);
  if Result = nil then
    Result := TryCreateTimeField(AOwner, AViewField, ARowField, AFieldCharWidth, AIsReadOnly);
  if Result = nil then
    Result := TryCreateDateTimeField(AOwner, AViewField, ARowField, AFieldCharWidth, AIsReadOnly);
  if Result = nil then
    Result := TryCreateNumberField(AOwner, AViewField, ARowField, AFieldCharWidth, AIsReadOnly);
  if Result = nil then
    Result := CreateTextField(AOwner, AViewField, ARowField, AFieldCharWidth, AIsReadOnly);

  LFormField := Result.AsExtFormField;
  if Assigned(LFormField) then
  begin
    if LFormField is TExtFormTextField then
    begin
      if AViewField.Hint <> '' then
        TExtFormTextField(LFormField).EmptyText := _(AViewField.Hint);
    end;

    if not AIsReadOnly then
      AViewField.ApplyRules(
        procedure (ARuleImpl: TKRuleImpl)
        begin
          if ARuleImpl is TKExtRuleImpl then
            TKExtRuleImpl(ARuleImpl).ApplyToFormField(LFormField);
        end);

    if AIsReadOnly then
      LFormField.Cls := 'x-form-readonly';
    LFormField.Name := AViewField.AliasedName;
    LFormField.ReadOnly := AIsReadOnly;
  end;
end;

function TKExtEditorManager.TryCreateComboBox(
  const AOwner: TComponent; const AViewField: TKViewField;
  const ARowField: TKExtFormRowField; const AFieldCharWidth: Integer;
  const AIsReadOnly: Boolean): IKExtEditor;
var
  LLookupCommandText: string;
  LAllowedValues: TEFPairs;
  LComboBox: TKExtFormComboBoxEditor;
  I: Integer;
  LKeyFieldsCount: Integer;
begin
  Assert(Assigned(AOwner));

  Result := nil;
  if not AViewField.IsDetailReference then
  begin
    LLookupCommandText := GetLookupCommandText(AViewField);
    LAllowedValues := AViewField.GetChildrenAsPairs('AllowedValues', True);
    // Translate allowed value descriptions if needed.
    for I := Low(LAllowedValues) to High(LAllowedValues) do
      LAllowedValues[I].Value := _(LAllowedValues[I].Value);
    if (LLookupCommandText <> '') or (Length(LAllowedValues) > 0) then
    begin
      LComboBox := TKExtFormComboBoxEditor.Create(AOwner);
      try
        if not Assigned(ARowField) then
          LComboBox.Width := LComboBox.CharsToPixels(AFieldCharWidth + TRIGGER_WIDTH)
        else
          ARowField.CharWidth := AFieldCharWidth + TRIGGER_WIDTH;

        // Enable the combo box to post its hidden value instead of the visible description.
        LComboBox.HiddenName := AViewField.FieldNamesForUpdate;
        LComboBox.SubmitValue := True;
        if Length(LAllowedValues) > 0 then
        begin
          LComboBox.StoreArray := LComboBox.JSArray(PairsToJSON(LAllowedValues));
          LComboBox.ValueField := AViewField.FieldNamesForUpdate;
          LComboBox.SubmitValue := False;
        end
        else // LLookupCommandText <> ''
        begin
          { TODO : move inside the combobox class }
          if AViewField.IsReference and AViewField.ModelField.ReferencedModel.IsLarge then
            LComboBox.SetupServerStore(AViewField, LLookupCommandText)
          else
          begin
            LComboBox.Mode := 'local';
            if AViewField.IsReference then
              LKeyFieldsCount := AViewField.ModelField.ReferencedModel.KeyFieldCount
            else
              LKeyFieldsCount := 0;
            LComboBox.StoreArray := LComboBox.JSArray(DataSetToJSON(
              Session.Config.DBConnections[AViewField.Table.DatabaseName],
              LLookupCommandText, LKeyFieldsCount));
            // Make the drop-down list larger.
            LComboBox.ListWidth := Trunc(((AFieldCharWidth + TRIGGER_WIDTH) * 12) * 1.1);
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
    end;
  end;
end;

function TKExtEditorManager.TryCreateTextArea(
  const AOwner: TComponent; const AViewField: TKViewField;
  const ARowField: TKExtFormRowField; const AFieldCharWidth: Integer;
  const AIsReadOnly: Boolean): IKExtEditor;
var
  LTextArea: TKExtFormTextArea;
begin
  Assert(Assigned(AOwner));

  if AViewField.IsBlob or (AViewField.Size div SizeOf(Char) >= MULTILINE_EDIT_THRESHOLD) then
  begin
    LTextArea := TKExtFormTextArea.Create(AOwner);
    try
      if not Assigned(ARowField) then
        LTextArea.Width := LTextArea.CharsToPixels(AFieldCharWidth)
      else
        ARowField.CharWidth := AFieldCharWidth;
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

function TKExtEditorManager.TryCreateCheckBox(
  const AOwner: TComponent; const AViewField: TKViewField;
  const AIsReadOnly: Boolean): IKExtEditor;
var
  LCheckbox: TKExtFormCheckbox;
begin
  Assert(Assigned(AOwner));

  if AViewField.DataType is TEFBooleanDataType then
  begin
    LCheckbox := TKExtFormCheckbox.Create(AOwner);
    try
      LCheckbox.BoxLabel := '';//LLabel;
      if AIsReadOnly then
        LCheckbox.Disabled := True;
      Result := LCheckbox;
    except
      LCheckbox.Free;
      raise;
    end;
  end
  else
    Result := nil;
end;

function TKExtEditorManager.TryCreateDateField(
  const AOwner: TComponent; const AViewField: TKViewField;
  const ARowField: TKExtFormRowField; const AFieldCharWidth: Integer;
  const AIsReadOnly: Boolean): IKExtEditor;
var
  LDateField: TKExtFormDateField;
  LFormat: string;
begin
  Assert(Assigned(AOwner));

  if AViewField.DataType is TEFDateDataType then
  begin
    LDateField := TKExtFormDateField.Create(AOwner);
    try
      if not Assigned(ARowField) then
        LDateField.Width := LDateField.CharsToPixels(AFieldCharWidth + TRIGGER_WIDTH)
      else
        ARowField.CharWidth := AFieldCharWidth + TRIGGER_WIDTH;
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

function TKExtEditorManager.TryCreateTimeField(
  const AOwner: TComponent; const AViewField: TKViewField;
  const ARowField: TKExtFormRowField; const AFieldCharWidth: Integer;
  const AIsReadOnly: Boolean): IKExtEditor;
var
  LTimeField: TKExtFormTimeField;
  LFormat: string;
begin
  Assert(Assigned(AOwner));

  if AViewField.DataType is TEFTimeDataType then
  begin
    LTimeField := TKExtFormTimeField.Create(AOwner);
    try
      if not Assigned(ARowField) then
        LTimeField.Width := LTimeField.CharsToPixels(AFieldCharWidth + TRIGGER_WIDTH)
      else
        ARowField.CharWidth := AFieldCharWidth + TRIGGER_WIDTH;

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

function TKExtEditorManager.TryCreateDateTimeField(
  const AOwner: TComponent; const AViewField: TKViewField;
  const ARowField: TKExtFormRowField; const AFieldCharWidth: Integer;
  const AIsReadOnly: Boolean): IKExtEditor;
const
  SPACER_WIDTH = 1;
var
  LDateTimeField: TKExtFormDateTimeField;
  LFormats: TStringDynArray;
  LDateFormat: string;
  LTimeFormat: string;
begin
  Assert(Assigned(AOwner));

  if AViewField.DataType is TEFDateTimeDataType then
  begin
    LDateTimeField := TKExtFormDateTimeField.Create(AOwner);
    try
      if not Assigned(ARowField) then
        LDateTimeField.Width := LDateTimeField.CharsToPixels(AFieldCharWidth + (2 * TRIGGER_WIDTH) + SPACER_WIDTH)
      else
        ARowField.CharWidth := AFieldCharWidth + (2 * TRIGGER_WIDTH) + SPACER_WIDTH;
      LFormats := Split(AViewField.EditFormat, ' ');
      if Length(LFormats) > 0 then
        LDateFormat := LFormats[0]
      else
        LDateFormat := Session.Config.UserFormatSettings.ShortDateFormat;
      if Length(LFormats) > 1 then
        LTimeFormat := LFormats[1]
      else
        LTimeFormat := Session.Config.UserFormatSettings.ShortTimeFormat;
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

function TKExtEditorManager.TryCreateFileEditor(
  const AOwner: TComponent; const AViewField: TKViewField;
  const ARowField: TKExtFormRowField; const AFieldCharWidth: Integer;
  const AIsReadOnly: Boolean; const ALabel: string): IKExtEditor;
var
  LFileEditor: TKExtFormFileEditor;
begin
  Assert(Assigned(AOwner));

  if (AViewField.DataType is TEFBlobDataType) or (AViewField.DataType is TKFileReferenceDataType) then
  begin
    if AViewField.DataType is TEFBlobDataType then
      LFileEditor := TKExtFormFileBlobEditor.Create(AOwner)
    else
      LFileEditor := TKExtFormFileReferenceEditor.Create(AOwner);
    try
      LFileEditor.IsReadOnly := AIsReadOnly;
      LFileEditor.FieldLabel := ALabel;
      LFileEditor.TotalCharWidth := AFieldCharWidth - 1;
      if Assigned(ARowField) then
        ARowField.CharWidth := AFieldCharWidth;
      Result := LFileEditor;
    except
      LFileEditor.Free;
      raise;
    end;
  end
  else
    Result := nil;
end;

function TKExtEditorManager.TryCreateNumberField(const AOwner: TComponent;
  const AViewField: TKViewField;
  const ARowField: TKExtFormRowField; const AFieldCharWidth: Integer;
  const AIsReadOnly: Boolean): IKExtEditor;
var
  LNumberField: TKExtFormNumberField;
begin
  Assert(Assigned(AOwner));

  if AViewField.DataType is TEFNumericDataTypeBase then
  begin
    LNumberField := TKExtFormNumberField.Create(AOwner);
    try
      if not Assigned(ARowField) then
        LNumberField.Width := LNumberField.CharsToPixels(AFieldCharWidth)
      else
        ARowField.CharWidth := AFieldCharWidth;
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

function TKExtEditorManager.CreateTextField(const AOwner: TComponent;
  const AViewField: TKViewField;
  const ARowField: TKExtFormRowField; const AFieldCharWidth: Integer;
  const AIsReadOnly: Boolean): IKExtEditor;
var
  LTextField: TKExtFormTextField;
begin
  Assert(Assigned(AOwner));

  LTextField := TKExtFormTextField.Create(AOwner);
  try
    if not Assigned(ARowField) then
      LTextField.Width := LTextField.CharsToPixels(AFieldCharWidth)
    else
      ARowField.CharWidth := AFieldCharWidth;
    if not AIsReadOnly then
    begin
      if AViewField.Size <> 0 then
        LTextField.MaxLength := AViewField.Size;
      LTextField.AllowBlank := not AViewField.IsRequired;
    end;
    if AViewField.GetBoolean('IsPassword') then
      LTextField.InputType := itPassword;
    Result := LTextField;
  except
    LTextField.Free;
    raise;
  end;
end;

function TKExtEditorManager.GetLookupCommandText(const AViewField: TKViewField): string;
begin
  if AViewField.IsReference then
  begin
    Result := TKSQLBuilder.GetLookupSelectStatement(AViewField);
    if AViewField.ModelField.ReferencedModel.IsLarge then
      Result := AddToSQLWhereClause(Result, '(' + AViewField.ModelField.ReferencedModel.CaptionField.DBColumnName + ' like ''{query}%'')');
    Result := TEFMacroExpansionEngine.Instance.Expand(Result);
  end
  else
    Result := '';
end;

function TKExtEditorManager.GetSession: TKExtSession;
begin
  Result := nil;
  if Assigned(FOnGetSession) then
    FOnGetSession(Result);
  if not Assigned(Result) then
    raise EKError.Create('Session not assigned');
end;

end.
