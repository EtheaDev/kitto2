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
  SysUtils
  , Classes
  , Generics.Collections
  , Types
  , Web.HTTPApp
  , Ext.Base
  , Ext.Form
  , Ext.Grid
  , Ext.Data
  , EF.Intf
  , EF.Classes
  , EF.Tree
  , EF.ObserverIntf
  , Kitto.Metadata.Views
  , Kitto.Metadata.DataView
  , Kitto.Store
  , Kitto.JS.Base
  , Kitto.Ext.Base
  , Kitto.Ext.Controller
  , Kitto.Ext.LookupField
  ;

const
  // String fields of this size or longer are represented by multiline edit
  // controls.
  MULTILINE_EDIT_THRESHOLD = 200;
  // Expected width of the combobox or calendar trigger button in characters.
  // Used to enlarge these kinds of editors.
  TRIGGER_WIDTH = 2;

  LAYOUT_MEMOWIDTH = 60;
  LAYOUT_MAXFIELDWIDTH = 60;
  LAYOUT_MINFIELDWIDTH = 5;
  LAYOUT_REQUIREDLABELTEMPLATE = '<b>{label}*</b>';
  LAYOUT_MSGTARGET = 'Qtip';
  DEFAULT_LABEL_SEPARATOR = ':';

type
  IKExtEditItem = interface(IEFInterface)
    ['{4F5A1E4E-D5A1-44FE-93DC-E1ABF1209CE1}']
    procedure SetOption(const ANode: TEFNode);
    function AsExtObject: TExtObject;
    // Generates and executes JS code to refresh the displayed value(s) from the server record.
    procedure RefreshValue;

    procedure SetTransientProperty(const APropertyName: string; const AValue: Variant);
    function GetEditItemId: string;
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

    function GetFieldName: string;
    procedure SetFieldName(const AValue: string);
    property FieldName: string read GetFieldName write SetFieldName;

    // Generates and executes JS code to store the displayed value(s) into the specified
    // JS object variable (as a property named after the editor's Name).
    procedure StoreValue(const AObjectName: string);

    // Generates and executes JS code to set the field read-only or editable.
    procedure SetReadOnly(const AValue: Boolean);
  end;

  TKExtEditPage = class;

  TKExtEditPanel = class(TExtFormFormPanel);

  TKExtEditPage = class(TExtPanel, IKExtEditItem, IKExtEditContainer)
  strict private
    FEditPanel: TKExtEditPanel;
    FDataRecord: TKViewTableRecord;
    FUnexpandedTitle: string;
    FEditItemId: string;
    procedure SetUnexpandedTitle(const AValue: string);
  protected
    procedure InitDefaults; override;
    function GetObjectNamePrefix: string; override;
  public
    function AsObject: TObject;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure AddChild(const AEditItem: IKExtEditItem);
    procedure SetOption(const ANode: TEFNode);
    function AsExtObject: TExtObject;
    procedure RefreshValue;
    procedure SetTransientProperty(const APropertyName: string; const AValue: Variant);
    function GetEditItemId: string;

    property EditPanel: TKExtEditPanel read FEditPanel write FEditPanel;
    property DataRecord: TKViewTableRecord read FDataRecord write FDataRecord;
    property UnexpandedTitle: string read FUnexpandedTitle write SetUnexpandedTitle;
  end;

  TKExtFormFieldSet = class(TExtFormFieldSet, IKExtEditItem, IKExtEditContainer)
  strict private
    FEditItemId: string;
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
    procedure SetTransientProperty(const APropertyName: string; const AValue: Variant);
    function GetEditItemId: string;
    property EditItemId: string read FEditItemId write FEditItemId;

    property DataRecord: TKViewTableRecord read FDataRecord write FDataRecord;
    property UnexpandedTitle: string read FUnexpandedTitle write SetUnexpandedTitle;
  end;

  TKExtFormCompositeField = class(TExtFormCompositeField, IKExtEditItem, IKExtEditContainer)
  strict private
    FEditItemId: string;
  public
    function AsObject: TObject; inline;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure AddChild(const AEditItem: IKExtEditItem);
    procedure SetOption(const ANode: TEFNode);
    function AsExtObject: TExtObject; inline;
    procedure RefreshValue;
    procedure SetTransientProperty(const APropertyName: string; const AValue: Variant);
    function GetEditItemId: string;
    property EditItemId: string read FEditItemId write FEditItemId;
  end;

  TKExtFormContainer = class(TExtContainer, IKExtEditItem)
  strict private
    FEditItemId: string;
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
    procedure SetTransientProperty(const APropertyName: string; const AValue: Variant);
    function GetEditItemId: string;
    property EditItemId: string read FEditItemId write FEditItemId;
  end;

  TKExtFormRow = class(TKExtFormContainer, IKExtEditContainer)
  protected
    procedure InitDefaults; override;
    function GetObjectNamePrefix: string; override;
  end;

  /// <summary>
  ///  Encapsulates a field in a row. Does NOT implement the container
  ///  interface, as it is a commodity class only.
  /// </summary>
  TKExtFormRowField = class(TKExtFormContainer, IKExtEditor)
  strict private
    FEditor: IKExtEditor;
    FCharWidth: Integer;
    FLabelWidth: Integer;
    FRecordField: TKViewTableField;
    FEditItemId: string;
    procedure SetCharWidth(const AValue: Integer);
    procedure SetLabelWidth(const AValue: Integer);
  protected
    procedure InitDefaults; override;
    function InternalSetOption(const ANode: TEFNode): Boolean; override;
    function GetObjectNamePrefix: string; override;
  public
    destructor Destroy; override;
  public
    function Encapsulate(const AValue: IKExtEditor): IKExtEditor;
    property CharWidth: Integer read FCharWidth write SetCharWidth;
    property LabelWidth: Integer read FLabelWidth write SetLabelWidth;
    function AsExtFormField: TExtFormField;
    function GetRecordField: TKViewTableField;
    procedure SetRecordField(const AValue: TKViewTableField);
    function GetFieldName: string;
    procedure SetFieldName(const AValue: string);
    procedure RefreshValue;
    procedure StoreValue(const AObjectName: string);
    procedure SetTransientProperty(const APropertyName: string; const AValue: Variant);
    function GetEditItemId: string;
    procedure SetReadOnly(const AValue: Boolean);
  end;

{ TODO : support the CheckboxGroup and Radiogroup containers? }

  TKExtFormNumberField = class(TExtFormNumberField, IKExtEditItem, IKExtEditor)
  private
    FFieldName: string;
    FRecordField: TKViewTableField;
    FThousandSeparator: string;
    FAlwaysDisplayDecimals: Boolean;
    FUseThousandSeparator: Boolean;
    procedure FieldChange(This: TExtFormField; NewValue, OldValue: string);
    procedure SetThousandSeparator(const AValue: string);
    procedure SetAlwaysDisplayDecimals(const AValue: Boolean);
    procedure SetUseThousandSeparator(const AValue: Boolean);
  public
    property ThousandSeparator: string read FThousandSeparator write SetThousandSeparator;
    property AlwaysDisplayDecimals: Boolean read FAlwaysDisplayDecimals write SetAlwaysDisplayDecimals;
    property UseThousandSeparator: Boolean read FUseThousandSeparator write SetUseThousandSeparator;

    function AsObject: TObject; inline;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure SetOption(const ANode: TEFNode);
    function AsExtObject: TExtObject; inline;
    function AsExtFormField: TExtFormField; inline;
    function GetRecordField: TKViewTableField;
    procedure SetRecordField(const AValue: TKViewTableField);
    function GetFieldName: string;
    procedure SetFieldName(const AValue: string);
    procedure RefreshValue;
    procedure StoreValue(const AObjectName: string);
    procedure SetTransientProperty(const APropertyName: string; const AValue: Variant);
    function GetEditItemId: string;
    procedure SetReadOnly(const AValue: Boolean);
  end;

  TKExtFormTextField = class(TExtFormTextField, IKExtEditItem, IKExtEditor)
  private
    FFieldName: string;
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
    function GetFieldName: string;
    procedure SetFieldName(const AValue: string);
    procedure RefreshValue;
    procedure StoreValue(const AObjectName: string);
    procedure SetTransientProperty(const APropertyName: string; const AValue: Variant);
    function GetEditItemId: string;
    procedure SetReadOnly(const AValue: Boolean);
  end;

  TKExtFormTextArea = class(TExtFormTextArea, IKExtEditItem, IKExtEditor)
  private
    FFieldName: string;
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
    function GetFieldName: string;
    procedure SetFieldName(const AValue: string);
    procedure RefreshValue;
    procedure StoreValue(const AObjectName: string);
    procedure SetTransientProperty(const APropertyName: string; const AValue: Variant);
    function GetEditItemId: string;
    procedure SetReadOnly(const AValue: Boolean);
  end;

  TKExtFormCheckbox = class(TExtFormCheckbox, IKExtEditItem, IKExtEditor)
  private
    FFieldName: string;
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
    function GetFieldName: string;
    procedure SetFieldName(const AValue: string);
    procedure RefreshValue;
    procedure StoreValue(const AObjectName: string);
    procedure SetTransientProperty(const APropertyName: string; const AValue: Variant);
    function GetEditItemId: string;
    procedure SetReadOnly(const AValue: Boolean);    
  end;

  TKExtFormDateField = class(TExtFormDateField, IKExtEditItem, IKExtEditor)
  private
    FFieldName: string;
    FRecordField: TKViewTableField;
    procedure FieldChange(This: TExtFormField; NewValue, OldValue: string);
    // Converts a date from javascript's implicit format (the one used when
    // firing the change handler) to the format expected by SetAsJSONValue
    // (see FieldChange)
    function ConvertDate(const AValue: string): string;
  public
    function AsObject: TObject; inline;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure SetOption(const ANode: TEFNode);
    function AsExtObject: TExtObject; inline;
    function AsExtFormField: TExtFormField; inline;
    function GetRecordField: TKViewTableField;
    procedure SetRecordField(const AValue: TKViewTableField);
    function GetFieldName: string;
    procedure SetFieldName(const AValue: string);
    procedure RefreshValue;
    procedure StoreValue(const AObjectName: string);
    procedure SetTransientProperty(const APropertyName: string; const AValue: Variant);
    function GetEditItemId: string;
    procedure SetReadOnly(const AValue: Boolean);
  end;

  TKExtFormTimeField = class(TExtFormTimeField, IKExtEditItem, IKExtEditor)
  private
    FFieldName: string;
    FRecordField: TKViewTableField;
    procedure FieldChange(This: TExtFormField; NewValue, OldValue: string);
    // Converts a time from javascript's implicit format (the one used when
    // firing the change handler) to the format expected by SetAsJSONValue
    // (see FieldChange)
    function ConvertTime(const AValue: string): string;
  public
    function AsObject: TObject; inline;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure SetOption(const ANode: TEFNode);
    function AsExtObject: TExtObject; inline;
    function AsExtFormField: TExtFormField; inline;
    function GetRecordField: TKViewTableField;
    procedure SetRecordField(const AValue: TKViewTableField);
    function GetFieldName: string;
    procedure SetFieldName(const AValue: string);
    procedure RefreshValue;
    procedure StoreValue(const AObjectName: string);
    procedure SetTransientProperty(const APropertyName: string; const AValue: Variant);
    function GetEditItemId: string;
    procedure SetReadOnly(const AValue: Boolean);
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
    FFieldName: string;
    FRecordField: TKViewTableField;
    procedure SetDateFormat(const AValue: string);
    procedure SetTimeFormat(const AValue: string);
    procedure SetAltDateFormats(const AValue: string);
    procedure SetAllowBlank(const AValue: Boolean);
    procedure SetAltTimeFormats(const AValue: string);
    procedure FieldChange(This: TExtFormField; NewValue, OldValue: string);
    // Converts a date/time from javascript's implicit format (the one used when
    // firing the change handler) to the format expected by SetAsJSONValue
    // (see FieldChange)
    function ConvertDateTime(const AValue: string): string;
  protected
    function GetObjectNamePrefix: string; override;
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
    function GetFieldName: string;
    procedure SetFieldName(const AValue: string);
    procedure RefreshValue;
    procedure StoreValue(const AObjectName: string);

    property DateFormat: string read FDateFormat write SetDateFormat;
    //property DateConfig: TExtObject read FDateConfig write SetDateConfig;
    property TimeFormat: string read FTimeFormat write SetTimeFormat;
    //property TimeConfig: TExtObject read FTimeConfig write SetTimeConfig;
    property AltDateFormats: string read FAltDateFormats write SetAltDateFormats;
    property AltTimeFormats: string read FAltTimeFormats write SetAltTimeFormats;
    property AllowBlank: Boolean read FAllowBlank write SetAllowBlank;
    procedure SetTransientProperty(const APropertyName: string; const AValue: Variant);
    function GetEditItemId: string;
    procedure SetReadOnly(const AValue: Boolean);
  end;

  TKExtLookupEditor = class(TKExtLookupField, IKExtEditItem, IKExtEditor)
  private
    FFieldName: string;
    FHiddenName: string;
    FRecordField: TKViewTableField;
  strict protected
    procedure LookupConfirmed(const ARecord: TKViewTableRecord); override;
  public
    procedure Setup(const AViewField: TKVIewField; const AIsReadOnly: Boolean; const AFieldCharWidth: Integer);
    function AsExtFormField: TExtFormField;
    function GetRecordField: TKViewTableField;
    procedure SetRecordField(const AValue: TKViewTableField);
    function GetFieldName: string;
    procedure SetFieldName(const AValue: string);
    procedure StoreValue(const AObjectName: string);
    procedure SetOption(const ANode: TEFNode);
    procedure RefreshValue;
    procedure SetTransientProperty(const APropertyName: string; const AValue: Variant);
    function GetEditItemId: string;
    function AsObject: TObject;
    procedure SetReadOnly(const AValue: Boolean);
  //published
    procedure ClearClick; override;
  end;

  TKExtFormComboBoxEditor = class(TKExtFormComboBox, IKExtEditItem, IKExtEditor)
  private type
    TListMode = (Fixed, Lookup);
  private
    FListMode: TListMode;
    FServerStore: TKStore;
    FFieldName: string;
    FRecordField: TKViewTableField;
  protected
    procedure InitDefaults; override;
  public
    destructor Destroy; override;
  public
    class function SupportsViewField(const AViewField: TKViewField): Boolean;
    procedure Setup(const AViewField: TKVIewField; const AIsReadOnly: Boolean; const AFieldCharWidth: Integer);
    procedure SetOption(const ANode: TEFNode);
    function AsExtObject: TExtObject; inline;
    function AsExtFormField: TExtFormField; inline;
    function GetRecordField: TKViewTableField;
    procedure SetRecordField(const AValue: TKViewTableField);
    function GetFieldName: string;
    procedure SetFieldName(const AValue: string);
    procedure RefreshValue;
    procedure StoreValue(const AObjectName: string);
    procedure SetTransientProperty(const APropertyName: string; const AValue: Variant);
    function GetEditItemId: string;
    procedure SetReadOnly(const AValue: Boolean);
  //published
    procedure GetRecordPage;
    procedure ValueChanged;
  end;

  TKExtFormFileUploadField = class(TExtFormFileField, IKExtEditItem, IKExtEditor)
  private
    FFieldName: string;
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
    function GetFieldName: string;
    procedure SetFieldName(const AValue: string);
    procedure RefreshValue;
    procedure StoreValue(const AObjectName: string);
    procedure SetTransientProperty(const APropertyName: string; const AValue: Variant);
    function GetEditItemId: string;
    procedure SetReadOnly(const AValue: Boolean);
  end;

  TKExtFormFileEditor = class(TExtFormFieldContainer, IKExtEditItem, IKExtEditor)
  strict private
    FDescriptionField: TExtFormTextField;
    FWindow: TKExtModalWindow;
    FDownloadButton: TKExtButton;
    FIsReadOnly: Boolean;
    FClearButton: TKExtButton;
    FTotalCharWidth: Integer;
    FPictureView: TExtPanel;
    FUploadButton: TKExtButton;
    function GetContentDescription: string;
    procedure UpdateGUI(const AUpdatePicture: Boolean);
    procedure PictureViewAfterRender(This: TExtComponent);
    procedure SetTotalCharWidth(const AValue: Integer);
    const EMPTY_DESCRIPTION = 'Empty';
  strict protected
    FFieldName: string;
    FRecordField: TKViewTableField;
    FLastUploadedClientFileName: string;
    FImageWidth: Integer;
    FImageHeight: Integer;
    function GetCurrentContentSize: Integer; virtual; abstract;
    procedure ProcessUploadedFile(const AFile: TAbstractWebRequestFile); virtual;
    procedure ClearContents; virtual;
    function IsEmpty: Boolean;
    function IsPicture: Boolean;

    function GetDownloadFileName: string;
    function GetStoredFileName: string;
    function GetDefaultFileName: string;
    function GetDownloadFileExtFromFieldData: string; virtual; abstract;
    procedure DoDownloadFieldData(const AFileName: string); virtual; abstract;
    function CreateImageDataStream: TStream; virtual; abstract;
  protected
    procedure CreateGUI(const AViewField: TKViewField);
    function GetObjectNamePrefix: string; override;
  public
    function AsObject: TObject; inline;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure SetOption(const ANode: TEFNode);
    function AsExtObject: TExtObject; inline;
    function AsExtFormField: TExtFormField; inline;
    function GetRecordField: TKViewTableField;
    procedure SetRecordField(const AValue: TKViewTableField);
    function GetFieldName: string;
    procedure SetFieldName(const AValue: string);
    procedure RefreshValue;
    procedure StoreValue(const AObjectName: string);
    property TotalCharWidth: Integer read FTotalCharWidth write SetTotalCharWidth;
    procedure SetTransientProperty(const APropertyName: string; const AValue: Variant);
    function GetEditItemId: string;
    procedure SetReadOnly(const AValue: Boolean);
  //published
    procedure ShowUploadFileDialog;
    procedure Upload;
    procedure Clear;
    procedure PostUpload;
    procedure StartDownload;
    procedure DownloadFieldData;
    procedure GetImageData;
    procedure GetImageMarkup;
  end;

  TKFileOpKind = (okDelete);
  TKFileOpEvent = (oePost, oeCancel);
  /// <summary>
  ///  Used to keep track of file operations to be performed after editing
  ///  or inserting a record containing file reference fields.
  /// </summary>
  TKFileOp = record
    Kind: TKFileOpKind;
    PathName: string;
    Event: TKFileOpEvent;
    Done: Boolean;
    constructor Create(const AKind: TKFileOpKind; const APathName: string; const AEvent: TKFileOpEvent);
    class operator Implicit(const AOther: TStringDynArray): TKFileOp;
    class operator Implicit(const AOther: TKFileOp): TStringDynArray;
  end;

  TKExtFormFileReferenceEditor = class(TKExtFormFileEditor)
  strict private
    function GetFieldPath: string;
    function GetServerFileName: string;
    procedure DeferredFileDelete(const AEvent: TKFileOpEvent; const APathName: string);
  strict protected
    procedure ClearContents; override;
    procedure ProcessUploadedFile(const AFile: TAbstractWebRequestFile); override;
    function GetCurrentContentSize: Integer; override;
    function GetDownloadFileExtFromFieldData: string; override;
    procedure DoDownloadFieldData(const AFileName: string); override;
    function CreateImageDataStream: TStream; override;
  end;

  TKExtFormFileBlobEditor = class(TKExtFormFileEditor)
  strict protected
    procedure ClearContents; override;
    procedure ProcessUploadedFile(const AFile: TAbstractWebRequestFile); override;
    function GetCurrentContentSize: Integer; override;
    function GetDownloadFileExtFromFieldData: string; override;
    procedure DoDownloadFieldData(const AFileName: string); override;
    function CreateImageDataStream: TStream; override;
  end;

  TKExtLayoutDefaults = record
    MemoWidth: Integer;
    MaxFieldWidth: Integer;
    MinFieldWidth: Integer;
    RequiredLabelTemplate: string;
    MsgTarget: string;
    LabelSeparator : string;
    procedure Init;
  end;

  type
    TKExtEditOperation = (eoUpdate, eoInsert);

  TKExtEditorManager = class;

  /// <summary>
  ///  Creates editors based on layouts. Can synthesize a default layout if
  ///  missing.
  /// </summary>
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
    FCurrentLabelWidth: Integer;
    FCurrentLabelAlign: TExtContainerLabelAlign;
    FEditContainers: TStack<IKExtEditContainer>;
    FOnNewEditItem: TProc<IKExtEditItem>;
    FOperation: TKExtEditOperation;
    FTabPanel: TExtTabPanel;
    FEditorManager: TKExtEditorManager;
    procedure SetMainEditPage(const AValue: TKExtEditPage);
    procedure FinalizeCurrentEditPage;
    function CreatePageBreak(const ATitle: string): IKExtEditItem;
    function GetViewTable: TKViewTable;
    function CreateEditItem(const ANode: TEFNode;
      const AContainer: IKExtEditContainer): IKExtEditItem;
    function CreateEditor(const AFieldName: string;
      const AContainer: IKExtEditContainer;
      const AOptions: TEFNode = nil): IKExtEditor;
    function CreateFieldSet(const AId: string): IKExtEditItem;
    function CreateCompositeField(const AId: string): IKExtEditItem;
    procedure SetGlobalOption(const ANode: TEFNode);
    procedure LayoutError(const AErrorMessage: string);
    function CreateRow(const AId: string): IKExtEditItem;
    procedure CreateEditorsFromLayout(const ALayout: TKLayout);
    procedure ProcessLayoutNode(const ANode: TEFNode);
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
  private
    procedure SetOperation(const AValue: TKExtEditOperation);
    procedure InitLabelPosAndWidth(const ANode: TEfTree);
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

    /// <summary>
    ///  Creates editors according to the specified layout or a default layout.
    /// </summary>
    /// <param name="ALayout">
    ///  Layout used to create the editors. Pass nil to manufacture a default
    ///  layout.
    /// </param>
    procedure CreateEditors(const ALayout: TKLayout);

    /// <summary>
    ///  A reference to the first field to focus. Only valid after calling
    ///  CreateEditors method.
    /// </summary>
    property FocusField: TExtFormField read FFocusField;
  end;

  /// <summary>
  ///  Creates editors for edit forms and in-place editors for grids.
  ///  Keeps track of created editors.
  ///  Used by the layout processor; can be used directly.
  /// </summary>
  TKExtEditorManager = class
  strict private
    FOperation: TKExtEditOperation;
    function TryCreateLookupEditor(const AOwner: TJSBase;
      const AViewField: TKViewField; const ARowField: TKExtFormRowField;
      const AFieldCharWidth: Integer; const AIsReadOnly: Boolean): IKExtEditor;
    function TryCreateComboBox(const AOwner: TJSBase; const AViewField: TKViewField;
      const ARowField: TKExtFormRowField; const AFieldCharWidth: Integer;
      const AIsReadOnly: Boolean): IKExtEditor;
    function TryCreateTextArea(const AOwner: TJSBase; const AViewField: TKViewField;
      const ARowField: TKExtFormRowField; const AFieldCharWidth: Integer;
      const AIsReadOnly: Boolean): IKExtEditor;
    function TryCreateCheckBox(const AOwner: TJSBase; const AViewField: TKViewField;
      const AIsReadOnly: Boolean): IKExtEditor;
    function TryCreateDateField(const AOwner: TJSBase; const AViewField: TKViewField;
      const ARowField: TKExtFormRowField; const AFieldCharWidth: Integer;
      const AIsReadOnly: Boolean): IKExtEditor;
    function TryCreateTimeField(const AOwner: TJSBase; const AViewField: TKViewField;
      const ARowField: TKExtFormRowField; const AFieldCharWidth: Integer;
      const AIsReadOnly: Boolean): IKExtEditor;
    function TryCreateDateTimeField(const AOwner: TJSBase; const AViewField: TKViewField;
      const ARowField: TKExtFormRowField; const AFieldCharWidth: Integer;
      const AIsReadOnly: Boolean): IKExtEditor;
    function TryCreateNumberField(const AOwner: TJSBase; const AViewField: TKViewField;
      const ARowField: TKExtFormRowField; const AFieldCharWidth: Integer;
      const AIsReadOnly: Boolean): IKExtEditor;
    function TryCreateFileEditor(const AOwner: TJSBase; const AViewField: TKViewField;
      const ARowField: TKExtFormRowField; const AFieldCharWidth: Integer;
      const AIsReadOnly: Boolean; const ALabel: string): IKExtEditor;
    function CreateTextField(const AOwner: TJSBase; const AViewField: TKViewField;
      const ARowField: TKExtFormRowField; const AFieldCharWidth: Integer;
      const AIsReadOnly: Boolean): IKExtEditor;
  public
    property Operation: TKExtEditOperation read FOperation write FOperation;
    function CreateEditor(const AOwner: TJSBase; const AViewField: TKViewField;
      const ARowField: TKExtFormRowField; const AFieldCharWidth: Integer;
      const AIsReadOnly: Boolean; const ALabel: string = ''): IKExtEditor;
    /// <summary>
    ///  Creates an in-place editor for the specified field.
    /// </summary>
    function CreateGridCellEditor(const AOwner: TJSBase;
      const AViewField: TKViewField): TExtFormField;
  end;

  TExtFormFieldHelper = class helper for TExtFormField
  public
    procedure StoreValue(const AObjectName: string);
    procedure SetTransientProperty(const APropertyName: string; const AValue: Variant);
  end;

  TKEditItemList = class(TList<TObject>)
  private
  public
    procedure EnumEditors(const APredicate: TFunc<IKExtEditor, Boolean>; const AHandler: TProc<IKExtEditor>);
    procedure EditorsByViewField(const AViewField: TKVIewField; const AHandler: TProc<IKExtEditor>);
    procedure EditorsByFieldName(const AFieldName: string; const AHandler: TProc<IKExtEditor>);
    procedure EditorsByField(const AField: TKField; const AHandler: TProc<IKExtEditor>);
    procedure AllEditors(const AHandler: TProc<IKExtEditor>);
    procedure EnumEditItems(const APredicate: TFunc<IKExtEditItem, Boolean>;
      const AHandler: TProc<IKExtEditItem>);
    procedure AllNonEditors(const AHandler: TProc<IKExtEditItem>);
    procedure AllEditItems(const AHandler: TProc<IKExtEditItem>);
    procedure EditItemsById(const AId: string; const AHandler: TProc<IKExtEditItem>);
  end;

implementation

uses
  Math
  , IOUtils
  , StrUtils
  {$IFDEF MSWINDOWS}
  , Windows
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  , Vcl.Graphics
  {$ENDIF}
  , Variants
  , DateUtils
  , System.JSON
  , Rtti
  , EF.Sys
  , EF.StrUtils
  , EF.Localization
  , EF.YAML
  , EF.Types
  , EF.SQL
  , EF.JSON
  , EF.DB
  , EF.Macros
  , EF.VariantUtils
  , Kitto.Config
  , Kitto.SQL
  , Kitto.Metadata
  , Kitto.Metadata.Models
  , Kitto.Types
  , Kitto.AccessControl
  , Kitto.Rules
  , Kitto.JS
  , Kitto.JS.Formatting
  , Kitto.Web.Application
  , Kitto.Web.Request
  , Kitto.Web.Response
  , Kitto.Ext.Form
  , Kitto.Ext.Utils
  , Kitto.Ext.Rules
  ;

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
    AFormField.WidthExpression := AFormField.CharsToPixels(ANode.AsInteger)
  else if SameText(ANode.Name, 'Width') then
    AFormField.WidthString := OptionAsIntegerOrPerc(ANode)
  else
    Result := False;
end;

function IsChangeHandlerNeeded(const AViewTableField: TKViewTableField): Boolean;
begin
  { TODO : Consider dependencies such as field names used in layout elements
    (such as field set titles). In order to do that, build a dependency list/tree. }
  if AViewTableField.ViewField.FileNameField <> '' then
    // Uploads always need the change handler.
    Result := True
  else if AViewTableField.ViewField.HasRules then
    Result := True
  else if AViewTableField.ViewField.DerivedFieldsExist then
    // Derived fields must be updated when source field changes.
    Result := True
  else if AViewTableField.ViewField.GetBoolean('NotifyChange') then
    // Temporary, for cases not handled by this detector and setup manually.
    Result := True
  else if Length(AViewTableField.ViewField.Table.GetFilterByFields(
      function (AFilterByViewField: TKFilterByViewField): Boolean
      begin
        Result := AFilterByViewField.SourceField = AViewTableField.ViewField;
      end)) > 0 then
    // If any fields are filtered by this field, then the change must be notified.
    Result := True
  else
    Result := False;
end;

procedure InvalidTransientProperty(APropertyName: string; const AValue: Variant);
begin
  raise EEFError.CreateFmt(_('Unknown transient property: %s = %s.'), [APropertyName, EFVarToStr(AValue)]);
end;

procedure SetComponentTransientProperty(const AComponent: TExtComponent; APropertyName: string; const AValue: Variant);
begin
  if SameText(APropertyName, 'Visible') then
    AComponent.SetVisible(AValue)
  else if SameText(APropertyName, 'Enabled') then
    AComponent.SetDisabled(not AValue)
  else
    InvalidTransientProperty(APropertyName, AValue);
end;

{ TKExtLayoutProcessor }

procedure TKExtLayoutProcessor.LayoutError(const AErrorMessage: string);
begin
  raise EEFError.CreateFmt(_('Layout parsing error. %s.'), [AErrorMessage]);
end;

procedure TKExtLayoutProcessor.InitLabelPosAndWidth(const ANode: TEfTree);
var
  LNode: TEFNode;
begin
  LNode := ANode.FindNode('LabelAlign');
  if Assigned(LNode) then
  begin
    FCurrentLabelAlign := OptionAsLabelAlign(LNode.AsString);
    if Assigned(FCurrentEditPage) then
      FCurrentEditPage.LabelAlign := FCurrentLabelAlign;
  end;
  LNode := ANode.FindNode('LabelWidth');
  if Assigned(LNode) then
    FCurrentLabelWidth := LNode.AsInteger;
end;

procedure TKExtLayoutProcessor.CreateEditorsFromLayout(const ALayout: TKLayout);
var
  I: Integer;
begin
  Assert(Assigned(FCurrentEditPage));
  Assert(Assigned(ALayout));

  FCurrentEditItem := nil;
  FCurrentLabelWidth := FORM_LABELWIDTH;
  FCurrentLabelAlign := FCurrentEditPage.LabelAlign;
  FEditContainers.Clear;

  InitLabelPosAndWidth(ALayout);

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

  InitLabelPosAndWidth(ANode);

  // Skip invisible fields.
  if SameText(ANode.Name, 'Field') then
  begin
    LViewField := ViewTable.FieldByAliasedName(ANode.AsString);
    if not ViewTable.IsFieldVisible(LViewField) then
      Exit;
  end;

  //Process Specific Nodes of Layout
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
    FCurrentEditItem := CreatePageBreak(_(ANode.Value));
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

    if (ANode.Parent is TKLayout) then
      SetGlobalOption(ANode)
    else if Assigned(FCurrentEditItem) then
      FCurrentEditItem.SetOption(ANode);
  end;

  if FCurrentEditItem is TKExtFormRowField then
  begin
    TKExtFormRowField(FCurrentEditItem).LabelAlign := FCurrentLabelAlign;
    TKExtFormRowField(FCurrentEditItem).LabelWidth := FCurrentLabelWidth;
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
    FCurrentEditPage.On('afterrender', FCurrentEditPage.GenerateAnonymousFunction(FFocusField.JSName + '.focus(false, 1000);'));
  FFocusField := nil;
end;

function TKExtLayoutProcessor.CreateEditItem(const ANode: TEFNode;
  const AContainer: IKExtEditContainer): IKExtEditItem;
begin
  if SameText(ANode.Name, 'Field') then
    Result := CreateEditor(ANode.Value, AContainer, ANode)
  else if SameText(ANode.Name, 'FieldSet') then
    Result := CreateFieldSet(ANode.AsExpandedString)
  else if SameText(ANode.Name, 'CompositeField') then
    Result := CreateCompositeField(ANode.AsExpandedString)
  else if SameText(ANode.Name, 'Row') then
    Result := CreateRow(ANode.AsExpandedString)
  else
    raise EEFError.CreateFmt(_('Unknown edit item type %s.'), [ANode.Name]);
  if Assigned(AContainer) then
    AContainer.AddChild(Result);
  if Assigned(FOnNewEditItem) then
    FOnNewEditItem(Result);
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
  LEmptyText: string;
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

  LIsReadOnly := FForceReadOnly or not LViewField.CanEditField(FOperation = eoInsert);

  if not LIsReadOnly and LViewField.IsDetailReference then
    LIsReadOnly := True;

  LLabel := '';
  if Assigned(AOptions) then
  begin
    LLabel := _(AOptions.GetString('DisplayLabel'));
    LEmptyText := _(AOptions.GetString('Hint'));
  end;
  if LLabel = '' then
    LLabel := _(LViewField.DisplayLabel);
  if LEmptyText = '' then
    LEmptyText := _(LViewField.Hint);

  if AContainer is TKExtFormRow then
  begin
    LRowField := TKExtFormRowField.Create(FCurrentEditPage);
    LRowField.HideLabels := TKExtFormRow(AContainer).HideLabels;
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
    if FCurrentEditPage.HideLabels and (LEmptyText = '') then
      LEmptyText := LLabel;
    if not LIsReadOnly and LViewField.IsRequired then
      LLabel := ReplaceText(FDefaults.RequiredLabelTemplate, '{label}', LLabel);
    LFormField.FieldLabel := LLabel;
    if FDefaults.LabelSeparator <> DEFAULT_LABEL_SEPARATOR then
      LFormField.LabelSeparator := FDefaults.LabelSeparator;

    if (LEmptyText <> '') and (LFormField is TExtFormTextField) then
      TExtFormTextField(LFormField).EmptyText := LEmptyText;
    //LFormField.SubmitValue := not LIsReadOnly;
    LFormField.MsgTarget := LowerCase(FDefaults.MsgTarget);

    if not FCurrentEditPage.HideLabels and (FFocusField = nil) and not LFormField.ReadOnly and not LFormField.Disabled then
      FFocusField := LFormField;
  end;

  if Assigned(LRowField) then
    Result := LRowField;
end;

function TKExtLayoutProcessor.CreateFieldSet(const AId: string): IKExtEditItem;
var
  LFieldSet: TKExtFormFieldSet;
begin
  Assert(Assigned(FCurrentEditPage));
  Assert(Assigned(FDataRecord));

  LFieldSet := TKExtFormFieldSet.Create(FCurrentEditPage);
  LFieldSet.HideLabels := FCurrentEditPage.HideLabels;
  LFieldSet.EditItemId := AId;
  LFieldSet.Collapsible := False;
  LFieldSet.DataRecord := FDataRecord;

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

  LPageBreak := TKExtEditPage.CreateAndAddToArray(FTabPanel.Items);
  LPageBreak.EditPanel := FFormPanel;
  LPageBreak.DataRecord := FDataRecord;
  LPageBreak.UnexpandedTitle := ATitle;
  LPageBreak.LabelWidth := FCurrentLabelWidth;
  FCurrentEditPage := LPageBreak;

  Result := LPageBreak;
end;

procedure TKExtLayoutProcessor.AfterConstruction;
begin
  inherited;
  FDefaults.Init;
  FEditContainers := TStack<IKExtEditContainer>.Create;
  FEditorManager := TKExtEditorManager.Create;
end;

function TKExtLayoutProcessor.CreateCompositeField(const AId: string): IKExtEditItem;
var
  LCompositeField: TKExtFormCompositeField;
begin
  Assert(Assigned(FCurrentEditPage));

  LCompositeField := TKExtFormCompositeField.Create(FCurrentEditPage);
  LCompositeField.EditItemId := AId;
  LCompositeField.Anchor := '-32';
  Result := LCompositeField;
end;

function TKExtLayoutProcessor.CreateRow(const AId: string): IKExtEditItem;
var
  LRow: TKExtFormRow;
begin
  Assert(Assigned(FCurrentEditPage));

  LRow := TKExtFormRow.Create(FCurrentEditPage);
  LRow.EditItemId := AId;
  LRow.LabelAlign := FCurrentLabelAlign;
  LRow.LabelWidth := FCurrentLabelWidth;
  LRow.HideLabels := FCurrentEditPage.HideLabels;
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
  else if SameText(ANode.Name, 'LabelSeparator') then
    FDefaults.LabelSeparator := ANode.AsString
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
var
  LConfigDefaultLayoutNode: TEFNode;
begin
  LConfigDefaultLayoutNode := TKWebApplication.Current.Config.Config.FindNode('Defaults/Layout');
  if Assigned(LConfigDefaultLayoutNode) then
  begin
    MemoWidth := LConfigDefaultLayoutNode.GetInteger('MemoWidth', LAYOUT_MEMOWIDTH);
    MaxFieldWidth := LConfigDefaultLayoutNode.GetInteger('MaxFieldWidth', LAYOUT_MAXFIELDWIDTH);
    MinFieldWidth := LConfigDefaultLayoutNode.GetInteger('MinFieldWidth', LAYOUT_MINFIELDWIDTH);
    MsgTarget := LAYOUT_MSGTARGET; // qtip title under side
    RequiredLabelTemplate := LConfigDefaultLayoutNode.GetString('RequiredLabelTemplate', LAYOUT_REQUIREDLABELTEMPLATE);
    LabelSeparator := LConfigDefaultLayoutNode.GetString('LabelSeparator', DEFAULT_LABEL_SEPARATOR);
  end
  else
  begin
    MemoWidth := LAYOUT_MEMOWIDTH;
    MaxFieldWidth := LAYOUT_MAXFIELDWIDTH;
    MinFieldWidth := LAYOUT_MINFIELDWIDTH;
    MsgTarget := LAYOUT_MSGTARGET;
    RequiredLabelTemplate := LAYOUT_REQUIREDLABELTEMPLATE;
    LabelSeparator := DEFAULT_LABEL_SEPARATOR;
  end;
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

function TKExtEditPage.GetEditItemId: string;
begin
  Result := FEditItemId;
end;

function TKExtEditPage.GetObjectNamePrefix: string;
begin
  Result := 'page';
end;

procedure TKExtEditPage.InitDefaults;
begin
  inherited;
  Border := False;
//  BodyStyle := 'background:none';
//  Layout := lyForm;
  // Leave room for the scroll bar on the right.
  PaddingString := '5px';
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
    FEditPanel.LabelAlign := OptionAsLabelAlign(ANode.AsString);
  end
  else if SameText(ANode.Name, 'LabelSeparator') then
    LabelSeparator := ANode.AsString
  else if SameText(ANode.Name, 'LabelPad') then
  begin
    Assert(Assigned(FEditPanel));
    FEditPanel.LabelPad := ANode.AsInteger;
  end
  else if SameText(ANode.Name, 'ImageName') then
    IconCls := TKWebApplication.Current.SetIconStyle('', ANode.AsString)
  else
    InvalidOption(ANode);
end;

procedure TKExtEditPage.SetTransientProperty(const APropertyName: string; const AValue: Variant);
begin
  SetComponentTransientProperty(Self, APropertyName, AValue);
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

function TKExtFormFieldSet.GetEditItemId: string;
begin
  Result := FEditItemId;
end;

procedure TKExtFormFieldSet.InitDefaults;
begin
  inherited;
  &On('expand', GenerateAnonymousFunction('this.getTopOwner().updateLayout();'), Self);
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
  if SameText(ANode.Name, 'LabelAlign') then
    LabelAlign := OptionAsLabelAlign(ANode.AsString)
  else if SameText(ANode.Name, 'LabelWidth') then
    LabelWidth := ANode.AsInteger
  else if SameText(ANode.Name, 'Collapsible') then
    Collapsible := ANode.AsBoolean
  else if SameText(ANode.Name, 'Collapsed') then
  begin
    // We need to defer expanding/collapsing the field set to give
    // compound contained editors (such as the file editors) a chance to
    // layout correctly.
    if ANode.AsBoolean then
      &On('afterrender', GenerateAnonymousFunction(Collapse(True)))
    else
      &On('afterrender', GenerateAnonymousFunction(Expand(True)));
  end
  else if SameText(ANode.Name, 'Title') then
    UnexpandedTitle := ANode.AsExpandedString
  else
    InvalidOption(ANode);
end;

procedure TKExtFormFieldSet.SetTransientProperty(const APropertyName: string; const AValue: Variant);
begin
  if SameText(APropertyName, 'Collapsed') then
  begin
    if AValue then
      Collapse(True)
    else
      Expand(True);
  end
  else
    SetComponentTransientProperty(Self, APropertyName, AValue);
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

function TKExtFormCompositeField.GetEditItemId: string;
begin
  Result := FEditItemId;
end;

procedure TKExtFormCompositeField.RefreshValue;
begin
end;

procedure TKExtFormCompositeField.SetOption(const ANode: TEFNode);
begin
  if SameText(ANode.Name, 'Title') then
    FieldLabel := _(ANode.AsExpandedString)
  else
    InvalidOption(ANode);
end;

procedure TKExtFormCompositeField.SetTransientProperty(const APropertyName: string; const AValue: Variant);
begin
  SetComponentTransientProperty(Self, APropertyName, AValue);
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

function TKExtFormTextField.GetFieldName: string;
begin
  Result := FFieldName;
end;

function TKExtFormTextField.GetEditItemId: string;
begin
  Result := FRecordField.FieldName;
end;

function TKExtFormTextField.GetRecordField: TKViewTableField;
begin
  Result := FRecordField;
end;

procedure TKExtFormTextField.RefreshValue;
var
  LJSONValue: string;
begin
  LJSONValue := JSONNullToEmptyStr(FRecordField.GetAsJSONValue(False, False));
  SetValue(LJSONValue);
end;

procedure TKExtFormTextField.SetReadOnly(const AValue: Boolean);
begin
  ReadOnly := AValue;
end;

procedure TKExtFormTextField.SetRecordField(const AValue: TKViewTableField);
begin
  FRecordField := AValue;
  if not ReadOnly and IsChangeHandlerNeeded(FRecordField) then
    OnChange := FieldChange;
end;

procedure TKExtFormTextField.SetTransientProperty(const APropertyName: string; const AValue: Variant);
begin
  AsExtFormField.SetTransientProperty(APropertyName, AValue);
end;

procedure TKExtFormTextField.StoreValue(const AObjectName: string);
begin
  AsExtFormField.StoreValue(AObjectName);
end;

procedure TKExtFormTextField.FieldChange(This: TExtFormField; NewValue: string;
  OldValue: string);
begin
  FRecordField.SetAsJSONValue(NewValue, False, TKWebApplication.Current.Config.UserFormatSettings);
end;

procedure TKExtFormTextField.SetFieldName(const AValue: string);
begin
  FFieldName := AValue;
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
  FRecordField.SetAsJSONValue(NewValue, False, TKWebApplication.Current.Config.UserFormatSettings);
end;

function TKExtFormTextArea.GetFieldName: string;
begin
  Result := FFieldName;
end;

function TKExtFormTextArea.GetEditItemId: string;
begin
  Result := FRecordField.FieldName;
end;

function TKExtFormTextArea.GetRecordField: TKViewTableField;
begin
  Result := FRecordField;
end;

procedure TKExtFormTextArea.RefreshValue;
begin
  SetValue(JSONNullToEmptyStr(FRecordField.GetAsJSONValue(False, False)));
end;

procedure TKExtFormTextArea.SetReadOnly(const AValue: Boolean);
begin
  ReadOnly := AValue;
end;

procedure TKExtFormTextArea.SetRecordField(const AValue: TKViewTableField);
begin
  FRecordField := AValue;
  if not ReadOnly and IsChangeHandlerNeeded(FRecordField) then
    OnChange := FieldChange;
end;

procedure TKExtFormTextArea.SetTransientProperty(const APropertyName: string; const AValue: Variant);
begin
  AsExtFormField.SetTransientProperty(APropertyName, AValue);
end;

procedure TKExtFormTextArea.StoreValue(const AObjectName: string);
begin
  AsExtFormField.StoreValue(AObjectName);
end;

procedure TKExtFormTextArea.SetFieldName(const AValue: string);
begin
  FFieldName := AValue;
end;

procedure TKExtFormTextArea.SetOption(const ANode: TEFNode);
begin
  if not SetExtFormFieldOption(AsExtFormField, ANode) then
  begin
    if SameText(ANode.Name, 'Lines') then
      HeightFunc := LinesToPixels(ANode.AsInteger)
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
  FRecordField.SetAsJSONValue(NewValue, False, TKWebApplication.Current.Config.UserFormatSettings);
end;

function TKExtFormCheckbox.GetFieldName: string;
begin
  Result := FFieldName;
end;

function TKExtFormCheckbox.GetEditItemId: string;
begin
  Result := FRecordField.FieldName;
end;

function TKExtFormCheckbox.GetRecordField: TKViewTableField;
begin
  Result := FRecordField;
end;

procedure TKExtFormCheckbox.RefreshValue;
begin
  SetValue(JSONNullToEmptyStr(FRecordField.GetAsJSONValue(False, False)));
end;

procedure TKExtFormCheckbox.SetReadOnly(const AValue: Boolean);
begin
  ReadOnly := AValue;
end;

procedure TKExtFormCheckbox.SetRecordField(const AValue: TKViewTableField);
begin
  FRecordField := AValue;
  if not ReadOnly and IsChangeHandlerNeeded(FRecordField) then
    OnChange := FieldChange;
end;

procedure TKExtFormCheckbox.SetTransientProperty(const APropertyName: string; const AValue: Variant);
begin
  AsExtFormField.SetTransientProperty(APropertyName, AValue);
end;

procedure TKExtFormCheckbox.StoreValue(const AObjectName: string);
begin
  if not ReadOnly then
    TKWebResponse.Current.Items.ExecuteJSCode(
      AObjectName + '["' + Name + '"]=' + JSName + '.checked;');
end;

procedure TKExtFormCheckbox.SetFieldName(const AValue: string);
begin
  FFieldName := AValue;
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

function TKExtFormDateField.ConvertDate(const AValue: string): string;
var
  LDate: TDate;
begin
  if AValue <> '' then
  begin
    LDate := DateOf(TJS.JSDateToDateTime(AValue));
    Result := DateToStr(LDate, TKWebApplication.Current.Config.UserFormatSettings);
  end
  else
    Result := '';
end;

procedure TKExtFormDateField.FieldChange(This: TExtFormField; NewValue, OldValue: string);
begin
  FRecordField.SetAsJSONValue(ConvertDate(NewValue), False, TKWebApplication.Current.Config.UserFormatSettings);
end;

function TKExtFormDateField.GetFieldName: string;
begin
  Result := FFieldName;
end;

function TKExtFormDateField.GetEditItemId: string;
begin
  Result := FRecordField.FieldName;
end;

function TKExtFormDateField.GetRecordField: TKViewTableField;
begin
  Result := FRecordField;
end;

procedure TKExtFormDateField.RefreshValue;
begin
  SetValue(JSONNullToEmptyStr(FRecordField.GetAsJSONValue(False, False)));
end;

procedure TKExtFormDateField.SetReadOnly(const AValue: Boolean);
begin
  ReadOnly := AValue;
end;

procedure TKExtFormDateField.SetRecordField(const AValue: TKViewTableField);
begin
  FRecordField := AValue;
  if not ReadOnly and IsChangeHandlerNeeded(FRecordField) then
    OnChange := FieldChange;
end;

procedure TKExtFormDateField.SetTransientProperty(const APropertyName: string; const AValue: Variant);
begin
  AsExtFormField.SetTransientProperty(APropertyName, AValue);
end;

procedure TKExtFormDateField.StoreValue(const AObjectName: string);
begin
  AsExtFormField.StoreValue(AObjectName);
end;

procedure TKExtFormDateField.SetFieldName(const AValue: string);
begin
  FFieldName := AValue;
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

function TKExtFormComboBoxEditor.GetFieldName: string;
begin
  Result := FFieldName;
end;

function TKExtFormComboBoxEditor.GetEditItemId: string;
begin
  Result := FRecordField.FieldName;
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
  LDBConnection: TEFDBConnection;
  LDBQuery: TEFDBQuery;
begin
  Assert(Assigned(FServerStore));

  LDBConnection := TKConfig.Instance.CreateDBConnection(FRecordField.ViewField.Table.DatabaseName);
  try
    LDBQuery := LDBConnection.CreateDBQuery;
    try
      TKSQLBuilder.CreateAndExecute(
        procedure (ASQLBuilder: TKSQLBuilder)
        begin
          ASQLBuilder.BuildLookupSelectStatement(FRecordField.ViewField, LDBQuery,
            ReplaceStr(ParamAsString('query'), '''', ''''''), FRecordField.ParentRecord);
        end);
      FServerStore.Load(LDBQuery);
    finally
      FreeAndNil(LDBQuery);
    end;
  finally
    FreeAndNil(LDBConnection);
  end;

  LStart := ParamAsInteger('start');
  LLimit := ParamAsInteger('limit');
  LPageRecordCount := Min(LLimit, FServerStore.RecordCount - LStart);

  TKWebResponse.Current.Items.AddJSON('{Total: ' + IntToStr(FServerStore.RecordCount)
    + ', Root: ' + FServerStore.GetAsJSON(False, LStart, LPageRecordCount) + '}');
end;

procedure TKExtFormComboBoxEditor.InitDefaults;
begin
  inherited;
  TriggerAction := 'all';
  TypeAhead := True;
  LazyRender := True;
  SelectOnFocus := False;
  ForceSelection := True;
end;

procedure TKExtFormComboBoxEditor.RefreshValue;
var
  LKeyFieldNames: string;
  LValue: string;
begin
  if Mode = 'local' then
    SetValue(JSONNullToEmptyStr(FRecordField.GetAsJSONValue(False, False)))
  else
  begin
    LKeyFieldNames := Join(FRecordField.ViewField.ModelField.GetFieldNames, TKConfig.Instance.MultiFieldSeparator);
    LValue := JSONNullToEmptyStr(FRecordField.ParentRecord.FieldByName(LKeyFieldNames).GetAsJSONValue(False, False));
    SetValue(LValue);
    SetRawValue(JSONNullToEmptyStr(FRecordField.GetAsJSONValue(False, False)));
{ TODO : needed? }
//    ApplyEmptyText;

    // Force the combo to refresh its list at next drop down.
    Store.RemoveAll();
    TKWebResponse.Current.Items.ExecuteJSCode(Format('delete %s.lastQuery;', [JSName]));

    // Provide the display value to set when the user types an invalid value
    // and the store is not loaded yet.
    TKWebResponse.Current.Items.ExecuteJSCode(Format('%s.lastSelectionText = %s.getRawValue();', [JSName, JSName]));
  end;
  TKWebResponse.Current.Items.ExecuteJSCode(JSName + '.kitto$isChanged = false;');
end;

procedure TKExtFormComboBoxEditor.SetReadOnly(const AValue: Boolean);
begin
  ReadOnly := AValue;
end;

procedure TKExtFormComboBoxEditor.SetRecordField(const AValue: TKViewTableField);
begin
  FRecordField := AValue;
  if not ReadOnly then
  begin
    if IsChangeHandlerNeeded(FRecordField) then
      &On('change', GenerateAnonymousFunction(GetJSCode(
        procedure
        begin
          TKWebResponse.Current.Items.ExecuteJSCode(
            'var json = new Object;' + sLineBreak +
            'json.new = new Object;');
          StoreValue('json.new');
          TKWebResponse.Current.Items.AjaxCallMethod(Self).SetMethod(ValueChanged)
            .Post('json')
            .AsExpression;
        end)));
    &On('select', GenerateAnonymousFunction(JSName + '.kitto$isChanged = true;'));
    &On('change', GenerateAnonymousFunction('field, newValue, oldValue',
      'if (newValue!==oldValue) ' + JSName + '.kitto$isChanged = true;'));
  end;
end;

procedure TKExtFormComboBoxEditor.SetTransientProperty(const APropertyName: string; const AValue: Variant);
begin
  AsExtFormField.SetTransientProperty(APropertyName, AValue);
end;

procedure TKExtFormComboBoxEditor.ValueChanged;
var
  LNewValues: TEFNode;
  LKeyFieldNames: string;
begin
  LNewValues := TKWebRequest.Current.JSONContentTree.ChildByName('new');
  if LNewValues.ChildCount > 0 then
  begin
    if Mode = 'local' then
      FRecordField.SetAsJSONValue(LNewValues.GetString(HiddenName), False, TKWebApplication.Current.Config.UserFormatSettings)
    else
    begin
      LKeyFieldNames := Join(FRecordField.ViewField.ModelField.GetFieldNames, TKConfig.Instance.MultiFieldSeparator);
      FRecordField.ParentRecord.FieldByName(LKeyFieldNames).SetAsJSONValue(LNewValues.GetString(HiddenName), False, TKWebApplication.Current.Config.UserFormatSettings);
    end;
  end;
end;

procedure TKExtFormComboBoxEditor.SetFieldName(const AValue: string);
begin
  FFieldName := AValue;
end;

procedure TKExtFormComboBoxEditor.SetOption(const ANode: TEFNode);
begin
  if not SetExtFormFieldOption(AsExtFormField, ANode) then
  begin
    if SameText(ANode.Name, 'Resizable') then
      ListConfig.SetConfigItem('resizable', ANode.AsBoolean)
    else
      InvalidOption(ANode);
  end;
end;

procedure TKExtFormComboBoxEditor.Setup(const AViewField: TKVIewField; const AIsReadOnly: Boolean;
  const AFieldCharWidth: Integer);
var
  LAllowedValues: TEFPairs;
  I: Integer;
  LProxy: TExtDataAjaxProxy;
  LReader: TExtDataJsonReader;
begin
  Assert(Assigned(AViewField));

  // Enable the combo box to post its hidden value instead of the visible description.
  // We don't use post, but we do need the name in order to get both key and
  // visible description on the server.
  HiddenName := AViewField.FieldNamesForUpdate;

  TypeAhead := True;
  MinChars := AViewField.AutoCompleteMinChars;
  if AViewField.IsReference then
  begin
    FListMode := Lookup;
    // Now both small and large referenced models use a remote combobox.
    if AViewField.IsReference {and AViewField.ModelField.ReferencedModel.IsLarge} then
    begin
      Mode := 'remote';
      FreeAndNil(FServerStore);
      FServerStore := AViewField.CreateReferenceStore;
      Store := TExtDataStore.CreateInline(Self);
      LProxy := TExtDataAjaxProxy.CreateInline(Store);
      LProxy.Url := GetMethodURL(GetRecordPage);
      Store.Proxy := LProxy;
      LReader := TExtDataJsonReader.CreateInline(Store);
      LProxy.Reader := LReader;
      LReader.RootProperty := 'Root';
      LReader.TotalProperty := 'Total';
      for I := 0 to FServerStore.Header.FieldCount - 1 do
        with TExtDataField.CreateInlineAndAddToArray(Store.Proxy.Reader.Fields) do
          Name := FServerStore.Header.Fields[I].FieldName;
      ValueField := Join(FServerStore.Key.GetFieldNames, TKConfig.Instance.MultiFieldSeparator);
      DisplayField := AViewField.ModelField.ReferencedModel.CaptionField.FieldName;
      if AViewField.ModelField.ReferencedModel.IsLarge then
        PageSize := 100;
      MinListWidth := 250; // Enough to accomodate all buttons.
      ListConfig.SetConfigItem('resizable', True);
//      MinHeightFunc := LinesToPixels(5);
    end;
  end
  else
  begin
    Mode := 'local';
    FListMode := Fixed;
    LAllowedValues := AViewField.GetChildrenAsPairs('AllowedValues', True);
    Assert(Length(LAllowedValues) > 0); // see SupportsViewField method.
    // Translate allowed value descriptions if needed.
    for I := Low(LAllowedValues) to High(LAllowedValues) do
      LAllowedValues[I].Value := _(LAllowedValues[I].Value);

    StoreArray := JSArray(PairsToJSON(LAllowedValues));
    ValueField := AViewField.FieldNamesForUpdate;
  end;

  if AIsReadOnly then
    ReadOnly := True;

  if AViewField.IsRequired then
    AllowBlank := False;
end;

procedure TKExtFormComboBoxEditor.StoreValue(const AObjectName: string);
var
  LCode: string;
begin
  if not ReadOnly then
  begin
    LCode :=
      AObjectName + '["' + HiddenName + '"]=' + GetJSCode(
        procedure
        begin
          GetValue;
        end) + ';';

    if (FListMode = Lookup) and (HiddenName <> Name) then
      LCode := LCode + sLineBreak +
        AObjectName + '["' + Name + '"]=' + GetJSCode(
          procedure
          begin
            GetRawValue;
          end) + ';';

    TKWebResponse.Current.Items.ExecuteJSCode(
      'if (' + JSName + '.kitto$isChanged == true) {' + sLineBreak +
      LCode + sLineBreak +
      '}');
  end;
end;

class function TKExtFormComboBoxEditor.SupportsViewField(const AViewField: TKViewField): Boolean;
var
  LAllowedValues: TEFPairs;
begin
  Assert(Assigned(AViewField));

  if AViewField.IsReference then
    Result := True
  else
  begin
    LAllowedValues := AViewField.GetChildrenAsPairs('AllowedValues');
    Result := Length(LAllowedValues) > 0;
  end;
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

function TKExtFormContainer.GetEditItemId: string;
begin
  Result := FEditItemId;
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
    WidthExpression := CharsToPixels(ANode.AsInteger)
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

procedure TKExtFormContainer.SetTransientProperty(const APropertyName: string; const AValue: Variant);
begin
  SetComponentTransientProperty(Self, APropertyName, AValue);
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

function TKExtFormRow.GetObjectNamePrefix: string;
begin
  Result := 'row';
end;

procedure TKExtFormRow.InitDefaults;
begin
  inherited;
  Layout := lyColumn;
end;

{ TKExtFormRowField }

function TKExtFormRowField.AsExtFormField: TExtFormField;
begin
  Assert(Assigned(FEditor));

  Result := FEditor.AsExtFormField;
end;

procedure TKExtFormRowField.InitDefaults;
begin
  inherited;
  Layout := lyFit;
  Padding := '0 10 0 0';
end;

function TKExtFormRowField.InternalSetOption(const ANode: TEFNode): Boolean;
begin
  Assert(Assigned(FEditor));

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
  if Assigned(FRecordField) then
    FEditor.RecordField := FRecordField;
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

function TKExtFormRowField.GetFieldName: string;
begin
  Assert(Assigned(FEditor));

  Result := FEditor.FieldName;
end;

function TKExtFormRowField.GetObjectNamePrefix: string;
begin
  Result := 'rowfld';
end;

function TKExtFormRowField.GetEditItemId: string;
begin
  Result := FEditItemId;
end;

function TKExtFormRowField.GetRecordField: TKViewTableField;
begin
  Result := FRecordField;
end;

procedure TKExtFormRowField.SetCharWidth(const AValue: Integer);
begin
  FCharWidth := AValue;
  WidthExpression := CharsToPixels(FCharWidth, 5);
end;

procedure TKExtFormRowField.SetLabelWidth(const AValue: Integer);
begin
  FLabelWidth := AValue;
  if AValue <> 0 then
    WidthExpression := CharsToPixels(FCharWidth, FLabelWidth + 10)
  else
    WidthExpression := CharsToPixels(FCharWidth, 5);
end;

procedure TKExtFormRowField.SetFieldName(const AValue: string);
begin
  Assert(Assigned(FEditor));

  FEditor.FieldName := AValue;
end;

procedure TKExtFormRowField.SetReadOnly(const AValue: Boolean);
begin
  Assert(Assigned(FEditor));

  FEditor.SetReadOnly(AValue);
end;

procedure TKExtFormRowField.SetRecordField(const AValue: TKViewTableField);
begin
  FRecordField := AValue;

  if Assigned(FEditor) then
    FEditor.RecordField := FRecordField;
end;

procedure TKExtFormRowField.SetTransientProperty(const APropertyName: string; const AValue: Variant);
begin
  SetComponentTransientProperty(Self, APropertyName, AValue);
end;

procedure TKExtFormRowField.StoreValue(const AObjectName: string);
begin
  Assert(Assigned(FEditor));

  FEditor.StoreValue(AObjectName);
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
  FRecordField.SetAsJSONValue(NewValue, False, TKWebApplication.Current.Config.JSFormatSettings);
end;

function TKExtFormNumberField.GetFieldName: string;
begin
  Result := FFieldName;
end;

function TKExtFormNumberField.GetEditItemId: string;
begin
  Result := FRecordField.FieldName;
end;

function TKExtFormNumberField.GetRecordField: TKViewTableField;
begin
  Result := FRecordField;
end;

procedure TKExtFormNumberField.RefreshValue;
begin
  SetValue(JSONNullToEmptyStr(FRecordField.GetAsJSONValue(False, False)));
end;

procedure TKExtFormNumberField.SetReadOnly(const AValue: Boolean);
begin
  ReadOnly := AValue;
end;

procedure TKExtFormNumberField.SetRecordField(const AValue: TKViewTableField);
begin
  FRecordField := AValue;
  if not ReadOnly and IsChangeHandlerNeeded(FRecordField) then
    OnChange := FieldChange;
end;

procedure TKExtFormNumberField.SetThousandSeparator(const AValue: string);
begin
  FThousandSeparator := SetConfigItem('thousandSeparator', AValue);
end;

procedure TKExtFormNumberField.SetTransientProperty(const APropertyName: string; const AValue: Variant);
begin
  AsExtFormField.SetTransientProperty(APropertyName, AValue);
end;

procedure TKExtFormNumberField.SetUseThousandSeparator(const AValue: Boolean);
begin
  FUseThousandSeparator := SetConfigItem('useThousandSeparator', AValue);
end;

procedure TKExtFormNumberField.StoreValue(const AObjectName: string);
begin
  AsExtFormField.StoreValue(AObjectName);
end;

procedure TKExtFormNumberField.SetAlwaysDisplayDecimals(const AValue: Boolean);
begin
  FAlwaysDisplayDecimals := SetConfigItem('alwaysDisplayDecimals', AValue);
end;

procedure TKExtFormNumberField.SetFieldName(const AValue: string);
begin
  FFieldName := AValue;
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

function TKExtFormDateTimeField.ConvertDateTime(const AValue: string): string;
var
  LDateTime: TDateTime;
begin
  if AValue <> '' then
  begin
    LDateTime := DateOf(TJS.JSDateToDateTime(AValue));
    Result := DateTimeToStr(LDateTime, TKWebApplication.Current.Config.UserFormatSettings);
  end
  else
    Result := '';
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
  FRecordField.SetAsJSONValue(ConvertDateTime(NewValue), False, TKWebApplication.Current.Config.UserFormatSettings);
end;

function TKExtFormDateTimeField.GetFieldName: string;
begin
  Result := FFieldName;
end;

function TKExtFormDateTimeField.GetObjectNamePrefix: string;
begin
  Result := 'datetimefld';
end;

function TKExtFormDateTimeField.GetEditItemId: string;
begin
  Result := FRecordField.FieldName;
end;

function TKExtFormDateTimeField.GetRecordField: TKViewTableField;
begin
  Result := FRecordField;
end;

class function TKExtFormDateTimeField.JSClassName: string;
begin
  Result := 'Ext.ux.DateTimeField';
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
  FAllowBlank := SetConfigItem('allowBlank', AValue);
end;

procedure TKExtFormDateTimeField.SetAltDateFormats(const AValue: string);
begin
  FAltDateFormats := SetConfigItem('altDateFormats', AValue);
end;

procedure TKExtFormDateTimeField.SetAltTimeFormats(const AValue: string);
begin
  FAltTimeFormats := SetConfigItem('altTimeFormats', AValue);
end;

procedure TKExtFormDateTimeField.SetDateFormat(const AValue: string);
begin
  FTimeFormat := SetConfigItem('dateFormat', AValue);
end;

procedure TKExtFormDateTimeField.SetFieldName(const AValue: string);
begin
  FFieldName := AValue;
end;

procedure TKExtFormDateTimeField.SetReadOnly(const AValue: Boolean);
begin
  ReadOnly := AValue;
end;

procedure TKExtFormDateTimeField.SetRecordField(const AValue: TKViewTableField);
begin
  FRecordField := AValue;
  if not ReadOnly and IsChangeHandlerNeeded(FRecordField) then
    OnChange := FieldChange;
end;

procedure TKExtFormDateTimeField.SetTimeFormat(const AValue: string);
begin
  FTimeFormat := SetConfigItem('timeFormat', AValue);
end;

procedure TKExtFormDateTimeField.SetTransientProperty(const APropertyName: string; const AValue: Variant);
begin
  AsExtFormField.SetTransientProperty(APropertyName, AValue);
end;

procedure TKExtFormDateTimeField.StoreValue(const AObjectName: string);
begin
  AsExtFormField.StoreValue(AObjectName);
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

function TKExtFormTimeField.ConvertTime(const AValue: string): string;
var
  LTime: TTime;
begin
  if AValue <> '' then
  begin
    LTime := TimeOf(TJS.JSDateToDateTime(AValue));
    Result := TimeToStr(LTime, TKWebApplication.Current.Config.UserFormatSettings);
  end
  else
    Result := '';
end;

procedure TKExtFormTimeField.FieldChange(This: TExtFormField; NewValue,
  OldValue: string);
begin
  FRecordField.SetAsJSONValue(ConvertTime(NewValue), False, TKWebApplication.Current.Config.UserFormatSettings);
end;

function TKExtFormTimeField.GetFieldName: string;
begin
  Result := FFieldName;
end;

function TKExtFormTimeField.GetEditItemId: string;
begin
  Result := FRecordField.FieldName;
end;

function TKExtFormTimeField.GetRecordField: TKViewTableField;
begin
  Result := FRecordField;
end;

procedure TKExtFormTimeField.RefreshValue;
begin
  SetValue(JSONNullToEmptyStr(FRecordField.GetAsJSONValue(False, False)));
end;

procedure TKExtFormTimeField.SetReadOnly(const AValue: Boolean);
begin
  ReadOnly := AValue;
end;

procedure TKExtFormTimeField.SetRecordField(const AValue: TKViewTableField);
begin
  FRecordField := AValue;
  if not ReadOnly and IsChangeHandlerNeeded(FRecordField) then
    OnChange := FieldChange;
end;

procedure TKExtFormTimeField.SetTransientProperty(const APropertyName: string; const AValue: Variant);
begin
  AsExtFormField.SetTransientProperty(APropertyName, AValue);
end;

procedure TKExtFormTimeField.StoreValue(const AObjectName: string);
begin
  AsExtFormField.StoreValue(AObjectName);
end;

procedure TKExtFormTimeField.SetFieldName(const AValue: string);
begin
  FFieldName := AValue;
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

function TKExtFormFileUploadField.GetFieldName: string;
begin
  Result := FFieldName;
end;

function TKExtFormFileUploadField.GetEditItemId: string;
begin
  Result := FRecordField.FieldName;
end;

function TKExtFormFileUploadField.GetRecordField: TKViewTableField;
begin
  Result := FRecordField;
end;

procedure TKExtFormFileUploadField.RefreshValue;
begin
  SetValue(JSONNullToEmptyStr(FRecordField.GetAsJSONValue(False, False)));
end;

procedure TKExtFormFileUploadField.SetReadOnly(const AValue: Boolean);
begin
  ReadOnly := AValue;
end;

procedure TKExtFormFileUploadField.SetRecordField(const AValue: TKViewTableField);
begin
  FRecordField := AValue;
end;

procedure TKExtFormFileUploadField.SetTransientProperty(const APropertyName: string; const AValue: Variant);
begin
  AsExtFormField.SetTransientProperty(APropertyName, AValue);
end;

procedure TKExtFormFileUploadField.StoreValue(const AObjectName: string);
begin
  AsExtFormField.StoreValue(AObjectName);
end;

procedure TKExtFormFileUploadField.SetFieldName(const AValue: string);
begin
  FFieldName := AValue;
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
  FLastUploadedClientFileName := '';

  LFileNameField := FRecordField.ViewField.FileNameField;
  if LFileNameField <> ''then
  begin
    LFileNameFieldReference := FRecordField.ParentRecord.FieldByName(LFileNameField);
    // Must clear the field both now, to have an exact picture in real time, and later
    // (through the SetToNull directive), when the record is persisted.
    //LFileNameFieldReference.SetBoolean('Sys/SetToNull', True);
    LFileNameFieldReference.SetToNull;
  end;
end;

procedure TKExtFormFileEditor.DownloadFieldData;
begin
  inherited;
  DoDownloadFieldData(GetDownloadFileName);
end;

procedure TKExtFormFileEditor.ProcessUploadedFile(const AFile: TAbstractWebRequestFile);
var
  LFileNameField: string;
  LFileNameFieldReference: TKViewTableField;
begin
  FLastUploadedClientFileName := AFile.FileName;
  LFileNameField := FRecordField.ViewField.FileNameField;
  if LFileNameField <> '' then
  begin
    LFileNameFieldReference := FRecordField.ParentRecord.FieldByName(LFileNameField);
    //LFileNameFieldReference.DeleteNode('Sys/SetToNull');
    LFileNameFieldReference.AsString := AFile.FileName;
  end;
end;

function TKExtFormFileEditor.GetRecordField: TKViewTableField;
begin
  Result := FRecordField;
end;

function TKExtFormFileEditor.GetStoredFileName: string;
var
  LFileNameField: string;
begin
  Assert(Assigned(FRecordField));

  LFileNameField := FRecordField.ViewField.FileNameField;
  if LFileNameField <> '' then
    Result := FRecordField.ParentRecord.FieldByName(LFileNameField).AsString
  else
    Result := '';
end;

function TKExtFormFileEditor.GetDefaultFileName: string;
var
  LCaptionField: TKViewTableField;
begin
  Assert(Assigned(FRecordField));

  Result := FRecordField.ViewField.GetExpandedString('DefaultFileName');
  if Result = '' then
  begin
    LCaptionField := FRecordField.ParentRecord.FindField(FRecordField.ViewField.ModelField.Model.CaptionField.FieldName);
    if Assigned(LCaptionField) then
      Result := LCaptionField.AsString + GetDownloadFileExtFromFieldData
    else
      Result := FRecordField.FieldName + GetDownloadFileExtFromFieldData;
  end;
end;

function TKExtFormFileEditor.GetDownloadFileName: string;
begin
  if FLastUploadedClientFileName <> '' then
    Result := ExtractFileName(FLastUploadedClientFileName)
  else
  begin
    Result := GetStoredFileName;
    if Result = '' then
      Result := GetDefaultFileName;
  end;
end;

function TKExtFormFileEditor.IsEmpty: Boolean;
begin
  Result := GetCurrentContentSize = 0;
end;

function TKExtFormFileEditor.IsPicture: Boolean;
begin
  Assert(Assigned(FRecordField));

  Result := FRecordField.ViewField.IsPicture;
end;

procedure TKExtFormFileEditor.RefreshValue;
begin
end;

procedure TKExtFormFileEditor.PictureViewAfterRender(This: TExtComponent);
begin
  Assert(Assigned(FPictureView));

  TKWebResponse.Current.Items.ExecuteJSCode(FPictureView.JSName + '.getLoader().load()');
end;

procedure TKExtFormFileEditor.CreateGUI(const AViewField: TKViewField);
var
  LPanel: TExtPanel;
  LToolbar: TKExtToolbar;
  LButtonCount: Integer;
  LIsPicture: Boolean;
begin
  LIsPicture := AViewField.IsPicture;

  LPanel := TExtPanel.CreateInlineAndAddToArray(Items);
  FImageWidth := AViewField.GetInteger('IsPicture/Thumbnail/Width', 100);
  FImageHeight := AViewField.GetInteger('IsPicture/Thumbnail/Height', 100);

  if LIsPicture then
  begin
    LPanel.Layout := lyColumn;
    FPictureView := TExtPanel.CreateAndAddToArray(LPanel.Items);
    //FPictureView.Frame := True;
    FPictureView.Border := False;
    FPictureView.Loader.SetConfigItem('url', GetMethodURL(GetImageMarkup));
    FPictureView.AfterRender := PictureViewAfterRender;

    LToolbar := TKExtToolbar.CreateAndAddToArray(LPanel.Items);
    // Version below puts the toolbar at the bottom (in which case we should adjust the height as well)
    //LToolbar := TKExtToolbar.Create;
    //FPictureView.Bbar := LToolbar;
  end
  else
  begin
    LPanel.Layout := lyHbox;
    FDescriptionField := TExtFormTextField.CreateAndAddToArray(LPanel.Items);
    FDescriptionField.ReadOnly := True;
    FDescriptionField.Cls := 'x-form-readonly';

    LToolbar := TKExtToolbar.CreateInlineAndAddToArray(LPanel.Items);
  end;

  LToolbar.Style := 'background: none; border: none;';

  FDownloadButton := TKExtButton.CreateAndAddToArray(LToolbar.Items);
  FDownloadButton.SetIconAndScale('download');
  FDownloadButton.Tooltip := _('Download file');
  //FDownloadButton.Handler := Ajax(StartDownload);
  FDownloadButton.Handler := TKWebResponse.Current.Items.AjaxCallMethod(Self).SetMethod(StartDownload).AsFunction;

  LButtonCount := 1;
  FUploadButton := TKExtButton.CreateAndAddToArray(LToolbar.Items);
  FUploadButton.SetIconAndScale('upload');
  FUploadButton.Tooltip := _('Upload file');
  //FUploadButton.Handler := Ajax(ShowUploadFileDialog);
  FUploadButton.Handler := TKWebResponse.Current.Items.AjaxCallMethod(Self).SetMethod(ShowUploadFileDialog).AsFunction;
  if FIsReadOnly then
    FUploadButton.SetDisabled(True);
  Inc(LButtonCount);

  FClearButton := TKExtButton.CreateAndAddToArray(LToolbar.Items);
  FClearButton.SetIconAndScale('clear');
  FClearButton.Tooltip := _('Clear field');
  //FClearButton.Handler := Ajax(Clear);
  FClearButton.Handler := TKWebResponse.Current.Items.AjaxCallMethod(Self).SetMethod(Clear).AsFunction;
  if FIsReadOnly then
    FClearButton.SetDisabled(True);
  Inc(LButtonCount);

  if Assigned(FDescriptionField) then
    // Keep 3 characters per button, leave the rest to the text field.
    FDescriptionField.WidthExpression := CharsToPixels(FTotalCharWidth - (3 * LButtonCount))
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

procedure TKExtFormFileEditor.SetReadOnly(const AValue: Boolean);
begin
  FIsReadOnly := AValue;
  UpdateGUI(False);
end;

procedure TKExtFormFileEditor.SetRecordField(const AValue: TKViewTableField);
begin
  FRecordField := AValue;
  UpdateGUI(False);
end;

procedure TKExtFormFileEditor.SetTotalCharWidth(const AValue: Integer);
begin
  FTotalCharWidth := AValue;
end;

procedure TKExtFormFileEditor.SetTransientProperty(const APropertyName: string; const AValue: Variant);
begin
  SetComponentTransientProperty(Self, APropertyName, AValue);
end;

procedure TKExtFormFileEditor.GetImageMarkup;
begin
  Assert(Assigned(FRecordField));

  if FRecordField.IsNull then
    TKWebResponse.Current.Items.AddHTML('<p>' + _(EMPTY_DESCRIPTION) + '</p>')
  else
    // Add dummy parameter to the URL to force the browser to refresh the image
    // after an upload.
    TKWebResponse.Current.Items.AddHTML(Format('<img src="%s">',
      [GetMethodURL(GetImageData) + '?_dc=' + FormatDateTime('yyyymmddhhnnsszzz', Now())]));
end;

function TKExtFormFileEditor.GetObjectNamePrefix: string;
begin
  Result := 'fileed';
end;

procedure TKExtFormFileEditor.UpdateGUI(const AUpdatePicture: Boolean);
var
  LIsEmpty: Boolean;
begin
  // Only if GUI already created.
  if Items.Count > 0 then
  begin
    LIsEmpty := IsEmpty;
    if Assigned(FDescriptionField) then
      FDescriptionField.Value := GetContentDescription;
    if AUpdatePicture and Assigned(FPictureView) then
      PictureViewAfterRender(FPictureView);
    FDownloadButton.SetDisabled(LIsEmpty);
    FClearButton.SetDisabled(LIsEmpty or FIsReadOnly);
    FUploadButton.SetDisabled(FIsReadOnly);
  end;
end;

procedure TKExtFormFileEditor.SetFieldName(const AValue: string);
begin
  FFieldName := AValue;
end;

procedure TKExtFormFileEditor.SetOption(const ANode: TEFNode);
begin
  if SameText(ANode.Name, 'CharWidth') then
    TotalCharWidth := ANode.AsInteger
  else if SameText(ANode.Name, 'Anchor') then
    Anchor := ANode.AsString
  else
    InvalidOption(ANode);
end;

procedure TKExtFormFileEditor.ShowUploadFileDialog;
var
  LUploadButton: TKExtButton;
  LFormPanel: TExtFormFormPanel;
  LSubmitAction: TExtFormActionSubmit;
  LUploadFormField: TKExtFormFileUploadField;
  LToolbar: TKExtToolbar;
begin
  if Assigned(FWindow) then
    FWindow.Delete;
  FreeAndNil(FWindow);
  FWindow := TKExtModalWindow.Create(Self);
  FWindow.Width := 500;
  FWindow.Height := 200;
  FWindow.Closable := True;
  FWindow.Title := _('File upload');

  LFormPanel := TExtFormFormPanel.CreateAndAddToArray(FWindow.Items);
  LFormPanel.Region := rgCenter;
  LFormPanel.Frame := True;
  LFormPanel.FileUpload := True;
  LFormPanel.LabelAlign := laRight;
  LFormPanel.LabelWidth := 50;

  LUploadFormField := TKExtFormFileUploadField.CreateInlineAndAddToArray(LFormPanel.Items);
  LUploadFormField.FieldLabel := _(FRecordField.ViewField.DisplayLabel);
  LUploadFormField.EmptyText := _('Select a file to upload');
  LUploadFormField.AllowBlank := False;
  LUploadFormField.Anchor := '0 5 0 0';
  LToolbar := TKExtToolbar.Create(Self);
  TExtToolbarFill.CreateInlineAndAddToArray(LToolbar.Items);
  LFormPanel.Fbar := LToolbar;
  LUploadButton := TKExtButton.CreateInlineAndAddToArray(LToolbar.Items);
  LUploadButton.Text := _('Upload');
  LUploadButton.SetIconAndScale('Upload', IfThen(TKWebRequest.Current.IsMobileBrowser,'medium', 'small'));

  LSubmitAction := TExtFormActionSubmit.CreateInline(FWindow);
  LSubmitAction.Url := GetMethodURL(Upload);
  LSubmitAction.WaitMsg := _('File upload in progress...');
  LSubmitAction.WaitTitle := _('Please wait...');
  //LSubmitAction.Success := Ajax(PostUpload);
  LSubmitAction.Success := TKWebResponse.Current.Items.AjaxCallMethod(Self).SetMethod(PostUpload).AsFunction;
  { TODO : find a way to substitute action.result.msg }
  LSubmitAction.Failure := GenerateAnonymousFunction('form, action', ExtMessageBox.Alert(_('File upload error'), 'action.result.msg'));

  LUploadButton.Handler := GenerateAnonymousFunction(Format(
    'var form = this.up("form"); if (form.isValid()) form.submit({%s});',
    [LSubmitAction.JSConfig.AsFormattedText]));
  FWindow.Show;
end;

procedure TKExtFormFileEditor.StartDownload;
begin
  Download(DownloadFieldData);
end;

procedure TKExtFormFileEditor.StoreValue(const AObjectName: string);
var
  LMsg: string;
begin
  Assert(Assigned(FRecordField));

  if FRecordField.ViewField.IsRequired then
  begin
    LMsg := Format(_('Field %s is required. Please upload a file.'), [FRecordField.ViewField.DisplayLabel]);
    if Assigned(FDescriptionField) then
      TKWebResponse.Current.Items.ExecuteJSCode(Self,
        Format('if (%s.getValue() == "%s") { alert("%s"); throw "validation error"; }',
        [FDescriptionField.JSName, _(EMPTY_DESCRIPTION), LMsg]))
    else if Assigned(FPictureView) then
      TKWebResponse.Current.Items.ExecuteJSCode(Self,
        Format('if (%s.html.indexOf("<img" = -1) { alert("%s"); throw "validation error"; }', [FDescriptionField.JSName, LMsg]));
  end;
end;

procedure TKExtFormFileEditor.Upload;
var
  LResult: TJSONObject;
  LMaxUploadSize: Integer;
begin
  LResult := TJSONObject.Create;
  try
    if TKWebRequest.Current.Files.Count = 0 then
    begin
      LResult.AddPair('success', TJSONFalse.Create);
      LResult.AddPair('msg', _('No file uploaded'));
    end
    else
    begin
      LMaxUploadSize := FRecordField.ViewField.GetInteger('MaxUploadSize', MaxInt);
      if TKWebRequest.Current.Files[0].Stream.Size > LMaxUploadSize then
      begin
        LResult.AddPair('success', TJSONFalse.Create);
        LResult.AddPair('msg', Format(_('File too large. Maximum size is %s.'),
          [FormatByteSize(LMaxUploadSize, TKWebApplication.Current.Config.UserFormatSettings)]));
      end
      else
      begin
        try
          ProcessUploadedFile(TKWebRequest.Current.Files[0]);
          LResult.AddPair('success', TJSONTrue.Create);
        except
          on E: Exception do
          begin
            LResult.AddPair('success', TJSONFalse.Create);
            LResult.AddPair('msg', E.Message);
          end;
        end;
      end;
    end;
  finally
    TKWebResponse.Current.Items.AddJSON(LResult.ToJSON);
    FreeAndNil(LResult);
  end;
end;

function TKExtFormFileEditor.GetContentDescription: string;
var
  LFileName: string;
begin
  LFileName := ExtractFileName(GetDownloadFileName);
  if LFileName <> '' then
    Result := Format(_('%s file (%s)'),
      [StripPrefix(ExtractFileExt(LFileName), '.'),
      FormatByteSize(GetCurrentContentSize, TKWebApplication.Current.Config.UserFormatSettings)])
  else
    Result := _(EMPTY_DESCRIPTION);
end;

function TKExtFormFileEditor.GetFieldName: string;
begin
  Result := FFieldName;
end;

function TKExtFormFileEditor.GetEditItemId: string;
begin
  Result := FRecordField.FieldName;
end;

procedure TKExtFormFileEditor.GetImageData;
begin
  DownloadThumbnailedStream(CreateImageDataStream, GetDownloadFileName, FImageWidth, FImageHeight);
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

function TKExtFormFileBlobEditor.GetDownloadFileExtFromFieldData: string;
begin
  Assert(Assigned(FRecordField));

  Result := '.' + GetDataType(FRecordField.AsBytes, 'dat');
end;

procedure TKExtFormFileBlobEditor.ClearContents;
begin
  inherited;
  FRecordField.SetToNull;
end;

function TKExtFormFileBlobEditor.CreateImageDataStream: TStream;
begin
  Assert(Assigned(FRecordField));
  Assert(not FRecordField.IsNull);

  Result := TBytesStream.Create(FRecordField.AsBytes);
end;

procedure TKExtFormFileBlobEditor.DoDownloadFieldData(const AFileName: string);
begin
  Assert(Assigned(FRecordField));

  TKWebApplication.Current.DownloadBytes(FRecordField.AsBytes, AFileName, '', False);
end;

procedure TKExtFormFileBlobEditor.ProcessUploadedFile(const AFile: TAbstractWebRequestFile);
begin
  inherited;
  FRecordField.LoadBytesFromStream(AFile.Stream);
end;

function TKExtFormFileBlobEditor.GetCurrentContentSize: Integer;
begin
  if FRecordField.IsNull then
    Result := 0
  else
    Result := Length(FRecordField.AsBytes);
end;

{ TKFileOp }

class operator TKFileOp.Implicit(const AOther: TStringDynArray): TKFileOp;
begin
  Assert(Length(AOther) = 3);

  Result := TKFIleOp.Create(
    TRttiEnumerationType.GetValue<TKFileOpKind>(AOther[0])
    , AOther[1]
    , TRttiEnumerationType.GetValue<TKFileOpEvent>(AOther[2])
  );
end;

class operator TKFileOp.Implicit(const AOther: TKFileOp): TStringDynArray;
begin
  Result := [
    TRttiEnumerationType.GetName<TKFileOpKind>(AOther.Kind)
    , AOther.PathName
    , TRttiEnumerationType.GetName<TKFileOpEvent>(AOther.Event)
  ];
end;

constructor TKFileOp.Create(const AKind: TKFileOpKind; const APathName: string; const AEvent: TKFileOpEvent);
begin
  Kind := AKind;
  PathName := APathName;
  Event := AEvent;
  Done := False;
end;

{ TKExtFormFileReferenceEditor }

function TKExtFormFileReferenceEditor.GetFieldPath: string;
begin
  inherited;
  Result := IncludeTrailingPathDelimiter(FRecordField.ViewField.GetExpandedString('Path'));
  if (Result = '') or (Result = PathDelim) then
    raise Exception.CreateFmt('Path not specified for file reference field %s.', [FRecordField.ViewField.FieldName]);
  if not DirectoryExists(Result) then
    raise Exception.CreateFmt('Directory %s not found for file reference field %s.', [Result, FRecordField.ViewField.FieldName]);
end;

function TKExtFormFileReferenceEditor.GetServerFileName: string;
begin
  Assert(Assigned(FRecordField));

  if FRecordField.AsString <> '' then
    Result := TPath.Combine(GetFieldPath, FRecordField.AsString)
  else
    Result := '';
end;

procedure TKExtFormFileReferenceEditor.ClearContents;
begin
  inherited;
  if FRecordField.AsString <> '' then
  begin
    DeferredFileDelete(oePost, GetServerFileName);
    FRecordField.SetToNull;
  end;
end;

procedure TKExtFormFileReferenceEditor.DoDownloadFieldData(const AFileName: string);
var
  LServerFileName: string;
begin
  Assert(Assigned(FRecordField));

  LServerFileName := GetServerFileName;
  Assert(FileExists(LServerFileName));

  TKWebApplication.Current.DownloadFile(LServerFileName, AFileName, '', False);
end;

function TKExtFormFileReferenceEditor.CreateImageDataStream: TStream;
var
  LServerFileName: string;
begin
  Assert(Assigned(FRecordField));

  LServerFileName := GetServerFileName;
  Assert(FileExists(LServerFileName));

  Result := TFileStream.Create(LServerFileName, fmOpenRead or fmShareDenyNone);
end;

procedure TKExtFormFileReferenceEditor.DeferredFileDelete(const AEvent: TKFileOpEvent; const APathName: string);
var
  LNode: TEFNode;
begin
  Assert(Assigned(FRecordField));

  LNode := FRecordField.GetNode('DeferredFileOps', True);
  LNode.AddChild('Item').AsStringArray := TKFileOp.Create(okDelete, APathName, AEvent);
end;

procedure TKExtFormFileReferenceEditor.ProcessUploadedFile(const AFile: TAbstractWebRequestFile);
var
  LCurrentFileName: string;
  LNewFileName: string;
begin
  inherited;
  LCurrentFileName := GetServerFileName;
  if FileExists(LCurrentFileName) then
    DeferredFileDelete(oePost, LCurrentFileName);

  // Save data to file with auto-generated name.
  LNewFileName := GetUniqueFileName(GetFieldPath, ExtractFileExt(AFile.FileName));
  StreamToFile(AFile.Stream, LNewFileName);

  FRecordField.AsString := ExtractFileName(LNewFileName);
  DeferredFileDelete(oeCancel, LNewFileName);
end;

function TKExtFormFileReferenceEditor.GetCurrentContentSize: Integer;
var
  LFileName: string;
begin
  LFileName := GetServerFileName;
  if FileExists(LFileName) then
    Result := GetFileSize(LFileName)
  else
    Result := 0;
end;

function TKExtFormFileReferenceEditor.GetDownloadFileExtFromFieldData: string;
begin
  Assert(Assigned(FRecordField));

  Result := ExtractFileExt(FRecordField.AsString);
  if Result = '' then
    Result := '.dat';
end;

{ TKExtEditorManager }

function TKExtEditorManager.CreateGridCellEditor(const AOwner: TJSBase;
  const AViewField: TKViewField): TExtFormField;
begin
  Result := CreateEditor(AOwner, AViewField, nil, AViewField.DisplayWidth, False).AsExtFormField;
end;

function TKExtEditorManager.CreateEditor(const AOwner: TJSBase;
  const AViewField: TKViewField; const ARowField: TKExtFormRowField;
  const AFieldCharWidth: Integer; const AIsReadOnly: Boolean;
  const ALabel: string): IKExtEditor;
var
  LFormField: TExtFormField;
begin
  Result := TryCreateFileEditor(AOwner, AViewField, ARowField, AFieldCharWidth, AIsReadOnly, ALabel);
  if Result = nil then
    Result := TryCreateLookupEditor(AOwner, AViewField, ARowField, AFieldCharWidth, AIsReadOnly);
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
  Result.FieldName := AViewField.AliasedName;

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
    LFormField.Disabled := AIsReadOnly;
  end;
end;

function TKExtEditorManager.TryCreateLookupEditor(
  const AOwner: TJSBase; const AViewField: TKViewField;
  const ARowField: TKExtFormRowField; const AFieldCharWidth: Integer;
  const AIsReadOnly: Boolean): IKExtEditor;
var
  LLookupEdit: TKExtLookupEditor;
begin
  Assert(Assigned(AOwner));

  Result := nil;
  if not AViewField.IsDetailReference then
  begin
    if TKExtLookupEditor.SupportsViewField(AViewField) then
    begin
      LLookupEdit := TKExtLookupEditor.Create(AOwner);
      try
        LLookupEdit.Setup(AViewField, AIsReadOnly, AFieldCharWidth);
        if not Assigned(ARowField) then
          LLookupEdit.WidthExpression := LLookupEdit.CharsToPixels(AFieldCharWidth + TRIGGER_WIDTH)
        else
          ARowField.CharWidth := AFieldCharWidth + TRIGGER_WIDTH;
        Result := LLookupEdit;
      except
        LLookupEdit.Free;
        raise;
      end;
    end;
  end;
end;

function TKExtEditorManager.TryCreateComboBox(
  const AOwner: TJSBase; const AViewField: TKViewField;
  const ARowField: TKExtFormRowField;
  const AFieldCharWidth: Integer;
  const AIsReadOnly: Boolean): IKExtEditor;
var
  LComboBox: TKExtFormComboBoxEditor;
begin
  Assert(Assigned(AOwner));

  Result := nil;
  if not AViewField.IsDetailReference then
  begin
    if TKExtFormComboBoxEditor.SupportsViewField(AViewField) then
    begin
      LComboBox := TKExtFormComboBoxEditor.Create(AOwner);
      try
        LComboBox.Setup(AViewField, AIsReadOnly, AFieldCharWidth);
        if not Assigned(ARowField) then
          LComboBox.WidthExpression := LComboBox.CharsToPixels(AFieldCharWidth + TRIGGER_WIDTH)
        else
          ARowField.CharWidth := AFieldCharWidth + TRIGGER_WIDTH;
        Result := LComboBox;
      except
        LComboBox.Free;
        raise;
      end;
    end;
  end;
end;

function TKExtEditorManager.TryCreateTextArea(
  const AOwner: TJSBase; const AViewField: TKViewField;
  const ARowField: TKExtFormRowField; const AFieldCharWidth: Integer;
  const AIsReadOnly: Boolean): IKExtEditor;
var
  LTextArea: TKExtFormTextArea;
begin
  Assert(Assigned(AOwner));

  if AViewField.IsBlob or (AViewField.Size div SizeOf(Char) >= MULTILINE_EDIT_THRESHOLD)
    or AViewField.HasChild('EditLines') then
  begin
    LTextArea := TKExtFormTextArea.Create(AOwner);
    try
      if not Assigned(ARowField) then
        LTextArea.WidthExpression := LTextArea.CharsToPixels(AFieldCharWidth)
      else
        ARowField.CharWidth := AFieldCharWidth;
      LTextArea.HeightFunc := LTextArea.LinesToPixels(AViewField.GetInteger('EditLines', 5));
      LTextArea.AutoScroll := True;
      // Set this if it's the last field.
      //Anchor := '100%';
      if not AIsReadOnly then
      begin
        if AViewField.Size > 0 then
          LTextArea.MaxLength  := AViewField.Size;
        LTextArea.AllowBlank := not AViewField.IsRequired;
      end;
      LTextArea.Grow := False;
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
  const AOwner: TJSBase; const AViewField: TKViewField;
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
  const AOwner: TJSBase; const AViewField: TKViewField;
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
        LDateField.WidthExpression := LDateField.CharsToPixels(AFieldCharWidth + TRIGGER_WIDTH)
      else
        ARowField.CharWidth := AFieldCharWidth + TRIGGER_WIDTH;
      LFormat := AViewField.EditFormat;
      if LFormat = '' then
        LFormat := TKWebApplication.Current.Config.UserFormatSettings.ShortDateFormat;
      LDateField.Format := TJS.DelphiDateFormatToJSDateFormat(LFormat);
      LDateField.AltFormats := TJS.DelphiDateFormatToJSDateFormat(TKWebApplication.Current.Config.JSFormatSettings.ShortDateFormat);
      if not AIsReadOnly then
        LDateField.AllowBlank := not AViewField.IsRequired;
      if TKWebRequest.Current.IsMobileBrowser then
        LDateField.Editable := False;
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
  const AOwner: TJSBase; const AViewField: TKViewField;
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
        LTimeField.WidthExpression := LTimeField.CharsToPixels(AFieldCharWidth + TRIGGER_WIDTH)
      else
        ARowField.CharWidth := AFieldCharWidth + TRIGGER_WIDTH;

      LFormat := AViewField.EditFormat;
      if LFormat = '' then
        LFormat := TKWebApplication.Current.Config.UserFormatSettings.ShortTimeFormat;
      LTimeField.Format := TJS.DelphiTimeFormatToJSTimeFormat(LFormat);
      LTimeField.AltFormats := TJS.DelphiTimeFormatToJSTimeFormat(TKWebApplication.Current.Config.JSFormatSettings.ShortTimeFormat);
      if not AIsReadOnly then
        LTimeField.AllowBlank := not AViewField.IsRequired;
      if TKWebRequest.Current.IsMobileBrowser then
        LTimeField.Editable := False;
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
  const AOwner: TJSBase; const AViewField: TKViewField;
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
        LDateTimeField.WidthExpression := LDateTimeField.CharsToPixels(AFieldCharWidth + (2 * TRIGGER_WIDTH) + SPACER_WIDTH)
      else
        ARowField.CharWidth := AFieldCharWidth + (2 * TRIGGER_WIDTH) + SPACER_WIDTH;
      LFormats := Split(AViewField.EditFormat, ' ');
      if Length(LFormats) > 0 then
        LDateFormat := LFormats[0]
      else
        LDateFormat := TKWebApplication.Current.Config.UserFormatSettings.ShortDateFormat;
      if Length(LFormats) > 1 then
        LTimeFormat := LFormats[1]
      else
        LTimeFormat := TKWebApplication.Current.Config.UserFormatSettings.ShortTimeFormat;
      LDateTimeField.DateFormat := TJS.DelphiDateFormatToJSDateFormat(LDateFormat);
      LDateTimeField.AltDateFormats := TJS.DelphiDateFormatToJSDateFormat(TKWebApplication.Current.Config.JSFormatSettings.ShortDateFormat);
      LDateTimeField.TimeFormat := TJS.DelphiTimeFormatToJSTimeFormat(TKWebApplication.Current.Config.UserFormatSettings.ShortTimeFormat);
      LDateTimeField.AltTimeFormats := TJS.DelphiTimeFormatToJSTimeFormat(TKWebApplication.Current.Config.JSFormatSettings.ShortTimeFormat);
      if not AIsReadOnly then
        LDateTimeField.AllowBlank := not AViewField.IsRequired;
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
  const AOwner: TJSBase; const AViewField: TKViewField;
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
      LFileEditor.SetReadOnly(AIsReadOnly);
      LFileEditor.FieldLabel := ALabel;
      LFileEditor.TotalCharWidth := AFieldCharWidth - 1;
      if Assigned(ARowField) then
        ARowField.CharWidth := AFieldCharWidth;
      LFileEditor.CreateGUI(AViewField);
      Result := LFileEditor;
    except
      LFileEditor.Free;
      raise;
    end;
  end
  else
    Result := nil;
end;

function TKExtEditorManager.TryCreateNumberField(const AOwner: TJSBase;
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
        LNumberField.WidthExpression := LNumberField.CharsToPixels(AFieldCharWidth)
      else
        ARowField.CharWidth := AFieldCharWidth;

      LNumberField.AllowDecimals := AViewField.DataType is TEFDecimalNumericDataTypeBase;
      LNumberField.AllowNegative := True;
      if LNumberField.AllowDecimals then
        LNumberField.DecimalPrecision := AViewField.DecimalPrecision;
      LNumberField.AllowBlank := not AViewField.IsRequired;
      LNumberField.DecimalSeparator := TKWebApplication.Current.Config.UserFormatSettings.DecimalSeparator;
      LNumberField.ThousandSeparator := TKWebApplication.Current.Config.UserFormatSettings.ThousandSeparator;
      if (AViewField.EditFormat = '') or (Pos(AViewField.EditFormat, TKWebApplication.Current.Config.UserFormatSettings.ThousandSeparator) >= 1) then
        LNumberField.UseThousandSeparator := True
      else
        LNumberField.UseThousandSeparator := False;
      LNumberField.AlwaysDisplayDecimals := AViewField.DecimalPrecision <> 0;
      Result := LNumberField;
    except
      LNumberField.Free;
      raise;
    end;
  end
  else
    Result := nil;
end;

function TKExtEditorManager.CreateTextField(const AOwner: TJSBase;
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
      LTextField.WidthExpression := LTextField.CharsToPixels(AFieldCharWidth)
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

{ TExtFormFieldHelper }

procedure TExtFormFieldHelper.SetTransientProperty(const APropertyName: string; const AValue: Variant);
begin
  SetComponentTransientProperty(Self, APropertyName, AValue);
end;

procedure TExtFormFieldHelper.StoreValue(const AObjectName: string);
begin
  if not ReadOnly then
    TKWebResponse.Current.Items.ExecuteJSCode(
      AObjectName + '["' + Name + '"]=' + GetJSCode(
        procedure
        begin
          GetRawValue;
        end));
end;

{ TKEditItemList }

procedure TKEditItemList.EditorsByFieldName(const AFieldName: string;
  const AHandler: TProc<IKExtEditor>);
begin
  EnumEditors(
    function (AEditor: IKExtEditor): Boolean
    begin
      Result := SameText(AEditor.GetRecordField.ViewField.AliasedName, AFieldName);
    end,
    AHandler);
end;

procedure TKEditItemList.EditorsByViewField(const AViewField: TKVIewField;
  const AHandler: TProc<IKExtEditor>);
begin
  EnumEditors(
    function (AEditor: IKExtEditor): Boolean
    begin
      Result := AEditor.GetRecordField.ViewField = AViewField;
    end,
    AHandler);
end;

procedure TKEditItemList.EditItemsById(const AId: string; const AHandler: TProc<IKExtEditItem>);
begin
  EnumEditItems(
    function (AEditItem: IKExtEditItem): Boolean
    begin
      Result := SameText(AEditItem.GetEditItemId, AId);
    end,
    AHandler);
end;

procedure TKEditItemList.EditorsByField(const AField: TKField;
  const AHandler: TProc<IKExtEditor>);
begin
  EnumEditors(
    function (AEditor: IKExteditor): Boolean
    begin
      Result := AEditor.GetRecordField = AField;
    end,
    AHandler);
end;

procedure TKEditItemList.EnumEditors(const APredicate: TFunc<IKExtEditor, Boolean>;
  const AHandler: TProc<IKExtEditor>);
var
  I: Integer;
  LEditorIntf: IKExtEditor;
begin
  for I := 0 to Count - 1 do
  begin
    if Supports(Items[I], IKExtEditor, LEditorIntf) then
    begin
      if APredicate(LEditorIntf) then
        AHandler(LEditorIntf);
    end;
  end;
end;

procedure TKEditItemList.EnumEditItems(
  const APredicate: TFunc<IKExtEditItem, Boolean>;
  const AHandler: TProc<IKExtEditItem>);
var
  I: Integer;
  LEditItemIntf: IKExtEditItem;
begin
  for I := 0 to Count - 1 do
  begin
    if Supports(Items[I], IKExtEditItem, LEditItemIntf) then
    begin
      if APredicate(LEditItemIntf) then
        AHandler(LEditItemIntf);
    end;
  end;
end;

procedure TKEditItemList.AllEditItems(const AHandler: TProc<IKExtEditItem>);
begin
  EnumEditItems(
    function (AEditItem: IKExteditItem): Boolean
    begin
      Result := True;
    end,
    AHandler);
end;

procedure TKEditItemList.AllEditors(
  const AHandler: TProc<IKExtEditor>);
begin
  EnumEditors(
    function (AEditor: IKExtEditor): Boolean
    begin
      Result := True;
    end,
    AHandler);
end;

procedure TKEditItemList.AllNonEditors(
  const AHandler: TProc<IKExtEditItem>);
begin
  EnumEditItems(
    function (AEditItem: IKExteditItem): Boolean
    begin
      Result := not Supports(AEditItem, IKExtEditor);
    end,
    AHandler);
end;

{ TKExtLookupEditor }

function TKExtLookupEditor.AsExtFormField: TExtFormField;
begin
  Result := Self;
end;

function TKExtLookupEditor.AsObject: TObject;
begin
  Result := Self;
end;

procedure TKExtLookupEditor.ClearClick;
var
  LKeyFieldNames: string;
begin
  LKeyFieldNames := Join(FRecordField.ViewField.ModelField.GetFieldNames, TKConfig.Instance.MultiFieldSeparator);
  FRecordField.ParentRecord.FieldByName(LKeyFieldNames).SetToNull;
end;

function TKExtLookupEditor.GetEditItemId: string;
begin
  Result := FRecordField.FieldName;
end;

function TKExtLookupEditor.GetFieldName: string;
begin
  Result := FFieldName;
end;

function TKExtLookupEditor.GetRecordField: TKViewTableField;
begin
  Result := FRecordField;
end;

procedure TKExtLookupEditor.LookupConfirmed(const ARecord: TKViewTableRecord);
var
  LResultKeyValues: string;
  LKeyFieldNames: string;
begin
  inherited;
  LResultKeyValues := ARecord.GetFieldValuesAsString(FRecordField.ViewField.ModelField.ReferencedModel.GetKeyFieldNames, TKConfig.Instance.MultiFieldSeparator);
  LKeyFieldNames := Join(FRecordField.ViewField.ModelField.GetFieldNames, TKConfig.Instance.MultiFieldSeparator);
  FRecordField.ParentRecord.FieldByName(LKeyFieldNames).AsString := LResultKeyValues;
end;

procedure TKExtLookupEditor.RefreshValue;
var
  LKeyFieldNames: string;
  LValue: string;
begin
  LKeyFieldNames := Join(FRecordField.ViewField.ModelField.GetFieldNames, TKConfig.Instance.MultiFieldSeparator);
  LValue := JSONNullToEmptyStr(FRecordField.ParentRecord.FieldByName(LKeyFieldNames).GetAsJSONValue(False, False));
  SetValue(LValue);
  SetRawValue(JSONNullToEmptyStr(FRecordField.GetAsJSONValue(False, False)));
{ TODO : needed? }
//  ApplyEmptyText;
end;

procedure TKExtLookupEditor.SetFieldName(const AValue: string);
begin
  FFieldName := AValue;
end;

procedure TKExtLookupEditor.SetOption(const ANode: TEFNode);
begin
  if not SetExtFormFieldOption(AsExtFormField, ANode) then
    InvalidOption(ANode);
end;

procedure TKExtLookupEditor.SetReadOnly(const AValue: Boolean);
begin
  ReadOnly := AValue;
end;

procedure TKExtLookupEditor.SetRecordField(const AValue: TKViewTableField);
begin
  FRecordField := AValue;
  if Assigned(FRecordField) then
    SetViewField(FRecordField.ViewField)
  else
    SetViewField(nil);
end;

procedure TKExtLookupEditor.SetTransientProperty(const APropertyName: string; const AValue: Variant);
begin
  AsExtFormField.SetTransientProperty(APropertyName, AValue);
end;

procedure TKExtLookupEditor.Setup(const AViewField: TKVIewField;
  const AIsReadOnly: Boolean; const AFieldCharWidth: Integer);
begin
  Assert(Assigned(AViewField));

  FHiddenName := AViewField.FieldNamesForUpdate;

  if AIsReadOnly then
    ReadOnly := True;

  if AViewField.IsRequired then
    AllowBlank := False;
end;

procedure TKExtLookupEditor.StoreValue(const AObjectName: string);
var
  LCode: string;
begin
  if not ReadOnly then
  begin
    LCode :=
      AObjectName + '["' + FHiddenName + '"]=' + GetJSCode(
        procedure
        begin
          GetValue;
        end) + ';';

    if FHiddenName <> Name then
      LCode := LCode + sLineBreak +
        AObjectName + '["' + Name + '"]=' + GetJSCode(
          procedure
          begin
            GetRawValue;
          end) + ';';
  end;
end;

end.
