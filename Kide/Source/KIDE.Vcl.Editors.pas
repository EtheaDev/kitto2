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
unit KIDE.Vcl.Editors;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, Generics.Collections,
  System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Buttons,
  Kitto.Metadata.DataView, Kitto.Metadata.Views, Kitto.Ext.Editors, EF.Tree,
  Vcl.ComCtrls, MD_Label;

type
  TKideEditPage = class;
  TVclFormField = class;

  TVclFormFormPanelLabelAlign = (laLeft, laTop, laRight);

  IVclEditItem = interface
    ['{CE515007-34C6-4A50-9459-CF6EB6E4973A}']
    procedure SetOption(const ANode: TEFNode);
    function AsWinControl: TWinControl;
    procedure RefreshValue;
  end;

  IVclEditContainer = interface(IVclEditItem)
    ['{802136B7-523B-44A9-B3B8-CA044E16C790}']
    procedure AddChild(const AEditItem: IVclEditItem);
    procedure ResizeByContent;
  end;

  IVclEditor = interface(IVclEditItem)
    ['{67387224-FF63-474E-AE39-590D1072D0AA}']
    function AsVclFormField: TVclFormField;
  end;

  TVclPanel = class(TPanel)
  protected
    procedure InitDefaults; virtual;
    function InternalSetOption(const ANode: TEFNode): Boolean; virtual;
  public
    procedure SetOption(const ANode: TEFNode);
    constructor CreateAndAddTo(AOwner: TComponent; AParent: TWinControl); virtual;
  end;

  TVclFormPanel = class(TScrollBox)
  strict private
    FLabelWidth: Integer; // 100
    FLabelAlign: TVclFormFormPanelLabelAlign;
    FLabelSeparator: string;
    FLabelPad: Integer; //5
    FColumnWidth: Double;
    FAnchor: string;
    procedure SetLabelWidth(const AValue: Integer);
    procedure SetLabelAlign(const AValue: TVclFormFormPanelLabelAlign);
    procedure SetLabelSeparator(const AValue: string);
    procedure SetLabelPad(const AValue: Integer);
    procedure SetColumnWidth(const AValue: Double);
    procedure SetAnchor(const AValue: string);
  protected
    procedure InitDefaults; virtual;
  public
    constructor CreateAndAddTo(AOwner: TComponent; AParent: TWinControl);
    property LabelWidth: Integer read FLabelWidth write SetLabelWidth;
    property LabelAlign: TVclFormFormPanelLabelAlign read FLabelAlign write SetLabelAlign;
    property LabelSeparator: string read FLabelSeparator write SetLabelSeparator;
    property LabelPad: Integer read FLabelPad write SetLabelPad;
    property ColumnWidth: Double read FColumnWidth write SetColumnWidth;
    property Anchor: string read FAnchor write SetAnchor;
  end;

  TVclTabPanel = class(TPageControl)
  strict private
  public
    constructor CreateAndAddTo(AOwner: TComponent; AParent: TWinControl);
  end;

  TVclContainer = Class(TVclPanel);

  TVclFormFieldSet = class(TGroupBox)
  private
    FCollapsiblePanel: TPanel;
    FRowsPanel: TPanel;
    FCollapsibleButton: TButton;
    FCheckboxToggle: string;
    FCollapsible: Boolean;
    FLabelWidth: Integer;
    FLayout: string; // 'form'
    procedure SetCheckboxToggle(AValue: string);
    procedure SetCollapsible(const AValue: Boolean);
    procedure SetLabelWidth(AValue: Integer);
    procedure SetLayout(AValue: string);
  protected
    procedure InitDefaults; virtual;
  public
    constructor CreateAndAddTo(AOwner: TComponent; AParent: TWinControl);
    procedure AddChild(const AEditItem: IVclEditItem);
    procedure SetOption(const ANode: TEFNode);
    function AsWinControl: TWinControl;
    function AsVclFormField: TVclFormField;
    property Collapsible: Boolean read FCollapsible write SetCollapsible;
    property LabelWidth: Integer read FLabelWidth write SetLabelWidth;
    property Layout: string read FLayout write SetLayout;
    property CheckboxToggle: string read FCheckboxToggle write SetCheckboxToggle;
  end;

  TVclFormField = class(TPanel, IVclEditItem, IVclEditor)
  strict private
    FLabel: TMDLabel;
    FRecordField: TKViewField;
    FFieldName: string;
    FFieldLabel: string;
    FMsgTarget: string;
    FReadOnly: Boolean;
    FDisabled: Boolean;
    FAnchor: string;
    FMaxLength: Integer;
    FAllowBlank: Boolean;
    FCharWidth: Integer;
    procedure SetFieldLabel(const AValue: string);
    procedure SetMsgTarget(const AValue: string);
    procedure SetReadOnly(const AValue: Boolean);
    procedure SetDisabled(const AValue: Boolean);
    procedure SetAnchor(const AValue: string);
    procedure SetMaxLength(const AValue: Integer);
    procedure SetAllowBlank(const AValue: Boolean);
  private
    procedure SetCharWidth(const Value: Integer);
  protected
    procedure InitDefaults; virtual;
    function InternalEditor: TWinControl; virtual;
    function InternalSetOption(const ANode: TEFNode): Boolean; virtual;
  public
    procedure RefreshValue;
    procedure SetOption(const ANode: TEFNode);
    function AsWinControl: TWinControl;
    function AsVclFormField: TVclFormField;
    constructor CreateAndAddTo(AOwner: TComponent; AParent: TWinControl);
    function CharsToPixels(const AChars: Integer; const AOffset: Integer = 0): Integer;
    function GetRecordField: TKViewField;
    procedure SetRecordField(const AValue: TKViewField);
    function GetFieldName: string;
    procedure SetFieldName(const AValue: string);
    property CharWidth: Integer read FCharWidth write SetCharWidth;
    property RecordField: TKViewField read GetRecordField write SetRecordField;
    property FieldLabel: string read FFieldLabel write SetFieldLabel;
    property MsgTarget: string read FMsgTarget write SetMsgTarget;
    property &ReadOnly: Boolean read FReadOnly write SetReadOnly;
    property Disabled: Boolean read FDisabled write SetDisabled;
    property Anchor: string read FAnchor write SetAnchor;
    property FieldName: string read GetFieldName write SetFieldName;
    property MaxLength: Integer read FMaxLength write SetMaxLength;
    property AllowBlank: Boolean read FAllowBlank write SetAllowBlank;
  end;

  TVclFormCompositeField = class(TVclFormField)
  private
  protected
    procedure InitDefaults; override;
  public
  end;

  ///	<summary>
  ///	  Encapsulates a field in a row. Does NOT implement the container
  ///	  interface, as it is a commodity class only.
  ///	</summary>
  TKideFormRowField = class(TVclContainer, IVCLEditor)
  private
    FEditor: IVclEditor;
    FCharWidth: Integer;
    FRecordField: TKViewField;
    procedure SetCharWidth(const AValue: Integer);
  protected
    procedure InitDefaults; override;
    function InternalSetOption(const ANode: TEFNode): Boolean; override;
    procedure SetRecordField(const ARecordField: TKViewField);
  public
    constructor CreateAndAddTo(AOwner: TComponent; AParent: TWinControl); override;
    destructor Destroy; override;
    procedure RefreshValue;
    procedure SetOption(const ANode: TEFNode);
    function AsWinControl: TWinControl;
    function Encapsulate(const AValue: IVclEditor): IVclEditor;
    property CharWidth: Integer read FCharWidth write SetCharWidth;
    function AsVclFormField: TVclFormField;
    property RecordField: TKViewField read FRecordField write SetRecordField;
  end;

  TKideEditPage = class(TTabSheet, IVclEditItem, IVclEditContainer)
  strict private
    FEditPanel: TVclFormPanel;
    FUnexpandedTitle: string;
    procedure SetUnexpandedTitle(const AValue: string);
  protected
    procedure InitDefaults; virtual;
  public
    procedure ResizeByContent;
    constructor CreateAndAddTo(AOwner: TPageControl);
    procedure AddChild(const AEditItem: IVclEditItem);
    procedure SetOption(const ANode: TEFNode);
    procedure RefreshValue;
    function AsWinControl: TWinControl;
    property EditPanel: TVclFormPanel read FEditPanel write FEditPanel;
    property UnexpandedTitle: string read FUnexpandedTitle write SetUnexpandedTitle;
  end;

  TKideFormFieldSet = class(TVclFormFieldSet, IVclEditItem, IVclEditContainer)
  strict private
    FUnexpandedTitle: string;
    FCollapsed: Boolean;
    procedure SetCollapsed(const AValue: Boolean);
  private
    procedure SetUnexpandedTitle(const AValue: string);
  protected
    procedure InitDefaults; override;
  public
    procedure ResizeByContent;
    procedure RefreshValue;
    procedure AddChild(const AEditItem: IVclEditItem);
    procedure SetOption(const ANode: TEFNode);
    function AsWinControl: TWinControl;
    property UnexpandedTitle: string read FUnexpandedTitle write SetUnexpandedTitle;
    property Collapsed: Boolean read FCollapsed write SetCollapsed;
  end;

  TKideFormCompositeField = class(TVclFormCompositeField, IVclEditItem, IVclEditContainer)
  public
    procedure ResizeByContent;
    procedure AddChild(const AEditItem: IVclEditItem);
    procedure SetOption(const ANode: TEFNode);
    function AsWinControl: TWinControl; inline;
  end;

  TKideFormRow = class(TFlowPanel, IVclEditItem, IVclEditContainer)
  protected
    function InternalSetOption(const ANode: TEFNode): Boolean;
  public
    procedure ResizeByContent;
    constructor CreateAndAddTo(AOwner: TComponent; AParent: TWinControl);
    procedure RefreshValue;
    procedure AddChild(const AEditItem: IVclEditItem);
    procedure SetOption(const ANode: TEFNode);
    function AsWinControl: TWinControl; inline;
  end;

  TKideFormNumberField = class(TVclFormField, IVclEditItem, IVclEditor)
  private
    FAllowDecimals: Boolean;
    FAllowNegative: Boolean;
    FDecimalPrecision: Integer;
    FDecimalSeparator: Char;
    FEdit: TEdit;
    procedure SetDecimalPrecision(const AValue: Integer);
    procedure SetDecimalSeparator(const AValue: Char);
    procedure SetAllowDecimals(const AValue: Boolean);
    procedure SetAllowNegative(const AValue: Boolean);
  protected
    function InternalEditor: TWinControl; override;
  public
    constructor CreateAndAddTo(AOwner: TComponent; AParent: TWinControl);
    function AsWinControl: TWinControl; inline;
    function AsVclFormField: TVclFormField;
    property AllowDecimals: Boolean read FAllowDecimals write SetAllowDecimals;
    property AllowNegative: Boolean read FAllowNegative write SetAllowNegative;
    property DecimalPrecision: Integer read FDecimalPrecision write SetDecimalPrecision;
    property DecimalSeparator: Char read FDecimalSeparator write SetDecimalSeparator;
  end;

  TKideFormTextField = class(TVclFormField, IVclEditItem, IVclEditor)
  private
    FIsPassword: Boolean;
    FEdit: TEdit;
    procedure SetIsPassword(const AValue: Boolean);
  protected
    function InternalEditor: TWinControl; override;
  public
    constructor CreateAndAddTo(AOwner: TComponent; AParent: TWinControl);
    function AsWinControl: TWinControl; inline;
    function AsVclFormField: TVclFormField;
    property IsPassword: Boolean read FIsPassword write SetIsPassword;
  end;

  TKideFormTextArea = class(TVclFormField, IVclEditItem, IVclEditor)
  private
    FAutoScroll: Boolean;
    FGrow: Boolean;
    FMemo: TMemo;
    procedure SetAutoScroll(const AValue: Boolean);
    procedure SetGrow(const AValue: Boolean);
  protected
    function InternalEditor: TWinControl; override;
  public
    constructor CreateAndAddTo(AOwner: TComponent; AParent: TWinControl);
    function AsWinControl: TWinControl; inline;
    function AsVclFormField: TVclFormField;
    property AutoScroll: Boolean read FAutoScroll write SetAutoScroll;
    property Grow: Boolean read FGrow write SetGrow;
  end;

  TKideFormCheckbox = class(TVclFormField, IVclEditItem, IVclEditor)
  private
    FBoxLabel: string;
    FCheckBox: TCheckBox;
    procedure SetBoxLabel(const AValue: string);
  protected
    function InternalEditor: TWinControl; override;
  public
    constructor CreateAndAddTo(AOwner: TComponent; AParent: TWinControl);
    function AsWinControl: TWinControl; inline;
    function AsVclFormField: TVclFormField;
    property BoxLabel: string read FBoxLabel write SetBoxLabel;
  end;

  TKideFormDateTimeField = class(TVclFormField, IVclEditItem, IVclEditor)
  private
    FTimeFormat: string;
    FDateFormat: string;
    FDateTimePicker: TDateTimePicker;
    procedure SetDateFormat(const AValue: string);
    procedure SetTimeFormat(const AValue: string);
  protected
    function InternalEditor: TWinControl; override;
  public
    constructor CreateAndAddTo(AOwner: TComponent; AParent: TWinControl);
    function AsWinControl: TWinControl; inline;
    function AsVclFormField: TVclFormField;
    property DateFormat: string read FDateFormat write SetDateFormat;
    property TimeFormat: string read FTimeFormat write SetTimeFormat;
  end;

  TKideFormDateField = class(TKideFormDateTimeField, IVclEditItem, IVclEditor)
  end;

  TKideFormTimeField = class(TKideFormDateTimeField, IVclEditItem, IVclEditor)
  protected
    procedure InitDefaults; override;
  end;

  TKideFormFileUploadField = class(TVclFormField, IVclEditItem, IVclEditor)
  private
    FEdit: TEdit;
    FButton: TButton;
  protected
    function InternalEditor: TWinControl; override;
  public
    constructor CreateAndAddTo(AOwner: TComponent; AParent: TWinControl);
    function AsWinControl: TWinControl; inline;
    function AsVclFormField: TVclFormField;
  end;

  TKideFormComboBoxField = class(TVclFormField, IVclEditItem, IVclEditor)
  strict private
    FMode: string;
    FComboBox: TComboBox;
    procedure SetMode(const AValue: string);
  protected
    procedure InitDefaults; override;
    function InternalEditor: TWinControl; override;
    function InternalSetOption(const ANode: TEFNode): boolean; override;
  public
    constructor CreateAndAddTo(AOwner: TComponent; AParent: TWinControl);
    class function SupportsViewField(const AViewField: TKViewField): Boolean;
    procedure Setup(const AViewField: TKVIewField; const AIsReadOnly: Boolean; const AFieldCharWidth: Integer);
    function AsWinControl: TWinControl; inline;
    function AsVclFormField: TVclFormField;
    property Mode: string read FMode write SetMode;
  end;

  TKideLayoutDefaults = record
    MemoWidth: Integer;
    MaxFieldWidth: Integer;
    MinFieldWidth: Integer;
    RequiredLabelTemplate: string;
    MsgTarget: string;
    procedure Init;
  end;

  type
    TKideEditOperation = (eoUpdate, eoInsert);

  TKideEditorManager = class;

  ///	<summary>
  ///	  Creates editors based on layouts. Can synthesize a default layout if
  ///	  missing.
  ///	</summary>
  TVclLayoutProcessor = class
  strict private
    FForceReadOnly: Boolean;
    FFormPanel: TVclFormPanel;
    FMainEditPage: TKideEditPage;
    FCurrentEditPage: TKideEditPage;
    FFocusField: TVclFormField;
    FDefaults: TKideLayoutDefaults;
    FCurrentEditItem: IVclEditItem;
    FEditContainers: TStack<IVclEditContainer>;
    FOnNewEditItem: TProc<IVclEditItem>;
    FOperation: TKideEditOperation;
    FTabPanel: TVclTabPanel;
    FEditorManager: TKideEditorManager;
    FViewTable: TKViewTable;
    procedure SetMainEditPage(const AValue: TKideEditPage);
    procedure FinalizeCurrentEditPage;
    function CreatePageBreak(const ATitle: string): IVclEditItem;
    function GetViewTable: TKViewTable;
    function CreateEditItem(const ANode: TEFNode;
      const AContainer: IVclEditContainer): IVclEditItem;
    function CreateEditor(const AFieldName: string;
      const AContainer: IVclEditContainer;
      const AOptions: TEFNode = nil): IVclEditor;
    function CreateFieldSet(const ATitle: string): IVclEditItem;
    function CreateCompositeField(const ALabel: string): IVclEditItem;
    procedure SetGlobalOption(const ANode: TEFNode);
    procedure LayoutError(const AErrorMessage: string);
    function CreateRow: IVclEditItem;
    procedure CreateEditorsFromLayout(const ALayout: TKLayout);
    procedure ProcessLayoutNode(const ANode: TEFNode);
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
  private
    FOwner: TComponent;
    procedure SetOperation(const AValue: TKideEditOperation);
    procedure SetOwner(const Value: TComponent);
  public
    // Set all properties before calling the CreateEditors methods.
    property Owner: TComponent read FOwner write SetOwner;
    property ViewTable: TKViewTable read FViewTable write FViewTable;
    property ForceReadOnly: Boolean read FForceReadOnly write FForceReadOnly;
    property FormPanel: TVclFormPanel read FFormPanel write FFormPanel;
    property TabPanel: TVclTabPanel read FTabPanel write FTabPanel;
    property MainEditPage: TKideEditPage read FMainEditPage write SetMainEditPage;
    property OnNewEditItem: TProc<IVclEditItem> read FOnNewEditItem write FOnNewEditItem;
    property Operation: TKideEditOperation read FOperation write SetOperation;

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
    property FocusField: TVclFormField read FFocusField;
  end;

  ///	<summary>
  ///	  Creates editors for edit forms and in-place editors for grids.
  ///   Keeps track of created editors.
  ///	  Used by the layout processor; can be used directly.
  ///	</summary>
  TKideEditorManager = class
  strict private
    FOperation: TKideEditOperation;
    function TryCreateComboBox(const AOwner: TComponent; const AViewField: TKViewField;
      const ARowField: TKideFormRowField; const AFieldCharWidth: Integer;
      const AIsReadOnly: Boolean): IVclEditor;
    function TryCreateTextArea(const AOwner: TComponent; const AViewField: TKViewField;
      const ARowField: TKideFormRowField; const AFieldCharWidth: Integer;
      const AIsReadOnly: Boolean): IVclEditor;
    function TryCreateCheckBox(const AOwner: TComponent; const AViewField: TKViewField;
      const AIsReadOnly: Boolean): IVclEditor;
    function TryCreateDateField(const AOwner: TComponent; const AViewField: TKViewField;
      const ARowField: TKideFormRowField; const AFieldCharWidth: Integer;
      const AIsReadOnly: Boolean): IVclEditor;
    function TryCreateTimeField(const AOwner: TComponent; const AViewField: TKViewField;
      const ARowField: TKideFormRowField; const AFieldCharWidth: Integer;
      const AIsReadOnly: Boolean): IVclEditor;
    function TryCreateDateTimeField(const AOwner: TComponent; const AViewField: TKViewField;
      const ARowField: TKideFormRowField; const AFieldCharWidth: Integer;
      const AIsReadOnly: Boolean): IVclEditor;
    function TryCreateNumberField(const AOwner: TComponent; const AViewField: TKViewField;
      const ARowField: TKideFormRowField; const AFieldCharWidth: Integer;
      const AIsReadOnly: Boolean): IVclEditor;
//    function TryCreateFileEditor(const AOwner: TComponent; const AViewField: TKViewField;
//      const ARowField: TKideFormRowField; const AFieldCharWidth: Integer;
//      const AIsReadOnly: Boolean; const ALabel: string): IVclEditor;
    function CreateTextField(const AOwner: TComponent; const AViewField: TKViewField;
      const ARowField: TKideFormRowField; const AFieldCharWidth: Integer;
      const AIsReadOnly: Boolean): IVclEditor;
  public
    property Operation: TKideEditOperation read FOperation write FOperation;
    function CreateEditor(const AOwner: TComponent; const AViewField: TKViewField;
      const ARowField: TKideFormRowField; const AFieldCharWidth: Integer;
      const AIsReadOnly: Boolean; const ALabel: string = ''): IVclEditor;
    ///	<summary>
    ///	  Creates an in-place editor for the specified field.
    ///	</summary>
    function CreateGridCellEditor(const AOwner: TComponent;
      const AViewField: TKViewField): TVclFormField;
  end;

  TVclFormFieldHelper = class helper for TVclFormField
  public
    procedure StoreValue(const AObjectName: string);
  end;

  TVclEditItemList = class(TList<TObject>)
  public
    procedure EnumEditors(const APredicate: TFunc<IVclEditor, Boolean>; const AHandler: TProc<IVclEditor>);
    procedure AllEditors(const AHandler: TProc<IVclEditor>);
    procedure EnumEditItems(const APredicate: TFunc<IVclEditItem, Boolean>;
      const AHandler: TProc<IVclEditItem>);
    //procedure AllNonEditors(const AHandler: TProc<IVclEditItem>);
    procedure AllEditItems(const AHandler: TProc<IVclEditItem>);
  end;

function OptionAsLabelAlign(const ANode: TEFNode): TVclFormFormPanelLabelAlign;
function CharsToPixels(const AChars: Integer; const AFont: TFont;
  const AOffset: Integer = 16): Integer;

implementation

uses
  System.Types, StrUtils, Math,
  Kitto.SQL, Kitto.Ext.Utils, Kitto.JS.Formatting,
  EF.StrUtils, EF.Intf, EF.Types, EF.JSON,
  KIDE.Project;

procedure ResizeContainerByContent(Container: TWinControl);
var
  LHeight: Integer;
  LWidth: Integer;
  I: Integer;
  LControl: TControl;
begin
  LWidth := Container.Width;
  LHeight := 0;
  for I := 0 to Container.ControlCount-1 do
  begin
    LControl := Container.Controls[I];
    LHeight := max(LHeight, LControl.Height+LControl.Top);
  end;
  Container.SetBounds(Container.Left, Container.Top, LWidth, LHeight);
end;

procedure CalcStandardEditSize(Font : TFont;
  Ctl3D : boolean; out Width, Height : integer);
var
  DC: HDC;
  SaveFont: HFont;
  I: Integer;
  SysMetrics, Metrics: TTextMetric;
begin
  DC := GetDC(0);
  GetTextMetrics(DC, SysMetrics);
  SaveFont := SelectObject(DC, Font.Handle);
  GetTextMetrics(DC, Metrics);
  SelectObject(DC, SaveFont);
  ReleaseDC(0, DC);
  if NewStyleControls then
  begin
    if Ctl3D then I := 8 else I := 6;
    I := GetSystemMetrics(SM_CYBORDER) * I;
  end
  else
  begin
    I := SysMetrics.tmHeight;
    if I > Metrics.tmHeight then I := Metrics.tmHeight;
    I := I div 4 + GetSystemMetrics(SM_CYBORDER) * 4;
  end;
  Height := Metrics.tmHeight + I;
  Width := Metrics.tmAveCharWidth + 2;
end;

function CharsToPixels(const AChars: Integer; const AFont: TFont;
  const AOffset: Integer = 16): Integer;
var
  LWidth, LHeight: Integer;
begin
  CalcStandardEditSize(AFont, False, LWidth, LHeight);
  Result := (LWidth * AChars) + AOffset;
end;


function LinesToPixels(const ALines: Integer; AFont: TFont): Integer;
var
  LWidth, LHeight: Integer;
begin
  CalcStandardEditSize(AFont, False, LWidth, LHeight);
  Result := LHeight * ALines; // * 0.8;
end;

procedure InvalidOption(const ANode: TEFNode);
begin
  raise EEFError.CreateFmt('Unknown or misplaced option %s: %s.', [ANode.Name, ANode.AsString]);
end;

function OptionAsLabelAlign(const ANode: TEFNode): TVclFormFormPanelLabelAlign;
begin
  if SameText(ANode.AsString, 'Left') then
    Result := laLeft
  else if SameText(ANode.AsString, 'Top') then
    Result := laTop
  else if SameText(ANode.AsString, 'Right') then
    Result := laRight
  else
    raise EEFError.CreateFmt('Invalid value %s. Valid values: "Left", "Top", "Right".', [ANode.AsString]);
end;

{ TVclFormFieldSet }

procedure TVclFormFieldSet.SetCollapsible(const AValue: Boolean);
begin
  FCollapsible := AValue;
end;

procedure TVclFormFieldSet.AddChild(const AEditItem: IVclEditItem);
begin
//  InsertComponent(AEditItem.AsWinControl);
end;

function TVclFormFieldSet.AsWinControl: TWinControl;
begin
  Result := Self;
end;

constructor TVclFormFieldSet.CreateAndAddTo(AOwner: TComponent;
  AParent: TWinControl);
begin
  inherited Create(AOwner);
  Parent := AParent;
  Align := alTop;
  Caption := 'FieldSet';
  FCollapsiblePanel := TPanel.Create(AOwner);
  FCollapsiblePanel.Parent := Self;
  FCollapsiblePanel.Align := alRight;
  FCollapsiblePanel.Width := 22;
  FCollapsiblePanel.Margins.Left := 1;
  FCollapsiblePanel.Margins.Right := 1;
  FCollapsiblePanel.BevelOuter := bvNone;
  FCollapsibleButton := TButton.Create(AOwner);
  FCollapsibleButton.Parent := FCollapsiblePanel;
  FCollapsibleButton.Align := alTop;
  FCollapsibleButton.AlignWithMargins := True;
  FCollapsibleButton.SetBounds(0,0,18,18);
  FCollapsibleButton.Caption := 'x';
  FCollapsibleButton.Font.Style := FCollapsibleButton.Font.Style + [fsBold];
  FRowsPanel := TPanel.Create(AOwner);
  FRowsPanel.Parent := Self;
  FRowsPanel.Align := alClient;
  FRowsPanel.BevelOuter := bvNone;
end;

function TVclFormFieldSet.AsVclFormField: TVclFormField;
begin
  Result := nil;
end;

procedure TVclFormFieldSet.SetCheckboxToggle(AValue: string);
begin
  FCheckboxToggle := AValue;
end;

procedure TVclFormFieldSet.SetLabelWidth(AValue: Integer);
begin
  FLabelWidth := AValue;
end;

procedure TVclFormFieldSet.SetLayout(AValue: string);
begin
  FLayout := AValue;
end;

procedure TVclFormFieldSet.SetOption(const ANode: TEFNode);
begin
  ;
end;

procedure TVclFormFieldSet.InitDefaults;
begin
  inherited;
  FLayout := 'form';
end;

{ TVclFormCompositeField }

procedure TVclFormCompositeField.InitDefaults;
begin
  inherited;
end;

{ TVclLayoutProcessor }

procedure TVclLayoutProcessor.LayoutError(const AErrorMessage: string);
begin
  raise EEFError.CreateFmt('Layout parsing error. %s.', [AErrorMessage]);
end;

procedure TVclLayoutProcessor.CreateEditorsFromLayout(const ALayout: TKLayout);
var
  I: Integer;
begin
  Assert(Assigned(FCurrentEditPage));
  Assert(Assigned(ALayout));

  FCurrentEditItem := nil;
  FEditContainers.Clear;
  for I := 0 to ALayout.ChildCount - 1 do
    ProcessLayoutNode(ALayout.Children[I]);
end;

procedure TVclLayoutProcessor.ProcessLayoutNode(const ANode: TEFNode);
var
  LViewField: TKViewField;
  LIntf: IVclEditContainer;

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
      FCurrentEditItem := CreateEditItem(ANode, FCurrentEditPage);
      //FCurrentEditPage.InsertComponent(FCurrentEditItem.AsWinControl);
    end;
    if Supports(FCurrentEditItem, IVclEditContainer, LIntf) then
      FEditContainers.Push(LIntf);
  end
  // Page breaks are not editors nor containers.
  else if SameText(ANode.Name, 'PageBreak') then
  begin
    if FEditContainers.Count > 0 then
      raise Exception.Create('PageBreak must be a top-level node in a layout.');
    CreatePageBreak(ANode.Value);
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
      LayoutError(Format('Option %s must have a value.', [ANode.Name]));
    if ANode.ChildCount > 0 then
      LayoutError(Format('Option node %s cannot have child nodes.', [ANode.Name]));

    if Assigned(FCurrentEditItem) then
      FCurrentEditItem.SetOption(ANode)
    else
      SetGlobalOption(ANode)
  end;

  ProcessChildNodes;

  if Assigned(LIntf) then // Pushed, so pop it.
    FEditContainers.Pop;
end;

procedure TVclLayoutProcessor.CreateEditors(const ALayout: TKLayout);
var
  I: Integer;
  LEditor: IVclEditor;
begin
  Assert(Assigned(FCurrentEditPage));

  FFocusField := nil;

  if Assigned(ALayout) then
    CreateEditorsFromLayout(ALayout)
  else
  begin
    for I := 0 to ViewTable.FieldCount - 1 do
    begin
      if ViewTable.IsFieldVisible(ViewTable.Fields[I]) then
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

procedure TVclLayoutProcessor.FinalizeCurrentEditPage;
begin
  Assert(Assigned(FCurrentEditPage));
end;

function TVclLayoutProcessor.CreateEditItem(const ANode: TEFNode;
  const AContainer: IVclEditContainer): IVclEditItem;
begin
  if SameText(ANode.Name, 'Field') then
    Result := CreateEditor(ANode.Value, AContainer, ANode)
  else if SameText(ANode.Name, 'FieldSet') then
    Result := CreateFieldSet(ANode.Value)
  else if SameText(ANode.Name, 'CompositeField') then
    Result := CreateCompositeField(ANode.Value)
  else if SameText(ANode.Name, 'Row') then
    Result := CreateRow
  else
    raise EEFError.CreateFmt('Unknown edit item type %s.', [ANode.Name]);
  if Assigned(AContainer) then
    AContainer.AddChild(Result);
  if Assigned(FOnNewEditItem) then
    FOnNewEditItem(Result);
end;

function TVclLayoutProcessor.GetViewTable: TKViewTable;
begin
  Result := FViewTable;
end;

function TVclLayoutProcessor.CreateEditor(const AFieldName: string;
  const AContainer: IVclEditContainer; const AOptions: TEFNode): IVclEditor;
var
  LFieldCharWidth: Integer;
  LIsReadOnly: Boolean;
  LLabel: string;
  LViewField: TKViewField;
  LRowField: TKideFormRowField;
  LFormField: TVclFormField;
  LRecordField: TKViewField;

  function CanEditField: Boolean;
  begin
    if FOperation = eoUpdate then
      Result := LViewField.CanUpdate
    else
      Result := LViewField.CanInsert
  end;

begin
  Assert(Assigned(FCurrentEditPage));

  LViewField := ViewTable.FieldByAliasedName(AFieldName);
  LRecordField := ViewTable.FieldByName(LViewField.AliasedName);

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
    LLabel := AOptions.GetString('DisplayLabel');
  if LLabel = '' then
    LLabel := LViewField.DisplayLabel;
  if not LIsReadOnly and LViewField.IsRequired then
    LLabel := ReplaceText(FDefaults.RequiredLabelTemplate, '{label}', LLabel);

  if AContainer is TKideFormRow then
  begin
    LRowField := TKideFormRowField.CreateAndAddTo(FCurrentEditPage, AContainer.AsWinControl);
    LRowField.SetRecordField(LRecordField);
  end
  else
    LRowField := nil;

  Result := FEditorManager.CreateEditor(FCurrentEditPage,
    LViewField, LRowField, LFieldCharWidth, LIsReadOnly, LLabel);

  if Assigned(LRowField) then
    LRowField.Encapsulate(Result);

  Result.AsVclFormField.SetRecordField(LRecordField);

  LFormField := Result.AsVclFormField;
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

function TVclLayoutProcessor.CreateFieldSet(const ATitle: string): IVclEditItem;
var
  LFieldSet: TKideFormFieldSet;
begin
  Assert(Assigned(FCurrentEditPage));

  LFieldSet := TKideFormFieldSet.CreateAndAddTo(Owner, FCurrentEditPage);
  LFieldSet.Collapsible := False;
  LFieldSet.UnexpandedTitle := ATitle;

  Result := LFieldSet;
end;

function TVclLayoutProcessor.CreatePageBreak(const ATitle: string): IVclEditItem;
var
  LPageBreak: TKideEditPage;
begin
  Assert(Assigned(FFormPanel));
  Assert(Assigned(FTabPanel));

  FinalizeCurrentEditPage;

  LPageBreak := TKideEditPage.CreateAndAddTo(FTabPanel);
  //LPageBreak.EditPanel := FFormPanel;
  LPageBreak.UnexpandedTitle := ATitle;
  FCurrentEditPage := LPageBreak;

  Result := LPageBreak;
end;

procedure TVclLayoutProcessor.AfterConstruction;
begin
  inherited;
  FDefaults.Init;
  FEditContainers := TStack<IVclEditContainer>.Create;
  FEditorManager := TKideEditorManager.Create;
end;

function TVclLayoutProcessor.CreateCompositeField(const ALabel: string): IVclEditItem;
var
  LCompositeField: TKideFormCompositeField;
begin
  Assert(Assigned(FCurrentEditPage));

  LCompositeField := TKideFormCompositeField.CreateAndAddTo(FOwner, FCurrentEditPage);
  if ALabel <> '' then
    LCompositeField.FieldLabel := ALabel;
  Result := LCompositeField;
end;

function TVclLayoutProcessor.CreateRow: IVclEditItem;
var
  LRow: TKideFormRow;
begin
  Assert(Assigned(FCurrentEditPage));

  LRow := TKideFormRow.CreateAndAddTo(FCurrentEditPage, FEditContainers.Peek.AsWinControl);
  Result := LRow;
end;

destructor TVclLayoutProcessor.Destroy;
begin
  FreeAndNil(FEditContainers);
  FreeAndNil(FEditorManager);
  inherited;
end;

procedure TVclLayoutProcessor.SetGlobalOption(const ANode: TEFNode);
begin
  if SameText(ANode.Name, 'MemoWidth') then
    FDefaults.MemoWidth := ANode.AsInteger
  else if SameText(ANode.Name, 'MaxFieldWidth') then
    FDefaults.MaxFieldWidth := ANode.AsInteger
  else if SameText(ANode.Name, 'MinFieldWidth') then
    FDefaults.MinFieldWidth := ANode.AsInteger
  else if SameText(ANode.Name, 'RequiredLabelTemplate') then
    FDefaults.RequiredLabelTemplate := ANode.AsString
//  else if SameText(ANode.Name, 'MsgTarget') then
//    FDefaults.MsgTarget := OptionAsString(ANode, ['Qtip', 'Title', 'Under', 'Side'])
  else
    FCurrentEditPage.SetOption(ANode);
end;

procedure TVclLayoutProcessor.SetMainEditPage(const AValue: TKideEditPage);
begin
  FMainEditPage := AValue;
  if Assigned(FMainEditPage) and not Assigned(FCurrentEditPage) then
    FCurrentEditPage := FMainEditPage;
end;

procedure TVclLayoutProcessor.SetOperation(const AValue: TKideEditOperation);
begin
  FOperation := AValue;
  FEditorManager.Operation := FOperation;
end;

procedure TVclLayoutProcessor.SetOwner(const Value: TComponent);
begin
  FOwner := Value;
end;

{ TKideLayoutDefaults }

procedure TKideLayoutDefaults.Init;
begin
  MemoWidth := LAYOUT_MEMOWIDTH;
  MaxFieldWidth := LAYOUT_MAXFIELDWIDTH;
  MinFieldWidth := LAYOUT_MINFIELDWIDTH;
  MsgTarget := LAYOUT_MSGTARGET; // qtip title under side
  RequiredLabelTemplate := LAYOUT_REQUIREDLABELTEMPLATE;
end;

{ TKideEditPage }

procedure TKideEditPage.AddChild(const AEditItem: IVclEditItem);
begin
//  InsertComponent(AEditItem.AsWinControl);
end;

function TKideEditPage.AsWinControl: TWinControl;
begin
  Result := Self;
end;

constructor TKideEditPage.CreateAndAddTo(AOwner: TPageControl);
begin
  inherited Create(AOwner);
  PageControl := AOwner;
end;

procedure TKideEditPage.InitDefaults;
begin
  inherited;
  Assert(Assigned(FEditPanel));
  //Layout := lyForm;
  Margins.Left := 5;
  Margins.Right := 5;
  Margins.Top := 5;
  Margins.Bottom := 5;
  if Assigned(FEditPanel) then
    FEditPanel.AutoScroll := True;
end;

procedure TKideEditPage.RefreshValue;
begin
  ;
end;

procedure TKideEditPage.ResizeByContent;
begin
  ;
end;

procedure TKideEditPage.SetOption(const ANode: TEFNode);
begin
  Assert(Assigned(FEditPanel));
  if SameText(ANode.Name, 'LabelWidth') then
    FEditPanel.LabelWidth := ANode.AsInteger
  else if SameText(ANode.Name, 'LabelAlign') then
  begin
    Assert(Assigned(FEditPanel));
    FEditPanel.LabelAlign := OptionAsLabelAlign(ANode);
  end
  else if SameText(ANode.Name, 'LabelSeparator') then
    FEditPanel.LabelSeparator := ANode.AsString
  else if SameText(ANode.Name, 'LabelPad') then
  begin
    Assert(Assigned(FEditPanel));
    FEditPanel.LabelPad := Anode.AsInteger;
  end
  else
    InvalidOption(ANode);
end;

procedure TKideEditPage.SetUnexpandedTitle(const AValue: string);
begin
  FUnexpandedTitle := AValue;
end;

{ TKideFormFieldSet }

procedure TKideFormFieldSet.AddChild(const AEditItem: IVclEditItem);
begin
//  InsertComponent(AEditItem.AsWinControl);
end;

function TKideFormFieldSet.AsWinControl: TWinControl;
begin
  Result := Self;
end;

procedure TKideFormFieldSet.InitDefaults;
begin
  inherited;
end;

procedure TKideFormFieldSet.RefreshValue;
begin
  ResizeByContent;
end;

procedure TKideFormFieldSet.ResizeByContent;
begin
  ResizeContainerByContent(Self);
end;

procedure TKideFormFieldSet.SetCollapsed(const AValue: Boolean);
begin
  FCollapsed := AValue;
end;

procedure TKideFormFieldSet.SetOption(const ANode: TEFNode);
begin
  if SameText(ANode.Name, 'LabelWidth') then
    LabelWidth := ANode.AsInteger
  else if SameText(ANode.Name, 'Collapsible') then
    Collapsible := ANode.AsBoolean
  else if SameText(ANode.Name, 'Collapsed') then
    Collapsed := True
  else
    InvalidOption(ANode);
end;

procedure TKideFormFieldSet.SetUnexpandedTitle(const AValue: string);
begin
  FUnexpandedTitle := AValue;
end;

{ TKideFormCompositeField }

procedure TKideFormCompositeField.AddChild(const AEditItem: IVclEditItem);
begin
//  InsertComponent(AEditItem.AsWinControl);
end;

function TKideFormCompositeField.AsWinControl: TWinControl;
begin
  Result := Self;
end;

procedure TKideFormCompositeField.ResizeByContent;
begin
  ResizeContainerByContent(Self);
end;

procedure TKideFormCompositeField.SetOption(const ANode: TEFNode);
begin
  InvalidOption(ANode);
end;

{ TKideFormTextField }

function TKideFormTextField.AsVclFormField: TVclFormField;
begin
  Result := Self;
end;

function TKideFormTextField.AsWinControl: TWinControl;
begin
  Result := Self;
end;

constructor TKideFormTextField.CreateAndAddTo(AOwner: TComponent;
  AParent: TWinControl);
begin
  inherited CreateAndAddTo(AOwner, AParent);
  FEdit := TEdit.Create(AOwner);
  FEdit.Parent := Self;
end;

function TKideFormTextField.InternalEditor: TWinControl;
begin
  Result := FEdit;
end;

procedure TKideFormTextField.SetIsPassword(const AValue: Boolean);
begin
  FIsPassword := AValue;
end;

{ TKideFormTextArea }

function TKideFormTextArea.AsVclFormField: TVclFormField;
begin
  Result := Self;
end;

function TKideFormTextArea.AsWinControl: TWinControl;
begin
  Result := Self;
end;

constructor TKideFormTextArea.CreateAndAddTo(AOwner: TComponent;
  AParent: TWinControl);
begin
  inherited CreateAndAddTo(AOwner, AParent);
  FMemo := TMemo.Create(AOwner);
  FMemo.Parent := Self;
end;

function TKideFormTextArea.InternalEditor: TWinControl;
begin
  Result := FMemo;
end;

procedure TKideFormTextArea.SetAutoScroll(const AValue: Boolean);
begin
  FAutoScroll := AValue;
end;

procedure TKideFormTextArea.SetGrow(const AValue: Boolean);
begin
  FGrow := AValue;
end;

{ TKideFormCheckbox }

function TKideFormCheckbox.AsVclFormField: TVclFormField;
begin
  Result := Self;
end;

function TKideFormCheckbox.AsWinControl: TWinControl;
begin
  Result := Self;
end;

constructor TKideFormCheckbox.CreateAndAddTo(AOwner: TComponent;
  AParent: TWinControl);
begin
  inherited CreateAndAddTo(AOwner, AParent);
  FCheckBox := TCheckBox.Create(AOwner);
  FCheckBox.Parent := Self;
end;

function TKideFormCheckbox.InternalEditor: TWinControl;
begin
  Result := FCheckBox;
end;

procedure TKideFormCheckbox.SetBoxLabel(const AValue: string);
begin
  FBoxLabel := AValue;
end;

{ TKideFormDateField }

{ TKideFormComboBoxEditor }

function TKideFormComboBoxField.AsVclFormField: TVclFormField;
begin
  Result := Self;
end;

function TKideFormComboBoxField.AsWinControl: TWinControl;
begin
  Result := Self;
end;

constructor TKideFormComboBoxField.CreateAndAddTo(AOwner: TComponent;
  AParent: TWinControl);
begin
  inherited CreateAndAddTo(AOwner, AParent);
  FComboBox := TComboBox.Create(AOwner);
  FComboBox.Parent := Self;
end;

procedure TKideFormComboBoxField.InitDefaults;
begin
  inherited;
end;

function TKideFormComboBoxField.InternalEditor: TWinControl;
begin
  Result := FComboBox;
end;

procedure TKideFormComboBoxField.SetMode(const AValue: string);
begin
  if AValue = 'local' then
  begin
//    FComboBox.Items.Text := JSONNullToEmptyStr(RecordField.GetAsJSONValue(False, False));
    FComboBox.Items.Text := '';
    if FComboBox.Items.Count > 0 then
      FComboBox.ItemIndex := 0;
  end
  else
    FComboBox.Items.Clear;
end;

function TKideFormComboBoxField.InternalSetOption(const ANode: TEFNode): boolean;
begin
  Result := inherited InternalSetOption(ANode);
  if not Result then
  begin
    if SameText(ANode.Name, 'Resizable') then
    begin
      FComboBox.Perform(CB_SETDROPPEDWIDTH, Width + 20, 0); //Show DropDown larger
      Result := True;
    end;
  end;
end;

procedure TKideFormComboBoxField.Setup(const AViewField: TKVIewField; const AIsReadOnly: Boolean;
  const AFieldCharWidth: Integer);
begin
  ReadOnly := AIsReadOnly;
end;

class function TKideFormComboBoxField.SupportsViewField(const AViewField: TKViewField): Boolean;
var
  LLookupCommandText: string;
  LAllowedValues: TEFPairs;
begin
//  LLookupCommandText := TKSQLBuilder.BuildLookupSelectStatement(AViewField, nil, '');
  LLookupCommandText := '';
  if LLookupCommandText <> '' then
    Result := True
  else
  begin
    LAllowedValues := AViewField.GetChildrenAsPairs('AllowedValues');
    Result := Length(LAllowedValues) > 0;
  end;
end;

{ TKideFormRow }

procedure TKideFormRow.AddChild(const AEditItem: IVclEditItem);
begin
//  InsertComponent(AEditItem.AsWinControl);
end;

function TKideFormRow.AsWinControl: TWinControl;
begin
  Result := Self;
end;

constructor TKideFormRow.CreateAndAddTo(AOwner: TComponent;
  AParent: TWinControl);
begin
  inherited Create(AOwner);
  if AParent is TKideFormFieldSet then
    Parent := TKideFormFieldSet(AParent).FRowsPanel
  else
    Parent := AParent;
  AlignWithMargins := True;
  Align := alTop;
  Height := 84;
end;

function TKideFormRow.InternalSetOption(const ANode: TEFNode): Boolean;
begin
  Result := True;
  if SameText(ANode.Name, 'CharWidth') then
    Width := CharsToPixels(ANode.AsInteger, Self.Font)
  else if SameText(ANode.Name, 'Width') then
//    WidthString := OptionAsIntegerOrPerc(ANode)
    Width := ANode.AsInteger
(*
  else if SameText(ANode.Name, 'Anchor') then
    Anchor := ANode.AsString
  else if SameText(ANode.Name, 'ColumnWidth') then
    ColumnWidth := ANode.AsFloat
*)
  else
    Result := False;
end;

procedure TKideFormRow.RefreshValue;
begin
  ResizeByContent;
end;

procedure TKideFormRow.ResizeByContent;
begin
  ResizeContainerByContent(Self);
end;

procedure TKideFormRow.SetOption(const ANode: TEFNode);
begin
  ;
end;

{ TKideFormRowField }

function TKideFormRowField.AsWinControl: TWinControl;
begin
  Result := Self;
end;

constructor TKideFormRowField.CreateAndAddTo(AOwner: TComponent;
  AParent: TWinControl);
begin
  inherited;
  ;
end;

function TKideFormRowField.AsVclFormField: TVclFormField;
begin
  Result := FEditor.AsVclFormField;
end;

procedure TKideFormRowField.InitDefaults;
begin
  inherited;
  //Layout := lyForm;
end;

function TKideFormRowField.InternalSetOption(const ANode: TEFNode): Boolean;
begin
  Assert(Assigned(FEditor));
  // Widths are set for both the container and the contained editor.
  if SameText(ANode.Name, 'CharWidth') then
    CharWidth := ANode.AsInteger
  else if SameText(ANode.Name, 'Width') then
    //WidthString := OptionAsIntegerOrPerc(ANode)
    Width := ANode.AsInteger
(*
  else if SameText(ANode.Name, 'ColumnWidth') then
  begin
    ColumnWidth := ANode.AsFloat;
    FEditor.SetOption(ANode);
  end
*)
  else
    FEditor.SetOption(ANode);
  Result := True;
end;

procedure TKideFormRowField.RefreshValue;
begin
  ;
end;

destructor TKideFormRowField.Destroy;
begin
  NilEFIntf(FEditor);
  inherited;
end;

function TKideFormRowField.Encapsulate(const AValue: IVclEditor): IVclEditor;
var
  LNode: TEFNode;
begin
  Assert(Assigned(AValue));
  FEditor := AValue;
  FEditor.AsVclFormField.SetRecordField(FRecordField);
  FEditor.AsVclFormField.Parent := Self;
  LNode := TEFNode.Create('Anchor');
  try
    LNode.AsInteger := -5;
    FEditor.SetOption(LNode);
  finally
    FreeAndNil(LNode);
  end;

  Result := Self;
end;

procedure TKideFormRowField.SetCharWidth(const AValue: Integer);
begin
  FCharWidth := AValue;
  Width := CharsToPixels(AValue, Self.Font, 5);
end;

procedure TKideFormRowField.SetOption(const ANode: TEFNode);
begin

end;

procedure TKideFormRowField.SetRecordField(const ARecordField: TKViewField);
begin
  FRecordField := ARecordField;
  if Assigned(FEditor) then
    FEditor.AsVclFormField.SetRecordField(ARecordField);
end;

{ TKideFormNumberField }

procedure TKideFormNumberField.SetAllowDecimals(const AValue: Boolean);
begin
  FAllowDecimals := AValue;
end;

procedure TKideFormNumberField.SetAllowNegative(const AValue: Boolean);
begin
  FAllowNegative := AValue;
end;

procedure TKideFormNumberField.SetDecimalPrecision(const AValue: Integer);
begin
  FDecimalPrecision := AValue;
end;

procedure TKideFormNumberField.SetDecimalSeparator(const AValue: Char);
begin
  FDecimalSeparator := AValue;
end;

function TKideFormNumberField.AsWinControl: TWinControl;
begin
  Result := Self;
end;

constructor TKideFormNumberField.CreateAndAddTo(AOwner: TComponent;
  AParent: TWinControl);
begin
  inherited CreateAndAddTo(AOwner, AParent);
  FEdit := TEdit.Create(AOwner);
  FEdit.Parent := Self;
end;

function TKideFormNumberField.InternalEditor: TWinControl;
begin
  Result := FEdit;
end;

function TKideFormNumberField.AsVclFormField: TVclFormField;
begin
  Result := Self;
end;

{ TKideFormDateTimeField }

function TKideFormDateTimeField.AsVclFormField: TVclFormField;
begin
  Result := Self;
end;

function TKideFormDateTimeField.AsWinControl: TWinControl;
begin
  Result := Self;
end;

constructor TKideFormDateTimeField.CreateAndAddTo(AOwner: TComponent;
  AParent: TWinControl);
begin
  inherited CreateAndAddTo(AOwner, AParent);
  FDateTimePicker := TDateTimePicker.Create(AOwner);
  FDateTimePicker.Parent := Self;
end;

function TKideFormDateTimeField.InternalEditor: TWinControl;
begin
  Result := FDateTimePicker;
end;

procedure TKideFormDateTimeField.SetDateFormat(const AValue: string);
begin
  FTimeFormat := AValue;
end;

procedure TKideFormDateTimeField.SetTimeFormat(const AValue: string);
begin
  FTimeFormat := AValue;
end;

{ TKideFormFileUploadField }

function TKideFormFileUploadField.AsVclFormField: TVclFormField;
begin
  Result := Self;
end;

function TKideFormFileUploadField.AsWinControl: TWinControl;
begin
  Result := Self;
end;

constructor TKideFormFileUploadField.CreateAndAddTo(AOwner: TComponent;
  AParent: TWinControl);
begin
  inherited CreateAndAddTo(AOwner, AParent);
  FEdit := TEdit.Create(AOwner);
  FEdit.Parent := Self;
  FButton := TButton.Create(AOwner);
  FButton.Parent := Self;
end;

function TKideFormFileUploadField.InternalEditor: TWinControl;
begin
  Result := FEdit;
end;

{ TKideEditorManager }

function TKideEditorManager.CreateGridCellEditor(const AOwner: TComponent;
  const AViewField: TKViewField): TVclFormField;
begin
  Result := CreateEditor(AOwner, AViewField, nil, AViewField.DisplayWidth, False).AsVclFormField;
end;

function TKideEditorManager.CreateEditor(const AOwner: TComponent;
  const AViewField: TKViewField; const ARowField: TKideFormRowField;
  const AFieldCharWidth: Integer; const AIsReadOnly: Boolean;
  const ALabel: string): IVclEditor;
var
  LFormField: TVclFormField;
begin
  Result := nil;
//  if Result = nil then
//  Result := TryCreateFileEditor(AOwner, AViewField, ARowField, AFieldCharWidth, AIsReadOnly, ALabel);
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
  Result.AsVclFormField.InitDefaults;
  Result.AsVclFormField.FieldName := AViewField.AliasedName;

  LFormField := Result.AsVclFormField;
  if Assigned(LFormField) then
  begin
    LFormField.Hint := AViewField.Hint;
    LFormField.ReadOnly := AIsReadOnly;
    LFormField.FieldName := AViewField.AliasedName;
  end;
end;

function TKideEditorManager.TryCreateComboBox(
  const AOwner: TComponent; const AViewField: TKViewField;
  const ARowField: TKideFormRowField; const AFieldCharWidth: Integer;
  const AIsReadOnly: Boolean): IVclEditor;
var
  LComboBox: TKideFormComboBoxField;
begin
  Assert(Assigned(AOwner));

  Result := nil;
  if not AViewField.IsDetailReference then
  begin
    if TKideFormComboBoxField.SupportsViewField(AViewField) then
    begin
      LComboBox := TKideFormComboBoxField.CreateAndAddTo(AOwner.Owner, AOwner as TWinControl);
      try
        LComboBox.Setup(AViewField, AIsReadOnly, AFieldCharWidth);
        if not Assigned(ARowField) then
          LComboBox.CharWidth := AFieldCharWidth + TRIGGER_WIDTH
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

function TKideEditorManager.TryCreateTextArea(
  const AOwner: TComponent; const AViewField: TKViewField;
  const ARowField: TKideFormRowField; const AFieldCharWidth: Integer;
  const AIsReadOnly: Boolean): IVclEditor;
var
  LTextArea: TKideFormTextArea;
begin
  Assert(Assigned(AOwner));

  if AViewField.IsBlob or (AViewField.Size div SizeOf(Char) >= MULTILINE_EDIT_THRESHOLD) then
  begin
    LTextArea := TKideFormTextArea.Create(AOwner);
    try
      if not Assigned(ARowField) then
        LTextArea.CharWidth := AFieldCharWidth
      else
        ARowField.CharWidth := AFieldCharWidth;
      LTextArea.Height := LinesToPixels(AViewField.GetInteger('EditLines', 5), ARowField.Font);
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

function TKideEditorManager.TryCreateCheckBox(
  const AOwner: TComponent; const AViewField: TKViewField;
  const AIsReadOnly: Boolean): IVclEditor;
var
  LCheckbox: TKideFormCheckbox;
begin
  Assert(Assigned(AOwner));

  if AViewField.DataType is TEFBooleanDataType then
  begin
    LCheckbox := TKideFormCheckbox.Create(AOwner);
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

function TKideEditorManager.TryCreateDateField(
  const AOwner: TComponent; const AViewField: TKViewField;
  const ARowField: TKideFormRowField; const AFieldCharWidth: Integer;
  const AIsReadOnly: Boolean): IVclEditor;
var
  LDateField: TKideFormDateField;
  LFormat: string;
begin
  Assert(Assigned(AOwner));

  if AViewField.DataType is TEFDateDataType then
  begin
    LDateField := TKideFormDateField.CreateAndAddTo(AOwner.Owner, AOwner as TWinControl);
    try
      if not Assigned(ARowField) then
        LDateField.CharWidth := AFieldCharWidth + TRIGGER_WIDTH
      else
        ARowField.CharWidth := AFieldCharWidth + TRIGGER_WIDTH;
      LFormat := AViewField.EditFormat;
      if LFormat = '' then
        LFormat := TProject.CurrentProject.Config.UserFormatSettings.ShortDateFormat;
      LDateField.DateFormat := TJS.DelphiDateFormatToJSDateFormat(LFormat);
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

function TKideEditorManager.TryCreateTimeField(
  const AOwner: TComponent; const AViewField: TKViewField;
  const ARowField: TKideFormRowField; const AFieldCharWidth: Integer;
  const AIsReadOnly: Boolean): IVclEditor;
var
  LTimeField: TKideFormTimeField;
  LFormat: string;
begin
  Assert(Assigned(AOwner));

  if AViewField.DataType is TEFTimeDataType then
  begin
    LTimeField := TKideFormTimeField.CreateAndAddTo(AOwner.Owner, AOwner as TWinControl);
    try
      if not Assigned(ARowField) then
        LTimeField.CharWidth := AFieldCharWidth + TRIGGER_WIDTH
      else
        ARowField.CharWidth := AFieldCharWidth + TRIGGER_WIDTH;

      LFormat := AViewField.EditFormat;
      if LFormat = '' then
        LFormat := TProject.CurrentProject.Config.UserFormatSettings.ShortTimeFormat;
      LTimeField.DateFormat := TJS.DelphiTimeFormatToJSTimeFormat(LFormat);
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

function TKideEditorManager.TryCreateDateTimeField(
  const AOwner: TComponent; const AViewField: TKViewField;
  const ARowField: TKideFormRowField; const AFieldCharWidth: Integer;
  const AIsReadOnly: Boolean): IVclEditor;
const
  SPACER_WIDTH = 1;
var
  LDateTimeField: TKideFormDateTimeField;
  LFormats: TStringDynArray;
  LDateFormat: string;
  LTimeFormat: string;
begin
  Assert(Assigned(AOwner));

  if AViewField.DataType is TEFDateTimeDataType then
  begin
    LDateTimeField := TKideFormDateTimeField.CreateAndAddTo(AOwner.Owner, AOwner as TWinControl);
    try
      if not Assigned(ARowField) then
        LDateTimeField.CharWidth := AFieldCharWidth + (2 * TRIGGER_WIDTH) + SPACER_WIDTH
      else
        ARowField.CharWidth := AFieldCharWidth + (2 * TRIGGER_WIDTH) + SPACER_WIDTH;
      LFormats := Split(AViewField.EditFormat, ' ');
      if Length(LFormats) > 0 then
        LDateFormat := LFormats[0]
      else
        LDateFormat := TProject.CurrentProject.Config.UserFormatSettings.ShortDateFormat;
      if Length(LFormats) > 1 then
        LTimeFormat := LFormats[1]
      else
        LTimeFormat := TProject.CurrentProject.Config.UserFormatSettings.ShortTimeFormat;
      LDateTimeField.DateFormat := TJS.DelphiDateFormatToJSDateFormat(LDateFormat);
      LDateTimeField.TimeFormat := TJS.DelphiTimeFormatToJSTimeFormat(TProject.CurrentProject.Config.UserFormatSettings.ShortTimeFormat);
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

(*
function TKideEditorManager.TryCreateFileEditor(
  const AOwner: TComponent; const AViewField: TKViewField;
  const ARowField: TKideFormRowField; const AFieldCharWidth: Integer;
  const AIsReadOnly: Boolean; const ALabel: string): IVclEditor;
var
  LFileEditor: TKideFormFileEditor;
begin
  Assert(Assigned(AOwner));

  if (AViewField.DataType is TEFBlobDataType) or (AViewField.DataType is TKFileReferenceDataType) then
  begin
    if AViewField.DataType is TEFBlobDataType then
      LFileEditor := TKideFormFileBlobEditor.Create(AOwner)
    else
      LFileEditor := TKideFormFileReferenceEditor.Create(AOwner);
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
*)

function TKideEditorManager.TryCreateNumberField(const AOwner: TComponent;
  const AViewField: TKViewField;
  const ARowField: TKideFormRowField; const AFieldCharWidth: Integer;
  const AIsReadOnly: Boolean): IVclEditor;
var
  LNumberField: TKideFormNumberField;
begin
  Assert(Assigned(AOwner));

  if AViewField.DataType is TEFNumericDataTypeBase then
  begin
    LNumberField := TKideFormNumberField.CreateAndAddTo(AOwner.Owner, AOwner as TWinControl);
    try
      if not Assigned(ARowField) then
        LNumberField.CharWidth := AFieldCharWidth
      else
        ARowField.CharWidth := AFieldCharWidth;
      if not AIsReadOnly then
      begin
        LNumberField.AllowDecimals := AViewField.DataType is TEFDecimalNumericDataTypeBase;
        LNumberField.AllowNegative := True;
        if LNumberField.AllowDecimals then
          LNumberField.DecimalPrecision := AViewField.DecimalPrecision;
      end;
      LNumberField.DecimalSeparator := TProject.CurrentProject.Config.UserFormatSettings.DecimalSeparator;
      Result := LNumberField;
    except
      LNumberField.Free;
      raise;
    end;
  end
  else
    Result := nil;
end;

function TKideEditorManager.CreateTextField(const AOwner: TComponent;
  const AViewField: TKViewField;
  const ARowField: TKideFormRowField; const AFieldCharWidth: Integer;
  const AIsReadOnly: Boolean): IVclEditor;
var
  LTextField: TKideFormTextField;
begin
  Assert(Assigned(AOwner));

  LTextField := TKideFormTextField.CreateAndAddTo(AOwner.Owner, AOwner as TWinControl);
  try
    if not Assigned(ARowField) then
      LTextField.CharWidth := AFieldCharWidth
    else
      ARowField.CharWidth := AFieldCharWidth;
    if not AIsReadOnly then
    begin
      if AViewField.Size <> 0 then
        LTextField.MaxLength := AViewField.Size;
      LTextField.AllowBlank := not AViewField.IsRequired;
    end;
    if AViewField.GetBoolean('IsPassword') then
      LTextField.IsPassword := True;
    Result := LTextField;
  except
    LTextField.Free;
    raise;
  end;
end;


{ TVclEditItemList }

procedure TVclEditItemList.EnumEditors(
  const APredicate: TFunc<IVclEditor, Boolean>;
  const AHandler: TProc<IVclEditor>);
var
  I: Integer;
  LEditorIntf: IVclEditor;
begin
  for I := 0 to Count - 1 do
  begin
    if Supports(Items[I], IVclEditor, LEditorIntf) then
    begin
      if APredicate(LEditorIntf) then
        AHandler(LEditorIntf);
    end;
  end;
end;

procedure TVclEditItemList.EnumEditItems(
  const APredicate: TFunc<IVclEditItem, Boolean>;
  const AHandler: TProc<IVclEditItem>);
var
  I: Integer;
  LEditItemIntf: IVclEditItem;
begin
  for I := 0 to Count - 1 do
  begin
    if Supports(Items[I], IVclEditItem, LEditItemIntf) then
    begin
      if APredicate(LEditItemIntf) then
        AHandler(LEditItemIntf);
    end;
  end;
end;

procedure TVclEditItemList.AllEditItems(const AHandler: TProc<IVclEditItem>);
begin
  EnumEditItems(
    function (AEditItem: IVclEditItem): Boolean
    begin
      Result := True;
    end,
    AHandler);
end;

procedure TVclEditItemList.AllEditors(
  const AHandler: TProc<IVclEditor>);
begin
  EnumEditors(
    function (AEditor: IVclEditor): Boolean
    begin
      Result := True;
    end,
    AHandler);
end;
(*
procedure TVclEditItemList.AllNonEditors(
  const AHandler: TProc<IVclEditItem>);
begin
  EnumEditItems(
    function (AEditItem: IVclEditItem): Boolean
    begin
      Result := not Supports(AEditItem);
    end,
    AHandler);
end;
*)
{ TVclFormField }

function TVclFormField.AsVclFormField: TVclFormField;
begin
  Result := Self;
end;

function TVclFormField.AsWinControl: TWinControl;
begin
  Result := Self;
end;

function TVclFormField.CharsToPixels(const AChars, AOffset: Integer): Integer;
begin
  Result := KIDE.VCL.Editors.CharsToPixels(AChars, Self.Font, AOffset);
end;

constructor TVclFormField.CreateAndAddTo(AOwner: TComponent;
  AParent: TWinControl);
begin
  inherited Create(AOwner);
  FLabel := TMDLabel.Create(Owner);
  FLabel.StyleElements := [];
  FLabel.Color := clWhite;
  FLabel.Parent := Self;
  if AOwner is TVclFormPanel then
  begin
    Parent := AParent;
    Align := alTop;
  end
  else if AOwner is TVclTabPanel then
  begin
    Parent := AParent;
    Align := alTop;
  end;
end;

function TVclFormField.GetFieldName: string;
begin
  Result := FFieldName;
end;

function TVclFormField.GetRecordField: TKViewField;
begin
  Result := FRecordField;
end;

procedure TVclFormField.InitDefaults;
var
  LInternalEdit: TWinControl;
begin
  LInternalEdit := InternalEditor;
  LInternalEdit.StyleElements := [];
end;

function TVclFormField.InternalEditor: TWinControl;
begin
  Result := nil;
end;

function TVclFormField.InternalSetOption(const ANode: TEFNode): Boolean;
begin
  Result := True;
  if SameText(ANode.Name, 'Anchor') then
    Anchor := ANode.AsString
  else if SameText(ANode.Name, 'CharWidth') then
    Width := CharsToPixels(ANode.AsInteger)
  else if SameText(ANode.Name, 'Width') then
    //WidthString := OptionAsIntegerOrPerc(ANode)
    Width := ANode.AsInteger
  else
    Result := False;
end;

procedure TVclFormField.RefreshValue;
var
  LInternalEdit: TWinControl;
  LWidth: Integer;
begin
  if Parent is TKideFormRowField then
    LWidth := CharsToPixels(TKideFormRowField(Parent).CharWidth)
  else
    LWidth := CharsToPixels(FCharWidth);
  Width := LWidth;
  LInternalEdit := InternalEditor;
  LInternalEdit.SetBounds(5,16,LWidth-10,21);
  FLabel.SetBounds(5,0,LInternalEdit.Width,13);
end;

procedure TVclFormField.SetAllowBlank(const AValue: Boolean);
begin
  FAllowBlank := AValue;
end;

procedure TVclFormField.SetAnchor(const AValue: string);
begin
  FAnchor := AValue;
end;

procedure TVclFormField.SetCharWidth(const Value: Integer);
begin
  FCharWidth := Value;
end;

procedure TVclFormField.SetDisabled(const AValue: Boolean);
begin
  Enabled := not Avalue;
end;

procedure TVclFormField.SetFieldLabel(const AValue: string);
begin
  FLabel.Caption := AValue;
end;

procedure TVclFormField.SetFieldName(const AValue: string);
begin
  FFieldName := AValue;
end;

procedure TVclFormField.SetMaxLength(const AValue: Integer);
begin
  FMaxLength := AValue;
end;

procedure TVclFormField.SetMsgTarget(const AValue: string);
begin
  FMsgTarget := AValue;
end;

procedure TVclFormField.SetOption(const ANode: TEFNode);
begin
  if not InternalSetOption(ANode) then
    InvalidOption(ANode);
end;

procedure TVclFormField.SetReadOnly(const AValue: Boolean);
begin
  FReadOnly := AValue;
end;

procedure TVclFormField.SetRecordField(const AValue: TKViewField);
begin
  FRecordField := AValue;
end;

{ TVclPanel }

constructor TVclPanel.CreateAndAddTo(AOwner: TComponent; AParent: TWinControl);
begin
  inherited Create(AOwner);
  Parent := AParent;
end;

procedure TVclPanel.InitDefaults;
begin
  ;
end;

function TVclPanel.InternalSetOption(const ANode: TEFNode): Boolean;
begin
  Result := False;
end;

procedure TVclPanel.SetOption(const ANode: TEFNode);
begin
  ;
end;

{ TVclFormPanel }

constructor TVclFormPanel.CreateAndAddTo(AOwner: TComponent; AParent: TWinControl);
begin
  inherited Create(AOwner);
  Parent := AParent;
  Color := clWhite;
  StyleElements := [];
  Ctl3D := False;
  AutoScroll := True;
end;

procedure TVclFormPanel.InitDefaults;
begin
  Margins.Left := 5;
  Margins.Right := 5;
  Margins.Top := 5;
  Margins.Bottom := 5;
end;

procedure TVclFormPanel.SetAnchor(const AValue: string);
begin
  FAnchor := AValue;
end;

procedure TVclFormPanel.SetColumnWidth(const AValue: Double);
begin
  FColumnWidth := AValue;
end;

procedure TVclFormPanel.SetLabelAlign(
  const AValue: TVclFormFormPanelLabelAlign);
begin
  FLabelAlign := AValue;
end;

procedure TVclFormPanel.SetLabelPad(const AValue: Integer);
begin
  FLabelPad := AValue;
end;

procedure TVclFormPanel.SetLabelSeparator(const AValue: string);
begin
  FLabelSeparator := AValue;
end;

procedure TVclFormPanel.SetLabelWidth(const AValue: Integer);
begin
  FLabelWidth := AValue;
end;

{ TVclFormFieldHelper }

procedure TVclFormFieldHelper.StoreValue(const AObjectName: string);
begin
  ;
end;

{ TVclTabPanel }

constructor TVclTabPanel.CreateAndAddTo(AOwner: TComponent;
  AParent: TWinControl);
begin
  inherited Create(AOwner);
  Parent := AParent;
  Align := alClient;
  Color := clWhite;
  StyleElements := [];
  Ctl3D := False;
end;

{ TKideFormTimeField }

procedure TKideFormTimeField.InitDefaults;
begin
  inherited;
  FDateTimePicker.Kind := dtkTime;
end;

end.
