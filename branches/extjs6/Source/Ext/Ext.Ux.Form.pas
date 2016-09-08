unit Ext.Ux.Form;

interface

uses
  StrUtils, Kitto.Ext, Ext.Base, Ext.Form;

type
  TExtUxFormItemSelector = class;
  TExtUxFormMultiSelect = class;
  TExtUxFormDateTime = class;
  TExtUxFormFileUploadField = class;
  TExtUxFormSpinnerField = class;
  TExtUxFormSelectBox = class;

  TExtUxFormItemSelector = class(TExtFormField)
  private
    FMultiselects : TExtObjectList;
    procedure SetFMultiselects(Value : TExtObjectList);
  protected
    procedure InitDefaults; override;
  public
    class function JSClassName : string; override;
    property Multiselects : TExtObjectList read FMultiselects write SetFMultiselects;
  end;

  TExtUxFormMultiSelect = class(TExtFormField)
  private
    FAllowBlank : Boolean; // true
    FAppendOnly : string;
    FDdReorder : Boolean;
    FDelimiter : string; // ','
    FDisplayField : string;
    FDisplayFieldNumber : Integer;
    FDragGroup : string;
    FDragGroupArray : TExtObjectList;
    FDropGroup : string;
    FDropGroupArray : TExtObjectList;
    FHeight : Integer; // 100
    FLegend : string;
    FMaxSelections : Integer;
    FMaxSelectionsText : string;
    FMinSelections : Integer; // 0
    FMinSelectionsText : string;
    FStore : TExtDataStore;
    FStoreArray : TExtObjectList;
    FTbar : TExtObject;
    FTbarArray : TExtObjectList;
    FValueField : string;
    FValueFieldNumber : Integer;
    FView : TExtObject;
    FWidth : Integer; // 100
    procedure SetFAllowBlank(Value : Boolean);
    procedure SetFAppendOnly(Value : string);
    procedure SetFDdReorder(Value : Boolean);
    procedure SetFDelimiter(Value : string);
    procedure SetFDisplayField(Value : string);
    procedure SetFDisplayFieldNumber(Value : Integer);
    procedure SetFDragGroup(Value : string);
    procedure SetFDragGroupArray(Value : TExtObjectList);
    procedure SetFDropGroup(Value : string);
    procedure SetFDropGroupArray(Value : TExtObjectList);
    procedure SetFHeight(Value : Integer);
    procedure SetFLegend(Value : string);
    procedure SetFMaxSelections(Value : Integer);
    procedure SetFMaxSelectionsText(Value : string);
    procedure SetFMinSelections(Value : Integer);
    procedure SetFMinSelectionsText(Value : string);
    procedure SetFStore(Value : TExtDataStore);
    procedure SetFStoreArray(Value : TExtObjectList);
    procedure SetFTbar(Value : TExtObject);
    procedure SetFTbarArray(Value : TExtObjectList);
    procedure SetFValueField(Value : string);
    procedure SetFValueFieldNumber(Value : Integer);
    procedure SetFView(Value : TExtObject);
    procedure SetFWidth(Value : Integer);
  protected
    procedure InitDefaults; override;
  public
    class function JSClassName : string; override;
    function GetTargetFromEvent : TExtFunction;
    function GetValue : TExtFunction;
    function SetValue(Values : string) : TExtFunction; overload;
    function SetValue(Values : TExtObjectList) : TExtFunction; overload;
    property AllowBlank : Boolean read FAllowBlank write SetFAllowBlank;
    property AppendOnly : string read FAppendOnly write SetFAppendOnly;
    property DdReorder : Boolean read FDdReorder write SetFDdReorder;
    property Delimiter : string read FDelimiter write SetFDelimiter;
    property DisplayField : string read FDisplayField write SetFDisplayField;
    property DisplayFieldNumber : Integer read FDisplayFieldNumber write SetFDisplayFieldNumber;
    property DragGroup : string read FDragGroup write SetFDragGroup;
    property DragGroupArray : TExtObjectList read FDragGroupArray write SetFDragGroupArray;
    property DropGroup : string read FDropGroup write SetFDropGroup;
    property DropGroupArray : TExtObjectList read FDropGroupArray write SetFDropGroupArray;
    property Height : Integer read FHeight write SetFHeight;
    property Legend : string read FLegend write SetFLegend;
    property MaxSelections : Integer read FMaxSelections write SetFMaxSelections;
    property MaxSelectionsText : string read FMaxSelectionsText write SetFMaxSelectionsText;
    property MinSelections : Integer read FMinSelections write SetFMinSelections;
    property MinSelectionsText : string read FMinSelectionsText write SetFMinSelectionsText;
    property Store : TExtDataStore read FStore write SetFStore;
    property StoreArray : TExtObjectList read FStoreArray write SetFStoreArray;
    property Tbar : TExtObject read FTbar write SetFTbar;
    property TbarArray : TExtObjectList read FTbarArray write SetFTbarArray;
    property ValueField : string read FValueField write SetFValueField;
    property ValueFieldNumber : Integer read FValueFieldNumber write SetFValueFieldNumber;
    property View : TExtObject read FView write SetFView;
    property Width : Integer read FWidth write SetFWidth;
  end;

  TExtUxFormDateTime = class(TExtFormField)
  private
    FConsole : TExtObject;
    FHighlightBgColors : TExtObject;
    FRuntime : TExtObject;
    procedure SetFConsole(Value : TExtObject);
    procedure SetFHighlightBgColors(Value : TExtObject);
    procedure SetFRuntime(Value : TExtObject);
  protected
    procedure InitDefaults; override;
  public
    class function JSClassName : string; override;
    function IntrospectorExtend : TExtFunction;
    function SelectBox : TExtFunction;
    function AddEventListener : TExtFunction;
    function AddKeyboardEvents : TExtFunction;
    function BlockWrap : TExtFunction;
    function Camelize : TExtFunction;
    function CanInit : TExtFunction;
    function Cleanup : TExtFunction;
    function CreateHighlight : TExtFunction;
    function CreateOpenConsoleButton : TExtFunction;
    function CreateTextField : TExtFunction;
    function DrawRect : TExtFunction;
    function Dump : TExtFunction;
    function EscapeHtml : TExtFunction;
    function ExtendRect : TExtFunction;
    function FindDebugWindow : TExtFunction;
    function FindLoader : TExtFunction;
    function GetBorderBox : TExtFunction;
    function GetHtmlWindows : TExtFunction;
    function GetIntProp : TExtFunction;
    function GetStyleProp : TExtFunction;
    function GetTextFormat : TExtFunction;
    function HideHighlight : TExtFunction;
    function HighlightElement : TExtFunction;
    function Init : TExtFunction;
    function IsArgumentsObject : TExtFunction;
    function IsArrayObject : TExtFunction;
    function IsDateObject : TExtFunction;
    function IsItemNative : TExtFunction;
    function IsNumberObject : TExtFunction;
    function IsStringObject : TExtFunction;
    function IsXMLObject : TExtFunction;
    function LoadDebugger : TExtFunction;
    function LogArguments : TExtFunction;
    function LogError : TExtFunction;
    function NoBridge : TExtFunction;
    function NoChildBridge : TExtFunction;
    function ReadConfigFile : TExtFunction;
    function Register : TExtFunction;
    function RegisterChildSandboxBridge : TExtFunction;
    function RegisterCloseEventListener : TExtFunction;
    function RegisterCompleteEventListener : TExtFunction;
    function RegisterDOMEventListeners : TExtFunction;
    function RegisterDeactivateEventListener : TExtFunction;
    function RegisterFrame : TExtFunction;
    function RegisterFramesParentSandboxBridge : TExtFunction;
    function RegisterUncaughtExceptionListener : TExtFunction;
    function RemoteClick : TExtFunction;
    function RemoveEventListener : TExtFunction;
    function ShowCssElement : TExtFunction;
    function ShowHighlight : TExtFunction;
    function ShowHighlightLabels : TExtFunction;
    function ToggleWindow : TExtFunction;
    function TryCreateWindow : TExtFunction;
    function TwoDigits : TExtFunction;
    function WaitForBody : TExtFunction;
    function WriteConfigFile : TExtFunction;
    function WriteConsoleToClipboard : TExtFunction;
    function WriteConsoleToFile : TExtFunction;
    property Console : TExtObject read FConsole write SetFConsole;
    property HighlightBgColors : TExtObject read FHighlightBgColors write SetFHighlightBgColors;
    property Runtime : TExtObject read FRuntime write SetFRuntime;
  end;

  // Procedural types for events TExtUxFormFileUploadField
  TExtUxFormFileUploadFieldOnFileselected = procedure(This : TExtUxFormFileUploadField; Value : string) of object;

  TExtUxFormFileUploadField = class(TExtFormTextField)
  private
    FButtonCfg : TExtObject;
    FButtonOffset : Integer; // 3
    FButtonOnly : Boolean;
    FButtonText : string; // 'Browse...'
    FOnFileselected : TExtUxFormFileUploadFieldOnFileselected;
    procedure SetFButtonCfg(Value : TExtObject);
    procedure SetFButtonOffset(Value : Integer);
    procedure SetFButtonOnly(Value : Boolean);
    procedure SetFButtonText(Value : string);
    procedure SetFOnFileselected(Value : TExtUxFormFileUploadFieldOnFileselected);
  protected
    procedure InitDefaults; override;
    procedure HandleEvent(const AEvtName: string); override;
    function GetObjectNamePrefix: string; override;
  public
    class function JSClassName : string; override;
    property ButtonCfg : TExtObject read FButtonCfg write SetFButtonCfg;
    property ButtonOffset : Integer read FButtonOffset write SetFButtonOffset;
    property ButtonOnly : Boolean read FButtonOnly write SetFButtonOnly;
    property ButtonText : string read FButtonText write SetFButtonText;
    property OnFileselected : TExtUxFormFileUploadFieldOnFileselected read FOnFileselected write SetFOnFileselected;
  end;

  TExtUxFormSpinnerField = class(TExtFormNumberField)
  protected
    procedure InitDefaults; override;
  public
    class function JSClassName : string; override;
  end;

  TExtUxFormSelectBox = class(TExtFormComboBox)
  protected
    procedure InitDefaults; override;
  public
    class function JSClassName : string; override;
  end;

implementation

uses
  EF.Localization;

procedure TExtUxFormItemSelector.SetFMultiselects(Value : TExtObjectList); begin
  FMultiselects := Value;
    JSCode('multiselects:' + VarToJSON([Value, false]));
end;

class function TExtUxFormItemSelector.JSClassName : string; begin
  Result := 'Ext.ux.form.ItemSelector';
end;

procedure TExtUxFormItemSelector.InitDefaults; begin
  inherited;
  FMultiselects := TExtObjectList.CreateInternal(Self, 'multiselects');
end;

procedure TExtUxFormMultiSelect.SetFAllowBlank(Value : Boolean); begin
  FAllowBlank := Value;
  JSCode('allowBlank:' + VarToJSON([Value]));
end;

procedure TExtUxFormMultiSelect.SetFAppendOnly(Value : string); begin
  FAppendOnly := Value;
  JSCode('appendOnly:' + VarToJSON([Value]));
end;

procedure TExtUxFormMultiSelect.SetFDdReorder(Value : Boolean); begin
  FDdReorder := Value;
  JSCode('ddReorder:' + VarToJSON([Value]));
end;

procedure TExtUxFormMultiSelect.SetFDelimiter(Value : string); begin
  FDelimiter := Value;
  JSCode('delimiter:' + VarToJSON([Value]));
end;

procedure TExtUxFormMultiSelect.SetFDisplayField(Value : string); begin
  FDisplayField := Value;
  JSCode('displayField:' + VarToJSON([Value]));
end;

procedure TExtUxFormMultiSelect.SetFDisplayFieldNumber(Value : Integer); begin
  FDisplayFieldNumber := Value;
  JSCode('displayField:' + VarToJSON([Value]));
end;

procedure TExtUxFormMultiSelect.SetFDragGroup(Value : string); begin
  FDragGroup := Value;
  JSCode('dragGroup:' + VarToJSON([Value]));
end;

procedure TExtUxFormMultiSelect.SetFDragGroupArray(Value : TExtObjectList); begin
  FDragGroupArray := Value;
    JSCode('dragGroup:' + VarToJSON([Value, false]));
end;

procedure TExtUxFormMultiSelect.SetFDropGroup(Value : string); begin
  FDropGroup := Value;
  JSCode('dropGroup:' + VarToJSON([Value]));
end;

procedure TExtUxFormMultiSelect.SetFDropGroupArray(Value : TExtObjectList); begin
  FDropGroupArray := Value;
    JSCode('dropGroup:' + VarToJSON([Value, false]));
end;

procedure TExtUxFormMultiSelect.SetFHeight(Value : Integer); begin
  FHeight := Value;
  JSCode('height:' + VarToJSON([Value]));
end;

procedure TExtUxFormMultiSelect.SetFLegend(Value : string); begin
  FLegend := Value;
  JSCode('legend:' + VarToJSON([Value]));
end;

procedure TExtUxFormMultiSelect.SetFMaxSelections(Value : Integer); begin
  FMaxSelections := Value;
  JSCode('maxSelections:' + VarToJSON([Value]));
end;

procedure TExtUxFormMultiSelect.SetFMaxSelectionsText(Value : string); begin
  FMaxSelectionsText := Value;
  JSCode('maxSelectionsText:' + VarToJSON([Value]));
end;

procedure TExtUxFormMultiSelect.SetFMinSelections(Value : Integer); begin
  FMinSelections := Value;
  JSCode('minSelections:' + VarToJSON([Value]));
end;

procedure TExtUxFormMultiSelect.SetFMinSelectionsText(Value : string); begin
  FMinSelectionsText := Value;
  JSCode('minSelectionsText:' + VarToJSON([Value]));
end;

procedure TExtUxFormMultiSelect.SetFStore(Value : TExtDataStore); begin
  FStore := Value;
    JSCode('store:' + VarToJSON([Value, false]));
end;

procedure TExtUxFormMultiSelect.SetFStoreArray(Value : TExtObjectList); begin
  FStoreArray := Value;
    JSCode('store:' + VarToJSON([Value, false]));
end;

procedure TExtUxFormMultiSelect.SetFTbar(Value : TExtObject); begin
  FTbar := Value;
    JSCode('tbar:' + VarToJSON([Value, false]));
end;

procedure TExtUxFormMultiSelect.SetFTbarArray(Value : TExtObjectList); begin
  FTbarArray := Value;
    JSCode('tbar:' + VarToJSON([Value, false]));
end;

procedure TExtUxFormMultiSelect.SetFValueField(Value : string); begin
  FValueField := Value;
  JSCode('valueField:' + VarToJSON([Value]));
end;

procedure TExtUxFormMultiSelect.SetFValueFieldNumber(Value : Integer); begin
  FValueFieldNumber := Value;
  JSCode('valueField:' + VarToJSON([Value]));
end;

procedure TExtUxFormMultiSelect.SetFView(Value : TExtObject); begin
  FView := Value;
    JSCode('view:' + VarToJSON([Value, false]));
end;

procedure TExtUxFormMultiSelect.SetFWidth(Value : Integer); begin
  FWidth := Value;
  JSCode('width:' + VarToJSON([Value]));
end;

class function TExtUxFormMultiSelect.JSClassName : string; begin
  Result := 'Ext.ux.form.MultiSelect';
end;

procedure TExtUxFormMultiSelect.InitDefaults; begin
  inherited;
  FAllowBlank := true;
  FDelimiter := ',';
  FDragGroupArray := TExtObjectList.CreateInternal(Self, 'dragGroup');
  FDropGroupArray := TExtObjectList.CreateInternal(Self, 'dropGroup');
  FHeight := 100;
  FMinSelections := 0;
  FStore := TExtDataStore.CreateInternal(Self, 'store');
  FStoreArray := TExtObjectList.CreateInternal(Self, 'store');
  FTbar := TExtObject.CreateInternal(Self, 'tbar');
  FTbarArray := TExtObjectList.CreateInternal(Self, 'tbar');
  FView := TExtObject.CreateInternal(Self, 'view');
  FWidth := 100;
end;

function TExtUxFormMultiSelect.GetTargetFromEvent : TExtFunction; begin
  JSCode(JSName + '.getTargetFromEvent();', 'TExtUxFormMultiSelect');
  Result := Self;
end;

function TExtUxFormMultiSelect.GetValue : TExtFunction; begin
  JSCode(JSName + '.getValue();', 'TExtUxFormMultiSelect');
  Result := Self;
end;

function TExtUxFormMultiSelect.SetValue(Values : string) : TExtFunction; begin
  JSCode(JSName + '.setValue(' + VarToJSON([Values]) + ');', 'TExtUxFormMultiSelect');
  Result := Self;
end;

function TExtUxFormMultiSelect.SetValue(Values : TExtObjectList) : TExtFunction; begin
  JSCode(JSName + '.setValue(' + VarToJSON(Values) + ');', 'TExtUxFormMultiSelect');
  Result := Self;
end;

procedure TExtUxFormDateTime.SetFConsole(Value : TExtObject); begin
  FConsole := Value;
    JSCode(JSName + '.console=' + VarToJSON([Value, false]) + ';');
end;

procedure TExtUxFormDateTime.SetFHighlightBgColors(Value : TExtObject); begin
  FHighlightBgColors := Value;
    JSCode(JSName + '.highlightBgColors=' + VarToJSON([Value, false]) + ';');
end;

procedure TExtUxFormDateTime.SetFRuntime(Value : TExtObject); begin
  FRuntime := Value;
    JSCode(JSName + '.runtime=' + VarToJSON([Value, false]) + ';');
end;

class function TExtUxFormDateTime.JSClassName : string; begin
  Result := 'Ext.ux.form.DateTime';
end;

procedure TExtUxFormDateTime.InitDefaults; begin
  inherited;
  FConsole := TExtObject.CreateInternal(Self, 'console');
  FHighlightBgColors := TExtObject.CreateInternal(Self, 'highlightBgColors');
  FRuntime := TExtObject.CreateInternal(Self, 'runtime');
end;

function TExtUxFormDateTime.IntrospectorExtend : TExtFunction; begin
  JSCode(JSName + '.Introspector.extend();', 'TExtUxFormDateTime');
  Result := Self;
end;

function TExtUxFormDateTime.SelectBox : TExtFunction; begin
  JSCode(JSName + '.SelectBox();', 'TExtUxFormDateTime');
  Result := Self;
end;

function TExtUxFormDateTime.AddEventListener : TExtFunction; begin
  JSCode(JSName + '.addEventListener();', 'TExtUxFormDateTime');
  Result := Self;
end;

function TExtUxFormDateTime.AddKeyboardEvents : TExtFunction; begin
  JSCode(JSName + '.addKeyboardEvents();', 'TExtUxFormDateTime');
  Result := Self;
end;

function TExtUxFormDateTime.BlockWrap : TExtFunction; begin
  JSCode(JSName + '.blockWrap();', 'TExtUxFormDateTime');
  Result := Self;
end;

function TExtUxFormDateTime.Camelize : TExtFunction; begin
  JSCode(JSName + '.camelize();', 'TExtUxFormDateTime');
  Result := Self;
end;

function TExtUxFormDateTime.CanInit : TExtFunction; begin
  JSCode(JSName + '.canInit();', 'TExtUxFormDateTime');
  Result := Self;
end;

function TExtUxFormDateTime.Cleanup : TExtFunction; begin
  JSCode(JSName + '.cleanup();', 'TExtUxFormDateTime');
  Result := Self;
end;

function TExtUxFormDateTime.CreateHighlight : TExtFunction; begin
  JSCode(JSName + '.createHighlight();', 'TExtUxFormDateTime');
  Result := Self;
end;

function TExtUxFormDateTime.CreateOpenConsoleButton : TExtFunction; begin
  JSCode(JSName + '.createOpenConsoleButton();', 'TExtUxFormDateTime');
  Result := Self;
end;

function TExtUxFormDateTime.CreateTextField : TExtFunction; begin
  JSCode(JSName + '.createTextField();', 'TExtUxFormDateTime');
  Result := Self;
end;

function TExtUxFormDateTime.DrawRect : TExtFunction; begin
  JSCode(JSName + '.drawRect();', 'TExtUxFormDateTime');
  Result := Self;
end;

function TExtUxFormDateTime.Dump : TExtFunction; begin
  JSCode(JSName + '.dump();', 'TExtUxFormDateTime');
  Result := Self;
end;

function TExtUxFormDateTime.EscapeHtml : TExtFunction; begin
  JSCode(JSName + '.escapeHtml();', 'TExtUxFormDateTime');
  Result := Self;
end;

function TExtUxFormDateTime.ExtendRect : TExtFunction; begin
  JSCode(JSName + '.extendRect();', 'TExtUxFormDateTime');
  Result := Self;
end;

function TExtUxFormDateTime.FindDebugWindow : TExtFunction; begin
  JSCode(JSName + '.findDebugWindow();', 'TExtUxFormDateTime');
  Result := Self;
end;

function TExtUxFormDateTime.FindLoader : TExtFunction; begin
  JSCode(JSName + '.findLoader();', 'TExtUxFormDateTime');
  Result := Self;
end;

function TExtUxFormDateTime.GetBorderBox : TExtFunction; begin
  JSCode(JSName + '.getBorderBox();', 'TExtUxFormDateTime');
  Result := Self;
end;

function TExtUxFormDateTime.GetHtmlWindows : TExtFunction; begin
  JSCode(JSName + '.getHtmlWindows();', 'TExtUxFormDateTime');
  Result := Self;
end;

function TExtUxFormDateTime.GetIntProp : TExtFunction; begin
  JSCode(JSName + '.getIntProp();', 'TExtUxFormDateTime');
  Result := Self;
end;

function TExtUxFormDateTime.GetStyleProp : TExtFunction; begin
  JSCode(JSName + '.getStyleProp();', 'TExtUxFormDateTime');
  Result := Self;
end;

function TExtUxFormDateTime.GetTextFormat : TExtFunction; begin
  JSCode(JSName + '.getTextFormat();', 'TExtUxFormDateTime');
  Result := Self;
end;

function TExtUxFormDateTime.HideHighlight : TExtFunction; begin
  JSCode(JSName + '.hideHighlight();', 'TExtUxFormDateTime');
  Result := Self;
end;

function TExtUxFormDateTime.HighlightElement : TExtFunction; begin
  JSCode(JSName + '.highlightElement();', 'TExtUxFormDateTime');
  Result := Self;
end;

function TExtUxFormDateTime.Init : TExtFunction; begin
  JSCode(JSName + '.init();', 'TExtUxFormDateTime');
  Result := Self;
end;

function TExtUxFormDateTime.IsArgumentsObject : TExtFunction; begin
  JSCode(JSName + '.isArgumentsObject();', 'TExtUxFormDateTime');
  Result := Self;
end;

function TExtUxFormDateTime.IsArrayObject : TExtFunction; begin
  JSCode(JSName + '.isArrayObject();', 'TExtUxFormDateTime');
  Result := Self;
end;

function TExtUxFormDateTime.IsDateObject : TExtFunction; begin
  JSCode(JSName + '.isDateObject();', 'TExtUxFormDateTime');
  Result := Self;
end;

function TExtUxFormDateTime.IsItemNative : TExtFunction; begin
  JSCode(JSName + '.isItemNative();', 'TExtUxFormDateTime');
  Result := Self;
end;

function TExtUxFormDateTime.IsNumberObject : TExtFunction; begin
  JSCode(JSName + '.isNumberObject();', 'TExtUxFormDateTime');
  Result := Self;
end;

function TExtUxFormDateTime.IsStringObject : TExtFunction; begin
  JSCode(JSName + '.isStringObject();', 'TExtUxFormDateTime');
  Result := Self;
end;

function TExtUxFormDateTime.IsXMLObject : TExtFunction; begin
  JSCode(JSName + '.isXMLObject();', 'TExtUxFormDateTime');
  Result := Self;
end;

function TExtUxFormDateTime.LoadDebugger : TExtFunction; begin
  JSCode(JSName + '.loadDebugger();', 'TExtUxFormDateTime');
  Result := Self;
end;

function TExtUxFormDateTime.LogArguments : TExtFunction; begin
  JSCode(JSName + '.logArguments();', 'TExtUxFormDateTime');
  Result := Self;
end;

function TExtUxFormDateTime.LogError : TExtFunction; begin
  JSCode(JSName + '.logError();', 'TExtUxFormDateTime');
  Result := Self;
end;

function TExtUxFormDateTime.NoBridge : TExtFunction; begin
  JSCode(JSName + '.noBridge();', 'TExtUxFormDateTime');
  Result := Self;
end;

function TExtUxFormDateTime.NoChildBridge : TExtFunction; begin
  JSCode(JSName + '.noChildBridge();', 'TExtUxFormDateTime');
  Result := Self;
end;

function TExtUxFormDateTime.ReadConfigFile : TExtFunction; begin
  JSCode(JSName + '.readConfigFile();', 'TExtUxFormDateTime');
  Result := Self;
end;

function TExtUxFormDateTime.Register : TExtFunction; begin
  JSCode(JSName + '.register();', 'TExtUxFormDateTime');
  Result := Self;
end;

function TExtUxFormDateTime.RegisterChildSandboxBridge : TExtFunction; begin
  JSCode(JSName + '.registerChildSandboxBridge();', 'TExtUxFormDateTime');
  Result := Self;
end;

function TExtUxFormDateTime.RegisterCloseEventListener : TExtFunction; begin
  JSCode(JSName + '.registerCloseEventListener();', 'TExtUxFormDateTime');
  Result := Self;
end;

function TExtUxFormDateTime.RegisterCompleteEventListener : TExtFunction; begin
  JSCode(JSName + '.registerCompleteEventListener();', 'TExtUxFormDateTime');
  Result := Self;
end;

function TExtUxFormDateTime.RegisterDOMEventListeners : TExtFunction; begin
  JSCode(JSName + '.registerDOMEventListeners();', 'TExtUxFormDateTime');
  Result := Self;
end;

function TExtUxFormDateTime.RegisterDeactivateEventListener : TExtFunction; begin
  JSCode(JSName + '.registerDeactivateEventListener();', 'TExtUxFormDateTime');
  Result := Self;
end;

function TExtUxFormDateTime.RegisterFrame : TExtFunction; begin
  JSCode(JSName + '.registerFrame();', 'TExtUxFormDateTime');
  Result := Self;
end;

function TExtUxFormDateTime.RegisterFramesParentSandboxBridge : TExtFunction; begin
  JSCode(JSName + '.registerFramesParentSandboxBridge();', 'TExtUxFormDateTime');
  Result := Self;
end;

function TExtUxFormDateTime.RegisterUncaughtExceptionListener : TExtFunction; begin
  JSCode(JSName + '.registerUncaughtExceptionListener();', 'TExtUxFormDateTime');
  Result := Self;
end;

function TExtUxFormDateTime.RemoteClick : TExtFunction; begin
  JSCode(JSName + '.remoteClick();', 'TExtUxFormDateTime');
  Result := Self;
end;

function TExtUxFormDateTime.RemoveEventListener : TExtFunction; begin
  JSCode(JSName + '.removeEventListener();', 'TExtUxFormDateTime');
  Result := Self;
end;

function TExtUxFormDateTime.ShowCssElement : TExtFunction; begin
  JSCode(JSName + '.showCssElement();', 'TExtUxFormDateTime');
  Result := Self;
end;

function TExtUxFormDateTime.ShowHighlight : TExtFunction; begin
  JSCode(JSName + '.showHighlight();', 'TExtUxFormDateTime');
  Result := Self;
end;

function TExtUxFormDateTime.ShowHighlightLabels : TExtFunction; begin
  JSCode(JSName + '.showHighlightLabels();', 'TExtUxFormDateTime');
  Result := Self;
end;

function TExtUxFormDateTime.ToggleWindow : TExtFunction; begin
  JSCode(JSName + '.toggleWindow();', 'TExtUxFormDateTime');
  Result := Self;
end;

function TExtUxFormDateTime.TryCreateWindow : TExtFunction; begin
  JSCode(JSName + '.tryCreateWindow();', 'TExtUxFormDateTime');
  Result := Self;
end;

function TExtUxFormDateTime.TwoDigits : TExtFunction; begin
  JSCode(JSName + '.twoDigits();', 'TExtUxFormDateTime');
  Result := Self;
end;

function TExtUxFormDateTime.WaitForBody : TExtFunction; begin
  JSCode(JSName + '.waitForBody();', 'TExtUxFormDateTime');
  Result := Self;
end;

function TExtUxFormDateTime.WriteConfigFile : TExtFunction; begin
  JSCode(JSName + '.writeConfigFile();', 'TExtUxFormDateTime');
  Result := Self;
end;

function TExtUxFormDateTime.WriteConsoleToClipboard : TExtFunction; begin
  JSCode(JSName + '.writeConsoleToClipboard();', 'TExtUxFormDateTime');
  Result := Self;
end;

function TExtUxFormDateTime.WriteConsoleToFile : TExtFunction; begin
  JSCode(JSName + '.writeConsoleToFile();', 'TExtUxFormDateTime');
  Result := Self;
end;

procedure TExtUxFormFileUploadField.SetFButtonCfg(Value : TExtObject); begin
  FButtonCfg.Free;
  FButtonCfg := Value;
    JSCode('buttonCfg:' + VarToJSON([Value, false]));
end;

procedure TExtUxFormFileUploadField.SetFButtonOffset(Value : Integer); begin
  FButtonOffset := Value;
  JSCode('buttonOffset:' + VarToJSON([Value]));
end;

procedure TExtUxFormFileUploadField.SetFButtonOnly(Value : Boolean); begin
  FButtonOnly := Value;
  JSCode('buttonOnly:' + VarToJSON([Value]));
end;

procedure TExtUxFormFileUploadField.SetFButtonText(Value : string); begin
  FButtonText := Value;
  JSCode('buttonText:' + VarToJSON([Value]));
end;

procedure TExtUxFormFileUploadField.SetFOnFileselected(Value : TExtUxFormFileUploadFieldOnFileselected); begin
  if Assigned(FOnFileselected) then
    JSCode(JSName+'.events ["fileselected"].listeners=[];');
  if Assigned(Value) then
    On('fileselected', Ajax('fileselected', ['This', '%0.nm','Value', '%1'], true));
  FOnFileselected := Value;
end;

class function TExtUxFormFileUploadField.JSClassName : string; begin
  Result := 'Ext.ux.form.FileUploadField';
end;

procedure TExtUxFormFileUploadField.InitDefaults; begin
  inherited;
  FButtonCfg := TExtObject.CreateInternal(Self, 'buttonCfg');
  FButtonOffset := 3;
  FButtonText := _('Browse...');
end;

function TExtUxFormFileUploadField.GetObjectNamePrefix: string;
begin
  Result := 'uplfld';
end;

procedure TExtUxFormFileUploadField.HandleEvent(const AEvtName : string); begin
  inherited;
  if (AEvtName = 'fileselected') and Assigned(FOnFileselected) then
    FOnFileselected(TExtUxFormFileUploadField(ParamAsObject('This')), ParamAsstring('Value'));
end;

class function TExtUxFormSpinnerField.JSClassName : string; begin
  Result := 'Ext.ux.form.SpinnerField';
end;

procedure TExtUxFormSpinnerField.InitDefaults; begin
  inherited;
end;

class function TExtUxFormSelectBox.JSClassName : string; begin
  Result := 'Ext.ux.form.SelectBox';
end;

procedure TExtUxFormSelectBox.InitDefaults; begin
  inherited;
end;

end.
