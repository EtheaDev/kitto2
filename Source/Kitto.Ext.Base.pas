{-------------------------------------------------------------------------------
   Copyright 2012-2018 Ethea S.r.l.

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

unit Kitto.Ext.Base;

{$I Kitto.Defines.inc}

interface

uses
  SysUtils
  , Classes
  , Generics.Collections
  , Ext.Base
  , Ext.Form
  , Ext.Ux
  , EF.Intf
  , EF.Tree
  , EF.ObserverIntf
  , EF.Classes
  , Kitto.JS.Base
  , Kitto.JS
  , Kitto.JS.Types
  , Kitto.Ext.Controller
  , Kitto.Metadata.Views
  ;

const
  DEFAULT_WINDOW_WIDTH = 800;
  DEFAULT_WINDOW_HEIGHT = 600;
  DEFAULT_WINDOW_TOOL_WIDTH = 600;
  DEFAULT_WINDOW_TOOL_HEIGHT = 400;

type
  { TODO : move to TExtContainer }
  TKExtContainerHelper = class helper for TExtContainer
  public
    procedure Apply(const AProc: TProc<TExtObject>);
  end;

  /// <summary>
  ///  Base Ext window with subject, observer and controller capabilities.
  /// </summary>
  TKExtWindowControllerBase = class(TExtWindow, IJSController, IJSControllerContainer)
  strict private
    FView: TKView;
    FConfig: TEFNode;
    FContainer: IJSContainer;
    function GetView: TKView;
    function GetConfig: TEFNode;
  strict protected
    procedure SetView(const AValue: TKView);
    procedure DoDisplay; virtual;
    function GetContainer: IJSContainer;
    procedure SetContainer(const AValue: IJSContainer);
    procedure InitDefaults; override;
    function GetControllerToRemove: IJSController; virtual;
    function CreateSubController: IJSController; virtual;
    procedure InitSubController(const ASubController: IJSController); virtual;
    procedure SetActiveSubController(const ASubController: IJSController); virtual;
    function GetDefaultWidth: Integer;
    function GetDefaultHeight: Integer;
  public
    destructor Destroy; override;

    class function SupportsContainer: Boolean;
    function IsSynchronous: Boolean;
    property Config: TEFNode read GetConfig;

    /// <summary>
    ///  Reads the Width, Height, FullScreen properties under <APath> from ATree
    ///  and sets its size accordingly. Applies defaults if properties are not
    ///  specified.
    /// </summry>
    procedure SetSizeFromTree(const ATree: TEFTree; const APath: string);

    property View: TKView read GetView write SetView;
    procedure Display;
    procedure SetModal;
  //published
    procedure PanelClosed; virtual;
    procedure WindowClosed;
  end;

  /// <summary>
  ///  A modal window used to host panels.
  /// </summary>
  TKExtModalWindow = class(TKExtWindowControllerBase)
  protected
    procedure InitDefaults; override;
  public
    /// <summary>
    ///  Call this after adding the panel so that the window can hook its
    ///  beforeclose event and close itself.
    /// <summary>
    procedure HookPanel(const APanel: TExtComponent);
  //published
    procedure PanelClosed; override;
  end;

  /// <summary>
  ///  Base ext viewport with subject, observer and controller capabilities.
  /// </summary>
  { TODO : deprecate and replace with mixin }
  TKExtViewportControllerBase = class(TExtViewport, IJSController, IJSControllerContainer)
  private
    FView: TKView;
    FConfig: TEFNode;
    FContainer: IJSContainer;
    function GetView: TKView;
    function GetConfig: TEFNode;
    procedure CreateSubController;
  protected
    procedure SetView(const AValue: TKView);
    procedure DoDisplay; virtual;
    function GetContainer: IJSContainer;
    procedure SetContainer(const AValue: IJSContainer);
    procedure InitDefaults; override;
  public
    procedure InitSubController(const ASubController: IJSController); virtual;
    procedure SetActiveSubController(const ASubController: IJSController); virtual;
    property Config: TEFNode read GetConfig;

    destructor Destroy; override;

    class function SupportsContainer: Boolean;
    function IsSynchronous: Boolean;

    property View: TKView read GetView write SetView;
    procedure Display;
    procedure SetModal;
  end;

  /// <summary>
  ///   Implemented by controllers that host panels and are able to close them
  ///   on request, such as the TabPanel controller.
  /// </summary>
  IKExtPanelHost = interface(IEFInterface)
    ['{F1DCE0C8-1CD9-4F97-9315-7FB2AC9CAADC}']
    procedure ClosePanel(const APanel: TExtComponent);
  end;

  TKExtButton = class;

  TKExtToolbar = class(TExtToolbar)
  strict private
    FButtonScale: string;
  private
    function GetVisibleButtonCount: Integer;
  public
    destructor Destroy; override;

    property ButtonScale: string read FButtonScale write FButtonScale;
    function FindButton(const AUniqueId: string): TKExtButton;
    property VisibleButtonCount: Integer read GetVisibleButtonCount;
  end;

  /// <summary>
  ///  Base Ext panel with subject and observer capabilities.
  /// </summary>
  TKExtPanelBase = class(TExtPanel, IKExtActivable)
  strict private
    FConfig: TEFNode;
  strict protected
    function GetConfig: TEFNode;
    function GetHostWindow: TExtWindow;
    procedure InitDefaults; override;
    procedure LoadHtml(const AFileName: string; const APostProcessor: TFunc<string, string> = nil);
  public
    destructor Destroy; override;

    procedure Activate; virtual;

    property Config: TEFNode read GetConfig;
  end;

  TKExtButton = class(TExtButton)
  strict private
    FUniqueId: string;
  strict protected
    function FindOwnerToolbar: TKExtToolbar;
    function GetOwnerToolbar: TKExtToolbar;
  protected
    procedure InitDefaults; override;
  public
    // Unique Id of the button in its toolbar (if any).
    property UniqueId: string read FUniqueId write FUniqueId;
    procedure SetIconAndScale(const AIconName: string; const AScale: string = '');
  end;

  TKExtActionButton = class(TKExtButton)
  strict private
    FView: TKView;
    FActionObserver: IEFObserver;
    FOnInitController: TProc<IJSController>;
  strict protected
    procedure InitController(const AController: IJSController); virtual;
    procedure SetView(const AValue: TKView); virtual;
    procedure PerformBeforeExecute;
    class procedure ExecuteHandler(const AButton: TKExtButton);
  public
    property View: TKView read FView write SetView;
    property ActionObserver: IEFObserver read FActionObserver write FActionObserver;
    property OnInitController: TProc<IJSController> read FOnInitController write FOnInitController;
    function GetConfirmCall(const AMessage: string): string;
    procedure ExecuteButtonAction; virtual;
  end;

  TKExtActionButtonClass = class of TKExtActionButton;

  /// <summary>
  ///  Base panel controller. Used for both floating and embedded panels.
  /// </summary>
  TKExtPanelControllerBase = class(TKExtPanelBase, IJSController, IJSControllerContainer)
  strict private
    FView: TKView;
    FContainer: IJSContainer;
    FTopToolbar: TKExtToolbar;
    procedure CreateTopToolbar;
    procedure EnsureAllSupportFiles;
  strict protected
    procedure PerformDelayedClick(const AButton: TExtButton);
    procedure ExecuteNamedAction(const AActionName: string); virtual;
    function GetDefaultSplit: Boolean; virtual;
    function GetView: TKView;
    procedure SetView(const AValue: TKView);
    procedure DoDisplay; virtual;
    function GetContainer: IJSContainer;
    procedure SetContainer(const AValue: IJSContainer);
    procedure InitDefaults; override;
    property Container: IJSContainer read GetContainer write SetContainer;
    property TopToolbar: TKExtToolbar read FTopToolbar;
    procedure BeforeCreateTopToolbar; virtual;
    procedure AfterCreateTopToolbar; virtual;
    function GetDefaultAllowClose: Boolean; virtual;
    function GetDefaultHeight: Integer; virtual;
    function GetDefaultWidth: Integer; virtual;

    /// <summary>
    ///  Adds built-in buttons to the top toolbar.
    /// </summary>
    procedure AddTopToolbarButtons; virtual;

    /// <summary>Adds ToolView buttons to the top toolbar. Called after
    /// AddTopToolbarButtons so that these stay at the end.</summary>
    procedure AddTopToolbarToolViewButtons; virtual;

    /// <summary>Adds to the specified toolbar buttons for any ToolViews
    /// configured in the specified node.</summary>
    /// <param name="AConfigNode">ToolViews node. If nil or childrenless, no
    /// buttons are added.</param>
    /// <param name="AToolbar">Destination toolbar.</param>
    procedure AddToolViewButtons(const AConfigNode: TEFNode; const AToolbar: TKExtToolbar);

    /// <summary>
    ///  Adds an action button representing the specified tool view to
    ///  the specified toolbar. Override this method to create action buttons of
    ///  classes inherited from the base TKExtActionButton.
    /// </summary>
    function AddActionButton(const AUniqueId: string; const AView: TKView;
      const AToolbar: TKExtToolbar): TKExtActionButton; virtual;

    procedure InitSubController(const ASubController: IJSController); virtual;
    procedure SetActiveSubController(const ASubController: IJSController); virtual;
  public
    destructor Destroy; override;
    function IsSynchronous: Boolean;
    property View: TKView read GetView write SetView;
    procedure Display;
    procedure SetModal;
  //published
    procedure Closed;
  end;

  /// <summary>
  ///  Base class for controllers that don't have a specific visual
  ///  representation, yet can be used to render views, such as custom action
  ///  controllers.
  /// </summary>
  TKExtControllerBase = class(TExtObject, IJSController)
  private
    FView: TKView;
    FContainer: IJSContainer;
    FConfig: TEFNode;
  protected
    function GetView: TKView;
    procedure SetView(const AValue: TKView);
    procedure DoDisplay; virtual;
    function GetContainer: IJSContainer;
    procedure SetContainer(const AValue: IJSContainer);
    property Container: IJSContainer read GetContainer write SetContainer;
    function GetConfig: TEFNode;
  public
    destructor Destroy; override;
    function IsSynchronous: Boolean; virtual;
    property View: TKView read GetView write SetView;
    procedure Display;
    procedure SetModal;
    property Config: TEFNode read GetConfig;
    procedure Apply(const AProc: TProc<IJSController>); virtual;
  end;

  /// <summary>
  ///  Base class for tool controllers.
  /// </summary>
  TKExtToolController = class(TKExtControllerBase)
  strict protected
    function GetDisplayLabel: string;
    procedure ExecuteTool; virtual;
    // Called after ExecuteTool. The default implementation calls AfterExecuteTool.
    // Override this with an empty method if you plan to call AfterExecuteTool at
    // a different time.
    procedure DoAfterExecuteTool; virtual;
    procedure AfterExecuteTool; virtual;
    procedure DoDisplay; override;
  public
    class function GetDefaultImageName: string; virtual;
    class function SupportsContainer: Boolean;
    function IsSynchronous: Boolean; override;
    property DisplayLabel: string read GetDisplayLabel;
  end;

  TExtToolControllerClass = class of TKExtToolController;

  /// <summary>
  ///  Base class for tool controllers that display a (modal) window with
  ///  a set of ok/cancel buttons.
  /// </summary>
  TKExtWindowToolController = class(TKExtWindowControllerBase)
  strict private
    FConfirmButton: TKExtButton;
    FCancelButton: TKExtButton;
    FSubController: IJSController;
    procedure CreateButtons;
  strict protected
    procedure SetWindowSize; virtual;
    procedure InitDefaults; override;
    procedure DoDisplay; override;
    function GetConfirmJSFunction: TJSFunction; virtual;
    function GetConfirmJsonData: string; virtual;
    procedure AfterExecuteTool; virtual;
    procedure InitSubController(const AController: IJSController); override;
    property SubController: IJSController read FSubController;
  public
    procedure UpdateObserver(const ASubject: IEFSubject; const AContext: string = ''); override;
  //published
    procedure Confirm; virtual;
    procedure Cancel;  virtual;
  end;

  TKExtFormComboBox = class(TExtFormComboBox)
  protected
    function GetEncodedValue: TExtExpression;
  end;

  TKExtFormTextField = class(TExtFormTextField);

  TKExtFormDateField = class(TExtFormDateField);

  TKExtFormCheckBoxField = class(TExtFormCheckBox);

  TKExtStatusBar = class(TExtUxStatusBar)
  public
    procedure SetErrorStatus(const AText: string);

    function ClearStatus: TJSExpression; virtual;
  end;

function OptionAsLabelAlign(const AAlign: string): TExtContainerLabelAlign;

implementation

uses
  StrUtils
  , EF.StrUtils
  , EF.Types
  , EF.Localization
  , EF.Macros
  , Kitto.AccessControl
  , Kitto.Web.Application
  , Kitto.Web.Session
  , Kitto.Web.Request
  , Kitto.Web.Response
  , Kitto.Ext.Utils
  ;

function OptionAsLabelAlign(const AAlign: string): TExtContainerLabelAlign;
begin
  if SameText(AAlign, 'Left') then
    Result := laLeft
  else if SameText(AAlign, 'Top') then
    Result := laTop
  else if SameText(AAlign, 'Right') then
    Result := laRight
  else
    raise EEFError.CreateFmt(_('Invalid value %s. Valid values: "Left", "Top", "Right".'), [AAlign]);
end;

{ TKExtWindowControllerBase }

function TKExtWindowControllerBase.CreateSubController: IJSController;
var
  LSubView: TKView;
  LNode: TEFNode;
begin
  Assert(Assigned(View));

  Result := nil;
  LNode := View.FindNode('Controller/SubView');
  if Assigned(LNode) then
  begin
    LSubView := TKWebApplication.Current.Config.Views.FindViewByNode(LNode);
    if Assigned(LSubView) then
    begin
      Result := TKExtControllerFactory.Instance.CreateController(Self, LSubView, Self);
      InitSubController(Result);
      Result.Display;
    end;
  end;
end;

destructor TKExtWindowControllerBase.Destroy;
begin
  FreeAndNil(FConfig);
  if TKWebSession.Current <> nil then
    TKWebSession.Current.RemoveController(Self);
  inherited;
end;

procedure TKExtWindowControllerBase.Display;
begin
  DoDisplay;
  &On('render', GenerateAnonymousFunction(UpdateLayout));
end;

procedure TKExtWindowControllerBase.DoDisplay;
var
  LStyle: TEFNode;
begin
  TKWebSession.Current.EnsureSupportFiles(TKExtControllerRegistry.Instance.FindClassId(Self.ClassType));
  TKWebSession.Current.EnsureViewSupportFiles(View);

  LStyle := Config.FindNode('Style');
  if Assigned(LStyle) and (LStyle.AsString <> '') then
    Style := LStyle.AsExpandedString;

  CreateSubController;
  Show;
end;

function TKExtWindowControllerBase.GetConfig: TEFNode;
begin
  if not Assigned(FConfig) then
    FConfig := TEFNode.Create;
  Result := FConfig;
end;

function TKExtWindowControllerBase.GetContainer: IJSContainer;
begin
  Result := FContainer;
end;

function TKExtWindowControllerBase.GetView: TKView;
begin
  Result := FView;
end;

procedure TKExtWindowControllerBase.InitDefaults;
begin
  inherited;
  Layout := lyFit;
  if TKWebRequest.Current.IsMobileBrowser then
    Maximized := True;
  Border := not Maximized;
  Plain := True;

  &On('close',
    TKWebResponse.Current.Items.AjaxCallMethod(Self).SetMethod(WindowClosed)
      .AddParam('Window', JSName).AsFunction);
end;

procedure TKExtWindowControllerBase.InitSubController(const ASubController: IJSController);
begin
end;

function TKExtWindowControllerBase.IsSynchronous: Boolean;
begin
  Result := False;
end;

procedure TKExtWindowControllerBase.PanelClosed;
begin
  NotifyObservers('Closed');
end;

procedure TKExtWindowControllerBase.SetActiveSubController(const ASubController: IJSController);
begin
  // This container does not support the concept of an active subcontroller.
end;

procedure TKExtWindowControllerBase.SetContainer(const AValue: IJSContainer);
begin
  FContainer := AValue;
end;

procedure TKExtWindowControllerBase.SetModal;
begin
//  Floating := True;
//  Modal := True;
end;

procedure TKExtWindowControllerBase.SetSizeFromTree(const ATree: TEFTree; const APath: string);
var
  LWidth: Integer;
  LHeight: Integer;
  LFullScreen: Boolean;
begin
  LWidth := ATree.GetInteger(APath + 'Width', GetDefaultWidth);
  LHeight := ATree.GetInteger(APath + 'Height', Height);
  LFullScreen := ATree.GetBoolean(APath + 'FullScreen', TKWebRequest.Current.IsMobileBrowser);

  if LFullScreen then
  begin
    Maximized := True;
    Border := not Maximized;
  end
  else
  begin
    Width := LWidth;
    Height := LHeight;
  end;
end;

procedure TKExtWindowControllerBase.SetView(const AValue: TKView);
begin
  FView := AValue;
end;

class function TKExtWindowControllerBase.SupportsContainer: Boolean;
begin
  Result := False;
end;

procedure TKExtWindowControllerBase.WindowClosed;
begin
  if TKWebSession.Current <> nil then
    TKWebSession.Current.RemoveController(GetControllerToRemove);
end;

function TKExtWindowControllerBase.GetControllerToRemove: IJSController;
begin
  Result := Self;
end;

function TKExtWindowControllerBase.GetDefaultHeight: Integer;
begin
  Result := TKWebApplication.Current.Config.Config.GetInteger('Defaults/Window/Height', DEFAULT_WINDOW_HEIGHT);
end;

function TKExtWindowControllerBase.GetDefaultWidth: Integer;
begin
  Result := TKWebApplication.Current.Config.Config.GetInteger('Defaults/Window/Width', DEFAULT_WINDOW_WIDTH);
end;

{ TKExtPanelBase }

procedure TKExtPanelBase.Activate;
begin
end;

destructor TKExtPanelBase.Destroy;
begin
  FreeAndNil(FConfig);
  inherited;
end;

function TKExtPanelBase.GetConfig: TEFNode;
begin
  if not Assigned(FConfig) then
    FConfig := TEFNode.Create;
  Result := FConfig;
end;

function TKExtPanelBase.GetHostWindow: TExtWindow;
begin
  Result := Config.GetObject('Sys/HostWindow') as TExtWindow;
end;

procedure TKExtPanelBase.InitDefaults;
begin
  inherited;
  Region := rgCenter;
  Border := False;
end;

procedure TKExtPanelBase.LoadHtml(const AFileName: string;
  const APostProcessor: TFunc<string, string>);
var
  LFullFileName: string;
  LHtml: string;
begin
  LFullFileName := TKWebApplication.Current.FindResourcePathName(AFileName);
  if LFullFileName <> '' then
    LHtml := TEFMacroExpansionEngine.Instance.Expand(TextFileToString(LFullFileName, TEncoding.UTF8))
  else
    LHtml := '';
  if Assigned(APostProcessor) then
    LHtml := APostProcessor(LHtml);
  Html := LHtml;
end;

{ TKExtViewportControllerBase }

destructor TKExtViewportControllerBase.Destroy;
begin
  FreeAndNil(FConfig);
  if TKWebSession.Current <> nil then
    TKWebSession.Current.RemoveController(Self);
  inherited;
end;

procedure TKExtViewportControllerBase.Display;
begin
  DoDisplay;
  &On('render', GenerateAnonymousFunction(UpdateLayout));
end;

procedure TKExtViewportControllerBase.DoDisplay;
var
  LStyle: TEFNode;
begin
  TKWebSession.Current.EnsureSupportFiles(TKExtControllerRegistry.Instance.FindClassId(Self.ClassType));
  TKWebSession.Current.EnsureViewSupportFiles(View);

  LStyle := Config.FindNode('Style');
  if Assigned(LStyle) and (LStyle.AsString <> '') then
    Style := LStyle.AsExpandedString;

  CreateSubController;
  Show;
end;

function TKExtViewportControllerBase.GetConfig: TEFNode;
begin
  if not Assigned(FConfig) then
    FConfig := TEFNode.Create;
  Result := FConfig;
end;

function TKExtViewportControllerBase.GetContainer: IJSContainer;
begin
  Result := FContainer;
end;

function TKExtViewportControllerBase.GetView: TKView;
begin
  Result := FView;
end;

procedure TKExtViewportControllerBase.InitDefaults;
begin
  inherited;
  Layout := lyBorder;
end;

procedure TKExtViewportControllerBase.InitSubController(const ASubController: IJSController);
begin
end;

function TKExtViewportControllerBase.IsSynchronous: Boolean;
begin
  Result := False;
end;

procedure TKExtViewportControllerBase.SetActiveSubController(const ASubController: IJSController);
begin
  // This container does not support the concept of an active subcontroller.
end;

procedure TKExtViewportControllerBase.SetContainer(const AValue: IJSContainer);
begin
  FContainer := AValue;
end;

procedure TKExtViewportControllerBase.SetModal;
begin
  // Viewport cannot be floating.
end;

procedure TKExtViewportControllerBase.SetView(const AValue: TKView);
begin
  FView := AValue;
end;

class function TKExtViewportControllerBase.SupportsContainer: Boolean;
begin
  Result := False;
end;

procedure TKExtViewportControllerBase.CreateSubController;
var
  LSubView: TKView;
  LController: IJSController;
  LNode: TEFNode;
begin
  Assert(Assigned(View));

  LNode := View.FindNode('Controller/SubView');
  if Assigned(LNode) then
  begin
    LSubView := TKWebApplication.Current.Config.Views.FindViewByNode(LNode);
    if Assigned(LSubView) then
    begin
      LController := TKExtControllerFactory.Instance.CreateController(Self, LSubView, Self);
      LController.Display;
    end;
  end;
end;

{ TKExtModalWindow }

procedure TKExtModalWindow.HookPanel(const APanel: TExtComponent);
begin
  Assert(Assigned(APanel));

  APanel.&On('close',
    TKWebResponse.Current.Items.AjaxCallMethod(Self).SetMethod(PanelClosed)
      .AddParam('Panel', APanel.JSName).AsFunction);
  APanel.&On('beforedestroy',
    TKWebResponse.Current.Items.AjaxCallMethod(Self).SetMethod(PanelClosed)
      .AddParam('Panel', APanel.JSName).AsFunction);
end;

procedure TKExtModalWindow.InitDefaults;
var
  LWidth: Integer;
  LHeight: Integer;
begin
  inherited;
  LWidth := GetDefaultWidth;
  if LWidth <> 0 then
    Width := LWidth;
  LHeight := GetDefaultHeight;
  if LHeight <> 0 then
    Height := LHeight;
  Closable := False;
  Modal := True;
end;

procedure TKExtModalWindow.PanelClosed;
begin
  inherited;
  Close;
end;

{ TKExtFormComboBox }

function TKExtFormComboBox.GetEncodedValue: TExtExpression;
begin
  Result := TKWebResponse.Current.Items.ExecuteJSCode(Self, Format('encodeURI(%s.getValue())', [JSName])).AsExpression;
end;

{ TKExtPanelControllerBase }

function TKExtPanelControllerBase.GetDefaultHeight: Integer;
begin
  Result := TKWebApplication.Current.Config.Config.GetInteger('Defaults/Window/Height', DEFAULT_WINDOW_HEIGHT);
end;

function TKExtPanelControllerBase.GetDefaultWidth: Integer;
begin
  Result := TKWebApplication.Current.Config.Config.GetInteger('Defaults/Window/Width', DEFAULT_WINDOW_WIDTH);
end;

destructor TKExtPanelControllerBase.Destroy;
begin
  if TKWebSession.Current <> nil then
    TKWebSession.Current.RemoveController(Self);
  inherited;
end;

procedure TKExtPanelControllerBase.Display;
begin
  if Container <> nil then
    Closable := Config.GetBoolean('AllowClose', GetDefaultAllowClose);
  DoDisplay;
  &On('render', GenerateAnonymousFunction(UpdateLayout));
  if Modal then
  //if Floating then
    ShowModal
  else
    Show(JSExpressionFromCodeBlock('getAnimationOrigin()'));
end;

procedure TKExtPanelControllerBase.EnsureAllSupportFiles;
var
  LClassType: TClass;
begin
  LClassType := ClassType;
  TKWebSession.Current.EnsureSupportFiles(TKExtControllerRegistry.Instance.FindClassId(LClassType));
  while LClassType.ClassParent <> nil do
  begin
    LClassType := LClassType.ClassParent;
    TKWebSession.Current.EnsureSupportFiles(TKExtControllerRegistry.Instance.FindClassId(LClassType));
  end;
  TKWebSession.Current.EnsureViewSupportFiles(View);
end;

procedure TKExtPanelControllerBase.DoDisplay;
var
  LWidth: Integer;
  LSplit: TEFNode;
  LCollapsible: TEFNode;
  LBorder: TEFNode;
  LHeight: Integer;
  LHeader: TEFNode;
  LWidthStr: string;
  LHeightStr: string;
  LView: TKView;
  LBodyStyle: string;
  LStyle: TEFNode;
  LLabelWidth: TEFNode;
  LRenderTo: string;
begin
  EnsureAllSupportFiles;

  LRenderTo := Config.GetString('ContainerElementId');
  if LRenderTo <> '' then
    RenderTo := TEFMacroExpansionEngine.Instance.Expand(LRenderTo);

  LLabelWidth := Config.FindNode('LabelWidth');
  if Assigned(LLabelWidth) then
    LabelWidth := LLabelWidth.AsInteger;

  if Title = '' then
  begin
    LView := View;
    if Assigned(LView) then
      Title := _(Config.GetExpandedString('Title', LView.DisplayLabel));
  end;

  LHeader := Config.FindNode('Header');
  if Assigned(LHeader) then
    Header := LHeader.AsBoolean
  else
    Header := (Title <> '') and not Assigned(Container);

  Draggable := Config.GetBoolean('Movable', Header);

  LWidthStr := Config.GetString('Width');
  if TryStrToInt(LWidthStr, LWidth) then
  begin
    if LWidth > 0 then
    begin
      Width := LWidth;
      MinWidth := LWidth;
    end
    else if LWidth = -1 then
      AutoWidth := True;
  end
  else if LWidthStr <> '' then
    WidthString := LWidthStr
  else if Floating then
    Width := GetDefaultWidth;

  LHeightStr := Config.GetString('Height');
  if TryStrToInt(LHeightStr, LHeight) then
  begin
    if LHeight > 0 then
      Height := LHeight
    else if LHeight = -1 then
      AutoHeight := True;
  end
  else if LHeightStr <> '' then
    HeightString := LHeightStr
  else if Floating then
    Height := GetDefaultHeight;

  LSplit := Config.FindNode('Split');
  if Assigned(LSplit) then
    Split := LSplit.AsBoolean
  else
    Split := GetDefaultSplit;

  LBorder := Config.FindNode('Border');
  if Assigned(LBorder) then
    Border := LBorder.AsBoolean
  else
    Border := False;

  LCollapsible := Config.FindNode('Collapsible');
  if Assigned(LCollapsible) then
    Collapsible := LCollapsible.AsBoolean
  else
    Collapsible := False;

  CreateTopToolbar;

  LBodyStyle := Config.GetExpandedString('BodyStyle');
  if LBodyStyle <> '' then
    BodyStyle := LBodyStyle;

  LStyle := Config.FindNode('Style');
  if Assigned(LStyle) and (LStyle.AsString <> '') then
    Style := LStyle.AsExpandedString;
end;

procedure TKExtPanelControllerBase.ExecuteNamedAction(const AActionName: string);
var
  LToolButton: TKExtButton;
begin
  LToolButton := FTopToolbar.FindButton(AActionName);
  if Assigned(LToolButton) then
    PerformDelayedClick(LToolButton);
end;

procedure TKExtPanelControllerBase.AddTopToolbarButtons;
begin
end;

function TKExtPanelControllerBase.AddActionButton(const AUniqueId: string;
  const AView: TKView; const AToolbar: TKExtToolbar): TKExtActionButton;
var
  LConfirmationMessage: string;
  LConfirmationJS: string;
begin
  Assert(Assigned(AView));
  Assert(Assigned(AToolbar));

  Result := TKExtActionButton.CreateAndAddToArray(AToolbar.Items);
  Result.Hidden := not AView.GetBoolean('IsVisible', True);
  Result.UniqueId := AUniqueId;
  Result.View := AView;
  Result.ActionObserver := Self;

  // A Tool may or may not have a confirmation message.
  LConfirmationMessage := AView.GetExpandedString('Controller/ConfirmationMessage');
  // Cleanup Linebreaks with <br> tag
  LConfirmationMessage := StringReplace(LConfirmationMessage, sLineBreak, '<br>',[rfReplaceAll]);
  LConfirmationJS := Result.GetConfirmCall(LConfirmationMessage);
  if LConfirmationMessage <> '' then
    Result.On('click', GenerateAnonymousFunction(LConfirmationJS))
  else
    //Result.On('click', Ajax(Result.ExecuteButtonAction, []));
    Result.On('click',
      TKWebResponse.Current.Items.AjaxCallMethod(Self, 'click').SetMethod(Result.ExecuteButtonAction).AsFunction);
end;

procedure TKExtPanelControllerBase.AddToolViewButtons(
  const AConfigNode: TEFNode; const AToolbar: TKExtToolbar);
var
  I: Integer;
  LView: TKView;
  LNode: TEFNode;
begin
  Assert(Assigned(AToolbar));

  if Assigned(AConfigNode) and (AConfigNode.ChildCount > 0) then
  begin
    TExtToolbarSeparator.CreateAndAddToArray(AToolbar.Items);
    for I := 0 to AConfigNode.ChildCount - 1 do
    begin
      LNode := AConfigNode.Children[I];
      LView := TKWebApplication.Current.Config.Views.ViewByNode(LNode);
      if LView.IsAccessGranted(ACM_VIEW) then
        AddActionButton(LNode.Name, LView, AToolbar);
    end;
  end;
end;

procedure TKExtPanelControllerBase.AfterCreateTopToolbar;
begin
end;

procedure TKExtPanelControllerBase.BeforeCreateTopToolbar;
begin
end;

procedure TKExtPanelControllerBase.AddTopToolbarToolViewButtons;
begin
  AddToolViewButtons(Config.FindNode('ToolViews'), TopToolbar);
end;

procedure TKExtPanelControllerBase.Closed;
begin
  Free;
end;

procedure TKExtPanelControllerBase.CreateTopToolbar;
begin
  BeforeCreateTopToolbar;

  FTopToolbar := TKExtToolbar.CreateInline(Self);
  try
    FTopToolbar.ButtonScale := Config.GetString('ToolButtonScale',
      IfThen(TKWebRequest.Current.IsMobileBrowser, 'large', 'small'));
    FTopToolbar.AutoScroll := TKWebRequest.Current.IsMobileBrowser;
    AddTopToolbarButtons;
    AddTopToolbarToolViewButtons;
  except
    FreeAndNil(FTopToolbar);
    raise;
  end;
  if FTopToolbar.Items.Count = 0 then
    FreeAndNil(FTopToolbar)
  else
  begin
    if FTopToolbar.VisibleButtonCount = 0 then
      FTopToolbar.Hidden := True;
    Tbar := FTopToolbar;
  end;
  AfterCreateTopToolbar;
end;

function TKExtPanelControllerBase.GetDefaultAllowClose: Boolean;
begin
  Result := False;
end;

function TKExtPanelControllerBase.GetDefaultSplit: Boolean;
begin
  Result := False;
end;

function TKExtPanelControllerBase.GetContainer: IJSContainer;
begin
  Result := FContainer;
end;

function TKExtPanelControllerBase.GetView: TKView;
begin
  Result := FView;
end;

procedure TKExtPanelControllerBase.InitDefaults;
begin
  inherited;
  &On('close', TKWebResponse.Current.Items.AjaxCallMethod(Self).SetMethod(Closed).AsFunction);
end;

procedure TKExtPanelControllerBase.InitSubController(const ASubController: IJSController);
var
  LSysConfigNode: TEFNode;
begin
  Assert(Assigned(ASubController));

  LSysConfigNode := Config.FindNode('Sys');
  if Assigned(LSysConfigNode) then
    ASubController.Config.GetNode('Sys', True).Assign(LSysConfigNode);
end;

function TKExtPanelControllerBase.IsSynchronous: Boolean;
begin
  Result := False;
end;

procedure TKExtPanelControllerBase.PerformDelayedClick(const AButton: TExtButton);
begin
  if Assigned(AButton) then
    AButton.On('render', GenerateAnonymousFunction(AButton.PerformClick));
end;

procedure TKExtPanelControllerBase.SetActiveSubController(const ASubController: IJSController);
begin
end;

procedure TKExtPanelControllerBase.SetContainer(const AValue: IJSContainer);
begin
  FContainer := AValue;
end;

procedure TKExtPanelControllerBase.SetModal;
begin
//  Floating := True;
  Modal := True;
//  RenderToExpression := JSExpressionFromCodeBlock('Ext.getBody()');
end;

procedure TKExtPanelControllerBase.SetView(const AValue: TKView);
begin
  FView := AValue;
end;

{ TKExtStatusBar }

function TKExtStatusBar.ClearStatus: TJSExpression;
begin
  Result := inherited ClearStatus;
end;

procedure TKExtStatusBar.SetErrorStatus(const AText: string);
begin
  SetStatus(JSObject(Format('text: "%s", iconCls:"x-status-error",clear:true', [AText])));
end;

{ TKExtControllerBase }

procedure TKExtControllerBase.Apply(const AProc: TProc<IJSController>);
begin
  Assert(Assigned(AProc));

  AProc(Self);
end;

destructor TKExtControllerBase.Destroy;
begin
  FreeAndNil(FConfig);
  if TKWebSession.Current <> nil then
    TKWebSession.Current.RemoveController(Self);
  inherited;
end;

procedure TKExtControllerBase.Display;
begin
  DoDisplay;
end;

procedure TKExtControllerBase.DoDisplay;
begin
  TKWebSession.Current.EnsureSupportFiles(TKExtControllerRegistry.Instance.FindClassId(Self.ClassType));
  TKWebSession.Current.EnsureViewSupportFiles(View);
end;

function TKExtControllerBase.GetConfig: TEFNode;
begin
  if not Assigned(FConfig) then
    FConfig := TEFNode.Create;
  Result := FConfig;
end;

function TKExtControllerBase.GetContainer: IJSContainer;
begin
  Result := FContainer;
end;

function TKExtControllerBase.GetView: TKView;
begin
  Result := FView;
end;

function TKExtControllerBase.IsSynchronous: Boolean;
begin
  Result := False;
end;

procedure TKExtControllerBase.SetContainer(const AValue: IJSContainer);
begin
  FContainer := AValue;
end;

procedure TKExtControllerBase.SetModal;
begin
  // No need.
end;

procedure TKExtControllerBase.SetView(const AValue: TKView);
begin
  FView := AValue;
end;

{ TKExtActionController }

procedure TKExtToolController.ExecuteTool;
begin
end;

function TKExtToolController.GetDisplayLabel: string;
begin
  if Assigned(View) then
    Result := Config.GetExpandedString('Title', View.DisplayLabel)
  else
    Result := '';
end;

function TKExtToolController.IsSynchronous: Boolean;
begin
  Result := True;
end;

class function TKExtToolController.SupportsContainer: Boolean;
begin
  Result := False;
end;

class function TKExtToolController.GetDefaultImageName: string;
begin
  Result := 'tool_exec';
end;

procedure TKExtToolController.AfterExecuteTool;
begin
end;

procedure TKExtToolController.DoAfterExecuteTool;
begin
  AfterExecuteTool;
end;

procedure TKExtToolController.DoDisplay;
begin
  inherited;
  ExecuteTool;
  DoAfterExecuteTool;
end;

{ TKExtContainerHelper }

procedure TKExtContainerHelper.Apply(const AProc: TProc<TExtObject>);
var
  I: Integer;
begin
  Assert(Assigned(AProc));

  for I := 0 to Items.Count - 1 do
  begin
    AProc(Items[I]);
    if Items[I] is TExtContainer then
      TExtContainer(Items[I]).Apply(AProc);
  end;
end;

{ TKExtActionButton }

procedure TKExtActionButton.ExecuteButtonAction;
var
  LController: IJSController;
begin
  Assert(Assigned(FView));
  Assert(Assigned(FActionObserver));

  if View.IsPersistent then
    TKWebApplication.Current.DisplayView(View, FActionObserver)
  else
  begin
    LController := TKExtControllerFactory.Instance.CreateController(TKWebSession.Current.ObjectSpace, FView, nil, nil, FActionObserver);
    InitController(LController);
    LController.Display;
  end;
end;

class procedure TKExtActionButton.ExecuteHandler(const AButton: TKExtButton);
var
  LResponseItemBranch: TJSResponseItems;
begin
  { TODO : does this code produce any client-side effect? }
  if AButton is TKExtActionButton then
  begin
    LResponseItemBranch := TKWebResponse.Current.BranchItems;
    try
      TKExtActionButton(AButton).ExecuteButtonAction;
    finally
      TKWebResponse.Current.UnbranchItems(LResponseItemBranch, False); // throw away
    end;
  end;
end;

function TKExtActionButton.GetConfirmCall(const AMessage: string): string;
begin
  Result := Format('confirmCall("%s", "%s", ajaxSimple, {methodURL: "%s"});',
    [_(TKWebApplication.Current.Config.AppTitle), AMessage, GetMethodURL(ExecuteButtonAction)]);
end;

procedure TKExtActionButton.InitController(const AController: IJSController);
begin
  if Assigned(FOnInitController) then
    FOnInitController(AController);
end;

procedure TKExtActionButton.PerformBeforeExecute;
var
  LUniqueId: string;
  LBeforeExecuteNode, LChildNode: TEFNode;
  I: Integer;
  LButton: TKExtButton;
begin
  LBeforeExecuteNode := View.FindNode('BeforeExecute');
  if Assigned(LBeforeExecuteNode) then
  begin
    for I := 0 to LBeforeExecuteNode.ChildCount - 1 do
    begin
      LChildNode := LBeforeExecuteNode.Children[I];
      if SameText(LChildNode.Name, 'ToolView') then
      begin
        LUniqueId := LChildNode.AsString;
        if LUniqueId <> '' then
        begin
          LButton := GetOwnerToolbar.FindButton(LUniqueId);
          if Assigned(LButton) then
            ExecuteHandler(LButton as TKExtActionButton);
        end;
      end;
    end;
  end;
end;

procedure TKExtActionButton.SetView(const AValue: TKView);
var
  LTooltip, LIconName: string;
begin
  Assert(Assigned(AValue));

  FView := AValue;
  Text := _(FView.DisplayLabel);

  LIconName := FView.GetString('ImageName');
  if LIconName = '' then
    LIconName := CallViewControllerStringMethod(FView, 'GetDefaultImageName', '');
  if LIconName = '' then
    LIconName := FView.ImageName;

  SetIconAndScale(LIconName);

  LTooltip := FView.GetExpandedString('Hint');
  if LTooltip <> '' then
    Tooltip := LTooltip;
end;

{ TKExtWindowToolController }

procedure TKExtWindowToolController.AfterExecuteTool;
begin
  NotifyObservers('ToolConfirmed');
  Close;
end;

procedure TKExtWindowToolController.Cancel;
begin
  NotifyObservers('ToolCanceled');
  Close;
end;

procedure TKExtWindowToolController.Confirm;
begin
  AfterExecuteTool;
end;

procedure TKExtWindowToolController.CreateButtons;
var
  LToolbar: TKExtToolbar;
begin
  LToolbar := TKExtToolbar.Create(Self);
  TExtToolbarFill.CreateInlineAndAddToArray(LToolbar.Items);
  Fbar := LToolbar;

  FConfirmButton := TKExtButton.CreateAndAddToArray(LToolbar.Items);
  FConfirmButton.SetIconAndScale('accept', Config.GetString('ButtonScale', 'medium'));
  FConfirmButton.FormBind := True;
  FConfirmButton.Text := Config.GetString('ConfirmButton/Caption', _('Confirm'));
  FConfirmButton.Tooltip := Config.GetString('ConfirmButton/Tooltip', _('Confirm action and close window'));
  FConfirmButton.Handler := GetConfirmJSFunction();

  FCancelButton := TKExtButton.CreateAndAddToArray(LToolbar.Items);
  FCancelButton.SetIconAndScale('cancel', Config.GetString('ButtonScale', 'medium'));
  FCancelButton.Text := _('Cancel');
  FCancelButton.Tooltip := _('Cancel changes');
  //FCancelButton.Handler := Ajax(Cancel);
  FCancelButton.Handler := TKWebResponse.Current.Items.AjaxCallMethod(Self).SetMethod(Cancel).AsFunction;
end;

procedure TKExtWindowToolController.DoDisplay;
begin
  inherited;
  Title := _(View.DisplayLabel);
  SetWindowSize;
  if not Config.GetBoolean('HideButtons') then
    CreateButtons;
end;

function TKExtWindowToolController.GetConfirmJSFunction: TJSFunction;
begin
  //Result := GetPOSTAjaxCode(Confirm, [], GetConfirmJsonData);
  Result := TKWebResponse.Current.Items.AjaxCallMethod(Self).SetMethod(Confirm)
    .Post(GetConfirmJsonData).AsFunction;
end;

function TKExtWindowToolController.GetConfirmJsonData: string;
begin
  Result := '{}';
end;

procedure TKExtWindowToolController.InitDefaults;
begin
  inherited;
  Modal := True;
  Closable := False;
  Layout := lyFit;
end;

procedure TKExtWindowToolController.InitSubController(const AController: IJSController);
var
  LSubject: IEFSubject;
begin
  inherited;
  FSubController := AController;
  if Supports(FSubController.AsObject, IEFSubject, LSubject) then
    LSubject.AttachObserver(Self);
end;

procedure TKExtWindowToolController.SetWindowSize;
begin
  Width := Config.GetInteger('WindowWidth', DEFAULT_WINDOW_TOOL_WIDTH);
  Height := Config.GetInteger('WindowHeight', DEFAULT_WINDOW_TOOL_HEIGHT);
  Resizable := False;
end;

procedure TKExtWindowToolController.UpdateObserver(const ASubject: IEFSubject;
  const AContext: string);
begin
  inherited;
  if (ASubject.AsObject = FSubController.AsObject) then
  begin
    if AContext = 'Confirmed' then
      Confirm
    else if AContext = 'Canceled' then
      Cancel;
  end;
end;

{ TKExtButton }

function TKExtButton.FindOwnerToolbar: TKExtToolbar;
begin
  if (Owner is TJSObjectArray) and (TJSObjectArray(Owner).Owner is TKExtToolbar) then
    Result := TKExtToolbar(TJSObjectArray(Owner).Owner)
  else
    Result := nil;
end;

function TKExtButton.GetOwnerToolbar: TKExtToolbar;
begin
  Result := FindOwnerToolbar;
  if Result = nil then
    raise Exception.Create('Owner Toolbar not found');
end;

procedure TKExtButton.InitDefaults;
begin
  inherited;
  &On('click', GenerateAnonymousFunction('comp', 'setAnimationOrigin(comp.id)'));
end;

procedure TKExtButton.SetIconAndScale(const AIconName: string; const AScale: string);
var
  LIconURL: string;
  LToolbar: TKExtToolbar;
  LIconName: string;
  LIconFileName: string;
begin
  LToolbar := FindOwnerToolbar;

  if AScale <> '' then
    Scale := AScale
  else if Assigned(LToolbar) and (LToolbar.ButtonScale <> '') then
    Scale := LToolbar.ButtonScale
  else
    Scale := 'small';

  if AIconName <> '' then
  begin
    LIconName := SmartConcat(AIconName, '_', Scale);
    LIconURL := TKWebApplication.Current.FindImageURL(LIconName);
    LIconFileName := TKWebApplication.Current.FindImagePathName(LIconName);
    if LIconFileName = '' then
      LIconURL := TKWebApplication.Current.FindImageURL(AIconName);
    Icon := LIconURL;
  end;
end;

{ TKExtToolbar }

destructor TKExtToolbar.Destroy;
begin
  inherited;
end;

function TKExtToolbar.FindButton(const AUniqueId: string): TKExtButton;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Items.Count - 1 do
  begin
    if (Items[I] is TKExtButton) and SameText(TKExtButton(Items[I]).UniqueId, AUniqueId) then
     begin
       Result := TKExtButton(Items[I]);
       Break;
     end;
  end;
end;

function TKExtToolbar.GetVisibleButtonCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Items.Count - 1 do
  begin
    if Items[I] is TKExtButton and not (TKExtButton(Items[I]).Hidden) then
      Inc(Result);
  end;
end;

end.

