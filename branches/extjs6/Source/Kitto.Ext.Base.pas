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

unit Kitto.Ext.Base;

{$I Kitto.Defines.inc}

interface

uses
  SysUtils, Classes, Generics.Collections,
  ExtPascal, ExtPascalUtils, Ext, ExtForm, ExtUx,
  EF.Intf, EF.Tree, EF.ObserverIntf, EF.Classes,
  Kitto.Ext.Controller, Kitto.Metadata.Views;

const
  DEFAULT_WINDOW_WIDTH = 800;
  DEFAULT_WINDOW_HEIGHT = 600;
  DEFAULT_WINDOW_TOOL_WIDTH = 600;
  DEFAULT_WINDOW_TOOL_HEIGHT = 400;

type
  TKExtContainerHelper = class helper for TExtContainer
  public
    procedure Apply(const AProc: TProc<TExtObject>);
  end;

  /// <summary>
  ///  Base Ext window with subject, observer and controller capabilities.
  /// </summary>
  TKExtWindowControllerBase = class(TExtWindow, IInterface, IEFInterface, IEFSubject, IEFObserver, IKExtController)
  strict private
    FSubjObserverImpl: TEFSubjectAndObserver;
    FView: TKView;
    FConfig: TEFNode;
    FContainer: TExtContainer;
    function GetView: TKView;
    function GetConfig: TEFNode;
  strict protected
    procedure SetView(const AValue: TKView);
    procedure DoDisplay; virtual;
    function GetContainer: TExtContainer;
    procedure SetContainer(const AValue: TExtContainer);
    procedure InitDefaults; override;
    function GetControllerToRemove: TObject; virtual;
    function CreateSubController: IKExtController; virtual;
    procedure InitSubController(const AController: IKExtController); virtual;
  public
    destructor Destroy; override;

    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function AsObject: TObject;
    procedure AttachObserver(const AObserver: IEFObserver); virtual;
    procedure DetachObserver(const AObserver: IEFObserver); virtual;
    procedure NotifyObservers(const AContext: string = ''); virtual;
    procedure UpdateObserver(const ASubject: IEFSubject; const AContext: string = ''); virtual;
    class function SupportsContainer: Boolean;
    function IsSynchronous: Boolean;
    property Config: TEFNode read GetConfig;

    /// <summary>
    ///  Reads the Width, Height, FullScreen properties under <APath> from ATree
    ///  and sets its size accordingly. Returns True if auto-sizing is in effect
    ///  (no explicit width and height) and False otherwise.
    /// </summry>
    function SetSizeFromTree(const ATree: TEFTree; const APath: string): Boolean;

    property View: TKView read GetView write SetView;
    procedure Display;
  published
    procedure PanelClosed;
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
    procedure HookPanel(const APanel: TExtPanel);
  published
    procedure PanelClosed;
  end;

  /// <summary>
  ///  Base ext viewport with subject, observer and controller capabilities.
  /// </summary>
  TKExtViewportControllerBase = class(TExtViewport, IInterface, IEFInterface, IEFSubject, IEFObserver, IKExtController)
  private
    FSubjObserverImpl: TEFSubjectAndObserver;
    FView: TKView;
    FConfig: TEFNode;
    FContainer: TExtContainer;
    function GetView: TKView;
    function GetConfig: TEFNode;
    procedure CreateSubController;
  protected
    procedure SetView(const AValue: TKView);
    procedure DoDisplay; virtual;
    function GetContainer: TExtContainer;
    procedure SetContainer(const AValue: TExtContainer);
    procedure InitDefaults; override;
  public
    destructor Destroy; override;

    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function AsObject: TObject;
    procedure AttachObserver(const AObserver: IEFObserver); virtual;
    procedure DetachObserver(const AObserver: IEFObserver); virtual;
    procedure NotifyObservers(const AContext: string = ''); virtual;
    procedure UpdateObserver(const ASubject: IEFSubject; const AContext: string = ''); virtual;
    class function SupportsContainer: Boolean;
    function IsSynchronous: Boolean;

    property View: TKView read GetView write SetView;
    procedure Display;
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
    property ButtonScale: string read FButtonScale write FButtonScale;
    function FindButton(const AUniqueId: string): TKExtButton;
    property VisibleButtonCount: Integer read GetVisibleButtonCount;
  end;

  /// <summary>
  ///   Base Ext panel with subject and observer capabilities.
  /// </summary>
  TKExtPanelBase = class(TExtPanel, IInterface, IEFInterface, IEFSubject, IEFObserver, IKExtActivable)
  strict private
    FSubjObserverImpl: TEFSubjectAndObserver;
    FConfig: TEFNode;
  strict protected
    function GetConfig: TEFNode;
    // Closes the hosting window or tab.
    procedure CloseHostContainer; virtual;
    function GetHostWindow: TExtWindow;
    procedure InitDefaults; override;
    procedure LoadHtml(const AFileName: string; const APostProcessor: TFunc<string, string> = nil);
  public
    destructor Destroy; override;

    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function AsObject: TObject;
    procedure AttachObserver(const AObserver: IEFObserver); virtual;
    procedure DetachObserver(const AObserver: IEFObserver); virtual;
    procedure NotifyObservers(const AContext: string = ''); virtual;
    procedure UpdateObserver(const ASubject: IEFSubject; const AContext: string = ''); virtual;
    procedure Activate; virtual;

    property Config: TEFNode read GetConfig;
  end;

  TKExtButton = class(TExtButton, IEFSubject, IEFObserver)
  strict private
    FSubjObserverImpl: TEFSubjectAndObserver;
    FUniqueId: string;
  strict protected
    function FindOwnerToolbar: TKExtToolbar;
    function GetOwnerToolbar: TKExtToolbar;
  protected
    procedure InitDefaults; override;
  public
    destructor Destroy; override;
    function AsObject: TObject;
    procedure AttachObserver(const AObserver: IEFObserver); virtual;
    procedure DetachObserver(const AObserver: IEFObserver); virtual;
    procedure NotifyObservers(const AContext: string = ''); virtual;
    procedure UpdateObserver(const ASubject: IEFSubject; const AContext: string = ''); virtual;
    // Unique Id of the button in its toolbar (if any).
    property UniqueId: string read FUniqueId write FUniqueId;
    procedure SetIconAndScale(const AIconName: string; const AScale: string = '');
  end;

  TKExtActionButton = class(TKExtButton)
  strict private
    FView: TKView;
    FActionObserver: IEFObserver;
    FOnInitController: TProc<IKExtController>;
  strict protected
    procedure InitController(const AController: IKExtController); virtual;
    procedure SetView(const AValue: TKView); virtual;
    procedure PerformBeforeExecute;
    class procedure ExecuteHandler(const AButton: TKExtButton);
  public
    property View: TKView read FView write SetView;
    property ActionObserver: IEFObserver read FActionObserver write FActionObserver;
    property OnInitController: TProc<IKExtController> read FOnInitController write FOnInitController;
  published
    procedure ExecuteButtonAction; virtual;
  end;

  TKExtActionButtonClass = class of TKExtActionButton;

  TKExtPanelControllerBase = class(TKExtPanelBase, IKExtController)
  strict private
    FView: TKView;
    FContainer: TExtContainer;
    FTopToolbar: TKExtToolbar;
    procedure CreateTopToolbar;
    procedure EnsureAllSupportFiles;
  strict protected
    procedure PerformDelayedClick(const AButton: TExtButton);
    procedure ExecuteNamedAction(const AActionName: string); virtual;
    function GetConfirmCall(const AMessage: string; const AMethod: TExtProcedure): string;
    function GetDefaultSplit: Boolean; virtual;
    function GetView: TKView;
    procedure SetView(const AValue: TKView);
    procedure DoDisplay; virtual;
    function GetContainer: TExtContainer;
    procedure SetContainer(const AValue: TExtContainer);
    property Container: TExtContainer read GetContainer write SetContainer;
    procedure InitSubController(const AController: IKExtController); virtual;
    property TopToolbar: TKExtToolbar read FTopToolbar;
    procedure BeforeCreateTopToolbar; virtual;
    procedure AfterCreateTopToolbar; virtual;
    function GetDefaultAllowClose: Boolean; virtual;

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
  public
    destructor Destroy; override;
    function IsSynchronous: Boolean;
    property View: TKView read GetView write SetView;
    procedure Display;
  end;

  /// <summary>
  ///  Base class for controllers that don't have a specific visual
  ///  representation, yet can be used to render views, such as custom action
  ///  controllers.
  /// </summary>
  TKExtControllerBase = class(TExtObject, IInterface, IEFInterface, IKExtController, IEFSubject, IEFObserver)
  private
    FSubjObserverImpl: TEFSubjectAndObserver;
    FView: TKView;
    FContainer: TExtContainer;
    FConfig: TEFNode;
  protected
    function GetView: TKView;
    procedure SetView(const AValue: TKView);
    procedure DoDisplay; virtual;
    function GetContainer: TExtContainer;
    procedure SetContainer(const AValue: TExtContainer);
    procedure InitDefaults; override;
    property Container: TExtContainer read GetContainer write SetContainer;
    function GetConfig: TEFNode;
  public
    destructor Destroy; override;
    function AsObject: TObject;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function IsSynchronous: Boolean; virtual;
    property View: TKView read GetView write SetView;
    procedure Display;
    property Config: TEFNode read GetConfig;
    procedure Apply(const AProc: TProc<IKExtController>); virtual;

    procedure AttachObserver(const AObserver: IEFObserver); virtual;
    procedure DetachObserver(const AObserver: IEFObserver); virtual;
    procedure NotifyObservers(const AContext: string = ''); virtual;
    procedure UpdateObserver(const ASubject: IEFSubject; const AContext: string = ''); virtual;
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
    FSubController: IKExtController;
    procedure CreateButtons;
  strict protected
    procedure SetWindowSize; virtual;
    procedure InitDefaults; override;
    procedure DoDisplay; override;
    function GetConfirmJSCode: string; virtual;
    function GetConfirmJsonData: string; virtual;
    procedure AfterExecuteTool; virtual;
    procedure InitSubController(const AController: IKExtController); override;
    property SubController: IKExtController read FSubController;
  public
    procedure UpdateObserver(const ASubject: IEFSubject; const AContext: string = ''); override;
  published
    procedure Confirm; virtual;
    procedure Cancel;  virtual;
  end;

  TKExtFormComboBox = class(TExtFormComboBox, IInterface, IEFInterface, IEFSubject)
  private
    FSubjObserverImpl: TEFSubjectAndObserver;
  protected
    procedure InitDefaults; override;
    function GetEncodedValue: TExtFunction;
  public
    destructor Destroy; override;
    function AsObject: TObject; inline;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure AttachObserver(const AObserver: IEFObserver); virtual;
    procedure DetachObserver(const AObserver: IEFObserver); virtual;
    procedure NotifyObservers(const AContext: string = ''); virtual;
  end;

  TKExtFormTextField = class(TExtFormTextField, IInterface, IEFInterface, IEFSubject)
  private
    FSubjObserverImpl: TEFSubjectAndObserver;
  protected
    procedure InitDefaults; override;
  public
    destructor Destroy; override;
    function AsObject: TObject; inline;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure AttachObserver(const AObserver: IEFObserver); virtual;
    procedure DetachObserver(const AObserver: IEFObserver); virtual;
    procedure NotifyObservers(const AContext: string = ''); virtual;
  end;

  TKExtFormDateField = class(TExtFormDateField, IInterface, IEFInterface, IEFSubject)
  private
    FSubjObserverImpl: TEFSubjectAndObserver;
  protected
    procedure InitDefaults; override;
  public
    destructor Destroy; override;
    function AsObject: TObject; inline;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure AttachObserver(const AObserver: IEFObserver); virtual;
    procedure DetachObserver(const AObserver: IEFObserver); virtual;
    procedure NotifyObservers(const AContext: string = ''); virtual;
  end;

  TKExtFormCheckBoxField = class(TExtFormCheckBox, IInterface, IEFInterface, IEFSubject)
  private
    FSubjObserverImpl: TEFSubjectAndObserver;
  protected
    procedure InitDefaults; override;
  public
    destructor Destroy; override;
    function AsObject: TObject; inline;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure AttachObserver(const AObserver: IEFObserver); virtual;
    procedure DetachObserver(const AObserver: IEFObserver); virtual;
    procedure NotifyObservers(const AContext: string = ''); virtual;
  end;

  TKExtStatusBar = class(TExtUxStatusBar)
  public
    procedure SetErrorStatus(const AText: string);

    procedure ClearStatus; virtual;
  end;

function OptionAsLabelAlign(const AAlign: string): TExtContainerLabelAlign;
function OptionAsGridColumnAlign(const AAlign: string): TExtGridColumnAlign;

implementation

uses
  StrUtils,
  EF.StrUtils, EF.Types, EF.Localization, EF.Macros,
  Kitto.AccessControl, Kitto.Ext.Utils, Kitto.Ext.Session;

function OptionAsGridColumnAlign(const AAlign: string): TExtGridColumnAlign;
begin
  //alLeft, alRight, alCenter
  if SameText(AAlign, 'Left') then
    Result := caLeft
  else if SameText(AAlign, 'Right') then
    Result := caRight
  else if SameText(AAlign, 'Center') then
    Result := caCenter
  else
    raise EEFError.CreateFmt(_('Invalid value %s. Valid values: "Left", "Right", "Center".'), [AAlign]);
end;

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

function TKExtWindowControllerBase.AsObject: TObject;
begin
  Result := Self;
end;

procedure TKExtWindowControllerBase.AttachObserver(const AObserver: IEFObserver);
begin
  FSubjObserverImpl.AttachObserver(AObserver);
end;

function TKExtWindowControllerBase.CreateSubController: IKExtController;
var
  LSubView: TKView;
  LNode: TEFNode;
begin
  Assert(Assigned(View));

  Result := nil;
  LNode := View.FindNode('Controller/SubView');
  if Assigned(LNode) then
  begin
    LSubView := Session.Config.Views.FindViewByNode(LNode);
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
  FreeAndNil(FSubjObserverImpl);
  FreeAndNil(FConfig);
  Session.RemoveController(Self);
  inherited;
end;

procedure TKExtWindowControllerBase.DetachObserver(const AObserver: IEFObserver);
begin
  FSubjObserverImpl.DetachObserver(AObserver);
end;

procedure TKExtWindowControllerBase.Display;
begin
  DoDisplay;
  On('render', UpdateLayout);
end;

procedure TKExtWindowControllerBase.DoDisplay;
begin
  Session.EnsureSupportFiles(TKExtControllerRegistry.Instance.FindClassId(Self.ClassType));
  Session.EnsureViewSupportFiles(View);
  CreateSubController;
  Show;
end;

function TKExtWindowControllerBase.GetConfig: TEFNode;
begin
  if not Assigned(FConfig) then
    FConfig := TEFNode.Create;
  Result := FConfig;
end;

function TKExtWindowControllerBase.GetContainer: TExtContainer;
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
  FSubjObserverImpl := TEFSubjectAndObserver.Create;
  Layout := lyFit;
  if Session.IsMobileBrowser then
    Maximized := True;
  Border := False;
  Plain := True;

  On('close', Ajax(WindowClosed, ['Window', JSName]));
end;

procedure TKExtWindowControllerBase.InitSubController(const AController: IKExtController);
begin
end;

function TKExtWindowControllerBase.IsSynchronous: Boolean;
begin
  Result := False;
end;

procedure TKExtWindowControllerBase.NotifyObservers(const AContext: string);
begin
  FSubjObserverImpl.NotifyObserversOnBehalfOf(Self, AContext);
end;

procedure TKExtWindowControllerBase.PanelClosed;
begin
  NotifyObservers('Closed');
end;

procedure TKExtWindowControllerBase.SetContainer(const AValue: TExtContainer);
begin
  FContainer := AValue;
end;

function TKExtWindowControllerBase.SetSizeFromTree(const ATree: TEFTree; const APath: string): Boolean;
var
  LWidth: Integer;
  LHeight: Integer;
  LFullScreen: Boolean;
begin
  LWidth := ATree.GetInteger(APath + 'Width');
  LHeight := ATree.GetInteger(APath + 'Height');
  LFullScreen := ATree.GetBoolean(APath + 'FullScreen', Session.IsMobileBrowser);

  Result := False;
  if LFullScreen then
  begin
    Maximized := True;
    Border := not Maximized;
  end
  else if (LWidth > 0) and (LHeight > 0) then
  begin
    Width := LWidth;
    Height := LHeight;
  end
  else
    Result := True;
end;

procedure TKExtWindowControllerBase.SetView(const AValue: TKView);
begin
  FView := AValue;
end;

class function TKExtWindowControllerBase.SupportsContainer: Boolean;
begin
  Result := False;
end;

procedure TKExtWindowControllerBase.UpdateObserver(const ASubject: IEFSubject;
  const AContext: string);
begin
end;

procedure TKExtWindowControllerBase.WindowClosed;
begin
  Session.RemoveController(GetControllerToRemove);
end;

function TKExtWindowControllerBase.GetControllerToRemove: TObject;
begin
  Result := Self;
end;

function TKExtWindowControllerBase._AddRef: Integer;
begin
  Result := -1;
end;

function TKExtWindowControllerBase._Release: Integer;
begin
  Result := -1;
end;

{ TKExtPanelBase }

procedure TKExtPanelBase.Activate;
begin
end;

function TKExtPanelBase.AsObject: TObject;
begin
  Result := Self;
end;

procedure TKExtPanelBase.AttachObserver(const AObserver: IEFObserver);
begin
  FSubjObserverImpl.AttachObserver(AObserver);
end;

destructor TKExtPanelBase.Destroy;
begin
  FreeAndNil(FSubjObserverImpl);
  FreeAndNil(FConfig);
  inherited;
end;

procedure TKExtPanelBase.DetachObserver(const AObserver: IEFObserver);
begin
  FSubjObserverImpl.DetachObserver(AObserver);
end;

function TKExtPanelBase.GetConfig: TEFNode;
begin
  if not Assigned(FConfig) then
    FConfig := TEFNode.Create;
  Result := FConfig;
end;

procedure TKExtPanelBase.NotifyObservers(const AContext: string);
begin
  FSubjObserverImpl.NotifyObserversOnBehalfOf(Self, AContext);
end;

procedure TKExtPanelBase.UpdateObserver(const ASubject: IEFSubject;
  const AContext: string);
begin
end;

function TKExtPanelBase._AddRef: Integer;
begin
  Result := -1;
end;

function TKExtPanelBase._Release: Integer;
begin
  Result := -1;
end;

function TKExtPanelBase.GetHostWindow: TExtWindow;
begin
  Result := Config.GetObject('Sys/HostWindow') as TExtWindow;
end;

procedure TKExtPanelBase.InitDefaults;
begin
  inherited;
  FSubjObserverImpl := TEFSubjectAndObserver.Create;
  Region := rgCenter;
end;

procedure TKExtPanelBase.LoadHtml(const AFileName: string;
  const APostProcessor: TFunc<string, string>);
var
  LFullFileName: string;
  LHtml: string;
begin
  LFullFileName := Session.Config.FindResourcePathName(AFileName);
  if LFullFileName <> '' then
    LHtml := TEFMacroExpansionEngine.Instance.Expand(TextFileToString(LFullFileName, TEncoding.UTF8))
  else
    LHtml := '';
  if Assigned(APostProcessor) then
    LHtml := APostProcessor(LHtml);
  Html := LHtml;
end;

procedure TKExtPanelBase.CloseHostContainer;
var
  LHostWindow: TExtWindow;
  LIntf: IKExtPanelHost;
begin
  { TODO : Perhaps we could unify the behaviour here by implementing IKExtPanelHost in a custom window class. }
  LHostWindow := GetHostWindow;
  if Assigned(LHostWindow) then
    LHostWindow.Close
  else if Supports(Owner, IKExtPanelHost, LIntf) then
    LIntf.ClosePanel(Self);
end;

{ TKExtViewportControllerBase }

function TKExtViewportControllerBase.AsObject: TObject;
begin
  Result := Self;
end;

procedure TKExtViewportControllerBase.AttachObserver(const AObserver: IEFObserver);
begin
  FSubjObserverImpl.AttachObserver(AObserver);
end;

destructor TKExtViewportControllerBase.Destroy;
begin
  FreeAndNil(FSubjObserverImpl);
  FreeAndNil(FConfig);
  Session.RemoveController(Self);
  inherited;
end;

procedure TKExtViewportControllerBase.DetachObserver(const AObserver: IEFObserver);
begin
  FSubjObserverImpl.DetachObserver(AObserver);
end;

procedure TKExtViewportControllerBase.Display;
begin
  DoDisplay;
  On('render', UpdateLayout);
end;

procedure TKExtViewportControllerBase.DoDisplay;
begin
  Session.EnsureSupportFiles(TKExtControllerRegistry.Instance.FindClassId(Self.ClassType));
  Session.EnsureViewSupportFiles(View);
  CreateSubController;
  Show;
end;

function TKExtViewportControllerBase.GetConfig: TEFNode;
begin
  if not Assigned(FConfig) then
    FConfig := TEFNode.Create;
  Result := FConfig;
end;

function TKExtViewportControllerBase.GetContainer: TExtContainer;
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
  FSubjObserverImpl := TEFSubjectAndObserver.Create;
  Layout := lyBorder;
end;

function TKExtViewportControllerBase.IsSynchronous: Boolean;
begin
  Result := False;
end;

procedure TKExtViewportControllerBase.NotifyObservers(const AContext: string);
begin
  FSubjObserverImpl.NotifyObserversOnBehalfOf(Self, AContext);
end;

procedure TKExtViewportControllerBase.SetContainer(const AValue: TExtContainer);
begin
  FContainer := AValue;
end;

procedure TKExtViewportControllerBase.SetView(const AValue: TKView);
begin
  FView := AValue;
end;

class function TKExtViewportControllerBase.SupportsContainer: Boolean;
begin
  Result := False;
end;

procedure TKExtViewportControllerBase.UpdateObserver(const ASubject: IEFSubject;
  const AContext: string);
begin
end;

function TKExtViewportControllerBase._AddRef: Integer;
begin
  Result := -1;
end;

function TKExtViewportControllerBase._Release: Integer;
begin
  Result := -1;
end;

procedure TKExtViewportControllerBase.CreateSubController;
var
  LSubView: TKView;
  LController: IKExtController;
  LNode: TEFNode;
begin
  Assert(Assigned(View));

  LNode := View.FindNode('Controller/SubView');
  if Assigned(LNode) then
  begin
    LSubView := Session.Config.Views.FindViewByNode(LNode);
    if Assigned(LSubView) then
    begin
      LController := TKExtControllerFactory.Instance.CreateController(Self, LSubView, Self);
      LController.Display;
    end;
  end;
end;

{ TKExtModalWindow }

procedure TKExtModalWindow.HookPanel(const APanel: TExtPanel);
begin
  Assert(Assigned(APanel));

  APanel.On('close', Ajax(PanelClosed, ['Panel', APanel.JSName]));
end;

procedure TKExtModalWindow.InitDefaults;
begin
  inherited;
  Width := DEFAULT_WINDOW_WIDTH;
  Height := DEFAULT_WINDOW_HEIGHT;
  Closable := False;
  Modal := True;
end;

procedure TKExtModalWindow.PanelClosed;
begin
  Close;
end;

{ TKExtFormComboBox }

function TKExtFormComboBox.AsObject: TObject;
begin
  Result := Self;
end;

procedure TKExtFormComboBox.AttachObserver(const AObserver: IEFObserver);
begin
  FSubjObserverImpl.AttachObserver(AObserver);
end;

destructor TKExtFormComboBox.Destroy;
begin
  FreeAndNil(FSubjObserverImpl);
  inherited;
end;

procedure TKExtFormComboBox.DetachObserver(const AObserver: IEFObserver);
begin
  FSubjObserverImpl.DetachObserver(AObserver);
end;

function TKExtFormComboBox.GetEncodedValue: TExtFunction;
begin
  Session.ResponseItems.ExecuteJSCode(Self, Format('encodeURI(%s.getValue())', [JSName]));
  Result := Self;
end;

procedure TKExtFormComboBox.InitDefaults;
begin
  inherited;
  FSubjObserverImpl := TEFSubjectAndObserver.Create;
end;

procedure TKExtFormComboBox.NotifyObservers(const AContext: string);
begin
  FSubjObserverImpl.NotifyObserversOnBehalfOf(Self, AContext);
end;

function TKExtFormComboBox._AddRef: Integer;
begin
  Result := -1;
end;

function TKExtFormComboBox._Release: Integer;
begin
  Result := -1;
end;

{ TKExtPanelControllerBase }

destructor TKExtPanelControllerBase.Destroy;
begin
  Session.RemoveController(Self);
  inherited;
end;

procedure TKExtPanelControllerBase.Display;
begin
  if Container <> nil then
  begin
    if Config.GetBoolean('AllowClose', GetDefaultAllowClose) then
    begin
      Closable := True;
      On('close', Container.Ajax('PanelClosed', ['Panel', JSName]));
    end
    else
      Closable := False;
  end;
  DoDisplay;
end;

procedure TKExtPanelControllerBase.EnsureAllSupportFiles;
var
  LClassType: TClass;
begin
  LClassType := ClassType;
  Session.EnsureSupportFiles(TKExtControllerRegistry.Instance.FindClassId(LClassType));
  while LClassType.ClassParent <> nil do
  begin
    LClassType := LClassType.ClassParent;
    Session.EnsureSupportFiles(TKExtControllerRegistry.Instance.FindClassId(LClassType));
  end;
  Session.EnsureViewSupportFiles(View);
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
begin
  EnsureAllSupportFiles;

  if Title = '' then
  begin
    LView := View;
    if Assigned(LView) then
      Title := _(Config.GetExpandedString('Title', LView.DisplayLabel));
  end;

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
    WidthString := LWidthStr;

  LHeightStr := Config.GetString('Height');
  if TryStrToInt(LHeightStr, LHeight) then
  begin
    if LHeight > 0 then
      Height := LHeight
    else if LHeight = -1 then
      AutoHeight := True;
  end
  else if LHeightStr <> '' then
    HeightString := LHeightStr;

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

  LHeader := Config.FindNode('Header');
  if Assigned(LHeader) then
    Header := LHeader.AsBoolean
  else
    Header := False;

  CreateTopToolbar;

  LBodyStyle := Config.GetExpandedString('BodyStyle');
  if LBodyStyle <> '' then
    BodyStyle := LBodyStyle;
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

  Result := TKExtActionButton.CreateAndAddTo(AToolbar.Items);
  Result.Hidden := not AView.GetBoolean('IsVisible', True);
  Result.UniqueId := AUniqueId;
  Result.View := AView;
  Result.ActionObserver := Self;

  // A Tool may or may not have a confirmation message.
  LConfirmationMessage := AView.GetExpandedString('Controller/ConfirmationMessage');
  // Cleanup Linebreaks with <br> tag
  LConfirmationMessage := StringReplace(LConfirmationMessage, sLineBreak, '<br>',[rfReplaceAll]);
  LConfirmationJS := GetConfirmCall(LConfirmationMessage, Result.ExecuteButtonAction);
  if LConfirmationMessage <> '' then
    Result.On('click', JSFunction(LConfirmationJS))
  else
    Result.On('click', Ajax(Result.ExecuteButtonAction, []));
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
    TExtToolbarSeparator.CreateAndAddTo(AToolbar.Items);
    for I := 0 to AConfigNode.ChildCount - 1 do
    begin
      LNode := AConfigNode.Children[I];
      LView := Session.Config.Views.ViewByNode(LNode);
      if LView.IsAccessGranted(ACM_VIEW) then
        AddActionButton(LNode.Name, LView, AToolbar);
    end;
  end;
end;

function TKExtPanelControllerBase.GetConfirmCall(const AMessage: string; const AMethod: TExtProcedure): string;
begin
  Result := Format('confirmCall("%s", "%s", ajaxSimple, {methodURL: "%s"});',
    [_(Session.Config.AppTitle), AMessage, MethodURI(AMethod)]);
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

procedure TKExtPanelControllerBase.CreateTopToolbar;
begin
  BeforeCreateTopToolbar;

  FTopToolbar := TKExtToolbar.Create(Self);
  try
    FTopToolbar.ButtonScale := Config.GetString('ToolButtonScale',
      IfThen(Session.IsMobileBrowser, 'large', 'small'));
    FTopToolbar.AutoScroll := Session.IsMobileBrowser;
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

function TKExtPanelControllerBase.GetContainer: TExtContainer;
begin
  Result := FContainer;
end;

function TKExtPanelControllerBase.GetView: TKView;
begin
  Result := FView;
end;

procedure TKExtPanelControllerBase.InitSubController(const AController: IKExtController);
var
  LSysConfigNode: TEFNode;
begin
  Assert(Assigned(AController));

  LSysConfigNode := Config.FindNode('Sys');
  if Assigned(LSysConfigNode) then
    AController.Config.GetNode('Sys', True).Assign(LSysConfigNode);
end;

function TKExtPanelControllerBase.IsSynchronous: Boolean;
begin
  Result := False;
end;

procedure TKExtPanelControllerBase.PerformDelayedClick(const AButton: TExtButton);
begin
  if Assigned(AButton) then
    AButton.On('render', JSFunction(AButton.PerformClick));
end;

procedure TKExtPanelControllerBase.SetContainer(const AValue: TExtContainer);
begin
  FContainer := AValue;
end;

procedure TKExtPanelControllerBase.SetView(const AValue: TKView);
begin
  FView := AValue;
end;

{ TKExtFormTextField }

function TKExtFormTextField.AsObject: TObject;
begin
  Result := Self;
end;

procedure TKExtFormTextField.AttachObserver(const AObserver: IEFObserver);
begin
  FSubjObserverImpl.AttachObserver(AObserver);
end;

destructor TKExtFormTextField.Destroy;
begin
  FreeAndNil(FSubjObserverImpl);
  inherited;
end;

procedure TKExtFormTextField.DetachObserver(const AObserver: IEFObserver);
begin
  FSubjObserverImpl.DetachObserver(AObserver);
end;

procedure TKExtFormTextField.InitDefaults;
begin
  inherited;
  FSubjObserverImpl := TEFSubjectAndObserver.Create;
end;

procedure TKExtFormTextField.NotifyObservers(const AContext: string);
begin
  FSubjObserverImpl.NotifyObserversOnBehalfOf(Self, AContext);
end;

function TKExtFormTextField._AddRef: Integer;
begin
  Result := -1;
end;

function TKExtFormTextField._Release: Integer;
begin
  Result := -1;
end;

{ TKExtFormDateField }

function TKExtFormDateField.AsObject: TObject;
begin
  Result := Self;
end;

procedure TKExtFormDateField.AttachObserver(const AObserver: IEFObserver);
begin
  FSubjObserverImpl.AttachObserver(AObserver);
end;

destructor TKExtFormDateField.Destroy;
begin
  FreeAndNil(FSubjObserverImpl);
  inherited;
end;

procedure TKExtFormDateField.DetachObserver(const AObserver: IEFObserver);
begin
  FSubjObserverImpl.DetachObserver(AObserver);
end;

procedure TKExtFormDateField.InitDefaults;
begin
  inherited;
  FSubjObserverImpl := TEFSubjectAndObserver.Create;
end;

procedure TKExtFormDateField.NotifyObservers(const AContext: string);
begin
  FSubjObserverImpl.NotifyObserversOnBehalfOf(Self, AContext);
end;

function TKExtFormDateField._AddRef: Integer;
begin
  Result := -1;
end;

function TKExtFormDateField._Release: Integer;
begin
  Result := -1;
end;

{ TKExtFormCheckBoxField }

function TKExtFormCheckBoxField.AsObject: TObject;
begin
  Result := Self;
end;

procedure TKExtFormCheckBoxField.AttachObserver(const AObserver: IEFObserver);
begin
  FSubjObserverImpl.AttachObserver(AObserver);
end;

destructor TKExtFormCheckBoxField.Destroy;
begin
  FreeAndNil(FSubjObserverImpl);
  inherited;
end;

procedure TKExtFormCheckBoxField.DetachObserver(const AObserver: IEFObserver);
begin
  FSubjObserverImpl.DetachObserver(AObserver);
end;

procedure TKExtFormCheckBoxField.InitDefaults;
begin
  inherited;
  FSubjObserverImpl := TEFSubjectAndObserver.Create;
end;

procedure TKExtFormCheckBoxField.NotifyObservers(const AContext: string);
begin
  FSubjObserverImpl.NotifyObserversOnBehalfOf(Self, AContext);
end;

function TKExtFormCheckBoxField._AddRef: Integer;
begin
  Result := -1;
end;

function TKExtFormCheckBoxField._Release: Integer;
begin
  Result := -1;
end;

{ TKExtStatusBar }

procedure TKExtStatusBar.ClearStatus;
begin
  inherited ClearStatus;
end;

procedure TKExtStatusBar.SetErrorStatus(const AText: string);
begin
  SetStatus(JSObject(Format('text: "%s", iconCls:"x-status-error",clear:true', [AText])));
end;

{ TKExtControllerBase }

procedure TKExtControllerBase.Apply(const AProc: TProc<IKExtController>);
begin
  Assert(Assigned(AProc));

  AProc(Self);
end;

function TKExtControllerBase.AsObject: TObject;
begin
  Result := Self;
end;

procedure TKExtControllerBase.AttachObserver(const AObserver: IEFObserver);
begin
  FSubjObserverImpl.AttachObserver(AObserver);
end;

destructor TKExtControllerBase.Destroy;
begin
  FreeAndNil(FSubjObserverImpl);
  FreeAndNil(FConfig);
  Session.RemoveController(Self);
  inherited;
end;

procedure TKExtControllerBase.DetachObserver(const AObserver: IEFObserver);
begin
  FSubjObserverImpl.DetachObserver(AObserver);
end;

procedure TKExtControllerBase.Display;
begin
  DoDisplay;
end;

procedure TKExtControllerBase.DoDisplay;
begin
  Session.EnsureSupportFiles(TKExtControllerRegistry.Instance.FindClassId(Self.ClassType));
  Session.EnsureViewSupportFiles(View);
end;

function TKExtControllerBase.GetConfig: TEFNode;
begin
  if not Assigned(FConfig) then
    FConfig := TEFNode.Create;
  Result := FConfig;
end;

function TKExtControllerBase.GetContainer: TExtContainer;
begin
  Result := FContainer;
end;

function TKExtControllerBase.GetView: TKView;
begin
  Result := FView;
end;

procedure TKExtControllerBase.InitDefaults;
begin
  inherited;
  FSubjObserverImpl := TEFSubjectAndObserver.Create;
end;

function TKExtControllerBase.IsSynchronous: Boolean;
begin
  Result := False;
end;

procedure TKExtControllerBase.NotifyObservers(const AContext: string);
begin
  FSubjObserverImpl.NotifyObserversOnBehalfOf(Self, AContext);
end;

procedure TKExtControllerBase.SetContainer(const AValue: TExtContainer);
begin
  FContainer := AValue;
end;

procedure TKExtControllerBase.SetView(const AValue: TKView);
begin
  FView := AValue;
end;

procedure TKExtControllerBase.UpdateObserver(const ASubject: IEFSubject;
  const AContext: string);
begin
end;

function TKExtControllerBase._AddRef: Integer;
begin
  Result := 0;
end;

function TKExtControllerBase._Release: Integer;
begin
  Result := 0;
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
  //NotifyObservers('Closed');
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
  LController: IKExtController;
begin
  Assert(Assigned(FView));
  Assert(Assigned(FActionObserver));

  LController := TKExtControllerFactory.Instance.CreateController(
    Session.ObjectCatalog, FView, nil, nil, FActionObserver);
  InitController(LController);
  LController.Display;
end;

class procedure TKExtActionButton.ExecuteHandler(const AButton: TKExtButton);
var
  LResponseItemBranch: TExtResponseItems;
begin
  if AButton is TKExtActionButton then
  begin
    LResponseItemBranch := AButton.Session.BranchResponseItems;
    try
      TKExtActionButton(AButton).ExecuteButtonAction;
    finally
      AButton.Session.UnbranchResponseItems(LResponseItemBranch, False); // throw away
    end;
  end;
end;

procedure TKExtActionButton.InitController(const AController: IKExtController);
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
    for I := 0 to LBeforeExecuteNode.ChildCount-1 do
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
begin
  FConfirmButton := TKExtButton.CreateAndAddTo(Buttons);
  FConfirmButton.SetIconAndScale('accept', Config.GetString('ButtonScale', 'medium'));
  FConfirmButton.FormBind := True;
  FConfirmButton.Text := Config.GetString('ConfirmButton/Caption', _('Confirm'));
  FConfirmButton.Tooltip := Config.GetString('ConfirmButton/Tooltip', _('Confirm action and close window'));
  FConfirmButton.Handler := JSFunction(GetConfirmJSCode());

  FCancelButton := TKExtButton.CreateAndAddTo(Buttons);
  FCancelButton.SetIconAndScale('cancel', Config.GetString('ButtonScale', 'medium'));
  FCancelButton.Text := _('Cancel');
  FCancelButton.Tooltip := _('Cancel changes');
  FCancelButton.Handler := Ajax(Cancel);
end;

procedure TKExtWindowToolController.DoDisplay;
begin
  inherited;
  Title := View.DisplayLabel;
  SetWindowSize;
  if not Config.GetBoolean('HideButtons') then
    CreateButtons;
end;

function TKExtWindowToolController.GetConfirmJSCode: string;
begin
  Result := GetPOSTAjaxCode(Confirm, [], GetConfirmJsonData);
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

procedure TKExtWindowToolController.InitSubController(const AController: IKExtController);
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

function TKExtButton.AsObject: TObject;
begin
  Result := Self;
end;

procedure TKExtButton.AttachObserver(const AObserver: IEFObserver);
begin
  FSubjObserverImpl.AttachObserver(AObserver);
end;

destructor TKExtButton.Destroy;
begin
  FreeAndNil(FSubjObserverImpl);
  inherited;
end;

procedure TKExtButton.DetachObserver(const AObserver: IEFObserver);
begin
  FSubjObserverImpl.DetachObserver(AObserver);
end;

function TKExtButton.FindOwnerToolbar: TKExtToolbar;
begin
  if (Owner is TExtObjectList) and (TExtObjectList(Owner).Owner is TKExtToolbar) then
    Result := TKExtToolbar(TExtObjectList(Owner).Owner)
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
  FSubjObserverImpl := TEFSubjectAndObserver.Create;
end;

procedure TKExtButton.NotifyObservers(const AContext: string);
begin
  FSubjObserverImpl.NotifyObserversOnBehalfOf(Self, AContext);
end;

procedure TKExtButton.SetIconAndScale(const AIconName: string; const AScale: string);
var
  LIconURL: string;
  LToolbar: TKExtToolbar;
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
    LIconURL := Session.Config.FindImageURL(SmartConcat(AIconName, '_', Scale));
    if LIconURL = '' then
      LIconURL := Session.Config.FindImageURL(AIconName);

    Icon := LIconURL;
  end;
end;

procedure TKExtButton.UpdateObserver(const ASubject: IEFSubject;
  const AContext: string);
begin
end;

{ TKExtToolbar }

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

