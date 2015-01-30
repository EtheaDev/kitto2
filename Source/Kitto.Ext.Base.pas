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

type
  TKExtContainerHelper = class helper for TExtContainer
  public
    procedure Apply(const AProc: TProc<TExtObject>);
  end;

  ///	<summary>
  ///	 Base Ext window with subject, observer and controller capabilities.
  ///	</summary>
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
  public
    destructor Destroy; override;

    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function AsObject: TObject;
    procedure AttachObserver(const AObserver: IEFObserver); virtual;
    procedure DetachObserver(const AObserver: IEFObserver); virtual;
    procedure NotifyObservers(const AContext: string = ''); virtual;
    procedure UpdateObserver(const ASubject: IEFSubject; const AContext: string = ''); virtual;
    function SupportsContainer: Boolean;
    function IsSynchronous: Boolean;
    property Config: TEFNode read GetConfig;

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
    ///	<summary>
    ///  Call this after adding the panel so that the window can hook its
    ///  beforeclose event and close itself.
    ///	<summary>
    procedure HookPanel(const APanel: TExtPanel);
  published
    procedure PanelClosed;
  end;

  ///	<summary>
  ///	 Base ext viewport with subject, observer and controller capabilities.
  ///	</summary>
  TKExtViewportControllerBase = class(TExtViewport, IInterface, IEFInterface, IEFSubject, IEFObserver, IKExtController)
  private
    FSubjObserverImpl: TEFSubjectAndObserver;
    FView: TKView;
    FConfig: TEFNode;
    FContainer: TExtContainer;
    function GetView: TKView;
    function GetConfig: TEFNode;
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
    function SupportsContainer: Boolean;
    function IsSynchronous: Boolean;

    property View: TKView read GetView write SetView;
    procedure Display;
  end;

  ///	<summary>
  ///	  Implemented by controllers that host panels and are able to close them
  ///	  on request, such as the TabPanel controller.
  ///	</summary>
  IKExtPanelHost = interface(IEFInterface)
    ['{F1DCE0C8-1CD9-4F97-9315-7FB2AC9CAADC}']
    procedure ClosePanel(const APanel: TExtComponent);
  end;

  ///	<summary>
  ///	  Base Ext panel with subject and observer capabilities.
  ///	</summary>
  TKExtPanelBase = class(TExtPanel, IInterface, IEFInterface, IEFSubject, IEFObserver)
  private
    FSubjObserverImpl: TEFSubjectAndObserver;
    FConfig: TEFNode;
  protected
    function GetConfig: TEFNode;
    // Closes the hosting window or tab.
    procedure CloseHostContainer;
    function GetHostWindow: TExtWindow;
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

    property Config: TEFNode read GetConfig;
  end;

  TKExtActionButton = class(TExtButton)
  strict private
    FView: TKView;
    FActionObserver: IEFObserver;
  strict protected
    procedure InitController(const AController: IKExtController); virtual;
    procedure SetView(const AValue: TKView); virtual;
  public
    property View: TKView read FView write SetView;
    property ActionObserver: IEFObserver read FActionObserver write FActionObserver;
  published
    procedure ExecuteButtonAction;
  end;

  TKExtActionButtonClass = class of TKExtActionButton;

  TKExtPanelControllerBase = class(TKExtPanelBase, IKExtController)
  strict private
    FView: TKView;
    FContainer: TExtContainer;
    FTopToolbar: TExtToolbar;
    FActionButtons: TDictionary<string, TKExtActionButton>;
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
    property TopToolbar: TExtToolbar read FTopToolbar;
    procedure BeforeCreateTopToolbar; virtual;
    procedure AfterCreateTopToolbar; virtual;

    ///	<summary>Adds built-in buttons to the top toolbar.</summary>
    procedure AddTopToolbarButtons; virtual;

    ///	<summary>Adds ToolView buttons to the top toolbar. Called after
    ///	AddTopToolbarButtons so that these stay at the end.</summary>
    procedure AddTopToolbarToolViewButtons; virtual;

    ///	<summary>Adds to the specified toolbar buttons for any ToolViews
    ///	configured in the specified node.</summary>
    ///	<param name="AConfigNode">ToolViews node. If nil or childrenless, no
    ///	buttons are added.</param>
    ///	<param name="AToolbar">Destination toolbar.</param>
    procedure AddToolViewButtons(const AConfigNode: TEFNode; const AToolbar: TExtToolbar);

    ///	<summary>Adds an action button representing the specified tool view to
    ///	the specified toolbar. Override this method to create action buttons of
    ///	classes inherited from the base TKExtActionButton.</summary>
    function AddActionButton(const AView: TKView;
      const AToolbar: TExtToolbar): TKExtActionButton; virtual;

    ///	<summary>
    ///	  Holds the list of named Buttons added to the Panel
    ///	</summary>
    property ActionButtons: TDictionary<string, TKExtActionButton> read FActionButtons;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
    function SupportsContainer: Boolean; virtual;
    function IsSynchronous: Boolean;
    property View: TKView read GetView write SetView;
    procedure Display;
  end;

  ///	<summary>
  ///  Base class for controllers that don't have a specific visual
  ///	 representation, yet can be used to render views, such as custom action
  ///	 controllers.
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
    function SupportsContainer: Boolean; virtual;
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

  ///	<summary>
  ///  Base class for tool controllers.
  /// </summary>
  TKExtToolController = class(TKExtControllerBase)
  strict protected
    procedure ExecuteTool; virtual;
    procedure AfterExecuteTool; virtual;
    procedure DoDisplay; override;
  public
    function SupportsContainer: Boolean; override;
    function IsSynchronous: Boolean; override;
  end;

  ///	<summary>
  ///  Base class for tool controllers that display a (modal) window with
  ///  a set of ok/cancel buttons.
  /// </summary>
  TKExtWindowToolController = class(TKExtWindowControllerBase)
  strict private
    FConfirmButton: TExtButton;
    FCancelButton: TExtButton;
    procedure CreateButtons;
  strict protected
    procedure SetWindowSize; virtual;
    procedure InitDefaults; override;
    procedure DoDisplay; override;
    function GetConfirmJSCode: string; virtual;
    procedure AfterExecuteTool; virtual;
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

function OptionAsLabelAlign(const AAlign: string): TExtFormFormPanelLabelAlign;
function OptionAsGridColumnAlign(const AAlign: string): TExtGridColumnAlign;

implementation

uses
  StrUtils,
  EF.StrUtils, EF.Types, EF.Localization,
  Kitto.Ext.Utils, Kitto.Ext.Session;

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

function OptionAsLabelAlign(const AAlign: string): TExtFormFormPanelLabelAlign;
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
  On('render', DoLayout);
end;

procedure TKExtWindowControllerBase.DoDisplay;
begin
  Session.EnsureSupportFiles(TKExtControllerRegistry.Instance.FindClassId(Self.ClassType));
  Session.EnsureViewSupportFiles(View);
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
  Layout := lyBorder;
  Border := False;
  Plain := True;

  On('close', Ajax(WindowClosed, ['Window', JSName]));
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

procedure TKExtWindowControllerBase.SetView(const AValue: TKView);
begin
  FView := AValue;
end;

function TKExtWindowControllerBase.SupportsContainer: Boolean;
begin
  Result := False;
end;

procedure TKExtWindowControllerBase.UpdateObserver(const ASubject: IEFSubject;
  const AContext: string);
begin
end;

procedure TKExtWindowControllerBase.WindowClosed;
begin
  Session.RemoveController(GetControllerToRemove, True);
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
  On('render', DoLayout);
end;

procedure TKExtViewportControllerBase.DoDisplay;
begin
  Session.EnsureSupportFiles(TKExtControllerRegistry.Instance.FindClassId(Self.ClassType));
  Session.EnsureViewSupportFiles(View);
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

function TKExtViewportControllerBase.SupportsContainer: Boolean;
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

{ TKExtModalWindow }

procedure TKExtModalWindow.HookPanel(const APanel: TExtPanel);
begin
  Assert(Assigned(APanel));

  APanel.On('close', Ajax(PanelClosed, ['Panel', APanel.JSName]));
end;

procedure TKExtModalWindow.InitDefaults;
begin
  inherited;
  Width := 600;
  Height := 400;
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
  ExtSession.ResponseItems.ExecuteJSCode(Self, Format('encodeURI(%s.getValue())', [JSName]));
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
  FActionButtons.Free;
  inherited;
end;

procedure TKExtPanelControllerBase.Display;
begin
  if Container <> nil then
  begin
    if Config.GetBoolean('AllowClose', True) then
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
end;

procedure TKExtPanelControllerBase.ExecuteNamedAction(const AActionName: string);
var
  LToolButton: TKExtActionButton;
begin
  if ActionButtons.TryGetValue(AActionName, LToolButton) then
    PerformDelayedClick(LToolButton);
end;

procedure TKExtPanelControllerBase.AddTopToolbarButtons;
begin
end;

function TKExtPanelControllerBase.AddActionButton(
  const AView: TKView; const AToolbar: TExtToolbar): TKExtActionButton;
var
  LConfirmationMessage: string;
  LConfirmationJS: string;
begin
  Assert(Assigned(AView));
  Assert(Assigned(AToolbar));

  Result := TKExtActionButton.CreateAndAddTo(AToolbar.Items);
  Result.View := AView;
  Result.ActionObserver := Self;

  // A Tool may or may not have a confirmation message.
  LConfirmationMessage := AView.GetExpandedString('Controller/ConfirmationMessage');
  LConfirmationJS := GetConfirmCall(LConfirmationMessage, Result.ExecuteButtonAction);
  if LConfirmationMessage <> '' then
    Result.Handler := JSFunction(LConfirmationJS)
  else
    Result.On('click', Ajax(Result.ExecuteButtonAction, []));
end;

procedure TKExtPanelControllerBase.AddToolViewButtons(
  const AConfigNode: TEFNode; const AToolbar: TExtToolbar);
var
  I: Integer;
  LView: TKView;
  LNode: TEFNode;
  //LToolButton: TKExtActionButton;
begin
  Assert(Assigned(AToolbar));

  if Assigned(AConfigNode) and (AConfigNode.ChildCount > 0) then
  begin
    TExtToolbarSeparator.CreateAndAddTo(AToolbar.Items);
    for I := 0 to AConfigNode.ChildCount - 1 do
    begin
      LNode := AConfigNode.Children[I];
      LView := Session.Config.Views.ViewByNode(LNode);
      FActionButtons.Add(LNode.Name, AddActionButton(LView, AToolbar));
      TExtToolbarSpacer.CreateAndAddTo(AToolbar.Items);
    end;
  end;
end;

function TKExtPanelControllerBase.GetConfirmCall(const AMessage: string; const AMethod: TExtProcedure): string;
begin
  Result := Format('confirmCall("%s", "%s", ajaxSimple, {methodURL: "%s"});',
    [_(Session.Config.AppTitle), AMessage, MethodURI(AMethod)]);
end;

procedure TKExtPanelControllerBase.AfterConstruction;
begin
  inherited;
  FActionButtons := TDictionary<string, TKExtActionButton>.Create;
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

  FTopToolbar := TExtToolbar.Create(Self);
  try
    AddTopToolbarButtons;
    AddTopToolbarToolViewButtons;
  except
    FreeAndNil(FTopToolbar);
    raise;
  end;
  if FTopToolbar.Items.Count = 0 then
    FreeAndNil(FTopToolbar)
  else
    Tbar := FTopToolbar;
  AfterCreateTopToolbar;
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

procedure TKExtPanelControllerBase.PerformDelayedClick(
  const AButton: TExtButton);
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

function TKExtPanelControllerBase.SupportsContainer: Boolean;
begin
  Result := True;
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

function TKExtControllerBase.SupportsContainer: Boolean;
begin
  Result := True;
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

function TKExtToolController.IsSynchronous: Boolean;
begin
  Result := True;
end;

function TKExtToolController.SupportsContainer: Boolean;
begin
  Result := False;
end;

procedure TKExtToolController.AfterExecuteTool;
begin
end;

procedure TKExtToolController.DoDisplay;
begin
  inherited;
  ExecuteTool;
  AfterExecuteTool;
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

procedure TKExtActionButton.InitController(const AController: IKExtController);
begin
end;

procedure TKExtActionButton.SetView(const AValue: TKView);
var
  LTooltip, LIcon: string;
begin
  Assert(Assigned(AValue));

  FView := AValue;
  Text := _(FView.DisplayLabel);

  LIcon := FView.GetString('ImageName');
  if LIcon = '' then
    LIcon := CallViewControllerStringMethod(FView, 'GetDefaultImageName', '');
  if LIcon = '' then
    LIcon := FView.ImageName;
  Icon := Session.Config.GetImageURL(LIcon);

  LTooltip := FView.GetExpandedString('Hint');
  if LTooltip <> '' then
    Tooltip := LTooltip;
end;

{ TKExtWindowToolController }

procedure TKExtWindowToolController.AfterExecuteTool;
begin
  NotifyObservers('Confirmed');
  Close;
end;

procedure TKExtWindowToolController.Cancel;
begin
  NotifyObservers('Canceled');
  Close;
end;

procedure TKExtWindowToolController.Confirm;
begin
  AfterExecuteTool;
end;

procedure TKExtWindowToolController.CreateButtons;
begin
  FConfirmButton := TExtButton.CreateAndAddTo(Buttons);
  FConfirmButton.Scale := Config.GetString('ButtonScale', 'medium');
  FConfirmButton.FormBind := True;
  FConfirmButton.Icon := Session.Config.GetImageURL('accept');
  FConfirmButton.Text := Config.GetString('ConfirmButton/Caption', _('Confirm'));
  FConfirmButton.Tooltip := Config.GetString('ConfirmButton/Tooltip', _('Confirm action and close window'));
  FConfirmButton.Handler := JSFunction(GetConfirmJSCode());

  FCancelButton := TExtButton.CreateAndAddTo(Buttons);
  FCancelButton.Scale := Config.GetString('ButtonScale', 'medium');
  FCancelButton.Icon := Session.Config.GetImageURL('cancel');
  FCancelButton.Text := _('Cancel');
  FCancelButton.Tooltip := _('Cancel changes');
  FCancelButton.Handler := Ajax(Cancel);
end;

procedure TKExtWindowToolController.DoDisplay;
begin
  inherited;
  Title := View.DisplayLabel;
  CreateButtons;
end;

function TKExtWindowToolController.GetConfirmJSCode: string;
begin
  Result := GetPOSTAjaxCode(Confirm, [], '');
end;

procedure TKExtWindowToolController.InitDefaults;
begin
  inherited;
  Modal := True;
  Closable := False;
  SetWindowSize;
  Layout := lyFit;
end;

procedure TKExtWindowToolController.SetWindowSize;
begin
  Width := 400;
  Height := 200;
  Resizable := False;
end;

end.

