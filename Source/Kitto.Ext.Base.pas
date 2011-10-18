unit Kitto.Ext.Base;

interface

uses
  ExtPascal, Ext, ExtForm, ExtUx,
  EF.Intf, EF.Tree, EF.ObserverIntf, EF.Classes,
  Kitto.Ext.Controller, Kitto.Metadata.Views;

type
  ///	<summary>
  ///	  Base Ext window with subject, observer and controller capabilities.
  ///	</summary>
  TKExtWindowControllerBase = class(TExtWindow, IInterface, IEFInterface, IEFSubject, IEFObserver, IKExtController)
  private
    FSubjObserverImpl: TEFSubjectAndObserver;
    FView: TKView;
    FConfig: TEFNode;
    FContainer: TExtContainer;
    FOwnsView: Boolean;
    function GetView: TKView;
    function GetConfig: TEFNode;
  protected
    procedure SetView(const AValue: TKView);
    procedure DoDisplay; virtual;
    function GetContainer: TExtContainer;
    procedure SetContainer(const AValue: TExtContainer);
    function GetOwnsView: Boolean;
    procedure SetOwnsView(const AValue: Boolean);
    procedure InitDefaults; override;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;

    function QueryInterface(const IID: TGUID; out Obj): HRESULT; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function AsObject: TObject;
    procedure AttachObserver(const AObserver: IEFObserver); virtual;
    procedure DetachObserver(const AObserver: IEFObserver); virtual;
    procedure NotifyObservers(const AContext: string = ''); virtual;
    procedure UpdateObserver(const ASubject: IEFSubject; const AContext: string = ''); virtual;

    property View: TKView read GetView write SetView;
    procedure Display;
  published
    procedure PanelClosed;
  end;

  {
    A modal window used to host panels.
  }
  TKExtModalWindow = class(TKExtWindowControllerBase)
  protected
    procedure InitDefaults; override;
  public
    {
      Call this after adding the panel so that the window can hook its
      beforeclose event and close itself.
    }
    procedure HookPanel(const APanel: TExtPanel);
  published
    procedure PanelClosed;
  end;

  ///	<summary>
  ///	  Base ext viewport with subject, observer and controller capabilities.
  ///	</summary>
  TKExtViewportControllerBase = class(TExtViewport, IInterface, IEFInterface, IEFSubject, IEFObserver, IKExtController)
  private
    FSubjObserverImpl: TEFSubjectAndObserver;
    FView: TKView;
    FConfig: TEFNode;
    FContainer: TExtContainer;
    FOwnsView: Boolean;
    function GetView: TKView;
    function GetConfig: TEFNode;
  protected
    procedure SetView(const AValue: TKView);
    procedure DoDisplay; virtual;
    function GetContainer: TExtContainer;
    procedure SetContainer(const AValue: TExtContainer);
    function GetOwnsView: Boolean;
    procedure SetOwnsView(const AValue: Boolean);
    procedure InitDefaults; override;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;

    function QueryInterface(const IID: TGUID; out Obj): HRESULT; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function AsObject: TObject;
    procedure AttachObserver(const AObserver: IEFObserver); virtual;
    procedure DetachObserver(const AObserver: IEFObserver); virtual;
    procedure NotifyObservers(const AContext: string = ''); virtual;
    procedure UpdateObserver(const ASubject: IEFSubject; const AContext: string = ''); virtual;

    property View: TKView read GetView write SetView;
    procedure Display;
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
    function CloseHostWindow: Boolean;
    function GetHostWindow: TExtWindow;
    procedure InitDefaults; override;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;

    function QueryInterface(const IID: TGUID; out Obj): HRESULT; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function AsObject: TObject;
    procedure AttachObserver(const AObserver: IEFObserver); virtual;
    procedure DetachObserver(const AObserver: IEFObserver); virtual;
    procedure NotifyObservers(const AContext: string = ''); virtual;
    procedure UpdateObserver(const ASubject: IEFSubject; const AContext: string = ''); virtual;

    property Config: TEFNode read GetConfig;
  end;

  TKExtPanelControllerBase = class(TKExtPanelBase, IKExtController)
  private
    FView: TKView;
    FContainer: TExtContainer;
    FOwnsView: Boolean;
  protected
    function GetView: TKView;
    procedure SetView(const AValue: TKView);
    procedure DoDisplay; virtual;
    function GetContainer: TExtContainer;
    procedure SetContainer(const AValue: TExtContainer);
    property Container: TExtContainer read GetContainer write SetContainer;
    function GetOwnsView: Boolean;
    procedure SetOwnsView(const AValue: Boolean);
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
    property View: TKView read GetView write SetView;
    procedure Display;
  end;

  TKExtFormComboBox = class(TExtFormComboBox, IInterface, IEFInterface, IEFSubject)
  private
    FSubjObserverImpl: TEFSubjectAndObserver;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
    function AsObject: TObject; inline;
    function QueryInterface(const IID: TGUID; out Obj): HRESULT; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure AttachObserver(const AObserver: IEFObserver); virtual;
    procedure DetachObserver(const AObserver: IEFObserver); virtual;
    procedure NotifyObservers(const AContext: string = ''); virtual;
  end;

  TKExtFormTextField = class(TExtFormTextField, IInterface, IEFInterface, IEFSubject)
  private
    FSubjObserverImpl: TEFSubjectAndObserver;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
    function AsObject: TObject; inline;
    function QueryInterface(const IID: TGUID; out Obj): HRESULT; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure AttachObserver(const AObserver: IEFObserver); virtual;
    procedure DetachObserver(const AObserver: IEFObserver); virtual;
    procedure NotifyObservers(const AContext: string = ''); virtual;
  end;

  TKExtStatusBar = class(TExtUxStatusBar)
  protected
    procedure InitDefaults; override;
  public
    procedure SetErrorStatus(const AText: string);

    procedure ClearStatus; virtual;
  end;

implementation

uses
  SysUtils,
  EF.StrUtils, EF.Types,
  Kitto.Ext.Utils, Kitto.Ext.Session;

{ TKExtWindowControllerBase }

procedure TKExtWindowControllerBase.AfterConstruction;
begin
  inherited;
  FOwnsView := True;
  FSubjObserverImpl := TEFSubjectAndObserver.Create;
end;

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
  if FOwnsView and Assigned(FView) and not FView.IsPersistent then
    FreeAndNil(FView);
  FreeAndNil(FSubjObserverImpl);
  FreeAndNil(FConfig);
  inherited;
end;

procedure TKExtWindowControllerBase.DetachObserver(const AObserver: IEFObserver);
begin
  FSubjObserverImpl.DetachObserver(AObserver);
end;

procedure TKExtWindowControllerBase.Display;
begin
  DoDisplay;
end;

procedure TKExtWindowControllerBase.DoDisplay;
begin
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

function TKExtWindowControllerBase.GetOwnsView: Boolean;
begin
  Result := FOwnsView;
end;

function TKExtWindowControllerBase.GetView: TKView;
begin
  Result := FView;
end;

procedure TKExtWindowControllerBase.InitDefaults;
begin
  inherited;
  Layout := lyBorder;
end;

procedure TKExtWindowControllerBase.NotifyObservers(const AContext: string);
begin
  FSubjObserverImpl.NotifyObserversOnBehalfOf(Self, AContext);
end;

procedure TKExtWindowControllerBase.PanelClosed;
begin
  NotifyObservers('Closed');
end;

function TKExtWindowControllerBase.QueryInterface(const IID: TGUID; out Obj): HRESULT;
begin
  // Don't delegate to FSubjObserverImpl. We want to expose our own interfaces
  // and get the callbacks.
  if GetInterface(IID, Obj) then Result := 0 else Result := E_NOINTERFACE;
end;

procedure TKExtWindowControllerBase.SetContainer(const AValue: TExtContainer);
begin
  FContainer := AValue;
end;

procedure TKExtWindowControllerBase.SetOwnsView(const AValue: Boolean);
begin
  FOwnsView := AValue;
end;

procedure TKExtWindowControllerBase.SetView(const AValue: TKView);
begin
  FView := AValue;
end;

procedure TKExtWindowControllerBase.UpdateObserver(const ASubject: IEFSubject;
  const AContext: string);
begin
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

procedure TKExtPanelBase.AfterConstruction;
begin
  inherited;
  FSubjObserverImpl := TEFSubjectAndObserver.Create;
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

function TKExtPanelBase.QueryInterface(const IID: TGUID; out Obj): HRESULT;
begin
  // Don't delegate to FSubjObserverImpl. We want to expose our own interfaces
  // and get the callbacks.
  if GetInterface(IID, Obj) then Result := 0 else Result := E_NOINTERFACE;
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
  Region := rgCenter;
end;

function TKExtPanelBase.CloseHostWindow: Boolean;
var
  LHostWindow: TExtWindow;
begin
  LHostWindow := GetHostWindow;
  Result := Assigned(LHostWindow);
  if Result then
    LHostWindow.Close;
end;

{ TKExtViewportControllerBase }

procedure TKExtViewportControllerBase.AfterConstruction;
begin
  inherited;
  FOwnsView := True;
  FSubjObserverImpl := TEFSubjectAndObserver.Create;
end;

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
  if FOwnsView and Assigned(FView) and not FView.IsPersistent then
    FreeAndNil(FView);
  FreeAndNil(FSubjObserverImpl);
  FreeAndNil(FConfig);
  inherited;
end;

procedure TKExtViewportControllerBase.DetachObserver(const AObserver: IEFObserver);
begin
  FSubjObserverImpl.DetachObserver(AObserver);
end;

procedure TKExtViewportControllerBase.Display;
begin
  DoDisplay;
end;

procedure TKExtViewportControllerBase.DoDisplay;
begin
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

function TKExtViewportControllerBase.GetOwnsView: Boolean;
begin
  Result := FOwnsView;
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

procedure TKExtViewportControllerBase.NotifyObservers(const AContext: string);
begin
  FSubjObserverImpl.NotifyObserversOnBehalfOf(Self, AContext);
end;

function TKExtViewportControllerBase.QueryInterface(const IID: TGUID; out Obj): HRESULT;
begin
  // Don't delegate to FSubjObserverImpl. We want to expose our own interfaces
  // and get the callbacks.
  if GetInterface(IID, Obj) then Result := 0 else Result := E_NOINTERFACE;
end;

procedure TKExtViewportControllerBase.SetContainer(const AValue: TExtContainer);
begin
  FContainer := AValue;
end;

procedure TKExtViewportControllerBase.SetOwnsView(const AValue: Boolean);
begin
  FOwnsView := AValue;
end;

procedure TKExtViewportControllerBase.SetView(const AValue: TKView);
begin
  FView := AValue;
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

  APanel.On('close', Ajax(PanelClosed, ['Panel', '%0.nm']));
end;

procedure TKExtModalWindow.InitDefaults;
begin
  inherited;
  Width := 600;
  Height := 400;
  Layout := lyFit;
  Closable := False;
  Modal := True;
end;

procedure TKExtModalWindow.PanelClosed;
begin
  Close;
end;

{ TKExtFormComboBox }

procedure TKExtFormComboBox.AfterConstruction;
begin
  inherited;
  FSubjObserverImpl := TEFSubjectAndObserver.Create;
end;

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

procedure TKExtFormComboBox.NotifyObservers(const AContext: string);
begin
  FSubjObserverImpl.NotifyObserversOnBehalfOf(Self, AContext);
end;

function TKExtFormComboBox.QueryInterface(const IID: TGUID; out Obj): HRESULT;
begin
  if GetInterface(IID, Obj) then Result := 0 else Result := E_NOINTERFACE;
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

procedure TKExtPanelControllerBase.AfterConstruction;
begin
  inherited;
  FOwnsView := True;
end;

destructor TKExtPanelControllerBase.Destroy;
begin
  if FOwnsView and Assigned(FView) and not FView.IsPersistent then
    FreeAndNil(FView);
  inherited;
end;

procedure TKExtPanelControllerBase.Display;
begin
  if Container <> nil then
  begin
    if View.GetBoolean('Controller/AllowClose', True) then
    begin
      Closable := True;
      On('close', Container.Ajax('PanelClosed', ['Panel', '%0.nm']));
    end
    else
      Closable := False;
  end;
  DoDisplay;
end;

procedure TKExtPanelControllerBase.DoDisplay;
begin
end;

function TKExtPanelControllerBase.GetContainer: TExtContainer;
begin
  Result := FContainer;
end;

function TKExtPanelControllerBase.GetOwnsView: Boolean;
begin
  Result := FOwnsView;
end;

function TKExtPanelControllerBase.GetView: TKView;
begin
  Result := FView;
end;

procedure TKExtPanelControllerBase.SetContainer(const AValue: TExtContainer);
begin
  FContainer := AValue;
end;

procedure TKExtPanelControllerBase.SetOwnsView(const AValue: Boolean);
begin
  FOwnsView := AValue;
end;

procedure TKExtPanelControllerBase.SetView(const AValue: TKView);
begin
  FView := AValue;
end;

{ TKExtFormTextField }

procedure TKExtFormTextField.AfterConstruction;
begin
  inherited;
  FSubjObserverImpl := TEFSubjectAndObserver.Create;
end;

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

procedure TKExtFormTextField.NotifyObservers(const AContext: string);
begin
  FSubjObserverImpl.NotifyObserversOnBehalfOf(Self, AContext);
end;

function TKExtFormTextField.QueryInterface(const IID: TGUID; out Obj): HRESULT;
begin
  if GetInterface(IID, Obj) then Result := 0 else Result := E_NOINTERFACE;
end;

function TKExtFormTextField._AddRef: Integer;
begin
  Result := -1;
end;

function TKExtFormTextField._Release: Integer;
begin
  Result := -1;
end;

{ TKExtStatusBar }

procedure TKExtStatusBar.ClearStatus;
begin
  inherited ClearStatus;
end;

procedure TKExtStatusBar.InitDefaults;
begin
  inherited;
  Session.StatusHost := Self;
end;

procedure TKExtStatusBar.SetErrorStatus(const AText: string);
begin
  SetStatus(JSObject(Format('text: "%s", iconCls:"x-status-error",clear:true', [AText])));
end;

end.
