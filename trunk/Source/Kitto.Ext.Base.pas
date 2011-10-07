unit Kitto.Ext.Base;

interface

uses
  ExtPascal, Ext, ExtForm,
  EF.Intf, EF.ObserverIntf, EF.Classes,
  Kitto.Controller, Kitto.Metadata.Views;

type
  ///	<summary>
  ///	  Base Ext window with subject, observer and controller capabilities.
  ///	</summary>
  TKExtWindowController = class(TExtWindow, IInterface, IEFInterface, IEFSubject, IEFObserver, IKController)
  private
    FSubjObserverImpl: TEFSubjectAndObserver;
    FView: TKView;
    FConfig: TEFConfig;
    function GetView: TKView;
    function GetConfig: TEFConfig;
  protected
    procedure SetView(const AValue: TKView);
    procedure DoDisplay; virtual;
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

    property Config: TEFConfig read GetConfig;
    property View: TKView read GetView write SetView;
    procedure Display;
  published
    procedure PanelClosed;
  end;

  {
    A modal window used to host panels.
  }
  TKExtModalWindow = class(TKExtWindowController)
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
  TKExtViewportController = class(TExtViewport, IInterface, IEFInterface, IEFSubject, IEFObserver, IKController)
  private
    FSubjObserverImpl: TEFSubjectAndObserver;
    FView: TKView;
    FConfig: TEFConfig;
    function GetView: TKView;
    function GetConfig: TEFConfig;
  protected
    procedure SetView(const AValue: TKView);
    procedure DoDisplay; virtual;
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

    property Config: TEFConfig read GetConfig;
    property View: TKView read GetView write SetView;
    procedure Display;
  end;

  ///	<summary>
  ///	  Base Ext panel with subject and observer capabilities.
  ///	</summary>
  TKExtPanel = class(TExtPanel, IInterface, IEFInterface, IEFSubject, IEFObserver)
  private
    FSubjObserverImpl: TEFSubjectAndObserver;
    FConfig: TEFConfig;
  protected
    function GetConfig: TEFConfig;
    function CloseHostWindow: Boolean;
    function GetHostWindow: TExtWindow;
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

    property Config: TEFConfig read GetConfig;
  end;

  TKExtPanelController = class(TKExtPanel, IKController)
  private
    FView: TKView;
  protected
    function GetView: TKView;
    procedure SetView(const AValue: TKView);
    procedure DoDisplay; virtual;
  public
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

implementation

uses
  SysUtils,
  EF.StrUtils, EF.Types,
  Kitto.Ext.Utils, Kitto.Ext.Session;

{ TKExtWindowController }

procedure TKExtWindowController.AfterConstruction;
begin
  inherited;
  FSubjObserverImpl := TEFSubjectAndObserver.Create;
end;

function TKExtWindowController.AsObject: TObject;
begin
  Result := Self;
end;

procedure TKExtWindowController.AttachObserver(const AObserver: IEFObserver);
begin
  FSubjObserverImpl.AttachObserver(AObserver);
end;

destructor TKExtWindowController.Destroy;
begin
  FreeAndNil(FSubjObserverImpl);
  FreeAndNil(FConfig);
  inherited;
end;

procedure TKExtWindowController.DetachObserver(const AObserver: IEFObserver);
begin
  FSubjObserverImpl.DetachObserver(AObserver);
end;

procedure TKExtWindowController.Display;
begin
  DoDisplay;
end;

procedure TKExtWindowController.DoDisplay;
begin
  Show;
end;

function TKExtWindowController.GetConfig: TEFConfig;
begin
  if not Assigned(FConfig) then
    FConfig := TEFConfig.Create;
  Result := FConfig;
end;

function TKExtWindowController.GetView: TKView;
begin
  Result := FView;
end;

procedure TKExtWindowController.NotifyObservers(const AContext: string);
begin
  FSubjObserverImpl.NotifyObservers(AContext);
end;

procedure TKExtWindowController.PanelClosed;
begin
  NotifyObservers('Closed');
end;

function TKExtWindowController.QueryInterface(const IID: TGUID; out Obj): HRESULT;
begin
  // Don't delegate to FSubjObserverImpl. We want to expose our own interfaces
  // and get the callbacks.
  if GetInterface(IID, Obj) then Result := 0 else Result := E_NOINTERFACE;
end;

procedure TKExtWindowController.SetView(const AValue: TKView);
begin
  FView := AValue;
end;

procedure TKExtWindowController.UpdateObserver(const ASubject: IEFSubject;
  const AContext: string);
begin
end;

function TKExtWindowController._AddRef: Integer;
begin
  Result := -1;
end;

function TKExtWindowController._Release: Integer;
begin
  Result := -1;
end;

{ TKExtPanel }

procedure TKExtPanel.AfterConstruction;
begin
  inherited;
  FSubjObserverImpl := TEFSubjectAndObserver.Create;
end;

function TKExtPanel.AsObject: TObject;
begin
  Result := Self;
end;

procedure TKExtPanel.AttachObserver(const AObserver: IEFObserver);
begin
  FSubjObserverImpl.AttachObserver(AObserver);
end;

destructor TKExtPanel.Destroy;
begin
  FreeAndNil(FSubjObserverImpl);
  FreeAndNil(FConfig);
  inherited;
end;

procedure TKExtPanel.DetachObserver(const AObserver: IEFObserver);
begin
  FSubjObserverImpl.DetachObserver(AObserver);
end;

function TKExtPanel.GetConfig: TEFConfig;
begin
  if not Assigned(FConfig) then
    FConfig := TEFConfig.Create;
  Result := FConfig;
end;

procedure TKExtPanel.NotifyObservers(const AContext: string);
begin
  FSubjObserverImpl.NotifyObservers(AContext);
end;

function TKExtPanel.QueryInterface(const IID: TGUID; out Obj): HRESULT;
begin
  // Don't delegate to FSubjObserverImpl. We want to expose our own interfaces
  // and get the callbacks.
  if GetInterface(IID, Obj) then Result := 0 else Result := E_NOINTERFACE;
end;

procedure TKExtPanel.UpdateObserver(const ASubject: IEFSubject;
  const AContext: string);
begin
end;

function TKExtPanel._AddRef: Integer;
begin
  Result := -1;
end;

function TKExtPanel._Release: Integer;
begin
  Result := -1;
end;

function TKExtPanel.GetHostWindow: TExtWindow;
begin
  Result := Config.GetObject('Sys/HostWindow') as TExtWindow;
end;

function TKExtPanel.CloseHostWindow: Boolean;
var
  LHostWindow: TExtWindow;
begin
  LHostWindow := GetHostWindow;
  Result := Assigned(LHostWindow);
  if Result then
    LHostWindow.Close;
end;

{ TKExtViewportController }

procedure TKExtViewportController.AfterConstruction;
begin
  inherited;
  FSubjObserverImpl := TEFSubjectAndObserver.Create;
end;

function TKExtViewportController.AsObject: TObject;
begin
  Result := Self;
end;

procedure TKExtViewportController.AttachObserver(const AObserver: IEFObserver);
begin
  FSubjObserverImpl.AttachObserver(AObserver);
end;

destructor TKExtViewportController.Destroy;
begin
  FreeAndNil(FSubjObserverImpl);
  FreeAndNil(FConfig);
  inherited;
end;

procedure TKExtViewportController.DetachObserver(const AObserver: IEFObserver);
begin
  FSubjObserverImpl.DetachObserver(AObserver);
end;

procedure TKExtViewportController.Display;
begin
  DoDisplay;
end;

procedure TKExtViewportController.DoDisplay;
begin
  Show;
end;

function TKExtViewportController.GetConfig: TEFConfig;
begin
  if not Assigned(FConfig) then
    FConfig := TEFConfig.Create;
  Result := FConfig;
end;

function TKExtViewportController.GetView: TKView;
begin
  Result := FView;
end;

procedure TKExtViewportController.NotifyObservers(const AContext: string);
begin
  FSubjObserverImpl.NotifyObservers(AContext);
end;

function TKExtViewportController.QueryInterface(const IID: TGUID; out Obj): HRESULT;
begin
  // Don't delegate to FSubjObserverImpl. We want to expose our own interfaces
  // and get the callbacks.
  if GetInterface(IID, Obj) then Result := 0 else Result := E_NOINTERFACE;
end;

procedure TKExtViewportController.SetView(const AValue: TKView);
begin
  FView := AValue;
end;

procedure TKExtViewportController.UpdateObserver(const ASubject: IEFSubject;
  const AContext: string);
begin
end;

function TKExtViewportController._AddRef: Integer;
begin
  Result := -1;
end;

function TKExtViewportController._Release: Integer;
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
  FSubjObserverImpl.NotifyObservers(AContext);
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

{ TKExtPanelController }

procedure TKExtPanelController.Display;
var
  LContainer: TObject;
begin
  LContainer := Config.GetObject('Sys/Container');

  if Assigned(LContainer) and (LContainer is TExtContainer) then
  begin
    // Don't call AddTo as a regular method: it will call AfterConstruction.
    TExtContainer(LContainer).Items.Add(Self);

    if Config.GetBoolean('Controller/AllowClose', True) then
    begin
      Closable := True;
      On('close', TExtContainer(LContainer).Ajax('PanelClosed', ['Panel', '%0.nm']));
    end
    else
      Closable := False;

    // Initialization should be done here, after AddTo, not earlier.
    DoDisplay;
  end
  else
  begin
    DoDisplay;
    Show;
  end;
end;

procedure TKExtPanelController.DoDisplay;
begin
  inherited;
  Assert(View <> nil);

  IconCls := Session.SetViewIconStyle(View);
end;

function TKExtPanelController.GetView: TKView;
begin
  Result := FView;
end;

procedure TKExtPanelController.SetView(const AValue: TKView);
begin
  FView := AValue;
end;

end.
