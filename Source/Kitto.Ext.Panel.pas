unit Kitto.Ext.Panel;

interface

uses
  EF.Tree
  , EF.ObserverIntf
  , Kitto.Metadata.Views
  , Kitto.JS.Base
  , Kitto.JS
  , Ext.Base
  , Kitto.Ext.Base
  ;

type
  /// <summary>
  ///  Base panel controller. Used for both floating and embedded panels.
  /// </summary>
  TKExtPanelControllerBase = class abstract(TKExtPanelBase, IJSController)
  strict private
    FView: TKView;
    FContainer: IJSContainer;
    FTopToolbar: TKExtToolbar;
    FDisplayMode: string;
    procedure CreateTopToolbar;
    procedure EnsureAllSupportFiles;
    function GetDisplayMode: string;
    procedure SetDisplayMode(const AValue: string);
  strict protected
    procedure InitDefaults; override;
    procedure PerformDelayedClick(const AButton: TExtButton);
    procedure ExecuteNamedAction(const AActionName: string); virtual;
    function GetDefaultSplit: Boolean; virtual;
    function GetView: TKView;
    procedure SetView(const AValue: TKView);
    procedure DoBeforeDisplay; virtual;
    procedure DoDisplay; virtual;
    procedure DoAfterDisplay; virtual;
    function GetContainer: IJSContainer;
    procedure SetContainer(const AValue: IJSContainer);
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

    procedure ItemAdded(const AItems: TJSObjectArray; const AItem: TJSObject); override;
    procedure InitSubController(const ASubController: IJSController); virtual;
  public
    destructor Destroy; override;
    function IsSynchronous: Boolean;
    property View: TKView read GetView write SetView;
    procedure Display;
    property DisplayMode: string read GetDisplayMode write SetDisplayMode;
  //published
    procedure Closed;
  end;

  TKExtPanelController = class(TKExtPanelControllerBase)
  end;

  /// <summary>
  ///  Base class for tool controllers that display a (modal) window with
  ///  a set of ok/cancel buttons.
  /// </summary>
  TKExtPanelToolController = class(TKExtPanelControllerBase)
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

implementation

uses
  SysUtils
  , StrUtils
  , EF.StrUtils
  , EF.Localization
  , EF.Macros
  , Kitto.AccessControl
  , Kitto.JS.Controller
  , Kitto.Web.Application
  , Kitto.Web.Request
  , Kitto.Web.Response
  , Kitto.Web.Session
  ;

{ TKExtPanelControllerBase }

function TKExtPanelControllerBase.GetDefaultHeight: Integer;
begin
  Result := TKWebApplication.Current.Config.Config.GetInteger('Defaults/Window/Height', DEFAULT_WINDOW_HEIGHT);
end;

function TKExtPanelControllerBase.GetDefaultWidth: Integer;
begin
  Result := TKWebApplication.Current.Config.Config.GetInteger('Defaults/Window/Width', DEFAULT_WINDOW_WIDTH);
end;

function TKExtPanelControllerBase.GetDisplayMode: string;
begin
  Result := FDisplayMode;
end;

procedure TKExtPanelControllerBase.InitDefaults;
begin
  inherited;
  &On('close', TKWebResponse.Current.Items.AjaxCallMethod(Self).SetMethod(Closed).AsFunction);
end;

destructor TKExtPanelControllerBase.Destroy;
begin
  if TKWebSession.Current <> nil then
    TKWebSession.Current.RemoveController(Self);
  inherited;
end;

procedure TKExtPanelControllerBase.Display;
begin
  DoBeforeDisplay;
  DoDisplay;
  DoAfterDisplay;
end;

procedure TKExtPanelControllerBase.EnsureAllSupportFiles;
var
  LClassType: TClass;
begin
  LClassType := ClassType;
  TKWebSession.Current.EnsureSupportFiles(TJSControllerRegistry.Instance.FindClassId(LClassType));
  while LClassType.ClassParent <> nil do
  begin
    LClassType := LClassType.ClassParent;
    TKWebSession.Current.EnsureSupportFiles(TJSControllerRegistry.Instance.FindClassId(LClassType));
  end;
  TKWebSession.Current.EnsureViewSupportFiles(View);
end;

procedure TKExtPanelControllerBase.DoAfterDisplay;
begin
  if (Container = nil) and (RenderTo = '') and (RenderToExpression = nil) then
  begin
    if DisplayMode = 'FullScreen'  then
    begin
      Draggable := False;
      PluginsString := 'viewport';
    end
    else
      ShowFloating(DisplayMode = 'Modal');
  end;
end;

procedure TKExtPanelControllerBase.DoBeforeDisplay;
begin
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

  if Container <> nil then
    Closable := Config.GetBoolean('AllowClose', GetDefaultAllowClose);

  DisplayMode := Config.GetString('DisplayMode');

  LRenderTo := Config.GetString('ContainerElementId');
  if LRenderTo <> '' then
  begin
    TEFMacroExpansionEngine.Instance.Expand(LRenderTo);
    RenderTo := LRenderTo;
  end;

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

  Draggable := Config.GetBoolean('Movable', False);

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
  else if Modal then
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
  else if Modal then
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
  ReplaceAllCaseSensitive(LConfirmationMessage, sLineBreak, '<br>');
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
    if AToolbar.Items.Count > 0 then
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

procedure TKExtPanelControllerBase.ItemAdded(const AItems: TJSObjectArray; const AItem: TJSObject);
var
  LController: IJSController;
begin
  inherited;
  if Supports(AItem, IJSController, LController) then
    InitSubController(LController);
end;

procedure TKExtPanelControllerBase.PerformDelayedClick(const AButton: TExtButton);
begin
  if Assigned(AButton) then
    AButton.On('render', GenerateAnonymousFunction(AButton.PerformClick));
end;

procedure TKExtPanelControllerBase.SetContainer(const AValue: IJSContainer);
begin
  FContainer := AValue;
end;

procedure TKExtPanelControllerBase.SetDisplayMode(const AValue: string);
begin
  FDisplayMode := AValue;
end;

procedure TKExtPanelControllerBase.SetView(const AValue: TKView);
begin
  FView := AValue;
end;

{ TKExtPanelToolController }

procedure TKExtPanelToolController.AfterExecuteTool;
begin
  NotifyObservers('ToolConfirmed');
  Close;
end;

procedure TKExtPanelToolController.Cancel;
begin
  NotifyObservers('ToolCanceled');
  Close;
end;

procedure TKExtPanelToolController.Confirm;
begin
  AfterExecuteTool;
end;

procedure TKExtPanelToolController.CreateButtons;
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

procedure TKExtPanelToolController.DoDisplay;
begin
  inherited;
  Title := _(View.DisplayLabel);
  SetWindowSize;
  if not Config.GetBoolean('HideButtons') then
    CreateButtons;
end;

function TKExtPanelToolController.GetConfirmJSFunction: TJSFunction;
begin
  //Result := GetPOSTAjaxCode(Confirm, [], GetConfirmJsonData);
  Result := TKWebResponse.Current.Items.AjaxCallMethod(Self).SetMethod(Confirm)
    .Post(GetConfirmJsonData).AsFunction;
end;

function TKExtPanelToolController.GetConfirmJsonData: string;
begin
  Result := '{}';
end;

procedure TKExtPanelToolController.InitDefaults;
begin
  inherited;
  Modal := True;
  Closable := False;
  Layout := lyFit;
end;

procedure TKExtPanelToolController.InitSubController(const AController: IJSController);
var
  LSubject: IEFSubject;
begin
  inherited;
  FSubController := AController;
  if Supports(FSubController.AsObject, IEFSubject, LSubject) then
    LSubject.AttachObserver(Self);
end;

procedure TKExtPanelToolController.SetWindowSize;
begin
  Width := Config.GetInteger('Width', DEFAULT_WINDOW_TOOL_WIDTH);
  Height := Config.GetInteger('Height', DEFAULT_WINDOW_TOOL_HEIGHT);
  Resizable := False;
end;

procedure TKExtPanelToolController.UpdateObserver(const ASubject: IEFSubject;
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

initialization
  TJSControllerRegistry.Instance.RegisterClass('Panel', TKExtPanelController);

finalization
  TJSControllerRegistry.Instance.UnregisterClass('Panel');

end.
