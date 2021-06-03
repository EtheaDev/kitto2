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
  , Kitto.Metadata.Views
  ;

type
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
  ///  Base Ext panel with IJSActivable support.
  /// </summary>
  TKExtPanelBase = class(TExtPanel, IJSActivable)
  strict protected
    procedure InitDefaults; override;
    procedure LoadHtml(const AFileName: string; const APostProcessor: TFunc<string, string> = nil);
  public
    // IJSActivable
    procedure Activate; virtual;
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
  ///  Base class for controllers that don't have a specific visual
  ///  representation, yet can be used to render views, such as custom action
  ///  controllers.
  /// </summary>
  TKExtControllerBase = class(TExtObject, IJSController)
  private
    FView: TKView;
    FContainer: IJSContainer;
    function GetDisplayMode: string;
    procedure SetDisplayMode(const Value: string);
  protected
    function GetView: TKView;
    procedure SetView(const AValue: TKView);
    procedure DoBeforeDisplay; virtual;
    procedure DoDisplay; virtual;
    procedure DoAfterDisplay; virtual;
    function GetContainer: IJSContainer;
    procedure SetContainer(const AValue: IJSContainer);
    property Container: IJSContainer read GetContainer write SetContainer;
  public
    destructor Destroy; override;
    function IsSynchronous: Boolean; virtual;
    property View: TKView read GetView write SetView;
    procedure Display;
    property DisplayMode: string read GetDisplayMode write SetDisplayMode;
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
    function IsSynchronous: Boolean; override;
    property DisplayLabel: string read GetDisplayLabel;
  end;

  TExtToolControllerClass = class of TKExtToolController;

  TKExtFormComboBox = class(TExtFormComboBox)
  protected
    function GetEncodedValue: TExtExpression;
  end;

  TKExtFormTextField = class(TExtFormTextField);

  TKExtFormDateField = class(TExtFormDateField);

  TKExtFormCheckBoxField = class(TExtFormCheckBox);

  TKExtFormNumberField = class(TExtFormNumberField);

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
  , Kitto.JS.Controller
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

{ TKExtPanelBase }

procedure TKExtPanelBase.Activate;
begin
end;

procedure TKExtPanelBase.InitDefaults;
begin
  inherited;
  //Region := 'center';
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
  begin
    LHtml := TextFileToString(LFullFileName, TEncoding.UTF8);
    TEFMacroExpansionEngine.Instance.Expand(LHtml);
  end
  else
    LHtml := '';
  if Assigned(APostProcessor) then
    LHtml := APostProcessor(LHtml);
  Html := LHtml;
end;

{ TKExtFormComboBox }

function TKExtFormComboBox.GetEncodedValue: TExtExpression;
begin
  Result := TKWebResponse.Current.Items.ExecuteJSCode(Self, Format('encodeURI(%s.getValue())', [JSName])).AsExpression;
end;

{ TKExtControllerBase }

procedure TKExtControllerBase.Apply(const AProc: TProc<IJSController>);
begin
  Assert(Assigned(AProc));

  AProc(Self);
end;

destructor TKExtControllerBase.Destroy;
begin
  if TKWebSession.Current <> nil then
    TKWebSession.Current.RemoveController(Self);
  inherited;
end;

procedure TKExtControllerBase.Display;
begin
  DoBeforeDisplay;
  DoDisplay;
  DoAfterDisplay;
end;

procedure TKExtControllerBase.DoAfterDisplay;
begin
end;

procedure TKExtControllerBase.DoBeforeDisplay;
begin
end;

procedure TKExtControllerBase.DoDisplay;
begin
  TKWebSession.Current.EnsureSupportFiles(TJSControllerRegistry.Instance.FindClassId(Self.ClassType));
  TKWebSession.Current.EnsureViewSupportFiles(View);
end;

function TKExtControllerBase.GetContainer: IJSContainer;
begin
  Result := FContainer;
end;

function TKExtControllerBase.GetDisplayMode: string;
begin
  Result := '';
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

procedure TKExtControllerBase.SetDisplayMode(const Value: string);
begin
  // Not supported.
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
    LController := TJSControllerFactory.Instance.CreateController(TKWebSession.Current.ObjectSpace, FView, nil, nil, FActionObserver);
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
    [_('Confirm operation'), AMessage, GetMethodURL(ExecuteButtonAction)]);
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
  //&On('click', GenerateAnonymousFunction('b, e', 'setAnimationOrigin(b.id)'));
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

{ TKExtStatusBar }

function TKExtStatusBar.ClearStatus: TJSExpression;
begin
  Result := inherited ClearStatus;
end;

procedure TKExtStatusBar.SetErrorStatus(const AText: string);
begin
  SetStatus(JSObject(Format('text: "%s", iconCls:"x-status-error",clear:true', [AText])));
end;

end.

