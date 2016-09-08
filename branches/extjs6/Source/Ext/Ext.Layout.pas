unit Ext.Layout;

interface

uses
  StrUtils, Kitto.Ext, Ext.Base;

type
  TExtLayoutAutoLayout = class;
  TExtLayoutBorderLayoutRegion = class;
  TExtLayoutContainerLayout = class;
  TExtLayoutMenuLayout = class;
  TExtLayoutTableLayout = class;
  TExtLayoutFitLayout = class;
  TExtLayoutColumnLayout = class;
  TExtLayoutBoxLayout = class;
  TExtLayoutAnchorLayout = class;
  TExtLayoutToolbarLayout = class;
  TExtLayoutBorderLayoutSplitRegion = class;
  TExtLayoutBorderLayout = class;
  TExtLayoutVBoxLayout = class;
  TExtLayoutHBoxLayout = class;
  TExtLayoutAccordionLayout = class;
  TExtLayoutAbsoluteLayout = class;
  TExtLayoutFormLayout = class;
  TExtLayoutCardLayout = class;

  TExtLayoutAutoLayout = class(TExtFunction)
  public
    class function JSClassName : string; override;
  end;

  TExtLayoutBorderLayoutRegion = class(TExtFunction)
  private
    FAnimFloat : Boolean; // true
    FAutoHide : Boolean; // true
    FCmargins : TExtObject;
    FCollapseMode : String;
    FCollapsible : Boolean;
    FFloatable : Boolean; // true
    FMargins : TExtObject;
    FMinHeight : Integer; // 50
    FMinWidth : Integer; // 50
    FSplit : Boolean;
    FIsCollapsed : Boolean;
    FLayout : TExtObject;
    FPanel : TExtPanel;
    FPosition : String;
    procedure SetFAnimFloat(Value : Boolean);
    procedure SetFAutoHide(Value : Boolean);
    procedure SetFCmargins(Value : TExtObject);
    procedure SetFCollapseMode(Value : String);
    procedure SetFCollapsible(Value : Boolean);
    procedure SetFFloatable(Value : Boolean);
    procedure SetFMargins(Value : TExtObject);
    procedure SetFMinHeight(Value : Integer);
    procedure SetFMinWidth(Value : Integer);
    procedure SetFSplit(Value : Boolean);
    procedure SetFIsCollapsed(Value : Boolean);
    procedure SetFLayout(Value : TExtObject);
    procedure SetFPanel(Value : TExtPanel);
    procedure SetFPosition(Value : String);
  protected
    procedure InitDefaults; override;
  public
    class function JSClassName : string; override;
    function MustHaveACenterRegion : TExtFunction;
    function GetMinHeight : TExtFunction;
    function GetMinWidth : TExtFunction;
    function GetSize : TExtFunction;
    function IsVisible : TExtFunction;
    function SetPanel(Panel : TExtPanel) : TExtFunction;
    function SlideIn : TExtFunction;
    function SlideOut : TExtFunction;
    property AnimFloat : Boolean read FAnimFloat write SetFAnimFloat;
    property AutoHide : Boolean read FAutoHide write SetFAutoHide;
    property Cmargins : TExtObject read FCmargins write SetFCmargins;
    property CollapseMode : String read FCollapseMode write SetFCollapseMode;
    property Collapsible : Boolean read FCollapsible write SetFCollapsible;
    property Floatable : Boolean read FFloatable write SetFFloatable;
    property Margins : TExtObject read FMargins write SetFMargins;
    property MinHeight : Integer read FMinHeight write SetFMinHeight;
    property MinWidth : Integer read FMinWidth write SetFMinWidth;
    property Split : Boolean read FSplit write SetFSplit;
    property IsCollapsed : Boolean read FIsCollapsed write SetFIsCollapsed;
    property Layout : TExtObject read FLayout write SetFLayout;
    property Panel : TExtPanel read FPanel write SetFPanel;
    property Position : String read FPosition write SetFPosition;
  end;

  TExtLayoutContainerLayout = class(TExtFunction)
  private
    FExtraCls : String;
    FRenderHidden : Boolean;
    FActiveItem : TExtComponent;
    FFieldTpl : TExtTemplate;
    FIfJS : TExtObject;
    procedure SetFExtraCls(Value : String);
    procedure SetFRenderHidden(Value : Boolean);
    procedure SetFActiveItem(Value : TExtComponent);
    procedure SetFFieldTpl(Value : TExtTemplate);
    procedure SetFIfJS(Value : TExtObject);
  protected
    procedure InitDefaults; override;
  public
    class function JSClassName : string; override;
    function ParseMargins(V : Integer) : TExtFunction; overload;
    function ParseMargins(V : String) : TExtFunction; overload;
    property ExtraCls : String read FExtraCls write SetFExtraCls;
    property RenderHidden : Boolean read FRenderHidden write SetFRenderHidden;
    property ActiveItem : TExtComponent read FActiveItem write SetFActiveItem;
    property FieldTpl : TExtTemplate read FFieldTpl write SetFFieldTpl;
    property IfJS : TExtObject read FIfJS write SetFIfJS;
  end;

  TExtLayoutMenuLayout = class(TExtLayoutContainerLayout)
  public
    class function JSClassName : string; override;
  end;

  TExtLayoutTableLayout = class(TExtLayoutContainerLayout)
  private
    FColumns : Integer;
    FTableAttrs : TExtObject;
    procedure SetFColumns(Value : Integer);
    procedure SetFTableAttrs(Value : TExtObject);
  protected
    procedure InitDefaults; override;
  public
    class function JSClassName : string; override;
    property Columns : Integer read FColumns write SetFColumns;
    property TableAttrs : TExtObject read FTableAttrs write SetFTableAttrs;
  end;

  TExtLayoutFitLayout = class(TExtLayoutContainerLayout)
  public
    class function JSClassName : string; override;
  end;

  TExtLayoutColumnLayout = class(TExtLayoutContainerLayout)
  public
    class function JSClassName : string; override;
  end;

  TExtLayoutBoxLayout = class(TExtLayoutContainerLayout)
  private
    FDefaultMargins : TExtObject;
    FPadding : String;
    procedure SetFDefaultMargins(Value : TExtObject);
    procedure SetFPadding(Value : String);
  protected
    procedure InitDefaults; override;
  public
    class function JSClassName : string; override;
    function UpdateChildBoxes(Boxes : TExtObjectList) : TExtFunction;
    property DefaultMargins : TExtObject read FDefaultMargins write SetFDefaultMargins;
    property Padding : String read FPadding write SetFPadding;
  end;

  TExtLayoutAnchorLayout = class(TExtLayoutContainerLayout)
  private
    FAnchor : String;
    FDefaultAnchor : String;
    procedure SetFAnchor(Value : String);
    procedure SetFDefaultAnchor(Value : String);
  public
    class function JSClassName : string; override;
    property Anchor : String read FAnchor write SetFAnchor;
    property DefaultAnchor : String read FDefaultAnchor write SetFDefaultAnchor;
  end;

  TExtLayoutToolbarLayout = class(TExtLayoutContainerLayout)
  private
    FHiddenItems : TExtObjectList;
    FNoItemsMenuText : String;
    FTriggerWidth : Integer;
    procedure SetFHiddenItems(Value : TExtObjectList);
    procedure SetFNoItemsMenuText(Value : String);
    procedure SetFTriggerWidth(Value : Integer);
  protected
    procedure InitDefaults; override;
  public
    class function JSClassName : string; override;
    property HiddenItems : TExtObjectList read FHiddenItems write SetFHiddenItems;
    property NoItemsMenuText : String read FNoItemsMenuText write SetFNoItemsMenuText;
    property TriggerWidth : Integer read FTriggerWidth write SetFTriggerWidth;
  end;

  TExtLayoutBorderLayoutSplitRegion = class(TExtLayoutBorderLayoutRegion)
  private
    FCollapsibleSplitTip : String; // 'Drag to resize. Double click to hide.'
    FSplitTip : String; // 'Drag to resize.'
    FTickSize : Integer;
    FUseSplitTips : Boolean;
    procedure SetFCollapsibleSplitTip(Value : String);
    procedure SetFSplitTip(Value : String);
    procedure SetFTickSize(Value : Integer);
    procedure SetFUseSplitTips(Value : Boolean);
  protected
    procedure InitDefaults; override;
  public
    class function JSClassName : string; override;
    function GetSplitBar : TExtFunction;
    property CollapsibleSplitTip : String read FCollapsibleSplitTip write SetFCollapsibleSplitTip;
    property SplitTip : String read FSplitTip write SetFSplitTip;
    property TickSize : Integer read FTickSize write SetFTickSize;
    property UseSplitTips : Boolean read FUseSplitTips write SetFUseSplitTips;
  end;

  TExtLayoutBorderLayout = class(TExtLayoutContainerLayout)
  public
    class function JSClassName : string; override;
  end;

  TExtLayoutVBoxLayout = class(TExtLayoutBoxLayout)
  private
    FAlign : String;
    FFlex : Integer;
    FPack : String;
    procedure SetFAlign(Value : String);
    procedure SetFFlex(Value : Integer);
    procedure SetFPack(Value : String);
  public
    class function JSClassName : string; override;
    property Align : String read FAlign write SetFAlign;
    property Flex : Integer read FFlex write SetFFlex;
    property Pack : String read FPack write SetFPack;
  end;

  TExtLayoutHBoxLayout = class(TExtLayoutBoxLayout)
  private
    FAlign : String;
    FFlex : Integer;
    FPack : String;
    procedure SetFAlign(Value : String);
    procedure SetFFlex(Value : Integer);
    procedure SetFPack(Value : String);
  public
    class function JSClassName : string; override;
    property Align : String read FAlign write SetFAlign;
    property Flex : Integer read FFlex write SetFFlex;
    property Pack : String read FPack write SetFPack;
  end;

  TExtLayoutAccordionLayout = class(TExtLayoutFitLayout)
  private
    FActiveOnTop : Boolean;
    FAnimate : Boolean;
    FAutoWidth : Boolean; // true
    FCollapseFirst : Boolean;
    FFill : Boolean; // true
    FHideCollapseTool : Boolean;
    FSequence : Boolean;
    FTitleCollapse : Boolean; // true
    procedure SetFActiveOnTop(Value : Boolean);
    procedure SetFAnimate(Value : Boolean);
    procedure SetFAutoWidth(Value : Boolean);
    procedure SetFCollapseFirst(Value : Boolean);
    procedure SetFFill(Value : Boolean);
    procedure SetFHideCollapseTool(Value : Boolean);
    procedure SetFSequence(Value : Boolean);
    procedure SetFTitleCollapse(Value : Boolean);
  protected
    procedure InitDefaults; override;
  public
    class function JSClassName : string; override;
    function SetActiveItem(Item : String) : TExtFunction; overload;
    function SetActiveItem(Item : Integer) : TExtFunction; overload;
    property ActiveOnTop : Boolean read FActiveOnTop write SetFActiveOnTop;
    property Animate : Boolean read FAnimate write SetFAnimate;
    property AutoWidth : Boolean read FAutoWidth write SetFAutoWidth;
    property CollapseFirst : Boolean read FCollapseFirst write SetFCollapseFirst;
    property Fill : Boolean read FFill write SetFFill;
    property HideCollapseTool : Boolean read FHideCollapseTool write SetFHideCollapseTool;
    property Sequence : Boolean read FSequence write SetFSequence;
    property TitleCollapse : Boolean read FTitleCollapse write SetFTitleCollapse;
  end;

  TExtLayoutAbsoluteLayout = class(TExtLayoutAnchorLayout)
  public
    class function JSClassName : string; override;
  end;

  TExtLayoutFormLayout = class(TExtLayoutAnchorLayout)
  private
    FFieldTpl : TExtTemplate;
    FLabelSeparator : String;
    FTrackLabels : Boolean;
    FLabelStyle : String;
    procedure SetFFieldTpl(Value : TExtTemplate);
    procedure SetFLabelSeparator(Value : String);
    procedure SetFTrackLabels(Value : Boolean);
    procedure SetFLabelStyle(Value : String);
  protected
    procedure InitDefaults; override;
  public
    class function JSClassName : string; override;
    function GetTemplateArgs(Field : TExtFormField) : TExtFunction;
    property FieldTpl : TExtTemplate read FFieldTpl write SetFFieldTpl;
    property LabelSeparator : String read FLabelSeparator write SetFLabelSeparator;
    property TrackLabels : Boolean read FTrackLabels write SetFTrackLabels;
    property LabelStyle : String read FLabelStyle write SetFLabelStyle;
  end;

  TExtLayoutCardLayout = class(TExtLayoutFitLayout)
  private
    FDeferredRender : Boolean;
    FLayoutOnCardChange : Boolean;
    procedure SetFDeferredRender(Value : Boolean);
    procedure SetFLayoutOnCardChange(Value : Boolean);
  public
    class function JSClassName : string; override;
    function SetActiveItem(Item : String) : TExtFunction; overload;
    function SetActiveItem(Item : Integer) : TExtFunction; overload;
    property DeferredRender : Boolean read FDeferredRender write SetFDeferredRender;
    property LayoutOnCardChange : Boolean read FLayoutOnCardChange write SetFLayoutOnCardChange;
  end;

implementation

class function TExtLayoutAutoLayout.JSClassName : string; begin
  Result := 'Ext.layout.AutoLayout';
end;

procedure TExtLayoutBorderLayoutRegion.SetFAnimFloat(Value : Boolean); begin
  FAnimFloat := Value;
  JSCode('animFloat:' + VarToJSON([Value]));
end;

procedure TExtLayoutBorderLayoutRegion.SetFAutoHide(Value : Boolean); begin
  FAutoHide := Value;
  JSCode('autoHide:' + VarToJSON([Value]));
end;

procedure TExtLayoutBorderLayoutRegion.SetFCmargins(Value : TExtObject); begin
  FCmargins := Value;
    JSCode('cmargins:' + VarToJSON([Value, false]));
end;

procedure TExtLayoutBorderLayoutRegion.SetFCollapseMode(Value : String); begin
  FCollapseMode := Value;
  JSCode('collapseMode:' + VarToJSON([Value]));
end;

procedure TExtLayoutBorderLayoutRegion.SetFCollapsible(Value : Boolean); begin
  FCollapsible := Value;
  JSCode('collapsible:' + VarToJSON([Value]));
end;

procedure TExtLayoutBorderLayoutRegion.SetFFloatable(Value : Boolean); begin
  FFloatable := Value;
  JSCode('floatable:' + VarToJSON([Value]));
end;

procedure TExtLayoutBorderLayoutRegion.SetFMargins(Value : TExtObject); begin
  FMargins := Value;
    JSCode('margins:' + VarToJSON([Value, false]));
end;

procedure TExtLayoutBorderLayoutRegion.SetFMinHeight(Value : Integer); begin
  FMinHeight := Value;
  JSCode('minHeight:' + VarToJSON([Value]));
end;

procedure TExtLayoutBorderLayoutRegion.SetFMinWidth(Value : Integer); begin
  FMinWidth := Value;
  JSCode('minWidth:' + VarToJSON([Value]));
end;

procedure TExtLayoutBorderLayoutRegion.SetFSplit(Value : Boolean); begin
  FSplit := Value;
  JSCode('split:' + VarToJSON([Value]));
end;

procedure TExtLayoutBorderLayoutRegion.SetFIsCollapsed(Value : Boolean); begin
  FIsCollapsed := Value;
  JSCode(JSName + '.isCollapsed=' + VarToJSON([Value]) + ';');
end;

procedure TExtLayoutBorderLayoutRegion.SetFLayout(Value : TExtObject); begin
  FLayout := Value;
    JSCode(JSName + '.layout=' + VarToJSON([Value, false]) + ';');
end;

procedure TExtLayoutBorderLayoutRegion.SetFPanel(Value : TExtPanel); begin
  FPanel := Value;
    JSCode(JSName + '.panel=' + VarToJSON([Value, false]) + ';');
end;

procedure TExtLayoutBorderLayoutRegion.SetFPosition(Value : String); begin
  FPosition := Value;
  JSCode(JSName + '.position=' + VarToJSON([Value]) + ';');
end;

class function TExtLayoutBorderLayoutRegion.JSClassName : string; begin
  Result := 'Ext.layout.BorderLayout.Region';
end;

procedure TExtLayoutBorderLayoutRegion.InitDefaults; begin
  inherited;
  FAnimFloat := true;
  FAutoHide := true;
  FCmargins := TExtObject.CreateInternal(Self, 'cmargins');
  FFloatable := true;
  FMargins := TExtObject.CreateInternal(Self, 'margins');
  FMinHeight := 50;
  FMinWidth := 50;
  FPanel := TExtPanel.CreateInternal(Self, 'panel');
end;

function TExtLayoutBorderLayoutRegion.MustHaveACenterRegion : TExtFunction; begin
  JSCode(JSName + '.must have a center region();', 'TExtLayoutBorderLayoutRegion');
  Result := Self;
end;

function TExtLayoutBorderLayoutRegion.GetMinHeight : TExtFunction; begin
  JSCode(JSName + '.getMinHeight();', 'TExtLayoutBorderLayoutRegion');
  Result := Self;
end;

function TExtLayoutBorderLayoutRegion.GetMinWidth : TExtFunction; begin
  JSCode(JSName + '.getMinWidth();', 'TExtLayoutBorderLayoutRegion');
  Result := Self;
end;

function TExtLayoutBorderLayoutRegion.GetSize : TExtFunction; begin
  JSCode(JSName + '.getSize();', 'TExtLayoutBorderLayoutRegion');
  Result := Self;
end;

function TExtLayoutBorderLayoutRegion.IsVisible : TExtFunction; begin
  JSCode(JSName + '.isVisible();', 'TExtLayoutBorderLayoutRegion');
  Result := Self;
end;

function TExtLayoutBorderLayoutRegion.SetPanel(Panel : TExtPanel) : TExtFunction; begin
  JSCode(JSName + '.setPanel(' + VarToJSON([Panel, false]) + ');', 'TExtLayoutBorderLayoutRegion');
  Result := Self;
end;

function TExtLayoutBorderLayoutRegion.SlideIn : TExtFunction; begin
  JSCode(JSName + '.slideIn();', 'TExtLayoutBorderLayoutRegion');
  Result := Self;
end;

function TExtLayoutBorderLayoutRegion.SlideOut : TExtFunction; begin
  JSCode(JSName + '.slideOut();', 'TExtLayoutBorderLayoutRegion');
  Result := Self;
end;

procedure TExtLayoutContainerLayout.SetFExtraCls(Value : String); begin
  FExtraCls := Value;
  JSCode('extraCls:' + VarToJSON([Value]));
end;

procedure TExtLayoutContainerLayout.SetFRenderHidden(Value : Boolean); begin
  FRenderHidden := Value;
  JSCode('renderHidden:' + VarToJSON([Value]));
end;

procedure TExtLayoutContainerLayout.SetFActiveItem(Value : TExtComponent); begin
  FActiveItem := Value;
    JSCode(JSName + '.activeItem=' + VarToJSON([Value, false]) + ';');
end;

procedure TExtLayoutContainerLayout.SetFFieldTpl(Value : TExtTemplate); begin
  FFieldTpl := Value;
    JSCode(JSName + '.fieldTpl=' + VarToJSON([Value, false]) + ';');
end;

procedure TExtLayoutContainerLayout.SetFIfJS(Value : TExtObject); begin
  FIfJS := Value;
    JSCode(JSName + '.ifJS=' + VarToJSON([Value, false]) + ';');
end;

class function TExtLayoutContainerLayout.JSClassName : string; begin
  Result := 'Ext.layout.ContainerLayout';
end;

procedure TExtLayoutContainerLayout.InitDefaults; begin
  inherited;
  FActiveItem := TExtComponent.CreateInternal(Self, 'activeItem');
  FFieldTpl := TExtTemplate.CreateInternal(Self, 'fieldTpl');
  FIfJS := TExtObject.CreateInternal(Self, 'ifJS');
end;

function TExtLayoutContainerLayout.ParseMargins(V : Integer) : TExtFunction; begin
  JSCode(JSName + '.parseMargins(' + VarToJSON([V]) + ');', 'TExtLayoutContainerLayout');
  Result := Self;
end;

function TExtLayoutContainerLayout.ParseMargins(V : String) : TExtFunction; begin
  JSCode(JSName + '.parseMargins(' + VarToJSON([V]) + ');', 'TExtLayoutContainerLayout');
  Result := Self;
end;

class function TExtLayoutMenuLayout.JSClassName : string; begin
  Result := 'Ext.layout.MenuLayout';
end;

procedure TExtLayoutTableLayout.SetFColumns(Value : Integer); begin
  FColumns := Value;
  JSCode('columns:' + VarToJSON([Value]));
end;

procedure TExtLayoutTableLayout.SetFTableAttrs(Value : TExtObject); begin
  FTableAttrs := Value;
    JSCode('tableAttrs:' + VarToJSON([Value, false]));
end;

class function TExtLayoutTableLayout.JSClassName : string; begin
  Result := 'Ext.layout.TableLayout';
end;

procedure TExtLayoutTableLayout.InitDefaults; begin
  inherited;
  FTableAttrs := TExtObject.CreateInternal(Self, 'tableAttrs');
end;

class function TExtLayoutFitLayout.JSClassName : string; begin
  Result := 'Ext.layout.FitLayout';
end;

class function TExtLayoutColumnLayout.JSClassName : string; begin
  Result := 'Ext.layout.ColumnLayout';
end;

procedure TExtLayoutBoxLayout.SetFDefaultMargins(Value : TExtObject); begin
  FDefaultMargins := Value;
    JSCode('defaultMargins:' + VarToJSON([Value, false]));
end;

procedure TExtLayoutBoxLayout.SetFPadding(Value : String); begin
  FPadding := Value;
  JSCode('padding:' + VarToJSON([Value]));
end;

class function TExtLayoutBoxLayout.JSClassName : string; begin
  Result := 'Ext.layout.BoxLayout';
end;

procedure TExtLayoutBoxLayout.InitDefaults; begin
  inherited;
  FDefaultMargins := TExtObject.CreateInternal(Self, 'defaultMargins');
end;

function TExtLayoutBoxLayout.UpdateChildBoxes(Boxes : TExtObjectList) : TExtFunction; begin
  JSCode(JSName + '.updateChildBoxes(' + VarToJSON(Boxes) + ');', 'TExtLayoutBoxLayout');
  Result := Self;
end;

procedure TExtLayoutAnchorLayout.SetFAnchor(Value : String); begin
  FAnchor := Value;
  JSCode('anchor:' + VarToJSON([Value]));
end;

procedure TExtLayoutAnchorLayout.SetFDefaultAnchor(Value : String); begin
  FDefaultAnchor := Value;
  JSCode('defaultAnchor:' + VarToJSON([Value]));
end;

class function TExtLayoutAnchorLayout.JSClassName : string; begin
  Result := 'Ext.layout.AnchorLayout';
end;

procedure TExtLayoutToolbarLayout.SetFHiddenItems(Value : TExtObjectList); begin
  FHiddenItems := Value;
    JSCode(JSName + '.hiddenItems=' + VarToJSON([Value, false]) + ';');
end;

procedure TExtLayoutToolbarLayout.SetFNoItemsMenuText(Value : String); begin
  FNoItemsMenuText := Value;
  JSCode(JSName + '.noItemsMenuText=' + VarToJSON([Value]) + ';');
end;

procedure TExtLayoutToolbarLayout.SetFTriggerWidth(Value : Integer); begin
  FTriggerWidth := Value;
  JSCode(JSName + '.triggerWidth=' + VarToJSON([Value]) + ';');
end;

class function TExtLayoutToolbarLayout.JSClassName : string; begin
  Result := 'Ext.layout.ToolbarLayout';
end;

procedure TExtLayoutToolbarLayout.InitDefaults; begin
  inherited;
  FHiddenItems := TExtObjectList.CreateInternal(Self, 'hiddenItems');
end;

procedure TExtLayoutBorderLayoutSplitRegion.SetFCollapsibleSplitTip(Value : String); begin
  FCollapsibleSplitTip := Value;
  JSCode('collapsibleSplitTip:' + VarToJSON([Value]));
end;

procedure TExtLayoutBorderLayoutSplitRegion.SetFSplitTip(Value : String); begin
  FSplitTip := Value;
  JSCode('splitTip:' + VarToJSON([Value]));
end;

procedure TExtLayoutBorderLayoutSplitRegion.SetFTickSize(Value : Integer); begin
  FTickSize := Value;
  JSCode('tickSize:' + VarToJSON([Value]));
end;

procedure TExtLayoutBorderLayoutSplitRegion.SetFUseSplitTips(Value : Boolean); begin
  FUseSplitTips := Value;
  JSCode('useSplitTips:' + VarToJSON([Value]));
end;

class function TExtLayoutBorderLayoutSplitRegion.JSClassName : string; begin
  Result := 'Ext.layout.BorderLayout.SplitRegion';
end;

procedure TExtLayoutBorderLayoutSplitRegion.InitDefaults; begin
  inherited;
  FCollapsibleSplitTip := 'Drag to resize. Double click to hide.';
  FSplitTip := 'Drag to resize.';
end;

function TExtLayoutBorderLayoutSplitRegion.GetSplitBar : TExtFunction; begin
  JSCode(JSName + '.getSplitBar();', 'TExtLayoutBorderLayoutSplitRegion');
  Result := Self;
end;

class function TExtLayoutBorderLayout.JSClassName : string; begin
  Result := 'Ext.layout.BorderLayout';
end;

procedure TExtLayoutVBoxLayout.SetFAlign(Value : String); begin
  FAlign := Value;
  JSCode('align:' + VarToJSON([Value]));
end;

procedure TExtLayoutVBoxLayout.SetFFlex(Value : Integer); begin
  FFlex := Value;
  JSCode('flex:' + VarToJSON([Value]));
end;

procedure TExtLayoutVBoxLayout.SetFPack(Value : String); begin
  FPack := Value;
  JSCode('pack:' + VarToJSON([Value]));
end;

class function TExtLayoutVBoxLayout.JSClassName : string; begin
  Result := 'Ext.layout.VBoxLayout';
end;

procedure TExtLayoutHBoxLayout.SetFAlign(Value : String); begin
  FAlign := Value;
  JSCode('align:' + VarToJSON([Value]));
end;

procedure TExtLayoutHBoxLayout.SetFFlex(Value : Integer); begin
  FFlex := Value;
  JSCode('flex:' + VarToJSON([Value]));
end;

procedure TExtLayoutHBoxLayout.SetFPack(Value : String); begin
  FPack := Value;
  JSCode('pack:' + VarToJSON([Value]));
end;

class function TExtLayoutHBoxLayout.JSClassName : string; begin
  Result := 'Ext.layout.HBoxLayout';
end;

procedure TExtLayoutAccordionLayout.SetFActiveOnTop(Value : Boolean); begin
  FActiveOnTop := Value;
  JSCode('activeOnTop:' + VarToJSON([Value]));
end;

procedure TExtLayoutAccordionLayout.SetFAnimate(Value : Boolean); begin
  FAnimate := Value;
  JSCode('animate:' + VarToJSON([Value]));
end;

procedure TExtLayoutAccordionLayout.SetFAutoWidth(Value : Boolean); begin
  FAutoWidth := Value;
  JSCode('autoWidth:' + VarToJSON([Value]));
end;

procedure TExtLayoutAccordionLayout.SetFCollapseFirst(Value : Boolean); begin
  FCollapseFirst := Value;
  JSCode('collapseFirst:' + VarToJSON([Value]));
end;

procedure TExtLayoutAccordionLayout.SetFFill(Value : Boolean); begin
  FFill := Value;
  JSCode('fill:' + VarToJSON([Value]));
end;

procedure TExtLayoutAccordionLayout.SetFHideCollapseTool(Value : Boolean); begin
  FHideCollapseTool := Value;
  JSCode('hideCollapseTool:' + VarToJSON([Value]));
end;

procedure TExtLayoutAccordionLayout.SetFSequence(Value : Boolean); begin
  FSequence := Value;
  JSCode('sequence:' + VarToJSON([Value]));
end;

procedure TExtLayoutAccordionLayout.SetFTitleCollapse(Value : Boolean); begin
  FTitleCollapse := Value;
  JSCode('titleCollapse:' + VarToJSON([Value]));
end;

class function TExtLayoutAccordionLayout.JSClassName : string; begin
  Result := 'Ext.layout.AccordionLayout';
end;

procedure TExtLayoutAccordionLayout.InitDefaults; begin
  inherited;
  FAutoWidth := true;
  FFill := true;
  FTitleCollapse := true;
end;

function TExtLayoutAccordionLayout.SetActiveItem(Item : String) : TExtFunction; begin
  JSCode(JSName + '.setActiveItem(' + VarToJSON([Item]) + ');', 'TExtLayoutAccordionLayout');
  Result := Self;
end;

function TExtLayoutAccordionLayout.SetActiveItem(Item : Integer) : TExtFunction; begin
  JSCode(JSName + '.setActiveItem(' + VarToJSON([Item]) + ');', 'TExtLayoutAccordionLayout');
  Result := Self;
end;

class function TExtLayoutAbsoluteLayout.JSClassName : string; begin
  Result := 'Ext.layout.AbsoluteLayout';
end;

procedure TExtLayoutFormLayout.SetFFieldTpl(Value : TExtTemplate); begin
  FFieldTpl := Value;
    JSCode('fieldTpl:' + VarToJSON([Value, false]));
end;

procedure TExtLayoutFormLayout.SetFLabelSeparator(Value : String); begin
  FLabelSeparator := Value;
  JSCode('labelSeparator:' + VarToJSON([Value]));
end;

procedure TExtLayoutFormLayout.SetFTrackLabels(Value : Boolean); begin
  FTrackLabels := Value;
  JSCode('trackLabels:' + VarToJSON([Value]));
end;

procedure TExtLayoutFormLayout.SetFLabelStyle(Value : String); begin
  FLabelStyle := Value;
  JSCode(JSName + '.labelStyle=' + VarToJSON([Value]) + ';');
end;

class function TExtLayoutFormLayout.JSClassName : string; begin
  Result := 'Ext.layout.FormLayout';
end;

procedure TExtLayoutFormLayout.InitDefaults; begin
  inherited;
  FFieldTpl := TExtTemplate.CreateInternal(Self, 'fieldTpl');
end;

function TExtLayoutFormLayout.GetTemplateArgs(Field : TExtFormField) : TExtFunction; begin
  JSCode(JSName + '.getTemplateArgs(' + VarToJSON([Field, false]) + ');', 'TExtLayoutFormLayout');
  Result := Self;
end;

procedure TExtLayoutCardLayout.SetFDeferredRender(Value : Boolean); begin
  FDeferredRender := Value;
  JSCode('deferredRender:' + VarToJSON([Value]));
end;

procedure TExtLayoutCardLayout.SetFLayoutOnCardChange(Value : Boolean); begin
  FLayoutOnCardChange := Value;
  JSCode('layoutOnCardChange:' + VarToJSON([Value]));
end;

class function TExtLayoutCardLayout.JSClassName : string; begin
  Result := 'Ext.layout.CardLayout';
end;

function TExtLayoutCardLayout.SetActiveItem(Item : String) : TExtFunction; begin
  JSCode(JSName + '.setActiveItem(' + VarToJSON([Item]) + ');', 'TExtLayoutCardLayout');
  Result := Self;
end;

function TExtLayoutCardLayout.SetActiveItem(Item : Integer) : TExtFunction; begin
  JSCode(JSName + '.setActiveItem(' + VarToJSON([Item]) + ');', 'TExtLayoutCardLayout');
  Result := Self;
end;

end.