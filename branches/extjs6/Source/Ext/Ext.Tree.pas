unit Ext.Tree;

interface

uses
  StrUtils,
  Kitto.Ext,
  Ext.Util,
  Ext.Base,
  Ext.Data;

type
  TExtTreeTreeNode = class;
  TExtTreeTreePanel = class;

  TExtTreeTreeNode = class(TExtDataNode)
  private
    FExpanded: Boolean;
    FHidden: Boolean;
    FQtip: string;
    FIconCls: String;
    FExpandable: Boolean;
    FDisabled: Boolean;
    FText: string;
    procedure SetDisabled(const AValue: Boolean);
    procedure SetExpandable(const AValue: Boolean);
    procedure SetExpanded(const AValue: Boolean);
    procedure SetHidden(const AValue: Boolean);
    procedure SetIconCls(const AValue: string);
    procedure SetQtip(const AValue: string);
    procedure _SetText(const AValue: string);
  protected
    function GetObjectNamePrefix: string; override;
  public
    class function JSClassName: string; override;
    function SetText(const AText: string): TExtFunction;
    function SetTooltip(const ATip: string; const ATitle: string = ''): TExtFunction;
    function Toggle: TExtFunction;
    function Unselect(const ASilent: Boolean = false): TExtFunction;
    property Disabled: Boolean read FDisabled write SetDisabled;
    property Expandable: Boolean read FExpandable write SetExpandable;
    property Expanded: Boolean read FExpanded write SetExpanded;
    property Hidden: Boolean read FHidden write SetHidden;
    property IconCls: String read FIconCls write SetIconCls;
    property Qtip: string read FQtip write SetQtip;
    property Text: string read FText write _SetText;
  end;

  TExtTreeTreePanel = class(TExtPanel)
  private
    FRoot: TExtTreeTreeNode;
    FRootVisible: Boolean; // true
    procedure SetRootVisible(const AValue: Boolean);
    function GetRoot: TExtTreeTreeNode;
    procedure SetRoot(const AValue: TExtTreeTreeNode);
  public
    class function JSClassName: string; override;
    property Root: TExtTreeTreeNode read GetRoot write SetRoot;
    property RootVisible: Boolean read FRootVisible write SetRootVisible;
  end;

implementation

procedure TExtTreeTreeNode.SetDisabled(const AValue: Boolean);
begin
  FDisabled := SetConfigItem('disabled', AValue);
end;

procedure TExtTreeTreeNode.SetExpandable(const AValue: Boolean);
begin
  FExpandable := SetConfigItem('expandable', AValue);
end;

procedure TExtTreeTreeNode.SetExpanded(const AValue: Boolean);
begin
  FExpanded := SetConfigItem('expanded', AValue);
end;

procedure TExtTreeTreeNode.SetHidden(const AValue: Boolean);
begin
  FHidden := SetConfigItem('hidden', AValue);
end;

procedure TExtTreeTreeNode.SetIconCls(const AValue: string);
begin
  FIconCls := SetConfigItem('iconCls', AValue);
end;

procedure TExtTreeTreeNode.SetQtip(const AValue: string);
begin
  FQtip := SetConfigItem('qtip', AValue);
end;

procedure TExtTreeTreeNode._SetText(const AValue: string);
begin
  FText := AValue;
  SetConfigItem('text', 'setText', AValue);
end;

class function TExtTreeTreeNode.JSClassName: string;
begin
  Result := 'Object';
end;

function TExtTreeTreeNode.GetObjectNamePrefix: string;
begin
  Result := 'treenode';
end;

function TExtTreeTreeNode.SetText(const AText: string): TExtFunction;
begin
  FText := AText;
  Result := CallMethod('setText').AddParam(AText).AsFunction;
end;

function TExtTreeTreeNode.SetTooltip(const ATip, ATitle: string): TExtFunction;
begin
  Result := CallMethod('setTooltip').AddParam(ATip).AddParam(ATitle).AsFunction;
end;

function TExtTreeTreeNode.Toggle: TExtFunction;
begin
  Result := CallMethod('toggle').AsFunction;
end;

function TExtTreeTreeNode.Unselect(const ASilent: Boolean): TExtFunction;
begin
  Result := CallMethod('unselect').AddParam(ASilent).AsFunction;
end;

procedure TExtTreeTreePanel.SetRoot(const AValue: TExtTreeTreeNode);
begin
  FRoot := TExtTreeTreeNode(SetConfigItem('root', FRoot));
end;

procedure TExtTreeTreePanel.SetRootVisible(const AValue: Boolean);
begin
  FRootVisible := SetConfigItem('rootVisible', AValue);
end;

class function TExtTreeTreePanel.JSClassName: string;
begin
  Result := 'Ext.tree.TreePanel';
end;

function TExtTreeTreePanel.GetRoot: TExtTreeTreeNode;
begin
  Result := FRoot;
end;

end.
