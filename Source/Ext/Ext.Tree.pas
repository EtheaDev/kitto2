unit Ext.Tree;

interface

uses
  StrUtils
  , Ext.Base
  , Ext.Data
  ;

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
    function SetText(const AText: string): TExtExpression;
    function SetTooltip(const ATip: string; const ATitle: string = ''): TExtExpression;
    function Toggle: TExtExpression;
    function Unselect(const ASilent: Boolean = false): TExtExpression;
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
    procedure SetRoot(const AValue: TExtTreeTreeNode);
  public
    class function JSClassName: string; override;
    property Root: TExtTreeTreeNode read FRoot write SetRoot;
    property RootVisible: Boolean read FRootVisible write SetRootVisible;
  end;

implementation

uses
  Kitto.Web.Response
  ;

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

function TExtTreeTreeNode.SetText(const AText: string): TExtExpression;
begin
  FText := AText;
  Result := TKWebResponse.Current.Items.CallMethod(Self, 'setText').AddParam(AText).AsExpression;
end;

function TExtTreeTreeNode.SetTooltip(const ATip, ATitle: string): TExtExpression;
begin
  Result := TKWebResponse.Current.Items.CallMethod(Self, 'setTooltip').AddParam(ATip).AddParam(ATitle).AsExpression;
end;

function TExtTreeTreeNode.Toggle: TExtExpression;
begin
  Result := TKWebResponse.Current.Items.CallMethod(Self, 'toggle').AsExpression;
end;

function TExtTreeTreeNode.Unselect(const ASilent: Boolean): TExtExpression;
begin
  Result := TKWebResponse.Current.Items.CallMethod(Self, 'unselect').AddParam(ASilent).AsExpression;
end;

procedure TExtTreeTreePanel.SetRoot(const AValue: TExtTreeTreeNode);
begin
  FRoot := TExtTreeTreeNode(SetConfigItem('root', AValue));
end;

procedure TExtTreeTreePanel.SetRootVisible(const AValue: Boolean);
begin
  FRootVisible := SetConfigItem('rootVisible', AValue);
end;

class function TExtTreeTreePanel.JSClassName: string;
begin
  Result := 'Ext.tree.TreePanel';
end;

end.
