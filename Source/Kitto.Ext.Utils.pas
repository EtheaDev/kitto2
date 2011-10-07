unit Kitto.Ext.Utils;

interface

uses
  Ext, ExtPascal, ExtMenu, ExtTree,
  EF.ObserverIntf,
  Kitto.Controller, Kitto.Metadata.Views;

type
  ///	<summary>
  ///	  Renders a tree view on a container in various ways: as a set of buttons
  ///	  with submenus, an Ext treeview control, etc.
  ///	</summary>
  TKExtTreeViewRenderer = class
  private
    FOwner: TExtObject;
    FAddedItems: Integer;
    procedure AddButton(const AViewRef: TKViewRef; const AContainer: TExtContainer);
    procedure AddMenuItem(const AViewRefs: TKViewRefs; const AMenu: TExtMenuMenu);
    procedure AddNode(const AViewRef: TKViewRef; const AParent: TExtTreeTreeNode);
    function GetClickHandler(const AView: TKView): TExtFunction;
  public
    {
      AOwner should publish the ExecuteGUIElement method. See GetClickHandler.
    }
    constructor Create(const AOwner: TExtObject);
    {
      Attaches to the container a set of buttons, one for each top-level
      element of AGUIElements. Each button matching the realm name
      has a submenu tree with the child GUI elements.
      Returns the total number of effectively added items.
    }
    function RenderAsButtons(const ATreeView: TKTreeView;
      const AContainer: TExtContainer): Integer;
    {
      Populates a tree view with all matching GUI elements.
      Returns the total number of effectively added items.
    }
    function RenderAsTree(const ATreeView: TKTreeView;
      const ARoot: TExtTreeTreeNode): Integer;
  end;

function DelphiDateFormatToJSDateFormat(const ADateFormat: string): string;
function DelphiTimeFormatToJSTimeFormat(const ATimeFormat: string): string;

implementation

uses
  SysUtils, StrUtils, HTTPApp,
  EF.SysUtils, EF.Classes, EF.Localization,
  Kitto.Environment, Kitto.AccessControl, Kitto.Ext.Session, Kitto.Ext.Base;

{ TKExtTreeViewRenderer }

procedure TKExtTreeViewRenderer.AddMenuItem(const AViewRefs: TKViewRefs;
  const AMenu: TExtMenuMenu);
var
  I: Integer;
  LViewRef: TKViewRef;
  LMenuItem: TExtMenuItem;
  LSubMenu: TExtMenuMenu;
  LIsEnabled: Boolean;
begin
  Assert(Assigned(AViewRefs));
  Assert(Assigned(AMenu));

  for I := 0 to AViewRefs.Count - 1 do
  begin
    LViewRef := AViewRefs[I];

    { TODO : implement AC }
    //if Environment.IsAccessGranted(LViewRef.View.GetResourceURI, ACM_VIEW) then
    begin
      LIsEnabled := Assigned(LViewRef.View) {and Environment.IsAccessGranted(AViewRef.View.GetResourceURI, ACM_RUN)};
      LMenuItem := TExtMenuItem.AddTo(AMenu.Items);
      try
        Inc(FAddedItems);
        LMenuItem.Text := HTMLEncode(_(LViewRef.DisplayLabel));
        if LViewRef.View <> nil then
          LMenuItem.IconCls := Session.SetViewIconStyle(LViewRef.View, LViewRef.ImageName);
        if LViewRef.ViewRefs.Count > 0 then
        begin
          LSubMenu := TExtMenuMenu.Create;
          try
            LMenuItem.Menu := LSubMenu;
            AddMenuItem(LViewRef.ViewRefs, LSubMenu);
            {if LSubMenu.Items.Count = 0 then
            begin
              LMenuItem.Menu := nil;
              FreeAndNil(LSubMenu);
            end;}
          except
            FreeAndNil(LSubMenu);
            raise;
          end;
        end
        else
        begin
          LMenuItem.Handler := GetClickHandler(LViewRef.View);
          LMenuItem.Disabled := not LIsEnabled;
        end;
      except
        FreeAndNil(LMenuItem);
        raise;
      end;
    end;
  end;
end;

constructor TKExtTreeViewRenderer.Create(const AOwner: TExtObject);
begin
  Assert(Assigned(AOwner));

  inherited Create;
  FOwner := AOwner;
end;

function TKExtTreeViewRenderer.GetClickHandler(
  const AView: TKView): TExtFunction;
begin
  Assert(Assigned(FOwner));

  if Assigned(AView) then
    Result := FOwner.Ajax('DisplayView', ['Name', AView.PersistentName])
  else
    Result := nil;
end;

procedure TKExtTreeViewRenderer.AddButton(const AViewRef: TKViewRef;
  const AContainer: TExtContainer);
var
  LButton: TExtButton;
  LMenu: TExtMenuMenu;
  LIsEnabled: Boolean;
begin
  Assert(Assigned(AViewRef));
  Assert(Assigned(AContainer));

  { TODO : implement AC }
  //if Environment.IsAccessGranted(AViewRef.View.GetResourceURI, ACM_VIEW) then
  begin
    LIsEnabled := Assigned(AViewRef.View) {and Environment.IsAccessGranted(AViewRef.View.GetResourceURI, ACM_RUN)};
    LButton := TExtButton.AddTo(AContainer.Items);
    try
      Inc(FAddedItems);
      LButton.Text := HTMLEncode(_(AViewRef.DisplayLabel));
      if AViewRef.View <> nil then
        LButton.IconCls := Session.SetViewIconStyle(AViewRef.View, AViewRef.ImageName);
      if AViewRef.ViewRefs.Count > 0 then
      begin
        LMenu := TExtMenuMenu.Create;
        try
          LButton.Menu := LMenu;
          AddMenuItem(AViewRef.ViewRefs, LMenu);
          {if LMenu.Items.Count = 0 then
          begin
            LButton.Menu_ := nil;
            FreeAndNil(LMenu);
          end;}
        except
          FreeAndNil(LMenu);
          raise;
        end;
      end
      else
      begin
        LButton.Handler := GetClickHandler(AViewRef.View);
        LButton.Disabled := not LIsEnabled;
      end;
    except
      FreeAndNil(LButton);
      raise;
    end;
  end;
end;

function TKExtTreeViewRenderer.RenderAsButtons(
  const ATreeView: TKTreeView; const AContainer: TExtContainer): Integer;
var
  I: Integer;

  procedure EmptyContainer;
  begin
    { TODO : implement }
  end;

begin
  Assert(Assigned(ATreeView));
  Assert(Assigned(AContainer));

  FAddedItems := 0;
  for I := 0 to ATreeView.ViewRefs.Count - 1 do
    AddButton(ATreeView.ViewRefs[I], AContainer);
  Result := FAddedItems;
end;

procedure TKExtTreeViewRenderer.AddNode(const AViewRef: TKViewRef;
  const AParent: TExtTreeTreeNode);
var
  LNode: TExtTreeTreeNode;
  I: Integer;
  LIsEnabled: Boolean;
begin
  Assert(Assigned(AViewRef));
  Assert(Assigned(AParent));

  { TODO : implement AC }
  //if Environment.IsAccessGranted(AViewRef.GetResourceURI, ACM_VIEW) then
  begin
    LIsEnabled := Assigned(AViewRef.View) {and Environment.IsAccessGranted(AViewRef.View.GetResourceURI, ACM_RUN)};
    LNode := TExtTreeTreeNode.Create;
    try
      LNode.Text := HTMLEncode(_(AViewRef.DisplayLabel));
      if AViewRef.View <> nil then
        LNode.IconCls := Session.SetViewIconStyle(AViewRef.View, AViewRef.ImageName);
      if AViewRef.ViewRefs.Count > 0 then
      begin
        for I := 0 to AViewRef.ViewRefs.Count - 1 do
          AddNode(AViewRef.ViewRefs[I], LNode);
        LNode.Expandable := True;
        LNode.Expanded := True;
        LNode.Leaf := False;
      end
      else
      begin
        LNode.On('click', GetClickHandler(AViewRef.View));
        LNode.Expandable := False;
        LNode.Expanded := False;
        LNode.Leaf := True;
        LNode.Disabled := not LIsEnabled;
      end;
      AParent.AppendChild(LNode);
      Inc(FAddedItems);
    except
      FreeAndNil(LNode);
      raise;
    end;
  end;
end;

function TKExtTreeViewRenderer.RenderAsTree(
  const ATreeView: TKTreeView; const ARoot: TExtTreeTreeNode): Integer;
var
  I: Integer;

  procedure EmptyTree;
  begin
    { TODO : implement }
  end;

begin
  Assert(Assigned(ATreeView));
  Assert(Assigned(ARoot));

  FAddedItems := 0;
  for I := 0 to ATreeView.ViewRefs.Count - 1 do
    AddNode(ATreeView.ViewRefs[I], ARoot);
  Result := FAddedItems;
end;

function DelphiDateFormatToJSDateFormat(const ADateFormat: string): string;
begin
  Result := ReplaceText(ADateFormat, 'yyyy', 'Y');
  Result := ReplaceText(Result, 'yy', 'y');
  Result := ReplaceText(Result, 'dd', 'd');
  Result := ReplaceText(Result, 'mm', 'm');
end;

function DelphiTimeFormatToJSTimeFormat(const ATimeFormat: string): string;
begin
  Result := ReplaceText(ATimeFormat, 'hh', 'H');
  Result := ReplaceText(Result, 'mm', 'i');
  Result := ReplaceText(Result, 'ss', 's');
end;

end.
