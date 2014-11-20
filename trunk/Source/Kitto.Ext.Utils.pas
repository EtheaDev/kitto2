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

unit Kitto.Ext.Utils;

{$I Kitto.Defines.inc}

interface

uses
  SysUtils,
  Ext, ExtPascal, ExtPascalUtils, ExtMenu, ExtTree,
  EF.ObserverIntf, EF.Tree,
  Kitto.Ext.Controller, Kitto.Metadata.Views, Kitto.Ext.Session;

type
  TKExtTreeTreeNode = class(TExtTreeTreeNode)
  private
    FView: TKView;
    procedure SetView(const AValue: TKView);
  public
    property View: TKView read FView write SetView;
  end;

  TKExtButton = class(TExtButton)
  private
    FView: TKView;
    procedure SetView(const AValue: TKView);
  public
    property View: TKView read FView write SetView;
  end;

  TKExtMenuItem = class(TExtMenuItem)
  private
    FView: TKView;
    procedure SetView(const AValue: TKView);
  public
    property View: TKView read FView write SetView;
  end;

  ///	<summary>
  ///	  Renders a tree view on a container in various ways: as a set of buttons
  ///	  with submenus, an Ext treeview control, etc.
  ///	</summary>
  TKExtTreeViewRenderer = class
  private
    FOwner: TExtObject;
    FClickHandler: TExtProcedure;
    FAddedItems: Integer;
    FSession: TKExtSession;
    procedure AddButton(const ANode: TKTreeViewNode; const ADisplayLabel: string; const AContainer: TExtContainer);
    procedure AddMenuItem(const ANode: TKTreeViewNode; const AMenu: TExtMenuMenu);
    procedure AddNode(const ANode: TKTreeViewNode; const ADisplayLabel: string; const AParent: TExtTreeTreeNode);
    function GetClickFunction(const AView: TKView): TExtFunction;

    ///	<summary>
    ///   Clones the specified tree view, filters all invisible items
    ///	  (including folders containing no visible items) and returns the
    ///	  clone.
    /// </summary>
    ///	<remarks>
    ///   The caller is responsible for freeing the returned object.
    /// </remarks>
    function CloneAndFilter(const ATreeView: TKTreeView): TKTreeView;
    procedure Filter(const ANode: TKTreeViewNode);
  public
    property Session: TKExtSession read FSession write FSession;

    ///	<summary>
    ///	  Attaches to the container a set of buttons, one for each top-level
    ///	  element of the specified tree view. Each button has a submenu tree
    ///	  with the child views. Returns the total number of effectively added
    ///	  items.
    ///	</summary>
    function RenderAsButtons(const ATreeView: TKTreeView;
      const AContainer: TExtContainer; const AOwner: TExtObject;
      const AClickHandler: TExtProcedure): Integer;

    ///	<summary>
    ///	  Renders a tree under ARoot with all views in the tree view. Returns
    ///	  the total number of effectively added items.
    ///	</summary>
    function RenderAsTree(const ATreeView: TKTreeView; const ARoot: TExtTreeTreeNode;
      const AOwner: TExtObject; const AClickHandler: TExtProcedure): Integer;

    ///	<summary>
    ///	  Renders a tree by calling AProc for each top-level element in the tree view.
    ///	</summary>
    function Render(const ATreeView: TKTreeView; const AProc: TProc<TKTreeViewNode, string>;
      const AOwner: TExtObject; const AClickHandler: TExtProcedure): Integer;
  end;

function DelphiDateTimeFormatToJSDateTimeFormat(const ADateTimeFormat: string): string;
function DelphiDateFormatToJSDateFormat(const ADateFormat: string): string;
function DelphiTimeFormatToJSTimeFormat(const ATimeFormat: string): string;

///	<summary>Adapts a standard number format string (with , as thousand
///	separator and . as decimal separator) according to the
///	specificed format settings for displaying to the user.</summary>
function AdaptExtNumberFormat(const AFormat: string; const AFormatSettings: TFormatSettings): string;


/// <summary>
///   Computes and returns a display label based on the underlying view,
///   if any, or the node itself (if no view is found).
/// </summary>
function GetDisplayLabelFromNode(const ANode: TKTreeViewNode; const AViews: TKViews): string;

/// <summary>
///   Invoke a method of a View that return a string using RTTI
/// </summary>
function CallViewControllerStringMethod(const AView: TKView;
  const AMethodName: string; const ADefaultValue: string): string;

implementation

uses
  Types, StrUtils, RTTI,
  EF.SysUtils, EF.StrUtils, EF.Classes, EF.Localization,
  Kitto.AccessControl, Kitto.Utils, Kitto.Ext.Base;

function CallViewControllerStringMethod(const AView: TKView;
  const AMethodName: string; const ADefaultValue: string): string;
var
  LControllerClass: TClass;
  LContext: TRttiContext;
  LMethod: TRttiMethod;
begin
  Assert(Assigned(AView));
  Assert(AMethodName <> '');

  LControllerClass := TKExtControllerRegistry.Instance.GetClass(AView.ControllerType);
  LMethod := LContext.GetType(LControllerClass).GetMethod(AMethodName);
  if Assigned(LMethod) then
    Result := LMethod.Invoke(LControllerClass, []).AsString
  else
    Result := ADefaultValue;
end;

function GetDisplayLabelFromNode(const ANode: TKTreeViewNode; const AViews: TKViews): string;
var
  LView: TKView;
begin
  Assert(Assigned(ANode));

  LView := ANode.FindView(AViews);
  if Assigned(LView) then
  begin
    Result := _(LView.DisplayLabel);
    if Result = '' then
      Result := CallViewControllerStringMethod(LView, 'GetDefaultDisplayLabel', Result);
  end
  else
    Result := _(ANode.AsString);
  Result := Result;
end;

function GetImageName(const ANode: TKTreeViewNode; const AView: TKView): string;
begin
  Assert(Assigned(ANode));
  Assert(Assigned(AView));

  Result := ANode.GetString('ImageName');
  if Result = '' then
    Result := CallViewControllerStringMethod(AView, 'GetDefaultImageName', '');
end;

{ TKExtTreeViewRenderer }

function TKExtTreeViewRenderer.GetClickFunction(
  const AView: TKView): TExtFunction;
begin
  Assert(Assigned(FOwner));
  Assert(Assigned(FClickHandler));

  if Assigned(AView) then
  begin
    if Session.StatusHost <> nil then
      Result := FOwner.Ajax(FClickHandler, ['View', Integer(AView), 'Dummy', Session.StatusHost.ShowBusy])
    else
      Result := FOwner.Ajax(FClickHandler, ['View', Integer(AView)]);
  end
  else
    Result := nil;
end;

procedure TKExtTreeViewRenderer.AddMenuItem(const ANode: TKTreeViewNode;
  const AMenu: TExtMenuMenu);
var
  I: Integer;
  LMenuItem: TKExtMenuItem;
  LSubMenu: TExtMenuMenu;
  LIsEnabled: Boolean;
  LView: TKView;
  LDisplayLabel: string;
begin
  Assert(Assigned(ANode));
  Assert(Assigned(AMenu));

  for I := 0 to ANode.TreeViewNodeCount - 1 do
  begin
    LView := ANode.TreeViewNodes[I].FindView(Session.Config.Views);

    if not Assigned(LView) or LView.IsAccessGranted(ACM_VIEW) then
    begin
      LIsEnabled := not Assigned(LView) or LView.IsAccessGranted(ACM_RUN);
      LMenuItem := TKExtMenuItem.CreateAndAddTo(AMenu.Items);
      try
        Inc(FAddedItems);
        LMenuItem.View := LView;
        if Assigned(LMenuItem.View) then
        begin
          LMenuItem.IconCls := Session.SetViewIconStyle(LMenuItem.View, GetImageName(ANode.TreeViewNodes[I], LMenuItem.View));
          LMenuItem.On('click', GetClickFunction(LMenuItem.View));
          LMenuItem.Disabled := not LIsEnabled;
          LDisplayLabel := _(LMenuItem.View.DisplayLabel);
          if LDisplayLabel = '' then
            LDisplayLabel := CallViewControllerStringMethod(LView, 'GetDefaultDisplayLabel', '');
          LMenuItem.Text := HTMLEncode(LDisplayLabel);
        end;
        if ANode.TreeViewNodes[I].TreeViewNodeCount > 0 then
        begin
          LSubMenu := TExtMenuMenu.Create(AMenu);
          try
            LMenuItem.Menu := LSubMenu;
            AddMenuItem(ANode.TreeViewNodes[I], LSubMenu);
          except
            FreeAndNil(LSubMenu);
            raise;
          end;
        end;
      except
        FreeAndNil(LMenuItem);
        raise;
      end;
    end;
  end;
end;

procedure TKExtTreeViewRenderer.AddButton(const ANode: TKTreeViewNode;
  const ADisplayLabel: string; const AContainer: TExtContainer);
var
  LButton: TKExtButton;
  LMenu: TExtMenuMenu;
  LIsEnabled: Boolean;
  LView: TKView;
begin
  Assert(Assigned(ANode));
  Assert(Assigned(AContainer));

  LView := ANode.FindView(Session.Config.Views);

  LIsEnabled := not Assigned(LView) or LView.IsAccessGranted(ACM_RUN);
  LButton := TKExtButton.CreateAndAddTo(AContainer.Items);
  try
    Inc(FAddedItems);
    LButton.View := LView;
    if Assigned(LButton.View) then
    begin
      LButton.IconCls := Session.SetViewIconStyle(LButton.View, GetImageName(ANode, LButton.View));
      LButton.On('click', GetClickFunction(LButton.View));
      LButton.Disabled := not LIsEnabled;
    end;
    LButton.Text := HTMLEncode(ADisplayLabel);
    LButton.SetTooltip(LButton.Text);

    if ANode.ChildCount > 0 then
    begin
      LMenu := TExtMenuMenu.Create(AContainer);
      try
        LButton.Menu := LMenu;
        AddMenuItem(ANode, LMenu);
      except
        FreeAndNil(LMenu);
        raise;
      end;
    end;
  except
    FreeAndNil(LButton);
    raise;
  end;
end;

procedure TKExtTreeViewRenderer.AddNode(const ANode: TKTreeViewNode;
  const ADisplayLabel: string; const AParent: TExtTreeTreeNode);
var
  LNode: TKExtTreeTreeNode;
  I: Integer;
  LIsEnabled: Boolean;
  LView: TKView;
  LSubNode: TKTreeViewNode;
  LDisplayLabel: string;
begin
  Assert(Assigned(ANode));
  Assert(Assigned(AParent));

  LView := ANode.FindView(Session.Config.Views);

  LIsEnabled := not Assigned(LView) or LView.IsAccessGranted(ACM_RUN);
  LNode := TKExtTreeTreeNode.Create(AParent.ChildNodes);
  try
    Inc(FAddedItems);
    LNode.View := LView;
    if Assigned(LNode.View) then
    begin
      LNode.IconCls := Session.SetViewIconStyle(LNode.View, GetImageName(ANode, LNode.View));
      LNode.On('click', GetClickFunction(LNode.View));
      LNode.Disabled := not LIsEnabled;
    end;
    LNode.Text := HTMLEncode(ADisplayLabel);
    LNode.SetTooltip(LNode.Text);
    if ANode.TreeViewNodeCount > 0 then
    begin
      for I := 0 to ANode.TreeViewNodeCount - 1 do
      begin
        LSubNode := ANode.TreeViewNodes[I];
        LDisplayLabel := GetDisplayLabelFromNode(LSubNode, Session.Config.Views);
        AddNode(LSubNode, LDisplayLabel, LNode);
      end;
      LNode.Expandable := True;
      if ANode is TKTreeViewFolder then
        LNode.Expanded := not TKTreeViewFolder(ANode).IsInitiallyCollapsed
      else
        LNode.Expanded := True;
      LNode.Leaf := False;
    end;
    AParent.AppendChild(LNode);
  except
    FreeAndNil(LNode);
    raise;
  end;
end;

function TKExtTreeViewRenderer.CloneAndFilter(const ATreeView: TKTreeView): TKTreeView;
var
  I: Integer;
begin
  Assert(Assigned(ATreeView));

  Result := TKTreeView.Clone(ATreeView);

  for I := Result.TreeViewNodeCount - 1 downto 0 do
    Filter(Result.TreeViewNodes[I]);
end;

procedure TKExtTreeViewRenderer.Filter(const ANode: TKTreeViewNode);
var
  LView: TKView;
  I: Integer;
begin
  Assert(Assigned(ANode));

  LView := ANode.FindView(Session.Config.Views);
  if Assigned(LView) and not LView.IsAccessGranted(ACM_VIEW) then
    ANode.Delete
  else
  begin
    for I := ANode.TreeViewNodeCount - 1 downto 0 do
      Filter(ANode.TreeViewNodes[I]);
    // Remove empty folders.
    if (ANode is TKTreeViewFolder) and (ANode.TreeViewNodeCount = 0) then
      ANode.Delete;
  end;
end;

function TKExtTreeViewRenderer.Render(const ATreeView: TKTreeView;
  const AProc: TProc<TKTreeViewNode, string>; const AOwner: TExtObject;
  const AClickHandler: TExtProcedure): Integer;
var
  I: Integer;
  LTreeView: TKTreeView;
  LNode: TKTreeViewNode;
begin
  Assert(Assigned(ATreeView));
  Assert(Assigned(AProc));
  Assert(Assigned(AOwner));
  Assert(Assigned(AClickHandler));

  FOwner := AOwner;
  FClickHandler := AClickHandler;
  FAddedItems := 0;

  LTreeView := CloneAndFilter(ATreeView);
  try
    for I := 0 to LTreeView.TreeViewNodeCount - 1 do
    begin
      LNode := LTreeView.TreeViewNodes[I];
      AProc(LNode, GetDisplayLabelFromNode(LNode, Session.Config.Views));
    end;
  finally
    FreeAndNil(LTreeView);
  end;
  Result := FAddedItems;
end;

function TKExtTreeViewRenderer.RenderAsButtons(
  const ATreeView: TKTreeView; const AContainer: TExtContainer;
  const AOwner: TExtObject;
  const AClickHandler: TExtProcedure): Integer;
begin
  Assert(Assigned(AContainer));

  Result := Render(ATreeView,
    procedure (ANode: TKTreeViewNode; ADisplayLabel: string)
    begin
      AddButton(ANode, ADisplayLabel, AContainer);
    end,
    AOwner, AClickHandler);
end;

function TKExtTreeViewRenderer.RenderAsTree(
  const ATreeView: TKTreeView; const ARoot: TExtTreeTreeNode;
  const AOwner: TExtObject;  const AClickHandler: TExtProcedure): Integer;
begin
  Assert(Assigned(ARoot));

  Result := Render(ATreeView,
    procedure (ANode: TKTreeViewNode; ADisplayLabel: string)
    begin
      AddNode(ANode, ADisplayLabel, ARoot);
    end,
    AOwner, AClickHandler);
end;

function DelphiDateTimeFormatToJSDateTimeFormat(const ADateTimeFormat: string): string;
var
  LFormats: TStringDynArray;
begin
  LFormats := Split(ADateTimeFormat);
  Assert(Length(LFormats) = 2);

  Result := DelphiDateFormatToJSDateFormat(LFormats[0]) + ' ' +
    DelphiTimeFormatToJSTimeFormat(LFormats[1]);
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
  Result := ReplaceText(Result, 'nn', 'i');
  Result := ReplaceText(Result, 'ss', 's');
end;

function AdaptExtNumberFormat(const AFormat: string; const AFormatSettings: TFormatSettings): string;
var
  I: Integer;
begin
  Result := AFormat;
  if AFormatSettings.DecimalSeparator = ',' then
  begin
    for I := 1 to Length(Result) do
    begin
      if Result[I] = '.' then
        Result[I] := ','
      else if Result[I] = ',' then
        Result[I] := '.';
    end;
    Result := Result + '/i';
  end;
end;

{ TKExtTreeTreeNode }

procedure TKExtTreeTreeNode.SetView(const AValue: TKView);
begin
  FView := AValue;
  if Assigned(FView) then
  begin
    Expandable := False;
    Expanded := False;
    Leaf := True;
  end;
end;

{ TKExtButton }

procedure TKExtButton.SetView(const AValue: TKView);
begin
  FView := AValue;
end;

{ TKExtMenuItem }

procedure TKExtMenuItem.SetView(const AValue: TKView);
begin
  FView := AValue;
end;

end.
