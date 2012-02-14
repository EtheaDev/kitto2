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
  Kitto.Ext.Controller, Kitto.Metadata.Views;

type
  TKExtTreeTreeNode = class(TExtTreeTreeNode)
  private
    FView: TKView;
    procedure SetView(const AValue: TKView);
  public
    property View: TKView read FView write SetView;
    destructor Destroy; override;
  end;

  TKExtButton = class(TExtButton)
  private
    FView: TKView;
    procedure SetView(const AValue: TKView);
  public
    property View: TKView read FView write SetView;
    destructor Destroy; override;
  end;

  TKExtMenuItem = class(TExtMenuItem)
  private
    FView: TKView;
    procedure SetView(const AValue: TKView);
  public
    property View: TKView read FView write SetView;
    destructor Destroy; override;
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
    procedure AddButton(const ANode: TKTreeViewNode; const AContainer: TExtContainer);
    procedure AddMenuItem(const ANode: TKTreeViewNode; const AMenu: TExtMenuMenu);
    procedure AddNode(const ANode: TKTreeViewNode; const AParent: TExtTreeTreeNode);
    function GetClickFunction(const AView: TKView): TExtFunction;

    function FindView(const ANode: TKTreeViewNode): TKView;
  public
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
  end;

function DelphiDateTimeFormatToJSDateTimeFormat(const ADateTimeFormat: string): string;
function DelphiDateFormatToJSDateFormat(const ADateFormat: string): string;
function DelphiTimeFormatToJSTimeFormat(const ATimeFormat: string): string;

///	<summary>Adapts a standard number format string (with , as thousand
///	separator and . as decimal separator) according to the
///	specificed format settings for displaying to the user.</summary>
function AdaptExtNumberFormat(const AFormat: string; const AFormatSettings: TFormatSettings): string;

implementation

uses
  StrUtils, HTTPApp, RTTI,
  EF.SysUtils, EF.Classes, EF.Localization,
  Kitto.Ext.Session, Kitto.AccessControl, Kitto.Ext.Base;

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
    Assert(Session.ViewHost <> nil);
    if Session.StatusHost <> nil then
      Result := FOwner.Ajax(FClickHandler, ['View', Integer(AView), 'AutoCollapseMenu', True,
        'Dummy', Session.StatusHost.ShowBusy])
    else
      Result := FOwner.Ajax(FClickHandler, ['View', Integer(AView), 'AutoCollapseMenu', True]);
  end
  else
    Result := nil;
end;

function TKExtTreeViewRenderer.FindView(const ANode: TKTreeViewNode): TKView;
begin
  if ANode is TKTreeViewFolder then
    Result := nil
  else
    Result := Session.Config.Views.ViewByNode(ANode);
end;

procedure TKExtTreeViewRenderer.AddMenuItem(const ANode: TKTreeViewNode;
  const AMenu: TExtMenuMenu);
var
  I: Integer;
  LMenuItem: TKExtMenuItem;
  LSubMenu: TExtMenuMenu;
  LIsEnabled: Boolean;
  LView: TKView;
begin
  Assert(Assigned(ANode));
  Assert(Assigned(AMenu));

  for I := 0 to ANode.TreeViewNodeCount - 1 do
  begin
    LView := FindView(ANode.TreeViewNodes[I]);

    if not Assigned(LView) or LView.IsAccessGranted(ACM_VIEW) then
    begin
      LIsEnabled := not Assigned(LView) or LView.IsAccessGranted(ACM_RUN);
      LMenuItem := TKExtMenuItem.AddTo(AMenu.Items);
      try
        Inc(FAddedItems);
        LMenuItem.View := LView;
        if Assigned(LMenuItem.View) then
        begin
          LMenuItem.IconCls := Session.SetViewIconStyle(LMenuItem.View, GetImageName(ANode.TreeViewNodes[I], LMenuItem.View));
          LMenuItem.On('click', GetClickFunction(LMenuItem.View));
          LMenuItem.Disabled := not LIsEnabled;
        end
        else
          LMenuItem.Text := HTMLEncode(_(ANode.TreeViewNodes[I].AsString));
        if ANode.TreeViewNodes[I].TreeViewNodeCount > 0 then
        begin
          LSubMenu := TExtMenuMenu.Create;
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
  const AContainer: TExtContainer);
var
  LButton: TKExtButton;
  LMenu: TExtMenuMenu;
  LIsEnabled: Boolean;
  LView: TKView;
begin
  Assert(Assigned(ANode));
  Assert(Assigned(AContainer));

  LView := FindView(ANode);

  if not Assigned(LView) or LView.IsAccessGranted(ACM_VIEW) then
  begin
    LIsEnabled := not Assigned(LView) or LView.IsAccessGranted(ACM_RUN);
    LButton := TKExtButton.AddTo(AContainer.Items);
    try
      Inc(FAddedItems);
      LButton.View := LView;
      if Assigned(LButton.View) then
      begin
        LButton.IconCls := Session.SetViewIconStyle(LButton.View, GetImageName(ANode, LButton.View));
        LButton.On('click', GetClickFunction(LButton.View));
        LButton.Disabled := not LIsEnabled;
      end
      else
        LButton.Text := HTMLEncode(_(ANode.AsString));
      if ANode.ChildCount > 0 then
      begin
        LMenu := TExtMenuMenu.Create;
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
end;

procedure TKExtTreeViewRenderer.AddNode(const ANode: TKTreeViewNode; const AParent: TExtTreeTreeNode);
var
  LNode: TKExtTreeTreeNode;
  I: Integer;
  LIsEnabled: Boolean;
  LView: TKView;
begin
  Assert(Assigned(ANode));
  Assert(Assigned(AParent));

  LView := FindView(ANode);

  if not Assigned(LView) or LView.IsAccessGranted(ACM_VIEW) then
  begin
    LIsEnabled := not Assigned(LView) or LView.IsAccessGranted(ACM_RUN);
    LNode := TKExtTreeTreeNode.Create;
    try
      Inc(FAddedItems);
      LNode.View := LView;
      if Assigned(LNode.View) then
      begin
        LNode.IconCls := Session.SetViewIconStyle(LNode.View, GetImageName(ANode, LNode.View));
        LNode.On('click', GetClickFunction(LNode.View));
        LNode.Disabled := not LIsEnabled;
      end
      else
        LNode.Text := HTMLEncode(_(ANode.AsString));

      if ANode.TreeViewNodeCount > 0 then
      begin
        for I := 0 to ANode.TreeViewNodeCount - 1 do
          AddNode(TKTreeViewNode(ANode.TreeViewNodes[I]), LNode);
        LNode.Expandable := True;
        LNode.Expanded := True;
        LNode.Leaf := False;
      end;
      AParent.AppendChild(LNode);
    except
      FreeAndNil(LNode);
      raise;
    end;
  end;
end;

function TKExtTreeViewRenderer.RenderAsButtons(
  const ATreeView: TKTreeView; const AContainer: TExtContainer;
  const AOwner: TExtObject;
  const AClickHandler: TExtProcedure): Integer;
var
  I: Integer;
begin
  Assert(Assigned(ATreeView));
  Assert(Assigned(AContainer));
  Assert(Assigned(AOwner));
  Assert(Assigned(AClickHandler));

  FOwner := AOwner;
  FClickHandler := AClickHandler;
  FAddedItems := 0;
  for I := 0 to ATreeView.TreeViewNodeCount - 1 do
    AddButton(ATreeView.TreeViewNodes[I], AContainer);
  Result := FAddedItems;
end;

function TKExtTreeViewRenderer.RenderAsTree(
  const ATreeView: TKTreeView; const ARoot: TExtTreeTreeNode;
  const AOwner: TExtObject;  const AClickHandler: TExtProcedure): Integer;
var
  I: Integer;
begin
  Assert(Assigned(ATreeView));
  Assert(Assigned(ARoot));
  Assert(Assigned(AOwner));
  Assert(Assigned(AClickHandler));

  FOwner := AOwner;
  FClickHandler := AClickHandler;
  FAddedItems := 0;
  for I := 0 to ATreeView.TreeViewNodeCount - 1 do
    AddNode(ATreeView.TreeViewNodes[I], ARoot);
  Result := FAddedItems;
end;

function DelphiDateTimeFormatToJSDateTimeFormat(const ADateTimeFormat: string): string;
begin
  Result := DelphiDateFormatToJSDateFormat(ADateTimeFormat);
  Result := DelphiTimeFormatToJSTimeFormat(Result);
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

destructor TKExtTreeTreeNode.Destroy;
begin
  if Assigned(FView) and not FView.IsPersistent then
    FreeAndNil(FView);
  inherited;
end;

procedure TKExtTreeTreeNode.SetView(const AValue: TKView);
var
  LLabel: string;
begin
  FView := AValue;
  if Assigned(FView) then
  begin
    LLabel := FView.DisplayLabel;
    if LLabel = '' then
      LLabel := CallViewControllerStringMethod(FView, 'GetDefaultDisplayLabel', LLabel);
    Text := HTMLEncode(_(LLabel));
    Expandable := False;
    Expanded := False;
    Leaf := True;
  end;
end;

{ TKExtButton }

destructor TKExtButton.Destroy;
begin
  if Assigned(FView) and not FView.IsPersistent then
    FreeAndNil(FView);
  inherited;
end;

procedure TKExtButton.SetView(const AValue: TKView);
var
  LLabel: string;
begin
  FView := AValue;
  if Assigned(FView) then
  begin
    LLabel := FView.DisplayLabel;
    if LLabel = '' then
      LLabel := CallViewControllerStringMethod(FView, 'GetDefaultDisplayLabel', LLabel);
    Text := HTMLEncode(_(LLabel));
  end;
end;

{ TKExtMenuItem }

destructor TKExtMenuItem.Destroy;
begin
  if Assigned(FView) and not FView.IsPersistent then
    FreeAndNil(FView);
  inherited;
end;

procedure TKExtMenuItem.SetView(const AValue: TKView);
var
  LLabel: string;
begin
  FView := AValue;
  if Assigned(FView) then
  begin
    LLabel := FView.DisplayLabel;
    if LLabel = '' then
      LLabel := CallViewControllerStringMethod(FView, 'GetDefaultDisplayLabel', LLabel);
    Text := HTMLEncode(_(LLabel));
  end;
end;

end.
