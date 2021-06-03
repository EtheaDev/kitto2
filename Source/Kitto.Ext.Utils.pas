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

unit Kitto.Ext.Utils;

{$I Kitto.Defines.inc}

interface

uses
  SysUtils
  , Classes
  , Ext.Base
  , Ext.Menu
  , EF.ObserverIntf
  , EF.Tree
  , Kitto.JS.Types
  , Kitto.Ext.Base
  , Kitto.JS.Controller
  , Kitto.Metadata.Views
  ;

type
  TKExtViewButton = class(TKExtButton)
  private
    FView: TKView;
    procedure SetView(const AValue: TKView);
  public
    property View: TKView read FView write SetView;
  end;

  TKExtViewMenuItem = class(TExtMenuItem)
  private
    FView: TKView;
    procedure SetView(const AValue: TKView);
  public
    property View: TKView read FView write SetView;
  end;

  /// <summary>
  ///  Renders a tree view on a container in various ways: as a set of buttons
  ///  with submenus, an Ext treeview control, etc.
  /// </summary>
  TKExtTreeViewRenderer = class
  private
    FOwner: TExtObject;
    FClickHandler: TJSProcedure;
    FTreeView: TKTreeView;
    procedure AddButton(const ANode: TKTreeViewNode; const ADisplayLabel: string; const AContainer: TExtContainer);
    procedure AddMenuItem(const ANode: TKTreeViewNode; const AMenu: TExtMenuMenu);
    function GetClickFunction(const AView: TKView): TExtExpression;

    /// <summary>
    ///  Clones the specified tree view, filters all invisible items
    ///  (including folders containing no visible items) and returns the
    ///  clone.
    /// </summary>
    /// <remarks>
    ///  The caller is responsible for freeing the returned object.
    /// </remarks>
    function CloneAndFilter(const ATreeView: TKTreeView): TKTreeView;
    procedure Filter(const ANode: TKTreeViewNode);
  public
    /// <summary>
    ///  Attaches to the container a set of buttons, one for each top-level
    ///  element of the specified tree view. Each button has a submenu tree
    ///  with the child views. Returns the total number of effectively added
    ///  items.
    /// </summary>
    { TODO : move elsewhere, like in Kitto.Ext.ToolBar }
    procedure RenderAsButtons(const ATreeView: TKTreeView;
      const AContainer: TExtContainer; const AOwner: TExtObject;
      const AClickHandler: TJSProcedure);

    /// <summary>
    ///  Renders a tree view by calling AProc for each top-level element in the tree view.
    /// </summary>
    procedure Render(const ATreeView: TKTreeView; const AProc: TProc<TKTreeViewNode, string>;
      const AOwner: TExtObject; const AClickHandler: TJSProcedure);
  end;

function GetTreeViewNodeImageName(const ANode: TKTreeViewNode; const AView: TKView): string;

/// <summary>
///  Computes and returns a display label based on the underlying view,
///  if any, or the node itself (if no view is found).
/// </summary>
function GetDisplayLabelFromNode(const ANode: TKTreeViewNode; const AViews: TKViews): string;

/// <summary>
///  Invoke a method of a View that return a string using RTTI
/// </summary>
function CallViewControllerStringMethod(const AView: TKView;
  const AMethodName: string; const ADefaultValue: string): string;

procedure DownloadThumbnailedStream(const AStream: TStream; const AFileName: string;
  const AThumbnailWidth, AThumbnailHeight: Integer);

implementation

uses
  Types
  , StrUtils
  , Rtti
  , NetEncoding
  {$IFDEF MSWINDOWS}
  , Vcl.Graphics
  , Vcl.Imaging.jpeg
  , Vcl.Imaging.pngimage
  {$ENDIF}
  , EF.Sys
  , EF.StrUtils
  , EF.Classes
  , EF.Localization
  , EF.Macros
  , Kitto.Web.Session
  , Kitto.Web.Application
  , Kitto.Web.Response
  , Kitto.Auth
  , Kitto.AccessControl
  , Kitto.Config;

function CallViewControllerStringMethod(const AView: TKView;
  const AMethodName: string; const ADefaultValue: string): string;
var
  LControllerClass: TClass;
  LContext: TRttiContext;
  LMethod: TRttiMethod;
  LType: string;
begin
  Assert(Assigned(AView));
  Assert(AMethodName <> '');

  Result := ADefaultValue;

  LType := AView.ControllerType;
  if LType <> '' then
  begin
    LControllerClass := TJSControllerRegistry.Instance.FindClass(LType);
    if Assigned(LControllerClass) then
    begin
      LMethod := LContext.GetType(LControllerClass).GetMethod(AMethodName);
      if Assigned(LMethod) then
        Result := LMethod.Invoke(LControllerClass, []).AsString;
    end;
  end;
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
  begin
    Result := _(ANode.AsString);
    TEFMacroExpansionEngine.Instance.Expand(Result);
  end;
end;

function GetTreeViewNodeImageName(const ANode: TKTreeViewNode; const AView: TKView): string;
begin
  Assert(Assigned(ANode));
  Assert(Assigned(AView));

  Result := ANode.GetString('ImageName');
  if Result = '' then
    Result := CallViewControllerStringMethod(AView, 'GetDefaultImageName', '');
end;

{ TKExtTreeViewRenderer }

function TKExtTreeViewRenderer.GetClickFunction(const AView: TKView): TExtExpression;
begin
  Assert(Assigned(FOwner));
  Assert(Assigned(FClickHandler));

  if Assigned(AView) then
  begin
    if TKWebSession.Current.StatusHost <> nil then
      //Result := FOwner.Ajax(FClickHandler, ['View', Integer(AView), 'Dummy', TKWebSession.Current.StatusHost.ShowBusy])
      Result := TKWebResponse.Current.Items.AjaxCallMethod(FOwner).SetMethod(FClickHandler)
        .AddParam('View', Integer(AView))
        .AddParam('Dummy', TKWebSession.Current.StatusHost.ShowBusy)
        .AsFunction
    else
      //Result := FOwner.Ajax(FClickHandler, ['View', Integer(AView)]);
      Result := TKWebResponse.Current.Items.AjaxCallMethod(FOwner).SetMethod(FClickHandler)
        .AddParam('View', Integer(AView))
        .AsFunction;
  end
  else
    Result := nil;
end;

procedure TKExtTreeViewRenderer.AddMenuItem(const ANode: TKTreeViewNode;
  const AMenu: TExtMenuMenu);
var
  I: Integer;
  LMenuItem: TKExtViewMenuItem;
  LSubMenu: TExtMenuMenu;
  LIsEnabled: Boolean;
  LView: TKView;
  LDisplayLabel: string;
  LNode: TKTreeViewNode;
begin
  Assert(Assigned(ANode));
  Assert(Assigned(AMenu));

  for I := 0 to ANode.TreeViewNodeCount - 1 do
  begin
    LNode := ANode.TreeViewNodes[I];
    LView := LNode.FindView(TKWebApplication.Current.Config.Views);

    if Assigned(LView) then
      LIsEnabled := LView.IsAccessGranted(ACM_RUN)
    else
      LIsEnabled := TKAccessController.Current.IsAccessGranted(
        TKAuthenticator.Current.UserName, ANode.GetACURI(FTreeView), ACM_RUN);

    LMenuItem := TKExtViewMenuItem.CreateAndAddToArray(AMenu.Items);
    try
      LMenuItem.Disabled := not LIsEnabled;
      LMenuItem.View := LView;
      if Assigned(LMenuItem.View) then
      begin
        LMenuItem.IconCls := TKWebApplication.Current.SetViewIconStyle(LMenuItem.View,
          GetTreeViewNodeImageName(LNode, LMenuItem.View));
        LMenuItem.On('click', GetClickFunction(LMenuItem.View));

        LDisplayLabel := _(LNode.GetString('DisplayLabel', LMenuItem.View.DisplayLabel));
        if LDisplayLabel = '' then
          LDisplayLabel := CallViewControllerStringMethod(LView, 'GetDefaultDisplayLabel', '');
        LMenuItem.Text := TNetEncoding.HTML.Encode(LDisplayLabel);
        // No tooltip here - could be done through javascript if needed.
      end
      else
      begin
        if ANode.TreeViewNodes[I].TreeViewNodeCount > 0 then
        begin
          LDisplayLabel := _(LNode.GetString('DisplayLabel', LNode.AsString));
          LMenuItem.Text := TNetEncoding.HTML.Encode(LDisplayLabel);
          LMenuItem.IconCls := TKWebApplication.Current.SetIconStyle('Folder', LNode.GetString('ImageName'));
          LSubMenu := TExtMenuMenu.Create(AMenu.Items);
          LMenuItem.Menu := LSubMenu;
          AddMenuItem(ANode.TreeViewNodes[I], LSubMenu);
        end;
      end;
    except
      FreeAndNil(LMenuItem);
      raise;
    end;
  end;
end;

procedure TKExtTreeViewRenderer.AddButton(const ANode: TKTreeViewNode;
  const ADisplayLabel: string; const AContainer: TExtContainer);
var
  LButton: TKExtViewButton;
  LMenu: TExtMenuMenu;
  LIsEnabled: Boolean;
  LView: TKView;
  LImageName: string;
begin
  Assert(Assigned(ANode));
  Assert(Assigned(AContainer));

  LView := ANode.FindView(TKWebApplication.Current.Config.Views);
  if Assigned(LView) then
    LIsEnabled := LView.IsAccessGranted(ACM_RUN)
  else
    LIsEnabled := TKAccessController.Current.IsAccessGranted(
      TKAuthenticator.Current.UserName, ANode.GetACURI(FTreeView), ACM_RUN);

  LButton := TKExtViewButton.CreateAndAddToArray(AContainer.Items);
  try
    LButton.View := LView;
    if Assigned(LButton.View) then
    begin
      //Button for View
      LButton.IconCls := TKWebApplication.Current.SetViewIconStyle(LButton.View, GetTreeViewNodeImageName(ANode, LButton.View));
      LButton.On('click', GetClickFunction(LButton.View));
      LButton.Disabled := not LIsEnabled;
    end
    else
    begin
      //Folder
      LImageName := ANode.GetString('ImageName');
      if LImageName <> '' then
        LButton.SetIconAndScale(LImageName, 'small');
    end;
    LButton.Text := TNetEncoding.HTML.Encode(ADisplayLabel);
    if TKWebApplication.Current.TooltipsEnabled then
      LButton.Tooltip := LButton.Text;

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

function TKExtTreeViewRenderer.CloneAndFilter(const ATreeView: TKTreeView): TKTreeView;
var
  I: Integer;
begin
  Assert(Assigned(ATreeView));

  Result := TKTreeView.Clone(ATreeView,
    procedure (const ASource, ADestination: TEFNode)
    begin
      ADestination.SetObject('Sys/SourceNode', ASource);
    end
  );

  for I := Result.TreeViewNodeCount - 1 downto 0 do
    Filter(Result.TreeViewNodes[I]);
end;

procedure TKExtTreeViewRenderer.Filter(const ANode: TKTreeViewNode);
var
  LView: TKView;
  I: Integer;
  LIsVisible: Boolean;
begin
  Assert(Assigned(ANode));

  LView := ANode.FindView(TKWebApplication.Current.Config.Views);
  if Assigned(LView) then
    LIsVisible := LView.IsAccessGranted(ACM_VIEW)
  else
    LIsVisible := TKAccessController.Current.IsAccessGranted(
      TKAuthenticator.Current.UserName, ANode.GetACURI(FTreeView), ACM_VIEW);

  if not LIsVisible then
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

procedure TKExtTreeViewRenderer.Render(const ATreeView: TKTreeView;
  const AProc: TProc<TKTreeViewNode, string>; const AOwner: TExtObject;
  const AClickHandler: TJSProcedure);
var
  I: Integer;
  LNode: TKTreeViewNode;
  LTreeView: TKTreeView;
begin
  Assert(Assigned(ATreeView));
  Assert(Assigned(AProc));
  Assert(Assigned(AOwner));

  FOwner := AOwner;
  FTreeView := ATreeView;
  FClickHandler := AClickHandler;

  LTreeView := CloneAndFilter(ATreeView);
  try
    for I := 0 to LTreeView.TreeViewNodeCount - 1 do
    begin
      LNode := LTreeView.TreeViewNodes[I];
      AProc(LNode, GetDisplayLabelFromNode(LNode, TKWebApplication.Current.Config.Views));
    end;
  finally
    FreeAndNil(LTreeView);
  end;
end;

procedure TKExtTreeViewRenderer.RenderAsButtons(
  const ATreeView: TKTreeView; const AContainer: TExtContainer;
  const AOwner: TExtObject;
  const AClickHandler: TJSProcedure);
begin
  Assert(Assigned(AContainer));

  Render(ATreeView,
    procedure (ANode: TKTreeViewNode; ADisplayLabel: string)
    begin
      AddButton(ANode, ADisplayLabel, AContainer);
    end,
    AOwner, AClickHandler);
end;

{ TKExtViewButton }

procedure TKExtViewButton.SetView(const AValue: TKView);
begin
  FView := AValue;
end;

{ TKExtViewMenuItem }

procedure TKExtViewMenuItem.SetView(const AValue: TKView);
begin
  FView := AValue;
end;

procedure DownloadThumbnailedStream(const AStream: TStream; const AFileName: string;
  const AThumbnailWidth, AThumbnailHeight: Integer);
{$IFDEF MSWINDOWS}
{ Paradox graphic BLOB header }

type
  TPDoxGraphicHeader = record
    Count: Word;                { Fixed at 1 }
    HType: Word;                { Fixed at $0100 }
    Size: Integer              { Size not including header }
  end;

var
  LFileExt: string;
  LBytes: TBytes;
  Size: Longint;
  Header: TBytes;
  GraphicHeader: TPDoxGraphicHeader;

  function CreateThumbnail(const AMaxWidth, AMaxHeight: Integer;
    const AImageClass: TGraphicClass): TBytes;
  var
    LImage: TGraphic;
    LScale: Extended;
    LBitmap: TBitmap;

    function GetImageBytes: TBytes;
    var
      LStream: TBytesStream;
    begin
      LStream := TBytesStream.Create;
      try
        LImage.SaveToStream(LStream);
        Result := Copy(LStream.Bytes, 0, LStream.Size);
      finally
        FreeAndNil(LStream);
      end;
    end;

  begin
    LImage := AImageClass.Create;
    try
      Size := AStream.Size;
      if Size >= SizeOf(TPDoxGraphicHeader) then
      begin
        SetLength(Header, SizeOf(TPDoxGraphicHeader));
        AStream.Read(Header, 0, Length(Header));
        Move(Header[0], GraphicHeader, SizeOf(TPDoxGraphicHeader));
        if (GraphicHeader.Count <> 1) or (GraphicHeader.HType <> $0100) or
          (GraphicHeader.Size <> Size - SizeOf(GraphicHeader)) then
          AStream.Position := 0;
      end;
      LImage.LoadFromStream(AStream);
      if (LImage.Height <= AMaxHeight) and (LImage.Width <= AMaxWidth) then
        Exit(GetImageBytes);

      if LImage.Height > LImage.Width then
        LScale := AMaxHeight / LImage.Height
      else
        LScale := AMaxWidth / LImage.Width;
      LBitmap := TBitmap.Create;
      try
        LBitmap.Width := Round(LImage.Width * LScale);
        LBitmap.Height := Round(LImage.Height * LScale);
        LBitmap.Canvas.StretchDraw(LBitmap.Canvas.ClipRect, LImage);

        LImage.Assign(LBitmap);

        Exit(GetImageBytes);
      finally
        LBitmap.Free;
      end;
    finally
      LImage.Free;
    end;
  end;
{$ENDIF}

begin
  Assert(Assigned(AStream));

  {$IFDEF MSWINDOWS}
  LFileExt := ExtractFileExt(AFileName);
  if MatchText(LFileExt, ['.jpg', '.jpeg', '.png']) then
  begin
    try
      if MatchText(LFileExt, ['.jpg', '.jpeg']) then
        LBytes := CreateThumbnail(AThumbnailWidth, AThumbnailHeight, TJPEGImage)
      else
        LBytes := CreateThumbnail(AThumbnailWidth, AThumbnailHeight, TPngImage);
    finally
      AStream.Free;
    end;
    TKWebApplication.Current.DownloadBytes(LBytes, AFileName);
  end
  else if MatchText(LFileExt, ['.bmp']) then
  begin
    try
      LBytes := CreateThumbnail(AThumbnailWidth, AThumbnailHeight, TBitmap);
    finally
      AStream.Free;
    end;
    TKWebApplication.Current.DownloadBytes(LBytes, AFileName);
  end
  else
  {$ENDIF}
    TKWebApplication.Current.DownloadStream(AStream, AFileName);
end;

end.
