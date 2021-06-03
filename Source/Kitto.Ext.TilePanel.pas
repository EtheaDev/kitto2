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

unit Kitto.Ext.TilePanel;

{$I Kitto.Defines.inc}

interface

uses
  Types
  , EF.Tree
  , Kitto.Metadata.Views
  , Kitto.Ext.Base
  , Kitto.JS.Controller
  , Kitto.Ext.TabPanel
  ;

const
  DEFAULT_TILE_HEIGHT = 50;
  DEFAULT_TILE_WIDTH = 100;

type
  /// <sumnmary>
  ///  A tile page to be added to a container.
  /// </sumnmary>
  TKExtTilePage = class(TKExtPanelBase)
  strict private
    FView: TKView;
    FTileBoxHtml: string;
    FColors: TStringDynArray;
    FColorIndex: Integer;
    FRootNode: TKTreeViewNode;
    procedure AddBreak;
    procedure AddTitle(const ADisplayLabel: string);
    function GetTileHeight: Integer;
    function GetTileWidth: Integer;
    function GetShowImage: Boolean;
    function GetImagePosition: string;
    procedure BuildTileBoxHtml(const ARootNode: TKTreeViewNode = nil);
    procedure AddTile(const ANode: TKTreeViewNode; const ADisplayLabel: string;
      AIsBack: Boolean = False);
    procedure AddTiles(const ANode: TKTreeViewNode; const ADisplayLabel: string);
    procedure AddBackTile;
    function GetNextTileColor: string;
    function GetColors(const AColorSetName: string): TStringDynArray;
  public
    const DEFAULT_COLOR_SET = 'Metro';
    property View: TKView read FView write FView;
    procedure DoDisplay;
  //published
    procedure DisplayView;
    procedure DisplayPage;
  end;

  /// <sumnmary>
  ///  A tab panel hosted by the tile panel controller; manages the additional tile tab.
  /// </sumnmary>
  TKExtTileTabPanel = class(TKExtTabPanel)
  strict private
    FTilePage: TKExtTilePage;
    procedure DisplayTilePage;
  strict protected
    function TabsVisible: Boolean; override;
  public
    procedure SetAsControllerContainer; override;
    procedure DisplaySubViewsAsTabs; override;
  end;

  /// <sumnmary>
  ///  A tab panel controller with a tile menu on the first page.
  /// </sumnmary>
  TKExtTilePanelController = class(TKExtTabPanelController)
  strict protected
    function GetTabPanelClass: TKExtTabPanelClass; override;
    function GetDefaultTabIconsVisible: Boolean; override;
  end;

implementation

uses
  SysUtils
  , StrUtils
  , NetEncoding
  , Ext.Base
  , EF.StrUtils
  , EF.Macros
  , EF.Localization
  , Kitto.JS
  , Kitto.Config
  , Kitto.Web.Application
  , Kitto.Web.Request
  , Kitto.Web.Response
  , Kitto.Ext.Utils
  ;

{ TKExtTilePanelController }

function TKExtTilePanelController.GetDefaultTabIconsVisible: Boolean;
begin
  Result := False;
end;

function TKExtTilePanelController.GetTabPanelClass: TKExtTabPanelClass;
begin
  Result := TKExtTileTabPanel;
end;

{ TKExtTileTabPanel }

procedure TKExtTileTabPanel.DisplaySubViewsAsTabs;
begin
  DisplayTilePage;
  inherited;
end;

procedure TKExtTileTabPanel.SetAsControllerContainer;
begin
  // Don't act as view host on mobile - we want modal views there.
  if not TKWebRequest.Current.IsMobileBrowser then
    inherited;
end;

function TKExtTileTabPanel.TabsVisible: Boolean;
begin
  Result := Config.GetBoolean('TabsVisible', not TKWebRequest.Current.IsMobileBrowser);
end;

procedure TKExtTileTabPanel.DisplayTilePage;
begin
  inherited;
  FTilePage := TKExtTilePage.CreateAndAddToArray(Items);
  FTilePage.View := View;
  FTilePage.Config.Assign(Config);
  FTilePage.DoDisplay;
end;

{ TKExtTilePanel }

procedure TKExtTilePage.DoDisplay;
var
  LTitle: string;
begin
  LTitle := _(Config.GetExpandedString('Title', View.DisplayLabel));
  if LTitle <> '' then
    Title := LTitle
  else
    Title := _('Home');
  AutoScroll := True;

  FColors := GetColors(Config.GetExpandedString('ColorSet', DEFAULT_COLOR_SET));
  BuildTileBoxHtml;
end;

procedure TKExtTilePage.DisplayPage;
begin
  BuildTileBoxHtml(TKTreeViewNode(ParamAsInteger('PageId')));
end;

procedure TKExtTilePage.DisplayView;
begin
  TKWebApplication.Current.DisplayView(TKView(ParamAsInteger('View')));
end;

function TKExtTilePage.GetColors(const AColorSetName: string): TStringDynArray;
begin
  if SameText(AColorSetName, 'Metro') then
  begin
    SetLength(Result, 10);
    Result[0] := '#00904A';
    Result[1] := '#FF0097';
    Result[2] := '#00ABA9';
    Result[3] := '#8CBF26';
    Result[4] := '#A05000';
    Result[5] := '#E671B8';
    Result[6] := '#F09609';
    Result[7] := '#1BA1E2';
    Result[8] := '#E51400';
    Result[9] := '#339933';
  end
  else if SameText(AColorSetName, 'Blue') then
  begin
    SetLength(Result, 5);
    Result[0] := '#1240AB';
    Result[1] := '#365BB0';
    Result[2] := '#5777C0';
    Result[3] := '#0D3184';
    Result[4] := '#082568';
  end
  else if SameText(AColorSetName, 'Red') then
  begin
    SetLength(Result, 5);
    Result[0] := '#FF0000';
    Result[1] := '#FF3939';
    Result[2] := '#FF6363';
    Result[3] := '#C50000';
    Result[4] := '#9B0000';
  end
  else if SameText(AColorSetName, 'Gold') then
  begin
    SetLength(Result, 5);
    Result[0] := '#FFD300';
    Result[1] := '#FFDD39';
    Result[2] := '#FFE463';
    Result[3] := '#C5A300';
    Result[4] := '#9B8000';
  end
  else if SameText(AColorSetName, 'Violet') then
  begin
    SetLength(Result, 5);
    Result[0] := '#3914AF';
    Result[1] := '#5538B4';
    Result[2] := '#735AC3';
    Result[3] := '#2B0E88';
    Result[4] := '#20096A';
  end
  else
  begin
    SetLength(Result, 1);
    Result[0] := '#000000';
  end;
end;

function TKExtTilePage.GetImagePosition: string;
begin
  Result := Config.GetString('ShowImage/Position', '');
end;

function TKExtTilePage.GetShowImage: Boolean;
begin
  Result := Config.GetBoolean('ShowImage', False);
end;

function TKExtTilePage.GetNextTileColor: string;
begin
  Result := FColors[FColorIndex];
  Inc(FColorIndex);
  if FColorIndex > High(FColors) then
    FColorIndex := Low(FColors);
end;

function TKExtTilePage.GetTileHeight: Integer;
begin
  Result := Config.GetInteger('TileHeight', DEFAULT_TILE_HEIGHT);
end;

function TKExtTilePage.GetTileWidth: Integer;
begin
  Result := Config.GetInteger('TileWidth', DEFAULT_TILE_WIDTH);
end;

procedure TKExtTilePage.AddTiles(const ANode: TKTreeViewNode; const ADisplayLabel: string);
var
  LOriginalNode: TKTreeViewNode;

  function IsNodeIncluded(const ACurrentNode: TEFNode): Boolean;
  begin
    Result := (FRootNode = nil) or FRootNode.HasChild(ACurrentNode, True);
  end;

  procedure ProcessChildren(const AParentNode: TKTreeViewNode);
  var
    LDisplayLabelNode: TEFNode;
    LDisplayLabel: string;
    I: Integer;
    LSubNode: TKTreeViewNode;
    LOriginalSubNode: TKTreeViewNode;
    LAddedTileCount: Integer;
  begin
    LAddedTileCount := 0;
    for I := 0 to AParentNode.TreeViewNodeCount - 1 do
    begin
      LSubNode := AParentNode.TreeViewNodes[I];
      LOriginalSubNode := TKTreeViewNode(LSubNode.GetObject('Sys/SourceNode'));
      if IsNodeIncluded(LOriginalSubNode) then
      begin
        LDisplayLabelNode := LOriginalSubNode.FindNode('DisplayLabel');
        if Assigned(LDisplayLabelNode) then
          LDisplayLabel := _(LDisplayLabelNode.AsString)
        else
          LDisplayLabel := GetDisplayLabelFromNode(LOriginalSubNode, TKWebApplication.Current.Config.Views);
        // Render folders as rows not tiles in second level.
        if not (LOriginalSubNode is TKTreeViewFolder) or not Assigned(FRootNode) then
        begin
          AddTile(LOriginalSubNode, LDisplayLabel);
          Inc(LAddedTileCount);
        end;
      end;
      // Only when not rendering the first level, look ahead for children.
      if (LSubNode is TKTreeViewFolder) and Assigned(FRootNode) then
        ProcessChildren(LSubNode);
    end;
    if LAddedTileCount > 0 then
      AddBreak;
  end;

begin
  LOriginalNode := TKTreeViewNode(ANode.GetObject('Sys/SourceNode'));
  FTileBoxHtml := FTileBoxHtml + '<div class="k-tile-row">';
  if ANode is TKTreeViewFolder then
  begin
    // Render folders as rows not tiles in second level.
    if IsNodeIncluded(LOriginalNode) and not Assigned(FRootNode) then
      AddTitle(ADisplayLabel);
    ProcessChildren(ANode);
  end
  else if IsNodeIncluded(LOriginalNode) then
    AddTile(LOriginalNode, ADisplayLabel);
  FTileBoxHtml := FTileBoxHtml + '</div>';
end;

procedure TKExtTilePage.AddBackTile;
var
  LClickCode: string;
begin
  LClickCode := GetJSCode(
    procedure
    begin
      TKWebResponse.Current.Items.AjaxCallMethod(Self).SetMethod(DisplayPage).AddParam('PageId', 0);
    end);
  FTileBoxHtml := FTileBoxHtml + Format(
    '<a href="#" onclick="%s"><div class="k-tile k-tile-back" style="background-color:%s;width:%dpx;height:%dpx">' +
    '<div class="k-tile-inner k-tile-back-inner">%s</div></div></a>',
    [TNetEncoding.HTML.Encode(LClickCode), GetNextTileColor, GetTileWidth, GetTileHeight, _('Back')]);
end;

procedure TKExtTilePage.AddBreak;
begin
  FTileBoxHtml := FTileBoxHtml + '<br style="clear:left;" />';
end;

procedure TKExtTilePage.AddTitle(const ADisplayLabel: string);
begin
  if ADisplayLabel <> '' then
    FTileBoxHtml := FTileBoxHtml + Format(
      '<div class="k-tile-title-row">%s</div>',
      [TNetEncoding.HTML.Encode(ADisplayLabel)]);
end;

procedure TKExtTilePage.AddTile(const ANode: TKTreeViewNode; const ADisplayLabel: string;
  AIsBack: Boolean = False);
var
  LClickCode: string;
  LCustomStyle: string;
  LTileBoxHTML: string;
  LImageName, LImageURL: string;
  LView: TKView;
  LShowImage: boolean;
  LPageId: Integer;
  LBackGroundColor: string;

  function GetCSS: string;
  var
    LCSS: string;
  begin
    LCSS := ANode.GetString('CSS');
    if LCSS <> '' then
      Result := ' ' + LCSS
    else
      Result := '';
  end;

  function GetDisplayLabel: string;
  begin
    if not ANode.GetBoolean('HideLabel') then
      Result := TNetEncoding.HTML.Encode(ADisplayLabel)
    else
      Result := '';
  end;

  procedure AddAttributeToStyle(var AStyle: string; const AStileAttributeName, AFormatStr: string;
    const ADefault: string = '');
  var
    LNode: TEFNode;
    LNodeStr: string;
  begin
    LNode := ANode.FindNode(AStileAttributeName);
    if Assigned(LNode) then
      LNodeStr := LNode.AsString
    else if ADefault <> '' then
      LNodeStr := ADefault
    else
      Exit;
    AStyle := Trim(AStyle + ' ' + Format(AFormatStr, [LNodeStr]));
  end;

begin
  if ANode is TKTreeViewFolder then
  begin
    LView := nil;
    if AIsBack then
      LPageId := 0
    else
      LPageId := Integer(ANode);

    LClickCode := GetJSCode(
      procedure
      begin
        TKWebResponse.Current.Items.AjaxCallMethod(Self).SetMethod(DisplayPage).AddParam('PageId', LPageId);
      end);
  end
  else
  begin
    LView := TKWebApplication.Current.Config.Views.ViewByNode(ANode);
    LClickCode := GetJSCode(
      procedure
      begin
        TKWebResponse.Current.Items.AjaxCallMethod(Self).SetMethod(DisplayView)
          .AddParam('View', Integer(LView));
      end);
  end;

  if AIsBack then
  begin
    LImageName := ANode.GetString('Back/ImageName');
    if (LImageName = '') and GetShowImage then
      LImageName := 'back';
    LBackGroundColor := ANode.GetString('Back/BackgroundColor', GetNextTileColor);
  end
  else
  begin
    LImageName := ANode.GetString('ImageName');
    LBackGroundColor := GetNextTileColor;
  end;

  //Build background css style
  LCustomStyle := 'background-repeat: no-repeat;';
  AddAttributeToStyle(LCustomStyle, 'BackgroundColor', 'background-color: %s;', LBackGroundColor);
  AddAttributeToStyle(LCustomStyle, 'Width', 'width:%spx;', IntToStr(GetTileWidth));
  AddAttributeToStyle(LCustomStyle, 'Height', 'height:%spx;', IntToStr(GetTileHeight));

  LShowImage := GetShowImage or (LImageName <> '');
  if LShowImage then
  begin
    if (LImageName = '') and Assigned(LView) then
    begin
      LImageName := CallViewControllerStringMethod(LView, 'GetDefaultImageName', LView.ImageName);
    end;
    if LImageName <> '' then
    begin
      AddAttributeToStyle(LCustomStyle, 'ImagePosition', 'background-position: %s;', GetImagePosition);
      LImageURL := TKWebApplication.Current.FindImageURL(SmartConcat(LImageName, '_', 'large'));
      if LImageURL = '' then
        LImageURL := TKWebApplication.Current.FindImageURL(LImageName);
      if LImageURL <> '' then
        LCustomStyle := LCustomStyle + Format('background-image: url(&quot;%s&quot);', [LImageURL]);
    end;
  end;
  AddAttributeToStyle(LCustomStyle, 'Style', '%s;');

  if GetCSS <> '' then
  begin
    LTileBoxHTML := Format(
      '<a href="#" onclick="%s"><div class="k-tile%s" style="%s">' +
      '<div class="k-tile-inner">%s</div></div></a>',
      [TNetEncoding.HTML.Encode(LClickCode), GetCSS, LCustomStyle, GetDisplayLabel]);
  end
  else
  begin
    LTileBoxHTML := Format(
      '<a href="#" onclick="%s"><div class="k-tile" style="%s">' +
      '<div class="k-tile-inner">%s</div></div></a>',
      [TNetEncoding.HTML.Encode(LClickCode), LCustomStyle, GetDisplayLabel]);
  end;

  FTileBoxHtml := FTileBoxHtml + LTileBoxHTML;
end;

procedure TKExtTilePage.BuildTileBoxHtml(const ARootNode: TKTreeViewNode);
var
  LTreeViewRenderer: TKExtTreeViewRenderer;
  LNode: TEFNode;
  LTreeView: TKTreeView;
  LFileName: string;
begin
  FColorIndex :=  0;
  FTileBoxHtml := '<div class="k-tile-box">';
  FRootNode := ARootNode;
  if Assigned(FRootNode) then
  begin
    AddBackTile;
    if FRootNode.GetBoolean('Back/AddBreak',True) then
      AddBreak;
  end;

  LTreeViewRenderer := TKExtTreeViewRenderer.Create;
  try
    LNode := Config.GetNode('TreeView');
    LTreeView := TKWebApplication.Current.Config.Views.ViewByNode(LNode) as TKTreeView;
    LTreeViewRenderer.Render(LTreeView,
      procedure (ANode: TKTreeViewNode; ADisplayLabel: string)
      begin
        AddTiles(ANode, ADisplayLabel);
      end,
      Self, DisplayView);
    FTileBoxHtml := FTileBoxHtml + '</div></div>';

    LFileName := ChangeFileExt(ExtractFileName(LTreeView.PersistentFileName), '.html');
    LoadHtml(LFileName,
      function (AHtml: string): string
      begin
        if AHtml <> '' then
          Result := ReplaceText(AHtml, '{content}', FTileBoxHtml)
        else
          Result := FTileBoxHtml;
      end);
  finally
    FreeAndNil(LTreeViewRenderer);
  end;
end;

initialization
  TJSControllerRegistry.Instance.RegisterClass('TilePanel', TKExtTilePanelController);

finalization
  TJSControllerRegistry.Instance.UnregisterClass('TilePanel');

end.
