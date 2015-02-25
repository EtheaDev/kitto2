{-------------------------------------------------------------------------------
   Copyright 2013 Ethea S.r.l.

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
  EF.Tree,
  Kitto.Metadata.Views,
  Kitto.Ext.Base, Kitto.Ext.Controller, Kitto.Ext.Session, Kitto.Ext.TabPanel;

type
  // A tile page to be added to a container.
  TKExtTilePanel = class(TKExtPanelBase)
  private
    const
      FColors: array[0..9] of string = (
        // Metro colors.
        '#A200FF', '#FF0097', '#00ABA9', '#8CBF26', '#A05000',
        '#E671B8', '#F09609', '#1BA1E2', '#E51400', '#339933');
    procedure AddBreak;
    procedure AddTitle(const ADisplayLabel: string);
    function GetTileHeight: Integer;
    function GetTileWidth: Integer;
    var
      FView: TKView;
      FConfig: TEFTree;
      FTileBoxHtml: string;
      FColorIndex: Integer;
      FMaxTilesPerFolder: Integer;
      FTileRows: Integer;
    procedure BuildTileBoxHtml;
    function GetBoxStyle(const ATilesPerRow, ARows: Integer): string;
    procedure AddTile(const ANode: TKTreeViewNode; const ADisplayLabel: string);
    procedure AddTiles(const ANode: TKTreeViewNode; const ADisplayLabel: string);
    function GetNextTileColor: string;
  public
    property Config: TEFTree read FConfig write FConfig;
    property View: TKView read FView write FView;
    procedure DoDisplay;
  published
    procedure DisplayView;
  end;

  // Hosted by the tile panel controller; manages the additional tile page.
  TKExtTileTabPanel = class(TKExtTabPanel)
  private
    FTilePanel: TKExtTilePanel;
    procedure AddTileSubPanel;
  strict protected
    function TabsVisible: Boolean; override;
  public
    procedure SetAsViewHost; override;
    procedure DisplaySubViewsAndControllers; override;
  end;

  // A tab panel controller with a tile menu on the first page.
  TKExtTilePanelController = class(TKExtTabPanelController)
  strict protected
    function GetTabPanelClass: TKExtTabPanelClass; override;
    function GetDefaultTabIconsVisible: Boolean; override;
  published
    procedure DisplayPage;
  end;

implementation

uses
  SysUtils, StrUtils,
  Ext,
  EF.StrUtils, EF.Macros, EF.Localization,
  Kitto.Config, Kitto.Utils, Kitto.Ext.Utils;

{ TKExtTilePanelController }

procedure TKExtTilePanelController.DisplayPage;
begin
  { TODO : To be called when the tile is a multi-level tile.
    Should change the html to reflect the specified page or sub-page,
    perhaps using animation. As an alternative, produce all pages at once
    and animate among them. }
end;

function TKExtTilePanelController.GetDefaultTabIconsVisible: Boolean;
begin
  Result := False;
end;

function TKExtTilePanelController.GetTabPanelClass: TKExtTabPanelClass;
begin
  Result := TKExtTileTabPanel;
end;

{ TKExtTileTabPanel }

procedure TKExtTileTabPanel.DisplaySubViewsAndControllers;
begin
  AddTileSubPanel;
  inherited;
  if Items.Count > 0 then
    SetActiveTab(0);
end;

procedure TKExtTileTabPanel.SetAsViewHost;
begin
  // Don't act as view host on mobile - we want modal views there.
  if not Session.IsMobileBrowser then
    inherited;
end;

function TKExtTileTabPanel.TabsVisible: Boolean;
begin
  Result := Config.GetBoolean('TabsVisible', not Session.IsMobileBrowser);
end;

procedure TKExtTileTabPanel.AddTileSubPanel;
begin
  inherited;
  FTilePanel := TKExtTilePanel.CreateAndAddTo(Items);
  FTilePanel.View := View;
  FTilePanel.Config := Config;
  FTilePanel.DoDisplay;
end;

{ TKExtTilePanel }

procedure TKExtTilePanel.DoDisplay;
var
  LTitle: string;
begin
  LTitle := _(Config.GetExpandedString('Title', View.DisplayLabel));
  if LTitle <> '' then
    Title := LTitle
  else
    Title := _('Home');
  BuildTileBoxHtml;
end;

procedure TKExtTilePanel.DisplayView;
begin
  Session.DisplayView(TKView(Session.QueryAsInteger['View']));
end;

function TKExtTilePanel.GetBoxStyle(const ATilesPerRow, ARows: Integer): string;
begin
//  LWidth := (ATilesPerRow * GetTileWidth) + Succ(ATilesPerRow) * 2 * GetBorderWidth;
//  LHeight := (ARows * GetTileHeight) + Succ(ATilesPerRow) * 2 * GetBorderWidth;
  { TODO : add space for folders/titles }
 //Result := Format('width:%dpx;height:%dpx;', [LWidth, LHeight]);
 Result := '';
end;

function TKExtTilePanel.GetNextTileColor: string;
begin
  Result := FColors[FColorIndex];
  Inc(FColorIndex);
  if FColorIndex > High(FColors) then
    FColorIndex := Low(FColors);
end;

function TKExtTilePanel.GetTileHeight: Integer;
begin
  Result := Config.GetInteger('TileHeight', 50);
end;

function TKExtTilePanel.GetTileWidth: Integer;
begin
  Result := Config.GetInteger('TileWidth', 100);
end;

procedure TKExtTilePanel.AddTiles(const ANode: TKTreeViewNode; const ADisplayLabel: string);
var
  I: Integer;
  LSubNode: TKTreeViewNode;
  LDisplayLabelNode: TEFNode;
  LDisplayLabel: string;
begin
  FTileBoxHtml := FTileBoxHtml + '<div class="k-tile-row">';
  if ANode is TKTreeViewFolder then
  begin
    AddTitle(ADisplayLabel);
    for I := 0 to ANode.TreeViewNodeCount - 1 do
    begin
      LSubNode := ANode.TreeViewNodes[I];
      LDisplayLabelNode := LSubNode.FindNode('DisplayLabel');
      if Assigned(LDisplayLabelNode) then
        LDisplayLabel := _(LDisplayLabelNode.AsString)
      else
        LDisplayLabel := GetDisplayLabelFromNode(LSubNode, Session.Config.Views);
      AddTile(LSubNode, LDisplayLabel);
    end;
    if FMaxTilesPerFolder < ANode.TreeViewNodeCount then
      FMaxTilesPerFolder := ANode.TreeViewNodeCount;
    Inc(FTileRows);
  end
  else
    AddTile(ANode, ADisplayLabel);
  AddBreak;
  FTileBoxHtml := FTileBoxHtml + '</div>';
end;

procedure TKExtTilePanel.AddBreak;
begin
  FTileBoxHtml := FTileBoxHtml + '<br style="clear:left;" />';
end;

procedure TKExtTilePanel.AddTitle(const ADisplayLabel: string);
begin
  FTileBoxHtml := FTileBoxHtml + Format(
    '<div class="k-tile-title-row">%s</div>',
    [HTMLEncode(ADisplayLabel)]);
end;

procedure TKExtTilePanel.AddTile(const ANode: TKTreeViewNode; const ADisplayLabel: string);
var
  LClickCode: string;

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
    if ANode.GetBoolean('HideLabel', False) then
      Result := ''
    else
      Result := HTMLEncode(ADisplayLabel);
  end;

  function GetColor: string;
  var
    LColor: TEFNode;
  begin
    LColor := ANode.FindNode('BackgroundColor');
    if Assigned(LColor) then
      Result := LColor.AsString
    else
      Result := GetNextTileColor;
  end;

begin
  Assert(not (ANode is TKTreeViewFolder));

  LClickCode := JSMethod(Ajax(DisplayView, ['View', Integer(Session.Config.Views.ViewByNode(ANode))]));
  if GetCSS <> '' then
  begin
    FTileBoxHtml := FTileBoxHtml + Format(
      '<a href="#" onclick="%s"><div class="k-tile%s">' +
      '<div class="k-tile-inner">%s</div></div></a>',
      [HTMLEncode(LClickCode), GetCSS, GetDisplayLabel]);
  end
  else
  begin
    FTileBoxHtml := FTileBoxHtml + Format(
      '<a href="#" onclick="%s"><div class="k-tile" style="background-color:%s;width:%dpx;height:%dpx">' +
      '<div class="k-tile-inner">%s</div></div></a>',
      [HTMLEncode(LClickCode), GetColor, GetTileWidth, GetTileHeight, GetDisplayLabel]);
  end;
end;

procedure TKExtTilePanel.BuildTileBoxHtml;
var
  LTreeViewRenderer: TKExtTreeViewRenderer;
  LNode: TEFNode;
  LTreeView: TKTreeView;
  LFileName: string;
begin
  FColorIndex :=  Low(FColors);
  FTileBoxHtml := '<div class="k-tile-box" style="%s">';

  LTreeViewRenderer := TKExtTreeViewRenderer.Create;
  try
    LTreeViewRenderer.Session := Session;
    LNode := Config.GetNode('TreeView');
    LTreeView := Session.Config.Views.ViewByNode(LNode) as TKTreeView;
    FMaxTilesPerFolder := 0;
    FTileRows := 0;
    LTreeViewRenderer.Render(LTreeView,
      procedure (ANode: TKTreeViewNode; ADisplayLabel: string)
      begin
        AddTiles(ANode, ADisplayLabel);
      end,
      Self, DisplayView);
    FTileBoxHtml := FTileBoxHtml + '</div></div>';
    FTileBoxHtml := Format(FTileBoxHtml, [GetBoxStyle(FMaxTilesPerFolder, FTileRows)]);
  (*
  <div class="k-tile-row">
    <a href="#" onclick="alert('hello');"><div class="k-tile" style="background-color:#A200FF">Dolls</div></a>
    <div class="k-tile" style="background-color:#FF0097">Girls</div>
    <div class="k-tile" style="background-color:#A05000">Parties</div>
    <br style="clear:left;" />
  </div>
  <div class="k-tile_row">
    <div class="k-tile" style="background-color:#00ABA9">Tile</div>
    <div class="k-tile" style="background-color:#8CBF26">Tile</div>
    <div class="k-tile" style="background-color:#E671B8">Tile</div>
    <br style="clear:left;" />
  </div>
</div>
  *)
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
  TKExtControllerRegistry.Instance.RegisterClass('TilePanel', TKExtTilePanelController);

finalization
  TKExtControllerRegistry.Instance.UnregisterClass('TilePanel');

end.
