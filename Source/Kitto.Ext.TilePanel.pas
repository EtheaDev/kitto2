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
    var
      FView: TKView;
      FConfig: TEFTree;
      FTileBoxHtml: string;
      FColorIndex: Integer;
    procedure BuildTileBoxHtml;
    function GetBoxStyle: string;
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
  public
    procedure DisplaySubViewsAndControllers; override;
  end;

  // A tab panel controller with a tile menu on the first page.
  TKExtTilePanelController = class(TKExtTabPanelController)
  strict protected
    function GetTabPanelClass: TKExtTabPanelClass; override;
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

function TKExtTilePanelController.GetTabPanelClass: TKExtTabPanelClass;
begin
  Result := TKExtTileTabPanel;
end;

{ TKExtTileTabPanel }

procedure TKExtTileTabPanel.DisplaySubViewsAndControllers;
begin
  AddTileSubPanel;
  inherited;
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

function TKExtTilePanel.GetBoxStyle: string;
//var
//  LWidth: Integer;
//  LHeight: Integer;
begin
{ TODO :
The box style should set the width, height and margins so that
the div is horizontally and vertically centered.

width: w
height: h
margins: -h/2 0 0 -w/2


 }
// LWidth := 320;
// LHeight := 170;
// Result := Format('width:%d;height:%d;margins:-%dpx 0 0 %dpx;',
//   [LWidth, LHeight, LHeight div 2, LWidth div 2]);
 Result := '';
end;

function TKExtTilePanel.GetNextTileColor: string;
begin
  Result := FColors[FColorIndex];
  Inc(FColorIndex);
  if FColorIndex > High(FColors) then
    FColorIndex := Low(FColors);
end;

procedure TKExtTilePanel.AddTiles(const ANode: TKTreeViewNode; const ADisplayLabel: string);
var
  I: Integer;
  LSubNode: TKTreeViewNode;
begin
  FTileBoxHtml := FTileBoxHtml + '<div class="k-tile-row">';
  if ANode is TKTreeViewFolder then
  begin
    AddTitle(ADisplayLabel);
    for I := 0 to ANode.TreeViewNodeCount - 1 do
    begin
      LSubNode := ANode.TreeViewNodes[I];
      AddTile(LSubNode, GetDisplayLabelFromNode(LSubNode, Session.Config.Views));
    end;
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
  LColor: TEFNode;
  LColorValue: string;
begin
  Assert(not (ANode is TKTreeViewFolder));

  LClickCode := JSMethod(Ajax(DisplayView, ['View', Integer(Session.Config.Views.ViewByNode(ANode))]));
  LColor := ANode.FindNode('BackgroundColor');
  if Assigned(LColor) then
    LColorValue := LColor.AsString
  else
    LColorValue := GetNextTileColor;
  FTileBoxHtml := FTileBoxHtml + Format(
    '<a href="#" onclick="%s"><div class="k-tile" style="background-color:%s">' +
    '<div class="k-tile-inner">%s</div></div></a>',
    [HTMLEncode(LClickCode), LColorValue, HTMLEncode(ADisplayLabel)]);
end;

procedure TKExtTilePanel.BuildTileBoxHtml;
var
  LTreeViewRenderer: TKExtTreeViewRenderer;
  LNode: TEFNode;
  LTreeView: TKTreeView;
begin
  FColorIndex :=  Low(FColors);
  FTileBoxHtml := '<div class="k-tile-box" style="' + GetBoxStyle + '">';

  LTreeViewRenderer := TKExtTreeViewRenderer.Create;
  try
    LTreeViewRenderer.Session := Session;
    LNode := Config.GetNode('TreeView');
    LTreeView := Session.Config.Views.ViewByNode(LNode) as TKTreeView;
    LTreeViewRenderer.Render(LTreeView,
      procedure (ANode: TKTreeViewNode; ADisplayLabel: string)
      begin
        AddTiles(ANode, ADisplayLabel);
      end,
      Self, DisplayView);
    FTileBoxHtml := FTileBoxHtml + '</div></div>';
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
  finally
    FreeAndNil(LTreeViewRenderer);
  end;
  //LTemplate := Session.GetPageTemplate('tilepanel');
  Html := FTileBoxHtml;
end;

initialization
  TKExtControllerRegistry.Instance.RegisterClass('TilePanel', TKExtTilePanelController);

finalization
  TKExtControllerRegistry.Instance.UnregisterClass('TilePanel');

end.
