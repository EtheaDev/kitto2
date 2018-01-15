{-------------------------------------------------------------------------------
   Copyright 2012-2018 Ethea S.r.l.

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
unit KIDE.BorderPanelControllerDesignerFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.UITypes,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, KIDE.EditNodeBaseFrameUnit,
  Vcl.Graphics, Vcl.ExtCtrls, Vcl.Tabs, KITTO.Ext.BorderPanel,
  System.Actions, Vcl.ActnList, Vcl.ComCtrls, Vcl.ToolWin, Vcl.StdCtrls, Vcl.Buttons,
  Ext.Base,
  KIDE.BaseFrameUnit, KIDE.CodeEditorFrameUnit, KIDE.PanelControllerDesignerFrameUnit,
  EF.Tree, KIDE.ControllerDesignerFrameUnit, Vcl.Samples.Spin;

type
  TBorderPanelControllerDesignerFrame = class(TPanelControllerDesignerFrame)
    ControllerPageControl: TPageControl;
    SubControllersTabSheet: TTabSheet;
    SubControllersPageControl: TPageControl;
    CenterControllerTabSheet: TTabSheet;
    NorthControllerTabSheet: TTabSheet;
    EastControllerTabSheet: TTabSheet;
    SouthControllerTabSheet: TTabSheet;
    WestControllerTabSheet: TTabSheet;
    SubViewsTabSheet: TTabSheet;
    SubViewsPageControl: TPageControl;
    CenterViewTabSheet: TTabSheet;
    NorthViewTabSheet: TTabSheet;
    EastViewTabSheet: TTabSheet;
    SouthViewTabSheet: TTabSheet;
    WestViewTabSheet: TTabSheet;
    ControllersGroupBox: TGroupBox;
    NorthControllerButton: TSpeedButton;
    WestControllerButton: TSpeedButton;
    EastControllerButton: TSpeedButton;
    SouthControllerButton: TSpeedButton;
    CenterControllerButton: TSpeedButton;
    ViewsGroupBox: TGroupBox;
    NorthViewButton: TSpeedButton;
    WestViewButton: TSpeedButton;
    EastViewButton: TSpeedButton;
    SouthViewButton: TSpeedButton;
    CenterViewButton: TSpeedButton;
    procedure SpeedButtonClick(Sender: TObject);
  strict private
    function GetRegionViewName(
      const ARegion: TExtBoxComponentRegion): string;
    function GetRegionControllerName(
      const ARegion: TExtBoxComponentRegion): string;
    function FindBorderPanelControllerClass(const ANode: TEFNode): TKExtBorderPanelControllerClass;
    procedure UpdateControllerGUI(const ARegion: TExtBoxComponentRegion;
      const AButton: TSpeedButton; const ATabSheet: TTabSheet);
    procedure UpdateViewGUI(const ARegion: TExtBoxComponentRegion;
      const AButton: TSpeedButton; const ATabSheet: TTabSheet);
    procedure GUIToViewNode(const ARegion: TExtBoxComponentRegion;
      const AButton: TSpeedButton);
    procedure GUIToControllerNode(const ARegion: TExtBoxComponentRegion;
      const AButton: TSpeedButton);
    protected
    class function SuitsNode(const ANode: TEFNode): Boolean; override;
    procedure UpdateDesignPanel(const AForce: Boolean = False); override;
    procedure CleanupDefaultsToEditNode; override;
    procedure DesignPanelToEditNode; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Init(const ANode: TEFTree); override;
  end;

implementation

{$R *.dfm}

uses
  EF.Macros, Kitto.Metadata.Views, KIDE.Utils,
  Kitto.Ext.Controller, Kitto.Ext.Base;

{ TDownloadFileToolDesignerFrame }

procedure TBorderPanelControllerDesignerFrame.CleanupDefaultsToEditNode;
begin
  inherited;
  Assert(Assigned(EditNode));
  //Clean SubControllers nodes
  CleanupOrphanNode(GetRegionControllerName(rgCenter));
  CleanupOrphanNode(GetRegionControllerName(rgNorth));
  CleanupOrphanNode(GetRegionControllerName(rgWest));
  CleanupOrphanNode(GetRegionControllerName(rgEast));
  CleanupOrphanNode(GetRegionControllerName(rgSouth));

  //Clean SubViews nodes
  CleanupOrphanNode(GetRegionViewName(rgCenter));
  CleanupOrphanNode(GetRegionViewName(rgNorth));
  CleanupOrphanNode(GetRegionViewName(rgWest));
  CleanupOrphanNode(GetRegionViewName(rgEast));
  CleanupOrphanNode(GetRegionViewName(rgSouth));

end;

constructor TBorderPanelControllerDesignerFrame.Create(AOwner: TComponent);
begin
  inherited;
end;

procedure TBorderPanelControllerDesignerFrame.Init(const ANode: TEFTree);
var
  LBorderPanelControllerClass: TKExtBorderPanelControllerClass;
(*
  procedure InitController(const ANodeName: string; const ATabSheet: TTabSheet);
  var
    LNode: TEFNode;
    LFrameClass: TEditNodeBaseFrameClass;
  begin
    LNode := ANode.FindNode(ANodeName);
    if Assigned(LNode) then
    begin
      LFrameClass := TEditNodeFrameFractory.Instance.GetEditNodeFrameClass(LNode);
      if Assigned(LFrameClass) then
        EmbedEditNodeFrame(ATabSheet, LFrameClass, LNode);
    end;
  end;
*)
begin
  inherited;
  Assert(ANode is TEFNode);
  LBorderPanelControllerClass := FindBorderPanelControllerClass(TEFNode(ANode));
(*
  //Init SubControllers
  InitController(LBorderPanelControllerClass.GetRegionControllerNodeName(rgCenter), CenterControllerTabSheet);
  InitController(LBorderPanelControllerClass.GetRegionControllerNodeName(rgNorth) , NorthControllerTabSheet);
  InitController(LBorderPanelControllerClass.GetRegionControllerNodeName(rgWest)  , WestControllerTabSheet);
  InitController(LBorderPanelControllerClass.GetRegionControllerNodeName(rgEast)  , EastControllerTabSheet);
  InitController(LBorderPanelControllerClass.GetRegionControllerNodeName(rgSouth) , SouthControllerTabSheet);

  //Init SubViews
  InitController(LBorderPanelControllerClass.GetRegionViewNodeName(rgCenter), CenterViewTabSheet);
  InitController(LBorderPanelControllerClass.GetRegionViewNodeName(rgNorth) , NorthViewTabSheet);
  InitController(LBorderPanelControllerClass.GetRegionViewNodeName(rgWest)  , WestViewTabSheet);
  InitController(LBorderPanelControllerClass.GetRegionViewNodeName(rgEast)  , EastViewTabSheet);
  InitController(LBorderPanelControllerClass.GetRegionViewNodeName(rgSouth) , SouthViewTabSheet);
*)
end;

function TBorderPanelControllerDesignerFrame.FindBorderPanelControllerClass(
  const ANode: TEFNode): TKExtBorderPanelControllerClass;
var
  LControllerClass: TClass;
begin
  LControllerClass := GetControllerClass(ANode);
  if Assigned(LControllerClass) then
    Result := TKExtBorderPanelControllerClass(LControllerClass)
  else
    Result := nil;
end;

function TBorderPanelControllerDesignerFrame.GetRegionViewName(
  const ARegion: TExtBoxComponentRegion): string;
var
  LBorderPanelControllerClass: TKExtBorderPanelControllerClass;
begin
  LBorderPanelControllerClass := FindBorderPanelControllerClass(TEFNode(EditNode));
  if Assigned(LBorderPanelControllerClass) then
    Result := GetRegionName(LBorderPanelControllerClass.ClassName, ARegion) + 'View'
  else
    Result := '';
end;

function TBorderPanelControllerDesignerFrame.GetRegionControllerName(
  const ARegion: TExtBoxComponentRegion): string;
var
  LBorderPanelControllerClass: TKExtBorderPanelControllerClass;
begin
  LBorderPanelControllerClass := FindBorderPanelControllerClass(EditNode as TEFNode);
  if Assigned(LBorderPanelControllerClass) then
    Result := GetRegionName(LBorderPanelControllerClass.ClassName, ARegion) + 'Controller'
  else
    Result := '';
end;

procedure TBorderPanelControllerDesignerFrame.GUIToViewNode(const ARegion: TExtBoxComponentRegion;
  const AButton: TSpeedButton);
var
  LNodeName: string;
begin
  LNodeName := GetRegionViewName(ARegion);
  if (LNodeName <> '') and not AButton.Down then
    EditNode.DeleteNode(LNodeName)
  else
    EditNode.SetString(LNodeName,'');
end;

procedure TBorderPanelControllerDesignerFrame.GUIToControllerNode(const ARegion: TExtBoxComponentRegion;
  const AButton: TSpeedButton);
var
  LNodeName: string;
begin
  LNodeName := GetRegionControllerName(ARegion);
  if (LNodeName <> '') and not AButton.Down then
    EditNode.DeleteNode(LNodeName);
end;


procedure TBorderPanelControllerDesignerFrame.DesignPanelToEditNode;
begin
  inherited;
  ControllerPageControl.ActivePageIndex := 0;

  GUIToViewNode(rgNorth, NorthViewButton);
  GUIToViewNode(rgWest, WestViewButton);
  GUIToViewNode(rgCenter, CenterViewButton);
  GUIToViewNode(rgEast, EastViewButton);
  GUIToViewNode(rgSouth, SouthViewButton);

  GUIToControllerNode(rgNorth, NorthControllerButton);
  GUIToControllerNode(rgWest, WestControllerButton);
  GUIToControllerNode(rgCenter, CenterControllerButton);
  GUIToControllerNode(rgEast, EastControllerButton);
  GUIToControllerNode(rgSouth, SouthControllerButton);
end;

procedure TBorderPanelControllerDesignerFrame.SpeedButtonClick(Sender: TObject);
var
  LSpeedButton: TSpeedButton;
  LNodeName: string;
begin
  inherited;
  LSpeedButton := Sender as TSpeedButton;
  if      LSpeedButton = NorthControllerButton  then LNodeName := GetRegionControllerName(rgNorth)
  else if LSpeedButton = WestControllerButton   then LNodeName := GetRegionControllerName(rgWest)
  else if LSpeedButton = EastControllerButton   then LNodeName := GetRegionControllerName(rgEast)
  else if LSpeedButton = SouthControllerButton  then LNodeName := GetRegionControllerName(rgSouth)
  else if LSpeedButton = CenterControllerButton then LNodeName := GetRegionControllerName(rgCenter)
  else if LSpeedButton = NorthViewButton        then LNodeName := GetRegionViewName(rgNorth)
  else if LSpeedButton = WestViewButton         then LNodeName := GetRegionViewName(rgWest)
  else if LSpeedButton = EastViewButton         then LNodeName := GetRegionViewName(rgEast)
  else if LSpeedButton = SouthViewButton        then LNodeName := GetRegionViewName(rgSouth)
  else if LSpeedButton = CenterViewButton       then LNodeName := GetRegionViewName(rgCenter)
  else LNodeName := '';
  if LNodeName <> '' then
  begin
    if LSpeedButton.Down then
      EditNode.GetNode(LNodeName, True)
    else
      EditNode.DeleteNode(LNodeName);
    UpdateDesignPanel(False);
  end;
end;

class function TBorderPanelControllerDesignerFrame.SuitsNode(
  const ANode: TEFNode): Boolean;
var
  LControllerClass: TClass;
begin
  Assert(Assigned(ANode));
  LControllerClass := GetControllerClass(ANode);
  Result := Assigned(LControllerClass) and
    LControllerClass.InheritsFrom(TKExtBorderPanelController);
end;

procedure TBorderPanelControllerDesignerFrame.UpdateControllerGUI(const ARegion: TExtBoxComponentRegion;
  const AButton: TSpeedButton; const ATabSheet: TTabSheet);
var
  LNodeName: string;
begin
  LNodeName := GetRegionControllerName(ARegion);
  if (LNodeName <> '') and (EditNode.FindNode(LNodeName) <> nil) then
  begin
    AButton.Down := True;
    AButton.Font.Style := AButton.Font.Style + [fsBold];
//    ATabSheet.Tabvisible := True;
  end
  else
  begin
    AButton.Down := False;
    AButton.ParentFont := True;
//    ATabSheet.Tabvisible := False;
  end;
end;

procedure TBorderPanelControllerDesignerFrame.UpdateViewGUI(const ARegion: TExtBoxComponentRegion;
  const AButton: TSpeedButton; const ATabSheet: TTabSheet);
var
  LNodeName: string;
begin
  LNodeName := GetRegionViewName(ARegion);
  if (LNodeName <> '') and (EditNode.FindNode(LNodeName) <> nil) then
  begin
    AButton.Down := True;
//    ATabSheet.TabVisible := True;
  end
  else
  begin
    AButton.Down := False;
//    ATabSheet.TabVisible := False;
  end;
end;

procedure TBorderPanelControllerDesignerFrame.UpdateDesignPanel(
  const AForce: Boolean);
begin
  Assert(Assigned(EditNode));
  inherited;
  ControllerPageControl.ActivePageIndex := 0;
  //Update SubView Buttons and Pages
  UpdateViewGUI(rgNorth,  NorthViewButton,  NorthViewTabSheet );
  UpdateViewGUI(rgWest,   WestViewButton,   WestViewTabSheet  );
  UpdateViewGUI(rgCenter, CenterViewButton, CenterViewTabSheet);
  UpdateViewGUI(rgEast,   EastViewButton,   EastViewTabSheet  );
  UpdateViewGUI(rgSouth,  SouthViewButton,  SouthViewTabSheet );
  SubViewsTabSheet.TabVisible :=
    CenterViewTabSheet.TabVisible or
    WestViewTabSheet.TabVisible or
    NorthViewTabSheet.TabVisible or
    EastViewTabSheet.TabVisible or
    SouthViewTabSheet.TabVisible;

  //Update SubController Buttons and Pages
  UpdateControllerGUI(rgNorth,  NorthControllerButton,  NorthControllerTabSheet );
  UpdateControllerGUI(rgWest,   WestControllerButton,   WestControllerTabSheet  );
  UpdateControllerGUI(rgCenter, CenterControllerButton, CenterControllerTabSheet);
  UpdateControllerGUI(rgEast,   EastControllerButton,   EastControllerTabSheet  );
  UpdateControllerGUI(rgSouth,  SouthControllerButton,  SouthControllerTabSheet );
  SubControllersTabSheet.TabVisible :=
    CenterControllerTabSheet.TabVisible or
    WestControllerTabSheet.TabVisible or
    NorthControllerTabSheet.TabVisible or
    EastControllerTabSheet.TabVisible or
    SouthControllerTabSheet.TabVisible;
end;

initialization
  TEditNodeFrameRegistry.Instance.RegisterClass(TBorderPanelControllerDesignerFrame.GetClassId, TBorderPanelControllerDesignerFrame);

finalization
  TEditNodeFrameRegistry.Instance.UnregisterClass(TBorderPanelControllerDesignerFrame.GetClassId);

end.
