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

unit Kitto.Ext.BorderPanel;

{$I Kitto.Defines.inc}

interface

uses
  SysUtils,
  Ext,
  EF.Tree,
  Kitto.Ext.Base, Kitto.Ext.Controller;

type
  TKExtBorderPanelController = class(TKExtPanelControllerBase)
  private
    FControllers: array[TExtBoxComponentRegion] of TObject;
    function GetRegionViewNodeName(const ARegion: TExtBoxComponentRegion): string;
    function GetRegionControllerNodeName(const ARegion: TExtBoxComponentRegion): string;
    procedure CreateController(const ARegion: TExtBoxComponentRegion);
  protected
    function GetRegionControllerName(const ARegion: TExtBoxComponentRegion): string; virtual;
    function GetRegionControllerConfig(const ARegion: TExtBoxComponentRegion): TEFNode; virtual;
    procedure DoDisplay; override;
  end;

implementation

uses
  TypInfo,
  EF.Intf, EF.StrUtils,
  Kitto.Ext.Session, Kitto.Metadata.Views;

{ TKExtBorderPanelController }

function TKExtBorderPanelController.GetRegionViewNodeName(const ARegion: TExtBoxComponentRegion): string;
begin
  Result := StripPrefix(GetEnumName(TypeInfo(TExtBoxComponentRegion), Ord(ARegion)), 'rg') + 'View';
end;

function TKExtBorderPanelController.GetRegionControllerNodeName(const ARegion: TExtBoxComponentRegion): string;
begin
  Result := StripPrefix(GetEnumName(TypeInfo(TExtBoxComponentRegion), Ord(ARegion)), 'rg') + 'Controller';
end;

function TKExtBorderPanelController.GetRegionControllerConfig(
  const ARegion: TExtBoxComponentRegion): TEFNode;
begin
  Result := Config.GetNode(GetRegionControllerNodeName(ARegion));
end;

function TKExtBorderPanelController.GetRegionControllerName(const ARegion: TExtBoxComponentRegion): string;
begin
  Result := Config.GetString(GetRegionControllerNodeName(ARegion));
end;

procedure TKExtBorderPanelController.CreateController(const ARegion: TExtBoxComponentRegion);
var
  LSubView: TKView;
  LSubControllerName: string;
  LControllerConfig: TEFNode;
  LIntf: IKExtController;
begin
  Assert(Assigned(View));

  LControllerConfig := nil;
  // If subcontrollers are specified, they inherit this controller's view.
  // If no subcontroller is configured for a given region, look for a subview.
  LSubControllerName := GetRegionControllerName(ARegion);
  if LSubControllerName <> '' then
  begin
    LSubView := View;
    LControllerConfig := GetRegionControllerConfig(ARegion);
  end
  else
    LSubView := Session.Config.Views.FindViewByNode(Config.FindNode(GetRegionViewNodeName(ARegion)));
  if LSubView <> nil then
  begin
    FControllers[ARegion] := TKExtControllerFactory.Instance.CreateController(LSubView, Self, LControllerConfig).AsObject;
    Assert(FControllers[ARegion] is TExtBoxComponent);
    TExtBoxComponent(FControllers[ARegion]).Region := ARegion;
    if Supports(FControllers[ARegion], IKExtController, LIntf) then
      InitSubController(LIntf);
    LIntf.Display;
  end;
end;

procedure TKExtBorderPanelController.DoDisplay;
var
  I: TExtBoxComponentRegion;
begin
  inherited;
  Layout := lyBorder;
  for I := Low(FControllers) to High(FControllers) do
    CreateController(I);
end;

initialization
  TKExtControllerRegistry.Instance.RegisterClass('BorderPanel', TKExtBorderPanelController);

finalization
  TKExtControllerRegistry.Instance.UnregisterClass('BorderPanel');

end.

