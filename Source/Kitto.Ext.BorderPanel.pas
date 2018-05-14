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

unit Kitto.Ext.BorderPanel;

{$I Kitto.Defines.inc}

interface

uses
  SysUtils,
  Ext.Base,
  EF.Tree,
  Kitto.Ext.Base, Kitto.Ext.Controller;

type
  TKExtBorderPanelController = class;

  TKExtBorderPanelControllerClass = class of TKExtBorderPanelController;

  TKExtBorderPanelController = class(TKExtPanelControllerBase)
  strict private
    FControllers: array[TExtBoxComponentRegion] of TObject;
    procedure CreateController(const ARegion: TExtBoxComponentRegion);
  strict protected
    function FindRegionControllerConfig(
      const ARegion: TExtBoxComponentRegion; out AFreeIt: Boolean): TEFNode; virtual;
    function GetObjectNamePrefix: string; override;
  protected
    procedure DoDisplay; override;
    function GetRegionDefaultControllerClass(const ARegion: TExtBoxComponentRegion): string; virtual;
    function GetRegionName(const ARegion: TExtBoxComponentRegion): string; virtual;
    function GetRegionViewNodeName(const ARegion: TExtBoxComponentRegion): string;
    function GetRegionControllerNodeName(const ARegion: TExtBoxComponentRegion): string;
  public
  end;

implementation

uses
  TypInfo
  , EF.Intf
  , EF.StrUtils
  , Kitto.JS
  , Kitto.Web.Application
  , Kitto.Metadata.Views
  ;

{ TKExtBorderPanelController }

function TKExtBorderPanelController.GetRegionName(const ARegion: TExtBoxComponentRegion): string;
begin
  Result := StripPrefix(GetEnumName(TypeInfo(TExtBoxComponentRegion), Ord(ARegion)), 'rg');
end;

function TKExtBorderPanelController.GetRegionViewNodeName(const ARegion: TExtBoxComponentRegion): string;
begin
  Result := GetRegionName(ARegion) + 'View';
end;

function TKExtBorderPanelController.GetObjectNamePrefix: string;
begin
  Result := 'border';
end;

function TKExtBorderPanelController.GetRegionControllerNodeName(const ARegion: TExtBoxComponentRegion): string;
begin
  Result := GetRegionName(ARegion) + 'Controller';
end;

function TKExtBorderPanelController.GetRegionDefaultControllerClass(const ARegion: TExtBoxComponentRegion): string;
begin
  Result := '';
end;

function TKExtBorderPanelController.FindRegionControllerConfig(
  const ARegion: TExtBoxComponentRegion; out AFreeIt: Boolean): TEFNode;
var
  LRegionControllerNodeName: string;
  LRegionDefaultControllerClass: string;
begin
  AFreeIt := False;
  LRegionControllerNodeName := GetRegionControllerNodeName(ARegion);
  Assert(LRegionControllerNodeName <> '');
  LRegionDefaultControllerClass := GetRegionDefaultControllerClass(ARegion);
  Result := Config.FindNode(LRegionControllerNodeName);
  if not Assigned(Result) then
  begin
    if LRegionDefaultControllerClass <> '' then
    begin
      Result := TEFNode.Create(LRegionControllerNodeName, LRegionDefaultControllerClass);
      AFreeIt := True;
    end;
  end
  else
  begin
    if Result.Value = '' then
      Result.Value := LRegionDefaultControllerClass;
  end;
end;

procedure TKExtBorderPanelController.CreateController(const ARegion: TExtBoxComponentRegion);
var
  LSubView: TKView;
  LControllerConfig: TEFNode;
  LIntf: IJSController;
  LFreeIt: Boolean;
begin
  Assert(Assigned(View));

  // If subcontrollers are specified, they inherit this controller's view.
  // If no subcontroller is configured for a given region, look for a subview.
  LControllerConfig := FindRegionControllerConfig(ARegion, LFreeIt);
  try
    if Assigned(LControllerConfig) then
    begin
      LSubView := View;
    end
    else
      LSubView := TKWebApplication.Current.Config.Views.FindViewByNode(Config.FindNode(GetRegionViewNodeName(ARegion)));
    if LSubView <> nil then
    begin
      FControllers[ARegion] := TKExtControllerFactory.Instance.CreateController(Self, LSubView, Self, LControllerConfig).AsObject;
      Assert(FControllers[ARegion] is TExtBoxComponent);
      TExtBoxComponent(FControllers[ARegion]).Region := ARegion;
      if Supports(FControllers[ARegion], IJSController, LIntf) then
        InitSubController(LIntf);
      LIntf.Display;
    end;
  finally
    if LFreeIt then
      FreeAndNil(LControllerConfig);
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

