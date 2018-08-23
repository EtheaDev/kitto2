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
  EF.Tree
  , Kitto.JS
  , Ext.Base
  , Kitto.Ext.Panel
  ;

type
  TKExtBorderPanelController = class(TKExtPanelControllerBase)
  strict private
    procedure CreateSubControllerForRegion(const ARegion: string);
    function GetRegionName(const ARegion: string): string;
    function GetRegionViewNodeName(const ARegion: string): string;
    function GetRegionControllerNodeName(const ARegion: string): string;
    function FindRegionControllerConfig(const ARegion: string; out AFreeIt: Boolean): TEFNode;
  strict protected
    function GetObjectNamePrefix: string; override;
    function GetRegionDefaultControllerClass(const ARegion: string): string; virtual;
    procedure DoDisplay; override;
  end;

implementation

uses
  SysUtils
  , TypInfo
  , EF.Intf
  , EF.StrUtils
  , Kitto.JS.Controller
  , Kitto.Web.Application
  , Kitto.Metadata.Views
  ;

{ TKExtBorderPanelController }

function TKExtBorderPanelController.GetRegionName(const ARegion: string): string;
begin
  Result := Config.GetString('Sys/RegionPrefix') + ARegion;
end;

function TKExtBorderPanelController.GetRegionViewNodeName(const ARegion: string): string;
begin
  Result := GetRegionName(ARegion) + 'View';
end;

function TKExtBorderPanelController.GetObjectNamePrefix: string;
begin
  Result := 'border';
end;

function TKExtBorderPanelController.GetRegionControllerNodeName(const ARegion: string): string;
begin
  Result := GetRegionName(ARegion) + 'Controller';
end;

function TKExtBorderPanelController.GetRegionDefaultControllerClass(const ARegion: string): string;
begin
  Result := '';
end;

function TKExtBorderPanelController.FindRegionControllerConfig(
  const ARegion: string; out AFreeIt: Boolean): TEFNode;
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

procedure TKExtBorderPanelController.CreateSubControllerForRegion(const ARegion: string);
var
  LSubView: TKView;
  LControllerConfig: TEFNode;
  LFreeIt: Boolean;
  LController: IJSController;
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
      LController := TJSControllerFactory.Instance.CreateController(Self, LSubView, Self, LControllerConfig);
      Assert(LController.AsJSObject is TExtBoxComponent);
      TExtBoxComponent(LController.AsJSObject).Region := ARegion.ToLower;
      LController.Display;
    end;
  finally
    if LFreeIt then
      FreeAndNil(LControllerConfig);
  end;
end;

procedure TKExtBorderPanelController.DoDisplay;
begin
  inherited;
  Layout := 'border';
  CreateSubControllerForRegion('Center');
  CreateSubControllerForRegion('North');
  CreateSubControllerForRegion('South');
  CreateSubControllerForRegion('East');
  CreateSubControllerForRegion('West');
end;

initialization
  TJSControllerRegistry.Instance.RegisterClass('BorderPanel', TKExtBorderPanelController);

finalization
  TJSControllerRegistry.Instance.UnregisterClass('BorderPanel');

end.

