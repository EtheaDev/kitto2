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
  Ext,
  Kitto.Ext.Base, Kitto.Ext.Controller;

type
  TKExtBorderPanelController = class(TKExtPanelControllerBase)
  private
    FControllers: array[TExtBoxComponentRegion] of IKExtController;
    procedure CreateController(const ARegion: TExtBoxComponentRegion);
    function GetRegionViewName(const ARegion: TExtBoxComponentRegion): string;
  protected
    procedure DoDisplay; override;
  public
    destructor Destroy; override;
  end;

implementation

uses
  TypInfo,
  EF.Intf, EF.StrUtils,
  Kitto.Ext.Session, Kitto.Metadata.Views;

{ TKExtBorderPanelController }

function TKExtBorderPanelController.GetRegionViewName(const ARegion: TExtBoxComponentRegion): string;
begin
  Result := StripPrefix(GetEnumName(TypeInfo(TExtBoxComponentRegion), Ord(ARegion)), 'rg') + 'View';
end;

procedure TKExtBorderPanelController.CreateController(const ARegion: TExtBoxComponentRegion);
var
  LSubView: TKView;
begin
  Assert(Assigned(View));


  LSubView := Session.Config.Views.FindViewByNode(View.FindNode('Controller/' + GetRegionViewName(ARegion)));
  if LSubView <> nil then
  begin
    FControllers[ARegion] := TKExtControllerFactory.Instance.CreateController(LSubView, Self);
    Assert(FControllers[ARegion].AsObject is TExtBoxComponent);
    TExtBoxComponent(FControllers[ARegion].AsObject).Region := ARegion;
    FControllers[ARegion].Display;
  end;
end;

destructor TKExtBorderPanelController.Destroy;
var
  I: TExtBoxComponentRegion;
begin
  // Prevent the compiler from calling _Release.
  for I := Low(FControllers) to High(FControllers) do
    Pointer(FControllers[I]) := nil;
  inherited;
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

