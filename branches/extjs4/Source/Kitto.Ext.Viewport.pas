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

unit Kitto.Ext.Viewport;

{$I Kitto.Defines.inc}

interface

uses
  Kitto.Ext.Base, Kitto.Ext.Controller;

type
  TKExtViewportController = class(TKExtViewportControllerBase)
  private
    FController: IKExtController;
    procedure CreateSubController;
  protected
    procedure DoDisplay; override;
  public
    destructor Destroy; override;
  end;

implementation

uses
  Ext,
  Kitto.Ext.Session, Kitto.Metadata.Views;

{ TKExtViewportController }

destructor TKExtViewportController.Destroy;
begin
  // Prevent the compiler from calling _Release.
  Pointer(FController) := nil;
  inherited;
end;

procedure TKExtViewportController.DoDisplay;
begin
  inherited;
  CreateSubController;
end;

procedure TKExtViewportController.CreateSubController;
var
  LSubView: TKView;
begin
  Assert(Assigned(View));

  LSubView := Session.Config.Views.ViewByNode(View.GetNode('Controller/SubView'));
  FController := TKExtControllerFactory.Instance.CreateController(LSubView, Self);
  FController.Display;
end;

initialization
  TKExtControllerRegistry.Instance.RegisterClass('Viewport', TKExtViewportController);

finalization
  TKExtControllerRegistry.Instance.UnregisterClass('Viewport');

end.

