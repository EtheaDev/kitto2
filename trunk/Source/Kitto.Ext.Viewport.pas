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
    procedure CreateSubController;
  protected
    procedure DoDisplay; override;
  end;

implementation

uses
  Ext,
  Kitto.Ext.Session, Kitto.Metadata.Views;

{ TKExtViewportController }

procedure TKExtViewportController.DoDisplay;
begin
  inherited;
  CreateSubController;
end;

procedure TKExtViewportController.CreateSubController;
var
  LSubView: TKView;
  LController: IKExtController;
begin
  Assert(Assigned(View));

  LSubView := Session.Config.Views.ViewByNode(View.GetNode('Controller/SubView'));
  LController := TKExtControllerFactory.Instance.CreateController(LSubView, Self);
  LController.Display;
end;

initialization
  TKExtControllerRegistry.Instance.RegisterClass('Viewport', TKExtViewportController);

finalization
  TKExtControllerRegistry.Instance.UnregisterClass('Viewport');

end.

