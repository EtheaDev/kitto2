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

unit Kitto.Ext.DataPanelComposite;

{$I Kitto.Defines.inc}

interface

uses
  Kitto.Ext.DataPanel, Kitto.Ext.Controller;

type
  ///	<summary>A data panel whose purpose is to contain other (leaf)
  ///	data panels and delegate implementation to them.</summary>
  TKExtDataPanelCompositeController = class abstract(TKExtDataPanelController)
  public
    procedure LoadData; override;
    procedure RefilterData(const AFilterExpression: string); override;
  strict protected
    procedure InitSubController(const AController: IKExtController); override;
  published
    procedure RefreshData; override;
  end;

implementation

uses
  ExtPascal,
  Kitto.Ext.Base;

{ TKExtDataPanelCompositeController }

procedure TKExtDataPanelCompositeController.RefilterData(const AFilterExpression: string);
begin
  inherited;
  Apply(
    procedure (AObject: TExtObject)
    begin
      if AObject is TKExtDataPanelController then
        TKExtDataPanelController(AObject).RefilterData(AFilterExpression);
    end);
end;

procedure TKExtDataPanelCompositeController.InitSubController(
  const AController: IKExtController);
begin
  inherited;
  Assert(Assigned(AController));

  AController.Config.SetObject('Sys/RefreshHandler', Self);
end;

procedure TKExtDataPanelCompositeController.LoadData;
begin
  inherited;
  Apply(
    procedure (AObject: TExtObject)
    begin
      if AObject is TKExtDataPanelController then
        TKExtDataPanelController(AObject).LoadData;
    end);
end;

procedure TKExtDataPanelCompositeController.RefreshData;
begin
  inherited;
  Apply(
    procedure (AObject: TExtObject)
    begin
      if AObject is TKExtDataPanelController then
        TKExtDataPanelController(AObject).RefreshData;
    end);
end;

end.
