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

unit Kitto.Ext.DataPanelComposite;

{$I Kitto.Defines.inc}

interface

uses
  Kitto.JS
  , Kitto.Ext.DataPanel
  , Kitto.Ext.Controller
  ;

type
  /// <summary>
  ///  A data panel whose purpose is to contain other (leaf)
  ///  data panels and delegate implementation to them.
  /// </summary>
  TKExtDataPanelCompositeController = class abstract(TKExtDataPanelController)
  public
    function GetFilterExpression: string; override;
  strict protected
    procedure InitSubController(const AController: IJSController); override;
    procedure DoDisplay; override;
  public
    procedure Activate; override;
  //published
    procedure LoadData; override;
  end;

implementation

uses
  EF.StrUtils
  , Ext.Base
  , Kitto.Ext.Base
  ;

{ TKExtDataPanelCompositeController }

procedure TKExtDataPanelCompositeController.Activate;
begin
  inherited;
  Apply(
    procedure (AObject: TExtObject)
    begin
      if AObject is TKExtPanelBase then
        TKExtPanelBase(AObject).Activate;
    end);
end;

procedure TKExtDataPanelCompositeController.DoDisplay;
begin
  inherited;
  CheckCanRead;
  if AutoLoadData then
    LoadData;
end;

function TKExtDataPanelCompositeController.GetFilterExpression: string;
var
  LResult: string;
begin
  LResult := '';
  Apply(
    procedure (AObject: TExtObject)
    begin
      if AObject is TKExtDataPanelController then
        LResult := SmartConcat(LResult, ' and ', TKExtDataPanelController(AObject).GetFilterExpression);
    end);
  Result := LResult;
end;

procedure TKExtDataPanelCompositeController.InitSubController(const AController: IJSController);
begin
  inherited;
  Assert(Assigned(AController));

  AController.Config.SetObject('Sys/ParentDataPanel', Self);
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

end.
