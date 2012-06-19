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

unit Kitto.Ext.StatusBar;

{$I Kitto.Defines.inc}

interface

uses
  Ext,
  Kitto.Ext.Base, Kitto.Metadata.Views;

type
  TKExtDefaultStatusBar = class(TKExtStatusBar)
  strict protected
    procedure InitDefaults; override;
  public
    procedure ClearStatus; override;
    destructor Destroy; override;
  end;

  TKExtStatusBarController = class(TKExtPanelControllerBase)
  strict private
    FStatusBar: TKExtDefaultStatusBar;
  strict protected
    function GetDefaultSplit: Boolean; override;
    procedure InitDefaults; override;
    procedure DoDisplay; override;
  end;

implementation

uses
  ExtPascal,
  EF.Tree,
  Kitto.Ext.Controller, Kitto.Ext.Session;

{ TKExtStatusBarController }

procedure TKExtStatusBarController.DoDisplay;
begin
  inherited;
  FStatusBar.DefaultText := Config.GetExpandedString('Text');
  FStatusBar.DefaultIconCls := Session.SetViewIconStyle(View, '', 'sb_', 'padding-left: 25px !important;');
end;

function TKExtStatusBarController.GetDefaultSplit: Boolean;
begin
  Result := False;
end;

procedure TKExtStatusBarController.InitDefaults;
begin
  inherited;
  Layout := lyFit;
  AutoHeight := True;

  FStatusBar := TKExtDefaultStatusBar.AddTo(Items);
end;

{ TKExtDefaultStatusBar }

procedure TKExtDefaultStatusBar.ClearStatus;
begin
  inherited;
  SetText(DefaultText);
  SetIcon(DefaultIconCls);
end;

destructor TKExtDefaultStatusBar.Destroy;
begin
  if Session.StatusHost = Self then
    Session.StatusHost := nil;
  inherited;
end;

procedure TKExtDefaultStatusBar.InitDefaults;
begin
  inherited;
  if Session.StatusHost = nil then
    Session.StatusHost := Self;
end;

initialization
  TKExtControllerRegistry.Instance.RegisterClass('StatusBar', TKExtStatusBarController);

finalization
  TKExtControllerRegistry.Instance.UnregisterClass('StatusBar');

end.
