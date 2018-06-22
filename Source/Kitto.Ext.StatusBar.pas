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

unit Kitto.Ext.StatusBar;

{$I Kitto.Defines.inc}

interface

uses
  Ext.Base
  , Kitto.JS.Base
  , Kitto.JS
  , Kitto.Ext.Base
  , Kitto.Ext.Panel
  , Kitto.Metadata.Views
  ;

type
  TKExtDefaultStatusBar = class(TKExtStatusBar, IJSStatusHost)
  strict protected
    procedure InitDefaults; override;
  public
    destructor Destroy; override;

    function ShowBusy: TJSExpression;
    function ClearStatus: TJSExpression; override;
  end;

  TKExtStatusBarController = class(TKExtPanelControllerBase)
  strict private
    FStatusBar: TKExtDefaultStatusBar;
  strict protected
    function GetDefaultHeight: Integer; override;
    function GetDefaultWidth: Integer; override;
    function GetDefaultSplit: Boolean; override;
    procedure InitDefaults; override;
    procedure DoDisplay; override;
    function GetObjectNamePrefix: string; override;
  end;

implementation

uses
  EF.Tree
  , Kitto.Web.Application
  , Kitto.Web.Session
  , Kitto.JS.Controller
  ;

{ TKExtStatusBarController }

procedure TKExtStatusBarController.DoDisplay;
begin
  inherited;
  FStatusBar.DefaultText := Config.GetExpandedString('Text');
  FStatusBar.DefaultIconCls := TKWebApplication.Current.SetViewIconStyle(View, '', 'sb_', 'padding-left: 25px !important;');
end;

function TKExtStatusBarController.GetDefaultHeight: Integer;
begin
  Result := 0;
end;

function TKExtStatusBarController.GetDefaultSplit: Boolean;
begin
  Result := False;
end;

function TKExtStatusBarController.GetDefaultWidth: Integer;
begin
  Result := 0;
end;

function TKExtStatusBarController.GetObjectNamePrefix: string;
begin
  Result := 'status';
end;

procedure TKExtStatusBarController.InitDefaults;
begin
  inherited;
  Layout := lyFit;
  AutoHeight := True;

  FStatusBar := TKExtDefaultStatusBar.CreateAndAddToArray(Items);
end;

{ TKExtDefaultStatusBar }

function TKExtDefaultStatusBar.ClearStatus: TJSExpression;
begin
  Result := inherited ClearStatus;
  SetText(DefaultText);
  SetIcon(DefaultIconCls);
end;

destructor TKExtDefaultStatusBar.Destroy;
begin
  if (TKWebSession.Current <> nil) and Assigned(TKWebSession.Current.StatusHost) and (TKWebSession.Current.StatusHost.AsObject = Self) then
    TKWebSession.Current.StatusHost := nil;
  inherited;
end;

procedure TKExtDefaultStatusBar.InitDefaults;
begin
  inherited;
  if TKWebSession.Current.StatusHost = nil then
    TKWebSession.Current.StatusHost := Self;
end;

function TKExtDefaultStatusBar.ShowBusy: TJSExpression;
begin
  Result := inherited ShowBusy;
end;

initialization
  TJSControllerRegistry.Instance.RegisterClass('StatusBar', TKExtStatusBarController);

finalization
  TJSControllerRegistry.Instance.UnregisterClass('StatusBar');

end.
