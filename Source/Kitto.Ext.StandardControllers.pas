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

unit Kitto.Ext.StandardControllers;

{$I Kitto.Defines.inc}

interface

uses
  Ext.Base
  , Kitto.Metadata.Views
  , Kitto.Ext.Base
  , Kitto.Ext.DataTool
  ;

type
  /// <summary>
  ///  Opens a view through Session.DisplayView. Useful to add a
  ///  view to a panel's toolbar with customized properties (such as
  ///  DisplayLabel or ImageName).
  ///  If no customization is needed, just add the view's name
  ///  or definition under the ToolViews node.
  /// </summary>
  TKExtDisplayViewController = class(TKExtToolController)
  private
    FTargetView: TKView;
    function GetTargetView: TKView;
  protected
    property TargetView: TKView read GetTargetView;
    procedure ExecuteTool; override;
  end;

  /// <summary>
  ///  Base class for URL controllers.
  /// </summary>
  TKExtURLControllerBase = class(TKExtDataToolController)
  protected
    function GetURL: string; virtual; abstract;
    procedure ExecuteTool; override;
  end;

  /// <summary>
  ///  <para>Navigates to a specified URL in a different browser
  ///  window/tab.</para>
  ///  <para>Params:</para>
  ///  <list type="table">
  ///   <listheader>
  ///    <term>Term</term>
  ///    <description>Description</description>
  ///   </listheader>
  ///   <item>
  ///    <term>TargetURL</term>
  ///    <description>URL to navigate to. May contain macros.</description>
  ///   </item>
  ///  </list>
  /// </summary>
  TKExtURLController = class(TKExtURLControllerBase)
  protected
    function GetURL: string; override;
  end;

  /// <summary>
  ///   <para>Navigates to a specified URL in a different browser window/tab.
  ///   Several URLs can be specified and the choice is made by filtering on
  ///   request headers (for example, you can navigate to a different URL
  ///   depending on the client IP address or class of addresses).</para>
  ///   <para>Params:</para>
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Term</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>Filters</term>
  ///       <description>A collection of filters. Each node contains a Header
  ///       to filter upon (ex. REMOTE_ADDR), a Pattern to match the header
  ///       value to, and a TargetURL which may contain macros.</description>
  ///     </item>
  ///     <item>
  ///       <term>DefaultURL</term>
  ///       <description>Optional URL to navigate to when no filters
  ///       apply.</description>
  ///     </item>
  ///   </list>
  /// </summary>
  /// <remarks>If no filters apply, and DefaultURL is not specified, navigation
  /// is not performed.</remarks>
  TKExtFilteredURLController = class(TKExtURLControllerBase)
  protected
    function GetURL: string; override;
  end;

implementation

uses
  EF.Sys
  , EF.Tree
  , EF.RegEx
  , Ext.Form
  , Kitto.Metadata.DataView
  , Kitto.JS
  , Kitto.JS.Controller
  , Kitto.Web.Request
  , Kitto.Web.Application
  ;

{ TKExtURLControllerBase }

procedure TKExtURLControllerBase.ExecuteTool;
var
  LURL: string;
begin
  inherited;
  LURL := GetURL;
  if LURL <> '' then
    TKWebApplication.Current.Navigate(LURL);
end;

{ TKExtURLController }

function TKExtURLController.GetURL: string;
begin
  Result := Config.GetExpandedString('TargetURL');
end;

{ TKExtFilteredURLController }

function TKExtFilteredURLController.GetURL: string;
var
  LFilters: TEFNode;
  I: Integer;
  LHeader: string;
  LPattern: string;
  LTargetURL: string;
begin
  Result := '';
  LFilters := Config.FindNode('Filters');
  if Assigned(LFilters) and (LFilters.ChildCount > 0) then
  begin
    for I := 0 to LFilters.ChildCount - 1 do
    begin
      LHeader := TKWebRequest.Current.GetHeaderField(LFilters.Children[I].GetExpandedString('Header'));
      LPattern := LFilters.Children[I].GetExpandedString('Pattern');
      LTargetURL := LFilters.Children[I].GetExpandedString('TargetURL');
      if StrMatchesPatternOrRegex(LHeader, LPattern) then
      begin
        Result := LTargetURL;
        Break;
      end;
    end;
    if Result = '' then
      Result := Config.GetExpandedString('DefaultURL');
  end;
end;

{ TKExtDisplayViewController }

procedure TKExtDisplayViewController.ExecuteTool;
begin
  inherited;
  TKWebApplication.Current.DisplayView(TargetView);
end;

function TKExtDisplayViewController.GetTargetView: TKView;
begin
  if not Assigned(FTargetView) then
    FTargetView := TKWebApplication.Current.Config.Views.FindViewByNode(View.FindNode('Controller/View'));
  Result := FTargetView;
  Assert(Assigned(Result));
end;

initialization
  TJSControllerRegistry.Instance.RegisterClass('DisplayView', TKExtDisplayViewController);
  TJSControllerRegistry.Instance.RegisterClass('URL', TKExtURLController);
  TJSControllerRegistry.Instance.RegisterClass('FilteredURL', TKExtFilteredURLController);

finalization
  TJSControllerRegistry.Instance.UnregisterClass('DisplayView');
  TJSControllerRegistry.Instance.UnregisterClass('URL');
  TJSControllerRegistry.Instance.UnregisterClass('FilteredURL');

end.
