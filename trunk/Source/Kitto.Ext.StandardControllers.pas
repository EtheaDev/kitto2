{-------------------------------------------------------------------------------
   Copyright 2011 Ethea S.r.l.

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
  Classes,
  Kitto.Ext.DataTool;

type
  TKExtURLControllerBase = class(TKExtDataToolController)
  protected
    function GetURL: string; virtual; abstract;
    procedure ExecuteTool; override;
  end;

  ///	<summary>Navigates to a specified URL in a different browser
  ///	window/tab.</summary>
  TKExtURLController = class(TKExtURLControllerBase)
  protected
    function GetURL: string; override;
  end;

  ///	<summary>Navigates to a specified URL in a different browser window/tab.
  ///	Several URLs can be specified and the choice is made by filtering on
  ///	request headers (for example, you can navigate to a different URL
  ///	depending on the client IP address or class of addresses).</summary>
  TKExtFilteredURLController = class(TKExtURLControllerBase)
  protected
    function GetURL: string; override;
  end;

  ///	<summary>Downloads a file that exists on disk or (by inheriting from it)
  ///	is prepared on demand as a file or stream.</summary>
  TKExtDownloadFileController = class(TKExtDataToolController)
  protected
    procedure ExecuteTool; override;
  protected
    ///	<summary>Override this method to provide a default file name if it's
    ///	not specified in the config. If you are using streams, don't override
    ///	this method.</summary>
    function GetDefaultFileName: string; virtual;

    ///	<summary>If you are creating a file on demand, do it in this method. If
    ///	you are using streams, don't override this method and use CreateStream
    ///	instead.</summary>
    procedure PrepareFile(const AFileName: string); virtual;

    ///	<summary>Creates and returns a stream with the content to download.
    ///	Override this method if you are using streams as opposed to
    ///	files.</summary>
    function CreateStream: TStream; virtual;
  published
    procedure DownloadFile;
  end;

implementation

uses
  SysUtils,
  EF.Tree, EF.RegEx,
  Kitto.Ext.Session, Kitto.Ext.Controller;

{ TKExtURLControllerBase }

procedure TKExtURLControllerBase.ExecuteTool;
var
  LURL: string;
begin
  inherited;
  LURL := GetURL;
  if LURL <> '' then
    Session.Navigate(LURL);
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
  LFilters := View.FindNode('Controller/Filters');
  if Assigned(LFilters) and (LFilters.ChildCount > 0) then
  begin
    for I := 0 to LFilters.ChildCount - 1 do
    begin
      LHeader := Session.RequestHeader[LFilters.Children[I].GetExpandedString('Header')];
      LPattern := LFilters.Children[I].GetExpandedString('Pattern');
      LTargetURL := LFilters.Children[I].GetExpandedString('TargetURL');
      if StrMatchesPatternOrRegex(LHeader, LPattern) then
      begin
        Result := LTargetURL;
        Break;
      end;
    end;
    if Result = '' then
      Result := View.GetExpandedString('Controller/DefaultURL');
  end;
end;

{ TKExtDownloadFileController }

function TKExtDownloadFileController.CreateStream: TStream;
begin
  Result := nil;
end;

procedure TKExtDownloadFileController.ExecuteTool;
begin
  inherited;
  Download(DownloadFile);
end;

procedure TKExtDownloadFileController.DownloadFile;
var
  LFileName: string;
  LStream: TStream;
begin
  inherited;
  LFileName := View.GetExpandedString('Controller/FileName', GetDefaultFileName);
  if LFileName <> '' then
  begin
    PrepareFile(LFileName);
    LStream := TFileStream.Create(LFileName, fmOpenRead);
    try
      Session.DownloadStream(LStream, View.GetExpandedString('Controller/ClientFileName',
        ExtractFileName(LFileName)), View.GetExpandedString('Controller/ContentType'));
    finally
      FreeAndNil(LStream);
    end;
  end
  else
  begin
    LStream := CreateStream;
    try
      if Assigned(LStream) then
        Session.DownloadStream(LStream, View.GetExpandedString('Controller/ClientFileName',
          ExtractFileName(LFileName)), View.GetExpandedString('Controller/ContentType'));
    finally
      FreeAndNil(LStream);
    end;
  end;
end;

function TKExtDownloadFileController.GetDefaultFileName: string;
begin
  Result := '';
end;

procedure TKExtDownloadFileController.PrepareFile(const AFileName: string);
begin

end;

initialization
  TKExtControllerRegistry.Instance.RegisterClass('URL', TKExtURLController);
  TKExtControllerRegistry.Instance.RegisterClass('FilteredURL', TKExtFilteredURLController);
  TKExtControllerRegistry.Instance.RegisterClass('DownloadFile', TKExtDownloadFileController);

finalization
  TKExtControllerRegistry.Instance.UnregisterClass('URL');
  TKExtControllerRegistry.Instance.UnregisterClass('FilteredURL');
  TKExtControllerRegistry.Instance.UnregisterClass('DownloadFile');

end.
