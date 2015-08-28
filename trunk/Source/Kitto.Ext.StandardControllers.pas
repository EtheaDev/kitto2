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

unit Kitto.Ext.StandardControllers;

{$I Kitto.Defines.inc}

interface

uses
  Classes, SysUtils,
  Kitto.Ext.Base, Kitto.Ext.DataTool;

type
  /// <summary>Logs the current user out ending the current session. Only
  /// useful if authentication is enabled.</summary>
  TKExtLogoutController = class(TKExtToolController)
  protected
    procedure ExecuteTool; override;
  public
    /// <summary>Returns the display label to use by default when not specified
    /// at the view or other level. Called through RTTI.</summary>
    class function GetDefaultDisplayLabel: string;

    /// <summary>Returns the image name to use by default when not specified at
    /// the view or other level. Called through RTTI.</summary>
    class function GetDefaultImageName: string; override;
  end;


  /// <summary>Base class for URL controllers.</summary>
  TKExtURLControllerBase = class(TKExtDataToolController)
  protected
    function GetURL: string; virtual; abstract;
    procedure ExecuteTool; override;
  end;

  /// <summary>
  ///   <para>Navigates to a specified URL in a different browser
  ///   window/tab.</para>
  ///   <para>Params:</para>
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Term</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>TargetURL</term>
  ///       <description>URL to navigate to. May contain macros.</description>
  ///     </item>
  ///   </list>
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

  /// <summary>Downloads a file that exists on disk or (by inheriting from it)
  /// is prepared on demand as a file or stream.</summary>
  /// <remarks>
  ///   <para>This class can be uses as-is to serve existing files, or
  ///   inherited to serve on-demand files and streams.</para>
  ///   <para>Params for the as-is version:</para>
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Term</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>FileName</term>
  ///       <description>Name of the file to serve (complete with full path).
  ///       May contain macros.</description>
  ///     </item>
  ///     <item>
  ///       <term>ClientFileName</term>
  ///       <description>File name as passed to the client; if not specified,
  ///       the name portion of FileName is used.</description>
  ///     </item>
  ///     <item>
  ///       <term>ContentType</term>
  ///       <description>Content type passed to the client; if not specified,
  ///       it is derived from the file name's extension.</description>
  ///     </item>
  ///     <item>
  ///       <term>PersistentFileName</term>
  ///       <description>Name of the file optionally persisted on the server
  ///        before download. No files are left on the server if this parameter
  ///        is not specified.</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TKExtDownloadFileController = class(TKExtDataToolController)
  strict private
    FTempFileNames: TStrings;
    FFileName: string;
    FStream: TStream;
    function GetClientFileName: string;
    function GetContentType: string;
    function GetFileName: string;
    procedure DoDownloadStream(const AStream: TStream;
      const AFileName: string; const AContentType: string);
    procedure PersistFile(const AStream: TStream);
  strict protected
    function GetPersistentFileName: string;
    procedure ExecuteTool; override;
    function GetFileExtension: string;
    function GetDefaultFileExtension: string; virtual;
    procedure AddTempFilename(const AFileName: string);
    procedure Cleanup;
    procedure DoAfterExecuteTool; override;
  protected
    /// <summary>Override this method to provide a default file name if it's
    /// not specified in the config. If you are using streams, don't override
    /// this method.</summary>
    function GetDefaultFileName: string; virtual;

    /// <summary>If you are creating a file on demand, do it in this method. If
    /// you are using streams, don't override this method and use CreateStream
    /// instead.</summary>
    /// <param name="AFileName">File name as read from the FileName param or
    /// returned by GetDefaultFileName.</param>
    procedure PrepareFile(const AFileName: string); virtual;

    /// <summary>Creates and returns a stream with the content to download.
    /// Override this method if you are using streams as opposed to
    /// files.</summary>
    /// <remarks>The caller will be responsible for freeing the stream when no
    /// longer needed.</remarks>
    function CreateStream: TStream; virtual;
    procedure InitDefaults; override;
  public
    destructor Destroy; override;
    /// <summary>Returns the image name to use by default when not specified at
    /// the view or other level. Called through RTTI.</summary>
    class function GetDefaultImageName: string; override;
  published
    procedure DownloadFile;
    procedure DownloadStream;
    property FileName: string read GetFileName;
    property ClientFileName: string read GetClientFileName;
    property ContentType: string read GetContentType;
  end;

  /// <summary>Uploads a file provided showing an Upload file dialog</summary>
  /// <remarks>
  ///   <para>This class can be uses as-is to Upload a file to the server, or
  ///   inherited to serve on-demand import.</para>
  ///   <para>Params for the as-is version:</para>
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Term</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>FileName</term>
  ///       <description>Name of the server file to save (complete with full path).
  ///       May contain macros.</description>
  ///     </item>
  ///     <item>
  ///       <term>ContentType</term>
  ///       <description>Content type passed from the client; if not specified,
  ///       it is derived from the file name's extension.</description>
  ///     </item>
  ///     <item>
  ///       <term>MaxUploadSize</term>
  ///       <description>Maximum allowed size for the uploaded file.</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TKExtUploadFileController = class(TKExtDataToolController)
  strict private
    FTempFileNames: TStrings;
    FWindow: TKExtModalWindow;
    function GetContentType: string;
    function GetPath: string;
    function GetMaxUploadSize: Integer;
    procedure ShowUploadFileDialog;
  strict protected
    procedure ExecuteTool; override;
    function GetWildCard: string; virtual;
    function GetDefaultPath: string; virtual;
    procedure AddTempFilename(const AFileName: string);
    procedure Cleanup;
    procedure DoAfterExecuteTool; override;
  protected
    /// <summary>This method is called when the file was uploaded to the server.</summary>
    /// <param name="AUploadedFileName">File name uploaded</param>
    procedure ProcessUploadedFile(const AUploadedFileName: string); virtual;

    procedure InitDefaults; override;
  public
    destructor Destroy; override;
    /// <summary>Returns the image name to use by default when not specified at
    /// the view or other level. Called through RTTI.</summary>
    class function GetDefaultImageName: string; override;
  published
    property Path: string read GetPath;
    property WildCard: string read GetWildCard;
    property ContentType: string read GetContentType;

    procedure Upload;
    procedure PostUpload;
  end;

implementation

uses
  StrUtils,
  Ext, ExtForm, ExtUxForm,
  EF.SysUtils, EF.Tree, EF.RegEx, EF.Localization,
  Kitto.Ext.Session, Kitto.Ext.Controller, Kitto.Metadata.DataView;

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
  LFilters := Config.FindNode('Filters');
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
      Result := Config.GetExpandedString('DefaultURL');
  end;
end;

{ TKExtDownloadFileController }

procedure TKExtDownloadFileController.AddTempFilename(const AFileName: string);
begin
  FTempFileNames.Add(AFileName);
end;

procedure TKExtDownloadFileController.Cleanup;
var
  I: Integer;
begin
  for I := 0 to FTempFileNames.Count - 1 do
    DeleteFile(FTempFileNames[I]);
  FTempFileNames.Clear;
end;

function TKExtDownloadFileController.CreateStream: TStream;
begin
  Result := nil;
end;

procedure TKExtDownloadFileController.ExecuteTool;
var
  LStream: TFileStream;
begin
  inherited;
  try
    FFileName := GetFileName;
    if FFileName <> '' then
    begin
      PrepareFile(FFileName);
      LStream := TFileStream.Create(FFileName, fmOpenRead);
      try
        PersistFile(LStream);
      finally
        FreeAndNil(LStream);
      end;
      Download(DownloadFile);
    end
    else
    begin
      FStream := CreateStream;
      PersistFile(FStream);
      Download(DownloadStream);
    end;
  except
    Cleanup;
    raise;
  end;
end;

destructor TKExtDownloadFileController.Destroy;
begin
  inherited;
  Cleanup;
  FTempFileNames.Free;
end;

procedure TKExtDownloadFileController.DoAfterExecuteTool;
begin
  // We'll call AfterExecuteTool in DoDownloadStream.
end;

procedure TKExtDownloadFileController.DoDownloadStream(const AStream: TStream;
  const AFileName, AContentType: string);
begin
  Session.DownloadStream(AStream, AFileName, AContentType);
  AfterExecuteTool;
end;

function TKExtDownloadFileController.GetPersistentFileName: string;
begin
  Result := ExpandServerRecordValues(Config.GetExpandedString('PersistentFileName'));
end;

procedure TKExtDownloadFileController.PersistFile(const AStream: TStream);
var
  LPersistentFileName: string;
  LFileStream: TFileStream;
begin
  Assert(Assigned(AStream));

  LPersistentFileName := GetPersistentFileName;
  if LPersistentFileName <> '' then
  begin
    if FileExists(LPersistentFileName) then
      DeleteFile(LPersistentFileName);
    LFileStream := TFileStream.Create(LPersistentFileName, fmCreate or fmShareExclusive);
    try
      AStream.Position := 0;
      LFileStream.CopyFrom(AStream, AStream.Size);
    finally
      FreeAndNil(LFileStream);
      AStream.Position := 0;
    end;
  end;
end;

procedure TKExtDownloadFileController.DownloadFile;
var
  LStream: TStream;
begin
  try
    // The file might not exist if the browser has sent multiple request and
    // it was served before and then deleted (see Cleanup).
    if FileExists(FFileName) then
    begin
      LStream := TFileStream.Create(FFileName, fmOpenRead);
      try
        DoDownloadStream(LStream, ClientFileName, ContentType);
      finally
        FreeAndNil(LStream);
      end;
    end;
  finally
//    Cleanup;
  end;
end;

procedure TKExtDownloadFileController.DownloadStream;
begin
  try
    try
      DoDownloadStream(FStream, ClientFileName, ContentType);
    finally
      FreeAndNil(FStream);
    end;
  finally
    Cleanup;
  end;
end;

function TKExtDownloadFileController.GetClientFileName: string;
begin
  Result := ExpandServerRecordValues(Config.GetExpandedString('ClientFileName'));
  if (Result = '') then
  begin
    if Assigned(ViewTable) then
      Result := ViewTable.PluralDisplayLabel + GetDefaultFileExtension
    else
      Result := ExtractFileName(FileName);
  end;
end;

function TKExtDownloadFileController.GetContentType: string;
begin
  Result := Config.GetExpandedString('ContentType');
end;

function TKExtDownloadFileController.GetDefaultFileExtension: string;
begin
  Result := '';
end;

function TKExtDownloadFileController.GetDefaultFileName: string;
begin
  Result := '';
end;

class function TKExtDownloadFileController.GetDefaultImageName: string;
begin
  Result := 'download';
end;

function TKExtDownloadFileController.GetFileExtension: string;
begin
  if ClientFileName <> '' then
    Result := ExtractFileExt(ClientFileName)
  else
    Result := GetDefaultFileExtension;
end;

function TKExtDownloadFileController.GetFileName: string;
begin
  Result := Config.GetExpandedString('FileName', GetDefaultFileName);
end;

procedure TKExtDownloadFileController.InitDefaults;
begin
  inherited;
  FTempFileNames := TStringList.Create;
end;

procedure TKExtDownloadFileController.PrepareFile(const AFileName: string);
begin
end;

{ TKExtLogoutController }

procedure TKExtLogoutController.ExecuteTool;
begin
  inherited;
  Session.Logout;
end;

class function TKExtLogoutController.GetDefaultDisplayLabel: string;
begin
  Result := _('Logout');
end;

class function TKExtLogoutController.GetDefaultImageName: string;
begin
  Result := 'logout';
end;

{ TKExtUploadFileController }

procedure TKExtUploadFileController.AddTempFilename(const AFileName: string);
begin
  FTempFileNames.Add(AFileName);
end;

procedure TKExtUploadFileController.Cleanup;
var
  I: Integer;
begin
  for I := 0 to FTempFileNames.Count - 1 do
    DeleteFile(FTempFileNames[I]);
  FTempFileNames.Clear;
end;

destructor TKExtUploadFileController.Destroy;
begin
  inherited;
  Cleanup;
  FTempFileNames.Free;
end;

procedure TKExtUploadFileController.DoAfterExecuteTool;
begin
  // We'll call AfterExecuteTool in PostUpload.
end;

procedure TKExtUploadFileController.ExecuteTool;
begin
  ShowUploadFileDialog;
end;

procedure TKExtUploadFileController.ShowUploadFileDialog;
var
  LUploadButton: TKExtButton;
  LFormPanel: TExtFormFormPanel;
  LSubmitAction: TExtFormActionSubmit;
  LUploadFormField: TExtUxFormFileUploadField;
begin
  FreeAndNil(FWindow);
  FWindow := TKExtModalWindow.Create(Self);
  FWindow.Width := 400;
  FWindow.Height := 120;
  FWindow.Maximized := Session.IsMobileBrowser;
  FWindow.Border := not FWindow.Maximized;
  FWindow.Closable := True;
  FWindow.Title := _('File upload');

  LFormPanel := TExtFormFormPanel.CreateAndAddTo(FWindow.Items);
  LFormPanel.Region := rgCenter;
  LFormPanel.Frame := True;
  LFormPanel.FileUpload := True;
  LFormPanel.LabelAlign := laRight;
  LFormPanel.LabelWidth := 100;
  LUploadFormField := TExtUxFormFileUploadField.CreateAndAddTo(LFormPanel.Items);
  LUploadFormField.FieldLabel := _('Upload a file');
  LUploadFormField.EmptyText := _('Select a file to upload');
  LUploadFormField.AllowBlank := False;
  LUploadFormField.Anchor := '0 5 0 0';
  LUploadButton := TKExtButton.CreateAndAddTo(LFormPanel.Buttons);
  LUploadButton.Text := _('Upload');
  LUploadButton.SetIconAndScale('Upload', IfThen(Session.IsMobileBrowser,'medium', 'small'));

  LSubmitAction := TExtFormActionSubmit.Create(FWindow);
  LSubmitAction.Url := MethodURI(Upload);
  LSubmitAction.WaitMsg := _('File upload in progress...');
  LSubmitAction.WaitTitle := _('Please wait...');
  LSubmitAction.Success := Ajax(PostUpload);
  LSubmitAction.Failure := ExtMessageBox.Alert(_('File upload error'), '%1.result.message');
  LUploadButton.Handler := TExtFormBasicForm(LFormPanel.GetForm).Submit(LSubmitAction);

  Session.MaxUploadSize := GetMaxUploadSize;
  FWindow.Show;
end;

procedure TKExtUploadFileController.Upload;
var
  LFileName: string;
begin
  LFileName := Session.FileUploadedFullName;
  { TODO : Check the file against limitations such as type and size}
  if (LFileName <> '') and FileExists(LFileName) then
  begin
    AddTempFilename(LFileName);
    try
      ProcessUploadedFile(LFileName);
    finally
      Cleanup;
    end;
  end;
  AfterExecuteTool;
end;

function TKExtUploadFileController.GetContentType: string;
begin
  Result := Config.GetExpandedString('ContentType');
end;

function TKExtUploadFileController.GetDefaultPath: string;
begin
  Result := '';
end;

function TKExtUploadFileController.GetMaxUploadSize: Integer;
begin
  Result := Config.GetInteger('MaxUploadSize', MaxLongint);
end;

class function TKExtUploadFileController.GetDefaultImageName: string;
begin
  Result := 'upload';
end;

function TKExtUploadFileController.GetPath: string;
begin
  Result := Config.GetExpandedString('Path', GetDefaultPath);
end;

function TKExtUploadFileController.GetWildCard: string;
begin
  Result := Config.GetExpandedString('Path', GetDefaultPath);
end;

procedure TKExtUploadFileController.InitDefaults;
begin
  inherited;
  FTempFileNames := TStringList.Create;
end;

procedure TKExtUploadFileController.PostUpload;
begin
  FWindow.Close;
end;

procedure TKExtUploadFileController.ProcessUploadedFile(const AUploadedFileName: string);
begin
end;

initialization
  TKExtControllerRegistry.Instance.RegisterClass('Logout', TKExtLogoutController);
  TKExtControllerRegistry.Instance.RegisterClass('URL', TKExtURLController);
  TKExtControllerRegistry.Instance.RegisterClass('FilteredURL', TKExtFilteredURLController);
  TKExtControllerRegistry.Instance.RegisterClass('DownloadFile', TKExtDownloadFileController);
  TKExtControllerRegistry.Instance.RegisterClass('UploadFile', TKExtUploadFileController);

finalization
  TKExtControllerRegistry.Instance.UnregisterClass('Logout');
  TKExtControllerRegistry.Instance.UnregisterClass('URL');
  TKExtControllerRegistry.Instance.UnregisterClass('FilteredURL');
  TKExtControllerRegistry.Instance.UnregisterClass('DownloadFile');
  TKExtControllerRegistry.Instance.UnregisterClass('UploadFile');

end.
