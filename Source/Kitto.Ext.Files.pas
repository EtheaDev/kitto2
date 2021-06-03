{-------------------------------------------------------------------------------
   Copyright 2012-2021 Ethea S.r.l.

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

unit Kitto.Ext.Files;

{$I Kitto.Defines.inc}

interface

uses
  Classes
  , HTTPApp
  , Ext.Base
  , Kitto.Ext.DataTool
  ;

type
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
    function GetContentType: string;
    function GetFileName: string;
    procedure DoDownloadStream(const AStream: TStream;
      const AFileName: string; const AContentType: string);
    procedure PersistFile(const AStream: TStream);
  strict protected
    function GetClientFileName: string; virtual;
    function GetPersistentFileName: string; virtual;
    procedure ExecuteTool; override;
    function GetFileExtension: string; virtual;
    function GetDefaultFileExtension: string; virtual;
    procedure AddTempFilename(const AFileName: string);
    procedure Cleanup;
    procedure DoAfterExecuteTool; override;
  protected
    /// <summary>
    ///  Override this method to provide a default file name if it's
    ///  not specified in the config. If you are using streams, don't override
    ///  this method.
    /// </summary>
    function GetDefaultFileName: string; virtual;

    /// <summary>
    ///  If you are creating a file on demand, do it in this method. If
    ///  you are using streams, don't override this method and use CreateStream
    ///  instead.
    /// </summary>
    /// <param name="AFileName">
    ///  File name as read from the FileName param or
    ///  returned by GetDefaultFileName.
    /// </param>
    procedure PrepareFile(const AFileName: string); virtual;

    /// <summary>
    ///  Creates and returns a stream with the content to download.
    ///  Override this method if you are using streams as opposed to
    ///  files.
    /// </summary>
    /// <remarks>
    ///  The caller will be responsible for freeing the stream when no
    ///  longer needed.
    /// </remarks>
    function CreateStream: TStream; virtual;
    procedure InitDefaults; override;
  public
    destructor Destroy; override;
    /// <summary>
    ///  Returns the image name to use by default when not specified at
    ///  the view or other level. Called through RTTI.
    /// </summary>
    class function GetDefaultImageName: string; override;
  //published
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
  ///       <term>Path</term>
  ///       <description>Full path in which to save the uploaded file.
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
    FUploadFileDialog: TExtPanel;
    function GetContentType: string;
    function GetPath: string;
    function GetMaxUploadSize: Integer;
  strict protected
    procedure ExecuteTool; override;
    function GetAcceptedWildcards: string; virtual;
    function GetDefaultPath: string; virtual;
    procedure AddTempFilename(const AFileName: string);
    procedure Cleanup;
    procedure DoAfterExecuteTool; override;
  protected
    /// <summary>
    ///  This method takes care of processing the uploaded file. The default
    ///  implementation just stores the file in the Path directory (no duplicate
    ///  name check).
    /// </summary>
    procedure ProcessUploadedFile(const AFile: TAbstractWebRequestFile); virtual;

    procedure InitDefaults; override;
  public
    destructor Destroy; override;
    /// <summary>
    ///  Returns the image name to use by default when not specified at
    ///  the view or other level. Called through RTTI.
    /// </summary>
    class function GetDefaultImageName: string; override;
  //published
    property Path: string read GetPath;
    property AcceptedWildcards: string read GetAcceptedWildcards;
    property ContentType: string read GetContentType;
    property MaxUploadSize: Integer read GetMaxUploadSize;

    procedure Upload;
    procedure PostUpload;
  end;

implementation

uses
  Types
  , SysUtils
  , StrUtils
  , IOUtils
  , JSON
  , Masks
  , EF.Localization
  , Kitto.Web.Application
  , Kitto.Web.Request
  , Kitto.Web.Response
  , Kitto.Ext.UploadFileDialog
  , Kitto.JS.Controller
  ;

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
    TFile.Delete(FTempFileNames[I]);
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
  FreeAndNil(FStream);
end;

procedure TKExtDownloadFileController.DoAfterExecuteTool;
begin
  // We'll call AfterExecuteTool in DoDownloadStream.
end;

procedure TKExtDownloadFileController.DoDownloadStream(const AStream: TStream;
  const AFileName, AContentType: string);
begin
  TKWebApplication.Current.DownloadStream(AStream, AFileName, AContentType, False);
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
      TFile.Delete(LPersistentFileName);
    LFileStream := TFileStream.Create(LPersistentFileName, fmCreate or fmShareExclusive);
    try
      AStream.Position := 0;
      LFileStream.CopyFrom(AStream, 0);
    finally
      FreeAndNil(LFileStream);
      AStream.Position := 0;
    end;
  end;
end;

procedure TKExtDownloadFileController.DownloadFile;
begin
  try
    // The file might not exist if the browser has sent multiple request and
    // it was served before and then deleted (see Cleanup).
    if FileExists(FFileName) then
      DoDownloadStream(TFileStream.Create(FFileName, fmOpenRead), ClientFileName, ContentType);
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
      FStream := nil;
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
  Result := ExpandServerRecordValues(Config.GetExpandedString('FileName', GetDefaultFileName));
end;

procedure TKExtDownloadFileController.InitDefaults;
begin
  inherited;
  FTempFileNames := TStringList.Create;
end;

procedure TKExtDownloadFileController.PrepareFile(const AFileName: string);
begin
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
    TFile.Delete(FTempFileNames[I]);
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
  FreeAndNil(FUploadFileDialog);
  FUploadFileDialog := ShowUploadFileDialog(Self, _(DisplayLabel),
    GetMethodURL(Upload),
    TKWebResponse.Current.Items.AjaxCallMethod(Self).SetMethod(PostUpload).AsFunction);
end;

procedure TKExtUploadFileController.Upload;
var
  LFileName, LAcceptedWildcards, LWildcard: string;
  LWildcards: TStringDynArray;
  I: Integer;
  LAccepted: Boolean;
  LResult: TJSONObject;
begin
  LResult := TJSONObject.Create;
  try
    if TKWebRequest.Current.Files.Count = 0 then
    begin
      LResult.AddPair('success', TJSONFalse.Create);
      LResult.AddPair('msg', _('No file uploaded'));
      TKWebResponse.Current.Items.AddJSON(LResult.ToJSON);
      Exit;
    end;

    LFileName := TKWebRequest.Current.Files[0].FileName;
    LAcceptedWildcards := AcceptedWildcards;
    if LAcceptedWildcards <> '' then
    begin
      LWildcards := SplitString(LAcceptedWildcards, ' ');
      LAccepted := False;
      for I := 0 to High(LWildcards) do
      begin
        LWildcard := LWildcards[I];
        if (LWildcard <> '') and MatchesMask(LFileName, LWildcard) then
        begin
          LAccepted := True;
          Break;
        end;
      end;
      if not LAccepted then
      begin
        LResult.AddPair('success', TJSONFalse.Create);
        LResult.AddPair('msg', Format(_('Error: uploaded file name doesn''t match wildcards (%s)'), [LAcceptedWildcards]));
        TKWebResponse.Current.Items.AddJSON(LResult.ToJSON);
        Exit;
      end;
    end;

    { TODO : Check the file against limitations such as type and size}
    ProcessUploadedFile(TKWebRequest.Current.Files[0]);
    LResult.AddPair('success', TJSONTrue.Create);
    TKWebResponse.Current.Items.AddJSON(LResult.ToJSON);
    AfterExecuteTool;
  finally
    FreeAndNil(LResult);
  end;
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
  Result := Config.GetInteger('MaxUploadSize', MaxInt);
end;

class function TKExtUploadFileController.GetDefaultImageName: string;
begin
  Result := 'upload';
end;

function TKExtUploadFileController.GetPath: string;
begin
  Result := Config.GetExpandedString('Path', GetDefaultPath);
end;

function TKExtUploadFileController.GetAcceptedWildcards: string;
begin
  Result := Config.GetString('AcceptedWildcards');
end;

procedure TKExtUploadFileController.InitDefaults;
begin
  inherited;
  FTempFileNames := TStringList.Create;
end;

procedure TKExtUploadFileController.PostUpload;
begin
  FUploadFileDialog.Close;
end;

procedure TKExtUploadFileController.ProcessUploadedFile(const AFile: TAbstractWebRequestFile);
var
  LFileStream: TFileStream;
begin
  LFileStream := TFileStream.Create(TPath.Combine(Path, AFile.FileName), fmCreate or fmShareExclusive);
  try
    LFileStream.CopyFrom(AFile.Stream, 0);
  finally
    FreeAndNil(LFileStream);
  end;
end;

initialization
  TJSControllerRegistry.Instance.RegisterClass('DownloadFile', TKExtDownloadFileController);
  TJSControllerRegistry.Instance.RegisterClass('UploadFile', TKExtUploadFileController);

finalization
  TJSControllerRegistry.Instance.UnregisterClass('DownloadFile');
  TJSControllerRegistry.Instance.UnregisterClass('UploadFile');

end.
