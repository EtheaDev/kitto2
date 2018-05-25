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

unit Kitto.Ext.Editors.Files;

interface

uses
  Classes
  , Web.HTTPApp
  , EF.Tree
  , Ext.Base
  , Ext.Form
  , Kitto.JS.Base
  , Kitto.Metadata.DataView
  , Kitto.Ext.Base
  , Kitto.Ext.Editors
  ;

type
  TKExtFormFileUploadField = class(TExtFormFileField, IKExtEditItem, IKExtEditor)
  private
    FFieldName: string;
    FRecordField: TKViewTableField;
    FAdditionalWidth: Integer;
  public
    function AsObject: TObject; inline;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure SetOption(const ANode: TEFNode);
    function AsExtObject: TExtObject; inline;
    function AsExtFormField: TExtFormField; inline;
    function GetRecordField: TKViewTableField;
    procedure SetRecordField(const AValue: TKViewTableField);
    function GetFieldName: string;
    procedure SetFieldName(const AValue: string);
    procedure RefreshValue;
    procedure StoreValue(const AObjectName: string);
    procedure SetTransientProperty(const APropertyName: string; const AValue: Variant);
    function GetEditItemId: string;
    procedure SetReadOnly(const AValue: Boolean);
  end;

  TKExtFormFileEditor = class(TExtFormFieldContainer, IKExtEditItem, IKExtEditor)
  strict private
    FDescriptionField: TExtFormTextField;
    FDownloadButton: TKExtButton;
    FIsReadOnly: Boolean;
    FClearButton: TKExtButton;
    FTotalCharWidth: Integer;
    FPictureView: TExtPanel;
    FUploadButton: TKExtButton;
    FUploadFileDialog: TExtPanel;
    const EMPTY_DESCRIPTION = 'Empty';
    function GetContentDescription: string;
    procedure UpdateGUI(const AUpdatePicture: Boolean);
    procedure PictureViewAfterRender(This: TExtComponent);
  private // friends
    FAdditionalWidth: Integer;
  strict protected
    FFieldName: string;
    FRecordField: TKViewTableField;
    FLastUploadedClientFileName: string;
    FImageWidth: Integer;
    FImageHeight: Integer;
    function GetCurrentContentSize: Integer; virtual; abstract;
    procedure ProcessUploadedFile(const AFile: TAbstractWebRequestFile); virtual;
    procedure ClearContents; virtual;
    function IsEmpty: Boolean;
    function IsPicture: Boolean;

    function GetDownloadFileName: string;
    function GetStoredFileName: string;
    function GetDefaultFileName: string;
    function GetDownloadFileExtFromFieldData: string; virtual; abstract;
    procedure DoDownloadFieldData(const AFileName: string); virtual; abstract;
    function CreateImageDataStream: TStream; virtual; abstract;
  protected
    procedure CreateGUI(const AViewField: TKViewField);
    function GetObjectNamePrefix: string; override;
  public
    function AsObject: TObject; inline;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure SetOption(const ANode: TEFNode);
    function AsExtObject: TExtObject; inline;
    function AsExtFormField: TExtFormField; inline;
    function GetRecordField: TKViewTableField;
    procedure SetRecordField(const AValue: TKViewTableField);
    function GetFieldName: string;
    procedure SetFieldName(const AValue: string);
    procedure RefreshValue;
    procedure StoreValue(const AObjectName: string);
    property TotalCharWidth: Integer read FTotalCharWidth write FTotalCharWidth;
    procedure SetTransientProperty(const APropertyName: string; const AValue: Variant);
    function GetEditItemId: string;
    procedure SetReadOnly(const AValue: Boolean);
  //published
    procedure ShowUploadFileDialog;
    procedure Upload;
    procedure Clear;
    procedure PostUpload;
    procedure StartDownload;
    procedure DownloadFieldData;
    procedure GetImageData;
    procedure GetImageMarkup;
  end;

  TKExtFormFileReferenceEditor = class(TKExtFormFileEditor)
  strict private
    function GetFieldPath: string;
    function GetServerFileName: string;
    procedure DeferredFileDelete(const AEvent: TKFileOpEvent; const APathName: string);
  strict protected
    procedure ClearContents; override;
    procedure ProcessUploadedFile(const AFile: TAbstractWebRequestFile); override;
    function GetCurrentContentSize: Integer; override;
    function GetDownloadFileExtFromFieldData: string; override;
    procedure DoDownloadFieldData(const AFileName: string); override;
    function CreateImageDataStream: TStream; override;
  end;

  TKExtFormFileBlobEditor = class(TKExtFormFileEditor)
  strict protected
    procedure ClearContents; override;
    procedure ProcessUploadedFile(const AFile: TAbstractWebRequestFile); override;
    function GetCurrentContentSize: Integer; override;
    function GetDownloadFileExtFromFieldData: string; override;
    procedure DoDownloadFieldData(const AFileName: string); override;
    function CreateImageDataStream: TStream; override;
  end;

function TryCreateFileEditor(
  const AOwner: TJSBase; const AViewField: TKViewField;
  const ARowField: TKExtFormRowField; const AFieldCharWidth: Integer;
  const AFieldAdditionalWidth: Integer; const AIsReadOnly: Boolean;
  const ALabel: string): IKExtEditor;

implementation

uses
  SysUtils
  , StrUtils
  , JSON
  , IOUtils
  , EF.StrUtils
  , EF.JSON
  , EF.Localization
  , EF.Sys
  , Kitto.Ext.Utils
  , Kitto.Web.Request
  , Kitto.Web.Response
  , Kitto.Web.Application
  ;

function TryCreateFileEditor(
  const AOwner: TJSBase; const AViewField: TKViewField;
  const ARowField: TKExtFormRowField; const AFieldCharWidth: Integer;
  const AFieldAdditionalWidth: Integer; const AIsReadOnly: Boolean;
  const ALabel: string): IKExtEditor;
var
  LFileEditor: TKExtFormFileEditor;
begin
  Assert(Assigned(AOwner));

  if (AViewField.DataType is TEFBlobDataType) or (AViewField.DataType is TKFileReferenceDataType) then
  begin
    if AViewField.DataType is TEFBlobDataType then
      LFileEditor := TKExtFormFileBlobEditor.Create(AOwner)
    else
      LFileEditor := TKExtFormFileReferenceEditor.Create(AOwner);
    try
      LFileEditor.FAdditionalWidth := AFieldAdditionalWidth;
      LFileEditor.SetReadOnly(AIsReadOnly);
      LFileEditor.FieldLabel := ALabel;
      LFileEditor.TotalCharWidth := AFieldCharWidth - 1;
      if Assigned(ARowField) then
        ARowField.SetCharWidth(AFieldCharWidth, AFieldAdditionalWidth);
      LFileEditor.CreateGUI(AViewField);
      Result := LFileEditor;
    except
      LFileEditor.Free;
      raise;
    end;
  end
  else
    Result := nil;
end;

{ TKExtFormFileUploadField }

function TKExtFormFileUploadField.AsExtFormField: TExtFormField;
begin
  Result := Self;
end;

function TKExtFormFileUploadField.AsExtObject: TExtObject;
begin
  Result := Self;
end;

function TKExtFormFileUploadField.AsObject: TObject;
begin
  Result := Self;
end;

function TKExtFormFileUploadField.GetFieldName: string;
begin
  Result := FFieldName;
end;

function TKExtFormFileUploadField.GetEditItemId: string;
begin
  Result := FRecordField.FieldName;
end;

function TKExtFormFileUploadField.GetRecordField: TKViewTableField;
begin
  Result := FRecordField;
end;

procedure TKExtFormFileUploadField.RefreshValue;
begin
  SetValue(JSONNullToEmptyStr(FRecordField.GetAsJSONValue(False, False)));
end;

procedure TKExtFormFileUploadField.SetReadOnly(const AValue: Boolean);
begin
  ReadOnly := AValue;
end;

procedure TKExtFormFileUploadField.SetRecordField(const AValue: TKViewTableField);
begin
  FRecordField := AValue;
end;

procedure TKExtFormFileUploadField.SetTransientProperty(const APropertyName: string; const AValue: Variant);
begin
  AsExtFormField.SetTransientProperty(APropertyName, AValue);
end;

procedure TKExtFormFileUploadField.StoreValue(const AObjectName: string);
begin
  AsExtFormField.StoreValue(AObjectName);
end;

procedure TKExtFormFileUploadField.SetFieldName(const AValue: string);
begin
  FFieldName := AValue;
end;

procedure TKExtFormFileUploadField.SetOption(const ANode: TEFNode);
begin
  if not SetExtFormFieldOption(AsExtFormField, ANode, FAdditionalWidth) then
    TKExtEditorManager.InvalidOption(ANode);
end;

function TKExtFormFileUploadField._AddRef: Integer;
begin
  Result := 0;
end;

function TKExtFormFileUploadField._Release: Integer;
begin
  Result := 0;
end;

{ TKExtFormFileEditor }

function TKExtFormFileEditor.AsExtFormField: TExtFormField;
begin
  Result := nil;
end;

function TKExtFormFileEditor.AsExtObject: TExtObject;
begin
  Result := Self;
end;

function TKExtFormFileEditor.AsObject: TObject;
begin
  Result := Self;
end;

procedure TKExtFormFileEditor.Clear;
begin
  ClearContents;
  UpdateGUI(True);
end;

procedure TKExtFormFileEditor.ClearContents;
var
  LFileNameField: string;
  LFileNameFieldReference: TKViewTableField;
begin
  FLastUploadedClientFileName := '';

  LFileNameField := FRecordField.ViewField.FileNameField;
  if LFileNameField <> ''then
  begin
    LFileNameFieldReference := FRecordField.ParentRecord.FieldByName(LFileNameField);
    // Must clear the field both now, to have an exact picture in real time, and later
    // (through the SetToNull directive), when the record is persisted.
    //LFileNameFieldReference.SetBoolean('Sys/SetToNull', True);
    LFileNameFieldReference.SetToNull;
  end;
end;

procedure TKExtFormFileEditor.DownloadFieldData;
begin
  inherited;
  DoDownloadFieldData(GetDownloadFileName);
end;

procedure TKExtFormFileEditor.ProcessUploadedFile(const AFile: TAbstractWebRequestFile);
var
  LFileNameField: string;
  LFileNameFieldReference: TKViewTableField;
begin
  FLastUploadedClientFileName := AFile.FileName;
  LFileNameField := FRecordField.ViewField.FileNameField;
  if LFileNameField <> '' then
  begin
    LFileNameFieldReference := FRecordField.ParentRecord.FieldByName(LFileNameField);
    //LFileNameFieldReference.DeleteNode('Sys/SetToNull');
    LFileNameFieldReference.AsString := AFile.FileName;
  end;
end;

function TKExtFormFileEditor.GetRecordField: TKViewTableField;
begin
  Result := FRecordField;
end;

function TKExtFormFileEditor.GetStoredFileName: string;
var
  LFileNameField: string;
begin
  Assert(Assigned(FRecordField));

  LFileNameField := FRecordField.ViewField.FileNameField;
  if LFileNameField <> '' then
    Result := FRecordField.ParentRecord.FieldByName(LFileNameField).AsString
  else
    Result := '';
end;

function TKExtFormFileEditor.GetDefaultFileName: string;
var
  LCaptionField: TKViewTableField;
begin
  Assert(Assigned(FRecordField));

  Result := FRecordField.ViewField.GetExpandedString('DefaultFileName');
  if Result = '' then
  begin
    LCaptionField := FRecordField.ParentRecord.FindField(FRecordField.ViewField.ModelField.Model.CaptionField.FieldName);
    if Assigned(LCaptionField) then
      Result := LCaptionField.AsString + GetDownloadFileExtFromFieldData
    else
      Result := FRecordField.FieldName + GetDownloadFileExtFromFieldData;
  end;
end;

function TKExtFormFileEditor.GetDownloadFileName: string;
begin
  if FLastUploadedClientFileName <> '' then
    Result := ExtractFileName(FLastUploadedClientFileName)
  else
  begin
    Result := GetStoredFileName;
    if Result = '' then
      Result := GetDefaultFileName;
  end;
end;

function TKExtFormFileEditor.IsEmpty: Boolean;
begin
  Result := GetCurrentContentSize = 0;
end;

function TKExtFormFileEditor.IsPicture: Boolean;
begin
  Assert(Assigned(FRecordField));

  Result := FRecordField.ViewField.IsPicture;
end;

procedure TKExtFormFileEditor.RefreshValue;
begin
end;

procedure TKExtFormFileEditor.PictureViewAfterRender(This: TExtComponent);
begin
  Assert(Assigned(FPictureView));

  TKWebResponse.Current.Items.ExecuteJSCode(FPictureView.JSName + '.getLoader().load()');
end;

procedure TKExtFormFileEditor.CreateGUI(const AViewField: TKViewField);
var
  LPanel: TExtPanel;
  LToolbar: TKExtToolbar;
  LButtonCount: Integer;
  LIsPicture: Boolean;
begin
  LIsPicture := AViewField.IsPicture;

  LPanel := TExtPanel.CreateInlineAndAddToArray(Items);
  FImageWidth := AViewField.GetInteger('IsPicture/Thumbnail/Width', 100);
  FImageHeight := AViewField.GetInteger('IsPicture/Thumbnail/Height', 100);

  if LIsPicture then
  begin
    LPanel.Layout := lyColumn;
    //LPanel.Frame := True;
    FPictureView := TExtPanel.CreateAndAddToArray(LPanel.Items);
    FPictureView.Frame := True;
    FPictureView.Border := False;
    FPictureView.Loader.SetConfigItem('url', GetMethodURL(GetImageMarkup));
    FPictureView.AfterRender := PictureViewAfterRender;

    LToolbar := TKExtToolbar.CreateAndAddToArray(LPanel.Items);
    // Version below puts the toolbar at the bottom (in which case we should adjust the height as well)
    //LToolbar := TKExtToolbar.Create;
    //FPictureView.Bbar := LToolbar;
  end
  else
  begin
    LPanel.Layout := lyHbox;
    FDescriptionField := TExtFormTextField.CreateAndAddToArray(LPanel.Items);
    FDescriptionField.ReadOnly := True;
    FDescriptionField.Cls := 'x-form-readonly';

    LToolbar := TKExtToolbar.CreateInlineAndAddToArray(LPanel.Items);
  end;

  LToolbar.Style := 'background: none; border: none;';

  FDownloadButton := TKExtButton.CreateAndAddToArray(LToolbar.Items);
  FDownloadButton.SetIconAndScale('download');
  FDownloadButton.Tooltip := _('Download file');
  //FDownloadButton.Handler := Ajax(StartDownload);
  FDownloadButton.Handler := TKWebResponse.Current.Items.AjaxCallMethod(Self).SetMethod(StartDownload).AsFunction;

  LButtonCount := 1;
  FUploadButton := TKExtButton.CreateAndAddToArray(LToolbar.Items);
  FUploadButton.SetIconAndScale('upload');
  FUploadButton.Tooltip := _('Upload file');
  //FUploadButton.Handler := Ajax(ShowUploadFileDialog);
  FUploadButton.Handler := TKWebResponse.Current.Items.AjaxCallMethod(Self).SetMethod(ShowUploadFileDialog).AsFunction;
  if FIsReadOnly then
    FUploadButton.SetDisabled(True);
  Inc(LButtonCount);

  FClearButton := TKExtButton.CreateAndAddToArray(LToolbar.Items);
  FClearButton.SetIconAndScale('clear');
  FClearButton.Tooltip := _('Clear field');
  //FClearButton.Handler := Ajax(Clear);
  FClearButton.Handler := TKWebResponse.Current.Items.AjaxCallMethod(Self).SetMethod(Clear).AsFunction;
  if FIsReadOnly then
    FClearButton.SetDisabled(True);
  Inc(LButtonCount);

  if Assigned(FDescriptionField) then
    // Keep 3 characters per button, leave the rest to the text field.
    FDescriptionField.WidthExpression := CharsToPixels(FTotalCharWidth - (3 * LButtonCount), FAdditionalWidth)
  else if Assigned(FPictureView) then
  begin
    if FPictureView.Frame then
    begin
      FPictureView.Width := FImageWidth + 10;
      FPictureView.Height := FImageHeight + 10;
    end
    else
    begin
      FPictureView.Width := FImageWidth;
      FPictureView.Height := FImageHeight;
    end;
    Width := FPictureView.Width + (22 * LButtonCount);
  end;
end;

procedure TKExtFormFileEditor.SetReadOnly(const AValue: Boolean);
begin
  FIsReadOnly := AValue;
  UpdateGUI(False);
end;

procedure TKExtFormFileEditor.SetRecordField(const AValue: TKViewTableField);
begin
  FRecordField := AValue;
  UpdateGUI(False);
end;

procedure TKExtFormFileEditor.SetTransientProperty(const APropertyName: string; const AValue: Variant);
begin
  SetExtComponentTransientProperty(Self, APropertyName, AValue);
end;

procedure TKExtFormFileEditor.GetImageMarkup;
begin
  Assert(Assigned(FRecordField));

  if FRecordField.IsNull then
    TKWebResponse.Current.Items.AddHTML('<p>' + _(EMPTY_DESCRIPTION) + '</p>')
  else
    // Add dummy parameter to the URL to force the browser to refresh the image
    // after an upload.
    TKWebResponse.Current.Items.AddHTML(Format('<img src="%s">',
      [GetMethodURL(GetImageData) + '?_dc=' + FormatDateTime('yyyymmddhhnnsszzz', Now())]));
end;

function TKExtFormFileEditor.GetObjectNamePrefix: string;
begin
  Result := 'fileed';
end;

procedure TKExtFormFileEditor.UpdateGUI(const AUpdatePicture: Boolean);
var
  LIsEmpty: Boolean;
begin
  // Only if GUI already created.
  if Items.Count > 0 then
  begin
    LIsEmpty := IsEmpty;
    if Assigned(FDescriptionField) then
      FDescriptionField.Value := GetContentDescription;
    if AUpdatePicture and Assigned(FPictureView) then
      PictureViewAfterRender(FPictureView);
    FDownloadButton.SetDisabled(LIsEmpty);
    FClearButton.SetDisabled(LIsEmpty or FIsReadOnly);
    FUploadButton.SetDisabled(FIsReadOnly);
  end;
end;

procedure TKExtFormFileEditor.SetFieldName(const AValue: string);
begin
  FFieldName := AValue;
end;

procedure TKExtFormFileEditor.SetOption(const ANode: TEFNode);
begin
  if SameText(ANode.Name, 'CharWidth') then
    TotalCharWidth := ANode.AsInteger
  else if SameText(ANode.Name, 'Anchor') then
    Anchor := ANode.AsString
  else
    TKExtEditorManager.InvalidOption(ANode);
end;

procedure TKExtFormFileEditor.ShowUploadFileDialog;
var
  LUploadButton: TKExtButton;
  LSubmitAction: TExtFormActionSubmit;
  LUploadFormField: TKExtFormFileUploadField;
  LToolbar: TKExtToolbar;
  LCancelButton: TKExtButton;
  LFormPanel: TExtFormFormPanel;
begin
  FreeAndNil(FUploadFileDialog);
  FUploadFileDialog := TExtPanel.Create(Self);
  FUploadFileDialog.Title := _('File upload');
  FUploadFileDialog.Width := 550;
  FUploadFileDialog.Height := 150;

  LFormPanel := TExtFormFormPanel.CreateAndAddToArray(FUploadFileDialog.Items);
  LFormPanel.FileUpload := True;
  LFormPanel.LabelAlign := laRight;
  LFormPanel.LabelWidth := 50;
  LFormPanel.Padding := '20px 10px 0 10px'; // top right bottom left
  LFormPanel.Border := False;

  LUploadFormField := TKExtFormFileUploadField.CreateInlineAndAddToArray(LFormPanel.Items);
  LUploadFormField.FieldLabel := _(FRecordField.ViewField.DisplayLabel);
  LUploadFormField.EmptyText := _('Select a file to upload');
  LUploadFormField.AllowBlank := False;
  LUploadFormField.Anchor := '0 5 0 0';
  LToolbar := TKExtToolbar.Create(Self);
  TExtToolbarFill.CreateInlineAndAddToArray(LToolbar.Items);
  FUploadFileDialog.Fbar := LToolbar;

  LUploadButton := TKExtButton.CreateInlineAndAddToArray(LToolbar.Items);
  LUploadButton.Text := _('Upload');
  LUploadButton.SetIconAndScale('Upload', IfThen(TKWebRequest.Current.IsMobileBrowser, 'medium', 'small'));

  LCancelButton := TKExtButton.CreateInlineAndAddToArray(LToolbar.Items);
  LCancelButton.Text := _('Cancel');
  LCancelButton.SetIconAndScale('Cancel', IfThen(TKWebRequest.Current.IsMobileBrowser, 'medium', 'small'));
  LCancelButton.Handler := GenerateAnonymousFunction(FUploadFileDialog.Close);

  LSubmitAction := TExtFormActionSubmit.CreateInline(LFormPanel);
  LSubmitAction.Url := GetMethodURL(Upload);
  LSubmitAction.WaitMsg := _('File upload in progress...');
  LSubmitAction.WaitTitle := _('Please wait...');
  //LSubmitAction.Success := Ajax(PostUpload);
  LSubmitAction.Success := TKWebResponse.Current.Items.AjaxCallMethod(Self).SetMethod(PostUpload).AsFunction;
  { TODO : find a way to substitute action.result.msg }
  LSubmitAction.Failure := GenerateAnonymousFunction('form, action', ExtMessageBox.Alert(_('File upload error'), 'action.result.msg'));

  LUploadButton.Handler := GenerateAnonymousFunction(Format(
    'if (%s.isValid()) %s.submit({%s});',
    [LFormPanel.JSName,
     LFormPanel.JSName,
     LSubmitAction.JSConfig.AsFormattedText]));
  FUploadFileDialog.ShowModal;
end;

procedure TKExtFormFileEditor.StartDownload;
begin
  Download(DownloadFieldData);
end;

procedure TKExtFormFileEditor.StoreValue(const AObjectName: string);
var
  LMsg: string;
begin
  Assert(Assigned(FRecordField));

  if FRecordField.ViewField.IsRequired then
  begin
    LMsg := Format(_('Field %s is required. Please upload a file.'), [FRecordField.ViewField.DisplayLabel]);
    if Assigned(FDescriptionField) then
      TKWebResponse.Current.Items.ExecuteJSCode(Self,
        Format('if (%s.getValue() == "%s") { alert("%s"); throw "validation error"; }',
        [FDescriptionField.JSName, _(EMPTY_DESCRIPTION), LMsg]))
    else if Assigned(FPictureView) then
      TKWebResponse.Current.Items.ExecuteJSCode(Self,
        Format('if (%s.html.indexOf("<img" = -1) { alert("%s"); throw "validation error"; }', [FDescriptionField.JSName, LMsg]));
  end;
end;

procedure TKExtFormFileEditor.Upload;
var
  LResult: TJSONObject;
  LMaxUploadSize: Integer;
begin
  LResult := TJSONObject.Create;
  try
    if TKWebRequest.Current.Files.Count = 0 then
    begin
      LResult.AddPair('success', TJSONFalse.Create);
      LResult.AddPair('msg', _('No file uploaded'));
    end
    else
    begin
      LMaxUploadSize := FRecordField.ViewField.GetInteger('MaxUploadSize', MaxInt);
      if TKWebRequest.Current.Files[0].Stream.Size > LMaxUploadSize then
      begin
        LResult.AddPair('success', TJSONFalse.Create);
        LResult.AddPair('msg', Format(_('File too large. Maximum size is %s.'),
          [FormatByteSize(LMaxUploadSize, TKWebApplication.Current.Config.UserFormatSettings)]));
      end
      else
      begin
        try
          ProcessUploadedFile(TKWebRequest.Current.Files[0]);
          LResult.AddPair('success', TJSONTrue.Create);
        except
          on E: Exception do
          begin
            LResult.AddPair('success', TJSONFalse.Create);
            LResult.AddPair('msg', E.Message);
          end;
        end;
      end;
    end;
  finally
    TKWebResponse.Current.Items.AddJSON(LResult.ToJSON);
    FreeAndNil(LResult);
  end;
end;

function TKExtFormFileEditor.GetContentDescription: string;
var
  LFileName: string;
begin
  LFileName := ExtractFileName(GetDownloadFileName);
  if LFileName <> '' then
    Result := Format(_('%s file (%s)'),
      [StripPrefix(ExtractFileExt(LFileName), '.'),
      FormatByteSize(GetCurrentContentSize, TKWebApplication.Current.Config.UserFormatSettings)])
  else
    Result := _(EMPTY_DESCRIPTION);
end;

function TKExtFormFileEditor.GetFieldName: string;
begin
  Result := FFieldName;
end;

function TKExtFormFileEditor.GetEditItemId: string;
begin
  Result := FRecordField.FieldName;
end;

procedure TKExtFormFileEditor.GetImageData;
var
  LDownloadFileName: string;
  LImageDataStream: TStream;
begin
  LImageDataStream := CreateImageDataStream;
  LDownloadFileName := GetDownloadFileName;
  DownloadThumbnailedStream(LImageDataStream, LDownloadFileName, FImageWidth, FImageHeight);
end;

procedure TKExtFormFileEditor.PostUpload;
begin
  FUploadFileDialog.Close;
  UpdateGUI(True);
end;

function TKExtFormFileEditor._AddRef: Integer;
begin
  Result := 0;
end;

function TKExtFormFileEditor._Release: Integer;
begin
  Result := 0;
end;

{ TKExtFormFileBlobEditor }

function TKExtFormFileBlobEditor.GetDownloadFileExtFromFieldData: string;
begin
  Assert(Assigned(FRecordField));

  Result := '.' + GetDataType(FRecordField.AsBytes, 'dat');
end;

procedure TKExtFormFileBlobEditor.ClearContents;
begin
  inherited;
  FRecordField.SetToNull;
end;

function TKExtFormFileBlobEditor.CreateImageDataStream: TStream;
begin
  Assert(Assigned(FRecordField));
  Assert(not FRecordField.IsNull);

  Result := TBytesStream.Create(FRecordField.AsBytes);
end;

procedure TKExtFormFileBlobEditor.DoDownloadFieldData(const AFileName: string);
begin
  Assert(Assigned(FRecordField));

  TKWebApplication.Current.DownloadBytes(FRecordField.AsBytes, AFileName, '', False);
end;

procedure TKExtFormFileBlobEditor.ProcessUploadedFile(const AFile: TAbstractWebRequestFile);
begin
  inherited;
  FRecordField.LoadBytesFromStream(AFile.Stream);
end;

function TKExtFormFileBlobEditor.GetCurrentContentSize: Integer;
begin
  if FRecordField.IsNull then
    Result := 0
  else
    Result := Length(FRecordField.AsBytes);
end;

{ TKExtFormFileReferenceEditor }

function TKExtFormFileReferenceEditor.GetFieldPath: string;
begin
  inherited;
  Result := IncludeTrailingPathDelimiter(FRecordField.ViewField.GetExpandedString('Path'));
  if (Result = '') or (Result = PathDelim) then
    raise Exception.CreateFmt('Path not specified for file reference field %s.', [FRecordField.ViewField.FieldName]);
  if not DirectoryExists(Result) then
    raise Exception.CreateFmt('Directory %s not found for file reference field %s.', [Result, FRecordField.ViewField.FieldName]);
end;

function TKExtFormFileReferenceEditor.GetServerFileName: string;
begin
  Assert(Assigned(FRecordField));

  if FRecordField.AsString <> '' then
    Result := TPath.Combine(GetFieldPath, FRecordField.AsString)
  else
    Result := '';
end;

procedure TKExtFormFileReferenceEditor.ClearContents;
begin
  inherited;
  if FRecordField.AsString <> '' then
  begin
    DeferredFileDelete(oePost, GetServerFileName);
    FRecordField.SetToNull;
  end;
end;

procedure TKExtFormFileReferenceEditor.DoDownloadFieldData(const AFileName: string);
var
  LServerFileName: string;
begin
  Assert(Assigned(FRecordField));

  LServerFileName := GetServerFileName;
  Assert(FileExists(LServerFileName));

  TKWebApplication.Current.DownloadFile(LServerFileName, AFileName, '', False);
end;

function TKExtFormFileReferenceEditor.CreateImageDataStream: TStream;
var
  LServerFileName: string;
begin
  Assert(Assigned(FRecordField));

  LServerFileName := GetServerFileName;
  Assert(FileExists(LServerFileName));

  Result := TFileStream.Create(LServerFileName, fmOpenRead or fmShareDenyNone);
end;

procedure TKExtFormFileReferenceEditor.DeferredFileDelete(const AEvent: TKFileOpEvent; const APathName: string);
var
  LNode: TEFNode;
begin
  Assert(Assigned(FRecordField));

  LNode := FRecordField.GetNode('DeferredFileOps', True);
  LNode.AddChild('Item').AsStringArray := TKFileOp.Create(okDelete, APathName, AEvent);
end;

procedure TKExtFormFileReferenceEditor.ProcessUploadedFile(const AFile: TAbstractWebRequestFile);
var
  LCurrentFileName: string;
  LNewFileName: string;
begin
  inherited;
  LCurrentFileName := GetServerFileName;
  if FileExists(LCurrentFileName) then
    DeferredFileDelete(oePost, LCurrentFileName);

  // Save data to file with auto-generated name.
  LNewFileName := GetUniqueFileName(GetFieldPath, ExtractFileExt(AFile.FileName));
  StreamToFile(AFile.Stream, LNewFileName);

  FRecordField.AsString := ExtractFileName(LNewFileName);
  DeferredFileDelete(oeCancel, LNewFileName);
end;

function TKExtFormFileReferenceEditor.GetCurrentContentSize: Integer;
var
  LFileName: string;
begin
  LFileName := GetServerFileName;
  if FileExists(LFileName) then
    Result := GetFileSize(LFileName)
  else
    Result := 0;
end;

function TKExtFormFileReferenceEditor.GetDownloadFileExtFromFieldData: string;
begin
  Assert(Assigned(FRecordField));

  Result := ExtractFileExt(FRecordField.AsString);
  if Result = '' then
    Result := '.dat';
end;

end.
