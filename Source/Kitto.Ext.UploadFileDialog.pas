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

unit Kitto.Ext.UploadFileDialog;

interface

uses
  Kitto.JS.Base
  , Kitto.JS
  , Kitto.Ext.Base
  ;

function ShowUploadFileDialog(const AOwner: TJSObject; const AUploadFieldLabel: string;
  const AUploadURL: string; const AOnSubmitActionSuccess: TJSExpression; const AWildcards: string = ''): TKExtPanelBase;

implementation

uses
  SysUtils
  , StrUtils
  , EF.Localization
  , Ext.Base
  , Ext.Form
  , Kitto.Config.Defaults
  , Kitto.Web.Request
  ;

function ShowUploadFileDialog(const AOwner: TJSObject; const AUploadFieldLabel: string;
  const AUploadURL: string; const AOnSubmitActionSuccess: TJSExpression; const AWildcards: string = ''): TKExtPanelBase;
var
  LUploadButton: TKExtButton;
  LFormPanel: TExtFormFormPanel;
  LSubmitAction: TExtFormActionSubmit;
  LUploadFormField: TExtFormFileField;
  LToolbar: TKExtToolbar;
  LCancelButton: TKExtButton;
begin
  Result := TKExtPanelBase.Create(AOwner);
  Result.Title := _('File upload');
  Result.Width := 550;
  Result.Height := 150;

  LFormPanel := TExtFormFormPanel.CreateAndAddToArray(Result.Items);
  LFormPanel.FileUpload := True;
  LFormPanel.LabelAlign := laRight;
  LFormPanel.LabelWidth := 50;
  LFormPanel.Padding := Format('%1:dpx %0:dpx 0 %0:dpx', [TKDefaults.GetSingleSpacing, TKDefaults.GetDoubleSpacing]); // top right bottom left
  LFormPanel.Border := False;

  LUploadFormField := TExtFormFileField.CreateInlineAndAddToArray(LFormPanel.Items);
  LUploadFormField.FieldLabel := AUploadFieldLabel;
  if AWildcards <> '' then
    LUploadFormField.EmptyText := Format(_('File matching %s'), [AWildcards])
  else
    LUploadFormField.EmptyText := _('Select a file to upload');
  LUploadFormField.AllowBlank := False;
  LUploadFormField.Anchor := '0 5 0 0';
  LToolbar := TKExtToolbar.Create(Result);
  TExtToolbarFill.CreateInlineAndAddToArray(LToolbar.Items);
  Result.Fbar := LToolbar;

  LUploadButton := TKExtButton.CreateInlineAndAddToArray(LToolbar.Items);
  LUploadButton.Text := _('Upload');
  LUploadButton.SetIconAndScale('Upload', IfThen(TKWebRequest.Current.IsMobileBrowser,'medium', 'small'));

  LCancelButton := TKExtButton.CreateInlineAndAddToArray(LToolbar.Items);
  LCancelButton.Text := _('Cancel');
  LCancelButton.SetIconAndScale('Cancel', IfThen(TKWebRequest.Current.IsMobileBrowser, 'medium', 'small'));
  LCancelButton.Handler := Result.GenerateAnonymousFunction(Result.Close);

  LSubmitAction := TExtFormActionSubmit.CreateInline(LFormPanel);
  LSubmitAction.Url := AUploadURL;
  LSubmitAction.WaitMsg := _('File upload in progress...');
  LSubmitAction.WaitTitle := _('Please wait...');
  //LSubmitAction.Success := Ajax(PostUpload);
  LSubmitAction.Success := AOnSubmitActionSuccess;
  { TODO : find a way to substitute action.result.msg }
  LSubmitAction.Failure := Result.GenerateAnonymousFunction('form, action', ExtMessageBox.Alert(_('File upload error'), 'action.result.msg'));

  LUploadButton.Handler := Result.GenerateAnonymousFunction(Format(
    'if (%s.isValid()) %s.submit({%s});',
    [LFormPanel.JSName, LFormPanel.JSName, LSubmitAction.JSConfig.AsFormattedText]));
  Result.ShowFloating(True);
end;

end.
