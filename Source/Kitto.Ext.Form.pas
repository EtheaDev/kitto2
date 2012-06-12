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

unit Kitto.Ext.Form;

{$I Kitto.Defines.inc}

interface

uses
  Generics.Collections,
  Ext, ExtData, ExtForm,
  EF.ObserverIntf,
  Kitto.Metadata.Views, Kitto.Metadata.DataView, Kitto.Store,
  Kitto.Ext.Controller, Kitto.Ext.Base, Kitto.Ext.DataPanel, Kitto.Ext.Editors,
  Kitto.Ext.GridPanel;

type
  ///	<summary>
  ///	  A button that opens a popup detail form.
  ///	</summary>
  TKExtDetailFormButton = class(TExtButton)
  private
    FViewTable: TKViewTable;
    FDetailHostWindow: TKExtModalWindow;
    FServerStore: TKViewTableStore;
    procedure SetViewTable(const AValue: TKViewTable);
  public
    destructor Destroy; override;
    property ViewTable: TKViewTable read FViewTable write SetViewTable;
    property ServerStore: TKViewTableStore read FServerStore write FServerStore;
  published
    procedure ShowDetailWindow;
  end;

  ///	<summary>
  ///	  The Form controller.
  ///	</summary>
  TKExtFormPanelController = class(TKExtDataPanelController)
  strict private
    FTabPanel: TExtTabPanel;
    FFormPanel: TKExtEditPanel;
    FIsReadOnly: Boolean;
    FSaveButton: TExtButton;
    FCancelButton: TExtButton;
    FDetailToolbar: TExtToolbar;
    FDetailButtons: TObjectList<TKExtDetailFormButton>;
    FDetailControllers: TObjectList<TObject>;
    FOperation: string;
    FFocusField: TExtFormField;
    FStoreRecord: TKViewTableRecord;
    FEditors: TList<TObject>;
    procedure CreateEditors(const AForceReadOnly: Boolean);
    procedure StartOperation;
    procedure FocusFirstField;
    procedure CreateDetailPanels;
    procedure CreateDetailToolbar;
    function GetDetailStyle: string;
    procedure OnFieldChange(AField: TExtFormField; ANewValue, AOldValue: string);
    function FindEditor(const AFieldName: string): IKExtEditor;
  strict protected
    procedure DoDisplay; override;
    procedure InitComponents; override;
  public
    procedure LoadData(const AFilterExpression: string); override;
    destructor Destroy; override;
  published
    procedure GetRecord;
    procedure SaveChanges;
    procedure CancelChanges;
  end;

implementation

uses
  SysUtils, StrUtils, Classes,
  EF.Localization, EF.Types, EF.Intf, EF.Tree, EF.DB, EF.JSON,
  Kitto.AccessControl, Kitto.Rules, Kitto.SQL,
  Kitto.Ext.Session, Kitto.Ext.Utils;

{ TKExtFormPanelController }

destructor TKExtFormPanelController.Destroy;
begin
  FreeAndNil(FEditors);
  FreeAndNil(FDetailButtons);
  FreeAndNil(FDetailControllers);
  inherited;
end;

procedure TKExtFormPanelController.DoDisplay;
begin
  inherited;
  LoadData('');
end;

procedure TKExtFormPanelController.CreateDetailToolbar;
var
  I: Integer;
begin
  Assert(ViewTable <> nil);
  Assert(FDetailToolbar = nil);
  Assert(FDetailButtons = nil);
  Assert(Assigned(FStoreRecord));

  if ViewTable.DetailTableCount > 0 then
  begin
    FStoreRecord.EnsureDetailStores;
    Assert(FStoreRecord.DetailStoreCount = ViewTable.DetailTableCount);
    FDetailToolbar := TExtToolbar.Create;
    FDetailButtons := TObjectList<TKExtDetailFormButton>.Create(False);
    for I := 0 to ViewTable.DetailTableCount - 1 do
    begin
      FDetailButtons.Add(TKExtDetailFormButton.AddTo(FDetailToolbar.Items));
      FDetailButtons[I].ServerStore := FStoreRecord.DetailStores[I];
      FDetailButtons[I].ViewTable := ViewTable.DetailTables[I];
    end;
    Tbar := FDetailToolbar;
  end;
end;

procedure TKExtFormPanelController.CreateDetailPanels;
var
  I: Integer;
  LController: IKExtController;
  LControllerType: string;
begin
  Assert(ViewTable <> nil);
  Assert(FDetailControllers = nil);
  Assert(Assigned(FStoreRecord));

  if ViewTable.DetailTableCount > 0 then
  begin
    Assert(FTabPanel <> nil);
    FStoreRecord.EnsureDetailStores;
    Assert(FStoreRecord.DetailStoreCount = ViewTable.DetailTableCount);
    FDetailControllers := TObjectList<TObject>.Create(False);
    for I := 0 to ViewTable.DetailTableCount - 1 do
    begin
      LControllerType := ViewTable.GetString('Controller', 'GridPanel');
      // The node may exist and be '', which does not return the default value.
      if LControllerType = '' then
        LControllerType := 'GridPanel';
      LController := TKExtControllerFactory.Instance.CreateController(View,
        FTabPanel, ViewTable.FindNode('Controller'), Self, LControllerType);
      LController.Config.SetObject('Sys/ViewTable', ViewTable.DetailTables[I]);
      LController.Config.SetObject('Sys/ServerStore', FStoreRecord.DetailStores[I]);
      LController.Config.SetBoolean('AllowClose', False);
      FDetailControllers.Add(LController.AsObject);
      LController.Display;
      if (LController.AsObject is TKExtDataPanelController) then
        TKExtDataPanelController(LController.AsObject).LoadData('');
    end;
  end;
end;

procedure TKExtFormPanelController.CreateEditors(const AForceReadOnly: Boolean);
var
  LLayoutProcessor: TKExtLayoutProcessor;
  LLayoutName: string;
begin
  FreeAndNil(FEditors);
  FEditors := TList<TObject>.Create;
  LLayoutProcessor := TKExtLayoutProcessor.Create;
  try
    LLayoutProcessor.DataRecord := FStoreRecord;
    LLayoutProcessor.FormPanel := FFormPanel;
    LLayoutProcessor.OnFieldChange := OnFieldChange;
    LLayoutProcessor.OnNewEditor :=
      procedure (AEditor: IKExtEditor)
      begin
        FEditors.Add(AEditor.AsObject);
      end;
    LLayoutProcessor.ForceReadOnly := AForceReadOnly;
    if FOperation = 'Add' then
      LLayoutProcessor.Operation := eoInsert
    else
      LLayoutProcessor.Operation := eoUpdate;

    LLayoutName := ViewTable.GetString('Controller/Form/Layout');
    if LLayoutName <> '' then
      LLayoutProcessor.CreateEditors(View.Catalog.Layouts.FindLayout(LLayoutName))
    else
      LLayoutProcessor.CreateEditors(ViewTable.FindLayout('Form'));
    FFocusField := LLayoutProcessor.FocusField;
  finally
    FreeAndNil(LLayoutProcessor);
  end;
  // Scroll back to top - can't do that until afterrender because body.dom is needed.
  FFormPanel.On('afterrender', JSFunction(FFormPanel.JSName + '.body.dom.scrollTop = 0;'));
end;

function TKExtFormPanelController.GetDetailStyle: string;
begin
  Result := ViewTable.GetString('DetailTables/Controller/Style', 'Tabs');
end;

procedure TKExtFormPanelController.LoadData(const AFilterExpression: string);
var
  LDetailStyle: string;
begin
  CreateEditors(FIsReadOnly);
  LDetailStyle := GetDetailStyle;
  if SameText(LDetailStyle, 'Tabs') then
    CreateDetailPanels
  else if SameText(LDetailStyle, 'Popup') then
    CreateDetailToolbar;
  StartOperation;
end;

procedure TKExtFormPanelController.StartOperation;
var
  LDefaultValues: TEFNode;
begin
  Assert(Assigned(FStoreRecord));

  try
    if FOperation = 'Add' then
    begin
      LDefaultValues := ViewTable.GetDefaultValues;
      try
        FStoreRecord.ReadFromNode(LDefaultValues);
        FStoreRecord.ApplyNewRecordRules;
      finally
        FreeAndNil(LDefaultValues);
      end;
    end;

    // Load data from FServerRecord.
    Session.JSCode(
      FFormPanel.JSName + '.getForm().load({url:"' + MethodURI(GetRecord) + '",' +
        'failure: function(form, action) { Ext.Msg.alert("' + _('Load failed.') + '", action.result.errorMessage);}});');
    FocusFirstField;
  except
    on E: EKValidationError do
    begin
      ExtMessageBox.Alert(_(Session.Config.AppTitle), E.Message);
      CancelChanges;
    end;
  end;
end;

procedure TKExtFormPanelController.OnFieldChange(AField: TExtFormField;
  ANewValue, AOldValue: string);
var
  LEditor: IKExtEditor;
  LStore: TKStore;
  I: Integer;
  LDerivedEditor: IKExtEditor;
begin
  Assert(Assigned(AField));

  if Supports(AField, IKExtEditor, LEditor) then
  begin
    Assert(LEditor.GetRecordField.ViewField.IsReference);

    // Get derived values.
    LStore := LEditor.GetRecordField.ViewField.CreateDerivedFieldsStore(ANewValue);
    try
      // Copy values to editors.
      for I := 0 to LStore.Header.FieldCount - 1 do
      begin
        LDerivedEditor := FindEditor(LStore.Header.Fields[I].FieldName);
        if Assigned(LDerivedEditor) then
        begin
          if LStore.RecordCount > 0 then
            { TODO : need to format value? }
            LDerivedEditor.AsExtFormField.SetValue(LStore.Records[0].Fields[I].AsString)
          else
            LDerivedEditor.AsExtFormField.SetValue('');
        end;
      end;
    finally
      FreeAndNil(LStore);
    end;
  end;
end;

function TKExtFormPanelController.FindEditor(const AFieldName: string): IKExtEditor;
var
  I: Integer;
  LEditorIntf: IKExtEditor;
begin
  Result := nil;
  for I := 0 to FEditors.Count - 1 do
  begin
    if Supports(FEditors[I], IKExtEditor, LEditorIntf) then
    begin
      if SameText(LEditorIntf.GetRecordField.ViewField.AliasedName, AFieldName) then
      begin
        Result := LEditorIntf;
        Break;
      end;
    end;
  end;
end;

procedure TKExtFormPanelController.FocusFirstField;
begin
  if Assigned (FFocusField) then
    FFocusField.Focus(False, 500);
end;

procedure TKExtFormPanelController.GetRecord;
begin
  Assert(Assigned(FStoreRecord));

  Session.Response := '{success:true,data:' + FStoreRecord.GetAsJSON(False) + '}';
end;

procedure TKExtFormPanelController.SaveChanges;
begin
  try
    // Get POST values.
    FStoreRecord.SetChildValuesfromStrings(Session.Queries,
      False, Session.Config.UserFormatSettings,
      function(const AName: string): string
      var
        LViewField: TKViewField;
      begin
        LViewField := ViewTable.FindFieldByAliasedName(AName);
        if Assigned(LViewField) then
          Result := LViewField.AliasedName
        else
          Result := AName;
      end);
    // Get uploaded files.
    Session.EnumUploadedFiles(
      procedure (AFile: TKExtUploadedFile)
      var
        LFileNameField: string;
      begin
        if (AFile.Context is TKViewField) and (TKViewField(AFile.Context).Table = ViewTable) then
        begin
          if TKViewField(AFile.Context).DataType is TEFBlobDataType then
            FStoreRecord.FieldByName(TKViewField(AFile.Context).AliasedName).AsBytes := AFile.Bytes
          else if TKViewField(AFile.Context).DataType is TKFileReferenceDataType then
            FStoreRecord.FieldByName(TKViewField(AFile.Context).AliasedName).AsString := AFile.FileName
          else
            raise Exception.CreateFmt(_('Data type %s does not support file upload.'), [TKViewField(AFile.Context).DataType.GetTypeName]);
          LFileNameField := TKViewField(AFile.Context).FileNameField;
          if LFileNameField <> ''then
            FStoreRecord.FieldByName(LFileNameField).AsString := AFile.OriginalFileName;
        end;
      end);

    // Save record.
    FStoreRecord.MarkAsModified;
    FStoreRecord.ApplyBeforeRules;
    if not ViewTable.IsDetail then
    begin
      FStoreRecord.Save(True);
      Session.Flash(_('Changes saved succesfully.'));
    end;

  except
    on E: EKValidationError do
    begin
      ExtMessageBox.Alert(_(Session.Config.AppTitle), E.Message);
      Exit;
    end;
  end;

  NotifyObservers('Confirmed');
  if not CloseHostWindow then
    StartOperation;
end;

procedure TKExtFormPanelController.InitComponents;
var
  LHostWindow: TExtWindow;
begin
  inherited;
  if Title = '' then
    Title := _(ViewTable.DisplayLabel);

  FOperation := Config.GetString('Sys/Operation');
  if FOperation = '' then
    FOperation := Config.GetString('Operation');

  FStoreRecord := Config.GetObject('Sys/Record') as TKViewTableRecord;
  Assert((FOperation = 'Add') or Assigned(FStoreRecord));
  if FOperation = 'Add' then
  begin
    Assert(not Assigned(FStoreRecord));
    FStoreRecord := ServerStore.AppendRecord(nil);
  end;

  if SameText(FOperation, 'Add') then
    FIsReadOnly := ViewTable.GetBoolean('Controller/PreventAdding')
      or View.GetBoolean('IsReadOnly')
      or ViewTable.IsReadOnly
      or Config.GetBoolean('PreventAdding')
      or not ViewTable.IsAccessGranted(ACM_ADD)
  else
    FIsReadOnly := ViewTable.GetBoolean('Controller/PreventEditing')
      or View.GetBoolean('IsReadOnly')
      or ViewTable.IsReadOnly
      or Config.GetBoolean('PreventEditing')
      or not ViewTable.IsAccessGranted(ACM_MODIFY);
  if SameText(FOperation, 'Add') and FIsReadOnly then
    raise EEFError.Create(_('Operation Add not supported on read-only data.'));

  if (ViewTable.DetailTableCount > 0) and SameText(GetDetailStyle, 'Tabs') then
  begin
    FTabPanel := TExtTabPanel.AddTo(Items);
    FTabPanel.Border := False;
    FTabPanel.Region := rgCenter;
    FTabPanel.AutoScroll := False;
    FTabPanel.SetActiveTab(0);
    FFormPanel := TKExtEditPanel.AddTo(FTabPanel.Items);
    FFormPanel.Title := _(ViewTable.DisplayLabel);
  end
  else
  begin
    FTabPanel := nil;
    FFormPanel := TKExtEditPanel.AddTo(Items);
    FFormPanel.Region := rgCenter;
  end;
  FFormPanel.Border := False;
  FFormPanel.Header := False;
  FFormPanel.Frame := True;
  FFormPanel.AutoScroll := True;
  FFormPanel.AutoWidth := True;
  FFormPanel.LabelWidth := 120;
  FFormPanel.MonitorValid := True;
  //TExtFormBasicForm(FFormPanel.GetForm).Url := MethodURI(SaveChanges);

  if not FIsReadOnly then
  begin
    FSaveButton := TExtButton.AddTo(FFormPanel.Buttons);
    FSaveButton.Scale := Config.GetString('ButtonScale', 'medium');
    FSaveButton.FormBind := True;
    FSaveButton.Text := _('Save');
    FSaveButton.Tooltip := _('Save changes and finish editing');
    FSaveButton.Icon := Session.Config.GetImageURL('accept');
    FSaveButton.Handler := AjaxForms(SaveChanges, [FFormPanel]);
    //FSaveButton.Handler := JSFunction(FFormPanel.JSName + '.getForm().doAction("submit", {success:"AjaxSuccess", failure:"AjaxFailure"});');
  end;
  FCancelButton := TExtButton.AddTo(FFormPanel.Buttons);
  FCancelButton.Scale := Config.GetString('ButtonScale', 'medium');
  FCancelButton.Icon := Session.Config.GetImageURL('cancel');
  if FIsReadOnly then
  begin
    FCancelButton.Text := _('Close');
    FCancelButton.Tooltip := _('Close this panel');
    // No need for an ajax call when we just close the client-side panel.
    LHostWindow := GetHostWindow;
    if Assigned(LHostWindow) then
      FCancelButton.Handler := JSFunction(LHostWindow.JSName + '.close();');
  end
  else
  begin
    FCancelButton.Text := _('Cancel');
    FCancelButton.Tooltip := _('Cancel changes');
    FCancelButton.Handler := Ajax(CancelChanges);
  end;
end;

procedure TKExtFormPanelController.CancelChanges;
begin
  if FOperation = 'Add' then
  begin
    ServerStore.RemoveRecord(FStoreRecord);
    FStoreRecord := nil;
  end;
  NotifyObservers('Canceled');
  if not CloseHostWindow then
    StartOperation;
end;

{ TKExtDetailFormButton }

destructor TKExtDetailFormButton.Destroy;
begin
  //FreeAndNil(FDetailHostWindow);
  inherited;
end;

procedure TKExtDetailFormButton.SetViewTable(const AValue: TKViewTable);
begin
  FViewTable := AValue;
  if Assigned(FViewTable) then
  begin
    Text := _(FViewTable.PluralDisplayLabel);
    Icon := Session.Config.GetImageURL(FViewTable.ImageName);
    Handler := Ajax(ShowDetailWindow, []);
  end;
end;

procedure TKExtDetailFormButton.ShowDetailWindow;
var
  LController: IKExtController;
begin
  Assert(Assigned(FViewTable));

  if Assigned(FDetailHostWindow) then
    FDetailHostWindow.Free(True);
  FDetailHostWindow := TKExtModalWindow.Create;

  FDetailHostWindow.Title := _(ViewTable.PluralDisplayLabel);
  FDetailHostWindow.Closable := True;

  LController := TKExtControllerFactory.Instance.CreateController(FViewTable.View, FDetailHostWindow);
  LController.Config.SetObject('Sys/ServerStore', ServerStore);
  LController.Config.SetObject('Sys/ViewTable', ViewTable);
  LController.Config.SetObject('Sys/HostWindow', FDetailHostWindow);
  LController.Display;
  FDetailHostWindow.Show;
end;

initialization
  TKExtControllerRegistry.Instance.RegisterClass('Form', TKExtFormPanelController);

finalization
  TKExtControllerRegistry.Instance.UnregisterClass('Form');

end.
