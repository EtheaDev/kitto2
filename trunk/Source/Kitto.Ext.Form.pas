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
  Generics.Collections, SysUtils,
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
    FMainPagePanel: TKExtEditPage;
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
    procedure EnumEditors(const APredicate: TFunc<IKExtEditor, Boolean>; const AHandler: TProc<IKExtEditor>);
    procedure EditorsByFieldName(const AFieldName: string; const AHandler: TProc<IKExtEditor>);
    procedure AllEditors(const AHandler: TProc<IKExtEditor>);
    function GetExtraHeight: Integer;
    procedure AssignFieldChangeEvent(const AAssign: Boolean);
    procedure FieldChange(const AField: TKField;
      const AOldValue, ANewValue: Variant);
    function GetAfterLoadJSCode: string;
    function FindViewLayout: TKLayout;
  strict protected
    procedure DoDisplay; override;
    procedure InitComponents; override;
  public
    procedure LoadData; override;
    destructor Destroy; override;
  published
    procedure GetRecord;
    procedure SaveChanges;
    procedure CancelChanges;
  end;

implementation

uses
  StrUtils, Classes, Variants,
  ExtPascal,
  EF.Localization, EF.Types, EF.Intf, EF.Tree, EF.DB, EF.JSON, EF.VariantUtils,
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
  LoadData;
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
    FDetailToolbar := TExtToolbar.Create(Self);
    FDetailButtons := TObjectList<TKExtDetailFormButton>.Create(False);
    for I := 0 to ViewTable.DetailTableCount - 1 do
    begin
      FDetailButtons.Add(TKExtDetailFormButton.CreateAndAddTo(FDetailToolbar.Items));
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
      LController := TKExtControllerFactory.Instance.CreateController(FTabPanel,
        View, FTabPanel, ViewTable.FindNode('Controller'), Self, LControllerType);
      LController.Config.SetObject('Sys/ViewTable', ViewTable.DetailTables[I]);
      LController.Config.SetObject('Sys/ServerStore', FStoreRecord.DetailStores[I]);
      LController.Config.SetBoolean('AllowClose', False);
      FDetailControllers.Add(LController.AsObject);
      LController.Display;
      if (LController.AsObject is TKExtDataPanelController) then
        TKExtDataPanelController(LController.AsObject).LoadData;
    end;
  end;
end;

procedure TKExtFormPanelController.CreateEditors(const AForceReadOnly: Boolean);
var
  LLayoutProcessor: TKExtLayoutProcessor;
begin
  FreeAndNil(FEditors);
  FEditors := TList<TObject>.Create;
  LLayoutProcessor := TKExtLayoutProcessor.Create;
  try
    LLayoutProcessor.DataRecord := FStoreRecord;
    LLayoutProcessor.FormPanel := FFormPanel;
    LLayoutProcessor.MainEditPage := FMainPagePanel;
    LLayoutProcessor.TabPanel := FTabPanel;
    LLayoutProcessor.OnNewEditor :=
      procedure (AEditor: IKExtEditor)
      var
        LEditorSubject: IEFSubject;
      begin
        FEditors.Add(AEditor.AsObject);
        if Supports(AEditor.AsObject, IEFSubject, LEditorSubject) then
          LEditorSubject.AttachObserver(Self);
      end;
    LLayoutProcessor.ForceReadOnly := AForceReadOnly;
    if FOperation = 'Add' then
      LLayoutProcessor.Operation := eoInsert
    else
      LLayoutProcessor.Operation := eoUpdate;
    LLayoutProcessor.CreateEditors(FindViewLayout);
    FFocusField := LLayoutProcessor.FocusField;
  finally
    FreeAndNil(LLayoutProcessor);
  end;
  // Scroll back to top - can't do that until afterrender because body.dom is needed.
  FMainPagePanel.On('afterrender', JSFunction(FMainPagePanel.JSName + '.body.dom.scrollTop = 0;'));
end;

function TKExtFormPanelController.FindViewLayout: TKLayout;
var
  LLayoutName: string;
begin
  LLayoutName := ViewTable.GetString('Controller/Form/Layout');
  if LLayoutName <> '' then
    Result := View.Catalog.Layouts.FindLayout(LLayoutName)
  else
    Result := ViewTable.FindLayout('Form');
end;

function TKExtFormPanelController.GetDetailStyle: string;
begin
  Result := ViewTable.GetString('DetailTables/Controller/Style', 'Tabs');
end;

procedure TKExtFormPanelController.LoadData;
var
  LDetailStyle: string;
  LHostWindow: TExtWindow;
begin
  CreateEditors(FIsReadOnly);
  LDetailStyle := GetDetailStyle;
  if SameText(LDetailStyle, 'Tabs') then
    CreateDetailPanels
  else if SameText(LDetailStyle, 'Popup') then
    CreateDetailToolbar;
  // Resize the window after setting up toolbars and tabs, so that we
  // know the exact extra height needed.
  if Config.GetBoolean('Sys/HostWindow/AutoSize') then
  begin
    LHostWindow := GetHostWindow;
    if Assigned(LHostWindow) then
      LHostWindow.On('afterrender', JSFunction(Format(
        '%s.setOptimalSize(0, %d); %s.center();',
          [LHostWindow.JSName, GetExtraHeight, LHostWindow.JSName])));
  end;
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
    FFormPanel.GetForm.Load(JSObject(Format(
      'url: "%s", ' +
      'failure: function(form, action) { Ext.Msg.alert("%s", action.result.errorMessage); }, ' +
      'success: function(form, action) { %s }',
      [MethodURI(GetRecord), _('Load failed.'), GetAfterLoadJSCode])));

    FocusFirstField;
  except
    on E: EKValidationError do
    begin
      ExtMessageBox.Alert(_(Session.Config.AppTitle), E.Message);
      CancelChanges;
    end;
  end;
end;

procedure TKExtFormPanelController.EditorsByFieldName(const AFieldName: string;
  const AHandler: TProc<IKExtEditor>);
begin
  EnumEditors(
    function (AEditor: IKExteditor): Boolean
    begin
      Result := SameText(AEditor.GetRecordField.ViewField.AliasedName, AFieldName);
    end,
    AHandler);
end;

procedure TKExtFormPanelController.EnumEditors(
  const APredicate: TFunc<IKExtEditor, Boolean>;
  const AHandler: TProc<IKExtEditor>);
var
  I: Integer;
  LEditorIntf: IKExtEditor;
begin
  for I := 0 to FEditors.Count - 1 do
  begin
    if Supports(FEditors[I], IKExtEditor, LEditorIntf) then
    begin
      if APredicate(LEditorIntf) then
        AHandler(LEditorIntf);
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

  ExtSession.ResponseItems.AddJSON('{success:true,data:' + FStoreRecord.GetAsJSON(False) + '}');
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
      begin
        if (AFile.Context is TKViewField) and (TKViewField(AFile.Context).Table = ViewTable) then
        begin
          if TKViewField(AFile.Context).DataType is TEFBlobDataType then
            FStoreRecord.FieldByName(TKViewField(AFile.Context).AliasedName).AsBytes := AFile.Bytes
          else if TKViewField(AFile.Context).DataType is TKFileReferenceDataType then
            FStoreRecord.FieldByName(TKViewField(AFile.Context).AliasedName).AsString := AFile.FileName
          else
            raise Exception.CreateFmt(_('Data type %s does not support file upload.'), [TKViewField(AFile.Context).DataType.GetTypeName]);
          Session.RemoveUploadedFile(AFile);
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
    StartOperation
  else
    AssignFieldChangeEvent(False);
end;

procedure TKExtFormPanelController.InitComponents;
var
  LHostWindow: TExtWindow;

  function LayoutContainsPageBreaks: Boolean;
  var
    LLayout: TKLayout;
  begin
    Result := False;
    LLayout := FindViewLayout;
    if Assigned(LLayout) then
    begin
      Result := Assigned(LLayout.FindChildByPredicate(
        function (const ANode: TEFNode): Boolean
        begin
          Result := SameText(ANode.Name, 'PageBreak');
        end));
    end
  end;

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
  AssignFieldChangeEvent(True);

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

  FFormPanel := TKExtEditPanel.CreateAndAddTo(Items);
  FFormPanel.Region := rgCenter;
  FFormPanel.Border := False;
  FFormPanel.Header := False;
  FFormPanel.Layout := lyFit; // Vital to avoid detail grids with zero height!
  FFormPanel.AutoScroll := False;
  FFormPanel.LabelWidth := 120;
  FFormPanel.MonitorValid := True;
  FFormPanel.Cls := 'x-panel-mc'; // Sets correct theme background color.
  { TODO : check pages in layout as well }
  if ((ViewTable.DetailTableCount > 0) and SameText(GetDetailStyle, 'Tabs')) or LayoutContainsPageBreaks then
  begin
    FTabPanel := TExtTabPanel.CreateAndAddTo(FFormPanel.Items);
    FTabPanel.Border := False;
    FTabPanel.AutoScroll := False;
    FTabPanel.BodyStyle := 'background:none'; // Respects parent's background color.
    FTabPanel.DeferredRender := False;
    FMainPagePanel := TKExtEditPage.CreateAndAddTo(FTabPanel.Items);
    FMainPagePanel.Title := _(ViewTable.DisplayLabel);
    FMainPagePanel.EditPanel := FFormPanel;
    FTabPanel.SetActiveTab(0);
  end
  else
  begin
    FTabPanel := nil;
    FMainPagePanel := TKExtEditPage.CreateAndAddTo(FFormPanel.Items);
    FMainPagePanel.Region := rgCenter;
    FMainPagePanel.EditPanel := FFormPanel;
  end;
  FMainPagePanel.PaddingString := '5px';
  //Session.ResponseItems.ExecuteJSCode(Format('%s.getForm().url = "%s";', [FFormPanel.JSName, MethodURI(SaveChanges)]));

  if not FIsReadOnly then
  begin
    FSaveButton := TExtButton.CreateAndAddTo(FFormPanel.Buttons);
    FSaveButton.Scale := Config.GetString('ButtonScale', 'medium');
    FSaveButton.FormBind := True;
    FSaveButton.Text := _('Save');
    FSaveButton.Tooltip := _('Save changes and finish editing');
    FSaveButton.Icon := Session.Config.GetImageURL('accept');
    // AjaxForms allows us to put JS code in the response, something the commented
    // versions don't allow.
    FSaveButton.Handler := AjaxForms(SaveChanges, [FFormPanel]);
    // Don't just call submit() - we want AjaxSuccess/AjaxFailure to be called so that our response is actually executed.
    //FSaveButton.Handler := JSFunction(FFormPanel.JSName + '.getForm().submit();');
    //FSaveButton.Handler := JSFunction(FFormPanel.JSName + '.getForm().doAction("submit", {success:"AjaxSuccess", failure:"AjaxFailure"});');
  end;
  FCancelButton := TExtButton.CreateAndAddTo(FFormPanel.Buttons);
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

function TKExtFormPanelController.GetExtraHeight: Integer;
begin
  Result := 10; // 5px padding * 2.
  if Assigned(FDetailToolbar) then
    Result := Result + 30;
  if Assigned(TopToolbar) then
    Result := Result + 30;
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
    StartOperation
  else
    AssignFieldChangeEvent(False);
end;

function TKExtFormPanelController.GetAfterLoadJSCode: string;
var
  LResponseItems: TExtResponseItems;
begin
  LResponseItems := ExtSession.BranchResponseItems;
  try
    AllEditors(
      procedure (AEditor: IKExtEditor)
      var
        LIntf: IKExtEditorAfterLoad;
      begin
        if Supports(AEditor, IKExtEditorAfterLoad, LIntf) then
          LIntf.AfterLoad;
        end);
    Result := LResponseItems.Consume;
  finally
    ExtSession.UnbranchResponseItems(LResponseItems, False);
  end;
end;

procedure TKExtFormPanelController.AllEditors(
  const AHandler: TProc<IKExtEditor>);
begin
  EnumEditors(
    function (AEditor: IKExteditor): Boolean
    begin
      Result := True;
    end,
    AHandler);
end;

procedure TKExtFormPanelController.AssignFieldChangeEvent(const AAssign: Boolean);
begin
  if Assigned(FStoreRecord) then
    if AAssign then
      FStoreRecord.OnFieldChange := FieldChange
    else
      FStoreRecord.OnFieldChange := nil;
end;

procedure TKExtFormPanelController.FieldChange(const AField: TKField;
  const AOldValue, ANewValue: Variant);
begin
  Assert(Assigned(AField));
  Assert(AField is TKViewTableField);

  EditorsByFieldName(TKViewTableField(AField).FieldName,
    procedure (AEditor: IKExtEditor)
    begin
      AEditor.RefreshValue;
    end);
end;

{ TKExtDetailFormButton }

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
  FDetailHostWindow := TKExtModalWindow.Create(Self);

  FDetailHostWindow.Title := _(ViewTable.PluralDisplayLabel);
  FDetailHostWindow.Closable := True;

  LController := TKExtControllerFactory.Instance.CreateController(
    FDetailHostWindow, FViewTable.View, FDetailHostWindow);
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
