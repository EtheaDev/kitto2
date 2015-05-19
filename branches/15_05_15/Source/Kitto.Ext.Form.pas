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
  Ext, ExtData, ExtForm, ExtPascalUtils,
  superobject,
  EF.ObserverIntf, EF.Tree,
  Kitto.Metadata.Views, Kitto.Metadata.DataView, Kitto.Store,
  Kitto.Ext.Controller, Kitto.Ext.Base, Kitto.Ext.DataPanel, Kitto.Ext.Editors,
  Kitto.Ext.GridPanel;

const
  FORM_LABELWIDTH = 120;

type
  /// <summary>
  ///  A button that opens a popup detail form.
  /// </summary>
  TKExtDetailFormButton = class(TKExtButton)
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

  /// <summary>
  ///  The Form controller.
  /// </summary>
  TKExtFormPanelController = class(TKExtDataPanelController)
  strict private
    FTabPanel: TExtTabPanel;
    FFormPanel: TKExtEditPanel;
    FMainPagePanel: TKExtEditPage;
    FIsReadOnly: Boolean;
    FConfirmButton: TKExtButton;
    FEditButton: TKExtButton;
    FCancelButton: TKExtButton;
    FCloseButton: TKExtButton;
    FDetailToolbar: TKExtToolbar;
    FDetailButtons: TObjectList<TKExtDetailFormButton>;
    FDetailControllers: TObjectList<TObject>;
    FOperation: string;
    FFocusField: TExtFormField;
    FStoreRecord: TKViewTableRecord;
    FCloneValues: TEFNode;
    FCloneButton: TKExtButton;
    FLabelAlign: TExtFormFormPanelLabelAlign;
    FDetailBottomPanel: TExtTabPanel;
    procedure CreateEditors;
    procedure RecreateEditors;
    procedure CreateButtons;
    procedure ChangeEditorsState;
    procedure StartOperation;
    procedure FocusFirstField;
    procedure CreateDetailPanels(const ATabPanel: TExtTabPanel);
    procedure CreateDetailToolbar;
    procedure CreateDetailBottomPanel;
    function GetDetailStyle: string;
    function GetExtraHeight: Integer;
    function GetDetailBottomPanelHeight: Integer;
    procedure AssignFieldChangeEvent(const AAssign: Boolean);
    procedure FieldChange(const AField: TKField; const AOldValue, ANewValue: Variant);
    procedure CreateFormPanel;
    function LayoutContainsPageBreaks: Boolean;
    function GetConfirmJSCode(const AMethod: TExtProcedure): string;
    procedure InitFlags;
    function FindLayout: TKLayout;
    function IsViewMode: Boolean;
    procedure RefreshEditorValues;
    procedure SetStoreRecord(const AValue: TKViewTableRecord);
  strict protected
    procedure DoDisplay; override;
    procedure InitComponents; override;
    property StoreRecord: TKViewTableRecord read FStoreRecord write SetStoreRecord;
    function AddActionButton(const AUniqueId: string; const AView: TKView;
      const AToolbar: TKExtToolbar): TKExtActionButton; override;
    procedure TabChange(AThis: TExtTabPanel; ATab: TExtPanel); virtual;
  public
    procedure LoadData; override;
    destructor Destroy; override;
    function GetFilterExpression: string; override;
  published
    procedure GetRecord;
    procedure SwitchToEditMode;
    procedure ConfirmChanges;
    procedure ConfirmChangesAndClone;
    procedure CancelChanges;
  end;

implementation

uses
  StrUtils, Classes, Variants, Types,
  ExtPascal,
  EF.Localization, EF.Types, EF.Intf, EF.DB, EF.JSON, EF.VariantUtils, EF.StrUtils,
  Kitto.Types, Kitto.AccessControl, Kitto.Rules, Kitto.SQL, Kitto.Config,
  Kitto.Ext.Session, Kitto.Ext.Utils;

{ TKExtFormPanelController }

procedure TKExtFormPanelController.ChangeEditorsState;
var
  LViewMode: Boolean;
  LInsertOperation: Boolean;
begin
  LViewMode := IsViewMode;
  LInsertOperation := FOperation = 'Add';
  FEditItems.AllEditors(
    procedure (AEditor: IKExtEditor)
    var
      LFormField: TExtFormField;
      LViewField: TKViewField;
    begin
      LFormField := AEditor.AsExtFormField;
      if Assigned(LFormField) then
      begin
        LViewField := ViewTable.FieldByAliasedName(AEditor.FieldName);
        if Assigned(LViewField) then
          LFormField.ReadOnly := LViewMode or not LViewField.CanEditField(LInsertOperation)
        else
          LFormField.ReadOnly := LViewMode;

          if not LFormField.ReadOnly and (FFocusField = nil) then
            FFocusField := LFormField;
      end;
    end);
end;

destructor TKExtFormPanelController.Destroy;
begin
  FreeAndNil(FCloneValues);
  FreeAndNil(FEditItems);
  FreeAndNil(FDetailButtons);
  FreeAndNil(FDetailControllers);
  inherited;
end;

procedure TKExtFormPanelController.DoDisplay;
begin
  inherited;
  CreateEditors;
  LoadData;
  ChangeEditorsState;
end;

procedure TKExtFormPanelController.CreateDetailToolbar;
var
  I: Integer;
begin
  Assert(ViewTable <> nil);
  Assert(FDetailToolbar = nil);
  Assert(FDetailButtons = nil);
  Assert(Assigned(StoreRecord));

  if ViewTable.DetailTableCount > 0 then
  begin
    StoreRecord.EnsureDetailStores;
    Assert(StoreRecord.DetailStoreCount = ViewTable.DetailTableCount);
    FDetailToolbar := TKExtToolbar.Create(Self);
    FDetailButtons := TObjectList<TKExtDetailFormButton>.Create(False);
    for I := 0 to ViewTable.DetailTableCount - 1 do
    begin
      FDetailButtons.Add(TKExtDetailFormButton.CreateAndAddTo(FDetailToolbar.Items));
      FDetailButtons[I].ServerStore := StoreRecord.DetailStores[I];
      FDetailButtons[I].ViewTable := ViewTable.DetailTables[I];
    end;
    Tbar := FDetailToolbar;
  end;
end;

procedure TKExtFormPanelController.CreateDetailBottomPanel;
begin
  Assert(ViewTable <> nil);
  Assert(FDetailControllers = nil);
  Assert(Assigned(StoreRecord));
  Assert(not Assigned(FDetailBottomPanel));

  if ViewTable.DetailTableCount > 0 then
  begin
    FDetailBottomPanel := TExtTabPanel.CreateAndAddTo(Items);
    FDetailBottomPanel.Split := True;
    FDetailBottomPanel.Region := rgSouth;
    FDetailBottomPanel.Border := False;
    FDetailBottomPanel.AutoScroll := False;
    FDetailBottomPanel.BodyStyle := 'background:none'; // Respects parent's background color.
    FDetailBottomPanel.DeferredRender := False;
    FDetailBottomPanel.EnableTabScroll := True;
    FDetailBottomPanel.Height := GetDetailBottomPanelHeight;
    FDetailBottomPanel.SetActiveTab(0);
    if Assigned(FTabPanel) then
      FTabPanel.OnTabChange := TabChange;
    FDetailBottomPanel.On('tabchange', FDetailBottomPanel.JSFunction(FDetailBottomPanel.JSName + '.doLayout();'));
    CreateDetailPanels(FDetailBottomPanel);
  end;
end;

procedure TKExtFormPanelController.CreateDetailPanels(const ATabPanel: TExtTabPanel);
var
  I: Integer;
  LController: IKExtController;
  LControllerType: string;
begin
  Assert(ViewTable <> nil);
  Assert(FDetailControllers = nil);
  Assert(Assigned(StoreRecord));

  if ViewTable.DetailTableCount > 0 then
  begin
    Assert(ATabPanel <> nil);
    StoreRecord.EnsureDetailStores;
    Assert(StoreRecord.DetailStoreCount = ViewTable.DetailTableCount);
    FDetailControllers := TObjectList<TObject>.Create(False);
    for I := 0 to ViewTable.DetailTableCount - 1 do
    begin
      LControllerType := ViewTable.GetString('Controller', 'GridPanel');
      // The node may exist and be '', which does not return the default value.
      if LControllerType = '' then
        LControllerType := 'GridPanel';
      LController := TKExtControllerFactory.Instance.CreateController(ATabPanel,
        View, ATabPanel, ViewTable.FindNode('Controller'), Self, LControllerType);
      LController.Config.SetObject('Sys/ViewTable', ViewTable.DetailTables[I]);
      LController.Config.SetObject('Sys/ServerStore', StoreRecord.DetailStores[I]);
      LController.Config.SetBoolean('AllowClose', False);
      if SameText(FOperation, 'View') then
      begin
        //Cascading View mode
        LController.Config.SetBoolean('AllowViewing', True);
        LController.Config.SetBoolean('PreventEditing', True);
        LController.Config.SetBoolean('PreventAdding', True);
        LController.Config.SetBoolean('PreventDeleting', True);
        LController.Config.SetBoolean('AllowDuplicating', False);
      end;
      FDetailControllers.Add(LController.AsObject);
      LController.Display;
      if (LController.AsObject is TKExtDataPanelController) then
        TKExtDataPanelController(LController.AsObject).LoadData;
    end;
  end;
end;

procedure TKExtFormPanelController.RecreateEditors;
begin
  FFormPanel.Free(True);
  CreateFormPanel;
  CreateEditors;
end;

procedure TKExtFormPanelController.CreateEditors;
var
  LLayoutProcessor: TKExtLayoutProcessor;
begin
  Assert(Assigned(StoreRecord));

  FreeAndNil(FEditItems);
  FEditItems := TKEditItemList.Create;
  LLayoutProcessor := TKExtLayoutProcessor.Create;
  try
    LLayoutProcessor.DataRecord := StoreRecord;
    LLayoutProcessor.FormPanel := FFormPanel;
    LLayoutProcessor.MainEditPage := FMainPagePanel;
    LLayoutProcessor.TabPanel := FTabPanel;
    LLayoutProcessor.OnNewEditItem :=
      procedure (AEditItem: IKExtEditItem)
      var
        LSubject: IEFSubject;
      begin
        FEditItems.Add(AEditItem.AsObject);
        if Supports(AEditItem.AsObject, IEFSubject, LSubject) then
          LSubject.AttachObserver(Self);
      end;
    LLayoutProcessor.ForceReadOnly := FIsReadOnly;
    if MatchStr(FOperation, ['Add', 'Dup']) then
      LLayoutProcessor.Operation := eoInsert
    else
      LLayoutProcessor.Operation := eoUpdate;
    LLayoutProcessor.CreateEditors(FindLayout);
    FFocusField := LLayoutProcessor.FocusField;
  finally
    FreeAndNil(LLayoutProcessor);
  end;
  // Scroll back to top - can't do that until afterrender because body.dom is needed.
  FMainPagePanel.On('afterrender', JSFunction(FMainPagePanel.JSName + '.body.dom.scrollTop = 0;'));
  // Set button handlers (editors are needed by GetConfirmJSCode).
  if Assigned(FConfirmButton) then
  begin
    FConfirmButton.Handler := JSFunction(GetConfirmJSCode(ConfirmChanges));
    FFormPanel.On('clientvalidation', JSFunction('form, valid', FConfirmButton.JSName+'.setDisabled(!valid);'));
  end;
  if Assigned(FEditButton) then
    FEditButton.Handler := JSFunction(GetConfirmJSCode(SwitchToEditMode));
  if Assigned(FCloneButton) then
  begin
    FCloneButton.Handler := JSFunction(GetConfirmJSCode(ConfirmChangesAndClone));
    FFormPanel.On('clientvalidation', JSFunction('form, valid', FCloneButton.JSName+'.setDisabled(!valid);'));
  end;
end;

function TKExtFormPanelController.GetDetailBottomPanelHeight: Integer;
const
  DEFAULT_DETAIL_BOTTOM_PANEL_HEIGHT = 200;
begin
  Result := ViewTable.GetInteger('DetailTables/Controller/Style/Height', DEFAULT_DETAIL_BOTTOM_PANEL_HEIGHT);
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
  LDetailStyle := GetDetailStyle;
  if SameText(LDetailStyle, 'Tabs') then
    CreateDetailPanels(FTabPanel)
  else if SameText(LDetailStyle, 'Popup') then
    CreateDetailToolbar
  else if SameText(LDetailStyle, 'Bottom') then
    CreateDetailBottomPanel;
  // Resize the window after setting up toolbars and tabs, so that we
  // know the exact extra height needed.
  if Config.GetBoolean('Sys/HostWindow/AutoSize') then
  begin
    LHostWindow := GetHostWindow;
    if Assigned(LHostWindow) and not LHostWindow.Maximized then
      LHostWindow.On('afterrender', JSFunction(Format(
        '%s.setOptimalSize(0, %d); %s.center();',
          [LHostWindow.JSName, GetExtraHeight, LHostWindow.JSName])));
  end;
  StartOperation;
end;

procedure TKExtFormPanelController.SetStoreRecord(const AValue: TKViewTableRecord);
begin
  FStoreRecord := AValue;
  if Assigned(FStoreRecord) then
  begin
    FStoreRecord.OnSetTransientProperty :=
      procedure(ASubjectType, ASubjectName, APropertyName: string; AValue: Variant)
      begin
        if Assigned(FEditItems) then
        begin
          if SameText(ASubjectType, 'Field') then
          begin
            FEditItems.EditorsByViewField(TKViewTableField(FStoreRecord.FieldByName(ASubjectName)).ViewField,
              procedure (AEditor: IKExtEditor)
              begin
                AEditor.SetTransientProperty(APropertyName, AValue);
              end);
          end
          else
          begin
            FEditItems.EditItemsById(ASubjectName,
              procedure (AEditItem: IKExtEditItem)
              begin
                AEditItem.SetTransientProperty(APropertyName, AValue);
              end);
          end;
        end;
      end;
  end;
end;

procedure TKExtFormPanelController.StartOperation;
var
  LDefaultValues: TEFNode;

  procedure MergeKeyDefaultValues;
  var
    LKeyDefaultValues: TEFNode;
  begin
    LKeyDefaultValues := ViewTable.GetDefaultValues(True);
    try
      LDefaultValues.Merge(LKeyDefaultValues);
    finally
      FreeAndNil(LKeyDefaultValues);
    end;
  end;

begin
  Assert(Assigned(StoreRecord));

  AssignFieldChangeEvent(True);
  try
    if MatchText(FOperation, ['Add', 'Dup']) then
    begin
      LDefaultValues := nil;
      try
        if Assigned(FCloneValues) then
        begin
          LDefaultValues := TEFNode.Clone(FCloneValues);
          MergeKeyDefaultValues;
        end
        else
          LDefaultValues := ViewTable.GetDefaultValues;
        if SameText(FOperation, 'Dup') then
          StoreRecord.Store.DisableChangeNotifications;
        try
          StoreRecord.ReadFromNode(LDefaultValues);
        finally
          if SameText(FOperation, 'Dup') then
            StoreRecord.Store.EnableChangeNotifications;
        end;
        ViewTable.Model.BeforeNewRecord(StoreRecord, Assigned(FCloneValues) and SameText(FOperation, 'Add'));
        StoreRecord.ApplyNewRecordRules;
        ViewTable.Model.AfterNewRecord(StoreRecord);
      finally
        FreeAndNil(LDefaultValues);
      end;
    end;

    RefreshEditorValues;

    FocusFirstField;
  except
    on E: EKValidationError do
    begin
      ExtMessageBox.Alert(_(Session.Config.AppTitle), E.Message);
      CancelChanges;
    end;
  end;
end;

procedure TKExtFormPanelController.RefreshEditorValues;
begin
  // Load data. Combo boxes can only have their raw value set after they're rendered.
  FEditItems.AllEditors(
    procedure (AEditor: IKExtEditor)
    var
      LFormField: TExtFormField;
    begin
      LFormField := AEditor.AsExtFormField;
      if Assigned(LFormField) then
      begin
        LFormField.RemoveAllListeners('afterrender');
        LFormField.On('afterrender', LFormField.JSFunction(
          procedure()
          begin
            AEditor.RefreshValue;
          end));
      end
      else
        AEditor.RefreshValue;
    end);
end;

procedure TKExtFormPanelController.SwitchToEditMode;
var
  LHostWindow: TExtWindow;
begin
  FStoreRecord.ApplyEditRecordRules;
  FEditButton.SetVisible(False);
  FConfirmButton.SetVisible(True);
  if Assigned(FCloneButton) then
    FCloneButton.SetVisible(True);
  FCloseButton.SetVisible(False);
  FCancelButton.SetVisible(True);
  FOperation := 'Edit';
  InitFlags;
  ChangeEditorsState;
  LHostWindow := GetHostWindow;
  if Assigned(LHostWindow) then
    LHostWindow.Title := Format(_('Edit %s'), [_(ViewTable.DisplayLabel)]);
  StartOperation;
end;

procedure TKExtFormPanelController.FocusFirstField;
begin
  if Assigned (FFocusField) then
    FFocusField.Focus(False, 500);
end;

procedure TKExtFormPanelController.GetRecord;
begin
  Assert(Assigned(StoreRecord));

  ExtSession.ResponseItems.AddJSON('{success:true,data:' + StoreRecord.GetAsJSON(False) + '}');
end;

procedure TKExtFormPanelController.ConfirmChanges;
var
  LError: string;
begin
  AssignFieldChangeEvent(False);
  LError := UpdateRecord(StoreRecord, SO(Session.RequestBody).O['new'], True);
  FreeAndNil(FCloneValues);
  if LError = '' then
  begin
    if Config.GetBoolean('KeepOpenAfterOperation') then
      StartOperation
    else
      CloseHostContainer;
  end;
end;

procedure TKExtFormPanelController.ConfirmChangesAndClone;
begin
  UpdateRecord(StoreRecord, SO(Session.RequestBody).O['new'], True);
  FCloneValues := TEFNode.Clone(StoreRecord);
  StoreRecord := ServerStore.AppendRecord(nil);
  FOperation := 'Add';
  // recupera dati record
  StartOperation;
end;

function TKExtFormPanelController.LayoutContainsPageBreaks: Boolean;
var
  LLayout: TKLayout;
begin
  Result := False;
  LLayout := FindLayout;
  if Assigned(LLayout) then
  begin
    Result := Assigned(LLayout.FindChildByPredicate(
      function (const ANode: TEFNode): Boolean
      begin
        Result := SameText(ANode.Name, 'PageBreak');
      end));
  end
end;

procedure TKExtFormPanelController.CreateButtons;
var
  LCloneButtonNode: TEFNode;
  LHostWindow: TExtWindow;
begin
  if not FIsReadOnly then
  begin
    LCloneButtonNode := Config.FindNode('CloneButton');
    if Assigned(LCloneButtonNode) then
    begin
      FCloneButton := TKExtButton.CreateAndAddTo(Buttons);
      FCloneButton.SetIconAndScale('accept_clone', Config.GetString('ButtonScale', 'medium'));
      FCloneButton.Text := LCloneButtonNode.GetString('Caption', _('Save & Clone'));
      FCloneButton.Tooltip := LCloneButtonNode.GetString('Tooltip', _('Save changes and create a new clone record'));
      FCloneButton.Hidden := FIsReadOnly or IsViewMode;
    end
    else
      FCloneButton := nil;
  end;
  FConfirmButton := TKExtButton.CreateAndAddTo(Buttons);
  if ViewTable.IsDetail or (ViewTable.DetailTableCount = 0) then
    FConfirmButton.SetIconAndScale('accept', Config.GetString('ButtonScale', 'medium'))
  else
    FConfirmButton.SetIconAndScale('save_all', Config.GetString('ButtonScale', 'medium'));

  if ViewTable.DetailTableCount > 0 then
    FConfirmButton.Text := Config.GetString('ConfirmButton/Caption', _('Save all'))
  else
    FConfirmButton.Text := Config.GetString('ConfirmButton/Caption', _('Save'));
  FConfirmButton.Tooltip := Config.GetString('ConfirmButton/Tooltip', _('Save changes and finish editing'));
  FConfirmButton.Hidden := FIsReadOnly or IsViewMode;

  if IsViewMode then
  begin
    FEditButton := TKExtButton.CreateAndAddTo(Buttons);
    FEditButton.SetIconAndScale('edit_record', Config.GetString('ButtonScale', 'medium'));
    FEditButton.Text := Config.GetString('ConfirmButton/Caption', _('Edit'));
    FEditButton.Tooltip := Config.GetString('ConfirmButton/Tooltip', _('Switch to edit mode'));
    FEditButton.Hidden := FIsReadOnly;
  end;

  FCancelButton := TKExtButton.CreateAndAddTo(Buttons);
  FCancelButton.SetIconAndScale('cancel', Config.GetString('ButtonScale', 'medium'));
  FCancelButton.Text := _('Cancel');
  FCancelButton.Tooltip := _('Cancel changes');
  FCancelButton.Handler := Ajax(CancelChanges);
  FCancelButton.Hidden := FIsReadOnly or IsViewMode;

  FCloseButton := TKExtButton.CreateAndAddTo(Buttons);
  FCloseButton.SetIconAndScale('close', Config.GetString('ButtonScale', 'medium'));
  FCloseButton.Text := _('Close');
  FCloseButton.Tooltip := _('Close this panel');
  // No need for an ajax call when we just close the client-side panel.
  LHostWindow := GetHostWindow;
  if Assigned(LHostWindow) then
    FCloseButton.Handler := JSFunction(LHostWindow.JSName + '.close();');
  FCloseButton.Hidden := not FIsReadOnly and not IsViewMode;
end;

procedure TKExtFormPanelController.InitComponents;
begin
  inherited;
  FOperation := Config.GetString('Sys/Operation');
  if FOperation = '' then
    FOperation := Config.GetString('Operation');
  InitFlags;
  CreateFormPanel;
  CreateButtons;
end;

function TKExtFormPanelController.GetFilterExpression: string;
begin
  Result := Config.GetExpandedString('FilterExpression');
end;

procedure TKExtFormPanelController.InitFlags;
var
  LLabelAlignNode: TEFNode;
begin
  if Title = '' then
    Title := _(ViewTable.DisplayLabel);

  if SameText(FOperation, 'Add') then
  begin
    Assert(not Assigned(StoreRecord));
    StoreRecord := ServerStore.AppendRecord(nil);
  end
  else if SameText(FOperation, 'Dup') then
  begin
    StoreRecord := Config.GetObject('Sys/Record') as TKViewTableRecord;
    FreeAndNil(FCloneValues);
    FCloneValues := TEFNode.Clone(StoreRecord);
    StoreRecord := ServerStore.AppendRecord(nil);
  end
  else if MatchText(FOperation, ['Edit', 'View']) then
  begin
    StoreRecord := Config.GetObject('Sys/Record') as TKViewTableRecord;
    if not Assigned(StoreRecord) then
    begin
      // Record was not provided by the caller, so we load the store and use
      // the single record contained.
      if ServerStore.RecordCount = 0 then
        ViewTable.Model.LoadRecords(ServerStore, GetFilterExpression, '', 0, 0);
      Assert(ServerStore.RecordCount=1);
      StoreRecord := ServerStore.Records[0];
    end;
  end;
  Assert(Assigned(StoreRecord));

  AssignFieldChangeEvent(True);

  if MatchText(FOperation, ['Add', 'Dup']) then
    FIsReadOnly := ViewTable.GetBoolean('Controller/PreventAdding')
      or View.GetBoolean('IsReadOnly')
      or ViewTable.IsReadOnly
      or Config.GetBoolean('PreventAdding')
      or not ViewTable.IsAccessGranted(ACM_ADD)
  else //Edit or View Mode
    FIsReadOnly := ViewTable.GetBoolean('Controller/PreventEditing')
      or View.GetBoolean('IsReadOnly')
      or ViewTable.IsReadOnly
      or Config.GetBoolean('PreventEditing')
      or not ViewTable.IsAccessGranted(ACM_MODIFY);

  if SameText(FOperation, 'Add') and FIsReadOnly then
    raise EEFError.Create(_('Operation Add not supported on read-only data.'))
  else if SameText(FOperation, 'Edit') and FIsReadOnly then
    raise EEFError.Create(_('Operation Edit not supported on read-only data.'))
  else if SameText(FOperation, 'Dup') and FIsReadOnly then
    raise EEFError.Create(_('Operation Duplicate not supported on read-only data.'));

  LLabelAlignNode := ViewTable.FindNode('Controller/FormController/LabelAlign');
  if FindLayout <> nil then
    FLabelAlign := laTop
  else if Assigned(LLabelAlignNode) then
    FLabelAlign := OptionAsLabelAlign(LLabelAlignNode.AsString)
  else if Session.IsMobileBrowser then
    FLabelAlign := laTop
  else
    FLabelAlign := laRight;
end;

procedure TKExtFormPanelController.CreateFormPanel;
var
  LDetailStyle: string;
begin
  Cls := 'x-panel-mc'; // Sets correct theme background color same as panel
  LDetailStyle := GetDetailStyle;

  FFormPanel := TKExtEditPanel.CreateAndAddTo(Items);
  FFormPanel.Region := rgCenter;
  FFormPanel.Border := False;
  FFormPanel.Header := False;
  FFormPanel.Layout := lyFit; // Vital to avoid detail grids with zero height!
  FFormPanel.AutoScroll := False;
  FFormPanel.LabelWidth := FORM_LABELWIDTH;
  FFormPanel.MonitorValid := True;
  FFormPanel.Cls := 'x-panel-mc'; // Sets correct theme background color.
  FFormPanel.LabelAlign := FLabelAlign;
  if ((ViewTable.DetailTableCount > 0) and SameText(LDetailStyle, 'Tabs')) or LayoutContainsPageBreaks then
  begin
    FTabPanel := TExtTabPanel.CreateAndAddTo(FFormPanel.Items);
    FTabPanel.Border := False;
    FTabPanel.AutoScroll := False;
    FTabPanel.BodyStyle := 'background:none'; // Respects parent's background color.
    FTabPanel.DeferredRender := False;
    FTabPanel.EnableTabScroll := True;
    FMainPagePanel := TKExtEditPage.CreateAndAddTo(FTabPanel.Items);
    FMainPagePanel.Title := _(ViewTable.DisplayLabel);
    if Config.GetBoolean('Sys/ShowIcon', True) then
      FMainPagePanel.IconCls := Session.SetViewIconStyle(ViewTable.View);
    FMainPagePanel.EditPanel := FFormPanel;
    FMainPagePanel.LabelAlign := FLabelAlign;
    FTabPanel.SetActiveTab(0);
    FTabPanel.OnTabChange := TabChange;
    FTabPanel.On('tabchange', FTabPanel.JSFunction(FTabPanel.JSName + '.doLayout();'));
  end
  else
  begin
    FTabPanel := nil;
    FMainPagePanel := TKExtEditPage.CreateAndAddTo(FFormPanel.Items);
    FMainPagePanel.Region := rgCenter;
    FMainPagePanel.EditPanel := FFormPanel;
    FMainPagePanel.LabelAlign := FLabelAlign;
  end;
  //Session.ResponseItems.ExecuteJSCode(Format('%s.getForm().url = "%s";', [FFormPanel.JSName, MethodURI(ConfirmChanges)]));
end;

procedure TKExtFormPanelController.TabChange(AThis: TExtTabPanel; ATab: TExtPanel);
var
  LIntf: IKExtActivable;
begin
  if Assigned(ATab) and Supports(ATab, IKExtActivable, LIntf) then
    LIntf.Activate;
end;

function TKExtFormPanelController.GetExtraHeight: Integer;
begin
  Result := 10; // 5px padding * 2.
  if Assigned(FDetailToolbar) then
    Result := Result + 30;
  if Assigned(TopToolbar) then
    Result := Result + 30;
  if Assigned(FDetailBottomPanel) then
    Result := Result + GetDetailBottomPanelHeight + 120;
end;

function TKExtFormPanelController.IsViewMode: Boolean;
begin
  Result := FOperation = 'View';
end;

procedure TKExtFormPanelController.CancelChanges;
var
  LKeepOpen: Boolean;
begin
  LKeepOpen := Config.GetBoolean('KeepOpenAfterOperation');

  if MatchText(FOperation, ['Add', 'Dup']) then
  begin
    ServerStore.RemoveRecord(StoreRecord);
    StoreRecord := nil;
  end
  else if SameText(FOperation, 'Edit') then
  begin
    StoreRecord.Store.DisableChangeNotifications;
    try
      StoreRecord.Refresh;
    finally
      StoreRecord.Store.EnableChangeNotifications;
    end;
  end;

  NotifyObservers('Canceled');
  if LKeepOpen then
  begin
    if SameText(FOperation, 'Add') then
    begin
      StoreRecord := ServerStore.AppendRecord(nil);
      RecreateEditors;
    end
    else
    begin
      { TODO: implement Dup + KeepOpenAfterOperation }
      Assert(False, 'Dup + KeepOpenAfterOperation not implemented.');
    end;
    StartOperation;
  end
  else
  begin
    AssignFieldChangeEvent(False);
    CloseHostContainer;
  end;
end;

function TKExtFormPanelController.AddActionButton(const AUniqueId: string;
  const AView: TKView; const AToolbar: TKExtToolbar): TKExtActionButton;
begin
  Result := inherited AddActionButton(AUniqueId, AView, AToolbar);
  TKExtDataActionButton(Result).OnGetServerRecord :=
    function: TKViewTableRecord
    begin
      Result := StoreRecord;
    end;
end;

procedure TKExtFormPanelController.AssignFieldChangeEvent(const AAssign: Boolean);
begin
  if Assigned(StoreRecord) then
    if AAssign then
      StoreRecord.OnFieldChange := FieldChange
    else
      StoreRecord.OnFieldChange := nil;
end;

procedure TKExtFormPanelController.FieldChange(const AField: TKField; const AOldValue, ANewValue: Variant);
var
  LField: TKViewTableField;
  LOldValue: Variant;
  LNewValue: Variant;
begin
  Assert(Assigned(AField));
  Assert(AField is TKViewTableField);

  if AField.IsPartOfCompositeField then
    Exit;

  LField := TKViewTableField(AField);

  LOldValue := AOldValue;
  LNewValue := ANewValue;
  LField.ViewField.ApplyRules(
    procedure (ARuleImpl: TKRuleImpl)
    begin
      ARuleImpl.AfterFieldChange(AField, LOldValue, LNewValue);
    end);

  { TODO :
  Refactor the way derived fields are determined.
  Reference fields should not have derived fields.
  Underlying key fields should.
  Meanwhile, we just ignore changes to reference fields
  that would not work due to having only the caption
  and not the key values here.
  After the refactoring, this test can be removed. }
//  if LField.ViewField.IsReference and not LField.IsPhysicalPartOfReference then
//    Exit;

  // Refresh editors linked to changed field.
  FEditItems.EditorsByViewField(LField.ViewField,
    procedure (AEditor: IKExtEditor)
    begin
      AEditor.RefreshValue;
    end);

  // Give all non-editors a chance to refresh (such as a FieldSet which might
  // need to refresh its title). This might be a performance bottleneck.
  FEditItems.AllNonEditors(
    procedure (AEditItem: IKExtEditItem)
    begin
      AEditItem.RefreshValue;
    end);
end;

function TKExtFormPanelController.FindLayout: TKLayout;
begin
  Result := FindViewLayout('Form');
end;

function TKExtFormPanelController.GetConfirmJSCode(const AMethod: TExtProcedure): string;
var
  LCode: string;
begin
  LCode :=
    'var json = new Object;' + sLineBreak +
    'json.new = new Object;' + sLineBreak;

  LCode := LCode + GetJSFunctionCode(
    procedure
    begin
      FEditItems.AllEditors(
        procedure (AEditor: IKExtEditor)
        begin
          AEditor.StoreValue('json.new');
        end);
    end,
    False) + sLineBreak;

  LCode := LCode + GetPOSTAjaxCode(AMethod, [], 'json') + sLineBreak;
  Result := LCode;
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
