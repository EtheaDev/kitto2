{-------------------------------------------------------------------------------
   Copyright 2012-2017 Ethea S.r.l.

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
  Generics.Collections
  , SysUtils
  , Ext.Base
  , Ext.Data
  , Ext.Form
  , EF.ObserverIntf
  , EF.Tree
  , Kitto.JS
  , Kitto.JS.Types
  , Kitto.Metadata.Views
  , Kitto.Metadata.DataView
  , Kitto.Store
  , Kitto.Ext.Controller
  , Kitto.Ext.Base
  , Kitto.Ext.DataPanel
  , Kitto.Ext.Editors
  , Kitto.Ext.GridPanel
  ;

const
  FORM_LABELWIDTH = 120;
  DEFAULT_DETAIL_PANEL_HEIGHT = 200;
  DEFAULT_DETAIL_STYLE = 'Tabs';

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
  //published
    procedure ShowDetailWindow;
  end;

  TKExtDetailPanel = class(TKExtPanelControllerBase)
  private
    FViewTable: TKViewTable;
    FServerStore: TKViewTableStore;
    procedure SetViewTable(const AValue: TKViewTable);
  protected
    procedure InitDefaults; override;
    function GetObjectNamePrefix: string; override;
  public
    property ViewTable: TKViewTable read FViewTable write SetViewTable;
    property ServerStore: TKViewTableStore read FServerStore write FServerStore;
  end;

  /// <summary>
  ///  The Form controller.
  /// </summary>
  TKExtFormPanelController = class(TKExtDataPanelController)
  strict private
    FTabPanel: TExtTabPanel;
    FFormPanel: TKExtEditPanel;
    FMainPage: TKExtEditPage;
    FIsReadOnly: Boolean;
    FConfirmButton: TKExtButton;
    FApplyButton: TKExtButton;
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
    FDetailBottomPanel: TExtTabPanel;
    FChangesApplied: Boolean;
    procedure CreateEditors;
    procedure CreateButtons;
    procedure ChangeEditorsState;
    procedure StartOperation;
    procedure FocusFirstField;
    procedure CreateDetailPanels(const AContainer: TExtPanel);
    procedure CreateDetailToolbar;
    procedure CreateDetailBottomPanel;
    function GetDetailStyle: string;
//    function GetExtraHeight: Integer;
    function GetDetailBottomPanelHeight: Integer;
    procedure AssignFieldChangeEvent(const AAssign: Boolean);
    procedure FieldChange(const AField: TKField; const AOldValue, ANewValue: Variant);
    procedure CreateFormPanel;
    function LayoutContainsPageBreaks: Boolean;
    function GetConfirmJSCode(const AMethod: TJSProcedure): TJSExpression;
    procedure InitFlags;
    function FindLayout: TKLayout;
    function IsViewMode: Boolean;
    procedure SetStoreRecord(const AValue: TKViewTableRecord);
    procedure EnsureDetailController(const AContainer: IJSControllerContainer;
      const ADetailIndex: Integer);
    function CreateLayoutProcessor: TKExtLayoutProcessor;
    procedure SetConfirmButtonHandlers;
  strict protected
    procedure DoDisplay; override;
    procedure InitComponents; override;
    property StoreRecord: TKViewTableRecord read FStoreRecord write SetStoreRecord;
    function AddActionButton(const AUniqueId: string; const AView: TKView;
      const AToolbar: TKExtToolbar): TKExtActionButton; override;
    procedure TabChange(ATabPanel: TExtTabPanel; ANewTab, AOldTab: TExtComponent);
    procedure RefreshEditorValues;
    procedure RefreshEditorFields;
    procedure CloseHostContainer; override;
  public
    procedure LoadData; override;
    destructor Destroy; override;
    function GetFilterExpression: string; override;
    function GetRegionName(const ARegion: TExtBoxComponentRegion): string; override;
    procedure AfterConstruction; override;
    procedure ChangeRecord(const ARecord: TKViewTableRecord);
  //published
    procedure GetRecord;
    procedure SwitchToEditMode;
    procedure ConfirmChanges;
    procedure ApplyChanges;
    procedure ConfirmChangesAndClone;
    procedure CancelChanges;
  end;

implementation

uses
  StrUtils
  , Classes
  , Variants
  , Types
  , JSON
  , EF.Localization
  , EF.Types
  , EF.Intf
  , EF.DB
  , EF.JSON
  , EF.VariantUtils
  , EF.StrUtils
  , Kitto.Types
  , Kitto.AccessControl
  , Kitto.Rules
  , Kitto.SQL
  , Kitto.Config
  , Kitto.Web.Application
  , Kitto.Web.Request
  , Kitto.Web.Response
  , Kitto.Ext.Utils
  ;

{ TKExtFormPanelController }

procedure TKExtFormPanelController.ChangeEditorsState;
var
  LIsViewMode: Boolean;
  LIsInsertOperation: Boolean;
begin
  LIsViewMode := IsViewMode;
  LIsInsertOperation := MatchStr(FOperation, ['Add', 'Dup']);
  EditItems.AllEditors(
    procedure (AEditor: IKExtEditor)
    var
      LFormField: TExtFormField;
      LViewField: TKViewField;
      LIsReadOnly: Boolean;
    begin
      LViewField := ViewTable.FieldByAliasedName(AEditor.FieldName);
      if Assigned(LViewField) then
        LIsReadOnly := LIsViewMode or not LViewField.CanEditField(LIsInsertOperation)
      else
        LIsReadOnly := LIsViewMode;
      AEditor.SetReadOnly(LIsReadOnly);
      LFormField := AEditor.AsExtFormField;
      if not LIsReadOnly and (FFocusField = nil) and Assigned(LFormField) then
        FFocusField := LFormField;
    end);
end;

procedure TKExtFormPanelController.ChangeRecord(const ARecord: TKViewTableRecord);
begin
  StoreRecord := ARecord;
  StartOperation;
end;

procedure TKExtFormPanelController.CloseHostContainer;
begin
  if FChangesApplied then
    NotifyObservers('Confirmed');
  inherited;
end;

destructor TKExtFormPanelController.Destroy;
begin
  FreeAndNil(FCloneValues);
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
      FDetailButtons.Add(TKExtDetailFormButton.CreateAndAddToArray(FDetailToolbar.Items));
      FDetailButtons[I].ServerStore := StoreRecord.DetailStores[I];
      FDetailButtons[I].ViewTable := ViewTable.DetailTables[I];
    end;
    // FIXME: We should really always use just Tbar here, but it is causing AV when
    // emitting the response items, so we are temporarily switching to a different panel.
    if Assigned(FTabPanel) then
      FTabPanel.Tbar := FDetailToolbar
    else if Assigned(FFormPanel) then
      FFormPanel.Tbar := FDetailToolbar
    else
      Tbar := FDetailToolbar;
  end;
end;

procedure TKExtFormPanelController.CreateDetailBottomPanel;
begin
  Assert(ViewTable <> nil);
  Assert(FDetailControllers.Count = 0);
  Assert(Assigned(StoreRecord));
  Assert(not Assigned(FDetailBottomPanel));

  if ViewTable.DetailTableCount > 0 then
  begin
    FDetailBottomPanel := TExtTabPanel.CreateAndAddToArray(Items);
    FDetailBottomPanel.Split := True;
    FDetailBottomPanel.Region := rgSouth;
    FDetailBottomPanel.Border := False;
    FDetailBottomPanel.AutoScroll := False;
    FDetailBottomPanel.BodyStyle := 'background:none'; // Respects parent's background color.
    FDetailBottomPanel.DeferredRender := False;
    FDetailBottomPanel.EnableTabScroll := True;
    FDetailBottomPanel.Height := GetDetailBottomPanelHeight;
    FDetailBottomPanel.OnTabChange := TabChange;
    FDetailBottomPanel.On('tabchange', FDetailBottomPanel.GenerateAnonymousFunction(FDetailBottomPanel.JSName + '.updateLayout();'));
    CreateDetailPanels(FDetailBottomPanel);
    FDetailBottomPanel.SetActiveTab(0);
    // Workaround for missing tabchange event when activating the first tab.
    FDetailBottomPanel.FireEvent('tabchange', [FDetailBottomPanel, FDetailBottomPanel.Items[0]]);
  end;
end;

procedure TKExtFormPanelController.CreateDetailPanels(const AContainer: TExtPanel);
var
  I: Integer;
  LDetailPanel: TKExtDetailPanel;
begin
  Assert(ViewTable <> nil);
  Assert(FDetailControllers.Count = 0);
  Assert(Assigned(StoreRecord));

  if ViewTable.DetailTableCount > 0 then
  begin
    Assert(AContainer <> nil);
    StoreRecord.EnsureDetailStores;
    Assert(StoreRecord.DetailStoreCount = ViewTable.DetailTableCount);
    for I := 0 to ViewTable.DetailTableCount - 1 do
    begin
      FDetailControllers.Add(nil);
      LDetailPanel := TKExtDetailPanel.CreateAndAddToArray(AContainer.Items);
      LDetailPanel.ServerStore := StoreRecord.DetailStores[I];
      LDetailPanel.ViewTable := ViewTable.DetailTables[I];
    end;
  end;
end;

procedure TKExtFormPanelController.EnsureDetailController(const AContainer: IJSControllerContainer;
  const ADetailIndex: Integer);
var
  LController: IJSController;
  LControllerType: string;
begin
  Assert(FDetailControllers.Count > ADetailIndex);

  if not Assigned(FDetailControllers[ADetailIndex]) then
  begin
    LControllerType := ViewTable.GetString('Controller', 'GridPanel');
    // The node may exist and be '', which does not return the default value.
    if LControllerType = '' then
      LControllerType := 'GridPanel';
    LController := TKExtControllerFactory.Instance.CreateController(AContainer.AsJSObject,
      View, AContainer, ViewTable.FindNode('Controller'), Self, LControllerType);
    LController.Config.SetObject('Sys/ViewTable', ViewTable.DetailTables[ADetailIndex]);
    LController.Config.SetObject('Sys/ServerStore', StoreRecord.DetailStores[ADetailIndex]);
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
    FDetailControllers[ADetailIndex] := LController.AsObject;
    LController.Display;
    if not SameText(FOperation, 'Add') then
      if (LController.AsObject is TKExtDataPanelController) then
        TKExtDataPanelController(LController.AsObject).LoadData;
  end;
end;

function TKExtFormPanelController.CreateLayoutProcessor: TKExtLayoutProcessor;
begin
  Result := TKExtLayoutProcessor.Create;
  try
    Result.DataRecord := StoreRecord;
    Result.MainEditPanel := FFormPanel;
    Result.TabPanel := FTabPanel;
    Result.OnNewEditItem :=
      procedure (AEditItem: IKExtEditItem)
      var
        LSubject: IEFSubject;
      begin
        EditItems.Add(AEditItem.AsObject);
        if Supports(AEditItem.AsObject, IEFSubject, LSubject) then
          LSubject.AttachObserver(Self);
      end;
    Result.ForceReadOnly := FIsReadOnly;
    if MatchStr(FOperation, ['Add', 'Dup']) then
      Result.Operation := eoInsert
    else
      Result.Operation := eoUpdate;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

procedure TKExtFormPanelController.CreateEditors;
var
  LLayoutProcessor: TKExtLayoutProcessor;
begin
  Assert(Assigned(StoreRecord));

  LLayoutProcessor := CreateLayoutProcessor;
  try
    LLayoutProcessor.CurrentEditPage := FMainPage;
    LLayoutProcessor.CreateEditors(FindLayout, 0);
    FFocusField := LLayoutProcessor.FocusField;
  finally
    FreeAndNil(LLayoutProcessor);
  end;

  // Handlers must be generated after creating the editors as their code
  // depends from them (see GetConfirmJSCode).
  SetConfirmButtonHandlers;
end;

procedure TKExtFormPanelController.SetConfirmButtonHandlers;
begin
  if Assigned(FApplyButton) then
  begin
    FApplyButton.Handler := GetConfirmJSCode(ApplyChanges);
    FFormPanel.On('clientvalidation', GenerateAnonymousFunction('form, valid', FApplyButton.JSName + '.setDisabled(!valid);'));
  end;

  if Assigned(FConfirmButton) then
  begin
    FConfirmButton.Handler := GetConfirmJSCode(ConfirmChanges);
    FFormPanel.On('clientvalidation', GenerateAnonymousFunction('form, valid', FConfirmButton.JSName+'.setDisabled(!valid);'));
  end;

  if Assigned(FEditButton) then
    FEditButton.Handler := GetConfirmJSCode(SwitchToEditMode);

  if Assigned(FCloneButton) then
  begin
    FCloneButton.Handler := GetConfirmJSCode(ConfirmChangesAndClone);
    FFormPanel.On('clientvalidation', GenerateAnonymousFunction('form, valid', FCloneButton.JSName+'.setDisabled(!valid);'));
  end;
end;

function TKExtFormPanelController.GetDetailBottomPanelHeight: Integer;
begin
  Result := ViewTable.GetInteger('DetailTables/Controller/Style/Height', DEFAULT_DETAIL_PANEL_HEIGHT);
end;

function TKExtFormPanelController.GetDetailStyle: string;
begin
  Result := Config.GetString('DetailStyle');
  if Result = '' then
    Result := ViewTable.GetString('DetailTables/Controller/Style', DEFAULT_DETAIL_STYLE);
end;

procedure TKExtFormPanelController.LoadData;
var
  LDetailStyle: string;
begin
  LDetailStyle := GetDetailStyle;
  if SameText(LDetailStyle, 'Tabs') then
    CreateDetailPanels(FTabPanel)
  else if SameText(LDetailStyle, 'Popup') then
    CreateDetailToolbar
  else if SameText(LDetailStyle, 'Bottom') then
    CreateDetailBottomPanel;

  StartOperation;
end;

procedure TKExtFormPanelController.SetStoreRecord(const AValue: TKViewTableRecord);
begin
  FStoreRecord := AValue;
  Config.SetObject('Sys/Record', FStoreRecord);
  if Assigned(FStoreRecord) then
  begin
    FStoreRecord.OnSetTransientProperty :=
      procedure(ASubjectType, ASubjectName, APropertyName: string; AValue: Variant)
      begin
        if SameText(ASubjectType, 'Field') then
        begin
          EditItems.EditorsByViewField(TKViewTableField(FStoreRecord.FieldByName(ASubjectName)).ViewField,
            procedure (AEditor: IKExtEditor)
            begin
              AEditor.SetTransientProperty(APropertyName, AValue);
            end);
        end
        else
        begin
          EditItems.EditItemsById(ASubjectName,
            procedure (AEditItem: IKExtEditItem)
            begin
              AEditItem.SetTransientProperty(APropertyName, AValue);
            end);
        end;
      end;
    RefreshEditorFields;
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

  procedure MergeCallerDefaultValues;
  var
    LCallerDefaultValues: TEFNode;
  begin
    LCallerDefaultValues := Config.FindNode('Sys/DefaultValues');
    if Assigned(LCallerDefaultValues) then
      LDefaultValues.Merge(LCallerDefaultValues);
  end;

  function IsCloned: Boolean;
  begin
    Result := SameText(FOperation, 'Add') and Assigned(FCloneValues);
  end;

  procedure SwitchChangeNotificationsForDupAndClone(const AOn: Boolean);
  begin
    if SameText(FOperation, 'Dup') or IsCloned then
    begin
      if AOn then
        StoreRecord.Store.EnableChangeNotifications
      else
        StoreRecord.Store.DisableChangeNotifications;
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
          LDefaultValues := TEFNode.Clone(FCloneValues,
            // Don't copy PK values.
            procedure (const ASource, ADestination: TEFNode)
            var
              LViewField: TKViewField;
            begin
              LViewField := ViewTable.FindField(ASource.Name);
              if Assigned(LViewField) and LViewField.IsKey then
                ADestination.Value := Null;
            end
          );
          MergeKeyDefaultValues;
        end
        else
          LDefaultValues := ViewTable.GetDefaultValues;
        MergeCallerDefaultValues;
        SwitchChangeNotificationsForDupAndClone(False);
        try
          StoreRecord.ReadFromNode(LDefaultValues);
        finally
          SwitchChangeNotificationsForDupAndClone(True);
        end;
        StoreRecord.ApplyNewRecordRulesAndFireEvents(ViewTable, IsCloned);
      finally
        FreeAndNil(LDefaultValues);
      end;
    end;

    RefreshEditorValues;

    FocusFirstField;
  except
    on E: EKValidationError do
    begin
      ExtMessageBox.Alert(_(TKWebApplication.Current.Config.AppTitle), E.Message);
      CancelChanges;
    end;
  end;
end;

procedure TKExtFormPanelController.RefreshEditorValues;
begin
  // Load data. Combo boxes can only have their raw value set after they're rendered.
  EditItems.AllEditors(
    procedure (AEditor: IKExtEditor)
    var
      LFormField: TExtFormField;
    begin
      LFormField := AEditor.AsExtFormField;
      if Assigned(LFormField) then
      begin
        // Already rendered - call RefreshValue directly; otherwise postpone it.
        LFormField.RemoveAllListeners('afterrender');
        LFormField.On('afterrender', LFormField.GenerateAnonymousFunction(GetJSCode(
          procedure
          begin
            AEditor.RefreshValue;
          end)));
      end

      else
        AEditor.RefreshValue;
    end);
end;

procedure TKExtFormPanelController.RefreshEditorFields;
begin
  EditItems.AllEditors(
    procedure (AEditor: IKExtEditor)
    begin
      AEditor.RecordField := StoreRecord.FieldByName(AEditor.FieldName);
    end);
end;

procedure TKExtFormPanelController.SwitchToEditMode;
var
  LHostWindow: TExtWindow;
begin
  FStoreRecord.ApplyEditRecordRules;
  FEditButton.SetVisible(False);
  if Assigned(FApplyButton) then
    FApplyButton.SetVisible(True);
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

  TKWebResponse.Current.Items.AddJSON('{success:true,data:' + StoreRecord.GetAsJSON(False) + '}');
end;

procedure TKExtFormPanelController.ConfirmChanges;
var
  LError: string;
begin
  AssignFieldChangeEvent(False);
  LError := UpdateRecord(StoreRecord, TKWebRequest.Current.JSONContentTree.ChildByName('new'), '', True);

  FreeAndNil(FCloneValues);
  if LError = '' then
  begin
    FChangesApplied := True;
    if Config.GetBoolean('KeepOpenAfterOperation') then
    begin
      if SameText(FOperation, 'Add') then
        StoreRecord := ServerStore.AppendRecord(nil);
      StartOperation;
    end
    else
      CloseHostContainer;
  end
  else
    AssignFieldChangeEvent(True);
end;

procedure TKExtFormPanelController.AfterConstruction;
begin
  inherited;
  FChangesApplied := False;
  FDetailControllers := TObjectList<TObject>.Create(False);
end;

procedure TKExtFormPanelController.ApplyChanges;
var
  LError: string;
begin
  AssignFieldChangeEvent(False);
  LError := UpdateRecord(StoreRecord, TKWebRequest.Current.JSONContentTree.ChildByName('new'), '', True);

  if LError = '' then
  begin
    FChangesApplied := True;
    FOperation := 'Edit';
    StartOperation;
  end
  else
    AssignFieldChangeEvent(True);
end;

procedure TKExtFormPanelController.ConfirmChangesAndClone;
var
  LError: string;
begin
  AssignFieldChangeEvent(False);
  LError := UpdateRecord(StoreRecord, TKWebRequest.Current.JSONContentTree.ChildByName('new'), '', True);

  if LError = '' then
  begin
    FCloneValues := TEFNode.Clone(StoreRecord);
    StoreRecord := ServerStore.AppendRecord(nil);
    FOperation := 'Add';
    // recupera dati record
    StartOperation;
  end
  else
    AssignFieldChangeEvent(True);
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
  LApplyButtonNode: TEFNode;
  LToolbar: TKExtToolbar;
begin
  LToolbar := TKExtToolbar.Create(Self);
  TExtToolbarFill.CreateInlineAndAddToArray(LToolbar.Items);
  Fbar := LToolbar;

  // Apply button
  FApplyButton := nil;
  LApplyButtonNode := ViewTable.FindNode('Controller/FormController/ApplyButton');
  if Assigned(LApplyButtonNode) and not ViewTable.IsDetail then
  begin
    FApplyButton := TKExtButton.CreateAndAddToArray(LToolbar.Items);
    FApplyButton.SetIconAndScale('accept', Config.GetString('ButtonScale', 'medium'));

    if ViewTable.DetailTableCount > 0 then
      FApplyButton.Text := LApplyButtonNode.GetString('Caption', _('Apply all'))
    else
      FApplyButton.Text := LApplyButtonNode.GetString('Caption', _('Apply'));
    FApplyButton.Tooltip := LApplyButtonNode.GetString('Tooltip', _('Apply changes and keep editing'));
    FApplyButton.Hidden := FIsReadOnly or IsViewMode;
  end;

  // Clone button
  if not FIsReadOnly then
  begin
    LCloneButtonNode := Config.FindNode('CloneButton');
    if Assigned(LCloneButtonNode) then
    begin
      FCloneButton := TKExtButton.CreateAndAddToArray(LToolbar.Items);
      FCloneButton.SetIconAndScale('accept_clone', Config.GetString('ButtonScale', 'medium'));
      FCloneButton.Text := LCloneButtonNode.GetString('Caption', _('Save & Clone'));
      FCloneButton.Tooltip := LCloneButtonNode.GetString('Tooltip', _('Save changes and create a new clone record'));
      FCloneButton.Hidden := FIsReadOnly or IsViewMode;
    end
    else
      FCloneButton := nil;
  end;

  // Confirm button
  FConfirmButton := TKExtButton.CreateAndAddToArray(LToolbar.Items);
  if ViewTable.IsDetail then
    FConfirmButton.SetIconAndScale('accept', Config.GetString('ButtonScale', 'medium'))
  else if ViewTable.DetailTableCount = 0 then
    FConfirmButton.SetIconAndScale('save', Config.GetString('ButtonScale', 'medium'))
  else
    FConfirmButton.SetIconAndScale('save_all', Config.GetString('ButtonScale', 'medium'));

  if ViewTable.IsDetail then
    FConfirmButton.Text := Config.GetString('ConfirmButton/Caption', _('OK'))
  else if ViewTable.DetailTableCount > 0 then
    FConfirmButton.Text := Config.GetString('ConfirmButton/Caption', _('Save all'))
  else
    FConfirmButton.Text := Config.GetString('ConfirmButton/Caption', _('Save'));

  if ViewTable.IsDetail then
    FConfirmButton.Tooltip := Config.GetString('ConfirmButton/Tooltip', _('Confirm changes and finish editing'))
  else
    FConfirmButton.Tooltip := Config.GetString('ConfirmButton/Tooltip', _('Save changes and finish editing'));

  FConfirmButton.Hidden := FIsReadOnly or IsViewMode;

  if IsViewMode then
  begin
    FEditButton := TKExtButton.CreateAndAddToArray(LToolbar.Items);
    FEditButton.SetIconAndScale('edit_record', Config.GetString('ButtonScale', 'medium'));
    FEditButton.Text := Config.GetString('ConfirmButton/Caption', _('Edit'));
    FEditButton.Tooltip := Config.GetString('ConfirmButton/Tooltip', _('Switch to edit mode'));
    FEditButton.Hidden := FIsReadOnly;
  end;

  FCancelButton := TKExtButton.CreateAndAddToArray(LToolbar.Items);
  FCancelButton.SetIconAndScale('cancel', Config.GetString('ButtonScale', 'medium'));
  FCancelButton.Text := Config.GetString('CancelButton/Caption', _('Cancel'));
  FCancelButton.Tooltip := Config.GetString('CancelButton/Tooltip', _('Cancel changes'));
  //FCancelButton.Handler := Ajax(CancelChanges);
  FCancelButton.Handler := TKWebResponse.Current.Items.AjaxCallMethod(Self).SetMethod(CancelChanges).AsFunction;
  FCancelButton.Hidden := FIsReadOnly or IsViewMode;

  FCloseButton := TKExtButton.CreateAndAddToArray(LToolbar.Items);
  FCloseButton.SetIconAndScale('close', Config.GetString('ButtonScale', 'medium'));
  FCloseButton.Text := Config.GetString('CloseButton/Caption', _('Close'));
  FCloseButton.Tooltip := Config.GetString('CloseButton/Tooltip', _('Close this panel'));
  // No need for an ajax call when we just close the client-side panel.
  LHostWindow := GetHostWindow;
  if Assigned(LHostWindow) then
    FCloseButton.Handler := GenerateAnonymousFunction(LHostWindow.JSName + '.close();');
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
  LLabelAlign: TExtContainerLabelAlign;
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
      Assert(ServerStore.RecordCount = 1);
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
    LLabelAlign := laTop
  else if Assigned(LLabelAlignNode) then
    LLabelAlign := OptionAsLabelAlign(LLabelAlignNode.AsString)
  else if TKWebRequest.Current.IsMobileBrowser then
    LLabelAlign := laTop
  else
    LLabelAlign := laRight;
  if LLabelAlign <> LabelAlign then
    LabelAlign := LLabelAlign;
end;

procedure TKExtFormPanelController.CreateFormPanel;
var
  LDetailStyle: string;
begin
  Cls := 'x-panel-mc'; // Sets correct theme background color same as panel

  FFormPanel := TKExtEditPanel.CreateAndAddToArray(Items);
  FFormPanel.Region := rgCenter;
  FFormPanel.Border := False;
  FFormPanel.Header := False;
  FFormPanel.Layout := lyFit; // Vital to avoid detail grids with zero height!
  FFormPanel.AutoScroll := False;
  FFormPanel.LabelWidth := FORM_LABELWIDTH;
  FFormPanel.MonitorValid := True;
  FFormPanel.Cls := 'x-panel-mc'; // Sets correct theme background color.
  FFormPanel.LabelAlign := LabelAlign;

  LDetailStyle := GetDetailStyle;
  if ((ViewTable.DetailTableCount > 0) and SameText(LDetailStyle, 'Tabs')) or LayoutContainsPageBreaks then
  begin
    FTabPanel := TExtTabPanel.CreateAndAddToArray(FFormPanel.Items);
    FTabPanel.Border := False;
    FTabPanel.AutoScroll := False;
    FTabPanel.BodyStyle := 'background:none'; // Respects parent's background color.
    FTabPanel.DeferredRender := False;
    FTabPanel.EnableTabScroll := True;
    FMainPage := TKExtEditPage.CreateAndAddToArray(FTabPanel.Items);
    FMainPage.Title := _(ViewTable.DisplayLabel);
    if Config.GetBoolean('Sys/ShowIcon', True) then
      FMainPage.IconCls := TKWebApplication.Current.SetViewIconStyle(ViewTable.View);
    FMainPage.LabelAlign := LabelAlign;
    FTabPanel.OnTabChange := TabChange;
    FTabPanel.On('tabchange', FTabPanel.GenerateAnonymousFunction(FTabPanel.UpdateLayout));
    FTabPanel.SetActiveTab(0);
    // Workaround for missing tabchange event when activating the first tab.
    FTabPanel.FireEvent('tabchange', [FTabPanel, FTabPanel.Items[0]]);
  end
  else
  begin
    FTabPanel := nil;
    FMainPage := TKExtEditPage.CreateAndAddToArray(FFormPanel.Items);
    FMainPage.Region := rgCenter;
    FMainPage.LabelAlign := LabelAlign;
  end;
  FMainPage.HideLabels := Config.GetBoolean('HideLabels');
  // Scroll back to top - can't do that until afterrender because body.dom is needed.
  FMainPage.On('afterrender', GenerateAnonymousFunction(FMainPage.JSName + '.body.dom.scrollTop = 0;'));
end;

procedure TKExtFormPanelController.TabChange(ATabPanel: TExtTabPanel; ANewTab, AOldTab: TExtComponent);
var
  LViewTable: TKViewTable;
  LDetailIndex: Integer;
  LActivableIntf: IKExtActivable;
  LLayoutProcessor: TKExtLayoutProcessor;
begin
  if Assigned(ANewTab) then
  begin
    if (ANewTab is TKExtDetailPanel) and (TKExtDetailPanel(ANewTab).Items.Count = 0) then
    begin
      LViewTable := TKExtDetailPanel(ANewTab).ViewTable;
      Assert(Assigned(LViewTable));
      LDetailIndex := ViewTable.GetDetailTableIndex(LViewTable);
      Assert(LDetailIndex >= 0);
      EnsureDetailController(TKExtDetailPanel(ANewTab), LDetailIndex);
      if Supports(FDetailControllers[LDetailIndex], IKExtActivable, LActivableIntf) then
        LActivableIntf.Activate;
    end
    else if (ANewTab is TKExtEditPage) and not TKExtEditPage(ANewTab).Rendered then
    begin
      LLayoutProcessor := CreateLayoutProcessor;
      try
        LLayoutProcessor.CurrentEditPage := TKExtEditPage(ANewTab);
        LLayoutProcessor.CreateEditors(FindLayout, TKExtEditPage(ANewTab).PageIndex);
      finally
        FreeAndNil(LLayoutProcessor);
      end;
      // Newly-generated editors must show the record values. Actually refreshing
      // all editors currently; seems no big deal performance-wise.
      RefreshEditorValues;
      // Handlers must be re-generated now as their code depends from newly added
      // editors (see GetConfirmJSCode).
      SetConfirmButtonHandlers;
    end;

    if Supports(ANewTab, IKExtActivable, LActivableIntf) then
      LActivableIntf.Activate;
  end;
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

  ChangesCanceled(StoreRecord);

  if MatchText(FOperation, ['Add', 'Dup']) then
  begin
    ServerStore.RemoveRecord(StoreRecord);
    StoreRecord := nil;
  end
  else if SameText(FOperation, 'Edit') then
  begin
    StoreRecord.Store.DoWithChangeNotificationsDisabled(
      procedure
      begin
        StoreRecord.Refresh;
      end);
  end;

  if LKeepOpen then
  begin
    if SameText(FOperation, 'Add') then
      StoreRecord := ServerStore.AppendRecord(nil)
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
  EditItems.EditorsByViewField(LField.ViewField,
    procedure (AEditor: IKExtEditor)
    begin
      AEditor.RefreshValue;
    end);

  // Give all non-editors a chance to refresh (such as a FieldSet which might
  // need to refresh its title). This might be a performance bottleneck.
  EditItems.AllNonEditors(
    procedure (AEditItem: IKExtEditItem)
    begin
      AEditItem.RefreshValue;
    end);
end;

function TKExtFormPanelController.FindLayout: TKLayout;
var
  LPrefix: string;
begin
  LPrefix := Config.GetString('LayoutNamePrefix');
  Result := FindViewLayout(Config.GetString('Layout', LPrefix + 'Form'));
  // Fallback to standard layout if no prefixed layout is available.
  if (Result = nil) and (LPrefix <> '') then
    Result := FindViewLayout(Config.GetString('Layout', 'Form'));
end;

function TKExtFormPanelController.GetRegionName(const ARegion: TExtBoxComponentRegion): string;
begin
  Result := inherited GetRegionName(ARegion);
  if Config.GetObject('Sys/CallingController') <> nil then
    Result := 'SecondaryController/' + Result;
end;

function TKExtFormPanelController.GetConfirmJSCode(const AMethod: TJSProcedure): TJSExpression;
begin
  Result := GenerateAnonymousFunction(GetJSCode(
    procedure
    begin
      TKWebResponse.Current.Items.ExecuteJSCode(
        'var json = new Object;' + sLineBreak +
        'json.new = new Object;');
      EditItems.AllEditors(
        procedure (AEditor: IKExtEditor)
        begin
          AEditor.StoreValue('json.new');
        end);
      TKWebResponse.Current.Items.AjaxCallMethod(Self).SetMethod(AMethod)
        .Post('json')
        .AsExpression;
    end));
end;

{ TKExtDetailFormButton }

procedure TKExtDetailFormButton.SetViewTable(const AValue: TKViewTable);
begin
  FViewTable := AValue;
  if Assigned(FViewTable) then
  begin
    Text := _(FViewTable.PluralDisplayLabel);
    Icon := TKWebApplication.Current.GetImageURL(FViewTable.ImageName);
    //Handler := Ajax(ShowDetailWindow, []);
    Handler := TKWebResponse.Current.Items.AjaxCallMethod(Self).SetMethod(ShowDetailWindow).AsFunction;
  end;
end;

procedure TKExtDetailFormButton.ShowDetailWindow;
var
  LController: IJSController;
begin
  Assert(Assigned(FViewTable));

  if Assigned(FDetailHostWindow) then
  begin
    FDetailHostWindow.Delete;
    FreeAndNil(FDetailHostWindow);
  end;
  FDetailHostWindow := TKExtModalWindow.Create(Self);

  FDetailHostWindow.Title := _(ViewTable.PluralDisplayLabel);
  FDetailHostWindow.Closable := True;

  LController := TKExtControllerFactory.Instance.CreateController(
    FDetailHostWindow, FViewTable.View, FDetailHostWindow);
  LController.Config.SetObject('Sys/ServerStore', ServerStore);
  LController.Config.SetObject('Sys/ViewTable', ViewTable);
  LController.Config.SetObject('Sys/HostWindow', FDetailHostWindow);
  FDetailHostWindow.SetSizeFromTree(FViewTable, 'Controller/PopupWindow/');
  LController.Display;
  FDetailHostWindow.Show;
end;

{ TKExtDetailPanel }

function TKExtDetailPanel.GetObjectNamePrefix: string;
begin
  Result := 'detailpnl';
end;

procedure TKExtDetailPanel.InitDefaults;
begin
  inherited;
  Layout := lyFit;
end;

procedure TKExtDetailPanel.SetViewTable(const AValue: TKViewTable);
begin
  FViewTable := AValue;
  if Assigned(FViewTable) then
  begin
    Title := _(FViewTable.PluralDisplayLabel);
    IconCls := TKWebApplication.Current.SetIconStyle(FViewTable.ImageName);
  end;
end;

initialization
  TKExtControllerRegistry.Instance.RegisterClass('Form', TKExtFormPanelController);

finalization
  TKExtControllerRegistry.Instance.UnregisterClass('Form');

end.

