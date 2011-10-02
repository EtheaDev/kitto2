unit Kitto.Ext.Form;

interface

uses
  Generics.Collections, DB,
  Ext, ExtData, ExtForm,
  EF.ObserverIntf, EF.Data,
  Kitto.Ext.Base, Kitto.DataSetTree, Kitto.Ext.DataPanel, Kitto.Ext.Editors,
  Kitto.Metadata.Views, Kitto.controller;

type
  ///	<summary>
  ///	  A button that opens a popup detail form.
  ///	</summary>
  TKExtDetailFormButton = class(TExtButton)
  private
    FViewTable: TKViewTable;
    FDetailHostWindow: TKExtModalWindow;
    FView: TKDataView;
    FDataSetTree: TKDataSetTree;
    FMasterDataSet: TKMasterDataSet;
    FController: IKController;
    procedure SetViewTable(const AValue: TKViewTable);
  public
    destructor Destroy; override;
    property ViewTable: TKViewTable read FViewTable write SetViewTable;
    property DataSetTree: TKDataSetTree read FDataSetTree write FDataSetTree;
    property MasterDataSet: TKMasterDataSet read FMasterDataSet write FMasterDataSet;
    property View: TKDataView read FView write FView;
  published
    procedure ShowDetailWindow;
  end;

  ///	<summary>
  ///	  The Form controller.
  ///	</summary>
  TKExtFormPanel = class(TKExtDataPanel)
  private
    FFormPanel: TKExtEditPanel;
    FIsReadOnly: Boolean;
    FEditors: TList<IKExtEditor>;
    FSaveButton: TExtButton;
    FCancelButton: TExtButton;
    FDetailToolbar: TExtToolbar;
    FDetailButtons: TObjectList<TKExtDetailFormButton>;
    FOperation: string;
    FFocusEditor: IKExtEditor;
    procedure CreateDetailToolbar;
    procedure CreateEditors(const AForceReadOnly: Boolean);
    procedure DataToEditors;
    procedure EnterEditMode;
  protected
    procedure OpenDataSet; override;
    procedure InitComponents; override;
    procedure DoDisplay; override;
  public
    destructor Destroy; override;
  published
    procedure SaveChanges;
    procedure CancelChanges;
  end;

implementation

uses
  SysUtils, StrUtils,
  EF.Localization, EF.Types, EF.Intf,
  Kitto.Environment, Kitto.AccessControl, Kitto.Ext.Session, Kitto.Ext.Utils,
  Kitto.JSON;

{ TKExtFormPanel }

destructor TKExtFormPanel.Destroy;
begin
  FreeAndNil(FEditors);
  FreeAndNil(FDetailButtons);
  inherited;
end;

procedure TKExtFormPanel.DoDisplay;
begin
  inherited;
  Title := ViewTable.DisplayLabel;
end;

procedure TKExtFormPanel.CreateDetailToolbar;
var
  I: Integer;
begin
  Assert(ViewTable <> nil);
  Assert(FDetailToolbar = nil);

  FreeAndNil(FDetailButtons);

  if ViewTable.DetailTableCount > 0 then
  begin
    FDetailToolbar := TExtToolbar.Create;
    FDetailButtons := TObjectList<TKExtDetailFormButton>.Create(False);
    for I := 0 to ViewTable.DetailTableCount - 1 do
    begin
      FDetailButtons.Add(TKExtDetailFormButton.AddTo(FDetailToolbar.Items));
      FDetailButtons[I].ViewTable := ViewTable.DetailTables[I];
      FDetailButtons[I].View := View;
      FDetailButtons[I].DataSetTree := DataSetTree;
      FDetailButtons[I].MasterDataSet := DataSet;
    end;
    Tbar := FDetailToolbar;
  end;
end;

procedure TKExtFormPanel.CreateEditors(const AForceReadOnly: Boolean);
var
  LLayoutProcessor: TKExtLayoutProcessor;
  LLayoutName: string;
begin
  FreeAndNil(FEditors);
  if DataSet.Active then
  begin
    FEditors := TList<IKExtEditor>.Create;
    LLayoutProcessor := TKExtLayoutProcessor.Create;
    try
      LLayoutProcessor.ViewTable := ViewTable;
      LLayoutProcessor.DataSet := DataSet;
      LLayoutProcessor.FormPanel := FFormPanel;
      LLayoutProcessor.OnNewEditor :=
        procedure (AEditor: IKExtEditor)
        begin
          FEditors.Add(AEditor);
        end;
      LLayoutProcessor.ForceReadOnly := AForceReadOnly;

      LLayoutName := ViewTable.GetString('Controller/Form/Layout');
      if LLayoutName <> '' then
        LLayoutProcessor.CreateEditors(View.Catalog.Layouts.FindLayout(LLayoutName))
      else
        LLayoutProcessor.CreateEditors(ViewTable.FindLayout('Form'));
      FFocusEditor := LLayoutProcessor.FocusEditor;
    finally
      FreeAndNil(LLayoutProcessor);
    end;
    // Scroll back to top - can't do that until afterrender because body.dom is needed.
    FFormPanel.On('afterrender', JSFunction(FFormPanel.JSName + '.body.dom.scrollTop = 0;'));
  end;
end;

procedure TKExtFormPanel.OpenDataSet;
begin
  inherited;
  if SameText(FOperation, 'Add') and FIsReadOnly then
    raise EEFError.Create(_('Operation Add not supported on read-only form.'));

  CreateEditors(FIsReadOnly);

  DataSet.RecreateDetailDataSetLists;
  CreateDetailToolbar;

  EnterEditMode;
end;

procedure TKExtFormPanel.EnterEditMode;
begin
  if not FIsReadOnly then
  begin
    if SameText(FOperation, 'Add') then
      DataSet.Append
    else
      DataSet.Edit;
  end;
  // Do this always, as Append might have set default values.
  DataToEditors;
  if Assigned (FFocusEditor) then
    FFocusEditor.AsExtFormField.Focus(False, 500);
end;

procedure TKExtFormPanel.SaveChanges;
var
  LValue: string;
  LEditor: IKExtEditor;
  LField: TField;
begin
  for LEditor in FEditors do
  begin
    LValue := Session.Query[LEditor.AsExtFormField.Name];
    LField := DataSet.FieldByName(LEditor.AsExtFormField.Name);
    if LField is TDateTimeField then
    begin
      if LValue = '' then
        LField.Clear
      else
        LField.AsDateTime := StrToDate(LValue, Session.FormatSettings);
      { TODO : support boolean values mapping }
    end
    else
      LField.AsString := LValue;
  end;
  if not ViewTable.IsDetail then
  begin
    { TODO : support AIsInsertingMasterRecord argument. }
    DataSetTree.WriteChanges(SameText(FOperation, 'Add'));
    ExtMessageBox.Alert(Title, 'Changes saved succesfully');
  end;
  NotifyObservers('Confirmed');
  if not CloseHostWindow then
    EnterEditMode;
end;

procedure TKExtFormPanel.InitComponents;
var
  LHostWindow: TExtWindow;
begin
  inherited;
  FOperation := Config.GetString('Sys/Operation');
  if FOperation = '' then
    FOperation := View.GetString('Controller/Operation');

  if SameText(FOperation, 'Add') then
    FIsReadOnly := View.GetBoolean('IsReadOnly') or View.GetBoolean('Controller/PreventAdding')
      { TODO : implement }
      //or not Environment.IsAccessGranted(View.GetResourceURI, ACM_ADD)
  else
    FIsReadOnly := View.GetBoolean('IsReadOnly') or ViewTable.IsReadOnly or View.GetBoolean('Controller/PreventEditing');
      { TODO : implement }
      //or not Environment.IsAccessGranted(View.GetResourceURI, ACM_MODIFY);

  ExtQuickTips.Init(True);

  FFormPanel := TKExtEditPanel.AddTo(Items);
  FFormPanel.Region := rgCenter;
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
    FSaveButton.Scale := View.GetString('Controller/ButtonScale', 'medium');
    FSaveButton.FormBind := True;
    FSaveButton.Text := _('Save');
    FSaveButton.Tooltip := _('Save changes and finish editing');
    FSaveButton.Icon := Environment.GetImageURL('ok');
    FSaveButton.Handler := AjaxForms(SaveChanges, [FFormPanel]);
    //FSaveButton.Handler := JSFunction(FFormPanel.JSName + '.getForm().doAction("submit", {success:"AjaxSuccess", failure:"AjaxFailure"});');
  end;
  FCancelButton := TExtButton.AddTo(FFormPanel.Buttons);
  FCancelButton.Scale := View.GetString('Controller/ButtonScale', 'medium');
  if FIsReadOnly then
  begin
    FCancelButton.Text := _('Close');
    FCancelButton.Tooltip := _('Close this panel');
    FCancelButton.Icon := Environment.GetImageURL('close');
    // No need for an ajax call when we just close the client-side panel.
    LHostWindow := GetHostWindow;
    if Assigned(LHostWindow) then
      FCancelButton.Handler := JSFunction(LHostWindow.JSName + '.close();');
  end
  else
  begin
    FCancelButton.Text := _('Cancel');
    FCancelButton.Tooltip := _('Cancel changes');
    FCancelButton.Icon := Environment.GetImageURL('cancel');
    FCancelButton.Handler := Ajax(CancelChanges);
  end;
end;

procedure TKExtFormPanel.CancelChanges;
begin
  DataSet.Cancel;
  NotifyObservers('Canceled');
  if not CloseHostWindow then
    EnterEditMode;
end;

procedure TKExtFormPanel.DataToEditors;
var
  LEditor: IKExtEditor;
begin
  for LEditor in FEditors do
    LEditor.SetValueFromField(DataSet.FieldByName(LEditor.AsExtFormField.Name));
end;

{ TKExtDetailFormButton }

destructor TKExtDetailFormButton.Destroy;
begin
  FreeAndNil(FDetailHostWindow);
  FreeAndNilEFIntf(FController);
  inherited;
end;

procedure TKExtDetailFormButton.SetViewTable(const AValue: TKViewTable);
begin
  FViewTable := AValue;
  if Assigned(FViewTable) then
  begin
    Text := FViewTable.DisplayLabel;
    //Icon :=
    Handler := Ajax(ShowDetailWindow, []);
  end;
end;

procedure TKExtDetailFormButton.ShowDetailWindow;
begin
  Assert(Assigned(FViewTable));

  FreeAndNil(FDetailHostWindow);
  FDetailHostWindow := TKExtModalWindow.Create;
  { TODO : use a master-record-specific title - define a way to specify a record's caption }
  FDetailHostWindow.Title := ViewTable.DisplayLabel;
  FDetailHostWindow.Closable := True;

  FreeAndNil(FController);
  FController := TKControllerFactory.Instance.CreateController(FView);
  FController.Config.SetObject('Sys/Container', FDetailHostWindow);
  FController.Config.SetObject('Sys/DataSetTree', DataSetTree);
  FController.Config.SetObject('Sys/DataSet', MasterDataSet.DetailDataSetListByViewTable(ViewTable).GetDataSetByMasterKey(MasterDataSet));
  FController.Display;
  FDetailHostWindow.Show;
end;

initialization
  TKControllerRegistry.Instance.RegisterClass('Form', TKExtFormPanel);

finalization
  TKControllerRegistry.Instance.UnregisterClass('Form');

end.
