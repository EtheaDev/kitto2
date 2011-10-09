unit Kitto.Ext.Form;

{$I Kitto.Defines.inc}

interface

uses
  Generics.Collections,
  Ext, ExtData, ExtForm,
  EF.ObserverIntf,
  Kitto.Ext.Base, Kitto.Ext.DataPanel, Kitto.Ext.Editors,
  Kitto.Metadata.Views, Kitto.Ext.Controller, Kitto.Store;

type
  ///	<summary>
  ///	  A button that opens a popup detail form.
  ///	</summary>
  TKExtDetailFormButton = class(TExtButton)
  private
    FViewTable: TKViewTable;
    FDetailHostWindow: TKExtModalWindow;
    FView: TKDataView;
    FController: IKExtController;
    procedure SetViewTable(const AValue: TKViewTable);
  public
    destructor Destroy; override;
    property ViewTable: TKViewTable read FViewTable write SetViewTable;
    property View: TKDataView read FView write FView;
  published
    procedure ShowDetailWindow;
  end;

  ///	<summary>
  ///	  The Form controller.
  ///	</summary>
  TKExtFormPanelController = class(TKExtDataPanelController)
  private
    FFormPanel: TKExtEditPanel;
    FIsReadOnly: Boolean;
    //FEditors: TList<IKExtEditor>;
    FSaveButton: TExtButton;
    FCancelButton: TExtButton;
    FDetailToolbar: TExtToolbar;
    FDetailButtons: TObjectList<TKExtDetailFormButton>;
    FOperation: string;
    FFocusField: TExtFormField;
    FStoreRecord: TKRecord;
    procedure CreateDetailToolbar;
    procedure CreateEditors(const AForceReadOnly: Boolean);
    procedure FocusFirstEditor;
  protected
    procedure LoadData; override;
    procedure InitComponents; override;
    procedure DoDisplay; override;
  public
    destructor Destroy; override;
  published
    procedure GetRecord;
    procedure SaveChanges;
    procedure CancelChanges;
  end;

implementation

uses
  SysUtils, StrUtils,
  EF.Localization, EF.Types, EF.Intf,
  Kitto.Environment, Kitto.AccessControl, Kitto.Ext.Session, Kitto.Ext.Utils,
  Kitto.JSON;

{ TKExtFormPanelController }

destructor TKExtFormPanelController.Destroy;
begin
  FreeAndNil(FDetailButtons);
  inherited;
end;

procedure TKExtFormPanelController.DoDisplay;
begin
  FStoreRecord := Config.GetObject('Sys/Record') as TKRecord;
  Assert(Assigned(FStoreRecord));
  inherited;
  Title := ViewTable.DisplayLabel;
end;

procedure TKExtFormPanelController.CreateDetailToolbar;
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
    end;
    Tbar := FDetailToolbar;
  end;
end;

procedure TKExtFormPanelController.CreateEditors(const AForceReadOnly: Boolean);
var
  LLayoutProcessor: TKExtLayoutProcessor;
  LLayoutName: string;
begin
//  FreeAndNil(FEditors);
//  FEditors := TList<IKExtEditor>.Create;
  LLayoutProcessor := TKExtLayoutProcessor.Create;
  try
    LLayoutProcessor.ViewTable := ViewTable;
    LLayoutProcessor.StoreRecord := FStoreRecord;
    LLayoutProcessor.FormPanel := FFormPanel;
//    LLayoutProcessor.OnNewEditor :=
//      procedure (AEditor: IKExtEditor)
//      begin
//        FEditors.Add(AEditor);
//      end;
    LLayoutProcessor.ForceReadOnly := AForceReadOnly;

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

procedure TKExtFormPanelController.LoadData;
begin
  inherited;
  CreateEditors(FIsReadOnly);

  //DataSet.RecreateDetailDataSetLists;
  CreateDetailToolbar;

  Session.JSCode(
    FFormPanel.JSName + '.getForm().load({url:"' + MethodURI(GetRecord) + '",' +
      'failure: function(form, action) { Ext.Msg.alert("' + _('Load failed.') + '", action.result.errorMessage);}});');

  //EnterEditMode;
end;

procedure TKExtFormPanelController.FocusFirstEditor;
begin
  if Assigned (FFocusField) then
    FFocusField.Focus(False, 500);
end;

procedure TKExtFormPanelController.GetRecord;
begin
  if Assigned(FStoreRecord) then
    Session.Response := '{success:true,data:' + FStoreRecord.GetAsJSON + '}'
  else
    Session.Response := '{success:false}';
end;

procedure TKExtFormPanelController.SaveChanges;
//var
//  LValue: string;
//  LEditor: IKExtEditor;
//  LField: TField;
begin
//  for LEditor in FEditors do
//  begin
//    LValue := Session.Query[LEditor.AsExtFormField.Name];
{ TODO : implement }
//    LField := DataSet.FieldByName(LEditor.AsExtFormField.Name);
//    if LField is TDateTimeField then
//    begin
//      if LValue = '' then
//        LField.Clear
//      else
//        LField.AsDateTime := StrToDate(LValue, Session.FormatSettings);
//      { TODO : support boolean values mapping }
//    end
//    else
//      LField.AsString := LValue;
//  end;
//  if not ViewTable.IsDetail then
//  begin
//    { TODO : support AIsInsertingMasterRecord argument. }
//    ServerStore.WriteChanges(SameText(FOperation, 'Add'));
//    ExtMessageBox.Alert(Title, 'Changes saved succesfully');
//  end;
  NotifyObservers('Confirmed');
  if not CloseHostWindow then
    FocusFirstEditor;
end;

procedure TKExtFormPanelController.InitComponents;
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
  if SameText(FOperation, 'Add') and FIsReadOnly then
    raise EEFError.Create(_('Operation Add not supported on read-only data.'));

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

procedure TKExtFormPanelController.CancelChanges;
begin
  NotifyObservers('Canceled');
  if not CloseHostWindow then
    FocusFirstEditor;
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
  FController := TKExtControllerFactory.Instance.CreateController(FView, FDetailHostWindow);
  FController.Display;
  FDetailHostWindow.Show;
end;

initialization
  TKExtControllerRegistry.Instance.RegisterClass('Form', TKExtFormPanelController);

finalization
  TKExtControllerRegistry.Instance.UnregisterClass('Form');

end.
