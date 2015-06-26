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

unit Kitto.Ext.DataPanel;

{$I Kitto.Defines.inc}

interface

uses
  Generics.Collections,
  Ext, ExtPascal, ExtPascalUtils, ExtData,
  superobject,
  EF.Classes, EF.ObserverIntf,
  Kitto.Metadata.Views, Kitto.Metadata.DataView, Kitto.Store, Kitto.Types,
  Kitto.Ext.Base, Kitto.Ext.Controller, Kitto.Ext.BorderPanel, Kitto.Ext.Editors;

type
  TKExtGetServerRecordEvent = reference to function: TKViewTableRecord;

  TKExtDataActionButton = class(TKExtActionButton)
  strict private
    FServerStore: TKViewTableStore;
    FViewTable: TKViewTable;
    FOnGetServerRecord: TKExtGetServerRecordEvent;
    FServerRecord: TKViewTableRecord;
    function GetServerRecord: TKViewTableRecord;
  strict protected
    procedure InitController(const AController: IKExtController); override;
  public
    property ViewTable: TKViewTable read FViewTable write FViewTable;
    property ServerStore: TKViewTableStore read FServerStore write FServerStore;
    property ServerRecord: TKViewTableRecord read GetServerRecord;
    property OnGetServerRecord: TKExtGetServerRecordEvent read FOnGetServerRecord write FOnGetServerRecord;
  published
    procedure ExecuteButtonAction; override;
  end;

  /// <summary>
  ///  Base class for controllers that handle database records (either
  ///  single records or record sets).
  /// </summary>
  TKExtDataPanelController = class abstract(TKExtBorderPanelController)
  strict private
    FServerStore: TKViewTableStore;
    FClientStore: TExtDataStore;
    FClientReader: TExtDataJsonReader;
    FViewTable: TKViewTable;
    FOwnsServerStore: Boolean;
    FVisibleActions: TDictionary<string, Boolean>;
    FAllowedActions: TDictionary<string, Boolean>;
    FEditHostWindow: TKExtModalWindow;
    FNewButton: TKExtButton;
    FEditButton: TKExtButton;
    FViewButton: TKExtButton;
    FDeleteButton: TKExtButton;
    FDupButton: TKExtButton;
    function GetView: TKDataView;
    function GetMaxRecords: Integer;
    function GetDefaultAutoOpen: Boolean;
    procedure SetupURLFields(const ARecord: TKViewTableRecord);
  strict protected
    FButtonsRequiringSelection: TList<TExtObject>;
    FEditItems: TKEditItemList;
    procedure CheckCanRead;
    function GetOrderByClause: string; virtual;
    procedure SetViewTable(const AValue: TKViewTable); virtual;
    procedure CreateToolbar; virtual;
    procedure DoDisplay; override;
    procedure InitComponents; virtual;
    procedure InitDefaults; override;
    procedure InitSubController(const AController: IKExtController); override;
    procedure AddTopToolbarButtons; override;
    function AddTopToolbarButton(const AActionName, ATooltip, AImageName: string;
      const ARequiresSelection: Boolean): TKExtButton;
    property View: TKDataView read GetView;
    property ClientStore: TExtDataStore read FClientStore;
    property ClientReader: TExtDataJsonReader read FClientReader;
    function CreateClientStore: TExtDataStore; virtual;
    function CreateClientReader: TExtDataJsonReader; virtual;
    function AddActionButton(const AUniqueId: string; const AView: TKView;
      const AToolbar: TKExtToolbar): TKExtActionButton; override;
    function GetSelectConfirmCall(const AMessage: string;
      const AMethod: TExtProcedure): string; virtual;
    function GetSelectCall(const AMethod: TExtProcedure): TExtFunction; virtual;
    function AutoLoadData: Boolean; virtual;
    function GetParentDataPanel: TKExtDataPanelController;
    function GetRootDataPanel: TKExtDataPanelController;
    function FindViewLayout(const ALayoutName: string): TKLayout;
    function UpdateRecord(const ARecord: TKVIewTableRecord; const ANewValues: ISuperObject;
      const APersist, ASuppressNotification: Boolean): string;
    function GetDefaultRemoteSort: Boolean; virtual;
    function GetCurrentViewRecord: TKViewTableRecord;
    procedure ShowEditWindow(const ARecord: TKRecord; const AEditMode: TKEditMode);
    function GetEditWindowDefaultControllerType: string; virtual;
    function IsMultiSelect: Boolean; virtual;
    function HasDefaultAction: Boolean;
    function GetExplicitDefaultAction: string;
    function HasExplicitDefaultAction: Boolean;
    function GetImplicitDefaultAction: string;
    function IsLookupMode: Boolean;
    procedure BeforeCreateTopToolbar; override;
    function IsActionVisible(const AActionName: string): Boolean; virtual;
    function IsActionAllowed(const AActionName: string): Boolean; virtual;
    function IsActionSupported(const AActionName: string): Boolean; virtual;
    function GetRowButtonsDisableJS: string;
    procedure ExecuteNamedAction(const AActionName: string); override;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
    property ViewTable: TKViewTable read FViewTable write SetViewTable;
    property ServerStore: TKViewTableStore read FServerStore write FServerStore;
    property DefaultAutoOpen: Boolean read GetDefaultAutoOpen;
    function GetFilterExpression: string; virtual;
    procedure UpdateObserver(const ASubject: IEFSubject; const AContext: string = ''); override;
  published
    procedure GetRecordPage;
    procedure GetImage;
    procedure LoadData; virtual; abstract;
    procedure EditRecord;
    procedure ViewRecord;
    procedure DefaultAction;
    procedure DuplicateRecord;
    procedure NewRecord;
    procedure DeleteCurrentRecord;
  end;

implementation

uses
  SysUtils, StrUtils, Math, Types, Classes,
  EF.StrUtils, EF.SysUtils, EF.Tree, EF.Localization,
  Kitto.AccessControl, Kitto.Config, Kitto.Rules, Kitto.SQL,
  Kitto.Ext.Session, KItto.Ext.Utils;

{ TKExtDataPanelController }

procedure TKExtDataPanelController.ExecuteNamedAction(const AActionName: string);
begin
  { TODO : check AC? }
  if (AActionName = 'Add') then
    FNewButton.PerformClick
  else if (AActionName = 'Edit') then
    FEditButton.PerformClick
  else if (AActionName = 'View') then
    FViewButton.PerformClick
  else if (AActionName = 'Delete') then
    FDeleteButton.PerformClick
  else if (AActionName = 'Dup') then
    FDupButton.PerformClick
  else
    inherited;
end;

procedure TKExtDataPanelController.DefaultAction;
var
  LActionName: string;
begin
  LActionName := GetExplicitDefaultAction;
  if LActionName = '' then
    LActionName := GetImplicitDefaultAction;
  if LActionName <> '' then
    ExecuteNamedAction(LActionName);
end;

procedure TKExtDataPanelController.DeleteCurrentRecord;
var
  LRecord: TKViewTableRecord;
begin
  Assert(ViewTable <> nil);

  // Apply BEFORE rules now even though actual save migh be deferred.
  LRecord := GetCurrentViewRecord;
  LRecord.MarkAsDeleted;
  try
    LRecord.ApplyBeforeRules;
  except
    on E: EKValidationError do
    begin
      LRecord.MarkAsClean;
      ExtMessageBox.Alert(_(Session.Config.AppTitle), E.Message);
      Exit;
    end;
  end;

  if not ViewTable.IsDetail then
  begin
    ViewTable.Model.SaveRecord(LRecord, not ViewTable.IsDetail, nil);
    Session.Flash(Format(_('%s deleted.'), [_(ViewTable.DisplayLabel)]));
  end;
  LoadData;
end;

destructor TKExtDataPanelController.Destroy;
begin
  if FOwnsServerStore then
    FreeAndNil(FServerStore);
  FreeAndNil(FAllowedActions);
  FreeAndNil(FVisibleActions);
  FreeAndNil(FEditItems);
  FreeAndNil(FButtonsRequiringSelection);
  inherited;
end;

function TKExtDataPanelController.GetView: TKDataView;
begin
  Result := inherited GetView as TKDataView;
end;

function TKExtDataPanelController.HasDefaultAction: Boolean;
var
  LDefaultAction: string;
begin
  Result := IsLookupMode;
  if not Result then
  begin
    LDefaultAction := GetExplicitDefaultAction;
    Result := LDefaultAction <> '';
    if not Result then
      Result := IsActionAllowed('View') or IsActionAllowed('Edit');
  end;
end;

function TKExtDataPanelController.HasExplicitDefaultAction: Boolean;
begin
  Result := GetExplicitDefaultAction <> '';
end;

procedure TKExtDataPanelController.DoDisplay;
var
  LViewTable: TKViewTable;
begin
  Assert(View is TKDataView);

  LViewTable := Config.GetObject('Sys/ViewTable') as TKViewTable;
  if LViewTable = nil then
    LViewTable := View.MainTable;
  Assert(Assigned(LViewTable));

  if Config.GetBoolean('Sys/ShowIcon', True) then
    IconCls := Session.SetViewIconStyle(View, LViewTable.Model.ImageName);

  FServerStore := Config.GetObject('Sys/ServerStore') as TKViewTableStore;
  if FServerStore = nil then
  begin
    FServerStore := LViewTable.CreateStore;
    FOwnsServerStore := True;
  end
  else
    FOwnsServerStore := False;
  Assert(Assigned(FServerStore));

  ViewTable := LViewTable;

  inherited; // Creates subcontrollers.

  InitComponents;
end;

procedure TKExtDataPanelController.DuplicateRecord;
begin
  ShowEditWindow(GetCurrentViewRecord, emDupCurrentRecord);
end;

procedure TKExtDataPanelController.EditRecord;
var
  LRecord: TKViewTableRecord;
begin
  LRecord := GetCurrentViewRecord;
  LRecord.ApplyEditRecordRules;
  ShowEditWindow(LRecord, emEditCurrentRecord);
end;

procedure TKExtDataPanelController.ShowEditWindow(const ARecord: TKRecord;
  const AEditMode: TKEditMode);
var
  LFormControllerType: string;
  LFormControllerNode: TEFNode;
  LFormController: IKExtController;
  LWidth: Integer;
  LHeight: Integer;
  LFullScreen: Boolean;
begin
  Assert((AEditMode = emNewrecord) or Assigned(ARecord));
  Assert(ViewTable <> nil);

  if Assigned(FEditHostWindow) then
    FEditHostWindow.Free(True);
  FEditHostWindow := TKExtModalWindow.Create(Self);

  //FEditHostWindow.ResizeHandles := 'n s';
  FEditHostWindow.Layout := lyFit;

  if AEditMode in [emNewRecord, emDupCurrentRecord] then
    FEditHostWindow.Title := Format(_('Add %s'), [_(ViewTable.DisplayLabel)])
  else if (AEditMode = emEditCurrentRecord) and IsActionAllowed('Edit') then
    FEditHostWindow.Title := Format(_('Edit %s'), [_(ViewTable.DisplayLabel)])
  else if (AEditMode = emViewCurrentRecord) and IsActionAllowed('View') then
    FEditHostWindow.Title := Format(_('View %s'), [_(ViewTable.DisplayLabel)])
  else
    FEditHostWindow.Title := _(ViewTable.DisplayLabel);

  LFormControllerNode := ViewTable.FindNode('Controller/FormController');
  if Assigned(LFormControllerNode) then
    LFormControllerType := LFormControllerNode.AsString;
  if LFormControllerType = '' then
    LFormControllerType := GetEditWindowDefaultControllerType;
  if LFormControllerType = '' then
    LFormControllerType := GetEditWindowDefaultControllerType;
  LFormController := TKExtControllerFactory.Instance.CreateController(
    FEditHostWindow, ViewTable.View, FEditHostWindow, LFormControllerNode, Self, LFormControllerType);
  LFormController.Config.SetObject('Sys/ServerStore', ServerStore);
  if Assigned(ARecord) then
    LFormController.Config.SetObject('Sys/Record', ARecord);
  LFormController.Config.SetObject('Sys/ViewTable', ViewTable);
  LFormController.Config.SetObject('Sys/HostWindow', FEditHostWindow);
  LFormController.Config.SetObject('Sys/CallingController', Self);

  LWidth := ViewTable.GetInteger('Controller/PopupWindow/Width');
  LHeight := ViewTable.GetInteger('Controller/PopupWindow/Height');
  LFullScreen := ViewTable.GetBoolean('Controller/PopupWindow/FullScreen', Session.IsMobileBrowser);

  if LFullScreen then
  begin
    FEditHostWindow.Maximized := True;
    FEditHostWindow.Border := not FEditHostWindow.Maximized;
  end
  else if (LWidth > 0) and (LHeight > 0) then
  begin
    FEditHostWindow.Width := LWidth;
    FEditHostWindow.Height := LHeight;
    LFormController.Config.SetBoolean('Sys/HostWindow/AutoSize', False);
  end
  else
    LFormController.Config.SetBoolean('Sys/HostWindow/AutoSize', True);

  case AEditMode of
    emNewRecord : LFormController.Config.SetString('Sys/Operation', 'Add');
    emDupCurrentRecord : LFormController.Config.SetString('Sys/Operation', 'Dup');
    emEditCurrentRecord : LFormController.Config.SetString('Sys/Operation', 'Edit');
    emViewCurrentRecord :
    begin
      if not IsActionAllowed('Edit') then
        LFormController.Config.SetBoolean('PreventEditing', True);
      LFormController.Config.SetString('Sys/Operation', 'View');
    end;
  end;

  LFormController.Display;

  FEditHostWindow.Show;
end;

function TKExtDataPanelController.GetCurrentViewRecord: TKViewTableRecord;
begin
  Result := ServerStore.GetRecord(Session.GetQueries, Session.Config.JSFormatSettings, IfThen(IsMultiSelect, 0, -1));
end;

function TKExtDataPanelController.FindViewLayout(
  const ALayoutName: string): TKLayout;
var
  LLayoutName: string;
  LLayoutNode: TEFNode;
begin
  LLayoutNode := ViewTable.FindNode('Controller/' + ALayoutName + '/Layout');
  if Assigned(LLayoutNode) then
  begin
    LLayoutName := LLayoutNode.AsString;
    if LLayoutName <> '' then
      Result := View.Catalog.Layouts.FindLayout(LLayoutName)
    else
      Result := View.Catalog.Layouts.FindLayoutByNode(LLayoutNode);
  end
  else
    Result := ViewTable.FindLayout(ALayoutName);
end;

function TKExtDataPanelController.GetSelectConfirmCall(const AMessage: string; const AMethod: TExtProcedure): string;
begin
  raise EKError.Create(_('Actions that require selection are not supported in this controller.'));
end;

function TKExtDataPanelController.GetSelectCall(const AMethod: TExtProcedure): TExtFunction;
begin
  raise EKError.Create(_('Actions that require selection are not supported in this controller.'));
end;

function TKExtDataPanelController.AddActionButton(const AUniqueId: string;
  const AView: TKView; const AToolbar: TKExtToolbar): TKExtActionButton;
var
  LConfirmationMessage: string;
  LRequireSelection: Boolean;
  LConfirmationJS: string;
begin
  Assert(Assigned(AView));
  Assert(Assigned(AToolbar));
  Assert(Assigned(ViewTable));
  Assert(Assigned(ServerStore));

  Result := TKExtDataActionButton.CreateAndAddTo(AToolbar.Items);
  Result.Hidden := not AView.GetBoolean('IsVisible', True);
  Result.UniqueId := AUniqueId;
  Result.View := AView;
  Result.ActionObserver := Self;
  TKExtDataActionButton(Result).ViewTable := ViewTable;
  TKExtDataActionButton(Result).ServerStore := ServerStore;

  // A Tool may or may not have a confirmation message and may or may not require
  // a selected row. We must handle all combinations.
  LConfirmationMessage := AView.GetExpandedString('Controller/ConfirmationMessage');
  // Cleanup Linebreaks with <br> tag
  LConfirmationMessage := StringReplace(LConfirmationMessage, sLineBreak, '<br>',[rfReplaceAll]);
  LRequireSelection := AView.GetBoolean('Controller/RequireSelection', True);

  if LRequireSelection then
    LConfirmationJS := GetSelectConfirmCall(LConfirmationMessage, TKExtDataActionButton(Result).ExecuteButtonAction)
  else
    LConfirmationJS := GetConfirmCall(LConfirmationMessage, Result.ExecuteButtonAction);

  if LConfirmationMessage <> '' then
    Result.Handler := JSFunction(LConfirmationJS)
  else if LRequireSelection then
    Result.On('click', GetSelectCall(TKExtDataActionButton(Result).ExecuteButtonAction))
  else
    Result.On('click', Ajax(Result.ExecuteButtonAction, []));
end;

function TKExtDataPanelController.CreateClientReader: TExtDataJsonReader;

  procedure DoAddReaderField(const AReader: TExtDataJsonReader; const AName, AType: string; const AUseNull: Boolean);
  var
    LField: TExtDataField;
  begin
    LField := TExtDataField.CreateAndAddTo(AReader.Fields);
    LField.Name := AName;
    LField.&Type := AType;
    LField.UseNull := AUseNull;
  end;

  procedure AddReaderField(const AReader: TExtDataJsonReader; const AViewField: TKViewField);
  var
    I: Integer;
  begin
    DoAddReaderField(AReader, AViewField.AliasedName, AViewField.ActualDataType.GetJSTypeName, not AViewField.IsRequired);
    if AViewField.IsPicture then
      DoAddReaderField(AReader, AViewField.GetURLFieldName,
        TEFDataTypeFactory.Instance.GetDataType('String').GetJSTypeName, not AViewField.IsRequired);
    if AViewField.IsReference then
    begin
      for I := 0 to AViewField.ModelField.FieldCount - 1 do
        DoAddReaderField(AReader, AViewField.ModelField.Fields[I].FieldName,
          AViewField.ModelField.Fields[I].DataType.GetJSTypeName, not AViewField.ModelField.Fields[I].IsRequired);
    end;
  end;
var
  I: Integer;

begin
  Assert(Assigned(ViewTable));

  Result := TExtDataJsonReader.Create(Self, JSObject('')); // Must pass '' otherwise invalid code is generated.
  Result.Root := 'Root';
  Result.TotalProperty := 'Total';
  Result.MessageProperty := 'Msg';
  Result.SuccessProperty := 'Success';

  for I := 0 to ViewTable.FieldCount - 1 do
    AddReaderField(Result, ViewTable.Fields[I]);
end;

function TKExtDataPanelController.CreateClientStore: TExtDataStore;
begin
  Result := TExtDataStore.Create(Self);
  Result.RemoteSort := ViewTable.GetBoolean('Controller/RemoteSort', GetDefaultRemoteSort);
  Result.Url := MethodURI(GetRecordPage);
  Result.On('exception', JSFunction('proxy, type, action, options, response, arg', 'loadError(type, action, response);'));
end;

procedure TKExtDataPanelController.SetupURLFields(const ARecord: TKViewTableRecord);
var
  I: Integer;
  LViewTableField: TKViewTableField;
  LImageField: TKViewTableField;
begin
  for I := 0 to ViewTable.FieldCount - 1 do
  begin
    if ViewTable.Fields[I].IsPicture then
    begin
      LViewTableField := ARecord.FieldByName(ViewTable.Fields[I].AliasedName);
      LImageField := ARecord.FieldByName(ViewTable.Fields[I].GetURLFieldName);
      if not LVIewTableField.IsNull then
        LImageField.AsString := MethodURI(GetImage, ['fn', LViewTableField.FieldName, 'rn', ARecord.Index])
      else
        LImageField.AsString := '';
      { TODO : handle null case? }
    end;
  end;
end;

procedure TKExtDataPanelController.GetRecordPage;
var
  LStart: Integer;
  LLimit: Integer;
  LTotal: Integer;
  LData: string;

  function GetFilter: string;
  var
    LFilter: string;
    LLookupFilter: string;
  begin
    LFilter := GetRootDataPanel.GetFilterExpression;
    if LFilter <> '' then
      LFilter := '(' + LFilter + ')';
    LLookupFilter := Config.GetString('Sys/LookupFilter');
    if LLookupFilter <> '' then
      LLookupFilter := '(' + LLookupFilter + ')';
    Result := SmartConcat(LFilter, ' and ', LLookupFilter);
  end;

begin
  try
    // Don't refresh if there are pending changes.
    if ServerStore.ChangesPending then
    begin
      LTotal := ServerStore.Records.GetRecordCount(ServerStore.Records.EnumNonDeletedRecords);
      LData := ServerStore.GetAsJSON(True);
    end
    else
    begin
      LStart := Session.QueryAsInteger['start'];
      LLimit := Session.QueryAsInteger['limit'];

      LTotal := ViewTable.Model.LoadRecords(ServerStore, GetFilter, GetOrderByClause, LStart, LLimit,
        procedure (ARecord: TEFNode)
        begin
          Assert(ARecord is TKViewTableRecord);
          SetupURLFields(TKViewTableRecord(ARecord));
        end);
      if (LStart <> 0) or (LLimit <> 0) then
        LData := ServerStore.GetAsJSON(True, LStart, LLimit)
      else
        // When loading all records, apply a limit on the display.
        { TODO : If there's a limit on the display of records, try to pass it over and only load
          needed records into the store. }
        LData := ServerStore.GetAsJSON(True, 0, Min(GetMaxRecords(), LTotal));
    end;
    Session.ResponseItems.AddJSON(Format('{Success: true, Total: %d, Root: %s}', [LTotal, LData]));
  except
    on E: Exception do
    begin
      Session.ResponseItems.Clear;
      Session.ResponseItems.AddJSON(Format('{Success: false, Msg: "%s", Root: []}', [E.Message]));
    end;
  end;
end;

function TKExtDataPanelController.GetRootDataPanel: TKExtDataPanelController;
begin
  Result := GetParentDataPanel;
  if Result = nil then
    Result := Self
  else
  begin
    while (Result.GetParentDataPanel <> nil) do
      Result := Result.GetParentDataPanel;
  end;
end;

function TKExtDataPanelController.GetMaxRecords: Integer;
begin
  Assert(ViewTable <> nil);

  if ViewTable.Model.IsLarge then
    Result := ViewTable.GetInteger('MaxRecords', 100)
  else
    Result := 1000;
end;

function TKExtDataPanelController.GetDefaultAutoOpen: Boolean;
begin
  Assert(Assigned(ViewTable));
  Assert(Assigned(ViewTable.Model));
  Result := not ViewTable.Model.IsLarge;
end;

function TKExtDataPanelController.GetDefaultRemoteSort: Boolean;
begin
  Result := False;
end;

function TKExtDataPanelController.GetEditWindowDefaultControllerType: string;
begin
  Result := 'Form';
end;

function TKExtDataPanelController.GetExplicitDefaultAction: string;
begin
  Result := ViewTable.GetExpandedString('Controller/DefaultAction');
end;

function TKExtDataPanelController.GetFilterExpression: string;
begin
  Result := '';
end;

procedure TKExtDataPanelController.GetImage;
var
  LImageField: TKViewTableField;
  LStream: TStream;
begin
  { TODO : Use PK to locate record instead? }
  LImageField := ServerStore.Records[Session.QueryAsInteger['rn']].FieldByName(Session.Query['fn']);

  if not LImageField.IsNull then
  begin
    LStream := TBytesStream.Create(LImageField.AsBytes);
    try
      DownloadThumbnailedStream(LStream, 'test.' + GetDataType(LImageField.AsBytes, '.png'),
        LImageField.ViewField.GetInteger('IsPicture/Thumbnail/Width', 100),
        LImageField.ViewField.GetInteger('IsPicture/Thumbnail/Height', 100));
    finally
      FreeAndNil(LStream);
    end;
  end;
  { TODO : handle null case. Empty image, default image, nothing? }
end;

function TKExtDataPanelController.GetImplicitDefaultAction: string;
begin
  if IsLookupMode then
    Result := 'LookupConfirm'
  else if IsActionAllowed('View') then
    Result := 'View'
  else if IsActionAllowed('Edit') then
    Result := 'Edit'
  else
    Result := '';
end;

function TKExtDataPanelController.GetOrderByClause: string;
var
  LFieldName: string;
  LDirection: string;
begin
  LFieldName := Session.Query['sort'];
  LDirection := Session.Query['dir'];

  if LFieldName <> '' then
    Result := ServerStore.Header.FieldByName(LFieldName).ViewField.BuildSortClause(SameText(LDirection, 'desc'))
  else
    Result := '';
end;

function TKExtDataPanelController.GetParentDataPanel: TKExtDataPanelController;
begin
  Result := TKExtDataPanelController(Config.GetObject('Sys/ParentDataPanel'));
end;

procedure TKExtDataPanelController.CreateToolbar;
begin
end;

procedure TKExtDataPanelController.InitComponents;
begin
end;

procedure TKExtDataPanelController.InitDefaults;
begin
  inherited;
end;

procedure TKExtDataPanelController.InitSubController(
  const AController: IKExtController);
begin
  inherited;
  Assert(Assigned(AController));
  Assert(Assigned(FViewTable));
  Assert(Assigned(FServerStore));

  AController.Config.SetObject('Sys/ViewTable', FViewTable);
  AController.Config.SetObject('Sys/ServerStore', FServerStore);
end;

function TKExtDataPanelController.IsActionAllowed(const AActionName: string): Boolean;
begin
  // Unknown actions are always allowed.
  if not FAllowedActions.TryGetValue(AActionName, Result) then
    Result := True;
end;

function TKExtDataPanelController.IsActionVisible(const AActionName: string): Boolean;
begin
  // Unknown actions are always visible.
  if not FVisibleActions.TryGetValue(AActionName, Result) then
    Result := True;
end;

function TKExtDataPanelController.IsActionSupported(const AActionName: string): Boolean;
begin
  // Let inherited controllers explictly declare their supported actions.
  Result := False;
end;

function TKExtDataPanelController.IsLookupMode: Boolean;
begin
  Result := Config.GetBoolean('Sys/LookupMode');
end;

function TKExtDataPanelController.IsMultiSelect: Boolean;
begin
  Result := False;
end;

procedure TKExtDataPanelController.NewRecord;
begin
  ShowEditWindow(nil, emNewRecord);
end;

procedure TKExtDataPanelController.SetViewTable(const AValue: TKViewTable);
begin
  FViewTable := AValue;

  FClientStore := CreateClientStore;
  FClientReader := CreateClientReader;
  FClientStore.Reader := FClientReader;

  FVisibleActions.AddOrSetValue('View', IsActionSupported('View') and (FViewTable.GetBoolean('Controller/AllowViewing') or Config.GetBoolean('AllowViewing')));
  FAllowedActions.AddOrSetValue('View', FVisibleActions['View'] and FViewTable.IsAccessGranted(ACM_VIEW));

  FVisibleActions.AddOrSetValue('Add',
    IsActionSupported('Add')
    and not FViewTable.GetBoolean('Controller/PreventAdding')
    and not View.GetBoolean('IsReadOnly')
    and not FViewTable.IsReadOnly
    and not Config.GetBoolean('PreventAdding'));
  FAllowedActions.AddOrSetValue('Add', FVisibleActions['Add'] and FViewTable.IsAccessGranted(ACM_ADD));

  FVisibleActions.AddOrSetValue('Dup',
    IsActionSupported('Dupo')
    and (FViewTable.GetBoolean('Controller/AllowDuplicating') or Config.GetBoolean('AllowDuplicating'))
    and not FViewTable.GetBoolean('Controller/PreventAdding')
    and not View.GetBoolean('IsReadOnly')
    and not FViewTable.IsReadOnly
    and not Config.GetBoolean('PreventAdding'));
  FAllowedActions.AddOrSetValue('Dup', FVisibleActions['Dup'] and FViewTable.IsAccessGranted(ACM_ADD));

  FVisibleActions.AddOrSetValue('Edit',
    IsActionSupported('Edit')
    and not FViewTable.GetBoolean('Controller/PreventEditing')
    and not View.GetBoolean('IsReadOnly')
    and not FViewTable.IsReadOnly
    and not Config.GetBoolean('PreventEditing'));
  FAllowedActions.AddOrSetValue('Edit', FVisibleActions['Edit'] and FViewTable.IsAccessGranted(ACM_MODIFY));

  FVisibleActions.AddOrSetValue('Delete',
    IsActionSupported('Delete')
    and not FViewTable.GetBoolean('Controller/PreventDeleting')
    and not View.GetBoolean('IsReadOnly')
    and not FViewTable.IsReadOnly
    and not Config.GetBoolean('PreventDeleting'));
  FAllowedActions.AddOrSetValue('Delete', FVisibleActions['Delete'] and FViewTable.IsAccessGranted(ACM_DELETE));
end;

{ TKExtDataActionButton }

procedure TKExtDataActionButton.ExecuteButtonAction;
var
  LController: IKExtController;
begin
  //inherited;
  Assert(Assigned(View));
  Assert(Assigned(FViewTable));
  Assert(Assigned(FServerStore));
  Assert(Assigned(ActionObserver));

  PerformBeforeExecute;
  LController := TKExtControllerFactory.Instance.CreateController(
    Session.ObjectCatalog, View, nil, nil, ActionObserver);
  if LController.Config.GetBoolean('RequireSelection', True) then
    FServerRecord := FServerStore.GetRecord(Session.GetQueries, Session.Config.JSFormatSettings, 0)
  else
    FServerRecord := nil;
  InitController(LController);
  LController.Display;
end;

function TKExtDataActionButton.GetServerRecord: TKViewTableRecord;
begin
  if Assigned(FOnGetServerRecord) then
    FServerRecord := FOnGetServerRecord;
  Result := FServerRecord;
end;

procedure TKExtDataActionButton.InitController(const AController: IKExtController);
begin
  inherited;
  Assert(Assigned(FViewTable));
  Assert(Assigned(FServerStore));
  Assert(Assigned(AController));

  AController.Config.SetObject('Sys/ServerStore', FServerStore);
  AController.Config.SetObject('Sys/ViewTable', FViewTable);
  AController.Config.SetObject('Sys/Record', ServerRecord);
end;

function TKExtDataPanelController.AddTopToolbarButton(const AActionName, ATooltip, AImageName: string;
  const ARequiresSelection: Boolean): TKExtButton;
begin
  if (AActionName <> '') and IsActionSupported(AActionName) then
  begin
    Result := TKExtButton.CreateAndAddTo(TopToolbar.Items);
    Result.Tooltip := ATooltip;
    Result.SetIconAndScale(AImageName);
    if (AActionName <> '') and not IsActionVisible(AActionName) then
      Result.Hidden := True
    else if (AActionName <> '') and not IsActionAllowed(AActionName) then
      Result.Disabled := True
    else if ARequiresSelection then
      FButtonsRequiringSelection.Add(Result);
  end
  else
    Result := nil;
end;

procedure TKExtDataPanelController.AddTopToolbarButtons;
begin
  Assert(ViewTable <> nil);
  Assert(TopToolbar <> nil);

  FNewButton := AddTopToolbarButton('Add', Format(_('Add %s'), [_(ViewTable.DisplayLabel)]), 'new_record', False);
  if Assigned(FNewButton) then
    FNewButton.On('click', Ajax(NewRecord));
  TExtToolbarSpacer.CreateAndAddTo(TopToolbar.Items);

  FDupButton := AddTopToolbarButton('Dup', Format(_('Duplicate %s'), [_(ViewTable.DisplayLabel)]), 'dup_record', True);
  if Assigned(FDupButton) then
    FDupButton.On('click', GetSelectCall(DuplicateRecord));
  TExtToolbarSpacer.CreateAndAddTo(TopToolbar.Items);

  FEditButton := AddTopToolbarButton('Edit', Format(_('Edit %s'), [_(ViewTable.DisplayLabel)]), 'edit_record', True);
  if Assigned(FEditButton) then
    FEditButton.On('click', GetSelectCall(EditRecord));
  TExtToolbarSpacer.CreateAndAddTo(TopToolbar.Items);

  FDeleteButton := AddTopToolbarButton('Delete', Format(_('Delete %s'), [_(ViewTable.DisplayLabel)]), 'delete_record', True);
  if Assigned(FDeleteButton) then
    FDeleteButton.On('click', JSFunction(GetSelectConfirmCall(
      Format(_('Selected %s {caption} will be deleted. Are you sure?'), [_(ViewTable.DisplayLabel)]), DeleteCurrentRecord)));

  FViewButton := AddTopToolbarButton('View', Format(_('View %s'), [_(ViewTable.DisplayLabel)]), 'view_record', True);
  if Assigned(FViewButton) then
    FViewButton.On('click', GetSelectCall(ViewRecord));

  inherited;
end;

procedure TKExtDataPanelController.AfterConstruction;
begin
  inherited;
  FAllowedActions := TDictionary<string, Boolean>.Create;
  FVisibleActions := TDictionary<string, Boolean>.Create;
  FButtonsRequiringSelection := TList<TExtObject>.Create;
end;

function TKExtDataPanelController.GetRowButtonsDisableJS: string;
var
  I: Integer;
begin
  Result := 'var disabled = s.getCount() == 0;';
  for I := 0 to FButtonsRequiringSelection.Count - 1 do
    Result := Result + Format('%s.setDisabled(disabled);', [FButtonsRequiringSelection[I].JSName]);
end;

function TKExtDataPanelController.AutoLoadData: Boolean;
begin
  Assert(ViewTable <> nil);

  Result := ViewTable.GetBoolean('Controller/AutoOpen', GetDefaultAutoOpen);
end;

procedure TKExtDataPanelController.BeforeCreateTopToolbar;
begin
  inherited;
  FButtonsRequiringSelection.Clear;
end;

procedure TKExtDataPanelController.CheckCanRead;
begin
  Assert(ViewTable <> nil);

  Session.Config.CheckAccessGranted(ViewTable.GetResourceURI, ACM_READ);
end;

function TKExtDataPanelController.UpdateRecord(const ARecord: TKVIewTableRecord; const ANewValues: ISuperObject;
  const APersist, ASuppressNotification: Boolean): string;
var
  LItem: TSuperAvlEntry;
  LOldRecord: TKViewTableRecord;
  LField: TKViewTableField;
  LParentField: TKViewField;
  LParentValue: string;

  procedure SetFieldValue;
  var
    LNames: TStringDynArray;
    LValues: TStringDynArray;
    LSep, LValue: string;
    I: Integer;
  begin
    LValue := LItem.Value.AsString;
    LField.SetAsJSONValue(LValue, False, Session.Config.UserFormatSettings);

    LSep := TKConfig.Instance.MultiFieldSeparator;
    if LItem.Name.Contains(LSep) then
    begin
      LNames := EF.StrUtils.Split(LItem.Name, LSep);
      LValues := EF.StrUtils.Split(LItem.Value.AsString, LSep);
      if Length(LValues) = 0 then
      begin
        SetLength(LValues, Length(LNames));
        for I := Low(LValues) to High(LValues) do
          LValues[I] := 'null';
      end;
      Assert(Length(LNames) = Length(LValues));
      for I := Low(LNames) to High(LNames) do
        LField.ParentRecord.FieldByName(LNames[I]).SetAsJSONValue(LValues[I], False, Session.Config.UserFormatSettings);
    end;
  end;

begin
  LOldRecord := TKViewTableRecord.Clone(ARecord);
  try
    try
      ARecord.Store.DisableChangeNotifications;
      try
        // Modify record values.
        for LItem in ANewValues.AsObject do
        begin
          LField := ARecord.FieldByName(LItem.Name);
          LParentField := LField.ViewField.Table.FindParentField(LField.FieldName);
          if Assigned(LParentField) then
            LParentValue := ANewValues.AsObject.S[LParentField.FieldName]
          else
            LParentValue := '';
          SetFieldValue;
        end;
      finally
        ARecord.Store.EnableChangeNotifications;
      end;
      // Get uploaded files.
      Session.EnumUploadedFiles(
        procedure (AFile: TKExtUploadedFile)
        begin
          if (AFile.Context is TKViewField) and (TKViewField(AFile.Context).Table = ViewTable) then
          begin
            if TKViewField(AFile.Context).DataType is TEFBlobDataType then
              ARecord.FieldByName(TKViewField(AFile.Context).AliasedName).AsBytes := AFile.Bytes
            else if TKViewField(AFile.Context).DataType is TKFileReferenceDataType then
              ARecord.FieldByName(TKViewField(AFile.Context).AliasedName).AsString := AFile.FileName
            else
              raise Exception.CreateFmt(_('Data type %s does not support file upload.'), [TKViewField(AFile.Context).DataType.GetTypeName]);
            Session.RemoveUploadedFile(AFile);
          end;
        end);

      // Save record.
      ViewTable.Model.SaveRecord(ARecord, APersist and not ViewTable.IsDetail,
        procedure
        begin
          Session.Flash(_('Changes saved succesfully.'));
        end);
      Result := '';
    except
      on E: EKValidationError do
      begin
        ExtMessageBox.Alert(_(Session.Config.AppTitle), E.Message);
        Result := E.Message;
        ARecord.Assign(LOldRecord);
        Exit;
      end;
    end;
    if not ASuppressNotification then
      NotifyObservers('Confirmed');
  finally
    FreeAndNil(LOldRecord);
  end;
end;

procedure TKExtDataPanelController.ViewRecord;
begin
  ShowEditWindow(GetCurrentViewRecord, emViewCurrentRecord);
end;

procedure TKExtDataPanelController.UpdateObserver(const ASubject: IEFSubject; const AContext: string);
begin
  inherited;
  if (AContext = 'Confirmed') and Supports(ASubject.AsObject, IKExtController) then
    LoadData;
end;

end.
