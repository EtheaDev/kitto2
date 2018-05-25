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

unit Kitto.Ext.DataPanel;

{$I Kitto.Defines.inc}

interface

uses
  SysUtils
  , JSON
  , Generics.Collections
  , Ext.Base
  , Ext.Data
  , EF.Classes
  , EF.ObserverIntf
  , EF.Tree
  , Kitto.JS
  , Kitto.JS.Types
  , Kitto.Metadata.Views
  , Kitto.Metadata.DataView
  , Kitto.Store
  , Kitto.Types
  , Kitto.Ext.Base
  , Kitto.Ext.Controller
  , Kitto.Ext.BorderPanel
  , Kitto.Ext.Editors
  ;

type
  TKExtGetServerRecordEvent = reference to function: TKViewTableRecord;
  TKExtGetServerStoreEvent = reference to function: TKViewTableStore;

  TKExtDataActionButton = class(TKExtActionButton)
  strict private
    FServerStore: TKViewTableStore;
    FOnGetServerStore: TKExtGetServerStoreEvent;
    FViewTable: TKViewTable;
    FServerRecord: TKViewTableRecord;
    FOnGetServerRecord: TKExtGetServerRecordEvent;
    function GetServerRecord: TKViewTableRecord;
    function GetServerStore: TKViewTableStore;
  strict protected
    procedure InitController(const AController: IJSController); override;
  public
    property ViewTable: TKViewTable read FViewTable write FViewTable;
    property ServerStore: TKViewTableStore read GetServerStore;
    property OnGetServerStore: TKExtGetServerStoreEvent read FOnGetServerStore write FOnGetServerStore;
    property ServerRecord: TKViewTableRecord read GetServerRecord;
    property OnGetServerRecord: TKExtGetServerRecordEvent read FOnGetServerRecord write FOnGetServerRecord;
  //published
    procedure ExecuteButtonAction; override;
  end;

  TKViewFieldFilterFunc = TFunc<TKViewField, Boolean>;

  /// <summary>
  ///  Base class for controllers that handle database records (either
  ///  single records or record sets).
  /// </summary>
  TKExtDataPanelController = class abstract(TKExtBorderPanelController)
  strict private
    FServerStore: TKViewTableStore;
    FClientStore: TExtDataStore;
    FViewTable: TKViewTable;
    FOwnsServerStore: Boolean;
    FVisibleActions: TDictionary<string, Boolean>;
    FAllowedActions: TDictionary<string, Boolean>;
    FNewButton: TKExtButton;
    FEditButton: TKExtButton;
    FViewButton: TKExtButton;
    FDeleteButton: TKExtButton;
    FDupButton: TKExtButton;
    FUsedViewFields: TArray<TKViewField>;
    FEditItems: TKEditItemList;
    function GetView: TKDataView;
    function GetMaxRecords: Integer;
    function GetDefaultAutoOpen: Boolean;
    procedure SetURLFieldValues(const ARecord: TKViewTableRecord);
    procedure SetFieldValue(const AField: TKViewTableField; const ANode: TEFNode);
    function GetFieldFilterFunc: TKFieldFilterFunc;
    procedure ExecuteDeferredFileOps(const ARecord: TKViewTableRecord; const AEvent: TKFileOpEvent);
    function GetEditItems: TKEditItemList;
  strict protected
    FButtonsRequiringSelection: TList<TExtObject>;
    procedure CheckCanRead;
    function GetOrderByClause: string; virtual;
    procedure SetViewTable(const AValue: TKViewTable); virtual;
    procedure CreateToolbar; virtual;
    procedure DoDisplay; override;
    procedure InitComponents; virtual;
    procedure InitDefaults; override;
    procedure InitSubController(const AController: IJSController); override;
    procedure AddTopToolbarButtons; override;
    function AddTopToolbarButton(const AActionName, ADefaultTooltip, AImageName: string;
      const ARequiresSelection: Boolean): TKExtButton;
    property View: TKDataView read GetView;
    property ClientStore: TExtDataStore read FClientStore;
    function CreateClientStore: TExtDataStore; virtual;
    procedure AddClientReaderField(const AStore: TExtDataStore; const AViewField: TKViewField);
    procedure CreateClientReaderFields;
    function AddActionButton(const AUniqueId: string; const AView: TKView;
      const AToolbar: TKExtToolbar): TKExtActionButton; override;
    function GetSelectConfirmCall(const AMessage: string; const AMethod: TJSProcedure): string; virtual;
    function GetSelectCall(const AMethod: TJSProcedure): TExtExpression; virtual;
    function AutoLoadData: Boolean; virtual;
    function GetParentDataPanel: TKExtDataPanelController;
    function GetRootDataPanel: TKExtDataPanelController;
    function FindViewLayout(const ALayoutName: string): TKLayout;
    function UpdateRecord(const ARecord: TKVIewTableRecord; const ANewValues: TEFTree;
      const AFieldName: string; const APersist: Boolean): string;
    function GetDefaultRemoteSort: Boolean; virtual;
    function FindCurrentViewRecord: TKViewTableRecord;
    function GetCurrentViewRecord: TKViewTableRecord;
    procedure DisplayEditController(const ARecord: TKViewTableRecord; const AOperation: string);
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
    procedure DoGetRecordPage(const AStart, ALimit: Integer; const AFillResponse: Boolean);
    function GetIsPaged: Boolean; virtual;
    function GetRecordPageFilter: string; virtual;
    procedure SetNewRecordDefaultValues(const ANode: TEFNode); virtual;
    function IsViewFieldIncludedInClientStore(const AViewField: TKViewField): Boolean; virtual;
    procedure AddUsedViewFields; virtual;
    procedure AddUsedViewField(const AViewField: TKViewField);
    function GetDefaultAllowClose: Boolean; override;
    // Inherited classes should call UpdateRecord when changes are confirmed/applied
    // and this method when changes are canceled. This class manages housekeeping.
    procedure ChangesCanceled(const ARecord: TKViewTableRecord);
    function InitEditController(const AContainer: IJSControllerContainer;
  const ARecord: TKViewTableRecord; const AOperation: string): IJSController;
    function GetDefaultEditControllerType: string; virtual;
    property EditItems: TKEditItemList read GetEditItems;
    function ExpandExpression(const AExpression: string): string; virtual;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
    property ViewTable: TKViewTable read FViewTable write SetViewTable;
    property ServerStore: TKViewTableStore read FServerStore write FServerStore;
    property DefaultAutoOpen: Boolean read GetDefaultAutoOpen;
    function GetFilterExpression: string; virtual;
    procedure UpdateObserver(const ASubject: IEFSubject; const AContext: string = ''); override;
    procedure InitActionController(const AAction: TKExtActionButton; const AController: IJSController); virtual;
  //published
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
  StrUtils
  , IOUtils
  , Math
  , Types
  , Classes
  , EF.Macros
  , EF.StrUtils
  , EF.Sys
  , EF.Localization
  , EF.Types
  , Kitto.Auth
  , Kitto.AccessControl
  , Kitto.Config
  , Kitto.JS.Formatting
  , Kitto.Rules
  , Kitto.SQL
  , Kitto.Web.Application
  , Kitto.Web.Request
  , Kitto.Web.Response
  , Kitto.Web.Session
  , Kitto.Ext.Utils
  ;

{ TKExtDataActionButton }

procedure TKExtDataActionButton.ExecuteButtonAction;
var
  LController: IJSController;
begin
  //inherited;
  Assert(Assigned(View));
  Assert(Assigned(FViewTable));
  Assert(Assigned(ActionObserver));

  PerformBeforeExecute;
  if View.IsPersistent then
    TKWebApplication.Current.DisplayView(View, ActionObserver)
  else
  begin
    LController := TKExtControllerFactory.Instance.CreateController(
      TKWebSession.Current.ObjectSpace, View, nil, nil, ActionObserver);
    if LController.Config.GetBoolean('RequireSelection', True) then
      FServerRecord := ServerStore.GetRecord(TKWebRequest.Current.QueryTree, TKWebApplication.Current.Config.JSFormatSettings, 0)
    else
      FServerRecord := nil;
    InitController(LController);
    LController.Display;
  end;
end;

function TKExtDataActionButton.GetServerRecord: TKViewTableRecord;
begin
  if Assigned(FOnGetServerRecord) then
    FServerRecord := FOnGetServerRecord;
  Result := FServerRecord;
end;

function TKExtDataActionButton.GetServerStore: TKViewTableStore;
begin
  if Assigned(FOnGetServerStore) then
    FServerStore := FOnGetServerStore;
  Result := FServerStore;
end;

procedure TKExtDataActionButton.InitController(const AController: IJSController);
begin
  inherited;
  Assert(Assigned(FViewTable));
  Assert(Assigned(AController));

  AController.Config.SetObject('Sys/ServerStore', ServerStore);
  AController.Config.SetObject('Sys/ViewTable', FViewTable);
  AController.Config.SetObject('Sys/Record', ServerRecord);
end;

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

function TKExtDataPanelController.ExpandExpression(const AExpression: string): string;
begin
  Result := TEFMacroExpansionEngine.Instance.Expand(AExpression);
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
  LPersistIt: Boolean;
  LOldState: TKRecordState;
begin
  Assert(ViewTable <> nil);

  LRecord := GetCurrentViewRecord;
  LPersistIt := not ViewTable.IsDetail;

  LOldState := LRecord.State;
  LRecord.MarkAsDeleted;
  try
    ViewTable.Model.SaveRecord(LRecord, LPersistIt, nil);
    // Make sure that we don't try to delete a nonpersistent record later
    // when we save master/detail changes to the database.
    if not LPersistIt and (LOldState = rsNew) then
      LRecord.MarkAsClean;
    if LPersistIt then
      TKWebApplication.Current.Toast(Format(_('%s deleted.'), [_(ViewTable.DisplayLabel)]));
  except
    on E: EKValidationError do
    begin
      LRecord.RestorePreviousState;
      ExtMessageBox.Alert(_(TKWebApplication.Current.Config.AppTitle), E.Message);
      Exit;
    end;
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
    IconCls := TKWebApplication.Current.SetViewIconStyle(View, LViewTable.Model.ImageName);

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

  // We do this after setting ViewTable in order to give descendants a chance
  // to define which view fields are used.
  CreateClientReaderFields;

  inherited; // Creates subcontrollers.

  InitComponents;
end;

procedure TKExtDataPanelController.DuplicateRecord;
var
  LRecord: TKViewTableRecord;
begin
  LRecord := GetCurrentViewRecord;
  LRecord.ApplyDuplicateRecordRules;
  DisplayEditController(LRecord, 'Dup');
end;

procedure TKExtDataPanelController.EditRecord;
var
  LRecord: TKViewTableRecord;
begin
  LRecord := GetCurrentViewRecord;
  LRecord.ApplyEditRecordRules;
  DisplayEditController(LRecord, 'Edit');
end;

procedure TKExtDataPanelController.DisplayEditController(const ARecord: TKViewTableRecord; const AOperation: string);
var
  LFormController: IJSController;
begin
  Assert((AOperation = 'Add') or Assigned(ARecord));
  Assert(ViewTable <> nil);

  LFormController := InitEditController(nil, ARecord, AOperation);
  LFormController.SetModal;
  LFormController.Display;
end;

function TKExtDataPanelController.InitEditController(const AContainer: IJSControllerContainer;
  const ARecord: TKViewTableRecord; const AOperation: string): IJSController;
var
  LFormControllerType: string;
  LFormControllerNode: TEFNode;
begin
  LFormControllerNode := ViewTable.FindNode('Controller/FormController');
  if Assigned(LFormControllerNode) then
    LFormControllerType := LFormControllerNode.AsString;
  if LFormControllerType = '' then
    LFormControllerType := GetDefaultEditControllerType;
  Result := TKExtControllerFactory.Instance.CreateController(Self,
    ViewTable.View, AContainer, LFormControllerNode, Self, LFormControllerType);
  Result.Config.SetObject('Sys/ServerStore', ServerStore);
  if Assigned(ARecord) then
    Result.Config.SetObject('Sys/Record', ARecord)
  else
    SetNewRecordDefaultValues(Result.Config);
  Result.Config.SetObject('Sys/ViewTable', ViewTable);
  Result.Config.SetObject('Sys/CallingController', Self);
  Result.Config.SetString('Sys/Operation', AOperation);

  if SameText(AOperation, 'View') and not IsActionAllowed('Edit') then
    Result.Config.SetBoolean('PreventEditing', True);

  // Merge config values.
  Result.Config.Merge(ViewTable.FindNode('EditController'));
end;

function TKExtDataPanelController.GetCurrentViewRecord: TKViewTableRecord;
begin
  Result := ServerStore.GetRecord(TKWebRequest.Current.QueryTree,
    TKWebApplication.Current.Config.JSFormatSettings, IfThen(IsMultiSelect, 0, -1));
end;

function TKExtDataPanelController.FindViewLayout(const ALayoutName: string): TKLayout;
var
  LLayoutName: string;
  LLayoutNode: TEFNode;
begin
  LLayoutNode := ViewTable.FindNode('Controller/' + ALayoutName + '/Layout');
  if Assigned(LLayoutNode) then
  begin
    LLayoutName := ExpandExpression(LLayoutNode.AsString);
    if LLayoutName <> '' then
      Result := View.Catalog.Layouts.FindLayout(LLayoutName)
    else
      Result := View.Catalog.Layouts.FindLayoutByNode(LLayoutNode);
  end
  else
    Result := ViewTable.FindLayout(ALayoutName);
end;

function TKExtDataPanelController.GetSelectConfirmCall(const AMessage: string; const AMethod: TJSProcedure): string;
begin
  raise EKError.Create(_('Actions that require selection are not supported in this controller.'));
end;

function TKExtDataPanelController.GetSelectCall(const AMethod: TJSProcedure): TExtExpression;
begin
  raise EKError.Create(_('Actions that require selection are not supported in this controller.'));
end;

function TKExtDataPanelController.AddActionButton(const AUniqueId: string;
  const AView: TKView; const AToolbar: TKExtToolbar): TKExtActionButton;
var
  LConfirmationMessage: string;
  LRequireSelection: Boolean;
  LConfirmationJS: string;
  LResult: TKExtActionButton;
begin
  Assert(Assigned(AView));
  Assert(Assigned(AToolbar));
  Assert(Assigned(ViewTable));
  Assert(Assigned(ServerStore));

  Result := TKExtDataActionButton.CreateAndAddToArray(AToolbar.Items);
  Result.Hidden := not AView.GetBoolean('IsVisible', True);
  Result.UniqueId := AUniqueId;
  Result.View := AView;
  Result.ActionObserver := Self;
  TKExtDataActionButton(Result).ViewTable := ViewTable;
  TKExtDataActionButton(Result).OnGetServerStore :=
    function: TKViewTableStore
    begin
      if GetIsPaged and not AView.GetBoolean('Controller/RequireSelection', True) and AView.GetBoolean('Controller/LoadAllRecords', True) then
        DoGetRecordPage(0, 0, False);
      Result := ServerStore;
    end;
  LResult := Result;
  TKExtDataActionButton(Result).OnInitController :=
    procedure (AController: IJSController)
    begin
      InitActionController(LResult, AController);
    end;

  // A Tool may or may not have a confirmation message and may or may not require
  // a selected row. We must handle all combinations.
  LConfirmationMessage := AView.GetExpandedString('Controller/ConfirmationMessage');
  // Cleanup Linebreaks with <br> tag
  LConfirmationMessage := StringReplace(LConfirmationMessage, sLineBreak, '<br>',[rfReplaceAll]);
  LRequireSelection := AView.GetBoolean('Controller/RequireSelection', True);

  if LRequireSelection then
    LConfirmationJS := GetSelectConfirmCall(LConfirmationMessage, TKExtDataActionButton(Result).ExecuteButtonAction)
  else
    LConfirmationJS := Result.GetConfirmCall(LConfirmationMessage);

  if LConfirmationMessage <> '' then
    Result.Handler := GenerateAnonymousFunction(LConfirmationJS)
  else if LRequireSelection then
    Result.On('click', GetSelectCall(TKExtDataActionButton(Result).ExecuteButtonAction))
  else
    Result.On('click', TKWebResponse.Current.Items.AjaxCallMethod(Self).SetMethod(Result.ExecuteButtonAction).AsFunction);
end;

procedure TKExtDataPanelController.AddClientReaderField(const AStore: TExtDataStore; const AViewField: TKViewField);
var
  I: Integer;

  procedure DoAddReaderField(const AStore: TExtDataStore; const AName, AType: string; const AUseNull: Boolean);
  var
    LField: TExtDataField;
    //LFormat: string;
  begin
    LField := TExtDataField.CreateInlineAndAddToArray(AStore.Fields);
    LField.Name := AName;
    LField.&Type := AType;
    LField.UseNull := AUseNull;
    {TODO : Refactor; also see GridPanel}
    //if LField.&Type = 'date' then
    //begin
    //  LFormat := AViewField.DisplayFormat;
    //  if LFormat = '' then
    //    LFormat := TKWebApplication.Current.Config.UserFormatSettings.ShortDateFormat;
    //  LField.SetConfigItem('dateFormat', TJS.DelphiDateFormatToJSDateFormat(LFormat));
    //end;
  end;

begin
  DoAddReaderField(AStore, AViewField.AliasedName, AViewField.ActualDataType.GetJSTypeName, not AViewField.IsRequired);
  if AViewField.IsPicture then
    DoAddReaderField(AStore, AViewField.GetURLFieldName,
      TEFDataTypeFactory.Instance.GetDataType('String').GetJSTypeName, not AViewField.IsRequired);
  if AViewField.IsReference then
  begin
    for I := 0 to AViewField.ModelField.FieldCount - 1 do
      DoAddReaderField(AStore, AViewField.ModelField.Fields[I].FieldName,
        AViewField.ModelField.Fields[I].DataType.GetJSTypeName, not AViewField.ModelField.Fields[I].IsRequired);
  end;
end;

procedure TKExtDataPanelController.CreateClientReaderFields;
var
  I: Integer;
begin
  for I := 0 to ViewTable.FieldCount - 1 do
    if IsViewFieldIncludedInClientStore(ViewTable.Fields[I]) then
      AddClientReaderField(FClientStore{.Proxy.Reader}, ViewTable.Fields[I]);
end;

function TKExtDataPanelController.CreateClientStore: TExtDataStore;
var
  LProxy: TExtDataAjaxProxy;
  LReader: TExtDataJsonReader;
begin
  Result := TExtDataStore.Create(Self);
  Result.RemoteSort := ViewTable.GetBoolean('Controller/RemoteSort', GetDefaultRemoteSort);
  LProxy := TExtDataAjaxProxy.CreateInline(Result);
  LProxy.Url := GetMethodURL(GetRecordPage);
  LReader := TExtDataJsonReader.CreateInline(Result);
  LReader.RootProperty := 'Root';
  LReader.TotalProperty := 'Total';
  LReader.MessageProperty := 'Msg';
  LReader.SuccessProperty := 'Success';
  LProxy.Reader := LReader;
  Result.Proxy := LProxy;
  Result.On('exception', GenerateAnonymousFunction('proxy, type, action, options, response, arg', 'loadError(type, action, response);'));
end;

procedure TKExtDataPanelController.SetURLFieldValues(const ARecord: TKViewTableRecord);
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
        LImageField.AsString := AddParamsToURL(GetMethodURL(GetImage), 'fn=' + LViewTableField.FieldName + '&rn=' + ARecord.Index.ToString)
      else
        LImageField.AsString := TKWebApplication.Current.GetImageURL(
          LImageField.ViewField.GetString('EmptyImageName', 'empty'));
    end;
  end;
end;

procedure TKExtDataPanelController.GetRecordPage;
begin
  DoGetRecordPage(ParamAsInteger('start'), ParamAsInteger('limit'), True);
end;

function TKExtDataPanelController.GetRecordPageFilter: string;
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

function TKExtDataPanelController.GetFieldFilterFunc: TKFieldFilterFunc;
begin
  Result :=
    function (AField: TKField): Boolean
    begin
      Result := IsViewFieldIncludedInClientStore((AField as TKViewTableField).ViewField);
    end;
end;

procedure TKExtDataPanelController.DoGetRecordPage(const AStart, ALimit: Integer;
  const AFillResponse: Boolean);
var
  LTotal: Integer;
  LData: string;
begin
  try
    // Don't refresh if there are pending changes.
    if ServerStore.ChangesPending then
    begin
      LTotal := ServerStore.Records.GetRecordCount(ServerStore.Records.EnumNonDeletedRecords);
      if AFillResponse then
        LData := ServerStore.GetAsJSON(True, 0, 0, GetFieldFilterFunc());
    end
    else
    begin
      LTotal := ViewTable.Model.LoadRecords(ServerStore, GetRecordPageFilter, GetOrderByClause, AStart, ALimit,
        procedure (ARecord: TEFNode)
        begin
          Assert(ARecord is TKViewTableRecord);
          SetURLFieldValues(TKViewTableRecord(ARecord));
        end);
      if AFillResponse then
      begin
        if (AStart <> 0) or (ALimit <> 0) then
          LData := ServerStore.GetAsJSON(True, 0, 0, GetFieldFilterFunc())
        else
          // When loading all records, apply a limit on the display.
          { TODO : If there's a limit on the display of records, try to pass it over and only load
            needed records into the store. }
          LData := ServerStore.GetAsJSON(True, 0, Min(GetMaxRecords(), LTotal), GetFieldFilterFunc());
      end;
    end;
    if AFillResponse then
      TKWebResponse.Current.Items.AddJSON(Format('{Success: true, Total: %d, Root: %s}', [LTotal, LData]));
  except
    on E: Exception do
    begin
      if AFillResponse then
      begin
        TKWebResponse.Current.Items.Clear;
        TKWebResponse.Current.Items.AddJSON(Format('{Success: false, Msg: "%s", Root: []}', [TJS.StrToJS(E.Message)]));
      end
      else
        raise;
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

  if ViewTable.IsLarge then
    Result := ViewTable.GetInteger('MaxRecords', 100)
  else
    Result := 1000;
end;

procedure TKExtDataPanelController.SetNewRecordDefaultValues(const ANode: TEFNode);
begin
end;

function TKExtDataPanelController.GetDefaultAllowClose: Boolean;
begin
  Result := True;
end;

function TKExtDataPanelController.GetDefaultAutoOpen: Boolean;
begin
  Assert(Assigned(ViewTable));
  Assert(Assigned(ViewTable.Model));
  Result := not ViewTable.IsLarge;
end;

function TKExtDataPanelController.GetDefaultEditControllerType: string;
begin
  Result := 'Form';
end;

function TKExtDataPanelController.GetDefaultRemoteSort: Boolean;
begin
  Result := False;
end;

function TKExtDataPanelController.GetEditItems: TKEditItemList;
begin
  if not Assigned(FEditItems) then
    FEditItems := TKEditItemList.Create;
  Result := FEditItems;
end;

function TKExtDataPanelController.GetExplicitDefaultAction: string;
begin
  Result := ViewTable.GetExpandedString('Controller/DefaultAction');
  if (Result <> '') and not IsActionAllowed(Result) then
    Result := '';
end;

function TKExtDataPanelController.GetFilterExpression: string;
begin
  Result := '';
end;

procedure TKExtDataPanelController.GetImage;
var
  LImageField: TKViewTableField;
begin
  { TODO : Use PK to locate record instead? }
  LImageField := ServerStore.Records[ParamAsInteger('rn')].FieldByName(ParamAsString('fn'));

  if not LImageField.IsNull then
  begin
    DownloadThumbnailedStream(TBytesStream.Create(LImageField.AsBytes), 'test.' + GetDataType(LImageField.AsBytes, '.png'),
      LImageField.ViewField.GetInteger('IsPicture/Thumbnail/Width', 100),
      LImageField.ViewField.GetInteger('IsPicture/Thumbnail/Height', 100));
  end
  else
    TKWebApplication.Current.DownloadFile(TKWebApplication.Current.GetImagePathName(
      LImageField.ViewField.GetString('EmptyImageName', 'empty')), 'test.png');
end;

function TKExtDataPanelController.GetImplicitDefaultAction: string;
var
  LConfigDefaultAction: string;
begin
  if IsLookupMode then
    Result := 'LookupConfirm'
  else
  begin
    LConfigDefaultAction := TKWebApplication.Current.Config.Config.GetString('Defaults/Grid/DefaultAction');
    if (LConfigDefaultAction <> '') and IsActionAllowed(LConfigDefaultAction) then
      Result := LConfigDefaultAction
  else if IsActionAllowed('View') then
    Result := 'View'
  else if IsActionAllowed('Edit') then
    Result := 'Edit'
  else
    Result := '';
  end;
end;

function TKExtDataPanelController.GetIsPaged: Boolean;
begin
  Result := False;
end;

function TKExtDataPanelController.GetOrderByClause: string;
var
  LJSonValue: TJSonValue;
  LJSonArray: TJSONArray;
  LJSonParam: string;
  LFieldName: string;
  LDirection: string;
begin
  Result := '';
  LJSonParam := ParamAsString('sort');
  if LJSonParam <> '' then
  begin
    LJSonValue := TJSONObject.ParseJSONValue(LJSonParam);
    Assert(LJSonValue is TJSonArray);
    LJSonArray := TJSonArray(LJSonValue);
    Assert(LJSonArray.Count = 1);
    LFieldName := LJSonArray.Items[0].GetValue('property','');
    LDirection := LJSonArray.Items[0].GetValue('direction','');
    if LFieldName <> '' then
      Result := ServerStore.Header.FieldByName(LFieldName).ViewField.BuildSortClause(SameText(LDirection, 'DESC'))
  end;
end;

function TKExtDataPanelController.GetParentDataPanel: TKExtDataPanelController;
begin
  Result := TKExtDataPanelController(Config.GetObject('Sys/ParentDataPanel'));
end;

procedure TKExtDataPanelController.CreateToolbar;
begin
end;

procedure TKExtDataPanelController.InitActionController(const AAction: TKExtActionButton;
  const AController: IJSController);
begin
  AController.Config.SetString('Sys/FilterExpression', GetFilterExpression);
end;

procedure TKExtDataPanelController.InitComponents;
begin
end;

procedure TKExtDataPanelController.InitDefaults;
begin
  inherited;
end;

procedure TKExtDataPanelController.InitSubController(
  const AController: IJSController);
begin
  inherited;
  Assert(Assigned(AController));
  Assert(Assigned(FViewTable));
  Assert(Assigned(FServerStore));

  AController.Config.SetObject('Sys/ViewTable', FViewTable);
  // Let each subcontroller create its own store (although it's less efficient),
  // as sharing the store means several GetRecordPage requests may come to the
  // same store simultaneously from different threads -> havoc.
  //AController.Config.SetObject('Sys/ServerStore', FServerStore);
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

function TKExtDataPanelController.IsViewFieldIncludedInClientStore(const AViewField: TKViewField): Boolean;
begin
  Result := Assigned(AViewField) and TEFArray.Contains<TKViewField>(FUsedViewFields, AViewField);
end;

procedure TKExtDataPanelController.NewRecord;
begin
  DisplayEditController(nil, 'Add');
end;

procedure TKExtDataPanelController.SetViewTable(const AValue: TKViewTable);
begin
  FViewTable := AValue;

  AddUsedViewFields;

  FClientStore := CreateClientStore;

  FVisibleActions.AddOrSetValue('View', IsActionSupported('View') and (FViewTable.GetBoolean('Controller/AllowViewing') or Config.GetBoolean('AllowViewing')));
  FAllowedActions.AddOrSetValue('View', FVisibleActions['View'] and FViewTable.IsAccessGranted(ACM_VIEW));

  FVisibleActions.AddOrSetValue('Add',
    IsActionSupported('Add') and (
    not FViewTable.GetBoolean('Controller/PreventAdding')
    and not View.GetBoolean('IsReadOnly')
    and not FViewTable.IsReadOnly
    and not Config.GetBoolean('PreventAdding'))
    or not FViewTable.GetBoolean('Controller/PreventAdding', True) //explicit PreventAdding: False
    );
  FAllowedActions.AddOrSetValue('Add', FVisibleActions['Add'] and FViewTable.IsAccessGranted(ACM_ADD));

  FVisibleActions.AddOrSetValue('Dup',
    IsActionSupported('Dup')
    and (FViewTable.GetBoolean('Controller/AllowDuplicating') or Config.GetBoolean('AllowDuplicating'))
    and not View.GetBoolean('IsReadOnly')
    and not FViewTable.IsReadOnly);
  FAllowedActions.AddOrSetValue('Dup', FVisibleActions['Dup'] and FViewTable.IsAccessGranted(ACM_ADD));

  FVisibleActions.AddOrSetValue('Edit',
    IsActionSupported('Edit') and (
    not FViewTable.GetBoolean('Controller/PreventEditing')
    and not View.GetBoolean('IsReadOnly')
    and not FViewTable.IsReadOnly
    and not Config.GetBoolean('PreventEditing'))
    or not FViewTable.GetBoolean('Controller/PreventEditing', True) //explicit PreventEditing: False;
    );
  FAllowedActions.AddOrSetValue('Edit', FVisibleActions['Edit'] and FViewTable.IsAccessGranted(ACM_MODIFY));

  FVisibleActions.AddOrSetValue('Delete',
    IsActionSupported('Delete') and
    (not FViewTable.GetBoolean('Controller/PreventDeleting')
    and not View.GetBoolean('IsReadOnly')
    and not FViewTable.IsReadOnly
    and not Config.GetBoolean('PreventDeleting'))
    or not FViewTable.GetBoolean('Controller/PreventDeleting', True) //explicit PreventDeleting: False;
    );
  FAllowedActions.AddOrSetValue('Delete', FVisibleActions['Delete'] and FViewTable.IsAccessGranted(ACM_DELETE));
end;

procedure TKExtDataPanelController.AddUsedViewFields;
var
  I: Integer;
begin
  // By default we add all fields to preserve backward compatibility.
  // Descendants may do things differently as appropriate.
  for I := 0 to ViewTable.FieldCount - 1 do
    AddUsedViewField(ViewTable.Fields[I]);
end;

function TKExtDataPanelController.AddTopToolbarButton(const AActionName, ADefaultTooltip, AImageName: string;
  const ARequiresSelection: Boolean): TKExtButton;
begin
  if (AActionName <> '') and IsActionSupported(AActionName) then
  begin
    Result := TKExtButton.CreateAndAddToArray(TopToolbar.Items);
    Result.Tooltip := FViewTable.GetString('Controller/' + AActionName + '/Tooltip', ADefaultTooltip);
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
  begin
    //FNewButton.On('click', Ajax(NewRecord));
    FNewButton.On('click', TKWebResponse.Current.Items.AjaxCallMethod(Self).SetMethod(NewRecord).AsFunction);
//    TExtToolbarSpacer.CreateAndAddTo(TopToolbar.Items);
  end;

  FDupButton := AddTopToolbarButton('Dup', Format(_('Duplicate %s'), [_(ViewTable.DisplayLabel)]), 'dup_record', True);
  if Assigned(FDupButton) then
  begin
    FDupButton.On('click', GetSelectCall(DuplicateRecord));
//    TExtToolbarSpacer.CreateAndAddTo(TopToolbar.Items);
  end;

  FEditButton := AddTopToolbarButton('Edit', Format(_('Edit %s'), [_(ViewTable.DisplayLabel)]), 'edit_record', True);
  if Assigned(FEditButton) then
  begin
    FEditButton.On('click', GetSelectCall(EditRecord));
//    TExtToolbarSpacer.CreateAndAddTo(TopToolbar.Items);
  end;

  FDeleteButton := AddTopToolbarButton('Delete', Format(_('Delete %s'), [_(ViewTable.DisplayLabel)]), 'delete_record', True);
  if Assigned(FDeleteButton) then
    FDeleteButton.On('click', GenerateAnonymousFunction(GetSelectConfirmCall(
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
  Result := 'var disabled = s.getCount() === 0;';
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

procedure TKExtDataPanelController.ChangesCanceled(const ARecord: TKViewTableRecord);
begin
  NotifyObservers('Canceled');
  ExecuteDeferredFileOps(ARecord, oeCancel);
end;

procedure TKExtDataPanelController.CheckCanRead;
begin
  Assert(ViewTable <> nil);

  TKAccessController.Current.CheckAccessGranted(TKAuthenticator.Current.UserName, ViewTable.GetACURI, ACM_READ);
end;

procedure TKExtDataPanelController.SetFieldValue(const AField: TKViewTableField; const ANode: TEFNode);
var
  LNames: TStringDynArray;
  LValues: TStringDynArray;
  LValue: string;
  LSep: string;
  I: Integer;
begin
  Assert(Assigned(AField));

  if Assigned(ANode) then
  begin
    LValue := ANode.AsString;
    AField.SetAsJSONValue(LValue, False, TKWebApplication.Current.Config.UserFormatSettings);

    LSep := TKConfig.Instance.MultiFieldSeparator;
    if ANode.Name.Contains(LSep) then
    begin
      LNames := EF.StrUtils.Split(ANode.Name, LSep);
      LValues := EF.StrUtils.Split(LValue, LSep);
      if Length(LValues) = 0 then
      begin
        SetLength(LValues, Length(LNames));
        for I := Low(LValues) to High(LValues) do
          LValues[I] := 'null';
      end;
      Assert(Length(LNames) = Length(LValues));
      for I := Low(LNames) to High(LNames) do
        AField.ParentRecord.FieldByName(LNames[I]).SetAsJSONValue(LValues[I], False, TKWebApplication.Current.Config.UserFormatSettings);
    end;
  end;
end;

function TKExtDataPanelController.FindCurrentViewRecord: TKViewTableRecord;
begin
  Result := ServerStore.FindRecord(TKWebRequest.Current.QueryTree,
    TKWebApplication.Current.Config.JSFormatSettings, IfThen(IsMultiSelect, 0, -1));
end;

function TKExtDataPanelController.UpdateRecord(const ARecord: TKVIewTableRecord; const ANewValues: TEFTree;
  const AFieldName: string; const APersist: Boolean): string;
var
  LOldRecord: TKViewTableRecord;
begin
  LOldRecord := TKViewTableRecord.Clone(ARecord);
  try
    try
      ARecord.Store.DoWithChangeNotificationsDisabled(
        procedure
        var
          LNode: TEFNode;
          LField: TKViewTableField;
          I: Integer;
        begin
          // Modify record value(s).
          if AFieldName <> '' then
          begin
            FEditItems.EditorsByFieldName(AFieldName,
              procedure (AEditor: IKExtEditor)
              begin
                LField := ARecord.FieldByName(AEditor.FieldName);
                LNode := ANewValues.FindNode(LField.FieldName);
                Assert(Assigned(LNode));
                SetFieldValue(LField, LNode);
              end);
          end
          else
          begin
            for I := 0 to ANewValues.ChildCount - 1 do
            begin
              LNode := ANewValues.Children[I];
              LField := ARecord.FieldByName(LNode.Name);
              SetFieldValue(LField, LNode);
            end;
          end;
        end);
      // Save record.
      ViewTable.Model.SaveRecord(ARecord, APersist and not ViewTable.IsDetail,
        procedure
        begin
          TKWebApplication.Current.Toast(_('Changes saved succesfully.'));
        end);
      ExecuteDeferredFileOps(ARecord, oePost);
      Config.SetObject('Sys/Record', ARecord);
      Result := '';
    except
      on E: EKValidationError do
      begin
        ExtMessageBox.Alert(_(TKWebApplication.Current.Config.AppTitle), E.Message);
        Result := E.Message;
        ARecord.Assign(LOldRecord);
        Exit;
      end;
    end;
  finally
    FreeAndNil(LOldRecord);
  end;
end;

procedure TKExtDataPanelController.ViewRecord;
begin
  DisplayEditController(GetCurrentViewRecord, 'View');
end;

procedure TKExtDataPanelController.UpdateObserver(const ASubject: IEFSubject; const AContext: string);
begin
  inherited;
  if (AContext = 'Confirmed') and Supports(ASubject.AsObject, IJSController) then
    LoadData;
end;

procedure TKExtDataPanelController.AddUsedViewField(const AViewField: TKViewField);
begin
  if not TEFArray.Contains<TKViewField>(FUsedViewFields, AViewField) then
  begin
    SetLength(FUsedViewFields, Length(FUsedViewFields) + 1);
    FUsedViewFields[High(FUsedViewFields)] := AViewField;
  end;
end;

procedure TKExtDataPanelController.ExecuteDeferredFileOps(const ARecord: TKViewTableRecord; const AEvent: TKFileOpEvent);
var
  LFileOp: TKFileOp;
  I: Integer;
  LDeferredFileOps: TEFNode;
  J: Integer;
begin
  for I := 0 to ARecord.FieldCount - 1 do
  begin
    LDeferredFileOps := ARecord.Fields[I].FindNode('DeferredFileOps');
    if Assigned(LDeferredFileOps) then
    begin
      for J := 0 to LDeferredFileOps.ChildCount - 1 do
      begin
        LFileOp := LDeferredFileOps.Children[J].AsStringArray;
        if (LFileOp.Event = AEvent) and (LFileOp.Kind = okDelete) then
        begin
          if FileExists(LFileOp.PathName) then
            TFile.Delete(LFileOp.PathName);
        end;
      end;
    end;
  end;
end;

end.
