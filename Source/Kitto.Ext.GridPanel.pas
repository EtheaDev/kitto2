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

unit Kitto.Ext.GridPanel;

{$I Kitto.Defines.inc}

interface

uses
  Generics.Collections,
  ExtPascal, Ext, ExtData, ExtForm, ExtGrid, ExtPascalUtils,
  EF.ObserverIntf, EF.Types,
  Kitto.Metadata.Views, Kitto.Metadata.DataView, Kitto.Store, Kitto.Types,
  Kitto.Ext.Base, Kitto.Ext.Controller, Kitto.Ext.DataPanelLeaf;

type
  TKExtGridPanel = class(TKExtDataPanelLeafController)
  strict private
    FGridEditorPanel: TExtGridEditorGridPanel;
    FIsAddAllowed: Boolean;
    FIsEditAllowed: Boolean;
    FIsDeleteAllowed: Boolean;
    FGridView: TExtGridGridView;
    FEditHostWindow: TKExtModalWindow;
    FPagingToolbar: TExtPagingToolbar;
    FPageRecordCount: Integer;
    FSelModel: TExtGridRowSelectionModel;
    FIsAddVisible: Boolean;
    FIsDeleteVisible: Boolean;
    FButtonsRequiringSelection: TList<TExtObject>;
    function GetGroupingFieldName: string;
    function CreatePagingToolbar: TExtPagingToolbar;
    procedure ShowEditWindow(const ARecord: TKRecord;
      const AEditMode: TKEditMode);
    procedure InitColumns;
    function GetRowButtonsDisableJS: string;
    function GetRowColorPatterns(out AFieldName: string): TEFPairs;
    procedure CreateGridView;
    procedure CheckGroupColumn;
  strict protected
    function GetOrderByClause: string; override;
//    function GetRefreshJSCode: string; override;
    procedure InitDefaults; override;
    procedure SetViewTable(const AValue: TKViewTable); override;
    function CreateClientStore: TExtDataStore; override;
    procedure BeforeCreateTopToolbar; override;
    procedure AfterCreateTopToolbar; override;
    procedure AddTopToolbarButtons; override;
    procedure AddTopToolbarToolViewButtons; override;
    function GetSelectConfirmCall(const AMessage: string;
      const AMethod: TExtProcedure): string; override;
    function AddActionButton(const AView: TKView;
      const AToolbar: TExtToolbar): TKExtActionButton; override;
    function GetSelectCall(const AMethod: TExtProcedure): TExtFunction; override;
  public
    procedure UpdateObserver(const ASubject: IEFSubject;
      const AContext: string = ''); override;
    destructor Destroy; override;
  published
    procedure EditViewRecord;
    procedure NewRecord(This: TExtButton; E: TExtEventObjectSingleton);
    procedure DeleteCurrentRecord;
    procedure RefreshData; override;
  end;

implementation

uses
  SysUtils, StrUtils, Math,
  EF.Tree, EF.StrUtils, EF.Localization, EF.JSON,
  Kitto.Metadata.Models, Kitto.Rules, Kitto.AccessControl,
  Kitto.Ext.Session, Kitto.Ext.Utils;

{ TKExtGridPanel }

function TKExtGridPanel.GetOrderByClause: string;
var
  LSortFieldName: string;
begin
  LSortFieldName := ViewTable.GetString('Controller/Grouping/SortFieldName', GetGroupingFieldName);
  if LSortFieldName <> '' then
    Result := ViewTable.FieldByName(LSortFieldName).QualifiedDBNameOrExpression
  else
    Result := inherited GetOrderByClause;
end;

function TKExtGridPanel.GetGroupingFieldName: string;
begin
  Result := ViewTable.GetString('Controller/Grouping/FieldName');
end;

procedure TKExtGridPanel.AfterCreateTopToolbar;
begin
  inherited;
  if FButtonsRequiringSelection.Count > 0 then
  begin
    FSelModel.On('selectionchange', JSFunction('s', GetRowButtonsDisableJS));
    On('afterrender', JSFunction(Format('var s = %s;', [FSelModel.JSName]) + GetRowButtonsDisableJS));
  end;
end;

procedure TKExtGridPanel.BeforeCreateTopToolbar;
begin
  inherited;
  FButtonsRequiringSelection.Clear;
end;

function TKExtGridPanel.CreateClientStore: TExtDataStore;
var
  LGroupingFieldName: string;
  LGroupingMenu: Boolean;
begin
  LGroupingFieldName := GetGroupingFieldName;
  LGroupingMenu := ViewTable.GetBoolean('Controller/Grouping/EnableMenu');
  if (LGroupingFieldName <> '') or LGroupingMenu then
  begin
    if ViewTable.FindField(LGroupingFieldName) = nil then
      raise Exception.CreateFmt('Field %s not found. Cannot group.', [LGroupingFieldName]);
    Result := TExtDataGroupingStore.Create;
    Result.Url := MethodURI(GetRecordPage);
    //TExtDataGroupingStore(Result).GroupOnSort := True;
    if LGroupingFieldName <> '' then
    begin
      TExtDataGroupingStore(Result).GroupField := LGroupingFieldName;
      Result.RemoteSort := True;
    end;
  end
  else
    Result := inherited CreateClientStore;
  Result.On('load', FSelModel.SelectFirstRow);
  FGridEditorPanel.Store := Result;
end;

procedure TKExtGridPanel.CreateGridView;
var
  LGroupingMenu: Boolean;
  LCountTemplate: string;
  LGroupingFieldName: string;
  LRowClassProvider: string;
  LRowColorPatterns: TEFPairs;
  LRowColorFieldName: string;
begin
  { TODO : investigate the row body feature }
  LGroupingFieldName := GetGroupingFieldName;
  LGroupingMenu := ViewTable.GetBoolean('Controller/Grouping/EnableMenu');
  if (LGroupingFieldName <> '') or LGroupingMenu then
  begin
    FGridView := TExtGridGroupingView.Create;
    TExtGridGroupingView(FGridView).EmptyGroupText := _('No data to display in this group.');
    { TODO : use singular and plural display labels of the form }
    //TExtGridGroupingView(FGridView).GroupTextTpl := '{text} ({[values.rs.length]} {[values.rs.length > 1 ? "Items" : "Item"]})';
    TExtGridGroupingView(FGridView).StartCollapsed := ViewTable.GetBoolean('Controller/Grouping/StartCollapsed');
    TExtGridGroupingView(FGridView).EnableGroupingMenu := LGroupingMenu;
    TExtGridGroupingView(FGridView).EnableNoGroups := LGroupingMenu;
    TExtGridGroupingView(FGridView).HideGroupedColumn := True;
    TExtGridGroupingView(FGridView).ShowGroupName := ViewTable.GetBoolean('Controller/Grouping/ShowName');
    if ViewTable.GetBoolean('Controller/Grouping/ShowCount') then
    begin
      LCountTemplate := ViewTable.GetString('Controller/Grouping/ShowCount/Template',
        '{text} ({[values.rs.length]} {[values.rs.length > 1 ? "%ITEMS%" : "%ITEM%"]})');
      LCountTemplate := ReplaceText(LCountTemplate, '%ITEMS%',
        _(ViewTable.GetString('Controller/Grouping/ShowCount/PluralItemName', ViewTable.PluralDisplayLabel)));
      LCountTemplate := ReplaceText(LCountTemplate, '%ITEM%',
        _(ViewTable.GetString('Controller/Grouping/ShowCount/ItemName', ViewTable.DisplayLabel)));
      TExtGridGroupingView(FGridView).GroupTextTpl := LCountTemplate;
    end;
  end
  else
    FGridView := TExtGridGridView.Create;
  FGridView.EmptyText := _('No data to display.');
  FGridView.EnableRowBody := True;
  { TODO : make ForceFit configurable? }
  FGridView.ForceFit := False;
  LRowClassProvider := ViewTable.GetExpandedString('Controller/RowClassProvider');
  if LRowClassProvider <> '' then
    FGridView.JSCode('getRowClass:' + LRowClassProvider)
  else
  begin
    LRowColorPatterns := GetRowColorPatterns(LRowColorFieldName);
    if Length(LRowColorPatterns) > 0 then
      FGridView.JSCode('getRowClass:' +
        Format('function (r) { return getRowColorStyleRule(r, ''%s'', [%s]);}',
          [LRowColorFieldName, PairsToJSON(LRowColorPatterns)]));
  end;
  FGridEditorPanel.View := FGridView;
end;

procedure TKExtGridPanel.InitDefaults;
begin
  inherited;
  FButtonsRequiringSelection := TList<TExtObject>.Create;
  FGridEditorPanel := TExtGridEditorGridPanel.AddTo(Items);
  FGridEditorPanel.Border := False;
  FGridEditorPanel.Header := False;
  FGridEditorPanel.Region := rgCenter;
  FSelModel := TExtGridRowSelectionModel.Create;
  FSelModel.SingleSelect := True;
  FSelModel.Grid := FGridEditorPanel;
  FGridEditorPanel.SelModel := FSelModel;
  FGridEditorPanel.StripeRows := True;
  FGridEditorPanel.Frame := False;
  FGridEditorPanel.AutoScroll := True;
  FGridEditorPanel.AutoWidth := True;
  FGridEditorPanel.ColumnLines := True;
  FGridEditorPanel.TrackMouseOver := True;
end;

procedure TKExtGridPanel.InitColumns;
var
  I: Integer;
  LLayout: TKLayout;
  LLayoutName: string;

  procedure AddGridColumn(const AViewField: TKViewField);
  var
    LColumn: TExtGridColumn;
    LColumnWidth: Integer;

    function SetRenderer(const AColumn: TExtGridColumn): Boolean;
    var
      LImages: TEFNode;
      LTriples: TEFTriples;
      I: Integer;
    begin
      Result := False;

      LImages := AViewField.FindNode('Images');
      if Assigned(LImages) and (LImages.ChildCount > 0) then
      begin
        // Get image list into array of triples (URL/regexp/template).
        SetLength(LTriples, LImages.ChildCount);
        for I := 0 to LImages.ChildCount - 1 do
        begin
          LTriples[I].Value1 := Session.Config.GetImageURL(LImages.Children[I].Name);
          LTriples[I].Value2 := LImages.Children[I].AsExpandedString;
          LTriples[I].Value3 := LImages.Children[I].GetExpandedString('DisplayTemplate');
          if LTriples[I].Value3 = '' then
            LTriples[I].Value3 := AViewField.DisplayTemplate;
        end;
        // Pass array to the client-side renderer.
        AColumn.RendererExtFunction := AColumn.JSFunction('v',
          Format('return formatWithImage(v, [%s], %s);',
            [TriplesToJSON(LTriples), IfThen(AViewField.BlankValue, 'false', 'true')]));
      end;
    end;

    function CreateColumn: TExtGridColumn;
    var
      LDataType: TEFDataType;
      LFormat: string;
    begin
      LDataType := AViewField.DataType;
      if LDataType is TKReferenceDataType then
        LDataType := AViewField.ModelField.ReferencedModel.CaptionField.DataType;

      if LDataType is TEFBooleanDataType then
      begin
        // Don't use TExtGridBooleanColumn here, otherwise the renderer will be inneffective.
        Result := TExtGridColumn.AddTo(FGridEditorPanel.Columns);
        if not SetRenderer(Result) then
          Result.Renderer := 'checkboxRenderer';
      end
      else if LDataType is TEFDateDataType then
      begin
        Result := TExtGridDateColumn.AddTo(FGridEditorPanel.Columns);
        LFormat := AViewField.DisplayFormat;
        if LFormat = '' then
          LFormat := Session.Config.UserFormatSettings.ShortDateFormat;
        TExtGridDateColumn(Result).Format := DelphiDateFormatToJSDateFormat(LFormat);
      end
      else if LDataType is TEFTimeDataType then
      begin
        Result := TExtGridColumn.AddTo(FGridEditorPanel.Columns);
        if not SetRenderer(Result) then
        begin
          LFormat := AViewField.DisplayFormat;
          if LFormat = '' then
            LFormat := Session.Config.UserFormatSettings.ShortTimeFormat;
          Result.RendererExtFunction := Result.JSFunction('v',
            Format('return formatTime(v, "%s");', [DelphiTimeFormatToJSTimeFormat(LFormat)]));
        end;
      end
      else if LDataType is TEFDateTimeDataType then
      begin
        Result := TExtGridDateColumn.AddTo(FGridEditorPanel.Columns);
        LFormat := AViewField.DisplayFormat;
        if LFormat = '' then
          LFormat := Session.Config.UserFormatSettings.ShortDateFormat + ' ' +
            Session.Config.UserFormatSettings.ShortTimeFormat;
        TExtGridDateColumn(Result).Format := DelphiDateTimeFormatToJSDateTimeFormat(LFormat);
      end
      else if LDataType is TEFIntegerDataType then
      begin
        Result := TExtGridNumberColumn.AddTo(FGridEditorPanel.Columns);
        if not SetRenderer(Result) then
        begin
          LFormat := AViewField.DisplayFormat;
          if LFormat = '' then
            LFormat := '0,000'; // '0';
          TExtGridNumberColumn(Result).Format := AdaptExtNumberFormat(LFormat, Session.Config.UserFormatSettings);
          Result.Align := alRight;
        end;
      end
      else if (LDataType is TEFFloatDataType) or (LDataType is TEFDecimalDataType) then
      begin
        Result := TExtGridNumberColumn.AddTo(FGridEditorPanel.Columns);
        if not SetRenderer(Result) then
        begin
          LFormat := AViewField.DisplayFormat;
          if LFormat = '' then
            LFormat := '0,000.' + DupeString('0', AViewField.DecimalPrecision);
          TExtGridNumberColumn(Result).Format := AdaptExtNumberFormat(LFormat, Session.Config.UserFormatSettings);
          Result.Align := alRight;
        end;
      end
      else if LDataType is TEFCurrencyDataType then
      begin
        Result := TExtGridNumberColumn.AddTo(FGridEditorPanel.Columns);
        if not SetRenderer(Result) then
        begin
          { TODO : format as money? }
          LFormat := AViewField.DisplayFormat;
          if LFormat = '' then
            LFormat := '0,000.00';
          TExtGridNumberColumn(Result).Format := AdaptExtNumberFormat(LFormat, Session.Config.UserFormatSettings);
          Result.Align := alRight;
        end;
      end
      else
      begin
        Result := TExtGridColumn.AddTo(FGridEditorPanel.Columns);
        SetRenderer(Result);
      end;
      if not ViewTable.IsFieldVisible(AViewField) and not (AViewField.AliasedName = GetGroupingFieldName) then
        FGridEditorPanel.ColModel.SetHidden(FGridEditorPanel.Columns.Count - 1, True);
    end;

  begin
    LColumn := CreateColumn;
    LColumn.Sortable := not AViewField.IsBlob;
    LColumn.Header := _(AViewField.DisplayLabel);
    LColumn.DataIndex := AViewField.AliasedName;

    LColumnWidth := AViewField.DisplayWidth;
    if LColumnWidth = 0 then
      LColumnWidth := Min(IfThen(AViewField.Size = 0, 40, AViewField.Size), 40);
    LColumn.Width := CharsToPixels(LColumnWidth);

    { TODO : add in-place editing as an option. }
    LColumn.Editable := False;
    //LColumn.Editor := ...
    LColumn.Hidden := not ViewTable.IsFieldVisible(AViewField);
  end;

  function SupportedAsGridColumn(const AViewField: TKViewField): Boolean;
  begin
    Result := not (AViewField.DataType is TEFBlobDataType);
  end;

  procedure AddColumn(const AViewField: TKViewField);
  begin
    if SupportedAsGridColumn(AViewField) then
    begin
      if AViewField.IsAccessGranted(ACM_READ) then
        AddGridColumn(AViewField);
    end;
  end;

begin
  Assert(ViewTable <> nil);

  LLayoutName := ViewTable.GetString('Controller/Grid/Layout');
  if LLayoutName <> '' then
    LLayout := ViewTable.View.Catalog.Layouts.FindLayout(LLayoutName)
  else
    LLayout := ViewTable.FindLayout('Grid');

  if LLayout <> nil then
  begin
    for I := 0 to LLayout.ChildCount - 1 do
      AddColumn(ViewTable.FieldByAliasedName(LLayout.Children[I].AsString));
  end
  else
  begin
    for I := 0 to ViewTable.FieldCount - 1 do
      AddColumn(ViewTable.Fields[I]);
  end;
  FGridEditorPanel.AutoExpandColumn := ViewTable.GetString('Controller/AutoExpandFieldName');
end;

procedure TKExtGridPanel.NewRecord(This: TExtButton; E: TExtEventObjectSingleton);
begin
  ShowEditWindow(nil, emNewRecord);
end;

procedure TKExtGridPanel.EditViewRecord;
begin
  { TODO : Make sure the view/edit button is disabled on the client when there are no records. }
  if ServerStore.RecordCount = 0 then
    Exit;
  ShowEditWindow(Session.LocateRecordFromQueries(ViewTable, ServerStore), emEditCurrentRecord);
end;

function TKExtGridPanel.GetRowColorPatterns(out AFieldName: string): TEFPairs;

  function GetFieldColors(const AField: TKViewField): TEFPairs;
  begin
    Result := AField.GetChildrenAsPairs('Colors', True);
  end;

  function HasFieldColors(const AField: TKViewField): Boolean;
  begin
    Result := Assigned(AField.FindNode('Colors'));
  end;

var
  LFieldNode: TEFNode;
  I: Integer;
begin
  AFieldName := '';
  Result := nil;
  LFieldNode := ViewTable.FindNode('Controller/RowColorField');
  if Assigned (LFieldNode) then
  begin
    AFieldName := LFieldNode.AsExpandedString;
    if LFieldNode.ChildCount > 0 then
      Result := LFieldNode.GetChildPairs(True)
    else
      Result := GetFieldColors(ViewTable.FieldByName(LFieldNode.AsExpandedString));
  end
  else
  begin
    for I := 0 to ViewTable.FieldCount - 1 do
    begin
      if HasFieldColors(ViewTable.Fields[I]) then
      begin
        AFieldName := ViewTable.Fields[I].FieldName;
        Result := GetFieldColors(ViewTable.Fields[I]);
        Break;
      end;
    end;
  end;
end;

procedure TKExtGridPanel.ShowEditWindow(const ARecord: TKRecord;
  const AEditMode: TKEditMode);
var
  LFormControllerType: string;
  LFormController: IKExtController;
begin
  Assert((AEditMode = emNewrecord) or Assigned(ARecord));
  Assert(ViewTable <> nil);

  if Assigned(FEditHostWindow) then
    FEditHostWindow.Free(True);
  FEditHostWindow := TKExtModalWindow.Create;
  FEditHostWindow.Width := ViewTable.GetInteger('Controller/PopupWindow/Width', FEditHostWindow.Width);
  FEditHostWindow.Height := ViewTable.GetInteger('Controller/PopupWindow/Height', FEditHostWindow.Height);
  FEditHostWindow.ResizeHandles := 'n s';
  FEditHostWindow.Closable := False;

  if AEditMode = emNewRecord then
    FEditHostWindow.Title := Format(_('Add %s'), [_(ViewTable.DisplayLabel)])
  else if FIsEditAllowed then
    FEditHostWindow.Title := Format(_('Edit %s'), [_(ViewTable.DisplayLabel)])
  else
    FEditHostWindow.Title := _(ViewTable.DisplayLabel);
  //FEditHostWindow.On('close', Ajax(EditWindowClosed, ['Window', '%0.nm']));

  LFormControllerType := Config.GetString('FormController', 'Form');
  LFormController := TKExtControllerFactory.Instance.CreateController(ViewTable.View, FEditHostWindow, nil, Self, LFormControllerType);
  LFormController.Config.SetObject('Sys/ServerStore', ServerStore);
  if Assigned(ARecord) then
    LFormController.Config.SetObject('Sys/Record', ARecord);
  LFormController.Config.SetObject('Sys/ViewTable', ViewTable);
  LFormController.Config.SetObject('Sys/HostWindow', FEditHostWindow);
  if AEditMode = emNewRecord then
    LFormController.Config.SetString('Sys/Operation', 'Add');
  LFormController.Display;
  FEditHostWindow.Show;
  //JSCode('showWindow(' + FEditHostWindow.JSName + ', ' + TExtObject(LFormController.AsObject).JSName + ');');
end;

procedure TKExtGridPanel.SetViewTable(const AValue: TKViewTable);
var
  LKeyFieldNames: string;
begin
  inherited;

  Assert(Assigned(AValue));

  if Title = '' then
    Title := _(ViewTable.PluralDisplayLabel);

  FIsAddVisible := not ViewTable.GetBoolean('Controller/PreventAdding')
    and not View.GetBoolean('IsReadOnly')
    and not ViewTable.IsReadOnly
    and not Config.GetBoolean('PreventAdding');
  FIsAddAllowed := FIsAddVisible and ViewTable.IsAccessGranted(ACM_ADD);

  FIsEditAllowed := not ViewTable.GetBoolean('Controller/PreventEditing')
    and not View.GetBoolean('IsReadOnly')
    and not ViewTable.IsReadOnly
    and not Config.GetBoolean('PreventEditing')
    and ViewTable.IsAccessGranted(ACM_MODIFY);

  FIsDeleteVisible := not ViewTable.GetBoolean('Controller/PreventDeleting')
    and not View.GetBoolean('IsReadOnly')
    and not ViewTable.IsReadOnly
    and not Config.GetBoolean('PreventDeleting');
  FIsDeleteAllowed := FIsDeleteVisible and ViewTable.IsAccessGranted(ACM_DELETE);

  CreateGridView;

  LKeyFieldNames := Join(ViewTable.GetKeyFieldAliasedNames, ',');
  FGridEditorPanel.On('rowdblclick', AjaxSelection(EditViewRecord, FSelModel, LKeyFieldNames, LKeyFieldNames, []));

  // By default show paging toolbar for large models.
  if ViewTable.GetBoolean('Controller/PagingTools', ViewTable.Model.IsLarge) then
  begin
    FPageRecordCount := ViewTable.GetInteger('Controller/PageRecordCount', 100);
    FGridEditorPanel.Bbar := CreatePagingToolbar;
  end;

  InitColumns;

  CheckGroupColumn;
end;

procedure TKExtGridPanel.CheckGroupColumn;
var
  I: Integer;
  LGroupingFieldName: string;
  LFound: Boolean;
begin
  LGroupingFieldName := GetGroupingFieldName;

  if LGroupingFieldName <> '' then
  begin
    LFound := False;
    for I := 0 to FGridEditorPanel.Columns.Count - 1 do
    begin
      if SameText(TExtGridColumn(FGridEditorPanel.Columns[I]).DataIndex, LGroupingFieldName) then
      begin
        LFound := True;
        Break;
      end;
    end;
    if not LFound then
      raise Exception.CreateFmt('Grouping field %s not found in grid.', [LGroupingFieldName]);
  end;
end;

//function TKExtGridPanel.GetRefreshJSCode: string;
//begin
//  if Assigned(FPagingToolbar) then
//    Result := FPagingToolbar.JSName + '.dorefresh();'
//  else
//    Result := inherited GetRefreshJSCode;
//end;

function TKExtGridPanel.GetRowButtonsDisableJS: string;
var
  I: Integer;
begin
  Result := 'var disabled = s.getCount() == 0;';
  for I := 0 to FButtonsRequiringSelection.Count - 1 do
    Result := Result + Format('%s.setDisabled(disabled);', [FButtonsRequiringSelection[I].JSName]);
end;

procedure TKExtGridPanel.UpdateObserver(const ASubject: IEFSubject;
  const AContext: string);
begin
  inherited;
  if (AContext = 'Confirmed') and Supports(ASubject.AsObject, IKExtController) then
    RefreshData;
end;

procedure TKExtGridPanel.DeleteCurrentRecord;
var
  LRecord: TKViewTableRecord;
begin
  Assert(ViewTable <> nil);

  // Apply BEFORE rules now even though actual save migh be deferred.
  LRecord := Session.LocateRecordFromQueries(ViewTable, ServerStore);
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
    LRecord.Save(True);
    Session.Flash(Format(_('%s deleted.'), [_(ViewTable.DisplayLabel)]));
  end;
  RefreshData;
end;

destructor TKExtGridPanel.Destroy;
begin
  FreeAndNil(FButtonsRequiringSelection);
  inherited;
end;

procedure TKExtGridPanel.RefreshData;
begin
  if Assigned(FPagingToolbar) then
    FPagingToolbar.DoRefresh
  else
    inherited;
end;

function TKExtGridPanel.CreatePagingToolbar: TExtPagingToolbar;
begin
  Assert(ViewTable <> nil);

  FPagingToolbar := TExtPagingToolbar.Create;
  FPagingToolbar.Store := FGridEditorPanel.Store;
  FPagingToolbar.DisplayInfo := False;
  FPagingToolbar.PageSize := FPageRecordCount;
  Result := FPagingToolbar;
  FPagingToolbar.Store := nil; // Avoid double destruction of the store.
end;

function TKExtGridPanel.AddActionButton(const AView: TKView;
  const AToolbar: TExtToolbar): TKExtActionButton;
begin
  Result := inherited AddActionButton(AView, AToolbar);
  if AView.GetBoolean('Controller/RequireSelection', True) then
    FButtonsRequiringSelection.Add(Result);
end;

procedure TKExtGridPanel.AddTopToolbarButtons;
var
  LNewButton: TExtButton;
  LEditButton: TExtButton;
  LDeleteButton: TExtButton;
  LKeyFieldNames: string;
begin
  Assert(ViewTable <> nil);
  Assert(TopToolbar <> nil);

  if FIsAddVisible then
  begin
    LNewButton := TExtButton.AddTo(TopToolbar.Items);
    LNewButton.Tooltip := Format(_('Add %s'), [_(ViewTable.DisplayLabel)]);
    LNewButton.Icon := Session.Config.GetImageURL('new_record');
    if not FIsAddAllowed then
      LNewButton.Disabled := True
    else
      LNewButton.OnClick := NewRecord;
  end;

  TExtToolbarSpacer.AddTo(TopToolbar.Items);
  LEditButton := TExtButton.AddTo(TopToolbar.Items);
  if FIsEditAllowed then
  begin
    LEditButton.Tooltip := Format(_('Edit %s'), [_(ViewTable.DisplayLabel)]);
    LEditButton.Icon := Session.Config.GetImageURL('edit_record');
  end
  else
  begin
    LEditButton.Tooltip := Format(_('View %s'), [_(ViewTable.DisplayLabel)]);
    LEditButton.Icon := Session.Config.GetImageURL('view_record');
  end;
  LKeyFieldNames := Join(ViewTable.GetKeyFieldAliasedNames, ',');
  LEditButton.On('click', AjaxSelection(EditViewRecord, FSelModel, LKeyFieldNames, LKeyFieldNames, []));
  FButtonsRequiringSelection.Add(LEditButton);

  if FIsDeleteVisible then
  begin
    TExtToolbarSpacer.AddTo(TopToolbar.Items);
    LDeleteButton := TExtButton.AddTo(TopToolbar.Items);
    LDeleteButton.Tooltip := Format(_('Delete %s'), [_(ViewTable.DisplayLabel)]);
    LDeleteButton.Icon := Session.Config.GetImageURL('delete_record');
    if not FIsDeleteAllowed then
      LDeleteButton.Disabled := True
    else
    begin
      LDeleteButton.Handler := JSFunction(GetSelectConfirmCall(
        Format(_('Selected %s will be deleted. Are you sure?'), [_(ViewTable.DisplayLabel)]), DeleteCurrentRecord));
      FButtonsRequiringSelection.Add(LDeleteButton);
    end;
  end;
  inherited;
end;

procedure TKExtGridPanel.AddTopToolbarToolViewButtons;
begin
  inherited;
  { TODO : Allow to specify the relative order of Controller-level and ViewTable-level tool buttons? }
  AddToolViewButtons(ViewTable.FindNode('Controller/ToolViews'), TopToolbar);
end;

function TKExtGridPanel.GetSelectConfirmCall(const AMessage: string; const AMethod: TExtProcedure): string;
begin
  Result := Format('confirmCall("%s", "%s", ajaxSingleSelection, {methodURL: "%s", selModel: %s, fieldNames: "%s"});',
    [_(Session.Config.AppTitle), AMessage, MethodURI(AMethod), FSelModel.JSName,
    Join(ViewTable.GetKeyFieldAliasedNames, ',')]);
end;

function TKExtGridPanel.GetSelectCall(const AMethod: TExtProcedure): TExtFunction;
var
  LKeyFieldNames: string;
begin
  LKeyFieldNames := Join(ViewTable.GetKeyFieldAliasedNames, ',');
  Result := AjaxSelection(AMethod, FSelModel, LKeyFieldNames, LKeyFieldNames, []);
end;

initialization
  TKExtControllerRegistry.Instance.RegisterClass('GridPanel', TKExtGridPanel);

finalization
  TKExtControllerRegistry.Instance.UnregisterClass('GridPanel');

end.
