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
    FIsDupVisible: Boolean;
    FIsDupAllowed: Boolean;
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
    function GetEditWindowDefaultControllerType: string; virtual;
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
    function IsMultiSelect: Boolean;
  public
    procedure UpdateObserver(const ASubject: IEFSubject;
      const AContext: string = ''); override;
    destructor Destroy; override;
  published
    procedure EditViewRecord;
    procedure DuplicateRecord;
    procedure NewRecord(This: TExtButton; E: TExtEventObjectSingleton);
    procedure DeleteCurrentRecord;
    procedure LoadData; override;
  end;

implementation

uses
  SysUtils, StrUtils, Math, Types,
  EF.Tree, EF.StrUtils, EF.Localization, EF.JSON, EF.Macros,
  Kitto.Metadata.Models, Kitto.Rules, Kitto.AccessControl,
  Kitto.Ext.Session, Kitto.Ext.Utils;

{ TKExtGridPanel }

function TKExtGridPanel.GetOrderByClause: string;
var
  LSortFieldNames: TStringDynArray;
  LGroupingFieldName: string;
  I: Integer;
begin
  LSortFieldNames := ViewTable.GetStringArray('Controller/Grouping/SortFieldNames');
  if Length(LSortFieldNames) = 0 then
  begin
    LGroupingFieldName := GetGroupingFieldName;
    if LGroupingFieldName <> '' then
      Result := ViewTable.FieldByName(GetGroupingFieldName).QualifiedDBNameOrExpression
    else
      Result := inherited GetOrderByClause;
  end
  else
  begin
    for I := Low(LSortFieldNames) to High(LSortFieldNames) do
      LSortFieldNames[I] := ViewTable.FieldByName(LSortFieldNames[I]).QualifiedDBNameOrExpression;
    Result := Join(LSortFieldNames, ', ');
  end;
end;

function TKExtGridPanel.GetEditWindowDefaultControllerType: string;
begin
  Result := 'Form';
end;

function TKExtGridPanel.GetGroupingFieldName: string;
begin
  Result := ViewTable.GetExpandedString('Controller/Grouping/FieldName');
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
    Result := TExtDataGroupingStore.Create(Self);
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
    FGridView := TExtGridGroupingView.Create(Self);
    TExtGridGroupingView(FGridView).EmptyGroupText := _('No data to display in this group.');
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
    FGridView := TExtGridGridView.Create(Self);
  FGridView.EmptyText := _('No data to display.');
  FGridView.EnableRowBody := True;
  { TODO : make ForceFit configurable? }
  FGridView.ForceFit := False;
  LRowClassProvider := ViewTable.GetExpandedString('Controller/RowClassProvider');
  if LRowClassProvider <> '' then
    FGridView.SetCustomConfigItem('getRowClass', [LRowClassProvider])
  else
  begin
    LRowColorPatterns := GetRowColorPatterns(LRowColorFieldName);
    if Length(LRowColorPatterns) > 0 then
      FGridView.SetCustomConfigItem('getRowClass',
        [JSFunction('r', Format('return getColorStyleRuleForRecordField(r, ''%s'', [%s]);',
          [LRowColorFieldName, PairsToJSON(LRowColorPatterns)])), True]);
  end;
  FGridEditorPanel.View := FGridView;
end;

procedure TKExtGridPanel.InitDefaults;
begin
  inherited;
  FButtonsRequiringSelection := TList<TExtObject>.Create;
  FGridEditorPanel := TExtGridEditorGridPanel.CreateAndAddTo(Items);
  FGridEditorPanel.Border := False;
  FGridEditorPanel.Header := False;
  FGridEditorPanel.Region := rgCenter;
  FSelModel := TExtGridRowSelectionModel.Create(FGridEditorPanel);
  FSelModel.Grid := FGridEditorPanel;
  FGridEditorPanel.SelModel := FSelModel;
  FGridEditorPanel.StripeRows := True;
  FGridEditorPanel.Frame := False;
  FGridEditorPanel.AutoScroll := True;
  FGridEditorPanel.AutoWidth := True;
  FGridEditorPanel.ColumnLines := True;
  FGridEditorPanel.TrackMouseOver := True;
  FGridEditorPanel.ClicksToEdit := 1;
end;

function TKExtGridPanel.IsMultiSelect: Boolean;
begin
  Assert(Assigned(FSelModel));

  Result := not FSelModel.SingleSelect;
end;

procedure TKExtGridPanel.InitColumns;
var
  I: Integer;
  LLayout: TKLayout;
  LAutoExpandColumn: string;

  procedure AddGridColumn(const AViewField: TKViewField);
  var
    LColumn: TExtGridColumn;
    LColumnWidth: Integer;

    function SetEditor(const AColumn: TExtGridColumn;
      ADataType: TEFDataType): Boolean;
    var
      LEditable: boolean;
    begin
      { TODO : complete in-place editing as an option. }
      LEditable := False;
      AColumn.Editable := LEditable;
      if LEditable then
      begin
        AColumn.Editor := TExtFormTextField.Create(FGridEditorPanel);
      end;
    end;

    function SetRenderer(const AColumn: TExtGridColumn): Boolean;
    var
      LImages: TEFNode;
      LTriples: TEFTriples;
      I: Integer;
      LCustomRenderer: TEFNode;
      LColorPairs: TEFPairs;
      LColors: TEFNode;
    begin
      Result := False;

      LCustomRenderer := AViewField.FindNode('JSRenderer');
      if Assigned(LCustomRenderer) and (LCustomRenderer.AsString <> '') then
      begin
        AColumn.RendererExtFunction := AColumn.JSFunction('value, metaData, record, rowIndex, colIndex, store',
          LCustomRenderer.AsExpandedString);
        Result := True;
        Exit;
      end;

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
        AColumn.RendererExtFunction := AColumn.JSFunction('value',
          Format('return formatWithImage(value, [%s], %s);',
            [TriplesToJSON(LTriples), IfThen(AViewField.BlankValue, 'false', 'true')]));
        Result := True;
        Exit;
      end;

      LColors := AViewField.FindNode('Colors');
      if Assigned(LColors) and (LColors.ChildCount > 0) then
      begin
        LColorPairs := AViewField.GetColorsAsPairs;
        // Get color list into array of triples (color/regexp/template).
        SetLength(LTriples, Length(LColorPairs));
        for I := 0 to High(LColorPairs) do
        begin
          LTriples[I].Value1 := LColorPairs[I].Key;
          LTriples[I].Value2 := TEFMacroExpansionEngine.Instance.Expand(LColorPairs[I].Value);
          LTriples[I].Value3 := AViewField.DisplayTemplate;
        end;
        // Pass array to the client-side renderer.
        AColumn.RendererExtFunction := AColumn.JSFunction('value, metaData',
          Format(
            'metaData.css += getColorStyleRuleForValue(value, [%s]);' +
            'return %s ? null : formatWithDisplayTemplate(value, ''%s'');',
            [PairsToJSON(LColorPairs), IfThen(AViewField.BlankValue, 'true', 'false'), AViewField.DisplayTemplate]));
        Result := True;
        Exit;
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
        Result := TExtGridColumn.CreateAndAddTo(FGridEditorPanel.Columns);
        if not SetRenderer(Result) then
          Result.Renderer := 'checkboxRenderer';
      end
      else if LDataType is TEFDateDataType then
      begin
        Result := TExtGridDateColumn.CreateAndAddTo(FGridEditorPanel.Columns);
        LFormat := AViewField.DisplayFormat;
        if LFormat = '' then
          LFormat := Session.Config.UserFormatSettings.ShortDateFormat;
        TExtGridDateColumn(Result).Format := DelphiDateFormatToJSDateFormat(LFormat);
      end
      else if LDataType is TEFTimeDataType then
      begin
        Result := TExtGridColumn.CreateAndAddTo(FGridEditorPanel.Columns);
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
        Result := TExtGridDateColumn.CreateAndAddTo(FGridEditorPanel.Columns);
        LFormat := AViewField.DisplayFormat;
        if LFormat = '' then
          LFormat := Session.Config.UserFormatSettings.ShortDateFormat + ' ' +
            Session.Config.UserFormatSettings.ShortTimeFormat;
        TExtGridDateColumn(Result).Format := DelphiDateTimeFormatToJSDateTimeFormat(LFormat);
      end
      else if LDataType is TEFIntegerDataType then
      begin
        Result := TExtGridNumberColumn.CreateAndAddTo(FGridEditorPanel.Columns);
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
        Result := TExtGridNumberColumn.CreateAndAddTo(FGridEditorPanel.Columns);
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
        Result := TExtGridNumberColumn.CreateAndAddTo(FGridEditorPanel.Columns);
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
        Result := TExtGridColumn.CreateAndAddTo(FGridEditorPanel.Columns);
        SetRenderer(Result);
      end;
      if not ViewTable.IsFieldVisible(AViewField) and not (AViewField.AliasedName = GetGroupingFieldName) then
        FGridEditorPanel.ColModel.SetHidden(FGridEditorPanel.Columns.Count - 1, True);

      //In-place editing
      SetEditor(Result, LDataType);
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

  LLayout := FindViewLayout('Grid');
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
  LAutoExpandColumn := ViewTable.GetString('Controller/AutoExpandFieldName');
  if LAutoExpandColumn <> '' then
    FGridEditorPanel.AutoExpandColumn := LAutoExpandColumn;
end;

procedure TKExtGridPanel.NewRecord(This: TExtButton; E: TExtEventObjectSingleton);
begin
  ShowEditWindow(nil, emNewRecord);
end;

procedure TKExtGridPanel.EditViewRecord;
begin
  ShowEditWindow(Session.LocateRecordFromQueries(ViewTable, ServerStore,
    IfThen(IsMultiSelect, 0, -1)), emEditCurrentRecord);
end;

function TKExtGridPanel.GetRowColorPatterns(out AFieldName: string): TEFPairs;
var
  LFieldNode: TEFNode;
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
      Result := ViewTable.FieldByName(LFieldNode.AsExpandedString).GetColorsAsPairs;
  end;
end;

procedure TKExtGridPanel.ShowEditWindow(const ARecord: TKRecord;
  const AEditMode: TKEditMode);
var
  LFormControllerType: string;
  LFormController: IKExtController;
  LWidth: Integer;
  LHeight: Integer;
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
  else if FIsEditAllowed then
    FEditHostWindow.Title := Format(_('Edit %s'), [_(ViewTable.DisplayLabel)])
  else
    FEditHostWindow.Title := _(ViewTable.DisplayLabel);

  LFormControllerType := Config.GetString('FormController', GetEditWindowDefaultControllerType);
  if LFormControllerType = '' then
    LFormControllerType := GetEditWindowDefaultControllerType;
  LFormController := TKExtControllerFactory.Instance.CreateController(
    FEditHostWindow, ViewTable.View, FEditHostWindow, Config.FindNode('FormController'), Self, LFormControllerType);
  LFormController.Config.SetObject('Sys/ServerStore', ServerStore);
  if Assigned(ARecord) then
    LFormController.Config.SetObject('Sys/Record', ARecord);
  LFormController.Config.SetObject('Sys/ViewTable', ViewTable);
  LFormController.Config.SetObject('Sys/HostWindow', FEditHostWindow);

  LWidth := ViewTable.GetInteger('Controller/PopupWindow/Width', -1);
  LHeight := ViewTable.GetInteger('Controller/PopupWindow/Height', -1);
  if (LWidth <> -1) and (LHeight <> -1) then
  begin
    FEditHostWindow.Width := LWidth;
    FEditHostWindow.Height := LHeight;
    LFormController.Config.SetBoolean('Sys/HostWindow/AutoSize', False);
  end
  else
    LFormController.Config.SetBoolean('Sys/HostWindow/AutoSize', True);
  if AEditMode = emNewRecord then
    LFormController.Config.SetString('Sys/Operation', 'Add')
  else if AEditMode = emDupCurrentRecord then
    LFormController.Config.SetString('Sys/Operation', 'Dup')
  else
    LFormController.Config.SetString('Sys/Operation', 'Edit');

  LFormController.Display;

  FEditHostWindow.Show;
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

  FIsDupVisible := (ViewTable.GetBoolean('Controller/AllowDuplicating')
    or Config.GetBoolean('AllowDuplicating'))
    and not ViewTable.GetBoolean('Controller/PreventAdding')
    and not View.GetBoolean('IsReadOnly')
    and not ViewTable.IsReadOnly
    and not Config.GetBoolean('PreventAdding');
  FIsDupAllowed := FIsDupVisible and ViewTable.IsAccessGranted(ACM_ADD);

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

  if not ViewTable.GetBoolean('Controller/IsMultiSelect', False) then
    FSelModel.SingleSelect := True;

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
    LoadData;
end;

procedure TKExtGridPanel.DeleteCurrentRecord;
var
  LRecord: TKViewTableRecord;
begin
  Assert(ViewTable <> nil);

  // Apply BEFORE rules now even though actual save migh be deferred.
  LRecord := Session.LocateRecordFromQueries(ViewTable, ServerStore,
    IfThen(IsMultiSelect, 0, -1));
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
  LoadData;
end;

destructor TKExtGridPanel.Destroy;
begin
  FreeAndNil(FButtonsRequiringSelection);
  inherited;
end;

procedure TKExtGridPanel.DuplicateRecord;
begin
  ShowEditWindow(Session.LocateRecordFromQueries(ViewTable, ServerStore,
    IfThen(IsMultiSelect, 0, -1)), emDupCurrentRecord);
end;

procedure TKExtGridPanel.LoadData;
begin
  if Assigned(FPagingToolbar) then
  begin
    // Calling both DoRefresh and MoveFirst causes a double call to GetRecordPage.
    // Since we now query the database every time in GetRecordPage,
    // calling MoveFirst is enough to trigger a refresh AND a move to the first
    // page if not there already.
    //FPagingToolbar.DoRefresh;
    FPagingToolbar.MoveFirst;
  end
  else
    inherited;
end;

function TKExtGridPanel.CreatePagingToolbar: TExtPagingToolbar;
begin
  Assert(ViewTable <> nil);

  FPagingToolbar := TExtPagingToolbar.Create(Self);
  FPagingToolbar.Store := FGridEditorPanel.Store;
  FPagingToolbar.DisplayInfo := False;
  FPagingToolbar.PageSize := FPageRecordCount;
  Result := FPagingToolbar;
  //FPagingToolbar.Store := nil; // Avoid double destruction of the store.
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
  LDupButton: TExtButton;
begin
  Assert(ViewTable <> nil);
  Assert(TopToolbar <> nil);

  if FIsAddVisible then
  begin
    LNewButton := TExtButton.CreateAndAddTo(TopToolbar.Items);
    LNewButton.Tooltip := Format(_('Add %s'), [_(ViewTable.DisplayLabel)]);
    LNewButton.Icon := Session.Config.GetImageURL('new_record');
    if not FIsAddAllowed then
      LNewButton.Disabled := True
    else
      LNewButton.OnClick := NewRecord;
  end;

  if FIsDupVisible then
  begin
    TExtToolbarSpacer.CreateAndAddTo(TopToolbar.Items);
    LDupButton := TExtButton.CreateAndAddTo(TopToolbar.Items);
    LDupButton.Tooltip := Format(_('Duplicate %s'), [_(ViewTable.DisplayLabel)]);
    LDupButton.Icon := Session.Config.GetImageURL('dup_record');
    if not FIsDupAllowed then
      LDupButton.Disabled := True
    else
    begin
      LKeyFieldNames := Join(ViewTable.GetKeyFieldAliasedNames, ',');
      LDupButton.On('click', AjaxSelection(DuplicateRecord, FSelModel, LKeyFieldNames, LKeyFieldNames, []));
      FButtonsRequiringSelection.Add(LDupButton);
    end;
  end;

  TExtToolbarSpacer.CreateAndAddTo(TopToolbar.Items);
  LEditButton := TExtButton.CreateAndAddTo(TopToolbar.Items);
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
    TExtToolbarSpacer.CreateAndAddTo(TopToolbar.Items);
    LDeleteButton := TExtButton.CreateAndAddTo(TopToolbar.Items);
    LDeleteButton.Tooltip := Format(_('Delete %s'), [_(ViewTable.DisplayLabel)]);
    LDeleteButton.Icon := Session.Config.GetImageURL('delete_record');
    if not FIsDeleteAllowed then
      LDeleteButton.Disabled := True
    else
    begin
      LDeleteButton.Handler := JSFunction(GetSelectConfirmCall(
        Format(_('Selected %s {caption} will be deleted. Are you sure?'), [_(ViewTable.DisplayLabel)]), DeleteCurrentRecord));
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
  if IsMultiSelect then
    Result := Format('confirmCall("%s", "%s", ajaxMultiSelection, {methodURL: "%s", selModel: %s, fieldNames: "%s"});',
      [_(Session.Config.AppTitle), AMessage, MethodURI(AMethod),
      FSelModel.JSName, Join(ViewTable.GetKeyFieldAliasedNames, ',')])
  else
    Result := Format('selectConfirmCall("%s", "%s", %s, "%s", {methodURL: "%s", selModel: %s, fieldNames: "%s"});',
      [_(Session.Config.AppTitle), AMessage, FSelModel.JSName, ViewTable.Model.CaptionFieldName,
      MethodURI(AMethod), FSelModel.JSName, Join(ViewTable.GetKeyFieldAliasedNames, ',')]);
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
