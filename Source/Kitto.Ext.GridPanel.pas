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
  ExtPascal, Ext, ExtData, ExtForm, ExtGrid, ExtPascalUtils, ExtUxGrid,
  EF.ObserverIntf, EF.Types, EF.Tree,
  Kitto.Metadata.Views, Kitto.Metadata.DataView, Kitto.Store, Kitto.Types,
  Kitto.Ext.Base, Kitto.Ext.Controller, Kitto.Ext.DataPanelLeaf, Kitto.Ext.Editors;

type
  TKExtGridPanel = class(TKExtDataPanelLeafController)
  strict private
    FEditorGridPanel: TExtGridEditorGridPanel;
    FIsActionVisible: TDictionary<string, Boolean>;
    FIsActionAllowed: TDictionary<string, Boolean>;
    FGridView: TExtGridGridView;
    FEditHostWindow: TKExtModalWindow;
    FPagingToolbar: TExtPagingToolbar;
    FPageRecordCount: Integer;
    FSelectionModel: TExtGridRowSelectionModel;
    FButtonsRequiringSelection: TList<TExtObject>;
    FInplaceEditing: Boolean;
    function GetGroupingFieldName: string;
    function CreatePagingToolbar: TExtPagingToolbar;
    procedure ShowEditWindow(const ARecord: TKRecord;
      const AEditMode: TKEditMode);
    procedure InitGridColumns;
    function GetRowButtonsDisableJS: string;
    function GetRowColorPatterns(out AFieldName: string): TEFPairs;
    procedure CreateGridView;
    procedure CheckGroupColumn;
    function GetCurrentViewRecord: TKViewTableRecord;
    procedure InitColumnEditors(const ARecord: TKViewTableRecord);
    procedure SetGridColumnEditor(const AEditorManager: TKExtEditorManager;
      const AViewField: TKViewField; const ALayoutNode: TEFNode; const AColumn: TExtGridColumn);
//    function GetConfirmJSCode(const AMethod: TExtProcedure): string;
  private
    FNewButton: TKExtButton;
    FEditButton: TKExtButton;
    FViewButton: TKExtButton;
    FDeleteButton: TKExtButton;
    FDupButton: TKExtButton;
    FConfirmButton: TKExtButton;
    FCancelButton: TKExtButton;
    function GetConfirmJSCode(const AMethod: TExtProcedure): string;
    function GetBeforeEditJSCode(const AMethod: TExtProcedure): string;
    procedure ShowConfirmButtons(const AShow: Boolean);
    function HasDefaultAction: Boolean;
    function GetDefaultAction: string;
    function HasExplicitDefaultAction: Boolean;
  strict protected
    procedure ExecuteNamedAction(const AActionName: string); override;
    function GetEditWindowDefaultControllerType: string; virtual;
    function GetOrderByClause: string; override;
    procedure InitDefaults; override;
    procedure SetViewTable(const AValue: TKViewTable); override;
    function CreateClientStore: TExtDataStore; override;
    procedure BeforeCreateTopToolbar; override;
    procedure AfterCreateTopToolbar; override;
    procedure AddTopToolbarButtons; override;
    procedure AddTopToolbarToolViewButtons; override;
    function GetSelectConfirmCall(const AMessage: string;
      const AMethod: TExtProcedure): string; override;
    function AddActionButton(const AUniqueId: string; const AView: TKView;
      const AToolbar: TKExtToolbar): TKExtActionButton; override;
    function GetSelectCall(const AMethod: TExtProcedure): TExtFunction; override;
    function IsMultiSelect: Boolean;
  public
    procedure UpdateObserver(const ASubject: IEFSubject; const AContext: string = ''); override;
    procedure AfterConstruction; override;
    destructor Destroy; override;
    procedure Activate; override;
  published
    procedure EditRecord;
    procedure ViewRecord;
    procedure DefaultAction;
    procedure DuplicateRecord;
    procedure NewRecord;
    procedure DeleteCurrentRecord;
    procedure LoadData; override;
    procedure SelectionChanged;
    procedure UpdateField;
    procedure BeforeEdit;
    procedure ConfirmInplaceChanges;
    procedure CancelInplaceChanges;
  end;

implementation

uses
  SysUtils, StrUtils, Math, Types,
  superobject,
  EF.StrUtils, EF.Localization, EF.JSON, EF.Macros,
  Kitto.Metadata.Models, Kitto.Rules, Kitto.AccessControl, Kitto.Config,
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

function TKExtGridPanel.GetCurrentViewRecord: TKViewTableRecord;
begin
  Result := ServerStore.GetRecord(Session.GetQueries, Session.Config.JSFormatSettings, IfThen(IsMultiSelect, 0, -1));
end;

function TKExtGridPanel.GetEditWindowDefaultControllerType: string;
begin
  Result := 'Form';
end;

function TKExtGridPanel.GetGroupingFieldName: string;
begin
  Result := ViewTable.GetExpandedString('Controller/Grouping/FieldName');
end;

procedure TKExtGridPanel.AfterConstruction;
begin
  inherited;
  FIsActionAllowed := TDictionary<string, Boolean>.Create;
  FIsActionVisible := TDictionary<string, Boolean>.Create;
end;

procedure TKExtGridPanel.AfterCreateTopToolbar;
var
  LAnyButtonsRequiringSelection: Boolean;
  LServerSideSelectionChangeNeeded: Boolean;
begin
  inherited;
  LAnyButtonsRequiringSelection := FButtonsRequiringSelection.Count > 0;
  // Server-side selectionchange notifcation is expensive - enable only if
  // strictly necessary.
  LServerSideSelectionChangeNeeded := False;//FInplaceEditing;

  // Note: the selectionchange handler must be called in afterrender as well
  // to account for the first row, which is selected by default.
  if LAnyButtonsRequiringSelection then
  begin
    FSelectionModel.On('selectionchange', JSFunction('s', GetRowButtonsDisableJS));
    On('afterrender', JSFunction(Format('var s = %s;', [FSelectionModel.JSName]) + GetRowButtonsDisableJS));
  end;

  if LServerSideSelectionChangeNeeded then
  begin
    FSelectionModel.On('selectionchange', GetSelectCall(SelectionChanged));
    On('afterrender', GetSelectCall(SelectionChanged));
  end;
end;

procedure TKExtGridPanel.BeforeCreateTopToolbar;
begin
  inherited;
  FButtonsRequiringSelection.Clear;
end;

procedure TKExtGridPanel.BeforeEdit;
var
  LReqBody: ISuperObject;
begin
  LReqBody := SO(Session.RequestBody);
  InitColumnEditors(ServerStore.GetRecord(LReqBody.O['data'], Session.Config.UserFormatSettings));

  ShowConfirmButtons(True);
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
  //Result.On('load', FSelectionModel.SelectFirstRow);
  FEditorGridPanel.Store := Result;
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
      LCountTemplate := _(LCountTemplate);
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
  //FGridView.ForceFit := False;
  LRowClassProvider := ViewTable.GetExpandedString('Controller/RowClassProvider');
  if LRowClassProvider <> '' then
    FGridView.GetRowClass :=  FGridView.JSFunctionInLine(LRowClassProvider)
  else
  begin
    LRowColorPatterns := GetRowColorPatterns(LRowColorFieldName);
    if Length(LRowColorPatterns) > 0 then
      FGridView.SetCustomConfigItem('getRowClass',
        [JSFunction('r', Format('return getColorStyleRuleForRecordField(r, ''%s'', [%s]);',
          [LRowColorFieldName, PairsToJSON(LRowColorPatterns)])), True]);
  end;
  FEditorGridPanel.View := FGridView;
end;

procedure TKExtGridPanel.InitDefaults;
begin
  inherited;
  FButtonsRequiringSelection := TList<TExtObject>.Create;
  FEditorGridPanel := TExtGridEditorGridPanel.CreateAndAddTo(Items);
  FEditorGridPanel.Border := False;
  FEditorGridPanel.Header := False;
  FEditorGridPanel.Region := rgCenter;
  FSelectionModel := TExtGridRowSelectionModel.Create(FEditorGridPanel);
  FSelectionModel.Grid := FEditorGridPanel;
  FEditorGridPanel.SelModel := FSelectionModel;
  FEditorGridPanel.StripeRows := True;
  FEditorGridPanel.Frame := False;
  FEditorGridPanel.AutoScroll := True;
  FEditorGridPanel.AutoWidth := True;
  FEditorGridPanel.ColumnLines := True;
  FEditorGridPanel.TrackMouseOver := True;
  FEditorGridPanel.EnableHdMenu := False;
end;

function TKExtGridPanel.IsMultiSelect: Boolean;
begin
  Assert(Assigned(FSelectionModel));

  Result := not FSelectionModel.SingleSelect;
end;

procedure TKExtGridPanel.SetGridColumnEditor(const AEditorManager: TKExtEditorManager;
  const AViewField: TKViewField; const ALayoutNode: TEFNode; const AColumn: TExtGridColumn);
var
  LEditable: boolean;
  LIsReadOnlyNode: TEFNode;
  LEditor: TExtFormField;
  LSubject: IEFSubject;
begin
  Assert(Assigned(AEditorManager));

  if Assigned(ALayoutNode) then
  begin
    LIsReadOnlyNode := ALayoutNode.FindNode('IsReadOnly');
    if Assigned(LIsReadOnlyNode) then
      LEditable := not LIsReadOnlyNode.AsBoolean
    else
      LEditable := FInplaceEditing and not AViewField.IsReadOnly;
  end
  else
    LEditable := FInplaceEditing and not AViewField.IsReadOnly;

  LEditable := LEditable and AViewField.IsAccessGranted(ACM_MODIFY);

  AColumn.Editable := LEditable;
  if LEditable then
  begin
    LEditor := AEditorManager.CreateGridCellEditor(FEditorGridPanel, AViewField);
    if Supports(LEditor, IEFSubject, LSubject) then
      LSubject.AttachObserver(Self);
    FEditItems.Add(LEditor);
    AColumn.Editor := LEditor;
  end;
end;

procedure TKExtGridPanel.InitGridColumns;
var
  I: Integer;
  LLayout: TKLayout;
  LLayoutNode: TEFNode;
  LAutoExpandColumn: string;
  LEditorManager: TKExtEditorManager;
  LFieldName: string;

  procedure AddGridColumn(const AViewField: TKViewField;
    const ALayoutNode: TEFNode);
  var
    LColumn: TExtGridColumn;
    LColumnWidth: Integer;

    function SetRenderer(const AColumn: TExtGridColumn): Boolean;
    var
      LImages: TEFNode;
      LTriples: TEFTriples;
      I: Integer;
      LCustomRenderer: TEFNode;
      LColorPairs: TEFPairs;
      LColors: TEFNode;
      LAllowedValues: TEFPairs;
      LJSCode: string;
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

      if Assigned(ALayoutNode) then
        LImages := ALayoutNode.FindNode('Images')
      else
        LImages := nil;
      if not Assigned(LImages) then
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

      if Assigned(ALayoutNode) then
        LColors := ALayoutNode.FindNode('Colors')
      else
        LColors := nil;
      if not Assigned(LColors) then
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

      LAllowedValues := AViewField.GetChildrenAsPairs('AllowedValues', True);
      if Length(LAllowedValues) > 0 then
      begin
        LJSCode := '';
        for I := Low(LAllowedValues) to High(LAllowedValues) do
        begin
          LAllowedValues[I].Value := _(LAllowedValues[I].Value);
          LJSCode := LJSCode + Format('if (v == "%s") return "%s";' + sLineBreak, [LAllowedValues[I].Key, LAllowedValues[I].Value]);
        end;
        LJSCode := LJSCode + 'return v;';
        AColumn.RendererExtFunction := AColumn.JSFunction('v', LJSCode);
        Result := True;
        Exit;
      end;
    end;

    function CreateColumn: TExtGridColumn;
    var
      LDataType: TEFDataType;
      LFormat: string;
      LAlignNode: TEFNode;

      function GetDisplayFormat: string;
      var
        LDisplayFormatNode: TEFNode;
      begin
        if Assigned(ALayoutNode) then
          LDisplayFormatNode := ALayoutNode.FindNode('DisplayFormat')
        else
          LDisplayFormatNode := nil;
        if Assigned(LDisplayFormatNode) then
          Result := LDisplayFormatNode.AsString
        else
          Result := AViewField.DisplayFormat;
      end;

    begin
      LDataType := AViewField.DataType;
      if LDataType is TKReferenceDataType then
        LDataType := AViewField.ModelField.ReferencedModel.CaptionField.DataType;

      if LDataType is TEFBooleanDataType then
      begin
        // Don't use TExtGridBooleanColumn here, otherwise the renderer will be inneffective.
        Result := TExtGridColumn.CreateAndAddTo(FEditorGridPanel.Columns);
        if not SetRenderer(Result) then
          Result.Renderer := 'checkboxRenderer';
      end
      else if LDataType is TEFDateDataType then
      begin
        Result := TExtGridDateColumn.CreateAndAddTo(FEditorGridPanel.Columns);
        LFormat := GetDisplayFormat;
        if LFormat = '' then
          LFormat := Session.Config.UserFormatSettings.ShortDateFormat;
        TExtGridDateColumn(Result).Format := DelphiDateFormatToJSDateFormat(LFormat);
      end
      else if LDataType is TEFTimeDataType then
      begin
        Result := TExtGridColumn.CreateAndAddTo(FEditorGridPanel.Columns);
        if not SetRenderer(Result) then
        begin
          LFormat := GetDisplayFormat;
          if LFormat = '' then
            LFormat := Session.Config.UserFormatSettings.ShortTimeFormat;
          Result.RendererExtFunction := Result.JSFunction('v',
            Format('return formatTime(v, "%s");', [DelphiTimeFormatToJSTimeFormat(LFormat)]));
        end;
      end
      else if LDataType is TEFDateTimeDataType then
      begin
        Result := TExtGridDateColumn.CreateAndAddTo(FEditorGridPanel.Columns);
        LFormat := GetDisplayFormat;
        if LFormat = '' then
          LFormat := Session.Config.UserFormatSettings.ShortDateFormat + ' ' +
            Session.Config.UserFormatSettings.ShortTimeFormat;
        TExtGridDateColumn(Result).Format := DelphiDateTimeFormatToJSDateTimeFormat(LFormat);
      end
      else if LDataType is TEFIntegerDataType then
      begin
        Result := TExtGridNumberColumn.CreateAndAddTo(FEditorGridPanel.Columns);
        if not SetRenderer(Result) then
        begin
          LFormat := GetDisplayFormat;
          if LFormat = '' then
            LFormat := '0,000'; // '0';
          TExtGridNumberColumn(Result).Format := AdaptExtNumberFormat(LFormat, Session.Config.UserFormatSettings);
        end;
      end
      else if (LDataType is TEFFloatDataType) or (LDataType is TEFDecimalDataType) then
      begin
        Result := TExtGridNumberColumn.CreateAndAddTo(FEditorGridPanel.Columns);
        if not SetRenderer(Result) then
        begin
          LFormat := GetDisplayFormat;
          if LFormat = '' then
            LFormat := '0,000.' + DupeString('0', AViewField.DecimalPrecision);
          TExtGridNumberColumn(Result).Format := AdaptExtNumberFormat(LFormat, Session.Config.UserFormatSettings);
        end;
      end
      else if LDataType is TEFCurrencyDataType then
      begin
        Result := TExtGridNumberColumn.CreateAndAddTo(FEditorGridPanel.Columns);
        if not SetRenderer(Result) then
        begin
          { TODO : format as money? }
          LFormat := GetDisplayFormat;
          if LFormat = '' then
            LFormat := '0,000.00';
          TExtGridNumberColumn(Result).Format := AdaptExtNumberFormat(LFormat, Session.Config.UserFormatSettings);
        end;
      end
      else
      begin
        Result := TExtGridColumn.CreateAndAddTo(FEditorGridPanel.Columns);
        SetRenderer(Result);
      end;

      //Column alignment
      Result.Align := OptionAsGridColumnAlign(LDataType.GetDefaultColumnAlignment);
      if Assigned(ALayoutNode) then
      begin
        LAlignNode := ALayoutNode.FindNode('Align');
        if Assigned(LAlignNode) then
          Result.Align := OptionAsGridColumnAlign(LAlignNode.AsString);
      end;

      if not ViewTable.IsFieldVisible(AViewField) and not (AViewField.AliasedName = GetGroupingFieldName) then
        FEditorGridPanel.ColModel.SetHidden(FEditorGridPanel.Columns.Count - 1, True);

      //In-place editing
      SetGridColumnEditor(LEditorManager, AViewField, ALayoutNode, Result);
    end;

  begin
    LColumn := CreateColumn;
    LColumn.Sortable := not AViewField.IsBlob;
    if Assigned(ALayoutNode) then
      LColumn.Header := _(ALayoutNode.GetString('DisplayLabel', AViewField.DisplayLabel))
    else
      LColumn.Header := _(AViewField.DisplayLabel);
    LColumn.DataIndex := AViewField.AliasedName;

    if Assigned(ALayoutNode) then
      LColumnWidth := ALayoutNode.GetInteger('DisplayWidth', AViewField.DisplayWidth)
    else
      LColumnWidth := AViewField.DisplayWidth;

    if LColumnWidth = 0 then
      LColumnWidth := Min(IfThen(AViewField.Size = 0, 40, AViewField.Size), 40);
    LColumn.Width := CharsToPixels(LColumnWidth);
  end;

  function SupportedAsGridColumn(const AViewField: TKViewField): Boolean;
  begin
    Result := not (AViewField.DataType is TEFBlobDataType);
  end;

  procedure AddColumn(const AViewField: TKViewField;
    const ALayoutNode: TEFNode);
  begin
    if SupportedAsGridColumn(AViewField) then
    begin
      if AViewField.IsAccessGranted(ACM_READ) then
        AddGridColumn(AViewField, ALayoutNode);
    end;
  end;

begin
  Assert(ViewTable <> nil);

  LEditorManager := TKExtEditorManager.Create;
  try
    FreeAndNil(FEditItems);
    FEditItems := TKEditItemList.Create;
    // Only in-place editing supported ATM, not inserting.
    LEditorManager.Operation := eoUpdate;
    LEditorManager.OnGetSession :=
      procedure (out ASession: TKExtSession)
      begin
        ASession := Session;
      end;
    LLayout := FindViewLayout('Grid');
    if LLayout <> nil then
    begin
      for I := 0 to LLayout.ChildCount - 1 do
      begin
        LLayoutNode := LLayout.Children[I];
        LFieldName := LLayoutNode.AsString;
        AddColumn(ViewTable.FieldByAliasedName(LFieldName), LLayoutNode);
      end;
    end
    else
    begin
      for I := 0 to ViewTable.FieldCount - 1 do
        AddColumn(ViewTable.Fields[I], nil);
    end;
    LAutoExpandColumn := ViewTable.GetString('Controller/AutoExpandFieldName');
    if LAutoExpandColumn <> '' then
      FEditorGridPanel.AutoExpandColumn := LAutoExpandColumn;
  finally
    FreeAndNil(LEditorManager);
  end;
end;

procedure TKExtGridPanel.NewRecord;
begin
  ShowEditWindow(nil, emNewRecord);
end;

procedure TKExtGridPanel.EditRecord;
var
  LRecord: TKViewTableRecord;
begin
  LRecord := GetCurrentViewRecord;
  LRecord.ApplyEditRecordRules;
  ShowEditWindow(LRecord, emEditCurrentRecord);
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
  else if (AEditMode = emEditCurrentRecord) and FIsActionAllowed[EDIT_OPERATION] then
    FEditHostWindow.Title := Format(_('Edit %s'), [_(ViewTable.DisplayLabel)])
  else if (AEditMode = emViewCurrentRecord) and FIsActionAllowed[VIEW_OPERATION] then
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
    emNewRecord : LFormController.Config.SetString('Sys/Operation', ADD_OPERATION);
    emDupCurrentRecord : LFormController.Config.SetString('Sys/Operation', DUPLICATE_OPERATION);
    emEditCurrentRecord : LFormController.Config.SetString('Sys/Operation', EDIT_OPERATION);
    emViewCurrentRecord :
    begin
      if not FIsActionAllowed[EDIT_OPERATION] then
        LFormController.Config.SetBoolean('PreventEditing', True);
      LFormController.Config.SetString('Sys/Operation', VIEW_OPERATION);
    end;
  end;

  LFormController.Display;

  FEditHostWindow.Show;
end;

procedure TKExtGridPanel.SelectionChanged;
begin
end;

procedure TKExtGridPanel.InitColumnEditors(const ARecord: TKViewTableRecord);
begin
  Assert(Assigned(ARecord));

  // Set record fields and load data.
  FEditItems.AllEditors(
    procedure (AEditor: IKExtEditor)
    begin
      AEditor.RecordField := ARecord.FieldByName(AEditor.FieldName);
      AEditor.RefreshValue;
    end);
end;

procedure TKExtGridPanel.SetViewTable(const AValue: TKViewTable);
var
  LKeyFieldNames: string;
  LView: TKDataView;
  LViewTable: TKViewTable;
  LEventName: string;
begin
  LView := View;
  LViewTable := AValue;

  Assert(Assigned(AValue));
  Assert(Assigned(LView));
  Assert(Assigned(FEditorGridPanel));

  FIsActionVisible.AddOrSetValue(VIEW_OPERATION, LViewTable.GetBoolean('Controller/AllowViewing') or Config.GetBoolean('AllowViewing'));
  FIsActionAllowed.AddOrSetValue(VIEW_OPERATION, FIsActionVisible[VIEW_OPERATION] and LViewTable.IsAccessGranted(ACM_VIEW));

  FIsActionVisible.AddOrSetValue('New',
    not LViewTable.GetBoolean('Controller/PreventAdding')
    and not LView.GetBoolean('IsReadOnly')
    and not LViewTable.IsReadOnly
    and not Config.GetBoolean('PreventAdding'));
  FIsActionAllowed.AddOrSetValue('New', FIsActionVisible['New'] and LViewTable.IsAccessGranted(ACM_ADD));

  FIsActionVisible.AddOrSetValue(DUPLICATE_OPERATION,
    LViewTable.GetBoolean('Controller/AllowDuplicating')
    or Config.GetBoolean('AllowDuplicating')
    and not LViewTable.GetBoolean('Controller/PreventAdding')
    and not LView.GetBoolean('IsReadOnly')
    and not LViewTable.IsReadOnly
    and not Config.GetBoolean('PreventAdding'));
  FIsActionAllowed.AddOrSetValue(DUPLICATE_OPERATION, FIsActionVisible[DUPLICATE_OPERATION] and LViewTable.IsAccessGranted(ACM_ADD));

  FIsActionVisible.AddOrSetValue(EDIT_OPERATION,
    not LViewTable.GetBoolean('Controller/PreventEditing')
    and not LView.GetBoolean('IsReadOnly')
    and not LViewTable.IsReadOnly
    and not Config.GetBoolean('PreventEditing'));
  FIsActionAllowed.AddOrSetValue(EDIT_OPERATION, FIsActionVisible[EDIT_OPERATION] and LViewTable.IsAccessGranted(ACM_MODIFY));

  FIsActionVisible.AddOrSetValue('Delete',
    not LViewTable.GetBoolean('Controller/PreventDeleting')
    and not LView.GetBoolean('IsReadOnly')
    and not LViewTable.IsReadOnly
    and not Config.GetBoolean('PreventDeleting'));
  FIsActionAllowed.AddOrSetValue('Delete', FIsActionVisible['Delete'] and LViewTable.IsAccessGranted(ACM_DELETE));

  FInplaceEditing := LView.GetBoolean('Controller/InplaceEditing');

  inherited;

  if Title = '' then
    Title := _(LViewTable.PluralDisplayLabel);

  if FInplaceEditing then
    FEditorGridPanel.ClicksToEdit := 1;

  CreateGridView;

  if not LViewTable.GetBoolean('Controller/IsMultiSelect', False) then
    FSelectionModel.SingleSelect := True;

  if not FInplaceEditing and HasDefaultAction then
  begin
    if Session.IsMobileBrowser then
      LEventName := IfThen(HasExplicitDefaultAction, 'rowclick', '')
    else
      LEventName := 'rowdblclick';
    if LEventName <> '' then
    begin
      LKeyFieldNames := Join(LViewTable.GetKeyFieldAliasedNames, ',');
      FEditorGridPanel.On(LEventName, AjaxSelection(DefaultAction, FSelectionModel, LKeyFieldNames, LKeyFieldNames, []));
    end;
  end;

  // By default show paging toolbar for large models.
  if LViewTable.GetBoolean('Controller/PagingTools', LViewTable.Model.IsLarge) then
  begin
    FPageRecordCount := LViewTable.GetInteger('Controller/PageRecordCount', 100);
    FEditorGridPanel.Bbar := CreatePagingToolbar;
  end;

  InitGridColumns;

  CheckGroupColumn;

  if FInplaceEditing then
  begin
    FConfirmButton := TKExtButton.CreateAndAddTo(Buttons);
    FConfirmButton.SetIconAndScale('accept', Config.GetString('ButtonScale', 'medium'));
    FConfirmButton.Text := Config.GetString('ConfirmButton/Caption', _('Save'));
    FConfirmButton.Tooltip := Config.GetString('ConfirmButton/Tooltip', _('Save changes and finish editing'));
    FConfirmButton.Hidden := True;
    FConfirmButton.Handler := Ajax(ConfirmInplaceChanges);

    FCancelButton := TKExtButton.CreateAndAddTo(Buttons);
    FCancelButton.SetIconAndScale('cancel', Config.GetString('ButtonScale', 'medium'));
    FCancelButton.Text := _('Cancel');
    FCancelButton.Tooltip := _('Cancel changes');
    FCancelButton.Hidden := True;
    FCancelButton.Handler := Ajax(CancelInplaceChanges);

    FEditorGridPanel.On('beforeedit', JSFunction('e', GetBeforeEditJSCode(BeforeEdit)));
    FEditorGridPanel.On('afteredit', JSFunction('e', GetConfirmJSCode(UpdateField)));
  end;
end;

function TKExtGridPanel.GetDefaultAction: string;
begin
  Result := ViewTable.GetExpandedString('Controller/DefaultAction');
end;

function TKExtGridPanel.HasDefaultAction: Boolean;
var
  LDefaultAction: string;
begin
  LDefaultAction := GetDefaultAction;
  Result := LDefaultAction <> '';
  if not Result then
    Result := FIsActionAllowed[VIEW_OPERATION] or FIsActionAllowed[EDIT_OPERATION];
end;

function TKExtGridPanel.HasExplicitDefaultAction: Boolean;
begin
  Result := GetDefaultAction <> '';
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
    for I := 0 to FEditorGridPanel.Columns.Count - 1 do
    begin
      if SameText(TExtGridColumn(FEditorGridPanel.Columns[I]).DataIndex, LGroupingFieldName) then
      begin
        LFound := True;
        Break;
      end;
    end;
    if not LFound then
      raise Exception.CreateFmt('Grouping field %s not found in grid.', [LGroupingFieldName]);
  end;
end;

procedure TKExtGridPanel.ConfirmInplaceChanges;
begin
  ShowConfirmButtons(False);
  ServerStore.Save(True);
  Session.Flash(_('Changes saved succesfully.'));
  LoadData;
end;

procedure TKExtGridPanel.CancelInplaceChanges;
begin
  ShowConfirmButtons(False);
  ServerStore.Records.MarkAsClean;
  LoadData;
end;

procedure TKExtGridPanel.ShowConfirmButtons(const AShow: Boolean);
begin
  if AShow then
  begin
    FConfirmButton.Show;
    FCancelButton.Show;
  end
  else
  begin
    FConfirmButton.Hide;
    FCancelButton.Hide;
  end;
  DoLayout();
end;

function TKExtGridPanel.GetRowButtonsDisableJS: string;
var
  I: Integer;
begin
  Result := 'var disabled = s.getCount() == 0;';
  for I := 0 to FButtonsRequiringSelection.Count - 1 do
    Result := Result + Format('%s.setDisabled(disabled);', [FButtonsRequiringSelection[I].JSName]);
end;

procedure TKExtGridPanel.UpdateField;
var
  LReqBody: ISuperObject;
  LError: string;
begin
  LReqBody := SO(Session.RequestBody);
  LError := UpdateRecord(ServerStore.GetRecord(LReqBody.O['new'], Session.Config.UserFormatSettings), LReqBody.O['new'], False);
  if LError = '' then
    // ok - nothing
  else
  begin
    // go back in edit mode.
    //FEditorGridPanel.StartEditing(LReqBody.I['rowIndex'], 0);
  end;
end;

procedure TKExtGridPanel.UpdateObserver(const ASubject: IEFSubject;
  const AContext: string);
begin
  inherited;
  if (AContext = 'Confirmed') and Supports(ASubject.AsObject, IKExtController) then
    LoadData;
end;

procedure TKExtGridPanel.ViewRecord;
begin
  ShowEditWindow(GetCurrentViewRecord, emViewCurrentRecord);
end;

procedure TKExtGridPanel.DefaultAction;
var
  LActionName: string;
begin
  LActionName := GetDefaultAction;
  if LActionName = '' then
  begin
    if FIsActionAllowed[VIEW_OPERATION] then
      LActionName := VIEW_OPERATION
    else if FIsActionAllowed[EDIT_OPERATION] then
      LActionName := EDIT_OPERATION;
  end;
  if LActionName <> '' then
    ExecuteNamedAction(LActionName);
end;

procedure TKExtGridPanel.DeleteCurrentRecord;
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
    LRecord.Save(True);
    Session.Flash(Format(_('%s deleted.'), [_(ViewTable.DisplayLabel)]));
  end;
  LoadData;
end;

destructor TKExtGridPanel.Destroy;
begin
  FreeAndNil(FIsActionAllowed);
  FreeAndNil(FIsActionVisible);
  FreeAndNil(FButtonsRequiringSelection);
  FreeAndNil(FEditItems);
  inherited;
end;


procedure TKExtGridPanel.DuplicateRecord;
begin
  ShowEditWindow(GetCurrentViewRecord, emDupCurrentRecord);
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

procedure TKExtGridPanel.ExecuteNamedAction(const AActionName: string);
begin
  { TODO : check AC? }
  if (AActionName = 'New') then
    FNewButton.PerformClick
  else if (AActionName = EDIT_OPERATION) then
    FEditButton.PerformClick
  else if (AActionName = VIEW_OPERATION) then
    FViewButton.PerformClick
  else if (AActionName = 'Delete') then
    FDeleteButton.PerformClick
  else if (AActionName = DUPLICATE_OPERATION) then
    FDupButton.PerformClick
  else
    inherited;
end;

function TKExtGridPanel.CreatePagingToolbar: TExtPagingToolbar;
begin
  Assert(ViewTable <> nil);

  FPagingToolbar := TExtPagingToolbar.Create(Self);
  FPagingToolbar.Store := FEditorGridPanel.Store;
  FPagingToolbar.DisplayInfo := False;
  FPagingToolbar.PageSize := FPageRecordCount;
  FPagingToolbar.Cls := 'k-bbar';
  Result := FPagingToolbar;
  //FPagingToolbar.Store := nil; // Avoid double destruction of the store.
end;

procedure TKExtGridPanel.Activate;
begin
  inherited;
  if Assigned(FSelectionModel) then
    FSelectionModel.SelectFirstRow;
end;

function TKExtGridPanel.AddActionButton(const AUniqueId: string;
  const AView: TKView; const AToolbar: TKExtToolbar): TKExtActionButton;
begin
  Result := inherited AddActionButton(AUniqueId, AView, AToolbar);

  if AView.GetBoolean('Controller/RequireSelection', True) then
    FButtonsRequiringSelection.Add(Result);
end;

procedure TKExtGridPanel.AddTopToolbarButtons;
var
  LKeyFieldNames: string;
begin
  Assert(ViewTable <> nil);
  Assert(TopToolbar <> nil);

  LKeyFieldNames := Join(ViewTable.GetKeyFieldAliasedNames, ',');

  FNewButton := TKExtButton.CreateAndAddTo(TopToolbar.Items);
  FNewButton.Tooltip := Format(_('Add %s'), [_(ViewTable.DisplayLabel)]);
  FNewButton.SetIconAndScale('new_record');
  FNewButton.On('click', Ajax(NewRecord));
  if not FIsActionVisible['New'] then
    FNewButton.Hidden := True
  else if not FIsActionAllowed['New'] then
    FNewButton.Disabled := True;

  TExtToolbarSpacer.CreateAndAddTo(TopToolbar.Items);
  FDupButton := TKExtButton.CreateAndAddTo(TopToolbar.Items);
  FDupButton.Tooltip := Format(_('Duplicate %s'), [_(ViewTable.DisplayLabel)]);
  FDupButton.SetIconAndScale('dup_record');
  FDupButton.On('click', AjaxSelection(DuplicateRecord, FSelectionModel, LKeyFieldNames, LKeyFieldNames, []));
  if not FIsActionVisible[DUPLICATE_OPERATION] then
    FDupButton.Hidden := True
  else if not FIsActionAllowed[DUPLICATE_OPERATION] then
    FDupButton.Disabled := True
  else
    FButtonsRequiringSelection.Add(FDupButton);

  TExtToolbarSpacer.CreateAndAddTo(TopToolbar.Items);
  FEditButton := TKExtButton.CreateAndAddTo(TopToolbar.Items);
  FEditButton.Tooltip := Format(_('Edit %s'), [_(ViewTable.DisplayLabel)]);
  FEditButton.SetIconAndScale('edit_record');
  FEditButton.On('click', AjaxSelection(EditRecord, FSelectionModel, LKeyFieldNames, LKeyFieldNames, []));
  if not FIsActionVisible[EDIT_OPERATION] or FInplaceEditing then
    FEditButton.Hidden := True
  else if not FIsActionAllowed[EDIT_OPERATION] then
    FEditButton.Disabled := True
  else
    FButtonsRequiringSelection.Add(FEditButton);

  TExtToolbarSpacer.CreateAndAddTo(TopToolbar.Items);
  FDeleteButton := TKExtButton.CreateAndAddTo(TopToolbar.Items);
  FDeleteButton.Tooltip := Format(_('Delete %s'), [_(ViewTable.DisplayLabel)]);
  FDeleteButton.SetIconAndScale('delete_record');
  FDeleteButton.On('click', JSFunction(GetSelectConfirmCall(
    Format(_('Selected %s {caption} will be deleted. Are you sure?'), [_(ViewTable.DisplayLabel)]), DeleteCurrentRecord)));
  if not FIsActionVisible['Delete'] then
    FDeleteButton.Hidden := True
  else if not FIsActionAllowed['Delete'] then
    FDeleteButton.Disabled := True
  else
    FButtonsRequiringSelection.Add(FDeleteButton);

  FViewButton := TKExtButton.CreateAndAddTo(TopToolbar.Items);
  FViewButton.Tooltip := Format(_('View %s'), [_(ViewTable.DisplayLabel)]);
  FViewButton.SetIconAndScale('view_record');
  FViewButton.On('click', AjaxSelection(ViewRecord, FSelectionModel, LKeyFieldNames, LKeyFieldNames, []));
  if not FIsActionVisible[VIEW_OPERATION] then
    FViewButton.Hidden := True
  else if not FIsActionAllowed[VIEW_OPERATION] then
   FViewButton.Disabled := True
  else
    FButtonsRequiringSelection.Add(FViewButton);

  inherited;
end;

function TKExtGridPanel.GetBeforeEditJSCode(const AMethod: TExtProcedure): string;
var
  LCode: string;
begin
  LCode :=
    'var json = new Object;' + sLineBreak +
    'json.data = e.record.data;' + sLineBreak;
  LCode := LCode + GetPOSTAjaxCode(AMethod, [], 'json') + sLineBreak;
  Result := LCode;
end;

function TKExtGridPanel.GetConfirmJSCode(const AMethod: TExtProcedure): string;
var
  LCode: string;
begin
  LCode :=
    'var json = new Object;' + sLineBreak +
    'json.new = e.record.data;' + sLineBreak;

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

procedure TKExtGridPanel.AddTopToolbarToolViewButtons;
begin
  { TODO : Allow to specify the relative order of Controller-level and ViewTable-level tool buttons? }
  inherited AddToolViewButtons(ViewTable.FindNode('Controller/ToolViews'), TopToolbar);
end;

function TKExtGridPanel.GetSelectConfirmCall(const AMessage: string; const AMethod: TExtProcedure): string;
begin
  if IsMultiSelect then
    Result := Format('confirmCall("%s", "%s", ajaxMultiSelection, {methodURL: "%s", selModel: %s, fieldNames: "%s"});',
      [_(Session.Config.AppTitle), AMessage, MethodURI(AMethod),
      FSelectionModel.JSName, Join(ViewTable.GetKeyFieldAliasedNames, ',')])
  else
    { TODO :
      Add CaptionField to ViewTable for cases when the model's CaptionField
      is not part of the ViewTable or is aliased. }
    Result := Format('selectConfirmCall("%s", "%s", %s, "%s", {methodURL: "%s", selModel: %s, fieldNames: "%s"});',
      [_(Session.Config.AppTitle), AMessage, FSelectionModel.JSName, ViewTable.Model.CaptionField.FieldName,
      MethodURI(AMethod), FSelectionModel.JSName, Join(ViewTable.GetKeyFieldAliasedNames, ',')]);
end;

function TKExtGridPanel.GetSelectCall(const AMethod: TExtProcedure): TExtFunction;
var
  LKeyFieldNames: string;
begin
  LKeyFieldNames := Join(ViewTable.GetKeyFieldAliasedNames, ',');
  Result := AjaxSelection(AMethod, FSelectionModel, LKeyFieldNames, LKeyFieldNames, []);
end;

initialization
  TKExtControllerRegistry.Instance.RegisterClass('GridPanel', TKExtGridPanel);

finalization
  TKExtControllerRegistry.Instance.UnregisterClass('GridPanel');

end.
