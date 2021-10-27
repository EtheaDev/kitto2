{-------------------------------------------------------------------------------
   Copyright 2012-2021 Ethea S.r.l.

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
  Generics.Collections
  , EF.ObserverIntf
  , EF.Types
  , EF.Tree
  , Kitto.Metadata.Views
  , Kitto.Metadata.DataView
  , Kitto.Store
  , Kitto.Types
  , Kitto.JS
  , Kitto.JS.Types
  , Kitto.JS.Controller
  , Ext.Base
  , Ext.Data
  , Ext.Form
  , Ext.Grid
  , Kitto.Ext.Base
  , Kitto.Ext.Panel
  , Kitto.Ext.DataPanelLeaf
  , Kitto.Ext.Editors
  ;

const
  DEFAULT_PAGE_RECORD_COUNT = 100;
  DEFAULT_AUTOFORM_WIDTH = 600;
  DEFAULT_AUTOFORM_HEIGHT = 300;

type
  TKExtGridPanel = class(TKExtDataPanelLeafController)
  strict private
    FConfirmButton: TKExtButton;
    FCancelButton: TKExtButton;
    FGridPanel: TExtGridGridPanel;
    FPagingToolbar: TExtPagingToolbar;
    FPageRecordCount: Integer;
    FSelectionModel: TExtSelectionRowModel;
    FInplaceEditing: Boolean;
    FAutoFormContainer: TKExtPanelBase;
    FAutoFormController: IJSController;
    function GetGroupingFieldName: string;
    function CreatePagingToolbar: TExtPagingToolbar;
    procedure InitGridColumns;
    function GetRowColorPatterns(out AFieldName: string): TEFPairs;
    procedure CheckGroupColumn;
    procedure InitColumnEditors(const ARecord: TKViewTableRecord);
    procedure SetGridColumnEditor(const AEditorManager: TKExtEditorManager;
      const AViewField: TKViewField; const ALayoutNode: TEFNode; const AColumn: TExtGridColumn);
    procedure ShowConfirmButtons(const AShow: Boolean);
    function GetSelectLastEditedRecordCode(const ARecord: TKViewTableRecord): string;
    procedure InitGroupingFeature;
    function GetAutoFormPlacement: string;
  strict protected
    procedure ExecuteNamedAction(const AActionName: string); override;
    function GetOrderByClause: string; override;
    procedure InitDefaults; override;
    procedure SetViewTable(const AValue: TKViewTable); override;
    function CreateClientStore: TExtDataStore; override;
    procedure AfterCreateTopToolbar; override;
    procedure AddTopToolbarToolViewButtons; override;
    function GetSelectConfirmCall(const AMessage: string; const AMethod: TJSProcedure): string; override;
    function AddActionButton(const AUniqueId: string; const AView: TKView;
      const AToolbar: TKExtToolbar): TKExtActionButton; override;
    function GetSelectCall(const AMethod: TJSProcedure): TExtExpression; override;
    function IsMultiSelect: Boolean; override;
    function GetDefaultRemoteSort: Boolean; override;
    function GetIsPaged: Boolean; override;
    function IsActionVisible(const AActionName: string): Boolean; override;
    function IsActionSupported(const AActionName: string): Boolean; override;
    procedure AddUsedViewFields; override;
  protected
    function GetObjectNamePrefix: string; override;
  public
    procedure UpdateObserver(const ASubject: IEFSubject; const AContext: string = ''); override;
  //published
    procedure LoadData; override;
    procedure SelectionChanged;
    procedure UpdateField;
    procedure BeforeEdit;
    procedure ConfirmInplaceChanges;
    procedure CancelInplaceChanges;
    procedure ConfirmLookup;
    procedure CancelLookup;
  end;

implementation

uses
  SysUtils
  , StrUtils
  , Math
  , Types
  , EF.StrUtils
  , EF.Localization
  , EF.JSON
  , EF.Macros
  , Kitto.Metadata.Models
  , Kitto.Rules
  , Kitto.AccessControl
  , Kitto.Config
  , Kitto.JS.Formatting
  , Kitto.Web.Application
  , Kitto.Web.Request
  , Kitto.Web.Response
  , Kitto.Ext.Utils
  , Kitto.Ext.Form
  , Kitto.Ext.DataPanel
  ;

{ TKExtGridPanel }

function TKExtGridPanel.GetObjectNamePrefix: string;
begin
  Result := 'grid';
end;

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

function TKExtGridPanel.GetGroupingFieldName: string;
begin
  Result := ViewTable.GetExpandedString('Controller/Grouping/FieldName');
end;

function TKExtGridPanel.GetIsPaged: Boolean;
begin
  Assert(Assigned(ViewTable));

  Result := not ViewTable.IsDetail and ViewTable.GetBoolean('Controller/PagingTools', ViewTable.IsLarge);
end;

function TKExtGridPanel.GetAutoFormPlacement: string;
begin
  Result := Config.GetString('AutoFormPlacement');
end;

procedure TKExtGridPanel.AfterCreateTopToolbar;
var
  LAnyButtonsRequiringSelection: Boolean;
  LServerSideSelectionChangeNeeded: Boolean;
begin
  inherited;
  LAnyButtonsRequiringSelection := FButtonsRequiringSelection.Count > 0;
  if LAnyButtonsRequiringSelection then
    FSelectionModel.On('selectionchange', GenerateAnonymousFunction('s', GetRowButtonsDisableJS));

  // Server-side selectionchange notifcation is expensive - enable only if
  // strictly necessary.
  LServerSideSelectionChangeNeeded := GetAutoFormPlacement <> '';
  if LServerSideSelectionChangeNeeded then
    FSelectionModel.On('selectionchange', GetSelectCall(SelectionChanged));
end;

procedure TKExtGridPanel.BeforeEdit;
begin
  InitColumnEditors(ServerStore.GetRecord(TKWebRequest.Current.JSONContentTree.ChildByName('data'), TKWebApplication.Current.Config.UserFormatSettings));
  ShowConfirmButtons(True);
end;

function TKExtGridPanel.CreateClientStore: TExtDataStore;
begin
  Result := inherited CreateClientStore;
  FGridPanel.Store := Result;
  FSelectionModel.Store := Result;
end;

procedure TKExtGridPanel.InitDefaults;
begin
  inherited;
  FGridPanel := TExtGridGridPanel.CreateAndAddToArray(Items);
  FGridPanel.Border := False;
  FGridPanel.Header := False;
  FGridPanel.Region := 'center';
//  FGridPanel.Layout := 'fit';
  FSelectionModel := TExtSelectionRowModel.Create(FGridPanel);
//  FSelectionModel.Grid := FGridPanel;
  FGridPanel.SelModel := FSelectionModel;
  FGridPanel.StripeRows := True;
  FGridPanel.Frame := False;
  FGridPanel.AutoScroll := True;
  FGridPanel.AutoWidth := True;
  FGridPanel.ColumnLines := True;
  FGridPanel.TrackMouseOver := True;
  FGridPanel.EnableHdMenu := False;
  // We mask globally, see kitto-init.js
  FGridPanel.ViewConfig.SetConfigItem('loadMask', False);
  TKWebResponse.Current.Items.ExecuteJSCode(FGridPanel, Format(
    '%s.getView().on("refresh", function() { %s.select(0); });',
    [FGridPanel.JSName, FSelectionModel.JSName]));
end;

function TKExtGridPanel.IsActionSupported(const AActionName: string): Boolean;
begin
  Result := True;
end;

function TKExtGridPanel.IsActionVisible(const AActionName: string): Boolean;
begin
  Result := inherited IsActionVisible(AActionName);
  if Result and SameText(AActionName, 'Edit') then
    Result := not FInplaceEditing;
end;

function TKExtGridPanel.IsMultiSelect: Boolean;
begin
  Assert(Assigned(FSelectionModel));

  Result := FSelectionModel.Mode = TExtSelectionRowModel.MULTI_SELECT;
end;

procedure TKExtGridPanel.SetGridColumnEditor(const AEditorManager: TKExtEditorManager;
  const AViewField: TKViewField; const ALayoutNode: TEFNode; const AColumn: TExtGridColumn);
var
  LEditable: boolean;
  LIsReadOnlyNode: TEFNode;
  LEditor: TExtFormField;
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
    LEditor := AEditorManager.CreateGridCellEditor(FGridPanel, AViewField);
    LEditor.AttachObserver(Self);
    EditItems.Add(LEditor);
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
      LColorValueFieldName: string;
      LColorPair: string;
    begin
      Result := False;

      LCustomRenderer := AViewField.FindNode('JSRenderer');
      if Assigned(LCustomRenderer) and (LCustomRenderer.AsString <> '') then
      begin
        AColumn.RendererFunc := AColumn.GenerateAnonymousFunction('value, metaData, record, rowIndex, colIndex, store',
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
          LTriples[I].Value1 := TKWebApplication.Current.GetImageURL(LImages.Children[I].Name);
          LTriples[I].Value2 := LImages.Children[I].AsExpandedString;
          LTriples[I].Value3 := LImages.Children[I].GetExpandedString('DisplayTemplate');
          if LTriples[I].Value3 = '' then
            LTriples[I].Value3 := AViewField.DisplayTemplate;
        end;
        // Pass array to the client-side renderer.
        AColumn.RendererFunc := AColumn.GenerateAnonymousFunction('value',
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
          LColorPair := LColorPairs[I].Value;
          TEFMacroExpansionEngine.Instance.Expand(LColorPair);
          LTriples[I].Value2 := LColorPair;
          LTriples[I].Value3 := AViewField.DisplayTemplate;
        end;
        // Pass array to the client-side renderer.
        LColorValueFieldName := AViewField.GetExpandedString('ColorValueFieldName');
        if LColorValueFieldName <> '' then
          AColumn.RendererFunc := AColumn.GenerateAnonymousFunction('value, metaData, record',
            Format(
              'metaData.css += getColorStyleRuleForRecordField(record, ''%s'', [%s]);' +
              'return %s ? null : formatWithDisplayTemplate(value, ''%s'');',
              [LColorValueFieldName, PairsToJSON(LColorPairs), IfThen(AViewField.BlankValue, 'true', 'false'), AViewField.DisplayTemplate]))
        else
          AColumn.RendererFunc := AColumn.GenerateAnonymousFunction('value, metaData',
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
        AColumn.RendererFunc := AColumn.GenerateAnonymousFunction('v', LJSCode);
        Result := True;
        Exit;
      end;
    end;

    function GetStringNode(const AName: string): string;
    var
      LNode: TEFNode;
    begin
      LNode := nil;
      if Assigned(ALayoutNode) then
        LNode := ALayoutNode.FindNode(AName);
      if not Assigned(LNode) then
        LNode := AViewField.FindNode(AName);
      if Assigned(LNode) then
        Result := LNode.AsExpandedString;
    end;

    procedure SetColumnCSS(const AColumn: TExtGridColumn);
    var
      LCSS: string;
      LColor: string;
    begin
      LCSS := GetStringNode('CSS');
      LColor := GetStringNode('Color');
      if LColor <> '' then
        LCSS := LCSS + 'background-color: ' + LColor + ';';
      if LCSS <> '' then
        AColumn.Css := LCSS;
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
        Result := TExtGridCheckColumn.CreateInlineAndAddToArray(FGridPanel.Columns);
        { TODO :  mind inline editing }
        Result.Disabled := True;
        Result.DisabledCls := ''; // make it look enabled.
        SetRenderer(Result);
      end
      else if LDataType is TEFDateDataType then
      begin
        Result := TExtGridDateColumn.CreateInlineAndAddToArray(FGridPanel.Columns);
        LFormat := GetDisplayFormat;
        if LFormat = '' then
          LFormat := TKWebApplication.Current.Config.UserFormatSettings.ShortDateFormat;
        TExtGridDateColumn(Result).Format := TJS.DelphiDateFormatToJSDateFormat(LFormat);
      end
      else if LDataType is TEFTimeDataType then
      begin
        Result := TExtGridColumn.CreateInlineAndAddToArray(FGridPanel.Columns);
        if not SetRenderer(Result) then
        begin
          LFormat := GetDisplayFormat;
          if LFormat = '' then
            LFormat := TKWebApplication.Current.Config.UserFormatSettings.ShortTimeFormat;
          Result.RendererFunc := Result.GenerateAnonymousFunction('v',
            Format('return formatTime(v, "%s");', [TJS.DelphiTimeFormatToJSTimeFormat(LFormat)]));
        end;
      end
      else if LDataType is TEFDateTimeDataType then
      begin
        Result := TExtGridDateColumn.CreateInlineAndAddToArray(FGridPanel.Columns);
        LFormat := GetDisplayFormat;
        if LFormat = '' then
          LFormat := TKWebApplication.Current.Config.UserFormatSettings.ShortDateFormat + ' ' +
            TKWebApplication.Current.Config.UserFormatSettings.ShortTimeFormat;
        TExtGridDateColumn(Result).Format := TJS.DelphiDateTimeFormatToJSDateTimeFormat(LFormat);
      end
      else if LDataType is TEFIntegerDataType then
      begin
        Result := TExtGridNumberColumn.CreateInlineAndAddToArray(FGridPanel.Columns);
        if not SetRenderer(Result) then
        begin
          LFormat := GetDisplayFormat;
          if LFormat = '' then
            LFormat := '0,000'; // '0';
          TExtGridNumberColumn(Result).Format := LFormat;
        end;
      end
      else if (LDataType is TEFFloatDataType) or (LDataType is TEFDecimalDataType) then
      begin
        Result := TExtGridNumberColumn.CreateInlineAndAddToArray(FGridPanel.Columns);
        if not SetRenderer(Result) then
        begin
          LFormat := GetDisplayFormat;
          if LFormat = '' then
            LFormat := '0,000.' + DupeString('0', AViewField.DecimalPrecision);
          TExtGridNumberColumn(Result).Format := LFormat;
        end;
      end
      else if LDataType is TEFCurrencyDataType then
      begin
        Result := TExtGridNumberColumn.CreateInlineAndAddToArray(FGridPanel.Columns);
        if not SetRenderer(Result) then
        begin
          { TODO : format as money? }
          LFormat := GetDisplayFormat;
          if LFormat = '' then
            LFormat := '0,000.00';
          TExtGridNumberColumn(Result).Format := LFormat;
        end;
      end
      else
      begin
        Result := TExtGridColumn.CreateInlineAndAddToArray(FGridPanel.Columns);
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
        Result.Hidden := True;

      //In-place editing
      SetGridColumnEditor(LEditorManager, AViewField, ALayoutNode, Result);

      SetColumnCSS(Result);
    end;

  begin
    AddUsedViewField(AViewField);

    LColumn := CreateColumn;
    LColumn.Sortable := not AViewField.IsBlob;
    LColumn.MenuDisabled := True; //Column Menu always disabled

    if Assigned(ALayoutNode) then
      LColumn.Header := _(ALayoutNode.GetString('DisplayLabel', AViewField.DisplayLabel_Grid))
    else
      LColumn.Header := _(AViewField.DisplayLabel_Grid);
    LColumn.DataIndex := AViewField.AliasedName;

    if Assigned(ALayoutNode) then
      LColumnWidth := ALayoutNode.GetInteger('DisplayWidth', AViewField.DisplayWidth)
    else
      LColumnWidth := AViewField.DisplayWidth;

    if LColumnWidth = 0 then
      LColumnWidth := Min(IfThen(AViewField.Size = 0, 40, AViewField.Size), 40);
    LColumn.WidthFunc := CharsToPixels(LColumnWidth);
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
    EditItems.Clear;
    // Only in-place editing supported ATM, not inserting.
    LEditorManager.Operation := eoUpdate;
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
      FGridPanel.AutoExpandColumn := LAutoExpandColumn;
  finally
    FreeAndNil(LEditorManager);
  end;
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

procedure TKExtGridPanel.SelectionChanged;
var
  LRecord: TKViewTableRecord;
begin
  if GetAutoFormPlacement <> '' then
  begin
    LRecord := FindCurrentViewRecord;
    if Assigned(LRecord) then
    begin
      if not Assigned(FAutoFormController) then
      begin
        FAutoFormController := InitEditController(FAutoFormContainer, LRecord, emViewCurrentRecord);
        FAutoFormController.Config.SetBoolean('HideButtons', True);
        FAutoFormController.Config.SetString('LayoutNamePrefix', 'Auto');
        FAutoFormController.Config.SetString('DetailStyle', 'None');
        FAutoFormController.Display;
      end
      else
      begin
        (FAutoFormController.AsObject as TKExtFormPanelController).ChangeRecord(LRecord);
      end;
      FAutoFormContainer.Show;
    end
    else
      FAutoFormContainer.Hide;
    UpdateLayout;
  end;
end;

procedure TKExtGridPanel.InitColumnEditors(const ARecord: TKViewTableRecord);
begin
  Assert(Assigned(ARecord));

  // Set record fields and load data.
  EditItems.AllEditors(
    procedure (AEditor: IKExtEditor)
    begin
      AEditor.RecordField := ARecord.FieldByName(AEditor.FieldName);
      AEditor.RefreshValue;
    end);
end;

procedure TKExtGridPanel.SetViewTable(const AValue: TKViewTable);
var
  LView: TKDataView;
  LViewTable: TKViewTable;
  LEventName: string;
  LCellEditing: TExtGridPluginCellEditing;
  LRowClassProvider: string;
  LRowColorPatterns: TEFPairs;
  LRowColorFieldName: string;
  LToolbar: TKExtToolbar;
  LAutoFormPlacement: string;
begin
  LView := View;
  LViewTable := AValue;

  Assert(Assigned(AValue));
  Assert(Assigned(LView));
  Assert(Assigned(FGridPanel));

  FInplaceEditing := LView.GetBoolean('Controller/InplaceEditing');

  inherited;

  if Title = '' then
  begin
    if IsLookupMode then
      Title := _(Format('Choose %s', [LViewTable.DisplayLabel]))
    else
      Title := _(LViewTable.PluralDisplayLabel);
  end;

  if FInplaceEditing then
  begin
    LCellEditing := TExtGridPluginCellEditing.CreateInlineAndAddToArray(FGridPanel.Plugins);
    LCellEditing.ClicksToEdit := 1;
  end;

  LRowClassProvider := ViewTable.GetExpandedString('Controller/RowClassProvider');
  if LRowClassProvider <> '' then
    FGridPanel.ViewConfig.SetConfigItem('getRowClass', JSExpressionFromCodeBlock(LRowClassProvider))
  else
  begin
    LRowColorPatterns := GetRowColorPatterns(LRowColorFieldName);
    if Length(LRowColorPatterns) > 0 then
      FGridPanel.ViewConfig.SetConfigItem('getRowClass',
        GenerateAnonymousFunction('r', Format('return getColorStyleRuleForRecordField(r, ''%s'', [%s]);',
          [LRowColorFieldName, PairsToJSON(LRowColorPatterns)])));
  end;

  if LViewTable.GetBoolean('Controller/IsMultiSelect', False) then
    FSelectionModel.Mode := TExtSelectionRowModel.MULTI_SELECT
  else
    FSelectionModel.Mode := TExtSelectionRowModel.SINGLE_SELECT;

  if not FInplaceEditing and HasDefaultAction then
  begin
    if TKWebRequest.Current.IsMobileBrowser then
      LEventName := IfThen(HasExplicitDefaultAction, 'rowclick', '')
    else
      LEventName := 'rowdblclick';
    if LEventName <> '' then
      FGridPanel.On(LEventName, GetSelectCall(DefaultAction));
  end;

  // By default show paging toolbar for large models.
  if GetIsPaged then
  begin
    FPageRecordCount := LViewTable.GetInteger('Controller/PageRecordCount',
      TKWebApplication.Current.Config.Config.GetInteger('Defaults/Grid/PageRecordCount', DEFAULT_PAGE_RECORD_COUNT));
    FGridPanel.Bbar := CreatePagingToolbar;
  end;

  InitGridColumns;

  CheckGroupColumn;

  InitGroupingFeature;

  if IsLookupMode then
  begin
    LToolbar := TKExtToolbar.Create(Self);
    TExtToolbarFill.CreateInlineAndAddToArray(LToolbar.Items);
    Fbar := LToolbar;

    FConfirmButton := TKExtButton.CreateAndAddToArray(LToolbar.Items);
    FConfirmButton.SetIconAndScale('accept', Config.GetString('ButtonScale', 'medium'));
    FConfirmButton.Text := Config.GetString('LookupConfirmButton/Caption', _('Select'));
    FConfirmButton.Tooltip := Config.GetString('LookupConfirmButton/Tooltip', _('Select the current record and close the window'));
    FConfirmButton.On('click', GetSelectCall(ConfirmLookup));
    FButtonsRequiringSelection.Add(FConfirmButton);

    FCancelButton := TKExtButton.CreateAndAddToArray(LToolbar.Items);
    FCancelButton.SetIconAndScale('cancel', Config.GetString('ButtonScale', 'medium'));
    FCancelButton.Text := _('Cancel');
    FCancelButton.Tooltip := _('Close the window without selecting a record');
    //FCancelButton.On('click', Ajax(CancelLookup));
    FCancelButton.On('click', TKWebResponse.Current.Items.AjaxCallMethod(Self).SetMethod(CancelLookup).AsFunction);
  end
  else if FInplaceEditing then
  begin
    LToolbar := TKExtToolbar.Create(Self);
    TExtToolbarFill.CreateInlineAndAddToArray(LToolbar.Items);
    Fbar := LToolbar;

    FConfirmButton := TKExtButton.CreateAndAddToArray(LToolbar.Items);
    FConfirmButton.SetIconAndScale('accept', Config.GetString('ButtonScale', 'medium'));
    FConfirmButton.Text := Config.GetString('ConfirmButton/Caption', _('Save'));
    FConfirmButton.Tooltip := Config.GetString('ConfirmButton/Tooltip', _('Save changes and finish editing'));
    FConfirmButton.Hidden := True;
    //FConfirmButton.On('click', Ajax(ConfirmInplaceChanges));
    FConfirmButton.On('click', TKWebResponse.Current.Items.AjaxCallMethod(Self).SetMethod(ConfirmInplaceChanges).AsFunction);

    FCancelButton := TKExtButton.CreateAndAddToArray(LToolbar.Items);
    FCancelButton.SetIconAndScale('cancel', Config.GetString('ButtonScale', 'medium'));
    FCancelButton.Text := _('Cancel');
    FCancelButton.Tooltip := _('Cancel changes');
    FCancelButton.Hidden := True;
    //FCancelButton.On('click', Ajax(CancelInplaceChanges));
    FCancelButton.On('click', TKWebResponse.Current.Items.AjaxCallMethod(Self).SetMethod(CancelInplaceChanges).AsFunction);
    FGridPanel.On('beforeedit',
      GenerateAnonymousFunction('editor, context', GetJSCode(
        procedure
        begin
          TKWebResponse.Current.Items.ExecuteJSCode(
            'var json = new Object;' + sLineBreak +
            'json.data = context.record.data;');
          TKWebResponse.Current.Items.AjaxCallMethod(Self).SetMethod(BeforeEdit)
            .Post('json')
            .AsExpression;
        end)));
    FGridPanel.On('edit',
      GenerateAnonymousFunction('editor, context', GetJSCode(
        procedure
        begin
          TKWebResponse.Current.Items.ExecuteJSCode(
            'var json = new Object;' + sLineBreak +
            'json.new = context.record.data;' + sLineBreak + // needed for the PK (see GetRecord).
            'json.fieldName = context.field;');
          EditItems.AllEditors(
            procedure (AEditor: IKExtEditor)
            begin
              AEditor.StoreValue('json.new');
            end);
          TKWebResponse.Current.Items.AjaxCallMethod(Self).SetMethod(UpdateField)
            .Post('json')
            .AsExpression;
        end)));
  end;

  LAutoFormPlacement := GetAutoFormPlacement;
  if LAutoFormPlacement <> '' then
  begin
    FAutoFormContainer := TKExtPanelBase.CreateAndAddToArray(Items);
    FAutoFormContainer.Layout := 'fit';
    FAutoFormContainer.Border := False;
    FAutoFormContainer.Header := False;
    FAutoFormContainer.Layout := 'fit';
    FAutoFormContainer.Split := True;
    if SameText(LAutoFormPlacement, 'Right') then
    begin
      FAutoFormContainer.Region := 'east';
      FAutoFormContainer.Width := Config.GetInteger('AutoFormPlacement/Size', DEFAULT_AUTOFORM_WIDTH);
    end
    else if SameText(LAutoFormPlacement, 'Bottom') then
    begin
      FAutoFormContainer.Region := 'south';
      FAutoFormContainer.Height := Config.GetInteger('AutoFormPlacement/Size', DEFAULT_AUTOFORM_HEIGHT);
    end
    else
      raise Exception.CreateFmt('Unsupported AutoFormPlacement value: %s', [LAutoFormPlacement]);
    FAutoFormContainer.Hide;
  end
  else
    FAutoFormContainer := nil;
end;

procedure TKExtGridPanel.InitGroupingFeature;
var
  LGroupingFieldName: string;
  LGroupingMenu: Boolean;
  LGroupingFeature: TExtGridGroupingFeature;
begin
  LGroupingFieldName := GetGroupingFieldName;
  LGroupingMenu := ViewTable.GetBoolean('Controller/Grouping/EnableMenu');

  if (LGroupingFieldName <> '') or LGroupingMenu then
  begin
    if ViewTable.FindField(LGroupingFieldName) = nil then
      raise Exception.CreateFmt('Field %s not found. Cannot group.', [LGroupingFieldName]);
    if LGroupingFieldName <> '' then
    begin
      LGroupingFeature := TExtGridGroupingFeature.Create(FGridPanel);
      LGroupingFeature.StartCollapsed := ViewTable.GetBoolean('Controller/Grouping/StartCollapsed');
      LGroupingFeature.EnableGroupingMenu := LGroupingMenu;
      LGroupingFeature.EnableNoGroups := LGroupingMenu;
      LGroupingFeature.HideGroupedHeader := True;
{ TODO : Set to true if at least one column has a summaryType - see also features Summary/GroupingSummary }
      LGroupingFeature.ShowSummaryRow := False;
{ TODO : use groupHeaderTpl }
//    LGroupingFeature.ShowGroupName := ViewTable.GetBoolean('Controller/Grouping/ShowName');
      FGridPanel.Features.Add(LGroupingFeature);
      ClientStore.GroupField := LGroupingFieldName;
      ClientStore.RemoteSort := True;
    end;
  end;
end;

function TKExtGridPanel.GetDefaultRemoteSort: Boolean;
begin
  Result := GetIsPaged;
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
    for I := 0 to FGridPanel.Columns.Count - 1 do
    begin
      if SameText(TExtGridColumn(FGridPanel.Columns[I]).DataIndex, LGroupingFieldName) then
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
  ViewTable.Model.SaveRecords(ServerStore, not ViewTable.IsDetail, nil);
  TKWebApplication.Current.Toast(_('Changes saved succesfully.'));
  LoadData;
end;

procedure TKExtGridPanel.ConfirmLookup;
var
  LParentDataPanel: TKExtDataPanelController;
begin
  LParentDataPanel := GetParentDataPanel;
  LParentDataPanel.Close;
  LParentDataPanel.Config.SetObject('Sys/LookupResultRecord', GetCurrentViewRecord);
  LParentDataPanel.NotifyObservers('LookupConfirmed');
end;

procedure TKExtGridPanel.CancelInplaceChanges;
begin
  ShowConfirmButtons(False);
  ServerStore.Records.MarkAsClean;
  LoadData;
end;

procedure TKExtGridPanel.CancelLookup;
var
  LParentDataPanel: TKExtDataPanelController;
begin
  LParentDataPanel := GetParentDataPanel;
  LParentDataPanel.Close;
  LParentDataPanel.NotifyObservers('LookupCanceled');
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
  UpdateLayout();
end;

procedure TKExtGridPanel.UpdateField;
var
  LError: string;
  LNewValues: TEFNode;
begin
  LNewValues := TKWebRequest.Current.JSONContentTree.ChildByName('new');
  LError := UpdateRecord(ServerStore.GetRecord(LNewValues, TKWebApplication.Current.Config.UserFormatSettings),
    LNewValues, TKWebRequest.Current.JSONContentTree.GetString('fieldName'), False);
  if LError = '' then
    // ok - nothing
  else
  begin
    // go back in edit mode.
    //FGridPanel.StartEditing(LReqBody.I['rowIndex'], 0);
  end;
end;

function TKExtGridPanel.GetSelectLastEditedRecordCode(const ARecord: TKViewTableRecord): string;
var
  LFunction: string;
  LFieldNames: TStringDynArray;
  LPredicate: string;
  I: Integer;
  LPredicates: string;
begin
  if Assigned(ARecord) then
  begin
    // Prepare a JS function that compares all key values in a record r with the
    // values in ARecord in order to locate it.
    LPredicate := '';
    LPredicates := '';
    LFieldNames := ViewTable.GetKeyFieldAliasedNames;
    for I := Low(LFieldNames) to High(LFieldNames) do
    begin
      LPredicate := Format('(r.get("%s") == %s)', [LFieldNames[I], ARecord.FieldByName(LFieldNames[I]).GetAsJSONValue(False)]);
      if LPredicates = '' then
        LPredicates := LPredicate
      else
        LPredicates := LPredicates + ' && ' + LPredicate;
    end;

    LFunction := Format('function(r) { if (%s) return true; else return false;}', [LPredicates]);

    // Make sure the record is selected and scrolled into view.
    //   findBy() retrieves the record index through our custom function, and
    //   getAt() returns the corresponding Record object to be passed to the selection model.
    Result :=
      Format('var idx = %s.findBy(%s);', [ClientStore.JSName, LFunction]) + sLineBreak +
      Format('%s.select([%s.getAt(idx)]);', [FSelectionModel.JSName, ClientStore.JSName]);// + sLineBreak +
//      Format('%s.getRow(idx).scrollIntoView();', [FExtGridView.JSName]);
  end
  else
    Result := 'return false;'
end;

procedure TKExtGridPanel.UpdateObserver(const ASubject: IEFSubject; const AContext: string);
var
  LController: IJSController;
begin
  if Supports(ASubject.AsObject, IJSController, LController) then
  begin
    if MatchText(AContext, ['Confirmed', 'Canceled']) then
    begin
      if Assigned(FAutoFormController) and (FAutoFormController.AsObject = ASubject.AsObject) then
      begin
        FAutoFormContainer.Hide;
        UpdateLayout;
      end;
      ClientStore.On('load', GenerateAnonymousFunction(GetSelectLastEditedRecordCode(LController.Config.GetObject('Sys/Record') as TKViewTableRecord)));
    end;
  end;
  inherited;
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
  if (AActionName = 'LookupConfirm') then
    FConfirmButton.PerformClick
  else
    inherited;
end;

function TKExtGridPanel.CreatePagingToolbar: TExtPagingToolbar;
begin
  Assert(ViewTable <> nil);

  FPagingToolbar := TExtPagingToolbar.Create(Self);
  FPagingToolbar.Store := FGridPanel.Store;
  FPagingToolbar.DisplayInfo := False;
  FPagingToolbar.PageSize := FPageRecordCount;
  FPagingToolbar.Cls := 'k-bbar';
  Result := FPagingToolbar;
  //FPagingToolbar.Store := nil; // Avoid double destruction of the store.
end;

function TKExtGridPanel.AddActionButton(const AUniqueId: string;
  const AView: TKView; const AToolbar: TKExtToolbar): TKExtActionButton;
begin
  Result := inherited AddActionButton(AUniqueId, AView, AToolbar);

  if AView.GetBoolean('Controller/RequireSelection', True) then
    FButtonsRequiringSelection.Add(Result);
end;

procedure TKExtGridPanel.AddTopToolbarToolViewButtons;
begin
  { TODO : Allow to specify the relative order of Controller-level and ViewTable-level tool buttons? }
  inherited AddToolViewButtons(ViewTable.FindNode('Controller/ToolViews'), TopToolbar);
end;

procedure TKExtGridPanel.AddUsedViewFields;
var
  I: Integer;
begin
  //inherited;
  // We only add key fields plus captionfield plus any fields used in the grid columns in order to
  // send smaller JSON data packets. See additional calls to AddUsedViewField
  // elsewhere in this class.
  for I := 0 to ViewTable.FieldCount - 1 do
    if ViewTable.Fields[I].IsKey or
        (ViewTable.Model.CaptionField = ViewTable.Fields[I].ModelField) then
      AddUsedViewField(ViewTable.Fields[I]);
end;

function TKExtGridPanel.GetSelectConfirmCall(const AMessage: string; const AMethod: TJSProcedure): string;
begin
  { TODO :
    Add CaptionField to ViewTable for cases when the model's CaptionField
    is not part of the ViewTable or is aliased. }
  Result := Format('selectConfirmCall("%s", "%s", %s, "%s", {methodURL: "%s", selModel: %s, fieldNames: "%s"});',
    [_(TKWebApplication.Current.Config.AppTitle), AMessage, FSelectionModel.JSName, ViewTable.Model.CaptionField.FieldName,
    GetMethodURL(AMethod), FSelectionModel.JSName, Join(ViewTable.GetKeyFieldAliasedNames, ',')]);
end;

function TKExtGridPanel.GetSelectCall(const AMethod: TJSProcedure): TExtExpression;
begin
  Result := GenerateAnonymousFunction(Format('ajaxSelection("yes", "", {params: {methodURL: "%s", selModel: %s, fieldNames: "%s"}});',
    [GetMethodURL(AMethod), FSelectionModel.JSName, Join(ViewTable.GetKeyFieldAliasedNames, ',')]));
end;

initialization
  TJSControllerRegistry.Instance.RegisterClass('GridPanel', TKExtGridPanel);

finalization
  TJSControllerRegistry.Instance.UnregisterClass('GridPanel');

end.
