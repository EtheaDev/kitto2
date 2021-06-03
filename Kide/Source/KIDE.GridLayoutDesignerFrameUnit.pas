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
unit KIDE.GridLayoutDesignerFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, KIDE.TreeDesignerFrameUnit, Vcl.Samples.Spin,
  KIDE.BaseFrameUnit, KIDE.CodeEditorFrameUnit, Vcl.ExtCtrls, Vcl.Tabs,
  System.Actions, Vcl.ActnList, Vcl.ComCtrls, Vcl.ToolWin, Vcl.StdCtrls,
  EF.Tree, KIDE.NodeDesignerFrameUnit, KIDE.PairsValuesFrameUnit, Kitto.Metadata.Views,
  Vcl.Buttons, Vcl.Grids, Vcl.DBGrids, Data.DB,
  Datasnap.DBClient, Kitto.Metadata.DataView;

type
  TGridLayoutDesignerFrame = class(TTreeDesignerFrame)
    EditorPageControl: TPageControl;
    MainTabSheet: TTabSheet;
    DBGrid: TDBGrid;
    ClientDataSet: TClientDataSet;
    DataSource: TDataSource;
    FieldPanel: TPanel;
    DisplayLabelEdit: TLabeledEdit;
    DisplayWidthEdit: TSpinEdit;
    DisplayFormatEdit: TLabeledEdit;
    DisplayWidthLabel: TLabel;
    AlignmentLabel: TLabel;
    AlignmentComboBox: TComboBox;
    FieldNameEdit: TLabeledEdit;
    IsReadOnlyCheckBox: TCheckBox;
    UpdateLayoutAction: TAction;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    procedure DBGridColExit(Sender: TObject);
    procedure DBGridCellClick(Column: TColumn);
    procedure DBGridColEnter(Sender: TObject);
    procedure DBGridEnter(Sender: TObject);
    procedure DBGridColumnMoved(Sender: TObject; FromIndex, ToIndex: Integer);
    procedure DisplayLabelEditChange(Sender: TObject);
    procedure DisplayWidthEditChange(Sender: TObject);
    procedure AlignmentComboBoxChange(Sender: TObject);
    procedure DisplayFormatEditChange(Sender: TObject);
    procedure DBGridDrawColumnCell(Sender: TObject; const Rect: TRect; DataCol: Integer;
      Column: TColumn; State: TGridDrawState);
    procedure DBGridDblClick(Sender: TObject);
    procedure IsReadOnlyCheckBoxClick(Sender: TObject);
    procedure UpdateLayoutActionExecute(Sender: TObject);
  private
    FViewTable: TKViewTable;
    FRefreshing: Boolean;
    function GetNodeLayout: TKLayout;
    procedure ColumnToDesignPanel;
    procedure DesignPanelToColumn;
    function GetNodeField(const AFieldName: string): TEFNode;
    function GridAlignmentToFieldAlignment(const GridAlignment: string): TAlignment;
    function PixelWidthToDisplayWidth(const AWidth: Integer): Integer;
    function DisplayWidthToPixelWidth(const AWidth: Integer): Integer;
    function ColumnAlignmentToGridAlignment(const Alignment: TAlignment): string;
    function GetViewTable: TKViewTable;
  protected
    class function SuitsTree(const ATree: TEFTree): Boolean; override;
    procedure CleanupDefaultsToEditNode; override;
    procedure UpdateDesignPanel(const AForce: Boolean = False); override;
    procedure DesignPanelToEditNode; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

{$R *.dfm}

uses
  TypInfo,
  EF.Classes, KIDE.Utils, KIDE.Project,
  KIDE.Vcl.Editors, KIDE.DesignMetadata,
  Kitto.Ext.Editors, Kitto.Store;

{ TLayoutDesignerFrame }

procedure TGridLayoutDesignerFrame.AlignmentComboBoxChange(Sender: TObject);
begin
  inherited;
  DesignPanelToColumn;
  IsChanged := True;
end;

procedure TGridLayoutDesignerFrame.CleanupDefaultsToEditNode;
begin
  inherited;
end;

constructor TGridLayoutDesignerFrame.Create(AOwner: TComponent);
begin
  inherited;
end;

function TGridLayoutDesignerFrame.ColumnAlignmentToGridAlignment(const Alignment: TAlignment): string;
begin
  case Alignment of
    taLeftJustify: Result := 'left';
    taRightJustify: Result := 'right';
    taCenter: Result := 'center';
  end;
end;

procedure TGridLayoutDesignerFrame.ColumnToDesignPanel;
var
  LColumn: TColumn;
  LField: TField;
begin
  if FRefreshing then Exit;
  FRefreshing := True;
  try
    ChangesDisabled := True;
    LColumn := DBGrid.Columns[DBGrid.SelectedIndex];
    LField := DBGrid.SelectedField;
    if Assigned(LField) then
    begin
      FieldNameEdit.Text := LField.FieldName;
      DisplayLabelEdit.Text := LColumn.Title.Caption;
      DisplayWidthEdit.Value := PixelWidthToDisplayWidth(LColumn.Width);
      AlignmentComboBox.ItemIndex :=
        AlignmentComboBox.Items.IndexOf(ColumnAlignmentToGridAlignment(LColumn.Alignment));
      if IsPublishedProp(LField, 'DisplayFormat') then
      begin
        DisplayFormatEdit.Text := GetStrProp(LField, 'DisplayFormat');
        DisplayFormatEdit.ReadOnly := False;
      end
      else
      begin
        DisplayFormatEdit.Text := '';
        DisplayFormatEdit.ReadOnly := True;
      end;
      IsReadOnlyCheckBox.Checked := LField.ReadOnly;
    end
    else
    begin
      FieldNameEdit.Text := '';
      DisplayLabelEdit.Text := '';
      DisplayWidthEdit.Value := 0;
      AlignmentComboBox.ItemIndex := -1;
      DisplayFormatEdit.Text := '';
      IsReadOnlyCheckBox.Checked := False;
    end;
  finally
    ChangesDisabled := False;
    FRefreshing := False;
  end;
end;

procedure TGridLayoutDesignerFrame.DBGridCellClick(Column: TColumn);
begin
  inherited;
  ColumnToDesignPanel;
end;

procedure TGridLayoutDesignerFrame.DBGridColEnter(Sender: TObject);
begin
  inherited;
  ColumnToDesignPanel;
end;

procedure TGridLayoutDesignerFrame.DBGridColExit(Sender: TObject);
begin
  inherited;
  ColumnToDesignPanel;
end;

procedure TGridLayoutDesignerFrame.DBGridColumnMoved(Sender: TObject; FromIndex, ToIndex: Integer);
begin
  inherited;
  ColumnToDesignPanel;
  IsChanged := True;
end;

procedure TGridLayoutDesignerFrame.DBGridDblClick(Sender: TObject);
var
  LColumn: TColumn;
begin
  inherited;
  LColumn := DBGrid.Columns[DBGrid.SelectedIndex];
  if Assigned(LColumn) then
  begin
    case LColumn.Title.Alignment of
      taLeftJustify: LColumn.Title.Alignment := taCenter;
      taCenter: LColumn.Title.Alignment := taRightJustify;
    else
      LColumn.Title.Alignment := taLeftJustify;
    end;
    LColumn.Alignment := LColumn.Title.Alignment;
    ColumnToDesignPanel;
  end;
end;

procedure TGridLayoutDesignerFrame.DBGridDrawColumnCell(Sender: TObject; const Rect: TRect;
  DataCol: Integer; Column: TColumn; State: TGridDrawState);
begin
  inherited;
  ColumnToDesignPanel;
end;

procedure TGridLayoutDesignerFrame.DBGridEnter(Sender: TObject);
begin
  inherited;
  ColumnToDesignPanel;
end;

procedure TGridLayoutDesignerFrame.DesignPanelToColumn;
var
  LColumn: TColumn;
begin
  if FRefreshing then Exit;
  FRefreshing := True;
  try
    LColumn := DBGrid.Columns[DBGrid.SelectedIndex];
    LColumn.Width := DisplayWidthToPixelWidth(DisplayWidthEdit.Value);
    LColumn.Title.Caption := DisplayLabelEdit.Text;
    LColumn.Alignment := GridAlignmentToFieldAlignment(AlignmentComboBox.Text);
    LColumn.Title.Alignment := LColumn.Alignment;
    LColumn.Field.ReadOnly := IsReadOnlyCheckBox.Checked;
  finally
    FRefreshing := False;
  end;
end;

procedure TGridLayoutDesignerFrame.DesignPanelToEditNode;
var
  I: Integer;
  LNewNode: TEFNode;
  LColumn: TColumn;
  LField: TField;
  LViewTable: TKViewTable;
  LViewField: TKViewField;
  LColumnAlignment: string;
  LDisplayWidth: Integer;
  LDisplayFormat: string;
  LInplaceEditing: Boolean;
begin
  inherited;
  EditNode.ClearChildren;
  LViewTable := GetViewTable;
  for I := 0 to DBGrid.Columns.Count-1 do
  begin
    DBGrid.SelectedIndex := I;
    LColumn := DBGrid.Columns[DBGrid.SelectedIndex];
    LField := DBGrid.SelectedField;
    if Assigned(LField) then
    begin
      LViewField := LViewTable.FieldByAliasedName(LField.FieldName);
      if Assigned(LViewField) then
      begin
        LNewNode := TEFNode.Create('Field', LViewField.AliasedName);
        if (LViewField.DisplayLabel <> LColumn.Title.Caption) then
          LNewNode.SetString('DisplayLabel', LColumn.Title.Caption);  

        LDisplayWidth := PixelWidthToDisplayWidth(LColumn.Width);  
        if (LViewField.DisplayWidth <> LDisplayWidth) then
          LNewNode.SetInteger('DisplayWidth', LDisplayWidth);

        LColumnAlignment := ColumnAlignmentToGridAlignment(LColumn.Alignment);
        if not SameText(LViewField.DataType.GetDefaultColumnAlignment, LColumnAlignment) then
          LNewNode.SetString('Alignment', LColumnAlignment);

        if IsPublishedProp(LField, 'DisplayFormat') then
        begin
          LDisplayFormat := GetStrProp(LField, 'DisplayFormat');
          if LViewField.DisplayFormat <> LDisplayFormat then
            LNewNode.SetString('DisplayFormat', LDisplayFormat);
        end;

        LInplaceEditing := LViewTable.View.GetBoolean('Controller/InplaceEditing');
        if LField.ReadOnly <> not (LInplaceEditing and not LViewField.IsReadOnly) then
          LNewNode.SetBoolean('IsReadOnly', LField.ReadOnly);

        EditNode.AddChild(LNewNode);    
      end;
    end;
  end;
  if DBGrid.Columns.Count > 0 then
    DBGrid.SelectedIndex := 0;
end;

procedure TGridLayoutDesignerFrame.DisplayFormatEditChange(Sender: TObject);
begin
  inherited;
  DesignPanelToColumn;
  IsChanged := True;
end;

procedure TGridLayoutDesignerFrame.DisplayLabelEditChange(Sender: TObject);
begin
  inherited;
  DesignPanelToColumn;
  IsChanged := True;
end;

procedure TGridLayoutDesignerFrame.DisplayWidthEditChange(Sender: TObject);
begin
  inherited;
  DesignPanelToColumn;
  IsChanged := True;
end;

function TGridLayoutDesignerFrame.DisplayWidthToPixelWidth(const AWidth: Integer): Integer;
begin
  Result := CharsToPixels(AWidth+1, Self.Font, 0)-1;
end;

function TGridLayoutDesignerFrame.GetViewTable;
begin
  inherited;
  if not Assigned(FViewTable) then
    FViewTable := GetViewTableOfLayout(GetNodeLayout, 'Grid');
  Result := FViewTable;
end;

procedure TGridLayoutDesignerFrame.IsReadOnlyCheckBoxClick(Sender: TObject);
begin
  inherited;
  DesignPanelToColumn;
end;

function TGridLayoutDesignerFrame.GetNodeField(const AFieldName: string): TEFNode;
var
  I: Integer;
  LChildNode: TEFNode;
begin
  Result := nil;
  for I := 0 to EditNode.ChildCount -1 do
  begin
    LChildNode := EditNode.Children[I];
    if SameText(LChildNode.Name, 'Field') and
      (SameText(LChildNode.Value, AFieldName)) then
    begin
      Result := LChildNode;
      break;
    end;
  end;
end;

function TGridLayoutDesignerFrame.GetNodeLayout: TKLayout;
begin
  Result := EditNode as TKLayout;
end;

class function TGridLayoutDesignerFrame.SuitsTree(const ATree: TEFTree): Boolean;
begin
  Result := (ATree is TKLayout) and TKLayout(ATree).IsGridLayout and
    Assigned(GetViewTableOfLayout(TKLayout(ATree), 'Grid'));
end;

function TGridLayoutDesignerFrame.GridAlignmentToFieldAlignment(const GridAlignment: string): TAlignment;
begin
  if SameText(GridAlignment, 'center') then
    Result := taCenter
  else if SameText(GridAlignment, 'right') then
    Result := taRightJustify
  else
    Result := taLeftJustify;
end;

function TGridLayoutDesignerFrame.PixelWidthToDisplayWidth(const AWidth: Integer): Integer;
begin
  Result := AWidth div CharsToPixels(1, Self.Font, 0);
end;

procedure TGridLayoutDesignerFrame.UpdateDesignPanel(const AForce: Boolean);
var
  I: Integer;
  LLayout: TKLayout;
  LLayoutNode: TEFNode;
  LFieldName: string;
  LViewField: TKViewField;
  LFieldDef: TFieldDef;
  LField: TField;
  Column: TColumn;
  LViewTable: TKViewTable;
  LInplaceEditing: Boolean;
  LIsReadOnlyNode: TEFNode;
begin
  inherited;
  LLayout := GetNodeLayout;
  LViewTable := GetViewTable;

  //Build ClientDataSet and Grid
  ClientDataSet.Close;
  ClientDataSet.FieldDefs.Clear;
  FRefreshing := True;
  try
    DBGrid.Columns.Clear;
    for I := 0 to LLayout.ChildCount -1 do
    begin
      LLayoutNode := LLayout.Children[I];
      if SameText(LLayoutNode.Name, 'Field') then
      begin
        LFieldName := LLayoutNode.Value;
        LViewField := LViewTable.FieldByAliasedName(LFieldName);
        Assert(Assigned(LViewField));
        LFieldDef := ClientDataSet.FieldDefs.AddFieldDef;
        LFieldDef.Name := LFieldName;
        LFieldDef.DataType := LViewField.FieldType;
        if LViewField.DataType.HasSize then
          LFieldDef.Size := LViewField.Size;
        if LViewField.DataType.HasScale then
          LFieldDef.Precision := LViewField.DecimalPrecision;
      end;
    end;

    //Update ClientDataSet fields and grid columns attributes
    if ClientDataSet.FieldDefs.Count > 0 then
    begin
      ClientDataSet.CreateDataSet;
      for I := 0 to ClientDataSet.Fields.Count -1 do
      begin
        LField := ClientDataSet.Fields[I];
        LFieldName := LField.FieldName;
        LLayoutNode := GetNodeField(LFieldName);
        Assert(Assigned(LLayoutNode));
        LViewField := LViewTable.FieldByAliasedName(LFieldName);
        Assert(Assigned(LViewField));

        Column := DBGrid.Columns.Add;
        Column.FieldName := LFieldName;

        LField.DisplayWidth := LLayoutNode.GetInteger('DisplayWidth', LViewField.DisplayWidth);
        Column.Width := DisplayWidthToPixelWidth(LField.DisplayWidth);

        LField.DisplayLabel := LLayoutNode.GetString('DisplayLabel', LViewField.DisplayLabel);
        Column.Title.Caption := LField.DisplayLabel;

        if IsPublishedProp(LField, 'DisplayFormat') then
          SetStrProp(LField, 'DisplayFormat', LViewField.DisplayFormat);

        LField.Alignment := GridAlignmentToFieldAlignment(LLayoutNode.GetString('Alignment',
          LViewField.DataType.GetDefaultColumnAlignment));
        Column.Title.Alignment := LField.Alignment;
        Column.Alignment := LField.Alignment;

        LIsReadOnlyNode := LLayoutNode.FindNode('IsReadOnly');
        if Assigned(LIsReadOnlyNode) then
        begin
          LField.ReadOnly := LIsReadOnlyNode.AsBoolean;
        end
        else
        begin
          LInplaceEditing := LViewTable.View.GetBoolean('Controller/InplaceEditing');
          LField.ReadOnly := not (LInplaceEditing and not LViewField.IsReadOnly);
        end;
      end;
    end;
  finally
    FRefreshing := False;
  end;
end;

procedure TGridLayoutDesignerFrame.UpdateLayoutActionExecute(Sender: TObject);
var
  LViewTable: TKViewTable;
  LLayout: TKLayout;
  I: Integer;
  LField: TKViewField;
  LNode: TEFNode;
begin
  inherited;
  LLayout := EditNode as TKLayout;
  LViewTable := GetViewTable;
  for I := 0 to LViewTable.FieldCount -1 do
  begin
    LField := LViewTable.Fields[I];
    if not LField.IsVisible then
      Continue;
    LNode := LLayout.FindChildByNameAndValue('Field', LField.AliasedName, True);
    if not Assigned(LNode) then
      LLayout.AddChild('Field', LField.AliasedName);
  end;
  Apply;
  UpdateDesigner;
end;

initialization
  TTreeDesignerFrameRegistry.Instance.RegisterClass(TGridLayoutDesignerFrame.GetClassId, TGridLayoutDesignerFrame);

finalization
  TTreeDesignerFrameRegistry.Instance.UnregisterClass(TGridLayoutDesignerFrame.GetClassId);

end.
