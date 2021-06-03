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
unit KIDE.ModelFieldsDesignerFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, KIDE.MetadataItemDesignerFrameUnit,
  KIDE.BaseFrameUnit, KIDE.CodeEditorFrameUnit, Vcl.ExtCtrls, Vcl.Tabs, EF.Tree,
  Vcl.StdCtrls, System.Actions, Vcl.ActnList, Vcl.ComCtrls, Vcl.ToolWin, Vcl.Samples.Spin, Vcl.DBGrids,
  Kitto.Metadata.Models,
  KIDE.PairsValuesFrameUnit, KIDE.EditNodeBaseFrameUnit, Data.DB, Datasnap.DBClient, Vcl.Grids,
  KIDE.DbGrid, Vcl.DBCtrls;

type
  TModelFieldsDesignerFrame = class(TMetadataItemDesignerFrame)
    FieldsDataSet: TClientDataSet;
    FieldNameEdit: TStringField;
    _PhysicalName: TStringField;
    _DefaultValue: TStringField;
    TypeComboBox: TStringField;
    ReferenceModelEditComboBox: TStringField;
    _FileNameField: TStringField;
    FieldSizeEdit: TIntegerField;
    DecimalPrecisionEdit: TIntegerField;
    PrimaryKeyCheckBox: TBooleanField;
    RequiredCheckBox: TBooleanField;
    _IsVisible: TBooleanField;
    _IsReadOnly: TBooleanField;
    _EmptyAsNull: TBooleanField;
    FieldsDBGrid: TDBGrid;
    FieldsLabel: TLabel;
    FieldsDataSource: TDataSource;
    _DisplayWidth: TIntegerField;
    _DisplayFormat: TStringField;
    _EditFormat: TStringField;
    _IsPicture: TBooleanField;
    _CanUpdate: TBooleanField;
    _CanInsert: TBooleanField;
    _DisplayLabel: TStringField;
    _DefaultFilterConnector: TStringField;
    _IsPicture_Thumbnail_Width: TIntegerField;
    _IsPicture_Thumbnail_Height: TIntegerField;
    _Expression: TMemoField;
    _IsComputed: TBooleanField;
    _IsGenerated: TBooleanField;
    _DefaultFilter: TStringField;
    _MaxUploadSize: TStringField;
    DBNavigator: TDBNavigator;
    _IsPassword: TBooleanField;
    procedure TypeComboBoxChange(Sender: TObject);
    procedure BooleanFieldGetText(Sender: TField; var Text: string; DisplayText: Boolean);
    procedure FieldsDataSetBeforeCancel(DataSet: TDataSet);
    procedure FieldsDataSourceStateChange(Sender: TObject);
  private
    function EditModelFields: TKModelFields;
    procedure ProcessFields(AProc: TProc;
      const AEditField: Boolean);
    function GetRecNo: integer;
    function GetComboBoxColumn(const AFieldName: string): TColumn;
    function GetTypeComboBoxColumn: TColumn;
    function GetReferenceModelEditComboBoxColumn: TColumn;
    function IsReferenceKeyField: Boolean;
    function IsFileReferenceField: Boolean;
    procedure SetFieldSpec;
    procedure UpdateDesignPanelForField;
    procedure CleanupDefaultsToEditNodeForField;
    procedure DesignPanelToEditNodeForField;
    property TypeComboBoxColumn: TColumn read GetTypeComboBoxColumn;
    property ReferenceModelEditComboBoxColumn: TColumn read GetReferenceModelEditComboBoxColumn;
  protected
    function GetEditModelField: TKModelField; virtual;
    class function SuitsNode(const ANode: TEFNode): Boolean; override;
    procedure CleanupDefaultsToEditNode; override;
    procedure DesignPanelToEditNode; override;
    procedure UpdateEditComponents; override;
    property EditModelField: TKModelField read GetEditModelField;
  public
    procedure Init(const ANode: TEFTree); override;
    procedure UpdateDesignPanel(const AForce: Boolean = False); override;
  end;

implementation

uses
  KIDE.Project, KIDE.NodeDesignerFrameUnit, Kitto.Metadata.ModelImplementation,
  Kitto.Metadata.DataView;

{$R *.dfm}

{ TModelFieldDesignerFrame }

procedure TModelFieldsDesignerFrame.UpdateEditComponents;
var
  LIsReference, LIsReferenceKeyField, LIsFileReferenceField: Boolean;
  LExpressionField: Boolean;
begin
  inherited;
  LIsReferenceKeyField := IsReferenceKeyField;
  //TypeComboBox.ReadOnly := LIsReferenceKeyField;
  LIsFileReferenceField := IsFileReferenceField;
  if Assigned(EditModelField) then
  begin
    DecimalPrecisionEdit.Visible := EditModelField.DataType.HasScale and not LIsReferenceKeyField;
    FieldSizeEdit.Visible := EditModelField.DataType.HasSize and not LIsReferenceKeyField;
  end;
  PrimaryKeyCheckBox.Visible := not LIsReferenceKeyField;
  RequiredCheckBox.Visible := not LIsReferenceKeyField;
  _CanUpdate.Visible := not LIsReferenceKeyField;
  _CanInsert.Visible := not LIsReferenceKeyField;
  _IsComputed.Visible := not LIsReferenceKeyField;
  _IsPassword.Visible := not LIsReferenceKeyField;
  _IsReadOnly.Visible := not LIsReferenceKeyField;
  _IsGenerated.Visible := not LIsReferenceKeyField;
  _EmptyAsNull.Visible := not LIsReferenceKeyField;
  _DisplayFormat.Visible := not LIsReferenceKeyField;
  _EditFormat.Visible := not LIsReferenceKeyField;
  _DefaultFilterConnector.Visible := not LIsReferenceKeyField;
  _DefaultFilter.Visible := not LIsReferenceKeyField;
  _FileNameField.Visible := LIsFileReferenceField;
  _Expression.Visible := not LIsReferenceKeyField;

  LIsReference := TypeComboBox.Text = TKReferenceDataType.GetTypeName;
  ReferenceModelEditComboBox.Visible := LIsReference;

  LExpressionField := _Expression.Text <> '';
(*
  if LExpressionField then
  begin
    _CanUpdate.AsBoolean := False;
    _CanInsert.AsBoolean := False;
    _IsComputed.AsBoolean := False;
  end;
  _CanUpdate.ReadOnly := LExpressionField;
  _CanInsert.ReadOnly := LExpressionField;
  _IsComputed.ReadOnly := LExpressionField;
*)
  _IsPicture_Thumbnail_Width.Visible := _IsPicture.AsBoolean;
  _IsPicture_Thumbnail_Height.Visible := _IsPicture.AsBoolean;
  _MaxUploadSize.Visible := _IsPicture.AsBoolean or LIsFileReferenceField;
end;

function TModelFieldsDesignerFrame.EditModelFields: TKModelFields;
begin
  if EditNode is TKModelFields then
    Result := EditNode as TKModelFields
  else
    Result := nil;
end;

procedure TModelFieldsDesignerFrame.FieldsDataSetBeforeCancel(DataSet: TDataSet);
begin
  inherited;
  if (EditModelFields <> nil) then
    EditModelFields.Children[EditModelFields.ChildCount-1].Delete;
end;

procedure TModelFieldsDesignerFrame.FieldsDataSourceStateChange(
  Sender: TObject);
begin
  inherited;
  if FieldsDataSource.State in dsEditModes then
    IsChanged := True;
end;

procedure TModelFieldsDesignerFrame.ProcessFields(AProc: TProc;
  const AEditField: Boolean);
begin
  FieldsDataSet.First;
  FieldsDataSet.DisableControls;
  try
    while not FieldsDataSet.eof do
    begin
      if AEditField then
        FieldsDataSet.Edit;
      try
        AProc;
      if AEditField then
        FieldsDataSet.Post;
      except
        FieldsDataSet.Cancel;
      end;
      FieldsDataSet.Next;
    end;
  finally
    FieldsDataSet.First;
    FieldsDataSet.EnableControls;
  end;
end;

procedure TModelFieldsDesignerFrame.BooleanFieldGetText(Sender: TField; var Text: string;
  DisplayText: Boolean);
begin
  inherited;
  if Sender.AsBoolean then
    Text := '1'
  else
    Text := '0';
end;

procedure TModelFieldsDesignerFrame.CleanupDefaultsToEditNode;
begin
  ProcessFields(CleanupDefaultsToEditNodeForField, False);
end;

procedure TModelFieldsDesignerFrame.CleanupDefaultsToEditNodeForField;
var
  LEditModelField: TKModelField;
  LNode: TEFNode;
  LOldEditNode: TEFTree;
begin
  inherited;
  LEditModelField := EditModelField;
  if Assigned(LEditModelField) then
  begin
    LOldEditNode := EditNode;
    SetEditNode(LEditModelField);
    try
      CleanupTextNode('PhysicalName');
      CleanupTextNode('DisplayLabel', LEditModelField.DefaultDisplayLabel);
      CleanupBooleanNode('IsVisible', True);
      CleanupBooleanNode('IsReadOnly');
      CleanupBooleanNode('IsGenerated');
      CleanupTextNode('Hint');
      CleanupIntegerNode('DisplayWidth', LEditModelField.DefaultDisplayWidth);
      CleanupTextNode('DisplayFormat');
      CleanupTextNode('EditFormat');
      CleanupTextNode('DefaultValue');
      CleanupTextNode('DefaultFilterConnector', 'and');
      CleanupBooleanNode('EmptyAsNull', LEditModelField.DefaultEmptyAsNull);
      CleanupBooleanNode('CanUpdate', LEditModelField.CanActuallyModify);
      CleanupBooleanNode('CanInsert', LEditModelField.CanActuallyModify);
      CleanupBooleanNode('IsComputed');
      CleanupBooleanNode('IsPassword');
      CleanupTextNode('FileNameField');
      CleanupTextNode('Expression');
      CleanupTextNode('DefaultFilter');
      CleanupBooleanNode('IsPicture');
      LNode := EditNode.FindNode('IsPicture/Thumbnail/Width');
      if Assigned(LNode) and (LNode.AsInteger = 0) then
        EditNode.DeleteNode('IsPicture/Thumbnail/Width');
      LNode := EditNode.FindNode('IsPicture/Thumbnail/Height');
      if Assigned(LNode) and (LNode.AsInteger = 0) then
        EditNode.DeleteNode('IsPicture/Thumbnail/Height');
      CleanupTextNode('MaxUploadSize');
      CleanupOrphanNode('IsPicture/Thumbnail');
      CleanupOrphanNode('AllowedValues');
    finally
      SetEditNode(LOldEditNode);
    end;
  end;
end;

procedure TModelFieldsDesignerFrame.DesignPanelToEditNode;
begin
  ProcessFields(DesignPanelToEditNodeForField, True);
end;

procedure TModelFieldsDesignerFrame.DesignPanelToEditNodeForField;
var
  I: Integer;
  LFieldsNode: TKModelFields;
  LDeleteFieldNodes: boolean;

  procedure UpdateNodeContent(const AField: TField);
  var
    LFieldName: string;
    LFieldNode: TEFNode;
  begin
    LFieldName := Copy(AField.FieldName,2,MaxInt);
    LFieldNode := EditModelField.GetNode(LFieldName, True);
    if (AField is TStringField) or (AField is TMemoField) then
      LFieldNode.AsString := AField.AsString
    else if AField is TIntegerField then
      LFieldNode.AsInteger := AField.AsInteger
    else if AField is TBooleanField then
      LFieldNode.AsBoolean := AField.AsBoolean;
  end;

begin
  inherited;
  if not (EditNode is TKViewField) then
  begin
    UpdateNodeContent(_PhysicalName);
    UpdateNodeContent(_DefaultValue);
    UpdateNodeContent(_FileNameField);
    UpdateNodeContent(_IsVisible);
    UpdateNodeContent(_IsReadOnly);
    UpdateNodeContent(_EmptyAsNull);
    UpdateNodeContent(_DisplayWidth);
    UpdateNodeContent(_DisplayFormat);
    UpdateNodeContent(_EditFormat);
    UpdateNodeContent(_IsPicture);
    UpdateNodeContent(_CanUpdate);
    UpdateNodeContent(_CanInsert);
    UpdateNodeContent(_DisplayLabel);
    UpdateNodeContent(_DefaultFilterConnector);
//    UpdateNodeContent(_IsPicture_Thumbnail_Width);
//    UpdateNodeContent(_IsPicture_Thumbnail_Height);
    UpdateNodeContent(_Expression);
    UpdateNodeContent(_IsComputed);
    UpdateNodeContent(_IsGenerated);
    UpdateNodeContent(_DefaultFilter);
    UpdateNodeContent(_MaxUploadSize);
    SetFieldSpec;
    EditModelField.Name := FieldNameEdit.Text;
    if Assigned(EditModelField) and EditModelField.IsReference and
      Assigned(EditModelField.ReferencedModel) then
    begin
      LFieldsNode := EditModelField.FindNode('Fields', True) as TKModelFields;
      //If a key field is different from referenced model key then delete all key fields
      LDeleteFieldNodes := False;
      if LFieldsNode.ChildCount <> EditModelField.ReferencedModel.KeyFieldCount then
        LDeleteFieldNodes := True
      else
      begin
      end;
      if LDeleteFieldNodes then
      begin
        LFieldsNode.ClearChildren;
        //Add key nodes for any key field included into reference model
        for I := 0 to EditModelField.ReferencedModel.KeyFieldCount -1 do
        begin
          LFieldsNode.FindNode(EditModelField.ReferencedModel.KeyFields[I].FieldName, True);
        end;
      end;
    end;
  end;
end;

function TModelFieldsDesignerFrame.GetComboBoxColumn(const AFieldName: string): TColumn;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FieldsDBGrid.Columns.Count -1 do
  begin
    Result := FieldsDBGrid.Columns[I];
    if SameText(Result.Field.FieldName, AFieldName) then
      break;
  end;
end;

function TModelFieldsDesignerFrame.GetEditModelField: TKModelField;
var
  LRecNo: Integer;
begin
  LRecNo := GetRecNo;
  if (LRecNo <> 0) and (EditModelFields <> nil) and (EditModelFields.ChildCount < LRecNo) then
  begin
    EditModelFields.AddChild('UNDEFINED');
    UpdateDesignPanelForField;
  end;
  if LRecNo > 0 then
    Result := (EditNode as TKModelFields).Fields[LRecNo-1]
  else
    Result := nil;
end;

function TModelFieldsDesignerFrame.GetRecNo: integer;
begin
  if FieldsDataSet.RecNo <> -1 then
    Result := FieldsDataSet.RecNo
  else
    Result := FieldsDataSet.RecordCount + 1;
end;

function TModelFieldsDesignerFrame.GetReferenceModelEditComboBoxColumn: TColumn;
begin
  Result := GetComboBoxColumn('ReferenceModelEditComboBox');
end;

function TModelFieldsDesignerFrame.GetTypeComboBoxColumn: TColumn;
begin
  Result := GetComboBoxColumn('TypeComboBox');
end;

procedure TModelFieldsDesignerFrame.Init(const ANode: TEFTree);
var
  I: Integer;
begin
  if FieldsDataSet.Active then
    FieldsDataSet.Close;
  FieldsDataSet.CreateDataSet;
  inherited;
  FieldsDataSet.DisableControls;
  try
    for I := 0 to EditNode.ChildCount -1 do
      FieldsDataSet.AppendRecord([EditNode.Children[I].Name]);
  finally
    FieldsDataSet.First;
    FieldsDataSet.EnableControls;
  end;
end;

function TModelFieldsDesignerFrame.IsReferenceKeyField: Boolean;
begin
  if Assigned(EditModelField) then
    Result := Assigned(EditModelField.ParentField) and EditModelField.ParentField.IsReference
  else
    Result := False;
end;

function TModelFieldsDesignerFrame.IsFileReferenceField: Boolean;
begin
  Result := TypeComboBox.Text = TKFileReferenceDataType.GetTypeName;
end;

procedure TModelFieldsDesignerFrame.SetFieldSpec;
var
  LSize, LScale: Integer;
begin
  if Assigned(EditModelField) and not IsReferenceKeyField then
  begin
    if EditModelField.DataType.HasScale then
      LScale := DecimalPrecisionEdit.Value
    else
      LScale := 0;

    if EditModelField.DataType.HasSize then
      LSize := FieldSizeEdit.Value
    else
      LSize := 0;
    EditModelField.SetFieldSpec(TypeComboBox.Text, LSize, LScale,
      RequiredCheckBox.AsBoolean, PrimaryKeyCheckBox.AsBoolean, ReferenceModelEditComboBox.Text);
  end;
end;

class function TModelFieldsDesignerFrame.SuitsNode(const ANode: TEFNode): Boolean;
begin
  Assert(Assigned(ANode));

  Result := ANode is TKModelFields;
end;

procedure TModelFieldsDesignerFrame.TypeComboBoxChange(Sender: TObject);
begin
  inherited;
  if Assigned(EditModelField) then
  begin
    //First call to change DataType
    SetFieldSpec;
    //Check if current DataTypes requires Scale and Precision
    if not EditModelField.DataType.HasScale then
      DecimalPrecisionEdit.Value := 0;
    if not EditModelField.DataType.HasSize then
      FieldSizeEdit.Value := 0;
    //Recall to change Size and Precision
    SetFieldSpec;
  end;
  IsChanged := True;
end;

procedure TModelFieldsDesignerFrame.UpdateDesignPanel(const AForce: Boolean);
begin
  ProcessFields(UpdateDesignPanelForField, True);
end;

procedure TModelFieldsDesignerFrame.UpdateDesignPanelForField;
var
  LValue, LDataTypeKey: string;
  LDataTypeStr: string;
  I: Integer;
  LModel: TKModel;
  LProject: TProject;

  procedure UpdateFieldContent(const AField: TField);
  var
    LFieldName: string;
    LFieldNode: TEFNode;
  begin
    LFieldName := Copy(AField.FieldName,2,MaxInt);
    LFieldNode := EditModelField.FindNode(LFieldName);
    if Assigned(LFieldNode) then
    begin
      if (AField is TStringField) or (AField is TMemoField) then
        AField.AsString := LFieldNode.AsString
      else if AField is TIntegerField then
        AField.AsInteger := LFieldNode.AsInteger
      else if AField is TBooleanField then
        AField.AsBoolean := LFieldNode.AsBoolean;
    end
    else
    begin
      if AField is TBooleanField then
        AField.AsBoolean := False;
    end;
  end;

begin
  inherited;
  if Assigned(EditModelField) then
  begin
    UpdateFieldContent(_PhysicalName);
    UpdateFieldContent(_DefaultValue);
    UpdateFieldContent(_FileNameField);
    UpdateFieldContent(_IsVisible);
    UpdateFieldContent(_IsReadOnly);
    UpdateFieldContent(_EmptyAsNull);
    UpdateFieldContent(_DisplayWidth);
    UpdateFieldContent(_DisplayFormat);
    UpdateFieldContent(_EditFormat);
    UpdateFieldContent(_IsPicture);
    UpdateFieldContent(_CanUpdate);
    UpdateFieldContent(_CanInsert);
    UpdateFieldContent(_DisplayLabel);
    UpdateFieldContent(_DefaultFilterConnector);
//    UpdateFieldContent(_IsPicture_Thumbnail_Width);
//    UpdateFieldContent(_IsPicture_Thumbnail_Height);
    UpdateFieldContent(_Expression);
    UpdateFieldContent(_IsComputed);
    UpdateFieldContent(_IsGenerated);
    UpdateFieldContent(_DefaultFilter);
    UpdateFieldContent(_MaxUploadSize);
    _IsVisible.AsBoolean := EditModelField.GetBoolean('IsVisible', True);
    if not Assigned(EditModelField.FindNode('CanUpdate')) then
      _CanUpdate.AsBoolean := EditModelField.CanActuallyModify;
    if not Assigned(EditModelField.FindNode('CanInsert')) then
      _CanInsert.AsBoolean := EditModelField.CanActuallyModify;
    _DisplayWidth.Value := EditModelField.DisplayWidth;
    _DisplayLabel.Text := EditModelField.DisplayLabel;
    _DefaultFilterConnector.Text := EditModelField.DefaultFilterConnector;
    _EmptyAsNull.AsBoolean := EditModelField.EmptyAsNull;
    FieldNameEdit.Text := EditModelField.FieldName;
    LDataTypeStr := EditModelField.DataType.GetTypeName;
    if EditModelField.DataType.HasSize then
      FieldSizeEdit.Value := EditModelField.Size
    else
      FieldSizeEdit.Value := 0;
    if EditModelField.DataType.HasScale then
      DecimalPrecisionEdit.Value := EditModelField.DecimalPrecision
    else
      DecimalPrecisionEdit.Value := 0;

    if not _IsPicture.AsBoolean then
    begin
      _IsPicture_Thumbnail_Width.Value := 0;
      _IsPicture_Thumbnail_Height.Value := 0;
    end;

    PrimaryKeyCheckBox.AsBoolean := EditModelField.IsKey;
    RequiredCheckBox.AsBoolean := EditModelField.IsRequired;
    LDataTypeKey := '';
    TypeComboBoxColumn.PickList.Clear;
    for LValue in TEFDataTypeRegistry.Instance.GetClassIds do
    begin
      TypeComboBoxColumn.PickList.Add(LValue);
      if SameText(LValue, LDataTypeStr) then
        LDataTypeKey := LValue;
    end;
    LProject := TProject.CurrentProject;
    ReferenceModelEditComboBoxColumn.PickList.Clear;
    for I := 0 to LProject.Config.Models.ModelCount - 1 do
    begin
      LModel := LProject.Config.Models[I];
      ReferenceModelEditComboBoxColumn.PickList.Add(LModel.ModelName);
    end;
    TypeComboBox.Text := LDataTypeKey;
    ReferenceModelEditComboBox.Text := EditModelField.ReferencedModelName;

    //Attributes of Model field not visible at View field level
    _IsGenerated.Visible := False;
  end;
end;

initialization
  TEditNodeFrameRegistry.Instance.RegisterClass(TModelFieldsDesignerFrame.GetClassId, TModelFieldsDesignerFrame);

finalization
  if Assigned(TEditNodeFrameRegistry.Instance) then
    TEditNodeFrameRegistry.Instance.UnregisterClass(TModelFieldsDesignerFrame.GetClassId);

end.
