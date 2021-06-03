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
unit KIDE.ModelFieldDesignerFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, KIDE.MetadataItemDesignerFrameUnit,
  KIDE.BaseFrameUnit, KIDE.CodeEditorFrameUnit, Vcl.ExtCtrls, Vcl.Tabs, EF.Tree,
  Vcl.StdCtrls, System.Actions, Vcl.ActnList, Vcl.ComCtrls, Vcl.ToolWin, Vcl.Samples.Spin,
  Kitto.Metadata.Models, SynEdit, SynHighlighterSQL,
  KIDE.PairsValuesFrameUnit, KIDE.EditNodeBaseFrameUnit;

type
  TModelFieldDesignerFrame = class(TMetadataItemDesignerFrame)
    FieldNameEdit: TLabeledEdit;
    TypeComboBox: TComboBox;
    TypeLabel: TLabel;
    FieldSizeEdit: TSpinEdit;
    FieldSizeLabel: TLabel;
    PrimaryKeyCheckBox: TCheckBox;
    RequiredCheckBox: TCheckBox;
    _PhysicalName: TLabeledEdit;
    _DisplayLabel: TLabeledEdit;
    _IsVisible: TCheckBox;
    _IsReadOnly: TCheckBox;
    _IsGenerated: TCheckBox;
    DecimalPrecisionEdit: TSpinEdit;
    DecimalPrecisionLabel: TLabel;
    _Hint: TLabeledEdit;
    _DisplayWidth: TSpinEdit;
    DisplayWidthLabel: TLabel;
    _DisplayFormat: TLabeledEdit;
    _EditFormat: TLabeledEdit;
    _DefaultValue: TLabeledEdit;
    _DefaultFilterConnector: TLabeledEdit;
    _DefaultFilter: TLabeledEdit;
    _EmptyAsNull: TCheckBox;
    _CanUpdate: TCheckBox;
    _CanInsert: TCheckBox;
    _FileNameField: TLabeledEdit;
    EditorPageControl: TPageControl;
    FieldPropertiesTabSheet: TTabSheet;
    RulesTabSheet: TTabSheet;
    AllowedValuesTabSheet: TTabSheet;
    FieldPropertiesPanel: TPanel;
    FieldPropertiesScrollPanel: TPanel;
    FieldPropertiesScrollBox: TScrollBox;
    _IsPicture: TCheckBox;
    _IsPicture_Thumbnail_Width: TSpinEdit;
    Thumbnail_WidthLabel: TLabel;
    _IsPicture_Thumbnail_Height: TSpinEdit;
    Thumbnail_HeightLabel: TLabel;
    _MaxUploadSize: TLabeledEdit;
    _IsComputed: TCheckBox;
    ExpressionGroupBox: TGroupBox;
    ReferenceTabSheet: TTabSheet;
    FieldsGroupBox: TGroupBox;
    AutoAddFieldsGroupBox: TGroupBox;
    ReferenceModelPanel: TPanel;
    ReferenceModelEditComboBox: TComboBox;
    ReferenceModelLabel: TLabel;
    LookupFilterGroupBox: TGroupBox;
    ReferenceModelEdit: TLabeledEdit;
    _IsPassword: TCheckBox;
    AutoCompleteMinCharsLabel: TLabel;
    _AutoCompleteMinChars: TSpinEdit;
    procedure TypeComboBoxChange(Sender: TObject);
    procedure ReferenceTabSheetResize(Sender: TObject);
    procedure EditorPageControlChange(Sender: TObject);
    procedure ReferenceModelEditClick(Sender: TObject);
  private
    FSynSQLSyn: TSynSQLSyn;
    FExpressionEdit: TSynEdit;
    FLookupFilterEdit: TSynEdit;
    function IsReferenceKeyField: Boolean;
    function IsFileReferenceField: Boolean;
    procedure SetFieldSpec;
  protected
    procedure UpdateDesignPanel(const AForce: Boolean = False); override;
    function GetAllowedValuesNode(const ACreateIfMissing: Boolean = False): TEFNode; virtual;
    function GetAutoAddFieldsNode(const ACreateIfMissing: Boolean = False): TEFNode; virtual;
    function GetRulesNode(const ACreateIfMissing: Boolean = False): TEFNode; virtual;
    function GetEditModelField: TKModelField; virtual;
    class function SuitsNode(const ANode: TEFNode): Boolean; override;
    procedure CleanupDefaultsToEditNode; override;
    procedure DesignPanelToEditNode; override;
    procedure UpdateEditComponents; override;
    property EditModelField: TKModelField read GetEditModelField;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Init(const ANode: TEFTree); override;
  end;

implementation

uses
  KIDE.Project, KIDE.NodeDesignerFrameUnit, KIDE.Utils,
  Kitto.Metadata.ModelImplementation, Kitto.Metadata.DataView,
  KIDE.ModelRulesNodeFrameUnit, KIDE.ModelFieldsDesignerFrameUnit;

{$R *.dfm}

{ TModelFieldDesignerFrame }

procedure TModelFieldDesignerFrame.UpdateEditComponents;
var
  LIsReference, LIsReferenceKeyField, LIsFileReferenceField: Boolean;
  LExpressionField: Boolean;
begin
  inherited;
  LIsReferenceKeyField := IsReferenceKeyField;
  TypeComboBox.Enabled := not LIsReferenceKeyField;
  LIsFileReferenceField := IsFileReferenceField;
  if Assigned(EditModelField) then
  begin
    DecimalPrecisionEdit.Visible := EditModelField.DataType.HasScale and not LIsReferenceKeyField;
    FieldSizeEdit.Visible := EditModelField.DataType.HasSize and not LIsReferenceKeyField;
    DecimalPrecisionLabel.Visible := DecimalPrecisionEdit.Visible;
    FieldSizeLabel.Visible := FieldSizeEdit.Visible;
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
  FExpressionEdit.Visible := not LIsReferenceKeyField;

  LIsReference := TypeComboBox.Text = TKReferenceDataType.GetTypeName;
  ReferenceModelEditComboBox.Visible := LIsReference;
  ReferenceModelLabel.Visible := ReferenceModelEditComboBox.Visible;

  AllowedValuesTabSheet.TabVisible := not LIsReferenceKeyField;
  RulesTabSheet.TabVisible := not LIsReferenceKeyField;
  //ImagesTabSheet.TabVisible := not LIsReferenceKeyField;
  ReferenceTabSheet.TabVisible := LIsReference;
  ReferenceModelEdit.Visible := LIsReference;

  LExpressionField := FExpressionEdit.Text <> '';
  if LExpressionField then
  begin
    _CanUpdate.Checked := False;
    _CanInsert.Checked := False;
    _IsComputed.Checked := False;
    _IsPassword.Checked := False;
  end;
  _CanUpdate.Enabled := not LExpressionField;
  _CanInsert.Enabled := not LExpressionField;
  _IsComputed.Enabled := not LExpressionField;
  _IsPassword.Enabled := not LExpressionField;

  _IsPicture_Thumbnail_Width.Visible := _IsPicture.Checked;
  Thumbnail_WidthLabel.Visible := _IsPicture_Thumbnail_Width.Visible;
  _IsPicture_Thumbnail_Height.Visible := _IsPicture.Checked;
  Thumbnail_HeightLabel.Visible := _IsPicture_Thumbnail_Height.Visible;
  _MaxUploadSize.Visible := _IsPicture.Checked or LIsFileReferenceField;
end;

procedure TModelFieldDesignerFrame.CleanupDefaultsToEditNode;
var
  LNode: TEFNode;
begin
  inherited;
  if Assigned(EditModelField) then
  begin
    CleanupTextNode('PhysicalName');
    CleanupTextNode('DisplayLabel', EditModelField.DefaultDisplayLabel);
    CleanupBooleanNode('IsVisible', True);
    CleanupBooleanNode('IsReadOnly');
    CleanupBooleanNode('IsGenerated');
    CleanupTextNode('Hint');
    CleanupIntegerNode('DisplayWidth', EditModelField.DefaultDisplayWidth);
    CleanupTextNode('DisplayFormat');
    CleanupTextNode('EditFormat');
    CleanupTextNode('DefaultValue');
    CleanupTextNode('DefaultFilterConnector', 'and');
    CleanupTextNode('DefaultFilter');
    CleanupTextNode('LookupFilter');
    CleanupBooleanNode('EmptyAsNull', EditModelField.DefaultEmptyAsNull);
    CleanupBooleanNode('CanUpdate', EditModelField.CanActuallyModify);
    CleanupBooleanNode('CanInsert', EditModelField.CanActuallyModify);
    CleanupIntegerNode('AutoCompleteMinChars', EditModelField.DEFAULT_AUTOCOMPLETEMINCHARS);
    CleanupBooleanNode('IsComputed');
    CleanupBooleanNode('IsPassword');
    CleanupTextNode('FileNameField');
    CleanupTextNode('Expression');
    LNode := EditNode.FindNode('IsPicture/Thumbnail/Width');
    if Assigned(LNode) and (LNode.AsInteger = 0) then
      EditNode.DeleteNode('IsPicture/Thumbnail/Width');
    LNode := EditNode.FindNode('IsPicture/Thumbnail/Height');
    if Assigned(LNode) and (LNode.AsInteger = 0) then
      EditNode.DeleteNode('IsPicture/Thumbnail/Height');
    CleanupBooleanNode('IsPicture');
    CleanupTextNode('MaxUploadSize');
    CleanupOrphanNode('IsPicture/Thumbnail');
    CleanupOrphanNode('AllowedValues');
    CleanupOrphanNode('AutoAddFields');
    CleanupOrphanNode('Rules');
    CleanupOrphanNode('Fields');
  end;
end;

constructor TModelFieldDesignerFrame.Create(AOwner: TComponent);
begin
  inherited;
  FSynSQLSyn := TSynSQLSyn.Create(Self);
  FExpressionEdit := CreateSynEditor(Self, ExpressionGroupBox, '_Expression',
    FSynSQLSyn, Font.Size, EditorChange);
  FExpressionEdit.Gutter.Visible := False;
  FLookupFilterEdit := CreateSynEditor(Self, LookupFilterGroupBox, '_LookupFilter',
    FSynSQLSyn, Font.Size, EditorChange);
  FLookupFilterEdit.Gutter.Visible := False;
end;

procedure TModelFieldDesignerFrame.DesignPanelToEditNode;
var
  I: Integer;
  LFieldsNode: TKModelFields;
  LDeleteFieldNodes: boolean;
begin
  inherited;
  EditorPageControl.ActivePageIndex := 0;
  if not (EditNode is TKViewField) then
  begin
    SetFieldSpec;
    EditModelField.Name := FieldNameEdit.Text;
    if EditModelField.IsReference and Assigned(EditModelField.ReferencedModel) then
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

procedure TModelFieldDesignerFrame.EditorPageControlChange(Sender: TObject);
begin
  inherited;
  if EditorPageControl.ActivePage = ReferenceTabSheet then
  begin
    EmbedEditNodeFrame(FieldsGroupBox, TModelFieldsDesignerFrame,
      EditNode.FindNode('Fields', True), True);
    EmbedEditNodeFrame(AutoAddFieldsGroupBox, TPairsValuesFrame,
      GetAutoAddFieldsNode(True), True);
  end
  else if EditorPageControl.ActivePage = AllowedValuesTabSheet then
  begin
    EmbedEditNodeFrame(AllowedValuesTabSheet, TPairsValuesFrame,
      GetAllowedValuesNode(True), True);
  end
  else if EditorPageControl.ActivePage = RulesTabSheet then
  begin
    EmbedEditNodeFrame(RulesTabSheet, TModelRulesNodeFrame,
      GetRulesNode(True), True);
  end;
end;

function TModelFieldDesignerFrame.GetAllowedValuesNode(const ACreateIfMissing: Boolean = False): TEFNode;
begin
  Assert(Assigned(EditNode));
  Result := EditNode.FindNode('AllowedValues', ACreateIfMissing);
end;

function TModelFieldDesignerFrame.GetAutoAddFieldsNode(
  const ACreateIfMissing: Boolean): TEFNode;
begin
  Assert(Assigned(EditNode));
  Result := EditNode.FindNode('AutoAddFields', ACreateIfMissing);
end;

function TModelFieldDesignerFrame.GetEditModelField: TKModelField;
begin
  Result := EditNode as TKModelField;
end;

function TModelFieldDesignerFrame.GetRulesNode(
  const ACreateIfMissing: Boolean): TEFNode;
begin
  Assert(Assigned(EditNode));
  Result := EditNode.FindNode('Rules', ACreateIfMissing);
end;

procedure TModelFieldDesignerFrame.Init(const ANode: TEFTree);
begin
  inherited;
end;

function TModelFieldDesignerFrame.IsReferenceKeyField: Boolean;
begin
  Assert(Assigned(EditModelField) );
  Result := Assigned(EditModelField.ParentField) and EditModelField.ParentField.IsReference;
end;

procedure TModelFieldDesignerFrame.ReferenceModelEditClick(Sender: TObject);
begin
  inherited;
  EditorPageControl.ActivePage := ReferenceTabSheet;
end;

procedure TModelFieldDesignerFrame.ReferenceTabSheetResize(Sender: TObject);
begin
  inherited;
  FieldsGroupBox.Height := (ReferenceTabSheet.Height - ReferenceModelPanel.Height) div 2;
end;

function TModelFieldDesignerFrame.IsFileReferenceField: Boolean;
begin
  Assert(Assigned(EditModelField) );
  Result := TypeComboBox.Text = TKFileReferenceDataType.GetTypeName;;
end;

procedure TModelFieldDesignerFrame.SetFieldSpec;
var
  LSize, LScale: Integer;
begin
  if not IsReferenceKeyField then
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
      RequiredCheckBox.Checked, PrimaryKeyCheckBox.Checked, ReferenceModelEditComboBox.Text);
  end;
end;

class function TModelFieldDesignerFrame.SuitsNode(const ANode: TEFNode): Boolean;
begin
  Assert(Assigned(ANode));

  Result := ANode is TKModelField;
end;

procedure TModelFieldDesignerFrame.TypeComboBoxChange(Sender: TObject);
begin
  inherited;
  //First call to change DataType
  SetFieldSpec;
  //Check if current DataTypes requires Scale and Precision
  if not EditModelField.DataType.HasScale then
    DecimalPrecisionEdit.Value := 0;
  if not EditModelField.DataType.HasSize then
    FieldSizeEdit.Value := 0;
  //Recall to change Size and Precision
  SetFieldSpec;
  IsChanged := True;
end;

procedure TModelFieldDesignerFrame.UpdateDesignPanel(const AForce: Boolean);
var
  LValue, LDataTypeKey: string;
  LDataTypeStr: string;
  I: Integer;
  LModel: TKModel;
  LProject: TProject;
begin
  Assert(Assigned(EditModelField));
  EditorPageControl.ActivePageIndex := 0;
  inherited;

  _IsVisible.Checked := EditNode.GetBoolean('IsVisible', True);
  if not Assigned(EditModelField.FindNode('CanUpdate')) then
    _CanUpdate.Checked := EditModelField.CanActuallyModify;
  if not Assigned(EditModelField.FindNode('CanInsert')) then
    _CanInsert.Checked := EditModelField.CanActuallyModify;
  _DisplayWidth.Value := EditModelField.DisplayWidth;
  _DisplayLabel.Text := EditModelField.DisplayLabel;
  _DefaultFilterConnector.Text := EditModelField.DefaultFilterConnector;
  _DefaultFilter.Text := EditModelField.DefaultFilter;
  _EmptyAsNull.Checked := EditModelField.EmptyAsNull;
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

  if not _IsPicture.Checked then
  begin
    _IsPicture_Thumbnail_Width.Value := 0;
    _IsPicture_Thumbnail_Height.Value := 0;
  end;

  PrimaryKeyCheckBox.Checked := EditModelField.IsKey;
  RequiredCheckBox.Checked := EditModelField.IsRequired;
  LDataTypeKey := '';
  TypeComboBox.Items.Clear;
  for LValue in TEFDataTypeRegistry.Instance.GetClassIds do
  begin
    TypeComboBox.Items.Add(LValue);
    if SameText(LValue, LDataTypeStr) then
      LDataTypeKey := LValue;
  end;
  LProject := TProject.CurrentProject;
  ReferenceModelEditComboBox.Items.Clear;
  for I := 0 to LProject.Config.Models.ModelCount - 1 do
  begin
    LModel := LProject.Config.Models[I];
    ReferenceModelEditComboBox.AddItem(LModel.ModelName, LModel);
  end;
  TypeComboBox.ItemIndex := TypeComboBox.Items.IndexOf(LDataTypeKey);
  ReferenceModelEditComboBox.Text := EditModelField.ReferencedModelName;
  ReferenceModelEdit.Text := EditModelField.ReferencedModelName;
  _AutoCompleteMinChars.Value := EditModelField.AutoCompleteMinChars;

  //Attributes of Model field not visible at View field level
  _IsGenerated.Visible := False;
end;

initialization
  TEditNodeFrameRegistry.Instance.RegisterClass(TModelFieldDesignerFrame.GetClassId, TModelFieldDesignerFrame);

finalization
  if Assigned(TEditNodeFrameRegistry.Instance) then
    TEditNodeFrameRegistry.Instance.UnregisterClass(TModelFieldDesignerFrame.GetClassId);

end.
