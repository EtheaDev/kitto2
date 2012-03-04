inherited ModelFieldUpdateActionFrame: TModelFieldUpdateActionFrame
  inherited Label1: TLabel
    Visible = False
  end
  object Label2: TLabel
    Left = 11
    Top = 96
    Width = 50
    Height = 13
    Caption = 'Data Type'
  end
  object FieldNameEdit: TLabeledEdit
    Left = 11
    Top = 24
    Width = 222
    Height = 21
    Hint = 'Customize the field name'
    Anchors = [akLeft, akTop, akRight]
    EditLabel.Width = 52
    EditLabel.Height = 13
    EditLabel.Caption = 'Field Name'
    TabOrder = 0
    OnChange = ControlChange
  end
  object PhysicalNameEdit: TLabeledEdit
    Left = 11
    Top = 68
    Width = 222
    Height = 21
    Hint = 'Name of the object in the database'
    Anchors = [akLeft, akTop, akRight]
    EditLabel.Width = 68
    EditLabel.Height = 13
    EditLabel.Caption = 'Physical Name'
    ReadOnly = True
    TabOrder = 1
    OnChange = ControlChange
  end
  object DataTypeComboBox: TComboBox
    Left = 11
    Top = 112
    Width = 102
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    Sorted = True
    TabOrder = 2
    OnChange = ControlChange
  end
  object SizeEdit: TLabeledEdit
    Left = 119
    Top = 112
    Width = 58
    Height = 21
    Hint = 'Name of the object in the database'
    Anchors = [akTop, akRight]
    EditLabel.Width = 19
    EditLabel.Height = 13
    EditLabel.Caption = 'Size'
    ReadOnly = True
    TabOrder = 3
    OnChange = ControlChange
  end
  object IsRequiredCheckBox: TCheckBox
    Left = 11
    Top = 139
    Width = 102
    Height = 17
    Caption = 'Is Required'
    TabOrder = 5
  end
  object IsKeyCheckBox: TCheckBox
    Left = 119
    Top = 139
    Width = 58
    Height = 17
    Caption = 'Is Key'
    TabOrder = 6
  end
  object ScaleEdit: TLabeledEdit
    Left = 183
    Top = 112
    Width = 50
    Height = 21
    Hint = 'Name of the object in the database'
    Anchors = [akTop, akRight]
    EditLabel.Width = 25
    EditLabel.Height = 13
    EditLabel.Caption = 'Scale'
    ReadOnly = True
    TabOrder = 4
    OnChange = ControlChange
  end
end
