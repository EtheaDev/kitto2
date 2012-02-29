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
  end
  object DataTypeComboBox: TComboBox
    Left = 11
    Top = 112
    Width = 127
    Height = 21
    Sorted = True
    TabOrder = 2
  end
  object SizeEdit: TLabeledEdit
    Left = 144
    Top = 112
    Width = 89
    Height = 21
    Hint = 'Name of the object in the database'
    Anchors = [akLeft, akTop, akRight]
    EditLabel.Width = 19
    EditLabel.Height = 13
    EditLabel.Caption = 'Size'
    ReadOnly = True
    TabOrder = 3
  end
  object IsRequiredCheckBox: TCheckBox
    Left = 11
    Top = 139
    Width = 127
    Height = 17
    Caption = 'Is Required'
    TabOrder = 4
  end
  object IsKeyCheckBox: TCheckBox
    Left = 144
    Top = 139
    Width = 89
    Height = 17
    Caption = 'Is Key'
    TabOrder = 5
  end
end
