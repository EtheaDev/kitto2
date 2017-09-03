inherited ReferenceFieldUpdateActionFrame: TReferenceFieldUpdateActionFrame
  inherited Label1: TLabel
    Visible = False
  end
  object ForeignKeyFieldsLabel: TLabel
    Left = 11
    Top = 95
    Width = 87
    Height = 13
    Caption = 'Foreign Key Fields'
  end
  object ReferenceNameEdit: TLabeledEdit
    Left = 11
    Top = 24
    Width = 222
    Height = 21
    Hint = 'Customize the field name'
    Anchors = [akLeft, akTop, akRight]
    EditLabel.Width = 80
    EditLabel.Height = 13
    EditLabel.Caption = 'Reference Name'
    TabOrder = 0
  end
  object ForeignKeyNameEdit: TLabeledEdit
    Left = 11
    Top = 68
    Width = 222
    Height = 21
    Hint = 'Name of the object in the database'
    Anchors = [akLeft, akTop, akRight]
    EditLabel.Width = 87
    EditLabel.Height = 13
    EditLabel.Caption = 'Foreign Key Name'
    ReadOnly = True
    TabOrder = 1
  end
  object IsRequiredCheckBox: TCheckBox
    Left = 11
    Top = 191
    Width = 102
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Is Required'
    TabOrder = 3
  end
  object ForeignKeyFieldsMemo: TMemo
    Left = 11
    Top = 112
    Width = 222
    Height = 73
    Anchors = [akLeft, akTop, akRight, akBottom]
    ReadOnly = True
    TabOrder = 2
  end
end
