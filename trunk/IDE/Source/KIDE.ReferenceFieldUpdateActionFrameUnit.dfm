inherited ReferenceFieldUpdateActionFrame: TReferenceFieldUpdateActionFrame
  inherited Label1: TLabel
    Visible = False
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
    Top = 95
    Width = 102
    Height = 17
    Caption = 'Is Required'
    TabOrder = 2
  end
end
