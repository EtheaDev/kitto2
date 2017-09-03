inherited DetailReferenceUpdateActionFrame: TDetailReferenceUpdateActionFrame
  inherited Label1: TLabel
    Visible = False
  end
  object DetailReferenceNameEdit: TLabeledEdit
    Left = 11
    Top = 24
    Width = 222
    Height = 21
    Hint = 'Customize the reference name'
    Anchors = [akLeft, akTop, akRight]
    EditLabel.Width = 110
    EditLabel.Height = 13
    EditLabel.Caption = 'Detail Reference Name'
    TabOrder = 0
  end
  object ForeignKeyNameEdit: TLabeledEdit
    Left = 11
    Top = 120
    Width = 222
    Height = 21
    Hint = 'Name of the referring foreign key in the database'
    Anchors = [akLeft, akTop, akRight]
    EditLabel.Width = 87
    EditLabel.Height = 13
    EditLabel.Caption = 'Foreign Key Name'
    ReadOnly = True
    TabOrder = 2
  end
  object DetailModelNameEdit: TLabeledEdit
    Left = 11
    Top = 72
    Width = 222
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    EditLabel.Width = 58
    EditLabel.Height = 13
    EditLabel.Caption = 'Detail Model'
    ReadOnly = True
    TabOrder = 1
  end
end
