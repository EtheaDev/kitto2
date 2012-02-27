inherited TableInfoModelUpdateActionFrame: TTableInfoModelUpdateActionFrame
  inherited Label1: TLabel
    Visible = False
  end
  object ModelNameEdit: TLabeledEdit
    Left = 11
    Top = 24
    Width = 222
    Height = 21
    Hint = 'Customize the model name'
    Anchors = [akLeft, akTop, akRight]
    EditLabel.Width = 58
    EditLabel.Height = 13
    EditLabel.Caption = 'Model Name'
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
end
