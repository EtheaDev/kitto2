inherited ListControllerFilterItemFrame: TListControllerFilterItemFrame
  inherited ClientPanel: TPanel
    inherited DesignPanel: TPanel
      Padding.Top = 50
      object LabelEdit: TLabeledEdit
        Left = 6
        Top = 21
        Width = 210
        Height = 21
        EditLabel.Width = 25
        EditLabel.Height = 13
        EditLabel.Caption = 'Label'
        TabOrder = 0
      end
      object ExpressionTemplateGroupBox: TGroupBox
        Left = 6
        Top = 48
        Width = 575
        Height = 241
        Anchors = [akLeft, akTop, akRight, akBottom]
        Caption = 'ExpressionTemplate'
        TabOrder = 1
      end
      object _DefaultValue: TLabeledEdit
        Left = 222
        Top = 21
        Width = 359
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        EditLabel.Width = 61
        EditLabel.Height = 13
        EditLabel.Caption = 'DefaultValue'
        TabOrder = 2
      end
    end
  end
end
